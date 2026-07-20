(ns com.blockether.vis.internal.foundation.shell-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]]))

;; The impls are private (named for clarity inside the ns); reach them by var
;; so tests drive the real gate/render contract without the Python wrapper.
(def ^:private shell-run* @#'shell/shell-run-impl)
(def ^:private shell-bg* @#'shell/shell-bg-impl)
(def ^:private shell-logs* @#'shell/shell-logs-impl)
(def ^:private shell-send* @#'shell/shell-send-impl)
(def ^:private render-shell-run-result @#'shell/render-shell-run-result)
(def ^:private render-shell-bg-result @#'shell/render-shell-bg-result)
(def ^:private render-shell-logs-result @#'shell/render-shell-logs-result)
(def ^:private format-shell-command @#'shell/format-shell-command)

(defn- with-shell-on
  [f]
  (toggles/set-enabled! :shell/enabled true)
  (try (f) (finally (toggles/reset-to-default! :shell/enabled))))

(defn- threw?
  "lazytest has no `thrown?`; run `thunk` and report whether it threw."
  [thunk]
  (try (thunk) false (catch Throwable _ true)))

(defn- poll
  "Re-run `thunk` until `pred` holds (~5s), returning the value."
  ([thunk pred] (poll thunk pred 50))
  ([thunk pred tries]
   (loop [i 0]
     (let [v (thunk)]
       (cond (pred v) v
             (>= i tries) (throw (ex-info "poll exhausted" {:last v}))
             :else (do (Thread/sleep 100) (recur (inc i))))))))

(defdescribe shell-toggle-gate-test
             (it "is OFF by default and short-circuits every call into a refusal envelope"
                 (toggles/reset-to-default! :shell/enabled)
                 (expect (false? (toggles/enabled? :shell/enabled)))
                 (let [gate
                       (@#'shell/shell-gate-before-fn :shell/run)

                       out
                       (gate {:session-id "t"} identity ["echo hi"])]

                   (expect (contains? out :result))
                   (expect (extension/envelope-failure? (:result out)))
                   (expect (= :shell-disabled (get-in out [:result :error :reason])))
                   ;; the human label is the snake call name, and the hint points at the USER
                   (expect (str/includes? (get-in out [:result :error :message]) "shell_run"))
                   (expect (str/includes? (get-in out [:result :error :hint]) "USER"))))
             (it "injects env as the first arg when the toggle is ON"
                 (with-shell-on (fn []
                                  (let [gate
                                        (@#'shell/shell-gate-before-fn :shell/run)

                                        out
                                        (gate {:session-id "t"} identity ["echo hi"])]

                                    (expect (not (contains? out :result)))
                                    (expect (= [{:session-id "t"} "echo hi"] (:args out))))))))

(defdescribe
  shell-run-sync-test
  (it "returns stdout / stderr / exit / duration_ms with NO dead keys"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [r (:result (shell-run* {} "echo out; echo err 1>&2; exit 3"))]
                           (expect (= "out\n" (get r "stdout")))
                           (expect (= "err\n" (get r "stderr")))
                           (expect (= 3 (get r "exit")))
                           (expect (number? (get r "duration_ms")))
                           ;; lean contract: echoes / always-false flags never ship
                           (expect (nil? (get r "op")))
                           (expect (not (contains? r "timed_out")))
                           (expect (not (contains? r "timeout_secs")))
                           (expect (not (contains? r "stdout_truncated")))
                           (expect (not (contains? r "stderr_truncated")))
                           (expect (not (contains? r "cwd"))))))))
  (it "omits :stderr when the stream is empty and :cwd unless narrowed"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [r (:result (shell-run* {} "echo only-out"))]
                           (expect (= "only-out\n" (get r "stdout")))
                           (expect (not (contains? r "stderr"))))
                         (let [r (:result (shell-run* {} "pwd" {"cwd" "src"}))]
                           (expect (string? (get r "cwd")))
                           (expect (str/ends-with? (get r "cwd") "/src")))))))
  (it "treats a non-zero exit as a SUCCESS envelope (data, not a tool error)"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [env (shell-run* {} "exit 42")]
                           (expect (extension/envelope-success? env))
                           (expect (= 42 (get (:result env) "exit"))))))))
  (it "kills the process tree on timeout and reports timed_out with nil exit"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [t0 (System/currentTimeMillis)
                               r (:result (shell-run* {} "sleep 30" {"timeout_secs" 1}))
                               dt (- (System/currentTimeMillis) t0)]

                           (expect (true? (get r "timed_out")))
                           (expect (nil? (get r "exit")))
                           (expect (< dt 15000)))))))
  (it "keeps BOTH the head and the tail of huge output, dropping only the middle"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         ;; ~72k chars of stdout — far over the head+tail budget.
                         (let [r (:result (shell-run*
                                            {}
                                            (str "echo HEAD_MARKER; " "for i in $(seq 1 2000); do "
                                                 "echo 'filler-filler-filler-filler-filler'; done; "
                                                 "echo TAIL_MARKER")))
                               out (get r "stdout")]

                           (expect (true? (get r "stdout_truncated")))
                           ;; the opening line is NO LONGER swallowed (the old tail-only cap ate it)
                           (expect (str/includes? out "HEAD_MARKER"))
                           ;; the closing summary still survives
                           (expect (str/includes? out "TAIL_MARKER"))
                           ;; and the drop is made visible, not silent
                           (expect (str/includes? out "chars omitted")))))))
  (it "honors a timeout above the 120s default (up to the 600s cap)"
      ;; :timeout_secs only ships on a TIMED-OUT result now, so the clamp is
      ;; asserted on the helper directly instead of burning wall-clock.
      (let [clamp @#'shell/clamp-timeout-secs]
        (expect (= 120 (clamp nil)))
        (expect (= 300 (clamp 300)))
        (expect (= 600 (clamp 5000)))
        (expect (= 1 (clamp 0)))))
  (it "surfaces timeout_secs alongside timed_out on the timeout path"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [r (:result (shell-run* {} "sleep 30" {"timeout_secs" 1}))]
                           (expect (true? (get r "timed_out")))
                           (expect (= 1 (get r "timeout_secs")))
                           (expect (not (contains? r "exit"))))))))
  (it "rejects a cwd that escapes the workspace root"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (expect (threw? #(shell-run* {} "pwd" {"cwd" "../.."})))))))
  (it "accepts an ABSOLUTE cwd that lands inside a workspace root"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [abs (.getCanonicalPath (java.io.File. (str (workspace/cwd))))
                               r (:result (shell-run* {} "pwd" {"cwd" abs}))]

                           (expect (string? (get r "cwd")))
                           (expect (= abs (get r "cwd"))))
                         ;; an absolute path OUTSIDE every root is still rejected
                         (expect (threw? #(shell-run* {} "pwd" {"cwd" "/"})))))))
  (it "accepts a float timeout but rejects a non-numeric one with a typed error"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (expect (extension/envelope-success?
                                   (shell-run* {} "true" {"timeout_secs" 1.0})))
                         (let [thrown (try (shell-run* {} "true" {"timeout_secs" "30"})
                                           nil
                                           (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                           (expect (= ::shell/bad-option (:type thrown)))))))))

(defdescribe
  shell-background-test
  (it "registers a session resource, tails logs, and stops cleanly via the registry"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let [sid "shell-ext-bg"
                  env {:session-id sid}]

              (try (let [reg (:result (shell-bg* env "worker" "echo l1; echo l2; sleep 60"))]
                     (expect (= "running" (get reg "status")))
                     (expect (pos? (get reg "pid"))))
                   (let [rs (resources/list-resources sid)]
                     (expect (= 1 (count rs)))
                     (expect (= "worker" (get (first rs) "id")))
                     (expect (= "shell" (get (first rs) "kind")))
                     (expect (true? (get (first rs) "can_stop"))))
                   (let [r (poll #(:result (shell-logs* env "worker"))
                                 #(>= (count (get % "lines")) 2))]
                     (expect (= [1 "l1"] (vec (first (get r "lines")))))
                     (expect (= "running" (get r "status"))))
                   (let [stop (resources/stop! sid "worker")]
                     (expect (= :stopped (:result stop)))
                     (expect (empty? (resources/list-resources sid)))
                     (expect (threw? #(shell-logs* env "worker"))))
                   (finally (resources/stop-all! sid))))))))
  (it "keeps an exited process listed (status :exited) with readable logs + exit"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [sid "shell-ext-exit"
                               env {:session-id sid}]

                           (try (shell-bg* env "quick" "echo done; exit 7")
                                (let [r (poll #(:result (shell-logs* env "quick"))
                                              #(= "exited" (get % "status")))]
                                  (expect (= 7 (get r "exit")))
                                  (expect (= [[1 "done"]] (mapv vec (get r "lines")))))
                                (let [res (first (resources/list-resources sid))]
                                  (expect (some? res))
                                  (expect (= "failed" (get res "status"))))
                                (finally (resources/stop-all! sid))))))))
  (it "refuses a duplicate id while the first is still running"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [sid "shell-ext-dup"
                               env {:session-id sid}]

                           (try (shell-bg* env "dup" "sleep 60")
                                (expect (threw? #(shell-bg* env "dup" "sleep 60")))
                                (finally (resources/stop-all! sid))))))))
  (it "carries uptime_ms and ships NO dead keys in the logs payload"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [sid "shell-ext-meta"
                               env {:session-id sid}]

                           (try (shell-bg* env "m" "sleep 60")
                                (let [r (:result (shell-logs* env "m"))]
                                  (expect (>= (get r "uptime_ms") 0))
                                  ;; lean contract: echoes / derivables / zero-counters are out
                                  (expect (not (contains? r "op")))
                                  (expect (not (contains? r "cmd")))
                                  (expect (not (contains? r "cwd")))
                                  (expect (not (contains? r "pid")))
                                  (expect (not (contains? r "shown_count")))
                                  (expect (not (contains? r "dropped")))
                                  (expect (not (contains? r "exit"))))
                                (finally (resources/stop-all! sid))))))))
  (it "stops promptly even when the command double-forks a detached daemon"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let [sid "shell-ext-nohup"
                  env {:session-id sid}]

              (try (shell-bg* env "d" "nohup sleep 120 >/dev/null 2>&1 & echo spawned; sleep 60")
                   (let [t0 (System/currentTimeMillis)
                         stop (resources/stop! sid "d")
                         dt (- (System/currentTimeMillis) t0)]

                     (expect (= :stopped (:result stop)))
                     (expect (< dt 8000))
                     (expect (empty? (resources/list-resources sid))))
                   (finally (resources/stop-all! sid)))))))))

(defdescribe shell-send-test
             (it "types into a running background shell's stdin and the program reads it"
                 (with-shell-on
                   (fn []
                     (binding [workspace/*workspace-root* (workspace/trunk-root)]
                       (let [sid "shell-ext-send"
                             env {:session-id sid}]

                         (try (shell-bg* env "echoer" "read x; echo GOT:$x; sleep 60")
                              (let [snt (:result (shell-send* env "echoer" "hi-there"))]
                                (expect (= "running" (get snt "status")))
                                ;; "hi-there" (8) + submitting newline = 9 chars written
                                (expect (= 9 (get snt "sent"))))
                              (let [hit? (fn [r]
                                           (some #(str/includes? (str (second %)) "GOT:hi-there")
                                                 (get r "lines")))
                                    r (poll #(:result (shell-logs* env "echoer")) hit?)]

                                (expect (hit? r)))
                              (finally (resources/stop-all! sid))))))))
             (it "refuses a send to an unknown id and to an exited shell"
                 (with-shell-on (fn []
                                  (binding [workspace/*workspace-root* (workspace/trunk-root)]
                                    (let [sid "shell-ext-send-err"
                                          env {:session-id sid}]

                                      (try (expect (threw? #(shell-send* env "nope" "x")))
                                           (shell-bg* env "gone" "exit 0")
                                           (poll #(:result (shell-logs* env "gone"))
                                                 #(= "exited" (get % "status")))
                                           (expect (threw? #(shell-send* env "gone" "x")))
                                           (finally (resources/stop-all! sid)))))))))

(defdescribe
  shell-render-test
  (it "renders shell_run like a REPL-style collapsible card"
      (let [card (render-shell-run-result
                   {"cmd" "echo hi" "exit" 0 "duration_ms" 12 "stdout" "hi"})]
        (expect (= "$ echo hi (success) · 12ms" (:summary card)))
        (expect (str/includes? (:body card) "**COMMAND**"))
        (expect (str/includes? (:body card) "**STATUS**"))
        (expect (str/includes? (:body card) "**STDOUT**"))))
  (it "separates a compound command onto its own lines in the COMMAND card"
      (let [card (render-shell-run-result {"cmd" "a; b && c" "exit" 0 "duration_ms" 1})]
        (expect (str/includes? (:body card) "a;\nb &&\nc"))))
  (it "pretty-prints top-level shell operators, quote/paren-aware"
      ;; top-level ; && || each end their line
      (expect (= "a;\nb &&\nc ||\nd" (format-shell-command "a; b && c || d")))
      ;; separators inside quotes stay put
      (expect (= "echo 'a; b && c'" (format-shell-command "echo 'a; b && c'")))
      (expect (= "echo \"a; b\"" (format-shell-command "echo \"a; b\"")))
      ;; separators nested in $(…) stay put; the top-level ; still breaks
      (expect (= "x=$(f || g);\ny" (format-shell-command "x=$(f || g); y")))
      ;; single & (background) and 2>&1 are never split
      (expect (= "nohup ./x > log 2>&1 &" (format-shell-command "nohup ./x > log 2>&1 &")))
      ;; a simple command comes back unchanged
      (expect (= "ls -la" (format-shell-command "ls -la")))
      (expect (= "" (format-shell-command nil))))
  (it "surfaces shell failures and timeouts on the collapsed chip"
      (expect (str/includes? (:summary (render-shell-run-result
                                         {"cmd" "grep nope missing" "exit" 2 "duration_ms" 34}))
                             "$ grep nope missing (failure) · exit 2 · 34ms"))
      (expect (str/includes? (:summary (render-shell-run-result {"cmd" "make test"
                                                                 "timed_out" true
                                                                 "timeout_secs" 5
                                                                 "duration_ms" 5000}))
                             "$ make test (failure) · timed out after 5s · 5.0s")))
  (it
    "renders background lifecycle/log cards with expandable sections"
    (let [bg
          (render-shell-bg-result {"id" "srv"
                                   "cmd" "npm run dev"
                                   "pid" 123
                                   "status" "running"
                                   "attach" "vis ext shell attach srv"})

          logs
          (render-shell-logs-result
            {"id" "srv" "status" "running" "lines" [[1 "ready"]] "line_count" 1 "uptime_ms" 1500})]

      (expect (str/includes? (:summary bg) "⚙ bg `srv` running · pid 123"))
      (expect (str/includes? (:body bg) "**COMMAND**"))
      (expect (str/includes? (:summary logs) "◷ `srv` running · 1 lines · 1.5s"))
      (expect (str/includes? (:body logs) "**LOGS**")))))

(defdescribe shell-native-contract-test
             (it "routes lifecycle through compact native descriptions"
                 (let [run
                       (:ext.symbol/description shell/shell-run-symbol)

                       bg
                       (:ext.symbol/description shell/shell-bg-symbol)

                       logs
                       (:ext.symbol/description shell/shell-logs-symbol)]

                   (expect (str/includes? run "command that should exit"))
                   (expect (str/includes? bg "resource_stop"))
                   (expect (str/includes? logs "succeeded, failed, or still runs"))
                   (expect (every? #(< (count %) 350) [run bg logs]))))
             (it "closes every native shell input schema"
                 (doseq [s shell/shell-symbols]
                   (expect (false? (get-in s [:ext.symbol/schema :additionalProperties]))))))

(defdescribe shell-extension-shape-test
             (it "is a registered aliased extension exposing run / bg / logs symbols"
                 (expect (= "foundation-shell" (:ext/name shell/vis-extension)))
                 (expect (= 'shell (get-in shell/vis-extension [:ext/engine :ext.engine/alias])))
                 (let [syms (set (map :ext.symbol/symbol shell/shell-symbols))]
                   (expect (contains? syms 'run))
                   (expect (contains? syms 'bg))
                   (expect (contains? syms 'logs))
                   (expect (contains? syms 'send)))))
