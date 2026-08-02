(ns com.blockether.vis.internal.foundation.shell-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.process-jail :as process-jail]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [org.graalvm.polyglot Context]))

;; The impls are private (named for clarity inside the ns); reach them by var
;; so tests drive the real gate/render contract without the Python wrapper.
(def ^:private shell-run* @#'shell/shell-run-impl)

(defn- shell-bg*
  "Background impl with the options map optional — most cases need no cwd/opts.
  `cmd` is the ONE-COMMAND SPELLING of the batch, exactly as dispatch admits it."
  ([env id cmd] (shell-bg* env id cmd nil))
  ([env id cmd opts] (@#'shell/shell-bg-impl env id [cmd] opts)))

(def ^:private shell-logs* @#'shell/shell-logs-impl)

(def ^:private shell-wait* @#'shell/shell-wait-impl)

(def ^:private shell-send* @#'shell/shell-send-impl)

(def ^:private shell* shell/shell)

(def ^:private render-shell-run-result @#'shell/render-shell-run-result)

(def ^:private render-shell-bg-result @#'shell/render-shell-bg-result)

(def ^:private render-shell-logs-result @#'shell/render-shell-logs-result)

(def ^:private render-shell-send-result @#'shell/render-shell-send-result)

(def ^:private keys-label @#'shell/keys-label)

(def ^:private format-shell-command @#'shell/format-shell-command)

(defn- with-shell-on
  "Shell is unconditionally available now; kept as a thin pass-through so the
   existing call sites read unchanged."
  [f]
  (f))

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

(defdescribe shell-env-injection-test
             (it "uses declarative env injection rather than a before middleware shim"
                 (expect (true? (:ext.symbol/inject-env? shell/shell-symbol)))
                 (expect (nil? (:ext.symbol/before-fn shell/shell-symbol))))
             (it "a failing or nil session policy denies spawn instead of failing open"
                 (binding [workspace/*workspace-root* (workspace/trunk-root)]
                   (expect (threw? #(shell-run* {:session-id "t"
                                                 :jail-policy-fn (fn []
                                                                   (throw (ex-info "boom" {})))}
                                                "echo escaped")))
                   (expect (threw? #(shell-run* {:session-id "t" :jail-policy-fn (constantly nil)}
                                                "echo escaped")))))
             (it "sandbox false is represented explicitly and still launches unwrapped"
                 (binding [workspace/*workspace-root* (workspace/trunk-root)]
                   (let
                     [r (shell-run* {:session-id "t" :jail-policy-fn (constantly {:disabled? true})}
                                    "printf explicit-opt-out")]
                     (expect (= 0 (get r "exit")))
                     (expect (= "explicit-opt-out" (get r "stdout")))))))

(defdescribe
  shell-run-sync-test
  (it "returns a TOTAL result: every key present, flags real booleans"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let [r (shell-run* {} "echo out; echo err 1>&2; exit 3")]
              (expect (= "out\n" (get r "stdout")))
              (expect (= "err\n" (get r "stderr")))
              (expect (= 3 (get r "exit")))
              (expect (number? (get r "duration_ms")))
              ;; TOTAL entry contract: every command-result key is present, so model
              ;; Python indexes any field directly and never branches on which shape
              ;; came back. The command's own line/output/exit live HERE (not on the
              ;; envelope); `stage`/`id`/`pid`/`lines`/`stopped` are tool-level keys the
              ;; `shell-result` envelope adds around the batch, never on a command entry.
              (expect (true? (get r "started")))
              (expect (false? (get r "timed_out")))
              (expect (false? (get r "timed_out")))
              (expect (= 0 (get r "stdout_omitted_chars")))
              (expect (= 0 (get r "stderr_omitted_chars")))
              ;; Request scope (`cwd`, `timeout_secs`) is GROUP scope: summarised ONCE
              ;; on the envelope and carried here as metadata, never a second copy on
              ;; every entry of a batch.
              (expect (not (contains? r "cwd")))
              (expect (not (contains? r "timeout_secs")))
              ;; …and no truncation flag beside the counts: 0 already IS "nothing lost".
              (expect (not (contains? r "stdout_truncated")))
              (expect (not (contains? r "stderr_truncated")))
              (expect (= 120 (:timeout-secs (meta r))))
              (expect (string? (:dir (meta r)))))))))
  (it "always carries a TOTAL stderr/exit (empty stderr is \"\", not a missing key) and a real cwd"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [r (shell-run* {} "echo only-out")]
                           (expect (= "only-out\n" (get r "stdout")))
                           ;; TOTAL shape: model Python indexes r["stderr"]/r["exit"]
                           ;; directly — a missing key used to KeyError and spin.
                           (expect (= "" (get r "stderr")))
                           (expect (contains? r "stderr"))
                           (expect (= 0 (get r "exit"))))
                         (let [r (shell-run* {} "pwd" {"cwd" "src"})]
                           (expect (string? (:dir (meta r))))
                           (expect (str/ends-with? (:dir (meta r)) "/src")))))))
  (it "treats a non-zero exit as DATA on the command's own entry (not a tool error)"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         ;; shell-run-impl answers the command's own entry, never a tool
                         ;; envelope: a non-zero exit is data ON THAT ENTRY, so a model
                         ;; reading its stdout/stderr/exit never branches on shape.
                         (let [r (shell-run* {} "exit 42")]
                           (expect (= 42 (get r "exit")))
                           (expect (true? (get r "started"))))))))
  (it "kills the process tree on timeout and reports timed_out with nil exit"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [t0 (System/currentTimeMillis)
                            r (shell-run* {} "sleep 30" {"timeout_secs" 1})
                            dt (- (System/currentTimeMillis) t0)]

                           (expect (true? (get r "timed_out")))
                           (expect (nil? (get r "exit")))
                           (expect (< dt 15000)))))))
  (it "keeps BOTH the head and the tail of huge output, dropping only the middle"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         ;; ~72k chars of stdout — far over the head+tail budget.
                         (let
                           [r (shell-run* {}
                                          (str "echo HEAD_MARKER; " "for i in $(seq 1 2000); do "
                                               "echo 'filler-filler-filler-filler'; done; "
                                               "echo TAIL_MARKER"))
                            out (get r "stdout")]

                           (expect (pos? (get r "stdout_omitted_chars")))
                           ;; the opening line is NO LONGER swallowed (the old tail-only cap ate it)
                           (expect (str/includes? out "HEAD_MARKER"))
                           ;; the closing summary still survives
                           (expect (str/includes? out "TAIL_MARKER"))
                           ;; and the drop is made visible, not silent
                           (expect (str/includes? out "chars omitted")))))))
  (it "honors a timeout above the 120s default (up to the 600s cap)"
      ;; `timeout_secs` is group scope on the envelope now, so the clamp is
      ;; asserted on the helper directly instead of burning wall-clock.
      (let [clamp @#'shell/clamp-timeout-secs]
        (expect (= 120 (clamp nil)))
        (expect (= 300 (clamp 300)))
        (expect (= 600 (clamp 5000)))
        (expect (= 1 (clamp 0)))))
  (it "carries the timeout budget as entry metadata alongside timed_out"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [r (shell-run* {} "sleep 30" {"timeout_secs" 1})]
                           (expect (true? (get r "timed_out")))
                           (expect (= 1 (:timeout-secs (meta r))))
                           ;; "exit" stays PRESENT and nil (Python None) on timeout.
                           (expect (contains? r "exit"))
                           (expect (nil? (get r "exit"))))))))
  (it "rejects a cwd outside every allowed filesystem root"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (expect (threw? #(shell-run* {} "pwd" {"cwd" "../.."})))))))
  (it "accepts a sibling cwd granted by the immutable environment snapshot"
      (let
        [parent
         (doto (io/file (System/getProperty "java.io.tmpdir")
                        (str "vis-shell-roots-" (System/nanoTime)))
           (.mkdirs))

         primary
         (doto (io/file parent "workspace") (.mkdirs))

         sibling
         (doto (io/file parent "svar") (.mkdirs))

         env
         {:security/filesystem-roots [(.getCanonicalPath sibling)]
          :jail-policy-fn (constantly {:disabled? true})}]

        (try (binding
               [workspace/*workspace-root*
                (.getCanonicalPath primary)

                workspace/*filesystem-roots*
                nil]

               (let [r (shell-run* env "pwd" {"cwd" "../svar"})]
                 (expect (= (.getCanonicalPath sibling) (str/trim (get r "stdout"))))
                 (expect (= (.getCanonicalPath sibling) (:dir (meta r))))))
             (finally (io/delete-file sibling true)
                      (io/delete-file primary true)
                      (io/delete-file parent true)))))
  (it "allows a descendant of the host filesystem root when the jail is disabled"
      (let
        [home
         (.getCanonicalPath (io/file (System/getProperty "user.home")))

         env
         {:security-policy {:sandbox false} :jail-policy-fn (constantly {:disabled? true})}]

        (binding
          [workspace/*workspace-root*
           (workspace/trunk-root)

           workspace/*filesystem-roots*
           nil]

          (let [r (shell-run* env "pwd" {"cwd" home})]
            (expect (= home (str/trim (get r "stdout"))))
            (expect (= home (:dir (meta r))))))))
  (it "accepts the HOME-relative paths advertised in session access"
      (let
        [home
         (.getCanonicalPath (io/file (System/getProperty "user.home")))

         env
         {:security/filesystem-roots [home] :jail-policy-fn (constantly {:disabled? true})}]

        (binding
          [workspace/*workspace-root*
           (workspace/trunk-root)

           workspace/*filesystem-roots*
           nil]

          ;; `cwd` is the ONLY spelling — `~` expands against the home root.
          (let [r (shell-run* env "pwd" {"cwd" "~"})]
            (expect (= home (str/trim (get r "stdout"))))
            (expect (= home (:dir (meta r))))))))
  (it "accepts an ABSOLUTE cwd that lands inside a workspace root"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [abs (.getCanonicalPath (java.io.File. (str (workspace/cwd))))
                            r (shell-run* {} "pwd" {"cwd" abs})]

                           (expect (string? (:dir (meta r))))
                           (expect (= abs (:dir (meta r)))))
                         ;; an absolute path OUTSIDE every root is still rejected
                         (expect (threw? #(shell-run* {} "pwd" {"cwd" "/"})))))))
  (it "accepts a float timeout but rejects a non-numeric one with a typed error"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         ;; a float timeout is accepted and the entry runs clean
                         (expect (= 0 (get (shell-run* {} "true" {"timeout_secs" 1.0}) "exit")))
                         (let
                           [thrown (try (shell-run* {} "true" {"timeout_secs" "30"})
                                        nil
                                        (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                           (expect (= ::shell/bad-option (:type thrown)))))))))

(defdescribe
  shell-background-test
  (it "registers a session resource, tails logs, and stops cleanly via the registry"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-ext-bg"
               env {:session-id sid}]

              (try (let [reg (:result (shell-bg* env "worker" "echo l1; echo l2; sleep 60"))]
                     (expect (= "running" (get reg "status")))
                     (expect (pos? (get reg "pid"))))
                   (let [rs (resources/list-resources sid)]
                     (expect (= 1 (count rs)))
                     (expect (= "worker" (get (first rs) "id")))
                     (expect (= "shell" (get (first rs) "kind")))
                     (expect (true? (get (first rs) "can_stop"))))
                   (let
                     [r (poll #(:result (shell-logs* env "worker"))
                              #(>= (count (get % "lines")) 2))]
                     ;; `lines` is plain strings and the ONLY copy of the tail — no
                     ;; pre-joined `text` twin, so the payload never doubles.
                     (expect (= "l1" (first (get r "lines"))))
                     (expect (every? string? (get r "lines")))
                     (expect (nil? (get r "text")))
                     (expect (= "running" (get r "status"))))
                   (let [stop (resources/stop! sid "worker")]
                     (expect (= :stopped (:result stop)))
                     (expect (empty? (resources/list-resources sid)))
                     (expect (threw? #(shell-logs* env "worker"))))
                   (finally (resources/stop-all! sid))))))))
  (it "keeps an exited process listed (status :exited) with readable logs + exit"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [sid "shell-ext-exit"
                            env {:session-id sid}]

                           (try (shell-bg* env "quick" "echo done; exit 7")
                                (let
                                  [r (poll #(:result (shell-logs* env "quick"))
                                           #(= "exited" (get % "status")))]
                                  (expect (= 7 (get r "exit")))
                                  (expect (= ["done"] (vec (get r "lines"))))
                                  (expect (nil? (get r "text"))))
                                (let [res (first (resources/list-resources sid))]
                                  (expect (some? res))
                                  (expect (= "failed" (get res "status"))))
                                (finally (resources/stop-all! sid))))))))
  (it
    "returns the SAME live shell (already_running) instead of failing on a duplicate id"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let
            [sid "shell-ext-dup"
             env {:session-id sid}]

            (try (let
                   [first-run (:result (shell-bg* env "dup" "sleep 60"))
                    again (:result (shell-bg* env "dup" "sleep 60"))
                    ;; Re-attaching to a LIVE id needs no command at all: the
                    ;; positional is the thing to START, and it is already
                    ;; started. Demanding it back was a pure dead end.
                    bare (:result (shell* env {"op" "background" "id" "dup"}))]

                   ;; No second process, no thrown failure: the
                   ;; model gets the running shell back with the
                   ;; flag + the logs-op hint, so "start it"
                   ;; cannot dead-end into a retry loop.
                   ;; Both branches return the SAME total shape.
                   (expect (false? (get first-run "already_running")))
                   (expect (= (get first-run "pid") (get again "pid")))
                   (expect (true? (get again "already_running")))
                   (expect (= "running" (get again "status")))
                   (expect (str/includes? (get again "note")
                                          "await shell({\"op\": \"logs\", \"id\": \"dup\"})"))
                   ;; the shared identity core rides EVERY stage
                   (expect (= "background" (get again "stage")))
                   (expect (contains? again "attach"))
                   (expect (contains? again "socket"))
                   (expect (nil? (get first-run "note")))
                   (expect (true? (get bare "already_running")))
                   (expect (= (get first-run "pid") (get bare "pid")))
                   ;; …but a command IS required when nothing is running yet.
                   (expect (threw? #(shell* env {"op" "background" "id" "never-started"}))))
                 (finally (resources/stop-all! sid))))))))
  (it "carries uptime_ms and the shared TOTAL identity core in the logs payload"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-ext-meta"
               env {:session-id sid}]

              (try (shell-bg* env "m" "sleep 60")
                   (let [r (:result (shell-logs* env "m"))]
                     (expect (>= (get r "uptime_ms") 0))
                     ;; TOTAL contract: every op of the ONE shell tool returns the
                     ;; identity keys, with `stage` naming the op that ran.
                     (expect (= "logs" (get r "stage")))
                     (expect (= "sleep 60" (first (map #(get % "command") (get r "commands")))))
                     (expect (contains? r "pid"))
                     ;; Stage-SCOPED: `logs` owns pid/status/uptime, never the background
                     ;; card's attach/socket — no stage carries another stage's keys.
                     (expect (not (contains? r "attach")))
                     (expect (not (contains? r "socket")))
                     (expect (not (contains? r "shown_count")))
                     ;; …but CORE keys stay TOTAL: 0 dropped / nil exit, never absent
                     (expect (contains? r "dropped"))
                     (expect (= 0 (get r "dropped")))
                     (expect (contains? r "exit"))
                     (expect (nil? (get r "exit"))))
                   (finally (resources/stop-all! sid))))))))
  (it "honors the bg op's cwd and reports it on every stage of that shell"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [sid "shell-ext-bg-cwd"
                            env {:session-id sid}]

                           (try (let [b (:result (shell-bg* env "c" "pwd; sleep 60" {"cwd" "src"}))]
                                  ;; The schema advertises `cwd` for run AND bg; a bg that silently
                                  (poll #(get (:result (shell-logs* env "c")) "lines")
                                        #(str/includes? (str/join "\n" %) "/src")
                                        20)
                                  (let [r (:result (shell-logs* env "c"))]
                                    (expect (= (get b "cwd") (get r "cwd")))
                                    (expect (str/includes? (str/join "\n" (get r "lines"))
                                                           "/src"))))
                                (finally (resources/stop-all! sid))))))))
  (it "stops promptly even when the command double-forks a detached daemon"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-ext-nohup"
               env {:session-id sid}]

              (try (shell-bg* env "d" "nohup sleep 120 >/dev/null 2>&1 & echo spawned; sleep 60")
                   (let
                     [t0 (System/currentTimeMillis)
                      stop (resources/stop! sid "d")
                      dt (- (System/currentTimeMillis) t0)]

                     (expect (= :stopped (:result stop)))
                     (expect (< dt 8000))
                     (expect (empty? (resources/list-resources sid))))
                   (finally (resources/stop-all! sid)))))))))

(defdescribe
  shell-wait-test
  (it "waits for completion on the host and returns the final bounded log tail"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [sid (str "shell-wait-exit-" (System/nanoTime))
                            env {:session-id sid}]

                           (try (shell-bg* env "job" "printf 'one\\ntwo\\n'; sleep 0.2; exit 7")
                                (let [r (:result (shell-wait* env "job" 5 1))]
                                  (expect (= "wait" (get r "stage")))
                                  (expect (= "exited" (get r "status")))
                                  (expect (= 7 (get r "exit")))
                                  (expect (false? (get r "timed_out")))
                                  (expect (= 5 (get r "timeout_secs")))
                                  (expect (= ["two"] (get r "lines")))
                                  (expect (= 2 (get r "line_count")))
                                  (expect (number? (get r "duration_ms"))))
                                (finally (resources/stop-all! sid))))))))
  (it "times out without stopping or unregistering a still-running shell"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [sid (str "shell-wait-timeout-" (System/nanoTime))
                            env {:session-id sid}]

                           (try (shell-bg* env "job" "echo started; sleep 30")
                                (let [r (:result (shell-wait* env "job" 1 10))]
                                  (expect (= "wait" (get r "stage")))
                                  (expect (= "running" (get r "status")))
                                  (expect (nil? (get r "exit")))
                                  (expect (true? (get r "timed_out")))
                                  (expect (= 1 (get r "timeout_secs")))
                                  (expect (str/includes? (get r "note") "still running"))
                                  (expect (some? (first (resources/list-resources sid)))))
                                (finally (resources/stop-all! sid)))))))))

(defdescribe shell-send-test
             (it "types into a running background shell's stdin and the program reads it"
                 (with-shell-on
                   (fn []
                     (binding [workspace/*workspace-root* (workspace/trunk-root)]
                       (let
                         [sid "shell-ext-send"
                          env {:session-id sid}]

                         (try (shell-bg* env "echoer" "read x; echo GOT:$x; sleep 60")
                              (let [snt (:result (shell-send* env "echoer" "hi-there"))]
                                (expect (= "running" (get snt "status")))
                                ;; "hi-there" (8) + submitting newline = 9 chars written
                                (expect (= 9 (get snt "sent")))
                                ;; The card must be able to show WHAT was typed, not
                                ;; just how many chars it was.
                                (expect (= "hi-there\n" (get snt "text")))
                                (expect (= "\"hi-there\" ↵" (get snt "keys"))))
                              (let
                                [hit? (fn [r]
                                        (str/includes? (str/join "\n" (get r "lines"))
                                                       "GOT:hi-there"))
                                 r (poll #(:result (shell-logs* env "echoer")) hit?)]

                                (expect (hit? r)))
                              (finally (resources/stop-all! sid))))))))
             (it "refuses a send to an unknown id and to an exited shell"
                 (with-shell-on (fn []
                                  (binding [workspace/*workspace-root* (workspace/trunk-root)]
                                    (let
                                      [sid "shell-ext-send-err"
                                       env {:session-id sid}]

                                      (try (expect (threw? #(shell-send* env "nope" "x")))
                                           (shell-bg* env "gone" "exit 0")
                                           (poll #(:result (shell-logs* env "gone"))
                                                 #(= "exited" (get % "status")))
                                           (expect (threw? #(shell-send* env "gone" "x")))
                                           (finally (resources/stop-all! sid)))))))))

(defdescribe
  shell-bg-lifecycle-op-test
  (it "drives start -> logs -> send -> stop through one options map"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-ext-ops"
               env {:session-id sid}]

              (try (let
                     [start (:result (shell* env
                                             {"commands" ["read x; echo GOT:$x; sleep 30"]
                                              "op" "background"
                                              "id" "ops"}))]
                     (expect (= "running" (get start "status"))))
                   (let [logs (:result (shell* env {"op" "logs" "id" "ops" "n" 5}))]
                     (expect (= "logs" (get logs "stage"))))
                   (let [sent (:result (shell* env {"op" "send" "id" "ops" "text" "hello"}))]
                     (expect (= 6 (get sent "sent"))))
                   (let [stop (:result (shell* env {"op" "stop" "id" "ops"}))]
                     (expect (= "stopped" (get stop "status"))))
                   (finally (resources/stop-all! sid)))))))))

(defdescribe shell-argument-contract-test
             "Every public shell call is one map; process lines are its commands array."
             (it "runs from commands and rejects positional or legacy carriers"
                 (binding [workspace/*workspace-root* (workspace/trunk-root)]
                   (let
                     [r (:result (shell* {} {"commands" ["echo mapped"]}))
                      one (first (get r "commands"))]

                     (expect (= "run" (get r "stage")))
                     (expect (= "echo mapped" (get one "command")))
                     (expect (= "mapped\n" (get one "stdout"))))
                   (expect (threw? #(shell* {} "echo positional")))
                   (expect (threw? #(shell* {} ["echo positional"])))
                   (expect (threw? #(shell* {} {"cmd" "echo legacy"})))
                   (expect (threw? #(shell* {} {"text" "echo wrong"})))))
             (it "uses text only inside the send map"
                 (expect (threw?
                           #(shell* {:session-id "shell-positional-id"} "ghost" {"op" "logs"})))
                 (expect (threw? #(shell* {:session-id "shell-positional-id"}
                                          {"op" "send" "id" "ghost" "commands" ["nope"]})))))

(defdescribe
  shell-render-test
  (it "renders the run op like a REPL-style collapsible card"
      (let
        [card (render-shell-run-result
                {"command" "echo hi" "exit" 0 "duration_ms" 12 "stdout" "hi"})]
        (expect (= "$ echo hi (success) · 12ms" (:summary card)))
        (expect (str/includes? (:body card) "**COMMAND**"))
        (expect (str/includes? (:body card) "**STATUS**"))
        (expect (str/includes? (:body card) "**STDOUT**"))))
  (it "separates a compound command onto its own lines in the COMMAND card"
      (let [card (render-shell-run-result {"command" "a; b && c" "exit" 0 "duration_ms" 1})]
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
                                         {"command" "grep nope missing" "exit" 2 "duration_ms" 34}))
                             "$ grep nope missing (failure) · exit 2 · 34ms"))
      (expect (str/includes? (:summary (render-shell-run-result {"command" "make test"
                                                                 "timed_out" true
                                                                 "timeout_secs" 5
                                                                 "duration_ms" 5000}))
                             "$ make test (failure) · timed out after 5s · 5.0s")))
  (it "normalizes terminal controls in rendered output without changing the native result"
      (let
        [stdout
         "\u001b[0;32m✓ PASS\u001b[0m\rnext\b!"

         result
         {"command" "tests" "exit" 0 "stdout" stdout}

         run-card
         (render-shell-run-result result)

         logs-card
         (render-shell-logs-result {"id" "tests"
                                    "status" "running"
                                    "lines" ["\u001b]0;title\u0007\u001b[31mready\u001b[0m"]})]

        (expect (= stdout (get result "stdout")))
        (expect (str/includes? (:body run-card) "✓ PASS\nnext!"))
        (expect (not (str/includes? (:body run-card) "\u001b")))
        (expect (not (str/includes? (:body run-card) "[0;32m")))
        (expect (str/includes? (:body logs-card) "ready"))
        (expect (not (str/includes? (:body logs-card) "\u001b")))))
  (it "renders background lifecycle/log cards with expandable sections"
      (let
        [bg
         (render-shell-bg-result {"id" "srv"
                                  "commands" [{"command" "npm run dev" "started" true}]
                                  "pid" 123
                                  "status" "running"
                                  "attach" "vis extension shell attach srv"})

         logs
         (render-shell-logs-result
           {"id" "srv" "status" "running" "lines" ["ready"] "line_count" 1 "uptime_ms" 1500})]

        (expect (str/includes? (:summary bg) "▸ background `srv` running · pid 123"))
        (expect (str/includes? (:body bg) "**COMMAND**"))
        (expect (str/includes? (:summary logs) "◷ `srv` running · 1 lines · 1.5s"))
        (expect (str/includes? (:body logs) "**LOGS**"))))
  (it "shows the KEYSTROKES a send typed, naming every control character"
      (let
        [typed
         (render-shell-send-result
           {"id" "ops" "status" "running" "sent" 6 "text" "hello\n" "keys" (keys-label "hello\n")})

         ;; A send is frequently ENTIRELY non-printing (Ctrl-C, Esc, a bare Enter):
         ;; the old card said "sent 1 chars" and the reader learned nothing.
         ctrl
         (render-shell-send-result
           {"id" "ops" "status" "running" "sent" 1 "text" "\u0003" "keys" (keys-label "\u0003")})]

        (expect (= "\"hello\" ↵" (keys-label "hello\n")))
        (expect (= "C-c" (keys-label "\u0003")))
        (expect (= "Esc ⇥ \"y\" ↵" (keys-label "\u001b\ty\n")))
        (expect (nil? (keys-label "")))
        (expect (= "↵ `ops` sent \"hello\" ↵" (:summary typed)))
        (expect (str/includes? (:body typed) "**KEYS**"))
        (expect (str/includes? (:body typed) "keys: \"hello\" ↵"))
        (expect (= "↵ `ops` sent C-c" (:summary ctrl)))
        (expect (str/includes? (:body ctrl) "C-c"))
        ;; Falls back to the payload when an older result carries no `keys`.
        (expect (= "↵ `ops` sent \"y\" ↵"
                   (:summary (render-shell-send-result {"id" "ops" "sent" 2 "text" "y\n"}))))
        (expect (= "↵ `ops` sent 0 chars"
                   (:summary (render-shell-send-result {"id" "ops" "sent" 0})))))))

(defdescribe shell-batch-test
             (it "runs the commands array in order and returns one entry per line"
                 (binding [workspace/*workspace-root* (workspace/trunk-root)]
                   (let [r (:result (shell* {} {"commands" ["printf first" "printf second"]}))]
                     (expect (= ["first" "second"]
                                (mapv #(str/trim (get % "stdout")) (get r "commands")))))))
             (it "rejects missing, blank, and non-map process requests"
                 (expect (threw? #(shell* {} {})))
                 (expect (threw? #(shell* {} {"commands" []})))
                 (expect (threw? #(shell* {} {"commands" [""]})))
                 (expect (threw? #(shell* {} ["printf first"])))))

(defdescribe
  shell-native-contract-test
  (it "advertises exactly one native shell tool covering the lifecycle"
      (expect (= ["shell"]
                 (mapv :ext.symbol/name (filter :ext.symbol/native-tool? shell/shell-symbols))))
      (expect (= 1 (count shell/shell-symbols))))
  (it "documents and schemas the one-map commands grammar"
      (let
        [d
         (:ext.symbol/description shell/shell-symbol)

         props
         (get-in shell/shell-symbol [:ext.symbol/schema :properties])]

        (expect (str/includes? d "ONE options map"))
        (expect (str/includes? d "never a positional string or array"))
        (expect (contains? props "commands"))
        (expect (contains? props "text"))
        (expect (not (contains? props "cmd")))
        (expect (= ["run" "background" "logs" "wait" "send" "stop"] (get-in props ["op" :enum])))
        (expect (= "array" (get-in props ["commands" :type])))))
  (it "closes every native shell input schema"
      (doseq [s shell/shell-symbols]
        (expect (false? (get-in s [:ext.symbol/schema :additionalProperties]))))))

(defdescribe shell-extension-shape-test
             (it "is a registered builtin extension exposing the ONE bare `shell` symbol"
                 (expect (= "foundation-shell" (:ext/name shell/vis-extension)))
                 ;; No engine alias any more: `shell` is bound BARE in the flat sandbox
                 ;; next to git / cat / grep, so there is no `shell.run(…)` namespace.
                 (expect (true? (get-in shell/vis-extension [:ext/engine :ext.engine/builtin?])))
                 (expect (nil? (get-in shell/vis-extension [:ext/engine :ext.engine/alias])))
                 (expect (= ['shell] (mapv :ext.symbol/symbol shell/shell-symbols)))))

(defdescribe
  macos-jailed-pty-e2e-test
  (it
    "keeps PTY/send/attach while nested shells inherit Seatbelt filesystem denial"
    (if (or (not (str/starts-with? (System/getProperty "os.name" "") "Mac"))
            (not (process-jail/supported?))
            (= "1" (System/getenv "VIS_SEATBELT_ACTIVE")))
      (expect true)
      (let
        [ws
         (doto (io/file (System/getProperty "java.io.tmpdir")
                        (str "vis-pty-e2e-" (System/nanoTime)))
           (.mkdirs))

         secret
         (io/file (System/getProperty "user.home") (str ".vis-pty-secret-" (System/nanoTime)))

         sid
         (str "pty-e2e-" (System/nanoTime))

         env
         {:session-id sid
          :jail-policy-fn (constantly {:roots-fn (constantly [(.getPath ws)])
                                       :net-enabled? false
                                       :deny-read [(.getPath secret)]})}

         cmd
         (str "test -t 0 && echo TTY_OK; "
              "if bash --noprofile --norc -lc 'cat " (.getPath secret)
              " >/dev/null 2>&1'; then echo ESCAPED; else echo NESTED_SEALED; fi; "
              "echo READY; read x; echo GOT:$x; sleep 2")]

        (spit secret "TOP-SECRET")
        (try (binding [workspace/*workspace-root* (.getCanonicalPath ws)]
               (let
                 [started (:result (shell-bg* env "pty" cmd))
                  ready? (fn [r]
                           (str/includes? (str/join "\n" (get r "lines")) "READY"))
                  before (poll #(:result (shell-logs* env "pty")) ready?)]

                 (expect (= "1"
                            (some->> (get before "lines")
                                     (some #(when (str/includes? (str %) "TTY_OK") "1")))))
                 (expect (str/includes? (str/join "\n" (get before "lines")) "NESTED_SEALED"))
                 (expect (not (str/includes? (str/join "\n" (get before "lines")) "ESCAPED")))
                 (expect (string? (get started "attach")))
                 (expect (string? (get started "socket")))
                 (shell-send* env "pty" "hello")
                 (let
                   [after (poll #(:result (shell-logs* env "pty"))
                                #(str/includes? (str/join "\n" (get % "lines")) "GOT:hello"))]
                   (expect (str/includes? (str/join "\n" (get after "lines")) "GOT:hello")))))
             (finally (resources/stop-all! sid)
                      (io/delete-file secret true)
                      (io/delete-file ws true)))))))

;; =============================================================================
;; The PYTHON SANDBOX surface — `python_execution` is the model's main hand, so
;; the shell has to be usable from ordinary Python, not only as a native tool.
;; =============================================================================

(defn- py-ctx
  "A real sandbox Context wired with the REAL built-in bindings, so `shell` here
   is the genuine tool (not a stub) resolving `env` at call time."
  ^Context [env]
  (:python-context (ep/create-python-context (extension/builtin-sandbox-bindings (constantly
                                                                                   env)))))

(defn- py
  "Eval one Python expression in `c` and marshal the value back to Clojure."
  [^Context c code]
  (ep/->clj (.eval c "python" code)))

(defdescribe
  python-sandbox-surface-test
  "The bare Python global has only the one-map shell grammar."
  (it "binds exactly one shell symbol into sandbox globals, with a doc"
      (let [bind (extension/builtin-sandbox-bindings (constantly nil))]
        (expect (= ['shell] (sort (filter #(str/includes? (name %) "shell") (keys bind)))))
        (expect (contains? (extension/sandbox-symbol-docs) 'shell))))
  (it "documents map calls and rejects positional commands"
      (let
        [source-doc
         (:doc (meta #'shell/shell))

         d
         (str (py (py-ctx {}) "doc('shell')"))]

        (expect (str/includes? source-doc "await shell({\"commands\": [\"git status\"]})"))
        (expect (not (str/includes? source-doc "await shell(\"git status\")")))
        (expect (str/includes? d "`commands`"))))
  (it
    "runs and backgrounds commands through the bare shell global using maps"
    (let
      [sid
       (str "py-shell-" (System/nanoTime))

       c
       (py-ctx {:session-id sid})]

      (try
        (expect (= ["run" "hi" "echo hi"]
                   (py c
                       (str "r = __vis_settle__(shell({'commands':['echo hi']}))\n"
                            "x = r['commands'][0]\n"
                            "[r['stage'], x['stdout'].strip(), x['command']]"))))
        (expect
          (=
            ["background" "logs" "stop"]
            (py
              c
              (str
                "b = __vis_settle__(shell({'commands':['echo alive; sleep 30'], 'op':'background', 'id':'pyjob'}))\n"
                "l = __vis_settle__(shell({'op':'logs','id':'pyjob','n':10}))\n"
                "s = __vis_settle__(shell({'op':'stop','id':'pyjob'}))\n"
                "[b['stage'], l['stage'], s['stage']]"))))
        (finally (resources/stop-all! sid)))))
  (it "removes the binding when the shell toggle is off"
      (let
        [before
         (toggles/enabled? "shell")

         c
         (py-ctx {})

         env
         {:extensions (atom (vec (extension/registered-extensions)))
          :active-extensions (atom [])
          :python-context c}]

        (try (toggles/set-enabled! "shell" false)
             (lp/sync-active-extension-symbols! env)
             (expect (false? (boolean (py c "'shell' in globals()"))))
             (finally (toggles/set-enabled! "shell" before)))))
  (it
    "routes map-only logs and stop calls through the native Python bridge"
    ;; Regression: a stale positional wrapper invoked shell-dispatch as
    ;; (env command options), which made these lifecycle maps fail before the
    ;; dispatcher ran with "Wrong number of args ... shell-dispatch".
    (let
      [sid
       (str "py-shell-lifecycle-" (System/nanoTime))

       c
       (py-ctx {:session-id sid})]

      (try
        (expect
          (=
            ["background" "logs" "stop"]
            (py
              c
              (str
                "job = "
                (pr-str sid)
                "\n"
                "b = __vis_settle__(shell({'commands':['echo ready; sleep 30'], 'op':'background', 'id':job}))\n"
                "l = __vis_settle__(shell({'op':'logs', 'id':job, 'n':1}))\n"
                "s = __vis_settle__(shell({'op':'stop', 'id':job}))\n"
                "[b['stage'], l['stage'], s['stage']]"))))
        (finally (resources/stop-all! sid))))))

(defdescribe
  python-shell-wait-surface-test
  (it
    "exposes the host-side wait stage through ordinary Python shell calls"
    (let
      [sid
       (str "py-shell-wait-" (System/nanoTime))

       c
       (py-ctx {:session-id sid})]

      (try
        (expect
          (=
            ["wait" "exited" 0 ["done"]]
            (py
              c
              (str
                "job = " (pr-str sid)
                "\n"
                "__vis_settle__(shell({'commands':['sleep 0.1; echo done'], 'op':'background', 'id':job}))\n"
                "w = __vis_settle__(shell({'op':'wait', 'id':job, 'timeout_secs':5}))\n"
                "[w['stage'], w['status'], w['exit'], w['lines']]"))))
        (finally (resources/stop-all! sid))))))
