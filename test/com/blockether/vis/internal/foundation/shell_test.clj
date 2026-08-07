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

(def ^:private shell-send* @#'shell/shell-send-impl)

(def ^:private shell* @#'shell/shell-dispatch)

(def ^:private render-shell-run-result @#'shell/render-shell-run-result)

(def ^:private render-shell-bg-result @#'shell/render-shell-bg-result)

(def ^:private render-shell-logs-result @#'shell/render-shell-logs-result)

(def ^:private render-shell-send-result @#'shell/render-shell-send-result)

(def ^:private keys-label @#'shell/keys-label)

(def ^:private format-shell-command @#'shell/format-shell-command)

(def ^:private render-shell-call @#'shell/render-shell-call)

(defn- with-shell-on
  "Shell is unconditionally available now; kept as a thin pass-through so the
   existing call sites read unchanged."
  [f]
  (f))

(defn- threw?
  "lazytest has no `thrown?`; run `thunk` and report whether it threw."
  [thunk]
  (try (thunk) false (catch Throwable _ true)))

(defn- throw-message
  "The message `thunk` throws, or nil when it returns — a refusal is only useful
   if it says the right thing."
  [thunk]
  (try (thunk) nil (catch Throwable t (ex-message t))))

(defn- poll
  "Re-run `thunk` until `pred` holds (~5s), returning the value."
  ([thunk pred] (poll thunk pred 50))
  ([thunk pred tries]
   (loop [i 0]
     (let [v (thunk)]
       (cond (pred v) v
             (>= i (long tries)) (throw (ex-info "poll exhausted" {:last v}))
             :else (do (Thread/sleep 100) (recur (inc i))))))))

(defdescribe shell-env-injection-test
             (it "uses declarative env injection rather than a before middleware shim"
                 (expect (true? (:ext.symbol/inject-env? shell/shell-run-symbol)))
                 (expect (nil? (:ext.symbol/before-fn shell/shell-run-symbol))))
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
  (it "captures a machine-readable payload WHOLE so the caller can parse it"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            ;; ~45k chars of JSON — the size of one ordinary `gh … --json` reply. The
            ;; old 4k+12k capture spliced its omitted-marker at char 4000, so every
            ;; `json.loads(r["commands"][0]["stdout"])` died with "Invalid control
            ;; character" on output a caller could trivially hold.
            (let
              [r (shell-run*
                   {}
                   (str "printf '['; for i in $(seq 1 1000); do "
                        "printf '{\"n\":%s,\"pad\":\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"}' \"$i\"; "
                        "[ \"$i\" -lt 1000 ] && printf ','; done; printf ']'"))
               out (get r "stdout")]

              (expect (= 0 (get r "stdout_omitted_chars")))
              (expect (nil? (get r "note")))
              (expect (> (count out) 40000))
              ;; whole and well-formed: both ends present and every element survived
              (expect (str/starts-with? out "["))
              (expect (str/ends-with? (str/trimr out) "]"))
              (expect (= 1000 (count (re-seq #"\"n\":" out))))
              (expect (not (str/includes? out "chars omitted"))))))))
  (it "keeps BOTH the head and the tail of huge output, dropping only the middle"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            ;; ~700k chars of stdout — far over the head+tail capture budget.
            (let
              [r (shell-run* {}
                             (str "echo HEAD_MARKER; " "for i in $(seq 1 12000); do "
                                  "echo 'filler-filler-filler-filler-filler-filler'; done; "
                                  "echo TAIL_MARKER"))
               out (get r "stdout")]

              (expect (pos? (get r "stdout_omitted_chars")))
              ;; the opening line is NO LONGER swallowed (the old tail-only cap ate it)
              (expect (str/includes? out "HEAD_MARKER"))
              ;; the closing summary still survives
              (expect (str/includes? out "TAIL_MARKER"))
              ;; and the drop is made visible, not silent
              (expect (str/includes? out "chars omitted"))
              ;; …including WHY a parser will now choke on it
              (expect (str/includes? (get r "note") "stdout truncated"))
              (expect (str/includes? (get r "note") "no longer parses")))))))
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
                   (expect (str/includes? (get again "note") "await shell_logs(\"dup\")"))
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
  (it "starts a background shell WITHOUT an id, naming it after the command"
      ;; Regression: `{op: "background", commands: […]}` was rejected for a missing
      ;; `id` — a name the caller had to invent for the ONE stage that acts on no
      ;; prior handle, so the most natural start was a hard failure. It now names
      ;; itself after the program, re-issuing the same script resolves to the SAME
      ;; shell instead of a duplicate, and a different script never takes a live id.
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-ext-auto-id"
               env {:session-id sid}]

              (try (let
                     [started (:result (shell* env {"op" "background" "commands" ["sleep 60"]}))
                      again (:result (shell* env {"op" "background" "commands" ["sleep 60"]}))
                      other (:result (shell* env
                                             {"op" "background" "commands" ["cd / && sleep 61"]}))]

                     (expect (= "sleep" (get started "id")))
                     (expect (= "running" (get started "status")))
                     (expect (false? (get started "already_running")))
                     (expect (= "sleep" (get again "id")))
                     (expect (true? (get again "already_running")))
                     (expect (= (get started "pid") (get again "pid")))
                     (expect (= "sleep-2" (get other "id")))
                     (expect (not= (get started "pid") (get other "pid")))
                     ;; every other stage still names the shell it acts on
                     (expect (threw? #(shell* env {"op" "logs"}))))
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
  ;; Regression, issue #shell-timings: `uptime_ms` was computed as `now -
  ;; started-at` even for a process that had ALREADY exited, so a job that ran for
  ;; three seconds reported "uptime: 10.1m" as soon as the reader came back to it
  ;; later — the one timing on the card was the age of the read, not of the job.
  (it "freezes uptime_ms at the moment the process exited"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid (str "shell-uptime-frozen-" (System/nanoTime))
               env {:session-id sid}]

              (try (shell-bg* env "quick" "echo done")
                   (poll #(:result (shell-logs* env "quick")) #(= "exited" (get % "status")))
                   (let
                     [first-read (:result (shell-logs* env "quick"))
                      _ (Thread/sleep 500)
                      later-read (:result (shell-logs* env "quick"))]

                     (expect (= "exited" (get later-read "status")))
                     ;; A lifetime is a fact about the PROCESS, not about when it is read.
                     (expect (= (get first-read "uptime_ms") (get later-read "uptime_ms"))))
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
                   (finally (resources/stop-all! sid))))))))
  (it "atomically deduplicates simultaneous starts of the same session/id"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-ext-concurrent-dup"
               env {:session-id sid}
               gate (promise)
               starts (mapv (fn [_]
                              (future @gate (:result (shell-bg* env "same" "sleep 60"))))
                            (range 20))]

              (try (deliver gate true)
                   (let
                     [results (mapv #(deref % 15000 ::timed-out) starts)
                      completed (remove #{::timed-out} results)
                      pids (set (map #(get % "pid") completed))]

                     ;; The live check and PTY spawn used to race: every caller saw
                     ;; no entry, launched a child, and overwrote the sole stop
                     ;; handle. Only one caller may spawn; every other result must
                     ;; identify that exact process as already running.
                     (expect (= 20 (count completed)))
                     (expect (= 1 (count pids)))
                     (expect (= 1 (count (remove #(get % "already_running") completed))))
                     (expect (= 19 (count (filter #(get % "already_running") completed))))
                     (let [registered (resources/list-resources sid)]
                       (expect (= 1 (count registered)))
                       (expect (= (first pids) (get (first registered) "pid")))))
                   (finally (resources/stop-all! sid))))))))
  (it
    "serializes an external stop callback with replacement of the same shell id"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let
            [sid "shell-ext-stop-replace-race"
             env {:session-id sid}
             kill-var (ns-resolve 'com.blockether.vis.internal.foundation.shell 'kill-tree!)
             original-kill (var-get kill-var)
             entered (promise)
             release (promise)]

            (try (shell-bg* env "same" "exit 0")
                 (poll #(:result (shell-logs* env "same")) #(= "exited" (get % "status")))
                 (with-redefs-fn {kill-var (fn [p]
                                             (deliver entered true)
                                             @release
                                             (original-kill p))}
                   (fn []
                     (let [stopping (future (resources/stop! sid "same"))]
                       @entered
                       (let [starting (future (shell-bg* env "same" "sleep 60"))]
                         ;; The replacement waits behind teardown instead of being
                         ;; installed and then erased by the old callback's keyed drop.
                         (expect (= ::blocked (deref starting 50 ::blocked)))
                         (deliver release true)
                         (expect (= :stopped (:result @stopping)))
                         (let [replacement (:result @starting)]
                           (expect (false? (get replacement "already_running")))
                           (expect (= "running" (get replacement "status")))
                           (expect (= (get replacement "pid")
                                      (get (:result (shell-logs* env "same")) "pid"))))))))
                 (finally (deliver release true) (resources/stop-all! sid))))))))
  (it
    "prevents an exiting old pump from publishing status onto its replacement"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let
            [sid (str "shell-ext-exit-replace-race-" (System/nanoTime))
             env {:session-id sid}
             update-var #'resources/update!
             original-update (var-get update-var)
             entered (promise)
             release (promise)]

            (try (with-redefs-fn {update-var (fn [& args]
                                               (when (= sid (str (first args)))
                                                 (deliver entered true)
                                                 @release)
                                               (apply original-update args))}
                   (fn []
                     (shell-bg* env "same" "exit 7")
                     (expect (not= ::timed-out (deref entered 5000 ::timed-out)))
                     (let [starting (future (shell-bg* env "same" "sleep 60"))]
                       ;; Exit finalization owns the same lifecycle lock as replacement.
                       ;; Without it, the replacement starts and the old delayed update
                       ;; marks that new resource exited/failed.
                       (expect (= ::blocked (deref starting 50 ::blocked)))
                       (deliver release true)
                       (let
                         [replacement (:result (deref starting 10000 ::timed-out))
                          registered (resources/get-resource sid "same")]

                         (expect (not= ::timed-out replacement))
                         (expect (= "running" (get replacement "status")))
                         (expect (= (get replacement "pid") (get registered "pid")))
                         (expect (= "running" (get registered "status")))))))
                 (finally (deliver release true) (resources/stop-all! sid)))))))))


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
                 (expect (threw? #(shell* {:session-id "shell-positional-id"} "ghost")))
                 (expect (threw? #(shell* {:session-id "shell-positional-id"}
                                          {"op" "send" "id" "ghost" "commands" ["nope"]})))))

;; Regression, issue #shell-lifecycle-shape: lifecycle calls carrying a stale `commands`
;; field were rejected before `logs`/`wait` could act on the requested background shell.
(defdescribe
  shell-lifecycle-coercion-test
  (it "ignores commands on lifecycle operations"
      (let [sid (str "shell-coercion-" (System/nanoTime))]
        (try (shell* {:session-id sid}
                     {"commands" ["echo ready; sleep 30"] "op" "background" "id" sid})
             (let
               [r (:result (shell* {:session-id sid}
                                   {"op" "logs" "id" sid "commands" [{"op" "wait" "id" "wrong"}]}))]
               (expect (= "logs" (get r "stage"))))
             (finally (resources/stop-all! sid))))))
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
  (it "keeps the line structure a multi-line command was written with"
      ;; REGRESSION: the COMMAND pretty-printer trimmed every line and deleted
      ;; every blank one, so a script's paragraph break was welded shut and a
      ;; block's indentation was flattened. Only the whitespace the SPLIT itself
      ;; introduces may go.
      ;; a blank line between two paragraphs survives, exactly once
      (expect (= "set -e\n\necho hi\n\necho bye"
                 (format-shell-command "set -e\n\necho hi\n\n\necho bye")))
      ;; indentation inside a block is the author's, not noise
      (expect (= "if [ -f x ];\nthen\n  echo yes\nfi"
                 (format-shell-command "if [ -f x ]; then\n  echo yes\nfi")))
      ;; blank head/tail never reaches the card, and an operator break does not
      ;; indent the line it creates
      (expect (= "a &&\nb" (format-shell-command "\n\na && b\n\n"))))
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

        (expect (str/includes? (:summary bg) "▸ `srv` running · pid 123"))
        (expect (str/includes? (:body bg) "**COMMAND**"))
        (expect (str/includes? (:summary logs) "◷ `srv` running · 1 lines · 1.5s"))
        (expect (str/includes? (:body logs) "**LOGS**"))))
  ;; Regression, issue #shell-timings: a `wait` that burned its whole budget
  ;; rendered exactly like a `logs` peek — "◷ `rel` running · 2 lines / 10 total ·
  ;; 10.1m" — never saying it timed out, and its only timing was the PROCESS
  ;; uptime rather than how long the wait itself had blocked.
  (it
    "says a wait timed out and reports the wait's OWN duration, not the uptime"
    (let
      [timed-out
       (render-shell-logs-result {"stage" "wait"
                                  "id" "rel"
                                  "status" "running"
                                  "lines" ["21:33:37 in_progress"]
                                  "line_count" 10
                                  "uptime_ms" 606000
                                  "duration_ms" 600031
                                  "timed_out" true
                                  "timeout_secs" 600
                                  "until" "DONE "
                                  "is_matched" false})

       matched
       (render-shell-logs-result {"stage" "wait"
                                  "id" "deep"
                                  "status" "running"
                                  "lines" ["DEPTHDONE"]
                                  "line_count" 14
                                  "uptime_ms" 8400
                                  "duration_ms" 4900
                                  "timed_out" false
                                  "timeout_secs" 300
                                  "until" "DEPTHDONE"
                                  "is_matched" true
                                  "matched" "DEPTHDONE"})]

      (expect (str/includes? (:summary timed-out) "⏱ `rel` timed out after 600s"))
      (expect (str/includes? (:summary timed-out) "· waited 10.0m"))
      (expect (str/includes? (:body timed-out) "timeout: 600s"))
      (expect (str/includes? (:body timed-out) "waited: 10.0m"))
      ;; The process clock stays readable — it just is not this call's timing.
      (expect (str/includes? (:body timed-out) "uptime: 10.1m"))
      ;; A wait a condition ended says how long IT took, not the job's age.
      (expect (str/includes? (:summary matched) "◷ `deep` running · matched"))
      (expect (str/includes? (:summary matched) "· waited 4.9s"))
      (expect (not (str/includes? (:summary matched) "8.4s")))))
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

(defdescribe
  shell-batch-test
  (it "runs the commands array in order and returns one entry per line"
      (binding [workspace/*workspace-root* (workspace/trunk-root)]
        (let [r (:result (shell* {} {"commands" ["printf first" "printf second"]}))]
          (expect (= ["first" "second"] (mapv #(str/trim (get % "stdout")) (get r "commands")))))))
  (it "rejects missing, blank, and non-map process requests"
      (expect (threw? #(shell* {} {})))
      (expect (threw? #(shell* {} {"commands" []})))
      (expect (threw? #(shell* {} {"commands" [""]})))
      (expect (threw? #(shell* {} ["printf first"]))))
  (it "coerces a bare command string into the batch of one"
      ;; `commands` is an array, but a lone command line has exactly one
      ;; reading — it is wrapped instead of failing the call, and the
      ;; result still carries it as the batch it always was.
      (binding [workspace/*workspace-root* (workspace/trunk-root)]
        (let [r (:result (shell* {} {"commands" "printf lone"}))]
          (expect (= ["printf lone"] (mapv #(get % "command") (get r "commands"))))
          (expect (= "lone" (str/trim (get-in r ["commands" 0 "stdout"])))))))
  (it "coerces an argv array entry into one quoted bash line"
      ;; The habitual `git`-shaped spelling: tokens instead of a line.
      ;; Each token stays ONE argument, so spaces survive quoting.
      (binding [workspace/*workspace-root* (workspace/trunk-root)]
        (let [r (:result (shell* {} {"commands" [["printf" "%s" "two words"]]}))]
          (expect (= ["printf %s 'two words'"] (mapv #(get % "command") (get r "commands"))))
          (expect (= "two words" (str/trim (get-in r ["commands" 0 "stdout"]))))))))

(defdescribe
  shell-mistaken-shape-test
  ;; Both of these were got wrong by a caller mid-task, not imagined: the `git`
  ;; habit put this call's OWN options inside `commands`. A wrong shape has to say
  ;; where the argument belongs — restating its type is what makes the tool look broken.
  (it "names the top-level lifecycle arguments when an options map lands in `commands`"
      (let [msg (throw-message #(shell* {} {"commands" [{"op" "logs" "id" "build" "n" 30}]}))]
        (expect (some? msg))
        (expect (str/includes? msg "TOP-LEVEL"))
        (expect (str/includes? msg "\"op\": \"logs\""))))
  (it "coerces a java.util.List entry, the Python spelling of that argv array"
      (binding [workspace/*workspace-root* (workspace/trunk-root)]
        (let
          [argv (java.util.ArrayList. ["printf" "%s" "two words"])
           r (:result (shell* {} {"commands" [argv]}))]

          (expect (= ["printf %s 'two words'"] (mapv #(get % "command") (get r "commands"))))
          (expect (= "two words" (str/trim (get-in r ["commands" 0 "stdout"]))))))))

(defdescribe
  shell-native-contract-test
  ;; Regression, un-unified shell: ONE `shell` tool with an `op` enum made the model
  ;; disambiguate five mutually-exclusive shapes on every call.
  (it "advertises five native shell tools, one per verb"
      (expect (= ["shell_run" "shell_background" "shell_logs" "shell_type" "shell_stop"]
                 (mapv :ext.symbol/name (filter :ext.symbol/native-tool? shell/shell-symbols))))
      (expect (= 5 (count shell/shell-symbols))))
  (it "gives each tool a satisfiable schema with no op discriminator and no blocking knobs"
      (doseq [s shell/shell-symbols]
        (let [props (get-in s [:ext.symbol/schema :properties])]
          ;; Closed input, and the merged tool's three confusions are gone everywhere:
          ;; the discriminator, the wait predicate, and the timeout that meant three things.
          (expect (false? (get-in s [:ext.symbol/schema :additionalProperties])))
          (expect (not (contains? props "op")))
          (expect (not (contains? props "until")))
          (expect (not (contains? props "timeout_secs")))
          (expect (not (contains? props "cmd"))))))
  (it "puts commands only on the two tools that start a process, and text only on type"
      (let
        [props (fn [s]
                 (get-in s [:ext.symbol/schema :properties]))]
        (expect (= "array" (get-in (props shell/shell-run-symbol) ["commands" :type])))
        (expect (= "array" (get-in (props shell/shell-background-symbol) ["commands" :type])))
        (expect (not (contains? (props shell/shell-logs-symbol) "commands")))
        (expect (contains? (props shell/shell-type-symbol) "text"))
        (expect (= ["id"] (get-in shell/shell-stop-symbol [:ext.symbol/schema :required])))))
  (it "points the background verbs at the Python loop that replaced `wait`"
      (expect (str/includes? (:ext.symbol/description shell/shell-run-symbol) "python_execution"))
      (expect (str/includes? (:ext.symbol/description shell/shell-logs-symbol)
                             "python_execution"))))

(defdescribe shell-extension-shape-test
             (it "is a registered builtin extension exposing the ONE bare `shell` symbol"
                 (expect (= "foundation-shell" (:ext/name shell/vis-extension)))
                 ;; No engine alias any more: `shell` is bound BARE in the flat sandbox
                 ;; next to git / cat / grep, so there is no `shell.run(…)` namespace.
                 (expect (true? (get-in shell/vis-extension [:ext/engine :ext.engine/builtin?])))
                 (expect (nil? (get-in shell/vis-extension [:ext/engine :ext.engine/alias])))
                 (expect (= '[shell-run shell-background shell-logs shell-type shell-stop]
                            (mapv :ext.symbol/symbol shell/shell-symbols)))))

(defdescribe
  shell-pending-call-render-test
  (it "renders a PENDING run batch with the finished card's own COMMAND section"
      (let
        [commands
         ["echo one && echo two" "ls -1"]

         display
         (render-shell-call "run" {"commands" commands})]

        ;; Built by the SAME section builder as the completed card, so the running
        ;; block IS the block it becomes — no pending dialect, no comment band.
        (expect (= (str "**COMMAND**\n```bash\n"
                        (str/join "\n" (map format-shell-command commands))
                        "\n```")
                   (:render display)))
        (expect (not (str/includes? (:render display) "#")))
        (expect (not (str/includes? (:render display) "{")))
        ;; …under the SAME headline the finished card wears, with the outcome
        ;; replaced by what the call is doing; a batch counts the rest.
        (expect (= "$ echo one && echo two · +1 more (running)" (:summary display)))))
  (it "accepts the ONE-COMMAND spelling and keyword-keyed input"
      (expect (= {:summary "$ ls -1 (running)" :render "**COMMAND**\n```bash\nls -1\n```"}
                 (render-shell-call "run" {:commands "ls -1"}))))
  (it "wears the background START headline, which comes from the TOOL and not the arguments"
      ;; A background start is a lifecycle card, not a run: it reports the handle
      ;; the session will keep, exactly like the finished `▸ … started`.
      (expect (= {:summary "▸ `dev` starting"
                  :render (str "**COMMAND**\n```bash\nnpm run dev\n```\n\n"
                               "**STATUS**\n```\nid: dev\n```")}
                 (render-shell-call "background" {"id" "dev" "commands" ["npm run dev"]}))))
  (it "reports a `type` by its keystroke label, never a byte count"
      (expect (= {:summary "↵ `dev` sending \"y\" ↵"
                  :render "**STATUS**\n```\nid: dev\nkeys: \"y\" ↵\n```"}
                 (render-shell-call "send" {"id" "dev" "text" "y\n"}))))
  (it "shows a lifecycle target's OWN command, read live from the registry"
      ;; A logs/type/stop runs no command of its own, but the bash it acts on
      ;; is right there in `bg-procs` — so its pending card carries a real COMMAND
      ;; section too instead of narrating the stage in prose.
      (let
        [sid
         (str (random-uuid))

         id
         "pending-live-shell"]

        (try
          (swap! @#'shell/bg-procs assoc-in [sid id] {:script "npm run dev"})
          (expect
            (= (str "**COMMAND**\n```bash\nnpm run dev\n```\n\n" "**STATUS**\n```\nid: " id "\n```")
               (:render (render-shell-call "logs" {"id" id}))))
          (finally (swap! @#'shell/bg-procs dissoc sid)))
        ;; No live shell answers to that id: the STATUS rows stand alone.
        (expect (= (str "**STATUS**\n```\nid: " id "\n```")
                   (:render (render-shell-call "logs" {"id" id}))))))
  (it "keeps the raw invocation when there is neither a command nor a target"
      (expect (nil? (render-shell-call "run" {})))
      ;; A malformed batch is the CALL's error to report, never this preview's.
      (expect (nil? (render-shell-call "run" {"commands" [""]}))))
  (it "publishes a renderer under EVERY tool's wire name so the loop can reach it"
      (let [rs (extension/native-tool-start-call-renderers [shell/vis-extension])]
        (expect (= ["shell_run" "shell_background" "shell_logs" "shell_type" "shell_stop"]
                   (vec (keys rs))))
        (expect (every? fn? (vals rs))))))

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
  (it "binds the five shell symbols into sandbox globals, each with a doc"
      (let [bind (extension/builtin-sandbox-bindings (constantly nil))]
        (expect (= '[shell-background shell-logs shell-run shell-stop shell-type]
                   (vec (sort (filter #(str/includes? (name %) "shell") (keys bind))))))
        (expect (contains? (extension/sandbox-symbol-docs) 'shell-run))))
  (it "documents the commands array on the run tool"
      (let
        [source-doc
         (:doc (meta #'shell/shell-run))

         d
         (str (py (py-ctx {}) "doc('shell_run')"))]

        (expect (str/includes? source-doc "await shell_run([\"git status\"])"))
        (expect (str/includes? d "`commands`"))))
  (it "runs and backgrounds commands through the bare shell global using maps"
      (let
        [sid
         (str "py-shell-" (System/nanoTime))

         c
         (py-ctx {:session-id sid})]

        (try
          (expect (= ["run" "hi" "echo hi"]
                     (py c
                         (str "r = __vis_settle__(shell_run(['echo hi']))\n"
                              "x = r['commands'][0]\n"
                              "[r['stage'], x['stdout'].strip(), x['command']]"))))
          (expect
            (=
              ["background" "logs" "stop"]
              (py
                c
                (str
                  "b = __vis_settle__(shell_background(['echo alive; sleep 30'], {'id':'pyjob'}))\n"
                  "l = __vis_settle__(shell_logs('pyjob', {'n':10}))\n"
                  "s = __vis_settle__(shell_stop('pyjob'))\n"
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
             (expect (false? (boolean (py c "'shell_run' in globals()"))))
             (finally (toggles/set-enabled! "shell" before)))))
  ;; Regression, issue #106: turning Shell commands back ON from a settings
  ;; dialog left every session that was already open without the `shell` global
  ;; (and `subprocess` still raising) until the process restarted — only the
  ;; gateway's HTTP settings handler ever fanned the refresh out to cached
  ;; sessions, so a TUI flip refreshed nothing.
  (it "restores the binding in an already-open CACHED session on the flip alone"
      (let
        [before
         (toggles/enabled? "shell")

         c
         (py-ctx {})

         env
         {:extensions (atom (vec (extension/registered-extensions)))
          :active-extensions (atom [])
          :python-context c}

         cached
         (lp/cache-env! (str "shell-toggle-fanout-" (System/nanoTime)) env)]

        (try (toggles/set-enabled! "shell" false)
             (lp/sync-active-extension-symbols! env)
             (expect (false? (boolean (py c "'shell_run' in globals()"))))
             ;; No explicit sync and no HTTP handler: the toggle flip alone has
             ;; to reach this session's live Python globals.
             (toggles/set-enabled! "shell" true)
             (expect (true? (boolean (py c "'shell_run' in globals()"))))
             (finally (swap! (deref #'lp/cache) dissoc (:id cached))
                      (toggles/set-enabled! "shell" before)))))
  (it "routes the lifecycle tools through the native Python bridge"
      ;; Regression: a stale positional wrapper invoked shell-dispatch as
      ;; (env command options), which made these lifecycle calls fail before the
      ;; dispatcher ran with "Wrong number of args ... shell-dispatch".
      (let
        [sid
         (str "py-shell-lifecycle-" (System/nanoTime))

         c
         (py-ctx {:session-id sid})]

        (try (expect
               (= ["background" "logs" "stop"]
                  (py
                    c
                    (str
                      "job = "
                      (pr-str sid)
                      "\n"
                      "b = __vis_settle__(shell_background(['echo ready; sleep 30'], {'id':job}))\n"
                      "l = __vis_settle__(shell_logs(job, {'n':1}))\n"
                      "s = __vis_settle__(shell_stop(job))\n"
                      "[b['stage'], l['stage'], s['stage']]"))))
             (finally (resources/stop-all! sid))))))


(def ^:private fd-exhaustion? @#'shell/fd-exhaustion?)

(def ^:private spawn-retrying-fds @#'shell/spawn-retrying-fds)

(defdescribe
  spawn-fd-exhaustion-test
  (it "reads the JDK's misleading spawn-helper text as what it really is: EMFILE"
      ;; The JDK blames a version mismatch / a bad JDK install; the true fault is
      ;; that THIS process has no descriptor left to fork with.
      (expect (fd-exhaustion? (java.io.IOException.
                                "Cannot run program \"bash\": error=24, Too many open files")))
      (expect (fd-exhaustion? (RuntimeException. "spawn helper failed"
                                                 (java.io.IOException.
                                                   "error: 24 (Too many open files)"))))
      (expect (not (fd-exhaustion? (java.io.IOException. "error=2, No such file or directory")))))
  (it "retries an EMFILE spawn exactly ONCE after reclaiming, and never retries anything else"
      (let
        [tries
         (atom 0)

         spawned
         (spawn-retrying-fds (fn []
                               (when (= 1 (swap! tries inc))
                                 (throw (java.io.IOException. "error=24, Too many open files")))
                               :spawned))]

        (expect (= :spawned spawned))
        (expect (= 2 @tries)))
      (let [tries (atom 0)]
        (expect (threw? #(spawn-retrying-fds (fn []
                                               (swap! tries inc)
                                               (throw (java.io.IOException.
                                                        "error=2, No such file or directory"))))))
        (expect (= 1 @tries))))
  (it "turns a PERSISTENT exhaustion into a typed diagnosis that names the real cause"
      (let
        [e (try (spawn-retrying-fds (fn []
                                      (throw (java.io.IOException.
                                               "error=24, Too many open files"))))
                nil
                (catch clojure.lang.ExceptionInfo ex ex))]
        (expect (= :com.blockether.vis.internal.foundation.shell/fd-exhausted (:type (ex-data e))))
        (expect (str/includes? (ex-message e) "Out of file descriptors"))
        (expect (str/includes? (ex-message e) "with open(...)"))
        (expect (fd-exhaustion? (ex-cause e))))))

;; The process-group detacher lives in `process-jail` (every spawner shares it:
;; shell/git children AND managed language processes), reached by var here.
(def ^:private path-executable @#'process-jail/path-executable)

(def ^:private execs-in-place? @#'process-jail/execs-in-place?)

(def ^:private detach-argv @#'process-jail/detach-argv)

(defn- pid+pgid
  "Run `argv` and read back the child's own `pid pgid` pair as strings. A process
   whose pgid EQUALS its pid leads its own group; anything else inherited the
   spawning JVM's group."
  [argv]
  (let
    [p
     (.start (ProcessBuilder.
               ^java.util.List
               (into (vec argv) ["/bin/sh" "-c" "echo \"$$ $(ps -o pgid= -p $$ | tr -d ' ')\""])))

     out
     (slurp (.getInputStream p))]

    (.waitFor p)
    (str/split (str/trim out) #"\s+")))

(defdescribe
  spawn-process-group-test
  (it "resolves a helper to an ABSOLUTE executable path, or nil when it does not exist"
      (let [sh (path-executable "sh")]
        (expect (some? sh))
        (expect (str/starts-with? sh "/"))
        (expect (.canExecute (io/file sh))))
      (expect (nil? (path-executable "vis-definitely-not-a-real-binary-xyz"))))
  (it "accepts only a detacher that EXECS — a forking wrapper would swallow the exit status"
      ;; `exec "$@"` keeps the command's own status; the second wrapper runs the
      ;; command and reports its own 0, exactly how a forking `setsid` lies.
      (expect (execs-in-place? ["/bin/sh" "-c" "exec \"$@\"" "sh"]))
      (expect (not (execs-in-place? ["/bin/sh" "-c" "\"$@\" >/dev/null 2>&1; exit 0" "sh"])))
      (expect (not (execs-in-place? ["/vis/no/such/detacher"]))))
  (it "puts every synchronous child in its OWN process group"
      ;; A plain spawn inherits the gateway's group, so a child that signals its
      ;; own group (`kill 0`, `kill -- -$$`, a harness cleaning up) killed the
      ;; DAEMON. Non-vacuous: the undetached spawn below still inherits.
      (let [[bare-pid bare-pgid] (pid+pgid [])]
        (expect (not= bare-pid bare-pgid)))
      (when (seq @detach-argv)
        (let [[pid pgid] (pid+pgid @detach-argv)]
          (expect (= pid pgid)))
        (with-shell-on (fn []
                         (binding [workspace/*workspace-root* (workspace/trunk-root)]
                           (let
                             [r (shell-run* {}
                                            "echo \"$$ $(ps -o pgid= -p $$ | tr -d ' ')\"; exit 5")
                              [pid pgid] (str/split (str/trim (get r "stdout")) #"\s+")]

                             (expect (= pid pgid))
                             ;; the detacher execs, so the command's own status survives
                             (expect (= 5 (get r "exit"))))))))))

(defn- own-pgid
  "This JVM's process group, read the only way the JVM can — by asking `ps`."
  []
  (let
    [^java.util.List argv
     ["/bin/sh" "-c"
      (str "ps -o pgid= -p " (.pid (java.lang.ProcessHandle/current)) " | tr -d ' '")]

     p
     (.start (ProcessBuilder. argv))

     out
     (slurp (.getInputStream p))]

    (.waitFor p)
    (str/trim out)))

(defdescribe child-group-signal-test
             (it "a child that signals its OWN group can no longer take this JVM down with it"
                 ;; THE gateway death: a tool doing `kill 0` / `kill -- -$$` used to deliver
                 ;; SIGTERM to the daemon, which drained and cancelled every other session's
                 ;; live turn (managed language REPLs sit in that same group too). The signal
                 ;; is sent ONLY after both guards prove the child leads a group that is not
                 ;; ours, so a regression fails on the guards instead of killing the runner.
                 (when (seq @detach-argv)
                   (with-shell-on
                     (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [probe (shell-run* {} "echo \"$$ $(ps -o pgid= -p $$ | tr -d ' ')\"")
                            [pid pgid] (str/split (str/trim (get probe "stdout")) #"\s+")
                            mine (own-pgid)]

                           (expect (= pid pgid))
                           (expect (not= pgid mine))
                           (when (and (= pid pgid) (some? pgid) (not= pgid mine))
                             (let [r (shell-run* {} "kill -TERM 0; sleep 5; echo survived")]
                               (expect (not (str/includes? (str (get r "stdout")) "survived")))
                               (expect (not= 0 (get r "exit")))))
                           ;; Still running after the group signal — the whole point.
                           (expect (= "alive\n" (get (shell-run* {} "echo alive") "stdout"))))))))))

;; ── Keychain advisory (#90) ──────────────────────────────────────────────────
;; A confined `gh`/`git`/`security` fails with an opaque Security-framework line
;; and no mention of the jail; the command result says what to turn on.

(def ^:private command-note @#'shell/command-note)

(defdescribe
  shell-keychain-note-test
  (it "names the config key when a live jail denied the Mach lookup"
      (let
        [note (command-note {:jail-policy-fn (constantly {:disabled? false :mach-services []})}
                            {:text ""}
                            {:text (str "security: SecKeychainSearchCreateFromAttributes:"
                                        " The specified item could not be found")})]
        (expect (str/includes? note "jail.mach_services.keychain"))))
  (it "silent when the grant is present, the jail is off, or there is no jail at all"
      (let [err {:text "SecKeychainSearchCreateFromAttributes: nope"}]
        (expect (nil? (command-note {:jail-policy-fn (constantly {:disabled? false
                                                                  :mach-services
                                                                  ["com.apple.SecurityServer"]})}
                                    {:text ""}
                                    err)))
        (expect (nil?
                  (command-note {:jail-policy-fn (constantly {:disabled? true})} {:text ""} err)))
        (expect (nil? (command-note {} {:text ""} err)))))
  (it "ordinary output carries no note"
      (expect (nil? (command-note {:jail-policy-fn (constantly {:disabled? false})}
                                  {:text "hello"}
                                  {:text ""})))))
