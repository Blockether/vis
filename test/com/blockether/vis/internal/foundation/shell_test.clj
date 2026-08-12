(ns com.blockether.vis.internal.foundation.shell-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.env-python :as ep]
            [com.blockether.vis.internal.foundation.shell :as shell]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.process-jail :as process-jail]
            [com.blockether.vis.internal.resources :as resources]
            [com.blockether.vis.internal.shell-log :as shell-log]
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
  ([env id cmd opts] (@#'shell/shell-bg-impl env id cmd opts)))

(def ^:private shell-logs* @#'shell/shell-logs-impl)

(defn- log-text
  "Everything shell `id` has printed, read the way the model is told to read it:
   start at 0 and continue from `next_offset` until a chunk says `is_eof`. A
   cursor that fails to advance ends the loop rather than spinning."
  [env id]
  (loop
    [offset
     0

     acc
     ""]

    (let
      [r
       (:result (shell-logs* env id {:offset offset}))

       acc
       (str acc (get r "stdout"))

       next-offset
       (long (get r "next_offset"))]

      (if (or (get r "is_eof") (<= next-offset (long offset))) acc (recur next-offset acc)))))

(def ^:private shell-send* @#'shell/shell-send-impl)

(def ^:private shell-stop* @#'shell/shell-stop-impl)

(def ^:private shell* @#'shell/shell-dispatch)

(def ^:private render-shell-run-result shell/render-shell-run-result)


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

(defn- alive-pid?
  "Is that OS process still alive? The only honest proof that `stop` killed a
   grandchild the shell had backgrounded."
  [pid]
  (boolean (when pid
             (let [h (java.lang.ProcessHandle/of (long pid))]
               (and (.isPresent h) (.isAlive ^java.lang.ProcessHandle (.get h)))))))

(defn- shell-thread-count
  "How many threads this extension owns right now — pump, PTY reader and attach
   acceptor are all named `vis-shell-*` / `vis-pty-*`, so a leak is countable."
  []
  (->> (Thread/getAllStackTraces)
       keys
       (map #(.getName ^Thread %))
       (filter #(or (str/starts-with? % "vis-shell") (str/starts-with? % "vis-pty")))
       count))

(def ^:private shell-wait* @#'shell/shell-wait-impl)

(defn- wait*
  "`sh.wait(secs)` in Clojure — the SAME host loop the sandbox handle and an
   extension handle call, not a fourth copy of it: the test waits exactly the way
   the product does, so a bug in the loop fails here."
  ([env id] (wait* env id 30))
  ([env id secs] (:result (shell-wait* env id {:seconds secs}))))

(defdescribe
  shell-env-injection-test
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
  (it "jail.enabled false is represented explicitly and still launches unwrapped"
      (binding [workspace/*workspace-root* (workspace/trunk-root)]
        (let
          [r (shell-run* {:session-id "t" :jail-policy-fn (constantly {:disabled? true})}
                         "printf explicit-opt-out")]
          (expect (= 0 (get r "exit")))
          (expect (= "explicit-opt-out" (get r "stdout"))))))
  ;; `jail.env` is gone: the ONE `environment:` declaration carries the VALUE,
  ;; so a name only `.env`, a keychain item or a helper command knows reaches a
  ;; spawned tool even though the shell that launched Vis never exported it.
  ;; The old list could re-admit an ambient name and nothing else.
  (it "a declared `environment:` value reaches the child, jail off or on"
      (binding [workspace/*workspace-root* (workspace/trunk-root)]
        (let
          [declared {"VIS_TEST_DECLARED" "from-declaration"}
           r (shell-run* {:session-id "t"
                          :jail-policy-fn (constantly {:disabled? true :env-values declared})}
                         "printf %s \"$VIS_TEST_DECLARED\"")]

          (expect (= "from-declaration" (get r "stdout")))
          (expect (= declared
                     (process-jail/child-env-additions {:disabled? true :env-values declared}))))))
  ;; The workspace's own `.env` needs no declaration at all: it is loaded whole
  ;; and layered onto the child's environment, which is what every other tool in
  ;; the project already does.
  (it "a workspace `.env` value reaches the child with nothing declared"
      (let
        [env-path
         (str (System/getProperty "java.io.tmpdir") "/vis-shell-dotenv-" (System/nanoTime))]
        (try (spit env-path "VIS_TEST_DOTENV=from-the-project-file\n")
             (binding
               [workspace/*workspace-root* (workspace/trunk-root)
                config/*extension-dotenv-path* env-path
                config/*extension-dotenv-local-path* nil]

               ;; The SAME `:env-values` the spawn path builds (`latest-jail-policy`).
               (let
                 [r (shell-run* {:session-id "t"
                                 :jail-policy-fn (constantly {:disabled? true
                                                              :env-values
                                                              (config/child-environment-values)})}
                                "printf %s \"$VIS_TEST_DOTENV\"")]
                 (expect (= "from-the-project-file" (get r "stdout")))))
             (finally (io/delete-file env-path true))))))

;; Regression: a background `git stash list` came back with a stray `=` above the
;; output, a `>` welded to the next command's first line and long paths broken
;; mid-token. Nothing was corrupt — the pty made isatty() true, git forked `less`
;; itself, and less's keypad-mode escapes rode into the captured log.
(defdescribe
  shell-pty-pager-test
  (it "hands every PTY child a no-op pager, so git never forks less in the first place"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-ext-pager"
               env {:session-id sid}]

              (try
                (shell-bg* env "pager" "printf 'pager=[%s] git=[%s]\\n' \"$PAGER\" \"$GIT_PAGER\"")
                (let [out (poll #(log-text env "pager") #(str/includes? % "pager="))]
                  (expect (str/includes? out "pager=[cat] git=[cat]")))
                (finally (resources/stop! sid "pager"))))))))
  (it "strips the TWO-BYTE keypad escapes a full-screen tool writes, not only CSI"
      (let
        [normalize
         @#'shell/normalize-terminal-output

         ;; Exactly what `less` wrapped around git's output: smkx on entry,
         ;; erase + rmkx on exit, with git's colour reset on the line between.
         cleaned
         (normalize (str "\u001B[?1h\u001B=stash@{0}: WIP\u001B[m\r\n"
                         "\r\u001B[K\u001B[?1l\u001B>--- status ---"))]

        (expect (str/includes? cleaned "stash@{0}: WIP"))
        (expect (str/includes? cleaned "--- status ---"))
        ;; The surviving final byte of `ESC =` / `ESC >` was the whole artifact.
        (expect (nil? (re-find #"[=>]" cleaned)))))
  (it "hands the CALLER the text a human saw, not the escapes that painted it"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [r (shell-run* {} "printf '\\033[33mcoloured\\033[m\\n'; printf 'first\\rlast\\n'")]
              ;; A tool colours and redraws ONLY because our pty makes isatty() true.
              ;; Stripping that on the card alone left the escapes in the `stdout` the
              ;; caller actually prints, which is where the artifact was reported.
              (expect (= "coloured\nlast" (str/trim (get r "stdout"))))
              (expect (not (str/includes? (get r "stdout") "\u001b")))))))))

;; Regression: a `git push` came back as 28 lines of `Counting objects: N%` and 11
;; more of `Compressing objects: N%`, because every bare carriage return was expanded
;; into a newline. The animation filled the capped capture window, so the rendered
;; card cut the answer the caller actually wanted — mid-word.
(defdescribe
  shell-pty-progress-test
  (it "resolves a redrawn progress line to the ONE frame the terminal was showing"
      (let
        [lf
         @#'shell/lf

         ;; Byte-for-byte what git writes down a pty while pushing.
         pushed
         (lf (str "Enumerating objects: 28, done.\r\n"
                  "Counting objects:   3% (1/28)\r" "Counting objects:  82% (23/28)\r"
                  "Counting objects: 100% (28/28), done.\r\n"
                  "Delta compression using up to 14 threads\r\n"))]

        (expect (= (str "Enumerating objects: 28, done.\n"
                        "Counting objects: 100% (28/28), done.\n"
                        "Delta compression using up to 14 threads\n")
                   pushed))))
  (it "keeps the last frame when the capture ends on a bare carriage return"
      (let [lf @#'shell/lf]
        (expect (= "Receiving objects:  40% (12/28)" (lf "Receiving objects:  40% (12/28)\r")))
        ;; A CR that only homes the cursor is a move, not a blank frame.
        (expect (= "--- status ---" (lf "\r--- status ---")))))
  (it "renders one progress line on the card instead of the whole animation"
      (let
        [card (render-shell-run-result {"command" "git push"
                                        "exit" 0
                                        "stdout" (str "Compressing objects:   9% (1/11)\r"
                                                      "Compressing objects: 100% (11/11), done.\r\n"
                                                      "To github.com:example/repo.git\r\n")})]
        (expect (str/includes? (:body card) "Compressing objects: 100% (11/11), done."))
        (expect (not (str/includes? (:body card) "9% (1/11)"))))))

(defdescribe
  shell-run-sync-test
  (it "returns a TOTAL result: every key present, flags real booleans"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            ;; Regression, issue #137: the result carried a `stderr` key that was
            ;; ALWAYS nil — every command runs under a pty, so what a command wrote
            ;; to fd 2 arrived on `stdout` — and a caller reading `stderr` to
            ;; diagnose a failure silently got nothing.
            (let [r (shell-run* {} "echo out; echo err 1>&2; exit 3")]
              (expect (= "out\nerr\n" (get r "stdout")))
              (expect (not (contains? r "stderr")))
              (expect (not (contains? r "stderr_omitted_chars")))
              (expect (= 3 (get r "exit")))
              (expect (number? (get r "duration_ms")))
              ;; ONE result shape: every key of `shell-result-base` is present on every
              ;; result of every stage, so model Python indexes any field directly and
              ;; never branches on which shape came back.
              (expect (true? (get r "started")))
              (expect (false? (get r "timed_out")))
              (expect (false? (get r "timed_out")))
              (expect (= 0 (get r "stdout_omitted_chars")))
              ;; Request scope rides the SAME map — one shape, no envelope to unwrap.
              (expect (contains? r "cwd"))
              (expect (contains? r "timeout_secs"))
              (expect (= "run" (get r "stage")))
              ;; …and no truncation flag beside the counts: 0 already IS "nothing lost".
              (expect (not (contains? r "stdout_truncated")))
              (expect (= 120 (:timeout-secs (meta r))))
              (expect (string? (:dir (meta r)))))))))
  (it "always carries a TOTAL stdout/exit (no output is \"\", not a missing key) and a real cwd"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let [r (shell-run* {} "echo only-out")]
                           (expect (= "only-out\n" (get r "stdout")))
                           ;; TOTAL shape: model Python indexes r["stdout"]/r["exit"]
                           ;; directly — a missing key used to KeyError and spin.
                           (expect (contains? r "stdout"))
                           (expect (= 0 (get r "exit"))))
                         (let [r (shell-run* {} "pwd" {"cwd" "src"})]
                           (expect (string? (:dir (meta r))))
                           (expect (str/ends-with? (:dir (meta r)) "/src")))))))
  (it "treats a non-zero exit as DATA on the command's own entry (not a tool error)"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         ;; shell-run-impl answers the command's own entry, never a tool
                         ;; envelope: a non-zero exit is data ON THAT ENTRY, so a model
                         ;; reading its stdout/exit never branches on shape.
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
            ;; `json.loads(r["stdout"])` died with "Invalid control
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
         {:security-policy {:jail-enabled false} :jail-policy-fn (constantly {:disabled? true})}]

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
  (it
    "registers a session resource, tails logs, and stops cleanly via the registry"
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
                            #(>= (count (str/split-lines (str (get % "stdout")))) 2))]
                   ;; `text` is the bytes as they were printed and `offset`/
                   ;; `next_offset` are the cursor into them — one copy plus a
                   ;; place to continue, never a window that forgot its head.
                   (expect (str/starts-with? (get r "stdout") "l1"))
                   (expect (= 0 (get r "offset")))
                   (expect (pos? (get r "next_offset")))
                   (expect (true? (get r "is_eof")))
                   (expect (nil? (get r "lines")))
                   (expect (= "running" (get r "status"))))
                 (let [stop (resources/stop! sid "worker")]
                   (expect (= :stopped (:result stop)))
                   (expect (empty? (resources/list-resources sid)))
                   ;; The registry entry is gone, but the LOG is the session's, not the
                   ;; process's: a stopped shell still reads back by id, as exited.
                   (let [r (:result (shell-logs* env "worker" {:offset 0}))]
                     (expect (str/starts-with? (get r "stdout") "l1"))
                     (expect (= "exited" (get r "status"))))
                   (expect (threw? #(shell-logs* env "no-such-shell"))))
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
                                  (expect (str/includes? (get r "stdout") "done"))
                                  (expect (nil? (get r "lines"))))
                                (let [res (first (resources/list-resources sid))]
                                  (expect (some? res))
                                  (expect (= "failed" (get res "status"))))
                                (finally (resources/stop-all! sid))))))))
  ;; The whole point of the cursor: a shell that printed far more than one read
  ;; returns loses NOTHING. The ring buffer this replaced dropped the head of a
  ;; long build and only said how many lines it had thrown away.
  (it "gives every byte of a long run back through the offset cursor"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid (str "shell-ext-cursor-" (System/nanoTime))
               env {:session-id sid}]

              (try (shell-bg* env "long" "seq 1 10000")
                   (poll #(:result (shell-logs* env "long")) #(= "exited" (get % "status")) 200)
                   (let [lines (str/split-lines (str/trim-newline (log-text env "long")))]
                     (expect (= 10000 (count lines)))
                     (expect (= "1" (str/trim (first lines))))
                     (expect (= "10000" (str/trim (last lines))))
                     ;; No gap and no overlap: the whole sequence, in order.
                     (expect (= (mapv str (range 1 10001)) (mapv str/trim lines))))
                   ;; A read past the end is empty and still at the end, so a
                   ;; polling loop terminates instead of re-reading the tail.
                   (let [end (:result (shell-logs* env "long" {:offset 1000000000}))]
                     (expect (= "" (get end "stdout")))
                     (expect (true? (get end "is_eof")))
                     (expect (= (get end "offset") (get end "next_offset"))))
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
                   (expect (str/includes? (get again "note") "sh.logs()"))
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
      ;; Regression: `{op: "background", command: "…"}` was rejected for a missing
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

              (try
                (let
                  [started (:result (shell* env {"op" "background" "command" "sleep 60"}))
                   again (:result (shell* env {"op" "background" "command" "sleep 60"}))
                   other (:result (shell* env {"op" "background" "command" "cd / && sleep 61"}))]

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
                     (expect (= "sleep 60" (get r "command")))
                     (expect (contains? r "pid"))
                     ;; ONE shape: a key another stage fills is present-but-neutral here
                     ;; rather than absent, so nothing KeyErrors on a stage boundary.
                     (expect (contains? r "attach"))
                     (expect (contains? r "socket"))
                     (expect (not (contains? r "shown_count")))
                     ;; …but CORE keys stay TOTAL: a cursor at 0 / nil exit, never absent
                     (expect (contains? r "offset"))
                     (expect (= 0 (get r "offset")))
                     (expect (contains? r "next_offset"))
                     (expect (contains? r "is_eof"))
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
                                  (poll #(get (:result (shell-logs* env "c")) "stdout")
                                        #(str/includes? (str %) "/src")
                                        20)
                                  (let [r (:result (shell-logs* env "c"))]
                                    (expect (= (get b "cwd") (get r "cwd")))
                                    (expect (str/includes? (get r "stdout") "/src"))))
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
                                ;; just how many chars it was — and the keystrokes are
                                ;; the LABEL, never a second spelling of `stdout`.
                                (expect (nil? (get snt "stdout")))
                                (expect (= "\"hi-there\" ↵" (get snt "keys"))))
                              (let
                                [hit? (fn [r]
                                        (str/includes? (str (get r "stdout")) "GOT:hi-there"))
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
                                             {"command" "read x; echo GOT:$x; sleep 30"
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
             "Every public shell call is one map; the process line is its `command`."
             (it "runs from command and rejects positional or legacy carriers"
                 (binding [workspace/*workspace-root* (workspace/trunk-root)]
                   (let [r (:result (shell* {} {"command" "echo mapped"}))]
                     (expect (= "run" (get r "stage")))
                     (expect (= "echo mapped" (get r "command")))
                     (expect (= "mapped\n" (get (wait* {} (get r "id")) "stdout"))))
                   (expect (threw? #(shell* {} "echo positional")))
                   (expect (threw? #(shell* {} ["echo positional"])))
                   (expect (threw? #(shell* {} {"cmd" "echo legacy"})))
                   (expect (threw? #(shell* {} {"text" "echo wrong"})))))
             (it "uses text only inside the send map"
                 (expect (threw? #(shell* {:session-id "shell-positional-id"} "ghost")))
                 (expect (threw? #(shell* {:session-id "shell-positional-id"}
                                          {"op" "send" "id" "ghost" "command" "nope"})))))

;; Regression, issue #shell-lifecycle-shape: lifecycle calls carrying a stale `command`
;; field were rejected before `logs`/`wait` could act on the requested background shell.
(defdescribe
  shell-lifecycle-coercion-test
  (it "ignores a command on lifecycle operations"
      (let [sid (str "shell-coercion-" (System/nanoTime))]
        (try (shell* {:session-id sid}
                     {"command" "echo ready; sleep 30" "op" "background" "id" sid})
             (let
               [r (:result (shell* {:session-id sid}
                                   {"op" "logs" "id" sid "command" {"op" "wait" "id" "wrong"}}))]
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
         (render-shell-run-result result)]

        (expect (= stdout (get result "stdout")))
        ;; The CR REDREW the line, so the frame the terminal was left showing is the
        ;; only one a reader is owed.
        (expect (str/includes? (:body run-card) "next!"))
        (expect (not (str/includes? (:body run-card) "PASS")))
        (expect (not (str/includes? (:body run-card) "\u001b")))
        (expect (not (str/includes? (:body run-card) "[0;32m")))))
  (it "answers the bang path by the SYMBOL it resolves at runtime"
      ;; `!cmd` builds its card by `requiring-resolve`-ing this exact public var —
      ;; no registry, no symbol table. Renaming or re-privatizing it would fail
      ;; silently at the call site, so the name is pinned here.
      (let
        [render (requiring-resolve
                  'com.blockether.vis.internal.foundation.shell/render-shell-run-result)]
        (expect (some? render))
        (expect (= "$ echo hi (success)" (:summary (render {"command" "echo hi" "exit" 0}))))))
  (it "names every control character a send typed"
      ;; `keys` is RESULT data, not paint: a send is frequently ENTIRELY
      ;; non-printing (Ctrl-C, Esc, a bare Enter), and "sent 1 chars" taught
      ;; the reader nothing.
      (expect (= "\"hello\" ↵" (keys-label "hello\n")))
      (expect (= "C-c" (keys-label "\u0003")))
      (expect (= "Esc ⇥ \"y\" ↵" (keys-label "\u001b\ty\n")))
      (expect (nil? (keys-label "")))))

(defdescribe
  shell-status-on-every-stage-test
  ;; Regression, status audit: the shell carried a `status` verb of its own, so "has it
  ;; finished", "since when", "where are the bytes on this machine" and "what is it
  ;; costing" read as a SEPARATE question — a second call about a shell whose own answer
  ;; had already said all four.
  (it
    "answers every stage with the lifecycle, the log file on disk and live tree cost"
    (with-shell-on
      (fn []
        (binding [workspace/*workspace-root* (workspace/trunk-root)]
          (let
            [sid "shell-status-stages"
             env {:session-id sid}]

            (try (let
                   [started (:result (shell* env {"command" "sleep 30" "id" "st1"}))
                    logs (:result (shell-logs* env "st1"))]

                   ;; RUNNING, on the RUN's own answer: no exit code yet, and
                   ;; `finished_at` says the same thing on the clock.
                   (expect (= "running" (get started "status")))
                   (expect (nil? (get started "exit")))
                   (expect (nil? (get started "finished_at")))
                   (expect (pos? (long (get started "started_at"))))
                   ;; WHERE the bytes are: an ordinary file a human can read.
                   (expect (= (.getPath (shell-log/log-file sid "st1")) (get started "log_path")))
                   (expect (.isFile (io/file ^String (get started "log_path"))))
                   ;; WHAT it costs, for the whole process TREE, sampled now.
                   (expect (pos? (long (get started "rss_bytes"))))
                   (expect (number? (get started "cpu_percent")))
                   (expect (number? (get started "cpu_ms")))
                   ;; A LOG read says all four again, so nothing has to ask twice.
                   (expect (= "running" (get logs "status")))
                   (expect (= (get started "log_path") (get logs "log_path")))
                   (expect (= (get started "started_at") (get logs "started_at")))
                   (expect (pos? (long (get logs "rss_bytes")))))
                 ;; FINISHED: the wait and a later log read agree on the exit code and
                 ;; the stopped clock, and the sampled cost is nil rather than a stale
                 ;; number about a process that is gone.
                 (let
                   [_started (:result (shell* env {"command" "echo done" "id" "st-done"}))
                    waited (wait* env "st-done" 10)
                    later (poll #(:result (shell-logs* env "st-done")) #(some? (get % "exit")))]

                   (doseq [r [waited later]]
                     (expect (= "exited" (get r "status")))
                     (expect (= 0 (get r "exit")))
                     (expect (>= (long (get r "finished_at")) (long (get r "started_at"))))
                     (expect (nil? (get r "rss_bytes")))
                     (expect (nil? (get r "cpu_percent")))
                     ;; The log FILE outlives the process, and every stage still names it.
                     (expect (.isFile (io/file ^String (get r "log_path"))))))
                 ;; STOP is a stage too: it reports the shell it just killed.
                 (let [stopped (:result (shell* env {"op" "stop" "id" "st1"}))]
                   (expect (= "stopped" (get stopped "status")))
                   (expect (some? (get stopped "log_path"))))
                 (finally (resources/stop-all! sid) (shell-log/delete-session-logs! sid)))))))))

(defdescribe
  shell-wait-answers-test
  ;; Regression, wait audit: a command that had ALREADY finished still cost its
  ;; caller ~130 ms of `wait` (a flat 50 ms poll, paid twice), and while it ran the
  ;; ticker said `_shell-wait tt` — a private op name and a handle the caller had
  ;; invented — so a wait doing exactly what it was asked read as a wait stuck on
  ;; nothing.
  (it "returns within a few ms of the exit it was waiting for"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-wait-latency"
               env {:session-id sid}]

              (try (dotimes [i 3]
                     (let
                       [id (str "fast" i)
                        _ (shell* env {"command" "echo hi" "id" id})
                        w (wait* env id 30)
                        back (System/currentTimeMillis)
                        st (:result (shell-logs* env id))]

                       (expect (= 0 (get w "exit")))
                       (expect (false? (get w "timed_out")))
                       (expect (str/includes? (str (get w "stdout")) "hi"))
                       ;; The bytes are all there AND the caller is not held past the
                       ;; exit: everything after `finished_at` is pure wait overhead.
                       (expect (< (- back (long (get st "finished_at"))) 100))))
                   (finally (resources/stop-all! sid) (shell-log/delete-session-logs! sid))))))))
  (it "backs its idle cadence off along the idle count instead of a flat 50 ms"
      (let [poll @#'shell/wait-idle-poll-ms]
        ;; Near-instant while a finish is plausible, cheap once it clearly is not.
        (expect (= 2 (poll 0)))
        (expect (= 2 (poll 3)))
        (expect (= 10 (poll 4)))
        (expect (= 50 (poll 10)))
        (expect (= 50 (poll 10000)))
        ;; Confirming an exited child's log is quiet is charged to a command that is
        ;; already done, so it is the shortest sleep in the loop.
        (expect (< @#'shell/wait-drain-poll-ms (poll 10000)))))
  (it "tells the ticker WHAT it waits for — the command and the budget, not the op"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-wait-ticker"
               env {:session-id sid}
               ticker-of #(:ext.symbol/ticker-fn %)]

              (try
                (shell* env {"command" "sleep 30" "id" "dev"})
                ;; The live phrase names the COMMAND and the budget — never the
                ;; handle, which is the caller's own bookkeeping and answers as
                ;; little as `_shell-wait dev` did.
                (expect (= "waiting up to 60s for: sleep 30"
                           ((ticker-of shell/shell-wait-symbol) env [{"id" "dev" "seconds" 60}])))
                (expect (= "waiting up to 120s for: sleep 30"
                           ((ticker-of shell/shell-wait-symbol) env [{"id" "dev"}])))
                (expect (= "stopping: sleep 30"
                           ((ticker-of shell/shell-stop-symbol) env [{"id" "dev"}])))
                ;; The SPAWN says what it is about to run, read from its own
                ;; argument — the one call that cannot look the command up yet.
                (expect (= "running: npm test" ((ticker-of shell/shell-symbol) env ["npm test"])))
                ;; Regression, user report (paraphrased: what is `tt`?): no live
                ;; shell answers, and the sentence says the generic noun rather
                ;; than a token only the caller can resolve.
                (expect (= "reading the log of the shell"
                           ((ticker-of shell/shell-logs-symbol) env [{"id" "zq7"}])))
                (expect (not (str/includes? ((ticker-of shell/shell-wait-symbol) env [{"id" "zq7"}])
                                            "zq7")))
                (finally (resources/stop-all! sid) (shell-log/delete-session-logs! sid)))))))))

(defdescribe shell-one-shape-test
             ;; Regression, one-shape refactor: `run` answered `stdout`, `logs` answered
             ;; `text`, `send`/`stop` carried their own stage-scoped subsets and an argv run built a
             ;; map of its own with an extra `args` — so a caller had to learn which shape a
             ;; call came back in, and reading the wrong name was a KeyError.
             (it
               "answers the SAME key set from EVERY stage and from an argv run"
               (with-shell-on
                 (fn []
                   (binding [workspace/*workspace-root* (workspace/trunk-root)]
                     (let
                       [sid "shell-one-shape"
                        env {:session-id sid}
                        ks #(set (keys %))]

                       (try (let
                              [run (:result (shell* env {"command" "echo hi"}))
                               bg (:result (shell* env {"command" "read x; echo got:$x" "id" "s1"}))
                               logs2 (:result (shell-logs* env "s1"))
                               logs (:result (shell-logs* env "s1"))
                               sent (:result (shell-send* env "s1" "yes"))
                               waited (wait* env (get run "id"))
                               stopped (:result (shell* env {"op" "stop" "id" "s1"}))
                               git (shell/run-argv env ["git" "--version"] nil)]

                              (expect (= (ks run)
                                         (ks bg)
                                         (ks logs2)
                                         (ks logs)
                                         (ks waited)
                                         (ks sent)
                                         (ks stopped)
                                         (ks git)))
                              ;; `stage` is the ONE thing that varies, and it varies as DATA.
                              (expect (= ["run" "run" "logs" "logs" "wait" "send" "stop" "run"]
                                         (mapv #(get % "stage")
                                               [run bg logs2 logs waited sent stopped git])))
                              ;; The bytes have ONE name, whether the call waited for them or
                              ;; came back for them later.
                              (expect (= "hi\n" (get waited "stdout")))
                              (expect (contains? logs "stdout")))
                            (finally (resources/stop-all! sid)))))))))

(defdescribe
  shell-one-wait-test
  ;; Regression, wait audit: the bounded poll loop was written three times — the
  ;; sandbox handle, an extension's handle and the tests each had their own copy,
  ;; free to disagree about the deadline, the cursor and what "done" means. It is
  ;; ONE host op now (`_shell_wait`), and these are its edges.
  (it "is the ONE loop: every caller routes through the host wait op"
      (expect (= "_shell_wait" (:ext.symbol/name shell/shell-wait-symbol)))
      ;; No Python caller may keep a loop of its own — the sandbox handle and
      ;; the extension handle both call the host op.
      (doseq
        [[f marker] {"vis-python/async_runtime.py" "_shell_wait"
                     ;; An extension authors the op map by hand; same host loop.
                     "vis-python/extension_bootstrap.py" "'op': 'wait'"}]
        (let [src (slurp (io/resource f))]
          (expect (str/includes? src marker) f)
          (expect (not (str/includes? src "time.sleep(poll)")) f)))
      ;; The POSIX shim is not a caller at all: `subprocess` never spawns, so it
      ;; owns no wait, no cursor, no second copy of the shell contract and no
      ;; wording of its own - it raises the host's `PROCESS_SURFACE` sentences.
      (let [src (slurp (io/resource "vis-shims/posix.py"))]
        (expect (not (str/includes? src "_shell_wait")))
        (expect (str/includes? src "__vis_process_surface__"))
        (doseq [copy ["never spawn in the vis sandbox" "DISABLED"]]
          (expect (not (str/includes? src copy)) copy))))
  (it "bounds MEMORY as well as time when a command never stops printing"
      ;; A runaway printer produced ~1 MB/s: an unbounded accumulator turned a long
      ;; wait into a heap problem, so the wait keeps head+tail exactly as a
      ;; foreground capture does and counts what it dropped.
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-wait-runaway"
               env {:session-id sid}]

              (try (let
                     [sh (:result (shell* env
                                          {"command" "while true; do echo xxxxxxxxxxxxxxxxx; done"
                                           "id" "loud"}))
                      r (wait* env (get sh "id") 1)]

                     (expect (true? (get r "timed_out")))
                     (expect (= "running" (get r "status")))
                     (expect (< (count (get r "stdout")) 500000))
                     (expect (pos? (long (get r "stdout_omitted_chars")))))
                   (finally (resources/stop-all! sid))))))))
  (it "answers from the LOG once the process is gone, and moves no cursor twice"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-wait-retired"
               env {:session-id sid}]

              (try (let
                     [sh (:result (shell* env {"command" "printf 'a\\nb\\n'"}))
                      id (get sh "id")
                      first-wait (wait* env id 20)
                      again (wait* env id 5)
                      onward
                      (:result
                        (shell-wait* env id {:seconds 5 :offset (get first-wait "next_offset")}))]

                     (expect (= "a\nb\n" (get first-wait "stdout")))
                     (expect (= 0 (get first-wait "exit")))
                     (expect (false? (get first-wait "timed_out")))
                     ;; A second wait from the start replays the same log — the file is
                     ;; the truth and a finished shell keeps answering by id.
                     (expect (= "a\nb\n" (get again "stdout")))
                     ;; From the cursor the first wait returned, there is nothing new.
                     (expect (= "" (get onward "stdout"))))
                   (finally (resources/stop-all! sid))))))))
  (it "refuses an unknown id, another origin's shell and a nonsense duration"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-wait-refusals"
               mine {:session-id sid :shell-origin "tool"}
               theirs {:session-id sid :shell-origin "jailed-extension"}]

              (try (let
                     [sh (:result (shell* mine {"command" "sleep 30" "id" "mine"}))
                      id (get sh "id")]

                     (expect (str/includes? (throw-message #(wait* mine "no-such-shell" 1))
                                            "No shell"))
                     (expect (str/includes? (throw-message #(wait* theirs id 1))
                                            "different trust origin"))
                     ;; The refusal names the key the CALLER spelled, not an internal one.
                     (expect (str/includes? (throw-message #(wait* mine id -5)) "seconds"))
                     (expect (str/includes? (throw-message #(:result
                                                              (shell-wait* mine id {:seconds 0.5})))
                                            "whole number")))
                   (finally (resources/stop-all! sid))))))))
  (it "ends the wait when someone else stops the shell, and keeps its bytes"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [sid "shell-wait-stopped"
                            env {:session-id sid}]

                           (try (let
                                  [sh (:result (shell* env
                                                       {"command" "echo before-stop; sleep 30"
                                                        "id" "victim"}))
                                   _ (Thread/sleep 300)
                                   _ (shell* env {"op" "stop" "id" "victim"})
                                   r (wait* env (get sh "id") 10)]

                                  (expect (false? (get r "timed_out")))
                                  (expect (str/includes? (get r "stdout") "before-stop")))
                                (finally (resources/stop-all! sid))))))))
  (it "unwinds at once when the turn is cancelled, leaving the shell alive"
      ;; A wait is HOST code now, so a cancelled turn must interrupt it promptly
      ;; instead of holding the thread until the deadline; the child keeps running
      ;; under its id, which is what makes the log a feature rather than a loss.
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-wait-interrupt"
               env {:session-id sid}]

              (try (let
                     [_ (shell* env {"command" "sleep 60" "id" "slow"})
                      out (promise)
                      t (Thread. (fn []
                                   (try (shell-wait* env "slow" {:seconds 300})
                                        (deliver out :finished)
                                        (catch InterruptedException _ (deliver out :interrupted))
                                        (catch Throwable e (deliver out (class e))))))]

                     (.start t)
                     (Thread/sleep 300)
                     (.interrupt t)
                     (expect (= :interrupted (deref out 5000 :still-blocked)))
                     (expect (= "running" (get (:result (shell-logs* env "slow")) "status"))))
                   (finally (resources/stop-all! sid))))))))
  (it "leaves no shell or pty thread behind after a burst of waits"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [sid "shell-wait-threads"
                            env {:session-id sid}
                            base (shell-thread-count)]

                           (try (doseq [i (range 6)]
                                  (let
                                    [sh (:result (shell* env
                                                         {"command" (str "echo burst-" i)
                                                          "id" (str "burst" i)}))]
                                    (expect (= (str "burst-" i "\n")
                                               (get (wait* env (get sh "id") 20) "stdout")))))
                                (finally (resources/stop-all! sid)))
                           (Thread/sleep 500)
                           (expect (<= (shell-thread-count) base))))))))

(defdescribe
  shell-handle-integrity-test
  ;; Regression, handle audit: a handle outlives the spawn, and everything that
  ;; drove one afterwards trusted the id alone.
  (it "keeps two ids that SANITIZE alike in two different log files"
      ;; `a/b` and `a_b` both became `a_b.log`, so the second `open!` truncated the
      ;; first shell's output and both handles reported the same bytes.
      (expect (not= (.getPath (shell-log/log-file "s" "a/b"))
                    (.getPath (shell-log/log-file "s" "a_b")))))
  (it "refuses a live id handed a DIFFERENT command or cwd instead of silently not running it"
      ;; Re-issuing `id` returned success for a process that never ran the requested
      ;; command, in a directory nobody asked for; `already_running` was nil besides.
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-reissue-identity"
               env {:session-id sid}]

              (try (let
                     [_ (shell* env {"command" "sleep 30" "id" "keep"})
                      same (:result (shell* env {"command" "sleep 30" "id" "keep"}))]

                     (expect (true? (get same "already_running")))
                     (expect (str/includes? (throw-message
                                              #(shell* env {"command" "sleep 31" "id" "keep"}))
                                            "different command"))
                     (expect (str/includes?
                               (throw-message
                                 #(shell* env {"command" "sleep 30" "id" "keep" "cwd" "/tmp"}))
                               "different directory")))
                   (finally (resources/stop-all! sid))))))))
  (it "refuses to read, type at, stop or re-issue a shell of another trust origin"
      ;; The jail is consulted at spawn only: a jailed extension could read a trusted
      ;; shell's output, type at its PTY or kill it just by naming the id.
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-origin"
               mine {:session-id sid :shell-origin "tool"}
               theirs {:session-id sid :shell-origin "jailed-extension"}]

              (try (let [_ (shell* mine {"command" "sleep 30" "id" "owned"})]
                     (doseq
                       [call [#(shell* theirs {"op" "logs" "id" "owned"})
                              #(shell* theirs {"op" "send" "id" "owned" "text" "x"})
                              #(shell* theirs {"op" "stop" "id" "owned"})
                              #(shell* theirs {"command" "sleep 30" "id" "owned"})]]
                       (expect (str/includes? (str (throw-message call)) "different trust origin")))
                     ;; The owner is unaffected.
                     (expect (= "owned"
                                (get (:result (shell* mine {"op" "logs" "id" "owned"})) "id"))))
                   (finally (resources/stop-all! sid))))))))
  (it "refuses a negative or fractional number instead of rounding it into another read"
      ;; Regression, handle audit: `->pos-long` rounded and clamped, so a number the
      ;; caller cannot have meant silently became a different one.
      (binding [workspace/*workspace-root* (workspace/trunk-root)]
        (expect (str/includes? (str (throw-message #(shell* {:session-id "t"}
                                                            {"op" "logs" "id" "nope" "offset" -5})))
                               "must not be negative"))
        (expect (str/includes? (str (throw-message #(shell* {:session-id "t"}
                                                            {"op" "logs" "id" "nope" "limit" 0.4})))
                               "whole number")))))


(defdescribe
  shell-one-command-test
  (it "runs the ONE command and answers with a flat result"
      ;; Regression, one-command refactor: `commands` was an ordered batch with
      ;; its own shared budget and `r["commands"][i]` indirection; a call now runs
      ;; one line and `&&` says the rest.
      (binding [workspace/*workspace-root* (workspace/trunk-root)]
        (let
          [r (wait* {}
                    (get (:result (shell* {} {"command" "printf first && printf second"})) "id"))]
          (expect (= "firstsecond" (str/trim (get r "stdout"))))
          (expect (not (contains? r "commands"))))))
  (it "rejects missing, blank, and non-map process requests"
      (expect (threw? #(shell* {} {})))
      (expect (threw? #(shell* {} {"command" ""})))
      (expect (threw? #(shell* {} ["printf first"]))))
  (it "coerces an argv array into one quoted bash line"
      ;; The habitual argv spelling: tokens instead of a line.
      ;; Each token stays ONE argument, so spaces survive quoting.
      (binding [workspace/*workspace-root* (workspace/trunk-root)]
        (let [r (:result (shell* {} {"command" ["printf" "%s" "two words"]}))]
          (expect (= "printf %s 'two words'" (get r "command")))
          (expect (= "two words" (str/trim (get (wait* {} (get r "id")) "stdout"))))))))

(defdescribe shell-mistaken-shape-test
             ;; Both of these were got wrong by a caller mid-task, not imagined: the argv
             ;; habit put this call's OWN options inside `command`. A wrong shape has to say
             ;; where the argument belongs — restating its type is what makes the tool look broken.
             (it "names the top-level lifecycle arguments when an options map lands in `command`"
                 (let
                   [msg (throw-message #(shell* {} {"command" {"op" "logs" "id" "build" "n" 30}}))]
                   (expect (some? msg))
                   (expect (str/includes? msg "TOP-LEVEL"))
                   (expect (str/includes? msg "\"op\": \"logs\""))))
             (it "coerces a java.util.List, the Python spelling of that argv array"
                 (binding [workspace/*workspace-root* (workspace/trunk-root)]
                   (let
                     [argv (java.util.ArrayList. ["printf" "%s" "two words"])
                      r (:result (shell* {} {"command" argv}))]

                     (expect (= "printf %s 'two words'" (get r "command")))
                     (expect (= "two words" (str/trim (get (wait* {} (get r "id")) "stdout"))))))))

(defdescribe
  shell-native-contract-test
  ;; Regression, un-unified shell: ONE `shell` tool with an `op` enum made the model
  ;; disambiguate five mutually-exclusive shapes on every call; five tools then cost
  ;; five schemas for operations on ONE object. The end of that line is NO native tool
  ;; at all — a process starts from Python, the one place that can HOLD the handle the
  ;; call answers with.
  (it "advertises no native shell tool, because the wire cannot hold a handle"
      (expect (= [] (mapv :ext.symbol/name (filter :ext.symbol/native-tool? shell/shell-symbols))))
      (expect (= 5 (count shell/shell-symbols)))
      ;; Still the public, documented Python verb — not a private underscore transport.
      (expect (= "shell" (:ext.symbol/name (first shell/shell-symbols))))
      (expect (contains? (extension/sandbox-symbol-docs) 'shell)))
  (it "points the shell verbs at the HANDLE that replaced a blocking wait"
      (expect (str/includes? (:ext.symbol/description shell/shell-symbol) "HANDLE"))
      (expect (str/includes? (:ext.symbol/description shell/shell-symbol) "sh.wait(secs)"))
      (expect (str/includes? (:ext.symbol/description shell/shell-logs-symbol) "sh.logs(")))
  ;; Regression, handle audit: the description taught the SHAPE of a run but not what to put
  ;; IN it, so commands arrived pre-trimmed - `| tail -50`, `| grep foo`, `2>/dev/null` - which
  ;; threw away bytes the handle already keeps whole and hid a nonzero exit behind the
  ;; pipeline's last stage.
  (it "teaches that trimming belongs on the HANDLE, never inside the command"
      (let [text (:ext.symbol/description shell/shell-symbol)]
        (doseq
          [needle ["| head" "| tail" "| grep" "2>/dev/null" "> file" "sh.logs(offset=…)"
                   "log_path"]]
          (expect (str/includes? text needle) needle))
        ;; The reason, not only the prohibition: a pipeline reports the LAST stage's exit.
        (expect (str/includes? text "LAST stage")))))

(defdescribe shell-extension-shape-test
             (it "is a registered builtin extension exposing the ONE bare `shell` symbol"
                 (expect (= "foundation-shell" (:ext/name shell/vis-extension)))
                 ;; No engine alias any more: `shell` is bound BARE in the flat sandbox
                 ;; next to cat / grep, so there is no `shell.run(…)` namespace.
                 (expect (true? (get-in shell/vis-extension [:ext/engine :ext.engine/builtin?])))
                 (expect (nil? (get-in shell/vis-extension [:ext/engine :ext.engine/alias])))
                 ;; No `status` transport either: every stage's answer already carries it.
                 (expect (= '[shell _shell-logs _shell-wait _shell-type _shell-stop]
                            (mapv :ext.symbol/symbol shell/shell-symbols)))))

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
                           (str/includes? (str (get r "stdout")) "READY"))
                  before (poll #(:result (shell-logs* env "pty")) ready?)]

                 (expect (str/includes? (get before "stdout") "TTY_OK"))
                 (expect (str/includes? (get before "stdout") "NESTED_SEALED"))
                 (expect (not (str/includes? (get before "stdout") "ESCAPED")))
                 (expect (string? (get started "attach")))
                 (expect (string? (get started "socket")))
                 (shell-send* env "pty" "hello")
                 (let
                   [after (poll #(:result (shell-logs* env "pty"))
                                #(str/includes? (str (get % "stdout")) "GOT:hello"))]
                   (expect (str/includes? (get after "stdout") "GOT:hello")))))
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
        (expect (= '[_shell-logs _shell-stop _shell-type _shell-wait shell]
                   (vec (sort (filter #(str/includes? (name %) "shell") (keys bind))))))
        (expect (contains? (extension/sandbox-symbol-docs) 'shell))))
  (it "documents the ONE command on the run tool"
      (let
        [source-doc
         (:doc (meta #'shell/shell))

         d
         (str (py (py-ctx {}) "doc('shell')"))]

        (expect (str/includes? source-doc "await shell(\"npm test\")"))
        (expect (str/includes? d "`command`"))))
  (it "runs and backgrounds a command through the bare shell global using maps"
      (let
        [sid
         (str "py-shell-" (System/nanoTime))

         c
         (py-ctx {:session-id sid})]

        (try (expect (= ["wait" "hi" "echo hi"]
                        (py c
                            (str "r = __vis_settle__(shell('echo hi')).wait(20)\n"
                                 "[r['stage'], r['stdout'].strip(), r['command']]"))))
             (expect (= ["run" "logs" "stop"]
                        (py c
                            (str
                              "b = __vis_settle__(shell('echo alive; sleep 30', {'id':'pyjob'}))\n"
                              "l = b.logs(limit=10)\n"
                              "s = b.stop()\n" "[b['stage'], l['stage'], s['stage']]"))))
             (finally (resources/stop-all! sid)))))
  (it "bounds sh.wait(secs) even when the command NEVER stops printing"
      ;; Regression, shell audit: the poll loop only checked its deadline once the log
      ;; was at EOF, so a command with bytes always available (`yes`, a chatty build)
      ;; never reached the clock and `sh.wait(1)` ran until the sandbox watchdog.
      (let
        [sid
         (str "py-wait-bound-" (System/nanoTime))

         c
         (py-ctx {:session-id sid})]

        (try (expect (= [true "running" true]
                        (py c
                            (str "import time\n"
                                 "sh = __vis_settle__(shell('while true; do echo x; done',"
                                 " {'id':'chatty'}))\n" "t0 = time.time()\n"
                                 "r = sh.wait(1)\n" "took = time.time() - t0\n"
                                 "sh.stop()\n" "[bool(r['timed_out']), r['status'], took < 45]"))))
             (finally (resources/stop-all! sid)))))
  (it "answers with a HANDLE whose own methods drive the process"
      ;; Phase 8: the three id-taking verbs are gone from the model's surface. What
      ;; came back IS the object you drive — and it is still an ordinary dict.
      (let
        [sid
         (str "py-handle-" (System/nanoTime))

         c
         (py-ctx {:session-id sid})]

        (try (expect (= [true "__VisShell__" false true "exited" false]
                        (py c
                            (str
                              "sh = __vis_settle__(shell('read x; echo got $x', {'id':'handle'}))\n"
                              "sh.type('ready')\n"
                              "w = sh.wait(30)\n" "sh.stop()\n"
                              "[isinstance(sh, dict), type(sh).__name__, 'shell_logs' in globals(),"
                              " 'got ready' in w['stdout'], w['status'], w['timed_out']]"))))
             (finally (resources/stop-all! sid)))))
  (it "reports the shell's status on the run's OWN answer, with no second call"
      ;; The handle carried a `status()` verb of its own, so "has it finished" read as a
      ;; separate question about a shell whose own answer had already said it.
      (let
        [sid
         (str "py-status-" (System/nanoTime))

         c
         (py-ctx {:session-id sid})]

        (try (expect (= [false "running" true true true true]
                        (py c
                            (str "sh = __vis_settle__(shell('sleep 30', {'id':'stat'}))\n"
                                 "sh.stop()\n"
                                 "[hasattr(sh, 'status'), sh['status'], sh['exit'] is None,"
                                 " sh['log_path'].endswith('stat.log'),"
                                 " sh['rss_bytes'] > 0, sh['started_at'] > 0]"))))
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
             (expect (false? (boolean (py c "'shell' in globals()"))))
             ;; No explicit sync and no HTTP handler: the toggle flip alone has
             ;; to reach this session's live Python globals.
             (toggles/set-enabled! "shell" true)
             (expect (true? (boolean (py c "'shell' in globals()"))))
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

        (try (expect (= ["run" "logs" "stop"]
                        (py c
                            (str "job = "
                                 (pr-str sid)
                                 "\n"
                                 "b = __vis_settle__(shell(['echo ready; sleep 30'], {'id':job}))\n"
                                 "l = b.logs(limit=1)\n"
                                 "s = b.stop()\n" "[b['stage'], l['stage'], s['stage']]"))))
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
;; shell children AND managed language processes), reached by var here.
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
                            {:text (str "security: SecKeychainSearchCreateFromAttributes:"
                                        " The specified item could not be found")})]
        (expect (str/includes? note "jail.mach_services.keychain"))))
  (it "silent when the grant is present, the jail is off, or there is no jail at all"
      (let [out {:text "SecKeychainSearchCreateFromAttributes: nope"}]
        (expect (nil? (command-note {:jail-policy-fn (constantly {:disabled? false
                                                                  :mach-services
                                                                  ["com.apple.SecurityServer"]})}
                                    out)))
        (expect (nil? (command-note {:jail-policy-fn (constantly {:disabled? true})} out)))
        (expect (nil? (command-note {} out)))))
  (it "ordinary output carries no note"
      (expect (nil? (command-note {:jail-policy-fn (constantly {:disabled? false})}
                                  {:text "hello"})))))

;; Phase 5: a run IS a handle — the wait expires, the process does not.
(defdescribe
  run-is-a-handle-test
  (it "answers with the handle immediately and keeps printing into its log"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-run-handle"
               env {:session-id sid}
               r (:result (shell* env {"command" "printf 'early\\n'; sleep 1; printf 'late\\n'"}))
               id (get r "id")]

              (try
                ;; Nothing was waited for, so there is no exit yet — and no timeout either.
                (expect (false? (get r "timed_out")))
                (expect (nil? (get r "exit")))
                (expect (string? id))
                (expect (seq id))
                ;; Everything the call could not have seen arrives on the handle.
                (let [w (wait* env id)]
                  (expect (str/includes? (get w "stdout") "early"))
                  (expect (str/includes? (get w "stdout") "late"))
                  (expect (= 0 (get w "exit"))))
                (finally (resources/stop-all! sid) (shell-log/delete-session-logs! sid))))))))
  (it "keeps a finished run's log readable by id after the process is gone"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-run-handle-fast"
               env {:session-id sid}
               r (:result (shell* env {"command" "printf 'done\\n'"}))
               id (get r "id")]

              (try (expect (seq id))
                   (expect (= "done\n" (get (wait* env id) "stdout")))
                   ;; No live process is left to account for, yet the bytes are still
                   ;; readable by id: the log belongs to the session, not to the child.
                   ;; Retention IS the feature — nothing reaps it behind the caller.
                   (let [logs (:result (shell-logs* env id {:offset 0}))]
                     (expect (str/includes? (get logs "stdout") "done"))
                     (expect (= "exited" (get logs "status"))))
                   (finally (resources/stop-all! sid) (shell-log/delete-session-logs! sid)))))))))


(defdescribe
  no-wait-knob-test
  ;; Regression, Phase 11: `wait` on the REQUEST was a number that selected a MODE
  ;; (0 meant background, 0.4 rounded into it, -5 clamped into a fake timeout).
  ;; Every run is a background run now and waiting is `sh.wait(secs)` on the handle.
  (it "returns at once with the shell still running, and its log reads whole"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-no-wait"
               env {:session-id sid}
               r (:result (shell* env
                                  {"command" "printf 'up\\n'; sleep 5; printf 'done\\n'"
                                   "id" "waiter"}))]

              (try
                ;; Nothing waited, so nothing timed out — and the answer is the
                ;; ordinary run shape, not a second result kind.
                (expect (= "waiter" (get r "id")))
                (expect (false? (get r "timed_out")))
                (expect (nil? (get r "exit")))
                (expect (= "run" (get r "stage")))
                (let
                  [text (loop [n 0]
                          (let [text (log-text env "waiter")]
                            (if (or (str/includes? text "up") (<= 40 n))
                              text
                              (do (Thread/sleep 100) (recur (inc n))))))]
                  (expect (str/includes? text "up")))
                (finally (resources/stop-all! sid) (shell-log/delete-session-logs! sid))))))))
  (it "the handle's wait is what fills exit and stdout"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-handle-wait"
               env {:session-id sid}
               r (:result (shell* env {"command" "printf 'done\\n'; exit 7" "id" "waited"}))
               w (wait* env "waited")]

              (try (expect (nil? (get r "exit")))
                   (expect (= 7 (get w "exit")))
                   (expect (str/includes? (get w "stdout") "done"))
                   (finally (resources/stop-all! sid) (shell-log/delete-session-logs! sid))))))))
  (it "re-issuing a live id answers with THAT shell instead of starting a second one"
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [sid "shell-reissue"
                            env {:session-id sid}
                            first-r (:result (shell* env {"command" "sleep 5" "id" "dev"}))
                            again (:result (shell* env {"command" "sleep 5" "id" "dev"}))]

                           (try (expect (= "dev" (get first-r "id")))
                                (expect (= "dev" (get again "id")))
                                (expect (str/includes? (get again "note") "ALREADY running"))
                                ;; One id, one process: a re-issue must never leave a second child
                                ;; printing into the same log.
                                (expect (= 1 (count (get @@#'shell/bg-procs sid))))
                                (finally (resources/stop-all! sid)
                                         (shell-log/delete-session-logs! sid))))))))
  (it "a spawned shell keeps a writable stdin, so an interactive command can be answered"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-stdin"
               env {:session-id sid}
               _ (shell* env
                         {"command" "read answer; printf 'got %s\\n' \"$answer\"" "id" "asker"})]

              (try (Thread/sleep 300)
                   (shell* env {"op" "send" "id" "asker" "text" "yes"})
                   (let
                     [text (loop [n 0]
                             (let [text (log-text env "asker")]
                               (if (or (str/includes? text "got yes") (<= 40 n))
                                 text
                                 (do (Thread/sleep 100) (recur (inc n))))))]
                     (expect (str/includes? text "got yes")))
                   (finally (resources/stop-all! sid) (shell-log/delete-session-logs! sid))))))))
  ;; The whole point of the phase: one process jail, and no schema at all. Everything the
  ;; family used to advertise operates on a handle the caller already holds, in Python.
  (it "advertises no native tool for the whole shell family"
      (expect (= []
                 (->> shell/shell-symbols
                      (filter :ext.symbol/native-tool?)
                      (mapv :ext.symbol/name))))
      (expect (= ["shell" "_shell_logs" "_shell_wait" "_shell_type" "_shell_stop"]
                 (mapv :ext.symbol/name shell/shell-symbols)))))


(defdescribe
  shell-failure-visibility-test
  ;; Regression, shell audit: a command that fails is the COMMON case, and every
  ;; piece of evidence about it — the exit code, the message the shell printed, the
  ;; fact that something is still running — has to survive the call that started it.
  (it "answers 127 and keeps the shell's own complaint when the command does not exist"
      ;; A misspelled program printed to stderr and died: the code and the sentence
      ;; explaining it must both be in the log, or a failure reads as an empty result.
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-missing-command"
               env {:session-id sid}]

              (try (let
                     [_ (shell* env {"command" "definitely-not-a-real-command-xyz" "id" "nope"})
                      r (wait* env "nope")]

                     (expect (= 127 (get r "exit")))
                     (expect (= "exited" (get r "status")))
                     (expect (str/includes? (get r "stdout") "command not found"))
                     (expect (false? (get r "timed_out"))))
                   (finally (resources/stop-all! sid))))))))
  (it "carries a non-zero exit and the STDERR that explains it"
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-exit-code"
               env {:session-id sid}]

              (try (let
                     [_ (shell* env {"command" "echo out; echo boom 1>&2; exit 3" "id" "e3"})
                      r (wait* env "e3")]

                     (expect (= 3 (get r "exit")))
                     (expect (str/includes? (get r "stdout") "out"))
                     (expect (str/includes? (get r "stdout") "boom")))
                   (finally (resources/stop-all! sid))))))))
  (it "says it is RUNNING, with the pid, the moment it spawns"
      ;; The run stage rebuilt its map from scratch and dropped `pid`, `status` and
      ;; `uptime_ms`, so a spawn could not say what it had started; `is_eof` claimed
      ;; the log of a live shell was already complete.
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-spawn-says-running"
               env {:session-id sid}]

              (try (let
                     [r (:result (shell* env {"command" "echo up; sleep 30" "id" "live"}))
                      again (:result (shell* env {"command" "echo up; sleep 30" "id" "live"}))]

                     (expect (= "running" (get r "status")))
                     (expect (pos? (long (get r "pid"))))
                     (expect (false? (get r "is_eof")))
                     (expect (false? (get r "timed_out")))
                     (expect (false? (get r "already_running")))
                     ;; Re-attaching made no wait, so nothing expired.
                     (expect (true? (get again "already_running")))
                     (expect (false? (get again "timed_out")))
                     (expect (= (get r "pid") (get again "pid")))
                     (expect (= "running" (get again "status"))))
                   (finally (resources/stop-all! sid))))))))
  (it "kills the whole process TREE on stop and still answers with the logs"
      ;; A backgrounded grandchild used to outlive its shell, and the bytes it had
      ;; already printed had to survive the kill or a stopped job is unexplainable.
      (with-shell-on
        (fn []
          (binding [workspace/*workspace-root* (workspace/trunk-root)]
            (let
              [sid "shell-stop-kills-tree"
               env {:session-id sid}]

              (try (let
                     [_ (shell* env {"command" "sleep 300 & echo CHILD=$!; sleep 300" "id" "tree"})
                      child (poll #(some->> (get (:result (shell-logs* env "tree" {:offset 0}))
                                                 "stdout")
                                            (re-find #"CHILD=(\d+)")
                                            second
                                            parse-long)
                                  some?)
                      parent (get (:result (shell-logs* env "tree" {:offset 0})) "pid")
                      stopped (:result (shell-stop* env "tree"))]

                     (expect (true? (get stopped "stopped")))
                     (expect (= "stopped" (get stopped "status")))
                     (expect (some? (get stopped "exit")))
                     (expect (nil? (poll #(when (alive-pid? child) :alive) nil? 30)))
                     (expect (nil? (poll #(when (alive-pid? parent) :alive) nil? 30)))
                     ;; The log outlives the kill: the id still reads back.
                     (expect (str/includes? (get (:result (shell-logs* env "tree" {:offset 0}))
                                                 "stdout")
                                            "CHILD=")))
                   (finally (resources/stop-all! sid))))))))
  (it "leaves no thread of its own behind once the command is gone"
      ;; Every run owns a pump, a PTY reader and an attach acceptor. They are daemons,
      ;; so a leak is invisible until a long session has hundreds of them.
      (with-shell-on (fn []
                       (binding [workspace/*workspace-root* (workspace/trunk-root)]
                         (let
                           [sid "shell-thread-hygiene"
                            env {:session-id sid}]

                           (try (let [before (shell-thread-count)]
                                  (doseq [i (range 3)]
                                    (shell* env {"command" "echo x" "id" (str "t" i)}))
                                  (expect (pos? (long (poll shell-thread-count pos? 30))))
                                  (doseq [i (range 3)]
                                    (wait* env (str "t" i)))
                                  (expect (= before (poll shell-thread-count #(= before %) 60))))
                                (finally (resources/stop-all! sid)))))))))
