(ns com.blockether.vis.ext.language-clojure.repl-manager-test
  "Hermetic tests for the owned, session-scoped REPL manager. The actual
   subprocess self-start is exercised in REPL-driven verification, not here, so
   these stay fast and side-effect-free."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.core :as core]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.repl-manager :as rm]
            [com.blockether.vis.ext.language-clojure.shadow-repl :as shadow-repl]
            [com.blockether.vis.internal.process-jail :as process-jail]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:private manager-test-session-ids
  ["sess-fail" "sess-fail-2" "sess-race" "sess-slow" "sess-ens" "sess-ens-2" "s"])

(doseq [sid manager-test-session-ids]
  (process-jail/register-session-jail! sid
                                       (constantly {:roots-fn (constantly [(System/getProperty
                                                                             "java.io.tmpdir")])
                                                    :net-enabled? true
                                                    :disabled? true})))

(defn- tmp-dir
  ^String []
  (.getAbsolutePath (.toFile (Files/createTempDirectory "vis-rm-" (into-array FileAttribute [])))))

(defn- with-file [^String dir name content] (spit (io/file dir name) content) dir)

(defn- await-until
  "Poll `pred` (a nullary fn) until truthy or `timeout-ms` passes. Returns the
   final truthiness — lets tests wait on the async .onExit watcher without
   sleeping a fixed amount."
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) (long timeout-ms))]
    (loop []

      (cond (pred) true
            (< (System/currentTimeMillis) deadline) (do (Thread/sleep 50) (recur))
            :else false))))

(defdescribe
  launcher-for-test
  ;; launcher-for is now 3-arity: [dir aliases port]. We always know our port.
  (it "selects clojure for deps.edn"
      (expect (= :clj (:tool (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}") nil 12345)))))
  (it "selects lein for project.clj"
      (expect (= :lein
                 (:tool (rm/launcher-for (with-file (tmp-dir) "project.clj" "(defproject x)")
                                         nil
                                         12345)))))
  (it "selects bb for bb.edn"
      (expect (= :bb (:tool (rm/launcher-for (with-file (tmp-dir) "bb.edn" "{}") nil 12345)))))
  (it "returns nil when no known build file is present"
      (expect (nil? (rm/launcher-for (tmp-dir) nil 12345))))
  (it "the clojure launcher injects the nrepl dep and runs nrepl.cmdline on our explicit port"
      (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}") nil 61234))]
        (expect (= "clojure" (first cmd)))
        ;; nrepl.cmdline rides the synthetic `:vis/nrepl-launch` alias's :main-opts
        ;; inside the -Sdeps EDN, with --port <ours> so we never read a file back.
        (expect (some #(str/includes? (str %) "nrepl.cmdline") cmd))
        (expect (some #(str/includes? (str %) "--port") cmd))
        (expect (some #(str/includes? (str %) "61234") cmd))
        ;; -M carries only the synthetic launch alias when no user aliases
        (expect (some #(= "-M:vis/nrepl-launch" %) cmd))))
  (it "threads deps.edn aliases into the clojure -M flag"
      (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "deps.edn" "{}") [:dev :test] 12345))]
        ;; user aliases come first, then the synthetic launch alias (last-wins)
        (expect (some #(= "-M:dev:test:vis/nrepl-launch" %) cmd))))
  (it "launch aliases always include dev and test before explicit aliases"
      (expect (= [:dev :test] (#'rm/launch-aliases nil)))
      (expect (= [:dev :test :bench] (#'rm/launch-aliases ["dev" "bench"]))))
  (it "threads lein profiles via with-profile and passes our port"
      (let
        [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "project.clj" "(defproject x)")
                                    [:dev :test]
                                    55123))]
        (expect (some #(= "with-profile" %) cmd))
        (expect (some #(= "+dev,+test" %) cmd))
        (expect (some #(= "55123" %) cmd)))))

(defdescribe
  inherited-jvm-opts-test
  ;; A nested project whose own deps.edn declares no :jvm-opts must inherit the
  ;; workspace's launch-alias JVM options, so its nREPL never boots a bare JVM.
  (it
    "a nested project with no :jvm-opts inherits an ancestor's launch-alias opts"
    (let
      [parent
       (tmp-dir)

       _
       (with-file
         parent
         "deps.edn"
         "{:aliases {:dev {:jvm-opts [\"--enable-preview\"]} :test {:jvm-opts [\"-Dfoo=bar\"]}}}")

       child
       (str (io/file parent "svc"))

       _
       (.mkdirs (io/file child))

       _
       (with-file child "deps.edn" "{:deps {}}")]

      ;; nested dir declares none -> inherits the parent's :dev + :test opts
      (expect (= ["--enable-preview" "-Dfoo=bar"]
                 (rm/inherited-jvm-opts (io/file child) [:dev :test])))
      ;; the parent declares its OWN -> nothing inherited (already applied by -M)
      (expect (nil? (rm/inherited-jvm-opts (io/file parent) [:dev :test])))
      ;; launcher-for for the nested dir bakes the inherited opts into the alias
      (let
        [cmd
         (:cmd (rm/launcher-for child [:dev :test] 12345))

         sdeps
         (some #(when (str/includes? (str %) ":jvm-opts") (str %)) cmd)]

        (expect (some? sdeps))
        (expect (str/includes? sdeps "--enable-preview"))
        (expect (str/includes? sdeps "-Dfoo=bar")))
      ;; launcher-for for the parent does NOT duplicate (no :jvm-opts injected)
      (let [cmd (:cmd (rm/launcher-for parent [:dev :test] 12345))]
        (expect (not-any? #(str/includes? (str %) ":jvm-opts") cmd))))))

(defdescribe status+stop-test
             ;; status/stop are SESSION-scoped and return STRING-keyed lifecycle maps (they
             ;; cross the strings-only boundary as tool `:result`s).
             (it "status reports a down, unmanaged REPL with a stable id for a fresh dir"
                 (let
                   [dir
                    (tmp-dir)

                    s
                    (rm/status "sess-a" dir)]

                   (expect (= "status" (get s "result")))
                   (expect (= "down" (get s "status")))
                   (expect (= (rm/id-of dir) (get s "id")))
                   (expect (nil? (get s "running")))))
             (it "stop is a safe no-op when nothing is managed"
                 (let
                   [dir
                    (tmp-dir)

                    r
                    (rm/stop! "sess-a" dir)]

                   (expect (= "not-managed" (get r "result")))
                   (expect (= (rm/id-of dir) (get r "id"))))))

(defdescribe
  failed-start-test
  (it "returns failed with exit code and log tail the MOMENT the launcher dies (no deadline burn)"
      (let [dir (tmp-dir)]
        (with-redefs
          [rm/launcher-for (fn [_ _ port]
                             {:tool :fake
                              :cmd ["sh" "-c" (str "echo repl boom on " port "; exit 42")]})]
          (let
            [t0 (System/currentTimeMillis)
             r (rm/start! "sess-fail" dir)
             elapsed (- (System/currentTimeMillis) t0)]

            (expect (= "failed" (get r "result")))
            (expect (= "failed" (get r "status")))
            (expect (= 42 (get r "exit")))
            ;; the full deadline is 120s — a dead launcher must surface in seconds
            (expect (< elapsed 15000))
            (expect (str/includes? (get r "message") "exited before accepting connections"))
            (expect (some #(str/includes? % "repl boom on") (get r "log_tail")))
            (expect (= "down" (get (rm/status "sess-fail" dir) "status")))))))
  (it "records the failure so health reports :failed until an explicit stop clears it"
      (let [dir (tmp-dir)]
        (with-redefs
          [rm/launcher-for (fn [_ _ _]
                             {:tool :fake :cmd ["sh" "-c" "exit 7"]})]
          (rm/start! "sess-fail-2" dir)
          (let [f (rm/last-failure "sess-fail-2" dir)]
            (expect (some? f))
            (expect (= 7 (get f "exit"))))
          (expect (= :failed (rm/health "sess-fail-2" dir)))
          ;; an intentional stop clears the remembered failure -> :down
          (rm/stop! "sess-fail-2" dir)
          (expect (nil? (rm/last-failure "sess-fail-2" dir)))
          (expect (= :down (rm/health "sess-fail-2" dir)))))))

(defdescribe
  concurrent-start-no-duplicate-test
  (it
    "serializes racing start! calls for one [session dir]: spawns ONE REPL, the rest see already-running (no orphaned duplicate JVM)"
    (let
      [dir
       (tmp-dir)

       sid
       "sess-race"

       n
       8

       results
       (atom [])]

      (with-redefs
        [rm/launcher-for
         (fn [_ _ _]
           {:tool :fake :cmd ["sleep" "30"]})

         ;; the sleep never binds an nREPL; treat the boot as up so
         ;; start! KEEPS the process (we exercise the spawn guard,
         ;; not the port probe). The small free-port! delay widens
         ;; the check->swap window, so a MISSING lock would let
         ;; several threads through and orphan duplicate JVMs.
         rm/wait-until-up
         (fn [& _]
           :up)

         rm/free-port!
         (fn []
           (Thread/sleep 25)
           0)]

        (try (let
               [threads (mapv (fn [_]
                                (Thread. (fn []
                                           (swap! results conj
                                             (get (rm/start! sid dir) "result")))))
                              (range n))]
               (run! #(.start ^Thread %) threads)
               (run! #(.join ^Thread %) threads))
             (let [freqs (frequencies @results)]
               ;; exactly one thread spawned; the other n-1 re-checked UNDER the
               ;; lock and got already-running -- never a second process.
               (expect (= 1 (get freqs "started")))
               (expect (= (dec n) (get freqs "already-running")))
               ;; and the session owns exactly ONE live REPL.
               (expect (= 1 (count (rm/session-repls sid)))))
             (finally (rm/stop! sid dir)))))))

(defdescribe id-of-test
             (it "derives a stable nrepl:<dir> id for a path outside home"
                 (expect (= "nrepl:/x/y" (rm/id-of "/x/y"))))
             (it "canonicalizes so `.`/`..`/trailing-slash spellings of ONE dir collapse to ONE id"
                 (let [home (System/getProperty "user.home")]
                   (expect (= (rm/id-of home) (rm/id-of (str home "/"))))
                   (expect (= (rm/id-of home) (rm/id-of (str home "/x/.."))))))
             (it "homogenizes the user-home prefix to ~"
                 (let [home (System/getProperty "user.home")]
                   (expect (= "nrepl:~" (rm/id-of home)))
                   (expect (= "nrepl:~/vis" (rm/id-of (str home "/vis")))))))

(defdescribe
  resolve-target-ownership-test
  ;; The ownership contract: an explicit id names a REPL; one owned REPL is the
  ;; implicit default; with several, the default is the one owning default-dir
  ;; (else the first) — never a throw. session-repls is stubbed so no
  ;; subprocess is spawned.
  (it "uses the single owned REPL as the implicit default (no id needed)"
      (with-redefs
        [rm/session-repls (fn [_]
                            [{:id "nrepl:/p" :dir "/p" :port 7001}])]
        (expect (= {:id "nrepl:/p" :dir "/p" :port 7001} (rm/resolve-target! "sess" nil "/p")))))
  (it
    "resolves an explicit id to that owned REPL"
    (with-redefs
      [rm/session-repls (fn [_]
                          [{:id "nrepl:/a" :dir "/a" :port 1} {:id "nrepl:/b" :dir "/b" :port 2}])]
      (expect (= {:id "nrepl:/b" :dir "/b" :port 2} (rm/resolve-target! "sess" "nrepl:/b" "/a")))))
  (it "throws :clj/unknown-repl-id for an id with no live REPL"
      (with-redefs
        [rm/session-repls (fn [_]
                            [])]
        (let
          [t (try (rm/resolve-target! "sess" "nrepl:/nope" "/p")
                  :no-throw
                  (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
          (expect (= :clj/unknown-repl-id t)))))
  (it "treats id \"default\" as the sentinel, not a real resource id"
      (with-redefs
        [rm/session-repls (fn [_]
                            [{:id "nrepl:/a" :dir "/a" :port 1}
                             {:id "nrepl:/b" :dir "/b" :port 2}])]
        ;; "default" (any case) must NOT throw — it resolves the implicit default.
        (expect (= {:id "nrepl:/b" :dir "/b" :port 2} (rm/resolve-target! "sess" "default" "/b")))
        (expect (= {:id "nrepl:/a" :dir "/a" :port 1} (rm/resolve-target! "sess" "DEFAULT" "/a")))))
  (it "defaults to the REPL owning default-dir when >1 live and no id"
      (with-redefs
        [rm/session-repls (fn [_]
                            [{:id "nrepl:/a" :dir "/a" :port 1}
                             {:id "nrepl:/b" :dir "/b" :port 2}])]
        (expect (= {:id "nrepl:/b" :dir "/b" :port 2} (rm/resolve-target! "sess" nil "/b")))))
  (it "falls back to the first REPL when default-dir owns none"
      (with-redefs
        [rm/session-repls (fn [_]
                            [{:id "nrepl:/a" :dir "/a" :port 1}
                             {:id "nrepl:/b" :dir "/b" :port 2}])]
        (expect (= {:id "nrepl:/a" :dir "/a" :port 1} (rm/resolve-target! "sess" nil "/other"))))))

(defdescribe repl-start-tool-gating-test
             (it "\"status\" always succeeds (start/stop are never flag-gated)"
                 (expect (:success? (core/repl-start-fn {:workspace/root (tmp-dir) :session-id "s"}
                                                        "status"))))
             (it "rejects an unknown op"
                 (let
                   [t (try (core/repl-start-fn {:workspace/root (tmp-dir) :session-id "s"}
                                               "frobnicate")
                           :no-throw
                           (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
                   (expect (= :clj/bad-args t)))))

(defdescribe resolve-repl-dir-test
             ;; resolve-repl-dir returns canonical paths (stable process-map keys), so
             ;; expectations canonicalize too.
             (let
               [resolve
                #'core/resolve-repl-dir

                canon
                (fn [p]
                  (.getCanonicalPath (io/file p)))]

               (it "blank/nil dir resolves to the workspace root"
                   (let [root (tmp-dir)]
                     (expect (= (canon root) (resolve root nil)))
                     (expect (= (canon root) (resolve root "")))))
               (it "a relative dir resolves under the workspace root"
                   (let
                     [root
                      (tmp-dir)

                      _
                      (.mkdirs (io/file root "a" "b"))]

                     (expect (= (canon (io/file root "a" "b")) (resolve root "a/b")))))
               (it "an absolute dir is used as-is"
                   (let
                     [root
                      (tmp-dir)

                      abs
                      (tmp-dir)]

                     (expect (= (canon abs) (resolve root abs)))))
               (it "a leading ~ expands to the user's home dir (not a subdir of root)"
                   (let
                     [root
                      (tmp-dir)

                      home
                      (System/getProperty "user.home")]

                     (expect (= (canon home) (resolve root "~")))
                     (expect (= (canon (io/file home "foo" "bar")) (resolve root "~/foo/bar")))
                     ;; the same home target resolves to ONE id regardless of spelling
                     (expect (= (resolve root "~") (resolve root home)))))))

(defn- fake-proc
  "A fake `Process` with fixed liveness — no real child process, no OS timing:
   `wait-until-up`'s process checks are exercised deterministically."
  ^Process [alive?]
  (proxy [Process] [] (isAlive [] (boolean alive?))))

(defdescribe
  wait-until-up-test
  ;; Hermetic: `probe!` is stubbed (never touches a real socket) and the poll
  ;; interval shrunk, so every case is deterministic and runs in milliseconds.
  (it "returns :up as soon as the probe answers"
      (with-redefs
        [nrepl-client/probe! (fn [_]
                               {:status :up})]
        (expect (= :up (#'rm/wait-until-up nil 59870 60000)))))
  (it "returns :died immediately when the process exits before binding — never burns the deadline"
      (with-redefs
        [nrepl-client/probe! (fn [_]
                               {:status :down})]
        (let
          [t0 (System/currentTimeMillis)
           st (#'rm/wait-until-up (fake-proc false) 59871 60000)]

          (expect (= :died st))
          (expect (< (- (System/currentTimeMillis) t0) 1000)))))
  (it "returns :starting when the deadline passes with the process still alive"
      (with-redefs
        [nrepl-client/probe!
         (fn [_]
           {:status :down})

         rm/wait-poll-ms
         1]

        (expect (= :starting (#'rm/wait-until-up (fake-proc true) 59872 30)))))
  (it "tolerates a nil process (pure port probe)"
      (with-redefs
        [nrepl-client/probe!
         (fn [_]
           {:status :down})

         rm/wait-poll-ms
         1]

        (expect (= :starting (#'rm/wait-until-up nil 59873 30))))))

(defn- sleep-proc
  "A real, long-lived child process so `proc-alive?` is genuinely true (no
   with-redefs — lazytest `it` bodies run OUTSIDE the surrounding dynamic scope)."
  ^Process []
  (.start (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String ["sh" "-c" "sleep 30"]))))

(defdescribe
  health-probe-ms-test
  "The wedged-vs-slow-boot guard: a still-booting REPL is given its REMAINING
   cold-boot window so a legitimately slow boot is never killed + restarted
   mid-flight (the cross-eval restart cycle), while a dead / past-deadline
   process gets only the short grace before it is judged wedged."
  (it "gives a still-booting REPL the remaining cold-boot window (not the short grace)"
      (let [p (sleep-proc)]
        (try (let
               [ms (#'rm/health-probe-ms
                    {:process p :started-at (- (System/currentTimeMillis) 30000) :port 1})]
               (expect (> ms 5000))
               (expect (<= ms @#'rm/start-deadline-ms)))
             (finally (.destroyForcibly p)))))
  (it "gives a live process past its boot deadline only the short grace"
      (let [p (sleep-proc)]
        (try (expect (= 5000
                        (#'rm/health-probe-ms
                         {:process p
                          :started-at (- (System/currentTimeMillis) @#'rm/start-deadline-ms 1000)
                          :port 1})))
             (finally (.destroyForcibly p)))))
  (it "gives a live process with no :started-at only the short grace"
      (let [p (sleep-proc)]
        (try (expect (= 5000 (#'rm/health-probe-ms {:process p :port 1})))
             (finally (.destroyForcibly p)))))
  (it
    "treats a dead process as not booting — never waits out the boot window"
    (let
      [p (.start (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String ["sh" "-c" "exit 0"])))]
      (.waitFor p)
      (expect (false? (#'rm/booting? {:process p :started-at (System/currentTimeMillis) :port 1})))
      (expect (= 5000
                 (#'rm/health-probe-ms
                  {:process p :started-at (System/currentTimeMillis) :port 1}))))))

(defdescribe
  slow-start-watcher-test
  (it "reports :starting for a slow boot, then the .onExit watcher flips a later death to :failed"
      (let [dir (tmp-dir)]
        (with-redefs
          [rm/launcher-for (fn [_ _ _]
                             {:tool :fake :cmd ["sh" "-c" "sleep 30"]})
           nrepl-client/probe! (fn [_]
                                 {:status :down})
           rm/start-deadline-ms 150
           rm/wait-poll-ms 5]

          (let [r (rm/start! "sess-slow" dir)]
            (expect (= "starting" (get r "result")))
            (expect (= "starting" (get r "status")))
            (expect (= :starting (rm/health "sess-slow" dir)))
            ;; kill it behind the manager's back — the watcher must record a failure
            (let [p (:process (get @@#'rm/processes ["sess-slow" dir]))]
              (.destroyForcibly ^Process p))
            (expect (await-until #(some? (rm/last-failure "sess-slow" dir)) 5000))
            (expect (= :failed (rm/health "sess-slow" dir)))
            (expect (= "down" (get (rm/status "sess-slow" dir) "status")))
            ;; an explicit stop acknowledges the failure -> back to plain :down
            (rm/stop! "sess-slow" dir)
            (expect (= :down (rm/health "sess-slow" dir))))))))

(defdescribe start-failure-test
             (it "returns start!'s failed lifecycle map instead of swallowing it"
                 (let [dir (tmp-dir)]
                   (with-redefs
                     [rm/launcher-for (fn [_ _ _]
                                        {:tool :fake
                                         :cmd ["sh" "-c" "echo bad classpath; exit 1"]})]
                     (let [r (rm/start! "sess-ens" dir)]
                       (expect (= "failed" (get r "result")))
                       (expect (= 1 (get r "exit")))
                       (expect (some #(str/includes? % "bad classpath") (get r "log_tail")))))))
             (it "returns the no-launcher lifecycle map when the dir has no build file"
                 (let [r (rm/start! "sess-ens-2" (tmp-dir))]
                   (expect (= "no-launcher" (get r "result"))))))

(defdescribe resolve-target-no-repl-test
             ;; Eval is CONNECT-ONLY: with no live REPL, resolve-target! throws :clj/no-repl
             ;; (it NEVER autostarts). session-repls is stubbed empty so nothing is spawned.
             (it "throws :clj/no-repl when the session owns no live REPL"
                 (with-redefs
                   [rm/session-repls (fn [_]
                                       [])]
                   (let
                     [t (try (rm/resolve-target! "sess" nil "/p")
                             :no-throw
                             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
                     (expect (= :clj/no-repl t))))))

(defdescribe tail-log-test
             (it "returns [] for a missing / blank path and an empty file"
                 (expect (= [] (rm/tail-log nil)))
                 (expect (= [] (rm/tail-log "")))
                 (expect (= [] (rm/tail-log (str (io/file (tmp-dir) "nope.log")))))
                 (let [f (io/file (tmp-dir) "empty.log")]
                   (spit f "")
                   (expect (= [] (rm/tail-log (str f))))))
             (it "tails the LAST n lines"
                 (let [f (io/file (tmp-dir) "a.log")]
                   (spit f (str/join "\n" (map #(str "line-" %) (range 100))))
                   (expect (= ["line-97" "line-98" "line-99"] (rm/tail-log (str f) 3)))
                   (expect (= 100 (count (rm/tail-log (str f) 500))))))
             (it "reads O(tail) — only the end of a log far bigger than the 256KB tail window"
                 (let
                   [f
                    (io/file (tmp-dir) "big.log")

                    line
                    (apply str (repeat 120 "x"))]

                   ;; ~1.2MB, well past the tail window
                   (spit f (str/join "\n" (map #(str line "-" %) (range 10000))))
                   (let [tail (rm/tail-log (str f) 5)]
                     (expect (= 5 (count tail)))
                     (expect (= (str line "-9999") (last tail)))
                     (expect (= (str line "-9995") (first tail))))))
             (it "handles a trailing newline without a phantom empty line"
                 (let [f (io/file (tmp-dir) "t.log")]
                   (spit f "a\nb\n")
                   (expect (= ["a" "b"] (rm/tail-log (str f)))))))

;; Regression: every managed nREPL rooted in the SAME directory got the SAME
;; `~/.vis/logs/vis-nrepl-<dir>.log`. ProcessBuilder's output redirect TRUNCATES,
;; so a second session — or simply a restart — in that directory wiped the log the
;; first REPL was still writing into, and both resources reported one path.
(defdescribe log-file-test
             (it "mints a UNIQUE log file per REPL start, under ~/.vis/logs, keyed by dir"
                 (let
                   [dir
                    "/tmp/vis-rm-log-uniqueness"

                    a
                    (#'rm/log-file dir)

                    b
                    (#'rm/log-file dir)]

                   (expect (not= (.getName a) (.getName b)))
                   (expect (= (.getParentFile a) (.getParentFile b)))
                   (expect (str/starts-with? (.getName a) "vis-nrepl-"))
                   (expect (str/ends-with? (.getName a) ".log"))
                   ;; the project dir stays legible in the name, so a log is greppable
                   (expect (str/includes? (.getName a) "tmp_vis_rm_log_uniqueness"))))
             (it "keeps different directories in different log files"
                 (expect (not= (.getName (#'rm/log-file "/tmp/vis-rm-log-a"))
                               (.getName (#'rm/log-file "/tmp/vis-rm-log-b"))))))

(defdescribe clj-eval-cwd-routing-test
             ;; Regression: clj-eval-fn must READ `cwd` from the arg map and hand the
             ;; RESOLVED (canonical) directory to resolve-target! as its default-dir — so a
             ;; multi-REPL session routes the eval to the REPL rooted at that directory instead
             ;; of silently dropping `cwd` and always matching the workspace-root REPL.
             ;; resolve-target! and the nREPL client are stubbed: no subprocess, no socket.
             (it "hands the resolved (canonical) `cwd` to resolve-target! as default-dir"
                 (let
                   [root
                    (tmp-dir)

                    _
                    (.mkdirs (io/file root "sub"))

                    captured
                    (atom nil)]

                   (with-redefs
                     [rm/resolve-target!
                      (fn [_sid _rid default-dir]
                        (reset! captured default-dir)
                        {:id "nrepl:/x" :dir default-dir :port 7777})

                      nrepl-client/eval!
                      (fn [_]
                        {"value" "2"})]

                     (core/clj-eval-fn {:workspace/root root :session-id "s"}
                                       {"code" "(+ 1 1)" "cwd" "sub"})
                     (expect (= (.getCanonicalPath (io/file root "sub")) @captured)))))
             (it "defaults default-dir to the workspace root when no `cwd` is given"
                 (let
                   [root
                    (tmp-dir)

                    captured
                    (atom nil)]

                   (with-redefs
                     [rm/resolve-target!
                      (fn [_sid _rid default-dir]
                        (reset! captured default-dir)
                        {:id "nrepl:/r" :dir default-dir :port 7777})

                      nrepl-client/eval!
                      (fn [_]
                        {"value" "2"})]

                     (core/clj-eval-fn {:workspace/root root :session-id "s"} {"code" "(+ 1 1)"})
                     (expect (= (.getCanonicalPath (io/file root)) @captured)))))
             (it "an explicit `id` is still forwarded to resolve-target! (dir unchanged)"
                 (let
                   [root
                    (tmp-dir)

                    captured
                    (atom nil)]

                   (with-redefs
                     [rm/resolve-target!
                      (fn [_sid rid default-dir]
                        (reset! captured [rid default-dir])
                        {:id rid :dir default-dir :port 7777})

                      nrepl-client/eval!
                      (fn [_]
                        {"value" "2"})]

                     (core/clj-eval-fn {:workspace/root root :session-id "s"}
                                       {"code" "(+ 1 1)" "id" "nrepl:/b"})
                     (expect (= ["nrepl:/b" (.getCanonicalPath (io/file root))] @captured)))))
             (it "a stale explicit `id` does not block explicit-dir autostart"
                 (let
                   [root
                    (tmp-dir)

                    _
                    (.mkdirs (io/file root "sub"))

                    captured
                    (atom nil)]

                   (with-redefs
                     [rm/repl-by-id
                      (fn [_sid _rid]
                        nil)

                      rm/resolve-target!
                      (fn [_sid rid default-dir]
                        (reset! captured [rid default-dir])
                        {:id "nrepl:/sub" :dir default-dir :port 7777})

                      nrepl-client/eval!
                      (fn [_]
                        {"value" "2"})]

                     (core/clj-eval-fn {:workspace/root root :session-id "s"}
                                       {"code" "(+ 1 1)" "id" "nrepl:/stale" "cwd" "sub"})
                     (expect (= [nil (.getCanonicalPath (io/file root "sub"))] @captured))))))

(defdescribe
  clj-eval-clean-failure-test
  ;; A missing/unknown REPL is an EXPECTED condition: clj-eval-fn must
  ;; return a TIGHT failure envelope (one-line :message + :hint, NO
  ;; :trace / raw ExceptionInfo class / ex-data dump) instead of letting
  ;; the throw bubble into `ex->op-error`'s internal stack trace.
  (it "turns :clj/no-repl into a clean failure envelope (message + hint, no trace)"
      (with-redefs
        [rm/resolve-target! (fn [_sid _rid default-dir]
                              (throw (ex-info "boom" {:type :clj/no-repl :dir default-dir})))]
        (let
          [root (tmp-dir)
           res (core/clj-eval-fn {:workspace/root root :session-id "s"} {"code" "(+ 1 1)"})]

          (expect (false? (:success? res)))
          (expect (not (contains? (:error res) :trace)))
          (expect (str/includes? (get-in res [:error :message]) "repl"))
          ;; message names the DIR the resolution ran against (from :dir ex-data)
          (expect (str/includes? (get-in res [:error :message])
                                 (rm/home-relativize (.getCanonicalPath (io/file root)))))
          (expect (some? (get-in res [:error :hint]))))))
  (it "home-homogenizes the dir in the message (`~/vis`, never `/Users/you/vis`)"
      (let
        [home
         (System/getProperty "user.home")

         dir
         (str home java.io.File/separator "vis")]

        (with-redefs
          [rm/resolve-target! (fn [_sid _rid _default-dir]
                                (throw (ex-info "boom" {:type :clj/no-repl :dir dir})))]
          (let
            [res (core/clj-eval-fn {:workspace/root (tmp-dir) :session-id "s"} {"code" "(+ 1 1)"})
             msg (str (get-in res [:error :message]))]

            (expect (false? (:success? res)))
            (expect (str/includes? msg "~/vis"))
            (expect (not (str/includes? msg home)))))))
  (it "turns :clj/unknown-repl-id into a clean failure echoing the bad id"
      (with-redefs
        [rm/resolve-target! (fn [_sid _rid _default-dir]
                              (throw (ex-info "boom" {:type :clj/unknown-repl-id :id "ghost"})))]
        (let
          [res (core/clj-eval-fn {:workspace/root (tmp-dir) :session-id "s"}
                                 {"code" "(+ 1 1)" "id" "ghost"})]
          (expect (false? (:success? res)))
          (expect (not (contains? (:error res) :trace)))
          (expect (str/includes? (get-in res [:error :message]) "ghost")))))
  (it "re-throws an UNexpected ExceptionInfo (not swallowed as a clean failure)"
      (with-redefs
        [rm/resolve-target! (fn [_sid _rid _default-dir]
                              (throw (ex-info "other" {:type :clj/some-other})))]
        (expect (= :clj/some-other
                   (try (core/clj-eval-fn {:workspace/root (tmp-dir) :session-id "s"}
                                          {"code" "(+ 1 1)"})
                        :no-throw
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

(defdescribe
  connect-external-test
  (it "registers a reachable external nREPL, lists it, and stop! only detaches"
      (let
        [sid
         "s-ext-attach"

         dir
         (tmp-dir)]

        (with-redefs
          [nrepl-client/probe! (fn [_]
                                 {:status :up})]
          (let [r (rm/connect! sid dir {:host " localhost " :port 59999})]
            (expect (= "connected" (get r "result")))
            (expect (true? (get r "external")))
            (expect (= "localhost" (get r "host"))))
          (let [repls (rm/session-repls sid)]
            (expect (= 1 (count repls)))
            (expect (true? (:external? (first repls))))
            (expect (= 59999 (:port (first repls)))))
          ;; A second connect RE-attaches (address, build and all) — it never
          ;; answers with some other REPL this dir happens to have.
          (expect (= "reconnected" (get (rm/connect! sid dir {:port 59999}) "result")))
          (expect (= "detached" (get (rm/stop! sid dir) "result")))
          (expect (empty? (rm/session-repls sid))))))
  (it "refuses to register an unreachable address"
      (let
        [sid
         "s-ext-refuse"

         dir
         (tmp-dir)]

        (with-redefs
          [nrepl-client/probe! (fn [_]
                                 {:status :down})]
          (expect (= "unreachable" (get (rm/connect! sid dir {:port 59998}) "result")))
          (expect (empty? (rm/session-repls sid))))))
  (it "live-repl-for-dir REUSES an external attachment and never spawns over it"
      (let
        [sid
         "s-ext-live"

         dir
         (tmp-dir)]

        (with-redefs
          [nrepl-client/probe! (fn [_]
                                 {:status :up})]
          (rm/connect! sid dir {:port 59997})
          (let [r (rm/live-repl-for-dir sid dir)]
            (expect (= 59997 (:port r)))
            (expect (true? (:external? r)))))
        (with-redefs
          [nrepl-client/probe!
           (fn [_]
             {:status :down})

           rm/start!
           (fn [& _]
             (throw (ex-info "must not spawn over an external attachment" {})))]

          ;; Unreachable = nothing to reuse: the caller gets nil (run_tests then
          ;; uses a clean JVM) and the user's own server is left untouched.
          (expect (nil? (rm/live-repl-for-dir sid dir))))
        (rm/stop! sid dir))))

(defdescribe
  eval-host-threading-test
  (it "clj-eval-fn dials the RESOLVED target's host (external, non-localhost)"
      (let [captured (atom nil)]
        (with-redefs
          [rm/resolve-target!
           (fn [_sid _rid _default]
             {:id "nrepl:/ext" :dir "/ext" :port 4001 :host "devbox.internal" :external? true})
           nrepl-client/eval! (fn [{:keys [host port]}]
                                (reset! captured [host port])
                                {"value" "2"})]

          (core/clj-eval-fn {:workspace/root (tmp-dir) :session-id "s"} {"code" "(+ 1 1)"})
          (expect (= ["devbox.internal" 4001] @captured))))))


(def ^:private shadow-watching
  "What `shadow-repl/probe!` answers for a live `shadow-cljs watch app`: the ids
   the SERVER loaded, a running worker, and that build's target."
  {:shadow? true :builds ["npm" "app"] :worker? true :target :node-script})

(defn- fake-live-process
  "A Process that only answers `.isAlive`. Registry liveness asks nothing else, so
   a MANAGED REPL can be staged beside an attachment without spawning anything."
  ^Process []
  (proxy [Process] [] (isAlive [] true)))

(defn- manager-atom
  "The manager's PRIVATE registry atom named `sym`, resolved at runtime."
  [sym]
  @(ns-resolve 'com.blockether.vis.ext.language-clojure.repl-manager sym))

(defn- with-shadow
  "Call `f` against a STAGED shadow-cljs nREPL: `:probe` decides what the server
   is, `:select` whether the build selects, `:answer` what every eval — the
   connect ping included — replies."
  [{:keys [probe select answer]} f]
  (with-redefs
    [nrepl-client/probe!
     (fn [_]
       {:status :up})

     shadow-repl/probe!
     (fn [_]
       (or probe shadow-watching))

     shadow-repl/select!
     (fn [_]
       (or select {:selected? true :session-token "tok-1"}))

     shadow-repl/eval!
     (fn [_ _]
       (or answer {:selected? true :result {"value" "1"} :session-token "tok-1"}))]

    (f)))

;; Regression, issue #151: `repl` `connect` on a shadow-cljs project answered
;; "already-running" with the MANAGED JVM REPL's own port — the address asked for
;; was never dialled — and nothing could select a build, so every `repl_eval` in a
;; ClojureScript project silently evaluated as JVM Clojure.
(defdescribe
  connect-shadow-build-test
  (it "attaches to the build and reports which runtime an eval now lands in"
      (let
        [sid
         "s-shadow-attach"

         dir
         (tmp-dir)]

        (with-shadow {}
                     (fn []
                       (let [r (rm/connect! sid dir {:port 9999 :build "app"})]
                         (expect (= "connected" (get r "result")))
                         (expect (= "app" (get r "build")))
                         (expect (= "cljs" (get r "dialect")))
                         (expect (= "node-script" (get r "target")))
                         (expect (= "connected" (get r "runtime")))
                         ;; Its OWN id — the managed REPL for this dir keeps `nrepl:<dir>`.
                         (expect (str/ends-with? (get r "id") "#app")))))
        (rm/detach! sid dir)))
  (it "attaches a watch with no JS runtime too, and says what starts one"
      (let
        [sid
         "s-shadow-noruntime"

         dir
         (tmp-dir)]

        (with-shadow {:answer {:selected? true
                               :result {}
                               :session-token "tok-1"
                               :message (shadow-repl/runtime-hint "app" :node-script)}}
                     (fn []
                       (let [r (rm/connect! sid dir {:port 9999 :build "app"})]
                         ;; Healthy attachment, missing runtime: the difference has to be
                         ;; visible HERE, not as a broken-looking eval later.
                         (expect (= "connected" (get r "result")))
                         (expect (= "none" (get r "runtime")))
                         (expect (str/includes? (get r "message") "node")))))
        (rm/detach! sid dir)))
  (it "refuses a plain JVM nREPL when a build was named, and registers nothing"
      (let
        [sid
         "s-shadow-plain"

         dir
         (tmp-dir)]

        (with-shadow {:probe {:shadow? false :builds [] :worker? false}}
                     (fn []
                       (let [r (rm/connect! sid dir {:port 9999 :build "app"})]
                         (expect (= "not-shadow" (get r "result")))
                         (expect (str/includes? (get r "message") ".shadow-cljs/nrepl.port"))
                         (expect (empty? (rm/session-repls sid))))))))
  (it "names the builds the SERVER loaded when the build is unknown"
      (let
        [sid
         "s-shadow-unknown"

         dir
         (tmp-dir)]

        (with-shadow {}
                     (fn []
                       (let [r (rm/connect! sid dir {:port 9999 :build "ap"})]
                         (expect (= "unknown-build" (get r "result")))
                         (expect (= ["npm" "app"] (get r "builds")))
                         (expect (str/includes? (get r "message") "npm, app"))
                         (expect (empty? (rm/session-repls sid))))))))
  (it "refuses a build with no watch, naming the command that starts one"
      (let
        [sid
         "s-shadow-nowatch"

         dir
         (tmp-dir)]

        (with-shadow {:probe (assoc shadow-watching :worker? false)}
                     (fn []
                       (let [r (rm/connect! sid dir {:port 9999 :build "app"})]
                         ;; Same shadow error for an unknown build and an unwatched one —
                         ;; they are told apart before selecting, because the fixes differ.
                         (expect (= "no-watch" (get r "result")))
                         (expect (str/includes? (get r "message") "shadow-cljs watch app"))
                         (expect (empty? (rm/session-repls sid))))))))
  (it "refuses without a port and names the file a watch publishes"
      (let [r (rm/connect! "s-shadow-noport" (tmp-dir) {:build "app"})]
        (expect (= "no-port" (get r "result")))
        (expect (str/includes? (get r "message") ".shadow-cljs/nrepl.port"))
        (expect (str/includes? (get r "message") "shadow-cljs watch app"))))
  (it "reads the port the watch published when the caller gives none"
      (let
        [sid
         "s-shadow-portfile"

         dir
         (tmp-dir)

         f
         (apply io/file dir shadow-repl/port-file-path)]

        (io/make-parents f)
        (spit f "65432\n")
        (with-shadow {}
                     (fn []
                       (let [r (rm/connect! sid dir {:build "app"})]
                         (expect (= "connected" (get r "result")))
                         (expect (= 65432 (get r "port"))))))
        (rm/detach! sid dir)))
  (it
    "lives BESIDE the managed REPL for the same dir, and a repeat connect RE-attaches"
    (let
      [sid
       "s-shadow-both"

       dir
       (tmp-dir)]

      (try (swap! (manager-atom 'processes) assoc
             [sid dir]
             {:id (rm/id-of dir)
              :dir dir
              :port 7000
              :tool :clj
              :aliases [:dev :test]
              :pid 4242
              :process (fake-live-process)})
           (with-shadow {}
                        (fn []
                          (expect (= "connected"
                                     (get (rm/connect! sid dir {:port 9999 :build "app"})
                                          "result")))
                          ;; THE bug: the second connect found the MANAGED process and
                          ;; answered "already-running" on port 7000, having never
                          ;; dialled 9999 at all.
                          (let [again (rm/connect! sid dir {:port 9999 :build "app"})]
                            (expect (= "reconnected" (get again "result")))
                            (expect (= 9999 (get again "port"))))
                          (let [repls (rm/session-repls sid)]
                            (expect (= 2 (count repls)))
                            ;; The MANAGED REPL sorts first, so an eval naming no target
                            ;; still lands in the JVM one.
                            (expect (= [false true] (mapv #(boolean (:external? %)) repls)))
                            (expect (= 7000 (:port (first repls))))
                            (expect (= :cljs (:dialect (second repls)))))
                          (let [st (rm/status sid dir)]
                            (expect (= "up" (get st "status")))
                            (expect (= "app" (get-in st ["attached" "build"]))))
                          (expect (= "detached" (get (rm/detach! sid dir) "result")))
                          (expect (= 1 (count (rm/session-repls sid))))))
           (finally (swap! (manager-atom 'processes) dissoc [sid dir])))))
  (it "never offers a ClojureScript session to a JVM test run"
      (let
        [sid
         "s-shadow-jvm-run"

         dir
         (tmp-dir)]

        (with-shadow {}
                     (fn []
                       (rm/connect! sid dir {:port 9999 :build "app"})
                       ;; A session selected on a build cannot load a `.clj` test
                       ;; namespace: handing it over would read as broken tests.
                       (expect (nil? (rm/live-repl-for-dir sid dir)))
                       ;; The same attachment without a build is a JVM REPL, and IS reused.
                       (rm/connect! sid dir {:port 9999})
                       (expect (= 9999 (:port (rm/live-repl-for-dir sid dir))))))
        (rm/detach! sid dir))))

(defdescribe
  shadow-eval-routing-test
  (it "routes an attachment's eval through its build and writes the fresh session back"
      (let
        [sid
         "s-shadow-eval"

         dir
         (tmp-dir)]

        (with-shadow {}
                     (fn []
                       (rm/connect! sid dir {:port 9999 :build "app"})
                       (with-redefs
                         [shadow-repl/eval! (fn [_ _]
                                              {:selected? true
                                               :result {"value" "\"Hello, REPL!\"" "ns" "cljs.user"}
                                               :session-token "tok-2"})]
                         (let
                           [target (rm/resolve-target! sid (str (rm/id-of dir) "#app") dir)
                            r (rm/eval! sid target {:code "(repro.core/greeting)"})]

                           (expect (= "app" (:build target)))
                           (expect (= :cljs (:dialect target)))
                           (expect (= "\"Hello, REPL!\"" (get r "value")))
                           (expect (= "cljs.user" (get r "ns")))
                           (expect (= "app" (get r "build")))
                           ;; The session the eval selected under is the one the NEXT
                           ;; eval re-checks — stale token, silent JVM answers.
                           (expect (= "tok-2" (:session-token (first (rm/session-repls sid)))))))))
        (rm/detach! sid dir)))
  (it "reports a lost selection as a build problem, with the command that fixes it"
      (with-redefs
        [shadow-repl/eval! (fn [_ _]
                             {:selected? false :message "watch for build not running"})]
        (let
          [r (rm/eval! "s-shadow-lost"
                       {:host "localhost" :port 9999 :build "app" :dir "/p"}
                       {:code "1"})]
          (expect (nil? (get r "value")))
          (expect (str/includes? (get r "error_message") "watch for build not running"))
          (expect (str/includes? (get r "message") "shadow-cljs watch app")))))
  (it "sends a target with no build straight to the JVM nREPL, untouched"
      (let [captured (atom nil)]
        (with-redefs
          [nrepl-client/eval! (fn [opts]
                                (reset! captured opts)
                                {"value" "2"})
           shadow-repl/eval! (fn [_ _]
                               (throw (ex-info "a JVM eval must not go through shadow" {})))]

          (rm/eval! "s-shadow-jvm" {:host "devbox.internal" :port 4001} {:code "(+ 1 1)"})
          (expect (= ["devbox.internal" 4001] [(:host @captured) (:port @captured)]))))))

(defdescribe
  cljs-eval-through-core-test
  (it "clj-eval-fn on a build id lands in its JS runtime, not the JVM the server also serves"
      (with-redefs
        [rm/resolve-target!
         (fn [_sid _rid _default]
           {:id "nrepl:/p#app"
            :dir "/p"
            :host "localhost"
            :port 4001
            :external? true
            :dialect :cljs
            :build "app"
            :target :node-script
            :session-token "tok-1"})

         shadow-repl/eval!
         (fn [_ _]
           {:selected? true
            :result {"value" "\"Hello, REPL!\"" "ns" "cljs.user"}
            :session-token "tok-1"})

         nrepl-client/eval!
         (fn [_]
           (throw (ex-info "a ClojureScript eval must never dial the JVM directly" {})))]

        (let
          [res (core/clj-eval-fn {:workspace/root (tmp-dir) :session-id "s"}
                                 {"code" "(repro.core/greeting)" "id" "nrepl:/p#app"})]
          (expect (:success? res))
          (expect (= "\"Hello, REPL!\"" (get-in res [:result "value"])))
          (expect (= "cljs.user" (get-in res [:result "ns"])))
          (expect (= "app" (get-in res [:result "build"])))
          (expect (= "nrepl:/p#app" (get-in res [:result "repl"])))))))
