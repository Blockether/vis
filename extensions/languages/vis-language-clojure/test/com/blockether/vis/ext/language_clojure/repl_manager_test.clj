(ns com.blockether.vis.ext.language-clojure.repl-manager-test
  "Hermetic tests for the owned, session-scoped REPL manager. The actual
   subprocess self-start is exercised in REPL-driven verification, not here, so
   these stay fast and side-effect-free."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.core :as core]
            [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
            [com.blockether.vis.ext.language-clojure.repl-manager :as rm]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

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
             (it "derives a stable nrepl:<dir> id" (expect (= "nrepl:/x/y" (rm/id-of "/x/y")))))

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

(defdescribe ensure-repl-failure-test
             (it "returns start!'s failed lifecycle map instead of swallowing it"
                 (let [dir (tmp-dir)]
                   (with-redefs
                     [rm/launcher-for (fn [_ _ _]
                                        {:tool :fake
                                         :cmd ["sh" "-c" "echo bad classpath; exit 1"]})]
                     (let [r (rm/ensure-repl-for-dir! "sess-ens" dir)]
                       (expect (= "failed" (get r "result")))
                       (expect (= 1 (get r "exit")))
                       (expect (some #(str/includes? % "bad classpath") (get r "log_tail")))))))
             (it "returns the no-launcher lifecycle map when the dir has no build file"
                 (let [r (rm/ensure-repl-for-dir! "sess-ens-2" (tmp-dir))]
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

(defdescribe clj-eval-dir-routing-test
             ;; Regression: clj-eval-fn must READ `dir` from the arg map and hand the
             ;; RESOLVED (canonical) dir to resolve-target! as its default-dir — so a
             ;; multi-REPL session routes the eval to the REPL rooted at that dir instead
             ;; of silently dropping `dir` and always matching the workspace-root REPL.
             ;; resolve-target! and the nREPL client are stubbed: no subprocess, no socket.
             (it "hands the resolved (canonical) `dir` to resolve-target! as default-dir"
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
                                       {"code" "(+ 1 1)" "dir" "sub"})
                     (expect (= (.getCanonicalPath (io/file root "sub")) @captured)))))
             (it "defaults default-dir to the workspace root when no `dir` is given"
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
                                       {"code" "(+ 1 1)" "id" "nrepl:/stale" "dir" "sub"})
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
          (expect (str/includes? (get-in res [:error :message]) "repl_start"))
          ;; message names the DIR the resolution ran against (from :dir ex-data)
          (expect (str/includes? (get-in res [:error :message]) (.getCanonicalPath (io/file root))))
          (expect (some? (get-in res [:error :hint]))))))
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
          (expect (= "already-running" (get (rm/connect! sid dir {:port 59999}) "result")))
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
  (it "ensure-repl-for-dir! NEVER replaces an external attachment with a spawn"
      (let
        [sid
         "s-ext-ensure"

         dir
         (tmp-dir)]

        (with-redefs
          [nrepl-client/probe! (fn [_]
                                 {:status :up})]
          (rm/connect! sid dir {:port 59997})
          (let [r (rm/ensure-repl-for-dir! sid dir)]
            (expect (= 59997 (:port r)))
            (expect (true? (:external? r)))))
        (with-redefs
          [nrepl-client/probe!
           (fn [_]
             {:status :down})

           rm/start!
           (fn [& _]
             (throw (ex-info "must not spawn over an external attachment" {})))]

          (expect (= "external-unreachable" (get (rm/ensure-repl-for-dir! sid dir) "result"))))
        (rm/stop! sid dir))))

(defdescribe crash-loop-guard-test
             (it "suspends autostart after repeated crashes and an explicit stop resets it"
                 (let
                   [sid
                    "s-crash-loop"

                    dir
                    (tmp-dir)

                    spawned
                    (atom 0)]

                   (dotimes [_ 5]
                     (#'rm/note-crash! [sid dir]))
                   (expect (true? (rm/crash-looping? sid dir)))
                   (with-redefs
                     [rm/start! (fn [& _]
                                  (swap! spawned inc)
                                  {"result" "started"})]
                     (let [r (rm/ensure-repl-for-dir! sid dir)]
                       (expect (= "crash-looping" (get r "result")))
                       (expect (zero? @spawned))))
                   ;; explicit stop = a deliberate human reset of the guard
                   (rm/stop! sid dir)
                   (expect (false? (rm/crash-looping? sid dir)))
                   (with-redefs
                     [rm/start! (fn [& _]
                                  (swap! spawned inc)
                                  {"result" "started"})]
                     (expect (= "started" (get (rm/ensure-repl-for-dir! sid dir) "result")))
                     (expect (= 1 @spawned))))))

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
