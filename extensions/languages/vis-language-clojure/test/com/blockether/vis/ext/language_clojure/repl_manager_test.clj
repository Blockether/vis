(ns com.blockether.vis.ext.language-clojure.repl-manager-test
  "Hermetic tests for the owned, session-scoped REPL manager. The actual
   subprocess self-start is exercised in REPL-driven verification, not here, so
   these stay fast and side-effect-free."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.language-clojure.core :as core]
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
      (let [cmd (:cmd (rm/launcher-for (with-file (tmp-dir) "project.clj" "(defproject x)")
                                       [:dev :test]
                                       55123))]
        (expect (some #(= "with-profile" %) cmd))
        (expect (some #(= "+dev,+test" %) cmd))
        (expect (some #(= "55123" %) cmd)))))

(defdescribe status+stop-test
             ;; status/stop are SESSION-scoped and return STRING-keyed lifecycle maps (they
             ;; cross the strings-only boundary as tool `:result`s).
             (it "status reports a down, unmanaged REPL with a stable id for a fresh dir"
                 (let [dir
                       (tmp-dir)

                       s
                       (rm/status "sess-a" dir)]

                   (expect (= "status" (get s "result")))
                   (expect (= "down" (get s "status")))
                   (expect (= (rm/id-of dir) (get s "id")))
                   (expect (nil? (get s "running")))))
             (it "stop is a safe no-op when nothing is managed"
                 (let [dir
                       (tmp-dir)

                       r
                       (rm/stop! "sess-a" dir)]

                   (expect (= "not-managed" (get r "result")))
                   (expect (= (rm/id-of dir) (get r "id"))))))

(defdescribe
  failed-start-test
  (it "returns failed with exit code and log tail the MOMENT the launcher dies (no deadline burn)"
      (let [dir (tmp-dir)]
        (with-redefs [rm/launcher-for
                      (fn [_ _ port]
                        {:tool :fake :cmd ["sh" "-c" (str "echo repl boom on " port "; exit 42")]})]
          (let [t0 (System/currentTimeMillis)
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
        (with-redefs [rm/launcher-for (fn [_ _ _]
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

(defdescribe id-of-test
             (it "derives a stable nrepl:<dir> id" (expect (= "nrepl:/x/y" (rm/id-of "/x/y")))))

(defdescribe
  resolve-target-ownership-test
  ;; The ownership contract: an explicit id names a REPL; one owned REPL is the
  ;; implicit default; with several, the default is the one owning default-dir
  ;; (else the first) — never a throw. session-repls is stubbed so no
  ;; subprocess is spawned.
  (it "uses the single owned REPL as the implicit default (no id needed)"
      (with-redefs [rm/session-repls (fn [_]
                                       [{:id "nrepl:/p" :dir "/p" :port 7001}])]
        (expect (= {:id "nrepl:/p" :dir "/p" :port 7001} (rm/resolve-target! "sess" nil "/p")))))
  (it "resolves an explicit id to that owned REPL"
      (with-redefs [rm/session-repls (fn [_]
                                       [{:id "nrepl:/a" :dir "/a" :port 1}
                                        {:id "nrepl:/b" :dir "/b" :port 2}])]
        (expect (= {:id "nrepl:/b" :dir "/b" :port 2}
                   (rm/resolve-target! "sess" "nrepl:/b" "/a")))))
  (it "throws :clj/unknown-repl-id for an id with no live REPL"
      (with-redefs [rm/session-repls (fn [_]
                                       [])]
        (let [t (try (rm/resolve-target! "sess" "nrepl:/nope" "/p")
                     :no-throw
                     (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
          (expect (= :clj/unknown-repl-id t)))))
  (it "treats id \"default\" as the sentinel, not a real resource id"
      (with-redefs [rm/session-repls (fn [_]
                                       [{:id "nrepl:/a" :dir "/a" :port 1}
                                        {:id "nrepl:/b" :dir "/b" :port 2}])]
        ;; "default" (any case) must NOT throw — it resolves the implicit default.
        (expect (= {:id "nrepl:/b" :dir "/b" :port 2} (rm/resolve-target! "sess" "default" "/b")))
        (expect (= {:id "nrepl:/a" :dir "/a" :port 1} (rm/resolve-target! "sess" "DEFAULT" "/a")))))
  (it "defaults to the REPL owning default-dir when >1 live and no id"
      (with-redefs [rm/session-repls (fn [_]
                                       [{:id "nrepl:/a" :dir "/a" :port 1}
                                        {:id "nrepl:/b" :dir "/b" :port 2}])]
        (expect (= {:id "nrepl:/b" :dir "/b" :port 2} (rm/resolve-target! "sess" nil "/b")))))
  (it "falls back to the first REPL when default-dir owns none"
      (with-redefs [rm/session-repls (fn [_]
                                       [{:id "nrepl:/a" :dir "/a" :port 1}
                                        {:id "nrepl:/b" :dir "/b" :port 2}])]
        (expect (= {:id "nrepl:/a" :dir "/a" :port 1} (rm/resolve-target! "sess" nil "/other"))))))

(defdescribe
  clj-repl-tool-gating-test
  (it "\"status\" always succeeds (start/stop are never flag-gated)"
      (expect (:success? (core/clj-repl-fn {:workspace/root (tmp-dir) :session-id "s"} "status"))))
  (it "rejects an unknown op"
      (let [t (try (core/clj-repl-fn {:workspace/root (tmp-dir) :session-id "s"} "frobnicate")
                   :no-throw
                   (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
        (expect (= :clj/bad-args t)))))

(defdescribe resolve-repl-dir-test
             ;; resolve-repl-dir returns canonical paths (stable process-map keys), so
             ;; expectations canonicalize too.
             (let [resolve
                   #'core/resolve-repl-dir

                   canon
                   (fn [p]
                     (.getCanonicalPath (io/file p)))]

               (it "blank/nil dir resolves to the workspace root"
                   (let [root (tmp-dir)]
                     (expect (= (canon root) (resolve root nil)))
                     (expect (= (canon root) (resolve root "")))))
               (it "a relative dir resolves under the workspace root"
                   (let [root
                         (tmp-dir)

                         _
                         (.mkdirs (io/file root "a" "b"))]

                     (expect (= (canon (io/file root "a" "b")) (resolve root "a/b")))))
               (it "an absolute dir is used as-is"
                   (let [root
                         (tmp-dir)

                         abs
                         (tmp-dir)]

                     (expect (= (canon abs) (resolve root abs)))))
               (it "a leading ~ expands to the user's home dir (not a subdir of root)"
                   (let [root
                         (tmp-dir)

                         home
                         (System/getProperty "user.home")]

                     (expect (= (canon home) (resolve root "~")))
                     (expect (= (canon (io/file home "foo" "bar")) (resolve root "~/foo/bar")))
                     ;; the same home target resolves to ONE id regardless of spelling
                     (expect (= (resolve root "~") (resolve root home)))))))

(defdescribe
  wait-until-up-test
  (it "returns :died immediately when the process exits before binding — never burns the deadline"
      (let [p (.start (ProcessBuilder. ^java.util.List ["sh" "-c" "exit 3"]))]
        (.waitFor p)
        (let [t0 (System/currentTimeMillis)
              st (#'rm/wait-until-up p 59871 60000)]

          (expect (= :died st))
          (expect (< (- (System/currentTimeMillis) t0) 5000)))))
  (it "returns :starting when the deadline passes with the process still alive"
      (let [p (.start (ProcessBuilder. ^java.util.List ["sh" "-c" "sleep 30"]))]
        (try (expect (= :starting (#'rm/wait-until-up p 59872 600)))
             (finally (.destroyForcibly p)))))
  (it "tolerates a nil process (pure port probe)"
      (expect (= :starting (#'rm/wait-until-up nil 59873 300)))))

(defdescribe
  slow-start-watcher-test
  (it "reports :starting for a slow boot, then the .onExit watcher flips a later death to :failed"
      (let [dir (tmp-dir)]
        (with-redefs [rm/launcher-for (fn [_ _ _]
                                        {:tool :fake :cmd ["sh" "-c" "sleep 30"]})
                      rm/start-deadline-ms 600]

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
                   (with-redefs [rm/launcher-for (fn [_ _ _]
                                                   {:tool :fake
                                                    :cmd ["sh" "-c" "echo bad classpath; exit 1"]})]
                     (let [r (rm/ensure-repl-for-dir! "sess-ens" dir)]
                       (expect (= "failed" (get r "result")))
                       (expect (= 1 (get r "exit")))
                       (expect (some #(str/includes? % "bad classpath") (get r "log_tail")))))))
             (it "returns the no-launcher lifecycle map when the dir has no build file"
                 (let [r (rm/ensure-repl-for-dir! "sess-ens-2" (tmp-dir))]
                   (expect (= "no-launcher" (get r "result"))))))

(defdescribe
  resolve-target-start-failure-test
  (it "throws :clj/start-failed carrying the launcher's REAL story (exit + message + log tail)"
      (with-redefs [rm/session-repls
                    (fn [_]
                      [])

                    rm/ensure-repl-for-dir!
                    (fn [_ _]
                      {"result" "failed"
                       "exit" 1
                       "message" "Error building classpath. boom"
                       "log_tail" ["Error building classpath." "boom"]})]

        (let [e
              (try (rm/resolve-target! "sess" nil "/p") nil (catch clojure.lang.ExceptionInfo e e))]
          (expect (some? e))
          (expect (= :clj/start-failed (:type (ex-data e))))
          (expect (= 1 (:exit (ex-data e))))
          (expect (str/includes? (ex-message e) "Error building classpath"))
          (expect (str/includes? (ex-message e) "(exit 1)")))))
  (it "still throws :clj/no-launcher when there is truly no build file"
      (with-redefs [rm/session-repls
                    (fn [_]
                      [])

                    rm/ensure-repl-for-dir!
                    (fn [_ _]
                      {"result" "no-launcher" "status" "down"})]

        (let [t (try (rm/resolve-target! "sess" nil "/p")
                     :no-throw
                     (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
          (expect (= :clj/no-launcher t))))))

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
                 (let [f
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
