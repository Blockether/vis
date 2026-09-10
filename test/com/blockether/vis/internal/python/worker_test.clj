(ns com.blockether.vis.internal.python.worker-test
  "The session-worker process boundary: control messages are bounded and a
   retired interpreter can never be entered again."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.loop :as loop]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.python.host :as python-host]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis.internal.python.worker-peer :as worker-peer]
            [com.blockether.vis.internal.python.runtime]
            [com.blockether.vis.internal.sandbox.jail]
            [com.blockether.vis.internal.util]
            [com.blockether.vis-python-runtime]
            [lazytest.core :refer [defdescribe expect it]])
  (:import (java.util.concurrent.atomic AtomicLong)))

(defn- with-worker-context
  "Run `f` with the session of a fresh confined session worker, then dispose it."
  [f]
  (let [root
        (.getCanonicalPath (java.io.File. (System/getProperty "user.dir")))

        roots-fn
        (constantly [root])

        made
        (env/create-python-context {'worker-echo (fn [value]
                                                   value)}
                                   roots-fn
                                   {:worker? true
                                    :worker-policy-fn (fn []
                                                        {:roots-fn roots-fn :net-enabled? false})
                                    :jail-enabled? true
                                    :enabled? false}
                                   nil)

        session
        (:python-context made)]

    (try (f session) (finally (env/dispose-python-context! session)))))

(defdescribe
  worker-session-state-test
  (it "imports host modules and preserves globals between blocks in the runtime worker"
      (with-worker-context
        (fn [session]
          (expect (= "41\n"
                     (:stdout (env/run-python-block
                                session
                                "import vis_introspection
worker_value = 41
print(worker_value)"))))
          (expect (= "42\n" (:stdout (env/run-python-block session "print(worker_value + 1)"))))))))

(defdescribe
  shared-packages-install-authority-test
  (it "does not expose a package installer to sandbox code"
      (let [installs (atom 0)]
        (with-redefs [com.blockether.vis.internal.python.runtime/pip-install! (fn [& _]
                                                                                (swap! installs inc)
                                                                                {:exit 1})]
          (with-worker-context
            (fn [session]
              (let [result (env/run-python-block
                             session
                             (str "assert '__vis_pip_install__' not in globals()\n"
                                  "try:\n    import absent_shared_packages_fixture\n"
                                  "except ModuleNotFoundError:\n    print('missing')\n"))]
                (expect (nil? (:error result)))
                (expect (= "missing\n" (:stdout result)))
                (expect (zero? @installs)))))))))

(defdescribe reload-worker-cleanup-test
             ;; Regression: /reload only advanced the policy epoch, leaving idle workers
             ;; alive until another turn or the idle reaper eventually visited the session.
             (it
               "closes idle workers on reload and rebuilds lazily under the same turn lock"
               (with-worker-context
                 (fn [session]
                   (with-redefs [loop/cache
                                 (atom {})

                                 loop/policy-reload-epoch
                                 (atom 0)

                                 loop/env-max-turns-per-ctx
                                 (delay 0)]

                     (let [id
                           (java.util.UUID/randomUUID)

                           entry
                           (#'loop/new-cache-entry {:python-context session})

                           ^Process process
                           (:process (get @@#'worker/workers session))

                           hook
                           (get @@#'extension/reload-hooks
                                :com.blockether.vis.internal.loop/security-policy-reload)

                           builds
                           (atom 0)

                           fresh
                           {:marker :fresh}]

                       (swap! loop/cache assoc id entry)
                       (with-redefs [loop/open-env!
                                     (fn [_ _]
                                       (swap! builds inc)
                                       fresh)

                                     loop/turn!
                                     (fn [environment _ _]
                                       environment)]

                         (expect (.isAlive process))
                         (dotimes [_ 2]
                           (hook))
                         (expect (.waitFor process 2 java.util.concurrent.TimeUnit/SECONDS))
                         (expect (false? (worker/worker-live? session)))
                         (expect (zero? @builds))
                         (expect (identical? entry (get @loop/cache id)))
                         (expect (= fresh (loop/send! id "after reload")))
                         (expect (= 1 @builds))
                         (expect (identical? (:lock entry) (:lock (get @loop/cache id)))))))))))

(defdescribe worker-unexpected-exit-test
             ;; Regression: a native image-library crash must not silently replace a fully
             ;; equipped sandbox with a bare interpreter under the same context name.
             (it "refuses a dead context and lets the next environment rebuild its tools"
                 (with-worker-context
                   (fn [session]
                     (let [^Process process (:process (get @@#'worker/workers session))]
                       (.destroyForcibly process)
                       (expect (.waitFor process 10 java.util.concurrent.TimeUnit/SECONDS))
                       (expect (false? (env/context-enterable? {:python-context session})))
                       (let [answer (try (env/run-python-block session "print('must not run')")
                                         (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                         (expect (re-find #"python-worker-retired" (str answer))))
                       (expect (false? (worker/worker-live? session)))
                       (with-worker-context
                         (fn [fresh-session]
                           (expect (not= session fresh-session))
                           (expect (env/context-enterable? {:python-context fresh-session}))
                           (expect (= "restored\n"
                                      (:stdout (env/run-python-block
                                                 fresh-session
                                                 "print(await worker_echo('restored'))")))))))))))

(defdescribe guest-sources-location-test
             ;; Vis #185, CI 34498148579: a prior native test removed the cached guest directory.
             (it "stages guest modules in the current home and restores removed files"
                 (let [base
                       (java.nio.file.Files/createTempDirectory
                         (.toPath (java.io.File. "target"))
                         "worker-guest-sources-"
                         (make-array java.nio.file.attribute.FileAttribute 0))

                       old-home
                       (System/getProperty "user.home")]

                   (try (doseq [name ["first" "second"]]
                          (let [home (io/file (.toFile base) name)]
                            (.mkdirs home)
                            (System/setProperty "user.home" (.getCanonicalPath home))
                            (let [directory (worker/guest-source-dir)]
                              (expect (.startsWith (.toPath (io/file directory))
                                                   (.toPath (.getCanonicalFile home))))
                              (doseq [^java.io.File file (reverse (file-seq home))]
                                (java.nio.file.Files/deleteIfExists (.toPath file)))
                              (expect (= directory (worker/guest-source-dir)))
                              (doseq [module ["vis_introspection.py" "vis_results.py"]]
                                (let [file (io/file (worker/guest-source-dir) module)]
                                  (expect (= (slurp (io/resource (str "vis-guest/" module)))
                                             (when (.isFile file) (slurp file)))))))))
                        (finally (System/setProperty "user.home" old-home)
                                 (doseq [^java.io.File file (reverse (file-seq (.toFile base)))]
                                   (java.nio.file.Files/deleteIfExists (.toPath file))))))))

(defdescribe
  worker-long-home-test
  ;; The installed SDK's isolated HOME exposed the Unix socket's 104/108-byte limit.
  (it
    "starts from a long home without broadening or retaining its IPC grant"
    (let [base
          (java.nio.file.Files/createTempDirectory (.toPath (java.io.File. "target"))
                                                   "worker-home-"
                                                   (make-array java.nio.file.attribute.FileAttribute
                                                               0))

          dir
          (.toFile (.resolve base (apply str (repeat 110 "x"))))

          socket
          (atom nil)

          launch
          @#'worker/launch-policy!]

      (.mkdirs dir)
      (try (with-redefs-fn {#'worker/worker-dir (constantly dir)
                            #'worker/launch-policy!
                            (fn [k run-directory control-socket boot-read-paths]
                              (reset! socket (java.io.File. ^String control-socket))
                              (expect (< (count (.getBytes ^String control-socket "UTF-8")) 104))
                              (expect (= "rwx------"
                                         (java.nio.file.attribute.PosixFilePermissions/toString
                                           (java.nio.file.Files/getPosixFilePermissions
                                             (.toPath (.getParentFile ^java.io.File @socket))
                                             (make-array java.nio.file.LinkOption 0)))))
                              (launch k run-directory control-socket boot-read-paths))}
             (fn []
               (with-worker-context
                 (fn [session]
                   (expect (= "42\n" (:stdout (env/run-python-block session "print(42)"))))))))
           (expect (some? @socket))
           (when-let [^java.io.File path @socket]
             (expect (not (.exists path)))
             (expect (not (.exists (.getParentFile path)))))
           (finally (doseq [^java.io.File file (reverse (file-seq (.toFile base)))]
                      (java.nio.file.Files/deleteIfExists (.toPath file))))))))

;; Regression, CI run 33987564965: the first pip install was invisible to
;; a worker whose package directory did not exist when its jail was installed.
(defdescribe
  cold-package-directory-test
  (it
    "imports a package installed after a cold worker starts"
    (let [base
          (java.nio.file.Files/createTempDirectory (.toPath (java.io.File. "target"))
                                                   "cold-worker-packages-"
                                                   (make-array java.nio.file.attribute.FileAttribute
                                                               0))

          packages
          (.resolve base "packages")

          module
          (.resolve packages "cold_release_module.py")]

      (try
        (with-redefs [com.blockether.vis-python-runtime/packages-dir (constantly (str packages))]
          (with-worker-context
            (fn [session]
              (expect (.isDirectory (.toFile packages)))
              (spit (.toFile module) "answer = 42\n")
              (expect
                (=
                  "42\n"
                  (:stdout
                    (env/run-python-block
                      session
                      "import importlib; importlib.invalidate_caches(); import cold_release_module; print(cold_release_module.answer)")))))))
        (finally (doseq [path [module packages base]]
                   (java.nio.file.Files/deleteIfExists path)))))))

(defdescribe worker-profiler-options-test
             ;; Full-suite JFR reproduction: a confined worker inherited the parent's
             ;; recording path and failed before connecting to its control socket.
             (it "does not inherit a parent recording or repository"
                 (expect (= ["-Xmx2g" "--enable-native-access=ALL-UNNAMED" "-Dvis.example=true"]
                            (vec (#'worker/worker-jvm-options
                                  ["-Xmx2g" "-XX:StartFlightRecording=filename=parent.jfr"
                                   "--enable-native-access=ALL-UNNAMED"
                                   "-XX:FlightRecorderOptions=repository=parent-recordings"
                                   "-Dvis.example=true"])))))
             (it "launches the worker without the current JVM's recording options"
                 (with-redefs [com.blockether.vis.internal.util/native-image? (constantly false)]
                   (expect (not-any?
                             #(re-find #"^-XX:(StartFlightRecording|FlightRecorderOptions)" %)
                             (#'worker/child-argv nil "/tmp/control.sock" "/tmp/host-modules"))))))

(defdescribe
  worker-entrypoint-test
  (it "launches the runtime Java worker with the control socket and host modules"
      (with-redefs [com.blockether.vis.internal.util/native-image? (constantly false)]
        (let [argv (#'worker/child-argv nil "/tmp/control.sock" "/tmp/host-modules")]
          (expect (= ["com.blockether.vispython.Worker" "/tmp/control.sock" "/tmp/host-modules"]
                     (vec (take-last 3 argv)))))))
  (it "makes runtime sources and their extraction marker readable at worker boot"
      (let [paths (set (#'worker/boot-read-paths nil "/tmp/host-modules"))]
        (expect (contains? paths
                           (.getCanonicalPath (java.io.File.
                                                (com.blockether.vispython.Locations/sourcesDir)))))
        (doseq [path (com.blockether.vispython.Sources/roots)]
          (expect (contains? paths (.getCanonicalPath (java.io.File. ^String path)))))))
  (it "launches the runtime executable with the parent's home in native Vis"
      (with-redefs [com.blockether.vis.internal.util/native-image?
                    (constantly true)

                    com.blockether.vis-python-runtime/resolve-worker
                    (fn [library]
                      (expect (= {:path "/runtime/libvispython.so"} library))
                      "/runtime/vis-python-worker")]

        (expect (= ["/runtime/vis-python-worker"
                    (str "-Duser.home=" (System/getProperty "user.home")) "/tmp/control.sock"
                    "/tmp/host-modules"]
                   (#'worker/child-argv
                    "/runtime/libvispython.so"
                    "/tmp/control.sock"
                    "/tmp/host-modules")))))
  (it "refuses a native runtime without its worker instead of starting Vis again"
      (with-redefs [com.blockether.vis.internal.util/native-image?
                    (constantly true)

                    com.blockether.vis-python-runtime/resolve-worker
                    (constantly nil)]

        (expect (= :vis/python-worker-missing
                   (try (#'worker/child-argv nil "/tmp/control.sock" "/tmp/host-modules")
                        (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))))

(defdescribe worker-host-authorization-test
             (it "rejects callers not assigned to the connection before host dispatch"
                 (let [peer
                       {:serving (atom {}) :host-sessions (atom #{"owned"})}

                       calls
                       (atom [])

                       replies
                       (atom [])]

                   (with-redefs [python-host/dispatch
                                 (fn [caller tool payload]
                                   (swap! calls conj [caller tool payload])
                                   "ok")

                                 worker-peer/send-line!
                                 (fn [_ reply]
                                   (swap! replies conj reply))]

                     (doseq [[id caller] [[1 "other"] [2 nil] [3 "owned"]]]
                       (#'worker/serve-host-call!
                        peer
                        {"id" id "session" caller "tool" "fixture" "payload" "{}"}))
                     (expect (= [["owned" "fixture" "{}"]] @calls))
                     (expect (every? #(contains? % "error") (take 2 @replies)))
                     (expect (= {"id" 3 "value" "ok"} (last @replies)))
                     (expect (empty? @(:serving peer)))))))

(defdescribe
  worker-host-authorization-lifecycle-test
  (it "assigns on host bootstrap, revokes on close, and rolls back failed bootstrap"
      (let [sessions
            (atom #{})

            peer
            {:host-sessions sessions}

            fail?
            (atom false)

            observed
            (atom [])]

        (with-redefs-fn {#'worker/live (fn [_]
                                         {:peer peer})
                         #'worker-peer/request!
                         (fn [_ message _]
                           (swap! observed conj [(get message "op") @sessions])
                           (when @fail? (throw (ex-info "bootstrap failure" {}))))}
          (fn []
            (worker/install-runtime! "worker" "owned")
            (expect (= #{"owned"} @sessions))
            (worker/close-session! "worker" "owned")
            (expect (empty? @sessions))
            (reset! fail? true)
            (expect (= "bootstrap failure"
                       (try (worker/install-runtime! "worker" "failed")
                            (catch clojure.lang.ExceptionInfo error (ex-message error)))))
            (expect (empty? @sessions))
            (expect (= [["install-runtime" #{"owned"}] ["close" #{}]
                        ["install-runtime" #{"failed"}]]
                       @observed)))))))

(defn- assert-trusted-cancellation
  [native-wait?]
  (with-worker-context
    (fn [session]
      (let [key
            (worker/extension-worker-key session)

            caller
            (str session "-extension")

            marker
            (doto (java.io.File/createTempFile "vis-extension-wait-" ".ready") (.delete))

            entered
            (promise)

            released
            (promise)

            retired
            (atom false)

            dispatch
            python-host/dispatch]

        (try (worker/trust! key caller true)
             (worker/install-runtime! key caller)
             (worker/exec!
               key
               caller
               (str "import threading, vis_runtime\nextension_value = 42\n"
                    "def wait_for_cancel():\n"
                    (if native-wait?
                      (str "    condition = threading.Condition()\n"
                           "    def notify_waiting():\n        with condition:\n"
                           "            with open("
                           (pr-str (str marker))
                           ", 'w') as ready:\n"
                           "                ready.write('waiting')\n" "    with condition:\n"
                           "        threading.Thread(target=notify_waiting, daemon=True).start()\n"
                           "        condition.wait(30)\n")
                      "    return vis_runtime.host_call('extension-blocked', '{}')\n")))
             (let [process ^Process (:process (get @(var-get #'worker/workers) key))]
               (with-redefs [python-host/dispatch (fn [host-session tool payload]
                                                    (case tool
                                                      "extension-wait"
                                                      (worker/run key caller "wait_for_cancel()")

                                                      "extension-blocked"
                                                      (do (deliver entered true)
                                                          (deref released 30000 "null"))

                                                      (dispatch host-session tool payload)))]
                 (let [execution (future (env/run-python-block
                                           session
                                           (str "import vis_runtime\nworker_value = 41\n"
                                                "vis_runtime.host_call('extension-wait', '{}')")))]
                   (try (expect (if native-wait?
                                  (loop [remaining 500]
                                    (cond (.exists marker) true
                                          (or (zero? remaining) (realized? execution)) false
                                          :else (do (Thread/sleep 10) (recur (dec remaining)))))
                                  (true? (deref entered 5000 false))))
                        (expect (true? (#'loop/interrupt-block!
                                        session
                                        execution
                                        {:python-context-retired-atom retired})))
                        (expect (not= ::parked (deref execution 5000 ::parked)))
                        (if native-wait?
                          (do
                            ;; The caller can finish while its trusted extension remains in C.
                            (expect (.waitFor process 10 java.util.concurrent.TimeUnit/SECONDS))
                            (expect (true? @retired))
                            (expect (false? (env/context-enterable? {:python-context session}))))
                          (do
                            ;; New work may outlive the previous interrupt's unwind deadline.
                            (expect (= "41\n"
                                       (:stdout
                                         (env/run-python-block
                                           session
                                           "import time\ntime.sleep(3)\nprint(worker_value)"))))
                            (expect (= "42" (worker/eval-str key caller "extension_value")))
                            (expect (identical? process
                                                (:process (get @(var-get #'worker/workers) key))))
                            (expect (.isAlive process))
                            (expect (false? @retired))
                            (expect (env/context-enterable? {:python-context session}))))
                        (finally (future-cancel execution))))))
             (finally (deliver released "null") (worker/stop-worker! key) (.delete marker)))))))

(defdescribe trusted-extension-cancellation-test
             (it "retires a trusted extension stuck in C after its sandbox caller unwinds"
                 (assert-trusted-cancellation true))
             (it "preserves both interpreters and subsequent work after cancelling a host wait"
                 (assert-trusted-cancellation false)))

(defdescribe
  worker-reply-lifetime-test
  (it "keeps a cancelled caller's completion until the actual reply or peer close"
      (doseq [reply-line ["{\"id\":1,\"value\":42}\n" ""]]
        (let [sent (promise)
              finished (promise)
              peer {:pending (atom {})
                    :seq (AtomicLong. 0)
                    :reader (java.io.BufferedReader. (java.io.StringReader. reply-line))
                    :workers (java.util.concurrent.Executors/newSingleThreadExecutor)}]

          (try (with-redefs [worker-peer/send-line! (fn [_ _]
                                                      (deliver sent true))]
                 (let [call (future (try (worker-peer/request! peer {"op" "run"})
                                         (finally (deliver finished true))))]
                   (try (expect (true? (deref sent 1000 false)))
                        (let [waiting (get @(:pending peer) 1)]
                          (future-cancel call)
                          (expect (true? (deref finished 1000 false)))
                          (expect (identical? waiting (get @(:pending peer) 1)))
                          (expect (not (realized? waiting)))
                          (worker-peer/pump! peer
                                             (fn [_ _])
                                             (constantly "closed"))
                          (expect (= (if (empty? reply-line) {"error" "closed"} {"id" 1 "value" 42})
                                     (deref waiting 1000 ::pending)))
                          (expect (empty? @(:pending peer))))
                        (finally (future-cancel call)))))
               (finally (.close ^java.io.BufferedReader (:reader peer))
                        (.shutdownNow ^java.util.concurrent.ExecutorService (:workers peer))))))))

(defdescribe
  worker-control-plane-test
  (it "interrupts trusted extension waits before releasing the session sandbox"
      ;; SDK cancellation must close an extension's input view before ending the turn.
      (with-worker-context (fn [session]
                             (let [calls (atom [])]
                               (with-redefs [worker/interrupt! (fn [key caller]
                                                                 (swap! calls conj [key caller])
                                                                 true)]
                                 (expect (true? (env/interrupt-guest! session))))
                               (expect (= [[(worker/extension-worker-key session) session]
                                           [session session]]
                                          @calls))))))
  (it "reports an extension interrupt even if its sandbox already stopped"
      (with-worker-context (fn [session]
                             (with-redefs [worker/interrupt! (fn [key _]
                                                               (not= session key))]
                               (expect (true? (env/interrupt-guest! session)))))))
  (it "bounds an interrupt whose child never replies"
      (let [pending
            (atom {})

            peer
            {:pending pending :serving (atom {}) :seq (AtomicLong. 0)}]

        (swap! @#'worker/workers assoc "session" {:peer peer})
        (with-redefs-fn {#'worker/alive? (fn [state]
                                           (= peer (:peer state)))
                         #'worker-peer/send-line! (fn [_ _]
                                                    nil)}
          (fn []
            (let [call
                  (future (try (worker/interrupt! "session" "sandbox")
                               (catch clojure.lang.ExceptionInfo e (ex-data e))))

                  observed
                  (deref call 1500 ::blocked)]

              ;; Release the old, unbounded implementation so a failing run leaves
              ;; no parked future behind.
              (doseq [[_ waiting] @pending]
                (deliver waiting {"value" false}))
              (deref call 1000 nil)
              (swap! @#'worker/workers dissoc "session")
              (expect (= :vis/python-worker-timeout (:type observed))))))))
  (it
    "fails a host call in flight so an interrupted guest parked in it unwinds"
    (with-worker-context
      (fn [session]
        (let [dispatch
              (deref #'python-host/dispatch)

              entered
              (promise)

              released
              (promise)]

          (with-redefs-fn {#'python-host/dispatch
                           (fn [caller tool payload]
                             (if (= "slow" tool)
                               (do (deliver entered true) (deref released 30000 nil) "\"late\"")
                               (dispatch caller tool payload)))}
            (fn []
              (let [block (future
                            (env/run-python-block
                              session
                              "import vis_runtime\nprint(vis_runtime.host_call('slow', '{}'))"))]
                (try (expect (true? (deref entered 5000 false)))
                     (expect (true? (env/interrupt-guest! session)))
                     (let [answer (deref block 5000 ::parked)]
                       (expect (not= ::parked answer))
                       ;; The failed host call lets the guest run Python again,
                       ;; where the pending KeyboardInterrupt lands; either
                       ;; spelling is the block ending for the right reason.
                       (expect (re-find #"(?i)interrupt" (str answer))))
                     (expect (worker/worker-live? session))
                     ;; The same worker, its tools intact, serves the next block.
                     (expect (= "ok\n" (:stdout (env/run-python-block session "print('ok')"))))
                     (finally (deliver released true))))))))))
  (it "refuses to restart a retired worker until its session is rebuilt"
      (with-worker-context (fn [session]
                             (expect (= "1\n" (:stdout (env/run-python-block session "print(1)"))))
                             (worker/retire-worker! session "test")
                             (expect (worker/retired? session))
                             (expect (false? (worker/worker-live? session)))
                             (let [answer (try (env/run-python-block session "print(2)")
                                               (catch clojure.lang.ExceptionInfo e (ex-data e)))]
                               (expect (re-find #"python-worker-retired" (str answer))))
                             (expect (false? (worker/worker-live? session)))
                             (worker/configure! session (constantly {}))
                             (expect (false? (worker/retired? session))))))
  (it "refuses an environment whose worker was retired"
      (expect (false? (env/context-enterable? {:python-context "retired-session"
                                               :python-context-retired-atom (atom true)}))))
  (it "confines a session worker before its first interpreter operation"
      (with-worker-context
        (fn [session]
          (let [answer (env/run-python-block
                         session
                         "import os\nprint(os.environ.get('VIS_SEATBELT_ACTIVE', 'missing'))")]
            (expect (= "1\n" (:stdout answer)))))))
  (it "kills only a worker whose interrupt control plane failed"
      (let [retired
            (atom false)

            stopped
            (atom [])

            release-task
            (promise)

            exec-task
            (future @release-task)]

        (try (with-redefs-fn {#'env/interrupt-guest! (fn [_]
                                                       (throw (ex-info "worker unavailable" {})))
                              #'env/retire-python-context! (fn [session]
                                                             (swap! stopped conj session))}
               (fn []
                 (expect (false? ((deref #'loop/interrupt-block!)
                                   "broken-session"
                                   exec-task
                                   {:python-context-retired-atom retired})))
                 (expect (true? @retired))
                 (expect (= ["broken-session"] @stopped))))
             (finally (deliver release-task true)))))
  (it "retires a worker that accepts an interrupt but never unwinds"
      (let [retired
            (atom false)

            stopped
            (promise)

            release-task
            (promise)

            exec-task
            (future @release-task)]

        (try (with-redefs-fn {#'loop/INTERRUPT_UNWIND_MS 25
                              #'env/interrupt-guest! (fn [_]
                                                       true)
                              #'env/retire-python-context! (fn [session]
                                                             (deliver stopped session))}
               (fn []
                 (expect (true? ((deref #'loop/interrupt-block!)
                                  "stuck-native-session"
                                  exec-task
                                  {:python-context-retired-atom retired})))
                 (expect (= "stuck-native-session" (deref stopped 1000 ::not-stopped)))
                 (expect (true? @retired))))
             (finally (deliver release-task true)))))
  (it "includes every worker process in the runtime RSS ownership set"
      (let [self (.pid (java.lang.ProcessHandle/current))]
        (with-redefs-fn {#'env/python-worker-pids (fn []
                                                    [self 424242 424242])}
          #(expect (= [self 424242] (vec ((deref #'loop/runtime-pids))))))))
  (it
    "kills the real session process when a native wait does not unwind"
    (let [marker
          (java.io.File/createTempFile "vis-native-wait-" ".ready")

          _
          (.delete marker)

          made
          (env/create-python-context {}
                                     (constantly [])
                                     {:worker? true
                                      :jail-enabled? false
                                      :enabled? false
                                      :allowed-domains []
                                      :denied-domains []
                                      :exclude-domains []}
                                     nil)

          session
          (:python-context made)

          ^Process process
          (:process (get @(var-get #'worker/workers) session))

          retired
          (atom false)

          ;; The notifier acquires the condition only after the main thread
          ;; releases it in wait: interruption cannot overtake the native wait.
          execution
          (future (env/run-python-block
                    session
                    (str "import threading\n"
                         "condition = threading.Condition()\n"
                         "def notify_waiting():\n"
                         "    with condition:\n"
                         "        with open("
                         (pr-str (str marker))
                         ", 'w') as ready_file:\n"
                         "            ready_file.write('ready')\n" "with condition:\n"
                         "    threading.Thread(target=notify_waiting, daemon=True).start()\n"
                         "    condition.wait(30)\n")))]

      (try (expect (loop [remaining 500]
                     (cond (.exists marker) true
                           (or (zero? remaining) (realized? execution)) false
                           :else (do (Thread/sleep 10) (recur (dec remaining))))))
           (expect (true? ((deref #'loop/interrupt-block!)
                            session
                            execution
                            {:python-context-retired-atom retired})))
           (expect (true? (.waitFor process 10 java.util.concurrent.TimeUnit/SECONDS)))
           (expect (true? @retired))
           (expect (false? (worker/worker-live? session)))
           (expect (false? (.isAlive process)))
           (finally (try (env/dispose-python-context! session) (catch Throwable _ nil))
                    (.delete marker)))))
  (it "reclaims a condemned environment's worker before detaching it"
      (let [retired
            (atom false)

            stopped
            (atom [])

            key
            (random-uuid)

            entry
            {:environment {:python-context "abandoned-session"
                           :python-context-retired-atom retired}}]

        (with-redefs-fn {#'loop/cache (atom {key entry})
                         #'env/retire-python-context! (fn [session]
                                                        (swap! stopped conj session))}
          (fn []
            (expect (true? ((deref #'loop/detach-entry!) key entry)))
            (expect (true? @retired))
            (expect (= ["abandoned-session"] @stopped))))))
  (it "keeps a healthy worker when interrupt lost a completion race"
      (let [retired
            (atom false)

            stopped
            (atom [])

            release-task
            (promise)

            exec-task
            (future @release-task)]

        (try (with-redefs-fn {#'env/interrupt-guest! (fn [_]
                                                       false)
                              #'env/retire-python-context! (fn [session]
                                                             (swap! stopped conj session))}
               (fn []
                 (expect (false? ((deref #'loop/interrupt-block!)
                                   "healthy-session"
                                   exec-task
                                   {:python-context-retired-atom retired})))
                 (expect (false? @retired))
                 (expect (empty? @stopped))))
             (finally (deliver release-task true))))))

;; Regression: a worker rejecting its arguments exited immediately, but startup
;; still waited sixty seconds for a connection that could never arrive.
(defdescribe worker-startup-exit-test
             (it "reports an exited worker without waiting for the connection deadline"
                 (let [process (.start (ProcessBuilder. ^java.util.List ["sh" "-c" "exit 2"]))]
                   (.waitFor process)
                   (with-redefs-fn {#'com.blockether.vis.internal.python.runtime/ensure-library!
                                    (constantly nil)
                                    #'com.blockether.vis.internal.python.worker/child-argv
                                    (constantly ["unused"])
                                    #'com.blockether.vis.internal.sandbox.jail/spawn! (fn [& _]
                                                                                        process)}
                     (fn []
                       (let [task (future (try (#'com.blockether.vis.internal.python.worker/start!
                                                worker/shared-key)
                                               (catch clojure.lang.ExceptionInfo e (ex-data e))))]
                         (try (let [result (deref task 2000 ::timeout)]
                                (expect (not= ::timeout result))
                                (expect (= :vis/python-worker (:type result))))
                              (finally (future-cancel task)))))))))
