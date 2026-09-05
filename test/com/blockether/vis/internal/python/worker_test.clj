(ns com.blockether.vis.internal.python.worker-test
  "The session-worker process boundary: control messages are bounded and a
   retired interpreter can never be entered again."
  (:require [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.loop :as loop]
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
        (env/create-python-context {}
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
  (it
    "imports host modules and preserves globals between blocks in the runtime worker"
    (with-worker-context
      (fn [session]
        (expect
          (=
            "41\n"
            (:stdout
              (env/run-python-block
                session
                "import vis_introspection, vis_autoinstall\nworker_value = 41\nprint(worker_value)"))))
        (expect (= "42\n" (:stdout (env/run-python-block session "print(worker_value + 1)"))))))))

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

(defdescribe worker-entrypoint-test
             (it "launches the runtime Java worker with the control socket and host modules"
                 (with-redefs [com.blockether.vis.internal.util/native-image? (constantly false)]
                   (let [argv (#'worker/child-argv nil "/tmp/control.sock" "/tmp/host-modules")]
                     (expect (= ["com.blockether.vispython.Worker" "/tmp/control.sock"
                                 "/tmp/host-modules"]
                                (vec (take-last 3 argv)))))))
             (it "launches the runtime executable in native Vis"
                 (with-redefs [com.blockether.vis.internal.util/native-image?
                               (constantly true)

                               com.blockether.vis-python-runtime/resolve-worker
                               (fn [library]
                                 (expect (= {:path "/runtime/libvispython.so"} library))
                                 "/runtime/vis-python-worker")]

                   (expect (= ["/runtime/vis-python-worker" "/tmp/control.sock" "/tmp/host-modules"]
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

(defdescribe
  worker-control-plane-test
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
  (it "fails a host call in flight so an interrupted guest parked in it unwinds"
      (with-worker-context
        (fn [session]
          (let [dispatch
                (deref #'python-host/dispatch)

                released
                (promise)]

            (with-redefs-fn {#'python-host/dispatch (fn [caller tool payload]
                                                      (if (= "slow" tool)
                                                        (do (deref released 30000 nil) "\"late\"")
                                                        (dispatch caller tool payload)))}
              (fn []
                (let [block (future
                              (env/run-python-block
                                session
                                "import vis_runtime\nprint(vis_runtime.host_call('slow', '{}'))"))]
                  (try (Thread/sleep 500)
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
    "kills the real session process when native sleep does not unwind"
    (let [entered
          (promise)

          made
          (env/create-python-context {(symbol "entered") (fn []
                                                           (deliver entered true))}
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

          execution
          (future (try (env/run-python-block session "entered()
import time
time.sleep(30)")
                       (catch Throwable _ :stopped)))]

      (try (expect (true? (deref entered 5000 false)))
           (expect (true? ((deref #'loop/interrupt-block!)
                            session
                            execution
                            {:python-context-retired-atom retired})))
           (expect (true? (.waitFor process 10 java.util.concurrent.TimeUnit/SECONDS)))
           (expect (true? @retired))
           (expect (false? (worker/worker-live? session)))
           (expect (false? (.isAlive process)))
           (finally (try (env/dispose-python-context! session) (catch Throwable _ nil))))))
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
