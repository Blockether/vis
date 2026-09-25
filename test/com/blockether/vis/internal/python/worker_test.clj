(ns com.blockether.vis.internal.python.worker-test
  "The session-worker process boundary: control messages are bounded and a
   retired interpreter can never be entered again."
  (:require [charred.api :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.loop :as loop]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.python-exec :as python-exec]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.python.host :as python-host]
            [com.blockether.vis.internal.python.worker :as worker]
            [com.blockether.vis.internal.python.worker-peer :as worker-peer]
            [com.blockether.vis.internal.python.runtime]
            [com.blockether.vis.internal.sandbox.jail]
            [com.blockether.vis.internal.util]
            [com.blockether.vis-python-runtime]
            [lazytest.core :refer [defdescribe expect it]]
            [taoensso.telemere :as tel])
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
  worker-runtime-source-selection-test
  ;; #194: JSON-escaped forward slashes are not Python path literals.
  (it "selects the host runtime roots as real import paths before installing the worker"
      (with-worker-context
        (fn [session]
          (let [result (worker/eval-str
                         session
                         com.blockether.vis-python-runtime/default-session
                         (str "sys.path[:len(__vis_runtime_roots__)] == __vis_runtime_roots__ == "
                              (env/py-json-literal (vec
                                                     (com.blockether.vispython.Sources/roots)))))]
            (expect (= "True" result) (str result)))))))

(defdescribe
  worker-runtime-source-snapshot-test
  (it
    "uses one runtime-source snapshot for the boot policy, launch and Python imports"
    (let [boot
          #'worker/boot-read-paths

          resolve-paths
          @boot

          launch-argv
          @#'worker/child-argv

          snapshots
          (atom [])

          launch-snapshots
          (atom [])]

      (with-redefs-fn {boot (fn [& args]
                              (swap! snapshots conj (nth args 2 nil))
                              (apply resolve-paths args))
                       #'worker/child-argv (fn [& args]
                                             (swap! launch-snapshots conj (nth args 4 nil))
                                             (apply launch-argv args))}
        (fn []
          (with-worker-context
            (fn [session]
              (expect (= 1 (count @snapshots)))
              (let [roots
                    (first @snapshots)

                    result
                    (worker/eval-str session
                                     com.blockether.vis-python-runtime/default-session
                                     (str "__vis_runtime_roots__ == " (env/py-json-literal roots)))]

                (expect (vector? roots))
                (expect (= [roots] @launch-snapshots))
                (expect (= "True" result) (str result))))))))))

(defdescribe
  worker-cold-source-cache-test
  (it "starts from the host runtime roots without extracting a cold worker cache"
      (let [home
            (java.nio.file.Files/createTempDirectory
              "vis-worker-cold-sources-"
              (make-array java.nio.file.attribute.FileAttribute 0))

            cache
            (.resolve home ".vis/python/sources")

            argv
            @#'worker/child-argv]

        (try (with-redefs-fn {#'worker/child-argv
                              (fn [& args]
                                (let [[command & arguments] (apply argv args)]
                                  (into [command (str "-Duser.home=" home)]
                                        (remove #(str/starts-with? % "-Duser.home=") arguments))))}
               #(with-worker-context (fn [session]
                                       (expect (= "ready\n"
                                                  (:stdout (env/run-python-block
                                                             session
                                                             "print(await worker_echo('ready'))"))))
                                       (expect (not (java.nio.file.Files/exists
                                                      cache
                                                      (make-array java.nio.file.LinkOption 0)))))))
             (finally (doseq [file (reverse (file-seq (.toFile home)))]
                        (.delete ^java.io.File file)))))))

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
                   (with-redefs [loop-env/cache
                                 (atom {})

                                 python-exec/policy-reload-epoch
                                 (atom 0)

                                 loop-env/env-max-turns-per-ctx
                                 (delay 0)]

                     (let [id
                           (java.util.UUID/randomUUID)

                           entry
                           (#'loop-env/new-cache-entry {:python-context session})

                           ^Process process
                           (:process (get @@#'worker/workers session))

                           hook
                           (get
                             @@#'extension/reload-hooks
                             :com.blockether.vis.internal.loop.environment/security-policy-reload)

                           builds
                           (atom 0)

                           fresh
                           {:marker :fresh}]

                       (swap! loop-env/cache assoc id entry)
                       (with-redefs [loop-env/open-env!
                                     (fn [_ _]
                                       (swap! builds inc)
                                       fresh)

                                     turn/turn!
                                     (fn [environment _ _]
                                       environment)]

                         (expect (.isAlive process))
                         (dotimes [_ 2]
                           (hook))
                         (expect (.waitFor process 2 java.util.concurrent.TimeUnit/SECONDS))
                         (expect (false? (worker/worker-live? session)))
                         (expect (zero? @builds))
                         (expect (identical? entry (get @loop-env/cache id)))
                         (expect (= fresh (loop/send! id "after reload")))
                         (expect (= 1 @builds))
                         (expect (identical? (:lock entry) (:lock (get @loop-env/cache id)))))))))))

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
  guest-sources-publication-test
  (it
    "never truncates published modules when two cold starts stage the same sources"
    (let [base
          (java.nio.file.Files/createTempDirectory (.toPath (io/file "target"))
                                                   "worker-source-publication-"
                                                   (make-array java.nio.file.attribute.FileAttribute
                                                               0))

          source
          "VALUE = 7\n"

          sources
          {"guest.py" source}

          entered
          (promise)

          write-first
          (promise)

          truncated
          (promise)

          release
          (promise)

          result
          (promise)

          original-spit
          spit

          first-writer
          (Thread. ^Runnable
                   (fn []
                     (try (deliver result
                                   (#'worker/materialize-guest-sources! (.toFile base) sources))
                          (catch Throwable error (deliver result error))))
                   "guest-source-publication-test")]

      (.setDaemon first-writer true)
      (try (with-redefs [spit (fn [file value & opts]
                                (when (identical? first-writer (Thread/currentThread))
                                  (deliver entered true)
                                  @write-first
                                  (original-spit file "")
                                  (deliver truncated true)
                                  @release)
                                (apply original-spit file value opts))]
             (try (.start first-writer)
                  (expect (= true (deref entered 2000 ::timeout)))
                  (let [directory (#'worker/materialize-guest-sources! (.toFile base) sources)
                        target (io/file directory "guest.py")]

                    (deliver write-first true)
                    (expect (= true (deref truncated 2000 ::timeout)))
                    (expect (= source (slurp target))))
                  (finally (deliver write-first true)
                           (deliver release true)
                           (.join first-writer 2000)
                           (expect (not (.isAlive first-writer))))))
           (expect (string? @result))
           (finally (doseq [^java.io.File file (reverse (file-seq (.toFile base)))]
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
          (.toFile (.resolve base ^String (apply str (repeat 110 "x"))))

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
             (it "does not inherit parent recording or diagnostic destinations"
                 (expect (= ["-Xmx2g" "--enable-native-access=ALL-UNNAMED"
                             "-XX:+HeapDumpOnOutOfMemoryError" "-Dvis.example=true"]
                            (vec (#'worker/worker-jvm-options
                                  ["-Xmx2g" "-XX:StartFlightRecording=filename=parent.jfr"
                                   "--enable-native-access=ALL-UNNAMED"
                                   "-XX:FlightRecorderOptions=repository=parent-recordings"
                                   "-XX:ErrorFile=parent-crash.log" "-XX:HeapDumpPath=parent.hprof"
                                   "-XX:+HeapDumpOnOutOfMemoryError" "-Dvis.example=true"])))))
             (it "launches the worker without the current JVM's recording options"
                 (with-redefs [com.blockether.vis.internal.util/native-image? (constantly false)]
                   (expect (not-any? #(re-find #"^-XX:(StartFlightRecording|FlightRecorderOptions)"
                                               %)
                                     (#'worker/child-argv
                                      nil
                                      "/tmp/control.sock"
                                      "/tmp/host-modules"
                                      "/tmp/worker-logs"
                                      ["/runtime/sources"]))))))

(defdescribe worker-entrypoint-test
             (it "uses the Java entrypoint when the selected runtime has no packaged worker"
                 (with-redefs [com.blockether.vis.internal.util/native-image?
                               (constantly false)

                               com.blockether.vis-python-runtime/resolve-worker
                               (constantly nil)]

                   (let [argv (#'worker/child-argv
                               nil
                               "/tmp/control.sock"
                               "/tmp/host-modules"
                               "/tmp/worker-logs"
                               ["/runtime/sources"])]
                     (expect (= ["com.blockether.vispython.Worker" "/tmp/control.sock"
                                 "--resolved-sources" "/runtime/sources" "/tmp/host-modules"]
                                (vec (take-last 5 argv)))))))
             (it "makes runtime sources and their extraction marker readable at worker boot"
                 (let [roots
                       (vec (com.blockether.vispython.Sources/roots))

                       paths
                       (set (#'worker/boot-read-paths
                             nil
                             "/tmp/host-modules"
                             roots
                             (com.blockether.vis-python-runtime/packages-dir)))]

                   (expect (contains? paths
                                      (.getCanonicalPath
                                        (java.io.File.
                                          (com.blockether.vispython.Locations/sourcesDir)))))
                   (doseq [path roots]
                     (expect (contains? paths (.getCanonicalPath (java.io.File. ^String path)))))))
             (it "launches the selected runtime executable from either a JVM or native host"
                 ;; JVM dogfooding: starting another JVM needlessly tripled per-worker RSS.
                 (doseq [native? [false true]]
                   (with-redefs [com.blockether.vis.internal.util/native-image? (constantly native?)
                                 com.blockether.vis-python-runtime/resolve-worker
                                 (fn [library]
                                   (expect (= {:path "/runtime/libvispython.so"} library))
                                   "/runtime/vis-python-worker")]

                     (expect (= ["/runtime/vis-python-worker"
                                 (str "-Duser.home=" (System/getProperty "user.home"))
                                 "/tmp/control.sock" "--resolved-sources" "/runtime/source modules"
                                 "/runtime/extra-sources" "/tmp/host-modules"]
                                (#'worker/child-argv
                                 "/runtime/libvispython.so"
                                 "/tmp/control.sock"
                                 "/tmp/host-modules"
                                 "/tmp/worker-logs"
                                 ["/runtime/source modules" "/runtime/extra-sources"]))))))
             (it "refuses a native runtime without its worker instead of starting Vis again"
                 (with-redefs [com.blockether.vis.internal.util/native-image?
                               (constantly true)

                               com.blockether.vis-python-runtime/resolve-worker
                               (constantly nil)]

                   (expect (= :vis/python-worker-missing
                              (try (#'worker/child-argv
                                    nil
                                    "/tmp/control.sock"
                                    "/tmp/host-modules"
                                    "/tmp/worker-logs"
                                    ["/runtime/sources"])
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
  background-wake-authorization-test
  ;; #202: connection ownership survives a trusted thread's missing activation;
  ;; it never grants an unbound sandbox thread access to host tools.
  (it "admits only Council wake from a trusted worker's assigned namespace"
      (doseq [[trusted? caller tool payload assigned? allowed?]
              [[true "" "__vis_host_council_wake__" "{\"session\":\"owned\"}" true true]
               [false "" "__vis_host_council_wake__" "{\"session\":\"owned\"}" true false]
               [true "" "__vis_host_shell__" "{\"session\":\"owned\"}" true false]
               [true "" "__vis_host_council_wake__" "{\"session\":\"other\"}" true false]
               [true "" "__vis_host_council_wake__" "{\"session\":\"owned\"}" false false]
               [true "other" "__vis_host_council_wake__" "{\"session\":\"owned\"}" true false]
               [true nil "__vis_host_council_wake__" "{\"session\":\"owned\"}" true false]
               [true "" "__vis_host_council_wake__" "invalid" true false]
               [true "" "__vis_host_council_wake__" "{}" true false]]]
        (let [peer {:serving (atom {})
                    :host-sessions (atom (if assigned? #{"owned"} #{}))
                    :trusted? trusted?}
              calls (atom [])
              replies (atom [])]

          (with-redefs [python-host/dispatch (fn [caller tool payload]
                                               (swap! calls conj [caller tool payload])
                                               "ok")
                        worker-peer/send-line! (fn [_ reply]
                                                 (swap! replies conj reply))]

            (#'worker/serve-host-call! peer {"id" 1 "session" caller "tool" tool "payload" payload})
            (expect (= allowed? (boolean (seq @calls))))
            (expect (= (if allowed?
                         {"id" 1 "value" "ok"}
                         {"id" 1 "error" "Worker is not authorized for this host session"})
                       (first @replies)))
            (expect (empty? @(:serving peer))))))))

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
                         (fn [_ message]
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
            (expect (= [["install-runtime" #{"owned"}] ["exec" #{"owned"}] ["close" #{}]
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
                        (expect (true? (#'python-exec/interrupt-block!
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

(defdescribe worker-message-framing-test
             (it "leaves the channel untouched on encoding failure and frames concurrent replies"
                 (let [out
                       (java.io.StringWriter.)

                       bad
                       (reify
                         json/PToJSON
                           (->json-data [_] (throw (ex-info "fixture conversion failed" {}))))]

                   (with-open [writer (java.io.BufferedWriter. out)]
                     (let [peer {:writer writer}
                           messages (mapv (fn [id]
                                            {"id" id "value" "Zażółć / 😀\na second line"})
                                          (range 32))]

                       (expect (= "fixture conversion failed"
                                  (try (worker-peer/send-line! peer {"before" [1 2] "bad" bad})
                                       ::no-error
                                       (catch clojure.lang.ExceptionInfo error
                                         (ex-message error)))))
                       (expect (= "" (.toString out)))
                       (let [calls (mapv (fn [message]
                                           (future (worker-peer/send-line! peer message) true))
                                         messages)]
                         (try (doseq [call calls]
                                (expect (true? (deref call 10000 false))))
                              (let [lines (str/split-lines (.toString out))]
                                (expect (= (count messages) (count lines)))
                                (expect (= (set messages) (set (map json/read-json lines)))))
                              (finally (run! future-cancel calls)))))))))

(defdescribe worker-message-allocation-test
             (it "does not allocate a file-sized buffer for every small IPC message"
                 ;; JVM SDK profiling attributed 125 MiB to send-line! for twenty tasks.
                 ;; Measure allocations, not elapsed time: JIT/scheduler speed is not the contract.
                 (let [^com.sun.management.ThreadMXBean bean
                       (java.lang.management.ManagementFactory/getThreadMXBean)

                       tid
                       (.threadId (Thread/currentThread))

                       message
                       {"op" "eval" "id" 1 "code" "print(42)"}]

                   (expect (.isThreadAllocatedMemorySupported bean))
                   (let [enabled? (.isThreadAllocatedMemoryEnabled bean)]
                     (try (.setThreadAllocatedMemoryEnabled bean true)
                          (with-open [writer (java.io.BufferedWriter. (java.io.Writer/nullWriter))]
                            (let [peer {:writer writer}]
                              (dotimes [_ 2000]
                                (worker-peer/send-line! peer message))
                              (let [before (.getThreadAllocatedBytes bean tid)]
                                (dotimes [_ 1000]
                                  (worker-peer/send-line! peer message))
                                (expect (< (- (.getThreadAllocatedBytes bean tid) before)
                                           (* 1000 4096))))))
                          (finally (.setThreadAllocatedMemoryEnabled bean enabled?)))))))

(defdescribe
  worker-reply-lifetime-test
  (it
    "keeps a cancelled caller's completion until the actual reply or peer close"
    (doseq [reply-line ["{\"id\":1,\"value\":42}\n" ""]]
      (let [sent (promise)
            finished (promise)
            peer {:pending (atom {})
                  :rpc-state (atom {})
                  :seq (AtomicLong. 0)
                  :reader (java.io.BufferedReader. (java.io.StringReader. reply-line))
                  :workers (java.util.concurrent.Executors/newSingleThreadExecutor)}]

        (try (with-redefs [worker-peer/send-line! (fn [_ _]
                                                    (deliver sent true))]
               (let [call (future (try (worker-peer/request! peer {"op" "run"})
                                       (finally (deliver finished true))))]
                 (try (expect (true? (deref sent 1000 false)))
                      (expect (= "run" (get-in @(:rpc-state peer) [:outbound :active 1 :op])))
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
                        (expect (empty? @(:pending peer)))
                        (expect (empty? (get-in @(:rpc-state peer) [:outbound :active])))
                        (expect (= (if (empty? reply-line) :closed :ok)
                                   (get-in @(:rpc-state peer)
                                           [:outbound :last-completed :status]))))
                      (finally (future-cancel call)))))
             (finally (.close ^java.io.BufferedReader (:reader peer))
                      (.shutdownNow ^java.util.concurrent.ExecutorService (:workers peer))))))))

(defn- expect-stalled-interrupt-timeout
  [blocked-op]
  (let [key
        (str (java.util.UUID/randomUUID))

        entered
        (promise)

        released
        (promise)

        exited
        (promise)

        peer
        {:pending (atom {})
         :rpc-state (atom {})
         :serving (atom (if blocked-op {} {"host-call" (Thread.)}))
         :seq (AtomicLong. 0)}]

    (swap! @#'worker/workers assoc key {:peer peer})
    (try (with-redefs-fn {#'worker/alive? (fn [state]
                                            (= peer (:peer state)))
                          #'worker/INTERRUPT_REPLY_MS 100
                          #'worker-peer/send-line!
                          (fn [_ message]
                            (when (= blocked-op (get message "op"))
                              (deliver entered true)
                              (try @released (finally (deliver exited true))))
                            (when (= "interrupt" (get message "op"))
                              (deliver (get @(:pending peer) (get message "id")) {"value" true})))}
           (fn []
             (let [call (future (try (worker/interrupt! key "sandbox")
                                     (catch clojure.lang.ExceptionInfo e (ex-data e))))]
               (try (expect (true? (deref entered 1000 false)))
                    ;; A reply-only timeout cannot help while either socket write stalls.
                    (expect (= {:type :vis/python-worker-timeout :op "interrupt" :timeout-ms 100}
                               (deref call 1500 ::blocked)))
                    (expect (true? (deref exited 1000 false)))
                    (finally
                      ;; Release the pre-fix implementation too: failed tests must not leak tasks.
                      (deliver released true)
                      (deref call 1000 nil)
                      (future-cancel call))))))
         (finally (swap! @#'worker/workers dissoc key)))))

(defdescribe
  worker-interrupt-deadline-test
  (it "bounds a stalled interrupt request write" (expect-stalled-interrupt-timeout "interrupt"))
  (it "bounds a stalled host-call failure write after the interrupt reply"
      (expect-stalled-interrupt-timeout nil))
  (it "uses the observed peer on a platform thread and preserves bindings and errors"
      (let [key
            (str (java.util.UUID/randomUUID))

            peer
            {:serving (atom {})}

            failure
            (ex-info "control failed" {:type :vis/python-worker})

            observed
            (atom nil)]

        (swap! @#'worker/workers assoc key {:peer peer})
        (try (with-redefs-fn {#'worker/alive? (constantly true)
                              #'worker/live (fn [_]
                                              (throw (IllegalStateException.
                                                       "must not start a worker")))
                              #'worker-peer/request!
                              (fn [actual-peer message]
                                (reset! observed [actual-peer message *print-length*
                                                  (.isVirtual (Thread/currentThread))])
                                (throw failure))}
               (fn []
                 (binding [*print-length* 7]
                   (expect (identical? failure
                                       (try (worker/interrupt! key "sandbox")
                                            (catch clojure.lang.ExceptionInfo error error)))))
                 (expect (= [peer {"op" "interrupt" "session" "sandbox"} 7 false] @observed))))
             (finally (swap! @#'worker/workers dissoc key)))))
  (it
    "cancels control work when the cancellation caller is interrupted"
    (let [key
          (str (java.util.UUID/randomUUID))

          peer
          {:serving (atom {})}

          caller
          (promise)

          entered
          (promise)

          released
          (promise)

          exited
          (promise)]

      (swap! @#'worker/workers assoc key {:peer peer})
      (try
        (with-redefs-fn {#'worker/alive? (constantly true)
                         #'worker/INTERRUPT_REPLY_MS 10000
                         #'worker-peer/request! (fn [_ _]
                                                  (deliver entered true)
                                                  (try @released (finally (deliver exited true))))}
          (fn []
            (let [call (future (deliver caller (Thread/currentThread))
                               (try (worker/interrupt! key "sandbox")
                                    (catch InterruptedException _ ::interrupted)))]
              (try (expect (true? (deref entered 1000 false)))
                   (.interrupt ^Thread @caller)
                   (expect (= ::interrupted (deref call 1000 ::blocked)))
                   (expect (true? (deref exited 1000 false)))
                   (finally (deliver released true) (deref call 1000 nil) (future-cancel call))))))
        (finally (swap! @#'worker/workers dissoc key))))))

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
            {:pending pending :rpc-state (atom {}) :serving (atom {}) :seq (AtomicLong. 0)}]

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
                 (expect (false? ((deref #'python-exec/interrupt-block!)
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

        (try (with-redefs-fn {#'python-exec/INTERRUPT_UNWIND_MS 25
                              #'env/interrupt-guest! (fn [_]
                                                       true)
                              #'env/retire-python-context! (fn [session]
                                                             (deliver stopped session))}
               (fn []
                 (expect (true? ((deref #'python-exec/interrupt-block!)
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
          #(expect (= [self 424242] (vec ((deref #'loop-env/runtime-pids))))))))
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
           (expect (true? ((deref #'python-exec/interrupt-block!)
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

        (with-redefs-fn {#'loop-env/cache (atom {key entry})
                         #'env/retire-python-context! (fn [session]
                                                        (swap! stopped conj session))}
          (fn []
            (expect (true? ((deref #'loop-env/detach-entry!) key entry)))
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
                 (expect (false? ((deref #'python-exec/interrupt-block!)
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
                 (let [^java.util.List command
                       ["sh" "-c" "exit 2"]

                       process
                       (.start (ProcessBuilder. command))]

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

;; A blocked startup held the process-wide monitor before the first provider call,
;; stalling unrelated sessions in :engine-start and ignoring their cancellation.
(defdescribe
  worker-startup-isolation-test
  (it
    "keeps ready and new workers independent while same-key waiters can cancel"
    (let [entered
          (promise)

          release
          (promise)

          threads
          (atom [])

          starts
          (atom {})

          state
          {:peer {:host-sessions (atom #{})}}

          call
          (fn [k]
            (let [result
                  (promise)

                  thread
                  (Thread. ^Runnable
                           (fn []
                             (try (deliver result (worker/exec! k k "pass"))
                                  (catch InterruptedException _ (deliver result ::interrupted))
                                  (catch Throwable error (deliver result error))))
                           "worker-startup-test")]

              (.setDaemon thread true)
              (swap! threads conj thread)
              (.start thread)
              [thread result]))]

      (with-redefs-fn {#'worker/workers (atom {"ready" state})
                       #'worker/retired-workers (atom {})
                       #'worker/worker-locks (atom {})
                       #'worker/alive? boolean
                       #'worker/start! (fn [k]
                                         (swap! starts update k (fnil inc 0))
                                         (when (= "blocked" k) (deliver entered true) @release)
                                         state)
                       #'worker-peer/request! (fn [& _]
                                                ::ok)}
        (fn []
          (try (let [[_ started] (call "blocked")]
                 (expect (= true (deref entered 2000 ::timeout)))
                 (let [[_ ready] (call "ready")
                       [_ cold] (call "cold")
                       [^Thread waiter waiting] (call "blocked")
                       [^Thread follower following] (call "blocked")
                       ^java.util.concurrent.locks.ReentrantLock gate
                       (:lock (get @(var-get #'worker/worker-locks) "blocked"))]

                   (expect (= ::ok (deref ready 1000 ::timeout)))
                   (expect (= ::ok (deref cold 1000 ::timeout)))
                   (expect (loop [remaining 200]
                             (cond (and (.hasQueuedThread gate waiter)
                                        (.hasQueuedThread gate follower))
                                   true
                                   (zero? remaining) false
                                   :else (do (Thread/sleep 5) (recur (dec remaining))))))
                   (.interrupt waiter)
                   (expect (= ::interrupted (deref waiting 1000 ::timeout)))
                   (expect (not (realized? started)))
                   (expect (not (realized? following)))
                   (deliver release true)
                   (expect (= ::ok (deref started 1000 ::timeout)))
                   (expect (= ::ok (deref following 1000 ::timeout)))
                   (expect (= {"blocked" 1 "cold" 1} @starts))))
               (finally (deliver release true)
                        (doseq [^Thread thread @threads]
                          (.join thread 2000)
                          (expect (not (.isAlive thread))))))
          (expect (empty? @(var-get #'worker/worker-locks))))))))

(defdescribe
  worker-lifecycle-lock-test
  (it "releases the per-key lock after a startup failure"
      (with-redefs-fn {#'worker/workers (atom {})
                       #'worker/retired-workers (atom {})
                       #'worker/worker-locks (atom {})
                       #'worker/start! (fn [_]
                                         (throw (ex-info "Startup failed" {:type ::failed})))}
        (fn []
          (expect (= ::failed
                     (try (worker/exec! "failed" "failed" "pass")
                          (catch clojure.lang.ExceptionInfo error (:type (ex-data error))))))
          (expect (empty? @(var-get #'worker/workers)))
          (expect (empty? @(var-get #'worker/worker-locks))))))
  (it "still stops a live worker when teardown's thread was interrupted"
      (with-worker-context
        (fn [session]
          (let [result
                (promise)

                thread
                (Thread. ^Runnable
                         (fn []
                           (.interrupt (Thread/currentThread))
                           (try (worker/stop-worker! session)
                                (deliver result ::stopped)
                                (catch Throwable error (deliver result error))
                                (finally (Thread/interrupted))))
                         "worker-stop-test")]

            (.setDaemon thread true)
            (.start thread)
            (try (expect (= ::stopped (deref result 2000 ::timeout)))
                 (expect (not (worker/worker-live? session)))
                 (expect (not (contains? @(var-get #'worker/worker-locks) session)))
                 (finally (.join thread 2000) (expect (not (.isAlive thread))))))))))

(defdescribe
  worker-rpc-evidence-test
  (it
    "records both sides of a live host callback without code, arguments or results"
    (with-worker-context
      (fn [session]
        (let [peer
              (:peer (get @@#'worker/workers session))

              dispatch
              python-host/dispatch

              entered
              (promise)

              release
              (promise)]

          (with-redefs [python-host/dispatch (fn [& args]
                                               (deliver entered true)
                                               @release
                                               (apply dispatch args))]
            (let [call (future (env/run-python-block
                                 session
                                 "print(await worker_echo('rpc-payload-fixture'))"))]
              (try (expect (true? (deref entered 2000 false)))
                   (let [snapshot (worker-peer/rpc-snapshot peer)]
                     (expect (= "run-block" (:op (first (get-in snapshot [:outbound :active])))))
                     (expect (= "host" (:op (first (get-in snapshot [:inbound :active])))))
                     (expect (seq (:stack (first (get-in snapshot [:inbound :active])))))
                     (expect (not (str/includes? (pr-str snapshot) "rpc-payload-fixture"))))
                   (deliver release true)
                   (expect (= "rpc-payload-fixture\n" (:stdout (deref call 2000 {}))))
                   (let [snapshot (worker-peer/rpc-snapshot peer)]
                     (expect (empty? (get-in snapshot [:outbound :active])))
                     (expect (not (str/includes? (pr-str snapshot) "rpc-payload-fixture"))))
                   (finally (deliver release true) (future-cancel call)))))))))
  (it "keeps only the last completion and caps active snapshots"
      (let [peer {:rpc-state (atom {})}]
        (dotimes [id 100]
          (#'worker-peer/begin-rpc! peer :outbound {"id" id "op" "eval" "code" "private"}))
        (let [snapshot (worker-peer/rpc-snapshot peer)]
          (expect (= 100 (get-in snapshot [:outbound :active-count])))
          (expect (= 64 (count (get-in snapshot [:outbound :active]))))
          (expect (not (str/includes? (pr-str snapshot) "private"))))
        (dotimes [id 100]
          (#'worker-peer/finish-rpc! peer :outbound id :ok))
        (let [snapshot (worker-peer/rpc-snapshot peer)]
          (expect (zero? (get-in snapshot [:outbound :active-count])))
          (expect (= 99 (get-in snapshot [:outbound :last-completed :id])))
          (expect (not (contains? (get-in snapshot [:outbound :last-completed]) :thread)))))))

(defdescribe
  worker-hang-evidence-test
  (it
    "writes private local evidence before stopping, and nothing for a healthy probe"
    (with-worker-context
      (fn [session]
        (let [state
              (get @@#'worker/workers session)

              file
              (io/file (.getParentFile ^java.io.File (:log state)) "hang.edn")

              python-file
              (io/file (.getParentFile ^java.io.File (:log state)) "python-stacks.log")

              stop
              worker/stop-worker!]

          (expect (worker/worker-ready? session session))
          (expect (not (.exists file)))
          (expect (zero? (.length python-file)))
          (with-redefs [worker/stop-worker! (fn [key]
                                              (expect (.isAlive ^Process (:process state)))
                                              (expect (.isFile file))
                                              (expect (pos? (.length python-file)))
                                              (stop key))]
            (worker/retire-worker! session "diagnostic fixture"))
          (let [report (edn/read-string (slurp file))]
            (expect (= session (:worker report)))
            (expect (= "eval" (get-in report [:rpc :outbound :last-completed :op])))
            (expect (seq (:jvm-threads report)))
            (expect (= {:status :written :path (.getAbsolutePath python-file)}
                       (:python-stacks report)))
            (expect (= (java.nio.file.attribute.PosixFilePermissions/fromString "rw-------")
                       (java.nio.file.Files/getPosixFilePermissions
                         (.toPath python-file)
                         (make-array java.nio.file.LinkOption 0))))
            (expect (= (java.nio.file.attribute.PosixFilePermissions/fromString "rw-------")
                       (java.nio.file.Files/getPosixFilePermissions
                         (.toPath file)
                         (make-array java.nio.file.LinkOption 0)))))
          (let [before (slurp file)]
            (worker/retire-worker! session "already retired")
            (expect (= before (slurp file))))))))
  (it "still retires after a failed capture without logging exception payloads"
      (with-worker-context
        (fn [session]
          (let [{:keys [signals]} (tel/with-signals
                                    (with-redefs-fn {#'worker/write-hang-diagnostic!
                                                     (fn [& _]
                                                       (throw (java.io.IOException.
                                                                "private-error-fixture")))}
                                      #(worker/retire-worker! session "capture failure")))]
            (expect (not (worker/worker-live? session)))
            (expect (= [:failed]
                       (mapv #(get-in % [:data :status])
                             (filter #(= ::worker/hang-diagnostic (:id %)) signals))))
            (expect (not (str/includes? (pr-str signals) "private-error-fixture")))))))
  (it "bounds slow collection, cancels its helper and then retires"
      (with-worker-context
        (fn [session]
          (let [entered
                (promise)

                exited
                (promise)

                release
                (promise)

                started
                (System/nanoTime)]

            (try (with-redefs-fn {#'worker/HANG_DIAGNOSTIC_MS 50
                                  #'worker/write-hang-diagnostic!
                                  (fn [& _]
                                    (deliver entered true)
                                    (try @release (finally (deliver exited true))))}
                   #(worker/retire-worker! session "slow capture"))
                 (expect (true? (deref entered 1000 false)))
                 (expect (true? (deref exited 1000 false)))
                 (expect (< (- (System/nanoTime) started) 1000000000))
                 (expect (not (worker/worker-live? session)))
                 (finally (deliver release true))))))))

(defdescribe
  worker-stack-control-test
  (it "keeps only known statuses, never a control reply or exception payload"
      (doseq [status ["ready" "written" "unavailable" "busy" "failed"]]
        (with-redefs [worker-peer/request! (fn [_ _]
                                             {"status" status "detail" "private-control-fixture"})]
          (expect (= {:status (keyword status)}
                     (#'worker/stack-control! {} {"op" "dump-stacks"})))))
      (with-redefs [worker-peer/request! (fn [_ _]
                                           {"status" "private-control-fixture"})]
        (expect (= {:status :failed :reason :invalid-reply}
                   (#'worker/stack-control! {} {"op" "dump-stacks"})))))
  (it "bounds the whole exchange even when its socket write stalls"
      (let [entered
            (promise)

            exited
            (promise)

            release
            (promise)

            peer
            {:pending (atom {}) :rpc-state (atom {}) :seq (AtomicLong. 0)}]

        (try (with-redefs-fn {#'worker/STACK_CONTROL_MS 50
                              #'worker-peer/send-line!
                              (fn [_ _]
                                (deliver entered (not (.isVirtual (Thread/currentThread))))
                                (try @release (finally (deliver exited true))))}
               #(expect (= {:status :timed-out}
                           (#'worker/stack-control! peer {"op" "dump-stacks"}))))
             (expect (true? (deref entered 1000 false)))
             (expect (true? (deref exited 1000 false)))
             (finally (deliver release true)))))
  (it "does not request a dump without this worker's successful setup"
      (with-redefs [worker-peer/request! (fn [& _]
                                           (throw (AssertionError. "unexpected dump request")))]
        (doseq [status [:failed :unavailable :timed-out]]
          (expect (= {:status status}
                     (#'worker/python-stack-diagnostic! {:python-stacks {:status status}}))))))
  (it "preserves RPC and JVM evidence and retires after a failed or stalled Python dump"
      (doseq [mode [:failed :timed-out]]
        (with-worker-context
          (fn [session]
            (let [state (get @@#'worker/workers session)
                  file (io/file (.getParentFile ^java.io.File (:log state)) "hang.edn")
                  release (promise)]

              (expect (= "True" (worker/eval-str session session "True")))
              (swap! @#'worker/workers assoc-in [session :python-stacks] {:status :ready})
              (try (with-redefs-fn {#'worker/STACK_CONTROL_MS 50
                                    #'worker-peer/request!
                                    (fn [_ message]
                                      (expect (= "dump-stacks" (get message "op")))
                                      (if (= :failed mode)
                                        (throw (java.io.IOException. "private-control-fixture"))
                                        @release))}
                     #(worker/retire-worker! session "stack control fixture"))
                   (expect (not (worker/worker-live? session)))
                   (let [report (edn/read-string (slurp file))]
                     (expect (= mode (get-in report [:python-stacks :status])))
                     (expect (seq (:jvm-threads report)))
                     (expect (= "eval" (get-in report [:rpc :outbound :last-completed :op])))
                     (expect (not (str/includes? (pr-str report) "private-control-fixture"))))
                   (finally (deliver release true)))))))))
