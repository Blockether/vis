(ns com.blockether.vis.internal.python.worker
  "Process boundaries for model Python and trusted Python extensions.

   Each session's sandbox owns a confined runtime worker. Its trusted extension
   namespaces run in a separate worker, preserving their host APIs and native
   library support without sharing interpreter memory or host-call authority
   with model code. Registration outside a session uses the shared trusted worker.

   Confinement, imports, native libraries and interpreter state are process-wide.
   A per-session extension worker keeps that state separate from other sessions
   as well as from the sandbox. Only the host chooses a worker's role.

   The wire is ONE line of JSON per message over a unix socket, both ways. The
   parent asks (`install-runtime`, `install-tool`, `exec`, `run`, `run-block`,
   `eval`, `confine`, `network`, `stdin`, `interrupt`, `close`); the child asks
   back with `host`, because the registry that knows what a name may call, the
   persistence handle and the caller's dynamic binding frame all live in the
   parent (`python-host/dispatch`). stdout is NOT the wire: Python that prints,
   or a native library writing to fd 1, would corrupt it, so a child's own stdio
   goes to a log file instead.

   A message carrying `op` is a request, one without is its reply, so each side
   numbers its own requests and no id can collide. Work has no timeout: a block
   or extension tool may legitimately run for minutes. CONTROL is different:
   an interrupt that cannot reach the child is bounded, because cancellation
   must be able to retire that process instead of parking its caller forever.
   A child that DIES is what the pump reports — every call waiting on it fails
   at once with the child's log to read.

   An interrupt reaches BOTH ends of a host call. The child's async exception
   lands only when the guest next runs Python, and a guest parked in `host`
   (a shell handle's `wait`, a long tool) runs none until the parent answers —
   so `interrupt!` also fails every host call the worker has in flight, and the
   tool thread serving it is interrupted. Measured before that: the unwind
   watch expired, the worker was killed, and the next block silently started a
   FRESH interpreter that had the runtime but none of the session's tools. A
   retired key now refuses to restart until the session is rebuilt."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.python.host :as python-host]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.worker-peer :as child]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis-python-runtime :as runtime]
            [taoensso.telemere :as tel])
  (:import (com.blockether.vispython Locations Sources)
           (java.io File)
           (java.lang.management ManagementFactory)
           (java.net StandardProtocolFamily UnixDomainSocketAddress)
           (java.nio.channels SelectionKey Selector ServerSocketChannel SocketChannel)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util.concurrent TimeUnit)))

(set! *warn-on-reflection* true)

(defn- materialize-guest-sources!
  "Stage modules under their content identity so engine versions cannot overwrite each other."
  [root sources]
  (let [dir (io/file root (util/sha256-hex (pr-str (into (sorted-map) sources))))]
    (.mkdirs dir)
    (doseq [[name source] sources]
      (let [target (io/file dir name)]
        (when-not (and (.isFile target) (= source (slurp target))) (spit target source))))
    (.getCanonicalPath dir)))

(defonce ^:private guest-sources
  (delay (into {}
               (map (fn [name]
                      (let [resource (or (io/resource (str "vis-guest/" name))
                                         (throw (ex-info (str "Missing Vis guest module " name)
                                                         {:module name})))]
                        [name (slurp resource)])))
               ["vis_introspection.py" "vis_results.py"])))

(defn guest-source-dir
  "Stage Vis-owned Python guest modules in the current home, restoring missing files."
  []
  (materialize-guest-sources! (io/file (System/getProperty "user.home") ".vis" "python" "vis-guest")
                              @guest-sources))

(defn- serve-host-call!
  "Serve only identities the host assigned to this connection. Worker claims do
   not authorize access to another worker's host bindings. This does not separate
   trust levels within one worker. Interrupts retain ownership of reply delivery."
  [peer message]
  (let [id
        (get message "id")

        caller
        (get message "session")]

    (swap! (:serving peer) assoc id (Thread/currentThread))
    (let [reply (if (and (string? caller) (contains? @(:host-sessions peer) caller))
                  {"value"
                   (python-host/dispatch caller (get message "tool") (get message "payload"))}
                  {"error" "Worker is not authorized for this host session"})]
      (when (child/claim-reply! peer id) (child/send-line! peer (assoc reply "id" id))))))

(defn- fail-host-calls!
  "Fail every host call the child has in flight on `peer`: each guest thread
   parked in one unwinds with the error, and the tool thread serving it is
   interrupted so a bounded wait stops early instead of running to its deadline.
   Process-wide on purpose, like the interpreter interrupt it accompanies: one
   worker is one session's Python, so every call it has open belongs to the
   block being cancelled or to a tool that block is waiting on."
  [peer reason]
  (let [[before _] (swap-vals! (:serving peer) empty)]
    (doseq [[id ^Thread thread] before]
      (.interrupt thread)
      (child/send-line! peer {"id" id "error" (str reason)}))
    (count before)))

;; The parent half

(defonce
  ^:private
  ^{:doc
    "worker key -> {:process :peer :log}. One entry per live worker: a
          SESSION's key gives that session its own interpreter, and the shared
          key is what everything not owned by a session runs in."}
  workers
  (atom {}))

(defonce
  ^:private
  ^{:doc
    "worker key -> 0-arg session policy fn. A session worker is refused until
          its launch policy exists; the shared trusted worker is deliberately absent."}
  worker-policy-fns
  (atom {}))

(defonce
  ^:private
  ^{:doc
    "worker key -> why its worker was retired. A retired key never restarts on
          its own: the fresh interpreter would carry the runtime but none of the
          session's tools, and the block that reached it would fail one call at a
          time. The mark lifts when the session is rebuilt (`configure!`) or
          disposed (`forget-policy!`)."}
  retired-workers
  (atom {}))

(def shared-key
  "The worker for Python that belongs to no single session: extension files
   loading at startup, whose REGISTRATION is the gateway's and not a session's."
  "shared")

(defrecord ^:private TrustedExtensionWorker [session])

(defn extension-worker-key
  "Host-owned worker identity for a session's trusted extensions, not its sandbox.
   The key is never accepted from Python or reconstructed from a wire payload."
  [session]
  (->TrustedExtensionWorker session))

(defn- trusted-worker? [k] (or (= k shared-key) (instance? TrustedExtensionWorker k)))

(defn- worker-jvm-options
  "Preserve runtime flags, but keep the parent's JFR recording process-local.
   Its destination and repository are neither the worker's nor writable in its jail."
  [options]
  (remove #(or (str/starts-with? % "-XX:StartFlightRecording")
               (str/starts-with? % "-XX:FlightRecorderOptions"))
    options))

(defn- child-argv
  "Start the runtime worker, never a second copy of Vis. JVM development uses
   the same Java entrypoint; native Vis requires the packaged runtime executable."
  [library socket guest-dir]
  (if (util/native-image?)
    (if-let [executable (runtime/resolve-worker {:path library})]
      [executable (str "-Duser.home=" (System/getProperty "user.home")) socket guest-dir]
      (throw (ex-info "The Python runtime archive has no worker executable"
                      {:type :vis/python-worker-missing})))
    (vec (concat [(str (System/getProperty "java.home") File/separator "bin" File/separator "java")]
                 (worker-jvm-options (.getInputArguments (ManagementFactory/getRuntimeMXBean)))
                 ["-cp" (System/getProperty "java.class.path") "com.blockether.vispython.Worker"
                  socket guest-dir]))))

(defn- worker-dir
  ^File [stamp]
  (doto (io/file (System/getProperty "user.home") ".vis" "run" (str "pyext-" stamp)) (.mkdirs)))

(defn configure!
  "Register the live session policy used whenever worker `k` starts or restarts.
   Must happen before the first interpreter request; a missing policy fails closed.
   A session being (re)built under a key that was retired is that key's new life."
  [k policy-fn]
  (when (or (trusted-worker? k) (not (ifn? policy-fn)))
    (throw (ex-info "A session Python worker requires a launch policy"
                    {:type ::worker-policy-missing :worker k})))
  (swap! worker-policy-fns assoc k policy-fn)
  (swap! retired-workers dissoc k)
  k)

(defn- launch-policy!
  [k run-directory control-socket boot-read-paths]
  (when-not (trusted-worker? k)
    (let [policy-fn
          (get @worker-policy-fns k)

          policy
          (when policy-fn (policy-fn))]

      (when-not policy
        (throw (ex-info "A session Python worker has no live launch policy"
                        {:type ::worker-policy-missing :worker k})))
      (process-jail/python-worker-policy policy run-directory control-socket boot-read-paths))))

(defn- boot-read-paths
  "Existing working directory, JVM classpath, Java home, package cache, runtime
   tree and Vis guest modules needed before the child can connect. They are
   read-only, not session roots."
  [library guest-dir]
  (->> (concat [(System/getProperty "user.dir") (System/getProperty "java.home")
                (runtime/packages-dir) (Locations/sourcesDir) guest-dir]
               ;; Extract before confinement; workers only read the versioned cache.
               (Sources/roots)
               (str/split (System/getProperty "java.class.path" "")
                          (re-pattern (java.util.regex.Pattern/quote File/pathSeparator)))
               (when library
                 [(some-> library
                          io/file
                          .getParentFile
                          .getParentFile
                          .getAbsolutePath)]))
       (remove str/blank?)
       (map #(.getCanonicalPath (io/file %)))
       distinct
       vec))

(defn- drain-output!
  "Drain the child process stream to `log`; a worker never gets to block on logs."
  [^Process process ^File log]
  (let [thread (Thread. ^Runnable
                        (fn []
                          (try (with-open [out (io/output-stream log :append true)]
                                 (io/copy (.getInputStream process) out))
                               (catch Throwable _ nil)))
                        "vis-python-worker-log")]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defn- await-worker-connection
  "Wait only while the child can still connect. No blocked accept future survives
   an early child exit or a startup timeout. The selector bounds exit detection."
  [^ServerSocketChannel server ^Process process]
  (.configureBlocking server false)
  (with-open [selector (Selector/open)]
    (.register server selector SelectionKey/OP_ACCEPT)
    (let [deadline (+ (long (util/now-ms)) 60000)]
      (loop []

        (when (.isInterrupted (Thread/currentThread))
          (throw (InterruptedException. "Python worker startup interrupted")))
        (or (.accept server)
            (when (and (.isAlive process) (< (long (util/now-ms)) deadline))
              (.select selector 100)
              (.clear (.selectedKeys selector))
              (recur)))))))

(defn- tls-strict?
  "Read the merged worker policy; invalid lenient YAML must not weaken TLS."
  []
  (let [strict (get-in (config/load-config-raw) ["python" "tls_strict"] true)]
    (when-not (boolean? strict)
      (throw (ex-info "python.tls_strict must be a boolean" {:type ::invalid-tls-strict})))
    strict))

(defn- start!
  "Start `k` behind its live session policy and answer it connected. The parent
   binds first; the run directory is the worker's only host-owned writable grant."
  [k]
  (let [stamp
        (str (discovery/current-pid)
             "-" (util/now-ms)
             "-" (subs (str (java.util.UUID/randomUUID)) 0 8))

        dir
        (worker-dir stamp)

        ;; Keep logs under HOME, but not the AF_UNIX endpoint: home paths can
        ;; exceed the portable 104-byte limit. A private random directory prevents
        ;; another user connecting before our child; remove it after the handshake.
        control-dir
        (Files/createTempDirectory (.toPath (io/file "/tmp"))
                                   "vis-py-"
                                   (into-array FileAttribute
                                               [(PosixFilePermissions/asFileAttribute
                                                  (PosixFilePermissions/fromString "rwx------"))]))

        socket
        (.toFile (.resolve control-dir "control.sock"))

        log
        (io/file dir "worker.log")

        address
        (UnixDomainSocketAddress/of (.toPath socket))]

    (with-open [server (ServerSocketChannel/open StandardProtocolFamily/UNIX)]
      (try
        (.bind server address)
        (let [library (try (python-runtime/ensure-library!)
                           (catch Throwable t
                             (tel/log! {:level :debug
                                        :id ::no-library-to-hand-over
                                        :data {:error (ex-message t)}})
                             nil))
              packages (some-> (runtime/packages-dir)
                               io/file)
              _ (when packages
                  (Files/createDirectories (.toPath packages)
                                           (make-array java.nio.file.attribute.FileAttribute 0)))
              guest-dir (guest-source-dir)
              policy (launch-policy! k
                                     (.getAbsolutePath dir)
                                     (.getAbsolutePath socket)
                                     (boot-read-paths library guest-dir))
              extra (cond-> {"VIS_PYTHON_TLS_STRICT" (str (tls-strict?))}
                      packages
                      (assoc Locations/PACKAGES_ENV (.getCanonicalPath packages))

                      library
                      (assoc runtime/native-path-env (str library)))]

          (spit log "" :append true)
          (let [^Process process (process-jail/spawn!
                                   (child-argv library (.getAbsolutePath socket) guest-dir)
                                   nil
                                   policy
                                   {:extra-environment extra :merge-stderr? true})
                _ (drain-output! process log)
                accepted (await-worker-connection server process)]

            (when-not accepted
              (.destroy process)
              (throw (ex-info "the python worker did not start"
                              {:type :vis/python-worker :log (.getAbsolutePath log)})))
            (let [peer (assoc (child/peer-over accepted) :host-sessions (atom #{}))
                  state {:process process :peer peer :log log}
                  thread (Thread. ^Runnable
                                  #(child/pump! peer
                                                serve-host-call!
                                                (fn []
                                                  (str "the python worker exited; see "
                                                       (.getAbsolutePath log))))
                                  "vis-python-extension-pump")]

              (.setDaemon thread true)
              (.start thread)
              (tel/log! {:level :debug :id ::started} (str "python worker pid " (.pid process)))
              state)))
        (finally (Files/deleteIfExists (.toPath socket)) (Files/deleteIfExists control-dir))))))

(defn- alive? [state] (and state (.isAlive ^Process (:process state))))

(defn worker-live?
  "True when `k` already owns a live worker; never starts one."
  [k]
  (boolean (alive? (get @workers k))))

(defn worker-pids
  "PIDs of every live session or shared Python worker this process owns."
  []
  (->> (vals @workers)
       (keep (fn [state]
               (when (alive? state) (.pid ^Process (:process state)))))
       vec))

(defn- live
  "Start a worker only for a new key. A dead or retired worker has lost its
   namespace and host bindings; only a rebuilt environment may replace it."
  [k]
  (locking workers
    (let [state
          (get @workers k)

          reason
          (or (get @retired-workers k)
              (when (and state (not (alive? state))) "exited unexpectedly"))]

      (when reason
        (swap! retired-workers assoc k reason)
        (throw (ex-info (str
                          "this session's Python worker was retired (" reason
                          "). Its sandbox — every variable, import and tool — is gone until "
                          "the next turn starts a fresh one; finish this turn with what you have.")
                        {:type :vis/python-worker-retired :worker k :reason reason})))
      (or state
          (let [started (start! k)]
            (swap! workers assoc k started)
            started)))))

(def ^:private INTERRUPT_REPLY_MS
  "Maximum wait for the worker control plane to acknowledge an interrupt."
  1000)

(defn- ask
  ([k op session code] (ask k op session code nil))
  ([k op session code timeout-ms]
   (let [peer
         (:peer (live k))

         installing?
         (= "install-runtime" op)

         sessions
         (:host-sessions peer)

         newly-assigned?
         (and installing? (not (contains? @sessions session)))]

     ;; Assign before bootstrap can call the host; revoke before closing a namespace.
     (when installing? (swap! sessions conj session))
     (when (= "close" op) (swap! sessions disj session))
     (try (child/request! peer
                          (cond-> {"op" op "session" session}
                            code
                            (assoc "code" code))
                          timeout-ms)
          (catch Throwable error
            (when newly-assigned? (swap! sessions disj session))
            (throw error))))))

(defn install-runtime! [k session] (ask k "install-runtime" session nil))

(defn install-sync-tool! [k session tool-name] (ask k "install-sync-tool" session tool-name))

(defn install-tool! [k session tool-name] (ask k "install-tool" session tool-name))

(defn install-module! [k session module] (ask k "install-module" session module))

(defn exec! [k session code] (ask k "exec" session code))

(defn run [k session code] (ask k "run" session code))

(defn run-block [k session code] (ask k "run-block" session code))

(defn eval-str [k session code] (ask k "eval" session code))

(defn close-session! [k session] (ask k "close" session nil))

(defn pending-replies
  "Snapshot the replies still owed by `k`, without starting a worker. These settle
   only when the guest replies or exits, even if its host caller was cancelled."
  [k]
  (some-> (get @workers k)
          :peer
          :pending
          deref
          vals
          vec))

(defn interrupt!
  "Interrupt whatever `k`'s interpreter is running for `session` and answer
   whether the child acknowledged it. A guest parked in a host call cannot take
   the interrupt until that call answers, so every host call in flight is failed
   here as well — whether or not the child answered in time."
  [k session]
  (when-let [state (let [state (get @workers k)]
                     (when (alive? state) state))]
    (try
      (ask k "interrupt" session nil INTERRUPT_REPLY_MS)
      (finally
        (fail-host-calls!
          (:peer state)
          "the block was interrupted while this host call was still running; its result is discarded")))))

(defn stdin! [k session text] (ask k "stdin" session (str text)))

(defn trust! [k session trusted?] (ask k "trust" session (if trusted? "1" "0")))

(defn confine!
  "Confine `k`'s interpreter to `read`/`write`, or lift it with two empty lists.
   The policy is that PROCESS's, which is why one worker per session is the whole
   point: what used to be every session in the gateway is now this session."
  [k session read write refusal]
  (ask k
       "confine"
       session
       (json/write-json-str {"read" (vec read) "write" (vec write) "refusal" (str refusal)})))

(defn network!
  [k session enabled? refusal]
  (ask k
       "network"
       session
       (json/write-json-str {"enabled" (boolean enabled?) "refusal" (str refusal)})))

(defn stop-worker!
  "Stop the worker for `k`, if there is one. Idempotent. Closing the socket
   releases every pending parent call; a child that does not leave promptly is
   force-killed so retired sessions cannot accumulate processes."
  [k]
  (locking workers
    (when-let [state (get @workers k)]
      (swap! workers dissoc k)
      ;; Lifecycle cancellation is explicit, not an authorization or crash error.
      (let [[pending _] (reset-vals! (:pending (:peer state)) {})]
        (doseq [[_ waiting] pending]
          (deliver waiting {"error" "Python worker stopped by session lifecycle cancellation"})))
      (try (.close ^SocketChannel (:channel (:peer state))) (catch Throwable _ nil))
      (let [^Process process (:process state)]
        (.destroy process)
        (try (when-not (.waitFor process 200 TimeUnit/MILLISECONDS) (.destroyForcibly process))
             (catch Throwable _ (try (.destroyForcibly process) (catch Throwable _ nil)))))))
  nil)

(defn retire-worker!
  "Stop `k`'s worker after its control plane stopped answering and refuse to
   start another under this key until the session is rebuilt or disposed."
  [k reason]
  (swap! retired-workers assoc k (str reason))
  (stop-worker! k))

(defn retired?
  "True when `k` was retired and not yet rebuilt or disposed."
  [k]
  (contains? @retired-workers k))

(defn forget-policy!
  "Forget `k` after ordinary session disposal; stopping a wedged worker alone keeps
   the policy so no late call can restart outside the boundary."
  [k]
  (swap! worker-policy-fns dissoc k)
  (swap! retired-workers dissoc k)
  nil)

(defn stop!
  "Stop every worker. Idempotent; each process is a daemon of this one's
   lifetime, so an unclean exit leaves nothing behind."
  []
  (doseq [k (keys @workers)]
    (stop-worker! k))
  (reset! worker-policy-fns {})
  (reset! retired-workers {})
  nil)
