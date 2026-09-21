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
   `eval`, `confine`, `network`, `stdin`, `interrupt`, `stack-diagnostics`,
   `dump-stacks`, `close`); the child asks
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
            [com.blockether.vis.internal.gateway.runtime :as gateway-runtime]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python.host :as python-host]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.worker-peer :as child]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis-python-runtime :as runtime]
            [taoensso.telemere :as tel])
  (:import (com.blockether.vispython Locations Sources)
           (java.io File FileOutputStream)
           (java.lang.management ManagementFactory)
           (java.net StandardProtocolFamily UnixDomainSocketAddress)
           (java.nio.channels SelectionKey Selector ServerSocketChannel SocketChannel)
           (java.nio.file CopyOption Files StandardCopyOption)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)
           (java.util.concurrent ExecutionException TimeUnit)
           (java.util.concurrent.locks ReentrantLock)))

(set! *warn-on-reflection* true)

(defn- materialize-guest-sources!
  "Publish complete modules atomically under their content identity, even during concurrent starts.

   The directory is named `<release>-<digest>`: the release so a person reading
   `~/.vis/python/vis-guest` sees which build left it there, the digest because
   the NAME is the identity. Two builds shipping different guest modules never
   share a directory, so a live worker's modules are never rewritten under it.
   This process CLAIMS the directory it uses, which is what lets another
   process' cleanup tell a served generation from an abandoned one."
  [root sources]
  (let [digest
        (util/sha256-hex (pr-str (into (sorted-map) sources)))

        release
        (str/replace (gateway-runtime/release-version) #"[^A-Za-z0-9._-]" "_")

        dir
        (io/file root (str release "-" (subs digest 0 12)))]

    (.mkdirs dir)
    (doseq [[name source] sources]
      (let [target (io/file dir name)]
        (when-not (and (.isFile target) (= source (slurp target)))
          (let [staged (Files/createTempFile (.toPath dir)
                                             ".vis-guest-"
                                             ".tmp"
                                             (make-array FileAttribute 0))]
            (try (spit (.toFile staged) source)
                 (Files/move staged
                             (.toPath target)
                             (into-array CopyOption
                                         [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING]))
                 (finally (Files/deleteIfExists staged)))))))
    (paths/claim-dir! dir)
    (.getCanonicalPath dir)))

(defonce ^:private guest-sources
  (delay (into {}
               (map (fn [name]
                      (let [resource (or (io/resource (str "vis-guest/" name))
                                         (throw (ex-info (str "Missing Vis guest module " name)
                                                         {:module name})))]
                        [name (slurp resource)])))
               ["vis_introspection.py" "vis_results.py" "vis_sdk.py"])))

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
        (get message "session")

        ;; Guest-created threads have no runtime activation identity. Only the
        ;; explicitly session-bound Council wake may use a trusted connection's
        ;; assigned namespace; model workers and every other host op still refuse.
        caller
        (if (and (= "" caller)
                 (true? (:trusted? peer))
                 (= "__vis_host_council_wake__" (get message "tool")))
          (try (get (json/read-json (str (get message "payload"))) "session")
               (catch Exception _ nil))
          caller)]

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

(defonce ^:private worker-locks
  ;; Count holders AND waiters so a key never gets two lifecycle locks. Entries
  ;; disappear after their last user; disposed sessions leave no lock registry.
  (atom {}))

(defn- with-worker-lock
  "Serialize one worker's lifecycle, never another worker's startup or control.
   Requests can cancel while waiting; teardown must also run on an interrupted thread."
  [k interruptible? f]
  (let [^ReentrantLock gate (locking worker-locks
                              (let [entry (or (get @worker-locks k)
                                              {:lock (ReentrantLock.) :users 0})]
                                (swap! worker-locks assoc k (update entry :users #(inc (long %))))
                                (:lock entry)))]
    (try (if interruptible? (.lockInterruptibly gate) (.lock gate))
         (try (f) (finally (.unlock gate)))
         (finally (locking worker-locks
                    (swap! worker-locks (fn [locks]
                                          (let [users (long (get-in locks [k :users]))]
                                            (if (= 1 users)
                                              (dissoc locks k)
                                              (assoc-in locks [k :users] (dec users)))))))))))

(def shared-key
  "The worker for Python that belongs to no single session: extension files
   loading at startup, whose REGISTRATION is the gateway's and not a session's."
  "shared")

(defrecord ^:private TrustedExtensionWorker [session packages])

(defn extension-worker-key
  "Host-owned worker identity for a session's trusted extensions, not its sandbox.
   The key is never accepted from Python or reconstructed from a wire payload.
   A declared project's package directory is immutable startup data, not a later
   sys.path override; nil keeps the shared-package default."
  ([session] (->TrustedExtensionWorker session nil))
  ([session packages]
   (->TrustedExtensionWorker session
                             (some-> packages
                                     io/file
                                     .getCanonicalPath))))

(defn- trusted-worker? [k] (or (= k shared-key) (instance? TrustedExtensionWorker k)))

(defn- worker-jvm-options
  "Keep runtime flags, not the parent's recording, repository or diagnostic destinations.
   Worker diagnostic files belong to its own writable log directory."
  [options]
  (remove #(or (str/starts-with? % "-XX:StartFlightRecording")
               (str/starts-with? % "-XX:FlightRecorderOptions")
               (str/starts-with? % "-XX:ErrorFile=")
               (str/starts-with? % "-XX:HeapDumpPath="))
    options))

(defn- child-argv
  "Prefer the selected runtime's packaged worker, including from a JVM host.
   A source-only runtime uses its Java entrypoint; native Vis requires the executable.
   Source roots are the same pre-confinement snapshot used by the boot policy."
  [library socket guest-dir run-directory runtime-roots]
  (let [arguments (into [socket "--resolved-sources"] (conj runtime-roots guest-dir))]
    (if-let [executable (runtime/resolve-worker {:path library})]
      (into [executable (str "-Duser.home=" (System/getProperty "user.home"))] arguments)
      (if (util/native-image?)
        (throw (ex-info "The Python runtime archive has no worker executable"
                        {:type :vis/python-worker-missing}))
        (vec (concat
               [(str (System/getProperty "java.home") File/separator "bin" File/separator "java")]
               (worker-jvm-options (.getInputArguments (ManagementFactory/getRuntimeMXBean)))
               [(str "-XX:ErrorFile=" (io/file run-directory "jvm-crash-%p.log"))
                (str "-XX:HeapDumpPath=" (io/file run-directory "jvm-heap.hprof")) "-cp"
                (System/getProperty "java.class.path") "com.blockether.vispython.Worker"]
               arguments))))))

(defn- worker-dir
  ^File [stamp]
  (doto (io/file (paths/ensure-log-date-dir!) (str "pyext-" stamp)) (.mkdirs)))

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
  [library guest-dir runtime-roots packages]
  (->> (concat [(System/getProperty "user.dir") (System/getProperty "java.home") packages
                (Locations/sourcesDir) guest-dir]
               runtime-roots
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
  "Drain process output without buffering; startup and hang evidence stays visible while alive."
  [^Process process ^File log]
  (let [thread (Thread. ^Runnable
                        (fn []
                          (try (with-open [out (FileOutputStream. log true)]
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

(def ^:private STACK_CONTROL_MS
  "Maximum wait for a stack-control exchange, including a blocked socket write."
  200)

(defn- stack-control!
  [peer message]
  (let [control (cancellation/worker-future "vis-python-stack-control"
                                            #(child/request! peer message)
                                            {:platform? true})]
    (try (let [reply (deref control STACK_CONTROL_MS ::timed-out)]
           (if (= ::timed-out reply)
             {:status :timed-out}
             (case (get reply "status")
               "ready"
               {:status :ready}

               "written"
               {:status :written}

               "unavailable"
               {:status :unavailable}

               "busy"
               {:status :busy}

               "failed"
               {:status :failed}

               {:status :failed :reason :invalid-reply})))
         (catch ExecutionException _ {:status :failed :reason :control-failed})
         (finally (future-cancel control)))))

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
              packages (some-> (or (when (instance? TrustedExtensionWorker k) (:packages k))
                                   (runtime/packages-dir))
                               io/file)
              _ (when packages
                  (Files/createDirectories (.toPath packages)
                                           (make-array java.nio.file.attribute.FileAttribute 0)))
              guest-dir (guest-source-dir)
              ;; Resolve once before confinement; reuse the same roots for Python imports.
              runtime-roots (vec (Sources/roots))
              policy (launch-policy! k
                                     (.getAbsolutePath dir)
                                     (.getAbsolutePath socket)
                                     (boot-read-paths library
                                                      guest-dir
                                                      runtime-roots
                                                      (some-> packages
                                                              .getCanonicalPath)))
              extra (cond-> {"VIS_PYTHON_TLS_STRICT" (str (tls-strict?))}
                      packages
                      (assoc Locations/PACKAGES_ENV (.getCanonicalPath packages))

                      library
                      (assoc runtime/native-path-env (str library)))]

          (spit log "" :append true)
          (let [^Process process
                (process-jail/spawn!
                  (child-argv library (.getAbsolutePath socket) guest-dir dir runtime-roots)
                  nil
                  policy
                  {:extra-environment extra :merge-stderr? true})
                _ (drain-output! process log)
                accepted (await-worker-connection server process)]

            (when-not accepted
              (.destroy process)
              (throw (ex-info "the python worker did not start"
                              {:type :vis/python-worker :log (.getAbsolutePath log)})))
            (let [peer (assoc (child/peer-over accepted)
                         :host-sessions (atom #{})
                         :trusted? (trusted-worker? k))
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
              (try
                ;; #194: preserve the host's pinned import order before install-runtime.
                ;; Bootstrap already uses these roots without extracting bundled sources.
                (child/request!
                  peer
                  {"op" "exec"
                   "session" runtime/default-session
                   "code" (str "import json, sys\n__vis_runtime_roots__ = json.loads("
                               (pr-str (util/json-str runtime-roots))
                               ")\nsys.path[:] = __vis_runtime_roots__ + [p for p in sys.path"
                               " if p not in __vis_runtime_roots__]\n")})
                (tel/log! {:level :debug :id ::started} (str "python worker pid " (.pid process)))
                ;; The runtime retains a private sink before interpreter confinement.
                ;; No trace is emitted until retirement requests one for this process.
                (let [path (.getAbsolutePath (io/file dir "python-stacks.log"))
                      setup (stack-control! peer {"op" "stack-diagnostics" "code" path})]

                  (assoc state
                    :python-stacks (cond-> setup
                                     (= :ready (:status setup))
                                     (assoc :path path))))
                (catch Throwable error (.destroy process) (throw error))))))
        (finally (Files/deleteIfExists (.toPath socket)) (Files/deleteIfExists control-dir))))))

(defn- alive? [state] (and state (.isAlive ^Process (:process state))))

(defn worker-live?
  "True when `k` already owns a live worker; never starts one."
  [k]
  (boolean (alive? (get @workers k))))

(def ^:private READY_REPLY_MS
  "Maximum between-turn wait for an existing worker to enter its Python namespace."
  5000)

(defn worker-ready?
  "Can the existing worker enter `session` before a new turn starts?

   Process liveness alone does not prove the interpreter can acquire its GIL.
   Bound the entire exchange, including a blocked socket write. Only call between
   turns: a running block may legitimately occupy the interpreter for minutes.
   Never starts or replaces a worker; the engine owns recovery after a false result.
   Caller cancellation propagates instead of declaring a healthy worker broken."
  [k session]
  (boolean (when-let [state (get @workers k)]
             (when (and (alive? state) (not (contains? @retired-workers k)))
               (let [peer (:peer state)
                     probe (cancellation/worker-future
                             "vis-python-readiness"
                             (bound-fn
                               []
                               (child/request! peer {"op" "eval" "session" session "code" "True"}))
                             {:platform? true})]

                 (try (and (= "True" (deref probe READY_REPLY_MS ::not-ready))
                           (identical? state (get @workers k)))
                      (catch ExecutionException _ false)
                      (finally (future-cancel probe))))))))

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
  (with-worker-lock
    k
    true
    (fn []
      (let [state
            (get @workers k)

            reason
            (or (get @retired-workers k)
                (when (and state (not (alive? state))) "exited unexpectedly"))]

        (when reason
          (swap! retired-workers assoc k reason)
          (throw (ex-info
                   (str "this session's Python worker was retired (" reason
                        "). Its sandbox — every variable, import and tool — is gone until "
                        "the next turn starts a fresh one; finish this turn with what you have.")
                   {:type :vis/python-worker-retired :worker k :reason reason})))
        (or state
            (let [started (start! k)]
              (swap! workers assoc k started)
              started))))))

(def ^:private INTERRUPT_REPLY_MS
  "Maximum wait for the whole interrupt exchange, including socket writes."
  1000)

(defn- ask
  [k op session code]
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
    (try
      (child/request! peer
                      (cond-> {"op" op "session" session}
                        code
                        (assoc "code" code)))
      (catch Throwable error (when newly-assigned? (swap! sessions disj session)) (throw error)))))

(defn install-runtime!
  [k session]
  (let [installed (ask k "install-runtime" session nil)]
    (ask k "exec" session (python-runtime/version-globals-python))
    installed))

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
  "Interrupt the current worker for `session` without starting a replacement.
   Fail its host calls too, so a guest parked in one can unwind. The entire
   exchange is bounded, including writes; on timeout the caller must retire
   the unresponsive worker. Answer whether the child acknowledged the interrupt."
  [k session]
  (when-let [state (let [state (get @workers k)]
                     (when (alive? state) state))]
    (let
      [peer (:peer state)
       ;; A blocked writer must not park the cancellation caller or pin a
       ;; virtual-thread carrier. Retirement closes this same peer on timeout.
       control
       (cancellation/worker-future
         "vis-python-interrupt"
         (bound-fn
           []
           (try
             (child/request! peer {"op" "interrupt" "session" session})
             (finally
               (fail-host-calls!
                 peer
                 "the block was interrupted while this host call was still running; its result is discarded"))))
         {:platform? true})]

      (try (let [reply (deref control INTERRUPT_REPLY_MS ::interrupt-timed-out)]
             (when (identical? ::interrupt-timed-out reply)
               (throw (ex-info "the python worker did not answer interrupt"
                               {:type :vis/python-worker-timeout
                                :op "interrupt"
                                :timeout-ms INTERRUPT_REPLY_MS})))
             reply)
           (catch ExecutionException error (throw (.getCause error)))
           (finally (future-cancel control))))))

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
  (with-worker-lock
    k
    false
    (fn []
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
               (catch Throwable _ (try (.destroyForcibly process) (catch Throwable _ nil))))))))
  nil)

(def ^:private HANG_DIAGNOSTIC_MS
  "Maximum added retirement delay for best-effort local hang evidence."
  500)

(defn- python-stack-diagnostic!
  [state]
  (let [setup (:python-stacks state)]
    (if (= :ready (:status setup))
      (let [result (stack-control! (:peer state) {"op" "dump-stacks"})]
        (cond-> result
          (= :written (:status result))
          (assoc :path (:path setup))))
      (or setup {:status :unavailable :reason :not-configured}))))

(defn- write-hang-diagnostic!
  [k state reason]
  (let [^File log
        (:log state)

        ^Process process
        (:process state)

        file
        (io/file (.getParent log) "hang.edn")

        jvm-stacks
        (Thread/getAllStackTraces)

        ;; Capture the stalled operation before diagnostic control changes last-RPC metadata.
        rpc
        (child/rpc-snapshot (:peer state))

        python-stacks
        (python-stack-diagnostic! state)

        report
        {:worker (str k)
         :pid (.pid process)
         :recorded-ms (util/now-ms)
         :jvm-thread-count (count jvm-stacks)
         :reason (str reason)
         :rpc rpc
         :python-stacks python-stacks
         :jvm-threads (mapv (fn [[^Thread thread stack]]
                              {:id (.threadId thread)
                               :name (.getName thread)
                               :state (str (.getState thread))
                               :stack (mapv str (take 64 stack))})
                            (take 128
                                  (sort-by (fn [[^Thread thread _]]
                                             (.threadId thread))
                                           jvm-stacks)))}

        staged
        (Files/createTempFile (.toPath (.getParentFile file))
                              ".hang-"
                              ".edn"
                              (into-array FileAttribute
                                          [(PosixFilePermissions/asFileAttribute
                                             (PosixFilePermissions/fromString "rw-------"))]))]

    (try (spit (.toFile staged) (str (pr-str report) "\n"))
         (Files/move staged
                     (.toPath file)
                     (into-array CopyOption
                                 [StandardCopyOption/ATOMIC_MOVE
                                  StandardCopyOption/REPLACE_EXISTING]))
         (.getAbsolutePath file)
         (finally (Files/deleteIfExists staged)))))

(defn- capture-hang-diagnostic!
  "Capture before socket close or process kill. Failure, cancellation or slow disk
   must not prevent retirement. No guest code, arguments, replies or exception text
   enter the report; local stack frames can still contain file and function names."
  [k state reason]
  (let [interrupted?
        (Thread/interrupted)

        capture
        (cancellation/worker-future "vis-python-hang-diagnostic"
                                    #(write-hang-diagnostic! k state reason)
                                    {:platform? true})]

    (try (let [path (deref capture HANG_DIAGNOSTIC_MS ::timed-out)]
           (tel/log! {:level :warn
                      :id ::hang-diagnostic
                      :data (cond-> {:worker (str k)
                                     :pid (.pid ^Process (:process state))
                                     :status (if (= ::timed-out path) :timed-out :written)}
                              (string? path)
                              (assoc :path path))}
                     (if (= ::timed-out path)
                       "Python worker hang evidence timed out; retirement will continue"
                       "Python worker hang evidence collected before retirement")))
         (catch Throwable error
           (when (instance? InterruptedException error) (.interrupt (Thread/currentThread)))
           (tel/log! {:level :warn
                      :id ::hang-diagnostic
                      :data {:worker (str k) :status :failed :error-type (str (class error))}}
                     "Could not collect Python worker hang evidence; retirement will continue"))
         (finally (future-cancel capture)
                  (when interrupted? (.interrupt (Thread/currentThread)))))))

(defn retire-worker!
  "Record bounded local RPC/JVM evidence and request Python stacks, then stop `k`
   and refuse to restart until rebuilt or disposed. Ordinary disposal does not dump stacks."
  [k reason]
  (with-worker-lock k
                    false
                    (fn []
                      (swap! retired-workers assoc k (str reason))
                      (when-let [state (get @workers k)]
                        (capture-hang-diagnostic! k state reason))
                      (stop-worker! k))))

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
