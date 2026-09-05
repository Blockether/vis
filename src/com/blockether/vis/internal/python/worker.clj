(ns com.blockether.vis.internal.python.worker
  "ONE Python execution environment per session: the runtime-owned worker
   holding the interpreter that session's sandbox AND its extensions run in.

   Why a process and not a namespace. Confinement, the thread cap and the
   network capability are PROCESS state in the embedded runtime — one policy for
   everything the interpreter serves — so sessions sharing one interpreter share
   one policy, and `sys.modules`, every module global and every native library's
   cache with it. A worker per session makes the policy the session's own, gives
   each session its own imports, and lets a wedged interpreter be killed without
   touching anybody else's work.

   Why the sandbox and the extensions belong TOGETHER in it. They are the same
   session's Python; what separates them is not a process but TRUST, which the
   runtime keeps per session name: an extension namespace is marked trusted and
   reaches the filesystem through `vis.fs`, the sandbox is not and is confined.
   The identity behind that flag is what the runtime was ASKED to run, which no
   Python can forge — `exec` into another namespace's globals does not move a
   block into it (measured; it did, against an identity taken from frames).

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
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.python.host :as python-host]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.python.worker-peer :as child]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis-python-runtime :as runtime]
            [taoensso.telemere :as tel])
  (:import (com.blockether.vispython Locations)
           (java.io File)
           (java.lang.management ManagementFactory)
           (java.net StandardProtocolFamily UnixDomainSocketAddress)
           (java.nio.channels SelectionKey Selector ServerSocketChannel SocketChannel)
           (java.nio.file Files)
           (java.util.concurrent TimeUnit)))

(set! *warn-on-reflection* true)

(defonce ^:private guest-source-directory
  (delay (let [dir (io/file (System/getProperty "user.home") ".vis" "python" "vis-guest")]
           (.mkdirs dir)
           (doseq [name ["vis_introspection.py" "vis_autoinstall.py"]]
             (let [resource (or (io/resource (str "vis-guest/" name))
                                (throw (ex-info (str "Missing Vis guest module " name)
                                                {:module name})))
                   target (io/file dir name)
                   source (slurp resource)]

               (when-not (and (.isFile target) (= source (slurp target))) (spit target source))))
           (.getCanonicalPath dir))))

(defn guest-source-dir
  "Stage and answer the directory containing Vis-owned Python guest modules."
  []
  @guest-source-directory)

(defn- serve-host-call!
  "Answer one `host` request from the child on this thread, which is on record
   under the request's id for as long as the tool runs so an interrupt can find it."
  [peer message]
  (let [id (get message "id")]
    (swap! (:serving peer) assoc id (Thread/currentThread))
    (let [value (python-host/dispatch (get message "session")
                                      (get message "tool")
                                      (get message "payload"))]
      (when (child/claim-reply! peer id) (child/send-line! peer {"id" id "value" value})))))

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

(defn- child-argv
  "Start the runtime worker, never a second copy of Vis. JVM development uses
   the same Java entrypoint; native Vis requires the packaged runtime executable."
  [library socket guest-dir]
  (if (util/native-image?)
    (if-let [executable (runtime/resolve-worker {:path library})]
      [executable socket guest-dir]
      (throw (ex-info "The Python runtime archive has no worker executable"
                      {:type :vis/python-worker-missing})))
    (vec (concat [(str (System/getProperty "java.home") File/separator "bin" File/separator "java")]
                 (.getInputArguments (ManagementFactory/getRuntimeMXBean))
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
  (when (or (= k shared-key) (not (ifn? policy-fn)))
    (throw (ex-info "A session Python worker requires a launch policy"
                    {:type ::worker-policy-missing :worker k})))
  (swap! worker-policy-fns assoc k policy-fn)
  (swap! retired-workers dissoc k)
  k)

(defn- launch-policy!
  [k run-directory control-socket boot-read-paths]
  (when-not (= k shared-key)
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
                (Locations/packagesDir) guest-dir]
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

        socket
        (io/file dir "control.sock")

        log
        (io/file dir "worker.log")

        address
        (UnixDomainSocketAddress/of (.toPath socket))]

    (with-open [server (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
                         (.bind address))]
      (try
        (let [library (try (python-runtime/ensure-library!)
                           (catch Throwable t
                             (tel/log! {:level :debug
                                        :id ::no-library-to-hand-over
                                        :data {:error (ex-message t)}})
                             nil))
              guest-dir (guest-source-dir)
              policy (launch-policy! k
                                     (.getAbsolutePath dir)
                                     (.getAbsolutePath socket)
                                     (boot-read-paths library guest-dir))
              extra (cond-> {}
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
            (let [peer (child/peer-over accepted)
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
        (finally (Files/deleteIfExists (.toPath socket)))))))

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
  "The worker for `k`, started if this is the first call or if the last one died.
   Starting is per key and under a lock, so two turns opening the same session at
   once share one worker instead of racing two interpreters into existence. A
   RETIRED key is refused instead: its session has to be rebuilt first."
  [k]
  (when-let [reason (get @retired-workers k)]
    (throw (ex-info (str "this session's Python worker was retired (" reason
                         "). Its sandbox — every variable, import and tool — is gone until "
                         "the next turn starts a fresh one; finish this turn with what you have.")
                    {:type :vis/python-worker-retired :worker k :reason reason})))
  (let [state (get @workers k)]
    (if (alive? state)
      state
      (locking workers
        (let [state (get @workers k)]
          (if (alive? state)
            state
            (let [started (start! k)]
              (swap! workers assoc k started)
              started)))))))

(def ^:private INTERRUPT_REPLY_MS
  "Maximum wait for the worker control plane to acknowledge an interrupt."
  1000)

(defn- ask
  ([k op session code] (ask k op session code nil))
  ([k op session code timeout-ms]
   (child/request! (:peer (live k))
                   (cond-> {"op" op "session" session}
                     code
                     (assoc "code" code))
                   timeout-ms)))

(defn install-runtime! [k session] (ask k "install-runtime" session nil))

(defn install-sync-tool! [k session tool-name] (ask k "install-sync-tool" session tool-name))

(defn install-tool! [k session tool-name] (ask k "install-tool" session tool-name))

(defn install-module! [k session module] (ask k "install-module" session module))

(defn exec! [k session code] (ask k "exec" session code))

(defn run [k session code] (ask k "run" session code))

(defn run-block [k session code] (ask k "run-block" session code))

(defn eval-str [k session code] (ask k "eval" session code))

(defn close-session! [k session] (ask k "close" session nil))

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
