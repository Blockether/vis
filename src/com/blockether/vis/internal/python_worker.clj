(ns com.blockether.vis.internal.python-worker
  "ONE Python execution environment per session: a child of this same binary
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
   numbers its own requests and no id can collide. Nothing here has a timeout: a
   block or an extension tool may legitimately run for minutes, and a child that
   DIES is what the pump reports — every call waiting on it fails at once with
   the child's log to read."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.python-host :as python-host]
            [com.blockether.vis.internal.python-runtime :as python-runtime]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis-python-runtime :as runtime]
            [taoensso.telemere :as tel])
  (:import (java.io BufferedReader BufferedWriter File InputStreamReader OutputStreamWriter)
           (java.lang ProcessBuilder$Redirect)
           (java.lang.management ManagementFactory)
           (java.net StandardProtocolFamily UnixDomainSocketAddress)
           (java.nio.channels Channels ServerSocketChannel SocketChannel)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files)
           (java.util.concurrent Executors ExecutorService)
           (java.util.concurrent.atomic AtomicLong)))

(set! *warn-on-reflection* true)

(def socket-env
  "The environment variable that turns a fresh vis process into this host. A
   process started with it set never reaches argv: the socket IS the whole
   instruction, which is why no CLI command exposes this."
  "VIS_PYTHON_WORKER_SOCKET")

;; The peer — one live connection, used identically on both sides

(defn- peer-over
  "A peer over `channel`: what to read, what to write, and who is waiting."
  [^SocketChannel channel]
  {:channel channel
   :reader (BufferedReader. (InputStreamReader. (Channels/newInputStream channel)
                                                StandardCharsets/UTF_8))
   :writer (BufferedWriter. (OutputStreamWriter. (Channels/newOutputStream channel)
                                                 StandardCharsets/UTF_8))
   :pending (atom {})
   :seq (AtomicLong. 0)
   :workers (Executors/newCachedThreadPool (reify
                                             java.util.concurrent.ThreadFactory
                                               (newThread [_ runnable]
                                                 (doto (Thread. ^Runnable runnable
                                                                "vis-python-worker")
                                                   (.setDaemon true)))))})

(defn- send-line!
  "Write one message. Synchronized because both a reply and a fresh request can
   be written from different threads and a torn line is unparseable."
  [peer message]
  (let [^BufferedWriter writer (:writer peer)]
    (locking writer
      (.write writer ^String (json/write-json-str message))
      (.write writer "\n")
      (.flush writer))))

(defn- request!
  "Ask the peer `message` and answer its reply value; its error throws here."
  [peer message]
  (let [id
        (.incrementAndGet ^AtomicLong (:seq peer))

        waiting
        (promise)]

    (swap! (:pending peer) assoc id waiting)
    (try (send-line! peer (assoc message "id" id))
         (let [reply @waiting]
           (if (contains? reply "error")
             (throw (ex-info (str (get reply "error"))
                             {:type :vis/python-worker :op (get message "op")}))
             (get reply "value")))
         (finally (swap! (:pending peer) dissoc id)))))

(defn- pump!
  "Read this peer until it closes: a reply settles whoever waits for it, a
   request goes to `serve` on a thread of its own — serving inline would stall
   the pump behind a call that is itself waiting on this peer.

   On close every pending call fails with `reason`, because a caller parked on a
   child that has died would otherwise wait forever."
  [peer serve reason]
  (try (loop []

         (when-let [line (.readLine ^BufferedReader (:reader peer))]
           (when-not (str/blank? line)
             (let [message (json/read-json line :key-fn identity)]
               (if (contains? message "op")
                 (.submit ^ExecutorService (:workers peer) ^Runnable #(serve peer message))
                 (some-> (get @(:pending peer) (get message "id"))
                         (deliver message)))))
           (recur)))
       (catch Throwable _ nil)
       (finally (doseq [[_ waiting] @(:pending peer)]
                  (deliver waiting {"error" (reason)})))))

;; The child half

(defn- serve-op
  "Run one request from the parent against this process's interpreter."
  [peer message]
  (let [{:strs [id op session code]} message]
    (send-line! peer
                (try {"id" id
                      "value" (case op
                                "install-runtime"
                                (runtime/install-runtime! session)

                                "install-sync-tool"
                                (runtime/install-sync-tool! session code)

                                "install-tool"
                                (runtime/install-tool! session code)

                                "install-module"
                                (runtime/install-module! session code)

                                "exec"
                                (do (runtime/exec! session code) nil)

                                "run"
                                (runtime/run session code)

                                "run-block"
                                (runtime/run-block session code)

                                "eval"
                                (runtime/eval-str session code)

                                ;; Policy is the PROCESS's, and this process is one
                                ;; session's, which is the whole reason the worker
                                ;; exists: what used to be "every session in the
                                ;; gateway" is now exactly this session.
                                "confine"
                                (let [{:strs [read write refusal]} (json/read-json code
                                                                                  :key-fn identity)]
                                  (runtime/confine! (vec read) (vec write) (str refusal))
                                  nil)

                                "network"
                                (let [{:strs [enabled refusal]} (json/read-json code
                                                                                :key-fn identity)]
                                  (runtime/network! (boolean enabled) (str refusal))
                                  nil)

                                "trust"
                                (do (runtime/trust! session (= "1" code)) nil)

                                "stdin"
                                (do (runtime/stdin! code) nil)

                                "interrupt"
                                (runtime/interrupt!)

                                "close"
                                (do (runtime/trust! session false)
                                    (runtime/close-session! session))

                                (throw (ex-info (str "no worker op named " op) {:op op})))}
                     (catch Throwable t {"id" id "error" (or (ex-message t) (str t))})))))

(defn serve!
  "BE a worker: connect back to the parent on `socket-path` and serve
   it until it hangs up.

   The interpreter here is deliberately the runtime's default one — no
   `confine!`, no thread cap — because the code it runs is the user's own
   extensions, at the same trust level as a Clojure extension on the classpath.
   Nothing the model wrote ever reaches this process."
  [socket-path]
  (let [channel
        (SocketChannel/open (UnixDomainSocketAddress/of ^String socket-path))

        peer
        (peer-over channel)]

    ;; This process resolves the interpreter for itself: it is a child JVM with
    ;; its own classpath and no inherited resolution.
    (python-runtime/ensure-library!)
    (runtime/initialize! {})
    ;; The caller is the CHILD interpreter's answer, forwarded whole: the parent
    ;; authorizes against it, and a payload that names something else is the
    ;; guest's word, not the interpreter's.
    (runtime/bind-host! (fn [session tool payload]
                          (request! peer {"op" "host"
                                          "session" session
                                          "tool" tool
                                          "payload" payload})))
    (pump! peer serve-op (constantly "the vis process that owns this host is gone"))
    (.close channel)))

(defn -main
  "Entry for the JVM child, which is started as this namespace rather than as
   the whole CLI: the socket comes from the environment either way."
  [& _]
  (serve! (System/getenv socket-env))
  (shutdown-agents))

;; The parent half

(defonce
  ^:private
  ^{:doc "worker key -> {:process :peer :log}. One entry per live worker: a
          SESSION's key gives that session its own interpreter, and the shared
          key is what everything not owned by a session runs in."}
  workers
  (atom {}))

(def shared-key
  "The worker for Python that belongs to no single session: extension files
   loading at startup, whose REGISTRATION is the gateway's and not a session's."
  "shared")

(defn- child-argv
  "How to start the child. The native binary starts ITSELF (the environment
   variable, read before argv, is what makes it the host); on the JVM the child
   is this namespace alone rather than the whole CLI, because loading the facade
   costs seconds a test run pays for nothing."
  []
  (if (util/native-image?)
    (vec (discovery/base-argv))
    (vec (concat [(str (System/getProperty "java.home") File/separator "bin" File/separator "java")]
                 (.getInputArguments (ManagementFactory/getRuntimeMXBean))
                 ["-cp" (System/getProperty "java.class.path") "clojure.main" "-m"
                  "com.blockether.vis.internal.python-worker"]))))

(defn- run-dir ^File [] (doto (io/file (System/getProperty "user.home") ".vis" "run") .mkdirs))

(defn- start!
  "Start the child and answer it, connected. The parent listens FIRST: the
   socket has to exist before the child can dial it, and accepting is how we
   learn the child came up rather than died on its own classpath."
  []
  (let [stamp
        (str (discovery/current-pid) "-" (util/now-ms))

        socket
        (io/file (run-dir) (str "pyext-" stamp ".sock"))

        log
        (io/file (run-dir) (str "pyext-" stamp ".log"))

        address
        (UnixDomainSocketAddress/of (.toPath socket))

        server
        (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX) (.bind address))

        builder
        (doto (ProcessBuilder. ^java.util.List (child-argv))
          (.redirectErrorStream true)
          (.redirectOutput (ProcessBuilder$Redirect/appendTo log)))]

    (.put (.environment builder) socket-env (.getAbsolutePath socket))
    (let [process
          (.start builder)

          accepted
          (deref (future (.accept server)) 60000 nil)]

      (.close server)
      (Files/deleteIfExists (.toPath socket))
      (when-not accepted
        (.destroy process)
        (throw (ex-info "the python worker did not start"
                        {:type :vis/python-worker :log (.getAbsolutePath log)})))
      (let [peer
            (peer-over accepted)

            state
            {:process process :peer peer :log log}]

        (doto (Thread. ^Runnable
                       #(pump! peer
                               (fn [p m]
                                 (send-line! p
                                             {"id" (get m "id")
                                              "value" (python-host/dispatch (get m "session")
                                                                            (get m "tool")
                                                                            (get m "payload"))}))
                               (fn []
                                 (str "the python worker exited; see "
                                      (.getAbsolutePath log))))
                       "vis-python-extension-pump")
          (.setDaemon true)
          (.start))
        (tel/log! {:level :debug :id ::started} (str "python worker pid " (.pid process)))
        state))))

(defn- alive? [state] (and state (.isAlive ^Process (:process state))))

(defn worker-live?
  "True when `k` already owns a live worker; never starts one."
  [k]
  (boolean (alive? (get @workers k))))

(defn- live
  "The worker for `k`, started if this is the first call or if the last one died.
   Starting is per key and under a lock, so two turns opening the same session at
   once share one worker instead of racing two interpreters into existence."
  [k]
  (let [state (get @workers k)]
    (if (alive? state)
      state
      (locking workers
        (let [state (get @workers k)]
          (if (alive? state)
            state
            (let [started (start!)]
              (swap! workers assoc k started)
              started)))))))

(defn- ask
  [k op session code]
  (request! (:peer (live k))
            (cond-> {"op" op "session" session}
              code
              (assoc "code" code))))

(defn install-runtime! [k session] (ask k "install-runtime" session nil))
(defn install-sync-tool! [k session tool-name] (ask k "install-sync-tool" session tool-name))
(defn install-tool! [k session tool-name] (ask k "install-tool" session tool-name))
(defn install-module! [k session module] (ask k "install-module" session module))
(defn exec! [k session code] (ask k "exec" session code))
(defn run [k session code] (ask k "run" session code))
(defn run-block [k session code] (ask k "run-block" session code))
(defn eval-str [k session code] (ask k "eval" session code))
(defn close-session! [k session] (ask k "close" session nil))
(defn interrupt! [k session] (ask k "interrupt" session nil))
(defn stdin! [k session text] (ask k "stdin" session (str text)))
(defn trust! [k session trusted?] (ask k "trust" session (if trusted? "1" "0")))

(defn confine!
  "Confine `k`'s interpreter to `read`/`write`, or lift it with two empty lists.
   The policy is that PROCESS's, which is why one worker per session is the whole
   point: what used to be every session in the gateway is now this session."
  [k session read write refusal]
  (ask k "confine" session (json/write-json-str {"read" (vec read)
                                                 "write" (vec write)
                                                 "refusal" (str refusal)})))

(defn network!
  [k session enabled? refusal]
  (ask k "network" session (json/write-json-str {"enabled" (boolean enabled?)
                                                 "refusal" (str refusal)})))

(defn stop-worker!
  "Stop the worker for `k`, if there is one. Idempotent."
  [k]
  (locking workers
    (when-let [state (get @workers k)]
      (swap! workers dissoc k)
      (try (.close ^SocketChannel (:channel (:peer state))) (catch Throwable _ nil))
      (.destroy ^Process (:process state))))
  nil)

(defn stop!
  "Stop every worker. Idempotent; each process is a daemon of this one's
   lifetime, so an unclean exit leaves nothing behind."
  []
  (doseq [k (keys @workers)] (stop-worker! k))
  nil)
