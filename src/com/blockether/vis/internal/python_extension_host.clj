(ns com.blockether.vis.internal.python-extension-host
  "The SECOND interpreter: the process trusted extension Python runs in.

   Confinement, the thread cap and the network capability are PROCESS state in
   the embedded runtime — one policy for every session it serves — so a single
   process cannot hold a confined block and unconfined extension code at once.
   Measured, before this existed: one `python_execution` confined the process,
   and every later extension load was refused for reading its own file. The
   sandbox keeps the process it is in, because it is the hot path; extension
   Python moves HERE, into a child of this same binary whose interpreter is
   unconfined and uncapped.

   The wire is ONE line of JSON per message over a unix socket, both ways. The
   parent asks (`install-runtime`, `install-sync-tool`, `exec`, `run`, `eval`,
   `close`); the child asks back with `host`, because the registry that knows
   what a name may call, the persistence handle and the caller's dynamic
   binding frame all live in the parent (`python-host/dispatch`). stdout is NOT
   the wire: extension Python that prints, or a native library writing to fd 1,
   would corrupt it, so the child's own stdio goes to a log file instead.

   A message carrying `op` is a request, one without is its reply, so each side
   numbers its own requests and no id can collide. Nothing here has a timeout:
   an extension tool may legitimately run for minutes, and a child that DIES is
   what the pump reports — every call waiting on it fails at once with the
   child's log to read."
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
  "VIS_PYTHON_EXTENSION_SOCKET")

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
                                                                "vis-python-extension-host")
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
                             {:type :vis/python-extension-host :op (get message "op")}))
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
                                ;; Trusted BEFORE anything runs in it: this
                                ;; process exists to run the user's own
                                ;; extension code, and the runtime's `_vis_fs`
                                ;; asks the interpreter whether the session it
                                ;; is running is one the host trusts.
                                (do (runtime/trust! session)
                                    (runtime/install-runtime! session))

                                "install-sync-tool"
                                (runtime/install-sync-tool! session code)

                                "exec"
                                (do (runtime/exec! session code) nil)

                                "run"
                                (runtime/run session code)

                                "eval"
                                (runtime/eval-str session code)

                                "close"
                                (do (runtime/trust! session false)
                                    (runtime/close-session! session))

                                (throw (ex-info (str "no extension-host op named " op) {:op op})))}
                     (catch Throwable t {"id" id "error" (or (ex-message t) (str t))})))))

(defn serve!
  "BE the extension host: connect back to the parent on `socket-path` and serve
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

(defonce ^:private ^{:doc "The live child: its process, its peer and the log it writes to."} child
  (atom nil))

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
                  "com.blockether.vis.internal.python-extension-host"]))))

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
        (throw (ex-info "the python extension host did not start"
                        {:type :vis/python-extension-host :log (.getAbsolutePath log)})))
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
                                 (str "the python extension host exited; see "
                                      (.getAbsolutePath log))))
                       "vis-python-extension-pump")
          (.setDaemon true)
          (.start))
        (tel/log! {:level :debug :id ::started} (str "python extension host pid " (.pid process)))
        state))))

(defn- live
  "The running child, started if this is the first call or if the last one died."
  []
  (let [state @child]
    (if (and state (.isAlive ^Process (:process state)))
      state
      (locking child
        (let [state @child]
          (if (and state (.isAlive ^Process (:process state))) state (reset! child (start!))))))))

(defn- ask
  [op session code]
  (request! (:peer (live))
            (cond-> {"op" op "session" session}
              code
              (assoc "code" code))))

(defn install-runtime! [session] (ask "install-runtime" session nil))
(defn install-sync-tool! [session tool-name] (ask "install-sync-tool" session tool-name))
(defn exec! [session code] (ask "exec" session code))
(defn run [session code] (ask "run" session code))
(defn eval-str [session code] (ask "eval" session code))
(defn close-session! [session] (ask "close" session nil))

(defn stop!
  "Stop the child, if there is one. Idempotent; the process is also a daemon of
   this one's lifetime, so an unclean exit leaves nothing behind."
  []
  (locking child
    (when-let [state @child]
      (reset! child nil)
      (try (.close ^SocketChannel (:channel (:peer state))) (catch Throwable _ nil))
      (.destroy ^Process (:process state))))
  nil)
