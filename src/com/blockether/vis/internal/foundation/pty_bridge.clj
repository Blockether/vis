(ns com.blockether.vis.internal.foundation.pty-bridge
  "Passthrough bridge on top of the FFM pseudo-terminal (internal.foundation.pty).

   The problem it solves: a `shell_bg` child runs INSIDE the vis process (the FFM
   PTY master fd + reader thread live in vis's heap). That's great for the agent
   (`shell_send` / `shell_logs`) but a HUMAN can't jump into the live terminal to
   finish a step the agent can't — click through a browser OAuth, answer a prompt
   only a person can. tmux gets that for free because its server is a separate
   daemon you can `tmux attach` to; the FFM child is not.

   This namespace restores that capability WITHOUT tmux: each background PTY
   optionally exposes a per-shell UNIX-DOMAIN SOCKET. vis is the server (it holds
   the master fd); `vis ext shell attach <id>` is a thin client the human runs in
   their OWN Terminal.app. On connect the server (a) tees live master output to the
   socket and (b) forwards the socket's bytes to the master (stdin) — a genuine
   bidirectional passthrough. Multiple humans can attach at once; detaching just
   drops the socket and leaves the child running (exactly like `tmux detach`).

   Everything here is stdlib: `java.nio.channels` AF_UNIX sockets (JDK 16+,
   already in vis's native-image reachability metadata) on the server side, and
   `stty` for raw mode on the client side (the human's interactive shell always
   has it). No JNA, no new dep, native-image clean."
  (:require [clojure.string :as str])
  (:import (java.io InputStream OutputStream)
           (java.net StandardProtocolFamily UnixDomainSocketAddress)
           (java.nio ByteBuffer)
           (java.nio.channels ServerSocketChannel SocketChannel)
           (java.nio.file Files LinkOption Path Paths)
           (java.nio.file.attribute FileAttribute)
           (java.util.stream Collectors)))

;; =============================================================================
;; Socket paths
;; =============================================================================

(defn- sanitize
  "Filesystem-safe slug for a session key / shell id."
  [s]
  (-> (str s)
      (str/replace #"[^A-Za-z0-9._-]" "_")
      (as-> x (if (str/blank? x) "_" x))))

(defn bridge-dir
  "Directory the per-shell attach sockets live in: `$VIS_BRIDGE_DIR` or
   `~/.vis/bridge`. Created on demand by `serve!`."
  ^Path []
  (let [base (or (not-empty (System/getenv "VIS_BRIDGE_DIR"))
                 (str (System/getProperty "user.home") "/.vis/bridge"))]
    (Paths/get base (make-array String 0))))

(defn socket-path
  "Deterministic socket path for a (session, id) background shell:
   `<bridge-dir>/<session>__<id>.sock`. The `__<id>.sock` suffix lets the
   separate attach client find a shell by id alone (it scans the dir)."
  ^Path [session id]
  (let [dir
        (bridge-dir)

        iid
        (sanitize id)

        name
        (str (sanitize session) "__" iid ".sock")

        ;; sun_path is ~104 bytes; `<dir>/<name>` must fit or bind() fails
        ;; silently. When a long home + long session/id would overflow, collapse
        ;; the SESSION half to a short stable hash — the readable `__<id>.sock`
        ;; suffix that find-socket matches on is always preserved.
        room
        (- 100 (count (str dir)) 1)]

    (.resolve dir
              (if (<= (count name) room)
                name
                (str (Integer/toHexString (bit-and (hash (sanitize session)) 0x7fffffff))
                     "__"
                     iid
                     ".sock")))))

(defn find-socket
  "Resolve a socket Path for the attach client. `socket` (explicit path) wins;
   otherwise scan `bridge-dir` for the NEWEST `*__<id>.sock`. Returns a Path or
   nil when nothing matches."
  ^Path [{:keys [socket id]}]
  (cond (not (str/blank? socket)) (Paths/get ^String socket (make-array String 0))
        (not (str/blank? id))
        (let [dir
              (bridge-dir)

              suffix
              (str "__" (sanitize id) ".sock")]

          (when (Files/isDirectory dir (make-array LinkOption 0))
            (with-open [s (Files/list dir)]
              (->> (.collect s (Collectors/toList))
                   (filter (fn [^Path p]
                             (str/ends-with? (str (.getFileName p)) suffix)))
                   (sort-by (fn [^Path p]
                              (.toMillis (Files/getLastModifiedTime p (make-array LinkOption 0))))
                            (comp - compare))
                   first))))
        :else nil))

(defn sweep-orphans!
  "Remove stale attach sockets left by a crashed/killed prior vis: the JVM held
   the AF_UNIX server + PTY master fd, so a hard exit never ran serve!'s :stop
   and the `*.sock` files linger. For each socket in `bridge-dir`, try to
   CONNECT — a live server (this vis or another) accepts, so leave it; a dead
   one refuses, so unlink the orphan. Best-effort, never throws; safe to call
   once at startup."
  []
  (try (let [dir (bridge-dir)]
         (when (Files/isDirectory dir (make-array LinkOption 0))
           (with-open [s (Files/list dir)]
             (doseq [^Path p (.collect s (Collectors/toList))]
               (when (str/ends-with? (str (.getFileName p)) ".sock")
                 (let [live? (try (with-open [ch (SocketChannel/open (UnixDomainSocketAddress/of
                                                                       p))]
                                    (some? ch))
                                  (catch Throwable _ false))]
                   (when-not live? (try (Files/deleteIfExists p) (catch Throwable _ nil)))))))))
       (catch Throwable _ nil)))

;; =============================================================================
;; Server — expose a live PTY over an AF_UNIX socket
;; =============================================================================

(defn- write-all!
  "Write a byte[] fully to a SocketChannel (channels can short-write)."
  [^SocketChannel ch ^bytes ba]
  (let [buf (ByteBuffer/wrap ba)]
    (while (.hasRemaining buf) (.write ch buf))))

(defn serve!
  "Start an AF_UNIX passthrough server for the PTY handle `pty` at `path` (a
   String or Path). Runs a daemon accept loop; per accepted client it:
     - pushes `(replay-fn)` bytes (recent output, optional) so a late attacher
       sees context, then
     - tees every live master chunk to the client (via `pty`'s `:add-listener`), and
     - forwards the client's bytes into the master (`:send`) on a reader thread.
   Detach = the client closes its socket; the child keeps running. Returns
   `{:path <str> :stop (fn [])}`; `:stop` closes the server + all clients and
   unlinks the socket file. Never blocks the caller."
  [{:keys [pty path replay-fn]}]
  (let [^Path p
        (if (instance? Path path) path (Paths/get ^String (str path) (make-array String 0)))

        parent
        (.getParent p)

        _
        (when parent (Files/createDirectories parent (make-array FileAttribute 0)))

        _
        (Files/deleteIfExists p)

        add-listener
        (:add-listener pty)

        send
        (:send pty)

        server
        (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
          (.bind (UnixDomainSocketAddress/of p)))

        clients
        (atom #{})

        running
        (atom true)

        drop!
        (fn [^SocketChannel ch unsub]
          (when unsub (try (unsub) (catch Throwable _ nil)))
          (swap! clients disj ch)
          (try (.close ch) (catch Throwable _ nil)))

        on-client
        (fn [^SocketChannel ch]
          (swap! clients conj ch)
          (when replay-fn
            (try (let [ba (replay-fn)]
                   (when (and ba (pos? (alength ^bytes ba))) (write-all! ch ba)))
                 (catch Throwable _ nil)))
          (let [unsub (add-listener (fn [^bytes ba]
                                      (try (write-all! ch ba) (catch Throwable _ (drop! ch nil)))))]
            (doto (Thread. ^Runnable
                           (fn []
                             (try (let [buf (ByteBuffer/allocate 4096)]
                                    (loop []

                                      (.clear buf)
                                      (let [n (.read ch buf)]
                                        (when (>= n 0)
                                          (when (pos? n)
                                            (.flip buf)
                                            (let [ba (byte-array n)]
                                              (.get buf ba)
                                              (send ba)))
                                          (recur)))))
                                  (catch Throwable _ nil)
                                  (finally (drop! ch unsub))))
                           "vis-pty-bridge-client")
              (.setDaemon true)
              (.start))))

        accept
        (fn []
          (try (loop []

                 (when @running
                   (when-let [ch (.accept server)]
                     (on-client ch))
                   (recur)))
               (catch Throwable _ nil)))]

    (doto (Thread. ^Runnable accept "vis-pty-bridge-accept") (.setDaemon true) (.start))
    {:path (str p)
     :stop (fn []
             (reset! running false)
             (doseq [^SocketChannel ch @clients]
               (try (.close ch) (catch Throwable _ nil)))
             (reset! clients #{})
             (try (.close server) (catch Throwable _ nil))
             (try (Files/deleteIfExists p) (catch Throwable _ nil)))}))

;; =============================================================================
;; Client — the human's raw-terminal attach (`vis ext shell attach <id>`)
;; =============================================================================

(def ^:private detach-byte
  "Ctrl-] (GS, 0x1d) — the telnet-style detach key. Leaves the child running."
  0x1d)

(defn- stty
  "Run `stty` against the controlling terminal (INHERITs our stdin, which is the
   human's tty), returning trimmed stdout. Best-effort: nil on any failure."
  [& args]
  (try (let [pb
             (doto (ProcessBuilder. ^java.util.List (into-array String (cons "stty" args)))
               (.redirectInput java.lang.ProcessBuilder$Redirect/INHERIT)
               (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT))

             proc
             (.start pb)

             out
             (slurp (.getInputStream proc))]

         (.waitFor proc)
         (str/trim out))
       (catch Throwable _ nil)))

(defn attach!
  "Human-side passthrough: connect to a background shell's AF_UNIX socket, put the
   local terminal into raw mode, and shuffle stdin<->socket until Ctrl-] (detach)
   or EOF. Restores the terminal on exit. `opts` is `{:id <shell-id>}` or
   `{:socket <path>}`. Returns an exit code (0 ok, 2 = no such socket)."
  [opts]
  (let [p (find-socket opts)]
    (if (or (nil? p) (not (Files/exists p (make-array LinkOption 0))))
      (do (binding [*out* *err*]
            (println (str "vis: no live background shell socket for "
                          (or (:socket opts) (:id opts) "?")
                          " — is it running? (see `resources` / shell_bg output)")))
          2)
      (let [ch (SocketChannel/open (UnixDomainSocketAddress/of p))
            saved (stty "-g")
            restore (fn []
                      (when saved (stty saved)))]

        (println (str "vis: attached to " p " — press Ctrl-] to detach (child keeps running)."))
        (flush)
        (try (stty "raw" "-echo")
             (let [^OutputStream out System/out
                   _pump (doto (Thread. ^Runnable
                                        (fn []
                                          (try (let [buf (ByteBuffer/allocate 4096)]
                                                 (loop []

                                                   (.clear buf)
                                                   (let [n (.read ch buf)]
                                                     (when (>= n 0)
                                                       (when (pos? n)
                                                         (.flip buf)
                                                         (let [ba (byte-array n)]
                                                           (.get buf ba)
                                                           (.write out ba)
                                                           (.flush out)))
                                                       (recur)))))
                                               (catch Throwable _ nil)))
                                        "vis-pty-attach-out")
                           (.setDaemon true)
                           (.start))
                   ^InputStream in System/in]

               (loop []

                 (let [c (.read in)]
                   (cond (or (= c -1) (= c detach-byte)) nil
                         :else (do (write-all! ch (byte-array [(unchecked-byte c)])) (recur)))))
               0)
             (finally (restore)
                      (try (.close ch) (catch Throwable _ nil))
                      (println "\r\nvis: detached.")
                      (flush)))))))
