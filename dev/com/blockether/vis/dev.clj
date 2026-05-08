(ns com.blockether.vis.dev
  "Developer entry point.

   `clojure -M:dev` starts an nREPL for LLM/editor driven work, writes
   `.nrepl-port`, and keeps the JVM alive. From another shell use
   `clj-nrepl-eval` to call the helper fns in this namespace:

     clj-nrepl-eval -p 7888 '(require '[com.blockether.vis.dev :as dev] :reload) (dev/tui!)'
     clj-nrepl-eval -p 7888 '(require '[com.blockether.vis.dev :as dev] :reload) (dev/cli! \"providers\" \"list\")'

   The TUI helper opens macOS Terminal.app and starts a dev JVM there;
   inside that one JVM, nREPL and the TUI run together so the TUI process
   is controllable from nREPL."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [com.blockether.vis.internal.crac-bootstrap :as crac-bootstrap]
            [com.blockether.vis.internal.main :as binary]
            [nrepl.server :as nrepl]))

(def ^:private default-port 7888)
(def ^:private nrepl-port-file ".nrepl-port")

(defonce ^:private server* (atom nil))
(defonce ^:private port* (atom nil))
(defonce ^:private shutdown-hook-installed?* (atom false))

(defn repo-root
  "Return the repository root used by the current JVM."
  []
  (.getCanonicalPath (io/file (System/getProperty "user.dir"))))

(defn- parse-port [s]
  (when (seq (str/trim (str s)))
    (try
      (Integer/parseInt (str/trim (str s)))
      (catch NumberFormatException _ nil))))

(defn- requested-port []
  (or (parse-port (System/getenv "NREPL_PORT"))
    default-port))

(defn- bind-exception? [^Throwable t]
  (boolean
    (some #(instance? java.net.BindException %)
      (take-while some? (iterate ex-cause t)))))

(defn- write-port-file! [port]
  (spit (io/file (repo-root) nrepl-port-file) (str port "\n")))

(defn- delete-port-file! []
  (try
    (io/delete-file (io/file (repo-root) nrepl-port-file) true)
    (catch Throwable _ nil)))

(defn stop-nrepl!
  "Stop the dev nREPL server and remove `.nrepl-port`."
  []
  (when-let [server @server*]
    (nrepl/stop-server server)
    (reset! server* nil)
    (reset! port* nil)
    (delete-port-file!)
    :stopped))

(defn- install-shutdown-hook! []
  (when (compare-and-set! shutdown-hook-installed?* false true)
    (.addShutdownHook (Runtime/getRuntime)
      (Thread. ^Runnable (fn [] (stop-nrepl!)) "vis-dev-nrepl-shutdown"))))

(defn start-nrepl!
  "Start the dev nREPL server.

   Options:
   - `:port` - bind port; defaults to `$NREPL_PORT` or 7888.
   - `:fallback-random?` - if true (default), use an ephemeral port when
     the requested port is already bound.

   Writes `.nrepl-port` so `clj-nrepl-eval --discover-ports` can find it."
  ([] (start-nrepl! {}))
  ([{:keys [port fallback-random?]
     :or   {fallback-random? true}}]
   (if @server*
     {:status :already-running
      :host   "127.0.0.1"
      :port   @port*}
     (let [port (or port (requested-port))
           start (fn [p]
                   (let [server (nrepl/start-server :bind "127.0.0.1" :port p)
                         actual (:port server)]
                     (reset! server* server)
                     (reset! port* actual)
                     (write-port-file! actual)
                     (install-shutdown-hook!)
                     {:status :started
                      :host   "127.0.0.1"
                      :port   actual}))]
       (try
         (start port)
         (catch Throwable t
           (if (and fallback-random? (bind-exception? t))
             (do
               (println (str "vis dev: port " port " is busy; falling back to an ephemeral nREPL port."))
               (start 0))
             (throw t))))))))

(defn nrepl-status
  "Return current nREPL status."
  []
  (if @server*
    {:status :running :host "127.0.0.1" :port @port*}
    {:status :stopped}))

(defn- sh-quote [s]
  (str "'" (str/replace (str s) "'" "'\\''") "'"))

(defn- applescript-string [s]
  (str "\""
    (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\""))
    "\""))

(defn- macos? []
  (str/includes? (str/lower-case (System/getProperty "os.name")) "mac"))

(defn- bin-vis []
  (.getCanonicalPath (io/file (repo-root) "bin" "vis")))

(defn- bin-dev []
  (.getCanonicalPath (io/file (repo-root) "bin" "dev")))

(defn cli!
  "Run `bin/vis` safely in a subprocess and return `{:exit :out :err}`.

   Intended for `clj-nrepl-eval`:

     (require '[com.blockether.vis.dev :as dev] :reload)
     (dev/cli! \"providers\" \"list\")"
  [& args]
  (apply sh/sh
    (concat [(bin-vis)] (map str args) [:dir (repo-root)])))

(defn- cli-inherit-io! [args]
  (let [pb (ProcessBuilder. ^java.util.List (into [(bin-vis)] (map str args)))]
    (.directory pb (io/file (repo-root)))
    (.inheritIO pb)
    (let [p (.start pb)]
      (.waitFor p))))

(defn in-process-tui!
  "Run `vis channels tui` in THIS JVM/process.

   This only works correctly when this JVM already owns the terminal that
   should display the TUI. For a separate Terminal.app window with nREPL
   control, use `tui!` / `bin/dev tui` instead."
  [& args]
  (apply binary/-main "channels" "tui" (map str args)))

(defn tui!
  "Open TUI in macOS Terminal.app as one attached dev JVM.

   The Terminal.app command runs `bin/dev terminal-tui ...`: that child JVM
   starts nREPL and then runs `vis channels tui` in the same process. Extra
   args are passed after `vis channels tui`, e.g. `(tui! \"--resume\")`."
  [& args]
  (when-not (macos?)
    (throw (ex-info "dev/tui! currently opens macOS Terminal.app; run `bin/dev terminal-tui` manually on this OS."
             {:os (System/getProperty "os.name")})))
  (let [cmd (str "cd " (sh-quote (repo-root))
              " && " (sh-quote (bin-dev)) " terminal-tui"
              (when (seq args)
                (str " " (str/join " " (map sh-quote args)))))
        script (str "tell application \"Terminal\"\n"
                 "  activate\n"
                 "  do script " (applescript-string cmd) "\n"
                 "end tell\n")
        result (sh/sh "osascript" "-e" script)]
    (assoc result :command cmd)))

(defn banner
  "Text printed by the dev launcher after nREPL startup."
  [{:keys [host port]}]
  (str "\nvis dev nREPL listening on " host ":" port "\n"
    "Port file: " (str (io/file (repo-root) nrepl-port-file)) "\n\n"
    "LLM/editor commands:\n"
    "  clj-nrepl-eval --discover-ports\n"
    "  clj-nrepl-eval -p " port " \"(+ 1 2 3\"\n"
    "  clj-nrepl-eval -p " port " \"(require '[com.blockether.vis.dev :as dev] :reload) (dev/cli! \\\"providers\\\" \\\"list\\\")\"\n"
    "  clj-nrepl-eval -p " port " \"(require '[com.blockether.vis.dev :as dev] :reload) (dev/tui!)\"\n\n"
    "Paren repair:\n"
    "  clj-paren-repair path/to/file.clj\n\n"
    "Keep this process alive; Ctrl-C stops nREPL.\n"))

(defn- usage []
  (str "vis dev launcher\n\n"
    "Usage:\n"
    "  clojure -M:dev [nrepl]             Start nREPL and wait forever.\n"
    "  clojure -M:dev tui [ARGS...]       Open Terminal.app running attached nREPL+TUI JVM.\n"
    "  clojure -M:dev terminal-tui [...]  Internal: start nREPL, run TUI in this JVM.\n"
    "  clojure -M:dev cli [ARGS...]       Start nREPL, run `bin/vis ARGS...`, then wait forever.\n"
    "  bin/dev ...                        Wrapper around the same alias.\n\n"
    "Env:\n"
    "  NREPL_PORT=7888               Override bind port.\n"))

(defn- wait-forever! []
  @(promise))

(defn -main [& args]
  (crac-bootstrap/pre-extension-bootstrap! {:phase :dev})
  (let [[cmd & more] args]
    (case cmd
      (nil "nrepl")
      (do
        (println (banner (start-nrepl!)))
        (wait-forever!))

      "tui"
      (let [{:keys [exit err command]} (apply tui! more)]
        (if (zero? exit)
          (println (str "Opened Terminal.app attached dev JVM: " command))
          (do
            (binding [*out* *err*] (println err))
            (System/exit exit))))

      "terminal-tui"
      (do
        (println (banner (start-nrepl!)))
        (apply in-process-tui! more)
        (wait-forever!))

      "cli"
      (do
        (println (banner (start-nrepl!)))
        (let [exit (cli-inherit-io! more)]
          (when-not (zero? exit)
            (println (str "vis dev: CLI exited " exit))))
        (wait-forever!))

      ("help" "--help" "-h")
      (println (usage))

      (do
        (binding [*out* *err*]
          (println (usage))
          (println (str "Unknown dev command: " cmd)))
        (System/exit 2)))))
