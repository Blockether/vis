(ns com.blockether.vis.docs-dev
  "Docs dev: serve the gateway + auto-reload on save, inside THIS (dev nREPL)
   JVM. Entry point: `bin/dev docs` → `serve-docs!` + `start-watcher!`."

  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.server :as gw]
            [taoensso.telemere :as tel])
  (:import (java.nio.file FileSystems Path StandardWatchEventKinds WatchService)
           (java.nio.file WatchEvent WatchKey)))

;; namespace reload — plain `require :reload` of the changed file

(defn- file->namespace
  [^Path f]
  (let [s (.toString f)]
    (some (fn [root]
            (let [seg (str "/" root "/")]
              (when-let [i (str/index-of s seg)]
                (let [trimmed (-> (subs s (+ i (count seg)))
                                (str/replace #"\.cljc?$" "")
                                (str/replace #"/" ".")
                                (str/replace #"_" "-"))]
                  (when (seq trimmed)
                    (symbol trimmed))))))
          ["src" "dev"])))

(defn- reload-namespace!
  [ns-sym]
  (try
    (clojure.core/require ns-sym :reload)
    true
    (catch Throwable t
      (tel/log! :error ["docs-dev: reload FAILED (save again after fixing)"
                        (str ns-sym) (ex-message t)])
      false)))

;; gateway — served IN this JVM

(defn serve-docs!
  ([]
   (serve-docs! {}))
  ([{:keys [port host]
     :or   {port 7890
            host "0.0.0.0"}}]
   (if (gw/running?)
     (do
       (tel/log! :info ["docs-dev: gateway already running in this JVM; reusing"])
       {:port port :host host})
     (do
       (gw/start! {:port port :host host})
       (tel/log! :info ["docs-dev: gateway serving docs in this JVM"
                        (str "http://0.0.0.0:" port "/docs")])
       {:port port :host host}))))

;; file watcher — direct JDK WatchService on a dedicated daemon thread

(def ^:private ^WatchService watch-service* (atom nil))
(def ^:private ^Thread watch-thread* (atom nil))
(def ^:private running?* (atom false))

(defn- handle-change!
  [^Path changed]
  (let [s   (.toString changed)
        ext (re-find #"\.[^.]+$" s)]
    (cond
      (#{".clj" ".cljc"} ext)
      (when-let [ns-sym (file->namespace changed)]
        (when (reload-namespace! ns-sym)
          (tel/log! :info ["docs-dev: reloaded" (str ns-sym) "— refresh the browser"])))

      (str/includes? s "vis-docs/")
      (tel/log! :info ["docs-dev: docs markdown/asset changed — refresh the browser"]))))

(defn- register-tree!
  [^WatchService ws ^Path dir-path]
  (.register dir-path ws
    (into-array java.nio.file.WatchEvent$Kind
      [StandardWatchEventKinds/ENTRY_MODIFY
       StandardWatchEventKinds/ENTRY_CREATE])))

(defn- watch-loop!
  [^WatchService ws ^Path internal-root]
  (try
    (loop []
      (when @running?*
        (let [^WatchKey key (.take ws)]
          (when @running?*
            (doseq [^WatchEvent ev (.pollEvents key)]
              (let [kind (.kind ev)
                    changed (.resolve internal-root ^Path (.context ev))]
                (when (not= kind StandardWatchEventKinds/OVERFLOW)
                  (handle-change! changed))))
            (.reset key)
            (recur)))))
    (catch java.nio.file.ClosedWatchServiceException _
      :closed)
    (catch Throwable t
      (tel/log! :error ["docs-dev: watcher loop error" (ex-message t)]))))

(defn start-watcher!
  []
  (when-not @watch-service*
    (let [ws (.newWatchService (FileSystems/getDefault))
          internal (.toPath (.getCanonicalFile (io/file "src/com/blockether/vis/internal")))
          vis-docs (.toPath (.getCanonicalFile (io/file "resources/vis-docs")))]
      (register-tree! ws internal)
      (register-tree! ws vis-docs)
      (reset! running?* true)
      (let [t (doto (Thread. ^Runnable (fn [] (watch-loop! ws internal))
                    "vis-docs-watcher")
                (.setDaemon true))]
        (.start t)
        (reset! watch-thread* t))
      (reset! watch-service* ws)
      (tel/log! :info ["docs-dev: file watcher armed — save a file, refresh the browser"])
      ws)))

(defn stop-watcher!
  []
  (reset! running?* false)
  (when-let [^WatchService ws @watch-service*]
    (try (.close ws) (catch Throwable _ nil))
    (reset! watch-service* nil))
  (reset! watch-thread* nil)
  :stopped)

(defn stop-docs!
  []
  (stop-watcher!)
  (gw/stop!)
  :stopped)
