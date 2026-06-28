(ns com.blockether.vis.docs-dev
  "Docs development helper: serve the gateway + auto-reload on save, all
   inside THIS (dev nREPL) JVM.

   Why a separate JVM was painful: the gateway normally runs in its own
   process. Markdown already live-reloads (the handler re-reads
   `resources/vis-docs/*.md` per request), but CSS/markup edits in
   `docs.clj` are compiled Clojure — a separate JVM needs a full restart
   (~40s) to see them, and a restart drops the cloudflared tunnel.

   The fix is to serve the gateway IN the dev JVM and recompile on save:
   `gateway.server` calls `docs/handle` through the symbol (a #'var), so
   force-reloading the `docs` namespace makes the running server serve the
   new code on the next request — no restart, no tunnel dance.

   Entry point: `bin/dev docs` → `serve-docs!` + `start-watcher!`.

   This namespace is :dev-only — it never reaches the GraalVM binary, and
   neither does its `directory-watcher` dep."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.gateway.server :as gw]
            [taoensso.telemere :as tel])
  (:import (io.methvin.watcher DirectoryWatcher DirectoryChangeListener)
           (io.methvin.watcher DirectoryChangeEvent$EventType)
           (java.nio.file Path)))

;; -----------------------------------------------------------------------------
;; namespace reload — plain `require :reload` of the changed file
;; -----------------------------------------------------------------------------

;; NOTE on why NOT clj-reload here: clj-reload diffs files against a baseline
;; captured at `init` time. A file watcher's baseline-race is fragile — if
;; `init` runs after an edit already landed, clj-reload reports "nothing to
;; reload" even though the live namespace is stale (verified in practice).
;; For a watcher we ALREADY know which file changed (the OS told us), so the
;; robust move is plain `clojure.core/require :reload` of exactly that
;; namespace. Simple, always-correct, no baseline state to drift.

(defn- file->namespace
  "Map a `.../src/...` or `.../dev/...` source file path to its namespace
   symbol, or nil if it isn't under a source root."
  [^java.nio.file.Path f]
  (let [s (.toString f)]
    (some (fn [root]
            (when (str/starts-with? s root)
              (let [trimmed (-> (subs s (count root))
                              (str/replace #"\.cljc?$" "")
                              (str/replace #"/" ".")
                              (str/replace #"_" "-"))]
                (when (seq trimmed)
                  (symbol trimmed)))))
          ["src/" "dev/"])))

(defn- reload-namespace!
  "Force-reload one namespace from source. Returns true if it compiled clean,
   false if the save had a syntax error (caught so a typo doesn't kill the
   watcher — fix it, save again, it recovers)."
  [ns-sym]
  (try
    (clojure.core/require ns-sym :reload)
    true
    (catch Throwable t
      (tel/log! :error ["docs-dev: reload FAILED (save again after fixing)"
                        (str ns-sym) (ex-message t)])
      false)))

;; -----------------------------------------------------------------------------
;; gateway — served IN this JVM
;; -----------------------------------------------------------------------------

(defn serve-docs!
  "Start the docs gateway INSIDE this (dev nREPL) JVM so `clj-reload` can
   recompile `docs.clj` and the running server serves it instantly — no
   restart. Idempotent: if the gateway is already running in this JVM, it's a
   no-op.

   Options:
     :port (default 7890)
     :host (default 127.0.0.1)

   Returns `{:port :host}`. Throws if the gateway is already running and the
   port differs — stop it first with `stop-docs!`."
  ([]
   (serve-docs! {}))
  ([{:keys [port host]
     :or   {port 7890
            host "127.0.0.1"}}]
   (if (gw/running?)
     (do
       (tel/log! :info ["docs-dev: gateway already running in this JVM; reusing"])
       {:port port :host host})
     (do
       (gw/start! {:port port :host host})
       (tel/log! :info ["docs-dev: gateway serving docs in this JVM"
                        (str "http://" host ":" port "/docs")])
       {:port port :host host}))))

;; -----------------------------------------------------------------------------
;; file watcher — native OS events, recompiles docs.clj on save
;; ----------------------------------------------------------------------------

(def ^:private ^DirectoryWatcher watcher* (atom nil))

(defn- handle-change!
  "Dispatch one filesystem event. A `.clj` under src/ or dev/ → force-reload
   that namespace from source (the running gateway then serves the new code on
   the next request, since it calls `docs/handle` through the #'var). Anything
   under resources/vis-docs/ is markdown/assets — already live per request — so
   just nudge. Editors that multi-write on save are fine: `:reload` is cheap
   and idempotent for an unchanged file."
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

(defn start-watcher!
  "Watch the docs source dirs for saves and auto-recompile. Native OS file
   events (no polling). Idempotent: if already watching, returns the existing
   watcher. Watches:
     src/com/blockether/vis/internal/   — docs.clj edits
     resources/vis-docs/                — markdown + assets (live per-request)"
  []
  (when-not @watcher*
    (let [internal (.toPath (.getCanonicalFile (io/file "src/com/blockether/vis/internal")))
          vis-docs (.toPath (.getCanonicalFile (io/file "resources/vis-docs")))
          listener (reify DirectoryChangeListener
                     (onEvent [_ e]
                       (when (and (not (.isDirectory e))
                                  (let [et (.eventType e)]
                                    (or (= et DirectoryChangeEvent$EventType/MODIFY)
                                        (= et DirectoryChangeEvent$EventType/CREATE))))
                         (handle-change! (.path e)))))]
      (let [^DirectoryWatcher w (-> (DirectoryWatcher/builder)
                                    (.paths [internal vis-docs])
                                    (.listener listener)
                                    (.build))]
        ;; watchAsync runs on its own daemon thread; keep the future so
        ;; stop-docs! can .close the watcher (completes the future).
        (.watchAsync w)
        (reset! watcher* w)
        (tel/log! :info ["docs-dev: file watcher armed (native) — save a file, refresh the browser"])
        w))))

(defn stop-watcher!
  "Stop the file watcher if running. Idempotent."
  []
  (when-let [^DirectoryWatcher w @watcher*]
    (try (.close w) (catch Throwable _ nil))
    (reset! watcher* nil)
    :stopped))

(defn stop-docs!
  "Stop the watcher and the gateway. Idempotent."
  []
  (stop-watcher!)
  (gw/stop!)
  :stopped)
