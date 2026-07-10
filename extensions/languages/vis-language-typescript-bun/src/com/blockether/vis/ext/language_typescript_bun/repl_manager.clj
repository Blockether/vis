(ns com.blockether.vis.ext.language-typescript-bun.repl-manager
  "A MANAGED TypeScript (Bun) REPL: a persistent `bun` subprocess running a
   line-framed JSON eval server — one request per stdin line ({\"code\": ...}
   or {\"op\": \"ping\"}), one response per stdout line
   ({ok, out, err, value, data, type, exc}). The server (a .js resource shipped
   with this pack, passed via `bun -e`) rewrites top-level const/let/var/
   function/class declarations to globalThis assignments and imports to dynamic
   `await import`, so REPL globals PERSIST across evals and top-level await just
   works. stdout/stderr are captured permanently server-side, so a live
   system's async logs ride back on the next response instead of corrupting the
   protocol. One process per dir; the `Process` handle is cached so teardown is
   clean."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.ext.language-typescript-bun.runner :as runner])
  (:import [java.io BufferedReader BufferedWriter]))

(def ^:private server-resource "com/blockether/vis/ext/typescript_bun/repl_server.js")

(defn- server-script
  ^String []
  (if-let [r (io/resource server-resource)]
    (slurp r)
    (throw (ex-info "repl_server.js missing from the classpath"
                    {:type :ts/no-server-resource :resource server-resource}))))

;; dir -> {:process ^Process :writer :reader :cmd :pid :started-at}
(defonce ^:private processes (atom {}))

(defn- alive?
  [info]
  (boolean (some-> ^Process (:process info)
                   .isAlive)))

(defn- display-cmd
  "The launch argv with the inlined server source elided — this rides into the
   resource registry / footer, where a 12KB arg would be noise."
  [cmd]
  (mapv (fn [^String arg]
          (if (< 200 (count arg)) "<repl_server.js>" arg))
        cmd))

(defn start!
  "Spawn (or replace) the managed Bun REPL for `dir`. Returns a STRING-keyed
   status map (crosses the strings-only boundary as a tool `:result`)."
  [dir _opts]
  (when-let [old (get @processes dir)]
    (try (.destroy ^Process (:process old)) (catch Throwable _ nil)))
  (let [cmd
        (conj (runner/resolve-command dir) "-e" (server-script))

        pb
        (doto (ProcessBuilder. ^java.util.List cmd)
          (.directory (io/file dir))
          (.redirectErrorStream false))

        p
        (.start pb)

        info
        {:process p
         :writer (io/writer (.getOutputStream p))
         :reader (io/reader (.getInputStream p))
         :cmd (display-cmd cmd)
         :pid (.pid p)
         :started-at (System/currentTimeMillis)}]

    (swap! processes assoc dir info)
    {"status" "up" "pid" (.pid p) "cmd" (display-cmd cmd) "dir" dir}))

(defn- request!
  [dir req timeout-ms]
  (let [info (get @processes dir)]
    (when-not (alive? info)
      (throw (ex-info "Bun REPL is not running for this dir — repl_start(\"typescript\") first."
                      {:type :ts/no-repl :dir dir})))
    (locking info
      (let [^BufferedWriter w (:writer info)
            ^BufferedReader r (:reader info)]

        (.write w (str (json/write-json-str req) "\n"))
        (.flush w)
        (let [fut (future (.readLine r))
              line (deref fut timeout-ms ::timeout)]

          (if (= line ::timeout)
            (do (future-cancel fut)
                (throw (ex-info "Bun eval timed out" {:type :ts/timeout :dir dir})))
            (if (nil? line)
              (throw (ex-info "Bun REPL closed the connection (process died)"
                              {:type :ts/closed :dir dir}))
              (json/read-json line))))))))

(defn eval!
  "Evaluate `code` (TypeScript or JavaScript) in the REPL for `dir`. Returns
   {\"ok\" \"out\" \"err\" \"value\" \"data\" \"type\" \"exc\"} — `value` is the
   last expression's inspect string, `data` its JSON-safe STRUCTURED view
   (objects/Maps/Sets/class instances, so the model can read real fields),
   `type` the constructor name. A `reload(path)` global does a cache-busted
   re-import of a project module."
  [dir code timeout-ms]
  (request! dir {"code" (str code)} (or timeout-ms 30000)))

(defn stop!
  [dir]
  (when-let [info (get @processes dir)]
    (try (.destroy ^Process (:process info)) (catch Throwable _ nil))
    (try (when (.isAlive ^Process (:process info)) (.destroyForcibly ^Process (:process info)))
         (catch Throwable _ nil)))
  (swap! processes dissoc dir)
  {"status" "stopped" "dir" dir})

(defn status
  "STRING-keyed lifecycle view (crosses as a tool `:result`)."
  [dir]
  (let [info (get @processes dir)]
    {"dir" dir
     "status" (if (alive? info) "up" "down")
     "pid" (some-> ^Process (:process info)
                   .pid)
     "cmd" (:cmd info)}))
