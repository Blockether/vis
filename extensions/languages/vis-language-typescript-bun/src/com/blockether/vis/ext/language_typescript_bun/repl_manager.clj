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
            [clojure.string :as str]
            [com.blockether.vis.ext.language-typescript-bun.runner :as runner]
            [com.blockether.vis.core :as vis])
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

(def ^:private stderr-tail-lines
  "How many of the child's last stderr lines a failed start hands back."
  40)

(defn- drain-stderr!
  "Pump the child's stderr into a BOUNDED tail of line strings on a daemon
   thread, answering `{:lines atom :done promise}`.

   Nothing else reads that pipe: left unread it fills and the child BLOCKS
   mid-write, and once the process is reaped the JDK closes the stream, so a
   failed start reading it AFTERWARDS gets `Stream closed` and loses exactly the
   words that explain the failure. Pumping from the start keeps both safe."
  [^java.io.BufferedReader reader]
  (let
    [lines
     (atom [])

     done
     (promise)]

    (doto (Thread. ^Runnable
                   (fn []
                     (try (loop []
                            (when-let [line (.readLine reader)]
                              (swap! lines (fn [ls] (vec (take-last stderr-tail-lines (conj ls line)))))
                              (recur)))
                          (catch Throwable _ nil)
                          (finally (deliver done true)))))
      (.setDaemon true)
      (.setName "vis-repl-stderr-pump")
      (.start))
    {:lines lines :done done}))

(defn- stderr-tail
  "What the child printed to stderr, as line strings — waiting BRIEFLY for the
   pump to reach EOF, so a just-exited child's last words are not lost to a
   race."
  [info]
  (let [{:keys [lines done]} (:stderr info)]
    (when lines
      (deref done 500 nil)
      (vec (remove str/blank? @lines)))))

(defn status
  "STRING-keyed lifecycle view (crosses as a tool `:result`): `result`, `cwd`,
   `status`, plus `running` / `pid` / `cmd` / `env` while the runtime is up.
   A key appears only where it MEANS something — a down REPL has no pid and no
   command — which is the shape every language answers. `env` is the delta this
   REPL was STARTED with, by NAME and digest only, and never a value."
  [dir]
  (let
    [info
     (get @processes dir)

     running?
     (alive? info)]

    (cond->
      {"result" "status"
       "cwd" dir
       "status" (if running? "up" "down")}

      running?
      (assoc "running" true)

      running?
      (assoc "pid" (some-> ^Process (:process info)
                           .pid))

      (and running? (:cmd info))
      (assoc "cmd" (:cmd info))

      (and running? (seq (:env-fingerprint info)))
      (assoc "env" (:env-fingerprint info)))))

(defn- request!
  [dir req timeout-ms]
  (let [info (get @processes dir)]
    (when-not (alive? info)
      (throw (ex-info "Bun REPL is not running for this dir — repl_start(\"typescript\") first."
                      {:type :ts/no-repl :dir dir})))
    (locking info
      (let
        [^BufferedWriter w (:writer info)
         ^BufferedReader r (:reader info)]

        (.write w (str (json/write-json-str req) "\n"))
        (.flush w)
        (let
          [fut (future (.readLine r))
           line (deref fut timeout-ms ::timeout)]

          (if (= line ::timeout)
            (do (future-cancel fut)
                (throw (ex-info "Bun eval timed out" {:type :ts/timeout :dir dir})))
            (if (nil? line)
              (throw (ex-info "Bun REPL closed the connection (process died)"
                              {:type :ts/closed :dir dir}))
              (json/read-json line))))))))

(defn- spawn!
  "Replace whatever is cached for `dir` with a freshly spawned Bun runtime,
   stamped with the env delta it was started with. Only `start!` calls this: a
   LIVE REPL is reused, never respawned."
  [dir {:keys [session-id] :as opts} env-fingerprint]
  (when-let [old (get @processes dir)]
    (try (.destroy ^Process (:process old)) (catch Throwable _ nil)))
  (let
    [cmd
     (conj (runner/resolve-command dir) "-e" (server-script))

     ;; Resolve argv + proxy env atomically. Unknown/disposed sessions are denied
     ;; before spawn, and direct outbound traffic remains behind Seatbelt.
     launch
     (vis/session-process-launch session-id cmd {:env (get opts "env")})

     pb
     (doto (ProcessBuilder. ^java.util.List (:argv launch))
       (.directory (io/file dir))
       (.redirectErrorStream false))

     _env
     (let [^java.util.Map e (.environment ^ProcessBuilder pb)]
       (when (:replace-env? launch) (.clear e))
       (doseq [[k v] (:env launch)]
         (.put e ^String k ^String v))
       ;; An inherited environment still carries what this start asked to UNSET.
       (doseq [k (:env-remove launch)]
         (.remove e ^String k)))

     p
     (.start pb)

     info
     {:process p
      :writer (io/writer (.getOutputStream p))
       :reader (io/reader (.getInputStream p))
       :stderr (drain-stderr! (io/reader (.getErrorStream p)))
      :cmd (display-cmd cmd)
      :pid (.pid p)
      :started-at (System/currentTimeMillis)
      ;; WHAT THIS REPL RUNS WITH, by name and digest: what the next start is
      ;; compared against, so a reuse can be refused without a value ever
      ;; reaching a result, a log or the transcript.
      :env-fingerprint env-fingerprint}]

    (swap! processes assoc dir info)
    ;; "up" only once the child ANSWERS — the same handshake the Python REPL
    ;; makes, so a runtime that died on its first line can never masquerade as
    ;; a usable REPL.
    (try (let [ping (request! dir {"op" "ping"} 10000)]
           (if (true? (get ping "pong"))
             (assoc (status dir) "result" "started")
             (throw (ex-info "Bun REPL did not acknowledge its startup ping"
                             {:type :ts/bad-handshake :response ping}))))
         (catch Throwable e
           (try (.destroy p) (catch Throwable _ nil))
           (try (.waitFor p) (catch Throwable _ nil))
           (try (when (.isAlive p) (.destroyForcibly p)) (catch Throwable _ nil))
           (let
             [exit-code
              (try (.exitValue p) (catch Throwable _ nil))

              tail
              (stderr-tail info)]

             (swap! processes dissoc dir)
             (cond->
               {"result" "failed"
                "status" "failed"
                "pid" (.pid p)
                "cmd" (display-cmd cmd)
                "cwd" dir
                "message" (str "Bun REPL failed its startup handshake: " (.getMessage e))}

               exit-code
               (assoc "exit" exit-code)

               (seq tail)
               (assoc "log_tail" tail)))))))

(defn start!
  "Start the managed Bun REPL for `dir` — or REUSE the one already running.

   A live REPL is NEVER silently replaced: its globals ARE the session's work,
   so a second start answers \"already-running\", exactly as every other Vis
   language answers it. A start succeeds only after the runtime answers its
   protocol ping; failed children never masquerade as usable REPLs.

   `opts` carries THIS start's own `env` delta over the project environment (the
   `env` key, string-keyed like every other model-facing option), and that env
   BELONGS to the REPL: a start naming a different one is refused by the keys
   that differ, because there is no restart — stop it, then start it.

   Returns a STRING-keyed lifecycle map."
  [dir opts]
  (let
    [id
     (or (get opts "id") (str "bunrepl:" dir))

     env-fingerprint
     (vis/env-fingerprint (vis/call-env-values (get opts "env")))]

    (if (alive? (get @processes dir))
      (let [refusal (vis/env-mismatch-refusal id
                                              (:env-fingerprint (get @processes dir))
                                              env-fingerprint)]
        (when refusal
          (throw (ex-info (:message refusal)
                          {:type :ts/repl-env-mismatch
                           :id id
                           :env (:differing refusal)})))
        (assoc (status dir) "result" "already-running"))
      (spawn! dir opts env-fingerprint))))

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
  "Stop THIS dir's managed runtime. No-op-safe: with nothing managed the result
   says `not-managed` rather than claiming a stop that never happened."
  [dir]
  (let [info (get @processes dir)]
    (when info
      (try (.destroy ^Process (:process info)) (catch Throwable _ nil))
      (try (when (.isAlive ^Process (:process info)) (.destroyForcibly ^Process (:process info)))
           (catch Throwable _ nil)))
    (swap! processes dissoc dir)
    {"result" (if info "stopped" "not-managed") "cwd" dir "status" "down"}))

