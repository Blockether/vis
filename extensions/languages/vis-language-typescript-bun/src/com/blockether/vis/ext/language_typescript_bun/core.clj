(ns com.blockether.vis.ext.language-typescript-bun.core
  "vis-language-typescript-bun — a managed TypeScript/Bun REPL exposed through
   the generic language facade (repl_start / repl_eval / repl_stop) plus
   run_tests -> `bun test`. Activates only when the workspace looks like a Bun
   project. The REPL is a persistent `bun` subprocess running a line-framed
   JSON eval server with REAL REPL semantics (persistent globals, top-level
   await, cache-busted `reload(path)`), registered as a session resource so it
   shows in ctx + the footer and is stoppable by id. The point is the Clojure
   reloaded workflow for TS: the app STARTS from the repl, lives in the repl,
   and its state is printable — no debugger needed."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.language-typescript-bun.repl-manager :as repl]
            [com.blockether.vis.ext.language-typescript-bun.runner :as runner]
            [com.blockether.vis.internal.extension :as extension]))

;; =============================================================================
;; Activation
;; =============================================================================

(defn- ts-file?
  [^java.io.File f]
  (and (.isFile f)
       (let [n (.getName f)]
         (or (str/ends-with? n ".ts") (str/ends-with? n ".tsx")))))

(defn- workspace-has-bun?
  [env]
  (let [root (some-> (:workspace/root env)
                     io/file)]
    (when (and root (.isDirectory root))
      (or (some #(.exists (io/file root %)) ["bunfig.toml" "bun.lock" "bun.lockb" ".bun-version"])
          ;; a generic package.json workspace with TS sources runs on bun too
          ;; (bounded scan for a .ts/.tsx anywhere, lazy file-seq, capped)
          (and (.exists (io/file root "package.json"))
               (boolean (some ts-file? (take 3000 (file-seq root)))))))))

(defn- activation-fn [env] (boolean (workspace-has-bun? env)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- env-root
  ^String [env]
  (or (:workspace/root env)
      (throw (ex-info "typescript tool fired without :workspace/root in env"
                      {:type :ts/no-workspace}))))

(defn- resolve-dir
  ^String [root dir]
  (let [d (str (or dir ""))]
    (.getCanonicalPath (cond (= "" d) (io/file root)
                             (.isAbsolute (io/file d)) (io/file d)
                             :else (io/file root d)))))

(defn- repl-resource-id
  [dir id]
  (let [id (some-> id
                   str
                   str/trim)]
    (if (seq id) id (str "bunrepl:" dir))))

(defn register-repl-resource!
  "Mirror a managed Bun REPL into the session resource registry (ctx + footer +
   stop/restart by id). No-op without a session or a live pid."
  [session dir result & [id]]
  ;; `result` is repl/start!'s STRING-keyed lifecycle map. The resource map is
  ;; the CENTRAL resources.clj DATA shape (keyword keys), but `:detail` is
  ;; passed THROUGH verbatim, so it must already be STRING-keyed.
  (when (and session (get result "pid"))
    (vis/register-resource! session
                            {:id (repl-resource-id dir id)
                             :kind :repl
                             :label (str "bun REPL " (.getName (io/file dir)))
                             :status (or (get result "status") :up)
                             :detail {"dir" dir "cmd" (get result "cmd")}
                             :pid (get result "pid")
                             :owner :ext/language-typescript-bun
                             :language :typescript}
                            {:stop-fn (fn []
                                        (repl/stop! dir))
                             :restart-fn (fn []
                                           (repl/stop! dir)
                                           (let [r (repl/start! dir {})]
                                             (register-repl-resource! session dir r id)
                                             r))})
    (vis/notify! (str "● bun REPL up — " (.getName (io/file dir))) :level :success :ttl-ms 4000)))

;; =============================================================================
;; Language-facade handlers
;; =============================================================================

(defn ts-start-repl-fn
  "repl_start handler for TypeScript/Bun. Positional `op` (default \"start\") +
   opts `{dir, id}`. Lifecycle: start / restart / stop / status. `op` arrives as
   a STRING from the model (strings-only boundary) — dispatch on it, no keyword
   minting."
  [env op opts]
  (let [root
        (env-root env)

        op
        (if (string? op) op "start")

        id
        (or (get opts "id") (get opts "repl_id"))

        dir
        (resolve-dir root (get opts "dir"))]

    (case op
      "status"
      (extension/success {:result (repl/status dir)})

      "stop"
      (let [r (repl/stop! dir)]
        (vis/unregister-resource! (:session-id env) (repl-resource-id dir id))
        (extension/success {:result r}))

      ("start" "restart")
      (do (when (= op "restart") (repl/stop! dir))
          (let [r (repl/start! dir (or opts {}))]
            (register-repl-resource! (:session-id env) dir r id)
            (extension/success {:result r})))

      (throw (ex-info (str "repl_start(typescript) unknown op: " (pr-str op))
                      {:type :ts/bad-args :got op})))))

(defn ts-repl-eval-fn
  "repl_eval handler for TypeScript/Bun. Accepts a code string or
   `{code, dir, timeout_ms}`. AUTO-STARTS a REPL for the dir if none is running,
   then evaluates (globals persist across calls; top-level await works;
   `reload(path)` re-imports a project module cache-busted)."
  [env arg]
  (let [root
        (env-root env)

        code
        (cond (string? arg) arg
              (map? arg) (str (or (get arg "code") (get arg "source")))
              :else (throw (ex-info "repl_eval(typescript) expects a code string or {\"code\": ...}"
                                    {:type :ts/bad-args :got arg})))

        dir
        (resolve-dir root (and (map? arg) (get arg "dir")))

        tmo
        (and (map? arg) (get arg "timeout_ms"))]

    (when-not (= "up" (get (repl/status dir) "status"))
      (let [r (repl/start! dir {})]
        (register-repl-resource! (:session-id env) dir r nil)))
    ;; Carry the evaluated code back on the result (string key) so the shared
    ;; repl_eval op-card can surface the FORM section.
    (let [res (repl/eval! dir code tmo)]
      (extension/success {:result (cond-> res
                                    (map? res)
                                    (assoc "code" code))}))))

(defn- tail-str [^String s n] (if (<= (count s) n) s (subs s (- (count s) n))))

(defn ts-test-fn
  "run_tests handler: `bun test` in the workspace (or `{dir}`), optionally
   narrowed to `{paths [...]}` / a `{filter \"name\"}` (-t). Returns the parsed
   pass/fail counts + the output tail."
  [env arg]
  (let [root
        (env-root env)

        opts
        (if (map? arg) arg {})

        dir
        (resolve-dir root (get opts "dir"))

        paths
        (seq (map str (get opts "paths")))

        cmd
        (cond-> (conj (runner/resolve-command dir) "test")
          (get opts "filter")
          (conj "-t" (str (get opts "filter")))

          paths
          (into paths))

        pb
        (doto (ProcessBuilder. ^java.util.List cmd)
          (.directory (io/file dir))
          (.redirectErrorStream true))

        p
        (.start pb)

        out
        (future (slurp (.getInputStream p)))

        done?
        (.waitFor p 300 java.util.concurrent.TimeUnit/SECONDS)]

    (when-not done? (.destroyForcibly p))
    (let [s
          (str @out)

          [_ pass]
          (re-find #"(?m)^\s*(\d+) pass" s)

          [_ fail]
          (re-find #"(?m)^\s*(\d+) fail" s)]

      (extension/success {:result {"cmd" (vec cmd)
                                   "dir" dir
                                   "exit" (if done? (.exitValue p) nil)
                                   "timed_out" (not done?)
                                   "passed" (some-> pass
                                                    parse-long)
                                   "failed" (some-> fail
                                                    parse-long)
                                   "output" (tail-str s 8000)}}))))

;; =============================================================================
;; Manifest
;; =============================================================================

;; No :ext/prompt-fn — the foundation advertises repl_eval / repl_start /
;; run_tests through the AUTO capability matrix; repl_eval's own result
;; ({ok,out,err,value,data,type,exc}; opaque values carry __type__/__attrs__/
;; __opaque__) is self-documenting.

(def vis-extension
  (vis/extension
    {:ext/name "language-typescript-bun"
     :ext/description
     "TypeScript/Bun language pack: a managed Bun REPL (persistent globals, top-level await, reload()) behind the generic repl_start/repl_eval/repl_stop facade, plus run_tests -> `bun test`. Activates on Bun/TypeScript workspaces."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn activation-fn
     :ext/language-tools [{:language "typescript"
                           :test-fn ts-test-fn
                           :repl-eval-fn ts-repl-eval-fn
                           :start-repl-fn (fn [env op opts]
                                            (ts-start-repl-fn env op opts))}]
     :ext/startable-resources [{:kind :repl
                                :dir? true
                                :label "Bun REPL"
                                :start-fn (fn [env _selected]
                                            (let [root
                                                  (env-root env)

                                                  dir
                                                  (resolve-dir root (:startable/dir env))

                                                  r
                                                  (repl/start! dir {})]

                                              (register-repl-resource! (:session-id env) dir r)
                                              r))}]
     :ext/kind "language"}))

(vis/register-extension! vis-extension)
