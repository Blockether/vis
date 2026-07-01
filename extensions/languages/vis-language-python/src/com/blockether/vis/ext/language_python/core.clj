(ns com.blockether.vis.ext.language-python.core
  "vis-language-python — a managed Python REPL exposed through the generic
   language facade (repl_start / repl_eval / repl_stop). Activates
   only when the workspace looks like a Python project. The REPL is a subprocess
   on a project-aware interpreter (uv / poetry / .venv / python3), registered as
   a session resource so it shows in ctx + the footer and is stoppable by id."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.language-python.repl-manager :as repl]
            [com.blockether.vis.internal.extension :as extension]))

;; =============================================================================
;; Activation
;; =============================================================================

(defn- workspace-has-python? [env]
  (let [root (some-> (:workspace/root env) io/file)]
    (when (and root (.isDirectory root))
      (or
       (some #(.exists (io/file root %))
             ["pyproject.toml" "setup.py" "setup.cfg" "requirements.txt" "Pipfile" "uv.lock"])
        ;; bounded scan for a .py anywhere (lazy file-seq, capped)
       (boolean (some #(and (.isFile ^java.io.File %)
                            (str/ends-with? (.getName ^java.io.File %) ".py"))
                      (take 3000 (file-seq root))))))))

(defn- activation-fn [env] (boolean (workspace-has-python? env)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- env-root ^String [env]
  (or (:workspace/root env)
      (throw (ex-info "python tool fired without :workspace/root in env"
                      {:type :py/no-workspace}))))

(defn- resolve-dir ^String [root dir]
  (let [d (str (or dir ""))]
    (.getCanonicalPath
     (cond
       (= "" d)                   (io/file root)
       (.isAbsolute (io/file d))  (io/file d)
       :else                      (io/file root d)))))

(defn- opt [m k] (when (map? m) (or (get m k) (get m (name k)))))

(defn- repl-resource-id [dir id]
  (let [id (some-> id str str/trim)]
    (if (seq id) id (str "pyrepl:" dir))))

(defn register-repl-resource!
  "Mirror a managed Python REPL into the session resource registry (ctx + footer
   + stop/restart by id). No-op without a session or a live pid."
  [session dir result & [id]]
  (when (and session (:pid result))
    (vis/register-resource! session
                            {:id       (repl-resource-id dir id)
                             :kind     :repl
                             :label    (str "python REPL " (.getName (io/file dir)))
                             :status   (or (:status result) :up)
                             :detail   {:dir dir :cmd (:cmd result)}
                             :pid      (:pid result)
                             :owner    :ext/language-python
                             :language :python}
                            {:stop-fn    (fn [] (repl/stop! dir))
                             :restart-fn (fn []
                                           (repl/stop! dir)
                                           (let [r (repl/start! dir {})]
                                             (register-repl-resource! session dir r id)
                                             r))})
    (vis/notify! (str "● python REPL up — " (.getName (io/file dir)))
                 :level :success :ttl-ms 4000)))

;; =============================================================================
;; Language-facade handlers
;; =============================================================================

(defn py-start-repl-fn
  "repl_start handler for Python. Positional `op` (default :start) + opts
   `{dir, id}`. Lifecycle: :start / :restart / :stop / :status."
  [env op opts]
  (let [root (env-root env)
        op   (cond (keyword? op) op (string? op) (keyword op) :else :start)
        id   (or (opt opts :id) (opt opts :repl_id))
        dir  (resolve-dir root (opt opts :dir))]
    (case op
      :status (extension/success {:result (repl/status dir)})
      :stop   (let [r (repl/stop! dir)]
                (vis/unregister-resource! (:session-id env) (repl-resource-id dir id))
                (extension/success {:result r}))
      (:start :restart)
      (do (when (= op :restart) (repl/stop! dir))
          (let [r (repl/start! dir (or opts {}))]
            (register-repl-resource! (:session-id env) dir r id)
            (extension/success {:result r})))
      (throw (ex-info (str "repl_start(python) unknown op: " (pr-str op))
                      {:type :py/bad-args :got op})))))

(defn py-repl-eval-fn
  "repl_eval handler for Python. Accepts a code string or
   `{code, dir, timeout_ms}`. AUTO-STARTS a REPL for the dir if none is running,
   then evaluates (globals persist across calls)."
  [env arg]
  (let [root (env-root env)
        code (cond (string? arg) arg
                   (map? arg) (str (or (opt arg :code) (opt arg :source)))
                   :else (throw (ex-info "repl_eval(python) expects a code string or {\"code\": ...}"
                                         {:type :py/bad-args :got arg})))
        dir  (resolve-dir root (opt arg :dir))
        tmo  (opt arg :timeout_ms)]
    (when-not (= :up (:status (repl/status dir)))
      (let [r (repl/start! dir {})]
        (register-repl-resource! (:session-id env) dir r nil)))
    (extension/success {:result (repl/eval! dir code tmo)})))

;; =============================================================================
;; Manifest
;; =============================================================================

;; No :ext/prompt-fn — the foundation advertises repl_eval / repl_start through
;; the AUTO capability matrix; repl_eval's own result ({ok,out,value,data,type,
;; exc}; opaque values carry __type__/__attrs__/__opaque__) is self-documenting.

(def vis-extension
  (vis/extension
   {:ext/name           "language-python"
    :ext/description    "Python language pack: a managed Python REPL (uv/poetry/venv/python3) behind the generic repl_start/repl_eval/repl_stop facade. Activates on Python workspaces."
    :ext/version        "0.1.0"
    :ext/author         "Blockether"
    :ext/owner          "vis"
    :ext/license        "Apache-2.0"
    :ext/activation-fn  activation-fn
    :ext/language-tools [{:language      :python
                          :repl-eval-fn  py-repl-eval-fn
                          :start-repl-fn (fn [env op opts] (py-start-repl-fn env op opts))}]
    :ext/startable-resources
    [{:kind     :repl
      :label    "Python REPL"
      :start-fn (fn [env _selected]
                  (let [root (env-root env)
                        dir  (resolve-dir root nil)
                        r    (repl/start! dir {})]
                    (register-repl-resource! (:session-id env) dir r)
                    r))}]
    :ext/kind           "language"}))

(vis/register-extension! vis-extension)
