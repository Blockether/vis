(ns com.blockether.vis.ext.language-python.core
  "vis-language-python — a managed Python REPL exposed through the generic
   language facade (repl_start / repl_eval / repl_stop). Activates
   only when the workspace looks like a Python project. The REPL is a subprocess
   on a project-aware interpreter (uv / poetry / .venv / python3), registered as
   a session resource so it shows in ctx + the footer and is stoppable by id."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.language-python.interpreter :as interpreter]
            [com.blockether.vis.ext.language-python.repl-manager :as repl]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.python-test-runner :as ptr]))

;; =============================================================================
;; Activation
;; =============================================================================

(defn- workspace-has-python?
  [env]
  (let
    [root (some-> (:workspace/root env)
                  io/file)]
    (when (and root (.isDirectory root))
      (or (some #(.exists (io/file root %))
                ["pyproject.toml" "setup.py" "setup.cfg" "requirements.txt" "Pipfile" "uv.lock"])
          ;; bounded scan for a .py anywhere (lazy file-seq, capped)
          (boolean (some #(and (.isFile ^java.io.File %)
                               (str/ends-with? (.getName ^java.io.File %) ".py"))
                         (take 3000 (file-seq root))))))))

(defn- activation-fn [env] (boolean (workspace-has-python? env)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- env-root
  ^String [env]
  (or (:workspace/root env)
      (throw (ex-info "python tool fired without :workspace/root in env"
                      {:type :py/no-workspace}))))

(defn- resolve-dir
  ^String [root dir]
  (let [d (str (or dir ""))]
    (.getCanonicalPath (cond (= "" d) (io/file root)
                             (.isAbsolute (io/file d)) (io/file d)
                             :else (io/file root d)))))

(defn- repl-resource-id
  [dir id]
  (let
    [id (some-> id
                str
                str/trim)]
    (if (seq id) id (str "pyrepl:" dir))))

(defn register-repl-resource!
  "Mirror a managed Python REPL into the session resource registry (ctx + footer
   + stop/restart by id). No-op without a session or a live pid."
  [session dir result & [id]]
  ;; `result` is repl/start!'s STRING-keyed lifecycle map. The resource map is
  ;; the CENTRAL resources.clj DATA shape (keyword keys — ->data stringifies its
  ;; own keys + kind/status/owner/language enums), but `:detail` is passed
  ;; THROUGH verbatim, so it must already be STRING-keyed for the boundary.
  (when (and session (get result "pid"))
    (vis/register-resource! session
                            {:id (repl-resource-id dir id)
                             :kind :repl
                             :label (str "python REPL " (.getName (io/file dir)))
                             :status (or (get result "status") :up)
                             :detail {"dir" dir "cmd" (get result "cmd")}
                             :pid (get result "pid")
                             :owner :ext/language-python
                             :language :python}
                            {:stop-fn (fn []
                                        (repl/stop! dir))
                             :restart-fn (fn []
                                           (repl/stop! dir)
                                           (let [r (repl/start! dir {:session-id session})]
                                             (register-repl-resource! session dir r id)
                                             r))})
    (vis/notify! (str "● python REPL up — " (.getName (io/file dir)))
                 :level :success
                 :ttl-ms 4000)))

;; =============================================================================
;; Language-facade handlers
;; =============================================================================

(defn py-start-repl-fn
  "repl_start handler for Python. Positional `op` (default \"start\") + opts
   `{dir, id}`. Lifecycle: start / restart / stop / status. `op` arrives as a
   STRING from the model (strings-only boundary) — dispatch on it, no keyword
   minting."
  [env op opts]
  (let
    [root
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
          (let [r (repl/start! dir (assoc (or opts {}) :session-id (:session-id env)))]
            (register-repl-resource! (:session-id env) dir r id)
            (extension/success {:result r})))

      (throw (ex-info (str "repl_start(python) unknown op: " (pr-str op))
                      {:type :py/bad-args :got op})))))

(defn py-repl-eval-fn
  "repl_eval handler for Python. Accepts a code string or
   `{code, dir, timeout_ms}`. Requires a running REPL for the dir, then evaluates
   with globals persistent across calls."
  [env arg]
  (let
    [root
     (env-root env)

     code
     (cond (string? arg) arg
           (map? arg) (str (or (get arg "code") (get arg "source")))
           :else (throw (ex-info "repl_eval(python) expects a code string or {\"code\": ...}"
                                 {:type :py/bad-args :got arg})))

     dir
     (resolve-dir root (and (map? arg) (get arg "dir")))

     tmo
     (and (map? arg) (get arg "timeout_ms"))]

    (when-not (= "up" (get (repl/status dir) "status"))
      (throw (ex-info (str "Python REPL is not up for "
                           dir
                           "; call repl_start(\"python\", {\"dir\": "
                           (pr-str dir)
                           "}) first")
                      {:type :py/no-repl :dir dir})))
    ;; Carry the evaluated code back on the result (string key) so the shared
    ;; repl_eval op-card can surface the FORM section — the render fn sees only
    ;; the result map, not the call args.
    (let [res (repl/eval! dir code tmo)]
      (extension/success {:result (cond-> res
                                    (map? res)
                                    (assoc "code" code))}))))

;; =============================================================================
;; run_tests
;; =============================================================================

(defn- tail-str
  "Last `n` chars of `s`, ellipsized when truncated."
  [^String s n]
  (if (<= (count s) (long n)) s (str "…" (subs s (- (count s) (long n))))))

(defn- resolve-test-paths
  "Absolute path strings to hand a test runner. Honors `{paths}`; else defaults
   to `tests/` when it exists, otherwise the workspace root."
  [^String root opts]
  (let [given (seq (map str (get opts "paths")))]
    (cond given (mapv #(resolve-dir root %) given)
          (.isDirectory (io/file root "tests")) [(resolve-dir root "tests")]
          :else [(resolve-dir root nil)])))

(defn- graalpy-test
  "Hermetic backend: discover test_*.py / *_test.py under `paths` and run each in
   a trusted GraalPy context via the built-in stdlib-only pytest shim. Adds a
   `hint` to switch to the project interpreter when a failure smells like a
   missing third-party module the sandbox can't see."
  [paths]
  (let
    [res
     (ptr/test-python-extensions! {:dirs paths})

     dep-smell?
     (boolean (some (fn [t]
                      (and (= :errored (:outcome t))
                           (re-find #"(?i)ModuleNotFoundError|No module named|ImportError"
                                    (str (:message t)))))
                    (:tests res)))]

    (cond->
      {"runner" "graalpy"
       "files" (:files res)
       "ok" (boolean (:ok? res))
       "passed" (or (:passed res) 0)
       "failed" (or (:failed res) 0)
       "errored" (or (:errored res) 0)
       "skipped" (or (:skipped res) 0)
       "output" (ptr/render-test-report res)}
      (:error res)
      (assoc "error" (:error res))

      dep-smell?
      (assoc "hint"
        (str "Some tests failed to import modules under the stdlib-only GraalPy "
             "sandbox. Re-run with {\"runner\": \"project\"} to use the project "
             "interpreter's installed dependencies.")))))

(defn- project-test
  "Escape-hatch backend: shell the project interpreter's pytest (uv / poetry /
   .venv / python3 `-m pytest <paths>`) in `dir` so installed deps are visible."
  [session-id ^String dir paths]
  (let
    [cmd
     (-> (interpreter/resolve-command dir)
         (conj "-m" "pytest")
         (into paths))

     launch
     (vis/session-process-launch session-id cmd)

     pb
     (doto (ProcessBuilder. ^java.util.List (:argv launch))
       (.directory (io/file dir))
       (.redirectErrorStream true))

     _env
     (let [^java.util.Map e (.environment ^ProcessBuilder pb)]
       (when (:replace-env? launch) (.clear e))
       (doseq [[k v] (:env launch)]
         (.put e ^String k ^String v)))

     p
     (.start pb)

     out
     (future (slurp (.getInputStream p)))

     done?
     (.waitFor p 300 java.util.concurrent.TimeUnit/SECONDS)]

    (when-not done? (.destroyForcibly p))
    (let
      [s
       (str @out)

       [_ passed]
       (re-find #"(?m)(\d+) passed" s)

       [_ failed]
       (re-find #"(?m)(\d+) failed" s)

       [_ errored]
       (re-find #"(?m)(\d+) error(?:ed|s)?\b" s)]

      {"runner" "project"
       "cmd" (vec cmd)
       "dir" dir
       "exit" (when done? (.exitValue p))
       "timed_out" (not done?)
       "passed" (some-> passed
                        parse-long)
       "failed" (some-> failed
                        parse-long)
       "errored" (some-> errored
                         parse-long)
       "output" (tail-str s 8000)})))

(defn py-test-fn
  "run_tests handler for Python. Two backends behind `{runner}`:
     - \"graalpy\" (DEFAULT) — hermetic, stdlib-only. Discovers `test_*.py` /
       `*_test.py` under `{paths}` (default: `tests/` if present, else the
       workspace root) and runs each in a TRUSTED GraalPy context via the
       built-in pytest shim. No project deps visible.
     - \"project\" — shells the project interpreter's pytest
       (`uv`/`poetry`/`.venv`/`python3` `-m pytest <paths>`) so installed test
       deps ARE visible. Aliases: `{interpreter true}`.
   Pass an opts map `{runner, paths, dir}` (a bare code string is not accepted)."
  [env arg]
  (let
    [root
     (env-root env)

     opts
     (if (map? arg) arg {})

     dir
     (resolve-dir root (get opts "dir"))

     runner
     (let
       [r (str/lower-case
            (str (or (get opts "runner") (when (get opts "interpreter") "project") "graalpy")))]
       (if (contains? #{"project" "interpreter" "real" "system"} r) "project" "graalpy"))

     paths
     (resolve-test-paths root opts)]

    (extension/success {:result (assoc (if (= "project" runner)
                                         (project-test (:session-id env) dir paths)
                                         (graalpy-test paths))
                                  "language" "python")})))

;; =============================================================================
;; Manifest
;; =============================================================================

;; No :ext/prompt-fn — the foundation advertises repl_eval / repl_start through
;; the AUTO capability matrix; repl_eval's own result ({ok,out,value,data,type,
;; exc}; opaque values carry __type__/__attrs__/__opaque__) is self-documenting.

(def vis-extension
  (vis/extension
    {:ext/name "language-python"
     :ext/description
     "Python language pack: a managed Python REPL (uv/poetry/venv/python3) behind the generic repl_start/repl_eval/repl_stop facade. Activates on Python workspaces."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/activation-fn activation-fn
     :ext/language-tools [{:language "python"
                           :repl-eval-fn py-repl-eval-fn
                           :test-fn py-test-fn
                           :start-repl-fn (fn [env op opts]
                                            (py-start-repl-fn env op opts))}]
     :ext/startable-resources [{:kind :repl
                                :dir? true
                                :label "Python REPL"
                                :start-fn (fn [env _selected]
                                            (let
                                              [root
                                               (env-root env)

                                               dir
                                               (resolve-dir root (:startable/dir env))

                                               r
                                               (repl/start! dir {:session-id (:session-id env)})]

                                              (register-repl-resource! (:session-id env) dir r)
                                              r))}]
     :ext/kind "language"}))

(vis/register-extension! vis-extension)
