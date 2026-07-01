(ns com.blockether.vis.ext.language-clojure.core
  "vis-language-clojure — Clojure language handlers for Vis.

   Format/test/REPL are exposed through the generic language facade
   (`format`, `test`, `repl_eval`, `repl_start`, `repl_stop`) —
   `format` here does parinfer delimiter repair + cljfmt. The pack also registers
   a cross-cutting op-hook that auto repairs+formats `.clj` files after the
   foundation's struct_patch / patch / write, so no separate repair step is
   needed."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.foundation.environment.languages :as languages]
   [com.blockether.vis.ext.language-clojure.format :as fmt]
   [com.blockether.vis.ext.language-clojure.paren-repair :as repair]
   [com.blockether.vis.ext.language-clojure.lint :as lint]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
   [com.blockether.vis.ext.language-clojure.nrepl-ctx :as nrepl-ctx]
   [com.blockether.vis.ext.language-clojure.ports :as ports]
   [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
   [com.blockether.vis.ext.language-clojure.test-runner :as test-runner]
   [com.blockether.vis.internal.extension :as extension]))

;; =============================================================================
;; Activation
;; =============================================================================

(defn- workspace-has-clojure?
  "Cheap activation check. Strategy, in order of preference:

   1. `:env/languages` already on the env — the engine MAY pre-populate
      it from a higher-level digest. Free win.
   2. Project-file probe — single `File.exists?` for `deps.edn`,
      `project.clj`, `shadow-cljs.edn`, `bb.edn`, `.nrepl-port`.
      This is the fast path: 1-5 syscalls, no walk.
   3. Bounded language scan (`languages/scan`). Only runs when the
      probe missed; some Clojure repos have no manifest at the
      workspace root (e.g. polylith sub-project pinned via channels)."
  [env]
  (let [root (some-> (:workspace/root env) io/file)]
    (when (and root (.isDirectory root))
      (or
        ;; (1) pre-populated env hint
       (boolean (some #(= "clojure" (:language %))
                      (some-> env :env/languages :languages)))

        ;; (2) project-file probe
       (some (fn [n] (.exists (io/file root n)))
             ["deps.edn" "project.clj" "shadow-cljs.edn" "bb.edn" ".nrepl-port"])

        ;; (3) bounded fallback scan
       (try
         (let [scan (languages/scan root {:max-files 2000 :deadline-ms 250})]
           (boolean (some #(= "clojure" (:language %)) (:languages scan))))
         (catch Throwable _ false))))))

(defn- activation-fn [env]
  (boolean (workspace-has-clojure? env)))

;; =============================================================================
;; Tool fns
;; =============================================================================

(defn- env-root ^String [env]
  (or (:workspace/root env)
      (throw (ex-info "clj/* tool fired without :workspace/root in env"
                      {:type :clj/no-workspace}))))

(defn- resolve-repl-dir
  "Resolve a `:start`/`:status` target dir against the workspace `root`. A blank
   dir means the workspace root; a relative dir is taken under root; an absolute
   dir is used as-is. Returns a canonical path string."
  ^String [root dir]
  (let [d (str dir)
        f (cond
            (= "" d)                   (io/file root)
            (.isAbsolute (io/file d))  (io/file d)
            :else                      (io/file root d))]
    (.getCanonicalPath f)))

(defn- coerce-aliases
  "Accept [:dev :test], :dev, [\"dev\"], or nil → a vec of keywords or nil."
  [a]
  (cond
    (nil? a)         nil
    (sequential? a)  (mapv keyword a)
    :else            [(keyword a)]))

(defn- opt [m k]
  (or (get m k) (get m (name k))))

(defn- repl-resource-id
  ([dir] (repl-resource-id dir nil))
  ([dir id]
   (let [id (some-> id str str/trim)]
     (if (seq id)
       id
       (str "nrepl:" dir)))))

(defn register-repl-resource!
  "Mirror a managed nREPL into the session-scoped resource registry so it shows
   in ctx (resources) + the footer, and can be stopped/restarted by id
   from the agent or the UI. No-op without a session or a live spawn. The
   stop-fn/restart-fn thunks ARE the canonical lifecycle — the footer and
   resource_stop both drive repl-manager through them."
  [session dir aliases result & [id]]
  (when (and session (or (:pid result) (:port result)))
    (vis/register-resource! session
                            {:id     (repl-resource-id dir id)
                             :kind   :nrepl
                             :label  (str "nREPL " (.getName (io/file dir))
                                          (when (seq aliases) (apply str (map #(str " :" (name %)) aliases))))
                             :status (or (:status result) :up)
                             :detail (cond-> {:dir dir}
                                       (:port result) (assoc :port (:port result))
                                       (seq aliases)  (assoc :aliases (vec aliases)))
                             :pid    (:pid result)
                             :owner  :ext/language-clojure
                             :language :clojure}
                            {:stop-fn    (fn [] (repl-manager/stop! dir))
                             :restart-fn (fn []
                                           (repl-manager/stop! dir)
                                           (let [r (repl-manager/start! dir {:aliases aliases})]
                                             (register-repl-resource! session dir aliases r id)
                                             r))})
    ;; Surface the registration in the TUI (header toast) so a spawned REPL is
    ;; visible the moment it lands, not just as a silent ● bump in the footer.
    (vis/notify! (str "● nREPL up — " (.getName (io/file dir))
                      (when-let [p (:port result)] (str " :" p)))
                 :level :success :ttl-ms 4000)))

(defn clj-repl-fn
  "Manage a workspace nREPL. Positional op (default \"status\") + optional opts
   dict `{\"dir\": <path>, \"aliases\": [\"dev\", \"test\"]}`:

     \"status\"  — managed-process + discovered-port view (always allowed)
     \"start\"   — self-start a project nREPL subprocess (always allowed)
     \"restart\" — stop then start (always allowed)
     \"stop\"    — stop the Vis-managed nREPL (always allowed)

   \"dir\" runs the REPL in a subdir (e.g. an extension) instead of the
   workspace root; \"aliases\" become deps.edn aliases / lein profiles. Live
   nREPL state already rides in ctx under
   `:session/env :languages :clojure :nrepl`; this tool acts on it."
  ([env] (clj-repl-fn env :status nil))
  ([env op] (clj-repl-fn env op nil))
  ([env op opts]
   (let [root    (env-root env)
         op      (cond (keyword? op) op
                       (string? op)  (keyword op)
                       :else         :status)
         opts    (when (map? opts) opts)
         id      (or (opt opts :id) (opt opts :repl_id))
         dir     (resolve-repl-dir root (opt opts :dir))
         aliases (coerce-aliases (opt opts :aliases))]
     (case op
       :status  (extension/success {:result (repl-manager/status dir)})
       :stop    (let [r (repl-manager/stop! dir)]
                  ;; Drop the session's resource mirror (best-effort; the thunk
                  ;; already ran the real teardown above).
                  (vis/unregister-resource! (:session-id env) (repl-resource-id dir id))
                  (extension/success {:result r}))
       (:start :restart)
       (do
         (when-not (.isDirectory (io/file dir))
           (throw (ex-info (str "clj_repl \"" (name op) "\" target dir does not exist: " dir)
                           {:type :clj/bad-args :dir dir})))
         (let [result (if (= op :restart)
                        (do (repl-manager/stop! dir) (repl-manager/start! dir {:aliases aliases}))
                        (repl-manager/start! dir {:aliases aliases}))]
           ;; Mirror the live REPL into the session resource registry → ctx +
           ;; footer + stoppable by id. External (not-ours) REPLs carry no pid,
           ;; so register-repl-resource! no-ops on them.
           (register-repl-resource! (:session-id env) dir aliases result id)
           (extension/success {:result result})))
       (throw (ex-info (str "clj_repl unknown op: " (pr-str op))
                       {:type :clj/bad-args :got op
                        :examples ["clj_repl()" "clj_repl(\"status\")" "clj_repl(\"start\")"
                                   "clj_repl(\"start\", {\"dir\": \"extensions/languages/vis-language-clojure\", \"aliases\": [\"dev\", \"test\"]})"
                                   "clj_repl(\"stop\")" "clj_repl(\"restart\")"]}))))))

(defn ui-start-repl!
  "Channel-invokable nREPL start for the Resources UI (web modal / TUI F4).
   Resolves the workspace dir from `env`, starts a managed nREPL with `aliases`
   (vec/seq of keyword-or-string names, or nil), and mirrors it into the session
   resource registry (ctx + footer + stop/restart). ALWAYS allowed: the
   `clj_repl` flag gates only the MODEL's self-start — a user clicking Start is
   explicit consent. Returns the start result map."
  [env aliases]
  (let [root (env-root env)
        dir  (resolve-repl-dir root nil)
        als  (coerce-aliases aliases)]
    (when-not (.isDirectory (io/file dir))
      (throw (ex-info (str "REPL target dir does not exist: " dir)
                      {:type :clj/bad-args :dir dir})))
    (let [result (repl-manager/start! dir {:aliases als})]
      (register-repl-resource! (:session-id env) dir als result)
      result)))

(defn available-aliases
  "Alias names declared in the workspace `deps.edn` — surfaced to the UI so the
   user picks REAL aliases (`:dev`, `:test`, …) instead of guessing. Returns a
   sorted vec of strings WITHOUT the leading colon; empty on any read/parse
   failure or a non-deps project."
  [env]
  (try
    (let [root (env-root env)
          dir  (resolve-repl-dir root nil)
          f    (io/file dir "deps.edn")]
      (if (.isFile f)
        (->> (:aliases (edn/read-string (slurp f)))
             keys (map name) sort vec)
        []))
    (catch Throwable _ [])))

(defn- coerce-eval-arg
  "Accept the call shapes the model is most likely to type:
     clj_eval(\"(+ 1 1)\")
     clj_eval({\"code\": \"(+ 1 1)\"})
     clj_eval({\"code\": \"...\", \"port\": 7888, \"ns\": \"user\", \"timeout_ms\": 5000})
     clj_eval({\"code\": \"...\", \"id\": \"<repl-id>\"})   ; target a registered REPL"
  [arg]
  (cond
    (string? arg) {:code arg}
    (map? arg)    arg
    :else (throw (ex-info "clj_eval expects a code string or opts map"
                          {:type :clj/bad-args :got arg
                           :examples ["clj_eval(\"(+ 1 1)\")"
                                      "clj_eval({\"code\": \"...\", \"port\": 7888})"]}))))

(defn- resolve-eval-port
  "Pick the nREPL port for an eval. Priority:
     1. explicit `port`;
     2. the registered REPL resource named by `id`/`repl_id` — so the model can
        TARGET a specific REPL it started (previously SILENTLY IGNORED, always
        falling back to discovery); errors if that id isn't live;
     3. the first DISCOVERED port that PROBES live (so a stale `.nrepl-port` no
        longer wins over a live REPL elsewhere);
     4. the first discovered (a lone stale port is still tried so its connect
        error surfaces, instead of being silently preferred)."
  [env root host port rid]
  (cond
    port port
    rid  (or (some-> (vis/get-resource (:session-id env) rid) (get-in [:detail :port]))
             (throw (ex-info (str "no nREPL registered under id '" rid
                                  "' — check session_resources for live REPL ids")
                             {:type :clj/unknown-repl-id :id rid})))
    :else
    (let [cands (map :port (ports/discover-all root))]
      (or (some (fn [p] (when (= :up (:status (nrepl-client/probe! {:host host :port p}))) p)) cands)
          (first cands)))))

(defn clj-eval-fn
  ([env arg]
   (let [{:keys [code port host ns timeout_ms id repl_id]} (coerce-eval-arg arg)
         root (env-root env)
         host (or host "localhost")
         rid  (some-> (or id repl_id) str str/trim not-empty)
         port (resolve-eval-port env root host port rid)]
     (when-not port
       (throw (ex-info "no nREPL port found — call clj_repl(\"start\") to boot a project nREPL now (autostart is ON by default), or clj_repl() to inspect candidates"
                       {:type :clj/no-port
                        :workspace-root root})))
     (extension/success
      {:result (nrepl-client/eval!
                {:host       host
                 :port       port
                 :code       code
                 :ns         ns
                 :timeout-ms (or timeout_ms 30000)})}))))

(defn clj-repair+format
  "The combined Clojure tidy used by BOTH `format` and the post-edit hook:
   parinfer delimiter repair FIRST (so unbalanced ( [ { from a raw edit are
   fixed), THEN cljfmt indentation. Total — returns `code` unchanged on any
   failure of either step."
  [code]
  (let [repaired (or (repair/fix-delimiters code) code)]
    (fmt/format-string repaired)))

(defn- relativize-path
  "Rewrite an absolute path to one relative to workspace `root` so tool output
   reads `src/foo.clj` instead of the noisy machine-absolute `/Users/…/src/foo.clj`.
   Paths that aren't under root, and non-path sentinels like `<stdin>`, pass
   through unchanged."
  [^java.io.File root file]
  (let [s (str file)]
    (if (and root (seq s) (not= s "<stdin>"))
      (try
        (let [rp (.toPath (.getCanonicalFile root))
              fp (.toPath (.getCanonicalFile (io/file s)))]
          (if (.startsWith fp rp)
            (str (.relativize rp fp))
            s))
        (catch Throwable _ s))
      s)))

(defn clj-format-fn
  ([arg] (clj-format-fn nil arg))
  ([env arg]
   (let [path (and (map? arg) (or (:path arg) (get arg "path")))
         code (cond
                (string? arg)                          arg
                (and (map? arg) (contains? arg :code)) (str (:code arg))
                (and (map? arg) (contains? arg "code")) (str (get arg "code"))
                path                                   (slurp (str path))
                :else (throw (ex-info "format expects a code string, {\"code\": ...}, or {\"path\": ...}"
                                      {:type :clj/bad-args :got arg
                                       :examples ["format(\"clojure\", \"(defn f [x]\\n(* x 2))\")"
                                                  "format(\"clojure\", {\"code\": \"...\"})"
                                                  "format(\"clojure\", {\"path\": \"src/foo.clj\"})"]})))
         out  (clj-repair+format code)]
     (when (and path (not= out code))
       (spit (str path) out))
     (extension/success
      {:result (cond-> {:op        :clj-format
                        :changed?  (not= out code)
                        :repaired? (not= (or (repair/fix-delimiters code) code) code)
                        :text      out}
                 path (assoc :path   (relativize-path (io/file (or (:workspace/root env) ".")) path)
                             :wrote? (not= out code)))}))))

(defn clj-lint-fn
  "clj-kondo lint via the language facade (`lint_code`). Accepts:
     - a raw code string / {:code ...}  -> lint it on stdin
     - {:path \"src/foo.clj\"}            -> lint that file
     - {:paths [\"src\" \"test\"]}          -> lint those paths
     - nothing / {}                     -> lint the workspace's src + test (or root)
   Paths are resolved against :workspace/root when relative. Finding `:file`
   paths are reported RELATIVE to :workspace/root (absolute only when outside)."
  [env arg]
  (let [root  (io/file (or (:workspace/root env) "."))
        path  (when (map? arg) (or (:path arg) (get arg "path")))
        paths (when (map? arg) (or (:paths arg) (get arg "paths")))
        code  (cond
                (string? arg)                          arg
                (and (map? arg) (contains? arg :code)) (str (:code arg))
                (and (map? arg) (contains? arg "code")) (str (get arg "code"))
                :else nil)
        under (fn [p] (let [f (io/file (str p))]
                        (str (if (.isAbsolute f) f (io/file root (str p))))))
        base  (cond
                code        (lint/lint-code code)
                path        (lint/lint-paths [(under path)])
                (seq paths) (lint/lint-paths (mapv under paths))
                :else       (let [defaults (->> ["src" "test"]
                                                (map #(io/file root %))
                                                (filter #(.exists ^java.io.File %))
                                                (mapv str))]
                              (lint/lint-paths (if (seq defaults) defaults [(str root)]))))]
    (extension/success
     {:result
      (assoc
       (update base :findings
               (fn [fs] (mapv #(update % :file (partial relativize-path root)) fs)))
       :language "clojure")})))

;; ── Auto-repair hook: keep .clj source tidy after a generic edit op ──────────
(def ^:private clj-source-exts [".clj" ".cljs" ".cljc" ".cljx" ".edn"])

(defn- clj-source-file? [path]
  (let [p (str/lower-case (str path))]
    (boolean (some #(str/ends-with? p %) clj-source-exts))))

(defn- edit-arg-paths
  "Edited file path(s) from a struct_patch/write call (a map with :path/\"path\")
   or a patch call (a seq of such maps). Tolerant of keyword OR string keys."
  [args]
  (let [a (first args)]
    (cond
      (map? a)        (when-let [p (or (:path a) (get a "path"))] [p])
      (sequential? a) (keep #(when (map? %) (or (:path %) (get % "path"))) a)
      (string? a)     [a]
      :else           nil)))

(defn- resolve-under-root ^java.io.File [env path]
  (let [f (io/file (str path))]
    (if (.isAbsolute f) f (io/file (or (:workspace/root env) ".") (str path)))))

(defn- repair-clj-file!
  "Re-tidy a just-edited Clojure file IN PLACE (paren-repair + cljfmt), writing
   back ONLY when the content actually changes — a clean file is a no-op so
   nothing churns. Returns true when it rewrote."
  [env path]
  (let [f (resolve-under-root env path)]
    (when (and (clj-source-file? path) (.isFile f))
      (let [code (slurp f)
            out  (clj-repair+format code)]
        (when (and (string? out) (not= out code))
          (spit f out)
          true)))))

(defn clj-edit-repair-hook
  "An :after op-hook (registered on struct_patch / patch / write): after a
   SUCCESSFUL edit, paren-repair + cljfmt every Clojure file it touched, so the
   model never needs a separate repair/format step and a raw `patch` that left
   delimiters unbalanced is auto-corrected. Returns the result unchanged; a
   throwing repair is caught + logged by the op-hook runner, never breaking the
   underlying edit."
  [env _op-kw args result]
  (when (:success? result)
    (doseq [p (edit-arg-paths args)]
      (repair-clj-file! env p)))
  result)

(defn clj-struct-patch-no-fail-around
  "MIDDLEWARE (:around) on struct_patch so a Clojure structural edit does NOT
   fail on unbalanced delimiters. If the call throws and it targeted a `.clj`
   file with a `:code` form, parinfer-repair the code and retry ONCE. If the
   repair changes nothing — or the retried edit still fails — the ORIGINAL error
   is surfaced (we never bury a real structural failure). Non-clj / non-code
   calls pass straight through to `next`."
  [_env _op-kw args next]
  (try
    (next args)
    (catch clojure.lang.ExceptionInfo e
      (let [m     (first args)
            path  (and (map? m) (or (:path m) (get m "path")))
            codek (cond (and (map? m) (contains? m :code))  :code
                        (and (map? m) (contains? m "code")) "code"
                        :else nil)
            code  (and codek (get m codek))
            fixed (when (and (clj-source-file? path) (string? code))
                    (repair/fix-delimiters code))]
        (if (and fixed (not= fixed code))
          (try (next (assoc-in (vec args) [0 codek] fixed))
               (catch Throwable _ (throw e)))            ; repaired retry failed → original
          (throw e))))))

;; =============================================================================
;; Extension manifest
;; =============================================================================

;; No :ext/prompt-fn — the foundation advertises this pack's verbs through the
;; AUTO capability matrix (language_surface/capability-matrix). nREPL state still
;; rides in ctx via :ext/ctx-fn.
(def vis-extension
  (vis/extension
   {:ext/name           "language-clojure"
    :ext/description    "Clojure language pack: live nREPL state in ctx, generic language-surface handlers for format/test/repl, and REPL/format/paren-repair tooling. Activates only when the workspace has Clojure sources."
    :ext/version        "0.1.0"
    :ext/author         "Blockether"
    :ext/owner          "vis"
    :ext/license        "Apache-2.0"
    :ext/activation-fn  activation-fn
    :ext/ctx-fn            nrepl-ctx/contribute
    :ext/language-tools [{:language :clojure
                          :format-fn (fn [env arg]
                                       (clj-format-fn env arg))
                          :lint-fn clj-lint-fn
                          :test-fn test-runner/clj-test-fn
                          :repl-eval-fn clj-eval-fn
                          :start-repl-fn (fn [env op opts]
                                           (clj-repl-fn env op opts))}]
     ;; Declarative cross-cutting op-hooks — registered/unregistered WITH this
     ;; extension's lifecycle (no imperative side effects at ns load). They keep
     ;; Clojure source tidy after the foundation's struct_patch / patch / write
     ;; (an :after repair+format), and make a struct_patch NOT fail on unbalanced
     ;; delimiters (an :around that repairs the code + retries). :owner is set to
     ;; this extension automatically.
    :ext/op-hooks [{:op :struct_patch :phase :after  :fn clj-edit-repair-hook}
                   {:op :patch        :phase :after  :fn clj-edit-repair-hook}
                   {:op :write        :phase :after  :fn clj-edit-repair-hook}
                   {:op :struct_patch :phase :around :fn clj-struct-patch-no-fail-around}]
     ;; Declarative startable resource — the Resources UI (web modal / TUI F4)
     ;; renders this generically: its title, the proposed deps.edn aliases, and
     ;; Start. Always allowed (the self-start flag gates only the model).
    :ext/startable-resources
    [{:kind          :nrepl
      :label         "nREPL"
      :options-label "aliases"
      :options-fn    (fn [env] (mapv #(str ":" %) (available-aliases env)))
      :start-fn      (fn [env selected]
                       (ui-start-repl! env (map #(str/replace (str %) #"^:" "") selected)))}]
    :ext/kind           "language"}))

(vis/register-extension! vis-extension)