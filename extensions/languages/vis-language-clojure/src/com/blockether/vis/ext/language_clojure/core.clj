(ns com.blockether.vis.ext.language-clojure.core
  "vis-language-clojure — Clojure-specific tooling for Vis.

   Activates ONLY when the workspace language scan (foundation-core)
   detects Clojure files. Surfaces a small `clj` alias inside the
   Python sandbox:

     clj_repl()     -> manage workspace nREPL (status/start/stop/restart)
     clj_eval(...)  -> eval in a running nREPL (per-port conn pool)
     clj_edit(...)  -> rewrite-clj edit

   No surface duplicates foundation-core's bare tools. `cat`/`rg`/
   `patch` stay the right answer for everything Clojure-agnostic —
   including structure exploration: `rg` (with `context`) plus the
   engine `doc` / `apropos` system calls cover def lookup. (The old `clj/outline` / `clj/find` rode a hardcoded
   def-head allowlist that silently dropped `deftest` and any
   macro-defined form; removed pending a cross-platform tree-sitter
   outline.)

   The Python surface registered here exposes `clj_eval`. Inside this
   namespace the matching var (`eval`) shadows `clojure.core/eval`;
   that is intentional — we never use the core fn here, and
   `:refer-clojure :exclude` silences the load-time warning so the
   build log stays clean."
  (:refer-clojure :exclude [eval])
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.foundation.environment.languages :as languages]
   [com.blockether.vis.ext.language-clojure.edit :as edit]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
   [com.blockether.vis.ext.language-clojure.nrepl-ctx :as nrepl-ctx]
   [com.blockether.vis.ext.language-clojure.ports :as ports]
   [com.blockether.vis.ext.language-clojure.render :as render]
   [com.blockether.vis.ext.language-clojure.repl-manager :as repl-manager]
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

(defn- repl-resource-id [dir] (str "nrepl:" dir))

(defn register-repl-resource!
  "Mirror a managed nREPL into the session-scoped resource registry so it shows
   in ctx (session_resources) + the footer, and can be stopped/restarted by id
   from the agent or the UI. No-op without a session or a live spawn. The
   stop-fn/restart-fn thunks ARE the canonical lifecycle — the footer and
   resource_stop both drive repl-manager through them."
  [session dir aliases result]
  (when (and session (or (:pid result) (:port result)))
    (vis/register-resource! session
      {:id     (repl-resource-id dir)
       :kind   :nrepl
       :label  (str "nREPL " (.getName (io/file dir))
                 (when (seq aliases) (apply str (map #(str " :" (name %)) aliases))))
       :status (or (:status result) :up)
       :detail (cond-> {:dir dir}
                 (:port result) (assoc :port (:port result))
                 (seq aliases)  (assoc :aliases (vec aliases)))
       :pid    (:pid result)
       :owner  :ext/language-clojure}
      {:stop-fn    (fn [] (repl-manager/stop! dir))
       :restart-fn (fn []
                     (repl-manager/stop! dir)
                     (let [r (repl-manager/start! dir {:aliases aliases})]
                       (register-repl-resource! session dir aliases r)
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
     \"start\"   — self-start a project nREPL subprocess (flag-gated)
     \"restart\" — stop then start (flag-gated)
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
         dir     (resolve-repl-dir root (:dir opts))
         aliases (coerce-aliases (:aliases opts))]
     (case op
       :status  (extension/success {:result (repl-manager/status dir)})
       :stop    (let [r (repl-manager/stop! dir)]
                  ;; Drop the session's resource mirror (best-effort; the thunk
                  ;; already ran the real teardown above).
                  (vis/unregister-resource! (:session-id env) (repl-resource-id dir))
                  (extension/success {:result r}))
       (:start :restart)
       (do
         (when-not (repl-manager/flag-enabled?)
           (throw (ex-info (str "clj_repl \"" (name op) "\" is disabled — self-start is on by default but "
                             repl-manager/flag-env " is set to a falsy value. Unset it (or set it truthy) to allow self-start.")
                    {:type :clj/repl-disabled :flag repl-manager/flag-env :op op})))
         (when-not (.isDirectory (io/file dir))
           (throw (ex-info (str "clj_repl \"" (name op) "\" target dir does not exist: " dir)
                    {:type :clj/bad-args :dir dir})))
         (let [result (if (= op :restart)
                        (do (repl-manager/stop! dir) (repl-manager/start! dir {:aliases aliases}))
                        (repl-manager/start! dir {:aliases aliases}))]
           ;; Mirror the live REPL into the session resource registry → ctx +
           ;; footer + stoppable by id. External (not-ours) REPLs carry no pid,
           ;; so register-repl-resource! no-ops on them.
           (register-repl-resource! (:session-id env) dir aliases result)
           (extension/success {:result result})))
       (throw (ex-info (str "clj_repl unknown op: " (pr-str op))
                {:type :clj/bad-args :got op
                 :examples ["clj_repl()" "clj_repl(\"status\")" "clj_repl(\"start\")"
                            "clj_repl(\"start\", {\"dir\": \"extensions/languages/vis-language-clojure\", \"aliases\": [\"dev\", \"test\"]})"
                            "clj_repl(\"stop\")" "clj_repl(\"restart\")"]}))))))

(defn- coerce-eval-arg
  "Accept the call shapes the model is most likely to type:
     clj_eval(\"(+ 1 1)\")
     clj_eval({\"code\": \"(+ 1 1)\"})
     clj_eval({\"code\": \"...\", \"port\": 7888, \"ns\": \"user\", \"timeout_ms\": 5000})"
  [arg]
  (cond
    (string? arg) {:code arg}
    (map? arg)    arg
    :else (throw (ex-info "clj_eval expects a code string or opts map"
                   {:type :clj/bad-args :got arg
                    :examples ["clj_eval(\"(+ 1 1)\")"
                               "clj_eval({\"code\": \"...\", \"port\": 7888})"]}))))

(defn clj-eval-fn
  ([env arg]
   (let [{:keys [code port host ns timeout_ms]} (coerce-eval-arg arg)
         root (env-root env)
         port (or port (ports/find-default root))]
     (when-not port
       (throw (ex-info "no nREPL port found — start one (e.g. `bin/dev`) or call clj_repl() to inspect candidates"
                {:type :clj/no-port
                 :workspace-root root})))
     (extension/success
       {:result (nrepl-client/eval!
                  {:host       (or host "localhost")
                   :port       port
                   :code       code
                   :ns         ns
                   :timeout-ms (or timeout_ms 30000)})}))))

(defn clj-edit-fn
  ([env arg]
   (when-not (map? arg)
     (throw (ex-info "clj_edit requires an opts map"
              {:type :clj/bad-args :got arg
               :examples ["clj_edit({\"path\": \"src/a.clj\", \"op\": \"replace\", \"target\": \"foo\", \"code\": \"(defn foo [] :ok)\"})"
                          "clj_edit({\"path\": \"src/a.clj\", \"op\": \"replace\", \"target\": [\"area\", \":rectangle\"], \"code\": \"...\"})"
                          "clj_edit({\"path\": \"src/a.clj\", \"op\": \"insert_after\", \"target\": \"foo\", \"code\": \"(defn bar [] 1)\"})"
                          "clj_edit({\"path\": \"src/a.clj\", \"op\": \"add\", \"code\": \"(defn baz [] 2)\"})"
                          "clj_edit({\"path\": \"src/a.clj\", \"op\": \"replace_doc\", \"target\": \"foo\", \"code\": \"Doubles x.\"})"
                          "clj_edit({\"path\": \"src/a.clj\", \"op\": \"replace_sexp\", \"target\": \"foo\", \"match\": \"(+ 1 1)\", \"code\": \"(+ 2 2)\"})"]})))
   (extension/success {:result (edit/apply-edit! (env-root env) arg)})))

;; =============================================================================
;; Symbols
;; =============================================================================

(defn- inject-env [env f args] {:env env :fn f :args (into [env] args)})

(def ^{:doc "Manage a workspace nREPL. Positional op: \"status\" (default), \"start\", \"stop\", \"restart\", plus an optional opts dict `{\"dir\": <path>, \"aliases\": [\"dev\", \"test\"]}`. Live nREPL state (ports/liveness/dialect/cwd) already rides in ctx under `:session/env :languages :clojure :nrepl`, refreshed per turn — read it there. \"start\"/\"restart\" self-start a project nREPL subprocess (deps.edn→clojure -M:aliases, project.clj→lein with-profile, bb.edn→bb) in \"dir\" (default workspace root; use a subdir to run a REPL inside e.g. an extension). Self-start is ON by default; set VIS_CLJ_REPL_AUTOSTART falsy to disable. Returns a status map."
       :arglists '([] [op] [op opts])} repl clj-repl-fn)

(def ^{:doc "Evaluate Clojure code in a running nREPL. Accepts a code string or `{\"code\": ..., \"port\": ..., \"host\": ..., \"ns\": ..., \"timeout_ms\": ...}`. Returns `{:value :values :out :err :ns :status :ex :root_ex :ms :port :host :timed_out}`. Default port is auto-discovered from workspace `.nrepl-port`; throws `:clj/no-port` when nothing is running."
       :arglists '([arg])} eval clj-eval-fn)

(def ^{:doc "Structure-aware Clojure edit via rewrite-clj. Opts: `{\"path\": ..., \"op\": ..., \"target\": ..., \"code\": ..., \"match\": ..., \"is_format\": ...}`. `op` ∈ #{\"replace\" \"insert_before\" \"insert_after\" \"add\" \"replace_doc\" \"replace_sexp\"}. \"add\" inserts after \"target\", or appends a new top-level form at EOF when no \"target\" is given. \"replace_doc\" swaps \"target\"'s docstring (inserting one if absent) — here \"code\" is the docstring TEXT, a plain string, not a quoted form. \"target\" is a defn/def name string, `[name, dispatch]` for defmethod, or the wrapping form name for \"replace_sexp\" (use \"match\" for the sexp text to swap). Writes only when the result round-trips parse-clean. `\"is_format\": true` (default) runs zprint before writing."
       :arglists '([opts])} edit clj-edit-fn)

;; Each `:render-fn` is a structured IR builder over the raw
;; `:result` map (see render.clj). The MODEL surface is the Python
;; return value (`pr-str` of the map) — these renderers shape
;; ONLY the channel/TUI preview, never what the LLM reads.

(def repl-symbol
  (vis/symbol #'repl
    {:before-fn inject-env :tag :mutation :render-fn render/render-repl}))

(def eval-symbol
  (vis/symbol #'eval
    {:before-fn inject-env :tag :mutation :render-fn render/render-eval}))

(def edit-symbol
  (vis/symbol #'edit
    {:before-fn inject-env :tag :mutation :render-fn render/render-edit}))

(def clj-symbols
  [repl-symbol eval-symbol edit-symbol])

;; =============================================================================
;; Extension manifest
;; =============================================================================

(def ^:private prompt-text
  (str "Clojure language pack active.\n"
    "Live nREPL state — ports, liveness, dialect (clj/cljs), working dir, and\n"
    "whether vis manages each one (managed, with tool/pid/aliases) — already\n"
    "rides in ctx under `:session/env :languages :clojure :nrepl`, refreshed every\n"
    "turn. Read it there; there is no ports tool to call.\n\n"
    "Tools under the `clj` alias:\n"
    "  clj_repl() | clj_repl(\"status\"|\"start\"|\"stop\"|\"restart\", opts)\n"
    "                                      Manage a workspace nREPL. \"status\" (default) +\n"
    "                                      \"stop\" are always allowed. \"start\"/\"restart\" self-start\n"
    "                                      a project nREPL subprocess (ON by default; set\n"
    "                                      " repl-manager/flag-env " falsy to disable).\n"
    "                                      opts: {\"dir\": <subdir>, \"aliases\": [\"dev\", \"test\"]} — run the\n"
    "                                      REPL in a subdir (e.g. an extension) with deps.edn\n"
    "                                      aliases / lein profiles. e.g.\n"
    "                                      clj_repl(\"start\", {\"dir\": \"extensions/...\", \"aliases\": [\"dev\"]})\n"
    "  clj_eval(\"...\") | clj_eval({\"code\": ..., \"port\": ..., \"ns\": ..., \"timeout_ms\": ...})\n"
    "                                      port is optional — auto-discovered from the\n"
    "                                      workspace `.nrepl-port` when omitted.\n"
    "  clj_edit({\"path\": ..., \"op\": ..., \"target\": ..., \"code\": ..., \"match\": ..., \"is_format\": ...})\n"
    "      op ∈ \"replace\" \"insert_before\" \"insert_after\" \"add\" \"replace_doc\" \"replace_sexp\".\n"
    "      \"add\" = insert_after target, or append at EOF when no target. \"replace_doc\"\n"
    "      swaps target's docstring (code = doc text, plain string). \"replace_sexp\"\n"
    "      swaps the match sexp inside target.\n"
    "For structure exploration use `rg` (with `context`) + the engine `doc` / `apropos` system calls — there is no clj outline/find tool.\n"
    "Use clj_edit for Clojure def/defmethod changes — it is name-addressed and round-trip-validated; prefer it over `patch` for `.clj/.cljc/.cljs`. Use clj_eval to verify behaviour against the running REPL before claiming a fix.\n"
    "clj_edit is STRUCTURE-AWARE (rewrite-clj): it edits the form, so it CANNOT leave unbalanced delimiters — you never count parens with it. If you instead hand-write Clojure via write/patch and a `.clj` won't parse, the cause is almost always an unbalanced ( [ {; fix it STRUCTURALLY (redo the change via clj_edit), don't hand-count brackets. After editing a `.clj`, VERIFY it still parses (clj_eval a load-file or eval the form); do NOT blanket-reformat the file — that buries a surgical change in unrelated layout churn."))

(def vis-extension
  (vis/extension
    {:ext/name           "language-clojure"
     :ext/description    "Clojure language pack: live nREPL state in ctx, nREPL eval (clj_eval), flag-gated REPL lifecycle (clj_repl self-start), structure-aware edits (clj_edit via rewrite-clj). Activates only when the workspace has Clojure sources."
     :ext/version        "0.1.0"
     :ext/author         "Blockether"
     :ext/owner          "vis"
     :ext/license        "Apache-2.0"
     :ext/activation-fn  activation-fn
     :ext/engine            {:ext.engine/alias 'clj
                          :ext.engine/symbols clj-symbols}
     :ext/env            [{:name        repl-manager/flag-env
                           :label       "Self-start nREPL"
                           :description "Controls whether clj_repl(\"start\") may launch a project nREPL subprocess (deps.edn→clojure, project.clj→lein, bb.edn→bb). ON by default; set to a falsy value (0/false/no/off) to disable self-start."
                           :secret?     false
                           :required?   false}]
     :ext/prompt         (fn [_env] prompt-text)
     :ext/ctx            nrepl-ctx/contribute
     :ext/kind           "language"}))

(vis/register-extension! vis-extension)
