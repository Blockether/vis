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
  (:refer-clojure :exclude [eval test format])
  (:require
   [clojure.java.io :as io]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.internal.foundation.environment.languages :as languages]
   [com.blockether.vis.ext.language-clojure.edit :as edit]
   [com.blockether.vis.ext.language-clojure.format :as fmt]
   [com.blockether.vis.ext.language-clojure.paren-repair :as repair]
   [com.blockether.vis.ext.language-clojure.nrepl-client :as nrepl-client]
   [com.blockether.vis.ext.language-clojure.nrepl-ctx :as nrepl-ctx]
   [com.blockether.vis.ext.language-clojure.ports :as ports]
   [com.blockether.vis.ext.language-clojure.render :as render]
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

(defn- repl-resource-id [dir] (str "nrepl:" dir))

(defn register-repl-resource!
  "Mirror a managed nREPL into the session-scoped resource registry so it shows
   in ctx (resources) + the footer, and can be stopped/restarted by id
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
       :owner  :ext/language-clojure
       :language :clojure}
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
        (->> (:aliases (clojure.edn/read-string (slurp f)))
          keys (map name) sort vec)
        []))
    (catch Throwable _ [])))

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
       (throw (ex-info "no nREPL port found — call clj_repl(\"start\") to boot a project nREPL now (autostart is ON by default), or clj_repl() to inspect candidates"
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

(defn clj-paren-repair-fn
  ([arg]
   (let [code  (cond (string? arg) arg
                 (and (map? arg) (contains? arg :code)) (str (:code arg))
                 :else (throw (ex-info "clj_paren_repair expects a code string or {\"code\": ...}"
                                {:type :clj/bad-args :got arg
                                 :examples ["clj_paren_repair(\"(defn f [] (+ 1 2)\")"
                                            "clj_paren_repair({\"code\": \"...\"})"]})))
         fixed (repair/fix-delimiters code)]
     (extension/success
       {:result {:op        :clj-paren-repair
                 :repaired? (some? fixed)
                 :changed?  (boolean (and fixed (not= fixed code)))
                 :text      (or fixed code)}}))))

(defn clj-format-fn
  ([arg]
   (let [code (cond
                (string? arg)                          arg
                (and (map? arg) (contains? arg :code)) (str (:code arg))
                :else (throw (ex-info "clj_format expects a code string or {\"code\": ...}"
                               {:type :clj/bad-args :got arg
                                :examples ["clj_format(\"(defn f [x]\\n(* x 2))\")"
                                           "clj_format({\"code\": \"...\"})"]})))
         out  (fmt/format-string code)]
     (extension/success
       {:result {:op       :clj-format
                 :changed? (not= out code)
                 :text     out}}))))

;; =============================================================================
;; Symbols
;; =============================================================================

(defn- inject-env [env f args] {:env env :fn f :args (into [env] args)})

(def ^{:doc "Manage a workspace nREPL. Positional op: \"status\" (default), \"start\", \"stop\", \"restart\", plus an optional opts dict `{\"dir\": <path>, \"aliases\": [\"dev\", \"test\"]}`. Live nREPL state (ports/liveness/dialect/cwd) already rides in ctx under `:session/env :languages :clojure :nrepl`, refreshed per turn — read it there. \"start\"/\"restart\" self-start a project nREPL subprocess (deps.edn→clojure -M:aliases, project.clj→lein with-profile, bb.edn→bb) in \"dir\" (default workspace root; use a subdir to run a REPL inside e.g. an extension). Self-start is ON by default; set VIS_CLJ_REPL_AUTOSTART falsy to disable. Returns a status map."
       :arglists '([] [op] [op opts])} repl clj-repl-fn)

(def ^{:doc "Evaluate Clojure code in a running nREPL. Accepts a code string or `{\"code\": ..., \"port\": ..., \"host\": ..., \"ns\": ..., \"timeout_ms\": ...}`. Returns `{:value :values :out :err :ns :status :ex :root_ex :ms :port :host :timed_out}`. Default port is auto-discovered from workspace `.nrepl-port`; throws `:clj/no-port` when nothing is running."
       :arglists '([arg])} eval clj-eval-fn) (def ^{:doc "Run tests for ONE or MANY namespaces with lazytest-modeled selectors. Accepts a namespace string, or a dict {\"ns\": <str OR list>, \"only\": [test-names], \"include\": [tags], \"exclude\": [tags]}. SELECTORS: only filters to vars whose name matches; include/exclude partition by metadata tag (a ^:slow / ^:integration var-meta key) - exclude OVERRIDES include. Uses the live nREPL when a port is discoverable (fast loop; framework auto-detected: clojure.test deftest -> clojure.test, otherwise lazytest) and OWNS the :reload; with no reachable nREPL it falls back to clojure -M:test (selectors do not apply there). Returns {:language \"clojure\" :mode \"repl\"|\"cli\" :framework :ns :total :pass :fail :selected :skipped :failures [{:ns :test :message :file :line}]} (cli mode carries :exit/:output/:note). Built through the shared com.blockether.vis.internal.test-contract so a future language pack returns the same shape.", :arglists (quote ([arg]))} test test-runner/clj-test-fn)

(def ^{:doc "Structure-aware Clojure edit via rewrite-clj. Opts: `{\"path\": ..., \"op\": ..., \"target\": ..., \"code\": ..., \"match\": ..., \"is_format\": ...}`. `op` ∈ #{\"replace\" \"insert_before\" \"insert_after\" \"add\" \"replace_doc\" \"replace_sexp\"}. \"add\" inserts after \"target\", or appends a new top-level form at EOF when no \"target\" is given. \"replace_doc\" swaps \"target\"'s docstring (inserting one if absent) — here \"code\" is the docstring TEXT, a plain string, not a quoted form. \"target\" is a defn/def name string, `[name, dispatch]` for defmethod, or the wrapping form name for \"replace_sexp\" (use \"match\" for the sexp text to swap). Writes only when the result round-trips parse-clean. Replacement `code` is parinfer-repaired for simple delimiter slips before parsing, then cljfmt-formatted when `\"is_format\": true`; if repair is needed the result carries `:repaired? true`. Still write `code` as properly-formatted, MULTI-LINE Clojure: put the body and nested forms on their own indented lines — do NOT collapse a whole defn/let/when onto a single line. Trivial one-form bodies may stay on one line. cljfmt fixes indentation/whitespace but does NOT reflow a one-liner into multiple lines, so the line breaks must come from your `code`."
       :arglists '([opts])} edit clj-edit-fn)

(def ^{:doc "Balance the delimiters of a Clojure source STRING (parinfer indent-mode — it trusts your INDENTATION to place the missing/extra ( [ {). Takes a code string or `{\"code\": ...}`; returns `{:op :clj-paren-repair :repaired? bool :changed? bool :text <fixed source>}`. PURE — it does not touch any file; you write the returned :text yourself via write/patch. Use it when you hand-wrote Clojure and a `.clj` won't parse, instead of counting brackets. For edits to EXISTING valid code prefer clj_edit (structure-aware — it can't unbalance)."
       :arglists '([arg])} paren-repair clj-paren-repair-fn)

(def ^{:doc "Pretty-print a Clojure source STRING with cljfmt (indentation + whitespace). Takes a code string or `{\"code\": ...}`; returns `{:op :clj-format :changed? bool :text <formatted source>}`. PURE — it does not touch any file; you write the returned :text yourself via write/patch. NOTE: cljfmt fixes indentation but does NOT reflow a one-liner into multiple lines — write the source multi-line. Use it to tidy Clojure you hand-wrote (via write/patch) before saving. clj_edit ALREADY formats on write, so prefer it for `.clj/.cljc/.cljs` edits; do NOT blanket-reformat existing files (it buries surgical changes in layout churn)."
       :arglists '([arg])} format clj-format-fn)

;; Each `:render-fn` is a structured IR builder over the raw
;; `:result` map (see render.clj). The MODEL surface is the Python
;; return value (`pr-str` of the map) — these renderers shape
;; ONLY the channel/TUI preview, never what the LLM reads.

(def repl-symbol
  (vis/symbol #'repl
    {:before-fn inject-env :tag :mutation :render-fn render/render-repl}))

(def eval-symbol
  (vis/symbol #'eval
    {:before-fn inject-env :tag :mutation :render-fn render/render-eval})) (def test-symbol (vis/symbol (var test) {:before-fn inject-env, :tag :mutation, :render-fn render/render-test}))

(def edit-symbol
  (vis/symbol #'edit
    {:before-fn inject-env :tag :mutation :render-fn render/render-edit}))

;; Tagged `:mutation` (alongside repl/eval/edit): it's a write-path tool —
;; the model calls it to produce Clojure source it is about to write, so it's
;; a decision-affecting action, not a read. `:mutation` also keeps it out of
;; the observation cache (no collapse of repeated repairs) and lets the
;; `(done …)`-as-proposal gate treat it like the other clj write tools.
(def paren-repair-symbol
  (vis/symbol #'paren-repair
    {:tag :mutation :render-fn render/render-paren-repair}))

;; `:mutation` like paren-repair — a write-path helper the model calls to
;; produce source it is about to write, not a read.
(def format-symbol
  (vis/symbol #'format
    {:tag :mutation :render-fn render/render-format}))

(def clj-symbols
  [repl-symbol eval-symbol edit-symbol paren-repair-symbol format-symbol test-symbol])

;; =============================================================================
;; Extension manifest
;; =============================================================================

(def ^:private prompt-text
  (str "Clojure language pack active.\n"
    "Live nREPL state — ports, liveness, dialect (clj/cljs), working dir, and\n"
    "whether vis manages each one (managed, with tool/pid/aliases) — already\n"
    "rides in ctx under `ctx[\"env\"][\"languages\"][\"clojure\"][\"nrepl\"]`, refreshed\n"
    "every turn. Read it there; there is no ports tool to call.\n\n"
    "These `clj_*` tools are MUTATIONS (clj_edit/clj_eval/clj_test/clj_format/\n"
    "clj_repl start) — batch them with your patches in a mutation reply.\n\n"
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
    "      swaps the match sexp inside target. Replacement code is parinfer-repaired before parsing. WRITE code as multi-line Clojure —\n"
    "      cljfmt fixes indentation on write but will NOT un-collapse a one-liner.\n"
    "      Formatting is SCOPED: only the edited/inserted form is cljfmt'd, so\n"
    "      unrelated (even non-cljfmt-clean) forms in the file stay byte-identical.\n"
    "      replace_sexp is WHOLE-FORM-scoped (matches a sexp at ANY depth inside\n"
    "      target); insert_after / add place a NEW TOP-LEVEL form (on its own line).\n"
    "  clj_test(\"my.app.core-test\") | clj_test({\"ns\": ..., \"only\": [...], \"include\": [...], \"exclude\": [...]})\n"
    "                                      Run tests for ONE or MANY namespaces (ns = string OR list).\n"
    "                                      Live nREPL when a port exists (fast; framework auto-detected:\n"
    "                                      clojure.test deftest OR lazytest); else falls back to clojure -M:test.\n"
    "                                      SELECTORS (lazytest-modeled): only=[test names] runs just those;\n"
    "                                      include/exclude=[metadata tags] partition by ^:tag (exclude wins).\n"
    "                                      Owns the :reload. Result has :mode (repl/cli), :selected/:skipped/:total;\n"
    "                                      failures carry file:line.\n"
    "  clj_format(\"<source>\") | clj_format({\"code\": ...})\n"
    "                                      cljfmt-format a Clojure source STRING (indentation/whitespace);\n"
    "                                      returns :text. PURE — write the :text yourself. Use it to tidy\n"
    "                                      Clojure you hand-wrote via write/patch; write the source MULTI-LINE\n"
    "                                      (cljfmt won't un-collapse a one-liner). Do NOT blanket-reformat\n"
    "                                      existing files — clj_edit already formats on write.\n"
    "For structure exploration use `rg` (with `context`) + the engine `doc` / `apropos` system calls — there is no clj outline/find tool.\n"
    "Use clj_edit for Clojure def/defmethod changes - it is name-addressed and round-trip-validated; prefer it over `patch` for `.clj/.cljc/.cljs`.\n"
    "VERIFY IN THE REPL, ALWAYS: the running nREPL is your source of truth. After EVERY Clojure change, clj_eval the touched fn (load-file / require :reload + call it on a real input) BEFORE claiming a fix. Reproduce reported bugs with clj_eval first, and prove the fix the same way.\n"
    "If NO nREPL is up (ctx shows no live port and clj_eval cannot connect), do NOT skip verification - START one with clj_repl(\"start\") (self-start is ON by default), wait for the port, then verify. Only when self-start is disabled or the start fails should you report the change as unverified, saying exactly why.\n"

    "clj_edit is STRUCTURE-AWARE (rewrite-clj): it edits the form, so it CANNOT leave unbalanced delimiters — you never count parens with it. It also auto-runs the parinfer repairer on replacement code before parsing, so a simple missing delimiter in the snippet is fixed before cljfmt/round-trip validation. If you instead hand-write Clojure via write/patch and a `.clj` won't parse, the cause is almost always an unbalanced ( [ {; fix it STRUCTURALLY (redo the change via clj_edit), don't hand-count brackets. After editing a `.clj`, VERIFY it still parses (clj_eval a load-file or eval the form); do NOT blanket-reformat the file — that buries a surgical change in unrelated layout churn.\n"
    "If you DID hand-write Clojure and the delimiters are off, call clj_paren_repair(\"<source>\") — it balances ( [ { from your INDENTATION (parinfer) and returns the fixed `:text`; write that, don't count brackets by hand."))
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
     :ext/language-tools [{:language :clojure
                           :format-fn (fn [_env arg]
                                        (clj-format-fn arg))
                           :test-fn test-runner/clj-test-fn
                           :repl-eval-fn clj-eval-fn
                           :start-repl-fn (fn [env op opts]
                                            (clj-repl-fn env op opts))}]
     ;; Declarative startable resource — the Resources UI (web modal / TUI F4)
     ;; renders this generically: its title, the proposed deps.edn aliases, and
     ;; Start. Always allowed (the self-start flag gates only the model).
     :ext/startable-resources
     [{:kind          :nrepl
       :label         "nREPL"
       :options-label "aliases"
       :options-fn    (fn [env] (mapv #(str ":" %) (available-aliases env)))
       :start-fn      (fn [env selected]
                        (ui-start-repl! env (map #(clojure.string/replace (str %) #"^:" "") selected)))}]
     :ext/kind           "language"}))

(vis/register-extension! vis-extension)
