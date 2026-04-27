(ns com.blockether.vis.loop.core
  "Environment lifecycle + system-prompt assembly.

   This namespace is intentionally small. It owns:

     * Environment construction/disposal (`create-environment`,
       `dispose-environment!`).
     * Extension registration (`register-extension!`).
     * The single source of truth for the agent system prompt
       (`CORE_SYSTEM_PROMPT`, `build-system-prompt`,
       `assemble-system-prompt`).
     * Extension activation (`active-extensions`) \u2014 fires
       `:ext/activation-fn` exactly ONCE per call. Thread the resulting
       vec through `assemble-system-prompt` AND every per-iteration
       call so activation never re-evaluates inside a single query.
     * Auto-forget bookkeeping (`auto-forget-candidates`,
       `auto-forget-stale-vars!`).

   The iteration loop, per-iteration context assembly, code execution,
   and result formatting all live in
   `loop.runtime.conversation.environment.query.{core,iteration.core}`.
   Do not move them back here \u2014 the previous monolithic version was a
   2400-line god file with shadow copies of every helper."
  (:require
   [clojure.string :as str]
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.svar.internal.util :as util]
   [com.blockether.vis.persistance.core :as db
    :refer [create-rlm-conn dispose-rlm-conn!]]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as sci-env]
   [com.blockether.vis.extension :as ext]
   [sci.core :as sci]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- format-exception-short [^Throwable t]
  {:class (.getName (class t)) :message (or (ex-message t) (str t))})

(defn- resolve-effective-model
  "Best-effort root model descriptor from router config."
  [router]
  (first (mapcat :models (:providers router))))

(defn provider-has-reasoning?
  "True when the configured root model exposes reasoning support."
  [router]
  (boolean (:reasoning? (resolve-effective-model router))))

(defn get-locals
  "Returns {sym \u2192 val} of user-defined vars in the SCI sandbox
   (excludes built-ins / kw-keyed entries). Direct atom read \u2014 zero
   eval overhead."
  [{:keys [sci-ctx initial-ns-keys]}]
  (try
    (let [sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox])]
      (persistent!
        (reduce-kv (fn [acc k v]
                     (if (or (contains? initial-ns-keys k) (keyword? k))
                       acc
                       (assoc! acc k (if (instance? clojure.lang.IDeref v) @v v))))
          (transient {}) sandbox)))
    (catch Exception e
      (tel/log! {:level :warn :id ::get-locals-fallback
                 :data {:error (ex-message e)}
                 :msg "Failed to read sandbox locals, returning empty map"})
      {})))

;; =============================================================================
;; Public env accessors
;; =============================================================================

(defn db-info
  "Current db-info map for env. Nil-safe."
  [env]
  (:db-info env))

(defn custom-bindings
  "Current custom SCI bindings {sym -> value}."
  [env]
  (some-> (:state-atom env) deref :custom-bindings))

;; =============================================================================
;; System Prompt
;; =============================================================================

(defn- environment-block
  "Runtime environment context: CWD, home, user, platform, shell."
  []
  (let [cwd   (System/getProperty "user.dir")
        os    (System/getProperty "os.name")
        arch  (System/getProperty "os.arch")
        shell (or (System/getenv "SHELL") "unknown")
        user  (System/getProperty "user.name")
        home  (System/getProperty "user.home")]
    (str "<environment>\n"
      "  Working directory: " cwd "\n"
      "  Home: " home "\n"
      "  User: " user "\n"
      "  Platform: " os " (" arch ")\n"
      "  Shell: " shell "\n"
      "</environment>\n")))

(def ^:private CORE_SYSTEM_PROMPT
  "Clojure SCI agent. SATISFY THE USER'S QUERY. Stop when answered.

ONE RULE: you model your context via calls. Reasoning happens in :code, not prose.
`(+ 2 2)` beats \"I think 4\". Asserts always have a message.

EVERY ITERATION:

  STEP 0 — PLAN. Read <plan>.
    • Empty? → EMIT :plan in this iteration. ≤3–7 items, exactly ONE :in-progress.
      Schema: {:goal \"…\" :items [{:id N :content \"…\" :status :in-progress
      :evidence \"iN.K\"} …] :open [\"…\"] :decided [\"…\"]}.
    • Reality forces a real change of approach? → RE-EMIT :plan with the change.
      The loop renders a [plan changed] line in <breadcrumbs> so you can SEE
      your own change next iteration. Don't drift silently.
    • Otherwise leave :plan absent. Your plan carries forward verbatim.
      Persisted plan == your plan. There is no plan-in-thinking; the plan is
      a first-class slot.
    Required: emit :plan AT MINIMUM on iteration 0 of any non-trivial task.

  STEP 1 — READ.
    <plan>           your sticky structured TODO list (you wrote this).
    <breadcrumbs>    last ≤20 one-liners YOU wrote in :breadcrumb. Past tense.
    <recent>         last iteration's :code results, addressable as iN.K.
    <recent_thought> last iteration's :thinking text. ALWAYS the most recent only.
    <system_state>   QUERY (current user request), ANSWER (last turn's final),
                     REASONING (your last iteration's thinking), and
                     PRIOR_TURN (digest of previous turn: goal/counts/outcome).
                     UPPERCASE names, no earmuffs. SYSTEM_VAR_NAMES = #{QUERY ANSWER REASONING}.
    <attempts>       deduped log of distinct (call args) results across this turn.
                     Reference results by iN.K id. NEVER repeat a successful call;
                     read its result here instead.
    <var_index>      every `(def name val)` you've written. Survives until `:forget`.
                     Type-aware: cheap values inline (`(def s \"hello\")`); large
                     values show schema (`(def m {:n 42 :keys-sample [:a :b]})`).
                     Stats live in a `;;` comment line above each entry.
    <vars_archive>   names of `:forget`'d / persisted-only vars; call
                     `(var-history 'sym)` to recover the body.
    If <plan>+<recent>+<system_state> already answers the query → STEP 4.
    Otherwise → STEP 2.

  STEP 2 — COMPUTE in :code. State the missing piece as a CLAIM and verify it.
    `(doc fn)` for tool docs. `(shape x)` for schema-only view of any value.
    When a `[system_nudge]` appears, follow it. Nudges are actionable directives.

    DOC YOUR DEFS. Every code block is a `:code_block` map with optional
    `:doc`; when `:expr` is `(def name val)` or `(defn name [args] body)`,
    set `:doc` to a one-line description of the var's purpose. The loop
    attaches it as :doc metadata; `<var_index>` then shows it next to the
    var. Future iterations — yours and the next turn's — read those docs
    from `<var_index>` instead of having to re-derive what each var is for.

    KEEP `:expr` CLEAN. Do NOT use Clojure's native
    `(def name \"docstring\" val)` / `(defn name \"doc\" [args] body)` form;
    that's redundant with the structured `:doc` field and bug-prone
    (a real value that happens to be a string gets eaten as the
    docstring, leaving the var bound to nothing). Always:
      :expr `(def name val)` or `(defn name [args] body)`
      :doc  \"one-line description\".

  STEP 3 — REPORT.
    Emit :breadcrumb (≤120 chars, past tense). Cite the active item id, what
    happened, and (when relevant) which iN.K result advanced it.
      e.g. `i3  [3] grep yielded 12 raw matches; filtering`
           `i4  [3] narrowed to 4 real callers (i4.2)`
    The breadcrumb chain is HOW YOU REMEMBER. Skipping it loses tactical
    history past one iteration.

  STEP 4 — FINALIZE.
    Conditions: ALL <plan>.items :done OR goal achieved with cited evidence.
    Set :answer. :code still runs first.
    If you must stop without finishing, set :answer AND :abandon-reason.
      e.g. :abandon-reason \"need access to GitHub API token to verify [4]\"
    The loop rejects :answer with open items unless :abandon-reason is set.
    The loop runs until you emit :answer -- there is NO iteration
    budget you need to track. Stop when the work is done.

DIRECT ANSWER (greetings, plain prose): empty :code `[]`, no :plan needed,
  emit :answer immediately.

:ANSWER. A Mustache template (markdown allowed). Plain text without
  `{{...}}` tags renders verbatim. To inject a computed value, def it
  in :code and reference it: `(def summary (str/join \"\\n\" rows))`
  in :code, then :answer `\"{{summary}}\"`.

GROUNDING. Every claim in :answer cites a slot:
  • a <plan>-item id ([N]) the item proves
  • a result id (iN.K) from <recent> or <attempts>
  • a named *var* in <var_index>
  No fabrication.

QUERY PRIMACY. QUERY is the CURRENT user request. It overrides EVERYTHING
  in ANSWER / REASONING from prior turns.

PERF (ms). def=100, assert=500, heavy=2000, grep/list=5000, max 10000.
  Compute, don't scan.

TOOL DISCIPLINE.
  • ONE broad grep beats many narrow ones. Use alternation: `(grep \"foo|bar|baz\" \".\")`.
  • NEVER repeat a call shown in <attempts>. Read iN.K. The loop dedups
    identical calls automatically and tells you `[deduped from iN.K]` —
    that's a free, instant result.
  • DEF a result only when a future iteration or :answer needs it. The
    loop auto-pins file paths and patches in <attempts> already;
    don't def-spam those.

<style_appendix>  ;; consult only when you trip on syntax.
  Recursion: `letfn`, never `(let [f (fn [] (f))] ...)`.
  `iterate` takes ONE-arg fn. Destructure pairs: `(fn [[a b]] ...)`.
  Prefer `(fn [x] ...)` over `#()`. Nested `#()` is illegal.
  Eager > lazy: `mapv` `filterv` `reduce` `into`.
  Quote lists: `'(1 2 3)`. One complete expr per block.
  ALWAYS test. Untested = wrong. No repeat fail → different approach.
  No prose in :code. Bare string literal = wrong. Prose → :answer.
  Simplest solution. No over-eng.
</style_appendix>

OUTPUT: Factual, direct, concise. No AI filler. No hedging. Tables/lists over prose.")

(defn build-system-prompt
  "Build the core system prompt: agent rules + environment block +
   optional caller-provided instructions. Does NOT include extension
   prompts \u2014 use `assemble-system-prompt`.

   Note: deliberately NO embedded timestamp. Date dies in the prompt
   prefix (kills cache hits at second granularity); place it in the
   per-iteration user block when the agent actually needs to know."
  [{:keys [system-prompt]}]
  (str CORE_SYSTEM_PROMPT
    "\n"
    (environment-block)
    (when (and system-prompt (not (str/blank? system-prompt)))
      (str "\nINSTRUCTIONS:\n" system-prompt "\n"))))

(defn active-extensions
  "Returns the seq of registered extensions whose `:ext/activation-fn`
   returns truthy for `environment`, in registration order.

   This is the SINGLE source of truth for activation. Call it ONCE at
   the top of a query and thread the resulting vec through both
   `assemble-system-prompt` and every per-iteration consumer
   (`build-iteration-context`, nudge collectors, etc). Do NOT re-deref
   `(:extensions environment)` and refilter \u2014 activation must fire
   exactly once per query.

   Errors thrown by an `:ext/activation-fn` are logged and treated as
   inactive: a buggy extension cannot break query execution."
  [environment]
  (when-let [exts (some-> (:extensions environment) deref seq)]
    (vec
      (filter (fn [ext]
                (try
                  (boolean ((:ext/activation-fn ext) environment))
                  (catch Throwable t
                    (tel/log! {:level :error :id ::ext-activation-error
                               :data {:ext (:ext/namespace ext)
                                      :error (ex-message t)}}
                      (str "Extension '" (:ext/namespace ext) "' activation-fn threw; treating as inactive"))
                    false)))
        exts))))

(defn- render-extension-prompt-block
  "Renders one extension's contribution to the system prompt: canonical
   docstring/arglist block + optional `:ext/prompt` extra body, prefixed
   with `[namespace: alias \u2192 fqn]` when an alias is set. Returns nil
   when the extension contributes no text."
  [environment ext]
  (try
    (let [canonical (ext/render-prompt ext)
          extra-fn  (:ext/prompt ext)
          extra     (when extra-fn (extra-fn environment))
          body      (->> [canonical extra]
                      (filter #(and (string? %) (not (str/blank? %))))
                      (str/join "\n"))]
      (when-not (str/blank? body)
        (if-let [{ns-sym :ns alias-sym :alias} (:ext/ns-alias ext)]
          (str "[namespace: " alias-sym " \u2192 " ns-sym "]\n" body)
          body)))
    (catch Throwable t
      (tel/log! {:level :error :id ::ext-prompt-error
                 :data {:ext (:ext/namespace ext)
                        :error (ex-message t)}}
        (str "Extension '" (:ext/namespace ext) "' prompt rendering failed"))
      nil)))

(defn assemble-system-prompt
  "Build the full system prompt: core agent rules + active-extension
   prompts. SINGLE source of truth for what goes into `{:role \"system\"}`.

   Layout:
     [CORE_SYSTEM_PROMPT][\\n][environment-block][\\n INSTRUCTIONS:\\n caller-system]
     [\\n\\n][ext-1 prompt][\\n\\n][ext-2 prompt]...

   Everything is one stable string \u2014 callers passing
   `{:cache-system? true}` to svar's `ask!` get the entire blob cached
   as one breakpoint.

   Required opts:
     `:active-extensions` \u2014 vec returned by `(active-extensions env)`.
       Caller MUST compute this exactly ONCE per query and pass it in;
       this fn does NOT re-evaluate `:ext/activation-fn`. Throws when
       the key is missing to keep activation single-path.

   Optional:
     `:system-prompt` \u2014 caller-provided instructions text."
  [environment {:keys [system-prompt active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (anomaly/incorrect!
      "assemble-system-prompt requires :active-extensions \u2014 compute via (active-extensions env) once per query"
      {:type :vis/missing-active-extensions}))
  (let [base   (build-system-prompt {:system-prompt system-prompt})
        ext-ps (seq (keep #(render-extension-prompt-block environment %) active-extensions))]
    (if ext-ps
      (str base "\n\n" (str/join "\n\n" ext-ps))
      base)))

;; =============================================================================
;; Auto-Forget
;; =============================================================================

(defn- system-var-sym?
  "Local alias — the canonical predicate lives in `sci-env`. Kept here
   as a private helper so this file's auto-forget code reads cleanly
   without an extra namespace bounce on every call."
  [sym]
  (sci-env/system-var-sym? sym))

(defn- forget-vars!
  "Unmap `names` from the SCI sandbox namespace. Used by the
   deterministic auto-forget at query boundaries.

   HARD GUARD: SYSTEM vars (QUERY, REASONING, ANSWER, ... see `sci-env/SYSTEM_VAR_NAMES`)
   can NEVER be forgotten \u2014 they are contract surfaces the iteration
   loop re-binds every turn; dropping them would tear the sandbox
   mid-turn. Filtered out + logged."
  [sci-ctx names]
  (let [raw-syms (keep (fn [n]
                         (cond (symbol? n) n
                           (string? n) (try (symbol n) (catch Throwable _ nil))
                           :else       nil))
                   names)
        {system-syms true user-syms false} (group-by (comp boolean system-var-sym?) raw-syms)]
    (when (seq system-syms)
      (tel/log! {:level :info :id ::forget-system-var-refused
                 :data {:requested (mapv str system-syms)}
                 :msg "Refusing to forget SYSTEM vars (*foo*) \u2014 ignoring those names"}))
    (when (seq user-syms)
      (try
        (swap! (:env sci-ctx) update-in [:namespaces 'sandbox]
          (fn [ns-map] (apply dissoc ns-map user-syms)))
        (catch Throwable e
          (tel/log! {:level :debug :id ::forget-vars-failed
                     :data {:error (ex-message e) :syms (mapv str user-syms)}
                     :msg "forget-vars! failed \u2014 skipping"}))))))

(def ^:const AUTO_FORGET_STALE_QUERIES
  "Number of recent queries a var must have been defined/redefined in to
   survive auto-forget. Vars without a docstring that were last touched
   more than this many queries ago are evicted at the start of each new
   query. DB rows are untouched \u2014 `(var-history 'sym)` still works."
  3)

(defn auto-forget-candidates
  "Pure function. Returns the set of sandbox var symbols that should be
   auto-forgotten at the start of a new query.

   A var is a candidate when ALL of:
   1. It is a user var (not in `initial-ns-keys`).
   2. It is not a SYSTEM var (per `sci-env/SYSTEM_VAR_NAMES`).
   3. It has NO docstring (runtime SCI meta `:doc` is nil/blank).
   4. It was last defined/redefined in a query that is NOT among the
      `recent-query-ids`.

   Params:
   - `sandbox-map`      \u2014 SCI sandbox namespace map {symbol \u2192 value-or-var}
   - `initial-ns-keys`  \u2014 set of symbols that are built-in tools/helpers
   - `var-registry`     \u2014 result of `db-latest-var-registry`:
                          {symbol \u2192 {:query-id ... :value ... :code ...}}
   - `recent-query-ids` \u2014 set of query UUIDs for the last N queries

   Returns: set of symbols to forget."
  [sandbox-map initial-ns-keys var-registry recent-query-ids]
  (let [recent-ids (set recent-query-ids)]
    (into #{}
      (filter
        (fn [sym]
          (let [v (get sandbox-map sym)
                doc (:doc (meta v))
                has-doc? (and doc (not (str/blank? doc)))
                reg-entry (get var-registry sym)
                defining-query-id (:query-id reg-entry)]
            (and
              (not (contains? initial-ns-keys sym))
              (not (system-var-sym? sym))
              (not has-doc?)
              (some? reg-entry)
              (not (contains? recent-ids defining-query-id))))))
      (keys sandbox-map))))

(defn auto-forget-stale-vars!
  "Deterministic cleanup at the query boundary: remove sandbox vars that
   (a) have no docstring AND (b) were last defined/redefined more than
   `AUTO_FORGET_STALE_QUERIES` queries ago. Replaces the unreliable
   ask-the-LLM-to-emit-`:forget` pattern for scratch vars. DB rows are
   untouched \u2014 `(var-history 'sym)` can inspect old values."
  [{:keys [db-info conversation-id sci-ctx initial-ns-keys var-index-atom]}]
  (when (and db-info conversation-id sci-ctx)
    (try
      (let [all-queries  (sort-by :created-at
                           (db/db-list-conversation-queries db-info conversation-id))
            recent-ids   (into #{} (map :id) (take-last AUTO_FORGET_STALE_QUERIES all-queries))
            var-registry (db/db-latest-var-registry db-info conversation-id)
            sandbox-map  (get-in @(:env sci-ctx) [:namespaces 'sandbox])
            candidates   (auto-forget-candidates sandbox-map initial-ns-keys
                           var-registry recent-ids)]
        (when (seq candidates)
          (tel/log! {:level :info :id ::auto-forget
                     :data {:forgotten (mapv str candidates) :count (count candidates)}
                     :msg (str "Auto-forget: evicting " (count candidates) " stale vars without docstrings")})
          (forget-vars! sci-ctx candidates)
          (when var-index-atom
            (swap! var-index-atom update :current-revision inc))))
      (catch Exception e
        (tel/log! {:level :warn :id ::auto-forget-failed
                   :data {:error (ex-message e)}
                   :msg "Auto-forget failed \u2014 skipping"})))))

;; =============================================================================
;; Environment Lifecycle
;; =============================================================================

;; `create-environment` calls `register-extension!` indirectly via
;; `ext/register-extensions!`. Forward-declare so the symbol resolves at
;; load time even though the def comes later in the file.
(declare register-extension!)

(defn create-environment
  "Creates a vis environment (component) for conversation lifecycle and
   querying.

   The environment holds:
     - SCI sandbox context with custom bindings + var-index cache
     - DB connection (or shared-mem datasource)
     - Router (LLM provider config)
     - Extension registry atom

   Params:
     `router` \u2014 Required. Result of `llm/make-router`.
     `opts`   \u2014 Map with `:db` and optional `:conversation`,
                 `:channel`, `:external-id`, `:title`.

     `:db` accepted forms:
       nil               \u2014 no DB (SCI-only execution)
       :memory           \u2014 ephemeral in-process SQLite DB
       path string       \u2014 persistent SQLite DB at path
       {:path p}         \u2014 persistent SQLite DB at path
       {:datasource ds}  \u2014 caller-owned DataSource (not closed on dispose)

   Returns the vis environment map."
  [router {:keys [db conversation channel external-id title]}]
  (when-not router
    (anomaly/incorrect! "Missing router" {:type :vis/missing-router}))
  (let [depth-atom               (atom 0)
        db-info                  (create-rlm-conn db)
        var-index-atom           (atom {:index nil :revision -1 :current-revision 0})
        autobind-events-atom     (atom [])
        autobind-registry-atom   (atom {})
        state-atom               (atom {:custom-bindings {}
                                        :environment     nil
                                        :conversation-id nil})
        environment-atom         (atom nil)
        environment-id           (str (util/uuid))
        root-model               (or (:name (resolve-effective-model router)) "unknown")
        ;; Snapshot a base system prompt for the conversation row so the
        ;; sidebar / DB inspectors have something stable to display.
        ;; Real per-query assembly goes through `assemble-system-prompt`
        ;; with `:active-extensions`, so this snapshot is just metadata.
        system-prompt            (build-system-prompt {})
        resolved-conversation-id (db/db-resolve-conversation-id db-info conversation)
        conversation-id          (or resolved-conversation-id
                                   (db/store-conversation! db-info
                                     {:channel       (or channel :vis)
                                      :external-id   external-id
                                      :model         root-model
                                      :title         title
                                      :system-prompt system-prompt}))
        ;; Bind sandbox helpers that need env identity (db-info +
        ;; conversation-id). They go through `custom-bindings` so they
        ;; land in `initial-ns-keys` and therefore stay out of
        ;; `<var_index>` (matches the treatment of every other system
        ;; binding shipped via EXTRA_BINDINGS).
        var-history-fn           (fn var-history [sym]
                                   (db/db-var-history db-info conversation-id
                                     (cond
                                       (symbol? sym) sym
                                       (string? sym) (symbol sym)
                                       :else (symbol (str sym)))))
        env-bindings             {'var-history var-history-fn}
        {:keys [sci-ctx sandbox-ns initial-ns-keys]}
        (sci-env/create-sci-context (merge env-bindings
                                      (:custom-bindings @state-atom)))
        env {:environment-id  environment-id
             :conversation-id conversation-id
             :depth-atom      depth-atom
             :db-info         db-info
             :var-index-atom  var-index-atom
             :autobind-events-atom autobind-events-atom
             :autobind-registry-atom autobind-registry-atom
             :state-atom      state-atom
             :sci-ctx         sci-ctx
             :sandbox-ns      sandbox-ns
             :initial-ns-keys initial-ns-keys
             :router          router
             :extensions      (atom [])}]
    (reset! environment-atom env)
    (swap! state-atom assoc :environment env :conversation-id conversation-id)
    ;; Restore persisted vars when resuming an existing conversation.
    (when resolved-conversation-id
      (try
        (sci-env/restore-sandbox! sci-ctx db-info conversation-id)
        (sci-env/bump-var-index! env)
        (catch Throwable t
          (tel/log! {:level :warn :id ::restore-sandbox-failed
                     :data {:error (ex-message t)
                            :conversation-id conversation-id}
                     :msg "Failed to restore sandbox from DB \u2014 starting empty"}))))
    ;; Auto-discover everything from `META-INF/vis.edn` on the
    ;; classpath, then install extensions in dependency order. The
    ;; same loader populates channel/command/provider/persistance
    ;; registries as a side effect; we just care about the extension
    ;; rows here.
    (ext/discover-extensions!)
    (ext/register-extensions! env register-extension!)
    env))

(defn dispose-environment!
  "Disposes a vis environment and releases resources. For persistent DBs
   (created with `:path`), data is preserved. For disposable DBs, all
   data is deleted."
  [environment]
  (when-let [db-info (:db-info environment)]
    (dispose-rlm-conn! db-info)))

(def ^:private AUTOBIND_KIND_HANDLERS
  {:file (fn [identifier]
           (let [sanitized (-> (str identifier)
                             (str/replace "\\" "/")
                             (str/replace #"^\./" "")
                             (str/replace #"^/+" "")
                             (str/replace "/" "__")
                             (str/replace #"\." "-")
                             (str/replace #"[^A-Za-z0-9_\-]" "-"))
                 suffix (if (str/blank? sanitized) "root" sanitized)]
             (symbol (str "file__" suffix))))
   :dir  (fn [identifier]
           (let [sanitized (-> (str identifier)
                             (str/replace "\\" "/")
                             (str/replace #"^\./" "")
                             (str/replace #"^/+" "")
                             (str/replace "/" "__")
                             (str/replace #"\." "-")
                             (str/replace #"[^A-Za-z0-9_\-]" "-"))
                 suffix (if (str/blank? sanitized) "root" sanitized)]
             (symbol (str "dir__" suffix))))})

(defn- trim-sha-tag
  [tag]
  (let [value (str tag)]
    (if (> (count value) 8)
      (subs value 0 8)
      value)))

(defn- autobind-symbol-for
  [kind identifier]
  (if-let [handler (get AUTOBIND_KIND_HANDLERS kind)]
    (handler identifier)
    (throw (ex-info (str "Unsupported autobind kind: " kind)
             {:type :autobind/unsupported-kind
              :kind kind
              :identifier identifier}))))

(defn- attach-autobind-doc!
  [{:keys [sci-ctx sandbox-ns]} symbol-name doc-string]
  (when (and (string? doc-string) (not (str/blank? doc-string)))
    (try
      (sci/eval-string+ sci-ctx
        (str "(when-let [v (resolve '" symbol-name ")]"
          " (alter-meta! v assoc :doc " (pr-str doc-string) "))")
        {:ns sandbox-ns})
      (catch Throwable _ nil))))

(defn- apply-autobind-binding!
  [environment source-symbol {:keys [kind id content doc tag]}]
  (let [symbol-name  (autobind-symbol-for kind id)
        registry-atom (:autobind-registry-atom environment)
        previous      (get @registry-atom symbol-name)
        unchanged?    (and (some? tag) (= tag (:tag previous)))]
    (if unchanged?
      (let [version (or (:version previous) 1)]
        {:symbol symbol-name
         :source-symbol source-symbol
         :status :unchanged
         :version version
         :kind kind
         :id id
         :footer (str "[autobind → " symbol-name " unchanged v=" version "]")})
      (do
        (sci-env/bind-and-bump! environment symbol-name content)
        (attach-autobind-doc! environment symbol-name doc)
        (let [next-version (inc (or (:version previous) 0))
              _            (swap! registry-atom assoc symbol-name {:version next-version
                                                                   :tag tag})
              sha-part     (when (some? tag)
                             (str " sha=" (trim-sha-tag tag)))]
          {:symbol symbol-name
           :source-symbol source-symbol
           :status :bound
           :version next-version
           :kind kind
           :id id
           :value content
           :footer (str "[autobind → " symbol-name " v=" next-version
                     (or sha-part "") "]")})))))

(defn- run-symbol-autobind!
  [environment extension symbol-entry arguments result]
  (when-let [autobind-fn (:ext.symbol/autobind-fn symbol-entry)]
    (let [events-atom (:autobind-events-atom environment)
          source-symbol (:ext.symbol/sym symbol-entry)]
      (try
        (let [autobind-result (autobind-fn {:args arguments
                                            :result result
                                            :environment environment})
              bindings        (vec (or (:bindings autobind-result) []))
              binding-events  (mapv (fn [binding-entry]
                                      (try
                                        (apply-autobind-binding! environment source-symbol binding-entry)
                                        (catch Throwable throwable
                                          {:symbol nil
                                           :source-symbol source-symbol
                                           :status :failed
                                           :kind (:kind binding-entry)
                                           :id (:id binding-entry)
                                           :footer (str "[autobind failed: " (ex-message throwable) "]")})))
                                bindings)]
          (when (and events-atom (seq binding-events))
            (swap! events-atom into binding-events)))
        (catch Throwable throwable
          (when events-atom
            (swap! events-atom conj
              {:symbol nil
               :source-symbol source-symbol
               :status :failed
               :footer (str "[autobind hook failed: " (ex-message throwable) "]")}))
          (tel/log! {:level :warn :id ::autobind-fn-failed
                     :data {:ext (:ext/namespace extension)
                            :symbol source-symbol
                            :error (ex-message throwable)}}))))))

(defn- wrap-extension-with-autobind
  [extension environment]
  (let [wrapped             (ext/wrap-extension extension environment)
        symbol-entry-by-name (into {}
                               (map (fn [symbol-entry]
                                      [(:ext.symbol/sym symbol-entry) symbol-entry]))
                               (:ext/symbols extension))]
    (into {}
      (map (fn [[symbol-name symbol-value]]
             (let [symbol-entry (get symbol-entry-by-name symbol-name)
                   autobind-fn  (:ext.symbol/autobind-fn symbol-entry)]
               [symbol-name
                (if (and symbol-entry autobind-fn (fn? symbol-value))
                  (fn [& arguments]
                    (let [result (apply symbol-value arguments)]
                      (run-symbol-autobind! environment extension symbol-entry (vec arguments) result)
                      result))
                  symbol-value)])))
      wrapped)))

(defn register-extension!
  "Register a validated extension into `environment`.

   Checks `:ext/requires` \u2014 if the extension declares dependencies, all
   listed extension namespaces must already be registered. Throws on
   missing dependencies.

   If an extension with the same `:ext/namespace` is already registered,
   it is replaced (not duplicated). Enables hot-swap via
   `reload-extension!`.

   Returns `environment` for chaining."
  [environment ext]
  (when-not (:extensions environment)
    (anomaly/incorrect! "Invalid vis environment \u2014 missing :extensions atom"
      {:type :vis/invalid-env}))
  (when-let [requires (seq (:ext/requires ext))]
    (let [registered (into #{} (map :ext/namespace) @(:extensions environment))
          missing    (vec (remove registered requires))]
      (when (seq missing)
        (anomaly/incorrect!
          (str "Extension '" (:ext/namespace ext)
            "' requires " missing " but they are not registered. "
            "Register dependencies first.")
          {:type       :extension/missing-dependencies
           :extension  (:ext/namespace ext)
           :requires   (vec requires)
           :missing    missing
           :registered (vec registered)}))))
  (swap! (:extensions environment)
    (fn [exts]
      (let [ns-sym  (:ext/namespace ext)
            without (vec (remove #(= (:ext/namespace %) ns-sym) exts))]
        (conj without ext))))
  ;; Bind extension symbols ONLY into the aliased namespace — never
  ;; into sandbox. The LLM must always use the alias form
  ;; `(alias/symbol ...)`, not `(symbol ...)`.
  (let [wrapped (wrap-extension-with-autobind ext environment)
        sci-ctx (:sci-ctx environment)]
    (when-let [{ns-sym :ns alias-sym :alias} (:ext/ns-alias ext)]
      (let [ext-ns      (sci/create-ns ns-sym)
            ns-bindings (into {} (map (fn [[sym val]]
                                        [sym (sci/new-var sym val {:ns ext-ns})]))
                          wrapped)]
        (swap! (:env sci-ctx) update :namespaces assoc ns-sym ns-bindings)
        (swap! (:env sci-ctx) update :ns-aliases assoc alias-sym ns-sym))
      ;; Auto-require the alias in sandbox so the LLM never has to call
      ;; `(require ...)` manually.
      (try
        (sci/eval-string+ sci-ctx
          (str "(require '[" ns-sym " :as " alias-sym "])")
          {:ns (sci/find-ns sci-ctx 'sandbox)})
        (catch Throwable t
          (tel/log! {:level :warn :id ::ext-alias-require-failed
                     :data (assoc (format-exception-short t)
                             :ext (:ext/namespace ext)
                             :alias alias-sym)}
            (str "Auto-require of alias '" alias-sym "' failed")))))
    ;; Inject extension-declared Java classes and imports.
    (when-let [classes (seq (:ext/classes ext))]
      (swap! (:env sci-ctx) update :classes merge (into {} classes)))
    (when-let [imports (seq (:ext/imports ext))]
      (swap! (:env sci-ctx) update :imports merge (into {} imports))))
  environment)
