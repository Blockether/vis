(ns com.blockether.vis.internal.prompt
  "Per-iteration context assembly.

   Two surfaces:

     1. The system prompt — written once, cached per-conversation.
        `assemble-system-prompt` joins the minimal core prompt + the
        environment block + each active extension's prompt fragment.

     2. The trailing user message — rebuilt every iteration. Two slots:
          <journal>      last JOURNAL_KEEP_ITERS iterations, code + result,
                        addressable as iN.K. The model's working memory.
          <var_index>   user-defined `(def ...)` bindings in the SCI env.
        Extensions can append `[system_nudge]` lines via `:ext/nudge-fn`.

   The two slots above plus the SYSTEM vars (every name in
   `SYSTEM_VAR_NAMES` — `TURN_USER_REQUEST`, `TURN_QUERY_ID`,
   `TURN_CONVERSATION_SOUL_ID`, `TURN_CONVERSATION_STATE_ID`,
   `TURN_SYSTEM_PROMPT`, `TURN_ACTIVE_EXTENSIONS`, `ITERATION_ID`,
   `ITERATION_PREVIOUS_REASONING`, `CONVERSATION_TITLE`,
   `CONVERSATION_PREVIOUS_ANSWER`) bound in SCI cover everything the
   model needs."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.extension :as extension]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:const MAX_RESULT_DISPLAY_CHARS
  "Hard cap on a single value's pr-str when shown to the model in <journal>."
  6000)

(def ^:const JOURNAL_KEEP_ITERS
  "Rolling-window size for <journal> entries. 12 carries the last
   dozen iterations of the conversation (cross-turn) so a follow-up
   turn sees the immediate context of the prior turn's work without
   re-fetching via `(v/conversation)`.

   When the iteration's prompt token count crosses
   `CONTEXT_PRESSURE_THRESHOLD` of the model's context window the
   loop fires a `[system_nudge]` instructing the model to curate
   `(def …)` summaries it cares about; older entries then drop off
   the rolling window verbatim. The runtime never auto-summarizes —
   the model owns its working memory, in line with the RLM
   principle this project is built on (see AGENTS.md ▸ 'No
   auto-compaction')."
  12)

(def ^:const CONTEXT_PRESSURE_THRESHOLD
  "Fraction of the model's effective input-token budget at which the
   loop fires a context-pressure nudge. 60% leaves headroom for the
   current iteration's own thinking + tool calls + answer payload
   without forcing the model to firefight an overflow."
  0.60)

;; =============================================================================
;; Generic helpers
;; =============================================================================

(defn- truncate [s n]
  (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) " …") s)))

(defn- strip-sandbox-ns [s]
  (str/replace (str s) #"\bsandbox/" ""))

(defn- realize-value [v]
  (cond
    (nil? v) nil
    (map? v) v
    (vector? v) v
    (string? v) v
    :else v))

(defn safe-pr-str
  "Bounded pr-str. Used in <journal> rendering."
  ([v] (safe-pr-str v {}))
  ([v {:keys [max-chars print-length print-level]
       :or {max-chars MAX_RESULT_DISPLAY_CHARS
            print-length 64
            print-level 6}}]
   (try
     (binding [*print-length* print-length
               *print-level*  print-level]
       (let [s (strip-sandbox-ns (pr-str v))]
         (if (> (count s) max-chars)
           (str (subs s 0 max-chars) " …<+" (- (count s) max-chars) " chars>")
           s)))
     (catch Throwable t
       (str "<unprintable: " (.getMessage t) ">")))))

(defn truncated-pr-str
  "Wrapper used by <journal>. Returns [bounded-str truncated?]."
  [v]
  (let [bounded   (safe-pr-str v {:max-chars MAX_RESULT_DISPLAY_CHARS})
        truncated? (boolean (re-find #" …<\+\d+ chars>$" bounded))]
    [bounded truncated?]))

;; =============================================================================
;; <journal> — last JOURNAL_KEEP_ITERS iterations of code + results
;; =============================================================================

(defn- format-block-line
  "One iteration's single block as a `<journal>` line. Renders
   `iN.K  <code> → <value>` plus any non-blank stdout/stderr and a
   slow-suffix when execution exceeded 5s. Bare-symbol blocks read
   naturally because the `<code>` IS the symbol and `<value>` is its
   bound content."
  [iteration-position k expr]
  (let [{:keys [code error result stdout stderr execution-time-ms]} expr
        code-str      (str/trim (or code ""))
        stdout-suffix (when-not (str/blank? stdout)
                        (str " :stdout " (pr-str (truncate stdout 600))))
        stderr-suffix (when-not (str/blank? stderr)
                        (str " :stderr " (pr-str (truncate stderr 600))))
        time-ms       (or execution-time-ms 0)
        slow-suffix   (when (> time-ms 5000)
                        (str " (" time-ms "ms)"))
        value-part    (if error
                        (str "ERROR: " (truncate error 600))
                        (let [v (realize-value result)
                              [value-str truncated?] (truncated-pr-str v)]
                          (str value-str
                            (when truncated? " :truncated? true"))))]
    (str "  i" iteration-position "." (inc k) "  " code-str " → " value-part
      (or slow-suffix "")
      (or stdout-suffix "")
      (or stderr-suffix ""))))

(defn- format-journal-iteration-block
  "One iteration's full `<journal>` segment: optional thinking line,
   then per-block `iN.K` lines that include the leading `:comment`
   (when present) right above the code→value line."
  [iteration-position iteration-data]
  (let [{:keys [thinking blocks]} iteration-data
        header-lines (when (and (string? thinking)
                             (not (str/blank? thinking)))
                       [(str "  i" iteration-position " thinking: "
                          (truncate (str/trim thinking) 800))])
        block-lines  (vec (mapcat (fn [[k blk]]
                                    (let [comment-text (some-> (:comment blk) str/trim)
                                          comment-line (when (and comment-text
                                                               (not (str/blank? comment-text)))
                                                         (str "  i" iteration-position "."
                                                           (inc k) "  ;; "
                                                           (truncate comment-text 400)))]
                                      (cond-> []
                                        comment-line (conj comment-line)
                                        :always      (conj (format-block-line iteration-position
                                                             k blk)))))
                            (map-indexed vector (or blocks []))))]
    (vec (concat header-lines block-lines))))

(defn- format-journal-block
  "Render the last JOURNAL_KEEP_ITERS iterations with iN.K addressable
   ids. `iters` is a seq of `[iteration-position {:thinking :blocks}]`
   pairs, oldest-first. Each iteration's segment carries:
     - `iN thinking: …` once at the top (when the iteration emitted
       any reasoning text)
     - per-block `iN.K  ;; <comment>` line above the code line, when
       the model authored a leading `;; …` / `#_(...)` comment for
       that form
     - `iN.K  <code> → <value>` for every block in the iteration"
  [iters]
  (let [kept  (take-last JOURNAL_KEEP_ITERS (or iters []))
        lines (->> kept
                (mapcat (fn [[pos iteration-data]]
                          (format-journal-iteration-block pos iteration-data)))
                vec)]
    (when (seq lines)
      (str "<journal>\n" (str/join "\n" lines) "\n</journal>"))))

;; =============================================================================
;; <var_index> — read/cache the current SCI sandbox shape
;; =============================================================================

(defn read-var-index-str
  "Lazily build (and cache) the <var_index> body for the active env.
   Returns nil when the env has no SCI context (test fixtures)."
  [environment]
  (when-let [sci-ctx (:sci-ctx environment)]
    (let [var-index-atom (or (:var-index-atom environment)
                           (atom {:index nil :revision -1 :current-revision 0}))
          {:keys [index revision current-revision]} @var-index-atom]
      (if (= revision current-revision)
        index
        (let [sandbox-map (get-in @(:env sci-ctx) [:namespaces 'sandbox])
              idx         (env/build-var-index
                            sci-ctx (:initial-ns-keys environment)
                            sandbox-map
                            (:db-info environment) (:conversation-id environment)
                            nil)]
          (swap! var-index-atom assoc :index idx :revision current-revision)
          idx)))))

;; =============================================================================
;; Iteration context — the trailing user message
;; =============================================================================

(def ^:const TITLE_REFRESH_NUDGE_PERIOD
  "Iteration cadence at which the loop re-nudges the model to refresh
   `CONVERSATION_TITLE`. Independent of the always-on nudge fired
   when the title is blank. 12 lands in the middle of the
   user-requested 10-20 range — frequent enough that the title stays
   current as the conversation drifts, infrequent enough that a
   settled conversation isn't pestered every turn."
  12)

(defn- ^:long count-prompt-tokens
  "Token count for `text` against `model`. Falls back to
   `(quot (count text) 4)` (rough English/code rule of thumb) when
   the encoder lookup fails for an unrecognized model id."
  [model text]
  (when (string? text)
    (or (try
          (when (string? model)
            (svar-router/count-tokens model text))
          (catch Throwable _ nil))
      (long (quot (count text) 4)))))

(defn- model-context-limit
  "Best-effort lookup of `model`'s context window. Falls back to a
   conservative 32k when the table doesn't know the model id, which
   is the smallest mainstream tier still in production use — better
   to nudge a bit early on a 200k model than to never nudge at all."
  [model]
  (or (try
        (when (string? model)
          (svar-router/context-limit model svar-router/MODEL_CONTEXT_LIMITS))
        (catch Throwable _ nil))
    32000))

(defn- context-pressure-nudge
  "Built-in `[system_nudge]` line that fires when the assembled prompt
   (system message + history + new iteration trailer) crosses
   `CONTEXT_PRESSURE_THRESHOLD` of the model's input-token budget.
   Returns nil when usage is below threshold or token info is
   unavailable.

   The nudge does NOT auto-summarize — RLM puts curation in the
   model's hands. Instead it (a) reports the live usage so the model
   sees the budget concretely, (b) gives a Chain-of-Density-style
   recipe for a `(def …)` summary the MODEL writes itself, and (c)
   reminds the model that older raw iters stay reachable through the
   foundation read API even after they roll off the journal."
  [_model used-tokens limit-tokens]
  (when (and (integer? used-tokens) (integer? limit-tokens) (pos? limit-tokens))
    (let [util (double (/ used-tokens limit-tokens))]
      (when (>= util CONTEXT_PRESSURE_THRESHOLD)
        (str "[system_nudge] Context window is at "
          (int (Math/round (* 100.0 util))) "% ("
          used-tokens " / " limit-tokens " tokens). Older <journal>\n"
          "  iterations will roll off the rolling window soon. Curate the\n"
          "  insight you've earned BEFORE that happens — emit a structured\n"
          "  `(def …)` so the value lands in <var_index> + var-history and\n"
          "  survives the roll. Chain-of-Density-style recipe (use only\n"
          "  facts that already appeared in the journal; no new\n"
          "  characterizations / evaluative adjectives):\n"
          "\n"
          "    (def turn-summary\n"
          "      {:findings   [{:where \"src/auth.clj:42\" :what \"jwt-decode rejects nbf-skew\"}\n"
          "                    {:where \"iN.K\"           :what \"<concrete-fact>\"}]\n"
          "       :errors     [{:iteration N :class :patch-no-match :recovery \"…\"}]\n"
          "       :decisions  [{:choice \"validate-then-decode\" :rationale \"…\"}]\n"
          "       :next-step  \"extract verify-jwt to its own ns\"})\n"
          "\n"
          "  Keys above are illustrative — use whatever shape fits the\n"
          "  task. Atoms preferred (file paths, symbol names, error keys,\n"
          "  iN.K refs) over prose. Raw history stays reachable via\n"
          "  `(v/find-attempts \"…\")` and `(v/conversation)` after\n"
          "  iterations roll off; use those when you genuinely need\n"
          "  precision your `(def …)` didn't capture.")))))

(defn- title-nudge
  "Built-in `[system_nudge]` line that fires when:
     1. `CONVERSATION_TITLE` is currently empty, OR
     2. `iteration` is a positive multiple of
        `TITLE_REFRESH_NUDGE_PERIOD` (cadence reminder once a title
        has been set).
   Returns nil otherwise."
  [environment iteration]
  (let [title (some-> (:conversation-title-atom environment) deref str str/trim)
        blank? (or (nil? title) (str/blank? title))]
    (cond
      blank?
      (str "[system_nudge] CONVERSATION_TITLE is currently empty. "
        "Set it via `(conversation-title \"…\")` (3-7-word noun phrase, "
        "e.g. \"Refactor auth flow\" or \"Triage 148 path failures\") so "
        "the conversation is discoverable in the sidebar.")

      (and (integer? iteration)
        (pos? iteration)
        (zero? (mod iteration TITLE_REFRESH_NUDGE_PERIOD)))
      (str "[system_nudge] You're " iteration " iterations into this turn. "
        "If the conversation's focus has shifted from \"" title "\", "
        "refresh the title via `(conversation-title \"…\")`."))))

(defn build-iteration-context
  "Assemble the per-iteration trailing user message.

   Two slots:
     <journal>     — last JOURNAL_KEEP_ITERS iterations, thinking +
                     comments + code + result.
     <var_index>   — `(def ...)` bindings in the SCI env.

   Plus zero or more `[system_nudge]` lines. Built-ins:
     - title nudge (fires on blank title or every
       TITLE_REFRESH_NUDGE_PERIOD iterations).
   Active extensions can append more via `:ext/nudge-fn`.

   Required opts:
     `:active-extensions` — vec from `(active-extensions env)`. Computed once
        per query; threaded through every iteration. Each extension's
        :ext/nudge-fn is consulted (rare).

   Optional:
     `:blocks-by-iteration` — last few iterations of
        `[iteration-position {:thinking :blocks}]` pairs for the
        <journal> renderer.
     `:iteration` — 0-based iteration counter; threaded into the
        title-nudge cadence check."
  [environment {:keys [blocks-by-iteration active-extensions iteration
                       model system-prompt]
                :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [recent-block (format-journal-block blocks-by-iteration)
        last-iteration-blocks (some-> blocks-by-iteration last second)
        var-index-str (read-var-index-str environment)
        var-block (when (and (string? var-index-str)
                          (not (str/blank? var-index-str)))
                    (str "<var_index>\n" var-index-str "\n</var_index>"))
        title-line (title-nudge environment iteration)
        ;; Token-budget probe. Estimate the size of the assembled
        ;; prompt that would be sent to the LLM; fire the
        ;; context-pressure nudge when it crosses
        ;; `CONTEXT_PRESSURE_THRESHOLD`. The probe is a no-op when
        ;; `:model` isn't supplied (e.g. test fixtures).
        prompt-text (str/join "\n\n"
                      (keep identity
                        [system-prompt recent-block var-block]))
        used-tokens (count-prompt-tokens model prompt-text)
        ctx-limit   (model-context-limit model)
        pressure-line (when (and used-tokens ctx-limit)
                        (context-pressure-nudge model used-tokens ctx-limit))
        ext-nudges (when (seq active-extensions)
                     (let [ctx {:environment environment
                                :previous-blocks last-iteration-blocks}]
                       (into []
                         (keep (fn [ext]
                                 (when-let [nudge-fn (:ext/nudge-fn ext)]
                                   (try
                                     (let [result (nudge-fn ctx)]
                                       (when (and (string? result) (not (str/blank? result)))
                                         result))
                                     (catch Throwable t
                                       (tel/log! {:level :warn :data {:ext (:ext/namespace ext) :error (ex-message t)}})
                                       nil)))))
                         active-extensions)))
        all-nudges (cond-> []
                     title-line       (conj title-line)
                     pressure-line    (conj pressure-line)
                     (seq ext-nudges) (into ext-nudges))
        nudges-block (when (seq all-nudges) (str/join "\n" all-nudges))
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [recent-block var-block nudges-block])]
    (when (seq parts)
      (str/join "\n" parts))))

;; =============================================================================
;; Initial messages
;; =============================================================================

(defn assemble-initial-messages
  [{:keys [system-prompt initial-user-content history-messages]}]
  (vec
    (concat
      (when system-prompt [{:role "system" :content system-prompt}])
      (or history-messages [])
      (when initial-user-content [{:role "user" :content initial-user-content}]))))

(defn trim-to-initial-history [messages initial-count]
  (vec (take initial-count messages)))

;; =============================================================================
;; System prompt
;; =============================================================================

(def CORE_SYSTEM_PROMPT
  "You are a Clojure agent. You make changes by writing code. The shape of every turn:

    write code -> get data -> process data -> emit answer.

Reply each turn with one or more ```clojure … ``` fences. Their source concatenates into top-level forms; each form runs in order, each produces an iN.K result the next iteration sees.

The canonical pattern for any tool call you'll inspect more than once:
```clojure
(def x (v/cat \"src/foo.clj\"))   ;; lands in <var_index> + var-history
x                                  ;; surfaces value in this iter's <journal>
```
`(def …)` persists across iterations; the bare symbol surfaces the value in the current iteration's `<journal>` so you (and future iterations) can see what you just bound. One-shot probes (count, presence-check) stay inline.

Terminal: `(answer …)` is the LAST top-level form of its iteration (last or only). ONE accepted answer ends the turn; compose your final string in let-bound vars and emit it once. Shapes:
```clojure
(answer \"done\")
(let [s (build-summary)] (answer s))
(do (v/edit …) (answer \"done\"))
(work-1) (work-2) (answer (compose work-1 work-2))   ;; iter 1+, after observing iN.K results
```

Iter 0 answer fits when the reply is self-contained: static markdown, a fixed list, the value of a SYSTEM var (`TURN_ACTIVE_EXTENSIONS`, `CONVERSATION_TITLE`, …), or anything you can compose inline without needing to read iN.K results first. Wrap any prerequisite work into one structural form: `(let [s (build)] (answer s))`, `(answer (md/join …))`, `(do (v/edit …) (answer \"done\"))`.

Iter 0 also fits exploration. Aim for 1–3 forms that narrow your search (one `(v/ls \".\")`, one targeted `(v/rg [\"keyword\"] \"src\")`, one `(v/cat path)` at the likely entry-point), then read results in iter 1's `<journal>` and commit `(answer …)` there once the picture is clear.

Each iteration's user msg carries:
  <journal>     last 2 iters: thinking + comments + code + results, addressable iN.K
  <var_index>   your `(def name val)` bindings still alive in the sandbox
  [system_nudge] lines (when relevant) — e.g. \"set the conversation title\"
                                          when CONVERSATION_TITLE is empty

SYSTEM vars (read-only; bound by name in the sandbox):
  TURN_USER_REQUEST            the user's message text — your goal for this turn
  TURN_QUERY_ID                UUID of the in-flight turn
  TURN_CONVERSATION_SOUL_ID    UUID of the parent conversation_soul
  TURN_CONVERSATION_STATE_ID   UUID of the conversation_state branch this turn lives on
  TURN_SYSTEM_PROMPT           the assembled system prompt for this turn
  TURN_ACTIVE_EXTENSIONS       vec of {:alias :namespace :doc :version :kind :symbols :docs} for every active extension
  ITERATION_ID                 UUID of the last persisted iteration (nil before iter 1)
  ITERATION_PREVIOUS_REASONING last iteration's :thinking text
  CONVERSATION_TITLE           current conversation title (\"\" until set)
  CONVERSATION_METADATA        frozen-at-this-iter map of conversation facts
                                 {:title :channel :external-id :created-at :turn-count}
  CONVERSATION_PREVIOUS_ANSWER previous turn's final answer

Host primitives (top-level, no alias — named for what they write):
  (answer ARG)               terminal answer; closes the turn
  (conversation-title ARG)   one-arity title write; broadcasts to every channel watching the conversation. Read via the `CONVERSATION_TITLE` SYSTEM var.")

(defn build-system-prompt
  "Core system prompt: agent rules + optional caller addendum.

   The `<environment>` block (cwd, OS, git facts, languages,
   monorepo shape) is NOT assembled here. It is rendered by the
   `vis-foundation` extension's `:ext/prompt` fragment, so
   the runtime no longer hardcodes any environment text. Drop the
   jar, drop the block."
  [{:keys [system-prompt]}]
  (str CORE_SYSTEM_PROMPT
    (when (and system-prompt (not (str/blank? system-prompt)))
      (str "\n\nINSTRUCTIONS:\n" system-prompt "\n"))))

(defn active-extensions
  "Returns the seq of registered extensions whose `:ext/activation-fn` returns
   truthy for `environment`, in registration order. Single source of truth for
   activation; call ONCE at the top of a query."
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
                      (str "Extension '" (:ext/namespace ext) "' activation-fn threw"))
                    false)))
        exts))))

(defn extensions-snapshot
  "Build the value of the `TURN_ACTIVE_EXTENSIONS` SYSTEM var from a precomputed
   active-extensions vec.

   Returns a vec of compact, fully-realized data maps — NO functions,
   NO atoms, NO opaque runtime objects. The model walks this with
   `filter` / `keep` / `some` exactly like any other Clojure data
   structure; never has to reach into `(v/extensions)` just to
   discover what's loaded.

   Per element:
     :alias     — short symbol the model calls under (`'v`, `'md`,
                  `'git`, ...). nil when the extension didn't declare
                  an `:ext/ns-alias`.
     :namespace — fully-qualified ns symbol of the extension.
     :doc       — one-line LLM description from `:ext/doc` (when set).
     :version   — semver string (when set).
     :kind      — categorical bucket (providers, channels, foundation,
                  languages, persistance, …) used as the section
                  label both in this snapshot and in `vis extensions
                  list` (when set).
     :symbols   — vec of bare symbol names the extension intern'd into
                  the sandbox (just the names; signatures + doc come
                  from `(v/extension-doc ...)` if the model wants
                  them).
     :docs      — vec of doc-name strings (e.g. `\"README.md\"`) the
                  extension ships in its `vis.edn` registry. Reachable
                  via `(v/extension-doc 'id name)`.

   The vec is bound ONCE at turn start (see `iteration-loop`) and
   stays frozen for the rest of the turn — every iteration sees the
   same value."
  [active-extensions]
  (->> (or active-extensions [])
    (mapv (fn [ext]
            (let [ext-ns   (:ext/namespace ext)
                  alias    (get-in ext [:ext/ns-alias :alias])
                  ;; Resolve doc names through the global extension
                  ;; registry. Same mapping `(v/extensions)` uses;
                  ;; we duplicate the lookup here (instead of calling
                  ;; the meta extension) because the loop layer is
                  ;; upstream of every ext, including meta itself —
                  ;; TURN_ACTIVE_EXTENSIONS must work even when vis-foundation
                  ;; isn't on the classpath.
                  registry-id (try (or alias (extension/extension-id-of-ns ext-ns))
                                (catch Throwable _ nil))
                  doc-names   (try (extension/extension-doc-names registry-id)
                                (catch Throwable _ []))]
              (cond-> {:namespace ext-ns
                       :symbols   (mapv :ext.symbol/sym (:ext/symbols ext))
                       :docs      (vec doc-names)}
                alias                (assoc :alias   alias)
                (:ext/kind ext)      (assoc :kind    (:ext/kind ext))
                (:ext/version ext)   (assoc :version (:ext/version ext))
                (:ext/doc ext)       (assoc :doc     (:ext/doc ext))))))))

(defn- render-extension-prompt-block
  "Render one extension's contribution to the system prompt. Honors
   ONLY `:ext/prompt`. The previous implementation also auto-rendered
   every `:ext/symbols` entry as a `- (alias/sym args) — docstring`
   line, which silently ballooned the prompt for any extension whose
   `:ext/symbols` was a thin wrapper around an upstream library: e.g.
   `vis-language-clojure`'s `z/` extension dumped 104 rewrite-clj.zip
   publics with full upstream docstrings (~7000 tokens), every
   iteration, of every conversation — even when the user just typed
   a one-word greeting. Authors who want their tools advertised in
   the prompt write `:ext/prompt` (string or `(fn [env] string)`);
   sandbox bindings are independently callable from `:code` whether
   advertised or not. Extensions that legitimately want auto-rendered
   symbol lines may call `extension/render-prompt` from inside their
   own `:ext/prompt` fn — explicit beats clever."
  [environment ext]
  (try
    (when-let [extra-fn (:ext/prompt ext)]
      (let [body (extra-fn environment)]
        (when (and (string? body) (not (str/blank? body)))
          (if-let [{ns-sym :ns alias-sym :alias} (:ext/ns-alias ext)]
            (str "[namespace: " alias-sym " → " ns-sym "]\n" body)
            body))))
    (catch Throwable t
      (tel/log! {:level :error :id ::ext-prompt-error
                 :data {:ext (:ext/namespace ext) :error (ex-message t)}}
        (str "Extension '" (:ext/namespace ext) "' prompt rendering failed"))
      nil)))

(defn assemble-system-prompt
  "Build the full system prompt: core agent rules + active-extension prompts.

   Required opts:
     `:active-extensions` — vec from `(active-extensions env)`."
  [environment {:keys [system-prompt active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-system-prompt requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [base   (build-system-prompt {:system-prompt system-prompt})
        ext-ps (seq (keep #(render-extension-prompt-block environment %) active-extensions))]
    (if ext-ps
      (str base "\n\n" (str/join "\n\n" ext-ps))
      base)))
