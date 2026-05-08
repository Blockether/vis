(ns com.blockether.vis.internal.prompt
  "Per-iteration context assembly.

   Two surfaces:

     1. The system prompt - written once, cached per-conversation.
        `assemble-system-prompt` joins tagged blocks: <system_prompt>,
        <environment-info>, <extensions>, and provider-specific prompt.

     2. The trailing user message - rebuilt every iteration. Two slots:
          <journal>      newest code + result lines that fit the dynamic
                        journal token budget.
          <var_index>   user-defined `(def ...)` bindings in the SCI env.
        Extensions can append `<system_nudge importance=\"...\">` entries via `:ext/nudge-fn`.

   The two slots above plus the SYSTEM vars (every name in
   `SYSTEM_VAR_NAMES` - `TURN_USER_REQUEST`, `TURN_CONVERSATION_TURN_ID`,
   `TURN_CONVERSATION_SOUL_ID`, `TURN_CONVERSATION_STATE_ID`,
   `TURN_SYSTEM_PROMPT`, `TURN_ACTIVE_EXTENSIONS`,
   `TURN_ACCESSIBLE_SKILLS`, `ITERATION_ID`,
   `ITERATION_PREVIOUS_REASONING`, `CONVERSATION_ID`,
   `CONVERSATION_SOUL_ID`, `CONVERSATION_STATE_ID`,
   `CONVERSATION_TITLE`, `CONVERSATION_PREVIOUS_ANSWER`) bound in SCI
   cover everything the model needs. These vars are direct sandbox
   bindings; reference them by name.

   Context-floor contract (CTX1; see prompt-test):

     Trivial / no-tool turns produce a NIL per-iteration trailer
     (no <journal>, no <active_skills>, no <var_index>, no
     <system_nudges>). The model-facing context for a trivial turn
     is just the cached system prompt + the current
     <user_turn_request_main_goal>. Coding turns surface
     observable journal entries and simple callable APIs,
     active-skill bodies, var_index entries, and tool output
     through the same XML-tagged surfaces. Prior LLM-only reasoning
     reaches the model only through the
     `ITERATION_PREVIOUS_REASONING` SYSTEM var; <journal> stays
     observed-evidence only.

     Preview / full boundary: <journal> and <var_index> are
     bounded previews. Full values stay in SCI vars, persisted
     iteration / block rows. The
     renderer never emits a full multi-KB tool / file payload
     into context; it emits a bounded preview and the model can fetch
     the full value through foundation APIs (`v/inspect`, `v/event`,
     the var binding)."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.format :as fmt]
   [com.blockether.vis.internal.skills :as skills]
   [taoensso.telemere :as tel]))

;; =============================================================================
;; Tunables
;; =============================================================================

(def ^:const MAX_RESULT_DISPLAY_CHARS
  "Hard cap on a single value's pr-str when shown to the model in <journal>.

   Keep this small. A journal line is repeated into subsequent provider
   prompts while it remains inside the token-budgeted window. Full
   forensic values stay in the DB/transcript; the model-facing journal
   gets a working-memory preview."
  1500)

(def ^:const JOURNAL_CONTEXT_FRACTION
  "Maximum fraction of the model context window allowed for rendered
   <journal> before the remaining per-iteration budget is considered.

   There is no fixed iteration-count cap. The window is token-budgeted:
   newest evidence stays, oldest journal lines drop when the rendered
   journal would exceed this cap. Actual journal budget may be smaller
   after protected/pinned context and <var_index> consume tokens."
  0.50)

(def ^:const VAR_INDEX_CONTEXT_FRACTION
  "Maximum fraction of the model context window reserved for rendered
   <var_index>. Entry count alone is not enough: 100 live string vars at
   8k chars each can consume ~200k tokens. Keep newest entries and drop
   older entries with an explicit marker."
  0.15)

(def ^:const MAX_ITERATION_CONTEXT_TOKENS
  "Hard cap for Vis' model-facing working memory, independent of provider
   advertised context size. Huge-window models can accept hundreds of thousands
   of input tokens, but the RLM loop gets worse there: output budgets are
   consumed by reasoning/tool planning and Responses streams can end as
   `max_output_tokens`. Durable evidence stays in the DB; each iteration only
   needs a compact working set."
  64000)

(def ^:const MAX_JOURNAL_TOKENS
  "Absolute token cap for rendered <journal>. This is layered on top of
   JOURNAL_CONTEXT_FRACTION so 1M-token models cannot inherit a 500k-token
   journal window."
  24000)

(def ^:const MAX_VAR_INDEX_TOKENS
  "Absolute token cap for rendered <var_index>. Live vars can contain large
   tool outputs; the model sees a compact index, not the full sandbox heap."
  12000)

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
  (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) " ...") s)))

(defn- strip-sandbox-ns [s]
  (str/replace (str s) #"\bsandbox/" ""))

(defn- runtime-sentinel? [v]
  (and (map? v) (= :expr (:vis/ref v))))

(defn- realize-value [v]
  (cond
    (nil? v) nil
    (runtime-sentinel? v) "<runtime value; re-evaluate expression to restore>"
    (map? v) v
    (vector? v) v
    (string? v) v
    :else v))

(defn- zprintable-data?
  [v]
  (or (map? v)
    (vector? v)
    (set? v)
    (instance? clojure.lang.IPersistentList v)))

(defn- value-pr-str
  [v bounded-print?]
  (if (and (zprintable-data? v) (not bounded-print?))
    (fmt/safe-zprint-str v {:width 80})
    (pr-str v)))

(defn safe-pr-str
  "Bounded Clojure data rendering. Used in <journal> and TUI result rendering."
  ([v] (safe-pr-str v {}))
  ([v {:keys [max-chars print-length print-level] :as opts
       :or {max-chars MAX_RESULT_DISPLAY_CHARS
            print-length 64
            print-level 6}}]
   (try
     (binding [*print-length* print-length
               *print-level*  print-level]
       (let [bounded-print? (or (contains? opts :print-length)
                              (contains? opts :print-level))
             s (strip-sandbox-ns (value-pr-str v bounded-print?))]
         (if (> (count s) max-chars)
           (str (subs s 0 max-chars) " ...<+" (- (count s) max-chars) " chars>")
           s)))
     (catch Throwable t
       (str "<unprintable: " (.getMessage t) ">")))))

(defn truncated-pr-str
  "Wrapper used by <journal>. Returns [bounded-str truncated?]."
  [v]
  (let [bounded   (safe-pr-str v {:max-chars MAX_RESULT_DISPLAY_CHARS})
        truncated? (boolean (re-find #" ...<\+\d+ chars>$" bounded))]
    [bounded truncated?]))

;; =============================================================================
;; <journal> - newest token-budgeted code + results
;; =============================================================================

(declare count-prompt-tokens model-context-limit)

(defn- effective-context-limit
  [model context-limit]
  (max 1 (min (long (or context-limit (model-context-limit model)))
           (long MAX_ITERATION_CONTEXT_TOKENS))))

(defn- journal-token-budget
  ([model]
   (journal-token-budget model nil))
  ([model context-limit]
   (max 1 (min (long MAX_JOURNAL_TOKENS)
            (long (Math/floor (* JOURNAL_CONTEXT_FRACTION
                                (double (effective-context-limit model context-limit)))))))))

(defn- tool-result-journal-text
  [v]
  (extension/render-tool-result :journal v))

(defn- format-block-line
  "One iteration's single block as a `<journal>` line. Renders
   `iN.K  <code> -> <value>` plus any non-blank stdout/stderr and a
   slow-suffix when execution exceeded 5s. Bare-symbol blocks read
   naturally because the `<code>` IS the symbol and `<value>` is its
   bound content. Tool-result envelopes render through shared display text
   instead of dumping the full map into the journal."
  [iteration-position k expr]
  (let [{:keys [code error result previews stdout stderr execution-time-ms]} expr
        block-label   (str "i" iteration-position "." (inc k))
        code-str      (truncate (str/trim (or code "")) MAX_RESULT_DISPLAY_CHARS)
        stdout-suffix (when-not (str/blank? stdout)
                        (str " :stdout " (pr-str (truncate stdout 600))))
        stderr-suffix (when-not (str/blank? stderr)
                        (str " :stderr " (pr-str (truncate stderr 600))))
        time-ms       (or execution-time-ms 0)
        slow-suffix   (when (> time-ms 5000)
                        (str " (" time-ms "ms)"))
        value-part    (if error
                        (str "ERROR: " (truncate error 600))
                        (if (extension/tool-result? result)
                          (tool-result-journal-text result)
                          (if-let [preview-values (seq previews)]
                            (str/join "\n"
                              (map tool-result-journal-text preview-values))
                            (let [v (realize-value result)
                                  [value-str truncated?] (truncated-pr-str v)]
                              (str value-str
                                (when truncated? " :truncated? true"))))))]
    (str "  " block-label "  " code-str " -> " value-part
      (or slow-suffix "")
      (or stdout-suffix "")
      (or stderr-suffix ""))))

(defn- format-journal-iteration-block
  "One iteration's full `<journal>` segment: per-block labels
   include the leading `:comment` (when present) right above
   the code->value line. LLM-only iteration `:thinking` is intentionally
   excluded; the previous value is available only through the
   `ITERATION_PREVIOUS_REASONING` system var, not the journal."
  [iteration-position iteration-data]
  (let [{:keys [blocks answer]} iteration-data
        block-lines (vec (mapcat (fn [[k blk]]
                                   (let [comment-text (some-> (:comment blk) str/trim)
                                         comment-line (when (and comment-text
                                                              (not (str/blank? comment-text)))
                                                        (str "  i" iteration-position "." (inc k)
                                                          "  ;; "
                                                          (truncate comment-text 400)))]
                                     (cond-> []
                                       comment-line (conj comment-line)
                                       :always      (conj (format-block-line iteration-position
                                                            k blk)))))
                           (map-indexed vector (or blocks []))))
        answer-text (some-> answer str str/trim)
        answer-label (str "i" iteration-position ".answer")
        answer-line (when (and answer-text (not (str/blank? answer-text)))
                      (str "  " answer-label "  <final-answer> -> "
                        (pr-str (truncate answer-text MAX_RESULT_DISPLAY_CHARS))))]
    (cond-> block-lines
      answer-line (conj answer-line))))

(defn- trim-journal-lines
  "Keep newest journal lines within the supplied journal token budget.

   By default, that budget is capped at 50% of the active model context,
   then may be reduced by protected/pinned context and <var_index> usage.

   Returns `[lines dropped-count budget-tokens used-tokens]`. Lines are
   indivisible; truncating inside a line would create misleading half-output.
   Per-value truncation
   above keeps individual lines bounded, while this token cap stops
   repeated reads from dominating every later provider prompt."
  ([model lines]
   (trim-journal-lines model lines nil))
  ([model lines token-budget]
   (let [budget-tokens (or token-budget (journal-token-budget model))]
     (loop [remaining (reverse lines)
            kept      '()
            used      0]
       (if-let [line (first remaining)]
         (let [line-tokens (or (count-prompt-tokens model line)
                             (long (quot (count line) 4)))
               next-used  (+ used line-tokens 1)]
           (if (and (seq kept) (> next-used budget-tokens))
             [(vec kept) (count remaining) budget-tokens used]
             (recur (rest remaining) (conj kept line) next-used)))
         [(vec kept) 0 budget-tokens used])))))

(defn- format-journal-block
  "Render all carried iterations, then trim the rendered lines by token
   budget instead of by iteration count.
   `iters` is a seq of `[iteration-position {:thinking :blocks}]` pairs,
   oldest-first. Iteration-level `:thinking` is LLM-only reasoning and is
   not rendered in `<journal>`; only `ITERATION_PREVIOUS_REASONING` exposes
   the latest prior value. Each iteration's segment carries:
     - per-block `iN.K  ;; <comment>` line above the code line, when
       the model authored a leading `;; ...` / `#_(...)` comment for
       that form
     - `iN.K  <code> -> <value>` for every block in the iteration"
  ([model iters]
   (format-journal-block model iters nil))
  ([model iters token-budget]
   (let [lines (->> (or iters [])
                 (mapcat (fn [[pos iteration-data]]
                           (format-journal-iteration-block pos iteration-data)))
                 vec)
         [trimmed-lines dropped-count budget-tokens used-tokens]
         (trim-journal-lines model lines token-budget)
         omitted-line (str "  ... " dropped-count
                        " older journal lines omitted to fit journal token budget "
                        used-tokens "/" budget-tokens
                        " tokens (journal cap <= " (long (* 100 JOURNAL_CONTEXT_FRACTION))
                        "% model context; actual budget may be lower)")
         rendered-lines (if (pos? dropped-count)
                          (vec (cons omitted-line trimmed-lines))
                          trimmed-lines)]
     (when (seq rendered-lines)
       (str "<journal>\n" (str/join "\n" rendered-lines) "\n</journal>")))))

;; =============================================================================
;; <var_index> - read/cache the current SCI sandbox shape
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

(defn- var-index-token-budget
  [context-limit remaining-budget]
  (let [fraction-budget (long (Math/floor (* VAR_INDEX_CONTEXT_FRACTION
                                            (double context-limit))))
        capped-budget   (min fraction-budget (long MAX_VAR_INDEX_TOKENS))]
    (max 1 (min capped-budget (max 1 (long (or remaining-budget capped-budget)))))))

(defn- var-index-entry-start? [line]
  (str/starts-with? (str line) ";; v="))

(defn- split-var-index-entries
  [var-index-str]
  (let [lines (str/split-lines (or var-index-str ""))]
    (loop [remaining lines
           current   []
           entries   []]
      (if-let [line (first remaining)]
        (if (and (var-index-entry-start? line) (seq current))
          (recur (rest remaining) [line] (conj entries (str/join "\n" current)))
          (recur (rest remaining) (conj current line) entries))
        (cond-> entries
          (seq current) (conj (str/join "\n" current)))))))

(defn- trim-var-index-str
  [model var-index-str token-budget]
  (when (and (string? var-index-str) (not (str/blank? var-index-str)))
    (let [entries (split-var-index-entries var-index-str)
          budget  (max 1 (long token-budget))]
      (loop [remaining entries
             kept      []
             used      0]
        (if-let [entry (first remaining)]
          (let [entry-tokens (or (count-prompt-tokens model entry)
                               (long (quot (count entry) 4)))
                next-used    (+ used entry-tokens 1)]
            (if (and (seq kept) (> next-used budget))
              (let [dropped (count remaining)
                    marker  (str ";; ... " dropped
                              " older <var_index> entries omitted to fit var-index token budget "
                              used "/" budget " tokens ("
                              (long (* 100 VAR_INDEX_CONTEXT_FRACTION))
                              "% of model context max)")]
                (str/join "\n" (conj kept marker)))
              (recur (rest remaining) (conj kept entry) next-used)))
          (str/join "\n" kept))))))

;; =============================================================================
;; Iteration context - provider context block inserted before current user goal
;; =============================================================================

(def ^:const TITLE_REFRESH_NUDGE_PERIOD
  "Iteration cadence at which the loop re-nudges the model to refresh
   `CONVERSATION_TITLE`. Independent of the always-on nudge fired
   when the title is blank. 12 lands in the middle of the
   user-requested 10-20 range - frequent enough that the title stays
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

(defn count-tokens
  "Public token counter mirroring the loop's internal accounting. Used
   by tests + diagnostics that want to report exact `<journal>` /
   `<var_index>` / iteration-trailer token impact without depending on
   the private encoder var. nil/blank inputs return 0 so callers can
   sum surfaces without nil-checking each one."
  ([text] (count-tokens nil text))
  ([model text]
   (long (or (count-prompt-tokens model text) 0))))

(defn- model-context-limit
  "Best-effort lookup of `model`'s context window. Falls back to a
   conservative 32k when the table doesn't know the model id, which
   is the smallest mainstream tier still in production use - better
   to nudge a bit early on a 200k model than to never nudge at all."
  [model]
  (or (try
        (when (string? model)
          (svar-router/context-limit model svar-router/MODEL_CONTEXT_LIMITS))
        (catch Throwable _ nil))
    32000))

(defn- context-pressure-nudge
  "Built-in context-pressure nudge that fires when the assembled prompt
   (system prompt + current user request + new iteration trailer) crosses
   `CONTEXT_PRESSURE_THRESHOLD` of the model's input-token budget.
   Returns nil when usage is below threshold or token info is
   unavailable.

   The nudge does NOT auto-summarize - RLM puts curation in the
   model's hands. Instead it (a) reports the live usage so the model
   sees the budget concretely, (b) gives a Chain-of-Density-style
   recipe for a `(def ...)` summary the MODEL writes itself, and (c)
   reminds the model that older raw iters stay reachable through the
   foundation read API even after they roll off the journal."
  [_model used-tokens limit-tokens]
  (when (and (integer? used-tokens) (integer? limit-tokens) (pos? limit-tokens))
    (let [util (double (/ used-tokens limit-tokens))]
      (when (>= util CONTEXT_PRESSURE_THRESHOLD)
        (str "Context window is at "
          (int (Math/round (* 100.0 util))) "% ("
          used-tokens " / " limit-tokens " tokens). Older <journal>\n"
          "  lines drop when the journal exceeds its token budget. Curate the\n"
          "  insight you've earned BEFORE that happens - emit a structured\n"
          "  `(def ...)` so the value lands in <var_index> + persisted var store and\n"
          "  survives the roll. Chain-of-Density-style recipe (use only\n"
          "  facts that already appeared in the journal; no new\n"
          "  characterizations / evaluative adjectives):\n"
          "\n"
          "    (def turn-summary\n"
          "      {:findings   [{:where \"src/auth.clj:42\" :what \"jwt-decode rejects nbf-skew\"}\n"
          "                    {:where \"turn/<turn8>/iteration/<n>/block/<k>\" :what \"<concrete-fact>\"}]\n"
          "       :errors     [{:iteration N :class :patch-no-match :recovery \"...\"}]\n"
          "       :decisions  [{:choice \"validate-then-decode\" :rationale \"...\"}]\n"
          "       :next-step  \"extract verify-jwt to its own ns\"})\n"
          "\n"
          "  Keys above are illustrative - use whatever shape fits the\n"
          "  task. Atoms preferred (file paths, symbol names, error keys,\n"
          "  concrete values) over prose. Raw iteration output stays reachable via\n"
          "  `(v/inspect)` after journal lines roll off; use its :transcript\n"
          "  and :failures keys when you genuinely need precision your\n"
          "  `(def ...)` didn't capture.")))))

(defn- title-nudge
  "Built-in title nudge that fires when:
     1. `CONVERSATION_TITLE` is currently empty, OR
     2. caller requests a turn-boundary refresh check, OR
     3. `iteration` is a positive multiple of
        `TITLE_REFRESH_NUDGE_PERIOD` (cadence reminder inside a long turn).
   Returns nil otherwise."
  [environment iteration refresh?]
  (let [title (some-> (:conversation-title-atom environment) deref str str/trim)
        blank? (or (nil? title) (str/blank? title))]
    (cond
      blank?
      (str "CONVERSATION_TITLE is currently empty. "
        "Set it via `(conversation-title \"...\")` (3-7-word noun phrase, "
        "e.g. \"Refactor auth flow\" or \"Triage 148 path failures\") so "
        "the conversation is discoverable in the sidebar.")

      refresh?
      (str "Current CONVERSATION_TITLE is \"" title "\". "
        "If this turn changes the conversation focus, refresh the title via "
        "`(conversation-title \"...\")`.")

      (and (integer? iteration)
        (pos? iteration)
        (zero? (mod iteration TITLE_REFRESH_NUDGE_PERIOD)))
      (str "You're " iteration " iterations into this turn. "
        "If the conversation's focus has shifted from \"" title "\", "
        "refresh the title via `(conversation-title \"...\")`."))))

(defn- prompt-block
  [tag body]
  (when (and (string? body) (not (str/blank? body)))
    (str "<" tag ">\n"
      body
      (when-not (str/ends-with? body "\n") "\n")
      "</" tag ">")))

(defn- attr-str
  [v]
  (-> (str v)
    (str/replace "&" "&amp;")
    (str/replace "\"" "&quot;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

(defn- attr-name
  [v]
  (attr-str (if (keyword? v) (name v) v)))

(defn- format-active-skill
  [{:keys [name source body]}]
  (str "<skill name=\"" (attr-str name) "\" source=\"" (attr-name (or source :unknown)) "\">\n"
    (or body "")
    (when-not (str/ends-with? (or body "") "\n") "\n")
    "</skill>"))

(defn- active-skills-block
  [environment]
  (when-let [skills (some-> (:active-skills-atom environment) deref vals seq)]
    (str "<active_skills count=\"" (count skills) "\">\n"
      (str/join "\n\n" (map format-active-skill (sort-by :name skills)))
      "\n</active_skills>")))

(defn- normalize-system-nudge
  [default-importance nudge]
  (let [entry (cond
                (string? nudge)
                {:importance default-importance
                 :text       nudge}

                (map? nudge)
                {:importance (or (:importance nudge) default-importance)
                 :text       (:text nudge)}

                :else nil)
        text  (some-> (:text entry) str str/trim)]
    (when (and text (not (str/blank? text)))
      {:importance (attr-name (or (:importance entry) default-importance))
       :text       text})))

(defn- format-system-nudge
  [{:keys [importance text]}]
  (str "<system_nudge importance=\"" importance "\">\n"
    (attr-str text)
    "\n</system_nudge>"))

(defn- system-nudges-block
  [nudges]
  (when-let [nudges (seq (keep identity nudges))]
    (str "<system_nudges>\n"
      (str/join "\n" (map format-system-nudge nudges))
      "\n</system_nudges>")))

(defn- call-extension-callback
  [ext f & args]
  (binding [extension/*current-extension* ext
            extension/*current-symbol* nil]
    (apply f args)))

(defn build-iteration-context
  "Assemble the per-iteration context block inserted before the current user goal.

   Two slots:
     <journal>     - newest token-budgeted comments + code + result.
                     Budget is capped at 50% of the model context, then
                     reduced by protected/pinned context and <var_index>.
                     It never renders LLM-only iteration `:thinking`; the
                     latest prior value is available as
                     `ITERATION_PREVIOUS_REASONING`.
     <var_index>   - `(def ...)` bindings in the SCI env.

   Plus zero or more tagged `<system_nudge importance=\"...\">` entries
   wrapped in `<system_nudges>`. Built-ins:
     - title nudge (importance low; fires on blank title or every
       TITLE_REFRESH_NUDGE_PERIOD iterations)
     - context-pressure nudge (importance high).
   Active extensions can append more via `:ext/nudge-fn` by returning
   either a string or `{:importance :low|:normal|:high|:critical :text ...}`.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Computed once
        per turn; threaded through every iteration. Each extension's
        :ext/nudge-fn is consulted (rare).

   Optional:
     `:blocks-by-iteration` - carried iterations as
        `[iteration-position {:thinking :blocks}]` pairs for the
        <journal> renderer. Rendering ignores `:thinking` and trims by token
        budget, not fixed iteration count.
     `:iteration` - current iteration position (1-based for rendered refs;
        callers that keep an internal counter convert before exposing it)."
  [environment {:keys [blocks-by-iteration active-extensions iteration
                       model system-prompt current-user-content context-limit
                       title-refresh?]
                :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [ctx-limit (effective-context-limit model context-limit)
        active-skills-block (active-skills-block environment)
        pinned-text (str/join "\n\n" (keep identity [system-prompt current-user-content active-skills-block]))
        pinned-tokens (or (count-prompt-tokens model pinned-text) 0)
        budget-after-pinned (max 1 (- ctx-limit (long pinned-tokens)))
        var-index-str (read-var-index-str environment)
        var-index-budget (var-index-token-budget ctx-limit budget-after-pinned)
        var-index-str* (trim-var-index-str model var-index-str var-index-budget)
        var-block (when (and (string? var-index-str*)
                          (not (str/blank? var-index-str*)))
                    (str "<var_index>\n" var-index-str* "\n</var_index>"))
        var-tokens (or (count-prompt-tokens model (or var-block "")) 0)
        journal-budget (max 1 (min (journal-token-budget model ctx-limit)
                                (- budget-after-pinned (long var-tokens))))
        recent-block (format-journal-block model blocks-by-iteration journal-budget)
        last-iteration-blocks (some-> blocks-by-iteration last second)
        title-line (title-nudge environment iteration title-refresh?)
        ;; Token-budget probe. Estimate the size of the assembled
        ;; prompt that would be sent to the LLM; fire the
        ;; context-pressure nudge when it crosses
        ;; `CONTEXT_PRESSURE_THRESHOLD`.
        prompt-text (str/join "\n\n"
                      (keep identity
                        [system-prompt current-user-content active-skills-block recent-block var-block]))
        used-tokens (count-prompt-tokens model prompt-text)
        pressure-line (when (and used-tokens ctx-limit)
                        (context-pressure-nudge model used-tokens ctx-limit))
        ext-nudges (when (seq active-extensions)
                     (let [iter-position (if (some? iteration)
                                           (inc (long iteration))
                                           1)
                           ctx {:environment environment
                                :iteration iter-position
                                :previous-blocks last-iteration-blocks}]
                       (into []
                         (keep (fn [ext]
                                 (when-let [nudge-fn (:ext/nudge-fn ext)]
                                   (try
                                     (let [result (call-extension-callback ext nudge-fn ctx)]
                                       (if (extension/system-nudge-result? result)
                                         (normalize-system-nudge :normal result)
                                         (do
                                           (tel/log! {:level :warn
                                                      :id ::invalid-extension-nudge
                                                      :data {:ext (:ext/namespace ext)
                                                             :explain (extension/explain-system-nudge-result result)}}
                                             "Extension nudge-fn returned invalid nudge; skipping")
                                           nil)))
                                     (catch Throwable t
                                       (tel/log! {:level :warn :data {:ext (:ext/namespace ext) :error (ex-message t)}})
                                       nil)))))
                         active-extensions)))
        all-nudges (cond-> []
                     title-line       (conj (normalize-system-nudge :low title-line))
                     pressure-line    (conj (normalize-system-nudge :high pressure-line))
                     (seq ext-nudges) (into ext-nudges))
        nudges-block (system-nudges-block all-nudges)
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [active-skills-block recent-block var-block nudges-block])]
    (when (seq parts)
      (str/join "\n" parts))))

;; =============================================================================
;; Initial messages
;; =============================================================================

(defn previous-turn-context-block
  "Full previous exchange context for follow-up turns.

   Vis deliberately does not replay the whole chat transcript; prior work
   flows through <journal>/<var_index>. But one-turn follow-ups like `A`,
   `yes`, or `do it` need the complete immediately previous answer as their
   referent. Do not truncate this block: provider/context management owns the
   final context budget."
  [{:keys [user-request answer]}]
  (let [answer (some-> answer str str/trim)]
    (when (and answer (not (str/blank? answer)))
      (prompt-block
        "previous_turn_context"
        (str
          (when-not (str/blank? (str user-request))
            (str (prompt-block "previous_user_request" user-request)
              "\n\n"))
          (prompt-block "previous_assistant_answer" answer))))))

(defn assemble-initial-messages
  "Initial provider messages for one turn. Deliberately excludes full prior
   dialog transcript: Vis state flows through SYSTEM vars, <journal>,
   <var_index>, and DB-backed tools. The current user request is tagged as
   <user_turn_request_main_goal>.

   One full previous-turn context block may be prepended so short follow-ups
   (`A`, `do it`, `yes`) keep the referent from the prior final answer without
   replaying the whole conversation."
  [{:keys [system-prompt initial-user-content previous-turn-context]}]
  (let [previous-block (previous-turn-context-block previous-turn-context)
        user-block     (when initial-user-content
                         (prompt-block "user_turn_request_main_goal" initial-user-content))]
    (vec
      (concat
        (when system-prompt [{:role "system" :content system-prompt}])
        (when user-block
          [{:role "user" :content (str/join "\n\n" (keep identity [previous-block user-block]))}])))))

;; =============================================================================
;; System prompt
;; =============================================================================

;; Citation: Michael Whitford, Nucleus, https://github.com/michaelwhitford/nucleus
;; The core prompt uses a Nucleus-style symbolic center and a VSM-shaped
;; operating stack, with attribution kept in source instead of prompt text.
(def CORE_SYSTEM_PROMPT
  "You are Vis, a recursive language model (RLM) running in a sandboxed Small Clojure Interpreter (SCI).\n\nLoop: read/eval/observe loop\n  emit Clojure forms -> host evaluates them -> results/errors enter <journal> -> continue or answer.\n\nOperate:\n  understand -> inspect -> change minimally -> run targeted checks -> answer.\n\nState -> decision matrix -> observed new state: UNDERSTAND INTENT EXPLORE OBSERVE ACT VERIFY ANSWER.\nCorrect multi-iteration finish pattern: iteration N+1: final turn-finisher after observed evidence, exactly one top-level form.\nHost context: <journal> <var_index> turn-state TURN_USER_REQUEST CONVERSATION_TITLE (answer ARG) (conversation-title ARG) (shape x). `(answer ARG)` is terminal.\n\nRules:\n  - Runtime truth beats source, docs, and assumption.\n  - For repo work, first iteration probes only; do not answer before observing state.\n  - Reply with exactly one executable clojure fenced block.\n  - If no answer form appears, the host continues the turn.\n  - After iteration 1, answer must be the only top-level form in its final iteration.\n  - Use v/needs-input only when required user material is missing.\n  - No extra workflow ledger.\n  - Verification means concrete command/tool output, not a separate record.\n  - Do normal inspect -> change -> test -> answer; inspect before edit.\n  - Skills are internal: TURN_ACCESSIBLE_SKILLS lists summaries; activate with (load-skill name), reload with (reload-skills!).\n  - Final answers name changed files/actions, checks run, and caveats.\n\nUseful pattern:\n  (def hits (v/rg {:all [\"foo\"] :paths [\"src\"]}))\n  ;; observe next iteration, then patch/check/answer.\n")

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
   activation; call ONCE at the top of a turn."
  [environment]
  (when-let [exts (some-> (:extensions environment) deref seq)]
    (vec
      (filter (fn [ext]
                (try
                  (boolean (call-extension-callback ext (:ext/activation-fn ext) environment))
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

   Returns a vec of compact, fully-realized data maps - NO functions,
   NO atoms, NO opaque runtime objects. The model walks this with
   `filter` / `keep` / `some` exactly like any other Clojure data
   structure; never has to reach into `(v/extensions)` just to
   discover what's loaded.

   Per element:
     :alias     - short symbol the model calls under (`'v`, `'z`,
                  `'git`, ...). nil when the extension didn't declare
                  an `:ext/ns-alias`.
     :namespace - fully-qualified ns symbol of the extension.
     :doc       - one-line LLM description from `:ext/doc` (when set).
     :kind      - categorical bucket (providers, channels, foundation,
                  languages, persistance, ...) used as the section
                  label both in this snapshot and in `vis extensions
                  list` (when set).
     :registry-id - canonical manifest/docs id, usually the alias symbol.
     :symbols   - vec of bare symbol names the extension intern'd into
                  the sandbox (just the names; signatures + doc come
                  from `(v/symbol-doc ...)` if the model wants them).
     :docs      - vec of doc-name strings (e.g. `\"README.md\"`) the
                  extension ships in its `vis.edn` registry. Reachable
                  via `(v/extension-doc 'id)`.

   The vec is bound ONCE at turn start (see `iteration-loop`) and
   stays frozen for the rest of the turn - every iteration sees the
   same value."
  [active-extensions]
  (->> (or active-extensions [])
    (mapv (fn [ext]
            (let [info (extension/extension-info ext)
                  registry-id (:registry-id info)
                  ;; Resolve doc names through the global extension
                  ;; registry. Same mapping `(v/extensions)` uses;
                  ;; we duplicate the lookup here (instead of calling
                  ;; the meta extension) because the loop layer is
                  ;; upstream of every ext, including meta itself -
                  ;; TURN_ACTIVE_EXTENSIONS must work even when vis-foundation
                  ;; isn't on the classpath.
                  doc-names   (try (if registry-id
                                     (extension/extension-doc-names registry-id)
                                     [])
                                (catch Throwable _ []))]
              (cond-> {:namespace   (:namespace info)
                       :alias       (:alias info)
                       :doc         (:doc info)
                       :kind        (:kind info)
                       :registry-id registry-id
                       :symbols     (mapv :ext.symbol/sym (:ext/symbols ext))
                       :docs        (vec doc-names)}
                (nil? (:alias info)) (dissoc :alias)
                (nil? (:doc info)) (dissoc :doc)
                (nil? (:kind info)) (dissoc :kind)
                (nil? registry-id) (dissoc :registry-id)))))))

(def active-extensions-snapshot
  "Public alias for `extensions-snapshot`."
  extensions-snapshot)

(def ^:private skills-prompt-budget-bytes
  "Total byte cap for the rendered internal <skills> block. Full bodies
   stay lazy; the block advertises names/descriptions plus activation."
  8192)

(defn- xml-attr
  [v]
  (str/escape (str v) {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;" \' "&#39;"}))

(defn- skill-preview-line
  [{:keys [name source description]}]
  (str "  <skill name=\"" (xml-attr name) "\" source=\"" (xml-attr (or source :unknown)) "\">\n"
    "    <activation_trigger>" (xml-attr description) "</activation_trigger>\n"
    "  </skill>"))

(defn skills-prompt-block
  "Render Vis' internal `<skills>` prompt block. Skills are not extensions:
   discovery, activation, and cache-busting are host-owned. Returns nil
   when the catalog is empty."
  [skills-list]
  (let [skills-list (vec skills-list)]
    (when (seq skills-list)
      (let [tail-line (str "  ;; Filter:   TURN_ACCESSIBLE_SKILLS    ; SYSTEM var, vec of {:name :description :path :source}\n"
                        "  ;; Activate: (load-skill \"name\")      ; internal host primitive, loads full SKILL.md body")
            header    (str "<skills count=\"" (count skills-list) "\">")
            footer    "</skills>"
            [lines dropped]
            (loop [acc []
                   used (+ (count header) 1 (count tail-line) 1 (count footer))
                   remain skills-list]
              (if (empty? remain)
                [acc 0]
                (let [line (skill-preview-line (first remain))
                      size (inc (count line))]
                  (if (and (seq acc) (> (+ used size) skills-prompt-budget-bytes))
                    [acc (count remain)]
                    (recur (conj acc line) (+ used size) (rest remain))))))
            trunc (when (pos? dropped)
                    (str "  + " dropped " more skills not shown (prompt budget). "
                      "Enumerate full list via TURN_ACCESSIBLE_SKILLS."))
            body  (->> (cond-> lines
                         trunc (conj "" trunc)
                         true  (conj "" tail-line))
                    (str/join "\n"))]
        (str header "\n" body "\n" footer)))))

(defn accessible-skills-snapshot
  "Build the value of the `TURN_ACCESSIBLE_SKILLS` SYSTEM var: a vec of
   compact skill summaries the model can `filter`/`map`/`some` over
   without paying for the full SKILL.md body.

   Per element: `{:name :description :path :source :extra}`. The `:body`
   field is INTENTIONALLY omitted - it lazy-loads via `(load-skill name)`,
   the canonical internal activation surface. Pulling every body into a
   SYSTEM var would balloon turn-start memory for a value the model rarely
   needs in full (the internal `<skills>` block already shows name +
   description).

   Frozen ONCE at turn start (see `iteration-loop`). The model sees the
   same vec across every iteration of the same turn."
  []
  (try
    (->> (skills/list-all)
      (mapv (fn [s]
              (cond-> (select-keys s [:name :description :path :source])
                (seq (:extra s)) (assoc :extra (:extra s))))))
    (catch Throwable t
      (tel/log! {:level :warn :id ::skills-snapshot-failed
                 :data  {:error (ex-message t)}}
        "TURN_ACCESSIBLE_SKILLS snapshot failed; defaulting to []")
      [])))

(defn- render-extension-prompt-block
  "Render one extension's contribution to the system prompt. Honors
   ONLY `:ext/prompt`. The previous implementation also auto-rendered
   every `:ext/symbols` entry as a `- (alias/sym args) - docstring`
   line, which silently ballooned the prompt for any extension whose
   `:ext/symbols` was a thin wrapper around an upstream library: e.g.
   older `vis-language-clojure` builds dumped many upstream zipper
   publics with full upstream docstrings (~7000 tokens), every
   iteration, of every conversation - even when the user just typed
   a one-word greeting. Authors who want their tools advertised in
   the prompt write `:ext/prompt` (string or `(fn [env] string)`);
   sandbox bindings are independently callable from `:code` whether
   advertised or not. Extensions that legitimately want auto-rendered
   symbol lines may call `extension/render-prompt` from inside their
   own `:ext/prompt` fn - explicit beats clever."
  [environment ext]
  (try
    (when-let [extra-fn (:ext/prompt ext)]
      (let [body (call-extension-callback ext extra-fn environment)]
        (when (and (string? body) (not (str/blank? body)))
          (let [{ns-sym :ns alias-sym :alias} (:ext/ns-alias ext)]
            (str "<extension namespace=\"" (:ext/namespace ext) "\""
              (when alias-sym (str " alias=\"" alias-sym "\""))
              (when ns-sym (str " target-namespace=\"" ns-sym "\""))
              ">\n"
              (when (and alias-sym ns-sym)
                (str "[namespace: " alias-sym " -> " ns-sym "]\n"))
              body
              (when-not (str/ends-with? body "\n") "\n")
              "</extension>")))))
    (catch Throwable t
      (tel/log! {:level :error :id ::ext-prompt-error
                 :data {:ext (:ext/namespace ext) :error (ex-message t)}}
        (str "Extension '" (:ext/namespace ext) "' prompt rendering failed"))
      nil)))

(defn- render-extensions-prompt-block
  [environment active-extensions]
  (when-let [sections (seq (keep #(render-extension-prompt-block environment %)
                             active-extensions))]
    (str "<extensions>\n"
      (str/join "\n\n" sections)
      "\n</extensions>")))

(defn- normalize-environment-info-body
  [body]
  (cond
    (nil? body) nil
    (string? body) (when-not (str/blank? body) body)
    (sequential? body) (let [joined (->> body
                                      (keep normalize-environment-info-body)
                                      (str/join "\n"))]
                         (when-not (str/blank? joined) joined))
    :else (str "<environment-info-error>invalid :ext/environment-info-fn return; "
            "expected string or seq of strings, got " (.getName (class body))
            "</environment-info-error>")))

(defn- render-environment-info-section
  "Render one active extension's dedicated environment-info contribution.

   This is distinct from `:ext/prompt`: it is for live, runtime facts
   such as repository state, cwd, versions, feature flags, or extension
   diagnostics. The host owns the section wrapper so any extension can
   add to the same prompt area without copying the foundation prompt."
  [environment ext]
  (try
    (when-let [info-fn (:ext/environment-info-fn ext)]
      (when-let [body (normalize-environment-info-body (call-extension-callback ext info-fn environment))]
        (str "<section extension=\"" (:ext/namespace ext) "\">\n"
          body
          (when-not (str/ends-with? body "\n") "\n")
          "</section>")))
    (catch Throwable t
      (tel/log! {:level :error :id ::ext-environment-info-error
                 :data {:ext (:ext/namespace ext) :error (ex-message t)}}
        (str "Extension '" (:ext/namespace ext) "' environment-info rendering failed"))
      nil)))

(defn- render-environment-info-block
  [environment active-extensions]
  (when-let [sections (seq (keep #(render-environment-info-section environment %)
                             active-extensions))]
    (str "<environment-info>\n"
      (str/join "\n\n" sections)
      "\n</environment-info>")))

(defn- render-provider-prompt-block
  "Render the active provider's append-only prompt contribution.

   `provider-prompt-context` is assembled by the loop so this namespace
   stays pure: `{:provider sanitized-provider-config
                 :descriptor registered-provider-descriptor
                 :model resolved-model
                 :environment environment}`.

   The provider prompt never replaces the core Vis prompt; it is a
   small provider-specific addendum. Hook failures are logged and the
   prompt contribution is skipped."
  [{:keys [provider descriptor] :as provider-prompt-context}]
  (try
    (when-let [prompt-fn (:provider/prompt-fn descriptor)]
      (let [body (prompt-fn provider-prompt-context)]
        (when (and (string? body) (not (str/blank? body)))
          (str "<specific_provider_model_prompt provider=\"" (name (:id provider)) "\""
            (when-let [model-name (:name (:model provider-prompt-context))]
              (str " model=\"" model-name "\""))
            ">\n"
            body
            (when-not (str/ends-with? body "\n") "\n")
            "</specific_provider_model_prompt>"))))
    (catch Throwable t
      (tel/log! {:level :warn :id ::provider-prompt-error
                 :data {:provider (:id provider)
                        :error    (ex-message t)}
                 :msg  (str "Provider prompt hook for " (:id provider)
                         " failed; skipping provider prompt block")})
      nil)))

(defn assemble-system-prompt
  "Build the full system prompt: core agent rules + active extension
   environment-info sections + active-extension prompts + the active
   provider's append-only prompt block.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`.

   Optional opts:
     `:provider-prompt-context` - map for provider `:provider/prompt-fn`:
        `{:provider sanitized-provider-config
          :descriptor registered-provider-descriptor
          :model resolved-model
          :environment environment}`."
  [environment {:keys [system-prompt active-extensions provider-prompt-context] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-system-prompt requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [base         (prompt-block "system_prompt" (build-system-prompt {:system-prompt system-prompt}))
        skills-block (skills-prompt-block (skills/list-all))
        env-info     (render-environment-info-block environment active-extensions)
        ext-ps       (render-extensions-prompt-block environment active-extensions)
        provider-p   (when provider-prompt-context
                       (render-provider-prompt-block provider-prompt-context))
        blocks       (seq (cond-> [base]
                            skills-block (conj skills-block)
                            env-info     (conj env-info)
                            ext-ps       (conj ext-ps)
                            provider-p   (conj provider-p)))]
    (str/join "\n\n" blocks)))

;; =============================================================================
;; Model-facing context impact (CTX1)
;;
;; Reports the exact byte/token cost of every surface that lands in the
;; provider prompt for a given turn shape. Used by:
;;   - tests that assert trivial / no-tool turns carry substantially less
;;     model-facing context than coding turns;
;;   - diagnostic tooling that wants to attribute prompt size to a
;;     specific surface (system / user goal / iteration trailer) without
;;     reaching into private helpers;
;;   - regression and diagnostic callers that need a single, reproducible
;;     "exact token/context impact" number per turn.
;;
;; Surfaces, in send order:
;;   :system                 system prompt (cached per turn)
;;   :user-turn-request      <user_turn_request_main_goal> wrapper
;;   :iteration-trailer      per-iteration <journal> + <var_index> +
;;                           <active_skills> + <system_nudges> bundle
;;
;; Trivial / no-tool turns produce a nil iteration trailer; the function
;; returns 0 bytes / 0 tokens for that surface so callers see the floor
;; explicitly instead of having to special-case nil.
;; =============================================================================

(defn- surface-stats
  [model surface text]
  {:surface surface
   :bytes  (long (count (or text "")))
   :tokens (count-tokens model (or text ""))})

(defn model-facing-context-stats
  "Compute byte + token cost of each model-facing surface for one turn.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Same
       contract as `assemble-system-prompt` / `build-iteration-context`.
     `:user-request` - the exact user text for the current turn.

   Optional opts:
     `:system-prompt` - caller addendum threaded into the core prompt.
     `:provider-prompt-context` - provider/model-specific prompt block.
     `:blocks-by-iteration` - same shape `build-iteration-context` takes.
     `:iteration` / `:context-limit` / `:title-refresh?` - forwarded.
     `:model` - model id used for the tokenizer; falls back to chars/4.

   Returns:
     {:model ...
      :surfaces [{:surface :system            :bytes N :tokens T}
                 {:surface :user-turn-request :bytes N :tokens T}
                 {:surface :iteration-trailer :bytes N :tokens T}]
      :total {:bytes N :tokens T}
      :iteration-trailer-empty? bool}

   The returned map is plain data; the assembler is the source of
   truth, this function only measures."
  [environment {:keys [active-extensions system-prompt provider-prompt-context
                       user-request blocks-by-iteration iteration context-limit
                       title-refresh? model] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "model-facing-context-stats requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [system (assemble-system-prompt environment
                 {:active-extensions active-extensions
                  :system-prompt system-prompt
                  :provider-prompt-context provider-prompt-context})
        user   (when (and user-request (not (str/blank? user-request)))
                 (prompt-block "user_turn_request_main_goal" user-request))
        trailer (build-iteration-context environment
                  {:active-extensions active-extensions
                   :blocks-by-iteration blocks-by-iteration
                   :iteration iteration
                   :model model
                   :system-prompt system
                   :current-user-content user
                   :context-limit context-limit
                   :title-refresh? title-refresh?})
        surfaces [(surface-stats model :system system)
                  (surface-stats model :user-turn-request user)
                  (surface-stats model :iteration-trailer trailer)]
        total-bytes (reduce + (map :bytes surfaces))
        total-tokens (reduce + (map :tokens surfaces))]
    {:model model
     :surfaces surfaces
     :total {:bytes total-bytes :tokens total-tokens}
     :iteration-trailer-empty? (nil? trailer)}))
