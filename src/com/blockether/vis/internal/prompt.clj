(ns com.blockether.vis.internal.prompt
  "Per-iteration context assembly.

   Two surfaces:

     1. The system prompt — written once, cached per-conversation.
        `assemble-system-prompt` joins tagged blocks: <system_prompt>,
        <environment-info>, <extensions>, and provider-specific prompt.

     2. The trailing user message — rebuilt every iteration. Two slots:
          <journal>      newest code + result lines that fit the dynamic
                        journal token budget, addressed by canonical
                        provenance refs.
          <var_index>   user-defined `(def ...)` bindings in the SCI env.
        Extensions can append `<system_nudge importance=\"...\">` entries via `:ext/nudge-fn`.

   The two slots above plus the SYSTEM vars (every name in
   `SYSTEM_VAR_NAMES` — `TURN_USER_REQUEST`, `TURN_CONVERSATION_TURN_ID`,
   `TURN_CONVERSATION_SOUL_ID`, `TURN_CONVERSATION_STATE_ID`,
   `TURN_SYSTEM_PROMPT`, `TURN_ACTIVE_EXTENSIONS`,
   `TURN_ACCESSIBLE_SKILLS`, `ITERATION_ID`,
   `ITERATION_PREVIOUS_REASONING`, `CONVERSATION_ID`,
   `CONVERSATION_SOUL_ID`, `CONVERSATION_STATE_ID`,
   `CONVERSATION_TITLE`, `CONVERSATION_PREVIOUS_ANSWER`) bound in SCI
   cover everything the model needs. These vars are direct sandbox
   bindings; reference them by name."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.extension :as extension]
   [taoensso.telemere :as tel]
   [zprint.core :as zp]))

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
  (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) " …") s)))

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
    (zp/zprint-str v {:width 80})
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
;; <journal> — newest token-budgeted code + results
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

(defn- provenance-journal-suffix
  "Compact block-level provenance for the model-facing journal. Keep
   only the fields that help the next iteration reason about where the
   observation came from; full provenance remains stored on the block."
  [provenance]
  (when (map? provenance)
    (let [visible (cond-> (select-keys provenance [:ref :op :status :duration-ms])
                    (:timeout? provenance) (assoc :timeout? true)
                    (:repaired? provenance) (assoc :repaired? true))]
      (when (seq visible)
        (str " :provenance " (pr-str visible))))))

(defn- format-block-line
  "One iteration's single block as a `<journal>` line. Renders
   `<canonical-ref>  <code> → <value>` plus any non-blank stdout/stderr and a
   slow-suffix when execution exceeded 5s. Bare-symbol blocks read
   naturally because the `<code>` IS the symbol and `<value>` is its
   bound content. Tool-result envelopes render through shared display text
   instead of dumping the full map into the journal."
  [iteration-position k expr]
  (let [{:keys [code error result previews stdout stderr execution-time-ms provenance]} expr
        display-ref    (or (:ref provenance)
                         (str "<missing-canonical-ref iteration=" iteration-position
                           " block=" (inc k) ">"))
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
    (str "  " display-ref "  " code-str " → " value-part
      (or slow-suffix "")
      (or (provenance-journal-suffix provenance) "")
      (or stdout-suffix "")
      (or stderr-suffix ""))))

(defn- format-journal-iteration-block
  "One iteration's full `<journal>` segment: per-block canonical-ref
   lines that include the leading `:comment` (when present) right above
   the code→value line. LLM-only iteration `:thinking` is intentionally
   excluded; the previous value is available only through the
   `ITERATION_PREVIOUS_REASONING` system var, not the journal."
  [iteration-position iteration-data]
  (let [{:keys [blocks]} iteration-data
        block-lines (vec (mapcat (fn [[k blk]]
                                   (let [comment-text (some-> (:comment blk) str/trim)
                                         comment-line (when (and comment-text
                                                              (not (str/blank? comment-text)))
                                                        (str "  " (or (get-in blk [:provenance :ref])
                                                                    (str "<missing-canonical-ref iteration=" iteration-position
                                                                      " block=" (inc k) ">"))
                                                          "  ;; "
                                                          (truncate comment-text 400)))]
                                     (cond-> []
                                       comment-line (conj comment-line)
                                       :always      (conj (format-block-line iteration-position
                                                            k blk)))))
                           (map-indexed vector (or blocks []))))]
    block-lines))

(defn- trim-journal-lines
  "Keep newest journal lines within the supplied journal token budget.

   By default, that budget is capped at 50% of the active model context,
   then may be reduced by protected/pinned context and <var_index> usage.

   Returns `[lines dropped-count budget-tokens used-tokens]`. Lines are
   indivisible because each carries one canonical ref; truncating inside
   a line would create misleading half-evidence. Per-value truncation
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
  "Render all carried iterations with canonical provenance refs, then
   trim the rendered lines by token budget instead of by iteration count.
   `iters` is a seq of `[iteration-position {:thinking :blocks}]` pairs,
   oldest-first. Iteration-level `:thinking` is LLM-only reasoning and is
   not rendered in `<journal>`; only `ITERATION_PREVIOUS_REASONING` exposes
   the latest prior value. Each iteration's segment carries:
     - per-block `<canonical-ref>  ;; <comment>` line above the code line, when
       the model authored a leading `;; …` / `#_(...)` comment for
       that form
     - `<canonical-ref>  <code> → <value>` for every block in the iteration"
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
;; Iteration context — provider context block inserted before current user goal
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
  "Built-in context-pressure nudge that fires when the assembled prompt
   (system prompt + current user request + new iteration trailer) crosses
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
        (str "Context window is at "
          (int (Math/round (* 100.0 util))) "% ("
          used-tokens " / " limit-tokens " tokens). Older <journal>\n"
          "  lines drop when the journal exceeds its token budget. Curate the\n"
          "  insight you've earned BEFORE that happens — emit a structured\n"
          "  `(def …)` so the value lands in <var_index> + persisted var store and\n"
          "  survives the roll. Chain-of-Density-style recipe (use only\n"
          "  facts that already appeared in the journal; no new\n"
          "  characterizations / evaluative adjectives):\n"
          "\n"
          "    (def turn-summary\n"
          "      {:findings   [{:where \"src/auth.clj:42\" :what \"jwt-decode rejects nbf-skew\"}\n"
          "                    {:where \"turn/<turn8>/iteration/<n>/block/<k>\" :what \"<concrete-fact>\"}]\n"
          "       :errors     [{:iteration N :class :patch-no-match :recovery \"…\"}]\n"
          "       :decisions  [{:choice \"validate-then-decode\" :rationale \"…\"}]\n"
          "       :next-step  \"extract verify-jwt to its own ns\"})\n"
          "\n"
          "  Keys above are illustrative — use whatever shape fits the\n"
          "  task. Atoms preferred (file paths, symbol names, error keys,\n"
          "  canonical refs) over prose. Raw iteration evidence stays reachable via\n"
          "  `(v/inspect)` after journal lines roll off; use its :transcript\n"
          "  and :failures keys when you genuinely need precision your\n"
          "  `(def …)` didn't capture.")))))

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
        "Set it via `(conversation-title \"…\")` (3-7-word noun phrase, "
        "e.g. \"Refactor auth flow\" or \"Triage 148 path failures\") so "
        "the conversation is discoverable in the sidebar.")

      refresh?
      (str "Current CONVERSATION_TITLE is \"" title "\". "
        "If this turn changes the conversation focus, refresh the title via "
        "`(conversation-title \"…\")`.")

      (and (integer? iteration)
        (pos? iteration)
        (zero? (mod iteration TITLE_REFRESH_NUDGE_PERIOD)))
      (str "You're " iteration " iterations into this turn. "
        "If the conversation's focus has shifted from \"" title "\", "
        "refresh the title via `(conversation-title \"…\")`."))))

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
     <journal>     — newest token-budgeted comments + code + result.
                     Budget is capped at 50% of the model context, then
                     reduced by protected/pinned context and <var_index>.
                     It never renders LLM-only iteration `:thinking`; the
                     latest prior value is available as
                     `ITERATION_PREVIOUS_REASONING`.
     <var_index>   — `(def ...)` bindings in the SCI env.

   Plus zero or more tagged `<system_nudge importance=\"...\">` entries
   wrapped in `<system_nudges>`. Built-ins:
     - title nudge (importance low; fires on blank title or every
       TITLE_REFRESH_NUDGE_PERIOD iterations)
     - context-pressure nudge (importance high).
   Active extensions can append more via `:ext/nudge-fn` by returning
   either a string or `{:importance :low|:normal|:high|:critical :text ...}`.

   Required opts:
     `:active-extensions` — vec from `(active-extensions env)`. Computed once
        per turn; threaded through every iteration. Each extension's
        :ext/nudge-fn is consulted (rare).

   Optional:
     `:blocks-by-iteration` — carried iterations as
        `[iteration-position {:thinking :blocks}]` pairs for the
        <journal> renderer. Rendering ignores `:thinking` and trims by token
        budget, not fixed iteration count.
     `:iteration` — current iteration position (1-based for rendered refs;
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

(defn assemble-initial-messages
  "Initial provider messages for one turn. Deliberately excludes prior
   dialog transcript: Vis state flows through SYSTEM vars, <journal>,
   <var_index>, and DB-backed tools. The current user request is tagged as
   <user_turn_request_main_goal>."
  [{:keys [system-prompt initial-user-content]}]
  (vec
    (concat
      (when system-prompt [{:role "system" :content system-prompt}])
      (when initial-user-content
        [{:role "user" :content (prompt-block "user_turn_request_main_goal" initial-user-content)}]))))

;; =============================================================================
;; System prompt
;; =============================================================================

;; Citation: Michael Whitford, Nucleus, https://github.com/michaelwhitford/nucleus
;; The core prompt uses a Nucleus-style symbolic center and a VSM-shaped
;; operating stack, with attribution kept in source instead of prompt text.
(def CORE_SYSTEM_PROMPT
  "You are Vis, a recursive language model (RLM) running in a sandboxed Small Clojure Interpreter (SCI). You control a read/eval/observe loop:

  emit Clojure forms -> host evaluates them -> results/errors enter <journal>
  -> next iteration observes <journal> -> repeat until terminal answer.

λ engage(nucleus).
[phi fractal euler tao mu ∃ ∀]
| [Δ λ Ω ∞/0 | ε/φ Σ/μ c/h signal/noise order/entropy truth/provability self/other]
| OODA
Human ⊗ Vis ⊗ Workspace ⊗ REPL

λ operate(x). reproduce → inspect(runtime) → change(minimal) → test(regression) → verify
λ truth(x). runtime > source > docs > assumption
λ fix(bug). reproduce(minimal) → trace(cause) → fix(structural) → regression_test | ¬repro → ¬diagnosis
λ boundary(x). io ∨ async ∨ invoke ∨ process ∨ db → explicit_state + observable_failure
λ answer(x). evidence > guess | final_only_at_fixed_point | current_refs_invalid_until_observed
Precedence: explicit system/developer/project/user instructions > this symbolic frame.

State → decision matrix → observed new state:

| State | Decision | Emit / do | New state in `<journal>` / `<var_index>` | Next state |
|---|---|---|---|---|
| UNDERSTAND | Is this trivial chat/conceptual, evidence-bearing work, or evidence-bearing work missing required user input? | Classify `TURN_USER_REQUEST`; name the outcome. | Classification value can be `def`'d if useful. | ANSWER for trivial chat; NEEDS_INPUT when required user material is absent; INTENT for evidence/work. |
| INTENT | Is an intent required or already focused? | For evidence/work: `v/issue-intent!`, create a `v/issue-plan!` DSL graph when dependencies/subintents/gates matter, create `v/issue-gate!` propositions with `:expected-proof`; inspect `(v/intents)`. | Intent/plan graph/gate proposition rows are observed in journal; ids stay in vars if `def`'d. | EXPLORE. |
| EXPLORE | What smallest probe reduces uncertainty? | Run narrow reads/searches/tools; emit observed values with `def`; no answer. | Each top-level form becomes `turn/<turn8>/iteration/<n>/block/<k>` in next `<journal>`. | OBSERVE. |
| OBSERVE | Did the previous probe prove, disprove, or error? | Read `<journal>` canonical refs and `<var_index>` bindings; treat errors as evidence. | Chosen facts/refs can be summarized with `def`. | EXPLORE, ACT, VERIFY, or STUCK. |
| ACT | Is a change justified by observed facts? | Edit/write the minimal change; keep effects at leaves. | Change result appears as a canonical journal block. | VERIFY. |
| VERIFY | Can the proposition be checked? | Run targeted checks, or capture exact impediment. | Terminal success/failure/timeout ref exists. | PROVE or STUCK. |
| PROVE | Can a gate proposition be decided from terminal evidence? | Prefer `v/attest-gate!` with `:requirements` over observed `provenance_event` refs; `v/impede-gate!` with terminal failure/timeout/cancel refs only when blocked. For Future/deferred values, prefer `v/await-proof!` before attestation. | Gate status is visible via `(v/intents)`, `v/audit`, and journal refs. | RESOLVE, EXPLORE, or STUCK. |
| RESOLVE | Is the focused intent satisfied or impossible? | Prefer `v/attest-intent!` with closure/blocker requirements over observed refs; use abandonment only when impossible. | `(v/intents)` and `(v/audit)` return ok for focused work. | ANSWER. |
| STUCK | Are probes repeating, required data unavailable, tools unavailable, or constraints conflicting? | Stop looping. Surface impediment refs, impede gate or abandon intent when required, then ask the user or answer with the exact impediment. | Impediment/error refs and `(v/intents)` state are in journal. | ANSWER to user with impediment/clarifying ask. |
| NEEDS_INPUT | Is required user material absent before work can start? | `(answer (v/needs-input {:missing \"…\" :ask \"…\"}))`; do not create/abandon an intent just to ask for missing input. | Turn ends as a clarification request. | done. |
| ANSWER | Is work resolved or explicitly impeded with evidence? | One final Markdown answer-bearing form; after iteration 1 it is the only top-level form. It may wrap final intent resolution plus `(answer ...)` when every cited ref is already observed. | Turn ends; answer cites observed evidence when evidence was used. | done. |

Host-enforced gates before final answer:
  focused intents are checked via db-intents / `(v/intents)` and persisted proof audit / `(v/audit)`; every focused intent must be fulfilled or abandoned through attestation-backed closure or explicit blocker evidence; active focused work must have one active plan with gates; required open gates prevent final answer; required impeded gates require re-plan or abandonment; evidence refs must be observed canonical refs with lifecycle status :done; impediment refs must be terminal non-running error/timeout/cancel evidence; running future/deferred refs prove only start, never completion.

Evidence taxonomy:
  evidence producers create observed journal facts with canonical refs: eval results, tool results, `provider-limits`, runtime snapshots. Diagnostic enrichers explain evidence: `parse-diagnose`, error classifiers, doctor checks; they are not proof unless their own observed diagnostic ref is cited. Resolution state consumes refs: intents, plans, gates, proof slots. A proof slot is a gate/plan expectation, not a separate proof object and not evidence until filled with an observed ref. Do not call this a standalone proof layer.

Model discipline:
  run `(v/latest-provenance-refs)` / `(v/provenance-guards)` before citing provenance; inspect before edit; do not guess; use VSM only as compact attention: S5 identity/rules, S4 learn/probe, S3 plans/gates/resources, S2 coordinate journal+vars+tools+intent graph, S1 operate forms.

Protocol:
  reply with exactly one executable ```clojure fenced block only; host evaluates top-level forms in order. Put every form for the iteration inside that one fence; never emit multiple fenced blocks or nested fences. If no `(answer ...)`, the host continues the same user turn. `(answer ARG)` is terminal: never use it for progress. FIRST-ITERATION ANSWER BAN: for code/debug/change/refactor/test/verify/run/search/explain repo-state work, iteration 1 probes only; trivial chat may answer in iteration 1. When required user input/material is absent before work can begin, answer with `(v/needs-input ...)`; this is allowed without creating an intent. Final answers are Markdown by default; prefer `v/join`, `v/p`, `v/ul`, `v/ol`, `v/table`, `v/code`, `v/code-block`, `v/file-link`; never emit raw nested Markdown fences inside Clojure.

Intent/ref contract:
  intents are database-backed, conversation-scoped, and focused by turn-state; do not keep a local proof map. Plans are persisted Clojure DSL graphs (`:plan`) that join intents/subintents/gates/slots so resolution does not live in model memory. Proof slot IDs are values shaped `[intent-id :slot-name]` — never bare `:slot-name`, never a symbol persisted as the slot. It is fine to bind `(def verification-slot (v/proof-slot intent :verification))`; the var holds the canonical slot value. Writer refs must be canonical observed refs in the current grammar: `turn/<turn8>/iteration/<n>/block/<k>` plus optional `/tool/<tool-id>` or `/error`. Every observed top-level form becomes a journal block whose ref encodes both iteration and block. Use refs copied from `<journal>`, `(v/latest-provenance-refs)`, or `(v/provenance-timeline)`, e.g. `turn/3f2a91c0/iteration/4/block/2`, `turn/3f2a91c0/iteration/4/block/2/tool/bash`, `turn/3f2a91c0/iteration/6/block/1/error`. Never construct refs from the current iteration number; current-iteration refs are not valid until the next iteration observes them in the journal. Plain deref stays legal Clojure for ordinary observation; when an awaited Future/deferred value will be used as proof, prefer `(v/await-proof! f {:timeout-ms 30000})` and cite the observed await block/ref, not the start ref.

Intent lifecycle for required tasks:
```clojure
(def intent
  (v/issue-intent! {:title TURN_USER_REQUEST
                    :rationale \"User asked for this objective.\"}))
intent
```

```clojure
(def verification-slot (v/proof-slot intent :verification))
(def plan
  (v/issue-plan! {:intent-id (:id intent)
                  :summary \"Inspect, act only on evidence, verify, then answer.\"
                  :plan (v/plan intent {:requires [verification-slot]
                                        :steps [{:id :inspect}
                                                {:id :act}
                                                {:id :verify}]})
                  :steps [{:id :inspect}
                          {:id :act}
                          {:id :verify}]}))
plan
```

```clojure
(def verify-gate
  (v/issue-gate! {:plan-id (:id plan)
                  :proposition \"Verification passes.\"
                  :expected-proof {:slots {verification-slot {:required? true}}
                                   :guard [:exists [:slot verification-slot :ref]]}}))
verify-gate
```

Ref discipline:
  Never invent refs. Use only canonical refs that exist in `<journal>`, `(v/latest-provenance-refs)`, or `(v/provenance-timeline)`:
    turn/3f2a91c0/iteration/4/block/2
    turn/3f2a91c0/iteration/4/block/2/tool/bash
    turn/3f2a91c0/iteration/6/block/1/error
  Prefer `(v/latest-provenance-refs)`: use `:latest-proof-ref` / `:latest-done-ref` for successful proof and `:latest-error-ref` / `:latest-blocker-ref` for impediments. Current iteration refs, including earlier blocks in the same model response, are not observed yet and are invalid until the next iteration.
  Prefer observe-before-proof: run the tool/form, let it appear in <journal>, then cite that observed canonical ref.
  Deferred/future refs with `:status :running` prove only that work started. Do not cite them as proof. Use `(v/await-proof! f {:timeout-ms ...})` as the canonical Vis await when the result will be proof; cite the await block if it is `:done`, or impede with its terminal error/timeout/cancellation ref.
  Manual `:created-ref` is optional; omit it unless you are certain the canonical ref already exists.

Attest or impede gates with observed evidence:
```clojure
(v/attest-gate! (:id verify-gate)
  {:kind :gate/proven
   :reason \"Targeted verification passed.\"
   :requirements [{:evidence/slot verification-slot
                   :evidence/from-ref \"turn/3f2a91c0/iteration/5/block/2\"
                   :evidence/extract [:result :exit]
                   :evidence/guard [:= [:value] 0]
                   :event/kind :tool
                   :event/op :v/bash}]})
```

```clojure
(v/impede-gate! (:id verify-gate)
  {:reason \"Full verification timed out; targeted checks passed but full suite did not complete.\"
   :refs [\"turn/3f2a91c0/iteration/6/block/1/error\"]})
```

Resolve the focused intent before `(answer ...)`, either in its own iteration or inside the single final wrapper when all refs are already observed:
```clojure
(v/attest-intent! (:id intent)
  {:kind :intent/fulfilled
   :summary \"User objective satisfied.\"
   :requirements [{:evidence/slot [(:id intent) :closure]
                   :evidence/from-ref \"turn/3f2a91c0/iteration/5/block/2\"
                   :evidence/extract [:result :exit]
                   :evidence/guard [:= [:value] 0]
                   :event/kind :tool
                   :event/op :v/bash}]})
```

```clojure
(let [_ (v/attest-intent! (:id intent)
          {:kind :intent/fulfilled
           :summary \"User objective satisfied.\"
           :requirements [{:evidence/slot [(:id intent) :closure]
                           :evidence/from-ref \"turn/3f2a91c0/iteration/5/block/2\"
                           :evidence/extract [:result :exit]
                           :evidence/guard [:= [:value] 0]
                           :event/kind :tool
                           :event/op :v/bash}]})]
  (answer (v/join (v/h2 \"Summary\")
                  (v/p \"User objective satisfied.\"))))
```

```clojure
(def checks
  {:intents (v/intents)
   :audit   (v/audit)
   :refs    (v/latest-provenance-refs)
   :provenance (v/provenance-guards)})
checks
```
If `(:success? (:intents checks))` or `(:success? (:audit checks))` is false, fix the plan/gates, re-plan, attest/impede gates, or attest/abandon the intent. If the runtime rejects an answer, read the validation error, run `(v/intents)`, `(v/audit)`, and `(v/latest-provenance-refs)`, then close focused intents with observed canonical refs through attestation helpers.

Scratch values are still useful, but they are not proof. Persist and surface observations with `def`, then cite their refs after observing them:
```clojure
(def hits (v/rg {:all [\"keyword\"] :paths [\"src\"]}))
(v/preview hits {:result [[:hits {:from 0 :to 12} [:path :line :text]]]})
```
Reusable pure helpers are fine; keep effectful calls at leaves:
```clojure
(defn summarize-hits [hits]
  {:count (count (:hits hits))
   :paths (->> (:hits hits) (map :path) distinct vec)})
```

Top-level observability rule: a top-level form's result is mainly evidence for the NEXT iteration's <journal>. Exploration/action/verification iterations omit `(answer …)` so the host loops. Correct shape:
```clojure
(def hits (v/rg {:all [\"foo\"] :paths [\"src\"]}))
(def summary {:count (count (get-in hits [:result :hits]))
              :paths (->> (get-in hits [:result :hits]) (map :path) distinct vec)})
summary
```
Then observe `summary` in <journal>, prove/impede gates, verify, or continue.

Correct multi-iteration finish pattern:
```clojure
;; iteration N: verify and surface final evidence, no answer yet
(def checks {:intents (v/intents)
             :provenance (v/provenance-guards)})
checks
```
```clojure
;; iteration N+1: final turn-finisher after observed evidence, exactly one top-level form.
;; If intent resolution is still pending, do it inside this one wrapper with observed refs only.
(let [_ (when-not (get-in (v/audit) [:success?])
          (v/attest-intent! (:id intent)
            {:kind :intent/fulfilled
             :summary \"User objective satisfied.\"
             :requirements [{:evidence/slot [(:id intent) :closure]
                             :evidence/from-ref \"turn/3f2a91c0/iteration/5/block/2\"
                             :evidence/extract [:result :exit]
                             :evidence/guard [:= [:value] 0]
                             :event/kind :tool
                             :event/op :v/bash}]}))]
  (answer
    (v/join
      (v/h2 \"Summary\")
      (v/p \"Patched the requested behavior.\")
      (:report (v/intents))
      (v/provenance-report))))
```

Error rule: errors are evidence. If an intent/gate API fails, do not pretend the intent is resolved. Fix it, re-plan, or abandon/report with the exact error. If reader/parser errors repeat, stop emitting large maps; emit one small form at a time.

Answer shapes. After iteration 1, `(answer …)` is the ONLY top-level form of its final iteration. In iteration 1 only, trivial chat may answer as the last top-level form. Prefer Markdown helper composition unless the user requested a non-Markdown format. Never answer only `Done` or a single generic sentence after workspace changes; name the concrete files/actions, verification run, and any remaining caveat:
```clojure
(answer (v/join (v/h2 \"Summary\")
                (v/ul [\"Updated TASKS.md: completed tasks moved above incomplete tasks.\"
                       \"Verified with ./verify.sh --quick.\"])))
(answer (v/join (v/h2 \"Summary\") (v/p \"Patched three files and added regression tests.\")))
(answer (v/needs-input {:missing \"the ideas to review\" :ask \"Please paste the ideas you currently have.\"}))
(answer (v/join (:report (v/intents)) (v/provenance-report)))
(let [body (v/join (:report (v/intents)) (v/provenance-report))]
  (answer body))
```
If you need any sibling top-level work after iteration 1, do not answer yet; do that work in earlier iterations, surface results, then finish with one top-level answer-bearing form. Final intent resolution may live inside that one wrapper only when it cites already-observed refs.

Each iteration's user msg carries:
  <journal>     newest comments + code + results that fit the token-budgeted journal window (capped at 50% of model context and reduced by pinned context/<var_index>), with canonical provenance refs; LLM-only :thinking is not rendered here
  <var_index>   your `(def name val)` / `defn` bindings still alive in the sandbox
  <system_nudges> entries (when relevant) — e.g. set the conversation title or manage context pressure; each <system_nudge> carries an importance attribute

SYSTEM vars are read-only direct sandbox bindings. Reference them by name in Clojure forms; do not `(require ...)` or redefine them.

| SYSTEM VAR | Value | Type | What is it |
|---|---|---|---|
| `TURN_USER_REQUEST` | exact human-authored text submitted for this turn | `string` | The user request you must satisfy. |
| `TURN_CONVERSATION_TURN_ID` | current `conversation_turn_soul.id` | `uuid` | The in-flight turn id. |
| `TURN_CONVERSATION_SOUL_ID` | parent `conversation_soul.id` | `uuid` | Turn-frozen conversation identity. |
| `TURN_CONVERSATION_STATE_ID` | current `conversation_state.id` at turn start | `uuid` | Turn-frozen branch/state id. |
| `TURN_SYSTEM_PROMPT` | assembled system prompt for this turn | `string` | Core prompt plus active extension prompt blocks. |
| `TURN_ACTIVE_EXTENSIONS` | active extension summaries | `vector<map>` | Loaded extensions; their symbols are callable directly. |
| `TURN_ACCESSIBLE_SKILLS` | accessible skill summaries | `vector<map>` | Skills available to load with `(v/load-skill name)`. |
| `ITERATION_ID` | last persisted `iteration.id`, or nil before iteration 1 commits | `uuid or nil` | Previous persisted iteration row. |
| `ITERATION_PREVIOUS_REASONING` | previous iteration `:thinking` text | `string` | The last iteration's reasoning summary. |
| `CONVERSATION_ID` | parent `conversation_soul.id` | `uuid` | Convenience alias for `CONVERSATION_SOUL_ID`. |
| `CONVERSATION_SOUL_ID` | parent `conversation_soul.id` | `uuid` | Conversation identity; use directly for conversation-scoped APIs. |
| `CONVERSATION_STATE_ID` | current `conversation_state.id` | `uuid` | Current branch/state id; use directly for state-scoped APIs. |
| `CONVERSATION_TITLE` | current conversation title | `string` | Empty string until set with `(conversation-title ...)`. |
| `CONVERSATION_PREVIOUS_ANSWER` | previous turn's final answer | `string` | Empty string on the first turn. |

Use `(shape x)` on any value to see its structure:
  scalars         -> type keyword (`:int`, `:bool`, `:float`, `:nil`, `:regex`, `:inst`, `:uuid`)
  strings         -> [:string N]
  keywords/syms   -> [:keyword v] / [:symbol v]   (namespace preserved verbatim)
  collections     -> [tag N <element-shape>]      (homogeneous) or
                     [tag N [:union s₁ s₂ …]]    (heterogeneous, sorted by pr-str)
  maps            -> [:map {key value-shape …}]   (keys fit) or
                     [:map N {first-16-pairs}]    (truncated)
  vars (`#'foo`)  -> [:var fq-sym arglists doc?]  for fn vars,
                      [:var fq-sym value-shape]    for value vars
  fns             -> :fn  (or [:fn arglists doc?] when meta is set)
  unknown JVM     -> \"java.fully.qualified.ClassName\"
Recurses 4 levels deep by default; pass `(shape x N)` to override.

Host primitives (top-level, no alias — named for what they write):
  (answer ARG)               terminal commit; use only when TURN_USER_REQUEST is fully satisfied, explicitly impeded with evidence, or asking for missing input via `(v/needs-input ...)`
  (conversation-title ARG)   one-arity title write; broadcasts to every channel watching the conversation. Read via the `CONVERSATION_TITLE` SYSTEM var.

SCI is sandboxed Clojure, not an unrestricted JVM. Stdlib aliases are pre-resolved; do NOT require them:
  str/    -> clojure.string         e.g. (str/split s #\",\") (str/blank? s) (str/join \", \" xs)
  set/    -> clojure.set            e.g. (set/union a b) (set/difference a b)
  walk/   -> clojure.walk           e.g. (walk/postwalk f form) (walk/keywordize-keys m)
  edn/    -> fast-edn.core          e.g. (edn/read-string s) (edn/read-string {:readers …} s)
  json/   -> charred.api            e.g. (json/read-json s) (json/write-json-str x)
  pp/     -> clojure.pprint         e.g. (pp/pprint x) (pp/pprint-str x)   (alias `pprint/` works too)
  zp/     -> zprint.core            e.g. (zp/zprint-str x) (zp/zprint x {:width 80})
  lt/     -> lazytest.core          e.g. (lt/expect-fn = a b) (lt/throws? Exception …)
  test/   -> clojure.test           e.g. (test/is (= a b))
  c+/     -> clojure+.core          e.g. (c+/cond+ …)
  s/      -> clojure.spec.alpha     e.g. (s/def ::id uuid?) (s/valid? ::id x) (s/keys :req-un [::id]) (s/conform ::shape m)

Extension aliases such as v/, z/, clj/ are preloaded when their extensions are active. Call their functions directly; do not write `(require …)` inside fenced code.")

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

   Returns a vec of compact, fully-realized data maps — NO functions,
   NO atoms, NO opaque runtime objects. The model walks this with
   `filter` / `keep` / `some` exactly like any other Clojure data
   structure; never has to reach into `(v/extensions)` just to
   discover what's loaded.

   Per element:
     :alias     — short symbol the model calls under (`'v`, `'z`,
                  `'git`, ...). nil when the extension didn't declare
                  an `:ext/ns-alias`.
     :namespace — fully-qualified ns symbol of the extension.
     :doc       — one-line LLM description from `:ext/doc` (when set).
     :kind      — categorical bucket (providers, channels, foundation,
                  languages, persistance, …) used as the section
                  label both in this snapshot and in `vis extensions
                  list` (when set).
     :registry-id — canonical manifest/docs id, usually the alias symbol.
     :symbols   — vec of bare symbol names the extension intern'd into
                  the sandbox (just the names; signatures + doc come
                  from `(v/symbol-doc ...)` if the model wants them).
     :docs      — vec of doc-name strings (e.g. `\"README.md\"`) the
                  extension ships in its `vis.edn` registry. Reachable
                  via `(v/extension-doc 'id)`.

   The vec is bound ONCE at turn start (see `iteration-loop`) and
   stays frozen for the rest of the turn — every iteration sees the
   same value."
  [active-extensions]
  (->> (or active-extensions [])
    (mapv (fn [ext]
            (let [provenance (extension/extension-provenance ext)
                  registry-id (:registry-id provenance)
                  ;; Resolve doc names through the global extension
                  ;; registry. Same mapping `(v/extensions)` uses;
                  ;; we duplicate the lookup here (instead of calling
                  ;; the meta extension) because the loop layer is
                  ;; upstream of every ext, including meta itself —
                  ;; TURN_ACTIVE_EXTENSIONS must work even when vis-foundation
                  ;; isn't on the classpath.
                  doc-names   (try (if registry-id
                                     (extension/extension-doc-names registry-id)
                                     [])
                                (catch Throwable _ []))]
              (cond-> {:namespace   (:namespace provenance)
                       :alias       (:alias provenance)
                       :doc         (:doc provenance)
                       :kind        (:kind provenance)
                       :registry-id registry-id
                       :symbols     (mapv :ext.symbol/sym (:ext/symbols ext))
                       :docs        (vec doc-names)}
                (nil? (:alias provenance)) (dissoc :alias)
                (nil? (:doc provenance)) (dissoc :doc)
                (nil? (:kind provenance)) (dissoc :kind)
                (nil? registry-id) (dissoc :registry-id)))))))

(def active-extensions-snapshot
  "Public alias for `extensions-snapshot`."
  extensions-snapshot)

(defn accessible-skills-snapshot
  "Build the value of the `TURN_ACCESSIBLE_SKILLS` SYSTEM var: a vec of
   compact skill summaries the model can `filter`/`map`/`some` over
   without paying for the full SKILL.md body.

   Per element: `{:name :description :path :source :extra}`. The `:body`
   field is INTENTIONALLY omitted — it lazy-loads via `(v/load-skill name)`,
   the canonical activation surface. Pulling every body into a SYSTEM
   var would balloon turn-start memory for a value the model rarely
   needs in full (the `<skills>` block already shows name + description).

   Reads from `vis-foundation`'s skills cache via `requiring-resolve`.
   When the foundation extension isn't on the classpath (smoke build,
   pared-down packaging) the snapshot degrades to `[]`, matching the
   \"no skills installed\" case.

   Frozen ONCE at turn start (see `iteration-loop`). The model sees the
   same vec across every iteration of the same turn."
  []
  (or (when-let [list-all (try (requiring-resolve
                                 'com.blockether.vis.ext.foundation.environment.skills/list-all)
                            (catch Throwable _ nil))]
        (try
          (->> (list-all)
            (mapv (fn [s]
                    (cond-> (select-keys s [:name :description :path :source])
                      (seq (:extra s)) (assoc :extra (:extra s))))))
          (catch Throwable t
            (tel/log! {:level :warn :id ::skills-snapshot-failed
                       :data  {:error (ex-message t)}}
              "TURN_ACCESSIBLE_SKILLS snapshot failed; defaulting to []")
            nil)))
    []))

(defn- render-extension-prompt-block
  "Render one extension's contribution to the system prompt. Honors
   ONLY `:ext/prompt`. The previous implementation also auto-rendered
   every `:ext/symbols` entry as a `- (alias/sym args) — docstring`
   line, which silently ballooned the prompt for any extension whose
   `:ext/symbols` was a thin wrapper around an upstream library: e.g.
   older `vis-language-clojure` builds dumped many upstream zipper
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
      (let [body (call-extension-callback ext extra-fn environment)]
        (when (and (string? body) (not (str/blank? body)))
          (let [{ns-sym :ns alias-sym :alias} (:ext/ns-alias ext)]
            (str "<extension namespace=\"" (:ext/namespace ext) "\""
              (when alias-sym (str " alias=\"" alias-sym "\""))
              (when ns-sym (str " target-namespace=\"" ns-sym "\""))
              ">\n"
              (when (and alias-sym ns-sym)
                (str "[namespace: " alias-sym " → " ns-sym "]\n"))
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
     `:active-extensions` — vec from `(active-extensions env)`.

   Optional opts:
     `:provider-prompt-context` — map for provider `:provider/prompt-fn`:
        `{:provider sanitized-provider-config
          :descriptor registered-provider-descriptor
          :model resolved-model
          :environment environment}`."
  [environment {:keys [system-prompt active-extensions provider-prompt-context] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-system-prompt requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [base       (prompt-block "system_prompt" (build-system-prompt {:system-prompt system-prompt}))
        env-info   (render-environment-info-block environment active-extensions)
        ext-ps     (render-extensions-prompt-block environment active-extensions)
        provider-p (when provider-prompt-context
                     (render-provider-prompt-block provider-prompt-context))
        blocks     (seq (cond-> [base]
                          env-info   (conj env-info)
                          ext-ps     (conj ext-ps)
                          provider-p (conj provider-p)))]
    (str/join "\n\n" blocks)))
