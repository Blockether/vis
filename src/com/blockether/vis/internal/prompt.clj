(ns com.blockether.vis.internal.prompt
  "Per-iteration context assembly.

   Two surfaces feed the model each iteration:

     1. System prompt - written once, cached per conversation.
        `assemble-system-prompt` joins tagged blocks: <system_prompt>,
        <environment-info>, <extensions>, <skills> (summaries only),
        and <llm_model_prompt>. Stable for the conversation
        lifetime so providers can cache it. Full skill bodies are
        not in the cached prompt; they arrive only after the model
        calls `(load-skill ...)`.

     2. Per-iteration trailer - rebuilt every iteration. Bounded
        XML-tagged blocks, each a preview not a full payload:
          <journal>        newest code + result lines that fit the
                           dynamic journal token budget.
          <bindings>      compact summaries of user-defined `(def ...)`
                           bindings live in the SCI env (shape, not
                           full value).
          <active_skills>  full SKILL.md bodies for skills the model
                           activated this turn via `(load-skill ...)`.
          <system_nudges>  zero or more `<system_nudge importance=\"...\">`
                           entries, contributed entirely by extensions
                           via `:ext/hooks`. Core owns the rendering
                           and the per-iteration ctx (model, context
                           limit, input-token estimate, title state,
                           user request); extensions own the policy
                           (title cadence, context-pressure, blind-
                           answer detector, future custom hooks).
        The current user goal arrives separately as
        `<user_turn_request_main_goal>`, not inside the trailer.

   The blocks above plus the SYSTEM vars (every name in
   `SYSTEM_VAR_NAMES` - `TURN_ID`, `TURN_POSITION`,
   `TURN_CONVERSATION_STATE_ID`, `TURN_SYSTEM_PROMPT`,
   `TURN_ACTIVE_EXTENSIONS`, `TURN_ACCESSIBLE_SKILLS`,
   `TURN_ITERATION_ID`, `TURN_ITERATION_POSITION`,
   `CONVERSATION_STATE_ID`, `CONVERSATION_TITLE`,
   `CONVERSATION_PREVIOUS_ANSWER`) bound in SCI
   cover everything the model needs. SYSTEM vars are direct sandbox
   bindings - reference them by name; they are not re-serialized
   into the trailer. The retired `TURN_USER_REQUEST` lives on as the
   sandbox `(turn-request)` fn; richer history flows through `(v/conversation-state)`.

   Context-floor contract (CTX1; see prompt-test):

     Trivial / no-tool turns produce a NIL per-iteration trailer
     (no <journal>, no <active_skills>, no <bindings>, no
     <system_nudges>). The model-facing context for a trivial turn
     is just the cached system prompt + the current
     <user_turn_request_main_goal>. Coding turns surface
     observable journal entries, active-skill bodies, bindings
     entries, and extension hook hints through the XML-tagged
     blocks above. Prior LLM-only reasoning reaches the model
     through preserved-thinking assistant messages echoed in the
     messages array (svar's canonical `:assistant-message`); the
     `<journal>` stays observed-evidence only and the iteration's
     `:thinking` column lives in the DB for forensic use.

     Preview / full boundary: <journal>, <bindings>, and tool
     results inside <journal> are bounded previews. Full values
     stay in SCI vars and persisted iteration / block rows. The
     renderer never emits a full multi-KB tool / file payload
     into context; it emits a bounded preview and the model can
     fetch the full value through foundation APIs (`v/conversation-state`,
     `v/event`, `v/cat`, or the live SCI var binding)."
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
   after protected/pinned context and <bindings> consume tokens."
  0.50)

(def ^:const BINDINGS_CONTEXT_FRACTION
  "Maximum fraction of the model context window reserved for rendered
   <bindings>. Entry count alone is not enough: 100 live string vars at
   8k chars each can consume ~200k tokens. Keep newest entries and drop
   older entries with an explicit marker."
  0.15)

(def ^:const PRESERVED_THINKING_REPLAY_FRACTION
  "Fraction of `MAX_ITERATION_CONTEXT_TOKENS` reserved for preserved-thinking
   replay across iterations within a single turn. Sized to fit ~10 newest
   iterations of typical thinking (~3k tokens each) under the 200k cap, which
   covers the soft convergence-nudge region (`CONVERGENCE_NUDGE_AT = 8`)
   comfortably while still leaving room for journal + bindings + output.

   Coupled to the cap: if `MAX_ITERATION_CONTEXT_TOKENS` is bumped, the
   replay budget tracks it (instead of being a magic 30000 that drifts out
   of policy). Sibling of `JOURNAL_CONTEXT_FRACTION` (0.50) and
   `BINDINGS_CONTEXT_FRACTION` (0.15)."
  0.15)

(def ^:const MAX_ITERATION_CONTEXT_TOKENS
  "Hard cap for Vis' model-facing working memory. UNIFORM across every
   provider: doesn't matter whether the model advertises 8k (Haiku),
   200k (Claude Sonnet), or 1M (Gemini) context - Vis treats them all
   as 200 000 tokens.

   Why a flat ceiling, not provider-derived:

   - The RLM loop degrades on huge windows: output budgets get consumed
     by reasoning/tool planning, Responses streams end as
     `max_output_tokens`, attention quality drops on long tails. The
     decode-clause / inference cost per token is also non-trivial above
     ~200k.
   - Durable evidence stays in the DB; each iteration only needs a
     compact working set, not the whole conversation.
   - Uniform behavior across providers makes budget math (journal,
     bindings, replay, headroom) one calculation, not a provider switch.
     `effective-context-limit` clamps every model to this value so a
     1M-token Gemini and a 200k-token Sonnet present the same working
     memory to the loop.
   - Smaller-window models still get their NATIVE size when it's below
     this cap (8k Haiku stays 8k); the cap only trims windows above 200k."
  200000)

(def ^:const MAX_JOURNAL_TOKENS
  "Absolute token cap for rendered <journal>. This is layered on top of
   JOURNAL_CONTEXT_FRACTION so 1M-token models cannot inherit a 500k-token
   journal window."
  24000)

(def ^:const MAX_BINDINGS_TOKENS
  "Absolute token cap for rendered <bindings>. Live vars can contain large
   tool outputs; the model sees a compact index, not the full sandbox heap."
  12000)

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

;; -----------------------------------------------------------------------------
;; Token-budget primitives (used by <journal> + nudges below).
;; -----------------------------------------------------------------------------

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
   is the smallest mainstream tier still in production use - better
   to nudge a bit early on a 200k model than to never nudge at all."
  [model]
  (or (try
        (when (string? model)
          (svar-router/context-limit model svar-router/MODEL_CONTEXT_LIMITS))
        (catch Throwable _ nil))
    32000))

;; =============================================================================
;; <journal> - newest token-budgeted code + results
;; =============================================================================

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
  (extension/journal-render-tool-result v))

(defn- format-sink-error-text
  "Render a sink failure entry's `:op/error` map via the same
   default-journal-error-text path the engine uses for whole-form
   failures, but synthesizing a tiny `:op/envelope` so the formatter
   sees the new flat shape (PLAN §2.1)."
  [entry]
  (extension/default-journal-error-text
    {:op/success? false
     :op/symbol   (when-let [f (:form entry)] (keyword f))
     :op/error    (:error entry)}))

(defn- format-sink-sub-row
  "One `iN.K.M` sub-row per :journal sink entry. Success entries surface
   the renderer's pre-rendered text; failure entries route through the
   engine's default error formatter (or the symbol's error-fn at
   write-time, if it had supplied one—its output already lives in
   :result if so, but with the spec invariant we keep one path here)."
  [iteration-position form-position {:keys [position form success? result] :as entry}]
  (let [label   (str "  i" iteration-position "." (inc form-position) "." position "  ")
        body    (if success?
                  result
                  (format-sink-error-text entry))]
    (str label form " -> " body)))

(defn- first-failed-sink-entry
  "Return the first sink-entry in `journal` whose `:success?` is false,
   or nil. Used to lift a buried tool failure into the block header so
   the next-iteration prompt cannot miss it (regression: convo
   `73f3d325` turn 5, where a `(z/patch ...)` returned
   `:success? false` but the parent block header rendered as a tidy
   `RESULT / N chars hidden` summary while the failure detail lived
   five keys deep, and the model claimed `Now fixed` in the same
   iteration)."
  [journal]
  (some (fn [entry]
          (when (false? (:success? entry)) entry))
    (or journal [])))

(defn- format-block-line
  "One iteration's single top-level form as journal text. Layout:

     iN.K     <code>   -> <form's last-expression value>
     iN.K.0   <form>   -> <result or error>     (from :journal sink)
     iN.K.1   <form>   -> <result or error>
     ...

   The iN.K row is the form's directly-returned value (truncated
   pr-str of the last-expression value, or ERROR on form throw). One
   iN.K.M sub-row per :journal sink entry surfaces every tool call the
   form made, regardless of nesting (do, let, deeply-nested calls, def
   bindings).

   When the block-level `:error` is nil but a sink-entry in `:journal`
   reports `:success? false`, the failure is LIFTED into the header
   value-part as `ERROR: <op> ...` so the model cannot mistake a
   tool-failure tool-result envelope for a successful run.

   Slow-suffix and per-form stdout/stderr suffixes still hang off the
   header row."
  [iteration-position k expr]
  (let [{:keys [code error result stdout stderr execution-time-ms journal]} expr
        block-label   (str "i" iteration-position "." (inc k))
        code-str      (truncate (str/trim (or code "")) MAX_RESULT_DISPLAY_CHARS)
        stdout-suffix (when-not (str/blank? stdout)
                        (str " :stdout " (pr-str (truncate stdout 600))))
        stderr-suffix (when-not (str/blank? stderr)
                        (str " :stderr " (pr-str (truncate stderr 600))))
        time-ms       (or execution-time-ms 0)
        slow-suffix   (when (> time-ms 5000)
                        (str " (" time-ms "ms)"))
        lifted-failure (when-not error (first-failed-sink-entry journal))
        value-part    (cond
                        error
                        (str "ERROR: " (truncate error 600))

                        lifted-failure
                        (str "ERROR: " (truncate (format-sink-error-text lifted-failure) 600))

                        (extension/tool-result? result)
                        (tool-result-journal-text result)

                        :else
                        (let [v (realize-value result)
                              [value-str truncated?] (truncated-pr-str v)]
                          (str value-str
                            (when truncated? " :truncated? true"))))
        header        (str "  " block-label "  " code-str " -> " value-part
                        (or slow-suffix "")
                        (or stdout-suffix "")
                        (or stderr-suffix ""))
        sub-rows      (when (seq journal)
                        ;; Sort by :position so racy futures (which can land
                        ;; in vec-completion order rather than source order)
                        ;; render in canonical source order.
                        (mapv #(format-sink-sub-row iteration-position k %)
                          (sort-by :position journal)))]
    (if (seq sub-rows)
      (str/join "\n" (cons header sub-rows))
      header)))

(defn- format-journal-iteration-block
  "One iteration's full `<journal>` segment: per-block labels
   include the leading `:comment` (when present) right above
   the code->value line. LLM-only iteration `:thinking` is intentionally
   excluded — prior thinking that needs to round-trip flows through
   preserved-thinking assistant messages echoed in the messages array,
   not through the journal."
  [iteration-position iteration-data]
  (let [{:keys [blocks answer]} iteration-data
        visible-blocks (vec (or blocks []))
        block-lines (vec (mapcat (fn [[k blk]]
                                   (let [comment-text (some-> (:comment blk) str/trim)
                                         ;; `:comment` is captured by `split-top-level-forms`
                                         ;; as the verbatim source slice between forms
                                         ;; (already includes `;;` prefixes / `#_(...)`
                                         ;; discards). Render as-is; DO NOT prepend
                                         ;; another `;; ` or we get `;; ;;` doubling
                                         ;; (conversation d2763464 regression).
                                         comment-line (when (and comment-text
                                                              (not (str/blank? comment-text)))
                                                        (str "  i" iteration-position "." (inc k)
                                                          "  "
                                                          (truncate comment-text 400)))]
                                     (cond-> []
                                       comment-line (conj comment-line)
                                       :always      (conj (format-block-line iteration-position
                                                            k blk)))))
                           (map-indexed vector visible-blocks)))
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
   then may be reduced by protected/pinned context and <bindings> usage.

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
   not rendered in `<journal>` — the model sees its prior reasoning via
   preserved-thinking assistant messages echoed in the wire messages
   array, and the iteration's `:thinking` column lives in the DB for
   forensic use. Each iteration's segment carries:
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
;; <bindings> - read/cache the current SCI sandbox shape
;; =============================================================================

(defn read-bindings-str
  "Lazily build (and cache) the <bindings> body for the active env.
   Returns nil when the env has no SCI context (test fixtures)."
  [environment]
  (when-let [sci-ctx (:sci-ctx environment)]
    (let [bindings-atom (or (:bindings-atom environment)
                          (atom {:index nil :revision -1 :current-revision 0}))
          {:keys [index revision current-revision]} @bindings-atom]
      (if (= revision current-revision)
        index
        (let [sandbox-map (get-in @(:env sci-ctx) [:namespaces 'sandbox])
              idx         (env/build-bindings
                            sci-ctx (:initial-ns-keys environment)
                            sandbox-map
                            (:db-info environment) (:conversation-id environment)
                            nil)]
          (swap! bindings-atom assoc :index idx :revision current-revision)
          idx)))))

(defn- bindings-token-budget
  [context-limit remaining-budget]
  (let [fraction-budget (long (Math/floor (* BINDINGS_CONTEXT_FRACTION
                                            (double context-limit))))
        capped-budget   (min fraction-budget (long MAX_BINDINGS_TOKENS))]
    (max 1 (min capped-budget (max 1 (long (or remaining-budget capped-budget)))))))

(defn- bindings-entry-start? [line]
  (str/starts-with? (str line) ";; v="))

(defn- split-bindings-entries
  [bindings-str]
  (let [lines (str/split-lines (or bindings-str ""))]
    (loop [remaining lines
           current   []
           entries   []]
      (if-let [line (first remaining)]
        (if (and (bindings-entry-start? line) (seq current))
          (recur (rest remaining) [line] (conj entries (str/join "\n" current)))
          (recur (rest remaining) (conj current line) entries))
        (cond-> entries
          (seq current) (conj (str/join "\n" current)))))))

(defn- trim-bindings-str
  [model bindings-str token-budget]
  (when (and (string? bindings-str) (not (str/blank? bindings-str)))
    (let [entries (split-bindings-entries bindings-str)
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
                              " older <bindings> entries omitted to fit bindings token budget "
                              used "/" budget " tokens ("
                              (long (* 100 BINDINGS_CONTEXT_FRACTION))
                              "% of model context max)")]
                (str/join "\n" (conj kept marker)))
              (recur (rest remaining) (conj kept entry) next-used)))
          (str/join "\n" kept))))))

;; =============================================================================
;; Iteration context - provider context block inserted before current user goal
;; =============================================================================

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
                     reduced by protected/pinned context and <bindings>.
                     It never renders LLM-only iteration `:thinking`;
                     prior reasoning reaches the model through the
                     preserved-thinking assistant messages echoed in
                     the wire messages array.
     <bindings>   - `(def ...)` bindings in the SCI env.

   Plus zero or more tagged `<system_nudge importance=\"...\">` entries
   wrapped in `<system_nudges>`. All entries come from `:ext/hooks`
   on active extensions; core owns no built-in hook policy. Each hook
   declares its lifecycle `:phase` (`:session/start`, `:turn/start`,
   `:turn.iteration/start`) and a `:fn` that receives the `nudge-ctx`
   (the per-iteration ctx below) and returns
   either nil (silent) or `{:hint :importance?}`.

   `nudge-ctx` fields (passed to every hook `:fn`):
     :environment        full environment map
     :iteration          1-based current iteration position
     :previous-blocks    last iteration's executed blocks (or nil)
     :model              resolved model map
     :context-limit      effective context window for the model
     :input-tokens       estimated tokens of the assembled prompt-so-far
     :title-refresh?     turn-boundary refresh hint
     :conversation-title current title (or nil/blank)
     :user-request       raw current-turn user request

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Computed once
        per turn; threaded through every iteration. Each extension's
        :ext/hooks vector is consulted (phase-filtered to the current
        iteration / turn / session position).

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
        bindings-str (read-bindings-str environment)
        bindings-budget (bindings-token-budget ctx-limit budget-after-pinned)
        bindings-str* (trim-bindings-str model bindings-str bindings-budget)
        bindings-block (when (and (string? bindings-str*)
                               (not (str/blank? bindings-str*)))
                         (str "<bindings>\n" bindings-str* "\n</bindings>"))
        bindings-tokens (or (count-prompt-tokens model (or bindings-block "")) 0)
        journal-budget (max 1 (min (journal-token-budget model ctx-limit)
                                (- budget-after-pinned (long bindings-tokens))))
        recent-block (format-journal-block model blocks-by-iteration journal-budget)
        last-iteration-blocks (some-> blocks-by-iteration last second)
        ;; Token-budget probe. Extensions (e.g. vis-foundation's
        ;; context-pressure nudge) read `:input-tokens` from the ctx
        ;; and compare against `:context-limit` to decide whether to
        ;; nudge the model toward convergence. Core just measures and
        ;; passes the value; policy lives in extensions.
        prompt-text (str/join "\n\n"
                      (keep identity
                        [system-prompt current-user-content active-skills-block recent-block bindings-block]))
        input-tokens (or (count-prompt-tokens model prompt-text) 0)
        conversation-title (some-> (:conversation-title-atom environment) deref str str/trim not-empty)
        nudge-ctx {:environment environment
                   :iteration (if (some? iteration) (inc (long iteration)) 1)
                   :previous-blocks last-iteration-blocks
                   :model model
                   :context-limit ctx-limit
                   :input-tokens input-tokens
                   :title-refresh? (boolean title-refresh?)
                   :conversation-title conversation-title
                   ;; Raw current-turn user request — hooks inspect it
                   ;; for investigation verbs / blind-answer prevention.
                   :user-request current-user-content}
        ;; <system_nudges> are populated by PRE-phase `:ext/hooks` — the
        ;; single mechanism for model-facing advisory hints. Each hook
        ;; declares :phase; the host invokes :fn only when the phase
        ;; matches the current lifecycle position. Pre-phase fns return
        ;; nil (silent) or `{:hint :importance?}`.
        ;;
        ;; Post-phase hooks (:turn.iteration/stop, :turn/stop) are NOT invoked
        ;; here; they fire from internal/loop.clj after eval completes.
        iter-pos     (long (or iteration 0))
        first-iter?  (zero? iter-pos)
        turn-pos     (long (or (some-> environment :current-turn-position-atom deref) 1))
        first-turn?  (= 1 turn-pos)
        phase-active? (fn [phase]
                        (case phase
                          :turn.iteration/start true
                          :turn/start           first-iter?
                          :session/start        (and first-iter? first-turn?)
                          false))
        all-nudges (into []
                     (mapcat
                       (fn [ext]
                         (for [{:keys [id phase fn]} (or (:ext/hooks ext) [])
                               :when (phase-active? phase)
                               :let [hit (try (call-extension-callback ext fn
                                                (assoc nudge-ctx :phase phase))
                                           (catch Throwable t
                                             (tel/log! {:level :warn
                                                        :id ::hook-threw
                                                        :data {:ext (:ext/namespace ext)
                                                               :hook id
                                                               :phase phase
                                                               :error (ex-message t)}})
                                             nil))]
                               :when (and (map? hit) (string? (:hint hit)) (not (str/blank? (:hint hit))))]
                           (normalize-system-nudge
                             (or (:importance hit) :normal)
                             (:hint hit)))))
                     (or active-extensions []))
        nudges-block (system-nudges-block all-nudges)
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [active-skills-block recent-block bindings-block nudges-block])]
    (when (seq parts)
      (str/join "\n" parts))))

;; =============================================================================
;; Initial messages
;; =============================================================================

(defn previous-turn-context-block
  "Full previous exchange context for follow-up turns.

   Vis deliberately does not replay the whole chat transcript; prior work
   flows through <journal>/<bindings>. But one-turn follow-ups like `A`,
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
   <bindings>, and DB-backed tools. The current user request is tagged as
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

(def CORE_SYSTEM_PROMPT
  "You are Vis. RLM in sandboxed SCI.

OUTPUT:
  Reply = one or more ```clojure fences. Nothing outside.
  Narrate inside fences with ;; comments.

LOOP:
  Two reply shapes, never mixed:
    (a) trivial chat — single iteration with one `(answer ...)` form.
        Only legitimate for: greetings ('hi', 'thx'), acks, social.
    (b) work — observe (tool calls) -> read <journal> next iteration
        -> observe more OR `(answer ...)` ALONE in its own iteration.
  Default shape: (b). Reach for (a) only when the user request is
  unambiguously trivial. Bias: investigate > ask.

  Investigation triggers (verbs that REQUIRE tool calls before answering):
    why, how, what, where, which, fix, check, find, look, inspect,
    investigate, reproduce, diagnose, verify, trace, debug, show me,
    search for, grep, locate, count, explain. Do not count substrings
    inside tool/symbol names like `z/patch-check`. Explicit
    planning-only / opinion-only / design-only requests may answer
    directly when the user asks for judgment, not runtime/file facts.

  HALLUCINATION GUARD: answering an investigation request from memory,
  without observing the actual file / runtime / journal, is a hard
  failure. If you don't know, call a tool. If a tool fails, read its
  `:op/error :hint` and retry. If genuinely ambiguous (truly no
  observation can resolve), ask exactly ONE clarifying question —
  always offer a default answer the user can accept silently.

  Bug work: reproduce first. ¬ repro -> ¬ diagnosis -> ¬ fix.

ITERATION (⊢ :ooda only):
  emit forms -> engine evals -> <journal> populated -> <bindings> updated
   -> observe -> emit more ∨ (answer ...)
  Only way to learn a value: evaluate it. Never narrate results.
  Engine populates one row per top-level form + sub-row per tool call.

ANSWER (HARD RULE, preflight-enforced):
  (answer ...) is its OWN iteration. ONE top-level form, NOTHING else.
  No `(def ...)`, no `(v/...)`, no `(z/...)`, no `(conversation-title ...)`,
  no `(do ...)` wrapper hiding tool calls. Just `(answer [:ir ...])`.
  Workflow:
    iteration N      -> emit observation/action forms; engine evals
    iteration N+1    -> read <journal>; emit MORE work OR `(answer ...)` alone
    iteration N+2    -> ONLY `(answer [:ir ...])`; turn closes.
  Violation -> engine rejects iteration before any eval and forces
  you to loop. The journal will show the preflight error; read it,
  drop the answer, finish the work, then answer alone next round.

JOURNAL:
  Engine writes; you read. Carries code, result preview, ::op/tag,
  ::op/success?, ::op/error if any.

BINDINGS:
  Your defs in the SCI sandbox. Compact shape per entry.
  Reach values later by name: (def x (v/cat \"f\")) -> (get-in x ...)
  Escape hatches: `*1` `*2` `*3` (last 3 values), `*e` (last throw).
  Turn-scoped. Prefer durable names.

SYSTEM VARS:
  Engine-managed. UPPERCASE names. You read; never set.
  Prefix = hierarchy (where the concept lives in the tree).
    CONVERSATION_*    conversation-level (STATE_ID, TITLE, PREVIOUS_ANSWER)
    TURN_*            turn-level (ID, POSITION, CONVERSATION_STATE_ID,
                                  SYSTEM_PROMPT, ACTIVE_EXTENSIONS,
                                  ACCESSIBLE_SKILLS)
    TURN_ITERATION_*  iteration-level (ID, POSITION)
  Full registry: 11 names, see internal/env.clj SYSTEM_VAR_NAMES.
  Branch identity = STATE_ID (live branch); raw SOUL_IDs retired.

OPS:
  Every op carries ::op/tag in #{:op.tag/observation :op.tag/action}.
    observation: cat ls glob rg locators verify.sh patch-check ...
    action:      patch write append delete move bash(mutating) ...
  Verification = observation with ::op/success?. Read it.
  Errors carry structured ::op/error: :message :hint :trace :block.
  Read :hint first; act on it.

CODE (when :ooda or :architect is sketching):
  code > markdown
  data > control_flow
  pure > stateful
  structural_editing > line_editing > raw_text
  one change -> verify -> next

TRUTH:
  runtime > source > docs > assumption.
")

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

(defn- environment-info-block
  "Collect `<environment-info>` from every active extension that declares
   `:ext/environment-info-fn`. Each fn receives the live environment and
   returns a string or nil. Non-blank results are joined and wrapped in
   `<environment-info>...</environment-info>`."
  [environment active-extensions]
  (let [fragments (keep (fn [ext]
                          (when-let [f (:ext/environment-info-fn ext)]
                            (try
                              (let [result (call-extension-callback ext f environment)]
                                (when (and (string? result) (not (str/blank? result)))
                                  result))
                              (catch Throwable t
                                (tel/log! {:level :warn
                                           :id ::environment-info-error
                                           :data {:ext (:ext/namespace ext)
                                                  :error (ex-message t)}}
                                  "Extension environment-info-fn threw")
                                nil))))
                    active-extensions)]
    (when (seq fragments)
      (prompt-block "environment-info" (str/join "\n\n" fragments)))))

(defn- extensions-prompt-block
  "Collect `<extensions>` from every active extension that declares
   `:ext/prompt`. Each prompt is `(fn [env] -> string)` (normalized at
   registration). Non-blank results are joined and wrapped in
   `<extensions>...</extensions>`."
  [environment active-extensions]
  (let [fragments (keep (fn [ext]
                          (when-let [f (:ext/prompt ext)]
                            (try
                              (let [result (call-extension-callback ext f environment)]
                                (when (and (string? result) (not (str/blank? result)))
                                  result))
                              (catch Throwable t
                                (tel/log! {:level :warn
                                           :id ::extension-prompt-error
                                           :data {:ext (:ext/namespace ext)
                                                  :error (ex-message t)}}
                                  "Extension :ext/prompt fn threw")
                                nil))))
                    active-extensions)]
    (when (seq fragments)
      (prompt-block "extensions" (str/join "\n\n" fragments)))))

(defn- skills-summary-block
  "Render `<skills>` with name + description for every accessible skill.
   Full bodies are NOT included; they arrive only after the model calls
   `(load-skill ...)`."
  []
  (let [all-skills (skills/list-all)]
    (when (seq all-skills)
      (prompt-block "skills"
        (str/join "\n"
          (map (fn [{:keys [name description]}]
                 (str "- " name
                   (when (and description (not (str/blank? description)))
                     (str ": " description))))
            (sort-by :name all-skills)))))))

(defn- provider-prompt-block
  "Render `<llm_model_prompt>` by calling the active provider's
   `:provider/prompt-fn` with the prompt context built by the loop
   layer. Returns nil when no provider hook is configured or when
   it returns blank."
  [provider-prompt-context]
  (when-let [{:keys [descriptor] :as ctx} provider-prompt-context]
    (when-let [f (:provider/prompt-fn descriptor)]
      (try
        (let [result (f ctx)]
          (when (and (string? result) (not (str/blank? result)))
            (prompt-block "llm_model_prompt" result)))
        (catch Throwable t
          (tel/log! {:level :warn
                     :id ::provider-prompt-error
                     :data {:provider (:id (:provider ctx))
                            :error (ex-message t)}}
            "Provider prompt-fn threw")
          nil)))))

(defn assemble-system-prompt
  "Full system prompt assembly.

   Joins tagged blocks:
     `<system_prompt>`                    - core RLM instructions + caller addendum
     `<environment-info>`                 - host/git/project facts from extensions
     `<extensions>`                       - extension `:ext/prompt` fragments
     `<skills>`                           - skill name + description summaries
     `<llm_model_prompt>`                 - provider/model-specific prompt hook

   Stable for the conversation lifetime so providers can cache it.
   Full skill bodies are not in the cached prompt; they arrive only
   after the model calls `(load-skill ...)`.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Drives
        environment-info, extension prompt, and nudge collection.

   Optional opts:
     `:system-prompt`            - caller addendum appended to CORE.
     `:provider-prompt-context`  - provider hook context."
  [environment {:keys [system-prompt active-extensions provider-prompt-context] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-system-prompt requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [core-block    (prompt-block "system_prompt" (build-system-prompt {:system-prompt system-prompt}))
        env-block     (environment-info-block environment active-extensions)
        ext-block     (extensions-prompt-block environment active-extensions)
        skills-block  (skills-summary-block)
        prov-block    (provider-prompt-block provider-prompt-context)]
    (str/join "\n\n"
      (keep identity [core-block env-block ext-block skills-block prov-block]))))