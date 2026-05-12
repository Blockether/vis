(ns com.blockether.vis.internal.prompt
  "Prompt assembly.

   Stable provider prefix is separate system-role messages: core RLM rules,
   then environment, extension, skill-summary, and provider fragments. Initial
   user-role content carries the current user goal plus optional previous-turn
   context. Per-iteration user-role context carries dynamic engine telemetry
   (<current_turn_context>), fresh evidence (<journal>), live bindings,
   activated skill bodies, and current engine nudges.

   Dynamic turn / iteration ids never enter the cached stable prefix. Static
   lifecycle and policy stay in the system prompt; `<current_turn_context>`
   reports only live ids, positions, and state labels."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.format :as fmt]
   [com.blockether.vis.internal.skills :as skills]
   [sci.core :as sci]
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
  "Render a sink failure entry's `:error` map via the same
   default-journal-error-text path the engine uses for whole-form
   failures, but synthesizing a tiny `:envelope` so the formatter
   sees the new flat shape (PLAN §2.1)."
  [entry]
  (extension/default-journal-error-text
    {:success? false
     :symbol   (when-let [f (:form entry)] (keyword f))
     :error    (:error entry)}))

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

     iN.K     <top-level code> -> <top-level return preview or ERROR>
     iN.K.M   <sink form>      -> <tool/journal result preview or ERROR>

   N = iteration position, K = 1-based top-level form number, M = the
   :journal sink position emitted while evaluating that top-level form.
   There are as many iN.K.M sub-rows as tool/journal sink entries; zero
   sub-rows means the form did not emit a sink entry.

   Important boundary: iN.K is NOT a complete trace of every nested form.
   It is only the direct return value of the top-level form (truncated
   pr-str of the last-expression value, or ERROR on form throw). iN.K.M
   sub-rows are the observable tool/journal events produced inside that
   form, regardless of nesting (do, let, deeply-nested calls, def bindings).
   The model must read the sub-rows to see actual tool successes/errors.

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

(do
  (defn- error-message-text
    [error]
    (cond
      (map? error) (or (:message error) (str error))
      (some? error) (str error)
      :else nil))

  (defn- answer-alone-preflight-message
    [block]
    (let [message (error-message-text (:error block))]
      (when (and message
              (str/includes? message "Answer-alone preflight rejected"))
        message)))

  (defn- collapse-answer-alone-preflight-blocks
    "Collapse legacy persisted answer-alone preflight rows.

     Older loop builds attached the same iteration-level answer-alone
     preflight error to every parsed top-level form, so `<journal>` showed
     fake rows like `(def a 1) -> ERROR` even though no form evaluated.
     Render those legacy batches as one synthetic guard row, matching the
     current persistence shape."
    [blocks]
    (let [blocks   (if (vector? blocks) blocks (vec (or blocks [])))
          messages (mapv answer-alone-preflight-message blocks)]
      (if (and (< 1 (count blocks))
            (every? some? messages)
            (= 1 (count (distinct messages))))
        [(assoc (first blocks)
           :code "(vis/preflight-error :answer-alone)"
           :error (first messages)
           :result nil
           :journal nil
           :stdout ""
           :stderr "")]
        blocks)))

  (defn- format-journal-iteration-block
    "One iteration's full `<journal>` segment: per-block labels
     include the leading `:comment` (when present) right above
     the code->value line. LLM-only iteration `:thinking` is intentionally
     excluded — prior thinking that needs to round-trip flows through
     preserved-thinking assistant messages echoed in the messages array,
     not through the journal."
    [iteration-position iteration-data]
    (let [{:keys [blocks answer]} iteration-data
          visible-blocks (collapse-answer-alone-preflight-blocks blocks)
          block-lines (persistent!
                        (reduce-kv
                          (fn [acc k blk]
                            (let [comment-text (some-> (:comment blk) str str/trim not-empty)
                                  ;; `:comment` is captured by `split-top-level-forms`
                                  ;; as the verbatim source slice between forms
                                  ;; (already includes `;;` prefixes / `#_(...)`
                                  ;; discards). Render as-is; DO NOT prepend
                                  ;; another `;; ` or we get `;; ;;` doubling
                                  ;; (conversation d2763464 regression).
                                  acc (if comment-text
                                        (conj! acc (str "  i" iteration-position "." (inc k)
                                                     "  "
                                                     (truncate comment-text 400)))
                                        acc)]
                              (conj! acc (format-block-line iteration-position k blk))))
                          (transient [])
                          visible-blocks))
          answer-text (some-> answer str str/trim not-empty)
          answer-line (when answer-text
                        (str "  i" iteration-position ".answer  <final-answer> -> "
                          (pr-str (truncate answer-text MAX_RESULT_DISPLAY_CHARS))))]
      (cond-> block-lines
        answer-line (conj answer-line)))))

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

(defn- short-ref
  [v]
  (let [s (str v)]
    (if (> (count s) 8) (subs s 0 8) s)))

(defn- current-engine-iteration-id
  [turn-id iteration-position]
  (str "turn/" (short-ref (or turn-id "unknown")) "/iteration/" iteration-position))

(defn- sandbox-value
  [environment sym default]
  (try
    (if-let [sci-ctx (:sci-ctx environment)]
      (if-let [ns-obj (sci/find-ns sci-ctx 'sandbox)]
        (:val (sci/eval-string+ sci-ctx (str sym) {:ns ns-obj}))
        default)
      default)
    (catch Throwable _
      default)))

(defn- context-value-str
  [v]
  (safe-pr-str v {:max-chars 2000
                  :print-length 32
                  :print-level 5}))

(defn- active-extensions-context-value
  [active-extensions]
  (mapv (fn [ext]
          (cond-> {:namespace (:ext/namespace ext)
                   :symbols   (mapv :ext.symbol/sym (:ext/symbols ext))}
            (get-in ext [:ext/ns-alias :alias])
            (assoc :alias (get-in ext [:ext/ns-alias :alias]))
            (:ext/doc ext)
            (assoc :doc (:ext/doc ext))
            (:ext/kind ext)
            (assoc :kind (:ext/kind ext))))
    (or active-extensions [])))

(defn- accessible-skills-context-value
  []
  (try
    (mapv (fn [s]
            (cond-> (select-keys s [:name :description :path :source])
              (seq (:extra s)) (assoc :extra (:extra s))))
      (skills/list-all))
    (catch Throwable _
      [])))

(defn- current-turn-context-block
  "Render dynamic engine telemetry into the user-role iteration trailer.

   These are live state facts, not policy. They stay out of the cached system
   prompt. Static lifecycle shape lives in `CORE_SYSTEM_PROMPT` / λENGINE;
   this block only reports the current turn and iteration coordinates.
   `current_engine_iteration_id` is a logical engine id for the in-flight
   iteration; the DB UUID appears only after persistence, so the previous
   persisted DB id is exposed separately."
  [environment {:keys [iteration max-iterations engine-state engine-phase
                       active-extensions system-prompt]}]
  (let [iteration-position  (inc (long (or iteration 0)))
        previous-position   (max 0 (dec iteration-position))
        turn-id             (or (some-> (:current-conversation-turn-id-atom environment) deref)
                              (sandbox-value environment 'TURN_ID nil))
        turn-position       (or (some-> (:current-turn-position-atom environment) deref)
                              (sandbox-value environment 'TURN_POSITION nil))
        previous-iter-id    (or (some-> (:current-iteration-id-atom environment) deref)
                              (sandbox-value environment 'TURN_ITERATION_ID nil))
        conversation-state-id (or (sandbox-value environment 'CONVERSATION_STATE_ID nil)
                                (sandbox-value environment 'TURN_CONVERSATION_STATE_ID nil))
        conversation-title  (or (some-> (:conversation-title-atom environment) deref)
                              (sandbox-value environment 'CONVERSATION_TITLE ""))
        previous-answer     (sandbox-value environment 'CONVERSATION_PREVIOUS_ANSWER "")
        turn-system-prompt  (or system-prompt
                              (sandbox-value environment 'TURN_SYSTEM_PROMPT ""))]
    (prompt-block
      "current_turn_context"
      (str "engine_state: " (or engine-state "turn.iteration/start") "\n"
        "engine_phase: " (or engine-phase "model_think") "\n"
        "conversation_id: " (:conversation-id environment) "\n"
        "engine_turn_id: " turn-id "\n"
        "engine_turn_position: " turn-position "\n"
        "current_engine_iteration_id: " (current-engine-iteration-id turn-id iteration-position) "\n"
        "engine_iteration_position: " iteration-position "\n"
        "engine_iteration_max: " (or max-iterations "unknown") "\n"
        "previous_persisted_iteration_id: " previous-iter-id "\n"
        "previous_persisted_iteration_position: " previous-position "\n"
        "turn_id: " (context-value-str (or turn-id "")) "\n"
        "turn_position: " (context-value-str (or turn-position 0)) "\n"
        "turn_conversation_state_id: " (context-value-str (or conversation-state-id "")) "\n"
        "turn_system_prompt: " (context-value-str (or turn-system-prompt "")) "\n"
        "turn_active_extensions: " (context-value-str (active-extensions-context-value active-extensions)) "\n"
        "turn_accessible_skills: " (context-value-str (accessible-skills-context-value)) "\n"
        "turn_iteration_id: " (context-value-str (or previous-iter-id "")) "\n"
        "turn_iteration_position: " (context-value-str previous-position) "\n"
        "conversation_state_id: " (context-value-str (or conversation-state-id "")) "\n"
        "conversation_title: " (context-value-str (or conversation-title "")) "\n"
        "conversation_previous_answer: " (context-value-str (or previous-answer "")) "\n"
        "prompt_role: user"))))

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

(defn- format-current-engine-nudge
  [{:keys [importance text]}]
  (str "<current_engine_start_nudge importance=\"" importance "\">\n"
    (attr-str text)
    "\n</current_engine_start_nudge>"))

(defn- current-engine-nudges-block
  [nudges]
  (when-let [nudges (seq (keep identity nudges))]
    (str "<current_engine_start_nudges>\n"
      (str/join "\n" (map format-current-engine-nudge nudges))
      "\n</current_engine_start_nudges>")))

(defn- call-extension-callback
  [ext f & args]
  (binding [extension/*current-extension* ext
            extension/*current-symbol* nil]
    (apply f args)))

(defn build-iteration-context
  "Assemble the per-iteration user-role trailer appended to each model call.

   Slots:
     <current_turn_context>
                   - dynamic engine telemetry for this in-flight iteration.
                     This is user-role runtime state, not cached system text.
     <journal>     - newest token-budgeted comments + code + result.
                     May include carried prior conversation iterations;
                     budget is capped at 50% of the model context, then
                     reduced by protected/pinned context and <bindings>.
                     It never renders LLM-only iteration `:thinking`;
                     prior reasoning reaches the model through the
                     preserved-thinking assistant messages echoed in
                     the wire messages array.
     <bindings>   - `(def ...)` user vars in the SCI env; SYSTEM vars and
                    initial tool/helper bindings are excluded.

   Plus zero or more tagged `<current_engine_start_nudge importance=\"...\">`
   entries wrapped in `<current_engine_start_nudges>`. All entries come from
   `:ext/hooks` on active extensions; core owns no built-in hook policy.
   Each hook declares its lifecycle `:phase` (`:session/start`,
   `:turn/start`, `:turn.iteration/start`) and a `:fn` that receives the
   `nudge-ctx` (the per-iteration ctx below) and returns either nil
   (silent) or `{:hint :importance?}`.

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
                       model stable-prompt-content current-user-content context-limit
                       title-refresh? max-iterations]
                :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [ctx-limit (effective-context-limit model context-limit)
        current-context-block (current-turn-context-block environment
                                {:iteration iteration
                                 :max-iterations max-iterations
                                 :engine-state :turn.iteration/start
                                 :engine-phase :model_think
                                 :active-extensions active-extensions
                                 :system-prompt stable-prompt-content})
        active-skills-block (active-skills-block environment)
        pinned-text (str/join "\n\n" (keep identity [stable-prompt-content current-user-content current-context-block active-skills-block]))
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
                        [stable-prompt-content current-user-content current-context-block active-skills-block recent-block bindings-block]))
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
        ;; <current_engine_start_nudges> are populated by PRE-phase `:ext/hooks` —
        ;; the single mechanism for model-facing advisory hints. Each hook
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
        nudges-block (current-engine-nudges-block all-nudges)
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [current-context-block active-skills-block recent-block bindings-block nudges-block])]
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
   dialog transcript: Vis state flows through <current_turn_context>,
   <journal>, <bindings>, and DB-backed tools. The current user request is tagged as
   <user_turn_request_main_goal>.

   One full previous-turn context block may be prepended so short follow-ups
   can inspect the prior exchange without replaying the whole conversation."
  [{:keys [stable-prompt-messages initial-user-content previous-turn-context]}]
  (let [previous-block (previous-turn-context-block previous-turn-context)
        user-block     (when initial-user-content
                         (prompt-block "user_turn_request_main_goal" initial-user-content))]
    (vec
      (concat
        (or stable-prompt-messages [])
        (when user-block
          [{:role "user" :content (str/join "\n\n" (keep identity [previous-block user-block]))}])))))

;; =============================================================================
;; System prompt
;; =============================================================================

(def ^:private CORE_SYSTEM_PROMPT
  "λVis.
You are Vis ≜ SCI-based Small Clojure Interpreter Recursive Model.
You emit Clojure forms into a live SCI sandbox. ENGINE evaluates them, records evidence, and asks again until final `(turn-answer! ...)`.
Sandbox surface := SCI Clojure core + active EXTENSIONS + host primitives.
   
All `clojure.core` vars are interned, for the other namespaces the following are aliases are available:
  - `clojure.walk` as `walk`
  - `clojure.string` as `str`
  - `clojure.set` as `set`
  - `clojure.pprint` as `pp`
  - `clojure.edn` as `edn`
  - `clojure.spec.alpha` as `s`
  - `clojure.repl` as `repl`
   
λOUTPUT.
  ∀ model replies: emit only ```clojure code fences```.
  fence_body := SCI forms
  narration := `;;` comments inside fences
  user_visible_final := FINAL_CALL := `(turn-answer! [:ir ...])`
  FINAL_CALL accepted ⇒ TURN ends
  never final raw Markdown/text.

λENGINE.
  machine := idle → receive_user_turn → build_turn_context → model_think
             → maybe_tool_call → observe_tool_result → decide_next
             → finalize_answer → persist_turn → idle

  TURN := USER_GOAL × WORK(ITERATIONS)* × FINAL

  WORK (ITERATIONS) :=
    MODEL_REPLY_IN_CODE_FENCES{form*}
    → ENGINE_SCI_EVAL(form*)
    → NEW_VARS(def|defn) → <bindings>
    → {result|error, stdout, stderr, journal sink entries} → <journal>
    → DECIDE_NEXT: READY? or WORK MORE

  READY? :=
    required_evidence_observed?
    ∧ unresolved_blockers = Ø
    ∧ user_goal_satisfiable_from(<journal>, <bindings>)
    ∧ no errors in the last iteration journal entry

  if ¬READY? → WORK MORE = ITERATE -> MODEL_REPLY_IN_CODE_FENCES + ENGINE_SCI_EVAL + OBSERVE THE RESULTS IN THE NEXT ITERATION'S <journal> + <bindings> => DECIDE_NEXT
  if READY?  → EMIT FINAL

  FINAL :=
    allowed top-level forms:
      optional `(set-conversation-title \"...\")`
      exactly one `(turn-answer! [:ir ...])`

  FINAL forbids:
    defs, tools, mutation, reloads, skill loads, extra work.

λSTATIC_CONTEXT.
  system-role stable prefix, built once at turn start; provider may cache it.
  <system_prompt>      := core RLM rules + caller addendum.
  <environment_info>   := turn-start host/project facts from active extension callbacks.
  <extensions>         := turn-start active extension prompt fragments.
  <skills>             := accessible skill summaries (names/descriptions only).
  <llm_model_prompt>   := provider/model-specific prompt fragment.

λDYNAMIC_CONTEXT.
  user-role turn/iteration context; initial user message plus per-iteration trailer.
  <user_turn_request_main_goal> := current user goal; initial user message.
  <previous_turn_context>       := optional immediately previous exchange only; initial user message.
  <current_turn_context>        := per-iteration engine ids/positions/state + direct turn/conversation values; no user intent; no policy; no named runtime-var indirection.
  <journal>                     := token-budgeted iteration evidence; newest at bottom; may carry prior conversation iterations.
  <bindings>                    := live SCI user-var index; excludes SYSTEM vars and tool/helper bindings.
  <active_skills>               := loaded skill bodies; appears after `(load-skill! ...)`.
  <current_engine_start_nudges> := extension hook runtime hints for this iteration.

λHOST.
  (turn-answer! ir)                 ; finalize turn, ir must be [:ir ...]
  (set-conversation-title s)      ; set conversation title
  (load-skill! name)          ; load skill body for next iteration, never FINAL
  (reload-skills!)            ; refresh skill cache, never FINAL
  (skills)                    ; list accessible skill summaries
  (var-history ...)           ; inspect persisted vars
  (var-history-timeline ...)  ; inspect var timeline

λREPL_RECOVERY.
  *1 *2 *3 *e := last values/errors for sandbox recovery; ordinary prompt context uses rendered values, not named runtime-var indirection.

λDISCIPLINE.
  separate_observe_code_blocks from mutation e.g. reads then (turn-answer! ...)/mutations like write file/patches in the same iteration are BANNED
  never_guess_when_asked_about_code -> emit observation code blocks if the data is not in <bindings> or <journal>; do not guess or fabricate code to fill in gaps in the observed evidence. 
  
λANSWER_IR.
  Build IR directly; do not render Markdown into IR.
  root   := [:ir block*]
  block  := :p | :h{:level 1-6} | :code{:lang} | :ul | :ol{:start} | :li
          | :quote | :table | :tr | :th | :td
  inline := :span{:preserve-ws? :nowrap?} | :br | :strong | :em | :c
          | :a{:href} | :img{:src :alt} | :kbd | :mark | :sup | :sub
  forbidden := :details | :summary | HTML <details>/<summary> | collapsible answer widgets
  text   := string shorthand
  preserve_ws := :code | :c | :kbd

λMIN_EXAMPLES.
  ITERATION 1 - Work iteration, observe only:
    ```clojure
    ;; Need evidence first. No answer in this iteration.
    (def src (v/cat \"src/foo.clj\"))
    ```

  ITERATION 2 - Work iteration, act + verify later:
    ```clojure
    ;; Mutate now; verify after journal records result.
    (v/patch [{:path \"src/foo.clj\"
               :search \"old\"
               :replace \"new\"}])
    ```

  ITERATION 3 - Final iteration:
    ```clojure
    (turn-answer! [:ir [:p \"Done.\"]])
    ```")

(defn build-system-prompt
  "Core system prompt: CORE_SYSTEM_PROMPT plus optional caller addendum."
  [{:keys [system-prompt]}]
  (str CORE_SYSTEM_PROMPT
    (when (and (string? system-prompt) (not (str/blank? system-prompt)))
      (str "\n\n" system-prompt))))

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
   field is INTENTIONALLY omitted - it lazy-loads via `(load-skill! name)`,
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
  "Collect `<environment_info>` from every active extension that declares
   `:ext/environment-info-fn`. Each fn receives the live environment and
   returns a string or nil. Non-blank results are joined and wrapped in
   `<environment_info>...</environment_info>`."
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
      (prompt-block "environment_info" (str/join "\n\n" fragments)))))

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
   `(load-skill! ...)`."
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

(defn- stable-prompt-message
  [content]
  (when (and (string? content) (not (str/blank? content)))
    {:role "system" :content content}))

(defn stable-prompt-text
  "Join stable prompt message contents for token budgeting and debug bindings only.
   Provider sends the original message vector; this is not a send path."
  [messages]
  (str/join "\n\n" (keep :content messages)))

(defn assemble-stable-prompt-messages
  "Assemble stable provider-prefix messages.

   Each tagged block becomes its own system-role message so provider prompt
   caching can catch stable boundaries independently:
     `<system_prompt>`      - CORE_SYSTEM_PROMPT + caller addendum
     `<environment_info>`   - host/git/project facts from extensions
     `<extensions>`         - extension `:ext/prompt` fragments
     `<skills>`             - skill name + description summaries
     `<llm_model_prompt>`   - provider/model-specific prompt hook

   Full skill bodies are not in the cached prefix; they arrive only after
   the model calls `(load-skill! ...)` and are rendered in user-role
   `<active_skills>`.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Drives
        environment_info, extension prompt, and nudge collection.

   Optional opts:
     `:system-prompt`            - caller addendum appended to CORE.
     `:provider-prompt-context`  - provider hook context."
  [environment {:keys [system-prompt active-extensions provider-prompt-context] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-stable-prompt-messages requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [core-block   (prompt-block "system_prompt"
                       (build-system-prompt {:system-prompt system-prompt}))
        env-block    (environment-info-block environment active-extensions)
        ext-block    (extensions-prompt-block environment active-extensions)
        skills-block (skills-summary-block)
        prov-block   (provider-prompt-block provider-prompt-context)]
    (vec
      (keep stable-prompt-message
        [core-block env-block ext-block skills-block prov-block]))))

(defn assemble-system-prompt
  "Backward-compatible joined-text view of `assemble-stable-prompt-messages`.
   Provider send path uses the message vector; tests and diagnostics can use
   this to assert on rendered prompt text."
  [environment opts]
  (stable-prompt-text (assemble-stable-prompt-messages environment opts)))
