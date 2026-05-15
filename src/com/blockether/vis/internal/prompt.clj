(ns com.blockether.vis.internal.prompt
  "Prompt assembly.

   Provider messages are explicit blocks in send order: core system rules,
   extension fragments, then the current user message. Extension prompts stay
   OUT of CORE_SYSTEM_PROMPT; active extensions own their model-facing blocks
   (including foundation's `<environment>`) inside the extension message.

   Per-iteration user-role context carries tiny live coordinates
   (<current_turn_context>), fresh evidence (<journal>), live bindings,
   and iteration hints. `<current_turn_context>` intentionally excludes
   system prompt text, extension inventory, engine internals, and prior
   answers."
  (:require
   [clojure.string :as str]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.format :as fmt]
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
   iterations of typical thinking (~3k tokens each) under the 200k cap while
   still leaving room for journal + bindings + output.

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
   `73f3d325` turn 5, where a `(v/patch ...)` returned
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

(defn- format-journal-iteration-block
  "One iteration's full `<journal>` segment: per-block labels
   include the leading `:comment` (when present) right above
   the code->value line. LLM-only iteration `:thinking` is intentionally
   excluded — prior thinking that needs to round-trip flows through
   preserved-thinking assistant messages echoed in the messages array,
   not through the journal."
  [iteration-position iteration-data]
  (let [{:keys [blocks answer]} iteration-data
        visible-blocks (if (vector? blocks) blocks (vec (or blocks [])))
        block-lines (persistent!
                      (reduce-kv
                        (fn [acc k blk]
                          (let [comment-text (some-> (:comment blk) str str/trim not-empty)
                                ;; `:comment` is a legacy per-form field from
                                ;; the pre-Phase-B splitter, which captured
                                ;; the verbatim prose slice between top-level
                                ;; forms (`;;` lines / `#_(...)` discards).
                                ;; Per-block eval no longer populates it —
                                ;; one block is one entry and its prose is
                                ;; part of `:code`. The branch survives so
                                ;; old conversations resumed from the DB
                                ;; still render their captured comments
                                ;; correctly. Render as-is; DO NOT prepend
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
           (if (> next-used budget-tokens)
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
            (if (> next-used budget)
              (let [dropped (count remaining)
                    marker  (str ";; ... " dropped
                              " <bindings> entries omitted to fit bindings token budget "
                              used "/" budget " tokens ("
                              (long (* 100 BINDINGS_CONTEXT_FRACTION))
                              "% of model context max)")]
                (str/join "\n" (conj kept marker)))
              (recur (rest remaining) (conj kept entry) next-used)))
          (str/join "\n" kept))))))

;; =============================================================================
;; Iteration context - provider context block inserted before current user message
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

(defn- current-turn-context-block
  "Render dynamic engine telemetry into the user-role iteration trailer.

   These are minimal live coordinates, not policy and not a transcript.
   Static lifecycle shape lives in `CORE_SYSTEM_PROMPT`; capability context
   lives in `<turn_system_context>`; prior answers live in the previous-turn
   context/tooling. Keep this block tiny because it is appended every
   iteration."
  [environment {:keys [iteration]}]
  (let [iteration-position  (inc (long (or iteration 0)))
        turn-id             (or (some-> (:current-conversation-turn-id-atom environment) deref)
                              (sandbox-value environment 'TURN_ID nil))
        turn-position       (or (some-> (:current-turn-position-atom environment) deref)
                              (sandbox-value environment 'TURN_POSITION nil))
        conversation-state-id (or (sandbox-value environment 'CONVERSATION_STATE_ID nil)
                                (sandbox-value environment 'TURN_CONVERSATION_STATE_ID nil))
        ;; Title lives only on `:conversation-title-atom` + the DB now;
        ;; the `CONVERSATION_TITLE` SCI binding was retired (see
        ;; `inject-system-var-snapshots` docstring for rationale).
        conversation-title  (some-> (:conversation-title-atom environment) deref)]
    (prompt-block
      "current_turn_context"
      (str "conversation_id: " (:conversation-id environment) "\n"
        "turn_id: " (context-value-str (or turn-id "")) "\n"
        "turn_position: " (context-value-str (or turn-position 0)) "\n"
        "iteration_position: " iteration-position "\n"
        "conversation_state_id: " (context-value-str (or conversation-state-id "")) "\n"
        "conversation_title: " (context-value-str (or conversation-title "")) "\n"
        "prompt_role: user"))))

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

(defn- format-iteration-hint
  [{:keys [importance text]}]
  (str "<iteration_hint importance=\"" importance "\">\n"
    (attr-str text)
    "\n</iteration_hint>"))

(defn- iteration-hints-block
  [hints]
  (when-let [hints (seq (keep identity hints))]
    (str "<iteration_hints>\n"
      (str/join "\n" (map format-iteration-hint hints))
      "\n</iteration_hints>")))

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

   Plus zero or more tagged `<iteration_hint importance=\"...\">`
   entries wrapped in `<iteration_hints>`. All entries come from
   `:turn.iteration/start` hooks on active extensions; core owns no built-in
   hook policy. Each hook receives the per-iteration hook ctx below and
   returns either nil (silent) or `{:hint :importance?}`.

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
        :turn.iteration/start hooks are consulted.

   Optional:
     `:blocks-by-iteration` - carried iterations as
        `[iteration-position {:thinking :blocks}]` pairs for the
        <journal> renderer. Rendering ignores `:thinking` and trims by token
        budget, not fixed iteration count.
     `:iteration` - current iteration position (1-based for rendered refs;
        callers that keep an internal counter convert before exposing it)."
  [environment {:keys [blocks-by-iteration active-extensions iteration
                       model stable-prompt-content current-user-content context-limit
                       title-refresh?]
                :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [ctx-limit (effective-context-limit model context-limit)
        current-context-block (current-turn-context-block environment
                                {:iteration iteration})
        pinned-text (str/join "\n\n" (keep identity [stable-prompt-content current-user-content current-context-block]))
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
                        [stable-prompt-content current-user-content current-context-block recent-block bindings-block]))
        input-tokens (or (count-prompt-tokens model prompt-text) 0)
        conversation-title (some-> (:conversation-title-atom environment) deref str str/trim not-empty)
        ;; Turn position reaches hooks via :turn-position. The title
        ;; nudge uses it to fire on a turn cadence (turn 1, then every
        ;; Nth turn) instead of an iteration cadence inside a single turn.
        turn-position (long (or (some-> environment :current-turn-position-atom deref) 1))
        nudge-ctx {:environment environment
                   :iteration (if (some? iteration) (inc (long iteration)) 1)
                   :turn-position turn-position
                   :previous-blocks last-iteration-blocks
                   :model model
                   :context-limit ctx-limit
                   :input-tokens input-tokens
                   :title-refresh? (boolean title-refresh?)
                   :conversation-title conversation-title
                   ;; Raw current-turn user request — available to hooks
                   ;; that want user-intent context (none in core today).
                   :user-request current-user-content}
        ;; <iteration_hints> are populated by :turn.iteration/start
        ;; `:ext/hooks` — the single mechanism for model-facing advisory hints.
        ;; Hook fns return nil (silent) or `{:hint :importance?}`.
        all-hints (into []
                    (mapcat
                      (fn [ext]
                        (for [{:keys [id phase fn]} (or (:ext/hooks ext) [])
                              :when (= :turn.iteration/start phase)
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
        hints-block (iteration-hints-block all-hints)
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [current-context-block recent-block bindings-block hints-block])]
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
   <journal>, <bindings>, and DB-backed tools. The current user message is tagged as
   <current_user_message>.

   One full previous-turn context block may be prepended so short follow-ups
   can inspect the prior exchange without replaying the whole conversation."
  [{:keys [stable-prompt-messages initial-user-content previous-turn-context]}]
  (let [previous-block (previous-turn-context-block previous-turn-context)
        user-block     (when initial-user-content
                         (prompt-block "current_user_message" initial-user-content))]
    (vec
      (concat
        (or stable-prompt-messages [])
        (when user-block
          [{:role "user" :content (str/join "\n\n" (keep identity [previous-block user-block]))}])))))

;; =============================================================================
;; System prompt
;; =============================================================================

(def ^:private CORE_SYSTEM_PROMPT
  "λVis — Clojure SCI harness with a recursive eval loop.

ARCHITECTURE
  Turn       : one user<->vis exchange. Iterate internally until
               `(done <IR>)` is accepted (see EMIT_FINAL).
  Iteration  : one reply with one or more ```clojure``` blocks.
               λVis evals each, records evidence, asks again.
  Block      : one ```clojure fenced form. Unit of eval and
               attribution. Return = value of last expr.
               stdout/println/throw captured into <journal>.
  <journal>  : append-only block-eval log. Persists across turns.
  <bindings> : namespace defs. Persists across turns.
  Tools      : every tool comes from an active extension (see
               <extensions>); the core harness names none.

ENV
  Aliases: walk str set pp edn s
  Banned : slurp spit clojure.java.io — all I/O is via extensions.
  Truth  : runtime > source > docs > memory

DEF DISCIPLINE
  Every (def …) requires a docstring as the second arg:
    (def NAME 'short doc' VAL)
    (defn NAME 'short doc' [args] body)
    (defn- NAME 'short doc' [args] body)
  (Use real Clojure double-quote strings; single quotes shown only
  to keep this prompt body parseable.)
  Vars persist across turns. λVis stores each def's source per name
  so re-evaluating restores the var verbatim on the next process run.

  ALLOWED def heads: def, defn, defn-, defonce, defmulti, defmacro.
  BANNED def heads : defrecord, deftype, defprotocol, gen-class,
                     extend-type, extend-protocol, definterface, reify.
  Reason: these produce JVM classes / protocol method tables that
  cannot round-trip through λVis's per-var restore path — the var
  name persists but the class does not, so the next process boot
  resurrects a half-broken binding. The sandbox refuses these heads
  at eval time. If you need ad-hoc polymorphism use plain maps +
  multimethods (`defmulti` / `defmethod`).

TURN PROTOCOL
  Exactly one ```clojure block per iteration; wrap multiple statements
  in `(do …)`. No prose outside the block.
  The block evals atomically: result attaches to <journal>; defs
  persist across turns under the name you gave them.
  Errors are evidence — a thrown form ends the iteration; the next
  iteration sees the structured error and you correct.

LOOP DISCIPLINE
  Vars are memory. When you need a probe result more than once, bind
  it (e.g. give a `report` var the value `(summary (v/cat 'foo'))`).
  The next iteration sees `report` in <bindings>; do not re-probe.
  Every claim traces to a value you observed, never memory.

EMIT_FINAL
  (done <IR>)

  Accepted whenever the form runs to completion without throwing. Tool
  calls and `(done …)` legitimately co-exist in the same `(do …)`
  block — handles return synchronously, so probe + answer can land in
  one iteration. Canonical 1-iteration shape (replace the single
  quotes with real double-quote strings when you emit code):

    (do (def h 'README handle' (v/cat 'README.md'))
        (done [:ir [:p (str 'lines: ' (:line-count (summary h)))]]))

  If the form throws before `(done …)`, the answer is not composed
  and the turn continues with the error in <journal>.

ANSWER_IR
  EDN hiccup: [:ir block*]
  Blocks: :p | :h {:level 1-6} | :code {:lang string} | :ul
        | :ol {:start int} | :li | :quote | :table | :tr | :th | :td
  Inline: :span {:preserve-ws? bool :nowrap? bool} | :br
        | :strong | :em | :c | :a {:href string}
        | :img {:src string :alt string} | :kbd | :mark | :sup | :sub
")

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
                  an `:ext/alias`.
     :namespace - fully-qualified ns symbol of the extension.
     :doc       - one-line LLM description from `:ext/doc` (when set).
     :kind      - categorical bucket (providers, channels, foundation,
                  languages, persistance, ...) used as the section
                  label both in this snapshot and in `vis extensions
                  list` (when set).
     :registry-id - canonical manifest id, usually the alias symbol.
     :symbols   - vec of bare symbol names the extension intern'd into
                  the sandbox.

   The vec is bound ONCE at turn start (see `iteration-loop`) and
   stays frozen for the rest of the turn - every iteration sees the
   same value."
  [active-extensions]
  (->> (or active-extensions [])
    (mapv (fn [ext]
            (let [info (extension/extension-info ext)
                  registry-id (:registry-id info)]
              (cond-> {:namespace   (:namespace info)
                       :alias       (:alias info)
                       :doc         (:doc info)
                       :kind        (:kind info)
                       :registry-id registry-id
                       :symbols     (mapv :ext.symbol/symbol (:ext/symbols ext))}
                (nil? (:alias info)) (dissoc :alias)
                (nil? (:doc info)) (dissoc :doc)
                (nil? (:kind info)) (dissoc :kind)
                (nil? registry-id) (dissoc :registry-id)))))))

(defn- extension-prompt-id
  [ext]
  (str (or (get-in ext [:ext/alias :alias])
         (:ext/namespace ext)
         "unknown")))

(defn- extension-prompt-fragment
  [ext body]
  (str "<extension id=\"" (attr-str (extension-prompt-id ext)) "\">\n"
    body
    (when-not (str/ends-with? body "\n") "\n")
    "</extension>"))

(defn- extensions-prompt-block
  "Collect `<extensions>` from every active extension that declares
   `:ext/prompt`. Each prompt is `(fn [env] -> string)` (normalized at
   registration). Non-blank results are wrapped as an extension element
   with an id attribute, then joined inside one `<extensions>...</extensions>`
   block."
  [environment active-extensions]
  (let [fragments (keep (fn [ext]
                          (when-let [f (:ext/prompt ext)]
                            (try
                              (let [result (call-extension-callback ext f environment)]
                                (when (and (string? result) (not (str/blank? result)))
                                  (extension-prompt-fragment ext result)))
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

(defn- turn-system-context-block
  "Turn-scoped system context that can be rebuilt/replaced as runtime
   capabilities change.

   Keep this as ONE provider system message. Extension prompts belong here,
   not in every per-iteration `<current_turn_context>` trailer. When a future
   reload path recomputes active extensions mid-turn, it should replace this
   message in the rebuilt stateless provider message vector rather than append
   a second extension/context message."
  [environment active-extensions]
  (when-let [extensions-block (extensions-prompt-block environment active-extensions)]
    (prompt-block "turn_system_context" extensions-block)))

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
  "Assemble provider-prefix messages.

   Send order is explicit and tested:
     `<system_prompt>`       - CORE_SYSTEM_PROMPT + caller addendum
     `<turn_system_context>` - turn-scoped runtime capability context. Today it
                               contains the single `<extensions>` block; future
                               extension reloads should replace this one message,
                               never append a second extension context.

   Extension fragments are separate from the core system prompt and are not
   repeated in per-iteration `<current_turn_context>` trailers.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Drives
        environment, extension prompt, and nudge collection.

   Optional opts:
     `:system-prompt`            - caller addendum appended to CORE."
  [environment {:keys [system-prompt active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "assemble-stable-prompt-messages requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [core-block (prompt-block "system_prompt"
                     (build-system-prompt {:system-prompt system-prompt}))
        turn-system-block (turn-system-context-block environment active-extensions)]
    (vec
      (keep stable-prompt-message
        [core-block turn-system-block]))))

;; =============================================================================
;; Phase 7 prep — REPL tape rendering primitives
;;
;; The pivot replaces <journal> + <bindings> + <current_user_message> +
;; <current_turn_context> with a pure-Clojure tape rendering of the
;; previous iteration's source + ;; => results. These primitives live
;; alongside the legacy renderers above; Phase 7 main wires them into
;; build-iteration-context. Until then they are dormant and tested in
;; isolation.
;; =============================================================================

(def ^:const TAPE_RESULT_MAX_CHARS
  "Per-result truncation cap when rendering `;; =>` lines on the tape."
  1500)

(defn tape-iteration-header
  "One-line `;; --- iteration N | turn=K conv=… state=… | status=… ---`
   header that prefixes each rendered iteration's code on the tape.
   `status` is :done | :error | :current. Optional ids: nil values are
   elided so test contexts can render with partial coordinates."
  [{:keys [iteration-position turn-position conv-id state-id status]}]
  (let [parts (cond-> []
                iteration-position (conj (str "iteration " iteration-position))
                turn-position      (conj (str "turn=" turn-position))
                conv-id            (conj (str "conv=" conv-id))
                state-id           (conj (str "state=" state-id))
                status             (conj (str "status=" (name status))))]
    (str ";; --- " (str/join " | " parts) " ---")))

(defn tape-result-line
  "Render `;; => <pr-str of result>` honoring `TAPE_RESULT_MAX_CHARS`.
   Returns nil for `:vis/no-result` so a thrown form skips the result
   line entirely (the error annotation carries the meaning)."
  [result]
  (when-not (= :vis/no-result result)
    (let [s (safe-pr-str result {:max-chars TAPE_RESULT_MAX_CHARS})]
      (str ";; => " s))))

(defn tape-side-effect-line
  "Render a side-effect annotation: `;; ! <kind>> <text>` for
   `:stdout` / `:stderr` / `:error` / `:timeout`. Empty / blank text
   returns nil so the renderer can drop it."
  [kind text]
  (when (and text (not (str/blank? (str text))))
    (let [tag (case kind
                :stdout  "stdout> "
                :stderr  "stderr> "
                :error   "ERROR "
                :timeout "TIMEOUT ")]
      (str ";; ! " tag (str/trim (str text))))))

(defn format-tape-iteration
  "Render one iteration as commented Clojure source for the tape.

   Input shape:
     {:iteration-position N
      :turn-position      K          ; optional, for cross-turn rendering
      :conv-id            \"…\"      ; optional, short id
      :state-id           \"…\"      ; optional, short id
      :status             :done|:error|:current
      :code               \"(def …)\"
      :result             <value>    ; or :vis/no-result on throw
      :error              {:message…} ; structured map or nil
      :stdout             \"…\"
      :stderr             \"…\"
      :timeout?           bool}

   Output (newline-joined):
     ;; --- iteration N | turn=K conv=… state=… | status=done ---
     <code text>
     ;; ! stdout> <captured>
     ;; ! stderr> <captured>
     ;; => <pr-str result>           ; omitted on error
     ;; ! ERROR <message>            ; only on error
     ;; ! TIMEOUT <message>          ; only on timeout

   Side-effect lines render in stdout / stderr / => / ERROR / TIMEOUT
   order. The :code body is preserved verbatim — the model's own `;;`
   thinking comments inside the form survive untouched."
  [{:keys [code result error stdout stderr timeout?] :as iter}]
  (let [header     (tape-iteration-header iter)
        body       (when (string? code) (str/trim-newline code))
        result-ln  (when-not (or error timeout?)
                     (tape-result-line result))
        stdout-ln  (tape-side-effect-line :stdout stdout)
        stderr-ln  (tape-side-effect-line :stderr stderr)
        error-ln   (when error
                     (tape-side-effect-line :error (or (:message error) (str error))))
        timeout-ln (when timeout?
                     (tape-side-effect-line :timeout
                       (or (:message error) "iteration timed out")))]
    (str/join "\n"
      (keep identity [header body stdout-ln stderr-ln result-ln error-ln timeout-ln]))))

(defn format-system-vars-block
  "Render the `;; system-vars:` block for the live-vars discovery
   surface. `entries` is a vec of `{:name :doc}` maps. UPPERCASE
   convention; engine-managed (USER_REQUEST, etc.). Returns nil when
   no entries — caller can omit the block entirely."
  [entries]
  (when (seq entries)
    (str/join "\n"
      (cons ";; system-vars:"
        (map (fn [{:keys [name doc]}]
               (str ";;   " name "  " (pr-str (or doc ""))))
          entries)))))

(defn format-live-vars-block
  "Render the `;; live-vars (N/30):` block for the discovery surface.
   `entries` is a vec of `{:name :doc}` maps. `cap` defaults to 30.
   Returns nil for an empty vec."
  ([entries] (format-live-vars-block entries 30))
  ([entries cap]
   (when (seq entries)
     (str/join "\n"
       (cons (str ";; live-vars (" (count entries) "/" cap "):")
         (map (fn [{:keys [name doc]}]
                (str ";;   " name "  " (pr-str (or doc ""))))
           entries))))))

(defn format-tape
  "Render a sequence of iterations as the full N-1 (or N-1 + N) tape
   the model sees in the user-role message. `iters` is a vec of
   per-iteration maps (see `format-tape-iteration` for the shape) in
   chronological order. Iterations render top-to-bottom separated by
   a blank line so the iteration-header lines visually anchor each
   block.

   Tape-window policy lives in the caller; this fn renders whatever
   it's given. Pass `[N-1]` on a clean run, `[N-1 N]` on an error
   recovery iteration."
  [iters]
  (when (seq iters)
    (str/join "\n\n" (map format-tape-iteration iters))))

(defn format-user-role-tape-message
  "Assemble the full user-role message body: optional system-vars
   header + optional live-vars header + the rendered tape. Each
   section is joined with a blank line so the tape header lines stay
   visually distinct.

   Phase 7 main will wire this through `build-iteration-context` in
   place of the legacy <journal>/<bindings>/<current_user_message>
   blocks. Until then the engine still uses the old assembly path
   and this fn is exercised only by tests."
  [{:keys [system-vars live-vars iters]}]
  (let [sys-block  (format-system-vars-block system-vars)
        live-block (format-live-vars-block live-vars)
        tape-block (format-tape iters)
        parts      (keep identity [sys-block live-block tape-block])]
    (when (seq parts)
      (str/join "\n\n" parts))))

(defn iteration->tape-iter
  "Adapt an engine iteration map (the multi-block shape from the legacy
   loop) into the tape-iter shape `format-tape-iteration` expects.

   Engine input:
     {:position N
      :blocks   [{:code :result :error :stdout :stderr
                  :execution-time-ms :timeout?} …]
      :answer   \"…\"          ; optional}

   Optional `coords` map carries per-iteration telemetry that does not
   live on the iteration row itself: turn-position, conv-id, state-id,
   status. Engine-side caller fills these from env / state.

   Joins block fields conservatively: code by `\\n`; stdout/stderr
   joined with `\\n` after dropping blanks; result is the LAST
   successful block's value (model's intended return); error is the
   FIRST error encountered (root cause); timeout? true if any block
   timed out. Status defaults to :error if any block errored, :done
   otherwise — caller can override via `coords`.

   Phase 7 main wires this in `build-iteration-context`. Single-form
   iterations (post-Phase-4) round-trip identity-preserved through
   the same adapter — the join over a 1-element blocks vec is still
   the same single block."
  ([iteration]
   (iteration->tape-iter iteration {}))
  ([iteration {:keys [iteration-position turn-position conv-id state-id status]}]
   (let [blocks (or (:blocks iteration) [])
         non-blank #(when (and (string? %) (not (str/blank? %))) %)
         joined-code (->> blocks (keep :code) (str/join "\n"))
         joined-stdout (->> blocks (keep :stdout) (keep non-blank) (str/join "\n"))
         joined-stderr (->> blocks (keep :stderr) (keep non-blank) (str/join "\n"))
         first-error  (some :error blocks)
         last-success (->> blocks (remove :error) last)
         last-result  (when last-success (:result last-success))
         any-timeout? (boolean (some :timeout? blocks))
         derived-status (cond
                          first-error  :error
                          any-timeout? :error
                          :else        :done)]
     (cond-> {:iteration-position (or iteration-position (:position iteration))
              :status             (or status derived-status)
              :code               joined-code
              :result             (if first-error :vis/no-result last-result)
              :error              first-error
              :stdout             joined-stdout
              :stderr             joined-stderr
              :timeout?           any-timeout?}
       turn-position (assoc :turn-position turn-position)
       conv-id       (assoc :conv-id conv-id)
       state-id      (assoc :state-id state-id)))))

(defn build-iteration-context-tape
  "Phase 7 build-iteration-context replacement: assembles the
   per-iteration user-role trailer in the new pivot shape — XML
   `<iteration_hints>` (preserved per design) followed by pure
   commented-Clojure sections (system-vars / live-vars / tape).

   Lives alongside the legacy `build-iteration-context`. Phase 7 main
   flips the engine call site in loop.clj from the legacy fn to this
   one. Until then the engine still uses the legacy assembly.

   Required opts:
     `:active-extensions` - vec from `(active-extensions env)`. Drives
        `:turn.iteration/start` hook collection for `<iteration_hints>`.

   Optional opts:
     `:blocks-by-iteration` - carried iterations as
        `[iteration-position {:blocks …}]` pairs. Fed through
        `iteration->tape-iter` to produce tape entries.
     `:iteration`            - current iteration position (1-based).
     `:current-status`       - status to stamp on the LATEST iteration
        in the rendered tape (default :done; pass :current for the
        in-flight iteration that has not yet executed).
     `:system-vars`          - vec of `{:name :doc}` for engine-managed
        UPPERCASE vars. Phase 7 main fills this from sandbox introspection.
     `:live-vars`            - vec of `{:name :doc}` for user vars
        within the LRU window. Phase 7 main fills from per-env LRU map.
     `:model` / `:context-limit` - unused for now; reserved so the
        signature matches `build-iteration-context` for a clean swap."
  [environment {:keys [blocks-by-iteration active-extensions iteration
                       current-status system-vars live-vars]
                :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context-tape requires :active-extensions"
             {:type :vis/missing-active-extensions})))
  (let [turn-position (long (or (some-> environment :current-turn-position-atom deref) 1))
        conv-id       (some-> environment :conversation-id str
                        (subs 0 (min 8 (count (str (:conversation-id environment))))))
        last-pos      (some-> blocks-by-iteration last first)
        tape-iters    (mapv (fn [[pos iter-data]]
                              (iteration->tape-iter
                                iter-data
                                (cond-> {:iteration-position pos
                                         :turn-position turn-position
                                         :conv-id conv-id}
                                  (and last-pos (= pos last-pos))
                                  (assoc :status (or current-status :done)))))
                        (or blocks-by-iteration []))
        body (format-user-role-tape-message
               {:system-vars (or system-vars [])
                :live-vars   (or live-vars [])
                :iters       tape-iters})
        ;; Reuse the legacy nudge-ctx + hint-collection path so existing
        ;; `:turn.iteration/start` hook contracts keep working unchanged.
        nudge-ctx {:environment environment
                   :iteration (if (some? iteration) (inc (long iteration)) 1)
                   :turn-position turn-position
                   :previous-blocks (some-> blocks-by-iteration last second :blocks)
                   :model nil
                   :context-limit nil
                   :input-tokens 0
                   :title-refresh? false
                   :conversation-title (some-> (:conversation-title-atom environment) deref str str/trim not-empty)
                   :user-request nil}
        all-hints (into []
                    (mapcat
                      (fn [ext]
                        (for [{:keys [id phase fn]} (or (:ext/hooks ext) [])
                              :when (= :turn.iteration/start phase)
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
        hints-block (iteration-hints-block all-hints)
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [hints-block body])]
    (when (seq parts)
      (str/join "\n\n" parts))))
