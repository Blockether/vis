(ns com.blockether.vis.internal.loop
  (:refer-clojure)
  (:require
   [clj-reload.core :as clj-reload]
   [clojure.set :as set]
   [charred.api :as json]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.svar.core :as svar]
   [com.blockether.svar.internal.llm :as svar-llm]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.svar.internal.util :as util]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.cancellation :as cancellation]
   [com.blockether.vis.internal.ctx :as vctx]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.error :as error]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.parse-diagnose :as pd]
   [com.blockether.vis.internal.paren-repair :as paren-repair]
   [com.blockether.vis.internal.env.sci-patches :as sci-patches]
   [com.blockether.vis.internal.render :as render]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.workspace :as workspace]
   [edamame.core :as edamame]
   [sci.core :as sci]
   [taoensso.telemere :as tel])
  (:import
   [java.io File]
   [java.security MessageDigest]))

;; =============================================================================
;; Query runtime settings
;; =============================================================================

(def DEFAULT_EVAL_TIMEOUT_MS
  "Default timeout in milliseconds for code evaluation in the SCI sandbox."
  120000)

(def MIN_EVAL_TIMEOUT_MS
  "Floor for :eval-timeout-ms."
  3000)

(def MAX_EVAL_TIMEOUT_MS
  "Hard ceiling for :eval-timeout-ms."
  (* 30 60 1000))

(def ^:dynamic *eval-timeout-ms*
  "Dynamic timeout in milliseconds for SCI code evaluation."
  DEFAULT_EVAL_TIMEOUT_MS)

(def ASK_CODE_IDLE_TIMEOUT_MS
  "Default inter-chunk idle timeout for Vis `svar/ask-code!` calls."
  (* 5 60 1000))

(def ASK_CODE_SEMANTIC_TIMEOUT_MS
  "Default model/progress timeout for Vis `svar/ask-code!` streams (ms).

   Catches the failure mode `idle-timeout-ms` cannot: the transport
   keeps emitting bytes (SSE `: ping` comments, blank separators, or
   any framing-layer keepalive that returns from `.readLine`) which
   resets the idle watchdog forever, yet zero `response.*.delta` /
   `message.*` events ever arrive. Without this watchdog the iteration
   loop blocks on `.readLine` until the model finally streams output
   (observed: codex/responses gpt-5.5 iter 0 silent for 11min 9s on
   2026-05-20, session da9f0b47, no timeout ever fired).

   4 minutes (240000 ms) is the considered ceiling: > Anthropic's
   documented 185s worst case for legitimate extended thinking on
   Opus 4.5 (anthropics/claude-agent-sdk-typescript#44), high enough
   that a deep reasoning model with a real long pre-token phase still
   succeeds, low enough that a stuck provider surfaces a real error
   in under 5 minutes instead of holding the whole turn hostage.

   Disable per call with `:semantic-timeout-ms nil`."
  (* 4 60 1000))

(defn- with-default-ask-code-idle-timeout
  [opts]
  (cond-> opts
    (not (contains? opts :idle-timeout-ms))
    (assoc :idle-timeout-ms ASK_CODE_IDLE_TIMEOUT_MS)

    (and (some? ASK_CODE_SEMANTIC_TIMEOUT_MS)
      (not (contains? opts :semantic-timeout-ms)))
    (assoc :semantic-timeout-ms ASK_CODE_SEMANTIC_TIMEOUT_MS)))

(defn clamp-eval-timeout-ms
  "Clamp a candidate eval timeout to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS]."
  [candidate]
  (-> candidate long (max MIN_EVAL_TIMEOUT_MS) (min MAX_EVAL_TIMEOUT_MS)))

(def ^:dynamic *rlm-context*
  "Dynamic context for RLM debug logging."
  nil)

;; =============================================================================
;; Single-iteration runner
;; =============================================================================

;; ---------------------------------------------------------------------------
;; Core helpers
;; ---------------------------------------------------------------------------

(defn- truncate [s n]
  (let [s (str s)] (if (> (count s) n) (subs s 0 n) s)))

(defn- realize-value [v]
  (cond
    (instance? clojure.lang.IDeref v) @v
    (map? v) (into {} (map (fn [[k vv]] [k (realize-value vv)])) v)
    (vector? v) (mapv realize-value v)
    (set? v) (set (map realize-value v))
    (sequential? v) (doall (map realize-value v))
    :else v))

;; `DEF_HEADS_FOR_RESTORE` lives in `env.clj` (single source of truth
;; shared by source extraction, dep-edge walking, and restore-time
;; shape check via `env/parsed-def-form?`).

(defn- extract-def-sources
  "Parse `code` and return `{var-name-string -> per-form-pr-str}` for
   every `(def NAME ...)` / `(defn NAME ...)` / ... shape it contains.
   Walks `(do ...)` containers transparently so
   `(do (def a 1) (def b 2))` produces both entries. Forms whose head
   is not in `env/DEF_HEADS_FOR_RESTORE` or whose name is not a symbol are
   ignored. Parse errors yield {} so the caller falls back to the
   whole-iteration source.

   Edamame preserves metadata; pr-str re-emits a round-trippable
   form. Per-var precise source is what `db-restore-blocks` re-evals
   when the stored result was `{:vis/ref :expr}` (functions, lazy
   seqs, runtime objects), so granularity here is the difference
   between restoring ONE var and accidentally re-running every
   side-effecting def in the same iteration block."
  [code]
  (try
    (let [forms (edamame/parse-string-all (or code "")
                  {:all true
                   :readers (fn [_tag] (fn [val] (list 'do val)))})
          flatten-do (fn flat [f]
                       (if (and (seq? f) (= 'do (first f)))
                         (mapcat flat (rest f))
                         [f]))
          flat-forms (mapcat flatten-do forms)]
      (reduce (fn [acc form]
                (if (and (seq? form)
                      (symbol? (first form))
                      (contains? env/DEF_HEADS_FOR_RESTORE (first form))
                      (symbol? (second form)))
                  (assoc acc (name (second form)) (pr-str form))
                  acc))
        {} flat-forms))
    (catch Throwable _ {})))

(defn dep-edges-from-source
  "Parse `iteration-code` and emit one raw dependency edge per
   (free-symbol-in-init → def-name) reference. Walks every form whose
   head is in `env/DEF_HEADS_FOR_RESTORE`; for each such form, collects
   ALL symbol references in the init / fn-body subtree and emits
   `{:upstream <ref-name> :downstream <def-name>}` edges.

   Why source-walk and not the SCI resolve hook: SCI analyzes /
   compiles forms before evaluating them, baking symbol-to-var
   resolution into the AST. The runtime `resolve-symbol*` patch never
   fires for symbols in an init body, so the only stable point at
   which we can recover dep relationships is the source itself.

   The output is intentionally permissive: it includes refs to core
   ops (`inc`, `*`, `+`), to locals introduced by `let` / `fn`
   parameter lists, and to macro symbols. The persistence layer
   filters every edge against the existing-soul-name set in its
   write transaction, so only refs that actually correspond to a
   tracked user var land in `definition_dependency`. Self-references
   (a recursive `defn` body using its own name) are dropped here
   since they degenerate the topo sort on restore.

   Parse errors yield [] — the iteration's eval already succeeded;
   downstream dep tracking is best-effort, not load-bearing."
  [iteration-code]
  (try
    (let [forms (edamame/parse-string-all (or iteration-code "")
                  {:all true
                   :readers (fn [_tag] (fn [val] (list 'do val)))})
          flatten-do (fn flat [f]
                       (if (and (seq? f) (= 'do (first f)))
                         (mapcat flat (rest f))
                         [f]))
          flat-forms (mapcat flatten-do forms)]
      (->> flat-forms
        (mapcat (fn [form]
                  (when (and (seq? form)
                          (symbol? (first form))
                          (contains? env/DEF_HEADS_FOR_RESTORE (first form))
                          (symbol? (second form)))
                    (let [def-name (name (second form))
                          ;; init-and-body: everything after (HEAD NAME)
                          ;; — includes optional docstring + arg vector
                          ;; for defn, plain value for def. We do NOT
                          ;; need to be precise about which subform is
                          ;; init; the persistence-side filter against
                          ;; the soul-name set discards everything
                          ;; that is not a tracked var.
                          init-and-body (drop 2 form)
                          refs (->> (tree-seq coll? seq init-and-body)
                                 (filter symbol?)
                                 (map name)
                                 set)]
                      (->> refs
                        (remove #(= % def-name))
                        (map (fn [r]
                               {:upstream r :downstream def-name})))))))
        distinct
        vec))
    (catch Throwable _ [])))

(defn def-sink->vars-snapshot
  "Convert per-iteration SCI def-sink entries into the persistence
   shape consumed by `definition_soul` / `definition_state` rows:
   `[{:name :value :code :time-ms?} ...]`.

   Replaces the legacy parse-source + walk-sandbox-locals path
   (`extract-defining-name` / `extract-def-names` /
   `restorable-var-snapshots`). Source-of-truth is the captured SCI
   var (IDeref) plus its evaluated metadata — every def the model
   evaluated lands here, regardless of nesting (`(do (def a …) …)`),
   macro-expansion (`defn` / `defmacro` / `s/def`), or syntactic
   shape.

   `:code` per var is the SMALLEST `(def NAME ...)` form edamame can
   recover from `iteration-code`. Restore re-evals that exact form
   (and only that form) when the stored value is a function / lazy
   seq / runtime object — functions round-trip correctly without
   accidentally re-running sibling defs in the same iteration.
   Sinks whose name does not match any parsed form (macro-expanded
   shapes the parser cannot see) fall back to the whole-iteration
   source.

   `iteration-time-ms` is total eval time for this iteration; rides
   along on every var row (iteration-scoped, not per-def)."
  [def-sink iteration-code iteration-time-ms]
  (let [src-by-name (extract-def-sources iteration-code)]
    (->> def-sink
      (keep (fn [{:keys [name var]}]
              (when (and name var)
                (let [raw          (try (deref var) (catch Throwable _ nil))
                      realized     (realize-value raw)
                      name-s       (str name)
                      precise-code (or (get src-by-name name-s)
                                     iteration-code)]
                  (cond-> {:name  name-s
                           :value realized
                           :code  precise-code}
                    iteration-time-ms
                    (assoc :time-ms iteration-time-ms))))))
      vec)))

(defn- format-exception-short [^Throwable t]
  {:class (.getName (class t))
   :message (or (ex-message t) (str t))})

(defn- format-exception [^Throwable t & [{:keys [context]}]]
  (merge (format-exception-short t)
    {:data (ex-data t) :context context}))

;; ---------------------------------------------------------------------------

(defn log-stage!
  [stage iteration data]
  (tel/log! {:level :info :data (merge {:stage stage :iteration iteration} data)}))

(defn- elapsed-ms
  [started-ns]
  (/ (double (- (System/nanoTime) started-ns)) 1000000.0))

(defn normalize-reasoning-level [v]
  (svar/normalize-reasoning-level v))

(defn- github-copilot-claude-model?
  [resolved-model]
  (and (contains? #{:github-copilot-individual :github-copilot-business}
         (:provider resolved-model))
    (boolean (re-find #"(?i)claude" (str (:name resolved-model))))))

(def ^:private casual-request-pattern
  #"(?iu)^\s*(hi|hey|hello|yo|sup|siema|cześć|czesc|hej|dzień dobry|dzie dobry|thanks|thank you|thx|ok|okay|👍|👋)[\s!.?,]*\s*$")

(defn- casual-user-request?
  [s]
  (let [text (some-> s str str/trim)]
    (boolean
      (and text
        (<= (count text) 80)
        (re-find casual-request-pattern text)))))

(defn- copilot-claude-safe-reasoning-level
  "Return the reasoning level Vis is willing to send to GitHub Copilot Claude.

   Copilot bills by interaction class, not just visible response text. Deep
   reasoning on Claude can burn multiple premium interactions for a trivial
   prompt. Default policy:
   - casual chat gets no reasoning parameter;
   - :deep is capped to :balanced unless the caller opts in with
     :allow-copilot-claude-deep? true;
   - non-Copilot/non-Claude models are untouched."
  [resolved-model user-request reasoning-level {:keys [allow-copilot-claude-deep?]}]
  (cond
    (not (github-copilot-claude-model? resolved-model)) reasoning-level
    (casual-user-request? user-request) nil
    (and (= :deep reasoning-level) (not allow-copilot-claude-deep?)) :balanced
    :else reasoning-level))

(defn- copilot-provider?
  [provider-id]
  (contains? #{:github-copilot :github-copilot-individual :github-copilot-business}
    provider-id))

(defn- copilot-llm-headers
  [resolved-model initiator]
  (when (and (copilot-provider? (:provider resolved-model))
          (#{"user" "agent"} initiator))
    {"X-Initiator" initiator}))

(defn- copilot-initiator-for-iteration
  [iteration]
  (if (zero? (long (or iteration 0))) "user" "agent"))

(defn needs-input-answer?
  "True for explicit clarification/needs-input answer payloads.

   Foundation exposes this through `(v/needs-input ...)`; the loop
   keeps the predicate data-shaped instead of depending on foundation
   namespaces so the core runtime has no extension cycle."
  [v]
  (and (map? v)
    (= :needs-input (:vis/answer-mode v))
    (string? (:answer/text v))
    (not (str/blank? (:answer/text v)))))

(defn markdown-answer?
  "True for a canonical final-answer payload: `{:answer string}`.
   This is the ONLY shape `(done ...)` accepts for a final answer
   (the other allowed shape is the `needs-input-answer?` map)."
  [v]
  (and (map? v)
    (string? (:answer v))))

(defn answer-markdown
  "Extract the raw Markdown source from a final-answer value.

   Canonical shapes:
   - `{:answer string}`           -> the string (from `(done {:answer ...})`)
   - `{:vis/answer-mode :needs-input :answer/text string}` -> `:answer/text`

   Returns nil for anything else. The answer pipeline only ever produces
   these two shapes; an unrecognized shape is an upstream bug."
  [answer]
  (let [v (:result answer answer)]
    (cond
      (needs-input-answer? v) (:answer/text v)
      (markdown-answer? v)    (:answer v)
      :else                   nil)))

(defn append-runtime-appendices
  "Pass-through on the Markdown-answer pipeline. Needs-input maps stay
   data-shaped so the prompt-flow gate can still read `:answer/text`
   without a render hop; Markdown-answer maps stay map-shaped so the
   persistence layer can read `:answer` verbatim."
  [_environment answer _answer-value]
  answer)

(def edamame-opts
  {:all true
   :fn true
   :regex true
   :readers (fn [_tag] (fn [val] (list 'do val)))})

(def ^:private edamame-streaming-opts
  ;; `parse-next` requires normalized opts. Raw opts make reader macros
  ;; like `'x` surface as a bare `'` symbol, which then explodes in SCI.
  (edamame/normalize-opts edamame-opts))

(def ^:private BARE_STRING_RE #"^\s*\"[^\"]*\"\s*$")
(def ^:private MARKDOWN_FENCE_RE #"^\s*`{3,}[A-Za-z0-9_-]*\s*$")

(defn- bare-string-code-block? [expr]
  (boolean (re-matches BARE_STRING_RE (str expr))))

(defn- markdown-fence-line? [line]
  (boolean (re-matches MARKDOWN_FENCE_RE (str line))))

(defn- markdown-fence-block? [expr]
  (let [lines (->> (str/split-lines (str expr))
                (map str/trim)
                (remove str/blank?))]
    (boolean (and (seq lines)
               (every? markdown-fence-line? lines)))))

(defn- comment-only-block? [^String expr]
  (try
    (zero? (count (edamame/parse-string-all (str/trim expr) edamame-opts)))
    (catch Throwable _ false)))

(defn- multi-fence-hint
  "Model-facing reminder when several ```clojure``` fences were merged into
   one eval source and that source then failed to parse / lint / eval.
   The merge is intentional tolerance (cheap fix for the common 2-3 fence
   case), but each extra fence is a fresh chance for a brace/quote to open
   in one fence and close in another, producing parser surprises like
   `the map literal contains 7 forms` on a source that LOOKS balanced.
   Returns nil when the entry was a single fence."
  [{:keys [multi-fence-merged? multi-fence-count]}]
  (when multi-fence-merged?
    (str "You emitted " (or multi-fence-count "multiple") " ```clojure``` fences in this iteration; "
      "the engine merged them into one eval source and that source failed. "
      "System prompt rule: emit exactly ONE ```clojure``` block per iteration. "
      "Fix: keep one fence; move secondary computation to defs inside the same block.")))

(defn- attach-multi-fence-hint
  "If `entry` was a multi-fence merge, splice the rule reminder into
   `error-map` as `:hint` (preserves an upstream hint by appending). The
   hint surfaces in the iteration trailer next to the original error so
   the model sees both the parser message and the systemic cause."
  [error-map entry]
  (if-let [hint (multi-fence-hint entry)]
    (let [existing (:hint error-map)]
      (assoc error-map :hint
        (if (and (string? existing) (not (str/blank? existing)))
          (str existing " " hint)
          hint)))
    error-map))

(defn- literal-code-block-error [expr]
  (cond
    (bare-string-code-block? expr)
    "Bare string literal in :code. Prose belongs in :answer (the loop auto-detects plain text), not in :code."

    (markdown-fence-block? expr)
    "Raw Markdown fence leaked into :code (` ```... `). Remove the fence marker and keep only executable Clojure forms in the code block."

    (comment-only-block? expr)
    "Code block contains only comments / discards (`;;` or `#_`) and no executable form. Add an expression to evaluate, or drop the block entirely."))

(defn- unqualified-symbol-named?
  [x n]
  (and (symbol? x)
    (nil? (namespace x))
    (= n (name x))))

(defn- top-level-do-form?
  [form]
  (and (seq? form)
    (unqualified-symbol-named? (first form) "do")))

(defn- form-source
  [form]
  (binding [*print-meta* false]
    (pr-str form)))

(defn- expand-top-level-do-form
  "Return executable top-level forms for `form`. A top-level `(do ...)` is
   syntax noise from old hints; flatten it so SCI eval still runs one real
   form at a time and result-level sentinels (`:vis/silent`, `:vis/answer`)
   remain enough for display. Nested `do` stays untouched because it may be
   semantically positional."
  [form]
  (if (top-level-do-form? form)
    (vec (rest form))
    [form]))

(defn- unwrap-top-level-do-source
  "Normalize legacy top-level `(do A B ...)` blocks to sibling top-level
   forms. Parse errors leave source unchanged so existing diagnostics keep
   original text."
  [code]
  (try
    (let [forms    (edamame/parse-string-all (or code "") edamame-opts)
          expanded (mapcat expand-top-level-do-form forms)]
      (if (= forms expanded)
        code
        (str/join "\n" (map form-source expanded))))
    (catch Throwable _
      code)))

(defn- parse-forms-streaming
  [code]
  (let [code (or code "")
        rdr  (edamame/reader code)
        opts edamame-streaming-opts
        out  (volatile! [])]
    (loop []
      (let [next-form (try
                        (edamame/parse-next rdr opts)
                        (catch Throwable t
                          {:vis/parse-error
                           (let [d (ex-data t)]
                             (cond-> {:message (or (ex-message t) (.getName (class t)))}
                               (:row d) (assoc :row (:row d))
                               (:col d) (assoc :col (:col d))))}))]
        (cond
          (and (map? next-form) (:vis/parse-error next-form))
          {:forms @out :parse-error (:vis/parse-error next-form)}

          (= next-form ::edamame/eof)
          {:forms @out :parse-error nil}

          :else
          (do
            (vswap! out into
              (mapv (fn [form]
                      {:source (form-source form) :form form})
                (expand-top-level-do-form next-form)))
            (recur)))))))

(defn- bare-list-literal-hint
  "Detect `(v1 v2 v3)` where the head is not a callable (not a symbol,
   keyword, or another collection that can act as a fn). Returns a hint
   string when the form would crash with ClassCastException at the call site,
   nil otherwise. Targets the classic LLM mistake of writing
     (1 4 7 10 13)
   instead of
     '(1 4 7 10 13)
   which the 4Clojure / swe-bench Lite traces show repeatedly."
  [form]
  (when (and (seq? form) (seq form))
    (let [head (first form)]
      (when-not (or (symbol? head) (keyword? head)
                  (seq? head) (set? head) (map? head) (vector? head))
        (str "Top-level list whose head is not callable: "
          (let [s (pr-str form)] (if (<= (count s) 200) s (str (subs s 0 200) "…")))
          ". Did you mean to quote it as data? Use `'" (pr-str form) "`.")))))

(defn- string-as-fn-hint
  "Detect `(\"text\" ...)` calls (JS/Python paren leakage into Clojure).
   Returns a hint when found, nil otherwise. Scans the form tree."
  [form]
  (let [bad (some (fn [node]
                    (when (and (seq? node) (string? (first node))) node))
              (when (coll? form) (tree-seq coll? seq form)))]
    (when bad
      (str "List with a String literal as head: " (pr-str bad)
        ". Strings are not callable in Clojure; this would throw "
        "ClassCastException. Most often this is JS/Python call-paren leakage: "
        "write `(v/bold \"text\")`, not `(v/bold(\"text\"))`."))))

(defn- pre-eval-lint-hint
  "Return a hint string when a parsed form would clearly fail at eval time
   (bare list literal, string-as-fn call). Returns nil for clean forms."
  [form]
  (or (bare-list-literal-hint form)
    (string-as-fn-hint form)))

(defn- enrich-parse-error
  "Add a `:hint` to a parse-error map when the catalogue (parse-diagnose)
   has a precise diagnostic for the underlying source. Returns the original
   error map unchanged when nothing matches."
  [parse-error code]
  (if (or (nil? parse-error) (:hint parse-error))
    parse-error
    (if-let [quote-hint (some-> (pd/diagnose-quote-balance (or code "")) :hint)]
      (assoc parse-error :hint quote-hint)
      parse-error)))

(defn- parse-top-level-forms
  "Parse `code` into a vec of {:source <pr-str> :form <edamame-form>} entries
   for per-form eval + capture.

   Parse errors are surfaced unchanged (plus quote-balance hints when precise)
   unless the error is delimiter-shaped and Parinfer can repair it into code
   that Edamame accepts. Repair is disclosed as `:repaired-source` so the
   trailer can show what changed.

   Uses edamame's incremental reader so a parse failure on form N still
   yields the parsed prefix [form-1 … form-(N-1)]. The caller evaluates the
   prefix in order and reports the parse error as form N's outcome — model
   loses only the broken form, not the entire iteration.

   Returns {:forms <vec> :parse-error <map|nil> :repaired-source <string|nil>}."
  [code]
  (let [first-pass (parse-forms-streaming code)]
    (if-not (:parse-error first-pass)
      first-pass
      (if-let [repaired (paren-repair/maybe-repair-delimiters code)]
        (let [repaired-pass (parse-forms-streaming repaired)]
          (if-not (:parse-error repaired-pass)
            (assoc repaired-pass :repaired-source repaired)
            (update first-pass :parse-error enrich-parse-error code)))
        (update first-pass :parse-error enrich-parse-error code)))))

(defn- run-sci-code [sci-ctx code & {:keys [sandbox-ns tool-event-fn env]}]
  (let [thrown       (atom nil)
        tool-counts  (atom {})
        ;; Per-top-level-form channel sink. `invoke-symbol-wrapper` writes
        ;; ONE entry per tool-symbol call; `*sink-position*` stamps each
        ;; entry with a stable position. Both rebind cleanly when the form
        ;; returns — late-arriving thread writes silently drop (the dynamic
        ;; var binding has unwound).
        channel-sink (atom [])
        sink-pos     (atom -1)
        ;; Per-iteration sinks. `*def-sink-atom*`
        ;; collects every (def …) the SCI sandbox runs (Phase 2).
        ;; `*lru-atom*` stamps every successful symbol resolution with
        ;; the current turn position (Phase 3). The engine reads both
        ;; after eval; live-vars rendering and definition_state
        ;; persistence consume them. Dependency edges (Phase 4b) come
        ;; from source parsing post-eval, not from this hook — SCI
        ;; analyzes references at compile time so the runtime resolve
        ;; hook never sees init-body symbols.
        def-sink     (sci-patches/fresh-sink-atom)
        lru          (sci-patches/fresh-lru-atom)
        turn-position (when env
                        (some-> (:current-turn-position-atom env) deref))
        ;; Live tool-event callback only - no longer accumulated into a vec.
        ;; Storage role retired (was dead persistence). The TUI/progress UI
        ;; consumes via `tool-event-fn` synchronously during eval.
        record-tool-event (fn [event]
                            (let [op (:op event)
                                  n  (get (swap! tool-counts update op (fnil inc 0)) op)
                                  event* (cond-> event
                                           (not= n 1) (assoc :id (str (name (or op :tool)) "-" n)))]
                              (when tool-event-fn (tool-event-fn event*))))
        {parsed-forms :forms parse-error :parse-error repaired-source :repaired-source}
        (parse-top-level-forms code)
        eval-one-form
        (fn [form source]
          (if-let [hint (pre-eval-lint-hint form)]
            ;; Pre-eval lint short-circuit: forms guaranteed to crash
            ;; (bare list literal head is a number/string, JS/Python paren
            ;; leakage) get a precise hint instead of SCI's generic
            ;; ClassCastException.
            {:source source
             :error {:message hint :data {:phase :vis/lint}}}
            (try
              {:source source :result (sci/eval-form sci-ctx form)}
              (catch Throwable e
                (let [err-map (try (extension/ex->op-error e {:block-source source})
                                (catch Throwable _
                                  {:message (or (ex-message e)
                                              (.getName (class e)))}))
                      sandbox-syms (try
                                     (keys (get-in @(:env sci-ctx) [:namespaces 'sandbox]))
                                     (catch Throwable _ nil))
                      sym-hint (when (and (:message err-map) (seq sandbox-syms))
                                 (pd/unresolved-symbol-hint (:message err-map) sandbox-syms))
                      err-map+hint (cond-> err-map
                                     (and sym-hint (not (:hint err-map)))
                                     (assoc :hint sym-hint))]
                  (reset! thrown e)
                  {:source source :error err-map+hint})))))
        eval-per-form
        (fn []
          ;; Walk parsed forms in order, capture per-form outcome; stop at
          ;; first eval error. Two failure shapes propagate to the trailer:
          ;;   1) eval error mid-iteration → per-form :error on that form.
          ;;   2) STREAMING PARSE ERROR (model wrote N good forms then a
          ;;      paren mistake on form N+1) → synthetic trailing entry
          ;;      showing exactly which form failed and where.
          ;; If parinferish was able to repair the source, parsed-forms
          ;; reflect the repaired version and we still mark :repaired-source
          ;; on the outcome so the trailer can disclose the auto-fix.
          (loop [todo parsed-forms
                 acc  []
                 last-result nil]
            (if (empty? todo)
              (if parse-error
                {:result nil
                 :forms  (conj acc {:source "<unparseable trailing form>"
                                    :error parse-error})
                 :error  parse-error
                 :repaired-source repaired-source}
                {:result last-result :forms acc :error nil
                 :repaired-source repaired-source
                 :repaired? (boolean repaired-source)})
              (let [{:keys [source form]} (first todo)
                    entry (eval-one-form form source)]
                (if-let [err (:error entry)]
                  {:result nil :forms (conj acc entry) :error err
                   :repaired-source repaired-source}
                  (recur (rest todo) (conj acc entry) (:result entry)))))))
        exec-future (cancellation/worker-future "vis-sci-eval"
                      (fn []
                        (try
                          (let [outcome
                                (binding [extension/*tool-event-sink* record-tool-event
                                          extension/*render-sink*         channel-sink
                                          extension/*sink-position*       sink-pos
                                          sci-patches/*def-sink-atom*     def-sink
                                          sci-patches/*lru-atom*          lru
                                          sci-patches/*current-turn-position* turn-position]
                                  (let [ns (or (sci/find-ns sci-ctx 'sandbox) sandbox-ns)]
                                    (if (or (seq parsed-forms) parse-error)
                                      (sci/with-bindings
                                        {sci/ns ns}
                                        (eval-per-form))
                                      (let [v (:val (sci/eval-string+ sci-ctx code (when ns {:ns ns})))]
                                        {:result v :forms [] :error nil}))))]
                            (assoc outcome
                              :channel @channel-sink
                              :def-sink @def-sink
                              :lru     @lru))
                          (catch Throwable e
                            (reset! thrown e)
                            ;; Whole-block parse path failure (parse-forms returned nil OR
                            ;; eval-string+ threw outside per-form path).
                            {:result nil
                             :channel @channel-sink
                             :def-sink @def-sink
                             :lru     @lru
                             :forms   []
                             :error   (try (extension/ex->op-error e {:block-source code})
                                        (catch Throwable _
                                          {:message (or (ex-message e)
                                                      (.getName (class e)))}))}))))
        timeout-ms (long *eval-timeout-ms*)
        execution-result (try
                           (deref exec-future timeout-ms nil)
                           (catch Throwable e
                             (reset! thrown e)
                             {:result nil
                              :channel @channel-sink
                              :def-sink @def-sink
                              :lru     @lru
                              :error   (try (extension/ex->op-error e {:block-source code})
                                         (catch Throwable _
                                           {:message (or (ex-message e)
                                                       (.getName (class e)))}))}))]
    ;; Park `*1`/`*2`/`*3`/`*e` after each top-level form. Push only when
    ;; the eval succeeded; on exception/timeout, set `*e` without
    ;; advancing the value stack (the form produced no value).
    (when env
      (cond
        (nil? execution-result)
        (env/push-eval-error! env (or @thrown (ex-info "Eval timeout" {})))

        (nil? (:error execution-result))
        (env/push-eval-result! env (:result execution-result))

        :else
        ;; :error is now structured ({:message ...}); fall back to
        ;; :message string when constructing the synthetic ex-info.
        (env/push-eval-error! env (or @thrown
                                    (ex-info (or (:message (:error execution-result))
                                               "eval error") {})))))
    (if (nil? execution-result)
      (do (.cancel ^java.util.concurrent.Future exec-future true)
        {:result nil
         :channel @channel-sink
         :def-sink @def-sink
         :lru     @lru
         :error   {:message (str "Timeout (" (/ timeout-ms 1000) "s)")}
         :timeout? true})
      execution-result)))

(defn- run-with-timing [sci-ctx code sandbox-ns timeout-ms start-time tool-event-fn env]
  (let [execution-result (if timeout-ms
                           (binding [*eval-timeout-ms* (clamp-eval-timeout-ms timeout-ms)]
                             (run-sci-code sci-ctx code :sandbox-ns sandbox-ns :tool-event-fn tool-event-fn :env env))
                           (run-sci-code sci-ctx code :sandbox-ns sandbox-ns :tool-event-fn tool-event-fn :env env))
        finished-time    (System/currentTimeMillis)
        execution-time   (- finished-time start-time)]
    (cond-> execution-result
      true (assoc :execution-started-at-ms start-time
             :execution-finished-at-ms finished-time
             :duration-ms execution-time)
      (:timeout? execution-result) (assoc :timeout? true)
      (not (:timeout? execution-result)) (assoc :timeout? false))))

;; Mandatory-docstring contract is enforced in the SCI sandbox by
;; `sci-patches/patched-eval-def`: `(def NAME "doc" VAL)` already
;; binds `:doc` as var metadata at eval time. The legacy
;; `extract-defining-name` + `attach-doc-meta!` post-eval source-parse
;; path was redundant after Phase 2 and has been removed.

(defn- execute-code
  "Run a single :code block through the SCI sandbox.

   Optional kwargs:
     :timeout-ms - hard-cap eval time, clamped at the
                   *eval-timeout-ms* bounds.

   Every call performs a real SCI eval. There is no result cache:
   forms with side effects (e.g. host primitives `(done ...)` and
   `(set-session-title! ...)`) MUST run their bodies on every
   invocation, and forms without side effects re-run cheaply enough
   that caching them is not worth the correctness footgun."
  [{:keys [sci-ctx sandbox-ns] :as environment} code
   & {:keys [timeout-ms tool-event-fn]}]
  (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :execute-code})]
    ;; Per-block-eval contract: feed original block source to `run-sci-code`;
    ;; it parses, repairs delimiter slips when safe, then evaluates parsed
    ;; forms. Guard validators run against the repaired source when one exists
    ;; so a stray close paren does not block repair before eval.
    (let [start-time (System/currentTimeMillis)
          exec       (try
                       (let [{:keys [parse-error repaired-source]} (parse-top-level-forms code)
                             validation-code (or repaired-source code)]
                         (when-not parse-error
                           (sci-patches/validate-non-empty-block! validation-code)
                           (sci-patches/validate-no-banned-defs! validation-code)))
                       (run-with-timing sci-ctx code sandbox-ns timeout-ms
                         start-time tool-event-fn environment)
                       (catch Throwable e
                         (env/push-eval-error! environment e)
                         {:result nil
                          :channel []
                          :def-sink []
                          :lru {}
                          :error (try (extension/ex->op-error e {:block-source code})
                                   (catch Throwable _
                                     {:message (or (ex-message e)
                                                 (.getName (class e)))
                                      :type (-> e ex-data :type)}))
                          :execution-started-at-ms start-time
                          :execution-finished-at-ms (System/currentTimeMillis)
                          :duration-ms (- (System/currentTimeMillis) start-time)
                          :timeout? false}))]
      exec)))

;; Print-cap defaults for `fmt/bounded-value-str` - chosen so a wide flat
;; collection or a deep nested map still pr-strs without materializing
;; an unbounded JVM string before truncation. Override per call site
;; when a tighter or looser bound is required.

;; ---------------------------------------------------------------------------
;; Error normalization
;; ---------------------------------------------------------------------------

(defn- op-error
  "Coerce engine/model error values into the canonical structured :error map.

   Iteration blocks require `:error` to be nil or a map. Preflight gates and
   answer validators naturally produce strings; wrap them before persistence so
   a useful model-facing error does not become `:vis/invalid-iteration-block`."
  ([err] (op-error err nil))
  ([err {:keys [code phase]}]
   (cond
     (nil? err) nil
     (map? err) err
     (instance? Throwable err)
     (try (extension/ex->op-error err
            (cond-> {}
              code (assoc :block-source code)))
       (catch Throwable _
         {:message (or (ex-message err) (.getName (class err)))}))
     :else
     (cond-> {:message (str err)}
       code (assoc :block {:source code
                           :phase  (or phase :preflight)})))))

(def ^:private INFRASTRUCTURE_ERROR_TYPES
  ;; These are provider/runtime failures, not model strategy failures.
  ;; svar already performs its own transport retry/fallback policy before
  ;; surfacing them to Vis, so feeding them back into the RLM only burns
  ;; visible iterations and cannot help the model self-correct.
  #{:svar.core/http-error
    :svar.llm/all-providers-exhausted
    :svar.llm/circuit-open
    :svar.llm/provider-exhausted})

(defn- infrastructure-error? [ex-data-map]
  (contains? INFRASTRUCTURE_ERROR_TYPES (:type ex-data-map)))

(def ^:private LAST_USER_PREVIEW_CHARS 500)

(defn- last-user-message-preview [messages]
  (when-let [c (some (fn [m] (when (= (:role m) "user") (:content m)))
                 (reverse messages))]
    (let [s (str c)]
      (if (> (count s) LAST_USER_PREVIEW_CHARS)
        (str (subs s 0 LAST_USER_PREVIEW_CHARS)
          " ...<+" (- (count s) LAST_USER_PREVIEW_CHARS) " chars>")
        s))))

(defn- exception->iteration-error-data
  "Normalize an exception into the iteration-error-data map stored on the turn row.
   Delegates to the unified `format-exception` and adds iteration context."
  [^Throwable e ctx]
  (format-exception e
    {:context {:iteration         (:iteration ctx)
               :messages-count    (count (:messages ctx))
               :routing           (:routing ctx)
               :reasoning-level   (:reasoning-level ctx)
               :last-user-preview (last-user-message-preview (:messages ctx))}}))

(defn handle-iteration-exception!
  "Error path for the main-loop try/catch around `run-iteration`.
   Infrastructure failures are terminal for the turn; model/format/code
   failures still return `{::iteration-error ...}` for RLM self-correction."
  [^Throwable e ctx]
  (let [ex-data-map (ex-data e)
        iteration (:iteration ctx)
        fatal? (infrastructure-error? ex-data-map)
        iteration-error-data (exception->iteration-error-data e ctx)]
    (tel/log! {:level (if fatal? :error :warn)
               :data  (let [base (assoc (format-exception-short e) :iteration iteration)
                            ed   (ex-data e)
                            body (some-> (:body ed) str)]
                        (cond-> base
                          (:status ed)            (assoc :status (:status ed))
                          (:request-id ed)        (assoc :request-id (:request-id ed))
                          (:request_id ed)        (assoc :request-id (:request_id ed))
                          (and body (not (str/blank? body)))
                          (assoc :body-snippet (truncate body 1000))))}
      (if fatal?
        "Provider infrastructure error - failing turn without RLM restarts"
        "RLM iteration failed, feeding error to LLM"))
    (cond-> {::iteration-error iteration-error-data}
      fatal? (assoc ::fatal-iteration-error true))))

;; ---------------------------------------------------------------------------
;; get-locals (read sandbox vars)
;; ---------------------------------------------------------------------------

(defn get-locals
  "Returns {sym -> val} of user-defined vars in the SCI sandbox
   (excludes built-ins / kw-keyed entries). Direct atom read - zero
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

(defn- def-display-result
  "Pass-through seam for future display tweaks. Silent system-call
   elision now happens explicitly on progress chunks (`:silent?`) and
   via the `:vis/silent` return sentinel for host primitives such as
   `session-title`; normal value-bearing forms remain visible."
  [_environment _code result]
  result)

;; ---------------------------------------------------------------------------
;; Answer-scoping helper (Option C)
;;
;; The iteration loop discards a `(done ...)` call iff the form
;; that ITSELF invoked it errored. Sibling errors (a typo in some
;; OTHER form, a bad v/edit elsewhere) do NOT gate termination -
;; the model's request to finalize is honored as long as the answer-
;; bearing form ran cleanly. Pre-Option C the loop discarded on ANY
;; sibling error, which is how a turn could rack up 148 retries with
;; the model repeatedly emitting `(done ...)` next to a single
;; broken `(def ...)`.
;;
;; Returns the error from the form at `form-idx` in `block-results`
;; or nil when that form's evaluation succeeded. `form-idx` may be
;; nil (older answer-atom payloads) or out-of-bounds (defensive
;; against shape drift) - both yield nil (no discard).
;; ---------------------------------------------------------------------------

(defn answer-form-error
  "Return the `:error` produced by the form at `form-idx` in
   `block-results`, or nil when the form succeeded or `form-idx`
   is missing/out-of-bounds. Pure; no side effects. Public so the
   loop and tests can both reach it without re-implementing the
   bounds check."
  [block-results form-idx]
  (when (and form-idx
          (integer? form-idx)
          (not (neg? form-idx))
          (< form-idx (count block-results)))
    (:error (nth block-results form-idx))))

;; ---------------------------------------------------------------------------
;; Parsed form helpers
;; ---------------------------------------------------------------------------

(defn- parsed-entry-form
  "Return the parsed Clojure form for a code entry/form/source string.

   With per-block eval, `code-entries-preflight` builds entries directly
   from svar's `:blocks` and the `:expr` field is the block's `:source`
   string verbatim — no `:form` pre-parse is cached on the entry. The
   preflight gates that need a parsed form (turn-answer / extension-call /
   needs-input / direct-answer detection) pay one edamame parse per
   gate per block here. That's cheap; blocks are short.

   Returns the cached `:form` when an entry carries one (older / synthetic
   call sites), otherwise re-parses `:expr` / the bare string."
  [entry-or-form-or-source]
  (try
    (cond
      (and (map? entry-or-form-or-source)
        (contains? entry-or-form-or-source :form))
      (:form entry-or-form-or-source)

      (map? entry-or-form-or-source)
      (when-let [expr (:expr entry-or-form-or-source)]
        (edamame/parse-string (str expr) edamame-opts))

      (string? entry-or-form-or-source)
      (edamame/parse-string entry-or-form-or-source edamame-opts)

      :else
      entry-or-form-or-source)
    (catch Throwable _
      nil)))

(defn- turn-answer-call-form?
  [form]
  (and (seq? form)
    (symbol? (first form))
    (= "done" (name (first form)))
    (nil? (namespace (first form)))))

(defn- session-title-meta-form?
  [entry-or-form-or-source]
  (let [form (parsed-entry-form entry-or-form-or-source)]
    (and (seq? form)
      (symbol? (first form))
      (= 'set-session-title! (first form)))))

(defn- raw-markdown-fence-leak-error [code]
  (let [fence (apply str (repeat 3 "`"))
        lines (str/split-lines (or code ""))]
    (when (some #(str/starts-with? (str/triml %) fence) lines)
      (str "Raw Markdown fence leaked into extracted :code before evaluation. "
        "Aborting the whole iteration before eval; remove all " fence
        " fence marker lines from extracted Clojure before retrying."))))

(defn- bytes->hex
  [bytes]
  (apply str (map #(format "%02x" (bit-and (int %) 0xff)) bytes)))

(defn- sha256-hex
  [s]
  (bytes->hex (.digest (MessageDigest/getInstance "SHA-256") (.getBytes (str s) "UTF-8"))))

(defn- ask-code-block-observation
  "Summarize svar 0.5.5 code-fence observations for logs/chunks."
  [ask-result]
  (let [blocks     (or (:blocks ask-result) [])
        all-blocks (or (:all-blocks ask-result) blocks)]
    {:block-count         (count blocks)
     :all-block-count     (count all-blocks)
     :dropped-block-count (max 0 (- (count all-blocks) (count blocks)))
     :saw-fence?          (boolean (:saw-fence? ask-result))
     :malformed?          (boolean (:malformed? ask-result))}))

(defn- empty-code-error-with-observation
  "Use svar 0.5.5 fence observations to make no-code retries precise."
  [default-error ask-result]
  (let [{:keys [all-block-count dropped-block-count saw-fence? malformed?]}
        (ask-code-block-observation ask-result)]
    (cond
      malformed?
      "LLM returned a malformed Markdown code fence. Emit one complete ```clojure``` block with opener and closer on their own lines."

      (and saw-fence? (pos? dropped-block-count))
      (str "LLM returned fenced blocks, but none survived Clojure selection. "
        "Use a ```clojure``` fence; untagged or other-language fences are dropped. "
        "Dropped blocks: " dropped-block-count " of " all-block-count ".")

      saw-fence?
      "LLM returned fenced content, but no executable Clojure block was selected. Use exactly one ```clojure``` block."

      :else
      default-error)))

;; `normalized-code-source` removed: `code-entries-preflight` now computes
;; the same join inline on the surviving block sources (was only ever called
;; from the splitter's old preflight path).

(defn- needs-input-call-form?
  [form]
  (and (seq? form)
    (symbol? (first form))
    (= "needs-input" (name (first form)))))

(defn- form-contains-needs-input-call?
  [entry-or-form-or-source]
  (let [form (parsed-entry-form entry-or-form-or-source)]
    (boolean (some needs-input-call-form? (tree-seq coll? seq form)))))

(defn- direct-answer-entry?
  [entry-or-form-or-source]
  (turn-answer-call-form? (parsed-entry-form entry-or-form-or-source)))

;; `bare-symbol-entry?` removed with `plain-prose-code-error` — the
;; per-block-eval cut routes prose into SCI as a parse / unresolved-symbol
;; error instead of detecting "every entry is a bare symbol" upfront.

;; Cut from this layer:
;;   - `plain-prose-code-error` + `prose->comment` — the splitter no longer
;;     produces multi-symbol entry vectors that a prose response could
;;     accidentally satisfy. With one block = one SCI eval, prose lands in
;;     SCI as a parse error or unresolved-symbol error and the model
;;     self-corrects from the structured error.
;;   - `duplicate-fenced-blocks?` + `dedupe-fenced-block-code` +
;;     `executable-block-source`/-`sources` — dedup now happens inline
;;     in `code-entries-preflight` on the block vector directly.

(defn- code-entries-preflight
  "Per-block-eval preflight. One svar Markdown code block becomes one
   code-entry; the block's `:source` is the entry's `:expr` verbatim.
   SCI parses + evals each entry as a single chunk during execution
   — there is no top-level form splitting at this layer.

   Gates retained:
     - `raw-markdown-fence-leak-error` per block. A nested ``` in the
       extracted source means svar's normalizer fell into a recursive shape;
       structured rejection beats a JVM crash.
     - Duplicate-block dedup. Some providers stutter and emit the same
       block twice; we keep the first copy and drop the rest.

  Legacy top-level `(do ...)` wrappers are unwrapped before eval/display.
  Direct sibling top-level forms are canonical; nested host bookkeeping is
  not a supported display contract."
  [_iteration-position blocks]
  (let [blocks                       (vec (or blocks []))
        ;; Dedupe by source. Same as the old `dedupe-fenced-block-code`
        ;; but operates on the block vector directly.
        unique-blocks                (->> blocks
                                       (remove #(str/blank? (:source %)))
                                       (reduce (fn [{:keys [seen acc]} b]
                                                 (if (contains? seen (:source b))
                                                   {:seen seen :acc acc}
                                                   {:seen (conj seen (:source b))
                                                    :acc  (conj acc b)}))
                                         {:seen #{} :acc []})
                                       :acc)
        duplicate-blocks-normalized? (< (count unique-blocks) (count blocks))
        ;; Build entries: one block → one entry. Per-block raw-fence-leak
        ;; guard — if a single block's source still carries a literal
        ;; ```, reject just that block; sibling blocks keep their chance
        ;; to run.
        ;; Each block becomes one code-entry. The entry carries:
        ;;   :expr             — verbatim block source (fed to SCI as-is)
        ;;   :block-lang       — svar's detected lang ("clojure" / nil)
        ;;   :render-segments  — per-form structural split for channel
        ;;                       rendering (P1.1; see
        ;;                       `render/parse-block-display`)
        ;;   :vis/structurally-silent?
        ;;                     — true iff the block contains ONLY structural
        ;;                       forms (`(done ...)` / `(set-
        ;;                       session-title! ...)`); channels that
        ;;                       don't read segments can drop the whole entry.
        raw-entries                  (mapv (fn [b]
                                             (let [source-src (:source b)
                                                   src (unwrap-top-level-do-source source-src)
                                                   unwrapped-do? (not= src source-src)]
                                               (if-let [err (raw-markdown-fence-leak-error src)]
                                                 {:expr "(vis/preflight-error :raw-markdown-fence-leak)"
                                                  :vis/preflight-error err
                                                  :block-lang (:lang b)}
                                                 (let [segments        (render/parse-block-display src)
                                                       structurally-silent?
                                                       (and (seq segments)
                                                         (not-any? #(= :code (:kind %)) segments))]
                                                   (cond-> {:expr       src
                                                            :block-lang (:lang b)
                                                            :render-segments segments
                                                            :vis/structurally-silent? structurally-silent?}
                                                     unwrapped-do? (assoc :vis/unwrapped-do? true))))))
                                       unique-blocks)
        raw-fence-error              (some :vis/preflight-error raw-entries)
        parsed-total-blocks          (count raw-entries)
        empty-code-error             (when (zero? parsed-total-blocks)
                                       "LLM returned no executable Clojure code block. Emit a ```clojure``` block; put prose in (done {:answer \"...\"}).")
        ;; Normalized concat of all surviving block sources — also the
        ;; identity used for iteration-hash dedup in the trailer.
        normalized-code              (->> raw-entries
                                       (remove :vis/preflight-error)
                                       (map (comp str/trim :expr))
                                       (remove str/blank?)
                                       (str/join "\n\n"))
        ;; Multi-fence tolerance: many providers emit several ```clojure```
        ;; fences per iteration despite the prompt asking for one. Rather
        ;; than reject (which burned an iteration), the engine concatenates
        ;; all valid fences into a single eval source. SCI parses + evals
        ;; the joined forms in sequence; the trailer carries the unified
        ;; entry.
        multi-fence-merged?          (> parsed-total-blocks 1)
        code-hash                    (when-not (str/blank? normalized-code)
                                       (sha256-hex normalized-code))]
    {:code-entries                  (cond
                                      empty-code-error
                                      [{:expr ""
                                        :vis/preflight-error empty-code-error}]

                                      multi-fence-merged?
                                      [(cond-> {:expr normalized-code
                                                :block-lang "clojure"
                                                :render-segments (render/parse-block-display normalized-code)
                                                :multi-fence-merged? true
                                                :multi-fence-count parsed-total-blocks}
                                         (some :repaired? raw-entries) (assoc :repaired? true))]

                                      :else
                                      raw-entries)
     :empty-code-preflight-error    empty-code-error
     :raw-fence-preflight-error     raw-fence-error
     :duplicate-blocks-normalized?  duplicate-blocks-normalized?
     :multi-fence-merged?           multi-fence-merged?
     :normalized-code               normalized-code
     :code-hash                     code-hash
     :original-total-blocks         parsed-total-blocks}))

(defn- answer-validation-rejection-message
  [{:keys [id]} hit]
  (let [message (some-> (:message hit) str str/trim not-empty)
        hint    (some-> (:hint hit) str str/trim not-empty)]
    (str "Answer validation hook " id " rejected the final answer."
      (when message (str " " message))
      (when hint (str " Recovery: " hint)))))

(defn- answer-validation-hook-error-message
  [ext id ^Throwable t]
  (tel/log! {:level :warn
             :id ::answer-validation-hook-threw
             :data {:ext (:ext/name ext)
                    :hook id
                    :phase :turn.answer/validate
                    :error (ex-message t)}})
  nil)

(defn- answer-validation-invalid-return-message
  [ext id hit]
  (tel/log! {:level :warn
             :id ::answer-validation-hook-invalid-return
             :data {:ext (:ext/name ext)
                    :hook id
                    :phase :turn.answer/validate
                    :returned hit
                    :explain (s/explain-data ::extension/answer-validation-reject hit)}})
  nil)

(defn- answer-validation-extensions
  [environment active-extensions]
  (or (seq active-extensions)
    (some-> (:extensions environment) deref seq)))

(defn- extension-call-block?
  [block]
  (boolean
    (or (seq (:channel block))
      (extension/tool-result? (:result block))
      (some #(extension/tool-result? (:result %)) (:forms block)))))

(defn- final-answer-structural-error
  [blocks]
  (when (some extension-call-block? blocks)
    "Final answer rejected: this iteration called an extension/tool. Run another iteration, read the tool output, then call `(done ...)` using observed evidence."))

(defn final-answer-gate-error
  "Dispatch `:turn.answer/validate` extension hooks against the
   candidate `(done …)` answer. Returns nil when every hook accepts,
   otherwise a single string surfaced as the rejected answer form's
   validation error.

   Runs the hard structural floor first: a final answer must not
   share an iteration with extension/tool calls. The model needs one
   iteration to observe tool output, then a later iteration may call
   `(done ...)` using that evidence. Own-form errors are enforced
   upstream by `answer-form-error`. Extensions that need an additional
   veto (e.g. user-facing safety / format gates) still get their
   `:turn.answer/validate` hook fired here.

   `active-extensions` is passed by the turn loop so activation is
   computed once per turn; direct callers may omit it and provide
   `:extensions` on the environment."
  ([environment iteration blocks]
   (final-answer-gate-error environment iteration blocks nil nil))
  ([environment iteration blocks answer-value]
   (final-answer-gate-error environment iteration blocks answer-value nil))
  ([environment iteration blocks answer-value active-extensions]
   (final-answer-gate-error environment iteration blocks answer-value active-extensions nil))
  ([environment iteration blocks answer-value active-extensions extra-ctx]
   (let [ctx (merge {:environment environment
                     :phase :turn.answer/validate
                     :iteration iteration
                     :blocks blocks
                     :answer answer-value}
               extra-ctx)]
     (or (final-answer-structural-error blocks)
       (some (fn [ext]
               (some (fn [{:keys [id phase] hook-fn :fn :as hook}]
                       (when (= :turn.answer/validate phase)
                         (binding [extension/*current-extension* ext
                                   extension/*current-symbol* nil]
                           (try
                             (let [hit (hook-fn ctx)]
                               (cond
                                 (s/valid? ::extension/answer-validation-reject hit)
                                 (answer-validation-rejection-message hook hit)

                                 (and (map? hit) (:reject hit))
                                 (answer-validation-invalid-return-message ext id hit)))
                             (catch Throwable t
                               (answer-validation-hook-error-message ext id t))))))
                 (or (:ext/hooks ext) [])))
         (answer-validation-extensions environment active-extensions))))))

(defn- iteration-start-hook-hit
  [ext id hit]
  (let [text (when (map? hit) (:text hit))]
    (cond
      (nil? hit) nil

      (and (map? hit)
        (string? text)
        (not (str/blank? text)))
      (cond-> {:id id
               :text text
               :satisfy-with (list 'satisfy-hint! id)}
        (:importance hit) (assoc :importance (:importance hit)))

      :else
      (do
        (tel/log! {:level :warn
                   :id ::iteration-start-hook-invalid-return
                   :data {:ext (:ext/name ext)
                          :hook id
                          :returned hit}}
          "Extension :turn.iteration/start hook returned invalid value; expected nil or {:text ...}")
        nil))))

(defn- iteration-start-hook-error-hit
  [ext id t]
  (tel/log! {:level :warn
             :id ::iteration-start-hook-threw
             :data {:ext (:ext/name ext)
                    :hook id
                    :error (ex-message t)}}
    "Extension :turn.iteration/start hook threw")
  nil)

(defn- collect-iteration-start-hints
  "Run active `:turn.iteration/start` hooks and return model-facing hints."
  [environment active-extensions ctx]
  (let [satisfied (or (some-> environment :satisfied-hints-atom deref) #{})]
    (vec
      (remove (fn [hint] (contains? satisfied (:id hint)))
        (mapcat (fn [ext]
                  (keep (fn [{:keys [id phase] hook-fn :fn}]
                          (when (= :turn.iteration/start phase)
                            (binding [extension/*current-extension* ext
                                      extension/*current-symbol* nil]
                              (try
                                (iteration-start-hook-hit ext id (hook-fn ctx))
                                (catch Throwable t
                                  (iteration-start-hook-error-hit ext id t))))))
                    (or (:ext/hooks ext) [])))
          (answer-validation-extensions environment active-extensions))))))

(defn- session-turn-position
  [environment session-turn-id]
  (or
    (try
      (when-let [session-id (:session-id environment)]
        (some (fn [turn]
                (when (= (str (:id turn)) (str session-turn-id))
                  (:position turn)))
          (persistance/db-list-session-turns (:db-info environment) session-id)))
      (catch Throwable t
        (tel/log! {:level :warn
                   :id ::session-turn-position-failed
                   :data {:session-id (:session-id environment)
                          :session-turn-id session-turn-id
                          :error (ex-message t)}}
          "Could not resolve session turn position for iteration hooks")
        nil))
    1))

(defn- runtime-turn-prefix
  [environment]
  (let [id-s (str (or (some-> (:current-session-turn-id-atom environment) deref)
                    (:environment-id environment)
                    "00000000"))
        prefix (subs id-s 0 (min 8 (count id-s)))]
    (if (re-matches #"(?i)[0-9a-f]{8}" prefix)
      prefix
      "00000000")))

(defn- eval-block-role
  "Block role for the outer lifecycle event — one of the four values
   in the iteration-block role enum (PLAN.md §1.3):
     :answer    the model's final answer to the user
     :tool      any SCI evaluation (tool call OR raw user code)
     :nudge     system-emitted reminders / diagnostics
     :thinking  model reasoning blocks
   The previous `:vis/error` role is gone — errors are derived from
   `:success?` on the envelope (or block-level `:error` slot for
   non-tool evals). Replaces the prior `eval-rendering-kind` fn."
  [result]
  (cond
    (= :answer    (:role result)) :answer
    (= :tool      (:role result)) :tool
    (= :nudge     (:role result)) :nudge
    (= :thinking  (:role result)) :thinking
    (keyword? (:role result))     (:role result)
    (= :vis/answer (:result result))    :answer
    :else :tool))

(defn- eval-envelope
  "Generic canonical envelope for every top-level form that passes
   through the Vis eval pipeline. Tool calls can add nested metadata
   in their returned envelope; this records the outer regular form
   evaluation so plain calls and tool calls share a common block-level
   trace."
  [turn-prefix iteration form-idx form-count result rendering-kind]
  (let [finished      (long (or (:execution-finished-at-ms result)
                              (System/currentTimeMillis)))
        duration      (long (or (:duration-ms result) 0))
        started       (long (or (:execution-started-at-ms result)
                              (max 0 (- finished duration))))
        form-position (inc (long form-idx))]
    {:op             (or (:op result)
                       (case rendering-kind
                         :nudge  :vis/system
                         :answer :vis/answer
                         :sci/eval))
     :started-at-ms  started
     :finished-at-ms finished
     :status         (cond
                       (:timeout? result) :timeout
                       (:error result) :error
                       :else :done)
     :iteration      iteration
     :form-position  form-position
     :form-count     form-count
     :ref            (str "turn/" turn-prefix "/iteration/" iteration "/block/" form-position)
     :timeout?       (boolean (:timeout? result))
     :repaired?      (boolean (:repaired? result))}))

(defn- envelope-timestamps-ordered?
  [envelope]
  (<= (long (:started-at-ms envelope))
    (long (:finished-at-ms envelope))))

(defn- envelope-form-position-valid?
  [envelope]
  (<= (long (:form-position envelope))
    (long (:form-count envelope))))

(defn- envelope-ref-consistent?
  [envelope]
  (let [[_ iteration block] (re-matches #"(?i)^turn/[0-9a-f]{8}/iteration/([1-9][0-9]*)/block/([1-9][0-9]*)$"
                              (:ref envelope))]
    (and iteration
      block
      (= (Long/parseLong iteration) (long (:iteration envelope)))
      (= (Long/parseLong block) (long (:form-position envelope))))))

(defn- envelope-has-no-derived-duration?
  [envelope]
  (not (contains? envelope :duration-ms)))

(defn- envelope-duration-ms
  [envelope]
  (when (and (map? envelope)
          (nat-int? (:started-at-ms envelope))
          (nat-int? (:finished-at-ms envelope)))
    (max 0 (- (long (:finished-at-ms envelope))
             (long (:started-at-ms envelope))))))

(defn- block-duration-ms
  [block]
  (or (envelope-duration-ms (:envelope block)) 0))

(s/def ::id nat-int?)
(s/def ::code string?)
(s/def ::error (s/nilable map?))                       ; structured :error map
(s/def ::timeout? (s/nilable boolean?))
(s/def ::repaired? (s/nilable boolean?))
(s/def ::comment string?)
(s/def ::op #{:sci/eval :edamame/parse :vis/guard :vis/system :vis/answer})
(s/def ::status #{:done :error :timeout})
(s/def ::iteration pos-int?)
(s/def ::form-position pos-int?)
(s/def ::form-count pos-int?)
(s/def ::started-at-ms nat-int?)
(s/def ::finished-at-ms nat-int?)
(s/def ::ref
  (s/and string?
    #(re-matches #"(?i)^turn/[0-9a-f]{8}/iteration/[1-9][0-9]*/block/[1-9][0-9]*$" %)))
(s/def ::block-envelope
  (s/and
    (s/keys :req-un [::op ::status ::iteration ::form-position ::form-count
                     ::started-at-ms ::finished-at-ms ::ref]
      :opt-un [::timeout? ::repaired?])
    envelope-timestamps-ordered?
    envelope-form-position-valid?
    envelope-ref-consistent?
    envelope-has-no-derived-duration?))
(s/def ::envelope ::block-envelope)
(s/def ::iteration-block
  (s/keys :req-un [::id ::code ::error ::envelope]
    :opt-un [::result ::timeout? ::repaired? ::comment]))

(defn validate-iteration-blocks!
  "Fail fast if a stored/evaluated block lost mandatory envelope.
   Tool-result envelopes enforce their nested info separately;
   this spec enforces the outer block-level eval envelope for every
   regular top-level form."
  [blocks]
  (let [blocks (mapv (fn [block]
                       (cond-> block
                         (contains? block :error)
                         (update :error op-error
                           {:code (:code block)
                            :phase (get-in block [:envelope :op])})))
                 (or blocks []))]
    (doseq [block blocks]
      (when-not (s/valid? ::iteration-block block)
        (throw (ex-info "Invalid iteration block"
                 {:type :vis/invalid-iteration-block
                  :block block
                  :explain (s/explain-data ::iteration-block block)}))))
    blocks))

;; ---------------------------------------------------------------------------
;; run-iteration
;; ---------------------------------------------------------------------------

(defn- token-number
  [tokens ks]
  (some (fn [k]
          (let [v (get tokens k)]
            (when (number? v) v)))
    ks))

(defn- ask-result->api-usage
  [{:keys [tokens]}]
  {:prompt_tokens (long (or (token-number tokens [:input]) 0))
   :completion_tokens (long (or (token-number tokens [:output]) 0))
   :completion_tokens_details {:reasoning_tokens (long (or (token-number tokens [:reasoning]) 0))}
   :prompt_tokens_details {:cached_tokens (long (or (token-number tokens [:cached :cached-input :input-cached]) 0))
                           :cache_creation_tokens (long (or (token-number tokens [:cache-created
                                                                                  :cache-created-input
                                                                                  :cache-creation
                                                                                  :cache-write
                                                                                  :cache_creation])
                                                          0))}})

(defn- reasoning-effort-configurable?
  "True when a model accepts a caller-selected reasoning effort.

   `:reasoning?` means the model can produce reasoning/thinking text.
   It does NOT imply that Vis may tune that thinking depth. Z.ai GLM
   thinking is binary/preserved-thinking only, so keep the stream visible
   but do not send `:reasoning` levels to svar. Providers can opt out
   explicitly with `:reasoning-effort? false`."
  [resolved-model]
  (and (boolean (:reasoning? resolved-model))
    (not= false (:reasoning-effort? resolved-model))
    (not= :zai-thinking (:reasoning-style resolved-model))))

(defn- ^:private replay-reasoning-chars
  "Total `:thinking-signature` (or `:thinking` fallback) char count for
   the canonical thinking blocks on `assistant-message`. 0 when nil.
   The signature field is what svar's wire serializer hoists into
   `reasoning_content` — that is what counts against the budget."
  [assistant-message]
  (->> (get assistant-message :content)
    (filter (fn [b] (= "thinking" (:type b))))
    (map (fn [b] (count (or (:thinking-signature b) (:thinking b) ""))))
    (reduce + 0)))

(defn- preserved-thinking-replay-messages
  "Provider-agnostic preserved-thinking replay. Returns every compatible
   `:assistant-message` from `trailer-iters` in arrival order.

   Why every message, not just the last:
     - Z.ai / GLM-5.x preserved thinking (`clear_thinking: false`) keeps
       reasoning_content across assistant turns only when each prior
       assistant message echoes the model's full reasoning back. Drop a
       step and GLM either re-derives the same scratch state at every
       iteration (observed: session 3102ad16, 26 iterations re-reading
       the same file with `cached_tokens` pinned at 2368) or starts to
       hallucinate that an earlier conclusion is still live.
     - Anthropic extended thinking signs each block with an HMAC and
       refuses replay if the chain is broken; sending only the last
       block fails signature validation as soon as the model produced
       more than one block since the user message.
     - OpenAI Responses encrypted reasoning items must replay in order
       — the next call rejects a single isolated item with
       'reasoning without following item'.

   The earlier conservative 'last-only' policy (rationale comment
   referenced session a9389e1d) was tuned for pre-`clear_thinking`
   GLM-4.6 where any replay contaminated the next step. The modern
   GLM-5.1 + Anthropic 4.x + OpenAI Responses contract all want full
   chains; pi-ai's `transform-messages.js` follows the same approach
   (every prior assistant `thinking` block preserved when same model).

   `compatible-preserved-thinking-trailer-iters` upstream has already
   filtered iterations to (a) same provider+model as the target call,
   (b) opted in via `:preserved-thinking/replay?` (live-turn freshly
   produced iterations), (c) carrying a valid `:assistant-message`,
   (d) signature-compatible with the replay target. Anything that
   reaches this fn is safe to replay verbatim.

   The wire serializer for the active model translates each canonical
   message to its native shape; iteration-loop never branches on
   provider."
  [trailer-iters]
  (let [msgs (vec (keep #(some-> % second :assistant-message) trailer-iters))]
    (when (seq msgs)
      ;; Keep this call so oversized reasoning chains are observable to
      ;; future budget instrumentation. Sum across the full chain instead
      ;; of just the latest step — budget watchers care about cumulative
      ;; replay size, not single-step size.
      (doseq [m msgs] (replay-reasoning-chars m)))
    msgs))

(defn- replay-context
  "Small identity map for deciding whether preserved-thinking can be
   replayed into the next provider call. Provider-native thinking
   signatures are not portable: z.ai stores reasoning text under
   `:thinking-signature`, Anthropic expects an HMAC signature, and
   OpenAI Responses stores a JSON reasoning item. Replaying across a
   provider/model switch corrupts the next request (Anthropic 400:
   invalid signature in thinking block)."
  [resolved-model]
  {:provider (:provider resolved-model)
   :model    (some-> (:name resolved-model) str)})

(defn- anthropic-replay-context?
  [{:keys [provider model]}]
  (or (boolean (re-find #"(?i)anthropic" (str provider)))
    (boolean (re-find #"(?i)^claude" (str model)))))

(defn- thinking-blocks
  [assistant-message]
  (filterv #(= "thinking" (:type %)) (:content assistant-message)))

(defn- anthropic-invalid-thinking-replay-block?
  "True for poisoned Anthropic replay state. In bad historical rows,
   Vis recorded a fallback z.ai response as Anthropic; z.ai stores raw
   reasoning text as `:thinking-signature`, so signature == thinking.
   Anthropic signatures are opaque HMACs and must not equal prose."
  [block]
  (let [thinking  (:thinking block)
        signature (:thinking-signature block)]
    (and (string? thinking)
      (not (str/blank? thinking))
      (string? signature)
      (= thinking signature))))

(defn- assistant-message-compatible-with-replay-target?
  [target assistant-message]
  (not (and (anthropic-replay-context? target)
         (some anthropic-invalid-thinking-replay-block?
           (thinking-blocks assistant-message)))))

(defn- actual-llm-provider
  "Provider that actually served an ask-result. svar may route/fallback
   inside ask-code!, so prefer routed metadata over Vis' pre-call guess."
  [resolved-model ask-result]
  (or (:routed/provider-id ask-result)
    (:provider resolved-model)))

(defn- actual-llm-model
  "Model that actually served an ask-result. See `actual-llm-provider`."
  [resolved-model ask-result]
  (or (:routed/model ask-result)
    (some-> (:name resolved-model) str)))

(defn- llm-id
  [provider model]
  (cond-> {}
    provider (assoc :provider (name (keyword provider)))
    model    (assoc :model (str model))))

(defn- llm-routing-summary
  [selected-model iteration-result]
  (let [selected (llm-id (:provider selected-model) (some-> (:name selected-model) str))
        actual   (llm-id (or (:llm-provider iteration-result) (:provider selected-model))
                   (or (:llm-model iteration-result) (some-> (:name selected-model) str)))
        routing-trace (vec (or (:llm-routing-trace iteration-result) []))]
    (cond-> {:selected selected
             :actual   actual
             :fallback? (boolean
                          (or (not= selected actual)
                            (some #(not= :llm.routing/provider-retry (:event/type %)) routing-trace)))}
      (seq routing-trace) (assoc :trace routing-trace))))

(defn- attach-llm-routing-summary
  [result selected-model iteration-result]
  (let [routing  (llm-routing-summary selected-model iteration-result)
        actual   (:actual routing)
        selected (:selected routing)]
    (cond-> (assoc result
              :provider (:provider actual)
              :model    (:model actual)
              :llm-selected selected
              :llm-actual actual
              :llm-fallback? (:fallback? routing))
      (seq (:trace routing))
      (assoc :llm-routing-trace (:trace routing))
      (:cost result)
      (update :cost merge (select-keys actual [:provider :model])))))

(defn- compatible-preserved-thinking-trailer-iters
  "Keep only iterations whose provider-native thinking may be replayed into
   the next provider call.

   Cross-turn trailer seeds explicitly carry
   `:preserved-thinking/replay? false`; those iterations remain visible in
   persisted iterations as durable evidence, but their opaque provider-native thinking
   state is not replayed into a different user turn. Within a live turn,
   freshly-produced iterations opt in by setting the flag to true. Historical
   in-memory test fixtures that omit the flag are treated as replayable for
   backward compatibility."
  [trailer-iters target]
  (let [{target-provider :provider target-model :model} target]
    (filterv (fn [[_ {:keys [assistant-message llm-provider llm-model]
                      replay? :preserved-thinking/replay?}]]
               (and (not= false replay?)
                 assistant-message
                 (= target-provider llm-provider)
                 (= target-model llm-model)
                 (assistant-message-compatible-with-replay-target?
                   target assistant-message)))
      (or trailer-iters []))))

(defn- append-preserved-thinking-replay
  "Appends svar's canonical assistant-replay messages from prior
   iterations to the END of `messages` (R3 hybrid shape).

   Replays are scoped to the exact provider/model that produced them.
   This is mandatory because preserved-thinking payloads are
   provider-native, despite sharing svar's canonical map shape:
   Anthropic `:thinking-signature` is an HMAC, z.ai's is raw
   reasoning text, and OpenAI Responses' is a JSON reasoning item.
   Crossing those streams reproduces Anthropic's
   `Invalid signature in thinking block` HTTP 400."
  ([messages trailer-iters]
   (append-preserved-thinking-replay messages trailer-iters nil))
  ([messages trailer-iters target]
   (let [messages* (vec (or messages []))
         compatible-iters (if target
                            (compatible-preserved-thinking-trailer-iters trailer-iters target)
                            (or trailer-iters []))
         replays   (preserved-thinking-replay-messages compatible-iters)]
     (cond-> messages*
       (seq replays) (into replays)))))

(defn run-iteration
  "Runs a single RLM iteration: ask! -> check final -> execute code.
   Returns map with :thinking :blocks :final-result :api-usage etc."
  [environment messages & [{:keys [routing iteration reasoning-level resolved-model on-chunk extra-body llm-headers active-extensions answer-validation-context]}]]
  (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :run-iteration})]
    (let [iteration-position (inc (long (or iteration 0)))
          turn-prefix (runtime-turn-prefix environment)
          form-ref (fn [idx]
                     (str "turn/" turn-prefix "/iteration/" iteration-position "/block/" (inc idx)))
          effective-reasoning (when (and (some? reasoning-level)
                                      (reasoning-effort-configurable? resolved-model))
                                (or (normalize-reasoning-level reasoning-level)
                                  (throw (ex-info "Invalid :reasoning-level."
                                           {:type :vis/invalid-reasoning-level
                                            :got reasoning-level}))))
          ;; Reset the per-environment answer-atom before this iteration.
          ;; The SCI sandbox's `(done "...")` fn `reset!`s it during
          ;; code evaluation; we read it back after all forms run.
          answer-atom (or (:answer-atom environment)
                        (throw (ex-info "environment missing :answer-atom"
                                 {:type :vis/missing-answer-atom})))
          _ (reset! answer-atom nil)
          ;; Form-index pointer the executed-mapv reset!s before each
          ;; expression's eval so `answer-fn` can stamp `:form-idx` on
          ;; the answer-atom payload. Pairs with the discard check
          ;; below: an answer is gated only by the form that emitted
          ;; it, not by sibling forms.
          current-form-idx-atom (or (:current-form-idx-atom environment)
                                  (throw (ex-info "environment missing :current-form-idx-atom"
                                           {:type :vis/missing-current-form-idx-atom})))
          _ (reset! current-form-idx-atom nil)
          ;; Stream reasoning chunks to the TUI while the LLM is
          ;; thinking. Every chunk carries `:phase` - consumers
          ;; dispatch on it. Phases:
          ;;   :reasoning      - LLM streaming reasoning text
          ;;   :form-start     - one form started evaluating (per-form)
          ;;   :form-result    - one form finished evaluating (per-form)
          ;;   :iteration-final - iteration complete (final-result
          ;;                      or normal end-of-iteration marker)
          ;;
          ;; Reasoning DELTA contract: providers (via svar) emit `:reasoning`
          ;; as the FULL accumulated reasoning text on every SSE tick.
          ;; Forwarding that verbatim makes append-only consumers (CLI
          ;; trace, JSON/EDN trace streams) re-emit the entire growing
          ;; block on every tick — the screenshotted "sending and sending"
          ;; bug. We close that here at the producer: track per-iteration
          ;; accumulated length, compute `:delta` (just the new tail) and
          ;; ship it alongside `:thinking` (still the full accumulated text
          ;; for redraw-style consumers like the TUI timeline). Consumers
          ;; that want append-only streaming append `:delta`; the others
          ;; ignore it and read `:thinking` as before.
          reasoning-len-volatile (volatile! 0)
          streaming-fn (when on-chunk
                         (fn [{:keys [reasoning done?] :as chunk}]
                           (cond
                             (:event/type chunk)
                             (on-chunk {:phase           :provider-fallback
                                        :iteration-count iteration-position
                                        :event           chunk})

                             (or (some? reasoning) done?)
                             (let [thinking (some-> reasoning str)
                                   prev-len (long @reasoning-len-volatile)
                                   cur-len  (long (count (or thinking "")))
                                   ;; If the accumulator shrank (rare:
                                   ;; provider reset mid-stream) treat the
                                   ;; whole new text as fresh delta.
                                   delta (cond
                                           (nil? thinking)        nil
                                           (< cur-len prev-len)   thinking
                                           (= cur-len prev-len)   ""
                                           :else                  (subs thinking prev-len))]
                               (vreset! reasoning-len-volatile cur-len)
                               (on-chunk {:phase           :reasoning
                                          :iteration-count iteration-position
                                          :thinking  thinking
                                          :delta     delta
                                          :done?     (boolean done?)})))))
          copilot-initiator (copilot-initiator-for-iteration iteration)
          effective-llm-headers (not-empty
                                  (merge (copilot-llm-headers resolved-model copilot-initiator)
                                    llm-headers))
          provider-started-at-ms (System/currentTimeMillis)
          _ (when on-chunk
              (on-chunk {:phase :provider-call
                         :iteration iteration-position
                         :started-at-ms provider-started-at-ms}))
          provider-start-ns (System/nanoTime)
          ask-result-raw (binding [svar-llm/*log-context* (assoc svar-llm/*log-context*
                                                            :session-turn-id (:environment-id environment)
                                                            :iteration iteration-position)]
                           (svar/ask-code! (:router environment)
                             (with-default-ask-code-idle-timeout
                               (cond-> {:lang     "clojure"
                                        :messages messages
                                        :routing  (or routing {})
                                        :check-context? true
                                        :preserved-thinking? true}
                                 effective-reasoning  (assoc :reasoning effective-reasoning)
                                 streaming-fn         (assoc :on-chunk streaming-fn)
                                 effective-llm-headers (assoc :llm-headers effective-llm-headers)
                                 extra-body           (assoc :extra-body extra-body)))))
          code-observation (ask-code-block-observation ask-result-raw)
          provider-duration-ms (elapsed-ms provider-start-ns)
          _ (log-stage! :provider-call/stop iteration
              (merge {:duration-ms provider-duration-ms
                      :raw-length (count (or (:raw ask-result-raw) ""))
                      :tokens (:tokens ask-result-raw)
                      :fallback? (boolean (some #(not= :llm.routing/provider-retry (:event/type %)) (:routed/trace ask-result-raw)))}
                code-observation))
          parse-started-at-ms (System/currentTimeMillis)
          _ (when on-chunk
              (on-chunk {:phase :response-parse
                         :status :start
                         :iteration iteration-position
                         :started-at-ms parse-started-at-ms
                         :provider-duration-ms provider-duration-ms
                         :raw-length (count (or (:raw ask-result-raw) ""))
                         :block-count (:block-count code-observation)
                         :code-observation code-observation}))
          ask-result ask-result-raw
          model-reasoning (:reasoning ask-result)
          thinking model-reasoning
          _ (log-stage! :llm-response iteration
              (merge {:has-reasoning (some? model-reasoning)
                      :raw-length    (count (or (:raw ask-result) ""))
                      :duration-ms   (:duration-ms ask-result)
                      :provider-duration-ms provider-duration-ms
                      :tokens        (:tokens ask-result)
                      :thinking      thinking}
                code-observation))
          api-usage (ask-result->api-usage ask-result)
          ;; svar/ask-code! returns the per-block vector in `:blocks`
          ;; (single source of truth; the legacy `:result` concatenated
          ;; string was removed in svar v0.5.3). One block → one
          ;; code-entry; SCI evaluates each entry as a single chunk.
          blocks (vec (:blocks ask-result))
          preflight-start-ns (System/nanoTime)
          preflight-result-raw (code-entries-preflight iteration-position blocks)
          empty-code-observation-error (when (:empty-code-preflight-error preflight-result-raw)
                                         (empty-code-error-with-observation
                                           (:empty-code-preflight-error preflight-result-raw)
                                           ask-result))
          preflight-result (cond-> preflight-result-raw
                             empty-code-observation-error
                             (assoc :empty-code-preflight-error empty-code-observation-error
                               :code-entries [{:expr ""
                                               :vis/preflight-error empty-code-observation-error}]))
          preflight-duration-ms (elapsed-ms preflight-start-ns)
          {:keys [code-entries normalized-code]} preflight-result
          _ (log-stage! :response-preflight/stop iteration
              (merge {:duration-ms preflight-duration-ms
                      :code-length (count normalized-code)
                      :forms (count code-entries)
                      :raw-fence-preflight? (boolean (:raw-fence-preflight-error preflight-result))}
                code-observation))
          _ (when on-chunk
              (on-chunk {:phase :response-parse
                         :status :done
                         :iteration iteration-position
                         :duration-ms preflight-duration-ms
                         :code-length (count normalized-code)
                         :forms (count code-entries)
                         :code-observation code-observation}))
          direct-answer-entry (when (= 1 (count code-entries))
                                (first code-entries))
          final-answer-preflight-error
          (when (and direct-answer-entry
                  (not (:vis/preflight-error direct-answer-entry))
                  (direct-answer-entry? direct-answer-entry)
                  (not (form-contains-needs-input-call? direct-answer-entry)))
            ;; Pass prior-iteration context so the structural gate sees
            ;; evidence-prior? correctly. Without this, a clean answer-only
            ;; iteration after several probe iterations is falsely rejected
            ;; as "no evidence for this turn yet" because the gate's
            ;; `previous-iterations` defaults to nil. That bug burns 5-7+
            ;; iterations on simple tasks (see autoresearch fw-005 trace).
            (final-answer-gate-error environment iteration-position [] nil
              active-extensions
              (assoc answer-validation-context
                :position 0
                :code-entries code-entries)))
          code-entries (if final-answer-preflight-error
                         [{:expr "(vis/preflight-error :final-answer-gate)"
                           :vis/preflight-error final-answer-preflight-error}]
                         code-entries)
          suppress-form-start? (or (some :vis/preflight-error code-entries)
                                 final-answer-preflight-error)
          total-blocks (count code-entries)
          executed (mapv (fn [idx {:keys [expr parse-error render-segments]
                                   :vis/keys [preflight-error structurally-silent?]
                                   form-repaired? :repaired?
                                   :as entry}]
                           (log-stage! :code-exec iteration
                             {:idx (inc idx) :total total-blocks :code expr})
                           (when (and on-chunk
                                   (not suppress-form-start?)
                                   (not (session-title-meta-form? entry))
                                   (not structurally-silent?))
                             (on-chunk {:phase           :form-start
                                        :iteration-count iteration-position
                                        :position        idx
                                        :count           total-blocks
                                        :scope           (form-ref idx)
                                        :code            expr
                                        :render-segments render-segments
                                        :vis/structurally-silent? (boolean structurally-silent?)
                                        :started-at-ms   (System/currentTimeMillis)}))
                           ;; Stamp form-idx BEFORE eval so any
                           ;; `(done ...)` call inside this form
                           ;; captures the right index on the
                           ;; answer-atom payload.
                           (reset! current-form-idx-atom idx)
                           (let [scope (form-ref idx)
                                 raw-result (cond
                                              preflight-error
                                              {:result nil
                                               :error (op-error preflight-error
                                                        {:code expr :phase :vis/preflight})
                                               :duration-ms 0
                                               :op :vis/guard}
                                              parse-error
                                              {:result nil
                                               :error (op-error (str "Parse error: " parse-error)
                                                        {:code expr :phase :edamame/parse})
                                               :duration-ms 0
                                               :op :edamame/parse}
                                              :else
                                              (if-let [err (literal-code-block-error expr)]
                                                {:result nil
                                                 :error (op-error err {:code expr :phase :vis/guard})
                                                 :duration-ms 0
                                                 :op :vis/guard}
                                                (let [tool-event-fn (when (and on-chunk
                                                                            (not suppress-form-start?)
                                                                            (not structurally-silent?))
                                                                      (fn [tool-event]
                                                                        (on-chunk {:phase           :tool-start
                                                                                   :iteration-count iteration-position
                                                                                   :position        idx
                                                                                   :count           total-blocks
                                                                                   :scope           scope
                                                                                   :code            expr
                                                                                   :render-segments render-segments
                                                                                   :vis/structurally-silent? (boolean structurally-silent?)
                                                                                   :tool-event     tool-event})))
                                                      r (if tool-event-fn
                                                          (execute-code environment expr :tool-event-fn tool-event-fn)
                                                          (execute-code environment expr))]
                                                  (log-stage! :code-result iteration
                                                    {:idx (inc idx) :total total-blocks
                                                     :duration-ms (:duration-ms r)
                                                     :error (:error r) :timeout? (:timeout? r) :result (:result r)})
                                                  r)))
                                 ;; Carry parinfer's whole-source
                                 ;; rebalance flag into the per-form
                                 ;; result. `execute-code` may also
                                 ;; set `:repaired?` (extension hook
                                 ;; rescue); both paths converge on
                                 ;; the same flag for the channel.
                                 result (cond-> raw-result
                                          form-repaired? (assoc :repaired? true)
                                          ;; If the merged-fence source produced ANY error
                                          ;; (parse, lint, eval, timeout), attach the
                                          ;; single-fence rule reminder so the model knows
                                          ;; the merge itself is a candidate root cause.
                                          (and (:multi-fence-merged? entry)
                                            (:error raw-result))
                                          (update :error attach-multi-fence-hint entry))
                                 display-result (def-display-result environment expr result)
                                 ;; def-display-result is now a pass-through; kept on the
                                 ;; call path so future display-tweaks have a single seam.

                                 block-role (eval-block-role display-result)
                                 envelope (eval-envelope turn-prefix iteration-position idx total-blocks display-result block-role)
                                 result* (assoc display-result
                                           :envelope envelope
                                           :role block-role)]
                             ;; Per-form streaming chunk (:phase
                             ;; :form-result). Fires the moment a
                             ;; form lands so the channel can render
                             ;; per-form results incrementally instead
                             ;; of waiting for the whole batch. Same
                             ;; envelope on success and error -
                             ;; consumers branch on `:error nil?`,
                             ;; not on shape.
                             ;;
                             ;; Preflight rejections are MODEL-FACING
                             ;; only: they teach the model to correct
                             ;; its next iteration, but the user does
                             ;; not need to see the synthetic error
                             ;; box. Suppress the live chunk when the
                             ;; result came from a preflight gate
                             ;; (mirrors `suppress-form-start?`).
                             (when (and on-chunk (not preflight-error))
                               (on-chunk {:phase             :form-result
                                          :iteration-count   iteration-position
                                          :position          idx
                                          :count             total-blocks
                                          :scope             scope
                                          :code              expr
                                          :render-segments   render-segments
                                          :vis/structurally-silent? (boolean structurally-silent?)
                                          :result            (:result result*)
                                          :channel           (:channel result*)
                                          :error             (:error result*)
                                          :envelope          (:envelope result*)
                                          :role              (:role result*)
                                          ;; :silent? is the channel-facing hide flag. Now the union of:
                                          ;;   - SCI runtime `:vis/silent` sentinel (legacy host primitives)
                                          ;;   - block-level structurally-silent? (block contains only
                                          ;;     answer / title forms; no useful code segments)
                                          ;; Mixed blocks (`(def x 1)` alongside `(done …)`)
                                          ;; are NOT silent; the channel reads :render-segments and
                                          ;; hides only the structural sub-forms.
                                          :silent?     (boolean (or (:vis/silent result*)
                                                                  (= :vis/silent (:result result*))
                                                                  structurally-silent?))
                                          :timeout?          (boolean (:timeout? result*))
                                          :repaired?         (boolean (:repaired? result*))}))
                             {:block expr
                              :result result*
                              :render-segments render-segments
                              :vis/structurally-silent? (boolean structurally-silent?)}))
                     (range) code-entries)
          code-blocks    (mapv :block executed)
          block-results  (mapv :result executed)
          block-segments (mapv :render-segments executed)
          block-silents  (mapv :vis/structurally-silent? executed)
          ;; Preflight gate → synthetic block carries `:vis/preflight? true`
          ;; so channels can suppress the model-facing-only error box. Keep
          ;; the block in the persisted/trailer stream so the model still
          ;; reads the failure on its next iteration.
          preflight-by-idx (zipmap (range) (map (fn [{:vis/keys [preflight-error]}]
                                                  (boolean preflight-error))
                                             code-entries))
          blocks (validate-iteration-blocks!
                   (mapv (fn [idx code result segments structurally-silent?]
                           (cond-> {:id idx
                                    :code code
                                    :result (:result result)
                                    :channel (:channel result)
                                    :error (op-error (:error result) {:code code :phase (get-in result [:envelope :op])})
                                    :envelope (:envelope result)
                                    :role (:role result)
                                    :timeout? (:timeout? result)
                                    :repaired? (:repaired? result)
                                    ;; Per-block def-sink: every (def ...) the
                                    ;; SCI sandbox evaluated in this block. The
                                    ;; iteration writer concats sinks across
                                    ;; blocks and drives definition_state from
                                    ;; the result. Replaces the legacy
                                    ;; parse-source path.
                                    :def-sink (vec (:def-sink result))
                                    ;; Per-block resolve-symbol* LRU stamps:
                                    ;; symbol-name -> current-turn-pos for every
                                    ;; symbol the SCI hook saw resolve during
                                    ;; this block's eval. Iteration writer
                                    ;; merges into the long-lived per-env LRU.
                                    :lru (or (:lru result) {})
                                    ;; Per-form outcomes: one entry per parsed
                                    ;; top-level form with {:source :result :error}.
                                    ;; The REPL trailer renders these in order so
                                    ;; the model sees every form's value, not
                                    ;; just the last one. Empty when the parser
                                    ;; rejected the source (fell back to whole-
                                    ;; block eval).
                                    :forms (vec (:forms result))
                                    ;; If the engine auto-repaired delimiter
                                    ;; mistakes (parinferish) before eval, the
                                    ;; repaired source flows here so the trailer
                                    ;; can disclose the diff and the model can
                                    ;; correct itself if the repair was wrong.
                                    :repaired-source (:repaired-source result)}
                             ;; Per-form render breakdown for channel display.
                             ;; Channels that read :render-segments hide
                             ;; (done …) / (set-session-title! …)
                             ;; forms while keeping the prelude visible.
                             ;; Legacy channels that only read :code fall
                             ;; back to the full block source.
                             (seq segments) (assoc :render-segments segments)
                             structurally-silent? (assoc :vis/structurally-silent? true)
                             (:vis/silent result) (assoc :vis/silent true)
                             (get preflight-by-idx idx) (assoc :vis/preflight? true)))
                     (range) code-blocks block-results block-segments block-silents))
          silent-form-idxs (into #{}
                             (keep-indexed (fn [idx block]
                                             (when (or (:vis/silent block)
                                                     (= :vis/silent (:result block))
                                                     (:vis/structurally-silent? block))
                                               idx)))
                             blocks)]
      (if-let [{value :value form-idx :position} @answer-atom]
          ;; FINAL path: model called `(done "...")` during this
          ;; iteration. Atom payload is `{:value :form-idx}`. The
          ;; form-scoped error gate fires if the answer-bearing form's
          ;; own evaluation errored anyway
          ;;      (e.g. `(do (v/edit ...throws...) (done "x"))` -
          ;;      the form had inner work that crashed), the answer
          ;; answer is discarded with the form's own error. Sibling
          ;; forms before the answer-form may error freely; that
          ;; doesn't gate termination.
          ;;
          ;; `resolved-model` is a MAP - `{:name str :provider kw
          ;; :reasoning? bool}` - not a string. Persisting `(str
          ;; resolved-model)` would land a stringified map in
          ;; `iteration.llm_model`; surface `:name` and `:provider`
          ;; separately so both columns get clean values.
        ;; `value` is already canonical `[:ir & nodes]` (or a
        ;; needs-input map) - `answer-fn` ran `render/->ast` at the
        ;; SCI boundary. Persist the IR as-is; channels render at
        ;; their boundary via `:channel/messages-renderer-fn`.
        (let [final-answer    value
              total-forms     (count code-entries)
              own-form-error  (answer-form-error block-results form-idx)
              gate-error      (when (nil? own-form-error)
                                (final-answer-gate-error environment iteration-position blocks value active-extensions
                                  (assoc answer-validation-context
                                    :position form-idx
                                    :code-entries code-entries)))
              validation-error (cond
                                 own-form-error
                                 (error/final-answer-code-error-message own-form-error)
                                 gate-error
                                 gate-error)
              ;; Surface the validation error on the answer-bearing
              ;; form's row so the model sees \"my (done ...) was
              ;; rejected because...\" right next to its own code.
              blocks*     (cond-> blocks
                            (and validation-error form-idx
                              (< form-idx (count blocks))
                              (nil? (get-in blocks [form-idx :error])))
                            (assoc-in [form-idx :error]
                              (op-error validation-error
                                {:code (get code-blocks form-idx)
                                 :phase :vis/final-answer-validation})))
              ;; Re-emit a `:phase :form-result` chunk for the
              ;; answer-bearing form when the validator attached an
              ;; error post-hoc. The original `:form-result` chunk
              ;; fired the moment the form returned `:vis/answer`
              ;; with `:error nil`; without this re-emit the TUI
              ;; tracker renders the rejected answer as a succeeded
              ;; form, hiding the validation error from the user.
              _ (when (and validation-error form-idx on-chunk
                        (< form-idx (count blocks*)))
                  (let [b (get blocks* form-idx)]
                    (on-chunk {:phase             :form-result
                               :iteration-count   iteration-position
                               :position          form-idx
                               :count             total-forms
                               :scope             (form-ref form-idx)
                               :code              (:code b)
                               :render-segments   (:render-segments b)
                               :vis/structurally-silent? (boolean (:vis/structurally-silent? b))
                               :result            (:result b)
                               :error             (:error b)
                               :envelope          (:envelope b)
                               :role              (:role b)
                               :silent?     (boolean (or (:vis/silent b)
                                                       (= :vis/silent (:result b))
                                                       (:vis/structurally-silent? b)))
                               :timeout?          (boolean (:timeout? b))
                               :repaired?         (boolean (:repaired? b))})))
              model-name       (actual-llm-model resolved-model ask-result)
              provider         (actual-llm-provider resolved-model ask-result)]
          (if validation-error
            {:thinking thinking
             :blocks (or (seq blocks*)
                       [{:id 0 :code "(final-answer-validation)"
                         :result nil
                         :error (op-error validation-error
                                  {:code "(final-answer-validation)"
                                   :phase :vis/final-answer-validation})}])
             :final-result nil :api-usage api-usage
             :duration-ms (or (:duration-ms ask-result) 0)
             :silent-form-idxs silent-form-idxs
             :llm-messages messages :llm-provider provider :llm-model model-name
             :llm-selected-provider (:provider resolved-model)
             :llm-selected-model (some-> (:name resolved-model) str)
             :llm-actual-provider provider
             :llm-actual-model model-name
             :llm-routing-trace (:routed/trace ask-result)
             :llm-raw-response (:raw ask-result)
             :llm-executable-blocks (:blocks ask-result)
             :llm-returned-empty-code? (empty? blocks)
             :assistant-message (:assistant-message ask-result)}
            (let [final-answer* (append-runtime-appendices environment final-answer value)]
              {:thinking thinking
               :blocks blocks
               :final-result {:final?           true
                              :answer           final-answer*
                              ;; Index of the form that called
                            ;; `(done ...)`. Channels use this to
                            ;; ELIDE the answer-bearing form from the
                            ;; per-iteration code trace (the channel
                            ;; renders the answer text below; showing
                            ;; `(done "...")` above it is
                              ;; redundant prose-as-code).
                              :answer-position  form-idx}
               :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)
               :silent-form-idxs silent-form-idxs
               :llm-messages messages :llm-provider provider :llm-model model-name
               :llm-selected-provider (:provider resolved-model)
               :llm-selected-model (some-> (:name resolved-model) str)
               :llm-actual-provider provider
               :llm-actual-model model-name
               :llm-routing-trace (:routed/trace ask-result)
               :llm-raw-response (:raw ask-result)
               :llm-executable-blocks (:blocks ask-result)
               :llm-returned-empty-code? (empty? blocks)
               :assistant-message (:assistant-message ask-result)})))
          ;; Normal path
        {:thinking thinking
         :blocks blocks
         :final-result nil :api-usage api-usage
         :duration-ms (or (:duration-ms ask-result) 0)
         :silent-form-idxs silent-form-idxs
         :llm-messages messages
         :llm-provider (actual-llm-provider resolved-model ask-result)
         :llm-model    (actual-llm-model resolved-model ask-result)
         :llm-selected-provider (:provider resolved-model)
         :llm-selected-model (some-> (:name resolved-model) str)
         :llm-actual-provider (actual-llm-provider resolved-model ask-result)
         :llm-actual-model (actual-llm-model resolved-model ask-result)
         :llm-routing-trace (:routed/trace ask-result)
         :llm-raw-response (:raw ask-result)
         :llm-executable-blocks (:blocks ask-result)
         :llm-returned-empty-code? (empty? blocks)
         :assistant-message (:assistant-message ask-result)}))))

;; =============================================================================
;; Multi-iteration turn engine
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Core helpers
;; -----------------------------------------------------------------------------

(defn- stream-output-overflow?
  [err]
  (let [data (:data err)]
    (and (= :svar.core/stream-incomplete (:type data))
      (= "max_output_tokens" (str (:reason data))))))

(def ^:private stream-truncated-types
  "Error types that indicate the provider dropped the SSE stream before
   emitting content or the terminal `[DONE]` marker. These are transient
   server-side failures — the model never saw the request fail, so
   retrying the identical call is safe and cheap."
  #{:svar.core/stream-truncated})

(def ^:private MAX_STREAM_TRUNCATED_RETRIES
  "Maximum transparent retries for stream-truncated errors per iteration.
   Each retry re-sends the identical messages; reasoning starts fresh on
   the provider side. 2 retries = 3 total attempts."
  2)

(defn- stream-truncated-error?
  "True when an exception represents a provider stream that was cut
   before any content arrived. Safe to retry transparently."
  [^Throwable e]
  (let [data (ex-data e)]
    (and (contains? stream-truncated-types (:type data))
      (zero? (or (:content-acc-len data) 0)))))

(defn- llm-provider-error-context
  [iteration iteration-error-data]
  (let [output-overflow? (stream-output-overflow? iteration-error-data)
        message (if output-overflow?
                  "Provider stopped the response as incomplete because output budget was exhausted (max_output_tokens)."
                  (str "LLM call failed: " (:message iteration-error-data)))
        hint (if output-overflow?
               "Do not continue the broad strategy. Use a compact path now: one small probe if essential, otherwise stop, report the exact impediment, and ask for confirmation before more changes. Avoid dumping large maps, file contents, diffs, or repeated diagnostics."
               "Adjust your approach or finish with `(done ...)` using only observed evidence.")]
    (cond-> {:phase     :llm-provider/generate
             :type      (if output-overflow?
                          :llm-provider/output-budget-exhausted
                          :llm-provider/call-failed)
             :iteration (inc (long iteration))
             :message   message
             :hint      hint}
      (and (not output-overflow?) (:type iteration-error-data))
      (assoc :source-type (:type iteration-error-data)))))

(defn- iteration-error-feedback
  [iteration iteration-error-data user-request]
  (let [llm-provider-error (llm-provider-error-context iteration iteration-error-data)]
    (str "[Iteration " (:iteration llm-provider-error) "]\n"
      ";; llm-provider-error =\n"
      (pr-str llm-provider-error)
      "\n"
      (when (stream-output-overflow? iteration-error-data)
        (str "Original request: " user-request)))))

(def ^:private CHAT_ERROR_BODY_RENDER_CHARS
  "Cap on raw upstream HTTP body chars surfaced in the chat error
   bubble. Long enough that Anthropic / OpenAI / z.ai full JSON error
   envelopes (`{\"type\":\"error\",\"error\":{...},\"request_id\":...}`)
   round-trip whole — their structured `error.message` is what the
   model and the user actually need to act on. Short enough that a
   pathological provider 5xx HTML page or full streamed partial body
   doesn't take over the chat transcript. svar already caps at
   `MAX_HTTP_ERROR_BODY_CHARS` (8 KiB) on the way in; this is the
   render-side ceiling and stays well under the TUI bubble “collapse
   long body” threshold."
  4000)

(defn- parse-provider-body
  [body]
  (when (and (string? body) (not (str/blank? body)))
    (try
      (json/read-json body :key-fn keyword)
      (catch Throwable _ nil))))

(defn- provider-body-message
  [body]
  (let [parsed (parse-provider-body body)]
    (or (get-in parsed [:error :message])
      (:message parsed)
      (some-> body str/trim not-empty))))

(defn- invalid-thinking-signature-message?
  [message]
  (boolean (and (string? message)
             (re-find #"(?i)invalid.*signature.*thinking.*block" message))))

(defn- auth-provider-error?
  [status message wrapper-message]
  (let [text (str (or message "") "\n" (or wrapper-message ""))]
    (boolean
      (or (contains? #{401 403} status)
        (re-find #"(?i)(authentication|unauthorized|forbidden|credential|api[ -]?key|access[ -]?token|expired token|invalid token)"
          text)))))

(defn- auth-provider-next-step
  [data]
  (let [provider-id (or (:provider-id data) (:provider data) (:provider/id data))]
    (str "NEXT STEP: re-authenticate this provider or update its API key, then retry. "
      "TUI: Ctrl+K -> Model / Providers -> re-authenticate provider. "
      "CLI: run `vis providers auth"
      (when provider-id (str " " provider-id))
      "` for OAuth providers; for API-key providers, fix the configured key/env var and restart Vis.")))

(defn- provider-error-explanation
  [err]
  (let [message          (or (:message err) (str err))
        data             (:data err)
        body-raw         (some-> (:body data) str)
        status           (:status data)
        provider-message (provider-body-message body-raw)]
    (cond
      (invalid-thinking-signature-message? provider-message)
      (str "WHAT HAPPENED: Anthropic rejected the request before the model ran because Vis sent a `thinking` block with a signature that is not valid for Anthropic. "
        "Most likely cause: preserved-thinking replay crossed a provider/model boundary (for example Z.ai/Codex/OpenAI reasoning state was replayed into Anthropic), or an old Anthropic thinking block came from a different session/key. "
        "Fix: do not replay preserved-thinking unless provider AND model match; retry with only normal transcript/trailer context.")

      (auth-provider-error? status provider-message message)
      (str "WHAT HAPPENED: provider rejected credentials before the model ran."
        (when (seq provider-message)
          (str " Provider message: " provider-message))
        " "
        (auth-provider-next-step data))

      (seq provider-message)
      (str "WHAT HAPPENED: provider rejected the request before the model ran. Provider message: " provider-message)

      :else
      "WHAT HAPPENED: provider rejected the request before the model ran.")))

(defn- provider-error-ir
  [err]
  (let [message          (or (:message err) (str err))
        data             (:data err)
        body-raw         (some-> (:body data) str)
        status           (:status data)
        request-id       (or (:request-id data) (:request_id data))
        provider-message (provider-body-message body-raw)
        provider-body    (when (and body-raw (not (str/blank? body-raw)))
                           (truncate body-raw CHAT_ERROR_BODY_RENDER_CHARS))
        facts            (cond-> [[:li {} [:p {} [:span {} "Wrapper: "] [:c {} message]]]]
                           status (conj [:li {} [:p {} [:span {} "HTTP: "] [:c {} (str status)]]])
                           request-id (conj [:li {} [:p {} [:span {} "Request id: "] [:c {} (str request-id)]]])
                           provider-message (conj [:li {} [:p {} [:span {} "Provider message: "] [:c {} provider-message]]]))
        ir               (render/->ast
                           (cond-> [:ir {}
                                    [:h {:level 2} [:span {} "🚨 PROVIDER_ERROR"]]
                                    [:p {} [:strong {} [:span {} "Provider call failed before the model could run."]]]
                                    [:p {} [:strong {} [:span {} (provider-error-explanation err)]]]
                                    (into [:ul {}] facts)]
                             provider-body (conj [:p {} [:span {} "Provider response:"]]
                                             [:code {:lang "json"} provider-body])))]
    (assoc ir 1 (assoc (second ir) :vis/provider-error true))))

;; -----------------------------------------------------------------------------
;; Router lifecycle + model helpers (turn single-file API)
;; -----------------------------------------------------------------------------

(defonce ^:private router-atom (atom nil))

(defn- runtime-router-providers
  "Resolve durable provider config into the svar runtime shape.

   On-disk config intentionally omits ephemeral credentials for OAuth-backed
   providers such as OpenAI Codex. Resolve those fields immediately before
   constructing a router so each provider can refresh tokens and attach any
   provider-specific headers."
  [config]
  (mapv config/->svar-provider (:providers config)))

(defn get-router
  "Get or create the shared LLM router.

   Honors `:router` opts from `~/.vis/config.edn` (`:rate-limit`,
   `:network`, `:budget`, ...). Without that block svar's built-in
   defaults apply. See `config/router-opts` for the supported keys."
  []
  (or @router-atom
    (let [cfg (config/resolve-config)
          r   (svar/make-router (runtime-router-providers cfg)
                (config/router-opts cfg))]
      (reset! router-atom r)
      r)))

(defn rebuild-router!
  "Rebuild the router from the given config. Used when provider settings change.

   Forwards `:router` opts so live config edits (e.g. tuning
   `:same-provider-delays-ms`) take effect on the next `set-provider!`
   without restarting the JVM."
  [config]
  (let [r (svar/make-router (runtime-router-providers config)
            (config/router-opts config))]
    (reset! router-atom r)
    r))

(defn ask-code!
  "One-shot routed `svar/ask-code!` against the global router.
   Plain-text completion + Markdown-code-block extraction — returns the
   svar map `{:blocks :raw :reasoning :tokens :cost :duration-ms
   :assistant-message :provider-state}`. `:blocks` is a vec of
   `{:lang :source}` (one entry per Markdown code block); concatenate
   yourself with `svar.internal.codes/concat-sources` if you need a
   single string. `ask!` (JSON-spec) is gone; every Vis caller uses
   `ask-code!`."
  [opts]
  (svar/ask-code! (get-router) (with-default-ask-code-idle-timeout opts)))

(defn llm-text!
  "Fast helper LLM call for extensions.

   Uses svar routing (`:routing {:optimize :cost}`) instead of Vis-side model
   name heuristics. The call still goes through `svar/ask-code!` because Vis no
   longer uses the retired `ask!` structured-output path; `:lang \"text\"`,
   `:reasoning :off`, and `:code-tail-pointer? true` make the return a plain
   text string under :text. Callers may pass either :messages or :system +
   :prompt."
  [{:keys [messages system prompt reasoning temperature routing] :as opts}]
  (let [messages (or messages
                   (cond-> []
                     (seq system) (conj {:role "system" :content system})
                     (seq prompt) (conj {:role "user" :content prompt})))
        resp     (svar/ask-code! (get-router)
                   (with-default-ask-code-idle-timeout
                     (merge (dissoc opts :system :prompt :temperature)
                       {:messages           messages
                        :lang               "text"
                        :reasoning          (or reasoning :off)
                        :routing            (or routing {:optimize :cost})
                        :code-tail-pointer? true}
                       (when (some? temperature)
                         {:temperature temperature}))))
        text     (or (some-> resp :result str/trim not-empty)
                   (some-> resp :raw str/trim not-empty)
                   "")]
    (assoc resp :text text)))

(defn resolve-effective-model
  "Best-effort root model descriptor from router config.

   The returned map carries `:name` (model id, e.g. \"gpt-4o\") AND
   `:provider` (provider id keyword, e.g. `:openai`) so every caller
   can persist BOTH alongside the model. Earlier versions returned
   just the model map and the provider id was silently dropped on
   the way to the DB - leaving the meta layer with no way to render
   `provider/model`."
  ([router]
   (let [provider (first (:providers router))
         model    (first (:models provider))]
     (when model
       (cond-> (if (map? model) model {:name (str model)})
         (:id provider) (assoc :provider (:id provider))))))
  ([router _routing-overrides]
   (resolve-effective-model router)))

;; -----------------------------------------------------------------------------
;; System var helpers
;;
;; Var snapshots used to live here as `extract-def-names` +
;; `restorable-var-snapshots`: post-eval parsers that walked the
;; iteration's block source for `(def NAME …)` shapes and then read
;; the sandbox locals to materialize values. The engine replaces that
;; path with the SCI eval-def monkey-patch — every def the sandbox
;; evaluates lands in `:def-sink`, and `def-sink->vars-snapshot` (above)
;; produces the persistence shape directly.
;; -----------------------------------------------------------------------------

;; =============================================================================
;; Auto-Archive
;; =============================================================================

(defn- system-var-sym?
  "Local alias - the canonical predicate lives in `sci-env`. Kept here
   so archive code reads cleanly without an extra namespace bounce on
   every call."
  [sym]
  (env/system-var-sym? sym))

(defn- archive-vars!
  "Unmap `names` from the SCI sandbox namespace. Archive removes live
   bindings only; persisted rows remain for automatic sandbox restore.

   HARD GUARD: SYSTEM symbols can NEVER be archived - they are contract
   surfaces the iteration loop re-binds every turn. Filtered out +
   logged."
  [sci-ctx names]
  (let [raw-syms (keep (fn [n]
                         (cond (symbol? n) n
                           (string? n) (try (clojure.core/symbol n) (catch Throwable _ nil))
                           :else       nil))
                   names)
        {system-syms true user-syms false} (group-by (comp boolean system-var-sym?) raw-syms)]
    (when (seq system-syms)
      (tel/log! {:level :info :id ::archive-system-symbol-refused
                 :data {:requested (mapv str system-syms)}
                 :msg "Refusing to archive SYSTEM symbols - ignoring those names"}))
    (when (seq user-syms)
      (try
        (swap! (:env sci-ctx) update-in [:namespaces 'sandbox]
          (fn [ns-map] (apply dissoc ns-map user-syms)))
        (catch Throwable e
          (tel/log! {:level :debug :id ::archive-vars-failed
                     :data {:error (ex-message e) :syms (mapv str user-syms)}
                     :msg "archive-vars! failed - skipping"}))))))

(def ^:const HOT_SYMBOL_CAP
  "Maximum live user-defined symbols targeted by RLM hot memory."
  100)

(def ^:const HOT_SYMBOL_COMPACTION_TARGET
  "Low-water mark after a final successful answer. Archiving down to 80
   leaves headroom for the next turn instead of immediately bumping into
   the hard cap again."
  80)

(defn auto-archive-candidates
  "Pure cap-based hot-memory archive selection. Counts every live
   user-defined symbol toward the target, but only user symbols without a
   docstring are eligible. Returns the least-recent eligible symbols needed
   to reach `target-count`; if protected symbols alone exceed the target,
   returns every eligible symbol and leaves diagnostics to the caller."
  [sandbox-map initial-ns-keys var-registry target-count]
  (let [recency-of (fn [sym]
                     (if-let [ts (some-> (get var-registry sym) :created-at)]
                       (cond (inst? ts) (inst-ms ts)
                         (integer? ts) (long ts)
                         :else Long/MAX_VALUE)
                       Long/MAX_VALUE))
        user-symbol? (fn [sym]
                       (and (symbol? sym)
                         (not (contains? initial-ns-keys sym))
                         (not (env/system-var-sym? sym))))
        live-user-syms (filterv user-symbol? (keys sandbox-map))
        overflow (max 0 (- (count live-user-syms) (long target-count)))
        eligible (->> live-user-syms
                   (remove (fn [sym]
                             (let [doc (:doc (meta (get sandbox-map sym)))]
                               (and (string? doc) (not (str/blank? doc))))))
                   (sort-by (fn [sym] [(long (recency-of sym)) (str sym)]))
                   vec)]
    (set (take overflow eligible))))

(defn auto-archive-hot-symbols!
  "Archive eligible live user symbols after a final successful answer.
   Archive means removing bindings from the live SCI sandbox only; DB
   rows remain the source of truth for automatic sandbox restore."
  [{:keys [db-info session-id sci-ctx initial-ns-keys]}]
  (when (and db-info session-id sci-ctx)
    (try
      (let [var-registry (persistance/db-latest-var-registry db-info session-id)
            sandbox-map  (get-in @(:env sci-ctx) [:namespaces 'sandbox])
            candidates   (auto-archive-candidates sandbox-map initial-ns-keys
                           var-registry HOT_SYMBOL_COMPACTION_TARGET)]
        (when (seq candidates)
          (tel/log! {:level :info :id ::auto-archive
                     :data {:archived (mapv str candidates)
                            :count (count candidates)
                            :target HOT_SYMBOL_COMPACTION_TARGET}
                     :msg (str "Auto-archive: evicting " (count candidates)
                            " hot symbols after final answer")})
          (archive-vars! sci-ctx candidates)))
      (catch Exception e
        (tel/log! {:level :warn :id ::auto-archive-failed
                   :data {:error (ex-message e)}
                   :msg "Auto-archive failed - skipping"})))))

;; -----------------------------------------------------------------------------
;; Iteration loop + run-turn! (inlined from former base)
;; -----------------------------------------------------------------------------

;; Forward reference: defined in the environment lifecycle section
;; ~1500 lines below. Removing this declare requires extracting
;; `sync-active-extension-symbols!` + its 3 helpers (`extension-
;; aliases`, `extension-namespace-bindings`, `require-extension-
;; alias!`) into a separate ns (e.g. `internal/extension_environment.clj`).
;; Tracked as the proper file-split task (sister of
;; `extension-info` declare in extension.clj). See AGENTS.md S2.
(declare sync-active-extension-symbols!)

(def ^:private FRESH_ITER_CARRY
  ;; `:trailer-iters` is a vec of `[iteration-position {:thinking :blocks}]`
  ;; pairs (oldest-first). The prompt renderer trims the rendered trailer
  ;; by token budget (50% of model context), not fixed iteration count.
  {:trailer-iters []})

(def ^:private balanced-reasoning :balanced)

(do
  (defn- status->id [status]
    (when status (keyword "rlm.status" (name status))))

  (def ^:private cost-map-keys
    [:input-cost
     :input-uncached-cost
     :input-cached-cost
     :input-cache-write-cost
     :cache-read-cost
     :cache-write-cost
     :output-cost
     :total-cost])

  (defn- estimate-token-cost
    "Estimate cost from provider usage while preserving cached/non-cached input split."
    ([model input-tokens output-tokens]
     (estimate-token-cost model input-tokens output-tokens {}))
    ([model input-tokens output-tokens opts]
     (try
       (svar-router/estimate-cost model input-tokens output-tokens
         svar-router/MODEL_PRICING
         (or opts {}))
       (catch Throwable _ nil))))

  (defn- merge-cost-maps
    [acc extra-cost]
    (merge-with +
      (select-keys acc cost-map-keys)
      (select-keys extra-cost cost-map-keys))))

(defn iteration-loop
  "The core iteration loop. Runs assemble -> ask LLM -> execute -> persist
   until the model emits `:answer` or the user cancels."
  [environment user-request
   {:keys [system-prompt
           session-turn-id
           ;; `max-context-tokens` feeds advisory context-pressure hooks;
           ;; trailer assembly itself still owns no token trimming.
           max-context-tokens
           hooks cancel-atom current-iteration-atom
           reasoning-default routing extra-body turn-features allow-copilot-claude-deep?
           workspace-overrides]}]
  (let [environment (cond-> environment
                      (seq turn-features) (assoc :turn/features turn-features)
                      (seq workspace-overrides) (merge workspace-overrides))
        resolved-model (resolve-effective-model (:router environment))
        effective-model (:name resolved-model)
        _ (assert effective-model "Router must resolve a root model")
        has-reasoning? (reasoning-effort-configurable? resolved-model)
        base-reasoning-level (or (normalize-reasoning-level reasoning-default) balanced-reasoning)
        ;; Activate extensions ONCE per session turn. Threaded through both
        ;; the prompt message assembler (core, environment, extension messages)
        ;; and the per-iteration ext hint collector - activation-fn never
        ;; re-fires inside the loop.
        active-exts   (prompt/active-extensions environment)
        extensions-snapshot (prompt/extensions-snapshot active-exts)
        _             (sync-active-extension-symbols! environment active-exts)
        session-snapshot (fn []
                           {:id           (:session-id environment)
                            :title        (some-> (:session-title-atom environment) deref str str/trim not-empty)
                            :turn-id      session-turn-id
                            :user-request user-request})
        session-base (session-snapshot)
        turn-position (session-turn-position environment session-turn-id)
        stable-prompt-messages (prompt/assemble-stable-prompt-messages environment
                                 {:system-prompt     system-prompt
                                  :active-extensions active-exts})
        initial-messages (prompt/assemble-initial-messages
                           {:stable-prompt-messages stable-prompt-messages
                            :initial-user-content   user-request
                            :previous-turn-context  nil})
        usage-atom (atom {:input-tokens 0 :output-tokens 0 :reasoning-tokens 0 :cached-tokens 0
                          :cache-creation-tokens 0})
        accumulate-usage! (fn [api-usage]
                            (when api-usage
                              (swap! usage-atom
                                (fn [acc]
                                  (-> acc
                                    (update :input-tokens + (or (:prompt_tokens api-usage) 0))
                                    (update :output-tokens + (or (:completion_tokens api-usage) 0))
                                    (update :reasoning-tokens + (or (get-in api-usage [:completion_tokens_details :reasoning_tokens]) 0))
                                    (update :cached-tokens + (or (get-in api-usage [:prompt_tokens_details :cached_tokens])
                                                               (get-in api-usage [:prompt_tokens_details :input_cached_tokens])
                                                               0))
                                    (update :cache-creation-tokens + (or (get-in api-usage [:prompt_tokens_details :cache_creation_tokens])
                                                                       (get-in api-usage [:prompt_tokens_details :cache_write_tokens])
                                                                       0)))))))
        ;; Per-iteration token + cost projection. The schema's
        ;; `iteration.llm_*_tokens` / `iteration.llm_cost_usd` columns
        ;; carry one row per iteration so a future `vis report`
        ;; caller can sum or break down cost without re-walking
        ;; provider envelopes. Returns nil when the call surfaced no
        ;; usage (e.g. iteration-level error before a response
        ;; landed), in which case the persistance layer leaves the
        ;; columns NULL.
        iteration-token-cost (fn [api-usage]
                               (when api-usage
                                 (let [in   (long (or (:prompt_tokens api-usage) 0))
                                       out  (long (or (:completion_tokens api-usage) 0))
                                       reas (long (or (get-in api-usage [:completion_tokens_details :reasoning_tokens]) 0))
                                       cach (long (or (get-in api-usage [:prompt_tokens_details :cached_tokens])
                                                    (get-in api-usage [:prompt_tokens_details :input_cached_tokens])
                                                    0))
                                       cache-created (long (or (get-in api-usage [:prompt_tokens_details :cache_creation_tokens])
                                                             (get-in api-usage [:prompt_tokens_details :cache_write_tokens])
                                                             0))
                                       ;; svar's `estimate-cost` returns a MAP
                                       ;; `{:input-cost :output-cost :total-cost
                                       ;; :model :pricing}`, NOT a bare number.
                                       ;; Pull `:total-cost` out; nil pricing
                                       ;; (e.g. unknown model) leaves the
                                       ;; column NULL on disk, which the read
                                       ;; side defaults to 0.0.
                                       cost-map (estimate-token-cost effective-model in out {:api-usage api-usage})
                                       total    (when (map? cost-map) (:total-cost cost-map))]
                                   {:tokens   {:input in :output out :reasoning reas :cached cach
                                               :cache-created cache-created}
                                    :cost-usd (when (number? total) (double total))})))
        finalize-cost (fn []
                        (let [{:keys [input-tokens output-tokens reasoning-tokens
                                      cached-tokens cache-creation-tokens]} @usage-atom
                              total-tokens (+ input-tokens output-tokens)
                              cost (estimate-token-cost effective-model input-tokens output-tokens
                                     {:cached-tokens cached-tokens
                                      :cache-creation-tokens cache-creation-tokens})]
                          {:tokens {:input input-tokens :output output-tokens
                                    :reasoning reasoning-tokens :cached cached-tokens
                                    :cache-created cache-creation-tokens
                                    :total total-tokens}
                           :cost cost}))
        ;; `:on-chunk` is a per-reasoning-chunk streaming hook fired
        ;; from svar's stream callback. It fires dozens of times per
        ;; iteration, not at lifecycle boundaries. Lifecycle callbacks
        ;; now use namespaced `:ext/hooks` phases; on-chunk stays the
        ;; high-frequency streaming-only surface.
        on-chunk             (:on-chunk hooks)
        emit-hook! (fn [hook-fn payload log-message]
                     ;; Single-fn caller-hook helper, used by
                     ;; on-chunk only.
                     (when hook-fn
                       (try (hook-fn payload)
                         (catch Exception e
                           (tel/log! {:level :warn :data (format-exception-short e)} log-message)))))
        iteration-cache-created-tokens
        (fn [token-cost]
          (let [cache-created (long (or (get-in token-cost [:tokens :cache-created]) 0))]
            (when (pos? cache-created) cache-created)))]
    ;; -----------------------------------------------------------------
    ;; Turn-start SYSTEM-var bindings.
    ;;
    ;; `ctx` is the single model-visible engine context value; rebuilt fresh
    ;; per iteration.
    (env/bind-and-bump! environment 'ctx
      (vctx/build {:environment environment
                   :session session-base
                   :extension-ctx (extension/ctx-contributions environment active-exts)
                   :extensions extensions-snapshot}))
    (when-let [a (:current-iteration-id-atom environment)] (reset! a nil))
    (when-let [a (:current-session-turn-id-atom environment)] (reset! a session-turn-id))
    (when-let [a (:current-user-request-atom environment)] (reset! a user-request))
    ;; REPL-style recovery slots (`*1` `*2` `*3` `*e`) are per-turn. A
    ;; follow-up turn opens with all four nil so leftover values from
    ;; the previous turn never bleed into the new OODA loop.
    (env/reset-eval-bindings! environment)
    ;; Hot symbol compaction is archive-based and runs only after a
    ;; final successful answer. Failed/cancelled turns keep their live
    ;; scratch symbols for recovery.
    ;; Cross-turn carry: seed `trailer-iters` with persisted iterations
    ;; of the current session (across every prior turn) so a
    ;; follow-up turn opens with prior context. Rendering trims by token
    ;; budget, so carry is not capped by iteration count. Each entry is
    ;; `[iter-position {:thinking :blocks}]` matching the in-memory shape
    ;; the renderer expects. Failures degrade silently to an empty seed.
    ;;
    ;; IMPORTANT: cross-turn entries feed the TRAILER ONLY. Do not replay
    ;; their provider-native preserved-thinking assistant messages into the
    ;; new user turn. In session a9389e1d, Z.ai/GLM received prior-turn
    ;; assistant replay and opened the next request with "answer already
    ;; accepted", then burned >100k input tokens. Durable cross-turn memory
    ;; must flow through persisted iterations, not hidden reasoning state.
    (let [seeded-trailer-iters
          (try
            (when-let [session-id (:session-id environment)]
              (let [d (:db-info environment)
                    queries (persistance/db-list-session-turns d session-id)
                    current-turn-id-str (str session-turn-id)
                    ;; Drop CURRENT turn rows (defensive: they should not
                    ;; exist yet at seed time, but a restart/recover path
                    ;; could leave partial rows) and PRIOR-turn iterations
                    ;; whose status is NOT :done. Erroring / running /
                    ;; interrupted iterations are exploration noise that
                    ;; poisoned follow-up turns in session 2ccde943: 7 handle
                    ;; mistakes from turn 1 were replayed verbatim into
                    ;; turn 3's trailer, teaching the model that probing
                    ;; is unreliable. Carry only the iterations that landed
                    ;; a clean result; defs from earlier exploration
                    ;; survive independently via the def restore path.
                    iters (->> queries
                            (remove #(= (str (:id %)) current-turn-id-str))
                            (mapcat (fn [q]
                                      (try (persistance/db-list-session-turn-iterations d (:id q))
                                        (catch Throwable _ []))))
                            (filter #(= :done (:status %)))
                            (sort-by :created-at)
                            vec)]
                (mapv (fn [it]
                        [(or (:position it) 1)
                         {:thinking (:thinking it)
                          :blocks   [(cond-> {:position 0
                                              :code (or (:code it) "")}
                                       (contains? it :result) (assoc :result (:result it))
                                       (contains? it :error) (assoc :error (:error it)))]
                          :llm-provider (:provider it)
                          :llm-model    (some-> (:model it) str)
                          ;; Persisted assistant messages are intentionally NOT
                          ;; replayed across user turns. Keep row diagnostics,
                          ;; but `compatible-preserved-thinking-trailer-iters`
                          ;; rejects this entry before replay.
                          :assistant-message (:llm-assistant-message it)
                          :preserved-thinking/replay? false}])
                  iters)))
            (catch Throwable t
              (tel/log! {:level :warn :id ::cross-turn-trailer-seed-failed
                         :data  {:error (ex-message t)}
                         :msg   "Cross-turn carry seed failed; first iteration starts with an empty tape"})
              nil))]
      (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :iteration-loop})]
        (loop [loop-state (merge {:iteration 0 :messages initial-messages
                                  :trace []}
                            FRESH_ITER_CARRY
                            (when (seq seeded-trailer-iters)
                              {:trailer-iters seeded-trailer-iters}))]
          (let [{:keys [iteration messages trace trailer-iters llm-provider]} loop-state]
            (when current-iteration-atom (reset! current-iteration-atom (inc (long iteration))))
            (cond
              (when cancel-atom @cancel-atom)
              (do (log-stage! :error iteration {:reason :cancelled})
                (let [result (merge {:answer nil :status :cancelled :status-id (status->id :cancelled)
                                     :trace trace :iteration-count iteration} (finalize-cost))]
                  result))

              :else
              (let [raw-reasoning-level (when has-reasoning?
                                          base-reasoning-level)
                    reasoning-level (copilot-claude-safe-reasoning-level
                                      resolved-model user-request raw-reasoning-level
                                      {:allow-copilot-claude-deep? allow-copilot-claude-deep?})
                    _ (log-stage! :iteration/start iteration {:message-count (count messages)
                                                              :reasoning reasoning-level
                                                              :requested-reasoning raw-reasoning-level})
                    pre-resolved-model (resolve-effective-model (:router environment) (or routing {}))
                    ;; `:context-limit` for the `context-pressure-hint`
                    ;; threshold. Walk three sources in priority order:
                    ;;   1. caller-supplied `:max-context-tokens` (turn-
                    ;;      level override; rarely set today — TUI
                    ;;      `vis/send!` does not pass it).
                    ;;   2. The resolved model's `:input-limit` (models.dev
                    ;;      input cap, e.g. Copilot Claude-sonnet-4.6 = 128K).
                    ;;   3. The resolved model's `:context` (input+output
                    ;;      budget, used when models.dev exposes no
                    ;;      separate input cap).
                    ;;   4. 200_000 fallback for unknown models, matching
                    ;;      the historical advisory ceiling.
                    ;; Without this the hint fired off a uniform 200K
                    ;; baseline and either pestered the model too early
                    ;; on a 1M-context Anthropic native call or, worse,
                    ;; under-warned on a 128K Copilot call where the
                    ;; 50% trigger now lands at ~64K instead of 100K.
                    effective-context-limit (or max-context-tokens
                                              (:input-limit pre-resolved-model)
                                              (:context pre-resolved-model)
                                              200000)
                    llm-provider-context (cond-> {:selected (llm-id (:provider pre-resolved-model)
                                                              (some-> (:name pre-resolved-model) str))
                                                  :routing  (cond-> {:fallback? false}
                                                              (seq routing) (assoc :request routing))}
                                           (:error llm-provider)
                                           (assoc :error (:error llm-provider)))
                    iteration-position (inc (long iteration))
                    current-session (session-snapshot)
                    iteration-hints (collect-iteration-start-hints environment active-exts
                                      {:environment environment
                                       :phase :turn.iteration/start
                                       :session current-session
                                       :iteration iteration-position
                                       :session-title (:title current-session)
                                       :title-refresh? (zero? (long iteration))
                                       :turn-position turn-position
                                       :input-tokens (:input-tokens @usage-atom)
                                       :context-limit effective-context-limit})
                    current-ctx-map (vctx/build
                                      {:environment environment
                                       :session current-session
                                       :iteration   {:id       (some-> (:current-iteration-id-atom environment) deref)
                                                     :position iteration-position}
                                       :hints       iteration-hints
                                       :llm-provider llm-provider-context
                                       :extension-ctx (extension/ctx-contributions environment active-exts)
                                       :extensions  extensions-snapshot})
                    _ (env/bind-and-bump! environment 'ctx current-ctx-map)
                    iteration-context (vctx/render-iteration-trailer
                                        {:environment   environment
                                         :trailer-iters trailer-iters
                                         :ctx           current-ctx-map})
                      ;; Single canonical preserved-thinking replay path —
                      ;; svar's per-provider wire serializer turns the
                      ;; canonical assistant messages into native
                      ;; Anthropic / z.ai / Responses shapes.
                      ;;
                      ;; R3 hybrid message shape (per ADR/session
                      ;; 1db62d10): preserved-thinking replays + the
                      ;; iteration-context trailer both APPEND to
                      ;; the end. The original user_initial stays as the
                      ;; ONE user-role anchor near the start (placed there
                      ;; by `assemble-initial-messages`); we never repeat
                      ;; it. Final wire shape:
                      ;;
                      ;;   [system, user_initial,
                      ;;    asst_iter1, user_trailer_after_iter1,
                      ;;    asst_iter2, user_trailer_after_iter2,
                      ;;    ...
                      ;;    asst_iter(n-1), user_trailer_after_iter(n-1)]
                      ;;
                      ;; This matches z.ai's canonical preserved-thinking
                      ;; example (user → asst → user → asst → user) and
                      ;; stops GLM-5.1 from re-reading the same initial
                      ;; goal every iter and restarting its plan.
                    provider-messages (append-preserved-thinking-replay
                                        messages trailer-iters (replay-context pre-resolved-model))
                    effective-messages (cond-> provider-messages
                                         (not (str/blank? (or iteration-context "")))
                                         (conj {:role "user" :content iteration-context}))
                    resolved-model pre-resolved-model
                    effective-routing (or routing {})
                    iteration-result
                    (loop [attempt 0]
                      (let [result (try
                                     (run-iteration environment effective-messages
                                       {:iteration iteration :reasoning-level reasoning-level
                                        :routing effective-routing
                                        :resolved-model resolved-model
                                        :on-chunk on-chunk
                                        :active-extensions active-exts
                                        :answer-validation-context
                                        {:user-request user-request
                                         :previous-iterations trailer-iters
                                         :previous-blocks (vec (mapcat (comp :blocks second) trailer-iters))}
                                        :extra-body extra-body})
                                     (catch Exception e
                                       (if (and (stream-truncated-error? e)
                                             (< attempt MAX_STREAM_TRUNCATED_RETRIES))
                                         (do
                                           (tel/log! {:level :warn
                                                      :id ::stream-truncated-retry
                                                      :data {:iteration iteration
                                                             :attempt (inc attempt)
                                                             :max-retries MAX_STREAM_TRUNCATED_RETRIES
                                                             :type (:type (ex-data e))}}
                                             (str "Stream truncated, transparent retry "
                                               (inc attempt) "/" MAX_STREAM_TRUNCATED_RETRIES))
                                           ::retry)
                                         (handle-iteration-exception! e
                                           {:iteration iteration :messages effective-messages
                                            :routing effective-routing :reasoning-level reasoning-level}))))]
                        (if (= result ::retry)
                          (recur (inc attempt))
                          result)))]
                (if-let [iteration-error-data (::iteration-error iteration-result)]
                  ;; Cancellation short-circuit. When the user pressed Esc
                  ;; mid-call, `cancel!` flipped the flag BEFORE
                  ;; future-cancel, so by the time we land here the flag is
                  ;; already true. Treat the resulting interrupt-shaped
                  ;; \"iteration-error-data\" as cancellation, not a real failure: skip
                  ;; the trace entry, skip the DB write, skip the on-chunk
                  ;; error chunk (otherwise the bubble paints a phantom
                  ;; ITERATION N ERROR block right next to FINAL ANSWER:
                  ;; \"_Cancelled by user._\"). Bail straight to the cancel
                  ;; result that the top-of-loop branch would have produced.
                  (if (and cancel-atom @cancel-atom)
                    (do (log-stage! :error iteration {:reason :cancelled})
                      (let [result (merge {:answer nil :status :cancelled
                                           :status-id (status->id :cancelled)
                                           :trace trace :iteration-count iteration}
                                     (finalize-cost))]
                        result))
                    (let [llm-provider-error (llm-provider-error-context iteration iteration-error-data)
                          error-feedback (iteration-error-feedback iteration iteration-error-data user-request)
                          trace-entry {:iteration iteration :error iteration-error-data :final? false}
                          empty-reasoning (when (= :svar.llm/empty-content (:type iteration-error-data))
                                            (:reasoning (:data iteration-error-data)))
                          err-iteration-id (persistance/db-store-iteration! (:db-info environment)
                                             (let [tc (iteration-token-cost (:api-usage iteration-result))]
                                               (cond-> {:session-turn-id session-turn-id :vars [] :code ""
                                                        :thinking empty-reasoning :duration-ms 0 :llm-full-duration-ms 0 :error iteration-error-data
                                                        :llm-messages effective-messages
                                                        :llm-provider (:provider resolved-model)
                                                        :llm-model (str (:name resolved-model))
                                                        :llm-routing (cond-> {:selected (llm-id (:provider resolved-model) (some-> (:name resolved-model) str))
                                                                              :actual   (llm-id (:provider resolved-model) (some-> (:name resolved-model) str))
                                                                              :fallback? false}
                                                                       (seq (get-in iteration-error-data [:data :routed/trace]))
                                                                       (assoc :fallback? true
                                                                         :trace (vec (get-in iteration-error-data [:data :routed/trace]))))
                                                        :cache-created-tokens (iteration-cache-created-tokens tc)}
                                                 tc (assoc :tokens (:tokens tc)
                                                      :cost-usd (:cost-usd tc)))))]
                      (when-let [a (:current-iteration-id-atom environment)] (reset! a err-iteration-id))
                      ;; Live error chunk - `:phase :iteration-error`
                      ;; signals the iteration aborted before any
                      ;; forms could run. No per-form chunks fired
                      ;; this iteration, so the channel sees a clean
                      ;; reasoning -> error transition.
                      (emit-hook! on-chunk
                        {:phase     :iteration-error
                         :iteration (inc (long iteration))
                         :thinking  empty-reasoning
                         :error     iteration-error-data
                         :done?     true}
                        "on-chunk (iteration error)")
                      (if (::fatal-iteration-error iteration-result)
                        (let [trace' (conj trace trace-entry)
                              fallback (or (some-> (:error trace-entry) provider-error-ir)
                                         (render/->ast [:ir {}
                                                        [:h {:level 2} [:span {} "🚨 PROVIDER_ERROR"]]
                                                        [:p {} [:span {} "Provider call failed before the model could run."]]]))
                              result (merge {:answer fallback
                                             :status :error
                                             :status-id (status->id :error)
                                             :trace trace'
                                             :iteration-count (inc iteration)}
                                       (finalize-cost))]
                          result)
                        (recur (assoc loop-state
                                 :iteration (inc iteration)
                                 :messages (conj messages {:role "user" :content error-feedback})
                                 :llm-provider {:error llm-provider-error}
                                 :trace (conj trace trace-entry))))))

                  (let [_ (accumulate-usage! (:api-usage iteration-result))
                        {:keys [thinking blocks final-result]} iteration-result
                        block (first blocks)
                        iteration-block-code (or (some-> block :code str/trim not-empty) "")
                        iteration-time-ms (long (block-duration-ms block))
                        vars-snapshot (def-sink->vars-snapshot
                                        (vec (:def-sink block))
                                        iteration-block-code
                                        (when (pos? iteration-time-ms) iteration-time-ms))
                        ;; Phase 4b: dependency edges from a post-eval
                        ;; source walk of the iteration block. SCI's
                        ;; analyzer bakes symbol resolution into the
                        ;; AST at compile time so a runtime resolve
                        ;; hook would not see init-body references;
                        ;; we recover the graph from source instead.
                        ;; The persistance layer filters every edge
                        ;; against the existing-soul-name set inside
                        ;; one transaction, so core ops, locals, and
                        ;; cross-iteration upstreams (this iter's defn
                        ;; depends on a prior iter's var) all sort
                        ;; themselves out there.
                        deps-snapshot (->> (dep-edges-from-source iteration-block-code)
                                        (map (fn [{:keys [upstream downstream]}]
                                               {:upstream-name upstream
                                                :downstream-name downstream}))
                                        distinct
                                        vec)
                        ;; Phase 7: merge per-iteration `:lru` stamps
                        ;; (collected by the patched resolve-symbol*)
                        ;; into the long-lived per-env LRU map. The trailer's
                        ;; live-vars view reads this to age user vars out of
                        ;; the discovery line after
                        ;; `JOURNAL_LRU_TURN_WINDOW` quiet turns.
                        _ (when-let [lru-atom (:def-resolve-lru-atom environment)]
                            (when-let [iteration-lru (not-empty (:lru block))]
                              (swap! lru-atom merge iteration-lru)))
                        store-block (or block {:code "" :error {:message "empty iteration"}})
                        iteration-id (persistance/db-store-iteration! (:db-info environment)
                                       (let [tc (iteration-token-cost (:api-usage iteration-result))]
                                         (cond-> {:session-turn-id session-turn-id
                                                  :code (:code store-block)
                                                  :result (:result store-block)
                                                  :error (:error store-block)
                                                  :duration-ms (long (or (envelope-duration-ms (:envelope store-block)) 0))
                                                  :llm-full-duration-ms (long (or (:duration-ms iteration-result) 0))
                                                  :vars vars-snapshot
                                                  :dependencies deps-snapshot
                                                  :thinking thinking
                                                  :answer (when final-result (answer-markdown (:answer final-result)))
                                                  :llm-messages (:llm-messages iteration-result)
                                                  :llm-provider (or (:llm-provider iteration-result) (:provider resolved-model))
                                                  :llm-model (:llm-model iteration-result)
                                                  :llm-raw-response (:llm-raw-response iteration-result)
                                                  :llm-executable-blocks (:llm-executable-blocks iteration-result)
                                                  :llm-returned-empty-code? (:llm-returned-empty-code? iteration-result)
                                                  :llm-assistant-message (:assistant-message iteration-result)
                                                  :llm-routing (llm-routing-summary pre-resolved-model iteration-result)
                                                  :cache-created-tokens (iteration-cache-created-tokens tc)}
                                           tc (assoc :tokens (:tokens tc)
                                                :cost-usd (:cost-usd tc)))))
                        _ (when-let [a (:current-iteration-id-atom environment)] (reset! a iteration-id))
                        trace-entry {:iteration iteration :thinking thinking
                                     :blocks blocks :final? (boolean final-result)}]
                    (cond
                      final-result
                      (do (log-stage! :final iteration
                            {:answer (answer-markdown (:answer final-result))
                             :iteration-count (inc iteration)})
                        (log-stage! :iteration/stop iteration
                          {:blocks (count blocks) :errors (count (filter :error blocks))
                           :times (mapv block-duration-ms blocks)})
                        ;; Iteration-final chunk (`:phase :iteration-final`).
                        ;; Per-form chunks already streamed every form
                        ;; result; this is the trim \"iteration is
                        ;; complete, here is the terminal answer\"
                        ;; signal. Consumers attach `:final` to
                        ;; whatever's already on screen.
                        ;;
                        ;; `:answer-position` tells the channel which
                        ;; per-block slot was the `(done ...)` call;
                        ;; the progress tracker elides that slot so
                        ;; the renderer doesn't paint the answer
                        ;; call's code above the answer text.
                        (when on-chunk
                          (on-chunk {:phase            :iteration-final
                                     :iteration-count  (inc (long iteration))
                                     :thinking         thinking
                                     :final            {:answer          (:answer final-result)
                                                        :iteration-count (inc iteration)
                                                        :status          :success}
                                     :answer-position  (:answer-position final-result)
                                     :silent-form-idxs (:silent-form-idxs iteration-result)
                                     :done?            true}))
                        (let [result (-> (merge {:answer (:answer final-result) :trace (conj trace trace-entry)
                                                 :iteration-count (inc iteration)}
                                           (finalize-cost))
                                       (attach-llm-routing-summary pre-resolved-model iteration-result))]
                          (auto-archive-hot-symbols! environment)
                          result))

                      :else
                      (if (empty? blocks)
                        (do (log-stage! :empty iteration {})
                          (log-stage! :iteration/stop iteration {:blocks 0 :errors 0 :times []})
                          (recur (merge loop-state
                                   {:iteration (inc iteration) :trace (conj trace trace-entry)})))

                        (do (log-stage! :iteration/stop iteration
                              {:blocks (count blocks) :errors (count (filter :error blocks))
                               :times (mapv block-duration-ms blocks)})
                          ;; Non-terminal iteration-final chunk: per-form
                          ;; chunks already streamed; this is the
                          ;; \"iteration done, more iterations coming\"
                          ;; marker. `:final` is nil because the
                          ;; turn isn't done yet.
                          (when on-chunk
                            (on-chunk {:phase            :iteration-final
                                       :iteration        (inc (long iteration))
                                       :thinking         thinking
                                       :final            nil
                                       :silent-form-idxs (:silent-form-idxs iteration-result)
                                       :done?            false}))
                          (let [;; Carry forward all observed iterations
                                ;; as `[pos {:thinking :blocks}]` for the
                                ;; next iteration's trailer. The renderer
                                ;; drops oldest entries by window, not
                                ;; by token count.
                                _ blocks
                                next-recent (conj (vec (or trailer-iters []))
                                              [(inc (long iteration))
                                               {:thinking thinking
                                                :blocks   blocks
                                                :llm-executable-blocks (:llm-executable-blocks iteration-result)
                                                :llm-provider (:llm-provider iteration-result)
                                                :llm-model    (:llm-model iteration-result)
                                                  ;; svar's canonical replay handle for this
                                                  ;; iteration. Re-emitted only within this
                                                  ;; live user turn via
                                                  ;; `append-preserved-thinking-replay`; cross-turn
                                                  ;; seeds opt out with
                                                  ;; `:preserved-thinking/replay? false`.
                                                :assistant-message (:assistant-message iteration-result)
                                                :preserved-thinking/replay? true}])]
                            (recur (merge (dissoc loop-state :llm-provider)
                                     {:iteration          (inc iteration)
                                      :messages           messages
                                      :trace              (conj trace trace-entry)
                                      :trailer-iters      next-recent}))))))))))))))))

(defn run-turn!
  "Store turn -> iteration-loop -> update turn -> return result.

   Derives `:prior-outcome` (one of `:complete`, `:cancelled`, `:error`)
   from the loop result and
   persists it on the `session_turn_state` row. The next turn's
   `<system_state>` digest reads it."
  [env user-request loop-opts]
  (when-not (map? env)
    (throw (ex-info "run-turn! requires an env map" {:got (type env)})))
  (when (clojure.string/blank? user-request)
    (throw (ex-info "run-turn! requires a non-blank user request" {:got user-request})))
  (let [session-turn-id (persistance/db-store-session-turn! (:db-info env)
                          {:parent-session-id (:session-id env)
                           :user-request user-request
                           :status :running})
        result (iteration-loop env user-request (assoc loop-opts :session-turn-id session-turn-id))
        prior-outcome (:status result)
        _ (persistance/db-update-session-turn! (:db-info env) session-turn-id
            {;; The persisted answer is the raw Markdown source the
             ;; model wrote in `(done {:answer ...})`. Channels parse
             ;; the Markdown into IR at render time via
             ;; `render/markdown->ir`; the database stays human-
             ;; readable and round-trips byte-for-byte through copy /
             ;; export / transcript.
             :answer-markdown (when-let [a (:answer result)] (answer-markdown a))
             :iteration-count (:iteration-count result)
             :duration-ms     (:duration-ms result)
             :status          (or (:status result) :success)
             :tokens          (:tokens result)
             :cost            (:cost result)
             :prior-outcome   prior-outcome})]
    (assoc result :session-turn-id session-turn-id :prior-outcome prior-outcome)))

(defn custom-bindings
  "Current custom SCI bindings {sym -> value}."
  [env]
  (some-> (:state-atom env) deref :custom-bindings))

;; -----------------------------------------------------------------------------
;; Prepare turn context
;; -----------------------------------------------------------------------------

(defn- prepare-turn-context
  "Validates inputs, resolves SCI bindings, sets up atoms.
   Returns a map of all computed context needed for subsequent phases."
  [env messages opts]
  (let [{:keys [spec model
                max-context-tokens
                system-prompt debug? hooks cancel-atom eval-timeout-ms
                reasoning-default routing extra-body]
         :or   {debug? false}} opts]
    (when-not (:db-info env)
      (anomaly/incorrect! "Invalid RLM environment" {:type :vis/invalid-env}))
    (when-not (and (vector? messages) (seq messages))
      (anomaly/incorrect! "messages must be a non-empty vector of message maps, e.g. [(svar/user \"...\")]"
        {:type :vis/invalid-messages :got (type messages)}))
    (when (and (some? eval-timeout-ms) (not (integer? eval-timeout-ms)))
      (anomaly/incorrect! ":eval-timeout-ms must be an integer (milliseconds)"
        {:type     :vis/invalid-eval-timeout
         :got      eval-timeout-ms
         :got-type (type eval-timeout-ms)}))
    (let [cancel-atom            (or cancel-atom (atom false))
          ;; `user-request` = ONLY the current turn's user message.
          ;;
          ;; Prior behavior joined every message's :content (including
          ;; previous turns' user messages + assistant answers + system!) into
          ;; one growing blob. That corrupted three things at once:
          ;;   1. the persisted user request stored the entire transcript
          ;;      for every turn - the sidebar showed "Siema\nSiema!\n...".
          ;;   2. Any model-facing context derived from that blob grew with
          ;;      each turn instead of reflecting the current ask. Surface now
          ;;      flows through ctx.
          ;;   3. The synthetic `{:requirement ...}` frame the LLM sees
          ;;      restated the whole session as the "requirement".
          ;;
          ;; Prior dialog transcript is dropped here. `user-request` is
          ;; ONLY the current turn - one ask, one value.
          extract-text           (fn [c]
                                   (cond
                                     (string? c)     c
                                     (sequential? c) (str/join " "
                                                       (keep #(when (= "text" (:type %)) (:text %)) c))
                                     :else           nil))
          ;; Locate the LAST user message once. It is the only human text
          ;; sent into this turn. Prior dialog transcript is intentionally
          ;; NOT replayed to the model; durable context flows through
          ;; persisted iterations, defs, SYSTEM vars, and DB-backed tools.
          last-user-idx          (->> (map-indexed vector messages)
                                   reverse
                                   (some (fn [[i m]]
                                           (when (contains? #{"user" :user} (:role m))
                                             i))))
          last-user-message      (when last-user-idx (nth messages last-user-idx))
          user-request           (or (some-> last-user-message :content extract-text)
                                   ;; Fallback: no :user role found (malformed caller) -
                                   ;; use the last message's text. Better than an empty user request.
                                   (some-> messages last :content extract-text)
                                   "")
          env-router             (:router env)
          root-resolved-model    (when env-router (resolve-effective-model env-router))
          root-model             (or (:name root-resolved-model) model)
          root-provider          (:provider root-resolved-model)
          db-info                (:db-info env)
          custom-bindings        (custom-bindings env)
          current-iteration-atom (:current-iteration-atom env)
          sci-ctx                (:sci-ctx env)
          _                      (doseq [[sym val] (or custom-bindings {})]
                                   (when val
                                     (env/sci-update-binding! sci-ctx sym val)))
          current-iteration-id-atom (:current-iteration-id-atom env)
          current-session-turn-id-atom (:current-session-turn-id-atom env)
          ;; Workspace pin lives on the env itself (set in create-environment).
          ;; Opts may carry namespaced `:workspace/*` overrides for unusual
          ;; per-turn cases; the legacy bare `:workspace` key was removed
          ;; (PLAN.md §5 — pure :workspace/* keys, never a bare :workspace).
          workspace-overrides    (select-keys opts [:workspace/root :workspace/id
                                                    :workspace/kind :workspace/branch])
          environment            (cond-> (assoc env
                                           :current-iteration-atom current-iteration-atom
                                           :current-iteration-id-atom current-iteration-id-atom
                                           :current-session-turn-id-atom current-session-turn-id-atom)
                                   (seq workspace-overrides) (merge workspace-overrides))
          environment-id         (:environment-id env)]
      {:cancel-atom            cancel-atom
       :user-request           user-request
       :router                 env-router
       :root-resolved-model    root-resolved-model
       :root-model             root-model
       :root-provider          root-provider
       :db-info                db-info
       :current-iteration-atom current-iteration-atom
       :environment            environment
       :environment-id         environment-id
       :spec                   spec
       :max-context-tokens     max-context-tokens
       :system-prompt          system-prompt
       :debug?                 debug?
       :hooks                  hooks
       :eval-timeout-ms        eval-timeout-ms
       :reasoning-default      reasoning-default
       :routing                routing
       :extra-body             extra-body
       :turn-features          (get opts :turn/features)
       :workspace-overrides    workspace-overrides
       :messages               messages})))

;; -----------------------------------------------------------------------------
;; Run iteration phase
;; -----------------------------------------------------------------------------

(defn- run-iteration-phase
  "Runs the main iteration loop via run-turn!.
   Returns iteration-result, session-turn-id, cost atoms, and merge-cost! fn."
  [{:keys [environment user-request spec
           max-context-tokens system-prompt
           current-iteration-atom hooks cancel-atom
           reasoning-default routing extra-body turn-features workspace-overrides]}]
  (let [iteration-result (run-turn! environment user-request
                           (cond-> {:output-spec            spec
                                    :max-context-tokens     max-context-tokens
                                    :system-prompt          system-prompt
                                    :reasoning-default      reasoning-default
                                    :current-iteration-atom current-iteration-atom
                                    :hooks                  hooks
                                    :cancel-atom            cancel-atom}
                             routing       (assoc :routing routing)
                             extra-body    (assoc :extra-body extra-body)
                             turn-features (assoc :turn-features turn-features)
                             (seq workspace-overrides) (assoc :workspace-overrides workspace-overrides)))
        session-turn-id         (:session-turn-id iteration-result)
        {iteration-tokens :tokens
         iteration-cost   :cost} iteration-result
        total-tokens-atom (atom (or iteration-tokens {}))
        total-cost-atom   (atom (or iteration-cost {}))
        merge-cost!       (fn [extra-tokens extra-cost]
                            (when extra-tokens
                              (swap! total-tokens-atom
                                (fn [acc]
                                  (merge-with + acc
                                    (select-keys extra-tokens [:input :output :reasoning :cached :total])))))
                            (when extra-cost
                              (swap! total-cost-atom
                                (fn [acc]
                                  (merge-cost-maps acc extra-cost)))))]
    {:iteration-result  iteration-result
     :session-turn-id         session-turn-id
     :total-tokens-atom total-tokens-atom
     :total-cost-atom   total-cost-atom
     :merge-cost!       merge-cost!}))

;; =============================================================================
;; Title listeners + set-title! broadcast
;;
;; Channels (TUI, Telegram, ...) that want to react to a session
;; title change - typically because the model emitted `(set-session-title! "...")`
;; mid-turn - register a listener via `add-title-listener!`. The
;; listener fn receives the new title; it MUST be cheap (typically a
;; `state/dispatch` into the channel's app-db). Listeners are stored
;; per session-id so a TUI watching session A doesn't get
;; woken by a Telegram bot updating session B.
;;
;; Both `set-title!` (host-driven, e.g. CLI rename) and the SCI
;; `(set-session-title! "...")` fn (model-driven) funnel through
;; `set-title-with-broadcast!`, which is the single mutation point.
;; That keeps the in-memory env atom + DB column + listener fan-out
;; in lockstep - no path can update one without the others.
;; =============================================================================

(defonce ^:private title-listeners
  ;; {session-id-uuid #{listener-fn ...}}
  (atom {}))

(defn add-title-listener!
  "Register `listener-fn` for `session-id`. The fn is invoked with
   the new title (a string) every time the title changes. Multiple
   listeners are supported; they fire in unspecified order.

   Returns the listener fn so callers can pass it to
   `remove-title-listener!` later."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-listeners update cid (fnil conj #{}) listener-fn))
  listener-fn)

(defn remove-title-listener!
  "Deregister a previously added listener. Idempotent."
  [session-id listener-fn]
  (let [cid (persistance/->uuid session-id)]
    (swap! title-listeners update cid
      (fn [existing] (disj (or existing #{}) listener-fn))))
  nil)

(defn- broadcast-title-change!
  "Fire every registered listener for `session-id` with `title`.
   Listeners that throw are swallowed and logged - a misbehaving
   channel must NOT block the iteration loop."
  [session-id title]
  (let [cid (persistance/->uuid session-id)]
    (doseq [f (get @title-listeners cid)]
      (try (f title)
        (catch Throwable t
          (tel/log! {:level :warn :id ::title-listener-failed
                     :data {:session-id cid
                            :error (ex-message t)}
                     :msg (str "Title listener threw: " (ex-message t))}))))))

(defn set-title-with-broadcast!
  "Single mutation point for session titles.

   1. Writes the title to the persisted `session_state` row.
   2. Updates the env's in-memory `:session-title-atom` so the next iteration's
      `:session-title-atom` mirror sees the new value AND so a
      read from the SCI sandbox returns the fresh string immediately,
      without a DB round-trip.
   3. Broadcasts to every registered listener.

   `session-title-atom` may be nil (host-driven path with no live env)."
  [db-info session-id session-title-atom title]
  (let [t (str title)]
    (persistance/db-update-session-title! db-info session-id t)
    (when session-title-atom (reset! session-title-atom t))
    (broadcast-title-change! session-id t)
    nil))

;; -----------------------------------------------------------------------------
;; Finalize turn result
;; -----------------------------------------------------------------------------

(defn- finalize-turn-result
  "Updates DB turn record, builds result map.

   `:provider` and `:model` are both attached to the persisted cost
   map so the web footer / meta layer can render `provider/model / N
   iteration / duration / tokens / $total` after a restart."
  [{:keys [db-info root-model root-provider]}
   {:keys [session-turn-id start-time iteration-count status status-id trace locals
           answer confidence reasoning total-tokens-atom total-cost-atom]}]
  (let [duration-ms (util/elapsed-since start-time)
        cost-with-model (cond-> @total-cost-atom
                          (and root-model (not (:model @total-cost-atom)))
                          (assoc :model (str root-model))
                          (and root-provider (not (:provider @total-cost-atom)))
                          (assoc :provider root-provider))]
    (if status
      ;; failure path - surface the fallback answer (built by the loop for
      ;; :error) to the caller. Leaving
      ;; :answer nil here meant the web bubble rendered blank even though
      ;; we had diagnostic text ready.
      (do
        (log-stage! :turn/complete 0
          {:duration-ms duration-ms :iteration-count iteration-count :status status})
        (let [fallback-answer (:result answer answer)]
          (try
            (persistance/db-update-session-turn! db-info session-turn-id
              {:answer          fallback-answer
               :iteration-count iteration-count
               :duration-ms     duration-ms
               :status          status
               :tokens          @total-tokens-atom
               :cost            cost-with-model})
            (catch Exception e
              (tel/log! {:level :warn :data (format-exception-short e)
                         :msg   "Failed to update turn (max iterations)"})))
          (cond-> {:answer          fallback-answer
                   :status          status
                   :status-id       status-id
                   :trace           trace
                   :iteration-count iteration-count
                   :duration-ms     duration-ms
                   :tokens          @total-tokens-atom
                   :cost            cost-with-model}
            (some? locals) (assoc :locals locals))))
      ;; success path
      (do
        (log-stage! :turn/complete 0
          {:duration-ms duration-ms :iteration-count iteration-count
           :cost (str (:total-cost cost-with-model))})
        (try
          (persistance/db-update-session-turn! db-info session-turn-id
            {:answer          answer
             :iteration-count iteration-count
             :duration-ms     duration-ms
             :status          :success
             :tokens          @total-tokens-atom
             :cost            cost-with-model})
          (catch Exception e
            (tel/log! {:level :warn :data (format-exception-short e)
                       :msg   "Failed to update turn (success)"})))
        (cond-> {:answer          answer
                 :trace           trace
                 :iteration-count iteration-count
                 :duration-ms     duration-ms
                 :tokens          @total-tokens-atom
                 :cost            cost-with-model}
          (some? confidence) (assoc :confidence confidence)
          (some? reasoning)  (assoc :reasoning reasoning))))))

;; -----------------------------------------------------------------------------
;; Public entry point
;; -----------------------------------------------------------------------------

(defn turn!
  "Runs one session turn on an RLM environment using iterative LLM code evaluation.

    Params:
    `environment` - RLM environment from create-environment.
    `messages` - Vector of message maps. Always a vector, e.g.:
                 [(svar/user <prompt-text>)]
                 [(svar/user <prompt-text> (svar/image <b64> <mime-type>))]
   `opts` - Map, optional:
     - :spec - Output spec for structured answers.
     - :model - Override config's default model.
      - :max-context-tokens - Token budget for context.
      - :debug? - Enable verbose debug logging (default: false). Logs iteration details,
        code evaluation, LLM responses at :info level with :rlm-phase context.
      - :reasoning-default - Optional base reasoning effort for reasoning-capable models.
        Accepts :low/:medium/:high or low/medium/high strings. Adaptive escalation still applies.
      - :extra-body - Optional provider-specific request-body params merged into the
        upstream LLM call after auto max_tokens + reasoning translation.

    Returns:
   Map with:
      - :trace - Vector of iteration trace entries, each containing:
          {:iteration N
           :response <llm-response-text>
           :blocks [{:id 0 :code <code-str> :result <value> :error nil
                     :envelope {:started-at-ms 10 :finished-at-ms 15 ...}}
                       ...]}
     - :iteration-count - Number of iterations used.
     - :duration-ms - Turn duration in milliseconds.
     - :tokens - Token usage map {:input N :output N :total N}.
     - :cost - Cost map {:input-cost N :output-cost N :total-cost N}.
     - :confidence - Confidence level (:high/:medium/:low) from final iteration.
      - :reasoning - String summary of how the answer was derived (from LLM's FINAL call).
      - :status - Only present on failure (`:error` or `:cancelled`)."
  ([environment messages]
   (turn! environment messages {}))
  ([environment messages opts]
   (let [ctx (prepare-turn-context environment messages opts)
         {:keys [eval-timeout-ms
                 debug? user-request root-model
                 db-info
                 environment-id]} ctx]
     (binding [*rlm-context*       {:rlm-environment-id environment-id :rlm-type :main
                                    :rlm-debug? debug? :rlm-phase :turn
                                    :db-info db-info
                                    :session-soul-id (:session-id environment)}
               *eval-timeout-ms*  (clamp-eval-timeout-ms
                                    (or eval-timeout-ms *eval-timeout-ms*))]
       (tel/with-ctx+ {:db-info db-info
                       :session-soul-id (:session-id environment)}
         (log-stage! :turn/open 0
           {:model root-model
            :reasoning? (boolean (:reasoning? (first (mapcat :models (:providers (:router environment))))))
            :user-request user-request})
         (let [start-time   (System/nanoTime)
               phase2       (run-iteration-phase ctx)
               {:keys [iteration-result session-turn-id
                       total-tokens-atom total-cost-atom]} phase2
               {iteration-answer :answer
                trace            :trace
                iteration-count  :iteration-count
                status           :status
                status-id        :status-id
                locals           :locals
                confidence       :confidence
                reasoning        :reasoning} iteration-result
               result
               (if status
                 (finalize-turn-result
                   ctx
                   {:session-turn-id          session-turn-id
                    :start-time        start-time
                    :iteration-count   iteration-count
                    :status            status
                    :status-id         status-id
                    :trace             trace
                    :locals            locals
                    :answer            iteration-answer
                    :total-tokens-atom total-tokens-atom
                    :total-cost-atom   total-cost-atom})
                 (finalize-turn-result
                   ctx
                   {:session-turn-id          session-turn-id
                    :start-time        start-time
                    :iteration-count   iteration-count
                    :trace             trace
                    :answer            iteration-answer
                    :confidence        confidence
                    :reasoning         reasoning
                    :total-tokens-atom total-tokens-atom
                    :total-cost-atom   total-cost-atom}))]
           result))))))

;; =============================================================================
;; Environment lifecycle + system prompt
;; =============================================================================

;; =============================================================================
;; Helpers
;; =============================================================================

;; =============================================================================
;; Public env accessors
;; =============================================================================

;; `db-info` (the env accessor) was a thin wrapper over `(:db-info env)`
;; that no caller actually invoked - every consumer either destructured
;; `:db-info` directly or used the no-arg `(db-info)` defined further
;; down (which returns the process-wide shared connection). The defn was
;; deleted to keep ONE canonical `db-info` symbol on this namespace.

(defn- extension-aliases
  [exts]
  (->> (or exts [])
    (keep extension/ext-alias)
    (distinct)
    vec))

(defn- require-extension-alias!
  [sci-ctx ext ns-sym alias-sym]
  (try
    (sci/eval-string+ sci-ctx
      (str "(require '[" ns-sym " :as " alias-sym "])")
      {:ns (sci/find-ns sci-ctx 'sandbox)})
    (catch Throwable t
      (tel/log! {:level :warn :id ::ext-alias-require-failed
                 :data (assoc (format-exception-short t)
                         :ext (:ext/name ext)
                         :alias alias-sym)}
        (str "Auto-require of alias '" alias-sym "' failed")))))

(defn- sci-binding-var
  "Build the SCI var the model interacts with for an extension symbol.

   Forwards `:doc` and `:arglists` from the symbol entry into the SCI var
   metadata so `(clojure.repl/doc v/cat)`, `(:doc (meta #'v/cat))`, and
   `(:arglists (meta #'v/cat))` all work inside the sandbox. Without this,
   docstrings stayed app-side only and the model could not introspect its
   own callable surface."
  [ext-ns sym val sym-entry]
  (let [doc      (:ext.symbol/doc sym-entry)
        arglists (:ext.symbol/arglists sym-entry)
        source   (:ext.symbol/source sym-entry)
        meta-map (cond-> {:ns ext-ns}
                   doc      (assoc :doc doc)
                   arglists (assoc :arglists arglists)
                   source   (assoc :vis/source source))]
    (if (and (map? val) (contains? val :vis.sci/macro-fn))
      (sci/new-var sym (:vis.sci/macro-fn val) (assoc meta-map :macro true))
      (sci/new-var sym val meta-map))))

(defn- extension-namespace-bindings
  [environment ns-sym alias-sym active-exts]
  (let [ext-ns (sci/create-ns ns-sym)]
    (loop [remaining active-exts
           bindings  {}
           owners    {}]
      (if-let [ext (first remaining)]
        (let [wrapped (extension/wrap-extension ext environment)
              entry-by-sym (into {} (map (juxt :ext.symbol/symbol identity)) (extension/ext-symbols ext))
              ns-bindings (into {}
                            (map (fn [[sym val]]
                                   [sym (sci-binding-var ext-ns sym val
                                          (get entry-by-sym sym))]))
                            wrapped)
              collisions (vec (filter #(contains? bindings %) (keys ns-bindings)))]
          (when (seq collisions)
            (tel/log! {:level :warn :id ::ext-symbol-collision
                       :data  {:ext       (:ext/name ext)
                               :ns        ns-sym
                               :alias     alias-sym
                               :symbols   collisions
                               :previous  (select-keys owners collisions)}
                       :msg   (str "Extension '" (:ext/name ext)
                                "' shadowed " (count collisions)
                                " active symbol(s) under alias '" alias-sym
                                "': " (str/join ", " collisions))}))
          (recur (next remaining)
            (merge bindings ns-bindings)
            (merge owners (zipmap (keys ns-bindings)
                            (repeat (:ext/name ext))))))
        bindings))))

(defn sync-active-extension-symbols!
  "Make SCI alias namespaces match active extension state.

   `install-extension!` keeps every extension row in `:extensions`, but only
   active extensions contribute callable alias symbols. Called after per-env
   installation and again at turn start so `:ext/activation-fn` changes become
   real tool availability, not just prompt visibility."
  ([environment]
   (sync-active-extension-symbols! environment (prompt/active-extensions environment)))
  ([environment active-extensions]
   (when-let [sci-ctx (:sci-ctx environment)]
     (let [installed (vec (or (some-> (:extensions environment) deref) []))
           active-set (set (map :ext/name active-extensions))]
       (doseq [{ns-sym :ns alias-sym :alias :as alias} (extension-aliases installed)]
         (let [active-for-alias (filterv (fn [ext]
                                           (and (contains? active-set (:ext/name ext))
                                             (= alias (extension/ext-alias ext))))
                                  installed)]
           (if (seq active-for-alias)
             (let [bindings (extension-namespace-bindings environment ns-sym alias-sym active-for-alias)]
               (swap! (:env sci-ctx) assoc-in [:namespaces ns-sym] bindings)
               (swap! (:env sci-ctx) update :ns-aliases assoc alias-sym ns-sym)
               (require-extension-alias! sci-ctx (last active-for-alias) ns-sym alias-sym))
             (swap! (:env sci-ctx)
               (fn [sci-env]
                 (-> sci-env
                   (update :namespaces dissoc ns-sym)
                   (update :ns-aliases dissoc alias-sym)))))))))
   environment))

(defn install-extension!
  "Register a validated extension into `environment` (per-env registration,
   distinct from the global-registry `register-extension!` defined earlier
   in this file).

   Checks `:ext/requires` - if the extension declares dependencies, all
   listed extension namespaces must already be registered. Throws on
   missing dependencies.

   If an extension with the same `:ext/name` is already registered,
   it is replaced (not duplicated). Enables hot-swap via
   `reload-extension!`.

   Returns `environment` for chaining."
  [environment ext]
  (when-not (:extensions environment)
    (anomaly/incorrect! "Invalid vis environment - missing :extensions atom"
      {:type :vis/invalid-env}))
  (when-let [requires (seq (:ext/requires ext))]
    (let [registered (into #{} (map :ext/name) @(:extensions environment))
          missing    (vec (remove registered requires))]
      (when (seq missing)
        (anomaly/incorrect!
          (str "Extension '" (:ext/name ext)
            "' requires " missing " but they are not registered. "
            "Register dependencies first.")
          {:type       :extension/missing-dependencies
           :extension  (:ext/name ext)
           :requires   (vec requires)
           :missing    missing
           :registered (vec registered)}))))
  (swap! (:extensions environment)
    (fn [exts]
      (let [ns-sym  (:ext/name ext)
            without (vec (remove #(= (:ext/name %) ns-sym) exts))]
        (conj without ext))))
  ;; Extension rows stay installed even when inactive, but callable symbol
  ;; bindings are activation-aware. Java classes/imports remain available once
  ;; an extension is installed because they are passive SCI configuration, not
  ;; model-visible tool affordances.
  (let [sci-ctx (:sci-ctx environment)]
    (when-let [classes (seq (extension/ext-classes ext))]
      (swap! (:env sci-ctx) update :classes merge (into {} classes)))
    (when-let [imports (seq (extension/ext-imports ext))]
      (swap! (:env sci-ctx) update :imports merge (into {} imports))))
  (sync-active-extension-symbols! environment)
  environment)

;; =============================================================================
;; Environment Lifecycle
;; =============================================================================

(defn create-environment
  "Creates a vis environment (component) for session lifecycle and
   querying.

   The environment holds:
     - SCI sandbox context with custom bindings + bindings cache
     - DB connection (or shared-mem datasource)
     - Router (LLM provider config)
     - Extension registry atom

   Params:
     `router` - Required. Result of `llm/make-router`.
     `opts`   - Map with `:db` and optional `:session`,
                 `:channel`, `:external-id`, `:title`.

     `:db` accepted forms:
       nil               - no DB (SCI-only execution)
       :memory           - ephemeral in-process SQLite DB
       path string       - persistent SQLite DB at path
       {:path p}         - persistent SQLite DB at path
       {:datasource ds}  - caller-owned DataSource (not closed on dispose)

   Returns the vis environment map."
  [router {:keys [db session channel external-id title workspace-id]}]
  (when-not router
    (anomaly/incorrect! "Missing router" {:type :vis/missing-router}))
  (let [depth-atom               (atom 0)
        db-info                  (persistance/db-create-connection! db)
        state-atom               (atom {:custom-bindings {}
                                        :environment     nil
                                        :session-id nil})
        environment-atom         (atom nil)
        environment-id           (str (util/uuid))
        ;; Iteration-final-answer signal. The SCI sandbox's `(done
        ;; "...")` fn `reset!`s this atom with `{:value :form-idx}`;
        ;; the iteration loop reads it back after evaluating each
        ;; iteration's forms and discards iff the form at `:form-idx`
        ;; itself errored (Option C scoping - sibling errors do NOT
        ;; gate the answer). Reset to nil before every iteration runs.
        answer-atom              (atom nil)
        ;; Form-index pointer the iteration loop reset!s before each
        ;; expression's `execute-code` call so `answer-fn` knows which
        ;; form's evaluation invoked it. Pairs with `answer-atom` to
        ;; implement Option C (scoped) discard semantics.
        current-form-idx-atom    (atom nil)
        ;; Stable per-env turn/iteration atoms. Extension symbol wrappers
        ;; close over the environment installed at creation time, so these
        ;; atoms must live on that base env and be reset per turn instead of
        ;; being introduced only on the transient turn env.
        current-iteration-atom    (atom 1)
        current-iteration-id-atom (atom nil)
        current-session-turn-id-atom (atom nil)
        current-turn-position-atom (atom nil)
        current-user-request-atom (atom nil)
        ;; Title atom: in-memory cache for the session title.
        ;; The DB column on `session_state` is the persisted
        ;; truth; this atom is the fast read path for  and
        ;; the source for the title hint / channel chrome at iteration
        ;; boundaries. `set-title!` writes both, in that order, then
        ;; broadcasts to every registered listener.
        session-title-atom               (atom (or title ""))
        root-resolved-model      (resolve-effective-model router)
        root-model               (or (:name root-resolved-model) "unknown")
        root-provider            (:provider root-resolved-model)
        ;; Snapshot a base system prompt for the session row so the
        ;; sidebar / DB inspectors have something stable to display.
        ;; Real per-turn assembly goes through `prompt/assemble-stable-prompt-messages`
        ;; with `:active-extensions`, so this snapshot is just metadata.
        system-prompt            (prompt/build-system-prompt {})
        resolved-session-id (persistance/db-resolve-session-id db-info session)
        ;; Workspace pin (1:1 with session_state, PLAN.md decision 1):
        ;;   - resuming a session       → derive workspace from its latest state
        ;;   - brand-new session        → mint a trunk workspace, pass its id
        ;;                                into db-store-session! below
        ;; db-info nil (SCI-only mode)  → skip; iteration loop never asserts
        ;;                                workspace pin when there's no DB
        active-workspace    (when db-info
                              (cond
                                ;; Resume path: the existing session already pins a
                                ;; workspace; honour it.
                                resolved-session-id
                                (some->> (persistance/db-latest-session-state-id db-info resolved-session-id)
                                  (persistance/db-workspace-for-session db-info))
                                ;; New session, caller pre-spawned a workspace
                                ;; (e.g. /workspace slash spawn-branch path).
                                workspace-id
                                (persistance/db-workspace-get db-info workspace-id)
                                ;; New session, no pre-spawn: default trunk.
                                :else
                                (workspace/ensure-trunk! db-info {})))
        session-id          (or resolved-session-id
                              (persistance/db-store-session! db-info
                                (cond-> {:channel       (or channel :tui)
                                         :external-id   external-id
                                         :model         root-model
                                         :title         title
                                         :system-prompt system-prompt
                                         :workspace-id  (:id active-workspace)}
                                  root-provider (assoc :provider root-provider))))
        ;; SCI binding for `(done "...")` - the canonical turn-
        ;; termination call. Closes over `answer-atom` AND
        ;; `current-form-idx-atom` so the iteration loop can scope
        ;; the discard check to the form that actually called this.
        ;; Returns the marker keyword so the per-form result row makes
        ;; request visible.
        answer-fn                (fn done [s]
                                   ;; Canonical final-answer shape:
                                   ;;   (done {:answer "markdown string"})
                                   ;;
                                   ;; The map form leaves room for future
                                   ;; metadata (`:format`, `:lang`, `:tags`,
                                   ;; `:attachments`, ...). The Markdown
                                   ;; string IS the answer source of truth;
                                   ;; channels derive IR via
                                   ;; `render/markdown->ir` when they need
                                   ;; layout.
                                   ;;
                                   ;; Needs-input maps stay data-shaped so
                                   ;; the prompt-flow gate reads them as
                                   ;; maps via `needs-input-answer?` /
                                   ;; `:answer/text`.
                                   ;;
                                   ;; Everything else is a programmer/model
                                   ;; error: we wrap it in a synthetic
                                   ;; Markdown answer so the loop can still
                                   ;; surface a user-visible message, and
                                   ;; the answer-validation gate may reject
                                   ;; it downstream.
                                   (let [value (cond
                                                 (needs-input-answer? s) s
                                                 (markdown-answer? s)    s
                                                 (string? s)             {:answer s}
                                                 (nil? s)                {:answer ""}
                                                 :else                   {:answer (pr-str s)
                                                                          :vis/coerced? true})]
                                     (reset! answer-atom
                                       {:value    value
                                        :position @current-form-idx-atom}))
                                   :vis/answer)
        ;; SCI binding for the session title:
        ;;   `(set-session-title! \"...\")` - writes the title through
        ;;                              to DB, syncs the in-memory
        ;;                              atom, and broadcasts
        ;;                              `:title-changed` to every
        ;;                              registered listener so
        ;;                              channels (e.g. the TUI
        ;;                              header) can refresh without
        ;;                              polling.
        ;; ONE-ARITY ONLY. There is no zero-arg reader by design: the
        ;; model has no in-sandbox read path for the title (the
        ;; `SESSION_TITLE` SYSTEM var was retired as redundant).
        ;; The foundation `title-hint` surfaces the current value
        ;; when relevant. Calling with the wrong arity raises an
        ;; `ArityException` from SCI like any other Clojure fn.
        ;; Returns `:vis/silent`: the title is visible in channel chrome
        ;; and the model trailer, but the host call itself is noise in
        ;; live progress / iteration rendering.
        session-title-fn    (fn set-session-title! [s]
                              (let [s (str s)]
                                (set-title-with-broadcast!
                                  db-info session-id
                                  session-title-atom s)
                                :vis/silent))
        satisfied-hints-atom     (atom #{})
        ;; `(satisfy-hint! :hint/id)` is silent model-visible bookkeeping.
        ;; It removes the hint id from `(get-in ctx [:session :hints])` on the next iteration;
        ;; it does not unregister the extension hook that may emit the hint
        ;; again if its runtime condition becomes true.
        satisfy-hint-fn          (fn satisfy-hint! [id]
                                   (when-not (keyword? id)
                                     (throw (ex-info "satisfy-hint! requires a keyword hint id"
                                              {:type :vis/invalid-hint-id
                                               :id id})))
                                   (swap! satisfied-hints-atom conj id)
                                   :vis/silent)
        ;; The current human turn text and engine context flow through ctx.
        env-bindings             {'done            answer-fn
                                  'set-session-title! session-title-fn
                                  'satisfy-hint!  satisfy-hint-fn}
        {:keys [sci-ctx sandbox-ns initial-ns-keys]}
        (env/create-sci-context (merge env-bindings
                                  (:custom-bindings @state-atom)))
        env (cond-> {:environment-id                    environment-id
                     :session-id                   session-id
                     :channel                           (or channel :tui)
                     :depth-atom                        depth-atom
                     :db-info                           db-info}
              ;; Workspace info attached at env-build time so the extension
              ;; wrapper's `(workspace/workspace-root env)` finds a non-blank
              ;; root the very first time it fires (PLAN.md §5).
              active-workspace
              (assoc :workspace        active-workspace
                :workspace/id     (:id active-workspace)
                :workspace/root   (:root active-workspace)
                :workspace/kind   (:kind active-workspace)
                :workspace/branch (:branch active-workspace)))
        env (assoc env
              :state-atom                        state-atom
              :sci-ctx                           sci-ctx
              :sandbox-ns                        sandbox-ns
              :initial-ns-keys                   initial-ns-keys
             ;; Long-lived per-env LRU map: `{var-name-string →
             ;; last-used-turn-pos}`. Merged from each iteration's
             ;; `:lru` after eval. Drives the trailer's live-vars
             ;; surface, which ages user vars out of the discovery
             ;; line after quiet turns.
              :def-resolve-lru-atom              (atom {})
              :router                            router
              :answer-atom                       answer-atom
              :current-form-idx-atom             current-form-idx-atom
              :current-iteration-atom            current-iteration-atom
              :current-iteration-id-atom         current-iteration-id-atom
              :current-session-turn-id-atom current-session-turn-id-atom
              :current-turn-position-atom        current-turn-position-atom
              :current-user-request-atom         current-user-request-atom
              :session-title-atom           session-title-atom
              :satisfied-hints-atom              satisfied-hints-atom
              :extensions                        (atom []))]
    (reset! environment-atom env)
    (swap! state-atom assoc :environment env :session-id session-id)
    ;; Restore persisted vars when resuming an existing session.
    (when resolved-session-id
      (try
        (env/restore-sandbox! sci-ctx db-info session-id)
        (catch Throwable t
          (tel/log! {:level :warn :id ::restore-sandbox-failed
                     :data {:error (ex-message t)
                            :session-id session-id}
                     :msg "Failed to restore sandbox from DB - starting empty"}))))
    ;; Auto-discover everything from `META-INF/vis-extension/vis.edn` on the
    ;; classpath, then install extensions in dependency order. The
    ;; same loader populates channel/command/provider/persistance
    ;; registries as a side effect; we just care about the extension
    ;; rows here.
    (extension/discover-extensions!)
    (extension/register-extensions! env install-extension!)
    env))

(defn dispose-environment!
  "Disposes a vis environment and releases resources. For persistent DBs
   (created with `:path`), data is preserved. For disposable DBs, all
   data is deleted."
  [environment]
  (when-let [db-info (:db-info environment)]
    (persistance/db-dispose-connection! db-info)))

;; =============================================================================
;; Session env cache
;; =============================================================================

;; ---------------------------------------------------------------------------
;; In-process session cache + channel utilities
;; ---------------------------------------------------------------------------

(defonce
  ^{:doc "In-process env cache.

   Keyed by `java.util.UUID` session-soul-id. Under the 1:1 session ↔
   workspace invariant (PLAN.md decision 1) this key is isomorphic to
   `(:workspace/id env)` — one cache entry = one session = one
   workspace = one SCI sandbox lineage. Lookups normalize incoming
   strings to UUID via `cache-key` so legacy string-id callers keep
   working during the UUID sweep."}
  cache (atom {}))

(defn- cache-key
  "Normalize an id-shaped value (UUID or string-UUID) to a UUID
   suitable for keying `cache`. Nil → nil so wrapped lookups stay
   honest."
  [id]
  (persistance/->uuid id))

(defn cache-env!
  "Insert `env` into the cache under `session-id` (UUID, or string
   normalized via `cache-key`). Returns `{:id <UUID> :environment env}`."
  [session-id env]
  (let [k (cache-key session-id)]
    (swap! cache assoc k {:environment env
                          :lock (java.util.concurrent.locks.ReentrantLock.)})
    {:id k :environment env}))

(defn refresh-cached-routers!
  "Reseat `:router` on every cached env's environment map.

  `create-environment` snapshots the router into
  `(:router env)` at construction time, and the iteration loop calls
  `(svar/ask-code! (:router environment) ...)` - not the global
  `router-atom`. So when a frontend changes provider
  config and rebuilds the global router, every long-lived env in the
  cache (TUI keeps one for the whole session) keeps talking to the
  *previous* model until disposed.

  Call this immediately after `rebuild-router!` so the
  next `send!` on any cached session picks up the new router."
  [router]
  (when router
    (swap! cache
      (fn [m]
        (reduce-kv
          (fn [acc id {:keys [environment] :as entry}]
            (assoc acc id
              (assoc entry :environment (assoc environment :router router))))
          {} m))))
  nil)

(defn set-provider!
  "Set the single active provider config. Persists to disk, updates
   in-memory state, rebuilds the global router, and reseats cached
   session envs. `provider` is a svar-native provider map
   `{:id :base-url :api-key :models [...]}`. Replaces an existing
   provider with the same `:id` or appends a new entry."
  [provider]
  (let [cfg     (or (config/current-config) {:providers []})
        pid     (:id provider)
        provs   (vec (:providers cfg))
        idx     (some (fn [[i p]] (when (= (:id p) pid) i))
                  (map-indexed vector provs))
        updated (if idx (assoc provs idx provider) (conj provs provider))
        prioritized (vec (cons provider (remove #(= (:id %) pid) updated)))
        new-cfg {:providers prioritized}]
    (config/save-config! new-cfg :set-provider!)
    (reset! @#'config/active-config new-cfg)
    (try (let [r (rebuild-router! new-cfg)]
           (refresh-cached-routers! r))
      (catch Exception e
        (tel/log! {:level :warn :data {:error (ex-message e)}}
          "Failed to rebuild router after provider change")))
    new-cfg))

;; ---------------------------------------------------------------------------
;; Extension hot-reload. See plan §1 Q12-Q16 + caveats.
;;
;; Surgical for `:added` / `:removed` (full side-effect cleanup,
;; symbol install). Still-present extension namespaces are delegated
;; to clj-reload after the first baseline pass, so unchanged helper
;; namespaces stop paying a full `(require :reload)` cost.
;;
;; SCI env refresh is scheduled, not performed in-place: the env that
;; called `(v/reload-extensions!)` is still executing old bindings, so
;; reset happens at the next `send!` boundary before user code runs.
;; ---------------------------------------------------------------------------

(def ^:const RELOAD_DEFAULT_TIMEOUT_MS
  "Deprecated compatibility knob for `reload-extensions!`. Env refresh no
   longer waits on busy turns; every cached env is marked dirty and rebuilt
   at its next safe `send!` boundary."
  0)

(defn- now-ms ^long [] (System/currentTimeMillis))

(def ^:private EXTENSION_RELOAD_DIRS
  ["extensions/channels/vis-channel-telegram/src"
   "extensions/channels/vis-channel-tui/src"
   "extensions/common/vis-exa/src"
   "extensions/common/vis-foundation/src"
   "extensions/common/vis-voice/src"
   "extensions/persistance/vis-persistance-sqlite/src"
   "extensions/providers/vis-provider-anthropic/src"
   "extensions/providers/vis-provider-github-copilot/src"
   "extensions/providers/vis-provider-openai-codex/src"
   "extensions/providers/vis-provider-standard/src"
   "extensions/providers/vis-provider-zai/src"])

(defn- user-extension-roots
  []
  (distinct
    (keep identity
      [(some-> (System/getProperty "user.dir")
         (File. ".vis/vis-extensions")
         .getPath)
       (some-> (System/getProperty "user.home")
         (File. ".vis/vis-extensions")
         .getPath)])))

(defn- extension-src-dirs-under
  [root]
  (let [root-file (File. ^String root)]
    (if-not (and (.exists root-file) (.isDirectory root-file))
      []
      (->> (.listFiles root-file)
        (filter #(.isDirectory ^File %))
        (keep (fn [^File extension-dir]
                (let [deps-file (File. extension-dir "deps.edn")
                      src-dir   (File. extension-dir "src")]
                  (when (and (.isFile deps-file) (.isDirectory src-dir))
                    (.getPath src-dir)))))
        sort
        vec))))

(defonce ^:private extension-reloader-state
  (atom nil))

(defn- existing-extension-reload-dirs
  []
  (vec
    (filter (fn [path]
              (let [f (File. ^String path)]
                (and (.exists f) (.isDirectory f))))
      (distinct
        (concat EXTENSION_RELOAD_DIRS
          (mapcat extension-src-dirs-under (user-extension-roots)))))))

(defn- ensure-extension-reloader!
  []
  (let [dirs (existing-extension-reload-dirs)]
    (when (seq dirs)
      (if (= dirs (:dirs @extension-reloader-state))
        :changed
        (do
          (clj-reload/init {:dirs dirs
                            :output :quiet
                            :no-reload '#{user}})
          (reset! extension-reloader-state {:dirs dirs})
          :all)))))

(defn- extension-loader-nses
  "Namespaces that load/register `ext`. Most extensions use their
   `:ext/name` directly. Lightweight registrars and provider bundles
   register one or more logical extension ids from a separate manifest ns;
   those must declare `:ext/source-nses` so reload diffs against the loader ns,
   not the logical extension id."
  [ext]
  (set (extension/ext-source-nses ext)))

(defn- diff-extensions
  "Compute the F1-lite diff between the current in-memory
   `extension-registry` and a freshly-scanned `manifests` map
   (manifest-id -> entry, where entry has `:nses`).

   Returns `{:added [...] :removed [...] :reloaded [...]}`.

   `:added` / `:reloaded` are manifest loader namespaces to require.
   `:removed` is logical `:ext/name` ids to deregister.

   This distinction is load-bearing: SQLite registers logical extension
   `com.blockether.vis.ext.persistance-sqlite.core` from lightweight loader
   `com.blockether.vis.ext.persistance-sqlite.registrar`; provider bundles
   similarly register plan-specific logical ids from one loader ns. Treating
   the logical id as the loader deregisters live side effects and skips the
   registrar re-exec, leaving e.g. `:sqlite` unregistered."
  [registered manifests]
  (let [registered-loader-ns (set (mapcat extension-loader-nses registered))
        manifest-ns          (set (mapcat :nses (vals manifests)))
        added                (vec (sort (set/difference manifest-ns registered-loader-ns)))
        removed              (->> registered
                               (filter #(empty? (set/intersection (extension-loader-nses %) manifest-ns)))
                               (map :ext/name)
                               sort
                               vec)
        reloaded             (vec (sort (set/intersection registered-loader-ns manifest-ns)))]
    {:added added :removed removed :reloaded reloaded}))

(defn- record-error
  "Append a `:phase`-tagged error to the orchestrator's accumulator."
  [errors-atom ns-sym phase ^Throwable t]
  (swap! errors-atom conj
    {:ns          ns-sym
     :phase       phase
     :reason      (or (ex-message t) (str t))
     :stack-trace (with-out-str (.printStackTrace t (java.io.PrintWriter. *out*)))}))

(defn- require-ns!
  "Require a namespace, optionally with `:reload`. Returns nil on
   success or the Throwable on failure."
  [ns-sym reload?]
  (try
    (if reload?
      (require ns-sym :reload)
      (require ns-sym))
    nil
    (catch Throwable t t)))

(defn- reload-extension-namespaces!
  [reload-candidates errors]
  (if-let [plan (ensure-extension-reloader!)]
    (let [started (now-ms)
          result  (clj-reload/reload {:throw false
                                      :log-fn (fn [& _])
                                      :only plan})
          failed  (:failed result)]
      (when failed
        (record-error errors failed :clj-reload (:exception result)))
      {:engine :clj-reload
       :plan plan
       :unloaded (vec (:unloaded result))
       :loaded (vec (:loaded result))
       :failed failed
       :duration-ms (- (now-ms) started)})
    (let [started (now-ms)]
      (doseq [ns-sym reload-candidates]
        (when-let [t (require-ns! ns-sym true)]
          (record-error errors ns-sym :require t)))
      {:engine :require-reload
       :plan :all
       :unloaded []
       :loaded (vec reload-candidates)
       :failed nil
       :duration-ms (- (now-ms) started)})))

(defn- schedule-cached-env-refresh!
  [id reason reload-id]
  (let [k (cache-key id)]
    (swap! cache update k
      (fn [entry]
        (cond-> entry
          entry
          (assoc :env-refresh {:status :scheduled
                               :reason reason
                               :reload-id reload-id
                               :scheduled-at-ms (now-ms)}))))
    k))

(defn- refresh-cached-env-if-needed!
  "Called with the session lock held, immediately before a turn starts.
   If `reload-extensions!` marked this env dirty, rebuild the SCI env now so
   the just-finished IR/render path never races its own symbol table."
  [id entry]
  (let [k       (cache-key id)
        entry*  (or (get @cache k) entry)
        refresh (:env-refresh entry*)]
    (if (= :scheduled (:status refresh))
      (let [old-env (:environment entry*)
            title   (some-> (:session-title-atom old-env) deref)
            new-env (create-environment (get-router)
                      (cond-> {:db (config/resolve-db-spec)
                               :session k}
                        (:channel old-env) (assoc :channel (:channel old-env))
                        title              (assoc :title title)))
            updated (assoc entry*
                      :environment new-env
                      :env-refresh (assoc refresh
                                     :status :refreshed
                                     :refreshed-at-ms (now-ms)))]
        (try (dispose-environment! old-env)
          (catch Throwable t
            (tel/log! {:level :warn :id ::env-refresh-dispose-failed
                       :data {:session-id k
                              :error (ex-message t)}})))
        (swap! cache assoc k updated)
        updated)
      entry*)))

(defn reload-extensions!
  "Re-discover extensions on the classpath, apply added/removed lifecycle
   changes, then ask clj-reload to reload only changed already-loaded
   extension namespaces (first call establishes the baseline with :all).

   Cached SCI envs are not mutated while a turn is executing. They are
   marked with `:env-refresh {:status :scheduled ...}` and rebuilt at the
   next `send!` boundary before user code runs.

   Returns structured data for renderers:
     {:added [...]
      :removed [...]
      :reloaded [...]
      :unchanged [...]
      :reload-candidates [...]
      :namespace-reload {:engine :clj-reload :plan :changed ...}
      :env-refresh {:status :scheduled :scheduled N :when :before-next-turn}
      :errors [...]
      :duration-ms 123}"
  ([] (reload-extensions! {}))
  ([_opts]
   (let [start         (now-ms)
         errors        (atom [])
         ;; Step 1: fresh scan.
         _             (try ((requiring-resolve
                               'com.blockether.vis.internal.manifest/rediscover!))
                         (catch Throwable t
                           (record-error errors :scan :require t)))
         manifests     (try ((requiring-resolve
                               'com.blockether.vis.internal.manifest/scan-extensions!))
                         (catch Throwable t
                           (record-error errors :scan :require t)
                           {}))
         ;; Step 2: diff.
         registered    (extension/registered-extensions)
         {:keys [added removed reloaded]} (diff-extensions registered manifests)
         ;; Step 3a: :removed first - pull side effects before any
         ;; new register-extension! could clash with their dispatch.
         _             (doseq [ns-sym removed]
                         (try (extension/deregister-extension! ns-sym)
                           (catch Throwable t
                             (record-error errors ns-sym :deregister t))))
         ;; Step 3b: :added - require + the namespace's top-level form
         ;; calls `register-extension!` itself.
         _             (doseq [ns-sym added]
                         (when-let [t (require-ns! ns-sym false)]
                           (record-error errors ns-sym :require t)))
         ;; Step 3c: still-present namespaces - clj-reload reloads only
         ;; changed loaded files after the first :all baseline.
         namespace-reload (reload-extension-namespaces! reloaded errors)
         loaded-nses      (set (:loaded namespace-reload))
         unloaded-nses    (set (:unloaded namespace-reload))
         failed-nses      (set (keep identity [(:failed namespace-reload)]))
         candidate-nses   (set reloaded)
         reloaded*        (if (= :require-reload (:engine namespace-reload))
                            (vec reloaded)
                            (vec (sort (set/intersection candidate-nses loaded-nses))))
         failed*          (vec (sort (set/difference
                                       (set/intersection candidate-nses
                                         (set/union unloaded-nses failed-nses))
                                       (set reloaded*))))
         unchanged        (vec (sort (set/difference candidate-nses (set reloaded*) (set failed*))))
         ;; Step 4: schedule SCI env refresh. No lock wait; next `send!`
         ;; rebuilds each dirty env before user code executes.
         reload-id        (str (util/uuid))
         envs-snapshot    (vec @cache)
         env-scheduled    (atom [])]
     (doseq [[id _entry] envs-snapshot]
       (try
         (swap! env-scheduled conj (schedule-cached-env-refresh! id :extension-reload reload-id))
         (catch Throwable t
           (record-error errors id :env-refresh-schedule t))))
     (let [duration (- (now-ms) start)]
       {:added                added
        :removed              removed
        :reloaded             reloaded*
        :failed               failed*
        :unchanged            unchanged
        :reload-candidates    reloaded
        :namespace-reload     namespace-reload
        :reload-engine        (:engine namespace-reload)
        :reload-plan          (:plan namespace-reload)
        :errors               @errors
        :env-refresh          {:status :scheduled
                               :scheduled (count @env-scheduled)
                               :env-ids @env-scheduled
                               :when :before-next-turn
                               :reload-id reload-id}
        ;; Compatibility fields for old renderers/callers.
        :envs-reseated        0
        :env-reseat-deferred  @env-scheduled
        :env-reseat-skipped   []
        :duration-ms          duration
        :blocked-ms           0}))))

(defn- open-env!
  [id {:keys [channel external-id title workspace-id]}]
  (let [router (get-router)
        env    (create-environment router
                 (cond-> {:db (config/resolve-db-spec)}
                   id          (assoc :session id)
                   channel     (assoc :channel channel)
                   external-id (assoc :external-id external-id)
                   title       (assoc :title title)
                   workspace-id (assoc :workspace-id workspace-id)))]
    env))

(defn- ensure-env!
  [id]
  (let [k (cache-key id)]
    (if-let [entry (get @cache k)]
      entry
      (let [env (open-env! k {})]
        (swap! cache
          (fn [m]
            (if (contains? m k)
              m
              (assoc m k {:environment env
                          :lock (java.util.concurrent.locks.ReentrantLock.)}))))
        (get @cache k)))))

(defn db-info
  "Return the process-wide shared DB connection bound to
   `(config/resolve-db-spec)`. Thin wrapper over
   `persistance.core/db-shared-connection!` that fills in the default db-spec
   so frontend callers stay clear of config resolution."
  []
  (persistance/db-shared-connection! (config/resolve-db-spec)))

(defn create!
  "Create a brand-new session.

   Opts (all optional):
     :title         display title
     :external-id   channel-specific external id
     :workspace-id  pre-spawned workspace to pin the new session to
                    (1:1, PLAN.md decision 1). When omitted, a trunk
                    workspace is auto-minted in create-environment."
  ([channel] (create! channel nil))
  ([channel {:keys [title external-id workspace-id]}]
   (let [env  (open-env! nil (cond-> {:channel     channel
                                      :external-id (some-> external-id str)
                                      :title       title}
                               workspace-id (assoc :workspace-id workspace-id)))
         id   (:session-id env)
         _    (cache-env! id env)]
     {:id           id                ; UUID
      :channel      channel
      :external-id  (some-> external-id str)
      :title        title
      :workspace-id (:workspace/id env)})))

(defn by-id
  "Return the session record (UUID `:id`) or nil."
  [id]
  (when-let [session (persistance/db-get-session (db-info) id)]
    {:id            (:id session)       ; UUID
     :channel       (:channel session)
     :external-id   (:external-id session)
     :system-prompt (:system-prompt session)
     :model         (:model session)
     :title         (:title session)
     :created-at    (:created-at session)}))

(defn by-channel
  [channel]
  (mapv (fn [c]
          {:id          (:id c)         ; UUID
           :channel     (:channel c)
           :external-id (:external-id c)
           :title       (:title c)
           :created-at  (:created-at c)})
    (persistance/db-list-sessions (db-info) channel)))

(defn for-telegram-chat!
  [chat-id]
  (let [ext (str chat-id)]
    (or (when-let [id (persistance/db-find-session-by-external (db-info) :telegram ext)]
          (by-id id))
      (create! :telegram {:external-id ext}))))

;; =============================================================================
;; Host title setter + public env accessor
;; =============================================================================

(defn env-for
  [id]
  (:environment (ensure-env! id)))

(defn set-title!
  "Host-driven title change. Resolves the live env (if any) so the
   in-memory atom + listener fan-out stay in sync; falls back to a
   plain DB write when no env is live for this session (e.g.
   `vis sessions` rename ops)."
  [id title]
  (let [env (env-for id)]
    (set-title-with-broadcast! (or (:db-info env) (db-info))
      id
      (:session-title-atom env)
      title))
  nil)

(defn send!
  ([id messages] (send! id messages {}))
  ([id messages opts]
   (let [{:keys [^java.util.concurrent.locks.ReentrantLock lock] :as entry}
         (ensure-env! id)
         message-vec (if (string? messages) [(svar/user messages)] messages)]
     ;; ReentrantLock keeps one turn per session. Extension reload marks
     ;; envs dirty; actual SCI reset happens here, after prior IR/render is
     ;; finished and before the next user code executes.
     (.lock lock)
     (try
       (let [{:keys [environment]} (refresh-cached-env-if-needed! id entry)]
         (turn! environment message-vec opts))
       (finally (.unlock lock))))))

(defn close!
  [id]
  (let [k (cache-key id)]
    (when-let [{:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}
               (clojure.core/get @cache k)]
      (.lock lock)
      (try
        (try (dispose-environment! environment) (catch Exception _ nil))
        (finally (.unlock lock))))
    (swap! cache dissoc k)))

(defn delete!
  [id]
  (close! id)
  (let [d (db-info)]
    (try (persistance/db-delete-session-tree! d id)
      (catch Exception _ nil))))

(def ^:private ORPHAN_INTERRUPTED_ANSWER
  "Warning: Turn interrupted - the server was restarted before this answer could finalize. Re-send the message to retry.")

(defn db-sweep-orphaned-running-turns!
  "Mark every `:running` turn as `:interrupted`. Run at process start
   to clean up turns that crashed or were killed mid-write so the next
   turn's handover digest renders the right outcome instead of guessing.
   Returns the number of turns swept."
  ([] (db-sweep-orphaned-running-turns! (db-info)))
  ([db]
   (let [orphans (try (persistance/db-list-session-turns-by-status db :running)
                   (catch Exception _ []))]
     (doseq [{:keys [id iteration-count duration-ms]} orphans]
       (try
         (persistance/db-update-session-turn! db id
           {:answer          ORPHAN_INTERRUPTED_ANSWER
            :iteration-count (or iteration-count 0)
            :duration-ms     (or duration-ms 0)
            :status          :interrupted
            :prior-outcome   :cancelled})
         (catch Exception _ nil)))
     (count orphans))))

(defn close-all!
  []
  (doseq [[_ {:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}] @cache]
    (.lock lock)
    (try
      (try (dispose-environment! environment) (catch Exception _ nil))
      (finally (.unlock lock))))
  (reset! cache {})
  (persistance/db-dispose-shared-connection!))
