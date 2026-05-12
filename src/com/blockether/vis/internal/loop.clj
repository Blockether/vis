(ns com.blockether.vis.internal.loop
  "vis iteration runtime + conversations - the library surface.

   Owns:
     - Query runtime settings (`*rlm-context*`, `*eval-timeout-ms*`, ...)
     - Single-iteration runner (`run-iteration`)
     - Multi-iteration turn engine + router cache (`turn!`,
       `get-router`, `rebuild-router!`)
     - Environment lifecycle (`create-environment`, `dispose-environment!`)
     - Conversation env cache (`create!`, `by-id`, `by-channel`,
       `send!`, `close!`, `delete!`, ...)

   The binary entry point (`-main`), the persistence-backed Telemere
   `:db` handler, the one-shot agent helper, and every built-in CLI
   command live in `com.blockether.vis.internal.main`. Channels and embedded
   consumers reach this namespace directly for `send!` / `turn!` /
   `create!` / `create-environment`; the binary reaches it through
   `main.clj`.

   Prompt / env / persistance / config / extension / error are all
   required directly. The SCI sandbox machinery lives in
   `com.blockether.vis.internal.env`; system-prompt assembly +
   per-iteration context blocks in `com.blockether.vis.internal.prompt`;
   the storage facade in `com.blockether.vis.internal.persistance`;
   configuration + active provider state in
   `com.blockether.vis.internal.config`; the extension subsystem in
   `com.blockether.vis.internal.extension`."
  (:refer-clojure)
  (:require
   [clojure.set :as set]
   [charred.api :as json]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.svar.core :as svar]
   [com.blockether.svar.internal.codes :as svar-codes]
   [com.blockether.svar.internal.llm :as svar-llm]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.svar.internal.util :as util]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.cancellation :as cancellation]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.error :as error]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.render :as render]
   [com.blockether.vis.internal.parse-diagnose :as parse-diagnose]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.registry :as registry]
   [com.blockether.vis.internal.skills :as skills]
   [edamame.core :as edamame]
   [sci.core :as sci]
   [taoensso.telemere :as tel])
  (:import
   [com.oakmac.parinfer Parinfer ParinferResult]
   [java.security MessageDigest]
   [java.util.concurrent ConcurrentHashMap Semaphore]))

;; ===========================================================================

;; =============================================================================
;; Query runtime settings
;; =============================================================================

;; =============================================================================
;; Eval timeout
;; =============================================================================

(def DEFAULT_EVAL_TIMEOUT_MS
  "Default timeout in milliseconds for code evaluation in the SCI sandbox."
  120000)

(def MIN_EVAL_TIMEOUT_MS
  "Floor for :eval-timeout-ms. 3 s gives filesystem tools (grep, list-dir)
   headroom on medium-sized repos. Below about 1 s nearly every grep timed out
   at the race boundary; 3 s leaves comfortable margin without masking
   genuine infinite loops."
  3000)

(def MAX_EVAL_TIMEOUT_MS
  "Hard ceiling for :eval-timeout-ms to prevent runaway SCI futures.
   30 minutes - anything longer is a bug, not a feature."
  (* 30 60 1000))

(def ^:dynamic *eval-timeout-ms*
  "Dynamic timeout in milliseconds for SCI code evaluation. Bound per turn!
   call via :eval-timeout-ms. Nested turns inherit the outer binding.
   Clamped at the turn API boundary to
   [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS]."
  DEFAULT_EVAL_TIMEOUT_MS)

(defn clamp-eval-timeout-ms
  "Clamp a candidate eval timeout to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS].
   `candidate` may be nil - callers should resolve the fallback before calling.
   Accepts any integer, coerces to long. Prevents runaway SCI futures from
   absurdly high values and sub-second timeouts from absurdly low values."
  [candidate]
  (-> candidate long (max MIN_EVAL_TIMEOUT_MS) (min MAX_EVAL_TIMEOUT_MS)))

;; =============================================================================
;; Concurrency settings
;; =============================================================================

(def DEFAULT_CONCURRENCY
  "Default concurrency settings. Applied when :concurrency is absent from turn!."
  {:max-parallel-llm 8
   :http-timeout-ms  20000})

(def ^:dynamic *concurrency*
  "Merged concurrency settings for the current turn! process."
  DEFAULT_CONCURRENCY)

;; =============================================================================
;; RLM debug context
;; =============================================================================

(def ^:dynamic *rlm-context*
  "Dynamic context for RLM debug logging. Bind with
   {:rlm-debug? true :rlm-phase :phase-name :rlm-environment-id \"...\"}."
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

(def ^:private parse-repair-timeout-ms
  "Hard wall-clock budget for one parser repair attempt. A malformed model
   response must not trap the turn in parinfer/quote repair for many seconds;
   after this budget the repair is cancelled and the next repair (or normal
   parse error path) proceeds."
  1000)

(defn normalize-reasoning-level [v]
  (svar/normalize-reasoning-level v))

(defn reasoning-level-for-errors [base consecutive-errors]
  (cond
    (<= (long consecutive-errors) 0) base
    (= 1 (long consecutive-errors)) (if (= base :quick) :balanced :deep)
    :else :deep))

(defn- github-copilot-claude-model?
  [resolved-model]
  (and (contains? #{:github-copilot-individual :github-copilot-business}
         (:provider resolved-model))
    (boolean (re-find #"(?i)claude" (str (:name resolved-model))))))

(def ^:private casual-request-pattern
  #"(?iu)^\s*(hi|hey|hello|yo|sup|siema|cześć|czesc|hej|dzień dobry|dzie dobry|thanks|thank you|thx|ok|okay|👍|👋|ping)[\s!.?,]*\s*$")

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

(defn answer-str
  "Render the answer value to a flat string for legacy callers (logs,
   error formatting, plain-text channel paths). New code should call
   `(render/render v :plain)` or another flavor directly.

   Bypasses the IR pipeline only for needs-input legacy maps so the
   prompt-flow gate keeps reading `:answer/text` without a render hop."
  [answer]
  (let [v (:result answer answer)]
    (cond
      (needs-input-answer? v) (:answer/text v)
      :else                   (render/render v :plain))))

(defn append-runtime-appendices
  "Canonicalize the final answer value before any channel or persistence
   boundary sees it. This is the answer-IR choke point: lazy seqs inside
   Hiccup children are safely bounded by `render/->ast`, malformed Hiccup is
   normalized, and already-canonical IR stays identity-preserved.

   Needs-input maps stay data-shaped so the prompt-flow gate can still read
   `:answer/text` without a render hop."
  [_environment answer _answer-value]
  (if (needs-input-answer? answer)
    answer
    (render/->ast answer)))

(def edamame-opts
  {:all true
   :readers (fn [_tag] (fn [val] (list 'do val)))})

(defn- check-syntax [code]
  (edamame/parse-string-all code edamame-opts))

(defn- check-bare-list [forms]
  (let [first-form (first forms)]
    (when (and (= 1 (count forms))
            (list? first-form) (seq first-form)
            (let [head (first first-form)]
              (not (or (symbol? head) (keyword? head)
                     (list? head) (set? head) (map? head) (vector? head)))))
      (str "Bare list literal: " (pr-str first-form)
        ". Quote it: '(" (str/join " " first-form) ")"))))

(defn- parse-clojure-syntax [code]
  (try
    (let [forms (check-syntax code)]
      (or (check-bare-list forms)
        nil))
    (catch Throwable e
      (ex-message e))))

(defn- string-as-fn-call?
  "True when `form` is a list whose head is a String literal -- a structural
   shape that has no legitimate use in Clojure (strings are not IFns) and
   produces a runtime ClassCastException at the call site. Conv 0d25a3e1
   shipped `(v/bold (\"...\"))` from JS/Python paren leakage; the reader
   accepts it but eval crashes."
  [form]
  (and (seq? form)
    (string? (first form))))

(defn- answer-form?
  "True for `(turn-answer! ...)` calls, regardless of namespace. The lint pass
   below only walks INSIDE these forms because (turn-answer! ...) is the
   user-facing answer surface where shapes like `(v/bold (\"x\"))`
   are unambiguously broken. Outside (turn-answer! ...), Clojure source can
   legitimately call into anything, so we don't second-guess it."
  [form]
  (and (seq? form)
    (symbol? (first form))
    (= "turn-answer!" (name (first form)))
    (nil? (namespace (first form)))))

(defn- find-string-as-fn-in-answer
  "Walk every `(turn-answer! ...)` subtree in `forms` and return the first list
   whose head is a String literal, or nil. The walk is scoped to answer
   bodies only -- regular Clojure code outside (turn-answer! ...) is left
   alone."
  [forms]
  (some (fn [top]
          (some (fn [node]
                  (when (string-as-fn-call? node) node))
            (when (coll? top) (tree-seq coll? seq top))))
    (filter answer-form?
      (mapcat #(tree-seq coll? seq %) forms))))

(defn- string-as-fn-mistake-message
  [forms]
  (when-let [bad (find-string-as-fn-in-answer forms)]
    (str "String-as-fn lint rejected this iteration before evaluation: "
      "inside (turn-answer! ...) a Clojure list has a String literal as its head -- "
      (pr-str bad)
      " -- which would throw ClassCastException (String cannot be cast to IFn) at runtime. "
      "Most often this is JS/Python call-paren syntax leaking in: write "
      "`(v/bold \"text\")`, not `(v/bold(\"text\"))`. Recovery: drop the inner "
      "parentheses around the string literal so the helper receives the "
      "string directly.")))

;; Forward declare — `raw-markdown-fence-leak-error` is defined ~900
;; lines below in the preflight section. `detect-common-mistakes`
;; (just below) needs it on the lint path; sorting defs would push
;; the lint section past the preflight, splitting two related
;; iteration-loop pieces. The declare is the cleaner exception per
;; AGENTS.md S4.
(declare raw-markdown-fence-leak-error)

(defn- detect-common-mistakes
  "Pre-eval lint pass. Returns an error STRING when the source carries
   a structural shape that would crash at SCI eval time. Returns nil
   when source is clean enough to hand to the parser. Currently:

     - Raw Markdown fence leaked into :code (` ``` ... `). The reader
       would fall into a re-parse loop and StackOverflow; rejecting
       early gives a typed engine error instead of a JVM crash.

     - String-as-fn calls, e.g. `(v/bold (\"x\"))` from JS/Python paren
       leakage — runtime would throw `String cannot be cast to IFn`.

   Reader / parser errors stay the responsibility of
   `parse-clojure-syntax`; this lint only fires on cleanly-parsed
   source for the string-as-fn check, but the fence check matches
   raw text first since it short-circuits the parser entirely."
  [code]
  (when (string? code)
    (or (raw-markdown-fence-leak-error code)
      (try
        (let [forms (check-syntax code)]
          (string-as-fn-mistake-message forms))
        (catch Throwable _
          ;; Reader failure — defer to parse-clojure-syntax / extension rescue.
          nil)))))

(defn- edamame-parses?
  "Predicate the repair search hands to `parse-diagnose/try-quote-rebalance`.
   Returns true when `src` survives `edamame/parse-string-all` with
   the same opts the rest of this ns uses; false on any throw."
  [^String src]
  (try
    (edamame/parse-string-all src edamame-opts)
    true
    (catch Throwable _ false)))

(defn quote-rebalance
  "Companion to `parinfer-rebalance` for the OTHER common shape of
   malformed Clojure source: an unbalanced double-quote string.

   Parinfer doesn't fix string delimiters. We do:

     1. Quick-out when the unescaped-quote count is even.
     2. Find the line where the running count first goes odd.
     3. Try removing each unescaped `\"` on that line, ONE AT A
        TIME (most common LLM mistake - conversation cf9e29b5).
     4. As a fallback, try appending `\"` at end of that line
        (the \"missing close-quote\" case).
     5. Each candidate goes to edamame; first one that parses wins.

   Returns the rebalanced source string, or `nil` when nothing in
   the search produces a parsable variant. Same contract as
   `parinfer-rebalance` so the two slot into the parse pipeline
   the same way."
  ^String [^String source]
  (parse-diagnose/try-quote-rebalance source edamame-parses?))

(defn parinfer-rebalance
  "First-line repair for malformed Clojure source. Calls parinfer's
   indent-mode auto-balancer (a battle-tested 1100-line algo from
   the parinfer.kt port) and returns the rebalanced source iff:

     1. Parinfer reports `success`, AND
     2. The result is actually different from the input, AND
     3. The rebalanced source NOW parses cleanly via edamame
        (parinfer's `success` flag means \"the algorithm finished\";
        we still need edamame to confirm the result is a valid
        program).

   Returns `nil` when parinfer can't help. Pure; no side effects.
   Public so tests can pin behavior on the three observed real-world
   failure cases (extra-close, delim-type-swap, missing-close)."
  ^String [^String source]
  (try
    (let [^ParinferResult r (Parinfer/indentMode source nil nil nil false)]
      (when (.success r)
        (let [rebalanced (.text r)]
          (when (and rebalanced
                  (not= rebalanced source)
                  ;; Re-feed to edamame; only accept the repair if
                  ;; it actually produces a valid program. Parinfer
                  ;; is permissive about what counts as \"success\";
                  ;; edamame is the authoritative arbiter.
                  (try (edamame/parse-string-all rebalanced edamame-opts) true
                    (catch Throwable _ false)))
            rebalanced))))
    (catch Throwable _ nil)))

(defn- try-repair-with-timeout
  "Run one parser repair with a hard wall-clock budget. Returns the repaired
   source string, or nil on no-op/error/timeout. The worker is cancelled on
   timeout so a bad response cannot keep the iteration stuck in repair code."
  [repair-name repair-fn src parse-error]
  (let [started-ns (System/nanoTime)
        fut        (future
                     (try
                       {:status :ok :value (repair-fn src parse-error)}
                       (catch Throwable t
                         {:status :error :error t})))
        out        (deref fut parse-repair-timeout-ms ::timeout)
        ms         (elapsed-ms started-ns)]
    (cond
      (= ::timeout out)
      (do
        (future-cancel fut)
        (log-stage! :parse-repair/timeout nil
          {:repair repair-name
           :timeout-ms parse-repair-timeout-ms
           :duration-ms ms
           :source-length (count (or src ""))
           :parse-error parse-error})
        nil)

      (= :error (:status out))
      (do
        (log-stage! :parse-repair/error nil
          {:repair repair-name
           :duration-ms ms
           :source-length (count (or src ""))
           :parse-error parse-error
           :error (format-exception-short (:error out))})
        nil)

      (string? (:value out))
      (do
        (log-stage! :parse-repair/applied nil
          {:repair repair-name
           :duration-ms ms
           :source-length (count (or src ""))
           :result-length (count (:value out))
           :parse-error parse-error})
        (:value out))

      :else
      (do
        (when (> ms 100.0)
          (log-stage! :parse-repair/noop nil
            {:repair repair-name
             :duration-ms ms
             :source-length (count (or src ""))
             :parse-error parse-error}))
        nil))))

(defn split-top-level-forms
  "Parse `code` (a Clojure source string) into top-level forms. Returns a
   vector of `{:expr str :repaired? bool}` maps, one per form, where
   `:expr` is the verbatim source slice for that form INCLUDING any
   leading `;; comments` and `#_` discards on prior lines (so the
   model's natural `;; what this does\n(def ...)` paragraphing
   survives into `<journal>` instead of getting silently stripped).

   Repair pipeline when source fails edamame: bounded loop over small,
   deterministic repairs, re-parsing after every successful rewrite. This lets
   independent fixes compose (e.g. answer-string `\\e` rescue, then parinfer).
   On final failure return `[nil parse-error]` and let the caller
   (`execute-code`) dispatch to the extension rescue chain
   (`try-extension-parse-rescue`).

   Empty / whitespace-only / comment-only input returns `[[] nil]`."
  [code]
  (let [code-str (or code "")]
    (letfn
      [(parse-and-slice [src repaired?]
         (let [forms (edamame/parse-string-all src edamame-opts)
               line-starts (let [lines (str/split src #"\n" -1)]
                             (->> lines
                               (reductions (fn [acc l] (+ acc (count l) 1)) 0)
                               vec))
               n (count line-starts)
               offset-of (fn [row col]
                           (when (and row col)
                             (let [line (max 0 (dec row))]
                               (when (< line n)
                                 (+ (nth line-starts line) (max 0 (dec col)))))))
               ;; Per-form bounds. Map shape (extended PLAN §2.6 +
               ;; §7.3.7) so callers needing block-global coordinate
               ;; translation (ex->op-error) can read `:start-row` /
               ;; `:start-col` / `:end-row` / `:end-col` directly.
               ;; Byte offsets remain for slice extraction.
               form-bounds (mapv (fn [f]
                                   (when-let [m (and (instance? clojure.lang.IObj f) (meta f))]
                                     (let [s (offset-of (:row m) (:col m))
                                           e (offset-of (:end-row m) (:end-col m))]
                                       (when (and s e (>= s 0) (<= e (count src)) (<= s e))
                                         {:start     s
                                          :end       e
                                          :start-row (:row m)
                                          :start-col (:col m)
                                          :end-row   (:end-row m)
                                          :end-col   (:end-col m)}))))
                             forms)
               ;; For form K, two slices:
               ;;   `:comment` = the GAP (end-of-K-1 .. start-of-K).
               ;;     Captures any `;; ...` / `#_(...)` / blank lines
               ;;     that sat between the previous form and this
               ;;     one. Trimmed; nil when empty.
               ;;   `:expr`    = the form's own bounds. The actual
               ;;     Clojure code, no preamble.
               ;; Persisting these as TWO fields (instead of glued
               ;; into one `:expr` blob) keeps the executable code
               ;; clean for display while still preserving the
               ;; model's authored prose alongside each form.
               comment-slice (fn [idx]
                               (when-let [bnd (nth form-bounds idx nil)]
                                 (let [start    (:start bnd)
                                       prev-end (or (some-> (nth form-bounds (dec idx) nil)
                                                      :end)
                                                  0)]
                                   (when (> start prev-end)
                                     (let [trimmed (str/trim (subs src prev-end start))]
                                       (when (pos? (count trimmed))
                                         trimmed))))))
               expr-slice (fn [idx]
                            (when-let [{:keys [start end]} (nth form-bounds idx nil)]
                              (subs src start end)))]
           (mapv (fn [idx form]
                   (let [expr-src (or (expr-slice idx)
                                    (binding [*print-meta* false] (pr-str form)))
                         comment  (comment-slice idx)
                         bnd      (nth form-bounds idx nil)]
                     (cond-> {:expr (str/trim (str expr-src))
                              :form form}
                       comment    (assoc :comment comment)
                       repaired?  (assoc :repaired? true)
                       bnd        (assoc :start-row (:start-row bnd)
                                    :start-col (:start-col bnd)
                                    :end-row   (:end-row   bnd)
                                    :end-col   (:end-col   bnd)))))
             (range) forms)))
       (answer-escape-repair [src parse-error]
         ;; Models often write terminal notation like `\\e[200~` in
         ;; final-answer prose. Clojure rejects that before IR exists,
         ;; so rewrite to a literal visible `\\\\e[200~` only inside
         ;; `(turn-answer! ...)`. Accept this partial repair even when
         ;; another parser error remains; the outer loop will re-parse.
         (parse-diagnose/try-answer-escape-rescue src parse-error (constantly true)))
       (first-repair-candidate [src parse-error seen repair-fns]
         (some (fn [{:keys [name f]}]
                 (when-let [fixed (try-repair-with-timeout name f src parse-error)]
                   (when-not (or (= fixed src) (contains? seen fixed))
                     fixed)))
           repair-fns))]
      (let [repair-fns [{:name :answer-escape-e
                         :f answer-escape-repair}
                        {:name :parinfer
                         :f (fn [src _parse-error] (parinfer-rebalance src))}
                        {:name :quote-rebalance
                         :f (fn [src _parse-error] (quote-rebalance src))}]]
        (loop [src       code-str
               repaired? false
               seen      #{code-str}
               budget    (count repair-fns)]
          (let [parsed (try
                         {:entries (parse-and-slice src repaired?)}
                         (catch Throwable err
                           {:error (ex-message err)}))]
            (if-let [entries (:entries parsed)]
              [entries nil]
              (let [parse-error (:error parsed)]
                (if (zero? budget)
                  [nil parse-error]
                  (if-let [fixed (first-repair-candidate src parse-error seen repair-fns)]
                    ;; After each accepted repair, go back to parse. If parsing
                    ;; still fails, scan repairs again from the first fixer.
                    (recur fixed true (conj seen fixed) (dec budget))
                    [nil parse-error]))))))))))

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

(defn- literal-code-block-error [expr]
  (cond
    (bare-string-code-block? expr)
    "Bare string literal in :code. Prose belongs in :answer (the loop auto-detects plain text), not in :code."

    (markdown-fence-block? expr)
    "Raw Markdown fence leaked into :code (` ```... `). Remove the fence marker and keep only executable Clojure forms inside the code block."

    (comment-only-block? expr)
    "Code block contains only comments / discards (`;;` or `#_`) and no executable form. Add an expression to evaluate, or drop the block entirely."))

(defn- run-sci-code [sci-ctx code & {:keys [sandbox-ns tool-event-fn env]}]
  (let [stdout-writer (java.io.StringWriter.)
        stderr-writer (java.io.StringWriter.)
        err-pw       (java.io.PrintWriter. stderr-writer true)
        thrown       (atom nil)
        tool-counts  (atom {})
        ;; Per-top-level-form sinks. `invoke-symbol-wrapper` writes ONE entry
        ;; to each per tool-symbol call; `*sink-position*` is shared so the
        ;; same call gets matching `:position` in both vectors. All three
        ;; rebind cleanly when the form returns - late-arriving thread
        ;; writes silently drop (the dynamic var binding has unwound).
        journal-sink (atom [])
        channel-sink (atom [])
        sink-pos     (atom -1)
        ;; Live tool-event callback only - no longer accumulated into a vec.
        ;; Storage role retired (was dead persistence). The TUI/progress UI
        ;; consumes via `tool-event-fn` synchronously during eval.
        record-tool-event (fn [event]
                            (let [op (:op event)
                                  n  (get (swap! tool-counts update op (fnil inc 0)) op)
                                  event* (cond-> event
                                           (not= n 1) (assoc :id (str (name (or op :tool)) "-" n)))]
                              (when tool-event-fn (tool-event-fn event*))))
        exec-future (cancellation/worker-future "vis-sci-eval"
                      (fn []
                        (try
                          (let [result (binding [extension/*tool-event-sink* record-tool-event
                                                 extension/*journal-render-sink* journal-sink
                                                 extension/*channel-render-sink* channel-sink
                                                 extension/*sink-position*       sink-pos]
                                         (sci/binding [sci/out stdout-writer
                                                       sci/err err-pw]
                                           (let [ns (or (sci/find-ns sci-ctx 'sandbox) sandbox-ns)]
                                             (:val (sci/eval-string+ sci-ctx code
                                                     (when ns {:ns ns}))))))]
                            {:result result :stdout (str stdout-writer) :stderr (str stderr-writer)
                             :journal     @journal-sink
                             :channel     @channel-sink
                             :error nil})
                          (catch Throwable e
                            (reset! thrown e)
                            ;; Per PLAN §2.1 + §2.6 + §7.3.5: :error is
                            ;; the STRUCTURED :error map
                            ;; ({:message :trace :hint? :block?}) — no
                            ;; legacy string fallback. SCI parses the
                            ;; whole `code` at once so its :line/:column
                            ;; in ex-data are already block-global; no
                            ;; form-row translation needed at this site.
                            {:result nil :stdout (str stdout-writer) :stderr (str stderr-writer)
                             :journal @journal-sink
                             :channel @channel-sink
                             :error   (try (extension/ex->op-error e {:block-source code})
                                        (catch Throwable _
                                          {:message (or (ex-message e)
                                                      (.getName (class e)))}))}))))
        timeout-ms (long *eval-timeout-ms*)
        execution-result (try
                           (deref exec-future timeout-ms nil)
                           (catch Throwable e
                             (reset! thrown e)
                             {:result nil :stdout "" :stderr ""
                              :journal @journal-sink
                              :channel @channel-sink
                              :error   (try (extension/ex->op-error e {:block-source code})
                                         (catch Throwable _
                                           {:message (or (ex-message e)
                                                       (.getName (class e)))}))}))]
    (.close stdout-writer)
    (.close stderr-writer)
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
        {:result nil :stdout "" :stderr ""
         :journal @journal-sink
         :channel @channel-sink
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
             :execution-time-ms execution-time)
      (:timeout? execution-result) (assoc :timeout? true)
      (not (:timeout? execution-result)) (assoc :timeout? false))))

(def ^:private parse-rescue-max-iterations
  "Hard cap on rescue retries. Bounds pathological hooks that keep
   rewriting the source without converging. The vast majority of
   real-world cases (single `\\|`, two `\\|` on different lines)
   need 1-3 iterations; 8 is generous."
  8)

(defn- try-extension-parse-rescue
  "Walk the environment's active extensions and ask each one's
   `:ext/on-parse-error-fn` to rewrite `code`. Loops the rescue chain
   so that hooks repairing only the FIRST offending site (the
   documented contract of `rescue-parse-error`) still converge when
   the source contains 2+ broken sites. Returns the rewritten string
   once it parses cleanly, nil otherwise.

   Termination conditions (in order):
     1. Rewrite parses cleanly             -> return rewrite.
     2. Hook returns nil                    -> return nil.
     3. Hook returns the input unchanged    -> return nil (no progress).
     4. Iteration cap reached               -> return nil (bounded).

   Read-only on the environment."
  [environment code parse-error]
  (when-let [exts (some-> (:extensions environment) deref seq)]
    (loop [current-code  code
           current-error parse-error
           iterations    0]
      (when (< iterations parse-rescue-max-iterations)
        (let [fixed (extension/try-rescue-parse-error exts current-code current-error environment)]
          (cond
            (nil? fixed)              nil
            (= fixed current-code)    nil  ; no progress - bail
            :else
            (let [next-error (parse-clojure-syntax fixed)]
              (if (nil? next-error)
                fixed                       ; parses cleanly - done
                (recur fixed next-error (inc iterations))))))))))

(def ^:private DEF_HEADS '#{def defn defn- defmacro})

(defn extract-defining-name
  "Return the symbol named by a `(def NAME ...)` / `(defn NAME ...)` /
   `(defn- NAME ...)` / `(defmacro NAME ...)` form, or nil otherwise.
   Tolerant: parse errors return nil rather than throwing."
  [expression]
  (try
    (let [forms (edamame/parse-string-all expression edamame-opts)
          form  (first forms)]
      (when (and (= 1 (count forms))
              (seq? form)
              (contains? DEF_HEADS (first form))
              (symbol? (second form)))
        (second form)))
    (catch Throwable _ nil)))

(defn- attach-doc-meta!
  "Attach `doc-string` as :doc metadata to the var named by `expression`'s
   defining form, if any. No-op when `expression` is not a (def...) /
   (defn...) shape, when doc-string is blank, or when the SCI eval throws.
   Failures are logged at :debug; the caller's eval already succeeded
   so a metadata-attach failure must NOT propagate."
  [{:keys [sci-ctx sandbox-ns]} expression doc-string]
  (when (and sci-ctx (string? doc-string) (not (str/blank? doc-string)))
    (when-let [defining-name (extract-defining-name expression)]
      (try
        (let [sandbox (or (sci/find-ns sci-ctx 'sandbox) sandbox-ns)
              attach-form (str "(when-let [v (resolve '" defining-name ")]"
                            "  (alter-meta! v assoc :doc " (pr-str doc-string) "))")]
          (sci/eval-string+ sci-ctx attach-form (when sandbox {:ns sandbox})))
        (catch Throwable t
          (tel/log! {:level :debug :id ::attach-doc-failed
                     :data {:name (str defining-name)
                            :error (ex-message t)}}))))))

(defn- try-extension-source-rewrite
  "Walk active extensions for parsed-source normalization before SCI eval.
   Extension-owned: the loop only supplies the hook seam and validates that
   rewritten source still parses."
  [environment code]
  (when-let [exts (some-> (:extensions environment) deref seq)]
    (when-let [rewritten (extension/try-rewrite-source exts code environment)]
      (when-not (parse-clojure-syntax rewritten)
        rewritten))))

(defn- execute-code
  "Run a single :code block through the SCI sandbox.

   Optional kwargs:
     :timeout-ms - hard-cap eval time, clamped at the
                   *eval-timeout-ms* bounds.
     :doc        - docstring to attach to the var defined by this :expr.

   Every call performs a real SCI eval. There is no result cache:
   forms with side effects (e.g. host primitives `(turn-answer! ...)` and
   `(set-conversation-title! ...)`) MUST run their bodies on every
   invocation, and forms without side effects re-run cheaply enough
   that caching them is not worth the correctness footgun."
  [{:keys [sci-ctx sandbox-ns] :as environment} code
   & {:keys [timeout-ms doc tool-event-fn]}]
  (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :execute-code})]
    (let [start-time (System/currentTimeMillis)
          lint-error (detect-common-mistakes code)]
      (if lint-error
        ;; Per PLAN §2.1 + §7.3.5: :error is the structured
        ;; :error map. Wrap engine-internal string errors
        ;; (lint, parse) into the same shape with :phase :preflight.
        {:result nil :stdout "" :stderr ""
         :error  {:message lint-error
                  :block   {:source code :phase :preflight}}
         :execution-time-ms 0 :timeout? false}
        (let [parse-error (parse-clojure-syntax code)]
          (if parse-error
            (if-let [rescued (try-extension-parse-rescue environment code parse-error)]
              (let [exec (run-with-timing sci-ctx rescued sandbox-ns timeout-ms start-time tool-event-fn environment)
                    eval-ok? (nil? (:error exec))]
                (when eval-ok?
                  (attach-doc-meta! environment rescued doc))
                ;; `:repaired? true` ONLY when the rescued source
                ;; actually evaluated cleanly. Setting it on a
                ;; still-failing eval misleads the journal/transcript
                ;; ("repaired" tag) and the model. Keep
                ;; `:original-error`/`:original-code` regardless so
                ;; the next iteration sees what we tried.
                (cond-> exec
                  true     (assoc :original-code code
                             :original-error parse-error)
                  eval-ok? (assoc :repaired? true)))
              {:result nil :stdout "" :stderr ""
               :error {:message parse-error
                       :block   {:source code :phase :edamame/parse}}
               :execution-time-ms 0 :timeout? false})
            (let [rewritten-code (try-extension-source-rewrite environment code)
                  eval-code      (or rewritten-code code)
                  initial-exec   (run-with-timing sci-ctx eval-code sandbox-ns timeout-ms start-time tool-event-fn environment)
                  initial-ok?    (nil? (:error initial-exec))
                  ;; Eval-time auto-repair scoped to `(turn-answer! ...)`
                  ;; forms. SCI surfaces `Unable to resolve symbol:
                  ;; X` when the model wrote a bare prose word
                  ;; inside a vector of strings (the iter-0/iter-1
                  ;; pattern in conv ec64266c-...). Only fires when
                  ;; the source carries `(turn-answer!`, X is prose-shaped,
                  ;; and X sits in a missing-quote-shaped context.
                  ;; Each candidate is re-evaluated; first that
                  ;; succeeds wins. A failed restitch falls through
                  ;; to the hint-enrichment path below.
                  ;; :error is now structured; pull :message for the
                  ;; prose-as-symbol detector.
                  initial-error-msg (some-> initial-exec :error :message)
                  restitched     (when (and (not initial-ok?) (string? initial-error-msg))
                                   (when-let [sym (some-> ^String initial-error-msg
                                                    (->> (re-find #"Unable to resolve symbol: (\S+)"))
                                                    second)]
                                     (when-let [candidates (parse-diagnose/try-answer-string-restitch
                                                             eval-code sym)]
                                       (some (fn [candidate]
                                               (let [retry (run-with-timing sci-ctx candidate sandbox-ns
                                                             timeout-ms start-time tool-event-fn environment)]
                                                 (when (nil? (:error retry))
                                                   {:retry retry :candidate candidate})))
                                         candidates))))
                  original-error (when restitched (:error initial-exec))
                  exec           (or (:retry restitched) initial-exec)
                  eval-ok?       (or (some? restitched) initial-ok?)
                  ;; Eval-time prose-as-symbol HINT for the surfaced
                  ;; error - only relevant when restitch did NOT
                  ;; recover. Pure advisory; never rewrites source.
                  exec-error-msg (some-> exec :error :message)
                  enriched-error (when (and (not eval-ok?) (string? exec-error-msg))
                                   (when-let [hint (parse-diagnose/unresolved-symbol-hint
                                                     exec-error-msg eval-code)]
                                     ;; Build a fresh structured error
                                     ;; that adds the hint per PLAN §2.1
                                     ;; ({:message :hint? :trace? :block?}).
                                     (-> (or (:error exec) {})
                                       (assoc :message exec-error-msg
                                         :hint hint))))]
              (when eval-ok?
                (attach-doc-meta! environment
                  (or (:candidate restitched) eval-code) doc))
              (cond-> exec
                enriched-error              (assoc :error enriched-error)
                ;; `:repaired?` ONLY when the source was actually
                ;; mutated AND eval was clean. A rewrite that still
                ;; throws is not a repair - it's an attempt that
                ;; fell through.
                (and rewritten-code eval-ok? (not restitched))
                (assoc :repaired? true
                  :original-code code
                  :repair :extension-source-rewrite)

                restitched
                (assoc :repaired? true
                  :original-code code
                  :original-error original-error
                  :repair :answer-string-restitch)))))))))

;; Print-cap defaults for `prompt/safe-pr-str` - chosen so a wide flat
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
   `conversation-title`; normal value-bearing forms remain visible."
  [_environment _code result]
  result)

;; ---------------------------------------------------------------------------
;; Answer-scoping helper (Option C)
;;
;; The iteration loop discards a `(turn-answer! ...)` call iff the form
;; that ITSELF invoked it errored. Sibling errors (a typo in some
;; OTHER form, a bad v/edit elsewhere) do NOT gate termination -
;; the model's request to finalize is honored as long as the answer-
;; bearing form ran cleanly. Pre-Option C the loop discarded on ANY
;; sibling error, which is how a turn could rack up 148 retries with
;; the model repeatedly emitting `(turn-answer! ...)` next to a single
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
   `split-top-level-forms` already parses once and stores `:form`; helpers
   use that instead of reparsing the same `:expr` over and over. String
   parsing remains only as a fallback for older/synthetic call sites."
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
    (= "turn-answer!" (name (first form)))
    (nil? (namespace (first form)))))

(defn- form-contains-turn-answer-call?
  [entry-or-form-or-source]
  (let [form (parsed-entry-form entry-or-form-or-source)]
    (boolean (some turn-answer-call-form? (tree-seq coll? seq form)))))

(defn- conversation-title-meta-form?
  [entry-or-form-or-source]
  (let [form (parsed-entry-form entry-or-form-or-source)]
    (and (seq? form)
      (symbol? (first form))
      (= 'set-conversation-title! (first form)))))

(defn- call-symbol->op-keyword
  "Convert a head symbol like `v/patch` or `spit` to the canonical
   op-keyword the engine knows about: `:v/patch` or `:spit`. Returns
   nil for non-symbols."
  [head]
  (when (symbol? head)
    (let [ns* (namespace head)
          nm  (name head)]
      (if ns*
        (keyword ns* nm)
        (keyword nm)))))

(defn- mutating-call-form?
  "True when `form` is a call whose head symbol classifies as an
   action op via the engine contract (`extension/op-tag` returns
   `:op.tag/action`). Source of truth lives in
   `src/com/blockether/vis/internal/extension.clj`; tools register
   or override their op-tag via `extension/register-op-tag!`.

   Reproduced from convo `73f3d325` turn 5: `(z/patch ...) ...
   (turn-answer! \"Now fixed\")` in one iteration; z/patch failed
   `matched 4 time(s)` but the answer claimed success because the
   model never saw the failure result before the answer was
   composed. Read-only tools (v/cat, v/rg, z/locators,
   z/patch-check, v/patch-check, ...) classify as
   `:op.tag/observation` and stay legal alongside the answer;
   action ops (`:op.tag/action`) are gated."
  [form]
  (and (seq? form)
    (when-let [op-kw (call-symbol->op-keyword (first form))]
      (= :op.tag/action (extension/op-tag op-kw)))))

(defn- form-contains-mutating-call?
  [entry-or-form-or-source]
  (let [form (parsed-entry-form entry-or-form-or-source)]
    (boolean (some mutating-call-form? (tree-seq coll? seq form)))))

(defn- answer-with-mutation-preflight-mismatch
  "When an iteration contains BOTH a top-level form that holds an
   `(turn-answer! ...)` call AND a top-level form that holds a mutating tool
   call (anywhere in its tree), return a map describing the violation
   so the engine can reject the iteration before any evaluation runs.
   Returns nil when the iteration is fine.

   The two forms may be the same form (e.g. `(do (z/patch ...) (turn-answer!
   ...))`) or different forms in the same iteration. Either way the
   model has no chance to observe the mutation result before the answer
   is composed."
  [code-entries]
  (let [answer-idx   (first (keep-indexed (fn [idx entry]
                                            (when (form-contains-turn-answer-call? entry) idx))
                              code-entries))
        mutating-idx (first (keep-indexed (fn [idx entry]
                                            (when (form-contains-mutating-call? entry) idx))
                              code-entries))]
    (when (and (some? answer-idx) (some? mutating-idx))
      {:answer-idx   answer-idx
       :mutating-idx mutating-idx
       :total-forms  (count code-entries)})))

(defn- block-result-error-summary
  "Return a short string describing the error in a per-form result map
   for the answer-after-error gate. Picks the block-level `:error`
   first; falls back to the first `:journal` sink-entry whose
   `:success?` is false (lifted tool-failure). Returns nil when the
   form ran cleanly.

   Per PLAN §2.1 + §7.3.5: `:error` is the structured `:error` map
   (`{:message :trace? :hint? :block?}`). This fn extracts `:message`
   for the answer-after-error gate's terse one-line display."
  [result]
  (or (some-> (:error result) :message)
    (some (fn [j]
            (when (false? (:success? j))
              (or (some-> (:error j) :message)
                (str "tool failure in " (pr-str (:form j))))))
      (:journal result))))

(defn- answer-with-mutation-preflight-error-message
  [{:keys [answer-idx mutating-idx total-forms]}]
  (str "Answer/mutation preflight rejected this iteration before evaluation: "
    "(turn-answer! ...) at top-level form " (inc (or answer-idx 0))
    " and a mutating tool call (e.g. v/patch, z/patch, v/copy, v/move, "
    "v/delete, v/delete-if-exists, v/create-dirs, z/repair-*) at top-level form "
    (inc (or mutating-idx 0))
    " appeared together in the same " total-forms "-form iteration. "
    "This is structurally unobservable: the SCI loop is write-then-read, "
    "so you cannot see the mutation's success/failure tool result before "
    "the answer is composed. Recovery: keep the mutating call in THIS "
    "iteration, drop the (turn-answer! ...). The host will loop; the next "
    "iteration's <journal> will carry the mutation's :success?/:error "
    "sink entry. Inspect it (and the file bytes via v/cat / z/locators) "
    "before claiming success. Use (z/patch-check edits) or (v/patch-check "
    "edits) to dry-run when locators may be stale."))

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

(defn- normalized-code-source
  [code-entries]
  (->> code-entries
    (map (comp str/trim str :expr))
    (remove str/blank?)
    (str/join "\n")))

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

(defn- bare-symbol-entry?
  [entry-or-form-or-source]
  (symbol? (parsed-entry-form entry-or-form-or-source)))

(defn- plain-prose-code-error
  [raw-code code-entries]
  (when (and (not (str/blank? (or raw-code "")))
          (re-find #"\s" (str raw-code))
          (<= 3 (count code-entries))
          (every? bare-symbol-entry? code-entries))
    (str "Provider returned plain prose where executable Clojure was required. "
      "Wrap final prose in `(turn-answer! ...)` or emit a fenced `clojure` code block; "
      "aborting this iteration instead of evaluating each word as a symbol.")))

(defn- prose->comment
  [raw-code]
  (->> (str/split-lines (str raw-code))
    (map #(str ";; " %))
    (str/join "\n")))

(defn- executable-block-source
  [block]
  (when (map? block)
    (some-> (or (:source block)
              (:code block)
              (:text block)
              (:content block))
      str
      str/trim)))

(defn- executable-block-sources
  [blocks]
  (->> (or blocks [])
    (keep executable-block-source)
    (remove str/blank?)
    vec))

(defn- duplicate-fenced-blocks?
  [blocks]
  (let [sources (executable-block-sources blocks)]
    (< (count (distinct sources)) (count sources))))

(defn- dedupe-fenced-block-code
  "Providers sometimes stutter and return the same fenced code block multiple
   times (` ```clojure ... ``````clojure ... ````). Treat exact duplicate
   executable blocks as transport noise: execute the first copy once instead
   of burning a retry iteration."
  [raw-code blocks]
  (let [sources (executable-block-sources blocks)]
    (if (< (count (distinct sources)) (count sources))
      (str/join "\n\n" (distinct sources))
      raw-code)))

;; ---------------------------------------------------------------------------
;; Vis-engine XML echo stripper (hallucinated <journal>/<bindings>/...).
;;
;; The model occasionally fabricates Vis-only XML envelopes in its OWN reply -
;; most often <journal> sections - and closes them with a stray ``` instead of
;; </journal>. That stray ``` then opens an untagged fenced block in the
;; raw response, swallowing the real ```clojure opener that follows. The
;; resulting blocks come out tagged :lang nil with literal ```clojure markers
;; embedded in their source, which the fence-leak preflight then rejects -
;; burning an entire iteration on a parser-recoverable error.
;;
;; Repair pass: at the line level, drop every <journal>/<bindings>/<active_skills>
;; /<current_turn_context>/<current_engine_start_nudge[s]> opener through its closer. Closer matches `</tag>` (proper),
;; a bare ``` line (LLM fumble), or another opener (implicit close), or EOF.
;; A ```lang line implicitly closes the envelope without itself being dropped
;; so a real fence directly after a fabricated envelope still parses cleanly.
;; ---------------------------------------------------------------------------

(def ^:private vis-engine-xml-echo-tags
  ;; Vis -> LLM ONLY. The model must never emit these. See
  ;; `com.blockether.vis.internal.prompt` for the renderers that own them.
  #{"journal" "bindings" "active_skills" "current_turn_context"
    "current_engine_start_nudges" "current_engine_start_nudge"
    ;; Backward-compatible echo cleanup for older transcripts / model habits.
    "system_vars" "system_var" "system_nudges" "system_nudge"})

(defn- vis-engine-xml-open-tag
  "Return the matched tag name (string) when `line` is a Vis-engine XML
   opener at the start of the trimmed line, else nil. Accepts both
   `<tag>` and `<tag attr=\"x\">` shapes."
  [line]
  (when (string? line)
    (let [t (str/triml line)]
      (some (fn [tag]
              (when (or (str/starts-with? t (str "<" tag ">"))
                      (str/starts-with? t (str "<" tag " ")))
                tag))
        vis-engine-xml-echo-tags))))

(defn- vis-engine-xml-close-line?
  [tag line]
  (when (and tag (string? line))
    (= (str "</" tag ">") (str/trim line))))

(defn- bare-fence-line?
  "True for a markdown fence line with NO language tag (just backticks).
   These are the LLM's most common closer fumble inside a fabricated
   <journal>: it ended the section with ``` instead of </journal>."
  [line]
  (when (string? line)
    (boolean (re-matches #"^[ \t]*`{3,}[ \t]*$" line))))

(defn- tagged-fence-opener-line?
  "True for a markdown fence opener WITH a language tag (e.g. ```clojure).
   When we hit one of these inside a fabricated envelope we treat the
   envelope as implicitly closed and KEEP the line so the downstream
   fence-block extractor still sees a valid opener."
  [line]
  (when (string? line)
    (boolean (re-matches #"^[ \t]*`{3,}[A-Za-z0-9_+\-]+[ \t]*$" line))))

(defn- contains-vis-engine-xml-echo?
  [^String raw]
  (and (string? raw)
    (boolean (some #(or (str/includes? raw (str "<" % ">"))
                      (str/includes? raw (str "<" % " ")))
               vis-engine-xml-echo-tags))))

(defn- strip-vis-engine-xml-echo
  "Remove hallucinated <journal>...</journal> (and similar Vis-only XML
   envelopes) from raw LLM text BEFORE fenced-block extraction. The model
   must never emit these tags - they are read-only surfaces the engine
   writes. When a model echoes them, the closer is often a stray ``` instead
   of </tag>; that ``` then poisons fence-block parsing.

   Returns `raw` unchanged when no opener appears."
  [raw]
  (if-not (contains-vis-engine-xml-echo? raw)
    raw
    (let [lines (str/split-lines raw)]
      (loop [remaining (seq lines)
             current-tag nil
             out (transient [])]
        (if-not remaining
          (str/join "\n" (persistent! out))
          (let [line (first remaining)
                rst  (next remaining)]
            (cond
              ;; Outside any envelope.
              (nil? current-tag)
              (if-let [opener (vis-engine-xml-open-tag line)]
                (recur rst opener out)
                (recur rst nil (conj! out line)))

              ;; Inside an envelope - matching </tag> closer (proper form).
              (vis-engine-xml-close-line? current-tag line)
              (recur rst nil out)

              ;; Inside an envelope - LLM fumbled the closer with bare ```.
              (bare-fence-line? line)
              (recur rst nil out)

              ;; Inside an envelope - real fence opener (```lang). Treat as
              ;; implicit close and KEEP the line so the extractor sees it.
              (tagged-fence-opener-line? line)
              (recur rst nil (conj! out line))

              ;; Inside an envelope - new envelope opener swaps current tag.
              :else
              (if-let [opener (vis-engine-xml-open-tag line)]
                (recur rst opener out)
                (recur rst current-tag out)))))))))

(defn- normalize-ask-result-vis-engine-xml-echo
  "When the LLM raw response contains hallucinated Vis-engine XML envelopes
   (`<journal>`, `<bindings>`, ...), strip them and re-extract fenced clojure
   blocks. Replaces `:result` and `:blocks` on the ask-result; leaves `:raw`
   ORIGINAL so forensic logs / DB rows show what the model actually emitted.

   Pass-through when no envelope appears or stripping yields nothing usable."
  [ask-result]
  (let [raw (or (:raw ask-result) "")]
    (if-not (contains-vis-engine-xml-echo? raw)
      ask-result
      (let [cleaned (strip-vis-engine-xml-echo raw)]
        (if (or (str/blank? cleaned) (= cleaned raw))
          ask-result
          (let [blocks   (svar-codes/extract-code-blocks cleaned)
                selected (svar-codes/select-blocks blocks "clojure")
                result   (svar-codes/concat-sources selected)]
            (if (str/blank? result)
              ask-result
              (assoc ask-result
                :result result
                :blocks selected
                :vis/normalized-from-raw? true))))))))

(defn- code-entries-preflight
  ([iteration-position raw-code]
   (code-entries-preflight iteration-position raw-code nil))
  ([_iteration-position raw-code opts]
   (let [raw-code (or raw-code "")
         duplicate-blocks-normalized? (duplicate-fenced-blocks? (:blocks opts))
         raw-code (dedupe-fenced-block-code raw-code (:blocks opts))
         raw-fence-error (raw-markdown-fence-leak-error raw-code)
         [forms parse-error] (if raw-fence-error
                               [nil (ex-info raw-fence-error
                                      {:vis/error :raw-markdown-fence-leak})]
                               (split-top-level-forms raw-code))
         parsed-code-entries (cond
                               raw-fence-error
                               [{:expr "(vis/preflight-error :raw-markdown-fence-leak)"
                                 :vis/preflight-error raw-fence-error}]

                               parse-error
                               [{:expr (str raw-code) :parse-error parse-error}]

                               :else
                               (vec (filter #(not (str/blank? (:expr %)))
                                      (or forms []))))
         plain-prose-error (when-not (or raw-fence-error parse-error)
                             (plain-prose-code-error raw-code parsed-code-entries))
         parsed-total-blocks (count parsed-code-entries)
         normalized-code (normalized-code-source parsed-code-entries)
         code-hash (when-not (str/blank? normalized-code)
                     (sha256-hex normalized-code))
         answer-with-mutation-mismatch (when (and (not raw-fence-error)
                                               (not plain-prose-error)
                                               (pos? parsed-total-blocks))
                                         (answer-with-mutation-preflight-mismatch
                                           parsed-code-entries))
         answer-with-mutation-error (when answer-with-mutation-mismatch
                                      (answer-with-mutation-preflight-error-message
                                        answer-with-mutation-mismatch))]
     {:code-entries (cond
                      plain-prose-error
                      [{:expr "(vis/preflight-error :plain-prose-code)"
                        :comment (prose->comment raw-code)
                        :vis/preflight-error plain-prose-error}]

                      answer-with-mutation-error
                      [{:expr "(vis/preflight-error :answer-with-mutation)"
                        :vis/preflight-error answer-with-mutation-error}]

                      :else
                      parsed-code-entries)
      :answer-with-mutation-preflight-error answer-with-mutation-error
      :plain-prose-preflight-error plain-prose-error
      :raw-fence-preflight-error raw-fence-error
      :duplicate-blocks-normalized? duplicate-blocks-normalized?
      :normalized-code normalized-code
      :code-hash code-hash
      :original-total-blocks parsed-total-blocks})))

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
             :data {:ext (:ext/namespace ext)
                    :hook id
                    :phase :turn.answer/validate
                    :error (ex-message t)}})
  nil)

(defn- answer-validation-invalid-return-message
  [ext id hit]
  (tel/log! {:level :warn
             :id ::answer-validation-hook-invalid-return
             :data {:ext (:ext/namespace ext)
                    :hook id
                    :phase :turn.answer/validate
                    :returned hit
                    :explain (s/explain-data ::extension/answer-validation-reject hit)}})
  nil)

(defn- answer-validation-extensions
  [environment active-extensions]
  (or (seq active-extensions)
    (some-> (:extensions environment) deref seq)))

(defn final-answer-gate-error
  "Run hard final-answer validation hooks. Returns nil when the candidate
   answer is accepted, otherwise a string error surfaced as the rejected
   answer form's validation error. `active-extensions` is passed by the
   turn loop so activation is computed once per turn; direct callers may
   omit it and provide `:extensions` on the environment."
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
       (answer-validation-extensions environment active-extensions)))))

(defn- runtime-turn-prefix
  [environment]
  (let [id-s (str (or (some-> (:current-conversation-turn-id-atom environment) deref)
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

(defn- eval-info
  "Generic canonical info for every top-level form that passes
   through the Vis eval pipeline. Tool calls can add nested info
   inside their returned envelope; this records the outer regular form
   evaluation so plain calls and tool calls share a common block-level
   trace."
  [turn-prefix iteration form-idx form-count result rendering-kind]
  (let [finished      (long (or (:execution-finished-at-ms result)
                              (System/currentTimeMillis)))
        duration      (long (or (:execution-time-ms result) 0))
        started       (long (or (:execution-started-at-ms result)
                              (max 0 (- finished duration))))
        form-position (inc (long form-idx))]
    (extension/normalize-metadata
      {:op             (or (:op result)
                         (case rendering-kind
                           :nudge  :vis/system
                           :answer :vis/answer
                           :sci/eval))
       :started-at-ms  started
       :finished-at-ms finished
       :duration-ms    duration
       :status         (cond
                         (:timeout? result) :timeout
                         (:error result) :error
                         :else :done)
       :iteration      iteration
       :form-position  form-position
       :form-count     form-count
       :ref            (str "turn/" turn-prefix "/iteration/" iteration "/block/" form-position)
       :timeout?       (boolean (:timeout? result))
       :repaired?      (boolean (:repaired? result))})))

(s/def ::id nat-int?)
(s/def ::code string?)
(s/def ::stdout string?)
(s/def ::stderr string?)
(s/def ::error (s/nilable map?))                       ; structured :error map
(s/def ::execution-time-ms nat-int?)
(s/def ::timeout? (s/nilable boolean?))
(s/def ::repaired? (s/nilable boolean?))
(s/def ::comment string?)
(s/def ::block-info
  (s/and
    map?
    #(contains? #{:sci/eval :edamame/parse :vis/guard :vis/system :vis/answer} (:op %))
    #(contains? #{:done :error :timeout} (:status %))
    #(pos-int? (:iteration %))
    #(pos-int? (:form-position %))
    #(pos-int? (:form-count %))
    #(re-matches #"(?i)^turn/[0-9a-f]{8}/iteration/[1-9][0-9]*/block/[1-9][0-9]*$" (:ref %))))
(s/def ::info ::block-info)
(s/def ::iteration-block
  (s/keys :req-un [::id ::code ::stdout ::stderr ::error
                   ::execution-time-ms ::info]
    :opt-un [::result ::timeout? ::repaired? ::comment]))

(defn validate-iteration-blocks!
  "Fail fast if a stored/evaluated block lost mandatory info.
   Tool-result envelopes enforce their nested info separately;
   this spec enforces the outer block-level eval info for every
   regular top-level form."
  [blocks]
  (let [blocks (mapv (fn [block]
                       (cond-> block
                         (contains? block :error)
                         (update :error op-error
                           {:code (:code block)
                            :phase (get-in block [:info :op])})))
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
   :prompt_tokens_details {:cached_tokens (long (or (token-number tokens [:cached :cached-input :input-cached]) 0))}})

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

(def ^:private preserved-thinking-replay-token-budget
  "Approximate max tokens of preserved `reasoning_content` carried
   across iter assistant replays. Resolves to
   `(long (* PRESERVED_THINKING_REPLAY_FRACTION MAX_ITERATION_CONTEXT_TOKENS))`
   = 30000 today (0.15 × 200000), and auto-tracks the cap if it changes.

   Z.ai's preserved-thinking contract requires CONSECUTIVE
   `reasoning_content` blocks (newest run, contiguous, verbatim).
   We always keep the most recent prior iter — even if its reasoning
   alone exceeds the budget — and walk newest→oldest until adding
   the next iter's reasoning would push total over the budget; we
   stop there (do NOT skip-and-continue, that would create a gap
   and break the contiguity rule).

   Char-count proxy: `chars / 4 ≈ tokens`. Conservative upper bound;
   real GLM tokens are slightly denser, so the actual carried budget
   sits somewhat under this number."
  (long (* prompt/PRESERVED_THINKING_REPLAY_FRACTION
          prompt/MAX_ITERATION_CONTEXT_TOKENS)))

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
  "Provider-agnostic preserved-thinking replay. Returns the canonical
   `:assistant-message` of the most-recent prior iterations whose
   cumulative reasoning_content fits inside
   `preserved-thinking-replay-token-budget`, in chronological order
   (oldest kept → newest); empty vec when none.

   Z.ai requires the kept block run to be CONTIGUOUS from the latest
   iter back; if iter K-2's reasoning would push total over budget,
   we stop and DO NOT skip past it to grab K-3 (that would create a
   gap → contract violation, model loops). The most recent iter is
   always kept, even when it alone exceeds budget, because dropping
   it breaks single-step continuity which all preserving providers
   need.

   The wire serializer for the active model translates each canonical
   message to its native shape; iteration-loop never branches on
   provider. Anthropic / OpenAI Responses don't carry `reasoning_content`
   at all in their canonical shape, so the budget walk just trims by
   thinking-block size and the per-provider serializer ignores anything
   it can't carry."
  [journal-iters]
  ;; Walks newest→oldest via `reverse`, sizes each non-nil assistant-message,
  ;; then folds with `reduced` to short-circuit at the first over-budget
  ;; older iter. The `kept` list is built front-prepended in newest-first
  ;; order during the fold; the newest message lands at the FRONT of the
  ;; list, the oldest kept message at the BACK — so when we `cons` in
  ;; reverse-time order during the fold, the final `:kept` already reads
  ;; oldest→newest (chronological), matching the contract.
  (let [budget-chr (* 4 preserved-thinking-replay-token-budget)
        sized      (->> journal-iters
                     reverse
                     (keep #(some-> % second :assistant-message))
                     (map (juxt identity #(long (replay-reasoning-chars %)))))
        {:keys [kept]}
        (reduce (fn [{:keys [used kept] :as acc} [msg size]]
                  (let [used' (+ (long used) (long size))]
                    (cond
                      ;; First (newest) kept iter: always keep, even when
                      ;; it alone exceeds the budget. Continuity > budget
                      ;; on the latest turn; preserved-thinking breaks
                      ;; otherwise.
                      (empty? kept)            {:used used' :kept (cons msg kept)}
                      (<= used' budget-chr)    {:used used' :kept (cons msg kept)}
                      ;; Adding this older iter would push us over budget.
                      ;; STOP — do not skip-and-continue (that creates a
                      ;; gap in the consecutive run that z.ai's contract
                      ;; forbids).
                      :else                    (reduced acc))))
          {:used 0 :kept ()}
          sized)]
    (vec kept)))

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

(defn- llm-routing-metadata
  [selected-model iteration-result]
  (let [selected (llm-id (:provider selected-model) (some-> (:name selected-model) str))
        actual   (llm-id (or (:llm-provider iteration-result) (:provider selected-model))
                   (or (:llm-model iteration-result) (some-> (:name selected-model) str)))
        fallback-trace (vec (or (:llm-fallback-trace iteration-result) []))]
    (cond-> {:selected selected
             :actual   actual
             :fallback? (or (not= selected actual) (seq fallback-trace))}
      (seq fallback-trace) (assoc :fallback-trace fallback-trace))))

(defn- attach-llm-routing-summary
  [result selected-model iteration-result]
  (let [routing  (llm-routing-metadata selected-model iteration-result)
        actual   (:actual routing)
        selected (:selected routing)]
    (cond-> (assoc result
              :provider (:provider actual)
              :model    (:model actual)
              :llm-selected selected
              :llm-actual actual
              :llm-fallback? (:fallback? routing))
      (seq (:fallback-trace routing))
      (assoc :llm-fallback-trace (:fallback-trace routing))
      (:cost result)
      (update :cost merge (select-keys actual [:provider :model])))))

(defn- compatible-preserved-thinking-journal-iters
  [journal-iters target]
  (let [{target-provider :provider target-model :model} target]
    (filterv (fn [[_ {:keys [assistant-message llm-provider llm-model]}]]
               (and assistant-message
                 (= target-provider llm-provider)
                 (= target-model llm-model)
                 (assistant-message-compatible-with-replay-target?
                   target assistant-message)))
      (or journal-iters []))))

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
  ([messages journal-iters]
   (append-preserved-thinking-replay messages journal-iters nil))
  ([messages journal-iters target]
   (let [messages* (vec (or messages []))
         compatible-iters (if target
                            (compatible-preserved-thinking-journal-iters journal-iters target)
                            (or journal-iters []))
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
          ;; The SCI sandbox's `(turn-answer! "...")` fn `reset!`s it during
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
          streaming-fn (when on-chunk
                         (fn [{:keys [reasoning done? reset? reason failed-provider new-provider] :as chunk}]
                           (cond
                             reset?
                             (on-chunk {:phase           :provider-fallback
                                        :iteration       iteration-position
                                        :reason          reason
                                        :failed-provider failed-provider
                                        :new-provider    new-provider
                                        :fallback        (select-keys chunk [:reason :failed-provider :new-provider])})

                             (or (some? reasoning) done?)
                             (on-chunk {:phase     :reasoning
                                        :iteration iteration-position
                                        :thinking  (some-> reasoning str)
                                        :done?     (boolean done?)}))))
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
                                                            :conversation-turn-id (:environment-id environment)
                                                            :iteration iteration-position)]
                           (svar/ask-code! (:router environment)
                             (cond-> {:lang     "clojure"
                                      :messages messages
                                      :routing  (or routing {})
                                      :check-context? true
                                      :preserved-thinking? true}
                               effective-reasoning  (assoc :reasoning effective-reasoning)
                               streaming-fn         (assoc :on-chunk streaming-fn)
                               effective-llm-headers (assoc :llm-headers effective-llm-headers)
                               extra-body           (assoc :extra-body extra-body))))
          provider-duration-ms (elapsed-ms provider-start-ns)
          _ (log-stage! :provider-call/stop iteration
              {:duration-ms provider-duration-ms
               :raw-length (count (or (:raw ask-result-raw) ""))
               :block-count (count (or (:blocks ask-result-raw) []))
               :tokens (:tokens ask-result-raw)
               :fallback? (boolean (seq (:routed/fallback-trace ask-result-raw)))})
          parse-started-at-ms (System/currentTimeMillis)
          _ (when on-chunk
              (on-chunk {:phase :response-parse
                         :status :start
                         :iteration iteration-position
                         :started-at-ms parse-started-at-ms
                         :provider-duration-ms provider-duration-ms
                         :raw-length (count (or (:raw ask-result-raw) ""))
                         :block-count (count (or (:blocks ask-result-raw) []))}))
          ;; Repair pass: strip hallucinated <journal>/<bindings>/...
          ;; envelopes from raw LLM text before downstream fence-block
          ;; consumers see them. See `strip-vis-engine-xml-echo`.
          normalize-start-ns (System/nanoTime)
          ask-result (normalize-ask-result-vis-engine-xml-echo ask-result-raw)
          normalize-duration-ms (elapsed-ms normalize-start-ns)
          model-reasoning (:reasoning ask-result)
          thinking model-reasoning
          _ (log-stage! :llm-response iteration
              {:has-reasoning (some? model-reasoning)
               :raw-length    (count (or (:raw ask-result) ""))
               :block-count   (count (or (:blocks ask-result) []))
               :duration-ms   (:duration-ms ask-result)
               :provider-duration-ms provider-duration-ms
               :normalize-duration-ms normalize-duration-ms
               :tokens        (:tokens ask-result)
               :thinking      thinking
               :vis-engine-xml-echo-stripped? (boolean (:vis/normalized-from-raw? ask-result))})
          api-usage (ask-result->api-usage ask-result)
          ;; svar/ask-code! returns the concatenated source string in :result.
          ;; Parse it into top-level forms; each form becomes one
          ;; expression_state row.
          raw-code (or (:result ask-result) "")
          preflight-start-ns (System/nanoTime)
          preflight-result (code-entries-preflight iteration-position raw-code
                             {:blocks (:blocks ask-result)})
          preflight-duration-ms (elapsed-ms preflight-start-ns)
          {:keys [code-entries]} preflight-result
          _ (log-stage! :response-preflight/stop iteration
              {:duration-ms preflight-duration-ms
               :code-length (count raw-code)
               :forms (count code-entries)
               :parse-error? (boolean (:parse-error (first code-entries)))
               :raw-fence-preflight? (boolean (:raw-fence-preflight-error preflight-result))
               :plain-prose-preflight? (boolean (:plain-prose-preflight-error preflight-result))})
          _ (when on-chunk
              (on-chunk {:phase :response-parse
                         :status :done
                         :iteration iteration-position
                         :duration-ms preflight-duration-ms
                         :code-length (count raw-code)
                         :forms (count code-entries)}))
          engine-timing {:provider-call-ms provider-duration-ms
                         :response-normalize-ms normalize-duration-ms
                         :response-preflight-ms preflight-duration-ms}
          direct-answer-entry (when (= 1 (count code-entries))
                                (first code-entries))
          final-answer-preflight-error
          (when (and direct-answer-entry
                  (not (:vis/preflight-error direct-answer-entry))
                  (direct-answer-entry? direct-answer-entry)
                  (not (form-contains-needs-input-call? direct-answer-entry)))
            (final-answer-gate-error environment iteration-position [] nil))
          code-entries (if final-answer-preflight-error
                         [{:expr "(vis/preflight-error :final-answer-gate)"
                           :vis/preflight-error final-answer-preflight-error}]
                         code-entries)
          suppress-form-start? (or (some :vis/preflight-error code-entries)
                                 final-answer-preflight-error)
          total-blocks (count code-entries)
          ;; Engine-level answer-after-error gate (ANALYSIS.md §4.2):
          ;; an atom that flips true the moment a non-answer form errors.
          ;; Any subsequent form containing an `(turn-answer! ...)` call is
          ;; rejected before SCI re-evals it, so a model that emits
          ;; `(z/patch ...)`-fails-then-(turn-answer! "shipped") gets a
          ;; structured preflight error in place of the answer instead
          ;; of a fabricated success-claim.
          prior-error-atom (atom nil)
          executed (mapv (fn [idx {:keys [expr parse-error] :vis/keys [preflight-error] form-repaired? :repaired? form-comment :comment :as entry}]
                           (log-stage! :code-exec iteration
                             {:idx (inc idx) :total total-blocks :code expr})
                           (when (and on-chunk
                                   (not suppress-form-start?)
                                   (not (conversation-title-meta-form? entry))
                                   (not (form-contains-turn-answer-call? entry)))
                             (on-chunk {:phase         :form-start
                                        :iteration     iteration-position
                                        :form-idx      idx
                                        :form-of       total-blocks
                                        :iteration-id  (form-ref idx)
                                        :code          expr
                                        :comment       form-comment
                                        :started-at-ms (System/currentTimeMillis)}))
                           ;; Stamp form-idx BEFORE eval so any
                           ;; `(turn-answer! ...)` call inside this form
                           ;; captures the right index on the
                           ;; answer-atom payload.
                           (reset! current-form-idx-atom idx)
                           (let [iteration-id (form-ref idx)
                                 raw-result (cond
                                              preflight-error
                                              {:result nil
                                               :error (op-error preflight-error
                                                        {:code expr :phase :vis/preflight})
                                               :stdout "" :stderr "" :execution-time-ms 0
                                               :op :vis/guard}
                                              parse-error
                                              {:result nil
                                               :error (op-error (str "Parse error: " parse-error)
                                                        {:code expr :phase :edamame/parse})
                                               :stdout "" :stderr "" :execution-time-ms 0
                                               :op :edamame/parse}
                                              ;; Answer-after-error gate: any prior form
                                              ;; in this iteration errored, and this form
                                              ;; carries an `(turn-answer! ...)` call. Reject
                                              ;; before eval so the answer can't claim
                                              ;; success on top of an unobserved failure.
                                              (and @prior-error-atom
                                                (form-contains-turn-answer-call? entry))
                                              {:result nil
                                               :error  (op-error
                                                         (str "Answer-after-error gate: form "
                                                           (inc idx) " of " total-blocks
                                                           " contains an `(turn-answer! ...)` call, but "
                                                           "a prior form in this iteration errored: "
                                                           (truncate (str @prior-error-atom) 280)
                                                           ". Recovery: drop the (turn-answer! ...) so the host loops; "
                                                           "the next iteration's <journal> carries the error "
                                                           "and you can observe + repair before answering.")
                                                         {:code expr :phase :vis/guard})
                                               :stdout "" :stderr "" :execution-time-ms 0
                                               :op :vis/guard}

                                              :else
                                              (if-let [err (literal-code-block-error expr)]
                                                {:result nil
                                                 :error (op-error err {:code expr :phase :vis/guard})
                                                 :stdout "" :stderr "" :execution-time-ms 0
                                                 :op :vis/guard}
                                                (let [tool-event-fn (when (and on-chunk
                                                                            (not suppress-form-start?)
                                                                            (not (conversation-title-meta-form? entry))
                                                                            (not (form-contains-turn-answer-call? entry)))
                                                                      (fn [tool-event]
                                                                        (on-chunk {:phase :tool-start
                                                                                   :iteration iteration-position
                                                                                   :form-idx idx
                                                                                   :form-of total-blocks
                                                                                   :iteration-id iteration-id
                                                                                   :code expr
                                                                                   :comment form-comment
                                                                                   :tool-event tool-event})))
                                                      r (if tool-event-fn
                                                          (execute-code environment expr :tool-event-fn tool-event-fn)
                                                          (execute-code environment expr))]
                                                  (log-stage! :code-result iteration
                                                    {:idx (inc idx) :total total-blocks
                                                     :execution-time-ms (:execution-time-ms r)
                                                     :error (:error r) :timeout? (:timeout? r) :result (:result r)})
                                                  r)))
                                 ;; Carry parinfer's whole-source
                                 ;; rebalance flag into the per-form
                                 ;; result. `execute-code` may also
                                 ;; set `:repaired?` (extension hook
                                 ;; rescue); both paths converge on
                                 ;; the same flag for the channel.
                                 result (cond-> raw-result
                                          form-repaired? (assoc :repaired? true))
                                 _ (when-let [err (and (nil? @prior-error-atom)
                                                    (block-result-error-summary result))]
                                     ;; Capture the first error so subsequent
                                     ;; answer-bearing forms get rejected. Lifted
                                     ;; sink failures (`:success? false` in :journal)
                                     ;; count too — same root cause as a thrown error.
                                     (reset! prior-error-atom err))
                                 display-result (def-display-result environment expr result)
                                 ;; def-display-result is now a pass-through; kept on the
                                 ;; call path so future display-tweaks have a single seam.

                                 block-role (eval-block-role display-result)
                                 info (eval-info turn-prefix iteration-position idx total-blocks display-result block-role)
                                 result* (assoc display-result
                                           :info info
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
                                          :iteration         iteration-position
                                          :form-idx          idx
                                          :form-of           total-blocks
                                          :iteration-id      iteration-id
                                          :code              expr
                                          :comment           form-comment
                                          :result            (:result result*)
                                          :journal           (:journal result*)
                                          :channel           (:channel result*)
                                          :error             (:error result*)
                                          :stdout            (:stdout result*)
                                          :stderr            (:stderr result*)
                                          :execution-time-ms (:execution-time-ms result*)
                                          :info        (:info result*)
                                          :role        (:role result*)
                                          :silent?     (boolean (or (:vis/silent result*)
                                                                  (= :vis/silent (:result result*))
                                                                  (conversation-title-meta-form? entry)
                                                                  (form-contains-turn-answer-call? entry)))
                                          :timeout?          (boolean (:timeout? result*))
                                          :repaired?         (boolean (:repaired? result*))}))
                             {:block expr :result result* :form-comment form-comment}))
                     (range) code-entries)
          code-blocks (mapv :block executed)
          block-results (mapv :result executed)
          block-comments (mapv :form-comment executed)
          ;; Preflight gate → synthetic block carries `:vis/preflight? true`
          ;; so channels can suppress the model-facing-only error box. Keep
          ;; the block in the persisted/journal stream so the model still
          ;; reads the failure on its next iteration.
          preflight-by-idx (zipmap (range) (map (fn [{:vis/keys [preflight-error]}]
                                                  (boolean preflight-error))
                                             code-entries))
          blocks (validate-iteration-blocks!
                   (mapv (fn [idx code result form-comment]
                           (cond-> {:id idx
                                    :code code
                                    :result (:result result)
                                    :journal (:journal result)
                                    :channel (:channel result)
                                    :stdout (:stdout result)
                                    :stderr (:stderr result)
                                    :error (op-error (:error result) {:code code :phase (:op result)})
                                    :execution-time-ms (:execution-time-ms result)
                                    :info (:info result)
                                    :role (:role result)
                                    :timeout? (:timeout? result)
                                    :repaired? (:repaired? result)}
                             form-comment (assoc :comment form-comment)
                             (:vis/silent result) (assoc :vis/silent true)
                             (get preflight-by-idx idx) (assoc :vis/preflight? true)))
                     (range) code-blocks block-results block-comments))
          silent-form-idxs (into #{}
                             (keep-indexed (fn [idx block]
                                             (when (or (:vis/silent block)
                                                     (= :vis/silent (:result block)))
                                               idx)))
                             blocks)]
      (if-let [{:keys [value form-idx]} @answer-atom]
          ;; FINAL path: model called `(turn-answer! "...")` during this
          ;; iteration. Atom payload is `{:value :form-idx}`. The
          ;; form-scoped error gate fires if the answer-bearing form's
          ;; own evaluation errored anyway
          ;;      (e.g. `(do (v/edit ...throws...) (turn-answer! "x"))` -
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
                                (final-answer-gate-error environment iteration-position blocks value active-extensions answer-validation-context))
              validation-error (cond
                                 own-form-error
                                 (error/final-answer-code-error-message own-form-error)
                                 gate-error
                                 gate-error)
              ;; Surface the validation error on the answer-bearing
              ;; form's row so the model sees \"my (turn-answer! ...) was
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
                               :iteration         iteration-position
                               :form-idx          form-idx
                               :form-of           total-forms
                               :iteration-id      (form-ref form-idx)
                               :code              (:code b)
                               :comment           (get block-comments form-idx)
                               :result            (:result b)
                               :error             (:error b)
                               :stdout            (:stdout b)
                               :stderr            (:stderr b)
                               :execution-time-ms (:execution-time-ms b)
                               :info        (:info b)
                               :role        (:role b)
                               :silent?     (boolean (or (:vis/silent b)
                                                       (= :vis/silent (:result b))
                                                       (conversation-title-meta-form? (:code b))
                                                       (form-contains-turn-answer-call? (:code b))))
                               :timeout?          (boolean (:timeout? b))
                               :repaired?         (boolean (:repaired? b))})))
              model-name       (actual-llm-model resolved-model ask-result)
              provider         (actual-llm-provider resolved-model ask-result)]
          (if validation-error
            {:thinking thinking
             :blocks (or (seq blocks*)
                       [{:id 0 :code "(final-answer-validation)"
                         :result nil :stdout "" :stderr ""
                         :error (op-error validation-error
                                  {:code "(final-answer-validation)"
                                   :phase :vis/final-answer-validation})}])
             :final-result nil :api-usage api-usage
             :duration-ms (or (:duration-ms ask-result) 0)
             :engine-timing engine-timing
             :silent-form-idxs silent-form-idxs
             :llm-messages messages :llm-provider provider :llm-model model-name
             :llm-selected-provider (:provider resolved-model)
             :llm-selected-model (some-> (:name resolved-model) str)
             :llm-actual-provider provider
             :llm-actual-model model-name
             :llm-fallback-trace (:routed/fallback-trace ask-result)
             :llm-raw-response (:raw ask-result)
             :llm-executable-code (:result ask-result)
             :llm-executable-blocks (:blocks ask-result)
             :assistant-message (:assistant-message ask-result)}
            (let [final-answer* (append-runtime-appendices environment final-answer value)]
              {:thinking thinking
               :blocks blocks
               :final-result {:final?           true
                              :answer           final-answer*
                              ;; Index of the form that called
                            ;; `(turn-answer! ...)`. Channels use this to
                            ;; ELIDE the answer-bearing form from the
                            ;; per-iteration code trace (the channel
                            ;; renders the answer text below; showing
                            ;; `(turn-answer! "...")` above it is
                              ;; redundant prose-as-code).
                              :answer-form-idx  form-idx}
               :api-usage api-usage
               :duration-ms (or (:duration-ms ask-result) 0)
               :engine-timing engine-timing
               :silent-form-idxs silent-form-idxs
               :llm-messages messages :llm-provider provider :llm-model model-name
               :llm-selected-provider (:provider resolved-model)
               :llm-selected-model (some-> (:name resolved-model) str)
               :llm-actual-provider provider
               :llm-actual-model model-name
               :llm-fallback-trace (:routed/fallback-trace ask-result)
               :llm-raw-response (:raw ask-result)
               :llm-executable-code (:result ask-result)
               :llm-executable-blocks (:blocks ask-result)
               :assistant-message (:assistant-message ask-result)})))
          ;; Normal path
        {:thinking thinking
         :blocks blocks
         :final-result nil :api-usage api-usage
         :duration-ms (or (:duration-ms ask-result) 0)
         :engine-timing engine-timing
         :silent-form-idxs silent-form-idxs
         :llm-messages messages
         :llm-provider (actual-llm-provider resolved-model ask-result)
         :llm-model    (actual-llm-model resolved-model ask-result)
         :llm-selected-provider (:provider resolved-model)
         :llm-selected-model (some-> (:name resolved-model) str)
         :llm-actual-provider (actual-llm-provider resolved-model ask-result)
         :llm-actual-model (actual-llm-model resolved-model ask-result)
         :llm-fallback-trace (:routed/fallback-trace ask-result)
         :llm-raw-response (:raw ask-result)
         :llm-executable-code (:result ask-result)
         :llm-executable-blocks (:blocks ask-result)
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

(defn- iteration-error-feedback
  [iteration iteration-error-data user-request]
  (if (stream-output-overflow? iteration-error-data)
    (str "[Iteration " (inc (long iteration)) "]\n"
      "<error>Provider stopped the response as incomplete because output budget was exhausted (max_output_tokens).</error>\n"
      "Recovery policy: do not continue the broad strategy. Use a compact path now: one small probe if essential, otherwise stop, report the exact impediment, and ask for confirmation before more changes. Avoid dumping large maps, file contents, diffs, or repeated diagnostics.\n"
      "Original request: " user-request)
    (str "[Iteration " (inc iteration) "]\n"
      "<error>LLM call failed: " (:message iteration-error-data) "</error>\n"
      "Adjust your approach or emit :final with what you have.")))

(def ^:private CHAT_ERROR_BODY_RENDER_CHARS
  "Cap on raw upstream HTTP body chars surfaced inside the chat error
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

(defn- provider-error-data?
  [err]
  (let [data (:data err)]
    (boolean (or (:status data) (:body data) (:request-id data) (:request_id data)))))

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

(defn- provider-error-explanation
  [err]
  (let [data             (:data err)
        body-raw         (some-> (:body data) str)
        provider-message (provider-body-message body-raw)]
    (cond
      (invalid-thinking-signature-message? provider-message)
      (str "WHAT HAPPENED: Anthropic rejected the request before the model ran because Vis sent a `thinking` block with a signature that is not valid for Anthropic. "
        "Most likely cause: preserved-thinking replay crossed a provider/model boundary (for example Z.ai/Codex/OpenAI reasoning state was replayed into Anthropic), or an old Anthropic thinking block came from a different session/key. "
        "Fix: do not replay preserved-thinking unless provider AND model match; retry with only normal transcript/journal context.")

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

(defn- format-provider-error
  [err]
  (render/render (provider-error-ir err) :plain))

(defn- format-iteration-error
  "Render one trace `:error` map as a Markdown bullet for the user.
   Provider HTTP failures get a dedicated PROVIDER_ERROR block with the
   exact upstream message plus a human explanation. Local eval/form
   errors stay as normal bullets."
  [err]
  (if (provider-error-data? err)
    (format-provider-error err)
    (let [message    (or (:message err) (str err))
          data       (:data err)
          raw        (some-> (:raw-data data) str)
          recv       (:received-type data)
          provider-payload (when (and raw (not (str/blank? raw)))
                             (truncate raw 400))
          overflow? (stream-output-overflow? err)]
      (cond-> (str "- " message)
        overflow?        (str " Reason: max_output_tokens. Vis will retry with compact working memory and an explicit compact-recovery instruction.")
        provider-payload (str "\n  provider returned"
                           (when recv (str " (" recv ")"))
                           ": " provider-payload)))))

(defn- format-block-error
  "Render one failed code block from an iteration trace. These are not
   provider failures, but they are exactly the feedback the model needs
   after repeated parse/eval failures.

   Per PLAN §2.1 + §7.3.5: `:error` is the structured `:error`
   map; pull `:message` for terse one-line display."
  [iteration block]
  (let [pos  (or (:idx block) (:id block))
        code (some-> (:code block) str str/trim)
        err  (or (some-> block :error :message)
               (some-> block :error str))]
    (str "- Iteration " (inc (long (or iteration 0)))
      (when (some? pos) (str ", form " (inc (long pos))))
      " failed: " err
      (when-not (str/blank? code)
        (str "\n  code: `" (truncate (str/replace code #"\s+" " ") 240) "`")))))

(defn- recent-errors-block
  "Render recent provider-level and block-level errors as a Markdown
   feedback block. Returns nil when the trace carries no errors so
   callers can `(when ...)` without churn."
  [trace n]
  (let [errs (->> trace
               reverse
               (mapcat (fn [{:keys [iteration error blocks]}]
                         (concat
                           (when error [(format-iteration-error error)])
                           (keep #(when (:error %)
                                    (format-block-error iteration %))
                             blocks))))
               (take n))]
    (when (seq errs)
      (str "**Recent errors:**\n\n"
        (str/join "\n\n" errs)
        "\n\n"))))

(defn- recent-provider-error-ir
  [trace]
  (some->> trace
    reverse
    (keep :error)
    (filter provider-error-data?)
    first
    provider-error-ir))

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
  "Get or create the shared LLM router."
  []
  (or @router-atom
    (let [cfg (config/resolve-config)
          r   (svar/make-router (runtime-router-providers cfg))]
      (reset! router-atom r)
      r)))

(defn reset-router!
  []
  (reset! router-atom nil))

(defn rebuild-router!
  "Rebuild the router from the given config. Used when provider settings change."
  [config]
  (let [r (svar/make-router (runtime-router-providers config))]
    (reset! router-atom r)
    r))

(defn ask-code!
  "One-shot routed `svar/ask-code!` against the global router.
   Plain-text completion + fenced code-block extraction - returns the
   svar map `{:result :blocks :raw :tokens :cost :duration-ms}`.
   `ask!` (JSON-spec) is gone; every Vis caller uses `ask-code!`."
  [opts]
  (svar/ask-code! (get-router) opts))

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
                   (merge (dissoc opts :system :prompt :temperature)
                     {:messages           messages
                      :lang               "text"
                      :reasoning          (or reasoning :off)
                      :routing            (or routing {:optimize :cost})
                      :code-tail-pointer? true}
                     (when (some? temperature)
                       {:temperature temperature})))
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

(defn provider-has-reasoning?
  [router]
  (boolean (:reasoning? (resolve-effective-model router))))

(defn- provider-prompt-context
  "Build the append-only provider prompt hook context for the active
   router root. Secrets stay out: the provider config passed to the
   hook is limited to routing/model metadata, never `:api-key` or
   provider-specific credential fields."
  [environment resolved-model]
  (let [provider-id (:provider resolved-model)
        router-provider (some #(when (= provider-id (:id %)) %)
                          (:providers (:router environment)))
        descriptor (registry/provider-by-id provider-id)]
    (when (and provider-id descriptor (:provider/prompt-fn descriptor))
      {:provider    (assoc (select-keys (or router-provider {})
                             [:id :base-url :api-style :models :reasoning?])
                      :id provider-id)
       :descriptor  descriptor
       :model       resolved-model
       :environment environment})))

;; -----------------------------------------------------------------------------
;; Concurrency primitives (reentrant semaphore, deadline helpers)
;; -----------------------------------------------------------------------------

(defn make-reentrant-semaphore
  "Creates a reentrant semaphore with `permits` slots.
   Thread-id keyed: same thread can re-acquire without blocking.
   Different threads contend for permits fairly (FIFO).

   Returns a map with :acquire! and :release! fns."
  [permits]
  (let [sem (Semaphore. (int permits) true)
        thread-depths (ConcurrentHashMap.)]
    {:acquire!
     (fn acquire! []
       (let [tid (.getId (Thread/currentThread))
             depth (.getOrDefault thread-depths tid (int 0))]
         (if (pos? depth)
           (.put thread-depths tid (int (inc depth)))
           (do (.acquire sem)
             (.put thread-depths tid (int 1))))))

     :release!
     (fn release! []
       (let [tid (.getId (Thread/currentThread))
             depth (.getOrDefault thread-depths tid (int 0))]
         (when (pos? depth)
           (if (= depth 1)
             (do (.remove thread-depths tid)
               (.release sem))
             (.put thread-depths tid (int (dec depth)))))))

     :permits (fn [] (.availablePermits sem))
     :queued  (fn [] (.getQueueLength sem))}))

;; -----------------------------------------------------------------------------
;; Var snapshot + system var helpers (inlined from former shared)
;; -----------------------------------------------------------------------------

(defn extract-def-names
  "Extracts var names from code blocks via EDN parsing of def-like forms."
  [blocks]
  (->> blocks
    (mapcat (fn [{:keys [code error]}]
              (when-not error
                (try
                  (->> (edamame/parse-string-all (or code "") {:all true})
                    (keep (fn [form]
                            (when (seq? form)
                              (let [[op name & _] form]
                                (when (and (contains? '#{def defn defn- defonce defmulti defmacro} op)
                                        (symbol? name))
                                  name)))))
                    distinct)
                  (catch Exception _ [])))))
    (map str)
    vec))

(defn restorable-var-snapshots
  "Serializable snapshots of `(def ...)` vars introduced by this iteration."
  [environment blocks]
  (let [execution->defs (mapv (fn [{:keys [error] :as execution}]
                                [execution (when-not error
                                             (set (map symbol (extract-def-names [execution]))))])
                          blocks)
        defined (into #{} (mapcat second) execution->defs)
        symbol->execution (reduce (fn [acc [{:keys [code execution-time-ms]} defs]]
                                    (if (and code (seq defs))
                                      (reduce #(assoc %1 %2 {:expr code :time-ms execution-time-ms}) acc defs)
                                      acc))
                            {}
                            execution->defs)
        locals (get-locals environment)]
    (->> locals
      (keep (fn [[symbol-name value]]
              (when (contains? defined symbol-name)
                (let [realized-value (realize-value value)
                      execution-information (get symbol->execution symbol-name)]
                  (cond-> {:name (str symbol-name)
                           :value realized-value
                           :code (:expr execution-information)}
                    (:time-ms execution-information)
                    (assoc :time-ms (:time-ms execution-information)))))))
      vec)))

(defn update-system-vars!
  "Rebind the per-iteration SYSTEM vars in the SCI sandbox after an
   iteration commits. See `SYSTEM_VAR_NAMES` for the full SYSTEM-var
   registry.

   Touches:
     CONVERSATION_PREVIOUS_ANSWER - latest finalized turn answer; only
                                     bumps when this iteration produced
                                     a `final-result` (= terminal answer
                                     for this turn). For all earlier
                                     iterations of the same turn it
                                     keeps the previous turn's value.
     CONVERSATION_TITLE           - mirrors `:conversation-title-atom`
                                     so a `(set-conversation-title! \"...\")`
                                     inside iteration N is observable
                                     to the model in iteration N+1
                                     without a DB round-trip.

   Prior thinking text used to be bound to ITERATION_PREVIOUS_REASONING
   here. That var was retired once preserved-thinking replay started
   shipping the canonical assistant message back to the provider; the
   model now sees its prior reasoning natively in the messages array,
   and any forensic / inspection use case can read the iteration's
   `:thinking` column straight from the DB."
  [environment {:keys [final-result final-answer]}]
  (when final-result
    (env/bind-and-bump! environment 'CONVERSATION_PREVIOUS_ANSWER final-answer))
  (when-let [conversation-title-atom (:conversation-title-atom environment)]
    (env/bind-and-bump! environment 'CONVERSATION_TITLE (or @conversation-title-atom ""))))

(defn update-iteration-id!
  "Rebind `TURN_ITERATION_ID` and `TURN_ITERATION_POSITION` in the
   SCI sandbox to the freshly-persisted iteration row's UUID + 1-based
   position. Mirrors `:current-iteration-id-atom`. No-op for both when
   `iteration-id` is nil. `iteration-position` is optional; nil leaves
   the position bound at its previous value (callers that don't know
   it can pass nil)."
  ([environment iteration-id]
   (update-iteration-id! environment iteration-id nil))
  ([environment iteration-id iteration-position]
   (when iteration-id
     (env/bind-and-bump! environment 'TURN_ITERATION_ID iteration-id))
   (when iteration-position
     (env/bind-and-bump! environment 'TURN_ITERATION_POSITION (long iteration-position)))))

(defn inject-system-var-snapshots
  "Append a SYSTEM-var snapshot to `vars-snapshot` for EVERY name in
   `SYSTEM_VAR_NAMES` on EVERY iteration. The inspect transcript and
   persisted var history then have ONE row per iteration for each X,
   even when the value is unchanged or blank.

   Yes, turn-frozen vars (TURN_ID, TURN_POSITION,
   TURN_CONVERSATION_STATE_ID, TURN_SYSTEM_PROMPT,
   TURN_ACTIVE_EXTENSIONS, TURN_ACCESSIBLE_SKILLS,
   CONVERSATION_STATE_ID) repeat verbatim across
   iterations of the same turn - that is the intentional contract:
   \"every iteration carries a snapshot of every SYSTEM var\". The
   dedup-on-unchanged optimization the previous version did was a
   row-saving micro-opt that kept the var-history vec stuck on iter 0
   for those names.

   Each var is normalized to a non-nil string so `expression_state`
   never stores nil for a SYSTEM var - makes the version vec a clean
   log of values across iterations."
  [vars-snapshot {:keys [final-answer
                         turn-id turn-position
                         iteration-id iteration-position
                         conversation-state-id
                         system-prompt
                         extensions-snapshot accessible-skills-snapshot
                         conversation-title]}]
  (let [stamp (fn [vs nm v]
                (conj vs {:name nm :value v :code ";; SYSTEM var"}))]
    (-> vars-snapshot
      (stamp "TURN_ID"                      (or turn-id ""))
      (stamp "TURN_POSITION"                (or turn-position 0))
      (stamp "TURN_CONVERSATION_STATE_ID"   (or conversation-state-id ""))
      (stamp "TURN_SYSTEM_PROMPT"           (or system-prompt ""))
      (stamp "TURN_ACTIVE_EXTENSIONS"       (or extensions-snapshot []))
      (stamp "TURN_ACCESSIBLE_SKILLS"       (or accessible-skills-snapshot []))
      (stamp "TURN_ITERATION_ID"            (or iteration-id ""))
      (stamp "TURN_ITERATION_POSITION"      (or iteration-position 0))
      (stamp "CONVERSATION_STATE_ID"        (or conversation-state-id ""))
      (stamp "CONVERSATION_TITLE"           (or conversation-title ""))
      (stamp "CONVERSATION_PREVIOUS_ANSWER" (or final-answer "")))))

(defn update-title-system-var!
  "Rebind CONVERSATION_TITLE in the SCI sandbox to whatever the env's
   conversation-title-atom currently holds. Called once at iteration 0
   so the first iteration sees the live title; per-iteration rebinds
   happen in `update-system-vars!` (alongside
   `CONVERSATION_PREVIOUS_ANSWER`)."
  [environment]
  (when-let [conversation-title-atom (:conversation-title-atom environment)]
    (env/bind-and-bump! environment 'CONVERSATION_TITLE (or @conversation-title-atom ""))))

;; =============================================================================
;; System Prompt
;; =============================================================================

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
   bindings only; persisted history remains available through
   `var-history`.

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
   history remains the source of truth for `var-history`."
  [{:keys [db-info conversation-id sci-ctx initial-ns-keys bindings-atom]}]
  (when (and db-info conversation-id sci-ctx)
    (try
      (let [var-registry (persistance/db-latest-var-registry db-info conversation-id)
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
          (archive-vars! sci-ctx candidates)
          (when bindings-atom
            (swap! bindings-atom update :current-revision inc))))
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
  ;; `:journal-iters` is a vec of `[iteration-position {:thinking :blocks}]`
  ;; pairs (oldest-first). The prompt renderer trims the rendered
  ;; journal by token budget (50% of model context), not fixed
  ;; iteration count.
  {:journal-iters []})

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

(def ^:private auto-diagnose-request-pattern
  #"(?i)\b(fix|bug|broken|failing|failure|duplicate|wrong|stuck|debug|diagnose|regression|throwing|crash|error)\b")

(defn- auto-skill-names
  [user-request]
  (cond-> []
    (re-find auto-diagnose-request-pattern (str user-request))
    (conj "diagnose")))

(do
  (defn- activate-auto-skills!
    [environment user-request]
    (when-let [active-skills-atom (:active-skills-atom environment)]
      (doseq [skill-name (auto-skill-names user-request)]
        (try
          (let [skill (skills/lookup skill-name)]
            (when (:found? skill)
              (swap! active-skills-atom assoc (:name skill) (assoc skill :auto? true))))
          (catch Throwable t
            (tel/log! {:level :warn :id ::auto-skill-activation-failed
                       :data {:skill skill-name
                              :error (ex-message t)}}))))
      @active-skills-atom))

  (defn- previous-turn-context
    "Latest completed prior turn answer for short follow-up disambiguation.

     Full chat replay stays out of provider messages. This bounded map is
     passed to prompt/assemble-initial-messages so `A`, `yes`, and `do it` can
     resolve against the immediately previous final answer instead of only the
     token-budgeted journal."
    [{:keys [db-info conversation-id]} current-turn-id]
    (try
      (when (and db-info conversation-id)
        (some->> (persistance/db-list-conversation-turns db-info conversation-id)
          (remove #(= (str (:id %)) (str current-turn-id)))
          (filter #(and (= :complete (:prior-outcome %))
                     (not (str/blank? (str (:answer %))))))
          (sort-by (fn [turn]
                     [(long (or (:position turn) 0))
                      (str (or (:created-at turn) ""))]))
          last
          (#(select-keys % [:id :position :user-request :answer]))))
      (catch Throwable t
        (tel/log! {:level :warn :id ::previous-turn-context-failed
                   :data {:error (ex-message t)}})
        nil))))

(defn iteration-loop
  "The core iteration loop. Runs assemble -> ask LLM -> execute -> persist
   until the model emits `:answer`, the user cancels, or the
   consecutive-error budget is exhausted."
  [environment user-request
   {:keys [system-prompt
           conversation-turn-id
           max-consecutive-errors max-restarts max-context-tokens
           hooks cancel-atom current-iteration-atom
           reasoning-default routing extra-body turn-features allow-copilot-claude-deep?
           workspace]}]
  (let [environment (cond-> environment
                      (seq turn-features) (assoc :turn/features turn-features)
                      (seq workspace) (merge workspace))
        ;; Tightened from 5 to 3. Three consecutive failures is enough
        ;; signal that the current approach is wrong; the nudge fires
        ;; at CONSECUTIVE_ERROR_NUDGE_AT (= 2) so the model gets a
        ;; warning before the strategy-restart kicks in.
        max-consecutive-errors (or max-consecutive-errors 3)
        max-restarts (or max-restarts 3)
        resolved-model (resolve-effective-model (:router environment))
        effective-model (:name resolved-model)
        _ (assert effective-model "Router must resolve a root model")
        has-reasoning? (reasoning-effort-configurable? resolved-model)
        base-reasoning-level (or (normalize-reasoning-level reasoning-default) balanced-reasoning)
        ;; Activate extensions ONCE per turn. Threaded through both the
        ;; stable prompt message assembler (cacheable prefix) and the
        ;; per-iteration ext nudge collector - activation-fn never re-fires inside the loop.
        active-exts   (prompt/active-extensions environment)
        _             (sync-active-extension-symbols! environment active-exts)
        _             (when-let [a (:active-skills-atom environment)]
                        (reset! a {}))
        _             (activate-auto-skills! environment user-request)
        stable-prompt-messages (prompt/assemble-stable-prompt-messages environment
                                 {:system-prompt     system-prompt
                                  :active-extensions active-exts})
        stable-prompt-content (prompt/stable-prompt-text stable-prompt-messages)
        initial-user-content user-request
        previous-turn-ctx (previous-turn-context environment conversation-turn-id)
        initial-messages (prompt/assemble-initial-messages
                           {:stable-prompt-messages stable-prompt-messages
                            :initial-user-content   initial-user-content
                            :previous-turn-context  previous-turn-ctx})
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
                                       ;; svar's `estimate-cost` returns a MAP
                                       ;; `{:input-cost :output-cost :total-cost
                                       ;; :model :pricing}`, NOT a bare number.
                                       ;; Pull `:total-cost` out; nil pricing
                                       ;; (e.g. unknown model) leaves the
                                       ;; column NULL on disk, which the read
                                       ;; side defaults to 0.0.
                                       cost-map (estimate-token-cost effective-model in out {:api-usage api-usage})
                                       total    (when (map? cost-map) (:total-cost cost-map))]
                                   {:tokens   {:input in :output out :reasoning reas :cached cach}
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
                                    :total total-tokens}
                           :cost cost}))
        bindings-atom (or (:bindings-atom environment)
                        (atom {:index nil :revision -1 :current-revision 0}))
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
        ;; Post-phase `:ext/hooks` invocation. Walks every active
        ;; extension's hooks vector, filters by namespaced :phase, calls :fn
        ;; with the post-eval ctx. Return value is IGNORED — these are
        ;; side-effect-only telemetry / logging / external-resource hooks.
        ;; Pre-phase hooks fire from prompt/build-iteration-context.
        emit-post-hooks! (fn [target-phase ctx]
                           (doseq [ext (or active-exts [])
                                   {:keys [id phase] hook-fn :fn} (or (:ext/hooks ext) [])
                                   :when (= phase target-phase)]
                             (binding [extension/*current-extension* ext
                                       extension/*current-symbol* nil]
                               (try (hook-fn (assoc ctx :phase target-phase))
                                 (catch Throwable t
                                   (tel/log! {:level :warn
                                              :id ::hook-threw
                                              :data {:ext (:ext/namespace ext)
                                                     :hook id
                                                     :phase target-phase
                                                     :error (ex-message t)}}))))))
        ;; Metadata persisted on each iteration row - reuses the
        ;; precomputed `active-exts` (no second activation pass).
        ;;
        ;; Source markers (paths, max-mtime, sha256) are written ONLY
        ;; on the first iteration of each turn (`iter-pos` = 0 internally,
        ;; persisted as position 1). The v2 change-detector reads `WHERE position = 1 ORDER BY
        ;; created_at DESC LIMIT 1` to compare against current state.
        ;; Subsequent iterations omit them - cuts ~99 % redundant DB
        ;; volume vs every-iteration. See plan Q15.
        iteration-metadata (fn [iter-pos llm-meta]
                             (cond-> {}
                               (seq llm-meta) (assoc :llm llm-meta)
                               (seq active-exts)
                               (assoc :extensions (mapv (fn [ext]
                                                          (let [ns-sym  (:ext/namespace ext)
                                                                markers (when (zero? (long iter-pos))
                                                                          (extension/extension-source-markers-of ns-sym))]
                                                            (cond-> {:namespace (str ns-sym)}
                                                              (:ext/version ext) (assoc :version (:ext/version ext))
                                                              markers            (merge (select-keys markers
                                                                                          [:source-paths
                                                                                           :source-mtime-max
                                                                                           :source-hash-sha256])))))
                                                    active-exts))))]
    ;; -----------------------------------------------------------------
    ;; Turn-start SYSTEM-var bindings.
    ;;
    ;; Every `TURN_*` here is bound exactly once per turn and never
    ;; mutated again until the next turn opens - the model gets a
    ;; stable view for the entire iteration loop. `ITERATION_*` resets
    ;; here and rebinds per iteration. `CONVERSATION_*` is touched at
    ;; iteration boundaries via `update-system-vars!` /
    ;; `update-title-system-var!`.
    ;; -----------------------------------------------------------------
    ;; TURN_USER_REQUEST retired. The current human turn text and richer
    ;; per-iteration / cross-turn history both flow through
    ;; `(v/conversation-state)` -> :current-turn :user-request / :transcript :turns.
    (env/bind-and-bump! environment 'TURN_ID conversation-turn-id)
    (let [turn-position (try
                          (some->> (persistance/db-list-conversation-turns
                                     (:db-info environment) (:conversation-id environment))
                            (some #(when (= (str (:id %)) (str conversation-turn-id))
                                     (:position %)))
                            long)
                          (catch Throwable _ 0))]
      (env/bind-and-bump! environment 'TURN_POSITION (or turn-position 0))
      (when-let [a (:current-turn-position-atom environment)]
        (reset! a (or turn-position 0))))
    (let [conversation-state-id (persistance/db-latest-conversation-state-id
                                  (:db-info environment) (:conversation-id environment))]
      (env/bind-and-bump! environment 'TURN_CONVERSATION_STATE_ID conversation-state-id)
      (env/bind-and-bump! environment 'CONVERSATION_STATE_ID conversation-state-id))
    ;; The stable prompt prefix that drives THIS turn, joined only for
    ;; debug/SYSTEM-var visibility. Provider send path keeps the original
    ;; separate messages for cache boundaries.
    (env/bind-and-bump! environment 'TURN_SYSTEM_PROMPT stable-prompt-content)
    ;; TURN_ACTIVE_EXTENSIONS = frozen, fully-realized vec describing
    ;; every extension that activated for THIS turn. Built off the same
    ;; `active-exts` we hand to the prompt assembler / nudge collector,
    ;; so the agent's <bindings> picture matches the actually-loaded
    ;; surface.
    (env/bind-and-bump! environment 'TURN_ACTIVE_EXTENSIONS
      (prompt/extensions-snapshot active-exts))
    ;; TURN_ACCESSIBLE_SKILLS = frozen vec of skill summaries the model
    ;; can filter/map/some over. Bodies are NOT included; loading one
    ;; is the internal activation step via (load-skill! name). See
    ;; prompt/accessible-skills-snapshot for the per-element shape.
    ;; The lazy `(skills)` sandbox fn (internal.skills/sandbox-bindings)
    ;; returns the same shape on demand; both surfaces stay in sync.
    (env/bind-and-bump! environment 'TURN_ACCESSIBLE_SKILLS
      (prompt/accessible-skills-snapshot))
    ;; Reset TURN_ITERATION_ID + TURN_ITERATION_POSITION at turn start;
    ;; rebound by
    ;; `update-iteration-id!` after each iteration row commits.
    (env/bind-and-bump! environment 'TURN_ITERATION_ID nil)
    (env/bind-and-bump! environment 'TURN_ITERATION_POSITION 0)
    (update-title-system-var! environment)
    (when-let [a (:current-iteration-id-atom environment)] (reset! a nil))
    (when-let [a (:current-conversation-turn-id-atom environment)] (reset! a conversation-turn-id))
    (when-let [a (:current-user-request-atom environment)] (reset! a user-request))
    ;; REPL-style recovery slots (`*1` `*2` `*3` `*e`) are per-turn. A
    ;; follow-up turn opens with all four nil so leftover values from
    ;; the previous turn never bleed into the new OODA loop.
    (env/reset-eval-bindings! environment)
    ;; Hot symbol compaction is archive-based and runs only after a
    ;; final successful answer. Failed/cancelled turns keep their live
    ;; scratch symbols for recovery.
    ;; Cross-turn carry: seed `journal-iters` with persisted iterations
    ;; of the current conversation (across every prior turn) so a
    ;; follow-up turn opens with prior context. Rendering trims by token
    ;; budget, so carry is not capped by iteration count. Each entry is
    ;; `[iter-position {:thinking :blocks}]` matching the in-memory shape
    ;; the renderer expects. Failures degrade silently to an empty seed.
    (let [seeded-journal-iters
          (try
            (when-let [conv-id (:conversation-id environment)]
              (let [d (:db-info environment)
                    queries (persistance/db-list-conversation-turns d conv-id)
                    iters (->> queries
                            (mapcat (fn [q]
                                      (try (persistance/db-list-conversation-turn-iterations d (:id q))
                                        (catch Throwable _ []))))
                            (sort-by :created-at)
                            vec)]
                (mapv (fn [it]
                        [(or (:position it) 1)
                         {:thinking (:thinking it)
                          :blocks   (try (persistance/db-list-iteration-blocks d (:id it))
                                      (catch Throwable _ []))
                          :llm-provider (:provider it)
                          :llm-model    (some-> (:model it) str)
                          ;; svar canonical assistant message persisted on the
                          ;; iteration row; rehydrating here keeps preserved-thinking
                          ;; replay alive across vis restarts, but only for the
                          ;; same provider/model that produced it.
                          :assistant-message (:llm-assistant-message it)}])
                  iters)))
            (catch Throwable t
              (tel/log! {:level :warn :id ::cross-turn-journal-seed-failed
                         :data  {:error (ex-message t)}
                         :msg   "Cross-turn journal seed failed; first iteration starts with an empty <journal>"})
              nil))]
      (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :iteration-loop})]
        (loop [loop-state (merge {:iteration 0 :messages initial-messages
                                  :trace [] :consecutive-errors 0 :restarts 0}
                            FRESH_ITER_CARRY
                            (when (seq seeded-journal-iters)
                              {:journal-iters seeded-journal-iters}))]
          (let [{:keys [iteration messages trace consecutive-errors restarts
                        journal-iters]} loop-state]
            (when current-iteration-atom (reset! current-iteration-atom (inc (long iteration))))
            (cond
              (when cancel-atom @cancel-atom)
              (do (log-stage! :error iteration {:reason :cancelled})
                (let [result (merge {:answer nil :status :cancelled :status-id (status->id :cancelled)
                                     :trace trace :iteration-count iteration} (finalize-cost))]
                  result))

              :else
              (if (>= consecutive-errors max-consecutive-errors)
                (if (< restarts max-restarts)
                  (let [failed (->> trace (filter :error) (take 3)
                                 (map #(str "- " (get-in % [:error :message] (str (:error %)))))
                                 (str/join "\n"))
                        hint (str "Previous attempts failed with these errors:\n" failed
                               "\n\nStart fresh with a DIFFERENT strategy.\n\nOriginal request: " user-request)
                        messages (conj stable-prompt-messages {:role "user" :content hint})]
                    (recur (assoc loop-state
                             :iteration (inc iteration) :messages messages
                             :trace trace :consecutive-errors 0 :restarts (inc restarts))))
                  (let [errors-block (recent-errors-block trace 3)
                        fallback     (str "Warning: Too many errors (" consecutive-errors ") across "
                                       (inc restarts) " restart(s).\n\n"
                                     ;; Includes the raw provider payload
                                     ;; (e.g. an HTTP plain-text auth rejection) so
                                     ;; the user can act on it instead of guessing.
                                       errors-block)
                        result       (merge {:answer fallback
                                             :status :error :status-id (status->id :error)
                                             :trace trace :iteration-count iteration} (finalize-cost))]
                    result))

                (let [raw-reasoning-level (when has-reasoning?
                                            (reasoning-level-for-errors base-reasoning-level consecutive-errors))
                      reasoning-level (copilot-claude-safe-reasoning-level
                                        resolved-model user-request raw-reasoning-level
                                        {:allow-copilot-claude-deep? allow-copilot-claude-deep?})
                      _ (log-stage! :turn.iteration/start iteration {:message-count (count messages)
                                                                     :reasoning reasoning-level
                                                                     :requested-reasoning raw-reasoning-level})
                      pre-resolved-model (resolve-effective-model (:router environment) (or routing {}))
                      iteration-context (prompt/build-iteration-context environment
                                          {:blocks-by-iteration journal-iters
                                           :active-extensions   active-exts
                                           :iteration           iteration
                                           :model               (some-> pre-resolved-model :name str)
                                           :context-limit       max-context-tokens
                                           :current-user-content user-request
                                           :stable-prompt-content stable-prompt-content
                                           :provider-prompt-context
                                           (provider-prompt-context environment pre-resolved-model)
                                           ;; One low-importance turn-boundary check keeps
                                           ;; titles live across topic shifts. The old
                                           ;; iteration-only cadence never fired for a
                                           ;; string of short turns, which reproduced in
                                           ;; conversation 9a55ca1a.
                                           :title-refresh?      (zero? (long iteration))})
                      ;; Single canonical preserved-thinking replay path —
                      ;; svar's per-provider wire serializer turns the
                      ;; canonical assistant messages into native
                      ;; Anthropic / z.ai / Responses shapes.
                      ;;
                      ;; R3 hybrid message shape (per ADR/conversation
                      ;; 1db62d10): preserved-thinking replays + the
                      ;; iteration-context journal trailer both APPEND to
                      ;; the end. The original user_initial stays as the
                      ;; ONE user-role anchor near the start (placed there
                      ;; by `assemble-initial-messages`); we never repeat
                      ;; it. Final wire shape:
                      ;;
                      ;;   [system, user_initial,
                      ;;    asst_iter1, user_journal_after_iter1,
                      ;;    asst_iter2, user_journal_after_iter2,
                      ;;    ...
                      ;;    asst_iter(n-1), user_journal_after_iter(n-1)]
                      ;;
                      ;; This matches z.ai's canonical preserved-thinking
                      ;; example (user → asst → user → asst → user) and
                      ;; stops GLM-5.1 from re-reading the same initial
                      ;; goal every iter and restarting its plan.
                      provider-messages (append-preserved-thinking-replay
                                          messages journal-iters (replay-context pre-resolved-model))
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
                                           :previous-iterations journal-iters
                                           :previous-blocks (vec (mapcat (comp :blocks second) journal-iters))}
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
                      (let [error-feedback (iteration-error-feedback iteration iteration-error-data user-request)
                            trace-entry {:iteration iteration :error iteration-error-data :final? false}
                            empty-reasoning (when (= :svar.llm/empty-content (:type iteration-error-data))
                                              (:reasoning (:data iteration-error-data)))
                            err-iteration-id (persistance/db-store-iteration! (:db-info environment)
                                               (let [tc (iteration-token-cost (:api-usage iteration-result))]
                                                 (cond-> {:conversation-turn-id conversation-turn-id :vars [] :blocks nil
                                                          :thinking empty-reasoning :duration-ms 0 :error iteration-error-data
                                                          :llm-messages effective-messages
                                                          :llm-provider (:provider resolved-model)
                                                          :llm-model (str (:name resolved-model))
                                                          :metadata (iteration-metadata iteration
                                                                      (cond-> {:selected (llm-id (:provider resolved-model) (some-> (:name resolved-model) str))
                                                                               :actual   (llm-id (:provider resolved-model) (some-> (:name resolved-model) str))
                                                                               :fallback? false}
                                                                        (seq (get-in iteration-error-data [:data :routed/fallback-trace]))
                                                                        (assoc :fallback? true
                                                                          :fallback-trace (vec (get-in iteration-error-data [:data :routed/fallback-trace])))))}
                                                   tc (assoc :tokens (:tokens tc)
                                                        :cost-usd (:cost-usd tc)))))]
                        (when-let [a (:current-iteration-id-atom environment)] (reset! a err-iteration-id))
                        (update-iteration-id! environment err-iteration-id (long (or iteration 0)))
                        ;; Post-phase :turn.iteration/stop hook (error path).
                        (emit-post-hooks! :turn.iteration/stop
                          {:iteration       (inc (long (or iteration 0)))
                           :iteration-id    err-iteration-id
                           :status          :error
                           :thinking        empty-reasoning
                           :blocks          nil
                           :error           iteration-error-data
                           :duration-ms     0})
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
                                fallback (or (recent-provider-error-ir trace')
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
                                   :trace (conj trace trace-entry)
                                   :consecutive-errors (inc consecutive-errors) :restarts restarts)))))

                    (let [_ (accumulate-usage! (:api-usage iteration-result))
                          {:keys [thinking blocks final-result]} iteration-result
                          final-answer (when final-result (:answer final-result))
                          _ (update-system-vars! environment
                              {:thinking thinking :final-result final-result :final-answer final-answer})
                          vars-snapshot (restorable-var-snapshots environment blocks)
                          previous-iteration-id (some-> (:current-iteration-id-atom environment) deref)
                          conversation-row (try
                                             (persistance/db-get-conversation
                                               (:db-info environment)
                                               (:conversation-id environment))
                                             (catch Throwable _ nil))
                          conversation-metadata (when conversation-row
                                                  (cond-> {:title       (:title conversation-row)
                                                           :channel     (:channel conversation-row)
                                                           :external-id (:external-id conversation-row)
                                                           :created-at  (:created-at conversation-row)}
                                                    (:turn-count conversation-row)
                                                    (assoc :turn-count (:turn-count conversation-row))))
                          turn-position (try
                                          (some->> (persistance/db-list-conversation-turns
                                                     (:db-info environment) (:conversation-id environment))
                                            (some #(when (= (str (:id %)) (str conversation-turn-id))
                                                     (:position %)))
                                            long)
                                          (catch Throwable _ 0))
                          vars-snapshot (inject-system-var-snapshots vars-snapshot
                                          {:thinking           thinking
                                           :final-answer       final-answer
                                           :turn-id                        conversation-turn-id
                                           :turn-position      (or turn-position 0)
                                           :iteration-id       previous-iteration-id
                                           :iteration-position (long (or iteration 0))
                                           :conversation-state-id (persistance/db-latest-conversation-state-id
                                                                    (:db-info environment)
                                                                    (:conversation-id environment))
                                           :system-prompt      stable-prompt-content
                                         ;; Same frozen snapshots bound in SCI.
                                         ;; Re-stamped every iteration so inspect
                                         ;; transcript data returns one row per
                                         ;; iter, not just iter 0.
                                           :extensions-snapshot        (prompt/extensions-snapshot active-exts)
                                           :accessible-skills-snapshot (prompt/accessible-skills-snapshot)
                                         ;; Live conversation title; same value
                                         ;; the SCI sandbox sees as CONVERSATION_TITLE.
                                           :conversation-title    (some-> (:conversation-title-atom environment)
                                                                    deref str)
                                         ;; Frozen-at-this-iter map of conversation
                                         ;; facts (channel, external-id, turn-count,
                                         ;; created-at). Saves the model an iter
                                         ;; round-trip when it just needs one of
                                         ;; those fields.
                                           :conversation-metadata conversation-metadata})
                          iteration-id (persistance/db-store-iteration! (:db-info environment)
                                         (let [tc (iteration-token-cost (:api-usage iteration-result))]
                                           (cond-> {:conversation-turn-id conversation-turn-id :blocks blocks :vars vars-snapshot
                                                    :thinking thinking
                                                    :answer (when final-result (answer-str (:answer final-result)))
                                                    :answer-form-idx (when final-result (:answer-form-idx final-result))
                                                    :duration-ms (or (:duration-ms iteration-result) 0)
                                                    :llm-messages (:llm-messages iteration-result)
                                                    :llm-provider (or (:llm-provider iteration-result) (:provider resolved-model))
                                                    :llm-model (:llm-model iteration-result)
                                                    :llm-raw-response (:llm-raw-response iteration-result)
                                                    :llm-executable-code (:llm-executable-code iteration-result)
                                                    :llm-executable-blocks (:llm-executable-blocks iteration-result)
                                                    :llm-assistant-message (:assistant-message iteration-result)
                                                    :metadata (cond-> (iteration-metadata iteration
                                                                        (llm-routing-metadata pre-resolved-model iteration-result))
                                                                (seq (:engine-timing iteration-result))
                                                                (assoc :engine-timing (:engine-timing iteration-result)))}
                                             tc (assoc :tokens (:tokens tc)
                                                  :cost-usd (:cost-usd tc)))))
                          _ (when-let [a (:current-iteration-id-atom environment)] (reset! a iteration-id))
                          _ (update-iteration-id! environment iteration-id (long (or iteration 0)))
                          trace-entry {:iteration iteration :thinking thinking
                                       :blocks blocks :final? (boolean final-result)}
                          _ (emit-post-hooks! :turn.iteration/stop
                              {:iteration       (inc (long (or iteration 0)))
                               :iteration-id    iteration-id
                               :status          (if final-result :final :continue)
                               :thinking        thinking
                               :blocks          blocks
                               :error           nil
                               :duration-ms     (or (:duration-ms iteration-result) 0)})]
                      (cond
                        final-result
                        (do (log-stage! :final iteration
                              {:answer (truncate (answer-str (:answer final-result)) 200)
                               :iteration-count (inc iteration)})
                          (log-stage! :turn.iteration/stop iteration
                            {:blocks (count blocks) :errors (count (filter :error blocks))
                             :times (mapv :execution-time-ms blocks)})
                        ;; Iteration-final chunk (`:phase :iteration-final`).
                        ;; Per-form chunks already streamed every form
                        ;; result; this is the trim \"iteration is
                        ;; complete, here is the terminal answer\"
                        ;; signal. Consumers attach `:final` to
                        ;; whatever's already on screen.
                        ;;
                        ;; `:answer-form-idx` tells the channel which
                        ;; per-form slot was the `(turn-answer! ...)` call;
                        ;; the progress tracker elides that slot so
                        ;; the renderer doesn't paint the answer
                        ;; call's code above the answer text.
                          (when on-chunk
                            (on-chunk {:phase            :iteration-final
                                       :iteration        (inc (long iteration))
                                       :thinking         thinking
                                       :final            {:answer          (:answer final-result)
                                                          :iteration-count (inc iteration)
                                                          :status          :success}
                                       :answer-form-idx  (:answer-form-idx final-result)
                                       :silent-form-idxs (:silent-form-idxs iteration-result)
                                       :done?            true}))
                          (let [result (-> (merge {:answer (:answer final-result) :trace (conj trace trace-entry)
                                                   :iteration-count (inc iteration)}
                                             (finalize-cost))
                                         (attach-llm-routing-summary pre-resolved-model iteration-result))]
                            (emit-post-hooks! :turn/stop
                              {:status          :final
                               :iteration       (long (or iteration 0))
                               :iteration-count (inc iteration)
                               :tokens          (:tokens result)
                               :cost-usd        (:cost result)
                               :answer          (:answer final-result)
                               :error           nil})
                            (auto-archive-hot-symbols! environment)
                            result))

                        :else
                        (if (empty? blocks)
                          (do (log-stage! :empty iteration {})
                            (log-stage! :turn.iteration/stop iteration {:blocks 0 :errors 0 :times []})
                            (recur (merge loop-state
                                     {:iteration (inc iteration) :trace (conj trace trace-entry)})))

                          (do (log-stage! :turn.iteration/stop iteration
                                {:blocks (count blocks) :errors (count (filter :error blocks))
                                 :times (mapv :execution-time-ms blocks)})
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
                            (let [had-success? (some #(nil? (:error %)) blocks)
                                  next-errors (if had-success? 0 (inc consecutive-errors))
                                  _ (when had-success? (swap! bindings-atom update :current-revision inc))
                                ;; Carry forward all observed iterations
                                ;; as `[pos {:thinking :blocks}]` for the
                                ;; next iteration's `<journal>`. The
                                ;; renderer drops oldest lines by token
                                ;; budget, not by iteration count.
                                  next-recent (conj (vec (or journal-iters []))
                                                [(inc (long iteration))
                                                 {:thinking thinking
                                                  :blocks   blocks
                                                  :llm-executable-code (:llm-executable-code iteration-result)
                                                  :llm-provider (:llm-provider iteration-result)
                                                  :llm-model    (:llm-model iteration-result)
                                                  ;; svar's canonical replay handle for this
                                                  ;; iteration. Re-emitted into messages on
                                                  ;; the next iteration via
                                                  ;; `append-preserved-thinking-replay`; the
                                                  ;; per-provider wire serializer translates
                                                  ;; it to native shapes.
                                                  :assistant-message (:assistant-message iteration-result)}])]
                              (recur (merge loop-state
                                       {:iteration          (inc iteration)
                                        :messages           messages
                                        :trace              (conj trace trace-entry)
                                        :consecutive-errors next-errors
                                        :journal-iters       next-recent})))))))))))))))))

(defn run-turn!
  "Store turn -> iteration-loop -> update turn -> return result.

   Derives `:prior-outcome` (one of `:complete`, `:cancelled`, `:error`)
   from the loop result and
   persists it on the `conversation_turn_state` row. The next turn's
   `<system_state>` digest reads it."
  [env user-request loop-opts]
  (when-not (map? env)
    (throw (ex-info "run-turn! requires an env map" {:got (type env)})))
  (when (clojure.string/blank? user-request)
    (throw (ex-info "run-turn! requires a non-blank user request" {:got user-request})))
  (let [conversation-turn-id (persistance/db-store-conversation-turn! (:db-info env)
                               {:parent-conversation-id (:conversation-id env)
                                :user-request user-request
                                :messages nil
                                :status :running})
        result (iteration-loop env user-request (assoc loop-opts :conversation-turn-id conversation-turn-id))
        prior-outcome (:status result)
        _ (persistance/db-update-conversation-turn! (:db-info env) conversation-turn-id
            {;; Coerce through `answer-str` so the persisted answer is
             ;; ALWAYS the plain-text rendering, never a stringified
             ;; IR vector. Some terminal paths (e.g. error/cancel
             ;; fallbacks) feed `:answer` in as the raw value passed
             ;; to `(turn-answer! ...)`; without this coercion the TUI
             ;; resume path showed literal `[:ir [:p "..."]]` to
             ;; the user (convo b7ba1d93 regression).
             :answer          (when-let [a (:answer result)] (answer-str a))
             :iteration-count (:iteration-count result)
             :duration-ms     (:duration-ms result)
             :status          (or (:status result) :success)
             :tokens          (:tokens result)
             :cost            (:cost result)
             :prior-outcome   prior-outcome})]
    (assoc result :conversation-turn-id conversation-turn-id :prior-outcome prior-outcome)))

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
          ;;   2. (The retired `TURN_USER_REQUEST` SYSTEM var, bound from
          ;;      this same string, grew with each turn instead of
          ;;      reflecting the current ask. Surface now flows through
          ;;      `(v/conversation-state)` -> :current-turn :user-request.)
          ;;   3. The synthetic `{:requirement ...}` frame the LLM sees
          ;;      restated the whole conversation as the "requirement".
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
          ;; <journal>, <bindings>, SYSTEM vars, and DB-backed tools.
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
          _                      (env/bump-bindings! env)
          _                      (doseq [[sym val] (or custom-bindings {})]
                                   (when val
                                     (env/sci-update-binding! sci-ctx sym val)))
          _                      (env/bump-bindings! env)
          current-iteration-id-atom (:current-iteration-id-atom env)
          current-conversation-turn-id-atom (:current-conversation-turn-id-atom env)
          workspace              (select-keys opts [:workspace/root :workspace/id
                                                    :workspace/repo-id :workspace/state
                                                    :workspace])
          environment            (cond-> (assoc env
                                           :current-iteration-atom current-iteration-atom
                                           :current-iteration-id-atom current-iteration-id-atom
                                           :current-conversation-turn-id-atom current-conversation-turn-id-atom)
                                   (seq workspace) (merge workspace))
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
       :workspace              workspace
       :messages               messages})))

;; -----------------------------------------------------------------------------
;; Run iteration phase
;; -----------------------------------------------------------------------------

(defn- run-iteration-phase
  "Runs the main iteration loop via run-turn!.
   Returns iteration-result, conversation-turn-id, cost atoms, and merge-cost! fn."
  [{:keys [environment user-request spec
           max-context-tokens system-prompt
           current-iteration-atom hooks cancel-atom
           reasoning-default routing extra-body turn-features workspace]}]
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
                             (seq workspace) (assoc :workspace workspace)))
        conversation-turn-id         (:conversation-turn-id iteration-result)
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
     :conversation-turn-id         conversation-turn-id
     :total-tokens-atom total-tokens-atom
     :total-cost-atom   total-cost-atom
     :merge-cost!       merge-cost!}))

;; =============================================================================
;; Title listeners + set-title! broadcast
;;
;; Channels (TUI, Telegram, ...) that want to react to a conversation
;; title change - typically because the model emitted `(set-conversation-title! "...")`
;; mid-turn - register a listener via `add-title-listener!`. The
;; listener fn receives the new title; it MUST be cheap (typically a
;; `state/dispatch` into the channel's app-db). Listeners are stored
;; per conversation-id so a TUI watching conversation A doesn't get
;; woken by a Telegram bot updating conversation B.
;;
;; Both `set-title!` (host-driven, e.g. CLI rename) and the SCI
;; `(set-conversation-title! "...")` fn (model-driven) funnel through
;; `set-title-with-broadcast!`, which is the single mutation point.
;; That keeps the in-memory env atom + DB column + listener fan-out
;; in lockstep - no path can update one without the others.
;; =============================================================================

(defonce ^:private title-listeners
  ;; {conversation-id-uuid #{listener-fn ...}}
  (atom {}))

(defn add-title-listener!
  "Register `listener-fn` for `conversation-id`. The fn is invoked with
   the new title (a string) every time the title changes. Multiple
   listeners are supported; they fire in unspecified order.

   Returns the listener fn so callers can pass it to
   `remove-title-listener!` later."
  [conversation-id listener-fn]
  (let [cid (persistance/->uuid conversation-id)]
    (swap! title-listeners update cid (fnil conj #{}) listener-fn))
  listener-fn)

(defn remove-title-listener!
  "Deregister a previously added listener. Idempotent."
  [conversation-id listener-fn]
  (let [cid (persistance/->uuid conversation-id)]
    (swap! title-listeners update cid
      (fn [existing] (disj (or existing #{}) listener-fn))))
  nil)

(defn- broadcast-title-change!
  "Fire every registered listener for `conversation-id` with `title`.
   Listeners that throw are swallowed and logged - a misbehaving
   channel must NOT block the iteration loop."
  [conversation-id title]
  (let [cid (persistance/->uuid conversation-id)]
    (doseq [f (get @title-listeners cid)]
      (try (f title)
        (catch Throwable t
          (tel/log! {:level :warn :id ::title-listener-failed
                     :data {:conversation-id cid
                            :error (ex-message t)}
                     :msg (str "Title listener threw: " (ex-message t))}))))))

(defn set-title-with-broadcast!
  "Single mutation point for conversation titles.

   1. Writes the title to the persisted `conversation_state` row.
   2. Updates the env's in-memory `:conversation-title-atom` so the next iteration's
      `CONVERSATION_TITLE` SYSTEM var rebind sees the new value AND so a
      read from the SCI sandbox returns the fresh string immediately,
      without a DB round-trip.
   3. Broadcasts to every registered listener.

   `conversation-title-atom` may be nil (host-driven path with no live env)."
  [db-info conversation-id conversation-title-atom title]
  (let [t (str title)]
    (persistance/db-update-conversation-title! db-info conversation-id t)
    (when conversation-title-atom (reset! conversation-title-atom t))
    (broadcast-title-change! conversation-id t)
    nil))

(def ^:private auto-title-max-words 6)
(def ^:private auto-title-max-chars 80)

(defn- clean-auto-title
  "Trim, drop wrapping quotes/backticks, collapse whitespace, drop
   trailing punctuation, and clamp word count. Returns nil if the
   result is unusable."
  [s]
  (when s
    (let [t (-> (str s)
              (str/replace #"^[\s\"'`]+|[\s\"'`]+$" "")
              (str/replace #"\s+" " ")
              (str/replace #"[.。\!\?]+$" ""))
          words (str/split t #"\s+")
          clamped (str/join " " (take auto-title-max-words words))]
      (when (and (not (str/blank? clamped))
              (<= (count clamped) auto-title-max-chars))
        clamped))))

(defn- auto-title!
  "Generate a 6-words-max title for `conversation-id` from the user
   request, persist it, and broadcast. Synchronous - caller picks the
   thread (we wrap it in a `future` from `turn!` so the answer path
   is never blocked).

   Skipped silently when:
     - the conversation already has a non-blank title
     - the user request is blank
     - the LLM call fails / returns blank
     - the post-cleaned candidate is unusable

   Goes through `svar/ask-code!` with `:lang \"text\"` (no JSON spec
   anywhere in Vis)."
  [{:keys [router db-info conversation-id conversation-title-atom user-request resolved-model]}]
  (when (and router db-info conversation-id
          (string? user-request)
          (not (str/blank? user-request)))
    (let [conv  (try (persistance/db-get-conversation db-info conversation-id)
                  (catch Throwable _ nil))
          cur   (some-> conv :title str)]
      (when (str/blank? cur)
        (try
          (let [prompt (str "Pick a short conversation title for this user request - at most "
                         auto-title-max-words " words, plain text only, no quotes, no period.\n\n"
                         "User request:\n" user-request "\n\n"
                         "Reply with ONE fenced ```text block containing only the title.")
                llm-headers (copilot-llm-headers resolved-model "agent")
                resp (binding [svar-llm/*log-context* (assoc svar-llm/*log-context*
                                                        :conversation-id conversation-id
                                                        :internal-call :auto-title)]
                       (svar/ask-code! router
                         (cond-> {:messages           [(svar/user prompt)]
                                  :lang               "text"
                                  :reasoning          :off
                                  :code-tail-pointer? true}
                           llm-headers (assoc :llm-headers llm-headers))))
                raw  (or (some-> resp :result str/trim not-empty)
                       (some-> resp :raw str/trim not-empty))
                title (clean-auto-title raw)]
            (when title
              (set-title-with-broadcast! db-info conversation-id
                conversation-title-atom title)))
          (catch Throwable t
            (tel/log! {:level :debug :id ::auto-title-failed
                       :data  {:conversation-id conversation-id
                               :error           (ex-message t)}
                       :msg   "Auto-title generation failed (silently skipped)"})))))))

(defn- spawn-auto-title!
  "Fire-and-forget wrapper around `auto-title!`. Runs on a JVM future
   so the iteration loop returns immediately to the channel; the
   eventual `set-title-with-broadcast!` call wakes title listeners
   (TUI header, Telegram label) on its own."
  [args]
  (when args
    (cancellation/worker-future "vis-auto-title"
      #(try (auto-title! args)
         (catch Throwable _))))
  nil)

;; -----------------------------------------------------------------------------
;; Finalize turn result
;; -----------------------------------------------------------------------------

(defn- finalize-turn-result
  "Updates DB turn record, builds result map.

   `:provider` and `:model` are both attached to the persisted cost
   map so the web footer / meta layer can render `provider/model / N
   iteration / duration / tokens / $total` after a restart."
  [{:keys [db-info root-model root-provider]}
   {:keys [conversation-turn-id start-time iteration-count status status-id trace locals
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
        (log-stage! :turn/stop 0
          {:duration-ms duration-ms :iteration-count iteration-count :status status})
        (let [fallback-answer (:result answer answer)]
          (try
            (persistance/db-update-conversation-turn! db-info conversation-turn-id
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
        (log-stage! :turn/stop 0
          {:duration-ms duration-ms :iteration-count iteration-count
           :cost (str (:total-cost cost-with-model))})
        (try
          (persistance/db-update-conversation-turn! db-info conversation-turn-id
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
  "Runs one conversation turn on an RLM environment using iterative LLM code evaluation.

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
           :blocks [{:id 0 :code <code-str> :result <value> :stdout <str> :error nil :execution-time-ms 5}
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
         {:keys [eval-timeout-ms concurrency
                 debug? user-request root-resolved-model root-model
                 db-info
                 environment-id]} ctx
         merged-concurrency (merge DEFAULT_CONCURRENCY concurrency)]
     (binding [*rlm-context*       {:rlm-environment-id environment-id :rlm-type :main
                                    :rlm-debug? debug? :rlm-phase :turn
                                    :db-info db-info
                                    :conversation-soul-id (:conversation-id environment)}
               *eval-timeout-ms*  (clamp-eval-timeout-ms
                                    (or eval-timeout-ms *eval-timeout-ms*))
               *concurrency*      merged-concurrency]
       (tel/with-ctx+ {:db-info db-info
                       :conversation-soul-id (:conversation-id environment)}
         (log-stage! :turn/start 0
           {:model root-model
            :reasoning? (boolean (:reasoning? (first (mapcat :models (:providers (:router environment))))))
            :user-request user-request})
         (let [start-time   (System/nanoTime)
               phase2       (run-iteration-phase ctx)
               {:keys [iteration-result conversation-turn-id
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
                   {:conversation-turn-id          conversation-turn-id
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
                   {:conversation-turn-id          conversation-turn-id
                    :start-time        start-time
                    :iteration-count   iteration-count
                    :trace             trace
                    :answer            iteration-answer
                    :confidence        confidence
                    :reasoning         reasoning
                    :total-tokens-atom total-tokens-atom
                    :total-cost-atom   total-cost-atom}))]
           ;; Auto-title hook: fire-and-forget on a successful turn
           ;; when the conversation has no title yet. Background
           ;; future -> answer return is never blocked. Failure is
           ;; logged at :debug and silently dropped.
           (when-not status
             (spawn-auto-title!
               {:router                  (:router environment)
                :db-info                 db-info
                :conversation-id         (:conversation-id environment)
                :conversation-title-atom (:conversation-title-atom environment)
                :user-request            user-request
                :resolved-model          root-resolved-model}))
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

(defn- normalize-history-symbol
  [sym]
  (cond
    (symbol? sym) sym
    (string? sym) (clojure.core/symbol sym)
    :else         (clojure.core/symbol (str sym))))

(defn- live-user-symbols
  [{:keys [sci-ctx initial-ns-keys]}]
  (let [sandbox-map (get-in @(:env sci-ctx) [:namespaces 'sandbox])]
    (into #{}
      (filter (fn [sym]
                (and (symbol? sym)
                  (not (contains? initial-ns-keys sym))
                  (not (env/system-var-sym? sym)))))
      (keys sandbox-map))))

(defn- with-live-status
  [live-syms entry]
  (assoc entry :status
    (cond
      (contains? live-syms (:name entry)) :live
      (= :unavailable (:status entry))   :unavailable
      :else                              :archived)))

(defn- history-preview
  [v]
  (prompt/safe-pr-str v {:max-chars 1000 :print-length 16 :print-level 4}))

(defn- history-entry-view
  [entry opts]
  (let [base (cond-> (dissoc entry :value :code)
               (:code? opts) (assoc :code (:code entry)))
        include (:include opts)]
    (case include
      :preview (assoc base :preview (history-preview (:value entry)))
      :value   (assoc base :value (:value entry))
      base)))

(defn- var-history-index
  [environment opts]
  (let [db-info (:db-info environment)
        conversation-id (:conversation-id environment)
        live-syms (live-user-symbols environment)]
    (->> (persistance/db-var-history-index db-info conversation-id (or opts {}))
      (mapv #(with-live-status live-syms %)))))

(defn- var-history-for-symbol
  [environment sym opts]
  (let [db-info (:db-info environment)
        conversation-id (:conversation-id environment)
        sym (normalize-history-symbol sym)
        live? (contains? (live-user-symbols environment) sym)]
    (->> (persistance/db-var-history db-info conversation-id sym)
      (mapv (fn [entry]
              (-> entry
                (assoc :name sym
                  :status (if live? :live :archived))
                (history-entry-view (or opts {}))))))))

(defn- var-history-timeline-view
  [environment opts]
  (let [db-info (:db-info environment)
        conversation-id (:conversation-id environment)
        opts (or opts {})
        symbol (:symbol opts)
        events (:events opts)
        timeline (persistance/db-var-history-timeline db-info conversation-id
                   (cond-> opts symbol (assoc :symbol (normalize-history-symbol symbol))))]
    (cond->> timeline
      (seq events) (filter #(contains? (set events) (:event %)))
      true vec)))

(defn- extension-aliases
  [exts]
  (->> (or exts [])
    (keep :ext/alias)
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
                         :ext (:ext/namespace ext)
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
              entry-by-sym (into {} (map (juxt :ext.symbol/symbol identity)) (:ext/symbols ext))
              ns-bindings (into {}
                            (map (fn [[sym val]]
                                   [sym (sci-binding-var ext-ns sym val
                                          (get entry-by-sym sym))]))
                            wrapped)
              collisions (vec (filter #(contains? bindings %) (keys ns-bindings)))]
          (when (seq collisions)
            (tel/log! {:level :warn :id ::ext-symbol-collision
                       :data  {:ext       (:ext/namespace ext)
                               :ns        ns-sym
                               :alias     alias-sym
                               :symbols   collisions
                               :previous  (select-keys owners collisions)}
                       :msg   (str "Extension '" (:ext/namespace ext)
                                "' shadowed " (count collisions)
                                " active symbol(s) under alias '" alias-sym
                                "': " (str/join ", " collisions))}))
          (recur (next remaining)
            (merge bindings ns-bindings)
            (merge owners (zipmap (keys ns-bindings)
                            (repeat (:ext/namespace ext))))))
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
           active-set (set (map :ext/namespace active-extensions))]
       (doseq [{ns-sym :ns alias-sym :alias :as alias} (extension-aliases installed)]
         (let [active-for-alias (filterv (fn [ext]
                                           (and (contains? active-set (:ext/namespace ext))
                                             (= alias (:ext/alias ext))))
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

   If an extension with the same `:ext/namespace` is already registered,
   it is replaced (not duplicated). Enables hot-swap via
   `reload-extension!`.

   Returns `environment` for chaining."
  [environment ext]
  (when-not (:extensions environment)
    (anomaly/incorrect! "Invalid vis environment - missing :extensions atom"
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
  ;; Extension rows stay installed even when inactive, but callable symbol
  ;; bindings are activation-aware. Java classes/imports remain available once
  ;; an extension is installed because they are passive SCI configuration, not
  ;; model-visible tool affordances.
  (let [sci-ctx (:sci-ctx environment)]
    (when-let [classes (seq (:ext/classes ext))]
      (swap! (:env sci-ctx) update :classes merge (into {} classes)))
    (when-let [imports (seq (:ext/imports ext))]
      (swap! (:env sci-ctx) update :imports merge (into {} imports))))
  (sync-active-extension-symbols! environment)
  environment)

;; =============================================================================
;; Environment Lifecycle
;; =============================================================================

(defn create-environment
  "Creates a vis environment (component) for conversation lifecycle and
   querying.

   The environment holds:
     - SCI sandbox context with custom bindings + bindings cache
     - DB connection (or shared-mem datasource)
     - Router (LLM provider config)
     - Extension registry atom

   Params:
     `router` - Required. Result of `llm/make-router`.
     `opts`   - Map with `:db` and optional `:conversation`,
                 `:channel`, `:external-id`, `:title`.

     `:db` accepted forms:
       nil               - no DB (SCI-only execution)
       :memory           - ephemeral in-process SQLite DB
       path string       - persistent SQLite DB at path
       {:path p}         - persistent SQLite DB at path
       {:datasource ds}  - caller-owned DataSource (not closed on dispose)

   Returns the vis environment map."
  [router {:keys [db conversation channel external-id title]}]
  (when-not router
    (anomaly/incorrect! "Missing router" {:type :vis/missing-router}))
  (let [depth-atom               (atom 0)
        db-info                  (persistance/db-create-connection! db)
        bindings-atom           (atom {:index nil :revision -1 :current-revision 0})
        active-skills-atom       (atom {})
        state-atom               (atom {:custom-bindings {}
                                        :environment     nil
                                        :conversation-id nil})
        environment-atom         (atom nil)
        environment-id           (str (util/uuid))
        ;; Iteration-final-answer signal. The SCI sandbox's `(turn-answer!
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
        current-conversation-turn-id-atom (atom nil)
        current-turn-position-atom (atom nil)
        current-user-request-atom (atom nil)
        ;; Title atom: in-memory cache for the conversation title.
        ;; The DB column on `conversation_state` is the persisted
        ;; truth; this atom is the fast read path for  and
        ;; the source for the `CONVERSATION_TITLE` SYSTEM var rebind at iteration
        ;; boundaries. `set-title!` writes both, in that order, then
        ;; broadcasts to every registered listener.
        conversation-title-atom               (atom (or title ""))
        root-resolved-model      (resolve-effective-model router)
        root-model               (or (:name root-resolved-model) "unknown")
        root-provider            (:provider root-resolved-model)
        ;; Snapshot a base system prompt for the conversation row so the
        ;; sidebar / DB inspectors have something stable to display.
        ;; Real per-turn assembly goes through `prompt/assemble-stable-prompt-messages`
        ;; with `:active-extensions`, so this snapshot is just metadata.
        system-prompt            (prompt/build-system-prompt {})
        resolved-conversation-id (persistance/db-resolve-conversation-id db-info conversation)
        conversation-id          (or resolved-conversation-id
                                   (persistance/db-store-conversation! db-info
                                     (cond-> {:channel       (or channel :tui)
                                              :external-id   external-id
                                              :model         root-model
                                              :title         title
                                              :system-prompt system-prompt}
                                       root-provider (assoc :provider root-provider))))
        ;; Bind sandbox helpers that need env identity (db-info +
        ;; conversation-id). They go through `custom-bindings` so they
        ;; land in `initial-ns-keys` and therefore stay out of
        ;; `<bindings>` (matches the treatment of every other system
        ;; binding shipped via EXTRA_BINDINGS).
        var-history-fn           (fn var-history
                                   ([]
                                    (var-history-index @environment-atom {}))
                                   ([sym-or-opts]
                                    (if (map? sym-or-opts)
                                      (var-history-index @environment-atom sym-or-opts)
                                      (var-history-for-symbol @environment-atom sym-or-opts {})))
                                   ([sym opts]
                                    (var-history-for-symbol @environment-atom sym opts)))
        var-history-timeline-fn  (fn var-history-timeline
                                   ([]
                                    (var-history-timeline-view @environment-atom {}))
                                   ([opts]
                                    (var-history-timeline-view @environment-atom opts)))
        ;; SCI binding for `(turn-answer! "...")` - the canonical turn-
        ;; termination call. Closes over `answer-atom` AND
        ;; `current-form-idx-atom` so the iteration loop can scope
        ;; the discard check to the form that actually called this.
        ;; Returns the marker keyword so the per-form result row makes
        ;; request visible.
        answer-fn                (fn turn-answer! [s]
                                   ;; Canonicalize the answer value to
                                   ;; `[:ir & nodes]` AT THE ENTRY
                                   ;; POINT. From here on, every
                                   ;; downstream consumer (DB persist,
                                   ;; channel renderers, voice TTS,
                                   ;; logs) sees one shape and one
                                   ;; shape only.
                                   ;;
                                   ;; `render/->ast` is total and pure:
                                   ;;   string         -> [:ir {} "..."]
                                   ;;   [:ir ...]      -> normalized in place
                                   ;;   [:tag ...]     -> wrapped in :ir
                                   ;;   seq of mixed   -> wrapped + per-child coerced
                                   ;;   anything else  -> [:ir {} [:code {:lang "edn"} (pr-str x)]]
                                   ;;
                                   ;; The previous `(str s)` invoked
                                   ;; Java `.toString` on Hiccup/IR
                                   ;; vectors, producing a literal
                                   ;; `"[:ir [:p ...]]"` string that no
                                   ;; downstream renderer could undo.
                                   ;; EXCEPTION: needs-input payloads
                                   ;; are data, not prose. The
                                   ;; prompt-flow gate reads them as
                                   ;; maps via `needs-input-answer?` /
                                   ;; `:answer/text`. Coercing those to
                                   ;; IR would dump them as an EDN code
                                   ;; block. Pass the map through
                                   ;; untouched - `answer-str` already
                                   ;; special-cases the shape.
                                   (reset! answer-atom
                                     {:value    (if (needs-input-answer? s)
                                                  s
                                                  (render/->ast s))
                                      :form-idx @current-form-idx-atom})
                                   :vis/answer)
        ;; SCI binding for the conversation title:
        ;;   `(set-conversation-title! \"...\")` - writes the title through
        ;;                              to DB, syncs the in-memory
        ;;                              atom, and broadcasts
        ;;                              `:title-changed` to every
        ;;                              registered listener so
        ;;                              channels (e.g. the TUI
        ;;                              header) can refresh without
        ;;                              polling.
        ;; ONE-ARITY ONLY. To READ the current title from `:code`,
        ;; reference the `CONVERSATION_TITLE` SYSTEM var - there is
        ;; no zero-arg reader, by design: a `(set-conversation-title!)`
        ;; call would invite the model to round-trip what it can
        ;; read for free. Calling with the wrong arity raises an
        ;; `ArityException` from SCI like any other Clojure fn.
        ;; Returns `:vis/silent`: the title is visible in channel chrome
        ;; and the model journal, but the host call itself is noise in
        ;; live progress / iteration rendering.
        conversation-title-fn    (fn set-conversation-title! [s]
                                   (let [s (str s)]
                                     (set-title-with-broadcast!
                                       db-info conversation-id
                                       conversation-title-atom s)
                                     :vis/silent))
        ;; The retired `TURN_USER_REQUEST` SYSTEM var has no replacement
        ;; binding by design - the same data already flows through
        ;; `(v/conversation-state)` -> :current-turn :user-request (and through
        ;; :transcript :turns for cross-turn history), so a separate
        ;; sandbox primitive would just duplicate the surface.
        env-bindings             (merge {'var-history          var-history-fn
                                         'var-history-timeline var-history-timeline-fn
                                         'turn-answer!         answer-fn
                                         'set-conversation-title! conversation-title-fn}
                                   (skills/sandbox-bindings active-skills-atom))
        {:keys [sci-ctx sandbox-ns initial-ns-keys]}
        (env/create-sci-context (merge env-bindings
                                  (:custom-bindings @state-atom)))
        env {:environment-id  environment-id
             :conversation-id conversation-id
             :channel         (or channel :tui)
             :depth-atom      depth-atom
             :db-info         db-info
             :bindings-atom  bindings-atom
             :active-skills-atom active-skills-atom
             :state-atom      state-atom
             :sci-ctx         sci-ctx
             :sandbox-ns      sandbox-ns
             :initial-ns-keys initial-ns-keys
             :router          router
             :answer-atom           answer-atom
             :current-form-idx-atom current-form-idx-atom
             :current-iteration-atom current-iteration-atom
             :current-iteration-id-atom current-iteration-id-atom
             :current-conversation-turn-id-atom current-conversation-turn-id-atom
             :current-turn-position-atom current-turn-position-atom
             :current-user-request-atom current-user-request-atom
             :conversation-title-atom            conversation-title-atom
             :extensions            (atom [])}]
    (reset! environment-atom env)
    (swap! state-atom assoc :environment env :conversation-id conversation-id)
    ;; Restore persisted vars when resuming an existing conversation.
    (when resolved-conversation-id
      (try
        (env/restore-sandbox! sci-ctx db-info conversation-id)
        (env/bump-bindings! env)
        (catch Throwable t
          (tel/log! {:level :warn :id ::restore-sandbox-failed
                     :data {:error (ex-message t)
                            :conversation-id conversation-id}
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
;; Conversation env cache
;; =============================================================================

;; ---------------------------------------------------------------------------
;; In-process conversation cache + channel utilities
;; ---------------------------------------------------------------------------

(defonce cache (atom {}))

(defn cache-env! [id env]
  (swap! cache assoc id {:environment env
                         :lock (java.util.concurrent.locks.ReentrantLock.)})
  {:id id :environment env})

(defn refresh-cached-routers!
  "Reseat `:router` on every cached env's environment map.

  `create-environment` snapshots the router into
  `(:router env)` at construction time, and the iteration loop calls
  `(svar/ask! (:router environment) ...)` - not the global
  `router-atom`. So when a frontend changes provider
  config and rebuilds the global router, every long-lived env in the
  cache (TUI keeps one for the whole session) keeps talking to the
  *previous* model until disposed.

  Call this immediately after `rebuild-router!` so the
  next `send!` on any cached conversation picks up the new router."
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
   conversation envs. `provider` is a svar-native provider map
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

(defn remove-provider!
  "Remove a provider by `:id`. Persists to disk and reseats cached envs."
  [provider-id]
  (let [cfg     (or (config/current-config) {:providers []})
        updated (vec (remove #(= (:id %) provider-id) (:providers cfg)))
        new-cfg {:providers updated}]
    (config/save-config! new-cfg)
    (reset! @#'config/active-config new-cfg)
    new-cfg))

;; ---------------------------------------------------------------------------
;; Extension hot-reload (F1-lite). See plan §1 Q12-Q16 + caveats.
;;
;; Surgical for `:added` / `:removed` (full side-effect cleanup,
;; symbol install). Treats every still-present extension as
;; `:reloaded` (no change-detection in v1; v2 uses persisted source
;; markers). Continue-on-error: per-ext failures land in :errors,
;; orchestration continues. The CALLING env's reseat is deferred
;; (would race with the SCI sandbox actively executing the call);
;; OTHER envs use a per-env lock-acquisition timeout.
;; ---------------------------------------------------------------------------

(def ^:const RELOAD_DEFAULT_TIMEOUT_MS
  "Default per-env lock-acquisition timeout for `reload-extensions!`.
   Envs that don't free their lock within this window are recorded
   as `:env-reseat-skipped` and rebind on next access."
  30000)

(defn- now-ms ^long [] (System/currentTimeMillis))

(defn- diff-extensions
  "Compute the F1-lite diff between the current in-memory
   `extension-registry` and a freshly-scanned `manifests` map
   (manifest-id -> entry, where entry has `:nses`).

   Returns `{:added [#{ns-syms...}] :removed [#{ns-syms...}]
             :reloaded [#{ns-syms...}]}` - each value is a vec of
   `:ext/namespace` symbols.

   `:added`: nses in some manifest's `:nses` but no extension
            currently registered for that ns.
   `:removed`: registered ext namespaces no longer covered by any
            manifest.
   `:reloaded`: ext namespaces present in BOTH - every still-here
            ext re-loaded under F1-lite (no change detection v1)."
  [registered manifests]
  (let [registered-ns (set (map :ext/namespace registered))
        manifest-ns   (set (mapcat :nses (vals manifests)))
        added         (vec (sort (set/difference manifest-ns registered-ns)))
        removed       (vec (sort (set/difference registered-ns manifest-ns)))
        reloaded      (vec (sort (set/intersection registered-ns manifest-ns)))]
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

(defn- reseat-cached-env!
  "Per-env reseat orchestration. Returns one of:
     :reseated
     :deferred (calling env, locked by current thread)
     {:reason :busy :waited-ms N}
   The current implementation focuses on lifecycle correctness -
   v1 doesn't surgically swap individual extensions' bindings
   in-place because `register-extensions!` (which `create-environment`
   uses) needs an SCI register-fn that we don't have post-hoc.
   Instead, the env stays alive but its sandbox bindings are
   considered stale until next access - the next `send!` re-runs the
   prompt assembler, which sees the freshly-registered extensions.
   This is honest: we tell the caller `:reseated` to mean we
   acquired the lock and confirmed nothing's mid-iteration; the
   actual binding refresh happens lazily."
  [^java.util.concurrent.locks.ReentrantLock lock timeout-ms]
  (cond
    (.isHeldByCurrentThread lock)
    :deferred

    (.tryLock lock
      (long timeout-ms)
      java.util.concurrent.TimeUnit/MILLISECONDS)
    (try :reseated
      (finally (.unlock lock)))

    :else
    {:reason :busy :waited-ms (long timeout-ms)}))

(defn reload-extensions!
  "Re-discover extensions on the classpath, diff against the in-memory
   registry, apply the diff (require/register for `:added`,
   deregister + side-effect cleanup for `:removed`, `(require :reload)`
   + re-register for everything else), and reseat every cached env.

   F1-lite: no change-detection in v1; every still-present extension
   is reloaded. Plan Q14.

   Continue-on-error: per-ext failures accumulate in `:errors` and
   orchestration continues. Plan Q16.

   Calling env defers reseat (mid-turn reload would race with the
   SCI sandbox actively executing the call); other envs are reseated
   under a `:reload/timeout-ms` (default 30s) per-env lock acquisition.
   Plan caveat: reload deadlock prevention.

   Returns:
     {:added [...]              ;; ns-syms newly registered
      :removed [...]             ;; ns-syms deregistered
      :reloaded [...]            ;; ns-syms re-required + re-registered
      :errors [{:ns :phase :reason :stack-trace} ...]
      :envs-reseated 3
      :env-reseat-deferred [#uuid \"...\"]
      :env-reseat-skipped  [{:env-id ... :reason :busy :waited-ms N}]
      :duration-ms 847
      :blocked-ms  127}"
  ([] (reload-extensions! {}))
  ([{:keys [reload/timeout-ms]
     :or   {timeout-ms RELOAD_DEFAULT_TIMEOUT_MS}}]
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
         ;; Step 3c: :reloaded - same as :added but with :reload.
         _             (doseq [ns-sym reloaded]
                         (when-let [t (require-ns! ns-sym true)]
                           (record-error errors ns-sym :require t)))
         ;; Step 4: reseat cached envs.
         lock-wait-start (now-ms)
         envs-snapshot   (vec @cache)
         reseated        (atom [])
         deferred        (atom [])
         skipped         (atom [])]
     (doseq [[id {:keys [^java.util.concurrent.locks.ReentrantLock lock]}] envs-snapshot]
       (try
         (let [outcome (reseat-cached-env! lock timeout-ms)]
           (cond
             (= :reseated outcome) (swap! reseated conj id)
             (= :deferred outcome) (swap! deferred conj id)
             (map? outcome)        (swap! skipped conj (assoc outcome :env-id id))))
         (catch Throwable t
           (record-error errors id :env-reseat t))))
     (let [blocked-ms (- (now-ms) lock-wait-start)
           duration   (- (now-ms) start)]
       {:added                added
        :removed              removed
        :reloaded             reloaded
        :errors               @errors
        :envs-reseated        (count @reseated)
        :env-reseat-deferred  @deferred
        :env-reseat-skipped   @skipped
        :duration-ms          duration
        :blocked-ms           blocked-ms}))))

(defn- open-env!
  [id {:keys [channel external-id title]}]
  (let [router (get-router)
        env    (create-environment router
                 (cond-> {:db (config/resolve-db-spec)}
                   id          (assoc :conversation id)
                   channel     (assoc :channel channel)
                   external-id (assoc :external-id external-id)
                   title       (assoc :title title)))]
    env))

(defn- ensure-env!
  [id]
  (if-let [entry (get @cache id)]
    entry
    (let [env (open-env! id {})]
      (swap! cache
        (fn [m]
          (if (contains? m id)
            m
            (assoc m id {:environment env
                         :lock (java.util.concurrent.locks.ReentrantLock.)}))))
      (get @cache id))))

(defn db-info
  "Return the process-wide shared DB connection bound to
   `(config/resolve-db-spec)`. Thin wrapper over
   `persistance.core/db-shared-connection!` that fills in the default db-spec
   so frontend callers stay clear of config resolution."
  []
  (persistance/db-shared-connection! (config/resolve-db-spec)))

(defn create!
  ([channel] (create! channel nil))
  ([channel {:keys [title external-id]}]
   (let [env  (open-env! nil {:channel     channel
                              :external-id (some-> external-id str)
                              :title       title})
         id   (str (:conversation-id env))
         _    (cache-env! id env)]
     {:id          id
      :channel     channel
      :external-id (some-> external-id str)
      :title       title})))

(defn by-id
  [id]
  (when-let [conversation (persistance/db-get-conversation (db-info) id)]
    {:id            (str (:id conversation))
     :channel       (:channel conversation)
     :external-id   (:external-id conversation)
     :system-prompt (:system-prompt conversation)
     :model         (:model conversation)
     :title         (:title conversation)
     :created-at    (:created-at conversation)}))

(defn by-channel
  [channel]
  (mapv (fn [c]
          {:id          (str (:id c))
           :channel     (:channel c)
           :external-id (:external-id c)
           :title       (:title c)
           :created-at  (:created-at c)})
    (persistance/db-list-conversations (db-info) channel)))

(defn for-telegram-chat!
  [chat-id]
  (let [ext (str chat-id)]
    (or (when-let [id (persistance/db-find-conversation-by-external (db-info) :telegram ext)]
          (by-id (str id)))
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
   plain DB write when no env is live for this conversation (e.g.
   `vis conversations` rename ops)."
  [id title]
  (let [env (env-for id)]
    (set-title-with-broadcast! (or (:db-info env) (db-info))
      id
      (:conversation-title-atom env)
      title))
  nil)

(defn send!
  ([id messages] (send! id messages {}))
  ([id messages opts]
   (let [{:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}
         (ensure-env! id)
         message-vec (if (string? messages) [(svar/user messages)] messages)]
     ;; ReentrantLock (not Object monitor) so `reload-extensions!`
     ;; can use `.isHeldByCurrentThread` to detect mid-iteration
     ;; reload calls and `.tryLock(timeout)` to bound waits on
     ;; OTHER busy envs. Plan caveat: reload deadlock prevention.
     (.lock lock)
     (try (turn! environment message-vec opts)
       (finally (.unlock lock))))))

(defn close!
  [id]
  (when-let [{:keys [environment]} (clojure.core/get @cache id)]
    (try (dispose-environment! environment) (catch Exception _ nil)))
  (swap! cache dissoc id))

(defn delete!
  [id]
  (close! id)
  (let [d (db-info)]
    (try (persistance/db-delete-conversation-tree! d id)
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
   (let [orphans (try (persistance/db-list-conversation-turns-by-status db :running)
                   (catch Exception _ []))]
     (doseq [{:keys [id iteration-count duration-ms]} orphans]
       (try
         (persistance/db-update-conversation-turn! db id
           {:answer          ORPHAN_INTERRUPTED_ANSWER
            :iteration-count (or iteration-count 0)
            :duration-ms     (or duration-ms 0)
            :status          :interrupted
            :prior-outcome   :cancelled})
         (catch Exception _ nil)))
     (count orphans))))

(defn close-all!
  []
  (doseq [[_ {:keys [environment]}] @cache]
    (try (dispose-environment! environment) (catch Exception _ nil)))
  (reset! cache {})
  (persistance/db-dispose-shared-connection!))
