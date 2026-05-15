(ns com.blockether.vis.internal.loop
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
   [com.blockether.vis.internal.env.sci-patches :as sci-patches]
   [com.blockether.vis.internal.render :as render]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.prompt :as prompt]
   [edamame.core :as edamame]
   [sci.core :as sci]
   [taoensso.telemere :as tel])
  (:import
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
  "Default model/progress timeout for Vis `svar/ask-code!` streams.
   Nil by default: upstream OpenAI/Z.ai/Anthropic streams generally do
   not send provider-level heartbeats, so Vis relies on idle timeout.
   Set per call with `:semantic-timeout-ms` when a proxy emits `: ping`
   heartbeats and transport liveness is not enough."
  nil)

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
   fires for symbols inside an init body, so the only stable point at
   which we can recover dep relationships is the source itself.

   The output is intentionally permissive: it includes refs to core
   ops (`inc`, `*`, `+`), to locals introduced by `let` / `fn`
   parameter lists, and to macro symbols. The persistence layer
   filters every edge against the existing-soul-name set inside its
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
        ;; Per-iteration sinks for the pivot patches. `*def-sink-atom*`
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
        exec-future (cancellation/worker-future "vis-sci-eval"
                      (fn []
                        (try
                          (let [result (binding [extension/*tool-event-sink* record-tool-event
                                                 extension/*journal-render-sink* journal-sink
                                                 extension/*channel-render-sink* channel-sink
                                                 extension/*sink-position*       sink-pos
                                                 sci-patches/*def-sink-atom*     def-sink
                                                 sci-patches/*lru-atom*          lru
                                                 sci-patches/*current-turn-position* turn-position]
                                         (sci/binding [sci/out stdout-writer
                                                       sci/err err-pw]
                                           (let [ns (or (sci/find-ns sci-ctx 'sandbox) sandbox-ns)]
                                             (:val (sci/eval-string+ sci-ctx code
                                                     (when ns {:ns ns}))))))]
                            {:result result :stdout (str stdout-writer) :stderr (str stderr-writer)
                             :journal     @journal-sink
                             :channel     @channel-sink
                             :def-sink    @def-sink
                             :lru         @lru
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
                             :def-sink @def-sink
                             :lru     @lru
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
                              :def-sink @def-sink
                              :lru     @lru
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
             :execution-time-ms execution-time)
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
   `(set-conversation-title! ...)`) MUST run their bodies on every
   invocation, and forms without side effects re-run cheaply enough
   that caching them is not worth the correctness footgun."
  [{:keys [sci-ctx sandbox-ns] :as environment} code
   & {:keys [timeout-ms tool-event-fn]}]
  (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :execute-code})]
    ;; Per-block-eval contract: feed the block source to SCI verbatim.
    ;; SCI parses + evaluates as one chunk; parse / runtime errors surface
    ;; as the block's structured `:error` map. No pre-eval lint, no
    ;; pre-parse check, no answer-string restitch — the model self-corrects
    ;; from SCI's error next iteration.
    (let [start-time (System/currentTimeMillis)
          exec       (try
                       (sci-patches/validate-non-empty-block! code)
                       (sci-patches/validate-no-banned-defs! code)
                       (run-with-timing sci-ctx code sandbox-ns timeout-ms
                         start-time tool-event-fn environment)
                       (catch Throwable e
                         (env/push-eval-error! environment e)
                         {:result nil
                          :stdout ""
                          :stderr ""
                          :journal []
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
                          :execution-time-ms (- (System/currentTimeMillis) start-time)
                          :timeout? false}))]
      exec)))

;; Print-cap defaults for `fmt/safe-pr-str` - chosen so a wide flat
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

;; Pivot: `extension-call-form?` / `form-contains-extension-call?` /
;; `call-symbol->op-keyword` lived here to feed the legacy structural
;; floor ("no extension call in the same iteration as (done …)"). The
;; floor was removed with the pivot — mixing tool calls and `(done …)`
;; in one `(do …)` is the canonical post-pivot shape, so the three
;; helpers had no live callers and went with it.

;; Pivot: with one form per iteration, the legacy three-fn gate
;; complex (`answer-with-extension-preflight-mismatch`,
;; `block-result-error-summary`, `answer-with-extension-preflight-error-message`)
;; is no longer needed. Tool calls and `(done …)` legitimately co-exist
;; in a single form via handles — `(do (def h (v/cat …)) (done [:ir [:p …]]))`
;; is the canonical one-iteration shape. If the form throws, `(done …)`
;; never runs; if it succeeds, the answer is observed inside the same
;; eval. No separate "observed tool evidence first" check applies.

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
;; per-block-eval pivot routes prose into SCI as a parse / unresolved-symbol
;; error instead of detecting "every entry is a bare symbol" upfront.

;; Removed during the per-block-eval pivot:
;;   - `plain-prose-code-error` + `prose->comment` — the splitter no longer
;;     produces multi-symbol entry vectors that a prose response could
;;     accidentally satisfy. With one block = one SCI eval, prose lands in
;;     SCI as a parse error or unresolved-symbol error and the model
;;     self-corrects from the structured error.
;;   - `duplicate-fenced-blocks?` + `dedupe-fenced-block-code` +
;;     `executable-block-source`/-`sources` — dedup now happens inline
;;     inside `code-entries-preflight` on the block vector directly.

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
;; Repair pass: at the line level, drop every <journal>/<bindings>
;; /<current_turn_context>/<iteration_hint[s]> opener through its closer. Closer matches `</tag>` (proper),
;; a bare ``` line (LLM fumble), or another opener (implicit close), or EOF.
;; A ```lang line implicitly closes the envelope without itself being dropped
;; so a real fence directly after a fabricated envelope still parses cleanly.
;; ---------------------------------------------------------------------------

(def ^:private vis-engine-xml-echo-tags
  ;; Vis -> LLM ONLY. The model must never emit these. See
  ;; `com.blockether.vis.internal.prompt` for the renderers that own them.
  #{"journal" "bindings" "current_turn_context"
    "iteration_hints" "iteration_hint"
    ;; Backward-compatible echo cleanup for older transcripts / model habits.
    "current_engine_start_nudges" "current_engine_start_nudge"
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
   (`<journal>`, `<bindings>`, ...), strip them and re-extract Markdown code
   blocks. Replaces `:blocks` on the ask-result; leaves `:raw` ORIGINAL so
   forensic logs / DB rows show what the model actually emitted.

   Pass-through when no envelope appears or stripping yields nothing usable."
  [ask-result]
  (let [raw (or (:raw ask-result) "")]
    (if-not (contains-vis-engine-xml-echo? raw)
      ask-result
      (let [cleaned (strip-vis-engine-xml-echo raw)]
        (if (or (str/blank? cleaned) (= cleaned raw))
          ask-result
          (let [blocks   (svar-codes/extract-code-blocks cleaned)
                selected (svar-codes/select-blocks blocks "clojure")]
            (if (empty? selected)
              ask-result
              (assoc ask-result
                :blocks selected
                :vis/normalized-from-raw? true))))))))

(defn- code-entries-preflight
  "Per-block-eval preflight. One svar Markdown code block becomes one
   code-entry; the block's `:source` is the entry's `:expr` verbatim.
   SCI parses + evals each entry as a single chunk during execution
   — there is no top-level form splitting at this layer.

   Gates retained:
     - `raw-markdown-fence-leak-error` per block. A nested ``` inside the
       extracted source means svar's normalizer fell into a recursive shape;
       structured rejection beats a JVM crash.
     - Duplicate-block dedup. Some providers stutter and emit the same
       block twice; we keep the first copy and drop the rest.

  Removed in the pivot: the answer-with-extension pre-eval gate. With
  one form per iteration + handle returns, `(do (def h (v/cat …))
  (done …))` IS the canonical answer shape; rejecting it here was a
  false positive that re-introduced multi-iteration probe overhead."
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
        ;;                       `render/code-block-segments`)
        ;;   :vis/structurally-silent?
        ;;                     — true iff the block contains ONLY structural
        ;;                       forms (`(done ...)` / `(set-
        ;;                       conversation-title! ...)`); channels that
        ;;                       don't read segments can drop the whole entry.
        raw-entries                  (mapv (fn [b]
                                             (let [src (:source b)]
                                               (if-let [err (raw-markdown-fence-leak-error src)]
                                                 {:expr "(vis/preflight-error :raw-markdown-fence-leak)"
                                                  :vis/preflight-error err
                                                  :block-lang (:lang b)}
                                                 (let [segments        (render/code-block-segments src)
                                                       structurally-silent?
                                                       (and (seq segments)
                                                         (not-any? #(= :code (:kind %)) segments))]
                                                   {:expr       src
                                                    :block-lang (:lang b)
                                                    :render-segments segments
                                                    :vis/structurally-silent? structurally-silent?}))))
                                       unique-blocks)
        raw-fence-error              (some :vis/preflight-error raw-entries)
        parsed-total-blocks          (count raw-entries)
        empty-code-error             (when (zero? parsed-total-blocks)
                                       "LLM returned no executable Clojure code block. Emit exactly one ```clojure block; put prose inside (done [:ir ...]).")
        multi-block-error            (when (> parsed-total-blocks 1)
                                       (str "Iteration contains " parsed-total-blocks
                                         " Clojure code blocks (separate ```clojure fences); emit exactly one fence per iteration. "
                                         "Inside that one fence, write as many top-level forms as you want — SCI evals them in sequence."))
        ;; Normalized concat of all surviving block sources — the
        ;; identity used for iteration-hash dedup in the journal.
        normalized-code              (->> raw-entries
                                       (remove :vis/preflight-error)
                                       (map (comp str/trim :expr))
                                       (remove str/blank?)
                                       (str/join "\n"))
        code-hash                    (when-not (str/blank? normalized-code)
                                       (sha256-hex normalized-code))]
    {:code-entries                  (cond
                                      empty-code-error
                                      [{:expr ""
                                        :vis/preflight-error empty-code-error}]

                                      multi-block-error
                                      [{:expr normalized-code
                                        :vis/preflight-error multi-block-error}]

                                      :else
                                      raw-entries)
     :empty-code-preflight-error    empty-code-error
     :raw-fence-preflight-error     raw-fence-error
     :duplicate-blocks-normalized?  duplicate-blocks-normalized?
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
  "Dispatch `:turn.answer/validate` extension hooks against the
   candidate `(done …)` answer. Returns nil when every hook accepts,
   otherwise a single string surfaced as the rejected answer form's
   validation error.

   Pivot note: this used to run a structural floor first (no sibling
   errors / no extension calls in this iter / prior-iteration
   evidence required). With one form per iteration:
     #1 own-form-error is enforced upstream by `answer-form-error`
        (if the (done …) form itself threw, the answer is dropped).
     #2 mixing tool + (done …) inside one `(do …)` is the canonical
        one-iteration shape post-pivot — handles return synchronously,
        the answer is composed in the same eval frame.
     #3 evidence-prior gating became a false-positive engine: probe-
        and-answer in a single iteration is the intended flow.
   So the floor is gone. Extensions that need an additional veto
   (e.g. user-facing safety / format gates) still get their
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
  "Provider-agnostic preserved-thinking replay. Returns at most the single
   canonical `:assistant-message` from the immediately previous compatible
   iteration; empty vec when none.

   This is intentionally conservative for Z.ai/GLM. Conversation
   a9389e1d showed that replaying a long run of assistant messages can
   contaminate the next step, amplify token usage, and make GLM believe a
   previous answer is still active. If preserved thinking is useful, the
   only state we trust is the last model step inside the same live user
   turn. Older iterations remain visible through `<journal>`.

   The wire serializer for the active model translates the canonical
   message to its native shape; iteration-loop never branches on provider."
  [journal-iters]
  (if-let [msg (some->> journal-iters
                 reverse
                 (keep #(some-> % second :assistant-message))
                 first)]
    ;; Keep this call so oversized single-step reasoning is still observable
    ;; to future budget instrumentation. We intentionally do not drop the
    ;; immediately previous step by size: single-step continuity beats replay.
    (do (replay-reasoning-chars msg)
      [msg])
    []))

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
  "Keep only iterations whose provider-native thinking may be replayed into
   the next provider call.

   Cross-turn journal seeds explicitly carry
   `:preserved-thinking/replay? false`; those iterations remain visible in
   `<journal>` as durable evidence, but their opaque provider-native thinking
   state is not replayed into a different user turn. Within a live turn,
   freshly-produced iterations opt in by setting the flag to true. Historical
   in-memory test fixtures that omit the flag are treated as replayable for
   backward compatibility."
  [journal-iters target]
  (let [{target-provider :provider target-model :model} target]
    (filterv (fn [[_ {:keys [assistant-message llm-provider llm-model]
                      replay? :preserved-thinking/replay?}]]
               (and (not= false replay?)
                 assistant-message
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
                               (on-chunk {:phase     :reasoning
                                          :iteration iteration-position
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
                                                            :conversation-turn-id (:environment-id environment)
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
          ;; svar/ask-code! returns the per-block vector in `:blocks`
          ;; (single source of truth; the legacy `:result` concatenated
          ;; string was removed in svar v0.5.3). One block → one
          ;; code-entry; SCI evaluates each entry as a single chunk.
          blocks (vec (:blocks ask-result))
          preflight-start-ns (System/nanoTime)
          preflight-result (code-entries-preflight iteration-position blocks)
          preflight-duration-ms (elapsed-ms preflight-start-ns)
          {:keys [code-entries normalized-code]} preflight-result
          _ (log-stage! :response-preflight/stop iteration
              {:duration-ms preflight-duration-ms
               :block-count (count blocks)
               :code-length (count normalized-code)
               :forms (count code-entries)
               :raw-fence-preflight? (boolean (:raw-fence-preflight-error preflight-result))})
          _ (when on-chunk
              (on-chunk {:phase :response-parse
                         :status :done
                         :iteration iteration-position
                         :duration-ms preflight-duration-ms
                         :code-length (count normalized-code)
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
          ;; Pivot: the legacy answer-after-error gate (engine-level
          ;; `prior-error-atom` + `block-result-error-summary`) was
          ;; deleted. One form per iteration means "prior form errored
          ;; in this same iteration" is structurally impossible — the
          ;; form either throws (and `(done …)` never runs) or
          ;; succeeds (and the answer is observed inside the same
          ;; eval frame).
          executed (mapv (fn [idx {:keys [expr parse-error render-segments]
                                   :vis/keys [preflight-error structurally-silent?]
                                   form-repaired? :repaired?
                                   :as entry}]
                           (log-stage! :code-exec iteration
                             {:idx (inc idx) :total total-blocks :code expr})
                           (when (and on-chunk
                                   (not suppress-form-start?)
                                   (not (conversation-title-meta-form? entry))
                                   (not (form-contains-turn-answer-call? entry)))
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
                                               :stdout "" :stderr "" :execution-time-ms 0
                                               :op :vis/guard}
                                              parse-error
                                              {:result nil
                                               :error (op-error (str "Parse error: " parse-error)
                                                        {:code expr :phase :edamame/parse})
                                               :stdout "" :stderr "" :execution-time-ms 0
                                               :op :edamame/parse}
                                              :else
                                              (if-let [err (literal-code-block-error expr)]
                                                {:result nil
                                                 :error (op-error err {:code expr :phase :vis/guard})
                                                 :stdout "" :stderr "" :execution-time-ms 0
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
                                          :iteration-count   iteration-position
                                          :position          idx
                                          :count             total-blocks
                                          :scope             scope
                                          :code              expr
                                          :render-segments   render-segments
                                          :vis/structurally-silent? (boolean structurally-silent?)
                                          :result            (:result result*)
                                          :journal           (:journal result*)
                                          :channel           (:channel result*)
                                          :error             (:error result*)
                                          :stdout            (:stdout result*)
                                          :stderr            (:stderr result*)
                                          :execution-time-ms (:execution-time-ms result*)
                                          :info        (:info result*)
                                          :role        (:role result*)
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
          ;; the block in the persisted/journal stream so the model still
          ;; reads the failure on its next iteration.
          preflight-by-idx (zipmap (range) (map (fn [{:vis/keys [preflight-error]}]
                                                  (boolean preflight-error))
                                             code-entries))
          blocks (validate-iteration-blocks!
                   (mapv (fn [idx code result segments structurally-silent?]
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
                                    :repaired? (:repaired? result)
                                    ;; Per-block def-sink: every (def ...) the
                                    ;; SCI sandbox evaluated in this block. The
                                    ;; iteration writer concats sinks across
                                    ;; blocks and drives definition_state from
                                    ;; the result. Pivot replaces the legacy
                                    ;; parse-source path.
                                    :def-sink (vec (:def-sink result))
                                    ;; Per-block resolve-symbol* LRU stamps:
                                    ;; symbol-name -> current-turn-pos for every
                                    ;; symbol the SCI hook saw resolve during
                                    ;; this block's eval. Iteration writer
                                    ;; merges into the long-lived per-env LRU.
                                    :lru (or (:lru result) {})}
                             ;; Per-form render breakdown for channel display.
                             ;; Channels that read :render-segments hide
                             ;; (done …) / (set-conversation-title! …)
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
                               :stdout            (:stdout b)
                               :stderr            (:stderr b)
                               :execution-time-ms (:execution-time-ms b)
                               :info        (:info b)
                               :role        (:role b)
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
               :engine-timing engine-timing
               :silent-form-idxs silent-form-idxs
               :llm-messages messages :llm-provider provider :llm-model model-name
               :llm-selected-provider (:provider resolved-model)
               :llm-selected-model (some-> (:name resolved-model) str)
               :llm-actual-provider provider
               :llm-actual-model model-name
               :llm-fallback-trace (:routed/fallback-trace ask-result)
               :llm-raw-response (:raw ask-result)
               :llm-executable-blocks (:blocks ask-result)
               :llm-returned-empty-code? (empty? blocks)
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

(defn rebuild-router!
  "Rebuild the router from the given config. Used when provider settings change."
  [config]
  (let [r (svar/make-router (runtime-router-providers config))]
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
;; the sandbox locals to materialize values. The pivot replaces that
;; path with the SCI eval-def monkey-patch — every def the sandbox
;; evaluates lands in `:def-sink`, and `def-sink->vars-snapshot` (above)
;; produces the persistence shape directly.
;; -----------------------------------------------------------------------------

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

   `CONVERSATION_TITLE` was retired as a SYSTEM var. The conversation
   title is a sidebar / channel-chrome label; the model never needed
   to read it from `<bindings>`. Setting flows through
   `(set-conversation-title! \"...\")` -> `:conversation-title-atom` +
   DB; the foundation `title-nudge` carries the current value in its
   text body when a refresh-cadence hint fires. Removing the binding
   drops one identical-per-iteration row from `definition_state`
   per conversation.

   Prior thinking text used to be bound to ITERATION_PREVIOUS_REASONING
   here. That var was retired once preserved-thinking replay started
   shipping the canonical assistant message back to the provider; the
   model now sees its prior reasoning natively in the messages array,
   and any forensic / inspection use case can read the iteration's
   `:thinking` column straight from the DB."
  [environment {:keys [final-result final-answer]}]
  (when final-result
    (env/bind-and-bump! environment 'CONVERSATION_PREVIOUS_ANSWER final-answer)))

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
   persisted var rows then have ONE row per iteration for each X,
   even when the value is unchanged or blank.

   Yes, turn-frozen vars (TURN_ID, TURN_POSITION,
   TURN_CONVERSATION_STATE_ID, TURN_SYSTEM_PROMPT,
   TURN_ACTIVE_EXTENSIONS, CONVERSATION_STATE_ID) repeat verbatim across
   iterations of the same turn - that is the intentional contract:
   \"every iteration carries a snapshot of every SYSTEM var\". The
   dedup-on-unchanged optimization the previous version did was a
   row-saving micro-opt that kept persisted rows stuck on iter 0
   for those names.

   Each var is normalized to a non-nil string so `definition_state`
   never stores nil for a SYSTEM var - makes the version vec a clean
   log of values across iterations.

   `CONVERSATION_TITLE` is intentionally absent: the conversation
   title is sidebar / channel chrome, not data the model uses for its
   work. Stamping it every iteration produced N identical rows for a
   value that changes ~once per conversation. Latest title lives on
   `conversation_state.title` (DB) + `:conversation-title-atom`
   (runtime); the foundation `title-nudge` carries the current value
   when refresh-cadence fires."
  [vars-snapshot {:keys [final-answer
                         turn-id turn-position
                         iteration-id iteration-position
                         conversation-state-id
                         system-prompt
                         extensions-snapshot]}]
  (let [stamp (fn [vs nm v]
                (conj vs {:name nm :value v :code ";; SYSTEM var"}))]
    (-> vars-snapshot
      (stamp "TURN_ID"                      (or turn-id ""))
      (stamp "TURN_POSITION"                (or turn-position 0))
      (stamp "TURN_CONVERSATION_STATE_ID"   (or conversation-state-id ""))
      (stamp "TURN_SYSTEM_PROMPT"           (or system-prompt ""))
      (stamp "TURN_ACTIVE_EXTENSIONS"       (or extensions-snapshot []))
      (stamp "TURN_ITERATION_ID"            (or iteration-id ""))
      (stamp "TURN_ITERATION_POSITION"      (or iteration-position 0))
      (stamp "CONVERSATION_STATE_ID"        (or conversation-state-id ""))
      (stamp "CONVERSATION_PREVIOUS_ANSWER" (or final-answer "")))))

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
      nil)))

(defn iteration-loop
  "The core iteration loop. Runs assemble -> ask LLM -> execute -> persist
   until the model emits `:answer` or the user cancels."
  [environment user-request
   {:keys [system-prompt
           conversation-turn-id
           ;; `max-context-tokens` was the legacy `build-iteration-context`
           ;; token-budget knob. The tape assembler ignores it for now;
           ;; rename / drop pending Phase 8 caller-signature cleanup.
           #_:clj-kondo/ignore max-context-tokens
           hooks cancel-atom current-iteration-atom
           reasoning-default routing extra-body turn-features allow-copilot-claude-deep?
           workspace]}]
  (let [environment (cond-> environment
                      (seq turn-features) (assoc :turn/features turn-features)
                      (seq workspace) (merge workspace))
        resolved-model (resolve-effective-model (:router environment))
        effective-model (:name resolved-model)
        _ (assert effective-model "Router must resolve a root model")
        has-reasoning? (reasoning-effort-configurable? resolved-model)
        base-reasoning-level (or (normalize-reasoning-level reasoning-default) balanced-reasoning)
        ;; Activate extensions ONCE per turn. Threaded through both the
        ;; prompt message assembler (core, environment, extension messages) and
        ;; the per-iteration ext nudge collector - activation-fn never re-fires inside the loop.
        active-exts   (prompt/active-extensions environment)
        _             (sync-active-extension-symbols! environment active-exts)
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
    ;; iteration boundaries via `update-system-vars!`. The retired
    ;; `CONVERSATION_TITLE` binding is gone — the title now lives
    ;; only on `:conversation-title-atom` + the DB, surfaced to the
    ;; model through the foundation `title-nudge` when blank or stale.
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
    ;; Reset TURN_ITERATION_ID + TURN_ITERATION_POSITION at turn start;
    ;; rebound by
    ;; `update-iteration-id!` after each iteration row commits.
    (env/bind-and-bump! environment 'TURN_ITERATION_ID nil)
    (env/bind-and-bump! environment 'TURN_ITERATION_POSITION 0)
    (when-let [a (:current-iteration-id-atom environment)] (reset! a nil))
    (when-let [a (:current-conversation-turn-id-atom environment)] (reset! a conversation-turn-id))
    (when-let [a (:current-user-request-atom environment)] (reset! a user-request))
    ;; Phase 7: inject USER_REQUEST as a regular sandbox def so the
    ;; model sees it in `(env/tape-system-vars …)` and can interpolate
    ;; via the same surface as every other var.
    (env/bind-and-bump-with-doc! environment 'USER_REQUEST
      "current turn user request"
      (or user-request ""))
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
    ;;
    ;; IMPORTANT: cross-turn entries are JOURNAL ONLY. Do not replay their
    ;; provider-native preserved-thinking assistant messages into the new
    ;; user turn. In conversation a9389e1d, Z.ai/GLM received prior-turn
    ;; assistant replay and opened the next request with "answer already
    ;; accepted", then burned >100k input tokens. Durable cross-turn memory
    ;; must flow through <journal>/<bindings>, not hidden reasoning state.
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
                          :blocks   [(cond-> {:position 0
                                              :code (or (:code it) "")}
                                       (contains? it :result) (assoc :result (:result it))
                                       (contains? it :error) (assoc :error (:error it))
                                       (contains? it :stdout) (assoc :stdout (:stdout it))
                                       (contains? it :stderr) (assoc :stderr (:stderr it))
                                       (contains? it :execution-time-ms)
                                       (assoc :execution-time-ms (:execution-time-ms it)))]
                          :llm-provider (:provider it)
                          :llm-model    (some-> (:model it) str)
                          ;; Persisted assistant messages are intentionally NOT
                          ;; replayed across user turns. Keep the row metadata for
                          ;; diagnostics, but `compatible-preserved-thinking-journal-iters`
                          ;; rejects this entry before replay.
                          :assistant-message (:llm-assistant-message it)
                          :preserved-thinking/replay? false}])
                  iters)))
            (catch Throwable t
              (tel/log! {:level :warn :id ::cross-turn-journal-seed-failed
                         :data  {:error (ex-message t)}
                         :msg   "Cross-turn journal seed failed; first iteration starts with an empty <journal>"})
              nil))]
      (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :iteration-loop})]
        (loop [loop-state (merge {:iteration 0 :messages initial-messages
                                  :trace []}
                            FRESH_ITER_CARRY
                            (when (seq seeded-journal-iters)
                              {:journal-iters seeded-journal-iters}))]
          (let [{:keys [iteration messages trace journal-iters]} loop-state]
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
                    ;; Phase 7 swap: tape-based assembly. system-vars +
                    ;; live-vars come from sandbox introspection;
                    ;; per-env LRU drives staleness; tape entries come
                    ;; from `journal-iters` via the same shape adapter.
                    iteration-context (prompt/build-iteration-context-tape environment
                                        {:blocks-by-iteration journal-iters
                                         :active-extensions   active-exts
                                         :iteration           iteration
                                         :current-status      :current
                                         :system-vars         (env/tape-system-vars (:sci-ctx environment))
                                         :live-vars           (env/tape-live-vars
                                                                (:sci-ctx environment)
                                                                (:initial-ns-keys environment)
                                                                (some-> (:def-resolve-lru-atom environment) deref)
                                                                (some-> (:current-turn-position-atom environment) deref))})
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
                                               (cond-> {:conversation-turn-id conversation-turn-id :vars [] :code ""
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
                                 :trace (conj trace trace-entry))))))

                  (let [_ (accumulate-usage! (:api-usage iteration-result))
                        {:keys [thinking blocks final-result]} iteration-result
                        final-answer (when final-result (:answer final-result))
                        _ (update-system-vars! environment
                            {:thinking thinking :final-result final-result :final-answer final-answer})
                        ;; Pivot: drive var persistence from the SCI def-sink,
                        ;; not from post-eval source parsing. Every (def …)
                        ;; the sandbox evaluated this iteration is captured
                        ;; by the eval-def monkey-patch and lands on each
                        ;; block's `:def-sink`. We concat across blocks (one
                        ;; block per iteration post-Phase-4 main, but the
                        ;; aggregator stays general) and produce one var
                        ;; row per def.
                        iteration-block-code (->> blocks
                                               (map :code)
                                               (remove str/blank?)
                                               (str/join "\n\n"))
                        iteration-time-ms (->> blocks
                                            (keep :execution-time-ms)
                                            (reduce + 0))
                        vars-snapshot (def-sink->vars-snapshot
                                        (vec (mapcat :def-sink blocks))
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
                        ;; into the long-lived per-env LRU map. The
                        ;; tape live-vars renderer reads this to age
                        ;; user vars out of the discovery line after
                        ;; `TAPE_LRU_TURN_WINDOW` quiet turns.
                        _ (when-let [lru-atom (:def-resolve-lru-atom environment)]
                            (let [iteration-lru (reduce (fn [m b]
                                                          (merge m (or (:lru b) {})))
                                                  {} blocks)]
                              (when (seq iteration-lru)
                                (swap! lru-atom merge iteration-lru))))
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
                                         ;; Frozen-at-this-iter map of conversation
                                         ;; facts (channel, external-id, turn-count,
                                         ;; created-at). Saves the model an iter
                                         ;; round-trip when it just needs one of
                                         ;; those fields.
                                         :conversation-metadata conversation-metadata})
                        store-block (or (first blocks) {:code "" :error {:message "empty iteration"}})
                        iteration-id (persistance/db-store-iteration! (:db-info environment)
                                       (let [tc (iteration-token-cost (:api-usage iteration-result))]
                                         (cond-> {:conversation-turn-id conversation-turn-id
                                                  :code (:code store-block)
                                                  :result (:result store-block)
                                                  :error (:error store-block)
                                                  :stdout (:stdout store-block)
                                                  :stderr (:stderr store-block)
                                                  :duration-ms (or (:execution-time-ms store-block) (:duration-ms iteration-result) 0)
                                                  :vars vars-snapshot
                                                  :dependencies deps-snapshot
                                                  :thinking thinking
                                                  :answer (when final-result (answer-str (:answer final-result)))
                                                  :llm-messages (:llm-messages iteration-result)
                                                  :llm-provider (or (:llm-provider iteration-result) (:provider resolved-model))
                                                  :llm-model (:llm-model iteration-result)
                                                  :llm-raw-response (:llm-raw-response iteration-result)
                                                  :llm-executable-blocks (:llm-executable-blocks iteration-result)
                                                  :llm-returned-empty-code? (:llm-returned-empty-code? iteration-result)
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
                                     :blocks blocks :final? (boolean final-result)}]
                    (cond
                      final-result
                      (do (log-stage! :final iteration
                            {:answer (truncate (answer-str (:answer final-result)) 200)
                             :iteration-count (inc iteration)})
                        (log-stage! :iteration/stop iteration
                          {:blocks (count blocks) :errors (count (filter :error blocks))
                           :times (mapv :execution-time-ms blocks)})
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
                            (recur (merge loop-state
                                     {:iteration          (inc iteration)
                                      :messages           messages
                                      :trace              (conj trace trace-entry)
                                      :journal-iters       next-recent}))))))))))))))))

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
             ;; to `(done ...)`; without this coercion the TUI
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
      `:conversation-title-atom` mirror sees the new value AND so a
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
                         (with-default-ask-code-idle-timeout
                           (cond-> {:messages           [(svar/user prompt)]
                                    :lang               "text"
                                    :reasoning          :off
                                    :code-tail-pointer? true}
                             llm-headers (assoc :llm-headers llm-headers)))))
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
        (log-stage! :turn/complete 0
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
        (log-stage! :turn/complete 0
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
         {:keys [eval-timeout-ms
                 debug? user-request root-resolved-model root-model
                 db-info
                 environment-id]} ctx]
     (binding [*rlm-context*       {:rlm-environment-id environment-id :rlm-type :main
                                    :rlm-debug? debug? :rlm-phase :turn
                                    :db-info db-info
                                    :conversation-soul-id (:conversation-id environment)}
               *eval-timeout-ms*  (clamp-eval-timeout-ms
                                    (or eval-timeout-ms *eval-timeout-ms*))]
       (tel/with-ctx+ {:db-info db-info
                       :conversation-soul-id (:conversation-id environment)}
         (log-stage! :turn/open 0
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
        bindings-atom           (atom {:index            nil
                                       :revision         -1
                                       :current-revision 0})
        state-atom               (atom {:custom-bindings {}
                                        :environment     nil
                                        :conversation-id nil})
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
        current-conversation-turn-id-atom (atom nil)
        current-turn-position-atom (atom nil)
        current-user-request-atom (atom nil)
        ;; Title atom: in-memory cache for the conversation title.
        ;; The DB column on `conversation_state` is the persisted
        ;; truth; this atom is the fast read path for  and
        ;; the source for the title nudge / channel chrome at iteration
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
        ;; SCI binding for `(done "...")` - the canonical turn-
        ;; termination call. Closes over `answer-atom` AND
        ;; `current-form-idx-atom` so the iteration loop can scope
        ;; the discard check to the form that actually called this.
        ;; Returns the marker keyword so the per-form result row makes
        ;; request visible.
        answer-fn                (fn done [s]
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
                                      :position @current-form-idx-atom})
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
        ;; ONE-ARITY ONLY. There is no zero-arg reader by design: the
        ;; model has no in-sandbox read path for the title (the
        ;; `CONVERSATION_TITLE` SYSTEM var was retired as redundant).
        ;; The foundation `title-nudge` surfaces the current value
        ;; when relevant. Calling with the wrong arity raises an
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
        env-bindings             {'done            answer-fn
                                  'set-conversation-title! conversation-title-fn}
        {:keys [sci-ctx sandbox-ns initial-ns-keys]}
        (env/create-sci-context (merge env-bindings
                                  (:custom-bindings @state-atom)))
        env {:environment-id                    environment-id
             :conversation-id                   conversation-id
             :channel                           (or channel :tui)
             :depth-atom                        depth-atom
             :db-info                           db-info
             :bindings-atom                     bindings-atom
             :state-atom                        state-atom
             :sci-ctx                           sci-ctx
             :sandbox-ns                        sandbox-ns
             :initial-ns-keys                   initial-ns-keys
             ;; Long-lived per-env LRU map: `{var-name-string →
             ;; last-used-turn-pos}`. Merged from each iteration's
             ;; `:lru` after eval. Read by `prompt/tape-live-vars` to
             ;; decide which user vars to surface in the live-vars
             ;; discovery line. Pivot Phase 7 wire.
             :def-resolve-lru-atom              (atom {})
             :router                            router
             :answer-atom                       answer-atom
             :current-form-idx-atom             current-form-idx-atom
             :current-iteration-atom            current-iteration-atom
             :current-iteration-id-atom         current-iteration-id-atom
             :current-conversation-turn-id-atom current-conversation-turn-id-atom
             :current-turn-position-atom        current-turn-position-atom
             :current-user-request-atom         current-user-request-atom
             :conversation-title-atom           conversation-title-atom
             :extensions                        (atom [])}]
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
  `(svar/ask-code! (:router environment) ...)` - not the global
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

(defn- extension-loader-nses
  "Namespaces that load/register `ext`. Most extensions use their
   `:ext/namespace` directly. Lightweight registrars and provider bundles
   register one or more logical extension ids from a separate manifest ns;
   those must declare `:ext/nses` so reload diffs against the loader ns,
   not the logical extension id."
  [ext]
  (set (or (some-> (:ext/nses ext) seq vec)
         [(:ext/namespace ext)])))

(defn- diff-extensions
  "Compute the F1-lite diff between the current in-memory
   `extension-registry` and a freshly-scanned `manifests` map
   (manifest-id -> entry, where entry has `:nses`).

   Returns `{:added [...] :removed [...] :reloaded [...]}`.

   `:added` / `:reloaded` are manifest loader namespaces to require.
   `:removed` is logical `:ext/namespace` ids to deregister.

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
                               (map :ext/namespace)
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
     {:added [...]              ;; loader ns-syms newly required
      :removed [...]             ;; logical extension ns-syms deregistered
      :reloaded [...]            ;; loader ns-syms re-required + re-registered
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
  (when-let [{:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}
             (clojure.core/get @cache id)]
    (.lock lock)
    (try
      (try (dispose-environment! environment) (catch Exception _ nil))
      (finally (.unlock lock))))
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
  (doseq [[_ {:keys [environment ^java.util.concurrent.locks.ReentrantLock lock]}] @cache]
    (.lock lock)
    (try
      (try (dispose-environment! environment) (catch Exception _ nil))
      (finally (.unlock lock))))
  (reset! cache {})
  (persistance/db-dispose-shared-connection!))
