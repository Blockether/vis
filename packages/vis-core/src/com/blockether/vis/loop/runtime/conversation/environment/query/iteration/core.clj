(ns com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core
  "Single LLM iteration.

   `run-iteration` calls the LLM via ask!, parses the structured response,
   executes code blocks, validates final answers. Returns a map describing
   everything that happened in one iteration.

   Also contains error-normalization helpers for infrastructure vs recoverable
   failures."
  (:require
   [clojure.set]
   [clojure.string :as str]
   [com.blockether.vis.error :as vis-error]
   [com.blockether.vis.loop.runtime.conversation.environment.core :as sci-env]
   [com.blockether.vis.extension :as ext]

   [com.blockether.vis.persistance.core :as db]
   [com.blockether.vis.persistance.spec :as rlm-spec
    :refer [ITERATION_SPEC_NON_REASONING
            *eval-timeout-ms* *rlm-ctx* clamp-eval-timeout-ms]]
   [com.blockether.vis.loop.mustache :as mustache]
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.svar.internal.router :as router]
   [edamame.core :as edamame]
   [sci.core :as sci]
   [taoensso.telemere :as tel]))

;; ---------------------------------------------------------------------------
;; Core helpers
;; ---------------------------------------------------------------------------

(def ^:const MAX_RESULT_DISPLAY_CHARS 30000)

(defn- truncate [s n]
  (let [s (str s)] (if (> (count s) n) (subs s 0 n) s)))

(defn- strip-sandbox-ns [s]
  (-> (str s) (str/replace "sandbox/" "")))

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

(defn normalize-reasoning-level [v]
  (router/normalize-reasoning-level v))

(defn reasoning-level-for-errors [base consecutive-errors]
  (cond
    (<= (long consecutive-errors) 0) base
    (= 1 (long consecutive-errors)) (if (= base :quick) :balanced :deep)
    :else :deep))

(defn answer-str [answer]
  (let [v (:result answer answer)]
    (if (string? v) v (str v))))

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

(def ^:private BARE_STRING_RE #"^\s*\"[^\"]*\"\s*$")

(defn- bare-string-code-block? [expr]
  (boolean (re-matches BARE_STRING_RE (str expr))))

(defn- comment-only-block? [^String expr]
  (try
    (zero? (count (edamame/parse-string-all (str/trim expr) edamame-opts)))
    (catch Throwable _ false)))

(defn- literal-code-block-error [expr]
  (cond
    (bare-string-code-block? expr)
    "Bare string literal in :code. Prose belongs in :answer (the loop auto-detects plain text), not in :code."

    (comment-only-block? expr)
    "Code block contains only comments / discards (`;;` or `#_`) and no executable form. Add an expression to evaluate, or drop the block entirely."))

(defn- detect-common-mistakes [code]
  (let [s (str/trim code)]
    (cond
      (re-find #"#\([^)]*#\(" s)
      "Nested #() is illegal in Clojure. Rewrite inner #() as (fn [...] ...)"
      :else nil)))

(defn- run-sci-code [sci-ctx code & {:keys [sandbox-ns]}]
  (let [stdout-writer (java.io.StringWriter.)
        stderr-writer (java.io.StringWriter.)
        err-pw       (java.io.PrintWriter. stderr-writer true)
        exec-future (future
                      (try
                        (let [result (sci/binding [sci/out stdout-writer
                                                   sci/err err-pw]
                                       (let [ns (or (sci/find-ns sci-ctx 'sandbox) sandbox-ns)]
                                         (:val (sci/eval-string+ sci-ctx code
                                                 (when ns {:ns ns})))))]
                          {:result result :stdout (str stdout-writer) :stderr (str stderr-writer) :error nil})
                        (catch Throwable e
                          {:result nil :stdout (str stdout-writer) :stderr (str stderr-writer)
                           :error (str (.getSimpleName (class e)) ": " (or (ex-message e) (str e)))})))
        timeout-ms (long *eval-timeout-ms*)
        execution-result (try
                           (deref exec-future timeout-ms nil)
                           (catch Throwable e
                             {:result nil :stdout "" :stderr "" :error (str (.getSimpleName (class e)) ": " (ex-message e))}))]
    (.close stdout-writer)
    (.close stderr-writer)
    (if (nil? execution-result)
      (do (future-cancel exec-future)
        {:result nil :stdout "" :stderr "" :error (str "Timeout (" (/ timeout-ms 1000) "s)") :timeout? true})
      execution-result)))

(defn- run-with-timing [sci-ctx code sandbox-ns timeout-ms start-time]
  (let [execution-result (if timeout-ms
                           (binding [*eval-timeout-ms* (clamp-eval-timeout-ms timeout-ms)]
                             (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
                           (run-sci-code sci-ctx code :sandbox-ns sandbox-ns))
        execution-time (- (System/currentTimeMillis) start-time)]
    (if (:timeout? execution-result)
      (assoc execution-result :execution-time-ms execution-time :timeout? true)
      (assoc execution-result :execution-time-ms execution-time :timeout? false))))

(defn- try-extension-parse-rescue
  "Walk the environment's active extensions and ask each one's
   `:ext/on-parse-error-fn` to rewrite `code`. Returns the rewritten
   string when the rescue both differs from the original AND parses
   cleanly. nil otherwise. Read-only on the environment."
  [environment code parse-error]
  (when-let [exts (some-> (:extensions environment) deref seq)]
    (when-let [fixed (ext/try-rescue-parse-error exts code parse-error environment)]
      (when (nil? (parse-clojure-syntax fixed))
        fixed))))

(defn canonical-expression-hash
  "Whitespace-and-form-normalized hash of `expression`. Parses the form
   via edamame so `(grep \"X\")` and `(grep   \"X\")` collapse to the
   same hash, then `pr-str`s the AST and hashes that. Falls back to a
   raw-string hash when the parser can't read the input — better to
   over-count duplicates than to miss them due to a tagged literal.

   Stable across JVM runs because we hash the printed AST, not the
   reader's internal data structures."
  [expression]
  (try
    (let [forms (edamame/parse-string-all expression edamame-opts)]
      (str (hash (pr-str forms))))
    (catch Throwable _
      (str (hash (str expression))))))

(defn count-duplicates
  "Count how many entries in `expressions` have a canonical hash that
   already lives in `seen-hashes-atom`, INCLUDING intra-iteration
   duplicates. The seen-set is grown incrementally during the walk:
   `(grep \"X\")` followed by another `(grep \"X\")` in the same iter
   counts the second occurrence as a duplicate. After counting, the
   atom is updated with the SUCCESSFUL hashes (errors / timeouts are
   NOT recorded — retrying after failure is legitimate).

   Returns `[duplicates total]`."
  [seen-hashes-atom expressions]
  (let [{:keys [duplicates seen]}
        (reduce (fn [{:keys [duplicates seen]} expression]
                  (let [h      (canonical-expression-hash (or (:code expression) ""))
                        is-dup (contains? seen h)
                        ok?    (and (nil? (:error expression))
                                 (not (:timeout? expression)))]
                    {:duplicates (if is-dup (inc duplicates) duplicates)
                     :seen (if (and ok? (not is-dup)) (conj seen h) seen)}))
          {:duplicates 0 :seen @seen-hashes-atom}
          expressions)]
    (reset! seen-hashes-atom seen)
    [duplicates (count expressions)]))

(defn dedup-cache-lookup
  "Returns a synthetic execution-result map when `expression`'s
   canonical hash is already in `dedup-cache-atom`, else nil. The
   cached map carries the prior result + a `:cached-from` annotation
   pointing at the iteration-id that originally produced it.

   The synthetic result has `:execution-time-ms 0` and `:cached? true`
   so downstream rendering / metadata collection can flag the
   short-circuit."
  [dedup-cache-atom expression]
  (when (and dedup-cache-atom expression)
    (let [h   (canonical-expression-hash expression)
          hit (get @dedup-cache-atom h)]
      (when hit
        (assoc hit
          :cached? true
          :execution-time-ms 0)))))

(defn dedup-cache-record!
  "Record `result` in `dedup-cache-atom` keyed by the canonical hash of
   `expression`. No-op when the result is an error/timeout (retries are
   legitimate). The recorded entry carries an `:cached-from` field
   naming the iteration-id where the call originally succeeded."
  [dedup-cache-atom expression result iteration-id]
  (when (and dedup-cache-atom expression result
          (nil? (:error result))
          (not (:timeout? result)))
    (let [h (canonical-expression-hash expression)]
      (swap! dedup-cache-atom
        (fn [cache]
          (if (contains? cache h)
            cache
            (assoc cache h
              {:result            (:result result)
               :stdout            (:stdout result)
               :stderr            (:stderr result)
               :cached-from       iteration-id
               :original-duration (:execution-time-ms result)})))))))

(def ^:private DEF_HEADS '#{def defn defn- defmacro})

(defn extract-defining-name
  "Return the symbol named by a `(def NAME …)` / `(defn NAME …)` /
   `(defn- NAME …)` / `(defmacro NAME …)` form, or nil otherwise.
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
   defining form, if any. No-op when `expression` is not a (def…) /
   (defn…) shape, when doc-string is blank, or when the SCI eval throws.
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

(defn- capture-new-autobind-events
  [environment event-start-count]
  (when-let [autobind-events-atom (:autobind-events-atom environment)]
    (let [all-events @autobind-events-atom
          total-count (count all-events)]
      (when (< event-start-count total-count)
        (vec (subvec all-events event-start-count total-count))))))

(defn- maybe-assoc-autobind-events
  [execution-result autobind-events]
  (if (seq autobind-events)
    (assoc execution-result :autobind-events autobind-events)
    execution-result))

(defn- execute-code
  "Run a single :code block through the SCI sandbox.

   Optional kwargs:
     :timeout-ms          — hard-cap eval time, clamped at the
                            *eval-timeout-ms* bounds.
     :doc                 — docstring to attach to the var defined by
                            this :expr.
     :dedup-cache-atom    — per-query `{hash -> cached-result}` atom.
                            When the canonical hash of `code` is
                            already in the cache, the SCI eval is
                            skipped and a synthetic `:cached?`
                            result is returned. Misses fall through
                            to a real eval and the success is
                            recorded back into the cache.
     :iteration-id        — `iN.K` string used as `:cached-from` on
                            cache writes. Required when
                            `:dedup-cache-atom` is provided."
  [{:keys [sci-ctx sandbox-ns] :as environment} code
   & {:keys [timeout-ms doc dedup-cache-atom iteration-id]}]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :execute-code})]
    (let [start-time (System/currentTimeMillis)
          autobind-events-atom (:autobind-events-atom environment)
          event-start-count (if autobind-events-atom
                              (count @autobind-events-atom)
                              0)
          lint-error (detect-common-mistakes code)
          cached     (dedup-cache-lookup dedup-cache-atom code)]
      (cond
        lint-error
        {:result nil :stdout "" :stderr "" :error lint-error
         :execution-time-ms 0 :timeout? false}

        ;; Dedup short-circuit: a prior successful execution of this
        ;; same canonical form returns its cached result without re-
        ;; running. The model sees `:cached? true` so it knows the
        ;; value came from the cache, not a fresh run.
        cached cached

        :else
        (let [parse-error (parse-clojure-syntax code)]
          (if parse-error
            (if-let [rescued (try-extension-parse-rescue environment code parse-error)]
              (let [exec (run-with-timing sci-ctx rescued sandbox-ns timeout-ms start-time)]
                (when (nil? (:error exec))
                  (attach-doc-meta! environment rescued doc)
                  (dedup-cache-record! dedup-cache-atom rescued exec iteration-id))
                (-> (assoc exec
                      :repaired? true
                      :original-code code
                      :original-error parse-error)
                  (maybe-assoc-autobind-events
                    (capture-new-autobind-events environment event-start-count))))
              {:result nil :stdout "" :stderr "" :error parse-error
               :execution-time-ms 0 :timeout? false})
            (let [exec (run-with-timing sci-ctx code sandbox-ns timeout-ms start-time)]
              (when (nil? (:error exec))
                (attach-doc-meta! environment code doc)
                (dedup-cache-record! dedup-cache-atom code exec iteration-id))
              (maybe-assoc-autobind-events
                exec
                (capture-new-autobind-events environment event-start-count)))))))))

(def ^:const SLOW_EXECUTION_MS 5000)
(def ^:const EXECUTION_SAFETY_CAP_CHARS MAX_RESULT_DISPLAY_CHARS)
(def ^:const EXECUTION_STDERR_CHARS 2000)
(def ^:const EXECUTION_STDOUT_CHARS 2000)
(def ^:const RECENT_THOUGHT_MAX_CHARS 4000)
(def ^:const BREADCRUMBS_KEEP_LAST 20)
(def ^:const RECENT_KEEP_ITERS 1)
(def ^:const PLAN_MAX_ITEMS 20)

;; Print-cap defaults for `safe-pr-str` — chosen so a wide flat
;; collection or a deep nested map still pr-strs without materializing
;; an unbounded JVM string before truncation. Override per call site
;; when a tighter or looser bound is required.
(def ^:const PRINT_LENGTH_DEFAULT 64)
(def ^:const PRINT_LEVEL_DEFAULT  8)
(def ^:const PRINT_CHARS_DEFAULT  1000)

(defn safe-pr-str
  "Bounded `pr-str`. Three overlapping caps so the resulting string
   never materializes more than the configured bound regardless of
   input size:

   1. `:print-length` (default `PRINT_LENGTH_DEFAULT`) — binds
      `*print-length*`, capping element count per collection during
      pr.
   2. `:print-level` (default `PRINT_LEVEL_DEFAULT`) — binds
      `*print-level*`, capping nesting depth during pr.
   3. `:max-chars` (default `PRINT_CHARS_DEFAULT`) — final clip on the
      already-bounded string.

   Returns either the full bounded string or, when the char cap was
   hit, the prefix + a `…<+N chars>` suffix so the caller / model
   knows the value was clipped.

   Use this anywhere a value of unbounded user/model origin is being
   embedded into a projection / log / nudge. Reserve raw `pr-str` for
   building Clojure source code (where size is bounded by the
   surrounding form) and for scalars whose printed size is bounded by
   their type."
  ([v] (safe-pr-str v nil))
  ([v {:keys [print-length print-level max-chars]
       :or   {print-length PRINT_LENGTH_DEFAULT
              print-level  PRINT_LEVEL_DEFAULT
              max-chars    PRINT_CHARS_DEFAULT}}]
   (let [bounded (binding [*print-length* print-length
                           *print-level*  print-level]
                   (pr-str v))]
     (if (> (count bounded) max-chars)
       (str (subs bounded 0 max-chars)
         " …<+" (- (count bounded) max-chars) " chars>")
       bounded))))

(defn- truncated-pr-str
  "Wrapper used by `<recent>` rendering. Returns `[bounded-string
   truncated?]`. Strips sandbox-ns prefix so the projection is clean."
  [v]
  (let [bounded (strip-sandbox-ns
                  (safe-pr-str v {:max-chars EXECUTION_SAFETY_CAP_CHARS}))
        ;; safe-pr-str's clip-marker (" …<+N chars>") is the truncation
        ;; signal; cheaper than re-comparing lengths.
        truncated? (boolean (re-find #" …<\+\d+ chars>$" bounded))]
    [bounded truncated?]))

;; ---------------------------------------------------------------------------
;; <attempts> ledger — deduped log of executed code blocks across the
;; entire turn so far, addressable by `iN.K` ids. Replaces the
;; old strategy of "the model has to remember every call it made"
;; with explicit projection-time visibility. The PEV gate's
;; `:evidence iN.K` references resolve into this block.
;; ---------------------------------------------------------------------------

(def ^:const ATTEMPTS_KEEP_LAST 50)
(def ^:const ATTEMPTS_VALUE_MAX_CHARS 120)
(def ^:const ATTEMPTS_CODE_MAX_CHARS 200)

#_{:clj-kondo/ignore [:unused-private-var]} ;; WIP: pending wire-in to build-iteration-context
(defn- format-attempts-block
  "Render the deduped attempts ledger as `<attempts>`. Each entry is a
   one-liner: `iN.K  <code> -> <result-or-error>`. Bounded by
   `ATTEMPTS_KEEP_LAST` (50) so multi-iteration turns can't blow up
   the projection. Dedup uses canonical hashes so the same call is
   never listed twice; the `iN.K` id refers to the FIRST occurrence."
  [attempts]
  (when (seq attempts)
    (let [seen (volatile! #{})
          rows (vec
                 (keep (fn [{:keys [iteration-id code result error]}]
                         (let [h (canonical-expression-hash code)]
                           (when-not (contains? @seen h)
                             (vswap! seen conj h)
                             (let [code-str  (truncate (str/trim (str code))
                                               ATTEMPTS_CODE_MAX_CHARS)
                                   value-str (cond
                                               error
                                               (str "ERROR: " (truncate (str error) 200))
                                               :else
                                               (safe-pr-str result
                                                 {:max-chars ATTEMPTS_VALUE_MAX_CHARS
                                                  :print-length 8
                                                  :print-level 4}))]
                               (str "  " iteration-id "  " code-str " → " value-str)))))
                   attempts))
          kept (take-last ATTEMPTS_KEEP_LAST rows)]
      (when (seq kept)
        (str "<attempts>\n" (str/join "\n" kept) "\n</attempts>")))))

(defn build-attempts-from-iterations
  "Walk persisted iteration rows and pull every executed expression
   into a flat seq of `{:iteration-id :code :result :error}` maps the
   `<attempts>` block can render. Used by the loop to gather the full
   query-so-far ledger; the underlying DB queries are the same shape
   the self-debug extension uses."
  [db-info iterations]
  (vec
    (mapcat (fn [iteration]
              (let [iteration-position (:position iteration)]
                (try
                  (mapv (fn [position row]
                          {:iteration-id (str "i" iteration-position "." (inc position))
                           :code         (:code row)
                           :result       (:result row)
                           :error        (:error row)})
                    (range)
                    (db/db-list-iteration-expressions db-info (:id iteration)))
                  (catch Throwable _ []))))
      iterations)))

(defn- format-recent-block
  "Render the last `RECENT_KEEP_ITERS` iterations of expression results
   with iN.K addressable ids.

   `expressions-by-iteration` is a seq of `[iteration-position [exprs]]` pairs, ordered
   oldest-first.. Only the most recent iteration is shipped
   to keep the projection bounded; the breadcrumb chain (one line per iteration)
   plus the addressable `iN.K` ids in <attempts> cover everything
   else the model needs to reference."
  [expressions-by-iteration]
  (let [kept (take-last RECENT_KEEP_ITERS (or expressions-by-iteration []))]
    (when (seq kept)
      (let [lines (for [[iteration-position exprs] kept
                        [k expr]         (map-indexed vector exprs)
                        :let [{:keys [code error result stdout stderr execution-time-ms autobind-events]} expr]]
                    (let [code-str      (str/trim (or code ""))
                          stdout-suffix (when-not (str/blank? stdout)
                                          (str " :stdout " (pr-str (truncate stdout EXECUTION_STDOUT_CHARS))))
                          stderr-suffix (when-not (str/blank? stderr)
                                          (str " :stderr " (pr-str (truncate stderr EXECUTION_STDERR_CHARS))))
                          time-ms       (or execution-time-ms 0)
                          slow-suffix   (when (> time-ms SLOW_EXECUTION_MS)
                                          (str " (" time-ms "ms SLOW)"))
                          autobind-suffix (when (seq autobind-events)
                                            (str " " (str/join " " (keep :footer autobind-events))))
                          value-part    (if error
                                          (str "ERROR: " (truncate error 400))
                                          (let [v (realize-value result)
                                                [value-str truncated?] (truncated-pr-str v)]
                                            (str value-str
                                              (when truncated? " :truncated? true")
                                              (or autobind-suffix ""))))]
                      (str "  i" iteration-position "." (inc k) "  " code-str " → " value-part
                        (or slow-suffix "")
                        (or stdout-suffix "")
                        (or stderr-suffix ""))))]
        (when (seq lines)
          (str "<recent>\n" (str/join "\n" lines) "\n</recent>"))))))

;; -- Plan-as-first-class-slot helpers --------------------------
;;
;; Two complementary projections of the agent's reasoning state:
;;
;;   <plan>          sticky structured TODO list, carried verbatim from
;;                   the most-recent iteration row that emitted a :plan. The
;;                   model only re-emits it when reality forces a real
;;                   change; otherwise the loop carries it forward.
;;
;;   <breadcrumbs>   bounded one-liner chain (last K=20). Built from
;;                   the :breadcrumb column of recent iteration rows, ordered
;;                   oldest-first, prefixed by "iN ".
;;
;; The plan never gets re-summarized through tactical iterations, and
;; the breadcrumbs preserve the strategic frame at one line per iteration.

(def ^:private PLAN_STATUS_DISPLAY
  {:in-progress "in-progress"
   :pending     "pending"
   :done        "done"
   :blocked     "blocked"})

(defn- normalize-plan-status [status]
  (cond
    (keyword? status)
    (or (PLAN_STATUS_DISPLAY status)
      (PLAN_STATUS_DISPLAY (-> status name (str/replace "_" "-") keyword))
      (name status))

    (string? status)
    (or (PLAN_STATUS_DISPLAY (-> status (str/replace "_" "-") keyword))
      status)

    :else "pending"))

(defn- plan-item-line [{:keys [id content status evidence]}]
  (let [status-s (normalize-plan-status status)
        ev-s    (when (and evidence (not (str/blank? (str evidence))))
                  (str "  (" (str/trim (str evidence)) ")"))]
    (str "  [" id "] " status-s " — " (str/trim (str content)) (or ev-s ""))))

(defn- format-plan-block
  "Render structured plan as the model-facing `<plan>` block. Returns nil
   when no plan ever emitted (model has not decomposed yet) so the model
   sees a clean iteration-0 prompt instead of an empty wrapper."
  [plan-state]
  (when (and (map? plan-state) (or (:goal plan-state) (seq (:items plan-state))))
    (let [{:keys [goal items open decided]} plan-state
          item-lines (map plan-item-line (or items []))
          open-lines (when (seq open) (map #(str "  - " (str/trim (str %))) open))
          decided-lines (when (seq decided) (map #(str "  - " (str/trim (str %))) decided))]
      (str "<plan>\n"
        (when goal (str "  goal: " (str/trim (str goal)) "\n"))
        (when (seq item-lines) (str (str/join "\n" item-lines) "\n"))
        (when (seq open-lines)
          (str "  open:\n" (str/join "\n" (map #(str "  " %) open-lines)) "\n"))
        (when (seq decided-lines)
          (str "  decided:\n" (str/join "\n" (map #(str "  " %) decided-lines)) "\n"))
        "</plan>"))))

(defn- format-breadcrumbs-block
  "Render the cumulative breadcrumb chain as `<breadcrumbs>`. Each entry
   is a one-line breadcrumb authored by the model in :breadcrumb. Bounded
   at last K=20. Ordered oldest-first (matches reading order)."
  [breadcrumb-rows]
  (let [entries (->> breadcrumb-rows
                  (keep (fn [{:keys [position breadcrumb]}]
                          (when (and breadcrumb (not (str/blank? breadcrumb)))
                            (str "  i" position "  " (str/trim breadcrumb)))))
                  (take-last BREADCRUMBS_KEEP_LAST))]
    (when (seq entries)
      (str "<breadcrumbs>\n" (str/join "\n" entries) "\n</breadcrumbs>"))))

(defn- format-recent-thought-block
  "Render the most-recent iteration's :thinking text under <recent_thought>.
   Hard-capped at RECENT_THOUGHT_MAX_CHARS to keep the projection bounded.

   This replaces the old <prior_thinking> wrapper. The semantic shift:
   <recent_thought> is explicitly the *current* tactical step's free-form
   text; the strategic frame lives in <plan> + <breadcrumbs> instead."
  [thinking]
  (when (and (string? thinking) (not (str/blank? thinking)))
    (str "<recent_thought>\n"
      (truncate thinking RECENT_THOUGHT_MAX_CHARS)
      "\n</recent_thought>")))

(defn- format-system-state-block
  "Render the always-present <system_state> block: SYSTEM vars
   (QUERY / ANSWER / REASONING, registered in `sci-env/SYSTEM_VAR_NAMES`)
   plus the bounded PRIOR_TURN digest. PRIOR_TURN is a digest
   pseudo-name, not a sandbox binding.

   Bounded by construction:
   - SYSTEM-var values truncated to 500 chars each.
   - PRIOR_TURN ships only `:goal :counts :outcome :abandon-reason`."
  [{:keys [system-vars prior-turn]}]
  (let [vars-lines (->> [["QUERY"     (:QUERY system-vars)]
                         ["ANSWER"    (:ANSWER system-vars)]
                         ["REASONING" (:REASONING system-vars)]]
                     (keep (fn [[label v]]
                             (when (some? v)
                               (str "  " label "  "
                                 (safe-pr-str v {:max-chars 500
                                                 :print-length 32
                                                 :print-level 5}))))))
        prior-lines (when (seq prior-turn)
                      [(str "  PRIOR_TURN {:goal "
                         (safe-pr-str (or (:goal prior-turn) "")
                           {:max-chars 200 :print-length 16})
                         " :outcome " (or (:outcome prior-turn) :unknown)
                         (when (:abandon-reason prior-turn)
                           (str " :abandon-reason "
                             (safe-pr-str (:abandon-reason prior-turn)
                               {:max-chars 300})))
                         (when (:counts prior-turn)
                           (str " :counts "
                             ;; counts is a tiny `{:done N :pending N :…}`
                             ;; map; bounded by construction. pr-str fine.
                             (pr-str (:counts prior-turn))))
                         "}")])
        all-lines (concat vars-lines prior-lines)]
    (when (seq all-lines)
      (str "<system_state>\n" (str/join "\n" all-lines) "\n</system_state>"))))

;; -- Cross-field plan validation -----------------------------------------
;;
;; svar's spec engine validates structural shape; the cross-field
;; rules (≤20 items, exactly-one :in-progress, monotonic ids) are
;; checked here in Clojure and surfaced as a structured error map
;; that the iteration loop renders into a `[system_nudge]` line via
;; `format-loop-nudge`.

(declare item-status-key)

(defn validate-plan-state
  "Returns nil when the plan is valid, or an error map shaped as
   `{:type :message :data}`. Side-effect-free; the caller hands the
   result to `format-loop-nudge` to render the user-facing string."
  [plan-state]
  (when (map? plan-state)
    (let [items (or (:items plan-state) [])
          in-progress-items (filter #(= :in-progress (item-status-key %)) items)
          ids (mapv :id items)]
      (cond
        (> (count items) PLAN_MAX_ITEMS)
        {:type :vis/plan-too-large
         :message (str "<plan>.items has " (count items)
                    " entries; max is " PLAN_MAX_ITEMS
                    ". Merge or drop items.")
         :data {:item-count (count items) :max PLAN_MAX_ITEMS}}

        (> (count in-progress-items) 1)
        {:type :vis/plan-multiple-in-progress
         :message (str "<plan> has " (count in-progress-items)
                    " :in-progress items. Set exactly one :in-progress at a time.")
         :data {:in-progress-ids (mapv :id in-progress-items)}}

        (and (seq ids) (not= ids (sort ids)))
        {:type :vis/plan-non-monotonic-ids
         :message "<plan>.items :id values must be monotonic and unique. Don't reuse ids."
         :data {:ids ids}}))))

(defn item-status-key
  "Normalize a plan item's :status to a canonical keyword
   (`:pending`, `:in-progress`, `:done`, `:blocked`). Tolerates both
   keyword and string inputs."
  [item]
  (let [status (:status item)
        status-keyword
        (cond
          (keyword? status) (keyword (str/replace (name status) "_" "-"))
          (string? status)  (keyword (str/replace status "_" "-"))
          :else :pending)]
    (case status-keyword
      :in-progress :in-progress
      status-keyword)))

;; ---------------------------------------------------------------------------
;; Loop nudge formatter — the user-facing string the model sees on the
;; NEXT iteration when the loop rejects the prior iteration's output
;; (PEV gate, plan validation, etc.). Mirrors `format-iteration-error`
;; for trace `:error` entries: takes a structured violation map and
;; produces the `[system_nudge]`-prefixed directive line.
;; ---------------------------------------------------------------------------

(defn format-loop-nudge
  "Render a loop-level violation map into the `[system_nudge]`-prefixed
   directive line. Returns nil for nil input so callers can `(keep
   format-loop-nudge …)` over a sparse vec.

   Recognized `:type` values supply a canonical message body when
   `:message` is absent; arbitrary `:type` falls through to a generic
   `Loop violation: <type>` line."
  [{:keys [type message data] :as violation}]
  (when violation
    (let [body (or message
                 (case type
                   :vis/incomplete-plan-on-answer
                   (let [open-ids (:open-item-ids data)]
                     (str "Cannot finalize: plan items "
                       (str/join ", " (map #(str "[" % "]") open-ids))
                       " are :in-progress / :pending. Either complete them with cited "
                       "evidence (set :status :done with :evidence iN.K), or set "
                       ":abandon-reason describing what blocks completion."))

                   :vis/plan-churn
                   (str "You have changed the plan " (:edit-distance data)
                     " times across " (:iteration-count data) " iterations. Commit"
                     " to ONE approach. If reality has changed enough that this many"
                     " edits made sense, set :abandon-reason and re-emit a single"
                     " fresh plan with the lessons learned in :decided.")

                   :vis/low-confidence-on-answer
                   ":answer with :confidence :low requires :abandon-reason describing what would raise confidence."

                   :vis/consecutive-errors
                   (str (:count data) " consecutive errors. The current approach is"
                     " not working. Re-emit :plan with a different strategy, or set"
                     " :abandon-reason describing what blocks progress.")

                   (str "Loop violation: " (or type :unknown))))]
      (str "[system_nudge] " body))))

;; ---------------------------------------------------------------------------
;; Plan diff — powers the plan-edit-distance metric and the
;; <breadcrumbs> annotation when a plan changes between iterations.
;; ---------------------------------------------------------------------------

(defn compute-plan-diff
  "Diff two plan-state maps. Returns nil when identical, or a map
   `{:added [ids] :removed [ids] :status-changed [{id :from :to}] :goal-changed? bool}`.
   Callers compute `(count added)+(count removed)+(count status-changed)`
   for the `plan-edit-distance` metric."
  [previous-plan new-plan]
  (when (or (some? previous-plan) (some? new-plan))
    (let [previous-items (into {} (map (juxt :id identity) (or (:items previous-plan) [])))
          new-items  (into {} (map (juxt :id identity) (or (:items new-plan) [])))
          previous-ids   (set (keys previous-items))
          new-ids    (set (keys new-items))
          added      (sort (clojure.set/difference new-ids previous-ids))
          removed    (sort (clojure.set/difference previous-ids new-ids))
          shared     (sort (clojure.set/intersection previous-ids new-ids))
          status-changes (vec
                           (keep (fn [id]
                                   (let [from (item-status-key (get previous-items id))
                                         to   (item-status-key (get new-items id))]
                                     (when (not= from to)
                                       {:id id :from from :to to})))
                             shared))
          goal-changed? (not= (:goal previous-plan) (:goal new-plan))
          changed? (or (seq added) (seq removed) (seq status-changes) goal-changed?)]
      (when changed?
        {:added (vec added)
         :removed (vec removed)
         :status-changed status-changes
         :goal-changed? goal-changed?}))))

(defn plan-edit-distance
  "Sum of additions, removals, and status changes. 0 → plan unchanged."
  [diff]
  (if (nil? diff)
    0
    (+ (count (:added diff))
      (count (:removed diff))
      (count (:status-changed diff)))))

(defn assemble-initial-messages [{:keys [system-prompt initial-user-content history-messages]}]
  (into [{:role "system" :content system-prompt}
         {:role "user"   :content initial-user-content}]
    (or history-messages [])))

(defn trim-to-initial-history [messages initial-count]
  (vec (take initial-count messages)))

(defn- read-var-index-str
  "Lazily build (and cache) the `<var_index>` body for the active env.
   Returns nil when the env has no SCI context (test fixtures that exercise
   only the projection layer)."
  [environment]
  (when-let [sci-ctx (:sci-ctx environment)]
    (let [var-index-atom (or (:var-index-atom environment)
                           (atom {:index nil :revision -1 :current-revision 0}))
          {:keys [index revision current-revision]} @var-index-atom]
      (if (= revision current-revision)
        index
        (let [sandbox-map (get-in @(:env sci-ctx)
                            [:namespaces 'sandbox])
              idx         (sci-env/build-var-index
                            sci-ctx (:initial-ns-keys environment)
                            sandbox-map
                            (:db-info environment) (:conversation-id environment)
                            nil)
              live-rev    (:current-revision @var-index-atom)]
          (swap! var-index-atom assoc :index idx :revision live-rev)
          idx)))))

;; -- Sticky-plan loader ---------------------------------------
;;
;; The plan slot is sticky: the model writes it once and the loop carries
;; the most-recent persisted plan forward across iterations until the model
;; re-emits one. This loader returns the latest non-nil plan_state from the
;; current query's iteration rows; nil when no plan has ever been emitted.

(defn load-effective-plan
  "Most-recent persisted :plan-state for this query, or nil. Reads the
   iteration rows in DB-time order; takes the last row whose plan-state
   is non-nil. This is the implementation of sticky-carry semantics:
   when iteration K omits :plan, we still know what the model's plan was."
  [db-info query-id]
  (when (and db-info query-id)
    (try
      (let [iters (db/db-list-query-iterations db-info query-id)]
        (some :plan-state (reverse iters)))
      (catch Throwable t
        (tel/log! {:level :warn
                   :data {:error (ex-message t) :query-id query-id}
                   :msg "load-effective-plan failed"})
        nil))))

(defn load-breadcrumb-chain
  "Vector of `{:position int :breadcrumb str}` for this query, oldest-first,
   capped at last `BREADCRUMBS_KEEP_LAST` entries. Skips iters with no
   breadcrumb so the chain is dense."
  [db-info query-id]
  (when (and db-info query-id)
    (try
      (let [iters (db/db-list-query-iterations db-info query-id)]
        (->> iters
          (keep-indexed (fn [idx it]
                          (when (and (:breadcrumb it)
                                  (not (str/blank? (:breadcrumb it))))
                            {:position idx :breadcrumb (:breadcrumb it)})))
          (take-last BREADCRUMBS_KEEP_LAST)
          vec))
      (catch Throwable t
        (tel/log! {:level :warn
                   :data {:error (ex-message t) :query-id query-id}
                   :msg "load-breadcrumb-chain failed"})
        []))))

(defn load-prior-turn-digest
  "Bounded handover digest for the previous query in the same conversation.
   Returns nil when there is no prior turn (first turn of conversation).
   Replaces the old `build-cross-query-handover` which shipped raw
   thinking strings; PLAN.md §5.5 caps this at goal/counts/outcome only."
  [db-info conversation-id current-query-id]
  (when (and db-info conversation-id)
    (try
      (let [all-queries (sort-by :created-at
                          (db/db-list-conversation-queries db-info conversation-id))
            prior       (last (remove #(= (:id %) current-query-id) all-queries))]
        (when prior
          (let [final-plan   (load-effective-plan db-info (:id prior))
                items        (or (:items final-plan) [])
                bucket       (frequencies (map item-status-key items))
                outcome      (or (:prior-outcome prior)
                               ;; Fallback when :prior-outcome is missing — derive
                               ;; from query status.
                               (case (:status prior)
                                 :done :complete
                                 :error :error
                                 :interrupted :cancelled
                                 :unknown))
                abandon-reason (when (and (= outcome :abandoned)
                                       (not (str/blank? (str (:answer prior)))))
                                 (str (:answer prior)))]
            {:goal           (:goal final-plan)
             :counts         (-> {}
                               (assoc :done (or (:done bucket) 0))
                               (assoc :pending (or (:pending bucket) 0))
                               (assoc :in-progress (or (:in-progress bucket) 0))
                               (assoc :blocked (or (:blocked bucket) 0)))
             :outcome        outcome
             :abandon-reason abandon-reason})))
      (catch Throwable t
        (tel/log! {:level :warn
                   :data {:error (ex-message t) :conversation-id conversation-id}
                   :msg "load-prior-turn-digest failed"})
        nil))))

;; ---------------------------------------------------------------------------
;; Nudges — per-iteration system hints injected into the iteration context
;; ---------------------------------------------------------------------------

;; A single repeat is enough signal: with <attempts> in the projection,
;; a single repeat is enough signal that the model should change strategy.
(def ^:private REPETITION_THRESHOLD 1)

(defn- repetition-warning
  [call-counts-atom previous-expressions]
  (when (and call-counts-atom (seq previous-expressions))
    ;; Fingerprint ignores the result content — we only care whether the
    ;; SAME `:code` was issued repeatedly. Hashing the result was a
    ;; bug: identical calls with non-deterministic results (timestamps,
    ;; uuids) used to slip past the dedup check, AND a 1MB result was
    ;; pr-str'd into the fingerprint string every iteration. Code-only
    ;; key is both correct and cheap.
    (let [keys* (mapv (fn [{:keys [code error]}]
                        (if error
                          [:error-only (str/trim (str error))]
                          [:code-only (str/trim (str code))]))
                  previous-expressions)
          max-count (swap! call-counts-atom
                      (fn [m]
                        (reduce (fn [acc k] (update acc k (fnil inc 0)))
                          (or m {}) keys*)))
          seen (apply max 0 (map #(get max-count % 0) keys*))]
      (when (>= seen REPETITION_THRESHOLD)
        "[system_nudge] You are repeating the same expression pattern. Change strategy."))))

(defn- collect-extension-nudges
  "Invoke `:ext/nudge-fn` on every PRECOMPUTED active extension and
   collect the resulting strings. Activation-fn is NOT evaluated here —
   `extensions` is expected to be the seq returned by
   `loop-core/active-extensions` once at query start. This keeps
   activation single-fire-per-query even though nudges fire per-iteration."
  [extensions ctx]
  (when (seq extensions)
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
      extensions)))

;; ---------------------------------------------------------------------------
;; Iteration context builder
;; ---------------------------------------------------------------------------

(defn build-iteration-context
  "Assemble the per-iteration trailing user message.

   Layered shape:
     <plan>          — sticky structured TODO list (PLAN_STATE_SPEC)
     <breadcrumbs>   — last K=20 one-liners, oldest-first
     <recent>        — last iteration's expressions with iN.K addressable ids
     <recent_thought> — last iteration's :thinking text (≤4000c)
     <system_state>  — QUERY/ANSWER/REASONING + ITERATION pointer + PRIOR_TURN digest
     <var_index>     — user-defined vars
     [system_nudge]  — loop / budget / repetition / extension nudges

   Note: the per-iteration pointer lives in
   `<system_state>.ITERATION` so all loop-managed pointers live in
   one coherent block instead of fragmented across the projection.

   Required opts:
     `:active-extensions` — vec returned by `(loop-core/active-extensions env)`.
        Caller MUST compute this exactly ONCE per query and thread it through
        every iteration. This fn does NOT re-evaluate `:ext/activation-fn`.

   Optional:
     `:iteration`, `:plan-state`, `:breadcrumbs`,
     `:expressions-by-iteration`, `:recent-thought`, `:system-vars`,
     `:prior-turn`, `:call-counts-atom`,
     `:loop-nudges` (vec of strings; gate-violation / plan-validation
        directives that the loop wants to surface to the model on the
        NEXT iteration —). Each entry is
        rendered as a `[system_nudge] …` line, prefix added if
        missing.

   Returns the joined block or nil when every component is blank."
  [environment {:keys [iteration
                       plan-state breadcrumbs attempts
                       expressions-by-iteration recent-thought
                       system-vars prior-turn
                       call-counts-atom
                       loop-nudges
                       active-extensions] :as opts}]
  (when-not (contains? opts :active-extensions)
    (throw (ex-info "build-iteration-context requires :active-extensions — compute once per query via (loop-core/active-extensions env)"
             {:type :vis/missing-active-extensions})))
  (let [plan-block (format-plan-block plan-state)
        breadcrumbs-block (format-breadcrumbs-block breadcrumbs)
        attempts-block (format-attempts-block attempts)
        recent-block (format-recent-block expressions-by-iteration)
        ;; Repetition warning still consumes flat (last-iteration) expressions —
        ;; pluck them out of the canonical pairs once.
        last-iteration-expressions (some-> expressions-by-iteration last second)
        recent-thought-block (format-recent-thought-block recent-thought)
        system-state-block (format-system-state-block
                             {:system-vars system-vars
                              :prior-turn  prior-turn})
        var-index-str (read-var-index-str environment)
        var-block (when (and (string? var-index-str)
                          (not (str/blank? var-index-str)))
                    (str "<var_index>\n" var-index-str "\n</var_index>"))
        nudge-ctx {:environment          environment
                   :iteration            iteration
                   :previous-expressions last-iteration-expressions
                   :plan-state           plan-state
                   :user-var-count       0}
        ;; Loop nudges (gate-violation, plan-validation, etc.) are
        ;; injected by the iteration loop based on the PRIOR iteration's
        ;; outcome; rendered as `[system_nudge]` lines so the model
        ;; sees them with the same UX as built-in/extension nudges.
        loop-nudge-lines (->> (or loop-nudges [])
                           (keep (fn [n]
                                   (when (and (string? n) (not (str/blank? n)))
                                     (if (str/starts-with? n "[system_nudge]")
                                       n
                                       (str "[system_nudge] " n))))))
        built-in-nudges (keep identity
                          [(repetition-warning call-counts-atom last-iteration-expressions)])
        ext-nudges (collect-extension-nudges active-extensions nudge-ctx)
        all-nudges (concat loop-nudge-lines built-in-nudges ext-nudges)
        nudges-block (when (seq all-nudges)
                       (str/join "\n" all-nudges))
        parts (keep (fn [p] (when (and (string? p) (not (str/blank? p))) p))
                [plan-block
                 breadcrumbs-block
                 attempts-block
                 recent-block
                 recent-thought-block
                 system-state-block
                 var-block
                 nudges-block])]
    (when (seq parts)
      (str/join "\n" parts))))

;; ---------------------------------------------------------------------------
;; Error normalization
;; ---------------------------------------------------------------------------

(def ^:private INFRASTRUCTURE_ERROR_TYPES
  #{:svar.llm/all-providers-exhausted
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
          " …<+" (- (count s) LAST_USER_PREVIEW_CHARS) " chars>")
        s))))

(defn- exception->iteration-error-data
  "Normalize an exception into the iteration-error-data map stored on the query row.
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
   Infrastructure failures re-throw; others return `{::iteration-error …}`."
  [^Throwable e ctx]
  (let [ex-data-map (ex-data e)
        iteration (:iteration ctx)]
    (if (infrastructure-error? ex-data-map)
      (do (tel/log! {:level :error
                     :data  (assoc (format-exception-short e) :iteration iteration)}
            "Provider infrastructure error — aborting iteration loop")
        (throw e))
      (let [iteration-error-data (exception->iteration-error-data e ctx)]
        (tel/log! {:level :warn
                   :data (assoc (format-exception-short e) :iteration iteration)}
          "RLM iteration failed, feeding error to LLM")
        {::iteration-error iteration-error-data}))))

;; ---------------------------------------------------------------------------
;; get-locals (read sandbox vars)
;; ---------------------------------------------------------------------------

(defn get-locals
  "Returns {sym → val} of user-defined vars in SCI sandbox."
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

;; ---------------------------------------------------------------------------
;; Noop expression filter
;; ---------------------------------------------------------------------------

(def ^:private noop-exprs
  "Expressions the LLM emits only to satisfy the 'must return code' constraint.
   These carry no information — filter them before storage and display."
  #{":ok" ":ok\n" "nil" ":noop"})

(defn- noop-expr?
  "True when an expression is a structural noop (e.g. `:ok`)."
  [expr]
  (contains? noop-exprs (str/trim (str expr))))

(defn- strip-noop-expressions
  "Remove noop expressions from a vec. Returns nil-safe vec."
  [expressions]
  (vec (remove #(noop-expr? (:code %)) (or expressions []))))

;; ---------------------------------------------------------------------------
;; run-iteration
;; ---------------------------------------------------------------------------

(defn run-iteration
  "Runs a single RLM iteration: ask! → check final → execute code.
   Returns map with :thinking :expressions :final-result :api-usage etc.

   Optional `:dedup-cache-atom` is the per-query cache that skips
   re-execution of identical canonical forms. The handler builds
   `iteration-id`s as `iN.K` (1-based) and threads them into each
   `execute-code` call so cache hits carry a stable `:cached-from`
   reference."
  [environment messages & [{:keys [iteration-spec routing iteration reasoning-level resolved-model on-chunk
                                   dedup-cache-atom]
                            :or {iteration-spec ITERATION_SPEC_NON_REASONING}}]]
  (binding [*rlm-ctx* (merge *rlm-ctx* {:rlm-phase :run-iteration})]
    (let [effective-reasoning (when (some? reasoning-level)
                                (or (normalize-reasoning-level reasoning-level)
                                  (throw (ex-info "Invalid :reasoning-level."
                                           {:type :vis/invalid-reasoning-level
                                            :got reasoning-level}))))
          ;; Stream reasoning chunks to the TUI while the LLM is thinking
          streaming-fn (when on-chunk
                         (fn [{:keys [reasoning done?]}]
                           (when (or (some? reasoning) done?)
                             (on-chunk {:iteration iteration
                                        :thinking  (some-> reasoning str)
                                        :code      nil
                                        :done?     (boolean done?)}))))
          ask-result (binding [llm/*log-context* {:query-id (:environment-id environment) :iteration iteration}]
                       (llm/ask! (:router environment)
                         (cond-> {:spec iteration-spec
                                  :messages messages
                                  :routing (or routing {})
                                  :check-context? false}
                           effective-reasoning (assoc :reasoning effective-reasoning)
                           streaming-fn       (assoc :on-chunk streaming-fn))))
          parsed (:result ask-result)
          model-reasoning (:reasoning ask-result)
          thinking (or model-reasoning (:thinking parsed))
          _ (log-stage! :llm-response iteration
              {:has-reasoning (some? model-reasoning)
               :has-final (some? (:final parsed))
               :code-count (count (:code parsed))
               :duration-ms (:duration-ms ask-result)
               :tokens (:tokens ask-result)
               :thinking thinking})
          next-hint (:next parsed)
          next-model (when-let [m (:model next-hint)] (keyword m))
          next-reasoning (normalize-reasoning-level (:reasoning next-hint))
          api-usage {:prompt_tokens (get-in ask-result [:tokens :input] 0)
                     :completion_tokens (get-in ask-result [:tokens :output] 0)
                     :completion_tokens_details {:reasoning_tokens (get-in ask-result [:tokens :reasoning] 0)}
                     :prompt_tokens_details {:cached_tokens (get-in ask-result [:tokens :cached] 0)}}
          ;; �� surface plan slot fields from svar parse so
          ;; downstream `store-iteration!` can persist them. Keep keys
          ;; absent when blank/missing.
          plan-state-raw  (:plan parsed)
          breadcrumb-raw  (:breadcrumb parsed)
          abandon-reason  (some-> (:abandon-reason parsed) str str/trim not-empty)]
      (if-let [raw-final-answer (:answer parsed)]
        ;; FINAL path
        (let [raw-code (or (:code parsed) [])
              code-entries (vec (keep (fn [block]
                                        (when (map? block)
                                          (let [expr (str (:expr block ""))
                                                time-ms (:time-ms block)
                                                doc     (:doc block)]
                                            (when-not (str/blank? expr)
                                              (cond-> {:expr expr
                                                       :time-ms (or time-ms (throw (ex-info "Code block missing :time-ms" {:expr expr})))}
                                                doc (assoc :doc doc))))))
                                  raw-code))
              code-blocks (mapv :expr code-entries)
              expression-results (when (seq code-blocks)
                                   (mapv (fn [position {:keys [expr time-ms doc]}]
                                           (if-let [err (literal-code-block-error expr)]
                                             {:result nil :error err
                                              :stdout "" :stderr "" :execution-time-ms 0}
                                             (execute-code environment expr
                                               :timeout-ms time-ms :doc doc
                                               :dedup-cache-atom dedup-cache-atom
                                               :iteration-id (str "i" iteration "." (inc position)))))
                                     (range)
                                     code-entries))
              expression-errors (when expression-results
                                  (seq (clojure.core/filter :error expression-results)))
              raw-answer (str raw-final-answer)
              locals (try (get-locals environment) (catch Throwable _ {}))
              ;; ONE rendering mode: every :answer is a Mustache
              ;; template against sandbox vars. Plain text / markdown
              ;; with no `{{...}}` tags renders verbatim, so prose
              ;; passes through unchanged. To inject a computed
              ;; value, the model defs it in :code (`(def x ...)`)
              ;; and references `{{x}}` in :answer. No second mode,
              ;; no auto-detect heuristics, no :answer-type field.
              mustache-result (when-not expression-errors
                                (try
                                  {:answer (mustache/render raw-answer locals)}
                                  (catch Exception e
                                    {:error (str "Mustache error: " (.getMessage e)
                                              ". Define all referenced vars in :code first.")})))
              mustache-answer  (:answer mustache-result)
              mustache-missing (:error mustache-result)
              final-answer (or mustache-answer raw-answer)
              confidence (or (:confidence parsed) :high)
              validation-error (or (when expression-errors
                                     (vis-error/final-answer-code-error-message (:error (first expression-errors))))
                                 mustache-missing)
              expressions (when expression-results
                            (mapv (fn [idx code result]
                                    {:id idx :code code
                                     :result (:result result) :stdout (:stdout result)
                                     :stderr (:stderr result) :error (:error result)
                                     :execution-time-ms (:execution-time-ms result)
                                     :repaired? (:repaired? result)
                                     :autobind-events (:autobind-events result)})
                              (range) code-blocks expression-results))]
          (if validation-error
            {:thinking thinking
             :next-model next-model :next-reasoning next-reasoning
             :expressions (or expressions
                            [{:id 0
                              :code vis-error/final-answer-validation-code-placeholder
                              :result nil :stdout "" :stderr ""
                              :error validation-error}])
             :final-result nil :api-usage api-usage
             :duration-ms (or (:duration-ms ask-result) 0)
             :llm-messages messages :llm-model (str resolved-model)}
            (let [final-result (cond-> {:final? true
                                        :answer final-answer
                                        :confidence confidence}
                                 (:reasoning parsed)  (assoc :reasoning (:reasoning parsed))
                                 abandon-reason       (assoc :abandon-reason abandon-reason))]
              (cond-> {:thinking thinking
                       :next-model next-model :next-reasoning next-reasoning
                       :expressions (strip-noop-expressions expressions)
                       :final-result final-result :api-usage api-usage
                       :duration-ms (or (:duration-ms ask-result) 0)
                       :llm-messages messages :llm-model (str resolved-model)}
                plan-state-raw (assoc :plan-state plan-state-raw)
                breadcrumb-raw (assoc :breadcrumb breadcrumb-raw)))))
        ;; Normal path: execute code blocks
        (let [normalized (vec (:code parsed))
              ;; Coalesce fragments: join unbalanced blocks with the next
              coalesced (loop [remaining normalized
                               result []]
                          ;; Without form-repair, pass blocks through as-is
                          (if (empty? remaining)
                            result
                            (recur (rest remaining) (conj result (first remaining)))))
              total-blocks (count coalesced)
              executed (mapv (fn [idx {:keys [expr time-ms doc]}]
                               (log-stage! :code-exec iteration
                                 {:idx (inc idx) :total total-blocks :code expr :time-ms time-ms
                                  :doc? (boolean doc)})
                               (let [iteration-id (str "i" iteration "." (inc idx))
                                     result (if-let [err (literal-code-block-error expr)]
                                              {:result nil :error err :stdout "" :stderr "" :execution-time-ms 0}
                                              (let [r (execute-code environment expr
                                                        :timeout-ms time-ms :doc doc
                                                        :dedup-cache-atom dedup-cache-atom
                                                        :iteration-id iteration-id)]
                                                (log-stage! :code-result iteration
                                                  {:idx (inc idx) :total total-blocks
                                                   :execution-time-ms (:execution-time-ms r)
                                                   :error (:error r) :timeout? (:timeout? r) :result (:result r)})
                                                r))]
                                 {:block expr :result result}))
                         (range) coalesced)
              code-blocks (mapv :block executed)
              expression-results (mapv :result executed)
              expressions (mapv (fn [idx code result]
                                  {:id idx
                                   :code code
                                   :result (:result result)
                                   :stdout (:stdout result)
                                   :stderr (:stderr result)
                                   :error (:error result)
                                   :execution-time-ms (:execution-time-ms result)
                                   :timeout? (:timeout? result)
                                   :repaired? (:repaired? result)
                                   :autobind-events (:autobind-events result)})
                            (range) code-blocks expression-results)]
          (cond-> {:thinking thinking
                   :next-model next-model :next-reasoning next-reasoning
                   :expressions (strip-noop-expressions expressions) :final-result nil :api-usage api-usage
                   :duration-ms (or (:duration-ms ask-result) 0)
                   :llm-messages messages :llm-model (str resolved-model)}
            plan-state-raw (assoc :plan-state plan-state-raw)
            breadcrumb-raw (assoc :breadcrumb breadcrumb-raw)))))))
