(ns com.blockether.vis.internal.loop
  "vis iteration runtime + conversations — the library surface.

   Owns:
     - Mustache template renderer for RLM answers
     - Query runtime settings (`*rlm-context*`, `*eval-timeout-ms*`, …)
     - Single-iteration runner (`run-iteration`)
     - Multi-iteration query engine + router cache (`query!`,
       `get-router`, `rebuild-router!`)
     - Environment lifecycle (`create-environment`, `dispose-environment!`)
     - Conversation env cache (`create!`, `by-id`, `by-channel`,
       `send!`, `close!`, `delete!`, …)

   The binary entry point (`-main`), the persistence-backed Telemere
   `:db` handler, the one-shot agent helper, and every built-in CLI
   command live in `com.blockether.vis.internal.main`. Channels and embedded
   consumers reach this namespace directly for `send!` / `query!` /
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
   [clojure.string :as str]
   [com.blockether.anomaly.core :as anomaly]
   [com.blockether.svar.core :as svar]
   [com.blockether.svar.internal.llm :as svar-llm]
   [com.blockether.svar.internal.router :as svar-router]
   [com.blockether.svar.internal.util :as util]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.error :as error]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.prompt :as prompt]
   [edamame.core :as edamame]
   [sci.core :as sci]
   [taoensso.telemere :as tel])
  (:import
   [com.oakmac.parinfer Parinfer ParinferResult]
   [com.samskivert.mustache Mustache Mustache$Collector Mustache$Compiler
    Mustache$Formatter Mustache$VariableFetcher]
   [java.util.concurrent ConcurrentHashMap Semaphore]))

(declare rebuild-router! refresh-cached-routers! try-rescue-parse-error custom-bindings auto-forget-stale-vars!
  set-title! set-title-with-broadcast! env-for)

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
        new-cfg {:providers updated}]
    (config/save-config! new-cfg)
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
   30 minutes — anything longer is a bug, not a feature."
  (* 30 60 1000))

(def ^:dynamic *eval-timeout-ms*
  "Dynamic timeout in milliseconds for SCI code evaluation. Bound per query!
   call via :eval-timeout-ms. Nested queries inherit the outer binding.
   Clamped at the query API boundary to
   [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS]."
  DEFAULT_EVAL_TIMEOUT_MS)

(defn clamp-eval-timeout-ms
  "Clamp a candidate eval timeout to [MIN_EVAL_TIMEOUT_MS, MAX_EVAL_TIMEOUT_MS].
   `candidate` may be nil — callers should resolve the fallback before calling.
   Accepts any integer, coerces to long. Prevents runaway SCI futures from
   absurdly high values and sub-second timeouts from absurdly low values."
  [candidate]
  (-> candidate long (max MIN_EVAL_TIMEOUT_MS) (min MAX_EVAL_TIMEOUT_MS)))

;; =============================================================================
;; Concurrency settings
;; =============================================================================

(def DEFAULT_CONCURRENCY
  "Default concurrency settings. Applied when :concurrency is absent from query!."
  {:max-parallel-llm 8
   :http-timeout-ms  20000})

(def ^:dynamic *concurrency*
  "Merged concurrency settings for the current query! process."
  DEFAULT_CONCURRENCY)

;; =============================================================================
;; RLM debug context
;; =============================================================================

(def ^:dynamic *rlm-context*
  "Dynamic context for RLM debug logging. Bind with
   {:rlm-debug? true :rlm-phase :phase-name :rlm-environment-id \"...\"}."
  nil)

;; =============================================================================
;; Mustache template rendering
;; =============================================================================

(defn- clj-fetch
  "Resolve a Mustache variable name against a Clojure context (map/coll).
   Handles dot-path navigation, .size on collections, keyword/string/symbol
   key lookup, and IDeref (atoms, delays)."
  [ctx ^String name]
  (if (str/includes? name ".")
    (let [parts (str/split name #"\.")]
      (reduce (fn [c part]
                (cond
                  (nil? c) (reduced nil)
                  (and (= part "size") (or (sequential? c) (set? c)))
                  (reduced (count c))
                  (map? c)
                  (let [v (or (get c (keyword part))
                            (get c part)
                            (get c (clojure.core/symbol part)))]
                    (if (instance? clojure.lang.IDeref v) @v v))
                  :else (reduced nil)))
        ctx parts))
    (cond
      (and (= name "size") (or (sequential? ctx) (set? ctx)))
      (count ctx)
      (map? ctx)
      (let [v (or (get ctx (keyword name))
                (get ctx name)
                (get ctx (clojure.core/symbol name)))]
        (when (some? v)
          (if (instance? clojure.lang.IDeref v) @v v)))
      :else nil)))

(def ^:private clj-collector
  "jmustache Collector that works natively with Clojure persistent data."
  (reify Mustache$Collector
    (toIterator [_ val]
      (cond
        (sequential? val) (.iterator ^Iterable val)
        (set? val)        (.iterator ^Iterable (vec val))
        (instance? Iterable val) (.iterator ^Iterable val)
        :else (.iterator java.util.Collections/EMPTY_LIST)))
    (createFetcher [_ _ctx _name]
      (reify Mustache$VariableFetcher
        (get [_ ctx name]
          (clj-fetch ctx name))))
    (createFetcherCache [_] (java.util.HashMap.))))

(def ^:private clj-formatter
  "jmustache Formatter for Clojure values — keywords render as name, rest as str."
  (reify Mustache$Formatter
    (format [_ val]
      (cond
        (nil? val)     ""
        (string? val)  val
        (keyword? val) (name val)
        :else          (str val)))))

(def ^:private compiler
  "Shared jmustache compiler configured for Clojure data."
  (-> (Mustache/compiler)
    (.withCollector clj-collector)
    (.withFormatter clj-formatter)))

(defn render
  "Render a Mustache template string against a Clojure map context.
   Throws on missing vars (strict mode) — caller should catch and feed
   the error back to the LLM.

   `template` — Mustache template string.
   `ctx`      — Clojure map (keyword/string/symbol keys). Values may be
                 strings, numbers, booleans, collections, nested maps,
                 atoms, delays."
  ^String [^String template ctx]
  (let [tpl (.compile ^Mustache$Compiler compiler template)]
    (.execute tpl ctx)))
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

(defn normalize-reasoning-level [v]
  (svar/normalize-reasoning-level v))

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

(defn split-top-level-forms
  "Parse `code` (a Clojure source string) into top-level forms. Returns a
   vector of `{:expr str :repaired? bool}` maps, one per form, where
   `:expr` is the verbatim source slice for that form INCLUDING any
   leading `;; comments` and `#_` discards on prior lines (so the
   model's natural `;; what this does\n(def ...)` paragraphing
   survives into `<recent>` instead of getting silently stripped).

   Repair pipeline when the raw source fails edamame:
     1. parinfer indent-mode rebalance → re-parse via edamame;
        on success every form gets `:repaired? true` so the channel
        + dedup layers can see the repair happened.
     2. on second failure return `[nil parse-error]` and let the
        caller (`execute-code`) dispatch to the extension rescue
        chain (`try-extension-parse-rescue`).

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
               form-bounds (mapv (fn [f]
                                   (when-let [m (and (instance? clojure.lang.IObj f) (meta f))]
                                     (let [s (offset-of (:row m) (:col m))
                                           e (offset-of (:end-row m) (:end-col m))]
                                       (when (and s e (>= s 0) (<= e (count src)) (<= s e))
                                         [s e]))))
                             forms)
               ;; For form K, two slices:
               ;;   `:comment` = the GAP (end-of-K-1 .. start-of-K).
               ;;     Captures any `;; …` / `#_(...)` / blank lines
               ;;     that sat between the previous form and this
               ;;     one. Trimmed; nil when empty.
               ;;   `:expr`    = the form's own bounds. The actual
               ;;     Clojure code, no preamble.
               ;; Persisting these as TWO fields (instead of glued
               ;; into one `:expr` blob) keeps the executable code
               ;; clean for hashing / dedup / display while still
               ;; preserving the model's authored prose alongside
               ;; each form.
               comment-slice (fn [idx]
                               (when-let [[start _] (nth form-bounds idx nil)]
                                 (let [prev-end (or (some-> (nth form-bounds (dec idx) nil)
                                                      (nth 1))
                                                  0)]
                                   (when (> start prev-end)
                                     (let [trimmed (str/trim (subs src prev-end start))]
                                       (when (pos? (count trimmed))
                                         trimmed))))))
               expr-slice (fn [idx]
                            (when-let [[start end] (nth form-bounds idx nil)]
                              (subs src start end)))]
           (mapv (fn [idx form]
                   (let [expr-src (or (expr-slice idx)
                                    (binding [*print-meta* false] (pr-str form)))
                         comment  (comment-slice idx)]
                     (cond-> {:expr (str/trim (str expr-src))}
                       comment    (assoc :comment comment)
                       repaired?  (assoc :repaired? true))))
             (range) forms)))]
      (try
        ;; Attempt 1: edamame on raw source.
        [(parse-and-slice code-str false) nil]
        (catch Throwable raw-err
          ;; Attempt 2: parinfer rebalance, then edamame on rebalanced.
          (if-let [rebalanced (parinfer-rebalance code-str)]
            (try
              [(parse-and-slice rebalanced true) nil]
              (catch Throwable _
                ;; Defensive: rebalance was \"clean\" per edamame in
                ;; the inner check, but slicing failed. Fall back to
                ;; the raw error so the caller can try the
                ;; extension chain on the original source.
                [nil (ex-message raw-err)]))
            [nil (ex-message raw-err)]))))))

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
     1. Rewrite parses cleanly             → return rewrite.
     2. Hook returns nil                    → return nil.
     3. Hook returns the input unchanged    → return nil (no progress).
     4. Iteration cap reached               → return nil (bounded).

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
            (= fixed current-code)    nil  ; no progress — bail
            :else
            (let [next-error (parse-clojure-syntax fixed)]
              (if (nil? next-error)
                fixed                       ; parses cleanly — done
                (recur fixed next-error (inc iterations))))))))))

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
  "Count how many entries in `blocks` have a canonical hash that
   already lives in `seen-hashes-atom`, INCLUDING intra-iteration
   duplicates. The seen-set is grown incrementally during the walk:
   `(grep \"X\")` followed by another `(grep \"X\")` in the same iteration
   counts the second occurrence as a duplicate. After counting, the
   atom is updated with the SUCCESSFUL hashes (errors / timeouts are
   NOT recorded — retrying after failure is legitimate).

   Returns `[duplicates total]`."
  [seen-hashes-atom blocks]
  (let [{:keys [duplicates seen]}
        (reduce (fn [{:keys [duplicates seen]} expression]
                  (let [h      (canonical-expression-hash (or (:code expression) ""))
                        is-dup (contains? seen h)
                        ok?    (and (nil? (:error expression))
                                 (not (:timeout? expression)))]
                    {:duplicates (if is-dup (inc duplicates) duplicates)
                     :seen (if (and ok? (not is-dup)) (conj seen h) seen)}))
          {:duplicates 0 :seen @seen-hashes-atom}
          blocks)]
    (reset! seen-hashes-atom seen)
    [duplicates (count blocks)]))

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
  (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :execute-code})]
    (let [start-time (System/currentTimeMillis)
          lint-error (detect-common-mistakes code)
          cached     (dedup-cache-lookup dedup-cache-atom code)]
      (cond
        lint-error
        {:result nil :stdout "" :stderr "" :error lint-error
         :execution-time-ms 0 :timeout? false}

        cached cached

        :else
        (let [parse-error (parse-clojure-syntax code)]
          (if parse-error
            (if-let [rescued (try-extension-parse-rescue environment code parse-error)]
              (let [exec (run-with-timing sci-ctx rescued sandbox-ns timeout-ms start-time)]
                (when (nil? (:error exec))
                  (attach-doc-meta! environment rescued doc)
                  (dedup-cache-record! dedup-cache-atom rescued exec iteration-id))
                (assoc exec
                  :repaired? true
                  :original-code code
                  :original-error parse-error))
              {:result nil :stdout "" :stderr "" :error parse-error
               :execution-time-ms 0 :timeout? false})
            (let [exec (run-with-timing sci-ctx code sandbox-ns timeout-ms start-time)]
              (when (nil? (:error exec))
                (attach-doc-meta! environment code doc)
                (dedup-cache-record! dedup-cache-atom code exec iteration-id))
              exec)))))))

;; Print-cap defaults for `prompt/safe-pr-str` — chosen so a wide flat
;; collection or a deep nested map still pr-strs without materializing
;; an unbounded JVM string before truncation. Override per call site
;; when a tighter or looser bound is required.

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
  "Returns {sym → val} of user-defined vars in the SCI sandbox
   (excludes built-ins / kw-keyed entries). Direct atom read — zero
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

(defn- strip-noop-blocks
  "Remove noop blocks from a vec. Returns nil-safe vec."
  [blocks]
  (vec (remove #(noop-expr? (:code %)) (or blocks []))))

;; ---------------------------------------------------------------------------
;; Answer-scoping helper (Option C)
;;
;; The iteration loop discards a `(answer ...)` call iff the form
;; that ITSELF invoked it errored. Sibling errors (a typo in some
;; OTHER form, a bad vis/edit elsewhere) do NOT gate termination —
;; the model's intent to finalize is honored as long as the answer-
;; bearing form ran cleanly. Pre-Option C the loop discarded on ANY
;; sibling error, which is how a turn could rack up 148 retries with
;; the model repeatedly emitting `(answer ...)` next to a single
;; broken `(def ...)`.
;;
;; Returns the error from the form at `form-idx` in `block-results`
;; or nil when that form's evaluation succeeded. `form-idx` may be
;; nil (legacy answer-atom payloads) or out-of-bounds (defensive
;; against shape drift) — both yield nil (no discard).
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
;; Answer-position contract (rule b' — \"answer is the last form, or the
;; only form\")
;;
;; An iteration that calls `(answer …)` MUST emit it from EITHER:
;;   (i)  the only top-level form, OR
;;   (ii) the last top-level form.
;;
;; In other words: every form preceding the answer call ran first; no
;; form runs AFTER it. Mid-iteration answers are rejected because
;; trailing work would silently discard the answer's intent (\"I said
;; we're done, then I did more stuff\" — incoherent).
;;
;; The check collapses to one comparison: `form-idx` (which top-level
;; form invoked `(answer …)`) must equal `(dec total-forms)`. That
;; naturally subsumes the single-form case (form-idx 0 == count 1 - 1).
;;
;; Structural wrappers — `(let […] (answer …))`, `(do … (answer …))`,
;; `(answer (build))` — stay legal because they are still ONE
;; top-level form (the wrapper) which itself is the last/only form.
;; ---------------------------------------------------------------------------

(defn answer-position-violation?
  "True when `(answer …)` fired from a form that is NOT the last
   top-level form of the iteration. `form-idx` is the 0-based index
   of the form that set `answer-atom`; `total-forms` is the count of
   parsed top-level blocks. Pure; public so loop + tests share
   the rule. Returns false when `form-idx` is nil (no answer fired)."
  [form-idx total-forms]
  (boolean (and form-idx
             (integer? form-idx)
             (not (neg? form-idx))
             (pos? total-forms)
             (not= form-idx (dec total-forms)))))

(defn answer-position-error-message
  "Validation-error string surfaced when `answer-position-violation?`
   fires. Tells the model exactly what's wrong (which form's index
   vs. the required last index, both 1-based for human readability)
   and offers two recovery paths."
  [form-idx total-forms]
  (let [actual-1 (inc (or form-idx 0))]
    (str "(answer …) must be either the ONLY top-level form OR the LAST "
      "top-level form in its iteration. This iteration had " total-forms
      " top-level forms but answer fired from form " actual-1
      "; it must fire from form " total-forms
      ". Either: (a) move the answer call to the end of this iteration, "
      "OR (b) drop trailing forms and emit them as a separate next "
      "iteration before the answer.")))

;; ---------------------------------------------------------------------------
;; Rule c — \"answer in iter 0 must be the ONLY form\"
;;
;; Rule b' allows answer as the last of N top-level forms, but in
;; iteration 0 the model has no prior `<recent>` context. If it
;; emits work-forms followed by `(answer …)` in iter 0, the answer
;; was formed WITHOUT observing the work's results — those land in
;; iter 1's prompt, not iter 0's. The answer is therefore
;; uninformed regardless of whether the work succeeded.
;;
;; Recovery: model can either (a) inline the work into the answer's
;; argument or wrap it in a structural `(let […] (answer …))` so
;; results ARE observed inline before the answer fires (still ONE
;; top-level form, allowed), OR (b) split the work into iter 0 and
;; emit `(answer …)` as the only form of iter 1 once the work's
;; results are visible in `<recent>`.
;;
;; Iter 1+ is unaffected — by then the model has seen at least one
;; feedback round, so multi-form answer iterations carry real
;; information.
;; ---------------------------------------------------------------------------

(defn answer-first-iteration-violation?
  "True when `(answer …)` fired during iteration 0 AND the iteration
   had more than one top-level form. `iteration` is the 0-based
   iteration number; `total-forms` is the count of parsed top-level
   blocks; `answer-fired?` is whether the answer-atom was set.
   Pure; public so loop + tests share the rule."
  [iteration total-forms answer-fired?]
  (boolean (and answer-fired?
             (integer? iteration)
             (zero? iteration)
             (integer? total-forms)
             (> total-forms 1))))

(defn answer-first-iteration-error-message
  "Validation-error string surfaced when
   `answer-first-iteration-violation?` fires. Spells out WHY this is
   wrong (the model didn't observe its own work's results before
   answering) and offers both recovery paths."
  [total-forms]
  (str "(answer …) cannot fire in iteration 0 alongside other top-level "
    "work forms. This iteration had " total-forms " top-level forms; the "
    "answer was discarded. The model has not yet observed the results "
    "of its own work in iteration 0 — those land in iteration 1's <recent>, "
    "not iteration 0's. Either: (a) inline the work into the answer's "
    "argument or wrap it as `(let […] (answer …))` so the iteration has "
    "ONE top-level form whose result IS observed before answer fires, OR "
    "(b) keep the work in iteration 0 and emit (answer …) as the only "
    "form of iteration 1 once the work's results are visible in <recent>."))

;; ---------------------------------------------------------------------------
;; run-iteration
;; ---------------------------------------------------------------------------

(defn run-iteration
  "Runs a single RLM iteration: ask! → check final → execute code.
   Returns map with :thinking :blocks :final-result :api-usage etc.

   Optional `:dedup-cache-atom` is the per-query cache that skips
   re-execution of identical canonical forms. The handler builds
   `iteration-id`s as `iN.K` (1-based) and threads them into each
   `execute-code` call so cache hits carry a stable `:cached-from`
   reference."
  [environment messages & [{:keys [routing iteration reasoning-level resolved-model on-chunk
                                   dedup-cache-atom]}]]
  (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :run-iteration})]
    (let [effective-reasoning (when (some? reasoning-level)
                                (or (normalize-reasoning-level reasoning-level)
                                  (throw (ex-info "Invalid :reasoning-level."
                                           {:type :vis/invalid-reasoning-level
                                            :got reasoning-level}))))
          ;; Reset the per-environment answer-atom before this iteration.
          ;; The SCI sandbox's `(answer "...")` fn `reset!`s it during
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
          ;; thinking. Every chunk carries `:phase` — consumers
          ;; dispatch on it. Phases:
          ;;   :reasoning      — LLM streaming reasoning text
          ;;   :form-result    — one form finished evaluating (per-form)
          ;;   :iteration-final — iteration complete (final-result
          ;;                      or normal end-of-iteration marker)
          streaming-fn (when on-chunk
                         (fn [{:keys [reasoning done?]}]
                           (when (or (some? reasoning) done?)
                             (on-chunk {:phase     :reasoning
                                        :iteration iteration
                                        :thinking  (some-> reasoning str)
                                        :done?     (boolean done?)}))))
          ask-result (binding [svar-llm/*log-context* {:query-id (:environment-id environment) :iteration iteration}]
                       (svar/ask-code! (:router environment)
                         (cond-> {:lang     "clojure"
                                  :messages messages
                                  :routing  (or routing {})
                                  :check-context? false}
                           effective-reasoning (assoc :reasoning effective-reasoning)
                           streaming-fn        (assoc :on-chunk streaming-fn))))
          model-reasoning (:reasoning ask-result)
          thinking model-reasoning
          _ (log-stage! :llm-response iteration
              {:has-reasoning (some? model-reasoning)
               :raw-length    (count (or (:raw ask-result) ""))
               :block-count   (count (or (:blocks ask-result) []))
               :duration-ms   (:duration-ms ask-result)
               :tokens        (:tokens ask-result)
               :thinking      thinking})
          api-usage {:prompt_tokens (get-in ask-result [:tokens :input] 0)
                     :completion_tokens (get-in ask-result [:tokens :output] 0)
                     :completion_tokens_details {:reasoning_tokens (get-in ask-result [:tokens :reasoning] 0)}
                     :prompt_tokens_details {:cached_tokens (get-in ask-result [:tokens :cached] 0)}}
          ;; svar/ask-code! returns the concatenated source string in :result.
          ;; Parse it into top-level forms; each form becomes one
          ;; expression_state row.
          raw-code (or (:result ask-result) "")
          [forms parse-error] (split-top-level-forms raw-code)
          code-entries (if parse-error
                           ;; Whole blob fails to parse — surface as one
                           ;; failed expression with the raw blob as :code.
                         [{:expr (str raw-code) :parse-error parse-error}]
                         (vec (filter #(not (str/blank? (:expr %))) (or forms []))))
          total-blocks (count code-entries)
          executed (mapv (fn [idx {:keys [expr parse-error] form-repaired? :repaired? form-comment :comment}]
                           (log-stage! :code-exec iteration
                             {:idx (inc idx) :total total-blocks :code expr})
                           ;; Stamp form-idx BEFORE eval so any
                           ;; `(answer ...)` call inside this form
                           ;; captures the right index on the
                           ;; answer-atom payload.
                           (reset! current-form-idx-atom idx)
                           (let [iteration-id (str "i" iteration "." (inc idx))
                                 raw-result (cond
                                              parse-error
                                              {:result nil :error (str "Parse error: " parse-error)
                                               :stdout "" :stderr "" :execution-time-ms 0}
                                              :else
                                              (if-let [err (literal-code-block-error expr)]
                                                {:result nil :error err :stdout "" :stderr "" :execution-time-ms 0}
                                                (let [r (execute-code environment expr
                                                          :dedup-cache-atom dedup-cache-atom
                                                          :iteration-id iteration-id)]
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
                                          form-repaired? (assoc :repaired? true))]
                             ;; Per-form streaming chunk (:phase
                             ;; :form-result). Fires the moment a
                             ;; form lands so the channel can render
                             ;; iN.K results incrementally instead
                             ;; of waiting for the whole batch. Same
                             ;; envelope on success and error —
                             ;; consumers branch on `:error nil?`,
                             ;; not on shape.
                             (when on-chunk
                               (on-chunk {:phase             :form-result
                                          :iteration         iteration
                                          :form-idx          idx
                                          :form-of           total-blocks
                                          :iteration-id      iteration-id
                                          :code              expr
                                          :comment           form-comment
                                          :result            (:result result)
                                          :error             (:error result)
                                          :stdout            (:stdout result)
                                          :stderr            (:stderr result)
                                          :execution-time-ms (:execution-time-ms result)
                                          :timeout?          (boolean (:timeout? result))
                                          :repaired?         (boolean (:repaired? result))}))
                             {:block expr :result result :form-comment form-comment}))
                     (range) code-entries)
          code-blocks (mapv :block executed)
          block-results (mapv :result executed)
          block-comments (mapv :form-comment executed)
          blocks (mapv (fn [idx code result form-comment]
                         (cond-> {:id idx
                                  :code code
                                  :result (:result result)
                                  :stdout (:stdout result)
                                  :stderr (:stderr result)
                                  :error (:error result)
                                  :execution-time-ms (:execution-time-ms result)
                                  :timeout? (:timeout? result)
                                  :repaired? (:repaired? result)}
                           form-comment (assoc :comment form-comment)))
                   (range) code-blocks block-results block-comments)]
      (if-let [{:keys [value form-idx]} @answer-atom]
          ;; FINAL path: model called `(answer "...")` during this
          ;; iteration. Atom payload is `{:value :form-idx}`. Three
          ;; gates fire in order:
          ;;
          ;;   1. Rule c (first-iter informedness): in iteration 0,
          ;;      `(answer …)` must be the ONLY top-level form.
          ;;      Multi-form iter-0 answers are rejected because the
          ;;      model has no prior `<recent>` to draw on — the
          ;;      answer was formed WITHOUT observing the iteration's
          ;;      own work, which only lands in iter 1's prompt.
          ;;
          ;;   2. Rule b' (answer position): `(answer …)` must fire
          ;;      from the last (or only) top-level form. Mid-
          ;;      iteration answers are rejected; trailing work
          ;;      after an answer is incoherent.
          ;;
          ;;   3. Option C (form-scoped error gate): if the answer-
          ;;      bearing form's own evaluation errored anyway
          ;;      (e.g. `(do (vis/edit …throws…) (answer "x"))` —
          ;;      the form had inner work that crashed), the answer
          ;;      is discarded with the form's own error. Sibling
          ;;      forms BEFORE the answer-form may error freely; that
          ;;      doesn't gate termination.
          ;;
          ;; `resolved-model` is a MAP — `{:name str :provider kw
          ;; :reasoning? bool}` — not a string. Persisting `(str
          ;; resolved-model)` would land a stringified map in
          ;; `iteration.llm_model`; surface `:name` and `:provider`
          ;; separately so both columns get clean values.
        (let [final-answer    (str value)
              total-forms     (count code-entries)
              first-iter-bad? (answer-first-iteration-violation? iteration total-forms true)
              position-bad?   (when-not first-iter-bad?
                                (answer-position-violation? form-idx total-forms))
              own-form-error  (when (and (not first-iter-bad?) (not position-bad?))
                                (answer-form-error block-results form-idx))
              validation-error (cond
                                 first-iter-bad?
                                 (answer-first-iteration-error-message total-forms)
                                 position-bad?
                                 (answer-position-error-message form-idx total-forms)
                                 own-form-error
                                 (error/final-answer-code-error-message own-form-error))
              ;; Surface the validation error on the answer-bearing
              ;; form's row so the model sees \"my (answer …) was
              ;; rejected because…\" right next to its own code.
              blocks*     (cond-> blocks
                            (and validation-error form-idx
                              (< form-idx (count blocks))
                              (nil? (get-in blocks [form-idx :error])))
                            (assoc-in [form-idx :error] validation-error))
              model-name       (some-> (:name resolved-model) str)
              provider         (:provider resolved-model)]
          (if validation-error
            {:thinking thinking
             :blocks (or (seq blocks*)
                       [{:id 0 :code "(final-answer-validation)"
                         :result nil :stdout "" :stderr ""
                         :error validation-error}])
             :final-result nil :api-usage api-usage
             :duration-ms (or (:duration-ms ask-result) 0)
             :llm-messages messages :llm-provider provider :llm-model model-name}
            {:thinking thinking
             :blocks (strip-noop-blocks blocks)
             :final-result {:final?           true
                            :answer           final-answer
                            ;; Index of the form that called
                            ;; `(answer …)`. Channels use this to
                            ;; ELIDE the answer-bearing form from the
                            ;; per-iteration code trace (the channel
                            ;; renders the answer text below; showing
                            ;; `(answer "...")` above it is
                            ;; redundant prose-as-code).
                            :answer-form-idx  form-idx}
             :api-usage api-usage
             :duration-ms (or (:duration-ms ask-result) 0)
             :llm-messages messages :llm-provider provider :llm-model model-name}))
          ;; Normal path
        {:thinking thinking
         :blocks (strip-noop-blocks blocks)
         :final-result nil :api-usage api-usage
         :duration-ms (or (:duration-ms ask-result) 0)
         :llm-messages messages
         :llm-provider (:provider resolved-model)
         :llm-model    (some-> (:name resolved-model) str)}))))

;; =============================================================================
;; Multi-iteration query engine
;; =============================================================================

;; -----------------------------------------------------------------------------
;; Core helpers
;; -----------------------------------------------------------------------------

(defn- format-iteration-error
  "Render one trace `:error` map as a Markdown bullet for the user.
   Always includes the wrapper message; appends the raw provider
   response when the spec layer captured one (`svar.spec/schema-rejected`
   stashes the literal model output under `:data :raw-data`). Without
   this, errors like \"Your organization does not have access to Claude\"
   were stored in the DB but the user only saw the schema-rejection
   wrapper text. The raw response is the actually-useful part."
  [err]
  (let [message (or (:message err) (str err))
        data    (:data err)
        raw     (some-> (:raw-data data) str)
        recv    (:received-type data)
        body    (when (and raw (not (str/blank? raw)))
                  (truncate raw 400))]
    (cond-> (str "- " message)
      body (str "\n  provider returned"
             (when recv (str " (" recv ")"))
             ": " body))))

(defn- recent-errors-block
  "Render the last `n` trace `:error` entries as a Markdown block
   (\"**Recent provider errors:**\\n\\n- …\"). Returns nil when the
   trace carries no errors so callers can `(when …)` without churn."
  [trace n]
  (let [errs (->> trace reverse (keep :error) (take n))]
    (when (seq errs)
      (str "**Recent provider errors:**\n\n"
        (str/join "\n" (map format-iteration-error errs))
        "\n\n"))))

;; -----------------------------------------------------------------------------
;; Router lifecycle + model helpers (query single-file API)
;; -----------------------------------------------------------------------------

(defonce ^:private router-atom (atom nil))

(defn get-router
  "Get or create the shared LLM router."
  []
  (or @router-atom
    (let [cfg (config/resolve-config)
          r   (svar/make-router (:providers cfg))]
      (reset! router-atom r)
      r)))

(defn reset-router!
  []
  (reset! router-atom nil))

(defn rebuild-router!
  "Rebuild the router from the given config. Used when provider settings change."
  [config]
  (let [r (svar/make-router (:providers config))]
    (reset! router-atom r)
    r))

(defn ask!
  [opts]
  (svar/ask! (get-router) opts))

(defn resolve-effective-model
  "Best-effort root model descriptor from router config.

   The returned map carries `:name` (model id, e.g. \"gpt-4o\") AND
   `:provider` (provider id keyword, e.g. `:openai`) so every caller
   can persist BOTH alongside the model. Earlier versions returned
   just the model map and the provider id was silently dropped on
   the way to the DB — leaving the meta layer with no way to render
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
     ITERATION_PREVIOUS_REASONING — last iteration's :thinking text.
     CONVERSATION_PREVIOUS_ANSWER — latest finalized turn answer; only
                                     bumps when this iteration produced
                                     a `final-result` (= terminal answer
                                     for this turn). For all earlier
                                     iterations of the same turn it
                                     keeps the previous turn's value.
     CONVERSATION_TITLE           — mirrors `:conversation-title-atom`
                                     so a `(conversation-title \"...\")`
                                     inside iteration N is observable
                                     to the model in iteration N+1
                                     without a DB round-trip."
  [environment {:keys [thinking final-result final-answer]}]
  (when (seq thinking)
    (env/bind-and-bump! environment 'ITERATION_PREVIOUS_REASONING thinking))
  (when final-result
    (env/bind-and-bump! environment 'CONVERSATION_PREVIOUS_ANSWER final-answer))
  (when-let [conversation-title-atom (:conversation-title-atom environment)]
    (env/bind-and-bump! environment 'CONVERSATION_TITLE (or @conversation-title-atom ""))))

(defn update-iteration-id!
  "Rebind `ITERATION_ID` in the SCI sandbox to the freshly-persisted
   iteration row's UUID. Mirrors `:current-iteration-id-atom`. No-op
   when `iteration-id` is nil."
  [environment iteration-id]
  (when iteration-id
    (env/bind-and-bump! environment 'ITERATION_ID iteration-id)))

(defn inject-system-var-snapshots
  "Append a SYSTEM-var snapshot to `vars-snapshot` for EVERY name in
   `SYSTEM_VAR_NAMES` on EVERY iteration. The model's
   `(vis/var-history 'X)` then returns ONE row per iteration
   for each X, even when the value is unchanged or blank.

   Yes, turn-frozen vars (TURN_USER_REQUEST, TURN_QUERY_ID,
   TURN_CONVERSATION_SOUL_ID, TURN_CONVERSATION_STATE_ID,
   TURN_SYSTEM_PROMPT, TURN_ACTIVE_EXTENSIONS) repeat verbatim across
   iterations of the same turn — that is the intentional contract:
   \"every iteration carries a snapshot of every SYSTEM var\". The
   dedup-on-unchanged optimization the previous version did was a
   row-saving micro-opt that kept the var-history vec stuck on iter 0
   for those names.

   Each var is normalized to a non-nil string so `expression_state`
   never stores nil for a SYSTEM var — makes the version vec a clean
   log of values across iterations."
  [vars-snapshot {:keys [query thinking final-answer
                         turn-query-id iteration-id
                         conversation-soul-id conversation-state-id
                         system-prompt
                         extensions-snapshot conversation-title]}]
  (let [stamp (fn [vs nm v]
                (conj vs {:name nm :value v :code ";; SYSTEM var"}))]
    (-> vars-snapshot
      (stamp "TURN_USER_REQUEST"            (or query ""))
      (stamp "TURN_QUERY_ID"                (or turn-query-id ""))
      (stamp "TURN_CONVERSATION_SOUL_ID"    (or conversation-soul-id ""))
      (stamp "TURN_CONVERSATION_STATE_ID"   (or conversation-state-id ""))
      (stamp "TURN_SYSTEM_PROMPT"           (or system-prompt ""))
      (stamp "TURN_ACTIVE_EXTENSIONS"       (or extensions-snapshot []))
      (stamp "ITERATION_ID"                 (or iteration-id ""))
      (stamp "ITERATION_PREVIOUS_REASONING" (or thinking ""))
      (stamp "CONVERSATION_TITLE"           (or conversation-title ""))
      (stamp "CONVERSATION_PREVIOUS_ANSWER" (or final-answer "")))))

(defn update-title-system-var!
  "Rebind CONVERSATION_TITLE in the SCI sandbox to whatever the env's
   conversation-title-atom currently holds. Called once at iteration 0
   so the first iteration sees the live title; per-iteration rebinds
   happen in `update-system-vars!` (alongside
   ITERATION_PREVIOUS_REASONING / CONVERSATION_PREVIOUS_ANSWER)."
  [environment]
  (when-let [conversation-title-atom (:conversation-title-atom environment)]
    (env/bind-and-bump! environment 'CONVERSATION_TITLE (or @conversation-title-atom ""))))

;; -----------------------------------------------------------------------------
;; Iteration loop + run-query! (inlined from former base)
;; -----------------------------------------------------------------------------

(def ^:private FRESH_ITER_CARRY
  {:previous-blocks nil :previous-iteration -1})

(def ^:private balanced-reasoning :balanced)

(defn- status->id [status]
  (when status (keyword "rlm.status" (name status))))

(defn iteration-loop
  "The core iteration loop. Runs assemble → ask LLM → execute → persist
   until the model emits `:answer`, the user cancels, or the
   consecutive-error budget is exhausted. There is NO iteration cap.
   If a buggy model never finalizes, the user cancels."
  [environment query
   {:keys [system-prompt
           query-id history-messages
           max-consecutive-errors max-restarts
           hooks cancel-atom current-iteration-atom
           reasoning-default routing]}]
  (let [;; Tightened from 5 to 3. Three consecutive failures is enough
        ;; signal that the current approach is wrong; the nudge fires
        ;; at CONSECUTIVE_ERROR_NUDGE_AT (= 2) so the model gets a
        ;; warning before the strategy-restart kicks in.
        max-consecutive-errors (or max-consecutive-errors 3)
        max-restarts (or max-restarts 3)
        effective-model (:name (resolve-effective-model (:router environment)))
        _ (assert effective-model "Router must resolve a root model")
        has-reasoning? (provider-has-reasoning? (:router environment))
        base-reasoning-level (or (normalize-reasoning-level reasoning-default) balanced-reasoning)
        ;; Activate extensions ONCE per query. Threaded through both the
        ;; system-prompt assembler (cacheable prefix) and the per-iteration
        ;; nudge collector — activation-fn never re-fires inside the loop.
        active-exts   (prompt/active-extensions environment)
        system-prompt (prompt/assemble-system-prompt environment
                        {:system-prompt      system-prompt
                         :active-extensions  active-exts})
        initial-user-content query
        initial-messages (prompt/assemble-initial-messages
                           {:system-prompt system-prompt
                            :initial-user-content initial-user-content
                            :history-messages history-messages})
        usage-atom (atom {:input-tokens 0 :output-tokens 0 :reasoning-tokens 0 :cached-tokens 0})
        accumulate-usage! (fn [api-usage]
                            (when api-usage
                              (swap! usage-atom
                                (fn [acc]
                                  (-> acc
                                    (update :input-tokens + (or (:prompt_tokens api-usage) 0))
                                    (update :output-tokens + (or (:completion_tokens api-usage) 0))
                                    (update :reasoning-tokens + (or (get-in api-usage [:completion_tokens_details :reasoning_tokens]) 0))
                                    (update :cached-tokens + (or (get-in api-usage [:prompt_tokens_details :cached_tokens]) 0)))))))
        call-counts-atom (atom {})
        ;; Phase 2-m measurement: per-query set of canonical hashes
        ;; for SUCCESSFUL blocks. The iteration handler counts how
        ;; many of THIS iteration's blocks were already in the set
        ;; (`:expression-redundancy-fraction` + `:dedup-saves` metadata)
        ;; before adding the new successful ones.
        seen-expression-hashes-atom (atom #{})
        ;; Per-query map of canonical hash -> cached execution result.
        ;; When the model re-issues an identical canonical form,
        ;; `execute-code` returns the cached result with `:cached?
        ;; true` and `:cached-from "iN.K"` instead of re-running SCI.
        ;; Saves SCI eval cost + downstream side effects AND tells the
        ;; model the call was already made so it stops re-issuing.
        dedup-cache-atom (atom {})
        finalize-cost (fn []
                        (let [{:keys [input-tokens output-tokens reasoning-tokens cached-tokens]} @usage-atom
                              total-tokens (+ input-tokens output-tokens)
                              cost (svar-router/estimate-cost effective-model input-tokens output-tokens)]
                          {:tokens {:input input-tokens :output output-tokens
                                    :reasoning reasoning-tokens :cached cached-tokens
                                    :total total-tokens}
                           :cost cost}))
        var-index-atom (or (:var-index-atom environment)
                         (atom {:index nil :revision -1 :current-revision 0}))
        on-iteration (:on-iteration hooks)
        on-chunk (:on-chunk hooks)
        on-cancel (:on-cancel hooks)
        emit-hook! (fn [hook-fn payload log-message]
                     (when hook-fn
                       (try (hook-fn payload)
                         (catch Exception e
                           (tel/log! {:level :warn :data (format-exception-short e)} log-message)))))
        ;; Metadata persisted on each iteration row — reuses the
        ;; precomputed `active-exts` (no second activation pass).
        iteration-metadata (fn []
                             (when (seq active-exts)
                               {:extensions (mapv (fn [ext]
                                                    (cond-> {:namespace (str (:ext/namespace ext))}
                                                      (:ext/version ext) (assoc :version (:ext/version ext))))
                                              active-exts)}))]
    ;; -----------------------------------------------------------------
    ;; Turn-start SYSTEM-var bindings.
    ;;
    ;; Every `TURN_*` here is bound exactly once per turn and never
    ;; mutated again until the next turn opens — the model gets a
    ;; stable view for the entire iteration loop. `ITERATION_*` resets
    ;; here and rebinds per iteration. `CONVERSATION_*` is touched at
    ;; iteration boundaries via `update-system-vars!` /
    ;; `update-title-system-var!`.
    ;; -----------------------------------------------------------------
    (env/bind-and-bump! environment 'TURN_USER_REQUEST query)
    (env/bind-and-bump! environment 'TURN_QUERY_ID query-id)
    (env/bind-and-bump! environment 'TURN_CONVERSATION_SOUL_ID
      (:conversation-id environment))
    (env/bind-and-bump! environment 'TURN_CONVERSATION_STATE_ID
      (persistance/db-latest-conversation-state-id
        (:db-info environment) (:conversation-id environment)))
    ;; The full assembled system prompt that drives THIS turn. SYSTEM
    ;; vars are excluded from `<var_index>` (see `env/build-var-index`)
    ;; so binding a multi-KB string here does NOT enter per-iteration
    ;; prompt context — it is only paid for if the model evaluates the
    ;; symbol explicitly (e.g. to verify what rules it is bound by).
    (env/bind-and-bump! environment 'TURN_SYSTEM_PROMPT system-prompt)
    ;; TURN_ACTIVE_EXTENSIONS = frozen, fully-realized vec describing
    ;; every extension that activated for THIS turn. Built off the same
    ;; `active-exts` we hand to the prompt assembler / nudge collector,
    ;; so the agent's <var_index> picture matches the actually-loaded
    ;; surface.
    (env/bind-and-bump! environment 'TURN_ACTIVE_EXTENSIONS
      (prompt/extensions-snapshot active-exts))
    ;; Reset ITERATION_ID to nil at turn start; rebound by
    ;; `update-iteration-id!` after each iteration row commits.
    (env/bind-and-bump! environment 'ITERATION_ID nil)
    (update-title-system-var! environment)
    (when-let [a (:current-iteration-id-atom environment)] (reset! a nil))
    (when-let [a (:current-query-id-atom environment)] (reset! a query-id))
    (auto-forget-stale-vars! environment)
    (binding [*rlm-context* (merge *rlm-context* {:rlm-phase :iteration-loop})]
      (loop [loop-state (merge {:iteration 0 :messages initial-messages
                                :trace [] :consecutive-errors 0 :restarts 0}
                          FRESH_ITER_CARRY)]
        (let [{:keys [iteration messages trace consecutive-errors restarts
                      previous-blocks previous-iteration]} loop-state]
          (when current-iteration-atom (reset! current-iteration-atom iteration))
          (cond
            (when cancel-atom @cancel-atom)
            (do (log-stage! :error iteration {:reason :cancelled})
              (emit-hook! on-cancel {:iteration iteration :status :cancelled
                                     :status-id (status->id :cancelled)} "on-cancel hook threw")
              (merge {:answer nil :status :cancelled :status-id (status->id :cancelled)
                      :trace trace :iteration-count iteration} (finalize-cost)))

            :else
            (if (>= consecutive-errors max-consecutive-errors)
              (if (< restarts max-restarts)
                (let [failed (->> trace (filter :error) (take 3)
                               (map #(str "- " (get-in % [:error :message] (str (:error %)))))
                               (str/join "\n"))
                      hint (str "Previous attempts failed with these errors:\n" failed
                             "\n\nStart fresh with a DIFFERENT strategy.\n\nOriginal request: " query)
                      messages [{:role "system" :content system-prompt} {:role "user" :content hint}]]
                  (recur (assoc loop-state
                           :iteration (inc iteration) :messages messages
                           :trace trace :consecutive-errors 0 :restarts (inc restarts))))
                (let [errors-block (recent-errors-block trace 3)
                      fallback     (str "Warning: Too many errors (" consecutive-errors ") across "
                                     (inc restarts) " restart(s).\n\n"
                                     ;; Includes the raw provider payload
                                     ;; (e.g. an HTTP plain-text auth rejection) so
                                     ;; the user can act on it instead of guessing.
                                     errors-block)]
                  (merge {:answer fallback
                          :status :error :status-id (status->id :error)
                          :trace trace :iteration-count iteration} (finalize-cost))))

              (let [reasoning-level (when has-reasoning?
                                      (reasoning-level-for-errors base-reasoning-level consecutive-errors))
                    _ (log-stage! :iteration-start iteration {:message-count (count messages) :reasoning reasoning-level})
                    blocks-by-iteration (when (seq previous-blocks)
                                          [[(or previous-iteration 0) previous-blocks]])
                    iteration-context (prompt/build-iteration-context environment
                                        {:blocks-by-iteration blocks-by-iteration
                                         :call-counts-atom         call-counts-atom
                                         :active-extensions        active-exts})
                    base-messages (prompt/trim-to-initial-history messages (count initial-messages))
                    effective-messages (cond-> base-messages
                                         (not (str/blank? iteration-context))
                                         (conj {:role "user" :content iteration-context}))
                    resolved-model (resolve-effective-model (:router environment) (or routing {}))
                    effective-routing (or routing {})
                    iteration-result (try
                                       (run-iteration environment effective-messages
                                         {:iteration iteration :reasoning-level reasoning-level
                                          :routing effective-routing
                                          :resolved-model resolved-model
                                          :on-chunk on-chunk
                                          :dedup-cache-atom dedup-cache-atom})
                                       (catch Exception e
                                         (handle-iteration-exception! e
                                           {:iteration iteration :messages effective-messages
                                            :routing effective-routing :reasoning-level reasoning-level})))]
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
                      (emit-hook! on-cancel {:iteration iteration :status :cancelled
                                             :status-id (status->id :cancelled)}
                        "on-cancel hook threw")
                      (merge {:answer nil :status :cancelled
                              :status-id (status->id :cancelled)
                              :trace trace :iteration-count iteration}
                        (finalize-cost)))
                    (let [error-feedback (str "[Iteration " (inc iteration) "]\n"
                                           "<error>LLM call failed: " (:message iteration-error-data) "</error>\n"
                                           "Adjust your approach or emit :final with what you have.")
                          trace-entry {:iteration iteration :error iteration-error-data :final? false}
                          empty-reasoning (when (= :svar.llm/empty-content (:type iteration-error-data))
                                            (:reasoning (:data iteration-error-data)))
                          err-iteration-id (persistance/db-store-iteration! (:db-info environment)
                                             {:query-id query-id :vars [] :blocks nil
                                              :thinking empty-reasoning :duration-ms 0 :error iteration-error-data
                                              :llm-messages effective-messages
                                              :llm-provider (:provider resolved-model)
                                              :llm-model (str (:name resolved-model))
                                              :metadata (iteration-metadata)})]
                      (when-let [a (:current-iteration-id-atom environment)] (reset! a err-iteration-id))
                      (update-iteration-id! environment err-iteration-id)
                      ;; Live error chunk — `:phase :iteration-error`
                      ;; signals the iteration aborted before any
                      ;; forms could run. No per-form chunks fired
                      ;; this iteration, so the channel sees a clean
                      ;; reasoning -> error transition.
                      (emit-hook! on-chunk
                        {:phase     :iteration-error
                         :iteration iteration
                         :thinking  empty-reasoning
                         :error     iteration-error-data
                         :done?     true}
                        "on-chunk (iteration error)")
                      (emit-hook! on-iteration
                        {:iteration iteration :status :error :status-id (status->id :error)
                         :thinking empty-reasoning :blocks nil :final-result nil
                         :error iteration-error-data :duration-ms 0} "on-iteration (error)")
                      (recur (assoc loop-state
                               :iteration (inc iteration)
                               :messages (conj messages {:role "user" :content error-feedback})
                               :trace (conj trace trace-entry)
                               :consecutive-errors (inc consecutive-errors) :restarts restarts))))

                  (let [_ (accumulate-usage! (:api-usage iteration-result))
                        {:keys [thinking blocks final-result]} iteration-result
                        final-answer (when final-result (:answer final-result))
                        _ (update-system-vars! environment
                            {:thinking thinking :final-result final-result :final-answer final-answer})
                        vars-snapshot (restorable-var-snapshots environment blocks)
                        previous-iteration-id (some-> (:current-iteration-id-atom environment) deref)
                        vars-snapshot (inject-system-var-snapshots vars-snapshot
                                        {:query              query
                                         :thinking           thinking
                                         :final-answer       final-answer
                                         :turn-query-id      query-id
                                         :iteration-id       previous-iteration-id
                                         :conversation-soul-id  (:conversation-id environment)
                                         :conversation-state-id (persistance/db-latest-conversation-state-id
                                                                  (:db-info environment)
                                                                  (:conversation-id environment))
                                         :system-prompt      system-prompt
                                         ;; Same frozen snapshot bound in SCI.
                                         ;; Re-stamped every iteration so
                                         ;; vis/var-history 'TURN_ACTIVE_EXTENSIONS
                                         ;; returns one row per iter, not just iter 0.
                                         :extensions-snapshot (prompt/extensions-snapshot active-exts)
                                         ;; Live conversation title; same value
                                         ;; the SCI sandbox sees as CONVERSATION_TITLE.
                                         :conversation-title (some-> (:conversation-title-atom environment)
                                                               deref str)})
                        [redundant-count expression-count]
                        (count-duplicates seen-expression-hashes-atom
                          (or blocks []))
                        redundancy-fraction
                        (if (pos? expression-count)
                          (double (/ redundant-count expression-count))
                          0.0)
                        iteration-metadata-with-metrics
                        (merge (or (iteration-metadata) {})
                          {:expression-redundancy-fraction redundancy-fraction
                           :dedup-saves                    redundant-count})
                        iteration-id (persistance/db-store-iteration! (:db-info environment)
                                       {:query-id query-id :blocks blocks :vars vars-snapshot
                                        :thinking thinking
                                        :answer (when final-result (answer-str (:answer final-result)))
                                        :answer-form-idx (when final-result (:answer-form-idx final-result))
                                        :duration-ms (or (:duration-ms iteration-result) 0)
                                        :llm-messages (:llm-messages iteration-result)
                                        :llm-provider (or (:llm-provider iteration-result) (:provider resolved-model))
                                        :llm-model (:llm-model iteration-result)
                                        :metadata iteration-metadata-with-metrics})
                        _ (when-let [a (:current-iteration-id-atom environment)] (reset! a iteration-id))
                        _ (update-iteration-id! environment iteration-id)
                        _ (emit-hook! on-iteration
                            {:iteration iteration
                             :status (cond final-result :final (empty? blocks) :empty :else :success)
                             :status-id (status->id (cond final-result :final (empty? blocks) :empty :else :success))
                             :thinking thinking :blocks blocks :final-result final-result
                             :error nil :duration-ms (or (:duration-ms iteration-result) 0)}
                            "on-iteration (success)")
                        trace-entry {:iteration iteration :thinking thinking
                                     :blocks blocks :final? (boolean final-result)}]
                    (cond
                      final-result
                      (do (log-stage! :final iteration
                            {:answer (truncate (answer-str (:answer final-result)) 200)
                             :iteration-count (inc iteration)})
                        (log-stage! :iteration-end iteration
                          {:blocks (count blocks) :errors (count (filter :error blocks))
                           :times (mapv :execution-time-ms blocks)})
                        ;; Iteration-final chunk (`:phase :iteration-final`).
                        ;; Per-form chunks already streamed every iN.K
                        ;; result; this is the trim \"iteration is
                        ;; complete, here is the terminal answer\"
                        ;; signal. Consumers attach `:final` to
                        ;; whatever's already on screen.
                        ;;
                        ;; `:answer-form-idx` tells the channel which
                        ;; per-form slot was the `(answer …)` call;
                        ;; the progress tracker elides that slot so
                        ;; the renderer doesn't paint the answer
                        ;; call's code above the answer text.
                        (when on-chunk
                          (on-chunk {:phase            :iteration-final
                                     :iteration        iteration
                                     :thinking         thinking
                                     :final            {:answer          (:answer final-result)
                                                        :iteration-count (inc iteration)
                                                        :status          :success}
                                     :answer-form-idx  (:answer-form-idx final-result)
                                     :done?            true}))
                        (merge {:answer (:answer final-result) :trace (conj trace trace-entry)
                                :iteration-count (inc iteration)}
                          (finalize-cost)))

                      :else
                      (if (empty? blocks)
                        (do (log-stage! :empty iteration {})
                          (log-stage! :iteration-end iteration {:blocks 0 :errors 0 :times []})
                          (recur (merge loop-state
                                   {:iteration (inc iteration) :trace (conj trace trace-entry)})))

                        (do (log-stage! :iteration-end iteration
                              {:blocks (count blocks) :errors (count (filter :error blocks))
                               :times (mapv :execution-time-ms blocks)})
                          ;; Non-terminal iteration-final chunk: per-form
                          ;; chunks already streamed; this is the
                          ;; \"iteration done, more iterations coming\"
                          ;; marker. `:final` is nil because the
                          ;; turn isn't done yet.
                          (when on-chunk
                            (on-chunk {:phase     :iteration-final
                                       :iteration iteration
                                       :thinking  thinking
                                       :final     nil
                                       :done?     false}))
                          (let [had-success? (some #(nil? (:error %)) blocks)
                                next-errors (if had-success? 0 (inc consecutive-errors))
                                _ (when had-success? (swap! var-index-atom update :current-revision inc))]
                            (recur (merge loop-state
                                     {:iteration (inc iteration) :messages messages
                                      :trace (conj trace trace-entry) :consecutive-errors next-errors
                                      :previous-blocks blocks :previous-iteration iteration}))))))))))))))))

(defn- ->prior-outcome
  [result]
  (case (:status result)
    :cancelled :cancelled
    :error     :error
    :complete))

(defn run-query!
  "Store query → iteration-loop → update query → return result.

   Derives `:prior-outcome` (one of `:complete`,
   `:abandoned`, `:cancelled`, `:error`) from the loop result and
   persists it on the `query_state` row. The next turn's
   `<system_state>` digest reads it."
  [env query loop-opts]
  (when-not (map? env)
    (throw (ex-info "run-query! requires an env map" {:got (type env)})))
  (when (clojure.string/blank? query)
    (throw (ex-info "run-query! requires a non-blank query string" {:got query})))
  (let [query-id (persistance/db-store-query! (:db-info env)
                   {:parent-conversation-id (:conversation-id env)
                    :query query
                    :messages nil
                    :status :running})
        result (iteration-loop env query (assoc loop-opts :query-id query-id))
        prior-outcome (->prior-outcome result)
        _ (persistance/db-update-query! (:db-info env) query-id
            {:answer          (:answer result)
             :iteration-count (:iteration-count result)
             :duration-ms     (:duration-ms result)
             :status          (or (:status result) :success)
             :tokens          (:tokens result)
             :cost            (:cost result)
             :prior-outcome   prior-outcome})]
    (assoc result :query-id query-id :prior-outcome prior-outcome)))

;; -----------------------------------------------------------------------------
;; Prepare query context
;; -----------------------------------------------------------------------------

(defn- prepare-query-context
  "Validates inputs, resolves SCI bindings, sets up atoms.
   Returns a map of all computed context needed for subsequent phases."
  [env messages opts]
  (let [{:keys [spec model
                max-context-tokens
                system-prompt debug? hooks cancel-atom eval-timeout-ms
                reasoning-default routing]
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
          ;; `query-str` = ONLY the current turn's user message.
          ;;
          ;; Prior behavior joined every message's :content (including
          ;; previous turns' user messages + assistant answers + system!) into
          ;; one growing blob. That corrupted three things at once:
          ;;   1. `query_attrs.text` / `.name` stored the entire transcript
          ;;      for every turn — the sidebar showed "Siema\nSiema!\n…".
          ;;   2. `*query*` SYSTEM var (bound from this same string) grew
          ;;      with each turn instead of reflecting the current ask.
          ;;   3. The synthetic `{:requirement …}` frame the LLM sees
          ;;      restated the whole conversation as the "requirement".
          ;;
          ;; Conversation history still reaches the model via the `messages`
          ;; vector itself (passed through to the LLM call unmodified).
          ;; `query-str` is ONLY the current turn — one ask, one value.
          extract-text           (fn [c]
                                   (cond
                                     (string? c)     c
                                     (sequential? c) (str/join " "
                                                       (keep #(when (= "text" (:type %)) (:text %)) c))
                                     :else           nil))
          ;; Locate the LAST user message once. It's both the source of
          ;; `query-str` (the `{:requirement ...}` payload for iteration 0)
          ;; AND the boundary between history and the current turn:
          ;; everything BEFORE its index is prior conversation history,
          ;; which `iteration-loop` replays as `:history-messages`.
          last-user-idx          (->> (map-indexed vector messages)
                                   reverse
                                   (some (fn [[i m]]
                                           (when (contains? #{"user" :user} (:role m))
                                             i))))
          last-user-message      (when last-user-idx (nth messages last-user-idx))
          query-str              (or (some-> last-user-message :content extract-text)
                                   ;; Fallback: no :user role found (malformed caller) —
                                   ;; use the last message's text. Better than an empty query.
                                   (some-> messages last :content extract-text)
                                   "")
          history-messages       (if last-user-idx
                                   (vec (take last-user-idx messages))
                                   (vec messages))
          env-router             (:router env)
          root-resolved-model    (when env-router (resolve-effective-model env-router))
          root-model             (or (:name root-resolved-model) model)
          root-provider          (:provider root-resolved-model)
          db-info                (:db-info env)
          custom-bindings        (custom-bindings env)
          current-iteration-atom (atom 0)
          sci-ctx                (:sci-ctx env)
          _                      (env/bump-var-index! env)
          _                      (doseq [[sym val] (or custom-bindings {})]
                                   (when val
                                     (env/sci-update-binding! sci-ctx sym val)))
          _                      (env/bump-var-index! env)
          current-iteration-id-atom (atom nil)
          current-query-id-atom    (atom nil)
          environment            (assoc env
                                   :current-iteration-atom current-iteration-atom
                                   :current-iteration-id-atom current-iteration-id-atom
                                   :current-query-id-atom current-query-id-atom)
          environment-id         (:environment-id env)]
      {:cancel-atom            cancel-atom
       :query-str              query-str
       :router                 env-router
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
       :messages               messages
       :history-messages       history-messages})))

;; -----------------------------------------------------------------------------
;; Run iteration phase
;; -----------------------------------------------------------------------------

(defn- run-iteration-phase
  "Runs the main iteration loop via run-query!.
   Returns iteration-result, query-id, cost atoms, and merge-cost! fn."
  [{:keys [environment query-str history-messages spec
           max-context-tokens system-prompt
           current-iteration-atom hooks cancel-atom
           reasoning-default routing]}]
  (let [iteration-result (run-query! environment query-str
                           (cond-> {:output-spec            spec
                                    :max-context-tokens     max-context-tokens
                                    :system-prompt          system-prompt
                                    :reasoning-default      reasoning-default
                                    :history-messages       history-messages
                                    :current-iteration-atom current-iteration-atom
                                    :hooks                  hooks
                                    :cancel-atom            cancel-atom}
                             routing (assoc :routing routing)))
        query-id         (:query-id iteration-result)
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
                                  (merge-with + (select-keys acc [:input-cost :output-cost :total-cost])
                                    (select-keys extra-cost [:input-cost :output-cost :total-cost]))))))]
    {:iteration-result  iteration-result
     :query-id         query-id
     :total-tokens-atom total-tokens-atom
     :total-cost-atom   total-cost-atom
     :merge-cost!       merge-cost!}))

;; -----------------------------------------------------------------------------
;; Finalize query result
;; -----------------------------------------------------------------------------

(defn- finalize-query-result
  "Updates DB query record, builds result map.

   `:provider` and `:model` are both attached to the persisted cost
   map so the web footer / meta layer can render `provider/model · N
   iteration · duration · tokens · $total` after a restart."
  [{:keys [db-info root-model root-provider]}
   {:keys [query-id start-time iteration-count status status-id trace locals
           answer confidence reasoning total-tokens-atom total-cost-atom]}]
  (let [duration-ms (util/elapsed-since start-time)
        cost-with-model (cond-> @total-cost-atom
                          (and root-model (not (:model @total-cost-atom)))
                          (assoc :model (str root-model))
                          (and root-provider (not (:provider @total-cost-atom)))
                          (assoc :provider root-provider))]
    (if status
      ;; failure path — surface the fallback answer (built by the loop for
      ;; :error) to the caller. Leaving
      ;; :answer nil here meant the web bubble rendered blank even though
      ;; we had diagnostic text ready.
      (do
        (log-stage! :query-end 0
          {:duration-ms duration-ms :iteration-count iteration-count :status status})
        (let [fallback-answer (:result answer answer)]
          (try
            (persistance/db-update-query! db-info query-id
              {:answer          fallback-answer
               :iteration-count iteration-count
               :duration-ms     duration-ms
               :status          status
               :tokens          @total-tokens-atom
               :cost            cost-with-model})
            (catch Exception e
              (tel/log! {:level :warn :data (format-exception-short e)
                         :msg   "Failed to update query (max iterations)"})))
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
        (log-stage! :query-end 0
          {:duration-ms duration-ms :iteration-count iteration-count
           :cost (str (:total-cost cost-with-model))})
        (try
          (persistance/db-update-query! db-info query-id
            {:answer          answer
             :iteration-count iteration-count
             :duration-ms     duration-ms
             :status          :success
             :tokens          @total-tokens-atom
             :cost            cost-with-model})
          (catch Exception e
            (tel/log! {:level :warn :data (format-exception-short e)
                       :msg   "Failed to update query (success)"})))
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

(defn query!
  "Runs a query on an RLM environment using iterative LLM code evaluation.

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

    Returns:
   Map with:
      - :trace - Vector of iteration trace entries, each containing:
          {:iteration N
           :response <llm-response-text>
           :blocks [{:id 0 :code <code-str> :result <value> :stdout <str> :error nil :execution-time-ms 5}
                       ...]}
     - :iteration-count - Number of iterations used.
     - :duration-ms - Query duration in milliseconds.
     - :tokens - Token usage map {:input N :output N :total N}.
     - :cost - Cost map {:input-cost N :output-cost N :total-cost N}.
     - :confidence - Confidence level (:high/:medium/:low) from final iteration.
      - :reasoning - String summary of how the answer was derived (from LLM's FINAL call).
      - :status - Only present on failure (`:error` or `:cancelled`)."
  ([environment messages]
   (query! environment messages {}))
  ([environment messages opts]
   (let [ctx (prepare-query-context environment messages opts)
         {:keys [eval-timeout-ms concurrency
                 debug? query-str root-model
                 db-info
                 environment-id]} ctx
         merged-concurrency (merge DEFAULT_CONCURRENCY concurrency)]
     (binding [*rlm-context*       {:rlm-environment-id environment-id :rlm-type :main
                                    :rlm-debug? debug? :rlm-phase :query
                                    :db-info db-info
                                    :conversation-soul-id (:conversation-id environment)}
               *eval-timeout-ms*  (clamp-eval-timeout-ms
                                    (or eval-timeout-ms *eval-timeout-ms*))
               *concurrency*      merged-concurrency]
       (tel/with-ctx+ {:db-info db-info
                       :conversation-soul-id (:conversation-id environment)}
         (log-stage! :query-start 0
           {:model root-model
            :reasoning? (boolean (:reasoning? (first (mapcat :models (:providers (:router environment))))))
            :query query-str})
         (let [start-time   (System/nanoTime)
               phase2       (run-iteration-phase ctx)
               {:keys [iteration-result query-id
                       total-tokens-atom total-cost-atom]} phase2
               {iteration-answer :answer
                trace            :trace
                iteration-count  :iteration-count
                status           :status
                status-id        :status-id
                locals           :locals
                confidence       :confidence
                reasoning        :reasoning} iteration-result]
           (if status
             (finalize-query-result
               ctx
               {:query-id          query-id
                :start-time        start-time
                :iteration-count   iteration-count
                :status            status
                :status-id         status-id
                :trace             trace
                :locals            locals
                :answer            iteration-answer
                :total-tokens-atom total-tokens-atom
                :total-cost-atom   total-cost-atom})
             (finalize-query-result
               ctx
               {:query-id          query-id
                :start-time        start-time
                :iteration-count   iteration-count
                :trace             trace
                :answer            iteration-answer
                :confidence        confidence
                :reasoning         reasoning
                :total-tokens-atom total-tokens-atom
                :total-cost-atom   total-cost-atom}))))))))

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
;; that no caller actually invoked — every consumer either destructured
;; `:db-info` directly or used the no-arg `(db-info)` defined further
;; down (which returns the process-wide shared connection). The defn was
;; deleted to keep ONE canonical `db-info` symbol on this namespace.

(defn custom-bindings
  "Current custom SCI bindings {sym -> value}."
  [env]
  (some-> (:state-atom env) deref :custom-bindings))

;; =============================================================================
;; System Prompt
;; =============================================================================

;; =============================================================================
;; Auto-Forget
;; =============================================================================

(defn- system-var-sym?
  "Local alias — the canonical predicate lives in `sci-env`. Kept here
   as a private helper so this file's auto-forget code reads cleanly
   without an extra namespace bounce on every call."
  [sym]
  (env/system-var-sym? sym))

(defn- forget-vars!
  "Unmap `names` from the SCI sandbox namespace. Used by the
   deterministic auto-forget at query boundaries.

   HARD GUARD: SYSTEM vars (every name in `SYSTEM_VAR_NAMES` — the
   `TURN_*` / `ITERATION_*` / `CONVERSATION_*` registry) can NEVER be
   forgotten — they are contract surfaces the iteration loop re-binds
   every turn; dropping them would tear the sandbox mid-turn.
   Filtered out + logged."
  [sci-ctx names]
  (let [raw-syms (keep (fn [n]
                         (cond (symbol? n) n
                           (string? n) (try (clojure.core/symbol n) (catch Throwable _ nil))
                           :else       nil))
                   names)
        {system-syms true user-syms false} (group-by (comp boolean system-var-sym?) raw-syms)]
    (when (seq system-syms)
      (tel/log! {:level :info :id ::forget-system-var-refused
                 :data {:requested (mapv str system-syms)}
                 :msg "Refusing to forget SYSTEM vars — ignoring those names"}))
    (when (seq user-syms)
      (try
        (swap! (:env sci-ctx) update-in [:namespaces 'sandbox]
          (fn [ns-map] (apply dissoc ns-map user-syms)))
        (catch Throwable e
          (tel/log! {:level :debug :id ::forget-vars-failed
                     :data {:error (ex-message e) :syms (mapv str user-syms)}
                     :msg "forget-vars! failed — skipping"}))))))

(def ^:const AUTO_FORGET_STALE_QUERIES
  "Number of recent queries a var must have been defined/redefined in to
   survive auto-forget. Vars without a docstring that were last touched
   more than this many queries ago are evicted at the start of each new
   query. DB rows are untouched — `(var-history 'sym)` still works."
  3)

(defn auto-forget-candidates
  "Pure function. Returns the set of sandbox var symbols that should be
   auto-forgotten at the start of a new query.

   A var is a candidate when ALL of:
   1. It is a user var (not in `initial-ns-keys`).
   2. It is not a SYSTEM var (per `SYSTEM_VAR_NAMES`).
   3. It has NO docstring (runtime SCI meta `:doc` is nil/blank).
   4. It was last defined/redefined in a query that is NOT among the
      `recent-query-ids`.

   Params:
   - `sandbox-map`      — SCI sandbox namespace map {symbol → value-or-var}
   - `initial-ns-keys`  — set of symbols that are built-in tools/helpers
   - `var-registry`     — result of `db-latest-var-registry`:
                          {symbol → {:query-id ... :value ... :code ...}}
   - `recent-query-ids` — set of query UUIDs for the last N queries

   Returns: set of symbols to forget."
  [sandbox-map initial-ns-keys var-registry recent-query-ids]
  (let [recent-ids (set recent-query-ids)]
    (into #{}
      (filter
        (fn [sym]
          (let [v (get sandbox-map sym)
                doc (:doc (meta v))
                has-doc? (and doc (not (str/blank? doc)))
                reg-entry (get var-registry sym)
                defining-query-id (:query-id reg-entry)]
            (and
              (not (contains? initial-ns-keys sym))
              (not (env/system-var-sym? sym))
              (not has-doc?)
              (some? reg-entry)
              (not (contains? recent-ids defining-query-id))))))
      (keys sandbox-map))))

(defn auto-forget-stale-vars!
  "Deterministic cleanup at the query boundary: remove sandbox vars that
   (a) have no docstring AND (b) were last defined/redefined more than
   `AUTO_FORGET_STALE_QUERIES` queries ago. Replaces the unreliable
   ask-the-LLM-to-emit-`:forget` pattern for scratch vars. DB rows are
   untouched — `(var-history 'sym)` can inspect old values."
  [{:keys [db-info conversation-id sci-ctx initial-ns-keys var-index-atom]}]
  (when (and db-info conversation-id sci-ctx)
    (try
      (let [all-queries  (sort-by :created-at
                           (persistance/db-list-conversation-queries db-info conversation-id))
            recent-ids   (into #{} (map :id) (take-last AUTO_FORGET_STALE_QUERIES all-queries))
            var-registry (persistance/db-latest-var-registry db-info conversation-id)
            sandbox-map  (get-in @(:env sci-ctx) [:namespaces 'sandbox])
            candidates   (auto-forget-candidates sandbox-map initial-ns-keys
                           var-registry recent-ids)]
        (when (seq candidates)
          (tel/log! {:level :info :id ::auto-forget
                     :data {:forgotten (mapv str candidates) :count (count candidates)}
                     :msg (str "Auto-forget: evicting " (count candidates) " stale vars without docstrings")})
          (forget-vars! sci-ctx candidates)
          (when var-index-atom
            (swap! var-index-atom update :current-revision inc))))
      (catch Exception e
        (tel/log! {:level :warn :id ::auto-forget-failed
                   :data {:error (ex-message e)}
                   :msg "Auto-forget failed — skipping"})))))

;; =============================================================================
;; Environment Lifecycle
;; =============================================================================

;; `create-environment` calls `register-extension!` indirectly via
;; `register-extensions!`. Forward-declare so the symbol resolves at
;; load time even though the def comes later in the file.
(declare install-extension!)

(defn create-environment
  "Creates a vis environment (component) for conversation lifecycle and
   querying.

   The environment holds:
     - SCI sandbox context with custom bindings + var-index cache
     - DB connection (or shared-mem datasource)
     - Router (LLM provider config)
     - Extension registry atom

   Params:
     `router` — Required. Result of `llm/make-router`.
     `opts`   — Map with `:db` and optional `:conversation`,
                 `:channel`, `:external-id`, `:title`.

     `:db` accepted forms:
       nil               — no DB (SCI-only execution)
       :memory           — ephemeral in-process SQLite DB
       path string       — persistent SQLite DB at path
       {:path p}         — persistent SQLite DB at path
       {:datasource ds}  — caller-owned DataSource (not closed on dispose)

   Returns the vis environment map."
  [router {:keys [db conversation channel external-id title]}]
  (when-not router
    (anomaly/incorrect! "Missing router" {:type :vis/missing-router}))
  (let [depth-atom               (atom 0)
        db-info                  (persistance/db-create-connection! db)
        var-index-atom           (atom {:index nil :revision -1 :current-revision 0})
        state-atom               (atom {:custom-bindings {}
                                        :environment     nil
                                        :conversation-id nil})
        environment-atom         (atom nil)
        environment-id           (str (util/uuid))
        ;; Iteration-final-answer signal. The SCI sandbox's `(answer
        ;; "…")` fn `reset!`s this atom with `{:value :form-idx}`;
        ;; the iteration loop reads it back after evaluating each
        ;; iteration's forms and discards iff the form at `:form-idx`
        ;; itself errored (Option C scoping — sibling errors do NOT
        ;; gate the answer). Reset to nil before every iteration runs.
        answer-atom              (atom nil)
        ;; Form-index pointer the iteration loop reset!s before each
        ;; expression's `execute-code` call so `answer-fn` knows which
        ;; form's evaluation invoked it. Pairs with `answer-atom` to
        ;; implement Option C (scoped) discard semantics.
        current-form-idx-atom    (atom nil)
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
        ;; Real per-query assembly goes through `prompt/assemble-system-prompt`
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
        ;; `<var_index>` (matches the treatment of every other system
        ;; binding shipped via EXTRA_BINDINGS).
        var-history-fn           (fn var-history [sym]
                                   (persistance/db-var-history db-info conversation-id
                                     (cond
                                       (symbol? sym) sym
                                       (string? sym) (clojure.core/symbol sym)
                                       :else (clojure.core/symbol (str sym)))))
        ;; SCI binding for `(answer "…")` — the canonical turn-
        ;; termination call. Closes over `answer-atom` AND
        ;; `current-form-idx-atom` so the iteration loop can scope
        ;; the discard check to the form that actually called this.
        ;; Returns the marker keyword so the iN.K result row makes
        ;; intent visible.
        answer-fn                (fn answer [s]
                                   (reset! answer-atom
                                     {:value    (str s)
                                      :form-idx @current-form-idx-atom})
                                   :vis/answer)
        ;; SCI binding for the conversation title:
        ;;   `(conversation-title \"...\")` — writes the title through
        ;;                              to DB, syncs the in-memory
        ;;                              atom, and broadcasts
        ;;                              `:title-changed` to every
        ;;                              registered listener so
        ;;                              channels (e.g. the TUI
        ;;                              header) can refresh without
        ;;                              polling.
        ;; ONE-ARITY ONLY. To READ the current title from `:code`,
        ;; reference the `CONVERSATION_TITLE` SYSTEM var — there is
        ;; no zero-arg reader, by design: a `(conversation-title)`
        ;; call would invite the model to round-trip what it can
        ;; read for free. Calling with the wrong arity raises an
        ;; `ArityException` from SCI like any other Clojure fn.
        conversation-title-fn    (fn conversation-title [s]
                                   (let [s (str s)]
                                     (set-title-with-broadcast!
                                       db-info conversation-id
                                       conversation-title-atom s)
                                     s))
        env-bindings             {'var-history        var-history-fn
                                  'answer             answer-fn
                                  'conversation-title conversation-title-fn}
        {:keys [sci-ctx sandbox-ns initial-ns-keys]}
        (env/create-sci-context (merge env-bindings
                                  (:custom-bindings @state-atom)))
        env {:environment-id  environment-id
             :conversation-id conversation-id
             :depth-atom      depth-atom
             :db-info         db-info
             :var-index-atom  var-index-atom
             :state-atom      state-atom
             :sci-ctx         sci-ctx
             :sandbox-ns      sandbox-ns
             :initial-ns-keys initial-ns-keys
             :router          router
             :answer-atom           answer-atom
             :current-form-idx-atom current-form-idx-atom
             :conversation-title-atom            conversation-title-atom
             :extensions            (atom [])}]
    (reset! environment-atom env)
    (swap! state-atom assoc :environment env :conversation-id conversation-id)
    ;; Restore persisted vars when resuming an existing conversation.
    (when resolved-conversation-id
      (try
        (env/restore-sandbox! sci-ctx db-info conversation-id)
        (env/bump-var-index! env)
        (catch Throwable t
          (tel/log! {:level :warn :id ::restore-sandbox-failed
                     :data {:error (ex-message t)
                            :conversation-id conversation-id}
                     :msg "Failed to restore sandbox from DB — starting empty"}))))
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

(defn install-extension!
  "Register a validated extension into `environment` (per-env registration,
   distinct from the global-registry `register-extension!` defined earlier
   in this file).

   Checks `:ext/requires` — if the extension declares dependencies, all
   listed extension namespaces must already be registered. Throws on
   missing dependencies.

   If an extension with the same `:ext/namespace` is already registered,
   it is replaced (not duplicated). Enables hot-swap via
   `reload-extension!`.

   Returns `environment` for chaining."
  [environment ext]
  (when-not (:extensions environment)
    (anomaly/incorrect! "Invalid vis environment — missing :extensions atom"
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
  ;; Bind extension symbols ONLY into the aliased namespace — never
  ;; into sandbox. The LLM must always use the alias form
  ;; `(alias/symbol ...)`, not `(sdk/symbol ...)`.
  ;;
  ;; Multi-extension MERGE: two extensions can share an `:ext/ns-alias`
  ;; (e.g. one ext registers `vis/cat`/`vis/ls`, another adds
  ;; `vis/diff` under the same `vis` alias). The bindings are MERGED
  ;; into the existing namespace map; same-name symbols get last-write-
  ;; wins (matching how `install-extension!` already replaces an
  ;; extension with the same `:ext/namespace`). A telemere warn line
  ;; fires on collisions so reviewers see which extension shadowed
  ;; whose symbol.
  (let [wrapped (extension/wrap-extension ext environment)
        sci-ctx (:sci-ctx environment)]
    (when-let [{ns-sym :ns alias-sym :alias} (:ext/ns-alias ext)]
      (let [ext-ns      (sci/create-ns ns-sym)
            ns-bindings (into {} (map (fn [[sym val]]
                                        [sym (sci/new-var sym val {:ns ext-ns})]))
                          wrapped)
            existing    (get-in @(:env sci-ctx) [:namespaces ns-sym])
            collisions  (when (seq existing)
                          (vec (filter #(contains? existing %) (keys ns-bindings))))]
        (when (seq collisions)
          (tel/log! {:level :warn :id ::ext-symbol-collision
                     :data  {:ext       (:ext/namespace ext)
                             :ns        ns-sym
                             :alias     alias-sym
                             :symbols   collisions}
                     :msg   (str "Extension '" (:ext/namespace ext)
                              "' shadowed " (count collisions)
                              " existing symbol(s) under alias '" alias-sym
                              "': " (str/join ", " collisions))}))
        (swap! (:env sci-ctx) update-in [:namespaces ns-sym] merge ns-bindings)
        (swap! (:env sci-ctx) update :ns-aliases assoc alias-sym ns-sym))
      ;; Auto-require the alias in sandbox so the LLM never has to call
      ;; `(require ...)` manually.
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
    ;; Inject extension-declared Java classes and imports.
    (when-let [classes (seq (:ext/classes ext))]
      (swap! (:env sci-ctx) update :classes merge (into {} classes)))
    (when-let [imports (seq (:ext/imports ext))]
      (swap! (:env sci-ctx) update :imports merge (into {} imports))))
  environment)

;; =============================================================================
;; Conversation env cache
;; =============================================================================

;; ---------------------------------------------------------------------------
;; In-process conversation cache + channel utilities
;; ---------------------------------------------------------------------------

(defonce cache (atom {}))

(defn cache-env! [id env]
  (swap! cache assoc id {:environment env :lock (Object.)})
  {:id id :environment env})

(defn refresh-cached-routers!
  "Reseat `:router` on every cached env's environment map.

  `create-environment` snapshots the router into
  `(:router env)` at construction time, and the iteration loop calls
  `(svar/ask! (:router environment) ...)` — not the global
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
            (assoc m id {:environment env :lock (Object.)}))))
      (get @cache id))))

(defn db-info
  "Return the process-wide shared DB connection bound to
   `(config/resolve-db-spec)`. Thin wrapper over
   `persistance.core/db-shared-connection!` that fills in the default db-spec
   so frontend callers don't have to know about config resolution."
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
    (or (when-let [ref (persistance/db-find-conversation-by-external (db-info) :telegram ext)]
          (by-id (str (second ref))))
      (create! :telegram {:external-id ext}))))

;; =============================================================================
;; Title listeners + set-title! broadcast
;;
;; Channels (TUI, Telegram, ...) that want to react to a conversation
;; title change — typically because the model emitted `(conversation-title "...")`
;; mid-turn — register a listener via `add-title-listener!`. The
;; listener fn receives the new title; it MUST be cheap (typically a
;; `state/dispatch` into the channel's app-db). Listeners are stored
;; per conversation-id so a TUI watching conversation A doesn't get
;; woken by a Telegram bot updating conversation B.
;;
;; Both `set-title!` (host-driven, e.g. CLI rename) and the SCI
;; `(conversation-title "...")` fn (model-driven) funnel through
;; `set-title-with-broadcast!`, which is the single mutation point.
;; That keeps the in-memory env atom + DB column + listener fan-out
;; in lockstep — no path can update one without the others.
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
   Listeners that throw are swallowed and logged — a misbehaving
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

(defn env-for
  [id]
  (:environment (ensure-env! id)))

(defn effective-system-prompt-for-query
  "Return the reconstructed prompt snapshot for a specific query-id in a
   conversation. Renders the minimal projection: <recent> + <var_index>."
  [conversation-id query-id]
  (when-let [env (env-for conversation-id)]
    (let [active-exts   (prompt/active-extensions env)
          system-prompt (prompt/assemble-system-prompt env
                          {:system-prompt     (:system-prompt (by-id conversation-id))
                           :active-extensions active-exts})
          db-info       (:db-info env)
          queries       (when db-info
                          (try (persistance/db-list-conversation-queries db-info (:conversation-id env))
                            (catch Throwable _ nil)))
          query-row     (some #(when (= (:id %) query-id) %) queries)
          query-text    (or (:query query-row) "<the user's message appears here>")
          iteration-context-block (prompt/build-iteration-context env
                                    {:call-counts-atom  (atom {})
                                     :active-extensions active-exts})]
      ;; Iterations now go through `svar/ask-code!` — a plain-text
      ;; completion path with no JSON spec. svar appends one short
      ;; code-format reminder (`:code-tail-pointer? true`) as the last
      ;; text block of the last user message. We render that hint here
      ;; so the inspector matches the wire shape the model actually sees.
      (str "═══ MESSAGE 1: System (role: system) ═══\n"
        system-prompt
        "\n\n═══ MESSAGE 2: User query (role: user) ═══\n"
        query-text
        (when iteration-context-block
          (str "\n\n═══ MESSAGE 3: Iteration context (role: user, appended per iteration) ═══\n"
            iteration-context-block))
        "\n\n═══ MESSAGE 4: Code-format reminder (role: user, appended by svar/ask-code!) ═══\n"
        "Reply with clojure source inside ```clojure … ``` fences. "
        "Multiple fenced blocks are allowed and will be concatenated in order. "
        "No prose outside fences. No commentary, no explanation."))))

(defn effective-system-prompt
  "Return the reconstructed prompt snapshot for the latest query in a conversation."
  [conversation-id]
  (when-let [d (some-> (env-for conversation-id) :db-info)]
    (let [queries (try (persistance/db-list-conversation-queries d conversation-id)
                    (catch Throwable _ nil))
          last-q  (last queries)]
      (when last-q
        (effective-system-prompt-for-query conversation-id (:id last-q))))))

(defn send!
  ([id messages] (send! id messages {}))
  ([id messages opts]
   (let [{:keys [environment lock]} (ensure-env! id)
         message-vec (if (string? messages) [(svar/user messages)] messages)]
     (locking lock
       (query! environment message-vec opts)))))

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

(defn db-sweep-orphaned-running-queries!
  "Convenience wrapper: run the persistence-layer sweep against the
   shared DB handle. Frontends call this on startup to mark queries
   that crashed mid-write as `:interrupted`."
  ([] (persistance/db-sweep-orphaned-running-queries! (db-info)))
  ([db] (persistance/db-sweep-orphaned-running-queries! db)))

(defn close-all!
  []
  (doseq [[_ {:keys [environment]}] @cache]
    (try (dispose-environment! environment) (catch Exception _ nil)))
  (reset! cache {})
  (persistance/db-dispose-shared-connection!))
