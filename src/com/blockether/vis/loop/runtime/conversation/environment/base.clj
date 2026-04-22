(ns com.blockether.vis.loop.runtime.conversation.environment.base
  "SCI sandbox primitives: context construction, bindings, var-index.

   * `sci-update-binding!` / `create-sci-context` — build and mutate the
     sandbox where user code runs.
   * `build-var-index` — compact table describing user-def'd vars, shown
     to the LLM between iterations."
  (:require
   [clojure.set]
   [clojure.string :as str]
   [clojure.walk]
   [clojure+.core]
   [clojure+.walk]
   [com.blockether.vis.loop.storage.db :as db]
   [com.blockether.vis.loop.runtime.shared :as sci-shared]
   [editscript.core :as es]
   [sci.addons.future :as sci-future]
   [sci.core :as sci])
  (:import [com.github.difflib DiffUtils UnifiedDiffUtils]))

;; =============================================================================
;; SCI Context Helpers
;; =============================================================================

(defn sci-update-binding!
  "Update a binding in an existing SCI context.
   Ensures the symbol is a real SCI var before interning the value,
   since bindings from sci/init :namespaces are not SCI vars.

   NOTE: This mutates the SCI sandbox only. If the caller wants the next
   iteration's `<var_index>` context block to reflect the new binding, they
   MUST also call `bump-var-index!` on the env — the var-index is cached and
   only rebuilds when `:current-revision` advances. Every past cache-staleness
   bug (4-iter `(restore-vars …)` spin, turn-3 `*query*=\"Siema\"` replay)
   traces back to forgetting this pair. Prefer `bind-and-bump!` below."
  [sci-ctx sym val]
  (let [ns-obj (sci/find-ns sci-ctx 'sandbox)]
    (sci/eval-string+ sci-ctx (str "(def " sym " nil)") {:ns ns-obj})
    (sci/intern sci-ctx ns-obj sym val)))

(defn bump-var-index!
  "Invalidate the env's cached `<var_index>` so the next `get-var-index` call
   rebuilds it from the live SCI sandbox. No-op when the env has no
   `:var-index-atom` (e.g. ad-hoc test contexts)."
  [env]
  (when-let [atom (:var-index-atom env)]
    (swap! atom update :current-revision (fnil inc 0))))

(defn bind-and-bump!
  "Atomic \"rebind var in SCI + invalidate var-index cache\" — the only API
   call sites should use when mutating runtime bindings that the LLM needs
   to see on the NEXT iteration. Fixes every instance of the model looping
   on `(restore-vars …)` / `(def X …)` because the var_index never caught up."
  [env sym val]
  (sci-update-binding! (:sci-ctx env) sym val)
  (bump-var-index! env))

;; =============================================================================
;; SCI Context Creation
;; =============================================================================

(defn- ->pattern
  "Promote `x` to a `java.util.regex.Pattern` if it's a string. Pass-through
   when it's already a Pattern. Shared helper behind all the auto-promoting
   regex wrappers below."
  ^java.util.regex.Pattern [x]
  (if (instance? java.util.regex.Pattern x)
    x
    (java.util.regex.Pattern/compile (str x))))

(defn- safe-split
  "Drop-in replacement for `clojure.string/split` that auto-promotes a string
   delimiter to a `java.util.regex.Pattern`. Clojure's native `str/split`
   requires a Pattern — LLMs frequently write `(str/split s \"\\n\")` and the
   error surfaces late (often inside a lazy seq realization), burning
   iterations. This wrapper matches what the LLM expects and never weakens the
   Pattern path."
  ([s re] (str/split s (->pattern re)))
  ([s re limit] (str/split s (->pattern re) limit)))

(defn- safe-re-find
  "Shadow for `clojure.core/re-find`: accepts a string pattern in addition to a
   `java.util.regex.Pattern`. Preserves Matcher-arity passthrough unchanged."
  ([x]
   (re-find x))
  ([re s]
   (re-find (->pattern re) (str s))))

(defn- safe-re-seq
  "Shadow for `clojure.core/re-seq`: accepts a string pattern too."
  [re s]
  (re-seq (->pattern re) (str s)))

(defn- safe-re-matches
  "Shadow for `clojure.core/re-matches`: accepts a string pattern too."
  [re s]
  (re-matches (->pattern re) (str s)))

(defn- banned-slurp
  "Sandbox `slurp` override that rejects every call. Agent-facing file
   reads go through `read-file` exclusively — line-numbered output, path
   sanity, size cap, offset/limit paging, and var-source tracking all
   live there. `slurp` returns raw bytes with none of that, making it a
   cache-coherency footgun: a var bound from `slurp` can't be validated
   against the filesystem the next iteration, so the agent silently
   trusts stale content. Better to refuse the call and point at
   `read-file`."
  [& _args]
  (throw (ex-info (str "slurp is banned in the sandbox — use (read-file \"path\") "
                    "or (read-file \"path\" offset limit). "
                    "read-file is the only sanctioned file read: line-numbered, "
                    "size-capped, symlink-safe, and tracked by <var_index> for "
                    "staleness between iterations.")
           {:type :tool/banned :tool 'slurp})))

(defn- format-printable
  "If `v` carries :rlm/format / :rlm/formatted metadata (attached by the tool
   wrapper), return its formatted string. Otherwise return `v` unchanged.

   Used by the sandbox `println` / `print` overrides to render tool results
   with their tool-specific formatter instead of pr-str'ing the raw map.
   Non-IObj values (strings, numbers, keywords) pass through untouched —
   they can't carry metadata and usually render cleanly with plain println."
  [v]
  (if (instance? clojure.lang.IObj v)
    (or (:rlm/formatted (meta v))
      (when-let [f (:rlm/format (meta v))]
        (try (f v) (catch Throwable _ v)))
      v)
    v))

(defn- sandbox-println
  "Sandbox replacement for clojure.core/println. Substitutes formatted strings
   for any arg that was produced by a tool with a :format-result-fn formatter.

   SCI rebinds `sci.core/out` (NOT Clojure's `*out*`) per-iteration via
   `sci/binding`. SCI's built-in println handles this by rebinding `*out*`
   from `@sci.core/out` inside the fn body — we do the same so stdout
   capture keeps working with our override in place."
  [& args]
  (binding [*out* @sci/out]
    (apply println (map format-printable args))))

(defn- sandbox-print
  "Sandbox replacement for clojure.core/print. Same substitution as
   sandbox-println, without the trailing newline."
  [& args]
  (binding [*out* @sci/out]
    (apply print (map format-printable args))))


;; =============================================================================
;; SCI namespace / binding helpers (moved from shared.clj — sandbox-only)
;; =============================================================================

(defn ns->sci-map
  "Builds an SCI :namespaces entry map from a Clojure namespace's public vars."
  [ns-sym]
  (require ns-sym)
  (into {} (for [[sym v] (ns-publics (the-ns ns-sym))
                 :when (and (var? v) (not (:macro (meta v))))]
             [sym @v])))

(def EXTRA_BINDINGS
  "Extra bindings beyond what SCI provides by default."
  {'abs abs, 'parse-long parse-long, 'parse-double parse-double,
   'parse-boolean parse-boolean, 'parse-uuid parse-uuid,
   'infinite? infinite?, 'NaN? NaN?,
   'url-encode (fn ^String url-encode [^String s] (java.net.URLEncoder/encode s "UTF-8")),
   'url-decode (fn ^String url-decode [^String s] (java.net.URLDecoder/decode s "UTF-8")),
   'shape sci-shared/shape})

;; =============================================================================
;; Date tool bindings (moved from shared.clj — sandbox-only)
;; =============================================================================

(defn parse-date
  "Parses an ISO-8601 date string (YYYY-MM-DD)."
  [date-str]
  (try
    (when date-str (str (java.time.LocalDate/parse date-str)))
    (catch Exception _ nil)))

(defn date-before? [date1 date2]
  (try
    (when (and date1 date2)
      (.isBefore (java.time.LocalDate/parse date1) (java.time.LocalDate/parse date2)))
    (catch Exception _ false)))

(defn date-after? [date1 date2]
  (try
    (when (and date1 date2)
      (.isAfter (java.time.LocalDate/parse date1) (java.time.LocalDate/parse date2)))
    (catch Exception _ false)))

(defn days-between [date1 date2]
  (try
    (when (and date1 date2)
      (.between java.time.temporal.ChronoUnit/DAYS
        (java.time.LocalDate/parse date1) (java.time.LocalDate/parse date2)))
    (catch Exception _ nil)))

(defn date-plus-days [date-str days]
  (try
    (when date-str (str (.plusDays (java.time.LocalDate/parse date-str) days)))
    (catch Exception _ nil)))

(defn date-minus-days [date-str days]
  (try
    (when date-str (str (.minusDays (java.time.LocalDate/parse date-str) days)))
    (catch Exception _ nil)))

(defn date-format [date-str pattern]
  (try
    (when (and date-str pattern)
      (let [formatter (java.time.format.DateTimeFormatter/ofPattern pattern)
            date (java.time.LocalDate/parse date-str)]
        (.format date formatter)))
    (catch Exception _ nil)))

(defn today-str [] (str (java.time.LocalDate/now)))


(defn create-sci-context
  "Creates the SCI sandbox context with all available bindings.

   Params:
   `custom-bindings` - Map of symbol->value for custom bindings (can be nil)"
  [custom-bindings]
  (let [base-bindings {                       ;; Formatter-aware println/print — substitute the tool's
                       ;; :format-result-fn output for args carrying :rlm/format meta.
                       ;; prn is intentionally NOT overridden: it's for data
                       ;; round-trip and must stay verbatim pr-str.
                       'println sandbox-println
                       'print sandbox-print
                       ;; LLM footgun shadows: auto-promote string→Pattern so
                       ;; (re-find "HITL" s), (re-seq "\\d+" s), etc. stop
                       ;; throwing ClassCastException late inside lazy seqs.
                       're-find safe-re-find
                       're-seq safe-re-seq
                       're-matches safe-re-matches
                       ;; `slurp` is BANNED. Every file read goes through
                       ;; `read-file` so var_index can track mtime/size and
                       ;; mark cached reads as `valid`/`stale`/`missing`
                       ;; between iterations. `slurp` bypassed all of that.
                       'slurp banned-slurp
                       ;; Date tools
                       'parse-date parse-date
                       'date-before? date-before?
                       'date-after? date-after?
                       'days-between days-between
                       'date-plus-days date-plus-days
                       'date-minus-days date-minus-days
                       'date-format date-format
                       'today-str today-str}
        all-bindings (merge EXTRA_BINDINGS base-bindings
                       (or custom-bindings {}))
        str-ns  (sci/create-ns 'clojure.string nil)
        set-ns  (sci/create-ns 'clojure.set nil)
        walk-ns (sci/create-ns 'clojure.walk nil)
        plus-ns (sci/create-ns 'clojure+.core nil)
        ;; Patch clojure.string/split so string delimiters auto-promote to
        ;; Patterns. The original raises a late ClassCastException when an
        ;; LLM passes a string, usually after the cast hides inside a lazy
        ;; seq that only realizes during answer/mustache rendering.
        str-ns-copied (assoc (sci/copy-ns clojure.string str-ns)
                        'split (sci/new-var 'split safe-split {:ns str-ns}))
        zp-resolve (fn [sym] (deref (requiring-resolve (symbol "zprint.core" (str sym)))))
        lt-resolve (fn [sym] (deref (requiring-resolve (symbol "lazytest.core" (str sym)))))
        sandbox-ns (sci/create-ns 'sandbox nil)
        sci-ctx (sci/init
                  (sci-future/install {:namespaces {'sandbox (merge all-bindings
                                                               {'cond+ (sci/copy-var clojure+.core/cond+ sandbox-ns)
                                                                'if+ (sci/new-var 'if+
                                                                       (fn [_ _ bindings then & [else]]
                                                                         (list 'let [(first bindings) (second bindings)]
                                                                           (list 'if (first bindings) then else)))
                                                                       {:macro true})
                                                                'when+ (sci/new-var 'when+
                                                                         (fn [_ _ bindings & body]
                                                                           (list 'let [(first bindings) (second bindings)]
                                                                             (cons 'when (cons (first bindings) body))))
                                                                         {:macro true})})
                                                    'clojure.string str-ns-copied
                                                    'clojure.set (sci/copy-ns clojure.set set-ns)
                                                    'clojure.walk (sci/copy-ns clojure+.walk walk-ns)
                                                    'clojure+.core (sci/copy-ns clojure+.core plus-ns)
                                                    'fast-edn.core (ns->sci-map 'fast-edn.core)
                                                    'clojure.edn (ns->sci-map 'fast-edn.core)
                                                    'zprint.core {'zprint-str (zp-resolve 'zprint-str)
                                                                  'zprint (zp-resolve 'zprint)
                                                                  'czprint-str (zp-resolve 'czprint-str)
                                                                  'czprint (zp-resolve 'czprint)
                                                                  'zprint-file-str (zp-resolve 'zprint-file-str)
                                                                  'set-options! (zp-resolve 'set-options!)
                                                                  'configure-all! (zp-resolve 'configure-all!)}
                                                    'clojure.pprint {'pprint (zp-resolve 'zprint)
                                                                     'pprint-str (zp-resolve 'zprint-str)}
                                                    'lazytest.core {'expect-fn (lt-resolve 'expect-fn)
                                                                    'ok? (lt-resolve 'ok?)
                                                                    'throws? (lt-resolve 'throws?)
                                                                    'causes? (lt-resolve 'causes?)
                                                                    'causes-with-msg? (lt-resolve 'causes-with-msg?)}
                                                    'clojure.test {'is (lt-resolve 'expect-fn)
                                                                   'throws? (lt-resolve 'throws?)}
                                                    'charred.api (ns->sci-map 'charred.api)}
                                       :readers {'p (fn [form]
                                                      (list 'do
                                                        (list 'println (str "#p " (pr-str form) " =>") (list 'pr-str form))
                                                        form))}
                                       :ns-aliases {'str 'clojure.string
                                                    'edn 'fast-edn.core
                                                    'zp 'zprint.core
                                                    'pprint 'clojure.pprint
                                                    'pp 'clojure.pprint
                                                    'set 'clojure.set
                                                    'walk 'clojure.walk
                                                    'json 'charred.api
                                                    'lt 'lazytest.core
                                                    'test 'clojure.test
                                                    'c+ 'clojure+.core}
                                       :classes {'java.lang.Character Character
                                                 'java.lang.Math Math
                                                 'java.lang.String String
                                                 'java.lang.StringBuilder java.lang.StringBuilder
                                                 'java.lang.Integer Integer
                                                 'java.lang.Long Long
                                                 'java.lang.Double Double
                                                 'java.lang.Float Float
                                                 'java.lang.Byte java.lang.Byte
                                                 'java.lang.Short java.lang.Short
                                                 'java.lang.Boolean Boolean
                                                 'java.lang.Comparable java.lang.Comparable
                                                 'java.lang.Number java.lang.Number
                                                 'java.lang.Exception java.lang.Exception
                                                 'java.util.Collections java.util.Collections
                                                 'java.util.Arrays java.util.Arrays
                                                 'java.util.regex.Pattern java.util.regex.Pattern
                                                 'java.util.regex.Matcher java.util.regex.Matcher
                                                 'java.time.LocalDate java.time.LocalDate
                                                 'java.time.Period java.time.Period
                                                 'java.time.Instant java.time.Instant
                                                 'java.time.LocalDateTime java.time.LocalDateTime
                                                 'java.time.format.DateTimeFormatter java.time.format.DateTimeFormatter
                                                 'java.util.UUID java.util.UUID
                                                 'clojure.lang.PersistentQueue clojure.lang.PersistentQueue
                                                 'clojure.lang.BigInt clojure.lang.BigInt
                                                 'clojure.lang.Ratio clojure.lang.Ratio
                                                 'java.math.BigInteger java.math.BigInteger
                                                 'java.math.BigDecimal java.math.BigDecimal
                                                 'java.util.Base64 java.util.Base64
                                                 'java.net.URLEncoder java.net.URLEncoder
                                                 'java.net.URLDecoder java.net.URLDecoder
                                                 'java.util.Map java.util.Map
                                                 'java.util.List java.util.List
                                                 'java.util.HashMap java.util.HashMap
                                                 'java.util.LinkedHashMap java.util.LinkedHashMap
                                                 'java.util.ArrayList java.util.ArrayList
                                                 'java.util.Random java.util.Random}
                                       :imports '{Boolean java.lang.Boolean
                                                  Byte java.lang.Byte
                                                  Character java.lang.Character
                                                  Comparable java.lang.Comparable
                                                  Double java.lang.Double
                                                  Exception java.lang.Exception
                                                  Float java.lang.Float
                                                  Integer java.lang.Integer
                                                  Long java.lang.Long
                                                  Math java.lang.Math
                                                  Number java.lang.Number
                                                  Short java.lang.Short
                                                  String java.lang.String
                                                  StringBuilder java.lang.StringBuilder
                                                  Arrays java.util.Arrays
                                                  Collections java.util.Collections
                                                  UUID java.util.UUID
                                                  Pattern java.util.regex.Pattern
                                                  Matcher java.util.regex.Matcher
                                                  LocalDate java.time.LocalDate
                                                  LocalDateTime java.time.LocalDateTime
                                                  Instant java.time.Instant
                                                  DateTimeFormatter java.time.format.DateTimeFormatter
                                                  Period java.time.Period
                                                  PersistentQueue clojure.lang.PersistentQueue
                                                  BigInt clojure.lang.BigInt
                                                  Ratio clojure.lang.Ratio
                                                  BigInteger java.math.BigInteger
                                                  BigDecimal java.math.BigDecimal
                                                  Base64 java.util.Base64
                                                  URLEncoder java.net.URLEncoder
                                                  URLDecoder java.net.URLDecoder
                                                  Map java.util.Map
                                                  List java.util.List
                                                  HashMap java.util.HashMap
                                                  LinkedHashMap java.util.LinkedHashMap
                                                  ArrayList java.util.ArrayList
                                                  Random java.util.Random}
                                               ;; `slurp` intentionally NOT denied at the SCI level: we
                                               ;; shadow it in sandbox bindings with `banned-slurp`,
                                               ;; which throws a descriptive ex-info pointing at
                                               ;; `read-file`. Keeping it as a sandbox binding (not a
                                               ;; deny) gives the LLM a useful error message instead of
                                               ;; SCI's generic "not allowed". `spit` stays denied —
                                               ;; `write-file` is the audited path that renders diffs.
                                               ;;
                                               ;; `require`, `import`, `find-ns` are NOT denied either
                                               ;; (real Clojure reach for namespace discovery), but we
                                               ;; deliberately don't advertise them in tool docs so the
                                               ;; LLM's canonical playbook stays narrow.
                                       :deny '[ns eval load-string load-file
                                               read-string
                                               spit
                                               intern
                                               sh
                                               *in* *out* *err* *command-line-args*]}))]
    {:sci-ctx sci-ctx
     :sandbox-ns sandbox-ns
     :initial-ns-keys (set (keys (:val (sci/eval-string+ sci-ctx "(ns-publics 'sandbox)" {:ns sandbox-ns}))))}))

;; =============================================================================
;; Restore-var factory
;; =============================================================================

(defn make-restore-var-fn
  "Creates a restore-var function that fetches the latest persisted data var
   from prior iterations. Returns the raw value — callers that want to rebind
   into the SCI sandbox must also call `bind-and-bump!`."
  [db-info conversation-id]
  (fn restore-var
    ([sym] (restore-var sym {}))
    ([sym opts]
     (if db-info
       (let [sym (if (symbol? sym) sym (symbol (str sym)))
             registry (db/db-latest-var-registry db-info conversation-id
                        (select-keys (or opts {}) [:max-scan-queries]))]
         (if-let [{:keys [value]} (get registry sym)]
           value
           (throw (ex-info (str "No restorable var found for " sym)
                    {:type :rlm/restore-var-missing :symbol sym}))))
       (throw (ex-info "No DB available for restore-var" {:type :rlm/no-db}))))))

;; =============================================================================
;; Conversation history factories
;; =============================================================================

(defn- resolve-query-id [db-info conversation-id query-selector]
  (let [history (db/db-query-history db-info conversation-id)]
    (cond
      (nil? query-selector) (some-> history last :query-id)
      (integer? query-selector) (some->> history (filter #(= (:query-pos %) query-selector)) first :query-id)
      (and (vector? query-selector) (= :id (first query-selector))) [:id (second query-selector)]
      (uuid? query-selector) [:id query-selector]
      :else nil)))

(defn make-conversation-history-fn
  "Creates conversation-history for browsing prior query summaries."
  [db-info conversation-id]
  (fn conversation-history
    ([] (conversation-history nil))
    ([n]
     (if db-info
       (let [history (db/db-query-history db-info conversation-id)
             selected (if (some? n) (take-last (max 0 (long n)) history) history)]
         (mapv #(select-keys % [:query-pos :query-id :query :answer-preview :status :iterations :key-vars :created-at])
           selected))
       []))))

(defn make-conversation-code-fn
  "Creates conversation-code for browsing prior query code blocks."
  [db-info conversation-id]
  (fn conversation-code [query-selector]
    (if db-info
      (if-let [query-id (resolve-query-id db-info conversation-id query-selector)]
        (db/db-query-code db-info query-id) [])
      [])))

(defn make-conversation-results-fn
  "Creates conversation-results for browsing prior query results and restorable vars."
  [db-info conversation-id]
  (fn conversation-results [query-selector]
    (if db-info
      (if-let [query-id (resolve-query-id db-info conversation-id query-selector)]
        (db/db-query-results db-info query-id) [])
      [])))

;; =============================================================================
;; Var history + diff
;; =============================================================================

(defn make-var-history-fn
  "Creates var-history: all persisted versions of a var, oldest first."
  [db-info conversation-id]
  (fn var-history [sym]
    (let [sym (if (symbol? sym) sym (symbol (str sym)))]
      (if (and db-info conversation-id)
        (let [entries (db/db-var-history db-info conversation-id sym)]
          (if (seq entries)
            entries
            (throw (ex-info (str "No history found for " sym)
                     {:type :rlm/var-history-empty :symbol sym}))))
        (throw (ex-info "No DB available for var-history" {:type :rlm/no-db}))))))

(defn- editscript->summary [script]
  (mapv (fn [[path op val]]
          (case op
            :+ {:op :added   :path path :value val}
            :- {:op :removed :path path}
            :r {:op :changed :path path :value val}))
    (es/get-edits script)))

(defn- diff-strings [a b]
  (let [old-lines (vec (str/split-lines (str a)))
        new-lines (vec (str/split-lines (str b)))
        patch     (DiffUtils/diff old-lines new-lines)
        unified   (vec (UnifiedDiffUtils/generateUnifiedDiff
                         "v-from" "v-to" old-lines patch 3))]
    {:type       :string-diff
     :unified    (str/join "\n" unified)
     :edit-count (count (.getDeltas patch))}))

(defn- diff-numbers [a b]
  (let [delta (- (double b) (double a))]
    {:type       :number-delta
     :from       a
     :to         b
     :delta      delta
     :pct-change (when-not (zero? (double a))
                   (* 100.0 (/ delta (double a))))}))

(defn- diff-values [from to]
  (cond
    (and (db/diffable-value? from) (db/diffable-value? to))
    (let [script (es/diff from to)]
      {:type       :structural
       :edit-count (es/edit-distance script)
       :edits      (editscript->summary script)})
    (and (string? from) (string? to))
    (diff-strings from to)
    (and (number? from) (number? to))
    (diff-numbers from to)
    :else
    {:type :replacement :from from :to to}))

(defn make-var-diff-fn
  "Creates var-diff: diff between two versions of a var."
  [db-info conversation-id]
  (fn var-diff [sym v1 v2]
    (let [sym (if (symbol? sym) sym (symbol (str sym)))
          history (db/db-var-history db-info conversation-id sym)]
      (when (empty? history)
        (throw (ex-info (str "No history found for " sym)
                 {:type :rlm/var-history-empty :symbol sym})))
      (let [get-ver (fn [v]
                      (or (some #(when (= (:version %) v) %) history)
                        (throw (ex-info (str "Version " v " not found for " sym
                                          ". Available: " (mapv :version history))
                                 {:type :rlm/var-version-missing :symbol sym :version v}))))
            entry-from (get-ver v1)
            entry-to   (get-ver v2)]
        (merge {:from-version v1 :to-version v2}
          (diff-values (:value entry-from) (:value entry-to)))))))

(def ^:private ^:const MAX_VAR_INDEX_COUNT 1000)
(def ^:private ^:const MAX_VAR_INDEX_CODE_CHARS
  "Per-row budget for the `code` column in <var_index>. We show the
   CODE that produced each var (not a preview of its value) — the
   journal already carries full values for the last iteration, and
   the model can always re-reference a var by name to materialize
   its value into the next journal. Code is bounded by what the
   model typed, so this cap is only a safety rail against pathological
   one-liners, not a lossy summary."
  400)

(def ^:private SYSTEM_VAR_CODE_PLACEHOLDER
  "(SYSTEM — bound each turn by the agent loop)")

(defn parse-rich-code
  "Decode the `code` column from the `iteration_var_attrs` DB row into
   a uniform map `{:expr :time-ms :metadata}`.

   New writes use `pr-str {:expr … :time-ms … :metadata …}`. Legacy
   writes (and user-supplied raw expressions) are bare strings. We
   accept both. Unparseable inputs degrade to `{:expr <raw>}` so we
   never lose provenance."
  [code-col]
  (cond
    (nil? code-col) nil

    (map? code-col)
    code-col

    (string? code-col)
    (if (str/starts-with? (str/triml code-col) "{")
      (try
        (let [parsed (read-string {:read-cond :allow :features #{:clj}} code-col)]
          (if (map? parsed) parsed {:expr code-col}))
        (catch Throwable _ {:expr code-col}))
      {:expr code-col})

    :else nil))

(defn- render-var-code
  "Render the `code` column entry for one var. We show CODE (plus
   `:time-ms` when known), not a preview of the value — the journal
   already carries the previous iteration's full values and the model
   can reference any var by name to materialize its current value
   into the NEXT journal. Preview columns forced the model to re-read
   files it had already stored (see conversation 1f44852d-…) because
   a bounded slice is ambiguous for structured data like source files
   or hiccup trees."
  [{:keys [system? expr time-ms]}]
  (cond
    system? SYSTEM_VAR_CODE_PLACEHOLDER

    (and (string? expr) (not (str/blank? expr)))
    (let [trimmed (sci-shared/truncate (str/trim expr) MAX_VAR_INDEX_CODE_CHARS)]
      (if time-ms
        (str trimmed " [" time-ms "ms]")
        trimmed))

    :else "-"))

(defn- render-var-status
  "Render the `status` column entry — a GC-grade lifecycle hint for
   the agent.

   Values:
     live       — bound in the current sandbox; usable by name.
     PERSISTED  — not live in the sandbox but the DB row survives;
                  `(restore-var 'sym)` brings it back.
     SYSTEM     — earmuffed (*query*, *reasoning*, …) — always live,
                  re-bound every turn.

   The agent uses this to decide whether to reclaim a persisted var
   instead of recomputing, and which vars are safe to ignore."
  [{:keys [system? persisted?]}]
  (cond
    system?    "SYSTEM"
    persisted? "forgotten"
    :else      "live"))

(defn build-var-index
  "Builds a formatted var index table from user-def'd vars in the SCI context.
   Filters out initial bindings (tools, helpers) using initial-ns-keys.
   Returns nil if no user vars exist.

   Row format: `name | ver | type | size | status | doc | code`

   The `code` column shows the DEFINING expression for each var plus
   its execution time in ms (`(def x …) [42ms]`) so the model has
   complete provenance without a lossy value preview. For the raw
   value the LLM references the var by name in its own code — the
   next <journal> then shows the full materialized value.

   All user vars appear — no `MAX_VAR_INDEX_ROWS` cap. SYSTEM vars
   sort first; the rest are newest-touched first.

   Opts (6-arity):
     :include-persisted? — when false, rows persisted in DB but NOT in
       the live sandbox are suppressed. Set by sub-RLM calls so a
       forked env only sees its own snapshot + its own new vars; it
       never leaks sibling queries' vars into the sub's <var_index>.
       Defaults to true (main RLM — full conversation view)."
  ([sci-ctx initial-ns-keys]
   (build-var-index sci-ctx initial-ns-keys nil nil nil nil))
  ([sci-ctx initial-ns-keys sandbox]
   (build-var-index sci-ctx initial-ns-keys sandbox nil nil nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-id]
   (build-var-index sci-ctx initial-ns-keys sandbox db-info conversation-id nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-id
    {:keys [include-persisted?] :or {include-persisted? true}}]
   (let [sandbox-map (or sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox]))
         var-registry (when (and db-info conversation-id)
                        (db/db-latest-var-registry db-info conversation-id))
         recency-of (fn [sym]
                      (if-let [ts (some-> (get var-registry sym) :created-at)]
                        (cond (inst? ts) (inst-ms ts)
                          (integer? ts) (long ts)
                          :else Long/MAX_VALUE)
                        Long/MAX_VALUE))
           ;; The `code` column in the DB carries rich `{:expr :time-ms
           ;; :metadata}` maps for new writes and bare strings for
           ;; legacy rows — `parse-rich-code` smooths the two.
         rich-code-for (fn [sym]
                         (parse-rich-code (get-in var-registry [sym :code])))
         live-info (into {}
                     (for [[s v] sandbox-map
                           :when (symbol? s)]
                       (let [rich (rich-code-for s)]
                         [s {:val (if (instance? clojure.lang.IDeref v) @v v)
                             :doc (:doc (meta v))
                             :arglists (:arglists (meta v))
                             :version (get-in var-registry [s :version] 1)
                             :expr (or (:expr rich) (:rlm/def-source (meta v)))
                             :time-ms (:time-ms rich)}])))
           ;; Sub-RLMs (env with :parent-iteration-id set → caller
           ;; passes :include-persisted? false) see ONLY their forked
           ;; sandbox. Sibling queries in the same conversation must
           ;; not leak into a sub-RLM's <var_index>, otherwise a
           ;; sub-sub-RLM would drown in its grandparent's junk.
         persisted-info (when (and var-registry include-persisted?)
                          (into {}
                            (for [[sym _] var-registry
                                  :when (and (symbol? sym)
                                          (not (contains? live-info sym))
                                          (not (contains? initial-ns-keys sym)))]
                              (let [rich (rich-code-for sym)]
                                [sym {:val ::persisted
                                      :doc (str "persisted - (restore-var '" sym ") to load")
                                      :expr (:expr rich)
                                      :time-ms (:time-ms rich)}]))))
         var-info (merge persisted-info live-info)
         earmuffed? (fn [sym]
                      (let [n (name sym)]
                        (and (> (count n) 2)
                          (str/starts-with? n "*")
                          (str/ends-with? n "*"))))
         system-doc (fn [sym]
                      (case (str sym)
                        "*query*"     "(SYSTEM, never forgotten) current user query"
                        "*reasoning*" "(SYSTEM, never forgotten) YOUR thinking from the previous iteration"
                        "*answer*"    "(SYSTEM, never forgotten) final answer from the previous turn in this conversation"
                        "(SYSTEM, never forgotten) agent-bound var"))
         entries (->> var-info
                   (remove (fn [[sym _]] (contains? initial-ns-keys sym)))
                   (sort-by (fn [[sym _]]
                              [(if (earmuffed? sym) 0 1)
                               (- (long (recency-of sym)))
                               (str sym)]))
                   (mapv (fn [[sym {:keys [val doc arglists expr time-ms]}]]
                           (let [persisted? (= val ::persisted)
                                 system?    (earmuffed? sym)
                                 type-label (cond
                                              persisted? "persisted"
                                              (nil? val) "nil"
                                              (fn? val) (if arglists (str "fn " arglists) "fn")
                                              (map? val) "map"
                                              (vector? val) "vector"
                                              (set? val) "set"
                                              (sequential? val) "seq"
                                              (string? val) "string"
                                              (integer? val) "int"
                                              (float? val) "float"
                                              (boolean? val) "bool"
                                              (keyword? val) "keyword"
                                              (symbol? val) "symbol"
                                              :else (.getSimpleName (class val)))
                                 size (cond
                                        persisted? "-"
                                        (nil? val) "-"
                                        (string? val) (str (count val) " chars")
                                        (or (map? val) (vector? val) (set? val))
                                        (str (count val) " items")
                                        (sequential? val)
                                        (let [n (bounded-count MAX_VAR_INDEX_COUNT val)]
                                          (if (= n MAX_VAR_INDEX_COUNT)
                                            (str MAX_VAR_INDEX_COUNT "+ items")
                                            (str n " items")))
                                        :else "-")
                                 code-col      (render-var-code {:system? system? :expr expr :time-ms time-ms})
                                 status-col    (render-var-status {:system? system? :persisted? persisted?})
                                 version (get-in var-info [sym :version] 1)
                                 ver-str (str version)]
                             {:name (str sym) :ver ver-str :type type-label :size size
                              :status status-col
                              :doc (cond
                                     system? (sci-shared/truncate (system-doc sym) 80)
                                     doc (sci-shared/truncate doc 80)
                                     :else "-")
                              :code code-col
                              :system? system?}))))]
     (when (seq entries)
       (let [visible entries   ;; full index — no MAX_VAR_INDEX_ROWS cap
             max-name (max 4 (apply max (map #(count (:name %)) visible)))
             max-ver  (max 3 (apply max (map #(count (:ver %)) visible)))
             max-type (max 4 (apply max (map #(count (:type %)) visible)))
             max-size (max 4 (apply max (map #(count (:size %)) visible)))
             max-status (max 6 (apply max (map #(count (:status %)) visible)))
             max-doc  (max 3 (apply max (map #(count (:doc %)) visible)))
             pad (fn [s n] (str s (apply str (repeat (max 0 (- n (count s))) \space))))
             header (str "  " (pad "name" max-name) " | " (pad "ver" max-ver)
                      " | " (pad "type" max-type) " | " (pad "size" max-size)
                      " | " (pad "status" max-status)
                      " | " (pad "doc" max-doc)
                      " | code")
             sep (str "  " (apply str (repeat max-name \-)) "-+-"
                   (apply str (repeat max-ver \-)) "-+-"
                   (apply str (repeat max-type \-)) "-+-"
                   (apply str (repeat max-size \-)) "-+-"
                   (apply str (repeat max-status \-)) "-+-"
                   (apply str (repeat max-doc \-)) "-+---------")
             rows (mapv (fn [{:keys [name ver type size status doc code]}]
                          (str "  " (pad name max-name)
                            " | " (pad ver max-ver)
                            " | " (pad type max-type)
                            " | " (pad size max-size)
                            " | " (pad status max-status)
                            " | " (pad doc max-doc)
                            " | " code))
                    visible)]
         (str/join "\n" (concat [header sep] rows)))))))
