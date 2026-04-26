(ns com.blockether.vis.loop.runtime.conversation.environment.core
  "Environment orchestration: SCI sandbox, var-index, extension system."
  (:require
   [clojure.set]
   [clojure.string :as str]
   [clojure.walk]
   [clojure+.core]
   [clojure+.walk]
   [com.blockether.vis.persistance.core :as db]
   [sci.addons.future :as sci-future]
   [sci.core :as sci]
   [taoensso.telemere :as tel]))

(defn- truncate-str [s n]
  (let [s (str s)] (if (> (count s) n) (subs s 0 n) s)))

(defn- shape [v]
  (cond (map? v) :map (vector? v) :vector (set? v) :set (list? v) :list
    (string? v) :string (number? v) :number (keyword? v) :keyword
    (symbol? v) :symbol (nil? v) :nil :else :value))

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

(defn- sandbox-println
  "Sandbox replacement for clojure.core/println.

   SCI rebinds `sci.core/out` (NOT Clojure's `*out*`) per-iteration via
   `sci/binding`. SCI's built-in println handles this by rebinding `*out*`
   from `@sci.core/out` inside the fn body — we do the same so stdout
   capture keeps working with our override in place."
  [& args]
  (binding [*out* @sci/out]
    (apply println args)))

(defn- sandbox-print
  "Sandbox replacement for clojure.core/print.
   Rebinds `*out*` from SCI's out for stdout capture."
  [& args]
  (binding [*out* @sci/out]
    (apply print args)))

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
   'shape shape})

(defn create-sci-context
  "Creates the SCI sandbox context with all available bindings.

   Params:
   `custom-bindings` - Map of symbol->value for custom bindings (can be nil)"
  [custom-bindings]
  (let [base-bindings {;; prn is intentionally NOT overridden: it's for data
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
                       'slurp banned-slurp}
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

(def ^:private ^:const MAX_VAR_INDEX_COUNT 1000)

;; SYSTEM vars are the read-only bindings the loop maintains for the
;; agent: QUERY (current user query), REASONING (last iteration's
;; thinking), ANSWER (previous turn's final answer). UPPERCASE matches
;; the Clojure idiom for constants and avoids the earmuff misread
;; (earmuffs imply dynamic-var semantics, which these are not).
;;
;; The set is a fixed registry; adding to it is a deliberate API
;; change, not a free-form pattern. See AGENTS.md → "SYSTEM vars are
;; UPPERCASE and explicitly defined".
(def SYSTEM_VAR_NAMES
  "Fixed set of SYSTEM-var symbols. Used everywhere a 'is-this-a-system-
   var?' check is needed: var-index sort+status, auto-forget guard,
   <system_state> rendering, etc."
  '#{QUERY REASONING ANSWER})

(defn system-var-sym?
  "True when `sym` is one of the registered SYSTEM-var names. The fixed
   set `SYSTEM_VAR_NAMES` is checked by membership, not pattern, so a
   user-defined uppercase var doesn't get misclassified as system."
  [sym]
  (contains? SYSTEM_VAR_NAMES sym))

(defn- var-status-keyword [{:keys [system? persisted?]}]
  (cond
    system? :sys
    persisted? :f
    :else :l))

(defn- var-type-keyword [val persisted?]
  (cond
    persisted? :persisted
    (nil? val) :nil
    (fn? val) :fn
    (map? val) :map
    (vector? val) :vector
    (set? val) :set
    (list? val) :list
    (sequential? val) :seq
    (string? val) :string
    (integer? val) :int
    (float? val) :float
    (boolean? val) :bool
    (keyword? val) :keyword
    (symbol? val) :symbol
    :else (some-> val class .getSimpleName str/lower-case keyword)))

(defn- var-size [val persisted?]
  (cond
    persisted? nil
    (nil? val) nil
    (string? val) (count val)
    (or (map? val) (vector? val) (set? val)) (count val)
    (sequential? val)
    (let [n (bounded-count MAX_VAR_INDEX_COUNT val)]
      (if (= n MAX_VAR_INDEX_COUNT) (str MAX_VAR_INDEX_COUNT "+") n))
    :else nil))

(defn- render-var-form
  [{:keys [sym version status type size arglists]}]
  (let [meta-map (cond-> {:v version :s status}
                   (and type (not= status :sys)) (assoc :t type)
                   (some? size) (assoc :n size))
        single-arglist? (and (sequential? arglists)
                          (= 1 (count arglists))
                          (vector? (first arglists)))
        fn-form? (= type :fn)]
    (if fn-form?
      (if single-arglist?
        (str "(defn ^" (pr-str meta-map) " " sym " " (pr-str (first arglists)) " ...)")
        (str "(defn ^"
          (pr-str (cond-> meta-map
                    (seq arglists) (assoc :args arglists)))
          " " sym " ...)"))
      (str "(def ^" (pr-str meta-map) " " sym " ...)"))))

(defn build-var-index
  "Builds a compact pseudo-source `<var_index>` from user-defined vars in the
   SCI context. Filters out initial bindings (tools, helpers) using
   `initial-ns-keys`. Returns nil if no user vars exist.

   Render format:
     (def  ^{:v N :s :l|:f|:sys :t kw :n size?} sym ...)
     (defn ^{:v N :s :l|:f|:sys :t :fn :n size?} sym [args] ...)

   The block is intentionally index-like, not executable source: `...` bodies
   save tokens because the actual defining expressions already live in the DB,
   journal, and `var-history`. SYSTEM vars sort first; the rest are
   newest-touched first. Forgotten vars remain visible so the model knows they
   exist and can call `(var-history 'sym)` to inspect old values."
  ([sci-ctx initial-ns-keys]
   (build-var-index sci-ctx initial-ns-keys nil nil nil nil))
  ([sci-ctx initial-ns-keys sandbox]
   (build-var-index sci-ctx initial-ns-keys sandbox nil nil nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-id]
   (build-var-index sci-ctx initial-ns-keys sandbox db-info conversation-id nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-id _opts]
   (let [sandbox-map (or sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox]))
         var-registry (when (and db-info conversation-id)
                        (db/db-latest-var-registry db-info conversation-id))
         recency-of (fn [sym]
                      (if-let [ts (some-> (get var-registry sym) :created-at)]
                        (cond (inst? ts) (inst-ms ts)
                          (integer? ts) (long ts)
                          :else Long/MAX_VALUE)
                        Long/MAX_VALUE))
         live-info (into {}
                     (for [[s v] sandbox-map
                           :when (symbol? s)]
                       [s {:val (if (instance? clojure.lang.IDeref v) @v v)
                           :arglists (:arglists (meta v))
                           :version (get-in var-registry [s :version] 1)}]))
         persisted-info (when var-registry
                          (into {}
                            (for [[sym _] var-registry
                                  :when (and (symbol? sym)
                                          (not (contains? live-info sym))
                                          (not (contains? initial-ns-keys sym)))]
                              [sym {:val ::persisted
                                    :version (get-in var-registry [sym :version] 1)}])))
         var-info (merge persisted-info live-info)
         entries (->> var-info
                   (remove (fn [[sym _]] (contains? initial-ns-keys sym)))
                   ;; Phase 1: SYSTEM vars (QUERY/REASONING/ANSWER) live in
                   ;; <system_state> with their current values inlined.
                   ;; Drop them from <var_index> entirely so the user-facing
                   ;; var list is just user-defined state.
                   (remove (fn [[sym _]] (system-var-sym? sym)))
                   (sort-by (fn [[sym _]]
                              [(- (long (recency-of sym)))
                               (str sym)]))
                   (mapv (fn [[sym {:keys [val arglists version]}]]
                           (let [persisted? (= val ::persisted)
                                 system?    false]
                             {:sym sym
                              :version version
                              :status (var-status-keyword {:system? system? :persisted? persisted?})
                              :type (var-type-keyword val persisted?)
                              :size (var-size val persisted?)
                              :arglists arglists}))))]
     (when (seq entries)
       (str/join "\n" (map render-var-form entries))))))

;; =============================================================================
;; Sandbox restore — rebuild SCI bindings from DB
;; =============================================================================

(defn restore-sandbox!
  "Restore all persisted vars into a SCI sandbox from the DB.

   Reads `db-restore-expressions` (topologically sorted) and for each entry:
   - Data value (nippy-thawed) → bind directly into the sandbox.
   - `{:vis/ref :expr}` → eval the `:expr` source code in the sandbox.
     Dependencies are guaranteed to be bound first (topological order).

   Returns a vec of {:name :restored-via (:data | :eval) :success? :error}."
  [sci-ctx db-info conversation-id]
  (let [entries (db/db-restore-expressions db-info conversation-id)]
    (mapv (fn [{:keys [name expr result]}]
            (let [sym (symbol name)]
              (try
                (if (and (map? result) (= :expr (:vis/ref result)))
                  ;; Function / lazy seq / runtime object → re-eval source
                  (if (and expr (not= expr ";; SYSTEM var"))
                    (do (sci/eval-string+ sci-ctx expr
                          {:ns (sci/find-ns sci-ctx 'sandbox)})
                      {:name name :restored-via :eval :success? true})
                    ;; SYSTEM var with :vis/ref but no real expr — skip
                    {:name name :restored-via :skip :success? true})
                  ;; Data value → bind directly
                  (do (sci-update-binding! sci-ctx sym result)
                    {:name name :restored-via :data :success? true}))
                (catch Throwable e
                  (tel/log! {:level :warn :id ::restore-failed
                             :data {:name name :error (ex-message e)}}
                    (str "Failed to restore " name))
                  {:name name :restored-via :error :success? false
                   :error (ex-message e)}))))
      entries)))

