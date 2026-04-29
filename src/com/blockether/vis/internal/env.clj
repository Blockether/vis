(ns com.blockether.vis.internal.env
  "SCI sandbox machinery: var-index, sandbox bindings, restore.

   INTERNAL — only `com.blockether.vis.internal.loop` imports this namespace.
   Foundation utilities (storage facade, extension specs, format helpers,
   etc.) live across `com.blockether.vis.internal.{persistance,extension,config,registry}`. Channels and extensions never
   reach into here; they go through the public iteration entry points
   in `com.blockether.vis.internal.loop` (`send!`, `create!`, `query!`, …)."
  (:require
   [clojure.set]
   [clojure.string :as str]
   [clojure.walk]
   [clojure+.core]
   [clojure+.walk]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :as lazytest]
   [sci.addons.future :as sci-future]
   [sci.core :as sci]
   [taoensso.telemere :as tel]
   [zprint.core :as zprint]))

;; =============================================================================
;; SCI sandbox + var-index
;; =============================================================================

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
   bug (4-iteration `(restore-vars …)` spin, turn-3 `*query*=\"Siema\"` replay)
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
   reads go through `vis/cat` exclusively — line-numbered output, path
   sanity, size cap, offset/limit paging, and var-source tracking all
   live there. `slurp` returns raw bytes with none of that, making it a
   cache-coherency footgun: a var bound from `slurp` can't be validated
   against the filesystem the next iteration, so the agent silently
   trusts stale content. Better to refuse the call and point at
   `vis/cat`."
  [& _args]
  (throw (ex-info (str "slurp is banned in the sandbox — use (vis/cat \"path\") "
                    "or (vis/cat \"path\" offset limit). "
                    "vis/cat is the only sanctioned file read: line-numbered, "
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
                       ;; `vis/cat` so var_index can track mtime/size and
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
                                                    'zprint.core {'zprint-str    zprint/zprint-str
                                                                  'zprint        zprint/zprint
                                                                  'czprint-str   zprint/czprint-str
                                                                  'czprint       zprint/czprint
                                                                  'zprint-file-str zprint/zprint-file-str
                                                                  'set-options!  zprint/set-options!
                                                                  'configure-all! zprint/configure-all!}
                                                    'clojure.pprint {'pprint     zprint/zprint
                                                                     'pprint-str zprint/zprint-str}
                                                    'lazytest.core {'expect-fn       lazytest/expect-fn
                                                                    'ok?              lazytest/ok?
                                                                    'throws?          lazytest/throws?
                                                                    'causes?          lazytest/causes?
                                                                    'causes-with-msg? lazytest/causes-with-msg?}
                                                    'clojure.test {'is      lazytest/expect-fn
                                                                   'throws? lazytest/throws?}
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
                                               ;; `vis/cat`. Keeping it as a sandbox binding (not a
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

;; SYSTEM vars are read-only bindings the loop maintains for the
;; agent:
;;   QUERY                 current user query (string)
;;   REASONING             last iteration's thinking (string)
;;   ANSWER                previous turn's final answer (string)
;;   CURRENT_QUERY_ID      UUID of the in-flight turn (= current query)
;;   CURRENT_ITERATION_ID  UUID of the most recently persisted iteration
;;                         row (nil before the first iteration commits;
;;                         iteration N's :code sees iteration N-1's id
;;                         because the row for N is written AFTER eval)
;; UPPERCASE marks them as constants. The set is a fixed registry;
;; adding to it is a deliberate API change. See AGENTS.md → "SYSTEM
;; vars are UPPERCASE and explicitly defined".
(def SYSTEM_VAR_NAMES
  "Fixed set of SYSTEM-var symbols. Used everywhere a 'is-this-a-system-
   var?' check is needed: var-index sort+status, auto-forget guard,
   <system_state> rendering, etc."
  '#{QUERY REASONING ANSWER CURRENT_QUERY_ID CURRENT_ITERATION_ID})

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

;; ---------------------------------------------------------------------------
;; Type-aware var rendering
;;
;; Cheap values get their actual content inlined so the model never has to
;; round-trip via `(var-history 'sym)` just to see what's in a var. Expensive
;; values fall back to a schema preview (key list, head, size). The render
;; emits VALID Clojure shape — stats live in a `;;` comment line, never as
;; reader-macro metadata onto the symbol (the old `^{:v N :s :l :t :map}` form
;; was fake-Clojure that confused parser priors).
;; ---------------------------------------------------------------------------

(def ^:private STRING_INLINE_MAX_CHARS 8000)
(def ^:private STRING_HEAD_PREVIEW_CHARS 800)
(def ^:private MAP_INLINE_MAX_KEYS 8)
(def ^:private MAP_KEYS_SAMPLE_SIZE 6)
(def ^:private SEQ_INLINE_MAX_ELEMS 5)
(def ^:private SEQ_HEAD_SAMPLE_SIZE 3)
(def ^:private DOCSTRING_FIRST_LINE_CHARS 100)
;; Per-entry char cap when rendering collection bodies. A 5-element
;; vector of 1000-char strings would otherwise blow up to 5000+ chars
;; even though our element-count check passes. This is the SAFETY net.
(def ^:private VAR_INDEX_BODY_MAX_CHARS 600)
(def ^:private VAR_INDEX_PRINT_LENGTH 32)
(def ^:private VAR_INDEX_PRINT_LEVEL 6)

(defn- bounded-pr-str
  "Local mirror of `iteration.core/safe-pr-str` for the var-index render
   path — reuses `*print-length*` + `*print-level*` to short-circuit
   pr during printing, then clips the resulting string. Kept here so
   the env-core namespace doesn't depend on iteration.core (would be
   a cycle)."
  [v]
  (let [bounded (binding [*print-length* VAR_INDEX_PRINT_LENGTH
                          *print-level*  VAR_INDEX_PRINT_LEVEL]
                  (pr-str v))]
    (if (> (count bounded) VAR_INDEX_BODY_MAX_CHARS)
      (str (subs bounded 0 VAR_INDEX_BODY_MAX_CHARS)
        " …<+" (- (count bounded) VAR_INDEX_BODY_MAX_CHARS) " chars>")
      bounded)))

(defn- truncate-string [s n]
  (if (and (string? s) (> (count s) n)) (subs s 0 n) s))

(defn- map-keys-preview [val]
  (let [ks (vec (keys val))]
    (if (<= (count ks) MAP_INLINE_MAX_KEYS)
      {:inline? true :keys ks}
      {:inline? false
       :keys-sample (vec (take MAP_KEYS_SAMPLE_SIZE ks))
       :total (count ks)})))

(defn- seq-head-preview [val]
  (let [n (var-size val false)
        sample-count (cond
                       (string? n) SEQ_HEAD_SAMPLE_SIZE          ;; "1000+" — lazy/big
                       (number? n) (min n SEQ_HEAD_SAMPLE_SIZE)
                       :else SEQ_HEAD_SAMPLE_SIZE)
        head (vec (take sample-count val))]
    {:total n :head head}))

(defn- stats-comment
  "Render the per-entry stats as a single `;;` comment line. Replaces the
   fake-Clojure metadata-on-symbol form. `:scope` uses full words
   (`:live | :forgotten | :system`); `:v` is the persisted version count."
  [{:keys [version status size]}]
  (let [scope-word (case status
                     :sys :system
                     :f   :forgotten
                     :l   :live
                     status)]
    (str ";; v=" version " scope=" (name scope-word)
      (when (some? size) (str " n=" size)))))

(defn- render-fn-form
  [{:keys [sym arglists doc] :as entry}]
  (let [stats   (stats-comment entry)
        single? (and (sequential? arglists)
                  (= 1 (count arglists))
                  (vector? (first arglists)))
        doc-line (when (and (string? doc) (not (str/blank? doc)))
                   (truncate-string
                     (-> (str doc) str/split-lines first str/trim)
                     DOCSTRING_FIRST_LINE_CHARS))
        body (cond
               doc-line (str " \"" doc-line "\" …")
               :else " …")
        sig  (cond
               single? (pr-str (first arglists))
               (seq arglists) (str/join " " (map pr-str arglists))
               :else "[& args]")]
    (str stats "\n(defn " sym " " sig body ")")))

(defn- render-data-form
  "Render a non-fn var with type-aware preview. Cheap values inline; large
   values fall back to a schema map (`{:n N :keys-sample […]}` etc.).
   When a `:doc` is set on the var meta, embed the first line of the
   docstring before the body — same UX as render-fn-form, so all
   var-defining forms surface their purpose in `<var_index>`."
  [{:keys [sym type val doc] :as entry}]
  (let [stats (stats-comment entry)
        doc-line (when (and (string? doc) (not (str/blank? doc)))
                   (truncate-string
                     (-> (str doc) str/split-lines first str/trim)
                     DOCSTRING_FIRST_LINE_CHARS))
        body  (case type
                ;; Scalars: pr-str output is bounded by the type. A
                ;; literal `true` / `42` / `:foo` cannot blow up.
                :nil     "nil"
                :bool    (pr-str val)
                :int     (pr-str val)
                :float   (pr-str val)
                :keyword (pr-str val)
                :symbol  (pr-str val)
                ;; Strings: capped before pr-str, so the printed form is
                ;; bounded by STRING_HEAD_PREVIEW_CHARS + quotes.
                :string  (let [s val]
                           (if (<= (count s) STRING_INLINE_MAX_CHARS)
                             (pr-str s)
                             (pr-str {:string-size (count s)
                                      :head (truncate-string s STRING_HEAD_PREVIEW_CHARS)})))
                ;; Collections route through bounded-pr-str so an inline
                ;; render of a 5-element vec of 1000-char strings can't
                ;; produce a 5000+ char body.
                :map     (let [{:keys [inline? keys keys-sample total]} (map-keys-preview val)]
                           (if inline?
                             (bounded-pr-str {:keys keys})
                             (bounded-pr-str {:n total :keys-sample keys-sample})))
                (:vector :set :list :seq)
                (let [{:keys [total head]} (seq-head-preview val)]
                  (cond
                    (and (number? total) (<= total SEQ_INLINE_MAX_ELEMS))
                    (bounded-pr-str (vec val))
                    :else
                    (bounded-pr-str {:n total :head head})))
                ;; unknown type — punt to a value-only stub
                (pr-str {:type type}))]
    (str stats
      "\n(def " sym
      (when doc-line (str " \"" doc-line "\""))
      " " body ")")))

(defn- render-var-form
  "Live-var render path. Persisted-only entries are routed to the archive
   block by the caller; this function should only see live entries."
  [{:keys [type] :as entry}]
  (if (= type :fn)
    (render-fn-form entry)
    (render-data-form entry)))

(defn build-var-index
  "Build the `<var_index>` block from user-defined vars in the SCI sandbox.

   Returns nil when no user vars exist; otherwise a multi-line string
   with one entry per `(def ...)`. SYSTEM vars (QUERY / REASONING /
   ANSWER) and initial-ns bindings (tools, helpers) are excluded —
   the model reads SYSTEM vars by name directly from the sandbox.

   Sort order: most-recently-bound first."
  ([sci-ctx initial-ns-keys]
   (build-var-index sci-ctx initial-ns-keys nil nil nil nil))
  ([sci-ctx initial-ns-keys sandbox]
   (build-var-index sci-ctx initial-ns-keys sandbox nil nil nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-id]
   (build-var-index sci-ctx initial-ns-keys sandbox db-info conversation-id nil))
  ([sci-ctx initial-ns-keys sandbox db-info conversation-id _opts]
   (let [sandbox-map (or sandbox (get-in @(:env sci-ctx) [:namespaces 'sandbox]))
         var-registry (when (and db-info conversation-id)
                        (persistance/db-latest-var-registry db-info conversation-id))
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
                           :doc      (:doc (meta v))
                           :version  (get-in var-registry [s :version] 1)}]))
         keep? (fn [[sym _]]
                 (and (not (contains? initial-ns-keys sym))
                   (not (system-var-sym? sym))))
         live-entries (->> live-info
                        (filter keep?)
                        (sort-by (fn [[sym _]]
                                   [(- (long (recency-of sym))) (str sym)]))
                        (mapv (fn [[sym {:keys [val arglists doc version]}]]
                                {:sym sym
                                 :version version
                                 :status (var-status-keyword {:system? false :persisted? false})
                                 :type (var-type-keyword val false)
                                 :size (var-size val false)
                                 :val val
                                 :arglists arglists
                                 :doc doc})))]
     (when (seq live-entries)
       (str/join "\n" (map render-var-form live-entries))))))

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
  (let [entries (persistance/db-restore-expressions db-info conversation-id)]
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

