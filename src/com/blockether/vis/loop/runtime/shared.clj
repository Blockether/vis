(ns com.blockether.vis.loop.runtime.shared
  "Shared helpers and core tool factories for the SCI sandbox.

   Contains:
   - General helpers: truncate, realize-value, format-exception, ->uuid
   - shape — recursive structural sketch of any value
   - result->display — exec-result formatter honoring :visibility + 30k cap

   Sandbox-specific things (EXTRA_BINDINGS, ns->sci-map, date fns)
   live in environment/base.clj.

   Safe to load first — no cross-deps on runtime/core."
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))

;; =============================================================================
;; General helpers
;; =============================================================================

(defn truncate
  "Return `s` capped at `n` chars, preserving nil."
  [s n]
  (when s
    (if (> (count s) n)
      (subs s 0 n)
      s)))

;; =============================================================================
;; UUID helpers
;; =============================================================================

(defn ->uuid
  "The ONE uuid function. No args = random. With arg = coerce.

   (->uuid)          → random UUID
   (->uuid some-uuid) → returned as-is
   (->uuid \"abc-...\") → parsed from string
   (->uuid nil)       → nil
   (->uuid 42)        → throws"
  ([] (java.util.UUID/randomUUID))
  ([v]
   (cond
     (nil? v)    nil
     (uuid? v)   v
     (string? v) (try (java.util.UUID/fromString v)
                  (catch IllegalArgumentException _
                    (throw (ex-info (str "Invalid UUID string: " (pr-str v))
                             {:type :vis/invalid-uuid :got v}))))
     :else       (throw (ex-info (str "Cannot coerce to UUID: " (pr-str v) " (" (type v) ")")
                          {:type :vis/invalid-uuid :got v :got-type (type v)})))))

;; =============================================================================
;; Unified exception formatting
;; =============================================================================

(defn format-exception
  "Unified exception → data map. Use this ONE function everywhere instead of
   ad-hoc (ex-message e) / (.getName (class e)) / (.getStackTrace e) combos.

   Returns:
     {:message str :type kw|nil :class str :data map|nil
      :cause {:message str :class str}|nil
      :stack [str...]}

   `opts` (optional):
     :stack-depth  — how many stack frames to keep (default 12)
     :context      — arbitrary map merged under :context key"
  ([^Throwable e] (format-exception e nil))
  ([^Throwable e {:keys [stack-depth context] :or {stack-depth 12}}]
   (let [ex-data-map (ex-data e)
         cause       (.getCause e)]
     (cond-> {:message (ex-message e)
              :type    (:type ex-data-map)
              :class   (.getName (class e))
              :data    (when (seq ex-data-map) (dissoc ex-data-map :type))
              :cause   (when cause
                         {:message (.getMessage ^Throwable cause)
                          :class   (.getName (class cause))})
              :stack   (mapv str (take stack-depth (.getStackTrace e)))}
       context (assoc :context context)))))

(defn format-exception-short
  "Compact one-liner for log :data. Returns {:error str :class str :type kw|nil}."
  [^Throwable e]
  {:error (ex-message e)
   :class (.getName (class e))
   :type  (:type (ex-data e))})

;; =============================================================================
;; Unified specs — every bounded context registers its shapes here
;; =============================================================================

;; Primitives
(s/def ::non-blank-string (s/and string? (complement str/blank?)))
(s/def ::entity-id (s/tuple #{:id} uuid?))

;; Env shape (the fields persistence cares about)
(s/def ::db-info map?)
(s/def ::conversation-id ::entity-id)
(s/def ::parent-iteration-id (s/nilable ::entity-id))
(s/def ::router some?)
(s/def ::env (s/keys :req-un [::db-info ::conversation-id ::router]))

;; Iteration persistence
(s/def ::query-id ::entity-id)
(s/def ::thinking (s/nilable string?))
(s/def ::executions (s/nilable sequential?))
(s/def ::vars (s/nilable sequential?))
(s/def ::answer (s/nilable string?))
(s/def ::duration-ms (s/nilable number?))
(s/def ::error (s/nilable map?))
(s/def ::iteration-store-opts
  (s/keys :req-un [::query-id]
          :opt-un [::thinking ::executions ::vars ::answer ::duration-ms ::error]))

;; Query result (what iteration-loop returns)
(s/def ::iterations int?)
(s/def ::status keyword?)
(s/def ::tokens (s/nilable map?))
(s/def ::cost (s/nilable map?))
(s/def ::query-result
  (s/keys :opt-un [::answer ::iterations ::status ::tokens ::cost]))

(defn validate!
  "Validate `data` against `spec`. Throws ex-info with explain-data on failure.
   Use this ONE function everywhere instead of ad-hoc :pre checks.

   Example:
     (validate! ::env env)
     (validate! ::iteration-store-opts opts)"
  [spec data]
  (when-not (s/valid? spec data)
    (throw (ex-info (str "Spec validation failed: " (pr-str spec) "\n"
                      (with-out-str (s/explain spec data)))
             {:type   :vis/spec-validation-failed
              :spec   spec
              :data   data
              :errors (s/explain-data spec data)}))))

(declare shape)

(def ^:private REALIZE_LAZY_LIMIT 100)

(defn realize-value
  "Recursively realizes lazy sequences in a value to prevent opaque LazySeq@hash.
   Converts lazy seqs to vectors (bounded), walks into maps/vectors/sets.
   Lazy seqs longer than REALIZE_LAZY_LIMIT are truncated with a trailing
   marker so downstream code can still serialize them safely."
  [v]
  (cond
    (instance? clojure.lang.LazySeq v)
    (let [head (take (inc REALIZE_LAZY_LIMIT) v)
          realized (mapv realize-value (take REALIZE_LAZY_LIMIT head))]
      (if (> (count head) REALIZE_LAZY_LIMIT)
        (conj realized (str "...<truncated lazy seq at " REALIZE_LAZY_LIMIT " elements>"))
        realized))
    (map? v) (persistent! (reduce-kv (fn [m k val] (assoc! m k (realize-value val))) (transient {}) v))
    (vector? v) (mapv realize-value v)
    (set? v) (into #{} (map realize-value) v)
    :else v))

;; =============================================================================
;; shape — recursive, size-bounded schema inference
;; =============================================================================
;;
;; Returns a pure-data schema sketch of any value. Types only, no values.
;; Used by the agent to rediscover the structure of a previously def'd var
;; without paying an iteration for `(keys x)` / `(first (:foo x))` probing.
;;
;; Returns DATA (symbols for atoms, maps/vecs/sets for collections), not a
;; string — so the model can slice it: `(:matches (shape hits))` works.
;;
;; Bounds:
;;   - depth          ≤ 10 nested levels before collapsing to `...`
;;   - per-map keys   ≤ 20 (extras summarised as `... N more`)
;;   - per-seq probe  = 5 items (used only to test homogeneity)
;; Sequences that are homogeneous on the probe render as `[one-shape]`;
;; heterogeneous ones render as `[first-shape ... last-shape]`.

(def ^:private MAX_SHAPE_DEPTH 10)
(def ^:private MAX_SHAPE_MAP_KEYS 20)
(def ^:private MAX_SHAPE_SEQ_PROBE 5)

(declare shape)

;; Tags are symbols so the output is pure Clojure data the caller can
;; `pr-str`, `=` against, or pattern-match on. `(symbol "nil")` is used
;; for the nil case because the reader literal `'nil` evaluates to the
;; nil VALUE, not a symbol — we need the actual Symbol object here.
(def ^:private NIL_SHAPE (symbol "nil"))

(defn- java-array? [v]
  (and (some? v) (.isArray ^Class (class v))))

(defn- sci-var?
  "True when v is a SCI var. Detection by class-name string instead of
   `instance? sci.lang.Var` because SCI's class loader ordering means the
   literal class symbol may not resolve at compile time of every consumer
   namespace — same approach used by storage and web."
  [v]
  (and (some? v)
    (= "sci.lang.Var" (.getName (class v)))))

(defn- var-like?
  "True when v is a `clojure.lang.Var` or `sci.lang.Var`. Both wrap a
   bound value that the user almost always cares about more than the
   wrapper itself, so `shape` deref's through them transparently."
  [v]
  (or (instance? clojure.lang.Var v) (sci-var? v)))

(defn- ideref-kind
  "Pick a tag for an IDeref wrapper. Covers Clojure's built-in deref
   containers plus raw `java.util.concurrent.Future`. Falls back to the
   generic `deref` tag for reify'd objects like `(promise)` (which doesn't
   implement any of the named classes but still satisfies IDeref).

   Vars are NOT in this list on purpose — `shape` deref's them transparently
   above so callers always see the bound value's shape, never `(var X)`."
  [v]
  (condp instance? v
    clojure.lang.Atom           'atom
    clojure.lang.Ref            'ref
    clojure.lang.Agent          'agent
    clojure.lang.Delay          'delay
    clojure.lang.Volatile       'volatile
    java.util.concurrent.Future 'future
    'deref))

(declare shape)

(defn- seq-shape-sample
  "Probe the first MAX_SHAPE_SEQ_PROBE items to decide between
   homogeneous (`[one]`) and heterogeneous (`[first ... last]`) rendering.
   Returns a vector ready to be re-typed into the caller's collection
   (vector, list, set)."
  [items depth]
  (let [probe          (doall (take MAX_SHAPE_SEQ_PROBE items))
        element-shapes (mapv #(shape % (dec depth)) probe)
        uniq           (distinct element-shapes)]
    (cond
      (empty? probe)      []
      (= 1 (count uniq))  [(first element-shapes)]
      :else               [(first element-shapes) '... (last element-shapes)])))

(defn- map-shape-body
  "Shape a map — capped at MAX_SHAPE_MAP_KEYS with an overflow marker for
   any extra keys. Keys are preserved as-is (keyword/string/int keys stay
   readable); values are recursively shaped."
  [m depth]
  (let [ks        (try (sort-by pr-str (keys m))
                    (catch Exception _ (seq (keys m))))
        total     (count ks)
        shown     (take MAX_SHAPE_MAP_KEYS ks)
        base      (into (array-map)
                    (map (fn [k] [k (shape (get m k) (dec depth))]))
                    shown)]
    (if (> total MAX_SHAPE_MAP_KEYS)
      (assoc base '... (- total MAX_SHAPE_MAP_KEYS))
      base)))

(defn shape
  "Return a compact schema-only Clojure structure describing v's recursive
   shape — types only, no actual values. Pure fn.

   Returns Clojure data (symbols for atoms, collections for collections), so
   you can introspect further:
     (:matches (shape hits))   ; peek one level down without seeing data

   Bounds: depth ≤ 10, maps ≤ 20 keys, sequences probed on first 5 items to
   decide homogeneous vs heterogeneous rendering. Overflow markers are
   literal `...` symbols.

   Examples:
     (shape 42)                            ;; => int
     (shape \"hello\")                     ;; => str
     (shape [1 2 3])                       ;; => [int]
     (shape [1 \"two\" :three])            ;; => [int ... kw]
     (shape {:a 1 :b \"x\"})               ;; => {:a int, :b str}
     (shape {:xs [{:n 1 :t \"a\"}]})       ;; => {:xs [{:n int, :t str}]}
     (shape (range 1000))                  ;; => (int)
     (shape (atom {:k 1}))                 ;; => (atom {:k int})
     (shape (byte-array 3))                ;; => (array byte)
     (shape #\"foo\")                      ;; => regex
     (shape {:k1 1 :k2 2 ...21 keys...})   ;; => {:k1 int, …, ... 1}

   Unrecognised Java objects fall back to the class's simple name."
  ([v] (shape v MAX_SHAPE_DEPTH))
  ([v depth]
   (cond
     ;; Scalars first — tightest hot path, no recursion.
     (nil? v)     NIL_SHAPE
     (boolean? v) 'bool
     (integer? v) 'int
     (float? v)   'float
     (ratio? v)   'ratio
     (number? v)  'num
     (char? v)    'char
     (string? v)  'str
     (keyword? v) 'kw
     (symbol? v)  'sym
     (inst? v)    'inst
     (uuid? v)    'uuid

     ;; Depth cap. Placed AFTER scalars so collapse only hits actual
     ;; recursion, not leaf scalars which are cheap to print anyway.
     (zero? depth) '...

     ;; Regex — a single tag beats `'Pattern` for readability.
     (instance? java.util.regex.Pattern v) 'regex

     ;; Clojure persistent collections. Records implement IPersistentMap
     ;; so they take the map branch automatically — the field names show
     ;; up as keys, which is exactly what a caller wants from a shape.
     (map? v)    (map-shape-body v depth)
     (vector? v) (seq-shape-sample v depth)
     (set? v)    (set (seq-shape-sample v depth))
     (seq? v)    (apply list (seq-shape-sample v depth))

      ;; Functions — no introspection beyond arity (we'd need metadata the
      ;; sandbox typically lacks).
      (fn? v) 'fn

      ;; Vars deref transparently — `(shape #'foo)` returns the shape of
      ;; whatever `foo` holds, never `(var X)` or `(deref X)`. Vars
      ;; conceptually ARE their bound value for shape purposes; the
      ;; wrapper is plumbing the user doesn't care about. MUST come
      ;; before the IDeref branch since vars also satisfy IDeref.
      (var-like? v)
      (try (shape (deref v) (dec depth))
        (catch Throwable _ 'opaque))

      ;; IDeref wrappers — unwrap one level so the caller sees the inner
      ;; shape. Returned as a list `(kind inner-shape)` so it reads like a
      ;; call: `(atom int)`, `(delay {:a str})`. Guards against blocking
      ;; on unrealised Delays / Futures / Promises via IPending.
      (instance? clojure.lang.IDeref v)
     (let [kind     (ideref-kind v)
           pending? (and (instance? clojure.lang.IPending v)
                      (try (not (realized? v))
                           (catch Throwable _ false)))]
       (if pending?
         (list kind 'unrealized)
         (let [inner (try (shape (deref v) (dec depth))
                          (catch Throwable _ 'opaque))]
           (list kind inner))))

     ;; Java arrays — element type as a list: `(array byte)`, `(array String)`.
     (java-array? v)
     (list 'array
       (symbol (.getSimpleName (.getComponentType ^Class (class v)))))

     ;; Java collections — coerce to the Clojure analogue. Covers things
     ;; like results from Java interop (`java.util.ArrayList`,
     ;; `HashMap`, `LinkedHashSet`). Ordering may not survive but the
     ;; shape does.
     (instance? java.util.Map v) (map-shape-body (into {} v) depth)
     (instance? java.util.Set v) (set (seq-shape-sample v depth))
     (instance? java.util.List v) (seq-shape-sample v depth)

     ;; Throwables — class name alone would drop the message; a
     ;; `(throwable <class>)` list at least signals it's an exception.
     (instance? Throwable v)
     (list 'throwable (symbol (.getSimpleName (class v))))

      ;; Fallback — class's simple name as a symbol. Last-resort for
      ;; arbitrary Java objects; useful enough for the caller to decide
      ;; whether to `(doc some-fn)` or just inline.
      :else (symbol (.getSimpleName (class v))))))

(def ^:const MAX_RESULT_DISPLAY_CHARS
  "Character cap applied to exec results when rendered to the user and when
   fed back to the LLM as journal context. DB persistence is uncapped on
   purpose — see the result-display block docstring."
  30000)

(def ^:private SANDBOX_VAR_RE
  "Matches `#'sandbox/<name>` in a pr-str'd string. The sandbox ns is the
   only ns the LLM-defined vars live in, so stripping it here is loss-free
   — the bare `#'name` always refers to the sandbox binding."
  #"#'sandbox/")

(defn strip-sandbox-ns
  "Rewrite `#'sandbox/foo` → `#'foo` in a string. Pure fn, idempotent.
   Used by `result->display` so var-typed exec results don't waste tokens
   on the sandbox prefix."
  [^String s]
  (when s
    (str/replace s SANDBOX_VAR_RE "#'")))

(defn truncate-with-marker
  "Cap `s` at `n` chars and append a `…[truncated, n of total chars]…`
   marker when truncation actually happens. Returns nil for nil; returns
   `s` unchanged when already within cap."
  [^String s ^long n]
  (when s
    (let [total (.length s)]
      (if (<= total n)
        s
        (str (subs s 0 n) "\n…[truncated, " n " of " total " chars]…")))))

(defn result->display
  "Convert a raw exec result value into the string the UI / next-iteration
   LLM context should see.

     :full / nil → (pr-str (realize-value v)), strip sandbox/, cap at
                   MAX_RESULT_DISPLAY_CHARS with a truncation marker.
     :shape      → (pr-str (shape v)), strip sandbox/. Shape output is
                   already small, so no cap is applied (depth/breadth
                   bounds inside `shape` keep it tame).

   `visibility` is a keyword or nil. Unknown values default to :full
   so a typo never blanks the result.

   Returns a string. Nil input returns nil so callers can decide whether
   to render an empty result row or skip entirely."
  [v visibility]
  (when (some? v)
    (case visibility
      :shape (-> v shape pr-str strip-sandbox-ns)
      ;; :full and any unknown value share the realized-and-capped path.
      (-> v realize-value pr-str strip-sandbox-ns
        (truncate-with-marker MAX_RESULT_DISPLAY_CHARS)))))

