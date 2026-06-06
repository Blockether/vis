(ns com.blockether.vis.internal.env-python
  "Embedded-GraalPy sandbox machinery — a DROP-IN twin of
   `com.blockether.vis.internal.env`, but the agent writes **Python** instead
   of Clojure. Same public surface (same fn names + arg shapes) so the loop can
   swap `[... env :as env]` → `[... env-python :as env]` and keep working:

     create-sci-context / sci-update-binding! / bind-and-bump! /
     bind-and-bump-with-doc! / push-eval-result! / push-eval-error! /
     reset-eval-bindings! / count-top-level-forms / validate-non-empty-block! /
     validate-no-banned-defs! / restore-sandbox! / SYSTEM_VAR_NAMES /
     system-var-sym? / *lru-atom* / *current-turn-position* / fresh-lru-atom

   The `:sci-ctx` slot now holds a GraalPy `org.graalvm.polyglot.Context`; the
   `'sandbox` namespace is the Python top scope (`context.getBindings(\"python\")`).
   Clojure tool fns are wired into the Python globals as `ProxyExecutable`s, so
   `(cat \"x\")` in Clojure becomes `cat(\"x\")` in Python and runs the SAME
   Clojure fn — zero behavioural change to the tools themselves.

   Requires the `:graalpy` deps alias (kept off the default classpath)."
  (:require
   [clojure.string :as str])
  (:import
   [org.graalvm.polyglot Context Value PolyglotAccess PolyglotException]
   [org.graalvm.polyglot.io IOAccess]
   [org.graalvm.polyglot.proxy ProxyExecutable ProxyArray ProxyHashMap]
   [java.util ArrayList HashMap]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Marshalling  Clojure  <->  Python (polyglot Value)
;; =============================================================================

(declare ->py ->clj)

(defn- key->py
  "Map/keyword key -> a Python-side string key. FULL SNAKE: any kebab is
   snake-cased so Clojure data reads as idiomatic Python (`:files-only` ->
   \"files_only\"); already-snake keys pass through unchanged. No `?`/`!`
   munging — snake all the way down."
  ^String [k]
  (cond
    (keyword? k) (str/replace (name k) "-" "_")
    (symbol? k)  (str/replace (str k) "-" "_")
    :else        (str k)))

(defn- py-key->clj
  "Dict key coming back from Python -> keyword, VERBATIM snake_case
   (`\"files_only\"` -> `:files_only`). The sandbox is full-snake, so tools key
   on snake_case keywords too — no kebab translation."
  [^String s]
  (keyword s))

(defn ->py
  "Clojure value -> something GraalPy accepts as a Python value. Primitives and
   Strings pass through (the Context auto-converts Java boxed types); collections
   become polyglot proxies so Python sees dict/list; keywords become their name."
  [x]
  (cond
    (nil? x)     nil
    (string? x)  x
    (boolean? x) x
    (keyword? x) (name x)
    (symbol? x)  (str x)
    (map? x)     (let [^HashMap hm (HashMap.)]
                   (doseq [[k v] x] (.put hm (key->py k) (->py v)))
                   (ProxyHashMap/from hm))
    (or (vector? x) (seq? x) (set? x))
    (ProxyArray/fromList (ArrayList. ^java.util.Collection (mapv ->py x)))
    ;; numbers, Java objects, etc. — hand straight to polyglot
    :else        x))

(defn ->clj
  "Polyglot `Value` (a Python value) -> Clojure data. Dicts -> maps with
   keyword keys, lists/tuples -> vectors, callables/opaque objects -> the raw
   `Value` (rare on the tool-arg path)."
  [^Value v]
  (cond
    (nil? v)            nil
    (.isNull v)         nil
    (.isBoolean v)      (.asBoolean v)
    (.isString v)       (.asString v)
    (.isNumber v)       (if (.fitsInLong v) (.asLong v) (.asDouble v))
    (.hasArrayElements v) (mapv #(->clj (.getArrayElement v (long %)))
                            (range (.getArraySize v)))
    (.hasHashEntries v) (let [it (.getHashKeysIterator v)]
                          (loop [m (transient {})]
                            (if (.hasIteratorNextElement it)
                              (let [k (.getIteratorNextElement it)]
                                (recur (assoc! m
                                         (py-key->clj (.asString k))
                                         (->clj (.getHashValue v k)))))
                              (persistent! m))))
    :else               v))

(defn sym->py-name
  "Clojure tool/binding symbol -> a Python-LEGAL global name. Python identifiers
   can't contain `-`/`?`/`!`, so `task-set!` -> `task_set`, `fact-set!` ->
   `fact_set`, `cat`/`done`/`ctx` unchanged. FULL SNAKE: this is how the agent
   actually reaches the tools — `task_set(...)` calls the Clojure `task-set!`."
  ^String [sym]
  (-> (str sym)
    (str/replace #"[?!]" "")
    (str/replace "-" "_")))

(defn- wrap-ifn
  "Wrap a Clojure fn as a Python-callable `ProxyExecutable`. Positional Python
   args are marshalled to Clojure, the fn is applied, and the result marshalled
   back to Python. Matches vis's positional-args tool contract."
  ^ProxyExecutable [f]
  (reify ProxyExecutable
    (execute [_ args]
      (->py (apply f (map ->clj args))))))

;; =============================================================================
;; Per-iteration LRU (kept for interface parity)
;;
;; SCI exposed a resolve-hook LRU of "vars referenced this iteration" for the
;; live-vars renderer. GraalPy has no equivalent cheap resolve hook, so these
;; stay as honoured no-op-compatible vars; the live-vars feature can later read
;; Python globals (`context.getBindings`) diffed against `:initial-ns-keys`.
;; =============================================================================

(def ^:dynamic *lru-atom* nil)
(def ^:dynamic *current-turn-position* nil)
(defn fresh-lru-atom [] (atom {}))

;; =============================================================================
;; Block validation (Python: top-level statement count + banned constructs)
;; =============================================================================

(defonce ^:private parser-ctx
  ;; Tiny throwaway context used ONLY to `ast.parse` candidate blocks for
  ;; validation (no execution). Reused so we don't pay context warmup per check.
  (delay
    (-> (Context/newBuilder (into-array String ["python"]))
      (.allowAllAccess false)
      (.allowIO IOAccess/NONE)
      (.allowPolyglotAccess PolyglotAccess/NONE)
      (.build))))

(def validation-edamame-opts
  "Parity stub: SCI used edamame opts to count Clojure forms. Python counts
   `ast.parse(...).body` instead; kept so callers referencing this var don't
   break."
  {:lang :python})

(defn count-top-level-forms
  "Number of top-level Python statements in `code` (the analogue of SCI's
   top-level-form count). Comment-/whitespace-only blocks return 0. Raises the
   underlying `PolyglotException` on a syntax error — that's a syntax issue, not
   a multi-statement issue, same contract as the SCI twin."
  [code]
  (let [^Context ctx @parser-ctx
        b (.getBindings ctx "python")]
    (.putMember b "__vis_src__" (str code))
    (long (.asLong (.eval ctx "python"
                     "len(__import__('ast').parse(__vis_src__).body)")))))

(defn validate-non-empty-block!
  "Throws `:vis/empty-block` when `code` parses to zero top-level statements
   (comment-only blocks). Iterations that produce no evidence are rejected at
   the model boundary."
  [code]
  (when (zero? (count-top-level-forms code))
    (throw (ex-info "Block is empty (only comments). Iteration produces no evidence."
             {:type :vis/empty-block :form-count 0}))))

(def BANNED_DEF_HEADS
  "Python constructs refused pre-eval. Unlike the SCI twin (which banned
   JVM-class-producing forms that can't round-trip per-var restore), the Python
   sandbox is fresh per turn, so the only bans are belt-and-suspenders against
   the obvious sandbox-escape footguns on top of the Context restrictions."
  #{"exec" "eval" "compile" "__import__"})

(defn validate-no-banned-defs!
  "Throws `:vis/banned-def-head` when `code` references a banned construct
   (`BANNED_DEF_HEADS`). Parse failures are silent — the eval that follows
   surfaces a clean syntax error with line/column."
  [code]
  (try
    (let [^Context ctx @parser-ctx
          b (.getBindings ctx "python")]
      (.putMember b "__vis_src__" (str code))
      ;; Collect every Name/attribute id in the AST and intersect with the bans.
      (.putMember b "__vis_banned__" (->py (vec BANNED_DEF_HEADS)))
      (let [hit (.eval ctx "python"
                  (str "next((n.id for n in __import__('ast').walk("
                    "__import__('ast').parse(__vis_src__)) "
                    "if isinstance(n, __import__('ast').Name) "
                    "and n.id in set(__vis_banned__)), None)"))]
        (when-not (.isNull hit)
          (throw (ex-info
                   (str "Block uses `" (.asString hit) "` which is banned in the "
                     "Python sandbox (sandbox-escape footgun).")
                   {:type :vis/banned-def-head :head (.asString hit)})))))
    (catch clojure.lang.ExceptionInfo ei
      (if (= :vis/banned-def-head (:type (ex-data ei))) (throw ei) nil))
    (catch Throwable _ nil)))

;; =============================================================================
;; Sandbox bindings
;; =============================================================================

(defn- ^Value py-globals [py-ctx]
  (.getBindings ^Context py-ctx "python"))

(defn sci-update-binding!
  "Bind `sym` -> `val` in the Python sandbox globals. Clojure fns are wired as
   callables; everything else is marshalled. (Name kept from the SCI twin for
   drop-in compatibility.)"
  [py-ctx sym val]
  (.putMember (py-globals py-ctx) (sym->py-name sym)
    (if (fn? val) (wrap-ifn val) (->py val))))

(defn bind-and-bump!
  "Set `sym` -> `val` in the env's Python sandbox."
  [env sym val]
  (sci-update-binding! (:sci-ctx env) sym val))

(defn bind-and-bump-with-doc!
  "Like `bind-and-bump!` but also records `doc` in the side `__vis_docs__` dict
   so a future live-vars view can surface name + doc (Python has no var
   metadata channel like SCI's :doc)."
  [env sym doc val]
  (let [py-ctx (:sci-ctx env)]
    (sci-update-binding! py-ctx sym val)
    (let [g (py-globals py-ctx)
          docs (let [d (.getMember g "__vis_docs__")]
                 (if (or (nil? d) (.isNull d)) (HashMap.) (HashMap.)))]
      ;; keep it simple: stash into a Python dict global
      (.putMember g "__vis_doc_sym__" (str sym))
      (.putMember g "__vis_doc_txt__" (str (or doc "vis-managed engine binding")))
      (.eval ^Context py-ctx "python"
        "globals().setdefault('__vis_docs__', {})[__vis_doc_sym__] = __vis_doc_txt__")
      docs)))

(defn push-eval-result!
  "REPL-style stack push for the sandbox `_1 _2 _3` recovery slots, mirroring
   SCI's `*1 *2 *3`. Python convention is `_`, but we use `_1/_2/_3` to match
   the engine's three-deep history."
  [env value]
  (let [py-ctx (:sci-ctx env)
        g (py-globals py-ctx)]
    (let [v1 (.getMember g "_1")
          v2 (.getMember g "_2")]
      (.putMember g "_3" v2)
      (.putMember g "_2" v1)
      (.putMember g "_1" (->py value)))))

(defn push-eval-error!
  "Park the most recent uncaught error in the sandbox `_e` slot (mirrors SCI
   `*e`). The `_1/_2/_3` value stack does NOT advance on error."
  [env throwable]
  (let [g (py-globals (:sci-ctx env))]
    (.putMember g "_e" (str throwable))))

(defn reset-eval-bindings!
  "Clear `_1 _2 _3 _e` at turn start so a follow-up turn doesn't see leftovers."
  [env]
  (let [g (py-globals (:sci-ctx env))]
    (doseq [s ["_1" "_2" "_3" "_e"]] (.putMember g s nil))))

;; =============================================================================
;; Python sandbox context creation
;; =============================================================================

(defn create-sci-context
  "Create the embedded-GraalPy sandbox context with all available bindings.
   Drop-in twin of the SCI `create-sci-context`.

   `custom-bindings` — map of symbol->value (tool fns + engine values). Fns are
   wired as Python callables; values are marshalled. Returns the same shape the
   SCI twin returns:

     {:sci-ctx          <org.graalvm.polyglot.Context>
      :sandbox-ns       :python          ; placeholder (Python has one top scope)
      :initial-ns-keys  #{...baseline globals...}}"
  [custom-bindings]
  (let [ctx (-> (Context/newBuilder (into-array String ["python"]))
              ;; deny-by-default; no host/file/native/threads. Tools do real IO
              ;; on the Clojure side via ProxyExecutable, so Python itself needs
              ;; none of it.
              (.allowAllAccess false)
              (.allowIO IOAccess/NONE)
              (.allowCreateThread false)
              (.allowNativeAccess false)
              (.allowPolyglotAccess PolyglotAccess/NONE)
              (.build))
        g   (.getBindings ctx "python")]
    ;; REPL recovery slots first (so they land in the baseline and get filtered
    ;; out of the model-visible live-vars view, like SCI's *1/*2/*3/*e).
    (doseq [s ["_1" "_2" "_3" "_e"]] (.putMember g s nil))
    ;; Tool fns + engine values (names snake-ified to Python-legal identifiers).
    (doseq [[sym val] (or custom-bindings {})]
      (.putMember g (sym->py-name sym) (if (fn? val) (wrap-ifn val) (->py val))))
    {:sci-ctx ctx
     :sandbox-ns :python
     :initial-ns-keys (set (map str (seq (.getMemberKeys g))))}))

;; =============================================================================
;; Eval — the loop's hook (SCI's eval lived in loop.clj; we surface a thin one
;; here so the spike + future Python loop have a single entry point).
;; =============================================================================

(defn eval-block
  "Evaluate a whole Python `code` block in `py-ctx`. Returns
   `{:source code :result <clj>}` on success; throws the PolyglotException on
   failure (caller maps it to the engine error shape). Globals (defs/imports/
   state) persist across calls in the same context — same as SCI's sandbox ns."
  [py-ctx code]
  {:source code
   :result (->clj (.eval ^Context py-ctx "python" (str code)))})

(defn map-polyglot-error
  "Map a GraalPy `PolyglotException` into the engine's op-error shape, mirroring
   `extension/ex->op-error` for the SCI path. `:phase` is `:python/syntax` for
   parse errors, else `:python/runtime`; `:line`/`:column` come from the Python
   source location when present. A host (Clojure-tool) exception is unwrapped so
   its real message surfaces."
  [^PolyglotException e _code]
  (let [host?  (.isHostException e)
        cause  (when host? (.asHostException e))
        loc    (.getSourceLocation e)
        msg    (or (when cause (or (ex-message cause) (.getMessage cause)))
                 (.getMessage e))]
    {:message msg
     :data (cond-> {:phase (cond host?              :python/host
                                 (.isSyntaxError e) :python/syntax
                                 :else              :python/runtime)}
             (some? loc) (assoc :line (.getStartLine loc)
                                :column (.getStartColumn loc))
             ;; ex-data from a Clojure tool's ex-info rides through verbatim so
             ;; e.g. :tool/banned, :vis/* keep their type for the trailer.
             (and cause (instance? clojure.lang.IExceptionInfo cause))
             (merge (ex-data cause)))}))

(defn run-python-block
  "Evaluate one Python `code` block in `py-ctx`, returning the SAME outcome
   contract `run-sci-code` produces in loop.clj:

     {:result <last-value-or-nil> :forms [{:source :result}|{:source :error}]
      :error <op-error-or-nil>}

   The whole block is one unit (v1): Python executes top-to-bottom and tool
   calls fire in order through the ProxyExecutable wrappers, so per-statement
   envelope splitting (via `ast.get_source_segment`) is a later refinement.
   The caller (loop) binds `extension/*tool-event-sink*` etc. around this so the
   existing Clojure tools' instrumentation fires unchanged."
  [py-ctx code]
  (try
    (let [v (->clj (.eval ^Context py-ctx "python" (str code)))]
      {:result v :forms [{:source code :result v}] :error nil})
    (catch PolyglotException e
      (let [err (map-polyglot-error e code)]
        {:result nil :forms [{:source code :error err}] :error err}))))

;; =============================================================================
;; Engine-owned sandbox names + restore (NOOP) — parity with the SCI twin
;; =============================================================================

(def SYSTEM_VAR_NAMES
  "Engine-owned symbols hidden from user live-var listings."
  '#{ctx})

(defn system-var-sym?
  [sym]
  (contains? SYSTEM_VAR_NAMES sym))

(defn restore-sandbox!
  "Deprecated NOOP (parity with SCI twin). Python sandbox is fresh per turn;
   cross-turn memory rides on `:session/facts` + the per-form blob."
  [_py-ctx _db-info _session-id]
  [])
