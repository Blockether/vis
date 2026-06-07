(ns com.blockether.vis.internal.env-python
  "Embedded-GraalPy sandbox machinery — the agent's action substrate. The agent
   writes **Python**; this ns embeds a GraalPy `org.graalvm.polyglot.Context`,
   marshals values across the Clojure↔Python boundary, wires the Clojure tool
   fns into the Python globals as `ProxyExecutable`s (so `cat(\"x\")` in Python
   runs the Clojure `cat`), and evaluates the model's code per top-level form.

   Public surface used by the loop:

     create-python-context / set-python-binding! / bind-and-bump! /
     bind-and-bump-with-doc! / push-eval-result! / push-eval-error! /
     reset-eval-bindings! / count-top-level-forms / validate-non-empty-block! /
     validate-no-banned-defs! / restore-sandbox! / SYSTEM_VAR_NAMES /
     system-var-sym? / *lru-atom* / *current-turn-position* / fresh-lru-atom /
     run-python-block / map-polyglot-error / bind-ctx! / ctx->python-str

   The `:python-context` slot holds the GraalPy `Context`; the Python top scope is
   `context.getBindings(\"python\")`. GraalPy ships in the default deps (runs on
   Oracle GraalVM 25 → Truffle gets the Graal JIT)."
  (:require
   [charred.api :as json]
   [clojure.string :as str])
  (:import
   [org.graalvm.polyglot Context Value PolyglotAccess PolyglotException]
   [org.graalvm.polyglot.io IOAccess]
   [org.graalvm.polyglot.proxy ProxyExecutable ProxyArray ProxyHashMap]
   [java.util ArrayList LinkedHashMap]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Marshalling  Clojure  <->  Python (polyglot Value)
;; =============================================================================

(declare ->py ->clj)

(defn kw->snake
  "Keyword -> FULL-SNAKE string: namespace folded in with `_`, kebab -> snake,
   trailing `?`/`!` stripped. `:session/utilization` -> \"session_utilization\",
   `:files-only?` -> \"files_only\", `:add-bcrypt` -> \"add_bcrypt\". The ONE
   transform used for BOTH dict keys AND keyword values, so the same id reads
   identically whether it is a key or appears inside a value (e.g. depends_on)."
  ^String [k]
  (-> (if (namespace k) (str (namespace k) "_" (name k)) (name k))
    (str/replace "-" "_")
    (str/replace #"[?!]$" "")))

(defn- key->py
  "Map key -> a Python-side string key. Keywords full-snake via `kw->snake`;
   symbols kebab -> snake; everything else stringified."
  ^String [k]
  (cond
    (keyword? k) (kw->snake k)
    (symbol? k)  (str/replace (str k) "-" "_")
    :else        (str k)))

(defn- py-key->clj
  "Python dict key string -> Clojure keyword, VERBATIM snake_case
   (`\"files_only\"` -> `:files_only`, `\"from_hash\"` -> `:from_hash`). The
   foundation tools natively destructure these snake_case option keys — the
   sandbox is FULL SNAKE end to end, no kebab translation."
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
    (keyword? x) (kw->snake x)
    ;; symbols (e.g. trailer form heads) snake to the SAME Python name the agent
    ;; calls — `task-set!` -> "task_set" — so stored forms read consistently.
    (symbol? x)  (-> (str x) (str/replace #"[?!]" "") (str/replace "-" "_"))
    (map? x)     (let [^LinkedHashMap hm (LinkedHashMap.)]
                   ;; LinkedHashMap preserves Clojure map ITERATION ORDER (so an
                   ;; array-map renders in its canonical key order), making the
                   ;; live `ctx` dict and the rendered `# ctx` text agree.
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
  "Clojure tool/binding symbol -> a Python-LEGAL global name. Purely mechanical:
   `/` and `-` fold to `_` (alias fold + kebab->snake); a trailing `!` (mutation
   marker) is dropped; a trailing `?` (predicate) becomes an `is_` prefix. So
   `git/status` -> `git_status`, `git/commit!` -> `git_commit`, `search/web` ->
   `search_web`, `task-set!` -> `task_set`, `exists?` -> `is_exists`. FULL SNAKE:
   this is how the agent reaches the tools — `git_status()` calls `git/status`."
  ^String [sym]
  (let [s     (str sym)
        pred? (str/ends-with? s "?")
        base  (-> s
                (str/replace "?" "")
                (str/replace "!" "")
                (str/replace "/" "_")
                (str/replace "-" "_"))]
    (if pred? (str "is_" base) base)))

(defn- wrap-ifn
  "Wrap a Clojure fn as a Python-callable `ProxyExecutable`. Positional Python
   args are marshalled to Clojure, the fn is applied, and the result marshalled
   back to Python. Matches vis's positional-args tool contract."
  ^ProxyExecutable [f]
  (reify ProxyExecutable
    (execute [_ args]
      (->py (apply f (map ->clj args))))))

;; =============================================================================
;; Canonical CONTEXT serialization — the agent-facing `context` snapshot is a
;; real PYTHON object, and its printed form is produced BY PYTHON (GraalPy), not
;; by a Clojure reimplementation. So the `<context>` text and `repr(context)`
;; cannot drift: the SAME JSON crosses the boundary, `json.loads` builds the
;; native dict once, and `__vis_pp__` (Python) stringifies it.
;; =============================================================================

(defn ->json-ready
  "Deep-convert a Clojure value to a JSON-encodable shape with FULL-SNAKE string
   keys and snaked keyword/symbol scalar VALUES — the exact transform `->py`
   applies — so the JSON round-trip reproduces the live `ctx` dict. Maps → maps
   with string keys; keywords/symbols → snake strings; vectors/seqs/sets →
   vectors; scalars pass through."
  [x]
  (cond
    (map? x)     (persistent!
                   (reduce-kv (fn [m k v] (assoc! m (key->py k) (->json-ready v)))
                     (transient {}) x))
    (keyword? x) (kw->snake x)
    (symbol? x)  (-> (str x) (str/replace #"[?!]" "") (str/replace "-" "_"))
    (or (vector? x) (seq? x) (set? x)) (mapv ->json-ready x)
    :else        x))

(def ^:private vis-pp-python
  "Deterministic Python pretty-printer for the `ctx` dict. Double-quoted strings,
   True/False/None, insertion order preserved, inline when it fits `width` else
   one entry per line (closing bracket aligned under the entry column)."
  "
import json as __vis_json__

def __vis_pp_str__(s):
    return '\"' + s.replace('\\\\', '\\\\\\\\').replace('\"', '\\\\\"').replace('\\n', '\\\\n').replace('\\r', '\\\\r').replace('\\t', '\\\\t') + '\"'

def __vis_pp__(o, indent=0, width=100):
    pad = ' ' * (indent + 1)
    cpad = ' ' * indent
    if isinstance(o, bool):
        return 'True' if o else 'False'
    if o is None:
        return 'None'
    if isinstance(o, str):
        return __vis_pp_str__(o)
    if isinstance(o, dict):
        if not o:
            return '{}'
        items = [(__vis_pp_str__(str(k)), __vis_pp__(v, indent + 1, width)) for k, v in o.items()]
        inline = '{' + ', '.join(k + ': ' + v for k, v in items) + '}'
        if '\\n' not in inline and indent + len(inline) <= width:
            return inline
        return '{\\n' + ',\\n'.join(pad + k + ': ' + v for k, v in items) + '\\n' + cpad + '}'
    if isinstance(o, (list, tuple)):
        if not o:
            return '[]'
        items = [__vis_pp__(x, indent + 1, width) for x in o]
        inline = '[' + ', '.join(items) + ']'
        if '\\n' not in inline and indent + len(inline) <= width:
            return inline
        return '[\\n' + ',\\n'.join(pad + x for x in items) + '\\n' + cpad + ']'
    return repr(o)

def __vis_render_ctx__(jsons):
    return __vis_pp__(__vis_json__.loads(jsons))
")

(defn- build-printer-context
  "Trusted, process-wide GraalPy context whose ONLY job is to turn ctx JSON into
   the canonical Python-pretty string. Permissive (`allowAllAccess`) because it
   imports `json` and never runs agent code — only our `__vis_pp__` over our own
   data."
  ^Context []
  (let [ctx (-> (Context/newBuilder (into-array String ["python"]))
              (.allowAllAccess true)
              (.build))]
    (.eval ctx "python" vis-pp-python)
    ctx))

(defonce ^:private printer-context (delay (build-printer-context)))

(defn ctx->python-str
  "Render a Clojure CTX map as the canonical Python-literal string — produced by
   Python (`__vis_pp__`) inside GraalPy, so it matches `repr`-style Python and the
   live `ctx` dict the agent reads. `data` is JSON-bridged through `->json-ready`."
  ^String [data]
  (let [^Context ctx @printer-context
        jsons (json/write-json-str (->json-ready data))]
    (locking ctx
      (let [^Value f (.getMember (.getBindings ctx "python") "__vis_render_ctx__")]
        (.asString (.execute f (object-array [jsons])))))))

(defn bind-ctx!
  "Bind `context` in the sandbox as a NATIVE Python dict (not a foreign proxy),
   built from the SAME JSON the renderer prints — so `context` has real dict
   ergonomics (`.get`, comprehensions, `context[\"k\"]`) AND agrees byte-for-byte
   with the rendered `<context>` block. Falls back to proxy marshalling if
   json.loads is unavailable."
  [python-context data]
  (let [g (.getBindings ^Context python-context "python")
        jsons (json/write-json-str (->json-ready data))]
    (try
      (.putMember g "__vis_ctx_json__" jsons)
      (.eval ^Context python-context "python" "import json as __vis_j__\ncontext = __vis_j__.loads(__vis_ctx_json__)")
      (catch Throwable _
        (.putMember g "context" (->py data))))))

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

(defn- ^Value python-globals [python-context]
  (.getBindings ^Context python-context "python"))

(defn set-python-binding!
  "Bind `sym` -> `val` in the Python sandbox globals. Clojure fns are wired as
   callables; everything else is marshalled. (Name kept from the SCI twin for
   drop-in compatibility.)"
  [python-context sym val]
  (.putMember (python-globals python-context) (sym->py-name sym)
    (if (fn? val) (wrap-ifn val) (->py val))))

(defn bind-and-bump!
  "Set `sym` -> `val` in the env's Python sandbox."
  [env sym val]
  (set-python-binding! (:python-context env) sym val))

(defn bind-and-bump-with-doc!
  "Like `bind-and-bump!` but also records `doc` in the side `__vis_docs__` dict
   so a future live-vars view can surface name + doc (Python has no var
   metadata channel like SCI's :doc)."
  [env sym doc val]
  (let [python-context (:python-context env)
        g (python-globals python-context)]
    (set-python-binding! python-context sym val)
    ;; Stash name -> doc text in a Python dict global that `doc(name)` reads.
    (.putMember g "__vis_doc_sym__" (str sym))
    (.putMember g "__vis_doc_txt__" (str (or doc "vis-managed engine binding")))
    (.eval ^Context python-context "python"
      "globals().setdefault('__vis_docs__', {})[__vis_doc_sym__] = __vis_doc_txt__")
    nil))

(defn push-eval-result!
  "REPL-style stack push for the sandbox `_1 _2 _3` recovery slots, mirroring
   SCI's `*1 *2 *3`. Python convention is `_`, but we use `_1/_2/_3` to match
   the engine's three-deep history."
  [env value]
  (let [python-context (:python-context env)
        g (python-globals python-context)]
    (let [v1 (.getMember g "_1")
          v2 (.getMember g "_2")]
      (.putMember g "_3" v2)
      (.putMember g "_2" v1)
      (.putMember g "_1" (->py value)))))

(defn push-eval-error!
  "Park the most recent uncaught error in the sandbox `_e` slot (mirrors SCI
   `*e`). The `_1/_2/_3` value stack does NOT advance on error."
  [env throwable]
  (let [g (python-globals (:python-context env))]
    (.putMember g "_e" (str throwable))))

(defn reset-eval-bindings!
  "Clear `_1 _2 _3 _e` at turn start so a follow-up turn doesn't see leftovers."
  [env]
  (let [g (python-globals (:python-context env))]
    (doseq [s ["_1" "_2" "_3" "_e"]] (.putMember g s nil))))

;; =============================================================================
;; Python sandbox context creation
;; =============================================================================

(defn- install-introspection!
  "Wire Python `apropos(pat)` and `doc(name)` over the live globals — the
   sandbox's own discovery surface, mirroring SCI's `apropos`/`doc`. Both read
   the wired member keys; `doc` also reports callable-ness + any registered
   `__vis_docs__` text."
  [^Context ctx]
  (let [g (.getBindings ctx "python")
        names (fn [] (sort (filter #(not (str/starts-with? % "__vis"))
                             (map str (seq (.getMemberKeys g))))))]
    (.putMember g "apropos"
      (reify ProxyExecutable
        (execute [_ args]
          (let [pat (if (pos? (alength args)) (.asString ^Value (aget args 0)) "")]
            (->py (filterv #(str/includes? % pat) (names)))))))
    (.putMember g "doc"
      (reify ProxyExecutable
        (execute [_ args]
          (let [nm (when (pos? (alength args)) (.asString ^Value (aget args 0)))
                m  (when nm (.getMember g nm))
                docs (let [d (.getMember g "__vis_docs__")]
                       (when (and d (not (.isNull d)) (.hasHashEntries d) nm
                               (.hasHashEntry d (->py nm)))
                         (.asString (.getHashValue d (->py nm)))))]
            (cond
              (nil? nm)                  "doc(name): describe a sandbox global"
              (or (nil? m) (.isNull m))  (str nm ": <not found> — try apropos(\"\")")
              :else (str nm
                      (when (.canExecute m) " (callable)")
                      (when docs (str " — " docs))))))))))

(defn create-python-context
  "Create the embedded-GraalPy sandbox context with all available bindings.
   Drop-in twin of the SCI `create-python-context`.

   `custom-bindings` — map of symbol->value (tool fns + engine values). Fns are
   wired as Python callables; values are marshalled. Returns the same shape the
   SCI twin returns:

     {:python-context          <org.graalvm.polyglot.Context>
      :sandbox-ns       :python          ; placeholder (Python has one top scope)
      :initial-ns-keys  #{...baseline globals...}}"
  [custom-bindings]
  ;; Warm the shared auxiliary GraalPy contexts (printer + parser) NOW — at
  ;; session start, while NO eval is running. Creating a second polyglot Context
  ;; lazily WHILE an eval is executing on another (virtual) thread DEADLOCKS
  ;; Truffle (proven: sequential create+use is fine; lazy create during a live
  ;; eval hangs). Forcing the `defonce` delays here (sequential, pre-eval)
  ;; guarantees they exist before the first concurrent render/validation call;
  ;; only the first session in the process pays the warmup.
  (try @printer-context (catch Throwable _ nil))
  (try @parser-ctx (catch Throwable _ nil))
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
    ;; Sandbox self-discovery (apropos / doc) over the wired globals.
    (install-introspection! ctx)
    {:python-context ctx
     :sandbox-ns :python
     :initial-ns-keys (set (map str (seq (.getMemberKeys g))))}))

;; =============================================================================
;; Eval — the loop's hook (SCI's eval lived in loop.clj; we surface a thin one
;; here so the spike + future Python loop have a single entry point).
;; =============================================================================

(defn eval-block
  "Evaluate a whole Python `code` block in `python-context`. Returns
   `{:source code :result <clj>}` on success; throws the PolyglotException on
   failure (caller maps it to the engine error shape). Globals (defs/imports/
   state) persist across calls in the same context — same as SCI's sandbox ns."
  [python-context code]
  {:source code
   :result (->clj (.eval ^Context python-context "python" (str code)))})

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

(def ^:private split-top-level-py
  "Python that splits the source into top-level statements, each as
   [source kind bound-name]. kind ∈ expr|assign|def|stmt; bound-name is the
   target of a simple `x = …` or the name of a def/class (else None). Drives
   per-form evaluation (the SCI `eval-form` analogue)."
  (str "import ast as _a\n"
    "def _vis_split(_s):\n"
    "    out = []\n"
    "    for n in _a.parse(_s).body:\n"
    "        src = _a.get_source_segment(_s, n)\n"
    "        if isinstance(n, _a.Expr):\n"
    "            out.append([src, 'expr', None])\n"
    "        elif isinstance(n, (_a.FunctionDef, _a.AsyncFunctionDef, _a.ClassDef)):\n"
    "            out.append([src, 'def', n.name])\n"
    "        elif isinstance(n, _a.Assign) and len(n.targets)==1 and isinstance(n.targets[0], _a.Name):\n"
    "            out.append([src, 'assign', n.targets[0].id])\n"
    "        else:\n"
    "            out.append([src, 'stmt', None])\n"
    "    return out\n"))

(defn- split-top-level
  "Return a vec of {:src :kind :name} for each top-level Python statement in
   `code`. Throws PolyglotException on a syntax error (caller maps it)."
  [^Context ctx code]
  (let [g (.getBindings ctx "python")
        existing (.getMember g "_vis_split")]
    (when (or (nil? existing) (.isNull existing))
      (.eval ctx "python" split-top-level-py))
    (.putMember g "__vis_src__" (str code))
    (let [v (.eval ctx "python" "_vis_split(__vis_src__)")]
      (mapv (fn [i]
              (let [t (.getArrayElement v (long i))
                    nm (.getArrayElement t 2)]
                {:src  (.asString (.getArrayElement t 0))
                 :kind (.asString (.getArrayElement t 1))
                 :name (when-not (.isNull nm) (.asString nm))}))
        (range (.getArraySize v))))))

(defn run-python-block
  "Evaluate one Python `code` block in `python-context` PER-FORM, returning the SAME
   outcome contract `run-sci-code` produces in loop.clj:

     {:result <last-form-value-or-nil>
      :forms  [{:source :result}|{:source :error} …]
      :error  <op-error-or-nil>}

   Each top-level statement is evaluated on its own (the SCI `eval-form`
   analogue), so the form result reflects its nature — an expression yields its
   value, `x = …` yields x's bound value, a `def`/`class` yields the defined
   object. Tools fire in order through their ProxyExecutable wrappers. Stops at
   the first form that errors (its entry carries `:error`)."
  [python-context code]
  (let [ctx ^Context python-context
        g   (.getBindings ctx "python")
        forms (try (split-top-level ctx code)
                (catch PolyglotException e {::syntax e}))]
    (if-let [se (and (map? forms) (::syntax forms))]
      (let [err (map-polyglot-error se code)]
        {:result nil :forms [{:source code :error err}] :error err})
      (loop [todo forms, acc [], last-res nil]
        (if (empty? todo)
          {:result last-res :forms acc :error nil}
          (let [{:keys [src kind name]} (first todo)
                outcome (try
                          (let [v (.eval ctx "python" ^String src)
                                res (cond
                                      (= kind "expr") (->clj v)
                                      (or (= kind "assign") (= kind "def"))
                                      (->clj (.getMember g name))
                                      :else nil)]
                            {:source src :result res})
                          (catch PolyglotException e
                            {:source src :error (map-polyglot-error e src)}))]
            (if (:error outcome)
              {:result nil :forms (conj acc outcome) :error (:error outcome)}
              (recur (rest todo) (conj acc outcome) (:result outcome)))))))))

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
  [_python-context _db-info _session-id]
  [])
