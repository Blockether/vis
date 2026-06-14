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
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.internal.parse-diagnose :as parse-diagnose])
  (:import
   [org.graalvm.polyglot Context Engine Value PolyglotAccess PolyglotException]
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
   keyword keys, lists/tuples -> vectors, host objects (Java values that
   crossed the boundary, e.g. UUIDs) -> their underlying Java value via
   `asHostObject`, callables/opaque objects -> the raw `Value`."
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
    (.isHostObject v)   (.asHostObject v)
    :else               v))

(defn boundary-view
  "What a plain-data Clojure value LOOKS LIKE after the GraalPy round trip —
   the mechanical composition of `->py` then `->clj` without a Python context:
   map keys -> snake KEYWORDS (`:short-sha` -> `:short_sha`, and DATA-string
   keys keywordize verbatim: `\"M\"` -> `:M`), keyword/symbol VALUES -> snake
   strings, sets/seqs -> vectors. Idempotent.

   Every tool result a `:model-render-fn` receives in production has already
   crossed the boundary, so render fns MUST be written (and tested) against
   THIS shape — a render fn that reads the raw tool shape silently drops data
   (git_status once pinned a dirty tree as a bare `branch @sha` header, and
   the model read it as clean). Tests feed `(boundary-view raw-result)` to a
   render fn to pin that contract without booting GraalPy."
  [x]
  (cond
    (map? x)     (into {}
                   (map (fn [[k v]] [(keyword (key->py k)) (boundary-view v)]))
                   x)
    (or (vector? x) (seq? x) (set? x))
    (mapv boundary-view x)
    (keyword? x) (kw->snake x)
    (symbol? x)  (-> (str x) (str/replace #"[?!]" "") (str/replace "-" "_"))
    :else        x))

(defn sym->py-name
  "Clojure tool/binding symbol -> a Python-LEGAL global name. Purely mechanical:
   `/` and `-` fold to `_` (alias fold + kebab->snake); a trailing `!` (mutation
   marker) is dropped; a trailing `?` (predicate) becomes an `is_` prefix. So
   `git/status` -> `git_status`, `git/commit!` -> `git_commit`, `search/web` ->
   `search_web`, `fact-set!` -> `fact_set`, `exists?` -> `is_exists`. FULL SNAKE:
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
              ;; inline (helper is defined below): silence the
              ;; experimental virtual-thread warning on this standalone
              ;; context's implicit engine.
              (.allowExperimentalOptions true)
              (.option "engine.WarnVirtualThreadSupport" "false")
              (.allowAllAccess true)
              (.build))]
    (.eval ctx "python" vis-pp-python)
    ctx))

(defonce polyglot-noise-silenced
  ;; Route Truffle's own logging into the vis log file — the same sink
  ;; telemere's :file handler writes — instead of the controlling
  ;; terminal. Also removes the "[To redirect Truffle log output ...]"
  ;; startup hint, which prints whenever Truffle logs without a
  ;; configured destination. Honors an explicit operator override.
  ;;
  ;; The virtual-thread warning is silenced on the BUILDERS below
  ;; (`engine.WarnVirtualThreadSupport` is an EXPERIMENTAL option:
  ;; setting it as a system property makes every Engine/Context build
  ;; THROW "must be enabled with allowExperimentalOptions").
  (do
    (when-not (System/getProperty "polyglot.log.file")
      (let [vis-dir (java.io.File. (System/getProperty "user.home") ".vis")]
        (.mkdirs vis-dir)
        (System/setProperty "polyglot.log.file"
          (str (java.io.File. vis-dir "vis.log")))))
    true))

;; `engine.WarnVirtualThreadSupport=false` is applied INLINE on each
;; Engine/Context builder chain below (no shared helper: an untyped
;; helper arg would force reflection on the whole chain). We
;; deliberately run polyglot contexts on virtual threads (gateway turn
;; workers, Jetty handlers); the per-engine experimental warning is
;; pure noise. The option itself is experimental, hence
;; `allowExperimentalOptions` — it gates only option NAMES we set
;; explicitly, nothing about sandbox permissions.

(defonce ^:private printer-context (delay (build-printer-context)))

(defonce shared-engine
  ;; ONE process-wide GraalVM Engine. Every AGENT Context — the main session
  ;; sandbox AND every forked `sub_loop` child — is built ON this engine so that
  ;; creating a Context WHILE another eval runs does NOT deadlock Truffle.
  ;;
  ;; The hazard (GraalVM 25.0.1, reproduced 2026-06-10): a STANDALONE
  ;; `Context.build()` called during a live eval on a (virtual) thread freezes the
  ;; whole JVM at a Truffle safepoint. Routing every Context through ONE shared,
  ;; pre-built Engine moves engine init off the hot path — concurrent create is
  ;; then safe (verified: create-during-eval returns cleanly; N children eval
  ;; concurrently). Bonus: shared code cache ⇒ ~38ms warm child vs ~60ms standalone.
  ;; Built lazily; `create-python-context` forces it at session start (pre-eval).
  (delay (-> (Engine/newBuilder (into-array String ["python"]))
           (.allowExperimentalOptions true)
           (.option "engine.WarnVirtualThreadSupport" "false")
           (.build))))

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
;; Per-iteration LRU
;;
;; GraalPy has no cheap resolve hook for "vars referenced this iteration", so
;; these stay as honoured no-op vars; the live-vars feature can later read
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
      (.allowExperimentalOptions true)
      (.option "engine.WarnVirtualThreadSupport" "false")
      (.allowAllAccess false)
      (.allowIO IOAccess/NONE)
      (.allowPolyglotAccess PolyglotAccess/NONE)
      (.build))))

(defn count-top-level-forms
  "Number of top-level Python statements in `code`. Comment-/whitespace-only
   blocks return 0. Raises the underlying `PolyglotException` on a syntax
   error — that's a syntax issue, not a multi-statement issue."
  [code]
  (let [^Context ctx @parser-ctx
        b (.getBindings ctx "python")]
    (.putMember b "__vis_src__" (str code))
    (long (.asLong (.eval ctx "python"
                     "len(__import__('ast').parse(__vis_src__).body)")))))

(defn single-call-expression-head
  "Return the bare call name when `code` parses as exactly one top-level
   Python expression whose value is a direct name call, else nil.

   Examples: `advance({...})` -> \"advance\"; `x = advance({...})`, two calls,
   or `obj.advance({...})` -> nil. Parsing errors propagate to the caller."
  [code]
  (let [^Context ctx @parser-ctx
        b (.getBindings ctx "python")]
    (.putMember b "__vis_src__" (str code))
    (let [v (.eval ctx "python"
              (str "(lambda a,t: (t.body[0].value.func.id "
                "if len(t.body)==1 and isinstance(t.body[0],a.Expr) "
                "and isinstance(t.body[0].value,a.Call) "
                "and isinstance(t.body[0].value.func,a.Name) else ''))"
                "(__import__('ast'),__import__('ast').parse(__vis_src__))"))
          head (.asString v)]
      (when-not (str/blank? head) head))))

(defn observation-calls-only?
  "True if `code` parses as one or more top-level Python expressions
   where every expression is a direct name call (e.g. `cat('a')`),
   with no assignments, prose, imports, or complex statements."
  [code]
  (try
    (let [^Context ctx @parser-ctx
          b (.getBindings ctx "python")]
      (.putMember b "__vis_src__" (str code))
      (boolean (.asBoolean (.eval ctx "python"
                             (str "(lambda a,t: (all(isinstance(n, a.Expr) "
                               "and isinstance(n.value, a.Call) "
                               "and isinstance(n.value.func, a.Name) for n in t.body) "
                               "if t.body else False))"
                               "(__import__('ast'), __import__('ast').parse(__vis_src__))")))))
    (catch Throwable _ false)))

(defn direct-call-names
  "Return direct bare function names called anywhere in Python `code`.

   Attribute calls such as `obj.run()` are intentionally excluded; sandbox
   tools and engine controls are exposed as bare functions. Parsing errors
   propagate to the caller."
  [code]
  (let [^Context ctx @parser-ctx
        b (.getBindings ctx "python")]
    (.putMember b "__vis_src__" (str code))
    (->clj
      (.eval ctx "python"
        (str "[n.func.id for n in __import__('ast').walk("
          "__import__('ast').parse(__vis_src__)) "
          "if isinstance(n,__import__('ast').Call) and "
          "isinstance(n.func,__import__('ast').Name)]")))))

(defn advance-literal-string-key-errors
  "Return root `advance({...})` payload keys whose values are not literal
   strings. Only inspects the first positional dict arg. Parsing errors
   propagate to the caller; non-advance shapes return []."
  [code keys]
  (let [^Context ctx @parser-ctx
        b (.getBindings ctx "python")]
    (.putMember b "__vis_src__" (str code))
    (.putMember b "__vis_keys__" (->py (vec (map str keys))))
    (->clj
      (.eval ctx "python"
        (str
          "import ast\n"
          "t = ast.parse(__vis_src__)\n"
          "out = []\n"
          "if len(t.body) == 1 and isinstance(t.body[0], ast.Expr):\n"
          "    call = t.body[0].value\n"
          "    if isinstance(call, ast.Call) and isinstance(call.func, ast.Name) and call.func.id == 'advance' and call.args:\n"
          "        payload = call.args[0]\n"
          "        if isinstance(payload, ast.Dict):\n"
          "            wanted = set(__vis_keys__)\n"
          "            for k, v in zip(payload.keys, payload.values):\n"
          "                key = k.value if isinstance(k, ast.Constant) and isinstance(k.value, str) else None\n"
          "                if key in wanted and not (isinstance(v, ast.Constant) and isinstance(v.value, str)):\n"
          "                    out.append(key)\n"
          "out")))))

(defn validate-non-empty-block!
  "Throws `:vis/empty-block` when `code` parses to zero top-level statements
   (comment-only blocks). Iterations that produce no evidence are rejected at
   the model boundary."
  [code]
  (when (zero? (count-top-level-forms code))
    (throw (ex-info "Block is empty (only comments). Iteration produces no evidence."
             {:type :vis/empty-block :form-count 0}))))

(def BANNED_DEF_HEADS
  "Python constructs refused pre-eval. The Python sandbox is fresh per turn, so
   the only bans are belt-and-suspenders against the obvious sandbox-escape
   footguns on top of the Context restrictions."
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

(defn- python-globals ^Value [python-context]
  (.getBindings ^Context python-context "python"))

(defn set-python-binding!
  "Bind `sym` -> `val` in the Python sandbox globals. Clojure fns are wired as
   callables; everything else is marshalled."
  [python-context sym val]
  (.putMember (python-globals python-context) (sym->py-name sym)
    (if (fn? val) (wrap-ifn val) (->py val))))

(defn remove-python-binding!
  "Remove `sym` from the Python sandbox globals ENTIRELY — the member key
   disappears, so `apropos`/`dir` no longer list it and calling it raises
   a plain NameError. This is how a deactivated tool must vanish:
   `putMember nil` only parks a None under the name, which `apropos`
   still lists and which calls as 'NoneType is not callable'."
  [python-context sym]
  (try
    (.removeMember (python-globals python-context) (sym->py-name sym))
    (catch Throwable _ false)))

(defn bind-and-bump!
  "Set `sym` -> `val` in the env's Python sandbox."
  [env sym val]
  (set-python-binding! (:python-context env) sym val))

(defn bind-and-bump-with-doc!
  "Like `bind-and-bump!` but also records `doc` in the side `__vis_docs__` dict
   so a future live-vars view can surface name + doc (Python has no var
   metadata channel for doc text)."
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
  "REPL-style stack push for the sandbox `_1 _2 _3` recovery slots. Python
   convention is `_`, but we use `_1/_2/_3` to match the engine's three-deep
   history."
  [env value]
  (let [python-context (:python-context env)
        g (python-globals python-context)
        v1 (.getMember g "_1")
        v2 (.getMember g "_2")]
    (.putMember g "_3" v2)
    (.putMember g "_2" v1)
    (.putMember g "_1" (->py value))))

(defn push-eval-error!
  "Park the most recent uncaught error in the sandbox `_e` slot. The `_1/_2/_3`
   value stack does NOT advance on error."
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
   sandbox's own discovery surface. Both read
   the wired member keys; `doc` also reports callable-ness + any registered
   `__vis_docs__` text."
  [^Context ctx]
  (let [g (.getBindings ctx "python")
        ;; Python's own builtins (`len`, `print`, every `*Error`/`*Warning`
        ;; class, …) are NOT vis tools, so `apropos` must NOT list them — it is
        ;; a TOOL-discovery surface, not a dump of the Python stdlib. Captured
        ;; once (builtins don't change over the context's life). Names starting
        ;; with `_` (REPL slots `_1`/`_e`, `__vis*`, dunders) are engine
        ;; bookkeeping and are filtered too.
        builtin-names (set (try (->clj (.eval ctx "python" "dir(__builtins__)"))
                             (catch Throwable _ nil)))
        names (fn [] (sort (filter (fn [n] (and (not (str/starts-with? n "_"))
                                             (not (contains? builtin-names n))))
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

(def ^:private posix-compat-shim-src
  "Pure-Python preamble that replaces `subprocess` / `os.system` / `os.popen`
   with thin wrappers that DELEGATE to the vis shell tools (`shell_run` /
   `shell_bg` / `shell_logs` / `resource_stop`). Loaded once from the classpath
   (it ships next to this ns under `src`). Lazily looks up the tool callables in
   the sandbox globals at CALL time, so it self-adapts: when the shell tool is
   absent or its toggle is off it raises a clear 'enable the shell tool' message
   instead of a confusing native-spawn failure. Soft string-level coupling to
   the tool NAMES only — no load dependency on the shell extension."
  (delay
    (try (some-> (io/resource "com/blockether/vis/internal/posix_compat_shim.py")
           slurp)
      (catch Throwable _ nil))))

(defn- install-posix-compat-shim!
  "Eval the POSIX-compat shim into `ctx`. Best-effort: a failure here just
   leaves the sandbox without the bridge (subprocess stays unavailable), it must
   never break context creation."
  [^Context ctx]
  (when-let [src @posix-compat-shim-src]
    (try (.eval ctx "python" ^String src)
      (catch Throwable _ nil))))

(defn- build-agent-context
  "Build ONE deny-by-default GraalPy agent sandbox Context ON the shared `Engine`,
   wire `custom-bindings` (tool/verb fns as Python callables, values marshalled),
   install introspection, and return `{:python-context :sandbox-ns :initial-ns-keys}`.
   Shared by `create-python-context` (the main session sandbox) and `fork-context!`
   (each `sub_loop` child) so they are byte-for-byte the same sandbox — only the
   bound env (which ctx-atom the verbs close over) differs."
  [custom-bindings]
  (let [ctx (-> (Context/newBuilder (into-array String ["python"]))
              ;; Build on the shared Engine — THE thing that makes concurrent
              ;; child forks safe (see `shared-engine`).
              (.engine ^Engine @shared-engine)
              ;; deny-by-default; no host/file/native/threads. Tools do real IO
              ;; on the Clojure side via ProxyExecutable, so Python needs none.
              (.allowAllAccess false)
              (.allowIO IOAccess/NONE)
              (.allowCreateThread false)
              (.allowNativeAccess false)
              (.allowPolyglotAccess PolyglotAccess/NONE)
              (.build))
        g   (.getBindings ctx "python")]
    ;; REPL recovery slots first (so they land in the baseline and get filtered
    ;; out of the model-visible live-vars view).
    (doseq [s ["_1" "_2" "_3" "_e"]] (.putMember g s nil))
    ;; Tool fns + engine values (names snake-ified to Python-legal identifiers).
    (doseq [[sym val] (or custom-bindings {})]
      (.putMember g (sym->py-name sym) (if (fn? val) (wrap-ifn val) (->py val))))
    ;; Sandbox self-discovery (apropos / doc) over the wired globals.
    (install-introspection! ctx)
    ;; POSIX-compat: route subprocess / os.system to the shell tools. Eval'd
    ;; BEFORE the initial-ns-keys snapshot so any names it parks are baseline
    ;; (filtered out of the model-visible live-vars view).
    (install-posix-compat-shim! ctx)
    {:python-context ctx
     :sandbox-ns :python
     :initial-ns-keys (set (map str (seq (.getMemberKeys g))))}))

(defn create-python-context
  "Create the embedded-GraalPy sandbox context with all available bindings.

   `custom-bindings` — map of symbol->value (tool fns + engine values). Fns are
   wired as Python callables; values are marshalled. Returns:

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
  ;; Force the shared Engine NOW (session start, pre-eval) so the first forked
  ;; child later doesn't trigger engine init mid-eval.
  (try @shared-engine (catch Throwable _ nil))
  (build-agent-context custom-bindings))

(defn fork-context!
  "Fork a CHILD agent Context for a `sub_loop` — same deny-by-default sandbox as
   the main context, built ON the shared `Engine` so it is SAFE to create even
   while the parent's eval is running (GraalVM-verified: no Truffle deadlock).
   `custom-bindings` wires the child's tool/verb fns, which close over the CHILD's
   env (its own ctx-atom + recall-back). Returns the same
   `{:python-context :sandbox-ns :initial-ns-keys}` shape as
   `create-python-context`. The caller owns the child Context's lifecycle (close
   it when the sub_loop ends)."
  [custom-bindings]
  (build-agent-context custom-bindings))

;; =============================================================================
;; Eval — the loop's hook (a thin entry point so the spike + Python loop share
;; a single eval surface).
;; =============================================================================

(defn eval-block
  "Evaluate a whole Python `code` block in `python-context`. Returns
   `{:source code :result <clj>}` on success; throws the PolyglotException on
   failure (caller maps it to the engine error shape). Globals (defs/imports/
   state) persist across calls in the same context."
  [python-context code]
  {:source code
   :result (->clj (.eval ^Context python-context "python" (str code)))})

(defn- prose-leading-syntax-hint
  "When a `:python/syntax` failure came from a reply that OPENED with PROSE — the
   recurring 'the model answered in Markdown' bug — return an actionable directive
   to prepend to the raw CPython message; else nil.

   The whole reply is run as one Python program, so a leading sentence/heading is
   itself a SyntaxError. CPython's message points at whatever mangled token trips
   first — an apostrophe (`I've` → unterminated string), a `×`/em-dash (invalid
   character), or an orphaned `)` (the matching `(` got swallowed by a quote-pair).
   Those messages read like unicode/typo bugs, so they get MISDIAGNOSED (and svar
   gets blamed). This converts them into one clear cause.

   Detection is high-precision: take the first non-blank, non-`#`-comment line; if
   it does NOT parse as Python on its own AND reads like a sentence (markdown
   marker, or 3+ space-separated word runs), it's prose. A genuine code line with a
   typo elsewhere parses fine alone → no hint, raw error preserved."
  [code]
  (let [first-real (->> (str/split-lines code)
                     (map str/trim)
                     (remove str/blank?)
                     (remove #(str/starts-with? % "#"))
                     first)]
    (when (and (seq first-real)
            (try (count-top-level-forms first-real) false ; parses alone → real code
              (catch PolyglotException _
                (boolean
                  (or (re-find #"^(#{1,6}\s|[-*]\s|>\s)" first-real)         ; heading/bullet/quote
                    (re-find #"\*\*" first-real)                             ; **bold**
                    (re-find #"[A-Za-z]{2,}\s+[A-Za-z]{2,}\s+[A-Za-z]{2,}" first-real)))))) ; sentence
      (str "Your reply opened with PROSE, not Python. The engine runs your ENTIRE "
        "reply as one Python program, so the narration itself is the syntax error "
        "(this is NOT a unicode, typo, or svar problem). Put ALL narration in `#` "
        "comments above the code, or inside done(\"\"\"…\"\"\"); the reply must START "
        "with runnable Python. Original parser error: "))))

(def ^:private glued-top-level-forms-re
  "Signature of two top-level forms smashed onto one line with NO separator: a
   closing delimiter (`)` `]` `}`) or a closing triple-quote directly ABUTTING a
   new call `ident(` / `ident[`. e.g. `cat(...)done(`, `\"\"\")rg(`, `})patch([`.
   In valid Python this adjacency is essentially never legal (`)x(` is a
   SyntaxError; a real chained call carries a `.`), so the match is high-precision
   — and we only ever consult it AFTER CPython has already raised a SyntaxError.
   Properly newline-separated forms (`cat(...)\\ndone(...)`) do NOT match: the
   newline breaks the abutment."
  #"(?:[\)\]\}]|\"\"\"|''')[A-Za-z_]\w*\s*[\(\[]")

(defn- glued-top-level-forms?
  "True when `code` carries the glued-top-level-forms signature (see
   `glued-top-level-forms-re`). The recurring OpenAI/Codex failure: the model
   emits several tool calls / `done(...)` with no newline between them, so the
   whole reply is one unparseable line. CPython reports a bare `invalid syntax`
   that reads like a mystery; this turns it into one clear cause."
  [code]
  (boolean (re-find glued-top-level-forms-re (str code))))

(defn repair-glued-top-level-forms
  "Insert a newline at every glued top-level boundary the detector finds: after a
   closing delimiter / triple-quote that directly abuts a new `ident(` / `ident[`
   call. Returns the repaired source (unchanged when nothing matched). Only sound
   to call AFTER glued-top-level-forms? matched - it does not itself re-check."
  [code]
  (str/replace (str code)
    #"([\)\]\}]|\"\"\"|''')(?=[A-Za-z_]\w*\s*[\(\[])"
    "$1\n"))

(def ^:private fabricated-result-line-re
  "Signature of a FABRICATED transcript line - the model hallucinating the
   agent loop inside ONE reply (its call, an invented result, the next call).
   Shapes seen in the wild:
     - `_result{...}` / `_result[f1] {...}` - an invented tool-output line
     - a line-leading `=` glued straight onto the next call, e.g.
       `=git_add([...])` - a degenerate result marker before the call
     - `_results <results scope=\"t4/i1/f1\">` / a bare `<results ...>` or
       `</results>` tag - the model regenerating the frozen results-pin
       envelope verbatim (session 372994ce, t5)
     - `assistant# ...` - a fabricated role/turn marker
     - `SyntaxError: ...` - the model echoing the host's OWN rejection
       feedback as part of an invented transcript
   None of these is ever legal Python at the start of a line; a legit
   `_result = ...` or `x = git_add(...)` assignment does NOT match
   (those lines start with an identifier, not `_result{` / `=`), and
   `except SyntaxError:` / `raise SyntaxError(...)` start with their
   keyword, not the bare exception name."
  #"(?m)^[ \t]*(?:_result\s*[\{\[]|_results?\s+<|</?results\b|=\s*[A-Za-z_]\w*\s*\(|assistant#|SyntaxError:)")

(defn- fabricated-result-line?
  "True when `code` contains a fabricated `_result{...}` / `_result[...]`
   transcript line (see `fabricated-result-line-re`). High-precision: only
   ever consulted AFTER CPython has already raised a SyntaxError."
  [code]
  (boolean (re-find fabricated-result-line-re (str code))))

(defn truncate-fabricated-results
  "Cut `code` at the FIRST fabricated `_result...` line, keeping only the
   genuine code prefix the model wrote BEFORE it started hallucinating the
   transcript. Everything from the fabricated line on is dropped - any later
   calls were premised on an INVENTED tool output, so running them would act
   on fiction; the loop feeds the REAL result back instead and the model
   continues from truth. Returns the truncated source, or nil when no
   fabricated line exists or the prefix is blank (the reply OPENED with a
   fabricated result - nothing genuine to run)."
  [code]
  (let [lines (vec (str/split-lines (str code)))
        idx   (first (keep-indexed
                       (fn [i l] (when (re-find fabricated-result-line-re l) i))
                       lines))]
    (when idx
      (let [prefix (str/join "\n" (subvec lines 0 idx))]
        (when-not (str/blank? prefix) prefix)))))

(def ^:dynamic *auto-repair-glued-forms?*
  "When true, run-python-block AUTO-REPAIRS a reply whose top-level forms were
   smashed onto one line (the OpenAI/Codex missing-newline failure) by inserting
   a newline at each glued boundary and re-splitting, instead of bouncing the
   whole turn. ON by default: the repair is high-precision (only fires AFTER
   CPython raised a SyntaxError AND glued-top-level-forms? matched) and the
   repaired source rides back under :auto-repaired so the model + user see it."
  true)

(def ^:dynamic *auto-repair-fabricated-results?*
  "When true, run-python-block AUTO-REPAIRS a reply that hallucinated the
   agent transcript (fabricated `_result{...}` lines after its own calls) by
   TRUNCATING the source at the first fabricated line and running only the
   genuine prefix, instead of bouncing the whole turn. ON by default: it only
   fires AFTER CPython raised a SyntaxError AND `fabricated-result-line?`
   matched, and the rewrite rides back under :auto-repaired so the model +
   user see it."
  true)

(def ^:dynamic *auto-repair-brackets?*
  "When true, a bracket-balance syntax hint ALSO appends `repair-bracket-balance`'s
   single-candidate suggested fix. OFF by default: the walker only DIAGNOSES; the
   auto-fix stays gated behind this flag until proven safe in the wild."
  false)

(defn- sanitize-cause-data
  "Prune host noise from a Clojure tool's ex-data before it rides into the
   op-error `:data` (the model trailer AND every channel render read it):
   drop the legacy nested `:tool-result` envelope (a verbatim copy of the
   same failure), strip the Java `:trace` from a structured `:error`, and
   drop the `:error` entirely when all it adds is the message the op-error
   already carries at top level. Actionable fields (`:reason`, `:unknown`,
   `:failures`, `:loop-hint`, …) survive untouched."
  [d message]
  (let [d (dissoc d :tool-result)
        e (:error d)]
    (if-not (map? e)
      d
      (let [e' (not-empty (dissoc e :trace))]
        (if (or (nil? e') (= e' {:message message}))
          (dissoc d :error)
          (assoc d :error e'))))))

(defn map-polyglot-error
  "Map a GraalPy `PolyglotException` into the engine's op-error shape. `:phase`
   is `:python/syntax` for parse errors, else `:python/runtime`; `:line`/`:column`
   come from the Python
   source location when present. A host (Clojure-tool) exception is unwrapped so
   its real message surfaces. Recurring syntax-failure classes get an actionable
   hint prepended: a NON-ASCII char in code position (em-dash, x, curly quote -
   CPython's `invalid character`, precise wherever it lands), a PROSE-leading
   reply (see `prose-leading-syntax-hint`, first-line only), a FABRICATED tool
   result (`_result{...}` transcript lines hallucinated after the model's own
   call; see `fabricated-result-line?`), GLUED top-level
   forms (`cat(...)done(...)` on one line - the OpenAI/Codex missing-newline
   pattern; see `glued-top-level-forms?`), and - via `parse-diagnose` - an
   unbalanced double-quote or an unbalanced (), [], {} bracket pinpointed to its
   line/col."
  [^PolyglotException e code]
  (let [host?      (.isHostException e)
        cause      (when host? (.asHostException e))
        loc        (.getSourceLocation e)
        syntax?    (and (not host?) (.isSyntaxError e))
        base       (or (when cause (or (ex-message cause) (.getMessage cause)))
                     (.getMessage e))
        ;; Prose-leading is the ROOT cause when the reply OPENS with prose (a `x`
        ;; in a leading sentence must be reported as PROSE, not "avoid x" - that
        ;; was the misdiagnosis we fixed). So check it FIRST. Non-ascii is the
        ;; fallback for a genuinely-code reply with a stray non-ASCII char mid-line
        ;; (CPython's "invalid character", precise wherever it lands - the
        ;; em-dash-at-line-71 case the first-line-only prose detector misses).
        prose-hint (when syntax? (prose-leading-syntax-hint code))
        non-ascii? (boolean (and syntax? (not prose-hint) base (re-find #"invalid character" base)))
        ;; Fabricated tool result: the model simulated the whole agent loop in
        ;; one reply (its call, an invented `_result{...}` transcript line,
        ;; then more calls). Checked BEFORE glued - the fabricated line often
        ;; ALSO abuts the next call (`}git_add(`), so the glue detector would
        ;; otherwise shadow the real cause with a misleading newline directive.
        fabricated? (boolean (and syntax? (not prose-hint) (not non-ascii?)
                               (fabricated-result-line? code)))
        ;; Glued top-level forms: a genuinely-code reply whose statements ran
        ;; together on one line (no newline between them). Disjoint from the
        ;; above - prose-leading opens with narration, non-ascii is CPython's
        ;; `invalid character`; a glue is plain `invalid syntax` on code. Check
        ;; last so those keep priority when they apply.
        glued?     (boolean (and syntax? (not prose-hint) (not non-ascii?)
                              (not fabricated?)
                              (glued-top-level-forms? code)))
        ;; parse-diagnose heuristics, only when none of the structural detectors
        ;; above already explained the failure. Quote-balance first (an open
        ;; string makes the reader treat brackets as bare tokens, so its diagnosis
        ;; supersedes a bracket count), then bracket-balance.
        quote-hint   (when (and syntax? (not prose-hint) (not non-ascii?)
                             (not fabricated?) (not glued?))
                       (:hint (parse-diagnose/diagnose-quote-balance code)))
        bracket-diag (when (and syntax? (not prose-hint) (not non-ascii?)
                             (not fabricated?) (not glued?)
                             (not quote-hint))
                       (parse-diagnose/diagnose-bracket-balance code))
        bracket-hint (when bracket-diag
                       (str (:hint bracket-diag)
                         (when *auto-repair-brackets?*
                           (when-let [fix (parse-diagnose/repair-bracket-balance code)]
                             (str " Suggested fix: " (:change fix) ".")))))
        hint       (cond
                     prose-hint prose-hint
                     non-ascii?
                     (str "A non-ASCII character leaked into CODE position - it is only "
                       "legal inside a \"...\" string or a `#` comment. This is almost always "
                       "a smart em-dash, en-dash, curly quote, or x that you "
                       "meant as prose. Replace it with plain ASCII, or move that whole line "
                       "into a `#` comment. Original parser error: ")
                     fabricated?
                     (str "You FABRICATED a tool result: a `_result{...}` line is the "
                       "transcript's rendering of a tool's OUTPUT, never code you write. "
                       "Emit ONLY the calls and STOP - the engine runs them and sends the "
                       "REAL results back; never predict a result, and never continue with "
                       "calls that depend on an invented one. Original parser error: ")
                     glued?
                     (str "You glued two top-level forms onto ONE line with no separator "
                       "(e.g. `cat(...)done(...)` or `\"\"\")rg(...)`). The engine runs your "
                       "whole reply as one Python program, so adjacent calls on one line are "
                       "a SyntaxError. Put EACH statement on its OWN line - one form per line, "
                       "newline after every call. Original parser error: ")
                     quote-hint   (str quote-hint " Original parser error: ")
                     bracket-hint (str bracket-hint " Original parser error: "))
        msg        (if hint (str hint base) base)]
    {:message msg
     :data (cond-> {:phase (cond host?   :python/host
                             syntax? :python/syntax
                             :else   :python/runtime)}
             (some? loc) (assoc :line (.getStartLine loc)
                           :column (.getStartColumn loc))
             non-ascii?   (assoc :non-ascii-in-code? true)
             fabricated?  (assoc :fabricated-results? true)
             glued?       (assoc :glued-forms? true)
             prose-hint   (assoc :prose-leading? true)
             quote-hint   (assoc :unbalanced-quote? true)
             bracket-diag (assoc :unbalanced-bracket? true)
             ;; ex-data from a Clojure tool's ex-info rides through so e.g.
             ;; :tool/banned, :vis/* keep their type for the trailer — minus
             ;; host noise (nested envelope / Java trace, see sanitize-cause-data).
             (and cause (instance? clojure.lang.IExceptionInfo cause))
             (merge (sanitize-cause-data (ex-data cause) base)))}))

(def ^:private split-top-level-py
  "Python that splits the source into top-level statements, each as
   [source kind bound-name]. kind ∈ expr|assign|def|stmt; bound-name is the
   target of a simple `x = …` or the name of a def/class (else None). Drives
   per-form evaluation.

   `_vis_seg` extracts each statement's source via pure-Python line/col slicing
   instead of `ast.get_source_segment`. GraalPy's native get_source_segment
   TRUNCATES the segment when the source carries an astral-plane char (e.g. an
   emoji 👆 in a `done(\"\"\"…\"\"\")` answer) — UTF-16-vs-codepoint offset skew
   drops the closing quotes, so the lone re-eval raises a spurious
   'unterminated triple-quoted string' SyntaxError, the answer form errors, the
   turn never finalizes, and the model loops re-emitting `done(...)`. Python str
   slicing by ast line/col is codepoint-correct, so it preserves the original
   source exactly. (Session f41ca531.)"
  (str "import ast as _a\n"
    "def _vis_seg(_s, n):\n"
    "    el = getattr(n, 'end_lineno', None); ec = getattr(n, 'end_col_offset', None)\n"
    "    if el is None or ec is None:\n"
    "        return _a.get_source_segment(_s, n)\n"
    "    lines = _s.splitlines(keepends=True)\n"
    "    chunk = lines[n.lineno-1:el]\n"
    "    if not chunk:\n"
    "        return ''\n"
    "    if len(chunk) == 1:\n"
    "        return chunk[0][n.col_offset:ec]\n"
    "    chunk[0] = chunk[0][n.col_offset:]\n"
    "    chunk[-1] = chunk[-1][:ec]\n"
    "    return ''.join(chunk)\n"
    "def _vis_split(_s):\n"
    "    out = []\n"
    "    for n in _a.parse(_s).body:\n"
    "        src = _vis_seg(_s, n)\n"
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

(def ^:private current-form-idx-var
  "Lazily resolved `extension/*current-form-idx*` dynamic var. Resolved via
   `requiring-resolve` (not a ns `:require`) so this sandbox ns stays free of
   a dependency edge on the extension registry. Bound per top-level form by
   `run-python-block` so `record-render-entry!` can stamp `:form-idx` on every
   channel sink entry - without it all tool IR clumps onto form 0 on restore."
  (delay (requiring-resolve 'com.blockether.vis.internal.extension/*current-form-idx*)))

(defn run-python-block
  "Evaluate one Python `code` block in `python-context` PER-FORM, returning the SAME
   outcome contract `run-python-code` produces in loop.clj:

     {:result <last-form-value-or-nil>
      :forms  [{:source :result}|{:source :error} ...]
      :error  <op-error-or-nil>
      :auto-repaired <nil-or {:kind :glued-forms|:fabricated-results :original .. :repaired ..}>}

   Each top-level statement is evaluated on its own, so the form result reflects
   its nature - an expression yields its value, `x = ...` yields x's bound value,
   a `def`/`class` yields the defined object. Tools fire in order through their
   ProxyExecutable wrappers. Stops at the first form that errors (its entry
   carries `:error`).

   Each form's eval runs with `extension/*current-form-idx*` bound to its
   zero-based index so `record-render-entry!` stamps `:form-idx` on every
   channel sink entry - the persistence rebuild partitions tool IR back onto
   the form that emitted it instead of clumping everything on form 0.

   AUTO-REPAIR (only after the split raised a SyntaxError):
     1. FABRICATED transcript - the model hallucinated the agent loop in one
        reply (its call, an invented `_result{...}` line, more calls). When
        `*auto-repair-fabricated-results?*` is on, the source is TRUNCATED at
        the first fabricated line and only the genuine prefix runs - any
        later calls were premised on an invented result. The prefix is
        glue-repaired too when it needs it.
     2. GLUED top-level forms (the OpenAI/Codex missing-newline failure) -
        when `*auto-repair-glued-forms?*` is on, a newline is inserted at
        each glued boundary and the source re-split.
   On success the REPAIRED forms run and the rewrite rides back under
   `:auto-repaired` so the loop can disclose it."
  [python-context code]
  (let [ctx ^Context python-context
        g   (.getBindings ctx "python")
        do-split (fn [src] (try (split-top-level ctx src)
                             (catch PolyglotException e {::syntax e})))
        forms0   (do-split code)
        attempt  (fn [kind fixed]
                   (let [f (when (and fixed (not= fixed code)) (do-split fixed))]
                     (when (and f (not (and (map? f) (::syntax f))))
                       {:code fixed :forms f :kind kind})))
        repaired (when (and (map? forms0) (::syntax forms0))
                   (or (when (and *auto-repair-fabricated-results?*
                               (fabricated-result-line? code))
                         (let [cut (truncate-fabricated-results code)]
                           (or (attempt :fabricated-results cut)
                             (when (and cut *auto-repair-glued-forms?*
                                     (glued-top-level-forms? cut))
                               (attempt :fabricated-results
                                 (repair-glued-top-level-forms cut))))))
                     (when (and *auto-repair-glued-forms?*
                             (glued-top-level-forms? code))
                       (attempt :glued-forms (repair-glued-top-level-forms code)))))
        run-code    (if repaired (:code repaired) code)
        forms       (if repaired (:forms repaired) forms0)
        repair-note (when repaired
                      {:kind (:kind repaired) :original code :repaired (:code repaired)})]
    (if-let [se (and (map? forms) (::syntax forms))]
      (let [err (map-polyglot-error se run-code)]
        {:result nil :forms [{:source run-code :error err}] :error err})
      (loop [todo forms, acc [], last-res nil]
        (if (empty? todo)
          {:result last-res :forms acc :error nil :auto-repaired repair-note}
          (let [{:keys [src kind name]} (first todo)
                outcome (with-bindings {@current-form-idx-var (count acc)}
                          (try
                            (let [v (.eval ctx "python" ^String src)
                                  res (cond
                                        (= kind "expr") (->clj v)
                                        (or (= kind "assign") (= kind "def"))
                                        (->clj (.getMember g name))
                                        :else nil)]
                              {:source src :result res})
                            (catch PolyglotException e
                              {:source src :error (map-polyglot-error e src)})))]
            (if (:error outcome)
              {:result nil :forms (conj acc outcome) :error (:error outcome) :auto-repaired repair-note}
              (recur (rest todo) (conj acc outcome) (:result outcome)))))))))

;; =============================================================================
;; Engine-owned sandbox names + restore (NOOP)
;; =============================================================================

(def SYSTEM_VAR_NAMES
  "Engine-owned symbols hidden from user live-var listings."
  '#{ctx})

(defn system-var-sym?
  [sym]
  (contains? SYSTEM_VAR_NAMES sym))

(defn restore-sandbox!
  "NOOP. The Python sandbox is fresh per turn; cross-turn memory rides on
   `:session/facts` + the per-form blob."
  [_python-context _db-info _session-id]
  [])
