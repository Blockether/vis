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
   [clojure.string :as str]
   [com.blockether.vis.internal.parse-diagnose :as parse-diagnose]
   [flatland.ordered.map :as omap]
   [taoensso.telemere :as tel])
  (:import
   [org.graalvm.polyglot Context Engine Value PolyglotAccess PolyglotException]
   [org.graalvm.polyglot.io IOAccess]
   [org.graalvm.polyglot.proxy ProxyExecutable ProxyArray ProxyHashMap]
   [java.nio.charset StandardCharsets]
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
   (`\"files_only\"` -> `:files_only`, `\"from_anchor\"` -> `:from_anchor`). The
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
    ;; calls — `git-fetch!` -> "git_fetch" — so stored forms read consistently.
    (symbol? x)  (-> (str x) (str/replace #"[?!]" "") (str/replace "-" "_"))
    ;; `java.util.Map` covers BOTH Clojure maps (which implement it) AND a raw
    ;; ordered `LinkedHashMap` a tool returns (e.g. cat's `:lines` anchor map).
    ;; The new LinkedHashMap preserves the source's ITERATION ORDER — Clojure
    ;; array-map canonical key order, or a LinkedHashMap's insertion order — so
    ;; the live `ctx` dict and the rendered text agree, and ordered tool maps
    ;; reach Python as ordered dicts (not opaque host objects).
    (instance? java.util.Map x)
    (let [^LinkedHashMap hm (LinkedHashMap.)]
      (doseq [[k v] x] (.put hm (key->py k) (->py v)))
      (ProxyHashMap/from hm))
    (or (vector? x) (seq? x) (set? x))
    (ProxyArray/fromList (ArrayList. ^java.util.Collection (mapv ->py x)))
    ;; UUIDs (workspace/session ids in ctx) and java.time instants have no
    ;; Python analog — GraalPy would otherwise expose them as opaque
    ;; `<JavaObject[...]>` host pointers (not valid Python, leaks an address).
    ;; Stringify so the rendered ctx and the live dict both read as plain str.
    (or (instance? java.util.UUID x) (instance? java.time.temporal.Temporal x))
    (str x)
    ;; numbers and other auto-convertible boxed types — hand straight to polyglot
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
    ;; Dicts preserve INSERTION ORDER: GraalPy's key iterator is insertion-
    ;; ordered, so accumulate into a flatland ordered-map (NOT a hash-map, whose
    ;; >8-key promotion scrambles order). Without this, a round-tripped ordered
    ;; tool result (cat's `:anchors` LinkedHashMap) comes back HASH-ordered and
    ;; the model reads the file out of line order. ordered-map is still a
    ;; persistent Clojure map (assoc/dissoc/keyword-lookup all work downstream).
    (.hasHashEntries v) (let [it (.getHashKeysIterator v)]
                          (loop [m (omap/ordered-map)]
                            (if (.hasIteratorNextElement it)
                              (let [k (.getIteratorNextElement it)]
                                (recur (assoc m
                                         (py-key->clj (.asString k))
                                         (->clj (.getHashValue v k)))))
                              m)))
    (.isHostObject v)   (.asHostObject v)
    :else               v))

(defn boundary-view
  "What a plain-data Clojure value LOOKS LIKE after the GraalPy round trip —
   the mechanical composition of `->py` then `->clj` without a Python context:
   map keys -> snake KEYWORDS (`:short-sha` -> `:short_sha`, and DATA-string
   keys keywordize verbatim: `\"M\"` -> `:M`), keyword/symbol VALUES -> snake
   strings, sets/seqs -> vectors. Idempotent.

   Every tool result the model sees in production (serialized structurally
   by `ctx-renderer/render-form-value`) has already crossed this boundary,
   so assertions about what the model reads MUST be written against THIS
   shape. Tests feed `(boundary-view raw-result)` to pin that contract
   without booting GraalPy."
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
   `search_web`, `exists?` -> `is_exists`. FULL SNAKE:
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
;; real PYTHON object (an ordered `ProxyHashMap` via `->py`, which GraalPy
;; treats as a native dict), and its printed form is produced BY PYTHON
;; (GraalPy `__vis_pp__`), not by a Clojure reimplementation. So the
;; `<context>` text and `repr(context)` cannot drift: the SAME polyglot object
;; is both bound live and pretty-printed — NO JSON round-trip.
;; =============================================================================

(def ^:private vis-pp-python
  "Deterministic Python pretty-printer for the `ctx` dict. Double-quoted strings,
   True/False/None, insertion order preserved, inline when it fits `width` else
   one entry per line (closing bracket aligned under the entry column)."
  "
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
")

(def ^:private async-runtime-python
  "ASYNC-BY-DEFAULT runtime (maki-style, on GraalPy — no asyncio/select/socket).

   Tools are DEFERRED: calling `cat('x')` returns a `__vis_Call__` thunk instead
   of running. You drive them three ways:
     • `await cat('x')`                         — canonical, anywhere nested
     • `a, b = await gather(cat(x), cat(y))`    — CONCURRENT on virtual threads
     • bare top-level `cat('x')` / `x = cat(y)` — auto-SETTLED by run-python-block

   `await` works because run-python-block AST-wraps an await-bearing program in
   an `async def` (GraalPy rejects top-level await), declares its assigned names
   `global` so REPL vars still persist, and drives the coroutine with the
   trampoline `__vis_drive__`. `gather` dispatches its awaitables to the
   host virtual-thread pool `__vis_par__` (bound from Clojure) — real overlap on
   blocking tool I/O, no event loop. A `__vis_Call__` that leaks into output
   (forgot `await`) repr's a loud hint instead of running silently."
  "
import ast as __vis_ast__

class __vis_Call__:
    __slots__ = ('fn', 'a', 'k', 'nm')
    def __init__(self, fn, a, k, nm='tool'):
        self.fn = fn; self.a = a; self.k = k; self.nm = nm
    def __await__(self):
        return (yield self)
    def __repr__(self):
        return '<unawaited async tool call: write `await ' + self.nm + '(...)`>'

class __vis_Gather__:
    __slots__ = ('aws',)
    def __init__(self, aws):
        self.aws = aws
    def __await__(self):
        return (yield self)

def gather(*aws):
    if len(aws) == 1 and isinstance(aws[0], (list, tuple)):
        aws = list(aws[0])
    return __vis_Gather__(list(aws))

def __vis_exec_call__(c):
    return c.fn(*c.a, **c.k)

def __vis_settle__(v):
    if isinstance(v, __vis_Call__):
        return __vis_exec_call__(v)
    if isinstance(v, __vis_Gather__):
        return __vis_par__([(lambda a=a: __vis_settle__(a)) for a in v.aws])
    if hasattr(v, '__await__') or hasattr(v, 'send'):
        return __vis_drive__(v)
    return v

def __vis_settle_binding__(name):
    g = globals(); g[name] = __vis_settle__(g[name]); return g[name]

def __vis_drive__(coro):
    it = coro.__await__() if hasattr(coro, '__await__') else coro
    send = None
    while True:
        try:
            y = it.send(send)
        except StopIteration as e:
            return e.value
        if isinstance(y, __vis_Call__):
            send = __vis_exec_call__(y)
        elif isinstance(y, __vis_Gather__):
            send = __vis_par__([(lambda a=a: __vis_settle__(a)) for a in y.aws])
        else:
            send = y

def __vis_deferred__(realfn, nm='tool'):
    def __vis_tool__(*a, **k):
        return __vis_Call__(realfn, a, k, nm)
    __vis_tool__.__name__ = nm
    return __vis_tool__

def __vis_assigned_names__(body):
    names = []; seen = set()
    def add(n):
        if n not in seen:
            seen.add(n); names.append(n)
    for node in body:
        if isinstance(node, __vis_ast__.Assign):
            for t in node.targets:
                for nn in __vis_ast__.walk(t):
                    if isinstance(nn, __vis_ast__.Name):
                        add(nn.id)
        elif isinstance(node, (__vis_ast__.AnnAssign, __vis_ast__.AugAssign)):
            for nn in __vis_ast__.walk(node.target):
                if isinstance(nn, __vis_ast__.Name):
                    add(nn.id)
        elif isinstance(node, (__vis_ast__.FunctionDef, __vis_ast__.AsyncFunctionDef, __vis_ast__.ClassDef)):
            add(node.name)
        elif isinstance(node, __vis_ast__.For):
            for nn in __vis_ast__.walk(node.target):
                if isinstance(nn, __vis_ast__.Name):
                    add(nn.id)
        elif isinstance(node, (__vis_ast__.Import, __vis_ast__.ImportFrom)):
            for al in node.names:
                add((al.asname or al.name).split('.')[0])
        elif isinstance(node, __vis_ast__.With):
            for itm in node.items:
                if itm.optional_vars is not None:
                    for nn in __vis_ast__.walk(itm.optional_vars):
                        if isinstance(nn, __vis_ast__.Name):
                            add(nn.id)
    return names

def __vis_run_async__(src):
    g = globals()
    tree = __vis_ast__.parse(src)
    assigned = __vis_assigned_names__(tree.body)
    body = list(tree.body)
    if body and isinstance(body[-1], __vis_ast__.Expr):
        body[-1] = __vis_ast__.Return(value=body[-1].value)
    inner = ([__vis_ast__.Global(names=assigned)] if assigned else []) + body
    fn = __vis_ast__.AsyncFunctionDef(
        name='__vis_main__',
        args=__vis_ast__.arguments(posonlyargs=[], args=[], vararg=None,
                                   kwonlyargs=[], kw_defaults=[], kwarg=None, defaults=[]),
        body=inner, decorator_list=[], returns=None, type_params=[])
    mod = __vis_ast__.Module(body=[fn], type_ignores=[])
    __vis_ast__.fix_missing_locations(mod)
    exec(compile(mod, '<prog>', 'exec'), g)
    g['__vis_async_result__'] = __vis_drive__(g['__vis_main__']())
    return assigned

def __vis_defer_tools__():
    g = globals()
    for __vis_n__ in list(__vis_defer_names__):
        if __vis_n__ in g and callable(g[__vis_n__]):
            g[__vis_n__] = __vis_deferred__(g[__vis_n__], __vis_n__)
")

(defn- build-printer-context
  "Trusted, process-wide GraalPy context whose ONLY job is to pretty-print a
   polyglot value into the canonical Python-literal string. Permissive
   (`allowAllAccess`) because it never runs agent code — only our `__vis_pp__`
   over our own data."
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
  "Render a Clojure value as the canonical Python-literal string — produced by
   Python (`__vis_pp__`) inside GraalPy, so it matches `repr`-style Python and
   the live `ctx` dict the agent reads. The value is marshalled to a polyglot
   object via `->py` (an ordered `ProxyHashMap`/`ProxyArray` that GraalPy treats
   as a native `dict`/`list`) and pretty-printed DIRECTLY — no JSON round-trip."
  ^String [data]
  (let [^Context ctx @printer-context]
    (locking ctx
      (let [^Value pp (.getMember (.getBindings ctx "python") "__vis_pp__")]
        (.asString (.execute pp (object-array [(->py data)])))))))

(declare set-python-binding!)

(defn bind-ctx!
  "Bind the standing context in the sandbox as an ordered polyglot dict (`->py`
   → `ProxyHashMap`, which GraalPy treats as a NATIVE `dict`: `session[\"k\"]`, `.get`,
   `.items()`, comprehensions and `{**ctx}` all work) built from the SAME
   projection the renderer prints — so the live dict and the wire's
   `session[\"a\"][\"b\"] = …` structural deltas agree. Bound under `session` only —
   decoupled from `r`, no legacy `context` alias. No JSON round-trip."
  [python-context data]
  (set-python-binding! python-context 'session data))

;; =============================================================================
;; Result pickle boundary — store a form result as raw Python pickle bytes so a
;; FRESH-per-turn interpreter can rebind it losslessly (`r["tN/iN/fN"]`).
;;
;; The blob persisted in `:result` IS the raw `pickle.dumps` bytes (protocol 5,
;; the C `_pickle` accelerator) — pure binary, no base64. The only friction is
;; the rebind hop: a Java `byte[]` crosses into GraalPy as a SIGNED ForeignList,
;; not `bytes`, so `pickle.loads` rejects it. The fix is the latin-1 bridge:
;; ship the bytes as a 1:1 byte↔codepoint String and re-`encode('latin-1')`
;; Python-side — a single C-level op (no per-byte loop).
;; =============================================================================

(defn pickle->bytes
  "Pickle the polyglot Python value `v` (live in `ctx`) to raw host bytes via
   the context's own `pickle.dumps`. Returns a Java `byte[]` (what lands in the
   persisted `:result`), or nil when `v` is absent/unpicklable — the caller
   stores no result blob then."
  ^bytes [^Context ctx v]
  (when (and ctx (some? v))
    (locking ctx
      (let [g (.getBindings ctx "python")]
        (try
          (.putMember g "__vis_pkl_in__" v)
          ;; Tool results cross `->py` as host PROXIES (ForeignDict / ForeignList /
          ;; ForeignNone), which `pickle` REFUSES ("cannot pickle 'ForeignDict'").
          ;; So deep-convert to NATIVE Python first, THEN dump:
          ;;   - mappings (`.keys()`) → native dict, preserving key ORDER;
          ;;   - sequences → native list;
          ;;   - native scalars pass straight through (string/int result unchanged);
          ;;   - anything else (foreign null, opaque handle) → None, so a stray
          ;;     unpicklable leaf never sinks the whole result.
          ;; protocol=HIGHEST (5): smallest+fastest in THIS GraalPy (C `_pickle`);
          ;; loads auto-detects, so only dumps pins it.
          (.as ^Value (.eval ctx "python"
                        (str "def __vis_native__(o):\n"
                          "    if o is None: return None\n"
                          "    if isinstance(o, (str, bytes, bytearray, bool, int, float)): return o\n"
                          "    if hasattr(o, 'keys'):\n"
                          "        return {k: __vis_native__(o[k]) for k in o.keys()}\n"
                          "    try:\n"
                          "        return [__vis_native__(x) for x in o]\n"
                          "    except TypeError:\n"
                          "        return None\n"
                          "import pickle as __vis_pk__\n"
                          "__vis_pk__.dumps(__vis_native__(__vis_pkl_in__), protocol=__vis_pk__.HIGHEST_PROTOCOL)"))
            (Class/forName "[B"))
          (catch Throwable t
            ;; An unpicklable result (a lambda, an open handle, …) must NOT crash
            ;; the form — we store no blob and the form still completes — but it
            ;; means `r["tN/iN/fN"]` won't rebind later, so it's a real warn.
            (tel/log! {:level :warn :id ::pickle-dump-failed :data {:error (ex-message t)}}
              "pickle->bytes: result is unpicklable; no result blob stored")
            nil)
          (finally (.putMember g "__vis_pkl_in__" nil)))))))

(defn bytes->py
  "Reconstruct a native Python object in `ctx` from raw pickle `bytes` via the
   C-level latin-1 bridge. Returns the live polyglot `Value`, ready to bind into
   the sandbox (e.g. under `r[\"tN/iN/fN\"]`). nil on failure."
  ^Value [^Context ctx ^bytes pkl]
  (when (and ctx pkl)
    (locking ctx
      (let [g (.getBindings ctx "python")]
        (try
          (.putMember g "__vis_pkl_b__" (String. pkl StandardCharsets/ISO_8859_1))
          (.eval ctx "python" "__import__('pickle').loads(__vis_pkl_b__.encode('latin-1'))")
          (catch Throwable t
            ;; A stored blob that won't load (corrupt / cross-version) means the
            ;; model asked for `r["tN/iN/fN"]` and we can't give it back — warn
            ;; so it's visible, return nil so the getter degrades gracefully.
            (tel/log! {:level :warn :id ::pickle-load-failed :data {:error (ex-message t)}}
              "bytes->py: stored result blob failed to unpickle")
            nil)
          (finally (.putMember g "__vis_pkl_b__" nil)))))))

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

(defn validate-non-empty-block!
  "Throws `:vis/empty-block` when `code` parses to zero top-level statements
   (comment-only blocks). Iterations that produce no evidence are rejected at
   the model boundary."
  [code]
  (when (zero? (count-top-level-forms code))
    (throw (ex-info "Block is empty (only comments). Iteration produces no evidence."
             {:type :vis/empty-block :form-count 0}))))

(def ^:private free-names-py-src
  ;; Free NAMES a block reads but never binds itself — the bare `a` in
  ;; `print(a)` after a prior turn did `a = 1`. Twin of `r-keys-py-src`, but for
  ;; the model's own variables: collect Load-context Names, subtract everything
  ;; bound IN this block (Store/Del targets, function/class defs, import names,
  ;; function args, comprehension/walrus targets). The CALLER intersects the
  ;; result with the cross-turn store, so builtins / bound tools / `r` / `ctx`
  ;; (never in the store) drop out automatically — we only rebind real user vars.
  "import ast as __ast__\n__t__=__ast__.parse(__vis_src__)\n__load__=set()\n__bound__=set()\nfor __n__ in __ast__.walk(__t__):\n    if isinstance(__n__, __ast__.Name):\n        (__load__ if isinstance(__n__.ctx, __ast__.Load) else __bound__).add(__n__.id)\n    elif isinstance(__n__, (__ast__.FunctionDef, __ast__.AsyncFunctionDef, __ast__.ClassDef)):\n        __bound__.add(__n__.name)\n    elif isinstance(__n__, __ast__.arg):\n        __bound__.add(__n__.arg)\n    elif isinstance(__n__, (__ast__.Import, __ast__.ImportFrom)):\n        for __a__ in __n__.names:\n            __bound__.add((__a__.asname or __a__.name).split('.')[0])\nsorted(__load__ - __bound__)")

(defn free-names-in-block
  "Return the DISTINCT free NAMES a block reads without binding (e.g.
   `#{\"a\"}` for `print(a)`). These are the model's own cross-turn variables;
   the caller rebinds the ones present in the session store (so builtins, bound
   tools, `r`/`ctx` — absent from the store — are skipped). Pure AST walk; empty
   set on a parse error (the eval surfaces the real error)."
  [code]
  (try
    (let [^Context ctx @parser-ctx
          b (.getBindings ctx "python")]
      (.putMember b "__vis_src__" (str code))
      (let [^Value v (.eval ctx "python" free-names-py-src)
            n (long (.getArraySize v))]
        (into #{} (map #(.asString (.getArrayElement v %))) (range n))))
    (catch Throwable t
      (tel/log! {:level :debug :id ::free-names-parse-failed :data {:error (ex-message t)}}
        "free-names-in-block: block did not parse; skipping name prefetch")
      #{})))

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

(defn- ^Value python-globals [python-context]
  (.getBindings ^Context python-context "python"))

(defn set-python-binding!
  "Bind `sym` -> `val` in the Python sandbox globals. Clojure fns are wired as
   callables; everything else is marshalled.

   ASYNC-BY-DEFAULT: a tool fn bound here is also DEFERRED (wrapped by
   `__vis_deferred__`, same as `build-agent-context`'s defer step) so
   `await tool(...)` / `gather(tool(...))` work. This matters because extension
   and foundation tools are (re)installed via this fn AFTER the context's own
   defer pass — without deferring here they'd stay raw/synchronous and the
   `await` the prompt teaches would fail. The compaction verbs
   (`summarize`/`drop`/`__vis_par__`) are bound via `create-python-context`, not
   here, so they stay direct. No-op when the async preamble isn't installed
   (the printer/parser helper contexts never bind tools)."
  [python-context sym val]
  (let [^Context ctx python-context
        g  (python-globals ctx)
        nm (sym->py-name sym)]
    (.putMember g nm (if (fn? val) (wrap-ifn val) (->py val)))
    (when (fn? val)
      (try
        (.putMember g "__vis_defer1__" nm)
        (.eval ctx "python"
          (str "if '__vis_deferred__' in globals() and callable(globals().get(__vis_defer1__)):\n"
            "    globals()[__vis_defer1__] = __vis_deferred__(globals()[__vis_defer1__], __vis_defer1__)"))
        (finally (.putMember g "__vis_defer1__" nil))))))

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

(def ^:private scope-key-re
  ;; A form-result store key (`tN/iN/fN`). Distinguishes `r`-dict entries (form
  ;; results + the special "session") from bare variable names (globals).
  #"\At[1-9][0-9]*/i[1-9][0-9]*/f[1-9][0-9]*\z")

(defn- scope-key? [k]
  (or (= k "session") (boolean (re-matches scope-key-re k))))

(defn rebind!
  "Make the cross-turn variables a block needs available in `ctx` BEFORE it
   evals, so a FRESH-per-turn interpreter transparently resolves the bare names
   (`a`) the model bound in a past turn (REPL-style persistence).

   `needed`  — bare names to provide (`free-names-in-block`).
   `resolve` — `(fn [key] -> {:pickle byte[]? :src str? :deps #{key}} | nil)`, the
   loop's DB-backed store lookup; nil for builtins / bound tools (skipped — never
   in the store). Strategy per name:
     • `:pickle` present → value-rebind (handles data + whole object graphs);
     • else `:src` (a NAME whose value won't pickle — a function) → source-replay:
       bind its `:deps` first (topological), then eval the defining form so the
       function rebuilds its closure over the now-bound globals.
   Names land as globals. Returns the set of names actually backed + bound."
  [^Context ctx needed resolve]
  (let [^Value r-dict (.eval ctx "python" "globals().setdefault('r', {})")
        g     (python-globals ctx)
        ;; single-threaded (ctx is locked for the turn), so a volatile beats an
        ;; atom — no CAS, just a cheap visited-set for cycle-safety + topo order.
        bound (volatile! #{})
        ;; A LIVE global wins over the store: within a turn the interpreter
        ;; persists across iterations, and the store lags (populated per-iter),
        ;; so re-binding a name the interpreter already holds would clobber a
        ;; fresher value. Scope keys are immutable past results — never clobbered.
        live? (fn [k] (and (not (scope-key? k)) (.hasMember g k)))
        bind1 (fn bind1 [k]
                (when (and (not (contains? @bound k)) (not (live? k)))
                  (when-let [{:keys [pickle src]} (resolve k)]
                    (vswap! bound conj k)               ; mark first → cycle-safe
                    ;; nil when there's no pickle OR the pickle won't LOAD (a
                    ;; function dumps by-reference but can't reload in a fresh
                    ;; interpreter — so value-rebind quietly fails to nil here).
                    (let [v (when pickle (bytes->py ctx pickle))]
                      (cond
                        ;; value rebind worked (data / whole object graph)
                        v (if (scope-key? k)
                            (.putHashEntry r-dict k v)
                            (.putMember g k v))
                        ;; no usable value → source-replay (NAME only, a `def`):
                        ;; bind the defining form's free-name deps first, then
                        ;; eval it so the closure rebuilds over the bound globals.
                        (and src (not (scope-key? k)))
                        (do (doseq [d (free-names-in-block src)] (bind1 d))
                            (.eval ctx "python" ^String src))
                        :else nil)))))]
    (doseq [k needed] (bind1 k))
    @bound))

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
        g (python-globals python-context)]
    (let [v1 (.getMember g "_1")
          v2 (.getMember g "_2")]
      (.putMember g "_3" v2)
      (.putMember g "_2" v1)
      (.putMember g "_1" (->py value)))))

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
   `shell_bg` / `shell_logs` / `resource_stop`). INLINED here (eval'd into every
   sandbox context) so the shim ships in-jar with NO separate `.py` resource.
   Tool callables are looked up in `globals()` at CALL time, so it self-adapts:
   when the shell tool is absent or its toggle is off it raises a clear 'enable
   the shell tool' message. Soft string-level coupling to the tool NAMES only."
  "# vis sandbox POSIX-compat shim.
#
# The agent sandbox is deny-by-default (no native access), so CPython's real
# `subprocess` / `os.system` cannot spawn — they fail with an opaque error.
# This shim replaces them with a thin layer that DELEGATES to the vis shell
# TOOLS (`shell_run` / `shell_bg` / `shell_logs` / `resource_stop`), so the
# model's ordinary Python (`subprocess.run([...])`, `os.system(...)`) just works
# and still rides the same toggle gate, workspace-cwd containment, timeout,
# process-tree kill, output bounding, render badge, and trace recording.
#
# It is tool-AGNOSTIC by construction: the tool callables are looked up in
# globals() at CALL time, not bound at import. So if the shell tool is absent
# (extension not installed) or OFF (:shell/enabled toggle), the shim raises a
# clear \"enable the shell tool\" message instead of a confusing spawn failure —
# and the instant the toggle flips on, the same code starts working.
#
# Installed once per sandbox context (main + every sub_loop fork) by
# env_python/build-agent-context, right after the apropos/doc introspection.

def __vis_install_posix_compat__():
    import sys
    import types
    import shlex
    import time

    _SHELL_DISABLED = (
        \"Shell is not enabled in this vis sandbox, so subprocess / os.system \"
        \"cannot run. Ask the user to turn on the 'Shell commands' toggle \"
        \"(:shell/enabled); then subprocess.run / check_output / os.system route \"
        \"to the shell tool. Prefer shell_run(cmd) directly (returns a dict with \"
        \"exit/stdout/stderr); use shell_bg(id, cmd) for long-running commands.\"
    )

    def _shell_run():
        fn = globals().get(\"shell_run\")
        if fn is None:
            raise RuntimeError(_SHELL_DISABLED)
        # Tools are async-DEFERRED (return a thunk); this internal bridge calls
        # them synchronously, so SETTLE the thunk into its real value here.
        settle = globals().get(\"__vis_settle__\")
        return (lambda *a, **k: settle(fn(*a, **k))) if settle else fn

    def _shell_bg():
        fn = globals().get(\"shell_bg\")
        if fn is None:
            raise RuntimeError(_SHELL_DISABLED)
        settle = globals().get(\"__vis_settle__\")
        return (lambda *a, **k: settle(fn(*a, **k))) if settle else fn

    def _to_cmd(args, shell):
        # A string is taken verbatim (the `bash -lc` line). A list/tuple is
        # quoted+joined so argv-style calls run safely under the shell tool.
        if isinstance(args, (list, tuple)):
            return \" \".join(shlex.quote(str(a)) for a in args)
        return str(args)

    class CalledProcessError(Exception):
        def __init__(self, returncode, cmd, output=None, stderr=None):
            self.returncode = returncode
            self.cmd = cmd
            self.output = output
            self.stdout = output
            self.stderr = stderr
            super().__init__(
                \"Command \" + repr(cmd) + \" returned non-zero exit status \"
                + repr(returncode) + \".\"
            )

    class TimeoutExpired(Exception):
        def __init__(self, cmd, timeout, output=None, stderr=None):
            self.cmd = cmd
            self.timeout = timeout
            self.output = output
            self.stdout = output
            self.stderr = stderr
            super().__init__(
                \"Command \" + repr(cmd) + \" timed out after \"
                + str(timeout) + \" seconds\"
            )

    class CompletedProcess(object):
        def __init__(self, args, returncode, stdout, stderr):
            self.args = args
            self.returncode = returncode
            self.stdout = stdout
            self.stderr = stderr

        def __repr__(self):
            parts = [\"args=\" + repr(self.args), \"returncode=\" + repr(self.returncode)]
            if self.stdout is not None:
                parts.append(\"stdout=\" + repr(self.stdout))
            if self.stderr:
                parts.append(\"stderr=\" + repr(self.stderr))
            return \"CompletedProcess(\" + \", \".join(parts) + \")\"

        def check_returncode(self):
            if self.returncode:
                raise CalledProcessError(
                    self.returncode, self.args, self.stdout, self.stderr
                )

    def run(args, capture_output=False, text=True, shell=False, cwd=None,
            timeout=None, check=False, input=None, encoding=None, errors=None,
            env=None, stdout=None, stderr=None, stdin=None, bufsize=-1,
            universal_newlines=None, **kwargs):
        # `text`/`universal_newlines` decide bytes-vs-str on the returned
        # streams; capture_output/stdout/stderr are accepted but the shell tool
        # always captures, so they only affect whether we surface the text.
        sr = _shell_run()
        cmd = _to_cmd(args, shell)
        opts = {}
        if timeout is not None:
            opts[\"timeout_secs\"] = int(timeout)
        if cwd is not None:
            opts[\"cwd\"] = str(cwd)
        r = sr(cmd, opts) if opts else sr(cmd)
        if r.get(\"timed_out\"):
            raise TimeoutExpired(cmd, r.get(\"timeout_secs\", timeout),
                                 r.get(\"stdout\", \"\"), r.get(\"stderr\", \"\"))
        rc = r.get(\"exit\")
        out = r.get(\"stdout\", \"\")
        err = r.get(\"stderr\", \"\")
        as_text = text if universal_newlines is None else universal_newlines
        if as_text is False:
            out = out.encode(\"utf-8\", \"replace\")
            err = err.encode(\"utf-8\", \"replace\")
        cp = CompletedProcess(args, rc, out, err)
        if check:
            cp.check_returncode()
        return cp

    def call(args, **kwargs):
        kwargs.pop(\"check\", None)
        return run(args, **kwargs).returncode

    def check_call(args, **kwargs):
        kwargs.pop(\"check\", None)
        cp = run(args, **kwargs)
        cp.check_returncode()
        return 0

    def check_output(args, text=True, **kwargs):
        kwargs.pop(\"capture_output\", None)
        kwargs.pop(\"check\", None)
        cp = run(args, capture_output=True, text=text, check=True, **kwargs)
        return cp.stdout

    def getstatusoutput(cmd):
        cp = run(cmd, shell=True, capture_output=True, text=True)
        out = cp.stdout or \"\"
        if out.endswith(\"\\n\"):
            out = out[:-1]
        return (cp.returncode or 0, out)

    def getoutput(cmd):
        return getstatusoutput(cmd)[1]

    class Popen(object):
        # Background process backed by the shell_bg session-resource tool. Auto
        # picks a resource id; stop it via .terminate()/.kill() (resource_stop),
        # poll/wait via shell_logs status, communicate() drains the log buffer.
        _counter = [0]

        def __init__(self, args, shell=False, cwd=None, **kwargs):
            sb = _shell_bg()
            Popen._counter[0] += 1
            self._id = \"popen_\" + str(Popen._counter[0])
            self.args = args
            reg = sb(self._id, _to_cmd(args, shell))
            self.pid = reg.get(\"pid\")
            self.returncode = None

        def _logs(self):
            sl = globals().get(\"shell_logs\")
            if sl is None:
                raise RuntimeError(_SHELL_DISABLED)
            settle = globals().get(\"__vis_settle__\")
            return settle(sl(self._id)) if settle else sl(self._id)

        def poll(self):
            r = self._logs()
            if r.get(\"status\") == \"exited\":
                self.returncode = r.get(\"exit\")
            return self.returncode

        def wait(self, timeout=None):
            deadline = None if timeout is None else time.time() + timeout
            while self.poll() is None:
                if deadline is not None and time.time() > deadline:
                    raise TimeoutExpired(self.args, timeout)
                time.sleep(0.1)
            return self.returncode

        def communicate(self, input=None, timeout=None):
            self.wait(timeout)
            r = self._logs()
            out = \"\\n\".join(t for _, t in r.get(\"lines\", []))
            return (out, \"\")

        def terminate(self):
            rs = globals().get(\"resource_stop\")
            if rs is not None:
                settle = globals().get(\"__vis_settle__\")
                settle(rs(self._id)) if settle else rs(self._id)
            self.returncode = self.returncode if self.returncode is not None else -15

        kill = terminate

        def __enter__(self):
            return self

        def __exit__(self, *exc):
            if self.poll() is None:
                self.terminate()
            return False

    # Assemble a module object and publish it so `import subprocess` finds it.
    mod = types.ModuleType(\"subprocess\")
    mod.run = run
    mod.call = call
    mod.check_call = check_call
    mod.check_output = check_output
    mod.getoutput = getoutput
    mod.getstatusoutput = getstatusoutput
    mod.Popen = Popen
    mod.CompletedProcess = CompletedProcess
    mod.CalledProcessError = CalledProcessError
    mod.TimeoutExpired = TimeoutExpired
    mod.SubprocessError = Exception
    mod.PIPE = -1
    mod.STDOUT = -2
    mod.DEVNULL = -3
    sys.modules[\"subprocess\"] = mod

    # Redirect os.system / os.popen to the same path (they reach the live os
    # module via sys.modules, so a later `import os` sees the patched callables).
    try:
        import os as _os

        def _os_system(command):
            try:
                return run(command, shell=True).returncode or 0
            except Exception:
                return 1

        def _os_popen(command, mode=\"r\", buffering=-1):
            import io
            return io.StringIO(getoutput(command))

        _os.system = _os_system
        _os.popen = _os_popen
    except Exception:
        pass


__vis_install_posix_compat__()
del __vis_install_posix_compat__
")

(defn- install-posix-compat-shim!
  "Eval the POSIX-compat shim into `ctx`. Best-effort: a failure here just
   leaves the sandbox without the bridge (subprocess stays unavailable), it must
   never break context creation."
  [^Context ctx]
  (when-let [src posix-compat-shim-src]
    (try (.eval ctx "python" ^String src)
      (catch Throwable _ nil))))

(defonce ^:private ^java.util.Map ctx->stdout
  ;; Context -> the ByteArrayOutputStream its Python `print`/sys.stdout writes
  ;; into. `run-python-block` resets+reads it per form so a form's stdout is
  ;; surfaced to the MODEL (without it, `print(x)` returns None and the model
  ;; never sees the printed value → it re-runs or gives up — the GPT/Copilot
  ;; "kept repeating print without converging" loop). WeakHashMap so a disposed
  ;; Context's buffer is GC'd with it.
  (java.util.Collections/synchronizedMap (java.util.WeakHashMap.)))

(defn- ctx-stdout-baos ^java.io.ByteArrayOutputStream [^Context ctx]
  (.get ctx->stdout ctx))

(defn- module-value?
  "True when `v` is the GraalPy `__main__` module Value. A bare statement
   (`print(...)`, `import os`, …) eval'd as an expression yields the module
   object rather than a value; surfacing `<module '__main__'>` as a form result
   is noise, so callers scrub it to nil (and use stdout instead)."
  [v]
  (and (instance? Value v)
    (str/starts-with? (str v) "<module ")))

(defn- build-agent-context
  "Build ONE deny-by-default GraalPy agent sandbox Context ON the shared `Engine`,
   wire `custom-bindings` (tool/verb fns as Python callables, values marshalled),
   install introspection, and return `{:python-context :sandbox-ns :initial-ns-keys}`.
   Shared by `create-python-context` (the main session sandbox) and `fork-context!`
   (each `sub_loop` child) so they are byte-for-byte the same sandbox — only the
   bound env (which ctx-atom the verbs close over) differs."
  [custom-bindings]
  (let [stdout-baos (java.io.ByteArrayOutputStream.)
        ctx (-> (Context/newBuilder (into-array String ["python"]))
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
              ;; Capture Python stdout so `run-python-block` can surface a form's
              ;; printed output to the model (see `ctx->stdout`). `.out` is
              ;; independent of IOAccess (which governs the filesystem).
              (.out stdout-baos)
              (.build))
        _   (.put ctx->stdout ctx stdout-baos)
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
    ;; ASYNC-BY-DEFAULT runtime: install the trampoline + `gather`, then DEFER
    ;; every tool binding (so `await cat(x)` / `gather(cat(x), cat(y))` work).
    ;; `__vis_par__` (the host virtual-thread pool) is wired as a binding above;
    ;; the compaction verbs (`summarize`/`drop`) stay direct (never deferred).
    ;; Eval'd before the snapshot so all `__vis_*` names land in the baseline.
    (.eval ctx "python" async-runtime-python)
    (let [defer-names (->> (or custom-bindings {})
                        (filter (fn [[_ v]] (fn? v)))
                        (map (fn [[sym _]] (sym->py-name sym)))
                        (remove #{"summarize" "drop" "__vis_par__"})
                        distinct vec)]
      (.putMember g "__vis_defer_names__" (->py defer-names))
      (.eval ctx "python" "__vis_defer_tools__()"))
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
     - a line-leading ``` markdown fence - the model parroting the wire's
       rendered transcript back as its OWN reply: a real reply is ONE
       ```python fence, but a confused model re-emits the ```ctx /
       ```python fences (and the `# tool results` heading) it SEES in
       context AFTER its genuine calls, so the whole tail of fence
       markers + JSON result echoes swallows into one `python` block that
       cannot parse (session d5a81236, t4/i1: ``` ```ctx[...] `` glued
       after `git_status()`).
     - `r[\"tN/iN/fN\"] = {...}` / `= [...]` - the same parroting, one
       step deeper: the model reconstructs the results-store WRITES it
       only ever READS. `r` is the read-only prior-results dict, so an
       ASSIGNMENT to an `r[\"...\"]` subscript is never code the model
       legitimately writes.
   None of these is ever legal Python at the start of a line; a legit
   `_result = ...` or `x = git_add(...)` assignment does NOT match
   (those lines start with an identifier, not `_result{` / `=`), and
   `except SyntaxError:` / `raise SyntaxError(...)` start with their
   keyword, not the bare exception name. A ``` fence that is genuine
   markdown inside a `done(\"\"\"…\"\"\")` answer is also safe: it sits
   INSIDE a string, so the reply parses and the SyntaxError-gated repair
   path that consults this regex never runs (the model only writes a
   bare-line ``` when it has fallen out of its own program)."
  #"(?m)^[ \t]*(?:_result\s*[\{\[]|_results?\s+<|</?results\b|=\s*[A-Za-z_]\w*\s*\(|assistant#|SyntaxError:|`{3,}|r\[\s*\"[^\"\n]*\"\s*\]\s*=\s*[\[{])")

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
                     (str "You PARROTED the transcript back as code: a `_result{...}` / "
                       "`r[\"tN/iN/fN\"] = {...}` line, or a ``` fence (```ctx / ```python) "
                       "and `# tool results` heading, is the host's RENDERING of the "
                       "wire you SEE - never code you write. Your reply is ONE ```python "
                       "fence and nothing after it. Emit ONLY the calls and STOP - the "
                       "engine runs them and sends the REAL results back; never re-emit "
                       "the result echoes, never predict a result, and never continue "
                       "with calls that depend on an invented one. Original parser error: ")
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

(defn- done-form?
  "True when a split top-level form is a `done(...)` call — the turn-finishing
   verb. Matches the canonical shape the model writes (`done(\"\"\"…\"\"\")` as its
   own statement); kind is \"expr\" for a bare call."
  [{:keys [src kind]}]
  (and (= kind "expr")
    (boolean (re-find #"(?s)^\s*done\s*\(" (str src)))))

(defn cap-forms
  "Enforce the per-iteration FORM budget BEFORE execution — the hard backstop on
   one-shotting (the model trying to do a whole turn in one reply).

   `cap` = max forms to RUN, counted AFTER pulling out any `done()` (nil = no
   cap). `drop-done?` = first-iteration behavior: when a `done()` shares the
   reply with other forms, drop it (the answer was decided before any result was
   seen — finish on a later reply). A LONE `done()` (pure answer, no other forms)
   is always kept and never capped.

   Pure. Returns `{:forms <forms-to-run> :note <map|nil>}`; the note (when the
   block was trimmed) carries `{:cap :kept :total :dropped-done? :dropped-extra}`
   so the caller can disclose the trim to the model."
  [forms cap drop-done?]
  (if (or (not (vector? forms)) (empty? forms))
    {:forms forms :note nil}
    (let [done-forms (filterv done-form? forms)
          non-done   (filterv (complement done-form?) forms)]
      (if (empty? non-done)
        {:forms forms :note nil}                       ;; lone/pure-answer done() — untouched
        (let [total-non-done (count non-done)
              kept-non-done  (if (and cap (> total-non-done (long cap)))
                               (subvec non-done 0 (long cap))
                               non-done)
              dropped-extra  (- total-non-done (count kept-non-done))
              drop-the-done? (and drop-done? (seq done-forms))
              ;; first iteration drops done() entirely; later iterations keep it
              ;; (the done-gate still governs whether it finalizes).
              final          (if drop-the-done?
                               kept-non-done
                               (into kept-non-done done-forms))]
          (if (and (not drop-the-done?) (zero? dropped-extra))
            {:forms forms :note nil}                   ;; nothing trimmed
            {:forms final
             :note {:cap cap
                    :kept (count final)
                    :total (count forms)
                    :dropped-done? (boolean drop-the-done?)
                    :dropped-extra dropped-extra}}))))))

(declare run-python-block-per-form)

(defn- has-await?
  "True when the program uses `await` — it must run as ONE driven coroutine via
   `run-async-program` (GraalPy rejects top-level await at parse, so it can't be
   split per-form). A false positive (the word in a string/comment) just routes a
   no-await program through the async-wrap, which runs it correctly anyway."
  [code]
  (boolean (re-find #"\bawait\b" (str code))))

(defn- run-async-program
  "Run an await-bearing program as ONE driven coroutine. `__vis_run_async__`
   AST-wraps it in an `async def` (with `global` decls for its assigned names so
   REPL vars still persist), the trampoline `__vis_drive__` drives it, and
   `gather` overlaps awaitables on the host virtual-thread pool. Returns the SAME
   outcome contract as the per-form path: a forms vector whose first entry carries
   the program's stdout/result, plus one synthetic `:bound-name`/`:result-pickle`
   entry per assigned global (so cross-turn rebind works exactly as for sync forms)."
  [^Context ctx ^Value g code]
  (let [baos      (ctx-stdout-baos ctx)
        _         (when baos (.reset baos))
        run-async (.getMember g "__vis_run_async__")
        read-out  (fn [] (when baos (let [s (.toString baos "UTF-8")]
                                      (when-not (str/blank? s) s))))]
    (with-bindings {@current-form-idx-var 0}
      (try
        (let [namesv     (.execute run-async (object-array [code]))
              names      (mapv str (or (->clj namesv) []))
              resv       (.getMember g "__vis_async_result__")
              res0       (->clj resv)
              res        (if (module-value? res0) nil res0)
              out        (read-out)
              main-form  (cond-> {:source code}        ; de-conflated: value vs stdout
                           (some? res) (assoc :result res)
                           out (assoc :stdout out))
              name-forms (vec (for [n names
                                    :let [pv  (.getMember g n)
                                          pkl (when (and pv (not (module-value? (->clj pv))))
                                                (try (pickle->bytes ctx pv)
                                                  (catch Throwable _ nil)))]
                                    :when pkl]
                                {:bound-name n :result-pickle pkl}))]
          (.putMember g "__vis_async_result__" nil) ;; clear stash for the next turn
          {:result res :forms (into [main-form] name-forms)  ; top-level :result = value (*1)
           :error nil :auto-repaired nil :forms-capped nil})
        (catch PolyglotException e
          (let [out (read-out)
                err (map-polyglot-error e code)]
            {:result nil
             :forms  [(cond-> {:source code :error err} out (assoc :stdout out))]
             :error  err :auto-repaired nil :forms-capped nil}))))))

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
  [python-context code & [scope-prefix opts]]
  (let [ctx ^Context python-context
        g   (.getBindings ctx "python")]
   (if (has-await? code)
    ;; Async-by-default: an `await` program runs as ONE driven coroutine (it
    ;; cannot be split per-form — GraalPy rejects top-level await at parse).
    (run-async-program ctx g code)
    (run-python-block-per-form ctx g code opts))))

(defn- run-python-block-per-form
  "The synchronous per-form path of `run-python-block` (no top-level await).
   Splits the block, auto-repairs syntax, and evaluates each form in order;
   deferred tool results (bare `cat(x)` / `gather(...)`) are SETTLED per form."
  [^Context ctx ^Value g code opts]
  (let [;; No r[] form-memory: context is print-only (the model prints what it
        ;; wants to see), so per-form values are not published into a sandbox `r`
        ;; dict. Cross-turn REPL variables persist by NAME via rebind! instead.
        settle-val  (.getMember g "__vis_settle__")
        settle-bind (.getMember g "__vis_settle_binding__")
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
      (let [{run-forms :forms cap-note :note} (cap-forms forms (:form-cap opts) (boolean (:drop-done? opts)))
            baos (ctx-stdout-baos ctx)]
        (loop [todo run-forms, acc [], last-res nil]
          (if (empty? todo)
            {:result last-res :forms acc :error nil :auto-repaired repair-note :forms-capped cap-note}
            (let [{:keys [src kind name]} (first todo)
                  _ (when baos (.reset baos)) ;; isolate THIS form's stdout
                  outcome (with-bindings {@current-form-idx-var (count acc)}
                            (try
                              (let [v (.eval ctx "python" ^String src)
                                    ;; the NATIVE python value that IS this form's
                                    ;; result — grabbed HERE while it's still a live
                                    ;; python object so we can pickle it losslessly
                                    ;; for cross-turn rebind (the bound name for a
                                    ;; bare `a = 1`). ASYNC-BY-DEFAULT: tools are
                                    ;; deferred, so SETTLE a bare top-level call here
                                    ;; (`cat(x)` runs; `gather(cat(x), cat(y))`
                                    ;; overlaps on virtual threads) — nested calls
                                    ;; need an explicit `await`.
                                    pv (cond
                                         (= kind "expr")
                                         (.execute settle-val (object-array [v]))
                                         (or (= kind "assign") (= kind "def"))
                                         (.execute settle-bind (object-array [name]))
                                         :else nil)
                                    res0 (when pv (->clj pv))
                                    ;; A bare statement (print/import/…) yields
                                    ;; the __main__ module object, not a value —
                                    ;; scrub it so the model never sees
                                    ;; "<module '__main__'>".
                                    res (if (module-value? res0) nil res0)
                                    ;; Surface captured stdout to the model.
                                    ;; `print(x)` returns None/module, so without
                                    ;; this the model never sees the printed value
                                    ;; and re-runs/gives up. The printed text IS
                                    ;; the meaningful result for a print form.
                                    out (when baos
                                          (let [s (.toString baos "UTF-8")]
                                            (when-not (str/blank? s) s)))
                                    ;; Pickle the native result (proto-5) ONLY to
                                    ;; back the by-NAME cross-turn rebind: a later
                                    ;; turn's bare `a` is restored from this blob,
                                    ;; REPL-style. (No by-scope r[] memory anymore.)
                                    pkl (when-not (module-value? res0) (pickle->bytes ctx pv))]
                                ;; DE-CONFLATED: :result is the form's VALUE only;
                                ;; :stdout is the printed output only (no `or`).
                                ;; The model reads :stdout (print-only); :result
                                ;; carries the bare value / verb sentinel
                                ;; (vis_answer/vis_silent) without stdout masking it.
                                (cond-> {:source src}
                                  (some? res) (assoc :result res)
                                  out (assoc :stdout out)
                                  pkl (assoc :result-pickle pkl)
                                  (and pkl (or (= kind "assign") (= kind "def")))
                                  (assoc :bound-name name)))
                              (catch PolyglotException e
                                (let [out (when baos
                                            (let [s (.toString baos "UTF-8")]
                                              (when-not (str/blank? s) s)))]
                                  (cond-> {:source src :error (map-polyglot-error e src)}
                                    out (assoc :stdout out))))))]
              (if (:error outcome)
                {:result nil :forms (conj acc outcome) :error (:error outcome) :auto-repaired repair-note :forms-capped cap-note}
                (recur (rest todo) (conj acc outcome) (:result outcome))))))))))

;; =============================================================================
;; Engine-owned sandbox names + restore (NOOP)
;; =============================================================================

(def SYSTEM_VAR_NAMES
  "Engine-owned symbols hidden from user live-var listings."
  '#{session})

(defn system-var-sym?
  [sym]
  (contains? SYSTEM_VAR_NAMES sym))

(defn restore-sandbox!
  "NOOP. The Python sandbox is fresh per turn; cross-turn memory rides on
   `:session/facts` + the per-form blob."
  [_python-context _db-info _session-id]
  [])
