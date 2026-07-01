(ns com.blockether.vis.internal.env-python
  "Embedded-GraalPy sandbox machinery — the agent's action substrate. The agent
   writes **Python**; this ns embeds a GraalPy `org.graalvm.polyglot.Context`,
   marshals values across the Clojure↔Python boundary, wires the Clojure tool
   fns into the Python globals as `ProxyExecutable`s (so `cat(\"x\")` in Python
   runs the Clojure `cat`), and runs the model's code block as ONE whole-block
   coroutine.

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
   [clojure.set :as set]
   [clojure.string :as str]
   [com.blockether.vis.internal.parse-diagnose :as parse-diagnose]
   [com.blockether.vis.internal.sandbox-fs :as sandbox-fs]
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
   this is how the agent reaches the tools — `git_status()` calls `git/status`.

   A tiny compatibility alias layer may additionally expose selected historical
   short names (currently `exists` for `exists?`), but the snake name remains
   canonical."
  ^String [sym]
  (let [s     (str sym)
        pred? (str/ends-with? s "?")
        base  (-> s
                  (str/replace "?" "")
                  (str/replace "!" "")
                  (str/replace "/" "_")
                  (str/replace "-" "_"))]
    (if pred? (str "is_" base) base)))

(defn- py-aliases-for-sym
  "Additional Python names intentionally accepted for a Clojure tool symbol.
   Keep tiny: aliases are prompt/API compatibility, not another naming scheme."
  [sym]
  (case sym
    exists?    ["exists"]
    find_files ["find"]   ;; `find_files` is the canonical name; `find` stays as a compat alias
    []))

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
     • bare top-level `cat('x')` / `x = cat(y)` — auto-SETTLED in place
   (via inline `__vis_settle__(...)` wrapping of every top-level assign/expr)

   `await` works because run-python-block AST-wraps an await-bearing program in
   an `async def` (GraalPy rejects top-level await), declares its assigned names
   `global` so REPL vars still persist, and drives the coroutine with the
   trampoline `__vis_drive__`. `gather` dispatches its awaitables to the
   host virtual-thread pool `__vis_par__` (bound from Clojure) — real overlap on
   blocking tool I/O, no event loop. A NESTED `__vis_Call__` that leaks into
   output (forgot `await` on a call you USE) repr's a loud hint instead of
   running silently — only TOP-LEVEL bare calls auto-settle."
  "
import ast as __vis_ast__

class __vis_Call__:
    __slots__ = ('fn', 'a', 'k', 'nm', 'ran', 'res')
    def __init__(self, fn, a, k, nm='tool'):
        self.fn = fn; self.a = a; self.k = k; self.nm = nm; self.ran = False; self.res = None
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

class __vis_Already__:
    # A trivially-ready awaitable: `await __vis_Already__(v)` immediately yields
    # `v` (the `if False: yield` makes this `__await__` a generator, so the
    # object is awaitable, but it never suspends). Used to make `await` on an
    # already-resolved value a no-op that returns the value.
    __slots__ = ('v',)
    def __init__(self, v):
        self.v = v
    def __await__(self):
        if False:
            yield
        return self.v

def __vis_awaitable__(v):
    # Normalize the operand of `await` so awaiting a NON-awaitable just returns
    # it instead of raising `TypeError: object X can't be used in 'await'
    # expression`. The classic trap: `x = patch(...)` AUTO-SETTLES on assignment
    # (so `x` already holds the real ForeignList result), then `await x` blows
    # up. With this, the stray `await` is harmless — we simply don't care.
    # Real awaitables (a deferred `__vis_Call__`, a `gather` `__vis_Gather__`,
    # or anything with `__await__`) pass straight through so `await tool(...)` /
    # `await gather(...)` keep being driven by `__vis_drive__` exactly as before.
    if isinstance(v, (__vis_Call__, __vis_Gather__)):
        return v
    if hasattr(v, '__await__'):
        return v
    return __vis_Already__(v)

def __vis_exec_call__(c):
    if not c.ran:
        # Fold Python **kwargs into a TRAILING DICT positional. The host tool
        # callables are foreign ProxyExecutables that accept ONLY positional args, so
        # `c.fn(*a, **k)` would raise `__call__() got an unexpected keyword argument`.
        # vis tools already take a trailing opts dict — `find(\"x\", paths=[...])`,
        # `rg(query=\"x\")`, `struct_patch(op=\"delete\", target=\"foo\")` — so folding
        # kwargs to one dict matches their contract (all-kwargs collapses to a spec map).
        c.res = c.fn(*c.a, dict(c.k)) if c.k else c.fn(*c.a); c.ran = True
    return c.res

class __VisResult__(dict):
    # A real dict subclass = a TOOL RESULT. `isinstance(x, __VisResult__)` is the
    # robust, UNFORGEABLE origin marker: a model can only build PLAIN dicts (even
    # one with an 'op' key is a plain dict, never a __VisResult__), so capture never
    # relies on the 'op' key alone. 'op' stays a normal key (the origin, for render).
    # It IS a dict, so it's invisible to the model — json/mutation/isinstance work.
    pass

def __vis_pyify__(x):
    # Tool results cross the host boundary as ProxyHashMap/ProxyArray. GraalPy lets
    # you subscript / iterate / .get them, but isinstance(_, dict), {**_},
    # json.dumps(_), dict(_) and type(_) all see a FOREIGN object — NOT a real
    # dict — a frequent source of friction. Rebuild proxies into REAL python
    # dict/list ONCE (at settle) so the model composes on true dicts. A HOST proxy
    # carrying 'op' is a tool result → mark its type __VisResult__. Identity for
    # values already native to python (incl. an already-pyified __VisResult__).
    # Order is preserved (source is an ordered LinkedHashMap; comprehensions keep it).
    if x is None or type(x).__name__ == 'NoneType':
        return None
    if type(x) in (dict, list, str, bytes, int, float, bool) or isinstance(x, __VisResult__):
        return x
    if hasattr(x, 'keys'):
        try:
            d = {__k__: __vis_pyify__(__v__) for __k__, __v__ in x.items()}
            return __VisResult__(d) if 'op' in d else d
        except Exception:
            return x
    try:
        return [__vis_pyify__(__e__) for __e__ in x]
    except Exception:
        return x

def __vis_settle__(v):
    if isinstance(v, __vis_Call__):
        return __vis_pyify__(__vis_exec_call__(v))
    if isinstance(v, __vis_Gather__):
        return __vis_pyify__(__vis_par__([(lambda a=a: __vis_settle__(a)) for a in v.aws]))
    if hasattr(v, '__await__') or hasattr(v, 'send'):
        return __vis_pyify__(__vis_drive__(v))
    return __vis_pyify__(v)

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

class __vis_asyncio__:
    # asyncio SHIM. The real asyncio is sandbox-excluded — importing it then
    # calling `asyncio.run(coro)` spins up an event loop and trips a native
    # `socket was excluded` crash. Rather than forbid the model's habit, we route
    # `asyncio.run` / `gather` / `run_until_complete` onto OUR virtual-thread
    # driver (`__vis_drive__`) and `gather` (which yields a concurrent
    # `__vis_Gather__`). `import asyncio` is AST-rewritten to bind THIS object, so
    # `asyncio.run(main())` drives `main()`'s coroutine exactly like top-level
    # `await` does — no event loop, no socket, identical result.
    @staticmethod
    def run(coro): return __vis_drive__(coro)
    @staticmethod
    def run_until_complete(coro): return __vis_drive__(coro)
    @staticmethod
    def gather(*aws): return gather(*aws)
    @staticmethod
    def create_task(coro): return coro
    @staticmethod
    def ensure_future(coro): return coro
    @staticmethod
    def get_event_loop(): return __vis_asyncio__
    @staticmethod
    def new_event_loop(): return __vis_asyncio__
    @staticmethod
    def set_event_loop(*a, **k): return None
    @staticmethod
    def sleep(*a, **k): return None
    @staticmethod
    def iscoroutine(v): return hasattr(v, 'send') or hasattr(v, '__await__')

asyncio = __vis_asyncio__

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

def __vis_strip_protected_imports__(src):
    # Rewrite imports so the sandbox can't break AND the model's habits still
    # work:
    #   • `import asyncio` / `import asyncio as aio`  ->  `aio = __vis_asyncio__`
    #     (our shim; real asyncio + `asyncio.run` trips a NATIVE
    #     `PosixSupportLibrary$UnsupportedPosixFeatureException: socket was
    #     excluded`). The shim routes run/gather/... onto our driver.
    #   • `from asyncio import run, sleep as s`        ->  `run = __vis_asyncio__.run`
    #     ; `s = __vis_asyncio__.sleep`. A name that is ALREADY a protected
    #     builtin (gather) is dropped so the builtin keeps showing through.
    #   • `import socket` / `select` / `ssl` ...       ->  dropped (no shim; a
    #     later use is a clean NameError, not a native crash).
    #   • imports binding a protected builtin           ->  dropped (would shadow).
    # Everything else (json, re, ...) is untouched; the ORIGINAL src is returned
    # when nothing changed (line numbers / formatting preserved).
    prot = set(globals().get('__vis_protected_names__') or [])
    drop = ('socket', 'select', 'selectors', 'ssl')
    def bind(name, attr):
        val = __vis_ast__.Name(id='__vis_asyncio__', ctx=__vis_ast__.Load())
        if attr is not None:
            val = __vis_ast__.Attribute(value=val, attr=attr, ctx=__vis_ast__.Load())
        return __vis_ast__.Assign(
            targets=[__vis_ast__.Name(id=name, ctx=__vis_ast__.Store())], value=val)
    tree = __vis_ast__.parse(src)
    changed = False; newbody = []
    for node in tree.body:
        if isinstance(node, __vis_ast__.Import):
            keep = []
            for a in node.names:
                base = a.name.split('.')[0]; bound = (a.asname or a.name).split('.')[0]
                if base == 'asyncio':
                    newbody.append(bind(a.asname or 'asyncio', None)); changed = True
                elif base in drop or bound in prot:
                    changed = True
                else:
                    keep.append(a)
            if keep:
                node.names = keep; newbody.append(node)
        elif isinstance(node, __vis_ast__.ImportFrom):
            base = (node.module or '').split('.')[0]
            if base == 'asyncio':
                for a in node.names:
                    bound = a.asname or a.name
                    if bound not in prot:            # gather etc. stay the builtin
                        newbody.append(bind(bound, a.name))
                changed = True
            elif base in drop:
                changed = True
            else:
                kept = [a for a in node.names if (a.asname or a.name).split('.')[0] not in prot]
                if len(kept) != len(node.names):
                    changed = True
                    if kept:
                        node.names = kept; newbody.append(node)
                else:
                    newbody.append(node)
        else:
            newbody.append(node)
    if not changed:
        return src
    tree.body = newbody
    __vis_ast__.fix_missing_locations(tree)
    return __vis_ast__.unparse(tree)

class __vis_AwaitFix__(__vis_ast__.NodeTransformer):
    # Wrap the operand of every `await EXPR` as `await __vis_awaitable__(EXPR)`
    # so awaiting a value that is NOT a real awaitable (a tool result that
    # already settled — `x = patch(...); await x`) returns the value instead of
    # raising. Visits the WHOLE tree so a nested `await` (inside `print(...)`,
    # an arg, a comprehension) is fixed too; real awaitables are untouched.
    def visit_Await(self, node):
        self.generic_visit(node)
        node.value = __vis_ast__.Call(
            func=__vis_ast__.Name(id='__vis_awaitable__', ctx=__vis_ast__.Load()),
            args=[node.value], keywords=[])
        return node

def __vis_run_async__(src):
    g = globals()
    g['__vis_printed_results__'] = []   # per-block reset (real python list, appendable)
    g['__vis_only_results__'] = True    # cleared if the block prints anything that isn't a tool result
    tree = __vis_ast__.parse(src)
    tree = __vis_AwaitFix__().visit(tree)
    __vis_ast__.fix_missing_locations(tree)
    assigned = __vis_assigned_names__(tree.body)
    body = list(tree.body)
    # AUTO-SETTLE inline, exactly like the sync per-form path: wrap the value of
    # every TOP-LEVEL assignment / bare expression in `__vis_settle__(...)` so a
    # bare deferred tool call (`res = patch(...)`, or a lone `patch(...)`) RUNS
    # in place — later statements (and `print(res)`) then see the real value,
    # not a `__vis_Call__` thunk. settle is identity for plain values and
    # idempotent for thunks already consumed by `await`/`gather`, so wrapping is
    # always safe. Nested calls still need an explicit `await` (we only touch
    # top-level statements, matching the sync contract).
    def __vis_wrap__(v):
        return __vis_ast__.Call(
            func=__vis_ast__.Name(id='__vis_settle__', ctx=__vis_ast__.Load()),
            args=[v], keywords=[])
    for __vis_node__ in body:
        if isinstance(__vis_node__, (__vis_ast__.Assign, __vis_ast__.AnnAssign)):
            if __vis_node__.value is not None:
                __vis_node__.value = __vis_wrap__(__vis_node__.value)
        elif isinstance(__vis_node__, __vis_ast__.Expr):
            __vis_node__.value = __vis_wrap__(__vis_node__.value)

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

# ── print-capture: a printed TOOL RESULT (a dict carrying 'op', stamped by the
# host) is recorded on the side so the host can render ONE op-card per printed
# result. The model's stdout/context is UNCHANGED — we delegate to the real print;
# capture is a pure side-effect. The list is reset per block from Clojure.
__vis_printed_results__ = []
__vis_real_print__ = print
def __vis_print__(*__vis_a__, **__vis_kw__):
    # Pyify args FIRST: a printed tool-result proxy becomes a __VisResult__ (so
    # `print(await rg(...))` is captured even without an intervening assignment) and
    # prints as a clean real dict. Capture by TYPE (isinstance), NOT the 'op' key —
    # a model-built dict with 'op' is a plain dict and is correctly NOT captured.
    # Track whether the block printed ONLY tool results: cards may replace the raw
    # stdout for display ONLY then; otherwise show the full stdout (no text lost).
    __vis_a__ = tuple(__vis_pyify__(__a__) for __a__ in __vis_a__)
    if __vis_kw__.get('file') is None:
        for __vis_x__ in __vis_a__:
            if isinstance(__vis_x__, __VisResult__):
                __vis_printed_results__.append(__vis_x__)
            else:
                globals()['__vis_only_results__'] = False
        if not __vis_a__:                 # a bare print() (blank line) is not a result
            globals()['__vis_only_results__'] = False
    return __vis_real_print__(*__vis_a__, **__vis_kw__)
print = __vis_print__
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
  ;; The hazard (GraalVM 25.0.1): a STANDALONE
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
  (set-python-binding! python-context 'session data)
  ;; `session` was bound as a ProxyHashMap (read-only, NOT json-serializable). Convert
  ;; to a REAL python dict so json.dumps(session) / {**session} / mutation work — the
  ;; same fix tool results get. Top stays a PLAIN dict (never __VisResult__, even with
  ;; an 'op' key); nested proxies are pyified. ~2–5ms for a typical session (measured).
  (.eval ^Context python-context "python"
         (str "if '__vis_pyify__' in globals():\n"   ; guard: a context without the substrate keeps the proxy
              "    globals()['session'] = {__k__: __vis_pyify__(__v__) for __k__, __v__ in session.items()}")))

;; =============================================================================
;; Pickle boundary (no longer used by the loop — globals persist NATURALLY in the
;; one persistent interpreter). Kept as a utility: marshal a polyglot value to
;; raw `pickle.dumps` bytes (protocol 5, the C `_pickle` accelerator) and back.
;; A Java `byte[]` crosses into GraalPy as a SIGNED ForeignList, not `bytes`, so
;; `pickle.loads` rejects it; the latin-1 bridge ships the bytes as a 1:1
;; byte↔codepoint String and re-`encode('latin-1')` Python-side — a single
;; C-level op (no per-byte loop).
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
  ;; `print(a)`. Collect Load-context Names, subtract everything bound IN this
  ;; block (Store/Del targets, function/class defs, import names, function args,
  ;; comprehension/walrus targets).
  "import ast as __ast__\n__t__=__ast__.parse(__vis_src__)\n__load__=set()\n__bound__=set()\nfor __n__ in __ast__.walk(__t__):\n    if isinstance(__n__, __ast__.Name):\n        (__load__ if isinstance(__n__.ctx, __ast__.Load) else __bound__).add(__n__.id)\n    elif isinstance(__n__, (__ast__.FunctionDef, __ast__.AsyncFunctionDef, __ast__.ClassDef)):\n        __bound__.add(__n__.name)\n    elif isinstance(__n__, __ast__.arg):\n        __bound__.add(__n__.arg)\n    elif isinstance(__n__, (__ast__.Import, __ast__.ImportFrom)):\n        for __a__ in __n__.names:\n            __bound__.add((__a__.asname or __a__.name).split('.')[0])\nsorted(__load__ - __bound__)")

(def BANNED_DEF_HEADS
  "Python constructs refused pre-eval — belt-and-suspenders against the obvious
   sandbox-escape footguns on top of the Context restrictions."
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

(declare add-protected-names!)

(defn set-python-binding!
  "Bind `sym` -> `val` in the Python sandbox globals. Clojure fns are wired as
   callables; everything else is marshalled.

   ASYNC-BY-DEFAULT: a tool fn bound here is also DEFERRED (wrapped by
   `__vis_deferred__`, same as `build-agent-context`'s defer step) so
   `await tool(...)` / `gather(tool(...))` work. This matters because extension
   and foundation tools are (re)installed via this fn AFTER the context's own
   defer pass — without deferring here they'd stay raw/synchronous and the
   `await` the prompt teaches would fail. The compaction verbs
   (`session_fold`/`session_drop`/`__vis_par__`) are bound via `create-python-context`, not
   here, so they stay direct. No-op when the async preamble isn't installed
   (the printer/parser helper contexts never bind tools)."
  [python-context sym val]
  (let [^Context ctx python-context
        g       (python-globals ctx)
        nm      (sym->py-name sym)
        aliases (py-aliases-for-sym sym)
        member  (if (fn? val) (wrap-ifn val) (->py val))]
    (add-protected-names! g (cons nm aliases))
    (.putMember g nm member)
    (doseq [alias aliases]
      (.putMember g alias member))
    (when (fn? val)
      (try
        (doseq [defer-name (cons nm aliases)]
          (.putMember g "__vis_defer1__" defer-name)
          (.eval ctx "python"
                 (str "if '__vis_deferred__' in globals() and callable(globals().get(__vis_defer1__)):\n"
                      "    globals()[__vis_defer1__] = __vis_deferred__(globals()[__vis_defer1__], __vis_defer1__)")))
        (finally (.putMember g "__vis_defer1__" nil))))))

(def ^:private protected-baseline-names
  "Python globals the agent may CALL but must not rebind. Rebinding a tool name
   (for example `patch = ...`) shadows the callable in the persistent sandbox;
   the next `patch(...)` then fails as `'str' object is not callable`."
  #{"apropos" "doc" "gather"})

(defn- protected-names-for-bindings
  [custom-bindings]
  (set (concat protected-baseline-names
               (mapcat (fn [[sym _]] (cons (sym->py-name sym) (py-aliases-for-sym sym)))
                       (or custom-bindings {})))))

(defn- install-protected-names!
  [^Value g custom-bindings]
  (.putMember g "__vis_protected_names__"
              (->py (vec (sort (protected-names-for-bindings custom-bindings))))))

(defn- add-protected-names!
  [^Value g names]
  (let [existing (set (map str (or (->clj (.getMember g "__vis_protected_names__")) [])))
        names'   (set (map str names))]
    (.putMember g "__vis_protected_names__" (->py (vec (sort (set/union existing names')))))))

(defn remove-python-binding!
  "Remove `sym` from the Python sandbox globals ENTIRELY — the member key
   disappears, so `apropos`/`dir` no longer list it and calling it raises
   a plain NameError. This is how a deactivated tool must vanish:
   `putMember nil` only parks a None under the name, which `apropos`
   still lists and which calls as 'NoneType is not callable'."
  [python-context sym]
  (try
    (let [g (python-globals python-context)]
      (.removeMember g (sym->py-name sym))
      (doseq [alias (py-aliases-for-sym sym)]
        (.removeMember g alias)))
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

(def ^:private auto-imports-python
  "Install tiny convenience imports into Python builtins so agents can use them
   without repeating imports in every run_python block."
  "import builtins as __vis_builtins__
import json as __vis_json__
import shlex as __vis_shlex__
import re as __vis_re__
import hashlib as __vis_hashlib__
import glob as __vis_glob__
import os as __vis_os__
import sys as __vis_sys__
import collections as __vis_collections__
import pathlib as __vis_pathlib__
import textwrap as __vis_textwrap__
import base64 as __vis_base64__
__vis_builtins__.json = __vis_json__
__vis_builtins__.shlex = __vis_shlex__
__vis_builtins__.re = __vis_re__
__vis_builtins__.hashlib = __vis_hashlib__
__vis_builtins__.glob = __vis_glob__
__vis_builtins__.os = __vis_os__
__vis_builtins__.sys = __vis_sys__
__vis_builtins__.collections = __vis_collections__
__vis_builtins__.pathlib = __vis_pathlib__
__vis_builtins__.Path = __vis_pathlib__.Path
__vis_builtins__.textwrap = __vis_textwrap__
__vis_builtins__.base64 = __vis_base64__
del __vis_builtins__, __vis_json__, __vis_shlex__, __vis_re__, __vis_hashlib__, __vis_glob__, __vis_os__, __vis_sys__, __vis_collections__, __vis_pathlib__, __vis_textwrap__, __vis_base64__
")

(defn- install-auto-imports!
  "Make selected stdlib modules available as builtin names in every sandbox.
   Keep this list deliberately tiny: only modules that are safe, pure, and
   repeatedly useful in agent glue code belong here."
  [^Context ctx]
  (try (.eval ctx "python" ^String auto-imports-python)
       (catch Throwable _ nil)))

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

(defn- baos->str
  "UTF-8 view of a captured-stdout BAOS with CRLF normalized to LF, so a form's
   printed output is byte-identical on every OS — GraalPy honors the host
   `os.linesep` when writing text, emitting CRLF on Windows."
  ^String [^java.io.ByteArrayOutputStream baos]
  (.replace (.toString baos "UTF-8") "\r\n" "\n"))

(defn- module-value?
  "True when `v` is the GraalPy `__main__` module Value. A bare statement
   (`print(...)`, `import os`, …) eval'd as an expression yields the module
   object rather than a value; surfacing `<module '__main__'>` as a form result
   is noise, so callers scrub it to nil (and use stdout instead)."
  [v]
  (and (instance? Value v)
       (str/starts-with? (str v) "<module ")))

(def ^:private default-denied-domains
  "Hosts ALWAYS blocked when the sandbox has network — even under an `*` allowlist.
   Cloud-metadata endpoints are the classic SSRF target (credentials / instance
   identity), so they are denied by default; config `:network/denied-domains`
   ADDS to this set, never removes from it. The metadata IP is enforced at the
   `connect()` level too (see `network-guard-python`) so a raw-IP socket can't
   sidestep DNS to reach it."
  ["169.254.169.254" "metadata.google.internal" "metadata.goog" "metadata"])

(def ^:private network-guard-python
  "Best-effort allow/deny host policy for the sandbox's network.

   ⚠ THREAT MODEL — this is a GUARDRAIL for COOPERATIVE / accidental egress, NOT a
   tamper-proof boundary. It patches Python's `socket` in the SAME interpreter the
   model controls, so a determined model can defeat it (e.g. `importlib.reload
   (socket)`, or the low-level `_socket`). The ONLY hard network control is the
   binary capability `allowHostSocketAccess` (the `:network/enabled` toggle): off ⇒
   no sockets at all. Use the allow/deny lists to steer normal code, not to contain
   an adversary. (Filesystem confinement, by contrast, IS enforced below Python at
   the Truffle FileSystem layer and cannot be patched away — see `sandbox-fs`.)

   Enforcement points (so a raw-IP `connect` can't skip DNS to reach a denied IP):
     - DNS:    `getaddrinfo`, `gethostbyname`
     - connect: `socket.connect`, `socket.connect_ex`, `create_connection`
   Each checks the host/IP against the policy and raises PermissionError first.

   Policy is SPECIFICITY-based, so both lists can use `*`:
     - a SPECIFIC (non-`*`) match wins over a `*` wildcard in the OTHER list:
         denied=[`*`], allowed=[a]   ⇒ deny everything EXCEPT a
         allowed=[`*`], denied=[a]   ⇒ allow everything EXCEPT a
     - a host matching a specific entry on BOTH lists is denied (fail safe);
     - no specific match: `*` in denied blocks; else empty/`*` allowlist allows;
       else (allowlist has entries, host matched none) blocks."
  (str
   "def __vis_install_net_guard__():\n"
   "    import socket as _s\n"
   "    def _norm(x):\n"
   "        return str(x).strip().lower().rstrip('.').lstrip('.')\n"
   "    _allowed = set(_norm(d) for d in __vis_allowed_domains__ if _norm(d))\n"
   "    _denied  = set(_norm(d) for d in __vis_denied_domains__ if _norm(d))\n"
   "    _allow_specific = set(d for d in _allowed if d != '*')\n"
   "    _deny_specific  = set(d for d in _denied if d != '*')\n"
   "    _allow_star = ('*' in _allowed) or (len(_allowed) == 0)\n"
   "    _deny_star  = ('*' in _denied)\n"
   "    def _match(h, pats):\n"
   "        return any(h == d or h.endswith('.' + d) for d in pats)\n"
   "    def _host_ok(host):\n"
   "        h = _norm(host)\n"
   "        if _match(h, _deny_specific):\n"
   "            return False\n"          ;; specific deny always wins (incl. both-specific)
   "        if _match(h, _allow_specific):\n"
   "            return True\n"           ;; specific allow beats a '*' in the denylist
   "        if _deny_star:\n"
   "            return False\n"          ;; deny=* with no specific allow ⇒ block the rest
   "        return _allow_star\n"        ;; empty/'*' allowlist ⇒ allow; else block
   "    def _check(host):\n"
   "        if not _host_ok(host):\n"
   "            raise PermissionError(\"vis: network host '%s' is blocked (allowlist=%s, denylist=%s)\" % (host, sorted(_allowed) or ['*'], sorted(_denied)))\n"
   "    def _addr_host(address):\n"
   "        if isinstance(address, (tuple, list)) and address and isinstance(address[0], str):\n"
   "            return address[0]\n"          ;; AF_INET/AF_INET6 (host, port, ...); AF_UNIX/str skipped
   "        return None\n"
   "    def _wrap_dns(orig):\n"
   "        def g(host, *a, **k):\n"
   "            _check(host); return orig(host, *a, **k)\n"
   "        return g\n"
   "    _s.getaddrinfo = _wrap_dns(_s.getaddrinfo)\n"
   "    _s.gethostbyname = _wrap_dns(_s.gethostbyname)\n"
   "    def _wrap_conn(orig):\n"
   "        def g(self, address, *a, **k):\n"
   "            h = _addr_host(address)\n"
   "            if h is not None: _check(h)\n"
   "            return orig(self, address, *a, **k)\n"
   "        return g\n"
   "    try:\n"
   "        _s.socket.connect = _wrap_conn(_s.socket.connect)\n"
   "        _s.socket.connect_ex = _wrap_conn(_s.socket.connect_ex)\n"
   "    except Exception:\n"
   "        pass\n"
   "    def _wrap_create(orig):\n"
   "        def g(address, *a, **k):\n"
   "            h = _addr_host(address)\n"
   "            if h is not None: _check(h)\n"
   "            return orig(address, *a, **k)\n"
   "        return g\n"
   "    try:\n"
   "        _s.create_connection = _wrap_create(_s.create_connection)\n"
   "    except Exception:\n"
   "        pass\n"
   "__vis_install_net_guard__()\n"))

(defn- build-agent-context
  "Build ONE deny-by-default GraalPy agent sandbox Context ON the shared `Engine`,
   wire `custom-bindings` (tool/verb fns as Python callables, values marshalled),
   install introspection, and return `{:python-context :sandbox-ns :initial-ns-keys}`.
   Shared by `create-python-context` (the main session sandbox) and `fork-context!`
   (each `sub_loop` child) so they are byte-for-byte the same sandbox — only the
   bound env (which ctx-atom the verbs close over) differs."
  [custom-bindings roots-fn network-opts]
  (let [stdout-baos (java.io.ByteArrayOutputStream.)
        net?       (boolean (:enabled? network-opts))
        allowed    (vec (:allowed-domains network-opts))
        denied     (into default-denied-domains (:denied-domains network-opts))
        ;; `*` (or an empty allowlist) ⇒ allow everything EXCEPT the denylist.
        allow-all? (or (empty? allowed) (some #(= "*" (str %)) allowed))
        ;; Install the guard whenever there is an actual restriction to enforce —
        ;; a denylist (always present: defaults) or a non-`*` allowlist. With net
        ;; off the socket capability is denied outright, so no guard is needed.
        guard?     (and net? (or (seq denied) (not allow-all?)))
        ;; Filesystem capability: when `roots-fn` is supplied, the sandbox gets
        ;; REAL filesystem access CONFINED to the current context roots (Python
        ;; `open()` etc. work, but only under a root — see `sandbox-fs`). Without
        ;; it (tests / no workspace) the sandbox stays IO-NONE; the file tools do
        ;; the I/O on the Clojure side regardless.
        ;; NETWORK capability: OFF by default. When the `:network/enabled` toggle is
        ;; on, host sockets are allowed (urllib/requests/socket work); a non-empty
        ;; `:network/allowed-domains` allowlist further confines connections (guard
        ;; installed below). Empty allowlist + enabled = unrestricted network.
        io-access (if (or roots-fn net?)
                    (-> (IOAccess/newBuilder)
                        (cond-> roots-fn (.fileSystem (sandbox-fs/confined-filesystem roots-fn)))
                        (.allowHostSocketAccess net?)
                        (.build))
                    IOAccess/NONE)
        ctx (-> (Context/newBuilder (into-array String ["python"]))
              ;; Build on the shared Engine — THE thing that makes concurrent
              ;; child forks safe (see `shared-engine`).
                (.engine ^Engine @shared-engine)
              ;; deny-by-default for the DANGEROUS capabilities — no host access,
              ;; native off. Filesystem is `io-access` above (confined to roots, or
              ;; NONE). THREADS are allowed, though: the
              ;; model's Python legitimately spins them up (importlib's import
              ;; machinery, `threading`, libs that allocate locks via `_thread`),
              ;; and denying it surfaced an opaque `SecurityException: Operation
              ;; is not allowed for:` mid-run. Guest threads share the context
              ;; (GraalPy is GIL-like) and can't reach IO/native/host, so this is
              ;; a cheap capability, not a sandbox hole.
                (.allowAllAccess false)
                (.allowIO io-access)
                (.allowCreateThread true)
                (.allowNativeAccess false)
                (.allowPolyglotAccess PolyglotAccess/NONE)
              ;; Capture Python stdout so `run-python-block` can surface a form's
              ;; printed output to the model (see `ctx->stdout`). `.out` is
              ;; independent of IOAccess (which governs the filesystem).
                (.out stdout-baos)
                (.build))
        _   (.put ctx->stdout ctx stdout-baos)
        g   (.getBindings ctx "python")]
    ;; Tiny stdlib conveniences as Python builtins (not globals):
    ;; `json.dumps(...)` and `shlex.quote(...)` work in every run_python
    ;; block without repeated imports.
    (install-auto-imports! ctx)
    ;; REPL recovery slots first (so they land in the baseline and get filtered
    ;; out of the model-visible live-vars view).
    (doseq [s ["_1" "_2" "_3" "_e"]] (.putMember g s nil))
    ;; Tool fns + engine values (names snake-ified to Python-legal identifiers).
    (install-protected-names! g custom-bindings)
    (doseq [[sym val] (or custom-bindings {})]
      (let [member (if (fn? val) (wrap-ifn val) (->py val))]
        (.putMember g (sym->py-name sym) member)
        (doseq [alias (py-aliases-for-sym sym)]
          (.putMember g alias member))))
    ;; Sandbox self-discovery (apropos / doc) over the wired globals.
    (install-introspection! ctx)
    ;; POSIX-compat: route subprocess / os.system to the shell tools. Eval'd
    ;; BEFORE the initial-ns-keys snapshot so any names it parks are baseline
    ;; (filtered out of the model-visible live-vars view).
    (install-posix-compat-shim! ctx)
    ;; ASYNC-BY-DEFAULT runtime: install the trampoline + `gather`, then DEFER
    ;; every tool binding (so `await cat(x)` / `gather(cat(x), cat(y))` work).
    ;; `__vis_par__` (the host virtual-thread pool) is wired as a binding above;
    ;; the compaction verbs (`session_fold`/`session_drop`) stay direct (never deferred).
    ;; Eval'd before the snapshot so all `__vis_*` names land in the baseline.
    (.eval ctx "python" async-runtime-python)
    ;; Auto-import `re` so the model can use regex without writing `import re`.
    ;; Eval'd before the snapshot so `re` is a BASELINE name (not surfaced as a
    ;; model-created live var). The model may still `import re` — re isn't
    ;; protected, so the redundant import is a harmless no-op.
    (.eval ctx "python" "import re")
    ;; NETWORK domain allowlist: when sockets are on AND domains are specified,
    ;; patch socket DNS resolution to refuse hosts outside the allowlist. Eval'd
    ;; before the snapshot so the guard's names are BASELINE (not model-visible).
    (when guard?
      (.putMember g "__vis_allowed_domains__" (->py allowed))
      (.putMember g "__vis_denied_domains__" (->py (vec denied)))
      (.eval ctx "python" network-guard-python))
    (let [defer-names (->> (or custom-bindings {})
                           (filter (fn [[_ v]] (fn? v)))
                           (mapcat (fn [[sym _]] (cons (sym->py-name sym) (py-aliases-for-sym sym))))
                           (remove #{"session_fold" "session_drop" "__vis_par__" "__vis_par_isolated__"})
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
      :initial-ns-keys  #{...baseline globals...}}

   `roots-fn` (optional) — a 0-arg fn returning the current allowed root path
   strings; when supplied the sandbox gets REAL filesystem access confined to
   them. Omitted ⇒ no Python filesystem (IO-NONE)."
  ([custom-bindings] (create-python-context custom-bindings nil nil))
  ([custom-bindings roots-fn] (create-python-context custom-bindings roots-fn nil))
  ([custom-bindings roots-fn network-opts]
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
   (build-agent-context custom-bindings roots-fn network-opts)))

(defn fork-context!
  "Fork a CHILD agent Context for a `sub_loop` — same deny-by-default sandbox as
   the main context, built ON the shared `Engine` so it is SAFE to create even
   while the parent's eval is running (GraalVM-verified: no Truffle deadlock).
   `custom-bindings` wires the child's tool/verb fns, which close over the CHILD's
   env (its own ctx-atom). Returns the same
   `{:python-context :sandbox-ns :initial-ns-keys}` shape as
   `create-python-context`. The caller owns the child Context's lifecycle (close
   it when the sub_loop ends). `roots-fn` (optional) confines the child's Python
   filesystem to the current context roots, same as the parent."
  ([custom-bindings] (fork-context! custom-bindings nil nil))
  ([custom-bindings roots-fn] (fork-context! custom-bindings roots-fn nil))
  ([custom-bindings roots-fn network-opts] (build-agent-context custom-bindings roots-fn network-opts)))

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
           "comments above the code; the reply must START "
           "with runnable Python. Original parser error: "))))

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
   reply (see `prose-leading-syntax-hint`, first-line only), and - via
   `parse-diagnose` - an unbalanced double-quote or an unbalanced (), [], {}
   bracket pinpointed to its line/col."
  [^PolyglotException e code]
  (let [host?      (.isHostException e)
        cause      (when host? (.asHostException e))
        loc        (.getSourceLocation e)
        syntax?    (and (not host?) (.isSyntaxError e))
        base       (or (when cause (or (ex-message cause) (.getMessage cause)))
                       (.getMessage e))
        ;; Prose-leading is the ROOT cause when the reply OPENS with prose (a `x`
        ;; in a leading sentence must be reported as PROSE, not "avoid x").
        ;; So check it FIRST. Non-ascii is the
        ;; fallback for a genuinely-code reply with a stray non-ASCII char mid-line
        ;; (CPython's "invalid character", precise wherever it lands - the
        ;; em-dash-at-line-71 case the first-line-only prose detector misses).
        prose-hint (when syntax? (prose-leading-syntax-hint code))
        non-ascii? (boolean (and syntax? (not prose-hint) base (re-find #"invalid character" base)))
        ;; parse-diagnose heuristics, only when none of the structural detectors
        ;; above already explained the failure. Quote-balance first (an open
        ;; string makes the reader treat brackets as bare tokens, so its diagnosis
        ;; supersedes a bracket count), then bracket-balance.
        quote-hint   (when (and syntax? (not prose-hint) (not non-ascii?))
                       (:hint (parse-diagnose/diagnose-quote-balance code)))
        bracket-diag (when (and syntax? (not prose-hint) (not non-ascii?)
                                (not quote-hint))
                       (parse-diagnose/diagnose-bracket-balance code))
        bracket-hint (when bracket-diag
                       (str (:hint bracket-diag)
                            (when *auto-repair-brackets?*
                              (when-let [fix (parse-diagnose/repair-bracket-balance code)]
                                (str " Suggested fix: " (:change fix) ".")))))
        ;; Runtime `NameError: name 'X' is not defined`. The #1 cause of an
        ;; undefined TOOL name is an extension toggled OFF — the engine REMOVES
        ;; its symbols when inactive, so the call raises a plain NameError with
        ;; no hint that the tool merely needs enabling (e.g. a call to shell_run
        ;; while :shell/enabled is off only yields "shell_run is not defined").
        ;; Point it at apropos + the user instead
        ;; of letting it retry a name that will never resolve on its own.
        undefined-name (when (and (not host?) (not syntax?) base)
                         (second (re-find #"name '([^']+)' is not defined" (str base))))
        ;; Sandbox capability DENIAL — the model reached for the real filesystem /
        ;; native / OS (importlib.exec_module on a project file, open(), socket,
        ;; subprocess). GraalVM raises an OPAQUE `SecurityException: Operation is
        ;; not allowed for:` / `… was excluded`. Steer it to the tools that DO work.
        sandbox-denied? (boolean (and base (re-find #"Operation is not allowed for|Operation not permitted|PermissionError|was excluded|UnsupportedPosixFeature" (str base))))
        ;; A wrong dict-method call on a tool result — the model must use
        ;; bracket/index access instead. Two spellings: a still-FOREIGN/polyglot
        ;; object ("foreign object has no attribute 'get'"), OR — once a top-level
        ;; result has been pyified to a REAL python list/dict — the NATIVE
        ;; "'list' object has no attribute 'get'". Same steer either way.
        foreign-attr (when (and (not host?) base)
                       (or (second (re-find #"foreign object has no attribute '([^']+)'" (str base)))
                           (second (re-find #"'(?:list|tuple|str|int|float|bool|NoneType|set)' object has no attribute '(get|items|keys|values)'" (str base)))))
        ;; Python indentation slip (a block not indented, or a stray indent).
        indent? (boolean (and base (re-find #"IndentationError|unexpected indent|expected an indented block" (str base))))
        hint       (cond
                     prose-hint prose-hint
                     non-ascii?
                     (str "A non-ASCII character leaked into CODE position - it is only "
                          "legal inside a \"...\" string or a `#` comment. This is almost always "
                          "a smart em-dash, en-dash, curly quote, or x that you "
                          "meant as prose. Replace it with plain ASCII, or move that whole line "
                          "into a `#` comment. Original parser error: ")
                     quote-hint   (str quote-hint " Original parser error: ")
                     bracket-hint (str bracket-hint " Original parser error: ")
                     undefined-name
                     (str "`" undefined-name "` is not defined. If it's a TOOL you expected, it is "
                          "likely an extension toggled OFF — its symbols are removed while disabled "
                          "(e.g. `shell_run`/`shell_bg` need the `:shell/enabled` toggle). Run "
                          "`apropos(\"" undefined-name "\")`; if it isn't listed, ask the USER to enable "
                          "it and do NOT retry the name. If it's a variable, define it first. "
                          "Original error: ")
                     foreign-attr
                     (str "`." foreign-attr "` failed because that value is a FOREIGN/polyglot "
                          "object (a tool result), not a native Python dict — dict methods like "
                          ".get / .items / .keys may be absent. If it's a dict, read it with bracket "
                          "access (result[\"key\"]); if it's a list, index it (result[0]); the result's "
                          "shape is in the tool's docstring. Original error: ")
                     indent?
                     (str "Python is INDENTATION-sensitive: a block (after def / if / for / with / "
                          "a trailing `:`) must be indented consistently (4 spaces), and a top-level "
                          "statement must start at column 0. Re-indent that region. Original error: ")
                     sandbox-denied?
                     (str "Your sandbox has NO real filesystem / native / process access — "
                          "importlib + exec_module on a project file, open(), subprocess, and sockets "
                          "CANNOT run here. To READ a project file use cat(path); to RUN project code "
                          "(import its modules, use its deps) use repl_eval(language, code) — that runs "
                          "in the project's interpreter where the file is importable. Original error: "))
        msg        (if hint (str hint base) base)]
    {:message msg
     :data (cond-> {:phase (cond host?   :python/host
                                 syntax? :python/syntax
                                 :else   :python/runtime)}
             (some? loc) (assoc :line (.getStartLine loc)
                                :column (.getStartColumn loc))
             non-ascii?   (assoc :non-ascii-in-code? true)
             prose-hint   (assoc :prose-leading? true)
             quote-hint   (assoc :unbalanced-quote? true)
             bracket-diag (assoc :unbalanced-bracket? true)
             undefined-name (assoc :name-undefined? true :undefined-name undefined-name)
             ;; ex-data from a Clojure tool's ex-info rides through so e.g.
             ;; :tool/banned, :vis/* keep their type for the trailer — minus
             ;; host noise (nested envelope / Java trace, see sanitize-cause-data).
             (and cause (instance? clojure.lang.IExceptionInfo cause))
             (merge (sanitize-cause-data (ex-data cause) base)))}))

(def ^:private current-form-idx-var
  "Lazily resolved `extension/*current-form-idx*` dynamic var. Resolved via
   `requiring-resolve` (not a ns `:require`) so this sandbox ns stays free of
   a dependency edge on the extension registry. Bound (to 0) while the block
   runs so `record-render-entry!` can stamp `:form-idx` on every channel sink
   entry."
  (delay (requiring-resolve 'com.blockether.vis.internal.extension/*current-form-idx*)))

(defn- run-async-program
  "Run the program as ONE driven coroutine. `__vis_run_async__` AST-wraps it in
   an `async def` (with `global` decls for its assigned names so they persist in
   the interpreter), the trampoline `__vis_drive__` drives it, and `gather`
   overlaps awaitables on the host virtual-thread pool. Returns the FLAT sum
   `{:stdout <printed>}` | `{:result <value>}` | `{:error <raised> :stdout?}`."
  [^Context ctx ^Value g code]
  (let [baos      (ctx-stdout-baos ctx)
        _         (when baos (.reset baos))
        ;; (The per-block print-capture list is reset INSIDE `__vis_run_async__` as
        ;; a real python list — resetting it from here with `->py []` would make it
        ;; a non-appendable ProxyArray and lose every capture.)
        run-async (.getMember g "__vis_run_async__")
        read-out  (fn [] (when baos (let [s (baos->str baos)]
                                      (when-not (str/blank? s) s))))
        ;; The tool-result objects the model print()ed this block — each a map
        ;; carrying "op" (its origin). The HOST renders one op-card per result;
        ;; stdout (context) is untouched.
        read-printed (fn [] (let [p (->clj (.getMember g "__vis_printed_results__"))]
                              (when (seq p) (vec p))))]
    (with-bindings {@current-form-idx-var 0}
      (try
        ;; Run the whole-block coroutine; it stashes the program's value in
        ;; `__vis_async_result__` and prints to `baos`. (Globals it assigns
        ;; persist NATURALLY in the live interpreter — no pickle, no rebind.)
        (.execute run-async (object-array [code]))
        (let [res0 (->clj (.getMember g "__vis_async_result__"))
              res  (if (module-value? res0) nil res0)
              out  (read-out)
              printed (read-printed)
              ;; true ⇔ the block printed NOTHING but tool results — only then may
              ;; the human display replace the raw stdout with cards (no text lost).
              only?   (true? (->clj (.getMember g "__vis_only_results__")))]
          (.putMember g "__vis_async_result__" nil) ;; clear stash for the next turn
          ;; FLAT sum type — success is ONE CONTEXT channel, never both:
          ;;   - printed output (`:stdout`) → the python_execution result; OR
          ;;   - the returned value (`:result`) → a native tool call (never prints).
          ;; Printed output WINS. `:printed-results` rides ALONGSIDE `:stdout` —
          ;; it is DISPLAY-only (cards), NOT a second context channel.
          (if out
            (cond-> {:stdout out}
              printed             (assoc :printed-results printed)
              (and printed only?) (assoc :only-printed-results? true))
            (cond-> {} (some? res) (assoc :result res))))
        (catch PolyglotException e
          ;; FLAT sum type — failure branch. The raised error IS the result, in
          ;; ONE place; any partial stdout before it rides along.
          (let [out (read-out)]
            (cond-> {:error (map-polyglot-error e code)}
              out (assoc :stdout out))))))))

(declare protected-rebind-error)

(defn- strip-protected-imports
  "AST-strip imports of protected sandbox builtins (e.g. `from asyncio import
   gather`) from `code` via the `__vis_strip_protected_imports__` preamble
   helper, so a redundant import of a builtin becomes a no-op instead of a
   protected-rebind error (and never shadows the builtin). Returns the original
   code unchanged when nothing is stripped or on any failure."
  [^Context ctx ^Value g code]
  (try
    (.putMember g "__vis_src__" (str code))
    (let [v (.eval ctx "python" "__vis_strip_protected_imports__(__vis_src__)")]
      (if (and v (.isString v)) (.asString v) (str code)))
    (catch Throwable _ (str code))))

(defn run-python-block
  "Evaluate one Python `code` block in `python-context` as ONE WHOLE-BLOCK
   coroutine, returning the FLAT sum-typed outcome:

     {:stdout <printed>}   ; SUCCESS — python_execution (what it print()ed)
     {:result <value>}     ; SUCCESS — a native tool value (nothing printed)
     {:error  <op-error>}  ; FAILURE — the raised error IS the result

   `__vis_run_async__` AST-wraps the block in an `async def`, AUTO-SETTLES every
   bare top-level tool call (so `cat(x)` without `await` still runs), drives it
   as a single coroutine, and maps any raised exception against the WHOLE source.
   The program runs exactly as the model wrote it — Python's own
   halt-on-exception decides what ran. A pre-eval protected-rebind violation
   short-circuits to an `:error` instead."
  [python-context code & [_opts]]
  (let [ctx ^Context python-context
        g   (.getBindings ctx "python")
        ;; Strip redundant imports of protected builtins (e.g. `from asyncio
        ;; import gather`) at the AST level BEFORE the protected-rebind check and
        ;; before running — so they're a silent no-op, not an error.
        code (strip-protected-imports ctx g code)]
    (if-let [err (protected-rebind-error ctx g code)]
      {:result nil :forms [{:source code :error err}] :error err}
      ;; ONE whole-block path. `__vis_run_async__` AST-wraps the program in an
      ;; `async def`, AUTO-SETTLES every bare top-level tool call (so `cat(x)`
      ;; without `await` still RUNS), drives it as a single coroutine, and reports
      ;; any error against the WHOLE source. The block runs as the model wrote it,
      ;; so its outcome is the flat `{:stdout}` | `{:result}` | `{:error}` sum.
      (run-async-program ctx g code))))

(defn- assigned-names-in-code
  "Top-level names a Python block binds (assign/import/def/for/with targets).
   Empty on parse failure; the normal evaluator reports the syntax error."
  [^Context ctx ^Value g code]
  (try
    (.putMember g "__vis_src__" (str code))
    (let [v (.eval ctx "python"
                   "__vis_assigned_names__(__import__('ast').parse(__vis_src__).body)")]
      (set (map str (or (->clj v) []))))
    (catch PolyglotException _ #{})
    (catch Throwable _ #{})))

(defn- protected-rebind-error
  [^Context ctx ^Value g code]
  (let [protected (set (map str (or (->clj (.getMember g "__vis_protected_names__")) [])))
        hits      (sort (set/intersection protected (assigned-names-in-code ctx g code)))]
    (when (seq hits)
      {:message (str "Block tries to rebind protected sandbox/tool name(s): "
                     (str/join ", " hits)
                     ". Tool names are read-only in run_python; choose a different variable name. "
                     "This prevents shadowing a callable (e.g. patch) with data and later seeing "
                     "'str' object is not callable.")
       :data {:phase :python/protected-name
              :protected-name? true
              :names hits}})))

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
  "NOOP. The session has ONE persistent interpreter; globals (defs/imports/vars)
   persist NATURALLY across turns, so there is nothing to restore."
  [_python-context _db-info _session-id]
  [])
