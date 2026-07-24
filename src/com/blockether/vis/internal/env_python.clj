(ns com.blockether.vis.internal.env-python
  "Embedded-GraalPy sandbox machinery — the agent's action substrate. The agent
   writes **Python**; this ns embeds a GraalPy `org.graalvm.polyglot.Context`,
   marshals values across the Clojure↔Python boundary, wires the Clojure tool
   fns into the Python globals as `ProxyExecutable`s (so `cat(\"x\")` in Python
   runs the Clojure `cat`), and runs the model's code block as ONE whole-block
   coroutine.

   Public surface used by the loop:

     create-python-context / set-python-binding! / bind-and-bump! /
     bind-and-bump-with-doc! / count-top-level-forms / validate-non-empty-block! /
     validate-no-banned-defs! / restore-sandbox! / SYSTEM_VAR_NAMES /
     system-var-sym? / *lru-atom* / *current-turn-position* / fresh-lru-atom /
     run-python-block / map-polyglot-error / bind-ctx! / ctx->python-str

   The `:python-context` slot holds the GraalPy `Context`; the Python top scope is
   `context.getBindings(\"python\")`. GraalPy ships in the default deps (runs on
   Oracle GraalVM 25 → Truffle gets the Graal JIT)."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.parse-diagnose :as parse-diagnose]
            [com.blockether.vis.internal.sandbox-fs :as sandbox-fs]
            [flatland.ordered.map :as omap]
            [taoensso.telemere :as tel])
  (:import [org.graalvm.polyglot Context Context$Builder Engine Value PolyglotAccess
            PolyglotException]
           [org.graalvm.polyglot.io IOAccess]
           [org.graalvm.polyglot.proxy ProxyExecutable ProxyArray ProxyHashMap]
           [java.util ArrayList LinkedHashMap]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Marshalling  Clojure  <->  Python (polyglot Value)
;; =============================================================================

(declare ->py)

(defn boundary-violation!
  "Throw on a keyword/symbol trying to cross the Clojure->Python boundary.
   The boundary is STRINGS-ONLY: every map that crosses (tool results, ctx,
   verb payloads) is built with string keys and string enum values at the
   SOURCE — there is no silent keyword->string conversion, so a keyword here
   means a producer bug, not data. `path` is the key path down from the value
   handed to `->py`, so the offending producer field is nameable."
  [kind x path]
  (throw (ex-info (str "STRINGS-ONLY boundary violation: "
                       (name kind)
                       " "
                       (pr-str x)
                       (when (seq path) (str " at path " (pr-str (vec path))))
                       " cannot cross Clojure->Python. Build boundary maps with"
                       " string keys and stringify enum values at the source.")
                  {:vis/boundary-violation kind :value x :path (vec path)})))

(defn normalize-dict-key
  "Model-input hygiene at the ONE inbound conversion: a dict key spelled
   `\":from_anchor\"` is still a STRING (the model drifting into colon
   spelling while reading keyword-heavy source), so strip the single leading
   colon when an identifier char follows and the call just works — no
   lecture, no failure. Data keys are untouched: anchors (`\"44:f14\"`) start
   with a digit, paths with a letter or `/`, neither with `:`. Produces
   strings, never keywords."
  ^String [^String s]
  (if (and (> (count s) 1)
           (= \: (.charAt s 0))
           (let [c (.charAt s 1)]
             (or (Character/isLetter c) (= \_ c))))
    (subs s 1)
    s))

(defn- key->py
  "Map key -> the Python-side dict key. STRINGS-ONLY: a string key passes
   verbatim; anything else (keyword, symbol, number, ...) is a producer bug
   and throws `boundary-violation!`."
  ^String [k path]
  (if (string? k) k (boundary-violation! :non-string-key k path)))

(defn- leaf->py
  "LEAF (non-collection) conversion shared by `->py` (the real boundary) and
   `boundary-view` (the no-context test mirror) — one fn so the mirror can
   never drift from the boundary again:

   - keywords/symbols are FORBIDDEN — strings-only boundary; throw with the
     key path so the producer that leaked one is directly nameable.
   - UUIDs (workspace/session ids in ctx) and java.time instants have no
     Python analog — GraalPy would otherwise expose them as opaque
     `<JavaObject[...]>` host pointers. Stringify so the rendered ctx and
     the live dict both read as plain str.
   - `java.util.Date` is what nippy hands back for every persisted `#inst`
     (session/turn created_at) and it is NOT a Temporal — left as a
     host object GraalPy materialises it as a Python datetime, which needs
     the context's datetime module data: never imported ⇒
     `NullPointerException: Cannot read field \"utc\" because \"moduleData\"
     is null` (session 9c829d10, `sessions()`). ISO-8601 string instead.
   - numbers and other auto-convertible boxed types hand straight to
     polyglot."
  [x path]
  (cond (keyword? x) (boundary-violation! :keyword-value x path)
        (symbol? x) (boundary-violation! :symbol-value x path)
        (instance? java.util.Date x) (str (.toInstant ^java.util.Date x))
        (or (instance? java.util.UUID x) (instance? java.time.temporal.Temporal x)) (str x)
        :else x))

(defn- ->py*
  "Recursive worker for `->py`, threading the key `path` so a strings-only
   violation names the exact producer field."
  [x path]
  (cond (nil? x) nil
        (string? x) x
        (boolean? x) x
        ;; `java.util.Map` covers BOTH Clojure maps (which implement it) AND a raw
        ;; ordered `LinkedHashMap` a tool returns (e.g. cat's anchor map). The new
        ;; LinkedHashMap preserves the source's ITERATION ORDER — Clojure array-map
        ;; canonical key order, or a LinkedHashMap's insertion order — so the live
        ;; `ctx` dict and the rendered text agree, and ordered tool maps reach
        ;; Python as ordered dicts (not opaque host objects).
        (instance? java.util.Map x) (let [^LinkedHashMap hm (LinkedHashMap.)]
                                      (doseq [[k v] x]
                                        (let [^String ks (key->py k path)]
                                          (.put hm ks (->py* v (conj path ks)))))
                                      (ProxyHashMap/from hm))
        (or (vector? x) (seq? x) (set? x))
        (ProxyArray/fromList (ArrayList. ^java.util.Collection (mapv #(->py* % path) x)))
        :else (leaf->py x path)))

(defn ->py
  "Clojure value -> something GraalPy accepts as a Python value. STRINGS-ONLY
   boundary: map keys must be strings and no keyword/symbol may appear at any
   depth — a violation throws `boundary-violation!` naming the key path.
   Primitives and Strings pass through (the Context auto-converts Java boxed
   types); collections become polyglot proxies so Python sees dict/list;
   leaves convert via `leaf->py` (UUID/Temporal/Date -> ISO strings)."
  [x]
  (->py* x []))

(defn ->clj
  "Polyglot `Value` (a Python value) -> Clojure data. STRINGS-ONLY boundary:
   dicts -> maps with VERBATIM STRING keys (exactly what Python held — no
   keywordizing, no regex key-shape sniffing), lists/tuples -> vectors, host
   objects (Java values that crossed the boundary, e.g. UUIDs) -> their
   underlying Java value via `asHostObject`, callables/opaque objects -> the
   raw `Value`. A non-string Python dict key (int, tuple, ...) stringifies via
   its Clojure conversion so the map stays string-keyed and total."
  [^Value v]
  (cond (nil? v) nil
        (.isNull v) nil
        (.isBoolean v) (.asBoolean v)
        (.isString v) (.asString v)
        (.isNumber v) (if (.fitsInLong v) (.asLong v) (.asDouble v))
        (.hasArrayElements v) (mapv #(->clj (.getArrayElement v (long %)))
                                    (range (.getArraySize v)))
        ;; Dicts preserve INSERTION ORDER: GraalPy's key iterator is insertion-
        ;; ordered, so accumulate into a flatland ordered-map (NOT a hash-map, whose
        ;; >8-key promotion scrambles order). Without this, a round-tripped ordered
        ;; tool result (cat's anchors LinkedHashMap) comes back HASH-ordered and
        ;; the model reads the file out of line order. ordered-map is still a
        ;; persistent Clojure map (assoc/dissoc/string-lookup all work downstream).
        (.hasHashEntries v) (let [it (.getHashKeysIterator v)]
                              (loop [m (omap/ordered-map)]
                                (if (.hasIteratorNextElement it)
                                  (let
                                    [k (.getIteratorNextElement it)
                                     ks (normalize-dict-key
                                          (if (.isString k) (.asString k) (str (->clj k))))]

                                    (recur (assoc m ks (->clj (.getHashValue v k)))))
                                  m)))
        (.isHostObject v) (.asHostObject v)
        :else v))

(defn boundary-view
  "What a plain-data Clojure value LOOKS LIKE after the GraalPy round trip —
   the mechanical composition of `->py` then `->clj` without a Python context.
   STRINGS-ONLY: string map keys pass VERBATIM, sets/seqs -> vectors,
   UUID/Temporal/Date leaves -> ISO strings. A keyword/symbol anywhere (key or
   value, any depth) throws `boundary-violation!` exactly like the real
   boundary — fix the producer fixture, never catch it. Idempotent.

   Every tool result the model sees in production (serialized structurally
   by `ctx-renderer/render-form-value`) has already crossed this boundary,
   so assertions about what the model reads MUST be written against THIS
   shape. Tests feed `(boundary-view raw-result)` to pin that contract
   without booting GraalPy."
  ([x] (boundary-view x []))
  ([x path]
   (cond (map? x) (into {}
                        (map (fn [[k v]]
                               ;; mirror the REAL round trip: `key->py` guards the
                               ;; outbound key, `normalize-dict-key` is what `->clj`
                               ;; does to it on the way back in.
                               (let [pk (normalize-dict-key (key->py k path))]
                                 [pk (boundary-view v (conj path pk))])))
                        x)
         (or (vector? x) (seq? x) (set? x)) (mapv #(boundary-view % path) x)
         ;; Leaves convert through the SAME fn the real boundary uses — this
         ;; mirror had drifted (Dates/UUIDs/Temporals passed through raw here
         ;; while `->py` stringified them), which let a test assert a shape the
         ;; model never actually sees.
         :else (leaf->py x path))))

(defn sym->py-name
  "Clojure tool/binding symbol -> a Python-LEGAL global name. Purely mechanical:
   `/` and `-` fold to `_` (alias fold + kebab->snake); a trailing `!` (mutation
   marker) is dropped; a trailing `?` (predicate) becomes an `is_` prefix. So
   `git/status` -> `git_status`, `git/commit!` -> `git_commit`, `search/web` ->
   `search_web`, `file-exists` -> `file_exists`. FULL SNAKE:
   this is how the agent reaches the tools — `git_status()` calls `git/status`.

   A tiny compatibility alias layer may additionally expose selected historical
   short names (currently `find` for `find_files`), but the snake name remains
   canonical."
  ^String [sym]
  (let
    [s
     (str sym)

     pred?
     (str/ends-with? s "?")

     base
     (-> s
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
    find_files
    ["find"]

    ;; `find_files` is the canonical name; `find` stays as a compat alias
    []))

(defn- wrap-ifn
  "Wrap a Clojure fn as a Python-callable `ProxyExecutable`. Positional Python
   args are marshalled to Clojure, the fn is applied, and the result marshalled
   back to Python. Matches vis's positional-args tool contract."
  ^ProxyExecutable [f]
  (reify
    ProxyExecutable
      (execute [_ args] (->py (apply f (map ->clj args))))))

;; =============================================================================
;; Canonical CONTEXT serialization — the agent-facing `context` snapshot is a
;; real PYTHON object (an ordered `ProxyHashMap` via `->py`, which GraalPy
;; treats as a native dict), and its printed form is produced BY PYTHON
;; (GraalPy `__vis_pp__`), not by a Clojure reimplementation. So the
;; `<context>` text and `repr(context)` cannot drift: the SAME polyglot object
;; is both bound live and pretty-printed — NO JSON round-trip.
;; =============================================================================

(defn- python-string-literal
  ^String [x]
  (str "\""
       (-> (str x)
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\"")
           (str/replace "\n" "\\n")
           (str/replace "\r" "\\r")
           (str/replace "\t" "\\t"))
       "\""))

(defn- python-number-literal
  ^String [x]
  (cond (and (instance? Double x) (Double/isNaN ^double x)) "nan"
        (and (instance? Double x) (Double/isInfinite ^double x)) (if (neg? ^double x) "-inf" "inf")
        (and (instance? Float x) (Float/isNaN ^float x)) "nan"
        (and (instance? Float x) (Float/isInfinite ^float x)) (if (neg? ^float x) "-inf" "inf")
        (instance? java.math.BigDecimal x) (.toPlainString ^java.math.BigDecimal x)
        (instance? clojure.lang.Ratio x) (str "(" (numerator x) " / " (denominator x) ")")
        :else (str x)))

(declare python-literal*)

(defn- python-map-literal
  ^String [m indent width path]
  (if (empty? m)
    "{}"
    (let
      [items
       (mapv (fn [[k v]]
               (let [ks (key->py k path)]
                 (str (python-string-literal ks)
                      ": "
                      (python-literal* v (inc (long indent)) width (conj path ks)))))
             m)

       inline
       (str "{" (str/join ", " items) "}")]

      (if (and (not (str/includes? inline "\n")) (<= (+ (long indent) (count inline)) (long width)))
        inline
        (str "{\n"
             (str/join ",\n" (map #(str (apply str (repeat (inc (long indent)) " ")) %) items))
             "\n"
             (apply str (repeat indent " "))
             "}")))))

(defn- python-list-literal
  ^String [xs indent width path]
  (if (empty? xs)
    "[]"
    (let
      [items
       (mapv #(python-literal* % (inc (long indent)) width path) xs)

       inline
       (str "[" (str/join ", " items) "]")]

      (if (and (not (str/includes? inline "\n")) (<= (+ (long indent) (count inline)) (long width)))
        inline
        (str "[\n"
             (str/join ",\n" (map #(str (apply str (repeat (inc (long indent)) " ")) %) items))
             "\n"
             (apply str (repeat indent " "))
             "]")))))

(defn- python-literal*
  ^String [x indent width path]
  (cond (instance? java.util.Map x) (python-map-literal x indent width path)
        (or (vector? x) (seq? x) (set? x)) (python-list-literal x indent width path)
        :else (let [v (leaf->py x path)]
                (cond (nil? v) "None"
                      (true? v) "True"
                      (false? v) "False"
                      (string? v) (python-string-literal v)
                      (number? v) (python-number-literal v)
                      (char? v) (python-string-literal v)
                      ;; Boundary producers should emit plain data. Keep an unexpected
                      ;; host leaf deterministic and executable instead of leaking a
                      ;; process-specific <JavaObject ... at 0x...> pseudo-literal.
                      :else (python-string-literal (str v))))))

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
   output because you forgot `await` on a call you USE still repr's a loud hint
   rather than running silently — EXCEPT `print(...)`, which auto-settles a
   deferred call/gather handed straight to it (so `print(rg(x))` shows the real
   result), and EXCEPT inline subscript / `len` / `in` on a deferred call (so
   `git(x)['stdout']` settles that ONE call in place — no concurrency to lose);
   only TOP-LEVEL bare calls otherwise auto-settle."
  "
import ast as __vis_ast__

def __vis_count_forms__(src):
    return len(__vis_ast__.parse(src).body)

def __vis_banned_name__(src, banned):
    banned = set(banned)
    return next((n.id for n in __vis_ast__.walk(__vis_ast__.parse(src))
                 if isinstance(n, __vis_ast__.Name) and n.id in banned), None)

class __vis_Raise__:
    # Driver -> awaitable signal that the tool/gather call the driver just ran
    # RAISED. The await point re-`raise`s the captured exception INSIDE the
    # coroutine (at the user's own `await`), so an in-block `try/except` around
    # `await tool(...)` CATCHES a tool failure like any other error; left
    # uncaught it escapes the driver exactly as before.
    __slots__ = ('exc',)
    def __init__(self, exc):
        self.exc = exc

class __vis_ToolError__(Exception):
    # A tool/gather failure normalized to a REAL Python exception. Host tool
    # callables raise a foreign exception that derives from BaseException but NOT
    # from Exception, so a plain `except Exception:` would MISS it. Wrapping gives
    # the model the ordinary contract (`except Exception` / `except BaseException`
    # both catch it) with a clean message, while `__vis_orig__` keeps the original
    # host exception so an UNCAUGHT failure still maps to the same host
    # tool-failure error (message + :data) at the sandbox boundary.
    def __init__(self, orig, msg):
        self.__vis_orig__ = orig
        super().__init__(msg)

def __vis_clean_msg__(exc):
    # The bare message of a foreign host exception. `str(exc)` on a host throwable
    # is `fully.qualified.ClassName: message`, and a deny-by-default sandbox does
    # NOT expose its Java `getMessage()`, so strip that leading dotted class name
    # to leave just the message. (The authoritative error channel still recovers
    # the exact host message via ex-message at the boundary.)
    try:
        m = exc.getMessage()
        if m:
            return str(m)
    except BaseException:
        pass
    s = str(exc)
    i = s.find(': ')
    if i > 0:
        head = s[:i]
        if '.' in head and ' ' not in head:
            return s[i + 2:]
    return s

def __vis_wrap_tool_exc__(exc):
    # A native Python exception passes through untouched (its own type/message are
    # the contract). A foreign host exception is wrapped so `except Exception`
    # catches it; the original rides along as `__vis_orig__` for boundary mapping.
    if isinstance(exc, Exception):
        return exc
    return __vis_ToolError__(exc, __vis_clean_msg__(exc))

class __vis_Call__:
    __slots__ = ('fn', 'a', 'k', 'nm', 'ran', 'res')
    def __init__(self, fn, a, k, nm='tool'):
        self.fn = fn; self.a = a; self.k = k; self.nm = nm; self.ran = False; self.res = None
    def __await__(self):
        __vis_r__ = yield self
        if type(__vis_r__) is __vis_Raise__:
            raise __vis_r__.exc
        return __vis_r__
    def __repr__(self):
        return '<unawaited async tool call: write `await ' + self.nm + '(...)`>'
    # INLINE-USE auto-settle. Subscripting / `len(...)` / `in` a deferred call is
    # ALWAYS a single-expression use of that ONE call's result — there is no
    # concurrency to forfeit (unlike a batchable set of calls), so we settle it
    # synchronously right here instead of raising 'not subscriptable'. This kills
    # the `git(...)[\"stdout\"]` / `cat(...)[\"anchors\"]` papercut. We deliberately
    # do NOT add `__getattr__`/`__iter__`: attribute access is probed by internal
    # plumbing (`hasattr(v, 'send')`), and iteration is exactly the batch-me-instead
    # case the loud repr must keep nudging toward `await gather(...)`.
    def __getitem__(self, k):
        return __vis_settle__(self)[k]
    def __len__(self):
        return len(__vis_settle__(self))
    def __contains__(self, k):
        return k in __vis_settle__(self)

class __vis_Gather__:
    __slots__ = ('aws',)
    def __init__(self, aws):
        self.aws = aws
    def __await__(self):
        __vis_r__ = yield self
        if type(__vis_r__) is __vis_Raise__:
            raise __vis_r__.exc
        return __vis_r__

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

class __VisResultList__(list):
    # A native tool result whose TOP-LEVEL shape is a LIST (patch / struct_patch /
    # write return one row per file; some tools return a list of hits). It stays a
    # REAL list — index / iterate / len / json.dumps / {**_}-free code all behave —
    # but ALSO answers the dict probes (.get/.keys/.items/.values) so a uniform
    # `for _id, res in ntr.items(): res.get('op')` sweep NEVER trips on it. A list has
    # no top-level 'op', so .get returns the default and each row stays reachable by
    # index (res[0]['op']).
    def get(self, __k__, __d__=None):
        return __d__
    def keys(self):
        return []
    def items(self):
        return []
    def values(self):
        return []

class __VisResultStr__(str):
    # A native tool result that is a bare STRING (a tool returning plain text). Still a
    # real str, but answers the same dict probes, so `.get('op')` yields None instead of
    # blowing up with a `'str' object has no attribute 'get'` when a mixed ntr sweep hits
    # it. .keys()/.items()/.values() are empty — a string has no fields.
    def get(self, __k__, __d__=None):
        return __d__
    def keys(self):
        return []
    def items(self):
        return []
    def values(self):
        return []

def __vis_as_result__(__vis_v__):
    # Normalize a STORED native result (ntr[id]) so EVERY value answers the dict probes
    # (.get/.keys/.items/.values) — the shape the model reaches for when it iterates the
    # store. A dict passes through untouched (a tool-result dict is already a
    # __VisResult__). A top-level list/tuple/str is re-typed to a probeable subclass that
    # KEEPS its native list/str behavior, so `res.get('op')` is safe on the whole set
    # without an isinstance guard. Rare scalars (int/float/None/bytes) pass through.
    if isinstance(__vis_v__, dict):
        return __vis_v__
    if isinstance(__vis_v__, (__VisResultList__, __VisResultStr__)):
        return __vis_v__
    if isinstance(__vis_v__, (list, tuple)):
        return __VisResultList__(__vis_v__)
    if isinstance(__vis_v__, str):
        return __VisResultStr__(__vis_v__)
    return __vis_v__

try:
    import polyglot as __vis_polyglot__
    __vis_Foreign__ = __vis_polyglot__.ForeignObject
    def __vis_is_foreign__(x):
        # A host/polyglot proxy (ProxyHashMap/ProxyArray/ForeignDict/…) that
        # crossed the Clojure->Python boundary. NATIVE python values (dict,
        # list, set, tuple, a user object) are NEVER a ForeignObject.
        return isinstance(x, __vis_Foreign__)
except Exception:
    def __vis_is_foreign__(x):
        # Fallback (no `polyglot` module, e.g. non-GraalPy): approximate the
        # old allowlist — treat anything outside real-python primitives as a
        # proxy so tool results still rebuild.
        return not (type(x) in (dict, list, str, bytes, int, float, bool)
                    or isinstance(x, __VisResult__))

def __vis_pyify__(x):
    # Tool results cross the host boundary as ProxyHashMap/ProxyArray. GraalPy lets
    # you subscript / iterate / .get them, but isinstance(_, dict), {**_},
    # json.dumps(_), dict(_) and type(_) all see a FOREIGN object — NOT a real
    # dict — a frequent source of friction. Rebuild proxies into REAL python
    # dict/list ONCE (at settle) so the model composes on true dicts. A HOST proxy
    # carrying 'op' is a tool result → mark its type __VisResult__. Order is
    # preserved (source is an ordered LinkedHashMap; comprehensions keep it).
    #
    # ONLY foreign proxies are rebuilt. A value the model itself built — set /
    # frozenset / tuple / defaultdict / Counter / any user object — is ALREADY
    # native python and passes through UNTOUCHED. (Blindly rebuilding by an
    # allowlist silently downgraded set/tuple/frozenset -> list and dict
    # subclasses -> dict, so `s = set(); s.add(1)` blew up with the
    # 'list' object has no attribute 'add' error.)
    if x is None or type(x).__name__ == 'NoneType':
        return None
    if not __vis_is_foreign__(x):
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
        try:
            if isinstance(y, __vis_Call__):
                send = __vis_exec_call__(y)
            elif isinstance(y, __vis_Gather__):
                send = __vis_par__([(lambda a=a: __vis_settle__(a)) for a in y.aws])
            else:
                send = y
        except BaseException as __vis_exc__:
            # The tool/gather call RAISED. Hand the exception to the awaitable via
            # the next send so it re-raises at the coroutine's OWN await point: an
            # in-block `try/except` can then catch it, and if uncaught it simply
            # propagates out of the driver just as it did before.
            send = __vis_Raise__(__vis_wrap_tool_exc__(__vis_exc__))

def __vis_error_pos__(e):
    # Deepest '<prog>' (user-code) traceback frame -> (line, col, end_col). The
    # async trampoline (__vis_drive__) unwinds the guest stack, so a GraalPy
    # PolyglotException.getPolyglotStackTrace() LOSES these frames; the Python
    # __traceback__ is the only place the failing user-code position survives.
    # col/end_col are 0-based (co_positions), None when column info is absent.
    tb = getattr(e, '__traceback__', None)
    line = None; col = None; end_col = None
    while tb is not None:
        f = tb.tb_frame
        if f.f_code.co_filename == '<prog>':
            line = tb.tb_lineno
            col = None; end_col = None
            try:
                p = list(f.f_code.co_positions())[f.f_lasti // 2]
                if p[2] is not None:
                    col = p[2]; end_col = p[3]
            except Exception:
                pass
        tb = tb.tb_next
    return None if line is None else (line, col, end_col)

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
        elif isinstance(node, (__vis_ast__.Import, __vis_ast__.ImportFrom)):
            for al in node.names:
                add((al.asname or al.name).split('.')[0])
        # `for`/`with` targets are DELIBERATELY excluded. They are transient
        # scratch names, so we leave them local to __vis_main__ (never added to
        # the `global` list) — they don't persist across blocks and so can't
        # clobber a protected callable. That also means innocuous loops like
        # `for doc in docs:` / `with open(p) as patch:` are NOT flagged as a
        # durable rebind of a tool name by the protected-rebind guard.
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
    #   • `import socket`                                ->  passthrough. socket is
    #     ALSO auto-imported onto builtins (always present); the module imports
    #     fine even with the network toggle off — only a live connect is gated by
    #     `allowHostSocketAccess`, which raises a clean UnsupportedOperation.
    #   • `import select` / `selectors` / `ssl` ...      ->  dropped (no shim; a
    #     later use is a clean NameError, not a native crash).
    #   • imports binding a protected builtin           ->  dropped (would shadow).
    # Everything else (json, re, ...) is untouched; the ORIGINAL src is returned
    # when nothing changed (line numbers / formatting preserved).
    prot = set(globals().get('__vis_protected_names__') or [])
    drop = ('select', 'selectors', 'ssl')
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
    g['__vis_err_pos__'] = None         # deepest <prog> failing position, set by the drive except below
    tree = __vis_ast__.parse(src)
    tree = __vis_AwaitFix__().visit(tree)
    __vis_ast__.fix_missing_locations(tree)
    # PRE-SCAN (piggybacks the block parse — zero extra parse cost): collect every
    # literal id read via ntr[...] (or legacy native_tools_results[...]) and PRIME
    # them in ONE batched DB query, so N literal reads never fan out to N fetches.
    # Dynamic keys fall back to a lazy per-key fetch in __getitem__. Guarded: the
    # prime callback is only bound in the full agent context (a bare test context
    # has neither the map nor the callback).
    if '__vis_native_result_prime__' in g and '__vis_native_result_scan__' in g:
        __vis_scan_ids__ = __vis_native_result_scan__(tree)
        if __vis_scan_ids__:
            ntr.__vis_prime__(__vis_scan_ids__)
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
    try:
        g['__vis_async_result__'] = __vis_drive__(g['__vis_main__']())
    except BaseException as __vis_err__:
        g['__vis_err_pos__'] = __vis_error_pos__(__vis_err__)
        raise
    return assigned

def __vis_defer_tools__():
    g = globals()
    for __vis_n__ in list(__vis_defer_names__):
        if __vis_n__ in g and callable(g[__vis_n__]):
            g[__vis_n__] = __vis_deferred__(g[__vis_n__], __vis_n__)

# ── echo-diff strip for a printed edit result: a patch/write/struct_patch result
# printed to stdout merely re-describes the bytes the model just authored, so drop
# each file summary's redundant 'diff' for DISPLAY only. The captured original is
# untouched, so the host op-card still renders the full diff.
def __vis_is_file_summary__(__m__):
    return (isinstance(__m__, dict) and isinstance(__m__.get('path'), str)
            and isinstance(__m__.get('op'), str) and 'changed' in __m__)
def __vis_strip_echo_diff__(__m__):
    return {__k__: __v__ for __k__, __v__ in __m__.items() if __k__ != 'diff'}
def __vis_strip_echo_diffs__(__x__):
    if isinstance(__x__, list) and __x__ and all(__vis_is_file_summary__(__e__) for __e__ in __x__):
        return [__vis_strip_echo_diff__(__e__) for __e__ in __x__]
    if __vis_is_file_summary__(__x__):
        return __vis_strip_echo_diff__(__x__)
    return __x__

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
    # Auto-SETTLE a deferred call/gather handed to print WITHOUT `await` (e.g.
    # `print(rg(...))`): run it and show the real result instead of the loud
    # '<unawaited async tool call …>' repr. Only OUR OWN deferred thunks are
    # settled (never a stray generator/coroutine the model meant to print); every
    # other arg pyifies exactly as before.
    __vis_a__ = tuple(
        __vis_settle__(__a__) if isinstance(__a__, (__vis_Call__, __vis_Gather__))
        else __vis_pyify__(__a__)
        for __a__ in __vis_a__)
    if __vis_kw__.get('file') is None:
        for __vis_x__ in __vis_a__:
            if isinstance(__vis_x__, __VisResult__):
                __vis_printed_results__.append(__vis_x__)
            else:
                globals()['__vis_only_results__'] = False
        if not __vis_a__:                 # a bare print() (blank line) is not a result
            globals()['__vis_only_results__'] = False
    # DISPLAY strips echo-diffs from a printed edit result (stdout mirrors the model
    # wire); capture above kept the un-stripped originals for the host op-card.
    return __vis_real_print__(*tuple(__vis_strip_echo_diffs__(__a__) for __a__ in __vis_a__), **__vis_kw__)
print = __vis_print__

# ── ntr / native_tools_results: retrieve a PRIOR native tool's result by its
# provider tool_use id, WITHOUT re-running the tool. `ntr` is the short public
# name; `native_tools_results` remains as a backwards-compatible verbose alias.
# Every native tool call vis persisted (this turn's earlier iterations AND past
# turns) is reachable by the SAME id the model saw on its tool_result. A read is
# a single DB fetch (thaw + rehydrate to the EXACT __VisResult__ dict the fresh
# call returned), then cached in-process.
#
# `__vis_native_result_prime__(ids)` (Clojure) does ONE batched DB query for a
# list of ids → {id: result} (a proxy per hit; misses absent). `__vis_run_async__`
# calls it with the LITERAL ids AST-scanned from the block, so N literal reads cost
# ONE query. `__vis_native_result_fetch__(id)` (Clojure) is the lazy single-id
# fallback for a DYNAMIC key (a variable / comprehension the scan can't see).
# A miss → a clean KeyError, never a crash.
#
# It is ALSO a read-only mapping: `__vis_native_result_ids__()` (Clojure) lists
# every persisted tool_use id in the session (newest first), backing keys() /
# items() / values() / __iter__ / __len__ so the store is BROWSEABLE without
# knowing an id up front.
class __VisNativeResults__:
    def __init__(self):
        self.__vis_cache__ = {}          # id -> pyified __VisResult__ (already fetched)
        self.__vis_missing__ = set()     # ids proven absent this process (skip re-fetch)

    def __vis_store__(self, __vis_id__, __vis_raw__):
        # Stamp the rehydrated proxy into the SAME __VisResult__ shape a fresh
        # native call yields (a dict carrying 'op' → __VisResult__ via pyify).
        __vis_v__ = __vis_as_result__(__vis_pyify__(__vis_raw__))
        self.__vis_cache__[__vis_id__] = __vis_v__
        return __vis_v__

    def __vis_prime__(self, __vis_ids__):
        # Pre-populate from ONE batched host query. Only ids we have NOT already
        # resolved (cached hit OR proven missing) are queried — a re-read of an
        # id primed by an earlier block hits the in-process cache with NO new DB
        # round-trip. Absent ids are recorded as missing so a later __getitem__
        # raises immediately (no redundant fetch).
        __vis_need__ = [i for i in __vis_ids__
                        if i not in self.__vis_cache__ and i not in self.__vis_missing__]
        if not __vis_need__:
            return
        try:
            __vis_hits__ = __vis_native_result_prime__(__vis_need__)
        except Exception:
            __vis_hits__ = None
        __vis_hits__ = __vis_hits__ or {}
        for __vis_id__ in __vis_need__:
            if __vis_id__ in __vis_hits__ and __vis_hits__[__vis_id__] is not None:
                self.__vis_store__(__vis_id__, __vis_hits__[__vis_id__])
            else:
                self.__vis_missing__.add(__vis_id__)

    def __getitem__(self, __vis_id__):
        if __vis_id__ in self.__vis_cache__:
            return self.__vis_cache__[__vis_id__]
        if __vis_id__ not in self.__vis_missing__:
            # Lazy single-id fetch (dynamic key the pre-scan couldn't see).
            try:
                __vis_raw__ = __vis_native_result_fetch__(__vis_id__)
            except Exception:
                __vis_raw__ = None
            if __vis_raw__ is not None:
                return self.__vis_store__(__vis_id__, __vis_raw__)
            self.__vis_missing__.add(__vis_id__)
        raise KeyError(
            'no native tool result for ' + repr(__vis_id__) +
            ' — that tool_use id is unknown or produced no return (a python_execution '
            'call returns what it print()s, not a stored value). Re-run the tool, or use '
            'the exact tool_use id shown on a prior tool_result.')

    def get(self, __vis_id__, __vis_default__=None):
        try:
            return self[__vis_id__]
        except KeyError:
            return __vis_default__

    def __contains__(self, __vis_id__):
        try:
            self[__vis_id__]
            return True
        except KeyError:
            return False

    def __vis_all_ids__(self):
        # Host list of EVERY native tool_use id persisted in this session branch
        # (newest first) so the store is BROWSEABLE. Degrades to the ids already
        # cached in-process when the callback isn't bound (bare test context).
        try:
            __vis_ids__ = __vis_native_result_ids__()
        except Exception:
            __vis_ids__ = None
        if __vis_ids__ is None:
            return list(self.__vis_cache__.keys())
        # De-dupe, preserving host (newest-first) order.
        __vis_seen__ = set()
        __vis_out__ = []
        for __vis_i__ in __vis_ids__:
            if __vis_i__ not in __vis_seen__:
                __vis_seen__.add(__vis_i__)
                __vis_out__.append(__vis_i__)
        return __vis_out__

    def keys(self):
        return self.__vis_all_ids__()

    def __iter__(self):
        return iter(self.__vis_all_ids__())

    def __len__(self):
        return len(self.__vis_all_ids__())

    def items(self):
        __vis_ids__ = self.__vis_all_ids__()
        self.__vis_prime__(__vis_ids__)   # ONE batched fetch for the whole set
        __vis_out__ = []
        for __vis_i__ in __vis_ids__:
            try:
                __vis_out__.append((__vis_i__, self[__vis_i__]))
            except KeyError:
                pass
        return __vis_out__

    def values(self):
        return [__vis_v__ for __vis_k__, __vis_v__ in self.items()]

ntr = __VisNativeResults__()
native_tools_results = ntr  # backwards-compatible verbose alias

# Literal-key ids a block reads via ntr[...] or native_tools_results[...]
# (STRING subscript only). Used by __vis_run_async__ to prime the whole batch in
# ONE query. A non-literal subscript (a variable / comprehension) is skipped here
# and served lazily by __getitem__.
def __vis_native_result_scan__(__vis_tree__):
    __vis_ids__ = []
    for __vis_n__ in __vis_ast__.walk(__vis_tree__):
        if (isinstance(__vis_n__, __vis_ast__.Subscript)
                and isinstance(__vis_n__.value, __vis_ast__.Name)
                and __vis_n__.value.id in ('ntr', 'native_tools_results')):
            __vis_k__ = __vis_n__.slice
            if isinstance(__vis_k__, __vis_ast__.Constant) and isinstance(__vis_k__.value, str):
                __vis_ids__.append(__vis_k__.value)
    return __vis_ids__
")



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
  (do (when-not (System/getProperty "polyglot.log.file")
        (let [vis-dir (java.io.File. (System/getProperty "user.home") ".vis/logs")]
          (.mkdirs vis-dir)
          (System/setProperty "polyglot.log.file" (str (java.io.File. vis-dir "vis.log")))))
      true))

(defonce graal-resource-cache-redirected
  ;; GraalPy materializes its Python stdlib ("internal resources", from the
  ;; python-resources jar) under `$XDG_CACHE_HOME/org.graalvm.polyglot` (or
  ;; `~/.cache/org.graalvm.polyglot`) — at RUNTIME, on the JVM and on the
  ;; compiled native-image binary alike. In a confined process — e.g. a macOS
  ;; sandbox profile that only whitelists the workspace and build caches — that
  ;; root is unwritable, and the very first stdlib import in the context
  ;; bootstrap dies with `ModuleNotFoundError: No module named 'ast'`. The
  ;; cache root is read ONCE per JVM (system property
  ;; `polyglot.engine.userResourceCache`) when internal resources initialize,
  ;; so resolve it HERE — at ns load, before `shared-engine` can be forced.
  ;; Precedence: explicit `-Dpolyglot.engine.userResourceCache` (untouched) >
  ;; `python.resource-cache` in merged vis.yml config > `~/.vis/cache/graal-resources`
  ;; (ALWAYS preferred: writable across sandboxed and normal runs, gitignored) >
  ;; `./.graal-resources` under the CWD. We deliberately do NOT keep the default
  ;; `~/.cache/org.graalvm.polyglot` root even when it's writable — redirecting
  ;; unconditionally makes stdlib materialization behave identically whether or
  ;; not the sandbox happens to whitelist that root. Documented in
  ;; vis-docs/configuration.md § GraalPy internal-resource cache.
  (or
    (some? (System/getProperty "polyglot.engine.userResourceCache"))
    (let
      [expand-home
       (fn [^String p]
         (if (or (= p "~") (str/starts-with? p "~/"))
           (str (System/getProperty "user.home") (subs p 1))
           p))

       usable?
       (fn [^java.io.File d]
         (try (.mkdirs d)
              (let [p (java.io.File. d ".vis-probe")]
                (spit p "")
                (.delete p)
                true)
              (catch Throwable _ false)))

       ;; `python.resource-cache` from the merged YAML config tiers.
       ;; requiring-resolve keeps this ns decoupled from config's load
       ;; order; any config error degrades to the automatic behavior.
       configured
       (try (when-let
              [path (some-> ((requiring-resolve
                               'com.blockether.vis.internal.config/load-config-raw))
                            (get-in ["python" "resource-cache"]))]
              (java.io.File. ^String (expand-home path)))
            (catch Throwable _ nil))

       ;; Always land on a writable, gitignored cache dir; never keep the
       ;; default `~/.cache/org.graalvm.polyglot` root.
       chosen
       (or (when (and configured (usable? configured)) configured)
           (first (filter usable?
                          [(java.io.File. (System/getProperty "user.home")
                                          ".vis/cache/graal-resources")
                           (java.io.File. ".graal-resources")])))]

      (when chosen
        (System/setProperty "polyglot.engine.userResourceCache"
                            (.getAbsolutePath ^java.io.File chosen)))
      true)))

;; `engine.WarnVirtualThreadSupport=false` is applied INLINE on each
;; Engine/Context builder chain below (no shared helper: an untyped
;; helper arg would force reflection on the whole chain). We
;; deliberately run polyglot contexts on virtual threads (gateway turn
;; workers, Jetty handlers); the per-engine experimental warning is
;; pure noise. The option itself is experimental, hence
;; `allowExperimentalOptions` — it gates only option NAMES we set
;; explicitly, nothing about sandbox permissions.


;; -----------------------------------------------------------------------------
;; Auxiliary engine cache — native binary ONLY
;;
;; GraalVM's auxiliary engine cache persists the shared Engine's warmed,
;; JIT-compiled Truffle code to disk; loading it on the next start skips the
;; JVMCI warm-up that leads every cumulative-CPU table during GraalPy boot. Two
;; hard facts (verified against polyglot 25.1.3) make it native-binary-only in
;; BOTH directions:
;;   - `Engine.storeCache(Path)` throws UnsupportedOperationException
;;     ("only supported on native-image hosts") on the JVM.
;;   - the `engine.CacheLoad` runtime option that restores it is contributed
;;     ONLY by the native-image Truffle runtime; the JVM host rejects it
;;     ("Could not find option engine.CacheLoad").
;; So both paths are gated behind `native-image?`. Mirrors the JFR philosophy:
;; idempotent, NEVER throws, NEVER blocks startup — any failure (unsupported
;; build, stale/corrupt image, engine-option or version drift) silently falls
;; back to a normal cold warm-up. The file is keyed by the GraalVM polyglot
;; version so an engine upgrade cannot load an incompatible image. Disable with
;; VIS_ENGINE_CACHE=0 (shares the falsey-token convention with VIS_MEM_LOG).

(defn- native-image?
  "True inside the compiled GraalVM binary (never on the JVM dev/gateway)."
  []
  (some? (System/getProperty "org.graalvm.nativeimage.imagecode")))

(def ^:private engine-cache-disabled-tokens #{"0" "false" "off" "no"})

(defn- engine-cache-enabled?
  "Off only when VIS_ENGINE_CACHE is a falsey token; unset/anything else = on."
  []
  (not (contains? engine-cache-disabled-tokens
                  (some-> (System/getenv "VIS_ENGINE_CACHE")
                          str/trim
                          str/lower-case))))

(defn- engine-cache-file
  "~/.vis/cache/engine/vis-engine-<polyglot-version>.img — versioned so an engine
   upgrade never loads a stale, incompatible cache image."
  ^java.io.File []
  (let
    [ver (or (some-> (io/resource "META-INF/graalvm/org.graalvm.polyglot/version")
                     slurp
                     str/trim
                     not-empty)
             "unknown")]
    (io/file (System/getProperty "user.home")
             ".vis" "cache"
             "engine" (str "vis-engine-" ver ".img"))))

(defn- store-engine-cache!
  "Persist the warmed shared Engine's compiled code to `f` on shutdown so the
   next native-binary start skips JVMCI warm-up. Native-image only; never throws."
  [^Engine engine ^java.io.File f]
  (try (.mkdirs (.getParentFile f)) (.storeCache engine (.toPath f)) (catch Throwable _ nil)))

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
  ;;
  ;; COMPILER-THREAD CAP: Truffle's JIT (`engine.CompilerThreads`) defaults to
  ;; -1 = scale with CPU cores. On a 14-core box that spawns a fistful of JVMCI
  ;; CompilerThreads that dominated every cumulative-CPU table during GraalPy
  ;; warm-up. Cap at 2 (plenty for one shared Engine) and let idle compiler
  ;; threads retire after 5s (default 10s) — fewer live threads, smaller CPU
  ;; bursts, at a slightly slower warm-up. Stable engine options; takes effect
  ;; on the next gateway start.
  (delay
    (let
      [build-engine
       (fn ^Engine [^java.io.File load-file]
         (let
           [b (-> (Engine/newBuilder (into-array String ["python"]))
                  (.allowExperimentalOptions true)
                  (.option "engine.WarnVirtualThreadSupport" "false")
                  (.option "engine.CompilerThreads" "2")
                  (.option "engine.CompilerIdleDelay" "5000"))]
           ;; `engine.CacheLoad` restores the warmed compiled code. The option
           ;; only EXISTS on a native-image host; on the JVM it throws, so we
           ;; only reach here under `use-cache?` (native binary). Any load
           ;; failure (stale/corrupt/option-drift image) is caught by the
           ;; caller and falls back to a plain cold build.
           (when load-file (.option b "engine.CacheLoad" (.getAbsolutePath load-file)))
           (.build b)))

       use-cache?
       (and (native-image?) (engine-cache-enabled?))

       cache-file
       (when use-cache? (engine-cache-file))

       engine
       (or (when (and cache-file (.exists ^java.io.File cache-file))
             (try (build-engine cache-file) (catch Throwable _ nil)))
           (build-engine nil))]

      ;; Store the warmed engine on process exit so the NEXT native start can
      ;; load it. Best-effort, never throws (see `store-engine-cache!`).
      (when use-cache?
        (.addShutdownHook (Runtime/getRuntime)
                          (Thread. ^Runnable
                                   (fn []
                                     (store-engine-cache! engine cache-file))
                                   "vis-engine-cache-store")))
      engine)))

(defn ctx->python-str
  "Render plain boundary data as a deterministic, executable Python literal.

   This is deliberately a pure JVM serializer: rendering never enters GraalPy,
   never waits behind a session's GIL, and needs no process-global printer
   Context or lock. It mirrors the Clojure->Python boundary (string-only map
   keys, list-like collections, ISO strings for Date/UUID/Temporal) and keeps
   insertion order plus the historical 100-column layout."
  ^String [data]
  (python-literal* data 0 100 []))

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
  (.eval
    ^Context python-context
    "python"
    (str
      "if '__vis_pyify__' in globals():\n" ; guard: a context without the substrate keeps the proxy
      "    globals()['session'] = {__k__: __vis_pyify__(__v__) for __k__, __v__ in session.items()}")))

(defn seed-cli-runtime!
  "Seed a standalone `vis python` CLI context with script `argv` (bound to
   `sys.argv`) and, when non-empty, an `env` map merged into `os.environ`.
   This is what gives the CLI real-`python` semantics: unlike the deny-by-
   default AGENT sandbox (env scrubbed for isolation — the human never sees
   their shell here), the human-run CLI forwards trailing script args and,
   by default, the caller's environment.

   Mirrors the `VIS_OUTBOX` injection: values cross via `putMember`, then a
   guest eval assigns them (a JSON hop keeps ProxyHashMaps off the boundary
   and reuses the auto-imported `json`). Best-effort: a bad value never
   aborts startup."
  [python-context {:keys [argv env]}]
  (let
    [^Context ctx
     python-context

     g
     (.getBindings ctx "python")]

    (when (some? argv)
      (try (.putMember g "__vis_cli_argv_json__" (json/write-json-str (vec argv)))
           (.eval ctx
                  "python"
                  (str "import sys as __vis_sys__\n"
                       "__vis_sys__.argv = list(json.loads(__vis_cli_argv_json__))\n"
                       "del __vis_sys__"))
           (.putMember g "__vis_cli_argv_json__" nil)
           (catch Throwable _ nil)))
    (when (seq env)
      (try (.putMember g "__vis_cli_env_json__" (json/write-json-str env))
           (.eval ctx
                  "python"
                  (str "import os as __vis_os__\n"
                       "__vis_os__.environ.update(json.loads(__vis_cli_env_json__))\n"
                       "del __vis_os__"))
           (.putMember g "__vis_cli_env_json__" nil)
           (catch Throwable _ nil)))
    python-context))

(def ^:dynamic *lru-atom* nil)

(def ^:dynamic *current-turn-position* nil)

(defn fresh-lru-atom [] (atom {}))

;; =============================================================================
;; Block validation (Python: top-level statement count + banned constructs)
;; =============================================================================


(defn count-top-level-forms
  "Number of top-level Python statements in `code`, parsed inside the session's
   own GraalPy Context. Comment-/whitespace-only blocks return 0. The source is
   passed directly to a cached helper — no shared scratch global, auxiliary
   Context, or cross-thread race."
  [python-context code]
  (let
    [^Context ctx
     python-context

     ^Value f
     (.getMember (.getBindings ctx "python") "__vis_count_forms__")]

    (long (.asLong (.execute f (object-array [(str code)]))))))

(defn validate-non-empty-block!
  "Throws `:vis/empty-block` when `code` parses to zero top-level statements
   (comment-only blocks). Iterations that produce no evidence are rejected at
   the model boundary."
  [python-context code]
  (when (zero? #_{:clj-kondo/ignore [:redundant-primitive-coercion]}
               (long (count-top-level-forms python-context code)))
    (throw (ex-info "Block is empty (only comments). Iteration produces no evidence."
                    {:type :vis/empty-block :form-count 0}))))

(def BANNED_DEF_HEADS
  "Python constructs refused pre-eval — belt-and-suspenders against the obvious
   sandbox-escape footguns on top of the Context restrictions."
  #{"exec" "eval" "compile" "__import__"})

(defn validate-no-banned-defs!
  "Throws `:vis/banned-def-head` when `code` references a banned construct
   (`BANNED_DEF_HEADS`). Parse failures are silent — the eval that follows
   surfaces a clean syntax error with line/column."
  [python-context code]
  (try (let
         [^Context ctx
          python-context

          ^Value f
          (.getMember (.getBindings ctx "python") "__vis_banned_name__")

          ^Value hit
          (.execute f (object-array [(str code) (->py (vec BANNED_DEF_HEADS))]))]

         (when-not (.isNull hit)
           (throw (ex-info (str "Block uses `" (.asString hit)
                                "` which is banned in the Python sandbox "
                                "(sandbox-escape footgun).")
                           {:type :vis/banned-def-head :head (.asString hit)}))))
       (catch clojure.lang.ExceptionInfo ei
         (if (= :vis/banned-def-head (:type (ex-data ei))) (throw ei) nil))
       (catch Throwable _ nil)))

;; =============================================================================
;; Sandbox bindings
;; =============================================================================

(defn- python-globals ^Value [python-context] (.getBindings ^Context python-context "python"))

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
  (let
    [^Context ctx
     python-context

     g
     (python-globals ctx)

     nm
     (sym->py-name sym)

     aliases
     (py-aliases-for-sym sym)

     member
     (if (fn? val) (wrap-ifn val) (->py val))]

    (add-protected-names! g (cons nm aliases))
    (.putMember g nm member)
    (doseq [alias aliases]
      (.putMember g alias member))
    (when (fn? val)
      (try
        (doseq [defer-name (cons nm aliases)]
          (.putMember g "__vis_defer1__" defer-name)
          (.eval
            ctx
            "python"
            (str
              "if '__vis_deferred__' in globals() and callable(globals().get(__vis_defer1__)):\n"
              "    globals()[__vis_defer1__] = __vis_deferred__(globals()[__vis_defer1__], __vis_defer1__)")))
        (finally (.putMember g "__vis_defer1__" nil))))))

(defn set-python-binding-doc!
  "Record `doc` text for `sym` (and its py-aliases) in the sandbox `__vis_docs__`
   dict that in-sandbox `doc(name)` / `apropos(pat)` read. Keyed by the SAME
   py-name(s) `set-python-binding!` binds under, so `doc(\"mcp_servers\")` resolves
   for ALIASED extensions that bind AFTER context creation (per turn, via
   `sync-active-extension-symbols!`) — not only the built-ins seeded eagerly in
   `build-agent-context`. No-op for a blank doc or a helper context with no
   sandbox."
  [python-context sym doc]
  (when (and python-context (string? doc) (not (str/blank? doc)))
    (let
      [^Context ctx
       python-context

       g
       (python-globals ctx)]

      (try (.putMember g "__vis_doc_txt__" (str doc))
           (doseq [nm (cons (sym->py-name sym) (py-aliases-for-sym sym))]
             (.putMember g "__vis_doc_sym__" (str nm))
             (.eval ctx
                    "python"
                    "globals().setdefault('__vis_docs__', {})[__vis_doc_sym__] = __vis_doc_txt__"))
           (finally (.putMember g "__vis_doc_sym__" nil) (.putMember g "__vis_doc_txt__" nil))))))

(def ^:private protected-baseline-names
  "Python globals the agent may CALL but must not rebind. Rebinding a tool or
   parser-helper name would shadow the persistent session substrate."
  #{"apropos" "doc" "gather" "ntr" "native_tools_results" "__vis_count_forms__"
    "__vis_banned_name__"})

(defn- protected-names-for-bindings
  [custom-bindings]
  (set (concat protected-baseline-names
               (mapcat (fn [[sym _]]
                         (cons (sym->py-name sym) (py-aliases-for-sym sym)))
                       (or custom-bindings {})))))

(defn- install-protected-names!
  [^Value g custom-bindings]
  (.putMember g
              "__vis_protected_names__"
              (->py (vec (sort (protected-names-for-bindings custom-bindings))))))

(defn- add-protected-names!
  [^Value g names]
  (let
    [existing
     (set (map str (or (->clj (.getMember g "__vis_protected_names__")) [])))

     names'
     (set (map str names))]

    (.putMember g "__vis_protected_names__" (->py (vec (sort (set/union existing names')))))))

(defn remove-python-binding!
  "Remove `sym` from the Python sandbox globals ENTIRELY — the member key
   disappears, so `apropos`/`dir` no longer list it and calling it raises
   a plain NameError. This is how a deactivated tool must vanish:
   `putMember nil` only parks a None under the name, which `apropos`
   still lists and which calls as 'NoneType is not callable'."
  [python-context sym]
  (try (let [g (python-globals python-context)]
         (.removeMember g (sym->py-name sym))
         (doseq [alias (py-aliases-for-sym sym)]
           (.removeMember g alias))
         ;; Drop any recorded doc too, so a deactivated tool leaves no stale
         ;; `__vis_docs__` entry that `doc`/`apropos` would keep surfacing.
         (doseq [nm (cons (sym->py-name sym) (py-aliases-for-sym sym))]
           (.putMember g "__vis_doc_sym__" (str nm))
           (.eval ^Context python-context
                  "python"
                  "globals().get('__vis_docs__', {}).pop(__vis_doc_sym__, None)"))
         (.putMember g "__vis_doc_sym__" nil))
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
  (let
    [python-context
     (:python-context env)

     g
     (python-globals python-context)]

    (set-python-binding! python-context sym val)
    ;; Stash name -> doc text in a Python dict global that `doc(name)` reads.
    (.putMember g "__vis_doc_sym__" (str sym))
    (.putMember g "__vis_doc_txt__" (str (or doc "vis-managed engine binding")))
    (.eval ^Context python-context
           "python"
           "globals().setdefault('__vis_docs__', {})[__vis_doc_sym__] = __vis_doc_txt__")
    nil))


;; =============================================================================
;; Python sandbox context creation
;; =============================================================================

(defn- install-introspection!
  "Wire Python `apropos(pat)` and `doc(name)` over the live globals — the
   sandbox's own discovery surface. Both read
   the wired member keys; `doc` also reports callable-ness + any registered
   `__vis_docs__` text."
  [^Context ctx]
  (let
    [g
     (.getBindings ctx "python")

     ;; Python's own builtins (`len`, `print`, every `*Error`/`*Warning`
     ;; class, …) are NOT vis tools, so `apropos` must NOT list them — it is
     ;; a TOOL-discovery surface, not a dump of the Python stdlib. Captured
     ;; once (builtins don't change over the context's life). Names starting
     ;; with `_` (REPL slots `_1`/`_e`, `__vis*`, dunders) are engine
     ;; bookkeeping and are filtered too.
     builtin-names
     (set (try (->clj (.eval ctx "python" "dir(__builtins__)")) (catch Throwable _ nil)))

     ;; Engine DATA-accessors that are baseline globals but NOT callable tools —
     ;; the prompt teaches them directly, so they must NOT clutter the tool
     ;; discovery surface (same spirit as filtering `__vis_*`/dunders).
     ;; `ntr` / `native_tools_results` are the prior-result mappings the model
     ;; subscripts; `asyncio` is the async-runtime shim global (`asyncio =
     ;; __vis_asyncio__`, so `import asyncio`/`asyncio.run(...)` work) — a
     ;; runtime, not a tool.
     non-tool-names
     #{"ntr" "native_tools_results" "asyncio"}

     ;; Shim MODULES (yaml, numpy, requests, …) publish via `sys.modules` so
     ;; `import <lib>` works, but many are NOT top-level globals — so they'd
     ;; miss the member-key scan below. The shim install seeds their names
     ;; into `__vis_shims__`; fold them in so `apropos` surfaces every shim.
     shim-names
     (fn []
       (try (let [d (.getMember g "__vis_shims__")]
              (when (and d (not (.isNull d)) (.hasArrayElements d))
                (into #{}
                      (map #(.asString ^Value (.getArrayElement d (long %))))
                      (range (.getArraySize d)))))
            (catch Throwable _ nil)))

     names
     (fn []
       (sort (distinct (concat (filter (fn [n]
                                         (and (not (str/starts-with? n "_"))
                                              (not (contains? builtin-names n))
                                              (not (contains? non-tool-names n))))
                                       (map str (seq (.getMemberKeys g))))
                               (shim-names)))))]

    (.putMember
      g
      "apropos"
      (reify
        ProxyExecutable
          (execute [_ args]
            (let
              [pat
               (if (pos? (alength args)) (.asString ^Value (aget args 0)) "")

               matched
               (filterv #(str/includes? % pat) (names))

               docs
               (let [d (.getMember g "__vis_docs__")]
                 (when (and d (not (.isNull d)) (.hasHashEntries d)) d))

               gist
               (fn [nm]
                 ;; first non-blank line of the registered doc, capped so
                 ;; `apropos` stays a scannable name -> gist index while still
                 ;; carrying enough essence to read (the TUI table-cell wrapper
                 ;; wraps it across rows; full text is `doc(name)`).
                 (let
                   [full
                    (when (and docs (.hasHashEntry docs (->py nm)))
                      (.asString (.getHashValue docs (->py nm))))

                    line
                    (when (seq (str full))
                      (first (remove str/blank? (str/split-lines (str full)))))]

                   (cond (str/blank? (str line)) ""
                         (> (count line) 240) (str (subs line 0 239) "…")
                         :else line)))]

              ;; Return a REAL native Python dict {name -> gist} (order preserved)
              ;; by zipping two parallel arrays guest-side — no ProxyHashMap crosses
              ;; the boundary, so `list()/in/sorted/set/**` all behave natively.
              (.putMember g "__vis_apropos_names__" (->py matched))
              (.putMember g "__vis_apropos_gists__" (->py (mapv gist matched)))
              (try (.eval ctx
                          "python"
                          "dict(zip(list(__vis_apropos_names__), list(__vis_apropos_gists__)))")
                   (finally (.putMember g "__vis_apropos_names__" nil)
                            (.putMember g "__vis_apropos_gists__" nil)))))))
    (.putMember
      g
      "doc"
      (reify
        ProxyExecutable
          (execute [_ args]
            (let
              [nm
               (when (pos? (alength args)) (.asString ^Value (aget args 0)))

               m
               (when nm (.getMember g nm))

               docs
               (let [d (.getMember g "__vis_docs__")]
                 (when (and d (not (.isNull d)) (.hasHashEntries d) nm (.hasHashEntry d (->py nm)))
                   (.asString (.getHashValue d (->py nm)))))]

              (cond (nil? nm) "doc(name): describe a sandbox global"
                    (and (or (nil? m) (.isNull m)) (nil? docs))
                    (str nm ": <not found> — try apropos(\"\")")
                    :else (str "# "
                               nm
                               (when (and m (not (.isNull m)) (.canExecute m)) "  ·  callable")
                               (when docs (str "\n\n" docs))))))))
    ;; Native `apropos` dispatch renders a scannable markdown table (its
    ;; :py-name call-shape points here); in-Python `apropos(query)` stays a real
    ;; dict for filtering. This wrapper reuses that dict and formats one
    ;; `| tool | gist |` table. Underscore-prefixed so `apropos` never lists it.
    (try (.eval ^Context ctx
                "python"
                (str "def __vis_apropos_table__(query=''):\n" "    d = apropos(query)\n"
                     "    if not d:\n"
                     "        return 'apropos(' + repr(query) + '): no tools match.'\n"
                     "    def __cell(s):\n"
                     "        return str(s).replace('\\n', ' ').replace('|', '\\\\|')\n"
                     "    rows = ['| tool | gist |', '| --- | --- |']\n" "    for k in d:\n"
                     "        rows.append('| `' + __cell(k) + '` | ' + __cell(d[k]) + ' |')\n"
                     "    return '\\n'.join(rows)\n"))
         (catch Throwable _ nil))
    ;; These two helpers are installed by the engine rather than the extension
    ;; registry, so seed their own docs here. This keeps native `doc` and
    ;; in-Python `doc(...)` equally useful without a copied prompt table.
    (set-python-binding-doc!
      ctx
      'apropos
      "apropos(query='') -> {name: gist}. List live Python sandbox tools; filter names by an optional substring.")
    (set-python-binding-doc!
      ctx
      'doc
      "doc(name) -> str. Show one live Python sandbox tool's callable contract, arguments, result shape, and mechanics.")
    (set-python-binding-doc!
      ctx
      'gather
      "gather(*awaitables) -> list. Concurrently run independent deferred tool calls or awaitables on host virtual threads; results preserve input order. Also accepts one list/tuple. Use `await gather(call1(...), call2(...))`; keep dependent calls sequential. All slots settle before an aggregated failure reports every failing slot index.")
    (set-python-binding-doc!
      ctx
      'session-fold
      (str
        "session_fold(target, gist=None) -> str. Collapse SETTLED wire steps into a breadcrumb: any prior turn, PLUS the current turn's already-completed iterations. The live iteration you are emitting right now (and any future step) is not settled and cannot be folded; trim the current turn up to the last finished iteration (e.g. {'through': 'tN/iK'}).\n"
        "Targets may be step/turn ids or through/from/to/since selectors. Folding changes rendering, not stored history; there is no destructive unfold command. Recover a folded step the cheap way: the fold breadcrumb itself lists its folded steps' `ntr[<id>]` accessors, and `ntr[tool_id]` retrieves that one prior native result from storage with no re-run — it SURVIVES a harness restart (`ntr.keys()` browses every stored id). Only if an id isn't in view, walk raw content — `s = await session_state()`, select `s['transcript']['turns']` by numeric `position`, then filter `['iterations'][...]['blocks']` (`code`/`result`). For another conversation, use `await sessions()` then `await session_state(id)`.\n"
        "Fold of fold: a broader newer fold supersedes fully covered narrower breadcrumbs; equal scopes keep the newer gist. Partial overlaps remain separate."))))


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
# and still rides the workspace-cwd containment, timeout,
# process-tree kill, output bounding, render badge, and trace recording.
#
# It is tool-AGNOSTIC by construction: the tool callables are looked up in
# globals() at CALL time, not bound at import. So if the shell tool is absent
# (extension not installed), the shim raises a clear message instead of a
# confusing spawn failure; when present, subprocess/os.system route to it.
#
# Installed once per sandbox context (main + every sub_loop fork) by
# env_python/build-agent-context, right after the apropos/doc introspection.

def __vis_install_posix_compat__():
    import sys
    import types
    import shlex
    import time

    _SHELL_DISABLED = (
        \"Shell tools are not available in this vis sandbox (the shell extension \"
        \"is not installed), so subprocess / os.system / os.popen cannot run.\"
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

(def ^:private posix-lazy-init-python
  "Eager, tiny LAZY installer for the POSIX-compat bridge. Registers a
   `subprocess` meta_path finder + `os.system`/`os.popen` thunks that DEFER the
   ~95ms `posix-compat-shim-src` body until the FIRST `import subprocess` /
   `os.system` / `os.popen`, so a session that never shells out never pays it.
   `__vis_load_posix__` is the host callback that eval's the real body, once."
  "def __vis_posix_lazy__():
    import sys as _sys
    import os as _os
    import importlib.util as _u
    _st = {'done': False}
    def _ensure():
        if _st['done']:
            return
        _st['done'] = True
        try:
            __vis_load_posix__()
        except Exception:
            pass
    class _PosixPreloaded:
        def __init__(self, m):
            self._m = m
        def create_module(self, spec):
            return self._m
        def exec_module(self, module):
            pass
    class _PosixFinder:
        def find_spec(self, fullname, path=None, target=None):
            if fullname != 'subprocess':
                return None
            _ensure()
            m = _sys.modules.get('subprocess')
            if m is None:
                return None
            return _u.spec_from_loader(fullname, _PosixPreloaded(m))
    _sys.meta_path.insert(0, _PosixFinder())
    def _mk(nm):
        def _thunk(*a, **k):
            _ensure()
            return getattr(_os, nm)(*a, **k)
        return _thunk
    try:
        _os.system = _mk('system')
        _os.popen = _mk('popen')
    except Exception:
        pass
__vis_posix_lazy__()
del __vis_posix_lazy__
")

(def AUTO_IMPORTED_PYTHON_NAMES
  "Python names installed into builtins for every `python_execution` context.
   This is the model-facing inventory; keep it synchronized with
   `auto-imports-python` and its real-context regression test."
  ["json" "shlex" "re" "hashlib" "glob" "os" "sys" "collections" "Counter" "pathlib" "Path"
   "textwrap" "base64" "math" "socket" "builtins"])

(def ^:private auto-imports-python
  "Install tiny convenience imports as Python builtins so agents can use them
   without repeating imports in every run_python block. os/sys/json/re (plus the
   builtins self-ref) are bound EAGERLY - they are cheap, load anyway via engine
   internals, and are the hottest names. Every OTHER name is a `_LazyStd` proxy
   on builtins: the real stdlib module is imported on the FIRST bare touch
   (`hashlib.md5(...)`, `Counter(...)`, `Path(...)`, `socket.socket(...)`, ...) and then REPLACES
   the proxy, so a session that never touches base64/textwrap/socket/... never
   pays their import at context build OR at every `sub_loop` fork. An explicit
   `import <name>` always works too (normal stdlib path). The model-facing
   inventory is unchanged - see `AUTO_IMPORTED_PYTHON_NAMES`."
  "def __vis_auto_imports__():
    import builtins as _b
    import importlib as _il
    import json as _json, re as _re, os as _os, sys as _sys
    _b.json = _json; _b.re = _re; _b.os = _os; _b.sys = _sys
    _b.builtins = _b
    class _LazyStd:
        def __init__(self, bind, mod, attr):
            object.__setattr__(self, '_bind', bind)
            object.__setattr__(self, '_mod', mod)
            object.__setattr__(self, '_attr', attr)
        def _resolve(self):
            bind = object.__getattribute__(self, '_bind')
            m = _il.import_module(object.__getattribute__(self, '_mod'))
            attr = object.__getattribute__(self, '_attr')
            val = getattr(m, attr) if attr else m
            setattr(_b, bind, val)
            return val
        def __getattr__(self, k):
            return getattr(_LazyStd._resolve(self), k)
        def __call__(self, *a, **k):
            return _LazyStd._resolve(self)(*a, **k)
    for bind, mod, attr in (
        ('shlex', 'shlex', None),
        ('hashlib', 'hashlib', None),
        ('glob', 'glob', None),
        ('collections', 'collections', None),
        ('Counter', 'collections', 'Counter'),
        ('pathlib', 'pathlib', None),
        ('Path', 'pathlib', 'Path'),
        ('textwrap', 'textwrap', None),
        ('base64', 'base64', None),
        ('math', 'math', None),
        ('socket', 'socket', None),
    ):
        setattr(_b, bind, _LazyStd(bind, mod, attr))
__vis_auto_imports__()
del __vis_auto_imports__
")

(defn- install-auto-imports!
  "Make selected stdlib modules and symbols available as builtins in every sandbox.
   os/sys/json/re are eager; the rest are lazy proxies materialized on first
   touch. Keep the inventory tiny: only safe, pure, repeatedly-useful glue."
  [^Context ctx]
  (try (.eval ctx "python" ^String auto-imports-python) (catch Throwable _ nil)))

(defn- install-posix-compat-shim!
  "Install the POSIX-compat bridge LAZILY. Instead of eval'ing the ~95ms shim
   body into every context (main + every `sub_loop` fork), wire the host loader
   `__vis_load_posix__` and eval the tiny `posix-lazy-init-python` stubs (a
   `subprocess` meta_path finder + `os.system`/`os.popen` thunks) that DEFER the
   body until the first `import subprocess` / `os.system` / `os.popen`. A session
   that never shells out never pays it. Best-effort: a failure leaves the sandbox
   without the bridge, it must never break context creation."
  [^Context ctx ^Value g]
  (when-let [src posix-compat-shim-src]
    (try (.putMember g
                     "__vis_load_posix__"
                     (wrap-ifn (fn []
                                 (.eval ctx "python" ^String src)
                                 nil)))
         (.eval ctx "python" ^String posix-lazy-init-python)
         (catch Throwable _ nil))))

(defn- registered-sandbox-shims
  "The Python sandbox SHIMS contributed by the extension registry
   (`extension/sandbox-shims`), resolved lazily to avoid a compile-time cycle
   (env-python is required by the extension kernel). Best-effort: a registry
   hiccup yields no shims rather than breaking context creation."
  []
  (try (when-let [f (requiring-resolve 'com.blockether.vis.internal.extension/sandbox-shims)]
         (vec (f)))
       (catch Throwable _ nil)))

(def ^:private lazy-shim-runtime-python
  "Central LAZY-SHIM runtime installed once per Context, BEFORE the shims.
   Instead of eagerly eval'ing every shim preamble (which materializes each
   module's whole live object graph - numpy/pandas/PIL/... - into THIS context,
   the fat per-session baseline), we register only a tiny trigger table and
   defer each preamble eval until the FIRST touch:
     * `import <name>` - served by a PREPENDED `sys.meta_path` finder (prepended
       so a shim can still shadow a stdlib name, e.g. `zoneinfo`), or
     * a no-import bare `<name>.attr` / `<name>(...)` - served by a `builtins`
       proxy staged under each autoload name.
   The heavy module is then built exactly once per context, on demand; a session
   that never touches numpy never pays numpy's heap. `__vis_load_shim__` is the
   host callback that eval's the matching preamble source into this ctx."
  "def __vis_init_lazy__():
    import sys as _sys
    import builtins as _b
    import importlib.util as _u
    reg = {}
    loading = set()
    loaded = set()
    def _load(sid):
        if sid in loaded or sid in loading:
            return
        loading.add(sid)
        try:
            __vis_load_shim__(sid)
            loaded.add(sid)
        finally:
            loading.discard(sid)
    class _Preloaded:
        def __init__(self, m):
            self._m = m
        def create_module(self, spec):
            return self._m
        def exec_module(self, module):
            pass
    class _Finder:
        def find_spec(self, fullname, path=None, target=None):
            try:
                sid = reg.get(fullname)
                if sid is None:
                    sid = reg.get(fullname.split('.')[0])
                if sid is None:
                    return None
                _load(sid)
                m = _sys.modules.get(fullname)
                if m is None:
                    return None
                return _u.spec_from_loader(fullname, _Preloaded(m))
            except Exception:
                return None
    class _Lazy:
        def __init__(self, sid, name):
            object.__setattr__(self, '_sid', sid)
            object.__setattr__(self, '_name', name)
        def _resolve(self):
            _load(object.__getattribute__(self, '_sid'))
            real = getattr(_b, object.__getattribute__(self, '_name'), None)
            if real is None or real is self:
                raise AttributeError(object.__getattribute__(self, '_name'))
            return real
        def __getattr__(self, k):
            return getattr(_Lazy._resolve(self), k)
        def __call__(self, *a, **k):
            return _Lazy._resolve(self)(*a, **k)
    def register(spec_json):
        import json as _j
        spec = _j.loads(spec_json)
        sid = spec['sid']
        for n in (spec.get('provides') or []):
            reg[n] = sid
        for n in (spec.get('autoload') or []):
            setattr(_b, n, _Lazy(sid, n))
    _sys.meta_path.insert(0, _Finder())
    return register
__vis_register_lazy_shim__ = __vis_init_lazy__()
del __vis_init_lazy__
")

(defn- shim-preamble-src
  "Resolve a shim's `:shim/preamble` (a string, or a 0-arg fn returning one)."
  [shim]
  (let [p (:shim/preamble shim)]
    (if (fn? p) (p) p)))

(defn- wire-shim-bindings!
  "Wire a shim's host `:shim/bindings` (`{py-name -> fn}`, or a 0-arg fn -> one)
   onto the sandbox globals `g` as Python callables. Cheap (proxies only) and
   done EAGERLY even for lazy shims, so a deferred preamble finds them at load."
  [^Value g shim]
  (let
    [b
     (:shim/bindings shim)

     bindings
     (if (fn? b) (b) b)]

    (doseq [[nm f] bindings]
      (.putMember g ^String nm (wrap-ifn f)))))

(defn- sha256-hex
  "Lowercase-hex SHA-256 of `s` (UTF-8) - the cache key over all preamble sources."
  ^String [^String s]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (->> (.digest md (.getBytes s "UTF-8"))
         (map #(format "%02x" (bit-and (int %) 0xff)))
         (apply str))))

(def ^:private shim-triggers-cache-file
  ;; Persistent trigger-map cache: `~/.vis/cache/shim-triggers.json`. Holds a
  ;; hash-keyed SET of trigger maps (newest first, capped) so a multi-project
  ;; user alternating folders with DIFFERENT shim sets keeps each set warm
  ;; instead of thrashing one slot. Each set self-invalidates on preamble change.
  (delay (io/file (System/getProperty "user.home") ".vis" "cache" "shim-triggers.json")))

(def ^:private shim-triggers-cache-max
  ;; Keep at most this many distinct shim-sets (disk + memo). Bounds a user who
  ;; churns extensions; an evicted set just re-captures (~0.5s) or re-reads (~0.6ms).
  8)

(defonce ^:private shim-triggers-memo
  ;; Process-wide LRU memo: a newest-first VECTOR of `{:h hash :m trigger-map}`,
  ;; so the capture probe runs at most ONCE per process per shim-set (never per
  ;; Context / sub_loop fork). Bounded to `shim-triggers-cache-max` with TRUE
  ;; move-to-front eviction, mirroring the disk cache (a >max churner drops only
  ;; the OLDEST set, never the whole memo).
  (atom []))

(defn- shim-entries-hash
  "Content hash over every string-preamble shim (sorted by name), so the trigger
   map is recomputed only when a preamble's SOURCE (or the shim set) changes."
  ^String [entries]
  (sha256-hex (->> entries
                   (filter #(string? (:src %)))
                   (sort-by :sid)
                   (map (fn [{:keys [sid src]}]
                          (str sid "\u0000" src)))
                   (str/join "\u0001"))))

(defn- decode-trigger-map
  "JSON object -> `{sid {:provides [...] :autoload [...]}}`."
  [m]
  (reduce-kv (fn [acc sid v]
               (assoc acc
                 sid {:provides (vec (get v "provides")) :autoload (vec (get v "autoload"))}))
             {}
             m))

(defn- encode-trigger-map
  "`{sid {:provides [...] :autoload [...]}}` -> JSON-ready object."
  [m]
  (reduce-kv (fn [o sid v]
               (assoc o sid {"provides" (:provides v) "autoload" (:autoload v)}))
             {}
             m))

(defn- read-cache-entries
  "On-disk entry list `[{\"hash\" h \"map\" {...}} ...]` (newest first), or nil
   when missing / unreadable / legacy single-slot format."
  []
  (try (let [f ^java.io.File @shim-triggers-cache-file]
         (when (.exists f) (get (json/read-json (slurp f)) "entries")))
       (catch Throwable _ nil)))

(defn- read-shim-triggers-cache
  "Read the disk trigger map matching `expected-hash`; nil otherwise (missing /
   stale / unreadable). JSON, so no extra reader deps."
  [expected-hash]
  (some (fn [e]
          (when (= (get e "hash") expected-hash) (decode-trigger-map (get e "map"))))
        (read-cache-entries)))

(defn- write-shim-triggers-cache!
  "Best-effort persist of the trigger map under `expected-hash`, moved to FRONT
   of a capped, hash-keyed set (multi-project safe). Never throws."
  [expected-hash m]
  (try (let
         [f
          ^java.io.File @shim-triggers-cache-file

          kept
          (->> (read-cache-entries)
               (remove #(= (get % "hash") expected-hash))
               (take (dec (long shim-triggers-cache-max))))

          entries
          (cons {"hash" expected-hash "map" (encode-trigger-map m)} kept)]

         (.mkdirs (.getParentFile f))
         (spit f (json/write-json-str {"entries" (vec entries)})))
       (catch Throwable _ nil)))

(defn- capture-shim-triggers
  "Learn each shim's lazy TRIGGER names by RUNNING it, not parsing it. Build ONE
   throwaway probe Context on the shared engine, eval every string preamble once,
   and DIFF `sys.modules` + `builtins` before/after:
     :autoload - the names the shim STAPLES onto builtins (its deliberate public
                 surface: bare `<name>.attr` / `<name>(...)` with no import), and
     :provides - the top-level modules it publishes for `import <name>`, taken as
                 the new `sys.modules` keys that are ALSO stapled (or the shim's
                 own name). Intersecting with the staples is what discards the
                 shim's TRANSITIVE stdlib imports (requests pulling in `ssl`,
                 `email`, `datetime`, ...) so they never become bogus triggers.
   A shim with neither (e.g. `attach`) is not lazy-eligible => stays eager. The
   probe Context is disposed before returning. Best-effort per shim: a preamble
   that throws yields empty triggers (=> that shim installs eager) rather than
   breaking the whole capture."
  [entries]
  (let
    [ctx
     (-> (Context/newBuilder (into-array String ["python"]))
         (.engine ^Engine @shared-engine)
         (.allowAllAccess false)
         (.allowCreateThread true)
         (.allowNativeAccess false)
         (.allowPolyglotAccess PolyglotAccess/NONE)
         (.build))

     ^Value g
     (.getBindings ctx "python")]

    (try
      (install-auto-imports! ctx)
      (let
        [snap
         (fn []
           (->
             ^Value
             (.eval
               ctx
               "python"
               "__import__('json').dumps({'m':sorted(__import__('sys').modules.keys()),'b':sorted(vars(__import__('builtins')).keys())})")
             (.asString)
             json/read-json))

         top
         #(first (str/split % #"\."))]

        (reduce (fn [acc {:keys [sid src shim]}]
                  (if-not (string? src)
                    (assoc acc sid {:provides [] :autoload []})
                    (let [before (snap)]
                      (wire-shim-bindings! g shim)
                      (try (.eval ctx "python" ^String src) (catch Throwable _ nil))
                      (let
                        [after (snap)
                         new-mods (->> (set/difference (set (get after "m")) (set (get before "m")))
                                       (map top)
                                       (remove #{"builtins"})
                                       set)
                         autoload (vec (sort (set/difference (set (get after "b"))
                                                             (set (get before "b")))))
                         keep? (into #{sid} autoload)
                         provides (vec (sort (filter keep? new-mods)))]

                        (assoc acc sid {:provides provides :autoload autoload})))))
                {}
                entries))
      (finally (.close ctx true)))))

(defn- shim-trigger-map
  "The `{sid {:provides [...] :autoload [...]}}` lazy-trigger map for `entries`
   (each `{:sid :src :shim}`), derived by CAPTURE (`capture-shim-triggers`), then
   MEMOIZED process-wide and CACHED to disk keyed by a hash of the preambles. The
   ~0.5s capture cost is therefore paid at most ONCE per machine per preamble
   change - never per Context build or `sub_loop` fork, and never on a warm
   restart (the disk cache is read instead). On any failure it degrades to `{}`
   (=> every shim installs eager: correct, only not lazy)."
  [entries]
  (let [h (shim-entries-hash entries)]
    (locking shim-triggers-memo
      (if-let
        [m (some (fn [e]
                   (when (= (:h e) h) (:m e)))
                 @shim-triggers-memo)]
        (do ;; hit: move-to-front so it survives eviction (true LRU)
          (swap! shim-triggers-memo (fn [v]
                                      (vec (cons {:h h :m m} (remove #(= (:h %) h) v)))))
          m)
        (let
          [m (try (or (read-shim-triggers-cache h)
                      (let [computed (capture-shim-triggers entries)]
                        (write-shim-triggers-cache! h computed)
                        computed))
                  (catch Throwable t
                    (tel/log! {:level :warn :id ::shim-trigger-capture-failed}
                              (str "shim-trigger capture failed: " (or (.getMessage t) t)))
                    nil))]
          (when m
            (swap! shim-triggers-memo (fn [v]
                                        (->> v
                                             (remove #(= (:h %) h))
                                             (cons {:h h :m m})
                                             (take (long shim-triggers-cache-max))
                                             vec))))
          (or m {}))))))

(defn- install-sandbox-shims!
  "Install EVERY extension-contributed sandbox shim into `ctx` (main context AND
   every `sub_loop` fork), in registration order, BEFORE the baseline snapshot.

   LAZY-BY-DEFAULT: rather than eval every shim preamble here (which materializes
   each module's full live object graph into THIS context - the fat per-session
   baseline), install the central lazy runtime once, wire each shim's cheap host
   bindings eagerly, and register its trigger names. Each preamble is then eval'd
   ON DEMAND, once, on the first `import`/attribute touch (see
   `lazy-shim-runtime-python`). A shim with no derivable trigger (no importable
   module, no builtins staple - e.g. `attach`) stays EAGER, preserving its side
   effects. Best-effort: one shim's failure never breaks context creation."
  [^Context ctx ^Value g]
  (let
    [shims
     (registered-sandbox-shims)

     base
     (mapv (fn [shim]
             {:shim shim :src (shim-preamble-src shim) :sid (:shim/name shim)})
           shims)

     ;; Capture-derived (not regex-parsed) lazy triggers: MEMOIZED process-wide +
     ;; disk-cached, so the probe cost is paid once per machine, not per build.
     trig-map
     (shim-trigger-map base)

     entries
     (mapv (fn [{:keys [sid src] :as e}]
             (let
               [trig
                (get trig-map sid {:provides [] :autoload []})

                lazy?
                (boolean
                  (and sid (string? src) (or (seq (:provides trig)) (seq (:autoload trig)))))]

               (assoc e
                 :trig trig
                 :lazy? lazy?)))
           base)

     id->src
     (into {} (comp (filter :lazy?) (map (juxt :sid :src))) entries)]

    ;; Central lazy runtime + host loader callback - once, only if anything is lazy.
    (when (seq id->src)
      (try (.eval ctx "python" ^String lazy-shim-runtime-python)
           (.putMember g
                       "__vis_load_shim__"
                       (wrap-ifn (fn [sid]
                                   (when-let [s (get id->src (str sid))]
                                     (.eval ctx "python" ^String s))
                                   nil)))
           (catch Throwable t
             (tel/log! {:level :warn :id ::lazy-shim-runtime-failed}
                       (str "lazy-shim runtime failed to install: " (or (.getMessage t) t))))))
    (doseq [{:keys [shim src sid trig lazy?]} entries]
      (try (wire-shim-bindings! g shim)
           (if (and lazy? (contains? id->src sid))
             (do (.putMember g
                             "__vis_lazy_spec_json__"
                             (json/write-json-str
                               {"sid" sid "provides" (:provides trig) "autoload" (:autoload trig)}))
                 (.eval ctx "python" "__vis_register_lazy_shim__(__vis_lazy_spec_json__)")
                 (.putMember g "__vis_lazy_spec_json__" nil))
             (when (string? src) (.eval ctx "python" ^String src)))
           (catch Throwable t
             (tel/log! {:level :warn :id ::sandbox-shim-install-failed}
                       (str "sandbox shim '" (:shim/name shim)
                            "' failed to install: " (or (.getMessage t) t))))))))

(defonce ^:private ^java.util.Map ctx->stdout
  ;; Context -> the ByteArrayOutputStream its Python `print`/sys.stdout writes
  ;; into. `run-python-block` resets+reads it per form so a form's stdout is
  ;; surfaced to the MODEL (without it, `print(x)` returns None and the model
  ;; never sees the printed value → it re-runs or gives up — the GPT/Copilot
  ;; "kept repeating print without converging" loop). WeakHashMap so a disposed
  ;; Context's buffer is GC'd with it.
  (java.util.Collections/synchronizedMap (java.util.WeakHashMap.)))

(defn- ctx-stdout-baos ^java.io.ByteArrayOutputStream [^Context ctx] (.get ctx->stdout ctx))

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
  (and (instance? Value v) (str/starts-with? (str v) "<module ")))

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
   binary capability `allowHostSocketAccess` (always ON now): host sockets are always
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
    "def __vis_install_net_guard__():\n" "    import socket as _s\n"
    "    def _norm(x):\n" "        return str(x).strip().lower().rstrip('.').lstrip('.')\n"
    "    _allowed = set(_norm(d) for d in __vis_allowed_domains__ if _norm(d))\n"
    "    _denied  = set(_norm(d) for d in __vis_denied_domains__ if _norm(d))\n"
    "    _allow_specific = set(d for d in _allowed if d != '*')\n"
    "    _deny_specific  = set(d for d in _denied if d != '*')\n"
    "    _allow_star = ('*' in _allowed) or (len(_allowed) == 0)\n"
    "    _deny_star  = ('*' in _denied)\n"
    "    def _match(h, pats):\n" "        return any(h == d or h.endswith('.' + d) for d in pats)\n"
    "    def _host_ok(host):\n" "        h = _norm(host)\n"
    "        if _match(h, _deny_specific):\n" "            return False\n" ;; specific deny always wins (incl. both-specific)
    "        if _match(h, _allow_specific):\n" "            return True\n" ;; specific allow beats a '*' in the denylist
    "        if _deny_star:\n" "            return False\n"                ;; deny=* with no specific allow ⇒ block the rest
    "        return _allow_star\n"                                         ;; empty/'*' allowlist ⇒ allow; else block
    "    def _check(host):\n"
    "        if not _host_ok(host):\n"
    "            raise PermissionError(\"vis: network host '%s' is blocked (allowlist=%s, denylist=%s)\" % (host, sorted(_allowed) or ['*'], sorted(_denied)))\n"
    "    def _addr_host(address):\n"
    "        if isinstance(address, (tuple, list)) and address and isinstance(address[0], str):\n"
    "            return address[0]\n" ;; AF_INET/AF_INET6 (host, port, ...); AF_UNIX/str skipped
    "        return None\n"
    "    def _wrap_dns(orig):\n" "        def g(host, *a, **k):\n"
    "            _check(host); return orig(host, *a, **k)\n" "        return g\n"
    "    _s.getaddrinfo = _wrap_dns(_s.getaddrinfo)\n"
    "    _s.gethostbyname = _wrap_dns(_s.gethostbyname)\n"
    "    def _wrap_conn(orig):\n" "        def g(self, address, *a, **k):\n"
    "            h = _addr_host(address)\n" "            if h is not None: _check(h)\n"
    "            return orig(self, address, *a, **k)\n" "        return g\n"
    "    try:\n" "        _s.socket.connect = _wrap_conn(_s.socket.connect)\n"
    "        _s.socket.connect_ex = _wrap_conn(_s.socket.connect_ex)\n" "    except Exception:\n"
    "        pass\n" "    def _wrap_create(orig):\n"
    "        def g(address, *a, **k):\n" "            h = _addr_host(address)\n"
    "            if h is not None: _check(h)\n" "            return orig(address, *a, **k)\n"
    "        return g\n" "    try:\n"
    "        _s.create_connection = _wrap_create(_s.create_connection)\n" "    except Exception:\n"
    "        pass\n" "__vis_install_net_guard__()\n"))

(def ^:private proxy-env-python
  "Point the sandbox interpreter's HTTP stack at the gateway egress proxy.

   Routes urllib / requests / httpx / urllib3 through the ONE shared gateway proxy
   (loopback, `__vis_proxy_url__`) and trusts the shared ephemeral MITM CA
   (`__vis_ca_file__`, written under $TMPDIR so the confined FS can read it). The
   gateway proxy becomes the SINGLE policy engine: declarative `:network :rules`
   AND programmable `network_filter`s enforce host + verb + path over both HTTP and
   (MITM'd) HTTPS — the SAME boundary shell children and managed REPLs use. This
   RETIRES the old in-interpreter urllib method-guard (one engine, not two).

   Same THREAT MODEL as the socket host guard: COOPERATIVE (an env-var proxy), so a
   hand-rolled raw `socket` bypasses it — the host allow/deny guard remains the raw
   floor. Loopback is left DIRECT (`no_proxy`) so local dev servers keep working."
  (str
    "def __vis_install_proxy_env__():\n" "    import os as _o\n"
    "    _u = __vis_proxy_url__\n" "    _ca = __vis_ca_file__\n"
    "    for _k in ('http_proxy','https_proxy','HTTP_PROXY','HTTPS_PROXY','all_proxy','ALL_PROXY'):\n"
    "        _o.environ[_k] = _u\n"
    "    for _k in ('no_proxy','NO_PROXY'):\n"
    "        _o.environ[_k] = 'localhost,127.0.0.1,::1'\n"
    "    if _ca:\n"
    "        for _k in ('REQUESTS_CA_BUNDLE','SSL_CERT_FILE','CURL_CA_BUNDLE','PIP_CERT'):\n"
    "            _o.environ[_k] = _ca\n" "__vis_install_proxy_env__()\n"))

(def ^:private network-probe-python
  "In-sandbox network-filter DEV loop, wired onto the session globals as
   `network_filter(fn)` + `network_probe([method,] target)`. GUARD-ONLY: it
   exercises the egress DECISION (gateway host/verb/path/port + SSRF gate + every
   registered filter, plus the session's own local `network_filter`s) against a
   SYNTHETIC request via the `__vis_net_probe__` host callback — NO socket is
   opened and NOTHING is sent. Eval'd before the initial-ns snapshot so the names
   stay baseline (not surfaced as model-created live vars)."
  "__vis_net_filters__ = []
def network_filter(fn):
    \"\"\"Register a guard fn(ctx)->None|reason for `network_probe` (GUARD-ONLY: a
    session filter never affects LIVE egress — author a `.py` extension for that).
    ctx = {phase,method,host,path,port,headers}. Return None to allow; a string
    reason, or a dict like {'reason': ...}, to block; a raise fails CLOSED. Returns
    fn, so it also works as a decorator.\"\"\"
    __vis_net_filters__.append(fn)
    return fn
def __vis_run_local_filters__(ctx):
    import traceback as _tb
    out = []
    for fn in __vis_net_filters__:
        nm = getattr(fn, '__name__', 'filter')
        v = {'owner': nm, 'allow': True, 'reason': None, 'error': None}
        try:
            r = fn(dict(ctx))
            if r is None or r is False or r is True:
                pass
            elif isinstance(r, str):
                v['allow'] = False
                v['reason'] = r
            elif isinstance(r, dict) and (r.get('__vis_block__') or r.get('marker') == 'block' or r.get('reason')):
                v['allow'] = False
                v['reason'] = r.get('reason') or 'blocked'
        except Exception as _e:
            v['allow'] = False
            v['reason'] = 'filter crashed (fail-closed): %s' % _e
            v['error'] = {'message': str(_e), 'trace': _tb.format_exc()}
        out.append(v)
    return out
def network_probe(method, target=None):
    \"\"\"GUARD-ONLY egress probe (NEVER sends): evaluate the gateway host/verb/path/
    port + SSRF gate and every registered network filter (extension + your local
    `network_filter`s) over a SYNTHETIC request, printing each verdict + any Python
    traceback. Usage: network_probe('POST','https://api.github.com/repos') or a
    bare host[:port] for ssh/db, e.g. network_probe('db.host:5432').\"\"\"
    import json as _json
    if target is None:
        target, method = method, None
    rep = _json.loads(__vis_net_probe__(method or '', str(target)))
    if 'error' in rep:
        print('net-probe: ' + str(rep['error']))
        return rep
    ctx = rep['ctx']
    gw = rep['filters']
    loc = __vis_run_local_filters__(ctx) if rep['tier1']['allow'] else []
    rep['local_filters'] = loc
    if not rep['tier1']['allow']:
        final = {'allow': False, 'reason': rep['tier1']['reason']}
    else:
        gd = next((f for f in gw if not f['allow']), None)
        ld = next((f for f in loc if not f['allow']), None)
        if gd is not None:
            final = {'allow': False, 'reason': gd['reason']}
        elif ld is not None:
            final = {'allow': False, 'reason': ld['reason']}
        else:
            final = {'allow': True, 'reason': None}
    rep['final'] = final
    tgt = '%s %s%s:%s%s' % (str(rep['scheme']).upper(),
                            (str(ctx['method']) + ' ') if ctx.get('method') else '',
                            ctx['host'], ctx['port'], ctx['path'] or '')
    print('Target: ' + tgt)
    print('')
    print('Tier-1 (host / port / SSRF): ' + ('ALLOW' if rep['tier1']['allow'] else 'DENY \u2014 ' + str(rep['tier1']['reason'])))
    def _rows(label, fs):
        print('%s (%d):' % (label, len(fs)))
        if not fs:
            print('  (none registered)')
        for f in fs:
            line = '  \u2022 %s \u2192 %s' % (f['owner'], 'ALLOW' if f['allow'] else 'DENY')
            if (not f['allow']) and f['reason'] and not f['error']:
                line += ' \u2014 ' + str(f['reason'])
            if f['error']:
                line += '\\n      \u26a0 CRASHED (fail-closed): ' + str(f['error']['message'])
                if f['error'].get('trace'):
                    line += '\\n' + f['error']['trace']
            print(line)
    _rows('gateway network_filters', gw)
    _rows('local network_filters', loc)
    print('')
    print('FINAL: ' + ('ALLOW' if final['allow'] else 'DENY \u2014 ' + str(final['reason'])))
    return rep
")

(defn- make-outbox
  "Create a fresh per-context OUTBOX directory under the system temp dir and return
   `{:dir <abs path string> :on-close record-file!}` for
   `sandbox-fs/confined-filesystem`. The sandbox may WRITE files there
   (`$VIS_OUTBOX`); each file it closes is captured AT THE SOURCE as a
   `session_iteration_attachment` — the implicit twin of `vis_attach`, for a
   library that only knows how to write a file. Best-effort: on any failure returns
   nil (⇒ no outbox tap, the filesystem stays plain-confined)."
  []
  (try (let
         [dir (java.nio.file.Files/createTempDirectory
                "vis-outbox-"
                (make-array java.nio.file.attribute.FileAttribute 0))]
         {:dir (str (.toAbsolutePath dir))
          :on-close (fn [^java.nio.file.Path p]
                      (mpl-capture/record-file! p))})
       (catch Throwable _ nil)))


(def ^:private py-gc-task-specs
  "GraalPy background cycle-detector tuning, applied per agent Context to attack
   native-extension RSS that GraalPy reclaims lazily (the `BackgroundGCTask*` context
   options — INTERNAL, so they need experimental-options). Each spec is
   `[env-var graalpy-option lo hi default]`; the resolved value (env var, else the
   `default`) is parsed as an integer and CLAMPED into `[lo, hi]`, so a misconfigured
   env var can NEVER throw at Context build. A `nil` default means the option is applied
   ONLY when its env var is set (keep GraalPy's own default otherwise). Native-image-safe
   (runtime context options, not build-time host-VM flags).
     VIS_PY_GC_INTERVAL_MS  python.BackgroundGCTaskInterval  RSS-monitor interval, ms   (GraalPy default 1000)
     VIS_PY_GC_THRESHOLD    python.BackgroundGCTaskThreshold %% RSS growth between GCs   [1,100] (GraalPy default 30)
     VIS_PY_GC_MINIMUM_MB   python.BackgroundGCTaskMinimum   RSS floor, MEGABYTES        (vis default 2048 = 2 GB;
                                                                                          GraalPy's own is 4096)
   The MINIMUM floor defaults to 2 GB so the background collector engages from 2 GB up
   instead of GraalPy's dormant-until-4 GB default — the 'box shouldn't balloon' backstop."
  [["VIS_PY_GC_INTERVAL_MS" "python.BackgroundGCTaskInterval" 1 Integer/MAX_VALUE nil]
   ["VIS_PY_GC_THRESHOLD" "python.BackgroundGCTaskThreshold" 1 100 nil]
   ["VIS_PY_GC_MINIMUM_MB" "python.BackgroundGCTaskMinimum" 1 Integer/MAX_VALUE 2048]])

(defn- clamp-gc-value
  "Parse `raw` as an integer and CLAMP it into `[lo, hi]`, returning the clamped value as
   a string. Returns nil for a blank/unparseable input, so a bad env var contributes
   nothing (never a Context-build-breaking value). Pure — the unit-testable core of the
   GC-option guard."
  [raw lo hi]
  (when-let
    [s (some-> raw
               str
               str/trim
               not-empty)]
    (when-let [n (try (Long/parseLong s) (catch NumberFormatException _ nil))]
      (str (min (long hi) (max (long lo) (long n)))))))

(defn- resolve-py-gc-options
  "Read `py-gc-task-specs` from the environment into a `{graalpy-option value-str}` map,
   parsing + clamping each via `clamp-gc-value` (env var wins, else the spec `default`).
   An unset env var with a nil default contributes nothing, so the result is always safe
   to apply and can never make a Context build throw."
  []
  (into {}
        (keep (fn [[env-var opt lo hi default]]
                (when-let [v (clamp-gc-value (or (System/getenv env-var) default) lo hi)]
                  [opt v])))
        py-gc-task-specs))

(defn- apply-py-gc-options!
  "Apply the resolved GraalPy GC options to a `Context.Builder`. Values are pre-validated
   and clamped by `resolve-py-gc-options`, and the whole application is additionally
   wrapped so any unexpected option rejection falls back to the un-tuned builder rather
   than failing the sandbox build. By default it applies the 2 GB `BackgroundGCTaskMinimum`
   floor (see `py-gc-task-specs`); `VIS_PY_GC_*` env vars tune or extend it."
  ^Context$Builder [^Context$Builder builder]
  (let [opts (resolve-py-gc-options)]
    (if (empty? opts)
      builder
      (try (reduce-kv (fn [^Context$Builder b ^String k ^String v]
                        (.option b k v))
                      (.allowExperimentalOptions builder true)
                      opts)
           (catch Throwable _ builder)))))


(defn- build-agent-context
  "Build ONE deny-by-default GraalPy agent sandbox Context ON the shared `Engine`,
   wire `custom-bindings` (tool/verb fns as Python callables, values marshalled),
   install introspection, and return `{:python-context :sandbox-ns :initial-ns-keys}`.
   Shared by `create-python-context` (the main session sandbox) and `fork-context!`
   (each `sub_loop` child) so they are byte-for-byte the same sandbox — only the
   bound env (which ctx-atom the verbs close over) differs."
  [custom-bindings roots-fn network-opts stdin]
  (let
    [stdout-baos
     (java.io.ByteArrayOutputStream.)

     net?
     (boolean (:enabled? network-opts))

     ;; When the session routes interpreter egress through the gateway proxy (net on
     ;; + jail possible), these carry its loopback port + the shared MITM CA PEM (in
     ;; $TMPDIR, so the confined FS can read it). See `proxy-env-python`.
     proxy-port
     (:proxy-port network-opts)

     ca-file
     (:ca-file network-opts)

     allowed
     (vec (:allowed-domains network-opts))

     denied
     (into default-denied-domains (:denied-domains network-opts))

     ;; `*` (or an empty allowlist) ⇒ allow everything EXCEPT the denylist.
     allow-all?
     (or (empty? allowed) (some #(= "*" (str %)) allowed))

     ;; Install the guard whenever there is an actual restriction to enforce —
     ;; a denylist (always present: defaults) or a non-`*` allowlist. With net
     ;; off the socket capability is denied outright, so no guard is needed.
     guard?
     (and net? (or (seq denied) (not allow-all?)))

     ;; When proxying, urllib must reach the loopback proxy even under a restrictive
     ;; allowlist — so the host guard always permits loopback (the proxy itself
     ;; enforces the real host/verb/path policy). Raw sockets keep the full policy.
     guard-allowed
     (if proxy-port (into allowed ["127.0.0.1" "::1" "localhost"]) allowed)

     ;; Filesystem capability: when `roots-fn` is supplied, the sandbox gets
     ;; REAL filesystem access CONFINED to the current filesystem roots (Python
     ;; `open()` etc. work, but only under a root — see `sandbox-fs`). Without
     ;; it (tests / no workspace) the sandbox stays IO-NONE; the file tools do
     ;; the I/O on the Clojure side regardless.
     ;; NETWORK capability: host sockets are ALWAYS allowed (urllib/requests/socket
     ;; work). Containment is the gateway egress proxy the interpreter is pointed at
     ;; (see `proxy-env-python`), not an on/off capability; a non-empty
     ;; `:network/allowed-domains` allowlist further confines the guard installed below.
     outbox
     (when roots-fn (make-outbox))

     io-access
     (if (or roots-fn net?)
       (-> (IOAccess/newBuilder)
           (cond->
             roots-fn
             (.fileSystem (sandbox-fs/confined-filesystem roots-fn outbox)))
           (.allowHostSocketAccess net?)
           (.build))
       IOAccess/NONE)

     ctx
     (->
       (Context/newBuilder (into-array String ["python"]))
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
       ;; `vis python` (CLI) wires the human's REAL stdin so guest `sys.stdin`
       ;; works alongside `-c`/FILE (real-python semantics). Agent sandboxes
       ;; pass nil here and keep the default (no host stdin) — unchanged.
       (cond->
         stdin
         (.in ^java.io.InputStream stdin))
       ;; Optional GraalPy background cycle-detector tuning (native-ext RSS);
       ;; a no-op unless a VIS_PY_GC_* env var is set.
       (apply-py-gc-options!)
       (.build))

     _
     (.put ctx->stdout ctx stdout-baos)

     g
     (.getBindings ctx "python")]

    ;; Tiny stdlib conveniences as Python builtins (not globals):
    ;; `json.dumps(...)` and `shlex.quote(...)` work in every run_python
    ;; block without repeated imports.
    (install-auto-imports! ctx)
    ;; Tool fns + engine values (names snake-ified to Python-legal identifiers).
    (install-protected-names! g custom-bindings)
    (doseq [[sym val] (or custom-bindings {})]
      (let [member (if (fn? val) (wrap-ifn val) (->py val))]
        (.putMember g (sym->py-name sym) member)
        (doseq [alias (py-aliases-for-sym sym)]
          (.putMember g alias member))))
    ;; Sandbox self-discovery (apropos / doc) over the wired globals.
    (install-introspection! ctx)
    ;; Seed `__vis_docs__` so in-sandbox `doc(name)` returns each tool's real
    ;; description (from the extension registry) instead of a bare
    ;; "name (callable)". Keyed to the SAME Python member names the binding loop
    ;; above wired (canonical + aliases). Marshalled as JSON and parsed with the
    ;; auto-imported `json` module so no ProxyHashMap crosses the boundary.
    ;; Best-effort: a registry hiccup must never break context creation.
    (try (when-let
           [docs-fn (requiring-resolve 'com.blockether.vis.internal.extension/sandbox-symbol-docs)]
           (let
             [sym->doc (docs-fn)
              py-docs (reduce (fn [m [sym _]]
                                (if-let [d (get sym->doc sym)]
                                  (reduce #(assoc %1 %2 d)
                                          m
                                          (cons (sym->py-name sym) (py-aliases-for-sym sym)))
                                  m))
                              {}
                              (or custom-bindings {}))]

             (when (seq py-docs)
               (.putMember g "__vis_docs_json__" (json/write-json-str py-docs))
               (.eval
                 ctx
                 "python"
                 "globals().setdefault('__vis_docs__', {}).update(json.loads(__vis_docs_json__))")
               (.putMember g "__vis_docs_json__" nil))))
         (catch Throwable _ nil))
    ;; POSIX-compat: route subprocess / os.system to the shell tools. Eval'd
    ;; BEFORE the initial-ns-keys snapshot so any names it parks are baseline
    ;; (filtered out of the model-visible live-vars view).
    (install-posix-compat-shim! ctx g)
    ;; SANDBOX SHIMS: install every extension-contributed Python shim (host
    ;; bridge callables wired onto `g`, then the shim's preamble eval'd). This
    ;; is the GENERIC mechanism — `yaml` (YAMLStar) and `matplotlib` (Java2D)
    ;; ship as built-in shim extensions, third-party extensions can add more.
    ;; Eval'd BEFORE the snapshot so each shim's `__vis_*`/published-module
    ;; names are baseline (filtered out of the model-visible live-vars view).
    (install-sandbox-shims! ctx g)
    ;; Advertise the installed shims to the sandbox's OWN discovery surface:
    ;; `__vis_shims__` (a name list `apropos` folds in) + `__vis_docs__`
    ;; (name -> description, so `doc("yaml")` works even for an import-only
    ;; module that isn't a top-level global). Same JSON-hop marshalling as the
    ;; tool docs above. Best-effort: a registry hiccup must never break context
    ;; creation.
    (try (let
           [shims
            (registered-sandbox-shims)

            names
            (into [] (comp (keep :shim/name) (distinct)) shims)

            docs
            (reduce (fn [m s]
                      (if-let [nm (:shim/name s)]
                        (let
                          [d (:shim/description s)
                           base (if (and (string? d) (not (str/blank? d)))
                                  (str "sandbox shim \u2014 " d)
                                  (str "sandbox shim: a pre-installed `" nm "` module"))]

                          (assoc m nm base))
                        m))
                    {}
                    shims)]

           (when (seq names)
             (.putMember g "__vis_shims_json__" (json/write-json-str names))
             (.eval ctx "python" "globals()['__vis_shims__'] = json.loads(__vis_shims_json__)")
             (.putMember g "__vis_shims_json__" nil))
           (when (seq docs)
             (.putMember g "__vis_docs_json__" (json/write-json-str docs))
             (.eval
               ctx
               "python"
               "globals().setdefault('__vis_docs__', {}).update(json.loads(__vis_docs_json__))")
             (.putMember g "__vis_docs_json__" nil)))
         (catch Throwable _ nil))
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
    ;; OUTBOX: expose the per-context capture dir to Python as a `VIS_OUTBOX`
    ;; global and `$VIS_OUTBOX` env var. A file the sandbox WRITES there is
    ;; captured at the source as a durable iteration attachment (see the
    ;; `sandbox-fs` outbox tap) — the implicit twin of `vis_attach` for libraries
    ;; that only know how to write a file. Eval'd before the snapshot so
    ;; `VIS_OUTBOX` is a BASELINE name (not surfaced as a model-created live var).
    (when outbox
      (.putMember g "VIS_OUTBOX" ^String (:dir outbox))
      (try
        (.eval
          ctx
          "python"
          "import os as __vis_os__\n__vis_os__.environ['VIS_OUTBOX'] = VIS_OUTBOX\ndel __vis_os__")
        (catch Throwable _ nil)))
    ;; NETWORK domain allowlist: when sockets are on AND domains are specified,
    ;; patch socket DNS resolution to refuse hosts outside the allowlist. Eval'd
    ;; before the snapshot so the guard's names are BASELINE (not model-visible).
    (when guard?
      (.putMember g "__vis_allowed_domains__" (->py guard-allowed))
      (.putMember g "__vis_denied_domains__" (->py (vec denied)))
      (.eval ctx "python" network-guard-python))
    ;; NETWORK EGRESS ROUTING: point the interpreter's HTTP stack at the gateway
    ;; proxy + trust the shared MITM CA, so declarative :rules AND programmable
    ;; network_filters enforce host + verb + path (retires the urllib method-guard —
    ;; ONE policy engine). Eval'd before the snapshot so the env names stay baseline.
    (when (and net? proxy-port)
      (.putMember g "__vis_proxy_url__" (str "http://127.0.0.1:" proxy-port))
      (.putMember g "__vis_ca_file__" ^String (or ca-file ""))
      (.eval ctx "python" proxy-env-python)
      (.putMember g "__vis_proxy_url__" nil)
      (.putMember g "__vis_ca_file__" nil))
    ;; DEV network-filter loop: an in-sandbox `network_probe([method,] target)` +
    ;; `network_filter(fn)`. `__vis_net_probe__` runs the gateway egress gate + every
    ;; registered filter over a SYNTHETIC request — PURE: no socket, no egress, nothing
    ;; sent (see `python-extensions/net-probe-report`); the Python glue adds the
    ;; session's own local guards. Eval'd before the snapshot so the names stay
    ;; baseline. Best-effort: a resolve/eval hiccup must never break context creation.
    (try (when-let
           [report-fn (requiring-resolve
                        'com.blockether.vis.internal.python-extensions/net-probe-report)]
           (.putMember g
                       "__vis_net_probe__"
                       (wrap-ifn (fn [method target]
                                   (report-fn method target))))
           (.eval ctx "python" network-probe-python))
         (catch Throwable _ nil))
    (let
      [defer-names (->> (or custom-bindings {})
                        (filter (fn [[_ v]]
                                  (fn? v)))
                        (mapcat (fn [[sym _]]
                                  (cons (sym->py-name sym) (py-aliases-for-sym sym))))
                        (remove #{"session_fold" "__vis_par__" "__vis_par_isolated__"
                                  ;; ntr/native_tools_results host callbacks:
                                  ;; plain sync lookups, never awaitable thunks.
                                  "__vis_native_result_prime__" "__vis_native_result_fetch__"
                                  "__vis_native_result_ids__"})
                        distinct
                        vec)]
      (.putMember g "__vis_defer_names__" (->py defer-names))
      (.eval ctx "python" "__vis_defer_tools__()"))
    {:python-context ctx
     :sandbox-ns :python
     :initial-ns-keys (set (map str (seq (.getMemberKeys g))))}))

(defn- graalvm-version-major-minor
  "Extract the leading `MAJOR.MINOR` from a version-bearing string (e.g.
   \"25.1.3\" or \"GraalVM CE 25.0.2+10.1\" -> \"25.1\" / \"25.0\"). nil when no
   `\\d+.\\d+` is present."
  [s]
  (when s
    (some-> (re-find #"(\d+)\.(\d+)" s)
            rest
            (->> (str/join "."))
            not-empty)))

(defonce ^:private graalvm-runtime-checked
  ;; Runs the preflight ONCE per process (idempotent memoisation): the first
  ;; deref performs the check — throwing on a mismatch — and every later deref
  ;; is a no-op. Forced at the top of `create-python-context`, before any
  ;; polyglot Context/Engine is built.
  (delay
    ;; Skip in a native image (ONE runtime is baked in — no split possible) and
    ;; on a stock (non-GraalVM) JDK, where Truffle "unchained" runs on any
    ;; JDK 21+ with no built-in Truffle to collide with the Maven-pinned one.
    (when-not (System/getProperty "org.graalvm.nativeimage.imagecode")
      (let [vendor (str (System/getProperty "java.vendor.version"))]
        (when (str/includes? vendor "GraalVM")
          (let
            [pinned (some-> (io/resource "META-INF/graalvm/org.graalvm.polyglot/version")
                            slurp
                            str/trim
                            not-empty)
             ;; Parse the JDK's GraalVM version from the substring AFTER
             ;; "GraalVM" so a stray leading digit can't win the regex.
             jdk-ver (graalvm-version-major-minor (subs vendor (str/index-of vendor "GraalVM")))
             want (graalvm-version-major-minor pinned)]

            (when (and want jdk-ver (not= want jdk-ver))
              (throw
                (ex-info (str "vis embeds GraalPy/Truffle "
                              pinned
                              " but is running on "
                              vendor
                              ", whose built-in Truffle "
                              jdk-ver
                              ".x collides with it — every org.graalvm.polyglot "
                              "initializer would die with an opaque "
                              "NoClassDefFoundError (…IOHelper$ImplHolder).\n"
                              "Fix: run vis on a GraalVM matching "
                              want
                              ".x (the "
                              "graalvm-community-jdk-25i1 / graal-"
                              pinned
                              " build), "
                              "OR run --jvm on a stock (non-GraalVM) JDK 25 "
                              "(e.g. `sdk install java 25.0.3-tem`).")
                         {:vis/error :graalvm-version-mismatch
                          :pinned pinned
                          :jdk-vendor vendor
                          :want-line want
                          :found-line jdk-ver})))))))
    true))

(defn create-python-context
  "Create one persistent, deny-by-default GraalPy Context for a Vis session.

   `custom-bindings` maps symbols to tool/verb functions or values. `roots-fn`
   optionally grants filesystem access confined to the current workspace roots.
   Every session Context rides the process-wide `shared-engine`; parsing and
   extension shims live inside that same Context. Rendering is pure JVM code, so
   normal sessions allocate no auxiliary GraalPy contexts. Extensions
   deliberately SHARE this one session Context (installed as guest callables, not
   separate contexts) — so a session holds exactly one Context; the only extra is
   a transient `fork-context!` child, created solely for `sub_loop` parallelism.
   The 4-arity `stdin`
   (optional InputStream) is wired to the guest `sys.stdin` — used by `vis python`
   to forward the caller's real stdin; agent sandboxes leave it nil."
  ([custom-bindings] (create-python-context custom-bindings nil nil nil))
  ([custom-bindings roots-fn] (create-python-context custom-bindings roots-fn nil nil))
  ([custom-bindings roots-fn network-opts]
   (create-python-context custom-bindings roots-fn network-opts nil))
  ([custom-bindings roots-fn network-opts stdin]
   @graalvm-runtime-checked
   ;; Build/force the one process-wide Engine before the session Context. Child
   ;; sub-loops create their own restrictive Context only when true parallel
   ;; Python execution is requested; all contexts share this Engine's code cache.
   (try @shared-engine (catch Throwable _ nil))
   (build-agent-context custom-bindings roots-fn network-opts stdin)))

(defn fork-context!
  "Fork a CHILD agent Context for a `sub_loop` — same deny-by-default sandbox as
   the main context, built ON the shared `Engine` so it is SAFE to create even
   while the parent's eval is running (GraalVM-verified: no Truffle deadlock).
   `custom-bindings` wires the child's tool/verb fns, which close over the CHILD's
   env (its own ctx-atom). Returns the same
   `{:python-context :sandbox-ns :initial-ns-keys}` shape as
   `create-python-context`. The caller owns the child Context's lifecycle (close
   it when the sub_loop ends). `roots-fn` (optional) confines the child's Python
   filesystem to the current filesystem roots, same as the parent."
  ([custom-bindings] (fork-context! custom-bindings nil nil))
  ([custom-bindings roots-fn] (fork-context! custom-bindings roots-fn nil))
  ([custom-bindings roots-fn network-opts]
   (build-agent-context custom-bindings roots-fn network-opts nil)))

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
  {:source code :result (->clj (.eval ^Context python-context "python" (str code)))})

(defn collect-garbage!
  "Best-effort GC between turns. Two steps, because GraalPy reclaims
   native-extension (numpy/pandas/PIL) memory in TWO stages:
     1. guest `gc.collect()` runs the cycle detector, marking dead native cycles
        so their Java mirrors become weakly reachable, and
     2. a JVM `System.gc()` then lets the Java tracing GC collect those mirrors
        and drain the reference queue that actually frees the native RSS (see
        graalpython IMPLEMENTATION_DETAILS: the guest collect ALONE does not free
        non-cyclic native objects whose only managed ref was just dropped).
   Runs while the interpreter is idle between turns, the cheapest time for a
   pause. Never throws; a closed/cancelled context is ignored."
  [environment]
  (when-let [python-context (:python-context environment)]
    (try (.eval ^Context python-context "python" "import gc\ngc.collect()") (catch Throwable _ nil))
    (try (System/gc) (catch Throwable _ nil))))

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
  [python-context code]
  (let
    [first-real (->> (str/split-lines code)
                     (map str/trim)
                     (remove str/blank?)
                     (remove #(str/starts-with? % "#"))
                     first)]
    (when (and (seq first-real)
               (try (count-top-level-forms python-context first-real)
                    false ; parses alone → real code
                    (catch PolyglotException _
                      (boolean (or (re-find #"^(#{1,6}\s|[-*]\s|>\s)" first-real) ; heading/bullet/quote
                                   (re-find #"\*\*" first-real)                   ; **bold**
                                   (re-find #"[A-Za-z]{2,}\s+[A-Za-z]{2,}\s+[A-Za-z]{2,}"
                                            first-real)))))) ; sentence
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
  (let
    [d
     (dissoc d :tool-result)

     e
     (:error d)]

    (if-not (map? e)
      d
      (let [e' (not-empty (dissoc e :trace))]
        (if (or (nil? e') (= e' {:message message})) (dissoc d :error) (assoc d :error e'))))))

(defn- render-source-context
  "Babashka-style source excerpt for an eval failure: a numbered ±2-line window of
   `code` around the 1-based `line`, with a caret run under the offending span
   (`col`/`end-col`, 0-based offsets into the detabbed line — tabs collapse to one
   space so 1 char == 1 caret column). Returns nil when `line` is out of range, so
   a positionless failure leaves the raw message untouched."
  [code line col end-col]
  (let
    [lines
     (vec (str/split-lines (str code)))

     n
     (count lines)]

    (when (and line (<= 1 (long line) n))
      (let
        [detab
         (fn [s]
           (str/replace s "\t" " "))

         i0
         (dec (long line))

         lo
         (max 0 (- i0 2))

         hi
         (min (dec n) (+ i0 2))

         width
         (count (str (inc hi)))

         sb
         (StringBuilder.)]

        (doseq [idx (range lo (inc hi))]
          (let
            [pfx (str (format (str "%" width "d") (inc (long idx))) ": ")
             txt (detab (nth lines idx))]

            (.append sb pfx)
            (.append sb txt)
            (.append sb "\n")
            (when (= idx i0)
              (let
                [c0 (if (and col (<= 0 (long col) (count txt))) (long col) 0)
                 end (if (and end-col (> (long end-col) c0)) (long end-col) (inc c0))
                 ;; Snap the caret start off leading whitespace: a `co_positions`
                 ;; quirk reports the ENCLOSING handler's column for a
                 ;; `raise … from …` inside an `except`, landing the caret start
                 ;; in the indentation gutter. Advance to the first non-space
                 ;; within the span so the caret always begins on real code (a
                 ;; no-op when the reported column already points at a token).
                 c (or (first (filter #(and (< (long %) end) (not= \space (nth txt %)))
                                      (range c0 (min end (count txt)))))
                       c0)
                 end* (min (long end) (count txt))
                 pad (+ (count pfx) (long c))
                 span (max 1 (- end* (long c)))]

                (.append sb (apply str (repeat pad \space)))
                (.append sb (apply str (repeat span \^)))
                (.append sb "\n")))))
        (str/trimr (str sb))))))

(defn- vis-tool-error-host-cause
  "When `e` is a GUEST `__vis_ToolError__` — a foreign tool failure the driver
   WRAPPED so the sandbox can `except Exception` it — return the ORIGINAL host
   exception it carries as `__vis_orig__`. That lets an UNCAUGHT wrapped failure
   map to the same host tool-failure error (clean message + ex-data) as a bare
   host exception. nil for any other error."
  [^PolyglotException e]
  (try (when (.isGuestException e)
         (let [g (.getGuestObject e)]
           (when (and g (.hasMember g "__vis_orig__"))
             (let [orig (.getMember g "__vis_orig__")]
               (when (and orig (.isHostObject orig))
                 (let [h (.asHostObject orig)]
                   (when (instance? Throwable h) h)))))))
       (catch Throwable _ nil)))

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
  [python-context ^PolyglotException e code]
  (let
    [wrapped-host-cause
     (vis-tool-error-host-cause e)

     host?
     (or (.isHostException e) (some? wrapped-host-cause))

     cause
     (cond (.isHostException e) (.asHostException e)
           wrapped-host-cause wrapped-host-cause
           :else nil)

     loc
     (.getSourceLocation e)

     syntax?
     (and (not host?) (.isSyntaxError e))

     base
     (or (when cause (or (ex-message cause) (.getMessage ^Throwable cause))) (.getMessage e))

     ;; Prose-leading is the ROOT cause when the reply OPENS with prose (a `x`
     ;; in a leading sentence must be reported as PROSE, not "avoid x").
     ;; So check it FIRST. Non-ascii is the
     ;; fallback for a genuinely-code reply with a stray non-ASCII char mid-line
     ;; (CPython's "invalid character", precise wherever it lands - the
     ;; em-dash-at-line-71 case the first-line-only prose detector misses).
     prose-hint
     (when syntax? (prose-leading-syntax-hint python-context code))

     non-ascii?
     (boolean (and syntax? (not prose-hint) base (re-find #"invalid character" base)))

     ;; parse-diagnose heuristics, only when none of the structural detectors
     ;; above already explained the failure. Quote-balance first (an open
     ;; string makes the reader treat brackets as bare tokens, so its diagnosis
     ;; supersedes a bracket count), then bracket-balance.
     quote-hint
     (when (and syntax? (not prose-hint) (not non-ascii?))
       (:hint (parse-diagnose/diagnose-quote-balance code)))

     bracket-diag
     (when (and syntax? (not prose-hint) (not non-ascii?) (not quote-hint))
       (parse-diagnose/diagnose-bracket-balance code))

     bracket-hint
     (when bracket-diag
       (str (:hint bracket-diag)
            (when *auto-repair-brackets?*
              (when-let [fix (parse-diagnose/repair-bracket-balance code)]
                (str " Suggested fix: " (:change fix) ".")))))

     ;; Runtime `NameError: name 'X' is not defined`. The #1 cause of an
     ;; undefined TOOL name is an extension toggled OFF — the engine REMOVES
     ;; its symbols when inactive, so the call raises a plain NameError with
     ;; no hint that the tool merely needs enabling (e.g. a call to shell_run
     ;; while the "shell" toggle is off only yields "shell_run is not defined").
     ;; Point it at apropos + the user instead
     ;; of letting it retry a name that will never resolve on its own.
     undefined-name
     (when (and (not host?) (not syntax?) base)
       (second (re-find #"name '([^']+)' is not defined" (str base))))

     ;; Sandbox capability DENIAL — the model reached for the real filesystem /
     ;; native / OS (importlib.exec_module on a project file, open(), socket,
     ;; subprocess). GraalVM raises an OPAQUE `SecurityException: Operation is
     ;; not allowed for:` / `… was excluded`. Steer it to the tools that DO work.
     sandbox-denied?
     (boolean
       (and
         base
         (re-find
           #"Operation is not allowed for|Operation not permitted|PermissionError|was excluded|UnsupportedPosixFeature"
           (str base))))

     ;; A genuine FOREIGN/polyglot value — a tool result whose interop wrapper
     ;; really lacks that method (GraalPy: "foreign object has no attribute …").
     foreign-attr
     (when (and (not host?) base)
       (second (re-find #"foreign object has no attribute '([^']+)'" (str base))))

     ;; A dict-method call on a value GraalPy names by a base type — `str`,
     ;; `list`, etc. From the AttributeError text alone this is INDISTINGUISHABLE
     ;; between a genuine native value (a `python_execution` result in `ntr` is
     ;; its printed stdout STRING) and a FOREIGN tool-result row reported by its
     ;; element type (e.g. `await lst()` → a list). Capture [type attr] so the
     ;; steer NAMES the type and covers BOTH readings instead of asserting one.
     native-nondict
     (when (and (not host?) base)
       (next
         (re-find
           #"'(list|tuple|str|int|float|bool|NoneType|set)' object has no attribute '(get|items|keys|values)'"
           (str base))))

     ;; Python indentation slip (a block not indented, or a stray indent).
     indent?
     (boolean (and base
                   (re-find #"IndentationError|unexpected indent|expected an indented block"
                            (str base))))

     hint
     (cond prose-hint prose-hint
           non-ascii? (str "A non-ASCII character leaked into CODE position - it is only "
                           "legal inside a \"...\" string or a `#` comment. This is almost always "
                           "a smart em-dash, en-dash, curly quote, or x that you "
                           "meant as prose. Replace it with plain ASCII, or move that whole line "
                           "into a `#` comment. Original parser error: ")
           quote-hint (str quote-hint " Original parser error: ")
           bracket-hint (str bracket-hint " Original parser error: ")
           undefined-name
           (str "`"
                undefined-name
                "` is not defined. If it's a TOOL you expected, it is "
                "likely an extension that is inactive — its symbols are removed while off. Run "
                "`apropos(\""
                undefined-name
                "\")`; if it isn't listed, ask the USER to enable "
                "it and do NOT retry the name. If it's a variable, define it first. "
                "Original error: ")
           foreign-attr (str
                          "`." foreign-attr
                          "` failed because that value is a FOREIGN/polyglot object (a tool "
                          "result whose interop wrapper lacks that method). Read it with bracket "
                          "access (result[\"key\"]) or index it (result[0]); the result's shape is "
                          "in the tool's docstring. Original error: ")
           native-nondict
           (str "`." (second native-nondict)
                "` failed because that value is a `" (first native-nondict)
                "`, not a dict — it has no ." (second native-nondict)
                ". It is EITHER a FOREIGN tool result (a `" (first native-nondict)
                "` row / string whose interop wrapper has no dict method — index it "
                "(result[0]), or bracket-access a dict row: result[\"key\"]) OR a plain "
                "native value (a `python_execution` result in `ntr` is its printed stdout "
                "STRING). This is NOT a general polyglot restriction — the value simply "
                "isn't a dict. Guard the type first (e.g. `if isinstance(res, dict):`) "
                "before calling dict methods, or index/slice it directly. Original error: ")
           indent? (str "Python is INDENTATION-sensitive: a block (after def / if / for / with / "
                        "a trailing `:`) must be indented consistently (4 spaces), and a top-level "
                        "statement must start at column 0. Re-indent that region. Original error: ")
           sandbox-denied?
           (str "Your sandbox has NO real filesystem / native / process access — "
                "importlib + exec_module on a project file, open(), subprocess, and sockets "
                "CANNOT run here. To READ a project file use cat(path); to RUN project code "
                "(import its modules, use its deps) use repl_eval(language, code) — that runs "
                "in the project's interpreter where the file is importable. Original error: "))

     ;; FAILING SOURCE POSITION, babashka-style. A runtime error's top-level
     ;; PolyglotException loses its guest frames to the async trampoline, so the
     ;; Python side stashed the deepest user-code frame in `__vis_err_pos__`
     ;; ([line col end-col], col 0-based); a syntax/compile error carries it on
     ;; `loc` (1-based). Runtime pos WINS over `loc` (nil or shallow-wrong there).
     err-pos
     (when (and (not host?) (not syntax?))
       (try (->clj (.getMember (.getBindings ^Context python-context "python") "__vis_err_pos__"))
            (catch Throwable _ nil)))

     pos
     (cond (and (sequential? err-pos) (first err-pos)) [(long (nth err-pos 0))
                                                        (some-> (nth err-pos 1)
                                                                long)
                                                        (some-> (nth err-pos 2)
                                                                long)]
           (some? loc) [(.getStartLine loc) (max 0 (dec (.getStartColumn loc))) nil]
           :else nil)

     source-context
     (when pos (render-source-context code (nth pos 0) (nth pos 1) (nth pos 2)))

     msg
     (cond-> (if hint (str hint base) base)
       source-context
       (str "\n\n" source-context))]

    {:message msg
     :data (cond->
             {:phase (cond host? :python/host
                           syntax? :python/syntax
                           :else :python/runtime)}
             pos
             (assoc :line
               (nth pos 0) :column
               (if-let [c (nth pos 1)]
                 (inc (long c))
                 1))

             non-ascii?
             (assoc :non-ascii-in-code? true)

             prose-hint
             (assoc :prose-leading? true)

             quote-hint
             (assoc :unbalanced-quote? true)

             bracket-diag
             (assoc :unbalanced-bracket? true)

             undefined-name
             (assoc :name-undefined?
               true :undefined-name
               undefined-name)

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

;; ── Per-block eval instrumentation (memory-leak observability) ──────────────
;; Every python_execution block is compiled to a FRESH GraalPython code unit
;; (`exec(compile(mod,'<prog>','exec'), g)` inside `__vis_run_async__`), and
;; BytecodeDSL attaches the source AST to each compiled unit, so a per-block
;; leak would show as the OLD-GEN FLOOR rising across full GCs. This counter +
;; periodic sample makes that visible: watch `old` (old-gen used) and `gc` climb
;; together in lock-step with `blocks`. Transient `heap-used` SAWTOOTHS and is
;; NOT a leak signal on its own. Cheap — JMX pool totals, no heap histogram.
;; Cadence via VIS_PY_BLOCK_LOG_EVERY (default 25; 0 disables).
(defonce ^:private py-block-count (atom 0))

(defonce ^:private py-block-prev-heap (atom 0))

(defonce ^:private py-block-prev-n (atom 0))

(defn- py-block-log-every
  "Sample cadence (blocks between heap logs) from VIS_PY_BLOCK_LOG_EVERY; 25 by
   default, 0 disables logging, malformed input falls back to 25."
  ^long []
  (let [raw (System/getenv "VIS_PY_BLOCK_LOG_EVERY")]
    (if (str/blank? raw) 25 (try (max 0 (Long/parseLong (str/trim raw))) (catch Exception _ 25)))))

(defn- mem-log-enabled?
  "Master switch for memory-observability logging: the per-block heap sample here
   AND the env-reaper sweep summary in `internal.loop`. Enabled unless VIS_MEM_LOG
   is a falsey token (0/false/off/no) — one flag silences every memory log."
  []
  (let
    [raw (some-> (System/getenv "VIS_MEM_LOG")
                 str/trim
                 str/lower-case)]
    (not (contains? #{"0" "false" "off" "no"} raw))))

(defn- old-gen-used
  "Live bytes in the tenured/old generation — the leak-truthful floor (rises
   across full GCs only when objects are genuinely retained). 0 if no old pool."
  ^long []
  (reduce (fn [^long acc ^java.lang.management.MemoryPoolMXBean p]
            (let [nm (.getName p)]
              (if (and nm (re-find #"(?i)old|tenured" nm)) (+ acc (.getUsed (.getUsage p))) acc)))
          0
          (java.lang.management.ManagementFactory/getMemoryPoolMXBeans)))

(defn- gc-count
  "Total GC collections across all collectors (so the reader can tell a rising
   old-gen floor apart from pre-GC churn)."
  ^long []
  (reduce (fn [^long acc ^java.lang.management.GarbageCollectorMXBean g]
            (let [c (.getCollectionCount g)]
              (if (neg? c) acc (+ acc c))))
          0
          (java.lang.management.ManagementFactory/getGarbageCollectorMXBeans)))

(defn- cpu-pcts
  "Process + whole-system CPU load as whole percents (0–100; -1 when the JVM can't
   sample the interval yet) plus the OS 1-minute load average. Uses com.sun's
   OperatingSystemMXBean when present; never throws. Cheap — no allocation."
  []
  (let
    [os
     (java.lang.management.ManagementFactory/getOperatingSystemMXBean)

     pct
     (fn [^double v]
       (if (>= v 0.0) (Math/round (* v 100.0)) -1))]

    (if (instance? com.sun.management.OperatingSystemMXBean os)
      (let [^com.sun.management.OperatingSystemMXBean sun os]
        [(pct (.getProcessCpuLoad sun)) (pct (.getSystemCpuLoad sun)) (.getSystemLoadAverage os)])
      [-1 -1 (.getSystemLoadAverage os)])))

(defn- log-block-eval!
  "Count one executed block and emit a heap sample: an immediate baseline on the
   FIRST block, then every Nth. Auto-enabled — cadence 25 with NO env var needed;
   set VIS_PY_BLOCK_LOG_EVERY=0 to silence. Called from a `finally` so both
   successful and error blocks count (the compile happens regardless)."
  []
  (let
    [n
     (swap! py-block-count inc)

     every
     (py-block-log-every)]

    (when (and (mem-log-enabled?) (pos? every) (or (= n 1) (zero? (rem (long n) every))))
      (let
        [^Runtime rt
         (Runtime/getRuntime)

         used
         (- (.totalMemory rt) (.freeMemory rt))

         oldb
         (old-gen-used)

         gcs
         (gc-count)

         max-m
         (.maxMemory rt)

         prev
         @py-block-prev-heap

         prev-n
         @py-block-prev-n

         window
         (long (max 1 (- (long n) (long prev-n))))

         delta
         (- used (long prev))

         mb
         (fn [^long b]
           (quot b 1048576))

         per-blk-kb
         (quot delta (* window 1024))

         [cpu-proc cpu-sys cpu-raw]
         (cpu-pcts)

         cpu-load
         (double (/ (Math/round (* (double cpu-raw) 100.0)) 100.0))]

        (reset! py-block-prev-heap used)
        (reset! py-block-prev-n n)
        (let
          [msg
           (format
             "python-block-eval blocks=%d heap=%dMB/%dMB old=%dMB gc=%d cpu=proc%d%%/sys%d%% load=%s Δ=%+dMB (~%+dKB/block over last %d)"
             n
             (mb used)
             (mb max-m)
             (mb oldb)
             gcs
             cpu-proc
             cpu-sys
             cpu-load
             (mb delta)
             per-blk-kb
             window)]
          ;; Direct file append: the gateway's telemere handler is async :dropping and
          ;; emits no internal.* file lines, so bypass it for the leak trace — this line
          ;; is guaranteed visible in ~/.vis/vis-pyblock.log even while the JVM is pegged.
          (try (spit (str (System/getProperty "user.home") "/.vis/vis-pyblock.log")
                     (str (java.time.Instant/now) "  " msg "\n")
                     :append
                     true)
               (catch Exception _ nil))
          (tel/log! {:level :info
                     :id ::python-block-eval
                     :data {:blocks n
                            :heap-used-mb (mb used)
                            :heap-max-mb (mb max-m)
                            :heap-delta-mb (mb delta)
                            :approx-kb-per-block per-blk-kb
                            :sample-window window
                            :old-gen-mb (mb oldb)
                            :gc-count gcs
                            :cpu-proc-pct cpu-proc
                            :cpu-sys-pct cpu-sys
                            :load-avg cpu-load}}
                    msg))))))


(defn- run-async-program
  "Run the program as ONE driven coroutine. `__vis_run_async__` AST-wraps it in
   an `async def` (with `global` decls for its assigned names so they persist in
   the interpreter), the trampoline `__vis_drive__` drives it, and `gather`
   overlaps awaitables on the host virtual-thread pool. Returns the FLAT sum
   `{:stdout <printed>}` | `{:result <value>}` | `{:error <raised> :stdout?}`."
  [^Context ctx ^Value g code]
  (let
    [baos
     (ctx-stdout-baos ctx)

     _
     (when baos (.reset baos))

     ;; (The per-block print-capture list is reset INSIDE `__vis_run_async__` as
     ;; a real python list — resetting it from here with `->py []` would make it
     ;; a non-appendable ProxyArray and lose every capture.)
     run-async
     (.getMember g "__vis_run_async__")

     read-out
     (fn []
       (when baos
         (let [s (baos->str baos)]
           (when-not (str/blank? s) s))))

     ;; The tool-result objects the model print()ed this block — each a map
     ;; carrying "op" (its origin). The HOST renders one op-card per result;
     ;; stdout (context) is untouched.
     read-printed
     (fn []
       (let [p (->clj (.getMember g "__vis_printed_results__"))]
         (when (seq p) (vec p))))

     sink
     (atom [])

     outbox-seen
     (atom #{})]

    (with-bindings {@current-form-idx-var 0
                    #'mpl-capture/*attachment-sink* sink
                    #'mpl-capture/*outbox-seen* outbox-seen}
      (try
        ;; Run the whole-block coroutine; it stashes the program's value in
        ;; `__vis_async_result__` and prints to `baos`. (Globals it assigns
        ;; persist NATURALLY in the live interpreter — no pickle, no rebind.)
        (.execute run-async (object-array [code]))
        (let
          [res0
           (->clj (.getMember g "__vis_async_result__"))

           res
           (if (module-value? res0) nil res0)

           out
           (read-out)

           printed
           (read-printed)

           ;; true ⇔ the block printed NOTHING but tool results — only then may
           ;; the human display replace the raw stdout with cards (no text lost).
           only?
           (true? (->clj (.getMember g "__vis_only_results__")))

           ;; Artifacts the block PRODUCED (matplotlib show/savefig, vis_attach,
           ;; or an $VIS_OUTBOX write), captured at the source into the per-block
           ;; sink — folded in as `:attachments` so the loop OWNS the bytes with
           ;; NO stdout-fence parsing.
           attachments
           (mpl-capture/drain sink)]

          (.putMember g "__vis_async_result__" nil) ;; clear stash for the next turn
          ;; FLAT sum type — success is ONE CONTEXT channel, never both:
          ;;   - printed output (`:stdout`) → the python_execution result; OR
          ;;   - the returned value (`:result`) → a native tool call (never prints).
          ;; Printed output WINS. `:printed-results` rides ALONGSIDE `:stdout` —
          ;; it is DISPLAY-only (cards), NOT a second context channel. `:attachments`
          ;; ride alongside EITHER — a produced-artifact channel, not context.
          (if out
            (cond-> {:stdout out}
              attachments
              (assoc :attachments attachments)

              printed
              (assoc :printed-results printed)

              (and printed only?)
              (assoc :only-printed-results? true))
            (cond-> {}
              attachments
              (assoc :attachments attachments)

              (some? res)
              (assoc :result res))))
        (catch PolyglotException e
          ;; FLAT sum type — failure branch. The raised error IS the result, in
          ;; ONE place; any partial stdout (and any artifact produced before it)
          ;; rides along.
          (let
            [out
             (read-out)

             attachments
             (mpl-capture/drain sink)]

            (cond-> {:error (map-polyglot-error ctx e code)}
              out
              (assoc :stdout out)

              attachments
              (assoc :attachments attachments))))
        (finally (log-block-eval!))))))

(declare protected-rebind-error)

(defn- strip-protected-imports
  "AST-strip imports of protected sandbox builtins (e.g. `from asyncio import
   gather`) from `code` via the `__vis_strip_protected_imports__` preamble
   helper, so a redundant import of a builtin becomes a no-op instead of a
   protected-rebind error (and never shadows the builtin). Returns the original
   code unchanged when nothing is stripped or on any failure."
  [^Context ctx ^Value g code]
  (try (.putMember g "__vis_src__" (str code))
       (let [v (.eval ctx "python" "__vis_strip_protected_imports__(__vis_src__)")]
         (if (and v (.isString v)) (.asString v) (str code)))
       (catch Throwable _ (str code))))

(defn- empty-block-error
  "Op-error when `code` has NO top-level statements — only comments/whitespace, or
   everything got stripped (e.g. a lone `from asyncio import gather`). Returns nil
   otherwise. Replaces the leaked async-wrapper internal
   `ValueError: empty body on AsyncFunctionDef` with a clean, actionable message.
   A parse failure (invalid syntax) is NOT empty — swallow it and return nil so the
   normal evaluator surfaces the precise `:python/syntax` error."
  [^Context ctx code]
  (when (zero? (long (try (count-top-level-forms ctx code) (catch Throwable _ -1))))
    {:message (str "Empty block — nothing to execute. The code is only comments or "
                   "whitespace, so this iteration produces no evidence. Write at least "
                   "one statement, and print() what you want back.")
     :data {:phase :python/empty-block :empty-block? true}}))

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
  (let
    [ctx
     ^Context python-context

     g
     (.getBindings ctx "python")

     ;; Strip redundant imports of protected builtins (e.g. `from asyncio
     ;; import gather`) at the AST level BEFORE the protected-rebind check and
     ;; before running — so they're a silent no-op, not an error.
     code
     (strip-protected-imports ctx g code)]

    (if-let [err (or (empty-block-error ctx code) (protected-rebind-error ctx g code))]
      {:result nil :forms [{:source code :error err}] :error err}
      ;; ONE whole-block path. `__vis_run_async__` AST-wraps the program in an
      ;; `async def`, AUTO-SETTLES every bare top-level tool call (so `cat(x)`
      ;; without `await` still RUNS), drives it as a single coroutine, and reports
      ;; any error against the WHOLE source. The block runs as the model wrote it,
      ;; so its outcome is the flat `{:stdout}` | `{:result}` | `{:error}` sum.
      (run-async-program ctx g code))))

(defn- assigned-names-in-code
  "Top-level names a Python block DURABLY binds (assign/import/def targets).
   Transient `for`/`with` loop targets are excluded (they stay function-local).
   Empty on parse failure; the normal evaluator reports the syntax error."
  [^Context ctx ^Value g code]
  (try (.putMember g "__vis_src__" (str code))
       (let
         [v
          (.eval ctx "python" "__vis_assigned_names__(__import__('ast').parse(__vis_src__).body)")]
         (set (map str (or (->clj v) []))))
       (catch PolyglotException _ #{})
       (catch Throwable _ #{})))

(defn- protected-rebind-error
  [^Context ctx ^Value g code]
  (let
    [protected
     (set (map str (or (->clj (.getMember g "__vis_protected_names__")) [])))

     hits
     (vec (sort (set/intersection protected (assigned-names-in-code ctx g code))))]

    (when (seq hits)
      {:message (str "Block tries to rebind protected sandbox/tool name(s): "
                     (str/join ", " hits)
                     ". Tool names are read-only in run_python; choose a different variable name. "
                     "This prevents shadowing a callable (e.g. patch) with data and later seeing "
                     "'str' object is not callable.")
       :data {:phase :python/protected-name :protected-name? true :names hits}})))

;; =============================================================================
;; Engine-owned sandbox names + restore (NOOP)
;; =============================================================================

(def SYSTEM_VAR_NAMES "Engine-owned symbols hidden from user live-var listings." '#{session})

(defn system-var-sym? [sym] (contains? SYSTEM_VAR_NAMES sym))

(defn restore-sandbox!
  "NOOP. The session has ONE persistent interpreter; globals (defs/imports/vars)
   persist NATURALLY across turns, so there is nothing to restore."
  [_python-context _db-info _session-id]
  [])
