(ns com.blockether.vis.internal.env-python
  "Embedded-GraalPy sandbox machinery — the agent's action substrate. The agent
   writes **Python**; this ns embeds a GraalPy `org.graalvm.polyglot.Context`,
   marshals values across the Clojure↔Python boundary, wires the Clojure tool
   fns into the Python globals as `ProxyExecutable`s (so `grep({\"query\": \"x\"})` in Python
   runs the Clojure `grep`), and runs the model's code block as ONE whole-block
   coroutine.

   Public surface used by the loop:

     create-python-context / set-python-binding! / bind-and-bump! /
     bind-and-bump-with-doc! / count-top-level-forms / validate-non-empty-block! /
     validate-no-banned-defs! / persist-session-defs! / restore-session-defs! /
     SYSTEM_VAR_NAMES /
     system-var-sym? / *lru-atom* / *current-turn-position* / fresh-lru-atom /
     run-python-block / map-polyglot-error / bind-ctx! / ctx->python-str

   The `:python-context` slot holds the GraalPy `Context`; the Python top scope is
   `context.getBindings(\"python\")`. GraalPy ships in the default deps (runs on
   GraalVM CE 25.1.3 → Truffle gets the Graal JIT; see .graalvm-version)."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.blockether.vis.internal.doc-corpus :as doc-corpus]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.foundation.mpl-capture :as mpl-capture]
            [com.blockether.vis.internal.parse-diagnose :as parse-diagnose]
            [com.blockether.vis.internal.sandbox-fs :as sandbox-fs]
            [com.blockether.vis.internal.sandbox-resources :as res]
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
   handed to `->py`, so the offending producer field is nameable.

   The message ALWAYS says WHERE: an empty path means the offending key sits on
   the value handed to the boundary itself, and a bare `:result`/`:success?`
   there is vis' own internal envelope — the producer must hand Python the
   PAYLOAD (`:result`), never the envelope that wraps it. Without that clause a
   report reads `non-string-key :result` with no location at all."
  [kind x path]
  (throw (ex-info (str "STRINGS-ONLY boundary violation: "
                       (name kind)
                       " "
                       (pr-str x)
                       (if (seq path)
                         (str " at path " (pr-str (vec path)))
                         (when (= :non-string-key kind) " at the TOP-LEVEL map key"))
                       " cannot cross Clojure->Python. Build boundary maps with"
                       " string keys and stringify enum values at the source."
                       (when (and (= :non-string-key kind) (contains? #{:result :success?} x))
                         (str " This looks like vis' INTERNAL result envelope:"
                              " hand Python the payload under :result, never the envelope.")))
                  {:vis/boundary-violation kind :value x :path (vec path)})))

(defn normalize-dict-key
  "Model-input hygiene at the ONE inbound conversion: a dict key spelled
   `\":target\"` is still a STRING (the model drifting into colon
   spelling while reading keyword-heavy source), so strip the single leading
   colon when an identifier char follows and the call just works — no
   lecture, no failure. Data keys are untouched: line numbers start with a
   digit, paths with a letter or `/`, neither with `:`. Produces
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
     is null` (session 9c829d10, `list_sessions()`). ISO-8601 string instead.
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
        ;; ordered `LinkedHashMap` a tool returns (e.g. `ls`'s entries). The new
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
   types); collections become polyglot proxies, which GraalPy shows as
   `ForeignDict`/`ForeignList` — subscript, `len`, iteration, `.keys()`, `dict(_)`,
   `{**_}` and even `isinstance(_, dict)` behave, but `json.dumps` refuses them
   (it dispatches on the exact type), so a value the guest must SERIALIZE is
   rebuilt by `__vis_pyify__` or handed over as a JSON string;
   leaves convert via `leaf->py` (UUID/Temporal/Date -> ISO strings)."
  [x]
  (->py* x []))

(defn- py-fspath
  "The filesystem STRING behind a Python `os.PathLike` — `pathlib.Path`,
   `os.DirEntry`, anything answering `__fspath__` — or nil for every other
   value. Asks Python's OWN duck-type (the one `os.fspath` asks) over interop
   instead of sniffing a type name, so a `PurePosixPath`, a subclass and a
   user-defined PathLike all answer alike. Total: a `__fspath__` that raises or
   hands back bytes answers nil, and the value stays whatever it was."
  ^String [^Value v]
  (try (when (.hasMember v "__fspath__")
         (let [^Value s (.invokeMember v "__fspath__" (object-array 0))]
           (when (.isString s) (.asString s))))
       (catch Throwable _ nil)))

(defn ->clj
  "Polyglot `Value` (a Python value) -> Clojure data. STRINGS-ONLY boundary:
   dicts -> maps with VERBATIM STRING keys (exactly what Python held — no
   keywordizing, no regex key-shape sniffing), lists/tuples -> vectors, host
   objects (Java values that crossed the boundary, e.g. UUIDs) -> their
   underlying Java value via `asHostObject`, callables/opaque objects -> the
   raw `Value`. A non-string Python dict key (int, tuple, ...) stringifies via
   its Clojure conversion so the map stays string-keyed and total. An
   `os.PathLike` (`pathlib.Path`, `os.DirEntry`, ...) crosses as its filesystem
   STRING at any depth, so `cat(root / 'q.clj')` is the same call as `cat` on
   the path text — see `py-fspath`."
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
        ;; tool result (`ls`'s nested entries) comes back HASH-ordered and
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
        ;; A `pathlib.Path` is a PATH the model happened to spell as an object,
        ;; and every tool that takes one takes a string. Left opaque it reached
        ;; the tool as the raw `Value` whose `toString` is the Python REPR, so
        ;; `cat(root / 'q.clj')` refused with `File not found:
        ;; ~/vis/PosixPath('/…/q.clj')` and `grep` called the same argument a
        ;; non-string path.
        :else (or (py-fspath v) v)))

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
   short names (currently `find_files`/`find` for `grep`), but the snake name remains
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
    grep
    ["find_files" "find"]

    find_files
    ["find"]

    ;; `grep` is canonical; `find_files`/`find` stay as compatibility aliases.
    ;; Keep the older symbol branch while contexts/extensions may still expose it.
    []))

(defn python-binding-names
  "Canonical Python global plus intentional compatibility aliases for `sym`.
   Used by provider/native discovery to deduplicate the same capability."
  [sym]
  (into [(sym->py-name sym)] (py-aliases-for-sym sym)))

(defn- wrap-ifn
  "Wrap a Clojure fn as a Python-callable `ProxyExecutable`. Positional Python
   args are marshalled to Clojure, the fn is applied, and the result marshalled
   back to Python. Matches vis's positional-args tool contract.

   A host `NullPointerException` from the tool body is an INTERNAL engine/tool
   bug, never the model's Python. Left raw it surfaces as a bare Java `Cannot
   invoke ... because ... is null` naming some private local (`row`), reads like
   the model's OWN error, and gets retried verbatim forever. Catch it AT THE
   BOUNDARY and rethrow a labelled, catchable `ex-info` that says so, keeping the
   original null message so the surfacing hint (`host-npe?`) still fires. A
   genuine Python fault (NameError, TypeError, ValueError) is raised by the guest
   and never reaches here, so those still terror through unchanged."
  ^ProxyExecutable [f]
  (reify
    ProxyExecutable
      (execute [_ args]
        (->py (try (apply f (map ->clj args))
                   (catch NullPointerException e
                     (throw (ex-info (str
                                       "internal tool fault: a vis tool binding hit a Java "
                                       "NullPointerException (" (.getMessage e)
                                       ") — a vis "
                                       "engine/tool bug, NOT your Python. Retrying the same call "
                                       "fails identically; take a different approach or report it "
                                       "to the user.")
                                     {:type :vis/host-null-tool-fault :npe-message (.getMessage e)}
                                     e))))))))

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

(defn ^:no-doc runtime-python-src
  "Python source of a RUNTIME helper, slurped from its CLASSPATH RESOURCE under
   `vis-python/` (`resources/vis-python/*.py`). Same contract as
   `extension/shim-src` for sandbox shims: the Python body is NEVER a Clojure
   string - it is real, lintable, diffable Python in a real `.py` file - and is
   embedded in the native image by build.clj's
   `-H:IncludeResources=vis-python/.*`. Keeping these bodies out of the
   class file also keeps every one of them clear of the JVM's 65535-byte
   UTF-8 constant-pool ceiling, which the async runtime source had outgrown.
   Throws when the resource is missing - a body that did not make it onto the
   classpath must fail loudly, not install a silently empty runtime."
  ^String [res]
  (if-let [u (io/resource res)]
    (slurp u)
    (throw (ex-info (str "runtime Python source not found on classpath: " res) {:resource res}))))

(def ^:private async-runtime-python
  "ASYNC-BY-DEFAULT runtime (maki-style, on GraalPy — no asyncio/select/socket).

   Tools are DEFERRED: calling `grep(spec)` returns a `__vis_Call__` thunk instead
   of running. You drive them three ways:
     • `await grep(spec)`                         — canonical, anywhere nested
     • `a, b = await gather(grep(x), grep(y))`    — CONCURRENT on bounded host workers
     • a bare `grep(spec)` / `x = grep(y)` STATEMENT  — auto-SETTLED in place, at
   EVERY depth: `__vis_settle__` wraps each top-level assign/expr and
   `__vis_settle_stmt__` each nested one, so a call inside `try:` / `for:` / a
   `def` body runs where it is written instead of leaving an unrun thunk. An
   `async def` body is the ONE exception: a coroutine is where HOLDING an
   awaitable (`t = asyncio.to_thread(f, x)` then `await gather(t, u)`) is the idiom

   `await` works because run-python-block AST-wraps an await-bearing program in
   an `async def` (GraalPy rejects top-level await), declares its assigned names
   `global` so REPL vars still persist, and drives the coroutine with the
   trampoline `__vis_drive__`. `gather` dispatches its awaitables to the
   host bounded platform pool `__vis_par__` (bound from Clojure) — real overlap on
   blocking tool I/O, no event loop. A call in EXPRESSION position — an argument,
   a comprehension element, a `return` value — stays DEFERRED, and that is the
   seam `gather` batches through; it repr's a loud hint if it leaks into output.
   `print(...)` settles a deferred call/gather handed straight to it (so
   `print(rg(x))` shows the real result), and inline subscript / `len` / `in`
   settle that ONE call in place. Every settle path raises the SAME catchable
   `__vis_ToolError__`, so a tool refusal is caught by the `except Exception:`
   the block wrote around it instead of escaping that handler."
  (runtime-python-src "vis-python/async_runtime.py"))



(def polyglot-noise-silenced
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
  ;;
  ;; A `delay` forced from `new-engine!`, never a load-time `defonce`:
  ;; `native-image` initializes this namespace at BUILD time, so an eager body
  ;; would create the BUILDER's `~/.vis/logs` and bake that path into the image.
  (delay (when-not (System/getProperty "polyglot.log.file")
           ;; Its OWN file, not vis's: two writers on one path break rotation,
           ;; and GraalPy holds this fd for the life of the process.
           (System/setProperty "polyglot.log.file" (paths/log-file "graalpy")))
         true))

(def graal-resource-cache-redirected
  ;; GraalPy materializes its Python stdlib ("internal resources", from the
  ;; python-resources jar) under `$XDG_CACHE_HOME/org.graalvm.polyglot` (or
  ;; `~/.cache/org.graalvm.polyglot`) — at RUNTIME, on the JVM and on the
  ;; compiled native-image binary alike. In a confined process — e.g. a macOS
  ;; sandbox profile that only whitelists the workspace and build caches — that
  ;; root is unwritable, and the very first stdlib import in the context
  ;; bootstrap dies with `ModuleNotFoundError: No module named 'ast'`. The
  ;; cache root is read ONCE per JVM (system property
  ;; `polyglot.engine.userResourceCache`) when internal resources initialize,
  ;; so resolve it lazily HERE, when the first engine is built — a
  ;; load-time `defonce` would run on the native-image BUILDER instead.
  ;; Precedence: explicit `-Dpolyglot.engine.userResourceCache` (untouched) >
  ;; `python.resource-cache` in merged vis.yml config > `~/.vis/cache/graal-resources`
  ;; (ALWAYS preferred: writable across sandboxed and normal runs, gitignored) >
  ;; `./.graal-resources` under the CWD. We deliberately do NOT keep the default
  ;; `~/.cache/org.graalvm.polyglot` root even when it's writable — redirecting
  ;; unconditionally makes stdlib materialization behave identically whether or
  ;; not the sandbox happens to whitelist that root. Documented in
  ;; vis-docs/configuration.md § GraalPy internal-resource cache.
  (delay
    (or
      (some? (System/getProperty "polyglot.engine.userResourceCache"))
      (let
        [expand-home
         paths/expand-home

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
                              (get-in ["python" "resource_cache"]))]
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
        true))))

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

(defonce ^:private engine-cache-store-hook-installed
  ;; The warmed-engine STORE hook is process-wide even though engines are not:
  ;; one hook, installed for the FIRST engine built, is all a native binary
  ;; needs to leave a warm cache behind for the next start.
  (atom false))

(defn new-engine!
  "Build a FRESH GraalVM Engine for ONE session sandbox.

   NOT process-wide, and that is the whole point. A shared Engine RETAINS every
   Context ever built on it: closing the Context frees nothing while the Engine
   lives, and only `Engine.close()` gives the memory back. Measured on GraalVM
   25.1.3 — six heavy contexts created and CLOSED on one shared engine grew the
   live heap by ~31 MB each and gave none of it back until the engine itself was
   closed, at which point all of it returned at once. An engine per session makes
   that leak structurally impossible: the engine dies with its context.

   The shared engine originally existed to dodge a Truffle deadlock (GraalVM
   25.0.1: a standalone `Context.build()` during a live eval froze the JVM at a
   safepoint). That hazard does not reproduce on 25.1.3 — see
   `env-python-engine-test`, which builds an engine + context while a virtual
   thread is mid-eval. The cost of not sharing is ~100ms per session start.

   COMPILER-THREAD CAP: Truffle's JIT (`engine.CompilerThreads`) defaults to
   -1 = scale with CPU cores. On a 14-core box that spawns a fistful of JVMCI
   CompilerThreads that dominated every cumulative-CPU table during GraalPy
   warm-up. Cap at 2 and let idle compiler threads retire after 5s (default 10s)
   — fewer live threads, smaller CPU bursts, at a slightly slower warm-up."
  ^Engine []
  ;; Both are load-order sensitive system properties: force them HERE, before
  ;; the first Engine exists, so the RUNNING process (not the native-image
  ;; builder) decides the Truffle log file and the resource-cache root.
  @polyglot-noise-silenced
  @graal-resource-cache-redirected
  (let
    [build-engine
     (fn ^Engine [^java.io.File load-file tune?]
       (let
         [b (-> (Engine/newBuilder (into-array String ["python"]))
                (.allowExperimentalOptions true)
                (.option "engine.WarnVirtualThreadSupport" "false"))]
         ;; Compiler-thread tuning only exists when the Truffle optimizing
         ;; runtime is present (JVM, or an Oracle/enterprise native image).
         ;; The community native image runs GraalPy in interpreter mode with
         ;; no compiler, so these options don't exist. Option validation is
         ;; DEFERRED to `.build`, so a bad `.option` can't be caught at set
         ;; time — the caller retries with `tune? false` when a tuned build
         ;; throws `Could not find option with name engine.CompilerThreads`.
         (when tune?
           (.option b "engine.CompilerThreads" "2")
           (.option b "engine.CompilerIdleDelay" "5000")
           ;; Enable the auxiliary-cache STORE path so the shutdown-hook
           ;; `.storeCache` actually writes (Engine javadoc: requires
           ;; engine.CacheStoreEnabled=true; without it storeCache returns
           ;; false and writes nothing). The option is contributed only by the
           ;; enterprise Truffle runtime in an Oracle native image, so gate it
           ;; on native-image? — the community image lacks it and drops it on
           ;; the tune? false retry; the JVM optimizing runtime never sets it
           ;; (store is a no-op there we never call).
           (when (and (native-image?) (engine-cache-enabled?))
             (.option b "engine.CacheStoreEnabled" "true")))
         ;; `engine.CacheLoad` restores the warmed compiled code. The option
         ;; only EXISTS on a native-image host; on the JVM it throws, so we
         ;; only reach here under `use-cache?` (native binary). Any load
         ;; failure (stale/corrupt/option-drift image) is caught by the
         ;; caller and falls back to a plain cold build.
         (when load-file (.option b "engine.CacheLoad" (.getAbsolutePath load-file)))
         (.build b)))

     build-engine*
     (fn ^Engine [^java.io.File load-file]
       (try (build-engine load-file true) (catch Throwable _ (build-engine load-file false))))

     use-cache?
     (and (native-image?) (engine-cache-enabled?))

     cache-file
     (when use-cache? (engine-cache-file))

     engine
     (or (when (and cache-file (.exists ^java.io.File cache-file))
           (try (build-engine* cache-file) (catch Throwable _ nil)))
         (build-engine* nil))]

    ;; Store the warmed engine on process exit so the NEXT native start can
    ;; load it. Best-effort, never throws (see `store-engine-cache!`). ONCE per
    ;; process: engines are per-session now, and one hook per session would pile
    ;; up a hook (and a strong reference to a dead engine) for every session the
    ;; gateway ever served.
    (when (and use-cache? (compare-and-set! engine-cache-store-hook-installed false true))
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. ^Runnable
                                 (fn []
                                   (store-engine-cache! engine cache-file))
                                 "vis-engine-cache-store")))
    engine))

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
  "Seed a standalone `vis-agent python` CLI context with script `argv` (bound to
   `sys.argv`) and, when non-empty, an `env` map merged into `os.environ`.
   This is what gives the CLI real-`python` semantics: unlike the deny-by-
   default AGENT sandbox (env scrubbed for isolation — the human never sees
   their shell here), the human-run CLI forwards trailing script args and,
   by default, the caller's environment.

   Same shape as any host→guest seeding here: values cross via `putMember`, then a
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
  (when (zero? (long (count-top-level-forms python-context code)))
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
   (`fold_session`/`session_drop`/`__vis_par__`) are bound via `create-python-context`, not
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

(defn- set-python-binding-meta!
  "Record one piece of model-facing metadata for `sym` (and its py-aliases) in
   the sandbox `dict-name` table, then RE-STAMP those names so the bound
   callable carries it: a tool DEFERRED before its metadata arrived only gets it
   from the stamp. Keyed by the SAME py-name(s) `set-python-binding!` binds
   under, so an ALIASED extension binding AFTER context creation (per turn, via
   `sync-active-extension-symbols!`) is covered too, not only the built-ins
    seeded eagerly in `build-agent-context`. An EMPTY string is a value, not a
    blank: it is how a zero-argument tool declares its parameter list. No-op for
    a non-string, and for a helper context with no sandbox or no async preamble."
  [python-context sym dict-name text]
  (when (and python-context (string? text))
    (let
      [^Context ctx
       python-context

       g
       (python-globals ctx)]

      (try (.putMember g "__vis_meta_txt__" (str text))
           (doseq [nm (cons (sym->py-name sym) (py-aliases-for-sym sym))]
             (.putMember g "__vis_meta_sym__" (str nm))
             (.eval ctx
                    "python"
                    (str "globals().setdefault('"
                         dict-name
                         "', {})[__vis_meta_sym__] = __vis_meta_txt__\n"
                         "if '__vis_stamp_tools__' in globals():\n"
                         "    __vis_stamp_tools__([__vis_meta_sym__])")))
           (finally (.putMember g "__vis_meta_sym__" nil) (.putMember g "__vis_meta_txt__" nil))))))

(defn set-python-binding-doc!
  "Record `doc` text for `sym` in the sandbox `__vis_docs__` dict that in-sandbox
   `doc(name)` / `apropos(pat)` read — and that a deferred tool carries as its
   `__doc__`, so `help(tool)` answers the same contract."
  [python-context sym doc]
  (when-not (str/blank? doc) (set-python-binding-meta! python-context sym "__vis_docs__" doc)))

(defn set-python-binding-signature!
  "Record `signature` — the Python parameter list from
   `extension/symbol-signature` — for `sym` in the sandbox `__vis_sigs__` dict.
   A deferred tool hangs it off `__wrapped__`, so `inspect.signature(tool)`
   reports the declared parameters instead of the async trampoline's `(*a, **k)`.
   The EMPTY string is meaningful — a tool that takes no arguments at all."
  [python-context sym signature]
  (set-python-binding-meta! python-context sym "__vis_sigs__" signature))

(defn set-python-binding-keys!
  "Record `keys-line` — the options-dict vocabulary from
   `extension/symbol-keys-line` — for `sym` in the sandbox `__vis_keys__` dict, so
   `doc(sym)` states which keys the dict must carry. Structure, not prose: it is
   printed above the document and never mixed into the text `apropos` ranks."
  [python-context sym keys-line]
  (set-python-binding-meta! python-context sym "__vis_keys__" keys-line))
(def ^:private protected-baseline-names
  "Python globals the agent may CALL but must not rebind. Rebinding a tool or
   parser-helper name would shadow the persistent session substrate."
  #{"apropos" "doc" "defs" "gather" "__vis_count_forms__" "__vis_banned_name__"})

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
           (.putMember g "__vis_meta_sym__" (str nm))
           (.eval ^Context python-context
                  "python"
                  "globals().get('__vis_docs__', {}).pop(__vis_meta_sym__, None)"))
         (.putMember g "__vis_meta_sym__" nil))
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
    (.putMember g "__vis_meta_sym__" (str sym))
    (.putMember g "__vis_meta_txt__" (str (or doc "vis-managed engine binding")))
    (.eval ^Context python-context
           "python"
           "globals().setdefault('__vis_docs__', {})[__vis_meta_sym__] = __vis_meta_txt__")
    nil))


;; =============================================================================
;; Python sandbox context creation
;; =============================================================================

(defn- value->str-map
  "A GraalPy dict `Value` as a Clojure `{string string}`. Missing / null / not-a
   -dict answers `{}`, because a discovery surface must never be the reason a
   block fails."
  [^Value d]
  (if-not (and d (not (.isNull d)) (.hasHashEntries d))
    {}
    (let [^Value it (.getHashEntriesIterator d)]
      (loop [m (transient {})]
        (if-not (.hasIteratorNextElement it)
          (persistent! m)
          (let
            [^Value e (.getIteratorNextElement it)
             ^Value v (.getArrayElement e 1)]

            ;; `.asString`, never `str`: `Value.toString` on a guest string is its
            ;; Python REPR, which quoted every gist the index printed.
            (recur (assoc! m
                           (.asString ^Value (.getArrayElement e 0))
                           (if (.isString v) (.asString v) (str v))))))))))

(defn- called-str-map
  "The dict a ZERO-ARGUMENT sandbox function answers, as a Clojure `{string
   string}`. `{}` when the bootstrap never bound that name, when it is not
   callable, or when the call throws — like `value->str-map`, a discovery
   surface must never be the reason a block fails."
  [^Value g ^String fn-name]
  (try (let [^Value f (.getMember g fn-name)]
         (if (and f (not (.isNull f)) (.canExecute f))
           (value->str-map (.execute f (object-array 0)))
           {}))
       (catch Throwable _ {})))
(def ^:private ^:const apropos-limit
  "How many documents `apropos` answers for a described ask. Terms are ORed, so
   a six-word question touches half the corpus; every measured ask answered
   inside the first four rows, and each row now carries an excerpt rather than
   a bare line, so the tail is paid for twice over."
  10)

(defn- seed-py-map!
  "Merge `m` into the sandbox dict `dict-name` with `setdefault` semantics — the
   FIRST source to claim a key keeps it, the same precedence rule `__vis_docs__`
   uses. Marshalled as JSON and parsed with the auto-imported `json` module, so
   no ProxyHashMap crosses the boundary. Best-effort: a registry hiccup must
   never break context creation."
  [^Context ctx ^Value g ^String dict-name m]
  (when (seq m)
    (try (.putMember g "__vis_seed_json__" (json/write-json-str m))
         (.eval ctx
                "python"
                (str "_vis_seed = globals().setdefault('" dict-name
                     "', {})\n" "for _k, _v in json.loads(__vis_seed_json__).items():\n"
                     "    _vis_seed.setdefault(_k, _v)\n" "del _vis_seed, _k, _v"))
         (.putMember g "__vis_seed_json__" nil)
         (catch Throwable _ nil))))

(defn- sandbox-corpus
  "The whole corpus as `doc-corpus` entries: the sandbox's own globals merged with
   the LIVE document corpus.

   Read off the GLOBALS, so a per-turn extension activation is searchable the
   moment it binds: `__vis_docs__` holds one document per handle (function
   contracts seeded from the extension registry, plus every shim's page),
   `__vis_calls__` holds the expression that uses the handles which are not
   themselves Python names, `__vis_sigs__` the declared parameter list of every
   callable, and `__vis_kinds__` labels the ones a source CLASSIFIED (shim).

   Read LIVE from `doc-corpus/entries`: every skill, every documentation page and
   every MCP tool. Those are never seeded, because this context outlives every
   `/reload` — a copy taken when it was built answers a skill added, edited or
   deleted mid-session with the corpus of an hour ago. The corpus memoizes on its
   sources' stamps, so an unchanged one costs a stat pass. A global still WINS the
   name: a callable's contract beats a documentation slug, which is exactly the
   precedence the old `setdefault` seed encoded.

   The two kinds nobody labels are derived here: a documented handle is a
   `tool`, and the model's own `def` is `local` — `__vis_def_docs__` names every
   one of them and answers its DOCSTRING, `__vis_def_calls__` its signature. A
   helper that documents itself is a document like any other: it owns a
   `doc(name)` page and a described ask can find it. One that documents nothing
   stays an EMPTY document — listed, because a callable the model can type must
   never be invisible, and never searched, because a bare handle (`where`,
   `vars`) is a common English word that would win rows off real contracts."
  [^Value g names-fn]
  (let
    [docs
     (value->str-map (.getMember g "__vis_docs__"))

     calls
     (value->str-map (.getMember g "__vis_calls__"))

     sigs
     (value->str-map (.getMember g "__vis_sigs__"))

     kinds
     (value->str-map (.getMember g "__vis_kinds__"))

     params
     (value->str-map (.getMember g "__vis_keys__"))

     ;; The model's OWN `def`s, read live through the bootstrap: one docstring
     ;; and one signature per helper, so a documented helper is a page like any
     ;; tool's and an undocumented one keeps an empty document.
     def-docs
     (called-str-map g "__vis_def_docs__")

     def-calls
     (called-str-map g "__vis_def_calls__")

     ;; The DOCUMENT half of the corpus, re-read on every call (see the
     ;; docstring). A source that throws contributes nothing — discovery must
     ;; never be the reason `doc`/`apropos` fails.
     documents
     (into {} (map (juxt :name identity)) (try (doc-corpus/entries) (catch Throwable _ nil)))]

    (into []
          (map
            (fn [nm]
              (let
                [document
                 (get documents nm)

                 text
                 (let [registered (str (get docs nm ""))]
                   (cond (seq registered) registered
                         (seq (str (:text document))) (str (:text document))
                         :else (str (get def-docs nm ""))))

                 ;; The CALL LINE `doc(name)` prints under the handle: the explicit
                 ;; expression when the handle is not a Python name of its own
                 ;; (`mcp__call(...)`), otherwise the DECLARED signature —
                 ;; `patch(path, edits)`. Prose can describe a tool; only the call
                 ;; line says which parameters are required and in what order.
                 call
                 (or (not-empty (str (get calls nm)))
                     (not-empty (str (:call document)))
                     (when-let [sig (get sigs nm)]
                       (str nm "(" sig ")"))
                     (not-empty (str (get def-calls nm))))]

                (cond->
                  {:name nm
                   :text text
                   ;; A REGISTERED document is what makes a handle a tool: a helper
                   ;; that shadows one keeps the tool's page, and a documented `def`
                   ;; stays `local` however much it says about itself.
                   :kind
                   (or (get kinds nm)
                       (:kind document)
                       (if (and (str/blank? (str (get docs nm))) (nil? document)) "local" "tool"))}
                  (seq (str call))
                  (assoc :call call)

                  (seq (str (get params nm)))
                  (assoc :params (str (get params nm)))))))
          (distinct (concat (names-fn) (sort (keys docs)) (sort (keys documents)))))))

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
     ;; `asyncio` is the async-runtime shim global (`asyncio = __vis_asyncio__`,
     ;; so `import asyncio`/`asyncio.run(...)` work) — a runtime, not a tool.
     non-tool-names
     #{"asyncio"}

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

     ;; A global that is not CALLABLE is not a tool: the model's own loop
     ;; variables (`x`, `qs`, `day_set`) are globals too, and each one used to
     ;; become a document whose NAME scores an exact hit — one-letter noise
     ;; hijacking every natural-language query. Data the prompt teaches keeps
     ;; its page through `__vis_docs__`, which is merged in separately, so this
     ;; filter drops only the undocumented, uncallable leftovers of a block.
     callable?
     (fn [^String n]
       (try (let [v (.getMember g n)]
              (boolean (and v (not (.isNull v)) (.canExecute v))))
            (catch Throwable _ false)))

     names
     (fn []
       (sort (distinct (concat (filter (fn [n]
                                         (and (not (str/starts-with? n "_"))
                                              (not (contains? builtin-names n))
                                              (not (contains? non-tool-names n))
                                              (callable? n)))
                                       (map str (seq (.getMemberKeys g))))
                               (shim-names)))))]

    (.putMember
      g
      "apropos"
      (reify
        ProxyExecutable
          (execute [_ args]
            (let
              [query
               (if (pos? (alength args)) (.asString ^Value (aget args 0)) "")

               ;; A described ask matches half the corpus once terms are ORed, and
               ;; rows past the first handful are noise the caller reads and pays
               ;; for. The EMPTY query is a LISTING, not a search: it stays whole.
               described?
               (seq (str/trim query))

               ;; Underscore-prefixed handles (`_shell_logs`, the deferred
               ;; transports) are PRIVATE: `doc(name)` still states their result
               ;; contract, but no listing advertises them.
               public
               (remove #(str/starts-with? (str (:name %)) "_"))

               ;; A callable with NO document — an undocumented `def`, a stdlib
               ;; name that leaked into the globals — is a NAME and nothing else.
               ;; It stays in the LISTING, because a callable the model can type
               ;; must never be invisible, but it is not in the corpus a described
               ;; ask searches: it has no text to answer with, and its handle
               ;; (`vars`, `where`, `hits`) is a common English word that would
               ;; win rows off real contracts. One DOCSTRING is the whole
               ;; difference — a helper that carries text is searched like a page.
               corpus
               (cond->> (sandbox-corpus g names)
                 described?
                 (into [] (remove #(str/blank? (str (:text %))))))

               ranked
               (doc-corpus/search corpus
                                  query
                                  (when described? {:limit (+ (long apropos-limit) 8)}))

               ;; The RESOLVED terms — what a prefix completed to, what a typo was
               ;; corrected to — ride on the ranking's metadata, which the
               ;; transducer below would drop.
               terms
               (:terms (meta ranked))

               hits
               (into [] (if described? (comp public (take apropos-limit)) public) ranked)

               shown
               (mapv #(doc-corpus/preview (:text %) terms) hits)]

              ;; Return a REAL native Python dict {name -> row} in RANK order by
              ;; zipping parallel arrays guest-side — no ProxyHashMap crosses the
              ;; boundary, so `list()/in/sorted/set/**` all behave natively.
              ;;
              ;; TWO SHAPES, because `at` and `hit` mean something only RELATIVE to a
              ;; query: a described ask answers {kind, gist, at, hit}, while the bare
              ;; LISTING answers {kind, gist} instead of repeating a dead `at: 0` and
              ;; `hit: ''` on every name the session can reach.
              (.putMember g "__vis_apropos_names__" (->py (mapv :name hits)))
              (.putMember g "__vis_apropos_kinds__" (->py (mapv #(str (:kind % "tool")) hits)))
              (.putMember g "__vis_apropos_gists__" (->py (mapv :gist shown)))
              (when described?
                (.putMember g "__vis_apropos_ats__" (->py (mapv :at shown)))
                (.putMember g "__vis_apropos_hits__" (->py (mapv #(str/join " " (:hit %)) shown))))
              (try (.eval
                     ctx
                     "python"
                     (if described?
                       (str "dict(zip(list(__vis_apropos_names__), "
                            "[{'kind': _k, 'gist': _g, 'at': _a, 'hit': _h} for _k, _g, _a, _h in "
                            "zip(list(__vis_apropos_kinds__), list(__vis_apropos_gists__), "
                            "list(__vis_apropos_ats__), list(__vis_apropos_hits__))]))")
                       (str "dict(zip(list(__vis_apropos_names__), "
                            "[{'kind': _k, 'gist': _g} for _k, _g in "
                            "zip(list(__vis_apropos_kinds__), list(__vis_apropos_gists__))]))")))
                   (finally (.putMember g "__vis_apropos_names__" nil)
                            (.putMember g "__vis_apropos_kinds__" nil)
                            (.putMember g "__vis_apropos_gists__" nil)
                            (.putMember g "__vis_apropos_ats__" nil)
                            (.putMember g "__vis_apropos_hits__" nil)))))))
    (.putMember
      g
      "doc"
      (reify
        ProxyExecutable
          (execute [_ args]
            (let
              [target
               (when (pos? (alength args)) (.asString ^Value (aget args 0)))

               es
               (sandbox-corpus g names)]

              (if (str/blank? (str target))
                (doc-corpus/index-text es)
                (let
                  [wanted
                   (doc-corpus/normalize-name target)

                   ;; Resolution order is precedence: the exact handle, then the
                   ;; forgiving one (case, whitespace, a trailing `.md`), so a
                   ;; function name always wins a documentation slug.
                   hit
                   (or (first (filter #(= (str target) (:name %)) es))
                       (first (filter #(= wanted (doc-corpus/normalize-name (:name %))) es)))

                   m
                   (when hit (.getMember g ^String (:name hit)))]

                  (if hit
                    (let
                      [note
                       (when (and m (not (.isNull m)) (.canExecute m)) "callable")

                       page
                       (doc-corpus/entry-text hit note)]

                      ;; A helper this session defined and never documented has
                      ;; no page to print — so the page says what would make one.
                      (if (and (= "local" (str (:kind hit))) (str/blank? (str (:text hit))))
                        (str page
                             "This session defined it and it carries no docstring, so there is "
                             "nothing to print. `defs(\""
                             (:name hit)
                             "\")` returns its source; a "
                             "docstring's first line becomes its `defs()` gist and the whole of it "
                             "becomes this page — which is also what lets `apropos` find it.")
                        page))
                    (doc-corpus/miss-text es target))))))))
    ;; These two helpers are installed by the engine rather than the extension
    ;; registry, so seed their own docs here.
    (set-python-binding-doc!
      ctx
      'apropos
      "apropos(query='') -> {name: {kind, gist, at, hit}}. FULL-TEXT SEARCH over every document this session can reach — every function's contract, every skill's whole SKILL.md, every Vis documentation page, every MCP tool's description. Ask in words: terms are ORed and ranked by BM25 relevance over name, first line and body, so a whole question answers the document that covers most of it and a word nothing carries costs nothing. A query that IS a handle wins that handle; a typo is spell-corrected and a prefix completed, and `hit` names the terms that landed, showing any rewrite (`pathc→patch`). `kind` is what the document IS — tool · shim · page · skill · mcp · local (a helper this session defined — its own DOCSTRING is its document, so a documented helper answers a described ask while an undocumented one is listed and never searched). `gist` is a BOUNDED excerpt, never the body: the document's opening, the region your terms landed in, and a fragment from deeper down; `at` is the line that region starts on, so a long document is read from where it answers. Ranked best-first and capped at 10 — a described ask matches half the corpus, so read down, not around. Called with NO argument (or an empty one) it LISTS instead of searching: every name this session can reach, in name order, uncapped, each row just {kind, gist} — `at` and `hit` exist only relative to a query. Read one whole with `doc(name)`.")
    (set-python-binding-doc!
      ctx
      'doc
      "doc(target) -> str. RETRIEVE one document whole: a function name, a Vis documentation slug or a skill name — case, whitespace and a trailing `.md` do not matter, and a function wins a name collision. `doc()` with no argument prints the curated index of the verbs a session starts from. A skill is one of these documents and nothing more: reading it is the whole of using it.")
    (set-python-binding-doc!
      ctx
      'gather
      "gather(*awaitables) -> list. Concurrently run independent deferred tool calls/awaitables on a bounded host pool; results preserve input order. One list/tuple works. Use `await gather(...)`; keep dependent calls sequential. All settle before failure reports every failing slot index.")
    (set-python-binding-doc!
      ctx
      'defs
      "defs(name=None) -> str. The FUNCTIONS this session defined, as text: `defs()` lists each one's name, signature, block, length and the first line of its DOCSTRING; `defs(name)` returns that one's source, so a helper is refined by editing what it already says instead of being re-pasted from memory. WRITE that docstring: its first line is the gist this listing prints, the whole of it is the helper's own `doc(name)` page, and it is what lets `apropos` find a helper you already wrote — an undocumented one is listed and never searched. A `def` outlives the block and the turn, and is re-created in a fresh sandbox after a gateway restart — a restored one is marked `(restored)`. Imported functions and engine internals are never listed. When the same helper recurs across sessions it has outgrown the sandbox: propose a Python extension (`doc(\"extending\")`).")
    (set-python-binding-doc!
      ctx
      'fold-session
      "fold_session(key, gist=None) -> str. Collapse SETTLED steps: prior turns and the current turn only through its last completed iteration; live/future steps cannot fold. The key is a STRING: \"t2/i5\" one step · \"t2\" a whole turn · \"t2/i1-i56\" a range · \"-t2/i56\" everything through it · \"t2/i5-\" everything since it · comma-separate several, disjoint ranges included (\"t1/i61-i98, t3/i111-i135, t4\" is ONE fold); a token that is not a step key, or that matches no settled step, is refused by name. Folding changes rendering, not storage; there is no destructive unfold command, and a folded step is not re-readable inline — its GIST is what survives. With introspection on, `s = await read_session()` and filter `['transcript']['turns'][...]['iterations'][...]['blocks']`. A broader newer fold supersedes fully covered breadcrumbs; equal scope keeps newer. Partial overlaps remain separate.")
    ;; …and their SIGNATURES. These five are host callables, not deferred tool
    ;; wrappers, so the extension registry never sees them: this is the only place
    ;; their parameter list can come from, and without it `doc(name)` would be the
    ;; one page in the corpus that never shows how to call what it documents.
    (doseq
      [[sym signature] {'apropos "query=''"
                        'doc "target=None"
                        'gather "*awaitables"
                        'defs "name=None"
                        'fold-session "key, gist=None"}]
      (set-python-binding-signature! ctx sym signature))))


(def PROCESS_SURFACE
  "THE sentences about this sandbox's process surface — written ONCE, here, and
   said verbatim by every surface that has to say them:

     - the `sandbox-shims` prompt block (`prompt/sandbox-shims-prompt-block`);
     - the POSIX refusal (`vis-shims/posix.py`), when `subprocess` / `os.system` /
       `os.popen` is called;
     - a live handle that cannot be driven (`__VisShell__.__vis_op__` in
       `vis-python/async_runtime.py`);
     - the corpus entry named `shell` (`env-python/create-python-context`), when the
       shell tools are off and `apropos(\"shell\")` / `doc(\"shell\")` would otherwise
       answer with silence.

   Every copy of one fact drifts, and the copy the model reads at the moment it
   is blocked is the one that must be right. Composed, never concatenated ad hoc:
   `ban` is the rule and is all the PROMPT says (invocation grammar belongs to the
   `shell` symbol's own docs, not to a supplemental block); `ban` + `use` is what a
   call site says, because there the model is already writing the call; `off` is
   the toggle state, and it names BOTH doors so silence is never read as
   \"`subprocess` might still work\"; `off` + `extension` is what DISCOVERY says,
   because a model that reads `off` alone concludes nothing in this product can
   start a process. The toggle closes the MODEL's door only: an installed Python
   extension's `vis.shell` is wired unconditionally in
   `python-extensions/build-context` and no toggle gates it.

   Reaches Python as the `__vis_process_surface__` global (see
   `install-process-surface!`), so no `.py` file carries a copy."
  {"ban" (str "`subprocess`, `os.system` and `os.popen` never spawn in the vis sandbox "
              "— every process starts through the sandbox's own `shell` verb, which "
              "owns the jail, the log file and the handle. There is no shell TOOL.")
   "use" (str "Use `sh = await shell('npm test')`; the handle it returns drives "
              "it with `logs()`, `wait()`, `type()` and `stop()`, and every answer "
              "already carries that shell's status.")
   "off" (str "Shell commands are DISABLED in this vis sandbox: `shell` is not bound, "
              "a live handle cannot be driven, and `subprocess`, `os.system` and "
              "`os.popen` all raise — nothing here can start a process. Turn on 'Shell "
              "commands' in the settings dialog.")
   "extension" (str
                 "That toggle closes the MODEL's door only: an installed Python extension "
                 "keeps its own trusted process boundary, so `vis.shell({'command': 'npm test'})` "
                 "in extension code still spawns and answers the same handle.")})

(def ^:private posix-refusal-shim-src
  "Pure-Python source that replaces `subprocess` / `os.system` / `os.popen` with
   ONE refusal that names the `shell` tool. The body lives in the
   `vis-shims/posix.py` CLASSPATH RESOURCE (`resources/vis-shims/posix.py`) -
   real Python in a real `.py` file, never a Clojure string - and is embedded in
   the native image by build.clj's `-H:IncludeResources=vis-shims/.*`.
   It carries no wording of its own: the sentences come from `PROCESS_SURFACE`
   via `__vis_process_surface__`, and `shell` is looked up in `globals()` at CALL
   time, so ONE file serves both states - with the shell tools on it names the
   tool and its invocation; with the toggle off it says the toggle disabled BOTH
   doors."
  (runtime-python-src "vis-shims/posix.py"))

(def AUTO_IMPORTED_PYTHON_NAMES
  "Python names installed into builtins for every `python_execution` context.
   This is the model-facing inventory; keep it synchronized with
   `auto-imports-python` and its real-context regression test."
  ["json" "shlex" "re" "hashlib" "glob" "os" "sys" "collections" "Counter" "pathlib" "Path"
   "textwrap" "base64" "math" "socket" "builtins" "time" "datetime"])

(def ^:private auto-imports-python
  "Install tiny convenience imports as Python builtins so agents can use them
   without repeating imports in every python_execution block. os/sys/json/re (plus the
   builtins self-ref) are bound EAGERLY - they are cheap, load anyway via engine
   internals, and are the hottest names. Every OTHER name is a `_LazyStd` proxy
   on builtins: the real stdlib module is imported on the FIRST bare touch
   (`hashlib.md5(...)`, `Counter(...)`, `Path(...)`, `socket.socket(...)`, ...) and then REPLACES
   the proxy, so a session that never touches base64/textwrap/socket/... never
   pays their import at context build. An explicit
   `import <name>` always works too (normal stdlib path). The model-facing
   inventory is `AUTO_IMPORTED_PYTHON_NAMES`.

   `time` is EAGER and NOT optional: GraalPy leaves the time module's state
   (its `currentZoneId`) uninitialized until `time` is first imported, so the
   first local-time conversion in a cold context - `datetime.fromtimestamp(...)`,
   `datetime.now()`, `time.localtime()` - died with
   `NullPointerException: Cannot read field \"currentZoneId\" because
   \"moduleState\" is null`. Importing `time` here initializes it once per
   context (~ms). `datetime` stays a lazy proxy - safe now that `time` is up."
  (runtime-python-src "vis-python/auto_imports.py"))

(defn- install-auto-imports!
  "Make selected stdlib modules and symbols available as builtins in every sandbox.
   os/sys/json/re are eager; the rest are lazy proxies materialized on first
   touch. Keep the inventory tiny: only safe, pure, repeatedly-useful glue."
  [^Context ctx]
  (try (.eval ctx "python" ^String auto-imports-python) (catch Throwable _ nil)))

(defn- install-process-surface!
  "Seed `__vis_process_surface__` (the ONE `PROCESS_SURFACE` wording) and install
   the POSIX refusal shim that raises out of it: `subprocess` / `os.system` /
   `os.popen` name the `shell` tool instead of spawning, and a handle that cannot
   be driven says the same thing. The shim body is a few dozen lines with no
   module graph behind it, so it is eval'd EAGERLY - the lazy `meta_path` finder
   it replaced existed only to defer a ~95ms delegation bridge that no longer
   exists. Best-effort: a failure leaves the sandbox without the shim (the spawn
   then fails opaquely), it must never break context creation."
  [^Context ctx]
  (when-let [src posix-refusal-shim-src]
    (try (let [g (python-globals ctx)]
           (.putMember g "__vis_process_surface_json__" (json/write-json-str PROCESS_SURFACE))
           (.eval ctx
                  "python"
                  "globals()['__vis_process_surface__'] = json.loads(__vis_process_surface_json__)")
           (.putMember g "__vis_process_surface_json__" nil)
           (.eval ctx "python" ^String src))
         (catch Throwable _ nil))))

(defn- registered-sandbox-shims
  "The Python sandbox shims contributed by the extension registry."
  []
  (extension/sandbox-shims))

(def ^:private lazy-shim-runtime-python
  "Central LAZY-SHIM runtime installed once per Context, BEFORE the shims.
   Instead of eagerly eval'ing every shim's source (which materializes each
   module's whole live object graph - numpy/pandas/PIL/... - into THIS context,
   the fat per-session baseline), we register only a tiny trigger table and
   defer each shim's eval until the FIRST touch:
     * `import <name>` - served by a PREPENDED `sys.meta_path` finder (prepended
       so a shim can still shadow a stdlib name, e.g. `zoneinfo`), or
     * a no-import bare `<name>.attr` / `<name>(...)` - served by a `builtins`
       proxy staged under each autoload name.
   The heavy module is then built exactly once per context, on demand; a session
   that never touches numpy never pays numpy's heap. `__vis_load_shim__` is the
   host callback that eval's the matching shim source into this ctx."
  (runtime-python-src "vis-python/lazy_shim_runtime.py"))

(defn- shim-source
  "Python source of a shim, slurped from its `:shim/source` classpath resource."
  [shim]
  (extension/shim-src shim))

(defn- wire-shim-bindings!
  "Wire a shim's host `:shim/bindings` (`{py-name -> fn}`, or a 0-arg fn -> one)
   onto the sandbox globals `g` as Python callables. Cheap (proxies only) and
   done EAGERLY even for lazy shims, so a deferred source finds them at load."
  [^Value g shim scope]
  (let
    [b
     (:shim/bindings shim)

     bindings
     ;; A shim that owns host objects takes the SCOPE so it can tie their life to
     ;; this Context's (see `own-in-scope!`); one that owns nothing keeps its
     ;; 0-arg factory. `ArityException` is thrown by `invoke` BEFORE any body
     ;; runs, so the retry cannot double any side effect.
     (if (fn? b)
       (try (b scope) (catch clojure.lang.ArityException _ (b)))
       b)]

    (doseq [[nm f] bindings]
      (.putMember g ^String nm (wrap-ifn f)))))

(defn- sha256-hex
  "Lowercase-hex SHA-256 of `s` (UTF-8) - the cache key over all shim sources."
  ^String [^String s]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (->> (.digest md (.getBytes s "UTF-8"))
         (map #(format "%02x" (bit-and (int %) 0xff)))
         (apply str))))

(def ^:private shim-triggers-cache-file
  ;; Persistent trigger-map cache: `~/.vis/cache/shim-triggers.json`. Holds a
  ;; hash-keyed SET of trigger maps (newest first, capped) so a multi-project
  ;; user alternating folders with DIFFERENT shim sets keeps each set warm
  ;; instead of thrashing one slot. Each set self-invalidates on source change.
  (delay (io/file (System/getProperty "user.home") ".vis" "cache" "shim-triggers.json")))

(def ^:private shim-triggers-cache-max
  ;; Keep at most this many distinct shim-sets (disk + memo). Bounds a user who
  ;; churns extensions; an evicted set just re-captures (~0.5s) or re-reads (~0.6ms).
  8)

(defonce ^:private shim-triggers-memo
  ;; Process-wide LRU memo: a newest-first VECTOR of `{:h hash :m trigger-map}`,
  ;; so the capture probe runs at most ONCE per process per shim-set (never per
  ;; Context). Bounded to `shim-triggers-cache-max` with TRUE
  ;; move-to-front eviction, mirroring the disk cache (a >max churner drops only
  ;; the OLDEST set, never the whole memo).
  (atom []))

(defn- shim-entries-hash
  "Content hash over every shim's source (sorted by name), so the trigger
   map is recomputed only when a shim's SOURCE (or the shim set) changes."
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

(defn- valid-shim-trigger-map?
  "A cache entry is usable only when every declared import was observed as a provided module.
   Older probes can miss modules already loaded by the shared engine; reject those entries so
   the next lookup recaptures and repairs the disk cache."
  [entries m]
  (and (map? m)
       (every? (fn [{:keys [sid shim]}]
                 (let
                   [imports
                    (vec (:shim/imports shim))

                    provides
                    (vec (get-in m [sid :provides]))]

                   (or (empty? imports) (seq provides))))
               entries)))
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
   throwaway probe Context on the shared engine, eval every shim source once,
   and DIFF `sys.modules` + `builtins` before/after:
     :autoload - the names the shim STAPLES onto builtins (its deliberate public
                 surface: bare `<name>.attr` / `<name>(...)` with no import), and
     :provides - the top-level modules it publishes for `import <name>`, taken as
                 the new `sys.modules` keys that are ALSO stapled (or the shim's
                 own name). Intersecting with the staples is what discards the
                 shim's TRANSITIVE stdlib imports (requests pulling in `ssl`,
                 `email`, `datetime`, ...) so they never become bogus triggers.
   A shim with neither (e.g. `attach`) is not lazy-eligible => stays eager. The
   probe Context is disposed before returning. Best-effort per shim: a source
   that throws yields empty triggers (=> that shim installs eager) rather than
   breaking the whole capture."
  [entries]
  (let
    [;; Its OWN engine, closed in the `finally` with the context: a probe that
     ;; rode a shared engine would leave its parsed Python behind for the life
     ;; of the process (see `new-engine!`).
     ^Engine probe-engine
     (new-engine!)

     ctx
     (-> (Context/newBuilder (into-array String ["python"]))
         (.engine probe-engine)
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
                      ;; nil scope: the probe is process-wide, owns nothing, and
                      ;; its context is closed a few lines below.
                      (wire-shim-bindings! g shim nil)
                      (try (.eval ctx "python" ^String src) (catch Throwable _ nil))
                      (let
                        [after (snap)
                         new-mods (->> (set/difference (set (get after "m")) (set (get before "m")))
                                       (map top)
                                       (remove #{"builtins"})
                                       set)
                         autoload0 (vec (sort (set/difference (set (get after "b"))
                                                              (set (get before "b")))))
                         keep? (into #{sid} autoload0)
                         provides0 (vec (sort (filter keep? new-mods)))
                         ;; Declared imports are the shim's owned public modules. Use them
                         ;; when the probe cannot observe a new sys.modules entry (for
                         ;; example a module already initialized by the engine).
                         autoload (if (seq autoload0) autoload0 (vec (:shim/imports shim)))
                         provides (if (seq provides0) provides0 (vec (:shim/imports shim)))]

                        (assoc acc sid {:provides provides :autoload autoload})))))
                {}
                entries))
      (finally (.close ctx true) (try (.close probe-engine true) (catch Throwable _ nil))))))

(defn- shim-trigger-map
  "The `{sid {:provides [...] :autoload [...]}}` lazy-trigger map for `entries`
   (each `{:sid :src :shim}`), derived by CAPTURE (`capture-shim-triggers`), then
   MEMOIZED process-wide and CACHED to disk keyed by a hash of the sources. The
   ~0.5s capture cost is therefore paid at most ONCE per machine per shim-source
   change - never per Context build, and never on a warm
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
          [m (try (or (let [cached (read-shim-triggers-cache h)]
                        (when (valid-shim-trigger-map? entries cached) cached))
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

(def ^:private shim-identity-python
  "Installs `__vis_stamp_shim__` — the ONE place a shim's module identity
   (`__file__`, `__vis_shim__`, fallback `__version__`) is stamped, so every
   synthesised module answers the introspection every real library answers."
  (runtime-python-src "vis-python/shim_identity.py"))

(def ^:private shim-origin-prefix
  ;; A shim's source is a CLASSPATH RESOURCE, not a file on disk (inside the
  ;; native image there is no file at all), so its `__file__` is angle-bracketed
  ;; the way CPython marks its own non-file origins (`<frozen ...>`).
  "<vis-shim>/")

(def ^:private shim-fallback-version
  ;; Only for a shim that declares no `__version__` of its own: something truthful
  ;; is better than an AttributeError, and a shim's own value always wins.
  "vis-shim")

(defn- shim-identity-json
  "Identity spec for `shim`: the modules it owns (declared `:shim/imports` plus
   the capture-derived `:provides`) and the `__file__` / `__version__` to stamp."
  ^String [shim trig]
  (json/write-json-str {"sid" (:shim/name shim)
                        "file" (str shim-origin-prefix (:shim/source shim))
                        "version" shim-fallback-version
                        "names" (vec (distinct (concat (:shim/imports shim) (:provides trig))))}))

(defn- install-sandbox-shims!
  "Install EVERY extension-contributed sandbox shim into `ctx`, in registration
   order, BEFORE the baseline snapshot.

   LAZY-BY-DEFAULT: rather than eval every shim's source here (which materializes
   each module's full live object graph into THIS context - the fat per-session
   baseline), install the central lazy runtime once, wire each shim's cheap host
   bindings eagerly, and register its trigger names. Each source is then eval'd
   ON DEMAND, once, on the first `import`/attribute touch (see
   `lazy-shim-runtime-python`). A shim with no derivable trigger (no importable
   module, no builtins staple - e.g. `attach`) stays EAGER, preserving its side
   effects. Best-effort: one shim's failure never breaks context creation.

   Whichever path materializes it, the module is stamped with its identity
   (`__file__` / `__vis_shim__` / fallback `__version__`) the moment its source
   has run - see `shim-identity-python`."
  [^Context ctx ^Value g]
  (let
    [shims
     (registered-sandbox-shims)

     base
     (mapv (fn [shim]
             {:shim shim :src (shim-source shim) :sid (:shim/name shim)})
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

     id->entry
     (into {} (comp (filter :lazy?) (map (juxt :sid identity))) entries)

     stamp!
     (fn [{:keys [shim trig]}]
       (try (.putMember g "__vis_shim_stamp_json__" (shim-identity-json shim trig))
            (.eval ctx "python" "__vis_stamp_shim__(__vis_shim_stamp_json__)")
            (.putMember g "__vis_shim_stamp_json__" nil)
            (catch Throwable t
              (tel/log! {:level :warn :id ::sandbox-shim-stamp-failed}
                        (str "sandbox shim '" (:shim/name shim)
                             "' identity stamp failed: " (or (.getMessage t) t))))))]

    ;; Module identity first: both paths below stamp through `__vis_stamp_shim__`.
    (try (.eval ctx "python" ^String shim-identity-python)
         (catch Throwable t
           (tel/log! {:level :warn :id ::shim-identity-runtime-failed}
                     (str "shim-identity runtime failed to install: " (or (.getMessage t) t)))))
    ;; Central lazy runtime + host loader callback - once, only if anything is lazy.
    (when (seq id->entry)
      (try (.eval ctx "python" ^String lazy-shim-runtime-python)
           (.putMember g
                       "__vis_load_shim__"
                       (wrap-ifn (fn [sid]
                                   (when-let [e (get id->entry (str sid))]
                                     (.eval ctx "python" ^String (:src e))
                                     (stamp! e))
                                   nil)))
           (catch Throwable t
             (tel/log! {:level :warn :id ::lazy-shim-runtime-failed}
                       (str "lazy-shim runtime failed to install: " (or (.getMessage t) t))))))
    (doseq [{:keys [shim src sid trig lazy?] :as e} entries]
      (try (wire-shim-bindings! g shim (res/scope-of ctx))
           (if (and lazy? (contains? id->entry sid))
             (do (.putMember g
                             "__vis_lazy_spec_json__"
                             (json/write-json-str
                               {"sid" sid "provides" (:provides trig) "autoload" (:autoload trig)}))
                 (.eval ctx "python" "__vis_register_lazy_shim__(__vis_lazy_spec_json__)")
                 (.putMember g "__vis_lazy_spec_json__" nil))
             (when (string? src) (.eval ctx "python" ^String src) (stamp! e)))
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
   `os.linesep` when writing text."
  ^String [^java.io.ByteArrayOutputStream baos]
  (.replace (.toString baos "UTF-8") "\r\n" "\n"))

(defn partial-stdout
  "Whatever the block currently running in `ctx` has PRINTED so far, or nil when
   it printed nothing.

   The eval watchdog kills a block from OUTSIDE the guest, so the flat
   `{:stdout}` | `{:error}` outcome `run-python-block` builds is never reached
   and everything printed before the wall — every progress line of a long fetch
   loop — used to be dropped with the frame. The capture buffer outlives the
   interrupt, so the timeout envelope drains it from here."
  [^Context ctx]
  (when-let [baos (ctx-stdout-baos ctx)]
    (let [s (baos->str baos)]
      (when-not (str/blank? s) s))))

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
  (runtime-python-src "vis-python/network_guard.py"))

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
  (runtime-python-src "vis-python/proxy_env.py"))

(def ^:private network-probe-python
  "In-sandbox network-filter DEV loop, wired onto the session globals as
   `network_filter(fn)` + `network_probe([method,] url)`. GUARD-ONLY: it
   exercises the egress DECISION (gateway host/verb/path/port + SSRF gate + every
   registered filter, plus the session's own local `network_filter`s) against a
   SYNTHETIC request via the `__vis_net_probe__` host callback — NO socket is
   opened and NOTHING is sent. Eval'd before the initial-ns snapshot so the names
   stay baseline (not surfaced as model-created live vars)."
  (runtime-python-src "vis-python/network_probe.py"))

(defn- make-outbox
  "DORMANT — not called while `mpl-capture/incidental-capture-enabled?` is false,
   so no context gets an outbox dir and the filesystem tap never arms.

   Create a fresh per-context OUTBOX directory under the system temp dir and return
   `{:dir <abs path string> :on-close record-file!}` for
   `sandbox-fs/confined-filesystem`. The sandbox may WRITE files there
   (`$VIS_OUTBOX`); each file it closes is captured AT THE SOURCE as a
   `session_iteration_attachment` — the implicit twin of `attach`, for a
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
  "Build ONE deny-by-default GraalPy agent sandbox Context on its OWN `Engine`,
   wire `custom-bindings` (tool/verb fns as Python callables, values marshalled),
   install introspection, and return
   `{:python-context :python-engine :sandbox-ns :initial-ns-keys}`. Used by
   `create-python-context` to build the one session sandbox. The caller owns the
   returned engine's lifecycle and MUST close it with the context."
  [custom-bindings roots-fn network-opts stdin stderr gate-fn]
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

     ;; The in-interpreter domain guard is part of the JAIL, not an independent
     ;; layer: it installs only when the jail is ON and there is a restriction to
     ;; enforce — a denylist (always present via defaults) or a non-`*` allowlist.
     ;; Jail OFF ⇒ no guard, so the sandbox network is unconfined (same as the OS
     ;; process jail). With net off the socket capability is denied outright.
     guard?
     (and net? (:jail-enabled? network-opts) (or (seq denied) (not allow-all?)))

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
     ;; OUTBOX (DORMANT): the automatic capture of everything the sandbox wrote
     ;; into `$VIS_OUTBOX` or system temp is OFF — `attach` is how an artifact
     ;; reaches the session now. See `mpl-capture/incidental-capture-enabled?`.
     ;; With no outbox the filesystem stays plain-confined: temp writes still
     ;; WORK, they are simply not harvested into the DB.
     outbox
     (when (and mpl-capture/incidental-capture-enabled? roots-fn) (make-outbox))

     io-access
     (if (or roots-fn net?)
       (-> (IOAccess/newBuilder)
           (cond->
             roots-fn
             (.fileSystem (sandbox-fs/confined-filesystem roots-fn outbox gate-fn)))
           (.allowHostSocketAccess net?)
           (.build))
       IOAccess/NONE)

     ;; THIS session's own Engine. It is closed by `dispose-environment!` right
     ;; after the Context, and closing it is what actually returns the memory:
     ;; an Engine retains every Context ever built on it (see `new-engine!`).
     ^Engine engine
     (new-engine!)

     ctx
     (->
       (Context/newBuilder (into-array String ["python"]))
       (.engine engine)
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
       ;; `vis-agent python` (CLI) wires the human's REAL stdin so guest `sys.stdin`
       ;; works alongside `-c`/FILE (real-python semantics), and its REAL stderr so
       ;; guest `sys.stderr` reaches the terminal: a Context with no `.err` inherits
       ;; `System/err`, which `config/init-cli!` has already pointed at vis.log — so
       ;; every guest traceback and warning was silently swallowed. Agent sandboxes
       ;; pass nil for both and keep the defaults — unchanged.
       (cond->
         stdin
         (.in ^java.io.InputStream stdin)

         stderr
         (.err ^java.io.OutputStream stderr))
       ;; Optional GraalPy background cycle-detector tuning (native-ext RSS);
       ;; a no-op unless a VIS_PY_GC_* env var is set.
       (apply-py-gc-options!)
       (.build))

     _
     (.put ctx->stdout ctx stdout-baos)

     g
     (.getBindings ctx "python")]

    ;; Tiny stdlib conveniences as Python builtins (not globals):
    ;; `json.dumps(...)` and `shlex.quote(...)` work in every python_execution
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
    (try
      (let
        [by-py-name
         (fn [sym->text]
           (reduce (fn [m [sym _]]
                     (if-let [d (get sym->text sym)]
                       (reduce #(assoc %1 %2 d)
                               m
                               (cons (sym->py-name sym) (py-aliases-for-sym sym)))
                       m))
                   {}
                   (or custom-bindings {})))

         py-docs
         (by-py-name (extension/sandbox-symbol-docs))

         ;; The signature twin of the docs: `__vis_sigs__` is the declared
         ;; parameter list a deferred tool reports through `__wrapped__`, so
         ;; `inspect.signature(grep)` / `help(grep)` answer with the contract
         ;; instead of the async trampoline's own `(*a, **k)`.
         py-sigs
         (by-py-name (extension/sandbox-symbol-signatures))

         ;; The requiredness twin: `__vis_keys__` is the options-dict vocabulary
         ;; `doc(name)` prints under the call line — which keys the dict must
         ;; carry, and which it may omit. Structure, so it never enters the
         ;; document text `apropos` ranks.
         py-keys
         (by-py-name (extension/sandbox-symbol-keys))]

        (when (seq py-docs)
          (.putMember g "__vis_docs_json__" (json/write-json-str py-docs))
          (.eval ctx
                 "python"
                 "globals().setdefault('__vis_docs__', {}).update(json.loads(__vis_docs_json__))")
          (.putMember g "__vis_docs_json__" nil))
        (when (seq py-sigs)
          (.putMember g "__vis_sigs_json__" (json/write-json-str py-sigs))
          (.eval ctx
                 "python"
                 "globals().setdefault('__vis_sigs__', {}).update(json.loads(__vis_sigs_json__))")
          (.putMember g "__vis_sigs_json__" nil))
        (when (seq py-keys)
          (.putMember g "__vis_keys_json__" (json/write-json-str py-keys))
          (.eval ctx
                 "python"
                 "globals().setdefault('__vis_keys__', {}).update(json.loads(__vis_keys_json__))")
          (.putMember g "__vis_keys_json__" nil)))
      (catch Throwable _ nil))
    ;; POSIX refusal: subprocess / os.system / os.popen point at the `shell`
    ;; tool instead of spawning. Eval'd BEFORE the initial-ns-keys snapshot so
    ;; any names it parks are baseline (filtered out of the live-vars view).
    (install-process-surface! ctx)
    ;; SANDBOX SHIMS: install every extension-contributed Python shim (host
    ;; bridge callables wired onto `g`, then the shim's source eval'd). This
    ;; is the GENERIC mechanism — `yaml` (YAMLStar) and `matplotlib` (imaging)
    ;; ship as built-in shim extensions, third-party extensions can add more.
    ;; Eval'd BEFORE the snapshot so each shim's `__vis_*`/published-module
    ;; names are baseline (filtered out of the model-visible live-vars view).
    (install-sandbox-shims! ctx g)
    ;; Advertise exact import names and direct globals to the sandbox's discovery
    ;; surface. `:shim/name` is registry identity only and must never imply importability.
    ;; `doc()` answers with `:shim/docs` when the shim declares one — the full
    ;; surface, PULLED — and falls back to the short `:shim/description` the
    ;; system prompt pushes. A shim whose own Python already documents a global
    ;; KEEPS that call contract — the argument list is what a caller types — and
    ;; gains the page under it, because a page nothing reaches was cut, not moved.
    (try
      (let
        [shims
         (registered-sandbox-shims)

         names
         (->> shims
              (mapcat #(concat (:shim/imports %) (:shim/globals %)))
              (filter #(and (string? %) (not (str/blank? %))))
              distinct
              vec)

         docs
         (reduce (fn [m s]
                   (let
                     [page
                      (when-not (str/blank? (:shim/docs s)) (:shim/docs s))

                      solo
                      (or page (:shim/description s))

                      entry
                      (cond->
                        {"solo" (if (str/blank? solo)
                                  "sandbox shim capability"
                                  (str "sandbox shim — " solo))}
                        page
                        (assoc "page" page))]

                     (reduce #(assoc %1 %2 entry) m (concat (:shim/imports s) (:shim/globals s)))))
                 {}
                 shims)]

        (when (seq names)
          (.putMember g "__vis_shims_json__" (json/write-json-str names))
          (.eval ctx "python" "globals()['__vis_shims__'] = json.loads(__vis_shims_json__)")
          (.putMember g "__vis_shims_json__" nil))
        (when (seq docs)
          (.putMember g "__vis_docs_json__" (json/write-json-str docs))
          (.eval ctx
                 "python"
                 (str "_vis_docs = globals().setdefault('__vis_docs__', {})\n"
                      "for _k, _v in json.loads(__vis_docs_json__).items():\n"
                      "    _own = _vis_docs.get(_k)\n"
                      "    _page = _v.get('page')\n" "    if not _own:\n"
                      "        _vis_docs[_k] = _v['solo']\n"
                      "    elif _page and _page not in _own:\n"
                      "        _vis_docs[_k] = _own + '\\n\\n' + _page\n"
                      "del _vis_docs, _k, _v, _own, _page"))
          (.putMember g "__vis_docs_json__" nil))
        (seed-py-map! ctx
                      g
                      "__vis_kinds__"
                      (into {}
                            (map (fn [n]
                                   [n "shim"]))
                            names)))
      (catch Throwable _ nil))
    ;; DOCUMENTS — every skill, every documentation page, every MCP tool — are NOT
    ;; seeded here: `sandbox-corpus` reads them LIVE off `doc-corpus/entries` on
    ;; every `doc`/`apropos`. This context outlives every `/reload`, so a copy taken
    ;; now would answer a skill added or edited mid-session with the corpus of an
    ;; hour ago, and would keep answering for one the user deleted.
    ;;
    ;; The one document that depends on THIS context's bindings still seeds here:
    ;; with the shell tools OFF the `shell` verb is simply not bound, and a
    ;; shell-shaped query would answer with silence — which reads as "no process can
    ;; be started in this product". It can: the toggle closes the MODEL's door only.
    ;; Same sentences as the POSIX refusal, never a copy. Best-effort: a registry
    ;; hiccup must never break context creation.
    (try (when-not (some (fn [[sym _]]
                           (= "shell" (sym->py-name sym)))
                         (or custom-bindings {}))
           (seed-py-map! ctx
                         g
                         "__vis_docs__"
                         {"shell"
                          (str (get PROCESS_SURFACE "off") " " (get PROCESS_SURFACE "extension"))}))
         (catch Throwable _ nil))
    ;; ASYNC-BY-DEFAULT runtime: install the trampoline + `gather`, then DEFER
    ;; every tool binding (so `await grep(x)` / `gather(grep(x), grep(y))` work).
    ;; `__vis_par__` (the bounded host platform pool) is wired as a binding above;
    ;; the compaction verbs (`fold_session`/`session_drop`) stay direct (never deferred).
    ;; Eval'd before the snapshot so all `__vis_*` names land in the baseline.
    (.eval ctx "python" async-runtime-python)
    ;; Auto-import `re` so the model can use regex without writing `import re`.
    ;; Eval'd before the snapshot so `re` is a BASELINE name (not surfaced as a
    ;; model-created live var). The model may still `import re` — re isn't
    ;; protected, so the redundant import is a harmless no-op.
    (.eval ctx "python" "import re")
    ;; OUTBOX (DORMANT — `outbox` is always nil, so neither the global nor the
    ;; env var exists in the sandbox): it exposed the per-context capture dir to
    ;; Python as a `VIS_OUTBOX` global and `$VIS_OUTBOX` env var, and a file the
    ;; sandbox WROTE there was captured at the source as a durable iteration
    ;; attachment (the `sandbox-fs` outbox tap) — the implicit twin of `attach`
    ;; for libraries that only know how to write a file. Retired in favour of
    ;; `attach` alone; see `mpl-capture/incidental-capture-enabled?`. Eval'd
    ;; before the snapshot so `VIS_OUTBOX` stayed a BASELINE name (not surfaced
    ;; as a model-created live var).
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
    ;; DEV network-filter loop: an in-sandbox `network_probe([method,] url)` +
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
                       (wrap-ifn (fn [method target headers body]
                                   (report-fn method target headers body))))
           (.eval ctx "python" network-probe-python))
         (catch Throwable _ nil))
    (let
      [defer-names (->> (or custom-bindings {})
                        (filter (fn [[_ v]]
                                  (fn? v)))
                        (mapcat (fn [[sym _]]
                                  (cons (sym->py-name sym) (py-aliases-for-sym sym))))
                        (remove #{"fold_session" "__vis_par__" "__vis_par_isolated__"})
                        distinct
                        vec)]
      (.putMember g "__vis_defer_names__" (->py defer-names))
      (.eval ctx "python" "__vis_defer_tools__()")
      ;; Docs and signatures are seeded ABOVE and the shims after them — so the
      ;; stamp runs LAST, when every tool's contract is already in
      ;; `__vis_docs__`/`__vis_sigs__` for it to carry.
      (.eval ctx "python" "__vis_stamp_tools__()"))
    ;; The DIRECT (never-deferred) verbs keep their synchronous identity but still
    ;; accept Python kwargs: wrap them so `fold_session(key, gist="…")` folds
    ;; its kwargs into ONE trailing dict positional the Clojure verb unwraps
    ;; (a bare ProxyExecutable is positional-only and would raise a TypeError).
    (let
      [direct-names (->> (or custom-bindings {})
                         (filter (fn [[_ v]]
                                   (fn? v)))
                         (mapcat (fn [[sym _]]
                                   (cons (sym->py-name sym) (py-aliases-for-sym sym))))
                         (filter #{"fold_session"})
                         distinct
                         vec)]
      (when (seq direct-names)
        (.putMember g "__vis_direct_names__" (->py direct-names))
        (.eval ctx "python" "__vis_kwargs_direct_tools__()")))
    {:python-context ctx
     ;; The caller OWNS this engine and must close it after the context —
     ;; `dispose-environment!` does. Dropping it on the floor is the leak
     ;; `new-engine!` exists to prevent.
     :python-engine engine
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
                              "OR run the source runtime on a stock (non-GraalVM) JDK 25 "
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
   Every session Context rides its OWN `Engine` (see `new-engine!`); parsing and
   extension shims live inside that same Context. Rendering is pure JVM code, so
   normal sessions allocate no auxiliary GraalPy contexts. Extensions
   deliberately SHARE this one session Context (installed as guest callables, not
   separate contexts), so a session holds exactly one Context. The 4-arity `stdin`
   (optional InputStream) is wired to the guest `sys.stdin` — used by `vis-agent python`
   to forward the caller's real stdin; agent sandboxes leave it nil. The 5-arity
   `stderr` (optional OutputStream) does the same for guest `sys.stderr`; nil keeps
   the JVM's `System/err`. The 6-arity `gate-fn` is the `:fs/access` gate the
   confined filesystem asks before every path operation (see
   `extension/fs-access-gate`); nil leaves only root confinement."
  ([custom-bindings] (create-python-context custom-bindings nil nil nil nil nil))
  ([custom-bindings roots-fn] (create-python-context custom-bindings roots-fn nil nil nil nil))
  ([custom-bindings roots-fn network-opts]
   (create-python-context custom-bindings roots-fn network-opts nil nil nil))
  ([custom-bindings roots-fn network-opts stdin]
   (create-python-context custom-bindings roots-fn network-opts stdin nil nil))
  ([custom-bindings roots-fn network-opts stdin stderr]
   (create-python-context custom-bindings roots-fn network-opts stdin stderr nil))
  ([custom-bindings roots-fn network-opts stdin stderr gate-fn]
   @graalvm-runtime-checked
   (build-agent-context custom-bindings roots-fn network-opts stdin stderr gate-fn)))

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

(def ^:private gil-budget-ms
  "How long best-effort guest work may wait for the Python GIL before giving up.

   Small on purpose: [[collect-garbage!]] runs in `loop/send!`'s `finally` and
   [[persist-session-defs!]] right after EVERY block, i.e. AFTER the turn's answer
   exists and BEFORE the gateway lands the turn's terminal event."
  2000)

(def ^:private detached-guest-work
  "Keys - `[context-identity purpose]` - whose best-effort guest thread was abandoned
   still parked on the GIL. Without it every subsequent turn of that session would add
   another parked thread to a GIL its dead owner will never release."
  (java.util.concurrent.ConcurrentHashMap/newKeySet))

(defn- run-detached-guest-work!
  "Run `work-fn` on a daemon thread named `thread-name` and wait at most
   [[gil-budget-ms]] for its value; nil when the budget expires or an earlier run for
   `key` is still parked.

   THE guard for best-effort guest work that sits between the engine unwinding and
   `gateway.state/run-turn!` appending the turn's terminal event. `.eval` first acquires
   this context's Python GIL. In-flight guest work is NOT what holds it up - GraalPy
   releases the GIL around foreign calls (`PythonContext.releaseGilAroundForeignCall`;
   `PythonLanguage.shouldGilBeLockedDuringForeignCalls` defaults to false), which is why
   a sibling thread keeps ticking through a whole `sh.wait`. What blocks here is a
   LEAKED GIL: a guest thread `Thread.interrupt`-ed at a GIL boundary and then abandoned
   dies inside `PythonContext.ensureGilAfterFailure`, which takes the lock
   UNINTERRUPTIBLY, and a `ReentrantLock` whose owner is dead is never released by
   anyone. Waiting for that is unbounded AND uninterruptible: a cancelled token does not
   unpark `PythonContext.acquireGil`. One such context wedged a finished turn forever -
   no `turn.completed` / `turn.cancelled` on the wire, the session pinned to a turn
   nobody was running, the queued backlog never drained, and every channel showed a live
   panel that Esc could not close. So give it a budget and walk away; the abandoned
   daemon thread finishes whenever the GIL frees. `rt/guest-safepoint!` is what keeps a
   cancel from leaking one in the first place."
  [key thread-name work-fn]
  (when (.add ^java.util.Set detached-guest-work key)
    (let [done (promise)]
      (doto (Thread. ^Runnable
                     (fn []
                       (try (deliver done (work-fn))
                            (catch Throwable _ nil)
                            (finally (.remove ^java.util.Set detached-guest-work key)
                                     (deliver done nil))))
                     ^String thread-name)
        (.setDaemon true)
        (.start))
      (deref done gil-budget-ms nil))))

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
   pause. Never throws; a closed/cancelled context is ignored, and a wedged one
   costs [[run-detached-guest-work!]]'s budget instead of the turn."
  [environment]
  (when-let [python-context (:python-context environment)]
    ;; Layer 2 only pays off once the guest collect actually ran.
    (when (run-detached-guest-work!
            [(System/identityHashCode python-context) :gc]
            "vis-python-gc"
            (fn []
              (try (.eval ^Context python-context "python" "import gc\ngc.collect()")
                   (catch Throwable _ nil))
              true))
      (try (System/gc) (catch Throwable _ nil)))))

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

(def ^:private ^java.util.Map block-failure-memory
  "Per-context memory of the LAST failing block: `Context -> [code message count]`.

   A `WeakHashMap` on purpose — the entry dies with the context, so a disposed
   or reloaded sandbox leaves nothing behind."
  (java.util.Collections/synchronizedMap (java.util.WeakHashMap.)))

(def ^:private repeat-breaker-threshold
  "Consecutive identical (code, error) failures before the loop breaker fires.
   Two retries is normal recovery; the third is a loop."
  3)

(defn- note-block-failure!
  "Record this failure and return how many times IN A ROW this exact `code` has
   failed on `python-context` with this exact `message`."
  [python-context code message]
  (if (nil? python-context)
    1
    (let
      [prev
       (.get block-failure-memory python-context)

       n
       (if (and (vector? prev) (= (nth prev 0) code) (= (nth prev 1) message))
         (inc (long (nth prev 2)))
         1)]

      (.put block-failure-memory python-context [code message n])
      n)))

(defn- clear-block-failures!
  "Forget a context's failure streak. Called on every SUCCESSFUL block so
   `note-block-failure!` only ever counts CONSECUTIVE identical failures."
  [python-context]
  (when python-context (.remove block-failure-memory python-context))
  nil)

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

     ;; A SyntaxError CARRIES its own position (`lineno`/`offset`, 1-based, over the
     ;; user's block source). `loc` is where it was RAISED, which for a compile error
     ;; re-raised from the preamble (or a check in it) is a line the USER never
     ;; wrote — so for a syntax error the exception's own attributes win whenever
     ;; they fit inside the block.
     syntax-pos
     (when (and syntax? code)
       (try (let
              [g
               (when (.isGuestException e) (.getGuestObject e))

               member
               (fn [^String n]
                 (when (and g (.hasMember ^org.graalvm.polyglot.Value g n))
                   (let [v (.getMember ^org.graalvm.polyglot.Value g n)]
                     (when (and v (.fitsInInt ^org.graalvm.polyglot.Value v))
                       (.asInt ^org.graalvm.polyglot.Value v)))))

               ln
               (member "lineno")

               off
               (member "offset")]

              (when (and ln (pos? (long ln)) (<= (long ln) (count (str/split-lines (str code)))))
                [(long ln) (max 0 (dec (long (or off 1)))) nil]))
            (catch Throwable _ nil)))

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
     ;; no hint that the tool merely needs enabling (e.g. a call to `shell`
     ;; while the "shell" toggle is off only yields "shell is not defined").
     ;; Point it at apropos + the user instead
     ;; of letting it retry a name that will never resolve on its own.
     undefined-name
     (when (and (not host?) (not syntax?) base)
       (second (re-find #"name '([^']+)' is not defined" (str base))))

     ;; Sandbox denial markers are produced by Vis's policy layer. Unlike a raw
     ;; EACCES/EPERM from Seatbelt or bubblewrap, they are deterministic and safe
     ;; to report because they contain neither the denied path nor approved roots.
     sandbox-denial-operation
     (some->>
       base
       str
       (re-find
         #"\[vis:sandbox_denied\] operation=([^ ]+) reason=outside_approved_filesystem_roots")
       second)

     ;; Generic sandbox capability denial — opaque runtime errors which cannot be
     ;; distinguished reliably from a host-level permission failure.
     sandbox-denied?
     (boolean
       (and
         base
         (re-find
           #"Operation is not allowed for|Operation not permitted|PermissionError|was excluded|UnsupportedPosixFeature"
           (str base))))

     ;; A HOST NullPointerException — Java's "Cannot invoke ... because ... is
     ;; null" / a bare "... is null" leaking from a tool or engine binding. This
     ;; is an INTERNAL fault, not the model's Python; with no hint it reads like
     ;; the model's OWN bug and gets retried verbatim forever. Flag it so the hint
     ;; steers to a different approach instead of a doomed retry.
     host-npe?
     (boolean
       (or (and host?
                (or (instance? NullPointerException cause)
                    (and base
                         (re-find
                           #"Cannot invoke .+ because .+ is null|\bis null\b|NullPointerException"
                           (str base)))))
           ;; Truffle's INTERNAL null-receiver NPE (an interop message sent to a raw
           ;; host null) is NOT reported as a host exception, so it used to reach the
           ;; model as a bare Java line with no guidance at all.
           (and base
                (re-find #"Null receiver values are not supported|\bNullPointerException\b"
                         (str base)))))

     ;; A genuine FOREIGN/polyglot value — a tool result whose interop wrapper
     ;; really lacks that method (GraalPy: "foreign object has no attribute …").
     foreign-attr
     (when (and (not host?) base)
       (second (re-find #"foreign object has no attribute '([^']+)'" (str base))))

     ;; A dict-method call on a value GraalPy names by a base type — `str`,
     ;; `list`, etc. From the AttributeError text alone this is INDISTINGUISHABLE
     ;; between a genuine host value and a FOREIGN tool-result row reported by its
     ;; element type (e.g. `await lst()` → a list). Capture [type attr] so the
     ;; steer NAMES the type instead of guessing which reading applies.
     native-nondict
     (when (and (not host?) base)
       (next
         (re-find
           #"'(list|tuple|str|int|float|bool|NoneType|set)' object has no attribute '(get|items|keys|values)'"
           (str base))))

     ;; The GraalPy CONTEXT itself is gone (issue #97): a host-phase teardown, a
     ;; `/reload`, or a cancellation closed it, and every later block then died
     ;; with GraalVM's bare "Context execution was cancelled." — no reason and no
     ;; recovery, so the model kept retrying valid code until the turn burned out.
     ;; ASK the exception first — `PolyglotException.isCancelled` is GraalVM's own
     ;; structural answer; the text match stays only for a cancellation RELAYED as
     ;; some other throwable (host wrapper, closed-context IllegalStateException).
     context-cancelled?
     (boolean (or (and (instance? PolyglotException e) (.isCancelled ^PolyglotException e))
                  (and
                    base
                    (re-find
                      #"Context execution was cancelled|Context is closed|Context is already closed"
                      (str base)))))

     ;; Python indentation slip (a block not indented, or a stray indent).
     indent?
     (boolean (and base
                   (re-find #"IndentationError|unexpected indent|expected an indented block"
                            (str base))))

     hint
     (cond
       context-cancelled?
       (str "The Python sandbox CONTEXT was torn down (cancelled or closed) — every import, "
            "variable, and definition from earlier blocks is GONE, so re-running the same "
            "block keeps failing. Recover in ONE fresh block: re-import what you need "
            "and rebuild any state before "
            "continuing. If it repeats, stop and tell the USER. Original error: ")
       prose-hint prose-hint
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
       foreign-attr (str "`." foreign-attr
                         "` failed: foreign/polyglot value, no such method. Read it with "
                         "result[\"key\"] or result[0]. Original error: ")
       native-nondict (str "`." (second native-nondict)
                           "` failed: value is a `" (first native-nondict)
                           "`, not a dict. Index it (result[0] / result[\"key\"]) or guard with "
                           "`isinstance(x, dict)`. Original error: ")
       indent? (str "Python is INDENTATION-sensitive: a block (after def / if / for / with / "
                    "a trailing `:`) must be indented consistently (4 spaces), and a top-level "
                    "statement must start at column 0. Re-indent that region. Original error: ")
       host-npe? (str "Engine fault: a host call returned null (Java NullPointerException) — "
                      "usually NOT your Python. Check what the failing expression indexes or "
                      "prints, then try another approach; if it repeats, tell the USER. "
                      "Original error: ")
       sandbox-denial-operation
       (str
         "Sandbox policy denied " sandbox-denial-operation
         ": the resource is outside approved filesystem roots. "
         "Use grep({\"query\": q}) or cat(path, start, end) to read, patch(path, edits) to edit, "
         "repl_eval(language, code) for project code, "
         "or ask the USER to add the path to workspace.filesystem in vis.yml and run /reload. Original error: ")
       sandbox-denied?
       (str
         "Your sandbox has NO real filesystem / native / process access — "
         "importlib + exec_module on a project file, open(), subprocess, and sockets "
         "CANNOT run here. To READ a project file use grep({\"query\": q}) or cat(path, start, end); to RUN project code "
         "(import its modules, use its deps) use repl_eval(language, code) — that runs "
         "in the project's interpreter where the file is importable. Original error: "))

     ;; FAILING SOURCE POSITION, babashka-style. A runtime error's top-level
     ;; PolyglotException loses its guest frames to the async trampoline, so the
     ;; Python side stashed the deepest user-code frame in `__vis_err_pos__`
     ;; ([line col end-col], col 0-based); a syntax/compile error carries it on
     ;; `loc` (1-based). Runtime pos WINS over `loc` (nil or shallow-wrong there).
     err-pos
     (when (and (not host?) (not syntax?))
       ;; Computed HERE, not in the guest `except`: the walk touches traceback
       ;; frames and can raise an internal Truffle NPE on a warm interpreter,
       ;; which is catchable at this host boundary but not in guest Python.
       (try (->clj (.eval ^Context python-context "python" "__vis_err_pos_now__()"))
            (catch Throwable _ nil)))

     pos
     (cond (and (sequential? err-pos) (first err-pos)) [(long (nth err-pos 0))
                                                        (some-> (nth err-pos 1)
                                                                long)
                                                        (some-> (nth err-pos 2)
                                                                long)]
           (some? syntax-pos) syntax-pos
           (some? loc) [(.getStartLine loc) (max 0 (dec (.getStartColumn loc))) nil]
           :else nil)

     source-context
     (when pos (render-source-context code (nth pos 0) (nth pos 1) (nth pos 2)))

     ;; Loop breaker (issue #97): the model cannot see its own retry history, so
     ;; one identical block failing identically was re-sent over and over until
     ;; the turn burned out. Say it out loud on the Nth strike.
     repeats
     (note-block-failure! python-context code (str base))

     breaker
     (when (>= (long repeats) (long repeat-breaker-threshold))
       (str
         "LOOP BREAKER: this EXACT block has now failed " repeats
         " times in a row with the SAME error. Re-sending it verbatim will fail again — "
         "change the approach, use a different tool, or tell the USER what is blocking you.\n\n"))

     msg
     (str breaker
          (cond-> (if hint (str hint base) base)
            source-context
            (str "\n\n" source-context)))]

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

             host-npe?
             (assoc :host-null? true)

             context-cancelled?
             (assoc :context-cancelled? true)

             (>= (long repeats) (long repeat-breaker-threshold))
             (assoc :repeated-failures repeats)

             undefined-name
             (assoc :name-undefined?
               true :undefined-name
               undefined-name)

             ;; ex-data from a Clojure tool's ex-info rides through so e.g.
             ;; :tool/banned, :vis/* keep their type for the trailer — minus
             ;; host noise (nested envelope / Java trace, see sanitize-cause-data).
             (and cause (instance? clojure.lang.IExceptionInfo cause))
             (merge (sanitize-cause-data (ex-data cause) base)))}))


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


(defn- ensure-async-runtime!
  "The live `__vis_run_async__` callable, REINSTALLING the async runtime first when
   it is gone. A block is allowed to run `globals().clear()` (or `del
   __vis_run_async__`) — CPython permits it — and without this the interpreter would
   be DEAD for the rest of the session: the host would execute a null member and
   EVERY later block would die with a bare `NullPointerException` (`this.run_async
   is null`), unrecoverable without restarting vis. The runtime preamble is a pure
   string, so re-eval'ing it always works; bindings the block itself deleted stay
   deleted (its own doing) and simply raise NameError."
  ^Value [^Context ctx ^Value g]
  (let
    [live (fn []
            (let [^Value v (try (.getMember g "__vis_run_async__") (catch Throwable _ nil))]
              (when (and v (not (.isNull v)) (.canExecute v)) v)))]
    (or (live)
        (do
          ;; UNWRAP `print` first. The preamble captures `__vis_real_print__ = print`
          ;; and its wrapper resolves that name at CALL time, so re-eval'ing over a
          ;; still-installed wrapper makes `print` call ITSELF forever (RecursionError
          ;; on the next print). Restoring the real builtin keeps re-install idempotent.
          (try (.eval ctx
                      "python"
                      (str "import builtins as __vis_b__\n" "globals()['print'] = __vis_b__.print\n"
                           "globals().pop('__vis_real_print__', None)\n" "del __vis_b__"))
               (catch Throwable _ nil))
          (try (.eval ctx "python" ^String async-runtime-python) (catch Throwable _ nil))
          (live)))))


(defn- run-async-program
  "Run the program as ONE driven coroutine. `__vis_run_async__` AST-wraps it in
   an `async def` (with `global` decls for its assigned names so they persist in
   the interpreter), the trampoline `__vis_drive__` drives it, and `gather`
   overlaps awaitables on the bounded host platform pool. Returns the FLAT sum
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
     ^Value run-async
     (ensure-async-runtime! ctx g)

     read-out
     (fn []
       (when baos
         (let [s (baos->str baos)]
           (when-not (str/blank? s) s))))

     sink
     (atom [])

     outbox-seen
     (atom #{})]

    (with-bindings {#'extension/*current-form-idx* 0
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

           ;; Artifacts the block PRODUCED (matplotlib show/savefig, or an
           ;; `attach` call), captured at the source into the per-block
           ;; sink — folded in as `:attachments` so the loop OWNS the bytes with
           ;; NO stdout-fence parsing.
           attachments
           (mpl-capture/drain sink)]

          (.putMember g "__vis_async_result__" nil) ;; clear stash for the next turn
          ;; A clean block ENDS any failure streak, so the loop breaker only ever
          ;; counts CONSECUTIVE identical failures.
          (clear-block-failures! ctx)
          ;; FLAT sum type — success is ONE CONTEXT channel, never both:
          ;;   - printed output (`:stdout`) → the python_execution result; OR
          ;;   - the returned value (`:result`) → a block that returned without printing.
          ;; Printed output WINS. `:attachments` ride alongside EITHER — a
          ;; produced-artifact channel, not context.
          (if out
            (cond-> {:stdout out}
              attachments
              (assoc :attachments attachments))
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

(defn- strip-protected-imports
  "AST-normalize imports of protected sandbox builtins (e.g. `from asyncio import
   gather`) in `code` via the `__vis_strip_protected_imports__` preamble helper,
   so a redundant import of a builtin becomes a no-op instead of shadowing it.
   Returns the original code unchanged when nothing is rewritten or on any
   failure."
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
     {:result <value>}     ; SUCCESS — the block's returned value (nothing printed)
     {:error  <op-error>}  ; FAILURE — the raised error IS the result

   `__vis_run_async__` AST-wraps the block in an `async def`, AUTO-SETTLES every
   bare tool-call STATEMENT at every depth (so `grep(x)` without `await` runs even
   inside `try:` or a `def` body), drives it
   as a single coroutine, and maps any raised exception against the WHOLE source.
   The program runs exactly as the model wrote it — Python's own
   halt-on-exception decides what ran. Assigning a bound tool name is allowed:
   `__vis_run_async__` keeps that binding BLOCK-LOCAL, so the shadow works here
   and the callable survives for the next block."
  [python-context code & [_opts]]
  (let
    [ctx
     ^Context python-context

     g
     (.getBindings ctx "python")

     ;; Normalize redundant imports of protected builtins (e.g. `from asyncio
     ;; import gather`) at the AST level before running — so they're a silent
     ;; no-op instead of hiding the builtin.
     code
     (strip-protected-imports ctx g code)]

    (if-let [err (empty-block-error ctx code)]
      {:result nil :forms [{:source code :error err}] :error err}
      ;; ONE whole-block path. `__vis_run_async__` AST-wraps the program in an
      ;; `async def`, AUTO-SETTLES every bare tool-call STATEMENT at every depth (so
      ;; `grep(x)` without `await` RUNS even inside `try:` or a `def`), drives it as
      ;; a single coroutine, and reports
      ;; any error against the WHOLE source. The block runs as the model wrote it,
      ;; so its outcome is the flat `{:stdout}` | `{:result}` | `{:error}` sum.
      (run-async-program ctx g code))))

;; =============================================================================
;; Engine-owned sandbox names + persisted session helper definitions
;; =============================================================================

(def SYSTEM_VAR_NAMES "Engine-owned symbols hidden from user live-var listings." '#{session})

(defn system-var-sym? [sym] (contains? SYSTEM_VAR_NAMES sym))

(def ^:private session-defs-max-bytes
  "Cap on ONE session's persisted helper source. Helpers are small; anything past
   this is a runaway generator, not a toolbox, and is neither written nor read."
  262144)

(def ^:private last-session-defs
  "session-id -> the snapshot last written, so an unchanged toolbox re-writes nothing."
  (atom {}))

(defn persist-session-defs!
  "Write this session's own `def`s beside the session, for a LATER process.

   Globals persist naturally across turns because the interpreter does - but the
   interpreter dies with the PROCESS. Restart the gateway and every helper the
   session refined is gone while the transcript still shows it, so the next call
   is a NameError against code the model can still read. `__vis_defs_snapshot__`
   renders the module aliases, scalar constants and function sources that
   re-create them; this stores that text at `paths/sandbox-defs-file`.

   Best effort and never in a block's way - which covers BLOCKING, not just throwing:
   this runs on the turn thread after every block, one line before the turn's outcome
   is persisted, so it is bounded by [[run-detached-guest-work!]]. Returns the file
   when it wrote one, nil otherwise."
  [python-context session-id]
  (when (and python-context session-id)
    (run-detached-guest-work!
      [(System/identityHashCode python-context) :defs]
      "vis-python-defs"
      (fn []
        (try (let
               [v
                (.eval ^Context python-context "python" "__vis_defs_snapshot__()")

                src
                (when (and v (.isString ^Value v)) (.asString ^Value v))

                f
                (io/file (paths/sandbox-defs-file (str session-id)))]

               (cond
                 ;; Nothing defined (or every helper deleted): drop the stale file so a
                 ;; restart restores exactly what the session has now, not what it had.
                 (str/blank? src)
                 (do (swap! last-session-defs dissoc session-id) (when (.exists f) (.delete f)) nil)
                 (> (count src) (long session-defs-max-bytes)) nil
                 (= src (get @last-session-defs session-id)) nil
                 :else (do (io/make-parents f)
                           (spit f src)
                           (swap! last-session-defs assoc session-id src)
                           f)))
             (catch Throwable _ nil))))))

(defn forget-session-defs!
  "Drop `session-id`'s entry from the in-memory dedup map.

   Called from `dispose-environment!`. [[persist-session-defs!]] only ever
   `dissoc`s when a session deletes its LAST helper, so without this a gateway
   keeps one entry — up to [[session-defs-max-bytes]] of source — for every
   helper-defining session it has ever served, long after that session's
   Context, Engine and env are gone.

   The on-disk snapshot deliberately SURVIVES: it is what a later process
   restores from ([[restore-session-defs!]]). This drops only the \"have we
   already written exactly this?\" memo, so the worst case after a forget is one
   redundant write of identical bytes."
  [session-id]
  (when session-id (swap! last-session-defs dissoc session-id) nil))

(defn restore-session-defs!
  "Re-create the helper definitions an EARLIER process persisted for `session-id`.

   Runs once, on a FRESH context, before the session's first block. The restored
   source is registered as a real block, so `defs(\"name\")` and
   `inspect.getsource` read a restored helper back exactly like a local one, and it
   goes through the SAME rewrite (`__vis_normalize_module__`) so it RUNS like one:
   a stray `await`, a plain `def` whose body awaits, a tool call in its body.
   Only `def`s and their imports/constants are stored, so nothing re-executes a
   previous block's side effects, and a statement that BINDS a name which is a
   bound tool in THIS process is dropped: a snapshot written before the tool
   existed used to overwrite it for the whole process.

   Returns the number of session-defined functions live afterwards, or nil when
   there was nothing to restore."
  [python-context session-id]
  (when (and python-context session-id)
    (try
      (let [f (io/file (paths/sandbox-defs-file (str session-id)))]
        (when (and (.isFile f) (<= (.length f) (long session-defs-max-bytes)))
          (let
            [src (slurp f)
             ^Value g (python-globals python-context)
             _ (.putMember g "__vis_restore_src__" src)
             n (.eval ^Context python-context "python" "__vis_restore_defs__(__vis_restore_src__)")]

            (.removeMember g "__vis_restore_src__")
            (swap! last-session-defs assoc session-id src)
            (when (and n (.fitsInLong ^Value n)) (.asLong ^Value n)))))
      (catch Throwable e
        (tel/log! {:level :debug :id ::restore-session-defs-failed :error e})
        nil))))
