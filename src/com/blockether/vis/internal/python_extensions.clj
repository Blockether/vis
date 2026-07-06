(ns com.blockether.vis.internal.python-extensions
  "Project-local Python extensions — trusted-context plug-ins.

   Vis extensions are normally Clojure libraries baked into the binary at
   build time. This namespace adds a second, fully dynamic authoring path:
   drop a `*.py` file into

     ~/.vis/extensions/           (global — every project)
     <project>/.vis/extensions/   (project-local — this project only)

   and it loads at startup (and on `/reload`) in BOTH the JVM and the
   GraalVM native-image build — Python redefinition is pure Truffle
   dynamism, no runtime class definition involved.

   Each file is evaluated in its own TRUSTED GraalPy context. This is NOT
   the model's sandbox: the model's context is untrusted, per-session and
   deny-by-default; extension contexts are user-trusted (same trust level
   as a Clojure extension on the classpath), process-wide, and get real
   filesystem / network / environment access. The two share nothing — the
   model can call an extension TOOL (through the host wrapper, envelope-
   checked like any tool) but can never evaluate code in the extension's
   context. Host capabilities are reachable ONLY through the bound `vis`
   API (no arbitrary Java interop: `allowAllAccess` stays false and no
   host classes are exposed).

   Every context is built on `env-python/shared-engine` — the ONE
   process-wide Engine that makes context creation safe even while a
   sandbox eval is running (see the deadlock notes there). Calls into an
   extension (tool, activation, prompt, slash, op hook) are serialized
   with `locking` on its context, the same proven pattern as the printer
   context.

   The file's top-level `vis.extension(...)` call registers through the
   ordinary `register-extension!` — from the registry's perspective a
   Python extension is indistinguishable from a Clojure one (activation,
   prompt assembly, slash dispatch, `vis extensions list` all just work).
   A file that fails to load becomes a load-failure warning (surfaced via
   `vis doctor`), never a crash."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.agents :as agents]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.extension-aggregate :as aggregate]
            [com.blockether.vis.internal.notifications :as notifications]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [org.graalvm.polyglot Context Engine EnvironmentAccess PolyglotAccess Source Value]
           [org.graalvm.polyglot.io IOAccess]
           [org.graalvm.polyglot.proxy ProxyExecutable]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; The `vis` Python module (bootstrap source)
;;
;; Evaluated in each extension context BEFORE the extension file. Builds a
;; real `vis` module (registered in `sys.modules`, so `import vis` works)
;; whose functions live in the module's own namespace — the extension
;; file's globals stay clean. Host callbacks (`__vis_host_*`, bound as
;; polyglot members before this runs) are handed in through the module
;; dict.
;; =============================================================================

(def ^:private bootstrap-python
  "import sys as _vis_sys, types as _vis_types

_vis_body = '''
import inspect

_registration = {'spec': None}

def extension(name=None, description=None, version=None, kind=None, alias=None,
              activation=None, symbols=None, prompt=None, slash_commands=None,
              op_hooks=None, ctx=None):
    if _registration['spec'] is not None:
        raise ValueError('vis.extension() may only be called once per file')
    if not name or not isinstance(name, str):
        raise ValueError('vis.extension(...) requires name=<non-empty string>')
    if not description or not isinstance(description, str):
        raise ValueError('vis.extension(...) requires description=<non-empty string>')
    if symbols and not alias:
        raise ValueError('vis.extension(...) requires alias=<string> when symbols= is declared')
    if ctx is not None and not callable(ctx):
        raise ValueError('vis.extension(...) ctx= must be a callable (env) -> dict of session contributions')
    _registration['spec'] = {
        'name': name, 'description': description, 'version': version,
        'kind': kind, 'alias': alias, 'activation': activation,
        'symbols': list(symbols or []), 'prompt': prompt,
        'slash_commands': list(slash_commands or []),
        'op_hooks': list(op_hooks or []), 'ctx': ctx,
    }

def symbol(fn, name=None, tag='observation', hidden=False):
    if not callable(fn):
        raise ValueError('vis.symbol(fn, ...) requires a callable')
    if tag not in ('observation', 'mutation'):
        raise ValueError('vis.symbol tag must be observation or mutation, got %r' % (tag,))
    doc = inspect.getdoc(fn)
    if not doc or not doc.strip():
        raise ValueError('vis.symbol: %s needs a docstring - it becomes the model-facing doc()'
                         % (getattr(fn, '__name__', '?'),))
    params, varargs = [], False
    for p in inspect.signature(fn).parameters.values():
        if p.kind == inspect.Parameter.VAR_POSITIONAL:
            varargs = True
        elif p.kind in (inspect.Parameter.POSITIONAL_ONLY,
                        inspect.Parameter.POSITIONAL_OR_KEYWORD):
            params.append(p.name)
    return {'marker': 'symbol', 'fn': fn, 'name': name or fn.__name__, 'tag': tag,
            'hidden': bool(hidden), 'doc': doc, 'params': params, 'varargs': varargs}

def slash(name, run, doc=None, usage=None):
    if not name or not isinstance(name, str):
        raise ValueError('vis.slash(name, run, ...) requires name=<non-empty string>')
    if not callable(run):
        raise ValueError('vis.slash(name, run, ...) requires a callable run')
    return {'marker': 'slash', 'name': name, 'run': run, 'doc': doc, 'usage': usage}

def op_hook(ops, fn, phase='before'):
    if phase not in ('before', 'after'):
        raise ValueError('vis.op_hook phase must be before or after, got %r' % (phase,))
    if not callable(fn):
        raise ValueError('vis.op_hook(ops, fn, ...) requires a callable fn')
    ops = [str(o) for o in (ops or [])]
    if not ops:
        raise ValueError('vis.op_hook requires a non-empty ops list')
    return {'marker': 'op_hook', 'ops': ops, 'fn': fn, 'phase': phase}

def ok(title, body=None, data=None):
    return {'marker': 'slash_result', 'status': 'ok', 'title': str(title),
            'body': body, 'data': data}

def err(title, body=None, data=None):
    return {'marker': 'slash_result', 'status': 'error', 'title': str(title),
            'body': body, 'data': data}

def block(reason):
    return {'marker': 'block', 'reason': str(reason)}

def strings_of(value):
    out = []
    def walk(v):
        if isinstance(v, str):
            out.append(v)
        elif isinstance(v, dict):
            for k, x in v.items():
                walk(k)
                walk(x)
        elif isinstance(v, (list, tuple, set)):
            for x in v:
                walk(x)
    walk(value)
    return out

class _State:
    def get(self, key, default=None):
        v = _host['state_get'](str(key))
        return default if v is None else v
    def __getitem__(self, key):
        v = _host['state_get'](str(key))
        if v is None:
            raise KeyError(key)
        return v
    def __setitem__(self, key, value):
        _host['state_put'](str(key), value)
    def __delitem__(self, key):
        _host['state_del'](str(key))
    def __contains__(self, key):
        return _host['state_get'](str(key)) is not None

state = _State()

def log(level, msg):
    _host['log'](str(level), str(msg))

def notify(text, level='info'):
    _host['notify'](str(text), str(level))
'''

_vis_mod = _vis_types.ModuleType('vis')
_vis_mod.__dict__['_host'] = {
    'state_get': __vis_host_state_get__,
    'state_put': __vis_host_state_put__,
    'state_del': __vis_host_state_del__,
    'log': __vis_host_log__,
    'notify': __vis_host_notify__,
}
exec(compile(_vis_body, '<vis-bootstrap>', 'exec'), _vis_mod.__dict__)
_vis_sys.modules['vis'] = _vis_mod

def __vis_registration__():
    return _vis_sys.modules['vis'].__dict__['_registration']['spec']
")

;; =============================================================================
;; Marshalling helpers
;; =============================================================================

(defn- ->executable
  "Wrap a Clojure fn as a Python callable: positional Python args marshal
   to Clojure via `->clj`, the return value marshals back via `->py`."
  ^ProxyExecutable [f]
  (reify
    ProxyExecutable
      (execute [_ args] (env/->py (apply f (map env/->clj args))))))

(defn- call-py
  "Enter an extension's context (serialized — Truffle contexts are single-
   entry; same `locking` pattern as the printer context) and call a Python
   callable with marshalled args. Returns the `->clj` view of the result."
  [^Context ctx ^Value f args]
  (locking ctx (env/->clj (.execute f (object-array (mapv env/->py args))))))

(defn- plainify
  "Deep-convert the `->clj` view of a Python value into plain EDN-printable
   Clojure data (ordered maps -> hash maps, seqs -> vectors) so `vis.state`
   rows round-trip through `pr-str`/`edn/read-string`."
  [x]
  (cond (map? x) (into {}
                       (map (fn [[k v]]
                              [k (plainify v)]))
                       x)
        (sequential? x) (mapv plainify x)
        (set? x) (mapv plainify x)
        :else x))

;; =============================================================================
;; Durable state (`vis.state`) — backed by the `extension_aggregate` table
;; (one row per key, kind "py-state", :global scope), NOT the filesystem.
;; State is owned by the extension NAME and survives `/reload` and restarts.
;; The live session env (carrying :db-info) is threaded in through `*state-env*`
;; when an adapter has it; otherwise `state-env` falls back to the process-wide
;; shared DB connection (the same vis.db sessions use) — all :global scope
;; needs. Values are the boundary view of Python data (plain EDN data).
;; =============================================================================

(def ^:private state-kind "py-state")

(def ^:private ^:dynamic *state-env*
  "Live session env (carrying :db-info and session/turn ids) bound around a
   call into an extension so `vis.state` reaches the extension-aggregate API
   with the session's own DB. nil outside a call — then `state-env` falls
   back to the process-wide shared connection. Also the seam tests use to
   confine `vis.state` to an in-memory DB."
  nil)

(defn- state-env
  []
  (or *state-env* {:db-info (persistance/db-shared-connection! (config/resolve-db-spec))}))

(defn- state-get*
  [k]
  (some-> (aggregate/extension-aggregate-get (state-env)
                                             {:key (str k) :kind state-kind :scope :global})
          :content))

(defn- state-put!*
  [k v]
  (aggregate/extension-aggregate-put!
    (state-env)
    {:key (str k) :kind state-kind :scope :global :content (plainify v)})
  nil)

(defn- state-del!*
  [k]
  (aggregate/extension-delete-aggregate! (state-env) {:key (str k) :kind state-kind :scope :global})
  nil)

;; =============================================================================
;; Trusted extension context
;; =============================================================================

(defn- build-context
  "Build one TRUSTED extension context on the shared Engine. Permissive
   about the world (real filesystem, sockets, subprocesses, env vars,
   threads) but strict about the host: no host interop beyond the
   explicitly bound `vis` API callbacks."
  ^Context []
  (-> (Context/newBuilder (into-array String ["python"]))
      (.engine ^Engine @env/shared-engine)
      (.allowAllAccess false)
      (.allowIO IOAccess/ALL)
      (.allowCreateThread true)
      (.allowCreateProcess true)
      (.allowNativeAccess false)
      (.allowPolyglotAccess PolyglotAccess/NONE)
      (.allowEnvironmentAccess EnvironmentAccess/INHERIT)
      (.build)))

;; Python hands LEVEL as a string; the boundary is strings-only, so the
;; lookup maps string -> the INTERNAL telemere/notification level keyword.
;; No `(keyword …)` minting of Python-supplied data.
(def ^:private log-levels {"trace" :trace "debug" :debug "info" :info "warn" :warn "error" :error})
(def ^:private notify-levels {"info" :info "success" :success "warn" :warn "error" :error})

(defn- bind-host!
  "Bind the `__vis_host_*` callbacks the bootstrap hands into the `vis`
   module. `label` is the file's name — used only for log context; durable
   state lives in the `extension_aggregate` table, owned by the running
   extension's identity (see `*state-env*`)."
  [^Context ctx label]
  (let [g (.getBindings ctx "python")]
    (.putMember g
                "__vis_host_state_get__"
                (->executable (fn [k]
                                (state-get* k))))
    (.putMember g
                "__vis_host_state_put__"
                (->executable (fn [k v]
                                (state-put!* k v))))
    (.putMember g
                "__vis_host_state_del__"
                (->executable (fn [k]
                                (state-del!* k))))
    (.putMember g
                "__vis_host_log__"
                (->executable
                  (fn [level msg]
                    (let [lvl (get log-levels (str level) :info)]
                      (tel/log!
                        {:level lvl :id ::extension-log :data {:extension label} :msg (str msg)}))
                    nil)))
    (.putMember g
                "__vis_host_notify__"
                (->executable
                  (fn [text level]
                    (notifications/notify! (str text) :level (get notify-levels (str level) :info))
                    nil)))))

;; =============================================================================
;; Adapters — Python callables wrapped as the Clojure fns the extension
;; registry expects. Every adapter is defensive: a closed context (after
;; `/reload`) or a Python error surfaces as a failure envelope / logged
;; warning, never an unhandled throw into the engine.
;; =============================================================================

(defn- slim-env
  "The read-only env dict Python activation/prompt callables receive.
   Deliberately small and documented — never the raw host env map (which
   carries atoms, contexts and other host-only handles). STRING keys —
   it crosses the strings-only boundary."
  [env]
  {"cwd" (System/getProperty "user.dir")
   "session_id" (some-> (:session-id env)
                        str)
   "channel" (some-> (:channel env)
                     str)})

(defn- call-py-ext
  "Invoke a Python callable with the extension identity bound (so `vis.state`
   host callbacks own their aggregate rows) and the live session env threaded
   through `*state-env*` (so state uses the session's own DB when available).
   Only an env that actually carries :db-info overrides the ambient
   `*state-env*` — a db-less env (or nil) keeps it, so global state still
   resolves through the shared connection / the test binding.
   Returns the `->clj` view of the result."
  [ext-name env ^Context ctx ^Value f args]
  (binding [extension/*current-extension*
            (or extension/*current-extension* {:ext/name ext-name})

            *state-env*
            (if (:db-info env) env *state-env*)]

    (call-py ctx f args)))

(defn- sctx->env
  "Minimal state env for a slash callback: the persistence handle and session
   id the dispatcher stamps onto the slash ctx (see `slash/dispatch`)."
  [sctx]
  (cond-> {}
    (:db-info sctx)
    (assoc :db-info (:db-info sctx))

    (:session/id sctx)
    (assoc :session-id (:session/id sctx))))

(defn- tool-adapter
  "Observed-tool fn for one Python-backed symbol. Return value = success
   payload; a raised Python exception = failure envelope (message + trace
   via `normalize-error`) — Python authors never construct envelopes."
  [ext-name sym ^Context ctx ^Value pyfn]
  (fn [& args]
    (try (extension/success {:result (call-py-ext ext-name nil ctx pyfn (vec args))})
         (catch Throwable t
           (extension/failure
             {:result nil :throwable t :metadata {:extension ext-name :tool (str sym)}})))))

(defn- activation-adapter
  [ext-name ^Context ctx ^Value pyfn]
  (fn [env]
    (try (boolean (call-py-ext ext-name env ctx pyfn [(slim-env env)]))
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::activation-failed
                      :data {:extension ext-name :error (ex-message t)}})
           false))))

(defn- prompt-adapter
  [ext-name ^Context ctx ^Value pyfn]
  (fn [env]
    (try (let [r (call-py-ext ext-name env ctx pyfn [(slim-env env)])]
           (when (string? r) r))
         (catch Throwable t
           (tel/log!
             {:level :warn :id ::prompt-failed :data {:extension ext-name :error (ex-message t)}})
           nil))))

(defn- ctx-adapter
  "`:ext/ctx-fn` for a Python `vis.extension(ctx=...)` callable. Runs per turn
   during ctx render: hands the Python fn the `slim-env` dict and folds the
   dict it returns into the model's `session` bag (deep-merged with every
   other extension's slice). The returned map MUST be STRING-keyed all the way
   down — the same contract as a Clojure `:ext/ctx-fn` (Python dict keys are
   strings, so this holds naturally). Non-map / error => empty contribution;
   bad optional context never blocks a turn."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [env]
    (try (let [r (call-py-ext ext-name env ctx pyfn [(slim-env env)])]
           (if (map? r) (plainify r) {}))
         (catch Throwable t
           (tel/log!
             {:level :warn :id ::ctx-failed :data {:extension ext-name :error (ex-message t)}})
           {}))))

(defn- slash-adapter
  "`:slash/run-fn` for one `vis.slash(...)` entry. The Python callable
   receives `{'channel', 'args', 'raw', 'session_id'}` and returns
   `vis.ok(...)` / `vis.err(...)` (or a plain string / None)."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [sctx]
    ;; The payload crosses INTO Python (string keys); the response crossed
    ;; BACK via `->clj` (string keys as well).
    (let [payload
          {"channel" (some-> (:channel/id sctx)
                             name)
           "args" (mapv str (:command/argv sctx))
           "raw" (str (:command/raw sctx))
           "session_id" (some-> (:session/id sctx)
                                str)}

          res
          (call-py-ext ext-name (sctx->env sctx) ctx pyfn [payload])]

      (cond (nil? res) {:slash/status :ok :slash/title (str ext-name ": done")}
            (string? res) {:slash/status :ok :slash/title res}
            (map? res) (cond-> {:slash/status (if (= "error" (get res "status")) :error :ok)}
                         (get res "title")
                         (assoc :slash/title (str (get res "title")))

                         (string? (get res "body"))
                         (assoc :slash/body (get res "body"))

                         (some? (get res "data"))
                         (assoc :slash/data (get res "data")))
            :else {:slash/status :ok :slash/title (pr-str res)}))))

(defn- guard-adapter
  "Python `phase='before'` hook -> a host :around op hook. The callable
   receives `{'op', 'args'}`; returning `vis.block(reason)` refuses the
   op with a failure envelope the model reads, returning None allows it.
   A hook error fails OPEN (op runs) — a broken guard must not brick the
   loop."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [env op-kw args next-fn]
    (let [res (try (call-py-ext ext-name env ctx pyfn [{"op" (name op-kw) "args" (vec args)}])
                   (catch Throwable t
                     (tel/log! {:level :warn
                                :id ::op-hook-failed
                                :data {:extension ext-name :op op-kw :error (ex-message t)}})
                     nil))]
      (if (and (map? res) (= "block" (get res "marker")))
        (extension/failure
          {:result nil
           :error {:message (str (or (get res "reason") "Blocked by a Python extension hook"))
                   :hint (str "Blocked by the '"
                              ext-name
                              "' Python extension. Ask the user before retrying.")}})
        (next-fn args)))))

(defn- after-adapter
  "Python `phase='after'` hook -> a host :after op hook. Observe-only:
   the callable receives `{'op', 'args', 'result'}`; its return value is
   ignored and the original result always flows on."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [env op-kw args result]
    (try (call-py-ext ext-name
                      env
                      ctx
                      pyfn
                      [{"op" (name op-kw) "args" (vec args) "result" (:result result)}])
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::op-hook-failed
                      :data {:extension ext-name :op op-kw :error (ex-message t)}})))
    result))

;; =============================================================================
;; Registration dict -> extension spec
;; =============================================================================

(defn- symbol-base-name
  "Symbol name for the registry: `name or fn.__name__` with a leading
   `<alias>_` stripped, so a module can use readable full names
   (`todo_add` under alias `todo`) without double-prefixing in the
   sandbox."
  [alias-sym ^String n]
  (let [prefix (str alias-sym "_")]
    (if (and alias-sym (str/starts-with? n prefix) (> (count n) (count prefix)))
      (subs n (count prefix))
      n)))

;; Extension TAG vocabulary: the .py author declares "observation"/"mutation";
;; the registry stores the internal tag keyword. Bounded map — no minting.
(def ^:private symbol-tags {"observation" :observation "mutation" :mutation})

(defn- ->symbol-entry
  "`spec` is a Python registration dict — STRING keys (strings-only boundary)."
  [ext-name alias-sym ^Context ctx spec]
  (let [sym
        (clojure.core/symbol (symbol-base-name alias-sym (str (get spec "name"))))

        pyfn
        (get spec "fn")

        argv
        (cond-> (mapv clojure.core/symbol (get spec "params"))
          (get spec "varargs")
          (-> (conj '&)
              (conj 'args)))]

    (cond-> #:ext.symbol{:symbol sym
                         :fn (tool-adapter ext-name sym ctx pyfn)
                         :doc (str (get spec "doc"))
                         :arglists [argv]
                         :tag (get symbol-tags (str (get spec "tag")) :observation)}
      (get spec "hidden")
      (assoc :ext.symbol/hidden? true))))

(defn- ->slash-spec
  [ext-name ^Context ctx spec]
  (let [doc
        (get spec "doc")

        usage
        (get spec "usage")]

    (cond-> {:slash/name (str (get spec "name"))
             :slash/run-fn (slash-adapter ext-name ctx (get spec "run"))}
      (string? doc)
      (assoc :slash/doc doc)

      (string? usage)
      (assoc :slash/usage usage))))

(defn- ->op-hook-entries
  [ext-name ^Context ctx spec]
  (let [before?
        (= "before" (str (get spec "phase")))

        pyfn
        (get spec "fn")

        f
        (if before? (guard-adapter ext-name ctx pyfn) (after-adapter ext-name ctx pyfn))]

    ;; `:op` keys the INTERNAL op-hook registry (keyword-keyed, matched against
    ;; canonical tool op keywords). The vocabulary is author-declared config,
    ;; not model data — the one sanctioned mint in this file.
    (mapv (fn [op]
            {:op (keyword (str op)) :phase (if before? :around :after) :fn f})
          (get spec "ops"))))

(defn- registration->spec
  "`reg` is the dict handed to Python `vis.register(...)` — STRING keys."
  [^Context ctx reg]
  (let [ext-name
        (str (get reg "name"))

        alias-sym
        (some-> (get reg "alias")
                str
                clojure.core/symbol)

        symbols
        (mapv #(->symbol-entry ext-name alias-sym ctx %) (get reg "symbols"))

        slashes
        (mapv #(->slash-spec ext-name ctx %) (get reg "slash_commands"))

        op-hooks
        (vec (mapcat #(->op-hook-entries ext-name ctx %) (get reg "op_hooks")))

        prompt
        (get reg "prompt")

        ctx-fn
        (get reg "ctx")

        activation
        (get reg "activation")]

    (cond-> {:ext/name ext-name
             :ext/description (str (get reg "description"))
             :ext/kind (str (or (get reg "kind") "python"))
             :ext/source-nses ['com.blockether.vis.internal.python-extensions]
             :ext/engine (cond-> {:ext.engine/symbols symbols}
                           alias-sym
                           (assoc :ext.engine/alias alias-sym))}
      (get reg "version")
      (assoc :ext/version (str (get reg "version")))

      (seq slashes)
      (assoc :ext/slash-commands slashes)

      (seq op-hooks)
      (assoc :ext/op-hooks op-hooks)

      (string? prompt)
      (assoc :ext/prompt-fn prompt)

      (instance? Value prompt)
      (assoc :ext/prompt-fn (prompt-adapter ext-name ctx prompt))

      (some? activation)
      (assoc :ext/activation-fn (activation-adapter ext-name ctx activation))

      (instance? Value ctx-fn)
      (assoc :ext/ctx-fn (ctx-adapter ext-name ctx ctx-fn)))))

;; =============================================================================
;; Loader
;; =============================================================================

(defonce ^:private loaded
  ;; canonical path -> {:sha :ext-name :context}
  (atom {}))

(defonce ^:private failures
  ;; [{:file :error}] from the most recent scan.
  (atom []))

(defonce ^:private last-fingerprint (atom nil))

;; Change listeners — the seam live surfaces subscribe to so a `/reload`
;; propagates beyond the global registry. Each session env caches its own
;; `:extensions` rows (slash dispatch + sandbox bindings read those, not
;; the registry), and the TUI memoizes its slash palette — both re-sync
;; through this hook. Fired AFTER a changed (re)load completes.
(defonce ^:private change-listeners (atom {}))

(defn add-change-listener!
  "Subscribe `f` to Python-extension set changes. `f` receives
   `{:extensions [<validated ext map> ...] :removed [<ext-name> ...]}`
   after every (re)load that changed anything: `:extensions` is the full
   freshly-registered set, `:removed` the names that no longer exist.
   Re-registering the same `listener-id` replaces the old listener.
   Returns `listener-id`."
  [listener-id f]
  (swap! change-listeners assoc listener-id f)
  listener-id)

(defn remove-change-listener!
  "Remove a listener registered with [[add-change-listener!]]. Returns nil."
  [listener-id]
  (swap! change-listeners dissoc listener-id)
  nil)

(defn- notify-change-listeners!
  [payload]
  (doseq [[id f] @change-listeners]
    (try (f payload)
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::change-listener-failed
                      :data {:listener id :error (ex-message t)}})))))

(defn load-failures
  "Load failures from the most recent Python-extension scan:
   `[{:file <path> :error <message>} ...]`."
  []
  @failures)

(defn loaded-python-extensions
  "Snapshot of the currently loaded Python extensions:
   `{<canonical-path> {:sha ... :ext-name ...}} ` (context handle elided)."
  []
  (into {}
        (map (fn [[p e]]
               [p (dissoc e :context :ext)]))
        @loaded))

(defn- default-extension-dirs
  []
  [(io/file (System/getProperty "user.home") ".vis" "extensions")
   (io/file (System/getProperty "user.dir") ".vis" "extensions")])

(defn- scan
  "All `*.py` files across the extension dirs, global dir first then
   project dir (name order within a dir) so a project file registering
   the same extension name wins. Deduped on canonical path."
  [dirs]
  (let [files (for [^File d (map io/file dirs)
                    :when (.isDirectory d)
                    ^File f (sort-by #(.getName ^File %) (.listFiles d))
                    :when (and (.isFile f) (str/ends-with? (.getName f) ".py"))]

                f)]
    (->> files
         (reduce (fn [[seen acc] ^File f]
                   (let [p (.getCanonicalPath f)]
                     (if (seen p) [seen acc] [(conj seen p) (conj acc f)])))
                 [#{} []])
         second)))

(defn- load-file!
  "Evaluate one extension file in a fresh trusted context and register the
   extension it declares. Returns `{:path :sha :ext-name :context}`;
   throws (with the context closed) on any failure."
  [^File f]
  (let [path
        (.getCanonicalPath f)

        source
        (slurp f)

        sha
        (extension/sha256-hex source)

        ctx
        (build-context)]

    (try (bind-host! ctx (.getName f))
         (locking ctx
           (.eval ctx "python" ^String bootstrap-python)
           (.eval ctx (.build (Source/newBuilder "python" ^String source (.getName f)))))
         (let [g
               (.getBindings ctx "python")

               reg
               (call-py ctx (.getMember g "__vis_registration__") [])]

           (when (nil? reg)
             (throw (ex-info (str (.getName f) " never called vis.extension(...)")
                             {:type ::no-registration :file path})))
           (let [spec
                 (registration->spec ctx reg)

                 validated
                 (extension/register-extension! spec)]

             (tel/log! {:level :info
                        :id ::loaded
                        :data {:file path :ext (:ext/name spec)}
                        :msg (str "Python extension '" (:ext/name spec) "' loaded from " path)})
             {:path path :sha sha :ext-name (:ext/name spec) :ext validated :context ctx}))
         (catch Throwable t (try (.close ctx true) (catch Throwable _)) (throw t)))))

(defn- teardown!
  "Deregister every loaded Python extension and close its context."
  []
  (doseq [[_ {:keys [ext-name ^Context context]}] @loaded]
    (try (extension/deregister-extension! ext-name) (catch Throwable _))
    (try (.close context true) (catch Throwable _)))
  (reset! loaded {}))

(declare register-loader-extension!)

(defn load-python-extensions!
  "Scan the Python extension dirs (default: `~/.vis/extensions` and
   `<cwd>/.vis/extensions`) and (re)load every `*.py` file. Idempotent:
   when no file changed since the last scan this is a cheap no-op. On any
   change the whole set is torn down and rebuilt (contexts are ~40ms warm
   on the shared engine) — deterministic ordering, no partial states.

   A file that fails to load is recorded in `load-failures` (and surfaced
   by `vis doctor`) — it never crashes the host.

   Returns `{:loaded n :failed n :changed? bool}`."
  ([] (load-python-extensions! nil))
  ([{:keys [dirs]}]
   (register-loader-extension!)
   (let [dirs
         (or dirs (default-extension-dirs))

         files
         (scan dirs)

         fp
         (mapv (fn [^File f]
                 [(.getCanonicalPath f) (extension/sha256-hex (slurp f))])
               files)]

     (if (= fp @last-fingerprint)
       {:loaded (count @loaded) :failed (count @failures) :changed? false}
       (let [old-names (set (map :ext-name (vals @loaded)))]
         (teardown!)
         (reset! failures [])
         (doseq [^File f files]
           (try (let [{:keys [path ext-name] :as entry} (load-file! f)]
                  ;; A later file (project dir) registering the same
                  ;; extension name supersedes the earlier one — the
                  ;; registry already swapped the registration; close the
                  ;; superseded context so its adapters can't linger.
                  (doseq [[opath {oname :ext-name ^Context octx :context}] @loaded
                          :when (and (= oname ext-name) (not= opath path))]

                    (try (.close octx true) (catch Throwable _))
                    (swap! loaded dissoc opath))
                  (swap! loaded assoc path (dissoc entry :path)))
                (catch Throwable t
                  (tel/log! {:level :warn
                             :id ::load-failed
                             :data {:file (str f) :error (ex-message t)}
                             :msg (str "Python extension failed to load: " f " — " (ex-message t))})
                  (swap! failures conj {:file (str f) :error (ex-message t)}))))
         (reset! last-fingerprint fp)
         ;; Propagate to live surfaces (cached session envs, TUI slash
         ;; palette). Without this a /reload only updates the GLOBAL
         ;; registry: new extensions stay invisible to running sessions
         ;; and stale env rows keep calling into the closed contexts.
         (let [entries (vals @loaded)
               new-names (set (map :ext-name entries))]

           (notify-change-listeners! {:extensions (vec (keep :ext entries))
                                      :removed (vec (sort (remove new-names old-names)))}))
         {:loaded (count @loaded) :failed (count @failures) :changed? true})))))

(defn reload-python-extensions!
  "Force a full reload of every Python extension (even when no file
   changed). Same return shape as `load-python-extensions!`. Live
   sessions pick the new tool bindings up at the next turn boundary."
  ([] (reload-python-extensions! nil))
  ([opts] (reset! last-fingerprint nil) (load-python-extensions! opts)))

;; =============================================================================
;; The loader's own host extension: `/reload` + doctor surface
;; =============================================================================

(defn- reload-slash
  [_ctx]
  ;; One user-facing reload for EVERY hot-reloadable resource: Python
  ;; extensions, project guidance (AGENTS.md/CLAUDE.md stack), prompt
  ;; templates, and any extension-owned discovery cache registered as a
  ;; reload hook (harness skills/agents).
  (let [{:keys [loaded failed]}
        (reload-python-extensions!)

        hook-results
        (extension/run-reload-hooks!)

        failed-hooks
        (into []
              (keep (fn [[id r]]
                      (when-not (:ok? r) id)))
              hook-results)

        guidance
        (try (agents/reload!) nil (catch Throwable t (ex-message t)))

        template-cnt
        (try (count (prompt-templates/reload!)) (catch Throwable _ nil))]

    {:slash/status (if (or (pos? (long failed)) (seq failed-hooks) guidance) :error :ok)
     :slash/title
     (str "Reloaded — Python extensions: " loaded
          " loaded" (when (pos? (long failed)) (str ", " failed " failed — see `vis doctor`"))
          "; skills/agents, prompt templates" (when template-cnt (str " (" template-cnt ")"))
          ", and context files rescanned" (when (seq failed-hooks)
                                            (str " — hook failures: "
                                                 (str/join ", " (map str failed-hooks)))))}))

(defn- doctor-fn
  [_env]
  (vec (concat (for [{:keys [file error]} @failures]
                 {:level :error
                  :check-id ::load
                  :message (str "Python extension failed to load: " file)
                  :remediation error})
               (for [[path {:keys [ext-name]}] @loaded]
                 {:level :info
                  :check-id ::load
                  :message (str "Python extension '" ext-name "' loaded from " path)}))))

(defonce ^:private loader-registered? (atom false))

(defn- register-loader-extension!
  []
  (when (compare-and-set! loader-registered? false true)
    (extension/register-extension!
      {:ext/name "python-extensions"
       :ext/description
       "Loads Python extensions from ~/.vis/extensions and <project>/.vis/extensions."
       :ext/kind "host"
       :ext/source-nses ['com.blockether.vis.internal.python-extensions]
       :ext/slash-commands
       [{:slash/name "reload"
         :slash/doc "Reload Python extensions, skills/agents, prompt templates, and context files."
         :slash/run-fn reload-slash}]
       :ext/doctor-fn doctor-fn})))
