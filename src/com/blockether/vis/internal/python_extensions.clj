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
   prompt assembly, slash dispatch, `vis-agent extension list` all just work).
   A file that fails to load becomes a load-failure warning (surfaced via
   `vis-agent doctor`), never a crash."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.agents :as agents]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.egress-proxy :as egress]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.extension-aggregate :as aggregate]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.notifications :as notifications]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.prompt-templates :as prompt-templates]
            [com.blockether.vis.internal.security-policy :as security-policy]
            [com.blockether.vis.internal.toggles :as toggles]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
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

(def ^:no-doc bootstrap-python
  "The `vis` module bootstrap. Evaluated in each extension context BEFORE the
   extension file: it builds a real `vis` module (registered in `sys.modules`, so
   `import vis` works) whose functions live in the module's own namespace, while
   the extension file's globals stay clean. Host callbacks (`__vis_host_*`, bound
   as polyglot members before this runs) are handed in through the module dict.

   The Python body is a real, lintable `.py` file under `vis-python/`, slurped
   from the classpath and embedded in the native image by build.clj's
   `-H:IncludeResources=vis-python/.*` (see `env-python/runtime-python-src`)."
  (env/runtime-python-src "vis-python/extension_bootstrap.py"))

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
  "Call a Python callable in an extension's context with marshalled args.
   Returns the `->clj` view of the result. Deliberately NO host-side lock:
   GraalPy's own GIL already serializes guest execution inside the context,
   and a Truffle-managed wait (unlike a JVM monitor) stays cancellable via
   `Context.interrupt`/`.close(true)` — so a wedged extension can be killed
   instead of queueing every later caller behind an uninterruptible monitor."
  [^Context _ctx ^Value f args]
  (env/->clj (.execute f (object-array (mapv env/->py args)))))

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

(defn- stringify-deep
  "Deep-convert host data to the strings-only shape the `->py` boundary
   accepts, so a map carrying keyword keys AND keyword values (a svar
   provider, a config, a selection event, a tool result) can cross INTO a
   Python callback. Keyword/symbol keys and values become their name string
   (leading `:` stripped, namespace kept); scalars pass through. `->py` forbids
   keywords outright, so without this an enrich-models / on-selected / render
   arg would throw a boundary violation before the Python fn ever runs."
  [x]
  (letfn [(k->s [k]
            (cond (keyword? k) (subs (str k) 1)
                  (symbol? k) (str k)
                  (string? k) k
                  :else (str k)))]
    (cond (map? x) (into {}
                         (map (fn [[k v]]
                                [(k->s k) (stringify-deep v)]))
                         x)
          (sequential? x) (mapv stringify-deep x)
          (set? x) (mapv stringify-deep x)
          (keyword? x) (subs (str x) 1)
          (symbol? x) (str x)
          :else x)))

(defn- host-tool-result
  "Unwrap a HOST tool envelope into the payload Python asked for.

   Host shell impls return either a plain result map or an
   `extension/success`/`failure` ENVELOPE — `{:result … :success? … :error …
   :metadata …}` — whose keyword keys `->py` rejects outright. Handing that
   straight back therefore killed every extension that shells out with
   `STRINGS-ONLY boundary violation: non-string-key :result`, blaming the
   extension for the framework's own payload. Python only ever wanted
   `:result`, deep-stringified; a failing envelope becomes an ordinary
   exception raised in the calling extension frame, where it can be caught.

   Non-envelope values (a plain map, a string) pass through `stringify-deep`
   unchanged, so a host callback that already returns wire data is unaffected."
  [envelope]
  (if (and (map? envelope) (contains? envelope :success?))
    (if (:success? envelope)
      (stringify-deep (:result envelope))
      (throw (ex-info (or (not-empty (str (:message (:error envelope)))) "host tool call failed")
                      {:type ::host-tool-failed :error (stringify-deep (:error envelope))})))
    (stringify-deep envelope)))

;; =============================================================================
;; Durable state (`vis.state`) — backed by the `extension_aggregate` table
;; (one row per key, kind "py-state", :global scope), NOT the filesystem.
;; State is owned by the extension NAME and survives `/reload` and restarts.
;; The live session env reaches this code as THE ONE context
;; (`extension/*current-environment*`, installed by `extension/with-context`);
;; a context without :db-info (outside a session, or a session whose env
;; carries none) falls back to the process-wide shared DB connection (the same
;; vis.db sessions use) — all :global scope needs. Values are the boundary view
;; of Python data (plain EDN data).
;; =============================================================================

(def ^:private state-kind "py-state")

(defn- state-env
  "State handle for `vis.state`: the session's OWN env when the current context
   carries a DB, else the process-wide shared connection. Tests confine
   `vis.state` to an in-memory DB by binding `extension/*current-environment*`."
  []
  (if (:db-info extension/*current-environment*)
    extension/*current-environment*
    {:db-info (persistance/db-shared-connection! (config/resolve-db-spec))}))

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

;; =============================================================================
;; Declared host environment (`vis.extension(env=["NAME", ...])`)
;;
;; An extension DECLARES the environment variables it needs; the host resolves
;; them through `config/extension-env-status` (the `environment:` declaration for
;; a declared name, the process environment for one nobody declared) and injects
;; ONLY those into the extension
;; context. There is no blanket passthrough of the HOST environment — that would
;; hand every third-party extension the user's AWS/Gerrit/GitHub credentials. The
;; user's own config is the exception, and an explicit one: a name written under
;; `environment:` or `extensions.env-passthrough` was declared by the operator
;; for exactly this purpose, so it is offered to extensions as well.
;; =============================================================================

(defn- config-env-passthrough
  "Extra env var names the USER allowed in `vis.yml`:

     extensions:
       env-passthrough: [RBI_GENAI_API_KEY]"
  []
  (try (let [cfg (config/current-config)]
         (into #{}
               (comp (map str) (remove str/blank?))
               (or (get-in cfg [:extensions :env-passthrough])
                   (get-in cfg [:extensions "env-passthrough"])
                   [])))
       (catch Throwable _ #{})))

(def ^:private env-name-re #"^[A-Za-z_][A-Za-z0-9_]*$")

(defn- normalize-env-names
  "Distinct, validated env-var names from any source: `str`, regex-filtered,
   deduped. Malformed entries are dropped. Used by BOTH the resolution path
   (runtime values via `resolve-declared-env`) and the spec path (`:ext/env`
   declarations in `registration->spec`), so the two never diverge on what
   counts as a valid name."
  [names]
  (into [] (comp (map str) (filter #(re-matches env-name-re %)) (distinct)) (or names [])))

(defn ^:no-doc resolve-declared-env
  "Values for the env var names an extension may see, resolved through the ONE
   funnel every surface uses (`config/extension-env-status`): this config's
   `environment:` declaration when the name has one — the process environment
   under another name, `.env`/`.env.local`, a keychain item or a helper command —
   and otherwise the process environment.

   Three sources of NAMES, unioned: what the extension DECLARED, what the user
   allowed with `extensions.env-passthrough`, and what the user declared under
   `environment:`. The last two are the user's own config, so they are an opt-in
   by construction; a name nobody declared stays unreachable no matter what the
   extension asks for.

   Names that resolve to nothing are simply absent from the result (never an
   empty string, so an extension's `os.environ.get(...) or default` still works)."
  [declared]
  (let
    [names (normalize-env-names (concat (or declared [])
                                        (config-env-passthrough)
                                        (config/declared-environment-names)))]
    (into {}
          (keep (fn [n]
                  (when-let [v (config/extension-env-value n)]
                    [n v])))
          names)))

(defn ^:no-doc build-context
  "Build one trusted extension context on the shared Engine. Extensions have
   real filesystem, network, environment, thread, and subprocess access; only
   arbitrary host interop remains unavailable. Use `vis.jailed_shell` when a
   command should instead run under the current session's jail policy."
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

(defn ^:no-doc bind-host!
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
                    nil)))
    (.putMember g
                "__vis_host_shell__"
                ;; `vis.shell` follows the extension's trusted process boundary,
                ;; even when its caller has an enabled session jail. It keeps the
                ;; native shell tool's one-options-map result grammar.
                (->executable
                  (fn [opts]
                    (host-tool-result
                      ((requiring-resolve
                         'com.blockether.vis.internal.foundation.shell/trusted-extension-shell)
                        extension/*current-environment*
                        opts)))))
    (.putMember g
                "__vis_host_jailed_shell__"
                ;; Latest-config jail: the shell implementation reloads, validates,
                ;; and freezes the merged disk policy at each process spawn. It is
                ;; deliberately independent of the invoking session snapshot.
                (->executable (fn [opts]
                                (host-tool-result
                                  ((requiring-resolve
                                     'com.blockether.vis.internal.foundation.shell/jailed-shell)
                                    extension/*current-environment*
                                    opts)))))
    (.putMember g
                "__vis_host_jailed_shell_session__"
                ;; Session-snapshot jail: stable for the session and unavailable
                ;; when a process-level callback has no invoking session.
                (->executable
                  (fn [opts]
                    (host-tool-result
                      ((requiring-resolve
                         'com.blockether.vis.internal.foundation.shell/session-jailed-shell)
                        extension/*current-environment*
                        opts)))))
    (.putMember
      g
      "__vis_host_request_input__"
      ;; Typed human-input pause: one JSON request object in, one JSON
      ;; answer object out. BLOCKS this extension call until the human
      ;; answers, cancels, or the request times out.
      ;;
      ;; A validator is a FUNCTION, so it cannot be part of that JSON:
      ;; `ask()` also hands over `{field name -> how many validators}`
      ;; and one Python callable, kept here as the raw polyglot `Value`
      ;; (never marshalled to Clojure data) and re-entered on the
      ;; SUBMITTING thread when the human confirms. GraalPy releases the
      ;; GIL while a host call blocks, which is what makes that legal.
      ;; Only a field name, an index and the value being judged cross,
      ;; as JSON, so the verdict path has no marshalling surprises.
      (reify
        ProxyExecutable
          (execute [_ args]
            (let
              [request-json (env/->clj (aget args 0))
               validators-json (when (> (alength args) 1) (env/->clj (aget args 1)))
               ^Value runner (when (> (alength args) 2) (aget args 2))
               run (when (some-> runner
                                 .canExecute)
                     (fn [field-name index value values]
                       (let
                         [verdict (env/->clj (.execute runner
                                                       (object-array [(str field-name) (long index)
                                                                      (json/write-json-str value)
                                                                      (json/write-json-str
                                                                        values)])))]
                         (when (some? verdict) (json/read-json (str verdict) :key-fn identity)))))]

              (env/->py ((requiring-resolve 'com.blockether.vis.internal.human-input/request-json!)
                          request-json
                          validators-json
                          run))))))
    (.putMember g
                "__vis_host_check_input__"
                ;; The same seam as `request_input`, minus the human: one JSON
                ;; request object in, one JSON verdict out. Nothing is drawn,
                ;; published or parked and no validator runs, so `vis.check(...)`
                ;; can prove a form an extension just built -- and
                ;; `vis-agent extension check` can prove one it never ran.
                (->executable (fn [request-json]
                                ((requiring-resolve
                                   'com.blockether.vis.internal.human-input/check-json)
                                  request-json))))
    (.putMember g
                "__vis_host_reveal_secret__"
                (->executable (fn [handle]
                                ((requiring-resolve
                                   'com.blockether.vis.internal.human-input/reveal-secret)
                                  (str handle)))))
    (.putMember g
                "__vis_host_forget_secret__"
                (->executable (fn [handle]
                                (boolean ((requiring-resolve
                                            'com.blockether.vis.internal.human-input/forget-secret!)
                                           (str handle))))))
    ;; DECLARED ENV: one JSON list of names in, one JSON object of the values
    ;; the host could resolve out. Backed by `resolve-declared-env`, so an
    ;; undeclared name is unreachable no matter what the extension asks for.
    (.putMember g
                "__vis_host_declare_env__"
                (->executable
                  (fn [names-json]
                    (let
                      [names (try (json/read-json (str names-json) :key-fn identity)
                                  (catch Throwable _ nil))
                       resolved (resolve-declared-env names)]

                      ;; Log NAMES only -- env values are secrets and never appear in logs.
                      (tel/log! {:level :debug
                                 :id ::declared-env
                                 :data {:ext label
                                        :declared (vec (map str (or names [])))
                                        :resolved (vec (sort (keys resolved)))}
                                 :msg (str "extension '"
                                           label
                                           "' declared env: "
                                           (str/join ", " (sort (keys resolved)))
                                           " resolved")})
                      (json/write-json-str resolved)))))))

(def ^:no-doc host-member-names
  "Every `__vis_host_*` global the bootstrap reads out of a context's bindings.

   The bootstrap builds its `_host` dict at MODULE level, so a member nobody
   bound is a `NameError` before the extension's first line runs. Both binders
   are checked against this list by a test, which is why adding a host call means
   adding it here."
  ["__vis_host_state_get__" "__vis_host_state_put__" "__vis_host_state_del__" "__vis_host_log__"
   "__vis_host_notify__" "__vis_host_shell__" "__vis_host_jailed_shell__"
   "__vis_host_jailed_shell_session__" "__vis_host_request_input__" "__vis_host_check_input__"
   "__vis_host_reveal_secret__" "__vis_host_forget_secret__" "__vis_host_declare_env__"])

(defn ^:no-doc bind-inert-host!
  "Bind every host member as a REFUSAL, so the `vis` module can be BUILT without
   any of it being usable.

   `vis-agent extension check` needs the real module -- it reads the builders and
   the exported names straight out of it -- but a static check runs nobody's side
   effects. A stub that throws is how the checker stays honest about what it did
   not run: a form is judged by the engine on the Clojure side, never by
   executing the extension.

   `overrides` may hand back ONE member by name: the checker binds the real
   `__vis_host_check_input__`, because validating a request reads nothing, writes
   nothing and asks nobody."
  ([^Context ctx] (bind-inert-host! ctx nil))
  ([^Context ctx overrides]
   (let [g (.getBindings ctx "python")]
     (doseq [member host-member-names]
       (.putMember g
                   ^String member
                   (->executable
                     (or (get overrides member)
                         (fn [& _]
                           (throw (ex-info
                                    (str "host call " member " is not available while checking")
                                    {:type :vis/extension-check-inert :member member}))))))))))

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
  "Invoke a Python callable inside THE session context (`extension/with-context`):
   the extension identity (so `vis.state` host callbacks own their aggregate
   rows) plus the live session env, which `vis.jailed_shell_session`, `vis.ask`
   and `vis.state` read.

   Adapters handed a real env (activation, prompt, ctx, slash, op hooks) pass it;
   adapters with none (symbol calls, render, provider callbacks) pass nil and
   inherit the caller's session instead of running session-less. That keeps
   `vis.jailed_shell_session` and `vis.ask` available when a caller has a live
   session. `vis.jailed_shell` does not depend on this context: it reads disk at
   each spawn.
   Returns the `->clj` view of the result."
  [ext-name env ^Context ctx ^Value f args]
  (extension/with-context {:ext (or extension/*current-extension* {:ext/name ext-name}) :env env}
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


(defn- op-hook-payload
  "STRINGS-ONLY `{'op' 'args' ['result']}` payload for a Python op hook.

   Host tool args and results are ORDINARY Clojure data — keyword keys, keyword
   enum values, symbols — and `->py` rejects every one of those outright. An
   unstringified payload therefore threw a boundary violation INSIDE the hook,
   killing the very call the hook was only observing (a `:before` guard fails
   open, but an `:after` hook's throw surfaces on the tool). Every other Python
   callback adapter (render, enrich-models, on-selected) already crosses through
   `stringify-deep`; op hooks now do the same."
  ([op-kw args] (stringify-deep {"op" (name op-kw) "args" (vec args)}))
  ([op-kw args result] (stringify-deep {"op" (name op-kw) "args" (vec args) "result" result})))

(def ^:private ^:dynamic *healing-symbol*
  "True while a healed retry is in flight, so one torn-down context triggers at
   most ONE rebuild + retry instead of recursing through fresh adapters."
  false)

(defn- context-dead?
  "True when `ctx` can no longer run guest code - it was cancelled or closed
   underneath callables that were captured over it.

   ASKED, never parsed. A torn-down context refuses at EVERY entry point, so the
   cheapest possible handshake answers the question: `Context.asValue` of a host
   long enters the context and executes nothing. It returns on a live context,
   throws `PolyglotException` (`.isCancelled`) on a cancelled one and
   `IllegalStateException` (\"The Context is already closed.\") on a closed one -
   no error-message matching anywhere. Two consequences worth keeping: an
   ordinary Python error leaves the context ALIVE, so it never triggers healing,
   and an `interrupt` (issue #102) leaves it alive too, so an interrupted call
   still surfaces as an interrupt. The probe runs under the same `locking ctx`
   every call uses (reentrant), so a live context that is merely busy on another
   thread is never mistaken for a dead one."
  [^Context ctx]
  (or (nil? ctx) (locking ctx (try (.asValue ctx (long 1)) false (catch Throwable _ true)))))

(declare live-symbol-fn)

(defn- tool-adapter
  "Observed-tool fn for one Python-backed symbol. Return value = success
   payload; a raised Python exception = failure envelope (message + trace
   via `normalize-error`) - Python authors never construct envelopes.

   SELF-HEALING (issue #103): a sandbox binding - and every cached session env
   row - captures THIS closure over the context that was alive at load time. A
   `/reload` (or any other rebuild) closes that context, and nothing re-binds
   the already-captured fns, so every later call died until the whole session
   was restarted. On a failure we ASK the context whether it is still alive
   (`context-dead?`); only when it is gone do we re-resolve the freshest fn for
   `[ext-name sym]` - rebuilding the file's context when the registry itself is
   stale - and retry the call ONCE. A live context means a genuine Python error,
   which stays a plain failure envelope."
  [ext-name sym ^Context ctx ^Value pyfn]
  (fn [& args]
    (let [argv (vec args)]
      (try (extension/success {:result (call-py-ext ext-name nil ctx pyfn argv)})
           (catch Throwable t
             (if-let
               [fresh
                (and (not *healing-symbol*) (context-dead? ctx) (live-symbol-fn ext-name sym ctx))]
               (binding [*healing-symbol* true]
                 (apply fresh argv))
               (extension/failure
                 {:result nil :throwable t :metadata {:extension ext-name :tool (str sym)}})))))))

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
    (let
      [payload
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
    (let
      [res (try (call-py-ext ext-name env ctx pyfn [(op-hook-payload op-kw args)])
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

(defn- gate-adapter
  "Python hook on a GATE op -> a host `:gate` hook. The callable receives that
   gate's own ctx, string-keyed (for `fs_access`: `{'operation', 'path'}`);
   returning `vis.block(reason)` REFUSES with that sentence and returning None
   allows.

   A hook error fails CLOSED — the opposite of `guard-adapter` — because a
   boundary that opens when its guard breaks is not a boundary. That asymmetry is
   the whole reason a gate is its own shape."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [env op-kw gate-ctx]
    (try (let
           [payload
            (reduce-kv (fn [m k v]
                         (assoc m (name k) (str v)))
                       {}
                       gate-ctx)

            res
            (call-py-ext ext-name env ctx pyfn [payload])]

           (when (and (map? res) (= "block" (get res "marker")))
             {:reason (str (or (get res "reason")
                               (str "Refused by the '" ext-name "' extension.")))}))
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::gate-hook-failed
                      :data {:extension ext-name :op op-kw :error (ex-message t)}})
           {:reason (str "the '" ext-name
                         "' extension's " (name op-kw)
                         " guard failed, and a boundary fails closed: " (ex-message t))}))))

(defn- after-adapter
  "Python `phase='after'` hook -> a host :after op hook. Observe-only:
   the callable receives `{'op', 'args', 'result'}`; its return value is
   ignored and the original result always flows on."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [env op-kw args result]
    (try (call-py-ext ext-name env ctx pyfn [(op-hook-payload op-kw args (:result result))])
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::op-hook-failed
                      :data {:extension ext-name :op op-kw :error (ex-message t)}})))
    result))

(defn- network-filter-adapter
  "Python `vis.network_filter(fn)` -> a host egress network filter `(fn [ctx])`.
   Fires at BOTH phases: the callable receives the decrypted request
   `{'phase','method','host','path','headers'}` on the way out and the upstream
   response `{'phase','method','host','path','status','headers'}` on the way back
   (`'phase'` distinguishes them). Returning `vis.block(reason)` DENIES (a denied
   response yields a 403 instead of the body), returning None allows it.
   FAIL-CLOSED: a hook error DENIES (a security filter must never fail open)."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [c]
    (let
      [pyctx
       (cond->
         {"phase" (some-> (:phase c)
                          name)
          "method" (some-> (:method c)
                           str)
          "host" (:host c)
          "path" (:path c)
          "headers" (or (:headers c) {})}
         (contains? c :status)
         (assoc "status" (:status c)))

       err
       (atom nil)

       res
       (try (call-py-ext ext-name nil ctx pyfn [pyctx])
            (catch Throwable t
              (let [e (extension/normalize-error t)]
                (reset! err e)
                (tel/log! {:level :warn
                           :id ::network-filter-failed
                           :data {:extension ext-name :error (:message e)}})
                {"marker" "block"
                 "reason" (str "network filter error in '" ext-name "': " (:message e))})))]

      (if (and (map? res) (= "block" (get res "marker")))
        (cond-> {:allow? false :reason (str (or (get res "reason") "blocked by network filter"))}
          @err
          (assoc :error @err))
        {:allow? true}))))

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

;; JSON Schema vocabulary the host reads with KEYWORD keys (see
;; `extension/schema->param-doc`). A Python author writes a plain dict, so the
;; keys arrive as strings: only these vocabulary words become keywords, while
;; author-chosen PROPERTY NAMES stay strings — exactly the shape a
;; Clojure-authored native tool schema has
;; (`{:type "object" :properties {"path" {:type "string"}}}`).
(def ^:private json-schema-words
  #{"type" "properties" "required" "description" "items" "prefixItems" "enum" "const" "default"
    "examples" "format" "pattern" "title" "not" "oneOf" "anyOf" "allOf" "additionalProperties"
    "patternProperties" "minimum" "maximum" "exclusiveMinimum" "exclusiveMaximum" "multipleOf"
    "minLength" "maxLength" "minItems" "maxItems" "uniqueItems" "minProperties" "maxProperties"
    "nullable" "$ref" "$defs" "definitions"})

;; Vocabulary words whose VALUE is a map keyed by author-chosen names rather
;; than by schema words — those keys must survive as strings.
(def ^:private json-schema-name-maps #{"properties" "patternProperties" "$defs" "definitions"})

(defn- py-schema
  "Python JSON Schema dict -> the host schema shape (keyword vocabulary keys,
   string property names). Without it a Python-declared schema would still be
   sent to the provider but read as EMPTY by every host projection
   (`doc(name)` params, wire docs), which reads `:properties` / `:required`."
  [x]
  (cond (map? x) (into {}
                       (map (fn [[k v]]
                              (let [ks (str k)]
                                (if (contains? json-schema-words ks)
                                  [(keyword ks)
                                   (if (and (map? v) (contains? json-schema-name-maps ks))
                                     (into {}
                                           (map (fn [[pk pv]]
                                                  [(str pk) (py-schema pv)]))
                                           v)
                                     (py-schema v))]
                                  [ks (py-schema v)]))))
                       x)
        (sequential? x) (mapv py-schema x)
        :else x))


(defn- render-start-call-adapter
  "`:ext.symbol/render-start-call-fn` for one Python-backed symbol."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [input]
    (try (let [r (call-py-ext ext-name nil ctx pyfn [(stringify-deep input)])]
           (when (map? r)
             (reduce
               (fn [card [wire-key host-key]]
                 (if (some? (get r wire-key)) (assoc card host-key (str (get r wire-key))) card))
               {}
               [["summary" :summary] ["render" :render] ["code" :code] ["language" :language]])))
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::render-start-call-failed
                      :data {:extension ext-name :error (ex-message t)}})
           nil))))

(defn- render-finish-call-adapter
  "`:ext.symbol/render-finish-call-fn` for one Python-backed symbol. The Python
   callable gets the tool result as plain string-keyed data and returns
   `{'summary': str, 'body': str}` (a bare string counts as the summary). A failing
   or unusable renderer yields nil, so a bad op card never breaks the tool itself."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [result]
    (try (let [r (call-py-ext ext-name nil ctx pyfn [(stringify-deep result)])]
           (cond (string? r) {:summary r}
                 (map? r) (cond-> {}
                            (some? (get r "summary"))
                            (assoc :summary (str (get r "summary")))

                            (string? (get r "body"))
                            (assoc :body (get r "body")))
                 :else nil))
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::render-finish-call-failed
                      :data {:extension ext-name :error (ex-message t)}})
           nil))))

(defn- ->symbol-entry
  "`spec` is a Python registration dict — STRING keys (strings-only boundary).
   Native-tool metadata rides through `extension/symbol-entry`, so a Python-declared
   tool is validated, advertised, documented and rendered exactly like a Clojure one."
  [ext-name alias-sym ^Context ctx spec]
  (let
    [sym
     (clojure.core/symbol (symbol-base-name alias-sym (str (get spec "name"))))

     pyfn
     (get spec "fn")

     argv
     (cond-> (mapv clojure.core/symbol (get spec "params"))
       (get spec "varargs")
       (-> (conj '&)
           (conj 'args)))

     render-start-call-fn
     (get spec "render_start_call_fn")

     render-finish-call-fn
     (get spec "render_finish_call_fn")

     opts
     (cond-> {:tag (get symbol-tags (str (get spec "tag")) :observation)}
       (get spec "hidden")
       (assoc :hidden? true)

       (get spec "is_native_tool")
       (assoc :native-tool?
         true :schema
         (py-schema (get spec "schema")) :description
         (str (get spec "description")) :result
         (str (get spec "result")))

       render-start-call-fn
       (assoc :render-start-call-fn (render-start-call-adapter ext-name ctx render-start-call-fn))

       render-finish-call-fn
       (assoc :render-finish-call-fn
         (render-finish-call-adapter ext-name ctx render-finish-call-fn)))]

    (extension/symbol-entry {:symbol sym
                             :fn (tool-adapter ext-name sym ctx pyfn)
                             :doc (str (get spec "doc"))
                             :arglists [argv]}
                            opts)))

(defn- ->slash-spec
  [ext-name ^Context ctx spec]
  (let
    [doc
     (get spec "doc")

     usage
     (get spec "usage")]

    (cond->
      {:slash/name (str (get spec "name"))
       :slash/run-fn (slash-adapter ext-name ctx (get spec "run"))}
      (string? doc)
      (assoc :slash/doc doc)

      (string? usage)
      (assoc :slash/usage usage))))

(defn- ->op-hook-entries
  [ext-name ^Context ctx spec]
  (let
    [before?
     (= "before" (str (get spec "phase")))

     pyfn
     (get spec "fn")]

    ;; `:op` keys the INTERNAL op-hook registry (keyword-keyed, matched against
    ;; canonical tool op keywords). The vocabulary is author-declared config,
    ;; not model data — the one sanctioned mint in this file.
    (mapv (fn [op]
            ;; A GATE op (`"fs_access"`) is a different shape, and the OP decides
            ;; it: asked rather than wrapped, refusing rather than rewriting, and
            ;; failing CLOSED. `phase` is not consulted — there is nothing to be
            ;; before or after when the operation has not been allowed yet.
            (if-let [gate-kw (extension/gate-op op)]
              {:op gate-kw :phase :gate :fn (gate-adapter ext-name ctx pyfn)}
              {:op (keyword (str op))
               :phase (if before? :around :after)
               :fn
               (if before? (guard-adapter ext-name ctx pyfn) (after-adapter ext-name ctx pyfn))}))
          (get spec "ops"))))

;; ── Providers: DECODED against a declared shape, never walked ────────────────
;; A `vis.provider(...)` dict -> a canonical provider descriptor entry, and a
;; provider CALLABLE's return -> the map the host schema behind it already
;; declares. There is ONE mechanism: `decode`, driven by a per-contract table of
;; engine key -> the coercion that key's spec demands (`token-fields`,
;; `status-fields`, `limits-fields`, `preset-fields`). A key the table does not
;; name keeps its Python spelling through `wire/engine-key` — the gateway's own
;; `wire-key` inverse, so this namespace owns no second spelling rule — and its
;; VALUE UNTOUCHED.
;;
;; Nothing recurses into data we did not declare. That is the whole point: an
;; author's `llm_headers` / `extra_body` reach svar exactly as written, in the
;; same string-keyed shape a Clojure provider returns
;; (`runtime-settings/AGENT_INITIATOR_HEADERS` is `{"X-Initiator" "agent"}`), and
;; a key nobody declared can never be re-typed behind the author's back — which
;; is how `is_unlimited` once became `:unlimited?` and made every Python limits
;; report fail `provider-limits/::limit-row`.
;;
;; base-url/api-style/default-models in `:preset` flow through
;; `config/known-provider-base-url` into svar's router, so a pure-Python provider
;; actually serves model calls once the user configures it.

(defn- as-str [v] (when (some? v) (str v)))

(defn- as-kw [v] (when (some? v) (clojure.core/keyword (str v))))

(defn- as-bool [v] (when (some? v) (boolean v)))

(defn- as-long [v] (when (number? v) (long v)))

(defn- as-num [v] (when (number? v) v))

(defn- as-strs [v] (when (sequential? v) (mapv str v)))

(defn- decode
  "Decode ONE map against `fields` (engine key -> 1-arg coercion). Every key is
   named through `wire/engine-key`; a key `fields` declares is coerced, and every
   other entry's value passes through as-is. One level, by declaration — this is
   deliberately not a walker."
  [fields m]
  (when (map? m)
    (reduce-kv (fn [acc k v]
                 (let
                   [kk
                    (wire/engine-key k)

                    f
                    (get fields kk)]

                   (assoc acc kk (if f (f v) v))))
               {}
               (into {} m))))

(defn- decoder
  "`fields` as a coercion, for a declared SUBMAP."
  [fields]
  (fn [v]
    (decode fields v)))

(defn- decoder-rows
  "`fields` as a coercion, for a declared vector of submaps."
  [fields]
  (fn [v]
    (when (sequential? v) (mapv (decoder fields) v))))

(defn- decoded-result
  "A provider callable's return: decoded when it is a map, untouched otherwise
   (`get_token_fn` may answer a bare token string, `detect_fn` a credential)."
  [fields v]
  (if (map? v) (decode fields v) v))

(def ^:private token-fields
  "`get_token_fn` / `refresh_token_fn` / `detect_fn` -> the credential map
   `config/->svar-provider` and `loop/hydrate-router-credentials` destructure.
   `llm-headers` is deliberately NOT declared: a header map is string-keyed on
   the wire and stays exactly as the author wrote it."
  {:token as-str :api-url as-str :responses-path as-str :source as-kw})

(def ^:private status-fields
  "`status_fn` -> the shared connection verdict (`providers/safe-provider-status`,
   the status dot, the status dialog, routing)."
  {:is-authenticated as-bool
   :error as-str
   :source as-kw
   :provider-id as-kw
   :status as-kw
   :base-url as-str
   :label as-str
   :config-path as-str})

(def ^:private window-fields
  "`provider-limits/::window`."
  {:kind as-kw :unit as-kw :size as-long :resets-at-ms as-long})

(def ^:private limit-row-fields
  "`provider-limits/::limit-row`, key for key."
  {:id as-kw
   :label as-str
   :scope as-kw
   :kind as-kw
   :precision as-kw
   :source as-kw
   :is-unlimited as-bool
   :subject (decoder {})
   :window (decoder window-fields)
   :used as-num
   :limit as-num
   :remaining as-num
   :note as-str})

(def ^:private limits-fields
  "`limits_fn` -> `provider-limits/::report`. The host backfills the rest."
  {:provider-id as-kw
   :status as-kw
   :fetched-at-ms as-long
   :static (decoder {:rpm as-long :tpm as-long})
   :dynamic (decoder {:limits (decoder-rows limit-row-fields) :note as-str})
   :error (decoder {:type as-kw :message as-str :data (decoder {})})})

(def ^:private preset-fields
  "The preset keys the host owns. ONE spelling per key: a Python author writes
   snake_case, the same as every other wire surface. Every OTHER preset key —
   `extra_body`, `responses_path`, `context`, `llm_headers` — is named and passed
   through verbatim, exactly the extra preset keys
   `config/registered-provider-metadata` merges into svar for a first-party
   provider."
  {:base-url as-str :api-style as-kw :default-models as-strs :is-hidden as-bool})

(defn- call-provider-fn
  "Invoke a Python provider callable with `args`, tolerating an arg-count
   mismatch the same way the loop's refresh path tolerates it for Clojure
   hooks (`(f rejected)` falling back to `(f)`, loop.clj): a Python callable
   that rejects the supplied args is retried with one fewer TRAILING arg, down
   to zero. In practice only `refresh_token` is ever handed an arg (the
   rejected token), so both a 0-param `def refresh_token():` and a 1-param
   `def refresh_token(rejected):` work. A genuine 0-arg failure re-throws (the
   caller logs it and yields nil)."
  [ext-name ^Context ctx ^Value pyfn args]
  (loop [args (vec args)]
    (let
      [r (try {:ok (call-py-ext ext-name nil ctx pyfn args)}
              (catch Throwable t (if (seq args) {:retry (vec (butlast args))} (throw t))))]
      (if (contains? r :ok) (:ok r) (recur (:retry r))))))

(defn- provider-fn-adapter
  "Wrap a Python provider callable as a Clojure provider fn. Args marshal in; the
   result is plainified and DECODED against `fields`, the shape that slot's host
   schema declares. A raised Python error is logged and surfaces as nil so a
   broken provider fn never bricks router build / auth."
  [ext-name ^Context ctx ^Value pyfn fields]
  (fn [& args]
    (try (decoded-result fields (plainify (call-provider-fn ext-name ctx pyfn args)))
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::provider-fn-failed
                      :data {:extension ext-name :error (ex-message t)}})
           nil))))

(def ^:private auth-success-results
  ;; The silent-success signals the TUI/CLI recognize (channel_tui provider.clj,
  ;; `auth-fn-success-results`): coerce a Python string return to its keyword so
  ;; the "success is silent" rule fires. Bounded map — no keyword minting from
  ;; arbitrary data.
  {"ok" :ok "already-authenticated" :already-authenticated "authenticated" :authenticated})

(defn- auth-fn-adapter
  "Wrap a Python `auth(printer)` callable as `:provider/auth-fn`. The host hands
   in a Clojure `print!` fn; it is marshalled INTO Python as a callable the
   extension invokes to emit one instruction line. The return coerces a
   success-signal string to its keyword (so the silent-success path matches),
   passes `True`/`None` through, and leaves anything else as-is. A raised Python
   error propagates — the caller (TUI/CLI) frames it as an auth failure."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [print!]
    (let
      [printer
       (->executable (fn [line]
                       (print! (str line))
                       nil))

       r
       (call-py-ext ext-name nil ctx pyfn [printer])]

      (if (string? r) (get auth-success-results r r) r))))

(defn- auth-prompt-fn-adapter
  "Wrap a Python `auth_prompt()` callable as `:provider/auth-prompt-fn` —
   `() -> guidance lines` shown in the API-key dialog body. Result coerces to a
   vector of strings (a bare string becomes a one-line vector); anything else,
   or an error, yields nil so a broken prompt never blocks the dialog."
  [ext-name ^Context ctx ^Value pyfn]
  (fn []
    (try (let [r (call-py-ext ext-name nil ctx pyfn [])]
           (cond (sequential? r) (mapv str r)
                 (string? r) [r]
                 :else nil))
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::provider-fn-failed
                      :data {:extension ext-name :error (ex-message t)}})
           nil))))

(defn- ->svar-model
  "One enriched model row from Python -> the shape svar's router reads. Keys take
   the mechanical `wire/engine-key` inverse; the handful svar itself spells with a
   trailing `?` then move through `config/svar-wire->runtime`, svar's OWN named
   table (`is_tool_call` -> `:tool-call?`). A foreign contract gets an explicit
   table at one seam — it never gets a convention applied to every other key."
  [row]
  (reduce-kv
    (fn [m wire-k runtime-k]
      (let [mechanical (wire/engine-key wire-k)]
        (if (contains? m mechanical) (assoc (dissoc m mechanical) runtime-k (get m mechanical)) m)))
    (decode {} row)
    config/svar-wire->runtime))

(defn- enrich-models-fn-adapter
  "Wrap a Python `enrich_models(provider, router_opts)` callable as
   `:provider/enrich-models-fn`. The host hands the svar-shaped provider and
   the router opts INTO Python as plain string-keyed dicts (`stringify-deep`);
   each returned model goes through `->svar-model`. A non-sequential return or
   any error yields nil, which the loop's `enrich-provider-models` treats as 'no
   enrichment' — the router still builds on svar's conservative defaults."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [svar-provider router-opts]
    (try (let
           [r (call-py-ext ext-name
                           nil
                           ctx
                           pyfn
                           [(stringify-deep svar-provider) (stringify-deep router-opts)])]
           (when (sequential? r) (mapv ->svar-model r)))
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::provider-fn-failed
                      :data {:extension ext-name :error (ex-message t)}})
           nil))))

(defn- on-selected-fn-adapter
  "Wrap a Python `on_selected(event)` callable as `:provider/on-selected-fn` —
   a side-effect hook run after the active provider changes and config is
   persisted. The event `{:previous-provider :provider :config :source}` crosses
   INTO Python as a plain string-keyed dict (`stringify-deep`); the return is
   ignored (the contract is nil). Errors are logged and swallowed so a broken
   hook never blocks provider selection."
  [ext-name ^Context ctx ^Value pyfn]
  (fn [event]
    (try (call-py-ext ext-name nil ctx pyfn [(stringify-deep event)])
         (catch Throwable t
           (tel/log! {:level :warn
                      :id ::provider-fn-failed
                      :data {:extension ext-name :error (ex-message t)}})))
    nil))

(defn- ->provider-entry
  "`spec` is a Python `vis.provider(...)` dict — STRING keys. Each callable slot
   is adapted with the field table its own host schema declares."
  [ext-name ^Context ctx spec]
  (let
    [adapt
     (fn [pk fields]
       (let [v (get spec pk)]
         (when (instance? Value v) (provider-fn-adapter ext-name ctx v fields))))

     preset
     (decode preset-fields (get spec "preset"))]

    (cond->
      {:provider/id (clojure.core/keyword (str (get spec "id")))
       :provider/label (str (get spec "label"))}
      (seq preset)
      (assoc :provider/preset preset)

      (adapt "get_token_fn" token-fields)
      (assoc :provider/get-token-fn (adapt "get_token_fn" token-fields))

      (adapt "detect_fn" token-fields)
      (assoc :provider/detect-fn (adapt "detect_fn" token-fields))

      (adapt "status_fn" status-fields)
      (assoc :provider/status-fn (adapt "status_fn" status-fields))

      (adapt "logout_fn" nil)
      (assoc :provider/logout-fn (adapt "logout_fn" nil))

      (adapt "limits_fn" limits-fields)
      (assoc :provider/limits-fn (adapt "limits_fn" limits-fields))

      (adapt "refresh_token_fn" token-fields)
      (assoc :provider/refresh-token-fn (adapt "refresh_token_fn" token-fields))

      (instance? Value (get spec "auth_fn"))
      (assoc :provider/auth-fn (auth-fn-adapter ext-name ctx (get spec "auth_fn")))

      (instance? Value (get spec "auth_prompt_fn"))
      (assoc :provider/auth-prompt-fn
        (auth-prompt-fn-adapter ext-name ctx (get spec "auth_prompt_fn")))

      (instance? Value (get spec "enrich_models_fn"))
      (assoc :provider/enrich-models-fn
        (enrich-models-fn-adapter ext-name ctx (get spec "enrich_models_fn")))

      (instance? Value (get spec "on_selected_fn"))
      (assoc :provider/on-selected-fn
        (on-selected-fn-adapter ext-name ctx (get spec "on_selected_fn"))))))

(defn- registration->spec
  "`reg` is the dict handed to Python `vis.register(...)` — STRING keys."
  [^Context ctx reg]
  (let
    [ext-name
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
     (get reg "activation")

     providers
     (mapv #(->provider-entry ext-name ctx %) (get reg "providers"))

     network-filters
     (mapv (fn [rf]
             (network-filter-adapter ext-name ctx (get rf "fn")))
           (get reg "network_filters"))

     declared-env
     (mapv (fn [n]
             {:name n :required? true})
           (normalize-env-names (get reg "env")))]

    (cond->
      {:ext/name ext-name
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
      (assoc :ext/ctx-fn (ctx-adapter ext-name ctx ctx-fn))

      (seq providers)
      (assoc :ext/providers providers)

      (seq network-filters)
      (assoc :ext/network-filters network-filters)

      (seq declared-env)
      (assoc :ext/env declared-env))))

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

(defn ^:no-doc default-extension-dirs
  []
  [(io/file (System/getProperty "user.home") ".vis" "extensions")
   (io/file (System/getProperty "user.dir") ".vis" "extensions")])

(defn ^:no-doc test-file?
  "A `test_*.py` / `*_test.py` module — a Python test, never an extension entry."
  [^File f]
  (let [n (.getName f)]
    (and (str/ends-with? n ".py") (or (str/starts-with? n "test_") (str/ends-with? n "_test.py")))))

(defn- scan
  "Entry `.py` files across the extension dirs, global dir first then project
   dir (name order within a dir) so a project file registering the same
   extension name wins. Two authoring conventions per dir:
     - a top-level `*.py` file is a single-file extension;
     - an immediate subdirectory holding `extension.py` is a PACKAGE extension —
       that `extension.py` is the entry and the rest of the package imports via
       the `sys.path` sugar in `load-file!`; the package's own modules are NEVER
       scanned as separate extensions.
   Test modules (`test_*.py` / `*_test.py`) are skipped — they are run by
   `test-python-extensions!`, not loaded. Deduped on canonical path."
  [dirs]
  (let
    [files (for
             [^File d (map io/file dirs)
              :when (.isDirectory d)
              ^File child (sort-by #(.getName ^File %) (.listFiles d))
              ^File f (cond (and (.isFile child)
                                 (str/ends-with? (.getName child) ".py")
                                 (not (test-file? child)))
                            [child]
                            (.isDirectory child) (let [ep (io/file child "extension.py")]
                                                   (when (.isFile ep) [ep]))
                            :else nil)]

             f)]
    (->> files
         (reduce (fn [[seen acc] ^File f]
                   (let [p (.getCanonicalPath f)]
                     (if (seen p) [seen acc] [(conj seen p) (conj acc f)])))
                 [#{} []])
         second)))

(defn- import-root
  "The directory an extension file is served from - what `load-file!` freezes and
   puts on `sys.path`. The loader's fingerprint, the freeze and the heal guard
   all resolve it HERE, so one file can never be keyed under two roots."
  ^File [^File f]
  (.getParentFile (.getCanonicalFile f)))

(defn- import-root-files
  "Every regular file under an extension's import root as `[rel-path file]`,
   sorted, `__pycache__` aside. The root is the directory `load-file!` puts on
   `sys.path`, so this is the whole body of code a lazy `import` inside a symbol
   can still reach at CALL time - long after the load.

   A symlinked subdirectory or module is FOLLOWED, because linking a package one
   is working on into `~/.vis/extensions` is how extensions get developed - but
   its `rel-path` stays lexically under the root. Resolving the file instead
   produced `../…` entries, which put the frozen copy OUTSIDE the snapshot (and
   `io/copy` truncates whatever it lands on) while leaving the module missing
   from the tree the extension actually imports. A directory already walked ends
   that branch, so a symlink cycle terminates."
  [^File root]
  (let [root-path (.toPath (.getAbsoluteFile root))]
    (loop
      [[^File dir & more] [root]
       seen #{}
       out []]

      (if (nil? dir)
        (vec (sort-by first out))
        (let [k (.getCanonicalPath dir)]
          (if (or (seen k) (= "__pycache__" (.getName dir)))
            (recur more seen out)
            (let [children (vec (.listFiles dir))]
              (recur (into (vec more)
                           (filter (fn [^File f]
                                     (.isDirectory f)))
                           children)
                     (conj seen k)
                     (into out
                           (comp (filter (fn [^File f]
                                           (.isFile f)))
                                 (map (fn [^File f]
                                        [(str (.relativize root-path
                                                           (.toPath (.getAbsoluteFile f)))) f])))
                           children)))))))))

(defn- code-sha
  "One digest over every `.py` under the import root. An extension's identity is
   its WHOLE tree, never its entry file alone: `def run(): import helper` reads
   `helper.py` at CALL time, so entry-only identity let a module edited after the
   load execute in the trusted context with no `/reload` behind it."
  [^File root]
  (extension/sha256-hex (str/join "\n"
                                  (keep (fn [[^String rel ^File f]]
                                          (when (str/ends-with? rel ".py")
                                            (str rel " " (extension/sha256-hex (slurp f)))))
                                        (import-root-files root)))))

(defn- delete-tree!
  [^File dir]
  (when (and dir (.exists dir))
    (doseq [^File f (reverse (file-seq dir))]
      (.delete f))))

(defonce ^:private snapshot-home
  ;; ONE temp tree per process for every frozen import root, removed at exit.
  (delay (let [d (.toFile (Files/createTempDirectory "vis-ext-code" (make-array FileAttribute 0)))]
           (.addShutdownHook (Runtime/getRuntime)
                             (Thread. ^Runnable
                                      (fn []
                                        (delete-tree! d))))
           d)))

(defn- freeze-root!
  "FREEZE an import root: copy it into a private temp tree and hand THAT to
   `sys.path`. After this the extension's own imports resolve against the bytes
   this load admitted - a module edited or planted next to it afterwards is
   invisible until the next `/reload`, which is the whole freshness contract.

   Returns `{:dir <frozen dir> :code-sha <digest>}`; the digest is taken over the
   COPY, so an entry's `:code-sha` describes the bytes that actually ran."
  [^File root]
  (let [dest (io/file @snapshot-home (str (java.util.UUID/randomUUID)))]
    (.mkdirs dest)
    (doseq [[rel ^File f] (import-root-files root)]
      (let [t (io/file dest rel)]
        (io/make-parents t)
        (io/copy f t)))
    {:dir dest :code-sha (code-sha dest)}))

(defn- close-quietly!
  "Close a GraalPy context, swallowing what it throws. Every close in this
   namespace tears down a context that is already superseded, dead or being
   replaced, so a failing close must never take the load - or the failure being
   reported - down with it."
  [ctx]
  (when ctx (try (.close ^Context ctx true) (catch Throwable _ nil))))

(defn- load-file!
  "Evaluate one extension file in a fresh trusted context and register the
   extension it declares. The import root - the file's own directory - is FROZEN
   into a private snapshot first and THAT is prepended to `sys.path`, so a
   sibling package/module (`my_ext.py` next to a `mypkg/` package, or its own
   `*_impl.py` helpers) imports with a plain `import mypkg` AND imports the bytes
   this load admitted. Pointed at the live directory instead, a lazy
   `import helper` inside a symbol would execute whatever the disk holds when the
   call finally runs - new code in the trusted context with no `/reload` anywhere
   in the chain.

   `frozen` reuses an already-frozen root: one freeze per root per load pass,
   and the heal path re-runs its entry against the root it was admitted with.

   Returns `{:path :sha :code-sha :snapshot :ext-name :context}`; throws (with
   the context closed) on any failure."
  ([^File f] (load-file! f nil))
  ([^File f frozen]
   (let
     [path
      (.getCanonicalPath f)

      frozen
      (or frozen (freeze-root! (import-root f)))

      ^File snap
      (:dir frozen)

      source
      (slurp (io/file snap (.getName f)))

      sha
      (extension/sha256-hex source)

      ctx
      (build-context)]

     (try (bind-host! ctx (.getName f))
          (locking ctx
            (.eval ctx "python" ^String bootstrap-python)
            ;; Prepend the FROZEN copy of the extension file's own dir to
            ;; sys.path so sibling packages/modules import cleanly, and import
            ;; the admitted bytes rather than whatever disk holds by the time the
            ;; import runs. Path crosses as a bound member (no string-escaping
            ;; into a Python snippet).
            (let [g (.getBindings ctx "python")]
              (.putMember g "__vis_ext_dir__" ^String (.getCanonicalPath snap))
              (.eval ctx
                     "python"
                     (str "import sys as __vis_pathsys__\n"
                          "if __vis_ext_dir__ not in __vis_pathsys__.path:\n"
                          "    __vis_pathsys__.path.insert(0, __vis_ext_dir__)\n")))
            (.eval ctx (.build (Source/newBuilder "python" ^String source (.getName f)))))
          (let
            [g
             (.getBindings ctx "python")

             reg
             (call-py ctx (.getMember g "__vis_registration__") [])]

            (when (nil? reg)
              (throw (ex-info (str (.getName f) " never called vis.extension(...)")
                              {:type ::no-registration :file path})))
            (let
              [spec
               (registration->spec ctx reg)

               validated
               (extension/register-extension! spec)]

              (tel/log! {:level :info
                         :id ::loaded
                         :data {:file path :ext (:ext/name spec)}
                         :msg (str "Python extension '" (:ext/name spec) "' loaded from " path)})
              {:path path
               :sha sha
               :code-sha (:code-sha frozen)
               :snapshot (.getCanonicalPath snap)
               :ext-name (:ext/name spec)
               :ext validated
               :context ctx}))
          (catch Throwable t (close-quietly! ctx) (throw t))))))

(defn- live-symbol-fn
  "The CURRENTLY registered fn for `[ext-name sym]`, after `dead-ctx` was torn
   down underneath an already-captured binding (issue #103).

   `loaded` is the loader's own truth. When its entry for this extension still
   points at `dead-ctx` - or at another context that is itself gone - the
   registry is stale too: a cached session env, a sandbox binding and the
   registry can all hold the very same closed context. The file is then
   re-evaluated HERE, because `load-python-extensions!` would refuse: its
   fingerprint gate sees an unchanged file. Change listeners are notified so the
   other live surfaces pick the fresh rows up as well.

   The rebuild runs the bytes THIS PROCESS LOADED and no others: healing is the
   one re-execution with no human act behind it, so a file edited since the load
   is refused and waits for `/reload`.

   Returns nil when the extension is gone, its file changed, or the rebuild
   fails; the caller then reports the original failure."
  [ext-name sym ^Context dead-ctx]
  (try
    (when-let
      [[path entry] (first (filter (fn [[_ e]]
                                     (= ext-name (:ext-name e)))
                                   @loaded))]
      (let
        [entry
         (if-not (or (identical? dead-ctx (:context entry)) (context-dead? (:context entry)))
           entry
           ;; The rebuild re-executes the extension, and its whole import root
           ;; may have moved since this process loaded it — a heal is not a back
           ;; door for bytes nobody reloaded, and a sidecar module is as much the
           ;; extension as its entry file. A process serves exactly what its own
           ;; start or the last `/reload` loaded, so a changed tree heals into
           ;; nothing and the caller reports the original failure until the next
           ;; `/reload`.
           (if-not (= (:code-sha entry) (code-sha (import-root (io/file path))))
             (do (tel/log! {:level :warn
                            :id ::heal-refused
                            :data {:extension ext-name :file path}
                            :msg (str "Python extension '" ext-name
                                      "' changed on disk since it was loaded - not running the"
                                      " new version; run /reload to pick it up")})
                 nil)
             (let
               [rebuilt (load-file! (io/file path)
                                    (let [snap (io/file (:snapshot entry))]
                                      (when (.isDirectory snap)
                                        {:dir snap :code-sha (:code-sha entry)})))]
               (swap! loaded assoc path (dissoc rebuilt :path))
               (close-quietly! dead-ctx)
               (tel/log! {:level :info
                          :id ::context-rebuilt
                          :data {:extension ext-name :file path :symbol (str sym)}
                          :msg
                          (str "Rebuilt torn-down context for Python extension '" ext-name "'")})
               (notify-change-listeners! {:extensions (vec (keep :ext (vals @loaded))) :removed []})
               rebuilt)))]
        (some (fn [e]
                (when (= sym (:ext.symbol/symbol e)) (:ext.symbol/fn e)))
              (get-in (:ext entry) [:ext/engine :ext.engine/symbols]))))
    (catch Throwable t
      (tel/log! {:level :warn
                 :id ::heal-failed
                 :data {:extension ext-name :symbol (str sym) :error (ex-message t)}
                 :msg (str "Could not recover Python extension '" ext-name "' - " (ex-message t))})
      nil)))

(declare register-loader-extension!)

(defn load-python-extensions!
  "Scan the Python extension dirs (default: `~/.vis/extensions` and
   `<cwd>/.vis/extensions`) and (re)load every `*.py` file. Idempotent:
   when no file changed since the last scan this is a cheap no-op. On any
   change the whole set is torn down and rebuilt (contexts are ~40ms warm
   on the shared engine) — deterministic ordering, no partial states.

   Change is measured over each extension's WHOLE import root, never its entry
   file alone — a package module the entry imports is part of the extension —
   and every root is FROZEN at load (`freeze-root!`), one freeze per root per
   pass.

   A file that fails to load is recorded in `load-failures` (and surfaced
   by `vis-agent doctor`) — it never crashes the host.

   Returns `{:loaded n :failed n :changed? bool}`."
  ([] (load-python-extensions! nil))
  ([{:keys [dirs]}]
   (register-loader-extension!)
   (let
     [dirs
      (or dirs (default-extension-dirs))

      files
      (scan dirs)

      roots
      (atom {})

      per-root
      (fn [k ^File root f]
        (let [rk (.getCanonicalPath root)]
          (or (get-in @roots [rk k])
              (let [v (f root)]
                (swap! roots assoc-in [rk k] v)
                v))))

      fp
      (mapv (fn [^File f]
              [(.getCanonicalPath f) (extension/sha256-hex (slurp f))
               (per-root :code-sha (import-root f) code-sha)])
            files)]

     (if (= fp @last-fingerprint)
       {:loaded (count @loaded) :failed (count @failures) :changed? false}
       (let
         [old-loaded
          @loaded

          old-names
          (set (map :ext-name (vals old-loaded)))

          scanned
          (set (map (fn [^File f]
                      (.getCanonicalPath f))
                    files))]

         (reset! failures [])
         ;; Build-then-swap, file by file. A file that reloads cleanly swaps in
         ;; (its PREVIOUS context is closed only after the new one is live); a
         ;; file that FAILS keeps its last-good entry — still registered, context
         ;; still open — untouched. So a failed reload never leaves the stale
         ;; old+dead mix issue #44 reported (old symbols bound to a CLOSED
         ;; context → "Context execution was cancelled", new symbols missing):
         ;; the live surface holds the working last-good module wholesale.
         ;; `vis-agent doctor` (a fresh process, no last-good) and a live `/reload`
         ;; run the SAME loader and diverge only in the fallback for a failed
         ;; load — nothing to fall back to vs. the retained last-good.
         (doseq [^File f files]
           (let
             [path (.getCanonicalPath f)
              prev-ctx (get-in @loaded [path :context])]

             (try (let
                    [{:keys [ext-name] :as entry}
                     (load-file! f (per-root :frozen (import-root f) freeze-root!))]
                    ;; A later file (project dir) registering the same extension
                    ;; name supersedes an earlier one at a DIFFERENT path — the
                    ;; registry already swapped the registration; close the
                    ;; superseded context so its adapters can't linger.
                    (doseq
                      [[opath {oname :ext-name ^Context octx :context}] @loaded
                       :when (and (= oname ext-name) (not= opath path))]

                      (close-quietly! octx)
                      (swap! loaded dissoc opath))
                    (swap! loaded assoc path (dissoc entry :path))
                    (close-quietly! prev-ctx))
                  (catch Throwable t
                    (tel/log! {:level :warn
                               :id ::load-failed
                               :data {:file (str f) :error (ex-message t)}
                               :msg (str "Python extension failed to load: " f
                                         " — " (ex-message t))})
                    (swap! failures conj {:file (str f) :error (ex-message t)})))))
         ;; Files that vanished from disk since the last scan (deleted / renamed)
         ;; have no entry to retain — deregister and close so they don't linger.
         (doseq
           [[opath {:keys [ext-name] :as e}]
            @loaded

            :when (not (scanned opath))]

           (try (extension/deregister-extension! ext-name) (catch Throwable _))
           (close-quietly! (:context e))
           (swap! loaded dissoc opath))
         ;; Frozen code this process no longer serves. A retained last-good entry
         ;; (its own reload failed) still points at its snapshot, so only trees
         ;; nothing references are removed.
         (let [live (set (keep :snapshot (vals @loaded)))]
           (doseq
             [s (distinct (keep :snapshot (vals old-loaded)))
              :when (not (live s))]

             (delete-tree! (io/file s)))
           (doseq
             [^File s (keep (comp :dir :frozen) (vals @roots))
              :when (not (live (.getCanonicalPath s)))]

             (delete-tree! s)))
         (reset! last-fingerprint fp)
         ;; Propagate to live surfaces (cached session envs, TUI slash
         ;; palette). Without this a /reload only updates the GLOBAL
         ;; registry: new extensions stay invisible to running sessions
         ;; and stale env rows keep calling into the closed contexts.
         (let
           [entries
            (vals @loaded)

            new-names
            (set (map :ext-name entries))]

           (notify-change-listeners! {:extensions (vec (keep :ext entries))
                                      :removed (vec (sort (remove new-names old-names)))}))
         {:loaded (count @loaded) :failed (count @failures) :changed? true})))))

(defn ensure-python-extensions-loaded!
  "Load the Python extension dirs only when this process has not loaded them
   yet, and NEVER pick an edit up.

   The freshness contract: a running process serves exactly the extension bytes
   its own start loaded, or the ones the last `/reload` loaded. Editing a `.py`
   on disk changes nothing until a human reloads. Every implicit load path — a
   session env cache miss, an env recycle, a `sub_loop` child env — goes through
   HERE rather than `load-python-extensions!`, whose content fingerprint would
   otherwise re-execute an edited file's top level at the next cache miss, with
   no human act anywhere in the chain.

   Same return shape as `load-python-extensions!`."
  ([] (ensure-python-extensions-loaded! nil))
  ([opts]
   (if (nil? @last-fingerprint)
     (load-python-extensions! opts)
     {:loaded (count @loaded) :failed (count @failures) :changed? false})))

(defn reload-python-extensions!
  "Force a full reload of every Python extension (even when no file
   changed). Same return shape as `load-python-extensions!`. Live
   sessions pick the new tool bindings up at the next turn boundary."
  ([] (reload-python-extensions! nil))
  ([opts] (reset! last-fingerprint nil) (load-python-extensions! opts)))

;; =============================================================================
;; The loader's own host extension: `/reload` + doctor surface
;; =============================================================================

(def ^:private diffed-config-keys [:network :filesystem :jail :toggles])

(defn- config-diff
  "Short, redacted summary of changed top-level config keys between the
   pre- and post-reload merged config. Each changed key reports only that it
   moved. nil when nothing changed."
  [old new]
  (let
    [tokens (keep (fn [k]
                    (let
                      [o (get old k)
                       n (get new k)]

                      (when (not= o n) (name k))))
                  diffed-config-keys)]
    (when (seq tokens) (str/join ", " tokens))))

(defn- reload-slash
  [_ctx]
  ;; One user-facing reload for EVERY hot-reloadable resource: configuration,
  ;; Python extensions, project guidance (AGENTS.md/CLAUDE.md stack), prompt
  ;; templates, and any extension-owned discovery cache registered as a
  ;; reload hook (harness skills/agents).
  (let
    [{:keys [loaded failed]}
     (reload-python-extensions!)

     old-config
     (config/current-config)

     _config
     (config/reload-config!)

     ;; Feature toggles live in the `toggles:` slot of the merged YAML and are
     ;; otherwise hydrated ONLY at process start (gateway
     ;; `install-toggle-persistence!`, TUI `screen/run-chat!`). Without this the
     ;; in-memory registry keeps the pre-edit value, so turning a capability off
     ;; in `vis.yml` (e.g. `web_search: false`) had no effect until a full
     ;; restart — `/reload` said "Reloaded" while the tool stayed live. Re-hydrate
     ;; from the freshly re-read raw config so YAML is the source of truth again;
     ;; ids absent from the file keep their current in-memory value.
     _toggles
     (try (toggles/hydrate-from-config! (or (config/load-config-raw) {})) (catch Throwable _ nil))

     cfg-changes
     (config-diff old-config (config/current-config))

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
     (str "Reloaded — configuration" (when cfg-changes (str " (" cfg-changes ")"))
          "; Python extensions: " loaded
          " loaded" (when (pos? (long failed))
                      (str ", "
                           failed
                           " failed (last-good kept): "
                           (str/join "; "
                                     (map (fn [{:keys [file error]}]
                                            (str (.getName (io/file ^String file)) " — " error))
                                          (load-failures)))
                           " — see `vis-agent doctor`"))
          "; skills/agents, prompt templates" (when template-cnt (str " (" template-cnt ")"))
          ", and context files rescanned" (when (seq failed-hooks)
                                            (str " — hook failures: "
                                                 (str/join ", " (map str failed-hooks)))))}))

(def ^:private http-methods
  #{"GET" "HEAD" "POST" "PUT" "PATCH" "DELETE" "OPTIONS" "TRACE" "CONNECT"})

(defn- parse-probe-target
  "Parse `/net-probe` argv `[METHOD] <url | host[:port]>` into a synthetic egress
   `ctx`. An http(s) URL → an HTTP-phase ctx (method + path visible); a bare
   host[:port] → a SOCKS-phase ctx (host + port only, like ssh/db). Returns
   `{:scheme s :ctx m}` or `{:error msg}`."
  [argv]
  (let
    [[a b]
     (remove str/blank? argv)

     [method target]
     (if (and a b (contains? http-methods (str/upper-case a))) [(str/upper-case a) b] ["GET" a])]

    (cond (str/blank? target) {:error "Usage: /net-probe [METHOD] <url | host[:port]>"}
          (re-find #"(?i)^https?://" target)
          (try (let
                 [u
                  (java.net.URI. target)

                  host
                  (.getHost u)

                  scheme
                  (str/lower-case (str (.getScheme u)))

                  port
                  (let [p (.getPort u)]
                    (if (pos? p) p (if (= "https" scheme) 443 80)))

                  raw
                  (.getRawPath u)

                  query
                  (.getRawQuery u)

                  base
                  (if (str/blank? raw) "/" raw)

                  path
                  (if (str/blank? query) base (str base "?" query))]

                 (if (str/blank? host)
                   {:error (str "Can't parse a host out of " target)}
                   {:scheme scheme
                    :ctx {:phase :http :method method :host host :path path :port port}}))
               (catch Exception e {:error (str "Bad URL: " (ex-message e))}))
          :else (let
                  [[host ports]
                   (str/split target #":" 2)

                   port
                   (some-> ports
                           str/trim
                           parse-long)]

                  (if (str/blank? host)
                    {:error (str "Can't parse a host out of " target)}
                    {:scheme "socks"
                     :ctx {:phase :socks :method nil :host host :path nil :port (or port 0)}})))))

(defn- session-network-policy
  "The live compiled egress policy for the current merged config (the same path
   the loop feeds the proxy), or nil when no network restriction is declared."
  []
  (try (some-> (security-policy/snapshot (or (config/load-config-raw) {}))
               :network
               egress/compile-policy)
       (catch Throwable _ nil)))

(defn- render-probe
  [{:keys [scheme ctx]} {:keys [tier1 filters final]}]
  (let
    [{:keys [host port method path]}
     ctx

     row
     (fn [{:keys [owner allow? reason error]}]
       (str "  • "
            owner
            " → "
            (if allow? "ALLOW" "DENY")
            (when (and (not allow?) reason (not error)) (str " — " reason))
            (when error
              (str "\n      ⚠ CRASHED (fail-closed): "
                   (:message error)
                   (when (:trace error) (str "\n" (:trace error)))))))]

    (str "Target: "
         (str/upper-case (str scheme))
         " "
         (when method (str method " "))
         host
         ":"
         port
         (when path path)
         "\n\n"
         "Tier-1 (host / port / SSRF): "
         (if (:allow? tier1) "ALLOW" (str "DENY — " (:reason tier1)))
         "\n"
         "network_filters (" (count filters)
         "):\n" (if (seq filters) (str/join "\n" (map row filters)) "  (none registered)")
         "\n\nFINAL: " (if (:allow? final) "ALLOW" (str "DENY — " (:reason final))))))

(defn net-probe-report
  "Guard-only egress probe for the in-sandbox `network_probe(...)` tool. Parses
   `target` (an http(s) URL or a bare `host[:port]`), then runs the gateway's
   Tier-1 host/port/SSRF gate + EVERY registered network filter over a SYNTHETIC
   ctx via [[egress/probe]] — PURE: no socket, no egress, nothing is sent. Returns
   a JSON string `{scheme, ctx, tier1, filters}` (or `{error}`) — the strings-only
   boundary the sandbox glue `json.loads`es before merging its own local
   `network_filter`s and printing the verdict. `method` may be blank/nil.
   `headers-json` is a JSON object string of request headers (or blank) and `body`
   is the request body string (or blank); both are merged into the synthetic
   HTTP-phase ctx so `:headers`/`:body` filter rules can be simulated."
  [method target headers-json body]
  (let [parsed (parse-probe-target (remove str/blank? [method target]))]
    (if-let [e (:error parsed)]
      (json/write-json-str {"error" e})
      (let
        [{:keys [scheme ctx]} parsed
         hdrs (try (let
                     [m (when-not (str/blank? headers-json)
                          (json/read-json headers-json :key-fn identity))]
                     (when (map? m) m))
                   (catch Throwable _ nil))
         ctx (cond-> ctx
               (seq hdrs)
               (assoc :headers hdrs)

               (not (str/blank? body))
               (assoc :body body))
         {:keys [tier1 filters]} (egress/probe (session-network-policy) ctx)]

        (json/write-json-str
          {"scheme" scheme
           "ctx" {"phase" (name (:phase ctx))
                  "method" (:method ctx)
                  "host" (:host ctx)
                  "path" (:path ctx)
                  "port" (:port ctx)
                  "headers" (or (:headers ctx) {})
                  "body" (:body ctx)}
           "tier1" {"allow" (boolean (:allow? tier1)) "reason" (:reason tier1)}
           "filters" (mapv (fn [{:keys [owner allow? reason error]}]
                             {"owner" (str owner)
                              "allow" (boolean allow?)
                              "reason" reason
                              "error" (when error
                                        {"message" (:message error) "trace" (:trace error)})})
                           filters)})))))

(defn- net-probe-slash
  "Dev/debug: run the host allow/deny gate + EVERY registered `network_filter`
   over a synthetic request WITHOUT touching the network, and show each verdict
   plus any Python traceback. The one loop for developing filters in-place:
   edit the extension `.py`, `/reload`, `/net-probe …`."
  [ctx]
  (let [parsed (parse-probe-target (:command/argv ctx))]
    (if-let [e (:error parsed)]
      {:slash/status :error :slash/title "Net probe" :slash/body e}
      (let [report (egress/probe (session-network-policy) (:ctx parsed))]
        {:slash/status (if (:allow? (:final report)) :ok :error)
         :slash/title (str "Net probe — " (if (:allow? (:final report)) "ALLOW" "DENY"))
         :slash/body (render-probe parsed report)
         :slash/data report}))))

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
    ;; `/test` + `vis-agent extension test` live in the sibling `python-test-runner` ns.
    ;; Resolve them lazily so THIS loader ns carries no compile-time dependency
    ;; on the runner (which itself depends on this ns's trusted-context builder
    ;; — the one seam that would otherwise be a require cycle).
    (let
      [test-cli!
       (requiring-resolve 'com.blockether.vis.internal.python-test-runner/test-cli!)

       test-slash
       (requiring-resolve 'com.blockether.vis.internal.python-test-runner/test-slash)]

      (extension/register-extension!
        {:ext/name "python-extensions"
         :ext/description
         "Loads Python extensions from ~/.vis/extensions and <project>/.vis/extensions."
         :ext/kind "host"
         :ext/source-nses ['com.blockether.vis.internal.python-extensions]
         :ext/slash-commands
         [{:slash/name "reload"
           :slash/doc
           "Reload configuration, Python extensions, skills/agents, prompt templates, and context files."
           :slash/run-fn reload-slash}
          {:slash/name "test"
           :slash/doc
           "Run every Python extension test (test_*.py / *_test.py) in a trusted GraalPy context."
           :slash/usage "/test"
           :slash/run-fn test-slash}
          {:slash/name "net-probe"
           :slash/doc
           "Debug network filters: run the host allow/deny gate + every registered network_filter over a synthetic request, showing each verdict and any Python traceback."
           :slash/usage "/net-probe [METHOD] <url | host[:port]>"
           :slash/run-fn net-probe-slash}]
         :ext/cli
         [{:cmd/name "test"
           :cmd/internal? true
           :cmd/doc
           "Run every Python extension test (test_*.py / *_test.py) in a trusted GraalPy context."
           :cmd/usage "vis-agent extension test"
           :cmd/examples ["vis-agent extension test"]
           :cmd/run-fn test-cli!}]
         :ext/doctor-fn doctor-fn}))))
