(ns com.blockether.vis.internal.loop.environment
  "Session environment lifecycle and the in-process session cache.

   Creates and disposes a session environment (router, Python sandbox and
   security snapshot), keeps Python extension symbols in sync, caches live
   environments, reaps idle ones, applies `/reload` and provider changes to
   cached sessions, and serializes turns per session."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.anomaly.core :as anomaly]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.extension.client :as client-extensions]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.extension.manifest :as manifest]
            [com.blockether.vis.internal.gateway.resources :as resources]
            [com.blockether.vis.internal.loop.compaction :as compaction]
            [com.blockether.vis.internal.loop.python-exec :as python-exec]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.persistance.codec :as codec]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.sandbox.egress-proxy :as egress]
            [com.blockether.vis.internal.sandbox.gateway :as gateway-sandbox]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.sandbox.policy :as security-policy]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace.core :as workspace]
            [taoensso.telemere :as tel]))

(defn sync-extension-symbols-into!
  "The symbol sync itself, against a context handed in EXPLICITLY.

   Separate from [[sync-active-extension-symbols!]] for one caller: the delay
   that builds a session's sandbox runs this with its own fresh context. That
   caller cannot reach the context through `environment`, because doing so
   would re-enter the very delay it is running inside, and a Clojure delay
   answers re-entry with a deadlock — the session's first turn would hang."
  [python-context environment active-extensions]
  (let [installed
        (vec (or (some-> (:extensions environment)
                         deref)
                 []))

        active-set
        (set (map :ext/name active-extensions))]

    (doseq [ext
            installed

            :let [alias
                  (extension/ext-alias-symbol ext)

                  exact-names?
                  (extension/ext-exact-symbol-names? ext)

                  by-sym
                  (into {} (map (juxt :ext.symbol/symbol identity) (extension/ext-symbols ext)))]
            [sym f]
            (try (extension/wrap-extension ext environment) (catch Throwable _ nil))]

      ;; Clojure extensions use `<alias>_<name>` in Python. Python-authored
      ;; extensions declare their public names verbatim: the registry alias is
      ;; metadata only, and dotted names become safe namespace objects in
      ;; env/set-python-binding!. Builtins carry no alias and remain bare.
      ;;
      ;; Deactivated extensions get their members REMOVED, not nil'd:
      ;; `putMember nil` parks a None under the name, which `apropos` kept
      ;; listing and which called as 'NoneType is not callable' — a disabled
      ;; tool must not exist in the sandbox at all.
      (let [target (if (and alias (not exact-names?))
                     (clojure.core/symbol (str alias "/" (name sym)))
                     sym)]
        ;; Bound only when the extension is active and the symbol's `:active-fn`
        ;; holds for env — one gate for every Python binding.
        (if (and (contains? active-set (:ext/name ext))
                 (extension/symbol-active? (get by-sym sym) environment))
          (do (env/set-python-binding! python-context target f)
              ;; Seed this symbol's doc into `__vis_docs__` keyed by its bound
              ;; py-name, so `doc(db_status)` / `doc(mcp_servers)` /
              ;; `apropos("mcp")` carry real descriptions. ALIASED extensions
              ;; bind here (per turn), NOT at context creation, so the eager
              ;; `build-agent-context` seed never saw them.
              (env/set-python-binding-doc! python-context
                                           target
                                           (extension/symbol-doc-text (get by-sym sym)))
              ;; ...and its declared signature, so `inspect.signature` / `help` /
              ;; `typing.get_type_hints` on an aliased tool answer with real
              ;; parameters and annotations instead of the async trampoline's
              ;; own `(*a, **k)`.
              (env/set-python-binding-signature! python-context
                                                 target
                                                 (extension/symbol-signature (get by-sym sym)))
              (env/set-python-binding-contract! python-context
                                                target
                                                (:ext.symbol/contract (get by-sym sym)))
              ;; ...and the keys its options dict must carry, so `doc(name)`
              ;; states requiredness for an aliased tool too.
              (env/set-python-binding-keys! python-context
                                            target
                                            (extension/symbol-keys-line (get by-sym sym))))
          (env/remove-python-binding! python-context target))))))

(defn sync-active-extension-symbols!
  "Make the Python sandbox's callable globals match active extension state.

   `install-extension!` keeps every extension row in `:extensions`, but only
   active extensions contribute callable symbols. Called after per-env
   installation and again at turn start so `:ext/activation-fn` changes become
   real tool availability, not just prompt visibility.

   The Python sandbox is FLAT globals (no namespaces/aliases/macros): active
   extensions putMember their symbols straight into the top scope; deactivated
   extensions have theirs removed (putMember nil). Symbol names are snake-ified
   by env/set-python-binding!."
  ([environment]
   (sync-active-extension-symbols! environment (prompt/active-extensions environment)))
  ([environment active-extensions]
   (when-let [active-atom (:active-extensions environment)]
     (reset! active-atom (vec (or active-extensions []))))
   ;; Reads the sandbox WITHOUT building one. Per-env installation runs while a
   ;; session is still cold, and starting an interpreter to seed symbols nobody
   ;; asked for is exactly the wait this laziness exists to remove. A sandbox
   ;; built later seeds itself: `create-environment`'s delay ends by calling
   ;; `sync-extension-symbols-into!` with its own fresh context.
   (when-let [python-context (env/python-context-if-built environment)]
     (sync-extension-symbols-into! python-context environment active-extensions))
   environment))

(defn install-extension!
  "Register a validated extension into `environment` (per-env registration,
   distinct from the global-registry `register-extension!` defined earlier
   in this file).

   If an extension with the same `:ext/name` is already registered,
   it is replaced (not duplicated). Enables hot-swap via
   `reload-extension!` (removed for GraalVM native-image compatibility).

   Returns `environment` for chaining."
  [environment ext]
  (when-not (:extensions environment)
    (anomaly/incorrect! "Invalid vis environment - missing :extensions atom"
                        {:type :vis/invalid-env}))
  (swap! (:extensions environment) (fn [exts]
                                     (let [ns-sym
                                           (:ext/name ext)

                                           without
                                           (vec (remove #(= (:ext/name %) ns-sym) exts))]

                                       (conj without ext))))
  ;; Extension rows stay installed even when inactive, but callable symbol
  ;; bindings are activation-aware (sync-active-extension-symbols!). The Python
  ;; sandbox has no passive Java class/import config — the agent writes Python +
  ;; uses its own stdlib; the Clojure tools do any Java work.
  (sync-active-extension-symbols! environment)
  environment)

;; Environment Lifecycle

(defn dispose-environment!
  "Disposes a vis environment and releases resources. For persistent DBs
   (created with `:path`), data is preserved. For disposable DBs, all
   data is deleted.

   Every env owns its DB connection, so disposing one always closes it."
  [environment]
  ;; Drop this session from the SHARED gateway egress proxy's registry. The shared
  ;; proxy + CA are daemon-lifetime (internal.sandbox.gateway/shutdown!), not
  ;; per-session, so nothing is stopped here — only this session's policy is removed.
  ;; EVERY step before the sandbox is best-effort AND cannot skip it. These run
  ;; first because they need the environment intact, but not one of them is worth
  ;; the Python worker: a throw here used to abandon that whole process, and the
  ;; only caller that recycles between turns swallows the exception — so a single
  ;; failing unregister leaked a worker silently every five turns. Worker teardown
  ;; is therefore a `finally`, and a failure is LOGGED rather than dropped, because
  ;; a leak nothing reports is one nobody can find.
  (try
    (doseq [[step run!] [[:egress-proxy
                          #(when-let [tok (:sandbox-token environment)]
                             (gateway-sandbox/unregister-session! tok))]
                         [:llm-session
                          #(when-let [a (:llm-session-atom environment)]
                             (locking a (transcript/close-llm-session! a)))]
                         ;; BEFORE the context goes: the session's helper-source memo outlives
                         ;; both the context and the engine, and nothing else ever drops it
                         ;; (see `env-python/forget-session-defs!`).
                         [:session-defs
                          #(when-let [sid (:session-id environment)] (env/forget-session-defs!
                                                                       sid))]]]
      (try (run!)
           (catch Throwable t
             (tel/log! :warn
                       ["gateway: env teardown step failed" (name step)
                        (str (:session-id environment)) (ex-message t)]))))
    (finally
      ;; The sandbox goes LAST and always. For a gateway session this kills its
      ;; worker process and releases both the sandbox and trusted extension
      ;; namespaces without entering a possibly wedged interpreter.
      ;; A sandbox that was never built has no interpreter to kill, and building
      ;; one here would start a process for the sole purpose of ending it.
      (try (env/dispose-sandbox! environment)
           (catch Throwable t
             (tel/log! :error
                       ["gateway: sandbox dispose failed - session LEAKED"
                        (str (:session-id environment)) (ex-message t)])))
      (when (:db-info environment)
        (try (persistance/db-dispose-connection! (:db-info environment))
             (catch Throwable t
               (tel/log! :warn ["gateway: env db close failed" (ex-message t)])))))))

(defonce ^:private last-good-security-snapshot
  ;; A failed project reload may retain only that project's last valid policy.
  (atom {}))

(defn- security-config-snapshot
  "Read, validate, resolve, and hash security configuration once for the bound
   workspace. Child environments inherit this exact immutable value.

   Invalid live configuration retains only this workspace's last-good snapshot,
   or a deny-safe default on its first load. Another project's grants must never
   become the fallback. An explicit rebuild replaces the snapshot."
  []
  (let [root (.getCanonicalPath (workspace/cwd))]
    (try (let [snap (security-policy/snapshot (or (config/load-config-raw) {}) {:base-dir root})]
           (swap! last-good-security-snapshot assoc root snap)
           snap)
         (catch Throwable e
           (let [last-good (get @last-good-security-snapshot root)
                 invalid? (and (instance? clojure.lang.ExceptionInfo e)
                               (= :vis/invalid-config (:type (ex-data e))))]

             (tel/log! {:level :warn
                        :id ::security-config-invalid
                        :data
                        (if invalid? {:problems (:problems (ex-data e))} {:error (ex-message e)})
                        :msg (str "security config could not be applied; "
                                  (if last-good
                                    "keeping this workspace's last-good policy"
                                    "falling back to a deny-safe policy")
                                  " so the session survives")})
             (let [problems (try (config/config-problems) (catch Throwable _ nil))
                   base (or last-good
                            (try (security-policy/snapshot {} {:base-dir root})
                                 (catch Throwable _ {})))]

               (assoc base
                 :config-error {"source" (or (:source (ex-data e)) "vis.yml / ~/.vis/state.yml")
                                "message" (str "The live config on disk could not be applied; "
                                               (if last-good
                                                 "the last-good policy is in effect."
                                                 "a deny-safe policy is in effect."))
                                "problems" (if (seq problems) (vec problems) [(ex-message e)])
                                "hint"
                                (str "Fix the keys above in vis.yml or ~/.vis/state.yml, then run "
                                     "/reload. Keys are snake_case strings; the config is closed, "
                                     "so unknown or renamed keys are rejected.")})))))))

(defn create-environment
  "Creates a vis environment (component) for session lifecycle and
   querying.

   The environment holds:
     - Python sandbox context with custom bindings + bindings cache
     - DB connection (or shared-mem datasource)
     - Router (LLM provider config)
     - Extension registry atom

   Params:
     `router` - Required. Result of `llm/make-router`.
     `opts`   - Map with `:db` and optional `:session`,
                 `:channel`, `:external-id`, `:title`.

     `:db` accepted forms:
       nil               - no DB (sandbox-only execution)
       :memory           - ephemeral in-process SQLite DB
       path string       - persistent SQLite DB at path
       {:path p}         - persistent SQLite DB at path
       {:datasource ds}  - caller-owned DataSource (not closed on dispose)

   Returns the vis environment map."
  [router {:keys [db session channel external-id title workspace-id]}]
  (when-not router (anomaly/incorrect! "Missing router" {:type :vis/missing-router}))
  ;; Everything from here to the end runs with the sandbox GUARDED: the session is
  ;; built ~130 lines before this function returns, and workspace resolution,
  ;; extension registration and the defs restore all still run after it. A throw in
  ;; that stretch used to ABANDON the sandbox — and an abandoned one is never
  ;; reclaimed, because nothing else disposes a session the loop no longer points
  ;; at: its namespace, its host doors and the native memory behind them stay live
  ;; for the process. So the failure path leaked worse than success ever could, on
  ;; exactly the runs a caller would retry.
  (let [pending (volatile! nil)]
    (try
      (let [db-info (persistance/db-create-connection! db)
            resolved-session-id (persistance/db-resolve-session-id db-info session)
            persisted-session (when resolved-session-id
                                (persistance/db-get-session db-info resolved-session-id))
            ;; A rebuild normally receives only the session id. Recover the immutable
            ;; channel from persistence so channel-owned stable prompt blocks remain
            ;; byte-identical across processes (notably the autonomous CLI block).
            resolved-channel (or channel (:channel persisted-session) :tui)
            state-atom (atom {:custom-bindings {} :environment nil :session-id nil})
            environment-atom (atom nil)
            environment-id (str (random-uuid))
            ;; SINGLE turn-state atom holds all per-turn cursor fields
            ;; (current-{turn-position,iteration,form-idx,iteration-id,
            ;;  session-turn-id,user-request}-atom). All six fields live
            ;; under map keys with the same names minus `current-` /
            ;; `-atom`. Reads via `ctx-loop/read-turn-state`; writes via
            ;; `ctx-loop/set-turn-state!` / `swap-turn-state!`. Extension
            ;; symbol wrappers close over THIS atom; the loop swap!s it
            ;; between turns and forms.
            turn-state-atom (ctx-loop/make-turn-state-atom)
            ;; Seed iteration to 1 so early hooks reading the atom before
            ;; the loop's per-turn reset see a sensible value.
            _ (swap! turn-state-atom assoc :iteration 1)
            ;; Title atom: in-memory cache for the session title.
            ;; The DB column on `session_state` is the persisted
            ;; truth; this atom is the fast read path for  and
            ;; the source for the title hint / channel chrome at iteration
            ;; boundaries. `set-title!` writes both, in that order, then
            ;; broadcasts to every registered listener.
            ;; On RESUME (no caller-supplied title) seed the atom from the PERSISTED
            ;; session title. Without this a fresh process starts the atom empty, so
            ;; `maybe-auto-title!`'s guard sees "untitled" and RE-titles the session
            ;; from the next message (e.g. a "continue") — overwriting a good title
            ;; cross-process. Placeholder titles ("Untitled") still fall through to
            ;; auto-title via `usable-existing-title`.
            resolved-title (or (not-empty (str title))
                               (some-> persisted-session
                                       :title
                                       str
                                       not-empty))
            session-title-atom (atom (or resolved-title ""))
            root-resolved-model (loop-router/resolve-effective-model router)
            root-model (or (:name root-resolved-model) "unknown")
            root-provider (:provider root-resolved-model)
            ;; Routing digest surfaced in the model-facing ctx (`routing`): the CURRENT
            ;; model + provider, nothing more. The provider/model CATALOG is deliberately
            ;; NOT shipped — there is no child dispatch to act on it, and it cost ~445
            ;; tokens on EVERY request. STRING-KEYED: this
            ;; digest lands in ctx as `session_routing` and crosses the Python boundary.
            routing-digest (cond-> {"model" root-model}
                             root-provider
                             (assoc "provider" (name root-provider)))
            ;; Workspace pin (1:1 with session_state):
            ;;   - resuming a session       → derive workspace from its latest state
            ;;   - brand-new session        → mint a trunk workspace, pass its id
            ;;                                into db-store-session! below
            ;; db-info nil (sandbox-only mode) → skip; iteration loop never asserts
            ;;                                workspace pin when there's no DB
            active-workspace
            (when db-info
              (cond
                ;; Resume path: the existing session already pins a
                ;; workspace; honour it.
                resolved-session-id
                (some->> (persistance/db-latest-session-state-id db-info resolved-session-id)
                         (persistance/db-workspace-for-session db-info))
                ;; New session, caller pre-spawned a workspace
                ;; (e.g. /workspace slash spawn-branch path).
                workspace-id (persistance/db-workspace-get db-info workspace-id)
                ;; New session, no pre-spawn: clone cwd.
                :else (workspace/ensure-workspace! db-info {})))
            ;; Persist the prompt from the pinned project, including on resume.
            system-prompt (prompt/build-system-prompt {:workspace-root (:root active-workspace)})
            session-id (or resolved-session-id
                           (persistance/db-store-session! db-info
                                                          (cond-> {:channel resolved-channel
                                                                   :external-id external-id
                                                                   :model root-model
                                                                   :title title
                                                                   :system-prompt system-prompt
                                                                   :workspace-id (:id
                                                                                   active-workspace)
                                                                   ;; Every session is created for somebody
                                                                   ;; who asked for it, so it is claimed from
                                                                   ;; the start and shows in the cross-channel
                                                                   ;; list.
                                                                   :claimed? true}
                                                            root-provider
                                                            (assoc :provider root-provider))))
            ;; Resolve the session_state row id ONCE here (reliable at env build)
            ;; and stamp it on the env, so workspace operations and turns do not re-query
            ;; it. The per-call re-query intermittently returned nil for fresh sessions.
            session-state-id (when (and db-info session-id)
                               (persistance/db-latest-session-state-id db-info session-id))
            persisted-prompt-cache-state
            (when resolved-session-id (transcript/load-prompt-cache-state db-info session-state-id))
            ;; Context wiring (see ctx-loop). `ctx-atom` carries stable session
            ;; context, while `turn-state-atom` tracks live counters. Seeded fresh;
            ;; reloaded from session_turn_state.ctx (Nippy BLOB) on session resume.
            ctx-atom (ctx-loop/make-ctx-atom session-id)
            ;; Large folds already invalidate the provider cache. Once their cumulative
            ;; newly reclaimed wire crosses the threshold, rebase the standing session
            ;; snapshot too instead of retaining an unbounded chain of historical deltas.
            session-rebase-atom (atom {:reclaimed-tokens 0 :pending? false})
            ;; `fold_session` records a summary or discard intent using the key grammar in
            ;; `ctx-engine/fold-key`, and returns a visible receipt.
            compaction (compaction/compaction-verbs ctx-atom
                                                    session-rebase-atom
                                                    #(compaction/checkpoint-fold! @environment-atom
                                                                                  %))
            ;; Build the ctx-loop env subset used by the engine bindings + helpers.
            ;; Just the cursor counters + the single ctx-atom. Warnings
            ;; live as `:engine/warnings` on the ctx itself, no side atoms.
            ;; (D12 retired `:engine/pending-satisfies` along with
            ;; satisfy-hint!; hook-task satisfaction is plain `plan_step`.)
            _ctx-loop-env {:ctx-atom ctx-atom
                           :turn-state-atom turn-state-atom
                           ;; DB + session id ride on the same env
                           ;; map so `build-introspect-bindings`
                           ;; can hit `session_turn_iteration.forms`
                           ;; for the per-form / per-iter / per-turn
                           ;; introspection verbs without an extra
                           ;; closure capture.
                           :db-info db-info
                           :session-id session-id}
            ;; The current human turn and engine context flow through ctx. Introspection
            ;; reads archived entries and per-form rows directly from the database.
            env-bindings (merge
                           ;; BUILT-IN extension kernel (`foundation`):
                           ;; cat/ls/rg/patch/… interned BARE into the
                           ;; sandbox ns next to the engine verbs — no
                           ;; `v/` alias. env resolved lazily (atom not
                           ;; built yet). Listed FIRST so engine verbs
                           ;; below win any accidental name collision.
                           (extension/builtin-sandbox-bindings (fn []
                                                                 @environment-atom))
                           ;; Engine verbs (no `done` — a plain-text reply
                           ;; finalizes the turn): the compaction verbs.
                           ;; `gather` is GUEST-side now — the interpreter's own
                           ;; worker pool runs the thunks, because a Python
                           ;; callable can only be called from inside the
                           ;; interpreter, never from a host thread.
                           compaction
                           ;; Canonical stateful-resource lifecycle:
                           ;; `resource_stop(id)` (B-dispatch — act by id;
                           ;; ctx advertises can_stop). Session-scoped so the
                           ;; agent only touches THIS session's resources.
                           ;; No context mutator or introspect
                           ;; bindings are installed here.
                           (resources/sandbox-bindings session-id))
            ;; Security configuration is resolved exactly once per environment; it never
            ;; re-reads model-writable vis.yml mid-life. `/reload` bumps
            ;; `policy-reload-epoch`, so each live env recycles at its next turn and
            ;; rebuilds this snapshot.
            security-config (binding [workspace/*workspace-root* (or (:root active-workspace)
                                                                     workspace/*workspace-root*)]
                              (security-config-snapshot))
            configured-rw-roots (security-policy/read-write-roots security-config)
            ;; Engine substrate: embedded CPython (env/create-python-context builds a
            ;; deny-by-default Python session, wires the Clojure tools as Python
            ;; callables, and installs doc/apropos introspection). Its live roots are the
            ;; workspace overlay plus immutable configured read/write roots. Python
            ;; filesystem bindings consume the same roots through the environment below.
            workspace-atom (atom active-workspace)
            sandbox-roots-fn
            (when (or active-workspace (seq configured-rw-roots))
              (fn []
                (let [ws @workspace-atom
                      ;; The same per-root draft resolution every Python binding uses, so the
                      ;; Python sandbox cannot reach a root the draft policy withholds
                      ;; (`:denied?`) or write straight into a root this draft only owns
                      ;; a private copy of — the clone is granted in its place.
                      entries (workspace/env-filesystem-roots {:security-policy security-config
                                                               :workspace ws
                                                               :security/filesystem-roots
                                                               configured-rw-roots})
                      clones (into []
                                   (comp (filter #(and (:clone %) (not= (:clone %) (:trunk %))))
                                         (map :clone))
                                   entries)
                      withheld (into #{}
                                     (comp (filter #(or (:denied? %)
                                                        (and (:clone %)
                                                             (not= (:clone %) (:trunk %)))))
                                           (map :trunk))
                                     entries)]

                  (vec (distinct (concat (when ws [(str (:root ws))])
                                         clones
                                         (remove #(contains? withheld (workspace/normalize-root %))
                                           configured-rw-roots)))))))
            access-view-fn (fn []
                             (let [ws @workspace-atom
                                   live-roots (when ws [(:root ws)])]

                               (security-policy/access-view security-config live-roots)))
            draft-required? (not= :off (workspace/draft-backend-setting))
            draft-home (when draft-required?
                         (let [directory (io/file (workspace/session-drafts-home session-id))]
                           (.mkdirs directory)
                           (.getCanonicalPath directory)))
            draft-env {:workspace-atom workspace-atom
                       :security-policy security-config
                       :security/filesystem-roots configured-rw-roots}
            draft-protected-roots (when draft-required? (process-jail/draft-source-roots draft-env))
            draft-env (assoc draft-env :workspace/draft-protected-roots draft-protected-roots)
            jail-config (:process-jail security-config)
            filesystem-policy-fn
            #(process-jail/runtime-policy
               (process-jail/draft-policy (assoc jail-config :roots-fn sandbox-roots-fn) draft-env))
            jail-enabled? (not (:disabled? jail-config))
            net-cfg (:network security-config)
            ;; Host sockets stay available to the interpreter; the jail is the ONE
            ;; network switch. With the jail OFF there is no egress proxy AND no
            ;; in-interpreter domain guard — the sandbox network is unconfined, the
            ;; same all-or-nothing containment the OS process jail gives subprocesses.
            net-on? true
            ;; One shared gateway proxy serves every environment. Unguessable tokens
            ;; attribute requests to this environment's immutable policy snapshot.
            sandbox-token (str (java.util.UUID/randomUUID))
            compiled-network-policy (some-> (egress/compile-policy net-cfg)
                                            (assoc :mitm? (boolean (seq (:rules net-cfg)))))
            _register-sandbox (when (and sandbox-roots-fn jail-enabled?)
                                (gateway-sandbox/register-session! sandbox-token
                                                                   (constantly
                                                                     compiled-network-policy)))
            ;; The user-controlled keys come only from config-validation/process-jail-config.
            ;; Per-spawn evaluation retains live workspace roots, lazy proxy startup and
            ;; the resolved `environment:` declarations; nothing else re-reads config.
            jail-policy-fn
            (when sandbox-roots-fn
              (fn []
                (let [proxy? (and jail-enabled? net-on?)
                      proxy-port (when proxy? (gateway-sandbox/ensure-proxy!))
                      ca-file (when proxy? (gateway-sandbox/ensure-ca!))
                      java-trust (when proxy? (gateway-sandbox/ensure-java-trust!))
                      worker-proxy-port (when proxy?
                                          (gateway-sandbox/ensure-session-proxy! sandbox-token))]

                  (process-jail/draft-policy
                    (merge jail-config
                           {:roots-fn sandbox-roots-fn
                            :net-enabled? net-on?
                            ;; Resolved per spawn (never baked into the session snapshot), so a
                            ;; `.env` edit or a refreshed keychain item reaches the next child.
                            :env-values (config/child-environment-values)
                            :proxy-port proxy-port
                            :worker-proxy-port worker-proxy-port
                            :proxy-token (when proxy? sandbox-token)
                            :java-trust-store (:java-trust-store java-trust)
                            :java-trust-store-password (:java-trust-store-password java-trust)
                            :ca-file ca-file})
                    draft-env))))
            network-opts {;; The worker launch reads this policy before its first
                          ;; interpreter request; the same snapshot configures its
                          ;; runtime audit-hook backstop immediately afterwards.
                          :worker? true
                          :worker-policy-fn (when jail-policy-fn
                                              #(cond-> (jail-policy-fn) draft-home
                                                 (update :allow-read-write
                                                         (fnil conj [])
                                                         draft-home)))
                          :filesystem-policy-fn filesystem-policy-fn
                          :draft-required? draft-required?
                          :enabled? net-on?
                          :jail-enabled? jail-enabled?
                          :allowed-domains (:allowed-domains net-cfg)
                          :denied-domains (:denied-domains net-cfg)
                          :exclude-domains (:exclude-domains net-cfg)
                          :allow-private (:allow-private net-cfg)
                          :rules (:rules net-cfg)}
            ;; The sandbox is a DELAY, not a value: an interpreter is what makes a
            ;; session expensive to create, and a session that never runs Python
            ;; never needs one. Whoever first enters Python pays for it, through
            ;; `env/python-context`; teardown and liveness checks read the sandbox
            ;; without building one (`env/sandbox-if-built`).
            sandbox
            (delay
              (let [built (env/create-python-context (merge env-bindings
                                                            (:custom-bindings @state-atom))
                                                     sandbox-roots-fn
                                                     network-opts
                                                     nil)
                    python-context (:python-context built)]

                (vreset! pending built)
                ;; Every step past the build carries its own teardown. `create-environment`'s
                ;; try/catch used to cover this stretch; it has long returned by the time
                ;; this delay runs, so the failure path has to live in here. An abandoned
                ;; sandbox is never reclaimed — its Python namespace is a reference cycle
                ;; through every function defined in it, and the host half holds one closure
                ;; per tool — which is why the FAILURE path leaks worse than success can.
                (try
                  ;; A gateway restart or a `/resume` in a new process builds a FRESH sandbox
                  ;; while the transcript still shows the helpers this session refined, so the
                  ;; next call would be a NameError against code the model can read. Re-create
                  ;; them from the snapshot `execute-code` wrote after every block.
                  (env/restore-session-defs! python-context session-id)
                  ;; Extensions installed while this sandbox was still cold skipped their
                  ;; symbol sync; give them their globals now. The context goes in by hand
                  ;; because reaching it through the environment would re-enter THIS delay.
                  (when-let [environment @environment-atom]
                    (sync-extension-symbols-into! python-context
                                                  environment
                                                  (prompt/active-extensions environment)))
                  built
                  (catch Throwable t
                    (try (env/dispose-python-context! python-context) (catch Throwable _ nil))
                    (throw t)))))
            env (cond-> {:environment-id environment-id
                         :session-id session-id
                         :session/state-id session-state-id
                         :channel resolved-channel
                         ;; Immutable canonical security policy plus its live workspace overlay.
                         ;; Python session, native file tools, shell, managed language processes,
                         ;; and egress all derive from this same environment-owned value.
                         :security-policy security-config
                         :security/filesystem-roots configured-rw-roots
                         :security/no-search-roots (security-policy/no-search-roots security-config)
                         :access-view-fn access-view-fn
                         ;; What the Python sandbox can ACTUALLY reach this session —
                         ;; `python-execution-tool` builds its fs/network description
                         ;; from this so the prompt never claims a capability the
                         ;; sandbox lacks (no workspace ⇒ no fs; toggle off ⇒ no net).
                         :sandbox-caps {:fs? (boolean sandbox-roots-fn) :network network-opts}
                         ;; Live workspace pointer for sandbox confinement. run-turn!
                         ;; refreshes it so `sandbox-roots-fn` tracks the active root.
                         :workspace-atom workspace-atom
                         :workspace/drafts-home draft-home
                         :workspace/draft-protected-roots draft-protected-roots
                         ;; routing digest → rendered into ctx as `routing`
                         ;; (current model + provider only).
                         :routing routing-digest
                         :db-info db-info
                         ;; Per-session OS-jail policy fn — the shell jail is ALWAYS ON; nil only when
                         ;; no sandbox roots exist. Shell/subprocess executors consult it per spawn; see process-jail.
                         :jail-policy-fn jail-policy-fn
                         ;; This session's unguessable token for the SHARED gateway egress proxy /
                         ;; MITM CA (internal.sandbox.gateway). Registered at env build; dropped from
                         ;; the proxy's session registry in dispose-environment!.
                         :sandbox-token sandbox-token}
                  ;; Workspace info attached at env-build time so the extension
                  ;; wrapper's `(workspace/workspace-root env)` finds a non-blank
                  ;; root the very first time it fires.
                  active-workspace
                  (assoc :workspace
                    active-workspace :workspace/id
                    (:id active-workspace) :workspace/root
                    (:root active-workspace)
                    ;; Every workspace is a rift CoW clone — always a sandbox.
                    ;; Reported on :workspace/sandbox?, NOT as a VCS. The
                    ;; model-facing :vcs/kind is the real repo VCS, computed in
                    ;; foundation.workspace-ctx/render-block.
                    :workspace/sandbox?
                    true))
            env (assoc env
                  ;; Context atoms — visible to the rest of the loop so renderer /
                  ;; per-iter capture / done snapshot can read or stamp them.
                  :ctx-atom ctx-atom
                  :turn-state-atom turn-state-atom
                  :session-rebase-atom session-rebase-atom
                  ;; PROMPT-CACHE STABILITY: the standing `session = {…}` block rides
                  ;; in the cached system prefix and is normally frozen across turns.
                  ;; State changes ride as appended `session[...] = …` deltas. A large
                  ;; fold deliberately rebases this block to the current materialized
                  ;; session, bounding the delta chain while spending a cache miss that
                  ;; compaction already made useful. The latest fresh terminal persists
                  ;; this exact block and baseline so a quick restart can retain the same
                  ;; provider prefix; stale checkpoints render a fresh canonical block.
                  :standing-ctx-atom (atom (:standing-ctx persisted-prompt-cache-state))
                  :state-atom state-atom
                  ;; The session's sandbox, unbuilt until something enters Python.
                  ;; `env/python-context` / `env/sandbox-ns` force it; teardown and
                  ;; liveness read it through `env/sandbox-if-built` and never do.
                  ;; It also carries what used to sit beside it as `:python-engine`
                  ;; and `:initial-ns-keys` — both come out of the same build.
                  :python-sandbox sandbox
                  ;; A failed guest interrupt makes reuse permanently unsafe even
                  ;; while a probe can still enter around extension-owned host work.
                  ;; The next turn abandons this environment instead.
                  :python-context-retired-atom (atom false)
                  ;; Long-lived per-env LRU map: `{var-name-string →
                  ;; last-used-turn-pos}`. Merged from each iteration's
                  ;; `:lru` after eval.
                  :def-resolve-lru-atom (atom {})
                  :router router
                  ;; A provider with a `:server-continuation` prompt cache keeps one explicit
                  ;; session/cursor per Vis environment, opened lazily on its first iteration
                  ;; and closed with env.
                  :llm-session-atom (atom nil)
                  ;; Compact fingerprints and weights support reuse telemetry without
                  ;; retaining full requests. Exact accepted prefixes live in the single
                  ;; disk checkpoint and are read only for cross-turn restoration.
                  :prompt-cache-history-atom (atom
                                               (if-let [route (:route persisted-prompt-cache-state)]
                                                 {route (transcript/compact-prompt-cache-entry
                                                          (:entry persisted-prompt-cache-state))}
                                                 {}))
                  :session-title-atom session-title-atom
                  :extensions (atom [])
                  :active-extensions (atom []))]

        (reset! environment-atom env)
        (swap! state-atom assoc :environment env :session-id session-id)
        ;; Restore the context state when resuming. Sandbox defs do NOT persist
        ;; across turns (the `definition_*` sidecar tables were dropped).
        (when resolved-session-id
          ;; The latest session_turn_state.ctx (Nippy BLOB) carries the persisted
          ;; context snapshot. Cursor is iter-local so we don't restore it; the
          ;; renderer stamps a fresh one from the loop counters.
          (try (when-let [persisted-ctx (persistance/db-load-latest-ctx db-info session-id)]
                 ;; The Nippy blob IS the whole ctx now (no separate task/fact/archive
                 ;; tables). It has no `"engine_*"` ephemeral keys (stripped before
                 ;; Nippy), so re-seed those empty here so swap! callers don't need
                 ;; nil-guards. Read once, on resume; the live render stays in-memory.
                 (reset! ctx-atom (assoc persisted-ctx
                                    "session_id" session-id
                                    "engine_warnings" []
                                    "engine_pending_satisfies" [])))
               (catch Throwable t
                 (tel/log! {:level :warn
                            :id ::restore-ctx-failed
                            :data {:error (ex-message t) :session-id session-id}
                            :msg "Failed to restore context state from DB - starting empty"}))))
        ;; Initialize the one closed distribution manifest before installing its
        ;; registered extensions into this environment.
        (manifest/initialize!)
        ;; Project-local Python extensions (`.vis/extensions/*.py`) load after
        ;; manifest initialization so they land in the same registry walk below.
        ;; Load-once, never adopt: this runs on every env cache miss, every recycle
        ;; and every child env, and none of those is a human act. Only
        ;; this process's own start and `/reload` may pick an edit up.
        (python-extensions/ensure-python-extensions-loaded!)
        (extension/register-extensions! env install-extension!)
        (doseq [ext (client-extensions/extensions-for session-id)]
          (install-extension! env ext))
        (let [final-env (loop-router/kickoff-session-providers env)]
          ;; Callbacks installed above closed over `environment-atom`; publish the
          ;; fully decorated session before create-environment returns.
          (reset! environment-atom final-env)
          (swap! state-atom assoc :environment final-env)
          final-env))
      (catch Throwable t
        ;; Best-effort: a teardown must never replace the real failure with its own.
        (try (env/dispose-python-context! (:python-context @pending)) (catch Throwable _ nil))
        (throw t)))))

;; Session env cache

;; In-process session cache + channel utilities

(defonce
  ^{:doc
    "In-process env cache.

   Keyed by `java.util.UUID` session-soul-id. Under the 1:1 session ↔
   workspace invariant this key is isomorphic to `(:workspace/id env)`
   — one cache entry = one session = one workspace = one Python sandbox
   lineage. Lookups normalize incoming strings to UUID via `cache-key`
   so string-id callers keep working alongside the UUID key."}
  cache
  (atom {}))

(defn policy-stale?
  [entry]
  (when-let [^java.util.concurrent.atomic.AtomicLong epoch (:policy-epoch entry)]
    (< (.get epoch) (long @python-exec/policy-reload-epoch))))

(defn dispose-reloaded-sandbox!
  "Close an idle, reload-stale sandbox without rebuilding it. The cached env and
   its lock remain until the next turn rebuilds the policy. Recheck ownership
   under the lock so a displaced entry cannot close another turn's sandbox."
  [k]
  (let [^java.util.concurrent.locks.ReentrantLock lock (:lock (get @cache k))]
    (when (and lock (not (.isHeldByCurrentThread lock)) (.tryLock lock))
      (try (let [cur (get @cache k)]
             (when (and (identical? lock (:lock cur)) (policy-stale? cur))
               (env/dispose-sandbox! (:environment cur))))
           (catch Throwable t
             (tel/log! :error ["gateway: reload failed to dispose sandbox" (str k) (ex-message t)]))
           (finally (.unlock lock))))))

(defn mark-policy-reload!
  "Invalidate every cached env's policy and close idle Python workers now.
   Busy sessions close their stale sandbox after the current turn releases its
   lock. The next turn rebuilds the environment from the reloaded configuration."
  []
  (swap! python-exec/policy-reload-epoch inc)
  (doseq [k (keys @cache)]
    (dispose-reloaded-sandbox! k))
  nil)

;; Run after config reload; keep the Var so namespace reloads retain live wiring.
(defonce ^:private _policy-reload-hook
  (extension/register-reload-hook! ::security-policy-reload #'mark-policy-reload!))

(defn cache-key
  "Normalize an id-shaped value (UUID or string-UUID) to a UUID
   suitable for keying `cache`. Nil → nil so wrapped lookups stay
   honest."
  [id]
  (codec/->uuid id))

;; Idle-env reaper — authoritative backstop against unbounded Python worker
;; growth. Every cached session env pins one process (see `dispose-environment!`);
;; the cache itself is never bounded and the tab-close release path (TUI → gateway
;; `/release`) is best-effort and skips busy / still-open / stale-registry sessions,
;; so workers leaked whenever that path missed. A background daemon thread sweeps
;; on an interval and disposes envs that have gone idle past a TTL — guarded by
;; each entry's `ReentrantLock` (a running turn holds it, so `tryLock` failing means
;; "busy, skip") so an eval is never killed mid-flight. Evicting a resident env is
;; SAFE: the transcript lives in the DB and `ensure-env!` transparently rebuilds
;; the session on the next touch.

(def ^:private env-idle-ttl-ms
  "Idle window before a cached session env's Python session is disposed by the
   background reaper. Override with `VIS_ENV_IDLE_TTL_MS`; <= 0 disables the TTL
   sweep. Default 3 min.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_IDLE_TTL_MS")
                     str/trim
                     parse-long)
             (* 3 60 1000))))

(def ^:private env-cache-max
  "Soft cap on resident session envs. After the TTL sweep, if the cache still
   exceeds this the reaper force-evicts the least-recently-active idle entries
   (still lock-guarded) until back under the cap. Override with
   `VIS_ENV_CACHE_MAX`; <= 0 disables it. Default 8.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_CACHE_MAX")
                     str/trim
                     parse-long)
             8)))

(def ^:private env-reaper-interval-ms
  "How often the idle-env reaper wakes to sweep. Override with
   `VIS_ENV_REAPER_INTERVAL_MS`. Default 60 s.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_REAPER_INTERVAL_MS")
                     str/trim
                     parse-long)
             (* 60 1000))))

(def env-max-turns-per-ctx
  "Turns a single session's Python worker serves before the reaper recycles it
   between turns. Override with `VIS_ENV_MAX_TURNS_PER_CTX`; <= 0 disables.
   Default 5.

   This bounds the ephemeral working set of a session that never stays idle long
   enough for the TTL/RSS reaper. A recycle now replaces that session's whole
   worker process — the startup cost is real, but so is releasing every imported
   native library. `persist-session-defs!` carries module aliases, scalar
   constants and function sources across; rebuildable data is deliberately lost.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer."
  (delay (or (some-> (System/getenv "VIS_ENV_MAX_TURNS_PER_CTX")
                     str/trim
                     parse-long)
             5)))

(def ^:private env-rss-budget-mb
  "Resident-set ceiling in MB. JVM heap alone misses each interpreter worker's
   native allocations, so this gate samples the gateway plus every live Python
   worker and forces idle-env eviction when their aggregate RSS is high. Override
   with `VIS_ENV_RSS_BUDGET_MB`; <= 0 disables.

   RUNTIME-DEPENDENT, because the two runtimes do not carry the same floor. The
   native image keeps 3072; the JVM gets 5120, because a `-Xmx5g` gateway sits
   ABOVE 3 GB resident as a matter of course — heap committed plus metaspace plus
   CPython native is already past it before any session is busy. A gate below the
   idle floor is not a gate: measured on a working gateway it read `pressure=true`
   on every single reaper sweep for hours, which drives `effective-ttl` to 0 and
   force-evicts every idle env on every sweep. The reaper was permanently in its
   emergency mode, and the flag carried no information because it never varied.

   A `delay`, never an eager read: `native-image` initializes this namespace at
   BUILD time, so a top-level `getenv` would ship the BUILDER's answer — and the
   runtime split below would answer for the BUILDER's runtime, not this one."
  (delay (or (some-> (System/getenv "VIS_ENV_RSS_BUDGET_MB")
                     str/trim
                     parse-long)
             (if (util/native-image?) 3072 5120))))

(defn- runtime-pids
  "The gateway and every live Python worker it owns, once each."
  []
  (distinct (cons (.pid (java.lang.ProcessHandle/current)) (env/python-worker-pids))))

(defn- proc-rss-bytes
  [pid]
  (let [status-path (java.nio.file.Path/of (str "/proc/" pid "/status") (make-array String 0))]
    (if (java.nio.file.Files/isRegularFile status-path (make-array java.nio.file.LinkOption 0))
      (let [status (java.nio.file.Files/readString status-path)
            kb (some-> (re-find #"(?m)^VmRSS:\s+(\d+)\s+kB" status)
                       second
                       parse-long)]

        (* (long (or kb 0)) 1024))
      0)))

(defn- process-rss-bytes
  "Best-effort resident set of the gateway PLUS its Python worker processes.
   Reads procfs on Linux and one `ps` sample on macOS/other Unix hosts. Returns
   0 when unavailable; never throws."
  []
  (try (let [pids
             (vec (runtime-pids))

             procfs?
             (java.nio.file.Files/isRegularFile (java.nio.file.Path/of "/proc/self/status"
                                                                       (make-array String 0))
                                                (make-array java.nio.file.LinkOption 0))]

         (if procfs?
           (reduce (fn [total pid]
                     (+ (long total) (long (proc-rss-bytes pid))))
                   0
                   pids)
           (let [process (.exec (Runtime/getRuntime)
                                ^"[Ljava.lang.String;"
                                (into-array String ["ps" "-o" "rss=" "-p" (str/join "," pids)]))]
             (try (if (and (.waitFor process 2 java.util.concurrent.TimeUnit/SECONDS)
                           (zero? (.exitValue process)))
                    (->> (slurp (.getInputStream process))
                         str/split-lines
                         (keep (fn [line]
                                 (some-> line
                                         str/trim
                                         parse-long)))
                         (reduce + 0)
                         long
                         (* 1024))
                    0)
                  (finally (.destroy process))))))
       ;; Same shape as the shell's usage sampler: `ps` is best-effort, the
       ;; cancel that interrupted it is not.
       (catch Throwable t (cancellation/preserve-interrupt! t) 0)))

(defn- heap-used-pct
  "Current JVM heap utilization as an integer percent of the max heap
   (used = total - free). 0 when the max heap is unknown."
  []
  (let [rt
        (Runtime/getRuntime)

        mx
        (.maxMemory rt)]

    (if (pos? mx) (long (/ (* 100 (- (.totalMemory rt) (.freeMemory rt))) mx)) 0)))

(defn- memory-pressure?
  "True when gateway-plus-worker RSS crosses [[env-rss-budget-mb]].

   RSS is the truthful gauge here: a session's native Python heap lives in its
   worker process, outside JVM heap accounting. Evicting an idle env kills that
   worker, whereas a JVM heap gate would fire on unrelated work and miss Python
   growth. Accepts a sampled RSS value to avoid duplicate process calls during
   metrics and reaper sweeps."
  ([] (memory-pressure? (process-rss-bytes)))
  ([rss-bytes]
   (and (pos? (long @env-rss-budget-mb))
        (>= (long rss-bytes) (* (long @env-rss-budget-mb) 1024 1024)))))

(defn- cpu-load-pct
  "Whole-process CPU load as a percent (0–100; -1 when the JVM can't sample the
   interval yet). Uses com.sun's OperatingSystemMXBean when present; never throws."
  ^long []
  (let [os (java.lang.management.ManagementFactory/getOperatingSystemMXBean)]
    (if (instance? com.sun.management.OperatingSystemMXBean os)
      (let [v (.getProcessCpuLoad ^com.sun.management.OperatingSystemMXBean os)]
        (if (>= v 0.0) (Math/round (* v 100.0)) -1))
      -1)))

(defn gateway-runtime-metrics
  "Bounded process/runtime gauges for the gateway metrics endpoint. Values are
   sampled on demand; no profiler or background allocation is required."
  []
  (let [rt
        (Runtime/getRuntime)

        heap-used
        (- (.totalMemory rt) (.freeMemory rt))

        rss
        (process-rss-bytes)

        gc-beans
        (java.lang.management.ManagementFactory/getGarbageCollectorMXBeans)

        thread-bean
        (java.lang.management.ManagementFactory/getThreadMXBean)]

    {:jvm-heap-used-bytes heap-used
     :jvm-heap-committed-bytes (.totalMemory rt)
     :jvm-heap-max-bytes (.maxMemory rt)
     :process-rss-bytes rss
     :jvm-gc-count-total (reduce (fn [^long n bean]
                                   (let [v (.getCollectionCount
                                             ^java.lang.management.GarbageCollectorMXBean bean)]
                                     (+ n (long (max 0 v)))))
                                 (long 0)
                                 gc-beans)
     :jvm-gc-time-ms-total (reduce (fn [^long n bean]
                                     (let [v (.getCollectionTime
                                               ^java.lang.management.GarbageCollectorMXBean bean)]
                                       (+ n (long (max 0 v)))))
                                   (long 0)
                                   gc-beans)
     :jvm-thread-count (.getThreadCount thread-bean)
     :env-cache-size (count @cache)
     :env-memory-pressure (memory-pressure? rss)}))

(defn- mem-log-enabled?
  "Master switch for memory-observability logging, shared conceptually with the
   per-block heap sample in `internal.python.env`. Enabled unless VIS_MEM_LOG is a
   falsey token (0/false/off/no) — one flag silences the reaper sweep summary."
  []
  (let [raw (some-> (System/getenv "VIS_MEM_LOG")
                    str/trim
                    str/lower-case)]
    (not (contains? #{"0" "false" "off" "no"} raw))))

(defn- new-cache-entry
  "Build a cache entry wrapping `env`: the environment, its per-session
   `ReentrantLock` (one turn at a time), an `AtomicLong` `:last-active`
   epoch-ms stamp the reaper reads to decide idleness, and the `:condemned`
   flag `condemn-env!` raises when this entry's turn was declared over by a
   thread that never came back."
  [env]
  {:environment env
   :lock (java.util.concurrent.locks.ReentrantLock.)
   :condemned (java.util.concurrent.atomic.AtomicBoolean. false)
   :last-active (java.util.concurrent.atomic.AtomicLong. (util/now-ms))
   :turns (java.util.concurrent.atomic.AtomicLong. 0)
   ;; The `/reload` epoch this env was built under. `send!` recycles the entry
   ;; when a later `/reload` has bumped `policy-reload-epoch` past this stamp.
   :policy-epoch (java.util.concurrent.atomic.AtomicLong. (long @python-exec/policy-reload-epoch))})

(defn touch-entry!
  "Bump `entry`'s `:last-active` stamp to now so the reaper treats it as warm.
   Returns `entry` for threading."
  [entry]
  (when-let [^java.util.concurrent.atomic.AtomicLong la (:last-active entry)]
    (.set la (util/now-ms)))
  entry)

(defn bump-turns!
  "Increment `entry`'s per-context turn counter and return the new count (0 when
   the entry carries no counter). Read by `send!` to decide a Layer-2 recycle."
  [entry]
  (if-let [^java.util.concurrent.atomic.AtomicLong t (:turns entry)]
    (.incrementAndGet t)
    0))

(defn- evict-if-idle!
  "Dispose + `dissoc` cache entry `k` when its lock is free (no turn running)
   AND it has been idle at least `min-idle-ms` (0 = force). Lock-guarded and
   re-checked under the lock, so it never races a live turn or a concurrent
   `close!`. Returns true iff it evicted."
  [k min-idle-ms]
  (let [entry
        (get @cache k)

        ^java.util.concurrent.locks.ReentrantLock lock
        (:lock entry)]

    (boolean (when (and entry lock (.tryLock lock))
               (try (let [cur
                          (get @cache k)

                          ^java.util.concurrent.atomic.AtomicLong la
                          (:last-active cur)

                          idle
                          (if la (- (util/now-ms) (.get la)) 0)]

                      (when (and cur (>= (long idle) (long min-idle-ms)))
                        (try (dispose-environment! (:environment cur)) (catch Throwable _ nil))
                        (swap! cache dissoc k)
                        true))
                    (finally (.unlock lock)))))))

(defn reap-idle-envs!
  "One reaper sweep: dispose + evict cached session envs idle past
   `env-idle-ttl-ms` (or, under memory pressure past `env-rss-budget-mb`,
   EVERY idle env this sweep — TTL ignored), then — if the cache still exceeds
   `env-cache-max` — force-evict the least-recently-active idle entries until
   back under the cap. Every eviction is lock-guarded (a running turn is
   skipped). Returns the number of entries evicted. Safe to call directly
   (tests / manual sweeps)."
  []
  (let [now
        (util/now-ms)

        age
        (fn [entry]
          (if-let [^java.util.concurrent.atomic.AtomicLong la (:last-active entry)]
            (- now (.get la))
            0))

        rss-bytes
        (process-rss-bytes)

        pressure?
        (memory-pressure? rss-bytes)

        effective-ttl
        (if pressure? 0 (long @env-idle-ttl-ms))

        ttl-evicted
        (if (or pressure? (pos? (long @env-idle-ttl-ms)))
          (->> @cache
               (filter (fn [[_ entry]]
                         (>= (long (age entry)) (long effective-ttl))))
               (reduce (fn [n [k _]]
                         (if (evict-if-idle! k effective-ttl) (inc (long n)) n))
                       0))
          0)

        lru-evicted
        (if (pos? (long @env-cache-max))
          (let [snapshot
                @cache

                over
                (- (long (count snapshot)) (long @env-cache-max))]

            (if (pos? (long over))
              (->> snapshot
                   (sort-by (fn [[_ e]]
                              (age e))
                            >)
                   (take over)
                   (reduce (fn [n [k _]]
                             (if (evict-if-idle! k 0) (inc (long n)) n))
                           0))
              0))
          0)

        total
        (+ (long ttl-evicted) (long lru-evicted))

        cpu
        (cpu-load-pct)]

    (when (mem-log-enabled?)
      (tel/log!
        {:level :info
         :id ::env-reaper-sweep
         :data {:evicted total
                :ttl-evicted ttl-evicted
                :lru-evicted lru-evicted
                :heap-used-pct (heap-used-pct)
                :process-rss-bytes rss-bytes
                :cpu-proc-pct cpu
                :memory-pressure? pressure?
                :cache-size (count @cache)}}
        (format
          "env-reaper evicted=%d (ttl=%d lru=%d) heap=%d%% rss=%dMB cpu=%d%% pressure=%s cache=%d"
          (long total)
          (long ttl-evicted)
          (long lru-evicted)
          (long (heap-used-pct))
          (quot (long rss-bytes) 1048576)
          cpu
          pressure?
          (count @cache))))
    total))

(defn- reaper-loop
  "Background sweep loop: sleep the interval, sweep, repeat. Exits on interrupt;
   any sweep error is logged and swallowed so a single bad sweep never kills the
   reaper."
  []
  (loop []

    (let [continue? (try (Thread/sleep (long @env-reaper-interval-ms))
                         (reap-idle-envs!)
                         true
                         (catch InterruptedException _ false)
                         (catch Throwable t
                           (tel/log! {:level :warn :data {:error (ex-message t)}}
                                     "env-reaper sweep failed")
                           true))]
      (when continue? (recur)))))

(defonce ^:private env-reaper-thread (atom nil))

(defn- env-reaper-enabled?
  "True when the sweep interval and at least one eviction policy are enabled."
  []
  (and (pos? (long @env-reaper-interval-ms))
       (or (pos? (long @env-idle-ttl-ms))
           (pos? (long @env-cache-max))
           (pos? (long @env-rss-budget-mb)))))

(defn- ensure-env-reaper!
  "Start the idle-env reaper daemon thread once, lazily, on the first cache
   insert. Started here (not at namespace load) so a native-image build-time
   init never spawns a thread, and only when reaping is actually enabled."
  []
  (when (and (env-reaper-enabled?) (nil? @env-reaper-thread))
    (locking cache
      (when (nil? @env-reaper-thread)
        (let [t (doto (Thread. ^Runnable reaper-loop "vis-env-reaper") (.setDaemon true))]
          (reset! env-reaper-thread t)
          (.start t))))))

(defn cache-env!
  "Insert `env` into the cache under `session-id` (UUID, or string
   normalized via `cache-key`). Returns `{:id <UUID> :environment env}`."
  [session-id env]
  (let [k
        (cache-key session-id)

        ;; Whatever this insert is about to displace. Overwriting the entry used
        ;; to drop it on the floor: its Python session — the namespace, its host
        ;; doors and the native memory behind them — stayed reachable for the life
        ;; of the process, because nothing else ever disposes an env the cache no
        ;; longer points at. A gateway up 16h held 36 such stranded sessions against
        ;; 3 live ones and only 5 reaper evictions: the difference was displaced
        ;; envs.
        displaced
        (get @cache k)]

    (swap! cache assoc k (new-cache-entry env))
    ;; Lock-guarded like `evict-if-idle!`, and only for a DIFFERENT env: a turn
    ;; running on the displaced env must finish on it rather than have its
    ;; interpreter closed underneath. A busy one is left to the reaper, which
    ;; sweeps whatever the cache still points at — this only rescues what the
    ;; cache has already forgotten.
    (when-let [old (:environment displaced)]
      (when-not (identical? old env)
        (let [^java.util.concurrent.locks.ReentrantLock lock (:lock displaced)]
          (when (and lock (.tryLock lock))
            (try (dispose-environment! old) (catch Throwable _ nil) (finally (.unlock lock)))))))
    (ensure-env-reaper!)
    {:id k :environment env}))

(defn sync-cached-extension-symbols!
  "Synchronize extension bindings in every idle cached session immediately.

   A Settings change is process-wide while each session owns a persistent
   Python context. Busy contexts retain their started tool surface and are
   synchronized at the next turn boundary. Returns the refreshed count."
  []
  (reduce-kv (fn [refreshed _ {:keys [environment lock]}]
               (if (and lock (.tryLock ^java.util.concurrent.locks.ReentrantLock lock))
                 (try (sync-active-extension-symbols! environment)
                      (unchecked-inc (long refreshed))
                      (finally (.unlock ^java.util.concurrent.locks.ReentrantLock lock)))
                 refreshed))
             0
             @cache))

;; A Settings flip must reach the TOOLS, whatever channel made it. The fan-out
;; used to sit inline in the gateway's HTTP settings handler, so a flip from the
;; TUI dialog (which calls `toggles/set-enabled!` straight) or from an extension
;; persisted to state.yml and refreshed nothing — every other cached session kept
;; its stale tool surface until a restart. The toggle registry is the ONE seam
;; every channel goes through, so the fan-out belongs on its listener.
;; `notify!` swallows listener throws, and `defonce` keeps the registration
;; idempotent across `(require ... :reload)`.
(defonce ^:private _toggle-extension-sync-listener
  (toggles/add-listener! (fn [event]
                           (when (= workspace/draft-backend-toggle-id (:id event))
                             (mark-policy-reload!))
                           (sync-cached-extension-symbols!))))

(defn- kickoff-cached-sessions
  "Run provider kickoff for each cached entry against `router`, outside any cache
   swap: extension hooks have side effects, and a contended `swap!` would repeat
   them. Answers `{:refreshed {id [environment refreshed]} :failures {id throwable}}`."
  [router entries]
  (reduce-kv (fn [acc id {:keys [environment]}]
               (try (assoc-in acc
                      [:refreshed id]
                      [environment
                       (loop-router/kickoff-session-providers (assoc environment :router router))])
                    (catch Throwable t (assoc-in acc [:failures id] t))))
             {:refreshed {} :failures {}}
             entries))

(defn- seat-refreshed-environments
  "Pure cache merge: seat each refreshed environment only where the cache still
   holds the environment it was computed from, so an evicted session stays evicted
   and a concurrently replaced one is not overwritten."
  [m refreshed]
  (reduce-kv (fn [acc id [environment refreshed-environment]]
               (if (identical? environment (get-in acc [id :environment]))
                 (assoc-in acc [id :environment] refreshed-environment)
                 acc))
             m
             refreshed))

(defn refresh-cached-routers!
  "Reseat `:router` on every cached env's environment map.

   `create-environment` snapshots the router into
   `(:router env)` at construction time, and the iteration loop calls
   `(svar/ask-code! (:router environment) ...)` - not the global
   `router-atom`. So when a frontend changes provider
   config and rebuilds the global router, every long-lived env in the
   cache (TUI keeps one for the whole session) keeps talking to the
   *previous* model until disposed.

   Provider kickoff hooks run against each session before its new snapshot is
   seated, covering providers added or reconfigured while that session is live.
   They run outside the cache swap, once per environment: a session replaced
   while its kickoff ran is kicked off again on its new environment, and an
   evicted one is dropped. A session whose kickoff fails keeps its previous
   environment while every other session moves; the failures are then thrown as
   one ex-info naming the affected session ids, with the first failure as cause.
   Call this immediately after `rebuild-router!` so the next `send!` on any cached
   session picks up the new router."
  [router]
  (when router
    (loop [entries
           @cache

           failures
           {}]

      (let [{:keys [refreshed] :as kicked}
            (kickoff-cached-sessions router entries)

            failures
            (merge failures (:failures kicked))

            seated
            (swap! cache seat-refreshed-environments refreshed)

            replaced
            (into {}
                  (keep (fn [[id [_ refreshed-environment]]]
                          (when-let [entry (get seated id)]
                            (when-not (identical? refreshed-environment (:environment entry))
                              [id entry]))))
                  refreshed)]

        (cond (seq replaced) (recur replaced failures)
              (seq failures) (throw (ex-info
                                      (str "Provider kickoff failed for " (count failures)
                                           " cached session(s), which keep their previous router: "
                                           (str/join ", " (keys failures)))
                                      {:type :vis/provider-kickoff-failed
                                       :session-ids (vec (keys failures))
                                       :errors (update-vals failures #(or (ex-message %) (str %)))}
                                      (val (first failures))))))))
  nil)

(defn reload-router!
  "Rebuild the shared LLM router from the freshly reloaded config and reseat it
   on every cached env. Registered as a `/reload` hook.

   `reload-slash` re-reads vis.yml through `config/reload-config!`, but the
   router is an immutable SNAPSHOT: built once by `get-router` and captured
   again inside every long-lived session env (`(:router environment)`). Without
   this hook a `default_model` / provider edit only took effect after a full
   restart — the engine kept routing turns through the previous router, and
   every frontend that names the router default (the TUI footer model chip via
   `resolve-effective-model`) kept showing the OLD model.

   No-ops when the router was never built, so lazy first use is preserved: a
   `/reload` must not force OAuth token fetches at TUI boot. Returns nil."
  []
  (when (loop-router/router-initialized?)
    (refresh-cached-routers! (loop-router/rebuild-router! (config/current-config))))
  nil)

;; Wire `reload-router!` into the `/reload` slash. `run-reload-hooks!` runs
;; AFTER `config/reload-config!`, so the rebuild always sees the new config.
;; `defonce` keeps the registration idempotent across `(require ... :reload)`.
(defonce ^:private _router-reload-hook
  (extension/register-reload-hook! ::router-reload reload-router!))

;; Keep live session envs in sync with Python-extension (re)loads. Each env
;; caches its own `:extensions` rows — slash dispatch (`active-slashes env`)
;; and sandbox bindings read those, NOT the global registry — so a `/reload`
;; that swaps the registry must also reseat every cached env. Otherwise a
;; newly added extension stays invisible to running sessions and stale rows
;; keep calling into a disposed Python session, whose doors went with it.
;; Same propagation pattern as `refresh-cached-routers!`.

(defn set-provider!
  "Set the single active provider config. Persists to disk, updates
   in-memory state, rebuilds the global router, and reseats cached
   session envs. `provider` is a svar-native provider map
   `{:id :base-url :api-key :models [...]}`. Replaces an existing
   provider with the same `:id` or appends a new entry."
  [provider]
  (let [cfg
        (or (config/current-config) {:providers []})

        pid
        (:id provider)

        provs
        (vec (:providers cfg))

        idx
        (some (fn [[i p]]
                (when (= (:id p) pid) i))
              (map-indexed vector provs))

        updated
        (if idx (assoc provs idx provider) (conj provs provider))

        prioritized
        (vec (cons provider (remove #(= (:id %) pid) updated)))

        new-cfg
        {:providers prioritized}]

    ;; The machine store carries far more than providers — toggles, the vision
    ;; memory, the MCP servers, the selection tags. Handing `save-config!` a map
    ;; that holds ONLY `:providers` replaced the file with that one key and
    ;; silently erased the rest, so the write goes through the locked
    ;; read-modify-write and touches nothing else.
    (config/update-machine-config!
      (fn [raw]
        (assoc raw "providers" (mapv providers/persisted-provider-config prioritized)))
      :set-provider!)
    (reset! @#'config/active-config new-cfg)
    (try (let [r (loop-router/rebuild-router! new-cfg)]
           (refresh-cached-routers! r))
         (catch Exception e
           (tel/log! {:level :warn :data {:error (ex-message e)}}
                     "Failed to rebuild router after provider change")))
    new-cfg))

(defn open-env!
  ;; App session entry (create! + resume). The vis engine is the embedded
  ;; CPython Python sandbox — there is no other substrate.
  [id {:keys [channel external-id title workspace-id]}]
  (let [router
        (loop-router/get-router)

        env
        (create-environment router
                            (cond-> {:db (config/resolve-db-spec)}
                              id
                              (assoc :session id)

                              channel
                              (assoc :channel channel)

                              external-id
                              (assoc :external-id external-id)

                              title
                              (assoc :title title)

                              workspace-id
                              (assoc :workspace-id workspace-id)))]

    env))

(defn ensure-env!
  [id]
  (let [k (cache-key id)]
    (if-let [entry (get @cache k)]
      ;; NB: a cache HIT does NOT touch `:last-active`. Idleness must reflect
      ;; real turn activity (marked in `send!`'s finally), not passive reads:
      ;; hot render/status paths (`gateway.state/live-env`, `context-snapshot`)
      ;; resolve the env via `env-for` on every poll, and touching here reset
      ;; the idle clock each time — so a rendered-but-idle session was NEVER
      ;; reaped (its Python session stayed resident indefinitely).
      entry
      (let [env (open-env! k {})]
        (swap! cache (fn [m]
                       (if (contains? m k) m (assoc m k (new-cache-entry env)))))
        (ensure-env-reaper!)
        (get @cache k)))))

(defn recycle-env!
  "Between-turns context recycle (Layer 2): rebuild a FRESH env for session `k`
   and swap it into the existing cache entry IN PLACE — REUSING the same
   `ReentrantLock` so a caller queued on the lock re-reads the fresh env — then
   dispose the OLD Python session (and its own per-env DB connection). MUST be
   called while holding the entry lock, so no turn races the swap and `old` is
   stable. The transcript lives in the DB; `open-env!` resumes it, so the model
   loses only its ephemeral Python globals — the point of the recycle."
  [k]
  (when-let [old (get @cache k)]
    (let [fresh-env (open-env! k {})]
      (swap! cache assoc
        k
        (assoc old
          :environment fresh-env
          :last-active (java.util.concurrent.atomic.AtomicLong. (util/now-ms))
          :turns (java.util.concurrent.atomic.AtomicLong. 0)
          ;; Restamp to the current epoch: the fresh env carries the latest
          ;; security-policy snapshot, so it is no longer reload-stale.
          :policy-epoch (java.util.concurrent.atomic.AtomicLong.
                          (long @python-exec/policy-reload-epoch))))
      ;; The recycle is the busiest teardown site there is — every N turns, for
      ;; the life of every session — so a failure here is the one that compounds.
      ;; It must not take the swap down, but it must not vanish either.
      (try (dispose-environment! (:environment old))
           (catch Throwable t
             (tel/log! :error
                       ["gateway: recycle failed to dispose the old env" (str k)
                        (ex-message t)]))))))

(def ^:private ENGINE_LOCK_POLL_MS
  "How long one attempt at a session's turn lock waits before `send!` re-reads
   the cache entry it is queueing on.

   Not a deadline: a turn legitimately owns that lock for its whole run, and a
   queued turn is supposed to wait. It is the beat at which the waiter notices
   its entry was CONDEMNED (see [[condemn-env!]]) and stops waiting for a thread
   that is never coming back."
  250)

(defn- detach-entry!
  "Drop `entry` from the cache, but only while it is still `k`'s entry.

   The abandoned turn may still own the lock, so full environment disposal remains
   unsafe. Its Python interpreter is now a separate process, however: retire that
   process without entering it as soon as cache ownership moves on. The stale host
   thread may linger, but its Python heap and worker capacity do not."
  [k entry]
  (loop []

    (let [m @cache]
      (cond (not (identical? entry (get m k))) false
            (compare-and-set! cache m (dissoc m k))
            (do (let [environment (:environment entry)]
                  (python-exec/retire-python-context-once! (env/python-context-if-built environment)
                                                           environment
                                                           :environment-detached
                                                           nil))
                true)
            :else (recur)))))

(defn condemn-env!
  "Mark session `id`'s engine entry CONDEMNED: the daemon has already declared
   this session's turn over, but the thread that ran it never came back, so it
   may be holding the entry's `ReentrantLock` forever.

   The mark is a fact the NEXT turn reads: [[acquire-turn-lock!]] abandons a
   condemned entry instead of queueing behind a dead lock. A worker that does
   thaw clears the mark simply by taking the lock normally, so a backstop that
   fired early costs nothing. Returns true when an entry was marked."
  [id]
  (boolean (when-let [^java.util.concurrent.atomic.AtomicBoolean flag
                      (:condemned (get @cache (cache-key id)))]
             (.set flag true)
             true)))

(defn acquire-turn-lock!
  "Take session `id`'s one-turn-at-a-time lock and return the entry that owns
   it, WITHOUT ever parking on it forever.

   This used to be a bare `.lock`. A turn wedged inside the engine — parked on
   CPython's GIL, where `Thread.interrupt` cannot reach it — never unlocks, so
   every later turn for that session parked in `Unsafe.park`: `turn.started` on
   the wire, not one event after it, and deaf to its own cancel, for the life of
   the daemon. Meanwhile the cancel backstop had already synthesized
   `turn.cancelled` and the daemon reported the session idle.

   So the wait is a POLL. Each round re-reads the session's CURRENT entry, and a
   CONDEMNED one is detached and rebuilt rather than waited on; `tryLock` is
   interruptible, so a queued turn's own cancel finally reaches it.

   A FREE lock is not enough. A session whose context was disposed — a teardown,
   a recycle, an environment that failed halfway through being built — cannot be
   entered again, and `env-python/context-enterable?` is what says so. A normal
   cancel leaves the session standing; only a worker that fails to acknowledge or
   unwind the interrupt is retired, and that process has already been killed.

   Nothing is disposed here because parent-side host work may still hold the old
   environment. The expensive interpreter process is already reclaimed on the
   retirement path. Rescue happens at most once per acquisition — a fresh context
   that still refuses is a real turn failure, not a reason to keep minting workers."
  [id]
  (let [k (cache-key id)]
    (loop [rescued? false]
      (let [{:keys [^java.util.concurrent.locks.ReentrantLock lock
                    ^java.util.concurrent.atomic.AtomicBoolean condemned]
             :as entry}
            (ensure-env! id)]
        (if (.tryLock lock (long ENGINE_LOCK_POLL_MS) java.util.concurrent.TimeUnit/MILLISECONDS)
          (if (try (or rescued? (policy-stale? entry) (env/context-enterable? (:environment entry)))
                   ;; The bounded guest readiness check is interruptible. Its caller
                   ;; already owns this lock, even though send! has not received it yet.
                   (catch Throwable error (.unlock lock) (throw error)))
            (do (when condemned (.set condemned false)) entry)
            (do
              (.unlock lock)
              (detach-entry! k entry)
              (tel/log!
                {:level :warn :id ::engine-context-unsafe :data {:session k}}
                "Session Python context is retired or cannot be entered; starting a fresh context")
              (recur true)))
          (do
            (when (and condemned (.get condemned) (detach-entry! k entry))
              (tel/log!
                {:level :warn :id ::engine-abandoned :data {:session k}}
                "Session engine abandoned: its turn was declared over but its thread never returned - starting a fresh context"))
            (recur rescued?)))))))
