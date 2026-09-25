(ns com.blockether.vis.internal.main
  "vis-agent CLI binary - :db Telemere handler, one-shot agent helper,
   built-in CLI commands, and the `-main` dispatcher entry point.

   Everything in this file is binary-only. The library surface lives in
   the `com.blockether.vis.internal.loop` namespaces: the session cache in
   `loop`, the turn engine in `loop.turn`, the iteration loop in
   `loop.iteration` and the environment lifecycle in `loop.environment`.
   This namespace wires them into the command tree the `vis-agent` wrapper
   exposes.

   Public entry point:

     (-main & args)   - invoked by the `:vis` alias / `bin/vis-agent`.
                       Configures logging, discovers Clojure extensions, loads Python
                       extensions before one-shot dispatches, redirects stderr to this
                       process's role/start-time/pid-stamped file under `~/.vis/logs/` for
                       any TTY-owning channel, then dispatches to the resolved
                       command's `:cmd/run-fn`.

   Built-in commands registered here:
     vis-agent extension list     - list registered extensions
     vis-agent channels <name>    - auto-mounted via the channel registry

   Domain namespaces own the other command groups. Each exports `command`
   and, when its children register through the registry, `subcommands`:
   `gateway.cli`, `foundation.mcp.cli`, `provider.cli`, `session.cli`,
   `workspace.cli`, `speech.cli` and `decisions.cli`.

   `vis-agent doctor` is host-owned. Extensions plug diagnostics into it
   with `:ext/doctor-fn`; extension-owned CLI commands stay under
   `vis-agent extension`."
  (:refer-clojure :exclude [agent run!])
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.doctor :as doctor]
            [com.blockether.vis.internal.decisions.cli :as decisions-cli]
            [com.blockether.vis.internal.speech.cli :as speech-cli]
            [com.blockether.vis.internal.foundation.mcp.cli :as mcp-cli]
            [com.blockether.vis.internal.gateway.cli :as gateway-cli]
            [com.blockether.vis.internal.provider.cli :as provider-cli]
            [com.blockether.vis.internal.session.cli :as session-cli]
            [com.blockether.vis.internal.workspace.cli :as workspace-cli]
            [com.blockether.vis.internal.foundation.housekeeping :as housekeeping]
            [com.blockether.vis-python-runtime :as pyrt]
            [com.blockether.vis.internal.python.env :as env]
            [com.blockether.vis.internal.python.host :as python-host]
            [com.blockether.vis.internal.python.runtime :as python-runtime]
            [com.blockether.vis.internal.error :as error]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.channel.form :as form]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.server :as gateway-server]
            [com.blockether.vis.internal.gateway.stdio :as gateway-stdio]
            [com.blockether.vis.internal.gateway.state :as gateway-state]
            [com.blockether.vis.internal.extension.manifest :as manifest]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python.project :as pyproj]
            [com.blockether.vis.internal.session.progress :as progress]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.system-trust :as system-trust]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [taoensso.telemere :as tel]))

;; Persistence-backed Telemere :db handler

;; Signal -> log entry

(defn- signal->entry
  "Transform a telemere signal into the entry map accepted by the
   persistence facade's `db-log!`. The facade fills in `:id`/`:created_at`
   and converts ids/keywords through `persistance.base`, so this fn
   only carries the semantic payload."
  [signal]
  (let [ctx
        (or (:ctx signal) {})

        level
        (or (:level signal) :info)

        event
        (or (some-> (:id signal)
                    str)
            (some-> (:ns signal)
                    str)
            "unknown")

        data
        ;; Persisted payload boundary: the log table's `data` column is JSON —
        ;; encode via the canonical wire (snake_case STRING keys, `foo?` -> is_foo).
        (try (wire/json-str (cond-> {}
                              (:msg_ signal)
                              (assoc :msg (force (:msg_ signal)))

                              (:data signal)
                              (assoc :data (:data signal))

                              (:ns signal)
                              (assoc :ns (str (:ns signal)))

                              (:error signal)
                              (assoc :error (str (:error signal)))))
             (catch Throwable _ nil))]

    (cond-> {:level level :event event :data data}
      (:session-soul-id ctx)
      (assoc :session-soul-id (:session-soul-id ctx))

      (:session-turn-id ctx)
      (assoc :session-turn-soul-id (:session-turn-id ctx))

      (:iteration-id ctx)
      (assoc :iteration-id (:iteration-id ctx)))))

(defn handler:db
  "Telemere handler that persists every signal through the
   `com.blockether.vis.core/log!` facade.

   The handler reads `:db-info` from the signal's telemere context
   (`*ctx*`). When `:db-info` is absent (no DB connection active in
   scope), the signal is silently dropped - the console handler still
   prints it.

   Usage:
     (tel/add-handler! :db (handler:db))

     (tel/with-ctx+ {:db-info db-info :session-soul-id session-id}
       (tel/log! :info \"something happened\"))"
  ([] (handler:db nil))
  ([_opts]
   (fn handler ([signal] (when-let [db-info (get-in signal [:ctx :db-info])]
                           (try (persistance/db-log! db-info (signal->entry signal))
                                (catch Throwable _ nil)))) ([] nil))))

(defn setup-db-handler!
  "Install the `:db` Telemere handler. Idempotent - reusing the same
   handler key replaces the previous registration. Call once at
   process startup, after the persistence backend is loaded
   (otherwise the handler will silently drop signals because no
   backend is registered with the facade yet).

   The handler is asynchronous (dropping mode, 2048-entry buffer,
   single drain thread) so a slow DB write never back-pressures the
   call site that emitted the signal."
  []
  (tel/add-handler! :db
                    (handler:db)
                    {:async {:mode :dropping :buffer-size 2048 :n-threads 1} :min-level :info}))

;; Extension CLI dispatcher

;;; ── Extension introspection ─────────────────────────────────────────────

(def ^:private ^String ext-ns-prefix "com.blockether.vis.internal.")

(defn- short-ext-ns
  "Render an extension namespace symbol with the `v/` prefix instead of
   the canonical `com.blockether.vis.internal.` package, so the table column
   stays narrow:

     com.blockether.vis.internal.foundation.core         -> v/foundation.core
     com.blockether.vis.internal.provider.vendor.github-copilot -> v/provider.github-copilot

   Anything that doesn't start with the canonical prefix is returned
   unchanged."
  [ns-sym]
  (let [s (str ns-sym)]
    (if (str/starts-with? s ext-ns-prefix) (str "v/" (subs s (count ext-ns-prefix))) s)))

(defn- per-kind-group
  "Per-row \"Group\" cell - a finer label *inside* `:ext/kind`. Pulled
   from the extension's contribution slot that matches its kind:

     - providers   -> joined `:provider/label`s
     - channels    -> joined `:channel/cmd`s
     - everything else (foundation, languages, uncategorized) -> blank

   Joined with `, ` so an extension contributing multiple of one
   surface (e.g. `provider-zai` exporting both Coding-Plan and
   pay-as-you-go) reads as a single comma-separated cell instead of
   a wrapped multi-line column."
  [e]
  (case (:ext/kind e)
    "providers"
    (->> (:ext/providers e)
         (keep :provider/label)
         (str/join ", "))

    "channels"
    (->> (:ext/channels e)
         (keep :channel/cmd)
         (str/join ", "))

    ""))

(defn list-extensions
  "Return all registered extensions with their metadata (table rows).

   Managed GitHub packages show their lowercase owner/repository and keep the
   technical package name in parentheses. Other namespaces use the `v/` prefix
   (see `short-ext-ns`). `:kind` groups rows; `:group` labels a contribution inside
   that kind. Managed packages derive `:owner` from the recorded GitHub source.
   Other owner, author and license values come from the extension manifest;
   bundled extensions use the distribution name (\"vis\") as their owner."
  []
  (mapv (fn [e]
          {:namespace (if-let [repository (:ext/repository e)]
                        (str repository " (" (:ext/name e) ")")
                        (short-ext-ns (:ext/name e)))
           :doc (:ext/description e)
           :kind (or (:ext/kind e) "uncategorized")
           :group (per-kind-group e)
           :author (or (:ext/author e) "-")
           :owner (or (some-> (:ext/repository e)
                              (str/split #"/")
                              first)
                      (:ext/owner e)
                      "-")
           :license (or (:ext/license e) "-")
           :version (or (:ext/version e) "-")})
        (extension/registered-extensions)))

;;; ── Arg parsing & validation ───────────────────────────────────────────

;;; ── Help rendering ─────────────────────────────────────────────────────

;;; ── Dispatch ───────────────────────────────────────────────────────────

;; Agent helper (root one-shot run)

;;; ── Agent Definition ─────────────────────────────────────────────────────

(defn agent
  "Create an agent definition (data map).

   Options:
   - :name        - Agent name (string, default \"default\")
   - :description - What the agent does
   - :constants   - Map of {symbol value} constants for the Python sandbox
   - :model       - Override default model selection

   The iteration loop runs until the model emits `:answer` or the
   user cancels.

   Example:
     (agent {:name \"code-reviewer\"
             :description \"Reviews Clojure code for quality\"
             :model \"gpt-4o\"})"
  [{:keys [name] :as opts}]
  (let [agent-name (or name "default")]
    (merge {:name agent-name :constants {}} opts)))

;;; ── Execution ────────────────────────────────────────────────────────────

(defn- split-provider-model
  "Return `[provider-id model-name]` for `provider/model`; nil for bare model names."
  [model]
  (when-let [model* (some-> model
                            str
                            str/trim
                            not-empty)]
    (when-let [idx (str/index-of model* "/")]
      (let [provider-name (subs model* 0 idx)
            model-name (subs model* (inc (long idx)))]

        (when (and (not (str/blank? provider-name)) (not (str/blank? model-name)))
          [(keyword provider-name) model-name])))))

(defn- select-model
  [provider model-name]
  (let [model-name*
        (str model-name)

        existing
        (some #(when (= (str/lower-case model-name*)
                        (some-> (config/model-name %)
                                str/lower-case))
                 %)
              (:models provider))

        selected
        (if (map? existing) (assoc existing :name model-name*) {:name model-name*})]

    (assoc provider
      :models (vec (cons selected
                         (remove #(= (str/lower-case model-name*)
                                     (some-> (config/model-name %)
                                             str/lower-case))
                           (:models provider)))))))

(defn- provider-from-template
  [provider-id]
  (when-let [template (config/provider-template provider-id)]
    (select-keys template [:id :base-url :api-style :llm-headers :responses-path])))

(defn- provider-with-model
  "Return the configured provider whose catalog lists `model-name` verbatim.

   Model ids may contain a slash (`z-ai/glm-4.6v`), so a whole-name catalog hit
   wins over reading the prefix as a provider tag."
  [providers model-name]
  (let [wanted (str/lower-case (str model-name))]
    (some (fn [provider]
            (when (some #(= wanted
                            (some-> (config/model-name %)
                                    str
                                    str/lower-case))
                        (:models provider))
              provider))
          providers)))

(defn- config-with-provider-override
  "Return config with `provider-id` promoted to the active (first) position.
   Resolves from configured providers first, falling back to provider templates.
   Throws if the provider is unknown."
  [config provider-id]
  (let [providers
        (vec (:providers config))

        provider
        (or (some #(when (= provider-id (:id %)) %) providers)
            (provider-from-template provider-id))]

    (if-not provider
      (throw (ex-info (str "Unknown provider: " (name provider-id))
                      {:type :vis.cli/unknown-provider :vis/user-error true :provider provider-id}))
      (assoc config :providers (vec (cons provider (remove #(= provider-id (:id %)) providers)))))))

(defn- config-with-model-override
  "Return config with `model` selected first.

   Bare model names select that model on the active provider. Provider-qualified
   names (`provider/model`) move or synthesize that provider as the one-shot
   root provider. This does not persist to `~/.vis/config.edn`."
  [config model]
  (if-let [model* (some-> model
                          str
                          str/trim
                          not-empty)]
    (let [providers (vec (:providers config))]
      (if-let [[provider-id model-name] (if-let [owner (when (str/includes? model* "/")
                                                         (provider-with-model providers model*))]
                                          [(:id owner) model*]
                                          (split-provider-model model*))]
        (let [provider (or (some #(when (= provider-id (:id %)) %) providers)
                           (provider-from-template provider-id)
                           (throw (ex-info (str "Unknown provider in --model: " (name provider-id))
                                           {:type :vis.cli/unknown-model-provider
                                            :vis/user-error true
                                            :provider provider-id
                                            :model model*})))
              selected (select-model provider model-name)]

          (assoc config
            :providers (vec (cons selected (remove #(= provider-id (:id %)) providers)))))
        (update config
                :providers
                (fn [providers]
                  (if-let [active (first providers)]
                    (vec (cons (select-model active model*) (rest providers)))
                    providers)))))
    config))

(defn- router-for-run
  [config use-local-router?]
  (if use-local-router?
    ;; Use the same provider enrichment and network-policy boundary as the
    ;; shared router. LM Studio needs this to replace svar's conservative
    ;; context fallback with the window reported by its native model endpoint.
    (loop-router/build-router config)
    (loop-router/get-router)))

(defn- run-error-result
  [session-id e]
  (let [data
        (ex-data e)

        unsupported?
        (= :vis/unsupported-reasoning-effort (:type data))]

    (cond-> {:session-id session-id
             :error (persistance/db-error->user-message e)
             :type (str (type e))
             :exception e}
      unsupported?
      (assoc :eval
        {:valid? false
         :invalid-reasons [{:type :unsupported-reasoning-effort
                            :requested (:requested data)
                            :provider (some-> (:provider data)
                                              name)
                            :model (:model data)
                            :supported (vec (:supported data))}]
         :reasoning-effort {:requested (:requested data) :iterations []}}))))

(defn run!
  "Execute a one-shot agent turn.

   Runs one turn. Default is ephemeral: in-memory SQLite only, no
   `:cli` session written to disk.

   Returns map with:
   - :session-id - Session ID (UUID string) when persisted;
                        nil for default ephemeral runs
   - :answer       - The agent's response
   - :iteration-count - Number of iterations executed
   - :duration-ms  - Total wall-clock time
   - :tokens       - {:input N :output N :reasoning N :cached N :total N}
   - :cost         - {\"input_cost\" N \"output_cost\" N \"total_cost\" N \"model\" str}
   - :trace        - Full iteration trace
   - :confidence   - :high/:medium/:low (when present)
   - :status - Only on failure (`:error` or `:cancelled`).
   - :error  - Error message (only on failure).

   Options:
   - :spec        - Output spec for structured responses
   - :provider    - Override provider (keyword or string, e.g. :openai)
   - :model       - Override model
   - :reasoning-effort - Exact provider-native effort (for example, `low`, `high` or `max`)
   - :on-chunk    - Streaming callback fn
   - :debug?      - Enable debug logging (default false)
   - :config      - Provider config override (skips ~/.vis/config.edn)
   - :db          - DB target for ephemeral runs (`:memory`, path, or db spec)
   - :persist?    - Write the run to ~/.vis/vis.mdb as a `:cli`
                    session. Default false.
   - :session-id  - Continue an existing persisted session (full UUID or
                    unambiguous prefix). Implies persistent execution.
   - :no-persist? - Backward-compatible override; when true, forces
                    ephemeral execution even if `:persist?` is true.

   Ephemeral runs use an in-memory SQLite DB (`:db :memory`), run the
   turn, then dispose the env (which vaporizes the DB). Result has
   `:session-id nil`. Useful for CI, scripting, sensitive prompts.

   Persistent calls (`:persist? true`) create a fresh session in
   the `:cli` channel. Past runs are browsable via
   `(sessions/by-channel :cli)`."
  [agent-def prompt &
   [{:keys [spec model provider reasoning-effort on-chunk debug? config db persist? no-persist?
            session-id]
     :as _opts}]]
  (let [mdl
        (or model (:model agent-def))

        cfg-base
        (config/resolve-config config)

        cfg
        (cond-> cfg-base
          provider
          (config-with-provider-override (keyword provider))

          mdl
          (config-with-model-override mdl))

        requested-provider
        (when (or provider (split-provider-model mdl)) (:id (first (:providers cfg))))

        requested-model
        (when mdl
          (or (some-> cfg
                      :providers
                      first
                      :models
                      first
                      config/model-name)
              mdl))

        local-router?
        (boolean (or config mdl provider))

        prompt-s
        (if (string? prompt) prompt (pr-str prompt))

        tracker
        (when on-chunk
          (progress/make-progress-tracker {:on-update (fn [_timeline chunk]
                                                        (on-chunk chunk))}))

        on-chunk*
        (when tracker (:on-chunk tracker))

        q-opts
        (cond-> {}
          spec
          (assoc :spec spec)

          requested-model
          (assoc :model requested-model)

          reasoning-effort
          (assoc :reasoning-effort reasoning-effort)

          on-chunk*
          (assoc :hooks {:on-chunk on-chunk*})

          debug?
          (assoc :debug? true))

        messages
        (if (string? prompt) [(svar/user prompt)] prompt)

        persistent?
        (and (or persist? session-id) (not no-persist?))]

    (if-not persistent?
      ;; Ephemeral path: build a fresh env on a `:memory` SQLite DB so
      ;; nothing touches `~/.vis/vis.mdb`. Disposing the env tears the
      ;; in-memory DB down with it. Bypasses `lp/create!`/`lp/send!`
      ;; (both go through the shared sessions cache + the on-disk
      ;; SQLite handle) on purpose. We use `:memory` instead of nil
      ;; because the iteration loop requires a non-nil `:db-info` (it
      ;; persists turns + iterations + expression history; nil would
      ;; reject in `prepare-turn-context`).
      ;; `:channel :cli` tags this as a NON-INTERACTIVE one-shot run (the
      ;; persistent path already creates a `:cli` session). The prompt keys
      ;; off it to drop the candidate propose-and-STOP-for-approval gate —
      ;; there is no human here to approve, so a candidate plan would stall.
      (let [env (loop-env/create-environment (router-for-run cfg local-router?)
                                             {:db (or db :memory) :channel :cli})]
        (try (let [result (turn/turn! env messages q-opts)]
               (cond-> {:session-id nil
                        :content (content/answer-content (:answer result))
                        :iteration-count (:iteration-count result)
                        :duration-ms (:duration-ms result)
                        :tokens (:tokens result)
                        :cost (:cost result)
                        :trace (:trace result)}
                 (:status result)
                 (assoc :status (:status result))

                 (:confidence result)
                 (assoc :confidence (:confidence result))

                 (:eval result)
                 (assoc :eval (:eval result))))
             (catch Exception e (run-error-result nil e))
             (finally (try (loop-env/dispose-environment! env) (catch Exception _ nil)))))
      ;; Persistent path: route through the canonical in-process gateway so
      ;; CLI, TUI, web, and transport clients share the same session/turn
      ;; machinery.
      (let [_
            (when local-router? (loop-router/rebuild-router! cfg))

            resolve-session
            (fn [input]
              (let [s (some-> input
                              str
                              str/trim)]
                (when (seq s)
                  (or (when-let [session (gateway-state/soul s)]
                        ;; A wire soul is STRING-keyed (`wire/canonical`), so `:id`
                        ;; read nil here and even a full id fell through to the
                        ;; prefix walk below.
                        (get session "id"))
                      ;; Not the navigator list: that one leaves out the sessions
                      ;; nobody has used yet (`state/session-listed?`), and a CLI
                      ;; session is created title-less and turn-less BEFORE it runs.
                      (let [matches (->> (gateway-state/session-ids)
                                         (filter #(str/starts-with? % s))
                                         distinct
                                         vec)]
                        (when (= 1 (count matches)) (first matches)))))))

            resolved-session-id
            (when session-id
              (or (resolve-session session-id)
                  (throw (ex-info (str "Session not found: " session-id)
                                  {:type :vis.cli/session-not-found
                                   :vis/user-error true
                                   :session-id session-id}))))

            created-session
            (when-not resolved-session-id
              ;; Create title-less so the async `maybe-auto-title!`
              ;; side-channel (fired during the turn, same as TUI/web)
              ;; generates a real LLM title. Passing a crude
              ;; truncated-prompt title here used to satisfy
              ;; `usable-existing-title` and SUPPRESS auto-titling,
              ;; leaving every persisted CLI session stuck on the
              ;; raw prompt text.
              (gateway-state/create-session! {:channel :cli}))

            session-id
            (or resolved-session-id (get created-session "id"))]

        (try (let [result (gateway-state/submit-turn-sync!
                            session-id
                            (cond-> {:request prompt-s :messages messages :engine-opts q-opts}
                              requested-provider
                              (assoc :provider requested-provider)

                              requested-model
                              (assoc :model requested-model)))]
               ;; The gateway result is canonical string-keyed; pick the
               ;; fields into the CLI envelope explicitly.
               (cond-> {:session-id session-id
                        :content (vec (or (get result "content") []))
                        :iteration-count (get result "iteration_count")
                        :duration-ms (get result "duration_ms")
                        :tokens (get result "tokens")
                        :cost (get result "cost")
                        :trace (get result "trace")}
                 (get result "status")
                 (assoc :status
                   (case (get result "status")
                     "needs_input"
                     :needs-input

                     (keyword (get result "status"))))

                 (get result "confidence")
                 (assoc :confidence (get result "confidence"))

                 (get result "eval")
                 (assoc :eval (get result "eval"))))
             (catch Exception e (run-error-result session-id e)))))))

;;; ── Output Formatting ───────────────────────────────────────────────────

(defn- json-key
  "Return a stable string key for CLI JSON output. Runtime trace maps can
   contain non-JSON map keys. Charred correctly rejects those, so normalize
   keys before writing the public `vis-agent --json` envelope."
  [k]
  (cond (string? k) k
        (keyword? k) (name k)
        (symbol? k) (str k)
        :else (pr-str k)))

(defn- json-safe
  [x]
  (cond (map? x) (reduce-kv (fn [m k v]
                              (assoc m (json-key k) (json-safe v)))
                            {}
                            x)
        (instance? java.util.Map$Entry x) [(json-safe (.getKey ^java.util.Map$Entry x))
                                           (json-safe (.getValue ^java.util.Map$Entry x))]
        (vector? x) (mapv json-safe x)
        (set? x) (mapv json-safe x)
        (seq? x) (mapv json-safe x)
        (keyword? x) (name x)
        (symbol? x) (str x)
        (uuid? x) (str x)
        (inst? x) (str x)
        (instance? Throwable x)
        {"type" (str (type x)) "message" (ex-message x) "data" (json-safe (ex-data x))}
        ;; JSON has no NaN/Infinity: charred rejects them and the whole
        ;; `vis-agent --json` envelope would fail over one field.
        (and (float? x) (not (Double/isFinite (double x)))) nil
        :else x))

(defn result->json [result] (json/write-json-str (json-safe result)))

;; Built-in CLI commands

(def ^:private trace-max-inline-chars 4000)

(defn- trace-safe
  "Make trace frames printable/readable for CLI streaming. Runtime values can
   contain Throwables, sets, lazy seqs, map entries, or other objects that are
   awkward in EDN/JSON output; keep the useful data and avoid unserializable
   exception objects."
  [x]
  (cond (instance? Throwable x) {:type (str (type x)) :message (.getMessage ^Throwable x)}
        (map? x) (into {}
                       (map (fn [[k v]]
                              [k (trace-safe v)]))
                       x)
        (map-entry? x) [(trace-safe (.getKey ^java.util.Map$Entry x))
                        (trace-safe (.getValue ^java.util.Map$Entry x))]
        (vector? x) (mapv trace-safe x)
        (set? x) (mapv trace-safe x)
        (seq? x) (mapv trace-safe x)
        (or (nil? x) (string? x) (number? x) (keyword? x) (symbol? x) (boolean? x) (char? x)) x
        :else (str x)))

(defn- trace-value-str
  [x]
  (try (pr-str (trace-safe x))
       (catch Throwable t (str "#<unprintable " (type t) ": " (.getMessage t) ">"))))

(defn- trace-pr-str
  [x]
  (let [s
        (trace-value-str x)

        c
        (long (count s))]

    (if (> c (long trace-max-inline-chars))
      (str (subs s 0 trace-max-inline-chars)
           "… [truncated "
           (- c (long trace-max-inline-chars))
           " chars]")
      s)))

(defn- trace-indent
  [s]
  (->> (str/split-lines (str s))
       (map #(str "    " %))
       (str/join "\n")))

(defn- trace-error-summary
  [err]
  (cond (map? err) (str (or (:message err) (:reason err) (:type err) "error")
                        (when-let [phase (:phase err)]
                          (str " [" phase "]"))
                        (when-let [hint (:hint err)]
                          (str "\n" (trace-indent (str "hint: " hint))))
                        (when-let [trace (:trace err)]
                          (str "\n" (trace-indent trace))))
        (some? err) (trace-pr-str err)
        :else nil))

(defn- print-full-trace-json-frame!
  [event payload]
  (commandline/stdout! (json/write-json-str (json-safe (trace-safe {:event event
                                                                    :payload payload})))))

(defn- trace-terminal?
  []
  (boolean (and (System/console)
                (str/blank? (System/getenv "NO_COLOR"))
                (not= "dumb" (System/getenv "TERM")))))

(defn- ansi [code s] (if (trace-terminal?) (str "\u001b[" code "m" s "\u001b[0m") (str s)))

(defn- trace-title [icon label] (ansi "1;96" (str icon " " label)))

;; Use bright-black, not ANSI dim (2): dim is unreadable on many themes.
(defn- trace-dim [s] (ansi "90" s))

(defn- trace-ok [s] (ansi "32" s))

(defn- trace-warn [s] (ansi "33" s))

(defn- trace-bad [s] (ansi "31" s))

(defn- trace-code [s] (ansi "36" s))

(def ^:private ansi-sgr-re #"\u001B\[[0-9;]*m")

(defn- strip-ansi [s] (str/replace (str s) ansi-sgr-re ""))

(defn- codepoint-width
  ^long [^long cp]
  (let [t (Character/getType (int cp))]
    (cond (= cp 9) 4
          (or (= t Character/NON_SPACING_MARK)
              (= t Character/COMBINING_SPACING_MARK)
              (= t Character/ENCLOSING_MARK))
          0
          (or (<= 0x1100 cp 0x115F)
              (<= 0x2E80 cp 0xA4CF)
              (<= 0xAC00 cp 0xD7A3)
              (<= 0xF900 cp 0xFAFF)
              (<= 0xFE10 cp 0xFE19)
              (<= 0xFE30 cp 0xFE6F)
              (<= 0xFF00 cp 0xFF60)
              (<= 0xFFE0 cp 0xFFE6)
              (<= 0x1F300 cp 0x1FAFF))
          2
          (< cp 32) 0
          :else 1)))

(defn- expand-tabs
  [s]
  (let [^String s
        (str s)

        n
        (.length s)

        sb
        (StringBuilder.)]

    (loop [i
           0

           col
           0]

      (if (>= i n)
        (.toString sb)
        (let [cp
              (.codePointAt s i)

              step
              (Character/charCount cp)]

          (cond (= cp 9) (let [spaces (- 4 (long (mod (long col) 4)))]
                           (.append sb (apply str (repeat spaces \space)))
                           (recur (+ i step) (+ (long col) spaces)))
                (= cp 10) (do (.append sb \newline) (recur (+ i step) 0))
                (< cp 32) (do (.append sb \space) (recur (+ i step) (inc (long col))))
                :else (let [piece (String. (Character/toChars cp))]
                        (.append sb piece)
                        (recur (+ i step) (+ (long col) (codepoint-width cp))))))))))

(defn- wrap-plain-line
  [s max-cols]
  (let [^String s
        (str s)

        n
        (.length s)

        max-cols
        (max 8 (long max-cols))]

    (loop [i
           0

           col
           0

           line
           (StringBuilder.)

           acc
           []]

      (if (>= i n)
        (cond-> acc
          (pos? (.length line))
          (conj (.toString line)))
        (let [cp
              (.codePointAt s i)

              step
              (Character/charCount cp)

              piece
              (String. (Character/toChars cp))

              w
              (codepoint-width cp)]

          (if (and (pos? (.length line)) (> (+ col w) max-cols))
            (recur i 0 (StringBuilder.) (conj acc (.toString line)))
            (do (.append line piece) (recur (+ i step) (+ col w) line acc))))))))

(defn- pretty-block
  [label body]
  (when-not (str/blank? (strip-ansi body))
    (let [cols
          (max 40 (- (long (commandline/terminal-width)) 4))

          lines
          (->> (str/split-lines (expand-tabs body))
               (mapcat (fn [line]
                         (let [wrapped (wrap-plain-line line cols)]
                           (if (seq wrapped) wrapped [""])))))]

      (str "\n" (trace-dim (str "  ┌─ " label))
           "\n" (->> lines
                     (map #(str (trace-dim "  │ ") %))
                     (str/join "\n"))
           "\n" (trace-dim "  └")))))

(defn- print-pretty-trace-chunk!
  [chunk]
  (let [phase
        (:phase chunk)

        iter
        (:iteration chunk)

        head
        (str (trace-dim "\n┌─")
             " "
             (trace-title "λ" "trace")
             (when iter (str " " (trace-dim (str "iteration " iter))))
             " ")]

    (case phase
      :provider-call
      (commandline/stdout! (str head
                                (trace-title "↗" "provider call")
                                (when-let [t (:started-at-ms chunk)]
                                  (str " " (trace-dim (str "started=" t))))))

      :provider-fallback
      (commandline/stdout! (str head
                                (trace-warn "↷ provider fallback")
                                " "
                                (or (:failed-provider chunk) "?")
                                " → "
                                (or (:new-provider chunk) "?")
                                (when-let [reason (:reason chunk)]
                                  (str " " (trace-dim (str "(" reason ")"))))))

      :provider-retry-reset
      (commandline/stdout! (str head
                                (trace-warn "↻ provider stream retry")
                                (when-let [attempt (:attempt chunk)]
                                  (str " " (trace-dim (str "attempt=" attempt))))
                                (when-let [delay-ms (:delay-ms chunk)]
                                  (str " " (trace-dim (str "delay=" delay-ms "ms"))))
                                (pretty-block "error"
                                              (or (some-> chunk
                                                          :event
                                                          :error)
                                                  (trace-pr-str (:error chunk))))))

      :reasoning
      ;; Discrete one-shot render: only fires once per iteration when
      ;; `:done?` is true. Append-only streaming during reasoning happens
      ;; in `make-pretty-trace-printer` via the `:delta` path. We keep
      ;; this branch tidy so callers that bypass the printer wrapper
      ;; still get the full block rendered once provider streaming
      ;; completes. Mid-stream chunks (`:done? false`) are no-ops here so the
      ;; accumulated thinking block is not re-printed on every SSE tick.
      (when (and (:done? chunk) (not (str/blank? (str (:thinking chunk)))))
        (commandline/stdout!
          (str head (trace-title "🧠" "reasoning") (pretty-block "thinking" (:thinking chunk)))))

      :response-parse
      (commandline/stdout!
        (if (= :start (:status chunk))
          (str head
               (trace-title "⌁" "response parse")
               " "
               (trace-dim "started")
               (when-let [n (:raw-length chunk)]
                 (str " " (trace-dim (str "raw=" n " chars"))))
               (when-let [n (:form-count chunk)]
                 (str " " (trace-dim (str "blocks=" n)))))
          (str head
               (trace-ok "✓ response parsed")
               (when-let [n (:forms chunk)]
                 (str " forms=" n))
               (when-let [n (:code-length chunk)]
                 (str " " (trace-dim (str "code=" n " chars"))))
               (when-let [n (:duration-ms chunk)]
                 (str " " (trace-dim (str n "ms")))))))

      :form-start
      (commandline/stdout! (str head
                                (trace-title "▶"
                                             (str "form "
                                                  (inc (long (or (:form-idx chunk) 0)))
                                                  (when-let [of (:form-of chunk)]
                                                    (str "/" of))))
                                " "
                                (trace-dim "started")
                                (pretty-block "code" (trace-code (:code chunk)))))

      :tool-start
      (commandline/stdout! (str head
                                (trace-title "⚙" "tool")
                                (pretty-block "event" (trace-pr-str (:tool-event chunk)))))

      :form-result
      (commandline/stdout!
        (str head
             (if (:error chunk) (trace-bad "✗ form failed") (trace-ok "✓ form finished"))
             " #"
             (inc (long (or (:form-idx chunk) 0)))
             (when-let [of (:form-of chunk)]
               (str "/" of))
             (when-let [ms (form/envelope-duration-ms (:envelope chunk))]
               (str " " (trace-dim (str ms "ms"))))
             (when (:repaired? chunk) (str " " (trace-warn "repaired")))
             (when (:timeout? chunk) (str " " (trace-bad "timeout")))
             (if-let [err (trace-error-summary (:error chunk))]
               (pretty-block "error" (trace-bad err))
               (when-let [stdout (not-empty (:stdout chunk))]
                 (pretty-block "stdout" stdout)))))

      :iteration-final
      (commandline/stdout!
        (str head
             (if (:done? chunk) (trace-ok "✓ turn complete") (trace-title "·" "iteration complete"))
             (when-let [final (:final chunk)]
               (pretty-block "final"
                             (trace-pr-str (select-keys final [:status :iteration-count]))))))

      :iteration-error
      (commandline/stdout!
        (str head
             (trace-bad "✗ iteration error")
             (pretty-block "error" (or (trace-error-summary (:error chunk)) (trace-pr-str chunk)))))

      (commandline/stdout! (str head
                                (trace-title "•" (name (or phase :unknown)))
                                (pretty-block "chunk" (trace-pr-str chunk)))))))

(defn- trace-final-summary-prose
  "Human prose for the pretty terminal trace footer. Keep raw maps for the
   EDN/JSON stream modes; the terminal trace should read like a tiny run
   report, not like dumped data."
  [result]
  (let [failed?
        (boolean (:error result))

        iters
        (fmt/format-iterations (:iteration-count result))

        duration
        (fmt/format-duration (:duration-ms result))

        tokens
        (fmt/format-tokens (:tokens result))

        cost
        (fmt/format-cost (:cost result))

        confidence
        (some-> (:confidence result)
                name)

        status
        (some-> (:status result)
                name)

        where
        (str/join " in " (remove str/blank? [iters duration]))

        opener
        (str (if failed? "The run stopped with an error" "The run completed successfully")
             (when-not (str/blank? where) (str " after " where))
             ".")]

    (str/join "\n"
              (remove str/blank?
                [opener (when tokens (str "It used " tokens "."))
                 (when cost (str "Estimated cost: " cost "."))
                 (when confidence (str "Confidence was " confidence "."))
                 (when status (str "Final status: " status "."))
                 (when-let [err (:error result)]
                   (str "Error: " err))]))))

;; Append-only pretty trace printer.
;;
;; Strictly append-only (no cursor-erase redraw): dedups iteration headers per
;; iteration, and streams reasoning as DELTAS (`:delta` is computed in
;; `loop.clj`'s `streaming-fn` as the new tail since the previous chunk) so
;; each reasoning character is emitted exactly once across the whole run.
;; Output is identical in a TTY, a pipe, or a pty wrapper, and non-TTY
;; consumers (CI logs, `vis-agent ... | tee`) get the full stream.

(defn- make-pretty-trace-printer
  []
  (let [;; Per-iteration display state:
        ;;   :reasoning-open? - whether the `┌─ λ trace iteration N 🧠
        ;;                      reasoning` header + `┌─ thinking` rail have
        ;;                      already been printed; subsequent deltas
        ;;                      append directly with the dim left rail.
        ;;   :pending-line    - in-flight partial line (no trailing newline)
        ;;                      so we can re-prefix correctly when more
        ;;                      delta text arrives.
        state (atom {})]
    (letfn
      [(close-reasoning! [iter]
         (let [s (get @state iter)]
           (when (:reasoning-open? s)
             (when-not (str/blank? (str (:pending-line s))) (commandline/stdout! ""))
             (commandline/stdout! (trace-dim "  └"))
             (swap! state assoc
               iter
               (assoc s
                 :reasoning-open? false
                 :pending-line nil)))))
       (emit-reasoning-delta! [iter delta]
         (when-not (get-in @state [iter :reasoning-open?])
           (commandline/stdout! (str (trace-dim "\n┌─")
                                     " "
                                     (trace-title "λ" "trace")
                                     (when iter (str " " (trace-dim (str "iteration " iter))))
                                     " "
                                     (trace-title "🧠" "reasoning")))
           (commandline/stdout! (trace-dim "  ┌─ thinking"))
           (commandline/write-stdout! (trace-dim "  │ "))
           (swap! state update iter assoc :reasoning-open? true :pending-line ""))
         ;; `parts` splits on '\n' preserving empty trailing segments.
         ;; Every segment except the LAST was followed by a newline in
         ;; the source delta; print it, end the line, and start a fresh
         ;; rail. The last segment may be a partial (no trailing \n)
         ;; that we keep buffered as `:pending-line` for the next
         ;; delta to extend.
         (let [parts (str/split (str delta) #"\n" -1)]
           (dotimes [i (dec (count parts))]
             (commandline/write-stdout! (nth parts i))
             (commandline/stdout! "") ; newline
             (commandline/write-stdout! (trace-dim "  │ "))
             (swap! state assoc-in [iter :pending-line] ""))
           (let [tail (peek parts)]
             (when (and tail (pos? (count tail))) (commandline/write-stdout! tail))
             (swap! state update-in [iter :pending-line] #(str (or % "") tail)))))]
      (fn pretty-trace-on-chunk [chunk]
        (let [phase (:phase chunk)
              iter (:iteration chunk)]

          (case phase
            :reasoning
            (let [delta (:delta chunk)
                  thinking (str (:thinking chunk))
                  done? (boolean (:done? chunk))
                  ;; Backward-compat: if `:delta` was not provided (older
                  ;; host), fall back to printing the full text only on
                  ;; `:done?` — still better than re-printing on every tick.
                  effective (cond (some? delta) delta
                                  (and done? (not (str/blank? thinking))) thinking
                                  :else "")]

              (when-not (str/blank? effective) (emit-reasoning-delta! iter effective))
              (when done? (close-reasoning! iter)))

            ;; Any non-reasoning phase implies this iteration's reasoning
            ;; stream is over: close the rail before printing the next
            ;; discrete event.
            (do (when iter (close-reasoning! iter)) (print-pretty-trace-chunk! chunk))))))))

(defn- print-section-heading!
  "Render a section heading line for a grouped table - used when
   `vis-agent extension list` breaks the rows into per-`:ext/kind`
   sub-tables. `width` is the total visible width of the surrounding
   table so the rule under the label spans the same column run."
  [label width]
  (let [label-str
        (str " " label " ")

        rule-len
        (max 4 (- (long width) (long (count label-str)) 2))]

    (commandline/stdout! "")
    (commandline/stdout! (str "── " label " " (apply str (repeat rule-len \─))))))

;;; ── Root one-shot run - handler + bespoke arg parser ─────────────────────

(def ^:private run-boolean-flags
  "Root one-shot flags that take no value, mapped to their opts key."
  {"--json" :json?
   "--code" :code?
   "--raw" :raw?
   "--full-trace-stream" :full-trace-stream?
   "--trace" :full-trace-stream?
   "--full-trace-json-stream" :full-trace-json-stream?
   "--full-trace-json-stream-raw" :full-trace-json-stream?
   ;; `--verbose` / `-v` are read by `configure-logging!` too; they are listed
   ;; here so they are consumed rather than glued into the prompt.
   "--debug" :debug?
   "--verbose" :debug?
   "-v" :debug?
   "--persist" :persist?})

(def ^:private run-value-flags
  "Root one-shot flags that consume the NEXT token as their value."
  {"--toggles" :toggles
   "--provider" :provider
   "--model" :model
   "--reasoning-effort" :reasoning-effort
   "--name" :agent-name
   "--db" :db
   "--session-id" :session-id})

(defn- option-token?
  "True for a bare token SHAPED like a flag (`-v`, `--json`, `--full-trace-stream`).
   Prompt prose never qualifies: a quoted prompt that opens with dashes carries
   whitespace (`vis-agent \"--json output is broken\"`), and `--` ends flag parsing."
  [arg]
  (boolean (re-matches #"-{1,2}[A-Za-z][A-Za-z0-9-]*" (str arg))))

(def ^:private renderer-flags
  "Flags that each OWN the run's output. Only one can win, so naming two is a
   question Vis cannot answer: `--json --code` silently printed JSON and dropped
   the code the caller actually asked for."
  {:json? "--json"
   :code? "--code"
   :full-trace-stream? "--full-trace-stream"
   :full-trace-json-stream? "--full-trace-json-stream"})

(defn- check-run-conflicts
  "Add a `:flag-errors` entry when the parsed run opts name more than one
   output mode. Aliases (`--trace`, `--full-trace-json-stream-raw`) report
   under their canonical flag."
  [opts]
  (let [named (->> renderer-flags
                   (keep (fn [[k flag]]
                           (when (get opts k) flag)))
                   sort
                   vec)]
    (cond-> opts
      (< 1 (count named))
      (update :flag-errors
              (fnil conj [])
              (str "name one output mode, not " (str/join " and " named))))))

(defn- check-db-target
  "`--db PATH` used to reach SQLite as a raw `[SQLITE_CANTOPEN] Failed to
   initialize pool` fatal that never named the path or the reason. Refuse a
   missing directory, a directory given as the file, and an unwritable target."
  [{:keys [db] :as opts}]
  (if (or (nil? db) (= ":memory" db))
    opts
    (let [^java.io.File f
          (.getAbsoluteFile (io/file db))

          ^java.io.File parent
          (.getParentFile f)

          err
          (cond
            (.isDirectory f) (str "--db " db " is a directory, not a database file")
            (and parent (not (.isDirectory parent)))
            (str "--db " db " needs an existing directory; " (.getPath parent) " does not exist")
            (and (.exists f) (not (.canWrite f))) (str "--db " db " is not writable")
            (and (not (.exists f)) parent (not (.canWrite parent)))
            (str "--db " db " cannot be created; " (.getPath parent) " is not writable"))]

      (cond-> opts
        err
        (update :flag-errors (fnil conj []) err)))))

(defn- parse-run-args
  "Parse root one-shot run arguments into {:prompt str :json? bool ...}.

   Bespoke instead of `commandline.base/parse-args` because everything
   that ISN'T a known flag is glued together as the prompt body. A token
   shaped like a flag but unknown, or a value flag left without a value,
   lands in `:flag-errors` instead of the prompt: `vis-agent --modle x
   \"task\"` used to run silently with the DEFAULT model and a prompt
   polluted with the typo. `--` ends flag parsing, so a prompt can still
   start with dashes."
  [args]
  (loop [args
         (seq args)

         opts
         {}

         prompt-parts
         []]

    (if-not args
      (assoc opts :prompt (str/join " " prompt-parts))
      (let [arg
            (first args)

            more
            (next args)]

        (cond (= "--" arg) (assoc opts :prompt (str/join " " (into prompt-parts more)))
              (contains? #{"--help" "-h"} arg) (assoc opts
                                                 :help? true
                                                 :prompt "")
              (contains? run-boolean-flags arg)
              (recur more (assoc opts (run-boolean-flags arg) true) prompt-parts)
              (contains? run-value-flags arg)
              ;; A value flag with no usable value used to vanish: `--model ""`
              ;; ran the DEFAULT model, and `--model --json "task"` ran a model
              ;; literally named "--json". Blank, `--`, and flag-shaped tokens
              ;; are all "you forgot the value".
              (let [v (first more)]
                (if (or (str/blank? v) (= "--" v) (option-token? v))
                  (recur more
                         (update
                           opts
                           :flag-errors
                           (fnil conj [])
                           (str arg " needs a value" (when (option-token? v) (str " (got " v ")"))))
                         prompt-parts)
                  (recur (next more)
                         (cond-> (assoc opts (run-value-flags arg) v)
                           (= "--session-id" arg)
                           (assoc :persist? true))
                         prompt-parts)))
              (option-token? arg)
              (recur more
                     (update opts :flag-errors (fnil conj []) (str "unknown flag " arg))
                     prompt-parts)
              :else (recur more opts (conj prompt-parts arg)))))))

(defn- print-run-usage!
  []
  (commandline/stdout! "Usage: vis-agent [FLAGS] \"prompt\"")
  (commandline/stdout! "")
  (commandline/stdout! "Flags:")
  (commandline/stdout! "  --json            Print result as a single JSON envelope.")
  (commandline/stdout! "  --code            Print only [:code] block contents from the")
  (commandline/stdout! "                    parsed Markdown. Concatenated in source order;")
  (commandline/stdout! "                    no fences, no language tags. Pipes cleanly")
  (commandline/stdout! "                    into editors / interpreters. Errors when")
  (commandline/stdout! "                    the answer contains no [:code] blocks.")
  (commandline/stdout! "  --raw             Render the answer as raw text (no markdown")
  (commandline/stdout! "                    bold/italics/heading bars). This is also the")
  (commandline/stdout! "                    auto-default when stdout is not a TTY (piped")
  (commandline/stdout! "                    or redirected), so `vis-agent ... > out.txt`")
  (commandline/stdout! "                    produces clean text without ANSI noise.")
  (commandline/stdout! "  --toggles LIST    Comma-separated NAME=VALUE pairs setting any")
  (commandline/stdout!
    "                    registered snake_case toggle id for this run only, e.g.")
  (commandline/stdout! "                    --toggles reasoning_level=deep")
  (commandline/stdout! "  --full-trace-stream")
  (commandline/stdout! "                    Stream a pretty terminal trace while the run is")
  (commandline/stdout! "                    happening, then print the answer.")
  (commandline/stdout! "  --full-trace-json-stream")
  (commandline/stdout! "                    Stream raw JSON trace frames, one object per line.")
  (commandline/stdout! "  --debug           Enable verbose debug logging.")
  (commandline/stdout! "  --provider PROVIDER  Use this provider (e.g. openai, anthropic).")
  (commandline/stdout! "  --model MODEL        Override the configured model. Also accepts")
  (commandline/stdout! "                       provider/name (e.g. openai/gpt-4o).")
  (commandline/stdout!
    "  --reasoning-effort E  Exact provider-native effort (e.g. low, high or max).")
  (commandline/stdout! "  --name NAME          Set the agent name (default: cli).")
  (commandline/stdout! "  --db PATH|:memory    Override the SQLite path (or :memory).")
  (commandline/stdout! "  --session-id ID      Continue an existing persisted session.")
  (commandline/stdout! "  --persist            Write this run to ~/.vis/vis.mdb as a")
  (commandline/stdout! "                       resumable :cli session. Without it a run is")
  (commandline/stdout! "                       ephemeral: no resume, no session row on disk.")
  (commandline/stdout! "  --                   End flag parsing: every later word is prompt")
  (commandline/stdout! "                       text, dashes and all.")
  (commandline/stdout! "")
  (commandline/stdout! "Examples:")
  (commandline/stdout!
    "  vis-agent --provider zai-coding-plan --model glm-5.2 --reasoning-effort high --json \"Task\"")
  (commandline/stdout! "  vis-agent \"Throwaway one-shot probe\"")
  (commandline/stdout! "  vis-agent --json --model gpt-4o \"Explain auth flow\"")
  (commandline/stdout!
    "  vis-agent --toggles reasoning_level=deep \"Run the test suite and fix failures\"")
  (commandline/stdout! "  vis-agent --toggles reasoning_level=balanced \"Refactor carefully\"")
  (commandline/stdout!
    "  vis-agent --persist --provider anthropic --model claude-sonnet-4-20250514 \"Keep this\""))

(defn- parse-toggle-overrides
  "Parse a `--toggles` value like
   \"reasoning_level=deep\" into a map of {toggle-id value}. NAME must be the
   exact registered snake_case string id (e.g. `reasoning_level`,
   `openai_codex_verbosity`). VALUE is validated against the registered `:type`:
   booleans accept true/false (plus on/off, yes/no, 1/0), enums must name one of
   the registered `:choices`. Throws `:vis/user-error` ex-info on any bad pair so
   the CLI error path renders it as a user mistake, not a crash."
  [s]
  (reduce (fn [acc pair]
            (let [[k v]
                  (str/split pair #"=" 2)

                  id
                  (or k "")

                  spec
                  (toggles/toggle-spec id)]

              (when-not spec
                (throw (ex-info (str "Unknown toggle: " k)
                                {:type :vis.cli/unknown-toggle
                                 :vis/user-error true
                                 :id id
                                 :known (mapv :id (toggles/registered-toggles))})))
              (when (or (nil? v) (str/blank? v))
                (throw (ex-info (str "Toggle needs NAME=VALUE, got: " pair)
                                {:type :vis.cli/invalid-toggle :vis/user-error true :pair pair})))
              (assoc acc
                id (case (:type spec)
                     :enum
                     (let [value (str/replace v #"^:" "")]
                       (when-not (contains? (set (:choices spec)) value)
                         (throw (ex-info (str "Invalid value for " k ": " v)
                                         {:type :vis.cli/invalid-toggle
                                          :vis/user-error true
                                          :id id
                                          :value value
                                          :choices (:choices spec)})))
                       value)

                     (case (str/lower-case v)
                       ("true" "on" "yes" "1")
                       true

                       ("false" "off" "no" "0")
                       false

                       (throw (ex-info (str "Boolean toggle " k " needs true/false, got: " v)
                                       {:type :vis.cli/invalid-toggle
                                        :vis/user-error true
                                        :id id
                                        :value v})))))))
          {}
          (remove str/blank? (str/split (or s "") #","))))

(defn- call-with-toggle-overrides
  "Run `f` with each toggle in `overrides` ({id value}) applied, restoring
   every prior effective value afterward. Process-local and never persists
   a config change."
  [overrides f]
  (if (empty? overrides)
    (f)
    (let [previous (into {}
                         (map (fn [[id _]]
                                [id (toggles/value-of id)]))
                         overrides)]
      (try (doseq [[id v] overrides]
             (toggles/set-value! id v))
           (f)
           (finally (doseq [[id v] previous]
                      (toggles/set-value! id v)))))))

(defn- result-content
  "Return one CLI result's canonical typed content blocks."
  [result]
  (vec (or (:content result) [])))

(defn- cli-result-exit-code
  [result]
  (let [invalid-reasons
        (get-in result [:eval :invalid-reasons])

        unsupported?
        (some #(= :unsupported-reasoning-effort (:type %)) invalid-reasons)]

    (cond unsupported? 2
          (or (:error result) (contains? #{:error :cancelled} (:status result))) 1
          (false? (get-in result [:eval :valid?])) 2
          :else 0)))

(defn- cli-run!
  "Root one-shot run handler. `_parsed` is unused - we re-parse the residual
   ourselves so anything that isn't a flag falls into the prompt."
  [_parsed residual]
  (config/init-cli!)
  ;; Dispatch has registered extension toggles. Hydrate merged settings before
  ;; constructing the one-shot environment; explicit --toggles still wins below.
  (toggles/hydrate-from-config! (config/load-config-raw))
  (let [{:keys [prompt json? code? raw? full-trace-stream? full-trace-json-stream? help? agent-name
                db toggles]
         :as opts}
        (-> residual
            parse-run-args
            check-run-conflicts
            check-db-target)]
    ;; A flag typo used to be smuggled into the prompt: `vis-agent --modle x "task"`
    ;; ran with the DEFAULT model and never said so. Refuse instead, and name the
    ;; escape hatch for prompts that really do start with dashes.
    (when-let [errors (seq (:flag-errors opts))]
      (doseq [e errors]
        (commandline/stdout! (str "vis-agent: " e)))
      (commandline/stdout! "  See the flag list:            vis-agent --help")
      (when (some #(str/starts-with? % "unknown flag") errors)
        (commandline/stdout! "  Or make it the prompt text:   vis-agent -- <text>"))
      (System/exit 2))
    (when (or help? (str/blank? prompt)) (print-run-usage!) (System/exit 0))
    ;; Auto-promote to raw when stdout is NOT a TTY (piped/redirected).
    ;; Otherwise `vis-agent ... > out.txt` leaves bold/italic ANSI markers in
    ;; the file. Structured output flags (--json/--edn/--code) win, and an
    ;; explicit --raw stays raw. The trace-stream flags own their own
    ;; output path and are unaffected.
    (let [structured-output? (or json? code? full-trace-stream? full-trace-json-stream?)
          effective-raw? (or raw? (and (not structured-output?) (not (trace-terminal?))))
          agent-def (agent {:name (or agent-name "cli")})
          trace-on-chunk (cond full-trace-json-stream? #(print-full-trace-json-frame! :trace-chunk
                                                                                      %)
                               full-trace-stream? (make-pretty-trace-printer))
          run-opts (cond-> (dissoc opts
                             :prompt
                             :json?
                             :code?
                             :raw?
                             :full-trace-stream?
                             :full-trace-json-stream?
                             :compact?
                             :agent-name
                             :db
                             :toggles)
                     trace-on-chunk
                     (assoc :on-chunk trace-on-chunk)

                     db
                     (assoc :db
                       (config/resolve-db-spec
                         (if (= db ":memory") :memory {:backend :sqlite :path db}))))
          result (call-with-toggle-overrides (parse-toggle-overrides toggles)
                                             #(run! agent-def prompt run-opts))
          exit-code (cli-result-exit-code result)
          trace-result (select-keys result
                                    [:session-id :content :trace :iteration-count :duration-ms
                                     :tokens :cost :confidence :status :error :type :eval])]

      (cond full-trace-json-stream? (print-full-trace-json-frame! :result trace-result)
            full-trace-stream?
            (do (tel/log! {:level :info :id ::cli-trace :data trace-result} "CLI trace result")
                (commandline/stdout!
                  (str "\n"
                       (trace-dim "└────────────────────────────────────────────────────────")))
                (commandline/stdout! (str "\n"
                                          (trace-title "◆" "final result")
                                          (pretty-block "summary"
                                                        (trace-final-summary-prose result))))
                (commandline/stdout! (str "\n" (trace-title "◆" "answer") "\n"))
                (commandline/stdout! (content/text-projection (result-content result)))
                (when (:error result)
                  (when-let [ex (:exception result)]
                    (commandline/stdout! "\nStack trace:")
                    (.printStackTrace ^Throwable ex ^java.io.PrintStream config/original-stdout))))
            json? (commandline/stdout! (result->json result))
            code?
            (let [blocks (->> (result-content result)
                              (keep #(when (= "code" (get % "type")) (get % "text")))
                              vec)]
              (cond (:error result) (commandline/stdout! (error/format-error (:error result)))
                    (empty? blocks)
                    (do (commandline/stdout!
                          "Error: --code expects at least one code content block; got prose only.")
                        (shutdown-agents)
                        (System/exit 1))
                    :else (commandline/stdout! (str/join "\n\n" blocks))))
            (:error result) (commandline/stdout! (error/format-error (:error result)))
            :else (do (commandline/stdout! (content/text-projection (result-content result)))
                      (when (and (:duration-ms result) (not effective-raw?))
                        (commandline/stdout! (str "\n[" (fmt/format-meta-line result) "]")))))
      (shutdown-agents)
      (when (pos? (long exit-code)) (System/exit exit-code)))))

;;; ── `vis-agent doctor` ────────────────────────────────────────────────────────

(defn- housekeeping-line
  [{:keys [kind label root age-days bytes is-purged]}]
  (str "  "
       (case kind
         :stale
         "draft   "

         :discarded
         "discard "

         :orphan
         "orphan  "

         :journal
         "journal "

         "?       ")
       (format "%-28s" (str (or label "?")))
       (format "%9s" (fmt/format-bytes (or bytes 0) " "))
       (when age-days (str "  " age-days "d"))
       (when (false? is-purged) "  (kept)")
       "\n      "
       root))

(defn- cli-doctor!
  "`vis-agent doctor` — cross-extension diagnostics, plus the housekeeping valve.

   Without flags it only REPORTS. `--purge` is the only thing that deletes,
   `--dry-run` turns it back into a listing, and `--days N` moves the staleness
   cutoff for both."
  [parsed _residual]
  (config/init-cli!)
  (let [db-info
        (config/resolve-db-spec)

        days
        (get parsed "days")

        is-purge
        (boolean (get parsed "purge"))

        is-dry-run
        (boolean (get parsed "dry-run"))]

    (if-not is-purge
      (let [msgs (doctor/run-checks (cond-> {:db-info db-info}
                                      days
                                      (assoc :housekeeping-days days)))]
        (commandline/stdout! (doctor/format-output msgs))
        (System/exit (int (doctor/exit-code msgs))))
      (let [{:keys [purged count bytes reclaimed-bytes] :as report}
            (housekeeping/purge! {:db-info db-info :days days :is-dry-run is-dry-run})]
        (commandline/stdout! (if (zero? (long (or count 0)))
                               (str "Nothing untouched for over "
                                    (:days report)
                                    " days — drafts and session journals are already tidy.")
                               (str (if is-dry-run "Would reclaim " "Reclaimed ")
                                    count
                                    (if (= 1 (long count)) " item, " " items, ")
                                    (fmt/format-bytes (or (if is-dry-run bytes reclaimed-bytes) 0)
                                                      " ")
                                    " (untouched for over "
                                    (:days report)
                                    " days):\n"
                                    (str/join "\n" (map housekeeping-line purged))
                                    (when is-dry-run "\n\nRe-run without --dry-run to reclaim."))))
        (System/exit 0)))))

;;; ── `vis-agent extension` ───────────────────────────────────────────────────────────

(def ^:private extensions-table-cols
  [{:key :namespace :label "Extension" :width 28 :align :left}
   {:key :group :label "Group" :width 18 :align :left}
   {:key :author :label "Author" :width 12 :align :left}
   {:key :owner :label "Owner" :width 16 :align :left}
   {:key :license :label "License" :width 10 :align :left}
   {:key :doc :label "Description" :width 36 :align :left :grow? true}
   {:key :version :label "Version" :width 10 :align :left}])

(defn- cli-extensions!
  [_parsed _residual]
  (config/init-cli!)
  (let [exts
        (list-extensions)

        cols
        (commandline/expand-table-cols extensions-table-cols (commandline/terminal-width))

        width
        (commandline/table-width cols)]

    (if (empty? exts)
      (commandline/stdout! "No extensions registered.")
      (do (commandline/stdout! "\n  Extensions\n")
          (doseq [[kind rows] (sort-by key (group-by :kind exts))]
            (print-section-heading! kind width)
            (commandline/print-table! cols (sort-by (juxt :group :namespace) rows)))
          (commandline/stdout! (str "\n  " (count exts) " extension(s)\n")))))
  (shutdown-agents))

;;; ── `vis-agent python` — standalone CPython interpreter ────────────────────────
;;
;; Expose JUST the embedded Python sandbox -- the real CPython, the packages
;; `pip` put in `~/.vis/python/packages`, the host-call doors and the
;; auto-imports -- with NO agent tool bindings. Handy for reproducing sandbox
;; behaviour straight from the shell. Behaves identically under the JVM and the
;; native image: both drive the same `env/*` machinery.

(defn- python-cli-project-environment
  "Select a project before interpreter startup; shared tools opt out explicitly."
  [cwd environment shared?]
  (when-not shared?
    (let [configured
          (not-empty (get environment "UV_PROJECT_ENVIRONMENT"))

          path
          (io/file (or configured ".venv"))

          venv
          (if (.isAbsolute path) path (io/file cwd path))]

      (when (or configured (.exists venv) (.isFile (io/file cwd "pyproject.toml")))
        (.getCanonicalPath venv)))))

(defn- activate-python-cli-environment!
  "Load an existing uv environment after confinement, without syncing or granting paths.
   Resolve UV_PROJECT_ENVIRONMENT relative to cwd; otherwise use cwd/.venv."
  [ctx cwd environment]
  (pyrt/exec!
    ctx
    (str
      "def __vis_activate_cli_environment__(cwd, configured):\n"
      "    import sys, sysconfig\n"
      "    from pathlib import Path\n"
      "    import package_paths\n"
      "    venv = Path(configured or '.venv')\n"
      "    if not venv.is_absolute():\n"
      "        venv = Path(cwd) / venv\n"
      "    sites = list(dict.fromkeys(\n"
      "        sysconfig.get_path(name, vars={'base': str(venv), 'platbase': str(venv)})\n"
      "        for name in ('purelib', 'platlib')))\n"
      "    if not any(Path(path).is_dir() for path in sites):\n"
      "        raise RuntimeError(\n"
      "            f'Project environment {venv} has no site-packages for embedded Python '\n"
      "            f'{sys.version_info.major}.{sys.version_info.minor}. '\n"
      "            'Run vis-agent python uv sync --python with a compatible interpreter, or use '\n"
      "            'vis-agent python uv run --no-sync python to run its own interpreter.')\n"
      "    before = list(sys.path)\n"
      "    for path in sites:\n"
      "        if Path(path).is_dir():\n"
      "            package_paths.refresh(path)\n"
      "    added = [path for path in sys.path if path not in before]\n"
      "    sys.path[:] = added + before\n"
      "try:\n"
      "    __vis_activate_cli_environment__("
      (env/py-json-literal cwd)
      ", "
      (env/py-json-literal (get environment "UV_PROJECT_ENVIRONMENT"))
      ")\n"
      "finally:\n"
      "    del __vis_activate_cli_environment__\n")))

(defn- python-cli-context
  "Build a fresh standalone Python sandbox for `vis-agent python`: the same
   interpreter the agent runs, filesystem rooted at the current working
   directory, network enabled unless `network?` is false. No tool bindings --
   just the interpreter and its host-call doors.

   Unlike the agent sandbox this is a HUMAN-run interpreter, so it gets
   real-`python` niceties: `argv` is bound to `sys.argv`; `env` is merged
   into `os.environ`; and `sys.path` starts with the invocation directory
   (except for FILE, whose directory the runner inserts), followed by
   `PYTHONPATH`, configured `python.source_paths`, and any `src`-layout roots
   declared by the project's packaging metadata. An existing cwd/.venv (or
   UV_PROJECT_ENVIRONMENT) supplies installed packages and editable hooks, without
   shared packages. A pyproject.toml without an environment reports a sync error.
   `shared?` skips project activation and inferred/configured source roots. The
   process stdin is wired to guest `sys.stdin`, so it works alongside `-c`/FILE."
  [{:keys [network? argv env shared? mode]}]
  (let [cwd
        (.getCanonicalPath (io/file (System/getProperty "user.dir")))

        project-environment
        (python-cli-project-environment cwd env shared?)

        _
        (env/ensure-interpreter! {:packages (when-not project-environment (pyrt/packages-dir))})

        {:keys [python-context]}
        (env/create-python-context {}
                                   (fn []
                                     [cwd])
                                   {:enabled? (boolean network?)}
                                   System/in)]

    ;; The CLI owns a real terminal: prompts must reach it before input() blocks.
    ;; Agent contexts keep the default captured-output callback.
    (python-host/install-sync-tools! python-context
                                     {"__vis_capture_stdout__" commandline/write-stdout!})
    ;; Bind an empty standing `ctx` dict so the async runtime has it available.
    (env/bind-ctx! python-context {})
    ;; Forward script argv + (by default) the caller's env — real-python CLI
    ;; semantics, distinct from the scrubbed agent sandbox.
    (env/seed-cli-runtime! python-context {:argv argv :env env})
    ;; The source launcher changes OS cwd to resolve deps, preserving the caller
    ;; in user.dir. Python and relative script reads must use that caller too.
    (try (pyrt/exec! python-context (str "import os\nos.chdir(" (env/py-json-literal cwd) ")\n"))
         (when project-environment
           (activate-python-cli-environment! python-context
                                             cwd
                                             {"UV_PROJECT_ENVIRONMENT" project-environment}))
         (catch Throwable t (env/dispose-python-context! python-context) (throw t)))
    ;; The interpreter receives the environment after startup, so PYTHONPATH
    ;; needs the same explicit sys.path setup a process launch would perform.
    ;; -m/-c start with cwd, while FILE starts with its own directory in runpy;
    ;; explicit entries follow, then configured and inferred project roots.
    (let [separator
          java.io.File/pathSeparator

          explicit
          (remove str/blank?
            (str/split (or (get env "PYTHONPATH") "")
                       (re-pattern (java.util.regex.Pattern/quote separator))))

          roots
          (distinct (concat (when-not (= :file mode) [cwd])
                            explicit
                            (when-not shared? (pyproj/import-roots python-context cwd))))]

      (when (seq roots)
        (pyrt/exec! python-context
                    (str "import sys\n" "sys.path[:0] = " (env/py-json-literal (vec roots)) "\n"))))
    python-context))

(defn- run-python-source!
  "Evaluate one Python source block in `ctx`, streaming printed output as it is
   written. Returns the process exit code (0 ok, 1 on a raised error). A bare
   trailing expression is never echoed."
  [ctx code]
  (let [{:keys [error]} (env/run-python-block ctx code)]
    (if error (do (commandline/stdout! (or (:message error) (pr-str error))) 1) 0)))

(defn- python-repl!
  "Minimal interactive REPL over one persistent standalone sandbox `ctx`.
   Reads a whole block (terminated by a blank line, so multi-line defs work),
   evaluates it, and prints captured stdout. Ctrl-D / EOF quits."
  [ctx]
  (commandline/stdout! (str
                         "vis-agent python -- embedded Python sandbox (no tools). "
                         "Blank line runs the block; use print(...) to see output; Ctrl-D quits."))
  (let [reader (java.io.BufferedReader. (java.io.InputStreamReader. System/in))]
    (loop []

      (commandline/write-stdout! ">>> ")
      (let [buf (StringBuilder.)
            eof? (loop []

                   (let [line (.readLine reader)]
                     (cond (nil? line) true
                           (str/blank? line) false
                           :else (do (.append buf line)
                                     (.append buf "\n")
                                     (commandline/write-stdout! "... ")
                                     (recur)))))
            code (str/trim (.toString buf))]

        (when (seq code) (run-python-source! ctx code))
        (if eof? (commandline/stdout! "") (recur))))))

(defn- python-cli-env-overrides->map
  "Turn `--env` values (`\"K=V\"`, or a bare `\"K\"`) into a `{key value}` map.
   A bare key with no `=` maps to an empty string."
  [overrides]
  (reduce (fn [m kv]
            (if-let [i (str/index-of kv "=")]
              (assoc m (subs kv 0 i) (subs kv (inc (long i))))
              (assoc m kv "")))
          {}
          overrides))

(defn- python-program-plan
  "Given the args from the program selector onward, return the run plan:
   `-c CODE …` → `{:mode :code :code CODE :argv [\"-c\" …trailing]}`,
   `-m MOD …`  → `{:mode :module :module MOD :argv [MOD …trailing]}`,
   `- …`       → `{:mode :stdin :argv [\"-\" …trailing]}`,
   `FILE …`    → `{:mode :file :file FILE :argv [FILE …trailing]}`.
   Trailing tokens ride into `argv` verbatim (CPython semantics; for `-m` the
   module name takes the `argv[0]` slot, as CPython puts the module there)."
  [prog]
  (cond (= "-c" (first prog)) {:mode :code :code (second prog) :argv (into ["-c"] (drop 2 prog))}
        (= "-m" (first prog))
        {:mode :module :module (second prog) :argv (into [(or (second prog) "-m")] (drop 2 prog))}
        (= "uv" (first prog)) {:mode :uv :argv (vec (rest prog))}
        (= "-" (first prog)) {:mode :stdin :argv (vec prog)}
        :else {:mode :file :file (first prog) :argv (vec prog)}))

(defn- parse-python-cli-args
  "Parse `vis-agent python` residual args into a runtime plan. Leading options
   (`--shared`, `--no-network`, `--no-env`, `--env K=V`, and an explicit `--`) are
   consumed until the program selector (`-c`, `-`, or a FILE); everything
   from the selector on is the program plus its verbatim script `argv`
   (mirrors CPython: trailing args land in `sys.argv`, flags included).
   With no selector the mode is `:interactive` (REPL on a TTY, else stdin)."
  [residual]
  (loop [network?
         true

         inherit-env?
         true

         env-overrides
         []

         shared?
         false

         args
         (vec residual)]

    (let [a (first args)]
      (cond (nil? a) {:network? network?
                      :inherit-env? inherit-env?
                      :env-overrides env-overrides
                      :shared? shared?
                      :mode :interactive
                      :argv []}
            (= a "--shared") (recur network? inherit-env? env-overrides true (subvec args 1))
            (= a "--no-network") (recur false inherit-env? env-overrides shared? (subvec args 1))
            (= a "--no-env") (recur network? false env-overrides shared? (subvec args 1))
            (= a "--env") (recur network?
                                 inherit-env?
                                 (cond-> env-overrides
                                   (some? (second args))
                                   (conj (second args)))
                                 shared?
                                 (subvec args (min (count args) 2)))
            (str/starts-with? a "--env=")
            (recur network? inherit-env? (conj env-overrides (subs a 6)) shared? (subvec args 1))
            :else (let [prog (if (= a "--") (subvec args 1) args)]
                    (merge {:network? network?
                            :inherit-env? inherit-env?
                            :env-overrides env-overrides
                            :shared? shared?}
                           (if (empty? prog)
                             {:mode :interactive :argv []}
                             (python-program-plan prog))))))))

(def ^:private python-cli-runner-src
  "Python helper for `vis-agent python -m MODULE` and `vis-agent python FILE`.
   Both run through runpy as `__main__`, recording SystemExit for the host."
  (slurp (io/resource "vis-python/module_runner.py")))

(defn- python-cli-exit-code
  "The exit code the guest runner recorded, or 0 when it recorded none.

   A block answers with what it PRINTED and nothing else, so the code the
   process owes is left in the session and read back here."
  [ctx]
  (try (let [v (json/read-json (pyrt/run ctx "globals().get('__vis_cli_exit__')") :key-fn identity)]
         (if (integer? v) (int v) 0))
       (catch Throwable _ 0)))

(defn- run-python-program!
  "Run a file or module as `__main__`, streaming its output to the terminal."
  [ctx runner target]
  (let [code
        (str python-cli-runner-src "\n" runner "(" (pr-str target) ")\n")

        ;; Programs own their event loop; do not wrap runpy in the tool coroutine.
        {:keys [error]}
        (json/read-json
          (pyrt/run ctx
                    (str "__import__('vis_runtime').run_sync_block(" (pr-str code) ", globals())"))
          :key-fn
          keyword)]

    (if error (do (commandline/stdout! error) 1) (python-cli-exit-code ctx))))

(defn- run-python-module!
  "Run MODULE as `__main__` in `ctx`, returning its exit code."
  [ctx module]
  (if (str/blank? module)
    (do (commandline/stderr! "vis-agent python -m requires a MODULE argument.") 2)
    (run-python-program! ctx "__vis_run_module__" module)))

(defn- run-python-file!
  "Run FILE as `__main__` in `ctx`, returning its exit code."
  [ctx file]
  (run-python-program! ctx "__vis_run_file__" file))

(defn- cli-python!
  "`vis-agent python` -- run code in the embedded Python sandbox (no tool
   bindings). Modes: `-c CODE` (run a string), `FILE.py` (run a file), `-` or
   piped stdin (run stdin), or an interactive REPL on a bare TTY. Trailing args
   after the program selector become `sys.argv`. `--no-network` disables sandbox
   network; the caller's environment is inherited into `os.environ` by default
   (`--no-env` scrubs it, `--env K=V` sets/overrides one var). `--shared` selects
   Vis's shared packages instead of the current project's environment."
  [_parsed residual]
  (config/init-cli!)
  (let [{:keys [network? inherit-env? env-overrides shared? mode code file module argv]}
        (parse-python-cli-args residual)

        env
        (merge (if inherit-env? (into {} (System/getenv)) {})
               (python-cli-env-overrides->map env-overrides))

        ctx
        (when-not (= :uv mode)
          (python-cli-context {:network? network? :argv argv :env env :shared? shared? :mode mode}))

        exit
        (case mode
          :uv
          (try (when (or (not network?) (not inherit-env?) (seq env-overrides))
                 (throw (ex-info
                          "Python sandbox options do not apply to uv; put uv directly after python"
                          {})))
               (python-runtime/uv-command! argv {:shared? shared?})
               (catch Throwable t (commandline/stderr! (.getMessage t)) 1))

          :code
          (if code
            (run-python-source! ctx code)
            (do (commandline/stderr! "vis-agent python -c requires a CODE argument.") 2))

          :stdin
          (run-python-source! ctx (slurp System/in))

          :file
          (let [f (io/file file)]
            (if (.isFile f)
              (run-python-file! ctx file)
              (do (commandline/stderr! (str "vis-agent python: no such file: " file)) 2)))

          :module
          (run-python-module! ctx module)

          :interactive
          (if (some? (System/console))
            (do (python-repl! ctx) 0)
            (run-python-source! ctx (slurp System/in))))]

    (shutdown-agents)
    (System/exit exit)))

;;; ── Top-level binary built-ins (registry/register-cmd! direct) ─────────
;;
;; These are the binary's own parent commands. They live at the top of the command
;; tree and bypass extension-contributed CLI registration.

(doseq
  [spec
   [{:cmd/name "stdio"
     :cmd/doc "Serve a Python SDK-owned local engine over stdin/stdout without HTTP."
     :cmd/usage "vis-agent stdio"
     :cmd/extra-sections
     [{:title "USE FROM PYTHON"
       :body (str/join "\n"
                       ["  Agent() and LocalEngine start this command for you, select a private"
                        "  session database, and stop the process when their context closes." ""
                        "  from blockether.vis.engine import Agent"
                        "  with Agent(project=\".\") as agent:"
                        "      print(agent.run(\"Summarize this project\")[\"status\"])" ""
                        "  Install the Python SDK and a compatible Vis executable separately."
                        "  A task can incur provider charges and access project files." ""])}
      {:title "DIRECT USE"
       :body (str/join
               "\n"
               ["  Set VIS_DB_PATH to an isolated, writable SQLite file path first."
                "  This command reads NDJSON requests from stdin and writes a protocol"
                "  handshake and NDJSON replies to stdout. Keep stdout free of other output."
                "  It exits on stdin EOF and neither starts nor stops an HTTP gateway."
                "  It is not an interactive shell or an MCP stdio server." ""])}
      {:title "DOCUMENTATION"
       :body "  https://vis.blockether.com/python-sdk.html#use-stdio-from-the-python-sdk"}]
     :cmd/run-fn
     (fn [_ _]
       (config/init-cli!)
       (when (str/blank? (System/getenv "VIS_DB_PATH"))
         (throw (ex-info
                  "stdio requires VIS_DB_PATH; use Agent() or LocalEngine() from the Python SDK"
                  {:vis/user-error true})))
       (try (gateway-stdio/serve!
              (java.io.BufferedReader.
                (java.io.InputStreamReader. System/in java.nio.charset.StandardCharsets/UTF_8))
              (java.io.OutputStreamWriter. config/original-stdout
                                           java.nio.charset.StandardCharsets/UTF_8)
              (gateway-server/local-handler))
            (finally (doseq [sid (gateway-state/session-ids)]
                       (gateway-state/release-session! sid))
                     (shutdown-agents))))} provider-cli/command session-cli/command
    workspace-cli/command
    {:cmd/name "doctor"
     :cmd/doc "Run cross-extension diagnostics, and reclaim stale drafts / session journals."
     :cmd/usage "vis-agent doctor [--purge] [--dry-run] [--days N]"
     :cmd/args [{:name "purge"
                 :kind :flag
                 :type :boolean
                 :doc "Delete the stale drafts and session journals the report lists."}
                {:name "dry-run"
                 :kind :flag
                 :type :boolean
                 :doc "With --purge, list exactly what would be deleted and delete nothing."}
                {:name "days" :kind :flag :type :int :doc "Staleness cutoff in days (default 14)."}]
     :cmd/examples ["vis-agent doctor" "vis-agent doctor --purge --dry-run"
                    "vis-agent doctor --purge --days 30"]
     :cmd/run-fn cli-doctor!} speech-cli/command decisions-cli/command
    {:cmd/name "extension"
     :cmd/doc "Inspect or run an extension-contributed CLI command."
     :cmd/usage "vis-agent extension <list|install|sync|versions|update|rollback|...> [args...]"
     :cmd/subcommands #(registry/registered-under ["extension"])} gateway-cli/command
    {:cmd/name "python"
     :cmd/doc
     "Run embedded Python, or pass commands unchanged to bundled uv: vis-agent python uv [ARGS...]"
     :cmd/usage "vis-agent python [OPTS] [-c CODE | -m MODULE | FILE.py | -] [ARG...]"
     :cmd/examples
     ["vis-agent python --shared -c \"import requests; print(requests.__version__)\""
      "vis-agent python --shared -m pip install requests   # shared sandbox packages"
      "vis-agent python --shared uv sync --all-groups --all-extras   # shared project dependencies"
      "vis-agent python uv sync --project ./einmal --locked   # prepare extension dependencies"
      "vis-agent python -m pytest tests/ -q   # module run as __main__"
      "vis-agent python -m pytest tests/   # src layout inferred from project metadata"
      "PYTHONPATH=extra vis-agent python -m pytest tests/   # merged with inferred roots"
      "vis-agent python script.py --flag foo   # ARGs land in sys.argv"
      "vis-agent python -c \"import os; print(os.environ['HOME'])\"   # env inherited"
      "vis-agent python --no-env -c \"import os; print(dict(os.environ))\"   # scrubbed"
      "vis-agent python --env FOO=bar -c \"import os; print(os.environ['FOO'])\""
      "echo 'print(1 + 1)' | vis-agent python" "vis-agent python   # interactive REPL"]
     :cmd/owns-tty? true
     :cmd/run-fn cli-python!}]]
  (registry/register-cmd! spec))

;;; ── Domain-owned subcommands ──────────────────────────────────────────────────

(doseq [spec (concat gateway-cli/subcommands
                     mcp-cli/subcommands
                     provider-cli/subcommands
                     session-cli/subcommands
                     workspace-cli/subcommands)]
  (registry/register-cmd! spec))

;;; ── `vis-agent extension` subcommand (host-owned canonical) ─────────────────────────

(defn- package-directory
  [project?]
  (str (io/file (System/getProperty (if project? "user.dir" "user.home")) ".vis" "extensions")))

(defn- print-package-result
  [action result]
  (commandline/stdout! (str action
                            " " (or (get result "repository") (get result "name"))
                            "@" (get result "version")
                            " (" (get result "mode")
                            "). " (get result "next")))
  result)

(registry/register-cmd! {:cmd/name "list"
                         :cmd/parent ["extension"]
                         :cmd/internal? true
                         :cmd/doc "List every registered extension with metadata."
                         :cmd/usage "vis-agent extension list"
                         :cmd/run-fn cli-extensions!})

(registry/register-cmd!
  {:cmd/name "install"
   :cmd/parent ["extension"]
   :cmd/internal? true
   :cmd/doc "Install an approved GitHub release, an explicit commit or a local Python project."
   :cmd/usage
   "vis-agent extension install SOURCE --trust [--save] [--subdirectory PATH] [--version VERSION | --revision SHA] [--project | --global]"
   :cmd/args
   [{:name "source"
     :kind :positional
     :type :string
     :required true
     :doc "Catalog identifier owner/repository[/folder], HTTPS URL, pyproject.toml or directory."}
    {:name "trust"
     :kind :flag
     :type :boolean
     :doc "Allow this package and its build backend to run with your permissions."}
    {:name "subdirectory"
     :kind :flag
     :type :string
     :doc "Project folder when the source names the repository only; defaults to its root."}
    {:name "revision"
     :kind :flag
     :type :string
     :doc "Reviewed full Git commit SHA for a GitHub install."}
    {:name "version"
     :kind :flag
     :type :string
     :doc "Approved release version; defaults to the latest approved stable release."}
    {:name "project" :kind :flag :type :boolean :doc "Install for this project only."}
    {:name "global" :kind :flag :type :boolean :doc "Install for all projects (the default)."}
    {:name "save"
     :kind :flag
     :type :boolean
     :doc "Save the installed package in configuration for extension sync."}]
   :cmd/run-fn (fn [{:strs [source trust subdirectory revision version project global save]} _]
                 (when (and project global)
                   (throw (ex-info "Choose --project or --global, not both" {})))
                 (print-package-result "Installed"
                                       (python-extensions/install-package!
                                         source
                                         {:trust trust
                                          :subdirectory (or subdirectory "")
                                          :revision revision
                                          :version version
                                          :save (boolean save)
                                          :project (boolean project)
                                          :directory (package-directory project)})))})

(registry/register-cmd!
  {:cmd/name "sync"
   :cmd/parent ["extension"]
   :cmd/internal? true
   :cmd/doc
   "Reconcile global and project YAML packages; reuse pinned sources and ready environments."
   :cmd/usage
   "vis-agent extension sync --trust [--project | --global] [--refresh] [--prune] [--dry-run]"
   :cmd/args
   [{:name "trust"
     :kind :flag
     :type :boolean
     :doc "Allow reviewed packages and their build backends to run."}
    {:name "project" :kind :flag :type :boolean :doc "Sync only this project's declarations."}
    {:name "global" :kind :flag :type :boolean :doc "Sync only global declarations."}
    {:name "refresh"
     :kind :flag
     :type :boolean
     :doc "Recheck approved releases for unpinned declarations."}
    {:name "prune"
     :kind :flag
     :type :boolean
     :doc "Remove only sync-owned links absent from configuration; retain all source."}
    {:name "dry-run"
     :kind :flag
     :type :boolean
     :doc "Show source actions without network, writes or dependency preparation."}]
   :cmd/run-fn
   (fn [{:strs [trust project global refresh prune dry-run]} _]
     (let [started
           (System/nanoTime)

           results
           (python-extensions/sync-packages! {:trust trust
                                              :project project
                                              :global global
                                              :refresh refresh
                                              :prune prune
                                              :dry-run dry-run})

           failed
           (count (filter #(= "failed" (get % "status")) results))]

       (doseq [result results]
         (commandline/stdout! (str (get result "scope")
                                   "  "
                                   (get result "name")
                                   "  "
                                   (get result "status")
                                   (when-let [version (get result "version")]
                                     (str "  " version))
                                   (when-let [error (get result "error")]
                                     (str " — " error)))))
       (commandline/stdout!
         (str "Synced " (count results)
              " package(s) in " (quot (- (System/nanoTime) started) 1000000)
              " ms; " failed
              " failed." (when-not dry-run
                           " Run /reload to use prepared extensions in existing sessions.")))
       (when (pos? failed)
         (throw (ex-info "Extension sync did not complete for every package" {:failed failed})))
       results))})

(registry/register-cmd!
  {:cmd/name "versions"
   :cmd/parent ["extension"]
   :cmd/internal? true
   :cmd/doc "List approved versions and check whether an installed extension has an update."
   :cmd/usage "vis-agent extension versions SOURCE [--subdirectory PATH] [--project]"
   :cmd/args [{:name "source"
               :kind :positional
               :type :string
               :required true
               :doc "Catalog identifier owner/repository[/folder] or HTTPS repository URL."}
              {:name "subdirectory"
               :kind :flag
               :type :string
               :doc "Project folder; omitted uses the sole installed project or repository root."}
              {:name "project" :kind :flag :type :boolean :doc "Check the project installation."}]
   :cmd/run-fn
   (fn [{:strs [source subdirectory project]} _]
     (let [result (python-extensions/package-versions source
                                                      {:subdirectory subdirectory
                                                       :directory (package-directory project)})]
       (when-let [installed (get result "installed")]
         (commandline/stdout! (str "Installed: " installed
                                   ". " (if (get result "update_available")
                                          "Update available."
                                          "No newer stable release."))))
       (commandline/stdout! (str "Latest approved stable: " (or (get result "latest") "none")))
       (doseq [release (get result "releases")]
         (commandline/stdout! (str (get release "version")
                                   "  "
                                   (get release "revision")
                                   (when (get release "prerelease") "  prerelease"))))
       result))})

(doseq [[command action operation] [["update" "Selected" #'python-extensions/update-package!]
                                    ["rollback" "Restored" #'python-extensions/rollback-package!]]]
  (registry/register-cmd!
    {:cmd/name command
     :cmd/parent ["extension"]
     :cmd/internal? true
     :cmd/doc (if (= command "update")
                "Explicitly select a newer approved extension release. Never updates automatically."
                "Restore the previous pinned source or choose an older approved release.")
     :cmd/usage (str "vis-agent extension "
                     command
                     " SOURCE --trust [--subdirectory PATH] [--version VERSION] [--project]")
     :cmd/args
     [{:name "source"
       :kind :positional
       :type :string
       :required true
       :doc "Catalog identifier owner/repository[/folder] or HTTPS URL of an installation."}
      {:name "subdirectory"
       :kind :flag
       :type :string
       :doc "Project folder; required when several extensions from this repository are installed."}
      {:name "trust"
       :kind :flag
       :type :boolean
       :doc "Allow the reviewed source and build backend to run with your permissions."}
      {:name "version" :kind :flag :type :string :doc "Select an approved release explicitly."}
      {:name "project" :kind :flag :type :boolean :doc "Change the project installation only."}]
     :cmd/run-fn (fn [{:strs [source trust subdirectory version project]} _]
                   (print-package-result action
                                         (operation source
                                                    {:trust trust
                                                     :subdirectory subdirectory
                                                     :version version
                                                     :directory (package-directory project)})))}))

;; Dispatcher entry point (-main)

;; Logging routing
;;
;; Telemere ships with a `:default/console` handler that prints EVERY
;; signal to stdout. That fills the terminal with registration noise
;; before the user ever sees the help text -- painful UX for a CLI.
;;
 ;; Default behavior:
 ;;   - stdout stays clean
 ;;   - every signal is appended to the process role's timestamped log file
 ;;     (`tui-…`, `gateway-…`, or `vis-…` for short-lived CLI work)
 ;;
;; Pass `--debug` / `--verbose` / `-v` (or set `VIS_DEBUG=1`) to KEEP
;; the console handler in addition to the file handler.

(def ^:private debug-flags #{"--debug" "--verbose" "-v"})

(defn- debug-mode? [args] (or (some debug-flags args) (= "1" (System/getenv "VIS_DEBUG"))))

(defn- log-file-path [] (config/log-path))

(defn- log-role-for-args
  "Classify the one long-lived process surface. Embedded Python executes
   inside gateway; every short-lived command keeps the neutral `vis` role."
  [args]
  (if (= ["gateway" "start"] (vec (take 2 args))) "gateway" "vis"))

(defn- configure-logging!
  "Route Telemere signals: file handler always on, persistence-backed
   `:db` handler always on (so the loop's `tel/with-ctx+ {:db-info ...}`
   bindings land in the session_log table), and the
   `:default/console` handler is OFF by default - it was removed by
   `internal.extension.registry` at namespace load so boot-time registration
   logs never spray to stdout. We re-add it here only when `--debug`
   / `--verbose` / `-v` / `VIS_DEBUG=1` is set. Idempotent."
  [args]
  (let [debug?
        (debug-mode? args)

        path
        (log-file-path)]

    (config/route-svar-logs!)
    (tel/set-min-level! (if debug? :debug :info))
    ;; File handler ALWAYS on, so post-mortem reads always have data.
    (try (tel/add-handler! :file
                           (tel/handler:file (assoc config/diagnostic-log-options :path path))
                           {:min-level :debug})
         (catch Throwable _ nil))
    ;; Console handler: re-add only when the user asked for verbosity.
    ;; Boot-time noise is already gone (registry.clj removed it during
    ;; namespace load); this restores the stdout stream for debugging.
    (when (and debug? (not= "stdio" (first args)))
      (try (tel/add-handler! :default/console (tel/handler:console)) (catch Throwable _ nil)))
    ;; Persistence handler: scopes signals to the right DB rows via
    ;; `:db-info` / `:session-soul-id` / `:session-turn-id` /
    ;; `:iteration-id` carried in telemere `*ctx*`. Wrapped because
    ;; the persistence facade is loaded lazily; if no backend has
    ;; registered yet, the handler will silently drop signals until
    ;; one does.
    (try (setup-db-handler!) (catch Throwable _ nil))))

(defn- initialize-clojure-extensions! [] (manifest/initialize!) nil)

(defn initialize-all!
  "Initialize the closed Clojure manifest, then project-local Python extensions."
  []
  (initialize-clojure-extensions!)
  (python-extensions/load-python-extensions!)
  nil)

(defn- deferred-python-dispatch?
  "Defer gateway Python loading and keep declarative sync free of entrypoint imports."
  [args]
  (or (= "stdio" (first args))
      (contains? #{["gateway" "start"] ["gateway" "tui"] ["extension" "sync"]}
                 (vec (take 2 args)))))

;; Root command
;;
;; The dispatcher's root has NO hard-coded subcommands. Every entry
;; comes from the global commandline registry. Built-ins (providers,
;; sessions, doctor, ...) are registered by vis-runtime; the `vis-agent channel` and
;; `vis-agent extension` parents are registered by the channel and extension
;; facades. Add a third-party jar with its own `register-cmd!`
;; calls and its commands appear here without any code change.

(def ^:private HELP_COL
  "Column - relative to the two-space body indent - where every description in
   the root doc starts. ONE gutter for USAGE, ONE-SHOT FLAGS, RUNTIME and
   CONFIGURATION: per-section columns made the help screen look ragged."
  31)

(defn- help-row
  "`TOKEN<pad>Description` on the shared `HELP_COL` gutter. A token wider than
   the gutter still keeps one space, so nothing collides."
  [token doc]
  (let [pad (max 1 (- (long HELP_COL) (long (count token))))]
    (str token (str/join (repeat pad \space)) doc)))

(def ^:private DEFAULT_DOC
  "Root help body. Section headings sit at column 0 and rows at column 2 -
   `commandline/render-tree` renders this doc with the same geometry as the
   COMMANDS block it appends, so write rows WITHOUT a leading indent."
  (str/join
    "\n"
    ["Vis - a coding agent that edits, runs and verifies code in your repo, with a persistent sandboxed Python REPL."
     "" "USAGE" (help-row "vis-agent [FLAGS] \"prompt\"" "Run one-shot agent work.")
     (help-row "vis-agent <command> [args...]" "Run a command.")
     (help-row "vis-agent <command> --help" "Show command help.")
     (help-row "vis-agent [--help|--version]" "Show this help, or the version.") "" "ONE-SHOT FLAGS"
     (help-row "--json" "Print result as JSON.")
     (help-row "--code" "Print only final answer code blocks.")
     (help-row "--raw" "Print plain text, no markdown styling.")
     (help-row "--toggles NAME=VAL[,..]" "Set registered toggles for this run only.")
     (help-row "--full-trace-stream" "Stream pretty human trace.")
     (help-row "--full-trace-json-stream" "Stream raw JSON trace frames.")
     (help-row "--provider PROVIDER" "Override provider.")
     (help-row "--model MODEL" "Override model, or use provider/model.")
     (help-row "--reasoning-effort E" "Exact provider-native effort: high or max.")
     (help-row "--name NAME" "Agent name for this run.")
     (help-row "--db PATH|:memory" "SQLite DB path or in-memory DB.")
     (help-row "--session-id ID" "Continue an existing persisted session.")
     (help-row "--persist" "Persist as a :cli session.")
     (help-row "--debug, --verbose, -v" "Enable verbose debug logging.")
     (help-row "--" "End flags: every later word is prompt text.")
     (help-row "--help, -h" "Show help.") "" "RUNTIME"
     (help-row "--jvm" "Run on the JVM once, without changing the installed track.") ""
     "GATEWAY (WHICH DAEMON RUNS THE WORK)"
     (help-row "--gateway HOST[:PORT]|URL" "Drive another machine's gateway (VIS_GATEWAY_URL).")
     (help-row "--gateway-token TOKEN" "Bearer token that gateway requires (VIS_GATEWAY_TOKEN).") ""
     "DESKTOP APP"
     (help-row "vis-agent desktop" "Open the selected track: release download or dev source build.")
     (help-row "vis-agent desktop --update"
               "Check release updates, or rebuild the current dev source.")
     (help-row "vis-agent desktop --track dev"
               "Build from source once; leave the engine track unchanged.") "" "UPDATES"
     (help-row "vis-agent update" "Install the latest complete native release (default).")
     (help-row "--track release|beta|dev" "Release, green-CI native beta, or main on JVM.")
     (help-row "vis-agent switch list" "Show the installed build and every version you can select.")
     (help-row "vis-agent switch <identifier>"
               "Install and run that release, beta or dev build from now on.") "" "CONFIGURATION"
     (help-row "~/.vis/config.yml" "Global settings: providers, models, tools.")
     (help-row "<project>/vis.yml" "Project settings; .vis/config.yml overrides it.")
     (help-row "vis-agent providers status" "Show provider auth and model catalogs.")
     (help-row "vis-agent doctor" "Diagnose config, extensions, stale state.") "" "EXAMPLES"
     "vis-agent \"fix failing tests\"" "vis-agent --json \"summarize this repo\""
     "vis-agent --provider zai-coding-plan --model glm-5.2 --reasoning-effort high --json \"task\""
     "vis-agent --toggles reasoning_level=deep \"refactor carefully\""
     "vis-agent --full-trace-json-stream --db :memory \"debug startup\""
     "vis-agent sessions search sqlite"
     "vis-agent --gateway 10.0.0.5 --gateway-token TOKEN sessions list"]))

(defn root-command
  "Build the root `vis-agent` command tree. Subcommands are pulled fresh on
   every call so newly registered extensions show up immediately."
  []
  (registry/command
    {:cmd/name "vis-agent" :cmd/doc DEFAULT_DOC :cmd/subcommands #(registry/registered-under [])}))

;; Pre-redirect stderr for TTY-owning channels
;;
;; Some leaves (TUI, ncurses) take over the controlling terminal and
;; need stderr re-routed to a log file BEFORE any further class loading
;; triggers JVM warnings. The check is data-driven via
;; `:cmd/owns-tty?`. Channels mark themselves through the channel
;; bridge; nothing here is channel-aware.

(defn- pre-redirect-stderr!
  [args]
  (when-let [{:keys [command]} (commandline/find-leaf (root-command) (cons "vis-agent" args))]
    (when (:cmd/owns-tty? command)
      ;; This process's own log file — see `internal.paths/log-file`.
      (let [log-path (paths/log-file)]
        (System/setErr (java.io.PrintStream. (java.io.FileOutputStream. ^String log-path true)
                                             true))))))

;; Main

(defn- root-help-request?
  "True when args ask only for the root help screen. This path can skip
   distribution initialization because the root tree lists built-in parent commands
   only; extension-owned commands are mounted below `ext` after initialization."
  [args]
  (or (empty? args) (contains? #{["help"] ["--help"] ["-h"]} (vec args))))

(defn- version-request?
  "True when args ask only for the version. Like help, this short-circuits
   BEFORE distribution initialization / agent boot — `vis-agent --version` must be instant
   and must NOT create the Python sandbox or contact a provider."
  [args]
  (contains? #{["--version"] ["-V"] ["version"]} (vec args)))

(defn- vis-version
  "Vis version string: the `vis/VERSION` resource written at build time from the
   repo-root VIS_VERSION, verbatim (`0.1.28`), else \"dev\"."
  []
  (or (some-> (io/resource "vis/VERSION")
              slurp
              str/trim
              not-empty)
      "dev"))

(defn- help-request?
  "True when args request help at any command depth. We can usually render
   help without initializing runtime resources; if a command is not registered
   yet, the caller falls back to full distribution initialization."
  [args]
  (boolean (or (root-help-request? args) (some #{"--help" "-h"} args))))

(defn- channel-help-request?
  "True for `vis-agent channels <channel> --help`. Rendering a concrete channel
   requires the closed distribution to register its descriptor first."
  [args]
  (let [[parent channel & more] (vec args)]
    (and (= "channels" parent) (some? channel) (boolean (some #{"--help" "-h"} more)))))

(defn- channel-parent-help-request?
  "True for `vis-agent channels --help`. Rendering the parent has to load
   channel-providing extensions first; otherwise the dynamic `channels`
   subtree is empty and help cannot list the available channels."
  [args]
  (let [[parent & more]
        (vec args)

        help?
        (boolean (some #{"--help" "-h"} more))

        before-help
        (take-while #(not (#{"--help" "-h"} %)) more)]

    (and (= "channels" parent) help? (empty? before-help))))

(defn- ext-help-request?
  "True for any `vis-agent extension ...` help invocation. The subtree is
   populated by manifest initializers before command rendering."
  [args]
  (contains? #{"ext" "extension"} (first (vec args))))

(defn- initialize-fast-help-deps!
  [args]
  (when (or (channel-help-request? args)
            (channel-parent-help-request? args)
            (ext-help-request? args))
    (initialize-all!)))

(defn- fast-help-dispatched?
  [_measure? args]
  (when (help-request? args)
    (initialize-fast-help-deps! args)
    (let [root
          (root-command)

          full-args
          (cons "vis-agent" args)

          {:keys [residual]}
          (commandline/find-leaf root full-args)

          unresolved
          (take-while #(not (#{"--help" "-h"} %)) residual)]

      (when-not (seq unresolved)
        (let [{:keys [status]} (commandline/dispatch! root full-args)]
          (= :help status))))))

(defn- unknown-command?
  "True when the user typed something the tree doesn't recognize.
   Detected by walking the tree: if `find-leaf` resolves only to the
   ROOT (path length 1) AND there's residual input, the user gave us
   an unknown command. Pure root help is handled before this check."
  [root args]
  (when (seq args)
    (let [{:keys [path residual]} (commandline/find-leaf root (cons (:cmd/name root) args))]
      (and (= 1 (count path)) (seq residual)))))

(def ^:private wrapper-owned-commands
  "Words the `vis-agent` launcher implements ITSELF and never forwards. The
   engine advertises them in its own help (`vis-agent update`, `switch`, …) but
   owns none of them, so one reaching this binary means the caller ran the
   engine directly, or a launcher too old to know the word — and without this
   set it would become a PROMPT: `vis-agent upgrade` silently spent a turn
   asking a model about the word \"upgrade\" instead of updating anything."
  #{"update" "upgrade" "switch" "desktop" "runtime"})

(defn- wrapper-owned-invocation?
  "True for EXACTLY one bare wrapper word. Deliberately not for longer argument
   lists: `vis-agent update the readme` is an ordinary prompt that happens to
   start with one of these words, and stealing it would be the same class of
   surprise this guard exists to remove."
  [args]
  (and (= 1 (count args)) (contains? wrapper-owned-commands (first args))))

(defn- root-run-shortcut?
  "True when bare `vis-agent ...` should run the one-shot CLI agent.
   Unknown commands that ask for help stay errors, so typo diagnostics
   remain honest (`vis-agent sessions --help` must not become a prompt), and a
   lone wrapper-owned word is an invocation mistake, never a question."
  [root args]
  (and (unknown-command? root args)
       (not (wrapper-owned-invocation? args))
       (not-any? #{"--help" "-h"} args)))

(defn- exit-with-user-error!
  [^Throwable t]
  ;; Some user errors ship a pre-rendered SCREEN (`:vis/panel`) — a boxed panel
  ;; naming what is wrong and the exact fix (e.g. a gateway/client version
  ;; mismatch). Print that instead of flattening it into one line.
  (if-let [panel (seq (:vis/panel (ex-data t)))]
    (doseq [line panel]
      (commandline/stdout! (str line)))
    (commandline/stdout! (str "vis-agent: " (or (ex-message t) "error"))))
  (shutdown-agents)
  (System/exit 2))

(defn- root-cause
  ^Throwable [^Throwable t]
  (loop [c t]
    (if-let [n (.getCause c)]
      (recur n)
      c)))

(defn- user-error-ex
  "First throwable in the cause chain that is a caller-facing error.
   Wrapping (futures, class init, `Compiler$CompilerException`) must not
   demote a user error — e.g. an invalid `config.yml` — into a stack trace."
  ^Throwable [^Throwable t]
  (loop [c t]
    (cond (nil? c) nil
          (:vis/user-error (ex-data c)) c
          :else (recur (.getCause c)))))

(defn- exit-with-fatal-error!
  [^Throwable t]
  (let [rc
        (root-cause t)

        same?
        (identical? rc t)]

    (commandline/stdout! (str "vis-agent: fatal error - " (or (ex-message t) (.getName (class t)))))
    ;; ExceptionInInitializerError etc. carry no message; surface the root cause
    ;; so failures (incl. native-image runtime class-init) are diagnosable.
    (when-not same?
      (commandline/stdout! (str "  caused by: "
                                (.getName (class rc))
                                (when-let [m (ex-message rc)]
                                  (str ": " m)))))
    ;; full trace when VIS_DEBUG is set — invaluable for native-image triage
    (when (some-> (System/getenv "VIS_DEBUG")
                  (.equalsIgnoreCase "1"))
      (.printStackTrace t))
    (commandline/stdout! (str "See " (config/log-path) " for details.")))
  (shutdown-agents)
  (System/exit 1))

(defn- exit-no-provider!
  "Calm, guided message when no AI provider is configured — never a stacktrace.
   Points at the interactive welcome (the curated, zero-friction path)."
  []
  (commandline/stdout! "")
  (commandline/stdout! "  vis-agent needs an AI provider to get started.")
  (commandline/stdout! "")
  (commandline/stdout! "  ▸ Run  vis-agent  with no arguments to open the welcome screen and")
  (commandline/stdout! "    connect one (Sign in with GitHub / OpenAI / Anthropic, paste an")
  (commandline/stdout! "    API key, or run a local model).")
  (commandline/stdout! "  ▸ Or hand-write ~/.vis/config.yml.")
  (commandline/stdout! "")
  (shutdown-agents)
  (System/exit 2))

(defn- truthy-value? [v] (contains? #{"1" "true" "yes" "on"} (str/lower-case (str v))))

(defn- measure-arg? [arg] (= "--measure" arg))

(def ^:private launcher-owned-args
  ;; `bin/vis-agent` normally consumes these before invoking Clojure, but keep
  ;; the JVM entry point tolerant too (e.g. `clojure -M:vis-agent channels --jfr --help`).
  ;; Runtime selection belongs to the launcher, not the engine.
  #{"--jfr" "--stream-trace"})

(defn- global-arg? [arg] (or (measure-arg? arg) (contains? launcher-owned-args arg)))

(defn- strip-global-args [args] (vec (remove global-arg? args)))

(def ^:private gateway-flags
  ;; Root flags that aim EVERY gateway call at a daemon this machine does not
  ;; manage. They are consumed here, ahead of the command tree, because they are
  ;; not one command's option: they decide WHICH gateway the whole invocation
  ;; drives — `vis-agent --gateway 10.0.0.5 tui`, `--gateway ... sessions list`.
  {"--gateway" :url "--gateway-token" :token})

(defn- split-gateway-flags
  "Split `--gateway URL` / `--gateway-token TOKEN` (space- or `=`-joined) out of
   `args`. Returns `{:gateway {:url :token} :args [...]}` with `:gateway` nil when
   neither appears. Parsing stops at a bare `--`, so prompt text is never eaten."
  [args]
  (loop [remaining
         (seq args)

         kept
         (transient [])

         gateway
         nil]

    (let [arg (first remaining)]
      (cond (nil? remaining) {:gateway gateway :args (persistent! kept)}
            (= "--" arg) {:gateway gateway :args (into (persistent! kept) remaining)}
            :else (let [[flag inline] (str/split (str arg) #"=" 2)
                        k (get gateway-flags flag)]

                    (cond (nil? k) (recur (next remaining) (conj! kept arg) gateway)
                          inline (recur (next remaining) kept (assoc gateway k inline))
                          :else
                          (recur (nnext remaining) kept (assoc gateway k (fnext remaining)))))))))

(defn- connect-gateway!
  "Point every gateway call of this invocation at the `--gateway` target. A missing
   address is a user error rather than a silent fall back to the local daemon —
   falling back would run the work on the wrong machine."
  [{:keys [url token]}]
  (when (str/blank? (str url))
    (throw (ex-info (str "--gateway needs a gateway address: HOST, HOST:PORT or "
                         "http(s)://HOST[:PORT] (or set VIS_GATEWAY_URL).")
                    {:vis/user-error true})))
  (gateway-client/connect-remote! {:url url :token token}))

(defn- rewrite-ext-alias
  "Back-compat: rewrite a leading `ext` into the canonical `extension`
   command so existing `vis-agent ext ...` invocations keep working."
  [args]
  (if (= "ext" (first args)) (into ["extension"] (rest args)) (vec args)))

(defn- startup-measure?
  [args]
  (or (some measure-arg? args)
      (truthy-value? (System/getenv "VIS_MEASURE"))
      (truthy-value? (System/getProperty "vis.measure"))))

(defn- elapsed-ms [^long started-ns] (/ (double (- (System/nanoTime) started-ns)) 1000000.0))

(defn- format-ms [^double ms] (String/format java.util.Locale/ROOT "%.1f ms" (object-array [ms])))

(defn- startup-measure-line!
  [label & kvs]
  (binding [*out* *err*]
    (println (str "[vis-agent measure] jvm:"
                  label
                  (when (seq kvs) (str " " (str/join " " (map str kvs))))))))

(defn- timed-startup!
  [measure? label f]
  (if measure?
    (let [started (System/nanoTime)]
      (try (f) (finally (startup-measure-line! label (format-ms (elapsed-ms started))))))
    (f)))

(defn- initialize-for-dispatch!
  "Initialize the closed manifest before dispatch. Long-lived TUI and gateway
   processes leave Python to the gateway's on-demand execution boundary; one-shot
   commands stay eager so their local extension surfaces are complete."
  [measure? args]
  (timed-startup! measure? "initialize-manifest" #(initialize-clojure-extensions!))
  (when-not (deferred-python-dispatch? args)
    (timed-startup! measure? "load-python-extensions" #(python-extensions/load-python-extensions!)))
  nil)

(defn- summarize-startup-registries!
  []
  (let [extensions
        (extension/registered-extensions)

        channels
        (registry/registered-channels)

        providers
        (registry/registered-providers)

        commands
        (registry/registered-commands)]

    (startup-measure-line! "registry totals"
                           (str "extensions=" (count extensions))
                           (str "channels=" (count channels))
                           (str "providers=" (count providers))
                           (str "commands=" (count commands)))
    (doseq [ext extensions]
      (startup-measure-line!
        "extension"
        (str "ns=" (:ext/name ext))
        (str "kind=" (or (:ext/kind ext) "uncategorized"))
        (str "channels=" (str/join "," (map :channel/cmd (:ext/channels ext))))
        (str "providers=" (str/join "," (map (comp name :provider/id) (:ext/providers ext))))))
    (doseq [channel channels]
      (startup-measure-line! "channel"
                             (str "id=" (:channel/id channel))
                             (str "cmd=" (:channel/cmd channel))
                             (str "owns-tty=" (boolean (:channel/owns-tty? channel)))))
    (doseq [provider providers]
      (startup-measure-line! "provider"
                             (str "id=" (:provider/id provider))
                             (str "label=" (pr-str (:provider/label provider)))))))

(defn -main
  "Initialize the closed distribution, walk the command tree, dispatch.

   Behavior:
     - No args                  -> top-level help
     - `help` / `--help` / `-h` -> help for the resolved command
     - Recognized command       -> invoke its `:cmd/run-fn`
     - Bare prompt / run flags  -> one-shot CLI agent
     - Unknown command + help   -> honest unknown-command error

   Root prompt shortcut lives here, not in `commandline/dispatch!`, so
   the generic dispatcher stays a pure command tree while the binary owns
   CLI ergonomics (`vis-agent fix this`, `vis-agent --json summarize`)."
  [& raw-args]
  ;; uv owns every following token, including help, version and child-program flags.
  (when (= ["python" "uv"] (take 2 raw-args))
    (System/exit (python-runtime/uv-command! (drop 2 raw-args))))
  (when (= ["python" "--shared" "uv"] (take 3 raw-args))
    (System/exit (try (python-runtime/uv-command! (drop 3 raw-args) {:shared? true})
                      (catch Throwable t (commandline/stderr! (.getMessage t)) 1))))
  (system-trust/install!)
  (let [main-started
        (System/nanoTime)

        measure?
        (startup-measure? raw-args)

        {gateway :gateway stripped :args}
        (split-gateway-flags (strip-global-args raw-args))

        args
        (rewrite-ext-alias stripped)]

    (paths/set-log-role! (log-role-for-args args))
    (when measure? (System/setProperty "vis.measure" "1"))
    ;; Opt-in JFR profiling (VIS_JFR set by `bin/vis-agent --jfr`). Role-tagged so a
    ;; spawned gateway daemon (`vis-agent gateway start`) records to its OWN file,
    ;; separate from this client's — see internal.jfr.
    (try ((requiring-resolve 'com.blockether.vis.internal.jfr/maybe-start!)
           (if (= "gateway" (first args)) "gateway" "client"))
         (catch Throwable _ nil))
    (try
      ;; `--gateway` decides WHICH daemon this invocation drives, so it is applied
      ;; before anything can reach for one.
      (when gateway (connect-gateway! gateway))
      ;; Quiet stdout BEFORE any extension load triggers Telemere registration
      ;; spam - the user only sees logs when they pass --debug / --verbose / -v
      ;; (or set VIS_DEBUG=1).
      (timed-startup! measure? "configure-logging" #(configure-logging! args))
      ;; Stale-state sweep: `~/.vis/logs` gains a file per nrepl/JFR start and a
      ;; directory per `shell` command, the display caches a file per rendered
      ;; picture, `~/.vis/rewind` a store per session — and nothing ever removed
      ;; one. Off-thread and best-effort — see `housekeeping/sweep-stale!` for
      ;; the windows and the guards.
      (try (housekeeping/sweep-stale-async!) (catch Throwable _ nil))
      (cond (version-request? args) (println (str "vis-agent " (vis-version)))
            (root-help-request? args) (println (commandline/render-tree (root-command)))
            (fast-help-dispatched? measure? args) nil
            :else (do (initialize-for-dispatch! measure? args)
                      (when measure? (summarize-startup-registries!))
                      (timed-startup! measure? "pre-redirect-stderr" #(pre-redirect-stderr! args))
                      (let [root
                            (root-command)

                            full-args
                            (cons "vis-agent" args)

                            unknown-root?
                            (unknown-command? root args)]

                        (cond (wrapper-owned-invocation? args)
                              (do (println (str "vis-agent: `" (first args)
                                                "` is handled by the vis-agent command itself, "
                                                "not by the engine."))
                                  (println (str "Run it through the launcher on your PATH: "
                                                "vis-agent "
                                                (first args)))
                                  (System/exit 2))
                              (and unknown-root? (root-run-shortcut? root args))
                              (timed-startup! measure? "run-shortcut" #(cli-run! {} args))
                              unknown-root? (do (println (commandline/render-tree root))
                                                (println)
                                                (println (str "Unknown command: "
                                                              (str/join " " args)))
                                                (System/exit 1))
                              :else
                              ;; `dispatch!` returns `{:status :ok|:help|:error|:no-match ...}`.
                              ;; `:error` covers spec-validation failures (missing required
                              ;; args, unknown flags). Without an explicit `System/exit 1` here
                              ;; the process exited 0 even though the user-visible output was
                              ;; an error message + help text -- so any shell pipeline like
                              ;; `vis-agent foo --bogus && echo ok` printed `ok`. Map `:error` to
                              ;; exit code 2 (POSIX convention for usage errors); `:no-match`
                              ;; can't actually fire here because `unknown-command?` above
                              ;; already short-circuited that case.
                              (let [{:keys [status]} (timed-startup!
                                                       measure?
                                                       "dispatch"
                                                       #(commandline/dispatch! root full-args))]
                                (case status
                                  :error
                                  (System/exit 2)

                                  ;; Success path: force a deterministic process exit.
                                  ;; Python extension loading can spin up CPython and extension
                                  ;; executors, some of which leave NON-daemon threads alive; a
                                  ;; bare `nil` return let `-main` finish while those threads
                                  ;; kept the JVM (and the native isolate) running, so a
                                  ;; one-shot command like `vis-agent sessions export` printed its
                                  ;; output and then HUNG the terminal forever. Draining agents
                                  ;; and calling `System/exit 0` guarantees termination.
                                  (do (shutdown-agents) (System/exit 0))))))))
      (catch Throwable t
        (cond (config/no-provider-ex t) (exit-no-provider!)
              (user-error-ex t) (exit-with-user-error! (user-error-ex t))
              :else (exit-with-fatal-error! t)))
      (finally (when measure?
                 (startup-measure-line! "main total" (format-ms (elapsed-ms main-started))))))))
