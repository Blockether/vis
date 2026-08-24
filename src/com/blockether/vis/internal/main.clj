(ns com.blockether.vis.internal.main
  "vis-agent CLI binary - :db Telemere handler, one-shot agent helper,
   built-in CLI commands, and the `-main` dispatcher entry point.

   Everything in this file is binary-only. The library surface
   (iteration loop, turn engine, environment lifecycle, session
   cache) lives in `com.blockether.vis.internal.loop`; this namespace requires
   that one and wires it into the command tree the `vis-agent` wrapper
   exposes.

   Public entry point:

     (-main & args)   - invoked by the `:vis` alias / `bin/vis-agent`.
                       Configures logging, discovers Clojure extensions, loads Python
                       extensions before one-shot dispatches, redirects stderr to this
                       process's role/start-time/pid-stamped file under `~/.vis/logs/` for
                       any TTY-owning channel, then dispatches to the resolved
                       command's `:cmd/run-fn`.

   Built-in commands registered here:
     vis-agent providers          - provider inspection, auth, and limits
     vis-agent sessions      - list persisted sessions
     vis-agent projects           - list projects, or delete one with its sessions
     vis-agent extension list     - list registered extensions
     vis-agent tui                - interactive terminal UI (alias for `channels tui`)
     vis-agent channels <name>    - auto-mounted via the channel registry

   `vis-agent doctor` is host-owned. Extensions plug diagnostics into it
   with `:ext/doctor-fn`; extension-owned CLI commands stay under
   `vis-agent extension`."
  (:refer-clojure :exclude [agent run!])
  (:require [babashka.process :as process]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.cancellation :as cancellation]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.doctor :as doctor]
            [com.blockether.vis.internal.foundation.housekeeping :as housekeeping]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.error :as error]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.python-extensions :as python-extensions]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.state :as gateway-state]
            [com.blockether.vis.internal.manifest :as manifest]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.python-project :as pyproj]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.progress :as progress]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.system-trust :as system-trust]
            [com.blockether.vis.internal.toggles :as toggles]
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

(def ^:private ^String ext-ns-prefix "com.blockether.vis.ext.")

(defn- short-ext-ns
  "Render an extension namespace symbol with the `v/` prefix instead of
   the canonical `com.blockether.vis.ext.` package, so the table column
   stays narrow:

     com.blockether.vis.internal.foundation.core      -> v/foundation.core
     com.blockether.vis.ext.provider-github-copilot -> v/provider-github-copilot

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
     - persistance -> joined `:persistance/id` names
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

    "persistance"
    (->> (:ext/persistance e)
         (keep (comp name :persistance/id))
         (str/join ", "))

    ""))

(defn list-extensions
  "Return all registered extensions with their metadata (table rows).

   `:namespace` is shortened with the `v/` prefix (see
   `short-ext-ns`). `:kind` carries the categorical bucket
   (`providers`, `channels`, `foundation`, ...) used to render the
   table in grouped sections. `:group` is a finer label *inside* the
   kind (provider label / channel cmd / persistance id), blank for
   kinds that don't have one. `:author`, `:owner`, and `:license` come
   straight from the extension manifest; `:owner` identifies the
   package's distribution (\"vis\" for everything bundled here), and `:license` carries the SPDX
   identifier (e.g. `Apache-2.0`)."
  []
  (mapv (fn [e]
          {:namespace (short-ext-ns (:ext/name e))
           :doc (:ext/description e)
           :kind (or (:ext/kind e) "uncategorized")
           :group (per-kind-group e)
           :author (or (:ext/author e) "-")
           :owner (or (:ext/owner e) "-")
           :license (or (:ext/license e) "-")
           :version (or (:ext/version e) "-")})
        (extension/registered-extensions)))

(defn find-extension-cmd
  "Find an extension CLI command by name. Returns {:ext ext :cmd cmd-map} or nil."
  [cmd-name]
  (some (fn [e]
          (some (fn [cmd]
                  (when (= (:cmd cmd) cmd-name) {:ext e :cmd cmd}))
                (:ext/cli e)))
        (extension/registered-extensions)))

(defn all-extension-cmds
  "Return a flat vec of {:cmd :doc :ext-ns :args} for every registered extension CLI command."
  []
  (into []
        (mapcat (fn [e]
                  (map (fn [c]
                         (assoc c :ext-ns (str (:ext/name e))))
                       (or (:ext/cli e) []))))
        (extension/registered-extensions)))

;;; ── Arg parsing & validation ───────────────────────────────────────────

(defn- flag-arg? [s] (str/starts-with? (str s) "--"))

(defn- coerce-arg
  [value type]
  (case (or type :string)
    :string
    value

    :int
    (if-let [n (parse-long value)]
      n
      (throw (ex-info (str "Expected integer, got: " value) {:value value})))

    :boolean
    (contains? #{"true" "1" "yes"} (str/lower-case (str value)))

    :file
    value

    value))

(defn parse-ext-args
  "Parse CLI args against an arg spec. Returns a map of {arg-name value}.

   :kind :positional args are matched in declaration order.
   :kind :flag args are matched by --name. Boolean flags need no value."
  [arg-specs raw-args]
  (let [positional
        (vec (filter #(= :positional (:kind %)) arg-specs))

        flags
        (into {}
              (map (fn [a]
                     [(:name a) a]))
              (filter #(= :flag (:kind %)) arg-specs))]

    (loop [args
           (seq raw-args)

           pos-idx
           0

           result
           {}]

      (if-not args
        result
        (let [arg
              (first args)

              more
              (next args)]

          (if (flag-arg? arg)
            ;; Flag
            (if-let [spec (get flags arg)]
              (if (= :boolean (:type spec))
                (recur more pos-idx (assoc result (:name spec) true))
                (recur (next more)
                       pos-idx
                       (assoc result (:name spec) (coerce-arg (first more) (:type spec)))))
              (recur more pos-idx result))
            (if (< pos-idx (count positional))
              (let [spec (nth positional pos-idx)]
                (recur more
                       (inc pos-idx)
                       (assoc result (:name spec) (coerce-arg arg (:type spec)))))
              (recur more pos-idx result))))))))

(defn validate-ext-args
  "Validate parsed args against spec. Returns nil on success, error string on failure."
  [arg-specs parsed]
  (let [required
        (filter :required arg-specs)

        missing
        (remove #(contains? parsed (:name %)) required)]

    (when (seq missing)
      (str "Missing required argument(s): " (str/join ", " (map :name missing))))))

;;; ── Help rendering ─────────────────────────────────────────────────────

(def ^:private pad
  "Right-pad to width — `commandline/pad-right` (was a second copy here)."
  commandline/pad-right)

(defn format-cmd-help
  "Build help text for a single extension CLI command."
  [{:keys [cmd doc args ext-ns]}]
  (let [positional
        (filter #(= :positional (:kind %)) (or args []))

        flags
        (filter #(= :flag (:kind %)) (or args []))

        usage-pos
        (str/join " "
                  (map (fn [{:keys [name required]}]
                         (if required (str "<" name ">") (str "[" name "]")))
                       positional))

        usage-flags
        (when (seq flags) "[flags]")

        usage
        (str/join " " (remove nil? [usage-pos usage-flags]))

        fmt-arg
        (fn [{:keys [name type required doc]}]
          (str "    "
               (pad name 20)
               (pad (or (some-> type
                                clojure.core/name)
                        "string")
                    10)
               (if required "required  " "optional  ")
               (or doc "")))]

    (str "  vis-agent extension "
         cmd
         (when (seq usage) (str " " usage))
         "\n\n  "
         (or doc "")
         (when ext-ns (str "\n  Extension: " ext-ns))
         (when (seq positional)
           (str "\n\n  Positional arguments:\n" (str/join "\n" (map fmt-arg positional))))
         (when (seq flags) (str "\n\n  Flags:\n" (str/join "\n" (map fmt-arg flags)))))))

(defn extension-help
  []
  (let [cmds (all-extension-cmds)]
    (if (empty? cmds)
      "No extension commands available. Run 'vis-agent extension' to see registered extensions."
      (str "Extension commands:\n\n"
           (str/join "\n\n"
                     (map
                       (fn [{:keys [cmd doc ext-ns]}]
                         (str "  vis-agent extension " (pad cmd 20) (or doc "") "  (" ext-ns ")"))
                       cmds))))))

;;; ── Dispatch ───────────────────────────────────────────────────────────

(defn run-extension-cmd!
  "Parse args, validate, and run an extension CLI command.
   Returns {:ok result} or {:error message}."
  [cmd-name raw-args]
  (if-let [{:keys [cmd]} (find-extension-cmd cmd-name)]
    (let [arg-specs (or (:args cmd) [])
          ;; --help on any command
          help? (some #{"--help" "-h"} raw-args)]

      (if help?
        {:help (format-cmd-help (assoc cmd :ext-ns (:ext-ns cmd)))}
        (let [parsed (parse-ext-args arg-specs raw-args)
              err (validate-ext-args arg-specs parsed)]

          (if err {:error (str err "\n\n" (format-cmd-help cmd))} {:ok ((:fn cmd) parsed)}))))
    {:error (str "Unknown command: " cmd-name "\n\n" (extension-help))}))

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
    ;; Honor `:router` block in caller's config (rate-limit/network/budget).
    ;; Single-arity make-router would silently use svar defaults.
    (svar/make-router (mapv config/->svar-provider (:providers config)) (config/router-opts config))
    (lp/get-router)))

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
   - :reasoning-effort - Exact provider-native effort (`high` or `max`)
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

          mdl
          (assoc :model mdl)

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
      (let [env (lp/create-environment (router-for-run cfg local-router?)
                                       {:db (or db :memory) :channel :cli})]
        (try (let [result (lp/turn! env messages q-opts)]
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
             (finally (try (lp/dispose-environment! env) (catch Exception _ nil)))))
      ;; Persistent path: route through the canonical in-process gateway so
      ;; CLI, TUI, web, and transport clients share the same session/turn
      ;; machinery.
      (let [_
            (when local-router? (lp/rebuild-router! cfg))

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
            (or resolved-session-id (:id created-session))]

        (try (let [result (gateway-state/submit-turn-sync!
                            session-id
                            {:request prompt-s :messages messages :engine-opts q-opts})]
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

;;; ── Output helpers ──────────────────────────────────────────────────────

(defn- stdout!
  "Print to the real terminal via the saved original stdout. Other
   output (telemere, SLF4J) is redirected to the log file."
  [^String s]
  (.println ^java.io.PrintStream config/original-stdout s)
  (.flush ^java.io.PrintStream config/original-stdout))

(defn- stderr!
  "Print a diagnostic to the process's real stderr."
  [^String s]
  (.println ^java.io.PrintStream config/original-stderr s)
  (.flush ^java.io.PrintStream config/original-stderr))

(defn- write-stdout!
  "Write to the real terminal without appending a newline. Used by the
   live trace renderer for cursor-back/redraw frames."
  [^String s]
  (.print ^java.io.PrintStream config/original-stdout s)
  (.flush ^java.io.PrintStream config/original-stdout))

(declare terminal-width)

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
  (stdout! (json/write-json-str (json-safe (trace-safe {:event event :payload payload})))))

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

(defn- envelope-duration-ms
  [envelope]
  (when (and (map? envelope)
             (nat-int? (:started-at-ms envelope))
             (nat-int? (:finished-at-ms envelope)))
    (max 0 (- (long (:finished-at-ms envelope)) (long (:started-at-ms envelope))))))

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
          (max 40 (- (long (terminal-width)) 4))

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
      (stdout! (str head
                    (trace-title "↗" "provider call")
                    (when-let [t (:started-at-ms chunk)]
                      (str " " (trace-dim (str "started=" t))))))

      :provider-fallback
      (stdout! (str head
                    (trace-warn "↷ provider fallback")
                    " "
                    (or (:failed-provider chunk) "?")
                    " → "
                    (or (:new-provider chunk) "?")
                    (when-let [reason (:reason chunk)]
                      (str " " (trace-dim (str "(" reason ")"))))))

      :provider-retry-reset
      (stdout! (str head
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
        (stdout!
          (str head (trace-title "🧠" "reasoning") (pretty-block "thinking" (:thinking chunk)))))

      :response-parse
      (stdout!
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
      (stdout! (str head
                    (trace-title "▶"
                                 (str "form "
                                      (inc (long (or (:form-idx chunk) 0)))
                                      (when-let [of (:form-of chunk)]
                                        (str "/" of))))
                    " "
                    (trace-dim "started")
                    (pretty-block "code" (trace-code (:code chunk)))))

      :tool-start
      (stdout! (str head
                    (trace-title "⚙" "tool")
                    (pretty-block "event" (trace-pr-str (:tool-event chunk)))))

      :form-result
      (stdout! (str head
                    (if (:error chunk) (trace-bad "✗ form failed") (trace-ok "✓ form finished"))
                    " #"
                    (inc (long (or (:form-idx chunk) 0)))
                    (when-let [of (:form-of chunk)]
                      (str "/" of))
                    (when-let [ms (envelope-duration-ms (:envelope chunk))]
                      (str " " (trace-dim (str ms "ms"))))
                    (when (:repaired? chunk) (str " " (trace-warn "repaired")))
                    (when (:timeout? chunk) (str " " (trace-bad "timeout")))
                    (if-let [err (trace-error-summary (:error chunk))]
                      (pretty-block "error" (trace-bad err))
                      (pretty-block "result" (trace-pr-str (:result chunk))))))

      :iteration-final
      (stdout!
        (str head
             (if (:done? chunk) (trace-ok "✓ turn complete") (trace-title "·" "iteration complete"))
             (when-let [final (:final chunk)]
               (pretty-block "final"
                             (trace-pr-str (select-keys final [:status :iteration-count]))))))

      :iteration-error
      (stdout! (str head
                    (trace-bad "✗ iteration error")
                    (pretty-block "error"
                                  (or (trace-error-summary (:error chunk)) (trace-pr-str chunk)))))

      (stdout! (str head
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
             (when-not (str/blank? (str (:pending-line s))) (stdout! ""))
             (stdout! (trace-dim "  └"))
             (swap! state assoc
               iter
               (assoc s
                 :reasoning-open? false
                 :pending-line nil)))))
       (emit-reasoning-delta! [iter delta]
         (when-not (get-in @state [iter :reasoning-open?])
           (stdout! (str (trace-dim "\n┌─")
                         " "
                         (trace-title "λ" "trace")
                         (when iter (str " " (trace-dim (str "iteration " iter))))
                         " "
                         (trace-title "🧠" "reasoning")))
           (stdout! (trace-dim "  ┌─ thinking"))
           (write-stdout! (trace-dim "  │ "))
           (swap! state update iter assoc :reasoning-open? true :pending-line ""))
         ;; `parts` splits on '\n' preserving empty trailing segments.
         ;; Every segment except the LAST was followed by a newline in
         ;; the source delta; print it, end the line, and start a fresh
         ;; rail. The last segment may be a partial (no trailing \n)
         ;; that we keep buffered as `:pending-line` for the next
         ;; delta to extend.
         (let [parts (str/split (str delta) #"\n" -1)]
           (dotimes [i (dec (count parts))]
             (write-stdout! (nth parts i))
             (stdout! "") ; newline
             (write-stdout! (trace-dim "  │ "))
             (swap! state assoc-in [iter :pending-line] ""))
           (let [tail (peek parts)]
             (when (and tail (pos? (count tail))) (write-stdout! tail))
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

(defn- wrap-str
  "Word-wrap `s` into a vector of lines, each <= `width` chars. Splits on
   whitespace; tokens longer than `width` are hard-broken so a single
   long URL or symbol can't blow the column out."
  [s ^long width]
  (let [s
        (str s)

        s-count
        (long (count s))]

    (cond (str/blank? s) [""]
          (<= s-count width) [s]
          :else
          (let [tokens (str/split s #"\s+")]
            (loop [tokens tokens
                   line ""
                   lines []]

              (if-let [tok (first tokens)]
                (cond
                  ;; token longer than the column -> hard-split it
                  (> (long (count tok)) width) (let [head (subs tok 0 width)
                                                     tail (subs tok width)
                                                     lines' (cond-> lines
                                                              (seq line)
                                                              (conj line))]

                                                 (recur (cons tail (rest tokens)) head lines'))
                  ;; fits on the current line
                  (or (str/blank? line) (<= (+ (long (count line)) 1 (long (count tok))) width))
                  (recur (rest tokens) (if (str/blank? line) tok (str line " " tok)) lines)
                  ;; doesn't fit -> push current line, start a new one
                  :else (recur (rest tokens) tok (conj lines line)))
                (cond-> lines
                  (seq line)
                  (conj line))))))))

(def ^:private fallback-terminal-width 120)

(defn- terminal-env [k] (System/getenv k))

(defn- parse-positive-long
  [s]
  (try (let [n (some-> s
                       str/trim
                       parse-long)]
         (when (and n (pos? (long n))) n))
       (catch Throwable _ nil)))

(defn- shell-first-line
  "Run a tiny terminal-size probe and return its first stdout line.
   Kept private and timeout-bounded so table rendering never hangs CLI startup."
  [cmd]
  (try (let [p
             (process/process {:cmd ["sh" "-c" cmd] :out :string :err :out})

             proc
             (:proc p)]

         (if (.waitFor ^Process proc 250 java.util.concurrent.TimeUnit/MILLISECONDS)
           (some-> @p
                   :out
                   str/split-lines
                   first)
           (do (process/destroy-tree p) nil)))
       (catch Throwable t (cancellation/preserve-interrupt! t) nil)))

(defn- stty-terminal-width
  []
  (when-let [line (shell-first-line "stty size < /dev/tty")]
    (some-> (re-find #"^\s*\d+\s+(\d+)\s*$" line)
            second
            parse-positive-long)))

(defn- tput-terminal-width [] (parse-positive-long (shell-first-line "tput cols")))

(defn- terminal-width
  "Best-effort terminal width for CLI tables. zsh/bash often keep COLUMNS
   as a shell variable instead of exporting it, so also query the controlling
   terminal via stty. Falls back to 120 for non-interactive runs."
  []
  (or (parse-positive-long (terminal-env "COLUMNS"))
      (stty-terminal-width)
      (tput-terminal-width)
      fallback-terminal-width))

(defn- table-width
  "Visible width of a rendered table with `cols`: outer padding + cells + separators."
  ^long [cols]
  (+ 2 (long (reduce + (map :width cols))) (* 3 (max 0 (dec (long (count cols)))))))

(defn- expand-table-cols
  "Grow table columns to `target-width`. Columns marked `:grow? true`
   share extra width; otherwise the final column grows. This keeps all
   CLI tables full-width while preserving fixed ID/count/date columns."
  [cols ^long target-width]
  (let [cols
        (vec cols)

        extra
        (max 0 (- target-width (table-width cols)))]

    (if (zero? (long extra))
      cols
      (let [grow-idxs
            (let [marked (keep-indexed (fn [idx col]
                                         (when (:grow? col) idx))
                                       cols)]
              (if (seq marked) (vec marked) [(dec (long (count cols)))]))

            n
            (long (count grow-idxs))

            base
            (quot (long extra) n)

            remainder
            (rem (long extra) n)

            additions
            (into {}
                  (map-indexed (fn [i idx]
                                 [idx (+ base (if (< (long i) (long remainder)) 1 0))]))
                  grow-idxs)]

        (mapv (fn [idx col]
                (update col :width + (get additions idx 0)))
              (range)
              cols)))))

(defn- print-table!
  "Print a formatted table to stdout!.
   `cols` is `[{:key :k :label \"L\" :width N :align :left|:right}]`.
   Cells are word-wrapped (not truncated) so long descriptions stay
   visible across multiple physical lines. Tables expand to terminal
   width by growing `:grow?` columns (or the final column by default)."
  [cols rows]
  (let [cols
        (expand-table-cols cols (terminal-width))

        align-line
        (fn [s {:keys [width align]}]
          (if (= align :right) (commandline/pad-left s width) (commandline/pad-right s width)))

        sep
        (str "─" (str/join "─┼─" (map #(apply str (repeat (:width %) \─)) cols)) "─")

        header
        (str " " (str/join " │ " (map #(commandline/pad-right (:label %) (:width %)) cols)) " ")]

    (stdout! header)
    (stdout! sep)
    (doseq [row rows]
      (let [wrapped (mapv (fn [c]
                            (wrap-str (get row (:key c)) (:width c)))
                          cols)
            row-lines (apply max 1 (map count wrapped))]

        (dotimes [i row-lines]
          (stdout! (str " "
                        (str/join " │ "
                                  (map (fn [lines col]
                                         (align-line (or (nth lines i nil) "") col))
                                       wrapped
                                       cols))
                        " ")))))))

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

    (stdout! "")
    (stdout! (str "── " label " " (apply str (repeat rule-len \─))))))

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
  (stdout! "Usage: vis-agent [FLAGS] \"prompt\"")
  (stdout! "")
  (stdout! "Flags:")
  (stdout! "  --json            Print result as a single JSON envelope.")
  (stdout! "  --code            Print only [:code] block contents from the")
  (stdout! "                    parsed Markdown. Concatenated in source order;")
  (stdout! "                    no fences, no language tags. Pipes cleanly")
  (stdout! "                    into editors / interpreters. Errors when")
  (stdout! "                    the answer contains no [:code] blocks.")
  (stdout! "  --raw             Render the answer as raw text (no markdown")
  (stdout! "                    bold/italics/heading bars). This is also the")
  (stdout! "                    auto-default when stdout is not a TTY (piped")
  (stdout! "                    or redirected), so `vis-agent ... > out.txt`")
  (stdout! "                    produces clean text without ANSI noise.")
  (stdout! "  --toggles LIST    Comma-separated NAME=VALUE pairs setting any")
  (stdout! "                    registered snake_case toggle id for this run only, e.g.")
  (stdout! "                    --toggles reasoning_level=deep")
  (stdout! "  --full-trace-stream")
  (stdout! "                    Stream a pretty terminal trace while the run is")
  (stdout! "                    happening, then print the answer.")
  (stdout! "  --full-trace-json-stream")
  (stdout! "                    Stream raw JSON trace frames, one object per line.")
  (stdout! "  --debug           Enable verbose debug logging.")
  (stdout! "  --provider PROVIDER  Use this provider (e.g. openai, anthropic).")
  (stdout! "  --model MODEL        Override the configured model. Also accepts")
  (stdout! "                       provider/name (e.g. openai/gpt-4o).")
  (stdout! "  --reasoning-effort E  Exact provider-native effort: high or max.")
  (stdout! "  --name NAME          Set the agent name (default: cli).")
  (stdout! "  --db PATH|:memory    Override the SQLite path (or :memory).")
  (stdout! "  --session-id ID      Continue an existing persisted session.")
  (stdout! "  --persist            Write this run to ~/.vis/vis.mdb as a")
  (stdout! "                       resumable :cli session. Without it a run is")
  (stdout! "                       ephemeral: no resume, no session row on disk.")
  (stdout! "  --                   End flag parsing: every later word is prompt")
  (stdout! "                       text, dashes and all.")
  (stdout! "")
  (stdout! "Examples:")
  (stdout!
    "  vis-agent --provider zai-coding-plan --model glm-5.2 --reasoning-effort high --json \"Task\"")
  (stdout! "  vis-agent \"Throwaway one-shot probe\"")
  (stdout! "  vis-agent --json --model gpt-4o \"Explain auth flow\"")
  (stdout! "  vis-agent --toggles reasoning_level=deep \"Run the test suite and fix failures\"")
  (stdout! "  vis-agent --toggles reasoning_level=balanced \"Refactor carefully\"")
  (stdout!
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
        (stdout! (str "vis-agent: " e)))
      (stdout! "  See the flag list:            vis-agent --help")
      (when (some #(str/starts-with? % "unknown flag") errors)
        (stdout! "  Or make it the prompt text:   vis-agent -- <text>"))
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
                (stdout! (str "\n"
                              (trace-dim
                                "└────────────────────────────────────────────────────────")))
                (stdout! (str "\n"
                              (trace-title "◆" "final result")
                              (pretty-block "summary" (trace-final-summary-prose result))))
                (stdout! (str "\n" (trace-title "◆" "answer") "\n"))
                (stdout! (content/text-projection (result-content result)))
                (when (:error result)
                  (when-let [ex (:exception result)]
                    (stdout! "\nStack trace:")
                    (.printStackTrace ^Throwable ex ^java.io.PrintStream config/original-stdout))))
            json? (stdout! (result->json result))
            code?
            (let [blocks (->> (result-content result)
                              (keep #(when (= "code" (get % "type")) (get % "text")))
                              vec)]
              (cond (:error result) (stdout! (error/format-error (:error result)))
                    (empty? blocks)
                    (do (stdout!
                          "Error: --code expects at least one code content block; got prose only.")
                        (shutdown-agents)
                        (System/exit 1))
                    :else (stdout! (str/join "\n\n" blocks))))
            (:error result) (stdout! (error/format-error (:error result)))
            :else (do (stdout! (content/text-projection (result-content result)))
                      (when (and (:duration-ms result) (not effective-raw?))
                        (stdout! (str "\n[" (fmt/format-meta-line result) "]")))))
      (shutdown-agents)
      (when (pos? (long exit-code)) (System/exit exit-code)))))

;;; ── `vis-agent sessions` ─────────────────────────────────────────────────

(def ^:private known-channels #{"tui" "cli" "api"})

(def ^:private known-channel-filters (conj known-channels "all"))

(defn- resolve-session-by-prefix
  "Resolve a user-supplied session reference (full UUID or an
   unambiguous prefix) to the canonical UUID. Scans every channel
   because forks are channel-agnostic; the user typed an id, we find
   it. Returns nil on miss or ambiguous prefix. Existence-checks full
   UUID strings; backend `db-resolve-session-id` only parses them."
  [d input]
  (let [s (some-> input
                  str
                  str/trim)]
    (when (seq s)
      (letfn [(existing-id [id]
                (when (and id (try (persistance/db-get-session d id) (catch Throwable _ nil))) id))]
        (or (try (existing-id (persistance/db-resolve-session-id d s)) (catch Throwable _ nil))
            (let [matches (->> (or (persistance/db-list-sessions d :all) [])
                               (filter #(str/starts-with? (str (:id %)) s))
                               (map :id)
                               distinct
                               vec)]
              (when (= 1 (count matches)) (existing-id (first matches)))))))))

(defn- cli-fork-session!
  "Fork a session by id. Creates a new `session_state` row
   that points at the latest state as its parent, optionally with a
   user-supplied title. Prints the new state UUID; the session
   id (soul-id) stays the same so `vis-agent channels tui --session-id
   <ID>` keeps working and now resumes from the fork."
  [cid-input title]
  (let [d
        (lp/db-info)

        resolved
        (resolve-session-by-prefix d cid-input)]

    (cond (nil? resolved) (do (stdout! (str "Session not found: " cid-input))
                              (stdout! "")
                              (stdout! "List existing sessions with:")
                              (stdout! "  vis-agent sessions")
                              (shutdown-agents)
                              (System/exit 1))
          :else
          (let [;; Fork = new session_state = new workspace pin (1:1).
                ;; Mint a fresh isolated workspace for the fork.
                ws-id
                (:id (workspace/ensure-workspace! d {}))

                opts
                (cond-> {:workspace-id ws-id}
                  (and title (not (str/blank? title)))
                  (assoc :title title))

                new-state
                (persistance/db-fork-session! d resolved opts)]

            (if new-state
              (do (stdout! "")
                  (stdout! (str "  Forked session " resolved))
                  (when title (stdout! (str "  Title:        " title)))
                  (stdout! (str "  New state-id: " new-state))
                  (stdout! "")
                  (stdout! (str "  Resume with: vis-agent channels tui --session-id " resolved))
                  (stdout! ""))
              (do (stdout!
                    (str "Failed to fork session " resolved "; no existing state to fork from."))
                  (shutdown-agents)
                  (System/exit 1)))
            (shutdown-agents)))))

(defn- session-sort-key
  [{:keys [last-turn-at created-at id]}]
  [(- (long (or (some-> last-turn-at
                        inst-ms)
                0)))
   (- (long (or (some-> created-at
                        inst-ms)
                0))) (str id)])

(defn- session-row
  [d c]
  (let [turns
        (or (persistance/db-list-session-turns d (:id c)) [])

        last-turn
        (last turns)

        channel-name
        (name (or (:channel c) :unknown))]

    {:id (str (:id c))
     :title (or (:title c) "-")
     :last-channel channel-name
     :turns (count turns)
     :forks (long (or (:fork-count c) 0))
     :last-turn-at (:created-at last-turn)
     :last-turn (or (some-> last-turn
                            :created-at
                            fmt/format-date)
                    "-")
     :created-at (:created-at c)
     :created (or (fmt/format-date (:created-at c)) "-")}))

(defn- session-rows
  [d sessions]
  (->> sessions
       (mapv #(session-row d %))
       (sort-by session-sort-key)
       vec))

(defn- sessions-for-listing
  [channel-input]
  (if channel-input (lp/by-channel (keyword channel-input)) (lp/by-channel :all)))

(defn- cli-list-sessions!
  "List persisted sessions. `channel-input` filters to one channel;
   nil lists every known channel. Rows sort by most recent turn first,
   with empty sessions after sessions that have turns."
  [channel-input]
  (let [channel-label
        (or channel-input "all")

        sessions
        (sessions-for-listing channel-input)

        d
        (lp/db-info)]

    (if (empty? sessions)
      (stdout! (if channel-input (str "No " channel-input " sessions found.") "No sessions found."))
      (let [rows (session-rows d sessions)]
        (stdout! (str "\n  " (if channel-input (str/upper-case channel-label) "All") " Sessions\n"))
        (print-table! [{:key :id :label "ID" :width 36 :align :left}
                       {:key :title :label "Title" :width 24 :align :left :grow? true}
                       {:key :last-channel :label "Last Channel" :width 12 :align :left}
                       {:key :turns :label "Turns" :width 5 :align :right}
                       {:key :forks :label "Forks" :width 5 :align :right}
                       {:key :last-turn :label "Last Turn" :width 16 :align :left}
                       {:key :created :label "Created" :width 16 :align :left}]
                      rows)
        (stdout! (str "\n  " (count rows) " session(s)\n"))
        (stdout! "  Resume with: vis-agent channels tui --session-id <ID>  (full or short)")
        (stdout! "  Pick latest: vis-agent channels tui --continue")
        (stdout! "  Browse:      vis-agent channels tui --resume")
        (stdout! "  Show:        vis-agent sessions show <ID>")
        (stdout! "  Fork:        vis-agent sessions fork <ID> [--title TITLE]")
        (stdout! "  Export:      vis-agent sessions export <ID> --md"))))
  (shutdown-agents))

(defn- cli-sessions-list!
  [parsed _residual]
  (config/init-cli!)
  (let [channel
        (get parsed "channel")

        ch
        (when (and channel (not= "all" channel)) (when (contains? known-channels channel) channel))]

    (when (and channel (not (contains? known-channel-filters channel)))
      (stdout! (str "Unknown channel: "
                    channel
                    ". Expected one of: "
                    (str/join ", " (sort known-channel-filters))
                    ". Showing all sessions."))
      (stdout! ""))
    (cli-list-sessions! ch)))

(defn- session-or-exit!
  [d cid-input]
  (let [resolved (resolve-session-by-prefix d cid-input)]
    (if-let [session (when resolved (persistance/db-get-session d resolved))]
      (assoc session :id resolved)
      (do (stdout! (str "Session not found: " cid-input))
          (stdout! "")
          (stdout! "List existing sessions with:")
          (stdout! "  vis-agent sessions list")
          (shutdown-agents)
          (System/exit 1)))))

(defn- session-detail-row [d session] (session-row d session))

(defn- cli-show-session!
  [parsed _residual]
  (config/init-cli!)
  (let [d
        (lp/db-info)

        session
        (session-or-exit! d (get parsed "session-id"))

        row
        (session-detail-row d session)

        states
        (persistance/db-list-session-states d (:id session))]

    (stdout! (str "\n  Session " (:id session)))
    (stdout! "  ─────────────────────────────────")
    (stdout! (str "  Title:        " (:title row)))
    (stdout! (str "  Channel:      " (:last-channel row)))
    (stdout! (str "  Turns:        " (:turns row)))
    (stdout! (str "  Forks:        " (:forks row)))
    (stdout! (str "  Created:      " (:created row)))
    (stdout! (str "  Last turn:    " (:last-turn row)))
    (when-let [model (:model session)]
      (stdout! (str "  Model:        " model)))
    (when-let [provider (:provider session)]
      (stdout! (str "  Provider:     " (name provider))))
    ;; The backend-resolved root remains useful session metadata; whether the engine
    ;; isolated it is intentionally not a human-facing mode.
    (when-let [ws (when-let [sid (persistance/db-latest-session-state-id d (:id session))]
                    (workspace/for-session d sid))]
      (stdout! (str "  Root:         " (:root ws))))
    (when (seq states)
      (stdout! "")
      (stdout! "  States")
      (print-table! [{:key :version :label "Version" :width 7 :align :right}
                     {:key :state-id :label "State ID" :width 36 :align :left}
                     {:key :parent :label "Parent" :width 8 :align :left}
                     {:key :turns :label "Turns" :width 5 :align :right}
                     {:key :created :label "Created" :width 16 :align :left}]
                    (mapv (fn [state]
                            {:version (:version state)
                             :state-id (str (:state-id state))
                             :parent (if-let [p (:parent-state-id state)]
                                       (subs (str p) 0 8)
                                       "-")
                             :turns (:turn-count state)
                             :created (or (fmt/format-date (:created-at state)) "-")})
                          states)))
    (stdout! "")
    (stdout! (str "  Resume:  vis-agent channels tui --session-id " (:id session)))
    (stdout! (str "  Export:  vis-agent sessions export " (subs (str (:id session)) 0 8) " --md"))
    (stdout! "")
    (shutdown-agents)))

(defn- export-html-str
  "Standalone, vis-light-styled HTML transcript for a session — the canonical
   `transcript/transcript-html` render (DB lookup + summary card + turn-by-turn
   forensic body, all CSS inlined), the SAME renderer every other surface
   (`/export`, gateway, companion) uses. No extra extension required."
  [db sid]
  ((requiring-resolve 'com.blockether.vis.internal.foundation.transcript/transcript-html) db sid))

(defn- cinema-export-fn
  "Resolve the headless session-cinema exporter from the channel-tui extension,
   or nil when that jar is not on the classpath. Deferred so `--md`/`--html`
   never pay the Lanterna load cost."
  []
  (requiring-resolve 'com.blockether.vis.ext.channel-tui.cinema/export!))

(defn- resolve-out-path
  "Resolve a user-supplied output path against the invocation directory.
   `bin/vis-agent` runs the JVM source runtime from its source root (so
   `clojure -M:vis` finds deps.edn) but passes the real invocation cwd as
   `-Duser.dir`. Java resolves relative `File` paths against the OS cwd, so a
   bare `out.html` would silently land in the source root while the printed
   path (from `user.dir`) said otherwise. Anchor relatives to `user.dir` (same
   convention as `vis-agent extension scaffold`); absolute paths pass through
   untouched."
  [path]
  (let [f (io/file path)]
    (.getPath (if (.isAbsolute f) f (io/file (System/getProperty "user.dir") path)))))

(defn- ensure-ext
  "Append `.ext` to `path` when it doesn't already end with it (case-insensitive),
   so a bare `siema` given to `--mp4`/`--html` lands as `siema.mp4`/`siema.html`."
  [path ext]
  (let [dot (str "." ext)]
    (if (str/ends-with? (str/lower-case path) (str/lower-case dot)) path (str path dot))))

(defn- cli-export-session!
  [parsed _residual]
  (config/init-cli!)
  (let [d
        (lp/db-info)

        session
        (session-or-exit! d (get parsed "session-id"))

        md?
        (boolean (get parsed "md"))

        html-path
        (some-> (get parsed "html")
                str/trim
                not-empty
                (ensure-ext "html")
                resolve-out-path)

        mp4-path
        (some-> (get parsed "mp4")
                str/trim
                not-empty
                (ensure-ext "mp4")
                resolve-out-path)

        chosen
        (filterv some? [(when md? :md) (when html-path :html) (when mp4-path :mp4)])]

    (when (> (count chosen) 1)
      (stdout! "Choose exactly one of --md, --html PATH, or --mp4 PATH.")
      (shutdown-agents)
      (System/exit 2))
    (cond html-path (let [target (io/file html-path)]
                      (when-let [parent (.getParentFile ^java.io.File target)]
                        (.mkdirs parent))
                      (spit target (export-html-str d (:id session)))
                      (stdout! (str "Exported HTML: " (paths/abbreviate-home (.getPath target)))))
          mp4-path
          (let [fmt
                :mp4

                path
                mp4-path

                export!
                (cinema-export-fn)]

            (when-let [parent (.getParentFile ^java.io.File (io/file path))]
              (.mkdirs parent))
            (if export!
              (let [res (export! (:id session) {:format fmt :out path})]
                (stdout! (format "Exported %s: %s  (%d frames, ~%ds)"
                                 (str/upper-case (name fmt))
                                 (paths/abbreviate-home (:path res))
                                 (:frames res)
                                 (long (/ (double (:video-ms res)) 1000.0)))))
              (do
                (stdout!
                  "Cinema export (--mp4) needs the channel-tui extension, which is not installed.")
                (shutdown-agents)
                (System/exit 2))))
          :else (write-stdout! ((requiring-resolve
                                  'com.blockether.vis.internal.foundation.transcript/transcript-md)
                                 d
                                 (:id session))))
    (shutdown-agents)))

(defn- cli-delete-session!
  [parsed _residual]
  (config/init-cli!)
  (let [d
        (lp/db-info)

        session
        (session-or-exit! d (get parsed "session-id"))]

    ;; DELETE removes the draft too: trash the session's draft clones (primary
    ;; + auto-cloned filesystem roots) before the DB tree. Draft-only — a trunk
    ;; workspace's roots are the user's real dirs and are never touched.
    ;; CLI one-shot: deref so reclamation finishes before the JVM exits (the
    ;; shared discard executor is a daemon thread and would be killed mid-delete).
    (try (some-> (workspace/discard-session-clones! d (:id session))
                 deref)
         (catch Throwable _ nil))
    (lp/delete! (:id session))
    (stdout! (str "Deleted session " (:id session)))
    (shutdown-agents)))


(defn- cli-fork-session-command!
  [parsed _residual]
  (config/init-cli!)
  (cli-fork-session! (get parsed "session-id") (get parsed "title")))

(defn- cli-sessions-search!
  "`vis-agent sessions search <query>` handler. Uses the same transcript search as the
   TUI session navigator: token-prefix matching across user requests and assistant
   replies (answer + thinking), ordered newest-first. Hits print one per line:

     <session-id-prefix>  <side>     <snippet>

   Snippets carry `[match]` markers around hit terms. `--limit N` caps the
   result count (default 25)."
  [parsed _residual]
  (config/init-cli!)
  (let [query
        (or (get parsed "query") "")

        limit
        (max 1
             (long (or (some-> (get parsed "limit")
                               str/trim
                               Long/parseLong)
                       25)))]

    (cond (str/blank? query) (do (stdout! "vis-agent sessions search <query> [--limit N]")
                                 (stdout! "")
                                 (stdout!
                                   "Searches transcripts exactly like the TUI session navigator.")
                                 (shutdown-agents)
                                 (System/exit 1))
          :else
          (let [d
                (lp/db-info)

                hits
                (->> (persistance/db-search-session-matches d :all query)
                     (mapcat (fn [{:keys [id hits]}]
                               (map #(assoc % :session-id id) hits)))
                     (take limit)
                     vec)]

            (cond (empty? hits) (do (stdout! (str "No matches for: " query)) (shutdown-agents))
                  :else (do (stdout! (str (count hits)
                                          " match" (when (not= 1 (count hits)) "es")
                                          " for: " query))
                            (stdout! "")
                            (doseq [{:keys [session-id side snippet]} hits]
                              (let [id-pref (let [s (str session-id)]
                                              (subs s 0 (min 8 (count s))))
                                    snippet (str/replace (or snippet "") #"\s+" " ")]

                                (stdout!
                                  (str id-pref "  " (format "%-8s" (name side)) "  " snippet))))
                            (shutdown-agents)))))))

(defn- cli-sessions!
  "`vis-agent sessions` default handler. Bare `vis-agent sessions` lists all
   sessions; every other operation is a canonical subcommand."
  [_parsed residual]
  (config/init-cli!)
  (if (seq residual)
    (do (stdout! (str "Unknown sessions command: " (first residual)))
        (stdout! "")
        (stdout! "Run: vis-agent sessions --help")
        (shutdown-agents)
        (System/exit 2))
    (cli-list-sessions! nil)))

;;; ── `vis-agent projects` ──────────────────────────────────────────────────────

(defn- match-projects
  "Projects from `projects` selected by `input`: an exact id wins outright,
   otherwise every id with that prefix (case-insensitive).

   Pure on purpose — this is the resolution RULE, so an ambiguous prefix is
   provable without a database."
  [projects input]
  (let [needle
        (str/lower-case (str/trim (str input)))

        pid
        #(str/lower-case (str (:id %)))]

    (if (str/blank? needle)
      []
      (let [exact (filterv #(= needle (pid %)) projects)]
        (if (seq exact) exact (filterv #(str/starts-with? (pid %) needle) projects))))))

(defn- project-or-exit!
  "Resolve a project by full id or unambiguous prefix, or print why not and exit."
  [input]
  (let [matches (match-projects (lp/projects {:include-archived? true}) input)]
    (cond (= 1 (count matches)) (first matches)
          (empty? matches) (do (stdout! (str "Project not found: " input))
                               (stdout! "")
                               (stdout! "List existing projects with:")
                               (stdout! "  vis-agent projects")
                               (shutdown-agents)
                               (System/exit 1))
          :else (do (stdout! (str "Ambiguous project id: " input " (" (count matches) " matches)"))
                    (stdout! "")
                    (doseq [p matches]
                      (stdout! (str "  " (:id p) "  " (or (not-empty (str (:name p))) "-"))))
                    (shutdown-agents)
                    (System/exit 1)))))

(defn- project-rows
  [projects]
  (mapv (fn [p]
          {:id (str (:id p))
           :name (or (not-empty (str (:name p))) "-")
           :root (or (not-empty (str (:workspace-root p))) "-")
           :sessions (str (or (:session-count p) 0))
           :state (if (:archived-at p) "archived" "active")})
        projects))

(defn- list-projects!
  "List projects (cross-channel, archived included) with their live session counts."
  []
  (let [projects (lp/projects {:include-archived? true})]
    (if (empty? projects)
      (stdout! "No projects found.")
      (do (stdout! "\n  Projects\n")
          (print-table! [{:key :id :label "ID" :width 36 :align :left}
                         {:key :name :label "Name" :width 24 :align :left :grow? true}
                         {:key :root :label "Workspace Root" :width 28 :align :left :grow? true}
                         {:key :sessions :label "Sessions" :width 8 :align :right}
                         {:key :state :label "State" :width 8 :align :left}]
                        (project-rows projects))
          (stdout! (str "\n  " (count projects) " project(s)\n"))
          (stdout! "  Delete the project only:      vis-agent projects delete <ID>")
          (stdout!
            "  Delete it and its sessions:   vis-agent projects delete <ID> --with-sessions"))))
  (shutdown-agents))

(defn- delete-project-tree!
  "Delete project `project-id` and return a summary; prints nothing.

   Without `:with-sessions` this is the SCATTER delete the schema has always
   had: the row goes and its sessions survive as project-less. With it, every
   MEMBER session tree is deleted FIRST — an interrupted teardown must leave a
   project holding survivors, never orphans with a dead parent. Membership is
   not a client's visible list: untitled and empty conversations are members
   too, and a caller fanning out over what it can see would keep the rest."
  [d project-id {:keys [with-sessions]}]
  (let [pid
        (str project-id)

        member-ids
        (vec (lp/project-session-ids pid))]

    (when with-sessions
      (doseq [sid member-ids]
        ;; Same order as `vis-agent sessions delete`: trash the draft clones
        ;; (primary + auto-cloned roots) before the DB tree, and deref so
        ;; reclamation finishes before this one-shot JVM exits.
        (try (some-> (workspace/discard-session-clones! d sid)
                     deref)
             (catch Throwable _ nil))
        (lp/delete! sid)))
    (lp/delete-project! pid)
    {:project-id pid
     :deleted-session-ids (if with-sessions member-ids [])
     :kept-session-ids (if with-sessions [] member-ids)}))

(defn- cli-delete-project!
  "`vis-agent projects delete <PROJECT-ID> [--with-sessions]` handler."
  [parsed _residual]
  (config/init-cli!)
  (let [d
        (lp/db-info)

        project
        (project-or-exit! (get parsed "project-id"))

        with-sessions
        (boolean (get parsed "with-sessions"))

        {:keys [project-id deleted-session-ids kept-session-ids]}
        (delete-project-tree! d (:id project) {:with-sessions with-sessions})]

    (stdout! (str "Deleted project "
                  project-id
                  (when-let [n (not-empty (str (:name project)))]
                    (str " (" n ")"))))
    (if with-sessions
      (stdout! (str "  " (count deleted-session-ids) " session(s) deleted with it."))
      (when (seq kept-session-ids)
        (stdout! (str "  " (count kept-session-ids) " session(s) kept, now project-less."))
        (stdout! "  Delete one with: vis-agent sessions delete <ID>"))))
  (shutdown-agents))

(defn- cli-projects-list! [_parsed _residual] (config/init-cli!) (list-projects!))

(defn- cli-projects!
  "`vis-agent projects` default handler. Bare `vis-agent projects` lists them;
   every other operation is a canonical subcommand."
  [_parsed residual]
  (config/init-cli!)
  (if (seq residual)
    (do (stdout! (str "Unknown projects command: " (first residual)))
        (stdout! "")
        (stdout! "Run: vis-agent projects --help")
        (shutdown-agents)
        (System/exit 2))
    (list-projects!)))

;;; ── `vis-agent providers` ─────────────────────────────────────────────────────

(def ^:private providers-table-cols
  [{:key :id :label "ID" :width 18 :align :left} {:key :label :label "Label" :width 28 :align :left}
   {:key :auth :label "Auth" :width 6 :align :left}
   {:key :rpm :label "Catalog RPM" :width 11 :align :right}
   {:key :tpm :label "Catalog TPM" :width 12 :align :right}
   {:key :base-url :label "Base URL" :width 36 :align :left}])

(defn- gateway-provider-status-safe
  [provider-id]
  (try (gateway-client/provider-status provider-id)
       (catch Throwable e {"is_authenticated" false "error" (or (ex-message e) (str e))})))

(defn- gateway-provider-limits-safe
  [provider-id]
  (try (gateway-client/provider-limits provider-id)
       (catch Throwable e
         {:provider-id provider-id
          :status :error
          :static {}
          :dynamic {:limits []}
          :error {:message (or (ex-message e) (str e))}})))

(defn- configured-provider-entry
  [provider-id]
  (->> (or (:providers (config/current-config)) [])
       (filter #(= provider-id (:id %)))
       first))

(defn- configured-provider-status [provider] (gateway-provider-status-safe (:provider/id provider)))

(defn- configured-provider-base-url
  [provider-id]
  (or (:base-url (configured-provider-entry provider-id))
      (some-> provider-id
              config/provider-template
              :base-url)))

(defn- provider-label-for-id
  "Registered/preset branding first, else the id VERBATIM — a `vis.yml` id keeps
   the casing its author typed (see `config/display-label`)."
  [provider-id]
  (or (some-> (registry/provider-by-id provider-id)
              :provider/label)
      (some-> (config/provider-template provider-id)
              :label)
      (some-> provider-id
              name)))

(defn- status-entry-label
  [k]
  (-> (name k)
      (str/replace #"-" " ")
      (str/capitalize)))

(defn- format-status-value
  [v]
  (cond (keyword? v) (name v)
        (map? v) (str/join ", "
                           (map (fn [[k2 v2]]
                                  (str (name k2) ": " (format-status-value v2)))
                                (sort-by (comp str key) v)))
        (sequential? v) (str/join ", " (map format-status-value v))
        :else (str v)))

(defn- format-limit-window
  [{:keys [kind unit size resets-at-ms]}]
  (when kind
    (str (name kind)
         (when unit (str " " (or size 1) "/" (name unit)))
         (when resets-at-ms
           (str ", resets " (fmt/format-date (java.util.Date. (long resets-at-ms))))))))

(defn- format-limit-row
  [{:keys [label scope kind is-unlimited used limit remaining note window]}]
  (let [quota
        (cond is-unlimited "unlimited"
              (number? limit) (str (when (number? used) (str used "/"))
                                   limit
                                   (when (number? remaining) (str " (" remaining " left)")))
              (number? used) (str "used " used)
              :else nil)

        attrs
        (->> [(some-> scope
                      name)
              (some-> kind
                      name) (format-limit-window window)]
             (remove nil?))]

    (str label
         (when (seq attrs) (str " [" (str/join ", " attrs) "]"))
         (when quota (str ": " quota))
         (when note (str " - " note)))))

(defn- provider-limit-lines
  [provider-id]
  (let [report
        (gateway-provider-limits-safe provider-id)

        static
        (:static report)

        dynamic
        (get-in report [:dynamic :limits])

        note
        (get-in report [:dynamic :note])

        error*
        (:error report)]

    (vec (concat [(str "  Limits status: " (name (:status report)))]
                 (when-let [rpm (:rpm static)]
                   [(str "  Catalog RPM:    " rpm)])
                 (when-let [tpm (:tpm static)]
                   [(str "  Catalog TPM:    " tpm)])
                 (if (seq dynamic)
                   (concat ["  Dynamic limits:"] (map #(str "    - " (format-limit-row %)) dynamic))
                   ["  Dynamic limits: none reported"])
                 (when note [(str "  Note:           " note)])
                 (when (seq static)
                   ["  Catalog RPM / TPM come from svar metadata, not live account quota usage."])
                 (when error* [(str "  Error:          " (:message error*))])))))

(defn- print-provider-status!
  [provider]
  (let [status
        (or (configured-provider-status provider) {"is_authenticated" false})

        provider-id
        (:provider/id provider)

        base-url
        (configured-provider-base-url provider-id)

        rows
        (->> status
             (remove (fn [[k _]]
                       (= k "is_authenticated")))
             (sort-by (comp str key)))]

    (stdout! (str "\n  " (:provider/label provider) " Provider Status"))
    (stdout! "  ─────────────────────────────────")
    (when base-url (stdout! (str "  Base URL:       " base-url)))
    (stdout! (str "  Authenticated:  " (if (get status "is_authenticated") "yes" "no")))
    (doseq [[k v] rows]
      (stdout! (str "  "
                    (commandline/pad-right (str (status-entry-label k) ":") 15)
                    (format-status-value v))))
    (doseq [line (provider-limit-lines provider-id)]
      (stdout! line))
    (stdout! "")))

(defn- print-provider-limits!
  [provider-id]
  (stdout! (str "\n  " (provider-label-for-id provider-id) " Limits"))
  (stdout! "  ─────────────────────────────────")
  (doseq [line (provider-limit-lines provider-id)]
    (stdout! line))
  (stdout! ""))

(defn- providers-list-rows
  []
  (->> (registry/registered-providers)
       (sort-by :provider/id)
       (mapv
         (fn [provider]
           (let [status
                 (configured-provider-status provider)

                 report
                 (gateway-provider-limits-safe (:provider/id provider))

                 base-url
                 (configured-provider-base-url (:provider/id provider))]

             {:id (name (:provider/id provider))
              :label (:provider/label provider)
              :auth (if (get status "is_authenticated") "yes" "no")
              :rpm (or (some-> report
                               :static
                               :rpm
                               str)
                       "-")
              :tpm (or (some-> report
                               :static
                               :tpm
                               str)
                       "-")
              :base-url (or base-url "-")})))))

(defn- print-registered-providers!
  []
  (let [all (registry/registered-providers)]
    (if (seq all)
      ;; Width tracks the LONGEST provider id + a 2-space gutter so ids like
      ;; `github-copilot-individual` (25 chars) never run into their label.
      (let [w (+ 2 (long (reduce max 0 (map #(count (name (:provider/id %))) all))))]
        (stdout! "Available providers:")
        (doseq [p (sort-by :provider/id all)]
          (stdout!
            (str "  " (commandline/pad-right (name (:provider/id p)) w) (:provider/label p)))))
      (stdout! "No providers registered. Drop a vis-provider-* jar onto the classpath."))))

(defn- cli-providers-list!
  [_parsed _residual]
  (config/init-cli!)
  (let [rows (providers-list-rows)]
    (if (empty? rows)
      (stdout! "No providers registered. Drop a vis-provider-* jar onto the classpath.")
      (do (stdout! "\n  Providers\n")
          (print-table! providers-table-cols rows)
          (stdout! (str "\n  " (count rows) " provider(s)\n")))))
  (shutdown-agents))

(defn- cli-providers-status!
  [_parsed residual]
  (config/init-cli!)
  (let [provider-name
        (first residual)

        provider-id
        (some-> provider-name
                keyword)

        provider
        (when provider-id (registry/provider-by-id provider-id))

        providers
        (if provider-name
          (if provider [provider] [])
          (sort-by :provider/id (registry/registered-providers)))]

    (cond (and provider-name (nil? provider)) (do (stdout! (str "Unknown provider: " provider-name))
                                                  (stdout! "")
                                                  (print-registered-providers!))
          (empty? providers)
          (stdout! "No providers registered. Drop a vis-provider-* jar onto the classpath.")
          :else (doseq [p providers]
                  (print-provider-status! p))))
  (shutdown-agents))

(defn- cli-providers-limits!
  [_parsed residual]
  (config/init-cli!)
  (let [provider-name
        (first residual)

        registered
        (sort-by :provider/id (registry/registered-providers))]

    (if provider-name
      (let [provider-id
            (keyword provider-name)

            known?
            (or (registry/provider-by-id provider-id)
                (config/provider-template provider-id)
                (seq (:static (gateway-provider-limits-safe provider-id))))]

        (if known?
          (print-provider-limits! provider-id)
          (do (stdout! (str "Unknown provider: " provider-name))
              (stdout! "")
              (print-registered-providers!))))
      (if (seq registered)
        (doseq [provider registered]
          (print-provider-limits! (:provider/id provider)))
        (stdout! "No providers registered. Drop a vis-provider-* jar onto the classpath."))))
  (shutdown-agents))

(defn- cli-providers-auth!
  [parsed residual]
  (config/init-cli!)
  (let [provider-name
        (or (get parsed "provider") (first residual))

        provider-id
        (some-> provider-name
                keyword)

        provider
        (when provider-id (registry/provider-by-id provider-id))]

    (cond (nil? provider-id) (do (stdout! "Usage: vis-agent providers auth <provider>")
                                 (stdout! "")
                                 (print-registered-providers!))
          (nil? provider) (do (stdout! (str "Unknown provider: " provider-name))
                              (stdout! "")
                              (print-registered-providers!))
          (nil? (:provider/auth-fn provider)) (stdout!
                                                (str "Provider "
                                                     (:provider/label provider)
                                                     " does not expose an interactive auth flow."))
          :else (try ((:provider/auth-fn provider) stdout!)
                     (catch Exception e
                       (stdout! (error/format-error (str "Authentication failed: "
                                                         (ex-message e))))))))
  (shutdown-agents))

(defn- cli-providers-logout!
  [parsed residual]
  (config/init-cli!)
  (let [provider-name
        (or (get parsed "provider") (first residual))

        provider-id
        (some-> provider-name
                keyword)

        provider
        (when provider-id (registry/provider-by-id provider-id))

        configured?
        (boolean (some #(= (name provider-id) (get % "id"))
                       (get (config/load-config-raw) "providers")))]

    (cond (nil? provider-id) (do (stdout! "Usage: vis-agent providers logout <provider>")
                                 (stdout! "")
                                 (print-registered-providers!))
          (nil? provider) (do (stdout! (str "Unknown provider: " provider-name))
                              (stdout! "")
                              (print-registered-providers!))
          (and (nil? (:provider/logout-fn provider)) (not configured?))
          (stdout! (str "Provider " (:provider/label provider) " does not persist credentials."))
          :else (do (if-let [logout-fn (:provider/logout-fn provider)]
                      (logout-fn)
                      ;; Key-only provider: forget the KEY, keep the entry.
                      (providers/clear-provider-api-key! provider-id :cli-provider-logout))
                    ;; The config entry stays: logging out drops the credential, not
                    ;; the provider's models/base-url, so signing back in is one
                    ;; `providers auth` away.
                    (stdout! (str "  Logged out of "
                                  (:provider/label provider)
                                  ". Credentials cleared; provider stays configured.")))))
  (shutdown-agents))

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
       (format "%9s" (housekeeping/format-bytes (long (or bytes 0))))
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
        (stdout! (doctor/format-output msgs))
        (System/exit (int (doctor/exit-code msgs))))
      (let [{:keys [purged count bytes reclaimed-bytes] :as report}
            (housekeeping/purge! {:db-info db-info :days days :is-dry-run is-dry-run})]
        (stdout! (if (zero? (long (or count 0)))
                   (str "Nothing untouched for over "
                        (:days report)
                        " days — drafts and session journals are already tidy.")
                   (str (if is-dry-run "Would reclaim " "Reclaimed ")
                        count
                        (if (= 1 (long count)) " item, " " items, ")
                        (housekeeping/format-bytes (long (or (if is-dry-run bytes reclaimed-bytes)
                                                             0)))
                        " (untouched for over "
                        (:days report)
                        " days):\n"
                        (str/join "\n" (map housekeeping-line purged))
                        (when is-dry-run "\n\nRe-run without --dry-run to reclaim."))))
        (System/exit 0)))))

;;; ── `vis-agent extension` ───────────────────────────────────────────────────────────

(def ^:private extensions-table-cols
  [{:key :namespace :label "Namespace" :width 28 :align :left}
   {:key :group :label "Group" :width 18 :align :left}
   {:key :author :label "Author" :width 12 :align :left}
   {:key :owner :label "Owner" :width 8 :align :left}
   {:key :license :label "License" :width 10 :align :left}
   {:key :doc :label "Description" :width 36 :align :left :grow? true}
   {:key :version :label "Version" :width 10 :align :left}])

(defn- cli-extensions!
  [_parsed _residual]
  (config/init-cli!)
  (let [exts
        (list-extensions)

        cols
        (expand-table-cols extensions-table-cols (terminal-width))

        width
        (table-width cols)]

    (if (empty? exts)
      (stdout! "No extensions registered.")
      (do (stdout! "\n  Extensions\n")
          (doseq [[kind rows] (sort-by key (group-by :kind exts))]
            (print-section-heading! kind width)
            (print-table! cols (sort-by (juxt :group :namespace) rows)))
          (stdout! (str "\n  " (count exts) " extension(s)\n")))))
  (shutdown-agents))

(defn- safe-extension-name
  [s]
  (let [name (some-> s
                     str
                     str/trim
                     (str/replace #"[^A-Za-z0-9._-]+" "-")
                     (str/replace #"^-+|-+$" ""))]
    (when (seq name) name)))

(defn- extension-namespace
  [name explicit]
  (let [base (or (some-> explicit
                         str/trim
                         not-empty)
                 (str "vis.ext."
                      (-> name
                          str/lower-case
                          (str/replace #"[^a-z0-9._-]+" "-")
                          (str/replace #"[-_]+" "-"))))]
    (symbol base)))

(defn- namespace->path
  [ns-sym]
  (str (-> (str ns-sym)
           (str/replace "-" "_")
           (str/replace "." "/"))
       ".clj"))

(defn- scaffold-extension-files
  [{:keys [name namespace]}]
  (let [ns-sym
        (extension-namespace name namespace)

        ns-path
        (namespace->path ns-sym)]

    {"deps.edn" (str "{:paths [\"src\" \"resources\"]\n" " :deps {}}\n")
     "resources/META-INF/vis-extension/vis.edn" (pr-str {(symbol name) {:nses [ns-sym]}})
     (str "src/" ns-path) (str "(ns "
                               ns-sym
                               "\n"
                               "  (:require [com.blockether.vis.core :as vis]))\n\n"
                               "(defn hello\n"
                               "  []\n"
                               "  \"hello from "
                               name
                               "\")\n\n"
                               "(def vis-extension\n"
                               "  (vis/extension\n"
                               "    {:ext/name \""
                               name
                               "\"\n"
                               "     :ext/description \"User extension " name
                               "\"\n" "     :ext/version \"0.1.0\"\n"
                               "     :ext/author \"local\"\n" "     :ext/owner \"local\"\n"
                               "     :ext/kind \"user\"}))\n\n"
                               "(vis/register-extension! vis-extension)\n")}))

(defn- parse-scaffold-opts
  [parsed residual]
  (let [argv
        (vec residual)

        parsed-name
        (:name parsed)

        parsed-dir
        (:dir parsed)

        parsed-namespace
        (:namespace parsed)

        force?
        (boolean (or (:force parsed) (some #{"--force"} argv)))

        parsed-argv
        (loop [xs
               argv

               positional
               []

               opts
               {}]

          (if-let [x (first xs)]
            (case x
              "--force"
              (recur (rest xs) positional opts)

              "--dir"
              (recur (nnext xs) positional (assoc opts :dir (second xs)))

              "--namespace"
              (recur (nnext xs) positional (assoc opts :namespace (second xs)))

              (recur (rest xs) (conj positional x) opts))
            (assoc opts :positional positional)))

        dir
        (or parsed-dir (:dir parsed-argv))

        namespace
        (or parsed-namespace (:namespace parsed-argv))

        name
        (safe-extension-name (or parsed-name (first (:positional parsed-argv))))]

    {:name name :dir dir :namespace namespace :force? force?}))

(defn- cli-extensions-scaffold!
  [parsed residual]
  (config/init-cli!)
  (let [{:keys [name dir force?] :as opts} (parse-scaffold-opts parsed residual)]
    (when-not name
      (throw (ex-info
               "Usage: vis-agent extension scaffold <name> [--dir DIR] [--namespace NS] [--force]"
               {:type :cli/usage})))
    (let [target-path (or dir (str ".vis/vis-extensions/" name))
          target (let [f (io/file target-path)]
                   (if (.isAbsolute f) f (io/file (System/getProperty "user.dir") target-path)))
          files (scaffold-extension-files opts)]

      (doseq [[rel content] files]
        (let [f (io/file target rel)]
          (when (and (.exists f) (not force?))
            (throw (ex-info "Refusing to overwrite existing extension file"
                            {:type :extension/scaffold-file-exists :path (.getPath f)})))
          (.mkdirs (.getParentFile ^java.io.File f))
          (spit f content)))
      (stdout!
        (str
          "Created extension scaffold at " (.getPath target)
          "\n"
          "It is auto-loaded when you run vis-agent from this project (or from ~/.vis/vis-extensions)."))))
  (shutdown-agents))

(defn- cli-extension-check!
  "Static-check Python extension files WITHOUT running them.

   Reports per file, then exits 1 if anything was refused, so it drops straight
   into a pre-commit hook or CI. Paths may be files or directories; with none, the
   extension directories vis itself loads are checked."
  [_parsed residual]
  (config/init-cli!)
  (let [ec
        (fn [sym]
          (requiring-resolve (symbol "com.blockether.vis.internal.extension-check" (name sym))))

        paths
        (vec (remove #(str/starts-with? (str %) "-") residual))

        files
        ((ec 'expand-paths) paths)]

    (if (empty? files)
      (stdout!
        "No Python extension files found. Pass a file or a directory, or add one to .vis/extensions/.")
      (let [reports ((ec 'check-files) files)]
        (stdout! ((ec 'report-text) reports))
        (when-not ((ec 'ok?) reports) (shutdown-agents) (System/exit 1))))
    (shutdown-agents)))










(defn- cli-gateway-start!
  "Run the HTTP/SSE gateway daemon. Lazy resolve keeps
   Ring/Jetty class loading off every other command's startup path."
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  ((requiring-resolve 'com.blockether.vis.internal.gateway.server/serve-main!)
    {:port (get parsed "port")
     :host (get parsed "host")
     :token-file (get parsed "token-file")
     :require-token? (boolean (get parsed "require-token"))
     :pair? (boolean (get parsed "pair"))
     :managed? (= "1" (System/getenv "VIS_GATEWAY_MANAGED"))
     :db (config/resolve-db-spec (when-let [db (get parsed "db")]
                                   (if (= db ":memory") :memory {:backend :sqlite :path db})))}))

(defn- plural
  "`n` with `one` pluralized by an s - the shape every gateway line counts in."
  [n one]
  (str n " " one (when (not= 1 (long n)) "s")))

(defn- build-label
  "How a Vis build names ITSELF to a human, read off its handshake
   (`{:version :build}`): the release version, or - when a source checkout has no
   release to be ordered by - the commit that is its identity, because \"dev\"
   alone names no code."
  [{:keys [version build]}]
  (if (and (= "dev" version) build) (str version " (" build ")") version))

(defn- this-handshake
  "What THIS runtime advertises about itself ([[protocol/handshake]])."
  []
  ((requiring-resolve 'com.blockether.vis.internal.gateway.protocol/handshake)))

(defn- stale-daemon-note
  "The line `gateway status` adds when THIS runtime (`ours`, a handshake) is newer
   code than the daemon `status` describes: what picks the new build up, and what
   still holds the old one. nil when it is not.

   Both halves come out of the handshake the status map already carries, so the
   answer costs no extra round trip, and it applies the SAME two rules an attach
   does - [[protocol/superseded?]] for the verdict, [[client/daemon-idle?]] for
   whether anything is in the way. A status line must never promise a replacement
   the next client would refuse to make."
  [status ours]
  (let [peer
        (get status "protocol")

        superseded?
        ((requiring-resolve 'com.blockether.vis.internal.gateway.protocol/superseded?)
          {:our-version (:version ours)
           :their-version (get peer "version")
           :our-build (:build ours)
           :their-build (get peer "build")})

        {:keys [reason clients running-turns pid]}
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/daemon-idle?) status)]

    (when superseded?
      (str "this build is " (build-label ours)
           " - " (case reason
                   :idle
                   "the next session starts on it"

                   :user-owned
                   (str "the running daemon is user-owned"
                        (when pid (str " (pid " pid ")"))
                        " - stop and start it yourself to pick it up")

                   (if (and clients running-turns)
                     (str "it is picked up once nothing is using this one ("
                          (plural clients "client")
                          ", "
                          (plural running-turns "running turn")
                          ")")
                     "it is picked up once this one is no longer in use"))))))

(defn- cli-gateway-status!
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  (let [{:strs [status pid host port db clients running_turns require_token] :as m}
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/status))]
    (if (= "running" status)
      (let [peer (get m "protocol")]
        (stdout!
          (str "gateway running pid="
               pid
               " url=http://"
               host
               ":"
               port
               " db="
               db
               " clients="
               clients
               " running-turns="
               running_turns
               " auth="
               (if require_token "token" "loopback-disabled")
               " version="
               (or (get peer "version") "unknown")
               (when-let [build (get peer "build")]
                 (str " build=" build))))
        ;; "why is my update not in effect yet" is what this command gets asked, and
        ;; the answer is already in the map. A --gateway target is another machine's
        ;; lifecycle, so nothing here is ever going to replace it.
        (when-not ((requiring-resolve 'com.blockether.vis.internal.gateway.client/remote-gateway))
          (when-let [note (stale-daemon-note m (this-handshake))]
            (stdout! note))))
      (stdout! (str "gateway stopped"
                    (when-let [db (get m "db")]
                      (str " db=" db)))))))

(defn- cli-gateway-pair!
  "Print a companion pairing QR for the gateway ALREADY running for this DB, so
   you can pair without stopping/restarting it. Refuses a loopback-bound daemon
   (a phone can never reach 127.0.0.1) with a copy-paste fix."
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  (let [{:keys [running? host port token loopback?]}
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/pairing-info))]
    (cond
      (not running?)
      (throw (ex-info (str "no gateway is running for this DB. Start one reachable first:\n"
                           "  vis-agent gateway start --host 0.0.0.0 --require-token --pair")
                      {:vis/user-error true}))
      loopback?
      (throw
        (ex-info
          (let [ts (first ((requiring-resolve
                             'com.blockether.vis.internal.gateway.pairing/tailscale-hosts)))]
            (str "the running gateway is bound to " host
                 " (loopback) \u2014 a phone cannot reach it.\n" "Restart it on a reachable host:\n"
                 "  vis-agent gateway stop\n"
                 (if ts
                   (str "  vis-agent gateway start --host " ts
                        " --require-token --pair"
                        "   # your Tailscale IP \u2014 reachable from the phone on your tailnet")
                   "  vis-agent gateway start --host 0.0.0.0 --require-token --pair")))
          {:vis/user-error true}))
      :else ((requiring-resolve 'com.blockether.vis.internal.gateway.pairing/print-pairing!)
              {:host host :port port :token token :require-token? (boolean token) :emit stdout!}))))

(defn- gateway-stop-if-idle!
  "`--if-idle`: release the daemon only when releasing it is free, and say why when
   it is not. Never fails - `vis-agent update` runs exactly this after installing a
   new runtime, and an update must not report failure because someone had a TUI
   open. Silent when nothing is running, so a plain update prints nothing extra.

   A daemon left alone here is not left stale: the next client to attach it with
   nobody using it replaces it itself (`client/stale-bounce-verdict`), so the advice
   printed for a busy one is to finish and close the session, not to run anything."
  []
  (let [{:keys [stopped? reason clients running-turns pid]}
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/stop-daemon-if-idle!))

        version
        (build-label (this-handshake))]

    (cond stopped? (stdout! (str "gateway stopped - next session starts on " version))
          (= :not-running reason) nil
          (= :remote reason) (stdout!
                               "gateway is a --gateway target on another machine - left alone")
          (= :user-owned reason) (stdout!
                                   (str "gateway is user-owned" (when pid (str " (pid " pid ")"))
                                        " - restart it yourself when ready:\n"
                                        "  vis-agent gateway stop && vis-agent gateway start"))
          :else (stdout! (str "gateway still running ("
                              (plural clients "client")
                              ", "
                              (plural running-turns "running turn")
                              ") - left alone, still serving the old build.\n"
                              "  Quit those sessions and the next vis picks up " version
                              " by itself; to bounce it now:\n" "  vis-agent gateway stop")))))

(defn- cli-gateway-stop!
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  (if (get parsed "if-idle")
    (gateway-stop-if-idle!)
    (let [{:keys [stopping status type host port pid recovery escalated] :as m}
          ((requiring-resolve 'com.blockether.vis.internal.gateway.client/stop-daemon!))]
      (stdout! (cond stopping "gateway stopping"
                     (= "stopped" status)
                     (if escalated
                       (str "gateway stopped by " (if (= :kill escalated) "SIGKILL" "SIGTERM")
                            " - it had stopped answering" (when pid (str " (pid " pid ")")))
                       "gateway stopped")
                     (= :gateway/orphaned-daemon type) (str "gateway stop found a live orphan at "
                                                            host
                                                            ":"
                                                            port
                                                            (when pid
                                                              (str " (registered PID " pid ")"))
                                                            ". "
                                                            recovery)
                     :else (str "gateway stop requested: " (pr-str m)))))))

;;; ── `vis-agent gateway mcp` subcommands ──────────────────────────────────────

(defn- with-mcp-db!
  [parsed]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db)))

(defn- parse-kv-list
  "Parse a comma-separated K=V,K2=V2 flag value into a string-keyed map, or nil
   when blank."
  [s]
  (when-not (str/blank? (str s))
    (into {}
          (map (fn [pair]
                 (let [[k v] (str/split pair #"=" 2)]
                   [(str/trim k) (str/trim (or v ""))])))
          (str/split s #","))))

(defn- mcp-spec-from-parsed
  "Build a wire-shaped MCP server spec (string-keyed) from parsed CLI flags.
   `url` implies Streamable HTTP; `command` implies stdio -- exactly what
   `mcp.core/transport-of` infers when no explicit `transport` is given."
  [parsed]
  (cond-> {"enabled" (not (boolean (get parsed "disabled")))}
    (get parsed "url")
    (assoc "url" (get parsed "url"))

    (parse-kv-list (get parsed "headers"))
    (assoc "headers" (parse-kv-list (get parsed "headers")))

    (get parsed "command")
    (assoc "command" (get parsed "command"))

    (not (str/blank? (str (get parsed "args"))))
    (assoc "args" (vec (str/split (get parsed "args") #"\s+")))

    (get parsed "cwd")
    (assoc "cwd" (get parsed "cwd"))

    (parse-kv-list (get parsed "env"))
    (assoc "env" (parse-kv-list (get parsed "env")))

    (get parsed "timeout-ms")
    (assoc "timeout_ms" (get parsed "timeout-ms"))))

(defn- require-mcp-name!
  [parsed]
  (let [n (get parsed "name")]
    (when (str/blank? n) (throw (ex-info "A server NAME is required." {:vis/user-error true})))
    n))

(defn- cli-mcp-list!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [rows ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-servers))]
    (if (empty? rows)
      (stdout! "No MCP servers configured. Add one: vis-agent gateway mcp add <NAME> --url <URL>")
      (print-table! [{:key "name" :label "Name" :width 16}
                     {:key "transport" :label "Transport" :width 15}
                     {:key "enabled" :label "Enabled" :width 7}
                     {:key "is_connected" :label "Connected" :width 9}
                     {:key "is_authorized" :label "Authorized" :width 10}
                     {:key "tools" :label "Tools" :width 5}]
                    rows))))

(defn- cli-mcp-add!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        spec
        (mcp-spec-from-parsed parsed)

        saved
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-save-server!)
          name
          spec)]

    (stdout! (str "Saved MCP server \"" name "\" (" (get saved "transport") ")."))
    (when (= "streamable_http" (get saved "transport"))
      (if (get saved "is_authorized")
        (stdout! "  Already authorized.")
        (do (stdout! "  This server needs OAuth sign-in before it can be used:")
            (stdout! (str "    vis-agent gateway mcp auth-start " name)))))))

(defn- cli-mcp-test!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        spec
        (mcp-spec-from-parsed parsed)

        result
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-test-server!)
          name
          spec)]

    (stdout! (str "connected=" (boolean (get result "is_connected"))
                  "  tools=" (count (get result "tools"))))))

(defn- cli-mcp-remove!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-delete-server!) name)
    (stdout! (str "Removed MCP server \"" name "\"."))))

(defn- cli-mcp-enable!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-set-server-enabled!)
      name
      true)
    (stdout! (str "Enabled MCP server \"" name "\"."))))

(defn- cli-mcp-disable!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-set-server-enabled!)
      name
      false)
    (stdout! (str "Disabled MCP server \"" name "\"."))))

(defn- cli-mcp-kill!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-kill-server!) name)
    (stdout! (str "Killed MCP server \"" name "\" (held down until started again)."))))

(defn- cli-mcp-start!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-start-server!) name)
    (stdout! (str "Started MCP server \"" name "\"."))))

(defn- cli-mcp-auth-start!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        {:strs [flow_id url redirect_uri]}
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-auth-start!) name)]

    (stdout! (str "Step 1. Open this URL in a browser and approve access for \"" name "\":"))
    (stdout! (str "  " url))
    (stdout! "")
    (stdout! (str "Step 2. The provider redirects to a loopback URL (" redirect_uri "?code=...)."))
    (stdout! "        Copy that FULL redirect URL (it works even if the page shows an error --")
    (stdout! "        the code is in the URL bar) and run:")
    (stdout! (str "  vis-agent gateway mcp auth-complete "
                  name
                  " --flow-id "
                  flow_id
                  " --input \"<PASTED_URL>\""))
    (stdout! "")
    (stdout! (str "flow_id=" flow_id))))

(defn- cli-mcp-auth-complete!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        flow-id
        (get parsed "flow-id")

        input
        (get parsed "input")

        {:strs [status] :as result}
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-auth-complete!)
          name
          flow-id
          input)]

    (stdout! (str "auth status: " (or status "unknown")))
    (when (= "error" status)
      (stdout! (str "  " (or (get result "message") "Authorization failed."))))))

(defn- cli-mcp-auth-poll!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        flow-id
        (get parsed "flow-id")

        {:strs [status] :as result}
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-auth-poll!)
          name
          flow-id)]

    (stdout! (str "auth status: " (or status "unknown")))
    (when (= "error" status)
      (stdout! (str "  " (or (get result "message") "Authorization failed."))))))

(defn- cli-mcp-auth-cancel!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name
        (require-mcp-name! parsed)

        flow-id
        (get parsed "flow-id")]

    ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-auth-cancel!) name flow-id)
    (stdout! (str "Cancelled auth flow for \"" name "\"."))))

(defn- cli-mcp-auth-logout!
  [parsed _residual]
  (with-mcp-db! parsed)
  (let [name (require-mcp-name! parsed)]
    ((requiring-resolve 'com.blockether.vis.internal.gateway.client/mcp-auth-logout!) name)
    (stdout! (str "Signed out of MCP server \"" name "\" (tokens forgotten)."))))



;;; ── `vis-agent python` — standalone GraalPy interpreter ────────────────────────
;;
;; Expose JUST the embedded GraalPy sandbox -- every foundation shim
;; (requests/pandas/numpy/yaml/sqlite3/...), the POSIX-compat shim, and
;; the auto-imports -- with NO agent tool bindings. Handy for reproducing
;; sandbox behaviour and exercising shims straight from the shell. Behaves
;; identically under the JVM and the native image: both drive the same
;; `env/*` machinery.

(defn- python-cli-context
  "Build a fresh standalone GraalPy sandbox for `vis-agent python`: all shims
   installed, filesystem rooted at the current working directory, network
   enabled unless `network?` is false. No tool bindings -- just the
   interpreter with its shims.

   Unlike the agent sandbox this is a HUMAN-run interpreter, so it gets
   real-`python` niceties: `argv` is bound to `sys.argv`; `env` is merged
   into `os.environ`; and `sys.path` is prepended with `PYTHONPATH`, the
   configured `python.source_paths`, and any `src`-layout import root the
   project's packaging metadata declares. The
   process stdin is wired to guest `sys.stdin`, so it works alongside `-c`/FILE."
  [{:keys [network? argv env]}]
  (let [cwd
        (.getCanonicalPath (io/file "."))

        {:keys [python-context]}
        (env/create-python-context {}
                                   (fn []
                                     [cwd])
                                   {:enabled? (boolean network?)}
                                   System/in
                                   config/original-stderr)]

    ;; Bind an empty standing `ctx` dict so the async runtime has it available.
    (env/bind-ctx! python-context {})
    ;; Forward script argv + (by default) the caller's env — real-python CLI
    ;; semantics, distinct from the scrubbed agent sandbox.
    (env/seed-cli-runtime! python-context {:argv argv :env env})
    ;; GraalPy receives the environment after interpreter startup, so PYTHONPATH
    ;; needs the same explicit sys.path setup a process launch would perform.
    ;; Explicit entries come first; configured and inferred project roots are
    ;; merged in after them, never replacing what the caller asked for.
    (let [separator
          java.io.File/pathSeparator

          explicit
          (remove str/blank?
            (str/split (or (get env "PYTHONPATH") "")
                       (re-pattern (java.util.regex.Pattern/quote separator))))

          roots
          (distinct (concat explicit (pyproj/import-roots python-context cwd)))]

      (when (seq roots)
        (let [^org.graalvm.polyglot.Value bindings
              (.getBindings ^org.graalvm.polyglot.Context python-context "python")

              binding-name
              "__vis_cli_pythonpath__"]

          (.putMember bindings binding-name (str/join separator roots))
          (try (.eval ^org.graalvm.polyglot.Context python-context
                      "python"
                      (str "import os, sys\n"
                           "sys.path[:0] = [p for p in globals()["
                           (pr-str binding-name)
                           "].split(os.pathsep) if p]\n"))
               (finally (.removeMember bindings binding-name))))))
    python-context))

(defn- run-python-source!
  "Evaluate one Python source block in `ctx`, rendering its outcome to the
   real terminal. Returns the process exit code (0 ok, 1 on a raised error).
   Renders like a shell REPL: captured `print(...)` output surfaces; with no
   stdout but a value, the value's repr is echoed (CPython-like)."
  [ctx code]
  (let [{:keys [stdout result error]} (env/run-python-block ctx code)]
    (cond error (do (stdout! (or (:message error) (pr-str error))) 1)
          (and (some? stdout) (seq stdout)) (do (write-stdout! stdout) 0)
          (some? result) (do (stdout! (pr-str result)) 0)
          :else 0)))

(defn- python-repl!
  "Minimal interactive REPL over one persistent standalone sandbox `ctx`.
   Reads a whole block (terminated by a blank line, so multi-line defs work),
   evaluates it, and prints captured stdout. Ctrl-D / EOF quits."
  [ctx]
  (stdout! (str "vis-agent python -- embedded GraalPy sandbox (all shims, no tools). "
                "Blank line runs the block; use print(...) to see output; Ctrl-D quits."))
  (let [reader (java.io.BufferedReader. (java.io.InputStreamReader. System/in))]
    (loop []

      (write-stdout! ">>> ")
      (let [buf (StringBuilder.)
            eof?
            (loop []

              (let [line (.readLine reader)]
                (cond (nil? line) true
                      (str/blank? line) false
                      :else
                      (do (.append buf line) (.append buf "\n") (write-stdout! "... ") (recur)))))
            code (str/trim (.toString buf))]

        (when (seq code) (run-python-source! ctx code))
        (if eof? (stdout! "") (recur))))))

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
        (= "-" (first prog)) {:mode :stdin :argv (vec prog)}
        :else {:mode :file :file (first prog) :argv (vec prog)}))

(defn- parse-python-cli-args
  "Parse `vis-agent python` residual args into a runtime plan. Leading options
   (`--no-network`, `--no-env`, `--env K=V`, and an explicit `--`) are
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

         args
         (vec residual)]

    (let [a (first args)]
      (cond (nil? a) {:network? network?
                      :inherit-env? inherit-env?
                      :env-overrides env-overrides
                      :mode :interactive
                      :argv []}
            (= a "--no-network") (recur false inherit-env? env-overrides (subvec args 1))
            (= a "--no-env") (recur network? false env-overrides (subvec args 1))
            (= a "--env") (recur network?
                                 inherit-env?
                                 (cond-> env-overrides
                                   (some? (second args))
                                   (conj (second args)))
                                 (subvec args (min (count args) 2)))
            (str/starts-with? a "--env=")
            (recur network? inherit-env? (conj env-overrides (subs a 6)) (subvec args 1))
            :else
            (let [prog (if (= a "--") (subvec args 1) args)]
              (merge
                {:network? network? :inherit-env? inherit-env? :env-overrides env-overrides}
                (if (empty? prog) {:mode :interactive :argv []} (python-program-plan prog))))))))

(def ^:private python-module-runner-src
  "Python helper installed for `vis-agent python -m MODULE`.

   Real CPython drives `-m` through `runpy`, which needs a loader that can hand
   back module CODE. Sandbox shims are synthesised `types.ModuleType` objects
   (no file, no loader), so `runpy` can never run them -- for those the console
   entry point (`console_main`/`main`, called with `sys.argv[1:]`) IS the module's
   `__main__`. Everything else (real stdlib modules and packages on disk) falls
   through to `runpy` with CPython semantics."
  (env/runtime-python-src "vis-python/module_runner.py"))

(defn- run-python-module!
  "Run `MODULE` as `__main__` in `ctx` (`vis-agent python -m MODULE`), rendering its
   output to the real terminal. Returns the module's exit code."
  [ctx module]
  (if (str/blank? module)
    (do (stderr! "vis-agent python -m requires a MODULE argument.") 2)
    (let [exit-name
          "__vis_cli_exit_code__"

          {:keys [stdout result error]}
          (env/run-python-block ctx
                                (str python-module-runner-src
                                     "\nglobals()["
                                     (pr-str exit-name)
                                     "] = __vis_run_module__("
                                     (pr-str module)
                                     ")\n"))

          ^org.graalvm.polyglot.Value bindings
          (.getBindings ^org.graalvm.polyglot.Context ctx "python")

          exit-code
          (try (env/->clj (.getMember bindings exit-name))
               (finally (.removeMember bindings exit-name)))]

      (when (seq stdout) (write-stdout! stdout))
      (cond error (do (stdout! (or (:message error) (pr-str error))) 1)
            (integer? exit-code) (int exit-code)
            (integer? result) (int result)
            :else 0))))

(defn- cli-python!
  "`vis-agent python` -- run code in the embedded GraalPy sandbox (all shims, no tool
   bindings). Modes: `-c CODE` (run a string), `FILE.py` (run a file), `-` or
   piped stdin (run stdin), or an interactive REPL on a bare TTY. Trailing args
   after the program selector become `sys.argv`. `--no-network` disables sandbox
   network; the caller's environment is inherited into `os.environ` by default
   (`--no-env` scrubs it, `--env K=V` sets/overrides one var)."
  [_parsed residual]
  (config/init-cli!)
  (let [{:keys [network? inherit-env? env-overrides mode code file module argv]}
        (parse-python-cli-args residual)

        env
        (merge (if inherit-env? (into {} (System/getenv)) {})
               (python-cli-env-overrides->map env-overrides))

        ctx
        (python-cli-context {:network? network? :argv argv :env env})

        exit
        (case mode
          :code
          (if code
            (run-python-source! ctx code)
            (do (stderr! "vis-agent python -c requires a CODE argument.") 2))

          :stdin
          (run-python-source! ctx (slurp System/in))

          :file
          (let [f (io/file file)]
            (if (.isFile f)
              (run-python-source! ctx (slurp f))
              (do (stderr! (str "vis-agent python: no such file: " file)) 2)))

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
;; `providers`, `sessions`, `doctor`, `runtime`, `update`, and `ext` are the
;; binary's own parent commands. They live at the top of the command
;; tree -- `vis-agent providers ...`, NOT `vis-agent extension providers ...` -- so they
;; bypass `:ext/cli` (the `vis-agent extension` subcommand slot). Direct
;; `register-cmd!` is the right plumbing here; vis-runtime is the host,
;; not an extension contributing to `vis-agent extension`.

(doseq
  [spec
   [{:cmd/name "providers"
     :cmd/doc "Inspect, authenticate, and introspect LLM providers."
     :cmd/usage "vis-agent providers <list|status|limits|auth|logout> [...]"
     :cmd/subcommands #(registry/registered-under ["providers"])}
    {:cmd/name "sessions"
     :cmd/doc "List, show, fork, delete, search, or export persisted sessions."
     :cmd/usage "vis-agent sessions <list|show|fork|delete|search|export> [...]"
     :cmd/examples
     ["vis-agent sessions" "vis-agent sessions list" "vis-agent sessions show 3a7b2c1d"
      "vis-agent sessions fork 3a7b2c1d --title \"Branch A\""
      "vis-agent sessions export 3a7b2c1d --md" "vis-agent sessions export 3a7b2c1d --html out.html"
      "vis-agent sessions search \"foo bar\""]
     :cmd/subcommands #(registry/registered-under ["sessions"])
     :cmd/run-fn cli-sessions!}
    {:cmd/name "projects"
     :cmd/doc "List projects, or delete one (optionally with every session in it)."
     :cmd/usage "vis-agent projects <list|delete> [...]"
     :cmd/examples ["vis-agent projects" "vis-agent projects list"
                    "vis-agent projects delete 9f2c1a44"
                    "vis-agent projects delete 9f2c1a44 --with-sessions"]
     :cmd/subcommands #(registry/registered-under ["projects"])
     :cmd/run-fn cli-projects!}
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
     :cmd/run-fn cli-doctor!}
    {:cmd/name "extension"
     :cmd/doc "Inspect, scaffold, or run an extension-contributed CLI command."
     :cmd/usage "vis-agent extension <list|scaffold|...> [args...]"
     :cmd/subcommands #(registry/registered-under ["extension"])}
    {:cmd/name "gateway"
     :cmd/doc "Start, inspect, or stop the long-lived gateway daemon."
     :cmd/usage
     "vis-agent [--gateway HOST[:PORT] --gateway-token TOKEN] gateway <start|status|stop|pair> [--db PATH]"
     :cmd/subcommands #(registry/registered-under ["gateway"])}
    {:cmd/name "python"
     :cmd/doc "Run code in the embedded GraalPy sandbox (all shims, no tool bindings)."
     :cmd/usage "vis-agent python [OPTS] [-c CODE | -m MODULE | FILE.py | -] [ARG...]"
     :cmd/examples
     ["vis-agent python -c \"import requests; print(requests.__version__)\""
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

;;; ── `vis-agent gateway` subcommands ──────────────────────────────────────────

(doseq
  [spec
   [{:cmd/name "start"
     :cmd/parent ["gateway"]
     :cmd/doc
     "Start the long-lived gateway daemon (HTTP + SSE runtime) in the foreground, always on THIS machine."
     :cmd/usage
     "vis-agent gateway start [--port 7890] [--host 127.0.0.1] [--token-file PATH] [--pair]"
     :cmd/args
     [{:name "port" :kind :flag :type :string :doc "TCP port to listen on (default 7890)."}
      {:name "host"
       :kind :flag
       :type :string
       :doc
       "Bind host (default 127.0.0.1, or a phone-reachable host when --pair is given; non-loopback always requires the token)."}
      {:name "token-file"
       :kind :flag
       :type :string
       :doc "Bearer-token file (default ~/.vis/gateway.token, minted on first run)."}
      {:name "db"
       :kind :flag
       :type :string
       :doc "SQLite DB path this daemon owns (default ~/.vis/vis.mdb or VIS_DB_PATH)."}
      {:name "require-token"
       :kind :flag
       :type :boolean
       :doc
       "Require the bearer token on loopback too (auth is OFF by default on 127.0.0.1; a non-loopback bind always requires it)."}
      {:name "pair"
       :kind :flag
       :type :boolean
       :doc
       "Print a VIS companion pairing QR (URL + bearer token). Implies a phone-reachable bind (Tailscale IP, else 0.0.0.0) unless --host says otherwise."}]
     :cmd/examples ["vis-agent gateway start" "vis-agent gateway start --port 8080"
                    "vis-agent gateway start --pair"
                    "vis-agent gateway start --host 0.0.0.0 --require-token --pair"]
     :cmd/run-fn cli-gateway-start!}
    {:cmd/name "status"
     :cmd/parent ["gateway"]
     :cmd/doc
     "Show the gateway this invocation drives — the --gateway target, else the daemon registered for the current DB — without starting it."
     :cmd/usage "vis-agent gateway status [--db PATH]"
     :cmd/args
     [{:name "db"
       :kind :flag
       :type :string
       :doc
       "SQLite DB path whose gateway registry should be inspected (ignored when --gateway names a remote gateway)."}]
     :cmd/examples ["vis-agent gateway status"
                    "vis-agent --gateway 10.0.0.5 --gateway-token TOKEN gateway status"]
     :cmd/run-fn cli-gateway-status!}
    {:cmd/name "stop"
     :cmd/parent ["gateway"]
     :cmd/doc
     "Stop the gateway daemon registered for the current DB - never a --gateway target, which vis attaches to but never manages."
     :cmd/usage "vis-agent gateway stop [--if-idle] [--db PATH]"
     :cmd/args
     [{:name "db" :kind :flag :type :string :doc "SQLite DB path whose gateway should be stopped."}
      {:name "if-idle"
       :kind :flag
       :type :boolean
       :doc
       "Stop it only when stopping is free: an auto-spawned daemon with no client and no turn still moving. Prints why it did not otherwise, is silent when none runs, and always succeeds."}]
     :cmd/examples ["vis-agent gateway stop" "vis-agent gateway stop --if-idle"]
     :cmd/run-fn cli-gateway-stop!}
    {:cmd/name "pair"
     :cmd/parent ["gateway"]
     :cmd/doc
     "Print a companion pairing QR for the gateway already running for this DB, or for the --gateway target."
     :cmd/usage "vis-agent gateway pair [--db PATH]"
     :cmd/examples ["vis-agent gateway pair"
                    "vis-agent --gateway 10.0.0.5 --gateway-token TOKEN gateway pair"]
     :cmd/args [{:name "db"
                 :kind :flag
                 :type :string
                 :doc "SQLite DB path whose running gateway should be paired."}]
     :cmd/run-fn cli-gateway-pair!}
    {:cmd/name "mcp"
     :cmd/parent ["gateway"]
     :cmd/doc
     "Manage gateway-owned MCP servers: add, list, test, enable/disable, kill/start, and OAuth sign-in."
     :cmd/usage
     "vis-agent gateway mcp <list|add|test|remove|enable|disable|kill|start|auth-start|auth-complete|auth-poll|auth-cancel|auth-logout>"
     :cmd/subcommands #(registry/registered-under ["gateway" "mcp"])}]]
  (registry/register-cmd! spec))

;;; ── `vis-agent gateway mcp` subcommand registrations ─────────────────────────────

(doseq
  [spec
   [{:cmd/name "list"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc
     "List MCP servers configured on this gateway (transport, enabled, connected, authorized, tool count)."
     :cmd/usage "vis-agent gateway mcp list [--db PATH]"
     :cmd/args
     [{:name "db" :kind :flag :type :string :doc "SQLite DB path whose gateway should be queried."}]
     :cmd/examples ["vis-agent gateway mcp list"]
     :cmd/run-fn cli-mcp-list!}
    {:cmd/name "add"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc
     "Add (or replace) a gateway-managed MCP server. A `--url` makes it Streamable HTTP; a `--command` makes it stdio. OAuth is never configured here -- save the server, then run `auth-start`."
     :cmd/usage
     "vis-agent gateway mcp add <NAME> (--url URL [--headers K=V,...] | --command CMD [--args \"a b\"] [--cwd DIR] [--env K=V,...]) [--timeout-ms MS] [--disabled] [--db PATH]"
     :cmd/args
     [{:name "name"
       :kind :positional
       :type :string
       :required true
       :doc "Server name, e.g. \"linear\"."}
      {:name "url"
       :kind :flag
       :type :string
       :doc "Streamable HTTP endpoint, e.g. https://mcp.linear.app/mcp."}
      {:name "headers"
       :kind :flag
       :type :string
       :doc
       "Static request headers as K=V,K2=V2 (e.g. Authorization=Bearer TOKEN) -- the bearer-token alternative to OAuth. Leave unset for OAuth servers; sign in with auth-start instead."}
      {:name "command" :kind :flag :type :string :doc "Executable for a stdio server."}
      {:name "args" :kind :flag :type :string :doc "Space-separated stdio arguments."}
      {:name "cwd" :kind :flag :type :string :doc "Working directory for a stdio server."}
      {:name "env" :kind :flag :type :string :doc "stdio environment as K=V,K2=V2."}
      {:name "timeout-ms" :kind :flag :type :int :doc "Per-call timeout override in milliseconds."}
      {:name "disabled" :kind :flag :type :boolean :doc "Save the server switched off."}
      {:name "db" :kind :flag :type :string :doc "SQLite DB path whose gateway should be updated."}]
     :cmd/examples
     ["vis-agent gateway mcp add linear --url https://mcp.linear.app/mcp"
      "vis-agent gateway mcp add linear-ro --url https://mcp.linear.app/mcp/readonly"
      "vis-agent gateway mcp add local-fs --command npx --args \"-y @modelcontextprotocol/server-filesystem /tmp\""]
     :cmd/run-fn cli-mcp-add!}
    {:cmd/name "test"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc
     "Connect a candidate server spec without saving it, and report whether it connected and how many tools it exposes."
     :cmd/usage "vis-agent gateway mcp test <NAME> (--url URL | --command CMD ...) [--db PATH]"
     :cmd/args
     [{:name "name" :kind :positional :type :string :required true :doc "Server name to test as."}
      {:name "url" :kind :flag :type :string :doc "Streamable HTTP endpoint."}
      {:name "headers" :kind :flag :type :string :doc "Static request headers as K=V,K2=V2."}
      {:name "command" :kind :flag :type :string :doc "Executable for a stdio server."}
      {:name "args" :kind :flag :type :string :doc "Space-separated stdio arguments."}
      {:name "cwd" :kind :flag :type :string :doc "Working directory for a stdio server."}
      {:name "env" :kind :flag :type :string :doc "stdio environment as K=V,K2=V2."}
      {:name "timeout-ms" :kind :flag :type :int :doc "Per-call timeout override in milliseconds."}
      {:name "db"
       :kind :flag
       :type :string
       :doc "SQLite DB path whose gateway should run the test."}]
     :cmd/examples ["vis-agent gateway mcp test linear --url https://mcp.linear.app/mcp"]
     :cmd/run-fn cli-mcp-test!}
    {:cmd/name "remove"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc "Delete a gateway-managed MCP server and stop it now."
     :cmd/usage "vis-agent gateway mcp remove <NAME> [--db PATH]"
     :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
                {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/examples ["vis-agent gateway mcp remove linear"]
     :cmd/run-fn cli-mcp-remove!}
    {:cmd/name "enable"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc "Turn a configured server back on."
     :cmd/usage "vis-agent gateway mcp enable <NAME> [--db PATH]"
     :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
                {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/run-fn cli-mcp-enable!}
    {:cmd/name "disable"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc "Turn a configured server off without deleting it."
     :cmd/usage "vis-agent gateway mcp disable <NAME> [--db PATH]"
     :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
                {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/run-fn cli-mcp-disable!}
    {:cmd/name "kill"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc
     "Stop a server now and hold it down until started again (runtime only, config unchanged)."
     :cmd/usage "vis-agent gateway mcp kill <NAME> [--db PATH]"
     :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
                {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/run-fn cli-mcp-kill!}
    {:cmd/name "start"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc "Undo a kill and reconnect a server now."
     :cmd/usage "vis-agent gateway mcp start <NAME> [--db PATH]"
     :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
                {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/run-fn cli-mcp-start!}
    {:cmd/name "auth-start"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc
     "Begin the headless OAuth 2.1 flow (RFC 9728/8414 discovery, dynamic client registration, PKCE) for an HTTP server. Prints the authorize URL to open and the auth-complete command to run after."
     :cmd/usage "vis-agent gateway mcp auth-start <NAME> [--db PATH]"
     :cmd/args [{:name "name"
                 :kind :positional
                 :type :string
                 :required true
                 :doc "Server name (must be Streamable HTTP)."}
                {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/examples ["vis-agent gateway mcp auth-start linear"]
     :cmd/run-fn cli-mcp-auth-start!}
    {:cmd/name "auth-complete"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc
     "Finish an OAuth flow with the redirect URL the browser landed on (or its bare `code=` value)."
     :cmd/usage
     "vis-agent gateway mcp auth-complete <NAME> --flow-id ID --input URL_OR_CODE [--db PATH]"
     :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
                {:name "flow-id"
                 :kind :flag
                 :type :string
                 :required true
                 :doc "flow_id printed by auth-start."}
                {:name "input"
                 :kind :flag
                 :type :string
                 :required true
                 :doc "The pasted redirect URL, or its bare authorization code."}
                {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/examples
     ["vis-agent gateway mcp auth-complete linear --flow-id abc123 --input \"http://127.0.0.1:5555/callback?code=xyz&state=...\""]
     :cmd/run-fn cli-mcp-auth-complete!}
    {:cmd/name "auth-poll"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc "Read an in-flight OAuth flow's verdict without blocking: pending, ok, or error."
     :cmd/usage "vis-agent gateway mcp auth-poll <NAME> --flow-id ID [--db PATH]"
     :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
                {:name "flow-id"
                 :kind :flag
                 :type :string
                 :required true
                 :doc "flow_id printed by auth-start."}
                {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/run-fn cli-mcp-auth-poll!}
    {:cmd/name "auth-cancel"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc "Abandon an in-flight OAuth flow and release its loopback listener."
     :cmd/usage "vis-agent gateway mcp auth-cancel <NAME> --flow-id ID [--db PATH]"
     :cmd/args
     [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
      {:name "flow-id" :kind :flag :type :string :required true :doc "flow_id to abandon."}
      {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/run-fn cli-mcp-auth-cancel!}
    {:cmd/name "auth-logout"
     :cmd/parent ["gateway" "mcp"]
     :cmd/doc "Forget the gateway's persisted OAuth tokens for a server."
     :cmd/usage "vis-agent gateway mcp auth-logout <NAME> [--db PATH]"
     :cmd/args [{:name "name" :kind :positional :type :string :required true :doc "Server name."}
                {:name "db" :kind :flag :type :string :doc "SQLite DB path."}]
     :cmd/examples ["vis-agent gateway mcp auth-logout linear"]
     :cmd/run-fn cli-mcp-auth-logout!}]]
  (registry/register-cmd! spec))


;;; ── `vis-agent providers` subcommands ─────────────────────────────────────────

(doseq [spec [{:cmd/name "list"
               :cmd/parent ["providers"]
               :cmd/doc "List registered providers with auth state, static limits, and base URLs."
               :cmd/usage "vis-agent providers list"
               :cmd/run-fn cli-providers-list!}
              {:cmd/name "status"
               :cmd/parent ["providers"]
               :cmd/doc "Show provider authentication status together with static/dynamic limits."
               :cmd/usage "vis-agent providers status [provider]"
               :cmd/examples ["vis-agent providers status"
                              "vis-agent providers status github-copilot-business"
                              "vis-agent providers status openai-codex"]
               :cmd/run-fn cli-providers-status!}
              {:cmd/name "limits"
               :cmd/parent ["providers"]
               :cmd/doc "Show provider rate-limit metadata and any dynamic quota report."
               :cmd/usage "vis-agent providers limits [provider]"
               :cmd/examples ["vis-agent providers limits" "vis-agent providers limits openai-codex"
                              "vis-agent providers limits ollama"]
               :cmd/run-fn cli-providers-limits!}
              {:cmd/name "auth"
               :cmd/parent ["providers"]
               :cmd/doc "Run a provider's interactive authentication flow."
               :cmd/usage "vis-agent providers auth <provider>"
               :cmd/args
               [{:name "provider"
                 :kind :positional
                 :type :string
                 :doc
                 "Registered provider id (for example: github-copilot-business or openai-codex)."}]
               :cmd/examples ["vis-agent providers auth github-copilot-business"
                              "vis-agent providers auth github-copilot-individual"
                              "vis-agent providers auth openai-codex"]
               :cmd/run-fn cli-providers-auth!}
              {:cmd/name "logout"
               :cmd/parent ["providers"]
               :cmd/doc "Clear saved credentials for a provider."
               :cmd/usage "vis-agent providers logout <provider>"
               :cmd/args
               [{:name "provider" :kind :positional :type :string :doc "Registered provider id."}]
               :cmd/examples ["vis-agent providers logout github-copilot-business"
                              "vis-agent providers logout github-copilot-individual"
                              "vis-agent providers logout openai-codex"]
               :cmd/run-fn cli-providers-logout!}]]
  (registry/register-cmd! spec))

;;; ── `vis-agent sessions` subcommands ──────────────────────────────────────────

(doseq
  [spec
   [{:cmd/name "list"
     :cmd/parent ["sessions"]
     :cmd/doc "List persisted sessions."
     :cmd/usage "vis-agent sessions list [all|tui|cli]"
     :cmd/args [{:name "channel"
                 :kind :positional
                 :type :string
                 :doc "Optional channel filter (all|tui|cli; default all)."}]
     :cmd/examples ["vis-agent sessions list" "vis-agent sessions list tui"]
     :cmd/run-fn cli-sessions-list!}
    {:cmd/name "show"
     :cmd/parent ["sessions"]
     :cmd/doc "Show one session's metadata, turns, and fork states."
     :cmd/usage "vis-agent sessions show <SESSION-ID>"
     :cmd/args [{:name "session-id"
                 :kind :positional
                 :type :string
                 :required true
                 :doc "Session id (full UUID or unambiguous prefix)."}]
     :cmd/examples ["vis-agent sessions show 3a7b2c1d"]
     :cmd/run-fn cli-show-session!}
    {:cmd/name "fork"
     :cmd/parent ["sessions"]
     :cmd/doc "Fork a session from its latest state."
     :cmd/usage "vis-agent sessions fork <SESSION-ID> [--title TITLE]"
     :cmd/args [{:name "session-id"
                 :kind :positional
                 :type :string
                 :required true
                 :doc "Session id (full UUID or unambiguous prefix)."}
                {:name "title" :kind :flag :type :string :doc "Title to set on the new fork."}]
     :cmd/examples ["vis-agent sessions fork 3a7b2c1d"
                    "vis-agent sessions fork 3a7b2c1d --title \"Branch A\""]
     :cmd/run-fn cli-fork-session-command!}
    {:cmd/name "delete"
     :cmd/parent ["sessions"]
     :cmd/doc "Delete a session tree from persistent storage."
     :cmd/usage "vis-agent sessions delete <SESSION-ID>"
     :cmd/args [{:name "session-id"
                 :kind :positional
                 :type :string
                 :required true
                 :doc "Session id (full UUID or unambiguous prefix)."}]
     :cmd/examples ["vis-agent sessions delete 3a7b2c1d"]
     :cmd/run-fn cli-delete-session!}
    {:cmd/name "export"
     :cmd/parent ["sessions"]
     :cmd/doc
     "Export a session: Markdown on stdout, HTML to a file, or a headless MP4 screencast of the TUI transcript."
     :cmd/usage "vis-agent sessions export <SESSION-ID> [--md | --html PATH | --mp4 PATH]"
     :cmd/args
     [{:name "session-id"
       :kind :positional
       :type :string
       :required true
       :doc "Session id (full UUID or unambiguous prefix)."}
      {:name "md" :kind :flag :type :boolean :doc "Print Markdown to stdout (default)."}
      {:name "html" :kind :flag :type :string :doc "Write styled HTML export to PATH."}
      {:name "mp4"
       :kind :flag
       :type :string
       :doc "Write a pure-JVM H.264 .mp4 screencast of the (uncollapsed) TUI transcript to PATH."}]
     :cmd/examples ["vis-agent sessions export 3a7b2c1d --md"
                    "vis-agent sessions export 3a7b2c1d --html out.html"
                    "vis-agent sessions export 3a7b2c1d --mp4 session.mp4"]
     :cmd/run-fn cli-export-session!}
    {:cmd/name "search"
     :cmd/parent ["sessions"]
     :cmd/doc "Transcript search with the same matching semantics as the TUI session navigator."
     :cmd/usage "vis-agent sessions search <query> [--limit N]"
     :cmd/args [{:name "query"
                 :kind :positional
                 :type :string
                 :doc "Words to search for (case-insensitive token prefixes, as in the TUI)."}
                {:name "limit" :kind :flag :type :string :doc "Max hits to print (default 25)."}]
     :cmd/examples ["vis-agent sessions search \"provider credentials\""
                    "vis-agent sessions search \"authentication failed\" --limit 100"]
     :cmd/run-fn cli-sessions-search!}]]
  (registry/register-cmd! spec))

;;; ── `vis-agent projects` subcommands ──────────────────────────────────────────

(doseq
  [spec
   [{:cmd/name "list"
     :cmd/parent ["projects"]
     :cmd/doc "List projects with their live session counts."
     :cmd/usage "vis-agent projects list"
     :cmd/examples ["vis-agent projects list"]
     :cmd/run-fn cli-projects-list!}
    {:cmd/name "delete"
     :cmd/parent ["projects"]
     :cmd/doc "Delete a project; with --with-sessions, every session in it too."
     :cmd/usage "vis-agent projects delete <PROJECT-ID> [--with-sessions]"
     :cmd/args
     [{:name "project-id"
       :kind :positional
       :type :string
       :required true
       :doc "Project id (full UUID or unambiguous prefix)."}
      {:name "with-sessions"
       :kind :flag
       :type :boolean
       :doc
       "Also delete every session in the project (drafts included), not just the project row."}]
     :cmd/examples ["vis-agent projects delete 9f2c1a44"
                    "vis-agent projects delete 9f2c1a44 --with-sessions"]
     :cmd/run-fn cli-delete-project!}]]
  (registry/register-cmd! spec))

;;; ── `vis-agent extension` subcommands (host-owned canonical) ────────────────────────
;;
;; `list` and `scaffold` are NOT extension contributions -- they are
;; the CANONICAL host commands the vis-agent binary ships with. Extensions add
;; to the `vis-agent extension` parent through `:ext/cli`; the host marks its own
;; entries with `:cmd/internal? true` so help and listing layers can
;; tell host-owned canonical commands apart from extension-contributed
;; ones at a glance.

(doseq [spec [{:cmd/name "list"
               :cmd/parent ["extension"]
               :cmd/internal? true
               :cmd/doc "List every registered extension with metadata."
               :cmd/usage "vis-agent extension list"
               :cmd/run-fn cli-extensions!}
              {:cmd/name "scaffold"
               :cmd/parent ["extension"]
               :cmd/internal? true
               :cmd/doc "Create a user extension project scaffold."
               :cmd/usage
               "vis-agent extension scaffold <name> [--dir DIR] [--namespace NS] [--force]"
               :cmd/examples
               ["vis-agent extension scaffold my-tools"
                "vis-agent extension scaffold my-tools --dir ~/.vis/vis-extensions/my-tools"]
               :cmd/run-fn cli-extensions-scaffold!}
              {:cmd/name "check"
               :cmd/parent ["extension"]
               :cmd/internal? true
               :cmd/doc "Statically check Python extension files without running them."
               :cmd/usage "vis-agent extension check [PATH ...]"
               :cmd/examples ["vis-agent extension check"
                              "vis-agent extension check .vis/extensions/deploy.py"]
               :cmd/run-fn cli-extension-check!}]]
  (registry/register-cmd! spec))

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
  "Classify only the two long-lived process surfaces. Embedded Python executes
   inside gateway; every short-lived command keeps the neutral `vis` role."
  [args]
  (cond (= ["channels" "tui"] (vec (take 2 args))) "tui"
        (= ["gateway" "start"] (vec (take 2 args))) "gateway"
        :else "vis"))

(defn- configure-logging!
  "Route Telemere signals: file handler always on, persistence-backed
   `:db` handler always on (so the loop's `tel/with-ctx+ {:db-info ...}`
   bindings land in the session_log table), and the
   `:default/console` handler is OFF by default - it was removed by
   `internal.registry` at namespace load so boot-time registration
   logs never spray to stdout. We re-add it here only when `--debug`
   / `--verbose` / `-v` / `VIS_DEBUG=1` is set. Idempotent."
  [args]
  (let [debug?
        (debug-mode? args)

        path
        (log-file-path)]

    ;; File handler ALWAYS on, so post-mortem reads always have data.
    (try (tel/add-handler! :file
                           (tel/handler:file (assoc config/diagnostic-log-options :path path))
                           {:min-level :info})
         (catch Throwable _ nil))
    ;; Console handler: re-add only when the user asked for verbosity.
    ;; Boot-time noise is already gone (registry.clj removed it during
    ;; namespace load); this restores the stdout stream for debugging.
    (when debug?
      (try (tel/add-handler! :default/console (tel/handler:console)) (catch Throwable _ nil)))
    ;; Persistence handler: scopes signals to the right DB rows via
    ;; `:db-info` / `:session-soul-id` / `:session-turn-id` /
    ;; `:iteration-id` carried in telemere `*ctx*`. Wrapped because
    ;; the persistence facade is loaded lazily; if no backend has
    ;; registered yet, the handler will silently drop signals until
    ;; one does.
    (try (setup-db-handler!) (catch Throwable _ nil))))

;; Extension discovery
;;
;; ONE call. The unified loader lives in the extension facade.

(defn- print-extension-load-failures!
  "Print every classpath extension namespace whose `(require)` blew
   up during the most recent scan to stderr, along with the
   user-actionable hint. Pre-fix the failure was a buried ERROR line in the
   process log under `~/.vis/logs/` and the user had no surface clue that an
   entire alias namespace was unbound - the LLM in the
   sandbox would loop on `Unable to resolve symbol: cat` until
   the user manually dug through the log file. Now the launcher
   shouts the failure on every startup so the user can `git diff`
   the broken extension and fix the typo.

   No-op when every extension loaded cleanly."
  []
  (let [failures (manifest/load-failures)]
    (when (seq failures)
      (binding [*out* *err*]
        (println)
        (println "⚠  vis-agent: " (count failures) "extension namespace(s) failed to load.")
        (println "   The associated alias namespace will be UNBOUND in the sandbox.")
        (println "   The agent will see `Unable to resolve symbol` for every call into it.")
        (println)
        (doseq [{:keys [extension-id extension-ns reason path]} failures]
          (println (str "   • extension '" extension-id "' (" extension-ns ")"))
          (println (str "     " reason))
          (when path (println (str "     manifest: " path))))
        (println)))))

(defn- discover-clojure-extensions!
  "Discover classpath extensions and surface namespace load failures."
  []
  (extension/discover-extensions!)
  (print-extension-load-failures!)
  nil)

(defn discover-all!
  "Run the unified Clojure and Python extension discovery scan. Idempotent through
   Clojure's `require` cache and Python extension fingerprints. Returns nil.

   Prints a stderr banner enumerating every extension namespace whose `(require)`
   failed during discovery. The same warnings are also fed into the per-turn
   `(:project ctx) :warnings` slice, so both the user (at the terminal) and the LLM
   (reading `ctx`) see the failure immediately instead of bouncing off `Unable to
   resolve symbol` for an entire session."
  []
  (discover-clojure-extensions!)
  (python-extensions/load-python-extensions!)
  nil)

(defn- tui-dispatch? [args] (= ["channels" "tui"] (vec (take 2 args))))

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
     (help-row "--help, -h" "Show help.") "" "GATEWAY (WHICH DAEMON RUNS THE WORK)"
     (help-row "--gateway HOST[:PORT]|URL"
               "Drive another machine's gateway, TUI included (VIS_GATEWAY_URL).")
     (help-row "--gateway-token TOKEN" "Bearer token that gateway requires (VIS_GATEWAY_TOKEN).") ""
     "RUNTIME (WHAT RUNS)"
     (help-row "vis-agent runtime" "Name the runtime installed, and where it lives.")
     (help-row "vis-agent update" "Update vis-agent and that runtime together.") "" "CONFIGURATION"
     (help-row "~/.vis/config.yml" "Global settings: providers, models, tools.")
     (help-row "<project>/vis.yml" "Project settings; .vis/config.yml overrides it.")
     (help-row "vis-agent providers status" "Show provider auth and model catalogs.")
     (help-row "vis-agent doctor" "Diagnose config, extensions, stale state.") "" "EXAMPLES"
     "vis-agent \"fix failing tests\"" "vis-agent --json \"summarize this repo\""
     "vis-agent --provider zai-coding-plan --model glm-5.2 --reasoning-effort high --json \"task\""
     "vis-agent --toggles reasoning_level=deep \"refactor carefully\""
     "vis-agent --full-trace-json-stream --db :memory \"debug startup\""
     "vis-agent sessions search sqlite" "vis-agent --gateway 10.0.0.5 --gateway-token TOKEN tui"]))

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
   extension discovery because the root tree lists built-in parent commands
   only; extension-owned commands are mounted below `ext` after
   discovery when that subtree is requested."
  [args]
  (or (empty? args) (contains? #{["help"] ["--help"] ["-h"]} (vec args))))

(defn- version-request?
  "True when args ask only for the version. Like help, this short-circuits
   BEFORE extension discovery / agent boot — `vis-agent --version` must be instant and
   must NOT create the GraalPy sandbox or contact a provider."
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

(def ^:private first-party-channel-bootstrap-nses {"tui" 'com.blockether.vis.ext.channel-tui.core})

(defn- help-request?
  "True when args request help at any command depth. We can usually render
   help without initializing runtime resources; if a command is not registered
   yet, the caller falls back to full extension discovery."
  [args]
  (boolean (or (root-help-request? args) (some #{"--help" "-h"} args))))

(defn- channel-help-request?
  "True for `vis-agent channels <first-party-channel> --help`. These requests need
   only the selected channel descriptor, not every extension namespace."
  [args]
  (let [[parent channel & more] (vec args)]
    (and (= "channels" parent)
         (contains? first-party-channel-bootstrap-nses channel)
         (boolean (some #{"--help" "-h"} more)))))

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
  "True for any `vis-agent extension ...` help invocation. The `vis-agent extension` subtree is
   populated by `:ext/cli` mounts that only land after
   `extension/discover-extensions!` has run, so help rendering for this
   subtree MUST trigger full extension discovery before the renderer
   reads `(registered-under [\"extension\"])`."
  [args]
  (contains? #{"ext" "extension"} (first (vec args))))

(defn- discover-fast-help-deps!
  [args]
  (cond (channel-help-request? args) (when-let [ns-sym (get first-party-channel-bootstrap-nses
                                                            (second (vec args)))]
                                       (require ns-sym))
        (channel-parent-help-request? args) (discover-all!)
        (ext-help-request? args) (discover-all!)))

(defn- fast-help-dispatched?
  [_measure? args]
  (when (help-request? args)
    (discover-fast-help-deps! args)
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

(defn- root-run-shortcut?
  "True when bare `vis-agent ...` should run the one-shot CLI agent.
   Unknown commands that ask for help stay errors, so typo diagnostics
   remain honest (`vis-agent sessions --help` must not become a prompt)."
  [root args]
  (and (unknown-command? root args) (not-any? #{"--help" "-h"} args)))

(defn- exit-with-user-error!
  [^Throwable t]
  ;; Some user errors ship a pre-rendered SCREEN (`:vis/panel`) — a boxed panel
  ;; naming what is wrong and the exact fix (e.g. a gateway/client version
  ;; mismatch). Print that instead of flattening it into one line.
  (if-let [panel (seq (:vis/panel (ex-data t)))]
    (doseq [line panel]
      (stdout! (str line)))
    (stdout! (str "vis-agent: " (or (ex-message t) "error"))))
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

    (stdout! (str "vis-agent: fatal error - " (or (ex-message t) (.getName (class t)))))
    ;; ExceptionInInitializerError etc. carry no message; surface the root cause
    ;; so failures (incl. native-image runtime class-init) are diagnosable.
    (when-not same?
      (stdout! (str "  caused by: "
                    (.getName (class rc))
                    (when-let [m (ex-message rc)]
                      (str ": " m)))))
    ;; full trace when VIS_DEBUG is set — invaluable for native-image triage
    (when (some-> (System/getenv "VIS_DEBUG")
                  (.equalsIgnoreCase "1"))
      (.printStackTrace t))
    (stdout! (str "See " (config/log-path) " for details.")))
  (shutdown-agents)
  (System/exit 1))

(defn- exit-no-provider!
  "Calm, guided message when no AI provider is configured — never a stacktrace.
   Points at the interactive welcome (the curated, zero-friction path)."
  []
  (stdout! "")
  (stdout! "  vis-agent needs an AI provider to get started.")
  (stdout! "")
  (stdout! "  ▸ Run  vis-agent  with no arguments to open the welcome screen and")
  (stdout! "    connect one (Sign in with GitHub / OpenAI / Anthropic, paste an")
  (stdout! "    API key, or run a local model).")
  (stdout! "  ▸ Or hand-write ~/.vis/config.yml.")
  (stdout! "")
  (shutdown-agents)
  (System/exit 2))

(defn- truthy-value? [v] (contains? #{"1" "true" "yes" "on"} (str/lower-case (str v))))

(defn- measure-arg? [arg] (= "--measure" arg))

(def ^:private launcher-owned-args
  ;; `bin/vis-agent` normally consumes these before invoking Clojure, but keep
  ;; the JVM entry point tolerant too (e.g. `clojure -M:vis-agent channels --jfr --help`).
  ;; No runtime selector belongs here: what runs is decided by what is installed.
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

(def ^:private session-shortcut-flags
  ;; Top-level `vis-agent --resume` / `vis-agent --continue` (pi-parity) are
  ;; ergonomic aliases for the `channels tui` session flags.
  {"--resume" "--resume" "-r" "--resume" "--continue" "--continue" "-c" "--continue"})

(defn- rewrite-session-shortcuts
  "Rewrite a leading `--resume`/`-r`/`--continue`/`-c` into
   `channels tui <flag>` so the session shortcuts work at the root.
   Only fires when the shortcut is the FIRST token, so flag-shaped
   one-shot prompts (e.g. `vis-agent --json \"...\"`) are untouched."
  [args]
  (if-let [canon (get session-shortcut-flags (first args))]
    (into ["channels" "tui" canon] (rest args))
    (vec args)))

(defn- rewrite-tui-shortcut
  "Rewrite a leading `tui` into `channels tui` so the terminal UI is a
   first-class top-level command (`vis-agent tui`) that boots the channels TUI.
   Only fires when `tui` is the FIRST token, so it never shadows a prompt."
  [args]
  (if (= "tui" (first args)) (into ["channels" "tui"] (rest args)) (vec args)))

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

(defn- discover-for-dispatch!
  "Discover what `args` needs before command dispatch. The TUI starts Python
   extension loading after its first painted frame; every other command stays eager."
  [measure? args]
  (timed-startup! measure? "discover-clojure-extensions" #(discover-clojure-extensions!))
  (when-not (tui-dispatch? args)
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
  "Discover extensions, walk the command tree, dispatch.

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
  (system-trust/install!)
  (let [main-started
        (System/nanoTime)

        measure?
        (startup-measure? raw-args)

        {gateway :gateway stripped :args}
        (split-gateway-flags (strip-global-args raw-args))

        args
        (-> stripped
            rewrite-session-shortcuts
            rewrite-tui-shortcut
            rewrite-ext-alias)]

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
            :else (do (discover-for-dispatch! measure? args)
                      (when measure? (summarize-startup-registries!))
                      (timed-startup! measure? "pre-redirect-stderr" #(pre-redirect-stderr! args))
                      (let [root
                            (root-command)

                            full-args
                            (cons "vis-agent" args)

                            unknown-root?
                            (unknown-command? root args)]

                        (cond (and unknown-root? (root-run-shortcut? root args))
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
                                  ;; Python extension discovery can spin up GraalPy and extension
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
