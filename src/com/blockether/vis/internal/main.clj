(ns com.blockether.vis.internal.main
  "vis CLI binary - :db Telemere handler, one-shot agent helper,
   built-in CLI commands, and the `-main` dispatcher entry point.

   Everything in this file is binary-only. The library surface
   (iteration loop, turn engine, environment lifecycle, session
   cache) lives in `com.blockether.vis.internal.loop`; this namespace requires
   that one and wires it into the command tree the `vis` binary
   exposes.

   Public entry point:

     (-main & args)   - invoked by the `:vis` alias / `bin/vis`.
                        Configures logging, runs the unified extension
                        discovery scan, redirects stderr to ~/.vis/vis.log
                        for any TTY-owning channel, then dispatches to
                        the resolved command's `:cmd/run-fn`.

   Built-in commands registered here:
     vis providers          - provider inspection, auth, and limits
     vis sessions      - list persisted sessions
     vis ext list           - list registered extensions
     vis channels <name>    - auto-mounted via the channel registry

   `vis doctor` is host-owned. Extensions plug diagnostics into it
   with `:ext/doctor-fn`; extension-owned CLI commands stay under
   `vis ext`."
  (:refer-clojure :exclude [agent run!])
  (:require [babashka.process :as process]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.doctor :as doctor]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.error :as error]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.python-extensions :as python-extensions]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.gateway.state :as gateway-state]
            [com.blockether.vis.internal.manifest :as manifest]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.workspace :as workspace]
            [com.blockether.vis.internal.progress :as progress]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.render :as render]
            [com.blockether.vis.internal.toggles :as toggles]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Persistence-backed Telemere :db handler
;; =============================================================================

;; =============================================================================
;; Signal -> log entry
;; =============================================================================

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
        (try (json/write-json-str (cond-> {}
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

;; =============================================================================
;; Handler
;; =============================================================================

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

;; =============================================================================
;; Extension CLI dispatcher
;; =============================================================================

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
            ;; Positional
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

    (str "  vis ext "
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
      "No extension commands available. Run 'vis ext' to see registered extensions."
      (str "Extension commands:\n\n"
           (str/join "\n\n"
                     (map (fn [{:keys [cmd doc ext-ns]}]
                            (str "  vis ext " (pad cmd 20) (or doc "") "  (" ext-ns ")"))
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

;; =============================================================================
;; Agent helper (root one-shot run)
;; =============================================================================

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
            model-name (subs model* (inc idx))]

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
      (if-let [[provider-id model-name] (split-provider-model model*)]
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
   - :cost         - {:input-cost N :output-cost N :total-cost N :model str}
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
                        :answer (:answer result)
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
                        (:id session))
                      (let [matches (->> (gateway-state/list-sessions)
                                         (map :id)
                                         (filter #(str/starts-with? (str %) s))
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
               (cond-> {:session-id session-id
                        :answer (or (:answer-ir result) (:answer result))
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
             (catch Exception e (run-error-result session-id e)))))))

;;; ── Output Formatting ───────────────────────────────────────────────────

(defn- json-key
  "Return a stable string key for CLI JSON output. Runtime trace maps can
   contain non-JSON map keys. Charred correctly rejects those, so normalize
   keys before writing the public `vis --json` envelope."
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
        :else x))

(defn result->json [result] (json/write-json-str (json-safe result)))

(defn result->edn [result] (pr-str result))

;; =============================================================================
;; Built-in CLI commands
;; =============================================================================

;;; ── Output helpers ──────────────────────────────────────────────────────

(defn- stdout!
  "Print to the real terminal via the saved original stdout. Other
   output (telemere, SLF4J) is redirected to the log file."
  [^String s]
  (.println ^java.io.PrintStream config/original-stdout s)
  (.flush ^java.io.PrintStream config/original-stdout))

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
  (let [s (trace-value-str x)]
    (if (> (count s) trace-max-inline-chars)
      (str (subs s 0 trace-max-inline-chars)
           "… [truncated "
           (- (count s) trace-max-inline-chars)
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

(defn- print-full-trace-edn-frame!
  [event payload]
  (stdout! (trace-value-str {:event event :payload payload})))

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

          (cond (= cp 9) (let [spaces (- 4 (mod col 4))]
                           (.append sb (apply str (repeat spaces \space)))
                           (recur (+ i step) (+ col spaces)))
                (= cp 10) (do (.append sb \newline) (recur (+ i step) 0))
                (< cp 32) (do (.append sb \space) (recur (+ i step) (inc col)))
                :else (let [piece (String. (Character/toChars cp))]
                        (.append sb piece)
                        (recur (+ i step) (+ col (codepoint-width cp))))))))))

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
          (max 40 (- (terminal-width) 4))

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

;; ---------------------------------------------------------------------------
;; Append-only pretty trace printer.
;;
;; Strictly append-only (no cursor-erase redraw): dedups iteration headers per
;; iteration, and streams reasoning as DELTAS (`:delta` is computed in
;; `loop.clj`'s `streaming-fn` as the new tail since the previous chunk) so
;; each reasoning character is emitted exactly once across the whole run.
;; Output is identical in a TTY, a pipe, or a pty wrapper, and non-TTY
;; consumers (CI logs, `vis ... | tee`) get the full stream.
;; ---------------------------------------------------------------------------

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
  [s width]
  (let [s (str s)]
    (cond (str/blank? s) [""]
          (<= (count s) width) [s]
          :else (let [tokens (str/split s #"\s+")]
                  (loop [tokens tokens
                         line ""
                         lines []]

                    (if-let [tok (first tokens)]
                      (cond
                        ;; token longer than the column -> hard-split it
                        (> (count tok) width) (let [head (subs tok 0 width)
                                                    tail (subs tok width)
                                                    lines' (cond-> lines
                                                             (seq line)
                                                             (conj line))]

                                                (recur (cons tail (rest tokens)) head lines'))
                        ;; fits on the current line
                        (or (str/blank? line) (<= (+ (count line) 1 (count tok)) width))
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
         (when (and n (pos? n)) n))
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
       (catch Throwable _ nil)))

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
  [cols]
  (+ 2 (reduce + (map :width cols)) (* 3 (max 0 (dec (count cols))))))

(defn- expand-table-cols
  "Grow table columns to `target-width`. Columns marked `:grow? true`
   share extra width; otherwise the final column grows. This keeps all
   CLI tables full-width while preserving fixed ID/count/date columns."
  [cols target-width]
  (let [cols
        (vec cols)

        extra
        (max 0 (- (long target-width) (table-width cols)))]

    (if (zero? extra)
      cols
      (let [grow-idxs
            (let [marked (keep-indexed (fn [idx col]
                                         (when (:grow? col) idx))
                                       cols)]
              (if (seq marked) (vec marked) [(dec (count cols))]))

            n
            (count grow-idxs)

            base
            (quot extra n)

            remainder
            (rem extra n)

            additions
            (into {}
                  (map-indexed (fn [i idx]
                                 [idx (+ base (if (< i remainder) 1 0))]))
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
   `vis ext list` breaks the rows into per-`:ext/kind`
   sub-tables. `width` is the total visible width of the surrounding
   table so the rule under the label spans the same column run."
  [label width]
  (let [label-str
        (str " " label " ")

        rule-len
        (max 4 (- width (count label-str) 2))]

    (stdout! "")
    (stdout! (str "── " label " " (apply str (repeat rule-len \─))))))

;;; ── Root one-shot run - handler + bespoke arg parser ─────────────────────

(defn- parse-run-args
  "Parse root one-shot run arguments into {:prompt str :json? bool ...}.

   Bespoke instead of `commandline.base/parse-args` because everything
   that ISN'T a known flag is glued together as the prompt body."
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

        (case arg
          "--json"
          (recur more (assoc opts :json? true) prompt-parts)

          "--edn"
          (recur more (assoc opts :edn? true) prompt-parts)

          "--code"
          (recur more (assoc opts :code? true) prompt-parts)

          "--raw"
          (recur more (assoc opts :raw? true) prompt-parts)

          "--toggles"
          (recur (next more) (assoc opts :toggles (first more)) prompt-parts)

          ("--full-trace-stream" "--trace")
          (recur more (assoc opts :full-trace-stream? true) prompt-parts)

          ("--full-trace-edn-stream" "--trace-stream")
          (recur more (assoc opts :full-trace-edn-stream? true) prompt-parts)

          ("--full-trace-json-stream" "--full-trace-json-stream-raw")
          (recur more (assoc opts :full-trace-json-stream? true) prompt-parts)

          ("--help" "-h")
          (assoc opts
            :help? true
            :prompt "")

          "--debug"
          (recur more (assoc opts :debug? true) prompt-parts)

          "--provider"
          (recur (next more) (assoc opts :provider (first more)) prompt-parts)

          "--model"
          (recur (next more) (assoc opts :model (first more)) prompt-parts)

          "--reasoning-effort"
          (recur (next more) (assoc opts :reasoning-effort (first more)) prompt-parts)

          "--name"
          (recur (next more) (assoc opts :agent-name (first more)) prompt-parts)

          "--db"
          (recur (next more) (assoc opts :db (first more)) prompt-parts)

          "--session-id"
          (recur (next more)
                 (assoc opts
                   :session-id (first more)
                   :persist? true)
                 prompt-parts)

          "--persist"
          (recur more (assoc opts :persist? true) prompt-parts)

          (recur more opts (conj prompt-parts arg)))))))

(defn- print-run-usage!
  []
  (stdout! "Usage: vis [FLAGS] \"prompt\"")
  (stdout! "")
  (stdout! "Flags:")
  (stdout! "  --json            Print result as a single JSON envelope.")
  (stdout! "  --edn             Print result as EDN.")
  (stdout! "  --code            Print only [:code] block contents from the")
  (stdout! "                    answer IR. Concatenated in source order;")
  (stdout! "                    no fences, no language tags. Pipes cleanly")
  (stdout! "                    into editors / interpreters. Errors when")
  (stdout! "                    the answer contains no [:code] blocks.")
  (stdout! "  --raw             Render the answer as raw text (no markdown")
  (stdout! "                    bold/italics/heading bars). This is also the")
  (stdout! "                    auto-default when stdout is not a TTY (piped")
  (stdout! "                    or redirected), so `vis ... > out.txt`")
  (stdout! "                    produces clean text without ANSI noise.")
  (stdout! "  --toggles LIST    Comma-separated NAME=VALUE pairs setting any")
  (stdout! "                    registered toggle for this run only. Bare names")
  (stdout! "                    work when unambiguous; namespace on collision, e.g.")
  (stdout! "                    --toggles shell/enabled=true,reasoning-level=deep")
  (stdout! "  --full-trace-stream")
  (stdout! "                    Stream a pretty terminal trace while the run is")
  (stdout! "                    happening, then print the answer.")
  (stdout! "  --full-trace-edn-stream")
  (stdout! "                    Stream raw EDN trace frames (:trace-chunk, :result).")
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
  (stdout! "                       `:cli` session. Default is ephemeral:")
  (stdout! "                       no resume, no session row on disk.")
  (stdout! "")
  (stdout! "Examples:")
  (stdout!
    "  vis --provider zai-coding-plan --model glm-5.2 --reasoning-effort high --json \"Task\"")
  (stdout! "  vis \"Throwaway one-shot probe\"")
  (stdout! "  vis --json --model gpt-4o \"Explain auth flow\"")
  (stdout! "  vis --toggles shell/enabled=true \"Run the test suite and fix failures\"")
  (stdout! "  vis --toggles shell/enabled=true,reasoning-level=deep \"Refactor\"")
  (stdout! "  vis --persist --provider anthropic --model claude-sonnet-4-20250514 \"Keep this\""))

(defn- parse-toggle-overrides
  "Parse a `--toggles` value like
   \"shell/enabled=true,reasoning-level=deep\" into a map of
   {toggle-id value}. NAME may be the bare toggle name
   (e.g. `reasoning-level`) when it is unambiguous across the registry, or
   the fully namespaced id (e.g. `shell/enabled`, leading `:`
   optional) - extensions register toggles under their own namespace,
   so the namespaced form disambiguates collisions. VALUE is validated
   against the registered `:type` -- booleans accept true/false (plus
   on/off, yes/no, 1/0), enums must name one of the registered
   `:choices`. Throws `:vis/user-error` ex-info on any bad pair so the
   CLI error path renders it as a user mistake, not a crash."
  [s]
  (reduce
    (fn [acc pair]
      (let [[k v]
            (str/split pair #"=" 2)

            raw
            (keyword (str/replace (or k "") #"^:" ""))

            id
            (if (namespace raw)
              raw
              (let [hits (filterv #(= (name raw) (name (:id %))) (toggles/registered-toggles))]
                (cond (= 1 (count hits)) (:id (first hits))
                      (seq hits) (throw (ex-info (str "Ambiguous toggle name: " k
                                                      " - use one of " (str/join ", "
                                                                                 (map :id hits)))
                                                 {:type :vis.cli/ambiguous-toggle
                                                  :vis/user-error true
                                                  :name raw
                                                  :candidates (mapv :id hits)}))
                      :else raw)))

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
               (let [value (keyword (str/replace v #"^:" ""))]
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

                 (throw
                   (ex-info
                     (str "Boolean toggle " k " needs true/false, got: " v)
                     {:type :vis.cli/invalid-toggle :vis/user-error true :id id :value v})))))))
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

(defn- answer->ir-safe
  "Coerce a turn's `:answer` into a render-ready IR/AST. The canonical path is
   `render/answer->ir` ({:answer md} / needs-input); but a turn can also surface a
   force-finalized answer, an already-built `[:ir …]` error-fallback AST, or a
   bare string. Those are non-canonical, so answer->ir would throw and crash the
   CLI render. Try the canonical lift; on any other shape, coerce instead of
   dying — a bare string becomes `{:answer string}`, an existing AST vector is
   used as-is, anything else is stringified."
  [answer]
  (try (render/answer->ir answer)
       (catch Exception _
         (cond (nil? answer) [:ir {}]
               (string? answer) (render/answer->ir {:answer answer})
               (vector? answer) answer
               :else (render/answer->ir {:answer (pr-str answer)})))))

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
  (let [{:keys [prompt json? edn? code? raw? full-trace-stream? full-trace-edn-stream?
                full-trace-json-stream? help? agent-name db toggles]
         :as opts}
        (parse-run-args residual)]
    (when (or help? (str/blank? prompt)) (print-run-usage!) (System/exit 0))
    ;; Auto-promote to raw when stdout is NOT a TTY (piped/redirected).
    ;; Otherwise `vis ... > out.txt` leaves bold/italic ANSI markers in
    ;; the file. Structured output flags (--json/--edn/--code) win, and an
    ;; explicit --raw stays raw. The trace-stream flags own their own
    ;; output path and are unaffected.
    (let [structured-output?
          (or json? edn? code? full-trace-stream? full-trace-edn-stream? full-trace-json-stream?)
          effective-raw? (or raw? (and (not structured-output?) (not (trace-terminal?))))
          agent-def (agent {:name (or agent-name "cli")})
          trace-on-chunk (cond full-trace-json-stream? #(print-full-trace-json-frame! :trace-chunk
                                                                                      %)
                               full-trace-edn-stream? #(print-full-trace-edn-frame! :trace-chunk %)
                               full-trace-stream? (make-pretty-trace-printer))
          run-opts (cond-> (dissoc opts
                             :prompt
                             :json?
                             :edn?
                             :code?
                             :raw?
                             :full-trace-stream?
                             :full-trace-edn-stream?
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
                                    [:session-id :answer :trace :iteration-count :duration-ms
                                     :tokens :cost :confidence :status :error :type :eval])]

      (cond
        full-trace-json-stream? (print-full-trace-json-frame! :result trace-result)
        full-trace-edn-stream? (print-full-trace-edn-frame! :result trace-result)
        full-trace-stream?
        (do (tel/log! {:level :info :id ::cli-trace :data trace-result} "CLI trace result")
            (stdout! (str "\n"
                          (trace-dim "└────────────────────────────────────────────────────────")))
            (stdout! (str "\n"
                          (trace-title "◆" "final result")
                          (pretty-block "summary" (trace-final-summary-prose result))))
            (stdout! (str "\n" (trace-title "◆" "answer") "\n"))
            (stdout! (render/render (answer->ir-safe (:answer result))
                                    (if (trace-terminal?) :markdown :plain)))
            (when (:error result)
              (when-let [ex (:exception result)]
                (stdout! "\nStack trace:")
                (.printStackTrace ^Throwable ex ^java.io.PrintStream config/original-stdout))))
        json? (stdout! (result->json result))
        edn? (stdout! (result->edn result))
        code?
        (let [blocks (render/extract-code (answer->ir-safe (:answer result)))]
          (cond
            (:error result) (stdout! (error/format-error (:error result)))
            (empty? blocks)
            (do
              (stdout!
                "Error: --code expects answer to contain at least one [:code] block; got prose only. Run without --code for rendered output.")
              (shutdown-agents)
              (System/exit 1))
            :else (stdout! (str/join "\n\n" blocks))))
        (:error result) (stdout! (error/format-error (:error result)))
        :else (do (stdout! (render/render (answer->ir-safe (:answer result))
                                          (if effective-raw? :plain :markdown)))
                  (when (and (:duration-ms result) (not effective-raw?))
                    (stdout! (str "\n[" (fmt/format-meta-line result) "]")))))
      (shutdown-agents)
      (when (pos? exit-code) (System/exit exit-code)))))

;;; ── `vis sessions` ─────────────────────────────────────────────────

(def ^:private known-channels #{"tui" "telegram" "cli" "api"})
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
   id (soul-id) stays the same so `vis channels tui --session-id
   <ID>` keeps working and now resumes from the fork."
  [cid-input title]
  (let [d
        (lp/db-info)

        resolved
        (resolve-session-by-prefix d cid-input)]

    (cond (nil? resolved) (do (stdout! (str "Session not found: " cid-input))
                              (stdout! "")
                              (stdout! "List existing sessions with:")
                              (stdout! "  vis sessions")
                              (shutdown-agents)
                              (System/exit 1))
          :else (let [;; Fork = new session_state = new workspace pin (1:1).
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
                        (stdout! (str "  Resume with: vis channels tui --session-id " resolved))
                        (stdout! ""))
                    (do (stdout! (str "Failed to fork session "
                                      resolved
                                      "; no existing state to fork from."))
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
        (stdout! "  Resume with: vis channels tui --session-id <ID>  (full or short)")
        (stdout! "  Pick latest: vis channels tui --continue")
        (stdout! "  Browse:      vis channels tui --resume")
        (stdout! "  Show:        vis sessions show <ID>")
        (stdout! "  Fork:        vis sessions fork <ID> [--title TITLE]")
        (stdout! "  Export:      vis sessions export <ID> --md"))))
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
          (stdout! "  vis sessions list")
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
    ;; Workspace: trunk vs draft, and any extra filesystem roots (auto-cloned in
    ;; a draft). This is the CLI/JSON surface for "what drafts + filesystem roots
    ;; does this session have".
    (when-let [ws (when-let [sid (persistance/db-latest-session-state-id d (:id session))]
                    (workspace/for-session d sid))]
      (stdout! (str "  Workspace:    "
                    (if (workspace/draft? ws) "draft (isolated workspace)" "trunk (live)")))
      (stdout! (str "  Root:         " (:root ws)))
      (let [roots (workspace/filesystem-roots ws)]
        (when (seq roots)
          (stdout! (str "  Filesystem dirs (" (count roots) "):"))
          (doseq [{:keys [trunk clone fork-ms]} roots]
            (stdout! (str "    "
                          trunk
                          (when (and fork-ms (not= clone trunk))
                            " (isolated draft copy — lands on /draft apply)")))))))
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
    (stdout! (str "  Resume:  vis channels tui --session-id " (:id session)))
    (stdout! (str "  Export:  vis sessions export " (subs (str (:id session)) 0 8) " --md"))
    (stdout! "")
    (shutdown-agents)))

(defn- export-html-str
  "Standalone HTML for a session — the SAME chat view /ui renders (bubbles +
   inline op-cards + inlined scripts), via the web channel's canonical
   `export-session-html`. Requires the web extension on the classpath."
  [_db sid]
  (str ((requiring-resolve 'com.blockether.vis.ext.channel-web.core/export-session-html) sid)))

(defn- cinema-export-fn
  "Resolve the headless session-cinema exporter from the channel-tui extension,
   or nil when that jar is not on the classpath. Deferred so `--md`/`--html`
   never pay the Lanterna load cost."
  []
  (requiring-resolve 'com.blockether.vis.ext.channel-tui.cinema/export!))

(defn- resolve-out-path
  "Resolve a user-supplied output path against the invocation directory.
   `bin/vis` runs the dev JVM from the repo root (so `clojure -M:vis` finds
   deps.edn) but passes the real invocation cwd as `-Duser.dir`. Java resolves
   relative `File` paths against the OS cwd, so a bare `out.html` would silently
   land in the repo root while the printed path (from `user.dir`) said
   otherwise. Anchor relatives to `user.dir` (same convention as
   `vis ext scaffold`); absolute paths pass through untouched."
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
                                 (long (/ (:video-ms res) 1000)))))
              (do
                (stdout!
                  "Cinema export (--mp4) needs the channel-tui extension, which is not installed.")
                (shutdown-agents)
                (System/exit 2))))
          :else (write-stdout! (render/session->markdown d (:id session))))
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
    (try (workspace/discard-session-clones! d (:id session)) (catch Throwable _ nil))
    (lp/delete! (:id session))
    (stdout! (str "Deleted session " (:id session)))
    (shutdown-agents)))

(defn- cli-draft-session!
  "Start a DRAFT for a session using an available isolation backend
   and pin the session to the draft, so subsequent turns (+ any `/fs add`
   filesystem roots) run isolated until `/draft apply` or `/draft abandon`."
  [parsed _residual]
  (config/init-cli!)
  (let [d
        (lp/db-info)

        session
        (session-or-exit! d (get parsed "session-id"))

        label
        (some-> (get parsed "label")
                str
                str/trim
                not-empty)

        state-id
        (persistance/db-latest-session-state-id d (:id session))]

    (cond
      (nil? state-id)
      (do (stdout! "Session has no state to draft from.") (shutdown-agents) (System/exit 1))
      (not (workspace/isolated-workspaces-supported? (or (:root (workspace/for-session d state-id))
                                                         (workspace/trunk-root))))
      (do
        (stdout!
          "Drafts need a workspace backend with isolation, rollback, merge-back, and retained revisions.")
        (shutdown-agents)
        (System/exit 1))
      :else (let [current (workspace/for-session d state-id)]
              (if (workspace/draft? current)
                (do (stdout! (str "Session is already in a draft: " (:root current)))
                    (stdout! "  Apply or abandon it first (TUI: /draft apply | /draft abandon).")
                    (shutdown-agents)
                    (System/exit 1))
                (let [draft (workspace/create! d
                                               (cond-> {:session-state-id state-id :from current}
                                                 label
                                                 (assoc :label label)))]
                  (stdout! (str "\n  Started draft for session " (:id session)))
                  (stdout! (str "    Clone: " (:root draft)))
                  (stdout! (str "    Trunk: " (:repo-root draft) "  (where /draft apply lands)"))
                  (stdout! "    Add filesystem dirs: /fs add <path>  (auto-cloned into the draft)")
                  (stdout! "    Land: /draft apply   ·   Discard: /draft abandon")
                  (stdout! "")
                  (shutdown-agents)))))))

(defn- cli-fork-session-command!
  [parsed _residual]
  (config/init-cli!)
  (cli-fork-session! (get parsed "session-id") (get parsed "title")))

(def ^:private search-field-labels
  {"answer_text" "answer"
   "thinking_text" "thinking"
   "comments_text" "comments"
   "user_request" "prompt"
   "expression" "expression"})

(defn- cli-sessions-search!
  "`vis sessions search <query>` handler. Runs an FTS5 search
   across user prompts, assistant answers, model thinking, per-block
   comments, and persisted expression source. Hits print one per
   line:

     <session-id-prefix>  <field>  <snippet>

   Snippets carry `[match]` markers around hit terms so the user can
   see context. `--limit N` caps the result count (default 25)."
  [parsed _residual]
  (config/init-cli!)
  (let [query
        (or (get parsed "query") "")

        limit
        (or (some-> (get parsed "limit")
                    str/trim
                    Long/parseLong)
            25)]

    (cond (str/blank? query) (do (stdout! "vis sessions search <query> [--limit N]")
                                 (stdout! "")
                                 (stdout! "Searches session answers, thinking, comments,")
                                 (stdout! "prompts, and persisted expression source.")
                                 (shutdown-agents)
                                 (System/exit 1))
          :else
          (let [d
                (lp/db-info)

                hits
                (or (persistance/db-search d query {:limit limit}) [])]

            (cond (empty? hits) (do (stdout! (str "No matches for: " query)) (shutdown-agents))
                  :else (do (stdout! (str (count hits)
                                          " match" (when (not= 1 (count hits)) "es")
                                          " for: " query))
                            (stdout! "")
                            (doseq [hit hits]
                              (let [label (get search-field-labels (:field hit) (:field hit))
                                    id-pref (let [s (str (:owner-id hit))]
                                              (subs s 0 (min 8 (count s))))
                                    snippet (str/replace (or (:snippet hit) "") #"\s+" " ")]

                                (stdout! (str id-pref "  " (format "%-8s" label) "  " snippet))))
                            (shutdown-agents)))))))

(defn- cli-sessions!
  "`vis sessions` default handler. Bare `vis sessions` lists all
   sessions; every other operation is a canonical subcommand."
  [_parsed residual]
  (config/init-cli!)
  (if (seq residual)
    (do (stdout! (str "Unknown sessions command: " (first residual)))
        (stdout! "")
        (stdout! "Run: vis sessions --help")
        (shutdown-agents)
        (System/exit 2))
    (cli-list-sessions! nil)))

;;; ── `vis providers` ─────────────────────────────────────────────────────

(def ^:private providers-table-cols
  [{:key :id :label "ID" :width 18 :align :left} {:key :label :label "Label" :width 28 :align :left}
   {:key :auth :label "Auth" :width 6 :align :left}
   {:key :rpm :label "Catalog RPM" :width 11 :align :right}
   {:key :tpm :label "Catalog TPM" :width 12 :align :right}
   {:key :base-url :label "Base URL" :width 36 :align :left}])

(defn- gateway-provider-status-safe
  [provider-id]
  (try (gateway-client/provider-status provider-id)
       (catch Throwable e {:authenticated? false :error (or (ex-message e) (str e))})))

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
  [provider-id]
  (or (some-> (registry/provider-by-id provider-id)
              :provider/label)
      (some-> (config/provider-template provider-id)
              :label)
      (str/capitalize (name provider-id))))

(defn- status-entry-label
  [k]
  (-> (name k)
      (str/replace #"-" " ")
      (str/capitalize)))

(defn- format-status-value
  [v]
  (cond (keyword? v) (name v)
        :else (str v)))

(defn- format-limit-window
  [{:keys [kind unit size resets-at-ms]}]
  (when kind
    (str (name kind)
         (when unit (str " " (or size 1) "/" (name unit)))
         (when resets-at-ms
           (str ", resets " (fmt/format-date (java.util.Date. (long resets-at-ms))))))))

(defn- format-limit-row
  [{:keys [label scope kind unlimited? used limit remaining note window]}]
  (let [quota
        (cond unlimited? "unlimited"
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
        (or (configured-provider-status provider) {:authenticated? false})

        provider-id
        (:provider/id provider)

        base-url
        (configured-provider-base-url provider-id)

        rows
        (->> status
             (remove (fn [[k _]]
                       (= k :authenticated?)))
             (sort-by (comp str key)))]

    (stdout! (str "\n  " (:provider/label provider) " Provider Status"))
    (stdout! "  ─────────────────────────────────")
    (when base-url (stdout! (str "  Base URL:       " base-url)))
    (stdout! (str "  Authenticated:  " (if (:authenticated? status) "yes" "no")))
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
              :auth (if (:authenticated? status) "yes" "no")
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
      (let [w (+ 2 (reduce max 0 (map #(count (name (:provider/id %))) all)))]
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

    (cond (nil? provider-id) (do (stdout! "Usage: vis providers auth <provider>")
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
        (boolean (some #(= provider-id (:id %)) (:providers (config/load-config-raw))))]

    (cond (nil? provider-id) (do (stdout! "Usage: vis providers logout <provider>")
                                 (stdout! "")
                                 (print-registered-providers!))
          (nil? provider) (do (stdout! (str "Unknown provider: " provider-name))
                              (stdout! "")
                              (print-registered-providers!))
          (and (nil? (:provider/logout-fn provider)) (not configured?))
          (stdout! (str "Provider " (:provider/label provider) " does not persist credentials."))
          :else (do (when-let [logout-fn (:provider/logout-fn provider)]
                      (logout-fn))
                    (config/remove-config-provider! provider-id :cli-provider-logout)
                    (stdout! (str "  Logged out of "
                                  (:provider/label provider)
                                  ". Tokens and config cleared.")))))
  (shutdown-agents))

;;; ── `vis doctor` ────────────────────────────────────────────────────────

(defn- cli-doctor!
  [_parsed _residual]
  (config/init-cli!)
  (let [env
        {:db-info (config/resolve-db-spec)}

        msgs
        (doctor/run-checks env)]

    (stdout! (doctor/format-output msgs))
    (System/exit (int (doctor/exit-code msgs)))))

;;; ── `vis ext` ───────────────────────────────────────────────────────────

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
      (throw (ex-info "Usage: vis ext scaffold <name> [--dir DIR] [--namespace NS] [--force]"
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
          "It is auto-loaded when you run vis from this project (or from ~/.vis/vis-extensions)."))))
  (shutdown-agents))

(defn- source-checkout-root
  []
  (or (some-> (System/getenv "VIS_SOURCE_ROOT")
              not-empty
              io/file)
      (when-let [^java.io.File f (some-> (io/resource "com/blockether/vis/internal/main.clj")
                                         str
                                         (str/replace-first #"^file:" "")
                                         java.net.URLDecoder/decode
                                         io/file)]
        (loop [^java.io.File d f
               n 6]

          (if (or (nil? d) (zero? n)) d (recur (.getParentFile d) (dec n)))))
      (io/file ".")))

(defn- git-checkout? [dir] (.exists (io/file dir ".git")))

(defn- process-result
  [cmd dir]
  (let [p
        (process/process {:cmd cmd :dir (.getPath (io/file dir)) :out :string :err :string})

        proc
        (:proc p)

        _
        (.waitFor ^Process proc)

        result
        @p]

    {:exit (:exit result) :out (:out result) :err (:err result)}))

(defn- cli-update!
  [_parsed _residual]
  (config/init-cli!)
  (let [^java.io.File root (source-checkout-root)]
    (when-not (git-checkout? root)
      (throw (ex-info "Vis update requires a git source checkout"
                      {:type :update/not-git-checkout :path (.getPath root)})))
    (stdout! (str "Updating Vis source at " (.getPath root)))
    (let [fetch (process-result ["git" "fetch" "--tags" "origin"] root)
          pull (when (zero? (:exit fetch)) (process-result ["git" "pull" "--ff-only"] root))]

      (when-not (zero? (:exit fetch))
        (throw (ex-info
                 "git fetch failed"
                 {:type :update/git-fetch-failed :stderr (:err fetch) :stdout (:out fetch)})))
      (when-not (zero? (:exit pull))
        (throw (ex-info "git pull --ff-only failed"
                        {:type :update/git-pull-failed :stderr (:err pull) :stdout (:out pull)})))
      (stdout! (str/trim (or (:out pull) "")))
      (stdout! "Vis update complete.")))
  (shutdown-agents))

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

(defn- cli-gateway-status!
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  (let [{:keys [status pid host port db clients running_turns require_token] :as m}
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/status))]
    (if (= "running" status)
      (stdout! (str "gateway running pid=" pid
                    " url=http://" host
                    ":" port
                    " db=" db
                    " clients=" clients
                    " running-turns=" running_turns
                    " auth=" (if require_token "token" "loopback-disabled")))
      (stdout! (str "gateway stopped"
                    (when-let [db (:db m)]
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
    (cond (not running?)
          (throw (ex-info (str "no gateway is running for this DB. Start one reachable first:\n"
                               "  vis gateway start --host 0.0.0.0 --require-token --pair")
                          {:vis/user-error true}))
          loopback? (throw (ex-info (str
                                      "the running gateway is bound to " host
                                      " (loopback) — a phone cannot reach it.\n"
                                      "Restart it on a reachable host:\n"
                                      "  vis gateway stop\n"
                                      "  vis gateway start --host 0.0.0.0 --require-token --pair")
                                    {:vis/user-error true}))
          :else ((requiring-resolve 'com.blockether.vis.internal.gateway.pairing/print-pairing!)
                  {:host host :port port :token token :require-token? (boolean token)}))))

(defn- cli-gateway-stop!
  [parsed _residual]
  (config/init-cli!)
  (when-let [db (get parsed "db")]
    (System/setProperty "vis.db.path" db))
  (let [{:keys [stopping status] :as m}
        ((requiring-resolve 'com.blockether.vis.internal.gateway.client/stop-daemon!))]
    (stdout! (cond stopping "gateway stopping"
                   (= "stopped" status) "gateway stopped"
                   :else (str "gateway stop requested: " (pr-str m))))))

;;; ── `vis python` — standalone GraalPy interpreter ────────────────────────
;;
;; Expose JUST the embedded GraalPy sandbox -- every foundation shim
;; (requests/pandas/numpy/yaml/sqlite3/...), the POSIX-compat preamble, and
;; the auto-imports -- with NO agent tool bindings. Handy for reproducing
;; sandbox behaviour and exercising shims straight from the shell. Behaves
;; identically under the JVM and the native image: both drive the same
;; `env/*` machinery.

(defn- python-cli-context
  "Build a fresh standalone GraalPy sandbox for `vis python`: all shims
   installed, filesystem rooted at the current working directory, network
   enabled unless `network?` is false. No tool bindings -- just the
   interpreter with its shims."
  [{:keys [network?]}]
  (let [cwd
        (.getCanonicalPath (io/file "."))

        {:keys [python-context]}
        (env/create-python-context {}
                                   (fn []
                                     [cwd])
                                   {:enabled? (boolean network?)})]

    ;; Bind an empty standing `ctx` dict so the async runtime has it available.
    (env/bind-ctx! python-context {})
    python-context))

(defn- run-python-source!
  "Evaluate one Python source block in `ctx`, rendering its outcome to the
   real terminal. Returns the process exit code (0 ok, 1 on a raised error).
   Matches the agent sandbox semantics: `print(...)` is what surfaces, a bare
   trailing expression does NOT echo."
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
  (stdout! (str "vis python -- embedded GraalPy sandbox (all shims, no tools). "
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

(defn- cli-python!
  "`vis python` -- run code in the embedded GraalPy sandbox (all shims, no tool
   bindings). Modes: `-c CODE` (run a string), `FILE.py` (run a file), `-` or
   piped stdin (run stdin), or an interactive REPL on a bare TTY. `--no-network`
   disables sandbox network."
  [_parsed residual]
  (config/init-cli!)
  (let [raw
        (vec residual)

        no-network?
        (boolean (some #{"--no-network"} raw))

        args
        (vec (remove #{"--no-network"} raw))

        ctx
        (python-cli-context {:network? (not no-network?)})

        exit
        (cond (= "-c" (first args)) (if-let [code (second args)]
                                      (run-python-source! ctx code)
                                      (do (stdout! "vis python -c requires a CODE argument.") 2))
              (= "-" (first args)) (run-python-source! ctx (slurp System/in))
              (seq args) (let [f (io/file (first args))]
                           (if (.isFile f)
                             (run-python-source! ctx (slurp f))
                             (do (stdout! (str "vis python: no such file: " (first args))) 2)))
              (some? (System/console)) (do (python-repl! ctx) 0)
              :else (run-python-source! ctx (slurp System/in)))]

    (shutdown-agents)
    (System/exit exit)))

;;; ── Top-level binary built-ins (registry/register-cmd! direct) ─────────
;;
;; `providers`, `sessions`, `doctor`, `update`, and `ext` are the
;; binary's own parent commands. They live at the top of the command
;; tree -- `vis providers ...`, NOT `vis ext providers ...` -- so they
;; bypass `:ext/cli` (the `vis ext` subcommand slot). Direct
;; `register-cmd!` is the right plumbing here; vis-runtime is the host,
;; not an extension contributing to `vis ext`.

(doseq [spec [{:cmd/name "providers"
               :cmd/doc "Inspect, authenticate, and introspect LLM providers."
               :cmd/usage "vis providers <list|status|limits|auth|logout> [...]"
               :cmd/subcommands #(registry/registered-under ["providers"])}
              {:cmd/name "sessions"
               :cmd/doc "List, show, fork, delete, search, or export persisted sessions."
               :cmd/usage "vis sessions <list|show|fork|delete|search|export> [...]"
               :cmd/examples ["vis sessions" "vis sessions list" "vis sessions show 3a7b2c1d"
                              "vis sessions fork 3a7b2c1d --title \"Branch A\""
                              "vis sessions export 3a7b2c1d --md"
                              "vis sessions export 3a7b2c1d --html out.html"
                              "vis sessions search \"foo bar\""]
               :cmd/subcommands #(registry/registered-under ["sessions"])
               :cmd/run-fn cli-sessions!}
              {:cmd/name "doctor"
               :cmd/doc "Run cross-extension diagnostics."
               :cmd/usage "vis doctor"
               :cmd/run-fn cli-doctor!}
              {:cmd/name "ext"
               :cmd/doc "Inspect, scaffold, or run an extension-contributed CLI command."
               :cmd/usage "vis ext <list|scaffold|...> [args...]"
               :cmd/subcommands #(registry/registered-under ["ext"])}
              {:cmd/name "update"
               :cmd/doc "Update the source checkout used by this Vis installation."
               :cmd/usage "vis update"
               :cmd/run-fn cli-update!}
              {:cmd/name "gateway"
               :cmd/doc "Start, inspect, or stop the long-lived gateway daemon."
               :cmd/usage "vis gateway <start|status|stop|pair> [--db PATH]"
               :cmd/subcommands #(registry/registered-under ["gateway"])}
              {:cmd/name "python"
               :cmd/doc "Run code in the embedded GraalPy sandbox (all shims, no tool bindings)."
               :cmd/usage "vis python [-c CODE | FILE.py | -] [--no-network]"
               :cmd/examples ["vis python -c \"import requests; print(requests.__version__)\""
                              "vis python script.py" "echo 'print(1 + 1)' | vis python"
                              "vis python   # interactive REPL"]
               :cmd/owns-tty? true
               :cmd/run-fn cli-python!}]]
  (registry/register-cmd! spec))

;;; ── `vis gateway` subcommands ──────────────────────────────────────────

(doseq
  [spec
   [{:cmd/name "start"
     :cmd/parent ["gateway"]
     :cmd/doc "Start the long-lived gateway daemon (HTTP + SSE runtime) in the foreground."
     :cmd/usage "vis gateway start [--port 7890] [--host 127.0.0.1] [--token-file PATH] [--pair]"
     :cmd/args
     [{:name "port" :kind :flag :type :string :doc "TCP port to listen on (default 7890)."}
      {:name "host"
       :kind :flag
       :type :string
       :doc "Bind host (default 127.0.0.1; non-loopback logs a warning)."}
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
       "Print a VIS companion pairing QR (URL + bearer token). Use with --host 0.0.0.0 or a Tailscale/LAN host for phone access."}]
     :cmd/examples ["vis gateway start" "vis gateway start --port 8080"
                    "vis gateway start --host 0.0.0.0 --require-token --pair"]
     :cmd/run-fn cli-gateway-start!}
    {:cmd/name "status"
     :cmd/parent ["gateway"]
     :cmd/doc "Show the gateway daemon registered for the current DB without starting it."
     :cmd/usage "vis gateway status [--db PATH]"
     :cmd/args [{:name "db"
                 :kind :flag
                 :type :string
                 :doc "SQLite DB path whose gateway registry should be inspected."}]
     :cmd/run-fn cli-gateway-status!}
    {:cmd/name "stop"
     :cmd/parent ["gateway"]
     :cmd/doc "Stop the gateway daemon registered for the current DB."
     :cmd/usage "vis gateway stop [--db PATH]"
     :cmd/args
     [{:name "db" :kind :flag :type :string :doc "SQLite DB path whose gateway should be stopped."}]
     :cmd/run-fn cli-gateway-stop!}
    {:cmd/name "pair"
     :cmd/parent ["gateway"]
     :cmd/doc "Print a companion pairing QR for the gateway already running for this DB."
     :cmd/usage "vis gateway pair [--db PATH]"
     :cmd/examples ["vis gateway pair"]
     :cmd/args [{:name "db"
                 :kind :flag
                 :type :string
                 :doc "SQLite DB path whose running gateway should be paired."}]
     :cmd/run-fn cli-gateway-pair!}]]
  (registry/register-cmd! spec))

;;; ── `vis providers` subcommands ─────────────────────────────────────────

(doseq [spec [{:cmd/name "list"
               :cmd/parent ["providers"]
               :cmd/doc "List registered providers with auth state, static limits, and base URLs."
               :cmd/usage "vis providers list"
               :cmd/run-fn cli-providers-list!}
              {:cmd/name "status"
               :cmd/parent ["providers"]
               :cmd/doc "Show provider authentication status together with static/dynamic limits."
               :cmd/usage "vis providers status [provider]"
               :cmd/examples ["vis providers status" "vis providers status github-copilot-business"
                              "vis providers status openai-codex"]
               :cmd/run-fn cli-providers-status!}
              {:cmd/name "limits"
               :cmd/parent ["providers"]
               :cmd/doc "Show provider rate-limit metadata and any dynamic quota report."
               :cmd/usage "vis providers limits [provider]"
               :cmd/examples ["vis providers limits" "vis providers limits openai-codex"
                              "vis providers limits ollama"]
               :cmd/run-fn cli-providers-limits!}
              {:cmd/name "auth"
               :cmd/parent ["providers"]
               :cmd/doc "Run a provider's interactive authentication flow."
               :cmd/usage "vis providers auth <provider>"
               :cmd/args
               [{:name "provider"
                 :kind :positional
                 :type :string
                 :doc
                 "Registered provider id (for example: github-copilot-business or openai-codex)."}]
               :cmd/examples ["vis providers auth github-copilot-business"
                              "vis providers auth github-copilot-individual"
                              "vis providers auth openai-codex"]
               :cmd/run-fn cli-providers-auth!}
              {:cmd/name "logout"
               :cmd/parent ["providers"]
               :cmd/doc "Clear saved credentials for a provider."
               :cmd/usage "vis providers logout <provider>"
               :cmd/args
               [{:name "provider" :kind :positional :type :string :doc "Registered provider id."}]
               :cmd/examples ["vis providers logout github-copilot-business"
                              "vis providers logout github-copilot-individual"
                              "vis providers logout openai-codex"]
               :cmd/run-fn cli-providers-logout!}]]
  (registry/register-cmd! spec))

;;; ── `vis sessions` subcommands ──────────────────────────────────────────

(doseq
  [spec
   [{:cmd/name "list"
     :cmd/parent ["sessions"]
     :cmd/doc "List persisted sessions."
     :cmd/usage "vis sessions list [all|tui|telegram|cli]"
     :cmd/args [{:name "channel"
                 :kind :positional
                 :type :string
                 :doc "Optional channel filter (all|tui|telegram|cli; default all)."}]
     :cmd/examples ["vis sessions list" "vis sessions list tui"]
     :cmd/run-fn cli-sessions-list!}
    {:cmd/name "show"
     :cmd/parent ["sessions"]
     :cmd/doc "Show one session's metadata, turns, and fork states."
     :cmd/usage "vis sessions show <SESSION-ID>"
     :cmd/args [{:name "session-id"
                 :kind :positional
                 :type :string
                 :required true
                 :doc "Session id (full UUID or unambiguous prefix)."}]
     :cmd/examples ["vis sessions show 3a7b2c1d"]
     :cmd/run-fn cli-show-session!}
    {:cmd/name "fork"
     :cmd/parent ["sessions"]
     :cmd/doc "Fork a session from its latest state."
     :cmd/usage "vis sessions fork <SESSION-ID> [--title TITLE]"
     :cmd/args [{:name "session-id"
                 :kind :positional
                 :type :string
                 :required true
                 :doc "Session id (full UUID or unambiguous prefix)."}
                {:name "title" :kind :flag :type :string :doc "Title to set on the new fork."}]
     :cmd/examples ["vis sessions fork 3a7b2c1d" "vis sessions fork 3a7b2c1d --title \"Branch A\""]
     :cmd/run-fn cli-fork-session-command!}
    {:cmd/name "draft"
     :cmd/parent ["sessions"]
     :cmd/doc "Start an isolated workspace draft for a session (apply/abandon via the TUI)."
     :cmd/usage "vis sessions draft <SESSION-ID> [--label NAME]"
     :cmd/args
     [{:name "session-id"
       :kind :positional
       :type :string
       :required true
       :doc "Session id (full UUID or unambiguous prefix)."}
      {:name "label" :kind :flag :type :string :doc "Draft folder label (default: auto)."}]
     :cmd/examples ["vis sessions draft 3a7b2c1d" "vis sessions draft 3a7b2c1d --label feature-x"]
     :cmd/run-fn cli-draft-session!}
    {:cmd/name "delete"
     :cmd/parent ["sessions"]
     :cmd/doc "Delete a session tree from persistent storage."
     :cmd/usage "vis sessions delete <SESSION-ID>"
     :cmd/args [{:name "session-id"
                 :kind :positional
                 :type :string
                 :required true
                 :doc "Session id (full UUID or unambiguous prefix)."}]
     :cmd/examples ["vis sessions delete 3a7b2c1d"]
     :cmd/run-fn cli-delete-session!}
    {:cmd/name "export"
     :cmd/parent ["sessions"]
     :cmd/doc
     "Export a session: Markdown on stdout, HTML to a file, or a headless MP4 screencast of the TUI transcript."
     :cmd/usage "vis sessions export <SESSION-ID> [--md | --html PATH | --mp4 PATH]"
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
     :cmd/examples ["vis sessions export 3a7b2c1d --md"
                    "vis sessions export 3a7b2c1d --html out.html"
                    "vis sessions export 3a7b2c1d --mp4 session.mp4"]
     :cmd/run-fn cli-export-session!}
    {:cmd/name "search"
     :cmd/parent ["sessions"]
     :cmd/doc "Full-text search across answers, thinking, comments, prompts, and expressions."
     :cmd/usage "vis sessions search <query> [--limit N]"
     :cmd/args [{:name "query"
                 :kind :positional
                 :type :string
                 :doc "FTS5 query (`foo bar` for AND, `foo OR bar`, `foo*` for prefix)."}
                {:name "limit" :kind :flag :type :string :doc "Max hits to print (default 25)."}]
     :cmd/examples ["vis sessions search \"znajduje nodes\"" "vis sessions search \"refactor*\""
                    "vis sessions search \"foo OR bar\" --limit 100"]
     :cmd/run-fn cli-sessions-search!}]]
  (registry/register-cmd! spec))

;;; ── `vis ext` subcommands (host-owned canonical) ────────────────────────
;;
;; `list` and `scaffold` are NOT extension contributions -- they are
;; the CANONICAL host commands the vis binary ships with. Extensions add
;; to the `vis ext` parent through `:ext/cli`; the host marks its own
;; entries with `:cmd/internal? true` so help and listing layers can
;; tell host-owned canonical commands apart from extension-contributed
;; ones at a glance.

(doseq [spec [{:cmd/name "list"
               :cmd/parent ["ext"]
               :cmd/internal? true
               :cmd/doc "List every registered extension with metadata."
               :cmd/usage "vis ext list"
               :cmd/run-fn cli-extensions!}
              {:cmd/name "scaffold"
               :cmd/parent ["ext"]
               :cmd/internal? true
               :cmd/doc "Create a user extension project scaffold."
               :cmd/usage "vis ext scaffold <name> [--dir DIR] [--namespace NS] [--force]"
               :cmd/examples ["vis ext scaffold my-tools"
                              "vis ext scaffold my-tools --dir ~/.vis/vis-extensions/my-tools"]
               :cmd/run-fn cli-extensions-scaffold!}]]
  (registry/register-cmd! spec))

;; =============================================================================
;; Dispatcher entry point (-main)
;; =============================================================================

;; =============================================================================
;; Logging routing
;;
;; Telemere ships with a `:default/console` handler that prints EVERY
;; signal to stdout. That fills the terminal with registration noise
;; before the user ever sees the help text -- painful UX for a CLI.
;;
;; Default behavior:
;;   - stdout stays clean
;;   - every signal is appended to `~/.vis/vis.log`
;;
;; Pass `--debug` / `--verbose` / `-v` (or set `VIS_DEBUG=1`) to KEEP
;; the console handler in addition to the file handler.
;; =============================================================================

(def ^:private debug-flags #{"--debug" "--verbose" "-v"})

(defn- debug-mode? [args] (or (some debug-flags args) (= "1" (System/getenv "VIS_DEBUG"))))

(defn- log-file-path
  []
  (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis/logs"))]
    (when-not (.exists log-dir) (.mkdirs log-dir))
    (str log-dir "/vis.log")))

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
    (try (tel/add-handler! :file (tel/handler:file {:path path}) {:min-level :info})
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

;; =============================================================================
;; Extension discovery
;;
;; ONE call. The unified loader lives in the extension facade.
;; =============================================================================

(defn- print-extension-load-failures!
  "Print every classpath extension namespace whose `(require)` blew
   up during the most recent scan to stderr, along with the
   user-actionable hint. Pre-fix the failure was a buried
   `~/.vis/vis.log` ERROR line and the user had no surface clue
   that an entire alias namespace was unbound - the LLM in the
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
        (println "⚠  vis: " (count failures) "extension namespace(s) failed to load.")
        (println "   The associated alias namespace will be UNBOUND in the sandbox.")
        (println "   The agent will see `Unable to resolve symbol` for every call into it.")
        (println)
        (doseq [{:keys [extension-id extension-ns reason path]} failures]
          (println (str "   • extension '" extension-id "' (" extension-ns ")"))
          (println (str "     " reason))
          (when path (println (str "     manifest: " path))))
        (println)))))

(defn discover-all!
  "Run the unified extension discovery scan. Idempotent through
   Clojure's `require` cache. Returns nil.

   Prints a stderr banner enumerating every extension namespace
   whose `(require)` failed during discovery. The same warnings are
   also fed into the per-turn `(:project ctx) :warnings` slice, so
   both the user (at the terminal) and the LLM (reading `ctx`) see the failure
   immediately instead of bouncing off `Unable to resolve symbol`
   for an entire session."
  []
  (extension/discover-extensions!)
  (python-extensions/load-python-extensions!)
  (print-extension-load-failures!)
  nil)

;; =============================================================================
;; Root command
;;
;; The dispatcher's root has NO hard-coded subcommands. Every entry
;; comes from the global commandline registry. Built-ins (providers,
;; sessions, doctor, ...) are registered by vis-runtime; the `vis channel` and
;; `vis ext` parents are registered by the channel and extension
;; facades. Add a third-party jar with its own `register-cmd!`
;; calls and its commands appear here without any code change.
;; =============================================================================

(def ^:private DEFAULT_DOC
  (str
    "Vis - persistent sandboxed Recursive Language Model powered by an embedded Python REPL.\n" "\n"
    "USAGE\n" "  vis [FLAGS] \"prompt\"          Run one-shot agent work.\n"
    "  vis [FLAGS]                    Show this help.\n"
    "  vis <command> [args...]        Run a command.\n"
    "  vis <command> --help           Show command help.\n" "\n"
    "EXAMPLES\n" "  vis \"fix failing tests\"\n"
    "  vis --json \"summarize this repo\"\n"
    "  vis --provider zai-coding-plan --model glm-5.2 --reasoning-effort high --json \"task\"\n"
    "  vis --full-trace-json-stream --db :memory \"debug startup\"\n" "  vis providers status\n"
    "  vis sessions search sqlite\n" "\n"
    "ONE-SHOT FLAGS\n" "  --json                       Print result as JSON.\n"
    "  --edn                        Print result as EDN.\n"
    "  --code                       Print only final answer code blocks.\n"
    "  --raw                        Print plain text, no markdown styling.\n"
    "  --toggles NAME=VAL[,..]      Set registered toggles for this run only (e.g. shell/enabled=true).\n"
    "  --full-trace-stream          Stream pretty human trace.\n"
    "  --full-trace-edn-stream      Stream raw EDN trace frames.\n"
    "  --full-trace-json-stream     Stream raw JSON trace frames.\n"
    "  --provider PROVIDER          Override provider.\n"
    "  --model MODEL                Override model or use provider/model.\n"
    "  --reasoning-effort E         Exact provider-native effort: high or max.\n"
    "  --name NAME                  Agent name for this run.\n"
    "  --db PATH|:memory            SQLite DB path or in-memory DB.\n"
    "  --session-id ID              Continue an existing persisted session.\n"
    "  --persist                    Persist as a :cli session.\n"
    "  --debug                      Enable verbose debug logging.\n"
    "  --help, -h                   Show help."))

(defn root-command
  "Build the root `vis` command tree. Subcommands are pulled fresh on
   every call so newly registered extensions show up immediately."
  []
  (registry/command
    {:cmd/name "vis" :cmd/doc DEFAULT_DOC :cmd/subcommands #(registry/registered-under [])}))

;; =============================================================================
;; Pre-redirect stderr for TTY-owning channels
;;
;; Some leaves (TUI, ncurses) take over the controlling terminal and
;; need stderr re-routed to a log file BEFORE any further class loading
;; triggers JVM warnings. The check is data-driven via
;; `:cmd/owns-tty?`. Channels mark themselves through the channel
;; bridge; nothing here is channel-aware.
;; =============================================================================

(defn- pre-redirect-stderr!
  [args]
  (when-let [{:keys [command]} (commandline/find-leaf (root-command) (cons "vis" args))]
    (when (:cmd/owns-tty? command)
      (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis/logs"))]
        (when-not (.exists log-dir) (.mkdirs log-dir))
        (System/setErr (java.io.PrintStream.
                         (java.io.FileOutputStream. ^String (str log-dir "/vis.log") true)
                         true))))))

;; =============================================================================
;; Main
;; =============================================================================

(defn- root-help-request?
  "True when args ask only for the root help screen. This path can skip
   extension discovery because the root tree lists built-in parent commands
   only; extension-owned commands are mounted below `ext` after
   discovery when that subtree is requested."
  [args]
  (or (empty? args) (contains? #{["help"] ["--help"] ["-h"]} (vec args))))

(defn- version-request?
  "True when args ask only for the version. Like help, this short-circuits
   BEFORE extension discovery / agent boot — `vis --version` must be instant and
   must NOT create the GraalPy sandbox or contact a provider."
  [args]
  (contains? #{["--version"] ["-V"] ["version"]} (vec args)))

(defn- vis-version
  "Vis version string: the `vis/VERSION` resource written at build time (git
   describe), else \"dev\"."
  []
  (or (some-> (io/resource "vis/VERSION")
              slurp
              str/trim
              not-empty)
      "dev"))

(def ^:private first-party-channel-bootstrap-nses
  {"tui" 'com.blockether.vis.ext.channel-tui.core
   "telegram" 'com.blockether.vis.ext.channel-telegram.bot})

(defn- help-request?
  "True when args request help at any command depth. We can usually render
   help without initializing runtime resources; if a command is not registered
   yet, the caller falls back to full extension discovery."
  [args]
  (boolean (or (root-help-request? args) (some #{"--help" "-h"} args))))

(defn- channel-help-request?
  "True for `vis channels <first-party-channel> --help`. These requests need
   only the selected channel descriptor, not every extension namespace."
  [args]
  (let [[parent channel & more] (vec args)]
    (and (= "channels" parent)
         (contains? first-party-channel-bootstrap-nses channel)
         (boolean (some #{"--help" "-h"} more)))))

(defn- channel-parent-help-request?
  "True for `vis channels --help`. Rendering the parent has to load
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
  "True for any `vis ext ...` help invocation. The `vis ext` subtree is
   populated by `:ext/cli` mounts that only land after
   `extension/discover-extensions!` has run, so help rendering for this
   subtree MUST trigger full extension discovery before the renderer
   reads `(registered-under [\"ext\"])`."
  [args]
  (= "ext" (first (vec args))))

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
          (cons "vis" args)

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
  "True when bare `vis ...` should run the one-shot CLI agent.
   Unknown commands that ask for help stay errors, so typo diagnostics
   remain honest (`vis sessions --help` must not become a prompt)."
  [root args]
  (and (unknown-command? root args) (not-any? #{"--help" "-h"} args)))

(defn- exit-with-user-error!
  [^Throwable t]
  (stdout! (str "vis: " (or (ex-message t) "error")))
  (shutdown-agents)
  (System/exit 2))

(defn- root-cause
  ^Throwable [^Throwable t]
  (loop [c t]
    (if-let [n (.getCause c)]
      (recur n)
      c)))

(defn- exit-with-fatal-error!
  [^Throwable t]
  (let [rc
        (root-cause t)

        same?
        (identical? rc t)]

    (stdout! (str "vis: fatal error - " (or (ex-message t) (.getName (class t)))))
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
    (stdout! "See ~/.vis/logs/vis.log for details."))
  (shutdown-agents)
  (System/exit 1))

(defn- exit-no-provider!
  "Calm, guided message when no AI provider is configured — never a stacktrace.
   Points at the interactive welcome (the curated, zero-friction path)."
  []
  (stdout! "")
  (stdout! "  vis needs an AI provider to get started.")
  (stdout! "")
  (stdout! "  ▸ Run  vis  with no arguments to open the welcome screen and")
  (stdout! "    connect one (Sign in with GitHub / OpenAI / Anthropic, paste an")
  (stdout! "    API key, or run a local model).")
  (stdout! "  ▸ Or hand-write ~/.vis/config.edn.")
  (stdout! "")
  (shutdown-agents)
  (System/exit 2))

(defn- truthy-value? [v] (contains? #{"1" "true" "yes" "on"} (str/lower-case (str v))))

(defn- measure-arg? [arg] (= "--measure" arg))

(def ^:private launcher-selector-args
  ;; `bin/vis` normally consumes these before invoking Clojure, but keep
  ;; the JVM entry point tolerant too (e.g. `clojure -M:vis channels --jvm --help`).
  #{"--source" "--jvm" "--jar" "--native" "--jfr"})

(defn- global-arg? [arg] (or (measure-arg? arg) (contains? launcher-selector-args arg)))

(defn- strip-global-args [args] (vec (remove global-arg? args)))

(def ^:private session-shortcut-flags
  ;; Top-level `vis --resume` / `vis --continue` (pi-parity) are
  ;; ergonomic aliases for the `channels tui` session flags.
  {"--resume" "--resume" "-r" "--resume" "--continue" "--continue" "-c" "--continue"})

(defn- rewrite-session-shortcuts
  "Rewrite a leading `--resume`/`-r`/`--continue`/`-c` into
   `channels tui <flag>` so the session shortcuts work at the root.
   Only fires when the shortcut is the FIRST token, so flag-shaped
   one-shot prompts (e.g. `vis --json \"...\"`) are untouched."
  [args]
  (if-let [canon (get session-shortcut-flags (first args))]
    (into ["channels" "tui" canon] (rest args))
    (vec args)))

(defn- startup-measure?
  [args]
  (or (some measure-arg? args)
      (truthy-value? (System/getenv "VIS_MEASURE"))
      (truthy-value? (System/getProperty "vis.measure"))))

(defn- elapsed-ms [started-ns] (/ (double (- (System/nanoTime) started-ns)) 1000000.0))

(defn- format-ms [ms] (String/format java.util.Locale/ROOT "%.1f ms" (object-array [(double ms)])))

(defn- startup-measure-line!
  [label & kvs]
  (binding [*out* *err*]
    (println
      (str "[vis measure] jvm:" label (when (seq kvs) (str " " (str/join " " (map str kvs))))))))

(defn- timed-startup!
  [measure? label f]
  (if measure?
    (let [started (System/nanoTime)]
      (try (f) (finally (startup-measure-line! label (format-ms (elapsed-ms started))))))
    (f)))

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
   CLI ergonomics (`vis fix this`, `vis --json summarize`)."
  [& raw-args]
  (let [main-started
        (System/nanoTime)

        measure?
        (startup-measure? raw-args)

        args
        (rewrite-session-shortcuts (strip-global-args raw-args))]

    (when measure? (System/setProperty "vis.measure" "1"))
    ;; Opt-in JFR profiling (VIS_JFR set by `bin/vis --jfr`). Role-tagged so a
    ;; spawned gateway daemon (`vis gateway start`) records to its OWN file,
    ;; separate from this client's — see internal.jfr.
    (try ((requiring-resolve 'com.blockether.vis.internal.jfr/maybe-start!)
           (if (= "gateway" (first args)) "gateway" "client"))
         (catch Throwable _ nil))
    (try
      ;; Quiet stdout BEFORE any extension load triggers Telemere registration
      ;; spam - the user only sees logs when they pass --debug / --verbose / -v
      ;; (or set VIS_DEBUG=1).
      (timed-startup! measure? "configure-logging" #(configure-logging! args))
      (cond (version-request? args) (println (str "vis " (vis-version)))
            (root-help-request? args) (println (commandline/render-tree (root-command)))
            (fast-help-dispatched? measure? args) nil
            :else (do (timed-startup! measure? "discover-all+extensions" #(discover-all!))
                      (when measure? (summarize-startup-registries!))
                      (timed-startup! measure? "pre-redirect-stderr" #(pre-redirect-stderr! args))
                      (let [root
                            (root-command)

                            full-args
                            (cons "vis" args)

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
                              ;; `vis foo --bogus && echo ok` printed `ok`. Map `:error` to
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
                                  ;; `discover-all!` spins up the GraalPy sandbox + extension
                                  ;; executors, some of which leave NON-daemon threads alive; a
                                  ;; bare `nil` return let `-main` finish while those threads
                                  ;; kept the JVM (and the native isolate) running, so a
                                  ;; one-shot command like `vis sessions export` printed its
                                  ;; output and then HUNG the terminal forever. Draining agents
                                  ;; and calling `System/exit 0` guarantees termination.
                                  (do (shutdown-agents) (System/exit 0))))))))
      (catch Throwable t
        (cond (= :vis/no-provider (:type (ex-data t))) (exit-no-provider!)
              (:vis/user-error (ex-data t)) (exit-with-user-error! t)
              :else (exit-with-fatal-error! t)))
      (finally (when measure?
                 (startup-measure-line! "main total" (format-ms (elapsed-ms main-started))))))))
