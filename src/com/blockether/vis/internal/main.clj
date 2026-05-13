(ns com.blockether.vis.internal.main
  "vis CLI binary - :db Telemere handler, one-shot agent helper,
   built-in CLI commands, and the `-main` dispatcher entry point.

   Everything in this file is binary-only. The library surface
   (iteration loop, turn engine, environment lifecycle, conversation
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
     vis run                - one-shot agent turn (CLI agent helper)
     vis providers          - provider inspection, auth, and limits
     vis conversations      - list persisted conversations
     vis extensions list    - list registered extensions
     vis channels <name>    - auto-mounted via the channel registry

   `vis extensions doctor` is registered by vis-foundation through
   `:ext/cli`, so extension-owned commands stay under `vis extensions`.
   Every extension can plug its `:ext/doctor-check-fn` into the aggregator."

  (:refer-clojure :exclude [agent run!])
  (:require
   [babashka.process :as process]
   [charred.api :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.commandline :as commandline]
   [com.blockether.vis.internal.config :as config]
   [com.blockether.vis.internal.crac-bootstrap :as crac-bootstrap]
   [com.blockether.vis.internal.doctor :as doctor]
   [com.blockether.vis.internal.error :as error]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.format :as fmt]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.manifest :as manifest]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.progress :as progress]
   [com.blockether.vis.internal.provider-limits :as provider-limits]
   [com.blockether.vis.internal.registry :as registry]
   [com.blockether.vis.internal.render :as render]
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
  (let [ctx   (or (:ctx signal) {})
        level (or (:level signal) :info)
        event (or (some-> (:id signal) str)
                (some-> (:ns signal) str)
                "unknown")
        data  (try
                (json/write-json-str
                  (cond-> {}
                    (:msg_ signal)  (assoc :msg (force (:msg_ signal)))
                    (:data signal)  (assoc :data (:data signal))
                    (:ns signal)    (assoc :ns (str (:ns signal)))
                    (:error signal) (assoc :error (str (:error signal)))))
                (catch Throwable _ nil))]
    (cond-> {:level level
             :event event
             :data  data}
      (:conversation-soul-id ctx) (assoc :conversation-soul-id (:conversation-soul-id ctx))
      (:conversation-turn-id ctx) (assoc :conversation-turn-soul-id (:conversation-turn-id ctx))
      (:iteration-id ctx)         (assoc :iteration-id (:iteration-id ctx)))))

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

     (tel/with-ctx+ {:db-info db-info :conversation-soul-id conversation-id}
       (tel/log! :info \"something happened\"))"
  ([] (handler:db nil))
  ([_opts]
   (fn handler
     ([signal]
      (when-let [db-info (get-in signal [:ctx :db-info])]
        (try
          (persistance/db-log! db-info (signal->entry signal))
          (catch Throwable _ nil))))
     ([] nil))))

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
    {:async     {:mode :dropping :buffer-size 2048 :n-threads 1}
     :min-level :info}))

;; =============================================================================
;; Extension CLI dispatcher
;; =============================================================================

;;; ── Extension introspection ─────────────────────────────────────────────

(def ^:private ^String ext-ns-prefix "com.blockether.vis.ext.")

(defn- short-ext-ns
  "Render an extension namespace symbol with the `v/` prefix instead of
   the canonical `com.blockether.vis.ext.` package, so the table column
   stays narrow:

     com.blockether.vis.ext.foundation.core      -> v/foundation.core
     com.blockether.vis.ext.provider-github-copilot -> v/provider-github-copilot

   Anything that doesn't start with the canonical prefix is returned
   unchanged."
  [ns-sym]
  (let [s (str ns-sym)]
    (if (str/starts-with? s ext-ns-prefix)
      (str "v/" (subs s (count ext-ns-prefix)))
      s)))

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
    "providers"   (->> (:ext/providers e) (keep :provider/label) (str/join ", "))
    "channels"    (->> (:ext/channels e)  (keep :channel/cmd)    (str/join ", "))
    "persistance" (->> (:ext/persistance e) (keep (comp name :persistance/id)) (str/join ", "))
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
          {:namespace (short-ext-ns (:ext/namespace e))
           :doc       (:ext/doc e)
           :kind      (or (:ext/kind e) "uncategorized")
           :group     (per-kind-group e)
           :author    (or (:ext/author e) "-")
           :owner     (or (:ext/owner e) "-")
           :license   (or (:ext/license e) "-")
           :version   (or (:ext/version e) "-")})
    (extension/registered-extensions)))

(defn find-extension-cmd
  "Find an extension CLI command by name. Returns {:ext ext :cmd cmd-map} or nil."
  [cmd-name]
  (some (fn [e]
          (some (fn [cmd]
                  (when (= (:cmd cmd) cmd-name)
                    {:ext e :cmd cmd}))
            (:ext/cli e)))
    (extension/registered-extensions)))

(defn all-extension-cmds
  "Return a flat vec of {:cmd :doc :ext-ns :args} for every registered extension CLI command."
  []
  (into []
    (mapcat (fn [e]
              (map (fn [c] (assoc c :ext-ns (str (:ext/namespace e))))
                (or (:ext/cli e) []))))
    (extension/registered-extensions)))

;;; ── Arg parsing & validation ───────────────────────────────────────────

(defn- flag-arg? [s] (str/starts-with? (str s) "--"))

(defn- coerce-arg [value type]
  (case (or type :string)
    :string  value
    :int     (if-let [n (parse-long value)] n
               (throw (ex-info (str "Expected integer, got: " value) {:value value})))
    :boolean (contains? #{"true" "1" "yes"} (str/lower-case (str value)))
    :file    value
    value))

(defn parse-ext-args
  "Parse CLI args against an arg spec. Returns a map of {arg-name value}.

   :kind :positional args are matched in declaration order.
   :kind :flag args are matched by --name. Boolean flags need no value."
  [arg-specs raw-args]
  (let [positional (vec (filter #(= :positional (:kind %)) arg-specs))
        flags      (into {} (map (fn [a] [(:name a) a]))
                     (filter #(= :flag (:kind %)) arg-specs))]
    (loop [args     (seq raw-args)
           pos-idx  0
           result   {}]
      (if-not args
        result
        (let [arg  (first args)
              more (next args)]
          (if (flag-arg? arg)
            ;; Flag
            (if-let [spec (get flags arg)]
              (if (= :boolean (:type spec))
                (recur more pos-idx (assoc result (:name spec) true))
                (recur (next more) pos-idx
                  (assoc result (:name spec) (coerce-arg (first more) (:type spec)))))
              (recur more pos-idx result))
            ;; Positional
            (if (< pos-idx (count positional))
              (let [spec (nth positional pos-idx)]
                (recur more (inc pos-idx)
                  (assoc result (:name spec) (coerce-arg arg (:type spec)))))
              (recur more pos-idx result))))))))

(defn validate-ext-args
  "Validate parsed args against spec. Returns nil on success, error string on failure."
  [arg-specs parsed]
  (let [required (filter :required arg-specs)
        missing  (remove #(contains? parsed (:name %)) required)]
    (when (seq missing)
      (str "Missing required argument(s): "
        (str/join ", " (map :name missing))))))

;;; ── Help rendering ─────────────────────────────────────────────────────

(defn- pad [s w]
  (let [s (str s)] (if (>= (count s) w) s (str s (apply str (repeat (- w (count s)) \space))))))

(defn format-cmd-help
  "Build help text for a single extension CLI command."
  [{:keys [cmd doc args ext-ns]}]
  (let [positional (filter #(= :positional (:kind %)) (or args []))
        flags      (filter #(= :flag (:kind %)) (or args []))
        usage-pos  (str/join " "
                     (map (fn [{:keys [name required]}]
                            (if required (str "<" name ">") (str "[" name "]")))
                       positional))
        usage-flags (when (seq flags) "[flags]")
        usage      (str/join " " (remove nil? [usage-pos usage-flags]))
        fmt-arg    (fn [{:keys [name type required doc]}]
                     (str "    " (pad name 20)
                       (pad (or (some-> type clojure.core/name) "string") 10)
                       (if required "required  " "optional  ")
                       (or doc "")))]
    (str "  vis ext " cmd (when (seq usage) (str " " usage))
      "\n\n  " (or doc "")
      (when ext-ns (str "\n  Extension: " ext-ns))
      (when (seq positional)
        (str "\n\n  Positional arguments:\n"
          (str/join "\n" (map fmt-arg positional))))
      (when (seq flags)
        (str "\n\n  Flags:\n"
          (str/join "\n" (map fmt-arg flags)))))))

(defn extension-help []
  (let [cmds (all-extension-cmds)]
    (if (empty? cmds)
      "No extension commands available. Run 'vis extensions' to see registered extensions."
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
          help?    (some #{"--help" "-h"} raw-args)]
      (if help?
        {:help (format-cmd-help (assoc cmd :ext-ns (:ext-ns cmd)))}
        (let [parsed (parse-ext-args arg-specs raw-args)
              err    (validate-ext-args arg-specs parsed)]
          (if err
            {:error (str err "\n\n" (format-cmd-help cmd))}
            {:ok ((:fn cmd) parsed)}))))
    {:error (str "Unknown command: " cmd-name "\n\n" (extension-help))}))

;; =============================================================================
;; Agent helper (one-shot `vis run`)
;; =============================================================================

;;; ── Agent Definition ─────────────────────────────────────────────────────

(defn agent
  "Create an agent definition (data map).

   Options:
   - :name        - Agent name (string, default \"default\")
   - :description - What the agent does
   - :constants   - Map of {symbol value} constants for SCI sandbox
   - :model       - Override default model selection

   The iteration loop runs until the model emits `:answer` or the
   user cancels.

   Example:
     (agent {:name \"code-reviewer\"
             :description \"Reviews Clojure code for quality\"
             :model \"gpt-4o\"})"
  [{:keys [name] :as opts}]
  (let [agent-name (or name "default")]
    (merge {:name      agent-name
            :constants {}}
      opts)))

;;; ── Execution ────────────────────────────────────────────────────────────

(defn- split-provider-model
  "Return `[provider-id model-name]` for `provider/model`; nil for bare model names."
  [model]
  (when-let [model* (some-> model str str/trim not-empty)]
    (when-let [idx (str/index-of model* "/")]
      (let [provider-name (subs model* 0 idx)
            model-name    (subs model* (inc idx))]
        (when (and (not (str/blank? provider-name)) (not (str/blank? model-name)))
          [(keyword provider-name) model-name])))))

(defn- select-model
  [provider model-name]
  (let [model-name* (str model-name)
        existing    (some #(when (= (str/lower-case model-name*)
                                   (some-> (config/model-name %) str/lower-case))
                             %)
                      (:models provider))
        selected    (if (map? existing)
                      (assoc existing :name model-name*)
                      {:name model-name*})]
    (assoc provider :models (vec (cons selected
                                   (remove #(= (str/lower-case model-name*)
                                              (some-> (config/model-name %) str/lower-case))
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
  (let [providers (vec (:providers config))
        provider  (or (some #(when (= provider-id (:id %)) %) providers)
                    (provider-from-template provider-id))]
    (if-not provider
      (throw (ex-info (str "Unknown provider: " (name provider-id))
               {:type :vis.cli/unknown-provider
                :provider provider-id}))
      (assoc config :providers
        (vec (cons provider (remove #(= provider-id (:id %)) providers)))))))

(defn- config-with-model-override
  "Return config with `model` selected first.

   Bare model names select that model on the active provider. Provider-qualified
   names (`provider/model`) move or synthesize that provider as the one-shot
   root provider. This does not persist to `~/.vis/config.edn`."
  [config model]
  (if-let [model* (some-> model str str/trim not-empty)]
    (let [providers (vec (:providers config))]
      (if-let [[provider-id model-name] (split-provider-model model*)]
        (let [provider (or (some #(when (= provider-id (:id %)) %) providers)
                         (provider-from-template provider-id)
                         (throw (ex-info (str "Unknown provider in --model: " (name provider-id))
                                  {:type :vis.cli/unknown-model-provider
                                   :provider provider-id
                                   :model model*})))
              selected (select-model provider model-name)]
          (assoc config :providers (vec (cons selected (remove #(= provider-id (:id %)) providers)))))
        (update config :providers
          (fn [providers]
            (if-let [active (first providers)]
              (vec (cons (select-model active model*) (rest providers)))
              providers)))))
    config))

(defn- router-for-run
  [config use-local-router?]
  (if use-local-router?
    (svar/make-router (mapv config/->svar-provider (:providers config)))
    (lp/get-router)))

(defn run!
  "Execute a one-shot agent turn.

   Runs one turn. Default is ephemeral: in-memory SQLite only, no
   `:cli` conversation written to disk.

   Returns map with:
   - :conversation-id - Conversation ID (UUID string) when persisted;
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
   - :on-chunk    - Streaming callback fn
   - :debug?      - Enable debug logging (default false)
   - :config      - Provider config override (skips ~/.vis/config.edn)
   - :db          - DB target for ephemeral runs (`:memory`, path, or db spec)
   - :persist?    - Write the run to ~/.vis/vis.mdb as a `:cli`
                    conversation. Default false.
   - :no-persist? - Backward-compatible override; when true, forces
                    ephemeral execution even if `:persist?` is true.

   Ephemeral runs use an in-memory SQLite DB (`:db :memory`), run the
   turn, then dispose the env (which vaporizes the DB). Result has
   `:conversation-id nil`. Useful for CI, scripting, sensitive prompts.

   Persistent calls (`:persist? true`) create a fresh conversation in
   the `:cli` channel. Past runs are browsable via
   `(conversations/by-channel :cli)`."
  [agent-def prompt & [{:keys [spec model provider on-chunk
                               debug? config db persist? no-persist?]
                        :as _opts}]]
  (let [mdl       (or model (:model agent-def))
        cfg-base  (config/resolve-config config)
        cfg       (cond-> cfg-base
                    provider (config-with-provider-override (keyword provider))
                    mdl      (config-with-model-override mdl))
        local-router? (boolean (or config mdl provider))
        prompt-s  (if (string? prompt) prompt (pr-str prompt))
        title     (let [t (str/trim prompt-s)]
                    (if (> (count t) 100) (str (subs t 0 97) "...") t))
        tracker   (when on-chunk
                    (progress/make-progress-tracker {:on-update (fn [_timeline chunk] (on-chunk chunk))}))
        on-chunk* (when tracker (:on-chunk tracker))
        q-opts    (cond-> {}
                    spec      (assoc :spec spec)
                    mdl       (assoc :model mdl)
                    on-chunk* (assoc :hooks {:on-chunk on-chunk*})
                    debug?    (assoc :debug? true))
        messages  (if (string? prompt) [(svar/user prompt)] prompt)
        persistent? (and persist? (not no-persist?))]
    (if-not persistent?
      ;; Ephemeral path: build a fresh env on a `:memory` SQLite DB so
      ;; nothing touches `~/.vis/vis.mdb`. Disposing the env tears the
      ;; in-memory DB down with it. Bypasses `lp/create!`/`lp/send!`
      ;; (both go through the shared conversations cache + the on-disk
      ;; SQLite handle) on purpose. We use `:memory` instead of nil
      ;; because the iteration loop requires a non-nil `:db-info` (it
      ;; persists turns + iterations + expression history; nil would
      ;; reject in `prepare-turn-context`).
      (let [env (lp/create-environment (router-for-run cfg local-router?) {:db (or db :memory)})]
        (try
          (let [result (lp/turn! env messages q-opts)]
            (cond-> {:conversation-id nil
                     :answer          (:answer result)
                     :iteration-count (:iteration-count result)
                     :duration-ms     (:duration-ms result)
                     :tokens          (:tokens result)
                     :cost            (:cost result)
                     :trace           (:trace result)}
              (:status result)     (assoc :status (:status result))
              (:confidence result) (assoc :confidence (:confidence result))))
          (catch Exception e
            {:conversation-id nil
             :error           (persistance/db-error->user-message e)
             :type            (str (type e))
             :exception       e})
          (finally
            (try (lp/dispose-environment! env) (catch Exception _ nil)))))
      ;; Persistent path: route through the shared conversation cache
      ;; so the run shows up in `(conversations/by-channel :cli)` and
      ;; survives process restarts.
      (let [_ (when local-router? (lp/rebuild-router! cfg))
            {conversation-id :id} (lp/create! :cli {:title title})]
        (try
          (let [result (lp/send! conversation-id messages q-opts)]
            (cond-> {:conversation-id conversation-id
                     :answer          (:answer result)
                     :iteration-count (:iteration-count result)
                     :duration-ms     (:duration-ms result)
                     :tokens          (:tokens result)
                     :cost            (:cost result)
                     :trace           (:trace result)}
              (:status result)     (assoc :status (:status result))
              (:confidence result) (assoc :confidence (:confidence result))))
          (catch Exception e
            {:conversation-id conversation-id
             :error           (persistance/db-error->user-message e)
             :type            (str (type e))
             :exception       e}))))))

;;; ── Output Formatting ───────────────────────────────────────────────────

(defn- json-key
  "Return a stable string key for CLI JSON output. Runtime trace maps can
   contain non-JSON map keys. Charred correctly rejects those, so normalize
   keys before writing the public `vis run --json` envelope."
  [k]
  (cond
    (string? k)  k
    (keyword? k) (name k)
    (symbol? k)  (str k)
    :else        (pr-str k)))

(defn- json-safe
  [x]
  (cond
    (map? x)
    (reduce-kv (fn [m k v]
                 (assoc m (json-key k) (json-safe v)))
      {} x)

    (instance? java.util.Map$Entry x)
    [(json-safe (.getKey ^java.util.Map$Entry x))
     (json-safe (.getValue ^java.util.Map$Entry x))]

    (vector? x)  (mapv json-safe x)
    (set? x)     (mapv json-safe x)
    (seq? x)     (mapv json-safe x)
    (keyword? x) (name x)
    (symbol? x)  (str x)
    (uuid? x)    (str x)
    (inst? x)    (str x)

    (instance? Throwable x)
    {"type"    (str (type x))
     "message" (ex-message x)
     "data"    (json-safe (ex-data x))}

    :else        x))

(defn result->json [result]
  (json/write-json-str (json-safe result)))

(defn result->edn [result]
  (pr-str result))

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
  (cond
    (instance? Throwable x)
    {:type    (str (type x))
     :message (.getMessage ^Throwable x)}

    (map? x)
    (into {}
      (map (fn [[k v]] [k (trace-safe v)]))
      x)

    (map-entry? x)
    [(trace-safe (.getKey ^java.util.Map$Entry x))
     (trace-safe (.getValue ^java.util.Map$Entry x))]

    (vector? x) (mapv trace-safe x)
    (set? x)    (mapv trace-safe x)
    (seq? x)    (mapv trace-safe x)

    (or (nil? x) (string? x) (number? x) (keyword? x)
      (symbol? x) (boolean? x) (char? x))
    x

    :else
    (str x)))

(defn- trace-safe-pr-str [x]
  (try
    (pr-str (trace-safe x))
    (catch Throwable t
      (str "#<unprintable " (type t) ": " (.getMessage t) ">"))))

(defn- trace-pr-str [x]
  (let [s (trace-safe-pr-str x)]
    (if (> (count s) trace-max-inline-chars)
      (str (subs s 0 trace-max-inline-chars) "… [truncated " (- (count s) trace-max-inline-chars) " chars]")
      s)))

(defn- trace-indent [s]
  (->> (str/split-lines (str s))
    (map #(str "    " %))
    (str/join "\n")))

(defn- trace-error-summary [err]
  (cond
    (map? err)
    (str (or (:message err) (:reason err) (:type err) "error")
      (when-let [phase (:phase err)] (str " [" phase "]"))
      (when-let [hint (:hint err)] (str "\n" (trace-indent (str "hint: " hint))))
      (when-let [trace (:trace err)] (str "\n" (trace-indent trace))))

    (some? err) (trace-pr-str err)
    :else       nil))

(defn- print-full-trace-edn-frame! [event payload]
  (stdout! (trace-safe-pr-str {:event event :payload payload})))

(defn- print-full-trace-json-frame! [event payload]
  (stdout! (json/write-json-str (json-safe (trace-safe {:event event :payload payload})))))

(defn- trace-terminal?
  []
  (boolean (and (System/console)
             (str/blank? (System/getenv "NO_COLOR"))
             (not= "dumb" (System/getenv "TERM")))))

(defn- ansi
  [code s]
  (if (trace-terminal?)
    (str "\u001b[" code "m" s "\u001b[0m")
    (str s)))

(defn- trace-title [icon label]
  (ansi "1;96" (str icon " " label)))

;; Use bright-black, not ANSI dim (2): dim is unreadable on many themes.
(defn- trace-dim [s] (ansi "90" s))
(defn- trace-ok [s] (ansi "32" s))
(defn- trace-warn [s] (ansi "33" s))
(defn- trace-bad [s] (ansi "31" s))
(defn- trace-code [s] (ansi "36" s))

(def ^:private ansi-sgr-re #"\u001B\[[0-9;]*m")

(defn- strip-ansi [s]
  (str/replace (str s) ansi-sgr-re ""))

(defn- codepoint-width ^long [^long cp]
  (let [t (Character/getType (int cp))]
    (cond
      (= cp 9) 4
      (or (= t Character/NON_SPACING_MARK)
        (= t Character/COMBINING_SPACING_MARK)
        (= t Character/ENCLOSING_MARK)) 0
      (or (<= 0x1100 cp 0x115F)
        (<= 0x2E80 cp 0xA4CF)
        (<= 0xAC00 cp 0xD7A3)
        (<= 0xF900 cp 0xFAFF)
        (<= 0xFE10 cp 0xFE19)
        (<= 0xFE30 cp 0xFE6F)
        (<= 0xFF00 cp 0xFF60)
        (<= 0xFFE0 cp 0xFFE6)
        (<= 0x1F300 cp 0x1FAFF)) 2
      (< cp 32) 0
      :else 1)))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} display-cols ^long [s]
  (let [^String s (strip-ansi s)
        n (.length s)]
    (loop [i 0 cols 0]
      (if (>= i n)
        cols
        (let [cp (.codePointAt s i)]
          (recur (+ i (Character/charCount cp))
            (+ cols (codepoint-width cp))))))))

(defn- expand-tabs [s]
  (let [^String s (str s)
        n (.length s)
        sb (StringBuilder.)]
    (loop [i 0 col 0]
      (if (>= i n)
        (.toString sb)
        (let [cp (.codePointAt s i)
              step (Character/charCount cp)]
          (cond
            (= cp 9)
            (let [spaces (- 4 (mod col 4))]
              (.append sb (apply str (repeat spaces \space)))
              (recur (+ i step) (+ col spaces)))

            (= cp 10)
            (do (.append sb \newline)
              (recur (+ i step) 0))

            (< cp 32)
            (do (.append sb \space)
              (recur (+ i step) (inc col)))

            :else
            (let [piece (String. (Character/toChars cp))]
              (.append sb piece)
              (recur (+ i step) (+ col (codepoint-width cp))))))))))

(defn- wrap-plain-line [s max-cols]
  (let [^String s (str s)
        n (.length s)
        max-cols (max 8 (long max-cols))]
    (loop [i 0 col 0 line (StringBuilder.) acc []]
      (if (>= i n)
        (cond-> acc (pos? (.length line)) (conj (.toString line)))
        (let [cp (.codePointAt s i)
              step (Character/charCount cp)
              piece (String. (Character/toChars cp))
              w (codepoint-width cp)]
          (if (and (pos? (.length line)) (> (+ col w) max-cols))
            (recur i 0 (StringBuilder.) (conj acc (.toString line)))
            (do
              (.append line piece)
              (recur (+ i step) (+ col w) line acc))))))))

(defn- pretty-block [label body]
  (when-not (str/blank? (strip-ansi body))
    (let [cols (max 40 (- (terminal-width) 4))
          lines (->> (str/split-lines (expand-tabs body))
                  (mapcat (fn [line]
                            (let [wrapped (wrap-plain-line line cols)]
                              (if (seq wrapped) wrapped [""])))))]
      (str "\n" (trace-dim (str "  ┌─ " label))
        "\n"
        (->> lines
          (map #(str (trace-dim "  │ ") %))
          (str/join "\n"))
        "\n"
        (trace-dim "  └")))))

(defn- ^{:clj-kondo/ignore [:unused-private-var]} print-pretty-trace-chunk! [chunk]
  (let [phase (:phase chunk)
        iter  (:iteration chunk)
        head  (str (trace-dim "\n╭─") " "
                (trace-title "λ" "trace")
                (when iter (str " " (trace-dim (str "iteration " iter))))
                " ")]
    (case phase
      :provider-call
      (stdout! (str head (trace-title "↗" "provider call")
                 (when-let [t (:started-at-ms chunk)] (str " " (trace-dim (str "started=" t))))))

      :provider-fallback
      (stdout! (str head (trace-warn "↷ provider fallback") " "
                 (or (:failed-provider chunk) "?") " → "
                 (or (:new-provider chunk) "?")
                 (when-let [reason (:reason chunk)] (str " " (trace-dim (str "(" reason ")"))))))

      :reasoning
      (when-not (str/blank? (str (:thinking chunk)))
        (stdout! (str head (trace-title "🧠" "reasoning")
                   (pretty-block "thinking" (:thinking chunk)))))

      :response-parse
      (stdout! (if (= :start (:status chunk))
                 (str head (trace-title "⌁" "response parse") " " (trace-dim "started")
                   (when-let [n (:raw-length chunk)] (str " " (trace-dim (str "raw=" n " chars"))))
                   (when-let [n (:block-count chunk)] (str " " (trace-dim (str "blocks=" n)))))
                 (str head (trace-ok "✓ response parsed")
                   (when-let [n (:forms chunk)] (str " forms=" n))
                   (when-let [n (:code-length chunk)] (str " " (trace-dim (str "code=" n " chars"))))
                   (when-let [n (:duration-ms chunk)] (str " " (trace-dim (str n "ms")))))))

      :form-start
      (stdout! (str head (trace-title "▶" (str "form " (inc (long (or (:form-idx chunk) 0)))
                                            (when-let [of (:form-of chunk)] (str "/" of))))
                 " " (trace-dim "started")
                 (pretty-block "code" (trace-code (:code chunk)))))

      :tool-start
      (stdout! (str head (trace-title "⚙" "tool")
                 (pretty-block "event" (trace-pr-str (:tool-event chunk)))))

      :form-result
      (stdout! (str head
                 (if (:error chunk)
                   (trace-bad "✗ form failed")
                   (trace-ok "✓ form finished"))
                 " #" (inc (long (or (:form-idx chunk) 0)))
                 (when-let [of (:form-of chunk)] (str "/" of))
                 (when-let [ms (:execution-time-ms chunk)] (str " " (trace-dim (str ms "ms"))))
                 (when (:repaired? chunk) (str " " (trace-warn "repaired")))
                 (when (:timeout? chunk) (str " " (trace-bad "timeout")))
                 (pretty-block "stdout" (:stdout chunk))
                 (pretty-block "stderr" (:stderr chunk))
                 (if-let [err (trace-error-summary (:error chunk))]
                   (pretty-block "error" (trace-bad err))
                   (pretty-block "result" (trace-pr-str (:result chunk))))))

      :iteration-final
      (stdout! (str head (if (:done? chunk)
                           (trace-ok "✓ turn complete")
                           (trace-title "·" "iteration complete"))
                 (when-let [final (:final chunk)]
                   (pretty-block "final" (trace-pr-str (select-keys final [:status :iteration-count]))))))

      :iteration-error
      (stdout! (str head (trace-bad "✗ iteration error")
                 (pretty-block "error" (or (trace-error-summary (:error chunk)) (trace-pr-str chunk)))))

      (stdout! (str head (trace-title "•" (name (or phase :unknown)))
                 (pretty-block "chunk" (trace-pr-str chunk)))))))

(defn- trace-entry-header [entry]
  (str (trace-dim "╭─") " "
    (trace-title "λ" "trace") " "
    (trace-dim (str "iteration " (:iteration entry)))
    (when-let [activity (:activity entry)]
      (str " " (trace-warn (name activity))))))

(defn- render-trace-form [entry idx]
  (let [code      (get (:code entry) idx)
        comment   (get (:comments entry) idx)
        result    (get (:results entry) idx)
        success?  (get (:successes entry) idx)
        duration  (get (:durations entry) idx)
        started?  (some? (get (:started-at-ms entry) idx))
        kind      (get (:result-kinds entry) idx)
        title     (str (if (some? success?)
                         (if success? (trace-ok "✓") (trace-bad "✗"))
                         (if started? (trace-warn "▶") (trace-dim "·")))
                    " form " (inc idx)
                    (when duration (str " " (trace-dim (str duration "ms"))))
                    (when kind (str " " (trace-dim (name kind)))))]
    (str "\n" title
      (pretty-block "comment" comment)
      (pretty-block "code" code)
      (pretty-block (if success? "result" "error") result))))

(defn- render-pretty-trace-entry [entry]
  (str (trace-entry-header entry)
    (pretty-block "thinking" (:thinking entry))
    (when (seq (:provider-fallbacks entry))
      (pretty-block "provider fallback" (trace-pr-str (:provider-fallbacks entry))))
    (apply str
      (map #(render-trace-form entry %)
        (remove #(true? (get (:silents entry) %))
          (range (count (:code entry))))))
    (when-let [err (:error entry)]
      (pretty-block "iteration error" (trace-error-summary err)))
    (when-let [final (:final entry)]
      (pretty-block (if (:done? entry) "turn complete" "iteration complete")
        (trace-pr-str (select-keys final [:status :iteration-count]))))))

(defn- render-pretty-trace-timeline [timeline]
  (str/join "\n" (map render-pretty-trace-entry timeline)))

(defn- trace-final-summary-prose
  "Human prose for the pretty terminal trace footer. Keep raw maps for the
   EDN/JSON stream modes; the terminal trace should read like a tiny run
   report, not like dumped data."
  [result]
  (let [failed?    (boolean (:error result))
        iters      (fmt/format-iterations (:iteration-count result))
        duration   (fmt/format-duration (:duration-ms result))
        tokens     (fmt/format-tokens (:tokens result))
        cost       (fmt/format-cost (:cost result))
        confidence (some-> (:confidence result) name)
        status     (some-> (:status result) name)
        where      (str/join " in " (remove str/blank? [iters duration]))
        opener     (str (if failed?
                          "The run stopped with an error"
                          "The run completed successfully")
                     (when-not (str/blank? where) (str " after " where))
                     ".")]
    (str/join "\n"
      (remove str/blank?
        [opener
         (when tokens
           (str "It used " tokens "."))
         (when cost
           (str "Estimated cost: " cost "."))
         (when confidence
           (str "Confidence was " confidence "."))
         (when status
           (str "Final status: " status "."))
         (when-let [err (:error result)]
           (str "Error: " err))]))))

(defn- terminal-erase-lines! [n]
  (when (and (trace-terminal?) (pos? n))
    (write-stdout! (apply str (repeat n "\u001b[1A\u001b[2K")))))

(defn- make-pretty-trace-printer []
  (let [printed-lines (atom 0)
        tracker (progress/make-progress-tracker
                  {:on-update
                   (fn [timeline _chunk]
                     (let [frame (render-pretty-trace-timeline timeline)
                           line-count (max 1 (count (str/split-lines frame)))]
                       (if (trace-terminal?)
                         (do
                           (terminal-erase-lines! @printed-lines)
                           (write-stdout! (str frame "\n"))
                           (reset! printed-lines line-count))
                         ;; Non-TTY: don't spam repeated reasoning snapshots.
                         ;; Use --full-trace-edn-stream/--full-trace-json-stream
                         ;; for pipe-friendly raw streaming.
                         nil)))})]
    (:on-chunk tracker)))

(defn- wrap-str
  "Word-wrap `s` into a vector of lines, each <= `width` chars. Splits on
   whitespace; tokens longer than `width` are hard-broken so a single
   long URL or symbol can't blow the column out."
  [s width]
  (let [s (str s)]
    (cond
      (str/blank? s)            [""]
      (<= (count s) width)      [s]
      :else
      (let [tokens (str/split s #"\s+")]
        (loop [tokens tokens
               line   ""
               lines  []]
          (if-let [tok (first tokens)]
            (cond
              ;; token longer than the column -> hard-split it
              (> (count tok) width)
              (let [head (subs tok 0 width)
                    tail (subs tok width)
                    lines' (cond-> lines (seq line) (conj line))]
                (recur (cons tail (rest tokens)) head lines'))

              ;; fits on the current line
              (or (str/blank? line)
                (<= (+ (count line) 1 (count tok)) width))
              (recur (rest tokens)
                (if (str/blank? line) tok (str line " " tok))
                lines)

              ;; doesn't fit -> push current line, start a new one
              :else
              (recur (rest tokens) tok (conj lines line)))
            (cond-> lines (seq line) (conj line))))))))

(def ^:private fallback-terminal-width 120)

(defn- terminal-env [k]
  (System/getenv k))

(defn- parse-positive-long [s]
  (try
    (let [n (some-> s str/trim parse-long)]
      (when (and n (pos? n)) n))
    (catch Throwable _ nil)))

(defn- shell-first-line
  "Run a tiny terminal-size probe and return its first stdout line.
   Kept private and timeout-bounded so table rendering never hangs CLI startup."
  [cmd]
  (try
    (let [p    (process/process {:cmd ["sh" "-c" cmd]
                                 :out :string
                                 :err :out})
          proc (:proc p)]
      (if (.waitFor ^Process proc 250 java.util.concurrent.TimeUnit/MILLISECONDS)
        (some-> @p :out str/split-lines first)
        (do
          (process/destroy-tree p)
          nil)))
    (catch Throwable _ nil)))

(defn- stty-terminal-width []
  (when-let [line (shell-first-line "stty size < /dev/tty")]
    (some-> (re-find #"^\s*\d+\s+(\d+)\s*$" line) second parse-positive-long)))

(defn- tput-terminal-width []
  (parse-positive-long (shell-first-line "tput cols")))

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
  (+ 2
    (reduce + (map :width cols))
    (* 3 (max 0 (dec (count cols))))))

(defn- expand-table-cols
  "Grow table columns to `target-width`. Columns marked `:grow? true`
   share extra width; otherwise the final column grows. This keeps all
   CLI tables full-width while preserving fixed ID/count/date columns."
  [cols target-width]
  (let [cols (vec cols)
        extra (max 0 (- (long target-width) (table-width cols)))]
    (if (zero? extra)
      cols
      (let [grow-idxs (let [marked (keep-indexed (fn [idx col]
                                                   (when (:grow? col) idx))
                                     cols)]
                        (if (seq marked) (vec marked) [(dec (count cols))]))
            n         (count grow-idxs)
            base      (quot extra n)
            remainder (rem extra n)
            additions (into {}
                        (map-indexed (fn [i idx]
                                       [idx (+ base (if (< i remainder) 1 0))]))
                        grow-idxs)]
        (mapv (fn [idx col]
                (update col :width + (get additions idx 0)))
          (range) cols)))))

(defn- print-table!
  "Print a formatted table to stdout!.
   `cols` is `[{:key :k :label \"L\" :width N :align :left|:right}]`.
   Cells are word-wrapped (not truncated) so long descriptions stay
   visible across multiple physical lines. Tables expand to terminal
   width by growing `:grow?` columns (or the final column by default)."
  [cols rows]
  (let [cols       (expand-table-cols cols (terminal-width))
        align-line (fn [s {:keys [width align]}]
                     (if (= align :right)
                       (commandline/pad-left s width)
                       (commandline/pad-right s width)))
        sep        (str "─" (str/join "─┼─"
                              (map #(apply str (repeat (:width %) \─)) cols)) "─")
        header     (str " " (str/join " │ "
                              (map #(commandline/pad-right (:label %) (:width %)) cols)) " ")]
    (stdout! header)
    (stdout! sep)
    (doseq [row rows]
      (let [wrapped   (mapv (fn [c]
                              (wrap-str (get row (:key c)) (:width c)))
                        cols)
            row-lines (apply max 1 (map count wrapped))]
        (dotimes [i row-lines]
          (stdout!
            (str " "
              (str/join " │ "
                (map (fn [lines col]
                       (align-line (or (nth lines i nil) "") col))
                  wrapped cols))
              " ")))))))

(defn- print-section-heading!
  "Render a section heading line for a grouped table - used when
   `vis extensions list` breaks the rows into per-`:ext/kind`
   sub-tables. `width` is the total visible width of the surrounding
   table so the rule under the label spans the same column run."
  [label width]
  (let [label-str (str " " label " ")
        rule-len  (max 4 (- width (count label-str) 2))]
    (stdout! "")
    (stdout! (str "── " label " " (apply str (repeat rule-len \─))))))

;;; ── `vis run` - handler + bespoke arg parser ────────────────────────────

(defn- parse-run-args
  "Parse `vis run` arguments into {:prompt str :json? bool ...}.

   Bespoke instead of `commandline.base/parse-args` because everything
   that ISN'T a known flag is glued together as the prompt body."
  [args]
  (loop [args         (seq args)
         opts         {}
         prompt-parts []]
    (if-not args
      (assoc opts :prompt (str/join " " prompt-parts))
      (let [arg  (first args)
            more (next args)]
        (case arg
          "--json"           (recur more (assoc opts :json? true) prompt-parts)
          "--edn"            (recur more (assoc opts :edn? true) prompt-parts)
          "--code"           (recur more (assoc opts :code? true) prompt-parts)
          ("--full-trace-stream" "--trace")
          (recur more (assoc opts :full-trace-stream? true) prompt-parts)
          ("--full-trace-edn-stream" "--trace-stream")
          (recur more (assoc opts :full-trace-edn-stream? true) prompt-parts)
          ("--full-trace-json-stream" "--full-trace-json-stream-raw")
          (recur more (assoc opts :full-trace-json-stream? true) prompt-parts)
          ("--help" "-h")    (assoc opts :help? true :prompt "")
          "--debug"          (recur more (assoc opts :debug? true) prompt-parts)
          "--provider"       (recur (next more) (assoc opts :provider (first more)) prompt-parts)
          "--model"          (recur (next more) (assoc opts :model (first more)) prompt-parts)
          "--name"           (recur (next more) (assoc opts :agent-name (first more)) prompt-parts)
          "--db"             (recur (next more) (assoc opts :db (first more)) prompt-parts)
          "--persist"        (recur more (assoc opts :persist? true) prompt-parts)
          (recur more opts (conj prompt-parts arg)))))))

(defn- print-run-usage! []
  (stdout! "Usage: vis run [FLAGS] \"prompt\"")
  (stdout! "")
  (stdout! "Flags:")
  (stdout! "  --json            Print result as a single JSON envelope.")
  (stdout! "  --edn             Print result as EDN.")
  (stdout! "  --code            Print only [:code] block contents from the")
  (stdout! "                    answer IR. Concatenated in source order;")
  (stdout! "                    no fences, no language tags. Pipes cleanly")
  (stdout! "                    into editors / interpreters. Errors when")
  (stdout! "                    the answer contains no [:code] blocks.")
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
  (stdout! "  --name NAME          Set the agent name (default: cli).")
  (stdout! "  --db PATH|:memory    Override the SQLite path (or :memory).")
  (stdout! "  --persist            Write this run to ~/.vis/vis.mdb as a")
  (stdout! "                       `:cli` conversation. Default is ephemeral:")
  (stdout! "                       no resume, no conversation row on disk.")
  (stdout! "")
  (stdout! "Examples:")
  (stdout! "  vis run \"Throwaway one-shot probe\"")
  (stdout! "  vis run --json --model gpt-4o \"Explain auth flow\"")
  (stdout! "  vis run --persist --provider anthropic --model claude-sonnet-4-20250514 \"Keep this\""))

(defn- cli-run!
  "`vis run` handler. `_parsed` is unused - we re-parse the residual
   ourselves so anything that isn't a flag falls into the prompt."
  [_parsed residual]
  (config/init-cli!)
  (let [{:keys [prompt json? edn? code? full-trace-stream?
                full-trace-edn-stream? full-trace-json-stream?
                help? agent-name db] :as opts}
        (parse-run-args residual)]
    (when (or help? (str/blank? prompt))
      (print-run-usage!)
      (System/exit 0))
    (let [agent-def (agent {:name (or agent-name "cli")})
          trace-on-chunk (cond
                           full-trace-json-stream?
                           #(print-full-trace-json-frame! :trace-chunk %)

                           full-trace-edn-stream?
                           #(print-full-trace-edn-frame! :trace-chunk %)

                           full-trace-stream?
                           (make-pretty-trace-printer))
          run-opts  (cond-> (dissoc opts :prompt :json? :edn? :code?
                              :full-trace-stream? :full-trace-edn-stream?
                              :full-trace-json-stream? :compact? :agent-name :db)
                      trace-on-chunk (assoc :on-chunk trace-on-chunk)
                      db (assoc :db (config/resolve-db-spec
                                      (if (= db ":memory") :memory
                                        {:backend :sqlite :path db}))))
          result    (run! agent-def prompt run-opts)
          trace-result (select-keys result [:conversation-id :answer :trace
                                            :iteration-count :duration-ms
                                            :tokens :cost :confidence
                                            :status :error :type])]
      (cond
        full-trace-json-stream?
        (do
          (print-full-trace-json-frame! :result trace-result)
          (when (:error result)
            (shutdown-agents)
            (System/exit 1)))

        full-trace-edn-stream?
        (do
          (print-full-trace-edn-frame! :result trace-result)
          (when (:error result)
            (shutdown-agents)
            (System/exit 1)))

        full-trace-stream?
        (do (tel/log! {:level :info :id ::cli-trace
                       :data  trace-result}
              "CLI trace result")
          (stdout! (str "\n" (trace-dim "╰────────────────────────────────────────────────────────")))
          (stdout! (str "\n" (trace-title "◆" "final result")
                     (pretty-block "summary" (trace-final-summary-prose result))))
          (stdout! (str "\n" (trace-title "◆" "answer") "\n"))
          (stdout! (render/render (:answer result) :markdown))
          (when (:error result)
            (when-let [ex (:exception result)]
              (stdout! "\nStack trace:")
              (.printStackTrace ^Throwable ex ^java.io.PrintStream config/original-stdout))
            (shutdown-agents)
            (System/exit 1)))

        json? (stdout! (result->json result))
        edn?  (stdout! (result->edn result))

        code?
        (let [blocks (render/extract-code (:answer result))]
          (cond
            (:error result)
            (do (stdout! (error/format-error (:error result)))
              (shutdown-agents)
              (System/exit 1))

            (empty? blocks)
            (do (stdout! "Error: --code expects answer to contain at least one [:code] block; got prose only. Run without --code for rendered output.")
              (shutdown-agents)
              (System/exit 1))

            :else
            (stdout! (str/join "\n\n" blocks))))

        (:error result)
        (do (stdout! (error/format-error (:error result)))
          (shutdown-agents)
          (System/exit 1))

        :else
        (do (stdout! (render/render (:answer result) :markdown))
          (when (:duration-ms result)
            (stdout! (str "\n[" (fmt/format-meta-line result) "]")))))
      (shutdown-agents))))

;;; ── `vis conversations` ─────────────────────────────────────────────────

(def ^:private known-channels #{"tui" "telegram" "cli"})
(def ^:private known-channel-filters (conj known-channels "all"))
(def ^:private default-conversation-channels [:tui :telegram :cli])

(defn- resolve-conversation-by-prefix
  "Resolve a user-supplied conversation reference (full UUID or an
   unambiguous prefix) to the canonical UUID. Scans every channel
   because forks are channel-agnostic; the user typed an id, we find
   it. Returns nil on miss or ambiguous prefix."
  [d input]
  (let [s (str input)]
    (or (try (persistance/db-resolve-conversation-id d s) (catch Throwable _ nil))
      (let [all     (mapcat #(lp/by-channel %) [:tui :telegram :cli])
            matches (vec (filter #(str/starts-with? (str (:id %)) s) all))]
        (when (= 1 (count matches))
          (persistance/db-resolve-conversation-id d (str (:id (first matches)))))))))

(defn- cli-fork-conversation!
  "Fork a conversation by id. Creates a new `conversation_state` row
   that points at the latest state as its parent, optionally with a
   user-supplied title. Prints the new state UUID; the conversation
   id (soul-id) stays the same so `vis channels tui --conversation-id
   <ID>` keeps working and now resumes from the fork."
  [cid-input title]
  (let [d        (lp/db-info)
        resolved (resolve-conversation-by-prefix d cid-input)]
    (cond
      (nil? resolved)
      (do (stdout! (str "Conversation not found: " cid-input))
        (stdout! "")
        (stdout! "List existing conversations with:")
        (stdout! "  vis conversations")
        (shutdown-agents)
        (System/exit 1))

      :else
      (let [opts      (cond-> {} (and title (not (str/blank? title)))
                        (assoc :title title))
            new-state (persistance/db-fork-conversation! d resolved opts)]
        (if new-state
          (do (stdout! "")
            (stdout! (str "  Forked conversation " resolved))
            (when title (stdout! (str "  Title:        " title)))
            (stdout! (str "  New state-id: " new-state))
            (stdout! "")
            (stdout! (str "  Resume with: vis channels tui --conversation-id " resolved))
            (stdout! ""))
          (do (stdout! (str "Failed to fork conversation " resolved
                         "; no existing state to fork from."))
            (shutdown-agents)
            (System/exit 1)))
        (shutdown-agents)))))

(defn- conversation-sort-key
  [{:keys [last-turn-at created-at id]}]
  [(- (long (or (some-> last-turn-at inst-ms) 0)))
   (- (long (or (some-> created-at inst-ms) 0)))
   (str id)])

(defn- conversation-row
  [d c]
  (let [turns        (or (persistance/db-list-conversation-turns d (:id c)) [])
        last-turn    (last turns)
        channel-name (name (or (:channel c) :unknown))]
    {:id           (str (:id c))
     :title        (or (:title c) "-")
     :last-channel channel-name
     :turns        (count turns)
     :forks        (long (or (:fork-count c) 0))
     :last-turn-at (:created-at last-turn)
     :last-turn    (or (some-> last-turn :created-at fmt/format-date) "-")
     :created-at   (:created-at c)
     :created      (or (fmt/format-date (:created-at c)) "-")}))

(defn- conversation-rows
  [d convs]
  (->> convs
    (mapv #(conversation-row d %))
    (sort-by conversation-sort-key)
    vec))

(defn- conversations-for-listing
  [channel-input]
  (let [channels (if channel-input [(keyword channel-input)] default-conversation-channels)]
    (mapcat lp/by-channel channels)))

(defn- cli-list-conversations!
  "List persisted conversations. `channel-input` filters to one channel;
   nil lists every known channel. Rows sort by most recent turn first,
   with empty conversations after conversations that have turns."
  [channel-input]
  (let [channel-label (or channel-input "all")
        convs         (conversations-for-listing channel-input)
        d             (lp/db-info)]
    (if (empty? convs)
      (stdout! (if channel-input
                 (str "No " channel-input " conversations found.")
                 "No conversations found."))
      (let [rows (conversation-rows d convs)]
        (stdout! (str "\n  " (if channel-input
                               (str/upper-case channel-label)
                               "All")
                   " Conversations\n"))
        (print-table!
          [{:key :id           :label "ID"           :width 36 :align :left}
           {:key :title        :label "Title"        :width 24 :align :left :grow? true}
           {:key :last-channel :label "Last Channel" :width 12 :align :left}
           {:key :turns        :label "Turns"        :width 5  :align :right}
           {:key :forks        :label "Forks"        :width 5  :align :right}
           {:key :last-turn    :label "Last Turn"    :width 16 :align :left}
           {:key :created      :label "Created"      :width 16 :align :left}]
          rows)
        (stdout! (str "\n  " (count rows) " conversation(s)\n"))
        (stdout! "  Resume with: vis channels tui --conversation-id <ID>  (full or short)")
        (stdout! "  Or latest:   vis channels tui --resume")
        (stdout! "  Fork:        vis conversations --fork <ID> [--title TITLE]"))))
  (shutdown-agents))

(def ^:private search-field-labels
  {"answer_text"   "answer"
   "thinking_text" "thinking"
   "comments_text" "comments"
   "user_request"  "prompt"
   "expression"    "expression"})

(defn- cli-conversations-search!
  "`vis conversations search <query>` handler. Runs an FTS5 search
   across user prompts, assistant answers, model thinking, per-block
   comments, and persisted expression source. Hits print one per
   line:

     <conversation-id-prefix>  <field>  <snippet>

   Snippets carry `[match]` markers around hit terms so the user can
   see context. `--limit N` caps the result count (default 25)."
  [parsed _residual]
  (config/init-cli!)
  (let [query (or (get parsed "query") "")
        limit (or (some-> (get parsed "limit") str/trim Long/parseLong) 25)]
    (cond
      (str/blank? query)
      (do (stdout! "vis conversations search <query> [--limit N]")
        (stdout! "")
        (stdout! "Searches conversation answers, thinking, comments,")
        (stdout! "prompts, and persisted expression source.")
        (shutdown-agents)
        (System/exit 1))

      :else
      (let [d    (lp/db-info)
            hits (or (persistance/db-search d query {:limit limit}) [])]
        (cond
          (empty? hits)
          (do (stdout! (str "No matches for: " query))
            (shutdown-agents))

          :else
          (do
            (stdout! (str (count hits) " match" (when (not= 1 (count hits)) "es")
                       " for: " query))
            (stdout! "")
            (doseq [hit hits]
              (let [label   (get search-field-labels (:field hit) (:field hit))
                    id-pref (let [s (str (:owner-id hit))]
                              (subs s 0 (min 8 (count s))))
                    snippet (str/replace (or (:snippet hit) "") #"\s+" " ")]
                (stdout!
                  (str id-pref
                    "  " (format "%-8s" label)
                    "  " snippet))))
            (shutdown-agents)))))))

(defn- cli-conversations!
  "`vis conversations` handler.

   Two modes:
   - List   --  `vis conversations [all|tui|telegram|cli]`
   - Fork   --  `vis conversations --fork <CONVERSATION-ID> [--title TITLE]`

   The `parsed` map carries the spec'd flags; the bare positional
   `channel` (if present) is also in `parsed`. Anything not in the
   spec is rejected upstream by `commandline/dispatch!`."
  [parsed _residual]
  (config/init-cli!)
  (let [fork-target (get parsed "fork")
        title       (get parsed "title")
        channel     (get parsed "channel")]
    (cond
      (and (some? fork-target) (not (str/blank? fork-target)))
      (cli-fork-conversation! fork-target title)

      :else
      (let [ch (when (and channel (not= "all" channel))
                 (when (contains? known-channels channel) channel))]
        (when (and channel (not (contains? known-channel-filters channel)))
          (stdout! (str "Unknown channel: " channel
                     ". Expected one of: " (str/join ", " (sort known-channel-filters))
                     ". Showing all conversations."))
          (stdout! ""))
        (cli-list-conversations! ch)))))

;;; ── `vis providers` ─────────────────────────────────────────────────────

(def ^:private providers-table-cols
  [{:key :id       :label "ID"          :width 18 :align :left}
   {:key :label    :label "Label"       :width 28 :align :left}
   {:key :auth     :label "Auth"        :width  6 :align :left}
   {:key :rpm      :label "Catalog RPM" :width 11 :align :right}
   {:key :tpm      :label "Catalog TPM" :width 12 :align :right}
   {:key :base-url :label "Base URL"    :width 36 :align :left}])

(defn- safe-provider-status
  [provider]
  (try
    (cond
      (:provider/status-fn provider) ((:provider/status-fn provider))
      (:provider/detect-fn provider) {:authenticated? (boolean ((:provider/detect-fn provider)))}
      :else                          nil)
    (catch Throwable e
      {:authenticated? false
       :error          (or (ex-message e) (str e))})))

(defn- configured-provider-entry
  [provider-id]
  (->> (or (:providers (config/current-config)) [])
    (filter #(= provider-id (:id %)))
    first))

(defn- configured-provider-status
  [provider]
  (let [provider-id (:provider/id provider)
        configured  (configured-provider-entry provider-id)]
    (cond
      (some? (:api-key configured))
      {:authenticated? true
       :source         :config
       :config-path    config/config-path}

      :else
      (or (safe-provider-status provider)
        {:authenticated? false}))))

(defn- configured-provider-base-url
  [provider-id]
  (or (:base-url (configured-provider-entry provider-id))
    (some-> provider-id config/provider-template :base-url)))

(defn- provider-label-for-id
  [provider-id]
  (or (some-> (registry/provider-by-id provider-id) :provider/label)
    (some-> (config/provider-template provider-id) :label)
    (str/capitalize (name provider-id))))

(defn- status-entry-label
  [k]
  (-> (name k)
    (str/replace #"-" " ")
    (str/capitalize)))

(defn- format-status-value
  [v]
  (cond
    (keyword? v) (name v)
    :else        (str v)))

(defn- format-limit-window
  [{:keys [kind unit size resets-at-ms]}]
  (when kind
    (str (name kind)
      (when unit
        (str " " (or size 1) "/" (name unit)))
      (when resets-at-ms
        (str ", resets " (fmt/format-date (java.util.Date. (long resets-at-ms))))))))

(defn- format-limit-row
  [{:keys [label scope kind unlimited? used limit remaining note window]}]
  (let [quota (cond
                unlimited?      "unlimited"
                (number? limit) (str (when (number? used) (str used "/")) limit
                                  (when (number? remaining)
                                    (str " (" remaining " left)")))
                (number? used)  (str "used " used)
                :else           nil)
        attrs (->> [(some-> scope name)
                    (some-> kind name)
                    (format-limit-window window)]
                (remove nil?))]
    (str label
      (when (seq attrs)
        (str " [" (str/join ", " attrs) "]"))
      (when quota
        (str ": " quota))
      (when note
        (str " - " note)))))

(defn- provider-limit-lines
  [provider-id]
  (let [report   (provider-limits/provider-limits provider-id)
        static   (:static report)
        dynamic  (get-in report [:dynamic :limits])
        note     (get-in report [:dynamic :note])
        error*   (:error report)]
    (vec
      (concat [(str "  Limits status: " (name (:status report)))]
        (when-let [rpm (:rpm static)]
          [(str "  Catalog RPM:    " rpm)])
        (when-let [tpm (:tpm static)]
          [(str "  Catalog TPM:    " tpm)])
        (if (seq dynamic)
          (concat ["  Dynamic limits:"]
            (map #(str "    - " (format-limit-row %)) dynamic))
          ["  Dynamic limits: none reported"])
        (when note
          [(str "  Note:           " note)])
        (when (seq static)
          ["  Catalog RPM / TPM come from svar metadata, not live account quota usage."])
        (when error*
          [(str "  Error:          " (:message error*))])))))

(defn- print-provider-status!
  [provider]
  (let [status      (or (configured-provider-status provider) {:authenticated? false})
        provider-id  (:provider/id provider)
        base-url     (configured-provider-base-url provider-id)
        rows     (->> status
                   (remove (fn [[k _]] (= k :authenticated?)))
                   (sort-by (comp str key)))]
    (stdout! (str "\n  " (:provider/label provider) " Provider Status"))
    (stdout! "  ─────────────────────────────────")
    (when base-url
      (stdout! (str "  Base URL:       " base-url)))
    (stdout! (str "  Authenticated:  " (if (:authenticated? status) "yes" "no")))
    (doseq [[k v] rows]
      (stdout! (str "  " (commandline/pad-right (str (status-entry-label k) ":") 15)
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
    (mapv (fn [provider]
            (let [status   (configured-provider-status provider)
                  report   (provider-limits/provider-limits (:provider/id provider))
                  base-url (configured-provider-base-url (:provider/id provider))]
              {:id       (name (:provider/id provider))
               :label    (:provider/label provider)
               :auth     (if (:authenticated? status) "yes" "no")
               :rpm      (or (some-> report :static :rpm str) "-")
               :tpm      (or (some-> report :static :tpm str) "-")
               :base-url (or base-url "-")})))))

(defn- print-registered-providers!
  []
  (let [all (registry/registered-providers)]
    (if (seq all)
      (do (stdout! "Available providers:")
        (doseq [p (sort-by :provider/id all)]
          (stdout! (str "  " (commandline/pad-right (name (:provider/id p)) 22)
                     (:provider/label p)))))
      (stdout! "No providers registered. Drop a vis-provider-* jar onto the classpath."))))

(defn- cli-providers-list! [_parsed _residual]
  (config/init-cli!)
  (let [rows (providers-list-rows)]
    (if (empty? rows)
      (stdout! "No providers registered. Drop a vis-provider-* jar onto the classpath.")
      (do (stdout! "\n  Providers\n")
        (print-table! providers-table-cols rows)
        (stdout! (str "\n  " (count rows) " provider(s)\n")))))
  (shutdown-agents))

(defn- cli-providers-status! [_parsed residual]
  (config/init-cli!)
  (let [provider-name (first residual)
        provider-id   (some-> provider-name keyword)
        provider      (when provider-id (registry/provider-by-id provider-id))
        providers     (if provider-name
                        (if provider [provider] [])
                        (sort-by :provider/id (registry/registered-providers)))]
    (cond
      (and provider-name (nil? provider))
      (do (stdout! (str "Unknown provider: " provider-name))
        (stdout! "")
        (print-registered-providers!))

      (empty? providers)
      (stdout! "No providers registered. Drop a vis-provider-* jar onto the classpath.")

      :else
      (doseq [p providers]
        (print-provider-status! p))))
  (shutdown-agents))

(defn- cli-providers-limits! [_parsed residual]
  (config/init-cli!)
  (let [provider-name (first residual)
        registered    (sort-by :provider/id (registry/registered-providers))]
    (if provider-name
      (let [provider-id (keyword provider-name)
            known?      (or (registry/provider-by-id provider-id)
                          (config/provider-template provider-id)
                          (seq (:static (provider-limits/provider-limits provider-id))))]
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

(defn- cli-providers-auth! [parsed residual]
  (config/init-cli!)
  (let [provider-name (or (get parsed "provider") (first residual))
        provider-id   (some-> provider-name keyword)
        provider      (when provider-id (registry/provider-by-id provider-id))]
    (cond
      (nil? provider-id)
      (do (stdout! "Usage: vis providers auth <provider>")
        (stdout! "")
        (print-registered-providers!))

      (nil? provider)
      (do (stdout! (str "Unknown provider: " provider-name))
        (stdout! "")
        (print-registered-providers!))

      (nil? (:provider/auth-fn provider))
      (stdout! (str "Provider " (:provider/label provider) " does not expose an interactive auth flow."))

      :else
      (try
        ((:provider/auth-fn provider) stdout!)
        (catch Exception e
          (stdout! (error/format-error (str "Authentication failed: " (ex-message e))))))))
  (shutdown-agents))

(defn- cli-providers-logout! [parsed residual]
  (config/init-cli!)
  (let [provider-name (or (get parsed "provider") (first residual))
        provider-id   (some-> provider-name keyword)
        provider      (when provider-id (registry/provider-by-id provider-id))
        configured?   (boolean
                        (some #(= provider-id (:id %))
                          (:providers (config/load-config-raw))))]
    (cond
      (nil? provider-id)
      (do (stdout! "Usage: vis providers logout <provider>")
        (stdout! "")
        (print-registered-providers!))

      (nil? provider)
      (do (stdout! (str "Unknown provider: " provider-name))
        (stdout! "")
        (print-registered-providers!))

      (and (nil? (:provider/logout-fn provider)) (not configured?))
      (stdout! (str "Provider " (:provider/label provider) " does not persist credentials."))

      :else
      (do
        (when-let [logout-fn (:provider/logout-fn provider)]
          (logout-fn))
        (config/remove-config-provider! provider-id :cli-provider-logout)
        (stdout! (str "  Logged out of " (:provider/label provider) ". Tokens and config cleared.")))))
  (shutdown-agents))

;;; ── `vis doctor` ────────────────────────────────────────────────────────

(defn- cli-doctor! [_parsed _residual]
  (config/init-cli!)
  (let [env  {:db-info (config/resolve-db-spec)}
        msgs (doctor/run-checks env)]
    (stdout! (doctor/format-output msgs))
    (System/exit (int (doctor/exit-code msgs)))))

;;; ── `vis extensions` ────────────────────────────────────────────────────

(def ^:private extensions-table-cols
  [{:key :namespace :label "Namespace"   :width 28 :align :left}
   {:key :group     :label "Group"       :width 18 :align :left}
   {:key :author    :label "Author"      :width 12 :align :left}
   {:key :owner     :label "Owner"       :width  8 :align :left}
   {:key :license   :label "License"     :width 10 :align :left}
   {:key :doc       :label "Description" :width 36 :align :left :grow? true}
   {:key :version   :label "Version"     :width 10 :align :left}])

(defn- cli-extensions! [_parsed _residual]
  (config/init-cli!)
  (let [exts  (list-extensions)
        cols  (expand-table-cols extensions-table-cols (terminal-width))
        width (table-width cols)]
    (if (empty? exts)
      (stdout! "No extensions registered.")
      (do (stdout! "\n  Extensions\n")
        (doseq [[kind rows] (sort-by key (group-by :kind exts))]
          (print-section-heading! kind width)
          (print-table! cols
            (sort-by (juxt :group :namespace) rows)))
        (stdout! (str "\n  " (count exts) " extension(s)\n")))))
  (shutdown-agents))

(defn- safe-extension-name
  [s]
  (let [name (some-> s str str/trim (str/replace #"[^A-Za-z0-9._-]+" "-") (str/replace #"^-+|-+$" ""))]
    (when (seq name) name)))

(defn- extension-namespace
  [name explicit]
  (let [base (or (some-> explicit str/trim not-empty)
               (str "vis.ext." (-> name
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
  (let [ns-sym (extension-namespace name namespace)
        ns-path (namespace->path ns-sym)]
    {"deps.edn" (str "{:paths [\"src\" \"resources\"]\n"
                  " :deps {}}\n")
     "resources/META-INF/vis-extension/vis.edn" (pr-str {(symbol name) {:nses [ns-sym]}})
     (str "src/" ns-path) (str "(ns " ns-sym "\n"
                            "  (:require [com.blockether.vis.core :as vis]))\n\n"
                            "(defn hello\n"
                            "  []\n"
                            "  \"hello from " name "\")\n\n"
                            "(def vis-extension\n"
                            "  (vis/extension\n"
                            "    {:ext/namespace '" ns-sym "\n"
                            "     :ext/doc \"User extension " name "\"\n"
                            "     :ext/version \"0.1.0\"\n"
                            "     :ext/author \"local\"\n"
                            "     :ext/owner \"local\"\n"
                            "     :ext/kind \"user\"}))\n\n"
                            "(vis/register-extension! vis-extension)\n")}))

(defn- parse-scaffold-opts
  [parsed residual]
  (let [argv (vec residual)
        parsed-name (:name parsed)
        parsed-dir (:dir parsed)
        parsed-namespace (:namespace parsed)
        force? (boolean (or (:force parsed) (some #{"--force"} argv)))
        parsed-argv (loop [xs argv
                           positional []
                           opts {}]
                      (if-let [x (first xs)]
                        (case x
                          "--force" (recur (rest xs) positional opts)
                          "--dir" (recur (nnext xs) positional (assoc opts :dir (second xs)))
                          "--namespace" (recur (nnext xs) positional (assoc opts :namespace (second xs)))
                          (recur (rest xs) (conj positional x) opts))
                        (assoc opts :positional positional)))
        dir (or parsed-dir (:dir parsed-argv))
        namespace (or parsed-namespace (:namespace parsed-argv))
        name (safe-extension-name (or parsed-name (first (:positional parsed-argv))))]
    {:name name :dir dir :namespace namespace :force? force?}))

(defn- cli-extensions-scaffold!
  [parsed residual]
  (config/init-cli!)
  (let [{:keys [name dir force?] :as opts} (parse-scaffold-opts parsed residual)]
    (when-not name
      (throw (ex-info "Usage: vis extensions scaffold <name> [--dir DIR] [--namespace NS] [--force]"
               {:type :cli/usage})))
    (let [target-path (or dir (str ".vis/vis-extensions/" name))
          target (let [f (io/file target-path)]
                   (if (.isAbsolute f)
                     f
                     (io/file (System/getProperty "user.dir") target-path)))
          files (scaffold-extension-files opts)]
      (doseq [[rel content] files]
        (let [f (io/file target rel)]
          (when (and (.exists f) (not force?))
            (throw (ex-info "Refusing to overwrite existing extension file"
                     {:type :extension/scaffold-file-exists
                      :path (.getPath f)})))
          (.mkdirs (.getParentFile f))
          (spit f content)))
      (stdout! (str "Created extension scaffold at " (.getPath target) "\n"
                 "It is auto-loaded when you run vis from this project (or from ~/.vis/vis-extensions)."))))
  (shutdown-agents))

(defn- source-checkout-root
  []
  (or (some-> (System/getenv "VIS_SOURCE_ROOT") not-empty io/file)
    (some-> (io/resource "com/blockether/vis/internal/main.clj")
      str
      (str/replace-first #"^file:" "")
      java.net.URLDecoder/decode
      io/file
      .getParentFile .getParentFile .getParentFile .getParentFile .getParentFile
      .getParentFile)
    (io/file ".")))

(defn- git-checkout?
  [dir]
  (.exists (io/file dir ".git")))

(defn- process-result
  [cmd dir]
  (let [p (process/process {:cmd cmd :dir (.getPath (io/file dir)) :out :string :err :string})
        proc (:proc p)
        _ (.waitFor ^Process proc)
        result @p]
    {:exit (:exit result)
     :out (:out result)
     :err (:err result)}))

(defn- cli-update!
  [_parsed _residual]
  (config/init-cli!)
  (let [root (source-checkout-root)]
    (when-not (git-checkout? root)
      (throw (ex-info "Vis update requires a git source checkout"
               {:type :update/not-git-checkout
                :path (.getPath root)})))
    (stdout! (str "Updating Vis source at " (.getPath root)))
    (let [fetch (process-result ["git" "fetch" "--tags" "origin"] root)
          pull (when (zero? (:exit fetch))
                 (process-result ["git" "pull" "--ff-only"] root))]
      (when-not (zero? (:exit fetch))
        (throw (ex-info "git fetch failed"
                 {:type :update/git-fetch-failed
                  :stderr (:err fetch)
                  :stdout (:out fetch)})))
      (when-not (zero? (:exit pull))
        (throw (ex-info "git pull --ff-only failed"
                 {:type :update/git-pull-failed
                  :stderr (:err pull)
                  :stdout (:out pull)})))
      (stdout! (str/trim (or (:out pull) "")))
      (stdout! "Vis update complete.")))
  (shutdown-agents))

;;; ── Top-level binary built-ins (registry/register-cmd! direct) ─────────
;;
;; `run`, `providers`, `conversations`, `doctor` are the binary's own
;; commands. They live at the top of the command tree -- `vis run
;; "..."`, NOT `vis extensions run "..."` -- so they bypass
;; `:ext/cli` (the extensions-subcommand slot, see below).
;; Direct `register-cmd!` is the right plumbing here; vis-runtime
;; is the host, not an extension contributing to `vis extensions`.

(doseq [spec
        [{:cmd/name  "run"
          :cmd/doc   "Run a one-shot agent turn and print the answer."
          :cmd/usage "vis run [FLAGS] \"prompt\""
          :cmd/args  [{:name "json"       :kind :flag :type :boolean :doc "Output result as JSON."}
                      {:name "edn"        :kind :flag :type :boolean :doc "Output result as EDN."}
                      {:name "full-trace-stream" :kind :flag :type :boolean :doc "Stream a pretty terminal trace."}
                      {:name "full-trace-edn-stream" :kind :flag :type :boolean :doc "Stream raw EDN trace frames."}
                      {:name "full-trace-json-stream" :kind :flag :type :boolean :doc "Stream raw JSON trace frames."}
                      {:name "debug"      :kind :flag :type :boolean :doc "Enable svar debug logging."}
                      {:name "model"      :kind :flag :type :string  :doc "Override the LLM model (accepts provider/name syntax)."}
                      {:name "provider"   :kind :flag :type :string  :doc "Override the LLM provider (e.g. openai, anthropic)."}
                      {:name "name"       :kind :flag :type :string  :doc "Agent name."}
                      {:name "db"         :kind :flag :type :string  :doc "DB target: PATH or :memory."}
                      {:name "persist"    :kind :flag :type :boolean :doc "Persist this run to ~/.vis/vis.mdb as a :cli conversation."}]
          :cmd/examples ["vis run \"Throwaway one-shot probe\""
                         "vis run --json --model gpt-4o \"Explain the auth flow\""
                         "vis run --persist --provider anthropic --model claude-sonnet-4-20250514 \"Keep this conversation\""]
          :cmd/run-fn cli-run!}

         {:cmd/name  "providers"
          :cmd/doc   "Inspect, authenticate, and introspect LLM providers."
          :cmd/usage "vis providers <list|status|limits|auth|logout> [...]"
          :cmd/subcommands #(registry/registered-under ["providers"])}

         {:cmd/name  "conversations"
          :cmd/doc   "List conversations stored on disk, or fork / search them."
          :cmd/usage "vis conversations [all|tui|telegram|cli] [--fork ID [--title TITLE]]"
          :cmd/args  [{:name "channel" :kind :positional :type :string
                       :doc  "Optional channel filter (all|tui|telegram|cli; default all)."}
                      {:name "fork"  :kind :flag :type :string
                       :doc  "Fork the conversation with the given id (full UUID or unambiguous prefix)."}
                      {:name "title" :kind :flag :type :string
                       :doc  "Title to set on the new fork (used with --fork)."}]
          :cmd/examples ["vis conversations"
                         "vis conversations telegram"
                         "vis conversations --fork 3a7b2c1d-..."
                         "vis conversations --fork 3a7b2c1d --title \"Branch A\""
                         "vis conversations search \"foo bar\""]
          :cmd/subcommands #(registry/registered-under ["conversations"])
          :cmd/run-fn cli-conversations!}

         {:cmd/name  "doctor"
          :cmd/doc   "Run cross-extension diagnostics."
          :cmd/usage "vis doctor"
          :cmd/run-fn cli-doctor!}

         {:cmd/name  "update"
          :cmd/doc   "Update the source checkout used by this Vis installation."
          :cmd/usage "vis update"
          :cmd/run-fn cli-update!}]]
  (registry/register-cmd! spec))

;;; ── Extensions-namespaced subcommand: `vis extensions list` ─────────────
;;
;; `extensions list` introspects the extension registry, so it
;; naturally lives under the `vis extensions <cmd>` parent. But it is
;; a HOST-owned built-in, NOT a third-party contribution -- vis core
;; is the host, never an extension. So it registers directly via
;; `registry/register-cmd!` with `:cmd/parent ["extensions"]`, the
;; same plumbing the top-level built-ins above use.

(doseq [spec
        [{:cmd/name   "list"
          :cmd/parent ["providers"]
          :cmd/doc    "List registered providers with auth state, static limits, and base URLs."
          :cmd/usage  "vis providers list"
          :cmd/run-fn cli-providers-list!}
         {:cmd/name   "status"
          :cmd/parent ["providers"]
          :cmd/doc    "Show provider authentication status together with static/dynamic limits."
          :cmd/usage  "vis providers status [provider]"
          :cmd/examples ["vis providers status"
                         "vis providers status github-copilot-business"
                         "vis providers status openai-codex"]
          :cmd/run-fn cli-providers-status!}
         {:cmd/name   "limits"
          :cmd/parent ["providers"]
          :cmd/doc    "Show provider rate-limit metadata and any dynamic quota report."
          :cmd/usage  "vis providers limits [provider]"
          :cmd/examples ["vis providers limits"
                         "vis providers limits openai-codex"
                         "vis providers limits ollama"]
          :cmd/run-fn cli-providers-limits!}
         {:cmd/name   "auth"
          :cmd/parent ["providers"]
          :cmd/doc    "Run a provider's interactive authentication flow."
          :cmd/usage  "vis providers auth <provider>"
          :cmd/args   [{:name "provider" :kind :positional :type :string
                        :doc  "Registered provider id (for example: github-copilot-business or openai-codex)."}]
          :cmd/examples ["vis providers auth github-copilot-business"
                         "vis providers auth github-copilot-individual"
                         "vis providers auth openai-codex"]
          :cmd/run-fn cli-providers-auth!}
         {:cmd/name   "logout"
          :cmd/parent ["providers"]
          :cmd/doc    "Clear saved credentials for a provider."
          :cmd/usage  "vis providers logout <provider>"
          :cmd/args   [{:name "provider" :kind :positional :type :string
                        :doc  "Registered provider id."}]
          :cmd/examples ["vis providers logout github-copilot-business"
                         "vis providers logout github-copilot-individual"
                         "vis providers logout openai-codex"]
          :cmd/run-fn cli-providers-logout!}
         {:cmd/name   "search"
          :cmd/parent ["conversations"]
          :cmd/doc    "Full-text search across answers, thinking, comments, prompts, and expressions."
          :cmd/usage  "vis conversations search <query> [--limit N]"
          :cmd/args   [{:name "query" :kind :positional :type :string
                        :doc  "FTS5 query (`foo bar` for AND, `foo OR bar`, `foo*` for prefix)."}
                       {:name "limit" :kind :flag :type :string
                        :doc  "Max hits to print (default 25)."}]
          :cmd/examples ["vis conversations search \"znajduje nodes\""
                         "vis conversations search \"refactor*\""
                         "vis conversations search \"foo OR bar\" --limit 100"]
          :cmd/run-fn cli-conversations-search!}
         {:cmd/name   "list"
          :cmd/parent ["extensions"]
          :cmd/doc    "List every registered extension with metadata."
          :cmd/usage  "vis extensions list"
          :cmd/run-fn cli-extensions!}
         {:cmd/name   "scaffold"
          :cmd/parent ["extensions"]
          :cmd/doc    "Create a user extension project scaffold."
          :cmd/usage  "vis extensions scaffold <name> [--dir DIR] [--namespace NS] [--force]"
          :cmd/examples ["vis extensions scaffold my-tools"
                         "vis extensions scaffold my-tools --dir ~/.vis/vis-extensions/my-tools"]
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

(defn- debug-mode? [args]
  (or (some debug-flags args)
    (= "1" (System/getenv "VIS_DEBUG"))))

(defn- log-file-path []
  (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis"))]
    (when-not (.exists log-dir) (.mkdirs log-dir))
    (str log-dir "/vis.log")))

(defn- configure-logging!
  "Route Telemere signals: file handler always on, persistence-backed
   `:db` handler always on (so the loop's `tel/with-ctx+ {:db-info ...}`
   bindings land in the conversation_log table), and the
   `:default/console` handler is OFF by default - it was removed by
   `internal.registry` at namespace load so boot-time registration
   logs never spray to stdout. We re-add it here only when `--debug`
   / `--verbose` / `-v` / `VIS_DEBUG=1` is set. Idempotent."
  [args]
  (let [debug? (debug-mode? args)
        path   (log-file-path)]
    ;; File handler ALWAYS on, so post-mortem reads always have data.
    (try
      (tel/add-handler! :file
        (tel/handler:file {:path path})
        {:min-level :info})
      (catch Throwable _ nil))
    ;; Console handler: re-add only when the user asked for verbosity.
    ;; Boot-time noise is already gone (registry.clj removed it during
    ;; namespace load); this restores the stdout stream for debugging.
    (when debug?
      (try (tel/add-handler! :default/console
             (tel/handler:console))
        (catch Throwable _ nil)))
    ;; Persistence handler: scopes signals to the right DB rows via
    ;; `:db-info` / `:conversation-soul-id` / `:conversation-turn-id` /
    ;; `:iteration-id` carried in telemere `*ctx*`. Wrapped because
    ;; the persistence facade is loaded lazily; if no backend has
    ;; registered yet, the handler will silently drop signals until
    ;; one does.
    (try (setup-db-handler!)
      (catch Throwable _ nil))))

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
   sandbox would loop on `Unable to resolve symbol: v/cat` until
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
          (when path
            (println (str "     manifest: " path))))
        (println)))))

(defn discover-all!
  "Run the unified extension discovery scan. Idempotent through
   Clojure's `require` cache. Returns nil.

   Prints a stderr banner enumerating every extension namespace
   whose `(require)` failed during discovery. The same warnings are
   also fed into the per-turn `<scan-warnings>` system-prompt block
   by `vis-foundation/combined-scan-warnings`, so both the user (at
   the terminal) and the LLM (in its prompt) see the failure
   immediately instead of bouncing off `Unable to resolve symbol`
   for an entire conversation."
  []
  (extension/discover-extensions!)
  (print-extension-load-failures!)
  nil)

;; =============================================================================
;; Root command
;;
;; The dispatcher's root has NO hard-coded subcommands. Every entry
;; comes from the global commandline registry. Built-ins (run, auth,
;; doctor, ...) are registered by vis-runtime; the `vis channel` and
;; `vis ext` parents are registered by the channel and extension
;; facades. Add a third-party jar with its own `register-cmd!`
;; calls and its commands appear here without any code change.
;; =============================================================================

(def ^:private DEFAULT_DOC "vis - iterative coding agent CLI")

(defn root-command
  "Build the root `vis` command tree. Subcommands are pulled fresh on
   every call so newly registered extensions show up immediately."
  []
  (registry/command
    {:cmd/name        "vis"
     :cmd/doc         DEFAULT_DOC
     :cmd/subcommands #(registry/registered-under [])}))

;; =============================================================================
;; Pre-redirect stderr for TTY-owning channels
;;
;; Some leaves (TUI, ncurses) take over the controlling terminal and
;; need stderr re-routed to a log file BEFORE any further class loading
;; triggers JVM warnings. The check is data-driven via
;; `:cmd/owns-tty?`. Channels mark themselves through the channel
;; bridge; nothing here is channel-aware.
;; =============================================================================

(defn- pre-redirect-stderr! [args]
  (when-let [{:keys [command]} (commandline/find-leaf (root-command) (cons "vis" args))]
    (when (:cmd/owns-tty? command)
      (let [log-dir (java.io.File. (str (System/getProperty "user.home") "/.vis"))]
        (when-not (.exists log-dir) (.mkdirs log-dir))
        (System/setErr (java.io.PrintStream.
                         (java.io.FileOutputStream.
                           (str log-dir "/vis.log") true) true))))))

;; =============================================================================
;; Main
;; =============================================================================

(defn- root-help-request?
  "True when args ask only for the root help screen. This path can skip
   extension discovery because the root tree lists built-in parent commands
   only; extension-owned commands are mounted below `extensions` after
   discovery when that subtree is requested."
  [args]
  (or (empty? args)
    (contains? #{["help"] ["--help"] ["-h"]} (vec args))))

(def ^:private first-party-channel-bootstrap-nses
  {"tui"      'com.blockether.vis.ext.channel-tui.core
   "telegram" 'com.blockether.vis.ext.channel-telegram.bot})

(defn- help-request?
  "True when args request help at any command depth. We can usually render
   help without initializing runtime resources; if a command is not registered
   yet, the caller falls back to full extension discovery."
  [args]
  (boolean
    (or (root-help-request? args)
      (some #{"--help" "-h"} args))))

(defn- channel-help-request?
  "True for `vis channels <first-party-channel> --help`. These requests need
   only the selected channel descriptor, not every extension namespace."
  [args]
  (let [[parent channel & more] (vec args)]
    (and (= "channels" parent)
      (contains? first-party-channel-bootstrap-nses channel)
      (boolean (some #{"--help" "-h"} more)))))

(defn- discover-fast-help-deps!
  [args]
  (when (channel-help-request? args)
    (when-let [ns-sym (get first-party-channel-bootstrap-nses (second (vec args)))]
      (require ns-sym))))

(defn- fast-help-dispatched?
  [_measure? args]
  (when (help-request? args)
    (discover-fast-help-deps! args)
    (let [root      (root-command)
          full-args (cons "vis" args)
          {:keys [status]} (commandline/dispatch! root full-args)]
      (= :help status))))

(defn- unknown-command?
  "True when the user typed something the tree doesn't recognize.
   Detected by walking the tree: if `find-leaf` resolves only to the
   ROOT (path length 1) AND there's a residual that isn't a help
   request, the user gave us an unknown command."
  [root args]
  (when (seq args)
    (let [{:keys [path residual]} (commandline/find-leaf root (cons (:cmd/name root) args))]
      (and (= 1 (count path))
        (seq residual)
        (not-any? #{"--help" "-h"} residual)))))

(defn- exit-with-user-error!
  [^Throwable t]
  (stdout! (str "vis: " (or (ex-message t) "error")))
  (shutdown-agents)
  (System/exit 2))

(defn- exit-with-fatal-error!
  [^Throwable t]
  (stdout! (str "vis: fatal error - " (or (ex-message t) (.getName (class t)))))
  (stdout! "See ~/.vis/vis.log for details.")
  (shutdown-agents)
  (System/exit 1))

(defn- truthy-value? [v]
  (contains? #{"1" "true" "yes" "on"} (str/lower-case (str v))))

(defn- measure-arg? [arg]
  (= "--measure" arg))

(defn- strip-global-args [args]
  (vec (remove measure-arg? args)))

(defn- startup-measure? [args]
  (or (some measure-arg? args)
    (truthy-value? (System/getenv "VIS_MEASURE"))
    (truthy-value? (System/getProperty "vis.measure"))))

(defn- elapsed-ms [started-ns]
  (/ (double (- (System/nanoTime) started-ns)) 1000000.0))

(defn- format-ms [ms]
  (String/format java.util.Locale/ROOT "%.1f ms" (object-array [(double ms)])))

(defn- startup-measure-line! [label & kvs]
  (binding [*out* *err*]
    (println
      (str "[vis measure] jvm:" label
        (when (seq kvs)
          (str " " (str/join " " (map str kvs))))))))

(defn- timed-startup! [measure? label f]
  (if measure?
    (let [started (System/nanoTime)]
      (try
        (f)
        (finally
          (startup-measure-line! label (format-ms (elapsed-ms started))))))
    (f)))

(defn- summarize-startup-registries! []
  (let [extensions (extension/registered-extensions)
        channels   (registry/registered-channels)
        providers  (registry/registered-providers)
        commands   (registry/registered-commands)]
    (startup-measure-line! "registry totals"
      (str "extensions=" (count extensions))
      (str "channels=" (count channels))
      (str "providers=" (count providers))
      (str "commands=" (count commands)))
    (doseq [ext extensions]
      (startup-measure-line! "extension"
        (str "ns=" (:ext/namespace ext))
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
     - Unknown command          -> top-level help + exit 1

   No magical fallback to a `run`-as-prompt shortcut -- the dispatcher
   is a pure command tree. Anyone who wants the old single-arg
   ergonomics can register a `:cmd/run-fn` on the root via a custom
   extension."
  [& raw-args]
  (let [main-started (System/nanoTime)
        measure?     (startup-measure? raw-args)
        args         (strip-global-args raw-args)]
    (when measure?
      (System/setProperty "vis.measure" "1"))
    (try
      (timed-startup! measure? "pre-extension-bootstrap"
        #(crac-bootstrap/pre-extension-bootstrap! {:phase :cli}))
      ;; Quiet stdout BEFORE any extension load triggers Telemere registration
      ;; spam - the user only sees logs when they pass --debug / --verbose / -v
      ;; (or set VIS_DEBUG=1).
      (timed-startup! measure? "configure-logging"
        #(configure-logging! args))
      (cond
        (root-help-request? args)
        (println (commandline/render-tree (root-command)))

        (fast-help-dispatched? measure? args)
        nil

        :else
        (do
          (timed-startup! measure? "discover-all+extensions"
            #(discover-all!))
          (when measure?
            (summarize-startup-registries!))
          (timed-startup! measure? "pre-redirect-stderr"
            #(pre-redirect-stderr! args))
          (let [root      (root-command)
                full-args (cons "vis" args)]
            (cond
              (unknown-command? root args)
              (do
                (println (commandline/render-tree root))
                (println)
                (println (str "Unknown command: " (str/join " " args)))
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
              (let [{:keys [status]} (timed-startup! measure? "dispatch"
                                       #(commandline/dispatch! root full-args))]
                (case status
                  :error (System/exit 2)
                  nil))))))
      (catch Throwable t
        (if (:vis/user-error (ex-data t))
          (exit-with-user-error! t)
          (exit-with-fatal-error! t)))
      (finally
        (when measure?
          (startup-measure-line! "main total" (format-ms (elapsed-ms main-started))))))))
