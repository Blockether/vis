(ns com.blockether.vis.internal.extension.registry
  "Three global registries in one place: channels, providers, commands.

   Each descriptor is checked by a local predicate before entering its process registry.

   Channel registry (`:channel/id` keyword):
     channel                  build + validate a descriptor
     register-channel!        register, idempotent on :channel/id
     deregister-channel!      remove by id
     registered-channels      all entries, vec
     channel-by-id            lookup by id
     by-cmd                   lookup by :channel/cmd

   Provider registry (`:provider/id` keyword):
     provider                 build + validate a descriptor
     register-provider!       register, idempotent on :provider/id
     deregister-provider!     remove by id
     registered-providers     all entries, vec
     provider-by-id           lookup by id

   Command registry (`[:cmd/parent :cmd/name]` tuple key):
     command                  build + validate a descriptor
     resolve-subcommands      static vec or dynamic 0-arg fn -> vec
     register-cmd!            register, idempotent on [parent name]
     deregister-cmd!          remove by [parent name]
     registered-commands      all entries, vec (registration order)
     registered-under         filter by parent path

   Channel mounting:
     channel-subcommands      compose `vis-agent channels` subcommand vec
                              from the channel registry + any commands
                              registered with `:cmd/parent [\"channels\"]`.
                              Loading this ns also registers the
                              `vis-agent channels` parent itself.


   Parsing / help rendering / dispatch utilities live in
   `com.blockether.vis.internal.commandline`. The closed initialization manifest
   lives in `com.blockether.vis.internal.extension.manifest`."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel]))

;; Quiet boot
;;
;; Telemere ships with a `:default/console` handler that prints every
;; signal to stdout at INFO and above. That is loud noise for a CLI:
;; every `register-cmd!` / `register-channel!` / `register-extension!`
;; that fires at namespace load (this very file does so at the bottom)
;; would dump a multi-line log entry before the user's `vis ...`
;; output ever appears. The CLI's `configure-logging!` hook in
;; `internal.main` removes the handler too, but it runs AFTER every
;; internal namespace has already loaded and emitted -- by then the
;; noise is on screen.
;;
;; Killing the default handler HERE -- at the very first vis namespace
;; that calls `tel/log!` at load time -- means `vis` boots silently.
;; Channels that genuinely want stdout output (none currently do; the
;; TUI uses `/dev/tty` directly, the CLI prints its own results) can
;; re-add it explicitly. Boot-time registration logs are noise; if a
;; user wants them they pass `--debug` and the CLI re-adds the handler.
(try (tel/remove-handler! :default/console) (catch Throwable _ nil))

(defn- optional-valid? [m k pred] (or (not (contains? m k)) (pred (get m k))))

(defn channel?
  [x]
  (and (map? x)
       (keyword? (:channel/id x))
       (util/non-blank-string? (:channel/cmd x))
       (util/non-blank-string? (:channel/doc x))
       (ifn? (:channel/main-fn x))
       (optional-valid? x :channel/usage util/non-blank-string?)
       (optional-valid? x :channel/owns-tty? boolean?)
       (optional-valid? x :channel/messages-renderer-fn ifn?)
       (optional-valid? x :channel/subcommands #(or (and (vector? %) (every? map? %)) (ifn? %)))))

(defn channel
  "Build and validate a channel descriptor map."
  [descriptor]
  (if (channel? descriptor)
    descriptor
    (throw (ex-info (str "Invalid channel '" (:channel/id descriptor) "'")
                    {:type :channel/invalid-spec
                     :id (:channel/id descriptor)
                     :explain {:valid false :value descriptor}}))))

(def ^:private provider-function-keys
  #{:provider/status-fn :provider/logout-fn :provider/detect-fn :provider/auth-fn
    :provider/auth-start-fn :provider/auth-complete-fn :provider/auth-await-fn
    :provider/get-token-fn :provider/refresh-token-fn :provider/limits-fn :provider/enrich-models-fn
    :provider/on-selected-fn})

(defn provider?
  [x]
  (and (map? x)
       (not (contains? x :provider/prompt-fn))
       (keyword? (:provider/id x))
       (util/non-blank-string? (:provider/label x))
       (every? #(optional-valid? x % ifn?) provider-function-keys)
       (optional-valid? x :provider/preset map?)
       (optional-valid? x :provider/is-managed boolean?)
       (optional-valid? x :provider/limits-cache-ms pos-int?)))

(defn provider
  "Build and validate a provider descriptor."
  [descriptor]
  (if (provider? descriptor)
    descriptor
    (throw (ex-info (str "Invalid provider '" (:provider/id descriptor) "'")
                    {:type :provider/invalid-spec
                     :id (:provider/id descriptor)
                     :explain {:valid false :value descriptor}}))))

(defn- arg?
  [x]
  (and (map? x)
       (util/non-blank-string? (:name x))
       (contains? #{:flag :positional} (:kind x))
       (optional-valid? x :type #{:string :int :boolean :file})
       (optional-valid? x :required boolean?)
       (optional-valid? x :doc string?)))

(defn command?
  [x]
  (and (map? x)
       (util/non-blank-string? (:cmd/name x))
       (util/non-blank-string? (:cmd/doc x))
       (optional-valid? x :cmd/usage util/non-blank-string?)
       (optional-valid? x :cmd/run-fn ifn?)
       (optional-valid? x :cmd/owns-tty? boolean?)
       (optional-valid? x :cmd/internal? boolean?)
       (optional-valid? x :cmd/parent #(and (vector? %) (every? string? %)))
       (optional-valid? x :cmd/args #(and (vector? %) (every? arg? %)))
       (optional-valid? x :cmd/subcommands #(or (and (vector? %) (every? map? %)) (ifn? %)))
       (optional-valid? x :cmd/examples #(and (vector? %) (every? string? %)))
       (optional-valid? x :cmd/extra-sections #(or (sequential? %) (ifn? %)))))

(defn command
  "Build and validate a command map without realizing dynamic children."
  [descriptor]
  (if (command? descriptor)
    descriptor
    (throw (ex-info (str "Invalid command '" (:cmd/name descriptor) "'")
                    {:type :commandline/invalid-spec
                     :name (:cmd/name descriptor)
                     :explain {:valid false :value descriptor}}))))

(defn resolve-subcommands
  "Return the static vector of subcommands, calling the dynamic fn
   when needed. Returns `[]` when the command has no children."
  [cmd]
  (let [s (:cmd/subcommands cmd)]
    (cond (nil? s) []
          (vector? s) s
          (sequential? s) (vec s)
          (ifn? s) (vec (s))
          :else (throw (ex-info ":cmd/subcommands must be a vector or 0-arg fn"
                                {:got (type s) :command (:cmd/name cmd)})))))

(defonce channel-registry
  ;; Process-level atom: {:channel/id -> channel-map}. Public so tests
  ;; can `(reset! @#'channel-registry {})` between cases.
  (atom {}))

(defn register-channel!
  "Register a channel in the global registry.
   Idempotent on :channel/id - re-registering replaces the prior spec.
   Returns the validated channel."
  [spec]
  (let [ch (channel spec)]
    (swap! channel-registry assoc (:channel/id ch) ch)
    (tel/log! {:level :info
               :id ::register-channel
               :data {:channel (:channel/id ch) :cmd (:channel/cmd ch)}
               :msg (str "Channel '" (:channel/id ch) "' registered (cmd: " (:channel/cmd ch) ")")})
    ch))

(defn deregister-channel! [id] (swap! channel-registry dissoc id) nil)

(defn registered-channels
  "All globally registered channels as a vector."
  []
  (vec (vals @channel-registry)))

(defn channel-by-id
  "Lookup the channel by :channel/id. Returns nil when absent."
  [id]
  (get @channel-registry id))

(defn by-cmd
  "Lookup the channel whose :channel/cmd equals `cmd`. Returns nil
   when no channel claims that command."
  [cmd]
  (when (string? cmd)
    (some (fn [c]
            (when (= (:channel/cmd c) cmd) c))
          (vals @channel-registry))))

(defonce provider-registry
  ;; {:provider/id -> provider-map}
  (atom {}))

(defn register-provider!
  "Register a provider in the global registry. Idempotent on
   `:provider/id` - re-registering replaces the previous descriptor.
   Returns the validated provider."
  [spec]
  (let [p (provider spec)]
    (swap! provider-registry assoc (:provider/id p) p)
    (tel/log! {:level :info
               :id ::register-provider
               :data {:provider (:provider/id p) :label (:provider/label p)}
               :msg (str "Provider '" (:provider/id p) "' (" (:provider/label p) ") registered")})
    p))

(defn deregister-provider! [id] (swap! provider-registry dissoc id) nil)

(defn registered-providers [] (vec (vals @provider-registry)))

(defn provider-by-id
  "Lookup a provider by `:provider/id`. Returns nil when absent."
  [id]
  (get @provider-registry id))

(defonce command-registry
  ;; Vector preserves registration order, which then becomes the
  ;; default ordering in help output. De-duplication is by
  ;; [parent vector + command name].
  (atom []))

(defn- registry-key [c] [(or (:cmd/parent c) []) (:cmd/name c)])

(defn register-cmd!
  "Register a command in the global registry. Idempotent on
   `[:cmd/parent :cmd/name]` - re-registering replaces the prior
   entry, useful for REPL-driven development. Returns the validated
   command map."
  [spec]
  (let [c
        (command spec)

        k
        (registry-key c)

        cur
        @command-registry]

    (reset! command-registry (let [stripped (vec (remove #(= k (registry-key %)) cur))]
                               (conj stripped c)))
    (tel/log! {:level :info
               :id ::register-cmd
               :data {:name (:cmd/name c) :parent (:cmd/parent c)}
               :msg (str "Command '"
                         (str/join " " (conj (or (:cmd/parent c) []) (:cmd/name c)))
                         "' registered")})
    c))

(defn deregister-cmd!
  "Remove a registered command. `parent` defaults to `[]` (top-level)."
  ([nm] (deregister-cmd! [] nm))
  ([parent nm]
   (swap! command-registry (fn [cur]
                             (vec (remove #(= [parent nm] (registry-key %)) cur))))
   nil))

(defn registered-commands
  "Return all registered commands as a vector, in registration order."
  []
  @command-registry)

(defn registered-under
  "Return the vector of registered commands whose `:cmd/parent` equals
   `parent-path` (a vector of names). Use this from a parent command's
   `:cmd/subcommands` slot - typically as a 0-arg fn so newly
   registered children appear immediately:

       {:cmd/name \"extension\"
        :cmd/doc  \"Run an extension command.\"
        :cmd/subcommands #(registered-under [\"extension\"])}"
  [parent-path]
  (let [k (vec parent-path)]
    (vec (filter #(= k (or (:cmd/parent %) [])) @command-registry))))

;; CLI mounting - the `vis-agent channels` parent
;;
;; The channel registry feeds the `vis-agent channels <cmd>` subcommand
;; tree. Loading this namespace registers the parent itself; subcommand
;; resolution is dynamic so newly registered channels appear without
;; a restart.

(defn- channel->command
  "Adapt a `:channel/...`-keyed channel descriptor into a command map.
   Channels parse their own raw args so we forward the residual
   untouched and ignore the parsed map. Channel subcommands live on
   `:channel/subcommands`; the registered-under fallback stays for
   host-owned compatibility, but extension namespaces should not call
   `register-cmd!` directly."
  [c]
  {:cmd/name (:channel/cmd c)
   :cmd/doc (:channel/doc c)
   :cmd/usage (or (:channel/usage c) (str "vis-agent channels " (:channel/cmd c)))
   :cmd/owns-tty? (boolean (:channel/owns-tty? c))
   :cmd/subcommands #(let [s (:channel/subcommands c) direct
                           (cond (nil? s) []
                                 (ifn? s) (vec (s))
                                 (sequential? s) (vec s)
                                 :else [])] (into direct
                                                  (registered-under ["channels" (:channel/cmd c)])))
   :cmd/run-fn (fn [_parsed residual]
                 ((:channel/main-fn c) (vec residual)))})

(defn channel-subcommands
  "Compose subcommands for the `vis-agent channels` parent from TWO sources:

     1. Every entry in the channel registry (TUI, ...)
     2. Every commandline extension registered with
        `:cmd/parent [\"channels\"]` (escape hatch for non-channel
        adapters that still want to live under `vis-agent channels`)

   Source #1 wins on name collision - channels are first-class so a
   stray extension can't shadow a real channel name. Both sorted
   together so help output is alphabetic."
  []
  (let [from-channels
        (mapv channel->command (registered-channels))

        regd
        (registered-under ["channels"])

        names
        (set (map :cmd/name from-channels))]

    (vec (sort-by :cmd/name (concat from-channels (remove #(names (:cmd/name %)) regd))))))

(register-cmd! {:cmd/name "channels"
                :cmd/doc "Run a registered channel (TUI, ...)."
                :cmd/usage "vis-agent channels <name> [args...]"
                :cmd/subcommands #'channel-subcommands})
