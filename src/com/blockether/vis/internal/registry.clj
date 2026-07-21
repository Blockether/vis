(ns com.blockether.vis.internal.registry
  "Three global registries in one place: channels, providers, commands.

   Each is a small, self-contained piece - keyword id, a spec for the
   descriptor map, a process-level atom holding the registered entries,
   and a handful of fns for register / deregister / lookup. Putting
   them together means one file owns every \"this is the canonical
   shape of an extension contribution\" decision and one file owns
   every place an extension's contribution lands at runtime.

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
     channel-subcommands      compose `vis channels` subcommand vec
                              from the channel registry + any commands
                              registered with `:cmd/parent [\"channels\"]`.
                              Loading this ns also registers the
                              `vis channels` parent itself.

   Specs for keyword fields (`:channel/id`, `:provider/id`, `:cmd/name`,
   ..., plus the descriptor specs `::channel`, `::provider`, `::command`,
   `::arg`) live here too - the spec IS the registry's contract.

   Parsing / help rendering / dispatch utilities live in
   `com.blockether.vis.internal.commandline`. Classpath manifest
   scanning lives in `com.blockether.vis.internal.manifest`; the
   extension layer that wraps it (and re-exports `discover-extensions!`)
   lives in `com.blockether.vis.internal.extension`."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [taoensso.telemere :as tel]))

;; =============================================================================
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

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

;; =============================================================================
;; Channel descriptor - spec
;; =============================================================================

;; Stable identity key for the channel, e.g. :tui, :cli, :api.
;; Used as the session-soul `channel` column and as the dedup key
;; in the registry.
(s/def :channel/id keyword?)

;; Sub-command word the CLI matches. `vis channels tui ...` -> :tui channel.
;; Two registered channels MUST NOT share the same :channel/cmd.
;; There is no "default" channel - invoking `vis` with no command
;; prints help. Every channel is an explicit subcommand.
(s/def :channel/cmd non-blank-string?)

;; One-line description shown in `vis help`.
(s/def :channel/doc non-blank-string?)

;; Optional usage line shown in `vis help` (defaults to "vis <cmd>").
(s/def :channel/usage non-blank-string?)

;; When true, the channel takes over the controlling terminal
;; (Lanterna, ncurses, anything that writes to /dev/tty directly).
;; The CLI dispatcher reroutes stderr to ~/.vis/vis.log BEFORE any
;; channel code (or its transitive class loading) executes, so JVM
;; warnings and library prints never corrupt the screen.
;; Defaults to false.
(s/def :channel/owns-tty? boolean?)

;; Entry point. (fn [args-vec] -> any). `args-vec` are the CLI tokens
;; AFTER the channel command (so for `vis channels tui --foo bar` it is
;; `["--foo" "bar"]`).
;; Accepts any IFn (functions OR vars) so callers can pass `#'channel-main`
;; and benefit from REPL redefinition.
(s/def :channel/main-fn ifn?)

;; Optional. (fn [input opts] -> renderer-output) called by the channel's
;; emit chokepoint to convert the canonical content blocks into the channel-flavored
;; output. The TUI registers a :markdown walker; another channel
;; could register an :html walker. Output type is channel-defined.
(s/def :channel/messages-renderer-fn ifn?)

;; Channel-owned nested commands, e.g. `vis channels tui approve`.
;; This keeps channel subcommands inside the channel descriptor instead of
;; extension namespaces registering global command-registry entries directly.
(s/def :channel/subcommands
  (s/or :static (s/coll-of map? :kind vector?)
        :dynamic ifn?))

(s/def ::channel
  (s/keys :req [:channel/id :channel/cmd :channel/doc :channel/main-fn]
          :opt [:channel/usage :channel/owns-tty? :channel/subcommands
                :channel/messages-renderer-fn]))

(defn channel
  "Build and validate a channel descriptor map."
  [spec]
  (when-not (s/valid? ::channel spec)
    (throw (ex-info (str "Invalid channel '" (:channel/id spec)
                         "':\n" (with-out-str (s/explain ::channel spec)))
                    {:type :channel/invalid-spec
                     :id (:channel/id spec)
                     :explain (s/explain-data ::channel spec)})))
  spec)

;; =============================================================================
;; Provider descriptor - spec
;; =============================================================================

(s/def :provider/id keyword?)

(s/def :provider/label non-blank-string?)

;; All four runtime fns are optional individually so a minimal provider
;; (e.g. one that reads a static API key from env) doesn't need to
;; ship a no-op stub for every slot. Whoever calls them handles the
;; absent case (`(when-let [f (:provider/status-fn p)] (f))`).
(s/def :provider/status-fn ifn?)  ;; () -> {:authenticated? bool ...}

(s/def :provider/logout-fn ifn?)  ;; () -> nil  (clear creds)

(s/def :provider/detect-fn ifn?)  ;; () -> token-or-nil  (non-interactive)

(s/def :provider/auth-fn ifn?)  ;; (printer-fn) -> nil (interactive)

(s/def :provider/get-token-fn ifn?)  ;; () -> token-string/map  (resolve usable token)

(s/def :provider/refresh-token-fn ifn?)  ;; () -> token-string/map  (FORCE refresh ignoring local expiry; runtime 401 recovery)

(s/def :provider/limits-fn ifn?)  ;; () -> normalized limits envelope/map

(s/def :provider/enrich-models-fn ifn?) ;; (svar-provider router-opts) -> models-vec (resolve :context/:tool-call? at router-build, e.g. LM Studio native endpoint)

(s/def :provider/preset map?)         ;; extension-owned UI/runtime defaults: :base-url, :default-models, :api-style, :hidden?

(s/def :provider/on-selected-fn ifn?) ;; ({:provider :previous-provider :config :source}) -> nil

(s/def ::provider
  (s/and #(not (contains? % :provider/prompt-fn))
         (s/keys :req [:provider/id :provider/label]
                 :opt [:provider/status-fn :provider/logout-fn :provider/detect-fn :provider/auth-fn
                       :provider/get-token-fn :provider/refresh-token-fn :provider/limits-fn
                       :provider/enrich-models-fn :provider/preset :provider/on-selected-fn])))

(defn provider
  "Build and validate a provider descriptor."
  [spec]
  (when-not (s/valid? ::provider spec)
    (throw (ex-info (str "Invalid provider '" (:provider/id spec)
                         "':\n" (with-out-str (s/explain ::provider spec)))
                    {:type :provider/invalid-spec
                     :id (:provider/id spec)
                     :explain (s/explain-data ::provider spec)})))
  spec)

;; =============================================================================
;; Command descriptor - spec
;; =============================================================================

(s/def :cmd/name non-blank-string?)

(s/def :cmd/doc non-blank-string?)

(s/def :cmd/usage non-blank-string?)

(s/def :cmd/run-fn ifn?)

(s/def :cmd/owns-tty? boolean?)

(s/def :cmd/internal? boolean?)  ;; host-owned canonical command, not an extension contribution

;; Where in the command tree this command mounts. Vector of parent
;; command-names from the root, EXCLUDING the root itself and the
;; command's own `:cmd/name`. Examples:
;;   []                  - top-level (`vis <name>`)
;;   ["extension"]       - nested under `vis extension`
;;   ["channels"]        - nested under `vis channels`
;;   ["foo" "bar"]       - nested as `vis foo bar <name>`
;; Used by the CLI dispatcher's auto-mount via `registered-under`.
(s/def :cmd/parent (s/coll-of string? :kind vector?))

;; arg spec: {:name "model" :kind :flag|:positional :type :string|:int|:boolean
;;            :required true :doc "..."}
(s/def :cmd.arg/name non-blank-string?)

(s/def :cmd.arg/kind #{:flag :positional})

(s/def :cmd.arg/type #{:string :int :boolean :file})

(s/def :cmd.arg/required boolean?)

(s/def :cmd.arg/doc string?)

(s/def ::arg
  (s/keys :req-un [:cmd.arg/name :cmd.arg/kind]
          :opt-un [:cmd.arg/type :cmd.arg/required :cmd.arg/doc]))

(s/def :cmd/args (s/coll-of ::arg :kind vector?))

;; subcommands: vector OR 0-arg ifn returning vector
(s/def :cmd/subcommands
  (s/or :static (s/coll-of map? :kind vector?)
        :dynamic ifn?))

;; Optional vector of single-line example invocations shown in the
;; EXAMPLES help section.
(s/def :cmd/examples (s/coll-of string? :kind vector?))

;; Extra help sections appended after SUBCOMMANDS / EXTENSION COMMANDS.
;; Each entry is `{:title string :body string-or-0-arg-fn}`. The whole
;; value may also be a 0-arg fn returning a sequence of entries -- used
;; when the body requires runtime discovery (e.g. installed
;; extensions) that should not run at registration time.
(s/def :cmd/extra-sections
  (s/or :static sequential?
        :dynamic ifn?))

(s/def ::command
  (s/keys :req [:cmd/name :cmd/doc]
          :opt [:cmd/usage :cmd/args :cmd/run-fn :cmd/subcommands :cmd/owns-tty? :cmd/examples
                :cmd/parent :cmd/internal? :cmd/extra-sections]))

(defn command
  "Build and validate a command map. Children are NOT validated
   recursively; they're checked the first time `dispatch!` or
   `render-help` walks into them, which keeps dynamic subcommands
   from forcing their fn at build time."
  [spec]
  (when-not (s/valid? ::command spec)
    (throw (ex-info (str "Invalid command '" (:cmd/name spec)
                         "':\n" (with-out-str (s/explain ::command spec)))
                    {:type :commandline/invalid-spec
                     :name (:cmd/name spec)
                     :explain (s/explain-data ::command spec)})))
  spec)

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

;; =============================================================================
;; Channel registry
;; =============================================================================

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

;; =============================================================================
;; Provider registry
;; =============================================================================

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

;; =============================================================================
;; Command registry
;; =============================================================================

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
  (let
    [c
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

;; =============================================================================
;; CLI mounting - the `vis channels` parent
;;
;; The channel registry feeds the `vis channels <cmd>` subcommand
;; tree. Loading this namespace registers the parent itself; subcommand
;; resolution is dynamic so newly registered channels appear without
;; a restart.
;; =============================================================================

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
   :cmd/usage (or (:channel/usage c) (str "vis channels " (:channel/cmd c)))
   :cmd/owns-tty? (boolean (:channel/owns-tty? c))
   :cmd/subcommands #(let
                       [s
                        (:channel/subcommands c)

                        direct
                        (cond (nil? s) []
                              (ifn? s) (vec (s))
                              (sequential? s) (vec s)
                              :else [])]

                       (into direct (registered-under ["channels" (:channel/cmd c)])))
   :cmd/run-fn (fn [_parsed residual]
                 ((:channel/main-fn c) (vec residual)))})

(defn channel-subcommands
  "Compose subcommands for the `vis channels` parent from TWO sources:

     1. Every entry in the channel registry (TUI, ...)
     2. Every commandline extension registered with
        `:cmd/parent [\"channels\"]` (escape hatch for non-channel
        adapters that still want to live under `vis channels`)

   Source #1 wins on name collision - channels are first-class so a
   stray extension can't shadow a real channel name. Both sorted
   together so help output is alphabetic."
  []
  (let
    [from-channels
     (mapv channel->command (registered-channels))

     regd
     (registered-under ["channels"])

     names
     (set (map :cmd/name from-channels))]

    (vec (sort-by :cmd/name (concat from-channels (remove #(names (:cmd/name %)) regd))))))

(register-cmd! {:cmd/name "channels"
                :cmd/doc "Run a registered channel (TUI, ...)."
                :cmd/usage "vis channels <name> [args...]"
                :cmd/subcommands #'channel-subcommands})
