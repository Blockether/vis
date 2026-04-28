(ns com.blockether.vis.channel
  "Channel registry — dynamic, extension-supplied CLI front-ends.

   A *channel* is a vis adapter (TUI, Telegram bot, …) discovered and
   dispatched at runtime. The CLI core in vis-core does
   NOT import any concrete channel namespace — it iterates this global
   registry to dispatch sub-commands. Same pattern as `extension`,
   different concern: extensions add tools to the SCI sandbox, channels
   add user-facing front-ends to the binary.

   Two ways to register:

   1. **Global registry** — `(register-global! spec)` at namespace load
      time. The CLI calls `(ext/discover-extensions!)` once at boot,
      which requires every namespace listed in any `META-INF/vis.edn`
      on the classpath, triggering their `register-global!` calls.

   2. **Programmatic** — anyone holding a channel spec can call
      `(register-global! spec)` directly (handy for tests / embedded
      uses).

   Example registration (lives in the channel package, not vis-core):

     (ns com.blockether.vis.channels.tui.screen
       (:require [com.blockether.vis.channel :as channel]))

     (channel/register-global!
       {:channel/id        :tui
        :channel/cmd       \"tui\"
        :channel/doc       \"Interactive terminal UI.\"
        :channel/usage     \"vis tui [--resume]\"
        :channel/owns-tty? true
        :channel/main-fn   (fn [args] (run-chat! (parse-args args)))})

   Auto-discovery: ship the unified `META-INF/vis.edn` in the jar's
   resources/ listing every namespace that calls `register-global!`
   (channels, extensions, commands, providers, persistance entries —
   the loader is type-agnostic):

     [com.blockether.vis.channels.tui.screen
      com.blockether.vis.channels.telegram.bot]

   `com.blockether.vis.extension/discover-extensions!` scans the
   classpath for every such resource and `require`s each namespace
   exactly once."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.commandline.base :as cmd]
            [taoensso.telemere :as tel]))

;; =============================================================================
;; Spec
;; =============================================================================

(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

;; Stable identity key for the channel, e.g. :tui, :telegram, :web.
;; Used as the conversation-soul `channel` column and as the dedup key
;; in the registry.
(s/def :channel/id keyword?)

;; Sub-command word the CLI matches. `vis tui …` → :tui channel.
;; Two registered channels MUST NOT share the same :channel/cmd.
;; There is no "default" channel — invoking `vis` with no command
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

;; Entry point. (fn [args-vec] → any). `args-vec` are the CLI tokens
;; AFTER the channel command (so for `vis telegram --foo bar` it is
;; `["--foo" "bar"]`).
;; Accepts any IFn (functions OR vars) so callers can pass `#'channel-main`
;; and benefit from REPL redefinition.
(s/def :channel/main-fn ifn?)

(s/def ::channel
  (s/keys :req [:channel/id :channel/cmd :channel/doc :channel/main-fn]
    :opt [:channel/usage :channel/owns-tty?]))

(defn channel
  "Build and validate a channel descriptor map."
  [spec]
  (when-not (s/valid? ::channel spec)
    (throw (ex-info (str "Invalid channel '" (:channel/id spec) "':\n"
                      (with-out-str (s/explain ::channel spec)))
             {:type    :channel/invalid-spec
              :id      (:channel/id spec)
              :explain (s/explain-data ::channel spec)})))
  spec)

;; =============================================================================
;; Global registry
;; =============================================================================

(defonce ^:private global-registry
  ;; Process-level atom: {:channel/id → channel-map}.
  (atom {}))

(defn register-global!
  "Register a channel in the global registry.
   Idempotent on :channel/id — re-registering replaces the prior spec.
   Returns the validated channel."
  [spec]
  (let [ch (channel spec)]
    (swap! global-registry assoc (:channel/id ch) ch)
    (tel/log! {:level :info :id ::register-global
               :data  {:channel (:channel/id ch) :cmd (:channel/cmd ch)}
               :msg   (str "Channel '" (:channel/id ch)
                        "' registered (cmd: " (:channel/cmd ch) ")")})
    ch))

(defn deregister-global!
  "Remove a channel from the registry by :channel/id."
  [id]
  (swap! global-registry dissoc id)
  nil)

(defn registered-channels
  "All globally registered channels as a vector, registration order
   approximated by `vals` of the underlying map."
  []
  (vec (vals @global-registry)))

(defn by-cmd
  "Lookup the channel whose :channel/cmd equals `cmd`. Returns nil
   when no channel claims that command."
  [cmd]
  (when (string? cmd)
    (some (fn [c] (when (= (:channel/cmd c) cmd) c))
      (vals @global-registry))))

(defn by-id
  "Lookup the channel by :channel/id. Returns nil when absent."
  [id]
  (get @global-registry id))

;; =============================================================================
;; Classpath auto-discovery
;;
;; There is no channel-specific scanner. The single source of truth
;; is `com.blockether.vis.extension/discover-extensions!` (in this same
;; jar), which scans every `META-INF/vis.edn` on the classpath and
;; `require`s the namespaces listed inside. Any of those namespaces
;; that calls `(channel/register-global! ...)` lands in this registry
;; as a side effect. No further code needed here.
;; =============================================================================

;; =============================================================================
;; CLI bridge — the `vis channel` parent
;;
;; Loading this namespace registers a top-level commandline parent
;; whose subcommands are computed lazily from the channel registry on
;; every help/dispatch walk. New channels show up immediately, no
;; restart required. The bridge lives here (not in commandline.base)
;; because that's where the registry is — commandline.base never
;; depends on the channel concept.
;; =============================================================================

(defn- channel->command
  "Adapt a `:channel/…`-keyed channel descriptor into a commandline.base
   command map. Channels parse their own raw args so we forward the
   residual untouched and ignore the parsed map."
  [c]
  {:cmd/name      (:channel/cmd c)
   :cmd/doc       (:channel/doc c)
   :cmd/usage     (or (:channel/usage c)
                    (str "vis channel " (:channel/cmd c)))
   :cmd/owns-tty? (boolean (:channel/owns-tty? c))
   :cmd/run-fn    (fn [_parsed residual]
                    ((:channel/main-fn c) (vec residual)))})

(defn channel-subcommands
  "Compose subcommands for the `vis channels` parent from TWO sources:

     1. Every entry in the channel registry (TUI, Telegram, web, …)
     2. Every commandline extension registered with
        `:cmd/parent [\"channels\"]` (escape hatch for non-channel
        adapters that still want to live under `vis channels`)

   Source #1 wins on name collision — channels are first-class so a
   stray extension can't shadow a real channel name. Both sorted
   together so help output is alphabetic."
  []
  (let [from-channels (mapv channel->command (registered-channels))
        regd          (cmd/registered-under ["channels"])
        names         (set (map :cmd/name from-channels))]
    (vec (sort-by :cmd/name
           (concat from-channels
             (remove #(names (:cmd/name %)) regd))))))

(cmd/register-global!
  {:cmd/name        "channels"
   :cmd/doc         "Run a registered channel (TUI, Telegram, …)."
   :cmd/usage       "vis channels <name> [args…]"
   :cmd/subcommands #'channel-subcommands})
