(ns com.blockether.vis.channel
  "Channel registry — dynamic, plug-in CLI front-ends.

   A *channel* is a vis adapter (TUI, Telegram bot, web server, …)
   discovered and dispatched at runtime. The CLI core in vis-core does
   NOT import any concrete channel namespace — it iterates this global
   registry to dispatch sub-commands. Same pattern as `extension`,
   different concern: extensions add tools to the SCI sandbox, channels
   add user-facing front-ends to the binary.

   Two ways to register:

   1. **Global registry** — `(register-global! spec)` at namespace load
      time. The CLI calls `(discover-channels!)` once at boot, which
      requires every `META-INF/vis/channels.edn` namespace, triggering
      their `register-global!` calls.

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

   Auto-discovery: ship `META-INF/vis/channels.edn` in the jar's
   resources/ listing every namespace that calls `register-global!`:

     [com.blockether.vis.channels.tui.screen
      com.blockether.vis.channels.telegram.bot]

   `discover-channels!` scans the classpath for that resource and
   `require`s each namespace exactly once."
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
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
;; =============================================================================

(def ^:private CHANNELS_RESOURCE "META-INF/vis/channels.edn")

(defn discover-channels!
  "Scan the classpath for every `META-INF/vis/channels.edn` resource.

   Each file is an EDN vector of namespace symbols, e.g.:

     [com.blockether.vis.channels.tui.screen
      com.blockether.vis.channels.telegram.bot]

   Every listed namespace is `require`d, which triggers its
   `register-global!` call. Already-loaded namespaces are silently
   skipped. Returns the count of channels added to the registry by
   this call (for diagnostics)."
  []
  (let [urls   (try
                 (enumeration-seq
                   (.getResources
                     (.getContextClassLoader (Thread/currentThread))
                     CHANNELS_RESOURCE))
                 (catch Exception _ nil))
        before (set (keys @global-registry))
        loaded (atom 0)]
    (doseq [^java.net.URL url urls]
      (try
        (let [ns-syms (edn/read-string (slurp url))]
          (when (sequential? ns-syms)
            (doseq [ns-sym ns-syms]
              (when (symbol? ns-sym)
                (try
                  (require ns-sym)
                  (tel/log! {:level :info :id ::discover
                             :data  {:channel-ns ns-sym :source (str url)}
                             :msg   (str "Auto-discovered channel ns '" ns-sym
                                      "' from " url)})
                  (catch Throwable t
                    (tel/log! {:level :error :id ::discover-failed
                               :data  {:channel-ns ns-sym :source (str url)
                                       :class (.getName (class t))
                                       :message (ex-message t)}
                               :msg   (str "Failed to load channel ns '"
                                        ns-sym "': " (ex-message t))})))))))
        (catch Throwable t
          (tel/log! {:level :error :id ::discover-parse-failed
                     :data  {:source (str url) :message (ex-message t)}
                     :msg   (str "Failed to parse " url ": " (ex-message t))}))))
    (- (count @global-registry) (count before))))
