(ns com.blockether.vis.internal.sdk
  "vis SDK — foundation utilities and the public surface that every
   channel, extension, and embedded consumer reaches into.

   What lives here:

     - Error formatting (`error-message`, `format-error`, `error->user-message`)
     - Cancellation primitive (`cancellation-token`, `cancel!`, `cancellation?`)
     - Classpath discovery (`discover-extensions!`)
     - CLI command spec + registry (`command`, `register-cmd!`, `dispatch!`)
     - Channel registry (`channel`, `register-channel!`, `channel-by-id`)
     - Provider registry (`provider`, `register-provider!`, `provider-by-id`)
     - Storage facade (`create-store-connection`, `register-backend!`, `log!`,
       every `store-*` / `db-*` delegating fn)
     - Extension specification + global registry (`extension`, `symbol`,
       `value`, `register-extension!`, `extension-doc`, `extension-readme`)
     - Configuration / paths / logging bootstrap (`init!`, `tty-in`,
       `original-stdout`, `load-config`, `save-config!`, `resolve-db-spec`)
     - Provider state (`set-provider!`, `current-config`, `active-provider`,
       `provider-presets`, `display-label`)
     - Streaming progress tracker (`make-progress-tracker`)
     - Format helpers (`format-date`, `format-duration`, `format-clojure`)

   Iteration entry points (`send!`, `create!`, `by-channel`, `query!`,
   `create-environment`, …) live in `com.blockether.vis.internal.loop`. Channels
   that need to drive a conversation require `loop` directly.

   The SCI sandbox machinery lives in `com.blockether.vis.internal.env` — that
   namespace is internal; nothing outside `loop` should import it."
  (:refer-clojure :exclude [symbol])
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure+.error]
   [com.blockether.vis.internal.cancellation :as cancellation]
   [com.blockether.vis.internal.commandline :as commandline]
   [com.blockether.vis.internal.registry :as registry]
   [com.blockether.vis.internal.error :as error]
   [com.blockether.vis.internal.format :as format]
   [com.blockether.vis.internal.progress :as progress]
   [taoensso.telemere :as tel])
  (:import
   [java.io FileInputStream FileOutputStream]
   [java.time Instant]
   [java.util Date UUID]))

;; ===========================================================================
;; Re-exports from leaf modules
;;
;; Several self-contained concerns now live in dedicated namespaces so
;; channels and extensions can require them without dragging in the
;; rest of the SDK. The SDK facade re-exports the public names so
;; existing callers (`sdk/cancellation-token`, `sdk/format-date`,
;; …) keep working unchanged.
;;
;; Direct requires for new code:
;;   com.blockether.vis.internal.error        — error formatting
;;   com.blockether.vis.internal.cancellation — cooperative + hard cancellation
;;   com.blockether.vis.internal.format       — date / duration / Clojure source
;;   com.blockether.vis.internal.progress     — streaming chunk tracker
;; ===========================================================================

;; ---- Error formatting -------------------------------------------------------
(def error-message error/error-message)
(def format-error  error/format-error)
(def final-answer-code-error-message error/final-answer-code-error-message)

;; ---- Cancellation -----------------------------------------------------------
(def cancellation-token        cancellation/cancellation-token)
(def cancellation-atom         cancellation/cancellation-atom)
(def cancellation-set-future!  cancellation/cancellation-set-future!)
(def cancel!                   cancellation/cancel!)
(def cancelled?                cancellation/cancelled?)
(def cancellation?             cancellation/cancellation?)
;; =============================================================================
;; Classpath discovery
;; =============================================================================

(def EXTENSIONS_RESOURCE
  "Classpath path of the per-jar discovery manifest. Resource name
   is preserved across the SDK rename for on-disk back-compat with
   already-shipped extension jars; see the README for context."
  "META-INF/vis-extension/vis.edn")

(defn- valid-link? [link]
  (and (map? link)
    (or (and (symbol? (:to-id link)) (string? (:to-doc link)))
      (string? (:to-doc link))
      (string? (:url link))
      (string? (:file link)))))

(defn- normalize-doc-descriptor
  "Validate one `[doc-name descriptor]` pair from a vis.edn `:docs`
   map. Returns the descriptor with empty defaults filled in, or
   `nil` when the entry is malformed (missing :abstract, missing
   :content, etc.). Logs the rejection reason at `:warn`."
  [doc-name descriptor]
  (cond
    (not (string? doc-name))
    (do (tel/log! {:level :warn :id ::doc-bad-name
                   :data {:doc-name doc-name}
                   :msg  (str "Doc name must be a string, got " (pr-str doc-name))})
      nil)

    (not (map? descriptor))
    (do (tel/log! {:level :warn :id ::doc-bad-shape
                   :data {:doc-name doc-name :type (some-> descriptor class .getName)}
                   :msg  (str "Doc descriptor must be a map: " doc-name)})
      nil)

    (not (string? (:abstract descriptor)))
    (do (tel/log! {:level :warn :id ::doc-missing-abstract
                   :data {:doc-name doc-name}
                   :msg  (str "Doc " doc-name " missing required :abstract string")})
      nil)

    (not (string? (:content descriptor)))
    (do (tel/log! {:level :warn :id ::doc-missing-content
                   :data {:doc-name doc-name}
                   :msg  (str "Doc " doc-name " missing required :content string")})
      nil)

    :else
    {:created-at (:created-at descriptor)
     :abstract   (:abstract descriptor)
     :content    (:content descriptor)
     :links      (vec (filter valid-link? (:links descriptor)))
     :reflinks   []}))

(defn- normalize-vis-edn
  "Coerce a parsed `vis.edn` payload into the canonical map shape
   `{<id-sym> {:nses [<ns-sym> ...] :docs {<doc-name> <descriptor>}}}`.
   Drops malformed entries silently; returns `{}` for unrecognized
   shapes."
  [parsed]
  (when (map? parsed)
    (into {}
      (keep (fn [[id entry]]
              (when (and (symbol? id) (map? entry))
                (let [nses (vec (filter symbol? (:nses entry)))
                      docs (when (map? (:docs entry))
                             (into {}
                               (keep (fn [[doc-name descriptor]]
                                       (when-let [norm (normalize-doc-descriptor doc-name descriptor)]
                                         [doc-name norm])))
                               (:docs entry)))]
                  (when (seq nses)
                    [id {:nses nses :docs (or docs {})}])))))
      parsed)))

(defn- merge-manifest-entry
  "Merge two parsed manifest entries for the same id. `:nses` are
   deduped (existing order preserved); `:docs` is a map merge with
   later entries winning per name."
  [existing entry]
  {:nses (vec (distinct (concat (:nses existing) (:nses entry))))
   :docs (merge (or (:docs existing) {}) (or (:docs entry) {}))})

(defonce ^:private cached-manifests (atom nil))
(defonce ^:private discovered? (atom false))

(defn- scan!
  "One pass: read every vis.edn URL, merge per id, require every
   declared namespace exactly once across all URLs. Returns the
   merged manifest map."
  []
  (let [urls   (try
                 (enumeration-seq
                   (.getResources
                     (.getContextClassLoader (Thread/currentThread))
                     EXTENSIONS_RESOURCE))
                 (catch Exception _ nil))
        merged (atom {})
        seen   (atom #{})]
    (doseq [^java.net.URL url urls]
      (try
        (let [content    (slurp url)
              parsed     (edn/read-string {:readers {} :default (fn [_ form] form)} content)
              normalized (normalize-vis-edn parsed)]
          (doseq [[id entry] normalized]
            (swap! merged update id merge-manifest-entry entry)
            (doseq [ns-sym (:nses entry)]
              (when (not (@seen ns-sym))
                (swap! seen conj ns-sym)
                (try
                  (require ns-sym)
                  (tel/log! {:level :info :id ::discover-extension
                             :data  {:extension-id id
                                     :extension-ns ns-sym
                                     :source (str url)}
                             :msg   (str "Auto-discovered extension ns '"
                                      ns-sym "' (id " id ") from " url)})
                  (catch Throwable t
                    (tel/log! {:level :error :id ::discover-extension-failed
                               :data  {:extension-id id
                                       :extension-ns ns-sym
                                       :source (str url)
                                       :class (.getName (class t))
                                       :message (ex-message t)}
                               :msg   (str "Failed to load extension ns '"
                                        ns-sym "': " (ex-message t))}))))))
          (when (empty? normalized)
            (tel/log! {:level :warn :id ::discover-extension-empty
                       :data {:source (str url)}
                       :msg  (str url " parsed but declared no extensions")})))
        (catch Throwable t
          (tel/log! {:level :error :id ::discover-extension-parse-failed
                     :data  {:source (str url) :message (ex-message t)}
                     :msg   (str "Failed to parse " url ": " (ex-message t))}))))
    @merged))

(defn- scan-extensions!
  "Private helper: idempotent classpath scan + namespace requires.
   Returns the merged parsed manifests as `{<id-sym> {:nses [...] :docs {...}}}`.
   Wrapped by the public `discover-extensions!` below, which adds the
   docs-registry merge on top. Storage and other internal callers that
   need ONLY the require side effect should call this private helper
   to skip the docs work."
  []
  (if @discovered?
    @cached-manifests
    (let [manifests (scan!)]
      (reset! cached-manifests manifests)
      (reset! discovered? true)
      manifests)))

(defn rediscover!
  "Force a fresh classpath scan, discarding the cached manifests.
   Test/REPL utility — production code should use the idempotent
   `discover-extensions!` instead."
  []
  (reset! discovered? false)
  (reset! cached-manifests nil)
  (scan-extensions!))

;; =============================================================================
;; Re-exports from com.blockether.vis.internal.commandline
;;
;; The CLI command spec, parser, dispatcher and global registry now
;; live in `com.blockether.vis.internal.commandline`. The SDK facade keeps
;; the public names so existing callers (`sdk/command`,
;; `sdk/dispatch!`, `sdk/register-cmd!`, `sdk/render-command`,
;; `sdk/render-tree`, `sdk/parse-args`, `sdk/validate-args`,
;; `sdk/find-leaf`, `sdk/find-named`, `sdk/registered-commands`,
;; `sdk/registered-under`) keep working unchanged. New code that
;; only needs the command-tree primitives should require
;; `com.blockether.vis.internal.commandline` directly.
;; =============================================================================

(def command             registry/command)
(def resolve-subcommands registry/resolve-subcommands)
(def find-leaf           commandline/find-leaf)
(def find-named          commandline/find-named)
(def parse-args          commandline/parse-args)
(def validate-args       commandline/validate-args)
(def pad-right           commandline/pad-right)
(def pad-left            commandline/pad-left)
(def render-command      commandline/render-command)
(def render-tree         commandline/render-tree)
(def dispatch!           commandline/dispatch!)
(def register-cmd!       registry/register-cmd!)
(def deregister-cmd!     registry/deregister-cmd!)
(def registered-commands registry/registered-commands)
(def registered-under    registry/registered-under)

;; Tiny shared predicate used by the channel/provider/extension specs
;; below — was previously private inside the CLI section that moved
;; to `com.blockether.vis.internal.commandline`. Kept here as a private helper
;; rather than re-exported because callers should reach for richer
;; spec primitives, not this one.
(defn- non-blank-string? [x] (and (string? x) (not (str/blank? x))))

;; =============================================================================
;; Channel registry
;; =============================================================================

;; =============================================================================
;; Spec
;; =============================================================================

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

(defonce ^:private channel-registry
  ;; Process-level atom: {:channel/id → channel-map}.
  (atom {}))

(defn register-channel!
  "Register a channel in the global registry.
   Idempotent on :channel/id — re-registering replaces the prior spec.
   Returns the validated channel."
  [spec]
  (let [ch (channel spec)]
    (swap! channel-registry assoc (:channel/id ch) ch)
    (tel/log! {:level :info :id ::register-global
               :data  {:channel (:channel/id ch) :cmd (:channel/cmd ch)}
               :msg   (str "Channel '" (:channel/id ch)
                        "' registered (cmd: " (:channel/cmd ch) ")")})
    ch))

(defn deregister-channel! [id]
  (swap! channel-registry dissoc id)
  nil)

(defn registered-channels
  "All globally registered channels as a vector, registration order
   approximated by `vals` of the underlying map."
  []
  (vec (vals @channel-registry)))

(defn by-cmd
  "Lookup the channel whose :channel/cmd equals `cmd`. Returns nil
   when no channel claims that command."
  [cmd]
  (when (string? cmd)
    (some (fn [c] (when (= (:channel/cmd c) cmd) c))
      (vals @channel-registry))))

(defn channel-by-id
  "Lookup the channel by :channel/id. Returns nil when absent."
  [id]
  (get @channel-registry id))

;; =============================================================================
;; Classpath auto-discovery
;;
;; There is no channel-specific scanner. The single source of truth
;; is `com.blockether.vis.core/discover-extensions!` (in this same
;; jar), which scans every `META-INF/vis-extension/vis.edn` on the classpath and
;; `require`s the namespaces listed inside. Any of those namespaces
;; that calls `(register-channel! ...)` lands in this registry
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
        regd          (registered-under ["channels"])
        names         (set (map :cmd/name from-channels))]
    (vec (sort-by :cmd/name
           (concat from-channels
             (remove #(names (:cmd/name %)) regd))))))

(register-cmd!
  {:cmd/name        "channels"
   :cmd/doc         "Run a registered channel (TUI, Telegram, …)."
   :cmd/usage       "vis channels <name> [args…]"
   :cmd/subcommands #'channel-subcommands})

;; =============================================================================
;; Provider registry
;; =============================================================================

;; =============================================================================
;; Spec
;; =============================================================================

(s/def :provider/id    keyword?)
(s/def :provider/label non-blank-string?)

;; All four runtime fns are optional individually so a minimal provider
;; (e.g. one that reads a static API key from env) doesn't need to
;; ship a no-op stub for every slot. Whoever calls them handles the
;; absent case (`(when-let [f (:provider/status-fn p)] (f))`).
(s/def :provider/status-fn    ifn?)  ;; () -> {:authenticated? bool …}
(s/def :provider/logout-fn    ifn?)  ;; () -> nil  (clear creds)
(s/def :provider/detect-fn    ifn?)  ;; () -> token-or-nil  (non-interactive)
(s/def :provider/auth-fn      ifn?)  ;; (printer-fn) -> nil (interactive)
(s/def :provider/get-token-fn ifn?)  ;; () -> token-string  (resolve usable token)

(s/def ::provider
  (s/keys :req [:provider/id :provider/label]
    :opt [:provider/status-fn :provider/logout-fn :provider/detect-fn
          :provider/auth-fn :provider/get-token-fn]))

(defn provider
  "Build and validate a provider descriptor."
  [spec]
  (when-not (s/valid? ::provider spec)
    (throw (ex-info (str "Invalid provider '" (:provider/id spec) "':\n"
                      (with-out-str (s/explain ::provider spec)))
             {:type    :provider/invalid-spec
              :id      (:provider/id spec)
              :explain (s/explain-data ::provider spec)})))
  spec)

;; =============================================================================
;; Global registry
;; =============================================================================

(defonce ^:private provider-registry
  ;; {:provider/id → provider-map}
  (atom {}))

(defn register-provider!
  "Register a provider in the global registry. Idempotent on
   `:provider/id` — re-registering replaces the previous descriptor.
   Returns the validated provider."
  [spec]
  (let [p (provider spec)]
    (swap! provider-registry assoc (:provider/id p) p)
    (tel/log! {:level :info :id ::register-global
               :data  {:provider (:provider/id p)
                       :label    (:provider/label p)}
               :msg   (str "Provider '" (:provider/id p)
                        "' (" (:provider/label p) ") registered")})
    p))

(defn deregister-provider! [id]
  (swap! provider-registry dissoc id) nil)

(defn registered-providers []
  (vec (vals @provider-registry)))

(defn provider-by-id
  "Lookup a provider by `:provider/id`. Returns nil when absent."
  [id] (get @provider-registry id))

;; =============================================================================
;; Auto-discovery
;;
;; There is no provider-specific scanner. The single source of truth
;; is `com.blockether.vis.core/discover-extensions!`, which scans
;; every `META-INF/vis-extension/vis.edn` on the classpath and `require`s the
;; namespaces listed inside. Any of those namespaces that calls
;; `(register-provider! ...)` lands in this registry as a side
;; effect. The CLI dispatcher (`commandline.main`) invokes the
;; loader once at boot; provider-aware code paths that bypass the CLI
;; (SDK callers, tests) can `requiring-resolve` it themselves.
;; =============================================================================

;; =============================================================================
;; Storage base helpers
;; =============================================================================

(defn ds [db-info] (:datasource db-info))

(defn now-ms ^long [] (System/currentTimeMillis))

(defn ->id [v]
  (cond
    (nil? v) nil
    (uuid? v) (str v)
    (string? v) v
    :else (str v)))

(defn ->uuid ^UUID [v]
  (cond
    (nil? v) nil
    (uuid? v) v
    (string? v) (try (UUID/fromString v) (catch IllegalArgumentException _ nil))
    :else nil))

(defn ->ref
  "Normalize an entity reference to a string ID for SQL.
   Accepts: UUID, string, or nil. Returns string or nil.

   The ONLY way to extract a SQL-ready string from an entity
   reference -- pass the plain UUID or string directly."
  [v]
  (cond
    (nil? v)    nil
    (uuid? v)   (str v)
    (string? v) v
    :else       (str v)))

(defn ->kw
  "Keyword/string → TEXT, stripping the leading colon. Nil → nil."
  [v]
  (cond
    (nil? v) nil
    (keyword? v) (subs (str v) 1)
    :else (str v)))

(defn ->kw-back [v]
  (when (and v (not= "" v))
    (keyword v)))

(defn ->epoch-ms [v]
  (cond
    (nil? v) nil
    (instance? Date v) (.getTime ^Date v)
    (instance? Instant v) (.toEpochMilli ^Instant v)
    (number? v) (long v)
    :else nil))

(defn ->date ^Date [v]
  (when v (Date. (long v))))

;; =============================================================================
;; Storage facade
;; =============================================================================
;;
;; The Flyway-backed migration runner used to live here. It moved into
;; the SQLite extension along with the `flyway-core` dep — it had
;; exactly one caller (`install-schema!` in vis-persistance-sqlite),
;; and Flyway is a dialect-aware concern. Other backends ship their
;; own migration entry point. See
;; `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/migration.clj`.

;; =============================================================================
;; Backend registry
;; =============================================================================

(defonce ^:private backends
  ;; {:sqlite {:ns 'com.blockether.vis.ext.persistance-sqlite.core}}
  (atom {}))

(defn register-backend!
  "Register a persistence backend implementation.

   `id`     — keyword identity, e.g. `:sqlite`.
   `ns-sym` — fully qualified namespace symbol that defines the backend
              functions (`open-store`, `close-store`, `log!`, every
              `store-*`/`db-*` fn used by this facade). Vars are
              resolved lazily via `ns-resolve` so REPL redefinition
              just works.

   Idempotent on `id`. Returns `id`."
  [id ns-sym]
  (when-not (keyword? id)
    (throw (ex-info "Backend id must be a keyword" {:id id})))
  (when-not (symbol? ns-sym)
    (throw (ex-info "Backend ns-sym must be a symbol" {:ns-sym ns-sym})))
  (swap! backends assoc id {:ns ns-sym})
  id)

(defn deregister-backend!
  [id]
  (swap! backends dissoc id)
  nil)

(defn registered-backends
  "Map of registered backends keyed by id."
  []
  @backends)

;; ----------------------------------------------------------------------------
;; Auto-discovery
;;
;; There is no backend-specific scanner. The single source of truth is
;; `discover-extensions!` (defined later in this file), which scans every
;; `META-INF/vis-extension/vis.edn` on the classpath and `require`s the
;; namespaces listed inside. Any of those namespaces that calls
;; `(register-backend! ...)` lands in this backend registry as a side
;; effect. Because `discover-extensions!` lives in the same namespace as
;; this facade, no `requiring-resolve` cycle workaround is needed: the
;; storage facade calls it directly through the private `scan-extensions!`
;; helper (also in this ns) on the first `create-store-connection`.
;; ----------------------------------------------------------------------------

(defn- pick-backend-id
  "Decide which backend handles this call. Honors an explicit
   `:backend` key on the spec/store; otherwise falls back to the
   single registered backend; otherwise throws."
  [db-spec-or-store]
  (or (when (map? db-spec-or-store) (:backend db-spec-or-store))
    (when (= 1 (count @backends)) (first (keys @backends)))
    (throw
      (ex-info
        (str "No persistence backend selected. "
          (if (empty? @backends)
            "No backends registered. Did you forget to require "
            "Multiple backends registered, pass {:backend …} in db-spec. ")
          (when (empty? @backends)
            "`com.blockether.vis.ext.persistance-sqlite.core`?"))
        {:registered (vec (keys @backends))}))))

(defn- resolve-impl
  "Resolve the var implementing `fn-name` on the chosen backend.
   Throws a useful error when the backend is missing the fn."
  [db-spec-or-store fn-name]
  (let [bid     (pick-backend-id db-spec-or-store)
        ns-sym  (get-in @backends [bid :ns])
        _       (when-not ns-sym
                  (throw (ex-info (str "Backend " bid " not registered")
                           {:backend bid :registered (vec (keys @backends))})))
        v       (ns-resolve ns-sym fn-name)]
    (when-not v
      (throw (ex-info (str "Backend " bid " (" ns-sym ") does not implement '" fn-name "'")
               {:backend bid :ns ns-sym :fn fn-name})))
    v))

(defn- normalize-spec
  "Reshape explicit-sqlite nested forms into the canonical shape
   backends accept. `:memory` is the canonical shorthand for the
   ephemeral in-process DB."
  [db-spec]
  (cond
    (and (map? db-spec)
      (= :sqlite (:backend db-spec)))
    (cond
      (:datasource db-spec) {:datasource (:datasource db-spec) :backend :sqlite}
      (:conn db-spec)       {:conn (:conn db-spec) :backend :sqlite}
      (:path db-spec)       (assoc {:backend :sqlite} :path (:path db-spec))
      :else                 db-spec)
    :else db-spec))

;; =============================================================================
;; Connection lifecycle
;; =============================================================================

(declare discover-extensions!)

(defn create-store-connection
  "Open a persistence connection from `db-spec`.

   Common spec forms:
     nil              — no DB (returns nil)
     :memory          — in-memory ephemeral store (backend-defined)
     \"path/to.db\"   — file-backed store (backend-defined)
     {:backend :sqlite :path …}     — explicit backend selection
     {:backend :sqlite :datasource ds} — caller-owned DataSource

   With a single registered backend, omitting `:backend` works and the
   facade tags the returned store map with the chosen backend so all
   subsequent facade calls dispatch correctly."
  [db-spec]
  (discover-extensions!)
  (let [normalized (normalize-spec db-spec)
        bid        (pick-backend-id (if (map? normalized)
                                      normalized
                                      {:backend (pick-backend-id {})}))
        f          @(resolve-impl {:backend bid} 'open-store)
        store      (f normalized)]
    (cond
      (nil? store) nil
      (map? store) (assoc store :backend bid)
      :else        store)))

(defn dispose-store-connection! [store]
  (when store
    (let [f @(resolve-impl store 'close-store)]
      (f store))))

;; =============================================================================
;; Delegated API — every fn delegates to the selected backend's var of
;; the same name. Each defn is intentionally one-line to keep the
;; surface obvious; add a new entry here only after the matching fn
;; lands in at least one backend.
;; =============================================================================

(defmacro ^:private defdelegate
  "Define a facade fn whose body resolves the matching backend var
   (using the first arg as the dispatch value) and applies it to
   the original args."
  [sym arglist]
  (let [bsym (gensym "backend-fn")]
    `(defn ~sym ~arglist
       (let [~bsym @(resolve-impl ~(first arglist) (quote ~sym))]
         (~bsym ~@arglist)))))

;; --- Logging ---
(defdelegate log! [db-info opts])

;; --- Conversation lifecycle ---
(defdelegate store-conversation!              [db-info opts])
(defdelegate db-get-conversation              [db-info ref])
(defdelegate db-resolve-conversation-id       [db-info sel])
(defdelegate db-list-conversations            [db-info channel])
(defdelegate db-find-conversation-by-external [db-info channel ext-id])
(defdelegate db-update-conversation-title!    [db-info ref title])
(defdelegate delete-conversation-tree!        [db-info id])
(defdelegate fork-conversation!               [db-info conv-id opts])

;; --- Query lifecycle ---
(defdelegate store-query!                  [db-info opts])
(defdelegate update-query!                 [db-info query-id opts])
(defdelegate db-list-queries-by-status     [db-info status])
(defdelegate db-list-conversation-queries  [db-info conv-ref])
(defdelegate retry-query!                  [db-info query-soul-id opts])

;; --- Iteration lifecycle ---
(defn store-iteration!
  "Same delegating shape as the macro-defined fns, but with input
   validation kept here so every backend gets the same precondition
   guarantees for free."
  [db-info opts]
  (when-not (map? opts)
    (throw (ex-info "store-iteration! opts must be a map" {:got (type opts)})))
  (when-not (:query-id opts)
    (throw (ex-info "store-iteration! requires :query-id" {:opts (keys opts)})))
  ((deref (resolve-impl db-info 'store-iteration!)) db-info opts))

(defdelegate db-list-query-iterations     [db-info query-ref])
(defdelegate db-list-iteration-vars       [db-info iter-ref])
(defdelegate db-list-iteration-expressions [db-info iter-ref])

;; --- Var registry & history ---
(defn db-latest-var-registry
  ([db-info conv-ref]      ((deref (resolve-impl db-info 'db-latest-var-registry)) db-info conv-ref))
  ([db-info conv-ref opts] ((deref (resolve-impl db-info 'db-latest-var-registry)) db-info conv-ref opts)))

(defdelegate db-var-history    [db-info conv-ref sym])
(defdelegate db-query-history  [db-info conv-ref])

;; --- Dependencies ---
(defdelegate store-dependency!     [db-info opts])
(defdelegate db-list-dependencies  [db-info conv-state-id])

;; --- Restore ---
(defdelegate db-restore-expressions [db-info conv-id])

;; =============================================================================
;; Error translation
;;
;; Frontends (TUI, CLI, Telegram) all surface persistence exceptions in
;; chat bubbles. The raw JDBC text (e.g. `[SQLITE_CANTOPEN] unable to
;; open the database file`) is meaningless without context, so the
;; persistence layer owns the translation — not any one frontend, not
;; the conversation runtime above it. Detection is text-based on
;; purpose so this jar keeps zero compile-time dep on driver classes.
;; =============================================================================

(defn- causal-chain
  "Walk `(.getCause e)` until a fixed point or cycle is hit. Returns the
   chain in causal order (innermost first), bounded so a self-referential
   cause graph can't loop forever."
  [^Throwable e]
  (loop [acc [] cur e seen #{}]
    (cond
      (nil? cur)           (reverse acc)
      (contains? seen cur) (reverse acc)
      (>= (count acc) 16)  (reverse acc)
      :else (recur (conj acc cur) (.getCause cur) (conj seen cur)))))

(defn- sqlite-cantopen-message?
  "True when any link in the cause chain looks like a SQLite open failure."
  [^Throwable e]
  (boolean
    (some (fn [^Throwable t]
            (let [^String m (or (ex-message t) "")]
              (or (.contains m "[SQLITE_CANTOPEN]")
                (.contains m "unable to open database file")
                (.contains m "Unable to open the database file"))))
      (causal-chain e))))

(defn error->user-message
  "Translate an exception from the persistence layer into something a
   human reading a chat bubble can act on.

   For most exceptions we surface `(ex-message e)` verbatim — provider
   errors, validation issues, etc. are often self-explanatory. The one
   case we rewrite is `SQLITE_CANTOPEN`, because the raw message is
   meaningless without context: the underlying file at
   `~/.vis/vis.mdb/vis.db` was either deleted out from under the
   running JVM, or moved, or the process lost write permissions to the
   directory. Anyone hitting this on the chat surface needs to know
   what to inspect, not the JDBC error code."
  [^Throwable e]
  (cond
    (sqlite-cantopen-message? e)
    (let [home   (System/getProperty "user.home")
          dbpath (str home "/.vis/vis.mdb/vis.db")
          dbdir  (str home "/.vis/vis.mdb")
          dirf   (java.io.File. dbdir)
          filef  (java.io.File. dbpath)]
      (str "Vis database is unavailable. "
        "Expected file: " dbpath ". "
        (cond
          (not (.exists filef))
          "The file is missing — likely deleted while Vis was running. Restart Vis to recreate it."

          (not (.canWrite dirf))
          (str "The directory " dbdir " is not writable by this process.")

          :else
          "The handle was lost mid-session. Restart Vis to reconnect.")))

    :else
    (or (ex-message e) "Internal error")))

;; =============================================================================
;; Process-wide shared connection (singleton helper)
;;
;; vis runs every channel (TUI, CLI, Telegram) against ONE SQLite DB
;; per process. Owning the singleton here — instead of in any
;; particular frontend or in conversation/core — keeps the DB
;; lifecycle inside the persistence layer where it belongs and lets
;; multiple frontends share the handle without each maintaining its
;; own atom.
;; =============================================================================

(defonce ^:private shared-conn (atom nil))

(defn shared-conn!
  "Return the process-wide shared persistence connection for `db-spec`,
   opening it on first call and caching the handle for the lifetime of
   the JVM. Subsequent calls return the cached handle regardless of
   the `db-spec` argument — the singleton intentionally pins to the
   first spec it saw.

   Pair with `dispose-shared-conn!` on process shutdown."
  [db-spec]
  (or @shared-conn
    (swap! shared-conn (fn [cur] (or cur (create-store-connection db-spec))))))

(defn dispose-shared-conn!
  "Close the shared connection if one is open. Idempotent."
  []
  (when-let [c @shared-conn]
    (try (dispose-store-connection! c) (catch Exception _ nil))
    (reset! shared-conn nil)))

;; =============================================================================
;; Orphan sweep (process-restart cleanup)
;; =============================================================================

(def ^:private ORPHAN_INTERRUPTED_ANSWER
  "Warning: Turn interrupted — the server was restarted before this answer could finalize. Re-send the message to retry.")

(defn sweep-orphaned-running-queries!
  "Mark every `:running` query as `:interrupted`. Run at process start
   to clean up queries that crashed or were killed mid-write so the
   next turn's handover digest renders the right outcome instead of
   guessing. Returns the number of queries swept."
  [db-info]
  (let [orphans (try (db-list-queries-by-status db-info :running)
                  (catch Exception _ []))]
    (doseq [{:keys [id iterations duration-ms]} orphans]
      (try
        (update-query! db-info id
          {:answer        ORPHAN_INTERRUPTED_ANSWER
           :iterations    (or iterations 0)
           :duration-ms   (or duration-ms 0)
           :status        :interrupted
           :prior-outcome :cancelled})
        (catch Exception _ nil)))
    (count orphans)))

;; =============================================================================
;; Extension specification
;; =============================================================================

;; =============================================================================
;; Predicates
;; =============================================================================

;; =============================================================================
;; Symbol entry spec
;; =============================================================================

;; Symbol name bound in the SCI sandbox.
(s/def :ext.symbol/sym symbol?)

;; Implementation function the LLM calls from :code blocks.
(s/def :ext.symbol/fn fn?)

;; One-liner description shown in the sandbox var's docstring.
(s/def :ext.symbol/doc non-blank-string?)

;; Argument signatures, e.g. '([query] [query opts]).
;; Shown in var meta :arglists and used to derive :examples when missing.
(s/def :ext.symbol/arglists (s/and vector? seq))

;; Usage examples injected into the system prompt so the LLM sees
;; concrete call patterns, e.g. ["(search-documents \"neural\")"]
(s/def :ext.symbol/examples (s/and vector? seq #(every? non-blank-string? %)))

;; Entry decorator: (fn [env f args] → map). Wraps :fn on the way in.
;; Receives the environment, the implementation fn, and the original args.
;; Returns a map — see Symbol Decorators in docs/src/extensions/hooks.md:
;;   {:args [...]}    — override args passed to :fn
;;   {:fn f'}         — override the implementation fn
;;   {:env env'}      — override env for the call
;;   {:result val}    — short-circuit: skip :fn entirely, return val
;; Missing keys keep the current value. Throw to abort.
;; This is the same pattern as Ring middleware / Pedestal :enter.
(s/def :ext.symbol/before-fn fn?)

;; Exit decorator: (fn [env f args result] → map). Wraps :fn on the way out.
;; Receives the environment, the implementation fn, the args, and the raw result.
;; Returns a map — see Symbol Decorators in docs/src/extensions/hooks.md:
;;   {:result val}    — override the result
;;   {:env :fn :args} — override (rarely needed)
;; Missing keys keep the current value.
;; This is the same pattern as Ring middleware / Pedestal :leave.
(s/def :ext.symbol/after-fn fn?)

;; Error decorator: (fn [err env f args] → map). Called when :fn throws.
;; Receives the exception, environment, the implementation fn, and the original args.
;; Returns a map — see Symbol Decorators in docs/src/extensions/hooks.md:
;;   {:result val}    — use as fallback result
;;   {:error err}     — throw this error instead
;;   {:fn f' :args a'} — retry with (possibly different) fn and args
;; If no :on-error-fn is defined, the original exception propagates.
;; This is the same pattern as Pedestal :error.
(s/def :ext.symbol/on-error-fn fn?)

;; Plain value bound in the sandbox (constant, data, config).
;; Mutually exclusive with :ext.symbol/fn - a symbol is either a
;; function or a value, never both.
(s/def :ext.symbol/val some?)

;; Function symbol: :fn is required, hooks are optional.
(s/def ::fn-symbol-entry
  (s/keys :req [:ext.symbol/sym :ext.symbol/fn :ext.symbol/doc
                :ext.symbol/arglists :ext.symbol/examples]
    :opt [:ext.symbol/before-fn :ext.symbol/after-fn
          :ext.symbol/on-error-fn]))

;; Value symbol: just name + value + doc. No hooks, no arglists.
(s/def ::val-symbol-entry
  (s/keys :req [:ext.symbol/sym :ext.symbol/val :ext.symbol/doc]))

;; A symbol entry is either a function or a value.
(s/def ::symbol-entry
  (s/or :fn  ::fn-symbol-entry
    :val ::val-symbol-entry))

;; =============================================================================
;; Extension spec
;; =============================================================================

;; Fully qualified extension name, e.g. 'com.blockether.vis.ext.common.
;; Used as the identity key in the extension registry and stored in
;; iteration metadata for post-mortem / reproducibility.
(s/def :ext/namespace symbol?)

;; Extension-level documentation - describes what this bundle provides.
(s/def :ext/doc non-blank-string?)

;; Top-level group for prompt rendering, e.g. "knowledge", "conversation".
;; Extensions in the same group are rendered together in the system prompt.
(s/def :ext/group non-blank-string?)

;; Subgroup within the group, e.g. "documents" under "knowledge".
;; Finer-grained grouping for prompt layout.
(s/def :ext/subgroup non-blank-string?)

;; Guard evaluated at each query boundary. When falsy, ALL symbols in
;; this extension are unbound from the sandbox - the LLM cannot call them.
;; (fn [env] -> bool). Default: (constantly true).
(s/def :ext/activation-fn fn?)

;; Optional extra LLM-facing documentation appended AFTER the canonical
;; symbol-derived prompt block when the extension is active.
;; Accepts string | (fn [env] → string). Nil means: rely only on the
;; auto-rendered prompt derived from :ext/doc + :ext/symbols metadata.
(s/def :ext/prompt fn?)

;; Optional per-iteration nudge composer.
;; (fn [ctx] → string-or-nil). Called every iteration; return a
;; `[system_nudge] …` string to inject a nudge, nil to skip.
;; See docs/src/extensions/nudges.md for the ctx shape.
(s/def :ext/nudge-fn fn?)

;; Optional source-code rewriter for SCI/edamame parse errors.
;; (fn [ctx] → string-or-nil), where ctx is
;;   {:code        original code string the LLM emitted
;;    :error       edamame's error message (e.g.
;;                 "[line 1, col 12] Unsupported escape character: \|")
;;    :environment the live conversation environment}
;; Returns:
;;   - a NEW source string — the iteration loop retries parsing/eval
;;     with the rewritten code and tags the result :repaired? true
;;   - nil — pass; the next extension's hook is consulted, or the
;;     parse error is surfaced to the LLM as before
;;
;; Symbol-level :on-error-fn cannot help here because parse failures
;; happen before any tool fn is dispatched, so the recovery is
;; necessarily extension-wide. Hooks that throw are logged and
;; treated as if they returned nil.
(s/def :ext/on-parse-error-fn fn?)

;; Optional dependency declaration.
;; Vector of extension namespace symbols that must be registered
;; before this extension. Checked at `register-extension!` time.
;; e.g. ['filesystem 'git]
(s/def :ext/requires (s/coll-of symbol? :kind vector?))

;; Semver version string, e.g. "1.0.0", "0.3.1-SNAPSHOT".
(s/def :ext/version non-blank-string?)

;; Author name or org, e.g. "Blockether", "Jane Doe <jane@example.com>".
(s/def :ext/author non-blank-string?)

;; SPDX license identifier, e.g. "MIT", "Apache-2.0", "EPL-2.0".
(s/def :ext/license non-blank-string?)

;; ============================================================================
;; Surface slots
;;
;; An extension declaration is the SINGLE entry point for everything
;; an extension contributes to vis. Whatever surfaces the extension
;; populates -- SCI sandbox symbols, CLI commands, channels,
;; providers, persistance entries -- it does so by listing them in
;; the matching `:ext/<surface>` slot. `ext/register-global!` then
;; dispatches each slot to its concrete sub-registry under the hood.
;;
;; The slot specs validate the FULL entry shape per surface -- vector
;; of `map?` is not enough, because a typo at extension-author time
;; (e.g. `:cmd/run` vs `:cmd/run-fn`, missing `:channel/main-fn`,
;; non-keyword `:provider/id`) would otherwise only blow up at
;; dispatch time, far from the offending `(register-global! …)` call
;; that introduced it. Concrete entry specs catch these at validation.
;;
;; Where possible we DELEGATE to the canonical spec for that surface
;; instead of duplicating field rules:
;;   :ext/cli      -> ::command
;;   :ext/channels -> :com.blockether.vis.core
;; All registries now live inside vis-runtime. The extension facade can
;; require them directly; there is no longer a separate compatibility
;; boundary between extension, commandline, provider, and persistence.
;; ============================================================================

;; CLI commands exported by this extension. Each entry must conform
;; to the canonical commandline command shape
;; (`::command`): `:cmd/name` +
;; `:cmd/doc` are required; `:cmd/usage`, `:cmd/args`, `:cmd/run-fn`,
;; `:cmd/subcommands`, `:cmd/owns-tty?`, `:cmd/examples`,
;; `:cmd/parent` optional. See commandline.base for the per-key specs.
;;
;; Every entry is auto-mounted under `vis extensions <name>` -- THIS
;; SLOT IS THE EXTENSIONS SUBCOMMAND TREE. The dispatcher sets
;; `:cmd/parent ["extensions"]` for entries that don't specify one;
;; entries that DO specify must start with `"extensions"` (deeper
;; nests like `["extensions" "git"]` are allowed for sub-trees) --
;; that's enforced at register time by `validate-cli-entry-parent!`,
;; below. The spec here only checks structural shape.
;;
;; Top-level built-ins like `vis run` / `vis auth` are the binary's
;; commands, not an extension's, and use `register-cmd!`
;; directly -- `:ext/cli` does NOT and CANNOT mount at the top level.
;;
;;     :ext/cli [{:cmd/name   "blame"
;;                :cmd/doc    "Show git blame."
;;                :cmd/run-fn #'cli-blame}]
;;
;;     ;; deeper nest -- shows up as `vis extensions git status`
;;     :ext/cli [{:cmd/name   "status"
;;                :cmd/doc    "Show git status."
;;                :cmd/parent ["extensions" "git"]
;;                :cmd/run-fn #'cli-git-status}]
(s/def :ext/cli
  (s/coll-of :com.blockether.vis.internal.registry/command :kind vector?))

;; Channels exported by this extension. Each entry must conform to
;; the canonical channel shape (`:com.blockether.vis.core`):
;; `:channel/id` (keyword), `:channel/cmd` (non-blank string),
;; `:channel/doc` (non-blank string), `:channel/main-fn` (ifn) are
;; required; `:channel/usage` (non-blank string), `:channel/owns-tty?`
;; (boolean) are optional. Each entry is forwarded to
;; `register-channel!`; it appears under `vis channels <cmd>`.
(s/def :ext/channels
  (s/coll-of ::channel :kind vector?))

;; LLM providers exported by this extension. Each entry mirrors the
;; canonical provider shape. Required: `:provider/id` (keyword),
;; `:provider/label` (non-blank string). Optional fns (every one ifn):
;; `:provider/status-fn`, `:provider/logout-fn`, `:provider/detect-fn`,
;; `:provider/auth-fn`, `:provider/get-token-fn`. The `or-nil-or-fn`
;; predicate accepts an absent key (treated as nil) or any IFn.
(let [or-nil-or-fn (fn [k] #(let [v (get % k ::absent)] (or (= v ::absent) (ifn? v))))]
  (s/def ::provider-entry
    (s/and map?
      #(keyword? (:provider/id %))
      #(non-blank-string? (:provider/label %))
      (or-nil-or-fn :provider/status-fn)
      (or-nil-or-fn :provider/logout-fn)
      (or-nil-or-fn :provider/detect-fn)
      (or-nil-or-fn :provider/auth-fn)
      (or-nil-or-fn :provider/get-token-fn))))
(s/def :ext/providers (s/coll-of ::provider-entry :kind vector?))

;; Persistence backends exported by this extension. Each entry is
;; `{:persistance/id <keyword>
;;   :persistance/ns <fully-qualified-symbol>}` -- the id is the
;; backend tag stored in config (`{:backend :sqlite ...}`) and the
;; ns is the namespace to `require` so its `register-backend!` call
;; runs. Each entry is forwarded to `register-backend!`.
;;
;; Slot name preserves the existing persistence vocabulary (and the
;; existing `:cli`/`:channels`/`:providers` slot-naming convention --
;; capability area, not the implementation noun "backend").
(s/def :persistance/id keyword?)
(s/def :persistance/ns
  ;; Must be a SYMBOL we can pass to `require` -- a fully-qualified
  ;; namespace symbol like 'com.blockether.vis.ext.persistance-sqlite.core,
  ;; not just any symbol. Allowing bare symbols would let an entry
  ;; ship a typo'd value that only blows up at the requiring-resolve
  ;; site (mid-boot, far from the extension declaration).
  (s/and symbol?
    #(nil? (namespace %))
    #(re-find #"\." (name %))))
(s/def :ext/persistance-entry (s/keys :req [:persistance/id :persistance/ns]))
(s/def :ext/persistance (s/coll-of :ext/persistance-entry :kind vector?))

;; Vector of symbol entries this extension binds into the sandbox.
(s/def :ext/symbols (s/coll-of ::symbol-entry :kind vector?))

;; Map of fully-qualified Java classes to expose in the sandbox.
;; Keys are FQ symbols, values are the Class objects.
;; Enables `(java.time.LocalDate/now)` style access.
;; e.g. {'java.time.LocalDate java.time.LocalDate}
(s/def :ext/classes
  (s/and map?
    #(every? symbol? (keys %))
    #(every? class? (vals %))))

;; Map of short-name imports for Java classes.
;; Keys are short symbols, values are FQ symbols.
;; Enables `(LocalDate/now)` style access.
;; e.g. {'LocalDate java.time.LocalDate}
(s/def :ext/imports
  (s/and map?
    #(every? symbol? (keys %))
    #(every? symbol? (vals %))))

;; Optional SCI namespace alias for this extension's symbols.
;; When set, a dedicated SCI namespace is created and aliased so
;; the LLM can call `(vis/cat "x")` in addition to `(cat "x")`.
;; e.g. {:ns 'vis.ext.tools :alias 'vis}
;;
;; Both `:ns` and `:alias` must be plain (non-namespaced) symbols --
;; SCI uses them as namespace names and aliases respectively, neither
;; of which is allowed to itself carry a namespace.
(s/def :ext.ns-alias/ns    (s/and symbol? #(nil? (namespace %))))
(s/def :ext.ns-alias/alias (s/and symbol? #(nil? (namespace %))))
(s/def :ext/ns-alias
  (s/and map?
    #(s/valid? :ext.ns-alias/ns    (:ns %))
    #(s/valid? :ext.ns-alias/alias (:alias %))))

(defn- ns-alias-required-when-symbols?
  "When :ext/symbols is non-empty, :ext/ns-alias must be present."
  [ext]
  (or (empty? (:ext/symbols ext))
    (some? (:ext/ns-alias ext))))

(defn- group-required-when-symbols?
  "When :ext/symbols is non-empty, :ext/group must be present (it's the
   prompt-rendering bucket for those symbols)."
  [ext]
  (or (empty? (:ext/symbols ext))
    (some? (:ext/group ext))))

(s/def ::extension
  (s/and
    ;; Only `:ext/namespace` and `:ext/doc` are unconditionally required.
    ;; Everything else is optional and defaulted -- an extension that
    ;; only ships, say, `:ext/channels` shouldn't be forced to declare
    ;; `:ext/group`, `:ext/symbols`, `:ext/activation-fn`, etc. just
    ;; because the SCI surface exists.
    (s/keys :req [:ext/namespace :ext/doc]
      :opt [:ext/group :ext/subgroup :ext/activation-fn
            :ext/symbols :ext/classes :ext/imports
            :ext/ns-alias :ext/prompt :ext/nudge-fn
            :ext/on-parse-error-fn :ext/requires
            :ext/version :ext/author :ext/license
            :ext/cli :ext/channels :ext/providers :ext/persistance])
    ns-alias-required-when-symbols?
    group-required-when-symbols?))

;; =============================================================================
;; Symbol helper
;; =============================================================================

(defn- validate-symbol-entry!
  "Assert a symbol entry conforms to ::symbol-entry. Throws on violation."
  [entry]
  (when-not (s/valid? ::symbol-entry entry)
    (throw (ex-info (str "Invalid symbol '" (:ext.symbol/sym entry) "':\n"
                      (with-out-str (s/explain ::symbol-entry entry)))
             {:type   :extension/invalid-symbol
              :sym    (:ext.symbol/sym entry)
              :explain (s/explain-data ::symbol-entry entry)})))
  entry)

(defn- derive-examples [sym arglists]
  (if-let [al (first (seq arglists))]
    (let [args (->> al (remove #{'&}) (map str) (str/join " "))]
      [(str "(" sym (when (seq args) (str " " args)) ")")])
    [(str "(" sym ")")]))

(defn symbol
  "Build a function symbol entry.

   Required: :doc, :arglists
   Optional: :examples, :before-fn, :after-fn, :on-error-fn,
             :autobind-fn, :on-parse-error-fn

   Defaults:
     :examples — derived from :arglists when not provided

   (symbol 'search-documents search-fn
     {:doc      \"Full-text search across documents.\"
      :arglists '([query] [query opts])
      :examples [\"(search-documents \\\"neural\\\")\"]
      ;; Runtime hooks — fire AFTER `:fn` is dispatched. Each returns a
      ;; MAP, not a direct value. See Symbol Decorators in docs/src/extensions/hooks.md.
      :before-fn   (fn [env f args] {:args (transform args)})    ;; override args/fn/env, or {:result v} to short-circuit
      :after-fn    (fn [env f args result] {:result (transform result)})  ;; override result
      :on-error-fn (fn [err env f args] {:result fallback})    ;; recover, retry, or {:error e} to re-throw
      :autobind-fn (fn [{:keys [args result environment]}]
                     {:bindings [{:kind :file :id (first args) :content result}]})
      ;; Parse-time hook — fires when SCI/edamame rejects the LLM's
      ;; source AND this symbol's name appears in the broken code.
      ;; Returns rewritten source (string) or nil.
      :on-parse-error-fn (fn [{:keys [code error sym environment]}]
                           (rewrite-source code error))})"
  [sym-name f opts]
  (let [arglists (:arglists opts)
        arglists (when arglists (if (seq? arglists) (vec arglists) arglists))
        examples (or (:examples opts)
                   (when arglists (derive-examples sym-name arglists)))]
    (validate-symbol-entry!
      (cond-> #:ext.symbol{:sym sym-name
                           :fn  f}
        (:doc opts)               (assoc :ext.symbol/doc (:doc opts))
        arglists                  (assoc :ext.symbol/arglists arglists)
        examples                  (assoc :ext.symbol/examples (vec examples))
        (:before-fn opts)         (assoc :ext.symbol/before-fn (:before-fn opts))
        (:after-fn opts)          (assoc :ext.symbol/after-fn (:after-fn opts))
        (:on-error-fn opts)       (assoc :ext.symbol/on-error-fn (:on-error-fn opts))))))

(defn value
  "Build a value symbol entry - a plain constant/data binding.

   (value 'max-retries 3
     {:doc \"Maximum retry attempts.\"})

   (value 'config {:host \"localhost\" :port 3000}
     {:doc \"Server configuration map.\"})

   All three args required. :doc in opts is required."
  [sym-name val opts]
  (let [entry #:ext.symbol{:sym sym-name
                           :val val
                           :doc (:doc opts)}]
    (validate-symbol-entry! entry)))

(defn- arglist->call-form
  [alias-sym sym-name arglist]
  (let [args   (->> arglist (remove #{'&}) (map str) (str/join " "))
        target (if alias-sym
                 (str alias-sym "/" sym-name)
                 (str sym-name))]
    (str "(" target (when (seq args) (str " " args)) ")")))

(defn- render-symbol-line
  [alias-sym entry]
  (let [{sym-name :ext.symbol/sym
         doc      :ext.symbol/doc
         arglists :ext.symbol/arglists} entry]
    (if (:ext.symbol/fn entry)
      (str "- "
        (str/join " or " (map #(arglist->call-form alias-sym sym-name %) arglists))
        " — " doc)
      (str "- "
        (if alias-sym
          (str alias-sym "/" sym-name)
          (str sym-name))
        " — " doc))))

(defn render-prompt
  "Render canonical :ext/prompt text from symbol docstrings + arglists.

   Accepts an extension map or any map with:
   - :ext/doc      or :heading
   - :ext/ns-alias optional {:alias 'vis}
   - :ext/symbols  vector of ext/symbol + ext/value entries
   - :usage-note   optional extra note added to the heading
   - :notes        optional string or seq of extra lines appended verbatim

   Returns a prompt string suitable for :ext/prompt."
  [{:keys [heading usage-note notes] :as opts}]
  (let [alias-sym    (get-in opts [:ext/ns-alias :alias])
        symbols      (or (:symbols opts) (:ext/symbols opts))
        heading      (or heading (:ext/doc opts) "Extension tools")
        header-notes (vec (remove nil?
                            [(when alias-sym (str "use " alias-sym "/ prefix"))
                             (when (non-blank-string? usage-note) usage-note)]))
        extra-lines  (cond
                       (nil? notes)        []
                       (string? notes)     [notes]
                       (sequential? notes) (vec notes)
                       :else               [(str notes)])
        body-lines   (mapv #(render-symbol-line alias-sym %) symbols)]
    (str/join "\n"
      (concat [(str heading
                 (when (seq header-notes)
                   (str " (" (str/join "; " header-notes) ")")))]
        body-lines
        extra-lines))))

;; =============================================================================
;; Normalization
;; =============================================================================

(defn- normalize-prompt [prompt]
  (cond
    (nil? prompt)    nil
    (fn? prompt)     prompt
    (string? prompt) (constantly prompt)
    :else (throw (ex-info ":ext/prompt must be a string or (fn [env] string)"
                   {:got (type prompt)}))))

;; =============================================================================
;; Validation
;; =============================================================================

(defn validate!
  "Normalize and assert that an extension map conforms to ::extension.
   Normalizes `:ext/prompt` (string → fn) before checking the spec
   when the key is present. Throws with spec explain-data on violation."
  [ext]
  (let [ext (cond-> ext
              (contains? ext :ext/prompt) (update :ext/prompt normalize-prompt))]
    (when-not (s/valid? ::extension ext)
      (throw (ex-info (str "Invalid extension '" (:ext/namespace ext) "':\n"
                        (with-out-str (s/explain ::extension ext)))
               {:type      :extension/invalid-spec
                :namespace (:ext/namespace ext)
                :explain   (s/explain-data ::extension ext)})))
    ext))

;; =============================================================================
;; Hook execution - runtime wrappers with output validation + logging
;;
;; Every hook returns {:fn f :args args :env env} to override the call
;; context. All keys are optional - missing keys keep the current value.
;; :before-fn can return {:result val} to short-circuit without calling :fn.
;; :on-error-fn can return {:result val}, {:error err}, or {:fn :args :env} to retry.
;; :after-fn can return {:result val} to override the result.
;; =============================================================================

(defn- validate-hook-return!
  [hook-name sym returned]
  (when-not (map? returned)
    (throw (ex-info (str hook-name " for '" sym "' must return a map, got: " (type returned))
             {:type (keyword "extension" (str hook-name "-error")) :sym sym :returned returned}))))

(defn- call-hook
  [hook-name sym hook-fn hook-args]
  (try
    (apply hook-fn hook-args)
    (catch clojure.lang.ArityException e
      (throw (ex-info (str hook-name " for '" sym "' has wrong arity: " (ex-message e))
               {:type (keyword "extension" (str hook-name "-error")) :sym sym} e)))
    (catch Throwable e
      (throw (ex-info (str hook-name " for '" sym "' threw: " (ex-message e))
               {:type (keyword "extension" (str hook-name "-error")) :sym sym} e)))))

(defn- elapsed-ms [t0] (/ (- (System/nanoTime) t0) 1e6))

(defn- log-hook! [level id ext-ns sym phase ms extra-msg]
  (tel/log! {:level level :id id
             :data {:ext ext-ns :sym sym :phase phase :ms ms}
             :msg (str ext-ns "/" sym " :invoke"
                    (when phase (str " " phase))
                    (when ms (str " " (format "%.1fms" (double ms))))
                    (when extra-msg (str " " extra-msg)))}))

(defn- run-before [ext-ns sym-entry env f args]
  (if-let [before (:ext.symbol/before-fn sym-entry)]
    (let [sym (:ext.symbol/sym sym-entry)
          t0  (System/nanoTime)
          _   (log-hook! :debug ::before-fn ext-ns sym :before-fn nil nil)
          ret (call-hook ":before-fn" sym before [env f args])
          _   (validate-hook-return! ":before-fn" sym ret)
          ms  (elapsed-ms t0)]
      (if (contains? ret :result)
        (do (log-hook! :debug ::before-fn-done ext-ns sym :before-fn ms "short-circuited")
          {:result (:result ret)})
        (do (log-hook! :debug ::before-fn-done ext-ns sym :before-fn ms nil)
          {:env  (get ret :env env)
           :fn   (get ret :fn f)
           :args (vec (get ret :args args))})))
    {:env env :fn f :args args}))

(defn- run-after [ext-ns sym-entry env f args result]
  (if-let [after (:ext.symbol/after-fn sym-entry)]
    (let [sym (:ext.symbol/sym sym-entry)
          t0  (System/nanoTime)
          _   (log-hook! :debug ::after-fn ext-ns sym :after-fn nil nil)
          ret (call-hook ":after-fn" sym after [env f args result])
          _   (validate-hook-return! ":after-fn" sym ret)
          ms  (elapsed-ms t0)]
      (log-hook! :debug ::after-fn-done ext-ns sym :after-fn ms nil)
      {:env    (get ret :env env)
       :fn     (get ret :fn f)
       :args   (vec (get ret :args args))
       :result (get ret :result result)})
    {:env env :fn f :args args :result result}))

(defn- run-on-error [ext-ns sym-entry err env f args]
  (if-let [on-error (:ext.symbol/on-error-fn sym-entry)]
    (let [sym (:ext.symbol/sym sym-entry)
          t0  (System/nanoTime)
          _   (log-hook! :warn ::on-error-fn ext-ns sym :on-error-fn nil (str "handling: " (ex-message err)))
          ret (try
                (call-hook ":on-error-fn" sym on-error [err env f args])
                (catch Throwable e
                  (if (identical? e err)
                    (throw e)
                    (throw (ex-info (str ":on-error-fn for '" sym "' threw: " (ex-message e))
                             {:type :extension/on-error-fn-error :sym sym} e)))))
          _   (validate-hook-return! ":on-error-fn" sym ret)
          ms  (elapsed-ms t0)]
      (cond
        (contains? ret :result)
        (do (log-hook! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "fallback result") ret)
        (contains? ret :error)
        (do (log-hook! :debug ::on-error-fn-done ext-ns sym :on-error-fn ms "surfacing error") ret)
        :else
        (do (log-hook! :info ::on-error-fn-done ext-ns sym :on-error-fn ms "retrying") ret)))
    (throw err)))

(defn invoke-symbol-wrapper
  "Full invocation pipeline for a function symbol entry:
   before-fn → fn → after-fn, with on-error-fn catching :fn errors.

   Every hook can override :fn, :args, :env via its return map.
   :before-fn can return {:result val} to short-circuit.
   :on-error-fn can return {:result val}, {:error err}, or {:fn :args :env} to retry.

   Returns the final result. Throws on any unrecoverable error."
  [ext sym-entry args env]
  (let [sym    (:ext.symbol/sym sym-entry)
        ext-ns (:ext/namespace ext)
        t0     (System/nanoTime)
        _      (log-hook! :info ::invoke ext-ns sym nil nil nil)
        before-out (run-before ext-ns sym-entry env (:ext.symbol/fn sym-entry) args)]
    (if (contains? before-out :result)
      (let [ms (elapsed-ms t0)]
        (log-hook! :info ::invoke-done ext-ns sym nil ms "short-circuited")
        (:result before-out))
      (let [{env  :env
             f    :fn
             args :args} before-out

            call-result
            (let [ct0 (System/nanoTime)]
              (try
                (let [r  (apply f args)
                      ms (elapsed-ms ct0)]
                  (log-hook! :debug ::fn-returned ext-ns sym :call ms nil)
                  {:result r})
                (catch Throwable e
                  (let [ms (elapsed-ms ct0)]
                    (log-hook! :warn ::fn-threw ext-ns sym :call ms (ex-message e))
                    (let [recovery (run-on-error ext-ns sym-entry e env f args)]
                      (cond
                        (contains? recovery :result) recovery
                        (contains? recovery :error)  (throw (:error recovery))
                        :else {:result (apply (get recovery :fn f)
                                         (vec (get recovery :args args)))}))))))

            {:keys [result]} (run-after ext-ns sym-entry env f args (:result call-result))
            ms (elapsed-ms t0)]
        (log-hook! :info ::invoke-done ext-ns sym nil ms nil)
        result))))

(def ^:private ^:dynamic *log-writer*
  "Writer that sends output to the log file instead of stdout/stderr.
   Bound during extension invocations so tool fns never bleed into the TUI."
  nil)

(defn- get-log-writer []
  (or *log-writer*
    (let [log-path (str (System/getProperty "user.home") "/.vis/vis.log")]
      (alter-var-root #'*log-writer*
        (fn [cur] (or cur (io/writer log-path :append true))))
      *log-writer*)))

(defn wrap-extension
  "Wrap all function symbols in an extension into invocation fns.

   Returns a map of {sym → (fn [& args] result)} where each fn
   closes over the extension, symbol entry, and environment, then
   routes through `invoke-symbol-wrapper`.

   All stdout/stderr from extension calls is redirected to the log
   file so nothing bleeds into the TUI.

   Value symbols are returned as {sym → value}."
  [ext env]
  (into {}
    (map (fn [sym-entry]
           (let [sym (:ext.symbol/sym sym-entry)]
             (if (contains? sym-entry :ext.symbol/fn)
               [sym (fn [& args]
                      (let [w (get-log-writer)]
                        (binding [*out* w *err* w]
                          (invoke-symbol-wrapper ext sym-entry (vec args) env))))]
               [sym (:ext.symbol/val sym-entry)]))))
    (:ext/symbols ext)))

;; =============================================================================
;; Parse-error rescue — walked by the iteration loop
;;
;; SCI/edamame parse failures happen BEFORE any tool fn is dispatched,
;; so symbol-level :on-error-fn is useless for them. Two recovery
;; layers exist instead, in priority order:
;;
;;   1. SYMBOL-level `:ext.symbol/on-parse-error-fn` — fires only for
;;      symbols whose name is mentioned in the broken source. This
;;      keeps rescue logic co-located with the tool that caused it.
;;   2. EXTENSION-level `:ext/on-parse-error-fn` — a catch-all for
;;      cross-cutting rewrites (e.g. \"strip every JSON-style
;;      smart-quote\"). Fires only when no symbol-level hook produced
;;      a rewrite.
;;
;; Both layers receive `{:code :error :environment}`; symbol-level
;; hooks additionally receive `:sym`. First non-nil rewrite different
;; from `code` wins. Hooks that throw are logged and skipped — a
;; buggy hook can never break query execution.
;; =============================================================================

(defn- code-mentions-symbol?
  "Cheap regex check: does `code` look like it invokes `sym-name`,
   either bare `(name ...)` or aliased `(alias/name ...)`? We can't
   parse the broken source, so this is a substring scan with word
   boundaries. False positives are harmless — the worst case is the
   hook gets called and returns nil."
  [^String code ^String sym-name alias-name]
  (let [esc-name (java.util.regex.Pattern/quote sym-name)
        bare     (re-pattern (str "\\(\\s*" esc-name "(?:[\\s)\\[]|$)"))
        prefixed (when (and alias-name (seq alias-name))
                   (re-pattern (str "\\(\\s*"
                                 (java.util.regex.Pattern/quote alias-name)
                                 "/" esc-name "(?:[\\s)\\[]|$)")))]
    (boolean (or (re-find bare code)
               (and prefixed (re-find prefixed code))))))

(defn- run-parse-rescue-hook
  "Invoke a single parse-error hook, swallowing throws.
   `id` is purely a tag for the warn log so we can tell symbol- vs
   extension-level breakage apart."
  [id hook ctx]
  (try
    (hook ctx)
    (catch Throwable t
      (tel/log! {:level :warn :id ::on-parse-error-fn-threw
                 :data {:source id :error (ex-message t)}
                 :msg   (str ":on-parse-error-fn (" id ") threw: "
                          (ex-message t))})
      nil)))

(defn- try-symbol-parse-rescue
  "Walk every symbol of every extension. For symbols whose name appears
   in `code` AND that carry `:ext.symbol/on-parse-error-fn`, call the
   hook. First non-nil rewrite wins."
  [extensions code error environment]
  (loop [exts (seq extensions)]
    (when exts
      (let [ext   (first exts)
            alias (some-> (:ext/ns-alias ext) :alias clojure.core/name)
            hit
            (loop [syms (seq (:ext/symbols ext))]
              (when syms
                (let [entry (first syms)
                      sym   (:ext.symbol/sym entry)
                      hook  (:ext.symbol/on-parse-error-fn entry)]
                  (if (and hook sym (code-mentions-symbol? code (str sym) alias))
                    (let [out (run-parse-rescue-hook
                                (str (:ext/namespace ext) "/" sym)
                                hook
                                {:code        code
                                 :error       error
                                 :sym         sym
                                 :environment environment})]
                      (if (and (string? out) (not= out code))
                        out
                        (recur (next syms))))
                    (recur (next syms))))))]
        (or hit (recur (next exts)))))))

(defn- try-extension-parse-rescue
  "Walk extension-level `:ext/on-parse-error-fn` hooks as a catch-all
   layer for cross-cutting rewrites that aren't tied to one symbol."
  [extensions code error environment]
  (loop [exts (seq extensions)]
    (when exts
      (let [ext  (first exts)
            hook (:ext/on-parse-error-fn ext)
            out  (when hook
                   (run-parse-rescue-hook (str (:ext/namespace ext))
                     hook
                     {:code        code
                      :error       error
                      :environment environment}))]
        (if (and (string? out) (not= out code))
          out
          (recur (next exts)))))))

(defn try-rescue-parse-error
  "Walk `extensions` and produce a rewritten source string for a
   broken `code`, or nil when nothing wants to rescue.

   Resolution order:
     1. Per-symbol `:ext.symbol/on-parse-error-fn` of any registered
        symbol whose name appears in `code`.
     2. Extension-level `:ext/on-parse-error-fn` as a fallback.

   Hooks that throw or return non-strings or the unchanged code are
   skipped."
  [extensions code error environment]
  (or (try-symbol-parse-rescue extensions code error environment)
    (try-extension-parse-rescue extensions code error environment)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn extension
  "Build and validate an extension. The canonical constructor.

   Keys:
     :ext/namespace      — required, fully qualified symbol, e.g. 'com.blockether.vis.ext.common
     :ext/doc            — required, extension-level description
     :ext/group          — required, prompt group (e.g. \"knowledge\")
     :ext/subgroup       — optional, defaults to :ext/group
     :ext/activation-fn  — optional, (fn [env] → bool), default (constantly true)
     :ext/prompt         — optional, string or (fn [env] → string);
                           appended after the canonical auto-rendered
                           symbol prompt
     :ext/nudge-fn       — optional, (fn [ctx] → string|nil)
     :ext/on-parse-error-fn — optional, (fn [{:code :error :environment}]
                           → string|nil). Called when SCI/edamame
                           rejects the LLM's source. Return a
                           rewritten source string to retry, nil to
                           defer to the next extension. See
                           `try-rescue-parse-error`.
     :ext/requires       — optional, vector of extension namespace symbols
                           that must be registered first, default []
     :ext/version        — optional, semver string, e.g. \"1.0.0\"
     :ext/author         — optional, author name/org
     :ext/license        — optional, SPDX identifier, e.g. \"MIT\"
     :ext/symbols        — required, vector of symbol entries
     :ext/classes        — optional, {fq-symbol → Class}, default {}
     :ext/imports        — optional, {short-symbol → fq-symbol}, default {}
     :ext/ns-alias       — required when :ext/symbols is non-empty,
                           {:ns 'vis.ext.tools :alias 'vis}
                           Creates a dedicated SCI namespace with that alias.
                           Symbols are bound ONLY into this aliased namespace,
                           NEVER into the `sandbox` namespace directly. The
                           alias is auto-required in the sandbox so the LLM
                           must call (vis/cat ...) — bare
                           (cat ...) does not resolve.

   Example:

   (extension
     {:ext/namespace     'com.blockether.vis.ext.documents
      :ext/doc           \"Document search and retrieval\"
      :ext/group         \"knowledge\"
      :ext/requires      ['filesystem]
      :ext/prompt        \"Prefer narrow searches before broad scans.\"
      :ext/activation-fn (fn [env] (seq (list-docs (:db-info env))))
      :ext/nudge-fn      (fn [{:keys [environment iteration]}]
                           (when (> iteration 8)
                             \"[system_nudge] Narrow search scope.\"))
      :ext/symbols       [search-sym max-results-sym]})

   Returns a validated extension map conforming to ::extension."
  [spec]
  (-> spec
    ;; `:ext/prompt` is optional. Only run normalize-prompt when the
    ;; key is present, otherwise we'd insert a nil value that fails
    ;; the (s/def :ext/prompt fn?) spec.
    (cond-> (contains? spec :ext/prompt) (update :ext/prompt normalize-prompt))
    (cond->
      (not (:ext/activation-fn spec))                  (assoc :ext/activation-fn (constantly true))
      ;; `:ext/subgroup` defaults to `:ext/group` -- but only when
      ;; `:ext/group` is itself present (extensions that don't ship
      ;; SCI symbols don't need either).
      (and (not (:ext/subgroup spec))
        (some? (:ext/group spec)))                     (assoc :ext/subgroup (:ext/group spec))
      (not (:ext/symbols spec))                        (assoc :ext/symbols [])
      (not (:ext/classes spec))                        (assoc :ext/classes {})
      (not (:ext/imports spec))                        (assoc :ext/imports {})
      (not (:ext/requires spec))                       (assoc :ext/requires [])
      (not (:ext/cli spec))                            (assoc :ext/cli [])
      (not (:ext/channels spec))                       (assoc :ext/channels [])
      (not (:ext/providers spec))                      (assoc :ext/providers [])
      (not (:ext/persistance spec))                    (assoc :ext/persistance []))
    (validate!)))

;; =============================================================================
;; Global Extension Registry
;; =============================================================================

(defonce ^:private extension-registry
  ;; Process-level atom holding all globally registered extensions.
  ;; Keyed by :ext/namespace to prevent duplicates.
  (atom {}))

(defn- dispatch-providers! [providers]
  (doseq [provider-entry providers]
    (register-provider! provider-entry)))

(defn- dispatch-persistance! [entries]
  (doseq [{:persistance/keys [id ns]} entries]
    (register-backend! id ns)))

(def ^:private EXTENSIONS_PARENT ["extensions"])

(defn- mount-under-extensions
  "Auto-place an `:ext/cli` entry under the `vis extensions` parent.

   `:ext/cli` is reserved for commands the extension contributes to
   `vis extensions <cmd>` -- top-level built-ins like `vis run` are
   the binary's, not an extension's, and use `register-cmd!`
   directly. So every entry here gets `:cmd/parent` defaulted to
   `[\"extensions\"]`. Authors who want nested placement (e.g.
   `vis extensions git status`) can pass `:cmd/parent
   [\"extensions\" \"git\"]` and the dispatcher respects it AS LONG
   AS the first element is `\"extensions\"`. Any other parent is
   rejected -- `:ext/cli` is the extensions slot; mount somewhere
   else through `register-cmd!` direct."
  [{:cmd/keys [parent name] :as entry}]
  (cond
    (or (nil? parent) (= [] parent))
    (assoc entry :cmd/parent EXTENSIONS_PARENT)

    (= "extensions" (first parent))
    entry

    :else
    (throw (ex-info
             (str ":ext/cli entry '" name "' has :cmd/parent " (pr-str parent)
               " -- :ext/cli mounts only under [\"extensions\" ...]."
               " Use register-cmd! directly for arbitrary placement.")
             {:type :ext/cli-bad-parent
              :entry entry}))))

(defn register-extension!
  "Register an extension in the global process-level registry.

   This is THE single entry point for everything an extension
   contributes to vis. Whatever the extension declares -- SCI sandbox
   symbols (`:ext/symbols`), CLI commands (`:ext/cli`), channels
   (`:ext/channels`), LLM providers (`:ext/providers`), persistence
   backends (`:ext/persistance`) -- gets routed here and dispatched into
   the matching sub-registry as a side effect.

   Call this at namespace load time so the extension is available
   to every environment created afterwards:

     (ns my.company.ext.git
       (:require [com.blockether.vis.core :as ext]))

     (ext/register-global!
       (ext/extension
         {:ext/namespace 'com.acme.ext.git
          :ext/doc       \"Git integration.\"
          :ext/symbols   [git-status-sym git-blame-sym]
          :ext/cli       [{:cmd/name \"git-status\"
                           :cmd/parent [\"extensions\"]
                           :cmd/run-fn #'cli-git-status}]}))

   Idempotent on `:ext/namespace` -- re-registering replaces the
   previous version of the extension AND re-applies every sub-registry
   side effect (the inner registrars are themselves idempotent on
   their respective identity keys).

   Returns the validated extension."
  [ext]
  (let [ext    (extension ext)
        ns-sym (:ext/namespace ext)]
    (swap! extension-registry assoc ns-sym ext)
    (tel/log! {:level :info :id ::register-global
               :data {:ext ns-sym
                      :symbols     (count (:ext/symbols ext))
                      :cli         (count (:ext/cli ext))
                      :channels    (count (:ext/channels ext))
                      :providers   (count (:ext/providers ext))
                      :persistance (count (:ext/persistance ext))}
               :msg (str "Extension '" ns-sym "' registered globally")})
    ;; Side-effect: route each surface to its concrete registry.
    ;; The inner registrars validate their own input via specs and
    ;; throw on bad shape -- we trust that here.
    (doseq [c (:ext/cli ext)]      (register-cmd! (mount-under-extensions c)))
    (doseq [c (:ext/channels ext)] (register-channel! c))
    (dispatch-providers!   (:ext/providers ext))
    (dispatch-persistance! (:ext/persistance ext))
    ext))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn deregister-extension! [ns-sym]
  (swap! extension-registry dissoc ns-sym)
  nil)

(defn registered-extensions []
  (vec (vals @extension-registry)))

(defn- topo-sort-extensions
  "Topologically sort extensions by :ext/requires.
   Throws on missing dependencies or cycles."
  [extensions]
  (let [by-ns   (into {} (map (juxt :ext/namespace identity)) extensions)
        visited (volatile! #{})
        path    (volatile! #{})
        result  (volatile! [])]
    (letfn [(visit [ns-sym]
              (when (contains? @path ns-sym)
                (throw (ex-info (str "Circular extension dependency: " ns-sym
                                  " → ... → " ns-sym)
                         {:type :extension/circular-dependency
                          :extension ns-sym
                          :path @path})))
              (when-not (contains? @visited ns-sym)
                (vswap! path conj ns-sym)
                (let [ext (get by-ns ns-sym)]
                  (when-not ext
                    (throw (ex-info (str "Extension '" ns-sym "' required but not registered")
                             {:type :extension/missing-dependency
                              :extension ns-sym
                              :available (vec (keys by-ns))})))
                  (doseq [dep (:ext/requires ext)]
                    (visit dep)))
                (vswap! path disj ns-sym)
                (vswap! visited conj ns-sym)
                (vswap! result conj (get by-ns ns-sym))))]
      (doseq [ns-sym (keys by-ns)]
        (visit ns-sym)))
    @result))

(defn register-extensions!
  "Install all globally registered extensions into an environment.

   Topologically sorts by :ext/requires so dependencies are registered
   before dependents. Throws on missing dependencies or cycles.

   Called by `create-environment` automatically. Returns environment."
  [environment register-fn!]
  (let [exts   (registered-extensions)
        sorted (when (seq exts) (topo-sort-extensions exts))]
    (doseq [ext sorted]
      (register-fn! environment ext))
    environment))

(defn load-extension!
  "Dynamically load an extension from a Clojure namespace.

   Requires the namespace (which should call `register-global!` at
   load time), then returns the extension from the global registry.

   This is how an extension loads another extension at runtime:

     (ext/load-extension! 'my.company.ext.git)

   The loaded extension's `register-global!` fires during `require`,
   making it available for the next `register-extensions!` call
   or for immediate `register-extension!` into a live environment.

   Returns the extension map, or throws if the namespace doesn't
   register an extension."
  [ns-sym]
  (require ns-sym)
  (or (get @extension-registry ns-sym)
    (throw (ex-info (str "Namespace '" ns-sym
                      "' was loaded but did not call register-global!")
             {:type :extension/no-registration
              :namespace ns-sym
              :registered (vec (keys @extension-registry))}))))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn reload-extension!
  "Reload an extension namespace and hot-swap it everywhere.

   1. Forces `(require ns :reload)` — re-executes `register-global!`
   2. Updates the global registry (automatic via register-global!)
   3. If `environments` are provided, replaces the old version in
      each live environment's `:extensions` atom immediately.

   Arity:
     (reload-extension! ns-sym)
       Reload + update global registry only.

     (reload-extension! ns-sym environment)
       Reload + update global + hot-swap into one environment.

     (reload-extension! ns-sym environments)
       Reload + update global + hot-swap into all environments.

   This is what a meta-extension calls to hot-reload another
   extension into running conversations without restart:

     (ext/reload-extension! 'my.company.ext.git environment)

   Returns the updated extension."
  ([ns-sym]
   (reload-extension! ns-sym nil))
  ([ns-sym env-or-envs]
   (require ns-sym :reload)
   (let [ext (or (get @extension-registry ns-sym)
               (throw (ex-info (str "Namespace '" ns-sym
                                 "' was reloaded but did not call register-global!")
                        {:type :extension/no-registration
                         :namespace ns-sym
                         :registered (vec (keys @extension-registry))})))
         envs (cond
                (nil? env-or-envs)  nil
                (map? env-or-envs)  [env-or-envs]
                (sequential? env-or-envs) env-or-envs)]
     (doseq [environment envs]
       (when-let [ext-atom (:extensions environment)]
         (swap! ext-atom
           (fn [exts]
             (let [without (vec (remove #(= (:ext/namespace %) ns-sym) exts))]
               (conj without ext))))
         (tel/log! {:level :info :id ::reload-hot-swap
                    :data {:ext ns-sym :environment-id (:environment-id environment)}
                    :msg (str "Hot-swapped '" ns-sym "' into environment " (:environment-id environment))})))
     ext)))

;; =============================================================================
;; Unified extension auto-discovery + inline extension docs
;;
;; ONE classpath scan, ONE resource per jar
;; (`META-INF/vis-extension/vis.edn`), ONE entry point. Every
;; extension surface in the system -- ext symbols, channels, commands,
;; providers, persistance entries -- routes through this single fn.
;; "Extension" here is the SUPERSET term: a channel is a kind of
;; extension, a CLI command is a kind of extension, a provider is a
;; kind of extension. The bespoke per-subsystem `discover-*!` fns are
;; gone; this is the only discovery mechanism in the codebase.
;;
;; The loader is type-agnostic: it just `require`s every namespace
;; listed under `:nses` in every `META-INF/vis-extension/vis.edn` it
;; finds on the classpath; whichever of those namespaces calls
;; `(ext/register-global! ...)`, `(register-channel! ...)`,
;; `(register-cmd! ...)`, `(register-provider! ...)`, or
;; `(register-backend! ...)` ends up in the matching
;; subsystem registry as a side effect.
;;
;; Resource shape (a single EDN map keyed by extension id):
;;
;;     ;; resources/META-INF/vis-extension/vis.edn
;;     {git
;;      {:nses [com.acme.ext.git.core           ; ext/register-global!
;;              com.acme.channel.web.bot        ; register-channel!
;;              com.acme.commands.git           ; register-cmd!
;;              com.acme.providers.openai       ; register-provider!
;;              com.acme.persistance.postgres]  ; register-backend!
;;       :docs {\"README.md\" {:created-at #inst \"2026-04-28\"
;;                              :abstract   \"...\"
;;                              :content    \"# Git\n...\"
;;                              :links      [{:to-id meta :to-doc \"README.md\"
;;                                            :context \"...\"}
;;                                           {:url \"https://...\" :context \"...\"}
;;                                           {:file \"packages/.../foo.clj\"
;;                                            :context \"...\"}]}
;;              \"EXAMPLES.md\" {:created-at ... :abstract ... :content ...}}}}
;;
;; The id (top-level key, here `git`) is the LLM-facing token — same as
;; `:ext/ns-alias :alias` on the registered extension. `:nses` is the
;; vector of namespaces to require. `:docs` is a map
;; `{<doc-name> <descriptor>}` where each descriptor is a map with:
;;
;;   :created-at — #inst, when the doc was first authored.
;;   :abstract   — one-paragraph LLM-facing summary (mandatory).
;;   :content    — full Markdown body (mandatory).
;;   :links      — author-declared outgoing links. Each link is a map:
;;                  - cross-ext doc:  {:to-id <id> :to-doc <name> :context ...}
;;                  - same-ext doc:   {:to-doc <name> :context ...}
;;                  - external URL:   {:url <url> :context ...}
;;                  - repo file:      {:file <path> :context ...}
;;
;; The loader derives `:reflinks` automatically by inverting every
;; cross-ext / same-ext outgoing link into a `{:from-id :from-doc
;; :context}` entry on the target descriptor. Authors never write
;; reflinks by hand.
;;
;; Why inline + structured: one resource per jar means one classpath
;; read at boot, no nested doc tree, no path-vs-call ambiguity
;; (`vis-extension/<id>` reads cleanly whether `<id>` is `meta`,
;; `vis`, or anything else), and `(meta/extension-doc id name)` is a
;; registry lookup with zero I/O. The structured shape lets the LLM
;; scan abstracts and follow link graphs as plain Clojure data — no
;; YAML frontmatter parsing, no Markdown link extraction.
;;
;; Why this lives in `com.blockether.vis.core`: the docs
;; registry is an extension-specific consumer of the parsed manifests;
;; the generic classpath SCAN+REQUIRE primitive lives in
;; `vis-sdk.discovery` so subsystems with their own use of the same
;; manifests (storage backends today, anything else tomorrow) don't
;; have to reach this namespace through `requiring-resolve`.
;; =============================================================================

(defonce ^:private extension-docs-registry
  ;; {<id-symbol>
  ;;  {:nses [<ns-symbol> ...]
  ;;   :docs {<doc-name>
  ;;          {:created-at <inst>
  ;;           :abstract   <string>
  ;;           :content    <string>
  ;;           :links      [<link> ...]
  ;;           :reflinks   [<reflink> ...]}}}}
  ;;
  ;; Populated by `discover-extensions!` from every
  ;; `META-INF/vis-extension/vis.edn` on the classpath. Multiple jars
  ;; that declare the same id are merged: `:nses` are deduped
  ;; (preserving order of first occurrence), `:docs` are merged with
  ;; later entries winning per doc name. `:reflinks` are recomputed
  ;; from the union of all `:links` on every merge.
  (atom {}))

(defn registered-extension-ids
  "Sorted vector of every extension id known to the docs registry.
   Each id is the top-level key from a `vis.edn` and the same token
   the LLM uses as the SCI sandbox alias."
  []
  (vec (sort (keys @extension-docs-registry))))

(defn extension-namespaces
  "Vector of namespaces declared under `:nses` for an id. Empty when
   the id is unknown."
  [id]
  (vec (get-in @extension-docs-registry [id :nses] [])))

(defn extension-id-of-ns
  "Reverse lookup: given a namespace symbol, return the extension id
   that registered it under `:nses`, or `nil`."
  [ns-sym]
  (some (fn [[id {nses :nses}]]
          (when (some #(= ns-sym %) nses) id))
    @extension-docs-registry))

(defn extension-doc
  "Return the full descriptor map for a declared extension doc:
   `{:name :created-at :abstract :content :links :reflinks}`. Returns
   `nil` when the id is unknown or no doc by that name was declared."
  [id doc-name]
  (when-let [descriptor (and id doc-name
                          (get-in @extension-docs-registry [id :docs doc-name]))]
    (assoc descriptor :name doc-name)))

(defn extension-doc-content
  "Plain `:content` body (Markdown string) of a declared doc, or `nil`
   when the doc is unknown. Convenience over `(:content (extension-doc
   id name))`."
  [id doc-name]
  (:content (extension-doc id doc-name)))

(defn extension-doc-abstract
  "Return the `:abstract` field of a declared extension doc, or `nil`
   when the doc is unknown."
  [id doc-name]
  (:abstract (extension-doc id doc-name)))

(defn extension-doc-summary
  "Lightweight doc descriptor (no `:content`):
   `{:name :created-at :abstract :links :reflinks}`. Returns `nil`
   when the doc is unknown. Use this for catalog listings; pull the
   full body with `extension-doc-content` only when needed."
  [id doc-name]
  (when-let [descriptor (and id doc-name
                          (get-in @extension-docs-registry [id :docs doc-name]))]
    (-> descriptor
      (dissoc :content)
      (assoc :name doc-name))))

(defn extension-docs
  "With one arg, return a vector of doc summaries
   `[{:name :created-at :abstract :links :reflinks} ...]` for every
   doc declared by `id`. With no arg, return the full registry as
   `{<id-sym> [<summary> ...]}`. Sorted by doc name within each id so
   the catalog is deterministic."
  ([]
   (into {}
     (map (fn [[id {docs :docs}]]
            [id (mapv #(extension-doc-summary id %) (sort (keys docs)))]))
     @extension-docs-registry))
  ([id]
   (let [docs (get-in @extension-docs-registry [id :docs])]
     (mapv #(extension-doc-summary id %) (sort (keys docs))))))

(defn extension-doc-names
  "Plain sorted vector of doc names declared by `id`."
  [id]
  (vec (sort (keys (get-in @extension-docs-registry [id :docs])))))

(defn- merge-manifest-entry!
  "Merge one `[id {:nses [...] :docs {name <descriptor>}}]` into the
   registry. `:nses` are deduped (existing order preserved); `:docs`
   is a map merge with later entries winning per name. Reflinks are
   recomputed by `recompute-reflinks!` after every merge so a later
   jar's links can target an earlier jar's docs."
  [id entry]
  (swap! extension-docs-registry
    update id
    (fn [existing]
      (let [merged-nses (vec (distinct (concat (:nses existing) (:nses entry))))
            merged-docs (merge (or (:docs existing) {}) (or (:docs entry) {}))]
        {:nses merged-nses :docs merged-docs}))))

(defn- link-target
  "Return `[<target-id> <target-doc>]` for a cross-ext or same-ext
   doc link, or `nil` for url/file/external links. `from-id` is the
   id of the extension that authored the link — used to resolve
   same-ext refs that omit `:to-id`."
  [from-id link]
  (cond
    (and (symbol? (:to-id link)) (string? (:to-doc link)))
    [(:to-id link) (:to-doc link)]

    (and (nil? (:to-id link)) (string? (:to-doc link)))
    [from-id (:to-doc link)]

    :else nil))

(defn- recompute-reflinks!
  "Walk every doc's `:links` across the entire registry and rebuild
   the `:reflinks` vector on each target. Idempotent: produces the
   same registry shape regardless of merge order."
  []
  (swap! extension-docs-registry
    (fn [registry]
      (let [;; Strip any prior :reflinks so this pass starts clean.
            cleared (reduce-kv
                      (fn [acc id entry]
                        (assoc acc id
                          (update entry :docs
                            (fn [docs]
                              (reduce-kv (fn [d name descriptor]
                                           (assoc d name (assoc descriptor :reflinks [])))
                                {} docs)))))
                      {} registry)
            ;; Walk every authored link; route a {:from-id :from-doc
            ;; :context} reflink onto the target descriptor.
            with-reflinks
            (reduce-kv
              (fn [acc from-id entry]
                (reduce-kv
                  (fn [acc2 from-doc descriptor]
                    (reduce
                      (fn [acc3 link]
                        (if-let [[to-id to-doc] (link-target from-id link)]
                          (if (get-in acc3 [to-id :docs to-doc])
                            (update-in acc3 [to-id :docs to-doc :reflinks]
                              (fnil conj [])
                              (cond-> {:from-id  from-id
                                       :from-doc from-doc}
                                (string? (:context link))
                                (assoc :context (:context link))))
                            acc3)
                          acc3))
                      acc2 (:links descriptor)))
                  acc (:docs entry)))
              cleared cleared)]
        with-reflinks))))

(defn registered-extensions-summary
  "Pure data view of the docs registry: returns
   `{<id> {:nses [...] :docs {<name> <summary>}}}` for every loaded
   extension. Useful for snapshot tests and ad-hoc inspection."
  []
  (reduce-kv
    (fn [acc id entry]
      (assoc acc id
        {:nses (:nses entry)
         :docs (reduce-kv (fn [d name _] (assoc d name (extension-doc-summary id name)))
                 {} (:docs entry))}))
    {} @extension-docs-registry))

(defn discover-extensions!
  "Public entry point for vis-sdk's classpath auto-discovery.

   Runs the private `scan-extensions!` helper (which scans every
   `META-INF/vis-extension/vis.edn`, `require`s every namespace listed
   under each manifest's `:nses` key, and returns the merged parsed
   manifests) and then merges every loaded extension's inline docs
   into this namespace's docs registry. Returns the count of
   namespaces declared under `:nses` across the merged manifests.

   Most callers want this fn: it covers the docs registry plus the
   require side effect that drives every `register-extension!`,
   `register-channel!`, `register-cmd!`, `register-provider!`, and
   `register-backend!` registry as namespaces self-register at load
   time.

   Idempotent on both layers: the underlying scan memoizes the parsed
   manifests; the docs-registry merge is a deterministic re-merge of
   the same data."
  []
  (let [manifests (scan-extensions!)]
    (doseq [[id entry] manifests]
      (merge-manifest-entry! id entry))
    ;; All authored links are now in the registry; invert them into
    ;; reflinks so each target descriptor carries its inbound graph.
    (recompute-reflinks!)
    (count (mapcat :nses (vals manifests)))))

;; =============================================================================
;; CLI bridge -- the `vis extensions` parent
;;
;; Self-registers a top-level `extensions` command into commandline.base
;; whose subcommands are computed lazily from every command registered
;; with `:cmd/parent ["extensions"]`. Extensions populate this slot
;; through `:ext/cli` on `ext/extension` -- the `register-global!`
;; dispatcher above forwards each entry to `register-cmd!`.
;;
;; Lives here (not in commandline.base) because this facade is the
;; canonical home for everything extension-shaped. The `cmd` alias is
;; established at the top-of-ns require form.
;; =============================================================================

(register-cmd!
  {:cmd/name        "extensions"
   :cmd/doc         "Run an extension-provided CLI command."
   :cmd/usage       "vis extensions <cmd> [args…]"
   :cmd/subcommands #(registered-under ["extensions"])})

;; =============================================================================
;; Configuration — paths, version, logging bootstrap, provider presets,
;; svar-native data helpers, config I/O. Was: vis-runtime.config.
;;
;; This block lives in vis-sdk because every channel (TUI, Telegram, CLI)
;; needs `init!` / `tty-in` / `original-stdout` / `load-config` /
;; `provider-presets` to function, and channels depend on vis-sdk only.
;; =============================================================================

(def config-dir  (str (System/getProperty "user.home") "/.vis"))
(def config-path (str config-dir "/config.edn"))
(def db-path     (str config-dir "/vis.mdb"))
(def default-db-spec {:backend :sqlite :path db-path})

(def ^:private ^String log-path (str config-dir "/vis.log"))

(def tty-in  (delay (FileInputStream.  "/dev/tty")))
(def tty-out (delay ^java.io.OutputStream (FileOutputStream. "/dev/tty")))

(def ^java.io.PrintStream original-stdout System/out)

(defn init!
  "Redirect System/out and System/err to the log file. Lanterna uses
   tty-in / tty-out for terminal I/O. Call from the TUI entry point."
  []
  (clojure+.error/install!)
  (set! *print-level* 10)
  (set! *print-length* 100)
  (let [dir (io/file config-dir)]
    (when-not (.exists dir) (.mkdirs dir)))
  (let [raw-out    (FileOutputStream. log-path true)
        log-stream (java.io.PrintStream. raw-out true)]
    (System/setOut log-stream)
    (System/setErr log-stream))
  (alter-var-root #'*out* (constantly (io/writer log-path :append true)))
  (alter-var-root #'*err* (constantly (io/writer log-path :append true)))
  (tel/remove-handler! :default/console)
  (tel/add-handler! :file/vis
    (tel/handler:file {:path              log-path
                       :interval          :monthly
                       :max-file-size     4000000
                       :max-num-parts     8
                       :max-num-intervals 6}))
  (tel/call-on-shutdown! (fn [] (tel/stop-handlers!))))

(defn init-cli!
  "Logging init for non-TUI processes. Same redirects as init! but
   without the shutdown hook (CLI commands run to completion and exit)."
  []
  (clojure+.error/install!)
  (set! *print-level* 10)
  (set! *print-length* 100)
  (let [dir (io/file config-dir)]
    (when-not (.exists dir) (.mkdirs dir)))
  (let [raw-out    (FileOutputStream. log-path true)
        log-stream (java.io.PrintStream. raw-out true)]
    (System/setOut log-stream)
    (System/setErr log-stream))
  (alter-var-root #'*out* (constantly (io/writer log-path :append true)))
  (alter-var-root #'*err* (constantly (io/writer log-path :append true)))
  (tel/remove-handler! :default/console)
  (tel/add-handler! :file/vis
    (tel/handler:file {:path              log-path
                       :interval          :monthly
                       :max-file-size     4000000
                       :max-num-parts     8
                       :max-num-intervals 6})))

(defn shutdown!
  "Flush and stop all telemere handlers. Call after the TUI screen
   stops."
  []
  (tel/stop-handlers!))

;;; ── Provider presets ─────────────────────────────────────────────────────

(def ^:private base-providers
  [{:id :openai     :label "OpenAI"     :base-url "https://api.openai.com/v1"
    :default-models ["gpt-4o" "gpt-4o-mini" "o3-mini" "gpt-4-turbo" "gpt-4"]}
   {:id :github-models :label "GitHub Models" :base-url "https://models.github.ai/inference"
    :default-models ["openai/gpt-4o" "openai/gpt-4o-mini" "openai/o3-mini"
                     "meta/llama-4-scout-17b-16e-instruct"
                     "deepseek/DeepSeek-R1" "mistralai/mistral-small-2503"]}
   {:id :github-copilot :label "GitHub Copilot" :base-url "https://api.githubcopilot.com"
    :default-models ["gpt-4o" "gpt-4o-mini" "o3-mini" "gemini-2.0-flash-001"]}
   {:id :openrouter :label "OpenRouter" :base-url "https://openrouter.ai/api/v1"
    :default-models ["openai/gpt-4o" "google/gemini-2.0-flash-001"
                     "meta-llama/llama-3.1-70b-instruct"]}
   {:id :ollama     :label "Ollama"     :base-url "http://localhost:11434/v1"
    :default-models ["llama3.1" "mistral" "codellama" "phi3"]}])

(defn- blockether-provider []
  (let [be-key (System/getenv "BLOCKETHER_OPENAI_API_KEY")
        be-url (System/getenv "BLOCKETHER_OPENAI_BASE_URL")]
    (when (and be-key be-url)
      {:id :blockether
       :label "Blockether"
       :base-url be-url
       :api-key be-key
       :default-models ["glm-5-turbo" "glm-5.1" "gpt-5-mini" "gpt-4o"
                        "minimax-m2.5" "gemini-2.5-pro"]})))

(defn provider-presets
  "All known provider presets. Includes Blockether when BLOCKETHER_*
   env vars are set."
  []
  (if-let [be (blockether-provider)]
    (into [be] base-providers)
    base-providers))

(defn provider-template [pid]
  (some #(when (= (:id %) pid) %) (provider-presets)))

(defn display-label
  "Human-readable label for a provider id. Never persisted."
  [pid]
  (or (:label (provider-template pid))
    (some-> pid name str/capitalize)
    "Provider"))

(defn provider-base-url
  "Resolve base-url for a provider, falling back to the preset."
  [provider]
  (or (:base-url provider)
    (:base-url (provider-template (:id provider)))))

;;; ── Svar-native data helpers ────────────────────────────────────────────

(defn model-name
  "Extract the model name string from a model (string or `{:name str}`)."
  [model]
  (cond
    (string? model) model
    (map? model)    (:name model)
    :else           nil))

(defn ->svar-model
  "Coerce a model representation to svar-native `{:name str}`."
  [model]
  (when-let [n (some-> (model-name model) str str/trim not-empty)]
    {:name n}))

(defn ->svar-provider
  "Coerce a provider map to svar-native shape (`:id`, `:api-key`,
   `:base-url`, `:models`).

   When `:api-key` is nil, look the provider up in the SDK's provider
   registry and call its `:provider/get-token-fn` to resolve a usable
   token. Each provider implementation handles its own auth lifecycle
   (OAuth refresh, env-var fallback, …) so this fn stays
   provider-agnostic and never references a concrete provider ns by
   name."
  [provider]
  (let [pid       (:id provider)
        api-key   (:api-key provider)
        models    (->> (:models provider) (keep ->svar-model) vec)
        base-url  (provider-base-url provider)
        get-token-fn (when (nil? api-key)
                       (some-> (provider-by-id pid) :provider/get-token-fn))]
    (if get-token-fn
      (let [{:keys [token api-url]} (get-token-fn)]
        (cond-> {:id pid :models models :api-key token}
          (some? (or base-url api-url)) (assoc :base-url (or base-url api-url))))
      (cond-> {:id pid :models models}
        (some? api-key)  (assoc :api-key api-key)
        (some? base-url) (assoc :base-url base-url)))))

;;; ── Config I/O ──────────────────────────────────────────────────────────

(defn- blockether-env-config []
  (when-let [be (blockether-provider)]
    {:providers [(cond-> {:id       :blockether
                          :base-url (:base-url be)
                          :models   (mapv (fn [m] {:name m}) (:default-models be))}
                   (:api-key be) (assoc :api-key (:api-key be)))]}))

(defn load-config-raw
  "Load raw `config.edn` map (or nil on read/parse error)."
  []
  (let [f (io/file config-path)]
    (when (.exists f)
      (try
        (let [raw (edn/read-string (slurp f))]
          (when (map? raw) raw))
        (catch Exception _ nil)))))

(defn load-config
  "Load provider config in svar-native syntax. `~/.vis/config.edn`
   takes priority, falling back to BLOCKETHER_* env vars."
  []
  (or (some-> (load-config-raw)
        ((fn [raw] (when (seq (:providers raw)) raw))))
    (blockether-env-config)))

(defn save-config!
  "Persist svar-native config to `~/.vis/config.edn`."
  [config]
  (let [dir (io/file config-dir)]
    (when-not (.exists dir) (.mkdirs dir))
    (spit config-path (pr-str config))))

(defn resolve-config
  "Resolve provider config: explicit → `~/.vis/config.edn` → BLOCKETHER_* env.
   Throws when nothing is available."
  ([] (resolve-config nil))
  ([explicit-config]
   (or explicit-config
     (load-config)
     (throw (ex-info (str "No provider config. Create ~/.vis/config.edn "
                       "or set BLOCKETHER_OPENAI_API_KEY env var.")
              {})))))

(defn resolve-db-spec
  "Resolve DB spec: explicit → VIS_DB_PATH env → `:db-spec` from
   config.edn → default sqlite at `~/.vis/vis.mdb`."
  ([] (resolve-db-spec nil))
  ([explicit-db-spec]
   (or explicit-db-spec
     (when-let [env-path (System/getenv "VIS_DB_PATH")]
       {:backend :sqlite :path env-path})
     (:db-spec (load-config-raw))
     default-db-spec)))

;; =============================================================================
;; Active provider state — was: vis-runtime.providers
;;
;; The active provider config is mirrored from disk into the
;; `active-config` atom for fast reads. Every mutation goes through
;; `set-provider!` / `remove-provider!`, which write to disk AND
;; rebuild the global router AND reseat it on every cached
;; conversation env so long-lived envs (TUI session, Telegram bot)
;; stop talking to the previous model. The router-rebuild and
;; cached-env reseat are owned by vis-runtime; this fn reaches into
;; them through `requiring-resolve` so the storage facade and other
;; vis-sdk consumers don't drag in the iteration runtime.
;; =============================================================================

;; ---------------------------------------------------------------------------
(defonce ^:private active-config (atom nil))

(defn current-config
  "Return the current provider config. Loads from disk on first call."
  []
  (or @active-config
    (let [cfg (load-config)]
      (reset! active-config cfg)
      cfg)))

(defn active-provider
  "Return the first (primary) provider from config, or nil."
  []
  (first (:providers (current-config))))

(defn active-model
  "Return the primary model name string, or nil."
  []
  (some-> (active-provider) :models first model-name))

(defn provider-ids
  "Set of configured provider `:id` keywords."
  []
  (into #{} (map :id) (:providers (or (current-config) {:providers []}))))

(defn has-provider? [provider-id]
  (contains? (provider-ids) provider-id))

(defn reload-config! []
  (reset! active-config (load-config)))

;; =============================================================================
;; Re-exports from leaf modules (progress tracker + format helpers)
;;
;; Channels (TUI, CLI agent, Telegram) consume svar's per-iteration
;; chunks via `make-progress-tracker`. Format helpers are pure
;; transforms used by the TUI footer, CLI status output, etc. Both
;; live in their own namespaces; the SDK facade just re-publishes
;; the public names so existing callers keep working unchanged.
;; =============================================================================

(def make-progress-tracker progress/make-progress-tracker)

(def format-date     format/format-date)
(def format-clojure  format/format-clojure)
(def format-duration format/format-duration)


