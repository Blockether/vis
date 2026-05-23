(ns com.blockether.vis.internal.toggles
  "Process-wide feature-toggle registry.

   Replaces the two parallel toggle plumbings we had drifting apart:
     - hard-coded TUI booleans (`:show-thinking`, `:show-iterations`,
       `:show-silent`) wired into `state/default-settings`,
     - per-render `if (some-flag …) … else …` checks scattered through
       `internal/render.clj` and channel-tui's render layer.

   A toggle has stable metadata (`id`, label, description, default,
   owner) and a current ON/OFF value. Anyone \u2014 internal modules,
   extensions, channels \u2014 registers their toggles into the same
   registry; any caller flips a toggle through the same `set!`. The
   TUI settings dialog walks the registry to render the list, so
   adding a new toggle from an extension shows up in the user's UI
   without any TUI patch.

   Persistence is opt-in: `register-toggle!` accepts `:persist? true`
   and on `set!` the wrapper file writes `{:toggles {id bool}}` into
   `~/.vis/config.edn` via `vis.config/save-config!`. Hydration
   happens at process start (call `hydrate-from-config!` once after
   `config/load-config-raw`).

   Contract:
     - Toggle ids are namespaced keywords (`:vis/show-raw-code`,
       `:foundation-git/auto-commit`, ...).
     - `enabled?` is cheap (single atom deref + keyword lookup),
       called per-paint per-row by the render layer; do not turn it
       into a function-call indirection.
     - Defaults are immutable once registered. Re-registering the
       same id is allowed (idempotent boot path) and merges over
       prior metadata; the live VALUE in `state` is left alone so a
       user override survives a reload."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

;; =============================================================================
;; Specs
;; =============================================================================

(s/def :toggle/id           qualified-keyword?)
(s/def :toggle/label        (s/and string? #(not (str/blank? %))))
(s/def :toggle/description  string?)
(s/def :toggle/default      boolean?)
(s/def :toggle/owner        (s/or :internal #{:vis} :extension string?))
(s/def :toggle/since        string?)
(s/def :toggle/persist?     boolean?)
(s/def :toggle/group        keyword?)

(s/def :toggle/spec
  (s/keys :req-un [:toggle/id :toggle/label :toggle/default]
    :opt-un [:toggle/description :toggle/owner :toggle/since
             :toggle/persist? :toggle/group]))

;; =============================================================================
;; Registries
;; =============================================================================

(defonce ^:private
  ^{:doc "id -> normalized spec map. Keyed by `:id` so re-register
          (defonce reload, channel-restart, ext reload) is idempotent."}
  registry
  (atom {}))

(defonce ^:private
  ^{:doc "id -> current value (boolean). When an id is ABSENT from
          this map the resolver falls back to the registered default.
          That distinction matters for the `reset-to-default!` op:
          dissoc'ing here is NOT the same as `(set! id false)`."}
  state
  (atom {}))

(defonce ^:private
  ^{:doc "Vec of listener fns `(fn [{:id :old :new}])`. Each `set!`
          / `reset-to-default!` fans out so the TUI render thread,
          channels, and any background consumer can react. Listeners
          run on the calling thread; they MUST be cheap."}
  listeners
  (atom []))

;; =============================================================================
;; Registry ops
;; =============================================================================

(defn- normalize-spec
  "Coerce a caller spec into the canonical registry shape. Drops
   unknown keys so a future field added here doesn't bleed into
   listeners; missing optional fields get sane defaults."
  [{:keys [id label default description owner since persist? group]}]
  (cond-> {:id          id
           :label       (str label)
           :default     (boolean default)
           :owner       (or owner :vis)
           :persist?    (boolean persist?)}
    description (assoc :description description)
    since       (assoc :since since)
    group       (assoc :group group)))

(defn register-toggle!
  "Register one toggle. `spec` must satisfy `:toggle/spec`.

   Re-registering the same `:id` is idempotent: metadata MERGES, the
   live VALUE in `state` is preserved (user overrides survive reload).
   Returns the canonical registered spec."
  [spec]
  (when-not (s/valid? :toggle/spec spec)
    (throw (ex-info "Invalid toggle spec"
             {:type    :vis.toggles/invalid-spec
              :spec    spec
              :explain (s/explain-data :toggle/spec spec)})))
  (let [normalized (normalize-spec spec)
        id         (:id normalized)]
    (swap! registry assoc id normalized)
    normalized))

(defn register-toggles!
  "Convenience: register a sequence of specs in order, returns the
   vec of canonical specs."
  [specs]
  (mapv register-toggle! specs))

(defn registered-toggles
  "Vec of every registered toggle's normalized spec, in registration
   insertion order. Stable for the TUI settings dialog."
  []
  (vec (vals @registry)))

(defn toggle-spec
  "Lookup the registered spec for `id`, or nil."
  [id]
  (get @registry id))

;; =============================================================================
;; State ops
;; =============================================================================

(defn- notify!
  [event]
  (doseq [f @listeners]
    (try (f event) (catch Throwable _ nil))))

(defn enabled?
  "Resolve the current ON/OFF value for `id`. Lookup order:
     1. live override in `state`,
     2. registered default,
     3. `false` if the toggle isn't registered (fail-closed).

   No allocation, no `try` \u2014 hot path."
  [id]
  (let [s @state]
    (if (contains? s id)
      (boolean (get s id))
      (boolean (:default (get @registry id))))))

(defn set-enabled!
  "Flip `id` to `value` (cast to boolean). Notifies listeners. Returns
   the new boolean.

   Does NOT auto-persist \u2014 the persistence side runs via
   `hydrate-from-config!` + `snapshot` so the host owns where bytes
   land. The TUI settings dispatcher calls `set-enabled!` and then writes
   `(snapshot)` into `~/.vis/config.edn` itself, same way it already
   handles `:tui-settings`."
  [id value]
  (let [v   (boolean value)
        old (enabled? id)]
    (swap! state assoc id v)
    (when (not= old v)
      (notify! {:id id :old old :new v}))
    v))

(defn reset-to-default!
  "Drop the user override for `id` so resolution falls back to the
   registered default. Notifies listeners when the effective value
   changes."
  [id]
  (let [old (enabled? id)]
    (swap! state dissoc id)
    (let [new (enabled? id)]
      (when (not= old new)
        (notify! {:id id :old old :new new}))
      new)))

(defn snapshot
  "Return a map `{id bool}` of EVERY toggle's effective value,
   intended for persistence. Skips toggles whose `:persist?` is
   false. Only registered ids are included; orphans from a
   previously-installed extension are dropped."
  []
  (let [reg @registry
        s   @state]
    (reduce-kv
      (fn [acc id spec]
        (if (:persist? spec)
          (assoc acc id (if (contains? s id)
                          (boolean (get s id))
                          (boolean (:default spec))))
          acc))
      {}
      reg)))

(defn hydrate-from-config!
  "Bulk-apply persisted toggle values from `(:toggles config-map)`.
   Silently skips ids not in the registry so a stale config file
   from a previous install can't break boot. Notifies listeners for
   every value that actually changed."
  [config-map]
  (let [persisted (some-> config-map :toggles)]
    (when (map? persisted)
      (let [reg @registry]
        (doseq [[id v] persisted
                :when  (contains? reg id)]
          (set-enabled! id v))))))

;; =============================================================================
;; Listener ops
;; =============================================================================

(defn add-listener!
  "Register a no-arg-or-event listener fn. Returns a `dispose!` thunk
   the caller invokes when their consumer goes away (channel close,
   extension reload, ...)."
  [f]
  (when (fn? f)
    (let [key (Object.)
          entry (with-meta f {::key key})]
      (swap! listeners conj entry)
      (fn dispose! []
        (swap! listeners
          (fn [xs]
            (vec (remove #(identical? key (-> % meta ::key)) xs))))))))

;; =============================================================================
;; Reset (dev / test helper)
;; =============================================================================

(defn clear-state!
  "Wipe live overrides and listeners. Registry is untouched. Used by
   tests; production callers should prefer `reset-to-default!`."
  []
  (reset! state {})
  (reset! listeners [])
  nil)

;; =============================================================================
;; Host-owned canonical toggles
;;
;; Internal toggles ship registered at load time so the TUI settings
;; dialog never paints "the host has nothing to flip" before an
;; extension lands its own toggles. `defonce` keeps re-loads (clj-reload
;; / `:reload`) idempotent: the live registry stays intact and any
;; user override in `state` survives.
;; =============================================================================

(defonce ^{:doc "Sentinel that records whether the canonical internal toggles
          have been installed (idempotent)."
          :clj-kondo/ignore [:clojure-lsp/unused-public-var :unused-private-var]}
  host-toggles-installed?
  (do
    (register-toggle!
      {:id          :vis/show-raw-code
       :label       "Show raw code rows"
       :description (str "When ON the bubble paints the literal source"
                      " of every form (def, defn, bare tool calls, ...)"
                      " instead of collapsing it. Off by default — the"
                      " channel preview + DEF SINK already cover what"
                      " the model wrote, and the source rows push real"
                      " answers off-screen on long traces.")
       :default     false
       :owner       :vis
       :group       :diagnostics
       :persist?    true})

    (register-toggle!
      {:id          :vis/show-tool-results
       :label       "Show tool result preview"
       :description (str "When ON the bubble paints the channel-render IR"
                      " of every tool call (LS / CAT / RG / clj-EVAL / …)"
                      " inside the form's result pane. Off hides the"
                      " preview while keeping the model's data intact;"
                      " use it when the traces are correct but you want"
                      " a quieter transcript.")
       :default     true
       :owner       :vis
       :group       :diagnostics
       :persist?    true})
    true))
