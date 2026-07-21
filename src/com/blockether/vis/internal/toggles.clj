(ns com.blockether.vis.internal.toggles
  "Process-wide feature-toggle registry.

   Replaces the two parallel toggle plumbings we had drifting apart:
     - hard-coded TUI booleans (`:show-thinking`, `:show-iterations`,
       `:show-silent`) wired into `state/default-settings`,
     - per-render `if (some-flag …) … else …` checks scattered through
       `internal/render.clj` and channel-tui's render layer.

   A toggle has stable metadata (`id`, label, description, default,
   owner) and a current ON/OFF value. Anyone — internal modules,
   extensions, channels — registers their toggles into the same
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
     - Toggle ids are namespaced keywords (`:vis/show-thinking`,
       `:foundation-git/auto-commit`, ...).
     - `enabled?` is cheap (single atom deref + keyword lookup),
       called per-paint per-row by the render layer; do not turn it
       into a function-call indirection.
     - Defaults are immutable once registered. Re-registering the
       same id is allowed (idempotent boot path) and merges over
       prior metadata; the live VALUE in `state` is left alone so a
       user override survives a reload."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;; =============================================================================
;; Specs
;; =============================================================================

(s/def :toggle/id qualified-keyword?)

(s/def :toggle/label (s/and string? #(not (str/blank? %))))

(s/def :toggle/description string?)

(s/def :toggle/default any?)            ;; cross-validated against :type below

(s/def :toggle/owner
  (s/or :internal #{:vis}
        :extension string?))

(s/def :toggle/since string?)

(s/def :toggle/persist? boolean?)

(s/def :toggle/group keyword?)

(s/def :toggle/type #{:boolean :enum})

(s/def :toggle/choices (s/and (s/coll-of any?) seq))

(s/def :toggle/channels (s/and (s/coll-of keyword?) seq))

(s/def :toggle/visible-fn ifn?) ;; () -> bool; hides irrelevant toggles from settings UIs

(s/def :toggle/spec
  (s/and (s/keys :req-un [:toggle/id :toggle/label :toggle/default]
                 :opt-un [:toggle/description :toggle/owner :toggle/since :toggle/persist?
                          :toggle/group :toggle/type :toggle/choices :toggle/visible-fn
                          :toggle/channels])
         (fn cross-validate [{:keys [type choices default]}]
           (case (or type :boolean)
             :boolean
             (boolean? default)

             :enum
             (and (sequential? choices) (some? default) (contains? (set choices) default))))))

;; =============================================================================
;; Registries
;; =============================================================================

(defonce
  ^:private
  ^{:doc
    "id -> normalized spec map. Keyed by `:id` so re-register
          (defonce reload, channel-restart, ext reload) is idempotent."}
  registry
  (atom {}))

(defonce
  ^:private
  ^{:doc
    "id -> current value (boolean). When an id is ABSENT from
          this map the resolver falls back to the registered default.
          That distinction matters for the `reset-to-default!` op:
          dissoc'ing here is NOT the same as `(set! id false)`."}
  state
  (atom {}))

(defonce
  ^:private
  ^{:doc
    "Vec of listener fns `(fn [{:id :old :new}])`. Each `set!`
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
   listeners; missing optional fields get sane defaults.

   Two kinds of toggles share the same registry:
     `:boolean` (default) -- simple ON/OFF.
     `:enum`              -- a closed set of named values
                             (e.g. `:vis/reasoning-level` cycles
                             `:quick -> :balanced -> :deep`).
   `:type` and `:choices` ride on the normalized spec so the dialog
   row can pick its rendering strategy (toggle vs. cycle) without
   re-deriving anything."
  [{:keys [id label default description owner since persist? group type choices visible-fn channels
           settings?]}]
  (let [t (or type :boolean)]
    (cond->
      {:id id
       :label (str label)
       :type t
       :default (case t
                  :boolean
                  (boolean default)

                  :enum
                  default)
       :owner (or owner :vis)
       :persist? (boolean persist?)
       ;; `:settings? false` keeps a toggle registered/persisted but OUT
       ;; of every channel's Settings dialog (it has its own control,
       ;; e.g. reasoning-effort on Ctrl+R). Default true = shown.
       :settings? (not (false? settings?))}
      description
      (assoc :description description)

      since
      (assoc :since since)

      group
      (assoc :group group)

      visible-fn
      (assoc :visible-fn visible-fn)

      ;; `:channels` (set of channel keywords) scopes a toggle to specific
      ;; channels' settings UIs. Absent = channel-neutral (shown everywhere).
      (seq channels)
      (assoc :channels (set channels))

      (= :enum t)
      (assoc :choices (vec choices)))))

(defn register-toggle!
  "Register one toggle. `spec` must satisfy `:toggle/spec`.

   Re-registering the same `:id` is idempotent: metadata MERGES, the
   live VALUE in `state` is preserved (user overrides survive reload).
   Returns the canonical registered spec."
  [spec]
  (when-not (s/valid? :toggle/spec spec)
    (throw (ex-info "Invalid toggle spec"
                    {:type :vis.toggles/invalid-spec
                     :spec spec
                     :explain (s/explain-data :toggle/spec spec)})))
  (let
    [normalized
     (normalize-spec spec)

     id
     (:id normalized)]

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

(defn toggle-visible?
  "True when a toggle should appear in a settings UI: no `:visible-fn`
   means always visible; a throwing predicate fails OPEN (shown) so a
   broken predicate can never hide a control the user needs."
  [spec]
  (if-let [f (:visible-fn spec)]
    (try (boolean (f)) (catch Throwable _ true))
    true))

(defn visible-toggles
  "`registered-toggles` filtered to what settings UIs should SHOW —
   provider-specific knobs declare a `:visible-fn` so e.g. the OpenAI
   Codex verbosity cycle only appears when a Codex provider is actually
   configured, and `:settings? false` toggles (e.g. reasoning-effort, which
   has its own Ctrl+R control) stay out of the Settings dialog entirely.
   State ops always work on the FULL registry; visibility is a presentation
   concern only."
  []
  (filterv #(and (not (false? (:settings? %))) (toggle-visible? %)) (registered-toggles)))

(defn toggle-for-channel?
  "True when a toggle should appear in `channel`'s settings UI. A toggle
   with no `:channels` set is channel-neutral (shown everywhere); one with
   a `:channels` set shows only in the listed channels. Keeps a channel's
   Settings free of OTHER channels' controls (e.g. the web has no use for
   the TUI's mouse-selection-copy or transcript-display toggles)."
  [channel spec]
  (let [chans (:channels spec)]
    (or (nil? chans) (contains? chans channel))))

(defn toggles-for-channel
  "`visible-toggles` further scoped to `channel` via `toggle-for-channel?`.
   Channels render THIS instead of `visible-toggles` so each Settings UI
   only shows controls it actually honours."
  [channel]
  (filterv #(toggle-for-channel? channel %) (visible-toggles)))

(defn toggle-spec "Lookup the registered spec for `id`, or nil." [id] (get @registry id))

;; =============================================================================
;; State ops
;; =============================================================================

(defn- notify!
  [event]
  (doseq [f @listeners]
    (try (f event) (catch Throwable _ nil))))

(defn value-of
  "Resolve the live value for `id`. Lookup order:
     1. live override in `state`,
     2. registered default,
     3. `nil` if the toggle isn't registered.

   Returns the raw value (boolean for `:boolean` toggles, any value
   from `:choices` for `:enum` toggles). `enabled?` is the
   boolean-cast convenience for the common boolean path."
  [id]
  (let [s @state]
    (if (contains? s id) (get s id) (:default (get @registry id)))))

(def ^:dynamic *forced-on*
  "Toggle ids forced ON for the current dynamic scope, regardless of the global
   value. `sub_loop` children bind this (Clojure binding-conveyance carries it
   into `parallel` futures) so a dispatched agent always has the shell + harness
   layers, even when the user left them OFF globally. RUNTIME gating only —
   `value-of` (settings UI, persistence) stays the global truth."
  #{})

(defn enabled?
  "Boolean cast of `(value-of id)`, OR forced ON in the current dynamic scope
   (`*forced-on*`). Fail-closed: returns `false` when `id` is not registered and
   not forced. Hot-path — one set lookup + one atom deref."
  [id]
  (or (contains? *forced-on* id) (boolean (value-of id))))

(defn choices-of
  "Vec of legal choices for an `:enum` toggle. Empty when `id` is
   unregistered or registered as `:boolean`."
  [id]
  (or (:choices (get @registry id)) []))

(defn type-of "`:boolean` / `:enum` / nil for unknown." [id] (:type (get @registry id)))

(defn set-value!
  "Set `id` to `value` and notify listeners. Returns the new value.
   Validation matches the registered `:type`:
     `:boolean` — coerce to boolean.
     `:enum`    — must be one of `:choices`; otherwise throws
                   `:vis.toggles/invalid-value` so the bug surfaces
                   at the call site instead of later in render."
  [id value]
  (let
    [spec
     (get @registry id)

     v
     (case (or (:type spec) :boolean)
       :boolean
       (boolean value)

       :enum
       (let [allowed (set (:choices spec))]
         (when-not (contains? allowed value)
           (throw
             (ex-info
               "Toggle value is not one of the registered :choices"
               {:type :vis.toggles/invalid-value :id id :value value :choices (:choices spec)})))
         value))

     old
     (value-of id)]

    (swap! state assoc id v)
    (when (not= old v) (notify! {:id id :old old :new v}))
    v))

(defn cycle-value!
  "Advance an `:enum` toggle one step through its registered
   `:choices`. Wraps at the end. Throws on boolean toggles."
  [id]
  (let
    [spec
     (get @registry id)

     choices
     (vec (:choices spec))]

    (when-not (and spec (= :enum (:type spec)))
      (throw (ex-info "cycle-value! requires an :enum toggle"
                      {:type :vis.toggles/wrong-kind :id id :got-type (:type spec)})))
    (when-not (seq choices)
      (throw (ex-info "Enum toggle has no choices" {:type :vis.toggles/invalid-spec :id id})))
    (let
      [current
       (value-of id)

       idx
       (.indexOf ^java.util.List choices current)

       next-v
       (let
         [idx
          (long idx)

          n
          (long (count choices))]

         (nth choices (mod (inc (if (neg? idx) -1 idx)) n)))]

      (set-value! id next-v))))

(defn set-enabled!
  "Boolean alias of `set-value!` for the TUI dialog — keeps the
   common toggle-flip call sites readable. Refuses `:enum` toggles
   so an accidental boolean-flip on a multi-value toggle surfaces
   loudly; use `cycle-value!` / `set-value!` for those."
  [id value]
  (let [spec (get @registry id)]
    (when (and spec (= :enum (:type spec)))
      (throw (ex-info "set-enabled! is boolean-only; use cycle-value! / set-value! for enum toggles"
                      {:type :vis.toggles/wrong-kind :id id :got-type (:type spec)}))))
  (set-value! id (boolean value)))

(defn reset-to-default!
  "Drop the user override for `id` so resolution falls back to the
   registered default. Notifies listeners when the effective value
   changes."
  [id]
  (let [old (value-of id)]
    (swap! state dissoc id)
    (let [new (value-of id)]
      (when (not= old new) (notify! {:id id :old old :new new}))
      new)))

(defn snapshot
  "Return a map `{id value}` of EVERY persistable toggle's effective
   value, intended for serialisation. Skips toggles whose
   `:persist?` is false. Boolean toggles are coerced to boolean;
   enum toggles surface their raw choice value. Orphans from a
   previously-installed extension are dropped."
  []
  (let
    [reg
     @registry

     s
     @state]

    (reduce-kv (fn [acc id spec]
                 (if (:persist? spec)
                   (let
                     [v
                      (if (contains? s id) (get s id) (:default spec))

                      v
                      (case (:type spec)
                        :boolean
                        (boolean v)

                        :enum
                        v

                        (boolean v))]

                     (assoc acc id v))
                   acc))
               {}
               reg)))

(defn hydrate-from-config!
  "Bulk-apply persisted toggle values from `(:toggles config-map)`. Silently
   skips ids not in the registry so a stale config file from a
   previous install can't break boot. Routes through `set-value!`
   so enum entries get validated; individual invalid values are
   dropped (logged via the listener) instead of aborting the whole
   hydrate."
  [config-map]
  (let
    [persisted (some-> config-map
                       :toggles)]
    (when (map? persisted)
      (let [reg @registry]
        (doseq
          [[id v] persisted
           :when (contains? reg id)]

          (try (set-value! id v) (catch clojure.lang.ExceptionInfo _ nil)))))))

;; =============================================================================
;; Listener ops
;; =============================================================================

(defn add-listener!
  "Register a no-arg-or-event listener fn. Returns a `dispose!` thunk
   the caller invokes when their consumer goes away (channel close,
   extension reload, ...)."
  [f]
  (when (fn? f)
    (let
      [key
       (Object.)

       entry
       (with-meta f {::key key})]

      (swap! listeners conj entry)
      (fn dispose! []
        (swap! listeners (fn [xs]
                           (vec (remove #(identical? key
                                                     (-> %
                                                         meta
                                                         ::key))
                                  xs))))))))

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

(defonce
  ^{:doc
    "Sentinel that records whether the canonical internal toggles
          have been installed (idempotent)."
    :clj-kondo/ignore [:clojure-lsp/unused-public-var :unused-private-var]}
  host-toggles-installed?
  (do
    ;; NOTE: `:vis/show-raw-code` was retired — the TUI now renders the model's
    ;; raw `:code` unconditionally, the SAME canonical contract as web's
    ;; `block-code`. There is no longer a gate that can hide the source rail.
    ;; --- TUI display toggles (migrated from `:tui-settings`) -------------
    ;; NOTE: the display gates `:vis/show-thinking`, `:vis/show-iterations`,
    ;; `:vis/show-silent`, and `:vis/show-timestamps` were retired — thinking,
    ;; the full execution trace, silent system calls, and timestamps are now
    ;; ALWAYS shown in both channels (same call as `:vis/show-raw-code`: the
    ;; trace IS the transcript, nothing to hide). The settings projection and
    ;; web `role-time` hardcode these on.
    (register-toggle! {:id :vis/mouse-selection-copy
                       :label "Mouse selection auto-copy"
                       :description
                       "Drag-select visible text; copied automatically on mouse release."
                       ;; ALWAYS ON — no longer a user-facing setting. Kept registered so the
                       ;; screen consumer + CLI overrides still resolve it, but `:settings? false`
                       ;; keeps it out of every Settings dialog.
                       :default true
                       :settings? false
                       :owner :vis
                       :group :tui-display
                       :channels #{:tui}
                       :persist? true})
    (register-toggle!
      {:id :network/enabled
       :label "Network access (Python sandbox)"
       :description
       (str
         "Let the Python sandbox open sockets (urllib/requests/socket). "
         "ALWAYS ON — the sandbox always has host socket access. "
         "Host policy in vis.yml network: is a best-effort GUARDRAIL for "
         "cooperative code (not adversary-proof): allowed-domains: [\"example.com\"] "
         "(empty or [\"*\"] = allow all), denied-domains: [...] on top of the "
         "cloud-metadata SSRF defaults, and rules: [{host: api.example.com, "
         "access: read-only, allow: [{method: POST, path: /v1/**}]}] to allow "
         "per-host HTTP verbs + paths (preset read-only = GET/HEAD/OPTIONS; unlisted "
         "hosts unrestricted). "
         "When the OS jail is on (shell: {jail: true}) these SAME domain+verb rules are "
         "enforced for SHELL children (curl/wget/scripts): the jail walls the child to a "
         "loopback egress proxy (net-off-except-proxy) that applies them — real "
         "containment, not a hint. HTTPS gets host allow/deny (verb needs future MITM); "
         "plain HTTP gets full verb+path. Opt out per session with :shell {:jail {:proxy false}}.")
       ;; A host capability, not a display concern. ON by default and out of the
       ;; Settings dialog (`:settings? false`) — the Python sandbox is always networked.
       :default true
       :settings? false
       :owner :vis
       :group :capabilities
       :persist? true})
    (register-toggle! {:id :vis/reasoning-level
                       :label "Reasoning effort"
                       :description "Reasoning budget hint passed to reasoning-capable models."
                       :type :enum
                       :choices [:quick :balanced :deep]
                       ;; Lives on its OWN control (TUI Ctrl+R, footer), not the Settings
                       ;; dialog — `:settings? false` keeps it registered + persisted but out
                       ;; of every channel's Settings list.
                       :settings? false
                       :default :balanced
                       :owner :vis
                       :group :provider
                       :persist? true})
    ;; NOTE: provider-specific knobs (e.g. :openai-codex/verbosity)
    ;; are registered by their PROVIDER EXTENSIONS, not here — a knob
    ;; belongs next to the backend it tunes, and its `:visible-fn`
    ;; keeps it out of Settings until that provider is configured.
    true))
