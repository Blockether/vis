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
(s/def :toggle/default      any?)            ;; cross-validated against :type below
(s/def :toggle/owner        (s/or :internal #{:vis} :extension string?))
(s/def :toggle/since        string?)
(s/def :toggle/persist?     boolean?)
(s/def :toggle/group        keyword?)
(s/def :toggle/type         #{:boolean :enum})
(s/def :toggle/choices      (s/coll-of any? :min-count 1))

(s/def :toggle/spec
  (s/and
    (s/keys :req-un [:toggle/id :toggle/label :toggle/default]
      :opt-un [:toggle/description :toggle/owner :toggle/since
               :toggle/persist? :toggle/group :toggle/type :toggle/choices])
    (fn cross-validate [{:keys [type choices default]}]
      (case (or type :boolean)
        :boolean (boolean? default)
        :enum    (and (sequential? choices)
                   (some? default)
                   (contains? (set choices) default))))))

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
   listeners; missing optional fields get sane defaults.

   Two kinds of toggles share the same registry:
     `:boolean` (default) -- simple ON/OFF.
     `:enum`              -- a closed set of named values
                             (e.g. `:vis/message-meta` cycles
                             `:off -> :short`).
   `:type` and `:choices` ride on the normalized spec so the dialog
   row can pick its rendering strategy (toggle vs. cycle) without
   re-deriving anything."
  [{:keys [id label default description owner since persist? group type choices]}]
  (let [t (or type :boolean)]
    (cond-> {:id       id
             :label    (str label)
             :type     t
             :default  (case t
                         :boolean (boolean default)
                         :enum    default)
             :owner    (or owner :vis)
             :persist? (boolean persist?)}
      description (assoc :description description)
      since       (assoc :since since)
      group       (assoc :group group)
      (= :enum t) (assoc :choices (vec choices)))))

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
    (if (contains? s id)
      (get s id)
      (:default (get @registry id)))))

(defn enabled?
  "Boolean cast of `(value-of id)`. Fail-closed: returns `false` when
   `id` is not registered. Hot-path — one atom deref."
  [id]
  (boolean (value-of id)))

(defn choices-of
  "Vec of legal choices for an `:enum` toggle. Empty when `id` is
   unregistered or registered as `:boolean`."
  [id]
  (or (:choices (get @registry id)) []))

(defn type-of
  "`:boolean` / `:enum` / nil for unknown."
  [id]
  (:type (get @registry id)))

(defn set-value!
  "Set `id` to `value` and notify listeners. Returns the new value.
   Validation matches the registered `:type`:
     `:boolean` — coerce to boolean.
     `:enum`    — must be one of `:choices`; otherwise throws
                   `:vis.toggles/invalid-value` so the bug surfaces
                   at the call site instead of later in render."
  [id value]
  (let [spec (get @registry id)
        v    (case (or (:type spec) :boolean)
               :boolean (boolean value)
               :enum    (let [allowed (set (:choices spec))]
                          (when-not (contains? allowed value)
                            (throw (ex-info "Toggle value is not one of the registered :choices"
                                     {:type    :vis.toggles/invalid-value
                                      :id      id
                                      :value   value
                                      :choices (:choices spec)})))
                          value))
        old  (value-of id)]
    (swap! state assoc id v)
    (when (not= old v)
      (notify! {:id id :old old :new v}))
    v))

(defn cycle-value!
  "Advance an `:enum` toggle one step through its registered
   `:choices`. Wraps at the end. Throws on boolean toggles."
  [id]
  (let [spec    (get @registry id)
        choices (vec (:choices spec))]
    (when-not (and spec (= :enum (:type spec)))
      (throw (ex-info "cycle-value! requires an :enum toggle"
               {:type :vis.toggles/wrong-kind :id id :got-type (:type spec)})))
    (when-not (seq choices)
      (throw (ex-info "Enum toggle has no choices"
               {:type :vis.toggles/invalid-spec :id id})))
    (let [current (value-of id)
          idx     (.indexOf ^java.util.List choices current)
          next-v  (nth choices (mod (inc (if (neg? idx) -1 idx)) (count choices)))]
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
      (when (not= old new)
        (notify! {:id id :old old :new new}))
      new)))

(defn snapshot
  "Return a map `{id value}` of EVERY persistable toggle's effective
   value, intended for serialisation. Skips toggles whose
   `:persist?` is false. Boolean toggles are coerced to boolean;
   enum toggles surface their raw choice value. Orphans from a
   previously-installed extension are dropped."
  []
  (let [reg @registry
        s   @state]
    (reduce-kv
      (fn [acc id spec]
        (if (:persist? spec)
          (let [v (if (contains? s id) (get s id) (:default spec))
                v (case (:type spec)
                    :boolean (boolean v)
                    :enum    v
                    (boolean v))]
            (assoc acc id v))
          acc))
      {}
      reg)))

(defn hydrate-from-config!
  "Bulk-apply persisted toggle values from `(:toggles config-map)`.
   Silently skips ids not in the registry so a stale config file
   from a previous install can't break boot. Routes through
   `set-value!` so enum entries get validated; individual invalid
   values are dropped (logged via the listener) instead of aborting
   the whole hydrate."
  [config-map]
  (let [persisted (some-> config-map :toggles)]
    (when (map? persisted)
      (let [reg @registry]
        (doseq [[id v] persisted
                :when  (contains? reg id)]
          (try (set-value! id v)
            (catch clojure.lang.ExceptionInfo _ nil)))))))

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

    ;; --- TUI display toggles (migrated from `:tui-settings`) -------------

    (register-toggle!
      {:id :vis/show-thinking :label "Show model thinking"
       :description (str "Stream reasoning_content / thinking deltas inside"
                      " each iteration bubble (z.ai GLM, Copilot Claude,"
                      " Codex reasoning summaries, Anthropic thinking)."
                      " Disable for a quieter transcript.")
       :default true :owner :vis :group :tui-display :persist? true})

    (register-toggle!
      {:id :vis/show-iterations :label "Show full execution trace"
       :description "Blocks, eval results, errors — the whole iteration history."
       :default true :owner :vis :group :tui-display :persist? true})

    (register-toggle!
      {:id :vis/show-silent :label "Show silent system calls"
       :description "Include successful :vis/silent forms (engine/system bookkeeping) in traces. Default ON — engine calls ARE your reasoning trace, not noise; show them."
       :default true :owner :vis :group :tui-display :persist? true})

    (register-toggle!
      {:id :vis/show-timestamps :label "Show per-message timestamps"
       :description "Date + time next to every 'You' / 'Vis' label."
       :default false :owner :vis :group :tui-display :persist? true})

    (register-toggle!
      {:id :vis/mouse-selection-copy :label "Mouse selection auto-copy"
       :description "Drag-select visible text; copied automatically on mouse release."
       :default true :owner :vis :group :tui-display :persist? true})

    (register-toggle!
      {:id :voice/respond? :label "Voice respond to answers"
       :description "Speak the final answer aloud via the foundation-voice extension."
       :default false :owner :vis :group :tui-display :persist? true})

    (register-toggle!
      {:id :vis/reasoning-level :label "Reasoning effort"
       :description "Reasoning budget hint passed to reasoning-capable models."
       :type :enum :choices [:quick :balanced :deep]
       :default :balanced :owner :vis :group :provider :persist? true})

    (register-toggle!
      {:id :openai-codex/verbosity :label "OpenAI Codex verbosity"
       :description "Provider-specific verbosity knob for the OpenAI Codex backend."
       :type :enum :choices [:low :medium :high]
       :default :low :owner :vis :group :provider :persist? true})

    true))
