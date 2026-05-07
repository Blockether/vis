(ns com.blockether.vis.internal.proof
  "Canonical internal proof-domain model.

   This namespace owns proof-facing shape, canonical evidence references,
   lifecycle truth, evidence clauses, bundles, attestations, resolution records,
   and audit result specs. Specs validate data shape at boundaries; runtime and
   persistence still own existence, same-conversation scope, event ordering,
   extraction, guard evaluation, and write transactions."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]))

;; -----------------------------------------------------------------------------
;; Scalar predicates
;; -----------------------------------------------------------------------------

(defn non-blank-string?
  [x]
  (and (string? x) (not (str/blank? x))))

(defn uuid-string?
  [x]
  (cond
    (uuid? x) true
    (string? x) (try
                  (java.util.UUID/fromString x)
                  true
                  (catch IllegalArgumentException _
                    false))
    :else false))

(def id-prefix-pattern #"(?i)[0-9a-f]{8}")
(def tool-id-pattern #"[A-Za-z0-9_.:-]+")

(defn id-prefix8?
  [x]
  (boolean (and (string? x) (re-matches id-prefix-pattern x))))

;; -----------------------------------------------------------------------------
;; Canonical provenance/evidence refs
;; -----------------------------------------------------------------------------

(def canonical-ref-pattern
  (re-pattern
    (str "(?i)^(?:conversation/([0-9a-f]{8})/)?"
      "turn/([0-9a-f]{8})/"
      "iteration/([1-9][0-9]*)/"
      "block/([1-9][0-9]*)"
      "(?:/(tool/([A-Za-z0-9_.:-]+)|error))?$")))

(defn canonical-ref?
  "True when `s` exactly matches the canonical proof-facing ref grammar."
  [s]
  (boolean (and (string? s) (re-matches canonical-ref-pattern s))))

(defn parse-ref
  "Parse a canonical proof-facing provenance ref into data.

   Returns nil for non-canonical refs. Compact display aliases such as `i4.2`,
   `i4.2/tool`, `E1`, and `G1` are intentionally rejected."
  [s]
  (when-let [[_ conversation-prefix turn-prefix iteration block child tool-id]
             (and (string? s) (re-matches canonical-ref-pattern s))]
    (cond-> {:scope       (if conversation-prefix :conversation :turn)
             :turn-prefix (str/lower-case turn-prefix)
             :iteration   (parse-long iteration)
             :block       (parse-long block)}
      conversation-prefix (assoc :conversation-prefix (str/lower-case conversation-prefix))
      child (assoc :child (if (= "error" (str/lower-case child))
                            {:kind :error}
                            {:kind :tool :op tool-id})))))

(defn- required-prefix!
  [k v]
  (when-not (id-prefix8? v)
    (throw (ex-info (str "Expected " (name k) " to be an 8-character hex prefix")
             {:key k :value v})))
  (str/lower-case v))

(defn- positive-int!
  [k v]
  (when-not (pos-int? v)
    (throw (ex-info (str "Expected " (name k) " to be a positive integer")
             {:key k :value v})))
  v)

(defn- tool-id!
  [tool-id]
  (when-not (and (string? tool-id) (re-matches tool-id-pattern tool-id))
    (throw (ex-info "Tool id must be one slash-free path segment"
             {:tool-id tool-id})))
  tool-id)

(defn format-ref
  "Format canonical proof-facing provenance reference data.

   Required keys: `:turn-prefix`, `:iteration`, `:block`.
   Optional keys: `:conversation-prefix`, `:child`.
   Child forms: `{:kind :tool :op \"bash\"}` or `{:kind :error}`."
  [{:keys [conversation-prefix turn-prefix iteration block child]}]
  (let [turn-prefix (required-prefix! :turn-prefix turn-prefix)
        iteration   (positive-int! :iteration iteration)
        block       (positive-int! :block block)
        base        (str (when conversation-prefix
                           (str "conversation/" (required-prefix! :conversation-prefix conversation-prefix) "/"))
                      "turn/" turn-prefix "/iteration/" iteration "/block/" block)]
    (case (:kind child)
      nil base
      :error (str base "/error")
      :tool  (str base "/tool/" (tool-id! (or (:op child) (:tool-id child))))
      (throw (ex-info "Unsupported provenance child kind" {:child child})))))

(defn display-ref
  "Return display data for a canonical ref. Display aliases are never proof refs."
  [canonical-ref]
  (when-let [{:keys [turn-prefix iteration block child] :as parsed} (parse-ref canonical-ref)]
    (let [child-label (case (:kind child)
                        nil nil
                        :error "error"
                        :tool (:op child))
          label       (str "T" turn-prefix " · i" iteration "." block
                        (when child-label (str " · " child-label)))
          short       (str "i" iteration "." block
                        (when child-label (str "/" child-label)))]
      {:canonical (format-ref parsed)
       :label     label
       :short     short})))

(defn display-provenance
  "Display helper for a provenance map containing at least `:ref`.
   Returns nil when the ref is not canonical."
  [{:keys [ref op status duration-ms parent-ref]}]
  (when-let [display (display-ref ref)]
    (cond-> (assoc display :op op :status status)
      duration-ms (assoc :duration-ms duration-ms)
      parent-ref  (assoc :parent-ref parent-ref))))

;; -----------------------------------------------------------------------------
;; Lifecycle truth
;; -----------------------------------------------------------------------------

(def terminal-statuses #{:done :error :interrupted :timeout :cancelled})
(def successful-statuses #{:done})
(def blocker-statuses #{:error :interrupted :timeout :cancelled})
(def lifecycle-statuses (conj terminal-statuses :running))

(def rendering-kinds #{:vis/sci :vis/silent :vis/system :vis/tool :vis/answer :vis/error :vis/diagnostic})

(defn terminal?
  [status]
  (contains? terminal-statuses status))

(defn successful?
  [status]
  (contains? successful-statuses status))

(defn blocker?
  [status]
  (contains? blocker-statuses status))

(defn op-slug
  "Convert an op keyword/string/symbol into one slash-free child id segment."
  [op]
  (let [s (cond
            (keyword? op) (if (namespace op)
                            (str (namespace op) "." (name op))
                            (name op))
            (symbol? op) (if (namespace op)
                           (str (namespace op) "." (name op))
                           (name op))
            :else (str op))
        slug (-> s
               (str/replace #"/" ".")
               (str/replace #"[^A-Za-z0-9_.:-]" "-"))]
    (if (str/blank? slug) "event" slug)))

(defn block-ref
  [{:keys [conversation-prefix turn-prefix iteration block]}]
  (format-ref
    (cond-> {:turn-prefix turn-prefix
             :iteration iteration
             :block block}
      conversation-prefix (assoc :conversation-prefix conversation-prefix))))

(defn child-ref
  [parent-ref {:keys [kind op id]}]
  (case (or kind :tool)
    :error (str parent-ref "/error")
    :tool  (str parent-ref "/tool/" (op-slug (or id op :tool)))
    (throw (ex-info "Unsupported provenance child kind" {:kind kind :op op :id id}))))

(defn event
  "Build a lifecycle event projection. `:ref`, `:op`, `:status`, and
   `:rendering-kind` are required by construction."
  [{:keys [ref parent-ref op status rendering-kind metadata duration-ms started-at-ms finished-at-ms]}]
  (when-not (canonical-ref? ref)
    (throw (ex-info "Lifecycle event requires a canonical ref" {:ref ref})))
  (when-not (contains? lifecycle-statuses status)
    (throw (ex-info "Unsupported provenance lifecycle status" {:status status})))
  (when-not (contains? rendering-kinds rendering-kind)
    (throw (ex-info "Unsupported rendering kind" {:rendering-kind rendering-kind})))
  (cond-> {:provenance (cond-> {:ref ref
                                :op op
                                :status status}
                         parent-ref (assoc :parent-ref parent-ref)
                         duration-ms (assoc :duration-ms duration-ms)
                         started-at-ms (assoc :started-at-ms started-at-ms)
                         finished-at-ms (assoc :finished-at-ms finished-at-ms)
                         metadata (assoc :metadata metadata))
           :rendering-kind rendering-kind}
    metadata (assoc :metadata metadata)))

(defn start-event
  [{:keys [ref parent-ref op rendering-kind metadata started-at-ms]}]
  (event {:ref ref
          :parent-ref parent-ref
          :op op
          :status :running
          :rendering-kind rendering-kind
          :metadata metadata
          :started-at-ms started-at-ms}))

(defn finish-event
  [event-projection {:keys [status metadata duration-ms finished-at-ms] :or {status :done}}]
  (let [prov (:provenance event-projection)]
    (event {:ref (:ref prov)
            :parent-ref (:parent-ref prov)
            :op (:op prov)
            :status status
            :rendering-kind (:rendering-kind event-projection)
            :metadata (merge (:metadata prov) metadata)
            :duration-ms (or duration-ms (:duration-ms prov))
            :started-at-ms (:started-at-ms prov)
            :finished-at-ms finished-at-ms})))

(defn proof-compatible?
  "Proof refs must cite completed successful observations, never running work."
  [event-projection]
  (successful? (get-in event-projection [:provenance :status])))

(defn blocker-compatible?
  "Blocker refs may cite terminal failure/context, never still-running work."
  [event-projection]
  (terminal? (get-in event-projection [:provenance :status])))

;; -----------------------------------------------------------------------------
;; Proof-domain enums and predicates used by specs and pure harnesses
;; -----------------------------------------------------------------------------

(def event-kinds #{:eval :tool :error :answer :system :diagnostic :lifecycle})
(def bundle-kinds #{:candidate :proof :impediment :completion :closure})
(def subject-kinds #{:gate :plan :intent})
(def bundle-sources #{:manual :derived :automatic})
(def member-roles #{:observation :support :blocker :artifact :context})
(def attestation-kinds
  ;; Adds intent/suggested, intent/accepted, intent/deferred, intent/resumed for
  ;; the deferred-intent lifecycle (PROOF.md Tasks 28-34). Existing fulfilled/
  ;; abandoned attestations stay; the new ones cover the suggested→accepted→
  ;; deferred→resumable→resumed transitions so audit can read every
  ;; commitment hop from the attestation ledger.
  #{:gate/proven :gate/impeded
    :plan/completed :plan/blocked
    :intent/suggested :intent/accepted :intent/deferred :intent/resumed
    :intent/fulfilled :intent/abandoned})
(def attestation-decisions
  #{:proven :impeded :completed :blocked
    :suggested :accepted :deferred :resumed
    :fulfilled :abandoned})
(def attestation-statuses #{:accepted :rejected :superseded})
(def attester-kinds
  ;; `:system-policy` was added with PROOF.md Tasks 28–34 so intent
  ;; acceptance/resume attestations made by system policy can land in the
  ;; ledger without being mis-classified as `:runtime` or `:user`.
  #{:runtime :model :user :migration :system-policy})
(def resolution-statuses #{:open :proven :impeded :active :completed :blocked :superseded :fulfilled :abandoned})
(def guard-ops #{:and :or :not := :!= :< :<= :> :>= :exists :contains :in :matches})

;; -----------------------------------------------------------------------------
;; Intent lifecycle vocabulary (PROOF.md Task 28)
;;
;; The intent tree is a single rooted DAG per conversation with exactly one
;; running intent cursor. An intent's `status` follows a strict lifecycle:
;;
;;   :suggested  — extension or system proposal; NOT a Vis commitment yet.
;;   :active     — commitment Vis is currently running; blocks final answer.
;;   :deferred   — commitment waiting on an explicit Defer Trigger.
;;   :fulfilled  — commitment closed positively (closure attestation).
;;   :abandoned  — commitment closed without fulfillment (abandonment).
;;
;; Only `:active` intents block the conversation cursor / final answer.
;; `:suggested` and `:deferred` are visible to extensions via Intent Queries
;; but never silently jump the cursor.
;; -----------------------------------------------------------------------------

(def intent-statuses
  "Closed set of intent lifecycle states. Audit tooling enumerates this; new
   states must be added in spec, schema, and audit at the same time."
  #{:suggested :deferred :active :fulfilled :abandoned})

(def intent-blocking-statuses
  "States that legitimately block final-answer / cursor advancement."
  #{:active})

(def intent-resolved-statuses
  "Terminal lifecycle states. A resolved intent never re-blocks the cursor."
  #{:fulfilled :abandoned})

(def intent-pending-commitment-statuses
  "Pending commitment categories that require user/system observation but do
   not block the cursor right now."
  #{:suggested :deferred})

(def intent-sources
  "Who proposed the intent. `:user` and `:system` may also self-accept; an
   `:extension`-sourced intent must not silently self-accept."
  #{:user :system :extension})

(def intent-acceptance-actor-kinds
  "Who legitimately moves a `:suggested` intent to `:active` / `:deferred`,
   or moves a `:deferred` intent to `:active` (resume). Extensions may
   suggest and query; they must not appear here."
  #{:user :system-policy})

(def defer-trigger-kinds
  "Minimum vocabulary for what a deferred intent waits on. Each kind has
   distinct observation semantics:
     :defer/user-input        — user must answer/decide.
     :defer/time              — wall-clock predicate.
     :defer/extension-signal  — named extension event observed.
     :defer/intent            — another intent reaches a target status."
  #{:defer/user-input :defer/time :defer/extension-signal :defer/intent})

(def defer-sibling-policies
  "Defines whether a deferred subintent blocks its parent / siblings.
     :defer/continue-siblings — cursor may continue with adjacent siblings.
     :defer/block-parent      — parent (and therefore all sibling work) waits."
  #{:defer/continue-siblings :defer/block-parent})

(def abandonment-scopes
  "How far an abandonment decision reaches:
     :abandon/current-intent  — only this intent.
     :abandon/current-branch  — this intent + descendants.
     :abandon/all-running     — everything currently active in the conversation.
   `nil` is the legacy default for current callers and means
   `:abandon/current-intent`."
  #{:abandon/current-intent :abandon/current-branch :abandon/all-running})

(defn intent-status?
  [x]
  (boolean (and x (contains? intent-statuses x))))

(defn intent-source?
  [x]
  (boolean (and x (contains? intent-sources x))))

(defn defer-trigger-kind?
  [x]
  (boolean (and x (contains? defer-trigger-kinds x))))

(defn defer-sibling-policy?
  [x]
  (boolean (and x (contains? defer-sibling-policies x))))

(defn abandonment-scope?
  [x]
  (boolean (and x (contains? abandonment-scopes x))))

(defn intent-acceptance-actor-kind?
  "Returns true when the actor kind is allowed to move an intent into a
   commitment state. Extensions are deliberately rejected here."
  [x]
  (boolean (and x (contains? intent-acceptance-actor-kinds x))))

(defn proof-slot-id?
  [x]
  (and (vector? x)
    (= 2 (count x))
    (uuid-string? (first x))
    (keyword? (second x))))

(defn extract-path?
  [x]
  (and (vector? x)
    (every? #(or (keyword? %) (string? %) (integer? %)) x)))

(defn- guard-leaf?
  [x]
  (or (keyword? x)
    (string? x)
    (number? x)
    (boolean? x)
    (nil? x)
    (extract-path? x)))

(defn guard-expr?
  "Shape predicate for the tiny data-only guard language. This validates shape;
   runtime evaluates guards over derived values."
  [x]
  (and (vector? x)
    (keyword? (first x))
    (contains? guard-ops (first x))
    (case (first x)
      :and (and (seq (rest x)) (every? guard-expr? (rest x)))
      :or  (and (seq (rest x)) (every? guard-expr? (rest x)))
      :not (and (= 2 (count x)) (guard-expr? (second x)))
      :exists (= 2 (count x))
      (:= :!= :< :<= :> :>= :contains :in :matches)
      (and (= 3 (count x))
        (guard-leaf? (second x))
        (or (guard-leaf? (nth x 2)) (vector? (nth x 2))))
      false)))

;; -----------------------------------------------------------------------------
;; Pure evidence derivation and guard evaluation
;; -----------------------------------------------------------------------------

(def ^:private missing ::missing)

(def disallowed-proof-event-kinds
  "Mutable sidecar kinds that may be useful runtime state but can never be proof
   evidence. Extension aggregates/cache/status/checkpoint must be observed by
   runtime first and converted into immutable provenance events before proof."
  #{:extension-aggregate :extension_aggregate :extension/aggregate
    :extension-cache :extension_cache :extension/cache
    :extension-status :extension_status :extension/status
    :extension-checkpoint :extension_checkpoint :extension/checkpoint
    :aggregate :cache :status :checkpoint})

(defn- map-get-flex
  [m k]
  (cond
    (contains? m k) (get m k)
    (and (keyword? k) (contains? m (name k))) (get m (name k))
    (and (string? k) (contains? m (keyword k))) (get m (keyword k))
    :else missing))

(defn- path-value
  [x path]
  (reduce
    (fn [v segment]
      (cond
        (identical? missing v) (reduced missing)
        (map? v) (map-get-flex v segment)
        (and (sequential? v) (integer? segment) (<= 0 segment) (< segment (count v))) (nth v segment)
        :else (reduced missing)))
    x
    path))

(defn- event-ref
  [event]
  (or (:event/ref event) (get-in event [:provenance :ref]) (:ref event)))

(defn- event-status
  [event]
  (or (:event/status event) (get-in event [:provenance :status]) (:status event)))

(defn- event-kind
  [event]
  (or (:event/kind event) (get-in event [:provenance :kind]) (:kind event)))

(defn- event-op
  [event]
  (or (:event/op event) (get-in event [:provenance :op]) (:op event)))

(defn- event-payload
  [event]
  (cond
    (contains? event :event/payload) (:event/payload event)
    (contains? event :payload) (:payload event)
    (contains? event :result) {:result (:result event)}
    :else event))

(defn- same-op?
  [expected actual]
  (= (op-slug expected) (op-slug actual)))

(defn- compare-values
  [op left right]
  (case op
    := (= left right)
    :!= (not= left right)
    :< (and (number? left) (number? right) (< left right))
    :<= (and (number? left) (number? right) (<= left right))
    :> (and (number? left) (number? right) (> left right))
    :>= (and (number? left) (number? right) (>= left right))
    false))

(defn- contains-value?
  [container value]
  (cond
    (string? container) (str/includes? container (str value))
    (map? container) (contains? container value)
    (set? container) (contains? container value)
    (sequential? container) (boolean (some #(= value %) container))
    :else false))

(defn- resolve-operand
  [env operand]
  (if (extract-path? operand)
    (path-value env operand)
    operand))

(defn- evaluate-guard*
  [guard env]
  (if-not (guard-expr? guard)
    false
    (let [[op a b] guard]
      (case op
        :and (every? #(evaluate-guard* % env) (rest guard))
        :or (boolean (some #(evaluate-guard* % env) (rest guard)))
        :not (not (evaluate-guard* a env))
        :exists (not (identical? missing (resolve-operand env a)))
        (:= :!= :< :<= :> :>=) (compare-values op (resolve-operand env a) (resolve-operand env b))
        :contains (contains-value? (resolve-operand env a) (resolve-operand env b))
        :in (contains-value? (resolve-operand env b) (resolve-operand env a))
        :matches (let [left (resolve-operand env a)
                       pattern (resolve-operand env b)]
                   (boolean (and (some? left) (some? pattern) (re-find (re-pattern (str pattern)) (str left)))))
        false))))

(defn evaluate-guard
  "Evaluate a data-only guard expression over a derived binding.

   Guard operands that are extract paths read from an environment containing
   `:value`, `:binding`, and `:event`. Invalid guard shapes evaluate false; no
   guard execution path can call arbitrary code."
  [guard derived-binding]
  (let [env {:value (:evidence/value derived-binding)
             :binding derived-binding
             :event (:evidence/event derived-binding)}]
    (boolean (evaluate-guard* guard env))))

(defn- binding-error
  [requirement code message]
  (cond-> {:evidence/slot (:evidence/slot requirement)
           :evidence/from-ref (:evidence/from-ref requirement)
           :evidence/extract (:evidence/extract requirement)
           :evidence/error message
           :evidence/error-code code}
    (:evidence/guard requirement) (assoc :evidence/guard (:evidence/guard requirement)
                                    :evidence/guard-ok false)))

(defn derive-binding
  "Derive one evidence binding from an immutable runtime event and an evidence
   requirement. Caller-supplied `:evidence/value` is ignored by design.

   Returns a derived binding map. Failures are data (`:evidence/error` and
   `:evidence/error-code`) so tests, bundle writers, and audit can explain why a
   gate was not proven."
  [requirement event]
  (let [from-ref (:evidence/from-ref requirement)
        ref (event-ref event)
        status (event-status event)
        kind (event-kind event)
        op (event-op event)
        extract (:evidence/extract requirement)
        guard (:evidence/guard requirement)]
    (cond
      ;; Runtime canonical-ref check runs BEFORE the spec catch-all so the
      ;; caller gets the precise `:non-canonical-ref` code for compact display
      ;; aliases (e.g. "i1.1", "E1", "G1"). Otherwise the spec on
      ;; `:evidence/from-ref` (typed as ::canonical-ref) would mask the
      ;; specific failure as a generic `:invalid-requirement` and the
      ;; downstream branch below would be dead code. Caught by autoresearch
      ;; Task 26 persistence ref-rejection regressions.
      (not (canonical-ref? from-ref))
      (binding-error requirement :non-canonical-ref "Evidence requirement must cite a canonical ref")

      (not (s/valid? ::evidence-requirement (dissoc requirement :evidence/value)))
      (binding-error requirement :invalid-requirement "Evidence requirement shape is invalid")

      (not= from-ref ref)
      (binding-error requirement :ref-mismatch "Runtime event ref does not match evidence requirement")

      (contains? disallowed-proof-event-kinds kind)
      (binding-error requirement :mutable-extension-state "Mutable extension state cannot be proof evidence")

      (and kind (not (contains? event-kinds kind)))
      (binding-error requirement :unsupported-event-kind "Runtime event kind is not proof evidence")

      (not (successful? status))
      (binding-error requirement :non-successful-event "Only terminal successful runtime observations can prove evidence")

      (and (:event/kind requirement) (not= (:event/kind requirement) kind))
      (binding-error requirement :event-kind-mismatch "Runtime event kind does not match evidence requirement")

      (and (:event/op requirement) (not (same-op? (:event/op requirement) op)))
      (binding-error requirement :event-op-mismatch "Runtime event op does not match evidence requirement")

      :else
      (let [value (path-value (event-payload event) extract)]
        (if (identical? missing value)
          (binding-error requirement :missing-extract "Evidence extraction path did not resolve in runtime payload")
          (let [binding (cond-> {:evidence/slot (:evidence/slot requirement)
                                 :evidence/from-ref from-ref
                                 :evidence/extract extract
                                 :evidence/value value
                                 :evidence/event event}
                          guard (assoc :evidence/guard guard))
                guard-ok (if guard (evaluate-guard guard binding) true)]
            (cond-> (assoc binding :evidence/guard-ok guard-ok)
              (false? guard-ok) (assoc :evidence/error "Evidence guard evaluated false"
                                  :evidence/error-code :guard-false))))))))

(defn- empty-requirements-error
  []
  ;; A `binding-error`-shaped row that does NOT carry slot/from-ref/extract
  ;; (there is no requirement to attribute it to). Audit/persistence code
  ;; that walks `:gate/errors` should treat this as a top-level proof-shape
  ;; failure, not a per-binding extraction failure.
  {:evidence/error "Evidence bundle requires at least one evidence requirement"
   :evidence/error-code :empty-requirements
   :evidence/guard-ok false})

(defn evaluate-gate
  "Pure pre-storage gate harness.

   `events` are immutable runtime event observations. `requirements` are evidence
   extraction requests. The harness derives every binding from events by ref and
   accepts the gate only when every required binding derives cleanly and all
   guards pass.

   An empty `requirements` list is treated as a hard rejection (`:empty-
   requirements`). PROOF.md's trust spine says every accepted bundle must
   derive at least one runtime fact; a bundle with zero members is exactly the
   fake-proof bypass the attestation ledger exists to prevent."
  [events requirements]
  (if (empty? requirements)
    {:gate/proven? false
     :attestation/status :rejected
     :attestation/decision :impeded
     :bundle/bindings []
     :gate/errors [(empty-requirements-error)]}
    (let [by-ref (into {} (map (juxt event-ref identity) events))
          bindings (mapv (fn [requirement]
                           (if-let [event (get by-ref (:evidence/from-ref requirement))]
                             (derive-binding requirement event)
                             (binding-error requirement :missing-event "No runtime event exists for evidence requirement ref")))
                     requirements)
          failures (filterv :evidence/error bindings)
          accepted? (empty? failures)]
      {:gate/proven? accepted?
       :attestation/status (if accepted? :accepted :rejected)
       :attestation/decision (if accepted? :proven :impeded)
       :bundle/bindings bindings
       :gate/errors failures})))

;; -----------------------------------------------------------------------------
;; Proof-domain specs
;; -----------------------------------------------------------------------------

(s/def ::uuid uuid-string?)
(s/def ::non-blank-string non-blank-string?)
(s/def ::canonical-ref canonical-ref?)
(s/def ::terminal-status terminal-statuses)
(s/def ::successful-status successful-statuses)
(s/def ::blocker-status blocker-statuses)
(s/def ::lifecycle-status lifecycle-statuses)
(s/def ::rendering-kind rendering-kinds)
(s/def ::event-kind event-kinds)
(s/def ::bundle-kind bundle-kinds)
(s/def ::subject-kind subject-kinds)
(s/def ::bundle-source bundle-sources)
(s/def ::member-role member-roles)
(s/def ::attestation-kind attestation-kinds)
(s/def ::attestation-decision attestation-decisions)
(s/def ::attestation-status attestation-statuses)
(s/def ::attester-kind attester-kinds)
(s/def ::resolution-status resolution-statuses)
(s/def ::proof-slot-id proof-slot-id?)
(s/def ::extract-path extract-path?)
(s/def ::guard-expr guard-expr?)
(s/def ::metadata map?)

(s/def :event/id ::uuid)
(s/def :event/ref ::canonical-ref)
(s/def :event/parent-ref ::canonical-ref)
(s/def :event/kind ::event-kind)
(s/def :event/op (s/or :keyword keyword? :string non-blank-string? :symbol symbol?))
(s/def :event/status ::lifecycle-status)
(s/def :event/rendering-kind ::rendering-kind)
(s/def :event/payload-sha256 non-blank-string?)
(s/def :event/metadata map?)
(s/def :event/created-at inst?)

(s/def ::event
  (s/keys :req [:event/ref :event/status :event/op :event/rendering-kind]
    :opt [:event/id :event/parent-ref :event/kind :event/payload-sha256 :event/metadata :event/created-at]))

(s/def ::terminal-event (s/and ::event #(terminal? (:event/status %))))
(s/def ::successful-event (s/and ::event #(successful? (:event/status %))))
(s/def ::blocker-event (s/and ::event #(blocker? (:event/status %))))

(s/def :evidence/slot ::proof-slot-id)
(s/def :evidence/from-ref ::canonical-ref)
(s/def :evidence/extract ::extract-path)
(s/def :evidence/guard ::guard-expr)
(s/def :evidence/required? boolean?)
(s/def :evidence/value any?)
(s/def :evidence/guard-ok boolean?)
(s/def :evidence/error non-blank-string?)
(s/def :evidence/member-role ::member-role)

(s/def ::evidence-requirement
  (s/keys :req [:evidence/slot :evidence/from-ref :evidence/extract]
    :opt [:evidence/guard :evidence/required? :event/kind :event/op]))

(s/def ::derived-binding
  (s/keys :req [:evidence/slot :evidence/from-ref :evidence/extract]
    :opt [:evidence/value :evidence/guard :evidence/guard-ok :evidence/error :evidence/member-role]))

(s/def :bundle/id ::uuid)
(s/def :bundle/kind ::bundle-kind)
(s/def :bundle/subject-kind ::subject-kind)
(s/def :bundle/subject-id ::uuid)
(s/def :bundle/source ::bundle-source)
(s/def :bundle/summary non-blank-string?)
(s/def :bundle/bindings (s/coll-of ::derived-binding :kind vector?))
(s/def :bundle/sha256 non-blank-string?)
(s/def :bundle/metadata map?)

(s/def ::evidence-bundle
  (s/keys :req [:bundle/id :bundle/kind :bundle/subject-kind :bundle/subject-id :bundle/source :bundle/bindings]
    :opt [:bundle/summary :bundle/sha256 :bundle/metadata]))

(s/def :attestation/id ::uuid)
(s/def :attestation/kind ::attestation-kind)
(s/def :attestation/subject-kind ::subject-kind)
(s/def :attestation/subject-id ::uuid)
(s/def :attestation/evidence-bundle-id ::uuid)
(s/def :attestation/decision ::attestation-decision)
(s/def :attestation/status ::attestation-status)
(s/def :attestation/reason non-blank-string?)
(s/def :attestation/policy-version non-blank-string?)
(s/def :attestation/attester-kind ::attester-kind)
(s/def :attestation/attester-id non-blank-string?)
(s/def :attestation/schema-version non-blank-string?)
(s/def :attestation/payload map?)
(s/def :attestation/payload-sha256 non-blank-string?)

(s/def ::attestation
  (s/keys :req [:attestation/id :attestation/kind :attestation/subject-kind
                :attestation/subject-id :attestation/evidence-bundle-id
                :attestation/decision :attestation/status]
    :opt [:attestation/reason :attestation/policy-version :attestation/attester-kind
          :attestation/attester-id :attestation/schema-version :attestation/payload
          :attestation/payload-sha256]))

(s/def :resolution/subject-kind ::subject-kind)
(s/def :resolution/subject-id ::uuid)
(s/def :resolution/status ::resolution-status)
(s/def :resolution/attestation-id ::uuid)
(s/def :resolution/bundle-id ::uuid)
(s/def :resolution/summary non-blank-string?)

(s/def ::gate-resolution
  (s/and #(= :gate (:resolution/subject-kind %))
    (s/keys :req [:resolution/subject-kind :resolution/subject-id :resolution/status]
      :opt [:resolution/attestation-id :resolution/bundle-id :resolution/summary])))

(s/def ::plan-resolution
  (s/and #(= :plan (:resolution/subject-kind %))
    (s/keys :req [:resolution/subject-kind :resolution/subject-id :resolution/status]
      :opt [:resolution/attestation-id :resolution/bundle-id :resolution/summary])))

(s/def ::intent-resolution
  (s/and #(= :intent (:resolution/subject-kind %))
    (s/keys :req [:resolution/subject-kind :resolution/subject-id :resolution/status]
      :opt [:resolution/attestation-id :resolution/bundle-id :resolution/summary])))

(s/def :audit/code keyword?)
(s/def :audit/severity #{:info :warning :error})
(s/def :audit/message non-blank-string?)
(s/def :audit/ref ::canonical-ref)
(s/def :audit/subject-kind ::subject-kind)
(s/def :audit/subject-id ::uuid)
(s/def :audit/violations (s/coll-of ::audit-violation :kind vector?))
(s/def :audit/success? boolean?)
(s/def :audit/report string?)

(s/def ::audit-violation
  (s/keys :req [:audit/code :audit/severity :audit/message]
    :opt [:audit/ref :audit/subject-kind :audit/subject-id]))

(s/def ::audit-result
  (s/keys :req [:audit/success? :audit/violations]
    :opt [:audit/report]))

;; -----------------------------------------------------------------------------
;; Intent lifecycle shape specs (PROOF.md Task 28)
;;
;; Specs validate boundary shape: status, source, owner, parent, defer, resume,
;; abandonment, acceptance fields. Persistence still owns existence, same-
;; conversation scope, single-running-cursor cardinality, and tree integrity.
;; -----------------------------------------------------------------------------

(s/def :intent/id ::uuid)
(s/def :intent/conversation-id ::uuid)
(s/def :intent/title non-blank-string?)
(s/def :intent/rationale non-blank-string?)
(s/def :intent/status intent-status?)
(s/def :intent/source intent-source?)
(s/def :intent/owner-extension-id (s/and string? non-blank-string?))
(s/def :intent/parent-intent-id ::uuid)
(s/def :intent/created-at inst?)
(s/def :intent/resolved-at inst?)
(s/def :intent/fulfillment-summary non-blank-string?)
(s/def :intent/abandonment-reason non-blank-string?)
(s/def :intent/abandonment-scope abandonment-scope?)

(s/def :intent/accepted-by-kind intent-acceptance-actor-kind?)
(s/def :intent/accepted-by-id (s/and string? non-blank-string?))
(s/def :intent/accepted-at inst?)

(s/def :intent/defer-trigger-kind defer-trigger-kind?)
(s/def :intent/defer-trigger-payload map?)
(s/def :intent/defer-sibling-policy defer-sibling-policy?)
(s/def :intent/resumable-at inst?)
(s/def :intent/resumed-by-kind intent-acceptance-actor-kind?)
(s/def :intent/resumed-by-id (s/and string? non-blank-string?))
(s/def :intent/resumed-at inst?)

(s/def ::intent
  (s/keys :req [:intent/id :intent/conversation-id :intent/title
                :intent/rationale :intent/status :intent/source]
    :opt [:intent/owner-extension-id :intent/parent-intent-id :intent/created-at
          :intent/resolved-at :intent/fulfillment-summary :intent/abandonment-reason
          :intent/abandonment-scope
          :intent/accepted-by-kind :intent/accepted-by-id :intent/accepted-at
          :intent/defer-trigger-kind :intent/defer-trigger-payload
          :intent/defer-sibling-policy :intent/resumable-at
          :intent/resumed-by-kind :intent/resumed-by-id :intent/resumed-at]))

;; Defer / resume / abandonment decisions — used by the host APIs in Task 31.
;; These decisions are persisted via attestation rows and audited.

(s/def ::defer-decision
  (s/keys :req [:intent/id :intent/defer-trigger-kind]
    :opt [:intent/defer-trigger-payload :intent/defer-sibling-policy]))

(s/def ::resume-decision
  (s/keys :req [:intent/id :intent/resumed-by-kind]
    :opt [:intent/resumed-by-id]))

(s/def ::abandonment-decision
  (s/keys :req [:intent/id]
    :opt [:intent/abandonment-scope :intent/abandonment-reason]))

(s/def ::acceptance-decision
  (s/keys :req [:intent/id :intent/accepted-by-kind]
    :opt [:intent/accepted-by-id]))

;; Intent cursor — there is exactly one cursor per conversation and at most
;; one running intent. The cursor row links a conversation_soul to the intent
;; whose plan/gates the runtime is currently driving.

(s/def :intent-cursor/conversation-id ::uuid)
(s/def :intent-cursor/intent-id (s/nilable ::uuid))
(s/def :intent-cursor/updated-at inst?)

(s/def ::intent-cursor
  (s/keys :req [:intent-cursor/conversation-id]
    :opt [:intent-cursor/intent-id :intent-cursor/updated-at]))

(defn legal-intent-transition?
  "Returns true when the lifecycle transition `(from -> to)` is allowed.

   PROOF.md Tasks 28 / 31 / 33 fix the legal transitions to match the intent
   tree decisions. Anything not enumerated here is rejected so silent shortcuts
   (e.g. `:active → :suggested`, `:fulfilled → :active`) cannot happen.

   The legal transitions:
     nil         → :suggested | :active | :deferred  (creation)
     :suggested  → :active | :deferred | :abandoned
     :deferred   → :active | :abandoned
     :active     → :deferred | :fulfilled | :abandoned
     :fulfilled  → (terminal)
     :abandoned  → (terminal)"
  [from to]
  (let [legal {nil         #{:suggested :active :deferred}
               :suggested  #{:active :deferred :abandoned}
               :deferred   #{:active :abandoned}
               :active     #{:deferred :fulfilled :abandoned}
               :fulfilled  #{}
               :abandoned  #{}}]
    (contains? (get legal from #{}) to)))

(defn valid?
  "True when value satisfies proof-domain spec `spec`."
  [spec value]
  (s/valid? spec value))

(defn explain-data
  "Return spec explain-data for proof-domain spec `spec` and `value`."
  [spec value]
  (s/explain-data spec value))
