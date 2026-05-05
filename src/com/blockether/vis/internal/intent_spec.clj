(ns com.blockether.vis.internal.intent-spec
  "Specs for conversation-scoped intents, canonical provenance refs, and
   persisted execution block projections.

   Specs validate shape at boundaries. Database existence, same-conversation
   scope, future-event rejection, and proof adequacy stay in persistence/runtime
   validation layers."
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [com.blockether.vis.internal.provenance-ref :as provenance-ref]))

;; -----------------------------------------------------------------------------
;; Scalar predicates
;; -----------------------------------------------------------------------------

(def non-blank-string?
  (s/and string? #(not (str/blank? %))))

(defn uuid-string? [x]
  (cond
    (uuid? x) true
    (string? x) (try
                  (java.util.UUID/fromString x)
                  true
                  (catch IllegalArgumentException _
                    false))
    :else false))

(def id-prefix8?
  (s/and string? #(boolean (re-matches #"(?i)[0-9a-f]{8}" %))))

(defn canonical-ref? [x]
  (provenance-ref/canonical-ref? x))

;; -----------------------------------------------------------------------------
;; Entity and public key specs
;; -----------------------------------------------------------------------------

(def intent-statuses #{:active :fulfilled :abandoned})
(def relation-kinds #{:subintent :related :supports :blocks})
(def intent-ref-roles #{:fulfillment-evidence :abandonment-evidence :context})
(def plan-statuses #{:active :completed :superseded :abandoned})
(def gate-statuses #{:open :proven :impeded})
(def gate-ref-roles #{:proof :impediment :context})
(def focus-sources #{:created :touched :inferred})
(def event-statuses #{:running :done :error :interrupted :timeout :cancelled})
(def rendering-kinds #{:vis/sci :vis/silent :vis/system :vis/tool :vis/answer :vis/error :vis/diagnostic})

;; Identity.
(s/def ::conversation-id uuid-string?)
(s/def ::conversation-prefix id-prefix8?)
(s/def ::turn-id uuid-string?)
(s/def ::turn-prefix id-prefix8?)
(s/def ::turn-state-id uuid-string?)
(s/def ::intent-id uuid-string?)
(s/def ::from-intent-id uuid-string?)
(s/def ::to-intent-id uuid-string?)
(s/def ::plan-id uuid-string?)
(s/def ::gate-id uuid-string?)

;; Common public aliases.
(s/def ::title non-blank-string?)
(s/def ::rationale non-blank-string?)
(s/def ::summary non-blank-string?)
(s/def ::reason non-blank-string?)
(s/def ::question non-blank-string?)
(s/def ::proposition non-blank-string?)
(s/def ::status event-statuses)
(s/def ::metadata map?)
(s/def ::steps vector?)
(s/def ::plan map?)
(s/def ::required? boolean?)
(s/def ::relation relation-kinds)
(s/def ::source focus-sources)
(s/def ::ref canonical-ref?)
(s/def ::refs (s/and (s/coll-of ::ref :kind vector?) #(= (count %) (count (distinct %)))))
(s/def ::created-ref ::ref)
(s/def ::resolved-ref ::ref)
(s/def ::parent-ref ::ref)
(s/def ::role keyword?)
(s/def ::created-at inst?)
(s/def ::resolved-at inst?)

;; Entity-focused registry entries. These explicit keywords preserve the plan's
;; short entity aliases without relying on auto-resolved nested namespaces.
(s/def :intent/id uuid-string?)
(s/def :intent/title non-blank-string?)
(s/def :intent/rationale non-blank-string?)
(s/def :intent/status intent-statuses)
(s/def :intent/relation relation-kinds)
(s/def :plan/id uuid-string?)
(s/def :plan/status plan-statuses)
(s/def :plan/summary non-blank-string?)
(s/def :gate/id uuid-string?)
(s/def :gate/status gate-statuses)
(s/def :gate/proposition non-blank-string?)
(s/def :provenance/ref canonical-ref?)
(s/def :rendering/kind rendering-kinds)
(s/def :block/idx nat-int?)
(s/def :report/ok? boolean?)

;; -----------------------------------------------------------------------------
;; Public writer opts — validates actual ergonomic public keys.
;; -----------------------------------------------------------------------------

(s/def ::issue-intent-opts
  (s/keys :req-un [::title ::rationale]
    :opt-un [::created-ref ::metadata]))

(s/def ::focus-intent-opts
  (s/keys :req-un [::rationale]
    :opt-un [::metadata]))

(s/def ::relate-intents-opts
  (s/and #(not= (:from-intent-id %) (:to-intent-id %))
    (s/keys :req-un [::from-intent-id ::to-intent-id ::relation]
      :opt-un [::rationale ::metadata])))

(s/def ::issue-plan-opts
  (s/keys :req-un [::intent-id ::summary]
    :opt-un [::plan ::steps ::created-ref ::metadata]))

(defn proof-slot-id? [x]
  (and (vector? x)
    (= 2 (count x))
    (uuid-string? (first x))
    (keyword? (second x))))

(s/def ::proof-slot-id proof-slot-id?)
(s/def ::slots (s/map-of ::proof-slot-id map?))
(s/def ::guard vector?)
(s/def ::expected-proof (s/keys :req-un [::slots]
                          :opt-un [::guard]))
(s/def ::candidate-proof (s/keys :opt-un [::slots ::refs]))
(s/def ::proof (s/keys :req-un [::summary ::refs]
                 :opt-un [::slots ::guard]))
(s/def ::impediment (s/keys :req-un [::reason ::refs]
                      :opt-un [::slots]))

(s/def ::issue-gate-opts
  (s/keys :req-un [::plan-id ::proposition ::expected-proof]
    :opt-un [::candidate-proof ::required? ::created-ref ::metadata]))

(s/def ::offer-proof-opts
  (s/keys :req-un [::gate-id]
    :opt-un [::slots ::refs ::metadata]))

(s/def ::prove-gate-opts
  (s/and #(seq (:refs %))
    (s/keys :req-un [::summary ::refs]
      :opt-un [::slots ::resolved-ref ::metadata])))

(s/def ::impede-gate-opts
  (s/and #(seq (:refs %))
    (s/keys :req-un [::reason ::refs]
      :opt-un [::slots ::resolved-ref ::metadata])))

(s/def ::block-gate-opts ::impede-gate-opts)

(s/def ::fulfill-intent-opts
  (s/and #(seq (:refs %))
    (s/keys :req-un [::summary ::refs]
      :opt-un [::resolved-ref ::metadata])))

(s/def ::abandon-intent-opts
  (s/and #(seq (:refs %))
    (s/keys :req-un [::reason ::refs]
      :opt-un [::resolved-ref ::metadata])))

;; -----------------------------------------------------------------------------
;; Provenance and rendering metadata
;; -----------------------------------------------------------------------------

(s/def ::op (s/or :keyword keyword? :string non-blank-string?))
(s/def ::duration-ms nat-int?)
(s/def ::started-at-ms nat-int?)
(s/def ::finished-at-ms nat-int?)
(s/def ::rendering-kind rendering-kinds)

(def obsolete-provenance-keys
  #{:markdown :label :short :engine :rendering-kind :result :error :stdout :stderr :vis/silent})

(defn no-obsolete-keys-in-provenance? [m]
  (empty? (set/intersection obsolete-provenance-keys (set (keys m)))))

(s/def ::provenance
  (s/and map?
    no-obsolete-keys-in-provenance?
    (s/keys :req-un [::ref ::op ::status]
      :opt-un [::parent-ref ::duration-ms ::started-at-ms ::finished-at-ms ::metadata])))

(s/def ::idx nat-int?)
(s/def ::code string?)
(s/def ::result any?)
(s/def ::error (s/nilable string?))
(s/def ::stdout string?)
(s/def ::stderr string?)
(s/def ::execution-time-ms nat-int?)

(s/def ::previews vector?)

(s/def ::executed-block
  (s/keys :req-un [::idx ::code ::provenance ::rendering-kind]
    :opt-un [::result ::previews ::error ::stdout ::stderr ::execution-time-ms]))

;; -----------------------------------------------------------------------------
;; Aggregate read/report shape
;; -----------------------------------------------------------------------------

(s/def ::handle non-blank-string?)
(s/def ::proof-summary non-blank-string?)
(s/def ::block-reason non-blank-string?)
(s/def ::fulfillment-summary non-blank-string?)
(s/def ::abandonment-reason non-blank-string?)
(s/def ::report string?)
(s/def ::ok? boolean?)
(s/def ::scope #{:conversation})
(s/def ::focused-intent-ids (s/coll-of uuid-string? :kind vector?))
(s/def ::unfocused-active-intent-ids (s/coll-of uuid-string? :kind vector?))
(s/def ::checks vector?)
(s/def ::violations vector?)
(s/def ::intents vector?)

(s/def ::intents-report
  (s/keys :req-un [::ok? ::scope ::conversation-id ::turn-state-id
                   ::focused-intent-ids ::unfocused-active-intent-ids
                   ::intents ::checks ::violations ::report]))
