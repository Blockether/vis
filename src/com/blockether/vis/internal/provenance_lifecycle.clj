(ns com.blockether.vis.internal.provenance-lifecycle
  "Compatibility facade for proof-domain lifecycle helpers.

   New code should require `com.blockether.vis.internal.proof` directly. This
   namespace remains temporarily while internal callers migrate; no new proof
   semantics live here."
  (:require
   [com.blockether.vis.internal.proof :as proof]))

(def terminal-statuses proof/terminal-statuses)
(def successful-statuses proof/successful-statuses)
(def blocker-statuses proof/blocker-statuses)
(def lifecycle-statuses proof/lifecycle-statuses)
(def rendering-kinds proof/rendering-kinds)

(defn terminal?
  [status]
  (proof/terminal? status))

(defn successful?
  [status]
  (proof/successful? status))

(defn blocker?
  [status]
  (proof/blocker? status))

(defn op-slug
  "Convert an op keyword/string into one slash-free child id segment."
  [op]
  (proof/op-slug op))

(defn block-ref
  [ref-data]
  (proof/block-ref ref-data))

(defn child-ref
  [parent-ref child]
  (proof/child-ref parent-ref child))

(defn event
  "Build a lifecycle event projection."
  [event-data]
  (proof/event event-data))

(defn start-event
  [event-data]
  (proof/start-event event-data))

(defn finish-event
  [event-projection opts]
  (proof/finish-event event-projection opts))

(defn proof-compatible?
  "Proof refs must cite completed successful observations, never running work."
  [event-projection]
  (proof/proof-compatible? event-projection))

(defn blocker-compatible?
  "Blocker refs may cite terminal failures/context, never running work."
  [event-projection]
  (proof/blocker-compatible? event-projection))
