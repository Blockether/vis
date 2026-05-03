(ns com.blockether.vis.internal.provenance-lifecycle
  "Lifecycle-oriented provenance event helpers.

   The runtime owns canonical ref allocation. Tools and deferred work may ask
   for child events, but they must not invent refs or mark running work as
   proof. A block can prove that a future was created; only a terminal child
   event can prove that deferred work completed."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.provenance-ref :as prov-ref]))

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
  "Convert an op keyword/string into one slash-free child id segment."
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
  (prov-ref/format-ref
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
  (when-not (prov-ref/canonical-ref? ref)
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
  "Proof refs must cite completed successful observations, never running or
   future/deferred placeholders."
  [event-projection]
  (successful? (get-in event-projection [:provenance :status])))

(defn blocker-compatible?
  "Blocker refs may cite successful context or terminal failures/timeouts, but
   never a still-running future/deferred event."
  [event-projection]
  (terminal? (get-in event-projection [:provenance :status])))
