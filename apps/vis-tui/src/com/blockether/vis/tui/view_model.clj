(ns com.blockether.vis.tui.view-model
  "Pure wire projections for gateway-owned input and live views."
  (:require [com.blockether.vis.contract.view :as view-spec]
            [com.blockether.vis.contract.wire :as wire]))

(defn input-fields
  [fields]
  (persistent! (letfn [(walk [acc nodes]
                         (reduce (fn [a node]
                                   (cond (= view-spec/group-type (:type node)) (walk a
                                                                                     (:fields node))
                                         (view-spec/decoration? node) a
                                         :else (conj! a node)))
                                 acc
                                 nodes))]
                 (walk (transient []) fields))))

(def ^:private keyword-value-keys
  #{:type :direction :tone :variant :status :state :action :kind :mode :align})

(defn- restore-values
  [x]
  (cond (map? x) (persistent! (reduce-kv (fn [out k v]
                                           (assoc! out
                                                   k
                                                   (if (and (keyword-value-keys k) (string? v))
                                                     (keyword v)
                                                     (restore-values v))))
                                         (transient {})
                                         x))
        (sequential? x) (mapv restore-values x)
        :else x))

(defn view<-wire
  [value]
  (some-> value
          wire/->engine
          restore-values))

(defn live-view<-wire
  [value]
  (some-> value
          wire/->engine
          restore-values))

(defn live-patch<-wire
  [value]
  (some-> value
          wire/->engine
          restore-values))

(defn live-result<-wire
  [value]
  (some-> value
          wire/->engine
          restore-values))
