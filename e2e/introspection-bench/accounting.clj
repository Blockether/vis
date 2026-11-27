(ns accounting
  (:require [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.core :as sql]
            [com.blockether.vis.internal.session.agents]
            [taoensso.nippy :as nippy]))

(defn- request [row]
  (let [health (nippy/thaw (:request_health row))]
    (when (number? (:estimated-input-tokens health))
      {:projection (:counted-projection health)
       :input (:estimated-input-tokens health)
       :parts (reduce (fn [parts {:keys [label tokens]}]
                        (let [bucket (cond (#{"Vis core system prompt" "Custom system prompt" "Custom prompt additions"
                                               "System instructions" "Built-in tools and rules"
                                               "Session and environment context"} label) label
                                           (.startsWith (str label) "Extension: ") "Extension instructions"
                                           :else "Other")]
                          (update parts bucket (fnil + 0) tokens)))
                      {} (:breakdown health))})))

(defn- run [db sid]
  {:handoff (when-let [estimates (ns-resolve 'com.blockether.vis.internal.session.agents 'handoff-estimates)]
              (estimates (ps/db-agent-checkpoint db sid)))
   :requests (->> (sql/query! db {:select [:i.request_health]
                                 :from [[:session_turn_iteration :i]]
                                 :join [[:session_turn_state :r] [:= :r.id :i.session_turn_state_id]
                                        [:session_turn_soul :t] [:= :t.id :r.session_turn_soul_id]
                                        [:session_state :s] [:= :s.id :t.session_state_id]]
                                 :where [:and [:= :s.session_soul_id sid]
                                             [:is-not :i.request_health nil]]
                                 :order-by [[:t.position :asc] [:r.version :asc] [:i.position :asc]]})
                   (keep request) vec)
   :children (mapv #(run db (:session_id %))
                   (sql/query! db {:select [:session_id] :from [:session_agent]
                                   :where [:= :parent_id sid] :order-by [[:ordinal :asc]]}))})

(let [db (ps/db-create-connection! (System/getenv "VIS_BENCH_DB"))]
  (try (println (str "VIS_BENCH_ACCOUNTING "
                     (wire/json-str (wire/->wire (run db (System/getenv "VIS_BENCH_SID"))))))
       (finally (ps/db-dispose-connection! db))))
(shutdown-agents)
