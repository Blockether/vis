(ns com.blockether.vis.internal.session.atif
  "ATIF export of the saved turns and tool calls of a session."
  (:require [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.loop.trajectory :as trajectory]
            [com.blockether.vis.internal.persistance.core :as db]))

(defn- attachment-descriptor
  [attachment]
  {:filename (:filename attachment)
   :media-type (:media-type attachment)
   :size-bytes (:size attachment)})

(defn- iteration-record
  [attachments iteration]
  (-> iteration
      (select-keys [:position :thinking :assistant-prose :created-at :finished-at :provider :model
                    :error :input-tokens :output-tokens :input-cache-read-tokens
                    :input-cache-write-tokens :output-reasoning-tokens :cost-usd :forms])
      (assoc :attachments (mapv attachment-descriptor attachments))
      (update :forms
              (fn [forms]
                (mapv #(select-keys %
                                    [:src :code :stdout :error :tool-name :tool-call-id :envelope
                                     :repaired? :timeout? :repaired-source])
                      forms)))
      wire/->wire))

(defn- read-iterations
  [store attachments rows]
  ;; Each batch holds a limited number of uncut tool payloads. Do not retain
  ;; the batches once the trajectory finishes their steps.
  (mapcat (fn [batch]
            (let [iterations (db/db-list-iterations store (map :id batch) {:activity? false})]
              (into []
                    (keep (fn [{:keys [id]}]
                            (when-let [iteration (get iterations (str id))]
                              (iteration-record (get attachments (str id)) iteration))))
                    batch)))
          (partition-all 16 rows)))

(defn- turn-record
  [store attachments rows turn]
  (let [tid (str (:id turn))]
    (assoc (wire/->wire {:position (:position turn)
                         :created-at (:created-at turn)
                         :request (or (:user-request turn) (get-in turn [:council :content]))
                         :content (:content turn)
                         :tokens {:input (:input-tokens turn)
                                  :output (:output-tokens turn)
                                  :cached (:input-cache-read-tokens turn)}
                         :cost {:total-cost (:total-cost turn)}
                         :attachments (mapv attachment-descriptor (get attachments tid))})
      "trace" (read-iterations store attachments (get rows tid)))))

(defn- read-turns
  [store sid]
  (let [turns
        (db/db-list-session-turns store sid)

        rows
        (db/db-list-session-turns-iterations-meta store (map :id turns))

        attachments
        (group-by #(or (:iteration-id %) (:turn-soul-id %))
                  (remove #(= "model" (:audience %))
                    (db/db-list-session-attachments-meta store sid)))]

    (map #(turn-record store attachments rows %) turns)))

(defn- child-records
  "Return only the children below `sid`, not its peers on the same team."
  [store sid]
  (let [leader
        (or (:leader_id (db/db-agent-info store sid)) sid)

        by-parent
        (group-by :parent_id (db/db-agent-list store leader))]

    (loop [seen
           #{sid}

           pending
           [sid]

           records
           []]

      (if-let [parent (peek pending)]
        (let [children (remove #(contains? seen (:session_id %)) (get by-parent parent))
              ids (map :session_id children)]

          (recur (into seen ids) (into (pop pending) ids) (into records children)))
        records))))

(defn- session-metadata
  [store session]
  {:started-at (:created-at session)
   :cwd (some->> (:project-id session)
                 (db/db-get-project store)
                 :workspace-root)})

(defn export-json
  "Read and encode a saved session without loading its attachment bytes."
  [store sid]
  (when-let [session (db/db-get-session store sid)]
    (let [sid (str sid)
          root (merge (select-keys session [:title :provider :model])
                      {:session-id sid}
                      (session-metadata store session))
          children (mapv (fn [record]
                           (merge record
                                  (session-metadata store (db/db-get-session store (:session_id record)))))
                         (child-records store sid))]
      (trajectory/saved-json root children #(read-turns store %)))))
