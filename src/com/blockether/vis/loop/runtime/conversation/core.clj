(ns com.blockether.vis.loop.runtime.conversation.core
  "Conversation lifecycle/send orchestration inside loop core."
  (:require [com.blockether.svar.internal.llm :as llm]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.loop.core :as loop-core]
            [com.blockether.vis.persistance.core :as db]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query-core])
  (:import [java.util UUID]))

;; ---------------------------------------------------------------------------
;; In-process conversation cache + channel utilities
;; ---------------------------------------------------------------------------

(defonce cache (atom {}))

(defn cache-env! [id env]
  (swap! cache assoc id {:environment env :lock (Object.)})
  {:id id :environment env})

(defn make-on-chunk-projector
  ([] (make-on-chunk-projector nil))
  ([{:keys [on-update]}]
   (let [timeline-atom (atom [])]
     (fn [chunk]
       (let [entry {:event :chunk :chunk chunk}
             timeline (swap! timeline-atom conj entry)]
         (when on-update (on-update timeline chunk))
         timeline)))))

(defn error->user-message [^Throwable e]
  (or (ex-message e) "Internal error"))

(defn- open-env!
  [id {:keys [channel external-id title]}]
  (let [router (query-core/get-router)
        sel    (when id [:id (UUID/fromString id)])
        env    (loop-core/create-environment router
                 (cond-> {:db (config/resolve-db-spec)}
                   sel         (assoc :conversation sel)
                   channel     (assoc :channel channel)
                   external-id (assoc :external-id external-id)
                   title       (assoc :title title)))]
    env))

(defn- ensure-env!
  [id]
  (if-let [entry (get @cache id)]
    entry
    (let [env (open-env! id {})]
      (swap! cache
        (fn [m]
          (if (contains? m id)
            m
            (assoc m id {:environment env :lock (Object.)}))))
      (get @cache id))))

(defonce ^:private shared-db (atom nil))

(defn db-info []
  (or @shared-db
    (swap! shared-db
      (fn [cur]
        (or cur (db/create-rlm-conn (config/resolve-db-spec)))))))

(defn create!
  ([channel] (create! channel nil))
  ([channel {:keys [title external-id]}]
   (let [env  (open-env! nil {:channel     channel
                              :external-id (some-> external-id str)
                              :title       title})
         id   (str (second (:conversation-id env)))
         _    (cache-env! id env)]
     {:id          id
      :channel     channel
      :external-id (some-> external-id str)
      :title       title})))

(defn by-id
  [id]
  (when-let [conv (db/db-get-conversation (db-info) [:id (java.util.UUID/fromString (str id))])]
    {:id            (str (:id conv))
     :channel       (:channel conv)
     :external-id   (:external-id conv)
     :system-prompt (:system-prompt conv)
     :model         (:model conv)
     :title         (:title conv)
     :created-at    (:created-at conv)}))

(defn by-channel
  [channel]
  (mapv (fn [c]
          {:id          (str (:id c))
           :channel     (:channel c)
           :external-id (:external-id c)
           :title       (:title c)
           :created-at  (:created-at c)})
    (db/db-list-conversations (db-info) channel)))

(defn for-telegram-chat!
  [chat-id]
  (let [ext (str chat-id)]
    (or (when-let [ref (db/db-find-conversation-by-external (db-info) :telegram ext)]
          (by-id (str (second ref))))
      (create! :telegram {:external-id ext}))))

(defn set-title!
  [id title]
  (db/db-update-conversation-title! (db-info) [:id (java.util.UUID/fromString (str id))] title)
  nil)

(defn env-for
  [id]
  (:environment (ensure-env! id)))

(defn send!
  ([id messages] (send! id messages {}))
  ([id messages opts]
   (let [{:keys [environment lock]} (ensure-env! id)
         msgs (if (string? messages) [(llm/user messages)] messages)]
     (locking lock
       (query-core/query! environment msgs opts)))))

(defn close!
  [id]
  (when-let [{:keys [environment]} (clojure.core/get @cache id)]
    (try (loop-core/dispose-environment! environment) (catch Exception _ nil)))
  (swap! cache dissoc id))

(defn delete!
  [id]
  (close! id)
  (let [d (db-info)]
    (try (db/delete-conversation-tree! d (java.util.UUID/fromString (str id)))
      (catch Exception _ nil))))

(defn sweep-orphaned-running-queries!
  ([] (sweep-orphaned-running-queries! (db-info)))
  ([db]
   (let [orphans (try (db/db-list-queries-by-status db :running)
                   (catch Exception _ []))
         answer  "⚠️ Turn interrupted — the server was restarted before this answer could finalize. Re-send the message to retry."]
     (doseq [{:keys [id iterations duration-ms]} orphans]
       (try
         (db/update-query! db [:id id]
           {:answer      answer
            :iterations  (or iterations 0)
            :duration-ms (or duration-ms 0)
            :status      :interrupted})
         (catch Exception _ nil)))
     (count orphans))))

(defn close-all!
  []
  (doseq [[_ {:keys [environment]}] @cache]
    (try (loop-core/dispose-environment! environment) (catch Exception _ nil)))
  (reset! cache {})
  (when-let [d @shared-db]
    (try (db/dispose-rlm-conn! d) (catch Exception _ nil))
    (reset! shared-db nil)))
