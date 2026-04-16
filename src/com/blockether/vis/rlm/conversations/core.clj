(ns com.blockether.vis.rlm.conversations.core
  "Conversation lifecycle/send orchestration inside RLM."
  (:require [com.blockether.svar.internal.llm :as llm]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.rlm :as rlm]
            [com.blockether.vis.rlm.persistence.db :as rlm-db]
            [com.blockether.vis.rlm.conversations.persistence :as persistence]
            [com.blockether.vis.rlm.conversations.shared :as shared])
  (:import [java.util UUID]))

(defn- open-env!
  [id]
  (config/resolve-config nil)
  (let [router (config/get-router)
        sel    (when id [:id (UUID/fromString id)])
        env    (rlm/create-env router
                 (cond-> {:db config/db-path}
                   sel (assoc :conversation sel)))]
    (shared/register-base-tools! env)))

(defn- ensure-env!
  [id]
  (if-let [entry (get @shared/cache id)]
    entry
    (let [env (open-env! id)]
      (swap! shared/cache
        (fn [m]
          (if (contains? m id)
            m
            (assoc m id {:env env :lock (Object.)}))))
      (get @shared/cache id))))

(defn create!
  ([channel] (create! channel nil))
  ([channel {:keys [title external-id]}]
   (let [env  (open-env! nil)
         id   (str (second (:conversation-ref env)))
         _    (shared/cache-env! id env)
         _    (persistence/insert-conversation! (persistence/db-info)
                {:id id
                 :channel channel
                 :external-id (some-> external-id str)
                 :title title})]
     {:id          id
      :channel     channel
      :external-id (some-> external-id str)
      :title       title})))

(defn by-id
  [id]
  (persistence/find-conversation-by-id (persistence/db-info) id))

(defn by-channel
  [channel]
  (persistence/list-conversations (persistence/db-info) channel))

(defn for-telegram-chat!
  [chat-id]
  (let [ext (str chat-id)]
    (or (persistence/find-conversation-by-external (persistence/db-info) :telegram ext)
      (create! :telegram {:external-id ext}))))

(defn set-title!
  [id title]
  (persistence/update-conversation-title! (persistence/db-info) id title)
  nil)

(defn env-for
  [id]
  (:env (ensure-env! id)))

(defn send!
  ([id messages] (send! id messages {}))
  ([id messages opts]
   (let [{:keys [env lock]} (ensure-env! id)
         msgs (if (string? messages) [(llm/user messages)] messages)]
     (locking lock
       (rlm/query-env! env msgs opts)))))

(defn close!
  [id]
  (when-let [{:keys [env]} (clojure.core/get @shared/cache id)]
    (try (rlm/dispose-env! env) (catch Exception _ nil)))
  (swap! shared/cache dissoc id))

(defn delete!
  [id]
  (close! id)
  (let [d (persistence/db-info)]
    (try (rlm-db/delete-entity-tree! d (UUID/fromString id))
      (catch Exception _ nil))
    (try (persistence/delete-conversation-row! d id)
      (catch Exception _ nil))))

(defn close-all!
  []
  (doseq [[_ {:keys [env]}] @shared/cache]
    (try (rlm/dispose-env! env) (catch Exception _ nil)))
  (reset! shared/cache {})
  (persistence/dispose-shared-db!))
