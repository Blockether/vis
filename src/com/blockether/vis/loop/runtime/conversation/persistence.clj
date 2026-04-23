(ns com.blockether.vis.loop.runtime.conversation.persistence
  "Conversation persistence boundary.

   Reads/writes against the V1 soul/state schema:
     conversation       — soul (id, channel, external_id)
     conversation_state — versioned mutable props (system_prompt, model, title)

   The old `vis_conversation` sidecar is dead — everything lives in V1."
  (:require [com.blockether.vis.config :as config]
            [com.blockether.vis.loop.storage.db :as rlm-db]
            [com.blockether.vis.loop.storage.sqlite.conversations :as conv]))

(defonce ^:private shared-db (atom nil))

(defn db-info []
  (or @shared-db
    (swap! shared-db
      (fn [cur]
        (or cur (rlm-db/create-rlm-conn config/db-path))))))

;; =============================================================================
;; Read
;; =============================================================================

(defn find-conversation-by-id
  "Returns merged soul+state map, or nil."
  [db id]
  (when-let [conv (conv/db-get-conversation db [:id (java.util.UUID/fromString (str id))])]
    {:id            (str (:id conv))
     :channel       (:channel conv)
     :external-id   (:external-id conv)
     :system-prompt (:system-prompt conv)
     :model         (:model conv)
     :title         (:title conv)
     :created-at    (:created-at conv)}))

(defn find-conversation-by-external
  "Find a conversation by channel + external-id."
  [db channel external-id]
  (when-let [ref (conv/db-find-conversation-by-external db channel (str external-id))]
    (find-conversation-by-id db (str (second ref)))))

(defn list-conversations
  "Lists conversations for a channel, most recent first.
   Returns soul+latest-state merged maps."
  [db channel]
  (mapv (fn [c]
          {:id          (str (:id c))
           :channel     (:channel c)
           :external-id (:external-id c)
           :title       (:title c)
           :created-at  (:created-at c)})
    (conv/db-list-conversations db channel)))

;; =============================================================================
;; Write — delegated to the soul/state layer
;; =============================================================================

(defn insert-conversation!
  "Creates a new conversation soul + initial state.
   Called by `conversation.core/create!` for conversations that need
   channel/external-id routing (web, telegram). Returns lookup ref."
  [db {:keys [id channel external-id title system-prompt model]}]
  (conv/store-conversation! db
    {:channel       (or channel :vis)
     :external-id   external-id
     :system-prompt (or system-prompt "")
     :model         (or model "")
     :title         title}))

(defn update-conversation-title!
  "Update the title on the latest conversation_state."
  [db id title]
  (conv/db-update-conversation-title!
    db [:id (java.util.UUID/fromString (str id))] title))

(defn delete-conversation-row!
  "Delete conversation soul + cascade (states, queries, iterations, vars)."
  [db id]
  (rlm-db/delete-conversation-tree! db (java.util.UUID/fromString (str id))))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn dispose-shared-db! []
  (when-let [d @shared-db]
    (try (rlm-db/dispose-rlm-conn! d) (catch Exception _ nil))
    (reset! shared-db nil)))

(defn sweep-orphaned-running-queries!
  "Reconciles query rows left in :running status by a prior process that
   was killed mid-turn. Rewrites status → :interrupted."
  ([] (sweep-orphaned-running-queries! (db-info)))
  ([db]
   (let [orphans (try
                   (conv/db-list-queries-by-status db :running)
                   (catch Exception _ []))
         answer  "⚠️ Turn interrupted — the server was restarted before this answer could finalize. Re-send the message to retry."]
     (doseq [{:keys [id iterations duration-ms]} orphans]
       (try
         (conv/update-query! db [:id id]
           {:answer      answer
            :iterations  (or iterations 0)
            :duration-ms (or duration-ms 0)
            :status      :interrupted})
         (catch Exception _ nil)))
     (count orphans))))
