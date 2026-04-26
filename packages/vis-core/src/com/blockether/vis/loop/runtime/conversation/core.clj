(ns com.blockether.vis.loop.runtime.conversation.core
  "Conversation lifecycle/send orchestration inside loop core."
  (:require [com.blockether.svar.internal.llm :as llm]
            [com.blockether.svar.internal.spec :as svar-spec]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.loop.core :as loop-core]
            [com.blockether.vis.loop.runtime.conversation.environment.query.iteration.core :as iterate]
            [com.blockether.vis.persistance.core :as db]
            [com.blockether.vis.persistance.spec :as rlm-spec]
            [com.blockether.vis.loop.runtime.conversation.environment.query.core :as query-core]))

;; ---------------------------------------------------------------------------
;; In-process conversation cache + channel utilities
;; ---------------------------------------------------------------------------

(defonce cache (atom {}))

(defn cache-env! [id env]
  (swap! cache assoc id {:environment env :lock (Object.)})
  {:id id :environment env})

(defn refresh-cached-routers!
  "Reseat `:router` on every cached env's environment map.

  `loop-core/create-environment` snapshots the router into
  `(:router env)` at construction time, and the iteration loop calls
  `(llm/ask! (:router environment) ...)` — not the global
  `query-core/router-atom`. So when a frontend changes provider
  config and rebuilds the global router, every long-lived env in the
  cache (TUI keeps one for the whole session) keeps talking to the
  *previous* model until disposed.

  Call this immediately after `query-core/rebuild-router!` so the
  next `send!` on any cached conversation picks up the new router."
  [router]
  (when router
    (swap! cache
      (fn [m]
        (reduce-kv
          (fn [acc id {:keys [environment] :as entry}]
            (assoc acc id
              (assoc entry :environment (assoc environment :router router))))
          {} m))))
  nil)

(defn error->user-message [^Throwable e]
  (or (ex-message e) "Internal error"))

(defn- open-env!
  [id {:keys [channel external-id title]}]
  (let [router (query-core/get-router)
        env    (loop-core/create-environment router
                 (cond-> {:db (config/resolve-db-spec)}
                   id          (assoc :conversation id)
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
         id   (str (:conversation-id env))
         _    (cache-env! id env)]
     {:id          id
      :channel     channel
      :external-id (some-> external-id str)
      :title       title})))

(defn by-id
  [id]
  (when-let [conv (db/db-get-conversation (db-info) id)]
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
  (db/db-update-conversation-title! (db-info) id title)
  nil)

(defn env-for
  [id]
  (:environment (ensure-env! id)))

(defn effective-system-prompt-for-query
  "Return the reconstructed prompt snapshot for a specific query-id in a conversation."
  [conversation-id query-id]
  (when-let [env (env-for conversation-id)]
    (let [;; Activate extensions ONCE for this snapshot — reused by both
          ;; assemble-system-prompt (system block) and build-iteration-context
          ;; (per-iter user block) below. Mirrors the runtime contract.
          active-exts   (loop-core/active-extensions env)
          system-prompt (loop-core/assemble-system-prompt env
                          {:system-prompt     (:system-prompt (by-id conversation-id))
                           :active-extensions active-exts})
          queries       (when-let [di (:db-info env)]
                          (try (db/db-list-conversation-queries di (:conversation-id env))
                            (catch Throwable _ nil)))
          query-row     (some #(when (= (:id %) query-id) %) queries)
          query-text    (or (:query query-row) "<the user's message appears here>")
          handover      (when (and (:db-info env) (:conversation-id env) query-id
                                (> (count queries) 1))
                          (try (iterate/build-cross-query-handover
                                 (:db-info env) (:conversation-id env) query-id nil)
                            (catch Throwable _ nil)))
          prior-thinking (when (and (:db-info env) query-id)
                           (try (iterate/build-prior-thinking env (:db-info env) query-id)
                             (catch Throwable _ nil)))
          combined-prior (cond
                           (and handover prior-thinking) (str handover "\n\n" prior-thinking)
                           handover handover
                           :else prior-thinking)
          iter-ctx (iterate/build-iteration-context env
                     {:iteration              0
                      :current-max-iterations rlm-spec/MAX_ITERATIONS
                      :prior-thinking         combined-prior
                      :prev-expressions       nil
                      :prev-iteration         nil
                      :call-counts-atom       (atom {})
                      :active-extensions      active-exts})
          has-reasoning? (loop-core/provider-has-reasoning? (:router env))
          iter-spec (if has-reasoning?
                      rlm-spec/ITERATION_SPEC_REASONING
                      rlm-spec/ITERATION_SPEC_NON_REASONING)
          spec-prompt (svar-spec/spec->prompt iter-spec)]
      (str "═══ MESSAGE 1: System (role: system) ═══\n"
        system-prompt
        "\n\n═══ MESSAGE 2: User query (role: user) ═══\n"
        query-text
        (when iter-ctx
          (str "\n\n═══ MESSAGE 3: Iteration context (role: user, appended per iteration) ═══\n"
            iter-ctx))
        "\n\n═══ MESSAGE 4: Spec schema (role: user, appended by svar) ═══\n"
        spec-prompt))))

(defn effective-system-prompt
  "Return the reconstructed prompt snapshot for the latest query in a conversation."
  [conversation-id]
  (when-let [d (some-> (env-for conversation-id) :db-info)]
    (let [queries (try (db/db-list-conversation-queries d conversation-id)
                    (catch Throwable _ nil))
          last-q  (last queries)]
      (when last-q
        (effective-system-prompt-for-query conversation-id (:id last-q))))))

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
    (try (db/delete-conversation-tree! d id)
      (catch Exception _ nil))))

(defn sweep-orphaned-running-queries!
  ([] (sweep-orphaned-running-queries! (db-info)))
  ([db]
   (let [orphans (try (db/db-list-queries-by-status db :running)
                   (catch Exception _ []))
         answer  "Warning: Turn interrupted — the server was restarted before this answer could finalize. Re-send the message to retry."]
     (doseq [{:keys [id iterations duration-ms]} orphans]
       (try
         (db/update-query! db id
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
