(ns com.blockether.vis.internal.council.host
  "Python host surface. Identity comes from the environment, never the mutable Python session dict."
  (:refer-clojure :exclude [read get])
  (:require [clojure.walk :as walk]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.extension.core :as extension]
            [taoensso.telemere :as tel]))

(defn- result
  [env op opts value]
  (let [gid
        (or (:group_id opts)
            (:group_id value)
            (:group_id (first (:entries value)))
            (council/default-group (:db-info env) (:session-id env)))

        entries
        (if (:entry_id value) [value] (:entries value))

        refs
        (vec (distinct (concat
                         [{:type "council-group" :id gid}]
                         (keep #(when-let [id (:thread_id %)] {:type "council-thread" :id (str id)})
                               entries)
                         (keep #(when-let [id (:entry_id %)] {:type "council-entry" :id (str id)})
                               entries))))]

    (try (extension/publish-activity! (presenter/result-presentation {:operation op}
                                                                     (wire/->wire value)))
         (catch Exception e
           (tel/log! {:level :warn
                      :id ::activity-publication-failed
                      :data {:session-id (str (:session-id env))
                             :op op
                             :entry-id (:entry_id value)
                             :error-class (.getName (class e))}})))
    (extension/success {:op op :result (wire/->wire value) :metadata {:activity/resources refs}})))

(defn members
  "List active participants as session_id, title and running/queued/held state. No activation ids."
  ([env] (members env {}))
  ([env opts]
   (let [opts (walk/keywordize-keys opts)]
     (result env
             :council.members
             opts
             (council/members (:db-info env)
                              #(council/runtime (:db-info env))
                              (str (:session-id env))
                              opts)))))

(defn publish
  "Publish an entry with required kind: complain for failures or concrete improvements, coordination for work/questions, or informational for results/decisions. Complaints enter the persistent improve register. Include evidence and the relevant turn/iteration; source_ref automatically stamps this publication for every kind. Select individual pings, all, or none; kind never selects recipients. FAILED python_execution is already recorded as autocomplain without pings. A no-ping continuation answers the latest entry addressed to this session if it is an unanswered request, and notifies its author; reply_to selects a request explicitly. reply_required=True requires an answer in the receiving iteration. Explicit ping IDs can wake idle peers; all selects active peers only."
  ([env content] (publish env content {}))
  ([env content opts]
   (let [db
         (:db-info env)

         sid
         (str (:session-id env))

         execution
         (:council (ctx-loop/read-turn-state env))

         source
         (cond-> (council/source-ref env)
           extension/*current-invocation-id*
           (assoc :operation_id extension/*current-invocation-id*))

         actor
         {:session-id sid
          :activation-id (:activation-id execution)
          :source "host"
          :source-ref source}

         opts
         (assoc (walk/keywordize-keys opts) :content content)

         entry
         (council/publish! db #(council/runtime db) actor opts)

         ref
         (select-keys entry [:entry_id :thread_id :group_id :kind :source_ref])]

     (let [[before after] (swap-vals!
                            (:turn-state-atom env)
                            (fn [state]
                              (if (and execution
                                       (= (select-keys execution [:activation-id :iteration-key])
                                          (select-keys (:council state)
                                                       [:activation-id :iteration-key])))
                                (update-in state [:council :publications] (fnil conj []) ref)
                                state)))]
       (when (identical? before after)
         (tel/log! {:level :warn
                    :id ::publication-execution-ended
                    :data {:session-id sid :entry-id (:entry_id entry)}})))
     (result env :council.publish opts entry))))

(defn threads
  "List thread roots with their root message kind, ascending by thread_id. Returns entries, after and has_more; no content."
  ([env] (threads env {}))
  ([env opts]
   (let [opts (walk/keywordize-keys opts)]
     (result env
             :council.threads
             opts
             (council/threads (:db-info env) (str (:session-id env)) opts)))))

(defn read
  "Read a bounded page of the group log, optionally filtered by thread_id. Does not consume pings."
  ([env] (read env {}))
  ([env opts]
   (let [opts (walk/keywordize-keys opts)]
     (result env
             :council.read
             opts
             (council/read-entries (:db-info env) (str (:session-id env)) opts)))))

(defn get
  "Fetch a full entry by entry_id; a truncated ping never changes the stored content."
  ([env entry-id] (get env entry-id {}))
  ([env entry-id opts]
   (let [opts (assoc (walk/keywordize-keys opts) :entry_id entry-id)]
     (result env
             :council.get
             opts
             (council/get-entry (:db-info env) (str (:session-id env)) opts)))))

(def symbols
  (mapv
    (fn [[v name tag params positional]]
      (extension/symbol
        v
        {:activity (presenter/for-tool (keyword (str name)))
         :symbol name
         :inject-env? true
         :tag tag
         :active-fn council/enabled?
         :call {:pos (or positional []) :rest :always}
         :params (mapv (fn [p]
                         (cond-> {:name p}
                           (= p "kind")
                           (assoc :required? true)))
                       params)
         :description (:doc (meta v))
         :result
         "Members: `{session_id, title, state}`. Entries: `{entry_id, thread_id, group_id, kind, content, author_session_id, created_at, source, ping}` plus root `title`, host `source_ref`, and optional `reply_required`, `replies` or `reply_to`. Replies report `{session_id, state, reply_entry_id?}`. Pages: `{entries, after, has_more}`; thread summaries: `{thread_id, kind, title, author_session_id, created_at}`."}))
    [[#'members 'council.members :observation ["group_id"]]
     [#'publish 'council.publish :mutation
      ["kind" "group_id" "thread_id" "title" "ping" "idempotency_key" "reply_required" "reply_to"]
      ["content"]] [#'threads 'council.threads :observation ["group_id" "after" "limit"]]
     [#'read 'council.read :observation ["group_id" "thread_id" "after" "limit"]]
     [#'get 'council.get :observation ["group_id"] ["entry_id"]]]))

(defn context
  [env]
  (when (council/enabled? env)
    (try {"default_group_id" (council/default-group (:db-info env) (:session-id env))
          "pending_replies" (wire/->wire (council/session-pending-replies env))}
         (catch Exception _ nil))))
