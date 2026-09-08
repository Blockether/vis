(ns com.blockether.vis.internal.council.host
  "Python host surface. Identity comes from the environment, never the mutable Python session dict."
  (:refer-clojure :exclude [read get])
  (:require [clojure.walk :as walk]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.extension.core :as extension]))

(defn- result
  [env op opts value]
  (let [gid
        (or (:group_id opts)
            (:group_id value)
            (:group_id (first (:entries value)))
            (council/default-group (:db-info env) (:session-id env)))

        entries
        (if (:id value) [value] (:entries value))

        refs
        (vec (distinct (concat [{:type "council-group" :id gid}]
                               (keep #(when-let [id (:thread_id %)] {:type "council-thread"
                                                                     :id (str id)})
                                     entries)
                               (keep #(when-let [id (:id %)] {:type "council-entry" :id (str id)})
                                     entries))))]

    (try (extension/publish-activity! {:headline (name op)
                                       :summary
                                       (if-let [id (:id value)]
                                         (str "Entry #" id " in thread #" (:thread_id value))
                                         (str (count (or (:entries value) value)) " records"))})
         (catch Exception _ nil))
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
  "Append content to a new titled thread, or an existing thread_id. Only explicit ping targets are notified."
  ([env content] (publish env content {}))
  ([env content opts]
   (let [db
         (:db-info env)

         sid
         (str (:session-id env))

         ctx
         @(:ctx-atom env)

         source
         (cond-> {:session_id sid :scope (ctx-loop/cursor-snapshot env)}
           extension/*current-invocation-id*
           (assoc :operation_id extension/*current-invocation-id*))

         actor
         {:session-id sid
          :activation-id (:activation-id (:council-actor ctx))
          :source "host"
          :source-ref source}

         opts
         (assoc (walk/keywordize-keys opts) :content content)

         entry
         (council/publish! db #(council/runtime db) actor opts)

         ref
         (select-keys entry [:id :thread_id :group_id :source_ref])]

     (swap! (:ctx-atom env) update :council-publications (fnil conj []) ref)
     (result env :council.publish opts entry))))

(defn threads
  "List titled thread roots, ascending by thread_id. Returns entries, after and has_more; no content."
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
        {:symbol name
         :inject-env? true
         :tag tag
         :active-fn council/enabled?
         :call {:pos (or positional []) :rest :always}
         :params (mapv (fn [p]
                         {:name p})
                       params)
         :description (:doc (meta v))
         :result
         "Members: `{session_id, title, state}`. Entries: `{id, thread_id, group_id, content, author_session_id, created_at, source, ping}` plus root `title` and host `source_ref`. Pages: `{entries, after, has_more}`; thread summaries: `{thread_id, title, author_session_id, created_at}`."}))
    [[#'members 'council.members :observation ["group_id"]]
     [#'publish 'council.publish :mutation ["group_id" "thread_id" "title" "ping" "idempotency_key"]
      ["content"]] [#'threads 'council.threads :observation ["group_id" "after" "limit"]]
     [#'read 'council.read :observation ["group_id" "thread_id" "after" "limit"]]
     [#'get 'council.get :observation ["group_id"] ["entry_id"]]]))

(defn context
  [env]
  (when (council/enabled? env)
    (try {"default_group_id" (council/default-group (:db-info env) (:session-id env))}
         (catch Exception _ nil))))
