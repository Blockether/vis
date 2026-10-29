(ns com.blockether.vis.internal.council.host
  "Python host surface. Identity comes from the environment, never the mutable Python session dict."
  (:refer-clojure :exclude [read get])
  (:require [clojure.walk :as walk]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.session.agents :as agents]
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
  "Publish a Council entry. Choose individual pings, all, or none.

   `title` is optional for a new thread. With `thread_id` or `reply_to`, Council ignores `title`.
   Validation and idempotency checks use the request without this field. The existing thread title
   stays unchanged.

   `source_ref` identifies this publication, not another execution. Keep secrets and private data
   out of messages.

   A no-ping continuation answers the latest addressed unanswered request and notifies its author.
   `reply_to` selects an unanswered request explicitly, once per recipient. `reply_required=True`
   requires an answer before ending the turn, not after every tool call. Read or do authorized work
   first. Acceptance is not task completion.

   Council is asynchronous message passing. Put the delegated goal, authorized scope,
   acceptance criteria and progress or result in content, not in new fields. Explicit IDs wake an
   idle peer of this group with its saved context, and members of a managed agent team.
   `ping=\"all\"` selects active peers only. A peer that is already running reads the ping inside
   its turn only with `reply_required=True`.

   With Subagents enabled, delegate work to owned children with `council.run` instead of treating
   project peers as subagents. See `doc(\"council\")` for the delivery and work protocols.

   A ping from a parent to its idle subagent gives it a follow-up task with its saved context.
   Then call `council.wait([session_id])` for its answer. `files` replaces the file grant of that
   subagent for the task. A subagent without iterations left needs `council.extend` first. A
   cancelled subagent continues only during a running turn of its leader."
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
         (agents/publish-with-files! env
                                     opts
                                     (fn [opts]
                                       (council/publish! db #(council/runtime db) actor opts)))

         ref
         (select-keys entry [:entry_id :thread_id :group_id :source_ref])]

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
  "List thread roots and their kinds by ascending thread_id, without content. group_id defaults to
   the session group. after is an exclusive nonnegative integer thread_id cursor (default 0). limit
   is an integer 1–50 (default 50). It returns entries, after and has_more. While has_more is true,
   pass the returned after for the next page.

   A page can be shorter because of the byte budget. It does not accept thread_id."
  ([env] (threads env {}))
  ([env opts]
   (let [opts (walk/keywordize-keys opts)]
     (result env
             :council.threads
             opts
             (council/threads (:db-info env) (str (:session-id env)) opts)))))

(defn read
  "Read the group log by ascending entry_id. It does not consume pings. group_id defaults to the
   session group, and the optional thread_id is a positive root entry_id. after is an
   exclusive nonnegative integer entry_id cursor (default 0). limit is an integer 1–50
   (default 50).

   It returns entries, after and has_more. While has_more is true, pass the returned after for the
   next page. A page can be shorter because of the byte budget."
  ([env] (read env {}))
  ([env opts]
   (let [opts (walk/keywordize-keys opts)]
     (result env
             :council.read
             opts
             (council/read-entries (:db-info env) (str (:session-id env)) opts)))))

(defn get
  "Get a full entry by entry_id. A truncated ping never changes the stored content."
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
                         {:name p})
                       params)
         :description (:doc (meta v))
         :result
         (str
           "Members: `{session_id, title, state}`. Entries: `{entry_id, thread_id, group_id, content, "
           "author_session_id, created_at, source, ping}`, plus the root `title`, the host "
           "`source_ref` and optional `reply_required`, `replies` or `reply_to`. Replies report "
           "`{session_id, state, reply_entry_id?}`. Pages: `{entries, after, has_more}`. Thread "
           "summaries: `{thread_id, title, author_session_id, created_at}`.")}))
    [[#'members 'council.members :observation ["group_id"]]
     [#'publish 'council.publish :external
      ["group_id" "thread_id" "title" "ping" "idempotency_key" "reply_required" "reply_to" "files"]
      ["content"]] [#'threads 'council.threads :observation ["group_id" "after" "limit"]]
     [#'read 'council.read :observation ["group_id" "thread_id" "after" "limit"]]
     [#'get 'council.get :observation ["group_id"] ["entry_id"]]]))

(defn context
  [env]
  (when (council/enabled? env)
    (try {"default_group_id" (council/default-group (:db-info env) (:session-id env))
          "pending_replies" (wire/->wire (council/session-pending-replies env))}
         (catch Exception _ nil))))
