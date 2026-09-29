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
  "Publish a Council entry. `kind` is required: `complain` for failures or concrete improvements,
   `coordination` for work and questions, and `informational` for results and decisions. When
   Improve is enabled, complaints enter the persistent improve register. `kind` never selects
   recipients. Choose individual pings, all, or none.

   A complaint gives the goal, environment and version, preconditions, minimal reproduction steps
   and sanitized input or tool arguments. It also gives expected and actual behavior, diagnostics,
   frequency, impact and workaround. Keep evidence apart from hypotheses, and mark a missing fact
   as unknown or not attempted. Name the affected session and its turn/iteration/form. `source_ref`
   identifies this publication, not another execution.

   Use `read_session(session_id)` to inspect the original evidence. Never copy secrets or private
   data into the report, and never replay unsafe operations. With Improve enabled, a failed
   python_execution is already recorded as an autocomplain without pings. That record has the
   failure or timeout, the duration and a source-session lookup. Add to its thread with an
   informational continuation instead of a duplicate.

   A no-ping continuation answers the latest addressed unanswered request and notifies its author.
   `reply_to` selects an unanswered request explicitly, once per recipient. `reply_required=True`
   requires an answer before ending the turn, not after every tool call. Read or do authorized work
   first. Acceptance is not task completion.

   Council is asynchronous message passing. Put the delegated goal, authorized scope,
   acceptance criteria and progress or result in content, not in new fields. Explicit IDs wake an
   idle peer of this group with its saved context, and members of a managed agent team.
   `ping=\"all\"` selects active peers only. A peer that is already running reads the ping inside
   its turn only with `reply_required=True`.

   With Subagents enabled, spawn owned children with `council.publish_spawn` instead of treating
   project peers as subagents. See `doc(\"council\")` for the delivery and work protocols."
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
                         (cond-> {:name p}
                           (= p "kind")
                           (assoc :required? true)))
                       params)
         :description (:doc (meta v))
         :result
         (str
           "Members: `{session_id, title, state}`. Entries: `{entry_id, thread_id, group_id, kind, "
           "content, author_session_id, created_at, source, ping}`, plus the root `title`, the host "
           "`source_ref` and optional `reply_required`, `replies` or `reply_to`. Replies report "
           "`{session_id, state, reply_entry_id?}`. Pages: `{entries, after, has_more}`. Thread "
           "summaries: `{thread_id, kind, title, author_session_id, created_at}`.")}))
    [[#'members 'council.members :observation ["group_id"]]
     [#'publish 'council.publish :external
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
