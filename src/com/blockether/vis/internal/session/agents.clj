(ns com.blockether.vis.internal.session.agents
  "Persisted leader/subagent ownership. An explicit ping resumes an independent peer; a managed session stays under its team rule."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.renderer :as ctx-renderer]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.session.model :as smodel]))

(defn info [db sid] (when (and db sid) (ps/db-agent-info db sid)))

(defn- wakeable?
  [agent]
  (and agent
       (not (contains? #{"cancelled" "budget_limited"} (:status agent)))
       (< (long (:iterations_used agent)) (long (:iteration_budget agent)))))

(defn wake-allowed?
  "Automatic resume has two sources: an explicit ping between independent sessions, and
   a managed team. A child may return to its leader; siblings must share a task team.
   A managed session keeps its team rule, so a cancelled or exhausted subagent stays idle.
   Only a managed subagent wakes itself; an independent session stays idle."
  [db author-id recipient-id]
  (let [author-id
        (str author-id)

        recipient-id
        (str recipient-id)

        author
        (info db author-id)

        recipient
        (info db recipient-id)]

    (boolean (or (and (nil? author) (nil? recipient) (not= author-id recipient-id))
                 (and (toggles/enabled? "subagents")
                      (or (and (wakeable? recipient) (= author-id (:leader_id recipient)))
                          ;; Returning an outcome costs no further child iteration.
                          (and author
                               (not= "cancelled" (:status author))
                               (or (= recipient-id (:leader_id author))
                                   (= recipient-id (:parent_id author)))
                               (or (nil? recipient) (wakeable? recipient)))
                          (and (wakeable? author)
                               (wakeable? recipient)
                               (= (:leader_id author) (:leader_id recipient))
                               (= (:team_id author) (:team_id recipient)))))))))

(def ^:private schema (document/schema-document "agents"))

(def max-depth (get-in schema ["$defs" "agent" "properties" "depth" "maximum"]))

(def default-iterations (get-in schema ["$defs" "spawn" "properties" "iteration_budget" "default"]))

(def max-team-children (get schema "x-vis-max-team-children"))

(def max-active-children (get schema "x-vis-max-active-children"))

(defonce ^:private runtime (atom {}))

(defn install-runtime! [callbacks] (reset! runtime callbacks) nil)

(defn context
  [env]
  (let [sid
        (str (:session-id env))

        child
        (info (:db-info env) sid)]

    (wire/->wire (merge {:role (if child "subagent" "leader") :session_id sid :leader_id sid}
                        (select-keys child
                                     [:parent_id :leader_id :team_id :task :depth :iteration_budget
                                      :iterations_used :status :allowed_models])
                        {:routing_locked (ps/db-routing-locked? (:db-info env) sid)}))))

(defn checkpoint!
  "Capture the last complete model input, before its in-flight tool call. Never copy runtime handles."
  [env messages]
  (when (:turn-state-atom env) (ctx-loop/set-turn-state! env :agent-checkpoint (vec messages)))
  nil)

(defn inherited-base
  [env turn-position current-messages summaries]
  (let [db
        (:db-info env)

        sid
        (:session-id env)

        child
        (info db sid)]

    (when (and child
               (zero? (long (:version (ps/db-get-session db sid))))
               (= (long turn-position) (inc (long (:inherited_turns child)))))
      {:messages (into (cond-> (vec (ps/db-agent-checkpoint db sid))
                         (:ctx-atom env)
                         (conj {:role "user"
                                :content
                                (str ";; -- CURRENT-SUBAGENT-CONTEXT --\n"
                                     (ctx-loop/render-block! env ctx-renderer/render-ctx-static))}))
                       current-messages)
       :summaries summaries
       :resumed? true})))

(defn claim-iteration!
  "Reserve one model iteration. A child budget is durable across replies, turns and restarts."
  [env]
  (let [db
        (:db-info env)

        sid
        (:session-id env)]

    (if (info db sid)
      (if (and (toggles/enabled? "subagents") (ps/db-agent-claim-iteration! db sid))
        true
        (let [child (info db sid)]
          (when (and (toggles/enabled? "subagents") (not= "cancelled" (:status child)))
            (ps/db-agent-update! db
                                 sid
                                 {:status (if (>= (long (:iterations_used child))
                                                  (long (:iteration_budget child)))
                                            "budget_limited"
                                            "failed")}))
          false))
      true)))

(defn finish!
  [db sid status]
  (when-let [child (info db sid)]
    (when-not (contains? #{"cancelled" "budget_limited"} (:status child))
      (ps/db-agent-update! db
                           sid
                           {:status (case status
                                      ("completed" "success")
                                      "completed"

                                      "cancelled"
                                      "cancelled"

                                      "failed")}))))

(defn reconcile-orphans!
  "Fail the children that a dead process left queued or running. Spare a child or a leader
   that `live-sids` names: a live process still owns that team. Returns the count."
  [db live-sids]
  (let [live
        (into #{} (map str) live-sids)

        orphans
        (filterv #(and (contains? #{"queued" "running"} (:status %))
                       (not (live (str (:session_id %))))
                       (not (live (str (:leader_id %)))))
          (ps/db-agent-list db nil))]

    (doseq [{sid :session_id} orphans]
      (ps/db-agent-update! db sid {:status "failed"}))
    (count orphans)))

(defn controls?
  "Leaders control their descendants. Subagents control themselves and their direct children."
  [db actor-id target-id]
  (let [actor-id
        (str actor-id)

        target-id
        (str target-id)

        child
        (info db target-id)]

    (or (= actor-id target-id) (= actor-id (:parent_id child)) (= actor-id (:leader_id child)))))

(defn fail! [error message] (throw (ex-info message {:error error})))

(defn require-enabled!
  "Refuse new managed work before bootstrapping a session or consuming a checkpoint."
  []
  (when-not (toggles/enabled? "subagents")
    (fail! :feature-disabled "Subagents are disabled. Enable Subagents in Experimental settings.")))

(defn operation!
  [env operation opts]
  (let [opts (walk/keywordize-keys opts)]
    ;; Reads and cancellation remain available for previously created teams.
    (when (or (= :spawn operation)
              (and (= :route operation)
                   (:session_id opts)
                   (not= (str (:session-id env)) (:session_id opts))))
      (require-enabled!))
    (when-not (document/valid-json? "agents" (name operation) (wire/->wire opts))
      (fail! :invalid-request (str "Invalid subagent " (name operation) " arguments")))
    (when (and (:task opts) (str/blank? (:task opts)))
      (fail! :invalid-request "A subagent needs a nonblank delegated task"))
    (when-let [target (:session_id opts)]
      (when-not (controls? (:db-info env) (:session-id env) target)
        (fail! :not-owner "This session does not own that subagent")))
    (if-let [f (get @runtime operation)]
      (f env opts)
      (fail! :unavailable "Managed subagents require the gateway runtime"))))

(defn- result [op value] (extension/success {:op op :result (wire/->wire value)}))

(def ^:private terminal-statuses #{"completed" "failed" "cancelled" "budget_limited" "lost"})

;; Open child questions by child session id: {:question text :entry_id id}.
(defonce ^:private questions (atom {}))

(defn- runtime-fn
  [k]
  (or (get @runtime k) (fail! :unavailable "Managed subagents require the gateway runtime")))

(defn- final-answer
  [db sid]
  (when-let [row (last (ps/db-list-session-turns db sid))]
    (some->> (:content row)
             (filterv #(contains? #{"prose" "speech"} (get % "type")))
             content/text-projection
             str/trim
             not-empty)))

(defn- record
  "One wait record. A queued or running child without a live turn after 30 s is lost."
  [db live? now row]
  (let [sid
        (str (:session_id row))

        status
        (:status row)

        open?
        (contains? #{"queued" "running"} status)

        status
        (if (and open? (> (- (long now) (long (or (:created_at row) now))) 30000) (not (live? sid)))
          "lost"
          status)

        open?
        (contains? #{"queued" "running"} status)

        question
        (when open? (get @questions sid))]

    (cond-> {:session_id sid
             :status (if question "asking" status)
             :answer (when (terminal-statuses status) (final-answer db sid))
             :question (:question question)
             :question_entry_id (:entry_id question)}
      (:iterations_used row)
      (assoc :iterations_used (:iterations_used row)))))

(defn- wait-records
  "Poll until every child ends, one child asks, or the timeout ends. A completed child gets 5 s to
   store its answer."
  [env ids timeout-secs]
  (let [list-fn
        (runtime-fn :list)

        live?
        (runtime-fn :live?)

        db
        (:db-info env)

        me
        (str (:session-id env))

        ids
        (when (seq ids) (set (map str ids)))

        deadline
        (+ (System/currentTimeMillis) (* 1000 (long timeout-secs)))

        pick
        (fn []
          (let [rows
                (list-fn env {})

                rows
                (if ids
                  (filter #(contains? ids (str (:session_id %))) rows)
                  (filter #(= me (str (:parent_id %))) rows))

                seen
                (set (map #(str (:session_id %)) rows))]

            (concat rows
                    (for [i
                          (sort ids)

                          :when (not (seen i))]

                      {:session_id i :status "unknown"}))))]

    (loop [grace-until nil]
      (let [now (System/currentTimeMillis)
            recs (mapv #(record db live? now %) (pick))
            done? (every? #(or (terminal-statuses (:status %)) (= "unknown" (:status %))) recs)
            answered? (every? #(or (not= "completed" (:status %)) (:answer %)) recs)]

        (if (or (some #(= "asking" (:status %)) recs)
                (and done? (or answered? (and grace-until (> now (long grace-until)))))
                (> now deadline))
          recs
          (do (Thread/sleep 1000) (recur (if done? (or grace-until (+ now 5000)) grace-until))))))))

(defn- wait-timeout
  [opts default]
  (let [t (or (:timeout_secs opts) default)]
    (when-not (and (number? t) (<= 1 t 1800))
      (fail! :invalid-request "timeout_secs must be 1..1800"))
    t))

(defn- task-spec
  [t]
  (cond (string? t) {:task t}
        (map? t) (select-keys (walk/keywordize-keys t)
                              [:task :model :provider :iteration_budget :context])
        :else (fail! :invalid-request "Each task must be a string or a dict with task")))

(defn run
  "Run each task in its own subagent and wait for all of them. A task is a string or a dict with
   task and optional model, provider, iteration_budget and context. A child uses your model and
   your full context. A child on another model gets only the system prompt and its task, unless
   context is \"full\". Children share the checkout.

   Records have session_id, status, answer and question. More than 16 tasks start as earlier ones
   finish. timeout_secs defaults to 240 (max 1800). If a status is still running, call
   council.wait. lost means the child stopped without an answer."
  ([env tasks] (run env tasks {}))
  ([env tasks opts]
   (let [opts
         (walk/keywordize-keys opts)

         specs
         (mapv task-spec (if (or (string? tasks) (map? tasks)) [tasks] tasks))

         _
         (when (empty? specs) (fail! :invalid-request "tasks must not be empty"))

         timeout
         (wait-timeout opts 240)

         started
         (System/currentTimeMillis)

         spawn
         (fn [spec]
           (loop []

             (let [r (try (str (:session_id (operation! env :spawn spec)))
                          (catch clojure.lang.ExceptionInfo e
                            ;; A full team admits the next child when an earlier one ends.
                            (if (and (= :agent-limit (:error (ex-data e)))
                                     (< (- (System/currentTimeMillis) started)
                                        (* 1000 (long timeout))))
                              ::full
                              (throw e))))]
               (if (= ::full r) (do (Thread/sleep 1000) (recur)) r))))

         ids
         (mapv spawn specs)

         left
         (max 1 (- (long timeout) (quot (- (System/currentTimeMillis) started) 1000)))]

     (result :council.run (wait-records env ids left)))))

(defn wait-agents
  "Wait again for subagents by session_ids (default: all your children). Same records and timeout as council.run."
  ([env] (wait-agents env nil {}))
  ([env ids-or-opts]
   (if (map? ids-or-opts) (wait-agents env nil ids-or-opts) (wait-agents env ids-or-opts {})))
  ([env ids opts]
   (let [opts
         (walk/keywordize-keys opts)

         ids
         (if (string? ids) [ids] ids)]

     (result :council.wait (wait-records env ids (wait-timeout opts 240))))))

(defn cancel-agent
  "Cancel a subagent and its descendants, including queued work. Returns its record."
  [env session-id]
  (operation! env :cancel {:session_id session-id})
  (result :council.cancel (wait-records env [session-id] 5)))

(defn ask
  "As a subagent, ask your parent for a decision and wait for the reply text. Returns None when
   the parent is not running or does not reply within timeout_secs (default 600). Then choose the
   safest option and say so in your answer."
  ([env question] (ask env question {}))
  ([env question opts]
   (let [db
         (:db-info env)

         sid
         (str (:session-id env))

         me
         (info db sid)

         _
         (when-not me (fail! :invalid-request "council.ask works only in a subagent"))

         _
         (when (or (not (string? question)) (str/blank? question))
           (fail! :invalid-request "question must be a nonblank string"))

         timeout
         (wait-timeout (walk/keywordize-keys opts) 600)

         deadline
         (+ (System/currentTimeMillis) (* 1000 (long timeout)))

         parent
         (str (:parent_id me))

         live?
         (runtime-fn :live?)]

     (if-not (live? parent)
       (result :council.ask nil)
       (let [entry
             ((runtime-fn :publish)
               env
               {:kind "coordination" :content question :ping [parent] :reply_required true})

             entry-id
             (:entry_id entry)]

         (swap! questions assoc sid {:question question :entry_id entry-id})
         (try (loop []

                (let [reply-id (some :reply_entry_id (:replies (ps/db-council-get db entry-id)))]
                  (cond reply-id (result :council.ask (:content (ps/db-council-get db reply-id)))
                        (or (> (System/currentTimeMillis) deadline)
                            (= "cancelled" (:status (info db sid)))
                            (not (live? parent)))
                        (result :council.ask nil)
                        :else (do (Thread/sleep 1000) (recur)))))
              (finally (swap! questions #(if (= entry-id (get-in % [sid :entry_id]))
                                           (dissoc % sid)
                                           %)))))))))

(def symbols
  (mapv
    (fn [[v sym call params result-doc]]
      (extension/symbol v
                        (cond-> {:symbol sym
                                 :inject-env? true
                                 :active-fn (fn [_]
                                              (toggles/enabled? "subagents"))
                                 :tag (if (= sym 'council.wait) :observation :mutation)
                                 :activity (presenter/for-tool (keyword (str sym)))
                                 :call call
                                 :description (:doc (meta v))
                                 :result result-doc}
                          (seq params)
                          (assoc :params
                            (mapv (fn [p]
                                    {:name p :required? false})
                                  params)))))
    (let
      [records
       "A list of records with `session_id`, `status`, `answer`, `question` and `question_entry_id`."]
      [[#'run 'council.run {:pos ["tasks"] :rest :always} ["timeout_secs"] records]
       [#'wait-agents 'council.wait {:opt-pos ["session_ids"] :rest :always} ["timeout_secs"]
        records] [#'cancel-agent 'council.cancel {:pos ["session_id"]} [] records]
       [#'ask 'council.ask {:pos ["question"] :rest :always} ["timeout_secs"]
        "A plain string with the parent's reply, or None."]])))

(defn routing-change
  "The route command issued for this session since `before`, as
   `{:preference {:provider :model} :command command}`, or nil. Agent routing and the
   engine's authentication rescue issue commands; a manual pick is composer state that
   the next submitted turn snapshots, so it never re-routes a running turn."
  [env before]
  (let [command (smodel/route-command (:session-id env))]
    (when (and command (not= command before))
      {:preference (select-keys command [:provider :model]) :command command})))

(defn restrict-router
  "Apply an inherited allowlist to every request and retry, including automatic fallback."
  [env router]
  (if-let [allowed (seq (:allowed_models (info (:db-info env) (:session-id env))))]
    (assoc router
      :providers
      (into []
            (keep (fn [provider]
                    (let [models (filterv (fn [model]
                                            (some
                                              #(and (= (name (:id provider)) (:provider %))
                                                    (= (if (map? model) (:name model) (str model))
                                                       (:model %)))
                                              allowed))
                                   (:models provider))]
                      (when (seq models)
                        (assoc provider
                          :models models
                          :root (if (some #(= (:root provider) (:name %)) models)
                                  (:root provider)
                                  (:name (first models))))))))
            (:providers router)))
    router))

(defn prompt
  [_env]
  (when (toggles/enabled? "subagents")
    "## Subagents (Council)
- Use subagents only when the user asks or the work has two or more independent parts. Do critical-path work, small tasks and single-file edits yourself.
- For independent parallel work, call council.run([task, ...]). Each task must name its files, scope and done condition. Children share the checkout: give each its own files.
- A child uses your model and your context. Set model and provider in a task dict only when the user or project names a model for that work. A child on another model gets only the system prompt and its task, so write a task that stands alone.
- Each record has session_id, status, answer, question and question_entry_id. A status of running means the timeout ended first: call council.wait(session_ids=[...]). Status lost means the child stopped without an answer. Verify answers before you use them.
- Status asking means a child waits for your decision: answer with council.publish(text, kind=\"informational\", reply_to=question_entry_id), then call council.wait again.
- As a subagent, do only your task and end with a short answer for your parent. If you need a decision that your task does not give, call council.ask(question) and follow the reply; if it returns None, choose the safest option and say so. Use council.publish for other messages to your parent or siblings."))
