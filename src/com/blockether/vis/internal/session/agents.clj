(ns com.blockether.vis.internal.session.agents
  "Persisted leader/subagent ownership. Project membership is not wake authority."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.config.toggles :as toggles]
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
  "Only a managed team can automatically resume a session. A child may return to
   its leader; siblings must share a task team. Replies cannot bypass this rule."
  [db author-id recipient-id]
  (let [author-id
        (str author-id)

        recipient-id
        (str recipient-id)

        author
        (info db author-id)

        recipient
        (info db recipient-id)]

    (boolean (and (toggles/enabled? "subagents")
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
                           (= (:team_id author) (:team_id recipient))))))))

(def limits (get (document/load! "agents") "limits"))

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

(defn- tool-result
  [env op opts]
  (extension/success {:op ({:spawn :council.publish_spawn
                            :list :council.subagents
                            :cancel :council.cancel
                            :route :council.route}
                           op)
                      :result (wire/->wire (operation! env op opts))}))

(defn spawn
  "Spawn a managed subagent with your full current model context and a delegated task.
   Use a concrete goal, scope and acceptance criteria. Fresh runtime, shared checkout (not isolation).
   A safe model checkpoint is required. Optional provider/model pair, allowed_models,
   iteration_budget (default 32, max 200), and retry key. Limits: depth 2, 32 children/task, 8 active.
   Reusing a key with different inputs fails. Inspect results with council.subagents and Council."
  ([env task] (spawn env task {}))
  ([env task opts] (tool-result env :spawn (assoc (walk/keywordize-keys opts) :task task))))

(defn list-agents
  "List your managed team: lineage, task, lifecycle, model, usage and pending input. Independent leaders are not children."
  ([env] (list-agents env {}))
  ([env opts] (tool-result env :list opts)))

(defn cancel
  "Cancel an owned subagent and its descendants, including queued work. Cancellation is durable."
  ([env session-id] (cancel env session-id {}))
  ([env session-id opts] (tool-result env :cancel (assoc opts :session_id session-id))))

(defn route
  "Choose a provider/model for yourself or an owned child at the next model-call boundary.
   The shared router is unchanged. Human model locks and inherited allowed_models cannot be bypassed."
  ([env model] (route env model {}))
  ([env model opts] (tool-result env :route (assoc (walk/keywordize-keys opts) :model model))))

(def symbols
  (mapv
    (fn [[v sym positional params]]
      (extension/symbol
        v
        (cond->
          {:symbol sym
           :inject-env? true
           :active-fn (fn [_]
                        (toggles/enabled? "subagents"))
           :tag (if (= sym 'council.subagents) :observation :mutation)
           :activity (presenter/for-tool (keyword (str sym)))
           :call {:pos positional :rest :always}
           :description (:doc (meta v))
           :result
           "Managed agent records with session_id, parent_id, leader_id, team_id, task, status, model and iteration usage."}
          (seq params)
          (assoc :params
            (mapv (fn [p]
                    {:name p :required? (and (= sym 'council.route) (= p "provider"))})
                  params)))))
    [[#'spawn 'council.publish_spawn ["task"]
      ["model" "provider" "iteration_budget" "allowed_models" "key"]]
     [#'list-agents 'council.subagents [] []] [#'cancel 'council.cancel ["session_id"] []]
     [#'route 'council.route ["model"] ["provider" "session_id"]]]))

(defn routing-change
  [env before]
  (let [pref (smodel/model-of (:db-info env) (:session-id env))]
    (when (not= pref before) {:preference pref})))

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
    "## Leadership and managed subagents
- session['agent'] is host-owned lineage and policy. Without a parent you are the leader: own the user's scope, integration, verification and final answer.
- For independent parallel work, use council.publish_spawn(task, ...) rather than waking another leader. Delegate a bounded goal, authorized scope, acceptance criteria and budget; inspect council.subagents(), verify evidence and integrate the result. A sent task or acknowledgement is not completion.
- A subagent inherits the parent's full current visible/folded context at a safe checkpoint, not Python handles. Inherited conversation is background evidence, not permission to resume the parent's task. Work only on your delegated task and report results/blockers to your parent through Council.
- Children share the checkout. Divide file ownership; spawning does not authorize new worktrees, external actions or broader access. Cancel unneeded children with council.cancel(session_id).
- Automatic wakes are restricted to managed teams: leader-to-child, child-to-leader and same-team children. Independent leaders NEVER wake one another, including replies and same-thread follow-ups. Active leaders may exchange messages; project membership is not leadership.
- council.route(model, provider=..., session_id=...) changes only your session or an owned child at the next request boundary. Respect human locks, inherited model allowlists and iteration budgets. Changing models can lose provider cache reuse; it never creates a new global router."))
