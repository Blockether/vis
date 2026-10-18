(ns com.blockether.vis.internal.session.agents
  "Persisted leader/subagent ownership. An explicit ping resumes an independent peer; a managed session stays under its team rule."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.renderer :as ctx-renderer]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.internal.util :as util]))

(defn info [db sid] (when (and db sid) (ps/db-agent-info db sid)))

(defn leader-id
  "The leader of session `sid` as a string: the team leader for a subagent, else `sid`."
  [db sid]
  (some-> (or (:leader_id (info db sid)) sid)
          str))

(defn enabled?
  "Whether session `sid` can use managed subagents. A new session starts without the session
   settings of its source, so a managed child reads the setting of its leader. Without such a
   session setting, or with an invocation value, the configuration decides."
  [db sid]
  (let [owner
        (leader-id db sid)

        row
        (when (and db
                   owner
                   (not (contains? toggles/*invocation-overrides* "subagents"))
                   (contains? (ps/db-scoped-settings db "session" owner) "subagents"))
          ;; The resolved row keeps the scope rules of the toggle.
          (try (some #(when (= "subagents" (:id %)) %)
                     (scoped/settings db (scoped/target db "session" owner)))
               (catch clojure.lang.ExceptionInfo _ nil)))]

    (if (= "session" (:source row)) (boolean (:value row)) (toggles/enabled? "subagents"))))

(defn- wakeable?
  [agent]
  (and agent
       (not= "cancelled" (:status agent))
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
                 (and (enabled? db (if recipient recipient-id author-id))
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

(def default-iterations (get-in schema ["$defs" "spawn" "properties" "iteration_budget" "default"]))

(def max-team-children (get schema "x-vis-max-team-children"))

(def max-active-children (get schema "x-vis-max-active-children"))

(defonce ^:private runtime (atom {}))

(defn install-runtime! [callbacks] (reset! runtime callbacks) nil)

;; Child session ids whose turn runs its handoff iteration. `finish!` reads it, so only a turn
;; that went past its budget ends as `budget_limited`.
(defonce ^:private handoffs (atom #{}))

(defn- budget-notice
  "Reminder for a subagent near the end of its iteration budget, or nil. It rides in
   `session[\"agent\"]`, so the model sees it as a context change after each iteration."
  [{:keys [iteration_budget iterations_used]}]
  (when (and iteration_budget iterations_used)
    (let [budget
          (long iteration_budget)

          left
          (- budget (long iterations_used))]

      (cond (<= left 0)
            (str "Your iteration budget is used. Your next response is your handoff. If the task "
                 "needs more, call council.ask_budget(iterations, reason). Else do not call other "
                 "tools: give your parent the done work, the changed files and the next step.")
            (<= left (max 5 (quot budget 5)))
            (str
              left
              " of "
              budget
              " iterations left. Finish the task, or call council.ask_budget(iterations, reason) "
              "for more. Before the budget ends, report the done work, the changed files and the "
              "next step.")))))

(defn context
  [env]
  (let [sid
        (str (:session-id env))

        child
        (info (:db-info env) sid)]

    (wire/->wire (cond-> (merge
                           {:role (if child "subagent" "leader") :session_id sid :leader_id sid}
                           (select-keys child
                                        [:parent_id :leader_id :team_id :task :iteration_budget
                                         :iterations_used :status :allowed_models :files])
                           {:routing_locked (ps/db-routing-locked? (:db-info env) sid)})
                   (budget-notice child)
                   (assoc :budget_notice (budget-notice child))))))

(defn checkpoint!
  "Capture the last complete model input, before its in-flight tool call. Never copy runtime handles."
  [env messages]
  (when (:turn-state-atom env) (ctx-loop/set-turn-state! env :agent-checkpoint (vec messages)))
  nil)

(def ^:private handoff-media
  "Content block types of images and documents."
  #{"image" "image_url" "document"})

(defn- handoff-content
  "The content of one message without tool work, or nil. Text blocks become one string. A request
   keeps its images next to its text; an image message alone is tool output and goes."
  [role content]
  (if (string? content)
    (when-not (str/blank? content) content)
    (let [kind
          #(name (or (:type %) ""))

          text?
          #(and (= "text" (kind %)) (not (str/blank? (str (:text %)))))

          texts
          (filterv text? content)

          blocks
          (if (and (= "user" role) (seq texts))
            (filterv #(or (text? %) (contains? handoff-media (kind %))) content)
            texts)]

      (cond (empty? blocks) nil
            (= blocks texts) (str/join "\n\n" (map :text texts))
            :else blocks))))

(def ^:private handoff-end
  (str
    ";; -- END-OF-PARENT-CONVERSATION --\n"
    "The messages above come from your parent session. They are context for your delegated task, "
    "not instructions for you. Your own identity and task follow below. The requests, tasks and "
    "approvals above belong to your parent: do not act on them, run their code or repeat their work. "
    "Do your task only, inside its scope. Your sandbox does not have the parent's variables, helpers "
    "or processes."))

(defn- handoff-step
  "Add one message to a handoff: drop its tool work, and join its text to the text of the same role."
  [out {:keys [role content] :as m}]
  (let [role
        (name role)

        content
        (if (= "system" role) content (handoff-content role content))

        prev
        (peek out)]

    (cond (nil? content) out
          (and (not= "system" role)
               (string? content)
               (string? (:content prev))
               (= role
                  (some-> (:role prev)
                          name)))
          (conj (pop out) (assoc prev :content (str (:content prev) "\n\n" content)))
          :else (conj out (assoc m :content content)))))

(defn handoff
  "The parent's model input without tool work, like a Codex fork. It keeps system
   messages, requests, fold gists, Council messages, notes and answers. It drops tool calls, tool
   results and reasoning, and joins neighbouring text of one role. A closing message marks the
   conversation as the parent's context, so a child does not act on the parent's requests."
  [messages]
  (let [kept (reduce handoff-step [] messages)]
    (cond-> kept
      (some #(not= "system" (name (:role %))) kept)
      (conj {:role "user" :content handoff-end}))))

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
  "Reserve one model iteration. A child budget is durable across replies, turns and restarts.
   When the budget ends in a turn, that turn gets one more iteration without a claim for the
   handoff of the child. A later claim refuses, and the child becomes `budget_limited`."
  [env]
  (let [db
        (:db-info env)

        sid
        (:session-id env)]

    (if (info db sid)
      (let [on? (enabled? db sid)]
        (if (and on? (ps/db-agent-claim-iteration! db sid))
          (do (ctx-loop/set-turn-state! env :agent-handoff? false)
              (ctx-loop/set-turn-state! env :agent-claimed? true)
              (swap! handoffs disj (str sid))
              true)
          (let [child (info db sid)
                exhausted? (>= (long (:iterations_used child)) (long (:iteration_budget child)))]

            (cond (and on?
                       exhausted?
                       (not (contains? #{"cancelled" "budget_limited"} (:status child)))
                       (:turn-state-atom env)
                       (:agent-claimed? (ctx-loop/read-turn-state env))
                       (not (:agent-handoff? (ctx-loop/read-turn-state env))))
                  (do (ctx-loop/set-turn-state! env :agent-handoff? true)
                      (swap! handoffs conj (str sid))
                      true)
                  :else (do (when (and on? exhausted? (not= "cancelled" (:status child)))
                              (ps/db-agent-update! db sid {:status "budget_limited"}))
                            false)))))
      true)))

(defn stop-reason
  "Give the cause when `claim-iteration!` refuses the child session of `env`."
  [env]
  (let [db
        (:db-info env)

        sid
        (:session-id env)

        child
        (info db sid)]

    (cond
      (not (enabled? db sid))
      "Subagent stopped: Subagents are disabled for this team. Enable Subagents in Experimental settings."
      (= "cancelled" (:status child)) "Subagent stopped: it is cancelled."
      (and child (>= (long (:iterations_used child)) (long (:iteration_budget child))))
      (str "Subagent stopped: it used its full iteration budget of " (:iteration_budget child)
           " and its handoff iteration. Its parent can add iterations with council.extend and "
           "continue it with council.resume.")
      :else (str "Subagent stopped: its iteration budget ended. Its parent can continue it with "
                 "council.resume or a Council ping."))))

(defn refusal-answer
  "The answer of a child turn that `claim-iteration!` stopped: the stop reason, then the last
   report of the turn. Without a report, it names where the parent can read the work."
  [env report]
  (str (stop-reason env)
       "\n\n"
       (if (util/non-blank-string? report)
         (str "Last report:\n" report)
         (str "The subagent gave no report. Read its work with read_session(\""
              (:session-id env)
              "\")."))))

(defn finish!
  "Store the status of a finished child turn. A child that ended its turn in its handoff
   iteration becomes `budget_limited`, so its parent knows that it needs council.extend before
   council.resume. A child that answered within its budget is `completed`."
  [db sid status]
  (let [handoff? (contains? (first (swap-vals! handoffs disj (str sid))) (str sid))]
    (when-let [child (info db sid)]
      (when-not (contains? #{"cancelled" "budget_limited"} (:status child))
        (ps/db-agent-update! db
                             sid
                             {:status (cond (= "cancelled" status) "cancelled"
                                            handoff? "budget_limited"
                                            (contains? #{"completed" "success"} status) "completed"
                                            :else "failed")})))))

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
  "A leader controls its children. A subagent controls only itself."
  [db actor-id target-id]
  (let [actor-id
        (str actor-id)

        target-id
        (str target-id)

        child
        (info db target-id)]

    (or (= actor-id target-id) (= actor-id (:parent_id child)))))

(defn fail! [error message] (throw (ex-info message {:error error})))

(defn require-enabled!
  "Refuse new managed work before bootstrapping a session or consuming a checkpoint."
  [db sid]
  (when-not (enabled? db sid)
    (fail! :feature-disabled "Subagents are disabled. Enable Subagents in Experimental settings.")))

(defn- checked
  "Validate `opts` for `operation` and the ownership of its target session. Answers the
   keywordized opts."
  [env operation opts]
  (let [opts (walk/keywordize-keys opts)]
    ;; Reads and cancellation remain available for previously created teams.
    (when (or (contains? #{:spawn :resume} operation)
              (and (= :route operation)
                   (:session_id opts)
                   (not= (str (:session-id env)) (:session_id opts))))
      (require-enabled! (:db-info env) (:session-id env)))
    ;; Only a leader delegates; a subagent asks its parent for more subagents.
    (when (and (= :spawn operation) (info (:db-info env) (:session-id env)))
      (fail! :not-allowed
             (str "A subagent cannot start subagents. End your turn with an answer that asks your "
                  "parent for them: name each part, its files and its done condition.")))
    (when-not (document/valid-json? "agents" (name operation) (wire/->wire opts))
      (fail! :invalid-request (str "Invalid subagent " (name operation) " arguments")))
    (when (and (:task opts) (str/blank? (:task opts)))
      (fail! :invalid-request "A subagent needs a nonblank delegated task"))
    (when-let [target (:session_id opts)]
      (when-not (controls? (:db-info env) (:session-id env) target)
        (fail! :not-owner "This session does not own that subagent")))
    opts))

(defn operation!
  [env operation opts]
  (let [opts (checked env operation opts)]
    (if-let [f (get @runtime operation)]
      (f env opts)
      (fail! :unavailable "Managed subagents require the gateway runtime"))))

(defn- result [op value] (extension/success {:op op :result (wire/->wire value)}))

(def ^:private terminal-statuses #{"completed" "failed" "cancelled" "budget_limited" "lost"})

;; Open child questions by child session id: {:question text :entry_id id}, plus
;; `:budget {:iterations n :reason text}` for a budget request or `:files {:files grant :reason
;; text}` for a file request. They stay in memory: the asking child blocks in this process, so a
;; restart ends the question with its turn.
(defonce ^:private questions (atom {}))

(defn- runtime-fn
  [k]
  (or (get @runtime k) (fail! :unavailable "Managed subagents require the gateway runtime")))

(defn- final-answer
  "The prose answer of the last turn of child `row`, or nil. After council.resume, only a turn
   of the follow-up task counts: `resumed_turns` in the row is the turn count before it."
  [db row]
  (let [turns (ps/db-list-session-turns db (:session_id row))]
    (when (> (count turns) (long (or (:resumed_turns row) 0)))
      (some->> (:content (last turns))
               (filterv #(contains? #{"prose" "speech"} (get % "type")))
               content/text-projection
               str/trim
               not-empty))))

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
        (if (and open?
                 (> (- (long now)
                       (max (long (or (:created_at row) now)) (long (or (:resumed_at row) 0))))
                    30000)
                 (not (live? sid)))
          "lost"
          status)

        open?
        (contains? #{"queued" "running"} status)

        question
        (when open? (get @questions sid))]

    (cond-> {:session_id sid
             :status (if question "asking" status)
             :answer (when (terminal-statuses status) (final-answer db row))
             :question (:question question)
             :question_entry_id (:entry_id question)}
      (:budget question)
      (assoc :budget_request (:budget question))

      (:files question)
      (assoc :files_request (:files question))

      (:iteration_budget row)
      (assoc :iteration_budget (:iteration_budget row))

      (:iterations_used row)
      (assoc :iterations_used (:iterations_used row))

      (:files row)
      (assoc :files (:files row)))))

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
        (+ (util/now-ms) (* 1000 (long timeout-secs)))

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
      (let [now (util/now-ms)
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
                              [:task :model :provider :iteration_budget :files])
        :else (fail! :invalid-request "Each task must be a string or a dict with task")))

(defn run
  "Run each task in its own subagent and wait for all of them. A task is a string or a dict with
   task and optional model, provider, iteration_budget and files. iteration_budget defaults to
   120 (max 600). Give a large task more at the start.

   files is a file grant: {\"write\": [...], \"read_only\": [...], \"deny\": [...]}. Each rule is a
   path or a glob, relative to your workspace root, ~ or absolute, and also covers files that do
   not exist yet. With write, the child can change only the files that its rules match.

   A child gets the system prompt, your requests, notes and answers, and its task. It does not get
   your tool calls or their output. Children share the checkout. Only a leader starts subagents.

   Records have session_id, status, answer and question. More than 16 tasks start as earlier ones
   finish. timeout_secs defaults to 240 (max 1800). If a status is still running, call
   council.wait. lost means the child stopped without an answer.

   asking with budget_request needs council.extend or council.deny. asking with files_request
   needs council.grant or council.deny. budget_limited needs council.extend before council.resume,
   and it stays budget_limited until council.resume."
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
         (util/now-ms)

         spawn
         (fn [spec]
           (loop []

             (let [r (try (str (:session_id (operation! env :spawn spec)))
                          (catch clojure.lang.ExceptionInfo e
                            ;; A full team admits the next child when an earlier one ends.
                            (if (and (= :agent-limit (:error (ex-data e)))
                                     (< (- (util/now-ms) started) (* 1000 (long timeout))))
                              ::full
                              (throw e))))]
               (if (= ::full r) (do (Thread/sleep 1000) (recur)) r))))

         ids
         (mapv spawn specs)

         left
         (max 1 (- (long timeout) (quot (- (util/now-ms) started) 1000)))]

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
  "Cancel a subagent, including its queued work. Returns its record."
  [env session-id]
  (operation! env :cancel {:session_id session-id})
  (result :council.cancel (wait-records env [session-id] 5)))

(defn- parent-reply!
  "Publish `question` to the parent of the calling child and wait for the reply text. Answers
   nil when the parent is not running, the child is cancelled or `timeout_secs` (default 600)
   ends. `request` adds a budget or file request to the open question: `{:budget …}` or
   `{:files …}`."
  [env op question request opts]
  (let [db
        (:db-info env)

        sid
        (str (:session-id env))

        me
        (info db sid)

        _
        (when-not me (fail! :invalid-request (str op " works only in a subagent")))

        timeout
        (wait-timeout (walk/keywordize-keys opts) 600)

        deadline
        (+ (util/now-ms) (* 1000 (long timeout)))

        parent
        (str (:parent_id me))

        live?
        (runtime-fn :live?)]

    (when (live? parent)
      (let [entry
            ((runtime-fn :publish)
              env
              {:kind "coordination" :content question :ping [parent] :reply_required true})

            entry-id
            (:entry_id entry)]

        (swap! questions assoc sid (merge {:question question :entry_id entry-id} request))
        (try (loop []

               (let [reply-id (some :reply_entry_id (:replies (ps/db-council-get db entry-id)))]
                 (cond reply-id (:content (ps/db-council-get db reply-id))
                       (or (> (util/now-ms) deadline)
                           (= "cancelled" (:status (info db sid)))
                           (not (live? parent)))
                       nil
                       :else (do (Thread/sleep 1000) (recur)))))
             (finally (swap! questions #(if (= entry-id (get-in % [sid :entry_id]))
                                          (dissoc % sid)
                                          %))))))))

(defn ask
  "As a subagent, ask your parent for a decision and wait for the reply text. Returns None when
   the parent is not running or does not reply within timeout_secs (default 600). Then choose the
   safest option and say so in your answer."
  ([env question] (ask env question {}))
  ([env question opts]
   (when (or (not (string? question)) (str/blank? question))
     (fail! :invalid-request "question must be a nonblank string"))
   (result :council.ask (parent-reply! env "council.ask" question nil opts))))

(def ^:private max-budget
  (get-in schema ["$defs" "spawn" "properties" "iteration_budget" "maximum"]))

(defn ask-budget
  "As a subagent, ask your parent for more model iterations and wait for the answer. Ask before
   your budget ends, with the number that you need and the reason. Returns a dict with granted
   (0 when the parent refuses), iteration_budget and reply. reply is None when the parent is not
   running or does not answer within timeout_secs (default 600)."
  ([env iterations reason] (ask-budget env iterations reason {}))
  ([env iterations reason opts]
   (when-not (and (integer? iterations) (<= 1 (long iterations) (long max-budget)))
     (fail! :invalid-request (str "iterations must be an integer from 1 to " max-budget)))
   (when (or (not (string? reason)) (str/blank? reason))
     (fail! :invalid-request "reason must be a nonblank string"))
   (let [db
         (:db-info env)

         sid
         (str (:session-id env))

         before
         (:iteration_budget (info db sid))

         reply
         (parent-reply! env
                        "council.ask_budget"
                        (str "Budget request: " iterations " more iterations. Reason: " reason)
                        {:budget {:iterations iterations :reason reason}}
                        opts)

         after
         (:iteration_budget (info db sid))

         state
         (ctx-loop/read-turn-state env)]

     ;; A request in the handoff iteration used that iteration. Without new iterations, the
     ;; turn gets one more handoff iteration, once, so the child can still report its state.
     (when (and (<= (long after) (long before))
                (:agent-handoff? state)
                (not (:agent-handoff-renewed? state)))
       (ctx-loop/set-turn-state! env :agent-handoff? false :agent-handoff-renewed? true))
     (result
       :council.ask_budget
       {:granted (max 0 (- (long after) (long before))) :iteration_budget after :reply reply}))))

(defn- covers?
  "True when the granted write rule `rule` names the resolved path `path`: the same path, an
   ancestor directory or a glob that matches it."
  [^String rule ^String path]
  (or (= rule path)
      (str/starts-with? path (str rule "/"))
      (and (re-find #"[*?\[{]" rule)
           (try (.matches (.getPathMatcher (java.nio.file.FileSystems/getDefault)
                                           (str "glob:" rule))
                          (java.nio.file.Paths/get path (make-array String 0)))
                (catch Exception _ false)))))

(defn- grant-text
  "One line that names the rules of a file grant `files`."
  [files]
  (str/join "; "
            (for [k
                  [:write :read_only :deny]

                  :let [rules
                        (get files k (get files (name k)))]
                  :when (seq rules)]

              (str (name k) " " (str/join ", " rules)))))

(defn ask-files
  "As a subagent, ask your parent for file access and wait for the answer. files holds write,
   read_only and deny rules: paths or globs, absolute, ~ or relative to the parent's workspace
    root. Returns a dict with granted, files (your grant now), missing and reply. granted is True
   only when the grant covers every write rule that you asked for; missing lists the others. reply
   is None when the parent is not running or does not answer within timeout_secs (default 600).
   After a grant, your Python sandbox restarts before the next block and restores your saved
   helpers."
  ([env files reason] (ask-files env files reason {}))
  ([env files reason opts]
   (let [{:keys [files reason]}
         (checked env :ask_files {:files files :reason reason})

         db
         (:db-info env)

         sid
         (str (:session-id env))

         before
         (:files (info db sid))

         reply
         (parent-reply! env
                        "council.ask_files"
                        (str "File request: " (grant-text files) ". Reason: " reason)
                        {:files {:files files :reason reason}}
                        opts)

         after
         (:files (info db sid))

         security
         (:security-policy env)

         ;; A real run granted another file and still answered granted: True.
         missing
         (vec (remove (fn [path]
                        (some #(covers? % path) (:write after)))
                (policy/resolve-rules (:write (walk/keywordize-keys files))
                                      (or (:base-dir security) (System/getProperty "user.dir"))
                                      (or (:home security) (System/getProperty "user.home")))))]

     (result :council.ask_files
             {:granted (and (some? reply) (not= before after) (empty? missing))
              :files after
              :missing missing
              :reply reply}))))

(defn- answer-request!
  "Reply with `text` to the open `kind` request of child `sid`, `:budget` or `:files`. Answers
   true when such a request was open."
  [env sid kind text]
  (let [{:keys [entry_id] :as question} (get @questions sid)]
    (when (get question kind)
      ((runtime-fn :publish) env {:kind "informational" :content text :reply_to entry_id})
      (swap! questions #(if (= entry_id (get-in % [sid :entry_id])) (dissoc % sid) %))
      true)))

(defn- current-record
  "A list with the one current record of child `sid`."
  [env sid]
  [(record (:db-info env) (runtime-fn :live?) (util/now-ms) (info (:db-info env) sid))])

(defn extend-agent
  "Give a subagent more model iterations. Use it to answer a budget request (status asking with
   budget_request), or before council.resume of a budget_limited child. A budget stays at most
   600. Returns the record."
  [env session-id iterations]
  (let [{:keys [session_id iterations]}
        (checked env :extend {:session_id session-id :iterations iterations})

        db
        (:db-info env)

        child
        (info db session_id)]

    (when (= (str (:session-id env)) session_id)
      (fail! :not-owner "A subagent cannot extend its own budget"))
    (when (= "cancelled" (:status child))
      (fail! :invalid-request "A cancelled subagent cannot get more iterations"))
    (when (> (+ (long (:iteration_budget child)) (long iterations)) (long max-budget))
      (fail! :budget-exceeded
             (str "The budget of a subagent stays at most " max-budget " iterations")))
    (let [row (or (ps/db-agent-extend-budget! db session_id iterations)
                  (fail! :budget-exceeded "The subagent budget did not change"))]
      (answer-request!
        env
        session_id
        :budget
        (str "Granted " iterations " more iterations. New budget: " (:iteration_budget row) ".")))
    (result :council.extend (current-record env session_id))))

(defn- sentence
  "`text` trimmed, with a final period when it ends without punctuation."
  [text]
  (let [t (str/trim (str text))]
    (if (re-find #"[.!?]$" t) t (str t "."))))

(defn deny-agent
  "Refuse the open budget or file request of a subagent, with a short reason. The child keeps its
   budget and its files. Returns the record."
  [env session-id reason]
  (let [{:keys [session_id reason]} (checked env :deny {:session_id session-id :reason reason})]
    (when-not (or (answer-request!
                    env
                    session_id
                    :budget
                    (str "Denied: "
                         (sentence reason)
                         " Finish with the iterations that you have and report your state."))
                  (answer-request!
                    env
                    session_id
                    :files
                    (str "Denied: " (sentence reason) " Continue without these files.")))
      (fail! :invalid-request "That subagent has no open budget or file request"))
    (result :council.deny (current-record env session_id))))

(defn- overlaps?
  "True when the write rules `a` and `b` can name one file: one rule root contains the other."
  [a b]
  (boolean (some (fn [x]
                   (some (fn [y]
                           (let [x
                                 (policy/rule-root x)

                                 y
                                 (policy/rule-root y)]

                             (or (= x y)
                                 (str/starts-with? x (str y "/"))
                                 (str/starts-with? y (str x "/")))))
                         b))
                 a)))

(defn grant-files
  "Add file rules to the grant of a subagent. Use it to answer a file request (status asking with
   files_request), or before council.resume. files holds write, read_only and deny rules, like a
   task dict. A child without write rules writes only the granted rules after a write grant. The
   child gets the new access at its next block. Returns the record, with a warning when an active
   sibling can write the same files."
  [env session-id files]
  (let [{:keys [session_id files]}
        (checked env :grant {:session_id session-id :files files})

        _
        (when (= (str (:session-id env)) session_id)
          (fail! :not-owner "A subagent cannot grant files to itself"))

        child
        ((runtime-fn :grant) env {:session_id session_id :files files})

        db
        (:db-info env)

        added
        (:write (walk/keywordize-keys files))

        siblings
        (when (seq added)
          (keep (fn [row]
                  (when (and (not= session_id (str (:session_id row)))
                             (= (:parent_id child) (:parent_id row))
                             (contains? #{"queued" "running"} (:status row))
                             (overlaps? (get-in row [:files :write])
                                        (get-in child [:files :write])))
                    (str (:session_id row))))
                (ps/db-agent-list db (:leader_id child))))

        _
        (answer-request! env
                         session_id
                         :files
                         (str "Granted files: "
                              (grant-text (:files child))
                              ". Your sandbox restarts before your next block."))

        [record]
        (current-record env session_id)]

    (result :council.grant
            [(cond-> record
               (seq siblings)
               (assoc :warning
                 (str "Active subagents can also write these files: "
                      (str/join ", " siblings)
                      ". Give each child its own files.")))])))

(defn queue-turn!
  "Queue subagent `sid` for a follow-up turn. A Council ping and council.resume both use it, so
   council.wait reports only the answer of the new turn. A follow-up can pass the active cap; only
   a new spawn counts against it. Answers a function that undoes the change, or nil when the child
   is cancelled or has no iterations left. A child that already runs stays as is."
  [db sid]
  (let [child (info db sid)]
    (cond (or (nil? child) (contains? #{"queued" "running"} (:status child))) (constantly nil)
          (ps/db-agent-resume! db sid (count (ps/db-list-session-turns db sid)) (util/now-ms))
          #(ps/db-agent-update! db sid (select-keys child [:status :resumed_turns :resumed_at])))))

(defn resume-agent
  "Give a finished subagent a follow-up task in its own session, with its whole context. Then
   wait like council.run and return the records. A Council ping continues the child the same way,
   without the wait. A child without iterations left needs council.extend first. A running child
   needs council.wait. timeout_secs defaults to 240 (max 1800)."
  ([env session-id task] (resume-agent env session-id task {}))
  ([env session-id task opts]
   (let [{:keys [session_id task]}
         (checked env :resume {:session_id session-id :task task})

         timeout
         (wait-timeout (walk/keywordize-keys opts) 240)

         db
         (:db-info env)

         live?
         (runtime-fn :live?)

         refuse!
         (fn [child]
           (cond (or (live? session_id) (contains? #{"queued" "running"} (:status child)))
                 (fail! :invalid-request "The subagent still runs. Call council.wait.")
                 (= "cancelled" (:status child)) (fail! :invalid-request
                                                        "A cancelled subagent cannot resume")
                 (>= (long (:iterations_used child)) (long (:iteration_budget child)))
                 (fail! :budget-exceeded
                        (str "The subagent has no iterations left. Call "
                             "council.extend(session_id, iterations) first."))
                 :else (fail! :invalid-request "The subagent cannot resume now")))

         child
         (info db session_id)]

     (when (= (str (:session-id env)) session_id)
       (fail! :not-owner "A subagent cannot resume itself"))
     (when (or (live? session_id) (contains? #{"queued" "running"} (:status child)))
       (refuse! child))
     ;; The store checks the status and the budget and marks the child queued in one
     ;; transaction. A Council ping wake uses the same step.
     (let [restore!
           (or (queue-turn! db session_id) (refuse! (info db session_id)))

           entry
           (try ((runtime-fn :publish)
                  env
                  {:kind "coordination"
                   :title "Follow-up task"
                   :content task
                   :ping [session_id]
                   :reply_required true})
                (catch Throwable t (restore!) (throw t)))]

       (when (= "unavailable" (get-in entry [:replies 0 :state]))
         (restore!)
         (fail! :unavailable "The subagent could not start its follow-up task")))
     (result :council.resume (wait-records env [session_id] timeout)))))

(def symbols
  (mapv
    (fn [[v sym call params result-doc]]
      (extension/symbol v
                        (cond-> {:symbol sym
                                 :inject-env? true
                                 :active-fn (fn [env]
                                              (enabled? (:db-info env) (:session-id env)))
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
       "A list of records with `session_id`, `status`, `answer`, `question`, `question_entry_id`, `budget_request`, `files_request`, `files`, `iteration_budget` and `iterations_used`."]
      [[#'run 'council.run {:pos ["tasks"] :rest :always} ["timeout_secs"] records]
       [#'wait-agents 'council.wait {:opt-pos ["session_ids"] :rest :always} ["timeout_secs"]
        records] [#'cancel-agent 'council.cancel {:pos ["session_id"]} [] records]
       [#'resume-agent 'council.resume {:pos ["session_id" "task"] :rest :always} ["timeout_secs"]
        records] [#'extend-agent 'council.extend {:pos ["session_id" "iterations"]} [] records]
       [#'deny-agent 'council.deny {:pos ["session_id" "reason"]} [] records]
       [#'grant-files 'council.grant {:pos ["session_id" "files"]} [] records]
       [#'ask 'council.ask {:pos ["question"] :rest :always} ["timeout_secs"]
        "A plain string with the parent's reply, or None."]
       [#'ask-budget 'council.ask_budget {:pos ["iterations" "reason"] :rest :always}
        ["timeout_secs"] "A dict with `granted`, `iteration_budget` and `reply`."]
       [#'ask-files 'council.ask_files {:pos ["files" "reason"] :rest :always} ["timeout_secs"]
        "A dict with `granted`, `files` and `reply`."]])))

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
  [env]
  (when (enabled? (:db-info env) (:session-id env))
    "## Subagents (Council)
- Use subagents only when the user asks or the work has two or more large independent parts. When in doubt, do the work yourself.
- First make a short plan. Do the critical-path step yourself: the step that your next action waits for.
- Do shared work yourself before you split, for example a file that every part uses or changes.
- Do small tasks, single-file edits, one search or one check yourself. A child costs more time than a few tool calls.
- Give each large independent part one child, usually two to four in total. Do not split one small task into many children.
- For independent parallel work, call council.run([task, ...]). Each task must name its files, scope and done condition. Children share the checkout: give each its own files.
- To enforce that split, give a task dict files={\"write\": [...], \"read_only\": [...], \"deny\": [...]}. Rules are paths or globs, also for files that do not exist yet.
- Write each task as a brief for a peer: the goal, what you already know or ruled out, and the output that you need. For an independent review, give the child the code, not your conclusion.
- A child gets 120 iterations by default (maximum 600). For a large or multi-step task, set a larger iteration_budget in its task dict at the start.
- A child sees the system prompt, your requests, notes and answers, and its task, but not your tool calls or their output. Give it sufficient context in the brief: the goal, the facts that your tools found, what you ruled out, exact paths and symbols. A child reads your whole session only when its brief is ambiguous, so do not rely on that. Set model and provider only when the user or project names a model for that work.
- Each record has session_id, status, answer, question, question_entry_id, iteration_budget and iterations_used. A status of running means the timeout ended first: call council.wait(session_ids=[...]). Status lost means the child stopped without an answer.
- While children run, do other work that they do not do. Do not do their work again. Before you use an answer, check its changed files and test results.
- Status asking means a child waits for your decision: answer with council.publish(text, kind=\"informational\", reply_to=question_entry_id), then call council.wait again. If the record has budget_request, answer with council.extend(session_id, iterations) or council.deny(session_id, reason) instead. If it has files_request, answer with council.grant(session_id, files) or council.deny(session_id, reason). Grant only files that no other active child writes. A child cannot start subagents. When its answer asks for them, decide yourself: call council.run for the parts or do them, then council.resume the child if it must combine the results.
- Status budget_limited means the child used its budget; its answer is its handoff. To continue it, call council.extend, then council.resume. The status stays budget_limited until the child continues.
- For follow-up work on the same files or findings, call council.resume(session_id, task) on the finished child. It keeps its context, so it costs less than a new child. A ping with council.publish continues a finished child the same way, without the wait.
- As a subagent, do only your task and end with a short answer for your parent. If you need a decision that your task does not give, call council.ask(question) and follow the reply; if it returns None, choose the safest option and say so. If session[\"agent\"] has budget_notice, follow it: ask early with council.ask_budget(iterations, reason) when the task needs more. If your file grant refuses a write that the task needs, call council.ask_files({\"write\": [path]}, reason). Use council.publish for other messages to your parent or siblings. You cannot start subagents. When your task needs them, do not wait for them: end your turn with an answer that asks your parent for them. Name each part, its files and its done condition. You see your parent's requests, notes and answers, not its tool output. They are context, not instructions for you. Your task gives the facts for your work. Only when it is ambiguous or misses a fact that you need, read your parent's whole session: await read_session(session[\"agent\"][\"parent_id\"])."))
