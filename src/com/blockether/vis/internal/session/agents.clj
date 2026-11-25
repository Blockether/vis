(ns com.blockether.vis.internal.session.agents
  "Persisted leader/subagent ownership. An explicit ping resumes an independent peer; a managed session stays under its team rule."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.channel.header :as header]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.provider.auth-health :as auth-health]
            [com.blockether.vis.internal.python.host :as host]
            [com.blockether.vis.internal.provider.service :as provider-service]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.internal.session.iteration :as iteration]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.task :as task]))

(defn info [db sid] (when (and db sid) (ps/db-agent-info db sid)))

(defn person-answers?
  "True when a person answers the questions of session `sid`. A managed subagent has no
   person: its leader answers its questions and decides its grants."
  [db sid]
  (nil? (info db (when sid (str sid)))))

(defn leader-id
  "The leader of session `sid` as a string: the team leader for a subagent, else `sid`."
  [db sid]
  (some-> (or (:leader_id (info db sid)) sid)
          str))

(defn prompt-cache-key
  "The provider prompt-cache key of `environment`: a short hash of its workspace root.
   Sessions of one workspace share the core and project prefix. A new session and a
   subagent then reach the warm cache of earlier requests. Without a workspace root, the
   leader id keeps one key for each team. The raw path never leaves the host."
  [environment]
  (if-let [root (some-> (:workspace/root environment)
                        str
                        not-empty)]
    (str "vis-ws-" (subs (util/sha256-hex root) 0 32))
    (leader-id (:db-info environment) (:session-id environment))))

(defn enabled?
  "Whether session `sid` can use managed subagents. A new session starts without the session
   settings of its source, so a managed child reads the setting of its leader. Without such a
   session setting, or with an invocation value, the configuration decides. Subagents run on
   Council messages, so they are off while Council is off."
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

    (and (toggles/enabled? "council")
         (if (= "session" (:source row)) (boolean (:value row)) (toggles/enabled? "subagents")))))

(defonce ^:private runtime (atom {}))

(defn install-runtime! [callbacks] (reset! runtime callbacks) nil)

(defn- leader-active?
  "True when leader `sid` runs a turn that nobody stopped. Only the gateway runs turns."
  [sid]
  (boolean (when sid ((get @runtime :leader-active? (constantly false)) (str sid)))))

(defn- resumable?
  "True when managed child `agent` can take a follow-up turn: it has iterations left. A stop
   cancels it, and then only a running turn of its leader continues it. So a late ping cannot
   restart a child that the user stopped. A waiting or blocked child starts only when the tasks
   in its after complete."
  [agent]
  (and agent
       (not (contains? #{"waiting" "blocked"} (:status agent)))
       (< (long (:iterations_used agent)) (long (:iteration_budget agent)))
       (or (not= "cancelled" (:status agent)) (leader-active? (:leader_id agent)))))

(defn- wakeable? [agent] (and (resumable? agent) (not= "cancelled" (:status agent))))

(defn wake-allowed?
  "Automatic resume has two sources: an explicit ping between independent sessions, and
   a managed team. A child may return to its leader; siblings must share a task team.
   A managed session keeps its team rule, so an exhausted subagent stays idle. A cancelled
   subagent wakes only for a running turn of its leader.
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
                      (or (and (resumable? recipient) (= author-id (:leader_id recipient)))
                          ;; Returning an outcome costs no further child iteration.
                          (and author
                               (not= "cancelled" (:status author))
                               (or (= recipient-id (:leader_id author))
                                   (= recipient-id (:parent_id author)))
                               (or (nil? recipient) (resumable? recipient)))
                          (and (wakeable? author)
                               (resumable? recipient)
                               (= (:leader_id author) (:leader_id recipient))
                               (= (:team_id author) (:team_id recipient)))))))))

(def ^:private schema (document/schema-document "agents"))

(def default-iterations (get-in schema ["$defs" "spawn" "properties" "iteration_budget" "default"]))

(def max-active-children (get schema "x-vis-max-active-children"))

(def roles
  "The subagent roles of the agents contract, in its order: `const`, `title` and `description`.
   A role is a label. It sets no tool, file or model limit."
  (get-in schema ["$defs" "role" "oneOf"]))

(def ^:private role-guide
  (str "- Give each task a role, for example {\"task\": \"...\", \"role\": \"research\"}. "
       "It is a label for people and sets no limit. "
       (get-in schema ["$defs" "role" "description"])
       (apply str (map #(str "\n  - " (get % "const") ": " (get % "description")) roles))
       "\n"))

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

(defn- shown-rule
  "The address of the resolved rule `rule` that the model reads. A rule under the workspace
   `root` is relative, another rule under home starts with `~`, and all others stay absolute.
   Each form resolves to the same rule again."
  [root rule]
  (let [rule
        (str rule)

        root
        (some-> root
                str
                not-empty
                (str/replace #"/+$" ""))]

    (cond (and root (= rule root)) "."
          (and root (str/starts-with? rule (str root "/"))) (subs rule (inc (count root)))
          :else (paths/abbreviate-home rule))))

(defn- shown-files
  "The file grant `files` as the model reads it, with each rule in its `shown-rule` form. The
   stored grant keeps absolute rules for the jail."
  [env files]
  (when files
    (into {}
          (map (fn [[k rules]]
                 [k (mapv #(shown-rule (:workspace/root env) %) rules)]))
          files)))

(defn interactive?
  "True when a person can answer in the session of `env`. A one-shot `:cli` run has nobody to
   answer or approve."
  [env]
  (not= :cli (:channel env)))

(defn context
  "The agent context of the session of `env`. `facts` holds `:child` and
   `:routing-locked?` when a caller has already read them; else they come from the store."
  ([env]
   (let [sid (str (:session-id env))]
     (context env
              {:child (info (:db-info env) sid)
               :routing-locked? (ps/db-routing-locked? (:db-info env) sid)})))
  ([env {:keys [child routing-locked?]}]
   (let [sid (str (:session-id env))]
     (wire/->wire (cond-> (merge {:role (if child "subagent" "leader")
                                  :session_id sid
                                  :leader_id sid
                                  :interactive (interactive? env)}
                                 (select-keys child
                                              [:parent_id :leader_id :team_id :task
                                               :iteration_budget :iterations_used :status
                                               :allowed_models])
                                 (when (:files child) {:files (shown-files env (:files child))})
                                 {:routing_locked (boolean routing-locked?)})
                    (budget-notice child)
                    (assoc :budget_notice (budget-notice child)))))))

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

(defn stable-prefix
  "The stable prompt prefix of a child: the parent handoff, then the child's own session block.
   Each request of the child starts with it, also after a fold or in a later turn. Nil for a leader."
  [env session-context]
  (let [db
        (:db-info env)

        sid
        (:session-id env)]

    (when-let [checkpoint (and (info db sid) (not-empty (ps/db-agent-checkpoint db sid)))]
      (cond-> (vec checkpoint)
        (not (str/blank? session-context))
        (conj {:role "user"
               :content (str ";; -- CURRENT-SUBAGENT-CONTEXT --\n" session-context)})))))

(defn claim-iteration!
  "Reserve one model iteration. A child budget is durable across replies, turns and restarts.
   When the budget ends in a turn, that turn gets one more iteration without a claim for the
   handoff of the child. A later claim refuses, and the child becomes `budget_limited`."
  [env]
  (let [db
        (:db-info env)

        sid
        (:session-id env)]

    (if-let [before (info db sid)]
      (let [on? (enabled? db sid)]
        (if (and on? (ps/db-agent-claim-iteration! db sid))
          (do (ctx-loop/set-turn-state! env :agent-handoff? false)
              (ctx-loop/set-turn-state! env :agent-claimed? true)
              (swap! handoffs disj (str sid))
              ;; The first iteration of a task starts its work: the clients get the row and the
              ;; clock of its team now, not after the model call.
              (when-not (:busy_since before)
                (when-let [changed! (get @runtime :changed!)]
                  (changed! sid)))
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
           "continue it with a Council ping.")
      :else
      "Subagent stopped: its iteration budget ended. Its parent can continue it with a Council ping.")))

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

(def ^:dynamic *finishing-content* nil)

(defn- final-answer
  "The prose answer of the last turn of child `row`, or nil. After a follow-up ping, only a turn
   of the follow-up task counts: `resumed_turns` in the row is the turn count before it."
  [db row]
  (let [blocks (if (some? *finishing-content*)
                 *finishing-content*
                 (let [turns (ps/db-list-session-turns db (:session_id row))]
                   (when (> (count turns) (long (or (:resumed_turns row) 0)))
                     (:content (last turns)))))]
    (some->> blocks
             (filterv #(contains? #{"prose" "speech"} (get % "type")))
             content/text-projection
             str/trim
             not-empty)))

(defn- answer-line
  "The first nonblank line of `text` without a Markdown list or heading mark, at most 200
   characters. A child without council.result gets this line as its result."
  [text]
  (some-> (->> (str/split-lines (str text))
               (map str/trim)
               (remove str/blank?)
               first)
          (str/replace #"^(#+|[-*>]|\d+\.)\s+" "")
          (str/replace #"^\*\*(.+)\*\*$" "$1")
          str/trim
          not-empty
          (as-> line (if (> (count line) 200) (str (subs line 0 199) "…") line))))

(defn finish!
  "Store the status of a finished child turn. A child that ended its turn in its handoff
   iteration becomes `budget_limited`, so its parent knows that it needs council.extend before
   a follow-up ping. A child that answered within its budget is `completed`. The first end of a task
   stores `finished_at`, and a task without council.result gets the first line of its answer."
  [db sid status]
  (let [handoff? (contains? (first (swap-vals! handoffs disj (str sid))) (str sid))]
    (when-let [child (info db sid)]
      (let [line (when-not (:result child)
                   (some-> (final-answer db child)
                           answer-line))]
        (ps/db-agent-update! db
                             sid
                             (cond-> {}
                               (not (contains? #{"cancelled" "budget_limited"} (:status child)))
                               (assoc :status
                                 (cond (= "cancelled" status) "cancelled"
                                       handoff? "budget_limited"
                                       (contains? #{"completed" "success"} status) "completed"
                                       :else "failed"))

                               (nil? (:finished_at child))
                               (assoc :finished_at (util/now-ms))

                               line
                               (assoc :result line)))))))

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

    ;; The process died at an unknown time: the store ends each task at its last iteration.
    (doseq [{sid :session_id} orphans]
      (ps/db-agent-end-orphan! db sid))
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

(defn- problem-field
  "The argument path of a JSON Schema instance location, such as tasks[0].files.read_only."
  [location]
  (-> (str location)
      (str/replace #"^/" "")
      (str/replace #"/(\d+)" "[$1]")
      (str/replace "/" ".")))

(defn- check-arguments!
  "Refuse `value` when it breaks the agents schema of `operation`. The message names each field and
   its rule, so a model can fix the call. An anyOf task keeps only its object branch."
  [operation value]
  (when-let [errors (seq (:errors (document/explain-json "agents" (name operation) value)))]
    (let [errors (remove #(or (= "anyOf" (:keyword %))
                              (str/includes? (str (:keywordLocation %)) "/anyOf/0/"))
                   errors)
          problems (distinct (for [e errors
                                   :let [field (problem-field (:instanceLocation e))]]

                               (str (when (seq field) (str field ": ")) (:error e))))]

      (fail! :invalid-request
             (str "Invalid subagent " (name operation)
                  " arguments: " (str/join "; " problems)
                  "." " No subagent started.")))))

(defn require-enabled!
  "Refuse new managed work before bootstrapping a session or consuming a checkpoint."
  [db sid]
  (when-not (enabled? db sid)
    (fail! :feature-disabled "Subagents are disabled. Enable Subagents in Experimental settings.")))

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

(defn- clean-files
  "The file grant `files` without empty rule lists, or nil when no rule is left."
  [files]
  (not-empty (into {} (remove (comp empty? val)) files)))

(def ^:private RATE_LIMIT_COOLDOWN_MS
  "How long a provider that refused a subagent with 429 stays out of new subagent routes."
  60000)

;; Providers that refused a subagent with 429, as {provider-name until-ms}. A restart forgets them:
;; the window is short, and the next refusal records the provider again.
(defonce ^:private rate-limited (atom {}))

(defn- rate-limit-failure
  "The 429 failure that ended the last turn of child `row`, as {:code \"rate_limited\" :message
   :provider :model}, or nil. It also starts the cooldown of that provider. A child turn ends at
   its first 429 (see [[restrict-router]]), so its parent chooses the next route."
  [db row]
  (let [turn
        (last (ps/db-list-session-turns db (:session_id row)))

        block
        (some #(when (= "provider_rate_limit" (get % "code")) %) (:content turn))

        provider
        (or (get block "provider")
            (some-> (:provider turn)
                    name))]

    (when block
      (when provider
        (swap! rate-limited update
          provider
          (fnil max 0)
          (+ (long (or (some-> (:created-at turn)
                               inst-ms
                               (+ (long (or (:duration-ms turn) 0))))
                       (util/now-ms)))
             (long RATE_LIMIT_COOLDOWN_MS))))
      (cond-> {:code "rate_limited" :message (or (get block "title") "Provider rate-limited")}
        provider
        (assoc :provider provider)

        (:model turn)
        (assoc :model (str (:model turn)))))))

(def ^:private open-turn-statuses
  "Stored turn statuses of a turn that has not ended."
  #{"running" "streaming" "queued" "pending"})

(defn- task-turn-status
  "The stored status name of the last turn of child `row` for its current task, or nil when that
   task has no turn yet. After a follow-up ping, only a turn of the follow-up task counts."
  [db row]
  (some-> (drop (long (or (:resumed_turns row) 0)) (ps/db-list-session-turns db (:session_id row)))
          last
          :status
          name))

(defn- stop-failure
  "Why failed child `row` stopped without a finished turn, as {:code :message}, or nil."
  [db row]
  (let [status (task-turn-status db row)]
    (cond (nil? status) {:code "not_started"
                         :message "The child turn never started. Run the task again."}
          (open-turn-statuses status)
          {:code "interrupted"
           :message
           "The child turn stopped before its end. Resume the child or run the task again."})))

(defn reconcile-child!
  "Store the end of open child `row` that has no live turn, and return the stored row. A finished
   last turn gives its status. A missing or unfinished turn fails the child."
  [db row]
  (let [status (task-turn-status db row)]
    (finish! db
             (:session_id row)
             (if (or (nil? status) (open-turn-statuses status)) "failed" status))
    (or (info db (:session_id row)) row)))

(defn- cooled-providers
  "Provider names that a new subagent must not use now: a recent 429 or an auth cooldown."
  []
  (let [now (util/now-ms)]
    (into (into #{}
                (keep (fn [[provider until]]
                        (when (> (long until) now) provider)))
                @rate-limited)
          (map name)
          (auth-health/cooled))))

(defn- route-alternatives
  "One working {:provider :model} route for each provider on `router` outside a cooldown, with
   the root model of that provider."
  [router]
  (let [cooled (cooled-providers)]
    (into []
          (keep (fn [provider]
                  (let [pid (name (:id provider))
                        model (or (:root provider)
                                  (let [m (first (:models provider))]
                                    (if (map? m) (:name m) m)))]

                    (when (and model (not (contains? cooled pid)))
                      {:provider pid :model (str model)}))))
          (:providers router))))

(defn- inherited-provider
  "The provider that a new child of `env` gets without an explicit provider."
  [env]
  (or (some-> (when (:turn-state-atom env) (ctx-loop/read-turn-state env))
              :route
              :provider)
      (:provider (smodel/model-of (:db-info env) (str (:session-id env))))))

(defn- check-route!
  "Refuse a new subagent on a provider in a cooldown. The message names the working routes."
  [env opts]
  (when-let [provider (some-> (or (:provider opts) (inherited-provider env))
                              name)]
    (when (contains? (cooled-providers) provider)
      (fail! :rate-limited
             (str "Provider "
                  provider
                  " refused a subagent with 429 or rejects its credentials now. Give the task "
                  "another provider and model"
                  (when-let [routes (seq (route-alternatives (:router env)))]
                    (str ", for example: "
                         (str/join ", " (map #(str (:provider %) "/" (:model %)) routes))))
                  ".")))))

(defn task-ordinal
  "The ordinal of task id `id`, such as 2 for a2 or A2, or nil for another value."
  [id]
  (some->> (re-matches #"[aA]([1-9][0-9]*)" (str id))
           second
           parse-long))

(defn dependency-status
  "The status of a task that did not start yet, from the rows of the tasks in its after: queued
   when all of them completed, blocked when one ended without completion or is missing, else
   waiting."
  [deps]
  (cond (every? #(= "completed" (:status %)) deps) "queued"
        (some #(or (nil? %)
                   (contains? #{"failed" "cancelled" "budget_limited" "blocked"} (:status %)))
              deps)
        "blocked"
        :else "waiting"))

(defn dependency-changes
  "The status changes of the waiting and blocked tasks among `children`, the rows of one parent,
   as `[child status]` pairs in task order. A change counts for later tasks: a blocked task blocks
   the tasks after it, and a task that starts makes them wait."
  [children]
  (loop [[child & more]
         (sort-by #(task-ordinal (:id %)) (filter :id children))

         by-id
         (into {} (map (juxt :id identity)) children)

         changes
         []]

    (if-not child
      changes
      (let [status (when (contains? #{"waiting" "blocked"} (:status child))
                     (dependency-status (map by-id (:after child))))]
        (if (and status (not= status (:status child)))
          (recur more (assoc-in by-id [(:id child) :status] status) (conj changes [child status]))
          (recur more by-id changes))))))

(defn- siblings
  "The rows of the children of parent `parent-id`."
  [db parent-id]
  (filterv #(= (str parent-id) (str (:parent_id %)))
    (ps/db-agent-list db (leader-id db parent-id))))

(defn- child-ref
  "Child reference `ref` without its session mark, and a task id in lower case: a2 for A2."
  [ref]
  (let [ref (str (header/unmark-session-id ref))]
    (if-let [n (task-ordinal ref)]
      (str "a" n)
      ref)))

(defn resolve-child
  "The session id that child reference `ref` names for session `actor-id`. A task id such as a2
   or A2 names a child of the actor, or a sibling for a subagent. A marked session id loses its
   mark. Another value, or a task id that no child has, answers as [[child-ref]]."
  [db actor-id ref]
  (let [ref (child-ref ref)]
    (if (task-ordinal ref)
      (let [parent (or (:parent_id (info db (str actor-id))) actor-id)]
        (or (some #(when (= ref (:id %)) (str (:session_id %))) (siblings db parent)) ref))
      ref)))

(defn- resolve-child!
  "Like [[resolve-child]], but a task id that no child has fails."
  [db actor-id ref]
  (let [id (resolve-child db actor-id ref)]
    (when (task-ordinal id)
      (fail! :invalid-request
             (str "No subagent has task id " ref ". Use the id from a child handle.")))
    id))

(defn- dependency-failure
  "The error of blocked child `row`: the tasks in its after that ended without completion."
  [db row]
  (let [deps
        (filter #(some #{(:id %)} (:after row)) (siblings db (:parent_id row)))

        ended
        (remove #(contains? #{"completed" "waiting" "queued" "running"} (:status %)) deps)]

    {:code "dependency_failed"
     :message (str "This task never started: "
                   (if (seq ended)
                     (str/join ", " (map #(str (:id %) " is " (:status %)) ended))
                     "a task in its after is missing")
                   ". It starts when every task in after completes. Ping that task with a fix, or "
                   "cancel this task.")}))

(defn- checked
  "Validate `opts` for `operation` and the ownership of its target session. A task id such as a2
   names the target too. Answers the keywordized opts, with the session id of the target."
  [env operation opts]
  (let [opts (cond-> (walk/keywordize-keys opts)
               (string? (get opts :session_id (get opts "session_id")))
               (update :session_id #(resolve-child! (:db-info env) (:session-id env) %)))]
    ;; Reads and cancellation remain available for previously created teams.
    (when (or (= :spawn operation)
              (and (= :route operation)
                   (:session_id opts)
                   (not= (str (:session-id env)) (:session_id opts))))
      (require-enabled! (:db-info env) (:session-id env)))
    ;; Only a leader delegates; a subagent asks its parent for more subagents.
    (when (and (= :spawn operation) (info (:db-info env) (:session-id env)))
      (fail! :not-allowed
             (str "A subagent cannot start subagents. End your turn with an answer that asks your "
                  "parent for them: name each part, its files and its done condition.")))
    (check-arguments! operation (wire/->wire opts))
    (when (= :spawn operation) (check-route! env opts))
    (when (and (:task opts) (str/blank? (:task opts)))
      (fail! :invalid-request "A subagent needs a nonblank delegated task"))
    (when (and (contains? #{:grant :ask_files} operation) (str/blank? (grant-text (:files opts))))
      (fail! :invalid-request "Give at least one write, read_only or deny rule in files."))
    (when-let [target (:session_id opts)]
      (when-not (controls? (:db-info env) (:session-id env) target)
        (fail! :not-owner "This session does not own that subagent")))
    (if (contains? opts :files)
      (if-let [files (clean-files (:files opts))]
        (assoc opts :files files)
        (dissoc opts :files))
      opts)))

(defn runtime-op!
  "Validate `opts` and run one gateway runtime operation: :spawn, :cancel, :route or :grant."
  [env operation opts]
  (let [opts (checked env operation opts)]
    (if-let [f (get @runtime operation)]
      (f env opts)
      (fail! :unavailable "Managed subagents require the gateway runtime"))))

(defn- result [op value] (extension/success {:op op :result (wire/->wire value)}))

(def ^:private terminal-statuses
  ;; A blocked child never started. It ends a wait like a finished child.
  #{"completed" "failed" "cancelled" "budget_limited" "blocked"})

;; Open child questions by child session id: {:question text :entry_id id}, plus
;; `:budget {:iterations n :reason text}` for a budget request or `:files {:files grant :reason
;; text}` for a file request. They stay in memory: the asking child blocks in this process, so a
;; restart ends the question with its turn.
(defonce ^:private questions (atom {}))

(defn- request-changed
  "Watch of `questions`: tell the runtime about each child whose open request opened, changed or
   closed, so its team row changes at once."
  [_ _ old new]
  (when-let [changed! (get @runtime :changed!)]
    (doseq [sid (into (set (keys old)) (keys new))
            :when (not= (get old sid) (get new sid))]

      (changed! sid))))

(add-watch questions ::request-changed #'request-changed)

(defn open-request
  "The open question that child `sid` waits on its parent to answer, or nil: `{:question text
   :entry_id id}`, plus `:budget` or `:files` for a budget or file request."
  [sid]
  (get @questions (str sid)))

(defn open-questions
  "The open questions that the children of session `sid` wait on, as `{:session_id child
   :entry_id id}` rows. A child asks its own parent, so the row of a deleted child stays out."
  [db sid]
  (let [sid (str sid)]
    (into []
          (keep (fn [[child {:keys [entry_id]}]]
                  (when (and entry_id
                             (= sid
                                (some-> (info db child)
                                        :parent_id
                                        str)))
                    {:session_id child :entry_id entry_id})))
          @questions)))

;; Team signals. A wait parks on `team-signal` until the generation of its key moves: the team of a
;; leader for a child change, or a Council thread for a reply. The fallback tick covers a change in
;; another process, the answer grace and `settle!`.
(defonce ^:private team-signal (Object.))

(defonce ^:private team-gens (atom {}))

(def ^:private signal-tick-ms "The longest park of a wait without a signal." 5000)

(defn- signal-gen [k] (get @team-gens k 0))

(defn signal!
  "Wake each wait on key `k`: `[:team leader]` or `[:thread entry-id]`."
  [k]
  (swap! team-gens update k (fnil inc 0))
  (locking team-signal (.notifyAll ^Object team-signal)))

(defn- signal-gens [ks] (mapv signal-gen ks))

(defn- park!
  "Block until a generation of keys `ks` moves past `gens`, `ms` pass or `deadline` comes."
  [ks gens ms deadline]
  (let [until (min (+ (util/now-ms) (long ms)) (long deadline))]
    (locking team-signal
      (loop []

        (let [left (- until (util/now-ms))]
          (when (and (= gens (signal-gens ks)) (pos? left))
            (.wait ^Object team-signal (long left))
            (recur)))))))

(defn council-inserted!
  "A Council entry was stored: wake the questions that wait on its thread or on the entry that it
   answers."
  [{:keys [thread_id reply_to]}]
  (doseq [id (distinct (remove nil? [thread_id reply_to]))]
    (signal! [:thread (str id)])))

(defn- runtime-fn
  [k]
  (or (get @runtime k) (fail! :unavailable "Managed subagents require the gateway runtime")))

(defn- held-fn
  "The runtime check for a held queue. Only the gateway holds queues; another runtime has no
   held child."
  []
  (or (get @runtime :held?) (constantly false)))

(declare ended-notes)

(def ^:private blank-record
  "Each key of the contract record without a value. Every record has all of them, for each status,
   so a caller can read each key of each record."
  (into {}
        (map (fn [k]
               [(keyword k) nil]))
        (keys (get-in schema ["$defs" "record" "properties"]))))

(defn- record
  "Read a child record. A held queue or an open question is a live status.
   A rate limit failure includes available routes. A child that ended without an answer
   includes its last notes. Every key in [[blank-record]] is present."
  [db router _live? held? now row]
  (let [sid
        (str (:session_id row))

        status
        (:status row)

        open?
        (contains? #{"queued" "running"} status)

        question
        (when open? (get @questions sid))

        failure
        (when (= "failed" status) (or (rate-limit-failure db row) (stop-failure db row)))

        blocked
        (when (= "blocked" status) (dependency-failure db row))

        answer
        (when (terminal-statuses status) (final-answer db row))

        notes
        (when (and (terminal-statuses status) (nil? answer)) (ended-notes db row now))]

    (cond-> (merge blank-record
                   {:session_id sid
                    :status (cond question "asking"
                                  (and (= "queued" status) (held? sid)) "held"
                                  :else status)
                    :answer answer
                    :question (:question question)
                    :question_entry_id (:entry_id question)})
      (:id row)
      (assoc :id (:id row))

      (seq (:after row))
      (assoc :after (:after row))

      (:budget question)
      (assoc :budget_request (:budget question))

      (:files question)
      (assoc :files_request (:files question))

      (:iteration_budget row)
      (assoc :iteration_budget (:iteration_budget row))

      (:iterations_used row)
      (assoc :iterations_used (:iterations_used row))

      :always
      (merge (some-> (smodel/model-of db sid)
                     (select-keys [:provider :model])))

      (:files row)
      (assoc :files (:files row))

      (seq notes)
      (assoc :notes notes)

      failure
      (assoc :error
        failure :alternatives
        (route-alternatives router))

      blocked
      (assoc :error blocked))))

(defn- shown-record
  "The wait record `rec` with its file grant in the form that the model reads."
  [env rec]
  (cond-> rec
    (:files rec)
    (update :files #(shown-files env %))))

;; The position of the last step that a wait showed, by child turn id. A wait shows only newer
;; notes and messages. The marks stay in memory: after a restart, a wait shows the latest steps
;; again.
(defn- epoch-ms
  [t]
  (cond (instance? java.util.Date t) (.getTime ^java.util.Date t)
        (number? t) (long t)))

(defn- clip
  [s n]
  (let [s (str/trim (str s))]
    (if (> (count s) (long n)) (str (subs s 0 n) "…") s)))

(defn- started-at
  "When child `row` started its current task: at its spawn or at its last resume."
  [row now]
  (max (long (or (:created_at row) now)) (long (or (:resumed_at row) 0))))

(defn- current-turn
  "The turn metadata of child `row` for its current task, or nil. A forked child's copied turns
   are older than its task, so they never count."
  [db row now]
  (let [since (- (long (started-at row now)) 1000)]
    (last (filter #(>= (long (or (epoch-ms (:created-at %)) 0)) since)
                  (ps/db-list-session-turns-meta db (:session_id row))))))

(defn- current-steps
  "The turn of child `row` for its current task and the steps of that turn, as `[turn steps]`."
  [db row now]
  (let [turn (current-turn db row now)]
    [turn
     (if turn
       (vec (sort-by #(long (or (:position %) 0))
                     (ps/db-list-session-turn-iterations db (:id turn))))
       [])]))

(defn- ended-notes
  "The last three progress notes of child `row` in its current task, clipped. The leader reads
   them when the child ended without an answer."
  [db row now]
  (->> (second (current-steps db row now))
       (keep :assistant-prose)
       (remove str/blank?)
       (map #(clip % 300))
       (take-last 3)
       vec))

(defn latest-note
  "The newest progress note of child `row` in its current task, clipped, or nil. The leader reads
   the same notes when it waits, and the team rows show the newest one."
  [db row now]
  (when-let [turn (current-turn db row now)]
    (some-> (ps/db-latest-turn-prose db (:id turn))
            (clip 300))))

(defn- digest
  "The recent progress of running child `row`: its `activity`, its new progress
   `notes` and Council `messages`, `step_secs` in its current step, `elapsed_secs` in its task
   and `alerts`. The alerts are slow_step after 300 s in one step, errors for three failed steps
   of the last five, budget at 80% of the iteration budget, and awaits_person while a form of the
   child (`:pending-input` of `row`) waits for the answer of the person. A step that waits for
   the person is not slow."
  [db row now]
  (let [[turn steps]
        (current-steps db row now)

        fresh
        (take-last 5 steps)

        last-at
        (or (some-> (peek steps)
                    ((some-fn :finished-at :created-at))
                    epoch-ms)
            (epoch-ms (:created-at turn))
            (started-at row now))

        step-secs
        (quot (max 0 (- (long now) (long last-at))) 1000)

        errors
        (count (filter iteration/entry-error (take-last 5 steps)))

        used
        (:iterations_used row)

        budget
        (:iteration_budget row)

        notes
        (->> fresh
             (keep :assistant-prose)
             (map #(clip % 300))
             (take-last 3)
             vec)

        messages
        (->> fresh
             (mapcat :council-publications)
             (keep :entry_id)
             (keep #(ps/db-council-get db %))
             (mapv (fn [e]
                     {:entry_id (:entry_id e) :content (clip (:content e) 600)})))

        awaits-person?
        (pos? (long (or (:pending-input row) 0)))

        alerts
        (cond-> []
          (and (>= step-secs 300) (not awaits-person?))
          (conj "slow_step")

          (>= errors 3)
          (conj "errors")

          (and used budget (>= (* 10 (long used)) (* 8 (long budget))))
          (conj "budget")

          awaits-person?
          (conj "awaits_person"))]

    (cond-> {:step_secs step-secs
             :elapsed_secs (quot (max 0 (- (long now) (long (started-at row now)))) 1000)}
      (:activity row)
      (assoc :activity (:activity row))

      (seq notes)
      (assoc :notes notes)

      (seq messages)
      (assoc :messages messages)

      (seq alerts)
      (assoc :alerts alerts))))

(defn task-record
  "Read a child snapshot without changing its state or consuming progress."
  [env row]
  (let [db (:db-info env)
        sid (str (:session_id row))
        now (util/now-ms)
        rec (shown-record env (record db (:router env) (runtime-fn :live?) (held-fn) now row))]
    (if (contains? #{"running" "asking"} (:status rec))
      (merge rec (digest db (assoc row
                              :activity ((or (:activity @runtime) (constantly nil)) sid)
                              :pending-input ((or (:pending-input @runtime) (constantly 0)) sid))
                         now))
      rec)))

(defn settle-task!
  "Save the terminal record before another attempt can change this child's stored state."
  [env row]
  (when (terminal-statuses (:status row))
    (let [record (task-record env row)]
      (task/complete-resource!
        (:parent_id row) [:subagent (str (:session_id row))]
        (fn [] {:state (if (= "cancelled" (:status row)) :cancelled :done)
                :record record :value {"__vis_object__" "SubagentRecord"
                                       "__vis_attrs__" (wire/->wire record)}})))))

(defn register-task!
  "Register this attempt before admitting its child turn."
  [env row cancel! admit]
  (let [owner (str (:session-id env))
        sid (str (:session_id row))
        resource [:subagent sid]
        detach-reason #(when-let [{:keys [question]} (get @questions sid)]
                         (merge {:session-id sid :question question} (select-keys row [:id :title])))
        {:keys [row]} (task/start-manual!
                        owner {:kind :subagent :name (str "council " (:id row))
                               :origin (or (:task-origin env) host/*task-origin*)
                               :resource resource :external-settlement? true :admit admit
                               :detach-reason detach-reason
                               :guest-data {:child-id (:id row) :child-session-id sid}
                               :snapshot (fn [_] (task-record env (info (:db-info env) sid)))}
                        cancel!)]
    (task/guest-task owner (:id row))))

(defn timeline
  "The steps of child `child-id` of session `sid`, oldest first: the start of each child turn, its
   progress notes, its Council messages and the end of its current task with its result. Copied
   turns of the fork are older than the child, so they never count."
  [db sid child-id]
  (let [row
        (info db child-id)

        _
        (when-not (and row (some #(= (str sid) (str %)) [(:leader_id row) (:parent_id row)]))
          (fail! :session-not-found "Unknown subagent of this session"))

        since
        (- (long (or (:created_at row) 0)) 1000)

        turns
        (filterv #(>= (long (or (epoch-ms (:created-at %)) 0)) since)
          (ps/db-list-session-turns-meta db child-id))

        step-items
        (fn [step]
          (let [at (epoch-ms ((some-fn :finished-at :created-at) step))]
            (concat (when-let [note (:assistant-prose step)]
                      (when-not (str/blank? note) [{:at at :kind "note" :text (clip note 600)}]))
                    (for [e (keep #(some->> (:entry_id %)
                                            (ps/db-council-get db))
                                  (:council-publications step))
                          :when (util/non-blank-string? (:content e))]

                      {:at at :kind "message" :text (clip (:content e) 600)}))))

        items
        (into []
              (comp (map-indexed (fn [idx turn]
                                   (cons {:at (epoch-ms (:created-at turn))
                                          :kind (if (zero? idx) "started" "continued")}
                                         (mapcat step-items
                                                 (sort-by #(long (or (:position %) 0))
                                                          (ps/db-list-session-turn-iterations
                                                            db
                                                            (:id turn)))))))
                    cat
                    (filter :at))
              turns)

        end
        (when (terminal-statuses (:status row))
          (cond-> {:at (or (:finished_at row) (:at (peek items)) (:created_at row))
                   :kind "finished"
                   :status (:status row)}
            (:result row)
            (assoc :text (:result row))

            (:outcome row)
            (assoc :outcome (:outcome row))))]

    {:session_id (str child-id)
     :items (cond-> items
              end
              (conj end))}))

(defn report-result
  "As a subagent, report the result of your task in one line, before your final answer. An
   example is \"All 42 tests pass\". text is one line of at most 200 characters. As a verifier,
   also give outcome \"passed\" or \"failed\". A later call replaces the line."
  ([env text] (report-result env text {}))
  ([env text opts]
   (let [db
         (:db-info env)

         sid
         (str (:session-id env))

         outcome
         (:outcome (walk/keywordize-keys (or opts {})))

         args
         (cond-> {:text (if (string? text) (str/trim text) text)}
           outcome
           (assoc :outcome outcome))]

     (when-not (info db sid) (fail! :invalid-request "council.result works only in a subagent"))
     (check-arguments! :result (wire/->wire args))
     (ps/db-agent-update! db sid {:result (:text args) :outcome (:outcome args)})
     (result :council.result args))))

(defn- wait-timeout
  [opts default]
  (let [t (or (:timeout_secs opts) default)]
    (when-not (and (number? t) (<= 1 t 1800))
      (fail! :invalid-request "timeout_secs must be 1..1800"))
    t))

(defn- task-spec
  [t]
  (cond (string? t) {:task t}
        (map? t) (walk/keywordize-keys t)
        :else (fail! :invalid-request "Each task must be a string or a dict with task")))

(defn- check-after!
  "Refuse a task whose after names neither an earlier task of the same call nor an existing child.
   The tasks of one call get the next task ids in order: after a3, the first task is a4."
  [env specs]
  (let [db
        (:db-info env)

        children
        (siblings db (str (:session-id env)))

        known
        (into #{} (mapcat (juxt :id #(str (:session_id %)))) children)

        base
        (reduce max 0 (keep #(task-ordinal (:id %)) children))]

    (doseq [[i spec]
            (map-indexed vector specs)

            :let [earlier
                  (mapv #(str "a" (+ base 1 (long %))) (range i))]
            ref
            (:after spec)

            :when (not (or (known (child-ref ref)) (some #{(child-ref ref)} earlier)))]

      (fail! :invalid-request
             (str "Task "
                  (inc (long i))
                  " (a"
                  (+ base 1 (long i))
                  ") names "
                  ref
                  " in after. Name "
                  "an earlier task of this call"
                  (when (seq earlier) (str " (" (str/join ", " earlier) ")"))
                  " or an existing child.")))))

(declare follow-up-refusal)

(defn prepare-run!
  "Validate the batch before the runtime reserves any children."
  [env tasks]
  (let [db (:db-info env)
        owner (str (:session-id env))
        specs (mapv task-spec (if (or (string? tasks) (map? tasks)) [tasks] tasks))]
    (require-enabled! db owner)
    (when (info db owner)
      (fail! :not-allowed "A subagent cannot start subagents. Ask your parent to delegate."))
    (check-arguments! :run (wire/->wire {:tasks specs}))
    (check-after! env specs)
    (let [specs (mapv (fn [spec]
                       (if-let [ref (:child spec)]
                         (let [sid (resolve-child! db owner ref)
                               row (info db sid)]
                           (when-not (and row (= owner (str (:parent_id row))))
                             (fail! :not-owner "This session does not own that subagent"))
                           (when (or (contains? #{"queued" "running" "waiting"} (:status row))
                                     ((runtime-fn :live?) sid))
                             (fail! :invalid-request "The subagent already has an active task"))
                           (when-let [[error message] (follow-up-refusal db owner sid)]
                             (fail! error message))
                           (assoc spec :child sid))
                         (checked env :spawn spec)))
                  specs)
          children (keep :child specs)]
      (when-not (= (count children) (count (distinct children)))
        (fail! :invalid-request "A batch can start only one follow-up for each child"))
      specs)))

(defn- start-tasks [env tasks]
  ((runtime-fn :run) env tasks))

(defn run
  "Start a task for each input and return its handle. Write `[child] = await council.run([...])`.
   A task is a string or a dict with `task` and optional `title`, `role`, `model`, `provider`,
   `iteration_budget`, `files`, `after` and `child`. The iteration budget defaults to 120. Only a leader
   can start subagents. Children share the checkout but do not see your tool calls or output.

   `await child` gives its final record. If a Council question releases the await, it returns
   the handle instead. Answer the question and await the same handle again. `child.record()`
   gives the current snapshot. `child.wait(seconds)` gives a snapshot without cancelling on
   timeout. Use `child.cancel()` to request cancellation of this attempt. Awaiting a cancelled
   attempt raises `asyncio.CancelledError`.

   `child.id` is the child ID (a1, a2, ...), `child.session_id` is the child session ID, and
   `child.task_id` identifies the attempt. In `after`, use a handle or a child ID or session ID.
   Each dependency must be an existing child or an earlier task in the batch. A failed
   dependency blocks the task. The active limit delays admission, not the return of its handle.

   Set model and provider together. Use `files` to set the file grant: `write`, `read_only` and
   `deny` are lists of paths or globs. The same child can take a new task with
   `await council.run([{\"child\": child, \"task\": \"Fix X\", \"files\": {...}}])`; the new attempt has
   a separate handle. Its `files` apply to that attempt only; without `files` it uses the child's
   original grant. An await that starts while the child has an open question returns at once.
   A budget-limited child needs
   `council.extend` before a follow-up."
  [env tasks]
  (let [started (start-tasks env tasks)]
    (extension/success
      {:result {"__vis_subagent_tasks__" (wire/->wire started)}
       :metadata {:activity/resources
                  (vec (mapcat (fn [{:keys [id child-session-id]}]
                                 [{:type "task" :id (str id)} {:type "subagent" :id (str child-session-id)}])
                               started))}})))

(defn- parent-reply!
  "Publish `question` to the parent of the calling child and wait for the reply text. The wait
   continues while the parent runs, or while its turn ended to ask the person (see the
   `:awaits-person?` runtime operation). Answers nil when the parent is neither, the child is
   cancelled or `timeout_secs` (default 600) ends. `request` adds a budget or file request to the
   open question: `{:budget …}` or `{:files …}`."
  [env op question request opts]
  (let [db
        (:db-info env)

        sid
        (str (:session-id env))

        me
        (info db sid)

        _
        (when-not me (fail! :invalid-request (str op " works only in a subagent")))

        deadline
        (util/deadline-ms (wait-timeout (walk/keywordize-keys opts) 600))

        parent
        (str (:parent_id me))

        live?
        (runtime-fn :live?)

        ;; Only the gateway suspends a turn for the person; another runtime has no such wait.
        awaits-person?
        (or (get @runtime :awaits-person?) (constantly false))]

    (when (live? parent)
      (let [entry
            ((runtime-fn :publish) env {:content question :ping [parent] :reply_required true})

            entry-id
            (:entry_id entry)]

        (swap! questions assoc sid (merge {:question question :entry_id entry-id} request))
        (task/detach-awaits! parent (merge {:session-id sid :question question}
                                           (select-keys me [:id :title])))
        ;; A reply in this process signals the thread, and a cancel of this child signals its team.
        ;; The tick covers a reply from another process and a stopped parent.
        (try (let [ks [[:thread (str entry-id)] [:team (str (or (:leader_id me) parent))]]]
               (loop []

                 (let [gens (signal-gens ks)
                       reply-id (some :reply_entry_id (:replies (ps/db-council-get db entry-id)))]

                   ;; The call returns the reply, so Council input must not deliver it again.
                   (cond reply-id (do (ps/db-council-delivered! db sid [reply-id])
                                      (:content (ps/db-council-get db reply-id)))
                         (or (= "cancelled" (:status (info db sid)))
                             (not (or (live? parent) (awaits-person? parent)))
                             (>= (util/now-ms) deadline))
                         nil
                         :else (do (park! ks gens signal-tick-ms deadline) (recur))))))
             (finally (swap! questions #(if (= entry-id (get-in % [sid :entry_id]))
                                          (dissoc % sid)
                                          %))))))))

(defn ask
  "As a subagent, ask your parent for a decision and wait for the reply text. If only the person
   can decide, your parent asks the person and you keep waiting. Returns None when the parent
   stops or does not reply within timeout_secs (default 600). Then choose the safest option and
   say so in your answer."
  ([env question] (ask env question {}))
  ([env question opts]
   (when (or (not (string? question)) (str/blank? question))
     (fail! :invalid-request "question must be a nonblank string"))
   (result :council.ask (parent-reply! env "council.ask" question nil opts))))

(defn ask-budget
  "As a subagent, ask your parent for more model iterations and wait for the answer. Ask before
   your budget ends, with the number that you need and the reason. Returns a dict with granted
   (0 when the parent refuses), iteration_budget and reply. reply is None when the parent is not
   running or does not answer within timeout_secs (default 600)."
  ([env iterations reason] (ask-budget env iterations reason {}))
  ([env iterations reason opts]
   (when-not (and (integer? iterations) (<= 1 (long iterations)))
     (fail! :invalid-request "iterations must be a positive integer"))
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

(defn ask-files
  "As a subagent, ask your parent for file access and wait for the answer. files holds write,
   read_only and deny rules: paths or globs, absolute, ~ or relative to the parent's workspace
   root. Give at least one rule. deny wins over read_only, and read_only wins over write.

   Returns a dict with granted, files (your grant now), missing and reply. granted is True
   only when the grant covers every write rule that you asked for. missing lists the others. reply
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

         missing
         (vec (remove (fn [path]
                        (some #(process-jail/rule-covers? % path) (:write after)))
                (policy/resolve-rules (:write (walk/keywordize-keys files))
                                      (or (:base-dir security) (System/getProperty "user.dir"))
                                      (or (:home security) (System/getProperty "user.home")))))]

     (result :council.ask_files
             ;; A deny leaves the grant as it was, also when nothing is missing.
             {:granted (and (some? reply) (not= before after) (empty? missing))
              :files (shown-files env after)
              :missing (mapv #(shown-rule (:workspace/root env) %) missing)
              :reply reply}))))

(defn- answer-request!
  "Reply with `text` to the open `kind` request of child `sid`, `:budget` or `:files`. Answers
   true when such a request was open."
  [env sid kind text]
  (let [{:keys [entry_id] :as question} (get @questions sid)]
    (when (get question kind)
      ((runtime-fn :publish) env {:content text :reply_to entry_id})
      (swap! questions #(if (= entry_id (get-in % [sid :entry_id])) (dissoc % sid) %))
      true)))

(defn- current-record
  "A list with the one current record of child `sid`."
  [env sid]
  [(shown-record env
                 (record (:db-info env)
                         (:router env)
                         (runtime-fn :live?)
                         (held-fn)
                         (util/now-ms)
                         (info (:db-info env) sid)))])

(defn- extend-records
  "Add iterations to the budget of a subagent and answer its record."
  [env session-id iterations]
  (let [{:keys [session_id iterations]}
        (checked env :extend {:session_id session-id :iterations iterations})

        db
        (:db-info env)]

    (when (= (str (:session-id env)) session_id)
      (fail! :not-owner "A subagent cannot extend its own budget"))
    (let [row (or (ps/db-agent-extend-budget! db session_id iterations)
                  (fail! :budget-exceeded "The subagent budget did not change"))]
      ;; The team row shows the budget, and a plain extend answers no request.
      (when-let [changed! (get @runtime :changed!)]
        (changed! session_id))
      (answer-request!
        env
        session_id
        :budget
        (str "Granted " iterations " more iterations. New budget: " (:iteration_budget row) ".")))
    (current-record env session_id)))

(defn extend-agent
  "Give a subagent more model iterations. Use it to answer a budget request (status asking with
   budget_request), or before a follow-up ping to a budget_limited child. A budget has no upper
   limit. Returns the record."
  [env session-id iterations]
  (result :council.extend (extend-records env session-id iterations)))

(defn- sentence
  "`text` trimmed, with a final period when it ends without punctuation."
  [text]
  (let [t (str/trim (str text))]
    (if (re-find #"[.!?]$" t) t (str t "."))))

(defn- deny-records
  "Refuse the open request of a subagent and answer its record."
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
    (current-record env session_id)))

(defn deny-agent
  "Refuse the open budget or file request of a subagent, with a short reason. The child keeps its
   budget and its files. Returns the record."
  [env session-id reason]
  (result :council.deny (deny-records env session-id reason)))

(defn- answer-records
  "Reply with `text` to the open question of a subagent and answer its record. A person answers
   here for the parent, so the child continues without the wait."
  [env session-id text]
  (let [{:keys [session_id text]}
        (checked env :answer {:session_id session-id :text text})

        {:keys [entry_id]}
        (get @questions session_id)]

    (when-not entry_id (fail! :invalid-request "That subagent has no open question"))
    ((runtime-fn :publish) env {:content text :reply_to entry_id})
    (swap! questions #(if (= entry_id (get-in % [session_id :entry_id])) (dissoc % session_id) %))
    (current-record env session_id)))

(defn- overlaps?
  "True when the write rules `a` and `b` can name one file: one rule root covers the other."
  [a b]
  (boolean (some (fn [x]
                   (some (fn [y]
                           (let [x
                                 (policy/rule-root x)

                                 y
                                 (policy/rule-root y)]

                             (or (process-jail/rule-covers? x y) (process-jail/rule-covers? y x))))
                         b))
                 a)))

(defn- grant-records
  "Add file rules to the grant of a subagent and answer its record."
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
        (answer-request!
          env
          session_id
          :files
          (str "Granted: " (grant-text files) ". Your sandbox restarts before your next block."))

        [record]
        (current-record env session_id)]

    [(if-let [warning (not-empty (str/join " "
                                           (cond-> []
                                             (seq (:unchanged child))
                                             (conj (str "No change for "
                                                        (str/join ", " (:unchanged child))
                                                        ": the subagent can already write there."))

                                             (seq siblings)
                                             (conj (str
                                                     "Active subagents can also write these files: "
                                                     (str/join ", " siblings)
                                                     ". Give each child its own files.")))))]
       (assoc record :warning warning)
       record)]))

(defn grant-files
  "Add file rules to the grant of a subagent. Use it to answer a file request (status asking with
   files_request). For a follow-up task with other files, pass files to council.publish instead.

   files holds write, read_only and deny rules, like a task dict. Give at least one rule. deny wins
   over read_only, and read_only wins over write.

   Returns the record. Its warning names write rules that changed nothing and active siblings that
   can write the same files.

   A grant gives at most your own access. A write rule outside your writable roots or under your
   deny rules fails, and your deny rules stay closed to the child. A write rule under a read_only or
   deny rule of the child fails too. A child without write rules already writes where you write, so
   a write grant changes nothing for it. The child gets the new access at its next block."
  [env session-id files]
  (result :council.grant (grant-records env session-id files)))

(defn queue-turn!
  "Queue subagent `sid` for a follow-up turn. A Council ping from its parent uses it, so
   a new attempt does not change the record of the previous one. An ordinary ping can pass the active cap; only
   a new spawn counts against it. Answers a function that undoes the change, or nil when the child
   has no iterations left, or is cancelled while its leader runs no turn. A child that already runs
   stays as is."
  [db sid]
  (let [child (info db sid)]
    (cond (or (nil? child) (contains? #{"queued" "running"} (:status child))) (constantly nil)
          (not (resumable? child)) nil
          (ps/db-agent-resume! db sid (count (ps/db-list-session-turns db sid)) (util/now-ms))
          #(ps/db-agent-update!
             db
             sid
             (merge
               {:result nil :outcome nil :started_at nil :finished_at nil :runs nil :turn_ids nil}
               (select-keys child
                            [:status :resumed_turns :resumed_at :result :outcome :started_at
                             :finished_at :runs :worked_ms :turn_ids]))))))

(defn task-request
  "The turn request of a Council wake from the parent of managed child `sid`, or nil for another
   author. The entry is the child's task, so the request gives its text as an instruction. It asks
   for the whole result as the final answer, because the parent reads that answer."
  [db sid entry]
  (let [parent
        (:parent_id (info db sid))

        id
        (:entry_id entry)]

    (when (and parent (= (str parent) (str (:author_session_id entry))))
      (str "Your parent session "
           parent
           " gives you this task in Council entry #"
           id
           ". It is your own task, not peer data. Do it now with your tools, inside its scope.\n\n"
           "Task:\n" (:content entry)
           "\n\n"
           (when (:reply_required entry)
             (str
               "When the task is done, reply once with the result: await council.publish(result, "
               "reply_to="
               id
               "). "))
           "End your turn with the complete result as your final answer. Your parent reads that "
           "answer, so a short acknowledgement such as \"Done.\" loses your work."))))

(defn follow-up-refusal
  "The refusal of a ping from `author-id` to its idle child `target-id`, as `[error message]`, or
   nil. A ping from the parent or the leader gives an idle child a follow-up task. The child
   needs iterations left. A cancelled child also needs a running turn of its leader. So a late
   ping cannot restart a child that the user stopped."
  [db author-id target-id]
  (let [child (info db target-id)]
    (when (and child
               (some #{(str author-id)} [(str (:parent_id child)) (str (:leader_id child))])
               (not (contains? #{"queued" "running"} (:status child))))
      (cond
        (= "waiting" (:status child)) [:invalid-recipient
                                       (str "Subagent "
                                            (:id child)
                                            " waits for "
                                            (str/join ", " (:after child))
                                            ". It starts by itself when they complete.")]
        (= "blocked" (:status child))
        [:invalid-recipient
         (str "Subagent "
              (:id child)
              " never started: a task in its after did not complete. Ping "
              "that task with a fix, and "
              (:id child)
              " starts when it completes. Or cancel it.")]
        (and (= "cancelled" (:status child)) (not (leader-active? (:leader_id child))))
        [:invalid-recipient
         "A cancelled subagent takes a follow-up task only during a running turn of its leader"]
        (>= (long (:iterations_used child)) (long (:iteration_budget child)))
        [:budget-exceeded
         "The subagent has no iterations left. Call council.extend(session_id, iterations) first."]))))

(defn publish-with-files!
  "Publish Council entry `opts` with `publish!`, a function of the opts. With `:files`, the one
   ping recipient must be an idle subagent of the caller. Its grant then changes before the wake,
   so its follow-up task starts in that scope (#414). A failed publication or an unavailable reply
   restores the old grant. A task id such as a2 in ping names that child."
  [env opts publish!]
  (let [ping
        (:ping opts)

        opts
        (cond-> opts
          (sequential? ping)
          (assoc :ping (mapv #(resolve-child (:db-info env) (:session-id env) %) ping)))]

    (if-not (contains? opts :files)
      (publish! opts)
      (let [db
            (:db-info env)

            ping
            (:ping opts)

            ;; A reply without ping goes to the author of the entry that it answers.
            id
            (cond (and (sequential? ping) (= 1 (count ping))) (str (header/unmark-session-id
                                                                     (first ping)))
                  (and (nil? ping) (:reply_to opts)) (some-> (ps/db-council-get db (:reply_to opts))
                                                             :author_session_id
                                                             str))

            child
            (info db id)

            _
            (when-not (and child (= (str (:session-id env)) (str (:parent_id child))))
              (fail! :invalid-request
                     (str "files needs exactly one recipient: an idle subagent of this session, "
                          "in ping or as the author of the reply_to entry")))

            _
            (when (or ((runtime-fn :live?) id) (contains? #{"queued" "running"} (:status child)))
              (fail! :invalid-request
                     "The subagent still runs. Give it more files with council.grant."))

            scope!
            (fn [rules]
              ((runtime-fn :grant) env {:session_id id :files rules :replace true}))

            before
            (:files child)

            entry
            (do (scope! (walk/keywordize-keys (:files opts)))
                (try (publish! opts) (catch Throwable t (scope! before) (throw t))))]

        (when (some #(= "unavailable" (:state %)) (:replies entry)) (scope! before))
        entry))))

(defn publish-warning
  "A short warning when Council publication `opts` of the caller pings a child with write rules
   while a verify or audit child of the same leader is queued or running, else nil. New edits then
   change the files that the verifier checks."
  [env opts]
  (let [db
        (:db-info env)

        me
        (str (:session-id env))

        ping
        (:ping opts)

        writers
        (when (sequential? ping)
          (filter #(and %
                        (seq (get-in % [:files :write]))
                        (some #{me} [(str (:parent_id %)) (str (:leader_id %))]))
                  (map #(info db (resolve-child db me %)) ping)))

        pinged
        (set (map #(str (:session_id %)) writers))

        checkers
        (when (seq writers)
          (keep #(when (and (contains? #{"verify" "audit"}
                                       (some-> (:role %)
                                               name))
                            (contains? #{"queued" "running"} (:status %))
                            (not (pinged (str (:session_id %)))))
                   (or (:id %) (str (:session_id %))))
                (ps/db-agent-list db (leader-id db me))))]

    (when (seq checkers)
      (str "Verifiers still run: " (str/join ", " checkers)
           ". This ping can change the files that they check. Collect all findings, send them in "
           "one round, then ping the verifiers on the final files."))))

(defn operation!
  "Run one managed-subagent operation for the calling session and answer its plain result.
   Gateway routes and extensions call this entry point; model tools call the same functions.
   `operation` is :spawn, :route, :run, :extend, :deny, :answer,
   :grant or :timeline. Records answer in their wire form."
  [env operation opts]
  (let [opts (walk/keywordize-keys (or opts {}))]
    (when (= :run operation) (check-arguments! operation (wire/->wire opts)))
    (if (contains? #{:spawn :route} operation)
      (runtime-op! env operation opts)
      (wire/->wire
        (case operation
          :run
          (mapv (fn [{:keys [id child-id child-session-id]}]
                  {:task_id id :id child-id :session_id child-session-id})
                (start-tasks env (:tasks opts)))

          :extend
          (extend-records env (:session_id opts) (:iterations opts))

          :deny
          (deny-records env (:session_id opts) (:reason opts))

          :answer
          (answer-records env (:session_id opts) (:text opts))

          :grant
          (grant-records env (:session_id opts) (:files opts))

          :timeline
          (timeline (:db-info env)
                    (:session-id env)
                    (resolve-child (:db-info env) (:session-id env) (:session_id opts)))

          (fail! :invalid-request (str "Unknown subagent operation " (name operation))))))))

(def symbols
  (mapv
    (fn [[v sym call params result-doc]]
      (extension/symbol v
                        (cond-> {:symbol sym
                                 :inject-env? true
                                 :active-fn (fn [env]
                                              (enabled? (:db-info env) (:session-id env)))
                                 :tag :mutation
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
       "A list of records. Every record has the same keys for each status: `session_id`, `id`, `status`, `after`, `answer`, `question`, `question_entry_id`, `budget_request`, `files_request`, `files`, `iteration_budget`, `iterations_used`, `provider`, `model`, `activity`, `notes`, `messages`, `step_secs`, `elapsed_secs`, `alerts`, `warning`, `error` and `alternatives`. A key without a value is None. `activity`, `messages`, `step_secs`, `elapsed_secs` and `alerts` have values only for a running child."]
      [[#'run 'council.run {:pos ["tasks"]} [] "An ordered list of SubagentTask handles."]
       [#'extend-agent 'council.extend {:pos ["session_id" "iterations"]} [] records]
       [#'deny-agent 'council.deny {:pos ["session_id" "reason"]} [] records]
       [#'grant-files 'council.grant {:pos ["session_id" "files"]} [] records]
       [#'ask 'council.ask {:pos ["question"] :rest :always} ["timeout_secs"]
        "A plain string with the parent's reply, or None."]
       [#'ask-budget 'council.ask_budget {:pos ["iterations" "reason"] :rest :always}
        ["timeout_secs"] "A dict with `granted`, `iteration_budget` and `reply`."]
       [#'report-result 'council.result {:pos ["text"] :rest :always} ["outcome"]
        "A dict with `text` and, for a verifier, `outcome`."]
       [#'ask-files 'council.ask_files {:pos ["files" "reason"] :rest :always} ["timeout_secs"]
        "A dict with `granted`, `files`, `missing` and `reply`."]])))

(defn routing-change
  "The route command issued for this session since `before`, as
   `{:preference {:provider :model} :command command}`, or nil. Agent routing and the
   engine's authentication rescue issue commands; a manual pick is composer state that
   the next submitted turn snapshots, so it never re-routes a running turn."
  [env before]
  (let [command (smodel/route-command (:session-id env))]
    (when (and command (not= command before))
      {:preference (select-keys command [:provider :model]) :command command})))

(defn- value-pair
  "The `{:provider :model}` that the model value `v` holds, or nil when it is empty."
  [v]
  (let [field
        #(some-> (or (get v %) (get v (keyword %)))
                 str
                 str/trim
                 not-empty)

        provider
        (field "provider")

        model
        (field "model")]

    (when (and provider model) {:provider provider :model model})))

(defn model-settings
  "The subagent model settings where session `sid` runs now: `{:models [pair]}`. `:models` keeps
   the order of the setting, so its first pair is the default model. An empty setting gives `[]`."
  [db sid]
  (let [models (get (scoped/values db sid) "subagent_models")]
    {:models (if (sequential? models) (vec (distinct (keep value-pair models))) [])}))

(defn routing-view
  "The subagent model settings of session `sid` as `session[\"routing\"][\"subagents\"]` shows
   them in the sandbox: `models` is a list of providers and models, where the first is the default
   and an empty list allows every model."
  [db sid]
  {"models" (mapv #(hash-map "provider" (:provider %) "model" (:model %))
                  (:models (model-settings db sid)))})

(defn- model-name [model] (if (map? model) (:name model) (str model)))

(defn- fallback-first
  "`router` with the provider of `fallback` first in priority and `fallback` as its root, so
   automatic fallback tries that model before the rest of the fleet. A model that only the live
   catalog of the provider lists joins its models. A provider outside `router` changes nothing."
  [router {:keys [provider model]}]
  (let [own? #(= provider (name (:id %)))]
    (if-let [p (first (filter own? (:providers router)))]
      (assoc router
        :providers (provider-service/reprioritize-providers
                     (cons (assoc p
                             :models (cond-> (vec (:models p))
                                       (not-any? #(= model (model-name %)) (:models p))
                                       (conj {:name model}))
                             :root model)
                           (remove own? (:providers router)))))
      router)))

(defn restrict-router
  "Apply the routing limits of a subagent to every request and retry, including automatic
   fallback. The fallback model of the child comes first in automatic fallback, and an inherited
   allowlist narrows the providers. A 429 ends the request at once, without a wait: it goes to
   the fallback model of the child, else the parent chooses the next route from the child record."
  [env router]
  (if-let [child (info (:db-info env) (:session-id env))]
    (let [fallback (:fallback_model child)]
      (cond-> (update router
                      :rate-limit assoc
                      :fail-fast? true
                      :fallback-provider? (some? fallback))
        fallback
        (fallback-first fallback)

        (seq (:allowed_models child))
        (as-> r (assoc r
                  :providers
                  (into []
                        (keep (fn [provider]
                                (let [models
                                      (filterv (fn [model]
                                                 (some #(and (= (name (:id provider)) (:provider %))
                                                             (= (model-name model) (:model %)))
                                                       (:allowed_models child)))
                                        (:models provider))]
                                  (when (seq models)
                                    (assoc provider
                                      :models models
                                      :root (if (some #(= (:root provider) (:name %)) models)
                                              (:root provider)
                                              (:name (first models))))))))
                        (:providers r))))))
    router))

(def ^:private lead-guide
  "The leader rules for subagents: an independent verifier and its checks."
  "- Start one verifier early with role verify. Give it the request and its acceptance criteria, not your design. When your work is ready, run its checks and fix each failure. Then start a follow-up for it to check the final files.
- Use `child.id` or `child.session_id` to refer to the child. Use `child.task_id` to identify the attempt.
- To order tasks, set `after=[child]` or give child IDs. If a dependency fails, start a follow-up to fix it before you retry the blocked work.
- When a verifier fails, start a follow-up for its implementer with the failures. When the fix is ready, start a follow-up for the verifier.
- Before your final answer, run every failing case that a child reported.
- Do not send new edits to an implementer while a verifier or auditor of its work runs. Collect all findings, send them in one round, then start or give a follow-up to the verifiers on the final files.
")

(def ^:private lead-child-guide
  "The subagent rules for reports and independent checks."
  "- As a subagent, do only your task. Before your final answer, call council.result(text) with one line of at most 200 characters; as a verifier, add outcome=\"passed\" or \"failed\". Give the complete result and its evidence in your final answer. The parent reads that answer from the Task. For a decision, call council.ask(question). For more budget or files, call council.ask_budget(iterations, reason) or council.ask_files({\"write\": [path]}, reason). You cannot start subagents.
- As a verifier, check the request, not your parent's summary. Write checks only in your scratch directory.")

(defn prompt
  [env]
  (when (enabled? (:db-info env) (:session-id env))
    (str
      "## Subagents (Council)
- Use subagents only when the user asks or the work has two or more large independent parts. When in doubt, do the work yourself.
- Do the critical-path step and shared work yourself. Give one coupled design one owner: never split modules that share an invariant, a protocol or a data format. Treat the modules of one program as one coupled design.
- Start children with `children = await council.run([...])`, usually two to four in total. Each task names its goal, known facts, files, done condition, role and a short title of at most 60 characters. files={\"write\": [...]} enforces the split; `doc(\"council.run\")` gives the rules.
"
      role-guide
      "- A child sees your requests and notes, not your tool output. For an independent review, give the child the code, not your conclusion.
- Await a child for its final record. To wait for one of several children, use `await asyncio.wait(children, return_when=asyncio.FIRST_COMPLETED)`. Only the tasks in `done` finished. Use `child.record()` for a snapshot.
- A Council question can release an await without ending the Task. If `await child` returns the handle, read the question and reply once with `await council.publish(text, reply_to=question_entry_id)`. Then await the same child again. For `budget_request`, use `council.extend` or `council.deny`. For `files_request`, use `council.grant` or `council.deny`.
- Read `error` in a failed record. For `rate_limited`, start the work on a route from `alternatives`. For `budget_limited`, read the handoff, call `council.extend`, and start a follow-up.
- While children run, do your own work. Do not do their work again. When a child ends, check its files and tests at once.
- For a follow-up, use `[next_child] = await council.run([{\"child\": child, \"task\": task}])`. Its `files` apply to that attempt only; without `files` the child has its original grant. Await `next_child` for the new answer. An await returns at once while the child has an open question. Use `child.cancel()` to request cancellation of only that attempt.
"
      lead-guide
      lead-child-guide)))
