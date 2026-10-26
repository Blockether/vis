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
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.provider.auth-health :as auth-health]
            [com.blockether.vis.internal.sandbox.jail :as process-jail]
            [com.blockether.vis.internal.sandbox.policy :as policy]
            [com.blockether.vis.internal.session.iteration :as iteration]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.internal.util :as util]))

(defn info [db sid] (when (and db sid) (ps/db-agent-info db sid)))

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

(defn context
  [env]
  (let [sid
        (str (:session-id env))

        child
        (info (:db-info env) sid)]

    (wire/->wire (cond-> (merge {:role (if child "subagent" "leader")
                                 :session_id sid
                                 :leader_id sid
                                 ;; A one-shot `:cli` run has nobody to answer or approve.
                                 :interactive (not= :cli (:channel env))}
                                (select-keys child
                                             [:parent_id :leader_id :team_id :task :iteration_budget
                                              :iterations_used :status :allowed_models])
                                (when (:files child) {:files (shown-files env (:files child))})
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
   council.resume. A child that answered within its budget is `completed`. The first end of a task
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

    (doseq [{sid :session_id} orphans]
      (ps/db-agent-update! db sid {:status "failed" :finished_at (util/now-ms)}))
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
   task has no turn yet. After council.resume, only a turn of the follow-up task counts."
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

(defn- settle!
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
  "Validate `opts` and run one gateway runtime operation: :spawn, :list, :cancel, :route or :grant."
  [env operation opts]
  (let [opts (checked env operation opts)]
    (if-let [f (get @runtime operation)]
      (f env opts)
      (fail! :unavailable "Managed subagents require the gateway runtime"))))

(defn- result [op value] (extension/success {:op op :result (wire/->wire value)}))

(def ^:private terminal-statuses #{"completed" "failed" "cancelled" "budget_limited"})

;; Open child questions by child session id: {:question text :entry_id id}, plus
;; `:budget {:iterations n :reason text}` for a budget request or `:files {:files grant :reason
;; text}` for a file request. They stay in memory: the asking child blocks in this process, so a
;; restart ends the question with its turn.
(defonce ^:private questions (atom {}))

(defn open-request
  "The open question that child `sid` waits on its parent to answer, or nil: `{:question text
   :entry_id id}`, plus `:budget` or `:files` for a budget or file request."
  [sid]
  (get @questions (str sid)))

(defn- runtime-fn
  [k]
  (or (get @runtime k) (fail! :unavailable "Managed subagents require the gateway runtime")))

(defn- held-fn
  "The runtime check for a held queue. Only the gateway holds queues; another runtime has no
   held child."
  []
  (or (get @runtime :held?) (constantly false)))

(defn- record
  "One wait record. A queued or running child without a live turn after 30 s settles from its
   last turn: see [[settle!]]. A queued
   child whose queue a human holds is held. A child that a 429 stopped gets error and the working
   routes of `router` in alternatives."
  [db router live? held? now row]
  (let [sid
        (str (:session_id row))

        row
        (if (and (contains? #{"queued" "running"} (:status row))
                 (> (- (long now)
                       (max (long (or (:created_at row) now)) (long (or (:resumed_at row) 0))))
                    30000)
                 (not (live? sid)))
          (settle! db row)
          row)

        status
        (:status row)

        open?
        (contains? #{"queued" "running"} status)

        question
        (when open? (get @questions sid))

        failure
        (when (= "failed" status) (or (rate-limit-failure db row) (stop-failure db row)))]

    (cond-> {:session_id sid
             :status (cond question "asking"
                           (and (= "queued" status) (held? sid)) "held"
                           :else status)
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
      (assoc :files (:files row))

      failure
      (assoc :error
        failure :alternatives
        (route-alternatives router)))))

(defn- shown-record
  "The wait record `rec` with its file grant in the form that the model reads."
  [env rec]
  (cond-> rec
    (:files rec)
    (update :files #(shown-files env %))))

;; The position of the last step that a wait showed, by child turn id. A wait shows only newer
;; notes and messages. The marks stay in memory: after a restart, a wait shows the latest steps
;; again.
(defonce ^:private shown (atom {}))

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

(defn- current-steps
  "The turn of child `row` for its current task and the steps of that turn, as `[turn steps]`.
   A forked child's copied turns are older than its task, so they never count."
  [db row now]
  (let [since
        (- (long (started-at row now)) 1000)

        turn
        (last (filter #(>= (long (or (epoch-ms (:created-at %)) 0)) since)
                      (ps/db-list-session-turns-meta db (:session_id row))))]

    [turn
     (if turn
       (vec (sort-by #(long (or (:position %) 0))
                     (ps/db-list-session-turn-iterations db (:id turn))))
       [])]))

(defn- fresh-steps
  "The steps of `turn` after the last step that a wait showed."
  [turn steps]
  (let [mark (long (get @shown (str (:id turn)) -1))]
    (filterv #(> (long (or (:position %) -1)) mark) steps)))

(defn- published?
  "Whether running child `row` published a Council message that no wait showed yet."
  [db row now]
  (let [[turn steps] (current-steps db row now)]
    (boolean (some #(seq (:council-publications %)) (fresh-steps turn steps)))))

(defn- digest
  "The progress of running child `row` since the last wait: its `activity`, its new progress
   `notes` and Council `messages`, `step_secs` in its current step, `elapsed_secs` in its task
   and `alerts`. The alerts are slow_step after 300 s in one step, errors for three failed steps
   of the last five, and budget at 80% of the iteration budget. Marks the steps as shown."
  [db row now]
  (let [[turn steps]
        (current-steps db row now)

        fresh
        (fresh-steps turn steps)

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
                     {:entry_id (:entry_id e) :kind (:kind e) :content (clip (:content e) 600)})))

        alerts
        (cond-> []
          (>= step-secs 300)
          (conj "slow_step")

          (>= errors 3)
          (conj "errors")

          (and used budget (>= (* 10 (long used)) (* 8 (long budget))))
          (conj "budget"))]

    (when-let [p (:position (peek steps))]
      (swap! shown assoc (str (:id turn)) p))
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

(defn- wait-records
  "Poll until every child ends, one child asks or is held, or the timeout ends. A completed
   child gets 5 s to store its answer. The wait also ends when a child that was open at its start
   ends or publishes a Council message. Then the record of each running child also has its
   [[digest]]."
  [env ids timeout-secs]
  (let [list-fn
        (runtime-fn :list)

        live?
        (runtime-fn :live?)

        held?
        (held-fn)

        db
        (:db-info env)

        me
        (str (:session-id env))

        ids
        (when (seq ids) (set (map str ids)))

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

                      {:session_id i :status "unknown"}))))

        ;; A completed child gets 5 s to store its answer.
        done-at
        (volatile! nil)

        ;; An event wait keeps the children that were open at the first poll, when each
        ;; of them ended, the step count of each running child and the rows of the last poll.
        open-ids
        (volatile! nil)

        ended
        (volatile! {})

        steps
        (volatile! {})

        last-rows
        (volatile! [])

        event?
        (fn [now rows recs]
          (when (nil? @open-ids)
            (vreset! open-ids
                     (set (keep #(when-not (terminal-statuses (:status %)) (:session_id %)) recs))))
          (doseq [{sid :session_id status :status}
                  recs

                  :when (and (contains? @open-ids sid)
                             (terminal-statuses status)
                             (not (contains? @ended sid)))]

            (vswap! ended assoc sid now))
          (or (some (fn [rec]
                      (when-let [t (get @ended (:session_id rec))]
                        (or (not= "completed" (:status rec))
                            (:answer rec)
                            (> (long now) (+ (long t) 5000)))))
                    recs)
              (some (fn [row]
                      (let [sid
                            (str (:session_id row))

                            n
                            (:iterations_used row)]

                        (when (and (= "running" (:status row)) (not= n (get @steps sid ::none)))
                          (vswap! steps assoc sid n)
                          (published? db row now))))
                    rows)))

        recs
        (util/poll
          (util/deadline-ms timeout-secs)
          1000
          (fn []
            (let [now
                  (util/now-ms)

                  rows
                  (vec (pick))

                  recs
                  (mapv #(shown-record env (record db (:router env) live? held? now %)) rows)

                  done?
                  (every? #(or (terminal-statuses (:status %)) (= "unknown" (:status %))) recs)

                  answered?
                  (every? #(or (not= "completed" (:status %)) (:answer %)) recs)]

              (vreset! last-rows rows)
              (when (and done? (nil? @done-at)) (vreset! done-at now))
              (if (or (some #(contains? #{"asking" "held"} (:status %)) recs)
                      (and done? (or answered? (> now (+ (long @done-at) 5000))))
                      (event? now rows recs))
                (reduced recs)
                recs))))

        now
        (util/now-ms)

        rows
        (into {} (map (juxt #(str (:session_id %)) identity)) @last-rows)]

    (mapv #(if (= "running" (:status %)) (merge % (digest db (get rows (:session_id %)) now)) %)
          recs)))

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

(defn- run-records
  "Spawn a subagent for each task, wait for all of them and answer their records."
  [env tasks opts]
  (let [opts
        (walk/keywordize-keys opts)

        specs
        (mapv task-spec (if (or (string? tasks) (map? tasks)) [tasks] tasks))

        _
        (when (empty? specs) (fail! :invalid-request "tasks must not be empty"))

        ;; Check every task before the first spawn, so a bad task starts no child.
        _
        (check-arguments! :run (wire/->wire {:tasks specs}))

        _
        (run! #(check-route! env %) specs)

        deadline
        (util/deadline-ms (wait-timeout opts 240))

        ;; A full team admits the next child when an earlier one ends.
        spawn
        (fn [spec]
          (let [r (util/poll deadline
                             1000
                             #(try (reduced (str (:session_id (runtime-op! env :spawn spec))))
                                   (catch clojure.lang.ExceptionInfo e
                                     (if (= :agent-limit (:error (ex-data e))) e (throw e)))))]
            (if (string? r) r (throw r))))

        ids
        (mapv spawn specs)]

    (wait-records env ids (max 1 (util/seconds-left deadline)))))

(defn run
  "Run each task in its own subagent and wait for all of them. A task is a string or a dict with
   task and optional title, role, model, provider, iteration_budget and files. iteration_budget
   defaults to 120 (max 600). Give a large task more at the start.

   title is a short label for people: one line of 1 to 40 characters, such as \"Fix login test\".
   Without it, people see the child number, such as #2.

   role is research, implement, verify or audit. It is a label for people and sets no tool, file or
   model limit. Choose it by the result that you need: facts, changed files, a pass or fail, or
   findings.

   files is a file grant: {\"write\": [...], \"read_only\": [...], \"deny\": [...]}. Give only the
   lists that you need. An empty list adds no rules. Each rule is a path or a glob, relative to
   your workspace root, ~ or absolute, and also covers files that do not exist yet.

   With write, the child can change only the files that its rules match. deny wins over read_only,
   and read_only wins over write. Without files, the child writes where you write. With the jail
   off, only host file tools follow all rules: Python follows only write, and shell follows none.

   A child gets the system prompt, your requests, notes and answers, and its task. It does not get
   your tool calls or their output. Children share the checkout. Only a leader starts subagents.

   Records have session_id, status, answer and question. More than 16 tasks start as earlier ones
   finish. timeout_secs defaults to 240 (max 1800). If a status is still running, call
   council.wait. A failed record can have error with its code and message.

   The call also returns when the first child ends, asks or publishes a message. A running
   child's record then also has activity, notes, messages, step_secs, elapsed_secs and alerts.

   asking with budget_request needs council.extend or council.deny. asking with files_request
   needs council.grant or council.deny. budget_limited needs council.extend before council.resume,
   and it stays budget_limited until council.resume."
  ([env tasks] (run env tasks {}))
  ([env tasks opts] (result :council.run (run-records env tasks opts))))

(defn- wait-records-for
  "Wait for the subagents `ids`, or for all children, and answer their records."
  [env ids opts]
  (let [opts
        (walk/keywordize-keys opts)

        ids
        (if (string? ids) [ids] ids)]

    (wait-records env ids (wait-timeout opts 240))))

(defn wait-agents
  "Wait again for subagents by session_ids (default: all your children). Same records and timeout as council.run."
  ([env] (wait-agents env nil {}))
  ([env ids-or-opts]
   (if (map? ids-or-opts) (wait-agents env nil ids-or-opts) (wait-agents env ids-or-opts {})))
  ([env ids opts] (result :council.wait (wait-records-for env ids opts))))

(defn- cancel-records
  "Cancel a subagent and answer its record."
  [env session-id]
  (runtime-op! env :cancel {:session_id session-id})
  (wait-records env [session-id] 5))

(defn cancel-agent
  "Cancel a subagent, including its queued work. Returns its record."
  [env session-id]
  (result :council.cancel (cancel-records env session-id)))

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

        deadline
        (util/deadline-ms (wait-timeout (walk/keywordize-keys opts) 600))

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
        (try (util/poll deadline
                        1000
                        (fn []
                          (let [reply-id (some :reply_entry_id
                                               (:replies (ps/db-council-get db entry-id)))]
                            ;; The call returns the reply, so Council input must not deliver it again.
                            (cond reply-id (reduced (do (ps/db-council-delivered! db sid [reply-id])
                                                        (:content (ps/db-council-get db reply-id))))
                                  (or (= "cancelled" (:status (info db sid))) (not (live? parent)))
                                  (reduced nil)
                                  :else nil))))
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
      ((runtime-fn :publish) env {:kind "informational" :content text :reply_to entry_id})
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
    (current-record env session_id)))

(defn extend-agent
  "Give a subagent more model iterations. Use it to answer a budget request (status asking with
   budget_request), or before council.resume of a budget_limited child. A budget stays at most
   600. Returns the record."
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
    ((runtime-fn :publish) env {:kind "informational" :content text :reply_to entry_id})
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
   files_request). For a follow-up task with other files, pass files to council.resume instead.

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
  "Queue subagent `sid` for a follow-up turn. A Council ping and council.resume both use it, so
   council.wait reports only the answer of the new turn. A follow-up can pass the active cap; only
   a new spawn counts against it. Answers a function that undoes the change, or nil when the child
   is cancelled or has no iterations left. A child that already runs stays as is."
  [db sid]
  (let [child (info db sid)]
    (cond (or (nil? child) (contains? #{"queued" "running"} (:status child))) (constantly nil)
          (ps/db-agent-resume! db sid (count (ps/db-list-session-turns db sid)) (util/now-ms))
          #(ps/db-agent-update! db
                                sid
                                (merge {:result nil :outcome nil :finished_at nil}
                                       (select-keys child
                                                    [:status :resumed_turns :resumed_at :result
                                                     :outcome :finished_at]))))))

(defn task-request
  "The turn request of a Council wake from the parent of managed child `sid`, or nil for another
   author. The entry is the child's task, so the request gives its text as an instruction. It asks
   for the whole result as the final answer, because council.wait returns that answer."
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
               "kind=\"informational\", reply_to="
               id
               "). "))
           "End your turn with the complete result as your final answer. Your parent reads that "
           "answer, so a short acknowledgement such as \"Done.\" loses your work."))))

(defn- resume-records
  "Queue a follow-up task for a finished subagent, wait for it and answer its records."
  [env session-id task opts]
  (let [opts
        (walk/keywordize-keys opts)

        {:keys [session_id task files]}
        (checked env
                 :resume
                 (cond-> {:session_id session-id :task task}
                   (contains? opts :files)
                   (assoc :files (:files opts))))

        timeout
        (wait-timeout opts 240)

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
    (when (or (live? session_id) (contains? #{"queued" "running"} (:status child))) (refuse! child))
    ;; The store checks the status and the budget and marks the child queued in one
    ;; transaction. A Council ping wake uses the same step.
    (let [queued
          (or (queue-turn! db session_id) (refuse! (info db session_id)))

          ;; New files replace the grant before the task reaches the child, so its first block
          ;; already has the scope of the follow-up task (#414).
          scope!
          (fn [rules]
            ((runtime-fn :grant) env {:session_id session_id :files rules :replace true}))

          before
          (:files child)

          restore!
          (fn []
            (queued)
            (when files (scope! before)))

          entry
          (try (when files (scope! files))
               ((runtime-fn :publish)
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
    (wait-records env [session_id] timeout)))

(defn resume-agent
  "Give a finished subagent a follow-up task in its own session, with its whole context. Then
   wait like council.run and return the records. A Council ping continues the child the same way,
   without the wait. A child without iterations left needs council.extend first. A running child
   needs council.wait. timeout_secs defaults to 240 (max 1800).

   files gives the follow-up task its own write, read_only and deny rules, like a task dict. They
   replace the grant of the child in the same call, so a read-only reviewer can then edit its
   files. Omit files to keep the grant."
  ([env session-id task] (resume-agent env session-id task {}))
  ([env session-id task opts] (result :council.resume (resume-records env session-id task opts))))

(defn operation!
  "Run one managed-subagent operation for the calling session and answer its plain result.
   Gateway routes and extensions call this entry point; model tools call the same functions.
   `operation` is :spawn, :list, :route, :run, :wait, :cancel, :resume, :extend, :deny, :answer,
   :grant or :timeline. Records answer in their wire form."
  [env operation opts]
  (let [opts
        (walk/keywordize-keys (or opts {}))

        timeout
        (select-keys opts [:timeout_secs])]

    (when (contains? #{:run :wait} operation) (check-arguments! operation (wire/->wire opts)))
    (if (contains? #{:spawn :list :route} operation)
      (runtime-op! env operation opts)
      (wire/->wire
        (case operation
          :run
          (run-records env (:tasks opts) timeout)

          :wait
          (wait-records-for env (:session_ids opts) timeout)

          :cancel
          (cancel-records env (:session_id opts))

          :resume
          (resume-records env
                          (:session_id opts)
                          (:task opts)
                          (select-keys opts [:timeout_secs :files]))

          :extend
          (extend-records env (:session_id opts) (:iterations opts))

          :deny
          (deny-records env (:session_id opts) (:reason opts))

          :answer
          (answer-records env (:session_id opts) (:text opts))

          :grant
          (grant-records env (:session_id opts) (:files opts))

          :timeline
          (timeline (:db-info env) (:session-id env) (:session_id opts))

          (fail! :invalid-request (str "Unknown subagent operation " (name operation))))))))

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
       "A list of records with `session_id`, `status`, `answer`, `question`, `question_entry_id`, `budget_request`, `files_request`, `files`, `iteration_budget` and `iterations_used`. A running child's record also has `activity`, `notes`, `messages`, `step_secs`, `elapsed_secs` and `alerts`."]
      [[#'run 'council.run {:pos ["tasks"] :rest :always} ["timeout_secs"] records]
       [#'wait-agents 'council.wait {:opt-pos ["session_ids"] :rest :always} ["timeout_secs"]
        records] [#'cancel-agent 'council.cancel {:pos ["session_id"]} [] records]
       [#'resume-agent 'council.resume {:pos ["session_id" "task"] :rest :always}
        ["timeout_secs" "files"] records]
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

(defn restrict-router
  "Apply the routing limits of a subagent to every request and retry, including automatic
   fallback. An inherited allowlist narrows the providers. A 429 ends the request at once, without
   a wait or another provider: the parent chooses the next route from the record of the child."
  [env router]
  (if-let [child (info (:db-info env) (:session-id env))]
    (cond-> (update router :rate-limit assoc :fail-fast? true :fallback-provider? false)
      (seq (:allowed_models child))
      (assoc :providers
        (into []
              (keep (fn [provider]
                      (let [models (filterv (fn [model]
                                              (some
                                                #(and (= (name (:id provider)) (:provider %))
                                                      (= (if (map? model) (:name model) (str model))
                                                         (:model %)))
                                                (:allowed_models child)))
                                     (:models provider))]
                        (when (seq models)
                          (assoc provider
                            :models models
                            :root (if (some #(= (:root provider) (:name %)) models)
                                    (:root provider)
                                    (:name (first models))))))))
              (:providers router))))
    router))

(def ^:private lead-guide
  "The leader rules for subagents: an independent verifier and its checks."
  "- Start one verifier early with role verify. Give it the request and its acceptance criteria, not your design. When your work is ready, run its checks and fix each failure. Then council.resume it on the final files.
- Before your final answer, run every failing case that a child reported.
")

(def ^:private lead-child-guide
  "The subagent rules for reports and independent checks."
  "- As a subagent, do only your task. Before your final answer, call council.result(text) with one line of at most 200 characters; as a verifier, add outcome=\"passed\" or \"failed\". End with the complete result and its evidence: command, input and output. For a decision, call council.ask(question). For more budget or files, call council.ask_budget(iterations, reason) or council.ask_files({\"write\": [path]}, reason). You cannot start subagents.
- As a verifier, check the request, not your parent's summary. Write checks only in your scratch directory.")

(defn prompt
  [env]
  (when (enabled? (:db-info env) (:session-id env))
    (str
      "## Subagents (Council)
- Use subagents only when the user asks or the work has two or more large independent parts. When in doubt, do the work yourself.
- Do the critical-path step and shared work yourself. Give one coupled design one owner: never split modules that share an invariant, a protocol or a data format. Treat the modules of one program as one coupled design.
- Start children with council.run([task, ...]), usually two to four in total. Each task names its goal, known facts, files, done condition, role and a short title of at most 40 characters. files={\"write\": [...]} enforces the split; `doc(\"council.run\")` gives the rules.
"
      role-guide
      "- A child sees your requests and notes, not your tool output. For an independent review, give the child the code, not your conclusion.
- council.run, council.wait and council.resume return at the first event or at the timeout. For running, call council.wait(session_ids=[...]) again. For asking, reply with council.publish(text, kind=\"informational\", reply_to=question_entry_id). Answer budget_request with council.extend or council.deny, and files_request with council.grant or council.deny.
- A failed record names its cause in error. For rate_limited, run the task again on a route from alternatives. For budget_limited, the answer is the handoff: call council.extend, then council.resume.
- While children run, do your own work. Do not do their work again. When a child ends, check its files and tests at once.
- For follow-up on the same files, call council.resume(session_id, task, files=...).
"
      lead-guide
      lead-child-guide)))
