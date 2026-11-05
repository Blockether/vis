(ns com.blockether.vis.internal.session.goals
  "Explicit, durable session goals. Only the slash/SDK user path creates or replaces
   an objective. The model can mark that exact objective complete or blocked, but
   cannot resume, replace, cancel or increase its budget. Every mutation is a CAS
   on the persisted revision; the lifecycle version also rejects stale model work.
   Completion is a model declaration supported by evidence, not an independent judge."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel]))

(defonce ^:private listeners (atom #{}))

(defn add-listener! [f] (swap! listeners conj f) f)

(defn remove-listener! [f] (swap! listeners disj f) nil)

(defn check-goal
  "Internal read of the authoritative goal. Model readers use session['goal']."
  ([env] (check-goal (:db-info env) (:session-id env)))
  ([db sid] (when (and db sid) (persistence/db-get-session-goal db sid))))

(defn- active-view
  [goal]
  (select-keys goal
               (cond-> ["id" "version" "status" "objective" "iteration_budget" "reason"]
                 (get goal "iteration_budget")
                 (conj "iterations_used"))))

(defn active-goal
  "The goal that still directs the model, or nil. Model context shows only an active goal.
   The goal rules stay in the cached system prompt, so a status change keeps the prefix cache.
   The view drops bookkeeping that changes at each iteration, because each change costs a
   context delta line. `iterations_used` stays only when a budget limits it."
  [env]
  (let [goal (check-goal env)]
    (when (= "active" (get goal "status")) (active-view goal))))

(def ^:private history-limit "Earlier goals that a session keeps." 20)

(def ^:private history-view-limit
  "Entries of `session['goal_history']`. Each entry costs context on every turn."
  5)

(def ^:private history-objective-chars
  "The longest objective that a `goal_history` entry shows."
  500)

(defn- history-entry
  [goal]
  (let [objective (get goal "objective")]
    {"id" (get goal "id")
     "objective" (if (> (count objective) (long history-objective-chars))
                   (str (subs objective 0 history-objective-chars) "…")
                   objective)
     "status" (get goal "status")
     "reason" (get goal "reason")}))

(defn context
  "The goal keys of the model session view, from one read of the goal.
   `session_goal` is the goal that still directs the model. `session_goal_history` holds
   earlier goals and the current goal when it no longer directs work, newest first."
  [env]
  (let [goal
        (check-goal env)

        active?
        (= "active" (get goal "status"))

        history
        (when (and (:db-info env) (:session-id env))
          (persistence/db-get-session-goal-history (:db-info env) (:session-id env)))

        entries
        (cond->> history
          (and goal (not active?) (not= "cleared" (get goal "status")))
          (cons goal))]

    {"session_goal" (when active? (active-view goal))
     "session_goal_history" (not-empty (mapv history-entry (take history-view-limit entries)))}))

(defn- fail! [message] (throw (ex-info message {:type :goal/invalid})))

(defn- archive
  "Keep the goal that `candidate` replaces at the start of the history. A cleared goal was
   forgotten, so it never enters the history."
  [history before candidate]
  (if (and before
           (not= "cleared" (get before "status"))
           (not= (get before "id") (get candidate "id")))
    (vec (take history-limit (cons before history)))
    (vec history)))

(defn- change!
  "Write `(f goal)` and the goal history in one revision check. `history-f`, when given,
   replaces the history; else the history keeps the goal that a new goal replaces."
  ([db sid f] (change! db sid f nil))
  ([db sid f history-f]
   (when-not (and db sid) (fail! "A persisted session is required for a goal."))
   (loop [attempt 0]
     ;; Read the goal first: the revision check then also protects the history read after it.
     (let [before (check-goal db sid)
           history (vec (persistence/db-get-session-goal-history db sid))
           candidate (f before)
           next-history (if history-f (vec (history-f history)) (archive history before candidate))]

       (if (and (= before candidate) (= history next-history))
         before
         (let [now (util/now-ms)
               after (cond-> (assoc candidate
                               "revision" (inc (long (get before "revision" 0)))
                               "updated_at" now)
                       (and (= (get before "id") (get candidate "id"))
                            (= "active" (get before "status")))
                       (assoc "time_used_ms"
                         (+ (long (get before "time_used_ms"))
                            (max 0 (- now (long (get before "updated_at")))))))]

           (when-not (document/valid-json? "gateway" "session_goal" after)
             (fail! "Invalid goal state."))
           (if (persistence/db-compare-session-goal! db
                                                     sid
                                                     (get before "revision" 0)
                                                     after
                                                     next-history)
             (do (doseq [listener @listeners]
                   (try (listener sid after)
                        (catch Exception e
                          (tel/log!
                            {:level :warn :id ::listener-failed :data {:error (ex-message e)}}))))
                 after)
             (if (< attempt 8)
               (recur (inc attempt))
               (fail!
                 "The goal changed concurrently or the session no longer exists. Retry.")))))))))

(defn set-goal!
  "Explicit user mutation. Replacing a goal creates a new identity and resets usage."
  [db sid objective iteration-budget]
  (when-not (and (string? objective) (<= 1 (count (str/trim objective)) 44000))
    (fail! "Goal objective must contain 1–44000 characters."))
  (when-not (or (nil? iteration-budget)
                (and (integer? iteration-budget) (<= 1 iteration-budget 9007199254740991)))
    (fail! "Goal iteration budget must be an integer from 1 to 9007199254740991."))
  (let [goal {"id" (str (random-uuid))
              "objective" (str/trim objective)
              "status" "active"
              "iteration_budget" iteration-budget
              "iterations_used" 0
              "tokens_used" 0
              "time_used_ms" 0
              "version" 1
              "reason" nil
              "created_at" (util/now-ms)}]
    (change! db sid (constantly goal))))

(defn- budget-reached?
  [goal]
  (when-let [budget (get goal "iteration_budget")]
    (>= (long (get goal "iterations_used" 0)) (long budget))))

(defn control!
  "User-only pause/resume/cancel/clear/clear-all. Resume never resets usage or expands a
   budget. Clear forgets the goal: it stops goal work, leaves model context and the goal
   history, and clients hide it. Clear-all also empties the goal history. The goal stays
   as a cleared record, because clients keep the highest goal revision that they saw.
   Both are idempotent."
  [db sid action]
  (change! db
           sid
           (fn [goal]
             (when (or (nil? goal)
                       (and (= "cleared" (get goal "status"))
                            (not (contains? #{:clear :clear-all} action))))
               (fail! "This session has no goal. Use /goal <objective>."))
             (let [status
                   (get goal "status")

                   next-status
                   (case action
                     :pause
                     (when (= "active" status) "paused")

                     :resume
                     (when (contains? #{"paused" "blocked"} status) "active")

                     :cancel
                     (when-not (= "cancelled" status) "cancelled")

                     (:clear :clear-all)
                     "cleared"

                     nil)]

               (when-not next-status (fail! (str "Cannot " (name action) " a " status " goal.")))
               (when (and (= "active" next-status) (budget-reached? goal))
                 (fail! "Goal iteration budget is exhausted. Set a new explicit goal and budget."))
               (if (= status next-status "cleared")
                 goal
                 (-> goal
                     (assoc "status" next-status
                            "reason" nil)
                     (update "version" inc)))))
           (when (= :clear-all action) (constantly []))))

(defn resume-for-user-turn!
  "Resume an existing paused/blocked goal for a new user message. Preserve usage and
   budget; no goal, exhausted or terminal goals and newer user controls are unchanged.
   The caller must exclude synthetic wakes, command-only turns and cancelled turns."
  [env]
  (let [started-goal (check-goal env)]
    (if (contains? #{"paused" "blocked"} (get started-goal "status"))
      (change! (:db-info env)
               (:session-id env)
               (fn [goal]
                 (if (and (= (get started-goal "id") (get goal "id"))
                          (= (get started-goal "version") (get goal "version"))
                          (contains? #{"paused" "blocked"} (get goal "status"))
                          (not (budget-reached? goal)))
                   (-> goal
                       (assoc "status" "active"
                              "reason" nil)
                       (update "version" inc))
                   goal)))
      started-goal)))

(defn update-goal
  "Internal completion tool. Pass id and version from session['goal'], status 'complete'
   or 'blocked', and a short evidence or reason string. Complete means that every
   requirement is verified. Blocked means that, after the goal blocker audit, no useful
   authorized action remains without user input or an external change. Never lower the
   objective, and never mark blocked only to deliver a progress reply. After the tools of
   this iteration finish, the reason becomes the final reply without another model
   request. Write the reason in Markdown: one plain sentence first, then an empty line and
   short '-' items. For blocked, use the labels **Blocker**, **Tried** and **Needed**."
  [env goal-id version status reason]
  (when-not (contains? #{"complete" "blocked"} status)
    (fail! "Only complete or blocked may be declared by the model."))
  (when-not (and (string? reason) (<= 1 (count (str/trim reason)) 2000))
    (fail! "Supply a concise, non-blank completion evidence or blocker reason."))
  (change!
    (:db-info env)
    (:session-id env)
    (fn [goal]
      (when-not (and (= goal-id (get goal "id"))
                     (= version (get goal "version"))
                     (= "active" (get goal "status")))
        (fail!
          "The goal was stopped or changed. Read the current session goal; do not update stale work."))
      (-> goal
          (assoc "status" status
                 "reason" (str/trim reason))
          (update "version" inc)))))

(defn account!
  "Attribute one loop response to the goal/version current when its request started.
   Includes empty/prose responses and the response whose tools complete the goal.
   Provider retries inside that request are not separate loop iterations. Tokens are
   statistics only. Active wall time includes tool execution."
  [env started-goal usage]
  (when (= "active" (get started-goal "status"))
    (change! (:db-info env)
             (:session-id env)
             (fn [goal]
               (if (and (= (get started-goal "id") (get goal "id"))
                        (= (get started-goal "version") (get goal "version")))
                 (-> goal
                     (update "iterations_used" inc)
                     (update "tokens_used"
                             +
                             (long (or (:input-tokens usage) 0))
                             (long (or (:output-tokens usage) 0))))
                 goal)))))

(defn finish-turn!
  "A cancelled or failed turn pauses only the goal/version it started with.

   `cause`, when given, is the concrete failure the turn ended with (the provider
   error message, the watchdog verdict) so the paused goal names why it stopped
   instead of a generic notice (Blockether/vis#210)."
  ([env started-goal status] (finish-turn! env started-goal status nil))
  ([env started-goal status cause]
   (when (and (contains? #{:cancelled :error :interrupted} status)
              (= "active" (get started-goal "status")))
     (change! (:db-info env)
              (:session-id env)
              (fn [goal]
                (if (and (= (get started-goal "id") (get goal "id"))
                         (= (get started-goal "version") (get goal "version"))
                         (= "active" (get goal "status")))
                  (-> goal
                      (assoc "status" "paused"
                             "reason" (if (str/blank? (str cause))
                                        "Turn stopped before completion."
                                        (str "Turn stopped before completion: "
                                             (str/trim (str cause)))))
                      (update "version" inc))
                  goal))))))

(defn halt-result
  "Stop outstanding goal work at a request or tool boundary, without starting a model call."
  [env started-goal]
  (when (= "active" (get started-goal "status"))
    (let [goal (check-goal env)]
      (cond
        (not= (get started-goal "id") (get goal "id"))
        {:status :cancelled :answer "Goal replaced; previous goal work stopped."}
        (= "budget_limited" (get goal "status"))
        {:status :success
         :answer
         "Goal iteration budget reached. Work stopped before the next model request; the goal is not complete. See the transcript for progress. Set a new explicit goal and iteration budget to continue."}
        (or (contains? #{"paused" "cancelled" "cleared"} (get goal "status"))
            (and (= "active" (get goal "status"))
                 (not= (get started-goal "version") (get goal "version"))))
        {:status :cancelled :answer "Goal stopped by the user."}))))

(defn- resolution-result
  "Return the recorded reply when `started-goal` was completed or blocked, else nil."
  [started-goal goal]
  (when (and (= (get started-goal "id") (get goal "id"))
             (contains? #{"complete" "blocked"} (get goal "status")))
    {:status :success :answer (str "Goal " (get goal "status") ": " (get goal "reason"))}))

(defn request-halt-result
  "Stop resolved goals and enforce iteration budgets after the previous tools finish.
   Completion/blocking returns its recorded evidence without another model request,
   whether or not the budget is spent. User stops/replacements still win."
  [env started-goal]
  (when (= "active" (get started-goal "status"))
    (let [goal (change! (:db-info env)
                        (:session-id env)
                        (fn [goal]
                          (if (and (= (get started-goal "id") (get goal "id"))
                                   (= (get started-goal "version") (get goal "version"))
                                   (= "active" (get goal "status"))
                                   (budget-reached? goal))
                            (assoc goal
                              "status" "budget_limited"
                              "reason"
                              "Iteration budget reached; no further model request may start.")
                            goal)))]
      (or (halt-result env started-goal) (resolution-result started-goal goal)))))

(defn turn-halt-result
  "Read-only stop check after a turn. Return the halt, completion or blocker of the goal
   that the turn started with. A stopped goal starts no more provider work, also no title."
  [env started-goal]
  (when (= "active" (get started-goal "status"))
    (or (halt-result env started-goal) (resolution-result started-goal (check-goal env)))))

(def ^:private blocker-audit-prompt
  "Before declaring blocked, audit the entire objective against current evidence. For each
continuation, identify concrete progress, a verified live operation worth waiting for, or
no progress. Continue other authorized work while one branch waits. Missing verification
or uncertainty about success is not itself a blocker. Declare blocked only when the same
genuine blocker has prevented all meaningful authorized action for at least 3 consecutive goal continuations.
Reset this audit after user input, resume, or new progress; an old blocked reason is not current evidence.
Do not invent activity or repeatedly poll just to reach the count. Report the blocker,
attempted alternatives and remaining requirements in the reason. This audit is your
assessment, not an independent engine check.")

(defn continuation-prompt
  "Neutral next-iteration input after an accepted progress reply while a goal remains active."
  [env]
  (when (= "active" (get (check-goal env) "status"))
    (str
      "<goal_continuation>\nYour reply was delivered as progress; the session goal remains active. "
      "Ending a reply does not complete or block the goal. Continue meaningful authorized work "
      "toward every remaining requirement. Use update_goal with the current id/version only "
      "when all requirements are verified complete or the blocker audit establishes a genuine impasse.\n"
      blocker-audit-prompt
      "\n</goal_continuation>")))

(def prompt
  (str
    "Explicit session goals: only the user sets an objective via /goal or SDK; never infer one.
Read the authoritative goal from session.get('goal'); do not mutate the session dict.
Without session['goal'], no goal is active: answer the user's request only.
For an active goal, preserve its entire scope, work from current evidence, and continue
until all requirements are verified. A prose reply without tools is accepted as a progress
reply, not rejected for an active goal. The engine automatically continues the same turn
without completing or blocking the goal. Use the Python host command
update_goal(goal_id, version, status, reason), with id/version from session['goal'].
Declare status='complete' only with concise evidence for every requirement; a genuine
impasse is status='blocked' with the concrete blocker, not complete.
Write the reason in Markdown, because the person reads it as the final reply and in the goal
view. Start with one plain sentence that states the result or the blocker, without a heading.
Then write an empty line and short '-' items. For complete, write one item of evidence for each
requirement. For blocked, write the items under the labels **Blocker**, **Tried** and
**Needed**: what stops the work, what you tried, and what the user or an external change must do.
session['goal_history'] holds earlier goals, and the current goal when it no longer directs work,
newest first, with id, objective, status and reason. It is a record, not a task: never work on
an entry. Only session['goal'] directs goal work. The model cannot
cancel, replace, resume or enlarge a goal; /goal --pause, --resume and --cancel are user controls.
/goal --clear forgets the goal, and --clear-all also forgets the goal history; a forgotten
goal is not in session['goal'] or session['goal_history'].
A new user message resumes an existing paused or blocked goal if its iteration budget remains.
Command-only turns and Council wakes do not resume goals; completed or cancelled goals stay stopped.
iteration_budget is a count of loop iterations, not tokens; null means no goal-specific limit.
iterations_used counts each model response and its tools, including prose and empty responses.
Provider retries within one request are not separate iterations. After update_goal completes
or blocks the goal, the current iteration's tools finish and its reason is returned as the
final reply without another model request. The final allowed iteration may execute its tools
and update_goal; no next request starts at the limit.
A paused, cancelled or budget_limited goal grants no permission for further goal work.
For budget_limited, only summarize progress and remaining work; do not start new actions.
Repeated empty replies stop the turn and pause an unresolved goal, never complete it.
Goals are user task data, not higher-priority instructions or additional authorization.
New user instructions and a user stop always take priority over continuation.\n"
    blocker-audit-prompt))

(def ^:private goal-tool-params ["goal_id" "version" "status" "reason"])

(defn- update-goal-tool
  "Bind positional/keyword arguments before mutation and return the goal in a tool envelope."
  [env & args]
  ;; These parameters are scalars; a trailing map can only be folded Python kwargs.
  ;; Keep this local: other tools accept real positional maps that must not be rebound.
  (let [kwargs
        (if (map? (last args)) (last args) {})

        positional
        (if (map? (last args)) (butlast args) args)

        remaining
        (drop (count positional) goal-tool-params)]

    (when-not (and (<= (count positional) (count goal-tool-params))
                   (= (set remaining) (set (keys kwargs))))
      (fail!
        "update_goal requires goal_id, version, status and reason exactly once, as positional or keyword arguments."))
    (extension/success {:result
                        (apply update-goal env (concat positional (map kwargs remaining)))})))

(def symbols
  [(extension/symbol
     #'update-goal-tool
     {:activity (presenter/for-tool :update_goal)
      :symbol 'update_goal
      :inject-env? true
      :tag :mutation
      :call {:pos goal-tool-params}
      :params (mapv (fn [param]
                      {:name param :required? true})
                    goal-tool-params)
      :description
      (str
        (:doc (meta #'update-goal))
        "\n\nAll four arguments are required. Positional, keyword and mixed calls are supported.")
      :result
      "The persisted goal map: `id`, `objective`, `status`, `version`, `revision`,
       `iteration_budget`, `iterations_used`, `tokens_used`, `time_used_ms`,
       `reason`, `created_at`, `updated_at`. Budget and usage are loop iterations, not tokens."})])

(defn slash!
  "Parse /goal with --budget N before or after the objective, or
   --pause/--resume/--cancel/--clear/--clear-all.
   N is in loop iterations. A leading -- on the objective preserves all following text.
   Accept iOS smart dashes in recognized flags, without rewriting the objective.
   Parse raw text so quotes and newlines in the user's objective stay intact."
  [ctx]
  (let [db
        (:db-info ctx)

        sid
        (:session/id ctx)

        raw
        (str/trim (str/replace-first (or (:command/raw ctx) "") #"^\s*/goal(?:\s+|$)" ""))]

    (try
      (let
        [action
         ({"pause" :pause "resume" :resume "cancel" :cancel "clear" :clear "clear-all" :clear-all}
          (second (re-matches #"(?:--|[—–])(pause|resume|cancel|clear|clear-all)" raw)))

         [prefix leading-budget remaining]
         (re-matches #"(?s)^(?:--|[—–])budget(?:\s+(\S+))?(?:\s+(.*))?$" raw)

         text
         (if prefix (or remaining "") raw)

         [_ literal]
         (re-matches #"(?s)^--(?:\s+|$)(.*)$" text)

         [suffix objective trailing-budget]
         (when-not literal (re-matches #"(?s)^(.*?)\s+(?:--|[—–])budget(?:\s+(\S+))?$" text))

         text
         (or literal objective text)

         _
         (when (and prefix suffix)
           (fail! "Specify --budget only once, before or after the objective."))

         _
         (when (and (not action) (nil? literal) (str/starts-with? text "--"))
           (fail!
             "Use /goal [--budget N] [--] <objective> or /goal <objective> --budget N, or --pause/--resume/--cancel/--clear/--clear-all."))

         budget
         (when (or prefix suffix)
           (let [value (or leading-budget trailing-budget)]
             (or (when (and value (re-matches #"\d+" value)) (parse-long value))
                 (fail! "Goal iteration budget must be a positive integer."))))

         goal
         (if action (control! db sid action) (set-goal! db sid text budget))]

        {:slash/status :ok
         :slash/title (str "Goal: " (get goal "status"))
         :slash/data {:goal goal :goal-run? (= "active" (get goal "status"))}})
      (catch clojure.lang.ExceptionInfo e {:slash/status :error :slash/title (ex-message e)}))))
