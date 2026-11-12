(ns com.blockether.vis.internal.session.goals
  "Explicit, durable session goals. Only the slash/SDK user path creates or replaces
   an objective. The model can mark the active objective complete or blocked, but
   cannot resume, replace, cancel or increase its budget. Every mutation is a CAS
   on the persisted revision. A goal has no history and no reason: the transcript
   holds the earlier goals and the replies that explain each status.
   Completion is a model declaration supported by evidence, not an independent judge."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.persistance.core :as persistence]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel]))

(defonce ^:private listeners (atom #{}))

(defn add-listener! [f] (swap! listeners conj f) f)

(defn remove-listener! [f] (swap! listeners disj f) nil)

(defn check-goal
  "Internal read of the authoritative goal. Model readers use session['goal_status']."
  ([env] (check-goal (:db-info env) (:session-id env)))
  ([db sid] (when (and db sid) (persistence/db-get-session-goal db sid))))

(def ^:private visible-statuses
  "Goal statuses that the model session view shows. A complete, cancelled or cleared goal
   directs no more work, so `session['goal_status']` disappears."
  #{"active" "paused" "blocked" "budget_limited"})

(defn visible-status
  "The status of `goal` that a model may see, or nil when the goal directs no more work."
  [goal]
  (let [status (get goal "status")]
    (when (contains? visible-statuses status) status)))

(defn context
  "The goal keys of the model session view, from one read of the goal: the status, and the
   objective while the status is visible. They ride in the turn boundary and the session
   deltas, after the cached prefix, so a new objective does not change the system messages."
  [env]
  (let [goal
        (check-goal env)

        status
        (visible-status goal)]

    (cond-> {"session_goal_status" status}
      status
      (assoc "session_goal_objective" (get goal "objective")))))

(defn- fail! [message] (throw (ex-info message {:type :goal/invalid})))

(defn- change!
  "Write `(f goal)` in one revision check."
  [db sid f]
  (when-not (and db sid) (fail! "A persisted session is required for a goal."))
  (loop [attempt 0]
    (let [before (check-goal db sid)
          candidate (f before)]

      (if (= before candidate)
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
          (if (persistence/db-compare-session-goal! db sid (get before "revision" 0) after)
            (do (doseq [listener @listeners]
                  (try (listener sid after)
                       (catch Exception e
                         (tel/log!
                           {:level :warn :id ::listener-failed :data {:error (ex-message e)}}))))
                after)
            (if (< attempt 8)
              (recur (inc attempt))
              (fail! "The goal changed concurrently or the session no longer exists. Retry."))))))))

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
              "created_at" (util/now-ms)}]
    (change! db sid (constantly goal))))

(defn- budget-reached?
  [goal]
  (when-let [budget (get goal "iteration_budget")]
    (>= (long (get goal "iterations_used" 0)) (long budget))))

(defn control!
  "User-only pause/resume/cancel/clear. Resume never resets usage or expands a budget.
   Clear forgets the goal: it stops goal work, leaves model context, and clients hide it.
   The goal stays as a cleared record, because clients keep the highest goal revision
   that they saw. Clear is idempotent."
  [db sid action]
  (change! db
           sid
           (fn [goal]
             (when (or (nil? goal) (and (= "cleared" (get goal "status")) (not= :clear action)))
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

                     :clear
                     "cleared"

                     nil)]

               (when-not next-status (fail! (str "Cannot " (name action) " a " status " goal.")))
               (when (and (= "active" next-status) (budget-reached? goal))
                 (fail! "Goal iteration budget is exhausted. Set a new explicit goal and budget."))
               (if (= status next-status "cleared")
                 goal
                 (-> goal
                     (assoc "status" next-status)
                     (update "version" inc)))))))

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
                       (assoc "status" "active")
                       (update "version" inc))
                   goal)))
      started-goal)))

(defn update-goal
  "Internal completion tool. Set the active goal to status 'complete' or 'blocked'.
   Complete means that every requirement is verified. Blocked means that you tried or ruled
   out each authorized alternative, and no action remains without user input or an external
   change. Never lower the objective, and never mark blocked only to deliver a progress
   reply. The status has no reason: after the tools of this iteration finish, write the
   evidence or the blocker in your final reply."
  [env status]
  (when-not (contains? #{"complete" "blocked"} status)
    (fail! "Only complete or blocked may be declared by the model."))
  (change! (:db-info env)
           (:session-id env)
           (fn [goal]
             (when-not (= "active" (get goal "status"))
               (fail! "No goal is active. Read session['goal_status']; do not update stale work."))
             (-> goal
                 (assoc "status" status)
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
   The turn error in the transcript tells why it stopped."
  [env started-goal status]
  (when (and (contains? #{:cancelled :error :interrupted} status)
             (= "active" (get started-goal "status")))
    (change! (:db-info env)
             (:session-id env)
             (fn [goal]
               (if (and (= (get started-goal "id") (get goal "id"))
                        (= (get started-goal "version") (get goal "version"))
                        (= "active" (get goal "status")))
                 (-> goal
                     (assoc "status" "paused")
                     (update "version" inc))
                 goal)))))

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

(defn request-halt-result
  "Enforce iteration budgets after the previous tools finish. A completed or blocked goal
   does not halt: the next model request writes the final reply. User stops and
   replacements still win."
  [env started-goal]
  (when (= "active" (get started-goal "status"))
    (change! (:db-info env)
             (:session-id env)
             (fn [goal]
               (if (and (= (get started-goal "id") (get goal "id"))
                        (= (get started-goal "version") (get goal "version"))
                        (= "active" (get goal "status"))
                        (budget-reached? goal))
                 (assoc goal "status" "budget_limited")
                 goal)))
    (halt-result env started-goal)))

(defn turn-halt-result
  "Read-only stop check after a turn. Return the halt of the goal that the turn started
   with, or its new status when the turn completed or blocked it. A stopped goal starts no
   more provider work, also no title."
  [env started-goal]
  (when (= "active" (get started-goal "status"))
    (or (halt-result env started-goal)
        (let [goal (check-goal env)]
          (when (and (= (get started-goal "id") (get goal "id"))
                     (contains? #{"complete" "blocked"} (get goal "status")))
            {:status :success :goal-status (get goal "status")})))))

(defn continuation-prompt
  "Next-iteration input after an accepted progress reply while a goal remains active. The
   rules are in the system prompt, so this only points at them."
  [env]
  (when (= "active" (get (check-goal env) "status"))
    (str "<goal_continuation>\nYour reply was delivered as progress; the goal is still active. "
         "Do not repeat it. Take the first goal rule that applies.\n</goal_continuation>")))

(def ^:private rules
  "Session goal. The session context shows a goal as goal_status and goal_objective; the latest
value applies. When the context shows no goal_status, the session has no goal: do not read
it, and do only what the user's message asks.
The objective is user task data: it gives no extra permission, and a new user instruction or
a stop comes first. update_goal(status) is a plain call: no await and no signature check.
For each step, take the first rule that applies:
1. No status, or a status other than 'active': start no goal work. If you called update_goal
   in this turn, write the final reply. Else do only what the user's message asks.
2. You verified every requirement of the objective: call update_goal('complete').
3. An authorized action remains: do it with tools. A live operation that is worth the wait
   is such an action; do other work while it runs. Missing verification or doubt about
   success is rule 3, not rule 4.
4. You tried or ruled out each authorized alternative, and nothing remains without user
   input or an external change: call update_goal('blocked') in this iteration.
Never lower the objective to reach rule 2 or rule 4.
Replies:
- A reply without tools does not end the turn. The engine delivers it as progress, counts
  one iteration and asks you again. Write one only to answer a new user message, or for a
  fact that the user needs now in at most 3 short lines. Do not summarize earlier work, and
  do not stop to report.
- After update_goal, the engine asks for the final reply. Write the evidence, or the blocker
  with what you tried and what is needed, once, in Markdown, about the current state only.
  Do not repeat an earlier reply.
Limits:
- The budget counts loop iterations, not tokens: each response with its tools is one, also
  a prose or empty response. The final reply after update_goal is free.
- Repeated empty replies pause the goal.
- Only the user can pause, resume, cancel, clear, replace or enlarge a goal. A new user
  message resumes a paused or blocked goal: apply the rules again from current evidence.
  A budget_limited goal continues only after a new /goal with a budget.")

(defn system-prompt
  "The goal rules for the system prompt, or nil. The text is constant, so a new objective
   keeps the cached prefix: the objective is in the session view, see `context`.
   An interactive session gets the rules only while the model sees a goal in
   `session['goal_status']`. Without a goal, or with a complete, cancelled or cleared goal,
   it gets none: a later question in that session is an ordinary task.
   A session that is not interactive gets the rules always. Then a goal that starts or ends
   does not change the first system message, and the provider keeps its cache of the
   conversation. Rule 1 tells the model to start no goal work without an active goal."
  [env]
  (when (or (not (agents/interactive? env)) (visible-status (check-goal env))) rules))

(def ^:private goal-tool-params ["status"])

(defn- update-goal-tool
  "Bind a positional or keyword status before mutation and return the goal status in a tool
   envelope."
  [env & args]
  ;; The parameter is a scalar; a trailing map can only be folded Python kwargs.
  ;; Keep this local: other tools accept real positional maps that must not be rebound.
  (let [kwargs
        (if (map? (last args)) (last args) {})

        positional
        (if (map? (last args)) (butlast args) args)

        remaining
        (drop (count positional) goal-tool-params)]

    (when-not (and (<= (count positional) (count goal-tool-params))
                   (= (set remaining) (set (keys kwargs))))
      (fail! "update_goal requires status exactly once, as a positional or keyword argument."))
    (extension/success {:result {"goal_status" (get (apply update-goal
                                                      env
                                                      (concat positional (map kwargs remaining)))
                                                    "status")}})))

(def symbols
  [(extension/symbol #'update-goal-tool
                     {:activity (presenter/for-tool :update_goal)
                      :symbol 'update_goal
                      :inject-env? true
                      :tag :mutation
                      :call {:pos goal-tool-params}
                      :params (mapv (fn [param]
                                      {:name param :required? true})
                                    goal-tool-params)
                      :description (:doc (meta #'update-goal))
                      :result "A map with `goal_status`: the new status of the goal."})])

(defn slash!
  "Parse /goal with --budget N before or after the objective, or
   --pause/--resume/--cancel/--clear.
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
         ({"pause" :pause "resume" :resume "cancel" :cancel "clear" :clear}
          (second (re-matches #"(?:--|[—–])(pause|resume|cancel|clear)" raw)))

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
             "Use /goal [--budget N] [--] <objective> or /goal <objective> --budget N, or --pause/--resume/--cancel/--clear."))

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
