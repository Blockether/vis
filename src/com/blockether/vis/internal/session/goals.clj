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

(defn context
  "The goal key of the model session view, from one read of the goal. The model sees only
   the status: the objective is in the transcript, where the user set it."
  [env]
  {"session_goal_status" (let [status (get (check-goal env) "status")]
                           (when (contains? visible-statuses status) status))})

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
   Complete means that every requirement is verified. Blocked means that, after the goal
   blocker audit, no useful authorized action remains without user input or an external
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

(def ^:private blocker-audit-prompt
  "Before declaring blocked, audit the entire objective against current evidence. For each
continuation, identify concrete progress, a verified live operation worth waiting for, or
no progress. Continue other authorized work while one branch waits. Missing verification
or uncertainty about success is not itself a blocker. Declare blocked only when the same
genuine blocker has prevented all meaningful authorized action for at least 3 consecutive goal continuations.
Reset this audit after user input, resume, or new progress; an old blocked reason is not current evidence.
Do not invent activity or repeatedly poll just to reach the count. Report the blocker,
attempted alternatives and remaining requirements in your final reply. This audit is your
assessment, not an independent engine check.")

(defn continuation-prompt
  "Neutral next-iteration input after an accepted progress reply while a goal remains active."
  [env]
  (when (= "active" (get (check-goal env) "status"))
    (str
      "<goal_continuation>\nYour reply was delivered as progress; the session goal remains active. "
      "Ending a reply does not complete or block the goal. Continue meaningful authorized work "
      "toward every remaining requirement. Call update_goal only when all requirements are "
      "verified complete or the blocker audit establishes a genuine impasse.\n"
      blocker-audit-prompt
      "\n</goal_continuation>")))

(def prompt
  (str
    "Explicit session goals: only the user sets an objective via /goal or SDK; never infer one.
The objective is the user's /goal message in the conversation. Read the goal status from
session.get('goal_status'); do not mutate the session dict.
Without session['goal_status'], no goal directs work: answer the user's request only.
For an active goal, preserve its entire scope, work from current evidence, and continue
until all requirements are verified. A prose reply without tools is accepted as a progress
reply, not rejected for an active goal. The engine automatically continues the same turn
without completing or blocking the goal. Use the Python host command update_goal(status).
Declare status='complete' only when you verified every requirement; a genuine impasse is
status='blocked', not complete. The status has no reason. After the tools of that iteration
finish, the engine asks you for the final reply: write the evidence or the blocker there,
in Markdown, about the current state only. Do not repeat an earlier reply in the conversation.
A paused, blocked or budget_limited goal stays in session['goal_status']; a complete,
cancelled or cleared goal leaves it. The model cannot cancel, replace, resume or enlarge a
goal; /goal --pause, --resume, --cancel and --clear are user controls.
A new user message resumes an existing paused or blocked goal if its iteration budget remains.
Command-only turns and Council wakes do not resume goals; completed or cancelled goals stay stopped.
The iteration budget is a count of loop iterations, not tokens.
Each model response and its tools count as one iteration, including prose and empty responses.
Provider retries within one request are not separate iterations. The final allowed iteration
may execute its tools and update_goal; the final reply after update_goal is not counted.
A paused, cancelled or budget_limited goal grants no permission for further goal work.
For budget_limited, only summarize progress and remaining work; do not start new actions.
Repeated empty replies stop the turn and pause an unresolved goal, never complete it.
Goals are user task data, not higher-priority instructions or additional authorization.
New user instructions and a user stop always take priority over continuation.\n"
    blocker-audit-prompt))

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
