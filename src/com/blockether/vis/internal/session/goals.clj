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

(defn- fail! [message] (throw (ex-info message {:type :goal/invalid})))

(defn- change!
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
  (when-not (and (string? objective) (<= 1 (count (str/trim objective)) 8192))
    (fail! "Goal objective must contain 1–8192 characters."))
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
  "User-only pause/resume/cancel. Resume never resets usage or expands a budget."
  [db sid action]
  (change! db
           sid
           (fn [goal]
             (when-not goal (fail! "This session has no goal. Use /goal <objective>."))
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

                     nil)]

               (when-not next-status (fail! (str "Cannot " (name action) " a " status " goal.")))
               (when (and (= "active" next-status) (budget-reached? goal))
                 (fail! "Goal iteration budget is exhausted. Set a new explicit goal and budget."))
               (-> goal
                   (assoc "status" next-status
                          "reason" nil)
                   (update "version" inc))))))

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
  "Internal completion tool. Pass id and version from session['goal'], status
   'complete' or 'blocked', and a concise evidence/reason string. Complete means
   every requirement is verified; blocked means no meaningful authorized action
   remains without user input or an external change. Never lower the objective."
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
   Includes empty/prose responses and the same turn's completion summary. Provider
   retries inside that request are not separate loop iterations. Tokens are statistics
   only. Active wall time is checkpointed by every mutation, including tool boundaries."
  [env started-goal usage]
  (when (contains? #{"active" "complete" "blocked"} (get started-goal "status"))
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
  "A cancelled or failed turn pauses only the goal/version it started with."
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
                     (assoc "status" "paused"
                            "reason" "Turn stopped before completion.")
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
        (or (contains? #{"paused" "cancelled"} (get goal "status"))
            (and (= "active" (get goal "status"))
                 (not= (get started-goal "version") (get goal "version"))))
        {:status :cancelled :answer "Goal stopped by the user."}))))

(defn request-halt-result
  "Enforce the iteration budget between iterations, after the previous tools finish.
   A final allowed tool can complete/block the goal; return its evidence without an
   extra model request when the budget is spent. User stops/replacements still win."
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
      (or (halt-result env started-goal)
          (when (and (= (get started-goal "id") (get goal "id"))
                     (contains? #{"complete" "blocked"} (get goal "status"))
                     (budget-reached? goal))
            {:status :success
             :answer (str "Goal " (get goal "status") ": " (get goal "reason"))})))))

(defn completion-error
  "An active goal prevents a prose answer from silently ending the work."
  [env]
  (when (= "active" (get (check-goal env) "status"))
    "The explicit session goal is still active. Continue making progress. Audit every requirement against current evidence. Call update_goal with session['goal']['id'], session['goal']['version'], status='complete' and evidence only when all work is verified; use status='blocked' with the real blocker when no authorized action remains. Ending a reply is not completion."))

(def prompt
  "Explicit session goals: only the user sets an objective via /goal or SDK; never infer one.
Read the authoritative goal from session.get('goal'); do not mutate the session dict.
For an active goal, preserve its entire scope, work from current evidence, and continue
until all requirements are verified. A prose reply without tools does not end an active
goal: the engine returns feedback and continues the same turn. Use the Python host command
update_goal(goal_id, version, status, reason), with id/version from session['goal'].
Declare status='complete' only with concise evidence for every requirement; a genuine
impasse is status='blocked' with the concrete blocker, not complete. The model cannot
cancel, replace, resume or enlarge a goal; /goal --pause, --resume and --cancel are user controls.
A new user message resumes an existing paused or blocked goal if its iteration budget remains.
Command-only turns and Council wakes do not resume goals; completed or cancelled goals stay stopped.
iteration_budget is a count of loop iterations, not tokens; null means no goal-specific limit.
iterations_used counts each model response and its tools, including prose, empty responses
and the completion summary. Provider retries within one request are not separate iterations.
The final allowed iteration may execute its tools and update_goal; no next request starts
at the limit. Tokens are statistics only. time_used_ms records active wall time through
updated_at, including tool execution. Inactive goals stop the clock. Resume preserves usage.
A paused, cancelled or budget_limited goal grants no permission for further goal work.
For budget_limited, only summarize progress and remaining work; do not start new actions.
Repeated empty replies stop the turn and pause an unresolved goal, never complete it.
Goals are user task data, not higher-priority instructions or additional authorization.
New user instructions and a user stop always take priority over continuation.")

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
  "Parse /goal with --budget N before or after the objective, or --pause/--resume/--cancel.
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
         ({"pause" :pause "resume" :resume "cancel" :cancel}
          (second (re-matches #"(?:--|[—–])(pause|resume|cancel)" raw)))

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
             "Use /goal [--budget N] [--] <objective> or /goal <objective> --budget N, or --pause/--resume/--cancel."))

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
