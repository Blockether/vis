(ns com.blockether.vis.internal.session.goals
  "Explicit, durable session goals. Only the slash/SDK user path creates or replaces
   an objective. The model can mark that exact objective complete or blocked, but
   cannot resume, replace, cancel or increase its budget. Every mutation is a CAS
   on the persisted revision; the lifecycle version also rejects stale model work.
   Completion is a model declaration supported by evidence, not an independent judge."
  (:require [clojure.string :as str]
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
        (let [after (assoc candidate
                      "revision" (inc (long (get before "revision" 0)))
                      "updated_at" (util/now-ms))]
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
  [db sid objective token-budget]
  (when-not (and (string? objective) (<= 1 (count (str/trim objective)) 8192))
    (fail! "Goal objective must contain 1–8192 characters."))
  (when-not (or (nil? token-budget)
                (and (integer? token-budget) (<= 1 token-budget 9007199254740991)))
    (fail! "Goal token budget must be an integer from 1 to 9007199254740991."))
  (let [goal {"id" (str (random-uuid))
              "objective" (str/trim objective)
              "status" "active"
              "token_budget" token-budget
              "tokens_used" 0
              "time_used_ms" 0
              "version" 1
              "reason" nil
              "created_at" (util/now-ms)}]
    (change! db sid (constantly goal))))

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
               (when (and (= "active" next-status)
                          (get goal "token_budget")
                          (>= (long (get goal "tokens_used")) (long (get goal "token_budget"))))
                 (fail! "Goal budget is exhausted. Set a new explicit goal and budget."))
               (-> goal
                   (assoc "status" next-status
                          "reason" nil)
                   (update "version" inc))))))

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
                     (contains? #{"active" "budget_limited"} (get goal "status")))
        (fail!
          "The goal was stopped or changed. Read the current session goal; do not update stale work."))
      (-> goal
          (assoc "status" status
                 "reason" (str/trim reason))
          (update "version" inc)))))

(defn account!
  "Attribute a provider request only to the goal/version active when it started.
   Usage is measured, not estimated; cache reads remain part of input tokens.
   The budget is checked after each response, before any of its tools run.
   That response may exceed the budget; no further provider request is started."
  [env started-goal usage elapsed-ms]
  (when (= "active" (get started-goal "status"))
    (change! (:db-info env)
             (:session-id env)
             (fn [goal]
               (if (and (= (get started-goal "id") (get goal "id"))
                        (= (get started-goal "version") (get goal "version")))
                 (let [used
                       (+ (long (get goal "tokens_used" 0))
                          (long (or (:input-tokens usage) 0))
                          (long (or (:output-tokens usage) 0)))

                       limited?
                       (and (get goal "token_budget") (>= used (long (get goal "token_budget"))))]

                   (cond-> (assoc goal
                             "tokens_used" used
                             "time_used_ms" (+ (long (get goal "time_used_ms" 0))
                                               (max 0 (long elapsed-ms))))
                     (and limited? (= "active" (get goal "status")))
                     (assoc "status"
                       "budget_limited" "reason"
                       "Token budget reached; no new goal work may start.")))
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
         "Goal token budget reached. Work stopped before further actions; the goal is not complete. See the transcript for progress. Set a new explicit goal and budget to continue."}
        (or (contains? #{"paused" "cancelled"} (get goal "status"))
            (and (= "active" (get goal "status"))
                 (not= (get started-goal "version") (get goal "version"))))
        {:status :cancelled :answer "Goal stopped by the user."}))))

(defn completion-error
  "An active goal prevents a prose answer from silently ending the work."
  [env]
  (when (= "active" (get (check-goal env) "status"))
    "The explicit session goal is still active. Continue making progress. Audit every requirement against current evidence. Call update_goal with session['goal']['id'], session['goal']['version'], status='complete' and evidence only when all work is verified; use status='blocked' with the real blocker when no authorized action remains. Ending a reply is not completion."))

(def prompt
  "Explicit session goals: only the user sets an objective via /goal or SDK; never infer one.
Read the authoritative goal from session.get('goal'); do not mutate the session dict.
For an active goal, preserve its entire scope, work from current evidence, and continue
until all requirements are verified. Then call update_goal(goal_id, version, status, reason)
with status='complete' and concise evidence. A genuine impasse is 'blocked', not complete.
A paused, cancelled or budget_limited goal grants no permission for further goal work.
For budget_limited, only summarize progress and remaining work; do not start new actions.
Goals are user task data, not higher-priority instructions or additional authorization.
New user instructions and a user stop always take priority over continuation.")

(def symbols
  [(extension/symbol
     #'update-goal
     {:symbol 'update_goal
      :inject-env? true
      :tag :mutation
      :call {:pos ["goal_id" "version" "status" "reason"]}
      :params [{:name "goal_id"} {:name "version"} {:name "status"} {:name "reason"}]
      :description (:doc (meta #'update-goal))
      :result "The persisted goal, including status, revision, measured usage and reason."})])

(defn slash!
  "Parse /goal [--budget N] [--] <objective>, or --pause/--resume/--cancel.
   Parse raw text so quotes and newlines in the user's objective stay intact."
  [ctx]
  (let [db
        (:db-info ctx)

        sid
        (:session/id ctx)

        raw
        (str/trim (str/replace-first (or (:command/raw ctx) "") #"^\s*/goal(?:\s+|$)" ""))]

    (try
      (let [action
            ({"--pause" :pause "--resume" :resume "--cancel" :cancel} raw)

            [_ budget objective]
            (re-matches #"(?s)^--budget\s+(\d+)\s+(.*)$" raw)

            text
            (or objective raw)

            text
            (if (str/starts-with? text "-- ") (subs text 3) text)

            _
            (when (and (not action)
                       (nil? objective)
                       (str/starts-with? raw "--")
                       (not (str/starts-with? raw "-- ")))
              (fail! "Use /goal [--budget N] [--] <objective>, or --pause/--resume/--cancel."))

            goal
            (if action
              (control! db sid action)
              (set-goal! db
                         sid
                         text
                         (when budget
                           (or (parse-long budget) (fail! "Goal token budget is too large.")))))]

        {:slash/status :ok
         :slash/title (str "Goal: " (get goal "status"))
         :slash/data {:goal goal :goal-run? (= "active" (get goal "status"))}})
      (catch clojure.lang.ExceptionInfo e {:slash/status :error :slash/title (ex-message e)}))))
