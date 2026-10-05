(ns com.blockether.vis.internal.automation.host
  "Python host surface for automations. Results never hold a webhook or callback
   secret: a person creates secrets in the Companion app or the TUI."
  (:require [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.automation.core :as automation]
            [com.blockether.vis.internal.automation.runner :as runner]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.util :as util]))

(defn- ok [value] (extension/success {:result value}))

(defn- check-interactive!
  [env]
  (when (runner/automation-session? (:session-id env))
    (throw (ex-info "An automation run cannot create, change, run or delete automations"
                    {:status 403 :code :automation-run}))))

(defn list-automations
  "List the automations of this machine with their triggers, prompt, target, delivery, state,
   next run and last run."
  [env]
  (ok (runner/overview (:db-info env) (util/now-ms))))

(defn get-automation
  "Read one automation by its id."
  [env automation-id]
  (ok (automation/describe (:db-info env) (str automation-id) (util/now-ms))))

(defn create-automation
  "Create an automation from one definition dict. Required: `name`, `prompt`, `triggers` and
   `target`. Optional: `enabled` (default True), `delivery`, `model` and `deliver_only`.

   Triggers (1 to 8):
   - `{\"kind\": \"cron\", \"expression\": \"0 8 * * 1-5\", \"timezone\": \"Europe/Warsaw\"}`:
     five cron fields or a macro such as `@daily`. Without `timezone`, the gateway time zone.
   - `{\"kind\": \"every\", \"seconds\": 3600}`: 60 seconds or more.
   - `{\"kind\": \"once\", \"at\": <epoch milliseconds>}`.
   - `{\"kind\": \"webhook\", \"signature\": \"github\" | \"standard\" | \"generic\" | \"token\",
     \"events\": [...], \"filters\": [{\"field\": \"action\", \"equals\": \"opened\"}]}`: a
     signed POST to `/v1/hooks/<id>`. The prompt can use `{dot.path}` and `{__raw__}` from the
     payload.

   Targets: `{\"mode\": \"session\", \"session_id\": ...}` queues a turn in that session.
   `{\"mode\": \"new\"}` keeps a new session for each run. `{\"mode\": \"temporary\"}` keeps
   only the answer. `new` and `temporary` accept `root`, and `new` accepts `group_id`.

   Delivery: `{\"push\": True, \"callback\": {\"url\": \"https://...\"}}`. An answer that starts
   with `[SILENT]` sends nothing. `deliver_only` sends the rendered prompt without a model.

   The person creates webhook and callback secrets in the Companion app or the TUI.
   This tool never returns a secret."
  [env definition]
  (check-interactive! env)
  (ok (automation/create! (:db-info env) definition (util/now-ms))))

(defn update-automation
  "Replace the given top-level fields of an automation, for example `{\"enabled\": False}` to
   pause it. `triggers`, `target` and `delivery` are replaced as a whole."
  [env automation-id changes]
  (check-interactive! env)
  (ok (automation/update! (:db-info env) (str automation-id) changes (util/now-ms))))

(defn delete-automation
  "Delete an automation and its run history. Ask the person first."
  [env automation-id]
  (check-interactive! env)
  (let [db
        (:db-info env)

        id
        (str automation-id)

        automation-name
        (get (automation/describe db id (util/now-ms)) "name")]

    (ok (assoc (automation/delete! db id) "name" automation-name))))

(defn run-automation
  "Start one run now, outside its triggers. The run waits in the queue. Follow it with
   `automations.runs(automation_id=...)`."
  [env automation-id]
  (check-interactive! env)
  (ok (runner/run-now! (:db-info env) (str automation-id))))

(defn list-runs
  "List recent runs, newest first. Optional keywords: `automation_id`, `status` (queued, running,
   completed, failed, cancelled, skipped or unknown), `session_id` and `limit` (1 to 200, default
   50)."
  ([env] (list-runs env {}))
  ([env opts] (ok {"runs" (automation/runs (:db-info env) (automation/run-filter opts))})))

(def ^:private automation-result
  "`{id, name, enabled, triggers, prompt, target, delivery, model, deliver_only, created_at,
   updated_at, next_run_at, webhook, secrets, last_run}`. `secrets` tells only if a secret exists.
   Times are epoch milliseconds.")

(def ^:private run-result
  "A run: `{id, automation_id, automation_name, trigger, status, reason, scheduled_at, created_at,
   started_at, finished_at, session_id, turn_id, answer, error, is_silent}`.")

(def symbols
  (mapv (fn [[v sym tag params result]]
          (extension/symbol v
                            (cond-> {:activity (presenter/for-tool (keyword (str sym)))
                                     :symbol sym
                                     :inject-env? true
                                     :tag tag
                                     :call (if (= 'automations.runs sym)
                                             {:pos [] :rest :always}
                                             {:pos (mapv :name params)})
                                     :description (:doc (meta v))
                                     :result result}
                              (seq params)
                              (assoc :params params))))
        [[#'list-automations 'automations.list :observation []
          (str "`{automations}`. Each automation: " automation-result)]
         [#'get-automation 'automations.get :observation [{:name "automation_id" :required? true}]
          automation-result]
         [#'create-automation 'automations.create :mutation [{:name "definition" :required? true}]
          automation-result]
         [#'update-automation 'automations.update :mutation
          [{:name "automation_id" :required? true} {:name "changes" :required? true}]
          automation-result]
         [#'delete-automation 'automations.delete :mutation
          [{:name "automation_id" :required? true}] "`{id, name, is_deleted}`."]
         [#'run-automation 'automations.run :mutation [{:name "automation_id" :required? true}]
          run-result]
         [#'list-runs 'automations.runs :observation
          [{:name "automation_id"} {:name "status"} {:name "session_id"} {:name "limit"}]
          (str "`{runs}`. " run-result)]]))

(defn prompt
  "Short guidance for creating automations."
  [_env]
  (str "## Automations\n"
       "- `automations.create(definition)` runs a prompt on a schedule or a signed webhook. "
       "Read `doc(\"automations.create\")` before the first call.\n"
       "- The person creates webhook and callback secrets in the Companion app or the TUI."))
