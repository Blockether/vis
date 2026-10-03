(ns com.blockether.vis.internal.automation.host-test
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.internal.automation.core :as automation]
            [com.blockether.vis.internal.automation.host :as host]
            [com.blockether.vis.internal.automation.runner :as runner]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.persistance.core :as ps]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private input
  {"name" "Morning report"
   "triggers" [{"kind" "every" "seconds" 3600}]
   "prompt" "Summarize the open pull requests."
   "target" {"mode" "temporary"}})

(defn- with-env
  "Run `f` with the env of one session over a fresh store."
  [f]
  (let [db (ps/db-create-connection! :memory)]
    (try (f {:db-info db :session-id "s-1"}) (finally (ps/db-dispose-connection! db)))))

(defn- refusal
  [f]
  (try (f)
       nil
       (catch clojure.lang.ExceptionInfo e
         {:message (ex-message e) :status (:status (ex-data e))})))

(defn- row
  "The replayed Activity row of one finished call of `op`."
  [op details]
  (let [ctx
        (event/context)

        invocation
        (event/invocation ctx nil)

        start-details
        {:operation op
         :presenter :generic
         :activity (presenter/for-tool op)
         :started-at-ms (System/currentTimeMillis)
         :args []
         :classification :observation}

        projection
        (activity/presentation
          (activity/replay [(event/start-event ctx invocation start-details)
                            (event/terminal-event ctx invocation (merge start-details details))]))]

    (expect (contract/valid-projection? projection))
    (first (:rows projection))))

(defn- presentation [op value] (:presentation (row op {:outcome :succeeded :result value})))

(defdescribe
  tools-test
  (it "creates, reads, changes, lists and deletes an automation"
      (with-env
        (fn [env]
          (let [created
                (:result (host/create-automation env input))

                id
                (get created "id")

                read
                (:result (host/get-automation env id))

                paused
                (:result (host/update-automation env id {"enabled" false}))

                listed
                (:result (host/list-automations env))

                deleted
                (:result (host/delete-automation env id))]

            (expect (= "Morning report" (get read "name")))
            (expect (false? (get paused "enabled")))
            (expect (= [id] (mapv #(get % "id") (get listed "automations"))))
            (expect (false? (get listed "is_enabled")))
            (expect (= {"id" id "name" "Morning report" "is_deleted" true} deleted))
            (expect (= [] (get (:result (host/list-automations env)) "automations")))
            (expect (= {"webhook" false "callback" false} (get created "secrets")))
            (expect (not (str/includes? (pr-str [created paused listed]) "whsec_")))))))
  (it "names the field of an invalid definition and an unknown automation"
      (with-env
        (fn [env]
          (expect (=
                    {:message
                     "Automation is not valid at /triggers/0/seconds: 6 is less than the minimum 60"
                     :status 400}
                    (refusal #(host/create-automation
                                env
                                (assoc input "triggers" [{"kind" "every" "seconds" 6}])))))
          (expect (= 404 (:status (refusal #(host/delete-automation env "missing"))))))))
  (it "starts a run and passes the run filters"
      (with-env
        (fn [env]
          (let [calls (atom [])]
            (with-redefs [runner/run-now! (fn [_ id]
                                            (swap! calls conj [:run id])
                                            {"id" "r-1" "automation_id" id "status" "queued"})
                          automation/runs (fn [_ opts]
                                            (swap! calls conj [:runs opts])
                                            [])]

              (expect (= "queued" (get (:result (host/run-automation env "a-1")) "status")))
              (expect (= {"runs" []}
                         (:result (host/list-runs
                                    env
                                    {"automation_id" "a-1" "status" "failed" "limit" 999}))))
              (host/list-runs env)
              (expect (= [[:run "a-1"]
                          [:runs
                           {:automation-id "a-1" :statuses ["failed"] :session-id nil :limit 200}]
                          [:runs {:automation-id nil :statuses nil :session-id nil :limit 50}]]
                         @calls)))))))
  (it "refuses every change from a session that an automation run uses"
      (with-env
        (fn [env]
          (with-redefs [runner/automation-session? #(= "s-1" %)]
            (doseq [f [#(host/create-automation env input)
                       #(host/update-automation env "a-1" {"enabled" false})
                       #(host/delete-automation env "a-1") #(host/run-automation env "a-1")]]
              (expect (= {:message
                          "An automation run cannot create, change, run or delete automations"
                          :status 403}
                         (refusal f))))
            (expect (= [] (get (:result (host/list-automations env)) "automations")))))))
  (it "appears with its guidance only while the automations setting is on"
      (binding [toggles/*overrides* {"automations" false}]
        (expect (false? (host/enabled?)))
        (expect (nil? (host/prompt {}))))
      (binding [toggles/*overrides* {"automations" true}]
        (expect (true? (host/enabled?)))
        (expect (str/includes? (host/prompt {}) "automations.create(definition)")))))

(defdescribe
  activity-test
  (it "declares an end-only Activity for every tool"
      (expect (= [["automations.list" "List automations"] ["automations.get" "Read automation"]
                  ["automations.create" "Create automation"]
                  ["automations.update" "Update automation"]
                  ["automations.delete" "Delete automation"] ["automations.run" "Run automation"]
                  ["automations.runs" "List automation runs"]]
                 (mapv (fn [entry]
                         [(str (:ext.symbol/symbol entry))
                          (get-in entry [:ext.symbol/activity :headline])])
                       host/symbols)))
      (expect (every? #(false? (get-in % [:ext.symbol/activity :show-start])) host/symbols)))
  (describe
    "success"
    (it "lists automations and runs as tables"
        (expect (= {"headline" "Listed automations"
                    "summary" "2 automations · Automations are off"
                    "content" [{"type" "table"
                                "columns" ["Automation" "State"]
                                "rows" [["Morning report" "On"] ["Build check" "Paused"]]}]}
                   (presentation :automations.list
                                 {"automations" [{"name" "Morning report" "enabled" true}
                                                 {"name" "Build check" "enabled" false}]
                                  "is_enabled" false})))
        (expect (= {"headline" "Listed automation runs"
                    "summary" "1 run"
                    "content" [{"type" "table"
                                "columns" ["Automation" "Status"]
                                "rows" [["Morning report" "Completed"]]}]}
                   (presentation :automations.runs
                                 {"runs" [{"automation_name" "Morning report"
                                           "status" "completed"}]}))))
    (it "names the automation of a single result"
        (expect (= "Morning report · On"
                   (get (presentation :automations.create {"name" "Morning report" "enabled" true})
                        "summary")))
        (expect (= "Morning report · Paused"
                   (get (presentation :automations.update {"name" "Morning report" "enabled" false})
                        "summary")))
        (expect (= "Morning report · Queued"
                   (get (presentation :automations.run
                                      {"automation_name" "Morning report" "status" "queued"})
                        "summary")))
        (expect (= "Morning report"
                   (get (presentation :automations.delete
                                      {"id" "a-1" "name" "Morning report" "is_deleted" true})
                        "summary")))))
  (it "shows empty lists"
      (expect (= {"headline" "Listed automations" "summary" "No automations" "content" []}
                 (presentation :automations.list {"automations" [] "is_enabled" true})))
      (expect (= {"headline" "Listed automation runs" "summary" "No runs" "content" []}
                 (presentation :automations.runs {"runs" []}))))
  (it "keeps the error of a failed or cancelled call"
      (doseq [op
              [:automations.create :automations.list :automations.run]

              outcome
              [:failed :cancelled]]

        (let [row (row op {:outcome outcome :error (ex-info "Automation a-1 was not found" {})})]
          (expect (= (name outcome) (:state row)))
          (expect (= "Automation a-1 was not found" (:error-summary row)))))))
