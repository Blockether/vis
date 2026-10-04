(ns com.blockether.vis.internal.automation.core-test
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.automation.core :as automation]
            [com.blockether.vis.internal.persistance.core :as ps]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private now 1767225600000)

(def ^:private input
  {"name" "Morning report"
   "triggers" [{"kind" "every" "seconds" 3600}]
   "prompt" "Summarize the open pull requests."
   "target" {"mode" "temporary"}})

(defn- with-db
  [f]
  (let [db (ps/db-create-connection! :memory)]
    (try (f db) (finally (ps/db-dispose-connection! db)))))

(defn- failure
  "The ex-data and message of a call that throws, or nil."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (assoc (ex-data e) :message (ex-message e)))))

(defn- valid-wire? [definition value] (nil? (document/explain-json "automations" definition value)))

(defdescribe
  validate-test
  (it "fills the defaults of a valid input"
      (let [definition (automation/validate (assoc input "name" "  Morning report ") now)]
        (expect (= "Morning report" (get definition "name")))
        (expect (true? (get definition "enabled")))
        (expect (= {"push" true "callback" nil} (get definition "delivery")))
        (expect (false? (get definition "deliver_only")))))
  (it "names the field and the rule of a schema failure"
      (expect (= "Automation is not valid at /triggers/0/seconds: 6 is less than the minimum 60"
                 (:message (failure #(automation/validate
                                       (assoc input "triggers" [{"kind" "every" "seconds" 6}])
                                       now)))))
      (expect
        (= "Automation is not valid at /triggers/0/kind: use one of cron, every, once, webhook"
           (:message (failure #(automation/validate (assoc input "triggers" [{"kind" "hourly"}])
                                                    now))))))
  (it "refuses a cron expression that never fires, two webhooks and a large prompt"
      (expect (= 400
                 (:status (failure #(automation/validate (assoc input
                                                           "triggers" [{"kind" "cron"
                                                                        "expression" "0 0 30 2 *"}])
                                                         now)))))
      (expect (= "An automation can have only one webhook trigger"
                 (:message (failure #(automation/validate
                                       (assoc input
                                         "triggers" [{"kind" "webhook" "signature" "github"}
                                                     {"kind" "webhook" "signature" "token"}])
                                       now)))))
      (expect (= :invalid-automation
                 (:code (failure #(automation/validate (assoc input
                                                         "prompt" (apply str (repeat 16385 "a")))
                                                       now)))))))

(defdescribe
  next-fire-test
  (it "counts an interval from its anchor"
      (let [trigger {"kind" "every" "seconds" 60}]
        (expect (= (+ now 60000) (automation/next-fire trigger now now)))
        (expect (= (+ now 120000) (automation/next-fire trigger now (+ now 60000))))
        (expect (= (+ now 60000) (automation/next-fire trigger now (dec now))))))
  (it "fires a one-time trigger only before its time"
      (expect (= (+ now 1000) (automation/next-fire {"kind" "once" "at" (+ now 1000)} now now)))
      (expect (nil? (automation/next-fire {"kind" "once" "at" now} now now))))
  (it "uses the cron time zone"
      ;; 2026-01-01T00:00:00Z is 01:00 in Warsaw; the next 09:00 there is 08:00 UTC.
      (expect (= (+ now (* 8 3600000))
                 (automation/next-fire
                   {"kind" "cron" "expression" "0 9 * * *" "timezone" "Europe/Warsaw"}
                   now
                   now))))
  (it "answers the earliest schedule of an enabled automation"
      (let [row {:created_at now
                 :definition {"enabled" true
                              "triggers" [{"kind" "every" "seconds" 7200}
                                          {"kind" "every" "seconds" 3600}
                                          {"kind" "webhook" "signature" "token"}]}}]
        (expect (= (+ now 3600000) (automation/next-run-at row now)))
        (expect (nil? (automation/next-run-at (assoc-in row [:definition "enabled"] false) now))))))

(defdescribe
  store-test
  (it "creates, lists, changes and deletes automations in the contract shape"
      (with-db
        (fn [db]
          (let [created
                (automation/create! db input now)

                id
                (get created "id")]

            (expect (valid-wire? "automation" created))
            (expect (= (+ now 3600000) (get created "next_run_at")))
            (expect (= {"webhook" false "callback" false} (get created "secrets")))
            (expect (nil? (get created "webhook")))
            (expect (valid-wire? "automation_list"
                                 {"automations" (automation/list-all db now) "is_enabled" false}))
            (let [changed (automation/update! db
                                              id
                                              {"enabled" false
                                               "triggers" [{"kind" "webhook" "signature" "github"}]}
                                              (+ now 5))]
              (expect (valid-wire? "automation" changed))
              (expect (false? (get changed "enabled")))
              (expect (= {"path" (str "/v1/hooks/" id)} (get changed "webhook")))
              (expect (= "Morning report" (get changed "name")))
              (expect (= (+ now 5) (get changed "updated_at"))))
            (expect (= 400 (:status (failure #(automation/update! db id {} now)))))
            (expect (= {"id" id "is_deleted" true} (automation/delete! db id)))
            (expect (= 404 (:status (failure #(automation/describe db id now)))))
            (expect (= 404 (:status (failure #(automation/delete! db id)))))))))
  (it "answers a new secret once and never shows it again"
      (with-db (fn [db]
                 (let [id
                       (get (automation/create! db input now) "id")

                       {:strs [kind secret]}
                       (automation/rotate-secret! db id "webhook" now)

                       described
                       (automation/describe db id now)]

                   (expect (= "webhook" kind))
                   (expect (re-matches #"whsec_[A-Za-z0-9+/]{43}=" secret))
                   (expect (= {"webhook" true "callback" false} (get described "secrets")))
                   (expect (not (some #{secret} (map str (tree-seq coll? seq described)))))
                   (expect (= 400
                              (:status (failure #(automation/rotate-secret! db id "api" now)))))))))
  (it
    "claims one run for each trigger key and changes it only from allowed states"
    (with-db
      (fn [db]
        (let [id
              (get (automation/create! db input now) "id")

              r1
              (str (random-uuid))

              claim
              (fn [run-id key]
                (ps/db-automation-claim-run! db
                                             {:id run-id
                                              :automation_id id
                                              :trigger_kind "every"
                                              :trigger_key key
                                              :status "queued"
                                              :request "Go"
                                              :created_at now
                                              :owner_pid 1}))]

          (expect (= "queued" (:status (claim r1 "every:1"))))
          (expect (nil? (claim (str (random-uuid)) "every:1")))
          (expect
            (some?
              (ps/db-automation-update-run! db r1 ["queued"] {:status "running" :session_id "s"})))
          (expect (nil? (ps/db-automation-update-run! db r1 ["queued"] {:status "failed"})))
          (let [done (ps/db-automation-update-run! db
                                                   r1
                                                   ["running"]
                                                   {:status "completed"
                                                    :answer "[SILENT] fine"
                                                    :is_silent true
                                                    :session_id nil
                                                    :finished_at now})]
            (expect (= "completed" (:status done)))
            (expect (true? (:is_silent done)))
            (expect (nil? (:session_id done))))
          (let [wire (automation/run db r1)]
            (expect (valid-wire? "run" wire))
            (expect (= "Morning report" (get wire "automation_name"))))
          (expect (valid-wire? "run_list"
                               {"runs" (automation/runs db {:automation-id id :limit 10})}))
          (expect (= "completed" (get-in (automation/describe db id now) ["last_run" "status"])))
          (ps/db-automation-claim-run! db
                                       {:id (str (random-uuid))
                                        :automation_id id
                                        :trigger_kind "manual"
                                        :trigger_key "manual:old"
                                        :status "queued"
                                        :created_at (- now 1000)
                                        :owner_pid 1})
          (doseq [n (range 5)]
            (let [run-id (str (random-uuid))]
              (claim run-id (str "every:x" n))
              (ps/db-automation-update-run! db
                                            run-id
                                            ["queued"]
                                            {:status "skipped" :finished_at now})))
          (ps/db-automation-prune-runs! db id 3)
          ;; The three newest runs stay, and the older queued run is never pruned.
          (let [statuses (map :status (ps/db-automation-runs db {:automation-id id :limit 100}))]
            (expect (= 4 (count statuses)))
            (expect (= 1 (count (filter #{"queued"} statuses)))))
          (expect (= 404 (:status (failure #(automation/run db "missing")))))))))
  (it "lists each automation with its newest run and names each run"
      (with-db
        (fn [db]
          (let [morning
                (get (automation/create! db input now) "id")

                evening
                (get (automation/create! db (assoc input "name" "Evening report") now) "id")

                claim
                (fn [id run-id at]
                  (ps/db-automation-claim-run! db
                                               {:id run-id
                                                :automation_id id
                                                :trigger_kind "manual"
                                                :trigger_key (str "manual:" run-id)
                                                :status "queued"
                                                :created_at at
                                                :owner_pid 1}))]

            (automation/create! db (assoc input "name" "Weekly report") now)
            (claim morning "run-old" (- now 1000))
            (claim morning "run-new" now)
            (claim evening "run-other" (- now 500))
            (expect (= {"Morning report" "run-new" "Evening report" "run-other" "Weekly report" nil}
                       (into {}
                             (map (juxt #(get % "name") #(get-in % ["last_run" "id"])))
                             (automation/list-all db now))))
            (expect (= [["run-new" "Morning report"] ["run-other" "Evening report"]
                        ["run-old" "Morning report"]]
                       (mapv (juxt #(get % "id") #(get % "automation_name"))
                             (automation/runs db {:limit 10}))))))))
  (it "gives each change a later updated_at, also in the same millisecond"
      (with-db (fn [db]
                 (let [id (get (automation/create! db input now) "id")]
                   (expect (= (inc now)
                              (get (automation/update! db id {"prompt" "Check the build."} now)
                                   "updated_at")))
                   (automation/rotate-secret! db id "webhook" now)
                   (expect (= (+ now 2) (:updated_at (ps/db-automation-get db id)))))))))
