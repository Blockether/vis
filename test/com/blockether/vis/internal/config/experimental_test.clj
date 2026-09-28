(ns com.blockether.vis.internal.config.experimental-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.contract.toggle :as contract]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.improve :as improve-settings]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.council.core :as council]
            ;; Registers the `draft_backend` experimental toggle this test pins.
            [com.blockether.vis.internal.foundation.drafts]
            [com.blockether.vis.internal.gateway.server.settings :as settings-api]
            [com.blockether.vis.internal.gateway.state :as gateway]
            [com.blockether.vis.internal.gateway.agents-test :as agent-http]
            [com.blockether.vis.internal.gateway.improve-test :as improve-http]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.agents-test :as agent-fixture]
            [lazytest.core :refer [defdescribe expect it]]))

(wiring/install!)

(h/use-mem-store! {"subagents" false "improve" false "plans" false})

(defn- failure-data [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defdescribe
  experimental-settings-contract-and-hydration
  (it "experimental settings contract and hydration"
      (doseq [id ["subagents" "improve" "plans"]]
        (let [spec (toggles/toggle-spec id)]
          (expect (false? (:default spec)))
          (expect (false? (toggles/enabled? id)))
          (expect (true? (:experimental? spec)))
          (expect (true? (:persist? spec)))
          (expect (= :experimental (:group spec)))
          (expect (contract/contribution-valid? spec))
          (expect (not (contract/contribution-valid? (assoc spec :experimental? "yes"))))))
      (expect (not (some #(= "improve_mode" (:id %)) (toggles/visible-toggles))))
      (let [groups
            (get (json/read-json (:body (#'settings-api/list-settings-handler {}))) "groups")

            rows
            (get (first (filter #(= "experimental" (get % "id")) groups)) "toggles")]

        (expect (= #{"subagents" "improve" "plans" "draft_backend"} (set (map #(get % "id") rows))))
        (expect (every? #(true? (get % "is_experimental")) rows))
        (expect (every? #(false? (get % "enabled")) (filter #(= "boolean" (get % "type")) rows)))
        (expect (= "off" (get (first (filter #(= "draft_backend" (get % "id")) rows)) "value"))))
      (toggles/hydrate-from-config! {"toggles" {"subagents" true "improve" "on" "plans" true}})
      (expect (every? toggles/enabled? ["subagents" "improve" "plans"]))
      (expect (some #(= "improve_mode" (:id %)) (toggles/visible-toggles)))
      (expect (= {"subagents" true "improve" true "plans" true}
                 (select-keys (toggles/snapshot) ["subagents" "improve" "plans"])))))

(defdescribe disabled-subagents-refuse-host-and-gateway-before-bootstrap
             (it
               "disabled subagents refuse host and gateway before bootstrap"
               (let [db
                     (h/store)

                     sid
                     (str (h/store-session! db {:channel :api}))

                     bootstraps
                     (atom 0)]

                 (expect (= :feature-disabled
                            (:error (failure-data #(agents/operation! {} :spawn {:task "Work"})))))
                 (with-redefs [lp/db-info
                               (constantly db)

                               lp/env-for
                               (fn [_]
                                 (swap! bootstraps inc))]

                   (expect (= :feature-disabled
                              (:error (failure-data
                                        #(gateway/agents-operation! sid :spawn {"task" "Work"})))))
                   (expect (= [] (gateway/agents-operation! sid :list {})))
                   (expect (zero? @bootstraps)))
                 (expect (nil? (agents/prompt {})))
                 (expect (every? #(false? ((:ext.symbol/active-fn %) {})) agents/symbols))
                 (toggles/set-enabled! "subagents" true)
                 (expect (str/includes? (agents/prompt {}) "council.publish_spawn"))
                 (expect (every? #((:ext.symbol/active-fn %) {}) agents/symbols)))))

(defdescribe disabling-subagents-stops-wakes-and-new-iterations-without-erasing-teams
             (it "disabling subagents stops wakes and new iterations without erasing teams"
                 (let [db
                       (h/store)

                       leader
                       (str (h/store-session! db {:channel :api}))

                       child
                       (agent-fixture/child! db leader {})

                       env
                       {:db-info db :session-id child}]

                   (toggles/set-enabled! "subagents" true)
                   (expect (agents/wake-allowed? db leader child))
                   (expect (agents/claim-iteration! env))
                   (let [before (agents/info db child)]
                     (toggles/set-enabled! "subagents" false)
                     (expect (not (agents/wake-allowed? db leader child)))
                     (expect (not (agents/wake-allowed? db child leader)))
                     (expect (not (agents/claim-iteration! env)))
                     (expect (= before (agents/info db child)))
                     (expect (agents/claim-iteration! {:db-info db :session-id leader})))
                   (toggles/set-enabled! "subagents" true)
                   (expect (agents/claim-iteration! env)))))

(defdescribe
  improve-opt-in-gates-intake-and-operations-without-deleting-records
  (it
    "improve opt in gates intake and operations without deleting records"
    (let [db
          (h/store)

          publish!
          (fn [key]
            (ps/db-council-insert! db
                                   {:author_sid "source"
                                    :activation_id "test"
                                    :source "host"
                                    :kind "complain"
                                    :title "Improve report"
                                    :content "Diagnostic evidence"
                                    :created_at 10
                                    :idempotency_key key
                                    :fingerprint key
                                    :source_ref {}}
                                   []
                                   false))]

      (publish! "disabled")
      (expect (zero? (h/raw-count db :improve)))
      (expect (= 1 (h/raw-count db :council_entry)))
      (let [failure {:error {:message "Failed"}}]
        (expect (= failure
                   (council/record-failure! {:db-info db :session-id "source"}
                                            {:vis/tool-name "python_execution"}
                                            failure))))
      (toggles/set-enabled! "improve" true)
      (publish! "enabled")
      (expect (= 1 (h/raw-count db :improve_record)))
      (toggles/set-enabled! "improve" false)
      (with-redefs [lp/db-info
                    (constantly db)

                    config/load-config-raw
                    (constantly {})]

        (expect (= "off" (:mode (gateway/improve-operation! :settings {}))))
        (doseq [op [:list :get :create :update :review :save-settings]]
          (expect (= {:type :improve/disabled :status 409}
                     (failure-data #(gateway/improve-operation! op {}))))))
      (expect (= 1 (h/raw-count db :improve_record)))
      (toggles/set-enabled! "improve" true)
      (with-redefs [lp/db-info (constantly db)]
        (expect (= 1 (count (:records (gateway/improve-operation! :list {})))))))))

(defdescribe improve-off-on-round-trip-invalidates-in-flight-review
             (it "improve off on round trip invalidates in flight review"
                 (let [mode (toggles/value-of "improve_mode")]
                   (try (with-redefs [config/load-config-raw (constantly {})]
                          (toggles/set-value! "improve_mode" "automatic")
                          (expect (= "off" (:mode (improve-settings/settings))))
                          (toggles/set-enabled! "improve" true)
                          (let [snapshot (improve-settings/snapshot)]
                            (expect (improve-settings/current? snapshot))
                            (toggles/set-enabled! "improve" false)
                            (expect (not (improve-settings/current? snapshot)))
                            (toggles/set-enabled! "improve" true)
                            (expect (not (improve-settings/current? snapshot)))
                            (expect (= "automatic" (:mode (improve-settings/settings))))))
                        (finally (toggles/set-value! "improve_mode" mode))))))

(defdescribe
  disabled-features-refuse-http-and-do-not-advertise-automatic-intake
  (it "disabled features refuse http and do not advertise automatic intake"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))]

        (with-redefs [lp/db-info
                      (constantly db)

                      lp/env-for
                      (fn [_]
                        (expect false "Disabled features must not bootstrap sessions"))

                      config/load-config-raw
                      (constantly {})]

          (expect (= 409
                     (:status (#'agent-http/request
                               :post
                               (str "/v1/sessions/" leader "/agents")
                               (json/write-json-str {:task "Work"})))))
          (doseq [[method path body] [[:get "/v1/improve" nil]
                                      [:post "/v1/improve" (json/write-json-str {:title "Report"})]
                                      [:post "/v1/improve/review" nil]
                                      [:patch "/v1/improve/settings"
                                       (json/write-json-str {:mode "automatic"})]]]
            (expect (= 409 (:status (#'improve-http/request method path body {}))))))
        (expect (not (str/includes? (:description (#'transcript/python-execution-tool {}))
                                    "autocomplain")))
        (toggles/set-enabled! "improve" true)
        (expect (str/includes? (:description (#'transcript/python-execution-tool {}))
                               "autocomplain")))))
