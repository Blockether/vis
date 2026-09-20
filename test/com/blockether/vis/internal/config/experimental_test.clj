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
            [com.blockether.vis.internal.gateway.server :as server]
            [com.blockether.vis.internal.gateway.state :as gateway]
            [com.blockether.vis.internal.gateway.agents-test :as agent-http]
            [com.blockether.vis.internal.gateway.improve-test :as improve-http]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.agents-test :as agent-fixture]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(h/use-mem-store! {"subagents" false "improve" false "plans" false})

(defn- failure-data [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest experimental-settings-contract-and-hydration
  (doseq [id ["subagents" "improve" "plans"]]
    (let [spec (toggles/toggle-spec id)]
      (is (false? (:default spec)))
      (is (false? (toggles/enabled? id)))
      (is (true? (:experimental? spec)))
      (is (true? (:persist? spec)))
      (is (= :experimental (:group spec)))
      (is (contract/contribution-valid? spec))
      (is (not (contract/contribution-valid? (assoc spec :experimental? "yes"))))))
  (is (not (some #(= "improve_mode" (:id %)) (toggles/visible-toggles))))
  (let [groups
        (get (json/read-json (:body (#'server/list-settings-handler {}))) "groups")

        rows
        (get (first (filter #(= "experimental" (get % "id")) groups)) "toggles")]

    (is (= #{"subagents" "improve" "plans" "draft_backend"} (set (map #(get % "id") rows))))
    (is (every? #(true? (get % "is_experimental")) rows))
    (is (every? #(false? (get % "enabled")) (filter #(= "boolean" (get % "type")) rows)))
    (is (= "off" (get (first (filter #(= "draft_backend" (get % "id")) rows)) "value"))))
  (toggles/hydrate-from-config! {"toggles" {"subagents" true "improve" "on" "plans" true}})
  (is (every? toggles/enabled? ["subagents" "improve" "plans"]))
  (is (some #(= "improve_mode" (:id %)) (toggles/visible-toggles)))
  (is (= {"subagents" true "improve" true "plans" true}
         (select-keys (toggles/snapshot) ["subagents" "improve" "plans"]))))

(deftest disabled-subagents-refuse-host-and-gateway-before-bootstrap
  (let [db
        (h/store)

        sid
        (str (h/store-session! db {:channel :api}))

        bootstraps
        (atom 0)]

    (is (= :feature-disabled (:error (failure-data #(agents/operation! {} :spawn {:task "Work"})))))
    (with-redefs [lp/db-info
                  (constantly db)

                  lp/env-for
                  (fn [_]
                    (swap! bootstraps inc))]

      (is (= :feature-disabled
             (:error (failure-data #(gateway/agents-operation! sid :spawn {"task" "Work"})))))
      (is (= [] (gateway/agents-operation! sid :list {})))
      (is (zero? @bootstraps)))
    (is (nil? (agents/prompt {})))
    (is (every? #(false? ((:ext.symbol/active-fn %) {})) agents/symbols))
    (toggles/set-enabled! "subagents" true)
    (is (str/includes? (agents/prompt {}) "council.publish_spawn"))
    (is (every? #((:ext.symbol/active-fn %) {}) agents/symbols))))

(deftest disabling-subagents-stops-wakes-and-new-iterations-without-erasing-teams
  (let [db
        (h/store)

        leader
        (str (h/store-session! db {:channel :api}))

        child
        (agent-fixture/child! db leader {})

        env
        {:db-info db :session-id child}]

    (toggles/set-enabled! "subagents" true)
    (is (agents/wake-allowed? db leader child))
    (is (agents/claim-iteration! env))
    (let [before (agents/info db child)]
      (toggles/set-enabled! "subagents" false)
      (is (not (agents/wake-allowed? db leader child)))
      (is (not (agents/wake-allowed? db child leader)))
      (is (not (agents/claim-iteration! env)))
      (is (= before (agents/info db child)))
      (is (agents/claim-iteration! {:db-info db :session-id leader})))
    (toggles/set-enabled! "subagents" true)
    (is (agents/claim-iteration! env))))

(deftest improve-opt-in-gates-intake-and-operations-without-deleting-records
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
    (is (zero? (h/raw-count db :improve)))
    (is (= 1 (h/raw-count db :council_entry)))
    (let [failure {:error {:message "Failed"}}]
      (is (= failure
             (council/record-failure! {:db-info db :session-id "source"}
                                      {:vis/tool-name "python_execution"}
                                      failure))))
    (toggles/set-enabled! "improve" true)
    (publish! "enabled")
    (is (= 1 (h/raw-count db :improve_record)))
    (toggles/set-enabled! "improve" false)
    (with-redefs [lp/db-info
                  (constantly db)

                  config/load-config-raw
                  (constantly {})]

      (is (= "off" (:mode (gateway/improve-operation! :settings {}))))
      (doseq [op [:list :get :create :update :review :save-settings]]
        (is (= {:type :improve/disabled :status 409}
               (failure-data #(gateway/improve-operation! op {}))))))
    (is (= 1 (h/raw-count db :improve_record)))
    (toggles/set-enabled! "improve" true)
    (with-redefs [lp/db-info (constantly db)]
      (is (= 1 (count (:records (gateway/improve-operation! :list {}))))))))

(deftest improve-off-on-round-trip-invalidates-in-flight-review
  (let [mode (toggles/value-of "improve_mode")]
    (try (with-redefs [config/load-config-raw (constantly {})]
           (toggles/set-value! "improve_mode" "automatic")
           (is (= "off" (:mode (improve-settings/settings))))
           (toggles/set-enabled! "improve" true)
           (let [snapshot (improve-settings/snapshot)]
             (is (improve-settings/current? snapshot))
             (toggles/set-enabled! "improve" false)
             (is (not (improve-settings/current? snapshot)))
             (toggles/set-enabled! "improve" true)
             (is (not (improve-settings/current? snapshot)))
             (is (= "automatic" (:mode (improve-settings/settings))))))
         (finally (toggles/set-value! "improve_mode" mode)))))

(deftest disabled-features-refuse-http-and-do-not-advertise-automatic-intake
  (let [db
        (h/store)

        leader
        (str (h/store-session! db {:channel :api}))]

    (with-redefs [lp/db-info
                  (constantly db)

                  lp/env-for
                  (fn [_]
                    (is false "Disabled features must not bootstrap sessions"))

                  config/load-config-raw
                  (constantly {})]

      (is (= 409
             (:status (#'agent-http/request
                       :post
                       (str "/v1/sessions/" leader "/agents")
                       (json/write-json-str {:task "Work"})))))
      (doseq [[method path body] [[:get "/v1/improve" nil]
                                  [:post "/v1/improve" (json/write-json-str {:title "Report"})]
                                  [:post "/v1/improve/review" nil]
                                  [:patch "/v1/improve/settings"
                                   (json/write-json-str {:mode "automatic"})]]]
        (is (= 409 (:status (#'improve-http/request method path body {}))))))
    (is (not (str/includes? (:description (#'lp/python-execution-tool {})) "autocomplain")))
    (toggles/set-enabled! "improve" true)
    (is (str/includes? (:description (#'lp/python-execution-tool {})) "autocomplain"))))
