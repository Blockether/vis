(ns com.blockether.vis.internal.config.experimental-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.contract.toggle :as contract]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.toggles :as toggles]
            ;; Registers the `draft_backend` experimental toggle this test pins.
            [com.blockether.vis.internal.foundation.drafts]
            [com.blockether.vis.internal.gateway.server.settings :as settings-api]
            [com.blockether.vis.internal.gateway.state :as gateway]
            [com.blockether.vis.internal.gateway.agents-test :as agent-http]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.agents-test :as agent-fixture]
            [lazytest.core :refer [defdescribe expect it]]))

(wiring/install!)

(h/use-mem-store! {"subagents" false})

(defn- failure-data [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defdescribe
  experimental-settings-contract-and-hydration
  (it "experimental settings contract and hydration"
      (doseq [id ["subagents"]]
        (let [spec (toggles/toggle-spec id)]
          (expect (false? (:default spec)))
          (expect (false? (toggles/enabled? id)))
          (expect (true? (:experimental? spec)))
          (expect (true? (:persist? spec)))
          (expect (= :experimental (:group spec)))
          (expect (contract/contribution-valid? spec))
          (expect (not (contract/contribution-valid? (assoc spec :experimental? "yes"))))))
      (let [groups
            (get (json/read-json (:body (#'settings-api/list-settings-handler {}))) "groups")

            general
            (get (first (filter #(= "general" (get % "id")) groups)) "toggles")

            rows
            (filter #(get % "is_experimental") general)]

        (expect (= #{"subagents" "draft_backend"} (set (map #(get % "id") rows))))
        ;; Experimental switches stand last in the General section.
        (expect (= rows (take-last (count rows) general)))
        (expect (every? #(true? (get % "is_experimental")) rows))
        (expect (every? #(false? (get % "enabled")) (filter #(= "boolean" (get % "type")) rows)))
        (expect (= "off" (get (first (filter #(= "draft_backend" (get % "id")) rows)) "value"))))
      (toggles/hydrate-from-config! {"toggles" {"subagents" "on"}})
      (expect (toggles/enabled? "subagents"))
      (expect (= {"subagents" true} (select-keys (toggles/snapshot) ["subagents"])))))

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
                 (expect (str/includes? (agents/prompt {}) "council.run"))
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

(defdescribe disabled-subagents-refuse-http
             (it "disabled subagents refuse http"
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
                                          (json/write-json-str {:task "Work"})))))))))
