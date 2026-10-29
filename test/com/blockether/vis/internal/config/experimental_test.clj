(ns com.blockether.vis.internal.config.experimental-test
  (:require [charred.api :as json]
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
      ;; Subagents left the experimental group: the row stands under Council and is on by default.
      (let [spec (toggles/toggle-spec "subagents")]
        (expect (true? (:default spec)))
        (expect (false? (toggles/enabled? "subagents")))
        (expect (false? (:experimental? spec)))
        (expect (true? (:persist? spec)))
        (expect (= :council (:group spec)))
        (expect (= "council" (:parent spec)))
        (expect (contract/contribution-valid? spec))
        (expect (not (contract/contribution-valid? (assoc spec :experimental? "yes")))))
      (let [groups
            ;; Read the defaults, not the global config of the machine that runs the test.
            (let [home
                  (java.nio.file.Files/createTempDirectory
                    "vis-experimental-home"
                    (make-array java.nio.file.attribute.FileAttribute 0))

                  previous
                  (System/getProperty "user.home")]

              (try (System/setProperty "user.home" (str home))
                   (config/invalidate-config-cache!)
                   (get (json/read-json (:body (#'settings-api/list-settings-handler {}))) "groups")
                   (finally (System/setProperty "user.home" previous)
                            (config/invalidate-config-cache!)
                            (doseq [^java.io.File file (reverse (file-seq (.toFile home)))]
                              (.delete file)))))

            general
            (get (first (filter #(= "general" (get % "id")) groups)) "toggles")

            rows
            (filter #(get % "is_experimental") general)]

        (expect (= #{"draft_backend"} (set (map #(get % "id") rows))))
        (expect (= ["subagents"]
                   (mapv #(get % "id")
                         (get (first (filter #(= "council" (get % "id")) general)) "children"))))
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
                 (expect (every? #((:ext.symbol/active-fn %) {}) agents/symbols)))))

(defdescribe council-off-turns-subagents-off
             (it "council off turns subagents off whatever the subagents value is"
                 (let [db
                       (h/store)

                       sid
                       (str (h/store-session! db {:channel :api}))]

                   (toggles/set-enabled! "subagents" true)
                   (try (expect (agents/enabled? db sid))
                        (toggles/set-enabled! "council" false)
                        (expect (not (agents/enabled? db sid)))
                        (expect (= :feature-disabled
                                   (:error (failure-data
                                             #(agents/operation! {} :spawn {:task "Work"})))))
                        (finally (toggles/reset-to-default! "council")
                                 (toggles/set-enabled! "subagents" false))))))

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
