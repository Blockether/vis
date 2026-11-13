(ns com.blockether.vis.internal.gateway.settings-changed-event-test
  (:require [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.toggle :as contract]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defdescribe settings-changed-event-test
             ;; An open settings view in one client must see a change that another client saves.
             (it "sends the owner and the setting of each saved change to the fleet feed"
                 (let [frames
                       (atom [])

                       db
                       (h/store)

                       sid
                       (h/store-session! db {:title "Settings"})

                       id
                       "settings_changed_event"]

                   (toggles/register-toggle!
                     {:id id :label "Event" :default true :scopes contract/scopes})
                   (with-redefs-fn {#'state/fleet-sinks (atom {"watch" #(swap! frames conj %)})
                                    #'config/load-global-yaml-config-raw (constantly {})
                                    #'config/load-global-config-raw (constantly {})
                                    #'config/load-project-tiers-raw (constantly {})
                                    #'config/load-project-config-raw (constantly {})}
                     #(scoped/set-setting! db (scoped/target db "session" sid) id "value" false))
                   (let [frame (last @frames)]
                     (expect (gateway-contract/session-event-type? (get frame "type")))
                     (expect (= {"type" "settings.changed"
                                 "target" {"scope" "session" "target_id" (str sid)}
                                 "id" id}
                                (select-keys frame ["type" "target" "id"])))))))
