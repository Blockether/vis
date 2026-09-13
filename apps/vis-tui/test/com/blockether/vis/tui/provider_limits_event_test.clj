(ns com.blockether.vis.tui.provider-limits-event-test
  (:require [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe provider-limits-change-test
             ;; Issue #214: automatic sign-in must wake the poller, not wait for its stale window.
             (it "refreshes limits through the persistent subscription even for an idle tab"
                 (let [sink
                       (atom nil)

                       db
                       (atom {:provider-limits-force? false})]

                   (with-redefs-fn {#'vis/add-title-listener! (fn [& _])
                                    #'vis/add-title-pending-listener! (fn [& _])
                                    #'vis/remove-title-listener! (fn [& _])
                                    #'vis/remove-title-pending-listener! (fn [& _])
                                    #'vis/worker-future (fn [& _])
                                    #'state/tab-id-for-session (fn [& _]
                                                                 "tab")
                                    #'state/app-db db
                                    #'chat/subscribe-session-events! (fn [_ f]
                                                                       (reset! sink f)
                                                                       (fn []))}
                     #(let [cleanup (#'screen/subscribe-session-live! "session") chunk
                            (#'chat/gateway-event->chunk
                             {"type" "provider.limits_changed" "provider_id" "corp"})]
                        (try (expect (= {:phase :provider-limits-changed :provider-id "corp"}
                                        chunk))
                             (@sink chunk)
                             (expect (true? (:provider-limits-force? @db)))
                             (finally (cleanup))))))))
