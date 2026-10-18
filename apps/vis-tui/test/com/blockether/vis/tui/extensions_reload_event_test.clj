(ns com.blockether.vis.tui.extensions-reload-event-test
  (:require [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe extensions-reload-event-test
             ;; Issue #393: a reload in another client changes the slash commands of this TUI.
             (it "loads the slash catalog again once for each reload id"
                 (let [sink
                       (atom nil)

                       loads
                       (atom [])]

                   (with-redefs-fn {#'vis/add-title-listener! (fn [& _])
                                    #'vis/add-title-pending-listener! (fn [& _])
                                    #'vis/remove-title-listener! (fn [& _])
                                    #'vis/remove-title-pending-listener! (fn [& _])
                                    #'vis/worker-future (fn [& _])
                                    #'state/tab-id-for-session (fn [& _]
                                                                 "tab")
                                    #'state/app-db (atom {})
                                    #'screen/last-extensions-reload (atom nil)
                                    #'screen/start-deferred-gateway-slash-load! #(swap! loads conj
                                                                                   %)
                                    #'chat/subscribe-session-events! (fn [_ f]
                                                                       (reset! sink f)
                                                                       (fn []))}
                     #(let [cleanup (#'screen/subscribe-session-live! "session") chunk
                            (fn [reload-id]
                              (#'chat/gateway-event->chunk
                               {"type" "extensions.reloaded" "reload_id" reload-id}))]
                        (try (expect (= {:phase :extensions-reloaded :reload-id "r1"} (chunk "r1")))
                             ;; Another tab stream carries a copy of the same reload.
                             (doseq [reload-id ["r1" "r1" "r2"]]
                               (@sink (chunk reload-id)))
                             (expect (= ["session" "session"] @loads))
                             (finally (cleanup))))))))
