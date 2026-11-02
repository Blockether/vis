(ns com.blockether.vis.tui.team-event-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.agent-team :as team]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defdescribe
  team-event-refresh-test
  ;; User report 2026-11-01: the running agent team showed only after seconds,
  ;; at the next poll. A child lifecycle event must read the team at once.
  (it
    "reads the team at once after session.team_updated, not at the next 5s refresh"
    (let [sink
          (atom nil)

          reads
          (LinkedBlockingQueue.)

          status
          (atom "[]")]

      (with-redefs-fn {#'vis/add-title-listener! (fn [& _])
                       #'vis/add-title-pending-listener! (fn [& _])
                       #'vis/remove-title-listener! (fn [& _])
                       #'vis/remove-title-pending-listener! (fn [& _])
                       #'vis/worker-future (fn [& _])
                       #'vis/setting (constantly {"enabled" true})
                       #'vis/request! (fn [_ url _]
                                        (when (str/includes? url "team-leader") (.offer reads url))
                                        {:status 200 :body @status})
                       #'state/tab-id-for-session (fn [& _]
                                                    "tab")
                       #'state/app-db (atom {})
                       #'chat/subscribe-session-events! (fn [_ f]
                                                          (reset! sink f)
                                                          (fn []))}
        #(let [cleanup (#'screen/subscribe-session-live! "team-leader") stop
               (team/start-refresh! (constantly "team-leader")
                                    (fn [])) chunk
               (#'chat/gateway-event->chunk
                {"type" "session.team_updated"
                 "session_id" "team-leader"
                 "team" {"active" 1 "total" 1}})]
           (try
             (expect (= {:phase :team-sync :session-id "team-leader" :team {"active" 1 "total" 1}}
                        chunk))
             (expect (some? (.poll reads 2 TimeUnit/SECONDS)) "The first read")
             (reset! status "[{\"session_id\":\"child\",\"task\":\"Work\",\"status\":\"running\"}]")
             (@sink chunk)
             (expect (some? (.poll reads 2 TimeUnit/SECONDS))
                     "The event reads the team before the 5s refresh")
             (expect (loop [left 40]
                       (or (= "running" (get-in (team/summary "team-leader") [:agents 0 "status"]))
                           (when (pos? left) (Thread/sleep 50) (recur (dec left)))))
                     "The event stores the new team before the 5s refresh")
             (finally (stop) (cleanup))))))))
