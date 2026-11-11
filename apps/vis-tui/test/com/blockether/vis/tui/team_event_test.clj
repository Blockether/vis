(ns com.blockether.vis.tui.team-event-test
  (:require [com.blockether.vis.tui.agent-team :as team]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- row [id status] {"session_id" id "task" "Work" "status" status})

(defn- wait-for
  "Poll `pred` for up to 2 s. Returns its last value."
  [pred]
  (loop [left 40]
    (or (pred) (when (pos? left) (Thread/sleep 50) (recur (dec left))))))

(defn- with-live-session
  "Open the live subscription of `sid` with the refresh worker, and call `f` with the raw
   gateway event sink. `requests` gets each gateway request: the team needs none."
  [sid requests f]
  (let [sink (atom nil)]
    (with-redefs-fn {#'vis/add-title-listener! (fn [& _])
                     #'vis/add-title-pending-listener! (fn [& _])
                     #'vis/remove-title-listener! (fn [& _])
                     #'vis/remove-title-pending-listener! (fn [& _])
                     #'vis/refresh-session-model! (fn [& _])
                     #'vis/worker-future (fn [& _])
                     #'vis/gateway-current-seq (constantly 0)
                     #'vis/gateway-mux-subscribe! (fn [_ s _]
                                                    (reset! sink s)
                                                    (fn []))
                     #'vis/setting (constantly {"enabled" true})
                     #'vis/request! (fn [& args]
                                      (swap! requests conj args)
                                      {:status 404})
                     #'state/tab-id-for-session (fn [& _]
                                                  "tab")
                     #'state/dispatch (fn [& _])
                     #'state/app-db (atom {})}
      #(let [cleanup (#'screen/subscribe-session-live! sid) stop
             (team/start-refresh! (constantly sid)
                                  (fn []))] (try (f @sink) (finally (stop) (cleanup)))))))

(defn- statuses [sid] (mapv #(get % "status") (:agents (team/summary sid))))

(defdescribe ready-frame-team-test
             ;; Requirement (leader task "remove GET /agents", 2026-11): the gateway answers the team read
             ;; no more. The rows of subscription.ready are the team, and pushed rows merge after them.
             (it "shows the agents of subscription.ready without a team read"
                 (let [requests (atom [])]
                   (with-live-session
                     "ready-leader"
                     requests
                     (fn [sink]
                       (sink {"type" "subscription.ready"
                              "session_id" "ready-leader"
                              "agents" [(row "c1" "running") (row "c2" "completed")]})
                       (expect (wait-for #(= ["running" "completed"] (statuses "ready-leader"))))
                       (expect (= "Agents 1 active/2 cached"
                                  (team/header-label "ready-leader" false)))
                       (Thread/sleep 300)
                       (expect (= [] @requests) "No request reads the team")))))
             (it "merges the rows of session.team_updated after the ready frame rows"
                 (with-live-session
                   "push-leader"
                   (atom [])
                   (fn [sink]
                     (sink {"type" "subscription.ready"
                            "session_id" "push-leader"
                            "agents" [(row "c1" "running")]})
                     (sink {"type" "session.team_updated"
                            "session_id" "push-leader"
                            "team" {"active" 1 "total" 2}
                            "agents" [(row "c1" "completed") (row "c2" "running")]})
                     (expect (wait-for #(= ["completed" "running"] (statuses "push-leader")))))))
             (it "keeps the team when session.team_updated has no usable agents"
                 (with-live-session "count-leader"
                                    (atom [])
                                    (fn [sink]
                                      (sink {"type" "subscription.ready"
                                             "session_id" "count-leader"
                                             "agents" [(row "c1" "running")]})
                                      (expect (wait-for #(= ["running"] (statuses "count-leader"))))
                                      (sink {"type" "session.team_updated"
                                             "session_id" "count-leader"
                                             "team" {"active" 0 "total" 1}})
                                      (Thread/sleep 400)
                                      (expect (= ["running"] (statuses "count-leader")))))))

(defdescribe team-reconnect-test
             ;; The gateway does not replay session.team_updated. An agent that starts during a dropped
             ;; stream shows only through the rows of the next subscription.ready.
             (describe "a reconnected event stream"
                       (it "replaces the team with the rows of the new ready frame"
                           (with-live-session
                             "gap-leader"
                             (atom [])
                             (fn [sink]
                               (sink {"type" "subscription.ready"
                                      "session_id" "gap-leader"
                                      "agents" [(row "c1" "running")]})
                               (expect (wait-for #(= ["running"] (statuses "gap-leader"))))
                               (sink {:type "gateway.disconnected"})
                               (expect (wait-for #(= "Agents stale"
                                                     (team/header-label "gap-leader" false))))
                               (sink {"type" "subscription.ready"
                                      "session_id" "gap-leader"
                                      "agents" [(row "c1" "completed") (row "c2" "running")]})
                               (expect (wait-for #(= "Agents 1 active/2 cached"
                                                     (team/header-label "gap-leader" false)))))))
                       (it "shows no rows when the new ready frame has no agents"
                           (with-live-session
                             "emptied-leader"
                             (atom [])
                             (fn [sink]
                               (sink {"type" "subscription.ready"
                                      "session_id" "emptied-leader"
                                      "agents" [(row "c1" "running")]})
                               (expect (wait-for #(= ["running"] (statuses "emptied-leader"))))
                               (sink {"type" "subscription.ready" "session_id" "emptied-leader"})
                               (expect (wait-for #(= [] (statuses "emptied-leader")))))))))

;; Protocol 23: a push sends only the changed keys of a known row, so the 8 KB task of a child
;; does not come again with each step. A lost change would show an old status.
(defdescribe team-change-test
             (it "applies the changes of session.team_updated to the known rows, in order"
                 (with-live-session
                   "change-leader"
                   (atom [])
                   (fn [sink]
                     (sink {"type" "subscription.ready"
                            "session_id" "change-leader"
                            "agents" [(assoc (row "c1" "running") "activity" "Read a.clj")]})
                     (sink {"type" "session.team_updated"
                            "session_id" "change-leader"
                            "team" {"active" 1 "total" 2}
                            "agents" [(row "c2" "running")]
                            "changes" [{"session_id" "c1" "activity" "Patched a.clj"}]})
                     (sink {"type" "session.team_updated"
                            "session_id" "change-leader"
                            "team" {"active" 1 "total" 2}
                            "agents" []
                            "changes" [{"session_id" "c1" "status" "completed" "activity" nil}]})
                     (expect (wait-for #(= ["completed" "running"] (statuses "change-leader"))))
                     (let [[c1] (:agents (team/summary "change-leader"))]
                       (expect (= "Work" (get c1 "task"))
                               "A change keeps the keys that it does not name")
                       (expect (not (contains? c1 "activity")) "A null value removes the key"))))))
