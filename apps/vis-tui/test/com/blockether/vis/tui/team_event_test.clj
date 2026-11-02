(ns com.blockether.vis.tui.team-event-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.agent-team :as team]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defn- row [id status] {"session_id" id "task" "Work" "status" status})

(defn- wait-for
  "Poll `pred` for up to 2 s. Returns its last value."
  [pred]
  (loop [left 40]
    (or (pred) (when (pos? left) (Thread/sleep 50) (recur (dec left))))))

(defn- with-live-session
  "Open the live subscription of `sid` with the refresh worker, and call `f` with the raw
   gateway event sink. `reads` gets the URL of each team read and `body` is its answer."
  [sid reads body f]
  (let [sink (atom nil)]
    (with-redefs-fn {#'vis/add-title-listener! (fn [& _])
                     #'vis/add-title-pending-listener! (fn [& _])
                     #'vis/remove-title-listener! (fn [& _])
                     #'vis/remove-title-pending-listener! (fn [& _])
                     #'vis/worker-future (fn [& _])
                     #'vis/gateway-current-seq (constantly 0)
                     #'vis/gateway-mux-subscribe! (fn [_ s _]
                                                    (reset! sink s)
                                                    (fn []))
                     #'vis/setting (constantly {"enabled" true})
                     #'vis/request! (fn [_ url _]
                                      (when (str/includes? url sid)
                                        (.offer ^LinkedBlockingQueue reads url))
                                      {:status 200 :body @body})
                     #'state/tab-id-for-session (fn [& _]
                                                  "tab")
                     #'state/dispatch (fn [& _])
                     #'state/app-db (atom {})}
      #(let [cleanup (#'screen/subscribe-session-live! sid) stop
             (team/start-refresh! (constantly sid)
                                  (fn []))] (try (f @sink) (finally (stop) (cleanup)))))))

(defdescribe
  team-event-refresh-test
  ;; User report 2026-11-01: the running agent team showed only after seconds,
  ;; at the next poll. A child lifecycle event must show the team at once.
  (it "shows the agents of session.team_updated at once, without a team read"
      (let [reads (LinkedBlockingQueue.)]
        (with-live-session
          "team-leader"
          reads
          (atom "[]")
          (fn [sink]
            (expect (some? (.poll reads 2 TimeUnit/SECONDS)) "The open read")
            (sink {"type" "session.team_updated"
                   "session_id" "team-leader"
                   "team" {"active" 1 "total" 1}
                   "agents" [(row "child" "running")]})
            (expect (wait-for #(= "running"
                                  (get-in (team/summary "team-leader") [:agents 0 "status"]))))
            (expect (nil? (.poll reads 1 TimeUnit/SECONDS)) "The event rows need no read")))))
  (it "reads the team when session.team_updated has no usable agents"
      ;; A gateway before protocol 21 sends only the counts.
      (let [reads
            (LinkedBlockingQueue.)

            body
            (atom "[]")]

        (with-live-session
          "old-gateway-leader"
          reads
          body
          (fn [sink]
            (expect (some? (.poll reads 2 TimeUnit/SECONDS)) "The open read")
            (reset! body "[{\"session_id\":\"child\",\"task\":\"Work\",\"status\":\"running\"}]")
            (sink {"type" "session.team_updated"
                   "session_id" "old-gateway-leader"
                   "team" {"active" 1 "total" 1}})
            (expect (some? (.poll reads 2 TimeUnit/SECONDS)) "The event reads the team")
            (expect (wait-for #(= "running"
                                  (get-in (team/summary "old-gateway-leader")
                                          [:agents 0 "status"])))))))))

(defdescribe
  team-reconnect-test
  ;; The gateway does not replay session.team_updated. Without a read after a reconnect, an
  ;; agent that started during the gap never shows, because no timed read heals it.
  (describe
    "a reconnected event stream"
    (it "reads the team again and shows the agents of the gap"
        (let [reads
              (LinkedBlockingQueue.)

              body
              (atom "[]")]

          (with-live-session
            "gap-leader"
            reads
            body
            (fn [sink]
              (expect (some? (.poll reads 2 TimeUnit/SECONDS)) "The open read")
              (sink {"type" "subscription.ready" "session_id" "gap-leader"})
              (expect (nil? (.poll reads 1 TimeUnit/SECONDS))
                      "The first subscription.ready follows the open read")
              (sink {:type "gateway.disconnected"})
              (expect (wait-for #(= "Agents stale" (team/header-label "gap-leader" false))))
              (reset! body "[{\"session_id\":\"child\",\"task\":\"Work\",\"status\":\"running\"}]")
              (sink {"type" "subscription.ready" "session_id" "gap-leader"})
              (expect (some? (.poll reads 2 TimeUnit/SECONDS)) "The reconnect reads the team")
              (expect (wait-for #(= "Agents 1 active/1 cached"
                                    (team/header-label "gap-leader" false))))))))
    (it "reads also a leader whose session row counted no agents"
        (let [reads (LinkedBlockingQueue.)]
          (team/note-team! "empty-leader" {"active" 0 "total" 0})
          (with-live-session "empty-leader"
                             reads
                             (atom "[]")
                             (fn [sink]
                               (expect (nil? (.poll reads 1 TimeUnit/SECONDS))
                                       "A team of 0 needs no open read")
                               (sink {"type" "subscription.ready" "session_id" "empty-leader"})
                               (sink {:type "gateway.disconnected"})
                               (sink {"type" "subscription.ready" "session_id" "empty-leader"})
                               (expect (some? (.poll reads 2 TimeUnit/SECONDS))
                                       "The gap can hold the first agents")))))))

(defdescribe
  no-timed-team-read-test
  ;; Requirement (leader task "stop polling GET /agents", 2026-11): the request count is the
  ;; requirement. The TUI read the team and the Subagents setting every 5 s, 24 requests a
  ;; minute for each open session. Now an open session reads once, and events keep it.
  (it "reads the team and the setting once while an active team runs"
      (let [requests (atom {})]
        (with-redefs [vis/setting (fn [& _]
                                    (swap! requests update :setting (fnil inc 0))
                                    {"enabled" true})
                      vis/request!
                      (fn [& _]
                        (swap! requests update :agents (fnil inc 0))
                        {:status 200
                         :body
                         "[{\"session_id\":\"c\",\"task\":\"Work\",\"status\":\"running\"}]"})]

          (let [stop (team/start-refresh! (constantly "timer-leader")
                                          (fn []))]
            (try (Thread/sleep 6000)
                 (expect (= {:setting 1 :agents 1} @requests))
                 (finally (stop)))))))
  (it "reads only the setting of a leader whose session row counts no agents"
      (let [requests (atom {})]
        (team/note-team! "no-team-leader" {"active" 0 "total" 0})
        (with-redefs [vis/setting (fn [& _]
                                    (swap! requests update :setting (fnil inc 0))
                                    {"enabled" true})
                      vis/request! (fn [& _]
                                     (swap! requests update :agents (fnil inc 0))
                                     {:status 200 :body "[]"})]

          (let [stop (team/start-refresh! (constantly "no-team-leader")
                                          (fn []))]
            (try (Thread/sleep 1000)
                 (expect (= {:setting 1} @requests))
                 (expect (= "Agents 0 active/0 cached" (team/header-label "no-team-leader" false)))
                 (finally (stop))))))))
