(ns com.blockether.vis.tui.goals-test
  "Goal projection and production footer fixtures. No gateway or model calls."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as contract]
            [com.blockether.vis.tui.capture :as capture]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.header :as header]
            [com.blockether.vis.tui.footer :as footer]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [com.googlecode.lanterna.screen TerminalScreen]))

(def goal
  "Deterministic goal shared by terminal-grid and HTML review fixtures."
  {"id" "goal-fixture"
   "objective" "Verify session goals across API, SDK and TUI — 界 é"
   "status" "active"
   "iteration_budget" 30
   "iterations_used" 12
   "tokens_used" 12400
   "time_used_ms" 32000
   "revision" 3
   "version" 1
   "created_at" 1
   "updated_at" 2})

(def session-id "123e4567-e89b-12d3-a456-426614174000")

(defn header-db
  "Session metadata as projected by production chat/resume-session."
  [current-goal]
  {:title "Explicit session goals" :session {:id session-id :goal current-goal}})

(defn paint-header!
  "Paint the real header and its click regions on either terminal backend."
  [^TerminalScreen screen current-goal cols]
  (.reset interactions/hit-map)
  (.beginFrame interactions/hit-map)
  (header/draw-header! (.newTextGraphics screen) (header-db current-goal) 0 cols)
  (.commitFrame interactions/hit-map))

(defdescribe
  goal-projection-test
  (it "keeps canonical wire data for live updates and reconnect snapshots"
      (expect (= {:phase :goal-sync :session-id session-id :goal goal}
                 (#'chat/gateway-event->chunk
                  {"type" "session.goal_updated" "session_id" session-id "goal" goal})))
      (expect (= goal
                 (:goal (#'chat/gateway-event->chunk
                         {"type" "subscription.ready" "is_live" false "goal" goal})))))
  (it "loads a persisted goal with the session instead of inventing one"
      (doseq [current [nil goal]]
        (with-redefs [client/gateway-soul (fn [_]
                                            {"id" session-id "status" "idle" "goal" current})
                      client/gateway-list-turns (constantly [])
                      chat/history-page (fn [& _]
                                          {:messages [] :offset 0 :total 0 :has-more false})]

          (expect (= current (:goal (chat/resume-session session-id))))))))

(defdescribe
  goal-tab-state-test
  (it
    "preserves revisions, session isolation and goal state across idle reconnects"
    (let [before
          @state/app-db

          done
          (assoc goal
            "revision" 4
            "status" "complete")

          replacement
          (assoc goal
            "id" "new-goal"
            "revision" 5)]

      (try (reset! state/app-db {:active-tab-id "a"
                                 :tabs [{:id "a"} {:id "b"}]
                                 :session {:id "other-session"}
                                 :render-version 0
                                 :tab-locals {"b" {:session {:id session-id}}}})
           (state/dispatch [:sync-session-goal "b" {:session-id session-id :goal done}])
           (expect (nil? (get-in @state/app-db [:session :goal])))
           (expect (= done (get-in @state/app-db [:tab-locals "b" :session :goal])))
           (doseq [incoming [goal
                             (assoc goal
                               "revision" 9
                               "status" "unknown")]]
             (state/dispatch [:sync-session-goal "b" {:goal incoming}])
             (expect (= done (get-in @state/app-db [:tab-locals "b" :session :goal]))))
           (state/dispatch [:sync-session-goal "a" {:session-id session-id :goal replacement}])
           (expect (nil? (get-in @state/app-db [:session :goal])))
           (state/dispatch [:sync-gateway-ready "b" {:is-state-known true :goal replacement}])
           (expect (= replacement (get-in @state/app-db [:tab-locals "b" :session :goal])))
           ;; /goal --clear arrives as a newer revision; an older snapshot cannot bring the goal back.
           (let [cleared (assoc replacement
                           "revision" 6
                           "status" "cleared")]
             (state/dispatch [:sync-session-goal "b" {:goal cleared}])
             (state/dispatch [:sync-session-goal "b" {:goal replacement}])
             (expect (= cleared (get-in @state/app-db [:tab-locals "b" :session :goal]))))
           (finally (reset! state/app-db before))))))

(defn- paint-footer
  "Paint the real footer and return its second row and click regions."
  [current-goal cols now-ms]
  (let [frame (capture/capture! {:cols cols
                                 :rows 6
                                 :paint! (fn [{:keys [screen]}]
                                           (.reset interactions/hit-map)
                                           (.beginFrame interactions/hit-map)
                                           (footer/draw-footer! (.newTextGraphics ^TerminalScreen
                                                                                  screen)
                                                                (header-db current-goal)
                                                                0
                                                                cols
                                                                now-ms)
                                           (.commitFrame interactions/hit-map))})]
    (expect (nil? (:error frame)))
    (nth (str/split-lines (capture/frame-text frame)) 1)))

(defdescribe
  goal-footer-test
  (it
    ;; Issue #204: limits are passive text, never a modal entry point. The goal is passive too.
    "renders Limits and the Goal state as text without objective, token counts or click regions"
    (doseq [cols
            [40 80 120]

            status
            (keys contract/session-goal-labels)]

      (let [row
            (paint-footer (assoc goal "status" status) cols 0)

            goal-col
            (str/index-of row "Goal")]

        (expect (not (str/includes? row "tokens")))
        (expect (not (str/includes? row "12,400")))
        (expect (not (some #(= :footer-limits (:kind %)) (.current interactions/hit-map))))
        (expect (not (str/includes? row "Verify session goals")))
        (if (contains? #{"cleared" "complete"} status)
          ;; A cleared goal was forgotten, and a complete goal leaves the footer.
          (expect (nil? goal-col))
          (do (when (= 120 cols)
                (expect (str/includes?
                          row
                          (str "Goal: " (get contract/session-goal-labels status) " - 32.0s"))))
              (when goal-col
                (doseq [col (range goal-col (+ (long goal-col) 5))]
                  (expect (nil? (.lookup interactions/hit-map col 1))))))))))
  (it "adds uncheckpointed active wall time but freezes all inactive states"
      (expect (str/includes? (paint-footer goal 120 3002) "Goal: Active - 35.0s"))
      (doseq [status ["paused" "blocked" "budget_limited" "cancelled"]]
        (expect (str/includes? (paint-footer (assoc goal "status" status) 120 100000)
                               " - 32.0s")))))
