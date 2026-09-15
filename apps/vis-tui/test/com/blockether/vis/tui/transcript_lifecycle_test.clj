(ns com.blockether.vis.tui.transcript-lifecycle-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.active-live-test :as active-live]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.scroll :as scroll]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.state-test :as state-test]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.virtual :as virtual]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(def ^:private checkpoint "Reasoning checkpoint 6")

(defn- lifecycle-db
  [pane]
  (let [iterations
        (into (if pane (:iterations active-live/review-progress) [])
              (mapv (fn [i]
                      {:thinking (str "Reasoning checkpoint " i)
                       :forms [{:code (str "verify_" i "()") :stdout "Verified" :success? true}]})
                    (range 12)))]
    (merge
      (dissoc (active-live/viewer-review-db pane) :live-viewer-id)
      (#'state-test/terminal-test-db)
      {:progress {:iterations iterations} :scroll scroll/follow :live-views (if pane [pane] [])})))

(defn- paint!
  [^TerminalScreen terminal]
  (let [layout (#'screen/render-frame! terminal 100 44 @state/app-db 1000)]
    ;; Production publishes after painting; reanchor-scroll is dispatched by the painter.
    (state/dispatch [:set-layout layout])
    layout))

(defn- painted-row
  [^TerminalScreen terminal text]
  (first (keep (fn [y]
                 (let [line (apply str
                              (for [x (range 100)]
                                (.getCharacterString
                                  (.getBackCharacter terminal (int x) (int y)))))]
                   (when (str/includes? line text) y)))
               (range 44))))

(defn- park-at-checkpoint!
  [^TerminalScreen terminal]
  (let [layout
        (paint! terminal)

        db
        @state/app-db

        payload
        (render/progress->lines-data (:progress db)
                                     (- 100 render/MESSAGE_SIDE_PAD)
                                     (:settings db)
                                     {:session-id "s1"
                                      :session-turn-id "c1"
                                      :now-ms 1000
                                      :live-runs (mapv lv/transcript-run (:live-views db))})

        row
        (first (keep-indexed #(when (str/includes? %2 checkpoint) %1) (:lines payload)))

        assistant-top
        (nth (:offsets layout) 1)]

    (is (number? row))
    (state/dispatch [:set-scroll (+ assistant-top row)])
    (paint! terminal)))

(defn- assert-following
  [layout]
  (is (= :follow (get-in @state/app-db [:scroll :mode])))
  (is (= (:eff-scroll layout) (max 0 (- (:total-h layout) (:inner-h layout))))))

(deftest terminal-handoff-keeps-the-painted-trace
  ;; #233: loading stops before the worker replaces the pending assistant. The
  ;; parked terminal trace must paint throughout that real intermediate state.
  (doseq [[source status]
          [[:terminal :cancelled] [:terminal :completed] [:cancel-ack :cancelled]]

          follow?
          [false true]]

    (virtual/invalidate-heights!)
    (render/invalidate-cache!)
    (with-open [terminal
                (DefaultVirtualTerminal. (TerminalSize. 100 44))

                ts
                (doto (TerminalScreen. terminal) (.startScreen))]

      (binding [interactions/hit-map (interactions/create-hit-map)]
        (with-redefs [state/app-db (atom (lifecycle-db nil))
                      client/get-router (constantly nil)
                      client/notify! (fn [& _])
                      client/worker-future (fn [_ _]
                                             (future nil))
                      timg/images-protocol (constantly nil)]

          (let [before (if follow? (paint! ts) (park-at-checkpoint! ts))
                row-before (painted-row ts checkpoint)
                code-row-before (painted-row ts "CODE")]

            (when-not follow? (is (number? row-before)))
            (if (= :cancel-ack source)
              (do
                ;; Cancel ACK parks only :trace, without the terminal event's :status.
                (swap! state/app-db assoc :cancelling? true :cancelling-at-ms 1000)
                (state/dispatch [:gateway-cancel-result 1000 {:status "cancelling"}])
                (is (nil? (get-in @state/app-db [:messages 1 :terminal-pending :status]))))
              (#'state-test/sync-terminal-without-timer! {:turn-id "t1" :status (name status)}))
            (is (false? (:loading? @state/app-db)))
            (is (nil? (get-in @state/app-db [:messages 1 :traces])))
            (is (= 12 (count (get-in @state/app-db [:messages 1 :terminal-pending :trace]))))
            (let [pending (paint! ts)
                  pending-again (paint! ts)]

              (is (= (:eff-scroll pending) (:eff-scroll pending-again)))
              (is (number? (painted-row ts "CODE")))
              (is (> (:total-h pending) (:inner-h pending)))
              (is (number? (painted-row ts (if follow? "Reasoning checkpoint 11" checkpoint))))
              (if follow?
                (assert-following pending)
                (do (is (= row-before (painted-row ts checkpoint)))
                    (is (= code-row-before (painted-row ts "CODE")))
                    (is (= (:eff-scroll before) (:eff-scroll pending)))))
              (if (= :cancel-ack source)
                (state/dispatch [:message-received nil ""
                                 {:client-turn-id "c1" :status :cancelled}])
                (#'state-test/settle-marked-terminal!))
              (is (false? (boolean (get-in @state/app-db [:messages 1 :pending?]))))
              (let [settled (paint! ts)
                    again (paint! ts)]

                (is (= (:eff-scroll settled) (:eff-scroll again)))
                (if follow?
                  (do (assert-following settled) (assert-following again))
                  (do (is (= row-before (painted-row ts checkpoint)))
                      (is (= code-row-before (painted-row ts "CODE")))))))))))))

(deftest retained-live-view-keeps-the-reading-position
  ;; #233: a receipt replaces the inline body above the reader. Code/reasoning
  ;; rows, not only Activity items, must retain their screen coordinates.
  (doseq [reason
          [:cancelled :completed]

          follow?
          [false true]]

    (virtual/invalidate-heights!)
    (render/invalidate-cache!)
    (with-open [terminal
                (DefaultVirtualTerminal. (TerminalSize. 100 44))

                ts
                (doto (TerminalScreen. terminal) (.startScreen))]

      (binding [interactions/hit-map (interactions/create-hit-map)]
        (with-redefs [state/app-db (atom (lifecycle-db (active-live/review-pane)))
                      client/get-router (constantly nil)
                      timg/images-protocol (constantly nil)]

          (let [before (if follow? (paint! ts) (park-at-checkpoint! ts))
                row-before (painted-row ts checkpoint)
                code-row-before (painted-row ts "CODE")
                view-id (lv/view-id (first (:live-views @state/app-db)))]

            (when-not follow? (is (number? row-before)))
            (state/dispatch [:live-view-close view-id {:reason reason}])
            (is (true? (:loading? @state/app-db)))
            (is (lv/dormant? (first (:live-views @state/app-db))))
            (let [after (paint! ts)
                  again (paint! ts)]

              (is (number? (painted-row ts "CODE")))
              (is (< (:total-h after) (:total-h before)))
              (is (= (:eff-scroll after) (:eff-scroll again)))
              (if follow?
                (do (assert-following after) (assert-following again))
                (do (is (= row-before (painted-row ts checkpoint)))
                    (is (= code-row-before (painted-row ts "CODE"))))))))))))
