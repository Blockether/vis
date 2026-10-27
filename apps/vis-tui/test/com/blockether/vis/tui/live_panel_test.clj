(ns com.blockether.vis.tui.live-panel-test
  "The panel placement: the side panel beside the chat."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.scroll :as scroll]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminal-image :as timg]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal]))

(defn- view
  "The shared primitives fixture as a view of `extension` with `placement`."
  [id title extension placement]
  (assoc (hi/live-view<-wire (json/read-json
                               (slurp (io/resource "vis-contract/fixtures/live-primitives.json"))))
    :id id
    :title title
    :extension extension
    :placement placement
    :session-id "s1"))

(defn- panel-lines
  [panes panel-state cols rows]
  (let [capture (cap/capture! {:cols cols
                               :rows rows
                               :paint!
                               (fn [{:keys [screen]}]
                                 (.beginFrame interactions/hit-map)
                                 (let [g (.newTextGraphics ^TerminalScreen screen)
                                       geom (lv/paint-panel! g 0 0 cols rows panes panel-state 0)]

                                   (.commitFrame interactions/hit-map)
                                   (.refresh ^TerminalScreen screen)
                                   geom))})]
    {:geometry (:ret capture) :lines (str/split-lines (cap/frame-text capture))}))

(defn- with-db
  [f]
  (let [old @state/app-db]
    (try (reset! state/app-db {:render-version 0 :session {:id "s1"} :messages []})
         (f)
         (finally (reset! state/app-db old)))))

(defdescribe
  panel-geometry-test
  (it "takes 40% of the chat columns, at least 40, and docks only with 60 columns left"
      (expect (= {:left 120 :width 80 :is-docked true :chat-cols 120} (lv/panel-geometry 200)))
      (expect (= {:left 50 :width 40 :is-docked false :chat-cols 90} (lv/panel-geometry 90)))
      (expect (= 30 (:width (lv/panel-geometry 30))))))

(defdescribe panel-paint-test
             (describe "a panel with views from two owners"
                       (let [panes (mapv lv/opened
                                         [(view "a" "Deploy board" "deploy" :panel)
                                          (view "b" "Job queue" "jobs" :panel)])]
                         (it "shows tabs and only the views of the selected owner"
                             (let [{:keys [geometry lines]} (panel-lines panes {} 48 24)]
                               (expect (str/includes? (first lines) "deploy"))
                               (expect (str/includes? (first lines) "jobs"))
                               (expect (some #(str/includes? % "▾ Deploy board") lines))
                               (expect (not-any? #(str/includes? % "Job queue") lines))
                               (expect (= {:selected "a" :shown ["a"] :tab "deploy"} geometry))
                               (expect (every? #(str/starts-with? % "│") lines))))
                         (it "shows the other owner's views on its tab"
                             (let [{:keys [lines]} (panel-lines panes {:tab "jobs"} 48 24)]
                               (expect (some #(str/includes? % "▾ Job queue") lines))
                               (expect (not-any? #(str/includes? % "Deploy board") lines))))))
             (describe "a folded view"
                       (it "keeps only its title row"
                           (let [pane
                                 (lv/opened (view "a" "Deploy board" "deploy" :panel))

                                 {:keys [lines]}
                                 (panel-lines [pane] {:folded #{"a"}} 48 12)]

                             (expect (str/includes? (first lines) "▸ Deploy board"))
                             (expect (every? #(= "│" (str/trim %)) (rest lines))))))
             (describe
               "a view with input fields"
               ;; Regression: the node label and the field renderer both drew the label.
               (it "draws each field label once"
                   (let [pane
                         (lv/opened (assoc (view "a" "Deploy board" "deploy" :panel)
                                      :nodes [{:id "comment" :type :multiline :label "Comment"}
                                              {:id "title" :type :plaintext :label "Title"}]))

                         {:keys [lines]}
                         (panel-lines [pane] {} 48 16)]

                     (expect (= 1 (count (filter #(= "Comment" (str/trim (subs % 1))) lines))))
                     (expect (= 1 (count (filter #(= "Title" (str/trim (subs % 1))) lines))))))))

(defdescribe
  panel-state-test
  (it "files no transcript receipt for a panel view, and its close removes it"
      (with-db (fn []
                 (state/dispatch [:live-view-open (view "a" "Deploy board" "deploy" :panel)])
                 (expect (= ["a"] (mapv lv/view-id (lv/panel-panes (:live-views @state/app-db)))))
                 (expect (empty? (:messages @state/app-db)))
                 (expect (keymap/verb-available? @state/app-db {:show-when :has-panel}))
                 (state/dispatch [:live-view-close "a" {:reason :completed}])
                 (expect (empty? (:live-views @state/app-db))))))
  (it "C-x V opens the panel with the focus, then moves the focus back to the transcript"
      (with-db (fn []
                 (state/dispatch [:live-panel-focus])
                 (expect (= {:open? true :focused? true} (:live-panel @state/app-db)))
                 (state/dispatch [:live-panel-focus])
                 (expect (= {:open? true :focused? false} (:live-panel @state/app-db)))
                 (state/dispatch [:live-panel-focus])
                 (expect (true? (get-in @state/app-db [:live-panel :focused?])))
                 (state/dispatch [:live-panel-fold "a" true])
                 (expect (= #{"a"} (get-in @state/app-db [:live-panel :folded])))
                 (state/dispatch [:live-panel-close])
                 (expect (false? (get-in @state/app-db [:live-panel :open?])))))))

(defdescribe
  view-action-route-test
  (it "sends an action to the route of the view's session"
      (let [called (atom nil)]
        (with-redefs-fn {#'client/send-json! (fn [method path body]
                                               (reset! called [method path body])
                                               {"action" "activate" "is_accepted" true})}
          (fn []
            (expect (:is-accepted (client/view-action! "s1" "a" {:action :activate :node-id "go"})))
            (expect (= ["POST" "/v1/sessions/s1/views/a/actions" {:action "activate" :node-id "go"}]
                       @called)))))))

(defn- frame-db
  []
  {:session {:id "s1"}
   :messages [{:role :assistant :text "The deploy is running."}]
   :input (input/empty-input)
   :scroll scroll/follow
   :settings {}
   :live-views [(lv/opened (view "a" "Deploy board" "deploy" :panel))]
   :live-panel {:open? true :focused? true}
   :pending-sends []
   :channel-status {}
   :tabs []
   :tab-locals {}
   :slash-command-index 0
   :render-version 0})

(defn- paint-frame!
  "Paint one production frame at `cols` x 40 and return its layout and text rows."
  [cols db]
  (with-open [terminal
              (DefaultVirtualTerminal. (TerminalSize. cols 40))

              ts
              (doto (TerminalScreen. terminal) (.startScreen))]

    (with-redefs [timg/images-protocol
                  (constantly nil)

                  client/get-router
                  (constantly nil)]

      (let [layout (#'screen/render-frame! ts cols 40 db 1000)]
        (.refresh ts)
        {:layout layout
         :lines (mapv (fn [row]
                        (apply str
                          (map #(.getCharacterString (.getCharacter terminal
                                                                    (TerminalPosition. % row)))
                               (range cols))))
                      (range 40))}))))

(defdescribe panel-frame-test
             (it "docks beside the chat when 60 chat columns remain"
                 (binding [interactions/hit-map (interactions/create-hit-map)]
                   (with-redefs [state/app-db (atom (frame-db))]
                     (let [{:keys [layout lines]} (paint-frame! 160 @state/app-db)]
                       (expect (= 96 (:cols layout)))
                       (expect (every? #(= \│ (nth % 96)) lines))
                       (expect (some #(str/includes? % "▾ Deploy board") lines))
                       (let [toggle (first (filter #(= :live-panel-toggle (:kind %))
                                                   (.current interactions/hit-map)))]
                         (expect (= "a" (:view-id toggle)))
                         (expect (#'screen/activate-live-region! @state/app-db toggle))
                         (expect (= #{"a"} (get-in @state/app-db [:live-panel :folded]))))))))
             (it "covers the right side of the transcript on a narrow terminal"
                 (binding [interactions/hit-map (interactions/create-hit-map)]
                   (with-redefs [state/app-db (atom (frame-db))]
                     (let [{:keys [layout lines]} (paint-frame! 90 @state/app-db)
                           top (long (:messages-top layout))]

                       (expect (= 90 (:cols layout)))
                       (expect (= \│ (nth (nth lines (inc top)) 50)))
                       (expect (not= \│ (nth (nth lines 1) 50))))))))

(defn- areas-db
  "The frame fixture with the project sidebar open next to the focused side panel."
  []
  (assoc (frame-db) :project-sidebar {:open? true :focused? false :index 0 :items []}))

(defn- focus-of [db] (#'screen/focused-area db))

(defdescribe focus-area-test
             (it "C-o gives the keyboard to the next open area and wraps after the last one"
                 (with-redefs [state/app-db (atom
                                              (assoc-in (areas-db) [:live-panel :focused?] false))]
                   (expect (= :chat (focus-of @state/app-db)))
                   (expect (true? (#'screen/cycle-focus! @state/app-db)))
                   (expect (= :panel (focus-of @state/app-db)))
                   (expect (#'screen/cycle-focus! @state/app-db))
                   (expect (= :sidebar (focus-of @state/app-db)))
                   (expect (false? (get-in @state/app-db [:live-panel :focused?])))
                   (expect (#'screen/cycle-focus! @state/app-db))
                   (expect (= :chat (focus-of @state/app-db)))))
             (it "does nothing while the chat is the only open area"
                 (with-redefs [state/app-db (atom (dissoc (frame-db) :live-panel))]
                   (expect (nil? (#'screen/cycle-focus! @state/app-db)))
                   (expect (= :chat (focus-of @state/app-db)))))
             (it "reads C-o, and no plain o, Alt+o or Tab, as the focus key"
                 (expect (keymap/focus-key? (KeyStroke. \o true false)))
                 (expect (not (keymap/focus-key? (cap/key-stroke \o))))
                 (expect (not (keymap/focus-key? (KeyStroke. \o true true))))
                 (expect (not (keymap/focus-key? (cap/key-stroke :tab)))))
             (it "keeps one focused area when the sidebar or the panel takes the focus"
                 (with-db (fn []
                            (swap! state/app-db merge (areas-db))
                            (state/dispatch [:project-sidebar {:focused? true}])
                            (expect (false? (get-in @state/app-db [:live-panel :focused?])))
                            (state/dispatch [:live-panel-focus])
                            (expect (true? (get-in @state/app-db [:live-panel :focused?])))
                            (expect (false? (get-in @state/app-db [:project-sidebar :focused?]))))))
             (it "in the focused panel, Tab still shows the next owner"
                 (with-redefs [state/app-db (atom (assoc (areas-db)
                                                    :live-views
                                                    (mapv lv/opened
                                                          [(view "a" "Deploy board" "deploy" :panel)
                                                           (view "b" "Job queue" "jobs" :panel)])))]
                   (expect (#'screen/live-panel-key! nil @state/app-db (cap/key-stroke :tab)))
                   (expect (= "jobs" (get-in @state/app-db [:live-panel :tab])))
                   (expect (= :panel (focus-of @state/app-db))))))

(defn- ink-at
  "The text colour of the first cell of `text` in a frame painted from `db`."
  [cols db text]
  (with-open [terminal
              (DefaultVirtualTerminal. (TerminalSize. cols 40))

              ts
              (doto (TerminalScreen. terminal) (.startScreen))]

    (with-redefs [timg/images-protocol
                  (constantly nil)

                  client/get-router
                  (constantly nil)]

      (#'screen/render-frame! ts cols 40 db 1000)
      (.refresh ts)
      (let [lines
            (mapv (fn [row]
                    (apply str
                      (map #(.getCharacterString (.getCharacter terminal (TerminalPosition. % row)))
                           (range cols))))
                  (range 40))

            row
            (first (keep-indexed #(when (str/includes? %2 text) %1) lines))]

        (str (.getForegroundColor
               (.getCharacter terminal
                              (TerminalPosition. (str/index-of (nth lines row) text) row))))))))

(defdescribe
  unfocused-fade-test
  (it "fades the area without the keyboard, and nothing while only the chat is open"
      (binding [interactions/hit-map (interactions/create-hit-map)]
        (let [panel-focused (frame-db)
              chat-focused (assoc-in (frame-db) [:live-panel :focused?] false)
              chat-only (dissoc (frame-db) :live-panel)
              ink (ink-at 160 chat-only "deploy is running")]

          (expect (not= ink (ink-at 160 panel-focused "deploy is running")))
          (expect (= ink (ink-at 160 chat-focused "deploy is running")))
          (expect (not= (ink-at 160 panel-focused "Deploy board")
                        (ink-at 160 chat-focused "Deploy board"))))))
  (it "does not fade a cell twice when no paint replaced it"
      (with-open [terminal
                  (DefaultVirtualTerminal. (TerminalSize. 20 4))

                  ts
                  (doto (TerminalScreen. terminal) (.startScreen))]

        (.putString (.newTextGraphics ts) 0 0 "chat text")
        (let [db
              (frame-db)

              fade!
              #(#'screen/fade-unfocused-areas! ts db 20 4 0 {:col 12 :row 0 :width 8 :height 4})

              painted
              (.getBackCharacter ts 0 0)

              _
              (fade!)

              once
              (.getBackCharacter ts 0 0)]

          (expect (= "c" (.getCharacterString once)))
          (expect (not= (.getForegroundColor painted) (.getForegroundColor once)))
          (fade!)
          (expect (identical? once (.getBackCharacter ts 0 0)))))))
