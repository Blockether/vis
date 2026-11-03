(ns com.blockether.vis.tui.live-panel-test
  "The panel placement: the side panel beside the chat."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.app-surface :as app-surface]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.human-input :as hi]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.input :as input]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.scroll :as scroll]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminal-image :as timg]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
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
  (it "takes half the chat columns, at least 48, and docks only with 60 columns left"
      (expect (= {:left 100 :width 100 :is-docked true :chat-cols 100} (lv/panel-geometry 200)))
      (expect (= {:left 42 :width 48 :is-docked false :chat-cols 90} (lv/panel-geometry 90)))
      (expect (= {:left 60 :width 60 :is-docked true :chat-cols 60} (lv/panel-geometry 120)))
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
                               (expect (= {:selected "a" :shown ["a"] :tab "deploy"} geometry))))))
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
                       (expect (= 80 (:cols layout)))
                       (expect (every? #(= \│ (nth % 80)) lines))
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
                       (expect (= \│ (nth (nth lines (inc top)) 42)))
                       (expect (not= \│ (nth (nth lines 1) 42))))))))

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
          (expect (identical? once (.getBackCharacter ts 0 0))))))
  ;; Perf pass 2026-10-31: a field log showed full frames with a 270 ms post phase, and
  ;; the fade made two colours and two cells for each unfocused cell of each frame.
  (it "reuses one faded cell for an equal cell that a new frame painted"
      (with-open [terminal
                  (DefaultVirtualTerminal. (TerminalSize. 20 4))

                  ts
                  (doto (TerminalScreen. terminal) (.startScreen))]

        (let [db
              (frame-db)

              fade!
              #(#'screen/fade-unfocused-areas! ts db 20 4 0 {:col 12 :row 0 :width 8 :height 4})

              paint!
              #(.putString (.newTextGraphics ts) 0 0 "chat text")]

          (paint!)
          (fade!)
          (let [once (.getBackCharacter ts 0 0)]
            (paint!)
            (expect (not (identical? once (.getBackCharacter ts 0 0))))
            (fade!)
            (expect (identical? once (.getBackCharacter ts 0 0)))))))
  (it "fades with the colours of the theme in use"
      (let [before
            @theme/active-theme-id

            faded-ink
            (fn [theme-id]
              (theme/apply-theme! theme-id)
              (with-open [terminal
                          (DefaultVirtualTerminal. (TerminalSize. 20 4))

                          ts
                          (doto (TerminalScreen. terminal) (.startScreen))]

                (.putString (.newTextGraphics ts) 0 0 "chat text")
                (#'screen/fade-unfocused-areas!
                 ts
                 (frame-db)
                 20
                 4
                 0
                 {:col 12 :row 0 :width 8 :height 4})
                (.getForegroundColor (.getBackCharacter ts 0 0))))]

        (try (expect (not= (faded-ink :vis-light) (faded-ink :vis-dark)))
             (finally (theme/apply-theme! before))))))

;; User report: returning to a session must keep its unfinished review.
(defdescribe
  review-pane-draft-return-test
  (it
    "keeps each app host's file, mode, position and comment editor"
    (let [pane-var
          #'screen/app-pane

          held
          @pane-var

          saved
          @held]

      (try (reset! held {})
           (with-redefs [state/app-db
                         (atom {:app-views {}})

                         app-surface/component
                         (fn [& _]
                           {:init {:selected 0 :scroll 0}})]

             (let [a
                   {:session-id "A" :view-id "review-A"}

                   b
                   {:session-id "B" :view-id "review-B"}

                   draft
                   {:selected 42
                    :scroll 30
                    :unified? false
                    :wrap? false
                    :editor {:text "Unsent A"}
                    :folded #{"a.clj"}}]

               (#'screen/app-pane-host nil a)
               (swap! held assoc-in [["A" "review-A"] :state] draft)
               (#'screen/app-pane-host nil b)
               (expect (= {:selected 0 :scroll 0} (:state (#'screen/app-pane-host nil b))))
               (expect (= draft (:state (#'screen/app-pane-host nil a))))))
           (finally (reset! held saved))))))

;; User report: switching sessions must work while a review draft is open.
(defdescribe review-pane-global-navigation-test
             (it "keeps C-x navigation outside the app while ordinary typing stays inside"
                 (let [db {:app-views {"v" {:view {:id "v" :nodes []}}}
                           :live-panel
                           {:open? true :focused? true :app {:session-id "A" :view-id "v"}}}]
                   (expect (false? (#'screen/app-pane-owns-key? db (KeyStroke. \x true false))))
                   (expect (true? (#'screen/app-pane-owns-key? db (KeyStroke. \a false false))))
                   (expect (false? (#'screen/app-pane-owns-key?
                                    (assoc-in db [:live-panel :focused?] false)
                                    (KeyStroke. \a false false)))))))

;; User report: unfinished reviews must survive typing and painting in other sessions.
(defdescribe
  review-pane-input-isolation-test
  (it
    "changes only the current session when a pane key moves the file selection"
    (let [held
          @#'screen/app-pane

          saved
          @held

          a
          {:session-id "A" :view-id "review-A"}

          panel
          {:open? true :focused? true :app a}

          view
          {:id "review-A" :nodes []}

          geom
          {:view view
           :list-h 10
           :rows [{:kind :file :path "a.py" :is-selectable true}
                  {:kind :file :path "next.py" :is-selectable true}]}

          component
          {:on-key #(app-surface/handle-key %1 %2 %3 {})}]

      (try (reset! held {["A" "review-A"]
                         {:state (app-surface/initial-state) :geom geom :component component}
                         ["B" "review-B"] {:state {:selected 7 :editor {:text "B draft"}}}})
           (with-redefs [state/app-db (atom {:live-panel panel
                                             :app-views {"review-A" {:view view}}})]
             (#'screen/app-pane-key! @state/app-db (KeyStroke. KeyType/ArrowDown))
             (expect (= 1 (get-in @held [["A" "review-A"] :state :selected])))
             (expect (= {:selected 7 :editor {:text "B draft"}}
                        (get-in @held [["B" "review-B"] :state])))
             (#'screen/app-pane-key! @state/app-db (KeyStroke. KeyType/Escape))
             (expect (false? (get-in @state/app-db [:live-panel :focused?])))
             (expect (= 1 (get-in @held [["A" "review-A"] :state :selected]))))
           (finally (reset! held saved)))))
  (it "keeps a newer comment draft when an older frame finishes painting"
      (let [held
            @#'screen/app-pane

            saved
            @held

            app
            {:session-id "A" :view-id "review-A"}

            newer
            {:selected 12 :editor {:text "Newer A draft"}}

            component
            {:measure-pane (fn [& _]
                             {:rows []})
             :reconcile (fn [st _]
                          st)
             :paint (fn [& _]
                      (swap! held assoc-in [["A" "review-A"] :state] newer))}]

        (try (reset! held {["A" "review-A"]
                           {:key ["A" "review-A"] :state {:selected 0} :component component}})
             (#'screen/paint-app-pane! nil nil {} app 0 0 80 40)
             (expect (= newer (get-in @held [["A" "review-A"] :state])))
             (expect (= {:left 0 :top 0 :width 80 :height 40}
                        (get-in @held [["A" "review-A"] :box])))
             (finally (reset! held saved))))))

;; User report: a cached review must return without waiting for the gateway.
(defdescribe
  review-session-offline-return-test
  (it
    "restores the retained pane before deferred metadata refresh"
    (let [step
          (fn [db event]
            (:db ((:fn (get @@#'state/event-registry (first event))) db event)))

          pane
          {:open? true :focused? true :app {:session-id "A" :view-id "review-A"}}

          a
          (step {:session {:id "A"} :render-version 0} [:open-session-tab {:id "A"} [] nil])

          b
          (step (assoc a :live-panel pane) [:open-session-tab {:id "B"} [] nil])

          seen
          (atom nil)]

      (with-redefs [state/app-db
                    (atom b)

                    chat/resume-session
                    (constantly nil)

                    client/notify!
                    (fn [& _])]

        (#'screen/select-session-target!
         "A"
         (fn [& _])
         (fn [_]
           (reset! seen (select-keys @state/app-db [:session :live-panel]))))
        (expect (= "A" (get-in @state/app-db [:session :id])))
        (expect (= pane (:live-panel @state/app-db)))
        (expect (= {:session {:id "A"} :live-panel pane} @seen))))))

(defdescribe review-session-load-test
             (describe
               "a session without a retained view"
               (doseq [pending? [false true]]
                 (it (if pending?
                       "hydrates a pending tab before showing its pane"
                       "loads a session that has not been opened")
                     (let [db {:session {:id "B"}
                               :active-tab-id :b
                               :tabs (cond-> [{:id :b :active? true}]
                                       pending?
                                       (conj {:id :a :pending? true :session-id "A"}))
                               :tab-locals (cond-> {}
                                             pending?
                                             (assoc :a {:session {:id "A"}}))
                               :render-version 0}
                           history [{:id "loaded" :role :user :content "Saved review work"}]]

                       (with-redefs [state/app-db (atom db)
                                     chat/resume-session (fn [_]
                                                           {:id "A" :history history})]

                         (#'screen/select-session-target!
                          "A"
                          (fn [session _]
                            (state/dispatch [:open-session-tab (dissoc session :history)
                                             (:history session) nil]))
                          (fn [& _]))
                         (expect (= "A" (get-in @state/app-db [:session :id])))
                         (expect (= history (:messages @state/app-db)))
                         (expect (not (:pending?
                                        (some #(when (= (:active-tab-id @state/app-db) (:id %)) %)
                                              (:tabs @state/app-db)))))))))))

;; User report: returning to A must not repaint B after the picker closes.
(defdescribe
  review-picker-close-frame-test
  (it
    "restores the selected pane before the dialog releases its drawing lock"
    (let [step
          (fn [db event]
            (:db ((:fn (get @@#'state/event-registry (first event))) db event)))

          pane
          {:open? true :focused? true :app {:session-id "A" :view-id "review-A"}}

          a
          (step {:session {:id "A"} :render-version 0} [:open-session-tab {:id "A"} [] nil])

          b
          (step (assoc a :live-panel pane) [:open-session-tab {:id "B"} [] nil])

          closing
          (atom nil)]

      (with-redefs [state/app-db
                    (atom b)

                    dlg/navigator-dialog!
                    (fn [& _]
                      {:action :switch :id "A"})]

        (with-redefs-fn {#'screen/with-dialog-lock
                         (fn [f]
                           (let [choice (f)]
                             (reset! closing (select-keys @state/app-db [:session :live-panel]))
                             choice))}
          (fn []
            (#'screen/show-session-picker!
             nil
             "B"
             b
             (fn [choice]
               (#'screen/select-session-target!
                (:id choice)
                (fn [& _])
                (fn [& _]))))))
        (expect (= {:session {:id "A"} :live-panel pane} @closing))))))

(defdescribe app-pane-hide-chord
             ;; User report: remove the close icon and hide the pane through C-x b p.
             (it "hides an app-only pane and reopens the same retained app"
                 (with-db
                   (fn []
                     (swap! state/app-db assoc
                       :app-views {"review" {:view {:id "review"}}}
                       :live-panel
                       {:open? true :focused? true :app {:session-id "s1" :view-id "review"}})
                     (#'screen/open-live-panel!)
                     (expect (false? (get-in @state/app-db [:live-panel :open?])))
                     (expect (false? (get-in @state/app-db [:live-panel :focused?])))
                     (expect (= "review" (get-in @state/app-db [:live-panel :app :view-id])))
                     (#'screen/open-live-panel!)
                     (expect (true? (get-in @state/app-db [:live-panel :open?])))
                     (expect (true? (get-in @state/app-db [:live-panel :focused?])))
                     (expect (= "review" (get-in @state/app-db [:live-panel :app :view-id])))))))
