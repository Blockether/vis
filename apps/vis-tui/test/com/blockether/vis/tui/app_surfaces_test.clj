(ns com.blockether.vis.tui.app-surfaces-test
  (:require [com.blockether.vis.tui.app-model :as model]
            [com.blockether.vis.tui.app-surface :as surface]
            [com.blockether.vis.tui.app-surfaces :as app-surfaces]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.keymap :as keymap]
            [com.blockether.vis.tui.live-view :as lv]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- app
  [id title surfaces & {:as extra}]
  (merge {:id id :title title :placement :attachment :seq 1 :nodes []}
         (when surfaces {:surfaces surfaces})
         extra))

(defdescribe
  surface-decisions-test
  (describe "where the session apps picker opens an app"
            (it "follows the session surface, and an absent or unknown value takes the default"
                (expect (= :sidebar (app-surfaces/session-target (app "a" "A" nil))))
                (expect (= :chat (app-surfaces/session-target (app "a" "A" {:session "chat"}))))
                (expect (nil? (app-surfaces/session-target (app "a" "A" {:session "off"}))))
                (expect (= :sidebar
                           (app-surfaces/session-target (app "a" "A" {:session "floating"}))))))
  (describe "the session apps picker"
            (it "lists open apps by title and leaves out off and closed apps"
                (let [views {"1" {:view (app "1" "zeta" {:session "chat"})}
                             "2" {:view (app "2" "Alpha" nil)}
                             "3" {:view (app "3" "Hidden" {:session "off"})}
                             "4" {:view (app "4" "Done" nil :is-closed true)}}]
                  (expect (= ["2" "1"] (mapv :id (app-surfaces/session-apps views))))
                  (expect (true? (keymap/verb-available? {:app-views views}
                                                         {:show-when :has-apps})))
                  (expect (false? (keymap/verb-available? {:app-views (select-keys views ["3" "4"])}
                                                          {:show-when :has-apps}))))))
  (describe
    "an app in its turn"
    (it "shows its frame and a Full screen button unless its inline surface is off"
        (expect (true? (app-surfaces/inline? (app "a" "A" nil))))
        (expect (false? (app-surfaces/inline? (app "a" "A" {:inline false}))))
        (expect (= " Full screen "
                   (app-surfaces/open-label {:media-type model/media-type :inline true})))
        (expect (= " Full screen " (app-surfaces/open-label {:media-type model/media-type})))
        (expect (= " Open " (app-surfaces/open-label {:media-type model/media-type :inline false})))
        (expect (= " Open " (app-surfaces/open-label {:media-type "text/plain" :inline true}))))
    ;; Council #9326: a durable app row lost its preview and showed Open after its block ended.
    (it "takes the inline choice of a finished turn from the held view, not the descriptor"
        (let [owner
              {:activity-id "act-1"}

              message
              {:role :assistant
               :traces [{:attachments [{"view_id" "v1" "media_type" model/media-type}]}]}

              other
              {:role :assistant :traces [{:attachments []}]}

              row
              (fn [surfaces]
                (->> (render/with-app-runs [message other]
                                           {"v1" {:view (app "v1" "Board" surfaces :owner owner)}})
                     first
                     :runs
                     (keep :app-row)
                     first))]

          (expect (= " Full screen " (app-surfaces/open-label (row nil))))
          (expect (= " Open " (app-surfaces/open-label (row {:inline false}))))
          (expect (identical? other
                              (second (render/with-app-runs
                                        [message other]
                                        {"v1" {:view (app "v1" "Board" nil :owner owner)}}))))))))

(defdescribe
  global-apps-test
  (describe
    "a fleet frame"
    (it "reaches the global apps only without a session, and a new connection reads them again"
        (expect (= [:refetch] (app-surfaces/fleet-app-frame {"type" "gateway.connected"})))
        (expect (nil? (app-surfaces/fleet-app-frame
                        {"type" "view.open" "kind" "live" "session_id" "s1" "view" {"id" "a"}})))
        (expect (nil? (app-surfaces/fleet-app-frame {"type" "view.open" "kind" "input" "view" {}})))
        (expect (= [:open {"id" "g"}]
                   (app-surfaces/fleet-app-frame
                     {"type" "view.open" "kind" "live" "view" {"id" "g"}})))
        (expect (= [:patch {"seq" 3} 2]
                   (app-surfaces/fleet-app-frame
                     {"type" "view.patch" "kind" "live" "patch" {"seq" 3} "first_seq" 2})))
        (expect (= [:close "g"]
                   (app-surfaces/fleet-app-frame
                     {"type" "view.close" "kind" "live" "view_id" "g"})))
        (expect (nil? (app-surfaces/fleet-app-frame {"type" "session.status" "session_id" "s1"})))))
  (describe
    "the held global apps"
    (it "follow the list read, mark a patch gap for a full read"
        (reset! state/app-db
          {:session {:id "s1"} :tabs [] :live-views [] :app-views {} :render-version 0})
        (state/dispatch [:global-apps-loaded [(app "g1" "Clock" nil :seq 2) (app "g2" "Mail" nil)]])
        (expect (= #{"g1" "g2"} (set (keys (:global-apps @state/app-db)))))
        (expect (true? (keymap/verb-available? @state/app-db {:show-when :has-global-apps})))
        (state/dispatch [:global-app-patch {:view-id "g1" :seq 6 :ops []} 5])
        (expect (true? (get-in @state/app-db [:global-apps "g1" :is-gap])))
        (state/dispatch [:global-app-close "g2"])
        (expect (= ["g1"] (mapv :id (app-surfaces/global-apps (:global-apps @state/app-db)))))
        (state/dispatch [:global-apps-loaded [(app "g1" "Clock" nil :seq 6)]])
        (expect (= ["g1"] (keys (:global-apps @state/app-db))))
        (expect (false? (get-in @state/app-db [:global-apps "g1" :is-gap])))
        (state/dispatch [:global-apps-loaded []])
        (expect (false? (keymap/verb-available? @state/app-db {:show-when :has-global-apps}))))))

(defn- painted-right
  "The last screen column that app `view` paints in `band` on a `cols`x`rows` terminal."
  [band cols rows view]
  (binding [dlg/*dialog-region* {:width (fn [c _]
                                          (app-surfaces/band-width band c))}]
    (let [geom
          (surface/measure (surface/initial-state) (app-surfaces/band-width band cols) rows view)]
      (+ (long (:left band)) (long (:right (:bounds geom)))))))

(defn- pane-geom
  "The side pane geometry of app `view` in chat pane `chat` on a `rows`-high terminal,
   placed as the screen places the side panel."
  [chat rows view]
  (let [{:keys [left width is-docked chat-cols]}
        (lv/panel-geometry (:cols chat))

        left
        (if is-docked chat-cols left)]

    (surface/pane-measure (surface/initial-state) left 0 width rows view)))

(defdescribe session-surface-width-test
             (it "keeps a session app inside the terminal at 40 and 120 columns (measured)"
                 (doseq [cols
                         [40 120]

                         chat
                         [{:left 0 :cols cols} {:left 12 :cols (- cols 12)}]

                         :let [view
                               (app "a" "A long app title that does not fit a narrow pane" nil)

                               pane
                               (:bounds (pane-geom chat 30 view))]]

                   (expect (< (+ (long (:left chat)) (long (:right pane))) cols)
                           (str "sidebar cols " cols " chat " chat))
                   (expect (<= (+ 20 3) (long (:inner-w pane)))
                           (str "pane rows fit, cols " cols " chat " chat))
                   (expect (< (long (painted-right (app-surfaces/chat-band chat) cols 30 view))
                              cols)
                           (str "chat cols " cols " chat " chat))))
             (it "narrows a band after the terminal shrinks"
                 (expect (= 30 (app-surfaces/band-width {:left 10 :width 110} 40)))))

(defdescribe
  side-pane-test
  (it "shows a held app before the live views, and nothing while closed"
      (let [views
            {"v1" {:view (app "v1" "A" nil)}}

            pane
            {:open? true :app {:session-id "s1" :view-id "v1"}}]

        (expect (= :app (app-surfaces/panel-content pane views [])))
        (expect (= :app (app-surfaces/panel-content pane views [{:id "p"}])))
        (expect
          (= :views
             (app-surfaces/panel-content (assoc-in pane [:app :view-id] "gone") views [{:id "p"}])))
        (expect (nil? (app-surfaces/panel-content (assoc-in pane [:app :view-id] "gone") views [])))
        (expect (nil? (app-surfaces/panel-content (assoc pane :open? false) views [{:id "p"}])))))
  (it "takes a click inside the pane to the row under it, and leaves clicks outside alone"
      (let [box
            {:left 72 :top 0 :width 48 :height 30}

            geom
            {:content-top 3 :list-h 20}]

        (expect (= {:row 4} (app-surfaces/pane-click box geom 2 10 80 5)))
        (expect (= {:row nil} (app-surfaces/pane-click box geom 0 10 80 1)))
        (expect (= {:row nil} (app-surfaces/pane-click box geom 0 3 80 9)))
        (expect (nil? (app-surfaces/pane-click box geom 0 10 71 5)))
        (expect (nil? (app-surfaces/pane-click box geom 0 10 120 5)))
        (expect (nil? (app-surfaces/pane-click nil geom 0 10 80 5)))))
  (it "closes the pane when the person picks the app that it shows"
      (let [pane {:open? true :app {:view-id "v1"}}]
        (expect (= :close (app-surfaces/pane-pick pane (app "v1" "A" nil))))
        (expect (= :open (app-surfaces/pane-pick pane (app "v2" "B" nil))))
        (expect (= :open (app-surfaces/pane-pick (assoc pane :open? false) (app "v1" "A" nil))))))
  (it "gives the keyboard back to the chat on Esc only after the app used its own Esc"
      (let [esc
            (com.googlecode.lanterna.input.KeyStroke. com.googlecode.lanterna.input.KeyType/Escape)

            view
            (app "v1" "A" nil)

            geom
            (surface/pane-measure (surface/initial-state) 0 0 48 30 view)

            focus-after
            #(app-surfaces/pane-focus-after (surface/handle-key % esc geom {}))]

        (expect (= :chat (focus-after (surface/initial-state))))
        (expect (= :panel (focus-after (assoc (surface/initial-state) :prefix true)))))))
