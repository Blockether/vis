(ns com.blockether.vis.tui.session-model-dialog-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.projects :as projects]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [com.blockether.vis.tui.terminals :as term]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType MouseAction MouseActionType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal
            VirtualTerminalListener]))

(defn- back-lines
  [^TerminalScreen screen]
  (let [size (.getTerminalSize screen)]
    (mapv (fn [row]
            (apply str
              (for [col (range (.getColumns size))]
                (.getCharacterString (.getBackCharacter screen (int col) (int row))))))
          (range (.getRows size)))))

(defn- capture-model-picker!
  "Run the real picker and capture its terminal frames before each supplied input."
  [cols rows current opts on-flush]
  (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
        (term/virtual-screen)

        frames
        (atom [])

        geometry
        (atom nil)

        draw-chrome!
        dlg/draw-dialog-chrome!]

    (try (.setTerminalSize terminal (TerminalSize. (int cols) (int rows)))
         (frame/resize! screen)
         (let [left
               (if-let [offset (:column-offset opts)]
                 (offset cols rows)
                 0)

               g
               (.newTextGraphics screen)]

           (dotimes [row rows]
             (p/put-str! g
                         0
                         row
                         (str (apply str (repeat left \R)) (apply str (repeat (- cols left) \.))))))
         (.addVirtualTerminalListener terminal
                                      (reify
                                        VirtualTerminalListener
                                          (onFlush [_]
                                            (let [frame (assoc @geometry
                                                          :lines (back-lines screen)
                                                          :cursor (.getCursorPosition screen))]
                                              (swap! frames conj frame)
                                              (on-flush terminal frame (count @frames))))
                                          (onBell [_])
                                          (onClose [_])
                                          (onResized [_ _terminal _size])))
         (with-redefs [vis/picker-fleet
                       (constantly [{:id :test-provider :models ["model-alpha" "model-beta"]}])

                       vis/display-label
                       (constantly "Test provider")

                       dlg/draw-dialog-chrome!
                       (fn [g cols rows title content-w content-h]
                         (let [bounds (draw-chrome! g cols rows title content-w content-h)]
                           (reset! geometry
                             {:offset frame/*column-offset* :cols cols :rows rows :bounds bounds})
                           bounds))]

           (let [choice (dlg/model-picker! screen current opts)]
             {:choice choice :frames @frames}))
         (finally (.stopScreen screen)))))

(defn- call-session-picker!
  "Capture the session entry point's model preference, pane resolver and dispatched choice."
  [choice]
  (let [seen
        (atom nil)

        events
        (atom [])

        reads
        (atom [])]

    (with-redefs-fn {#'screen/with-dialog-lock (fn [f]
                                                 (f))
                     #'dlg/model-picker! (fn [_ current opts]
                                           (reset! seen {:current current :opts opts})
                                           choice)
                     #'vis/gateway-session-model-cached (fn [sid]
                                                          (swap! reads conj sid)
                                                          {:provider "test-provider"
                                                           :model "model-alpha"})
                     #'state/dispatch (fn [event]
                                        (swap! events conj event))}
      (fn []
        (#'screen/pick-session-model! nil)
        {:seen @seen :events @events :reads @reads}))))

(defdescribe
  session-model-pane-test
  ;; Regression: the session model dialog used the entire terminal and covered the project rail.
  (it "measures and paints the model list inside the owning session pane"
      (let [{:keys [^DefaultVirtualTerminal terminal ^TerminalScreen screen]}
            (term/virtual-screen)

            painted
            (atom nil)

            draw-chrome!
            dlg/draw-dialog-chrome!]

        (try (.setTerminalSize terminal (TerminalSize. 200 40))
             (frame/resize! screen)
             (let [g (.newTextGraphics screen)]
               (dotimes [row 40]
                 (p/put-str! g 0 row (apply str (repeat 80 \R)))))
             (.addInput terminal (KeyStroke. KeyType/Escape))
             (with-redefs [dlg/draw-dialog-chrome!
                           (fn [g cols rows title content-w content-h]
                             (reset! painted {:offset frame/*column-offset* :cols cols :rows rows})
                             (draw-chrome! g cols rows title content-w content-h))]
               (dlg/list-dialog! screen
                                 "Session model"
                                 [{:label "* router default"}]
                                 {:filter? true
                                  :height :content
                                  :column-offset (fn [_ _]
                                                   80)}))
             (expect (= {:offset 80 :cols 120 :rows 40} @painted))
             (expect (every? #(= (apply str (repeat 80 \R)) (subs % 0 80)) (back-lines screen)))
             (finally (.stopScreen screen)))))
  (it "uses the same dialog geometry as a standalone terminal the size of the session pane"
      (let [items
            [{:label "* router default"} {:label "Test provider / model-alpha"}]

            opts
            {:filter? true :height :content}

            scoped
            (dlg/select-modal-component "Session model"
                                        items
                                        (assoc opts
                                          :column-offset (fn [_ _]
                                                           80)))

            standalone
            (dlg/select-modal-component "Session model" items opts)

            actual
            ((:measure scoped) (:init scoped) 200 40)

            expected
            ((:measure standalone) (:init standalone) 120 40)]

        (expect (= 80 (:column-offset actual)))
        (expect (= 120 (:cols actual)))
        (expect (= (:bounds expected) (:bounds actual)))
        (expect (= (:list-h expected) (:list-h actual)))))
  (it "keeps the project rail visible and puts the filter cursor in the session pane"
      (let [{:keys [choice frames]}
            (capture-model-picker! 200
                                   40
                                   {:provider "test-provider" :model "model-alpha"}
                                   {:column-offset (fn [_ _]
                                                     80)}
                                   (fn [^DefaultVirtualTerminal terminal _ _]
                                     (.addInput terminal (KeyStroke. KeyType/Escape))))

            {:keys [offset cols bounds lines] :as painted}
            (first frames)

            ^TerminalPosition cursor
            (:cursor painted)]

        (expect (nil? choice))
        (expect (= [80 120] [offset cols]))
        (expect (every? #(= (apply str (repeat 80 \R)) (subs % 0 80)) lines))
        (expect (some #(str/includes? % "Session model") lines))
        (expect (some #(str/includes? % "current") lines))
        (expect (= (+ offset (:left bounds) 3) (.getColumn cursor)))
        (expect (= (+ (:top bounds) 3) (.getRow cursor)))))
  (it "filters and chooses a configured model through the scoped terminal dialog"
      (let [{:keys [choice]} (capture-model-picker! 200
                                                    40
                                                    nil
                                                    {:column-offset (fn [_ _]
                                                                      80)}
                                                    (fn [^DefaultVirtualTerminal terminal _ n]
                                                      (when (= 1 n)
                                                        (doseq [c "beta"]
                                                          (.addInput terminal (term/keystroke c)))
                                                        (.addInput terminal
                                                                   (KeyStroke. KeyType/Enter)))))]
        (expect (= {:provider "test-provider" :model "model-beta"}
                   (select-keys choice [:provider :model])))))
  (it "recomputes the session pane when a resize changes or removes the docked rail"
      (let [{:keys [frames]}
            (capture-model-picker!
              200
              40
              nil
              {:column-offset (fn [cols rows]
                                (:chat-left
                                  (projects/geometry {:project-sidebar {:open? true}} cols rows)))}
              (fn [^DefaultVirtualTerminal terminal _ n]
                (case n
                  1
                  (.setTerminalSize terminal (TerminalSize. 140 30))

                  2
                  (.setTerminalSize terminal (TerminalSize. 80 30))

                  (.addInput terminal (KeyStroke. KeyType/Escape)))))]
        (expect (= [[80 120 40] [56 84 30] [0 80 30]] (mapv (juxt :offset :cols :rows) frames)))))
  (it "closes only at the button's physical position, not its old unshifted position"
      (let [{:keys [choice frames]}
            (capture-model-picker!
              200
              40
              nil
              {:column-offset (fn [_ _]
                                80)}
              (fn [^DefaultVirtualTerminal terminal {:keys [offset bounds]} n]
                (.addInput terminal
                           (MouseAction. MouseActionType/CLICK_RELEASE
                                         1
                                         (TerminalPosition. (int (+ (dec (:right bounds))
                                                                    (if (= 1 n) 0 offset)))
                                                            (int (inc (:top bounds))))))
                (when (= 2 n) (.addInput terminal (KeyStroke. KeyType/Escape)))))]
        (expect (nil? choice))
        (expect (= 2 (count frames)))))
  (it "keeps global pickers full-screen when no session pane is supplied"
      (let [{:keys [choice frames]} (capture-model-picker!
                                      200
                                      40
                                      nil
                                      {}
                                      (fn [^DefaultVirtualTerminal terminal _ _]
                                        (.addInput terminal (KeyStroke. KeyType/Enter))))]
        (expect (:reset? choice))
        (expect (= [0 200] ((juxt :offset :cols) (first frames)))))))

(defdescribe session-model-routing-test
             (it "resolves the live session pane instead of using stale rendered dimensions"
                 (let [current
                       {:provider "test-provider" :model "model-beta"}

                       db
                       (atom {:session {:id "session-1"}
                              :session-model-pref current
                              :project-sidebar {:open? true}})]

                   (with-redefs [state/app-db db]
                     (let [{:keys [seen reads events]} (call-session-picker! current)
                           offset (get-in seen [:opts :column-offset])]

                       (expect (= current (:current seen)))
                       (expect (= [] reads))
                       (expect (= [[:set-model "test-provider" "model-beta"]] events))
                       (expect (= 80 (offset 200 40)))
                       (expect (= 56 (offset 140 30)))
                       (expect (= 0 (offset 80 30)))
                       (swap! db assoc-in [:project-sidebar :open?] false)
                       (expect (= 0 (offset 200 40)))
                       (swap! db assoc :project-sidebar {:open? true} :help-open? true)
                       (expect (= 0 (offset 200 40)))))))
             (it "preserves choosing, reset, cancellation and the existing-dialog guard"
                 (with-redefs [state/app-db (atom {:session {:id "session-1"}})]
                   (doseq [[choice expected] [[{:provider "test-provider" :model "model-beta"}
                                               [[:set-model "test-provider" "model-beta"]]]
                                              [{:reset? true} [[:set-model nil nil]]] [nil []]]]
                     (let [{:keys [seen events reads]} (call-session-picker! choice)]
                       (expect (= ["session-1"] reads))
                       (expect (= {:provider "test-provider" :model "model-alpha"} (:current seen)))
                       (expect (= expected events))))
                   (swap! state/app-db assoc :dialog-open? true)
                   (expect (= {:seen nil :events [] :reads []}
                              (call-session-picker! {:reset? true}))))))
