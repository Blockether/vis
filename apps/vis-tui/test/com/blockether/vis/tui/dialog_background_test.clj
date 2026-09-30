(ns com.blockether.vis.tui.dialog-background-test
  (:require [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.dialogs :as dlg]
            [com.blockether.vis.tui.frame :as frame]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal VirtualTerminalListener]
           [clojure.lang ExceptionInfo]))

(def small-lines (mapv #(str "Line " %) (range 10)))

(def large-lines (mapv #(str "Line " %) (range 38)))

(defn- paint-chat!
  [g]
  (p/set-colors! g t/dialog-hint t/terminal-bg)
  (p/fill-rect! g 0 0 120 60)
  (p/styled g
            [p/BOLD]
            (p/put-str! g 8 8 "Chat above the compact dialog")
            (p/put-str! g 8 50 "Chat below the compact dialog")))

(defn- capture-dialog
  [keys dialog!]
  (cap/capture! {:cols 120
                 :rows 60
                 :keys keys
                 :paint! (fn [{:keys [screen g]}]
                           (paint-chat! g)
                           (dialog! screen))}))

(defdescribe
  modal-background-lifecycle-test
  (describe
    "Changing dialog content"
    (it "restores the original cells and styles after a text refresh grows and shrinks"
        (let [refreshes
              (atom 0)

              capture
              (capture-dialog [\r \r :esc]
                              #(dlg/text-view-dialog! %
                                                      "Text" small-lines
                                                      :refresh-fn (fn []
                                                                    (if (odd? (swap! refreshes inc))
                                                                      large-lines
                                                                      small-lines))))

              restored?
              (= (first (:frames capture)) (last (:frames capture)))]

          (expect (nil? (:error capture)))
          (expect (= 3 (count (:frames capture))))
          (expect restored?
                  "Refreshing back to the original content must restore the whole frame")))
    (it "keeps changes outside the dialog footprint when its content refreshes"
        (let [capture (capture-dialog [\r :esc]
                                      (fn [^TerminalScreen screen]
                                        (dlg/text-view-dialog!
                                          screen
                                          "Text" small-lines
                                          :refresh-fn
                                          (fn []
                                            (p/put-str! (.newTextGraphics screen) 1 1 "Live update")
                                            small-lines))))]
          (expect (nil? (:error capture)))
          (expect (.contains ^String (cap/frame-text capture) "Live update")))))
  (describe "Shared modal driver"
            (it "keeps updates outside the painted footprint across common-driver frames"
                (let [capture (capture-dialog
                                [\a :esc]
                                (fn [screen]
                                  (dlg/run-modal!
                                    screen
                                    {:init 0
                                     :measure (fn [_ cols rows]
                                                {:cols cols :rows rows})
                                     :paint (fn [g state {:keys [cols rows]}]
                                              (when (zero? state) (p/put-str! g 1 1 "Live update"))
                                              (dlg/draw-dialog-chrome! g cols rows "Common" 8)
                                              nil)
                                     :on-key (fn [state _ _]
                                               (if (zero? state) 1 {::dlg/done nil}))})))]
                  (expect (nil? (:error capture)))
                  (expect (= 2 (count (:frames capture))))
                  (expect (.contains ^String (cap/frame-text capture) "Live update")))))
  (describe
    "Dialog transitions"
    (it "restores physical cells and cursor after a pane-scoped dialog"
        (let [position
              (TerminalPosition. 4 3)

              capture
              (capture-dialog [:esc]
                              (fn [^TerminalScreen screen]
                                (.setCursorPosition screen position)
                                (dlg/list-dialog! screen
                                                  "Right pane"
                                                  ["One" "Two"]
                                                  {:column-offset (fn [cols _rows]
                                                                    (quot cols 2))})
                                (frame/refresh! screen)
                                (.getCursorPosition screen)))

              reference
              (capture-dialog [] frame/refresh!)

              restored?
              (= (last (:frames reference)) (last (:frames capture)))]

          (expect (nil? (:error capture)))
          (expect (= 2 (count (:frames capture))))
          (expect restored? "Pane-local chrome must restore its physical screen footprint")
          (expect (= position (:ret capture)))))
    (it "restores a smaller parent after a larger child closes"
        (let [capture
              (capture-dialog [\r :esc :esc]
                              (fn [screen]
                                (dlg/text-view-dialog!
                                  screen
                                  "Parent" small-lines
                                  :refresh-fn (fn []
                                                (dlg/text-view-dialog! screen "Child" large-lines)
                                                small-lines))))

              restored?
              (= (first (:frames capture)) (last (:frames capture)))]

          (expect (nil? (:error capture)))
          (expect (= 3 (count (:frames capture))))
          (expect restored? "The child border and shadow must not remain around its parent")))
    (it "starts the next dialog on the original backdrop rather than the previous dialog"
        (let [capture
              (capture-dialog [:esc :esc]
                              (fn [screen]
                                (dlg/text-view-dialog! screen "Large" large-lines)
                                (dlg/text-input-dialog! screen "Small" "Name")))

              reference
              (capture-dialog [:esc] #(dlg/text-input-dialog! % "Small" "Name"))

              restored?
              (= (last (:frames reference)) (last (:frames capture)))]

          (expect (nil? (:error capture)))
          (expect (nil? (:error reference)))
          (expect (= 2 (count (:frames capture))))
          (expect restored? "Sequential dialogs must not retain the first dialog's frame")))
    (it
      "restores the backdrop on every direct local modal entry point"
      (let [reference
            (capture-dialog [] frame/refresh!)

            dialogs
            [["List" #(dlg/list-dialog! % "List" [{:label "One" :id :one}] {})]
             ["Table" #(dlg/table-view-dialog! % {:csv "Name,Value\nOne,1"})]
             ["Multiple choices" #(dlg/multi-select-dialog! % "Choices" ["One" "Two"])]
             ["Text" #(dlg/text-view-dialog! % "Text" small-lines)]
             ["Fullscreen log" #(dlg/log-view-dialog! % "Log" large-lines)]
             ["Text input" #(dlg/text-input-dialog! % "Input" "Name")]
             ["Flat input" #(dlg/text-input-dialog! % "Input" "Name" :flat? true)]
             ["Confirmation" #(dlg/confirm-dialog! % "Confirm" "Continue?")]
             ["Transient"
              #(dlg/transient-dialog!
                 %
                 "Commands"
                 []
                 {:groups [{:items [{:key "a" :type :action :id :choose :label "Choose"}]}]})]
             ["Text viewer" #(dlg/text-viewer-dialog! % "Text" "Long text")]
             ["Markdown viewer"
              #(dlg/markdown-viewer-dialog! % "Markdown" "## Heading\n\n**Text**")]
             ;; Keep the settings paint deterministic and off the gateway.
             ["Settings"
              (fn [screen]
                (with-redefs-fn {#'dlg/settings-rows (constantly [{:type :section :label "Options"}
                                                                  {:type :toggle
                                                                   :key :local-setting
                                                                   :label "Local setting"}])
                                 #'dlg/load-inventories! (constantly nil)}
                  #(dlg/settings-dialog! screen {})))]
             ["Session picker" #(dlg/session-picker-dialog! % [] nil)]
             ["Session navigator" #(dlg/navigator-dialog! % {:sessions []})]]]

        (expect (nil? (:error reference)))
        (doseq [[label dialog!] dialogs]
          (let [capture (capture-dialog [:esc]
                                        (fn [screen]
                                          (dialog! screen)
                                          (frame/refresh! screen)))
                restored? (= (last (:frames reference)) (last (:frames capture)))]

            (expect (nil? (:error capture)) label)
            (expect restored? (str label " must return the original cells and styles"))))))
    (it "restores the host after a parent clears the screen for a child"
        (let [capture
              (capture-dialog [\r :esc :esc]
                              (fn [screen]
                                (dlg/text-view-dialog!
                                  screen
                                  "Parent" small-lines
                                  :refresh-fn (fn []
                                                (dlg/clear-screen! screen)
                                                (dlg/text-view-dialog! screen "Child" large-lines)
                                                small-lines))))]
          (expect (nil? (:error capture)))
          (expect (= 4 (count (:frames capture))))
          (expect (= (first (:frames capture)) (last (:frames capture)))
                  "An explicit full-screen clear belongs to the parent's cleanup")))
    (it "restores the background and cursor when a refresh throws"
        (let [position
              (TerminalPosition. 4 3)

              capture
              (capture-dialog
                [\r]
                (fn [^TerminalScreen screen]
                  (.setCursorPosition screen position)
                  (let [failure (try (dlg/text-view-dialog! screen
                                                            "Text" small-lines
                                                            :refresh-fn
                                                            #(throw (ex-info "Refresh failed" {})))
                                     nil
                                     (catch ExceptionInfo e (.getMessage e)))]
                    (frame/refresh! screen)
                    {:failure failure :cursor (.getCursorPosition screen)})))

              reference
              (capture-dialog [] frame/refresh!)

              restored?
              (= (last (:frames reference)) (last (:frames capture)))]

          (expect (nil? (:error capture)))
          (expect (= "Refresh failed" (get-in capture [:ret :failure])))
          (expect (= position (get-in capture [:ret :cursor])))
          (expect restored? "Failure must not leave a dialog in the host back buffer"))))
  (describe "Rectangle restoration"
            (it "clips inclusive physical coordinates without restoring adjacent cells"
                (let [capture
                      (capture-dialog []
                                      (fn [^TerminalScreen screen]
                                        (let [restore!
                                              (dlg/frame-restorer screen)

                                              g
                                              (.newTextGraphics screen)]

                                          (p/set-colors! g t/dialog-border t/dialog-title-bg)
                                          (p/fill-rect! g 0 0 120 60)
                                          (p/put-str! g 8 9 "Keep outside")
                                          (binding [frame/*column-offset* 40]
                                            (restore! -20 8 8 8))
                                          (frame/refresh! screen))))

                      reference
                      (capture-dialog [] frame/refresh!)

                      restored?
                      (= (get-in reference [:frames 0 8 8]) (get-in capture [:frames 0 8 8]))]

                  (expect (nil? (:error capture)))
                  (expect restored?
                          "The boundary cell must retain its original character and styles")
                  (expect (= " " (get-in capture [:frames 0 8 9 :ch])))
                  (expect (.contains ^String (cap/frame-text capture) "Keep outside")))))
  (describe
    "Resize invalidation"
    (it
      "invalidates parent cells and cursor when a child grows and shrinks the terminal"
      (let [capture
            (cap/capture!
              {:cols 120
               :rows 60
               :paint! (fn [{:keys [^TerminalScreen screen g ^DefaultVirtualTerminal terminal]}]
                         (paint-chat! g)
                         (p/put-str! g 60 30 "Obsolete host cell")
                         (let [flushes (atom 0)]
                           (.addVirtualTerminalListener
                             terminal
                             (reify
                               VirtualTerminalListener
                                 (onFlush [_]
                                   (case (swap! flushes inc)
                                     1
                                     (.addInput terminal (cap/key-stroke \r))

                                     2
                                     (.setTerminalSize terminal (TerminalSize. 160 66))

                                     3
                                     (.setTerminalSize terminal (TerminalSize. 120 60))

                                     4
                                     (.addInput terminal (cap/key-stroke :esc))

                                     5
                                     (.addInput terminal (cap/key-stroke :esc))

                                     nil))
                                 (onBell [_])
                                 (onClose [_])
                                 (onResized [_ _terminal _size])))
                           (dlg/text-view-dialog! screen
                                                  "Parent" small-lines
                                                  :refresh-fn (fn []
                                                                (dlg/text-view-dialog! screen
                                                                                       "Child"
                                                                                       large-lines)
                                                                small-lines))
                           (frame/refresh! screen)
                           (.getCursorPosition screen)))})

            reference
            (cap/capture! {:cols 120
                           :rows 60
                           :keys [:esc]
                           :paint! (fn [{:keys [screen g]}]
                                     (p/set-bg! g t/terminal-bg)
                                     (p/fill-rect! g 0 0 120 60)
                                     (dlg/text-view-dialog! screen
                                                            "Parent" small-lines
                                                            :refresh-fn (constantly small-lines))
                                     (frame/refresh! screen))})

            parent-restored?
            (= (first (:frames reference)) (nth (:frames capture) 4 nil))

            host-restored?
            (= (last (:frames reference)) (last (:frames capture)))]

        (expect (nil? (:error capture)))
        (expect (nil? (:error reference)))
        (expect (= 6 (count (:frames capture))))
        (expect parent-restored? "The parent must repaint without the child's frame")
        (expect host-restored? "Returning to old dimensions must not restore obsolete host cells")
        (expect (nil? (:ret capture)) "A resize must invalidate the saved host cursor")))))
