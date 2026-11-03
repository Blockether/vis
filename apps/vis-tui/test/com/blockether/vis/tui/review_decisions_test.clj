;; Review UI decisions, Council #9483.
(ns com.blockether.vis.tui.review-decisions-test
  (:require [com.blockether.vis.tui.app-surface :as surface]
            [com.blockether.vis.tui.app-surfaces :as surfaces]
            [com.blockether.vis.tui.live-view :as live]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe picker-eligibility
             (it "blocks modal reentry and requires a session only for session apps"
                 (doseq [[dialog? views session? sid expected]
                         [[true [{}] true "s" :blocked] [false [] false nil :empty]
                          [false [{}] true nil :empty] [false [{}] true "s" :ready]
                          [false [{}] false nil :ready]]]
                   (expect (= expected (surfaces/picker-state dialog? views session? sid))))))

(defdescribe
  picked-app-target
  (it "closes the shown pane, opens another pane, and keeps chat apps over chat"
      (doseq [[panel target expected] [[{:open? true :app {:view-id "a"}} "sidebar" :close]
                                       [{:open? false :app {:view-id "a"}} "sidebar" :pane]
                                       [{:open? true :app {:view-id "b"}} "sidebar" :pane]
                                       [{:open? true :app {:view-id "a"}} "chat" :chat]]]
        (expect (= expected
                   (surfaces/picked-target panel {:id "a" :surfaces {:session target}}))))))

(defdescribe save-completion
             (it "attaches accepted drafts without recreating an editor that closed during save"
                 (doseq [[editor outcome expected]
                         [[{:target {:node-id "d"}} {:is-accepted true :seq 2 :thread-id "t"}
                           {:node-id "d" :draft "saved" :thread-id "t"}]
                          [{:target {:node-id "d" :thread-id "old"}} {:is-accepted true :seq 2}
                           {:node-id "d" :thread-id "old" :draft "saved"}]
                          [{:target {:node-id "d"}} {:is-accepted true} {:node-id "d"}]
                          [nil {:is-accepted true :seq 2 :thread-id "t"} nil]]]
                   (let [result (#'surface/saved-editor "saved" {:editor editor} outcome)]
                     (expect (map? result))
                     (expect (= expected (get-in result [:editor :target])))))))

(defdescribe
  editor-cursor-window
  (it "scrolls the editor to its cursor and hides the cursor when the editor does not fit"
      (doseq [[crow ccol editor-rows content-h expected-first cursor?]
              [[0 0 4 12 0 true] [8 80 4 12 4 true] [0 0 4 4 0 false]]]
        (let [result (#'surface/editor-geometry
                      {:input {:crow crow :ccol ccol :lines (mapv str (range 12))}}
                      {:bounds {:left 2 :inner-w 20}
                       :content-top 3
                       :list-h 5
                       :legend "keys"
                       :editor-rows editor-rows
                       :content-h content-h})
              cursor (:cursor result)]

          (expect (= expected-first (:first-line result)))
          (expect (= (str expected-first) (first (:lines result))))
          (expect (= cursor? (some? cursor)))
          (when cursor
            (expect
              (<= (:left result) (.getColumn cursor) (+ (:left result) (dec (:width result)))))
            (expect (<= (inc (:top result)) (.getRow cursor) (+ (:top result) 5))))))))

(defdescribe natural-plan-width
             (it "reserves the width that a picture, scene, field, or styled row paints"
                 (doseq [[row expected]
                         [[{:kind :columns} 40] [{:kind :picture} 40]
                          [{:kind :field :text "abcdefghijklmnopqrstuvwxyz123456"} 35]
                          [{:kind :stats :items [{:label [{:text "界"}] :value [{:text "ok"}]}]} 8]
                          [{:kind :scene :frame {:width 7}} 7]
                          [{:segments [{:text "界x"}] :runs [{:text "ignored"}]} 3]
                          [{:runs [{:text "界"}]} 2] [{:text "界x" :indent 2 :search-label true} 20]]]
                   (expect (= expected (#'live/row-text-width 40 row))))))
