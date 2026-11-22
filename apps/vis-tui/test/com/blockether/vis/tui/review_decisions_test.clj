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
  (it "keeps the last line of a multiline draft visible when the editor shrinks"
      (doseq [[cols rows expected-first] [[80 24 "4"] [20 10 "8"]]]
        (let [view {:id "app" :nodes []}
              editor {:input {:crow 8 :ccol 80 :lines (mapv str (range 12))}}
              state (assoc (surface/initial-state) :editor editor)
              geom (surface/measure state cols rows view)
              {:keys [left top width lines cursor]} (#'surface/editor-geometry editor geom)]
          (expect (= expected-first (first lines)))
          (expect (= "8" (last lines)))
          (expect (<= left (.getColumn cursor) (+ left (dec width))))
          (expect (= (+ top (dec (count lines))) (.getRow cursor)))
          (expect (< (.getRow cursor) (- (get-in geom [:bounds :bottom]) 2)))))))

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
