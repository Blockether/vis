(ns com.blockether.vis.tui.view-materializer-test
  (:require [com.blockether.vis.tui.view-materializer :as view]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe canvas-selection-test
             (it
               "clears selection when the owner turns it off"
               (let [before
                     {:id "v"
                      :seq 1
                      :nodes [{:id "board"
                               :type :canvas
                               :is-selectable true
                               :cards [{:id "a" :title "A"}]
                               :selected-ids ["a"]}]}

                     after
                     (view/apply-patch before
                                       {:view-id "v"
                                        :seq 2
                                        :ops [{:op :set :node-id "board" :is-selectable false}]})]

                 (expect (false? (get-in after [:nodes 0 :is-selectable])))
                 (expect (not (contains? (get-in after [:nodes 0]) :selected-ids))))))

(defdescribe board-patches-test
  (it "replays issue changes without losing their order or keeping stale selection"
    (let [a {:id "a" :title "First" :column "open"}
          b {:id "b" :title "Second" :column "open"}
          view {:id "v" :seq 0
                :nodes [{:id "work" :type :board :is-selectable true :selected-ids ["a"]
                         :columns [{:id "open" :title "Open"}] :issues [a b]}]}
          apply-op (fn [v op]
                     (view/apply-patch v {:view-id "v" :seq (inc (:seq v))
                                          :ops [(assoc op :node-id "work")]}))
          columns [{:id "open" :title "Ready"} {:id "next" :title "Next"}]
          updated (assoc a :title "Updated" :column "next")
          changed (-> view
                      (apply-op {:op :set :columns columns :is-editable true})
                      (apply-op {:op :append :issues [updated]}))
          removed (apply-op changed {:op :remove :item-ids ["a"]})
          replaced (apply-op changed {:op :replace :issues [b]})
          cleared (apply-op changed {:op :clear})
          node #(first (:nodes %))]
      (expect (= [updated b] (:issues (node changed))))
      (expect (= columns (:columns (node changed))))
      (expect (true? (:is-editable (node changed))))
      (expect (= ["a"] (:selected-ids (node changed))))
      (expect (= [b] (:issues (node removed)) (:issues (node replaced))))
      (expect (empty? (:issues (node cleared))))
      (expect (every? #(= [] (:selected-ids (node %))) [removed replaced cleared])))))

(defdescribe board-record-test
  (it "reads a saved board with its columns, issues, and selection"
    (let [board {:id "work" :type :board :label "Work" :is-selectable true
                 :columns [{:id "open" :title "Open" :tone :warn}]
                 :people [{:id "ada" :name "Ada"}]
                 :selected-ids ["a"]
                 :issues [{:id "a" :title "Write a test" :column "open" :assignee "ada"
                           :text "First line.\n\nSecond line."}]}
          saved (view/->markdown {:title "Record" :nodes [board]})
          read-back (get-in (view/parse-markdown saved) [:view :nodes 0])]
      (expect (= board read-back)))))
