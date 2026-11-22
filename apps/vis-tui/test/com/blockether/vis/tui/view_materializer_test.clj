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
