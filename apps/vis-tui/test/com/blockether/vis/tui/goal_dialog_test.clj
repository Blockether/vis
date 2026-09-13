(ns com.blockether.vis.tui.goal-dialog-test
  "Exercise every Goal activation branch with the production dialog and virtual terminal."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.capture :as capture]
            [com.blockether.vis.tui.dialogs :as dialogs]
            [com.blockether.vis.tui.goals-test :as goals]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io PushbackReader]
           [java.util.concurrent.locks ReentrantLock]))

(defn- goal-handlers
  "Read the real key, release and click dispatch branches without starting a gateway."
  []
  (binding [*ns* (the-ns 'com.blockether.vis.tui.screen)]
    (with-open [reader (PushbackReader. (io/reader (io/resource
                                                     "com/blockether/vis/tui/screen.clj")))]
      (let [forms (doall (take-while #(not= ::eof %) (repeatedly #(read {:eof ::eof} reader))))]
        (vec (for [form (tree-seq coll? seq forms)
                   :when (seq? form)
                   [tag handler] (partition 2 1 form)
                   :when (and (= :footer-goal tag) (seq? handler))]

               (eval (list 'fn ['screen] handler))))))))

(defdescribe
  goal-dialog-render-ownership-test
  ;; Issue #215: all three Goal entry points used to bypass the modal draw lock.
  (it
    "freezes background painting while processing updates and restores live state on close"
    (let [handlers
          (goal-handlers)

          original-viewer
          dialogs/text-view-dialog!

          draw-lock
          ^ReentrantLock @#'screen/draw-lock]

      (expect (= 3 (count handlers)))
      (doseq [handler handlers]
        (let [db (atom {:active-tab-id "a"
                        :tabs [{:id "a"}]
                        :session {:id goals/session-id :goal goals/goal}
                        :render-version 0})
              updated (assoc goals/goal
                        "revision" 4
                        "iterations_used" 13)
              ownership (atom nil)
              frame (with-redefs [state/app-db db
                                  dialogs/text-view-dialog!
                                  (fn [terminal title lines]
                                    (reset! ownership [(:dialog-open? @db)
                                                       (.isHeldByCurrentThread draw-lock)])
                                    ;; A backend worker remains free to publish newer iterations.
                                    (expect (= :updated
                                               (deref (future (state/dispatch
                                                                [:sync-session-goal "a"
                                                                 {:session-id goals/session-id
                                                                  :goal updated}])
                                                              :updated)
                                                      2000
                                                      :timeout)))
                                    (expect (= updated (get-in @db [:session :goal])))
                                    (expect (false? (deref (future
                                                             (let [acquired? (.tryLock draw-lock)]
                                                               (when acquired? (.unlock draw-lock))
                                                               acquired?))
                                                           2000
                                                           :timeout)))
                                    (original-viewer terminal title lines))]

                      (capture/capture! {:cols 60
                                         :rows 20
                                         :keys [:down :down :down :down :esc]
                                         :paint! (fn [{:keys [screen]}]
                                                   (handler screen))}))]

          (expect (= [true true] @ownership))
          (expect (nil? (:error frame)))
          (expect (str/includes? (capture/frame-text frame :first) "Status: Active"))
          (expect (str/includes? (capture/frame-text frame) "Iterations: 12 / 30"))
          (expect (false? (:dialog-open? @db)))
          (expect (not (.isLocked draw-lock)))
          (expect (= updated (get-in @db [:session :goal])))
          (expect (pos? (:render-version @db))))))))
