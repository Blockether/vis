(ns com.blockether.vis.ext.foundation-core.hints-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation-core.hints :as hints]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe title-hint-test
  (it "emits a :critical hook-task when the title is blank AND we're on a cadence tick (iter 0 of turn 1 or every Nth turn)"
    (doseq [tp [1 hints/TITLE_REFRESH_TURN_PERIOD
                (* 2 hints/TITLE_REFRESH_TURN_PERIOD)
                (* 5 hints/TITLE_REFRESH_TURN_PERIOD)]]
      (let [n (hints/title-hint {:session-title nil
                                 :title-refresh? true
                                 :turn-position tp
                                 :iteration 0})]
        (expect (= :critical (:importance n)))
        (expect (str/includes? (:title n) "session title"))
        (expect (str/includes? (:title n) "set-session-title!"))
        (expect (str/includes? (:title n) "task-set! :vis.foundation/session-title")))))

  (it "stays silent on blank title at non-cadence turns even when refresh is flagged"
    (doseq [tp [2 3 4 5 6 7 8 9 11 19 21 99]]
      (expect (nil? (hints/title-hint {:session-title nil
                                       :title-refresh? true
                                       :turn-position tp
                                       :iteration 0})))))

  (it "stays silent on blank title when host did NOT flag :title-refresh? (mid-turn iterations)"
    (doseq [tp [1 10 20]]
      (expect (nil? (hints/title-hint {:session-title nil
                                       :title-refresh? false
                                       :turn-position tp
                                       :iteration 3})))))

  (it "fires on turn 1 (first turn) when host flags :title-refresh?"
    (let [n (hints/title-hint {:session-title "Refactor auth flow"
                               :title-refresh? true
                               :turn-position 1
                               :iteration 1})]
      (expect (= :info (:importance n)))
      (expect (str/includes? (:title n) "Refactor auth flow"))
      (expect (str/includes? (:title n) "set-session-title!"))
      (expect (str/includes? (:title n) "1 turn(s)"))))

  (it "fires on every TITLE_REFRESH_TURN_PERIOD-th turn when host flags :title-refresh?"
    (doseq [tp [hints/TITLE_REFRESH_TURN_PERIOD
                (* 2 hints/TITLE_REFRESH_TURN_PERIOD)
                (* 5 hints/TITLE_REFRESH_TURN_PERIOD)]]
      (let [n (hints/title-hint {:session-title "Triage 148 path failures"
                                 :title-refresh? true
                                 :turn-position tp
                                 :iteration 1})]
        (expect (some? n))
        (expect (str/includes? (:title n) (str tp " turn(s)")))
        (expect (str/includes? (:title n) "Triage 148 path failures")))))

  (it "stays silent on non-cadence turns even when refresh is flagged"
    (doseq [tp [2 3 4 5 6 7 8 9 11 19 21 99]]
      (expect (nil? (hints/title-hint {:session-title "Stable"
                                       :title-refresh? true
                                       :turn-position tp
                                       :iteration 1})))))

  (it "stays silent on cadence turn when host did not flag :title-refresh? (set OR blank)"
    (doseq [tp [1 hints/TITLE_REFRESH_TURN_PERIOD
                (* 2 hints/TITLE_REFRESH_TURN_PERIOD)]
            title ["Stable" nil ""]]
      (expect (nil? (hints/title-hint {:session-title title
                                       :title-refresh? false
                                       :turn-position tp
                                       :iteration 1})))))

  (it "never fires on iteration cadence (the old mod-N-iterations rule is gone)"
    (doseq [it [3 6 9 12 24 36 100]]
      (expect (nil? (hints/title-hint {:session-title "Stable"
                                       :title-refresh? false
                                       :turn-position 5
                                       :iteration it}))))))

(defdescribe context-pressure-hint-test
  (it "stays silent below the threshold"
    (expect (nil? (hints/context-pressure-hint
                    {:input-tokens 1000
                     :context-limit 200000}))))

  (it "fires at or above CONTEXT_PRESSURE_THRESHOLD with :warn importance"
    (let [limit 200000
          used  (long (* limit hints/CONTEXT_PRESSURE_THRESHOLD))
          n     (hints/context-pressure-hint {:input-tokens used
                                              :context-limit limit})]
      (expect (= :warn (:importance n)))
      (expect (str/includes? (:title n) "Converge now"))))

  (it "fires for 100k/200k (the z.ai GLM sweet-spot boundary)"
    (let [n (hints/context-pressure-hint {:input-tokens 100000
                                          :context-limit 200000})]
      (expect (some? n))
      (expect (str/includes? (:title n) "100000"))
      (expect (str/includes? (:title n) "200000"))))

  (it "is nil-safe when token/limit info is missing or zero"
    (expect (nil? (hints/context-pressure-hint {})))
    (expect (nil? (hints/context-pressure-hint {:input-tokens 0 :context-limit 200000})))
    (expect (nil? (hints/context-pressure-hint {:input-tokens 50000 :context-limit 0})))))

(defdescribe hooks-registration-test
  (it "foundation ships only the two iteration-start soft hooks"
    (let [ids (set (map :id hints/hooks))]
      (expect (= #{:vis.foundation/session-title
                   :vis.foundation/context-pressure}
                ids))))

  (it "every hook declares the four required keys (:id :doc :phase :fn)"
    (doseq [h hints/hooks]
      (expect (keyword? (:id h)))
      (expect (string? (:doc h)))
      (expect (= :turn.iteration/start (:phase h)))
      (expect (fn? (:fn h)))))

  (it "title hook returns hook-task shape with :title + :importance only"
    (let [h (some #(when (= :vis.foundation/session-title (:id %)) %) hints/hooks)
          hit ((:fn h) {:session-title nil :title-refresh? true
                        :turn-position 1 :iteration 0})]
      (expect (string? (:title hit)))
      ;; Hook tasks no longer carry a `:validator-fn`; the model
      ;; self-asserts done via `(task-set! id {:status :done})`.
      (expect (nil? (:validator-fn hit)))
      (expect (= #{:title :importance} (set (keys hit))))
      (expect (= :critical (:importance hit)))))

  (it "hooks return nil when their underlying condition is absent"
    (let [title-h    (some #(when (= :vis.foundation/session-title (:id %)) %) hints/hooks)
          pressure-h (some #(when (= :vis.foundation/context-pressure (:id %)) %) hints/hooks)]
      (expect (nil? ((:fn title-h)    {:session-title "Set" :title-refresh? false
                                       :turn-position 5 :iteration 1})))
      (expect (nil? ((:fn pressure-h) {:input-tokens 100 :context-limit 200000}))))))
