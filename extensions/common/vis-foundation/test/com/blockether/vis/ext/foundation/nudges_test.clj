(ns com.blockether.vis.ext.foundation.nudges-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.nudges :as nudges]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe title-nudge-test
  (it "nudges when CONVERSATION_TITLE is blank"
    (let [n (nudges/title-nudge {:conversation-title nil
                                 :title-refresh? false
                                 :iteration 1})]
      (expect (= :low (:importance n)))
      (expect (str/includes? (:text n) "CONVERSATION_TITLE is currently empty"))))

  (it "nudges with current title on turn-boundary refresh"
    (let [n (nudges/title-nudge {:conversation-title "Refactor auth flow"
                                 :title-refresh? true
                                 :iteration 1})]
      (expect (= :low (:importance n)))
      (expect (str/includes? (:text n) "Refactor auth flow"))
      (expect (str/includes? (:text n) "refresh the title"))))

  (it "fires on iteration cadence (multiple of TITLE_REFRESH_NUDGE_PERIOD)"
    (let [n (nudges/title-nudge {:conversation-title "Triage 148 path failures"
                                 :title-refresh? false
                                 :iteration nudges/TITLE_REFRESH_NUDGE_PERIOD})]
      (expect (some? n))
      (expect (str/includes? (:text n) "iterations into this turn"))
      (expect (str/includes? (:text n) "Triage 148 path failures"))))

  (it "stays silent on non-cadence iteration with non-blank title and no refresh hint"
    (expect (nil? (nudges/title-nudge {:conversation-title "Stable"
                                       :title-refresh? false
                                       :iteration 3})))))

(defdescribe context-pressure-nudge-test
  (it "stays silent below the threshold"
    (expect (nil? (nudges/context-pressure-nudge
                    {:input-tokens 1000
                     :context-limit 200000}))))

  (it "fires at or above CONTEXT_PRESSURE_THRESHOLD"
    (let [limit 200000
          used  (long (* limit nudges/CONTEXT_PRESSURE_THRESHOLD))
          n     (nudges/context-pressure-nudge {:input-tokens used
                                                :context-limit limit})]
      (expect (= :high (:importance n)))
      (expect (str/includes? (:text n) "Context pressure"))
      (expect (str/includes? (:text n) "Converge now"))))

  (it "fires for 100k/200k (the z.ai GLM sweet-spot boundary)"
    (let [n (nudges/context-pressure-nudge {:input-tokens 100000
                                            :context-limit 200000})]
      (expect (some? n))
      (expect (str/includes? (:text n) "100000"))
      (expect (str/includes? (:text n) "200000"))))

  (it "is nil-safe when token/limit info is missing or zero"
    (expect (nil? (nudges/context-pressure-nudge {})))
    (expect (nil? (nudges/context-pressure-nudge {:input-tokens 0 :context-limit 200000})))
    (expect (nil? (nudges/context-pressure-nudge {:input-tokens 50000 :context-limit 0})))))

(defdescribe hooks-registration-test
  (it "foundation ships three hooks: title, context-pressure, blind-answer"
    (let [ids (set (map :id nudges/hooks))]
      (expect (= #{:foundation/conversation-title
                   :foundation/context-pressure
                   :foundation/blind-answer}
                ids))))

  (it "every hook declares the four required keys (:id :doc :phase :fn)"
    (doseq [h nudges/hooks]
      (expect (keyword? (:id h)))
      (expect (string? (:doc h)))
      (expect (contains? #{:session-start :turn-start :iteration-start :iteration-end :turn-end}
                (:phase h)))
      (expect (fn? (:fn h)))))

  (it "title hook adapts title-nudge into the {:hint :importance} shape"
    (let [h (some #(when (= :foundation/conversation-title (:id %)) %) nudges/hooks)
          hit ((:fn h) {:conversation-title nil :title-refresh? false :iteration 1})]
      (expect (string? (:hint hit)))
      (expect (= :low (:importance hit)))))

  (it "hooks return nil when their underlying condition is absent"
    (let [title-h    (some #(when (= :foundation/conversation-title (:id %)) %) nudges/hooks)
          pressure-h (some #(when (= :foundation/context-pressure (:id %)) %) nudges/hooks)]
      (expect (nil? ((:fn title-h)    {:conversation-title "Set" :title-refresh? false :iteration 1})))
      (expect (nil? ((:fn pressure-h) {:input-tokens 100 :context-limit 200000}))))))

(defdescribe blind-answer-guard-test
  (it "fires on iter 1 + investigation verb + no prior blocks"
    (doseq [req ["Why is the footer not showing?"
                 "Investigate the SCI sandbox setup"
                 "Fix the broken test"
                 "Check the conversation persistence"
                 "Find where v/cat is defined"
                 "Show me the loop preflight code"
                 "Debug iteration cancel"]]
      (let [hit (nudges/blind-answer-guard-check
                  {:iteration 1 :user-request req :previous-blocks nil})]
        (expect (some? hit))
        (expect (= :high (:importance hit)))
        (expect (string? (:text hit))))))

  (it "stays silent on trivial chat"
    (doseq [req ["hey" "thx" "siema" "Hi!" "ok" "yes"]]
      (expect (nil? (nudges/blind-answer-guard-check
                      {:iteration 1 :user-request req :previous-blocks nil})))))

  (it "stays silent on iteration 2+ even with investigation verbs"
    (expect (nil? (nudges/blind-answer-guard-check
                    {:iteration 2
                     :user-request "Why is X broken?"
                     :previous-blocks nil}))))

  (it "stays silent when previous-blocks is non-empty (model already observed)"
    (expect (nil? (nudges/blind-answer-guard-check
                    {:iteration 1
                     :user-request "Why is X broken?"
                     :previous-blocks [{:code "(v/cat \"x\")" :result {}}]})))))
