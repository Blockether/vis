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

(defdescribe nudge-fn-test
  (it "returns a vector (sequential coll) so the host can flatten it"
    (let [out (nudges/nudge-fn {:conversation-title "Stable"
                                :title-refresh? false
                                :iteration 1
                                :input-tokens 100
                                :context-limit 200000})]
      (expect (vector? out))
      (expect (= [] out))))

  (it "emits both nudges when both conditions hold (blank title + context pressure)"
    (let [out (nudges/nudge-fn {:conversation-title nil
                                :title-refresh? false
                                :iteration 5
                                :input-tokens 150000
                                :context-limit 200000})]
      (expect (= 2 (count out)))
      (expect (= #{:low :high} (set (map :importance out))))))

  (it "drops nil entries"
    (let [out (nudges/nudge-fn {:conversation-title "Set"
                                :title-refresh? false
                                :iteration 1
                                :input-tokens 1
                                :context-limit 200000})]
      (expect (every? some? out)))))
