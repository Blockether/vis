(ns com.blockether.vis.ext.provider-openai-codex.limits-test
  (:require [com.blockether.vis.ext.provider-openai-codex.limits :as codex]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  usage->dynamic-limits-test
  (it "normalizes primary and secondary Codex usage windows"
      (let [report (codex/usage->dynamic-limits
                     {:rate_limit {:allowed true
                                   :limit_reached false
                                   :primary_window {:used_percent 40 :reset_after_seconds 3600}
                                   :secondary_window {:used_percent 10 :reset_at 2000}}}
                     {:id "gpt-5.3-codex"}
                     100000)]
        (expect (= [:codex-5h :codex-7d] (mapv :id (:limits report))))
        (expect (= 40.0 (get-in report [:limits 0 :used])))
        (expect (= 60.0 (get-in report [:limits 0 :remaining])))
        (expect (= 3700000 (get-in report [:limits 0 :window :resets-at-ms])))
        (expect (= 2000000 (get-in report [:limits 1 :window :resets-at-ms])))))
  (it "uses ChatGPT's explicit window duration instead of assuming primary means 5h"
      (let [report (codex/usage->dynamic-limits {:rate_limit {:allowed true
                                                              :limit_reached false
                                                              :primary_window
                                                              {:used_percent 10
                                                               :limit_window_seconds (* 7 24 60 60)
                                                               :reset_after_seconds 3600}}}
                                                {:id "gpt-5.3-codex"}
                                                100000)]
        (expect (= [:codex-7d] (mapv :id (:limits report))))
        (expect (= "Codex 7d quota (%)" (get-in report [:limits 0 :label])))
        (expect (= {:kind :rolling :unit :day :size 7 :resets-at-ms 3700000}
                   (get-in report [:limits 0 :window])))))
  (it "selects the nested Codex Spark bucket for the Spark model"
      (let [report (codex/usage->dynamic-limits
                     {:rate_limit {:primary_window {:used_percent 99}}
                      :additional_rate_limits
                      [{:limit_name "other" :rate_limit {:primary_window {:used_percent 80}}}
                       {:limit_name "GPT-5.3-Codex-Spark"
                        :rate_limit {:primary_window {:used_percent 25}
                                     :secondary_window {:used_percent 50}}}]}
                     {:id "gpt-5.3-codex-spark"}
                     0)]
        (expect (= 75.0 (get-in report [:limits 0 :remaining])))
        (expect (= 50.0 (get-in report [:limits 1 :remaining])))))
  (it "reports a note when no matching bucket exists"
      (let [report (codex/usage->dynamic-limits {} {:id "gpt-5.3-codex-spark"} 0)]
        (expect (= [] (:limits report)))
        (expect (= "OpenAI Codex usage endpoint did not return a matching quota bucket."
                   (:note report))))))
