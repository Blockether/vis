(ns com.blockether.vis.ext.channel-tui.footer-test
  (:require [com.blockether.vis.ext.channel-tui.footer :as footer]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe build-segments-test
  (it "shows the selected reasoning level for reasoning-capable models"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-5"
                                                          :provider :openai
                                                          :reasoning? true})}
        (fn []
          (expect (= ["openai/gpt-5" "(Ctrl+T)" "reasoning: deep" "(Ctrl+R)"]
                    (->> (build-segments {:messages []
                                          :settings {:reasoning-level :deep}}
                           0)
                      (filter #(= :left (:region %)))
                      (mapv :text))))))))

  (it "shows Codex verbosity in the footer for the Codex provider"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-5.5"
                                                          :provider :openai-codex
                                                          :reasoning? true})}
        (fn []
          (expect (= ["openai-codex/gpt-5.5" "(Ctrl+T)"
                      "reasoning: balanced" "(Ctrl+R)"
                      "verbosity: high" "(Ctrl+L)"]
                    (->> (build-segments {:messages []
                                          :settings {:reasoning-level :balanced
                                                     :openai-codex-verbosity :high}}
                           0)
                      (filter #(= :left (:region %)))
                      (mapv :text))))))))

  (it "shows Codex dynamic quota windows on the second footer line"
    (let [build-limits-segments @#'footer/build-limits-segments
          now-ms                1000000000000
          report                {:dynamic {:limits [{:id :codex-5h
                                                     :label "Codex 5h quota (%)"
                                                     :remaining 76.0
                                                     :window {:resets-at-ms (+ now-ms (* 115 60 1000))}}
                                                    {:id :codex-7d
                                                     :label "Codex 7d quota (%)"
                                                     :remaining 85.0
                                                     :window {:resets-at-ms (+ now-ms (* (+ (* 3 24) 18) 60 60 1000))}}]}}]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-5.5"
                                                          :provider :openai-codex})}
        (fn []
          (let [text (->> (build-limits-segments {:messages []
                                                  :settings {}
                                                  :provider-limits {:provider-id :openai-codex
                                                                    :report report}}
                            now-ms)
                       (filter #(= :left (:region %)))
                       first
                       :text)]
            (expect (re-find #"Codex 5h 76% left ↺1h55m @" text))
            (expect (re-find #"7d 85% left ↺3d18h @" text)))))))

  (it "shows cumulative token totals in the right footer"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai
                                                          :reasoning? false})}
        (fn []
          (expect (= ["total ↑150 (cached 70) ↓45" "$0.015"]
                    (->> (build-segments {:messages [{:role :assistant
                                                      :tokens {:input 100 :output 30 :cached 60}
                                                      :cost {:total-cost 0.01}}
                                                     {:role :assistant
                                                      :tokens {:input 50 :output 15
                                                               :cached-input 10}
                                                      :cost {:total-cost 0.005}}]
                                          :settings {}}
                           0)
                      (filter #(= :right (:region %)))
                      (mapv :text))))))))

  (it "omits the reasoning suffix for non-reasoning models"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai
                                                          :reasoning? false})}
        (fn []
          (expect (= ["openai/gpt-4o" "(Ctrl+T)"]
                    (->> (build-segments {:messages []
                                          :settings {:reasoning-level :deep}}
                           0)
                      (filter #(= :left (:region %)))
                      (mapv :text))))))))

  (it "joins shortcuts to their labels without separator dots"
    (let [spans-width @#'footer/spans-width]
      (expect (= (count "model (Ctrl+T) · reasoning: deep (Ctrl+R)")
                (spans-width [{:text "model"}
                              {:text "(Ctrl+T)" :join-left? true}
                              {:text "reasoning: deep"}
                              {:text "(Ctrl+R)" :join-left? true}]
                  " · "))))))
