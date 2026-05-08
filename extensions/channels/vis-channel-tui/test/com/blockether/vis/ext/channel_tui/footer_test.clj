(ns com.blockether.vis.ext.channel-tui.footer-test
  (:require [com.blockether.vis.ext.channel-tui.footer :as footer]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.git :as git]
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

  (it "hides reasoning effort for Z.ai fixed-thinking models"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "glm-4.7"
                                                          :provider :zai
                                                          :reasoning? true
                                                          :reasoning-style :zai-thinking
                                                          :reasoning-effort? false})}
        (fn []
          (expect (= ["zai/glm-4.7" "(Ctrl+T)"]
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

  (it "leaves voice recording status out of the footer because header owns channel statuses"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-5"
                                                          :provider :openai})}
        (fn []
          (let [voice (->> (build-segments {:messages []
                                            :settings {}
                                            :channel-status {:voice/input {:text "● Recording 00:01"
                                                                           :level :warn}}}
                             0)
                        (filter #(= "● Recording 00:01" (:text %)))
                        first)]
            (expect (nil? voice))
            (expect (nil? voice))
            (expect (nil? voice)))))))

  (it "leaves cancelling status out of the footer because notifications own it"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-5"
                                                          :provider :openai})
                       #'git/cached-workspace-status (fn [] {:workspace? false})}
        (fn []
          (expect (not-any? #(= "cancelling…" (:text %))
                    (build-segments {:messages []
                                     :settings {}
                                     :cancelling? true}
                      0)))))))

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
                                                          :provider :openai-codex})
                       #'git/cached-workspace-status (fn [] {:workspace? false})}
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
            (expect (re-find #"Codex 7d 85% left ↺3d18h @" text)))))))

  (it "shows Z.ai Coding Plan quota windows as percentages on the second footer line"
    (let [build-limits-segments @#'footer/build-limits-segments
          now-ms                1000000000000
          report                {:provider-id :zai-coding
                                 :dynamic {:limits [{:id :zai-coding-5h
                                                     :label "Z.ai Coding 5h token quota"
                                                     :kind :tokens
                                                     :used 25.0
                                                     :limit 100.0
                                                     :remaining 75.0
                                                     :unlimited? false
                                                     :window {:resets-at-ms (+ now-ms (* 90 60 1000))}}
                                                    {:id :zai-coding-7d
                                                     :label "Z.ai Coding 7d token quota"
                                                     :kind :tokens
                                                     :used 50.0
                                                     :limit 100.0
                                                     :remaining 50.0
                                                     :unlimited? false
                                                     :window {:resets-at-ms (+ now-ms (* 3 24 60 60 1000))}}]}}]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "glm-5.1"
                                                          :provider :zai-coding})}
        (fn []
          (let [text (->> (build-limits-segments {:messages []
                                                  :settings {}
                                                  :provider-limits {:provider-id :zai-coding
                                                                    :report report}}
                            now-ms)
                       (filter #(= :left (:region %)))
                       first
                       :text)]
            (expect (re-find #"Z\.ai Coding 5h token 75% left ↺1h30m" text))
            (expect (re-find #"Z\.ai Coding 7d token 50% left ↺3d0h" text)))))))

  (it "shows GitHub Copilot premium interaction utilization on the second footer line"
    (let [build-limits-segments @#'footer/build-limits-segments
          now-ms                1000000000000
          report                {:provider-id :github-copilot
                                 :dynamic {:limits [{:id :chat
                                                     :label "Chat"
                                                     :used 0.0
                                                     :limit 0.0
                                                     :remaining 0.0
                                                     :unlimited? false}
                                                    {:id :completions
                                                     :label "Completions"
                                                     :used 0.0
                                                     :limit 0.0
                                                     :remaining 0.0
                                                     :unlimited? false}
                                                    {:id :premium_interactions
                                                     :label "Premium interactions"
                                                     :used 60.0
                                                     :limit 300.0
                                                     :remaining 240.0
                                                     :unlimited? false
                                                     :window {:resets-at-ms (+ now-ms (* 2 24 60 60 1000))}}]}}]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "claude-opus-4-6"
                                                          :provider :github-copilot})}
        (fn []
          (let [text (->> (build-limits-segments {:messages []
                                                  :settings {}
                                                  :provider-limits {:provider-id :github-copilot
                                                                    :report report}}
                            now-ms)
                       (filter #(= :left (:region %)))
                       first
                       :text)]
            (expect (re-find #"Premium interactions 60/300 used \(240 left\) ↺2d0h" text)))))))

  (it "shows git repository state with one changed-file count on the first footer line right side"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai})
                       #'git/cached-workspace-status (fn [] {:workspace? true
                                                             :repo "vis"
                                                             :branch "main"
                                                             :modified 2
                                                             :created 3
                                                             :deleted 1
                                                             :upstream? true
                                                             :ahead 4
                                                             :behind 0})}
        (fn []
          (expect (= ["git ~/vis (main)" "6 modified" "commits: ⇡4"]
                    (->> (build-segments {:messages [] :settings {}} 0)
                      (filter #(= :right (:region %)))
                      (mapv :text))))))))

  (it "shows when the current directory is outside a git workspace on the first footer line"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai})
                       #'git/cached-workspace-status (fn [] {:workspace? false})}
        (fn []
          (let [spans (->> (build-segments {:messages [] :settings {}} 0)
                        (filter #(= :right (:region %))))]
            (expect (= ["No git"] (mapv :text spans)))
            (expect (= t/footer-error-fg (:fg (first spans))))
            (expect (true? (:bold? (first spans)))))))))

  (it "collapses clean file and synced commit state"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai})
                       #'git/cached-workspace-status (fn [] {:workspace? true
                                                             :repo "vis"
                                                             :branch "main"
                                                             :modified 0
                                                             :created 0
                                                             :deleted 0
                                                             :upstream? true
                                                             :ahead 0
                                                             :behind 0})}
        (fn []
          (expect (= ["git ~/vis (main)" "files: clean" "(up to date)"]
                    (->> (build-segments {:messages [] :settings {}} 0)
                      (filter #(= :right (:region %)))
                      (mapv :text))))))))

  (it "shows missing upstream distinctly from up-to-date commits"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai})
                       #'git/cached-workspace-status (fn [] {:workspace? true
                                                             :repo "vis"
                                                             :branch "main"
                                                             :modified 0
                                                             :created 0
                                                             :deleted 0
                                                             :upstream? false
                                                             :ahead 0
                                                             :behind 0})}
        (fn []
          (expect (= ["git ~/vis (main)" "files: clean" "(no upstream)"]
                    (->> (build-segments {:messages [] :settings {}} 0)
                      (filter #(= :right (:region %)))
                      (mapv :text))))))))

  (it "shows cumulative token and cost splits in the second footer row right side"
    (let [build-limits-segments @#'footer/build-limits-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai
                                                          :reasoning? false})}
        (fn []
          (expect (= ["total ↑150 (cached 70) ↓45"
                      "cost input ~$0.006000, input cached ~$0.002000, output ~$0.007000, total ~$0.015000"]
                    (->> (build-limits-segments {:messages [{:role :assistant
                                                             :tokens {:input 100 :output 30 :cached 60}
                                                             :cost {:total-cost 0.01
                                                                    :input-uncached-cost 0.004
                                                                    :input-cached-cost 0.001
                                                                    :output-cost 0.005}}
                                                            {:role :assistant
                                                             :tokens {:input 50 :output 15
                                                                      :cached-input 10}
                                                             :cost {:total-cost 0.005
                                                                    :input-uncached-cost 0.002
                                                                    :input-cached-cost 0.001
                                                                    :output-cost 0.002}}]
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
