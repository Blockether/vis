(ns com.blockether.vis.ext.channel-tui.footer-test
  (:require [com.blockether.vis.ext.channel-tui.footer :as footer]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.git :as git]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- sentinel-char?
  "True when `c` is a footer-unsafe sentinel codepoint: either a
   block-marker (Unicode invisible-format range \\u2061-\\u206F, or
   the markdown PUA range \\uE000-\\uE0FF) or an inline-span
   sentinel (\\uE110-\\uE119). `draw-spans!` writes characters
   verbatim into terminal cells, so any of these would leak as a
   stray blank column."
  [^Character c]
  (let [n (int c)]
    (or (<= 0x2061 n 0x206F)
      (<= 0xE000 n 0xE0FF)
      (p/inline-sentinel? (str c)))))

(defdescribe ir->footer-text-test
  ;; Regression for conversation 39a73cfb: footer hook IR was routed
  ;; through `lines->sentinel-strings`, which prepends
  ;; `MARKER_ANSWER_TXT` (\u206E). `draw-spans!` then wrote that
  ;; marker into a real terminal cell, showing as a leading blank
  ;; before "zai-coding/glm-5.1" in the second footer row.
  (let [ir->footer-text @#'footer/ir->footer-text]
    (it "strips block markers so plain-text IR yields plain text"
      (expect (= "zai-coding/glm-5.1"
                (ir->footer-text
                  [:ir {} [:p {} [:span {} "zai-coding/glm-5.1"]]]))))

    (it "never returns a leading sentinel character"
      (let [s (ir->footer-text
                [:ir {} [:p {} [:span {} "openai/gpt-5"]]])]
        (expect (pos? (count s)))
        (expect (not (sentinel-char? (.charAt ^String s 0))))))

    (it "strips inline style sentinels too (footer uses :bold? on seg, not IR)"
      (let [s (ir->footer-text
                [:ir {} [:p {} [:strong {} "bold"] [:span {} "plain"]]])]
        (expect (= "boldplain" s))
        (expect (every? #(not (sentinel-char? %)) s))))))

(defdescribe build-segments-test
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
          (expect (not-any? #(= "cancelling..." (:text %))
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
          report                {:provider-id :zai-coding-plan
                                 :dynamic {:limits [{:id :zai-coding-plan-5h
                                                     :label "Z.ai Coding 5h token quota"
                                                     :kind :tokens
                                                     :used 25.0
                                                     :limit 100.0
                                                     :remaining 75.0
                                                     :unlimited? false
                                                     :window {:resets-at-ms (+ now-ms (* 90 60 1000))}}
                                                    {:id :zai-coding-plan-7d
                                                     :label "Z.ai Coding 7d token quota"
                                                     :kind :tokens
                                                     :used 50.0
                                                     :limit 100.0
                                                     :remaining 50.0
                                                     :unlimited? false
                                                     :window {:resets-at-ms (+ now-ms (* 3 24 60 60 1000))}}]}}]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "glm-5.1"
                                                          :provider :zai-coding-plan})}
        (fn []
          (let [text (->> (build-limits-segments {:messages []
                                                  :settings {}
                                                  :provider-limits {:provider-id :zai-coding-plan
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

  (it "uses active workspace root for footer git status"
    (let [build-segments @#'footer/build-segments
          seen-root      (atom nil)]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai})
                       #'git/cached-workspace-status (fn
                                                       ([] {:workspace? false})
                                                       ([root]
                                                        (reset! seen-root (.getPath root))
                                                        {:workspace? true
                                                         :repo "vis"
                                                         :branch "feature/ws"
                                                         :modified 0
                                                         :created 0
                                                         :deleted 0
                                                         :upstream? false
                                                         :ahead 0
                                                         :behind 0}))}
        (fn []
          (expect (= ["git ~/vis (feature/ws)" "files: clean" "(no upstream)"]
                    (->> (build-segments {:messages []
                                          :settings {}
                                          :workspace/root "/tmp/vis-ws"} 0)
                      (filter #(= :right (:region %)))
                      (mapv :text))))
          (expect (= "/tmp/vis-ws" @seen-root))))))

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

  (it "joins shortcuts to their labels without separator dots"
    (let [spans-width @#'footer/spans-width]
      (expect (= (count "model (Ctrl+T) / reasoning: deep (Ctrl+R)")
                (spans-width [{:text "model"}
                              {:text "(Ctrl+T)" :join-left? true}
                              {:text "reasoning: deep"}
                              {:text "(Ctrl+R)" :join-left? true}]
                  " / "))))))

(defdescribe generic-limits-footer-text-test
  (it "shows a loading placeholder before the polling thread populates :provider-limits"
    (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
      (expect (= "limits: loading…"
                (generic-limits-footer-text {} :openai-codex 0)))))

  (it "shows a loading placeholder when the polled report is for a different provider"
    (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
      (expect (= "limits: loading…"
                (generic-limits-footer-text
                  {:provider-limits
                   {:provider-id :anthropic
                    :report {:provider-id :anthropic :status :ok
                             :dynamic {:limits []}}}}
                  :openai-codex 0)))))

  (it "surfaces the provider error message when the limits-fn failed"
    (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
      (expect (= "limits: error (boom)"
                (generic-limits-footer-text
                  {:provider-limits
                   {:provider-id :openai-codex
                    :report {:provider-id :openai-codex :status :error
                             :error {:type :provider/limits-error
                                     :message "boom"}
                             :dynamic {:limits []}}}}
                  :openai-codex 0)))))

  (it "asks for sign-in when the provider report is :unauthenticated"
    (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
      (expect (= "limits: sign in required"
                (generic-limits-footer-text
                  {:provider-limits
                   {:provider-id :openai-codex
                    :report {:provider-id :openai-codex :status :unauthenticated
                             :dynamic {:limits []}}}}
                  :openai-codex 0)))))

  (it "stays silent for providers that legitimately don't expose limits"
    (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
      (expect (nil? (generic-limits-footer-text
                      {:provider-limits
                       {:provider-id :openai-codex
                        :report {:provider-id :openai-codex :status :unsupported
                                 :dynamic {:limits []}}}}
                      :openai-codex 0))))))
