(ns com.blockether.vis.tui.footer-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.footer :as footer]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.client :as vis]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- fixture-seg?
  "No always-on right-side fixture segments remain; retained to keep git assertions focused."
  [_]
  false)

(defn- sentinel-char?
  "True when `c` is a footer-unsafe sentinel codepoint: either a
   block-marker (Unicode invisible-format range \\u2061-\\u206F, or
   the markdown PUA range \\uE000-\\uE0FF) or an inline-span
   sentinel (\\uE110-\\uE119). `draw-spans!` writes characters
   verbatim into terminal cells, so any of these would leak as a
   stray blank column."
  [^Character c]
  (let [n (int c)]
    (or (<= 0x2061 n 0x206F) (<= 0xE000 n 0xE0FF) (p/inline-sentinel? (str c)))))

(defdescribe
  footer-edge-alignment-test
  (it
    "aligns limits and Git without painting below the two status rows"
    (with-redefs-fn {#'footer/model-segments (fn [_ _ ^long _row]
                                               [])
                     #'footer/session-model-info (constantly {:reasoning-effort? false})}
      (fn []
        (doseq [cols
                [80 120 220]

                provider
                [nil :corp]]

          (with-redefs-fn {#'footer/session-effective-provider (constantly provider)}
            (fn []
              (let [db
                    {:messages [{:tokens {"input" 100 "output" 20} :cost {"total_cost" 0.0042}}]
                     :workspace
                     {"git" {"is_workspace" true "repo" "vis" "branch" "main" "is_upstream" true}}}

                    frame
                    (cap/capture! {:cols cols
                                   :rows 3
                                   :paint! (fn [{:keys [g]}]
                                             (p/put-str! g 0 2 (apply str (repeat cols ".")))
                                             (footer/draw-footer! g db 0 cols 0))})

                    [git-row limits-row untouched-row]
                    (mapv #(apply str (map :ch %)) (last (:frames frame)))]

                (expect (nil? (:error frame)))
                (expect (= 2 footer/height))
                (expect (= (apply str (repeat cols ".")) untouched-row))
                (expect (str/starts-with? limits-row "  Limits"))
                (expect (str/ends-with? git-row "git ~/vis (main)  "))
                (expect (str/ends-with? limits-row "~$0.0042  "))))))))))

(defdescribe
  passive-limits-test
  ;; Issue #204: paint cached quota states without callback IO, hover or click targets.
  (it "keeps every cached limits state passive at narrow and wide terminal sizes"
      (doseq [cols
              [40 80 120]

              status
              [nil :ok :loading :error :unauthenticated :unsupported]]

        (binding [interactions/hit-map (interactions/create-hit-map)]
          (with-redefs [vis/gateway-provider-limits
                        (fn [& _]
                          (throw (ex-info "Painting must not query provider limits" {})))]
            (let [db {:messages []
                      :settings {}
                      :session-model-pref {:provider "corp"}
                      :provider-limits {:provider-id :corp :report (when status {:status status})}}
                  frame (cap/capture! {:cols cols
                                       :rows 3
                                       :paint! (fn [{:keys [g]}]
                                                 (.beginFrame interactions/hit-map)
                                                 (footer/draw-footer! g db 0 cols 0)
                                                 (.commitFrame interactions/hit-map))})]

              (expect (nil? (:error frame)))
              (expect (str/includes? (cap/frame-text frame) "Limits"))
              (doseq [col (range cols)]
                (expect (nil? (.lookup interactions/hit-map col 1))))))))))

(defdescribe
  ir->footer-text-test
  ;; Footer hook IR was routed
  ;; through `lines->sentinel-strings`, which prepends
  ;; `MARKER_ANSWER_TXT` (\u206E). `draw-spans!` then wrote that
  ;; marker into a real terminal cell, showing as a leading blank
  ;; before "zai-coding/glm-5.1" in the second footer row.
  (let [ast->footer-text @#'footer/ast->footer-text]
    (it "strips block markers so plain-text IR yields plain text"
        (expect (= "zai-coding/glm-5.1"
                   (ast->footer-text [:ast {} [:p {} [:span {} "zai-coding/glm-5.1"]]]))))
    (it "strips inline style sentinels too (footer uses :bold? on seg, not IR)"
        (let [s (ast->footer-text [:ast {} [:p {} [:strong {} "bold"] [:span {} "plain"]]])]
          (expect (= "boldplain" s))
          (expect (every? #(not (sentinel-char? %)) s))))))

(defdescribe
  build-segments-test
  (it
    "shows Codex dynamic quota windows in the limits row"
    (let [build-limits-segments
          @#'footer/build-limits-segments

          now-ms
          1000000000000

          report
          {:dynamic {:limits [{:id :codex-5h
                               :label "Codex 5h quota (%)"
                               :remaining 76.0
                               :window {:resets-at-ms (+ now-ms (* 115 60 1000))}}
                              {:id :codex-7d
                               :label "Codex 7d quota (%)"
                               :remaining 85.0
                               :window {:resets-at-ms (+ now-ms
                                                         (* (+ (* 3 24) 18) 60 60 1000))}}]}}]

      (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                    {:name "gpt-5.5" :provider :openai-codex})}
        (fn []
          (let [text (->> (build-limits-segments {:messages []
                                                  :settings {}
                                                  :provider-limits {:provider-id :openai-codex
                                                                    :report report}}
                                                 now-ms)
                          first
                          :text)]
            (expect (re-find #"Codex 5h 76% ⟳1h55m@.* / 7d 85%" text))
            (expect (not (str/includes? text "Codex 7d")))
            ;; ONE reset stamp, on the leading window: a stamp per cell is what
            ;; pushed a three-window plan past the footer width.
            (expect (= 1 (count (re-seq #"⟳" text))))
            ;; the absolute half keeps its "EEE h:mm a" shape
            (expect (re-find #"⟳1h55m@[A-Z][a-z]{2} [0-9]{1,2}:[0-9]{2} [AP]M" text))
            (expect (not (re-find #"[0-9]:[0-5][0-9][ap]" text))))))))
  (it
    "keeps a visible Codex 5h window even when the provider omits its data"
    (let [build-limits-segments
          @#'footer/build-limits-segments

          now-ms
          1000000000000

          ;; Codex omitted the 5h window: a placeholder row with no usage
          ;; signal (no :remaining / :resets-at-ms). It must still render
          ;; beside the data-bearing 7d row, not be filtered away.
          report
          {:dynamic {:limits [{:id :codex-5h
                               :label "Codex 5h quota (%)"
                               :precision :unknown
                               :window {:kind :rolling :unit :hour :size 5}}
                              {:id :codex-7d
                               :label "Codex 7d quota (%)"
                               :remaining 81.0
                               :limit 100.0
                               :window {:resets-at-ms (+ now-ms
                                                         (* (+ (* 3 24) 18) 60 60 1000))}}]}}]

      (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                    {:name "gpt-5.5" :provider :openai-codex})}
        (fn []
          (let [text (->> (build-limits-segments {:messages []
                                                  :settings {}
                                                  :provider-limits {:provider-id :openai-codex
                                                                    :report report}}
                                                 now-ms)
                          first
                          :text)]
            (expect (re-find #"Codex 5h / 7d 81%" text)))))))
  ;; Regression, reported by a user against Claude: the footer sorted limit rows
  ;; by pressure alone, so a nearly-spent 7d window jumped in front of a fresh 5h
  ;; one and the line read "Claude 7d 12% … / 5h 90%". The short window leads.
  (it
    "keeps the Claude 5h window first even when the 7d window is tighter"
    (let [build-limits-segments
          @#'footer/build-limits-segments

          now-ms
          1000000000000

          report
          {:provider-id :anthropic-coding-plan
           :dynamic {:limits [{:id :claude-7d
                               :label "Claude 7d"
                               :kind :rate
                               :limit 100.0
                               :remaining 12.0
                               :window {:resets-at-ms (+ now-ms (* 6 24 60 60 1000))}}
                              {:id :claude-5h
                               :label "Claude 5h"
                               :kind :rate
                               :limit 100.0
                               :remaining 90.0
                               :window {:resets-at-ms (+ now-ms (* 5 60 60 1000))}}]}}]

      (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                    {:name "claude-opus-4-6"
                                                     :provider :anthropic-coding-plan})}
        (fn []
          (let [text (->> (build-limits-segments
                            {:messages []
                             :settings {}
                             :provider-limits {:provider-id :anthropic-coding-plan :report report}}
                            now-ms)
                          first
                          :text)]
            (expect (re-find #"Claude 5h 90% .* / 7d 12%" text)))))))
  (it
    "shows GitHub Copilot premium interaction utilization in the limits row"
    (let [build-limits-segments
          @#'footer/build-limits-segments

          now-ms
          1000000000000

          report
          {:provider-id :github-copilot
           :dynamic
           {:limits
            [{:id :chat :label "Chat" :used 0.0 :limit 0.0 :remaining 0.0 :is-unlimited false}
             {:id :completions
              :label "Completions"
              :used 0.0
              :limit 0.0
              :remaining 0.0
              :is-unlimited false}
             {:id :premium_interactions
              :label "Premium interactions"
              :used 60.0
              :limit 300.0
              :remaining 240.0
              :is-unlimited false
              :window {:resets-at-ms (+ now-ms (* 2 24 60 60 1000))}}]}}]

      (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                    {:name "claude-opus-4-6"
                                                     :provider :github-copilot})}
        (fn []
          (let [text (->> (build-limits-segments {:messages []
                                                  :settings {}
                                                  :provider-limits {:provider-id :github-copilot
                                                                    :report report}}
                                                 now-ms)
                          first
                          :text)]
            (expect (re-find #"Premium 60/300 \(240\) ⟳2d0h" text)))))))
  (it "shows draft identity and the gateway's aggregate live counts"
      ;; #241 and #247: use aggregate live changes, not primary-only Git or review counts.
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-4o" :provider :openai})}
          (fn []
            (doseq [[identity expected] [[{"label" "api-and-sdk"} "api-and-sdk"]
                                         [{"id" "12345678-abcd"} "12345678"]]]
              (let [texts (->> (build-segments {:messages []
                                                :settings {}
                                                :workspace
                                                (merge {"root" "/internal/isolated-copy"
                                                        "is_draft" true
                                                        "working_changes"
                                                        {"modified" 2 "created" 3 "deleted" 1}
                                                        "ahead" 4
                                                        "draft_changes" {"modified" 88}
                                                        "git" {"is_workspace" true "modified" 99}}
                                                       identity)}
                                               0)
                               (filter #(= :right (:region %)))
                               (remove fixture-seg?)
                               (mapv :text))]
                (expect (= [(str " DRAFT (" expected) "~2 +3 -1 ↑4)"] texts))))))))
  (it "renders the gateway :git fact even when the top-level root was lost"
      ;; A stale tab snapshot can null the denormalized `:workspace/root`, but the
      ;; git fact still rides on the session's `:workspace` record — the footer
      ;; renders straight from it, never falling back to the vis process cwd.
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-4o" :provider :openai})}
          (fn []
            (let [db {:messages []
                      :settings {}
                      :workspace {"git" {"is_workspace" true
                                         "repo" "demo"
                                         "branch" "main"
                                         "modified" 0
                                         "created" 0
                                         "deleted" 0
                                         "is_upstream" true
                                         "ahead" 0
                                         "behind" 0}}}]
              (expect (= [" git ~/demo (main)"]
                         (->> (build-segments db 0)
                              (filter #(= :right (:region %)))
                              (remove fixture-seg?)
                              (mapv :text)))))))))
  (it "collapses clean file and synced commit state"
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-4o" :provider :openai})}
          (fn []
            (expect
              (= [" git ~/vis (main)"]
                 (->> (build-segments {:messages []
                                       :settings {}
                                       :workspace {"root" "/tmp/vis"
                                                   "git" {"is_workspace" true
                                                          "repo" "vis"
                                                          "branch" "main"
                                                          "modified" 0
                                                          "created" 0
                                                          "deleted" 0
                                                          "is_upstream" true
                                                          "ahead" 0
                                                          "behind" 0}}}
                                      0)
                      (filter #(= :right (:region %)))
                      (remove fixture-seg?)
                      (mapv :text))))))))
  (it "shows missing upstream distinctly from up-to-date commits"
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-4o" :provider :openai})}
          (fn []
            (expect
              (= [" git ~/vis (main —)"]
                 (->> (build-segments {:messages []
                                       :settings {}
                                       :workspace {"root" "/tmp/vis"
                                                   "git" {"is_workspace" true
                                                          "repo" "vis"
                                                          "branch" "main"
                                                          "modified" 0
                                                          "created" 0
                                                          "deleted" 0
                                                          "is_upstream" false
                                                          "ahead" 0
                                                          "behind" 0}}}
                                      0)
                      (filter #(= :right (:region %)))
                      (remove fixture-seg?)
                      (mapv :text))))))))
  (it "hides the verbosity knob when the session's model rejects the field"
      ;; Regression: the chip was gated on the provider being `:openai-codex`, so a
      ;; Claude session inherited the knob whenever the GLOBAL router default
      ;; happened to be Codex, and a Copilot GPT session never got it at all.
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/session-model-info (fn [_]
                                                       {:name "claude-opus-4-8"
                                                        :provider :github-copilot})}
          (fn []
            (let [texts (->> (build-segments {:messages []
                                              :settings {:verbosity "high"}
                                              :session-model-pref {:provider "github-copilot"
                                                                   :model "claude-opus-4-8"}}
                                             0)
                             (mapv :text))]
              (expect (not-any? #(str/starts-with? % "verbosity: ") texts)))))))
  (it "uses gateway model details to show Astra verbosity only on a supported session"
      ;; Regression, Vis session 95c4a9b0-ba88-4e8d-86ef-252cb522bcf4: exercise
      ;; the real client decoder and resolver rather than stubbing model capabilities.
      (with-redefs [vis/router-cached (constantly
                                        [{"id" "openai-codex"
                                          "is_default" true
                                          "default_model" "gpt-6-astra"
                                          "models" ["gpt-6-astra"]
                                          "model_details" [{"name" "gpt-6-astra"
                                                            "is_reasoning_effort_configurable" true
                                                            "verbosity_style" "openai-text"}]}
                                         {"id" "github-copilot"
                                          "models" ["gpt-6-astra" "claude-opus-5"]
                                          "model_details" [{"name" "gpt-6-astra"
                                                            "is_reasoning_effort_configurable" true
                                                            "verbosity_style" "openai-text"}
                                                           {"name" "claude-opus-5"
                                                            "is_reasoning_effort_configurable" true
                                                            "verbosity_style" nil}]}])]
        (doseq [[provider model expected] [["openai-codex" "gpt-6-astra" ["verbosity: high"]]
                                           ["github-copilot" "gpt-6-astra" ["verbosity: high"]]
                                           ["github-copilot" "claude-opus-5" []]]]
          (let [db {:messages []
                    :settings {:verbosity "high"}
                    :session-model-pref {:provider provider :model model}}
                texts (mapv :text (#'footer/build-segments db 0))]

            (expect (= expected (filterv #(str/starts-with? % "verbosity: ") texts)))))))
  (it "uses gateway model details to show the thinking summary only for adaptive Claude"
      (with-redefs [vis/router-cached
                    (constantly [{"id" "anthropic"
                                  "is_default" true
                                  "default_model" "claude-opus-5"
                                  "models" ["claude-opus-5"]
                                  "model_details" [{"name" "claude-opus-5"
                                                    "is_reasoning_effort_configurable" true
                                                    "verbosity_style" nil
                                                    "thinking_display_style" "anthropic-display"}]}
                                 {"id" "zai-coding-plan"
                                  "models" ["glm-5.2"]
                                  "model_details" [{"name" "glm-5.2"
                                                    "is_reasoning_effort_configurable" true
                                                    "verbosity_style" nil
                                                    "thinking_display_style" nil}]}])]
        (doseq [[provider model session-settings expected]
                [["anthropic" "claude-opus-5" nil [" thinking: summarized (C-x x) "]]
                 ["anthropic" "claude-opus-5" {:thinking-summary false}
                  [" thinking: omitted (C-x x) "]] ["zai-coding-plan" "glm-5.2" nil []]]]
          (let [db {:messages []
                    :session {:id "s1"}
                    :session-settings {"s1" session-settings}
                    :session-model-pref {:provider provider :model model}}
                texts (mapv :text (#'footer/build-segments db 0))]

            (expect (= expected (filterv #(str/starts-with? % " thinking: ") texts)))
            (expect (= (if (seq expected) [:footer-thinking] [])
                       (->> (#'footer/build-segments db 0)
                            (filter #(str/starts-with? (str (:text %)) " thinking: "))
                            (mapv :kind))))))))
  (it "reads capability off the SESSION's model, not the global router default"
      ;; Regression: opening a GitHub Copilot session offered no way to change
      ;; reasoning, because the footer asked the router's DEFAULT model instead
      ;; of the model this session routes to.
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/session-model-info (fn [_]
                                                       {:name "gpt-5.6-sol"
                                                        :provider :github-copilot
                                                        :reasoning? true
                                                        :reasoning-style :openai-effort
                                                        :reasoning-effort? true
                                                        :verbosity-style :openai-text})}
          (fn []
            (let [texts (->> (build-segments {:messages []
                                              :settings {:reasoning-level "deep"
                                                         :verbosity "medium"}
                                              :session-model-pref {:provider "github-copilot"
                                                                   :model "gpt-5.6-sol"}}
                                             0)
                             (mapv :text))]
              (expect (= [" reasoning: deep (C-x r) " "verbosity: medium"]
                         (filterv #{" reasoning: deep (C-x r) " "verbosity: medium"} texts))))))))
  (it "resolves the SESSION's model, falling back to the router root without a pick"
      ;; The plumbing under the chips: the same GitHub Copilot provider serves an
      ;; Anthropic wire for Claude and a Responses wire for GPT, so asking the
      ;; router's DEFAULT model answers about a model this session never uses.
      (let [session-model-info
            @#'footer/session-model-info

            default-model
            {:name "glm-4.7" :provider :zai}

            copilot-gpt
            {:name "gpt-5.6-sol" :provider :github-copilot}]

        (with-redefs-fn {#'vis/get-router (constantly :router)
                         #'vis/gateway-session-model-cached (constantly nil)
                         #'vis/resolve-effective-model (constantly default-model)
                         #'vis/resolve-model-info
                         (fn [_ provider _model]
                           (if (= "github-copilot" provider) copilot-gpt default-model))}
          (fn []
            (expect (= copilot-gpt
                       (session-model-info {:session {:id "s1"}
                                            :session-model-pref {:provider "github-copilot"
                                                                 :model "gpt-5.6-sol"}})))
            (expect (= default-model (session-model-info {:session {:id "s1"}}))))))))

(defdescribe
  shrink-to-fit-test
  (let [shrink
        @#'footer/shrink-to-fit

        total
        @#'footer/total-width

        fits?
        (fn [[segs sepa] cols]
          (<= (total segs sepa) cols))

        row
        [{:text "openai-codex/gpt-5.5 (C-x c) (cycle 1/3 C-x m)" :region :left :priority 2}
         {:text "reasoning: deep" :region :left :priority 3}
         {:text " resources 0 (C-x b) " :region :right :priority 2}]

        limits
        [{:text "limits: 5h 1200/2000  7d 40000/50000 resets in 3h" :region :left :priority 1}]]

    (it "keeps every segment untouched when the row already fits"
        (expect (= [row "  /  "] (shrink row 200))))
    (it "drops the least-important segment before touching the critical ones"
        (let [[segs _] (shrink row 60)]
          (expect (not (some #(= "reasoning: deep" (:text %)) segs)))
          (expect (some #(str/starts-with? (:text %) "openai-codex/") segs))))
    (it "truncates the most-important tier with an ellipsis rather than dropping it"
        (doseq [cols [40 24 12 6]]
          (let [result (shrink row cols)]
            (expect (fits? result cols))
            (expect (seq (first result))))))
    (it "compacts a lone priority-1 segment instead of letting it overflow"
        (doseq [cols [40 20 8]]
          (let [[segs _ :as result] (shrink limits cols)]
            (expect (fits? result cols))
            (expect (= 1 (count segs)))
            (expect (str/ends-with? (:text (first segs)) "…")))))))

(defdescribe
  generic-limits-footer-text-test
  (it "shows a loading placeholder when the polled report is for a different provider"
      (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
        (expect (= "limits: loading…"
                   (generic-limits-footer-text {:provider-limits {:provider-id :anthropic
                                                                  :report {:provider-id :anthropic
                                                                           :status :ok
                                                                           :dynamic {:limits []}}}}
                                               :openai-codex
                                               0)))))
  ;; Per-provider memory: the active poller slot still belongs to the
  ;; provider we just switched AWAY from, but we already know this
  ;; provider's report — render it instead of blanking to "loading…"
  ;; on every C-x c cycle.
  (it "renders the remembered report for the provider while a switch refetch is in flight"
      (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
        (expect (= "limits: sign in required"
                   (generic-limits-footer-text
                     {:provider-limits {:provider-id :anthropic
                                        :report
                                        {:provider-id :anthropic :status :ok :dynamic {:limits []}}}
                      :provider-limits-cache {:openai-codex {:provider-id :openai-codex
                                                             :report {:provider-id :openai-codex
                                                                      :status :unauthenticated
                                                                      :dynamic {:limits []}}}}}
                     :openai-codex
                     0))))))

(defdescribe
  draft-summary-test
  ;; #247: use live Git-style counts, not the immutable task-review snapshot.
  (it "shows only nonzero live counts and preserves unavailable and recovery states"
      (doseq [[fields expected] [[{"working_changes" {"modified" 2 "created" 3 "deleted" 1}
                                   "ahead" 4} [" DRAFT (multi" "~2 +3 -1 ↑4)"]]
                                 [{"working_changes" {"modified" 0 "created" 0 "deleted" 0}
                                   "ahead" 0} [" DRAFT (multi)"]]
                                 [{"working_changes" {"modified" 0 "created" 2 "deleted" 0}
                                   "ahead" 0} [" DRAFT (multi" "+2)"]]
                                 [{} [" DRAFT (multi" "CHANGES UNAVAILABLE)"]]
                                 [{"working_changes" {"modified" 0} "ahead" 0}
                                  [" DRAFT (multi" "CHANGES UNAVAILABLE)"]]
                                 [{"working_changes" {"modified" 0 "created" 0 "deleted" 0}}
                                  [" DRAFT (multi" "CHANGES UNAVAILABLE)"]]
                                 [{"working_changes" {"modified" 0 "created" 0 "deleted" 0}
                                   "ahead" 0
                                   "draft_error" "Cannot read review snapshot"} [" DRAFT (multi)"]]
                                 [{"recovery_required" true
                                   "working_changes" {"modified" 0 "created" 0 "deleted" 0}
                                   "ahead" 0} [" DRAFT (multi" "RECOVERY REQUIRED)"]]]]
        (expect (= expected
                   (mapv :text (#'footer/draft-footer-spans (assoc fields "label" "multi")))))))
  (it "keeps the change state visible when a long draft name is shortened"
      (let [segments
            (#'footer/draft-footer-spans
             {"label" "finish-dev-env-live-view-with-a-long-name"
              "working_changes" {"modified" 2 "created" 3 "deleted" 1}
              "ahead" 4})

            [fitted _]
            (#'footer/shrink-to-fit segments 40)]

        (expect (str/starts-with? (:text (first fitted)) " DRAFT ("))
        (expect (= "~2 +3 -1 ↑4)" (:text (last fitted))))))
  (it "uses canonical draft identity rather than treating the fork timestamp as a UI flag"
      (with-redefs-fn {#'footer/session-model-info (constantly {:reasoning-effort? false})}
        (fn []
          (doseq [[workspace expected]
                  [[{"is_draft" true "label" "feature" "recovery_required" true}
                    [" DRAFT (feature" "RECOVERY REQUIRED)"]]
                   [{"is_draft" false
                     "fork_ms" 1
                     "git" {"is_workspace" true "repo" "vis" "branch" "main" "is_upstream" true}}
                    [" git ~/vis (main)"]]]]
            (expect (= expected
                       (->> (#'footer/build-segments {:workspace workspace :settings {}} 0)
                            (filter #(= :right (:region %)))
                            (mapv :text)))))))))

(defdescribe
  draft-landing-status-test
  ;; #247: approval clears live counts without erasing the task-review baseline.
  (it "shows edits, then unmerged commits, then only the draft name after approval"
      (let [draft {"label" "feature"
                   "draft_changes" {"modified" 1 "created" 3 "deleted" 1}
                   "git" {"modified" 99 "created" 0 "deleted" 0 "ahead" 99}}]
        (doseq [[facts expected]
                [[{"working_changes" {"modified" 1 "created" 0 "deleted" 0} "pending" 1 "ahead" 0}
                  [" DRAFT (feature" "~1)"]]
                 [{"working_changes" {"modified" 0 "created" 0 "deleted" 0} "pending" 0 "ahead" 1}
                  [" DRAFT (feature" "↑1)"]]
                 [{"working_changes" {"modified" 0 "created" 0 "deleted" 0} "pending" 0 "ahead" 0}
                  [" DRAFT (feature)"]]]]
          (expect (= expected (mapv :text (#'footer/draft-footer-spans (merge draft facts))))))))
  (it "shows copied source edits even when the task review is empty"
      (expect (= [" DRAFT (copied" "~1 +1)"]
                 (mapv :text
                       (#'footer/draft-footer-spans
                        {"label" "copied"
                         "working_changes" {"modified" 1 "created" 1 "deleted" 0}
                         "pending" 2
                         "ahead" 0
                         "draft_changes" {"modified" 0 "created" 0 "deleted" 0}}))))))
