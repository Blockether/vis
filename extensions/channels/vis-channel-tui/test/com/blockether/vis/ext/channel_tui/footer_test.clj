(ns com.blockether.vis.ext.channel-tui.footer-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.footer :as footer]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.keymap :as keymap]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.git :as git]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- fixture-seg?
  "Always-on footer fixtures that ride the :right region alongside git —
   the managed-resource button (` ⚙ N (⌥J) `) and the context-dir
   button (` ⌂ N (⌥D) `). The git-rendering tests filter these out so
   they stay focused on git. Keys on the stable `:kind` tag, not the
   icon glyph or the platform-conditional chord label."
  [{:keys [kind]}]
  (boolean (#{:footer-resources :footer-dirs} kind)))

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

(defdescribe ir->footer-text-test
             ;; Footer hook IR was routed
             ;; through `lines->sentinel-strings`, which prepends
             ;; `MARKER_ANSWER_TXT` (\u206E). `draw-spans!` then wrote that
             ;; marker into a real terminal cell, showing as a leading blank
             ;; before "zai-coding/glm-5.1" in the second footer row.
             (let [ir->footer-text @#'footer/ir->footer-text]
               (it "strips block markers so plain-text IR yields plain text"
                   (expect (= "zai-coding/glm-5.1"
                              (ir->footer-text [:ir {} [:p {} [:span {} "zai-coding/glm-5.1"]]]))))
               (it "never returns a leading sentinel character"
                   (let [s (ir->footer-text [:ir {} [:p {} [:span {} "openai/gpt-5"]]])]
                     (expect (pos? (count s)))
                     (expect (not (sentinel-char? (.charAt ^String s 0))))))
               (it "strips inline style sentinels too (footer uses :bold? on seg, not IR)"
                   (let [s (ir->footer-text [:ir {}
                                             [:p {} [:strong {} "bold"] [:span {} "plain"]]])]
                     (expect (= "boldplain" s))
                     (expect (every? #(not (sentinel-char? %)) s))))))

(defdescribe
  echo-area-test
  (let [echo-segments @#'footer/echo-segments]
    (it "is empty on an idle draft (no keybinding nags)"
        (expect (= [] (echo-segments {:input (input/empty-input)})))
        (expect (= [] (echo-segments {:input (input/paste-text (input/empty-input) "hello")}))))
    (it "shows the C-g cancel hint while a turn is live"
        (expect (= [(str (keymap/chord keymap/abort-key) " cancel")]
                   (mapv :text (echo-segments {:loading? true :input (input/empty-input)})))))
    (it "shows a wait notice while cancelling, not the cancel hint"
        (expect (= ["Cancelling... please wait"]
                   (mapv :text
                         (echo-segments
                           {:loading? true :cancelling? true :input (input/empty-input)})))))
    (it "surfaces the which-key strip while the C-x prefix is armed"
        (let [text (mapv :text (echo-segments {:input (assoc (input/empty-input) :prefix true)}))]
          (expect (some #(str/includes? % "palette") text))))
    (it "renders a transient :echo message"
        (expect (= ["Copied"]
                   (mapv :text (echo-segments {:input (input/empty-input) :echo "Copied"})))))
    (it "never leaks voice recording status into the echo row"
        (let [text (mapv :text
                         (echo-segments {:input (input/empty-input)
                                         :channel-status {:voice/input {:text "● Recording 00:01"
                                                                        :level :warn}}}))]
          (expect (= [] text))))
    (it
      "paints a flat row with no box chrome"
      (let [puts (atom [])
            g (proxy [com.googlecode.lanterna.graphics.TextGraphics] []
                (clearModifiers [] this)
                (enableModifiers [_] this)
                (disableModifiers [_] this)
                (getActiveModifiers [] (java.util.EnumSet/noneOf com.googlecode.lanterna.SGR))
                (setForegroundColor [_] this)
                (setBackgroundColor [_] this)
                (fillRectangle [_ _ _] this)
                (setCharacter [_ _ _] this)
                (putString [col row text] (swap! puts conj {:col col :row row :text text}) this))]

        (footer/draw-echo-area! g {:loading? true :input (input/empty-input)} 4 90 0)
        (let [painted (str/join "" (map :text @puts))
              rows (set (map :row @puts))]

          (expect (= #{4} rows))
          (expect (not (str/includes? painted "┌")))
          (expect (not (str/includes? painted "└")))
          (expect (str/includes? painted (str (keymap/chord keymap/abort-key) " cancel"))))))))

(defdescribe
  build-segments-test
  (it "labels resources with its C-x prefix sequence, not the M-x palette chord"
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-5" :provider :openai})
                         #'git/cached-working-tree-status (fn []
                                                            {:workspace? false})
                         #'vis/list-resources (fn [_]
                                                [{:id :nrepl}])}
          (fn []
            (let [resource-text (->> (build-segments {:messages [] :settings {} :session {:id "s1"}}
                                                     0)
                                     (filter #(= :footer-resources (:kind %)))
                                     first
                                     :text)]
              (expect (str/includes? resource-text "(C-x s)"))
              (expect (not (str/includes? resource-text keymap/palette-chord))))))))
  (it "leaves voice recording status out of the footer because header owns channel statuses"
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-5" :provider :openai})}
          (fn []
            (let [voice (->> (build-segments {:messages []
                                              :settings {}
                                              :channel-status {:voice/input {:text
                                                                             "● Recording 00:01"
                                                                             :level :warn}}}
                                             0)
                             (filter #(= "● Recording 00:01" (:text %)))
                             first)]
              (expect (nil? voice))
              (expect (nil? voice))
              (expect (nil? voice)))))))
  (it "leaves cancelling status out of the footer because notifications own it"
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-5" :provider :openai})
                         #'git/cached-working-tree-status (fn []
                                                            {:workspace? false})}
          (fn []
            (expect (not-any? #(= "cancelling..." (:text %))
                              (build-segments {:messages [] :settings {} :cancelling? true} 0)))))))
  (it
    "shows Codex dynamic quota windows on the second footer line"
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
                                                    {:name "gpt-5.5" :provider :openai-codex})
                       #'git/cached-working-tree-status (fn []
                                                          {:workspace? false})}
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
  (it
    "shows Z.ai coding plan quota windows as percentages on the second footer line"
    (let [build-limits-segments
          @#'footer/build-limits-segments

          now-ms
          1000000000000

          report
          {:provider-id :zai-coding-plan
           :dynamic {:limits [{:id :zai-coding-plan-5h
                               :label "Z.ai coding plan 5h token quota"
                               :kind :tokens
                               :used 25.0
                               :limit 100.0
                               :remaining 75.0
                               :unlimited? false
                               :window {:resets-at-ms (+ now-ms (* 90 60 1000))}}
                              {:id :zai-coding-plan-7d
                               :label "Z.ai coding plan 7d token quota"
                               :kind :tokens
                               :used 50.0
                               :limit 100.0
                               :remaining 50.0
                               :unlimited? false
                               :window {:resets-at-ms (+ now-ms (* 3 24 60 60 1000))}}]}}]

      (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                    {:name "glm-5.1" :provider :zai-coding-plan})}
        (fn []
          (let [text (->> (build-limits-segments {:messages []
                                                  :settings {}
                                                  :provider-limits {:provider-id :zai-coding-plan
                                                                    :report report}}
                                                 now-ms)
                          (filter #(= :left (:region %)))
                          first
                          :text)]
            (expect (re-find #"Z\.ai coding plan 5h 75% left ↺1h30m" text))
            (expect (re-find #"Z\.ai coding plan 7d 50% left ↺3d0h" text)))))))
  (it
    "shows GitHub Copilot premium interaction utilization on the second footer line"
    (let [build-limits-segments
          @#'footer/build-limits-segments

          now-ms
          1000000000000

          report
          {:provider-id :github-copilot
           :dynamic
           {:limits [{:id :chat :label "Chat" :used 0.0 :limit 0.0 :remaining 0.0 :unlimited? false}
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

      (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                    {:name "claude-opus-4-6"
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
      (let [build-segments
            @#'footer/build-segments

            seen-root
            (atom nil)]

        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-4o" :provider :openai})
                         #'git/cached-working-tree-status
                         (fn ([] {:workspace? false}) ([root] (reset! seen-root (.getPath root))
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
            (expect
              (= ["git ~/vis (feature/ws ∅)"]
                 (->> (build-segments {:messages [] :settings {} :workspace/root "/tmp/vis-ws"} 0)
                      (filter #(= :right (:region %)))
                      (remove fixture-seg?)
                      (mapv :text))))
            ;; `.getPath` yields the OS separator; the logical root is `/`-based.
            (expect (= "/tmp/vis-ws"
                       (some-> @seen-root
                               (.replace "\\" "/"))))))))
  (it "shows git repository state with one changed-file count on the first footer line right side"
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-4o" :provider :openai})
                         #'git/cached-working-tree-status (fn []
                                                            {:workspace? true
                                                             :repo "vis"
                                                             :branch "main"
                                                             :modified 2
                                                             :created 3
                                                             :deleted 1
                                                             :upstream? true
                                                             :ahead 4
                                                             :behind 0})}
          (fn []
            (expect (= ["git ~/vis (main ~2 +3 -1 ⇡4)"]
                       (->> (build-segments {:messages [] :settings {}} 0)
                            (filter #(= :right (:region %)))
                            (remove fixture-seg?)
                            (mapv :text))))))))
  (it "shows when the current directory is outside a git workspace on the first footer line"
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-4o" :provider :openai})
                         #'git/cached-working-tree-status (fn []
                                                            {:workspace? false})}
          (fn []
            (let [spans (->> (build-segments {:messages [] :settings {}} 0)
                             (filter #(= :right (:region %)))
                             (remove fixture-seg?))]
              (expect (= ["No git"] (mapv :text spans)))
              (expect (= t/footer-error-fg (:fg (first spans))))
              (expect (true? (:bold? (first spans)))))))))
  (it "collapses clean file and synced commit state"
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-4o" :provider :openai})
                         #'git/cached-working-tree-status (fn []
                                                            {:workspace? true
                                                             :repo "vis"
                                                             :branch "main"
                                                             :modified 0
                                                             :created 0
                                                             :deleted 0
                                                             :upstream? true
                                                             :ahead 0
                                                             :behind 0})}
          (fn []
            (expect (= ["git ~/vis (main)"]
                       (->> (build-segments {:messages [] :settings {}} 0)
                            (filter #(= :right (:region %)))
                            (remove fixture-seg?)
                            (mapv :text))))))))
  (it "shows missing upstream distinctly from up-to-date commits"
      (let [build-segments @#'footer/build-segments]
        (with-redefs-fn {#'footer/chosen-model-info (fn []
                                                      {:name "gpt-4o" :provider :openai})
                         #'git/cached-working-tree-status (fn []
                                                            {:workspace? true
                                                             :repo "vis"
                                                             :branch "main"
                                                             :modified 0
                                                             :created 0
                                                             :deleted 0
                                                             :upstream? false
                                                             :ahead 0
                                                             :behind 0})}
          (fn []
            (expect (= ["git ~/vis (main ∅)"]
                       (->> (build-segments {:messages [] :settings {}} 0)
                            (filter #(= :right (:region %)))
                            (remove fixture-seg?)
                            (mapv :text))))))))
  (it "shows cumulative token usage + price on the second footer row right side"
      ;; The old context-window pct gauge is retired. The `:right` region now
      ;; carries the billing-relevant cumulative numbers: tokens in→out and the
      ;; running session price, and emits nothing until a turn carries usage.
      (let [build-limits-segments @#'footer/build-limits-segments]
        (with-redefs-fn {#'footer/chosen-model-info
                         (fn []
                           {:name "gpt-4o" :provider :openai :reasoning? false})}
          (fn []
            ;; Right region: bare in→out token counts and the running session
            ;; price (`~$` = approximate — rounded display, not billed-to-cent).
            (expect (= ["100→20" "~$0.0042"]
                       (->> (build-limits-segments {:messages [{:tokens {:input 100 :output 20}
                                                                :cost {:total-cost 0.0042}}]
                                                    :settings {}}
                                                   0)
                            (filter #(= :right (:region %)))
                            (remove fixture-seg?)
                            (mapv :text))))
            (expect (empty? (->> (build-limits-segments {:messages [] :settings {}} 0)
                                 (filter #(= :right (:region %)))
                                 (mapv :text))))))))
  (it "joins shortcuts to their labels without separator dots"
      (let [spans-width @#'footer/spans-width]
        (expect (= (count "model (C-x m) / reasoning: deep (C-x r)")
                   (spans-width [{:text "model"} {:text "(C-x m)" :join-left? true}
                                 {:text "reasoning: deep"} {:text "(C-x r)" :join-left? true}]
                                " / "))))))

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
        [{:text "openai-codex/gpt-5.5 (C-x o) (cycle 1/3 C-x m)" :region :left :priority 2}
         {:text "reasoning: deep" :region :left :priority 3}
         {:text " resources 0 (C-x s) " :region :right :priority 2}]

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

(defdescribe generic-limits-footer-text-test
             (it "shows a loading placeholder before the polling thread populates :provider-limits"
                 (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
                   (expect (= "limits: loading…" (generic-limits-footer-text {} :openai-codex 0)))))
             (it "shows a loading placeholder when the polled report is for a different provider"
                 (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
                   (expect (= "limits: loading…"
                              (generic-limits-footer-text {:provider-limits
                                                           {:provider-id :anthropic
                                                            :report {:provider-id :anthropic
                                                                     :status :ok
                                                                     :dynamic {:limits []}}}}
                                                          :openai-codex
                                                          0)))))
             (it "surfaces the provider error message when the limits-fn failed"
                 (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
                   (expect (= "limits: error (boom)"
                              (generic-limits-footer-text
                                {:provider-limits {:provider-id :openai-codex
                                                   :report {:provider-id :openai-codex
                                                            :status :error
                                                            :error {:type :provider/limits-error
                                                                    :message "boom"}
                                                            :dynamic {:limits []}}}}
                                :openai-codex
                                0)))))
             (it "asks for sign-in when the provider report is :unauthenticated"
                 (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
                   (expect (= "limits: sign in required"
                              (generic-limits-footer-text {:provider-limits
                                                           {:provider-id :openai-codex
                                                            :report {:provider-id :openai-codex
                                                                     :status :unauthenticated
                                                                     :dynamic {:limits []}}}}
                                                          :openai-codex
                                                          0)))))
             (it "stays silent for providers that legitimately don't expose limits"
                 (let [generic-limits-footer-text @#'footer/generic-limits-footer-text]
                   (expect (nil? (generic-limits-footer-text {:provider-limits
                                                              {:provider-id :openai-codex
                                                               :report {:provider-id :openai-codex
                                                                        :status :unsupported
                                                                        :dynamic {:limits []}}}}
                                                             :openai-codex
                                                             0))))))
