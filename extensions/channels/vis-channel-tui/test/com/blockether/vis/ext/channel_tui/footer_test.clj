(ns com.blockether.vis.ext.channel-tui.footer-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.footer :as footer]
            [com.blockether.vis.ext.channel-tui.input :as input]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.git :as git]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- fixture-seg?
  "Always-on footer fixtures that ride the :right region alongside git —
   the managed-resource button (` ● N resources (F4) `) and the context-dir
   count (`N dirs` + `(/dir)`). The git-rendering tests filter these out so
   they stay focused on git."
  [{:keys [text]}]
  (boolean (re-find #"resources|dirs?$|\(F4\)|\(/dir\)" (str text))))

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
  ;; Regression for session 39a73cfb: footer hook IR was routed
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

(defdescribe footer-subtitle-test
  (let [build-subtitle-segments @#'footer/build-subtitle-segments]
    (it "builds contextual helpers for the input-border footer subtitle"
      (let [empty-text (mapv :text (build-subtitle-segments {:input (input/empty-input)} 0))
            typed-text (mapv :text (build-subtitle-segments {:input (input/paste-text (input/empty-input) "hello")} 0))]
        (expect (some #{"Ctrl+B voice"} empty-text))
        (expect (some #{"Ctrl+G sessions"} empty-text))
        (expect (some #{"Ctrl+K menu"} empty-text))
        (expect (some #{"↑↓ history"} empty-text))
        (expect (some #{"Ctrl+B voice"} typed-text))
        (expect (some #{"Ctrl+G sessions"} typed-text))
        (expect (some #{"Ctrl+K menu"} typed-text))
        (expect (not (some #{"↑↓ history"} typed-text)))))

    (it "advertises workspace switching only when multiple workspaces exist"
      (let [one-workspace (mapv :text (build-subtitle-segments {:input (input/empty-input)
                                                                :tabs [{:id :main}]} 0))
            two-workspaces (mapv :text (build-subtitle-segments {:input (input/empty-input)
                                                                 :tabs [{:id :main}
                                                                        {:id :feature}]} 0))]
        (expect (not (some #{"Shift+Tab switch workspace"} one-workspace)))
        (expect (some #{"Shift+Tab switch workspace"} two-workspaces))))

    (it "switches subtitle helpers while loading"
      (expect (= ["Esc / Ctrl+C cancel"]
                (mapv :text (build-subtitle-segments {:loading? true
                                                      :input (input/empty-input)} 0)))))

    (it "does not advertise quit while cancelling a live turn"
      (expect (= ["Cancelling... please wait"]
                (mapv :text (build-subtitle-segments {:loading? true
                                                      :cancelling? true
                                                      :input (input/empty-input)} 0))))))

  (it "accepts footer-subtitle contribution segments"
    (let [extension-subtitle-segments @#'footer/extension-subtitle-segments]
      (with-redefs-fn {#'vis/channel-contributions-for
                       (fn [_channel slot]
                         (case slot
                           :tui.slot/footer-subtitle-segment
                           [{:id :demo/subtitle
                             :fn (fn [_db _now-ms]
                                   {:ir [:ir {} [:p {} [:span {} "Demo helper"]]]
                                    :fg-role :muted
                                    :priority 2
                                    :region :center})}]
                           []))}
        (fn []
          (expect (= ["Demo helper"]
                    (mapv :text (extension-subtitle-segments {} 0))))))))

  (it "draws a bordered helper pocket"
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
              (putString [col row text]
                (swap! puts conj {:col col :row row :text text})
                this))]
      (with-redefs-fn {#'vis/channel-contributions-for (fn [_ _] [])}
        (fn []
          (footer/draw-footer-subtitle! g {:input (input/empty-input)} 4 90 0)
          (let [painted (str/join "" (map :text @puts))
                rows    (set (map :row @puts))]
            (expect (= #{4 5 6} rows))
            (expect (str/includes? painted "┌"))
            (expect (str/includes? painted "┐"))
            (expect (str/includes? painted "└"))
            (expect (str/includes? painted "┘"))
            (expect (str/includes? painted "│ "))
            (expect (str/includes? painted " │"))
            (expect (not-any? #(and (= 5 (:row %))
                                 (re-matches #"─+" (:text %)))
                      @puts))
            (expect (some #(and (= 6 (:row %))
                             (re-matches #"─+" (:text %)))
                      @puts))
            (expect (str/includes? painted "Ctrl+B voice"))))))))

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
                       #'git/cached-working-tree-status (fn [] {:workspace? false})}
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
                       #'git/cached-working-tree-status (fn [] {:workspace? false})}
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

  (it "shows Z.ai coding plan quota windows as percentages on the second footer line"
    (let [build-limits-segments @#'footer/build-limits-segments
          now-ms                1000000000000
          report                {:provider-id :zai-coding-plan
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
            (expect (re-find #"Z\.ai coding plan 5h 75% left ↺1h30m" text))
            (expect (re-find #"Z\.ai coding plan 7d 50% left ↺3d0h" text)))))))

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
                       #'git/cached-working-tree-status (fn
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
                      (remove fixture-seg?)
                      (mapv :text))))
          (expect (= "/tmp/vis-ws" @seen-root))))))

  (it "shows git repository state with one changed-file count on the first footer line right side"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai})
                       #'git/cached-working-tree-status (fn [] {:workspace? true
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
                      (remove fixture-seg?)
                      (mapv :text))))))))

  (it "shows when the current directory is outside a git workspace on the first footer line"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai})
                       #'git/cached-working-tree-status (fn [] {:workspace? false})}
        (fn []
          (let [spans (->> (build-segments {:messages [] :settings {}} 0)
                        (filter #(= :right (:region %)))
                        (remove fixture-seg?))]
            (expect (= ["No git"] (mapv :text spans)))
            (expect (= t/footer-error-fg (:fg (first spans))))
            (expect (true? (:bold? (first spans)))))))))

  (it "collapses clean file and synced commit state"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai})
                       #'git/cached-working-tree-status (fn [] {:workspace? true
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
                      (remove fixture-seg?)
                      (mapv :text))))))))

  (it "shows missing upstream distinctly from up-to-date commits"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai})
                       #'git/cached-working-tree-status (fn [] {:workspace? true
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
                      (remove fixture-seg?)
                      (mapv :text))))))))

  (it "shows cumulative token usage + price on the second footer row right side"
    ;; The old context-window pct gauge is retired. The `:right` region now
    ;; carries the billing-relevant cumulative numbers: tokens in→out and the
    ;; running session price, and emits nothing until a turn carries usage.
    (let [build-limits-segments @#'footer/build-limits-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai
                                                          :reasoning? false})}
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
