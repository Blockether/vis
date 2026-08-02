(ns com.blockether.vis.internal.limits-format-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.limits-format :as lf]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe format-limit-number-test
             (it "renders integral values clean and fractional ones with one decimal"
                 (expect (= "3" (lf/format-limit-number 3)))
                 (expect (= "3" (lf/format-limit-number 3.0)))
                 (expect (= "3.1" (lf/format-limit-number 3.14)))
                 ;; Locale/ROOT: a JVM started under a comma-decimal locale must not emit
                 ;; "3,1" next to English suffix text.
                 (expect (not (str/includes? (lf/format-limit-number 3.14) ","))))
             (it "returns nil for anything that is not a number"
                 (expect (nil? (lf/format-limit-number nil)))
                 (expect (nil? (lf/format-limit-number "12")))))

(defdescribe
  generic-limit-label-test
  (it "uses the hand-rolled overrides for the known plan rows"
      (expect (= "Codex 5h" (lf/generic-limit-label {:id :codex-5h})))
      (expect (= "Codex 7d" (lf/generic-limit-label {:id :codex-7d})))
      (expect (= "Z.ai coding plan 5h" (lf/generic-limit-label {:id :zai-coding-plan-5h})))
      (expect (= "Premium interactions" (lf/generic-limit-label {:id :premium_interactions}))))
  (it "matches the overrides through the gateway wire, where ids arrive as strings"
      ;; The JSON hop stringifies keyword VALUES; only KEYS are keywordized on
      ;; parse. Both shapes must render identically.
      (expect (= (lf/generic-limit-label {:id :codex-5h})
                 (lf/generic-limit-label {:id "codex-5h"}))))
  (it "derives a label from :label or :id and trims the redundant quota suffix"
      (expect (= "Premium Interactions"
                 (lf/generic-limit-label {:label "Premium Interactions Quota (%)"})))
      (expect (= "Chat" (lf/generic-limit-label {:label "Chat Quota"})))
      (expect (= "Some thing" (lf/generic-limit-label {:id :some_thing})))
      (expect (= "Limit" (lf/generic-limit-label {})))))

(defdescribe percentage-limit-row?-test
             (it "treats the known plan windows as percentages"
                 (expect (lf/percentage-limit-row? {:id :codex-5h :remaining 46.7}))
                 (expect (lf/percentage-limit-row? {:id "zai-coding-plan-7d" :remaining 5})))
             (it "catches generic 0-100 rate rows, wire-encoded kinds included"
                 (expect (lf/percentage-limit-row? {:kind :rate :limit 100 :remaining 42}))
                 (expect (lf/percentage-limit-row? {:kind "rate" :limit 100 :remaining 42})))
             (it "needs a numeric remaining and refuses a token-count row"
                 (expect (not (lf/percentage-limit-row? {:id :codex-5h})))
                 (expect (not (lf/percentage-limit-row? {:kind :rate :limit 500000 :remaining 42})))
                 (expect (not (lf/percentage-limit-row?
                                {:kind :tokens :limit 100 :remaining 42})))))

(defdescribe format-limit-usage-test
             (it "picks the most informative shape the row's numbers allow"
                 (expect (= "unlimited" (lf/format-limit-usage {:is-unlimited true})))
                 (expect (= "47% left" (lf/format-limit-usage {:id :codex-5h :remaining 46.7})))
                 (expect (= "3/5 used (2 left)"
                            (lf/format-limit-usage {:used 3 :limit 5 :remaining 2})))
                 (expect (= "3/5 used" (lf/format-limit-usage {:used 3 :limit 5})))
                 (expect (= "2/5 left" (lf/format-limit-usage {:remaining 2 :limit 5})))
                 (expect (= "2 left" (lf/format-limit-usage {:remaining 2})))
                 (expect (= "3 used" (lf/format-limit-usage {:used 3}))))
             (it "returns nil only when the row carries no usage signal at all"
                 ;; Callers rely on this to skip empty cells with `(when usage ...)`.
                 (expect (nil? (lf/format-limit-usage {})))
                 (expect (nil? (lf/format-limit-usage {:label "Chat"})))))

(defdescribe generic-limit-has-signal?-test
             (it "counts a reset timestamp as signal even at zero remaining"
                 ;; Exactly when the user needs to know when credits come back.
                 (expect (lf/generic-limit-has-signal? {:remaining 0
                                                        :window {:resets-at-ms 1785000000000}})))
             (it "counts unlimited and any positive number as signal"
                 (expect (lf/generic-limit-has-signal? {:is-unlimited true}))
                 (expect (lf/generic-limit-has-signal? {:limit 5}))
                 (expect (lf/generic-limit-has-signal? {:remaining 5}))
                 (expect (lf/generic-limit-has-signal? {:used 5})))
             (it "finds no signal in an empty or all-zero row"
                 (expect (not (lf/generic-limit-has-signal? {})))
                 (expect (not (lf/generic-limit-has-signal? {:limit 0 :remaining 0 :used 0})))))

(defdescribe limit-row-exhausted?-test
             (it "is true only when zero remaining is a WALL"
                 (expect (lf/limit-row-exhausted? {:remaining 0 :limit 5}))
                 (expect (lf/limit-row-exhausted? {:remaining 0 :used 3}))
                 (expect (lf/limit-row-exhausted? {:remaining -1 :limit 5})))
             (it "is false for a brand new all-zero row that simply is not filled in yet"
                 (expect (not (lf/limit-row-exhausted? {:remaining 0 :limit 0})))
                 (expect (not (lf/limit-row-exhausted? {}))))
             (it "is false for an unlimited row and for a row with remaining left"
                 (expect (not (lf/limit-row-exhausted? {:is-unlimited true :remaining 0 :limit 5})))
                 (expect (not (lf/limit-row-exhausted? {:remaining 2 :limit 5})))))

(defdescribe prioritize-limit-rows-test
             (it "leads with the bucket that is actually rejecting requests"
                 ;; Regression: GitHub Copilot lists chat/completions (unlimited) before
                 ;; premium_interactions, so a truncated summary used to read
                 ;; "Chat unlimited · Completions unlimited" while the ONE bucket that
                 ;; rejects requests sat silently at 0 remaining.
                 (expect (= [:blocked :tight :loose :unlimited]
                            (mapv :id
                                  (lf/prioritize-limit-rows
                                    [{:id :unlimited :is-unlimited true}
                                     {:id :loose :remaining 9 :limit 10}
                                     {:id :tight :remaining 1 :limit 10}
                                     {:id :blocked :remaining 0 :limit 10}])))))
             (it "ranks by pressure: exhausted, then tightest fraction, then no tank at all"
                 (expect (= [0 0.0] (lf/limit-row-pressure {:remaining 0 :limit 5})))
                 (expect (= [1 0.25] (lf/limit-row-pressure {:remaining 1 :limit 4})))
                 (expect (= [2 0.0] (lf/limit-row-pressure {:is-unlimited true})))
                 ;; unmeasurable rows sort with the loosest metered ones, never ahead of them
                 (expect (= [1 1.0] (lf/limit-row-pressure {}))))
             (it "returns a vector and keeps every row"
                 (let [rows [{:id :a} {:id :b} {:id :c}]]
                   (expect (vector? (lf/prioritize-limit-rows rows)))
                   (expect (= (set rows) (set (lf/prioritize-limit-rows rows)))))))

(defdescribe label+usage-test
             (it "joins label and usage, or renders the label alone"
                 (expect (= "Codex 5h 47% left" (lf/label+usage {:id :codex-5h :remaining 46.7})))
                 (expect (= "Chat" (lf/label+usage {:label "Chat"}))))
             (it "never returns nil while a fallback label exists"
                 ;; `generic-limit-label` bottoms out at "Limit", so a row always renders.
                 (expect (= "Limit" (lf/label+usage {})))))

(defdescribe
  dynamic-summary-test
  (it "summarises the rows with signal, pressure first, joined by a middot"
      (expect (= "Premium interactions 3/5 used (2 left) · Chat unlimited"
                 (lf/dynamic-summary
                   {:dynamic {:limits
                              [{:id :chat :is-unlimited true}
                               {:id :premium_interactions :used 3 :limit 5 :remaining 2}]}}))))
  (it "honours max-rows and defaults to two"
      (let
        [limits {:dynamic {:limits [{:id :a :remaining 1 :limit 10} {:id :b :remaining 2 :limit 10}
                                    {:id :c :remaining 3 :limit 10}]}}]
        (expect (= 2 (count (str/split (lf/dynamic-summary limits) #" · "))))
        (expect (= 1 (count (str/split (lf/dynamic-summary limits 1) #" · "))))
        (expect (= 3 (count (str/split (lf/dynamic-summary limits 3) #" · "))))))
  (it "still surfaces SOMETHING when a fresh report has no signal yet"
      (expect (some? (lf/dynamic-summary {:dynamic {:limits
                                                    [{:id :x :limit 0 :remaining 0 :used 0}]}}))))
  (it "keeps a placeholder plan window visible even without usage signal"
      ;; Codex / Z.ai windows are surfaced as a PAIR; a provider omitting data for
      ;; one window must not make that window disappear.
      (expect (str/includes? (lf/dynamic-summary {:dynamic {:limits [{:id :codex-5h}
                                                                     {:id :codex-7d}]}})
                             "Codex"))
      (expect (lf/account-plan-window-row? {:id "codex-7d"}))
      (expect (not (lf/account-plan-window-row? {:id :chat}))))
  (it "returns nil when there is nothing to render"
      (expect (nil? (lf/dynamic-summary nil)))
      (expect (nil? (lf/dynamic-summary {})))
      (expect (nil? (lf/dynamic-summary {:dynamic {:limits []}})))))
