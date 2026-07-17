(ns com.blockether.vis.ext.channel-tui.limits-fmt-test
  (:require [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.limits-fmt :as lfmt]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe format-limit-number-test
             (it "renders integers without a decimal suffix"
                 (expect (= "500" (lfmt/format-limit-number 500)))
                 (expect (= "2000000" (lfmt/format-limit-number 2000000.0))))
             (it "renders non-integers with one decimal"
                 (expect (= "47.5" (lfmt/format-limit-number 47.5)))
                 (expect (= "0.3" (lfmt/format-limit-number 0.3))))
             (it "is nil-safe"
                 (expect (nil? (lfmt/format-limit-number nil)))
                 (expect (nil? (lfmt/format-limit-number "abc")))))

(defdescribe
  generic-limit-label-test
  (it "hard-coded labels for well-known plan rows"
      (expect (= "Codex 5h" (lfmt/generic-limit-label {:id :codex-5h})))
      (expect (= "Codex 7d" (lfmt/generic-limit-label {:id :codex-7d})))
      (expect (= "Z.ai coding plan 5h" (lfmt/generic-limit-label {:id :zai-coding-plan-5h})))
      (expect (= "Z.ai coding plan 7d" (lfmt/generic-limit-label {:id :zai-coding-plan-7d})))
      (expect (= "Premium interactions" (lfmt/generic-limit-label {:id :premium_interactions}))))
  (it "derives a label from :label, stripping ` Quota` / ` Quota (%)` suffixes"
      (expect (= "Z.ai coding plan 5h"
                 (lfmt/generic-limit-label {:id :zai-coding-plan-5h
                                            :label "Z.ai coding plan 5h token quota"})))
      (expect (= "Something" (lfmt/generic-limit-label {:id :other :label "Something Quota (%)"}))))
  (it "falls back to the :id when no :label is supplied"
      ;; Unknown ids still use the generic id-derived label path.
      (expect (= "Some provider window" (lfmt/generic-limit-label {:id :some-provider-window})))))

(defdescribe percentage-limit-row?-test
             (it "true for well-known percentage plan rows"
                 (expect (lfmt/percentage-limit-row? {:id :zai-coding-plan-5h :remaining 47}))
                 (expect (lfmt/percentage-limit-row? {:id :codex-7d :remaining 12})))
             (it "true for generic :rate rows scaled to 100"
                 (expect (lfmt/percentage-limit-row?
                           {:id :other :kind :rate :limit 100.0 :remaining 30})))
             (it "false when :remaining is absent or :limit isn't 100"
                 (expect (not (lfmt/percentage-limit-row? {:id :zai-coding-plan-5h})))
                 (expect (not (lfmt/percentage-limit-row?
                                {:id :other :kind :rate :limit 500 :remaining 30})))))

(defdescribe account-plan-window-row?-test
             (it "true for known Codex / Z.ai plan windows regardless of signal"
                 ;; No :remaining here: the whole point is a placeholder window
                 ;; (provider omitted data) still counts as a plan window.
                 (expect (lfmt/account-plan-window-row? {:id :codex-5h :precision :unknown}))
                 (expect (lfmt/account-plan-window-row? {:id :codex-7d}))
                 (expect (lfmt/account-plan-window-row? {:id :zai-coding-plan-5h})))
             (it "matches string ids that crossed the gateway wire"
                 (expect (lfmt/account-plan-window-row? {:id "codex-5h"})))
             (it "false for unrelated rows"
                 (expect (not (lfmt/account-plan-window-row? {:id :premium_interactions})))
                 (expect (not (lfmt/account-plan-window-row? {:id :rpm})))))

(defdescribe
  format-limit-usage-test
  (it "renders percentage-style rows as `N% left`"
      (expect (= "47% left" (lfmt/format-limit-usage {:id :zai-coding-plan-5h :remaining 47}))))
  (it "renders the full triple `used/limit used (remaining left)` when available"
      (expect (= "3/5 used (2 left)" (lfmt/format-limit-usage {:used 3 :limit 5 :remaining 2}))))
  (it "renders `used/limit used` when remaining is missing"
      (expect (= "3/5 used" (lfmt/format-limit-usage {:used 3 :limit 5}))))
  (it "renders `remaining/limit left` when used is missing"
      (expect (= "2/5 left" (lfmt/format-limit-usage {:remaining 2 :limit 5}))))
  (it "handles `:unlimited?` explicitly"
      (expect (= "unlimited" (lfmt/format-limit-usage {:unlimited? true}))))
  (it "returns nil when no usage signal is present" (expect (nil? (lfmt/format-limit-usage {})))))

(defdescribe generic-limit-has-signal?-test
             (it "true on :unlimited?" (expect (lfmt/generic-limit-has-signal? {:unlimited? true})))
             (it "true on any positive numeric field"
                 (expect (lfmt/generic-limit-has-signal? {:remaining 47}))
                 (expect (lfmt/generic-limit-has-signal? {:limit 100}))
                 (expect (lfmt/generic-limit-has-signal? {:used 12})))
             (it "true on reset timestamps even when credits are exhausted"
                 (expect (lfmt/generic-limit-has-signal? {:remaining 0
                                                          :window {:resets-at-ms 1778155200000}})))
             (it "false when all fields are zero/nil"
                 (expect (not (lfmt/generic-limit-has-signal? {})))
                 (expect (not (lfmt/generic-limit-has-signal? {:remaining 0 :limit 0 :used 0})))))

(defdescribe
  dynamic-summary-test
  (it "renders the Z.ai coding plan 5h + 7d rows compactly"
      (let [limits
            {:dynamic {:limits [{:id :zai-coding-plan-5h :remaining 47}
                                {:id :zai-coding-plan-7d :remaining 80}]}}

            out
            (lfmt/dynamic-summary limits)]

        (expect (str/includes? out "47% left"))
        (expect (str/includes? out "80% left"))
        (expect (str/includes? out " · "))))
  (it "caps to `max-rows` (default 2)"
      (let [limits
            {:dynamic {:limits [{:id :a :label "a" :remaining 10} {:id :b :label "b" :remaining 20}
                                {:id :c :label "c" :remaining 30}]}}

            out
            (lfmt/dynamic-summary limits)]

        (expect (= 1 (count (re-seq #" · " out))))))
  (it "prefers rows with signal; falls back to all when none have signal"
      (let [limits
            {:dynamic {:limits [{:id :a :label "a" :remaining 0} {:id :b :label "b" :remaining 0}]}}

            out
            (lfmt/dynamic-summary limits)]

        (expect (some? out))))
  (it "keeps a no-signal account plan window so a companion pair stays visible"
      ;; A provider that omits one window ships a placeholder row with no usage
      ;; signal (Codex 5h here); it must still render beside its data-bearing
      ;; companion instead of collapsing to a lone `7d`.
      (let [limits
            {:dynamic {:limits [{:id :codex-5h :precision :unknown}
                                {:id :codex-7d :remaining 81 :limit 100.0}]}}

            out
            (lfmt/dynamic-summary limits)]

        (expect (str/includes? out "Codex 5h"))
        (expect (str/includes? out "Codex 7d"))))
  (it "is nil when `[:dynamic :limits]` is empty"
      (expect (nil? (lfmt/dynamic-summary {:dynamic {:limits []}})))
      (expect (nil? (lfmt/dynamic-summary {})))))
