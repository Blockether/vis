(ns com.blockether.vis.loop.context-flow-test
  "Regression tests for cross-iteration and cross-turn context preservation.

   These cover three previously-silent bugs that together caused the
   'agent forgets prior turns' symptom on web/Telegram:

   1. Plain-text user/assistant history was filtered out of `initial-messages`
      behind a multimodal-only gate — web's reconstructed transcript was
      silently dropped, so every new turn saw only the current question.
   2. The per-iteration prompt rebuild hard-coded `(take 2 messages)`, so
      any history longer than (system + requirement) was truncated on the
      very first iteration, even after fix #1.
   3. The previous iteration's `:thinking` was re-injected ONLY as a
      150-char preview inside `<var_index>` — not as a first-class prompt
      block — so long reasoning became illegible noise for the next step."
  (:require
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe it expect]]
   [com.blockether.vis.loop.core :as rlm-core]))

;; =============================================================================
;; Fix #1 — assemble-initial-messages carries cross-turn history verbatim
;; =============================================================================

(defdescribe assemble-initial-messages-test
  (it "returns [system, requirement] + history, in order"
    (let [result (rlm-core/assemble-initial-messages
                   {:system-prompt "SYS"
                    :initial-user-content "{:requirement \"new q\"}"
                    :history-messages [{:role "user"      :content "earlier q"}
                                       {:role "assistant" :content "earlier a"}]})]
      (expect (= 4 (count result)))
      (expect (= "SYS" (:content (nth result 0))))
      (expect (= "{:requirement \"new q\"}" (:content (nth result 1))))
      (expect (= "earlier q" (:content (nth result 2))))
      (expect (= "earlier a" (:content (nth result 3))))))

  (it "preserves multimodal (sequential) content unchanged"
    (let [img {:type "image_url" :image_url {:url "data:..."}}
          result (rlm-core/assemble-initial-messages
                   {:system-prompt "SYS"
                    :initial-user-content "{:req ...}"
                    :history-messages [{:role "user" :content [img]}]})]
      (expect (= 3 (count result)))
      (expect (sequential? (:content (nth result 2))))))

  (it "mixes plain-text and multimodal history"
    (let [img {:type "image_url" :image_url {:url "data:..."}}
          result (rlm-core/assemble-initial-messages
                   {:system-prompt "SYS"
                    :initial-user-content "Q"
                    :history-messages [{:role "user"      :content "q1"}
                                       {:role "assistant" :content "a1"}
                                       {:role "user"      :content [img]}]})]
      (expect (= 5 (count result)))
      (expect (= "q1"     (:content (nth result 2))))
      (expect (= "a1"     (:content (nth result 3))))
      (expect (sequential? (:content (nth result 4))))))

  (it "handles nil history-messages without NPE"
    (let [result (rlm-core/assemble-initial-messages
                   {:system-prompt "SYS"
                    :initial-user-content "Q"
                    :history-messages nil})]
      (expect (vector? result))
      (expect (= 2 (count result)))))

  (it "handles empty history-messages"
    (let [result (rlm-core/assemble-initial-messages
                   {:system-prompt "SYS"
                    :initial-user-content "Q"
                    :history-messages []})]
      (expect (= 2 (count result))))))

;; =============================================================================
;; Fix #2 — per-iteration rebuild keeps the full initial history, not just 2
;; =============================================================================

(defdescribe trim-to-initial-history-test
  (it "keeps system + requirement + all prior user/assistant messages"
    (let [initial [{:role "system"    :content "S"}
                   {:role "user"      :content "{:req Q}"}
                   {:role "user"      :content "old q1"}
                   {:role "assistant" :content "old a1"}]
          accumulated (conj initial
                        {:role "user"      :content "[error retry]"}
                        {:role "assistant" :content "[empty]"})]
      (expect (= initial (rlm-core/trim-to-initial-history accumulated (count initial))))))

  (it "drops intra-loop error/nudge messages added via recur"
    (let [initial [{:role "system" :content "S"}
                   {:role "user"   :content "Q"}]
          accumulated (into initial
                        [{:role "user"      :content "[err]"}
                         {:role "assistant" :content "[empty]"}])]
      (expect (= initial (rlm-core/trim-to-initial-history accumulated 2)))))

  (it "is a no-op when initial-count matches message count"
    (let [initial [{:role "system" :content "S"}
                   {:role "user"   :content "Q"}]]
      (expect (= initial (rlm-core/trim-to-initial-history initial (count initial))))))

  (it "returns a vector (not a seq) so `conj` appends on the right end"
    (let [result (rlm-core/trim-to-initial-history
                   [{:role "system" :content "S"}
                    {:role "user"   :content "Q"}
                    {:role "user"   :content "[extra]"}]
                   2)]
      (expect (vector? result)))))

;; =============================================================================
;; Fix #3 — prior thinking gets a dedicated full-text block, not a 150-char stub
;; =============================================================================

(defdescribe build-iteration-context-test
  (it "includes prior-thinking as a <prior_thinking> block when non-blank"
    (let [ctx (rlm-core/build-iteration-context
                {:prior-thinking "I realized the pattern is X because Y"
                 :exec-results nil
                 :var-index-str nil
                 :budget-warning-str nil})]
      (expect (some? ctx))
      (expect (str/includes? ctx "<prior_thinking>"))
      (expect (str/includes? ctx "I realized the pattern is X because Y"))))

  (it "shows FULL thinking well past the old <var_index> 150-char cap"
    (let [long-thinking (apply str (repeat 1500 "reasoning "))  ;; 15000 chars
          ctx (rlm-core/build-iteration-context
                {:prior-thinking long-thinking
                 :exec-results nil
                 :var-index-str nil
                 :budget-warning-str nil})
          between (-> ctx
                    (str/replace "<prior_thinking>\n" "")
                    (str/replace "\n</prior_thinking>" ""))]
      (expect (> (count between) 150))
      (expect (<= (count between) rlm-core/PRIOR_THINKING_MAX_CHARS))))

  (it "truncates prior-thinking at PRIOR_THINKING_MAX_CHARS"
    (let [huge (apply str (repeat (* 10 rlm-core/PRIOR_THINKING_MAX_CHARS) "x"))
          ctx (rlm-core/build-iteration-context
                {:prior-thinking huge
                 :exec-results nil
                 :var-index-str nil
                 :budget-warning-str nil})]
      (expect (<= (count ctx)
                (+ rlm-core/PRIOR_THINKING_MAX_CHARS 200)))))  ;; + tag overhead

  (it "omits <prior_thinking> when thinking is nil or blank"
    (let [with-journal (rlm-core/build-iteration-context
                         {:prior-thinking nil
                          :exec-results "<journal>foo</journal>"
                          :var-index-str nil
                          :budget-warning-str nil})
          blank-string (rlm-core/build-iteration-context
                         {:prior-thinking "   \n  "
                          :exec-results "<journal>foo</journal>"
                          :var-index-str nil
                          :budget-warning-str nil})]
      (expect (not (str/includes? with-journal "<prior_thinking>")))
      (expect (not (str/includes? blank-string "<prior_thinking>")))))

  (it "wraps var-index-str in <var_index>...</var_index>"
    (let [ctx (rlm-core/build-iteration-context
                {:prior-thinking nil
                 :exec-results nil
                 :var-index-str "  x | 1 | int | - | - | 42"
                 :budget-warning-str nil})]
      (expect (str/includes? ctx "<var_index>"))
      (expect (str/includes? ctx "x | 1 | int"))
      (expect (str/includes? ctx "</var_index>"))))

  (it "returns nil when every component is blank"
    (let [ctx (rlm-core/build-iteration-context
                {:prior-thinking nil
                 :exec-results nil
                 :var-index-str nil
                 :budget-warning-str nil})]
      (expect (nil? ctx))))

  (it "returns nil for whitespace-only components"
    (let [ctx (rlm-core/build-iteration-context
                {:prior-thinking " "
                 :exec-results "   "
                 :var-index-str ""
                 :budget-warning-str "\n"})]
      (expect (nil? ctx))))

  (it "orders blocks: prior-thinking, exec-results, var-index, budget-warning"
    (let [ctx (rlm-core/build-iteration-context
                {:prior-thinking "PT"
                 :exec-results "JOURNAL"
                 :var-index-str "VI"
                 :budget-warning-str "BW"})
          pt-idx (str/index-of ctx "PT")
          j-idx  (str/index-of ctx "JOURNAL")
          vi-idx (str/index-of ctx "<var_index>")
          bw-idx (str/index-of ctx "BW")]
      (expect (< pt-idx j-idx))
      (expect (< j-idx vi-idx))
      (expect (< vi-idx bw-idx)))))
