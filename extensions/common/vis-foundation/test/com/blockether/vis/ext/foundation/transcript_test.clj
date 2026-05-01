(ns com.blockether.vis.ext.foundation.transcript-test
  "Coverage for `transcript` (data) + `transcript-md` (Markdown
   renderer). The data fn is the canonical surface; the renderer is
   one transformation on top. Tests assert against the data first
   (so a future renderer rewrite or a JSON exporter doesn't break
   them) and only spot-check the Markdown for the few literals the
   CLI relies on."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation.transcript :as transcript]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- seed!
  "Two-turn fixture exercising the full transcript surface: one
   clean turn with comment / result / stdout / a `(def …)` var /
   thinking trace / answer-form-idx, and one failing turn with a
   prose-in-code error block + a clean follow-up block + stderr
   capture. Returns the conversation id."
  [s]
  (let [cid (vis/db-store-conversation! s {:channel :tui
                                           :title "Transcript fixture"
                                           :model "gpt-4o"
                                           :system-prompt "sys"})
        q1  (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                :query "First turn"
                                                :status :running})]
    ;; Turn 1: terminal iteration with a `(def …)` var, an `(answer …)`
    ;; block (idx 1), thinking trace, system prompt, and a full LLM
    ;; message envelope. The persistance layer derives :llm_system_prompt
    ;; + :llm_user_prompt from the :llm-messages we pass in here.
    (vis/db-store-iteration! s {:conversation-turn-id q1
                                :blocks [{:code              "(+ 1 1)"
                                          :comment           ";; double-check arithmetic"
                                          :result            2
                                          :stdout            "hello from clojure"
                                          :execution-time-ms 5}
                                         {:code              "(answer \"42\")"
                                          :result            :vis/silent
                                          :execution-time-ms 2}]
                                :answer        "42"
                                :answer-form-idx 1
                                :thinking      "Reasoning about arithmetic"
                                :vars          [{:name "x" :value 42 :code "(def x 42)"}]
                                :duration-ms 12
                                :llm-provider :blockether
                                :llm-model    "gpt-4o"
                                :llm-messages [{:role "system" :content "SYS_PROMPT_TEXT_FIXTURE"}
                                               {:role "user"   :content "USER_TURN_TEXT_FIXTURE"}]
                                :llm-raw-response "```clojure\n(+ 1 1)\n```"
                                :llm-selected-blocks [{:lang "clojure" :source "(+ 1 1)"}]
                                :tokens   {:input 100 :output 20 :reasoning 0 :cached 30}
                                :cost-usd 0.0042})
    (vis/db-update-conversation-turn! s q1 {:status :done :answer "42"})
    ;; Turn 2: failure iteration. No vars, no answer.
    (let [q2 (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                 :query "Second turn that fails"
                                                 :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id q2
                                  :blocks [{:code              "Let"
                                            :error             "ExceptionInfo: Unable to resolve symbol: Let"
                                            :stderr            "warning: prose-in-code"
                                            :execution-time-ms 1}
                                           {:code              "(+ 1 1)"
                                            :result            2
                                            :execution-time-ms 3}]
                                  :duration-ms 9
                                  :llm-provider :blockether
                                  :llm-model    "gpt-4o"
                                  :tokens   {:input 80 :output 10 :reasoning 0 :cached 20}
                                  :cost-usd 0.0021})
      (vis/db-update-conversation-turn! s q2 {:status :error}))
    cid))

;; ---------------------------------------------------------------------------
;; Data \u2014 the canonical contract every consumer (CLI, agent, future
;; JSON exporter) reads against.
;; ---------------------------------------------------------------------------

(defdescribe transcript-data-test
  (it "returns nil for a missing conversation id"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (expect (nil? (transcript/transcript s "00000000-0000-0000-0000-000000000000")))
        (finally (vis/db-dispose-connection! s)))))

  (it "returns the canonical {:conversation :totals :turns} shape"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid  (seed! s)
              data (transcript/transcript s cid)]
          (expect (map? data))
          (expect (= #{:conversation :totals :turns} (set (keys data))))
          ;; Conversation header carries the canonical fields.
          (expect (= cid    (:id    (:conversation data))))
          (expect (= "Transcript fixture" (:title (:conversation data))))
          (expect (= :tui   (:channel (:conversation data))))
          (expect (= "gpt-4o" (:model (:conversation data)))))
        (finally (vis/db-dispose-connection! s)))))

  (it "accepts an unambiguous string id prefix and normalizes back to the full UUID"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid    (seed! s)
              prefix (subs (str cid) 0 8)
              data   (transcript/transcript s prefix)]
          (expect (map? data))
          (expect (= cid (:id (:conversation data))))
          (expect (= 2 (count (:turns data)))))
        (finally (vis/db-dispose-connection! s)))))

  (it "returns nil for an ambiguous string prefix instead of picking an arbitrary conversation"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (doseq [_ (range 17)]
          (vis/db-store-conversation! s {:channel :tui}))
        (let [ids     (mapv :id (vis/db-list-conversations s :tui))
              buckets (vals (group-by #(subs (str %) 0 1) ids))
              matches (first (filter #(> (count %) 1) buckets))
              prefix  (subs (str (first matches)) 0 1)]
          ;; 17 UUIDs guarantee at least one shared first hex digit.
          (expect (> (count matches) 1))
          (expect (nil? (transcript/transcript s prefix))))
        (finally (vis/db-dispose-connection! s)))))

  (it "rolls up turn / iteration counts and tokens / cost into :totals"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid    (seed! s)
              totals (:totals (transcript/transcript s cid))]
          (expect (= 2 (:turns totals)))
          (expect (= 2 (:iterations totals)))
          ;; Tokens summed across both turns.
          (expect (= 180 (:input  (:tokens totals))))
          (expect (= 30  (:output (:tokens totals))))
          ;; Cost summed across both turns: 0.0042 + 0.0021 = 0.0063.
          ;; Compare with epsilon — IEEE 754 doubles don't land
          ;; exactly on 0.0063.
          (expect (< (Math/abs (- 0.0063 (double (:cost-usd totals)))) 1.0E-9)
            (str "actual: " (:cost-usd totals))))
        (finally (vis/db-dispose-connection! s)))))

  (it "carries every turn with goal / status / iteration count / failures"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid   (seed! s)
              turns (:turns (transcript/transcript s cid))]
          (expect (= 2 (count turns)))
          (expect (= "First turn"             (:goal (first turns))))
          (expect (= "Second turn that fails" (:goal (second turns))))
          (expect (= :done  (:status (first turns))))
          (expect (= :error (:status (second turns))))
          ;; Failure count comes from block-level :error keys, not a
          ;; query-level flag \u2014 the failing turn must report exactly 1.
          (expect (= 0 (:failure-count (first turns))))
          (expect (= 1 (:failure-count (second turns)))))
        (finally (vis/db-dispose-connection! s)))))

  (it "embeds every iteration on its turn, with full :blocks"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid    (seed! s)
              turn-2 (second (:turns (transcript/transcript s cid)))
              iter   (first  (:iterations turn-2))
              blocks (:blocks iter)]
          ;; Two blocks: the failing prose-in-code one + the clean
          ;; (+ 1 1) one.
          (expect (= 2 (count blocks)))
          (expect (= "Let"     (:code  (first  blocks))))
          (expect (= "(+ 1 1)" (:code  (second blocks))))
          ;; Failed block surfaces error + stderr verbatim.
          (expect (str/includes? (:error  (first blocks)) "Unable to resolve symbol: Let"))
          (expect (= "warning: prose-in-code" (:stderr (first blocks))))
          ;; Clean block surfaces result + no error key.
          (expect (= 2 (:result (second blocks))))
          (expect (nil? (:error (second blocks)))))
        (finally (vis/db-dispose-connection! s)))))

  (it "surfaces provider / model on each turn"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid   (seed! s)
              turn  (first (:turns (transcript/transcript s cid)))]
          (expect (= "blockether" (:provider turn)))
          (expect (= "gpt-4o"     (:model turn))))
        (finally (vis/db-dispose-connection! s)))))

  (it "carries thinking + answer-form-idx + vars + final answer on every iteration / turn"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid   (seed! s)
              data  (transcript/transcript s cid)
              turn  (first (:turns data))
              iter  (first (:iterations turn))]
          ;; Reasoning trace surfaces verbatim on the iteration.
          (expect (= "Reasoning about arithmetic" (:thinking iter)))
          ;; The terminal block index points at the `(answer …)` form.
          (expect (= 1 (:answer-form-idx iter)))
          ;; Per-iteration vars carry the (def …) we persisted.
          (let [vars (:vars iter)]
            (expect (= 1 (count vars)))
            (expect (= "x" (:name (first vars))))
            (expect (= 42  (:value (first vars))))
            (expect (str/includes? (:code (first vars)) "(def x 42)")))
          ;; The final answer surfaces on the turn.
          (expect (= "42" (:answer turn))))
        (finally (vis/db-dispose-connection! s)))))

  (it "surfaces raw LLM response diagnostics on iteration data"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid  (seed! s)
              iter (-> (transcript/transcript s cid)
                     :turns first :iterations first)]
          (expect (= "```clojure\n(+ 1 1)\n```" (:llm-raw-response-preview iter)))
          (expect (= 22 (:llm-raw-response-length iter)))
          (expect (= "66668222ec30f95b93cbd218b2406162d0bdb0e0d02b95db890a9d08d60592ed"
                    (:llm-raw-response-sha256 iter)))
          (expect (= 1 (:llm-selected-block-count iter)))
          (expect (= ["clojure"] (:llm-selected-block-langs iter))))
        (finally (vis/db-dispose-connection! s)))))

  (it "surfaces :returned-empty-blocks? as a typed boolean"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid (vis/db-store-conversation! s {:channel :tui :title "empty" :model "x"})
              q   (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                      :query "empty turn"
                                                      :status :running})
              _   (vis/db-store-iteration! s {:conversation-turn-id q :blocks []
                                              :duration-ms 1
                                              :tokens {:input 10 :output 0}
                                              :cost-usd 0.0001})
              _   (vis/db-update-conversation-turn! s q {:status :done})
              iter (-> (transcript/transcript s cid)
                     :turns first :iterations first)]
          ;; Empty-blocks? defaults to true (bit was 1) when the iter
          ;; row recorded zero blocks.
          (expect (true? (:returned-empty-blocks? iter))))
        (finally (vis/db-dispose-connection! s))))))

;; ---------------------------------------------------------------------------
;; Markdown renderer \u2014 spot-check a few literals the CLI relies on.
;; The data tests above are the real contract.
;; ---------------------------------------------------------------------------

(defdescribe transcript-md-test
  (it "returns a 'Conversation not found' line for a missing id (no throw)"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [out (transcript/transcript-md s "00000000-0000-0000-0000-000000000000")]
          (expect (string? out))
          (expect (str/includes? out "Conversation not found")))
        (finally (vis/db-dispose-connection! s)))))

  (it "accepts an unambiguous string prefix in the Markdown renderer too"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid    (seed! s)
              prefix (subs (str cid) 0 8)
              out    (transcript/transcript-md s prefix)]
          (expect (string? out))
          (expect (str/includes? out (str "conversation `" cid "`")))
          (expect (not (str/includes? out "Conversation not found"))))
        (finally (vis/db-dispose-connection! s)))))

  (it "renders header + per-turn block + per-iteration block dump"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid (seed! s)
              out (transcript/transcript-md s cid)]
          (expect (str/includes? out (str "conversation `" cid "`")))
          (expect (str/includes? out "Total turns:** 2"))
          (expect (str/includes? out "Total iterations:** 2"))
          ;; Per-turn header.
          (expect (str/includes? out "Goal:** First turn"))
          (expect (str/includes? out "Provider/model:** blockether/gpt-4o"))
          ;; Per-iteration header.
          (expect (str/includes? out "#### Iteration 0"))
          ;; Per-block header.
          (expect (str/includes? out "##### Block 0"))
          ;; Code rendered inside a fenced ```clojure block (NOT a
          ;; backtick cell) so multi-line code prints verbatim.
          (expect (str/includes? out "```clojure\n(+ 1 1)\n```"))
          ;; Comment preserved verbatim above the fence.
          (expect (str/includes? out ";; double-check arithmetic"))
          ;; Result line for the clean block.
          (expect (str/includes? out "Result: `2`"))
          ;; stdout / stderr captured under fenced text blocks.
          (expect (str/includes? out "hello from clojure"))
          (expect (str/includes? out "warning: prose-in-code"))
          ;; Failed blocks render the FULL error inside an `_error:_`
          ;; fence, not a truncated table cell.
          (expect (str/includes? out "_error:_"))
          (expect (str/includes? out "Unable to resolve symbol: Let"))
          ;; The failure marker stamps `[error]` on the status line.
          (expect (str/includes? out "[error]"))
          ;; Locale-stable dot separator for cost.
          (expect (str/includes? out "$0.0042"))
          (expect (str/includes? out "$0.0021"))
          ;; Thinking trace renders under a `_thinking:_` label.
          (expect (str/includes? out "_thinking:_"))
          (expect (str/includes? out "Reasoning about arithmetic"))
          ;; Vars renders under a `_vars defined this iteration:_`
          ;; label with one bullet per var.
          (expect (str/includes? out "_vars defined this iteration:_"))
          (expect (str/includes? out "`x`"))
          ;; The `(answer ...)` block is flagged with `[answer]` on
          ;; the status line so the reader spots the terminal form.
          (expect (str/includes? out "[answer]"))
          ;; The final answer text renders under a `#### Final answer`
          ;; section after every iteration of its turn.
          (expect (str/includes? out "#### Final answer"))
          ;; System prompt renders inside a collapsible <details> with
          ;; a size summary so the file stays scannable.
          (expect (str/includes? out "<details><summary>System prompt ("))
          (expect (str/includes? out "SYS_PROMPT_TEXT_FIXTURE"))
          ;; LLM message envelope ALSO renders collapsible — every
          ;; `[{:role :content}]` pair the provider saw, with a
          ;; per-role fenced sub-block.
          (expect (str/includes? out "<details><summary>LLM messages ("))
          (expect (str/includes? out "_system:_"))
          (expect (str/includes? out "_user:_"))
          (expect (str/includes? out "USER_TURN_TEXT_FIXTURE")))
        (finally (vis/db-dispose-connection! s))))))
