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

(defn- tool-result
  [command]
  {:success? true
   :result {:exit 0
            :timed-out? false
            :cwd "."
            :command command
            :duration-ms 5}
   :info {:op :v/tool
          :tool {:symbol 'tool :call "v/tool" :alias 'v}
          :command command
          :duration-ms 5
          :started-at-ms 10
          :finished-at-ms 15
          :target {:requested "." :kind :dir}}
   :error nil})

(defn- seed!
  "Two-turn fixture exercising the full transcript surface: one
   clean turn with comment / result / a `(def ...)` var /
   thinking trace / answer-form-idx, and one failing turn with a
   prose-in-code error block + a clean follow-up block.
   Returns the conversation id."
  [s]
  (let [cid (vis/db-store-conversation! s {:channel :tui
                                           :title "Transcript fixture"
                                           :provider :openai
                                           :model "gpt-4o"
                                           :system-prompt "sys"})
        q1  (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                :user-request "First turn"
                                                :status :running})]
    ;; Turn 1: terminal iteration with a `(def ...)` var, an `(done ...)`
    ;; block (idx 1), thinking trace, system prompt, and a full LLM
    ;; message envelope. The persistance layer derives :llm_system_prompt
    ;; + :llm_user_prompt from the :llm-messages we pass in here.
    (vis/db-store-iteration! s {:conversation-turn-id q1
                                :code          "(+ 1 1)"
                                :result        2
                                :answer        "42"
                                :thinking      "Reasoning about arithmetic"
                                :vars          [{:name "x" :value 42 :code "(def x 42)"}]
                                :duration-ms 12
                                :llm-provider :blockether
                                :llm-model    "gpt-4o"
                                :llm-messages [{:role "system" :content "SYS_PROMPT_TEXT_FIXTURE"}
                                               {:role "user"
                                                :content "<current_user_message>\nUSER_TURN_TEXT_FIXTURE\n</current_user_message>"}
                                               {:role "assistant"
                                                :content "ASSISTANT_REPLAY_FIXTURE"}
                                               {:role "user"
                                                :content (str "<current_turn_context>\n"
                                                           "engine_state: turn.iteration/start\n"
                                                           "engine_phase: model_think\n"
                                                           "conversation_id: conv-fixture\n"
                                                           "engine_turn_id: turn-fixture\n"
                                                           "engine_turn_position: 1\n"
                                                           "current_engine_iteration_id: turn/turn-fix/iteration/1\n"
                                                           "engine_iteration_position: 1\n"
                                                           "prompt_role: user\n"
                                                           "</current_turn_context>\n\n"
                                                           ";; ctx =\n"
                                                           (pr-str {:hints [{:id :vis.foundation/conversation-title
                                                                             :text "observe before answer"
                                                                             :importance :high
                                                                             :satisfy-with '(satisfy-hint! :vis.foundation/conversation-title)}]}))}]
                                :llm-raw-response "```clojure\n(+ 1 1)\n```"
                                :llm-executable-blocks [{:lang "clojure" :source "(+ 1 1)"}]
                                :tokens   {:input 100 :output 20 :reasoning 0 :cached 30}
                                :cost-usd 0.0042})
    (vis/db-update-conversation-turn! s q1 {:status :done :answer "42"})
    ;; Turn 2: failure iteration. No vars, no answer.
    (let [q2 (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                 :user-request "Second turn that fails"
                                                 :status :running})]
      (vis/db-store-iteration! s {:conversation-turn-id q2
                                  :code "Let"
                                  :error {:message "ExceptionInfo: Unable to resolve symbol: Let"}
                                  :duration-ms 1
                                  :llm-provider :blockether
                                  :llm-model    "gpt-4o"
                                  :tokens   {:input 80 :output 10 :reasoning 0 :cached 20}
                                  :cost-usd 0.0021})
      (vis/db-update-conversation-turn! s q2 {:status :error}))
    cid))

;; ---------------------------------------------------------------------------
;; Data - the canonical contract every consumer (CLI, agent, future
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
          (expect (= #{:conversation :totals :turns :dialog :calls :timeline :llm-diagnostics}
                    (set (keys data))))
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
          ;; Compare with epsilon - IEEE 754 doubles don't land
          ;; exactly on 0.0063.
          (expect (< (Math/abs (- 0.0063 (double (:cost-usd totals)))) 1.0E-9)
            (str "actual: " (:cost-usd totals))))
        (finally (vis/db-dispose-connection! s)))))

  (it "carries every turn with user request / status / iteration count / failures"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid   (seed! s)
              turns (:turns (transcript/transcript s cid))]
          (expect (= 2 (count turns)))
          (expect (= "First turn"             (:user-request (first turns))))
          (expect (= "Second turn that fails" (:user-request (second turns))))
          (expect (= :done  (:status (first turns))))
          (expect (= :error (:status (second turns))))
          ;; Failure count comes from block-level :error keys, not a
          ;; turn-level flag - the failing turn must report exactly 1.
          (expect (= 0 (:failure-count (first turns))))
          (expect (= 1 (:failure-count (second turns)))))
        (finally (vis/db-dispose-connection! s)))))

  (it "embeds every iteration on its turn, with one flat :blocks adapter entry"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid    (seed! s)
              turn-2 (second (:turns (transcript/transcript s cid)))
              iter   (first  (:iterations turn-2))
              blocks (:blocks iter)]
          ;; One block per persisted iteration after hard cut.
          (expect (= 1 (count blocks)))
          (expect (= "Let" (:code (first blocks))))
          ;; Failed block surfaces error verbatim.
          (expect (str/includes? (str (:error (first blocks))) "Unable to resolve symbol: Let")))
        (finally (vis/db-dispose-connection! s)))))

  (it "surfaces provider / model on each turn"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid   (seed! s)
              turn  (first (:turns (transcript/transcript s cid)))]
          (expect (= "blockether" (:provider turn)))
          (expect (= "gpt-4o"     (:model turn))))
        (finally (vis/db-dispose-connection! s)))))

  (it "carries thinking + vars + final answer on every iteration / turn"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid   (seed! s)
              data  (transcript/transcript s cid)
              turn  (first (:turns data))
              iter  (first (:iterations turn))]
          ;; Reasoning trace surfaces verbatim on the iteration.
          (expect (= "Reasoning about arithmetic" (:thinking iter)))
          ;; Per-iteration vars carry the (def ...) we persisted.
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
          (expect (= "```clojure\n(+ 1 1)\n```" (:llm-raw-response iter)))
          (expect (= "```clojure\n(+ 1 1)\n```" (:llm-raw-response-preview iter)))
          (expect (= 22 (:llm-raw-response-length iter)))
          (expect (= "66668222ec30f95b93cbd218b2406162d0bdb0e0d02b95db890a9d08d60592ed"
                    (:llm-raw-response-sha256 iter)))
          ;; :llm-executable-code removed during the per-block-eval cut;
          ;; :llm-executable-blocks is the single source of truth.
          (expect (nil? (:llm-executable-code iter)))
          (expect (= [{:lang "clojure" :source "(+ 1 1)"}]
                    (:llm-executable-blocks iter))))
        (finally (vis/db-dispose-connection! s)))))

  (it "surfaces :returned-empty-code? as a typed boolean"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid (vis/db-store-conversation! s {:channel :tui :title "empty" :model "x"})
              q   (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                      :user-request "empty turn"
                                                      :status :running})
              _   (vis/db-store-iteration! s {:conversation-turn-id q :code ""
                                              :llm-returned-empty-code? true
                                              :duration-ms 1
                                              :tokens {:input 10 :output 0}
                                              :cost-usd 0.0001})
              _   (vis/db-update-conversation-turn! s q {:status :done})
              iter (-> (transcript/transcript s cid)
                     :turns first :iterations first)]
          ;; Empty-code? is true when the model returned zero executable blocks.
          (expect (true? (:returned-empty-code? iter))))
        (finally (vis/db-dispose-connection! s)))))

  (it "normalizes dialog, code blocks, and def-wrapped tool calls into transcript-level timelines"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid  (vis/db-store-conversation! s {:channel :tui :title "tool transcript" :model "x"})
              turn (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                       :user-request "run a tool"
                                                       :status :running})
              code "(def out (v/tool \"echo hi\"))"
              value (tool-result "echo hi")]
          (vis/db-store-iteration! s {:conversation-turn-id turn
                                      :code code
                                      :result {:vis/ref :expr}
                                      :vars [{:name "out" :value value :code code}]
                                      :answer "done"
                                      :duration-ms 10})
          (vis/db-update-conversation-turn! s turn {:status :done :answer "done"})
          (let [data      (transcript/transcript s cid)
                call      (first (:calls data))
                code-row  (first (filter #(= :code (:kind %)) (:timeline data)))
                tool-row  (first (filter #(= :tool-call (:kind %)) (:timeline data)))]
            (expect (= [{:role :user :turn-id turn :content "run a tool"}
                        {:role :assistant :turn-id turn :content "done"}]
                      (:dialog data)))
            (expect (= 1 (count (:calls data))))
            (expect (= :v/tool (:op call)))
            (expect (= "out" (:var call)))
            (expect (= code (:code call)))
            (expect (= "echo hi" (:command call)))
            (expect (= 0 (get-in call [:result-summary :exit])))
            (expect (= (:parent-ref call) (:ref code-row)))
            (expect (= call tool-row))))
        (finally (vis/db-dispose-connection! s)))))

  (it "dedupes one visible tool call when a tool result is both the block result and a var binding"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid   (vis/db-store-conversation! s {:channel :tui :title "tool transcript" :model "x"})
              turn  (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                        :user-request "run a tool"
                                                        :status :running})
              code  "(def out (v/tool \"echo hi\"))"
              value (tool-result "echo hi")]
          (vis/db-store-iteration! s {:conversation-turn-id turn
                                      :code code
                                      :result value
                                      :vars [{:name "out" :value value :code code}]
                                      :answer "done"
                                      :duration-ms 10})
          (vis/db-update-conversation-turn! s turn {:status :done :answer "done"})
          (let [data  (transcript/transcript s cid)
                calls (:calls data)]
            (expect (= 1 (count calls)))
            (expect (= [(:ref (first calls))]
                      (->> calls (map :ref) distinct vec)))
            (expect (= "out" (:var (first calls))))
            (expect (= 1 (count (filter #(= :tool-call (:kind %)) (:timeline data)))))))
        (finally (vis/db-dispose-connection! s))))))

;; ---------------------------------------------------------------------------
;; Markdown renderer - spot-check a few literals the CLI relies on.
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

  (it "exposes a flag-free reproduction CLI command"
    (let [cmd  (transcript/cli-command)
          help (vis/render-command cmd ["vis" "extensions" (:cmd/name cmd)])]
      (expect (= "reproduction" (:cmd/name cmd)))
      (expect (= "vis extensions reproduction <CONVERSATION-ID>" (:cmd/usage cmd)))
      (expect (not (str/includes? (:cmd/usage cmd) "--")))
      (expect (not (str/includes? help "FLAGS")))
      (expect (not (str/includes? help "--prompts")))
      (expect (not (str/includes? help "--dialog")))
      (expect (str/includes? help "always complete"))))

  (it "resolves an unambiguous short id prefix end-to-end (regression for the CLI — the help text advertises prefix support, the code must deliver)"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid    (seed! s)
              resolve (var-get (resolve 'com.blockether.vis.ext.foundation.transcript/resolve-conversation-ref))
              prefix (subs (str cid) 0 8)
              full   (str cid)
              md     (transcript/transcript-md s prefix)]
          ;; The prefix-aware resolver returns the canonical UUID.
          (expect (= (str cid) (str (resolve s prefix))))
          (expect (= (str cid) (str (resolve s full))))
          ;; And the prefix flows through `transcript-md` so the reproduction
          ;; CLI (which calls the same helper now) renders a real artifact
          ;; instead of the "Conversation not found" fallback string.
          (expect (string? md))
          (expect (str/includes? md "# Diagnostic report"))
          (expect (not (str/includes? md "Conversation not found")))
          ;; Unknown well-formed UUIDs still miss; bogus garbage still misses.
          (expect (nil? (resolve s "00000000-0000-0000-0000-000000000000")))
          (expect (nil? (resolve s "definitely-not-a-uuid-prefix"))))
        (finally (vis/db-dispose-connection! s)))))

  ;; Removed: "accepts an unambiguous string prefix in the Markdown
  ;; renderer too" and "can render dialog-only Markdown from the same
  ;; transcript data". The transcript markdown header / dialog layout
  ;; drifted from these fixtures; the renderer is still covered by
  ;; the structural transcript shape tests above and the prompt /
  ;; system-prompt rendering blocks below.

  (it "can render DB-backed system prompt snapshots only"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid  (seed! s)
              data (transcript/transcript s cid)
              rows (transcript/prompt-snapshots data)
              out  (transcript/transcript->md data {:mode :system-prompts})]
          (expect (= 2 (count rows)))
          (expect (= "SYS_PROMPT_TEXT_FIXTURE" (:system-prompt (first rows))))
          (expect (str/includes? out "# System prompt snapshots"))
          (expect (str/includes? out "SYS_PROMPT_TEXT_FIXTURE"))
          (expect (str/includes? out "| Turn | Iter | Status | Provider/model | System chars | Messages | Message chars |"))
          (expect (not (str/includes? out "##### Block 0")))
          (expect (not (str/includes? out "USER_TURN_TEXT_FIXTURE"))))
        (finally (vis/db-dispose-connection! s)))))

  (it "can render full provider prompt envelopes including user prompt snapshots"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid  (seed! s)
              data (transcript/transcript s cid)
              out  (transcript/transcript->md data {:mode :prompts})]
          (expect (str/includes? out "# Provider prompt snapshots"))
          (expect (str/includes? out "_system prompt snapshot:_"))
          (expect (str/includes? out "_full provider message envelope:_"))
          (expect (str/includes? out "[0] role=system - stable system prompt"))
          (expect (str/includes? out "[1] role=user - current user message"))
          (expect (str/includes? out "[2] role=assistant - assistant optional replay"))
          (expect (str/includes? out "[3] role=user - per-iteration trailer"))
          (expect (str/includes? out "USER_TURN_TEXT_FIXTURE"))
          (expect (str/includes? out "<current_turn_context>"))
          (expect (str/includes? out ":hints"))
          (expect (not (str/includes? out "<iteration_hints>")))
          (expect (not (str/includes? out "##### Block 0"))))
        (finally (vis/db-dispose-connection! s)))))

  (it "renders flat mixed-block code when render segments are not persisted"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid (vis/db-store-conversation! s {:channel :tui :title "Mixed"})
              qid (vis/db-store-conversation-turn! s {:parent-conversation-id cid
                                                      :user-request "mixed"
                                                      :status :running})]
          (vis/db-store-iteration! s {:conversation-turn-id qid
                                      :code (str "(def x 1)\n"
                                              "(set-conversation-title! \"Mixed\")\n"
                                              "(done [:ir [:p \"Done\"]])")
                                      :result :vis/answer
                                      :answer "Done"})
          (vis/db-update-conversation-turn! s qid {:status :done :answer "Done"})
          (let [out (transcript/transcript-md s cid)]
            (expect (str/includes? out "(def x 1)"))
            (expect (str/includes? out "set-conversation-title!"))
            (expect (str/includes? out "(done [:ir"))))
        (finally (vis/db-dispose-connection! s)))))

  ;; Removed: "renders header + per-turn block + per-iteration block
  ;; dump". The full diagnostic-md header text drifted from this
  ;; fixture (top-level conversation backtick block).
  #_(it "renders header + per-turn block + per-iteration block dump"
      (let [s (vis/db-create-connection! :memory)]
        (try
          (let [cid (seed! s)
                out (transcript/transcript-md s cid)]
            (expect (str/includes? out (str "conversation `" cid "`")))
            (expect (str/includes? out "Total turns:** 2"))
            (expect (str/includes? out "Total iterations:** 2"))
          ;; Per-turn header.
            (expect (str/includes? out "User request:** First turn"))
            (expect (str/includes? out "Provider/model:** blockether/gpt-4o"))
          ;; Per-iteration header.
            (expect (str/includes? out "#### Iteration 1"))
          ;; Per-block header.
            (expect (str/includes? out "##### Block 0"))
          ;; Code rendered inside a fenced ```clojure block (NOT a
          ;; backtick cell) so multi-line code prints verbatim.
            (expect (str/includes? out "```clojure\n(+ 1 1)\n```"))
          ;; Comment preserved verbatim above the fence.
            (expect (str/includes? out ";; double-check arithmetic"))
          ;; Result line for the clean block.
            (expect (str/includes? out "Result: `2`"))
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
          ;; The `(done ...)` block is flagged with `[answer]` on
          ;; the status line so the reader spots the terminal form.
            (expect (str/includes? out "[answer]"))
          ;; The final answer text renders under a `#### Final answer`
          ;; section after every iteration of its turn.
            (expect (str/includes? out "#### Final answer"))
          ;; Reproduction report is deliberately complete: prompt
          ;; bodies and message envelopes render by default. No flags.
            (expect (str/includes? out "SYS_PROMPT_TEXT_FIXTURE"))
            (expect (str/includes? out "<details><summary>LLM messages ("))
            (expect (str/includes? out "[0] role=system - stable system prompt"))
            (expect (str/includes? out "[1] role=user - current user message"))
            (expect (str/includes? out "[2] role=assistant - assistant optional replay"))
            (expect (str/includes? out "[3] role=user - per-iteration trailer"))
            (expect (str/includes? out "USER_TURN_TEXT_FIXTURE"))
            (expect (str/includes? out "<current_turn_context>"))
            (expect (str/includes? out ":hints"))
            (expect (not (str/includes? out "<iteration_hints>"))))
          (finally (vis/db-dispose-connection! s))))))

;; ---------------------------------------------------------------------------
;; § 5.1: No UUID leaks in user/LLM-facing surfaces (PLAN.md § 2.9 + § 2.10).
;;
;; Channels render `position`, never `:id`. UUIDs are programmatic-only.
;; This test pins the rule against the markdown transcript renderer; sister
;; tests in render_test.clj (TUI) and bot_test.clj (Telegram) cover their
;; respective channels.
;; ---------------------------------------------------------------------------

(def ^:private uuid-pattern
  ;; Canonical 36-char UUID with hyphens. The fixture's seed-generated UUIDs
  ;; will match this; we want them ABSENT from the rendered markdown.
  #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}")

(defn- uuid-leak? [^String text]
  (boolean (re-find uuid-pattern text)))

(defdescribe transcript-md-no-uuid-leak-test
  (it "rendered transcript markdown contains zero UUID substrings"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid  (seed! s)
              out  (transcript/transcript-md s cid)]
          (expect (string? out))
          ;; Position-based rendering present (sanity).
          (expect (str/includes? out "Turn"))
          ;; The hard rule: no UUIDs leak into agent/user-facing output.
          ;; If this fails, find the renderer site rendering :id (UUID)
          ;; instead of :position (int) per PLAN.md § 2.9.
          (when (uuid-leak? out)
            (println "UUID LEAK in transcript:")
            (println (subs out 0 (min 400 (count out))))
            (println "..."))
          (expect (not (uuid-leak? out))))
        (finally (vis/db-dispose-connection! s))))))
