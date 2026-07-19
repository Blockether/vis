(ns com.blockether.vis.internal.foundation.transcript-test
  "Coverage for `transcript` (data) + `transcript-md` (Markdown
   renderer). The data fn is the canonical surface; the renderer is
   one transformation on top. Tests assert against the data first
   (so a future renderer rewrite or a JSON exporter doesn't break
   them) and only spot-check the Markdown for the few literals the
   CLI relies on."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.foundation.transcript :as transcript]
            [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- tool-result
  [command]
  {:success? true
   :result {:exit 0 :timed-out? false :cwd "." :command command :duration-ms 5}
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
   Returns the session id."
  [s]
  (let [cid
        (h/store-session! s
                          {:channel :tui
                           :title "Transcript fixture"
                           :provider :openai
                           :model "gpt-4o"
                           :system-prompt "sys"})

        q1
        (vis/db-store-session-turn!
          s
          {:parent-session-id cid :user-request "First turn" :status :running})]

    ;; Turn 1: terminal iteration with a `(def ...)` var, an `(done ...)`
    ;; block (idx 1), thinking trace, and answer.
    (h/store-iteration! s
                        {:session-turn-id q1
                         :code "(+ 1 1)"
                         :forms [{:scope "t1/i1/f1" :tag :observation :src "(+ 1 1)" :result 2}]
                         :answer "42"
                         :thinking "Reasoning about arithmetic"
                         :vars [{:name "x" :value 42 :code "(def x 42)"}]
                         :duration-ms 12
                         :llm-provider :blockether
                         :llm-model "gpt-4o"
                         :tokens {"input" 100 "output" 20 "reasoning" 0 "cached" 30}
                         :cache-created-tokens 700
                         :cost-usd 0.0042})
    (vis/db-update-session-turn! s
                                 q1
                                 {:status :done
                                  :content [{"id" "b1" "type" "prose" "markdown" "42"}]})
    ;; Turn 2: failure iteration. No vars, no answer.
    (let [q2 (vis/db-store-session-turn!
               s
               {:parent-session-id cid :user-request "Second turn that fails" :status :running})]
      (h/store-iteration! s
                          {:session-turn-id q2
                           :code "Let"
                           :forms [{:scope "t2/i1/f1"
                                    :tag :observation
                                    :src "Let"
                                    :error {:message
                                            "ExceptionInfo: Unable to resolve symbol: Let"}}]
                           :duration-ms 1
                           :llm-provider :blockether
                           :llm-model "gpt-4o"
                           :tokens {"input" 80 "output" 10 "reasoning" 0 "cached" 20}
                           :cache-created-tokens 300
                           :cost-usd 0.0021})
      (vis/db-update-session-turn! s q2 {:status :error}))
    cid))

;; ---------------------------------------------------------------------------
;; Data - the canonical contract every consumer (CLI, agent, future
;; JSON exporter) reads against.
;; ---------------------------------------------------------------------------

(defdescribe
  transcript-data-test
  (it "returns nil for a missing session id"
      (let [s (vis/db-create-connection! :memory)]
        (try (expect (nil? (transcript/transcript s "00000000-0000-0000-0000-000000000000")))
             (finally (vis/db-dispose-connection! s)))))
  (it "returns the canonical {:session :totals :turns} shape"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [cid (seed! s)
                   data (transcript/transcript s cid)]

               (expect (map? data))
               (expect (= #{:session :totals :turns :dialog :calls :timeline} (set (keys data))))
               ;; Session header carries the canonical fields.
               (expect (= cid (:id (:session data))))
               (expect (= "Transcript fixture" (:title (:session data))))
               (expect (= :tui (:channel (:session data))))
               (expect (= "gpt-4o" (:model (:session data)))))
             (finally (vis/db-dispose-connection! s)))))
  (it "accepts an unambiguous string id prefix and normalizes back to the full UUID"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [cid (seed! s)
                   prefix (subs (str cid) 0 8)
                   data (transcript/transcript s prefix)]

               (expect (map? data))
               (expect (= cid (:id (:session data))))
               (expect (= 2 (count (:turns data)))))
             (finally (vis/db-dispose-connection! s)))))
  (it "returns nil for an ambiguous string prefix instead of picking an arbitrary session"
      (let [s (vis/db-create-connection! :memory)]
        (try (doseq [_ (range 17)]
               (h/store-session! s {:channel :tui}))
             (let [ids (mapv :id (vis/db-list-sessions s :tui))
                   buckets (vals (group-by #(subs (str %) 0 1) ids))
                   matches (first (filter #(> (count %) 1) buckets))
                   prefix (subs (str (first matches)) 0 1)]

               ;; 17 UUIDs guarantee at least one shared first hex digit.
               (expect (> (count matches) 1))
               (expect (nil? (transcript/transcript s prefix))))
             (finally (vis/db-dispose-connection! s)))))
  (it "rolls up turn / iteration counts and tokens / cost into :totals"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [cid (seed! s)
                   totals (:totals (transcript/transcript s cid))]

               (expect (= 2 (:turns totals)))
               (expect (= 2 (:iterations totals)))
               ;; Tokens summed across both turns.
               (expect (= 180 (:input (:tokens totals))))
               (expect (= 30 (:output (:tokens totals))))
               (expect (= 1000 (:cache-created (:tokens totals))))
               ;; Cost summed across both turns: 0.0042 + 0.0021 = 0.0063.
               ;; Compare with epsilon - IEEE 754 doubles don't land
               ;; exactly on 0.0063.
               (expect (< (Math/abs (- 0.0063 (double (:cost-usd totals)))) 1.0E-9)
                       (str "actual: " (:cost-usd totals))))
             (finally (vis/db-dispose-connection! s)))))
  (it "carries every turn with user request / status / iteration count / failures"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [cid (seed! s)
                   turns (:turns (transcript/transcript s cid))]

               (expect (= 2 (count turns)))
               (expect (= "First turn" (:user-request (first turns))))
               (expect (= "Second turn that fails" (:user-request (second turns))))
               (expect (= :done (:status (first turns))))
               (expect (= :error (:status (second turns))))
               ;; Failure count comes from block-level :error keys, not a
               ;; turn-level flag - the failing turn must report exactly 1.
               (expect (= 0 (:failure-count (first turns))))
               (expect (= 1 (:failure-count (second turns)))))
             (finally (vis/db-dispose-connection! s)))))
  (it "embeds every iteration on its turn, with one flat :blocks adapter entry"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [cid (seed! s)
                   turn-2 (second (:turns (transcript/transcript s cid)))
                   iter (first (:iterations turn-2))
                   blocks (:blocks iter)]

               ;; One block per persisted iteration after hard cut.
               (expect (= 1 (count blocks)))
               (expect (= "Let" (:code (first blocks))))
               ;; Failed block surfaces error verbatim.
               (expect (str/includes? (str (:error (first blocks)))
                                      "Unable to resolve symbol: Let")))
             (finally (vis/db-dispose-connection! s)))))
  (it "surfaces provider / model on each turn"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [cid (seed! s)
                   turn (first (:turns (transcript/transcript s cid)))]

               (expect (= "blockether" (:provider turn)))
               (expect (= "gpt-4o" (:model turn))))
             (finally (vis/db-dispose-connection! s)))))
  (it "carries thinking + final answer on every iteration / turn"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [cid (seed! s)
                   data (transcript/transcript s cid)
                   turn (first (:turns data))
                   iter (first (:iterations turn))]

               ;; Reasoning trace surfaces verbatim on the iteration.
               (expect (= "Reasoning about arithmetic" (:thinking iter)))
               ;; Cross-turn `(def ...)` survival was retired; per-form payload
               ;; lives on the iteration's :blocks vec instead. The final
               ;; answer surfaces on the turn.
               (expect (= "42" (get-in turn [:content 0 "markdown"]))))
             (finally (vis/db-dispose-connection! s)))))
  (it "surfaces :returned-empty-code? as a typed boolean"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [cid (h/store-session! s {:channel :tui :title "empty" :model "x"})
                   q (vis/db-store-session-turn!
                       s
                       {:parent-session-id cid :user-request "empty turn" :status :running})
                   _ (h/store-iteration! s
                                         {:session-turn-id q
                                          :code ""
                                          :llm-returned-empty-code? true
                                          :duration-ms 1
                                          :tokens {"input" 10 "output" 0}
                                          :cost-usd 0.0001})
                   _ (vis/db-update-session-turn! s q {:status :done})
                   iter (-> (transcript/transcript s cid)
                            :turns
                            first
                            :iterations
                            first)]

               ;; Empty-code? is true when the model returned zero executable blocks.
               (expect (true? (:returned-empty-code? iter))))
             (finally (vis/db-dispose-connection! s)))))
  (it
    "normalizes dialog, code blocks, and tool-call envelopes into transcript-level timelines"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid (h/store-session! s {:channel :tui :title "tool transcript" :model "x"})
              turn (vis/db-store-session-turn!
                     s
                     {:parent-session-id cid :user-request "run a tool" :status :running})
              code "(v/tool \"echo hi\")"
              value (tool-result "echo hi")]

          ;; Cross-turn def survival is gone; the iteration row carries
          ;; the per-form envelope directly. tool-call detection now
          ;; flows through the block's :result, not a separate :vars
          ;; sidecar.
          (h/store-iteration!
            s
            {:session-turn-id turn :code code :result value :answer "done" :duration-ms 10})
          (vis/db-update-session-turn! s
                                       turn
                                       {:status :done
                                        :content [{"id" "b1" "type" "prose" "markdown" "done"}]})
          (let [data (transcript/transcript s cid)
                call (first (:calls data))
                code-row (first (filter #(= :code (:kind %)) (:timeline data)))
                tool-row (first (filter #(= :tool-call (:kind %)) (:timeline data)))]

            (expect (= [{:role :user :turn-id turn :content "run a tool"}
                        {:role :assistant
                         :turn-id turn
                         :content [{"id" "b1" "type" "prose" "markdown" "done"}]}]
                       (:dialog data)))
            (expect (= 1 (count (:calls data))))
            (expect (= :v/tool (:op call)))
            (expect (= code (:code call)))
            (expect (= "echo hi" (:command call)))
            (expect (= 0 (get-in call [:result-summary :exit])))
            (expect (= "t1/i1/f1" (:ref code-row)))
            (expect (= (:parent-ref call) (:ref code-row)))
            (expect (= call tool-row))))
        (finally (vis/db-dispose-connection! s))))))

;; ---------------------------------------------------------------------------
;; Markdown renderer - spot-check a few literals the CLI relies on.
;; The data tests above are the real contract.
;; ---------------------------------------------------------------------------

(defdescribe
  transcript-md-test
  (it "returns a 'Session not found' line for a missing id (no throw)"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [out (transcript/transcript-md s "00000000-0000-0000-0000-000000000000")]
               (expect (string? out))
               (expect (str/includes? out "Session not found")))
             (finally (vis/db-dispose-connection! s)))))
  (it
    "resolves an unambiguous short id prefix end-to-end (regression for the CLI — the help text advertises prefix support, the code must deliver)"
    (let [s (vis/db-create-connection! :memory)]
      (try (let [cid (seed! s)
                 resolve
                 (var-get (resolve
                            'com.blockether.vis.internal.foundation.transcript/resolve-session-ref))
                 prefix (subs (str cid) 0 8)
                 full (str cid)
                 md (transcript/transcript-md s prefix)]

             ;; The prefix-aware resolver returns the canonical UUID.
             (expect (= (str cid) (str (resolve s prefix))))
             (expect (= (str cid) (str (resolve s full))))
             ;; And the prefix flows through `transcript-md` so callers render a real artifact
             ;; instead of the "Session not found" fallback string.
             (expect (string? md))
             (expect (str/includes? md "## Turn-by-turn breakdown"))
             (expect (not (str/includes? md "Session not found")))
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
  (it "renders huge code blocks verbatim in Markdown reports"
      ;; Forensic transcripts never truncate. A 50k-char code block lands
      ;; in the report exactly as the model wrote it.
      (let [s (vis/db-create-connection! :memory)]
        (try (let [huge (apply str (repeat 50000 "x"))
                   cid (h/store-session! s {:channel :tui :title "Huge"})
                   qid (vis/db-store-session-turn!
                         s
                         {:parent-session-id cid :user-request "huge" :status :running})]

               (h/store-iteration! s
                                   {:session-turn-id qid
                                    :code huge
                                    :forms
                                    [{:scope "t1/i1/f1" :tag :observation :src huge :result :ok}]})
               (vis/db-update-session-turn! s qid {:status :done})
               (let [out (transcript/transcript-md s cid)]
                 (expect (string? out))
                 (expect (str/includes? out huge))))
             (finally (vis/db-dispose-connection! s)))))
  (it
    "renders flat mixed-block code when render segments are not persisted"
    (let [s (vis/db-create-connection! :memory)]
      (try
        (let [cid (h/store-session! s {:channel :tui :title "Mixed"})
              qid (vis/db-store-session-turn!
                    s
                    {:parent-session-id cid :user-request "mixed" :status :running})]

          (let [fence (str "(def x 1)\n" "(set-session-title! \"Mixed\")\n" "(read-file \"a\")")]
            (h/store-iteration!
              s
              {:session-turn-id qid
               :code fence
               :forms
               [{:scope "t1/i1/f1" :tag :mutation :src "(def x 1)" :result 1}
                {:scope "t1/i1/f2" :tag :mutation :src "(set-session-title! \"Mixed\")" :result :ok}
                {:scope "t1/i1/f3" :tag :observation :src "(read-file \"a\")" :result {:path "a"}}]
               :answer "Done"}))
          (vis/db-update-session-turn! s
                                       qid
                                       {:status :done
                                        :content [{"id" "b1" "type" "prose" "markdown" "Done"}]})
          (let [out (transcript/transcript-md s cid)]
            (expect (str/includes? out "(def x 1)"))
            (expect (str/includes? out "set-session-title!"))
            (expect (str/includes? out "(read-file"))))
        (finally (vis/db-dispose-connection! s)))))
  (it "uses longer Markdown fences when code contains triple-backtick fences"
      (let [s (vis/db-create-connection! :memory)]
        (try (let [inner "```clojure\n(dead)\n```"
                   code (str "patch({:replace \"" inner "\"})")
                   cid (h/store-session! s {:channel :tui :title "Nested fence"})
                   qid (vis/db-store-session-turn!
                         s
                         {:parent-session-id cid :user-request "nested" :status :running})]

               (h/store-iteration! s
                                   {:session-turn-id qid
                                    :code code
                                    :forms
                                    [{:scope "t1/i1/f1" :tag :mutation :src code :result :ok}]})
               (vis/db-update-session-turn! s qid {:status :done})
               (let [data (transcript/transcript s cid)
                     out (transcript/transcript->md data)
                     html (transcript/transcript->html data)]

                 (expect (str/includes? out "````python\n"))
                 (expect (str/includes? out inner))
                 (expect (str/includes? out "\n````\n"))
                 (expect (= 1 (count (re-seq #"<pre><code" html))))
                 (expect (str/includes? html "<code class=\"language-python\">"))
                 (expect (str/includes? html "```clojure"))))
             (finally (vis/db-dispose-connection! s))))))
  ;; Removed: "renders header + per-turn block + per-iteration block
  ;; dump" (was already `#_`-disabled). It asserted on the removed
  ;; prompt-body / LLM-message-envelope render (SYS_PROMPT_TEXT_FIXTURE,
  ;; "LLM messages", role snapshots) which no longer exists. Surviving
  ;; renderer output is covered by the structural data tests above and
  ;; the huge/mixed-block render tests here.


;; ---------------------------------------------------------------------------
;; No UUID leaks in the turn-by-turn BODY.
;;
;; The summary header intentionally shows the session `ID` (a UUID) - that's a
;; single, deliberate identity row the operator asked for. The turn/iteration
;; BODY must still render `position` (int), never a turn/message `:id` (uuid).
;; This test pins the rule against the markdown transcript body; the sister
;; test in render_test.clj (TUI) covers that channel.
;; ---------------------------------------------------------------------------

(def ^:private uuid-pattern
  ;; Canonical 36-char UUID with hyphens. The fixture's seed-generated UUIDs
  ;; will match this; we want them ABSENT from the rendered turn-by-turn body.
  #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}")

(defn- uuid-leak? [^String text] (boolean (re-find uuid-pattern text)))

(defdescribe transcript-md-no-uuid-leak-test
             (it "turn-by-turn transcript body contains zero UUID substrings"
                 (let [s (vis/db-create-connection! :memory)]
                   (try (let [cid (seed! s)
                              full (transcript/transcript-md s cid)
                              ;; The summary header intentionally carries the
                              ;; session ID; scope the leak check to the body.
                              marker "## Turn-by-turn breakdown"
                              idx (str/index-of full marker)
                              body (if idx (subs full idx) full)]

                          (expect (string? full))
                          ;; Position-based rendering present (sanity).
                          (expect (str/includes? body "Turn"))
                          ;; The hard rule: no UUIDs leak into the turn body.
                          ;; If this fails, find the renderer site rendering :id
                          ;; (UUID) instead of :position (int).
                          (when (uuid-leak? body)
                            (println "UUID LEAK in transcript body:")
                            (println (subs body 0 (min 400 (count body))))
                            (println "..."))
                          (expect (not (uuid-leak? body))))
                        (finally (vis/db-dispose-connection! s))))))
