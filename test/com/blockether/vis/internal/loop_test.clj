(ns com.blockether.vis.internal.loop-test
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.ctx-loop :as ctx-loop]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.env-python :as env]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.provider-zones :as provider-zones]
   [com.blockether.vis.internal.toggles :as toggles]
   [com.blockether.vis.internal.workspace :as workspace]
   [lazytest.core :refer [defdescribe describe it expect throws?]]))

(defn- captured-ask-code-opts
  [opts]
  (let [seen (atom nil)]
    (with-redefs-fn {#'lp/get-router (fn [] ::router)
                     #'svar/ask-code! (fn [router opts]
                                        (reset! seen {:router router :opts opts})
                                        {:blocks [] :raw ""})}
      #(lp/ask-code! opts))
    @seen))

(def ^:private provider-error-explanation
  (deref #'lp/provider-error-explanation))

(def ^:private collect-iteration-start-hints
  (deref #'lp/collect-iteration-start-hints))

(def ^:private ask-result->api-usage
  (deref #'lp/ask-result->api-usage))

(def ^:private ask-code-block-observation
  (deref #'lp/ask-code-block-observation))

(def ^:private preserved-thinking-replay-messages
  (deref #'lp/preserved-thinking-replay-messages))

(def ^:private compatible-preserved-thinking-trailer-iters
  (deref #'lp/compatible-preserved-thinking-trailer-iters))

(def ^:private max-tokens-exceeded-error?
  (deref #'lp/max-tokens-exceeded-error?))

(def ^:private bumped-max-tokens-extra-body
  (deref #'lp/bumped-max-tokens-extra-body))

(def ^:private llm-provider-error-context
  (deref #'lp/llm-provider-error-context))

(def ^:private previous-turn-context
  (deref #'lp/previous-turn-context))

(def ^:private previous-request-usage
  (deref #'lp/previous-request-usage))

(def ^:private call-provider-with-interrupt-retry!
  (deref #'lp/call-provider-with-interrupt-retry!))

(def ^:private run-normal-turn!
  (deref #'lp/run-normal-turn!))

(def ^:private maybe-auto-title!
  (deref #'lp/maybe-auto-title!))

(def ^:private seed-current-turn-goal!
  (deref #'lp/seed-current-turn-goal!))

(def ^:private current-turn-goal-key
  (deref #'lp/current-turn-goal-key))

(def ^:private assistant-replayable-iteration?
  (deref #'lp/assistant-replayable-iteration?))

(defdescribe provider-request-zone-accounting-test
  (it "classifies the DAG-aware provider wire shape by cache zone"
    (let [messages [{:role "system"
                     :content ";; -- SYSTEM-PROMPT --\ncore"}
                    {:role "system"
                     :content ";; -- TURN-SYSTEM-CONTEXT --\nextensions"}
                    {:role "user"
                     :content "<results scope=\"t1/i1/f1\">\ncat(\"x\")\nok\n</results>"}
                    {:role "user"
                     :content (str ";; -- PREVIOUS-TURN-CONTEXT --\nold\n"
                                ";; -- CURRENT-USER-MESSAGE --\nnew")}
                    {:role "assistant"
                     :content "advance({...})"}
                    {:role "user"
                     :content (str "<results scope=\"t2/i1/f1\">\n"
                                "{\"status\":\"accepted\",\"graph_diff\":{},"
                                "\"resolved_evidence\":[]}\n"
                                "</results>")}
                    {:role "user"
                     :content "<context>\n{}\n</context>"}]
          zones (provider-zones/provider-request-zones messages)]
      (expect (= [:stable-system
                  :capability-system-context
                  :frozen-ledger
                  :previous-turn-context
                  :current-user-request
                  :current-turn-ledger
                  :dag-ledger
                  :mutable-context]
                (mapv :zone zones)))
      (expect (= [:stable-prefix
                  :stable-prefix
                  :append-only-prefix
                  :turn-prefix
                  :turn-prefix
                  :append-only-prefix
                  :append-only-prefix
                  :mutable-tail]
                (mapv :cache-class zones)))))

  (it "classifies folded result pins as explicit compaction ledger entries"
    (let [zones (provider-zones/provider-request-zones
                  [{:role "system" :content ";; -- SYSTEM-PROMPT --\ncore"}
                   {:role "user"
                    :content (str "<results scope=\"t1/i1..t1/i3\" folded>\n"
                               "folded summary\n"
                               "</results>")}
                   {:role "user"
                    :content ";; -- CURRENT-USER-MESSAGE --\nnext"}])]
      (expect (= :compaction-ledger (:zone (second zones))))
      (expect (= :ctx/compaction-ledger (:source (second zones))))))

  (it "labels assistant text replay separately from preserved thinking"
    (let [zones (provider-zones/provider-request-zones
                  [{:role "user" :content ";; -- CURRENT-USER-MESSAGE --\nfix it"}
                   {:role "assistant"
                    :content [{:type "thinking" :thinking "considering"}
                              {:type "text" :text "<context>\n...\n</context>\nrg({})"}]}])]
      (expect (= :current-turn-ledger (:zone (second zones))))
      (expect (= :svar/assistant-message (:source (second zones)))))))

;; NOTE: `dag-stream-contract-guard-test` was removed with the FENCED-mode
;; switch — the stream-time narration guard it covered conflicted with fenced
;; prose-immunity (prose outside the ```python fence is now legal and dropped at
;; extraction), so the guard itself was deleted. See the note above `truncate`
;; in loop.clj.

(defdescribe provider-stream-rewind-retry-test
  (it "rewinds streamed reasoning and retries the provider call before eval"
    (let [env    (lp/create-environment ::router {:db :memory})
          calls  (atom 0)
          chunks (atom [])]
      (try
        (with-redefs [svar/ask-code!
                      (fn [_router opts]
                        (case (swap! calls inc)
                          1 (do
                              ;; partial DAG-compliant code (passes the stream
                              ;; contract guard) cut off by a mid-stream drop
                              ((:on-chunk opts) {:reasoning "dead thinking"
                                                 :content "advance({\"tasks\""})
                              (throw (ex-info "Stream connection error: closed"
                                       {:type :svar.core/http-error
                                        :stream? true
                                        :content-acc-len 15
                                        :reasoning-acc-len 13
                                        :reasoning "dead thinking"
                                        :partial-content "advance({\"tasks\""})))
                          2 (do
                              ((:on-chunk opts) {:reasoning "fresh thinking"})
                              {:blocks [{:lang "python"
                                         :source "advance({\"tasks\": {\"respond\": {\"status\": \"done\", \"evidence\": \"ok\"}}, \"answer\": \"ok\", \"done\": True})"}]
                               :raw "```python\nadvance({\"tasks\": {\"respond\": {\"status\": \"done\", \"evidence\": \"ok\"}}, \"answer\": \"ok\", \"done\": True})\n```"
                               :reasoning "fresh thinking"
                               :tokens {}})))]
          (let [result (lp/run-iteration env []
                         {:iteration 0
                          :resolved-model {:provider :zai-coding-plan :name "glm-5.1"}
                          :on-chunk #(swap! chunks conj %)})
                reset-chunk (first (filter #(= :provider-retry-reset (:phase %)) @chunks))
                fresh-reasoning (last (filter #(= :reasoning (:phase %)) @chunks))]
            (expect (= 2 @calls))
            (expect (= "ok" (get-in result [:final-result :answer :answer])))
            (expect (some? reset-chunk))
            (expect (= "fresh thinking" (:thinking fresh-reasoning)))
            (expect (= "fresh thinking" (:delta fresh-reasoning)))
            (expect (= [:llm.routing/provider-retry]
                      (mapv :event/type (:llm-routing-trace result))))))
        (finally
          (lp/dispose-environment! env))))))

(defdescribe provider-interrupt-retry-test
  (it "retries a provider interrupt once when user did not cancel"
    (let [calls (atom 0)
          out   (call-provider-with-interrupt-retry!
                  {:cancel-atom (atom false)}
                  6
                  (fn []
                    (if (= 1 (swap! calls inc))
                      (throw (InterruptedException. "java.lang.InterruptedException"))
                      ::ok)))]
      (expect (= ::ok out))
      (expect (= 2 @calls))))

  (it "does not retry a provider interrupt after user cancel"
    (let [calls (atom 0)]
      (try
        (call-provider-with-interrupt-retry!
          {:cancel-atom (atom true)}
          6
          (fn []
            (swap! calls inc)
            (throw (InterruptedException. "cancel"))))
        (expect false)
        (catch InterruptedException _
          (expect (= 1 @calls)))))))

(defdescribe previous-turn-context-test
  (it "loads the latest completed prior answer for short follow-ups"
    (with-redefs [persistance/db-list-session-turns
                  (fn [_db-info session-id]
                    (expect (= "s1" session-id))
                    [{:id "t1" :status :done :user-request "What changed?"
                      :answer-markdown "First answer"}
                     {:id "t2" :status :done :user-request "Which option?"
                      :answer-markdown "Use option B"}
                     {:id "t3" :status :running :user-request "yes"}])]
      (expect (= {:user-request "Which option?" :answer "Use option B"}
                (previous-turn-context {:session-id "s1" :db-info ::db} "t3")))))

  (it "skips current/running/blank-answer turns"
    (with-redefs [persistance/db-list-session-turns
                  (constantly
                    [{:id "t1" :status :done :user-request "old"
                      :answer-markdown ""}
                     {:id "t2" :status :running :user-request "now"
                      :answer-markdown "partial"}])]
      (expect (nil? (previous-turn-context {:session-id "s1" :db-info ::db} "t2"))))))

(defdescribe previous-request-usage-test
  (it "loads latest persisted request before current turn for iter-1 utilization"
    (with-redefs [persistance/db-list-session-turns
                  (fn [_db-info session-id]
                    (expect (= "s1" session-id))
                    [{:id "t1" :position 1}
                     {:id "t2" :position 2}
                     {:id "t3" :position 3 :status :running}])
                  persistance/db-list-session-turn-iterations
                  (fn [_db-info turn-id]
                    (case turn-id
                      "t2" [{:position 1 :input-tokens 42000}
                            {:position 2 :input-tokens 51000}]
                      "t1" [{:position 1 :input-tokens 10000}]
                      []))]
      (expect (= {:last-request-tokens 51000
                  :last-request-turn-id "t2"
                  :last-request-turn-position 2
                  :last-request-iteration 2}
                (previous-request-usage {:session-id "s1" :db-info ::db} "t3")))))

  (it "returns nil when no prior iteration has input tokens"
    (with-redefs [persistance/db-list-session-turns
                  (constantly [{:id "t1" :position 1}
                               {:id "t2" :position 2}])
                  persistance/db-list-session-turn-iterations
                  (constantly [{:position 1 :input-tokens 0}])]
      (expect (nil? (previous-request-usage {:session-id "s1" :db-info ::db} "t2"))))))

(defdescribe stamp-utilization-monotonic-test
  ;; Regression: the stamp used to (dissoc :engine/utilization) on a nil
  ;; measurement, so a transient req=0 (iter-1 seed miss / errored iter)
  ;; BLANKED an already-shown :session/utilization — the "sometimes works,
  ;; sometimes doesn't" flicker. The stamp must be monotonic.
  (let [stamp (var-get #'lp/stamp-utilization!)
        util1 {:last-request-tokens 5000 :pct-of-limit 3}
        util2 {:last-request-tokens 9000 :pct-of-limit 5}]

    (it "stamps a real measurement onto the ctx-atom"
      (let [ca (atom {})]
        (stamp ca util1)
        (expect (= util1 (:engine/utilization @ca)))))

    (it "NEVER blanks an existing value on a transient nil measurement"
      (let [ca (atom {:engine/utilization util1})]
        (stamp ca nil)
        (expect (= util1 (:engine/utilization @ca)))))

    (it "upgrades to a fresh measurement when one arrives"
      (let [ca (atom {:engine/utilization util1})]
        (stamp ca util2)
        (expect (= util2 (:engine/utilization @ca)))))

    (it "is a no-op on a nil ctx-atom"
      (expect (nil? (stamp nil util1))))))

(defdescribe turn-position-state-test
  (it "seeds turn-state with persisted turn position before iteration render"
    (let [seen (atom nil)
          env {:db-info ::db
               :session-id "s1"
               :turn-state-atom (ctx-loop/make-turn-state-atom)}]
      (with-redefs [persistance/db-store-session-turn!
                    (fn [_db opts]
                      (expect (= {:parent-session-id "s1"
                                  :user-request "follow up"
                                  :status :running}
                                opts))
                      "turn-3")
                    persistance/db-update-session-turn!
                    (fn [_db turn-id opts]
                      (reset! seen {:turn-id turn-id :opts opts}))
                    lp/session-turn-position
                    (fn [_env turn-id]
                      (expect (= "turn-3" turn-id))
                      3)
                    lp/iteration-loop
                    (fn [env* user-request opts]
                      (expect (= "follow up" user-request))
                      (expect (= "turn-3" (:session-turn-id opts)))
                      (expect (= 3 (:turn-position (ctx-loop/read-turn-state env*))))
                      (expect (nil? (:iteration (ctx-loop/read-turn-state env*))))
                      {:iteration-count 1 :duration-ms 0})]
        (let [result (run-normal-turn! env "follow up" {})]
          (expect (= "turn-3" (:session-turn-id result)))
          (expect (= "turn-3" (:turn-id @seen))))))))

(defdescribe max-tokens-exceeded-retry-test
  (it "recognises :svar.llm/max-tokens-exceeded as retry-able"
    (let [e (ex-info "max_tokens hit" {:type :svar.llm/max-tokens-exceeded
                                       :output-tokens 2048
                                       :reasoning-length 1900})]
      (expect (true? (max-tokens-exceeded-error? e)))))

  (it "does not confuse other svar errors with the max-tokens variant"
    ;; `:svar.llm/empty-content` is the genuine \"model returned nothing useful\"
    ;; failure mode. It must NOT trigger the max-tokens-bump retry path — that
    ;; would burn provider tokens without any chance of fixing the underlying
    ;; problem (the model is confused, more budget will not help).
    (let [e (ex-info "blank" {:type :svar.llm/empty-content})]
      (expect (false? (max-tokens-exceeded-error? e))))
    (let [e (ex-info "http" {:type :svar.core/http-error :status 500})]
      (expect (false? (max-tokens-exceeded-error? e)))))

  (it "doubles max_tokens from the reported `:output-tokens`"
    ;; Provider reports the exact number it cut off at — doubling that gives
    ;; the next attempt enough headroom in the common case (reasoning ate
    ;; roughly all of the budget).
    (expect (= {:max_tokens 4096} (bumped-max-tokens-extra-body nil 2048)))
    (expect (= {:max_tokens 16000} (bumped-max-tokens-extra-body nil 8000)))
    ;; Preserves caller-supplied extra-body keys so the bump does not drop
    ;; their overrides (e.g. `:store false` for Codex).
    (expect (= {:store false :max_tokens 4096}
              (bumped-max-tokens-extra-body {:store false} 2048))))

  (it "falls back to 8192 when no previous max is known"
    ;; Defensive: the error carries no `:output-tokens` (older svar version,
    ;; or non-streaming path). Use a moderate-sized cap as fallback so we
    ;; don't accidentally explode the request body.
    (expect (= {:max_tokens 16384} (bumped-max-tokens-extra-body nil nil)))))

(defdescribe llm-provider-error-context-test
  ;; Iteration-error-data shape (built by `format-exception`):
  ;;   {:class "..."      — exception class name
  ;;    :message "..."    — ex-message
  ;;    :data {...}       — raw `(ex-data t)` from svar, untouched
  ;;    :context {...}}   — vis loop ctx snapshot
  ;; So predicate / context helpers consume `(:data iter-err)` for any
  ;; svar-side ex-info keys, NOT top-level. Tests reflect that.
  (it "surfaces dedicated copy + hint for :svar.llm/max-tokens-exceeded"
    (let [iter-err {:type :svar.llm/max-tokens-exceeded
                    :data {:reasoning-length 1900
                           :output-tokens 2048}}
          ctx (llm-provider-error-context 3 iter-err)]
      (expect (= :llm-provider/max-tokens-exhausted (:type ctx)))
      (expect (= 1900 (:reasoning-length ctx)))
      (expect (= 2048 (:output-tokens ctx)))
      (expect (str/includes? (:message ctx) "max_tokens"))
      (expect (str/includes? (:message ctx) "hidden reasoning"))
      (expect (str/includes? (:hint ctx) ":session/hints"))
      (expect (str/includes? (:hint ctx) "canonical"))
      (expect (not (str/includes? (:hint ctx) "v/strategy")))
      (expect (not (str/includes? (:hint ctx) ":start/:max-lines")))))

  (it "keeps the legacy `:llm-provider/output-budget-exhausted` mapping"
    ;; Anthropic native `:svar.core/stream-incomplete + :reason
    ;; max_output_tokens` is detected through `:data` (nested), not
    ;; top-level — `format-exception` puts raw `ex-data` under `:data`.
    (let [iter-err {:data {:type :svar.core/stream-incomplete
                           :reason "max_output_tokens"}}
          ctx (llm-provider-error-context 2 iter-err)]
      (expect (= :llm-provider/output-budget-exhausted (:type ctx))))))

(defn- stub-iter
  "Build a synthetic trailer-iters entry for preserved-thinking tests.
   `id` is any unique label for the position; `provider`/`model` control
   how `compatible-preserved-thinking-trailer-iters` filters; the rest
   default to a same-model, replay-eligible canonical thinking block."
  [{:keys [id provider model thinking signature replay?]
    :or   {provider :zai-coding-plan model "glm-5.1" replay? true}}]
  [id
   {:assistant-message {:role "assistant"
                        :content [{:type "thinking"
                                   :thinking (or thinking (str "think-" id))
                                   :thinking-signature (or signature (str "sig-" id))}]}
    :llm-provider provider
    :llm-model model
    :preserved-thinking/replay? replay?}])

(defdescribe preserved-thinking-replay-test
  (it "returns every compatible assistant message in arrival order"
    ;; Why every message, not just the last: GLM clear_thinking,
    ;; Anthropic HMAC chains, and OpenAI Responses encrypted reasoning
    ;; all require the full assistant chain since the last user turn.
    ;; Returning only the latest step (the pre-fix behaviour) made GLM
    ;; re-derive scratch state each iteration — see session 3102ad16
    ;; where `cached_tokens` stayed pinned at 2368 across 26 iterations.
    (let [target  {:provider :zai-coding-plan :model "glm-5.1"}
          trailer (mapv #(stub-iter {:id %}) [1 2 3])
          compat  (compatible-preserved-thinking-trailer-iters trailer target)
          replays (preserved-thinking-replay-messages compat)]
      (expect (= 3 (count compat)))
      (expect (= 3 (count replays)))
      (expect (= ["think-1" "think-2" "think-3"]
                (mapv (fn [m] (-> m :content first :thinking)) replays)))))

  (it "drops iterations from a different provider/model"
    ;; Cross-provider replay is forbidden: provider-native thinking
    ;; signatures are not portable (z.ai = raw text, Anthropic = HMAC,
    ;; OpenAI Responses = JSON reasoning item). The compatible filter
    ;; must reject mismatches before this fn sees them.
    (let [target  {:provider :zai-coding-plan :model "glm-5.1"}
          trailer [(stub-iter {:id 1})
                   (stub-iter {:id 2 :provider :anthropic :model "claude-sonnet-4.6"})
                   (stub-iter {:id 3})]
          compat  (compatible-preserved-thinking-trailer-iters trailer target)
          replays (preserved-thinking-replay-messages compat)]
      (expect (= 2 (count replays)))
      (expect (= ["think-1" "think-3"]
                (mapv (fn [m] (-> m :content first :thinking)) replays)))))

  (it "drops iterations explicitly flagged :preserved-thinking/replay? false"
    ;; Cross-turn trailer seeds carry the opt-out flag so historical
    ;; iterations stay visible in transcripts but their opaque thinking
    ;; state is not replayed into a new user turn.
    (let [target  {:provider :zai-coding-plan :model "glm-5.1"}
          trailer [(stub-iter {:id 1 :replay? false})
                   (stub-iter {:id 2 :replay? true})]
          compat  (compatible-preserved-thinking-trailer-iters trailer target)
          replays (preserved-thinking-replay-messages compat)]
      (expect (= 1 (count replays)))
      (expect (= ["think-2"]
                (mapv (fn [m] (-> m :content first :thinking)) replays)))))

  (it "returns empty when no iteration has an :assistant-message"
    ;; Iterations that errored before the model produced a usable
    ;; assistant turn (e.g. provider HTTP 4xx mid-stream) lack
    ;; `:assistant-message`; the compatible filter drops them so the
    ;; replay never tries to send an empty/partial block.
    (let [target  {:provider :zai-coding-plan :model "glm-5.1"}
          trailer [[1 {:llm-provider :zai-coding-plan
                       :llm-model "glm-5.1"
                       :preserved-thinking/replay? true}]]
          compat  (compatible-preserved-thinking-trailer-iters trailer target)
          replays (preserved-thinking-replay-messages compat)]
      (expect (zero? (count compat)))
      (expect (zero? (count replays))))))

;; multi-fence-hint / attach-multi-fence-hint / empty-code-error-with-observation
;; tests removed: those fns were deleted with the fenced-era machinery (lenient
;; mode yields <=1 block, so multi-fence merge + fence-dropped diagnostics are
;; unreachable). See refactor "remove dead fenced-era code-block machinery".

(defdescribe token-usage-normalization-test
  (it "preserves Anthropic cache write tokens from svar token maps"
    (expect (= {:prompt_tokens 112
                :completion_tokens 69
                :completion_tokens_details {:reasoning_tokens 0}
                :prompt_tokens_details {:cached_tokens 0
                                        :cache_creation_tokens 8777}}
              (ask-result->api-usage {:tokens {:input 112
                                               :output 69
                                               :cached 0
                                               :cache-created 8777}})))))

(defdescribe ask-code-block-observation-test
  (it "reports the block count (lenient mode: only the count is meaningful)"
    (expect (= {:form-count 1}
              (ask-code-block-observation {:blocks [{:source "(def x 1)" :lang "clojure"}]})))
    (expect (= {:form-count 0} (ask-code-block-observation {:blocks []})))
    (expect (= {:form-count 0} (ask-code-block-observation {})))))

(defdescribe iteration-start-hook-test
  (it "collects active :turn.iteration/start hooks as hook-task descriptors (D12)"
    (let [seen (atom nil)
          ext {:ext/name "test.hooks"
               :ext/hooks [{:id :test/title
                            :doc "title"
                            :phase :turn.iteration/start
                            :fn (fn [ctx]
                                  (reset! seen ctx)
                                  {:title "set title"
                                   :importance :warn})}
                           {:id :test/answer
                            :doc "answer"
                            :phase :turn.answer/validate
                            :fn (fn [_] {:reject true})}
                           {:id :test/no-title
                            :doc "missing title—rejected"
                            :phase :turn.iteration/start
                            :fn (fn [_] {:importance :warn})}]}
          ctx {:session-title nil
               :title-refresh? true
               :turn-position 1}
          hits (collect-iteration-start-hints {} [ext] ctx)]
      ;; Only the title-bearing :turn.iteration/start hook materialises;
      ;; the :turn.answer/validate hook is the wrong phase and the
      ;; title-less hook is dropped. Self-asserted done means no
      ;; validator-fn and no :specs in the hook-task descriptor.
      (expect (= [{:id :test/title
                   :task {:title "set title"
                          :status :todo
                          :source :hook
                          :hook-id :test/title
                          :importance :warn}}]
                hits))
      (expect (= ctx @seen))))

  (it "does NOT re-title when a real title already exists (generate once, never re-title)"
    (let [env (lp/create-environment {:providers []} {:db :memory :title "Old focus"})]
      (try
        ;; svar/ask! must NEVER fire — guard it so a regression to re-titling
        ;; throws instead of silently passing.
        (with-redefs [svar/ask! (fn [& _] (throw (ex-info "must not re-title" {})))]
          (expect (nil? (maybe-auto-title! env "some unrelated new request")))
          (expect (= "Old focus" @(:session-title-atom env))))
        (finally
          (lp/dispose-environment! env)))))

  (it "auto-title treats Untitled placeholders as missing previous titles"
    (let [seen (atom nil)
          router-stub {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}]}
          env  (lp/create-environment router-stub {:db :memory :title "Untitled"})]
      (try
        (with-redefs [svar/ask!
                      (fn [_router opts]
                        (reset! seen opts)
                        {:result {:title "Current Bug Triage"}})]
          (let [f (maybe-auto-title! env "Wez to sprawdz")]
            @f
            (expect (= "Current Bug Triage" @(:session-title-atom env)))
            (expect (str/includes? (-> @seen :messages second :content)
                      "Previous title: <none>"))))
        (finally
          (lp/dispose-environment! env)))))

  (it "auto-title declares the preferred plan order, then deterministic fallback when the chain fails"
    (let [router-stub {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}
                                   {:id :openai-codex :models [{:name "gpt-5.3-codex"}]}]}
          seen (atom nil)
          env (lp/create-environment router-stub {:db :memory :title "Untitled"})]
      (try
        ;; svar owns the per-provider walk now; the host makes ONE call that
        ;; declares `:prefer-providers`. A thrown call → deterministic fallback.
        (with-redefs [svar/ask!
                      (fn [_router opts]
                        (reset! seen opts)
                        (throw (ex-info "Exceptional status code: 400" {})))]
          (let [f (maybe-auto-title! env "1dff1f5a-76dc-431e-ad2b-97af14c731f1 can you check why TUI title is missing?")]
            @f
            (expect (= [:zai-coding-plan :openai-codex]
                      (take 2 (get-in @seen [:routing :prefer-providers]))))
            (expect (= "can you check why TUI title is" @(:session-title-atom env)))))
        (finally
          (lp/dispose-environment! env)))))

  (it "set_session_title is NOT a tool — the title is host-generated"
    (let [env (lp/create-environment ::router {:db :memory})]
      (try
        ;; The model has no `set_session_title` binding; calling it raises
        ;; (NameError) and surfaces as a structured eval error.
        (let [bad (env/run-python-block (:python-context env)
                    "set_session_title(\"Liveness check\")")]
          (expect (some? (:error bad))))
        (finally
          (lp/dispose-environment! env))))))

(defdescribe provider-error-explanation-test
  (it "tells users to re-authenticate or update keys on provider auth failures"
    (let [text (provider-error-explanation
                 {:message "API authentication failed. Check your API key. (Original: Exceptional status code: 401)"
                  :data {:status 401
                         :body "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"Invalid authentication credentials\"}}"}})]
      (expect (str/includes? text "provider rejected credentials"))
      (expect (str/includes? text "Provider message: Invalid authentication credentials"))
      (expect (str/includes? text "NEXT STEP: re-authenticate this provider or update its API key"))
      (expect (str/includes? text "Ctrl+K -> Model / Providers"))
      (expect (str/includes? text "vis providers auth")))))

(defdescribe ask-code-idle-timeout-test
  (it "uses a sixty-second TTFT timeout and three-minute idle timeout by default"
    (expect (= (* 60 1000) lp/ASK_CODE_TTFT_TIMEOUT_MS))
    (expect (= (* 3 60 1000) lp/ASK_CODE_IDLE_TIMEOUT_MS))
    (let [{:keys [router opts]} (captured-ask-code-opts {:lang "clojure" :messages []})]
      (expect (= ::router router))
      (expect (= lp/ASK_CODE_TTFT_TIMEOUT_MS (:ttft-timeout-ms opts)))
      (expect (= lp/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts)))
      ;; Semantic timeout is now auto-added by `with-default-ask-code-idle-timeout`
      ;; (default 4min, catches transport-alive-but-model-silent stalls).
      (expect (= lp/ASK_CODE_SEMANTIC_TIMEOUT_MS (:semantic-timeout-ms opts)))))

  (it "preserves explicit ask-code TTFT and idle timeout overrides"
    (expect (= 77 (:ttft-timeout-ms (:opts (captured-ask-code-opts {:ttft-timeout-ms 77})))))
    (expect (contains? (:opts (captured-ask-code-opts {:ttft-timeout-ms nil})) :ttft-timeout-ms))
    (expect (nil? (:ttft-timeout-ms (:opts (captured-ask-code-opts {:ttft-timeout-ms nil})))))
    (expect (= 42 (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms 42})))))
    (expect (contains? (:opts (captured-ask-code-opts {:idle-timeout-ms nil})) :idle-timeout-ms))
    (expect (nil? (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms nil}))))))

  (it "uses a four-minute semantic timeout by default and accepts overrides"
    ;; Codex/Claude over Copilot can sit silent for minutes while the
    ;; model reasons server-side; idle-timeout-ms keeps resetting on
    ;; SSE pings. The semantic watchdog surfaces \"transport alive but
    ;; no model events\" inside 4 minutes — see session da9f0b47
    ;; (2026-05-20) for the 11-minute pre-fix stall.
    (expect (= (* 4 60 1000) lp/ASK_CODE_SEMANTIC_TIMEOUT_MS))
    (let [opts (:opts (captured-ask-code-opts {:semantic-timeout-ms 180000}))]
      (expect (= 180000 (:semantic-timeout-ms opts)))
      (expect (= lp/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts))))
    (let [opts (:opts (captured-ask-code-opts {:semantic-timeout-ms nil}))]
      ;; Explicit nil opts the call out of the watchdog.
      (expect (contains? opts :semantic-timeout-ms))
      (expect (nil? (:semantic-timeout-ms opts))))))

(defdescribe python-eval-test
  (it "executes a Python assignment and the binding persists in the sandbox"
    (let [env (lp/create-environment ::router {:db :memory})]
      (try
        (let [result ((var-get #'lp/execute-code) env "x = 1")]
          (expect (nil? (:error result))))
        ;; Sandbox globals persist REPL-style across evals on the same context.
        (let [read-back (env/run-python-block (:python-context env) "x")]
          (expect (nil? (:error read-back)))
          (expect (= 1 (:result read-back))))
        (finally
          (lp/dispose-environment! env)))))

  (it "splits + evals multi-form blocks whose statements contain astral chars (emoji)"
    ;; Regression (session f41ca531): GraalPy's ast.get_source_segment truncates
    ;; the per-form source when a statement carries a non-BMP char (emoji 👆),
    ;; dropping the closing quotes -> the lone re-eval raised a spurious
    ;; "unterminated triple-quoted string" SyntaxError, the (done ...) answer
    ;; form errored, the turn never finalized, and the model looped re-emitting
    ;; done(). Our pure-Python codepoint slice must keep every segment intact —
    ;; including a MULTILINE triple-quoted string with emoji mid- and last-line.
    (let [{:keys [python-context]} (env/create-python-context {})
          ;; emoji on the first AND a later line; the second form re-reads the var.
          code (str "msg = \"\"\"# Heading 👆\n\n- bin/ 🚀\n\nPełne ł ó ż 🌳\"\"\"\n"
                 "msg")
          {:keys [forms error result]} (env/run-python-block python-context code)]
      (expect (nil? error))
      (expect (= 2 (count forms)))
      (expect (every? (comp nil? :error) forms))
      ;; the final expression re-reads the multi-line emoji string unchanged
      (expect (string? result))
      (expect (clojure.string/includes? result "👆"))
      (expect (clojure.string/includes? result "🌳"))
      (expect (clojure.string/includes? result "Pełne ł ó ż")))))

(defdescribe final-answer-gate-test
  ;; The structural "called a tool this iteration" floor was REMOVED from the
  ;; gate. A (done …) that ran alongside tool calls is now a PROPOSAL
  ;; (run-iteration sets :answer-proposed? and the loop asks the model to
  ;; confirm/refine), not a hard reject. The gate now only carries extension
  ;; :turn.answer/validate vetoes.
  (it "no longer rejects a (done) that ran alongside tool calls (now a proposal)"
    (expect (nil? (lp/final-answer-gate-error
                    {}
                    1
                    [{:id 0
                      :code "(cat \"deps.edn\")"
                      :channel [{:success? true :result [:ir {}]}]
                      :error nil}]
                    {:answer "done"}
                    nil))))

  (it "allows answer-only iterations when no extension tool ran"
    (expect (nil? (lp/final-answer-gate-error
                    {}
                    1
                    [{:id 0
                      :code "(+ 1 2)"
                      :result 3
                      :error nil}]
                    {:answer "done"}
                    nil)))))

;; ---------------------------------------------------------------------------
;; def-sink -> vars-snapshot (per-var precise source extraction)
;; ---------------------------------------------------------------------------

(defdescribe repetition-loop-detection-test
  "Repetition-only loop detector + decision-checkpoint. No iteration/budget
   counting — fires solely on a non-finalizing (done) repeated, or identical
   non-(done) action code repeated."
  (let [detect (var-get #'lp/repetition-loop-state)
        msg    (var-get #'lp/loop-checkpoint-message)]
    (describe "repetition-loop-state"
      (it "is not stuck on a single (done) that didn't finalize"
        (let [r (detect [{:code "(done \"hi\")"}] nil)]
          (expect (false? (:stuck? r)))
          (expect (= 1 (:done-streak r)))))

      (it "trips when a non-finalizing (done) repeats (streak reaches 2)"
        (let [r1 (detect [{:code "(let [x 1] (done \"a\"))"}] nil)
              r2 (detect [{:code "(let [x 2] (done \"b\"))"}] {:done-streak (:done-streak r1)})]
          (expect (true? (:stuck? r2)))
          (expect (= 2 (:done-streak r2)))))

      (it "trips when identical non-(done) action code repeats"
        (let [blocks [{:code "(rg {:any [\"cancel\"]})"} {:code "(cat \"x.clj\")"}]
              r1 (detect blocks nil)
              r2 (detect blocks {:last-sig (:action-sig r1)})]
          (expect (false? (:stuck? r1)))
          (expect (true? (:stuck? r2)))))

      (it "resets the (done) streak on an iteration with no (done)"
        (let [r (detect [{:code "(rg {:any [\"x\"]})"}] {:done-streak 1})]
          (expect (false? (:stuck? r)))
          (expect (zero? (:done-streak r)))))

      (it "does not trip on distinct action code across iterations"
        (let [r1 (detect [{:code "(rg {:any [\"a\"]})"}] nil)
              r2 (detect [{:code "(rg {:any [\"b\"]})"}] {:last-sig (:action-sig r1)})]
          (expect (false? (:stuck? r2))))))

    (describe "loop-checkpoint-message"
      (it "shows the sticky best-answer and forces a decision"
        (let [m (msg "The atom and token serve distinct roles.")]
          (expect (str/includes? m "STOP"))
          (expect (str/includes? m "best answer so far"))
          (expect (str/includes? m "The atom and token serve distinct roles."))
          (expect (str/includes? m "done("))))
      (it "handles no answer yet"
        (let [m (msg nil)]
          (expect (str/includes? m "NOT produced any answer")))))))

(defdescribe done-proposal-confirm-test
  "A (done …) emitted alongside tool calls is a PROPOSAL, not an error: the
   model is asked to confirm/refine after seeing results, never hard-rejected."
  (let [msg (var-get #'lp/proposal-confirm-message)]
    (it "shows the proposed answer and asks to confirm or refine"
      (let [m (msg "Token = cooperative cancel; atom = local interrupt flag.")]
        (expect (str/includes? m "PROPOSAL"))
        (expect (str/includes? m "proposed answer"))
        (expect (str/includes? m "Token = cooperative cancel; atom = local interrupt flag."))
        (expect (str/includes? m "done("))
        ;; must not read as a rejection/error
        (expect (not (str/includes? m "rejected")))))
    (it "handles a proposal with no captured text"
      (expect (string? (msg nil))))))

(defdescribe open-plan-steps-gate-test
  "Forcing done-gate: unresolved plan steps refuse to finalize (open OR
   :done-without-evidence). See dev/TASK_GATES_PROPOSAL.md."
  (let [block (var-get #'lp/open-plan-steps-block)
        task  (fn [m] (merge {:title "t" :born "t1/i1/f1"} m))]
    (it "refuses finalize while a plan step is open (:todo/:doing)"
      (let [msg (block {"design" (task {:plan? true :status :doing})
                        "impl"   (task {:plan? true :status :todo})})]
        (expect (string? msg))
        (expect (str/includes? msg "unresolved plan step"))
        (expect (str/includes? msg "design"))
        (expect (str/includes? msg "impl"))))
    (it "clears once every plan step is terminal (done w/o acceptance + cancelled w/ reason)"
      (expect (nil? (block {"design" (task {:plan? true :status :done})
                            "impl"   (task {:plan? true :status :cancelled :reason "not needed"})}))))
    (it "blocks finalize when the plan is empty — only non-plan/hook tasks, no plan step (DAG needs a root-goal task)"
      (let [msg (block {"scratch" (task {:status :todo})
                        "hookx"   (task {:status :doing :source :hook})})]
        (expect (string? msg))
        (expect (str/includes? msg "plan is empty"))))
    (it "a :candidate proposal does NOT block — propose-a-plan-then-done() is stop-and-wait"
      ;; Regression (infinite-loop bug): the model lays an all-candidate plan and
      ;; calls done() to present it + STOP for approval. Blocking candidates makes
      ;; that impossible — done() is refused, the turn never ends, done() retries
      ;; forever. A candidate is a PROPOSAL, not committed open work.
      (expect (nil? (block {"prop" (task {:plan? true :status :candidate})})))
      (expect (nil? (block {"a" (task {:plan? true :status :candidate})
                            "b" (task {:plan? true :status :candidate})}))))
    (it "candidate + an ACCEPTED open step → still blocks (on the accepted step only)"
      (let [msg (block {"prop" (task {:plan? true :status :candidate})
                        "impl" (task {:plan? true :status :todo})})]
        (expect (string? msg))
        (expect (str/includes? msg "impl"))
        (expect (not (str/includes? msg "prop")))))
    ;; evidence-not-status: a :done step with an :acceptance but no :evidence is
    ;; UNRESOLVED (closes the 'mark everything done to escape' silencing loop)
    (it "blocks a :done step that has an :acceptance but blank/absent :evidence"
      (let [msg (block {"impl" (task {:plan? true :status :done :acceptance "tests green"})})]
        (expect (string? msg))
        (expect (str/includes? msg "needs :evidence"))))
    (it "clears a :done step once :evidence is present"
      (expect (nil? (block {"impl" (task {:plan? true :status :done
                                          :acceptance "tests green"
                                          :evidence "ran clj -M:test -> 0 fail"})}))))
    (it "a :done step with NO :acceptance needs no evidence (nothing to prove)"
      (expect (nil? (block {"impl" (task {:plan? true :status :done})}))))
    ;; silent-abandonment: a non-success terminal without :reason is UNRESOLVED
    (it "blocks a :cancelled/:deferred/:failed step that has no :reason"
      (expect (str/includes? (block {"x" (task {:plan? true :status :cancelled})}) "needs :reason"))
      (expect (str/includes? (block {"x" (task {:plan? true :status :deferred})}) "needs :reason"))
      (expect (str/includes? (block {"x" (task {:plan? true :status :failed})}) "needs :reason")))
    (it "clears a non-success terminal once a :reason is given"
      (expect (nil? (block {"x" (task {:plan? true :status :cancelled :reason "superseded by y"})})))
      (expect (nil? (block {"x" (task {:plan? true :status :deferred :reason "blocked upstream"})}))))))

(defdescribe current-turn-goal-seed-test
  (it "seeds DAG turns with an unresolved root goal for the current request"
    (let [env {:ctx-atom (ctx-loop/make-ctx-atom "s1")
               :turn-state-atom (ctx-loop/make-turn-state-atom)}]
      (ctx-loop/set-turn-state! env :turn-position 42)
      (with-redefs [toggles/enabled? (fn [toggle]
                                       (= :vis/dag-expression toggle))]
        (seed-current-turn-goal! env "implement regex-lite" 42))
      (let [key (current-turn-goal-key 42)
            task (get-in @(:ctx-atom env) [:session/tasks key])
            block ((var-get #'lp/open-plan-steps-block)
                   (:session/tasks @(:ctx-atom env)))]
        (expect (= "turn_42_goal" key))
        (expect (= :doing (:status task)))
        (expect (true? (:plan? task)))
        (expect (str/includes? (:acceptance task) "discovery-only evidence is not sufficient"))
        (expect (str/includes? block "turn_42_goal")))))

  (it "keeps discovery-only subtasks from satisfying the seeded root goal"
    (let [tasks {"turn_42_goal" {:title "Current request"
                                 :status :doing
                                 :plan? true
                                 :acceptance "implement and verify"}
                 "read_files" {:title "Read stub and tests"
                               :status :done
                               :plan? true
                               :evidence "cat(lib.rs), cat(tests.rs)"}}]
      (expect (str/includes? ((var-get #'lp/open-plan-steps-block) tasks)
                "turn_42_goal")))))

(defdescribe assistant-replayability-test
  (it "opts syntax/preflight correction iterations out of assistant replay"
    (expect (false? (assistant-replayable-iteration?
                      [{:code "<context>\n...\nrg({})"
                        :error {:message "syntax"
                                :data {:phase :python/syntax}}}])))
    (expect (false? (assistant-replayable-iteration?
                      [{:code "advance({})"
                        :error {:message "preflight"
                                :block {:phase :vis/preflight}}}])))
    (expect (true? (assistant-replayable-iteration?
                     [{:code "rg({\"any\":[\"context-overlay\"]})"
                       :result {:hit_count 2}
                       :error nil}])))))

(defdescribe forced-loop-termination-test
  "STERN PATH (integration): a model that emits the SAME non-(done) action every
   iteration trips the repetition detector → decision-checkpoint → force-finalize,
   so the turn TERMINATES with a (give-up) answer instead of looping forever.
   Pre-fix this would loop until cancel; the safety cap below turns a regression
   into a loud failure instead of a hang."
  (it "force-finalizes a repeating turn instead of looping forever"
    (let [router-stub {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}]}
          env   (lp/create-environment router-stub {:db :memory})
          calls (atom 0)]
      (try
        (with-redefs [svar/ask-code!
                      (fn [_ _]
                        (when (> (swap! calls inc) 12)
                          (throw (ex-info "force-finalize never fired — looped >12x" {})))
                        ;; identical non-(done) action every iteration → action-sig repeats
                        {:blocks [{:lang "clojure" :source "(def probe 1)"}]
                         :raw    "```clojure\n(def probe 1)\n```"
                         :tokens {}})]
          (let [result (lp/turn! env [(svar/user "go in circles")] {})]
            (expect (some? result))
            ;; terminated via force-finalize, not a hang / not blank
            (expect (not (str/blank? (str (lp/answer-markdown (:answer result))))))
            ;; converged fast: iter1 (seed) → iter2 (stuck+checkpoint) → iter3 (force)
            (expect (<= @calls 5))))
        (finally (lp/dispose-environment! env))))))

(defdescribe append-runtime-appendices-unverified-test
  (describe "truthful backstop: tasks closed :done + :acceptance but not :verified?"
    (let [env-with (fn [tasks] {:ctx-atom (atom {:session/tasks tasks})})]
      (it "appends an Unverified note naming the offending task"
        (let [env (env-with {:work {:status :done :title "add subtract"
                                    :acceptance "returns a-b" :verified? false}})
              md  (:answer (lp/append-runtime-appendices env {:answer "Added subtract."} nil))]
          (expect (str/includes? md "Added subtract."))
          (expect (str/includes? md "Unverified"))
          (expect (str/includes? md "add subtract"))))
      (it "leaves a verified done task untouched"
        (let [env (env-with {:work {:status :done :title "x" :acceptance "y" :verified? true}})
              ans {:answer "All good."}]
          (expect (= ans (lp/append-runtime-appendices env ans nil)))))
      (it "ignores a :done task with no :acceptance"
        (let [env (env-with {:work {:status :done :title "x"}})
              ans {:answer "All good."}]
          (expect (= ans (lp/append-runtime-appendices env ans nil)))))
      (it "passes needs-input answers through untouched"
        (let [env (env-with {:work {:status :done :title "x" :acceptance "y" :verified? false}})
              ans {:vis/answer-mode :needs-input :answer/text "Which DB?"}]
          (expect (= ans (lp/append-runtime-appendices env ans nil)))))
      (it "appends into the wrapped {:result {:answer …}} shape"
        (let [env (env-with {:work {:status :done :title "tw" :acceptance "y" :verified? false}})
              out (lp/append-runtime-appendices env {:result {:answer "Body."}} nil)]
          (expect (str/includes? (get-in out [:result :answer]) "Unverified"))
          (expect (str/includes? (get-in out [:result :answer]) "tw")))))))

(defdescribe honor-config-primary-test
  (describe "honor-config-primary! — the USER's first-configured model wins as primary"
    ;; Bug: svar's make-router PREPENDS a provider's catalog :default-models, so
    ;; (first :models) — the effective model — was always svar's default, never
    ;; the model the user put first. The user reorders to primary, nothing happens.
    (let [f (var-get #'lp/honor-config-primary!)]
      (it "floats the user's config-first model to the front (+ :root), keeping the rest as fallbacks"
        (let [;; svar prepended opus-4-8; user actually configured fable-5 first
              router {:providers [{:id :anthropic-coding-plan
                                   :models [{:name "claude-opus-4-8"} {:name "claude-fable-5"}]}]}
              config {:providers [{:id :anthropic-coding-plan
                                   :models [{:name "claude-fable-5"} {:name "claude-opus-4-8"}]}]}
              p      (first (:providers (f router config)))]
          (expect (= ["claude-fable-5" "claude-opus-4-8"] (mapv :name (:models p))))
          (expect (= "claude-fable-5" (:root p)))
          (expect (= "claude-fable-5" (:name (lp/resolve-effective-model (f router config)))))))
      (it "accepts a string model in config (model-name coercion)"
        (let [router {:providers [{:id :anthropic-coding-plan
                                   :models [{:name "claude-opus-4-8"} {:name "claude-sonnet-4-6"}]}]}
              config {:providers [{:id :anthropic-coding-plan :models ["claude-sonnet-4-6"]}]}]
          (expect (= "claude-sonnet-4-6" (:name (lp/resolve-effective-model (f router config)))))))
      (it "no-op when the config-first model isn't in the built provider (never empties it)"
        (let [router {:providers [{:id :anthropic-coding-plan :models [{:name "claude-opus-4-8"}]}]}
              config {:providers [{:id :anthropic-coding-plan :models [{:name "totally-unknown"}]}]}
              p      (first (:providers (f router config)))]
          (expect (= ["claude-opus-4-8"] (mapv :name (:models p))))))
      (it "leaves a provider the user didn't configure untouched"
        (let [router {:providers [{:id :zai-coding-plan :models [{:name "glm-4"}]}]}
              config {:providers [{:id :anthropic-coding-plan :models [{:name "claude-fable-5"}]}]}]
          (expect (= [{:name "glm-4"}] (:models (first (:providers (f router config)))))))))))

(defdescribe router-for-model-test
  (describe "router-for-model — a coordinator PROPOSES a child model"
    (let [router {:providers [{:id :anthropic-coding-plan :models [{:name "claude-opus-4-8"}]}
                              {:id :anthropic :models [{:name "claude-haiku-4-5"}
                                                       {:name "claude-sonnet-4-6"}]}]}]
      (it "the proposed model becomes the child's EFFECTIVE model"
        (expect (= "claude-haiku-4-5"
                  (:name (lp/resolve-effective-model (lp/router-for-model router "claude-haiku-4-5")))))
        (expect (= "claude-sonnet-4-6"
                  (:name (lp/resolve-effective-model (lp/router-for-model router "claude-sonnet-4-6"))))))
      (it "an ORDERED preference list reorders provider/model order (svar falls back)"
        (let [r (lp/router-for-model router ["claude-sonnet-4-6" "claude-haiku-4-5"])]
          ;; most-preferred is effective; the full order reflects the preference
          ;; then the rest as fallback — svar routes this order, no svar change.
          (expect (= "claude-sonnet-4-6" (:name (lp/resolve-effective-model r))))
          (expect (= ["claude-sonnet-4-6" "claude-haiku-4-5" "claude-opus-4-8"]
                    (vec (for [p (:providers r) m (:models p)] (:name m)))))))
      (it "omitted (nil/blank) → child inherits the parent's default model"
        (expect (= "claude-opus-4-8" (:name (lp/resolve-effective-model (lp/router-for-model router nil)))))
        (expect (= "claude-opus-4-8" (:name (lp/resolve-effective-model (lp/router-for-model router "  "))))))
      (it "unknown model → falls back to the parent's default (no crash)"
        (expect (= "claude-opus-4-8" (:name (lp/resolve-effective-model (lp/router-for-model router "gpt-9"))))))
      (it "preserves the full provider set (just reordered) so keys/opts survive"
        (expect (= #{:anthropic-coding-plan :anthropic}
                  (set (map :id (:providers (lp/router-for-model router "claude-haiku-4-5"))))))))))

(defdescribe subctx->seed-ctx-test
  (describe "subctx->seed-ctx — model's keyword-snake dict → engine ctx"
    (let [seed (lp/subctx->seed-ctx
                 {:tasks {:oauth {:status "doing" :title "OAuth" :parent "auth"}
                          :auth  {:status "in_progress" :composite "selector"}}
                  :facts {:ev_a {:content "secret needed"}}
                  :focus "oauth"})]
      (it "renames top keys to :session/* namespaced"
        (expect (contains? seed :session/tasks))
        (expect (contains? seed :session/facts))
        (expect (not (contains? seed :tasks))))
      (it "stringifies entity map keys (ids are strings only)"
        (expect (= #{"auth" "oauth"} (set (keys (:session/tasks seed)))))
        (expect (= #{"ev_a"} (set (keys (:session/facts seed))))))
      (it "normalizes string status VALUES to keywords"
        (expect (= :doing (get-in seed [:session/tasks "oauth" :status])))
        (expect (= :doing (get-in seed [:session/tasks "auth" :status]))))  ; in_progress → :doing
      (it "passes other fields through (parent stays a string, composite kept)"
        (expect (= "auth" (get-in seed [:session/tasks "oauth" :parent])))
        (expect (= "selector" (get-in seed [:session/tasks "auth" :composite]))))
      (it "empty subctx → empty seed (no nil subtrees)"
        (expect (= {} (lp/subctx->seed-ctx {})))))))

(defdescribe sub-loop!-test
  (describe "sub-loop! assembly (stubbed env/turn — no LLM, no FS)"
    (let [child-ctx (atom {:session/tasks {"oauth" {:status :done :evidence "tests green"}}
                           :session/facts {"f1" {:content "found"}}})
          captured  (atom nil)
          router    {:providers [{:id :anthropic-coding-plan :models [{:name "opus"}]}
                                 {:id :anthropic :models [{:name "haiku"}]}]}
          parent    {:router router :db-info :db :depth-atom (atom 0)
                     :session/state-id "parent-state-123"
                     :workspace {:id "parent-ws" :root "/parent"}}
          r (with-redefs [lp/child-workspace!     (fn [_db _pw] {:id "child-ws" :root "/child" :fork-ms 0})
                          lp/create-environment   (fn [router opts]
                                                    (reset! captured {:router router :opts opts})
                                                    {:ctx-atom child-ctx :owns-db? false :db-info :db})
                          lp/run-turn!            (fn [_e _p _o] {:status :success :answer "did it"})
                          lp/dispose-environment! (fn [_])]
              (lp/sub-loop! parent {:prompt "implement oauth"
                                    :subctx {:focus "oauth" :tasks {:oauth {:status "doing"}}}
                                    :models ["haiku"]}))]
      (it "routes the child to the PROPOSED model"
        (expect (= "haiku" (:name (lp/resolve-effective-model (:router @captured))))))
      (it "passes parent-db-info + depth(parent+1) + seed-ctx as :child opts"
        (expect (= :db (get-in @captured [:opts :child :parent-db-info])))
        (expect (= 1 (get-in @captured [:opts :child :depth])))
        (expect (contains? (get-in @captured [:opts :child :seed-ctx]) :session/tasks))
        (expect (= "child-ws" (get-in @captured [:opts :workspace-id])))
        ;; child soul links to the PARENT's session_state (cross-soul) → hidden
        ;; from the top-level list, queryable as the parent's sub-tree
        (expect (= "parent-state-123" (get-in @captured [:opts :child :parent-state-id]))))
      (it "returns the focus result shape (status/evidence/facts from the child ctx)"
        (expect (= "oauth" (:task_id r)))
        ;; status is coerced to a python-facing STRING (never a keyword)
        (expect (= "done" (:status r)))
        (expect (= "tests green" (:evidence r)))
        (expect (= {"f1" {:content "found"}} (:facts r)))
        (expect (= "did it" (:answer r))))))
  (describe "depth cap"
    (it "throws :vis/subloop-depth-exceeded past MAX-SUBLOOP-DEPTH"
      (with-redefs [lp/child-workspace!     (fn [& _] {:id "x" :root "/x"})
                    lp/create-environment   (fn [& _] {:ctx-atom (atom {}) :owns-db? false})
                    lp/run-turn!            (fn [& _] {})
                    lp/dispose-environment! (fn [_])]
        (expect (throws? clojure.lang.ExceptionInfo
                  #(lp/sub-loop! {:depth-atom (atom 5) :router {} :workspace {} :db-info :db}
                     {:prompt "x" :subctx {}}))))))

  (describe "child cleanup — clone trashed + env disposed (no leaks)"
    (let [stub-ws {:id "child-ws" :root "/child" :fork-ms 0}]
      (it "on success: disposes the env AND abandons the rift clone (after merging it back)"
        (let [events  (atom [])]
          (with-redefs [lp/child-workspace!     (fn [_ _] stub-ws)
                        lp/create-environment   (fn [_ _] {:ctx-atom (atom {}) :owns-db? false :db-info :db})
                        lp/run-turn!            (fn [_ _ _] {:status :success :answer "ok"})
                        workspace/apply!        (fn [_ _] (swap! events conj :apply) {:changed []})
                        workspace/abandon!      (fn [_ a] (swap! events conj [:abandon (:workspace-id a)]) a)
                        lp/dispose-environment! (fn [_] (swap! events conj :dispose))]
            (lp/sub-loop! {:router {} :db-info :db :depth-atom (atom 0)
                           :workspace {:id "parent-ws" :root "/parent"}}
              {:prompt "p" :subctx {:focus "t"}}))
          ;; merge happens before dispose before abandon — and abandon names the clone
          (expect (= [:apply :dispose [:abandon "child-ws"]] @events))))

      (it "on a thrown turn: STILL disposes the env AND abandons the clone (finally), then rethrows"
        (let [events (atom [])]
          (with-redefs [lp/child-workspace!     (fn [_ _] stub-ws)
                        lp/create-environment   (fn [_ _] {:ctx-atom (atom {}) :owns-db? false :db-info :db})
                        lp/run-turn!            (fn [_ _ _] (throw (ex-info "turn blew up" {})))
                        workspace/apply!        (fn [_ _] (swap! events conj :apply) {:changed []})
                        workspace/abandon!      (fn [_ a] (swap! events conj [:abandon (:workspace-id a)]) a)
                        lp/dispose-environment! (fn [_] (swap! events conj :dispose))]
            (expect (throws? clojure.lang.ExceptionInfo
                      #(lp/sub-loop! {:router {} :db-info :db :depth-atom (atom 0)
                                      :workspace {:id "parent-ws" :root "/parent"}}
                         {:prompt "p" :subctx {:focus "t"}})))
            ;; no merge (turn failed), but BOTH cleanups ran
            (expect (= [:dispose [:abandon "child-ws"]] @events)))))))

  (describe "parallel-sub-loops! (stubbed sub-loop! — concurrency, ordering, failure isolation)"
    (let [live    (atom 0)
          peak    (atom 0)
          run     (fn [parent specs]
                    (with-redefs [lp/sub-loop!
                                  (fn [_parent {:keys [subctx]}]
                                    (let [n (swap! live inc)]
                                      (swap! peak max n))
                                    (Thread/sleep 25)
                                    (swap! live dec)
                                    (let [focus (:focus subctx)]
                                      (when (= focus "boom")
                                        (throw (ex-info "child blew up" {})))
                                      {:task_id focus :status "done" :changed_files []}))]
                      (lp/parallel-sub-loops! parent specs)))
          specs   (mapv (fn [i] {:prompt (str "t" i)
                                 :subctx {:focus (str "task" i)}
                                 :models ["haiku"]})
                    (range 8))
          results (run {:depth-atom (atom 0)} specs)]
      (it "returns one result per spec, in INPUT ORDER"
        (expect (= 8 (count results)))
        (expect (= (mapv #(str "task" %) (range 8)) (mapv :task_id results))))
      (it "bounds concurrency to the cap (peak in-flight never exceeds 4)"
        (expect (<= @peak 4))
        (expect (pos? @peak)))
      (it "a child that throws surfaces as a failed slot, not a batch-killing exception"
        (let [r (run {:depth-atom (atom 0)}
                  [{:prompt "ok"   :subctx {:focus "good"}}
                   {:prompt "bad"  :subctx {:focus "boom"}}
                   {:prompt "ok2"  :subctx {:focus "fine"}}])]
          (expect (= ["good" "boom" "fine"] (mapv :task_id r)))
          (expect (= ["done" "failed" "done"] (mapv :status r)))
          (expect (= "child blew up" (:error (second r))))))))

  (describe "sequence-sub-loops! (:sequence composite — in order, fail-fast)"
    ;; stub sub-loop! to succeed/fail based on the spec's focus key
    (let [run (fn [focuses fail-set]
                (let [ran (atom [])]
                  (with-redefs [lp/sub-loop! (fn [_ {:keys [subctx]}]
                                               (let [f (:focus subctx)]
                                                 (swap! ran conj f)
                                                 {:task_id f :status (if (fail-set f) "failed" "done")}))]
                    {:results (lp/sequence-sub-loops! {}
                                (mapv (fn [f] {:prompt f :subctx {:focus f}}) focuses))
                     :ran @ran})))]
      (it "all succeed → runs every child in order, returns all"
        (let [{:keys [results ran]} (run ["a" "b" "c"] #{})]
          (expect (= ["a" "b" "c"] ran))
          (expect (= ["a" "b" "c"] (mapv :task_id results)))
          (expect (= ["done" "done" "done"] (mapv :status results)))))
      (it "stops at the FIRST failure — later children never run; result includes the failure"
        (let [{:keys [results ran]} (run ["a" "b" "c"] #{"b"})]
          (expect (= ["a" "b"] ran))                       ; "c" never ran
          (expect (= ["a" "b"] (mapv :task_id results)))
          (expect (= ["done" "failed"] (mapv :status results)))))))

  (describe "selector-sub-loops! (:selector composite — alternatives until one succeeds)"
    (let [run (fn [focuses fail-set]
                (let [ran (atom [])]
                  (with-redefs [lp/sub-loop! (fn [_ {:keys [subctx]}]
                                               (let [f (:focus subctx)]
                                                 (swap! ran conj f)
                                                 {:task_id f :status (if (fail-set f) "failed" "done")}))]
                    {:results (lp/selector-sub-loops! {}
                                (mapv (fn [f] {:prompt f :subctx {:focus f}}) focuses))
                     :ran @ran})))]
      (it "first child succeeds → stops immediately, no alternatives tried"
        (let [{:keys [results ran]} (run ["a" "b" "c"] #{})]
          (expect (= ["a"] ran))
          (expect (= ["a"] (mapv :task_id results)))
          (expect (= ["done"] (mapv :status results)))))
      (it "tries alternatives in order until one succeeds; later ones skipped"
        (let [{:keys [results ran]} (run ["a" "b" "c"] #{"a"})]
          (expect (= ["a" "b"] ran))                       ; "c" never tried
          (expect (= ["failed" "done"] (mapv :status results)))))
      (it "all alternatives fail → returns every attempt (all failures)"
        (let [{:keys [results ran]} (run ["a" "b"] #{"a" "b"})]
          (expect (= ["a" "b"] ran))
          (expect (= ["failed" "failed"] (mapv :status results)))))))

  (describe "retry-sub-loop! (stubbed sub-loop! — selector: re-run until success)"
    (it "succeeds on the first attempt — no re-run"
      (let [calls (atom 0)]
        (with-redefs [lp/sub-loop! (fn [_ _] (swap! calls inc) {:task_id "t" :status "done"})]
          (let [r (lp/retry-sub-loop! {} {:prompt "p" :subctx {:focus "t"}} 3)]
            (expect (= "done" (:status r)))
            (expect (= 1 (:attempts r)))
            (expect (= 1 @calls))))))
    (it "re-runs a failing child until it succeeds, stamping the winning attempt"
      (let [calls (atom 0)]
        (with-redefs [lp/sub-loop! (fn [_ _]
                                     (let [n (swap! calls inc)]
                                       (if (< n 3)
                                         {:task_id "t" :status "failed"}
                                         {:task_id "t" :status "done"})))]
          (let [r (lp/retry-sub-loop! {} {:prompt "p" :subctx {:focus "t"}} 5)]
            (expect (= "done" (:status r)))
            (expect (= 3 (:attempts r)))
            (expect (= 3 @calls))))))
    (it "exhausts n attempts and returns the last failure (status in the failure set)"
      (let [calls (atom 0)]
        (with-redefs [lp/sub-loop! (fn [_ _] (swap! calls inc) {:task_id "t" :status "rejected"})]
          (let [r (lp/retry-sub-loop! {} {:prompt "p" :subctx {:focus "t"}} 2)]
            (expect (= "rejected" (:status r)))
            (expect (= 2 (:attempts r)))
            (expect (= 2 @calls))))))
    (it "treats a THROWN child as a failure and retries; defaults to 2 attempts"
      (let [calls (atom 0)]
        (with-redefs [lp/sub-loop! (fn [_ _] (swap! calls inc) (throw (ex-info "blew up" {})))]
          (let [r (lp/retry-sub-loop! {} {:prompt "p" :subctx {:focus "t"}} nil)]
            (expect (= "failed" (:status r)))
            (expect (= "blew up" (:error r)))
            (expect (= 2 (:attempts r)))
            (expect (= 2 @calls)))))))

  (describe "SINGULAR DB connection (child reuses the parent's; never opens its own)"
    (it "a child env shares the EXACT parent db-info and disposing it leaves the parent connection alive"
      ;; A sub_loop child (and every parallel child) must run on the parent's ONE
      ;; DB connection — `:parent-db-info` short-circuits `db-create-connection!`,
      ;; so no new pool/datasource is opened. Critical for `:memory` (per-connection
      ;; — a fresh one would be a SEPARATE empty DB) and to avoid connection sprawl.
      (let [parent (lp/create-environment ::router {:db :memory})]
        (try
          (let [child (lp/create-environment ::router
                        {:child {:parent-db-info (:db-info parent) :depth 1}})]
            ;; SAME connection object — not a second pool
            (expect (identical? (:db-info parent) (:db-info child)))
            (expect (false? (:owns-db? child)))
            (expect (not (false? (:owns-db? parent))))
            ;; disposing the child must NOT close the shared connection
            (lp/dispose-environment! child))
          ;; parent's db-info is still usable after the child was disposed
          (expect (= [] (persistance/db-list-session-turns (:db-info parent) (random-uuid))))
          (finally (lp/dispose-environment! parent)))))))

(defdescribe hopeless-context-overflow-breaker-test
  "VIS-9: a preflight context overflow far beyond the call's max-input
   budget must fail the turn (fatal), not feed back to the model — the
   fed error never reaches the model (the next call dies in the same
   preflight) and appending it only grows the input. Marginal overflows
   keep the feed path so trailer folding / summarize can recover."
  (let [overflow-ex (fn [input max-input]
                      (ex-info "Context overflow"
                        {:type :svar.tokens/context-overflow
                         :model "claude-fable-5"
                         :input-tokens input
                         :max-input-tokens max-input
                         :overflow (- input max-input)}))
        ctx {:iteration 1 :messages [] :routing {} :reasoning-level nil}]
    (it "10x over budget (the observed runaway) is fatal on iteration 1"
      (let [result (lp/handle-iteration-exception! (overflow-ex 81325 8192) ctx)]
        (expect (contains? result :com.blockether.vis.internal.loop/iteration-error))
        (expect (true? (:com.blockether.vis.internal.loop/fatal-iteration-error result)))))
    (it "exactly at the 1.5x factor is fatal"
      (let [result (lp/handle-iteration-exception! (overflow-ex 12288 8192) ctx)]
        (expect (true? (:com.blockether.vis.internal.loop/fatal-iteration-error result)))))
    (it "a marginal overflow (1.1x) still feeds the error to the model"
      (let [result (lp/handle-iteration-exception! (overflow-ex 9000 8192) ctx)]
        (expect (contains? result :com.blockether.vis.internal.loop/iteration-error))
        (expect (not (:com.blockether.vis.internal.loop/fatal-iteration-error result)))))
    (it "a non-overflow model error keeps the feed path"
      (let [result (lp/handle-iteration-exception!
                     (ex-info "NameError: nope" {:type :vis/eval-error}) ctx)]
        (expect (not (:com.blockether.vis.internal.loop/fatal-iteration-error result)))))))

(defdescribe provider-error-circuit-breaker-test
  (describe "next-provider-error-streak"
    (it "increments on consecutive :llm-provider/generate errors"
      (expect (= 1 ((deref #'lp/next-provider-error-streak) nil {:phase :llm-provider/generate})))
      (expect (= 3 ((deref #'lp/next-provider-error-streak) 2 {:phase :llm-provider/generate}))))
    (it "resets on any non-provider-generate error so RLM self-correction keeps its budget"
      (expect (= 0 ((deref #'lp/next-provider-error-streak) 2 {:phase :python/syntax})))
      (expect (= 0 ((deref #'lp/next-provider-error-streak) 7 nil)))))
  (describe "provider-error-breaker-tripped?"
    (it "trips only at CONSECUTIVE_PROVIDER_ERROR_LIMIT (3)"
      (expect (not ((deref #'lp/provider-error-breaker-tripped?) 2)))
      (expect ((deref #'lp/provider-error-breaker-tripped?) 3)))))
