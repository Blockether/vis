(ns com.blockether.vis.internal.loop-test
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.ctx-loop :as ctx-loop]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.env-python :as env]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe describe it expect]]))

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
                              ((:on-chunk opts) {:reasoning "dead thinking"
                                                 :content "```clojure\n(dead)"})
                              (throw (ex-info "Stream connection error: closed"
                                       {:type :svar.core/http-error
                                        :stream? true
                                        :content-acc-len 19
                                        :reasoning-acc-len 13
                                        :reasoning "dead thinking"
                                        :partial-content "```clojure\n(dead)"})))
                          2 (do
                              ((:on-chunk opts) {:reasoning "fresh thinking"})
                              {:blocks [{:lang "python"
                                         :source "done(\"\"\"ok\"\"\")"}]
                               :raw "```python\ndone(\"\"\"ok\"\"\")\n```"
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
  "Forcing done-gate: open plan steps refuse to finalize. See dev/TASK_GATES_PROPOSAL.md."
  (let [block (var-get #'lp/open-plan-steps-block)
        task  (fn [m] (merge {:title "t" :born "t1/i1/f1"} m))]
    (it "refuses finalize while a plan step is open (:todo/:doing)"
      (let [msg (block {"design" (task {:plan? true :status :doing})
                        "impl"   (task {:plan? true :status :todo})})]
        (expect (string? msg))
        (expect (str/includes? msg "open plan step"))
        (expect (str/includes? msg "design"))
        (expect (str/includes? msg "impl"))))
    (it "clears once every plan step is terminal"
      (expect (nil? (block {"design" (task {:plan? true :status :done})
                            "impl"   (task {:plan? true :status :cancelled})}))))
    (it "only :plan? steps block — non-plan + hook tasks are ignored"
      (expect (nil? (block {"scratch" (task {:status :todo})
                            "hookx"   (task {:status :doing :source :hook})}))))
    (it "candidate (unapproved proposal) is open and blocks"
      (expect (some? (block {"prop" (task {:plan? true :status :candidate})}))))))

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
