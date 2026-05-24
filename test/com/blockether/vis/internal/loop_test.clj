(ns com.blockether.vis.internal.loop-test
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.env :as env]
   [com.blockether.vis.internal.ctx-loop :as ctx-loop]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.persistance :as persistance]
   [lazytest.core :refer [defdescribe describe it expect]]
   [sci.core :as sci]))

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

(def ^:private call-provider-with-interrupt-retry!
  (deref #'lp/call-provider-with-interrupt-retry!))

(def ^:private run-normal-turn!
  (deref #'lp/run-normal-turn!))

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
      (expect (str/includes? (:hint ctx) ":start"))))

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

(def ^:private parse-top-level-forms
  (deref #'lp/parse-top-level-forms))

(def ^:private code-entries-preflight
  (deref #'lp/code-entries-preflight))

(def ^:private empty-code-error-with-observation
  (deref #'lp/empty-code-error-with-observation))

(def ^:private multi-fence-hint     (deref #'lp/multi-fence-hint))
(def ^:private attach-multi-fence-hint (deref #'lp/attach-multi-fence-hint))

(defdescribe multi-fence-hint-test
  (it "is nil for a single-fence entry"
    (expect (nil? (multi-fence-hint {})))
    (expect (nil? (multi-fence-hint {:multi-fence-merged? false :multi-fence-count 1}))))

  (it "names the rule and the count when several fences were merged"
    (let [h (multi-fence-hint {:multi-fence-merged? true :multi-fence-count 5})]
      (expect (string? h))
      (expect (str/includes? h "5"))
      (expect (str/includes? h "```clojure```"))
      (expect (str/includes? h "exactly ONE"))))

  (it "falls back to 'multiple' when the count is missing"
    (let [h (multi-fence-hint {:multi-fence-merged? true})]
      (expect (str/includes? h "multiple")))))

(defdescribe attach-multi-fence-hint-test
  (it "is identity on single-fence entries"
    (let [err {:message "boom"}]
      (expect (= err (attach-multi-fence-hint err {})))
      (expect (= err (attach-multi-fence-hint err {:multi-fence-merged? false})))))

  (it "adds :hint when the entry was a multi-fence merge and no hint existed"
    (let [out (attach-multi-fence-hint {:message "parse error"}
                {:multi-fence-merged? true :multi-fence-count 3})]
      (expect (string? (:hint out)))
      (expect (str/includes? (:hint out) "3"))))

  (it "appends to an existing upstream hint instead of clobbering it"
    (let [out (attach-multi-fence-hint {:message "parse error" :hint "close brace at line 12"}
                {:multi-fence-merged? true :multi-fence-count 2})]
      (expect (str/starts-with? (:hint out) "close brace at line 12"))
      (expect (str/includes? (:hint out) "2"))))

  (it "treats a blank upstream hint as absent"
    (let [out (attach-multi-fence-hint {:message "x" :hint "   "}
                {:multi-fence-merged? true :multi-fence-count 4})]
      (expect (not (str/starts-with? (:hint out) " "))))))

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
  (it "summarizes svar 0.5.5 fence observations"
    (expect (= {:form-count 1
                :all-form-count 3
                :dropped-form-count 2
                :saw-fence? true
                :malformed? true}
              (ask-code-block-observation
                {:blocks [{:source "(def x 1)" :lang "clojure"}]
                 :all-blocks [{:source "(def x 1)" :lang "clojure"}
                              {:source "console.log(1)" :lang "javascript"}
                              {:source "oops" :lang nil}]
                 :saw-fence? true
                 :malformed? true}))))

  (it "keeps pre-0.5.5 shape compatible"
    (expect (= {:form-count 1
                :all-form-count 1
                :dropped-form-count 0
                :saw-fence? false
                :malformed? false}
              (ask-code-block-observation
                {:blocks [{:source "(def x 1)" :lang "clojure"}]})))))

(defdescribe empty-code-error-with-observation-test
  (it "reports malformed fences before generic no-code"
    (let [msg (empty-code-error-with-observation "generic" {:malformed? true})]
      (expect (str/includes? msg "malformed Markdown code fence"))))

  (it "reports dropped non-clojure fences with counts"
    (let [msg (empty-code-error-with-observation
                "generic"
                {:blocks []
                 :all-blocks [{:source "x" :lang "python"}
                              {:source "y" :lang nil}]
                 :saw-fence? true})]
      (expect (str/includes? msg "none survived Clojure selection"))
      (expect (str/includes? msg "Dropped blocks: 2 of 2"))))

  (it "keeps generic message for fenceless responses"
    (expect (= "generic" (empty-code-error-with-observation "generic" {})))))

(defdescribe iteration-start-hook-test
  (it "collects active :turn.iteration/start hooks as hook-task descriptors (D12)"
    (let [seen (atom nil)
          validator "(fn [_] true)"
          ext {:ext/name "test.hooks"
               :ext/hooks [{:id :test/title
                            :doc "title"
                            :phase :turn.iteration/start
                            :fn (fn [ctx]
                                  (reset! seen ctx)
                                  {:title "set title"
                                   :importance :warn
                                   :validator-fn validator})}
                           {:id :test/answer
                            :doc "answer"
                            :phase :turn.answer/validate
                            :fn (fn [_] {:reject true})}
                           {:id :test/no-validator
                            :doc "missing validator-fn—rejected"
                            :phase :turn.iteration/start
                            :fn (fn [_] {:title "hook without validator"})}
                           {:id :test/no-title
                            :doc "missing title—rejected"
                            :phase :turn.iteration/start
                            :fn (fn [_] {:importance :warn :validator-fn validator})}]}
          ctx {:session-title nil
               :title-refresh? true
               :turn-position 1}
          hits (collect-iteration-start-hints {} [ext] ctx)]
      (expect (= [{:id :test/title
                   :task {:title "set title"
                          :specs {}
                          :status :todo
                          :source :hook
                          :hook-id :test/title
                          :importance :warn
                          :validator-fn validator}}]
                hits))
      (expect (= ctx @seen))))

  (it "set-session-title! is a bare engine symbol, not a v/ tool"
    (let [env (lp/create-environment ::router {:db :memory})]
      (try
        (expect (= :vis/silent
                  (:val (sci/eval-string+ (:sci-ctx env)
                          "(set-session-title! \"Liveness check\")"
                          {:ns (:sandbox-ns env)}))))
        (try
          (sci/eval-string+ (:sci-ctx env)
            "(v/set-session-title! \"Liveness check\")"
            {:ns (:sandbox-ns env)})
          (expect false)
          (catch Throwable e
            (expect (str/includes? (ex-message e)
                      "Unable to resolve symbol: v/set-session-title!"))))
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

(defdescribe parse-top-level-forms-test
  (it "keeps quote reader macro attached when streaming top-level forms"
    (let [{:keys [forms parse-error]} (parse-top-level-forms
                                        "(require '[clojure.string :as str])")
          form (:form (first forms))]
      (expect (nil? parse-error))
      (expect (= 1 (count forms)))
      (expect (= '(require (quote [clojure.string :as str])) form))
      (expect (= "(require (quote [clojure.string :as str]))"
                (:source (first forms))))))

  (it "keeps a blank line between merged clojure fences"
    (let [{:keys [code-entries normalized-code]}
          (code-entries-preflight
            1
            [{:lang "clojure" :source "(def a 1)"}
             {:lang "clojure" :source "(def b 2)"}])
          entry (first code-entries)]
      (expect (= "(def a 1)\n\n(def b 2)" normalized-code))
      (expect (= normalized-code (:expr entry)))
      (expect (true? (:multi-fence-merged? entry)))
      (expect (= 2 (:multi-fence-count entry))))))

(defdescribe malformed-direct-answer-repair-test
  (it "auto-repairs delimiter slips in direct answer blocks"
    (let [preflight (var-get #'lp/code-entries-preflight)
          parse-forms (var-get #'lp/parse-top-level-forms)
          src "(done [:ir [:p \"Hotword biasing - add \" [:c \"setHotwordsFile\") \"/\" [:c \"setHotwordsScore\"]]])"
          entry (first (:code-entries (preflight 1 [{:source src :lang "clojure"}])))
          parsed (parse-forms (:expr entry))]
      (expect (nil? (:repaired? entry)))
      (expect (string? (:repaired-source parsed)))
      (expect (nil? (:parse-error parsed)))
      (expect (false? (:vis/structurally-silent? entry)))
      (expect (= src (:expr entry)))
      (expect (str/includes? (:repaired-source parsed) "\"setHotwordsFile\" \"/\""))))

  (it "recovers direct answers torn by nested Markdown code fences"
    (let [env    (lp/create-environment ::router {:db :memory})
          answer "# Result\n\n```\nSUBCOMMANDS\n  export\n```\n\nDone."
          raw    (str "```clojure\n(done {:answer \"" answer "\"})\n```")
          torn   "(done {:answer \"# Result\n\n"]
      (try
        (with-redefs [svar/ask-code! (fn [_ _]
                                       {:blocks [{:lang "clojure" :source torn}]
                                        :raw raw
                                        :tokens {}})]
          (let [result (lp/run-iteration env []
                         {:iteration 0
                          :resolved-model {:provider :test :name "test"}})]
            (expect (= answer (get-in result [:final-result :answer :answer])))
            (expect (nil? (get-in result [:blocks 0 :error])))
            (expect (str/includes? (get-in result [:llm-executable-blocks 0 :source])
                      "```\\nSUBCOMMANDS"))))
        (finally
          (lp/dispose-environment! env)))))

  (it "recovers torn direct answers before executing nested example fences"
    (let [env    (lp/create-environment ::router {:db :memory})
          answer (str "# What went wrong\n\n"
                   "```clojure\n"
                   "(v/rg {:any [\":ext/slash-commands\"] :paths [\"extensions\" \"src\"]})\n"
                   "```\n"
                   "Done.")
          raw    (str "```clojure\n(done {:answer " (pr-str answer) "})\n```")
          torn   "(done {:answer \"# What went wrong\n\n```clojure\n(v/rg {:any [\\\":ext/slash-commands\\\"] :paths [\\\"extensions\\\" \\\"src\\\"]})"
          example "(v/rg {:any [\\\":ext/slash-commands\\\"] :paths [\\\"extensions\\\" \\\"src\\\"]})"]
      (try
        (with-redefs [svar/ask-code! (fn [_ _]
                                       {:blocks [{:lang "clojure" :source torn}
                                                 {:lang "clojure" :source example}]
                                        :raw raw
                                        :tokens {}})]
          (let [result (lp/run-iteration env []
                         {:iteration 0
                          :resolved-model {:provider :test :name "test"}})]
            (expect (= answer (get-in result [:final-result :answer :answer])))
            (expect (= 1 (count (:llm-executable-blocks result))))
            (expect (nil? (get-in result [:blocks 0 :error])))))
        (finally
          (lp/dispose-environment! env)))))

  (it "auto-repairs stray close delimiters before eval"
    (let [parsed ((var-get #'lp/parse-top-level-forms) "(def x 1))")]
      (expect (nil? (:parse-error parsed)))
      (expect (= "(def x 1)" (:repaired-source parsed)))
      (expect (= '(def x 1) (:form (first (:forms parsed)))))))

  (it "executes repaired source instead of failing pre-eval validators"
    (let [env (lp/create-environment ::router {:db :memory})]
      (try
        (let [result ((var-get #'lp/execute-code) env "(def x 1))")]
          (expect (nil? (:error result)))
          (expect (true? (:repaired? result)))
          (expect (= "(def x 1)" (:repaired-source result)))
          (expect (= 1 (:val (sci/eval-string+ (:sci-ctx env) "x" {:ns (:sandbox-ns env)})))))
        (finally
          (lp/dispose-environment! env)))))

  (it "per-form segments: `(done…) + (def…)` block surfaces answer-ref + :code (channel hides code via toggle)"
    (let [preflight (var-get #'lp/code-entries-preflight)
          src "(done {:answer \"ok\"})\n(def x \"doc\" 1)"
          entry (first (:code-entries (preflight 1 [{:source src :lang "clojure"}])))]
      ;; The engine-side `structurally-silent?` is narrow: only
      ;; true when ZERO `:code` segments survive parsing (a block
      ;; of pure recap / answer-ref forms). Anything with a `:code`
      ;; segment flows through; the CHANNEL hides those at paint
      ;; time when `:vis/show-raw-code` is OFF, so the user sees
      ;; only the answer / recap rails. This way flipping the toggle
      ;; ON reveals historical iterations without re-parsing.
      (expect (false? (:vis/structurally-silent? entry)))
      (expect (= [{:kind :answer-ref}
                  {:kind :code :source "(def x \"doc\" 1)"}]
                (:render-segments entry)))))

  (it "still hides standalone direct-answer blocks"
    (let [preflight (var-get #'lp/code-entries-preflight)
          src "(done {:answer \"ok\"})"
          entry (first (:code-entries (preflight 1 [{:source src :lang "clojure"}])))]
      (expect (true? (:vis/structurally-silent? entry)))
      (expect (= [{:kind :answer-ref}] (:render-segments entry)))))

  (it "mixed answer/def blocks: form-start chunk fires; CHANNEL decides per-paint whether to hide the code"
    ;; The engine no longer stamps `structurally-silent?` on a block
    ;; just because every `:code` form is a def — \":vis/show-raw-code\"
    ;; toggle ON should still reveal the def source. The CHANNEL
    ;; reads the toggle per paint instead. Loop emits the chunk
    ;; either way.
    (let [env    (lp/create-environment ::router {:db :memory})
          chunks (atom [])
          src    "(done {:answer \"ok\"})\n(def x \"doc\" 1)"]
      (try
        (with-redefs [svar/ask-code! (fn [_ _]
                                       {:blocks [{:source src :lang "clojure"}]
                                        :raw ""
                                        :tokens {}})]
          (lp/run-iteration env []
            {:iteration 0
             :resolved-model {:provider :test :name "test"}
             :on-chunk #(swap! chunks conj %)})
          (let [starts (filter #(= :form-start (:phase %)) @chunks)]
            (expect (= 1 (count starts)))
            (expect (false? (:vis/structurally-silent? (first starts))))))
        (finally
          (lp/dispose-environment! env)))))

  (it "does not rewrite malformed non-answer code when quotes are unsafe"
    (let [preflight (var-get #'lp/code-entries-preflight)
          parse-forms (var-get #'lp/parse-top-level-forms)
          src "(println \"a\" broken \"b)"
          entry (first (:code-entries (preflight 1 [{:source src :lang "clojure"}])))
          parsed (parse-forms (:expr entry))]
      (expect (nil? (:repaired? entry)))
      (expect (nil? (:repaired-source parsed)))
      (expect (some? (:parse-error parsed)))
      (expect (= src (:expr entry)))))

  (it "streams fn shorthand and regex literals through parser instead of rejecting them"
    (let [parse-forms (var-get #'lp/parse-top-level-forms)
          regex-src (str "(re-find #" "\"v\" \"voice\")")]
      (expect (nil? (:parse-error (parse-forms "(filter #(= % 1) [1 2])"))))
      (expect (nil? (:parse-error (parse-forms regex-src)))))))

(defdescribe top-level-do-unwrapping-test
  (it "unwraps legacy top-level do before display and eval; the def inside hides per the def-sink contract"
    (let [preflight (var-get #'lp/code-entries-preflight)
          src "(do (set-session-title! \"Triage render noise\") (def x \"doc\" 1))"
          entry (first (:code-entries (preflight 1 [{:source src :lang "clojure"}])))]
      (expect (= "(set-session-title! \"Triage render noise\")\n(def x \"doc\" 1)"
                (:expr entry)))
      (expect (true? (:vis/unwrapped-do? entry)))
      (expect (= [{:kind :title :value "Triage render noise"}
                  {:kind :code :source "(def x \"doc\" 1)"}]
                (:render-segments entry))))))

(defdescribe final-answer-gate-test
  (it "rejects answers from iterations that called extension tools"
    (let [err (lp/final-answer-gate-error
                {}
                1
                [{:id 0
                  :code "(v/cat \"deps.edn\")"
                  :channel [{:success? true :result [:ir {}]}]
                  :error nil}]
                {:answer "done"}
                nil)]
      (expect (string? err))
      (expect (str/includes? err "extension/tool"))))

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

(defdescribe raw-markdown-fence-leak-test
  ;; Vis conv 311fd734 / t3/i9: model emitted a single
  ;; `(done {:answer "… ```clojure (deftest …) ``` …"})` form whose
  ;; :answer string contained embedded markdown code samples. The old
  ;; preflight rejected ANY start-of-line ` ``` ` as raw chrome and
  ;; aborted the iteration before SCI ever saw the form — even though
  ;; the source parses cleanly (the backticks are inside a string
  ;; literal). Guard now consults the reader before rejecting.
  (let [leak-err @(ns-resolve 'com.blockether.vis.internal.loop
                    'raw-markdown-fence-leak-error)]
    (describe "raw-markdown-fence-leak-error"
      (it "returns nil when start-of-line backticks live inside a string literal of a balanced form"
        (let [src (str "(done\n"
                    "  {:answer\n"
                    "   \"## Findings\n\n"
                    "Example:\n\n"
                    "```clojure\n"
                    "(deftest x (is (= 1 1)))\n"
                    "```\n\n"
                    "End.\"})")]
          (expect (nil? (leak-err src)))))

      (it "flags a fence chunk that makes the source unreadable"
        ;; Stray triple-backtick + tagged fence + un-parsed body line
        ;; that breaks the reader (un-closed paren). Without the
        ;; reader check, the heuristic flagged correctly; with the
        ;; reader check we ONLY flag when the source truly fails to
        ;; parse, so the guard still catches the genuine-leak case.
        (let [src "(def x 1)\n```clojure\n(def y"]
          (expect (some? (leak-err src)))
          (expect (str/includes? (leak-err src) "fence leaked"))))

      (it "returns nil on a clean source with no fences"
        (expect (nil? (leak-err "(done {:answer \"all good\"})"))))

      (it "returns nil on empty / blank input"
        (expect (nil? (leak-err "")))
        (expect (nil? (leak-err "   \n  ")))))))
