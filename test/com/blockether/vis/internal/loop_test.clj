(ns com.blockether.vis.internal.loop-test
  (:require
   [clojure.string :as str]
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.ctx-loop :as ctx-loop]
   [com.blockether.vis.internal.loop :as lp]
   [com.blockether.vis.internal.titling :as titling]
   [com.blockether.vis.internal.runtime-settings :as rt]
   [com.blockether.vis.internal.provider-error :as perr]
   [com.blockether.vis.internal.env-python :as env]
   [com.blockether.vis.internal.persistance :as persistance]
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

(def ^:private provider-error-explanation perr/provider-error-explanation)

(def ^:private collect-iteration-start-hints
  (deref #'lp/collect-iteration-start-hints))

(def ^:private ask-result->api-usage
  (deref #'lp/ask-result->api-usage))

(def ^:private ask-code-block-observation
  (deref #'lp/ask-code-block-observation))

(def ^:private prose-beyond-code
  (deref #'lp/prose-beyond-code))

(defdescribe prose-beyond-code-test
  ;; The model often restates its run_python code in its message prose; that
  ;; prose must be SUPPRESSED so it doesn't render as a dim duplicate of the
  ;; real code block. Only genuine commentary survives.
  (let [tc [{:input {:code "await patch(x)"}}]]
    (it "suppresses prose that is only the code in a fence"
      (expect (nil? (prose-beyond-code "```python\nawait patch(x)\n```" tc))))
    (it "suppresses prose that is the code verbatim (no fence)"
      (expect (nil? (prose-beyond-code "await patch(x)" tc))))
    (it "keeps prose that adds commentary beyond the code"
      (expect (= "I'll bump the **timeout**.\n```python\nawait patch(x)\n```"
                (prose-beyond-code "I'll bump the **timeout**.\n```python\nawait patch(x)\n```" tc))))
    (it "keeps pure commentary with no code at all"
      (expect (= "Done — re-running tests." (prose-beyond-code "Done — re-running tests." tc))))
    (it "is nil for blank / nil prose"
      (expect (nil? (prose-beyond-code nil tc)))
      (expect (nil? (prose-beyond-code "   " tc))))))

(def ^:private eval-timeout-ms-for-code
  (deref #'rt/eval-timeout-ms-for-code))

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

(def ^:private call-provider-with-stream-rewind-retry!
  (deref #'lp/call-provider-with-stream-rewind-retry!))

(def ^:private run-normal-turn!
  (deref #'lp/run-normal-turn!))

(def ^:private maybe-auto-title!
  (deref #'titling/maybe-auto-title!))

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
                              ;; Native tool calling: a reply with NO tool call
                              ;; (`:stop-reason :end`) is the answer (`:content`)
                              ;; — finalizes the turn.
                              {:stop-reason :end
                               :tool-calls []
                               :content "ok"
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
          (lp/dispose-environment! env)))))

  (it "uses three capped exponential rewind retries for transient stream failures"
    (let [calls  (atom 0)
          chunks (atom [])]
      (with-redefs [lp/PROVIDER_STREAM_REWIND_DELAYS_MS [0 0 0]]
        (let [result (call-provider-with-stream-rewind-retry!
                       {:cancel-atom (atom false)}
                       {:iteration-position 1
                        :provider "openai"
                        :model "gpt-x"
                        :on-chunk #(swap! chunks conj %)
                        :reset-stream-state! (fn [])}
                       (fn []
                         (if (<= (swap! calls inc) 3)
                           (throw (ex-info "Stream connection error: closed"
                                    {:type :svar.core/http-error :stream? true}))
                           {:routed/trace []})))]
          (expect (= 4 @calls))
          (expect (= [1 2 3]
                    (mapv :attempt (filter #(= :provider-retry-reset (:phase %)) @chunks))))
          (expect (= [1 2 3]
                    (mapv :attempt (:routed/trace result)))))))))

(defdescribe native-tool-call-execution-test
  ;; REGRESSION: native tool calling once shipped 100% broken — `run-iteration`
  ;; synthesized `env* (assoc environment)` (a 1-arg assoc) before execute-code, so
  ;; EVERY tool-call iteration threw ArityException ("Provider unavailable / Wrong
  ;; number of args (1) passed to clojure.core/assoc"). 120+ loop tests stayed green
  ;; because none drove a real tool-call response through `run-iteration`. This does.
  (it "executes a python_execution tool call through run-iteration without throwing"
    (let [env    (lp/create-environment ::router {:db :memory})
          chunks (atom [])]
      (try
        (with-redefs [svar/ask-code!
                      (fn [_router _opts]
                        {:stop-reason :tool-calls
                         :tool-calls  [{:id "call_1" :name "python_execution"
                                        :input {:code "print(6*7)"}}]
                         :content     nil
                         :reasoning   "computing"
                         :tokens      {}})]
          ;; The bug threw HERE — a tool-call iteration reaching the execute path.
          (let [result   (lp/run-iteration env []
                           {:iteration 0
                            :resolved-model {:provider :zai-coding-plan :name "glm-5.1"}
                            :on-chunk #(swap! chunks conj %)})
                tool-calls (:tool-calls result)
                form-res   (first (filter #(= :form-result (:phase %)) @chunks))]
            ;; tool-call iteration (not a final answer)
            (expect (nil? (:final-result result)))
            (expect (= 1 (count tool-calls)))
            (expect (= "python_execution" (:name (first tool-calls))))
            ;; the call ACTUALLY executed in the sandbox (env was passed correctly):
            ;; python_execution returns what it print()s.
            (expect (some? form-res))
            (expect (nil? (:error form-res)))
            (expect (str/includes? (str (:stdout form-res)) "42"))))
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
  ;; Cross-process RESUME carry must be a pure function of the DB so the wire is
  ;; identical regardless of process (see DERIVED_WIRE.md). These pin: ALL prior
  ;; answered turns carried (not just the latest), each with its r[] scope index;
  ;; determinism; and summary-awareness (drop/summarize reshape uniformly).
  (it "carries ALL prior answered turns with their r[] scope index"
    (with-redefs [persistance/db-list-session-turns
                  (fn [_db session-id]
                    (expect (= "s1" session-id))
                    [{:id "t1" :status :done :position 1 :user-request "Read a" :answer-markdown "Read it"}
                     {:id "t2" :status :done :position 2 :user-request "Read b" :answer-markdown "Read b too"}
                     {:id "t3" :status :running :user-request "yes"}])
                  persistance/db-list-session-turn-iterations
                  (fn [_db id]
                    (case id
                      "t1" [{:status :done :position 1
                             :forms [{:scope "t1/i1/f1" :src "cat(\"a\")" :result {:path "a"}}
                                     {:scope "t1/i1/f2" :src "set_session_title(...)" :result "vis_silent"}]}]
                      "t2" [{:status :done :position 1
                             :forms [{:scope "t2/i1/f1" :src "rg({...})" :result {:hits []}}]}]
                      []))]
      (let [out (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})} "t3")]
        (expect (= 2 (count out)))                                          ; both answered turns, not just latest
        (expect (= "Read a" (:user-request (first out))))
        (expect (= [{:scope "t1/i1/f1" :src "cat(\"a\")"}] (:results (first out)))) ; sentinel f2 excluded
        (expect (= [{:scope "t2/i1/f1" :src "rg({...})"}] (:results (second out)))))))

  (it "is deterministic — same DB ⇒ identical output (process-invariant)"
    (with-redefs [persistance/db-list-session-turns
                  (constantly [{:id "t1" :status :done :position 1 :user-request "q" :answer-markdown "a"}])
                  persistance/db-list-session-turn-iterations
                  (constantly [{:status :done :position 1
                                :forms [{:scope "t1/i1/f1" :src "cat(x)" :result {:k 1}}]}])]
      (let [env {:session-id "s1" :db-info ::db :ctx-atom (atom {})}]
        (expect (= (previous-turn-context env "t9") (previous-turn-context env "t9"))))))

  (it "is summary-aware: drop OMITS a scope, summarize SWAPS src->gist"
    (with-redefs [persistance/db-list-session-turns
                  (constantly [{:id "t1" :status :done :position 1 :user-request "q" :answer-markdown "a"}])
                  persistance/db-list-session-turn-iterations
                  (constantly [{:status :done :position 1
                                :forms [{:scope "t1/i1/f1" :src "cat(a)" :result {:k 1}}
                                        {:scope "t1/i1/f2" :src "cat(b)" :result {:k 2}}]}])]
      (let [env {:session-id "s1" :db-info ::db
                 :ctx-atom (atom {:session/summaries [{:scopes #{"t1/i1/f1"}}                 ; drop f1
                                                      {:scopes #{"t1/i1/f2"} :gist "b pinned"}]})} ; summarize f2
            results (:results (first (previous-turn-context env "t9")))]
        (expect (= 1 (count results)))                                      ; f1 dropped
        (expect (= "t1/i1/f2" (:scope (first results))))
        (expect (= "b pinned" (:gist (first results))))
        (expect (nil? (:src (first results)))))))

  (it "returns nil when every prior turn is current/running/blank-answer"
    (with-redefs [persistance/db-list-session-turns
                  (constantly
                    [{:id "t1" :status :done :position 1 :user-request "old" :answer-markdown ""}
                     {:id "t2" :status :running :user-request "now" :answer-markdown "partial"}])
                  persistance/db-list-session-turn-iterations (constantly [])]
      (expect (nil? (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})} "t2")))))

  (it "carries prior provider-error turns as unfinished cross-turn context"
    (with-redefs [persistance/db-list-session-turns
                  (constantly
                    [{:id "t1" :status :error :position 1 :user-request "fix web" :answer-markdown "## 🚨 PROVIDER_ERROR"}
                     {:id "t2" :status :running :user-request "continue"}])
                  persistance/db-list-session-turn-iterations
                  (constantly [{:status :done :position 1
                                :forms [{:scope "t1/i1/f1" :src "cat(ui)" :stdout "read ui"}]}])]
      (let [out (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})} "t2")]
        (expect (= [{:user-request "fix web"
                     :answer nil
                     :interrupted? true
                     :results [{:scope "t1/i1/f1" :src "cat(ui)"}]}]
                  out))))))

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
    ;; re-derive scratch state each iteration, pinning `cached_tokens`
    ;; across many iterations before the fix.
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
    (expect (= (* 60 1000) rt/ASK_CODE_TTFT_TIMEOUT_MS))
    (expect (= (* 3 60 1000) rt/ASK_CODE_IDLE_TIMEOUT_MS))
    (let [{:keys [router opts]} (captured-ask-code-opts {:lang "clojure" :messages []})]
      (expect (= ::router router))
      (expect (= rt/ASK_CODE_TTFT_TIMEOUT_MS (:ttft-timeout-ms opts)))
      (expect (= rt/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts)))
      ;; Semantic timeout is now auto-added by `with-default-ask-code-idle-timeout`
      ;; (default 4min, catches transport-alive-but-model-silent stalls).
      (expect (= rt/ASK_CODE_SEMANTIC_TIMEOUT_MS (:semantic-timeout-ms opts)))))

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
    ;; no model events\" inside 4 minutes (a transport-alive stream with
    ;; zero events could otherwise stall for many minutes).
    (expect (= (* 4 60 1000) rt/ASK_CODE_SEMANTIC_TIMEOUT_MS))
    (let [opts (:opts (captured-ask-code-opts {:semantic-timeout-ms 180000}))]
      (expect (= 180000 (:semantic-timeout-ms opts)))
      (expect (= rt/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts))))
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

  (it "extends the outer eval timeout when shell code asks for a longer timeout"
    (expect (= 120000 (eval-timeout-ms-for-code 120000 "print(1)")))
    (expect (= 190000
              (eval-timeout-ms-for-code
                120000
                "await shell_run(\"./verify.sh --quick\", {\"timeout_secs\": 180})")))
    (expect (= 310000
              (eval-timeout-ms-for-code
                120000
                "subprocess.run([\"sleep\", \"1\"], timeout=300)"))))

  (it "splits + evals multi-form blocks whose statements contain astral chars (emoji)"
    ;; Regression: GraalPy's ast.get_source_segment truncates
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
          {:keys [error result]} (env/run-python-block python-context code)]
      (expect (nil? error))
      ;; the final expression re-reads the multi-line emoji string unchanged
      (expect (string? result))
      (expect (clojure.string/includes? result "👆"))
      (expect (clojure.string/includes? result "🌳"))
      (expect (clojure.string/includes? result "Pełne ł ó ż")))))

(defdescribe final-answer-gate-test
  ;; `final-answer-gate-error` itself carries ONLY extension
  ;; :turn.answer/validate vetoes. The structural "done() shared its fence with
  ;; a MUTATION/FAILED op" structural gate is GONE with the fence reader
  ;; (a reply with a ```python fence = code, else the prose is the answer);
  ;; this fn now carries ONLY extension
  ;; :turn.answer/validate vetoes.
  (it "does not reject a done() that ran alongside a pure read (cat)"
    (expect (nil? (lp/final-answer-gate-error
                    {}
                    1
                    [{:id 0
                      :code "cat(\"deps.edn\")"
                      :channel [{:success? true :tag :observation :result [:ir {}]}]
                      :error nil}]
                    {:answer "done"}
                    nil))))

  (it "allows answer-only iterations when no extension tool ran"
    (expect (nil? (lp/final-answer-gate-error
                    {}
                    1
                    [{:id 0
                      :code "1 + 2"
                      :result 3
                      :error nil}]
                    {:answer "done"}
                    nil)))))

;; ---------------------------------------------------------------------------
;; def-sink -> vars-snapshot (per-var precise source extraction)
;; ---------------------------------------------------------------------------

(defdescribe gather-builtin-test
  "maki-style in-program concurrency: `await gather(*awaitables)` runs each
   awaitable on a virtual thread and returns results IN ORDER. Guards the async
   runtime end-to-end through a real sandbox: the await path AST-wraps + drives
   the coroutine, gather dispatches awaitables to __vis_par__ (the host
   virtual-thread pool). Concurrency itself is proven by GraalPy lock-release."
  (it "awaits gathered coroutines and returns their results in order"
    (let [environment (lp/create-environment ::router {:db :memory})]
      (try
        (let [r (env/run-python-block (:python-context environment)
                  "async def work(n):\n    return n * n\nvals = await gather(work(2), work(3), work(4))\nprint(list(vals))"
                  "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "[4, 9, 16]" (clojure.string/trim (str (:stdout r))))))
        (finally (try (lp/dispose-environment! environment) (catch Throwable _ nil)))))))

(defdescribe iteration-summarize-test
  "summarize/drop operate at ITERATION (tN/iN) granularity: a summarized step
   collapses entirely (its assistant+tool_result pair leaves the wire) to one
   gist line; a non-collapsed step renders as a tool_result tagged `# tN/iN`."
  (let [apply-summaries (var-get #'lp/apply-summaries)
        irm             (var-get #'lp/iteration-results-message)]
    (it "summarize([tN/iN]) tags the iteration :collapsed? and swaps it for the gist"
      (let [tis [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big output"}]}]
                 [2 {:forms-vec [{:scope "t1/i2/f1" :stdout "keep me"}]}]]
            out (apply-summaries tis [{:scopes #{"t1/i1"} :gist "did the thing"}])
            r1  (second (first out))
            r2  (second (second out))]
        (expect (true? (:collapsed? r1)))
        (expect (nil? (:collapsed? r2)))
        ;; collapsed → plain-text gist line (NOT a tool_result)
        (expect (= "# -- t1/i1 -- summarized: did the thing" (:content (irm r1))))))

    (it "drop([tN/iN]) collapses to a `-- dropped` line"
      (let [out (apply-summaries [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big"}]}]]
                  [{:scopes #{"t1/i1"}}])]
        (expect (= "# -- t1/i1 -- dropped" (:content (irm (second (first out))))))))

    (it "a live step renders as a tool_result tagged with its # tN/iN handle"
      (let [m (irm {:forms-vec [{:scope "t1/i1/f1" :stdout "hello"}]
                    :tool-calls [{:id "c1"}]})]
        (expect (= "c1" (get-in m [:content 0 :tool_use_id])))
        (expect (str/includes? (get-in m [:content 0 :content]) "# t1/i1"))
        (expect (str/includes? (get-in m [:content 0 :content]) "hello"))))

    (it "no summaries ⇒ trailer-iters unchanged"
      (let [tis [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "x"}]}]]]
        (expect (= tis (apply-summaries tis [])))))))

(defdescribe native-tool-result-pairing-test
  "REARCHITECTURE (same DB schema): an iteration is a LIST of tool-calls, one of
   which may be `python_execution`. Each tool_use gets its OWN tool_result,
   carrying ITS OWN forms' return — forms are grouped by `:svar/tool-call-id`. A
   DIRECT file tool's return is its value; python_execution's return IS the text
   it print()s. No more 'first call carries everything, the rest get a stub'."
  (let [irm (var-get #'lp/iteration-results-message)
        pre (var-get #'lp/code-entries-preflight)]
    (it "each parallel tool_use is answered by its OWN result"
      (let [m (irm {:tool-calls [{:id "A" :name "cat"}
                                 {:id "B" :name "rg"}
                                 {:id "P" :name "python_execution"}]
                    ;; native cat/rg carry :result (their value); python_execution
                    ;; carries :stdout (what it printed) — the engine emits ONE
                    ;; channel per call, never both.
                    :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "A" :result "AAA"}
                                {:scope "t1/i1/f2" :svar/tool-call-id "B" :result "BBB"}
                                {:scope "t1/i1/f3" :svar/tool-call-id "P" :stdout "PPP"}]})
            by-id (into {} (map (juxt :tool_use_id :content)) (:content m))]
        (expect (= 3 (count (:content m))))
        ;; native cat/rg → their RETURN value; not cross-contaminated
        (expect (str/includes? (by-id "A") "AAA"))
        (expect (not (str/includes? (by-id "A") "BBB")))
        (expect (str/includes? (by-id "B") "BBB"))
        ;; python_execution → its PRINTED string
        (expect (str/includes? (by-id "P") "PPP"))))

    (it "a FAILED call's tool_result is flagged :is_error true; a successful one is not"
      (let [m (irm {:tool-calls [{:id "ok" :name "cat"} {:id "bad" :name "cat"}]
                    :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "ok" :result "FILE"}
                                {:scope "t1/i1/f2" :svar/tool-call-id "bad" :error "No such file"}]})
            by-id (into {} (map (juxt :tool_use_id identity)) (:content m))]
        ;; svar passes :is_error to Anthropic as `is_error: true`; on OpenAI/Gemini
        ;; the error TEXT carries the signal.
        (expect (nil? (:is_error (by-id "ok"))))
        (expect (true? (:is_error (by-id "bad"))))
        (expect (str/includes? (:content (by-id "bad")) "No such file"))))

    (it "a call that produced no output returns the no-return hint"
      ;; engine emitted neither :stdout nor :result (e.g. python_execution that
      ;; only did assignments and printed nothing)
      (let [m (irm {:tool-calls [{:id "P" :name "python_execution"}]
                    :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "P"}]})]
        (expect (str/includes? (get-in m [:content 0 :content]) "no return"))))

    (it "an unpaired/fold form folds onto the FIRST call (nothing lost)"
      (let [m (irm {:tool-calls [{:id "A" :name "cat"}]
                    :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "A" :result "body"}
                                {:summary? true :summary-iters ["t1/i0"] :summary-gist "ctx"}]})
            c (get-in m [:content 0 :content])]
        (expect (str/includes? c "body"))
        (expect (str/includes? c "summarized: ctx"))))

    (it "code-entries-preflight keeps distinct native tool-calls SEPARATE (no merge)"
      (let [entries (:code-entries
                     (pre 1 [{:lang "python" :source "cat(\"a\")" :svar/tool-call-id "A" :vis/tool-name "cat"}
                             {:lang "python" :source "rg({\"any\":[\"x\"]})" :svar/tool-call-id "B" :vis/tool-name "rg"}]))]
        (expect (= 2 (count entries)))
        (expect (= ["A" "B"] (mapv :svar/tool-call-id entries)))))

    (it "code-entries-preflight STILL merges legacy id-less blocks (provider stutter)"
      (let [entries (:code-entries
                     (pre 1 [{:lang "python" :source "x = 1"}
                             {:lang "python" :source "y = 2"}]))]
        (expect (= 1 (count entries)))))))

(defdescribe repetition-loop-detection-test
  "Repetition-only loop detector + decision-checkpoint. No iteration/budget
   counting — fires solely on identical action code repeated across iterations."
  (let [detect (var-get #'lp/repetition-loop-state)
        msg    (var-get #'lp/loop-checkpoint-message)]
    (describe "repetition-loop-state"
      (it "is not stuck on a single iteration"
        (let [r (detect [{:code "rg({\"any\": [\"x\"]})"}] nil)]
          (expect (false? (:stuck? r)))))

      (it "trips when identical action code repeats across iterations"
        (let [blocks [{:code "rg({\"any\": [\"cancel\"]})"} {:code "cat(\"x.clj\")"}]
              r1 (detect blocks nil)
              r2 (detect blocks {:last-sig (:action-sig r1)})]
          (expect (false? (:stuck? r1)))
          (expect (true? (:stuck? r2)))))

      (it "does not trip on distinct action code across iterations"
        (let [r1 (detect [{:code "rg({\"any\": [\"a\"]})"}] nil)
              r2 (detect [{:code "rg({\"any\": [\"b\"]})"}] {:last-sig (:action-sig r1)})]
          (expect (false? (:stuck? r2))))))

    (describe "loop-checkpoint-message"
      (it "shows the sticky best-answer and forces a decision"
        (let [m (msg "The atom and token serve distinct roles.")]
          (expect (str/includes? m "STOP"))
          (expect (str/includes? m "best answer so far"))
          (expect (str/includes? m "The atom and token serve distinct roles."))
          (expect (str/includes? m "plain prose"))))
      (it "handles no answer yet"
        (let [m (msg nil)]
          (expect (str/includes? m "NOT produced any answer")))))))

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

(defdescribe sub-loop!-test
  (describe "sub-loop! assembly (stubbed env/turn — no LLM, no FS)"
    (let [child-ctx (atom {})
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
        (expect (= "child-ws" (get-in @captured [:opts :workspace-id])))
        ;; child soul links to the PARENT's session_state (cross-soul) → hidden
        ;; from the top-level list, queryable as the parent's sub-tree
        (expect (= "parent-state-123" (get-in @captured [:opts :child :parent-state-id]))))
      (it "returns the focus result shape (task_id/status/answer)"
        (expect (= "oauth" (:task_id r)))
        ;; status is the child turn's status, coerced to a python-facing STRING
        (expect (= "success" (:status r)))
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

;; The reply is ONE program: multiple fences a provider splits out collapse to a
;; single code-entry so the form cap spans the whole reply and r["…/fF"] numbers
;; continuously (no per-fence f1 collision).
(defdescribe code-entries-preflight-merge-test
  (it "collapses multiple fenced blocks into ONE code-entry = the normalized concat"
    (let [pre     (@#'lp/code-entries-preflight 2
                                                [{:source "rg(1)" :lang "python"}
                                                 {:source "cat(2)" :lang "python"}])
          entries (:code-entries pre)]
      (expect (= 1 (count entries)))
      (expect (= "rg(1)\n\ncat(2)" (:expr (first entries))))))

  (it "leaves a single fenced block untouched"
    (let [entries (:code-entries (@#'lp/code-entries-preflight 1
                                                               [{:source "rg(1)" :lang "python"}]))]
      (expect (= 1 (count entries)))
      (expect (= "rg(1)" (:expr (first entries))))))

  (it "dedups identical stutter-fences first (one survivor, not merged with itself)"
    (let [entries (:code-entries (@#'lp/code-entries-preflight 2
                                                               [{:source "rg(1)" :lang "python"}
                                                                {:source "rg(1)" :lang "python"}]))]
      (expect (= 1 (count entries)))
      (expect (= "rg(1)" (:expr (first entries)))))))

;; The model-facing disclosure: a trimmed iteration tells the model what dropped.
(defdescribe literal-code-block-error-test
  (let [err #'lp/literal-code-block-error]
    (it "valid Python code passes the guard (nil)"
      (expect (nil? (err "x = 1"))))
    (it "a bare string program is rejected and points at native answering, not :answer/:code"
      (let [m (err "\"just prose\"")]
        (expect (some? m))
        (expect (str/includes? m "run_python"))
        (expect (not (str/includes? m ":answer")))))
    (it "a leaked Markdown fence says PYTHON, never Clojure"
      (let [m (err "```python")]
        (expect (some? m))
        (expect (str/includes? m "Python"))
        (expect (not (str/includes? m "Clojure")))))
    (it "a comment-only block references `#` (Python), not `;;`/`#_`"
      (let [m (err "# only a comment")]
        (expect (some? m))
        (expect (str/includes? m "#"))
        (expect (not (str/includes? m ";;")))))))

(defdescribe live-code-from-tool-input-test
  ;; Native tool calling streams the model's Python as the run_python tool
  ;; call's `{"code": …}` argument JSON. The live bubble decodes the `code`
  ;; value (possibly mid-stream / truncated) so it can paint the code as it is
  ;; written instead of showing only reasoning.
  (let [decode @#'lp/live-code-from-tool-input]
    (it "decodes a complete code argument, unescaping JSON string escapes"
      (expect (= "(git/status)\nprint(1)"
                (decode "{\"code\": \"(git/status)\\nprint(1)\"}"))))
    (it "returns the partial value when the args JSON is truncated mid-stream"
      (expect (= "(git/sta" (decode "{\"code\": \"(git/sta"))))
    (it "returns nil before the code key/opening quote has streamed"
      (expect (nil? (decode "{\"co")))
      (expect (nil? (decode "")))
      (expect (nil? (decode "   "))))
    (it "decodes \\uXXXX escapes and stops at the closing quote"
      (expect (= "aAb" (decode "{\"code\":\"a\\u0041b\"}"))))
    (it "keeps escaped quotes/backslashes inside the code intact"
      (expect (= "print(\"x\\y\")"
                (decode "{\"code\":\"print(\\\"x\\\\y\\\")\"}"))))))
