(ns com.blockether.vis.internal.loop-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.ctx-loop :as ctx-loop]
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.ctx-engine :as eng]
            [com.blockether.vis.internal.foundation.editing.core :as ed]
            [com.blockether.vis.internal.foundation.shell :as sh]
            [com.blockether.vis.internal.foundation.language-surface :as lsf]
            [com.blockether.vis.internal.titling :as titling]
            [com.blockether.vis.internal.runtime-settings :as rt]
            [com.blockether.vis.internal.provider-error :as perr]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.env-python :as env]
            [com.blockether.vis.internal.persistance :as persistance]
            [com.blockether.vis.internal.workspace :as workspace]
            [lazytest.core :refer [defdescribe describe it expect throws?]]))

(defn- captured-ask-code-opts
  [opts]
  (let [seen (atom nil)]
    (with-redefs-fn {#'lp/get-router (fn []
                                       ::router)
                     #'svar/ask-code! (fn [router opts]
                                        (reset! seen {:router router :opts opts})
                                        {:blocks [] :raw ""})}
      #(lp/ask-code! opts))
    @seen))

(def ^:private provider-error-explanation perr/provider-error-explanation)

(def ^:private collect-iteration-start-hints (deref #'lp/collect-iteration-start-hints))

(def ^:private ask-result->api-usage (deref #'lp/ask-result->api-usage))

(def ^:private ask-code-block-observation (deref #'lp/ask-code-block-observation))
(def ^:private log-stage-level (deref #'lp/log-stage-level))

(defdescribe loop-stage-logging-test
             (it "keeps routine telemetry debug-only but logs failed turns at error level"
                 (expect (= :debug (log-stage-level :provider-call/stop {:duration-ms 12})))
                 (expect (= :error (log-stage-level :error {:reason :provider-failed})))
                 (expect (= :error (log-stage-level :turn/complete {:status :error})))
                 (expect (= :info (log-stage-level :error {:reason :cancelled})))
                 (expect (= :info (log-stage-level :turn/complete {:status :cancelled})))))

(def ^:private prose-beyond-code (deref #'lp/prose-beyond-code))

(defdescribe
  copilot-action-service-headers-test
  (it "marks Copilot Enterprise requests with X-Initiator for the action service"
      (expect (= {"X-Initiator" "agent"}
                 (#'lp/copilot-llm-headers {:provider :github-copilot-enterprise} "agent"))))
  (it "does not add action-service headers for non-Copilot providers"
      (expect (nil? (#'lp/copilot-llm-headers {:provider :anthropic-coding-plan} "agent")))))

(defdescribe
  environment-lifecycle-test
  (it "closes the GraalPy context when disposing an environment"
      (let [environment
            (lp/create-environment ::router {:db :memory})

            python-context
            (:python-context environment)]

        (lp/dispose-environment! environment)
        (expect (try (env/run-python-block python-context "1") false (catch Throwable _ true))))))

(defdescribe
  prose-beyond-code-test
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
                   (prose-beyond-code "I'll bump the **timeout**.\n```python\nawait patch(x)\n```"
                                      tc))))
    (it "keeps pure commentary with no code at all"
        (expect (= "Done — re-running tests." (prose-beyond-code "Done — re-running tests." tc))))
    (it "is nil for blank / nil prose"
        (expect (nil? (prose-beyond-code nil tc)))
        (expect (nil? (prose-beyond-code "   " tc))))))

(def ^:private eval-timeout-ms-for-code (deref #'rt/eval-timeout-ms-for-code))

(def ^:private preserved-thinking-replay-messages (deref #'lp/preserved-thinking-replay-messages))

(def ^:private compatible-preserved-thinking-trailer-iters
  (deref #'lp/compatible-preserved-thinking-trailer-iters))

(def ^:private conversation-suffix (deref #'lp/conversation-suffix))

(def ^:private max-tokens-exceeded-error? (deref #'lp/max-tokens-exceeded-error?))

(def ^:private provider-unavailable-error? (deref #'lp/provider-unavailable-error?))
(def ^:private provider-unavailable-retry? (deref #'lp/provider-unavailable-retry?))
(def ^:private provider-unavailable-retry-delay-ms (deref #'lp/provider-unavailable-retry-delay-ms))
(def ^:private next-retry-counters (deref #'lp/next-retry-counters))
(def ^:private MAX_PROVIDER_UNAVAILABLE_RETRIES (deref #'lp/MAX_PROVIDER_UNAVAILABLE_RETRIES))

(def ^:private bumped-max-tokens-extra-body (deref #'lp/bumped-max-tokens-extra-body))

(def ^:private llm-provider-error-context (deref #'lp/llm-provider-error-context))

(def ^:private previous-turn-context (deref #'lp/previous-turn-context))

(def ^:private previous-request-usage (deref #'lp/previous-request-usage))

(def ^:private call-provider-with-interrupt-retry! (deref #'lp/call-provider-with-interrupt-retry!))

(def ^:private retryable-provider-interrupt? (deref #'lp/retryable-provider-interrupt?))

(def ^:private INTERRUPT_RETRY_MAX_ELAPSED_MS (deref #'lp/INTERRUPT_RETRY_MAX_ELAPSED_MS))

(def ^:private call-provider-with-stream-rewind-retry!
  (deref #'lp/call-provider-with-stream-rewind-retry!))

(def ^:private run-normal-turn! (deref #'lp/run-normal-turn!))

(def ^:private maybe-auto-title! (deref #'titling/maybe-auto-title!))

(defdescribe
  provider-stream-rewind-retry-test
  (it
    "rewinds streamed reasoning and retries the provider call before eval"
    (let [env
          (lp/create-environment ::router {:db :memory})

          calls
          (atom 0)

          chunks
          (atom [])]

      (try
        (with-redefs [svar/ask-code!
                      (fn [_router opts]
                        (case (swap! calls inc)
                          1
                          (do ((:on-chunk opts)
                                {:reasoning "dead thinking" :content "```clojure\n(dead)"})
                              (throw (ex-info "Stream connection error: closed"
                                              {:type :svar.core/http-error
                                               :stream? true
                                               :content-acc-len 19
                                               :reasoning-acc-len 13
                                               :reasoning "dead thinking"
                                               :partial-content "```clojure\n(dead)"})))

                          2
                          (do ((:on-chunk opts) {:reasoning "fresh thinking"})
                              ;; Native tool calling: a reply with NO tool call
                              ;; (`:stop-reason :end`) is the answer (`:content`)
                              ;; — finalizes the turn.
                              {:stop-reason :end
                               :tool-calls []
                               :content "ok"
                               :reasoning "fresh thinking"
                               :tokens {}})))]
          (let [result (lp/run-iteration env
                                         []
                                         {:iteration 0
                                          :resolved-model {:provider :zai-coding-plan
                                                           :name "glm-5.1"}
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
        (finally (lp/dispose-environment! env)))))
  (it "uses three capped exponential rewind retries for transient stream failures"
      (let [calls
            (atom 0)

            chunks
            (atom [])]

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
            (expect (= [1 2 3] (mapv :attempt (:routed/trace result)))))))))

(defdescribe native-tool-call-execution-test
             ;; REGRESSION: native tool calling once shipped 100% broken — `run-iteration`
             ;; synthesized `env* (assoc environment)` (a 1-arg assoc) before execute-code, so
             ;; EVERY tool-call iteration threw ArityException ("Provider unavailable / Wrong
             ;; number of args (1) passed to clojure.core/assoc"). 120+ loop tests stayed green
             ;; because none drove a real tool-call response through `run-iteration`. This does.
             (it
               "executes a python_execution tool call through run-iteration without throwing"
               (let [env
                     (lp/create-environment ::router {:db :memory})

                     chunks
                     (atom [])]

                 (try (with-redefs [svar/ask-code! (fn [_router _opts]
                                                     {:stop-reason :tool-calls
                                                      :tool-calls [{:id "call_1"
                                                                    :name "python_execution"
                                                                    :input {:code "print(6*7)"}}]
                                                      :content nil
                                                      :reasoning "computing"
                                                      :tokens {}})]
                        ;; The bug threw HERE — a tool-call iteration reaching the execute path.
                        (let [result (lp/run-iteration env
                                                       []
                                                       {:iteration 0
                                                        :resolved-model {:provider :zai-coding-plan
                                                                         :name "glm-5.1"}
                                                        :on-chunk #(swap! chunks conj %)})
                              tool-calls (:tool-calls result)
                              form-res (first (filter #(= :form-result (:phase %)) @chunks))]

                          ;; tool-call iteration (not a final answer)
                          (expect (nil? (:final-result result)))
                          (expect (= 1 (count tool-calls)))
                          (expect (= "python_execution" (:name (first tool-calls))))
                          ;; the call ACTUALLY executed in the sandbox (env was passed correctly):
                          ;; python_execution returns what it print()s.
                          (expect (some? form-res))
                          (expect (nil? (:error form-res)))
                          (expect (str/includes? (str (:stdout form-res)) "42"))))
                      (finally (lp/dispose-environment! env))))))

(defdescribe provider-interrupt-retry-test
             (it "retries a provider interrupt once when user did not cancel"
                 (let [calls
                       (atom 0)

                       out
                       (call-provider-with-interrupt-retry!
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
                   (try (call-provider-with-interrupt-retry! {:cancel-atom (atom true)}
                                                             6
                                                             (fn []
                                                               (swap! calls inc)
                                                               (throw (InterruptedException.
                                                                        "cancel"))))
                        (expect false)
                        (catch InterruptedException _ (expect (= 1 @calls))))))
             (it "treats a live thread interrupt as user cancel even when the atom lost the race"
                 ;; Issue #13: on Esc the worker-thread interrupt can land BEFORE
                 ;; `vis/cancel!` flips the cancel-atom. The atom still reads false
                 ;; here, but the live interrupt must classify this as a user cancel
                 ;; (NOT a retryable blip).
                 (let [verdict (atom nil)]
                   (try (.interrupt (Thread/currentThread))
                        (reset! verdict (retryable-provider-interrupt?
                                          (InterruptedException. "java.lang.InterruptedException")
                                          {:cancel-atom (atom false)}
                                          500))
                        (finally
                          ;; clear the interrupt we set so it cannot leak into later tests
                          (Thread/interrupted)))
                   (expect (false? @verdict))))
             (it "retries a spurious interrupt that fired before the TTFT budget"
                 (expect (retryable-provider-interrupt? (InterruptedException.
                                                          "java.lang.InterruptedException")
                                                        {:cancel-atom (atom false)}
                                                        500)))
             (it "does NOT retry a watchdog interrupt that fired past the TTFT budget"
                 ;; svar idle/semantic watchdog: the call already burned its full
                 ;; budget, so retrying just doubles the wall-clock hang. Surface it.
                 (expect (not (retryable-provider-interrupt?
                                (InterruptedException. "java.lang.InterruptedException")
                                {:cancel-atom (atom false)}
                                (inc (long INTERRUPT_RETRY_MAX_ELAPSED_MS)))))))

(defdescribe
  previous-turn-context-test
  ;; Cross-process RESUME carry must be a pure function of the DB so the wire is
  ;; identical regardless of process (see DERIVED_WIRE.md). These pin: ALL prior
  ;; answered turns carried (not just the latest), each with its r[] scope index;
  ;; determinism; and summary-awareness (drop/summarize reshape uniformly).
  (it
    "carries ALL prior answered turns with their r[] scope index"
    (with-redefs [persistance/db-list-session-turns
                  (fn [_db session-id]
                    (expect (= "s1" session-id))
                    [{:id "t1"
                      :status :done
                      :position 1
                      :user-request "Read a"
                      :answer-markdown "Read it"}
                     {:id "t2"
                      :status :done
                      :position 2
                      :user-request "Read b"
                      :answer-markdown "Read b too"}
                     {:id "t3" :status :running :user-request "yes"}])

                  persistance/db-list-session-turn-iterations
                  (fn [_db id]
                    (case id
                      "t1"
                      [{:status :done
                        :position 1
                        :forms
                        [{:scope "t1/i1/f1" :src "cat(\"a\")" :result {:path "a"}}
                         {:scope "t1/i1/f2" :src "set_session_title(...)" :result "vis_silent"}]}]

                      "t2"
                      [{:status :done
                        :position 1
                        :forms [{:scope "t2/i1/f1" :src "rg({...})" :result {:hits []}}]}]

                      []))]

      (let [out (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})} "t3")]
        (expect (= 2 (count out))) ; both answered turns, not just latest
        (expect (= "Read a" (:user-request (first out))))
        (expect (= [{:scope "t1/i1/f1" :src "cat(\"a\")"}] (:results (first out)))) ; sentinel f2 excluded
        (expect (= [{:scope "t2/i1/f1" :src "rg({...})"}] (:results (second out)))))))
  (it "is deterministic — same DB ⇒ identical output (process-invariant)"
      (with-redefs [persistance/db-list-session-turns
                    (constantly
                      [{:id "t1" :status :done :position 1 :user-request "q" :answer-markdown "a"}])

                    persistance/db-list-session-turn-iterations
                    (constantly [{:status :done
                                  :position 1
                                  :forms [{:scope "t1/i1/f1" :src "cat(x)" :result {:k 1}}]}])]

        (let [env {:session-id "s1" :db-info ::db :ctx-atom (atom {})}]
          (expect (= (previous-turn-context env "t9") (previous-turn-context env "t9"))))))
  (it
    "is summary-aware at ITERATION granularity: session_drop leaves a dropped breadcrumb, session_fold collapses to one gist"
    ;; Folds are recorded at iteration scope (tN/iN) — what the prompt instructs
    ;; and what the live wire (apply-summaries) matches. Each form carries a FORM
    ;; scope (tN/iN/fN); prior-turn-scope-index normalizes form→iteration before
    ;; matching (the path-A fix). :drop? — not gist presence — picks the label. A
    ;; dropped iteration collapses to ONE `dropped` audit line (keeping the why);
    ;; a folded iteration with multiple forms collapses to ONE gist line.
    (with-redefs [persistance/db-list-session-turns
                  (constantly
                    [{:id "t1" :status :done :position 1 :user-request "q" :answer-markdown "a"}])

                  persistance/db-list-session-turn-iterations
                  (constantly [{:status :done
                                :position 1
                                :forms [{:scope "t1/i1/f1" :src "cat(a)" :result {:k 1}} ; iter i1 → dropped
                                        {:scope "t1/i2/f1" :src "cat(b)" :result {:k 2}} ; iter i2 → folded
                                        {:scope "t1/i2/f2" :src "cat(c)" :result {:k 3}}]}])]

      ; (same iter, 2nd form)
      (let [env
            {:session-id "s1"
             :db-info ::db
             :ctx-atom (atom {"session_summaries"
                              [{"scopes" #{"t1/i1"} "drop" true "gist" "wrong file"} ; drop i1
                               {"scopes" #{"t1/i2"} "gist" "b pinned"}]})}

            ; fold i2
            results
            (:results (first (previous-turn-context env "t9")))]

        (expect (= 2 (count results))) ; i1 dropped-line + i2 gist (each deduped)
        (let [by-scope (into {} (map (juxt :scope identity)) results)]
          (expect (= {:scope "t1/i1" :dropped? true :note "wrong file"} (get by-scope "t1/i1")))
          (expect (= {:scope "t1/i2" :gist "b pinned"} (get by-scope "t1/i2")))))))
  (it "returns nil when every prior turn is current/running/blank-answer"
      (with-redefs [persistance/db-list-session-turns
                    (constantly
                      [{:id "t1" :status :done :position 1 :user-request "old" :answer-markdown ""}
                       {:id "t2" :status :running :user-request "now" :answer-markdown "partial"}])

                    persistance/db-list-session-turn-iterations
                    (constantly [])]

        (expect (nil? (previous-turn-context {:session-id "s1" :db-info ::db :ctx-atom (atom {})}
                                             "t2")))))
  (it "carries prior provider-error turns as unfinished cross-turn context"
      (with-redefs [persistance/db-list-session-turns
                    (constantly [{:id "t1"
                                  :status :error
                                  :position 1
                                  :user-request "fix web"
                                  :answer-markdown "## 🚨 PROVIDER_ERROR"}
                                 {:id "t2" :status :running :user-request "continue"}])

                    persistance/db-list-session-turn-iterations
                    (constantly [{:status :done
                                  :position 1
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
                                 [{:id "t1" :position 1} {:id "t2" :position 2}
                                  {:id "t3" :position 3 :status :running}])

                               persistance/db-list-session-turn-iterations
                               (fn [_db-info turn-id]
                                 (case turn-id
                                   "t2"
                                   [{:position 1 :input-tokens 42000}
                                    {:position 2 :input-tokens 51000}]

                                   "t1"
                                   [{:position 1 :input-tokens 10000}]

                                   []))]

                   (expect (= {:last-request-tokens 51000
                               :last-request-turn-id "t2"
                               :last-request-turn-position 2
                               :last-request-iteration 2}
                              (previous-request-usage {:session-id "s1" :db-info ::db} "t3")))))
             (it "returns nil when no prior iteration has input tokens"
                 (with-redefs [persistance/db-list-session-turns
                               (constantly [{:id "t1" :position 1} {:id "t2" :position 2}])

                               persistance/db-list-session-turn-iterations
                               (constantly [{:position 1 :input-tokens 0}])]

                   (expect (nil? (previous-request-usage {:session-id "s1" :db-info ::db} "t2"))))))

(defdescribe stamp-utilization-monotonic-test
             ;; Regression: the stamp used to (dissoc "engine_utilization") on a nil
             ;; measurement, so a transient req=0 (iter-1 seed miss / errored iter)
             ;; BLANKED an already-shown "session_utilization" — the "sometimes works,
             ;; sometimes doesn't" flicker. The stamp must be monotonic.
             (let [stamp
                   (var-get #'lp/stamp-utilization!)

                   util1
                   {"last_request_tokens" 5000 "saturation" 3}

                   util2
                   {"last_request_tokens" 9000 "saturation" 5}]

               (it "stamps a real measurement onto the ctx-atom"
                   (let [ca (atom {})]
                     (stamp ca util1)
                     (expect (= util1 (get @ca "engine_utilization")))))
               (it "NEVER blanks an existing value on a transient nil measurement"
                   (let [ca (atom {"engine_utilization" util1})]
                     (stamp ca nil)
                     (expect (= util1 (get @ca "engine_utilization")))))
               (it "upgrades to a fresh measurement when one arrives"
                   (let [ca (atom {"engine_utilization" util1})]
                     (stamp ca util2)
                     (expect (= util2 (get @ca "engine_utilization")))))
               (it "is a no-op on a nil ctx-atom" (expect (nil? (stamp nil util1))))))

(defdescribe
  session-fold-scope-test
  (let [scope-key
        (var-get #'eng/scope-key)

        expand-through
        (var-get #'eng/expand-through)

        apply-summaries
        (var-get #'lp/apply-summaries)

        prior-scope-index
        (var-get #'lp/prior-turn-scope-index)

        fold-candidates
        (var-get #'lp/fold-candidates)

        over-hint
        (var-get #'lp/over-utilization-hint)]

    (it "scope-key parses iter + form scopes, dropping the form index"
        (expect (= [1 2] (scope-key "t1/i2")))
        (expect (= [1 2] (scope-key "t1/i2/f3")))
        (expect (= [10 20] (scope-key "t10/i20")))
        (expect (nil? (scope-key "garbage"))))
    (it "expand-through resolves a range cursor against the universe (inclusive)"
        (let [out (expand-through [{"through" "t1/i3" "gist" "g"}]
                                  ["t1/i1" "t1/i2" "t1/i3" "t1/i4"])]
          (expect (= #{"t1/i1" "t1/i2" "t1/i3"} (get (first out) "scopes")))
          (expect (nil? (get (first out) "through")))
          (expect (= "g" (get (first out) "gist")))))
    (it "expand-through leaves explicit-scope summaries untouched"
        (let [s [{"scopes" #{"t1/i2"} "gist" "g"}]]
          (expect (= s (expand-through s ["t1/i1" "t1/i2"])))))
    (it "apply-summaries collapses a through-range over the trailer, sparing later steps"
        (let [trailer
              [[0 {:forms-vec [{:scope "t1/i1/f1" :result "a"}]}]
               [1 {:forms-vec [{:scope "t1/i2/f1" :result "b"}]}]
               [2 {:forms-vec [{:scope "t1/i3/f1" :result "c"}]}]]

              out
              (apply-summaries trailer [{"through" "t1/i2" "gist" "early"}])]

          (expect (true? (:collapsed? (second (nth out 0)))))
          (expect (true? (:collapsed? (second (nth out 1)))))
          (expect (nil? (:collapsed? (second (nth out 2)))))))
    (it "prior-turn-scope-index: gist applies via form->iter normalization, ONE deduped entry"
        ;; The path-A regression: a fold recorded at iteration scope (t1/i1) must
        ;; apply to forms carrying FORM scopes (t1/i1/f1, t1/i1/f2) and collapse to
        ;; a SINGLE gist line, not repeat per form.
        (let [forms
              [{:scope "t1/i1/f1" :result "a" :src "(cat \"x\")"}
               {:scope "t1/i1/f2" :result "b" :src "(rg \"y\")"}
               {:scope "t1/i2/f1" :result "c" :src "(ls)"}]

              out
              (prior-scope-index forms [{"scopes" #{"t1/i1"} "gist" "explored"}])]

          (expect (= 1 (count (filter :gist out))))
          (expect (= {:scope "t1/i1" :gist "explored"} (first (filter :gist out))))
          (expect (some #(= "t1/i2/f1" (:scope %)) out))))
    (it
      "prior-turn-scope-index: a dropped iteration collapses to ONE dropped breadcrumb keeping the why"
      (let [forms
            [{:scope "t1/i1/f1" :result "a" :src "(cat)"}
             {:scope "t1/i1/f2" :result "b" :src "(rg)"} ; same dropped iter → still ONE line
             {:scope "t1/i2/f1" :result "c" :src "(ls)"}]

            out
            (prior-scope-index forms [{"scopes" #{"t1/i1"} "drop" true "gist" "misread"}])]

        ;; the iteration's forms collapse to a single audit line, not per-form, not vanished
        (expect (= {:scope "t1/i1" :dropped? true :note "misread"} (first (filter :dropped? out))))
        (expect (= 1 (count (filter :dropped? out))))
        (expect (not-any? #(re-find #"^t1/i1/" (str (:scope %))) out)) ; no raw forms from i1
        (expect (some #(= "t1/i2/f1" (:scope %)) out))))
    (it "fold-candidates ranks heaviest non-folded, excluding most-recent + folded"
        (let [big
              (apply str (repeat 4000 "x"))

              ; ~1000 tok
              trailer
              [[0 {:forms-vec [{:scope "t1/i1/f1" :stdout big :src "(cat)"}]}]
               [1 {:forms-vec [{:scope "t1/i2/f1" :stdout "y" :src "(ls)"}]}]
               [2 {:forms-vec [{:scope "t1/i3/f1" :stdout big :src "(cat)"}]}]]

              ; most-recent
              cands
              (fold-candidates trailer [])]

          (expect (= ["t1/i1" "t1/i2"] (mapv :scope cands))) ; t1/i3 excluded
          (expect (> (:tokens (first cands)) (:tokens (second cands))))
          (expect (= ["t1/i2"] (mapv :scope (fold-candidates trailer [{"scopes" #{"t1/i1"}}]))))))
    (it "over-utilization-hint stays nil under budget, names heaviest steps when firing"
        (let [big
              (apply str (repeat 8000 "x"))

              trailer
              [[0 {:forms-vec [{:scope "t1/i1/f1" :stdout big :src "(cat)"}]}]
               [1 {:forms-vec [{:scope "t1/i2/f1" :stdout "z" :src "(ls)"}]}]]]

          (expect (nil? (over-hint 10 1000 trailer []))) ; under 50% → silent
          (let [hint (over-hint 600 1000 trailer [])]    ; over 50% → fires
            (expect (string? hint))
            (expect (re-find #"session_fold" hint))
            (expect (re-find #"Heaviest live steps" hint))
            (expect (re-find #"t1/i1" hint)))))
    (it "supersede-summaries collapses summary-of-summary (subset dropped, superset/newer wins)"
        (let [supersede (var-get #'eng/supersede-summaries)]
          ;; proper subset is covered by the broader fold → only the superset survives
          (expect (= [{"scopes" #{"t1/i2" "t1/i3" "t1/i4"} "gist" "B"}]
                     (supersede [{"scopes" #{"t1/i2" "t1/i3"} "gist" "A"}
                                 {"scopes" #{"t1/i2" "t1/i3" "t1/i4"} "gist" "B"}])))
          ;; equal sets → the later (newer) gist wins
          (expect (= [{"scopes" #{"t1/i1"} "gist" "new"}]
                     (supersede [{"scopes" #{"t1/i1"} "gist" "old"}
                                 {"scopes" #{"t1/i1"} "gist" "new"}])))
          ;; disjoint and partial-overlap → both kept (coverage differs)
          (expect (= 2
                     (count (supersede [{"scopes" #{"t1/i1"} "gist" "A"}
                                        {"scopes" #{"t1/i2"} "gist" "B"}]))))
          (expect (= 2
                     (count (supersede [{"scopes" #{"t1/i1" "t1/i2"} "gist" "A"}
                                        {"scopes" #{"t1/i2" "t1/i3"} "gist" "B"}]))))))))

(defdescribe
  turn-position-state-test
  (it
    "seeds turn-state with persisted turn position before iteration render"
    (let [seen
          (atom nil)

          env
          {:db-info ::db :session-id "s1" :turn-state-atom (ctx-loop/make-turn-state-atom)}]

      (with-redefs [persistance/db-store-session-turn!
                    (fn [_db opts]
                      (expect (=
                                {:parent-session-id "s1" :user-request "follow up" :status :running}
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
                 (let [e (ex-info "max_tokens hit"
                                  {:type :svar.llm/max-tokens-exceeded
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
                 (let [iter-err
                       {:type :svar.llm/max-tokens-exceeded
                        :data {:reasoning-length 1900 :output-tokens 2048}}

                       ctx
                       (llm-provider-error-context 3 iter-err)]

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
                 (let [iter-err
                       {:data {:type :svar.core/stream-incomplete :reason "max_output_tokens"}}

                       ctx
                       (llm-provider-error-context 2 iter-err)]

                   (expect (= :llm-provider/output-budget-exhausted (:type ctx))))))

(defn- stub-iter
  "Build a synthetic trailer-iters entry for preserved-thinking tests.
   `id` is any unique label for the position; `provider`/`model` control
   how `compatible-preserved-thinking-trailer-iters` filters; the rest
   default to a same-model, replay-eligible canonical thinking block."
  [{:keys [id provider model thinking signature replay?]
    :or {provider :zai-coding-plan model "glm-5.1" replay? true}}]
  [id
   {:assistant-message {:role "assistant"
                        :content [{:type "thinking"
                                   :thinking (or thinking (str "think-" id))
                                   :thinking-signature (or signature (str "sig-" id))}]}
    :llm-provider provider
    :llm-model model
    :preserved-thinking/replay? replay?}])

(defdescribe
  preserved-thinking-replay-test
  (it "returns every compatible assistant message in arrival order"
      ;; Why every message, not just the last: GLM clear_thinking,
      ;; Anthropic HMAC chains, and OpenAI Responses encrypted reasoning
      ;; all require the full assistant chain since the last user turn.
      ;; Returning only the latest step (the pre-fix behaviour) made GLM
      ;; re-derive scratch state each iteration, pinning `cached_tokens`
      ;; across many iterations before the fix.
      (let [target
            {:provider :zai-coding-plan :model "glm-5.1"}

            trailer
            (mapv #(stub-iter {:id %}) [1 2 3])

            compat
            (compatible-preserved-thinking-trailer-iters trailer target)

            replays
            (preserved-thinking-replay-messages compat)]

        (expect (= 3 (count compat)))
        (expect (= 3 (count replays)))
        (expect (= ["think-1" "think-2" "think-3"]
                   (mapv (fn [m]
                           (-> m
                               :content
                               first
                               :thinking))
                         replays)))))
  (it "drops iterations from a different provider/model"
      ;; Cross-provider replay is forbidden: provider-native thinking
      ;; signatures are not portable (z.ai = raw text, Anthropic = HMAC,
      ;; OpenAI Responses = JSON reasoning item). The compatible filter
      ;; must reject mismatches before this fn sees them.
      (let [target
            {:provider :zai-coding-plan :model "glm-5.1"}

            trailer
            [(stub-iter {:id 1}) (stub-iter {:id 2 :provider :anthropic :model "claude-sonnet-4.6"})
             (stub-iter {:id 3})]

            compat
            (compatible-preserved-thinking-trailer-iters trailer target)

            replays
            (preserved-thinking-replay-messages compat)]

        (expect (= 2 (count replays)))
        (expect (= ["think-1" "think-3"]
                   (mapv (fn [m]
                           (-> m
                               :content
                               first
                               :thinking))
                         replays)))))
  (it "drops iterations explicitly flagged :preserved-thinking/replay? false"
      ;; Cross-turn trailer seeds carry the opt-out flag so historical
      ;; iterations stay visible in transcripts but their opaque thinking
      ;; state is not replayed into a new user turn.
      (let [target
            {:provider :zai-coding-plan :model "glm-5.1"}

            trailer
            [(stub-iter {:id 1 :replay? false}) (stub-iter {:id 2 :replay? true})]

            compat
            (compatible-preserved-thinking-trailer-iters trailer target)

            replays
            (preserved-thinking-replay-messages compat)]

        (expect (= 1 (count replays)))
        (expect (= ["think-2"]
                   (mapv (fn [m]
                           (-> m
                               :content
                               first
                               :thinking))
                         replays)))))
  (it
    "returns empty when no iteration has an :assistant-message"
    ;; Iterations that errored before the model produced a usable
    ;; assistant turn (e.g. provider HTTP 4xx mid-stream) lack
    ;; `:assistant-message`; the compatible filter drops them so the
    ;; replay never tries to send an empty/partial block.
    (let [target
          {:provider :zai-coding-plan :model "glm-5.1"}

          trailer
          [[1
            {:llm-provider :zai-coding-plan :llm-model "glm-5.1" :preserved-thinking/replay? true}]]

          compat
          (compatible-preserved-thinking-trailer-iters trailer target)

          replays
          (preserved-thinking-replay-messages compat)]

      (expect (zero? (count compat)))
      (expect (zero? (count replays))))))

(defn- stub-tool-iter
  "Trailer entry for conversation-suffix tests: one tool call with its
   result, an assistant message carrying thinking + the tool_use."
  [{:keys [id provider model replay? content attachments]
    :or {provider :lmstudio model "google/gemma-4-12b-qat" replay? true}}]
  [id
   {:assistant-message
    {:role "assistant"
     :content
     (or content
         [{:type "thinking" :thinking (str "think-" id) :thinking-signature (str "sig-" id)}
          {:type "tool_use" :id (str "tc-" id) :name "find_files" :input {"query" "lmstudio"}}])}
    :llm-provider provider
    :llm-model model
    :preserved-thinking/replay? replay?
    :attachments attachments
    :tool-calls [{:id (str "tc-" id) :name "find_files" :input {"query" "lmstudio"}}]
    :forms-vec [{:scope (str "t1/i" id)
                 :svar/tool-call-id (str "tc-" id)
                 :result {"item_count" 2 "paths" ["a.clj" "b.clj"]}}]}])

(defdescribe conversation-suffix-mismatch-test
             ;; The session-c4b630c7 regression: the health gate demoted lmstudio so the
             ;; SELECTED model (target) was anthropic/opus while the ACTUAL server was
             ;; lmstudio/gemma. The old suffix dropped the whole [assistant, tool_result]
             ;; pair on that mismatch — the model never saw its own find_files result and
             ;; re-issued the identical call every iteration.
             (it "replays [assistant sans thinking, tool_result] on provider/model mismatch"
                 (let [target
                       {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

                       suffix
                       (conversation-suffix [(stub-tool-iter {:id 1})] target)]

                   (expect (= 2 (count suffix)))
                   (let [[assistant results]
                         suffix

                         types
                         (mapv :type (:content assistant))]

                     (expect (= "assistant" (:role assistant)))
                     ;; thinking stripped, tool_use kept — the tool_result stays answerable
                     (expect (= ["tool_use"] types))
                     (expect (= "user" (:role results)))
                     (expect (= "tc-1"
                                (-> results
                                    :content
                                    first
                                    :tool_use_id)))
                     (expect (string? (-> results
                                          :content
                                          first
                                          :content)))
                     (expect (str/includes? (-> results
                                                :content
                                                first
                                                :content)
                                            "item_count")))))
             (it "replays thinking verbatim when provider+model match the target"
                 (let [target
                       {:provider :lmstudio :model "google/gemma-4-12b-qat"}

                       suffix
                       (conversation-suffix [(stub-tool-iter {:id 1})] target)]

                   (expect (= 2 (count suffix)))
                   (expect (= ["thinking" "tool_use"] (mapv :type (:content (first suffix)))))))
             (it "degrades to a plain-text results message when only thinking remains"
                 ;; No tool_use survives the strip → a tool_result would be orphaned
                 ;; (wire error on Anthropic), so the outputs ride as plain text.
                 (let [target
                       {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

                       entry
                       (stub-tool-iter {:id 1
                                        :content [{:type "thinking"
                                                   :thinking "only-thinking"
                                                   :thinking-signature "sig"}]})

                       suffix
                       (conversation-suffix [entry] target)]

                   (expect (= 1 (count suffix)))
                   (let [[results] suffix]
                     (expect (= "user" (:role results)))
                     (expect (string? (:content results)))
                     (expect (str/includes? (:content results) "item_count")))))
             (it "still excludes cross-turn seeds entirely"
                 (let [target
                       {:provider :anthropic-coding-plan :model "claude-opus-4-8"}

                       suffix
                       (conversation-suffix [(stub-tool-iter {:id 1 :replay? false})] target)]

                   (expect (empty? suffix)))))

(defdescribe
  conversation-suffix-image-replay-test
  ;; Generated figures (matplotlib plt.show()) an iteration's tool call
  ;; produced are persisted as :attachments and replayed to the model as
  ;; their OWN vision user message AFTER the <results> — but ONLY when the
  ;; target model advertises :vision.
  (let [att
        {:tool-call-id "tc-1" :media-type "image/png" :base64 "QUJD" :filename "plot.png" :size 3}]
    (it "appends a vision user message with the image AFTER results"
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})] target)]

          ;; [assistant-replay, <results>, image-user] — image is LAST so it
          ;; never sits between a tool_use and its tool_result.
          (expect (= 3 (count suffix)))
          (let [img (last suffix)]
            (expect (= "user" (:role img)))
            (expect (= ["image_url"] (mapv :type (:content img))))
            (expect (= "data:image/png;base64,QUJD"
                       (-> img
                           :content
                           first
                           :image_url
                           :url))))))
    (it "omits the image entirely for a text-only (non-vision) target"
        (let [target {:provider :zai-coding-plan :model "glm-5-turbo"}
              suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att]})] target)]

          ;; back to the plain [assistant, results] pair, no image block
          (expect (= 2 (count suffix)))
          (expect (not-any? (fn [m]
                              (and (vector? (:content m))
                                   (some #(= "image_url" (:type %)) (:content m))))
                            suffix))))
    (it "skips a non-image vis_attach artifact — a csv never rides as an image block"
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              csv {:tool-call-id "tc-2"
                   :media-type "text/csv"
                   :base64 "YSxi"
                   :filename "data.csv"
                   :size 3
                   :kind "file"}
              suffix (conversation-suffix [(stub-tool-iter {:id 1 :attachments [att csv]})] target)
              img (last suffix)]

          ;; the image still replays, but ONLY it — the csv artifact is
          ;; DB/display-only, never a broken data:text/csv image block.
          (expect (= 3 (count suffix)))
          (expect (= "user" (:role img)))
          (expect (= ["image_url"] (mapv :type (:content img))))
          (expect (= "data:image/png;base64,QUJD"
                     (-> img
                         :content
                         first
                         :image_url
                         :url)))))
    (it "drops the image when session_fold collapsed the iteration"
        ;; The invariant: a figure's vision visibility TRACKS its iteration's
        ;; textual visibility. Once session_fold/session_drop collapses the
        ;; step, its image bytes leave the wire with it — never re-billed.
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              [pos rec] (stub-tool-iter {:id 1 :attachments [att]})
              suffix (conversation-suffix [[pos (assoc rec :collapsed? true)]] target)]

          (expect (not-any? (fn [m]
                              (and (vector? (:content m))
                                   (some #(= "image_url" (:type %)) (:content m))))
                            suffix))))
    (it "drops a folded cross-turn seed's image — collapse wins over the seed branch"
        ;; The leak this guards: a prior-turn figure carried as a seed
        ;; (:preserved-thinking/replay? false) used to be byte-immune to
        ;; compaction because the seed branch ran BEFORE the collapse check.
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              [pos rec] (stub-tool-iter {:id 1 :replay? false :attachments [att]})
              suffix (conversation-suffix [[pos (assoc rec :collapsed? true)]] target)]

          (expect (not-any? (fn [m]
                              (and (vector? (:content m))
                                   (some #(= "image_url" (:type %)) (:content m))))
                            suffix))))
    (it "still rides a NON-folded cross-turn seed's image to a vision target"
        ;; The reorder must not break the one path that legitimately emits a
        ;; seed's image: its bytes were never wired to any prior turn.
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              suffix (conversation-suffix [(stub-tool-iter
                                             {:id 1 :replay? false :attachments [att]})]
                                          target)]

          (expect (= 1 (count suffix)))
          (expect (= ["image_url"] (mapv :type (:content (first suffix)))))))
    (it "emits no image message when an iteration produced no attachments"
        (let [target {:provider :anthropic-coding-plan :model "claude-opus-4-8"}
              suffix (conversation-suffix [(stub-tool-iter {:id 1})] target)]

          (expect (= 2 (count suffix)))))))

;; multi-fence-hint / attach-multi-fence-hint / empty-code-error-with-observation
;; tests removed: those fns were deleted with the fenced-era machinery (lenient
;; mode yields <=1 block, so multi-fence merge + fence-dropped diagnostics are
;; unreachable). See refactor "remove dead fenced-era code-block machinery".

(defdescribe token-usage-normalization-test
             (it "preserves Anthropic cache write tokens from svar token maps"
                 (expect (= {:prompt_tokens 112
                             :completion_tokens 69
                             :completion_tokens_details {:reasoning_tokens 0}
                             :prompt_tokens_details {:cached_tokens 0 :cache_creation_tokens 8777}}
                            (ask-result->api-usage
                              {:tokens {:input 112 :output 69 :cached 0 :cache-created 8777}})))))

(defdescribe ask-code-block-observation-test
             (it "reports the block count (lenient mode: only the count is meaningful)"
                 (expect (= {:form-count 1}
                            (ask-code-block-observation {:blocks [{:source "(def x 1)"
                                                                   :lang "clojure"}]})))
                 (expect (= {:form-count 0} (ask-code-block-observation {:blocks []})))
                 (expect (= {:form-count 0} (ask-code-block-observation {})))))

(defdescribe
  iteration-start-hook-test
  (it
    "collects active :turn.iteration/start hooks as hook-task descriptors (D12)"
    (let [seen
          (atom nil)

          ext
          {:ext/name "test.hooks"
           :ext/hooks [{:id :test/title
                        :doc "title"
                        :phase :turn.iteration/start
                        :fn (fn [ctx]
                              (reset! seen ctx)
                              {:title "set title" :importance :warn})}
                       {:id :test/answer
                        :doc "answer"
                        :phase :turn.answer/validate
                        :fn (fn [_]
                              {:reject true})}
                       {:id :test/no-title
                        :doc "missing title—rejected"
                        :phase :turn.iteration/start
                        :fn (fn [_]
                              {:importance :warn})}]}

          ctx
          {:session-title nil :title-refresh? true :turn-position 1}

          hits
          (collect-iteration-start-hints {} [ext] ctx)]

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
          (with-redefs [svar/ask! (fn [& _]
                                    (throw (ex-info "must not re-title" {})))]
            (expect (nil? (maybe-auto-title! env "some unrelated new request")))
            (expect (= "Old focus" @(:session-title-atom env))))
          (finally (lp/dispose-environment! env)))))
  (it "auto-title treats Untitled placeholders as missing previous titles"
      (let [seen
            (atom nil)

            router-stub
            {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}]}

            env
            (lp/create-environment router-stub {:db :memory :title "Untitled"})]

        (try (with-redefs [svar/ask! (fn [_router opts]
                                       (reset! seen opts)
                                       {:result {:title "Current Bug Triage"}})]
               (let [f (maybe-auto-title! env "Wez to sprawdz")]
                 @f
                 (expect (= "Current Bug Triage" @(:session-title-atom env)))
                 (expect (str/includes? (-> @seen
                                            :messages
                                            second
                                            :content)
                                        "Previous title: <none>"))))
             (finally (lp/dispose-environment! env)))))
  (it
    "auto-title declares the preferred plan order, then deterministic fallback when the chain fails"
    (let [router-stub
          {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}
                       {:id :openai-codex :models [{:name "gpt-5.3-codex"}]}]}

          seen
          (atom nil)

          env
          (lp/create-environment router-stub {:db :memory :title "Untitled"})]

      (try
        ;; svar owns the per-provider walk now; the host makes ONE call that
        ;; declares `:prefer-providers`. A thrown call → deterministic fallback.
        (with-redefs [svar/ask! (fn [_router opts]
                                  (reset! seen opts)
                                  (throw (ex-info "Exceptional status code: 400" {})))]
          (let [f (maybe-auto-title!
                    env
                    "1dff1f5a-76dc-431e-ad2b-97af14c731f1 can you check why TUI title is missing?")]
            @f
            (expect (= [:zai-coding-plan :openai-codex]
                       (take 2 (get-in @seen [:routing :prefer-providers]))))
            (expect (= "can you check why TUI title is" @(:session-title-atom env)))))
        (finally (lp/dispose-environment! env)))))
  (it "set_session_title is NOT a tool — the title is host-generated"
      (let [env (lp/create-environment ::router {:db :memory})]
        (try
          ;; The model has no `set_session_title` binding; calling it raises
          ;; (NameError) and surfaces as a structured eval error.
          (let [bad (env/run-python-block (:python-context env)
                                          "set_session_title(\"Liveness check\")")]
            (expect (some? (:error bad))))
          (finally (lp/dispose-environment! env))))))

(defdescribe
  provider-error-explanation-test
  (it
    "diagnoses auth failures; the re-authenticate step is a SEPARATE next-step block"
    (let
      [err
       {:message
        "API authentication failed. Check your API key. (Original: Exceptional status code: 401)"
        :data
        {:status 401
         :body
         "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"Invalid authentication credentials\"}}"}}

       text
       (provider-error-explanation err)

       step
       (perr/provider-error-next-step err)]

      ;; explanation = diagnosis only
      (expect (str/includes? text "provider rejected credentials"))
      (expect (str/includes? text "Provider message: Invalid authentication credentials"))
      ;; the actionable step lives in provider-error-next-step now
      (expect (str/includes? step "NEXT STEP: re-authenticate this provider or update its API key"))
      (expect (str/includes? step "Ctrl+K -> Model / Providers"))
      (expect (str/includes? step "vis providers auth")))))

(defdescribe
  ask-code-idle-timeout-test
  (it "uses a sixty-second TTFT timeout and three-minute idle timeout by default"
      (expect (= (* 60 1000) rt/ASK_CODE_TTFT_TIMEOUT_MS))
      (expect (= (* 3 60 1000) rt/ASK_CODE_IDLE_TIMEOUT_MS))
      (let [{:keys [router opts]} (captured-ask-code-opts {:lang "clojure" :messages []})]
        (expect (= ::router router))
        (expect (= rt/ASK_CODE_TTFT_TIMEOUT_MS (:ttft-timeout-ms opts)))
        (expect (= rt/ASK_CODE_IDLE_TIMEOUT_MS (:idle-timeout-ms opts)))
        ;; Semantic timeout is now auto-added by `with-default-ask-code-idle-timeout`
        ;; (default 185s, catches transport-alive-but-model-silent stalls).
        (expect (= rt/ASK_CODE_SEMANTIC_TIMEOUT_MS (:semantic-timeout-ms opts)))))
  (it "preserves explicit ask-code TTFT and idle timeout overrides"
      (expect (= 77 (:ttft-timeout-ms (:opts (captured-ask-code-opts {:ttft-timeout-ms 77})))))
      (expect (contains? (:opts (captured-ask-code-opts {:ttft-timeout-ms nil})) :ttft-timeout-ms))
      (expect (nil? (:ttft-timeout-ms (:opts (captured-ask-code-opts {:ttft-timeout-ms nil})))))
      (expect (= 42 (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms 42})))))
      (expect (contains? (:opts (captured-ask-code-opts {:idle-timeout-ms nil})) :idle-timeout-ms))
      (expect (nil? (:idle-timeout-ms (:opts (captured-ask-code-opts {:idle-timeout-ms nil}))))))
  (it "uses a 185-second semantic timeout by default and accepts overrides"
      ;; Codex/Claude over Copilot can sit silent for minutes while the
      ;; model reasons server-side; idle-timeout-ms keeps resetting on
      ;; SSE pings. The semantic watchdog surfaces \"transport alive but
      ;; no model events\" inside 185 seconds (a transport-alive stream with
      ;; zero events could otherwise stall for many minutes).
      (expect (= 185000 rt/ASK_CODE_SEMANTIC_TIMEOUT_MS))
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
                   (try (let [result ((var-get #'lp/execute-code) env "x = 1")]
                          (expect (nil? (:error result))))
                        ;; Sandbox globals persist REPL-style across evals on the same context.
                        (let [read-back (env/run-python-block (:python-context env) "x")]
                          (expect (nil? (:error read-back)))
                          (expect (= 1 (:result read-back))))
                        (finally (lp/dispose-environment! env)))))
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
                 (let [{:keys [python-context]}
                       (env/create-python-context {})

                       ;; emoji on the first AND a later line; the second form re-reads the var.
                       code
                       (str "msg = \"\"\"# Heading 👆\n\n- bin/ 🚀\n\nPełne ł ó ż 🌳\"\"\"\n" "msg")

                       {:keys [error result]}
                       (env/run-python-block python-context code)]

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
                                 [{:id 0 :code "1 + 2" :result 3 :error nil}]
                                 {:answer "done"}
                                 nil)))))

;; ---------------------------------------------------------------------------
;; def-sink -> vars-snapshot (per-var precise source extraction)
;; ---------------------------------------------------------------------------

(defdescribe
  gather-builtin-test
  "maki-style in-program concurrency: `await gather(*awaitables)` runs each
   awaitable on a virtual thread and returns results IN ORDER. Guards the async
   runtime end-to-end through a real sandbox: the await path AST-wraps + drives
   the coroutine, gather dispatches awaitables to __vis_par__ (the host
   virtual-thread pool). Concurrency itself is proven by GraalPy lock-release."
  (it
    "awaits gathered coroutines and returns their results in order"
    (let [environment (lp/create-environment ::router {:db :memory})]
      (try
        (let
          [r
           (env/run-python-block
             (:python-context environment)
             "async def work(n):\n    return n * n\nvals = await gather(work(2), work(3), work(4))\nprint(list(vals))"
             "t1/i1")]
          (expect (nil? (:error r)))
          (expect (= "[4, 9, 16]" (clojure.string/trim (str (:stdout r))))))
        (finally (try (lp/dispose-environment! environment) (catch Throwable _ nil))))))
  (it
    "a failing member surfaces the RIGHT slot + its OWN error (never mis-attributed)"
    ;; gather is all-or-nothing, but the error must name the EXACT failing index
    ;; and carry that call's real message — not a sibling's, not a generic one.
    ;; (Independent of the `__vis_par_isolated__` batch path, which never routes
    ;; through gather — this guards `gather`/`__vis_par__` behavior verbatim.)
    (let [environment (lp/create-environment ::router {:db :memory})]
      (try
        (let
          [r
           (env/run-python-block
             (:python-context environment)
             "async def ok(n):\n    return n\nasync def boom():\n    raise ValueError('DISTINCT_BOOM_42')\nawait gather(ok(1), boom(), ok(3))"
             "t1/i1")
           msg (str (:message (:error r)))]

          (expect (some? (:error r)))
          (expect (clojure.string/includes? msg "[1]"))               ;; the failing slot, not [0]/[2]
          (expect (clojure.string/includes? msg "DISTINCT_BOOM_42"))) ;; boom's own message
        (finally (try (lp/dispose-environment! environment) (catch Throwable _ nil)))))))

(defdescribe
  native-tools-results-e2e-test
  "END-TO-END over the REAL loop path: a real `create-environment` (real embedded
   GraalPy sandbox + real in-memory SQLite), a native tool result PERSISTED via the
   same `db-store-iteration!` the loop uses, then a `python_execution` block that
   reads `ntr[<id>]` and gets back the SAME dict — no re-fetch."
  (let [store-native! (fn [env id result]
                        ;; Persist one iteration whose form is a native tool result
                        ;; keyed by `id` — exactly the shape the loop writes.
                        (let [tid (persistance/db-store-session-turn!
                                    (:db-info env)
                                    {:parent-session-id (:session-id env) :user-request "e2e"})]
                          (persistance/db-store-iteration! (:db-info env)
                                                           {:session-turn-id tid
                                                            :code "cat(\"x\")"
                                                            :forms [{:scope "t1/i1"
                                                                     :tag :observation
                                                                     :src "cat(\"x\")"
                                                                     :svar/tool-call-id id
                                                                     :vis/tool-name "cat"
                                                                     :result result}]})))]
    (it "a later python_execution retrieves a PERSISTED native result by its tool_use id"
        (let [env (lp/create-environment ::router {:db :memory})]
          (try (store-native! env "toolu_E2E" {"op" "cat" "path" "x.py" "text" "hello world"})
               (let [r (env/run-python-block
                         (:python-context env)
                         (str "res = ntr[\"toolu_E2E\"]\n"
                              "print(res[\"text\"], res[\"path\"], type(res).__name__)")
                         "t2/i1")]
                 (expect (nil? (:error r)))
                 ;; SAME value the native call returned, rehydrated to the __VisResult__
                 ;; dict shape the model works with.
                 (expect (= "hello world x.py __VisResult__" (str/trim (str (:stdout r))))))
               (finally (try (lp/dispose-environment! env) (catch Throwable _ nil))))))
    (it "an unknown id raises a clean KeyError through the normal op-error path (no crash)"
        (let [env (lp/create-environment ::router {:db :memory})]
          (try (let [r (env/run-python-block (:python-context env)
                                             "print(ntr[\"toolu_DOES_NOT_EXIST\"])"
                                             "t2/i1")]
                 (expect (some? (:error r)))
                 (expect (= :python/runtime (:phase (:data (:error r)))))
                 (expect (str/includes? (str (:message (:error r))) "no native tool result")))
               (finally (try (lp/dispose-environment! env) (catch Throwable _ nil))))))
    (it "retrieval does NOT re-run the tool (payoff): the native fn is never called"
        ;; The retrieval reads from the STORE, not the tool. We assert the sandbox
        ;; never invokes a `cat` binding during retrieval by binding a `cat` that
        ;; would flip a flag if called — it must stay unflipped.
        (let [env (lp/create-environment ::router {:db :memory})
              called (atom false)]

          (try (store-native! env "toolu_NOFETCH" {"op" "cat" "text" "cached body"})
               ;; Shadow-proof: install a probe `cat` in the sandbox; retrieval must
               ;; NOT call it (ntr goes straight to the store).
               (env/bind-and-bump! env
                                   'cat
                                   (fn [& _]
                                     (reset! called true)
                                     {"op" "cat" "text" "REFETCHED"}))
               (let [r (env/run-python-block (:python-context env)
                                             "print(ntr[\"toolu_NOFETCH\"][\"text\"])"
                                             "t2/i1")]
                 (expect (nil? (:error r)))
                 (expect (= "cached body" (str/trim (str (:stdout r)))))
                 (expect (false? @called))) ;; the tool was NOT re-run
               (finally (try (lp/dispose-environment! env) (catch Throwable _ nil))))))
    (it "iteration DISCOVERS the store: keys/items/values/len/iter over every persisted id"
        ;; The issue: the proxy only exposed get(). Now it's a read-only mapping —
        ;; the sandbox can browse ids it never had up front.
        (let [env (lp/create-environment ::router {:db :memory})]
          (try (store-native! env "toolu_ONE" {"op" "cat" "text" "one"})
               (store-native! env "toolu_TWO" {"op" "rg" "text" "two"})
               (let [r (env/run-python-block
                         (:python-context env)
                         (str "ks = sorted(ntr.keys())\n"
                              "items = dict(ntr.items())\n"
                              "ops = sorted(v[\"op\"] for v in ntr.values())\n"
                              "print(len(ntr), ks,\n" "      items[\"toolu_ONE\"][\"text\"], ops,\n"
                              "      sorted(list(ntr)) == ks,\n" "      (\"toolu_ONE\" in ntr))")
                         "t3/i1")]
                 (expect (nil? (:error r)))
                 (expect (= "2 ['toolu_ONE', 'toolu_TWO'] one ['cat', 'rg'] True True"
                            (str/trim (str (:stdout r))))))
               (finally (try (lp/dispose-environment! env) (catch Throwable _ nil))))))))

(defdescribe
  iteration-summarize-test
  "summarize/drop operate at ITERATION (tN/iN) granularity: a summarized step
   collapses entirely (its assistant+tool_result pair leaves the wire) to one
   gist line; a non-collapsed step renders as a tool_result tagged `# tN/iN`."
  (let [apply-summaries
        (var-get #'lp/apply-summaries)

        irm
        (var-get #'lp/iteration-results-message)]

    (it "summarize([tN/iN]) tags the iteration :collapsed? and swaps it for the gist"
        (let [tis
              [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big output"}]}]
               [2 {:forms-vec [{:scope "t1/i2/f1" :stdout "keep me"}]}]]

              out
              (apply-summaries tis [{"scopes" #{"t1/i1"} "gist" "did the thing"}])

              r1
              (second (first out))

              r2
              (second (second out))]

          (expect (true? (:collapsed? r1)))
          (expect (nil? (:collapsed? r2)))
          ;; collapsed → plain-text gist line (NOT a tool_result)
          (expect (= "# ⋯ folded t1/i1 · did the thing" (:content (irm r1))))))
    (it "session_drop collapses to a `⋯ dropped <scopes> · <why>` line (reason kept)"
        (let [out (apply-summaries [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "big"}]}]]
                                   [{"scopes" #{"t1/i1"} "drop" true "gist" "misread"}])]
          (expect (= "# ⋯ dropped t1/i1 · misread" (:content (irm (second (first out))))))))
    (it "a live step renders as a tool_result tagged with its # tN/iN handle"
        (let [m (irm {:forms-vec [{:scope "t1/i1/f1" :stdout "hello"}] :tool-calls [{:id "c1"}]})]
          (expect (= "c1" (get-in m [:content 0 :tool_use_id])))
          (expect (str/includes? (get-in m [:content 0 :content]) "# t1/i1"))
          (expect (str/includes? (get-in m [:content 0 :content]) "hello"))))
    (it "no summaries ⇒ trailer-iters unchanged"
        (let [tis [[1 {:forms-vec [{:scope "t1/i1/f1" :stdout "x"}]}]]]
          (expect (= tis (apply-summaries tis [])))))))

(defdescribe
  native-tool-result-pairing-test
  "REARCHITECTURE (same DB schema): an iteration is a LIST of tool-calls, one of
   which may be `python_execution`. Each tool_use gets its OWN tool_result,
   carrying ITS OWN forms' return — forms are grouped by `:svar/tool-call-id`. A
   DIRECT file tool's return is its value; python_execution's return IS the text
   it print()s. No more 'first call carries everything, the rest get a stub'."
  (let [irm
        (var-get #'lp/iteration-results-message)

        pre
        (var-get #'lp/code-entries-preflight)]

    (it "each parallel tool_use is answered by its OWN result"
        (let [m
              (irm {:tool-calls [{:id "A" :name "cat"} {:id "B" :name "rg"}
                                 {:id "P" :name "python_execution"}]
                    ;; native cat/rg carry :result (their value); python_execution
                    ;; carries :stdout (what it printed) — the engine emits ONE
                    ;; channel per call, never both.
                    :forms-vec
                    [{:scope "t1/i1/f1" :svar/tool-call-id "A" :result "AAA"}
                     {:scope "t1/i1/f2" :svar/tool-call-id "B" :result "BBB"}
                     {:scope "t1/i1/f3" :svar/tool-call-id "P" :result nil :stdout "PPP"}]})

              by-id
              (into {} (map (juxt :tool_use_id :content)) (:content m))]

          (expect (= 3 (count (:content m))))
          ;; native cat/rg → their RETURN value; not cross-contaminated
          (expect (str/includes? (by-id "A") "AAA"))
          (expect (not (str/includes? (by-id "A") "BBB")))
          (expect (str/includes? (by-id "B") "BBB"))
          ;; python_execution → its PRINTED string
          (expect (str/includes? (by-id "P") "PPP"))))
    (it "a NATIVE result tool_result carries its ntr[id] handle; python_execution does NOT"
        ;; MODEL-SEES-IT: each native call's result is retrievable later via
        ;; `ntr[<tool_use_id>]`; the id vis stored can differ from the wire id the
        ;; model saw (OpenAI Responses composite), so we surface the EXACT key inline.
        ;; python_execution stores no return → no handle.
        (let [m
              (irm {:tool-calls [{:id "toolu_A" :name "cat"} {:id "call_1|fc_9" :name "rg"}
                                 {:id "P" :name "python_execution"}]
                    :forms-vec
                    [{:scope "t1/i1/f1" :svar/tool-call-id "toolu_A" :result "AAA"}
                     {:scope "t1/i1/f2" :svar/tool-call-id "call_1|fc_9" :result "BBB"}
                     {:scope "t1/i1/f3" :svar/tool-call-id "P" :result nil :stdout "PPP"}]})

              by-id
              (into {} (map (juxt :tool_use_id :content)) (:content m))]

          ;; native results advertise the literal retrieval key (composite included)
          (expect (str/includes? (by-id "toolu_A") "ntr[\"toolu_A\"]"))
          (expect (str/includes? (by-id "call_1|fc_9") "ntr[\"call_1|fc_9\"]"))
          ;; python_execution printed — no stored return, so no handle offered
          (expect (not (str/includes? (by-id "P") "ntr")))))
    (it "an ERRORED native call gets no result handle (nothing was stored)"
        (let [m
              (irm {:tool-calls [{:id "bad" :name "cat"}]
                    :forms-vec
                    [{:scope "t1/i1/f1" :svar/tool-call-id "bad" :error "No such file"}]})

              c
              (get-in m [:content 0 :content])]

          (expect (not (str/includes? c "ntr")))))
    (it "a FAILED call's tool_result is flagged :is_error true; a successful one is not"
        (let [m
              (irm {:tool-calls [{:id "ok" :name "cat"} {:id "bad" :name "cat"}]
                    :forms-vec
                    [{:scope "t1/i1/f1" :svar/tool-call-id "ok" :result "FILE"}
                     {:scope "t1/i1/f2" :svar/tool-call-id "bad" :error "No such file"}]})

              by-id
              (into {} (map (juxt :tool_use_id identity)) (:content m))]

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
        (let [m
              (irm {:tool-calls [{:id "A" :name "cat"}]
                    :forms-vec [{:scope "t1/i1/f1" :svar/tool-call-id "A" :result "body"}
                                {:summary? true :summary-iters ["t1/i0"] :summary-gist "ctx"}]})

              c
              (get-in m [:content 0 :content])]

          (expect (str/includes? c "body"))
          (expect (str/includes? c "folded t1/i0 · ctx"))))
    (it "code-entries-preflight keeps distinct native tool-calls SEPARATE (no merge)"
        (let [entries (:code-entries (pre 1
                                          [{:lang "python"
                                            :source "cat(\"a\")"
                                            :svar/tool-call-id "A"
                                            :vis/tool-name "cat"}
                                           {:lang "python"
                                            :source "rg({\"any\":[\"x\"]})"
                                            :svar/tool-call-id "B"
                                            :vis/tool-name "rg"}]))]
          (expect (= 2 (count entries)))
          (expect (= ["A" "B"] (mapv :svar/tool-call-id entries)))))
    (it "code-entries-preflight STILL merges legacy id-less blocks (provider stutter)"
        (let [entries (:code-entries (pre 1
                                          [{:lang "python" :source "x = 1"}
                                           {:lang "python" :source "y = 2"}]))]
          (expect (= 1 (count entries)))))))

(defdescribe
  repetition-loop-detection-test
  "Repetition-only loop detector + decision-checkpoint. No iteration/budget
   counting — fires solely on identical action code repeated across iterations."
  (let [detect
        (var-get #'lp/repetition-loop-state)

        msg
        (var-get #'lp/loop-checkpoint-message)]

    (describe "repetition-loop-state"
              (it "is not stuck on a single iteration"
                  (let [r (detect [{:code "rg({\"any\": [\"x\"]})"}] nil)]
                    (expect (false? (:stuck? r)))))
              (it "trips when identical action code repeats across iterations"
                  (let [blocks
                        [{:code "rg({\"any\": [\"cancel\"]})"} {:code "cat(\"x.clj\")"}]

                        r1
                        (detect blocks nil)

                        r2
                        (detect blocks {:last-sig (:action-sig r1)})]

                    (expect (false? (:stuck? r1)))
                    (expect (true? (:stuck? r2)))))
              (it "does not trip on distinct action code across iterations"
                  (let [r1
                        (detect [{:code "rg({\"any\": [\"a\"]})"}] nil)

                        r2
                        (detect [{:code "rg({\"any\": [\"b\"]})"}] {:last-sig (:action-sig r1)})]

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

(defdescribe
  forced-loop-termination-test
  "STERN PATH (integration): a model that emits the SAME non-(done) action every
   iteration trips the repetition detector → decision-checkpoint → force-finalize,
   so the turn TERMINATES with a (give-up) answer instead of looping forever.
   Pre-fix this would loop until cancel; the safety cap below turns a regression
   into a loud failure instead of a hang."
  (it "force-finalizes a repeating turn instead of looping forever"
      (let [router-stub
            {:providers [{:id :zai-coding-plan :models [{:name "glm-5-turbo"}]}]}

            env
            (lp/create-environment router-stub {:db :memory})

            calls
            (atom 0)]

        (try (with-redefs [svar/ask-code!
                           (fn [_ _]
                             (when (> (swap! calls inc) 12)
                               (throw (ex-info "force-finalize never fired — looped >12x" {})))
                             ;; identical non-(done) action every iteration → action-sig repeats
                             {:blocks [{:lang "clojure" :source "(def probe 1)"}]
                              :raw "```clojure\n(def probe 1)\n```"
                              :tokens {}})]
               (let [result (lp/turn! env [(svar/user "go in circles")] {})]
                 (expect (some? result))
                 ;; terminated via force-finalize, not a hang / not blank
                 (expect (not (str/blank? (str (lp/answer-markdown (:answer result))))))
                 ;; converged fast: iter1 (seed) → iter2 (stuck+checkpoint) → iter3 (force)
                 (expect (<= @calls 5))))
             (finally (lp/dispose-environment! env))))))

(defdescribe
  honor-config-primary-test
  (describe
    "honor-config-primary! — the USER's first-configured model wins as primary"
    ;; Bug: svar's make-router PREPENDS a provider's catalog :default-models, so
    ;; (first :models) — the effective model — was always svar's default, never
    ;; the model the user put first. The user reorders to primary, nothing happens.
    (let [f (var-get #'lp/honor-config-primary!)]
      (it
        "floats the user's config-first model to the front (+ :root), keeping the rest as fallbacks"
        (let [;; svar prepended opus-4-8; user actually configured fable-5 first
              router {:providers [{:id :anthropic-coding-plan
                                   :models [{:name "claude-opus-4-8"} {:name "claude-fable-5"}]}]}
              config {:providers [{:id :anthropic-coding-plan
                                   :models [{:name "claude-fable-5"} {:name "claude-opus-4-8"}]}]}
              p (first (:providers (f router config)))]

          (expect (= ["claude-fable-5" "claude-opus-4-8"] (mapv :name (:models p))))
          (expect (= "claude-fable-5" (:root p)))
          (expect (= "claude-fable-5" (:name (lp/resolve-effective-model (f router config)))))))
      (it "accepts a string model in config (model-name coercion)"
          (let [router {:providers [{:id :anthropic-coding-plan
                                     :models [{:name "claude-opus-4-8"}
                                              {:name "claude-sonnet-4-6"}]}]}
                config {:providers [{:id :anthropic-coding-plan :models ["claude-sonnet-4-6"]}]}]

            (expect (= "claude-sonnet-4-6"
                       (:name (lp/resolve-effective-model (f router config)))))))
      (it
        "no-op when the config-first model isn't in the built provider (never empties it)"
        (let [router {:providers [{:id :anthropic-coding-plan :models [{:name "claude-opus-4-8"}]}]}
              config {:providers [{:id :anthropic-coding-plan :models [{:name "totally-unknown"}]}]}
              p (first (:providers (f router config)))]

          (expect (= ["claude-opus-4-8"] (mapv :name (:models p))))))
      (it "leaves a provider the user didn't configure untouched"
          (let [router {:providers [{:id :zai-coding-plan :models [{:name "glm-4"}]}]}
                config {:providers [{:id :anthropic-coding-plan
                                     :models [{:name "claude-fable-5"}]}]}]

            (expect (= [{:name "glm-4"}] (:models (first (:providers (f router config)))))))))))

(defdescribe
  router-for-model-test
  (describe
    "router-for-model — a coordinator PROPOSES a child model"
    (let [router {:providers [{:id :anthropic-coding-plan :models [{:name "claude-opus-4-8"}]}
                              {:id :anthropic
                               :models [{:name "claude-haiku-4-5"} {:name "claude-sonnet-4-6"}]}]}]
      (it "the proposed model becomes the child's EFFECTIVE model"
          (expect (= "claude-haiku-4-5"
                     (:name (lp/resolve-effective-model (lp/router-for-model router
                                                                             "claude-haiku-4-5")))))
          (expect (= "claude-sonnet-4-6"
                     (:name (lp/resolve-effective-model
                              (lp/router-for-model router "claude-sonnet-4-6"))))))
      (it "an ORDERED preference list reorders provider/model order (svar falls back)"
          (let [r (lp/router-for-model router ["claude-sonnet-4-6" "claude-haiku-4-5"])]
            ;; most-preferred is effective; the full order reflects the preference
            ;; then the rest as fallback — svar routes this order, no svar change.
            (expect (= "claude-sonnet-4-6" (:name (lp/resolve-effective-model r))))
            (expect (= ["claude-sonnet-4-6" "claude-haiku-4-5" "claude-opus-4-8"]
                       (vec (for [p (:providers r)
                                  m (:models p)]

                              (:name m)))))))
      (it "omitted (nil/blank) → child inherits the parent's default model"
          (expect (= "claude-opus-4-8"
                     (:name (lp/resolve-effective-model (lp/router-for-model router nil)))))
          (expect (= "claude-opus-4-8"
                     (:name (lp/resolve-effective-model (lp/router-for-model router "  "))))))
      (it "unknown model → falls back to the parent's default (no crash)"
          (expect (= "claude-opus-4-8"
                     (:name (lp/resolve-effective-model (lp/router-for-model router "gpt-9"))))))
      (it "preserves the full provider set (just reordered) so keys/opts survive"
          (expect (= #{:anthropic-coding-plan :anthropic}
                     (set (map :id
                               (:providers (lp/router-for-model router "claude-haiku-4-5"))))))))))

(defdescribe
  sub-loop!-test
  (describe
    "sub-loop! assembly (stubbed env/turn — no LLM, no FS)"
    (let [child-ctx
          (atom {})

          captured
          (atom nil)

          router
          {:providers [{:id :anthropic-coding-plan :models [{:name "opus"}]}
                       {:id :anthropic :models [{:name "haiku"}]}]}

          parent
          {:router router
           :db-info :db
           :depth-atom (atom 0)
           :session/state-id "parent-state-123"
           :workspace {:id "parent-ws" :root "/parent"}}

          r
          (with-redefs [lp/child-workspace!
                        (fn [_db _pw]
                          {:id "child-ws" :root "/child" :fork-ms 0})

                        lp/create-environment
                        (fn [router opts]
                          (reset! captured {:router router :opts opts})
                          {:ctx-atom child-ctx :owns-db? false :db-info :db})

                        lp/run-turn!
                        (fn [_e _p _o]
                          {:status :success :answer "did it"})

                        lp/dispose-environment!
                        (fn [_])]

            (lp/sub-loop! parent
                          {:prompt "implement oauth"
                           :subctx {"focus" "oauth" "tasks" {"oauth" {"status" "doing"}}}
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
          (expect (= "oauth" (get r "task_id")))
          ;; status is the child turn's status, coerced to a python-facing STRING
          (expect (= "success" (get r "status")))
          (expect (= "did it" (get r "answer"))))))
  (describe "depth cap"
            (it "throws :vis/subloop-depth-exceeded past MAX-SUBLOOP-DEPTH"
                (with-redefs [lp/child-workspace!
                              (fn [& _]
                                {:id "x" :root "/x"})

                              lp/create-environment
                              (fn [& _]
                                {:ctx-atom (atom {}) :owns-db? false})

                              lp/run-turn!
                              (fn [& _]
                                {})

                              lp/dispose-environment!
                              (fn [_])]

                  (expect (throws? clojure.lang.ExceptionInfo
                                   #(lp/sub-loop!
                                      {:depth-atom (atom 5) :router {} :workspace {} :db-info :db}
                                      {:prompt "x" :subctx {}}))))))
  (describe
    "child cleanup — clone trashed + env disposed (no leaks)"
    (let [stub-ws {:id "child-ws" :root "/child" :fork-ms 0}]
      (it "on success: disposes the env AND abandons the rift clone (after merging it back)"
          (let [events (atom [])]
            (with-redefs [lp/child-workspace! (fn [_ _]
                                                stub-ws)
                          lp/create-environment
                          (fn [_ _]
                            {:ctx-atom (atom {}) :owns-db? false :db-info :db})
                          lp/run-turn! (fn [_ _ _]
                                         {:status :success :answer "ok"})
                          workspace/apply! (fn [_ _]
                                             (swap! events conj :apply)
                                             {:changed []})
                          workspace/abandon! (fn [_ a]
                                               (swap! events conj [:abandon (:workspace-id a)])
                                               a)
                          lp/dispose-environment! (fn [_]
                                                    (swap! events conj :dispose))]

              (lp/sub-loop! {:router {}
                             :db-info :db
                             :depth-atom (atom 0)
                             :workspace {:id "parent-ws" :root "/parent"}}
                            {:prompt "p" :subctx {"focus" "t"}}))
            ;; merge happens before dispose before abandon — and abandon names the clone
            (expect (= [:apply :dispose [:abandon "child-ws"]] @events))))
      (it "on a thrown turn: STILL disposes the env AND abandons the clone (finally), then rethrows"
          (let [events (atom [])]
            (with-redefs [lp/child-workspace! (fn [_ _]
                                                stub-ws)
                          lp/create-environment
                          (fn [_ _]
                            {:ctx-atom (atom {}) :owns-db? false :db-info :db})
                          lp/run-turn! (fn [_ _ _]
                                         (throw (ex-info "turn blew up" {})))
                          workspace/apply! (fn [_ _]
                                             (swap! events conj :apply)
                                             {:changed []})
                          workspace/abandon! (fn [_ a]
                                               (swap! events conj [:abandon (:workspace-id a)])
                                               a)
                          lp/dispose-environment! (fn [_]
                                                    (swap! events conj :dispose))]

              (expect (throws? clojure.lang.ExceptionInfo
                               #(lp/sub-loop! {:router {}
                                               :db-info :db
                                               :depth-atom (atom 0)
                                               :workspace {:id "parent-ws" :root "/parent"}}
                                              {:prompt "p" :subctx {"focus" "t"}})))
              ;; no merge (turn failed), but BOTH cleanups ran
              (expect (= [:dispose [:abandon "child-ws"]] @events)))))))
  (describe
    "parallel-sub-loops! (stubbed sub-loop! — concurrency, ordering, failure isolation)"
    (let [live
          (atom 0)

          peak
          (atom 0)

          run
          (fn [parent specs]
            (with-redefs [lp/sub-loop! (fn [_parent {:keys [subctx]}]
                                         (let [n (swap! live inc)]
                                           (swap! peak max n))
                                         (Thread/sleep 25)
                                         (swap! live dec)
                                         (let [focus (get subctx "focus")]
                                           (when (= focus "boom")
                                             (throw (ex-info "child blew up" {})))
                                           {"task_id" focus "status" "done" "changed_files" []}))]
              (lp/parallel-sub-loops! parent specs)))

          specs
          (mapv (fn [i]
                  {"prompt" (str "t" i) "subctx" {"focus" (str "task" i)} "models" ["haiku"]})
                (range 8))

          results
          (run {:depth-atom (atom 0)} specs)]

      (it "returns one result per spec, in INPUT ORDER"
          (expect (= 8 (count results)))
          (expect (= (mapv #(str "task" %) (range 8)) (mapv #(get % "task_id") results))))
      (it "bounds concurrency to the cap (peak in-flight never exceeds 4)"
          (expect (<= @peak 4))
          (expect (pos? @peak)))
      (it "a child that throws surfaces as a failed slot, not a batch-killing exception"
          (let [r (run {:depth-atom (atom 0)}
                       [{"prompt" "ok" "subctx" {"focus" "good"}}
                        {"prompt" "bad" "subctx" {"focus" "boom"}}
                        {"prompt" "ok2" "subctx" {"focus" "fine"}}])]
            (expect (= ["good" "boom" "fine"] (mapv #(get % "task_id") r)))
            (expect (= ["done" "failed" "done"] (mapv #(get % "status") r)))
            (expect (= "child blew up" (get (second r) "error")))))))
  (describe
    "sequence-sub-loops! (:sequence composite — in order, fail-fast)"
    ;; stub sub-loop! to succeed/fail based on the spec's focus key
    (let [run (fn [focuses fail-set]
                (let [ran (atom [])]
                  (with-redefs [lp/sub-loop! (fn [_ {:keys [subctx]}]
                                               (let [f (get subctx "focus")]
                                                 (swap! ran conj f)
                                                 {"task_id" f
                                                  "status" (if (fail-set f) "failed" "done")}))]
                    {:results (lp/sequence-sub-loops! {}
                                                      (mapv (fn [f]
                                                              {"prompt" f "subctx" {"focus" f}})
                                                            focuses))
                     :ran @ran})))]
      (it "all succeed → runs every child in order, returns all"
          (let [{:keys [results ran]} (run ["a" "b" "c"] #{})]
            (expect (= ["a" "b" "c"] ran))
            (expect (= ["a" "b" "c"] (mapv #(get % "task_id") results)))
            (expect (= ["done" "done" "done"] (mapv #(get % "status") results)))))
      (it "stops at the FIRST failure — later children never run; result includes the failure"
          (let [{:keys [results ran]} (run ["a" "b" "c"] #{"b"})]
            (expect (= ["a" "b"] ran)) ; "c" never ran
            (expect (= ["a" "b"] (mapv #(get % "task_id") results)))
            (expect (= ["done" "failed"] (mapv #(get % "status") results)))))))
  (describe "selector-sub-loops! (:selector composite — alternatives until one succeeds)"
            (let [run
                  (fn [focuses fail-set]
                    (let [ran (atom [])]
                      (with-redefs [lp/sub-loop! (fn [_ {:keys [subctx]}]
                                                   (let [f (get subctx "focus")]
                                                     (swap! ran conj f)
                                                     {"task_id" f
                                                      "status" (if (fail-set f) "failed" "done")}))]
                        {:results (lp/selector-sub-loops! {}
                                                          (mapv (fn [f]
                                                                  {"prompt" f "subctx" {"focus" f}})
                                                                focuses))
                         :ran @ran})))]
              (it "first child succeeds → stops immediately, no alternatives tried"
                  (let [{:keys [results ran]} (run ["a" "b" "c"] #{})]
                    (expect (= ["a"] ran))
                    (expect (= ["a"] (mapv #(get % "task_id") results)))
                    (expect (= ["done"] (mapv #(get % "status") results)))))
              (it "tries alternatives in order until one succeeds; later ones skipped"
                  (let [{:keys [results ran]} (run ["a" "b" "c"] #{"a"})]
                    (expect (= ["a" "b"] ran)) ; "c" never tried
                    (expect (= ["failed" "done"] (mapv #(get % "status") results)))))
              (it "all alternatives fail → returns every attempt (all failures)"
                  (let [{:keys [results ran]} (run ["a" "b"] #{"a" "b"})]
                    (expect (= ["a" "b"] ran))
                    (expect (= ["failed" "failed"] (mapv #(get % "status") results)))))))
  (describe "retry-sub-loop! (stubbed sub-loop! — selector: re-run until success)"
            (it "succeeds on the first attempt — no re-run"
                (let [calls (atom 0)]
                  (with-redefs [lp/sub-loop! (fn [_ _]
                                               (swap! calls inc)
                                               {"task_id" "t" "status" "done"})]
                    (let [r (lp/retry-sub-loop! {} {"prompt" "p" "subctx" {"focus" "t"}} 3)]
                      (expect (= "done" (get r "status")))
                      (expect (= 1 (get r "attempts")))
                      (expect (= 1 @calls))))))
            (it "re-runs a failing child until it succeeds, stamping the winning attempt"
                (let [calls (atom 0)]
                  (with-redefs [lp/sub-loop! (fn [_ _]
                                               (let [n (swap! calls inc)]
                                                 (if (< n 3)
                                                   {"task_id" "t" "status" "failed"}
                                                   {"task_id" "t" "status" "done"})))]
                    (let [r (lp/retry-sub-loop! {} {"prompt" "p" "subctx" {"focus" "t"}} 5)]
                      (expect (= "done" (get r "status")))
                      (expect (= 3 (get r "attempts")))
                      (expect (= 3 @calls))))))
            (it "exhausts n attempts and returns the last failure (status in the failure set)"
                (let [calls (atom 0)]
                  (with-redefs [lp/sub-loop! (fn [_ _]
                                               (swap! calls inc)
                                               {"task_id" "t" "status" "rejected"})]
                    (let [r (lp/retry-sub-loop! {} {"prompt" "p" "subctx" {"focus" "t"}} 2)]
                      (expect (= "rejected" (get r "status")))
                      (expect (= 2 (get r "attempts")))
                      (expect (= 2 @calls))))))
            (it "treats a THROWN child as a failure and retries; defaults to 2 attempts"
                (let [calls (atom 0)]
                  (with-redefs [lp/sub-loop! (fn [_ _]
                                               (swap! calls inc)
                                               (throw (ex-info "blew up" {})))]
                    (let [r (lp/retry-sub-loop! {} {"prompt" "p" "subctx" {"focus" "t"}} nil)]
                      (expect (= "failed" (get r "status")))
                      (expect (= "blew up" (get r "error")))
                      (expect (= 2 (get r "attempts")))
                      (expect (= 2 @calls)))))))
  (describe
    "SINGULAR DB connection (child reuses the parent's; never opens its own)"
    (it
      "a child env shares the EXACT parent db-info and disposing it leaves the parent connection alive"
      ;; A sub_loop child (and every parallel child) must run on the parent's ONE
      ;; DB connection — `:parent-db-info` short-circuits `db-create-connection!`,
      ;; so no new pool/datasource is opened. Critical for `:memory` (per-connection
      ;; — a fresh one would be a SEPARATE empty DB) and to avoid connection sprawl.
      (let [parent (lp/create-environment ::router {:db :memory})]
        (try (let [child (lp/create-environment ::router
                                                {:child {:parent-db-info (:db-info parent)
                                                         :depth 1}})]
               ;; SAME connection object — not a second pool
               (expect (identical? (:db-info parent) (:db-info child)))
               (expect (false? (:owns-db? child)))
               (expect (not (false? (:owns-db? parent))))
               ;; disposing the child must NOT close the shared connection
               (lp/dispose-environment! child))
             ;; parent's db-info is still usable after the child was disposed
             (expect (= [] (persistance/db-list-session-turns (:db-info parent) (random-uuid))))
             (finally (lp/dispose-environment! parent)))))))

(defdescribe
  hopeless-context-overflow-breaker-test
  "VIS-9: a preflight context overflow far beyond the call's max-input
   budget must fail the turn (fatal), not feed back to the model — the
   fed error never reaches the model (the next call dies in the same
   preflight) and appending it only grows the input. Marginal overflows
   keep the feed path so trailer folding / summarize can recover."
  (let [overflow-ex
        (fn [input max-input]
          (ex-info "Context overflow"
                   {:type :svar.tokens/context-overflow
                    :model "claude-fable-5"
                    :input-tokens input
                    :max-input-tokens max-input
                    :overflow (- input max-input)}))

        ctx
        {:iteration 1 :messages [] :routing {} :reasoning-level nil}]

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
        (let [result (lp/handle-iteration-exception! (ex-info "NameError: nope"
                                                              {:type :vis/eval-error})
                                                     ctx)]
          (expect (not (:com.blockether.vis.internal.loop/fatal-iteration-error result)))))))

(defdescribe
  provider-unavailable-retry-test
  "svar's terminal `:svar.llm/provider-unavailable` (a single pinned provider
   whose upstream call failed before any usable response) gets a few transparent
   re-sends (widening backoff) at the provider-call boundary; if it still fails the turn ends fatal
   with the provider-error card — never fed back to the model as an
   unactionable outage, never a silent cross-provider hop."
  (let [ctx {:iteration 1 :messages [] :routing {} :reasoning-level nil}]
    (it "recognises :svar.llm/provider-unavailable as retry-able"
        (let [e (ex-info "Provider unavailable" {:type :svar.llm/provider-unavailable :status 500})]
          (expect (true? (provider-unavailable-error? e)))))
    (it "does not confuse other svar errors with the provider-unavailable variant"
        (let [e (ex-info "blank" {:type :svar.llm/empty-content})]
          (expect (false? (provider-unavailable-error? e))))
        (let [e (ex-info "max" {:type :svar.llm/max-tokens-exceeded})]
          (expect (false? (provider-unavailable-error? e)))))
    (it "is fatal once retries are exhausted — fails the turn, not fed back"
        ;; After the single transparent retry the same error reaches
        ;; handle-iteration-exception!, which must mark it fatal so the turn
        ;; surfaces the informative card instead of re-prompting the model
        ;; with an outage it cannot fix.
        (let [result (lp/handle-iteration-exception! (ex-info "Provider unavailable"
                                                              {:type :svar.llm/provider-unavailable
                                                               :status 500})
                                                     ctx)]
          (expect (contains? result :com.blockether.vis.internal.loop/iteration-error))
          (expect (true? (:com.blockether.vis.internal.loop/fatal-iteration-error result)))))))

(defdescribe
  provider-unavailable-retry-loop-test
  "Drives the per-iteration retry machinery the loop actually recurs on
   (`provider-unavailable-retry?` gate, `provider-unavailable-retry-delay-ms`
   backoff, `next-retry-counters` counter-threading) end-to-end — proving the
   1s->2s->4s / 3-retry budget AND that provider-unavailable's budget is
   independent of any stream/auth retries that ran earlier in the same iteration."
  (let [pu
        (fn []
          (ex-info "Provider unavailable" {:type :svar.llm/provider-unavailable :status 503}))

        ;; Faithful stand-in for the real recur: re-run `attempt-fn` (which
        ;; either throws or returns a value) threading the SAME counters the
        ;; production loop threads, recording each backoff slept. Returns
        ;; {:delays [...] :pu-attempt N :outcome ...}.
        drive
        (fn [init attempt-fn]
          (loop [{:keys [attempt max-tokens-attempt pu-attempt]}
                 init

                 delays
                 []]

            (let [result (try (attempt-fn) (catch Exception e e))]
              (cond
                ;; Not a throwable — a real iteration result: done.
                (not (instance? Throwable result))
                {:delays delays :pu-attempt pu-attempt :outcome result}
                ;; provider-unavailable with budget left: sleep the indexed
                ;; backoff and recur bumping ONLY pu-attempt, exactly like
                ;; the ::retry-provider-unavailable arm.
                (provider-unavailable-retry? result pu-attempt)
                (let [d (provider-unavailable-retry-delay-ms pu-attempt)
                      [a m p] (next-retry-counters ::lp/retry-provider-unavailable
                                                   {:attempt attempt
                                                    :max-tokens-attempt max-tokens-attempt
                                                    :pu-attempt pu-attempt})]

                  (recur {:attempt a :max-tokens-attempt m :pu-attempt p} (conj delays d)))
                ;; Budget exhausted (or a different error): the loop's
                ;; :else arm hands it to handle-iteration-exception!.
                :else {:delays delays :pu-attempt pu-attempt :outcome :fatal}))))

        zero
        {:attempt 0 :max-tokens-attempt 0 :pu-attempt 0}]

    (it "gate retries while budget remains and stops exactly at the cap"
        (expect (= [true true true false]
                   (mapv #(provider-unavailable-retry? (pu) %)
                         (range (inc MAX_PROVIDER_UNAVAILABLE_RETRIES))))))
    (it "a non-provider-unavailable error is never gated for PU retry"
        (expect (false? (provider-unavailable-retry? (ex-info "x" {:type :svar.llm/empty-content})
                                                     0))))
    (it "backoff widens 1s -> 2s -> 4s and clamps past the vector"
        (expect (= [1000 2000 4000 4000 4000]
                   (mapv provider-unavailable-retry-delay-ms (range 5)))))
    (it "a persistent outage: exactly 3 retries at 1s/2s/4s, then fatal"
        (let [{:keys [delays outcome pu-attempt]} (drive zero pu)]
          (expect (= [1000 2000 4000] delays))
          (expect (= MAX_PROVIDER_UNAVAILABLE_RETRIES pu-attempt))
          (expect (= :fatal outcome))))
    (it "recovers transparently when a retry finally succeeds (no fatal)"
        (let [calls
              (atom 0)

              flaky
              (fn []
                (if (< (swap! calls inc) 3) (throw (pu)) {:final-result :answer}))

              {:keys [delays outcome]}
              (drive zero flaky)]

          ;; two failures -> two backoffs -> third call returns the result
          (expect (= [1000 2000] delays))
          (expect (= {:final-result :answer} outcome))))
    (it "PU budget is INDEPENDENT of a preceding stream retry (turn-8 fix)"
        ;; Enter as if two stream-rewinds already burned `attempt` to 2. PU must
        ;; still get its FULL 1s/2s/4s budget — not start mid-backoff / short.
        (let [{:keys [delays pu-attempt]} (drive {:attempt 2 :max-tokens-attempt 0 :pu-attempt 0}
                                                 pu)]
          (expect (= [1000 2000 4000] delays))
          (expect (= MAX_PROVIDER_UNAVAILABLE_RETRIES pu-attempt))))
    (it "next-retry-counters advances ONLY the owning policy's budget"
        (let [base {:attempt 2 :max-tokens-attempt 1 :pu-attempt 1}]
          (expect (= [3 1 1] (next-retry-counters ::lp/retry-stream base)))
          (expect (= [2 1 2] (next-retry-counters ::lp/retry-provider-unavailable base)))
          (expect (= [2 2 1] (next-retry-counters {::lp/retry-max-tokens {:max_tokens 9}} base)))
          (expect (= [3 1 1] (next-retry-counters ::lp/retry-auth-refresh base)))
          (expect (= [3 1 1] (next-retry-counters ::lp/retry-auth-backoff base)))))
    (it "next-retry-counters returns nil for a real iteration result (loop exits)"
        (expect (nil? (next-retry-counters {:blocks [] :final-result :answer} zero))))))

(defdescribe
  provider-error-circuit-breaker-test
  (describe
    "next-provider-error-streak"
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
                 (let [pre
                       (@#'lp/code-entries-preflight
                        2
                        [{:source "rg(1)" :lang "python"} {:source "cat(2)" :lang "python"}])

                       entries
                       (:code-entries pre)]

                   (expect (= 1 (count entries)))
                   (expect (= "rg(1)\n\ncat(2)" (:expr (first entries))))))
             (it "leaves a single fenced block untouched"
                 (let [entries (:code-entries (@#'lp/code-entries-preflight
                                               1
                                               [{:source "rg(1)" :lang "python"}]))]
                   (expect (= 1 (count entries)))
                   (expect (= "rg(1)" (:expr (first entries))))))
             (it "dedups identical stutter-fences first (one survivor, not merged with itself)"
                 (let [entries (:code-entries (@#'lp/code-entries-preflight
                                               2
                                               [{:source "rg(1)" :lang "python"}
                                                {:source "rg(1)" :lang "python"}]))]
                   (expect (= 1 (count entries)))
                   (expect (= "rg(1)" (:expr (first entries)))))))

;; The model-facing disclosure: a trimmed iteration tells the model what dropped.
(defdescribe
  literal-code-block-error-test
  (let [err #'lp/literal-code-block-error]
    (it "valid Python code passes the guard (nil)" (expect (nil? (err "x = 1"))))
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
                   (expect (= "print(\"x\\y\")" (decode "{\"code\":\"print(\\\"x\\\\y\\\")\"}"))))))

;; Build the wire-name → `:call` shape map from the REAL tool symbols, exactly as
;; `extension/native-tool-call-shapes` does at runtime. Driving the synthesizer with
;; the ACTUAL declarations (not a hand-copied table) makes this an EQUIVALENCE ORACLE:
;; every pinned string below is the output the old hardcoded `case` produced, so a
;; green run proves the data-driven interpreter reproduces the prior behavior AND
;; that each tool's on-symbol `:call` is wired correctly. mcp lives in a separate
;; extension (off this test's classpath) so its trivial positional shapes are added
;; inline — the interpreter path they exercise is identical.
(defn- shapes-from
  [& symbol-vecs]
  (into {}
        (keep (fn [s]
                (when-let [c (:ext.symbol/call s)]
                  [(or (:ext.symbol/name s) (name (:ext.symbol/symbol s))) c])))
        (apply concat symbol-vecs)))

(def ^:private real-call-shapes
  (merge (shapes-from @ed/editing-symbols sh/shell-symbols lsf/symbols)
         {"mcp__servers" {:pos []}
          "mcp__tools" {:pos ["server"]}
          "mcp__call" {:pos ["server" "tool"] :opt-pos ["args"]}
          "mcp__connect" {:pos ["server"]}
          "mcp__disconnect" {:pos ["server"]}}))

(defdescribe
  tool-call->python-source-test
  ;; A no-`:handler` native tool is dispatched by synthesizing a bare Python call
  ;; into its bound fn. The call SHAPE is DATA on each tool's symbol (`:call`); the
  ;; engine holds NO per-tool list. A tool with no `:call` → the generic
  ;; `name({input})` form. `synth` drives the synthesizer with the REAL declarations.
  (let [synth (fn [tc]
                (@#'lp/tool-call->python-source real-call-shapes tc))]
    (it "the shapes come from the tools themselves (not a hand-copied engine table)"
        ;; wiring: the interesting shapes are declared on the real symbols.
        (expect (= {:pos ["path"] :rest :opt} (get real-call-shapes "cat")))
        (expect (= {:lead-opt "language" :rest :always} (get real-call-shapes "repl_eval")))
        (expect (= {:pos ["path"]} (get real-call-shapes "file_exists")))
        (expect (fn? (get real-call-shapes "patch")))
        (expect (fn? (get real-call-shapes "ls")))
        ;; lint_code takes a whole dict → it declares NO :call and uses the default.
        (expect (nil? (get real-call-shapes "lint_code"))))
    (it "a tool with NO :call gets the generic whole-dict call"
        (expect (= "rg({\"query\": [\"x\"]})" (synth {:name "rg" :input {"query" ["x"]}})))
        (expect (= "find_files({\"query\": \"x\"})"
                   (synth {:name "find_files" :input {"query" "x"}})))
        (expect (= "occurrences({\"name\": \"foo\"})"
                   (synth {:name "occurrences" :input {"name" "foo"}})))
        (expect (= "outline({\"path\": \"src/x.clj\"})"
                   (synth {:name "outline" :input {"path" "src/x.clj"}})))
        (expect (= "lint_code({\"code\": \"x\"})" (synth {:name "lint_code" :input {"code" "x"}}))))
    (it "python_execution still passes the model's code through"
        (expect (= "print(1)" (synth {:name "python_execution" :input {"code" "print(1)"}}))))
    (it
      "a native tool's `code` PAYLOAD is escaped into the call, NOT dumped as a program"
      ;; Regression: struct_patch/symbol_rename carry the replacement SOURCE under
      ;; `code` (arbitrary language, not Python). The generic branch once returned
      ;; that payload verbatim as the program, so a Clojure `(defn …)` was fed to the
      ;; Python engine and died with `unterminated string literal` — the edit never
      ;; ran. It must synthesize `struct_patch({…})` with the payload escaped.
      (expect
        (=
          "struct_patch({\"path\": \"a.clj\", \"op\": \"replace\", \"target\": \"f\", \"code\": \"(defn f []\\n  \\\"doc — x\\\"\\n  1)\"})"
          (synth {:name "struct_patch"
                  :input {"path" "a.clj"
                          "op" "replace"
                          "target" "f"
                          "code" "(defn f []\n  \"doc — x\"\n  1)"}})))
      ;; the synthesized program is a SINGLE line (no raw newline the Python
      ;; tokenizer could trip on) and never starts with a bare `(`.
      (let [prog (synth {:name "struct_patch"
                         :input {"path" "a.clj"
                                 "op" "replace"
                                 "target" "f"
                                 "code" "(defn f []\n  \"d\"\n  1)"}})]
        (expect (not (.contains ^String prog "\n")))
        (expect (.startsWith ^String prog "struct_patch("))))
    (it "positional + optional-rest-dict shapes (cat/sexpr/copy/shell_run)"
        (expect (= "cat(\"a.clj\")" (synth {:name "cat" :input {"path" "a.clj"}})))
        (expect (= "cat(\"a.clj\", {\"range\": [1, 2]})"
                   (synth {:name "cat" :input {"path" "a.clj" "range" [1 2]}})))
        (expect (= "sexpr(\"a.clj\", {\"nav\": [\"down\"]})"
                   (synth {:name "sexpr" :input {"path" "a.clj" "nav" ["down"]}})))
        (expect (= "copy(\"a\", \"b\")" (synth {:name "copy" :input {"src" "a" "dest" "b"}})))
        (expect (= "copy(\"a\", \"b\", {\"is_overwrite\": True})"
                   (synth {:name "copy" :input {"src" "a" "dest" "b" "is_overwrite" true}})))
        (expect (= "shell_run(\"ls\", {\"timeout_secs\": 30})"
                   (synth {:name "shell_run" :input {"cmd" "ls" "timeout_secs" 30}}))))
    (it "all-positional + optional-trailing-positional shapes"
        (expect (= "move(\"a\", \"b\")" (synth {:name "move" :input {"src" "a" "dest" "b"}})))
        (expect (= "delete(\"p\")" (synth {:name "delete" :input {"path" "p"}})))
        (expect (= "create_dirs(\"d\")" (synth {:name "create_dirs" :input {"path" "d"}})))
        (expect (= "delete_if_exists(\"d\")"
                   (synth {:name "delete_if_exists" :input {"path" "d"}})))
        (expect (= "shell_bg(\"x\", \"sleep 1\")"
                   (synth {:name "shell_bg" :input {"id" "x" "cmd" "sleep 1"}})))
        (expect (= "shell_logs(\"x\", 50)" (synth {:name "shell_logs" :input {"id" "x" "n" 50}})))
        (expect (= "symbol_rename(\"a\", \"b\")"
                   (synth {:name "symbol_rename" :input {"name" "a" "new_name" "b"}})))
        (expect (= "shell_logs(\"x\")" (synth {:name "shell_logs" :input {"id" "x"}}))))
    (it "file_exists synthesizes its wire name file_exists (bound name matches)"
        (expect (= "file_exists(\"p\")" (synth {:name "file_exists" :input {"path" "p"}}))))
    (it "ls forces a leading path when opts are present (escape hatch)"
        (expect (= "ls()" (synth {:name "ls" :input {}})))
        (expect (= "ls(\"src\")" (synth {:name "ls" :input {"path" "src"}})))
        (expect (= "ls(\"src\", {\"depth\": 2})"
                   (synth {:name "ls" :input {"path" "src" "depth" 2}})))
        (expect (= "ls(\".\", {\"depth\": 2})" (synth {:name "ls" :input {"depth" 2}}))))
    (it "patch carries a single-file top-level path through (escape hatch)"
        (expect (= "patch([{\"from_anchor\": \"1:a\"}])"
                   (synth {:name "patch" :input {"edits" [{"from_anchor" "1:a"}]}})))
        (expect (= "patch({\"path\": \"a.clj\", \"edits\": [{\"from_anchor\": \"1:a\"}]})"
                   (synth {:name "patch"
                           :input {"path" "a.clj" "edits" [{"from_anchor" "1:a"}]}}))))
    (it
      "strips the model-drift leading colon at EVERY depth (not just top-level)"
      ;; Regression: the model reads keyword-heavy Clojure source and drifts into
      ;; `:key` spelling. A shallow normalize fixed top-level keys (cat(\"x\")) but
      ;; left NESTED dict keys colon-prefixed, so `patch` leaked
      ;; `patch([{\":from_anchor\": …}])`. Keys normalize at all depths; VALUES
      ;; that happen to start with a colon are untouched.
      (expect (= "cat(\"x\")" (synth {:name "cat" :input {":path" "x"}})))
      (expect (= "patch([{\"from_anchor\": \"1:a\", \"replace\": \"x\"}])"
                 (synth {:name "patch" :input {"edits" [{":from_anchor" "1:a" ":replace" "x"}]}})))
      (expect
        (=
          "patch({\"path\": \"a.clj\", \"edits\": [{\"from_anchor\": \"1:a\", \"to_anchor\": \"2:b\"}]})"
          (synth {:name "patch"
                  :input {":path" "a.clj" "edits" [{":from_anchor" "1:a" ":to_anchor" "2:b"}]}})))
      ;; a VALUE spelled like a keyword survives verbatim — only KEYS are cleaned.
      (expect (= "patch([{\"from_anchor\": \"1:a\", \"replace\": \":kw-value\"}])"
                 (synth {:name "patch"
                         :input {"edits" [{"from_anchor" "1:a" "replace" ":kw-value"}]}}))))
    (it "language facade splits `language` into the leading positional"
        (expect (= "repl_eval(\"clojure\", {\"code\": \"(+ 1 2)\"})"
                   (synth {:name "repl_eval" :input {"language" "clojure" "code" "(+ 1 2)"}})))
        (expect (= "format_code(\"clojure\", {\"code\": \"x\"})"
                   (synth {:name "format_code" :input {"language" "clojure" "code" "x"}})))
        (expect (= "run_tests({\"namespaces\": [\"a.b\"]})"
                   (synth {:name "run_tests" :input {"namespaces" ["a.b"]}})))
        (expect (= "repl_stop(\"r1\")" (synth {:name "repl_stop" :input {"id" "r1"}}))))
    (it "mcp verbs bind positionally under the wire name"
        (expect (= "mcp__servers()" (synth {:name "mcp__servers" :input {}})))
        (expect (= "mcp__tools(\"fs\")" (synth {:name "mcp__tools" :input {"server" "fs"}})))
        (expect (= "mcp__call(\"fs\", \"read\", {\"p\": 1})"
                   (synth {:name "mcp__call" :input {"server" "fs" "tool" "read" "args" {"p" 1}}})))
        (expect (= "mcp__call(\"fs\", \"read\")"
                   (synth {:name "mcp__call" :input {"server" "fs" "tool" "read"}})))
        (expect (= "mcp__connect(\"fs\")" (synth {:name "mcp__connect" :input {"server" "fs"}})))
        (expect (= "mcp__disconnect(\"fs\")"
                   (synth {:name "mcp__disconnect" :input {"server" "fs"}}))))))

;; ===========================================================================
;; All-observation concurrent batch
;; ===========================================================================

(def ^:private observation-batch? (deref #'lp/observation-batch?))

(def ^:private observation-batch-program (deref #'lp/observation-batch-program))

(def ^:private execute-observation-batch (deref #'lp/execute-observation-batch))

(defdescribe
  observation-batch-gate-test
  ;; The gate decides whether an iteration's tool calls may run concurrently.
  ;; It is CONSERVATIVE: any mutation / python_execution / native handler /
  ;; preflight error / <2 calls / blank source ⇒ serial (false). Classification
  ;; is by the AUTHORITATIVE per-symbol :tag (never a hardcoded name list).
  (let [tags
        {"cat" :observation
         "rg" :observation
         "find_files" :observation
         "ls" :observation
         "outline" :observation
         "occurrences" :observation
         "file_exists" :observation
         "sexpr" :observation
         "patch" :mutation
         "write" :mutation
         "struct_patch" :mutation}

        obs
        (fn [id nm]
          {:expr (str nm "({})") :svar/tool-call-id id :vis/tool-name nm})]

    (it "batches two observations"
        (expect (true? (observation-batch? [(obs "a" "cat") (obs "b" "rg")] tags))))
    (it "batches three observations"
        (expect (true? (observation-batch? [(obs "a" "cat") (obs "b" "cat") (obs "c" "rg")] tags))))
    (it "does NOT batch a single call"
        (expect (false? (observation-batch? [(obs "a" "cat")] tags))))
    (it "does NOT batch when any call is a mutation"
        (expect (false? (observation-batch? [(obs "a" "cat") (obs "b" "patch")] tags))))
    (it "does NOT batch when any call is python_execution (no tag)"
        (expect (false? (observation-batch? [(obs "a" "cat")
                                             {:expr "print(1)"
                                              :svar/tool-call-id "b"
                                              :vis/tool-name "python_execution"}]
                                            tags))))
    (it "does NOT batch when a native-handler entry is present"
        (expect (false? (observation-batch? [(obs "a" "cat")
                                             {:expr ""
                                              :svar/tool-call-id "b"
                                              :vis/tool-name "skill"
                                              :vis/native-handler (fn [_ _]
                                                                    {})}]
                                            tags))))
    (it "does NOT batch when any entry has a preflight error"
        (expect (false? (observation-batch? [(obs "a" "cat")
                                             (assoc (obs "b" "rg")
                                               :vis/preflight-error {:message "boom"})]
                                            tags))))
    (it "does NOT batch when a tool-call-id is missing"
        (expect (false? (observation-batch? [(obs "a" "cat")
                                             (dissoc (obs "b" "rg") :svar/tool-call-id)]
                                            tags))))
    (it "does NOT batch when a source is blank"
        (expect (false? (observation-batch? [(obs "a" "cat") (assoc (obs "b" "rg") :expr "  ")]
                                            tags))))
    (it "does NOT batch an unknown/untagged tool name"
        (expect (false? (observation-batch? [(obs "a" "cat") (obs "b" "mystery")] tags))))
    (it "does NOT batch a native-handler OBSERVATION (search_web runs in Clojure, not GraalPy)"
        ;; search_web is :tag :observation BUT dispatched via run-native-handler
        ;; (pure Clojure, not synthesized-python) — its block carries
        ;; :vis/native-handler and NO :expr, so it can't ride a gather batch. The
        ;; gate must fall the whole iteration to serial. Double-guarded: the
        ;; native-handler flag AND the nil/blank :expr both reject it.
        (let [tags* (assoc tags "search_web" :observation)]
          (expect (false? (observation-batch? [(obs "a" "cat")
                                               {:expr nil
                                                :svar/tool-call-id "b"
                                                :vis/tool-name "search_web"
                                                :vis/native-handler (fn [_ _]
                                                                      {})
                                                :vis/native-input {}}]
                                              tags*)))))))

(defdescribe
  observation-batch-program-shape-test
  (it "wraps the exprs in an isolated-par settle over a deferred list"
      (let [p (observation-batch-program ["cat(\"a\", {})" "rg({\"query\": \"x\"})"])]
        (expect (str/includes? p "__vis_obs_batch__ = [cat(\"a\", {}), rg({\"query\": \"x\"})]"))
        (expect (str/includes? p "__vis_par_isolated__("))
        (expect (str/includes? p "__vis_settle__(__x__)")))))

(defn- env-root
  "The sandbox's primary allowed root — where cat is confined for a :memory env.
   Falls back to cwd. Temp files for the fs tests MUST live under here or cat
   refuses them (`escapes the allowed workspace roots`)."
  [env]
  (or (:workspace/root env)
      (some-> (:workspace env)
              :root
              str)
      (System/getProperty "user.dir")))

(defn- write-tmp!
  "Write a temp file UNDER `dir` (an allowed sandbox root) and return its path."
  [dir content]
  (let [f (java.io.File/createTempFile "vis-obs-batch" ".txt" (java.io.File. (str dir)))]
    (spit f content)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defdescribe
  execute-observation-batch-integration-test
  ;; End-to-end against a REAL GraalPy sandbox built by create-environment (so
  ;; __vis_par_isolated__ + cat are wired exactly as production). Proves: ordered
  ;; re-split (result[i] ↔ entry[i]), per-call error isolation (a missing file
  ;; only fails ITS slot), and — with slow fake tools — real concurrency.
  (it
    "runs an all-cat batch concurrently, preserving order + isolating one failure"
    (let [env
          (lp/create-environment ::router {:db :memory})

          root
          (env-root env)

          pa
          (write-tmp! root "AAA-content")

          pc
          (write-tmp! root "CCC-content")]

      (try (let [;; slot 0 ok, slot 1 missing file (isolated error), slot 2 ok
                 entries
                 [{:expr (str "cat(" (pr-str pa) ", {})")
                   :svar/tool-call-id "id-0"
                   :vis/tool-name "cat"}
                  {:expr "cat(\"/no/such/file-xyz.txt\", {})"
                   :svar/tool-call-id "id-1"
                   :vis/tool-name "cat"}
                  {:expr (str "cat(" (pr-str pc) ", {})")
                   :svar/tool-call-id "id-2"
                   :vis/tool-name "cat"}]

                 out
                 (execute-observation-batch env entries)]

             (expect (= 3 (count out)))
             ;; ordered + paired: slot 0 read AAA, slot 2 read CCC, both succeeded
             (expect (nil? (:error (nth out 0))))
             (expect (some? (:result (nth out 0))))
             (expect (str/includes? (str (:result (nth out 0))) "AAA-content"))
             (expect (nil? (:error (nth out 2))))
             (expect (str/includes? (str (:result (nth out 2))) "CCC-content"))
             ;; ISOLATION: only slot 1 errored; its siblings still returned values
             (expect (nil? (:result (nth out 1))))
             (expect (some? (:error (nth out 1))))
             (expect (map? (:error (nth out 1)))))
           (finally (lp/dispose-environment! env)
                    (.delete (java.io.File. ^String pa))
                    (.delete (java.io.File. ^String pc))))))
  (it "actually overlaps calls on virtual threads (wall ≈ max, not sum)"
      ;; Bind two SLOW fake observation tools via set-python-binding! so their
      ;; host bodies (Thread/sleep) overlap. 3×250ms serial=750ms; concurrent≈250.
      (let [env (lp/create-environment ::router {:db :memory})]
        (try (let [pc (:python-context env)
                   slow (fn [nm]
                          (fn [& _args]
                            (Thread/sleep 250)
                            {"op" nm "v" nm}))]

               (env/set-python-binding! pc 'slowcat (slow "slowcat"))
               (env/set-python-binding! pc 'slowrg (slow "slowrg"))
               (let [entries [{:expr "slowcat({})" :svar/tool-call-id "a" :vis/tool-name "slowcat"}
                              {:expr "slowcat({})" :svar/tool-call-id "b" :vis/tool-name "slowcat"}
                              {:expr "slowrg({})" :svar/tool-call-id "c" :vis/tool-name "slowrg"}]
                     t0 (System/currentTimeMillis)
                     out (execute-observation-batch env entries)
                     dt (- (System/currentTimeMillis) t0)]

                 (expect (= 3 (count out)))
                 (expect (every? (comp nil? :error) out))
                 ;; Concurrency proof: comfortably under the 750ms serial floor.
                 (expect (< dt 600))))
             (finally (lp/dispose-environment! env))))))

(defdescribe
  run-iteration-observation-batch-test
  ;; FULL pipeline: a model reply with TWO cat tool_calls drives run-iteration;
  ;; the all-observation batch runs concurrently, and each result is re-split +
  ;; PAIRED to its own tool_use_id in the emitted blocks + :form-result chunks.
  (it
    "pairs each batched observation result to its tool_use_id, in order"
    (let [env
          (lp/create-environment ::router {:db :memory})

          chunks
          (atom [])

          root
          (env-root env)

          pa
          (write-tmp! root "FIRST-file")

          pb
          (write-tmp! root "SECOND-file")]

      (try
        (let [active (deref (:extensions env))]
          (with-redefs [svar/ask-code! (fn [_router _opts]
                                         {:stop-reason :tool-calls
                                          :tool-calls [{:id "call_A" :name "cat" :input {:path pa}}
                                                       {:id "call_B" :name "cat" :input {:path pb}}]
                                          :content nil
                                          :reasoning nil
                                          :tokens {}})]
            (let [result (lp/run-iteration env
                                           []
                                           {:iteration 0
                                            :active-extensions active
                                            :resolved-model {:provider :zai-coding-plan
                                                             :name "glm-5.1"}
                                            :on-chunk #(swap! chunks conj %)})
                  blocks (:blocks result)
                  frs (filterv #(= :form-result (:phase %)) @chunks)]

              ;; two blocks, each paired to its own tool_use id, in order
              (expect (= 2 (count blocks)))
              (expect (= ["call_A" "call_B"] (mapv :svar/tool-call-id blocks)))
              ;; each block carries its OWN file's content (correct re-split)
              (expect (str/includes? (str (:result (nth blocks 0))) "FIRST-file"))
              (expect (str/includes? (str (:result (nth blocks 1))) "SECOND-file"))
              (expect (nil? (:error (nth blocks 0))))
              (expect (nil? (:error (nth blocks 1))))
              ;; per-call streaming stayed coherent: one :form-result per call,
              ;; at positions 0 and 1
              (expect (= 2 (count frs)))
              (expect (= [0 1] (mapv :position frs))))))
        (finally (lp/dispose-environment! env)
                 (.delete (java.io.File. ^String pa))
                 (.delete (java.io.File. ^String pb))))))
  (it "keeps a mutation-bearing iteration serial (patch is never batched)"
      ;; A cat + patch iteration must NOT batch. We assert the gate rejects it via
      ;; the real tags; the serial path still runs (both blocks present, paired).
      (let [env (lp/create-environment ::router {:db :memory})]
        (try (let [active (deref (:extensions env))
                   tags (extension/native-tool-tags active)]

               (expect (= :observation (get tags "cat")))
               (expect (= :mutation (get tags "patch")))
               (expect (false?
                         (observation-batch?
                           [{:expr "cat(\"x\", {})" :svar/tool-call-id "a" :vis/tool-name "cat"}
                            {:expr "patch([])" :svar/tool-call-id "b" :vis/tool-name "patch"}]
                           tags))))
             (finally (lp/dispose-environment! env))))))

(def ^:private settle-gather-futures! (deref #'lp/settle-gather-futures!))

(defdescribe
  settle-gather-futures-test
  ;; The gather settle loop `(.get f)`-ed each child in order and swallowed
  ;; InterruptedException as that slot's `:err` — so an eval-timeout/cancel
  ;; `.cancel(true)` on the worker never reached the CHILD futures: cancelled
  ;; `gather(rg(...), rg(...))` calls left orphaned virtual threads grinding
  ;; at 100% CPU each until process exit. Now an interrupt during settle
  ;; hard-cancels every child and propagates.
  (it "settles every slot in order — value OR error, a failing slot never aborts siblings"
      (let [exec (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)]
        (try (let [futs [(.submit exec
                                  ^java.util.concurrent.Callable
                                  (fn []
                                    1))
                         (.submit exec
                                  ^java.util.concurrent.Callable
                                  (fn []
                                    (throw (ex-info "boom" {}))))
                         (.submit exec
                                  ^java.util.concurrent.Callable
                                  (fn []
                                    3))]
                   outcomes (settle-gather-futures! futs)]

               (expect (= {:ok 1} (nth outcomes 0)))
               (expect (= "boom" (ex-message (:err (nth outcomes 1)))))
               (expect (= {:ok 3} (nth outcomes 2))))
             (finally (.shutdownNow exec)))))
  (it "hard-cancels still-running children and rethrows when the settling thread is interrupted"
      (let [exec
            (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)

            child-interrupted
            (promise)]

        (try (let [futs
                   [(.submit exec
                             ^java.util.concurrent.Callable
                             (fn []
                               (try (Thread/sleep 60000)
                                    :never
                                    (catch InterruptedException _
                                      (deliver child-interrupted true)
                                      :interrupted))))]

                   _
                   (.interrupt (Thread/currentThread))

                   thrown
                   (try (settle-gather-futures! futs)
                        nil
                        (catch InterruptedException e e)
                        (finally
                          ;; clear the flag before any further test plumbing
                          (Thread/interrupted)))]

               (expect (some? thrown))
               (expect (.isCancelled ^java.util.concurrent.Future (first futs)))
               (expect (= true (deref child-interrupted 2000 ::timeout))))
             (finally (Thread/interrupted) (.shutdownNow exec))))))

(defdescribe parallel-sub-loops-cancel-test
             ;; `(mapv deref futs)` propagated an interrupt from the COORDINATING thread
             ;; but never cancelled the child futures — a cancelled parallel sub-loop
             ;; batch left orphaned full LLM turns running to completion.
             (it "hard-cancels child sub-loops and rethrows when the coordinator is interrupted"
                 (let [child-interrupted (promise)]
                   (with-redefs-fn {#'lp/run-spec! (fn [_ _]
                                                     (try (Thread/sleep 60000)
                                                          :never
                                                          (catch InterruptedException _
                                                            (deliver child-interrupted true)
                                                            :interrupted)))}
                     #(try (let [thrown (try (.interrupt (Thread/currentThread))
                                             (lp/parallel-sub-loops! nil [{:prompt "x"}])
                                             nil
                                             (catch InterruptedException e e)
                                             (finally (Thread/interrupted)))]
                             (expect (some? thrown))
                             (expect (= true (deref child-interrupted 2000 ::timeout))))
                           (finally (Thread/interrupted)))))))

;; ── post-refresh propagation backoff (gateway-wide OAuth-401 storm guard) ──
(def ^:private auth-last-refreshed (deref #'lp/auth-last-refreshed))
(def ^:private refresh-just-failed? (deref #'lp/refresh-just-failed?))
(def ^:private note-provider-request-ok! (deref #'lp/note-provider-request-ok!))
(def ^:private auth-refreshable-error? (deref #'lp/auth-refreshable-error?))
(def ^:private auth-propagation-backoff-ms (deref #'lp/auth-propagation-backoff-ms))
(def ^:private AUTH_PROPAGATION_WINDOW_MS (deref #'lp/AUTH_PROPAGATION_WINDOW_MS))

(defn- auth-401
  []
  (ex-info "boom"
           {:status 401 :body "{\"error\":{\"message\":\"Invalid authentication credentials\"}}"}))

(defdescribe
  post-refresh-propagation-backoff-test
  (describe
    "a token we JUST refreshed that 401s again is treated as propagation lag, not dead"
    (it "refresh-just-failed? fires when we force-refreshed within the propagation window"
        (reset! auth-last-refreshed {:ap {:at (System/currentTimeMillis)}})
        (expect (true? (refresh-just-failed? (auth-401) {:provider :ap}))))
    (it "does NOT fire once the last refresh is older than the window (real rotation → refresh)"
        (reset! auth-last-refreshed {:ap {:at (- (System/currentTimeMillis)
                                                 (long AUTH_PROPAGATION_WINDOW_MS)
                                                 1)}})
        (expect (not (refresh-just-failed? (auth-401) {:provider :ap}))))
    (it "does NOT fire when the provider was never refreshed"
        (reset! auth-last-refreshed {})
        (expect (not (refresh-just-failed? (auth-401) {:provider :ap}))))
    (it "fires regardless of token VALUE — covers providers that mint a fresh token each exchange"
        ;; Regression for the Copilot 401 storm: the old value-equality check
        ;; (minted == baked-token) never matched a rotating-token provider and
        ;; fell open into an endless re-mint. Recency matches every provider.
        (reset! auth-last-refreshed {:ap {:at (System/currentTimeMillis)}})
        (with-redefs [config/baked-token (fn [_]
                                           "a-totally-different-token")]
          (expect (true? (refresh-just-failed? (auth-401) {:provider :ap})))))
    (it "note-provider-request-ok! clears the marker so a later 401 re-mints, not backs off"
        (reset! auth-last-refreshed {:ap {:at (System/currentTimeMillis)}})
        (note-provider-request-ok! {:provider :ap})
        (expect (nil? (get @auth-last-refreshed :ap)))
        (expect (not (refresh-just-failed? (auth-401) {:provider :ap}))))
    (it "a post-refresh 401 stays REFRESHABLE-shaped but routes to backoff, never a dead latch"
        ;; No dead-credential latch exists any more: the provider is
        ;; always eligible to recover; the classifier just prefers the
        ;; SAME-token backoff over another re-mint while lag settles.
        (with-redefs [config/baked-token
                      (fn [_]
                        "T-fresh")

                      registry/provider-by-id
                      (fn [_]
                        {:provider/get-token-fn (fn []
                                                  {:token "T-fresh"})
                         :provider/refresh-token-fn (fn [& _]
                                                      :ok)})]

          (reset! auth-last-refreshed {:ap {:at (System/currentTimeMillis)}})
          (expect (true? (auth-refreshable-error? (auth-401) {:provider :ap})))))
    (it "backoff widens with the attempt count and is capped at 5s"
        (expect (= 1200 (auth-propagation-backoff-ms 0)))
        (expect (= 3600 (auth-propagation-backoff-ms 2)))
        (expect (= 5000 (auth-propagation-backoff-ms 10))))))

(def ^:private env-cache (deref #'lp/cache))
(def ^:private new-cache-entry (deref #'lp/new-cache-entry))
(def ^:private touch-entry! (deref #'lp/touch-entry!))
(def ^:private evict-if-idle! (deref #'lp/evict-if-idle!))

(defn- backdate-entry!
  "Push `entry`'s :last-active `ms` into the past so it reads as idle."
  [entry ms]
  (let [^java.util.concurrent.atomic.AtomicLong la (:last-active entry)]
    (.set la (- (System/currentTimeMillis) ms))))

(defdescribe env-reaper-test
             ;; The idle-env reaper is the authoritative backstop against unbounded
             ;; GraalPy Context growth. An empty {} env is safe to dispose:
             ;; dispose-environment! no-ops with no :python-context / :db-info.
             (describe "evict-if-idle!"
                       (it "disposes + evicts an idle, unlocked entry"
                           (let [k
                                 "reaper-test/idle"

                                 entry
                                 (new-cache-entry {})]

                             (swap! env-cache assoc k entry)
                             (try (backdate-entry! entry 10000)
                                  (expect (true? (evict-if-idle! k 5000)))
                                  (expect (not (contains? @env-cache k)))
                                  (finally (swap! env-cache dissoc k)))))
                       (it
                         "skips an entry whose lock is held (a running turn)"
                         (let [k
                               "reaper-test/busy"

                               entry
                               (new-cache-entry {})

                               ^java.util.concurrent.locks.ReentrantLock lock
                               (:lock entry)]

                           (swap! env-cache assoc k entry)
                           ;; A running turn holds the lock on ANOTHER thread;
                           ;; ReentrantLock is reentrant, so the lock MUST be
                           ;; held off-thread for tryLock to genuinely fail.
                           (let [held
                                 (promise)

                                 release
                                 (promise)

                                 holder
                                 (Thread. ^Runnable
                                          (fn []
                                            (.lock lock)
                                            (deliver held true)
                                            @release
                                            (.unlock lock)))]

                             (try (backdate-entry! entry 10000)
                                  (.start holder)
                                  @held
                                  (expect (false? (evict-if-idle! k 5000)))
                                  (expect (contains? @env-cache k))
                                  (finally (deliver release true)
                                           (.join holder 1000)
                                           (swap! env-cache dissoc k))))))
                       (it "keeps a freshly-touched (not-yet-idle) entry"
                           (let [k
                                 "reaper-test/warm"

                                 entry
                                 (new-cache-entry {})]

                             (swap! env-cache assoc k entry)
                             (try (touch-entry! entry)
                                  (expect (false? (evict-if-idle! k 60000)))
                                  (expect (contains? @env-cache k))
                                  (finally (swap! env-cache dissoc k)))))))

(def ^:private reap-idle-envs! (deref #'lp/reap-idle-envs!))
(def ^:private heap-pressure? (deref #'lp/heap-pressure?))

(defdescribe env-heap-watermark-test
             ;; Layer 3: under JVM heap pressure the reaper force-evicts EVERY
             ;; idle (unlocked) env this sweep, ignoring the idle TTL, to shed
             ;; GraalPy Contexts fast. A running turn (lock held off-thread) is
             ;; still skipped; the transcript reloads from the DB.
             (describe "reap-idle-envs! under heap pressure"
                       (it "force-evicts fresh, unlocked entries when pressured"
                           (let [k "watermark-test/fresh"]
                             (swap! env-cache assoc k (new-cache-entry {}))
                             (try
                               ;; not idle (just touched) + default 15m TTL: a
                               ;; normal sweep keeps it ...
                               (with-redefs [lp/heap-pressure? (constantly false)]
                                 (reap-idle-envs!)
                                 (expect (contains? @env-cache k)))
                               ;; ... but under pressure it is evicted now.
                               (with-redefs [lp/heap-pressure? (constantly true)]
                                 (expect (pos? (reap-idle-envs!)))
                                 (expect (not (contains? @env-cache k))))
                               (finally (swap! env-cache dissoc k)))))
                       (it "still skips a locked entry (a running turn) under pressure"
                           (let [k
                                 "watermark-test/busy"

                                 entry
                                 (new-cache-entry {})

                                 ^java.util.concurrent.locks.ReentrantLock lock
                                 (:lock entry)]

                             (swap! env-cache assoc k entry)
                             (let [held
                                   (promise)

                                   release
                                   (promise)

                                   holder
                                   (Thread. ^Runnable
                                            (fn []
                                              (.lock lock)
                                              (deliver held true)
                                              @release
                                              (.unlock lock)))]

                               (try (.start holder)
                                    @held
                                    (with-redefs [lp/heap-pressure? (constantly true)]
                                      (reap-idle-envs!)
                                      (expect (contains? @env-cache k)))
                                    (finally (deliver release true)
                                             (.join holder 1000)
                                             (swap! env-cache dissoc k))))))
                       (it "heap-pressure? is disabled when the watermark is <= 0"
                           (expect (false? (with-redefs [lp/env-heap-watermark-pct 0]
                                             (heap-pressure?)))))))

(def ^:private bump-turns! (deref #'lp/bump-turns!))
(def ^:private recycle-env! (deref #'lp/recycle-env!))

(defdescribe env-recycle-test
             ;; Layer 2: a single long-lived (never-idle) session's Context is
             ;; recycled between turns after `env-max-turns-per-ctx` turns — dispose
             ;; + rebuild IN PLACE, reusing the same lock so a queued caller stays
             ;; correct.
             (describe "bump-turns!"
                       (it "increments the per-context counter and returns the count"
                           (let [entry (new-cache-entry {})]
                             (expect (= 1 (bump-turns! entry)))
                             (expect (= 2 (bump-turns! entry)))
                             (expect (= 3 (bump-turns! entry)))))
                       (it "returns 0 for an entry with no counter"
                           (expect (= 0 (bump-turns! {})))))
             (describe
               "recycle-env!"
               (it
                 "swaps a fresh env in place, reuses the lock, disposes the old"
                 (let [k
                       "recycle-test/turn-cap"

                       old-env
                       {:marker :old}

                       fresh-env
                       {:marker :fresh}

                       entry
                       (new-cache-entry old-env)

                       disposed
                       (atom [])]

                   (swap! env-cache assoc k entry)
                   (try (with-redefs [lp/open-env!
                                      (fn [_ _]
                                        fresh-env)

                                      lp/dispose-environment!
                                      (fn [e]
                                        (swap! disposed conj e))]

                          (recycle-env! k))
                        (let [e2 (get @env-cache k)]
                          ;; fresh env installed under the same key
                          (expect (= fresh-env (:environment e2)))
                          ;; SAME lock preserved so a queued caller stays correct
                          (expect (identical? (:lock entry) (:lock e2)))
                          ;; turn counter reset for the fresh context
                          (expect (= 0 (.get ^java.util.concurrent.atomic.AtomicLong (:turns e2))))
                          ;; the OLD env disposed exactly once
                          (expect (= [old-env] @disposed)))
                        (finally (swap! env-cache dissoc k)))))))
