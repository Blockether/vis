(ns com.blockether.vis.internal.provider.error-test
  "Provider-error presentation and canonical typed error content."
  (:require [com.blockether.vis.contract.content :as content-contract]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.provider.error :as perr]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private exhausted-err
  "The shape vis builds from svar's `all-providers-exhausted` ex-info: the full
   ex-data lands under `:data` (see loop/format-exception)."
  {:message "All providers exhausted"
   :data {:type :svar.llm/all-providers-exhausted
          :attempts
          [{:provider "anthropic"
            :model "claude-opus-4"
            :status 429
            :reason :rate-limit
            :error "rate limited"}
           {:provider "openai" :model "gpt-5" :status 401 :reason :auth :error "unauthorized"}]}})

(defdescribe refresh-grant-rejection-presentation-test
             (it "shows a sign-in requirement instead of suggesting another unchanged retry"
                 (let [error
                       (ex-info
                         "Your sign-in has expired or was revoked. Select Connect to sign in again."
                         {:type :provider/reauthentication-required
                          :provider-id :anthropic-coding-plan})]
                   (doseq [value [error {:message (ex-message error) :data (ex-data error)}]]
                     (expect (= :auth (perr/provider-error-kind value)))))))

;; Regression, issue #167: the upstream status survived classification but the
;; canonical block flattened the diagnosis into prose, leaving channels to lose
;; the HTTP fact or parse sentences back into a card.
(defdescribe provider-error-block-view-model-test
             (it "ships the presentation and upstream facts as separate fields"
                 (let [err
                       {:message "Exceptional status code: 429"
                        :data {:status 429
                               :provider :anthropic
                               :request-id "req_167"
                               :body "{\"error\":{\"message\":\"rate limited\"}}"}}

                       block
                       (first (perr/provider-error-content err))]

                   (expect (= "rate-limit" (get block "kind")))
                   (expect (= 429 (get block "status")))
                   (expect (= "req_167" (get block "request_id")))
                   (expect (= "anthropic" (get block "provider"))))))

;; Issue #403: the Vis query id keys the log rows and the log file of the request.
(defdescribe provider-error-log-id-test
             (let [err {:message "Exceptional status code: 503"
                        :data {:status 503 :request-id "req_up" :log-id "q-403"}}]
               (it "ships the Vis query id on the typed block"
                   (expect (= "q-403" (:log-id (perr/provider-error-info err))))
                   (expect (= "q-403" (get (first (perr/provider-error-content err)) "log_id"))))
               (it "omits the row when no query id is known"
                   (let [err (update err :data dissoc :log-id)]
                     (expect (not-any? #(= "Log id" (first %)) (perr/provider-error-facts err)))
                     (expect (not (contains? (first (perr/provider-error-content err))
                                             "log_id")))))))

(defdescribe
  provider-error-attempts-test
  (it "reads the per-provider attempts from the ex-data"
      (expect (= 2 (count (perr/provider-error-attempts exhausted-err))))
      (expect (= "anthropic" (:provider (first (perr/provider-error-attempts exhausted-err))))))
  (it "no attempts (older svar / non-routing failure) → empty + nil summary"
      (let [bare {:message "All providers exhausted"
                  :data {:type :svar.llm/all-providers-exhausted}}]
        (expect (empty? (perr/provider-error-attempts bare)))))
  (it "emits one structured provider error block"
      (let [blocks
            (perr/provider-error-content exhausted-err)

            block
            (first blocks)]

        (expect (= 1 (count blocks)))
        (expect (= "error" (get block "type")))
        (expect (= 2 (count (get block "attempts"))))))
  (it "the bare `All providers exhausted` wrapper is NOT repeated as a fact row"
      ;; title + attempts already carry it — no redundant `Wrapper: …` line
      (expect (not-any? #(= "Wrapper" (first %)) (perr/provider-error-facts exhausted-err)))))

(def ^:private single-attempt-err
  "A pinned main turn (post cross-provider-fallback removal) fails on ONE provider,
   yet svar still wraps it as `all-providers-exhausted`. The presentation must NOT
   claim the whole fleet was tried."
  {:message "All providers exhausted"
   :data {:type :svar.llm/all-providers-exhausted
          :attempts [{:provider "zai-coding-plan"
                      :model "glm-5.2"
                      :status 500
                      :reason :transient-error
                      :error "boom"}]}})

(defdescribe single-provider-exhausted-test
             (it "one attempt uses Svar's gateway verdict without claiming the fleet was tried"
                 (expect (= :gateway-unavailable
                            (:category (perr/svar-classification single-attempt-err)))))
             (it "mixed attempts keep Svar's unknown verdict while naming the fleet in the title"
                 (expect (= :unknown (:category (perr/svar-classification exhausted-err))))))

(def ^:private provider-unavailable-err
  "svar >= 0.7.55 no longer wraps a one-provider turn as `all-providers-exhausted`:
   it throws `:svar.llm/provider-unavailable` with message `Provider unavailable`
   and the upstream transient's status preserved. Exactly the turn-1 screenshot."
  {:message "Provider unavailable"
   :data {:type :svar.llm/provider-unavailable
          :status 500
          :attempts [{:provider "zai-coding-plan"
                      :model "glm-5.2"
                      :status 500
                      :reason :transient-error
                      :error "boom"}]}})

(defdescribe provider-unavailable-message-test
             (it "uses Svar's gateway verdict for the native single-provider wrapper"
                 (expect (= :gateway-unavailable
                            (:category (perr/svar-classification provider-unavailable-err)))))
             (it "the bare `Provider unavailable` wrapper is NOT repeated as a fact row"
                 (expect (not-any? #(= "Wrapper" (first %))
                                   (perr/provider-error-facts provider-unavailable-err)))))

(def ^:private unroutable-err
  "svar's router giving up BEFORE it sends anything: `select-and-claim!` found no
   candidate at all, so the terminal ex-info is typed `:svar.llm/provider-unavailable`
   and carries ONLY the routing prefs — no status, no body, no attempt row."
  {:message "Provider unavailable"
   :data {:type :svar.llm/provider-unavailable
          :prefs {:force-provider :openai-codex :force-model "gpt-5.6-sol"}
          :tried #{}
          :attempts []}})

;; Regression: this exact card read `Provider unavailable / WHAT HAPPENED: the
;; provider call failed / NEXT STEP: retry; if it persists, switch
;; provider/model` — three sentences that repeat each other, never say the
;; request was never sent, never name the model that could not be routed, and
;; advise the one thing that cannot help (a routing failure has no upstream to
;; retry against until the cooldown ends).
(defdescribe unroutable-presentation-test
             (it "is its own kind, not the catch-all generic card"
                 (expect (= :unknown (:category (perr/svar-classification unroutable-err))))
                 (expect (= :unroutable (perr/provider-error-kind unroutable-err))))
             (it "ships as its own machine code so a bug report can quote it"
                 (let [block (first (perr/provider-error-content unroutable-err))]
                   (expect (= "provider_unroutable" (get block "code")))))
             (it "reads the route from :tried when nothing was pinned"
                 (expect (= "openai-codex"
                            (perr/requested-route {:message "Provider unavailable"
                                                   :data {:type :svar.llm/provider-unavailable
                                                          :tried #{:openai-codex}}})))
                 (expect (nil? (perr/requested-route {:message "Provider unavailable" :data {}}))))
             (it "svar's concrete verdict still wins when the router kept upstream evidence"
                 ;; The router copies the last transient's status/body onto the same typed
                 ;; error; that failure DID reach a provider and must keep its own card.
                 (expect (= :gateway-unavailable
                            (perr/provider-error-kind provider-unavailable-err)))
                 (expect (false? (perr/unroutable-error? provider-unavailable-err)))))

;; Regression: an `:unknown` verdict used to win over the provider-message
;; branch, so a body that said exactly what was wrong was replaced by the bare
;; `the provider call failed`.
(defdescribe
  unknown-category-keeps-provider-words-test
  (it "appends the provider's own message to svar's summary"
      (let [err {:message "closed"
                 :data {:body
                        "{\"error\":{\"message\":\"Your plan does not include this model.\"}}"}}]
        (expect (= :unknown (:category (perr/svar-classification err)))))))

(defdescribe transport-error-test
             ;; A socket that dies before any response byte arrives ("HTTP/1.1 header
             ;; parser received no bytes") is a network/transport failure, NOT a rejection
             ;; — nothing answered, so there is no HTTP status and the model never ran.
             (let [err {:message "HTTP/1.1 header parser received no bytes" :data {}}]
               (it "classifies a no-bytes wrapper failure as :transport, not :generic"
                   (expect (= :transport (perr/provider-error-kind err))))
               (it "does not treat a bare `closed` string as transport evidence"
                   (let [closed-err {:message "closed" :data {}}]
                     (expect (= :unknown (:category (perr/svar-classification closed-err))))
                     (expect (= :generic (perr/provider-error-kind closed-err)))))
               (it "a real HTTP status is a request rejection, not a transport failure"
                   (expect (= :invalid-request
                              (perr/provider-error-kind
                                {:message "Exceptional status code: 400"
                                 :data {:status 400
                                        :body "{\"error\":{\"message\":\"bad\"}}"}}))))))

(defdescribe
  stream-connection-error-test
  ;; A provider can drop a long reasoning stream mid-response. Svar reports that
  ;; as a typed `:svar.core/http-error` with `:stream? true`.
  (let [dropped {:type :svar.core/http-error :stream? true}]
    (it "recognises a dropped stream in ex-data, an exception and iteration error data"
        (expect (true? (perr/stream-connection-error? dropped)))
        (expect (true? (perr/stream-connection-error? (ex-info "Stream connection error: closed"
                                                               dropped))))
        (expect (true? (perr/stream-connection-error? {:message "Stream connection error: closed"
                                                       :data dropped}))))
    (it "ignores failed requests that never streamed and other stream outcomes"
        (expect (false? (perr/stream-connection-error? {:type :svar.core/http-error :status 502})))
        (expect (false? (perr/stream-connection-error? (assoc dropped :stream? false))))
        (expect (false? (perr/stream-connection-error? {:type :svar.core/stream-truncated
                                                        :stream? true})))
        (expect (false? (perr/stream-connection-error? (ex-info "closed" {}))))
        (expect (false? (perr/stream-connection-error? nil))))
    ;; #401: the card said only "the provider call failed" and lost the cause.
    (let [closed {:message "Stream connection error: closed"
                  :data (assoc dropped :stream-recovery {:attempts 0 :declined :output-started})}]
      (it "names a dropped stream on the card and keeps the provider's cause"
          (expect (= :stream-interrupted (perr/provider-error-kind closed)))))))

(defdescribe
  tool-schema-rejection-test
  (let
    [message
     "tools.11.custom.input_schema: input_schema does not support oneOf, allOf, or anyOf at the top level"

     err
     {:message "Exceptional status code: 400"
      :data {:status 400
             :body (str "{\"error\":{\"message\":\"" message "\"}}")
             :tool-index 11
             :tool-name "patch"
             :tool-schema-field "input_schema"
             :tool-schema-path "tools.11.custom.input_schema"}}]

    (it "classifies the deterministic request defect separately from outages"
        (expect (= :tool-schema (perr/provider-error-kind err))))
    (it "renders exact structured facts instead of making the user count array entries"
        (expect (some #{["Tool" "patch"]} (perr/provider-error-facts err)))
        (expect (some #{["Schema" "input_schema"]} (perr/provider-error-facts err)))
        (expect (some #{["Provider path" "tools.11.custom.input_schema"]}
                      (perr/provider-error-facts err))))))

(defdescribe
  gateway-injected-tool-field-test
  (let
    [message
     "litellm.BadRequestError: BedrockException - {\"message\":\"The model returned the following errors: tools.0.custom.strict: Extra inputs are not permitted\"}"

     err
     {:message "Exceptional status code: 400"
      :data {:status 400 :body (str "{\"error\":{\"message\":\"" message "\"}}")}}]

    (it "names the field the GATEWAY injected, which Vis never sends"
        (expect (= "strict" (perr/gateway-tool-field-rejection err))))
    (it "is terminal like any other tool-payload rejection, not an outage"
        (expect (= :tool-schema (perr/provider-error-kind err)))
        (expect (not (perr/provider-error-retryable? err))))))

(defdescribe
  output-budget-too-small-test
  (let
    [message
     "Invalid 'max_output_tokens': integer below minimum value. Expected a value >= 16, but got 8 instead."

     err
     {:message "Exceptional status code: 400"
      :data {:status 400 :body (str "{\"error\":{\"message\":\"" message "\"}}")}}]

    (it "classifies the provider's output-token floor separately from an outage"
        (expect (= :output-budget-too-small (perr/provider-error-kind err))))
    (it "names the minimum the provider asked for and forbids an unchanged retry"
        (expect (not (perr/provider-error-retryable? err))))
    (it "does not hijack unrelated 400s or a max_tokens mention without a floor"
        (expect (not= :output-budget-too-small
                      (perr/provider-error-kind
                        {:message "Exceptional status code: 400"
                         :data {:status 400 :body "{\"error\":{\"message\":\"bad request\"}}"}})))
        (expect (not (perr/output-budget-too-small-error? 400
                                                          "max_tokens exhausted while reasoning")))
        (expect
          (not (perr/output-budget-too-small-error? 500 "max_output_tokens must be at least 16"))))
    (it "reads no minimum when the provider named none"
        (expect (nil? (perr/output-budget-minimum "max_output_tokens is too small"))))))

(defdescribe empty-content-kind-test
             (it "typed :svar.llm/empty-content → honest empty-response card, no 'rejected' wording"
                 (let [err {:message "The model produced neither text nor a tool call"
                            :data {:type :svar.llm/empty-content :empty-reply-resends 2}}]
                   (expect (= :empty-content (perr/provider-error-kind err)))))
             (it "empty-content without svar resend bookkeeping still classifies and reads honestly"
                 (let [err {:message "blank" :data {:type :svar.llm/empty-content}}]
                   (expect (= :empty-content (perr/provider-error-kind err))))))

(defdescribe
  refusal-kind-test
  (it "typed :svar.llm/refusal → 'declined' card with category, not an empty-stall"
      (let [err {:message "The model declined this request."
                 :data {:type :svar.llm/refusal
                        :stop-details {"type" "refusal"
                                       "category" "cyber"
                                       "explanation" "Declined: could enable cyber harm."}}}]
        (expect (= :refusal (perr/provider-error-kind err)))
        ;; distinct from empty-content: never presented as a stall
        (expect (not= :empty-content (perr/provider-error-kind err)))
        ;; deterministic — never advertised as retryable
        (expect (false? (perr/provider-error-retryable? err)))))
  (it "an uncategorised refusal (null category) still classifies and reads honestly"
      (let [err {:message "The model declined this request."
                 :data {:type :svar.llm/refusal
                        :stop-details {"type" "refusal" "category" nil "explanation" nil}}}]
        (expect (= :refusal (perr/provider-error-kind err))))))

(defdescribe
  content-filter-kind-test
  ;; Issue #413: a Responses content_filter stop read as a missing terminal marker.
  (it "names a Responses content_filter stop instead of an interrupted stream"
      (let [e (ex-info "Stream ended with incomplete response, reason: content_filter"
                       {:type :svar.core/stream-incomplete
                        :reason "content_filter"
                        :partial-content "I'm sorry, but I cannot assist with that request."
                        :content-acc-len 49
                        :reasoning-acc-len 0
                        :stream-finalization {:terminal? true
                                              :terminal-kind :done-marker
                                              :last-event-type "response.incomplete"
                                              :incomplete? true
                                              :incomplete-reason "content_filter"
                                              :finish-reason "incomplete"
                                              :http-status 200}})]
        (expect (= :content-filter (perr/provider-error-kind e)))
        (expect (true? (:reached-model? (perr/svar-classification e))))))
  (it "names the flagged categories of a filtered request"
      (let [err {:message "HTTP 400"
                 :data {:status 400
                        :body (str "{\"error\":{\"code\":\"content_filter\","
                                   "\"message\":\"The response was filtered\","
                                   "\"inner_error\":{\"code\":\"ResponsibleAIPolicyViolation\","
                                   "\"content_filter_result\":{\"violence\":"
                                   "{\"filtered\":true,\"severity\":\"medium\"}}}}}")}}]
        (expect (= :content-filter (perr/provider-error-kind err))))))

(defdescribe
  pre-output-stream-abort-test
  "The typed watchdog family Vis may re-issue while nothing has streamed. The set
   itself is svar's; this proves Vis reads it, and reads it in both error shapes."
  (it "recognizes ttft / idle / semantic aborts as ex-info and as a trace error map"
      (doseq [error-type [:svar.core/stream-ttft-timeout :svar.core/stream-idle-timeout
                          :svar.core/stream-semantic-timeout]]
        (expect (true? (perr/pre-output-stream-abort? (ex-info "watchdog" {:type error-type}))))
        (expect (true? (perr/pre-output-stream-abort? {:message "watchdog"
                                                       :data {:type error-type}})))))
  (it "excludes a user cancellation and an ordinary provider failure"
      (expect (false? (perr/pre-output-stream-abort?
                        (ex-info "cancelled" {:type :svar.core/stream-cancelled}))))
      (expect (false? (perr/pre-output-stream-abort? {:data {:status 429}})))))

(defdescribe
  stream-timeout-kind-test
  (it "typed :svar.core/stream-semantic-timeout → honest stall card, never 'Provider unavailable'"
      (let [err {:message "Stream semantic timeout (300000ms without model/progress event): closed"
                 :data {:type :svar.core/stream-semantic-timeout
                        :stream? true
                        :semantic-timeout-ms 300000
                        :cause-class "java.io.IOException"}}]
        (expect (perr/stream-timeout-error? err))
        (expect (= :stream-timeout (perr/provider-error-kind err)))
        (expect (= (boolean (:retryable? (perr/svar-classification err)))
                   (get (first (perr/provider-error-content err)) "retryable")))))
  (it "a real generic failure is untouched"
      (let [err {:message "Provider unavailable" :data {}}]
        (expect (= :generic (perr/provider-error-kind err))))))

(defdescribe
  stream-truncated-presentation-test
  (doseq [[label recovery _]
          [["exhausted" {:attempts 2 :declined :retry-budget-exhausted} "after 2 retries"]
           ["output" {:attempts 0 :declined :output-started} "answer text or tool input"]
           ["one retry" {:attempts 1 :declined :retry-budget-exhausted} "after 1 retry."]]]
    (doseq [throwable? [false true]]
      (it (str label " / throwable=" throwable?)
          (let [data {:type :svar.core/stream-truncated
                      :reasoning-acc-len 10013
                      :content-acc-len 0
                      :stream-recovery recovery}
                err (if throwable?
                      (ex-info "Stream ended before terminal marker." data)
                      {:message "Stream ended before terminal marker." :data data})]

            (expect (= :stream-interrupted (perr/provider-error-kind err))))))))

(defdescribe
  context-overflow-presentation-test
  (let [data
        {:type :svar.tokens/context-overflow
         :source :provider
         :status 400
         :provider-error-code "context_length_exceeded"
         :provider-message "maximum context length exceeded"
         :input-tokens 210000
         :max-input-tokens 200000}

        map-err
        {:message "Provider stream failed" :data data}

        throwable
        (ex-info "Provider stream failed" data)]

    (doseq [[label err] [["trace map" map-err] ["throwable" throwable]]]
      (it (str "recognizes canonical type from " label)
          (expect (true? (perr/context-overflow-error? err)))
          (expect (= :context-overflow (perr/provider-error-kind err)))))
    (it "recognizes the ask-code! PREFLIGHT guard type, not only the tokens variant"
        ;; svar throws `:svar.core/context-overflow` from the `ask-code!` preflight
        ;; guard (llm.clj) and `:svar.tokens/context-overflow` from the token-budget
        ;; checker. VIS took the preflight path, so matching only the tokens variant
        ;; presented a 1.44M-of-1M-token session as `Provider unavailable`.
        (let [err (ex-info "Context overflow: 1437952 tokens exceed limit of 1000000"
                           {:type :svar.core/context-overflow
                            :source :preflight
                            :model "claude-opus-5"
                            :input-tokens 1437952
                            :max-input-tokens 1000000
                            :overflow 437952})]
          (expect (true? (perr/context-overflow-error? err)))
          (expect (= :context-overflow (perr/provider-error-kind err)))))
    (it "supports provider-confirmed overflow without local token counts"
        (let [err {:message "failed"
                   :data {:type :svar.tokens/context-overflow
                          :provider-error-code "prompt_too_long"}}]
          (expect (= :context-overflow (perr/provider-error-kind err)))))
    (it "uses Svar's canonical context verdict even when the typed Vis preflight marker is absent"
        (let [err {:message "context_length_exceeded" :data {:status 400}}]
          (expect (false? (perr/context-overflow-error? err)))
          (expect (= :context-length-exceeded (:category (perr/svar-classification err))))
          (expect (= :context-overflow (perr/provider-error-kind err)))))
    (it "keeps the separate Extra inputs rejection out of context-overflow presentation"
        (let [err {:message "Provider stream failed"
                   :data {:type :svar.core/stream-failed
                          :status 400
                          :provider-error-code "invalid_request_error"
                          :provider-message "Extra inputs are not permitted"}}]
          (expect (= :invalid-request (perr/provider-error-kind err)))))))

;; Regression, issue #105: Vis used to override Svar's 402 quota verdict with
;; a separate local `:billing` classifier.
(defdescribe billing-required-error-test
             (let [err {:message "Exceptional status code: 402"
                        :data {:status 402
                               :body
                               "{\"error\":{\"message\":\"Payment required: add credits\"}}"}}]
               (it "presents HTTP 402 using Svar's canonical terminal quota verdict"
                   (expect (= :quota-exhausted (:category (perr/svar-classification err))))
                   (expect (= :quota-exhausted (perr/provider-error-kind err)))
                   (let [block (first (perr/provider-error-content err))]
                     (expect (= "provider_quota_exhausted" (get block "code")))
                     (expect (false? (get block "retryable")))))))

;; Regression, issue #105: Anthropic's exhausted extra-usage 400 was parsed again in Vis while
;; svar treated it as transient, so nested retries hid the account-limit error.
(defdescribe quota-exhausted-presentation-test
             (let [err {:message "Exceptional status code: 400"
                        :data {:status 400
                               :body (str
                                       "{\"type\":\"error\",\"error\":{\"type\":"
                                       "\"invalid_request_error\",\"message\":"
                                       "\"Third-party apps now draw from your extra usage.\"}}")}}]
               (it "presents svar's canonical quota verdict as a terminal account-limit error"
                   (expect (= :quota-exhausted (:category (perr/svar-classification err))))
                   (expect (= :quota-exhausted (perr/provider-error-kind err)))
                   (expect (false? (perr/provider-error-retryable? err))))
               (it "follows Svar's invalid-request verdict without parsing Anthropic prose"
                   (with-redefs [perr/svar-classification (constantly {:category :invalid-request
                                                                       :retryable? false})]
                     (expect (= :invalid-request (perr/provider-error-kind err)))))))

;; Regression, reported session b30f87ac-f20e-4d7f-9fd2-416788d10527:
;; an Anthropic 400 was titled "Provider unavailable" even though the body named
;; the invalid service-tier value precisely.
;; Regression, reported session 10bb33ec-42b3-42a2-9c29-42956810aae2:
;; provider presentation kinds leaked their kebab-case spelling into machine codes.
(defdescribe
  invalid-request-presentation-test
  (let [err {:message "Exceptional status code: 400"
             :data {:status 400
                    :body (str "{\"type\":\"error\",\"error\":{\"type\":"
                               "\"invalid_request_error\",\"message\":"
                               "\"service_tier: Input should be 'auto' or 'standard_only'\"}}")}}]
    (it "presents the provider's invalid request as a rejection, not an outage"
        (expect (= :invalid-request (:category (perr/svar-classification err))))
        (expect (= :invalid-request (perr/provider-error-kind err)))
        (expect (= "provider_invalid_request"
                   (get (first (perr/provider-error-content err)) "code")))
        (expect (false? (perr/provider-error-retryable? err))))))

;; Regression, issue #105: Svar's model-unavailable verdict used to fall through
;; Vis's legacy prose/status heuristics and become an unrelated generic failure.
(defdescribe svar-category-presentation-test
             (it "maps model availability from Svar without reclassifying the provider prose"
                 (with-redefs [perr/svar-classification
                               (constantly {:category :model-unavailable
                                            :retryable? false
                                            :summary "The requested model is unavailable."
                                            :next-step "Choose another model."})]
                   (let [err {:message "opaque upstream failure" :data {:status 418}}]
                     (expect (= :model-unavailable (perr/provider-error-kind err)))))))

(def ^:private bedrock-timeout-err
  "A gateway timeout that reaches Vis ONLY as prose on the routing attempts —
   the wrapper still says the generic `Provider unavailable` (issue #60)."
  {:message "Provider unavailable"
   :data {:attempts [{:provider "bedrock"
                      :model "claude-opus-4-8"
                      :reason :error
                      :error {:message (str "litellm.Timeout: BedrockException: Timeout Error - "
                                            "litellm.Timeout: Connection timed out. Timeout "
                                            "passed=Timeout(connect=5.0, read=600.0), time "
                                            "taken=0.001 seconds. Received Model "
                                            "Group=claude-opus-4-8")}}]}})

(def ^:private resource-mismatch-err
  "Azure OpenAI refusing an item created under another resource (issue #59)."
  {:message "Provider unavailable"
   :data {:status 400
          :body (str "{\"error\":{\"message\":\"The requested item was created under a different "
                     "Azure OpenAI resource. Use the same resource that created the item to "
                     "access it.\"}}")}})

(defdescribe upstream-timeout-classification-test
             "An UPSTREAM timeout is its own kind — never the generic outage bucket."
             (it "classifies a Bedrock/litellm timeout hidden on the attempts"
                 (expect (= :upstream-timeout (perr/provider-error-kind bedrock-timeout-err))))
             (it "says the request never reached the model on a CONNECT timeout"
                 (expect (= :connect
                            (perr/upstream-timeout-phase nil
                                                         (perr/provider-error-upstream-text
                                                           bedrock-timeout-err)))))
             (it "distinguishes a READ timeout, where the model may have started"
                 (let [err {:message "Provider unavailable" :data {:status 504}}]
                   (expect (= :upstream-timeout (perr/provider-error-kind err)))
                   (expect (= :read (perr/upstream-timeout-phase 504 "")))))
             (it "keeps a timeout retryable"
                 (expect (true? (perr/provider-error-retryable? bedrock-timeout-err))))
             (it "leaves svar's TYPED stream watchdogs alone"
                 (let [err {:message "stream stalled" :data {:type :svar.core/stream-idle-timeout}}]
                   (expect (= :stream-timeout (perr/provider-error-kind err)))))
             (it "does not revive the legacy bare-`closed` transport heuristic"
                 (expect (= :generic (perr/provider-error-kind {:message "closed" :data {}})))))

(def ^:private bedrock-timeout-throwable
  "The SAME failure as `bedrock-timeout-err`, but as the live `ex-info` svar's
   router actually throws: the message is only `Provider unavailable` and the
   real cause rides `:attempts` inside `ex-data`, never a trace entry's `:data`."
  (ex-info "Provider unavailable"
           {:type :svar.llm/provider-unavailable
            :tried [:bedrock]
            :attempts [{:provider :bedrock
                        :model "claude-opus-4-8"
                        :reason :transient-error
                        :error (str "litellm.Timeout: BedrockException: Timeout Error - "
                                    "litellm.Timeout: Connection timed out. Timeout "
                                    "passed=Timeout(connect=5.0, read=600.0), time "
                                    "taken=0.001 seconds. Received Model "
                                    "Group=claude-opus-4-8")}]}))

(defdescribe live-routing-throwable-attempts-test
             "Attempts must be read off a THROWABLE too, not only off a trace entry (#60)."
             (it "classifies the hidden timeout instead of the generic outage"
                 (expect (= :upstream-timeout
                            (perr/provider-error-kind bedrock-timeout-throwable)))))

;; Issue #351: svar's attempts hold keywords, so the error block failed the content
;; contract. A one-shot run then printed "Final answer must be canonical content or
;; Markdown prose" and lost the provider failure.
(defdescribe
  provider-error-content-contract-test
  (it "keeps keyword attempts as canonical wire data"
      (let [blocks (perr/provider-error-content bedrock-timeout-throwable)]
        (expect (every? content-contract/block-valid? blocks))
        (expect (= [{"provider" "bedrock" "model" "claude-opus-4-8" "reason" "transient-error"}]
                   (mapv #(select-keys % ["provider" "model" "reason"])
                         (get (first blocks) "attempts"))))))
  (it "passes as a final answer, so the provider failure reaches the caller"
      (let [blocks (perr/provider-error-content bedrock-timeout-throwable)]
        (expect (= blocks (content/answer-content blocks))))))

(defdescribe resource-mismatch-classification-test
             "A conversation pinned to another backend resource is TERMINAL, not an outage."
             (it "classifies the Azure resource-mismatch body"
                 (expect (= :resource-mismatch (perr/provider-error-kind resource-mismatch-err))))
             (it "never suggests a blind retry"
                 (expect (false? (perr/provider-error-retryable? resource-mismatch-err))))
             (it "marks the typed content non-retryable and kind-specific"
                 (let [[c] (perr/provider-error-content resource-mismatch-err)]
                   (expect (= "provider_resource_mismatch" (get c "code")))
                   (expect (false? (get c "retryable"))))))

(def ^:private status-bearing-drop-err
  "Issue #69: the shared gateway ACCEPTS the connection, then closes it before a
   single response byte — and answers the wrapper with a 502 whose body carries
   the real cause. That status alone used to make vis call it a rejection."
  {:message "Provider unavailable"
   :data {:type :svar.core/http-error
          :status 502
          :body "litellm.APIConnectionError: HTTP/1.1 header parser received no bytes"}})

(defdescribe
  unanswered-request-presentation-test
  (it "classifies a pre-response drop as :transport even when it wears a status"
      (expect (perr/unanswered-request? status-bearing-drop-err))
      (expect (= :transport (perr/provider-error-kind status-bearing-drop-err))))
  (it "reports it as safe to retry, and keeps the gateway's status as a fact"
      (expect (true? (perr/provider-error-retryable? status-bearing-drop-err)))
      (expect (some #(= ["HTTP" "502"] %) (perr/provider-error-facts status-bearing-drop-err))))
  (it "a plain gateway outage with no drop evidence is NOT called a transport drop"
      (let [err {:message "Provider unavailable" :data {:status 502 :body "Bad gateway"}}]
        (expect (not (perr/unanswered-request? err)))
        (expect (not= :transport (perr/provider-error-kind err)))))
  (it "a connect timeout keeps its phase-aware upstream-timeout wording"
      ;; `upstream-timeout-phase` already says the request never reached the
      ;; model, so :connect-timeout must not be swallowed by :transport.
      (let [err {:message "Provider unavailable"
                 :data {:status 408
                        :body (str "litellm.Timeout: BedrockException: Timeout Error - "
                                   "litellm.Timeout: Connection timed out.")}}]
        (expect (not (perr/unanswered-request? err)))
        (expect (= :upstream-timeout (perr/provider-error-kind err)))))
  (it "a definitive client error still reads as a rejection"
      (let [err {:message "Exceptional status code: 400"
                 :data {:status 400
                        :body "{\"error\":{\"message\":\"messages must not be empty\"}}"}}]
        (expect (not (perr/unanswered-request? err))))))

(defdescribe
  correlation-id-presentation-test
  ;; Issue #69's second symptom: the gateway answered with NOTHING but a
  ;; correlation id — no status, no provider message. Calling that "the provider
  ;; rejected the request" is unsupported by any evidence vis holds.
  (let [err {:message "2c0061ee-46c5-4ec4-b6e0-e1f97120e680" :data {}}]
    (it "surfaces the bare id as a Request id fact, not as a wrapper message"
        (let [facts (perr/provider-error-facts err)]
          (expect (some #(= ["Request id" "2c0061ee-46c5-4ec4-b6e0-e1f97120e680"] %) facts))
          (expect (not-any? #(= "Wrapper" (first %)) facts))))
    (it "an explicit request id in the ex-data still wins the row"
        (let [err (assoc-in err [:data :request-id] "req_explicit")]
          (expect (some #(= ["Request id" "req_explicit"] %) (perr/provider-error-facts err))))))
  (it "a real wrapper message is still quoted as the wrapper"
      (let [err {:message "litellm.BadRequestError: deployment is not permitted"
                 :data {:status 400 :body "{\"error\":{\"message\":\"bad\"}}"}}]
        (expect (some #(= "Wrapper" (first %)) (perr/provider-error-facts err))))))

(defdescribe
  retryability-defers-to-svar-test
  ;; Every retryability answer is Svar's `:retryable?`; Vis has no fallback set.
  (it "maps a gateway 503 to Svar's retryable gateway category"
      (let [err {:message "Provider unavailable"
                 :data {:status 503 :body "upstream connect failure"}}]
        (expect (= :gateway-unavailable (:category (perr/svar-classification err))))
        (expect (= :gateway-unavailable (perr/provider-error-kind err)))
        (expect (true? (perr/provider-error-retryable? err)))))
  (it "a definitive 400 is not retryable"
      (expect (false? (perr/provider-error-retryable?
                        {:message "Exceptional status code: 400"
                         :data {:status 400
                                :body
                                "{\"error\":{\"message\":\"messages must not be empty\"}}"}}))))
  ;; Regression: Vis used to label every 429 `:rate-limit`; svar's canonical
  ;; quota verdict must drive both the presentation kind and terminal retry policy.
  (it
    "a quota-exhausted 429 is presented as terminal quota exhaustion"
    (let
      [quota-429
       {:message "Exceptional status code: 429"
        :data
        {:status 429
         :body
         "{\"error\":{\"message\":\"You exceeded your current quota, please check your plan and billing details.\"}}"}}]
      (expect (= :quota-exhausted (perr/provider-error-kind quota-429)))
      (expect (false? (perr/provider-error-retryable? quota-429)))))
  ;; And a bare 429 with no quota body IS still retryable — svar classifies it
  ;; :rate-limited (genuine throttle), not :quota-exhausted.
  (it "a bare 429 with no quota body is still retryable"
      (let [bare-429 {:message "Exceptional status code: 429" :data {:status 429}}]
        (expect (= :rate-limit (perr/provider-error-kind bare-429)))
        (expect (true? (perr/provider-error-retryable? bare-429)))))
  (it "no classification can soften a terminal kind"
      (expect (false? (perr/provider-error-retryable? resource-mismatch-err)))
      (expect (false? (perr/provider-error-retryable? resource-mismatch-err)))))

(def ^:private all-auth-exhausted-err
  "Every provider in the fallback list bowed out on credentials. svar's wrapper
   carries NO status and no provider prose — the 401s live only on `:attempts`."
  {:message "All providers exhausted"
   :data {:type :svar.llm/all-providers-exhausted
          :attempts [{:provider :rbi_genai
                      :model "claude-opus-4-8"
                      :status 401
                      :reason :authentication
                      :error "API authentication failed. Check your API key."}
                     {:provider :openai_fb
                      :model "gpt-5.1"
                      :status 401
                      :reason :authentication
                      :error "Invalid API key provided"}]}})

;; Regression, issue #105: Vis used to inspect attempt status/reason/prose itself
;; instead of consuming Svar's canonical aggregate classification.
(defdescribe
  all-attempts-auth-test
  "An all-401 fleet failure used to classify `:generic` and tell the user to
   `retry, or switch provider/model` — advice that re-sends the same rejected
   key to every provider. The verdict lives on the attempts, not the wrapper."
  (it "classifies the wrapper as :auth from Svar's attempts verdict alone"
      (expect (= {:category :auth :all-attempts-category? true}
                 (select-keys (perr/svar-classification all-auth-exhausted-err)
                              [:category :all-attempts-category?])))
      (expect (= :auth (perr/provider-error-kind all-auth-exhausted-err))))
  (it "is terminal: an identical retry fails identically"
      (expect (false? (perr/provider-error-retryable? all-auth-exhausted-err))))
  (it "a single-provider 401 wrapper keeps the singular headline and pronoun"
      (let [one {:message "Provider unavailable"
                 :data {:type :svar.llm/provider-unavailable
                        :attempts [{:provider :rbi_genai
                                    :model "m"
                                    :status 401
                                    :reason :authentication
                                    :error "unauthorized"}]}}]
        (expect (= :auth (perr/provider-error-kind one)))))
  (it "ONE non-auth attempt keeps the generic fleet wording"
      (expect (= :generic (perr/provider-error-kind exhausted-err))))
  (it "a wrapper with no attempts is no auth verdict"
      (let [bare {:message "Provider unavailable" :data {:type :svar.llm/provider-unavailable}}]
        (expect (not (:all-attempts-category? (perr/svar-classification bare))))
        (expect (not= :auth (perr/provider-error-kind bare)))
        ;; No attempt row and no status: svar sent NOTHING, so this is the
        ;; routing failure, not the catch-all `:generic` card it used to be.
        (expect (= :unroutable (perr/provider-error-kind bare)))))
  (it "a timeout fleet is still an upstream timeout, not auth"
      (let [t {:message "All providers exhausted"
               :data {:type :svar.llm/all-providers-exhausted
                      :attempts [{:provider :a
                                  :model "m"
                                  :status 408
                                  :reason :transient-error
                                  :error "litellm.Timeout: BedrockException: Timeout Error"}]}}]
        (expect (= :upstream-timeout (perr/provider-error-kind t)))
        (expect (perr/provider-error-retryable? t))))
  (it "lets Svar classify auth prose retained only on an attempt"
      (let [p
            {:message "All providers exhausted"
             :data {:type :svar.llm/all-providers-exhausted
                    :attempts [{:provider :a
                                :model "m"
                                :reason :error
                                :error "Authentication failed: invalid api key"}]}}

            classification
            (perr/svar-classification p)]

        (expect (= :auth (:category classification)))
        (expect (:all-attempts-category? classification))
        (expect (= [:a] (perr/auth-failed-provider-ids p))))))

(defdescribe failed-turn-masking-test
             ;; Regression: a turn that dies on provider exhaustion (Anthropic 529
             ;; overloaded, retries spent) hands back a NON-answer-shaped fallback. The old
             ;; path ran that through answer-content, threw "Final answer must be canonical
             ;; content or Markdown prose", and that misleading string MASKED the real
             ;; provider failure (which lives as the last error in the turn trace). The fix
             ;; surfaces the trace's provider error for both the card and the headline.
             (it "the real 529 provider error surfaces, not the canonical-content string"
                 (let [terminal-error
                       {:message "Provider stream failed (overloaded_error): Overloaded"
                        :data
                        {:type :svar.core/stream-failed :status 529 :provider-message "Overloaded"}
                        :status 529}

                       card
                       (first (perr/provider-error-content terminal-error))]

                   ;; classifies as a genuine provider failure, not :generic
                   (expect (not= :generic (perr/provider-error-kind terminal-error)))
                   ;; the card is a real provider_ error block, not a bare turn_failed
                   (expect (= "error" (get card "type")))))
             (it "answer-content still rejects a non-answer-shaped fallback (the masking trigger)"
                 ;; documents WHY the guard exists: a raw non-answer value on the failure
                 ;; path throws here, so the loop must NOT let that throw escape send!.
                 (require 'com.blockether.vis.internal.content)
                 (let [answer-content
                       (resolve 'com.blockether.vis.internal.content/answer-content)

                       outcome
                       (try (answer-content {:overloaded true :status 529})
                            ::no-throw
                            (catch clojure.lang.ExceptionInfo _ ::threw))]

                   (expect (= ::threw outcome)))))

(defdescribe
  fd-exhaustion-kind-test
  ;; Regression: vis session 7d3f9026 — a sandbox walk holding hundreds of open
  ;; handles exhausted the JVM's shared descriptor table. That broke the OAuth refresh
  ;; SOCKET, so the turn failed and surfaced as a generic "Provider unavailable"
  ;; — masking the real cause. The FD signature must be recognised wherever it
  ;; tunnels (message, body, or cause) and named honestly.
  (it "an FD-exhaustion tunnelled through the provider path is named, not masked"
      (let [err {:message "Provider unavailable"
                 :data
                 {:type :svar.llm/provider-unavailable
                  :body
                  "git run-git failed ... Bad file descriptor, error: 24 (Too many open files)"}}]
        (expect (= :file-descriptors-exhausted (perr/provider-error-kind err)))))
  (it "matches the bare `Too many open files` phrase too"
      (let [err {:message "java.io.IOException: Too many open files" :data {}}]
        (expect (= :file-descriptors-exhausted (perr/provider-error-kind err)))))
  (it "does NOT trip on an ordinary provider failure with no FD signature"
      (let [err {:message "Provider unavailable"
                 :data {:type :svar.llm/provider-unavailable :status 500}}]
        (expect (not= :file-descriptors-exhausted (perr/provider-error-kind err))))))

;; Regression, vis session f2cfccd5: a stale `py` global restored into the session
;; sandbox made every tool bind throw, and the card called the local worker's death
;; "Provider unavailable" while the loop re-asked the model for two hours.
(defdescribe
  python-worker-kind-test
  (it "a failed tool bind in the local worker is named, not blamed on the provider"
      (let [err {:message
                 "vis-python: ValueError: extension tool namespace collides with global: py"
                 :data {:type :vis/python-worker :op "set-tools"}}]
        (expect (= :python-worker (perr/provider-error-kind err)))
        ;; It has to reach the styled card at all: `provider-failure?` is what the
        ;; loop and the gateway ask before they paint anything but a bare message.
        (expect (perr/provider-failure? err))
        (expect (false? (perr/provider-error-retryable? err)))))
  (it "reads the typed exception the worker actually throws"
      (doseq [error-type [:vis/python-worker :vis/python-worker-missing :vis/python-worker-retired
                          :vis/python-worker-timeout]]
        (expect (= :python-worker
                   (perr/provider-error-kind (ex-info "the python worker did not answer exec"
                                                      {:type error-type :op "exec"}))))))
  (it "does NOT trip on an ordinary provider failure"
      (expect (not= :python-worker
                    (perr/provider-error-kind {:message "Provider unavailable"
                                               :data {:type :svar.llm/provider-unavailable
                                                      :status 500}})))))

;; The live wire fact this predicate exists to catch: a gateway proxying six models
;; models.dev lists as taking image input answers HTTP 400 for every one of them,
;; because the gateway's OWN request schema has no image variant. The capability
;; tables describe the model, never the endpoint.
(defdescribe
  image-rejection-scope-test
  "`image-rejection-scope`: whether the image could not be carried at all, and how
   far that answer generalizes — the endpoint's whole wire, or one model's name."
  (it "reads a gateway whose request schema has no image variant as :wire"
      (let [err {:message "Exceptional status code: 400"
                 :data {:status 400
                        :body (str "{\"error\":{\"message\":\"Error from provider (Console Go): "
                                   "unknown variant `image_url`, expected `text`\"}}")}}]
        (expect (= :wire (perr/image-rejection-scope err)))
        (expect (perr/image-input-rejection? err))))
  ;; The expensive mistake in the other direction: a provider serving one small
  ;; text-only model alongside its seeing ones would lose the whole fleet's eyes
  ;; if this read as :wire.
  (it "reads a refusal that names the MODEL as :model"
      (expect (= :model
                 (perr/image-rejection-scope {:message "This model does not support image input"
                                              :data {:status 400}})))
      (expect (= :model
                 (perr/image-rejection-scope {:message "Bad request"
                                              :data {:status 422
                                                     :body "image_url is not supported here"}})))
      (expect (= :model
                 (perr/image-rejection-scope {:message "Vision is not supported for this model"
                                              :data {:status 400}}))))
  (it "reads it from a bare ex-info, not just the trace-entry shape"
      (expect (= :wire
                 (perr/image-rejection-scope (ex-info "unknown variant `image_url`, expected `text`"
                                                      {:status 400})))))
  ;; A false positive costs a provider its eyes for the whole process, so an
  ;; ordinary 400 and an outage carrying the same words must not match.
  (it "ignores a 400 that is about something else"
      (expect (nil? (perr/image-rejection-scope
                      {:message "Exceptional status code: 400"
                       :data {:status 400
                              :body "{\"error\":\"messages: at least one required\"}"}}))))
  (it "ignores a rejected image that is merely too large"
      (expect (nil? (perr/image-rejection-scope {:message "image exceeds the 5MB limit"
                                                 :data {:status 400}}))))
  (it "ignores the same wording on a 5xx, which is an outage, not a schema"
      (expect (nil? (perr/image-rejection-scope {:message "unknown variant `image_url`"
                                                 :data {:status 500}}))))
  (it "ignores an empty error"
      (expect (nil? (perr/image-rejection-scope {})))
      (expect (not (perr/image-input-rejection? {})))
      (expect (nil? (perr/image-rejection-scope {:message "" :data {:status 400}})))))

;; The `all-providers-exhausted` shape: the turn's own ask fails as the SECOND
;; provider, so the rejection is one row among several and only its row counts.
(defdescribe
  image-rejections-test
  "`image-rejections`: what a failure proves about carrying images, per row."
  (it "picks the rejecting provider out of an exhausted attempt list"
      (let [err {:message "All providers exhausted"
                 :data {:type :svar.llm/all-providers-exhausted
                        :attempts [{:provider "opencode-go"
                                    :model "mimo-v2.5"
                                    :status 400
                                    :error (str "Error from provider (Console Go): unknown variant "
                                                "`image_url`, expected `text`")}
                                   {:provider "anthropic"
                                    :model "claude-opus-4-8"
                                    :status 429
                                    :error "rate limited"}]}}]
        (expect (= #{{:provider :opencode-go :model "mimo-v2.5" :scope :wire}}
                   (perr/image-rejections err)))))
  (it "keeps a model-shaped refusal off its provider's account"
      (let [err
            {:message "All providers exhausted"
             :data {:type :svar.llm/all-providers-exhausted
                    :attempts [{:provider "openai"
                                :model "gpt-5.4-nano"
                                :status 400
                                :error "The model gpt-5.4-nano does not support image input"}]}}

            rows
            (perr/image-rejections err)]

        (expect (= #{{:provider :openai :model "gpt-5.4-nano" :scope :model}} rows))
        ;; The provider is NOT named as blind anywhere in the row set.
        (expect (empty? (filter #(= :wire (:scope %)) rows)))))
  (it "picks a lone provider that names itself"
      (expect (= #{{:provider :opencode-go :model "mimo-v2.5" :scope :wire}}
                 (perr/image-rejections
                   (ex-info "unknown variant `image_url`, expected `text`"
                            {:status 400 :provider-id :opencode-go :model "mimo-v2.5"})))))
  (it "answers nothing for failures that are not about image parts"
      (expect (empty? (perr/image-rejections (ex-info "rate limited"
                                                      {:status 429 :provider-id :anthropic}))))
      (expect (empty? (perr/image-rejections {})))
      (expect (empty? (perr/image-rejections exhausted-err)))))
