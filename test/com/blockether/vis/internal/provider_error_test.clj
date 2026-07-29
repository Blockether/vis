(ns com.blockether.vis.internal.provider-error-test
  "Provider-error presentation and canonical typed error content."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.provider-error :as perr]
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

(defdescribe
  provider-error-attempts-test
  (it "reads the per-provider attempts from the ex-data"
      (expect (= 2 (count (perr/provider-error-attempts exhausted-err))))
      (expect (= "anthropic" (:provider (first (perr/provider-error-attempts exhausted-err))))))
  (it "one attempt formats as provider/model: status reason"
      (expect (= "anthropic/claude-opus-4: 429 rate-limit"
                 (perr/attempt->line (first (perr/provider-error-attempts exhausted-err))))))
  (it "the summary joins every attempt with ·"
      (expect (= "anthropic/claude-opus-4: 429 rate-limit · openai/gpt-5: 401 auth"
                 (perr/provider-error-attempts-summary exhausted-err))))
  (it "no attempts (older svar / non-routing failure) → empty + nil summary"
      (let
        [bare {:message "All providers exhausted" :data {:type :svar.llm/all-providers-exhausted}}]
        (expect (empty? (perr/provider-error-attempts bare)))
        (expect (nil? (perr/provider-error-attempts-summary bare)))))
  (it "title for exhausted is the specific headline"
      (expect (= "All providers unavailable" (perr/provider-error-title exhausted-err))))
  (it "emits one structured provider error block"
      (let
        [blocks
         (perr/provider-error-content exhausted-err)

         block
         (first blocks)]

        (expect (= 1 (count blocks)))
        (expect (= "error" (get block "type")))
        (expect (= 2 (count (get block "attempts"))))
        (expect (str/includes? (get block "message") "All providers unavailable"))))
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
             (it "one attempt does NOT claim the whole fleet was tried"
                 (expect (= "Provider unavailable" (perr/provider-error-title single-attempt-err)))
                 (let [ex (perr/provider-error-explanation single-attempt-err)]
                   (expect (str/includes? ex "selected provider"))
                   (expect (not (str/includes? ex "every provider"))))
                 (expect (str/includes? (perr/provider-error-next-step single-attempt-err)
                                        "switch provider/model"))
                 ;; Surface-agnostic: the card must not assume a TUI or a shell.
                 (expect (nil? (re-find #"(?i)ctrl\\+k|`vis "
                                        (perr/provider-error-next-step single-attempt-err)))))
             (it "two+ attempts still read as the fleet-wide exhaustion"
                 (expect (= "All providers unavailable" (perr/provider-error-title exhausted-err)))
                 (expect (str/includes? (perr/provider-error-explanation exhausted-err)
                                        "every provider"))))

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
             (it "the native single-provider message reads as one provider, not the fleet"
                 (expect (= "Provider unavailable"
                            (perr/provider-error-title provider-unavailable-err)))
                 (let [ex (perr/provider-error-explanation provider-unavailable-err)]
                   (expect (str/includes? ex "selected provider"))
                   (expect (not (str/includes? ex "every provider"))))
                 (expect (str/includes? (perr/provider-error-next-step provider-unavailable-err)
                                        "switch provider/model")))
             (it "the bare `Provider unavailable` wrapper is NOT repeated as a fact row"
                 (expect (not-any? #(= "Wrapper" (first %))
                                   (perr/provider-error-facts provider-unavailable-err)))))

(defdescribe transport-error-test
             ;; A socket that dies before any response byte arrives ("HTTP/1.1 header
             ;; parser received no bytes") is a network/transport failure, NOT a rejection
             ;; — nothing answered, so there is no HTTP status and the model never ran.
             (let [err {:message "HTTP/1.1 header parser received no bytes" :data {}}]
               (it "classifies a no-bytes wrapper failure as :transport, not :generic"
                   (expect (= :transport (perr/provider-error-kind err))))
               (it "titles it as an unreachable provider"
                   (expect (= "Could not reach provider" (perr/provider-error-title err))))
               (it "the explanation does NOT falsely claim the provider rejected the request"
                   (let [ex (perr/provider-error-explanation err)]
                     (expect (str/includes? ex "connection dropped"))
                     (expect (not (str/includes? ex "rejected the request")))))
               (it "the next step tells the user to just retry"
                   (expect (str/includes? (perr/provider-error-next-step err) "retry")))
               (it "classifies the terse `closed` wrapper as transport"
                   (let [closed-err {:message "closed" :data {}}]
                     (expect (= :transport (perr/provider-error-kind closed-err)))
                     (expect (= "Could not reach provider" (perr/provider-error-title closed-err)))
                     (expect (str/includes? (perr/provider-error-explanation closed-err)
                                            "connection dropped"))))
               (it "a real HTTP status is NOT mistaken for a transport failure"
                   (expect (= :generic
                              (perr/provider-error-kind
                                {:message "Exceptional status code: 400"
                                 :data {:status 400
                                        :body "{\"error\":{\"message\":\"bad\"}}"}}))))))

(defdescribe
  transport-throwable-test
  ;; `transport-throwable?` is the RETRY gate's classifier: a Throwable is
  ;; retry-safe iff it's a CONNECTION/transport failure (no response byte
  ;; received), regardless of whether a stream had started. It shares the
  ;; exact patterns `transport-error?` applies to the human message, so the
  ;; "just retry" advice and the actual retry can never disagree again (the
  ;; disagreement that made a failed turn NOT retry while telling the user
  ;; it would).
  (it "retries a socket that closed before any response byte"
      (expect (perr/transport-throwable? (ex-info "HTTP/1.1 header parser received no bytes" {}))))
  (it "retries a TTFT timeout before response headers"
      (expect (perr/transport-throwable?
                (ex-info "Stream TTFT timeout (60000ms with no response headers)" {}))))
  (it "walks the cause chain for a wrapped transport failure"
      (expect (perr/transport-throwable? (ex-info "provider call failed"
                                                  {}
                                                  (ex-info "header parser received no bytes" {})))))
  (it "retries a connection reset / DNS failure"
      (expect (perr/transport-throwable? (ex-info "java.net.SocketException: Connection reset" {})))
      (expect (perr/transport-throwable? (ex-info "java.net.UnknownHostException: api.host" {}))))
  (it "does NOT retry a real rejection — 429/400 carry an HTTP status"
      (expect (not (perr/transport-throwable? (ex-info "rate limited" {:status 429}))))
      (expect (not (perr/transport-throwable? (ex-info "bad request" {:status 400})))))
  (it "is nil-safe" (expect (not (perr/transport-throwable? nil)))))

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
        (expect (= :tool-schema (perr/provider-error-kind err)))
        (expect (= "Native tool schema rejected: patch" (perr/provider-error-title err))))
    (it "names the exact tool and schema, then forbids an unchanged retry"
        (expect (re-find #"`patch`" (perr/provider-error-explanation err)))
        (expect (re-find #"`input_schema`" (perr/provider-error-explanation err)))
        (expect (re-find #"top-level" (perr/provider-error-explanation err)))
        (expect (re-find #"schema defect" (perr/provider-error-explanation err)))
        (expect (re-find #"update Vis or disable" (perr/provider-error-next-step err)))
        (expect (not (re-find #"transient" (perr/provider-error-next-step err)))))
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
        (expect (not (perr/provider-error-retryable? err))))
    (it "blames the gateway instead of a Vis extension schema"
        (expect (= "Gateway sent an unsupported tool field: strict"
                   (perr/provider-error-title err)))
        (expect (re-find #"never sends that field" (perr/provider-error-explanation err)))
        (expect (re-find #"gateway" (perr/provider-error-next-step err)))
        (expect (not (re-find #"disable the offending extension"
                              (perr/provider-error-next-step err)))))))

(defdescribe
  output-budget-too-small-test
  (let
    [message
     "Invalid 'max_output_tokens': integer below minimum value. Expected a value >= 16, but got 8 instead."

     err
     {:message "Exceptional status code: 400"
      :data {:status 400 :body (str "{\"error\":{\"message\":\"" message "\"}}")}}]

    (it "classifies the provider's output-token floor separately from an outage"
        (expect (= :output-budget-too-small (perr/provider-error-kind err)))
        (expect (= "Output token budget too small" (perr/provider-error-title err))))
    (it "names the minimum the provider asked for and forbids an unchanged retry"
        (expect (re-find #"at least 16" (perr/provider-error-explanation err)))
        (expect (re-find #"request defect" (perr/provider-error-explanation err)))
        (expect (re-find #"max_output_tokens" (perr/provider-error-next-step err)))
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
                 (let
                   [err {:message "The model produced neither text nor a tool call"
                         :data {:type :svar.llm/empty-content :empty-reply-resends 2}}]
                   (expect (= :empty-content (perr/provider-error-kind err)))
                   (expect (= "Model returned an empty response" (perr/provider-error-title err)))
                   (expect (re-find #"no text and no tool" (perr/provider-error-explanation err)))
                   (expect (re-find #"Re-sent 2 times" (perr/provider-error-explanation err)))
                   (expect (nil? (re-find #"(?i)rejected" (perr/provider-error-explanation err))))
                   (expect (re-find #"Vis re-sends" (perr/provider-error-next-step err)))))
             (it "empty-content without svar resend bookkeeping still classifies and reads honestly"
                 (let [err {:message "blank" :data {:type :svar.llm/empty-content}}]
                   (expect (= :empty-content (perr/provider-error-kind err)))
                   (expect (nil? (re-find #"(?i)rejected"
                                          (perr/provider-error-explanation err)))))))

(defdescribe
  stream-timeout-kind-test
  (it "typed :svar.core/stream-semantic-timeout → honest stall card, never 'Provider unavailable'"
      (let
        [err
         {:message "Stream semantic timeout (300000ms without model/progress event): closed"
          :data {:type :svar.core/stream-semantic-timeout
                 :stream? true
                 :semantic-timeout-ms 300000
                 :cause-class "java.io.IOException"}}

         next-step
         (perr/provider-error-next-step err)]

        (expect (perr/stream-timeout-error? err))
        (expect (= :stream-timeout (perr/provider-error-kind err)))
        (expect (= "Stream went quiet — Vis timed out" (perr/provider-error-title err)))
        (expect (nil? (re-find #"(?i)provider unavailable" (perr/provider-error-title err))))
        ;; The pre-fix copy claimed a pre-model REJECTION and pointed at auth/quota.
        (expect (nil? (re-find #"(?i)rejected the request" (perr/provider-error-explanation err))))
        (expect (re-find #"Nothing was rejected" (perr/provider-error-explanation err)))
        (expect (re-find #"300s" (perr/provider-error-explanation err)))
        (expect (re-find #"still reasoning" (perr/provider-error-explanation err)))
        (expect (nil? (re-find #"(?i)auth, and quota" next-step)))
        ;; Cross-validated vs Codex: svar never same-provider-retries a watchdog abort
        ;; (deliberate-stream-abort-types), but the gateway queue DOES auto-retry a
        ;; :stream-timeout as transient. The copy stays surface-agnostic: no CLI
        ;; commands, no keybindings, no internal var names.
        (expect (re-find #"retrying automatically" next-step))
        (expect (re-find #"semantic-timeout-ms" next-step))
        (expect (re-find #"set it to `nil`" next-step))
        (expect (nil? (re-find #"idle-timeout-ms" next-step)))
        (expect (true? (get (first (perr/provider-error-content err)) "retryable")))))
  (it "the idle-timeout sibling classifies the same way"
      (let
        [err {:message "Stream idle timeout (180000ms with no bytes)."
              :data {:type :svar.core/stream-idle-timeout :idle-timeout-ms 180000}}]
        (expect (= :stream-timeout (perr/provider-error-kind err)))
        (expect (re-find #"180s" (perr/provider-error-explanation err)))))
  (it "a real generic failure is untouched"
      (let [err {:message "Provider unavailable" :data {}}]
        (expect (= :generic (perr/provider-error-kind err)))
        (expect (= "Provider unavailable" (perr/provider-error-title err))))))

(defdescribe
  context-overflow-presentation-test
  (let
    [data
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
          (expect (= :context-overflow (perr/provider-error-kind err)))
          (expect (= "Context window exceeded" (perr/provider-error-title err))))
      (it (str "renders measured limits from " label)
          (let [explanation (perr/provider-error-explanation err)]
            (expect (str/includes? explanation "210000"))
            (expect (str/includes? explanation "200000"))))
      (it (str "never recommends unchanged retry for " label)
          (let [next-step (perr/provider-error-next-step err)]
            (expect (str/includes? next-step "fold older settled history"))
            (expect (str/includes? next-step "larger-context model"))
            (expect (not (re-find #"(?i)next step: retry" next-step))))))
    (it "supports provider-confirmed overflow without local token counts"
        (let
          [err {:message "failed"
                :data {:type :svar.tokens/context-overflow :provider-error-code "prompt_too_long"}}]
          (expect (= :context-overflow (perr/provider-error-kind err)))
          (expect (str/includes? (perr/provider-error-explanation err) "context window"))))
    (it "does not classify matching prose without the canonical type"
        (let [err {:message "context_length_exceeded" :data {:status 400}}]
          (expect (false? (perr/context-overflow-error? err)))
          (expect (= :generic (perr/provider-error-kind err)))))
    (it "keeps the separate Extra inputs request-schema failure generic"
        (let
          [err {:message "Provider stream failed"
                :data {:type :svar.core/stream-failed
                       :status 400
                       :provider-error-code "invalid_request_error"
                       :provider-message "Extra inputs are not permitted"}}]
          (expect (= :generic (perr/provider-error-kind err)))
          (expect (not= "Context window exceeded" (perr/provider-error-title err)))))))

(defdescribe
  billing-required-error-test
  (let
    [err {:message "Exceptional status code: 402"
          :data {:status 402 :body "{\"error\":{\"message\":\"Payment required: add credits\"}}"}}]
    (it "turns HTTP 402 into a clear, non-retryable billing card"
        (expect (= :billing (perr/provider-error-kind err)))
        (expect (= "Provider billing required" (perr/provider-error-title err)))
        (expect (str/includes? (perr/provider-error-explanation err) "requires payment"))
        (expect (str/includes? (perr/provider-error-next-step err) "billing and available credits"))
        (let [block (first (perr/provider-error-content err))]
          (expect (= "provider_billing" (get block "code")))
          (expect (false? (get block "retryable")))))))

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

(defdescribe
  upstream-timeout-classification-test
  "An UPSTREAM timeout is its own kind — never the generic outage bucket."
  (it "classifies a Bedrock/litellm timeout hidden on the attempts"
      (expect (= :upstream-timeout (perr/provider-error-kind bedrock-timeout-err))))
  (it "titles it as a timeout, not as `Provider unavailable`"
      (expect (= "Provider request timed out" (perr/provider-error-title bedrock-timeout-err)))
      (expect (nil? (re-find #"(?i)provider unavailable"
                             (perr/provider-error-title bedrock-timeout-err)))))
  (it "says the request never reached the model on a CONNECT timeout"
      (expect (= :connect
                 (perr/upstream-timeout-phase nil
                                              (perr/provider-error-upstream-text
                                                bedrock-timeout-err))))
      (expect (str/includes? (perr/provider-error-explanation bedrock-timeout-err)
                             "never reached the model")))
  (it "distinguishes a READ timeout, where the model may have started"
      (let [err {:message "Provider unavailable" :data {:status 504}}]
        (expect (= :upstream-timeout (perr/provider-error-kind err)))
        (expect (= :read (perr/upstream-timeout-phase 504 "")))
        (expect (str/includes? (perr/provider-error-explanation err) "may have started"))))
  (it "keeps a timeout retryable"
      (expect (true? (perr/provider-error-retryable? bedrock-timeout-err))))
  (it "leaves svar's TYPED stream watchdogs alone"
      (let [err {:message "stream stalled" :data {:type :svar.core/stream-idle-timeout}}]
        (expect (= :stream-timeout (perr/provider-error-kind err)))))
  (it "leaves a bare transport drop alone"
      (expect (= :transport (perr/provider-error-kind {:message "closed" :data {}})))))

(defdescribe resource-mismatch-classification-test
             "A conversation pinned to another backend resource is TERMINAL, not an outage."
             (it "classifies the Azure resource-mismatch body"
                 (expect (= :resource-mismatch (perr/provider-error-kind resource-mismatch-err))))
             (it "titles the pinning instead of a generic outage"
                 (expect (= "Conversation pinned to another provider resource"
                            (perr/provider-error-title resource-mismatch-err))))
             (it "explains that an identical retry fails identically"
                 (expect (str/includes? (perr/provider-error-explanation resource-mismatch-err)
                                        "identical retry fails identically")))
             (it "never suggests a blind retry"
                 (let [next-step (perr/provider-error-next-step resource-mismatch-err)]
                   (expect (str/includes? next-step "don't retry as-is"))
                   (expect (false? (perr/provider-error-retryable? resource-mismatch-err)))))
             (it "marks the typed content non-retryable and kind-specific"
                 (let [[c] (perr/provider-error-content resource-mismatch-err)]
                   (expect (= "provider_resource-mismatch" (get c "code")))
                   (expect (false? (get c "retryable"))))))
