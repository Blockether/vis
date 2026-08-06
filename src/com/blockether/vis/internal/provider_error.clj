(ns com.blockether.vis.internal.provider-error
  "Single source of truth for provider-error presentation.

   Typed provider-error content and per-iteration trace rows derive their wording
   and facts from this namespace, so a failure reads identically everywhere.

   `err` is the error map carried on a trace entry / ex-info:
   `{:message .. :data {:status .. :body .. :request-id ..} ..}`. Every
   helper tolerates the bare ex-info shape too (via `ex-message`).

   CLASSIFICATION IS SVAR'S. `svar-classification` wraps
   `svar.internal.failure/classify` — the single owner of failure families,
   retry safety and `:reached-model?` for everything svar transports. This
   namespace owns WORDING, plus the handful of failures svar cannot see (its
   typed empty-content and stream-watchdog outcomes, gateway tool-field
   rejections, tool-schema defects). Never grow a second copy of svar's
   heuristics here."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.svar.internal.failure :as failure]
            [com.blockether.vis.internal.content :as content]
            [com.blockether.vis.internal.strutil :refer [truncate]]))

(def ^:private CHAT_ERROR_BODY_RENDER_CHARS
  "Cap on raw upstream HTTP body chars surfaced in the chat error bubble.
   Long enough that full JSON error envelopes round-trip whole, short
   enough that a pathological 5xx HTML page can't take over the
   transcript."
  4000)

(defn parse-provider-body
  [body]
  (when (and (string? body) (not (str/blank? body)))
    (try (json/read-json body :key-fn keyword) (catch Throwable _ nil))))

(defn provider-structured-message
  "Human-readable message a provider put in its error envelope, if any.
   Covers Anthropic/OpenAI `{:error {:message}}`, bare `{:message}`, and
   Codex/ChatGPT `{:detail}`. nil when the body has no message field (HTML
   page, bare 5xx) — the caller then surfaces the raw body instead."
  [body]
  (let [parsed (parse-provider-body body)]
    (some-> (or (get-in parsed [:error :message])
                (:message parsed)
                (:detail parsed)
                (get-in parsed [:error :detail]))
            str
            str/trim
            not-empty)))

(defn provider-body-message
  [body]
  (or (provider-structured-message body)
      (some-> body
              str/trim
              not-empty)))

(defn generic-wrapper-message?
  "True when the svar wrapper message is just a status echo (e.g.
   `Exceptional status code: 400`) that adds nothing beyond the HTTP row."
  [message]
  (boolean (when-let
             [t (some-> message
                        str
                        str/lower-case)]
             (or (str/includes? t "exceptional status code")
                 (str/includes? t "provider error http")
                 ;; the title already says "All providers unavailable" and the
                 ;; per-provider attempts carry the detail — the bare wrapper echo
                 ;; adds nothing, so don't repeat it as a fact row.
                 (str/includes? t "all providers exhausted")
                 ;; svar's single-provider terminal error (new in 0.7.55) — the
                 ;; title + next-step already say it; the bare echo adds nothing.
                 (str/includes? t "provider unavailable")))))

(defn invalid-thinking-signature-message?
  [message]
  (boolean (and (string? message) (re-find #"(?i)invalid.*signature.*thinking.*block" message))))

(defn tool-schema-rejection-message?
  "True when a provider rejected a native tool because its input-schema root
   uses a JSON Schema union that provider tool APIs forbid."
  [message]
  (let
    [text (some-> message
                  str/lower-case)]
    (boolean (and text
                  (or (str/includes? text "input_schema") (str/includes? text "input schema"))
                  (str/includes? text "does not support")
                  (str/includes? text "top level")
                  (some #(str/includes? text %) ["oneof" "allof" "anyof"])))))

(def ^:private GATEWAY_INJECTED_TOOL_FIELD_PATTERN
  "Provider path of a tool-payload field Vis does NOT emit, e.g.
   `tools.0.custom.strict`."
  #"(?i)tools\.\d+(?:\.(?:custom|function))?\.(strict|additionalProperties)")

(def ^:private GATEWAY_FIELD_REJECTION_PATTERN
  #"(?i)extra inputs are not permitted|unrecognized request argument|unknown field|is not permitted")

(defn gateway-injected-tool-field
  "The tool-payload FIELD an OpenAI-compatible gateway added on Vis' behalf and
   the upstream then rejected, lower-cased — or nil.

   Vis never emits `strict` nor a tool-level `additionalProperties`: LiteLLM
   forwards them into the Bedrock Converse `toolSpec` while translating our tool
   defs, and Bedrock's Anthropic-compatible validator answers
   `tools.0.custom.strict: Extra inputs are not permitted`. So this is NOT a
   defect in a Vis tool schema — no schema edit can avoid it, and an unchanged
   retry fails identically."
  [message]
  (let [text (str message)]
    (when (re-find GATEWAY_FIELD_REJECTION_PATTERN text)
      (some-> (re-find GATEWAY_INJECTED_TOOL_FIELD_PATTERN text)
              second
              str/lower-case))))

(defn gateway-tool-field-rejection
  "`gateway-injected-tool-field` over an error's own body + message."
  [err]
  (let [data (or (:data err) (ex-data err) err)]
    (gateway-injected-tool-field (str (some-> (:body data)
                                              str)
                                      "\n"
                                      (or (ex-message err) (:message err) (str err))))))

(defn output-budget-too-small-error?
  "True when a provider rejected the request because the OUTPUT-token budget it
   was given is below that model's minimum — e.g. `gpt-5.6-terra` 400s on
   `max_output_tokens: 8` but accepts `16`. Deterministic like a schema defect:
   an unchanged retry fails identically, raising the configured floor fixes it."
  [status message]
  (let
    [text (some-> message
                  str/lower-case)]
    (boolean (and text
                  (= 400 status)
                  (some #(str/includes? text %)
                        ["max_output_tokens" "max output tokens" "max_tokens" "max tokens"])
                  (re-find #"minimum|at least|too small|greater than or equal|>=|must be greater"
                           text)))))

(defn output-budget-minimum
  "The minimum output-token count the provider NAMED in its rejection, when it
   named one (`Expected a value >= 16`, `must be at least 16`). nil otherwise."
  [message]
  (some->
    (re-find
      #"(?i)(?:>=|greater than or equal to|at least|minimum(?:\s+value)?(?:\s+of|\s+is)?)\s*(\d+)"
      (str message))
    second
    parse-long))

(defn- ->classifiable
  "`err` as the Throwable svar's classifier expects. A trace-entry error map is
   the same evidence wearing a map, so rebuild the ex-info instead of
   re-deriving svar's heuristics over here."
  ^Throwable [err]
  (cond (instance? Throwable err) err
        (map? err) (ex-info (str (or (:message err) (ex-message err)))
                            (or (:data err) (ex-data err) err))
        :else (ex-info (str err) {})))

(defn svar-classification
  "svar's canonical verdict for `err`: `{:category :retryable? :reached-model?
   :request-id :status …}` from `svar.internal.failure/classify`.

   Vis presents, svar classifies. Reading this instead of re-implementing it is
   what keeps the two layers from disagreeing about whether the model ever saw
   the request — and svar, not vis, owns the retry ladder that follows."
  [err]
  (failure/classify (->classifiable err)))

(defn unanswered-request?
  "True when Svar says nothing reached the model: a canonical pre-response
   `:transport-drop`. A connect timeout remains distinct so presentation can
   explain that phase precisely."
  [err]
  (let [{:keys [category reached-model?]} (svar-classification err)]
    (boolean (and (false? reached-model?) (= :transport-drop category)))))

(defn empty-content-error?
  "True when the failure is svar's TYPED `:svar.llm/empty-content` — the
   provider ACCEPTED the request and answered a normal HTTP-200 stream that
   carried no text and no tool call. Dispatches on the typed `ex-data`
   (attached verbatim under `:data` on trace-entry error maps) — NEVER on
   message text.

   Emphatically NOT a rejection: the model ran and emitted nothing. Presenting
   it as 'the provider rejected the request' is false and points the user at
   auth/quota — the wrong place."
  [err]
  (= :svar.llm/empty-content (or (:type (:data err)) (:type err) (:type (ex-data err)))))

(defn stream-timeout-error?
  "True when the failure is one of svar's TYPED stream watchdogs firing:
   `:svar.core/stream-semantic-timeout` (transport alive, zero model/progress
   events inside the budget) or `:svar.core/stream-idle-timeout` (no bytes at
   all). Dispatches on typed `ex-data` — NEVER on message text.

   Emphatically NOT a rejection and NOT an outage: the provider accepted the
   request, the model was (as far as anyone knows) still working, and VIS hung
   up. Presenting it as 'the provider rejected the request before the model
   ran' is false and points the user at auth/quota — the wrong place."
  [err]
  (contains? #{:svar.core/stream-semantic-timeout :svar.core/stream-idle-timeout}
             (or (:type (:data err)) (:type err) (:type (ex-data err)))))

(def CONTEXT_OVERFLOW_TYPES
  "Every typed context-window failure svar can raise.

   `:svar.tokens/context-overflow` comes from the token-budget checker
   (`router/check-context`) and from the streamed `context_length_exceeded`
   family. `:svar.core/context-overflow` is what the `ask-code!` PREFLIGHT
   guard throws — and that is the path VIS actually takes, so it must be
   recognized too. Matching only the `tokens` variant made a preflight
   overflow miss EVERY overflow handler (emergency fold, terminal breaker,
   error card) and surface as a bogus `Provider unavailable`, with the error
   fed back into a request that could never reach the model."
  #{:svar.tokens/context-overflow :svar.core/context-overflow})

(defn context-overflow-error?
  "True only for a canonical typed context-window failure."
  [err]
  (contains? CONTEXT_OVERFLOW_TYPES (or (:type (:data err)) (:type err) (:type (ex-data err)))))

(defn- provider-id-of [data] (or (:provider-id data) (:provider data) (:provider/id data)))

(defn auth-provider-next-step
  [data]
  (let [provider-id (provider-id-of data)]
    (str "NEXT STEP: re-authenticate"
         (when provider-id (str " " provider-id))
         " or fix its API key, then retry.")))

(defn- rate-limit-next-step [] "NEXT STEP: wait and retry, or switch provider/model.")

(defn- transport-next-step
  []
  "NEXT STEP: retry. If it keeps failing, check your connection and the provider's status.")

(declare provider-error-attempts)

(defn- ->error-text
  "Best-effort human text for ONE upstream failure value — a string, an svar
   error map or a Throwable — for presentation details such as timeout phase."
  [x]
  (cond (nil? x) nil
        (string? x) x
        (keyword? x) (name x)
        (instance? Throwable x) (str (ex-message x) " " (:body (ex-data x)))
        (map? x) (str (or (:message x) (ex-message x)) " " (:body (or (:data x) x)))
        :else (str x)))

(defn- all-attempts-auth?
  "Svar's explicit unanimous-attempt verdict, used only for plural UI copy."
  [err]
  (let [{:keys [category all-attempts-category?]} (svar-classification err)]
    (boolean (and (= :auth category) all-attempts-category?))))

(defn auth-failed-provider-ids
  "Provider ids whose Svar attempt classification is `:auth`, in routing order."
  [err]
  (let [{:keys [attempt-categories]} (svar-classification err)]
    (into []
          (comp (keep (fn [[attempt category]]
                        (when (= :auth category) (:provider attempt))))
                (distinct))
          (map vector (provider-error-attempts err) attempt-categories))))

(defn provider-error-upstream-text
  "Every upstream text fragment retained for presentation: wrapper, body and
   Svar routing-attempt details. Used only to describe timeout phase, never to
   classify the failure."
  [err]
  (let [data (or (:data err) (ex-data err) err)]
    (str/lower-case (str/join "\n"
                              (remove str/blank?
                                (map ->error-text
                                     (concat [(or (ex-message err) (:message err)) (:body data)
                                              (:error data)]
                                             (mapcat (fn [a]
                                                       [(:reason a) (:error a)])
                                                     (provider-error-attempts err)))))))))


(defn upstream-timeout-phase
  "`:connect` (the request never reached the model), `:read` (it did, and the
   response never finished) or nil (the provider only said 'deadline'). The
   phase is the whole difference between 'safe, nothing ran' and 'the model may
   have started', so the NEXT STEP line depends on it."
  [status text]
  (let [text (str/lower-case (str text))]
    (cond (or (str/includes? text "connection timed out")
              (str/includes? text "connect timeout")
              (str/includes? text "connection timeout"))
          :connect
          (or (= 504 status)
              (str/includes? text "read timeout")
              (str/includes? text "read timed out")
              (str/includes? text "response timed out")
              (str/includes? text "stream timed out"))
          :read
          :else nil)))

(declare provider-error-kind)

(defn provider-error-explanation
  "The `WHAT HAPPENED:` prose line — the single canonical human sentence for this
   failure, shared by every surface. The actionable step lives in
   `provider-error-next-step` (a separate block), so this is JUST the diagnosis."
  [err]
  (let
    [message
     (or (ex-message err) (:message err) (str err))

     data
     (or (:data err) (ex-data err) err)

     body-raw
     (some-> (:body data)
             str)

     status
     (:status data)

     provider-message
     (provider-body-message body-raw)

     schema-rejection?
     (tool-schema-rejection-message? (str provider-message "\n" message))

     tool-name
     (:tool-name data)

     schema-field
     (:tool-schema-field data)

     output-budget-too-small?
     (output-budget-too-small-error? status (str provider-message "\n" message))]

    (cond
      (context-overflow-error? err)
      (str "WHAT HAPPENED: the request exceeded the model's context window."
           (when-let [input (:input-tokens data)]
             (str " Input: " input " tokens."))
           (when-let [limit (:max-input-tokens data)]
             (str " Limit: " limit " tokens.")))
      (stream-timeout-error? err)
      (let
        [data
         (or (:data err) (ex-data err))

         budget-ms
         (or (:semantic-timeout-ms data) (:idle-timeout-ms data))

         semantic?
         (= :svar.core/stream-semantic-timeout
            (or (:type (:data err)) (:type err) (:type (ex-data err))))]

        (str "WHAT HAPPENED: the stream stalled — "
             (if semantic? "no model progress" "no bytes")
             (when budget-ms (str " for " (long (/ (long budget-ms) 1000)) "s"))
             ". The model was likely still reasoning. Nothing was rejected; your "
             "transcript and tool results are intact."))
      (empty-content-error? err)
      (let [resends (long (or (:empty-reply-resends data) 0))]
        (str "WHAT HAPPENED: the model replied with no text and no tool call."
             (when (pos? resends)
               (str " Re-sent " resends (if (= 1 resends) " time" " times") "; still empty."))
             " A model-side stall — your transcript and tool results are intact."))
      (invalid-thinking-signature-message? provider-message)
      (str "WHAT HAPPENED: Anthropic rejected a `thinking` block signature — usually "
           "preserved thinking replayed across a provider/model switch.")
      (gateway-tool-field-rejection err)
      (str "WHAT HAPPENED: the gateway added `"
           (gateway-tool-field-rejection err)
           "` to the tool payload and this provider path rejected its own addition. Vis "
           "never sends that field, so no tool schema can fix it."
           (when (seq provider-message) (str " " provider-message)))
      schema-rejection? (str "WHAT HAPPENED: tool `" (or tool-name "unknown")
                             "` has a top-level `oneOf`/`allOf`/`anyOf` in `" (or schema-field
                                                                                  "input_schema")
                             "` — a schema defect, not an outage." (when (seq provider-message)
                                                                     (str " " provider-message)))
      output-budget-too-small?
      (str "WHAT HAPPENED: the provider rejected the request because the output-token budget "
           "was below this model's minimum"
           (when-let [minimum (output-budget-minimum (str provider-message "\n" message))]
             (str " (it requires at least " minimum ")"))
           " — a request defect, not an outage." (when (seq provider-message)
                                                   (str " " provider-message)))
      (and (= :auth (provider-error-kind err)) (not (all-attempts-auth? err)))
      (str "WHAT HAPPENED: the provider rejected your credentials."
           (when (seq provider-message) (str " " provider-message)))
      (all-attempts-auth? err)
      (let [ids (auth-failed-provider-ids err)]
        (str "WHAT HAPPENED: "
             (if (> (count (provider-error-attempts err)) 1)
               "every provider in this turn's fallback list rejected your credentials"
               "the provider rejected your credentials")
             (when (seq ids) (str " (" (str/join ", " (map str ids)) ")"))
             " — an unchanged retry re-sends the same rejected key."))
      (= :quota-exhausted (provider-error-kind err))
      (str "WHAT HAPPENED: the provider says this account has no usable quota or credits."
           (when (seq provider-message) (str " " provider-message)))
      (= :transport (provider-error-kind err))
      "WHAT HAPPENED: the connection dropped before any response came back. The model never saw the request."
      (= :rate-limit (provider-error-kind err))
      (str "WHAT HAPPENED: the provider rate-limited this request."
           (when (seq provider-message) (str " " provider-message)))
      (= :resource-mismatch (provider-error-kind err))
      (str "WHAT HAPPENED: this conversation is pinned to a different provider resource — the "
           "item was created under another deployment/endpoint, so the one Vis just called "
           "cannot read it. Not an outage: an identical retry fails identically."
           (when (seq provider-message) (str " " provider-message)))
      (= :upstream-timeout (provider-error-kind err))
      (let [phase (upstream-timeout-phase status (provider-error-upstream-text err))]
        (str "WHAT HAPPENED: the provider request timed out upstream"
             (case phase
               :connect
               " while connecting — the request never reached the model"

               :read
               " while reading the response — the model may have started work"

               "")
             ". Nothing was rejected; your transcript and tool results are intact."))
      (contains? #{:model-unavailable :gateway-unavailable :stream-interrupted}
                 (provider-error-kind err))
      (str "WHAT HAPPENED: "
           (or (:summary (svar-classification err))
               "the provider failed before a usable response."))
      (and (nil? status) (str/blank? provider-message) (:request-id (svar-classification err)))
      (str "WHAT HAPPENED: the provider call failed with nothing but a correlation id — no "
           "status, no provider message. Nothing here says the request was rejected; quote the "
           "id below if it keeps happening.")
      (= :unknown (:category (svar-classification err)))
      (str "WHAT HAPPENED: " (or (:summary (svar-classification err)) "the provider call failed."))
      (seq provider-message) (str "WHAT HAPPENED: the provider rejected the request. "
                                  provider-message)
      :else (str "WHAT HAPPENED: "
                 (or (:summary (svar-classification err)) "the provider call failed.")))))


(defn provider-error-title
  "A SHORT headline for the failure, by kind — the card title on every surface."
  [err]
  (let
    [message
     (or (ex-message err) (:message err) (str err))

     data
     (or (:data err) (ex-data err) err)]

    (case (provider-error-kind err)
      :context-overflow
      "Context window exceeded"

      :stream-timeout
      "Stream went quiet — Vis timed out"

      :empty-content
      "Model returned an empty response"

      :invalid-thinking-signature
      "Provider rejected the request"

      :tool-schema
      (if-let [field (gateway-tool-field-rejection err)]
        (str "Gateway sent an unsupported tool field: " field)
        (if-let [tool-name (:tool-name data)]
          (str "Native tool schema rejected: " tool-name)
          "Native tool schema rejected"))

      :output-budget-too-small
      "Output token budget too small"

      :auth
      (if (and (all-attempts-auth? err) (> (count (provider-error-attempts err)) 1))
        "All providers rejected your credentials"
        "Provider authentication failed")

      :quota-exhausted
      "Provider quota exhausted"

      :rate-limit
      "Provider rate-limited"

      :model-unavailable
      "Provider model unavailable"

      :gateway-unavailable
      "Provider gateway unavailable"

      :stream-interrupted
      "Provider stream interrupted"

      :upstream-timeout
      "Provider request timed out"

      :resource-mismatch
      "Conversation pinned to another provider resource"

      :transport
      "Could not reach provider"

      (if (and (= "All providers exhausted" message) (> (count (provider-error-attempts err)) 1))
        "All providers unavailable"
        "Provider unavailable"))))

(defn provider-error-next-step
  "The actionable `NEXT STEP:` line — what the user should DO — by kind. Kept
   SEPARATE from the diagnosis so surfaces can make it prominent."
  [err]
  (let
    [message
     (or (ex-message err) (:message err) (str err))

     data
     (or (:data err) (ex-data err) err)]

    (case (provider-error-kind err)
      :context-overflow
      "NEXT STEP: fold older settled history, choose a larger-context model, or start a fresh session."

      :stream-timeout
      (if (= :svar.core/stream-semantic-timeout (:type data))
        (str "NEXT STEP: retry explicitly. If long reasoning turns keep tripping the "
             "watchdog, raise `semantic-timeout-ms` or set it to `nil`.")
        (str "NEXT STEP: retry explicitly. If the transport is expected to stay silent "
             "this long, raise `idle-timeout-ms`."))

      :empty-content
      (str "NEXT STEP: retry explicitly. If it persists, rephrase or trim the last " "message.")

      :invalid-thinking-signature
      "NEXT STEP: retry with plain context. If it persists, don't replay preserved thinking across a provider/model switch."

      :tool-schema
      (if-let [field (gateway-tool-field-rejection err)]
        (str "NEXT STEP: fix it at the gateway — LiteLLM must stop forwarding `"
             field
             "` for this model (`bedrock_converse_supports_strict_tools: false` in its model "
             "map, or a newer LiteLLM) — or switch model/provider. An unchanged retry fails "
             "identically.")
        "NEXT STEP: update Vis or disable the offending extension, then retry.")

      :output-budget-too-small
      (str "NEXT STEP: raise `max_tokens`/`max_output_tokens` for this provider (its `extra_body` "
           "in vis.yml) or drop the override, then retry — an unchanged retry fails identically.")

      :auth
      (let [ids (auth-failed-provider-ids err)]
        (if (and (nil? (provider-id-of data)) (seq ids))
          (str "NEXT STEP: re-authenticate "
               (str/join ", " (map str ids))
               " or fix "
               (if (= 1 (count ids)) "its API key" "their API keys")
               ", then retry.")
          (auth-provider-next-step data)))

      :quota-exhausted
      "NEXT STEP: check the provider plan, usage limits, and available credits, then retry."

      :rate-limit
      (rate-limit-next-step)

      :upstream-timeout
      (case (upstream-timeout-phase (:status data) (provider-error-upstream-text err))
        :connect
        (str "NEXT STEP: retry — the request never reached the model. If it keeps timing "
             "out, check network/proxy reachability or switch provider/model.")

        :read
        (str "NEXT STEP: retry — the model may have started, so re-read the last output "
             "before assuming nothing ran. If it repeats, trim the request or switch to a "
             "faster model.")

        (str "NEXT STEP: retry once; if it repeats, raise the provider request timeout or switch "
             "provider/model."))

      :resource-mismatch
      (str "NEXT STEP: don't retry as-is — point Vis back at the resource/deployment that created "
           "this conversation, or start a fresh session (or switch provider/model).")

      :transport
      (transport-next-step)

      (if-let [step (:next-step (svar-classification err))]
        (str "NEXT STEP: " step)
        (if (and (= "All providers exhausted" message) (> (count (provider-error-attempts err)) 1))
          "NEXT STEP: retry, or switch provider/model."
          "NEXT STEP: retry; if it persists, switch provider/model.")))))

(defn provider-error-attempts
  "The per-provider failure records svar accumulates on an `all-providers-exhausted`
   error — `[{:provider :model :status :reason :error} …]`, one per provider tried.
   Empty when svar didn't attach them (older svar / a non-routing failure).

   Read `ex-data` too, not only a trace entry's `:data`: svar's router throws a
   LIVE `ex-info` whose message is just `Provider unavailable` and keeps the real
   cause (`litellm.Timeout: BedrockException: Timeout Error …`) on `:attempts` in
   its `ex-data`. Missing that reduced every routed failure to the generic card
   on the surface that presents the throwable itself."
  [err]
  (let [data (or (:data err) (ex-data err) err)]
    (vec (or (:attempts data) (:attempts err) []))))

(defn attempt->line
  "One attempt → a compact `provider/model: <status> <reason>` line, e.g.
   `anthropic/claude-opus-4: 429 rate-limit`."
  [{:keys [provider model status reason]}]
  (str (some-> provider
               str)
       (when (seq (str model)) (str "/" model))
       ": "
       (when status (str status " "))
       (some-> reason
               name)))

(defn provider-error-attempts-summary
  "The attempts joined into ONE scannable line (`a: 429 rate-limit · b: 401 auth`),
   or nil when there are none. The at-a-glance 'why each provider bowed out'."
  [err]
  (let [as (provider-error-attempts err)]
    (when (seq as) (str/join " · " (map attempt->line as)))))

(defn provider-error-kind
  "Map Svar's canonical category to Vis's presentation vocabulary.

   Vis only refines failures whose typed payload carries UI-specific detail. It
   never reclassifies provider status codes, prose, or routing outcomes."
  [err]
  (let
    [message
     (or (ex-message err) (:message err) (str err))

     data
     (or (:data err) (ex-data err) err)

     provider-message
     (provider-body-message (some-> (:body data)
                                    str))]

    (cond (context-overflow-error? err) :context-overflow
          (stream-timeout-error? err) :stream-timeout
          (empty-content-error? err) :empty-content
          (invalid-thinking-signature-message? provider-message) :invalid-thinking-signature
          (or (tool-schema-rejection-message? (str provider-message "\n" message))
              (some? (gateway-tool-field-rejection err)))
          :tool-schema
          (output-budget-too-small-error? (:status data) (str provider-message "\n" message))
          :output-budget-too-small
          :else (case (:category (svar-classification err))
                  :auth
                  :auth

                  :quota-exhausted
                  :quota-exhausted

                  :rate-limited
                  :rate-limit

                  :transport-drop
                  :transport

                  :connect-timeout
                  :upstream-timeout

                  :upstream-timeout
                  :upstream-timeout

                  :gateway-unavailable
                  :gateway-unavailable

                  :model-unavailable
                  :model-unavailable

                  :resource-mismatch
                  :resource-mismatch

                  :tool-schema-unsupported
                  :tool-schema

                  :context-length-exceeded
                  :context-overflow

                  :stream-interrupted
                  :stream-interrupted

                  :generic))))



(defn provider-error-retryable?
  "Svar's canonical retry-safety verdict, exposed to Vis presentation surfaces.

   This does not drive a retry in Vis. Svar owns and exhausts the retry ladder
   before an error reaches this namespace."
  [err]
  (boolean (:retryable? (svar-classification err))))

(defn provider-failure?
  "True when `err` is a PROVIDER failure — presentable as the styled card
   `provider-error-content` builds — rather than an internal Vis bug whose bare
   message is all there is. Either it classifies to a KNOWN kind, or it carries
   provider evidence: an upstream HTTP status, a routing trace, a provider id."
  [err]
  (let [data (or (:data err) (ex-data err))]
    (boolean (or (not= :generic (provider-error-kind err))
                 (some? (:status data))
                 (seq (:routed/trace data))
                 (some? (provider-id-of data))))))

(defn provider-error-facts
  "Ordered `[label value]` rows of the bare facts (no prose). Same set the
   IR renders as a `<ul>` and the TUI renders as plain rows."
  [err]
  (let
    [message
     (or (ex-message err) (:message err) (str err))

     data
     (:data err)

     status
     (:status data)

     request-id
     (or (:request-id data)
         (:request_id data)
         ;; A gateway that answers with nothing but a correlation id leaves it in
         ;; the message; svar already knows how to read it out (issue #69).
         (:request-id (svar-classification err)))

     provider-id
     (provider-id-of data)

     tool-name
     (:tool-name data)

     schema-field
     (:tool-schema-field data)

     schema-path
     (:tool-schema-path data)]

    (cond-> []
      (and (seq message)
           (not (generic-wrapper-message? message))
           (not= (str/trim message) (str request-id)))
      (conj ["Wrapper" message])

      status
      (conj ["HTTP" (str status)])

      provider-id
      (conj ["Provider" (str provider-id)])

      tool-name
      (conj ["Tool" (str tool-name)])

      schema-field
      (conj ["Schema" (str schema-field)])

      schema-path
      (conj ["Provider path" (str schema-path)])

      request-id
      (conj ["Request id" (str request-id)]))))

(defn provider-error-raw-body
  "Truncated raw upstream body — surfaced ONLY when no structured provider
   message could be extracted (HTML pages, bare 5xx). nil otherwise so the
   readable message isn't echoed twice."
  [err]
  (let
    [body-raw
     (some-> (:body (:data err))
             str)

     structured-msg
     (provider-structured-message body-raw)]

    (when (and body-raw (not (str/blank? body-raw)) (not structured-msg))
      (truncate body-raw CHAT_ERROR_BODY_RENDER_CHARS))))

(defn provider-error-info
  "Structured echo of the facts a chat surface can render
   compactly without parsing the IR back out."
  [err]
  (let
    [message
     (or (ex-message err) (:message err) (str err))

     data
     ;; Accept BOTH shapes the callers hold: svar's error MAP (`:data`) and a
     ;; raw Throwable (`ex-data`). Reading only `:data` silently dropped the
     ;; status/request-id/body of every Throwable-shaped provider failure —
     ;; the same lookup every other fn in this ns already does.
     (or (:data err) (ex-data err))

     body-raw
     (some-> (:body data)
             str)

     status
     (:status data)

     request-id
     (or (:request-id data) (:request_id data))

     provider-message
     (provider-body-message body-raw)]

    {:kind (provider-error-kind err)
     :title (provider-error-title err)
     :explanation (provider-error-explanation err)
     :next-step (provider-error-next-step err)
     :status status
     :request-id (some-> request-id
                         str)
     :provider-message (not-empty provider-message)
     :wrapper-message (not-empty message)
     :provider-id (provider-id-of data)
     :is-retryable (provider-error-retryable? err)
     :attempts (not-empty (provider-error-attempts err))
     :body (provider-error-raw-body err)}))

(defn split-error-label
  "Split a provider-error prose line into `[label body]` at the first `: `.

   `provider-error-explanation` / `provider-error-next-step` always lead with
   an ALL-CAPS label (`WHAT HAPPENED: ` / `NEXT STEP: `). This returns
   `[\"<LABEL>: \" \"<body sentence>\"]` so a surface can render the label
   distinctly (bold in final prose, SGR/bold sentinels in the TUI trace)
   while the body stays plain — ONE convention, shared by every surface, so
   the label/body split never diverges between the IR and the TUI recap.

   Returns `[nil s]` (whole string as the body, no label) when `s` does not
   follow the `ALL-CAPS:` convention, so callers can treat the label as
   optional without a separate nil-check."
  [s]
  (if-let [[_ label body] (re-matches #"(?s)^([A-Z ]+):\s*(.*)$" (str s))]
    [(str label ": ") body]
    [nil (str s)]))

(defn provider-error-content
  "Canonical typed content for a provider failure. The error remains data;
   channels decide how to present it and Markdown is not used as an envelope."
  [err]
  (let
    [{:keys [kind title explanation next-step status request-id provider-id attempts body]}
     (provider-error-info err)

     retryable?
     (provider-error-retryable? err)

     message
     (str/join "\n\n" (remove str/blank? [title explanation next-step]))]

    [(cond-> (content/error (str "provider_" (name (or kind :failure))) message retryable?)
       status
       (assoc "status" status)

       request-id
       (assoc "request_id" request-id)

       provider-id
       (assoc "provider" (name provider-id))

       attempts
       (assoc "attempts" attempts)

       body
       (assoc "body" body))]))
