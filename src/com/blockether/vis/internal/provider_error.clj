(ns com.blockether.vis.internal.provider-error
  "Single source of truth for provider-error presentation.

   Typed provider-error content and per-iteration trace rows derive their wording
   and facts from this namespace, so a failure reads identically everywhere.

   `err` is the error map carried on a trace entry / ex-info:
   `{:message .. :data {:status .. :body .. :request-id ..} ..}`. Every
   helper tolerates the bare ex-info shape too (via `ex-message`)."
  (:require [charred.api :as json]
            [clojure.string :as str]
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

(defn auth-provider-error?
  [status message wrapper-message]
  (let [text (str (or message "") "\n" (or wrapper-message ""))]
    (boolean
      (or
        (contains? #{401 403} status)
        (re-find
          #"(?i)(authentication|unauthorized|forbidden|credential|api[ -]?key|access[ -]?token|expired token|invalid token)"
          text)))))

(defn- anthropic-extra-usage-error?
  "True for Anthropic's 'third-party apps now draw from your extra usage' 400 —
   a quota/billing gate that svar retries transiently but that persists when the
   user's extra-usage allowance is genuinely exhausted."
  [status message wrapper-message]
  (let [text (str/lower-case (str (or message "") "\n" (or wrapper-message "")))]
    (boolean (and (= 400 status)
                  (or (str/includes? text "draw from your extra usage")
                      (str/includes? text "third-party apps now draw")
                      (str/includes? text "extra usage"))))))

(defn- rate-limit-error?
  [status message wrapper-message]
  (let [text (str/lower-case (str (or message "") "\n" (or wrapper-message "")))]
    (boolean
      (or (= 429 status) (str/includes? text "rate limit") (str/includes? text "rate-limit")))))

(defn- transport-error?
  "True for a TRANSPORT/connection failure — the HTTP request never got a
   response back: the socket closed with no bytes, the connection was reset /
   refused / timed out, DNS failed, TLS was torn down. The model NEVER saw the
   request, so it is emphatically NOT a rejection — surfacing it as one (\"the
   provider rejected the request\") is both false and useless. These carry no
   usable HTTP status (nothing answered), so we key off the wrapper/cause text."
  [status message wrapper-message]
  (let [text (str/lower-case (str (or message "") "\n" (or wrapper-message "")))]
    (boolean
      (and (nil? status)
           (or (str/includes? text "received no bytes")
               (str/includes? text "no response headers")
               (str/includes? text "header parser")
               (str/includes? text "no bytes")
               (str/includes? text "unexpected end of stream")
               (str/includes? text "end of stream")
               (str/includes? text "connection reset")
               (str/includes? text "connection closed")
               (str/includes? text "connection refused")
               (str/includes? text "connection timed out")
               (str/includes? text "broken pipe")
               (str/includes? text "no route to host")
               (str/includes? text "network is unreachable")
               (str/includes? text "unknownhostexception")
               (str/includes? text "name or service not known")
               (str/includes? text "handshake")
               (str/includes? text "premature")
               (str/includes? text "eof")
               (= "closed" (str/trim text)))))))

(defn transport-throwable?
  "True when Throwable `t` is a CONNECTION/transport failure — the SAME
   classification `transport-error?` makes from a parsed provider error, but
   taken straight off a Throwable so the RETRY gate and the human message agree.

   Walks the whole cause chain for the message text. Idempotent by definition:
   a transport failure means the request never reached the model (the socket
   closed with no bytes, was reset/refused/timed out, DNS/TLS died), so it is
   ALWAYS safe to retry — regardless of whether a response STREAM had started
   (a pre-response failure carries no `:stream?`, yet is the safest retry of
   all). A real REJECTION carries an HTTP status, so `transport-error?`'s
   `nil status` guard keeps 4xx/5xx out of this path."
  [^Throwable t]
  (when t
    (let
      [chain
       (take 16
             (take-while some?
                         (iterate (fn [^Throwable x]
                                    (some-> x
                                            .getCause))
                                  t)))

       text
       (str/join "\n"
                 (keep (fn [^Throwable x]
                         (.getMessage x))
                       chain))

       data
       (ex-data t)

       status
       (or (:status data) (:status (:data data)))]

      (transport-error? status text nil))))

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

(defn- provider-id-of [data] (or (:provider-id data) (:provider data) (:provider/id data)))

(defn auth-provider-next-step
  [data]
  (let [provider-id (provider-id-of data)]
    (str
      "NEXT STEP: re-authenticate this provider or update its API key, then retry. "
      "TUI: Ctrl+K -> Model / Providers -> re-authenticate provider. "
      "CLI: run `vis providers auth"
      (when provider-id (str " " provider-id))
      "` for OAuth providers; for API-key providers, fix the configured key/env var and restart Vis.")))

(defn- rate-limit-next-step
  []
  "NEXT STEP: rate limit — wait and retry, re-authenticate, or switch provider/model.")

(defn- transport-next-step
  []
  (str "NEXT STEP: this is a network/connection blip, not a rejection — just retry "
       "(Vis resends the same request). If it keeps failing, check your internet "
       "connection, any VPN/proxy/firewall, and the provider's status page."))

(declare provider-error-attempts)

(defn provider-error-explanation
  "The `WHAT HAPPENED:` prose line — the single canonical human sentence for this
   failure, shared by every surface. The actionable step lives in
   `provider-error-next-step` (a separate block), so this is JUST the diagnosis."
  [err]
  (let
    [message
     (or (ex-message err) (:message err) (str err))

     data
     (:data err)

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
     (:tool-schema-field data)]

    (cond
      (empty-content-error? err)
      (let [resends (long (or (:empty-reply-resends data) 0))]
        (str "WHAT HAPPENED: the provider accepted the request and answered with a "
             "normal HTTP-200 stream, but the model produced no text and no tool "
             "call (an empty reply)."
             (when (pos? resends)
               (str " Vis already re-sent the SAME request to the SAME model "
                    resends
                    (if (= 1 resends) " more time" " more times")
                    " with widening backoff; every attempt came back empty."))
             " This is a model-side stall on this exact request — not a rejection "
             "and not an auth, quota, or network problem. Your transcript and tool "
             "results are intact — nothing was lost."))
      (invalid-thinking-signature-message? provider-message)
      (str
        "WHAT HAPPENED: Anthropic rejected the request before the model ran because Vis sent a `thinking` block with a signature that is not valid for Anthropic. "
        "Most likely cause: preserved-thinking replay crossed a provider/model boundary (for example Z.ai/Codex/OpenAI reasoning state was replayed into Anthropic), or an old Anthropic thinking block came from a different session/key.")
      schema-rejection?
      (str
        "WHAT HAPPENED: the provider rejected the request before the model ran because native tool `"
        (or tool-name "unknown")
        "` has a top-level `oneOf`, `allOf`, or `anyOf` in `"
        (or schema-field "input_schema")
        "`. "
        "This is a deterministic Vis/extension schema defect, not an outage, auth failure, or quota problem."
        (when (seq provider-message) (str " Provider message: " provider-message)))
      (auth-provider-error? status provider-message message)
      (str "WHAT HAPPENED: the provider rejected credentials before the model ran."
           (when (seq provider-message) (str " Provider message: " provider-message)))
      (anthropic-extra-usage-error? status provider-message message)
      (str
        "WHAT HAPPENED: Anthropic rejected the request — your Claude subscription's extra-usage allowance is exhausted. "
        "Third-party apps (including Vis) now draw from extra usage rather than your plan. "
        "Provider message: " (or provider-message
                                 "Third-party apps now draw from your extra usage"))
      (transport-error? status provider-message message)
      "WHAT HAPPENED: Vis could not complete the HTTP request to the provider — the connection dropped before any response came back (a network/transport failure; here the socket closed with no bytes). The model never saw the request, so nothing was rejected and nothing was lost."
      (rate-limit-error? status provider-message message)
      (str "WHAT HAPPENED: the provider rate-limited the request before the model ran."
           (when (seq provider-message) (str " Provider message: " provider-message)))
      (or (= "All providers exhausted" message) (= "Provider unavailable" message))
      (if (or (= "Provider unavailable" message) (<= (count (provider-error-attempts err)) 1))
        (str "WHAT HAPPENED: the request to the selected provider failed before a usable "
             "response came back. Vis did NOT fall back across your other providers — the "
             "choice of where to go next is yours. Your transcript and tool results are "
             "intact — nothing was lost.")
        (str "WHAT HAPPENED: every provider in this turn's fallback list was tried and each "
             "attempt failed before a usable response came back. Your transcript and tool "
             "results are intact — nothing was lost."))
      (seq provider-message)
      (str
        "WHAT HAPPENED: the provider rejected the request before the model ran. Provider message: "
        provider-message)
      :else "WHAT HAPPENED: the provider rejected the request before the model ran.")))

(declare provider-error-kind)

(defn provider-error-title
  "A SHORT headline for the failure, by kind — the card title on every surface."
  [err]
  (let
    [message
     (or (ex-message err) (:message err) (str err))

     data
     (:data err)]

    (case (provider-error-kind err)
      :empty-content
      "Model returned an empty response"

      :invalid-thinking-signature
      "Provider rejected the request"

      :tool-schema
      (if-let [tool-name (:tool-name data)]
        (str "Native tool schema rejected: " tool-name)
        "Native tool schema rejected")

      :auth
      "Provider authentication failed"

      :anthropic-extra-usage
      "Claude subscription quota exhausted"

      :rate-limit
      "Provider rate-limited"

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
     (:data err)]

    (case (provider-error-kind err)
      :empty-content
      (str "NEXT STEP: retry the turn — Vis re-sends the same request to the same "
           "model, and these stalls usually clear with a little time. If it keeps "
           "happening, rephrase or trim the last message; switching model "
           "(TUI: Ctrl+K · CLI: `vis providers`) is a last resort, not a requirement.")

      :invalid-thinking-signature
      "NEXT STEP: retry — Vis will resend with only normal transcript/trailer context. If it persists, don't replay preserved-thinking across a provider/model switch."

      :tool-schema
      "NEXT STEP: update Vis or disable the offending extension, then retry. Retrying the unchanged tool schema cannot work."

      :auth
      (auth-provider-next-step data)

      :anthropic-extra-usage
      "NEXT STEP: add extra usage credits at https://claude.ai/settings/usage then retry. If you haven't set a spend limit yet, set one there to unblock requests."

      :rate-limit
      (rate-limit-next-step)

      :transport
      (transport-next-step)

      (if (and (= "All providers exhausted" message) (> (count (provider-error-attempts err)) 1))
        "NEXT STEP: retry once (transient outages recover), then check provider status, auth, and quota — or switch provider/model (TUI: Ctrl+K · CLI: `vis providers`)."
        "NEXT STEP: retry once (transient outages recover); if it persists, switch provider/model (TUI: Ctrl+K · CLI: `vis providers`) or check that provider's status, auth, and quota."))))

(defn provider-error-attempts
  "The per-provider failure records svar accumulates on an `all-providers-exhausted`
   error — `[{:provider :model :status :reason :error} …]`, one per provider tried.
   Empty when svar didn't attach them (older svar / a non-routing failure)."
  [err]
  (let [data (:data err)]
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
  [err]
  (let
    [message
     (or (ex-message err) (:message err) (str err))

     data
     (:data err)

     body-raw
     (some-> (:body data)
             str)

     status
     (:status data)

     provider-message
     (provider-body-message body-raw)

     schema-rejection?
     (tool-schema-rejection-message? (str provider-message "\n" message))]

    (cond (empty-content-error? err) :empty-content
          (invalid-thinking-signature-message? provider-message) :invalid-thinking-signature
          schema-rejection? :tool-schema
          (auth-provider-error? status provider-message message) :auth
          (anthropic-extra-usage-error? status provider-message message) :anthropic-extra-usage
          (rate-limit-error? status provider-message message) :rate-limit
          (transport-error? status provider-message message) :transport
          :else :generic)))

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
     (or (:request-id data) (:request_id data))

     provider-id
     (provider-id-of data)

     tool-name
     (:tool-name data)

     schema-field
     (:tool-schema-field data)

     schema-path
     (:tool-schema-path data)]

    (cond-> []
      (and (seq message) (not (generic-wrapper-message? message)))
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
     (:data err)

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
     (contains? #{:rate-limit :transport :overloaded :empty-content} kind)

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
