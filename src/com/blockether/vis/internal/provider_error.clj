(ns com.blockether.vis.internal.provider-error
  "Single source of truth for provider-error presentation.

   Both the turn engine's answer IR (`provider-error-ir`, rendered by the
   Web channel and the TUI's final-answer bubble) and the TUI's
   per-iteration trace error rows derive their wording and facts from
   THIS namespace, so a provider failure reads IDENTICALLY everywhere it
   surfaces — no more divergent `PROVIDER_ERROR HTTP 400` vs the polished
   `WHAT HAPPENED:` banner.

   `err` is the error map carried on a trace entry / ex-info:
   `{:message .. :data {:status .. :body .. :request-id ..} ..}`. Every
   helper tolerates the bare ex-info shape too (via `ex-message`)."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.render :as render]
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
  (boolean (when-let [t (some-> message
                                str
                                str/lower-case)]
             (or (str/includes? t "exceptional status code")
                 (str/includes? t "provider error http")
                 ;; the title already says "All providers unavailable" and the
                 ;; per-provider attempts carry the detail — the bare wrapper echo
                 ;; adds nothing, so don't repeat it as a fact row.
                 (str/includes? t "all providers exhausted")))))

(defn invalid-thinking-signature-message?
  [message]
  (boolean (and (string? message) (re-find #"(?i)invalid.*signature.*thinking.*block" message))))

(defn auth-provider-error?
  [status message wrapper-message]
  (let [text (str (or message "") "\n" (or wrapper-message ""))]
    (boolean
      (or
        (contains? #{401 403} status)
        (re-find
          #"(?i)(authentication|unauthorized|forbidden|credential|api[ -]?key|access[ -]?token|expired token|invalid token)"
          text)))))

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
               (str/includes? text "eof"))))))

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
    (let [chain
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

(defn provider-error-explanation
  "The `WHAT HAPPENED:` prose line — the single canonical human sentence for this
   failure, shared by every surface. The actionable step lives in
   `provider-error-next-step` (a separate block), so this is JUST the diagnosis."
  [err]
  (let [message
        (or (ex-message err) (:message err) (str err))

        data
        (:data err)

        body-raw
        (some-> (:body data)
                str)

        status
        (:status data)

        provider-message
        (provider-body-message body-raw)]

    (cond
      (invalid-thinking-signature-message? provider-message)
      (str
        "WHAT HAPPENED: Anthropic rejected the request before the model ran because Vis sent a `thinking` block with a signature that is not valid for Anthropic. "
        "Most likely cause: preserved-thinking replay crossed a provider/model boundary (for example Z.ai/Codex/OpenAI reasoning state was replayed into Anthropic), or an old Anthropic thinking block came from a different session/key.")
      (auth-provider-error? status provider-message message)
      (str "WHAT HAPPENED: the provider rejected credentials before the model ran."
           (when (seq provider-message) (str " Provider message: " provider-message)))
      (transport-error? status provider-message message)
      "WHAT HAPPENED: Vis could not complete the HTTP request to the provider — the connection dropped before any response came back (a network/transport failure; here the socket closed with no bytes). The model never saw the request, so nothing was rejected and nothing was lost."
      (rate-limit-error? status provider-message message)
      (str "WHAT HAPPENED: the provider rate-limited the request before the model ran."
           (when (seq provider-message) (str " Provider message: " provider-message)))
      (= "All providers exhausted" message)
      "WHAT HAPPENED: Vis tried every configured provider (route + fallbacks) and each attempt failed before a usable response came back. Your transcript and tool results are intact — nothing was lost."
      (seq provider-message)
      (str
        "WHAT HAPPENED: the provider rejected the request before the model ran. Provider message: "
        provider-message)
      :else "WHAT HAPPENED: the provider rejected the request before the model ran.")))

(declare provider-error-kind)

(defn provider-error-title
  "A SHORT headline for the failure, by kind — the card title on every surface."
  [err]
  (let [message (or (ex-message err) (:message err) (str err))]
    (case (provider-error-kind err)
      :invalid-thinking-signature
      "Provider rejected the request"

      :auth
      "Provider authentication failed"

      :rate-limit
      "Provider rate-limited"

      :transport
      "Could not reach provider"

      (if (= "All providers exhausted" message)
        "All providers unavailable"
        "Provider unavailable"))))

(defn provider-error-next-step
  "The actionable `NEXT STEP:` line — what the user should DO — by kind. Kept
   SEPARATE from the diagnosis so surfaces can make it prominent."
  [err]
  (let [message
        (or (ex-message err) (:message err) (str err))

        data
        (:data err)]

    (case (provider-error-kind err)
      :invalid-thinking-signature
      "NEXT STEP: retry — Vis will resend with only normal transcript/trailer context. If it persists, don't replay preserved-thinking across a provider/model switch."

      :auth
      (auth-provider-next-step data)

      :rate-limit
      (rate-limit-next-step)

      :transport
      (transport-next-step)

      (if (= "All providers exhausted" message)
        "NEXT STEP: retry once (transient outages recover), then check provider status, auth, and quota — or switch provider/model (TUI: Ctrl+K · CLI: `vis providers`)."
        "NEXT STEP: retry; if it persists, check the provider's status page, your auth, and your quota."))))

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
  (let [message
        (or (ex-message err) (:message err) (str err))

        data
        (:data err)

        body-raw
        (some-> (:body data)
                str)

        status
        (:status data)

        provider-message
        (provider-body-message body-raw)]

    (cond (invalid-thinking-signature-message? provider-message) :invalid-thinking-signature
          (auth-provider-error? status provider-message message) :auth
          (rate-limit-error? status provider-message message) :rate-limit
          (transport-error? status provider-message message) :transport
          :else :generic)))

(defn provider-error-facts
  "Ordered `[label value]` rows of the bare facts (no prose). Same set the
   IR renders as a `<ul>` and the TUI renders as plain rows."
  [err]
  (let [message
        (or (ex-message err) (:message err) (str err))

        data
        (:data err)

        status
        (:status data)

        request-id
        (or (:request-id data) (:request_id data))

        provider-id
        (provider-id-of data)]

    (cond-> []
      (and (seq message) (not (generic-wrapper-message? message)))
      (conj ["Wrapper" message])

      status
      (conj ["HTTP" (str status)])

      provider-id
      (conj ["Provider" (str provider-id)])

      request-id
      (conj ["Request id" (str request-id)]))))

(defn provider-error-raw-body
  "Truncated raw upstream body — surfaced ONLY when no structured provider
   message could be extracted (HTML pages, bare 5xx). nil otherwise so the
   readable message isn't echoed twice."
  [err]
  (let [body-raw
        (some-> (:body (:data err))
                str)

        structured-msg
        (provider-structured-message body-raw)]

    (when (and body-raw (not (str/blank? body-raw)) (not structured-msg))
      (truncate body-raw CHAT_ERROR_BODY_RENDER_CHARS))))

(defn provider-error-info
  "Structured echo of the facts a chat surface (Telegram) can render
   compactly without parsing the IR back out."
  [err]
  (let [message
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

(defn provider-error-ir
  "Canonical answer-IR for a provider failure. Rendered verbatim by the Web
   channel and the TUI final-answer bubble; the TUI trace rows mirror its
   wording via the helpers above. Blocks in reading order: TITLE → what happened
   → the actionable NEXT STEP → the bare facts → (optional) raw provider body.
   The root attrs carry `:vis/provider-error` + the full `:vis/provider-error-data`
   so a surface can paint a styled CARD instead of walking the generic nodes."
  [err]
  (let [facts
        (->> (provider-error-facts err)
             (mapv (fn [[label value]]
                     [:li {} [:p {} [:span {} (str label ": ")] [:c {} value]]])))

        attempts
        (provider-error-attempts err)

        raw-body
        (provider-error-raw-body err)

        ir
        (render/->ast (cond-> [:ir {} [:h {:level 2} [:span {} (provider-error-title err)]]
                               [:p {} [:span {} (provider-error-explanation err)]]
                               [:p {} [:strong {} [:span {} (provider-error-next-step err)]]]]
                        ;; Per-provider breakdown — WHY each provider bowed out — as
                        ;; its own list, so "all providers exhausted" is specific.
                        (seq attempts)
                        (conj [:p {} [:span {} "Providers tried:"]]
                              (into [:ul {}]
                                    (mapv (fn [a]
                                            [:li {} [:c {} (attempt->line a)]])
                                          attempts)))

                        :always
                        (conj (into [:ul {}] facts))

                        raw-body
                        (conj [:p {} [:span {} "Provider response:"]]
                              [:code {:lang "json"} raw-body])))]

    (assoc ir
      1 (assoc (second ir)
          :vis/provider-error true
          :vis/provider-error-data (provider-error-info err)))))
