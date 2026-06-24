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
  (:require
   [charred.api :as json]
   [clojure.string :as str]
   [com.blockether.vis.internal.render :as render]))

(def ^:private CHAT_ERROR_BODY_RENDER_CHARS
  "Cap on raw upstream HTTP body chars surfaced in the chat error bubble.
   Long enough that full JSON error envelopes round-trip whole, short
   enough that a pathological 5xx HTML page can't take over the
   transcript."
  4000)

(defn- truncate [s n]
  (let [s (str s)] (if (> (count s) n) (subs s 0 n) s)))

(defn parse-provider-body
  [body]
  (when (and (string? body) (not (str/blank? body)))
    (try
      (json/read-json body :key-fn keyword)
      (catch Throwable _ nil))))

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
    (some-> body str/trim not-empty)))

(defn generic-wrapper-message?
  "True when the svar wrapper message is just a status echo (e.g.
   `Exceptional status code: 400`) that adds nothing beyond the HTTP row."
  [message]
  (boolean (when-let [t (some-> message str str/lower-case)]
             (or (str/includes? t "exceptional status code")
               (str/includes? t "provider error http")))))

(defn invalid-thinking-signature-message?
  [message]
  (boolean (and (string? message)
             (re-find #"(?i)invalid.*signature.*thinking.*block" message))))

(defn auth-provider-error?
  [status message wrapper-message]
  (let [text (str (or message "") "\n" (or wrapper-message ""))]
    (boolean
      (or (contains? #{401 403} status)
        (re-find #"(?i)(authentication|unauthorized|forbidden|credential|api[ -]?key|access[ -]?token|expired token|invalid token)"
          text)))))

(defn- rate-limit-error?
  [status message wrapper-message]
  (let [text (str/lower-case (str (or message "") "\n" (or wrapper-message "")))]
    (boolean (or (= 429 status)
               (str/includes? text "rate limit")
               (str/includes? text "rate-limit")))))

(defn- provider-id-of [data]
  (or (:provider-id data) (:provider data) (:provider/id data)))

(defn auth-provider-next-step
  [data]
  (let [provider-id (provider-id-of data)]
    (str "NEXT STEP: re-authenticate this provider or update its API key, then retry. "
      "TUI: Ctrl+K -> Model / Providers -> re-authenticate provider. "
      "CLI: run `vis providers auth"
      (when provider-id (str " " provider-id))
      "` for OAuth providers; for API-key providers, fix the configured key/env var and restart Vis.")))

(defn- rate-limit-next-step []
  "NEXT STEP: rate limit — wait and retry, re-authenticate, or switch provider/model.")

(defn provider-error-explanation
  "The `WHAT HAPPENED:` prose line — the single canonical human sentence
   for this failure, shared by every surface."
  [err]
  (let [message          (or (ex-message err) (:message err) (str err))
        data             (:data err)
        body-raw         (some-> (:body data) str)
        status           (:status data)
        provider-message (provider-body-message body-raw)]
    (cond
      (invalid-thinking-signature-message? provider-message)
      (str "WHAT HAPPENED: Anthropic rejected the request before the model ran because Vis sent a `thinking` block with a signature that is not valid for Anthropic. "
        "Most likely cause: preserved-thinking replay crossed a provider/model boundary (for example Z.ai/Codex/OpenAI reasoning state was replayed into Anthropic), or an old Anthropic thinking block came from a different session/key. "
        "Fix: do not replay preserved-thinking unless provider AND model match; retry with only normal transcript/trailer context.")

      (auth-provider-error? status provider-message message)
      (str "WHAT HAPPENED: provider rejected credentials before the model ran."
        (when (seq provider-message)
          (str " Provider message: " provider-message))
        " "
        (auth-provider-next-step data))

      (rate-limit-error? status provider-message message)
      (str "WHAT HAPPENED: provider rejected the request before the model ran because of rate limiting."
        (when (seq provider-message)
          (str " Provider message: " provider-message))
        " "
        (rate-limit-next-step))

      (= "All providers exhausted" message)
      "WHAT HAPPENED: Vis tried the configured provider route/fallbacks, but every provider attempt failed before a usable response was available. The transcript/tool results are still intact; retry after checking provider availability, auth, quota, or network."

      (seq provider-message)
      (str "WHAT HAPPENED: provider rejected the request before the model ran. Provider message: " provider-message)

      :else
      "WHAT HAPPENED: provider rejected the request before the model ran.")))

(defn provider-error-kind
  [err]
  (let [message          (or (ex-message err) (:message err) (str err))
        data             (:data err)
        body-raw         (some-> (:body data) str)
        status           (:status data)
        provider-message (provider-body-message body-raw)]
    (cond
      (invalid-thinking-signature-message? provider-message)             :invalid-thinking-signature
      (auth-provider-error? status provider-message message)             :auth
      (rate-limit-error? status provider-message message)                :rate-limit
      :else                                                              :generic)))

(defn provider-error-facts
  "Ordered `[label value]` rows of the bare facts (no prose). Same set the
   IR renders as a `<ul>` and the TUI renders as plain rows."
  [err]
  (let [message     (or (ex-message err) (:message err) (str err))
        data        (:data err)
        status      (:status data)
        request-id  (or (:request-id data) (:request_id data))
        provider-id (provider-id-of data)]
    (cond-> []
      (and (seq message) (not (generic-wrapper-message? message)))
      (conj ["Wrapper" message])
      status      (conj ["HTTP" (str status)])
      provider-id (conj ["Provider" (str provider-id)])
      request-id  (conj ["Request id" (str request-id)]))))

(defn provider-error-raw-body
  "Truncated raw upstream body — surfaced ONLY when no structured provider
   message could be extracted (HTML pages, bare 5xx). nil otherwise so the
   readable message isn't echoed twice."
  [err]
  (let [body-raw       (some-> (:body (:data err)) str)
        structured-msg (provider-structured-message body-raw)]
    (when (and body-raw (not (str/blank? body-raw)) (not structured-msg))
      (truncate body-raw CHAT_ERROR_BODY_RENDER_CHARS))))

(defn provider-error-info
  "Structured echo of the facts a chat surface (Telegram) can render
   compactly without parsing the IR back out."
  [err]
  (let [message          (or (ex-message err) (:message err) (str err))
        data             (:data err)
        body-raw         (some-> (:body data) str)
        status           (:status data)
        request-id       (or (:request-id data) (:request_id data))
        provider-message (provider-body-message body-raw)]
    {:kind             (provider-error-kind err)
     :status           status
     :request-id       (some-> request-id str)
     :provider-message (not-empty provider-message)
     :wrapper-message  (not-empty message)
     :provider-id      (provider-id-of data)
     :body             (provider-error-raw-body err)}))

(defn provider-error-ir
  "Canonical answer-IR for a provider failure. Rendered verbatim by the
   Web channel and the TUI final-answer bubble; the TUI trace rows mirror
   its wording via the helpers above."
  [err]
  (let [facts    (->> (provider-error-facts err)
                   (mapv (fn [[label value]]
                           [:li {} [:p {} [:span {} (str label ": ")] [:c {} value]]])))
        raw-body (provider-error-raw-body err)
        ir       (render/->ast
                   (cond-> [:ir {}
                            [:h {:level 2} [:span {} "Provider unavailable"]]
                            [:p {} [:strong {} [:span {} "The model provider failed before Vis received a usable response."]]]
                            [:p {} [:strong {} [:span {} (provider-error-explanation err)]]]
                            (into [:ul {}] facts)]
                     raw-body
                     (conj [:p {} [:span {} "Provider response:"]]
                       [:code {:lang "json"} raw-body])))]
    (assoc ir 1 (assoc (second ir)
                  :vis/provider-error true
                  :vis/provider-error-data (provider-error-info err)))))
