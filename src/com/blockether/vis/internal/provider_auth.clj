(ns com.blockether.vis.internal.provider-auth
  "Headless, resumable OAuth for registered providers — the daemon-side engine
   behind `POST /v1/providers/:id/auth/{start,complete,poll}`.

   WHY THIS EXISTS. `:provider/auth-fn` is interactive: it prints prompts and
   blocks on `read-line` or a Lanterna dialog. That works only when the client
   IS the daemon's own terminal, which is exactly why the TUI reaches PAST the
   gateway and calls `provider-anthropic/login!` in-process. A phone, a browser
   tab, or any remote client cannot do that.

   This namespace splits the same flows into steps that survive a request
   boundary, keyed by a short-lived server-side flow id:

     start    -> mints a flow, returns what the USER must see (URL, user code)
     complete -> PKCE: exchange the pasted redirect URL           (Anthropic, Codex)
     poll     -> device: read the background await's verdict      (GitHub Copilot)

   SECURITY BOUNDARY. The provider's `:flow` value (PKCE verifier, CSRF state,
   device code) stays in this atom and is NEVER emitted. Credentials are
   written by the provider extension into the daemon's own auth file, exactly
   as the interactive path does — no token, verifier, or refresh token is ever
   serialized to a client. `public-view` is an allowlist, not a redaction pass."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.providers :as providers]
            [com.blockether.vis.internal.registry :as registry]
            [taoensso.telemere :as tel]))

(set! *unchecked-math* :warn-on-boxed)

(def ^:private default-flow-ttl-ms
  "How long an unfinished flow stays resumable. Device flows carry their own
   (usually 900s) expiry; this is the ceiling for everything else."
  (* 15 60 1000))

(defonce ^:private flows
  ;; flow-id -> {:id :provider-id :kind :created-at :expires-at :flow
  ;;             :public {…} :result (atom {:status :pending})}
  (atom {}))

(defn- now-ms ^long [] (System/currentTimeMillis))

(defn- new-flow-id [] (str (java.util.UUID/randomUUID)))

(defn- expired? [{:keys [expires-at]}] (boolean (and expires-at (> (now-ms) (long expires-at)))))

(defn- stop-await!
  "Stop a flow's background device poll. WITHOUT this, a cancelled or expired
   device flow leaves the provider's await leg hammering its endpoint for the
   rest of the device code's life — and a late success would write credentials
   for a flow the user already walked away from."
  [{:keys [await-future]}]
  (when await-future (future-cancel await-future))
  nil)

(defn- drop-flows!
  "Forget every flow matching `pred?` and stop its background poll. The ONLY way
   a flow leaves the atom, so no removal path can leak a running future."
  [pred?]
  (let [[before _] (swap-vals! flows
                               (fn [m]
                                 (into {} (remove (comp pred? val)) m)))]
    (run! stop-await! (filter pred? (vals before)))
    nil))

(defn- drop-flow!
  [flow-id]
  (drop-flows! (fn [e]
                 (= flow-id (:id e)))))

(defn- sweep!
  "Drop expired flows. Runs on every public op — a gateway with no auth traffic
   should not keep a timer alive just for this."
  []
  (drop-flows! expired?))

(defn- provider-descriptor [provider-id] (registry/provider-by-id provider-id))

(defn- auth-kinds
  "Which headless legs `provider-id` supports."
  [provider-id]
  (let [p (provider-descriptor provider-id)]
    {:start (:provider/auth-start-fn p)
     :complete (:provider/auth-complete-fn p)
     :await (:provider/auth-await-fn p)
     :logout (:provider/logout-fn p)}))

(defn- api-key-leg?
  "True when `provider-id` is REGISTERED, has no headless OAuth leg, and
   authenticates with a plain API key. The gateway still owns it: the client
   only collects the key, the DAEMON writes it into its OWN config."
  [provider-id]
  (and (some? (provider-descriptor provider-id))
       (nil? (:start (auth-kinds provider-id)))
       (= :api-key (providers/auth-kind provider-id))))

(defn- api-key-instructions
  "The provider's own guidance lines (`:provider/auth-prompt-fn`) for the
   API-key dialog. Never throws — a broken prompt must not block auth."
  [provider-id]
  (try (when-let [f (:provider/auth-prompt-fn (provider-descriptor provider-id))]
         (into [] (comp (map str) (remove str/blank?)) (f)))
       (catch Throwable _ nil)))

(defn- configured-provider
  "The configured provider map for `provider-id`, or nil when the fleet does
   not carry it. Cached — this sits on the auth request path."
  [provider-id]
  (let [pid (keyword (name provider-id))]
    (first (filter (fn [p]
                     (= pid (:id p)))
                   (providers/configured-providers-cached)))))

(defn self-minted?
  "True when configuration mints this provider's credential itself (an
   `api_key_command` helper). There is no key for a human to collect, so no
   auth flow may be offered for it — a key typed into a dialog would silently
   outrank the helper on the next request."
  [provider-id]
  (try (providers/command-minted? (configured-provider provider-id)) (catch Throwable _ false)))

(defn supported?
  "True when `provider-id` can be authenticated over the wire — OAuth (PKCE or
   device) or a plain API key — as opposed to only through the interactive
   terminal `:provider/auth-fn`."
  [provider-id]
  (boolean (and (not (self-minted? provider-id))
                (or (:start (auth-kinds provider-id)) (api-key-leg? provider-id)))))

(defn- refresh-fleet!
  "Auth changed the credential file, so every cached status/limits view is
   stale. Invalidate before the caller re-reads `/v1/router`."
  []
  (try (providers/invalidate-configured-providers!) (catch Throwable _ nil)))

(defn- public-view
  "The ONLY fields that may cross the wire. Allowlisted on purpose: the
   provider's `:flow` secret is structurally unable to leak through here."
  [{:keys [id provider-id kind public expires-at]}]
  (cond-> {:flow-id id
           :provider-id (name provider-id)
           :kind (name (or kind :pkce))
           :expires-at (long (or expires-at 0))}
    (:url public)
    (assoc :url (:url public))

    (:user-code public)
    (assoc :user-code (:user-code public))

    (:verification-uri public)
    (assoc :verification-uri (:verification-uri public))

    (:interval-ms public)
    (assoc :interval-ms (long (:interval-ms public)))

    (seq (:instructions public))
    (assoc :instructions (vec (:instructions public)))))

(defn- start-await!
  "Device flows finish on their own, but the provider's await leg BLOCKS for
   minutes. Run it once on a daemon thread and let `poll-auth!` read the
   verdict — an HTTP request must never sit on it."
  [{:keys [provider-id flow result]} await-fn]
  (future (try (await-fn flow)
               (refresh-fleet!)
               (reset! result {:status :ok})
               (catch Throwable t
                 (tel/log! :warn ["provider-auth: device flow failed" provider-id (ex-message t)])
                 (reset! result {:status :error
                                 :message (or (ex-message t) "device authorization failed")})))))

(defn start-auth!
  "Begin headless auth for `provider-id`. Returns `{:ok? true :flow {…}}` with
   the public flow view, or `{:ok? false :error kw :message str}`.

   THREE kinds, ONE wire shape, so a client never special-cases a provider:
   `pkce` stops here until `complete-auth!`, `device` already has a background
   await running by the time this returns, and `api-key` asks the client to
   collect a key and hand it back to `complete-auth!` — even plain key providers
   are persisted BY THE DAEMON, never by the calling process."
  [provider-id]
  (sweep!)
  ;; ONE live flow per provider. A client that retries `auth/start` — a phone
  ;; reconnecting, a dialog reopened, a nervous finger — must not stack device
  ;; polls or grow `flows` without bound: the newest attempt wins and the
  ;; previous one is stopped.
  (drop-flows! (fn [e]
                 (= provider-id (:provider-id e))))
  (let [{:keys [start await]} (auth-kinds provider-id)]
    (cond (self-minted? provider-id)
          {:ok? false
           :error :auth-self-minted
           :message (str
                      (name provider-id)
                      " mints its own credential with api_key_command — there is no key to enter")}
          (providers/managed? provider-id)
          {:ok? false
           :error :auth-managed
           :message (str (name provider-id)
                         " is managed — its credential is issued by the runtime, so there is no key"
                         " to enter or change")}
          (nil? (provider-descriptor provider-id))
          {:ok? false :error :unknown-provider :message (str "no registered provider " provider-id)}
          (and (nil? start) (not (api-key-leg? provider-id)))
          {:ok? false
           :error :auth-unsupported
           :message (str (name provider-id) " has no headless auth flow")}
          (nil? start) (let [id (new-flow-id)
                             entry {:id id
                                    :provider-id provider-id
                                    :kind :api-key
                                    :public {:instructions (api-key-instructions provider-id)}
                                    :created-at (now-ms)
                                    :expires-at (+ (now-ms) (long default-flow-ttl-ms))}]

                         (swap! flows assoc id entry)
                         (tel/log! :info ["provider-auth: flow started" provider-id "api-key"])
                         {:ok? true :flow (public-view entry)})
          :else
          (try (let [{:keys [kind flow expires-in-ms] :as started} (start)
                     id (new-flow-id)
                     entry {:id id
                            :provider-id provider-id
                            :kind (or kind :pkce)
                            :flow flow
                            :public (dissoc started :flow)
                            :created-at (now-ms)
                            :expires-at (+ (now-ms) (long (or expires-in-ms default-flow-ttl-ms)))
                            :result (atom {:status :pending})}
                     entry (if (and (= :device (:kind entry)) await)
                             (assoc entry :await-future (start-await! entry await))
                             entry)]

                 (swap! flows assoc id entry)
                 (tel/log! :info ["provider-auth: flow started" provider-id (name (:kind entry))])
                 {:ok? true :flow (public-view entry)})
               (catch Throwable t
                 (tel/log! :warn ["provider-auth: start failed" provider-id (ex-message t)])
                 {:ok? false
                  :error :auth-start-failed
                  :message (or (ex-message t) "could not start OAuth")})))))

(defn- claim-flow!
  "Atomically take the flow OUT of the atom so exactly one caller can spend it.
   A double-submitted dialog — or two clients racing on the same flow id — must
   not run the token exchange twice against an already-spent code; the loser
   sees `unknown-flow`."
  [flow-id]
  (let [[before _] (swap-vals! flows dissoc flow-id)]
    (get before flow-id)))

(defn- settle!
  "Auth landed: drop every cached view of the fleet. Never throws — the
   credential is already written, so a cache hiccup must not be reported back as
   a failed sign-in."
  [provider-id]
  (refresh-fleet!)
  (try (provider-limits/flush-limits-cache! provider-id) (catch Throwable _ nil))
  nil)

(defn- flow-by-id [flow-id] (sweep!) (get @flows flow-id))

(def ^:private max-input-chars
  "Nothing legitimate — a redirect URL or an API key — runs longer than this. A
   bigger payload is refused at the door: never handed to a provider, never
   written into the daemon's config."
  8192)

(defn- clean-input
  "The wire is JSON, so `input` is whatever the client posted. Only a plain
   string can be a credential — a map or vector would otherwise be `str`'d and
   persisted verbatim — and a pasted redirect URL nearly always carries a
   trailing newline. Returns `[value]`, `nil` when there is nothing usable, or
   `:invalid` for something structurally wrong."
  [input api-key?]
  (if-not (string? input)
    (when (some? input) :invalid)
    (let [v (str/trim input)]
      (cond (str/blank? v) nil
            (> (count v) (long max-input-chars)) :invalid
            ;; An API key is a single token; internal whitespace means the
            ;; client sent a multi-line paste, not a credential.
            (and api-key? (re-find #"\s" v)) :invalid
            :else [v]))))

(defn complete-auth!
  "Finish a flow the client cannot finish on its own: `pkce` takes the redirect
   URL (or bare `code#state`) the user pasted back, `api-key` takes the key the
   user typed and the DAEMON persists it into its own config. The flow is
   CLAIMED before the exchange runs, so a captured or double-submitted id cannot
   spend it twice; a failed exchange puts it back so a mistyped paste is
   retryable. Returns `{:ok? true :status \"ok\"}` or an error map."
  [flow-id input]
  (let [{:keys [provider-id kind] :as peek}
        (flow-by-id flow-id)

        complete-fn
        (:complete (auth-kinds provider-id))

        api-key?
        (= :api-key kind)

        cleaned
        (clean-input input api-key?)]

    (cond (nil? peek) {:ok? false :error :unknown-flow :message "unknown or expired flow"}
          (and (not api-key?) (nil? complete-fn))
          {:ok? false
           :error :auth-unsupported
           :message (str (name provider-id) " finishes on its own — poll instead of completing")}
          (nil? cleaned) {:ok? false
                          :error :missing-input
                          :message (if api-key? "api_key is required" "redirect_url is required")}
          (= :invalid cleaned) {:ok? false
                                :error :invalid-input
                                :message (str (if api-key? "api_key" "redirect_url")
                                              " must be a single-line string of at most "
                                              max-input-chars
                                              " characters")}
          :else (if-let [{:keys [flow] :as entry} (claim-flow! flow-id)]
                  (let [value (first cleaned)
                        failure (try (if api-key?
                                       (providers/save-provider-api-key! provider-id value)
                                       (complete-fn flow value))
                                     nil
                                     (catch Throwable t t))]

                    (if failure
                      (do
                        ;; A mistyped key or a truncated redirect URL is the common case,
                        ;; so hand the flow back instead of forcing a fresh sign-in.
                        (when-not (expired? entry) (swap! flows assoc flow-id entry))
                        (tel/log! :warn
                                  ["provider-auth: complete failed" provider-id
                                   (ex-message failure)])
                        {:ok? false
                         :error :auth-failed
                         :message (or (ex-message failure) "authorization failed")})
                      (do (stop-await! entry)
                          (settle! provider-id)
                          (tel/log! :info ["provider-auth: authenticated" provider-id])
                          {:ok? true :status "ok"})))
                  {:ok? false :error :unknown-flow :message "unknown or expired flow"}))))

(defn poll-auth!
  "Read a device flow's verdict without blocking: `pending`, `ok`, `error`, or
   `expired`. Consumes the flow once it settles."
  [flow-id]
  (let [{:keys [result] :as entry} (flow-by-id flow-id)]
    (cond (nil? entry) {:ok? false :error :unknown-flow :message "unknown or expired flow"}
          (nil? result) {:ok? false :error :auth-unsupported :message "this flow is not pollable"}
          :else (let [{:keys [status message]} @result]
                  (when (not= :pending status) (drop-flow! flow-id))
                  (when (= :ok status) (settle! (:provider-id entry)))
                  (cond-> {:ok? true :status (name status)}
                    message
                    (assoc :message message))))))

(defn cancel-auth!
  "Forget a flow the user abandoned AND stop its background device poll, so an
   abandoned sign-in stops talking to the provider immediately. Idempotent."
  [flow-id]
  (drop-flow! flow-id)
  {:ok? true :status "cancelled"})

(defn logout!
  "Clear `provider-id`'s persisted credentials, then invalidate the cached fleet so
   status flips on the next read.

   Two shapes of credential:
     - a registered `:logout` (OAuth/device providers) revokes the session;
     - everything else is an API key, which is cleared from the config entry.

   The CONFIG ENTRY always survives — logging out forgets the credential, never the
   provider's models/base-url. Returning `:auth-unsupported` for key providers made
   the gateway answer 400 and channels surface an ordinary logout as a fatal error."
  [provider-id]
  ;; A flow still in flight for this provider would land AFTER the logout and
  ;; silently re-authenticate it.
  (drop-flows! (fn [e]
                 (= provider-id (:provider-id e))))
  (let [logout-fn (:logout (auth-kinds provider-id))]
    (if-not logout-fn
      (try (let [cleared? (providers/clear-provider-api-key! provider-id :provider-auth-logout)]
             (settle! provider-id)
             (tel/log! :info ["provider-auth: cleared stored api key" provider-id])
             {:ok? true :status (if cleared? "logged-out" "not-authenticated")})
           (catch Throwable t
             {:ok? false :error :logout-failed :message (or (ex-message t) "logout failed")}))
      (try (logout-fn)
           (settle! provider-id)
           (tel/log! :info ["provider-auth: logged out" provider-id])
           {:ok? true :status "logged-out"}
           (catch Throwable t
             {:ok? false :error :logout-failed :message (or (ex-message t) "logout failed")})))))
