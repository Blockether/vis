(ns com.blockether.vis.internal.provider.auth
  "Model-provider adapter to the shared gateway authentication lifecycle in `flow`.
   Registered providers own protocol start/complete/await and credential persistence;
   `flow` owns callback transport, expiry, cancellation, single exchange and verdicts.
   This adapter handles provider eligibility, API-key storage and fleet invalidation.
   No browser, relay, token or PKCE verifier crosses the public flow view."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.provider.flow :as auth-flow]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.extension.registry :as registry]
            [taoensso.telemere :as tel]))

(set! *unchecked-math* :warn-on-boxed)

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
  "Auth changed the credential file, so every cached status/limits view is stale —
   and so is the shared ROUTER, which SKIPS a provider whose credential could not be
   resolved when it was built. Rebuild it here or a sign-in never reaches routing:
   the provider stays absent, and a session pinned to it degrades onto whatever other
   vendor advertises the same model name. Invalidate before the caller re-reads
   `/v1/router`."
  []
  (try (providers/rebuild-shared-router!) (catch Throwable _ nil)))

(defn- settle!
  "Auth landed: drop cached fleet and limits views once, before publishing success."
  [provider-id]
  (refresh-fleet!)
  (try (provider-limits/flush-limits-cache! provider-id) (catch Throwable _ nil))
  nil)

(defn- provider-view
  [result]
  (if-let [flow (:flow result)]
    {:ok? true
     :flow (-> flow
               (assoc :provider-id (name (:subject flow)))
               (dissoc :subject :status :message))}
    result))

(defn start-auth!
  "Begin headless auth for `provider-id`. Returns `{:ok? true :flow {…}}` with
   the public flow view, or `{:ok? false :error kw :message str}`.

   THREE kinds, ONE wire shape, so a client never special-cases a provider:
   `pkce` uses the adapter's registered callback transport and accepts `complete-auth!`;
   `device` starts its background await; `api-key` asks the client to collect a
   key and hand it back to `complete-auth!`. Even plain key providers
   are persisted BY THE DAEMON, never by the calling process."
  [provider-id]
  (auth-flow/cancel-owner! [:provider provider-id])
  (let [{:keys [start await complete]} (auth-kinds provider-id)]
    (cond (self-minted? provider-id)
          {:ok? false
           :error :auth-self-minted
           :message (str
                      (name provider-id)
                      " mints its own credential with api_key_command — there is no key to enter")}
          (= :managed (providers/auth-kind provider-id))
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
          :else (try (provider-view
                       (auth-flow/start!
                         [:provider provider-id]
                         {:start (or start
                                     (fn []
                                       {:kind :api-key
                                        :instructions (api-key-instructions provider-id)}))
                          :complete (if start
                                      complete
                                      (fn [_ value]
                                        (providers/save-provider-api-key! provider-id value)))
                          :await await
                          :settle #(settle! provider-id)}))
                     (catch Throwable _
                       {:ok? false
                        :error :auth-start-failed
                        :message "Could not start authorization. Try signing in again."})))))

(defn complete-auth!
  "Complete a provider flow through the same callback validation and exchange as MCP."
  [flow-id input]
  (dissoc (auth-flow/complete! :provider flow-id input) :flow))

(defn poll-auth!
  "Read a retained browser/device verdict without blocking."
  [flow-id]
  (dissoc (auth-flow/poll! :provider flow-id) :flow))

(defn cancel-auth!
  "Forget a provider flow and stop its callback/device worker. Idempotent."
  [flow-id]
  (auth-flow/cancel! :provider flow-id))

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
  (auth-flow/cancel-owner! [:provider provider-id])
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
