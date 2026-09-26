(ns com.blockether.vis.internal.gateway.server.providers
  "Provider routes: the model catalog, provider accounts and presets,
   authentication, status, limits and routing."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.provider.auth :as provider-auth]
            [com.blockether.vis.internal.provider.catalog :as catalog]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.provider.service :as providers]))

(defn- models-handler
  [_]
  (http/json-response
    {:providers (mapv (fn [{:provider/keys [id doc]}]
                        {:id (name id) :doc doc})
                      (registry/registered-providers))
     ;; Configured fleet with per-provider model names — the same source every
     ;; channel's model picker renders (`configured-providers`), so no channel
     ;; needs its own catalog route.
     :catalog (into []
                    (keep (fn [{:keys [id models]}]
                            (let [names (into [] (keep :name) models)]
                              (when (seq names)
                                {:id (name id) :label (config/display-label id) :models names}))))
                    (providers/configured-providers))}))

(defn- configured-provider
  [provider-id]
  (or (some (fn [provider]
              (when (= provider-id (:id provider)) provider))
            (providers/configured-providers))
      {:id provider-id}))

(defn- provider-status-handler
  [request]
  (let [provider-id (some-> (get-in request [:path-params :provider-id])
                            keyword)]
    ;; A recheck is the user looking straight at this provider, which is the
    ;; moment to pull its live catalog too — off-thread, so a fleet configured by
    ;; an older build stops advertising that build's models without this answer
    ;; ever waiting for a `/models` call.
    (providers/refresh-models-async! provider-id :gateway)
    (http/json-response {:status (providers/provider-status (configured-provider provider-id))})))

(defn- provider-limits-handler
  [request]
  (let [provider-id (some-> (get-in request [:path-params :provider-id])
                            keyword)]
    (http/json-response {:report (provider-limits/provider-limits provider-id)})))

(defn- provider-consume-reset-credit-handler
  "POST /v1/providers/:provider-id/reset-credits/consume.
   Account identity and a stable idempotency key are required; credentials never
   leave the daemon. Unknown results are errors, not optimistic quota updates."
  [request]
  (let [body
        (try (http/body-json request) (catch Exception _ nil))

        provider-id
        (some-> (get-in request [:path-params :provider-id])
                keyword)

        {:keys [error message] :as result}
        (provider-limits/consume-reset-credit! provider-id
                                               {:account-id (get body "account_id")
                                                :idempotency-key (get body "idempotency_key")})]

    (if error
      (http/error-response (case error
                             :unknown-provider
                             404

                             (:reset-unsupported :invalid-reset-request)
                             400

                             :account-changed
                             409

                             502)
                           error
                           message)
      (http/json-response result))))

(defn- provider-models-handler
  "GET /v1/providers/:provider-id/models[?show_all=true] — the LIVE model
   catalog for ONE provider, fetched DAEMON-side so the gateway stays the SOLE
   owner of OAuth token resolution. The `svar/models!` probe (and any OAuth
   token refresh it triggers) runs HERE, in the daemon that owns the credential
   file — never in a thin client. Returns `{models [id …] hidden_count n}` with
   snake_case STRING wire keys."
  [request]
  (let [provider-id
        (some-> (get-in request [:path-params :provider-id])
                keyword)

        show-all?
        (contains? #{"1" "true" "yes"}
                   (some-> (get-in request [:query-params "show_all"])
                           str/lower-case))

        provider
        (configured-provider provider-id)

        {:keys [models hidden-count]}
        (providers/model-options provider (providers/default-model-names provider) show-all?)]

    (http/json-response {:models (vec models) :hidden-count (long (or hidden-count 0))})))

(defn- auth-error-response
  "Map a `provider-auth` failure map onto an HTTP status. Unknown provider/flow
   is 404, an unsupported or malformed request is 400, and a genuine upstream
   OAuth failure is 502 — the caller can tell 'you asked wrong' from 'GitHub
   said no'."
  [{:keys [error message]}]
  (http/error-response (case error
                         (:unknown-provider :unknown-flow)
                         404

                         (:auth-unsupported :auth-self-minted :missing-input :invalid-input)
                         400

                         502)
                       (or error :auth-failed)
                       (or message "authorization failed")))

(defn- provider-auth-start-handler
  "POST /v1/providers/:provider-id/auth/start — mint a headless OAuth flow.

   Answers `{flow_id, kind, url, user_code?, verification_uri?, interval_ms?,
   instructions?}`. `kind` is `pkce` (finish with `auth/complete`) or `device`
   (finish by polling `auth/poll`). The PKCE verifier and device code stay in
   the daemon and never appear in this response."
  [request]
  (let [provider-id
        (some-> (get-in request [:path-params :provider-id])
                keyword)

        result
        (provider-auth/start-auth! provider-id)]

    (if (:ok? result) (http/json-response (:flow result)) (auth-error-response result))))

(defn- provider-auth-complete-handler
  "POST /v1/providers/:provider-id/auth/complete {flow_id, redirect_url|api_key} —
   finish the flow the client cannot finish alone: a PKCE flow with the URL the
   user pasted back from the browser, or an `api-key` flow with the key the user
   typed. Credentials are exchanged and persisted DAEMON-side; the response
   carries only `{status}`."
  [request]
  (let [body
        (try (http/body-json request) (catch Throwable _ nil))

        flow-id
        (or (get body "flow_id") (get-in request [:query-params "flow_id"]))

        input
        (or (get body "redirect_url")
            (get body "api_key")
            (get body "code")
            (get-in request [:query-params "redirect_url"]))

        result
        (provider-auth/complete-auth! flow-id input)]

    (if (:ok? result)
      (http/json-response {:status (:status result)})
      (auth-error-response result))))

(defn- provider-auth-poll-handler
  "POST /v1/providers/:provider-id/auth/poll {flow_id} — read a device flow's
   verdict WITHOUT blocking: `pending`, `ok`, or `error`. The blocking wait
   runs on a daemon thread from the moment `auth/start` returned, so a phone
   can poll this on any cadence it likes."
  [request]
  (let [body
        (try (http/body-json request) (catch Throwable _ nil))

        flow-id
        (or (get body "flow_id") (get-in request [:query-params "flow_id"]))

        result
        (provider-auth/poll-auth! flow-id)]

    (if (:ok? result)
      (http/json-response (select-keys result [:status :message]))
      (auth-error-response result))))

(defn- provider-auth-cancel-handler
  "POST /v1/providers/:provider-id/auth/cancel {flow_id} — forget an abandoned
   flow. Idempotent, so a client that lost track of its flow can always call it."
  [request]
  (let [body
        (try (http/body-json request) (catch Throwable _ nil))

        flow-id
        (or (get body "flow_id") (get-in request [:query-params "flow_id"]))]

    (http/json-response (select-keys (provider-auth/cancel-auth! flow-id) [:status]))))

(defn- provider-logout-handler
  "POST /v1/providers/:provider-id/logout — clear the provider's persisted
   credentials through its registered logout and invalidate the cached fleet,
   so the very next `/v1/router` read shows `is_authenticated` false."
  [request]
  (let [provider-id
        (some-> (get-in request [:path-params :provider-id])
                keyword)

        result
        (provider-auth/logout! provider-id)]

    (if (:ok? result)
      (http/json-response {:status (:status result)})
      (auth-error-response result))))

(defn- router-provider-entry
  "One row of the unified router payload, carrying both explicit tags: the
   PRIMARY pair every turn starts on and the FALLBACK pair on another provider.

   `probe?` decides what this row's `:status` and `:limits` COST: true asks the
   provider live, false answers from what the daemon already knows. Everything
   else in the row is config, and free either way."
  [provider primary fallback probe?]
  (let [id
        (:id provider)

        is-default
        (= id (:provider-id primary))

        is-fallback
        (= id (:provider-id fallback))

        ;; Normalize metadata without resolving credentials or probing a provider.
        ;; The TUI gates controls on these Svar facts, not on provider/model names.
        models
        (when (seq (:models provider))
          (catalog/normalize-models
            0
            {:id id
             :base-url (config/provider-base-url provider)
             :api-style (config/effective-api-style {:declared (config/provider-api-style provider)
                                                     :responses-path (:responses-path provider)})
             :models (into [] (keep config/->svar-model) (:models provider))}))]

    {:id (name id)
     :label (config/display-label id)
     :is-managed (providers/managed? id)
     :base-url (or (config/provider-base-url provider) (:base-url provider))
     :models (into [] (keep :name) (:models provider))
     :model-details (mapv (fn [model]
                            {:name (:name model)
                             :is-reasoning-effort-configurable (:reasoning-effort? model)
                             :verbosity-style (:verbosity-style model)})
                          models)
     :is-default is-default
     :default-model (when is-default (:model primary))
     :is-fallback is-fallback
     :fallback-model (when is-fallback (:model fallback))
     :status
     (if probe? (providers/provider-status provider) (providers/provider-status-cached provider))
     :limits (if probe?
               (providers/provider-limits-safe provider)
               (provider-limits/limits-without-fetching id))}))

(defn- router-selection-json
  "Both router tags as one payload, `null` where a role is untagged. Answered by
   every PATCH so a client repaints without re-reading the catalog."
  []
  (let [fleet
        (providers/picker-fleet)

        primary
        (providers/default-selection fleet)

        fallback
        (providers/fallback-selection fleet primary)]

    {:default-provider (some-> (:provider-id primary)
                               name)
     :default-model (:model primary)
     :fallback-provider (some-> (:provider-id fallback)
                                name)
     :fallback-model (:model fallback)}))

(defn- router-fleet-json
  "The whole provider catalog with both explicit tags.

   Every fleet MUTATION answers with this exact payload, so a client that just
   added or removed a provider repaints from the response it already holds —
   no second read, and no window where the phone shows a fleet the daemon no
   longer has.

   `probe?` is what that costs. `GET /v1/router` asks every provider live. A
   MUTATION passes false: adding a provider used to re-probe the auth and quota
   endpoints of every OTHER provider before answering, so tapping Add sat on a
   spinner for seconds before the API-key box — which needs neither — appeared.
   The payload shape is identical either way."
  ([] (router-fleet-json true))
  ([probe?]
   (let [fleet
         (providers/picker-fleet)

         primary
         (providers/default-selection fleet)

         fallback
         (providers/fallback-selection fleet primary)]

     ;; One PROBING row costs a LIVE auth (and limits) probe against that
     ;; provider — seconds each. Serially, a fleet of eight took ~60s, past the
     ;; companion's 30s request bound, so the Providers screen sat on "Checking
     ;; provider sign-in…" forever. Probing the fleet in parallel makes the
     ;; payload cost the SLOWEST provider instead of their sum; each row already
     ;; answers a report rather than throwing.
     {:providers (->> fleet
                      (mapv (fn [provider]
                              (future (router-provider-entry provider primary fallback probe?))))
                      (mapv deref))})))

(defn- router-handler
  "GET /v1/router — the whole provider catalog and both explicit tags."
  [_]
  (http/json-response (router-fleet-json)))

(defn- router-default-handler
  "PATCH /v1/router — tag one provider/model pair.

   `role` selects the tag: `primary` (the default, and what every client written
   before roles sends) or `fallback`, which the daemon REFUSES on the primary's
   own provider. `{\"role\": \"fallback\"}` with no provider and no model clears
   the fallback. The answer always carries both tags."
  [request]
  (let [{:strs [provider model role]}
        (http/body-json request)

        role
        (or (some-> role
                    str
                    str/trim
                    str/lower-case
                    not-empty)
            "primary")

        is-blank
        (and (str/blank? (str provider)) (str/blank? (str model)))]

    (cond (not (contains? #{"primary" "fallback"} role))
          (http/error-response 400 :invalid-request "role must be \"primary\" or \"fallback\"")
          (and (= role "fallback") is-blank) (do (providers/clear-fallback-selection! :gateway)
                                                 (http/json-response (router-selection-json)))
          is-blank
          (http/error-response 400 :invalid-request "provider and model must be non-blank strings")
          :else (try (if (= role "fallback")
                       (providers/save-fallback-selection! provider model :gateway)
                       (providers/save-default-selection! provider model :gateway))
                     (http/json-response (router-selection-json))
                     (catch clojure.lang.ExceptionInfo e
                       (http/error-response 400 :invalid-request (ex-message e)))))))

(defn- provider-preset-json
  "One 'Add Provider' row: what the preset IS, and what adding it will ask for.

   `auth_kind` tells the client which second step follows the add — `oauth`
   (start a flow), `api-key` (collect a key), `none` (a local runtime needs
   neither) — and `is_local` marks the presets whose `base_url` the user OWNS,
   since LM Studio and Ollama listen wherever that machine put them."
  [preset]
  (let [pid (:id preset)]
    (cond-> {:id (name pid)
             :label (or (:label preset) (config/display-label pid))
             :auth-kind (name (providers/auth-kind pid))
             :is-local (contains? providers/local-no-auth-provider-ids pid)
             :models (mapv :name (providers/default-model-configs preset))}
      (:base-url preset)
      (assoc :base-url (:base-url preset))

      (:api-style preset)
      (assoc :api-style (name (:api-style preset))))))

(defn- provider-presets-handler
  "GET /v1/provider-presets — every provider this machine knows how to add and
   does NOT carry yet. This is the 'Add Provider' picker, headless: without it a
   client can only ever operate the providers that are already configured."
  [_]
  (http/json-response {:presets (mapv provider-preset-json (providers/available-presets))}))

(defn- add-provider-handler
  "POST /v1/providers {id, base_url?} — put a preset into THIS machine's fleet.

   The daemon owns config: a client names a preset and, for a LOCAL provider,
   where it listens; the models come from the preset. No credential is accepted
   here — a fresh provider starts signed out and finishes through
   `/v1/providers/:id/auth/*`, which is the ONE path that writes a key, on the
   machine that owns it."
  [request]
  (let [body
        (try (http/body-json request) (catch Throwable _ nil))

        raw-id
        (some-> (get body "id")
                str
                str/trim
                not-empty)

        provider-id
        (some-> raw-id
                keyword)

        preset
        (some-> provider-id
                catalog/template)

        base-url
        (some-> (get body "base_url")
                str
                str/trim
                (str/replace #"/+$" "")
                not-empty)

        configured
        (into #{} (map :id) (providers/configured-providers))]

    (cond (nil? provider-id)
          (http/error-response 400 :invalid-request "id must be a non-blank provider id")
          (nil? preset)
          (http/error-response 404 :unknown-provider (str "no such provider preset: " raw-id))
          (contains? configured provider-id)
          (http/error-response 409 :provider-exists (str raw-id " is already configured"))
          :else (let [preset
                      (cond-> preset
                        base-url
                        (assoc :base-url base-url))

                      models
                      (providers/default-model-configs preset)]

                  (providers/add-config-provider! (providers/provider-config-with-models preset
                                                                                         models)
                                                  :gateway)
                  ;; The preset's catalog is the one this BUILD shipped. Pull the
                  ;; vendor's live list off-thread, so a model released since then is
                  ;; selectable moments later instead of never — and the key dialog
                  ;; this answer opens still paints immediately.
                  (providers/refresh-models-async! provider-id :gateway)
                  (http/json-response (router-fleet-json false))))))

(defn- remove-provider-handler
  "DELETE /v1/providers/:provider-id — remove a user-owned provider and its credential.
   Extension-managed providers return 409 without mutation. Otherwise idempotent:
   `is_removed` is true once the provider is absent, even if it was never configured."
  [request]
  (let [provider-id (some-> (get-in request [:path-params :provider-id])
                            keyword)]
    (try (when provider-id (providers/remove-provider! provider-id :gateway))
         (let [fleet (router-fleet-json false)
               survivor (some #(= provider-id
                                  (some-> (:id %)
                                          keyword))
                              (:providers fleet))]

           (http/json-response (assoc fleet :is-removed (not survivor))))
         (catch clojure.lang.ExceptionInfo e
           (if (= :provider/managed (:type (ex-data e)))
             (http/error-response 409 :provider-managed (ex-message e))
             (throw e))))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/models"] models-handler
   [:post "/v1/providers"] add-provider-handler
   [:get "/v1/provider-presets"] provider-presets-handler
   [:delete "/v1/providers/:provider-id"] remove-provider-handler
   [:get "/v1/providers/:provider-id/status"] provider-status-handler
   [:get "/v1/providers/:provider-id/limits"] provider-limits-handler
   [:post "/v1/providers/:provider-id/reset-credits/consume"] provider-consume-reset-credit-handler
   [:get "/v1/providers/:provider-id/models"] provider-models-handler
   [:post "/v1/providers/:provider-id/auth/start"] provider-auth-start-handler
   [:post "/v1/providers/:provider-id/auth/complete"] provider-auth-complete-handler
   [:post "/v1/providers/:provider-id/auth/poll"] provider-auth-poll-handler
   [:post "/v1/providers/:provider-id/auth/cancel"] provider-auth-cancel-handler
   [:post "/v1/providers/:provider-id/logout"] provider-logout-handler
   [:get "/v1/router"] router-handler
   [:patch "/v1/router"] router-default-handler})
