(ns com.blockether.vis.internal.loop.router
  "The process-wide router and per-request provider routing.

   Builds and rebuilds the shared svar router from configuration, hydrates
   provider credentials and model metadata, recovers from a rejected credential
   by refreshing or rerouting, resolves the effective model and its context
   budget, and estimates request cost, including a provider's fast mode."
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.runtime-settings :as rt]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.context.engine :as ctx-engine]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.provider.auth-health :as auth-health]
            [com.blockether.vis.internal.provider.error :as perr]
            [com.blockether.vis.internal.provider.catalog :as catalog]
            [com.blockether.vis.internal.provider.limits :as provider-limits]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.internal.python.extensions :as python-extensions]
            [com.blockether.vis.internal.session.model :as session-model]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel]))

(defn normalize-reasoning-level [v] (svar/normalize-reasoning-level v))

(defn- adaptive-reasoning-model?
  "True when `resolved-model`'s provider declares, in its policy's
  `:adaptive-reasoning-models`, that this model chooses its own thinking depth."
  [resolved-model]
  (let [pattern (:adaptive-reasoning-models (catalog/policy (:provider resolved-model)))]
    (boolean (and pattern (re-find (re-pattern pattern) (str (:name resolved-model)))))))

(def ^:private PROVIDER_FALLBACK_TOGGLE
  "Feature-toggle id gating every AUTOMATIC route away from the provider+model a
   session picked. Registered once in `toggles`, so it appears in the TUI Settings
   dialog and the companion Settings sheet from that one declaration."
  "provider_fallback")

(defn provider-fallback-allowed?
  "True while a failed turn may be rescued on a DIFFERENT provider or model.

   OFF makes the session's pick a contract: a dead credential, an exhausted rate
   limit or a broken wire ends the turn with that provider's own error instead
   of quietly answering from somewhere else. Fallback is never free — the peer's
   prompt cache is cold (~4x input spend for the rest of the session, issue #154)
   and the answer arrives from a model the human did not choose — so whether to pay
   that is theirs to decide.

   Reads the VALUE rather than `enabled?`: an unregistered id (a JVM that never
   loaded the toggle defaults) must keep TODAY's rescue behaviour, while `enabled?`
   is deliberately fail-CLOSED, which here would strand every turn on one provider."
  []
  (not (false? (toggles/value-of PROVIDER_FALLBACK_TOGGLE))))

(def ^:private REFUSAL_FALLBACK_TOGGLE
  "Feature-toggle id gating the automatic switch to a SIBLING MODEL after Anthropic's
   safety classifier declines a request. Deliberately NOT `provider_fallback`: a
   refusal is an HTTP-200 content decision by one model, with a healthy credential,
   provider and wire behind it, so a human who refused peer credentials has not
   thereby refused the one recovery Anthropic itself documents."
  "refusal_fallback")

(defn- refusal-fallback-allowed?
  "True while a DECLINED request may be re-asked of a sibling model of the SAME
   provider.

   OFF surfaces the refusal itself: the turn ends with the decline and its category
   instead of an answer from a model the human did not pick. Reads the VALUE rather
   than `enabled?` for the same reason as [[provider-fallback-allowed?]] — an
   unregistered id must keep TODAY's behaviour, not strand the turn."
  []
  (not (false? (toggles/value-of REFUSAL_FALLBACK_TOGGLE))))

(defn refusal-fallbacks-for
  "The refusal-fallback chain for `resolved-model` WITHIN its own provider, or nil.
   The provider's policy names the models whose safety classifier can decline a
   request (`stop_reason: refusal`) and the ordered models to retry it on
   (`:refusal-fallback`); svar owns the actual client-side switch. The current
   model is dropped — an identical retry earns the identical decline.

   Every candidate is checked against the models `router` says that provider actually
   serves. svar switches by handing the name back as `:routing {:model …}`, which the
   router turns into `:force-model`: a name this provider does not serve either dies
   as a routing failure naming no credential, or resolves on ANOTHER provider — a
   content decision quietly moving the session's billing and its cache. No router, or
   no sibling on that provider, therefore means no chain and the refusal surfaces as
   itself.

   nil while `refusal_fallback` is off."
  [router resolved-model]
  (when (refusal-fallback-allowed?)
    (let [nm
          (str (:name resolved-model))

          provider-id
          (some-> (:provider resolved-model)
                  name
                  keyword)

          {:keys [models fallbacks]}
          (:refusal-fallback (catalog/policy provider-id))

          served
          (into #{}
                (map #(str (:name %)))
                (:models (some #(when (= provider-id (:id %)) %) (:providers router))))]

      (when (and models (re-find (re-pattern models) nm))
        (not-empty (into [] (comp (remove #{nm}) (filter served)) fallbacks))))))

(defn pin-routing-to-model
  "Routing svar cannot walk away from once the human turned `provider_fallback` off.

   Fallback ON returns `routing` untouched — today's rescue ladder decides. Fallback OFF
   stamps the model THIS call already resolved to as an explicit `:provider`/`:model` pin,
   so svar's own provider walk has nowhere to land and a failure comes back as the pinned
   provider's own error instead of a peer's answer. A half-named resolution (no provider,
   or a blank name) is left alone: pinning half a route names a different model, not this
   one."
  [routing resolved-model]
  (let [model-name (not-empty (str (:name resolved-model)))]
    (cond-> (or routing {})
      (and (not (provider-fallback-allowed?)) (:provider resolved-model) model-name)
      (assoc :provider
        (:provider resolved-model) :model
        model-name))))

(defn pin-routing-to-provider
  "Routing a REFUSAL switch cannot walk out of.

   svar re-asks a declined request by replacing `:routing {:model …}` and keeping the
   rest of that map, so a routing which never named a provider lets the router resolve
   the fallback name wherever it is cheapest — a peer credential answering for a
   content decision the original provider made. Stamping the provider this call already
   resolved to keeps the switch inside it. A resolution with no provider is left alone:
   half a pin routes to a different model, not this one."
  [routing resolved-model]
  (let [provider-id (some-> (:provider resolved-model)
                            name
                            keyword)]
    (cond-> (or routing {})
      (and provider-id (not (contains? routing :provider)))
      (assoc :provider provider-id))))

(defn provider-network-policy
  "Provider defaults baked into the current router, resolved at the request boundary."
  [router resolved-model]
  (let [provider-id (some-> (:provider resolved-model)
                            name
                            keyword)]
    (:network (some #(when (= provider-id (:id %)) %) (:providers router)))))

(defn provider-watchdog-timeouts
  "Keep gateway backstops outside the provider-owned network deadlines."
  [provider-network]
  (when (seq provider-network)
    (let [effective
          (rt/with-default-ask-code-idle-timeout {} provider-network)

          positive-ms
          (fn [v]
            (when (and (number? v) (pos? (long v))) (long v)))

          ttft-ms
          (positive-ms (:ttft-timeout-ms effective))

          body-limits
          (keep (comp positive-ms effective)
                [:first-byte-timeout-ms :idle-timeout-ms :semantic-timeout-ms])

          body-ms
          (when (seq body-limits) (apply min body-limits))

          stream-limits
          (keep (comp positive-ms effective) [:idle-timeout-ms :semantic-timeout-ms])

          stream-ms
          (when (seq stream-limits) (apply max stream-limits))

          whole-ms
          (positive-ms (:timeout-ms effective))

          first-output-ms
          (when (and ttft-ms body-ms) (+ (long ttft-ms) (long body-ms)))

          bounded
          (fn [phase-ms]
            (or (when (and whole-ms phase-ms) (min (long whole-ms) (long phase-ms)))
                phase-ms
                whole-ms))]

      {:first-output-timeout-ms (bounded first-output-ms) :stall-timeout-ms (bounded stream-ms)})))

(defn- with-provider-network-defaults
  [router opts]
  (rt/with-default-ask-code-idle-timeout
    opts
    (provider-network-policy router (svar/resolve-effective-model router (:routing opts)))))

(def ^:private casual-request-pattern
  #"(?iu)^\s*(hi|hey|hello|yo|sup|siema|cześć|czesc|hej|dzień dobry|dzie dobry|thanks|thank you|thx|ok|okay|👍|👋)[\s!.?,]*\s*$")

(defn- casual-user-request?
  [s]
  (let [text (some-> s
                     str
                     str/trim)]
    (boolean (and text (<= (count text) 80) (re-find casual-request-pattern text)))))

(defn casual-reasoning-level
  "Return the reasoning level Vis sends for `user-request` to `resolved-model`.

  Only casual chat to a model that chooses its own thinking depth
  (`:adaptive-reasoning-models` in its provider's policy) is special-cased: a
  bare greeting names no depth, and the model's adaptive thinking then decides
  for itself whether the turn is worth thinking about. Every other request keeps
  `reasoning-level`."
  [resolved-model user-request reasoning-level]
  (if (and (adaptive-reasoning-model? resolved-model) (casual-user-request? user-request))
    nil
    reasoning-level))

(defn initiator-llm-headers
  "`{header initiator}` when `resolved-model`'s provider names an
  `:initiator-header` in its policy and `initiator` is \"user\" or \"agent\";
  nil otherwise. The provider bills by who started the call."
  [resolved-model initiator]
  (when-let [header (:initiator-header (catalog/policy (:provider resolved-model)))]
    (when (#{"user" "agent"} initiator) {header initiator})))

(defn iteration-initiator
  "Who started iteration `iteration` of a turn: the person for the first, Vis
  for every tool-call continuation."
  [iteration]
  (if (zero? (long (or iteration 0))) "user" "agent"))

;; Router lifecycle + model helpers

(defonce ^:private router-atom (atom nil))

(defn- enrich-provider-models
  "Apply a provider's optional `:provider/enrich-models-fn` hook to a
   svar-shaped provider at router-build time. Providers whose backend can
   report a model's real context window (LM Studio via its native endpoint)
   register this hook to resolve `:context`/`:tool-call?`; the host stays
   provider-agnostic — no per-provider branching here.

   Runs only at router build (`get-router` / `rebuild-router!`, both memoized
   via `router-atom`), so any network the hook does is once-per-build, never
   per turn. Failure-safe: a throwing or empty hook leaves models untouched and
   svar falls back to its conservative DEFAULT_CONTEXT_LIMIT."
  [svar-provider router-opts]
  (if-let [f (:provider/enrich-models-fn (registry/provider-by-id (:id svar-provider)))]
    (try (let [models (f svar-provider router-opts)]
           (cond-> svar-provider
             (seq models)
             (assoc :models (vec models))))
         (catch Throwable _ svar-provider))
    svar-provider))

(defn- boot-refresh-provider-token!
  "Build-time sibling of `try-refresh-provider-token!`.

   When Svar classifies a router-build failure as authentication and the
   provider exposes `:provider/refresh-token-fn`, force one credential mutation
   so the caller can make a new build request. Svar remains the sole failure
   classifier; this function only refreshes a credential Svar cannot mint.

   Unlike the mid-turn path, it must not rebuild the router recursively."
  [pid ^Throwable t]
  (let [provider
        (registry/provider-by-id pid)

        f
        (:provider/refresh-token-fn provider)]

    (boolean
      (when (and f
                 (= :auth (:category (perr/svar-classification t)))
                 (auth-health/refresh-allowed? pid))
        (let [rejected (config/baked-token pid)]
          (try (try (f rejected) (catch clojure.lang.ArityException _ (f)))
               (provider-limits/auth-changed! pid)
               (tel/log! {:level :warn :id ::boot-auth-token-refreshed :data {:provider pid}}
                         (str "Provider build hit auth error — force-refreshed OAuth token for "
                              pid
                              "; retrying build"))
               true
               (catch Throwable rt
                 (tel/log! {:level :warn
                            :id ::boot-auth-token-refresh-failed
                            :data {:provider pid :error (ex-message rt)}}
                           (str "Provider build auth refresh FAILED for " pid "; skipping"))
                 false)))))))

(defn- boot-refresh-credential-command!
  "Command-backed sibling of `boot-refresh-provider-token!`.

   Svar decides whether the failure is authentication. This function only
   invalidates a short-lived command credential so a subsequent build request
   can carry a new token; `auth-health/refresh-allowed?` bounds that mutation."
  [p ^Throwable t]
  (let [pid (:id p)]
    (boolean (when (and (:api-key-command p)
                        (= :auth (:category (perr/svar-classification t)))
                        (auth-health/refresh-allowed? pid))
               (config/invalidate-credential-command! pid)
               (tel/log!
                 {:level :warn :id ::boot-credential-command-refreshed :data {:provider pid}}
                 (str "Provider build hit auth error — re-running credential command for "
                      pid
                      "; retrying build"))
               true))))

(defn- runtime-router-providers
  "Resolve durable provider config into the svar runtime shape.

   On-disk config intentionally omits ephemeral credentials for OAuth-backed
   providers such as OpenAI Codex. Resolve those fields immediately before
   constructing a router so each provider can refresh tokens and attach any
   provider-specific headers.

   Each provider may also enrich its own models via `:provider/enrich-models-fn`
   (e.g. LM Studio resolving real context windows)."
  [config]
  (let [ropts
        (config/router-opts config)

        ;; Route authenticated-but-unconfigured OAuth providers too, so a
        ;; provider chosen in a channel model picker (`picker-fleet`) ACTUALLY
        ;; routes without first being persisted into `:providers`. `->svar-provider`
        ;; resolves their token from the registry `:provider/get-token-fn`, so they
        ;; need no on-disk api-key. Config entries win on id; the rest are appended.
        configured
        (:providers config)

        configured-ids
        (into #{} (map :id) configured)

        provider-fleet
        (into (vec configured)
              (comp (remove #(contains? configured-ids (:id %)))
                    ;; Picker rows repeat preset transport data for display. They
                    ;; are not user overrides: a minted credential must still win.
                    (map #(dissoc % :base-url :api-style)))
              (try (providers/authenticated-preset-providers) (catch Throwable _ nil)))]

    ;; RESILIENT build: `->svar-provider` may eagerly fetch an OAuth token
    ;; (Copilot/Codex), and that can fail (expired token, GitHub 403
    ;; "not accessible by integration", network). A single failing provider
    ;; must NOT abort the whole router build and crash startup — skip it with a
    ;; warning and keep every provider that DID resolve. Falling through with
    ;; the others (or none) lets the app start and surface a fixable message.
    (->> provider-fleet
         ;; A provider whose `${NAME}` never resolved CANNOT authenticate: its
         ;; `:api-key` is still the literal reference. Drop it here rather than
         ;; letting it 401 on a real turn, and — crucially — keep every other
         ;; provider. One unset var must never cost you a session running on a
         ;; healthy provider. If it was the ONLY provider, svar raises
         ;; `:svar/no-providers`, which already routes to the provider manager
         ;; (see `config/no-provider-ex`), and that dialog now names
         ;; the exact variable.
         (remove (fn [p]
                   (when-let [{:keys [reason env-vars]} (config/provider-credential-gap p)]
                     (tel/log! {:level :warn
                                :id ::provider-env-unresolved-skipped
                                :data {:provider (:id p) :env-vars env-vars}
                                :msg reason})
                     true)))
         (keep
           (fn [p]
             (letfn [(build [] (enrich-provider-models (config/->svar-provider p) ropts))]
               (try (build)
                    (catch Throwable t
                      ;; Before dropping an auth-failed provider, try to HEAL it:
                      ;; a server-rotated OAuth token is auth-shaped and force-
                      ;; refreshable in place — refresh once, retry the build once.
                      ;; Anything else (or a failed retry) falls through to skip.
                      (or (try (when (or (boot-refresh-provider-token! (:id p) t)
                                         (boot-refresh-credential-command! p t))
                                 (build))
                               (catch Throwable _ nil))
                          (do (tel/log! {:level :warn
                                         :id ::provider-unavailable-skipped
                                         :data {:provider (:id p)
                                                :status (:status (ex-data t))
                                                :error (ex-message t)}
                                         :msg (str "Provider "
                                                   (some-> (:id p)
                                                           name)
                                                   " unavailable — skipping ("
                                                   (ex-message t)
                                                   ")")})
                              nil)))))))
         vec)))

(defn- config-root-pair
  "Resolve ONE `<role>` provider/model tag to `[provider-keyword wanted-model]`,
   or nil when the role names no provider.

   The model key accepts the same `provider/model` form as `--model`; its
   provider part wins over the sibling provider key, but ONLY when the fleet
   really has that provider — model ids CONTAIN slashes (openrouter serves
   `z-ai/glm-4.6v`), and splitting those seated the wrong root, so a default the
   user picked never took effect. Resolution mirrors
   `providers/default-selection` / `providers/fallback-selection` so the picker
   and the router can never disagree: once the provider resolves it is promoted
   even when the model name does not match its catalog, in which case its first
   model becomes that root."
  [config {:keys [provider-key model-key implicit-provider]}]
  (let [requested-model
        (some-> (get config model-key)
                str
                str/trim
                not-empty)

        provider-by-id
        (fn [id]
          (some #(when (= id (:id %)) %) (:providers config)))

        tagged-value
        (or (get config provider-key) implicit-provider)

        tagged
        (cond (keyword? tagged-value) tagged-value
              (string? tagged-value) (keyword tagged-value))

        whole-model?
        (boolean (some #(= requested-model (config/model-name %))
                       (:models (provider-by-id tagged))))

        slash
        (when (and requested-model (not whole-model?))
          (when-let [idx (str/index-of requested-model "/")]
            (let [idx (long idx)
                  prefix (keyword (subs requested-model 0 (long idx)))]

              (when (provider-by-id prefix)
                [prefix (not-empty (subs requested-model (inc idx)))]))))

        provider
        (or (first slash) tagged)

        wanted-model
        (or (if slash (second slash) requested-model)
            ;; No explicit model tag: the configured provider's FIRST model is
            ;; the selection, exactly as when no default was ever picked.
            (some-> (provider-by-id provider)
                    :models
                    first
                    config/model-name))]

    (when provider [provider wanted-model])))

(defn- seat-root
  "Return `provider-entries` with `provider-id` moved to the FRONT and
   `wanted-model` promoted to its `:root`. nil when that provider — or any model
   for it — is absent, so the caller can leave the fleet untouched."
  [provider-entries provider-id wanted-model]
  (when-let [selected (some #(when (= provider-id (:id %)) %) provider-entries)]
    (when-let [hit (or (some #(when (= wanted-model (:name %)) %) (:models selected))
                       (first (:models selected)))]
      (into [(assoc selected
               :models (into [hit] (remove #(= (:name hit) (:name %))) (:models selected))
               :root (:name hit))]
            (remove #(= provider-id (:id %)))
            provider-entries))))

(defn- honor-config-roots!
  "Make the explicit provider/model tags the router's effective roots: the
   PRIMARY pair first, the FALLBACK pair — always a DIFFERENT provider — second,
   every other provider left in its configured order behind them.

   Provider/model vector order is otherwise left alone and has no configuration
   meaning. A config that tags nothing keeps its first provider/first model
   selection, and an untagged, unknown or
   primary-colliding fallback leaves the tail exactly as it was."
  [router config]
  (let [primary
        (config-root-pair config
                          {:provider-key :default-provider
                           :model-key :default-model
                           :implicit-provider (:id (first (:providers config)))})

        fallback
        (config-root-pair config {:provider-key :fallback-provider :model-key :fallback-model})

        fallback
        (when (and fallback (not= (first fallback) (first primary))) fallback)]

    (if (or primary fallback)
      (update router
              :providers
              (fn [provider-entries]
                (let [seated (cond-> provider-entries
                               fallback
                               (as-> entries (or (apply seat-root entries fallback) entries))

                               primary
                               (as-> entries (or (apply seat-root entries primary) entries)))]
                  (if (identical? seated provider-entries)
                    provider-entries
                    (providers/reprioritize-providers seated)))))
      router)))

(defn- env-gap-router-error
  "Restate a bare `:svar/no-providers` when the REASON is an unset `${NAME}`:
   `runtime-router-providers` drops every provider whose reference never
   resolved, so an empty fleet is the LAST place that knowledge still exists.
   Without this a headless/CLI user sees svar's generic \"requires at least one
   provider\" and has to guess WHICH variable is missing — exactly the debug
   session the `${NAME}` feature exists to prevent.

   Keeps `:type :svar/no-providers` AND the original as the cause, so
   `config/no-provider-ex` still routes the TUI to the provider manager.
   Returns `t` untouched when the failure has nothing to do with env gaps."
  [config ^Throwable t]
  (let [gaps (config/provider-env-gaps config)]
    (if (or (empty? gaps) (not (config/no-provider-ex t)))
      t
      (ex-info (str "No usable provider — "
                    (str/join "; "
                              (map (fn [[provider-id env-vars]]
                                     (config/provider-env-message provider-id env-vars))
                                   gaps))
                    ". Set "
                    (str/join ", " (distinct (mapcat val gaps)))
                    " in your shell (export NAME=value) and start vis again.")
               {:type :svar/no-providers :vis/user-error true :env-gaps gaps}
               t))))

(defn build-router
  "Build a router, retaining network policy and account-scoped model metadata provenance."
  [config]
  (try (let [providers
             (runtime-router-providers config)

             by-id
             (into {} (map (juxt :id identity)) providers)

             router
             (svar/make-router providers (config/router-opts config))]

         (update router
                 :providers
                 (fn [normalized]
                   (mapv (fn [provider]
                           (let [source
                                 (by-id (:id provider))

                                 catalog-id
                                 (::config/model-catalog-identity source)]

                             (cond-> (merge provider (select-keys source [:network]))
                               catalog-id
                               (assoc ::model-catalog
                                 {:identity catalog-id
                                  :learned (into {} (map (juxt :name identity)) (:models provider))
                                  :fallback (into {}
                                                  (map (juxt :name identity))
                                                  (catalog/normalize-models
                                                    (:priority provider)
                                                    (assoc source
                                                      :models (::config/configured-models
                                                                source))))}))))
                         normalized))))
       (catch Throwable t (throw (env-gap-router-error config t)))))

(defn- refresh-router-models!
  "Schedule catalog discovery after the router is published; never block its caller."
  [router]
  (doseq [{:keys [id]} (:providers router)]
    (providers/refresh-models-async! id ::model-metadata)))

(defn get-router
  "Get or create the shared LLM router.

   Honors `:router` opts from `~/.vis/config.edn` (`:rate-limit`,
   `:network`, `:budget`, ...). Without that block svar's built-in
   defaults apply. See `config/router-opts` for the supported keys."
  []
  (or @router-atom
      (let [;; Python providers own endpoint/model defaults needed by config itself.
            ;; The first router precedes the first session environment, so waiting
            ;; for create-environment to load extensions is already too late.
            _
            (python-extensions/ensure-python-extensions-loaded!)

            cfg
            (or (config/load-config false) {})

            r
            (-> (build-router cfg)
                (honor-config-roots! cfg))]

        (reset! router-atom r)
        (refresh-router-models! r)
        r)))

(defn router-initialized?
  "True once the shared router has been built (via `get-router`/`rebuild-router!`).
   Lets a frontend defer the FIRST build to lazy first-use instead of forcing it
   at startup — so OAuth token fetches (Copilot/Codex) never run at TUI boot."
  []
  (some? @router-atom))

(defn rebuild-router!
  "Rebuild the router from the given config. Used when provider settings change.

   Forwards `:router` opts so live config edits (e.g. tuning
   `:same-provider-delays-ms`) take effect on the next `set-provider!`
   without restarting the JVM."
  [config]
  (let [r (-> (build-router config)
              (honor-config-roots! config))]
    (reset! router-atom r)
    (refresh-router-models! r)
    r))

;; ── OAuth credential hydration + 401 recovery ────────────────────────────
;;
;; svar routers intentionally retain provider health/budget state, but their
;; provider maps are immutable snapshots. OAuth credentials must not share that
;; lifetime: another tab/process can rotate a token at any moment. Therefore
;; every provider attempt gets a shallow router copy whose dynamic credential
;; fields are resolved immediately before network I/O. The shared router keeps
;; all of its state; only the attempt's provider vector is credential-hydrated.
;; A 401 then refreshes storage only. The retry boundary reads the new credential
;; itself, so recovery never depends on rebuilding global or cached routers.

(def ^:private AUTH_PROPAGATION_BACKOFF_MS
  "Base backoff (ms) before retrying the SAME just-refreshed token after a
   post-refresh auth 401. A freshly-minted OAuth token is briefly not-yet-valid
   at the provider edge; a short wait lets propagation settle instead of
   re-minting — which only spawns another not-yet-valid token (the 401 storm)."
  1200)

(defn auth-propagation-backoff-ms
  "Backoff (ms) for the Nth (0-based) post-refresh propagation retry, capped 5s."
  [attempt]
  (long (min 5000 (* (long AUTH_PROPAGATION_BACKOFF_MS) (inc (long attempt))))))

(defn apply-auth-cooldown-routing
  "Seed an iteration's routing with the providers still serving an auth cooldown so
   the dead credential is skipped BEFORE the request instead of being rediscovered
   with another 401.

   A PIN does not outrank the cooldown. EVERY main turn is pinned — `prepare-turn-context`
   forces the active provider+model into `:routing` so a provider failure surfaces as
   an error the user acts on — so exempting a pinned provider exempted every real
   turn: vis logged a five-minute cooldown and then re-probed, re-minted and
   re-fell-back on the very next iteration, ~12-16s later (issue #114). A COOLED pin
   is released exactly the way [[auth-fallback-routing]] releases it, which is the
   route the previous fallback already took. A pin on a HEALTHY provider is left
   alone, and the provider's own accepted request re-admits it immediately.

   No-op while `provider_fallback` is off: with nowhere to route, excluding the
   cooled provider would only trade its real error for a routing failure."
  [routing]
  (let [current
        (or routing {})

        cooled
        (auth-health/cooled)

        pinned
        (or (:provider current) (:force-provider current))]

    (if (or (empty? cooled) (not (provider-fallback-allowed?)))
      current
      (cond-> (-> current
                  (cond->
                    (contains? cooled pinned)
                    (dissoc :provider :model :force-provider :force-model))
                  (assoc :on-auth-error :fallback-provider)
                  (update :exclude-providers (fnil into #{}) cooled))
        (or (nil? (:on-transient-error current))
            (= :fallback-model-in-the-same-provider (:on-transient-error current)))
        (assoc :on-transient-error :hybrid)))))

(defn- auth-error-shaped?
  "True exactly when Svar's canonical failure verdict is authentication.

   Vis uses the verdict only to cool down or mutate credentials; it never
   reclassifies provider status codes, prose, or routing attempts."
  [^Throwable e]
  (= :auth (:category (perr/svar-classification e))))

(defn auth-fallback-routing
  "Build one cross-provider rescue route after OAuth refresh/backoff is exhausted.
   Returns nil after visible output, without a provider id, once enabled, or while
   `provider_fallback` is off."
  [^Throwable e routing resolved-model]
  (let [data
        (ex-data e)

        provider
        (:provider resolved-model)

        output-started?
        (or (pos? (long (or (:content-acc-len data) 0)))
            (pos? (long (or (:reasoning-acc-len data) 0)))
            (some? (:partial-content data))
            (some? (:reasoning data)))

        current
        (or routing {})]

    (when (and provider
               (provider-fallback-allowed?)
               (auth-error-shaped? e)
               (not output-started?)
               (not= :fallback-provider (:on-auth-error current)))
      (cond-> (-> current
                  (dissoc :provider :model :force-provider :force-model)
                  (assoc :on-auth-error :fallback-provider)
                  (update :exclude-providers (fnil conj #{}) provider))
        (or (nil? (:on-transient-error current))
            (= :fallback-model-in-the-same-provider (:on-transient-error current)))
        (assoc :on-transient-error :hybrid)))))

(defn refresh-just-failed?
  "True when we FORCED an OAuth refresh for this provider very recently (inside the
   [[auth-health/propagation-lag?]] window) and the credential is STILL auth-failing.
   Signals propagation lag (back off and retry the request-bound hydrated token)
   rather than a genuinely dead credential. The recency marker is provider-wide
   and is cleared by [[note-provider-request-ok!]] after accepted I/O."
  [^Throwable e resolved-model]
  (and (auth-error-shaped? e) (auth-health/propagation-lag? (:provider resolved-model))))

(defn note-provider-request-ok!
  "Clear the just-refreshed propagation marker AND any auth cooldown for the provider
   that ACCEPTED this iteration's request. Keeps [[refresh-just-failed?]]'s recency
   window scoped to the post-refresh settling burst, so a real credential rotation
   later is treated as a fresh 401 (re-mint), never misread as propagation lag, and
   lets a re-authenticated provider re-enter routing immediately instead of waiting
   out [[auth-health/AUTH_COOLDOWN_MS]].

   `iteration-result`'s `:llm-provider` is the provider that actually SERVED the
   request; `resolved-model` is only Vis' pre-call guess — `resolve-effective-model`
   reads the router HEAD, which the turn's pin hoists — so noting the guess let a
   turn RESCUED on a peer re-admit the dead credential, and the next iteration
   re-probed it (issue #114). No-op when the provider has neither marker."
  [resolved-model iteration-result]
  (when-let [pid (let [served (:llm-provider iteration-result)]
                   (cond (keyword? served) served
                         (string? served) (keyword served)
                         :else (:provider resolved-model)))]
    (auth-health/note-ok! pid)))

(defn- auth-provider-key
  "Provider id as a keyword, or nil when there is none. Picks arrive as strings from
   the DB and as keywords from the router, and both name the same provider."
  [pid]
  (cond (keyword? pid) pid
        (string? pid) (some-> (not-empty (str/trim pid))
                              keyword)
        (some? pid) (keyword (str pid))))

(defn- auth-rescue-pick-move
  "The session-pick move a rescued turn owes the human: `{:from {:provider :model}
   :to {:provider :model}}` when the pick names a provider whose credentials are
   PROVEN dead (it is serving an auth cooldown) and this iteration was answered by a
   different, healthy provider. nil for every other shape.

   Only a pick actually pinned to the dead provider moves: an unpinned session shows
   no wrong model to correct, a peer that is itself cooled is no place to land, and a
   route with no model name would CLEAR the pick instead of moving it."
  [pick iteration-result cooled]
  (let [from-pid
        (auth-provider-key (:provider pick))

        from-model
        (some-> (:model pick)
                str
                str/trim
                not-empty)

        to-pid
        (auth-provider-key (:llm-provider iteration-result))

        to-model
        (some-> (:llm-model iteration-result)
                str
                str/trim
                not-empty)]

    (when (and from-pid
               from-model
               to-pid
               to-model
               (contains? cooled from-pid)
               (not= from-pid to-pid)
               (not (contains? cooled to-pid)))
      {:from {:provider (name from-pid) :model from-model}
       :to {:provider (name to-pid) :model to-model}})))

(defn pick-move-event
  "The routing-trace event for a session pick that moved off a dead credential.

   Rides the turn's OWN `:llm-routing-trace`, which every surface already carries end
   to end (CLI bracket, TUI bubble footer, companion, `read_session` usage), so the
   note under the answer can say why the model chip changed without a new wire key.
   `:scope :session-pick` marks it as a SESSION-level change rather than this turn's
   route, which the summary and the note both anchor on separately."
  [{:keys [from to]}]
  {:event/type :llm.routing/provider-fallback
   :reason :authentication-fallback
   :scope :session-pick
   :from-provider (:provider from)
   :from-model (:model from)
   :to-provider (:provider to)
   :to-model (:model to)})

(defn pick-moved-chunk
  "Live-progress chunk announcing that a session pick moved off a dead credential. The
   `:provider-fallback` shape every channel already draws, so the CLI trace names the
   swap and the gateway forwards the routing event unchanged."
  [iteration-position {:keys [from to] :as move}]
  {:phase :provider-fallback
   :iteration iteration-position
   :reason :authentication-fallback
   :failed-provider (str (:provider from) "/" (:model from))
   :new-provider (str (:provider to) "/" (:model to))
   :event (pick-move-event move)})

(defn reseat-pick-after-auth-rescue!
  "Repoint the session's model pick onto the provider that actually answered, once the
   pinned provider's credentials are proven dead. Returns the move it made as
   `{:from {…} :to {…}}` for the caller to announce, or nil when nothing moved.

   The pick is what the TUI footer chip and the companion header show, and what
   `prepare-turn-context` re-pins on EVERY later turn — so leaving it on a dead
   provider is not cosmetic. The human reads a model the session is not running on,
   and each lapsed cooldown buys another 401, another forced refresh and another
   fallback before the same rescue lands again (issue #154). Moving the pick makes the
   rescue hold for the whole session instead of being rediscovered per turn.

   Prompt-cache continuity is already gone by the time this runs — the peer never saw
   the pinned provider's cache — so the move does not restore it; it stops the session
   from paying for the same discovery over and over. `set-model!` carries the reason,
   which rides the `session.model_updated` broadcast to every attached surface."
  [env iteration-result]
  (let [db
        (:db-info env)

        sid
        (:session-id env)]

    ;; Cooled providers first: on a healthy turn the set is empty and the session's
    ;; pick is never read, so the common path costs one atom deref, not a DB read per
    ;; iteration.
    (when-let [cooled (and db sid (not-empty (auth-health/cooled)))]
      (when-let [move
                 (auth-rescue-pick-move (session-model/model-of db sid) iteration-result cooled)]
        (session-model/set-model! db
                                  sid
                                  (:provider (:to move))
                                  (:model (:to move))
                                  :authentication-fallback)
        (session-model/record-switch! db sid (:from move) (:to move) :authentication-fallback)
        (tel/log! {:level :warn
                   :id ::auth-rescue-pick-moved
                   :data {:session-id (str sid)
                          :from (:from move)
                          :to (:to move)
                          :cooldown-ms auth-health/AUTH_COOLDOWN_MS}}
                  "Session model repointed: the pinned provider's credentials were rejected")
        move))))

(defn auth-refreshable-error?
  "True when Svar classified `e` as authentication and Vis can produce a new
   credential for the failing provider.

   Refreshing OAuth or `api_key_command` output mutates the next request; it is
   not a second provider failure classifier or transport retry policy."
  [^Throwable e resolved-model]
  (let [pid (:provider resolved-model)]
    (boolean (and (= :auth (:category (perr/svar-classification e)))
                  (or (some-> (registry/provider-by-id pid)
                              :provider/refresh-token-fn)
                      (auth-health/managed? (registry/provider-by-id pid))
                      (config/command-backed? pid))))))

(defn- hydrate-model-metadata
  "Select learned or fallback model facts for this attempt's account, preserving order."
  [provider]
  (if-let [catalog (::model-catalog provider)]
    (let [models (if (= (:identity catalog) (svar/model-catalog-identity provider))
                   (:learned catalog)
                   (:fallback catalog))]
      (update provider
              :models
              #(mapv (fn [model]
                       (get models (:name model) model))
                     %)))
    provider))

(defn- hydrate-router-credentials
  "Return an attempt-local copy of `router` with every provider's current
   credential fields resolved immediately before request dispatch.

   Two credential sources are hydrated here: a registry-backed
   `:provider/get-token-fn` (OAuth and friends), and a command-backed
   `api_key_command`, whose token is re-read from the credential cache so an
   `invalidate-credential-command!` on a 401 actually reaches the wire instead of
   waiting for the next router build.

   Router health, budget and retry state are preserved by sharing the original
   map. Stateful session lifecycle compares the effective router snapshots by value,
   so repeated hydration with the same token, endpoint, and headers keeps its opaque
   Svar handle. A changed credential or route produces a different snapshot and
   replaces that handle. A provider token lookup failure is deliberately failure-safe:
   that provider retains its previous snapshot so normal request/error handling remains
   authoritative."
  [router]
  (let [provider-entries
        (:providers router)

        hydrated
        (mapv
          (fn [{:keys [id] :as provider-entry}]
            (if-let [get-token-fn (some-> (registry/provider-by-id id)
                                          :provider/get-token-fn)]
              (try (let [{:keys [token api-url llm-headers responses-path api-style]} (get-token-fn)
                         ;; The credential may also NAME the wire it issued
                         ;; (#152): an extension that mints its own `api_url`
                         ;; is the only thing that knows the dialect. Config
                         ;; precedence was resolved when the router was built,
                         ;; so a runtime dialect fills a gap, never overrides.
                         dialect (when (nil? (:api-style provider-entry))
                                   (config/effective-api-style {:runtime api-style}))]

                     (cond-> provider-entry
                       (some? token)
                       (assoc :api-key token)

                       (some? api-url)
                       (assoc :base-url api-url)

                       (some? llm-headers)
                       (assoc :llm-headers llm-headers)

                       (some? responses-path)
                       (assoc :responses-path responses-path)

                       (some? dialect)
                       (assoc :api-style dialect)))
                   (catch Throwable t
                     (tel/log! {:level :warn
                                :id ::provider-credential-hydration-failed
                                :data {:provider id :error (ex-message t)}}
                               (str "Could not hydrate current credential for "
                                    id
                                    "; retaining the previous request snapshot"))
                     provider-entry))
              ;; Command-backed: the cache serves the same token in the
              ;; steady state (no fork per request) and re-execs the
              ;; helper exactly once after a 401 invalidated it. A helper
              ;; that is failing right now yields nil and keeps the
              ;; snapshot, so the provider error stays authoritative.
              (if-let [token (config/command-token id)]
                (assoc provider-entry :api-key token)
                provider-entry)))
          provider-entries)

        hydrated
        (mapv hydrate-model-metadata hydrated)]

    (if (= hydrated provider-entries) router (assoc router :providers hydrated))))

(defn- with-session-llm-headers
  "Decorate one immutable router snapshot with session-scoped provider headers.
   Configured and credential-derived headers are retained; a provider kickoff hook
   owns any same-named key it contributes. The shared process router is never mutated."
  [router headers-by-provider]
  (if (empty? headers-by-provider)
    router
    (update router
            :providers
            (fn [providers]
              (mapv (fn [{:keys [id] :as provider}]
                      (if-let [headers (not-empty (get headers-by-provider id))]
                        (update provider :llm-headers #(merge (or % {}) headers))
                        provider))
                    providers)))))

(defn kickoff-session-providers
  "Run provider kickoff hooks against every provider in this session's router.

   Preparing the whole fleet before Svar sees it covers internal fallbacks as well
   as the selected root. The result is recomputed whenever a router is seated, so
   providers added or reconfigured during a live session receive current metadata."
  [environment]
  (let [active-extensions
        (prompt/active-extensions environment)

        headers-by-provider
        (into {}
              (keep (fn [{:keys [id] :as provider}]
                      (when-let [headers (not-empty (extension/session-provider-kickoff-llm-headers
                                                      environment
                                                      active-extensions
                                                      provider))]
                        [id headers])))
              (get-in environment [:router :providers]))]

    (-> environment
        (assoc :session-llm-headers headers-by-provider)
        (update :router with-session-llm-headers headers-by-provider))))

(defn hydrate-environment-router
  "Hydrate only the router snapshot used by this provider attempt. The two-arity
   form is the real request boundary: it may run managed first-use authentication
   for the provider the router already resolved, never for unrelated fleet entries."
  ([environment]
   (update environment
           :router
           #(with-session-llm-headers (hydrate-router-credentials %)
                                      (:session-llm-headers environment))))
  ([environment provider-id]
   (auth-health/ensure-authenticated! provider-id)
   (hydrate-environment-router environment)))

(defn- router-provider-token
  "Token actually carried by provider `pid` in this exact router snapshot."
  [router pid]
  (some #(when (= pid (:id %)) (:api-key %)) (:providers router)))

(defn try-refresh-provider-token!
  "Recover a refreshable auth rejection without mutating any router.

   `attempt-router` is the exact request snapshot that received the 401, making
   its provider `:api-key` the exact rejected token. Before spending refresh
   budget, resolve current storage once: if a peer already installed a different
   token, simply retry and let request-bound hydration adopt it. Otherwise force
   one persisted refresh. The next attempt hydrates from storage; no global
   rebuild or cached-environment reseat is involved.

   A command-backed provider has no OAuth hook at all: its refresh is dropping
   the memoized `api_key_command` token so the next request boundary re-runs the
   helper. Same budget, same one-retry contract."
  [attempt-router resolved-model]
  (let [pid
        (:provider resolved-model)

        provider
        (registry/provider-by-id pid)

        f
        (:provider/refresh-token-fn provider)

        get-token-fn
        (:provider/get-token-fn provider)

        rejected
        (router-provider-token attempt-router pid)

        current
        (try (some-> get-token-fn
                     (apply [])
                     :token)
             (catch Throwable _ nil))

        managed-auth?
        (auth-health/managed? provider)

        refreshed?
        (cond (and (not f) (config/command-backed? pid))
              (if (auth-health/refresh-allowed? pid)
                (do (config/invalidate-credential-command! pid)
                    ;; Mark the attempt like an OAuth refresh so a SECOND 401 takes
                    ;; the propagation backoff instead of re-forking the helper.
                    (auth-health/note-refreshed! pid)
                    (tel/log!
                      {:level :warn :id ::credential-command-refreshed :data {:provider pid}}
                      (str "Auth 401 for " pid
                           " — re-running its credential command; retrying with"
                           " request-bound credential hydration"))
                    true)
                (do (tel/log! {:level :error
                               :id ::auth-refresh-circuit-open
                               :data {:provider pid
                                      :window-ms auth-health/AUTH_REFRESH_WINDOW_MS
                                      :max auth-health/AUTH_REFRESH_WINDOW_MAX}}
                              (str "Auth 401 — credential-command refresh circuit OPEN for "
                                   pid
                                   "; NOT re-running the helper — surfacing provider error"))
                    false))
              (not f) false
              ;; A concurrent request/process already won the rotation. Do not
              ;; touch either the breaker or token endpoint; retry hydration will
              ;; pick this value up at the request boundary.
              (and (util/non-blank-string? current) (not= current rejected))
              (do (tel/log! {:level :warn :id ::auth-peer-token-adopted :data {:provider pid}}
                            (str "Auth 401 for "
                                 pid
                                 " used a stale request credential; adopting the peer token"))
                  true)
              (not (auth-health/refresh-allowed? pid))
              (do (tel/log! {:level :error
                             :id ::auth-refresh-circuit-open
                             :data {:provider pid
                                    :window-ms auth-health/AUTH_REFRESH_WINDOW_MS
                                    :max auth-health/AUTH_REFRESH_WINDOW_MAX}}
                            (str "Auth 401 — OAuth refresh circuit OPEN for " pid
                                 " (" auth-health/AUTH_REFRESH_WINDOW_MAX
                                 " refreshes in " (quot (long auth-health/AUTH_REFRESH_WINDOW_MS)
                                                        1000)
                                 "s); NOT refreshing — surfacing provider error,"
                                 " re-authenticate this provider"))
                  false)
              :else (try
                      ;; Pass exactly what this attempt sent. Older/third-party hooks
                      ;; may still expose only a zero-arity implementation.
                      (try (f rejected) (catch clojure.lang.ArityException _ (f)))
                      (auth-health/note-refreshed! pid)
                      (tel/log! {:level :warn :id ::auth-token-refreshed :data {:provider pid}}
                                (str "Auth 401 — force-refreshed OAuth token for "
                                     pid
                                     "; retrying with request-bound credential hydration"))
                      true
                      (catch Throwable t
                        (tel/log! {:level :error
                                   :id ::auth-token-refresh-failed
                                   :data {:provider pid :error (ex-message t)}}
                                  (str "Auth 401 — OAuth token refresh FAILED for "
                                       pid
                                       "; surfacing provider error"))
                        false)))]

    (boolean (or (and refreshed?
                      (or (not managed-auth?)
                          (auth-health/usable-token? (try (get-token-fn) (catch Throwable _ nil))
                                                     rejected))
                      (do (provider-limits/auth-changed! pid) true))
                 (when managed-auth?
                   ;; A failed refresh does not revoke the extension's interactive login contract.
                   ;; The rejected nonblank token is not a usable credential (issue #204).
                   (auth-health/forget-refresh! pid)
                   (auth-health/reauthenticate! pid provider rejected)
                   (auth-health/note-refreshed! pid)
                   true)))))

(defn ask-code!
  "One-shot routed `svar/ask-code!` against the global router.
   Plain-text completion + Markdown-code-block extraction — returns the
   svar map `{:blocks :raw :reasoning :tokens :cost :duration-ms
   :assistant-message :provider-state}`. `:blocks` is a vec of
   `{:lang :source}` (one entry per Markdown code block); concatenate
   yourself with `svar.internal.codes/concat-sources` if you need a
   single string. `ask!` (JSON-spec) is gone; every Vis caller uses
   `ask-code!`."
  [opts]
  (let [router (get-router)]
    (svar/ask-code! router
                    (with-provider-network-defaults router (catalog/with-agent-initiator opts)))))

(defn llm-text!
  "Fast helper LLM call for extensions.

   Uses svar routing (`:routing {:optimize :cost}`) instead of Vis-side model
   name heuristics. The call still goes through `svar/ask-code!` because Vis no
   longer uses the retired `ask!` structured-output path; `:lang \"text\"`,
   `:reasoning :off`, and `:code-tail-pointer? true` make the return a plain
   text string under :text. Callers may pass either :messages or :system +
   :prompt."
  [{:keys [messages system prompt reasoning temperature routing] :as opts}]
  (let [opts
        ;; Helper traffic is agent activity, not a human prompt: a provider that
        ;; bills by initiator treats an unmarked request as a person's.
        (catalog/with-agent-initiator opts)

        messages
        (or messages
            (cond-> []
              (seq system)
              (conj {:role "system" :content system})

              (seq prompt)
              (conj {:role "user" :content prompt})))

        router
        (get-router)

        resp
        (svar/ask-code! router
                        (with-provider-network-defaults
                          router
                          (merge (dissoc opts :system :prompt :temperature)
                                 {:messages messages
                                  :lang "text"
                                  :reasoning (or reasoning :off)
                                  :routing (or routing {:optimize :cost})
                                  :code-tail-pointer? true}
                                 (when (some? temperature) {:temperature temperature}))))

        text
        (or (some-> resp
                    :result
                    str/trim
                    not-empty)
            (some-> resp
                    :raw
                    str/trim
                    not-empty)
            "")]

    (assoc resp :text text)))

(defn resolve-effective-model
  "Best-effort root model descriptor from router config.

   The returned map carries `:name` (model id, e.g. \"gpt-4o\") AND
   `:provider` (provider id keyword, e.g. `:openai`) so every caller
   can persist BOTH alongside the model. Earlier versions returned
   just the model map and the provider id was silently dropped on
   the way to the DB - leaving the meta layer with no way to render
   `provider/model`."
  ([router]
   (let [provider
         (first (:providers router))

         model
         (first (:models provider))]

     (when model
       (cond-> (if (map? model) model {:name (str model)})
         (:id provider)
         (assoc :provider (:id provider))))))
  ([router _routing-overrides] (resolve-effective-model router)))

(defn resolve-model-info
  "Resolved model map for the model a SESSION actually routes to.

   `resolve-effective-model` answers a different question — the router's GLOBAL
   root — and a channel that asks it about a session's capabilities describes
   the wrong model whenever the session picked something else (which is the
   normal case: Ctrl+T and the web picker both write a per-session preference).
   `provider-id`/`model-name` come from that preference; either may be nil, and
   the first provider/model that matches what IS given wins. Falls back to the
   root model so a session with no preference still gets an answer."
  [router provider-id model-name]
  (let [provider-id
        (some-> provider-id
                name
                not-empty
                keyword)

        hit
        (first (for [provider
                     (:providers router)

                     :when (or (nil? provider-id) (= provider-id (:id provider)))
                     model
                     (:models provider)

                     :let [model
                           (if (map? model) model {:name (str model)})]
                     :when (or (nil? model-name) (= (str model-name) (str (:name model))))]

                 (cond-> model
                   (:id provider)
                   (assoc :provider (:id provider)))))]

    (or hit (resolve-effective-model router))))

(defn turn-served-model
  "Model map for the provider/model that actually answered this turn's last request,
   or nil before anything has. Falls back to the router's root through
   `resolve-model-info` when the served pair is no longer in the fleet."
  [env]
  (when-let [ctx-atom (:ctx-atom env)]
    (when-let [served (ctx-engine/served-route @ctx-atom)]
      (resolve-model-info (:router env) (get served "provider") (get served "model")))))

(defn token-limit
  "One context-window candidate as a usable ceiling, or nil.

   Candidates come from provider catalogs and from config a human edits, so a window
   can arrive as `\"128000\"`, as 0, or as something that is not a number at all. A
   ceiling that is not a positive number is not a smaller budget — it is a reading
   that would carry nonsense into every saturation the session prints — so it is
   skipped in favour of the next source rather than trusted."
  [v]
  (let [n (cond (number? v) (double v)
                (string? v) (some-> v
                                    str/trim
                                    not-empty
                                    parse-double)
                :else nil)]
    (when n
      (let [d (double n)]
        (when (and (not (Double/isNaN d)) (not (Double/isInfinite d)) (pos? d)) (long d))))))

(defn iteration-context-limit
  "Input ceiling shared by CTX and folding. The routed Svar budget already accounts
   for the requested output and independent input cap; never subtract output again.
   An optional caller ceiling may only reduce it. Without a resolved request, retain
   the served-model, pinned-model, then historical advisory fallback."
  [max-context-tokens served-model pinned-model & [request-budget]]
  (let [caller
        (token-limit max-context-tokens)

        routed
        (when (integer? (:max-input-tokens request-budget))
          (max 1 (long (:max-input-tokens request-budget))))]

    (if routed
      (if caller (min (long caller) (long routed)) routed)
      (or caller
          (token-limit (:input-limit served-model))
          (token-limit (:context served-model))
          (token-limit (:input-limit pinned-model))
          (token-limit (:context pinned-model))
          200000))))

(defn resolved-context-budget
  "Resolve the same routed generation controls Svar will use for preflight."
  [environment resolved-model routing extra-body]
  (when (and (:provider resolved-model) (seq (get-in environment [:router :providers])))
    (svar/context-budget (:router environment)
                         {:routing (transcript/model-accounting-routing routing resolved-model)
                          :extra-body extra-body})))

(defn context-fold-budget
  "Soft folding threshold for a known input window. Windows below the normal 200K
   operating budget keep a 10% provider-rejection reserve. Unknown and >=200K windows
   retain the historical 200K threshold."
  [context-limit]
  (if-let [raw-limit (token-limit context-limit)]
    (let [limit (long raw-limit)
          default-budget (long ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS)]

      (if (< limit default-budget) (max 1 (quot (* limit 9) 10)) default-budget))
    ctx-engine/DEFAULT_PROMPT_BUDGET_TOKENS))

(defn router-for-model
  "Return a router variant whose provider/model ORDER reflects a model PREFERENCE,
   so svar's router picks + falls back accordingly — WE don't pick one model, we
   express the preference and let the inner router decide (no svar change: it
   already routes by the router's order). `prefs` is a model name OR an ORDERED
   coll of names; matching models are hoisted to the front in preference order
   (within each provider AND across providers), and the rest of the router follows
   UNCHANGED as fallback. Blank/unknown prefs → the router as-is (child inherits the
   parent's order).

   Vector order alone is DECORATION to svar, which selects by provider `:priority`
   and then by the provider's `:root` model name. So the hoist is also written into
   both fields: a matched provider's `:root` becomes its preferred model and the
   whole fleet is renumbered from its new position. Without that, a coordinator's
   `models` list changed the turn card and the cost row while every child turn
   still ran the default provider's root model."
  [router prefs]
  (let [names (->> (if (coll? prefs) prefs [prefs])
                   (keep #(some-> %
                                  str
                                  not-empty))
                   vec)]
    (if (empty? names)
      router
      (let [m-name (fn [m]
                     (:name (if (map? m) m {:name (str m)})))
            rank (zipmap names (range))
            ;; lower = more preferred; unlisted = +inf (keeps relative order, stable sort)
            m-rank (fn [m]
                     (get rank (m-name m) Long/MAX_VALUE))
            p-rank (fn [p]
                     (reduce min Long/MAX_VALUE (map m-rank (:models p))))
            reorder (fn [p]
                      (let [models (vec (sort-by m-rank (:models p)))
                            head (first models)]

                        (cond-> (assoc p :models models)
                          ;; Only a provider that actually offers a PREFERRED model
                          ;; gets a new root; the rest keep their configured one so
                          ;; fallback still lands on what the config chose.
                          (contains? rank (m-name head))
                          (assoc :root (m-name head)))))]

        (assoc router
          :providers (->> (:providers router)
                          (map reorder)
                          (sort-by p-rank)
                          providers/reprioritize-providers))))))

(defn- provider-root-model
  "Root model NAME for a provider id in `router`, or nil. Prefers the provider's
   declared `:root`, else its first model."
  [router pid]
  (when-let [p (first (filter #(= (:id %) pid) (:providers router)))]
    (or (some-> (:root p)
                str
                not-empty)
        (let [m (first (:models p))]
          (some-> (if (map? m) (:name m) m)
                  str)))))

(defn model-routing-status
  "Live routing health for the model a channel is DISPLAYING (`displayed-provider`
   + `displayed-model` — the per-session pick or the config default the picker
   shows).

   svar opens a circuit breaker on a provider after repeated transient failures
   (5xx / 'Overloaded' 529 / dropped streams) and routes turns to the next
   AVAILABLE provider so work keeps flowing. The displayed model is computed
   from config ORDER and is NOT breaker-aware, so during an outage the picker
   says `opus` while turns actually land on `zai`. This reconciles the two: when
   the displayed provider's breaker is open/half-open, it reports what svar is
   actually serving so the channel can surface
   `⚠ <displayed> overloaded — routing to <serving>`.

   Returns nil when the displayed provider is healthy, else
   `{:overloaded-provider <kw> :overloaded-model <str>
     :serving-provider <kw> :serving-model <str>}`. `serving-*` is nil if every
   provider is down."
  ([displayed-provider displayed-model]
   (model-routing-status (get-router) displayed-provider displayed-model))
  ([router displayed-provider displayed-model]
   (when (and router displayed-provider)
     (let [pid
           (keyword displayed-provider)

           stats
           (try (svar/router-stats router) (catch Throwable _ nil))

           cb-of
           (fn [p]
             (get-in stats [:providers p :circuit-breaker] :closed))

           open?
           (fn [p]
             (contains? #{:open :half-open} (cb-of p)))]

       (when (open? pid)
         (let [serving
               (first (remove #(open? (:id %)) (:providers router)))

               sp
               (:id serving)]

           {:overloaded-provider pid
            :overloaded-model (some-> displayed-model
                                      str)
            :serving-provider sp
            :serving-model (when sp (provider-root-model router sp))}))))))

(defn status->id [status] (when status (keyword "rlm.status" (name status))))

(def ^:private cost-map-keys
  ["input_cost" "input_uncached_cost" "input_cached_cost" "input_cache_write_cost" "cache_read_cost"
   "cache_write_cost" "output_cost" "total_cost"])

(def ^:private service-tier-keys [:service_tier "service_tier" :service-tier "service-tier"])

(defn- service-tier
  "The service tier `extra-body` names under `k`, lower-cased, or nil."
  [extra-body k]
  (some-> (get extra-body k)
          str
          str/lower-case))

(defn- fast-mode
  "The fast mode `provider`'s policy declares, or nil. `provider` is a keyword
  or string id."
  [provider]
  (:fast-mode (catalog/policy provider)))

(defn- fast-mode-requested?
  "True when this turn asks for `fast-mode`: the provider's own turn feature is
  on, or a caller-level extra body already names its service tier - the spelling
  of clients predating the turn feature."
  [fast-mode extra-body turn-features]
  (boolean (when fast-mode
             (or (true? (get turn-features (:turn-feature fast-mode)))
                 (some #(= (str/lower-case (:service-tier fast-mode)) (service-tier extra-body %))
                       service-tier-keys)))))

(defn fast-mode-router
  "Put a requested fast service tier only on the router entry of the provider
  that declares it, so Svar fallback cannot carry it to another provider."
  [router extra-body turn-features]
  (let [tier-for (fn [provider]
                   (let [fm (fast-mode (:id provider))]
                     (when (fast-mode-requested? fm extra-body turn-features) (:service-tier fm))))]
    (if-not (some tier-for (:providers router))
      router
      (update router
              :providers
              (fn [providers]
                (mapv (fn [provider]
                        (if-let [tier (tier-for provider)]
                          (update provider
                                  :extra-body
                                  (fn [body]
                                    (assoc (apply dissoc (or body {}) service-tier-keys)
                                      :service_tier tier)))
                          provider))
                      providers))))))

(defn provider-extra-body
  "Remove every fast service tier a provider declares from caller-level options
  after [[fast-mode-router]] scoped it to that provider's router entry. Other
  tiers and unrelated fields remain."
  [extra-body]
  (let [fast-tiers (into #{}
                         (keep #(some-> %
                                        :fast-mode
                                        :service-tier
                                        str/lower-case))
                         (vals (catalog/policies)))]
    (not-empty (reduce (fn [body k]
                         (if (contains? fast-tiers (service-tier body k)) (dissoc body k) body))
                       (or extra-body {})
                       service-tier-keys))))

(defn fast-mode-cost-multiplier
  "The price multiplier of `provider`'s fast mode when this turn requested it,
  else 1.0. The multiplier stays provider-gated so fast intent cannot change a
  fallback provider's pricing."
  [extra-body turn-features provider]
  (let [fm (fast-mode provider)]
    (if (fast-mode-requested? fm extra-body turn-features) (double (:cost-multiplier fm)) 1.0)))

(defn estimate-token-cost
  "Estimate cost from provider usage while preserving cached/non-cached input split.
     `:cost-multiplier` scales every monetary component after svar prices the
     canonical usage; token counts remain untouched."
  ([model input-tokens output-tokens] (estimate-token-cost model input-tokens output-tokens {}))
  ([model input-tokens output-tokens opts]
   (try (let [opts
              (or opts {})

              multiplier
              (double (or (:cost-multiplier opts) 1.0))

              cost-map
              (wire/canonical (catalog/estimate-cost model
                                                     input-tokens
                                                     output-tokens
                                                     (dissoc opts :cost-multiplier)))]

          (if (and (map? cost-map) (not= 1.0 multiplier))
            (reduce (fn [m k]
                      (update m k #(if (number? %) (* multiplier (double %)) %)))
                    cost-map
                    cost-map-keys)
            cost-map))
        (catch Throwable _ nil))))

(defn merge-cost-maps
  [acc extra-cost]
  (merge-with + (select-keys acc cost-map-keys) (select-keys extra-cost cost-map-keys)))
