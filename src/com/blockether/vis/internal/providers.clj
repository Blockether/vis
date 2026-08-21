(ns com.blockether.vis.internal.providers
  "Channel-neutral provider management service.

   Everything a channel needs to render and mutate the provider fleet
   — status probing, account limits, live model catalogs, presets, and
   config persistence — WITHOUT any UI. Hoisted from the TUI extension
   (`channel_tui/provider.clj`) so any future surface manages the SAME
   fleet through the SAME primitives; the channels keep only their
   interaction layer (lanterna dialogs, ...).

   Auth is classified, not implemented, here: `auth-kind` tells a
   channel whether a provider wants an API key, an interactive OAuth
   flow (owned by the provider extension + channel), or nothing
   (local). The registry's `:provider/*-fn` contract stays the single
   integration point for provider extensions, so a provider extension
   automatically works in every channel."
  (:require [babashka.http-client :as http]
            [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.cancellation :as cancel]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.format :as format]
            [com.blockether.vis.internal.gateway.wire :as wire]
            [com.blockether.vis.internal.limits-format :as limits-format]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.registry :as registry])
  (:import [java.net ConnectException URI]
           [java.util Date]))

;;; ── Classification ─────────────────────────────────────────────────────────

(def local-no-auth-provider-ids
  "Local OpenAI-compatible providers that need no credentials."
  #{:ollama :lmstudio})

(def github-copilot-account-types
  "GitHub Copilot preset id -> device-flow account type."
  {:github-copilot-individual :individual
   :github-copilot-business :business
   :github-copilot-enterprise :enterprise})

(def oauth-provider-ids
  "Providers whose credentials come from an interactive OAuth flow and
   live OUTSIDE config.edn (keychain / token files owned by the
   provider extension)."
  (into #{:openai-codex :anthropic-coding-plan} (keys github-copilot-account-types)))

(defn command-minted?
  "True when the credential is minted BY THE MACHINE: config carries an
   `api_key_command`, so the helper mints (and rotates) the token on every
   request and there is nothing for a human to type or paste."
  [provider]
  (some? (:api-key-command provider)))

(defn managed?
  "True when the extension that registered `provider-id` declares
   `:provider/is-managed`: the RUNTIME issues that provider's credential — a
   corporate gateway, a device policy, a host the user is already signed into —
   so there is no key for a human to type, paste or rotate, and no `Add
   provider` step to run. A managed provider binds itself as soon as its
   extension is loaded ([[authenticated-preset-providers]]) and every key-taking
   path refuses it (`provider-auth/start-auth!`, the channels' auth verbs)."
  [provider-id]
  (boolean (:provider/is-managed (registry/provider-by-id provider-id))))

(defn auth-kind
  "How a provider authenticates: `:command` (an `api_key_command` mints the
   credential — never prompt), `:oauth` (interactive flow owned by the
   provider extension), `:managed` (the extension declares
   `:provider/is-managed`, so the runtime issues the credential and there is
   nothing to type), `:none` (local, no credentials), or `:api-key`.

   The 1-arity classifies by id alone and therefore can never see a
   command-minted provider; pass the configured provider map when the
   answer decides whether to prompt a human."
  ([pid] (auth-kind pid nil))
  ([pid provider]
   (cond (command-minted? provider) :command
         (managed? pid) :managed
         (contains? oauth-provider-ids pid) :oauth
         (contains? local-no-auth-provider-ids pid) :none
         :else :api-key)))

(defn url-host
  "Extract host from URL for display. 'https://llm.blockether.com/v1' ->
   'llm.blockether.com'."
  [url]
  (try (.getHost (URI. url)) (catch Exception _ (or url ""))))

;;; ── Model fetching ──────────────────────────────────────────────────────────

(def ^:private non-chat-pattern
  "Regex matching model IDs that aren't chat/completion models."
  #"(?i)^(whisper|eleven|text-embedding|tts|dall-e|stable-diffusion|wav2vec|canary|speech)")

(defn- chat-model? [id] (not (re-find non-chat-pattern id)))

(defn fetch-models
  "List models for a vis provider via `svar/models!`.

   Returns vec of chat model id strings, or nil on failure. Filters
   out TTS / embedding / speech / image and provider-excluded models.

   Routing through svar means the call automatically picks up
   provider-specific OAuth headers (`anthropic-version`,
   `anthropic-beta` for the Anthropic Claude subscription;
   `chatgpt-account-id` for OpenAI Codex; bare Bearer for everyone
   else).

   `provider` is a vis-shaped provider map. We coerce to svar shape
   (resolving OAuth tokens via the provider's `:provider/get-token-fn`
   when `:api-key` is absent) and ask svar."
  [provider]
  (try (let [provider-id
             (:id provider)

             ;; ->svar-provider needs at least one model on the provider
             ;; for `normalize-provider` not to throw. The concrete model
             ;; doesn't matter for `/models`.
             probe
             (cond-> provider
               (empty? (:models provider))
               (assoc :models [{:name "probe"}]))

             svar-provider
             (config/->svar-provider probe)

             ;; Honor `:router` opts (retry/network/budget) so the probe
             ;; respects the same policy a real turn would.
             router
             (svar/make-router [svar-provider] (config/router-opts (config/current-config)))

             raw
             (svar/models! router)]

         (->> raw
              (map (fn [m]
                     (or (:id m) (:name m) (str m))))
              (filter string?)
              (filter chat-model?)
              (filter #(config/provider-model-visible? provider-id %))
              distinct
              sort
              vec))
       (catch Exception _ nil)))

(def ^:private dated-variant-pattern
  "Matches model IDs that are dated snapshots, e.g. gpt-4o-2024-08-06."
  #"-\d{4}-\d{2}-\d{2}$")

(defn dated-variant? [id] (boolean (re-find dated-variant-pattern id)))

(defn- pin-default
  "Move env default model to front of list."
  [ids]
  (let [env-default (System/getenv "BLOCKETHER_LLM_DEFAULT_MODEL")]
    (if env-default (into (filterv #(= % env-default) ids) (remove #(= % env-default) ids)) ids)))

(defn default-model-names
  "Union of model names already on the provider map plus the preset /
   provider `:default-models`, deduped. Config order leads: the models a
   user wrote in vis.yml come first and `model-options` keeps them there."
  [provider]
  (let [template (config/provider-template (:id provider))]
    (->> (concat (:models provider) (:default-models template) (:default-models provider))
         (keep config/model-name)
         distinct
         vec)))

(defn configured-model-names
  "Model names a provider declares in config, in the exact order they are
   written in vis.yml. Never filtered by svar's catalog visibility rules - an
   explicitly configured model is a statement of intent, not a suggestion."
  [provider]
  (->> (:models provider)
       (keep config/model-name)
       distinct
       vec))

(defn model-options
  "Selectable model ids for a provider: configured models first IN vis.yml
   ORDER, then live-fetched + preset defaults deduped and sorted, env
   default pinned first. When `show-all?` is false, dated snapshot
   variants (gpt-4o-2024-08-06) are hidden.

   Returns `{:models [id ...] :hidden-count n}` - channels render
   their own 'show all' affordance from `:hidden-count`."
  ([provider] (model-options provider (default-model-names provider) false))
  ([provider default-models show-all?]
   (let [provider-id
         (:id provider)

         configured
         (configured-model-names provider)

         configured?
         (set configured)

         fetched
         (or (fetch-models provider) [])

         defaults
         (filterv #(config/provider-model-visible? provider-id %) (or default-models []))

         all-ids
         (into configured
               (->> (concat fetched defaults)
                    distinct
                    (remove configured?)
                    sort))

         pinned
         (pin-default all-ids)

         visible
         (if show-all? pinned (filterv (complement dated-variant?) pinned))]

     {:models visible :hidden-count (- (count pinned) (count visible))})))

;;; ── Status + limits ─────────────────────────────────────────────────────────

(def probe-timeout-ms
  "Wall a provider lifecycle callback (`:provider/status-fn`,
   `:provider/detect-fn`) gets before it is declared wedged. These callbacks are
   trusted extension code — often Python — and a hung one used to hold the
   gateway request, and the card behind it, for the client's whole 30s budget."
  5000)

(defn- probe-within
  "Run `f` off the calling thread and wait at most `probe-timeout-ms` for it.
   Returns `::timed-out` when the callback is still running by then; whatever
   the callback threw is rethrown as-is, so callers keep one error path.

   The worker runs `f` through `bound-fn*`. A provider callback reads its context
   off dynamic vars — `extension/*current-environment*` and the workspace roots
   derived from it — and `cancel/worker-future` starts a bare thread that conveys
   no bindings (unlike `clojure.core/future`). Without conveyance, bounding the
   probe silently unbound the session: a callback invoked from inside a live
   session had `vis.jailed_shell_session` and `vis.ask` refuse it as \"available
   only while handling a session\", `vis.state` fall back to the process-wide DB,
   and a `vis.jailed_shell` spawn scoped to the process cwd instead of the
   caller's workspace."
  [label f]
  (let [fut (cancel/worker-future (str "vis-provider-" label) (bound-fn* f))]
    (try (let [value (deref fut probe-timeout-ms ::timed-out)]
           (when (identical? ::timed-out value) (.cancel ^java.util.concurrent.Future fut true))
           value)
         (catch java.util.concurrent.ExecutionException e (throw (or (.getCause e) e))))))

(defn safe-provider-status
  "Status of a REGISTERED provider descriptor via its `:provider/status-fn`
   (falling back to `:provider/detect-fn`). Never throws, and never runs longer
   than `probe-timeout-ms`: a callback that is still going by then answers
   `{:is-authenticated false :error \"…timed out…\"}` — an honest verdict the
   surface can paint — instead of parking the thread that asked."
  [provider]
  (let [label
        (str (or (:id provider) "provider"))

        verdict
        (fn [value]
          (if (identical? ::timed-out value)
            {:is-authenticated false
             :error (str "status check timed out after " (quot (long probe-timeout-ms) 1000) "s")}
            value))]

    (try (cond (:provider/status-fn provider) (verdict
                                                (probe-within label (:provider/status-fn provider)))
               (:provider/detect-fn provider)
               (let [detected (probe-within label (:provider/detect-fn provider))]
                 (if (identical? ::timed-out detected)
                   (verdict detected)
                   {:is-authenticated (boolean detected)}))
               :else nil)
         (catch Throwable e {:is-authenticated false :error (or (ex-message e) (str e))}))))

(defonce ^:private local-probe-http-client
  (delay (http/client {:connect-timeout 1500 :version :http1.1})))

(defn probe-local-reachable
  "Probe a local OpenAI-compatible provider (Ollama / LM Studio) by
   GETting its `<base-url>/models` endpoint with a short timeout.
   Reachable → `{:is-authenticated true …}`; refused / timeout / other →
   `{:is-authenticated false :error \"<human hint>\"}` so the channel can
   SAY why the dot is red. Blocking ≤ ~2.5s — call off the render path."
  [provider]
  (let [base
        (or (config/provider-base-url provider) (:base-url provider))

        label
        (config/display-label (:id provider))

        base*
        {:is-authenticated false :source :local :provider-id (:id provider) :base-url base}]

    (if (str/blank? base)
      (assoc base* :error (str label ": no base URL configured"))
      (let [url
            (str (str/replace base #"/+$" "") "/models")

            host
            (url-host base)]

        (try
          (let [resp
                (http/request {:uri url
                               :method :get
                               :client @local-probe-http-client
                               :timeout 2500
                               :throw false
                               :as :string})

                code
                (:status resp)]

            ;; Any answer below 500 means the server is up — even a 401/404 proves
            ;; the port is live. 5xx is the server failing.
            (if (< (long code) 500)
              (assoc base* :is-authenticated true)
              (assoc base* :error (str label " returned HTTP " code " at " host))))
          (catch ConnectException _
            (assoc base* :error (str "Can't reach " label " at " host " — is it running?")))
          (catch Throwable e
            (if (str/includes? (str/lower-case (or (ex-message e) "")) "timed out")
              (assoc base* :error (str label " timed out at " host " — is it running?"))
              (assoc base*
                :error
                (str "Can't reach " label " at " host " (" (or (ex-message e) (str e)) ")")))))))))

(declare provider-limits-safe)

(defn provider-status
  "Auth/liveness status for a CONFIGURED provider map. Local providers
   are probed for real; an explicit `:api-key` is trusted; otherwise
   the registered extension's status/detect fns answer. When an authenticated
   provider exposes live limits and that check says `:unauthenticated`, the
   live verdict wins over a merely-present credential file. Never throws."
  [provider]
  (let [registered
        (registry/provider-by-id (:id provider))

        status
        (cond
          ;; FIRST, ahead of the `:api-key` trust branch: an unresolved `${NAME}`
          ;; leaves the literal reference sitting in `:api-key`, which is `some?` and
          ;; would otherwise read as "authenticated from config" — the worst possible
          ;; verdict, since the truth only surfaces as a 401 on the first real turn.
          ;; A configured `api_key_command` that cannot currently produce a token is
          ;; the same class of un-authenticatable provider and answers here too.
          (config/provider-credential-gap provider)
          (let [{:keys [reason env-vars]} (config/provider-credential-gap provider)]
            (cond-> {:is-authenticated false :source (if env-vars :env :command) :error reason}
              env-vars
              (assoc :needs-env (str/join ", " env-vars))))
          ;; Local no-auth providers (Ollama / LM Studio) have no key and
          ;; their registered status-fn is a hardcoded stub — probe the
          ;; endpoint for real.
          (contains? local-no-auth-provider-ids (:id provider)) (probe-local-reachable provider)
          (some? (:api-key provider))
          {:is-authenticated true :source :config :config-path (config/state-path)}
          ;; No gap above means the helper DID produce a token just now.
          (some? (:api-key-command provider)) {:is-authenticated true :source :command}
          registered (or (safe-provider-status registered) {:is-authenticated false})
          :else {:is-authenticated false})]

    ;; OAuth credentials can remain on disk after their subscription has ended.
    ;; A limits endpoint that rejects those credentials is the one live account
    ;; signal available to every channel, so surface its explanation as status.
    (if (and (:is-authenticated status) (:provider/limits-fn registered))
      (let [limits (provider-limits-safe provider)]
        (if (= :unauthenticated (:status limits))
          (assoc status
            :is-authenticated false
            :error (or (get-in limits [:dynamic :note])
                       "Provider rejected the current credentials."))
          status))
      status)))

(defn provider-reachable?
  "Cheap ROUTING-time liveness verdict: local providers (Ollama /
   LM Studio) get the real HTTP probe; remote providers are assumed
   reachable — their auth/network failures surface as call errors svar
   already fails over on, and a per-turn network check against every
   remote backend would tax every turn."
  [provider]
  (if (contains? local-no-auth-provider-ids (:id provider))
    (boolean (:is-authenticated (probe-local-reachable provider)))
    true))

(defn reprioritize-providers
  "Renumber `:priority` from vector position; returns a vector.

   svar bakes `:priority` at `make-router` time from the DECLARED index, and every
   candidate sort reads that NUMBER rather than vector order (see
   `svar…router/candidate-sort-key`). Reordering a router's `:providers` vector
   alone therefore promotes a provider in NAME only: the health gate below, a
   session pin and a coordinator's `models` preference each looked applied while
   svar kept routing to the original head. Every Vis reorder ends here."
  [provider-entries]
  (into []
        (map-indexed (fn [idx provider]
                       (assoc provider :priority (long idx))))
        provider-entries))

(defn demote-unreachable-providers
  "Health-order a ROUTER (svar shape, `{:providers [...]}`) for one
   turn: LOCAL providers that fail the liveness probe sink to the END
   of the fleet — kept as last resort, never silently dropped — so a
   dead local endpoint can't catch a turn (or an svar fallback) that a
   healthy provider should have taken. Probes run ONLY when local
   providers are configured (≤ ~2.5s each; zero cost otherwise).
   Returns `{:router r :demoted [provider-ids]}`. NEVER throws —
   routing must survive a broken probe (falls back to the router
   as-is), so callers need no defensive wrapping."
  [router]
  (try (let [providers (vec (:providers router))]
         (if-not (some #(contains? local-no-auth-provider-ids (:id %)) providers)
           {:router router :demoted []}
           (let [{ok true bad false} (group-by provider-reachable? providers)]
             (if (seq bad)
               {:router (assoc router :providers (reprioritize-providers (concat ok bad)))
                :demoted (mapv :id bad)}
               {:router router :demoted []}))))
       (catch Throwable _ {:router router :demoted []})))

(defn provider-limits-safe
  "Normalized limits report for a provider id; an error report instead
   of a throw."
  [provider]
  (try (provider-limits/provider-limits (:id provider))
       (catch Throwable e
         {:provider-id (:id provider)
          :status :error
          :static {}
          :dynamic {:limits []}
          :error {:message (or (ex-message e) (str e))}})))

(defn initial-provider-status
  "Placeholder status while a real probe runs in the background. A credential gap
   is decided synchronously — it is a pure read of the config already in hand,
   plus at most one cached credential-command probe — so the card never flashes
   an authenticated verdict it is about to retract."
  [provider]
  (wire/canonical
    (if-let [{:keys [reason env-vars]} (config/provider-credential-gap-cached provider)]
      (cond-> {:is-authenticated false :source (if env-vars :env :command) :error reason}
        env-vars
        (assoc :needs-env (str/join ", " env-vars)))
      (cond (some? (:api-key provider))
            {:is-authenticated true :source :config :config-path (config/state-path)}
            (some? (:api-key-command provider)) {:is-authenticated true :source :command}
            :else {:is-authenticated nil :loading? true}))))

(defn initial-provider-limits
  "Placeholder limits report while the real fetch runs."
  [provider]
  {:provider-id (:id provider) :status :loading :static {} :dynamic {:limits []}})

;;; ── Status report text ──────────────────────────────────────────────────────

(defn- status-entry-label
  [k]
  (-> (name k)
      (str/replace #"[-_]" " ")
      (str/capitalize)))

(defn- format-status-value
  [v]
  (cond (keyword? v) (name v)
        (map? v) (str/join ", "
                           (map (fn [[k2 v2]]
                                  (str (name k2) ": " (format-status-value v2)))
                                (sort-by (comp str key) v)))
        (sequential? v) (str/join ", " (map format-status-value v))
        :else (str v)))

(defn- format-limit-window
  [{:keys [kind unit size resets-at-ms]}]
  (when kind
    (str (name kind)
         (when unit (str " " (or size 1) "/" (name unit)))
         (when resets-at-ms (str ", resets " (format/format-date (Date. (long resets-at-ms))))))))

(defn format-limit-row
  [{:keys [label scope kind is-unlimited used limit remaining note window]}]
  (let [quota
        (cond is-unlimited "unlimited"
              (number? limit) (str (when (number? used) (str used "/"))
                                   limit
                                   (when (number? remaining) (str " (" remaining " left)")))
              (number? used) (str "used " used)
              :else nil)

        attrs
        (->> [(some-> scope
                      name)
              (some-> kind
                      name) (format-limit-window window)]
             (remove nil?))]

    (str label
         (when (seq attrs) (str " [" (str/join ", " attrs) "]"))
         (when quota (str ": " quota))
         (when note (str " - " note)))))

(defn- auth-verdict
  "Three-valued authentication verdict for a status map. A probe still in
   flight is `:checking`, NEVER a definitive `:no` the card is about to
   retract one refresh later."
  [status]
  (cond (get status "is_authenticated") :yes
        (get status "is_loading") :checking
        :else :no))

(defn status-text
  "Multi-line human status + limits report for a configured provider.
   The single source for the TUI 'Show Status + Limits' dialog and the
   web status view."
  ([provider] (status-text provider (provider-status provider) (provider-limits-safe provider)))
  ([provider status limits]
   (let [status
         (wire/canonical (or status (initial-provider-status provider)))

         limits
         (or limits (initial-provider-limits provider))

         title
         (str (config/display-label (:id provider)) " Status")

         rows
         (->> status
              (remove (fn [[k _]]
                        (contains? #{"is_authenticated" "is_loading"} k)))
              (sort-by (comp str key))
              (map (fn [[k v]]
                     (str (status-entry-label k) ": " (format-status-value v)))))

         dynamic
         (get-in limits [:dynamic :limits])]

     (str/join
       "\n"
       (concat
         [title "" (str "Base URL: " (or (config/provider-base-url provider) "-"))
          (str "Authenticated: "
               (case (auth-verdict status)
                 :yes
                 "yes"

                 :no
                 "no"

                 :checking
                 "checking…"))]
         (when-let [e (get status "error")]
           ["" (str "Error: " e)])
         (when (seq rows) (concat [""] rows))
         ["" "Limits" (str "Status: " (name (:status limits)))]
         (when-let [rpm (get-in limits [:static :rpm])]
           [(str "Catalog RPM: " rpm)])
         (when-let [tpm (get-in limits [:static :tpm])]
           [(str "Catalog TPM: " tpm)])
         (if (seq dynamic)
           (concat ["Dynamic limits:"] (map #(str "- " (format-limit-row %)) dynamic))
           ["Dynamic limits: none reported"])
         (when-let [note (get-in limits [:dynamic :note])]
           [(str "Note: " note)])
         (when (seq (:static limits))
           ["Catalog RPM / TPM come from the provider catalog, not live account quota usage."])
         (when-let [message (get-in limits [:error :message])]
           [(str "Limits error: " message)]))))))

;;; ── Status report, rich form ────────────────────────────────────────────────

(defn- md-escape-cell
  [s]
  (-> (str s)
      (str/replace "|" "\\|")
      (str/replace #"\s+" " ")
      str/trim))

(defn- status-detail-rows
  "Status-map detail entries (minus :is-authenticated/:error) as md list rows."
  [status]
  (->> status
       (remove (fn [[k _]]
                 (contains? #{"is_authenticated" "error" "is_loading"} k)))
       (sort-by (comp str key))
       (map (fn [[k v]]
              (str "- **" (status-entry-label k) ":** " (format-status-value v))))))

(defn status-md
  "The provider status + limits report as MARKDOWN — one rich canonical
   form every channel renders natively: the web through its markdown
   pipeline and the TUI through its transient Markdown layout walker. The same
   facts as [[status-text]], structured instead of flat."
  ([provider] (status-md provider (provider-status provider) (provider-limits-safe provider)))
  ([provider status limits]
   (let [status
         (wire/canonical (or status (initial-provider-status provider)))

         limits
         (or limits (initial-provider-limits provider))

         label
         (config/display-label (:id provider))

         verdict
         (auth-verdict status)

         dynamic
         (get-in limits [:dynamic :limits])

         rpm
         (get-in limits [:static :rpm])

         tpm
         (get-in limits [:static :tpm])]

     (str/join
       "\n"
       (concat
         [(str "## " label) ""
          (str "**Authenticated:** "
               (case verdict
                 :yes
                 "yes ✓"

                 :no
                 "no ✗"

                 :checking
                 "checking…")
               "  ·  **Base URL:** `"
               (or (config/provider-base-url provider) "-")
               "`")]
         (when-let [e (get status "error")]
           ["" (str "> ⚠ " e)])
         (let [rows (status-detail-rows status)]
           (when (seq rows) (concat [""] rows)))
         ["" "### Limits" "" (str "_status: " (name (:status limits)) "_")]
         (if (seq dynamic)
           (concat ["" "| Limit | Scope | Window | Usage | Note |" "|---|---|---|---|---|"]
                   (map (fn [{:keys [scope kind window note] :as row}]
                          (str "| "
                               (md-escape-cell (limits-format/generic-limit-label row))
                               " | "
                               (md-escape-cell (or (some-> scope
                                                           name)
                                                   (some-> kind
                                                           name)
                                                   "-"))
                               " | "
                               (md-escape-cell (or (format-limit-window window) "-"))
                               " | **"
                               (md-escape-cell (or (limits-format/format-limit-usage row) "-"))
                               "** | "
                               (md-escape-cell (or note "-"))
                               " |"))
                        dynamic))
           ["" "_No dynamic account limits reported._"])
         (when-let [note (get-in limits [:dynamic :note])]
           ["" (str "_" note "_")])
         (when (or rpm tpm)
           [""
            (str "Catalog defaults: "
                 (str/join " · "
                           (remove nil? [(when rpm (str "RPM " rpm)) (when tpm (str "TPM " tpm))]))
                 " — provider-catalog numbers, not live account quota.")])
         (when-let [message (get-in limits [:error :message])]
           ["" (str "> ⚠ Limits error: " message)]))))))

;;; ── Fleet + presets + persistence ───────────────────────────────────────────

(defn configured-providers
  "The persisted provider fleet (global + project overlay), priority
   order, catalog metadata applied (base-url/api-style filled in)."
  []
  (vec (:providers (config/load-config))))

(defonce ^:private fleet-cache
  ;; {:at <epoch-ms> :val <fleet vec>} — last-known `configured-providers`
  ;; snapshot; nil until the first read or right after an invalidation.
  (atom nil))

(defonce ^:private fleet-refreshing (atom false))

(def ^:private fleet-cache-ttl-ms
  ;; Cross-process safety net ONLY: same-process fleet mutations invalidate
  ;; explicitly (`save-providers!` / `remove-provider!`), so the TTL just
  ;; bounds staleness when ANOTHER process (a second channel, a hand edit)
  ;; changes the config files.
  30000)

(defn invalidate-configured-providers!
  "Drop the fleet snapshot so the next `configured-providers-cached` read
   re-enumerates. Called by every same-process fleet mutation — which is what
   lets the TTL stay long (issue #29 follow-up: invalidate on change instead
   of polling)."
  []
  (reset! fleet-cache nil))

;; ── Shared-router rebuild hook ──────────────────────────────────────────────
;; The shared LLM router and every cached session env that snapshotted it live in
;; `loop`, which REQUIRES this namespace — so a provider/selection mutation cannot
;; rebuild the router itself. It fires this hook instead. Without it, picking a new
;; default persisted config and the picker showed the new model while the shared
;; `router-atom` (and every env built since) kept the OLD root: a new session's
;; first turn ran the previous model until the user re-pinned it on the session.
;; `loop` registers `reload-router!` here at load (no-op until the router is built).
(defonce ^:private router-rebuild-hook (atom nil))

(defn set-router-rebuild-hook!
  "Register the function `loop` calls to rebuild the shared router from current
  config and reseed every cached session env. Idempotent; latest wins; nil clears."
  [f]
  (reset! router-rebuild-hook f))

(defn router-rebuild-hook-val
  "Current registered router-rebuild hook fn (or nil) — inspection/test accessor."
  []
  @router-rebuild-hook)

(defn rebuild-shared-router!
  "Drop the configured-provider cache, then fire the registered router-rebuild
  hook best-effort so the shared router (and every cached session env that
  snapshotted it) rebuilds from current config. No-op before `loop` registers
  (early boot) or when the router was never built — `reload-router!` guards the
  latter on `router-initialized?`.

  PUBLIC because AUTH needs it too: a provider whose credential cannot be resolved
  is skipped at router build, so signing in must rebuild or the daemon keeps
  routing as though that provider did not exist."
  []
  (invalidate-configured-providers!)
  (when-let [f @router-rebuild-hook]
    (try (f) (catch Throwable _ nil))))

(defn- refresh-fleet-cache!
  "Single-flight BACKGROUND re-enumeration of the fleet snapshot. Errors
   leave the last-known value untouched."
  []
  (when (compare-and-set! fleet-refreshing false true)
    (future (try (reset! fleet-cache {:at (System/currentTimeMillis) :val (configured-providers)})
                 (catch Throwable _ nil)
                 (finally (reset! fleet-refreshing false))))))

(defn configured-providers-cached
  "Frame/request-frequency read of `configured-providers` that never re-runs
   the full enumeration on a warm caller. The enumeration behind
   `config/load-config` parses four config files per call — ~200ms on
   machines with slow file IO — which stalled every TUI footer frame when it
   ran on the render thread (issue #29).

   - FRESH snapshot → returned as-is (pure atom read).
   - STALE snapshot → returned immediately; a single-flight background
     refresh replaces it off-thread.
   - COLD (first read / just invalidated) → enumerates synchronously ONCE, so
     callers always get a real fleet, never a nil-because-cold."
  []
  (let [now
        (System/currentTimeMillis)

        {:keys [at val]}
        @fleet-cache]

    (cond (nil? at) (let [v (configured-providers)]
                      (reset! fleet-cache {:at now :val v})
                      v)
          (>= (- now (long at)) (long fleet-cache-ttl-ms)) (do (refresh-fleet-cache!) val)
          :else val)))

(defn available-presets
  "Provider presets not yet in the configured fleet — the 'Add
   Provider' picker contents.

   A MANAGED provider is never offered here: it carries no credential a human
   supplies and it binds itself, so an `Add provider` row for it could only ask
   for a key that every seam below must refuse."
  []
  (let [configured (into #{} (map :id) (configured-providers))]
    (vec (remove #(or (contains? configured (:id %)) (managed? (:id %)))
           (config/provider-presets)))))

(defn ensure-base-url
  [provider]
  (if (:base-url provider)
    provider
    (if-let [resolved (:base-url (config/provider-template (:id provider)))]
      (assoc provider :base-url resolved)
      provider)))

(defn persisted-provider-config
  "Convert an in-memory provider entry to the durable on-disk shape."
  [provider]
  (ensure-base-url provider))

(defn default-model-configs
  "Preset `:default-models` as persisted model maps. A bare-string entry
   becomes `{:name str}`; a MAP entry is carried through verbatim (name
   normalized) so a provider can declare `:context` / `:output-limit` / … for
   a model svar's pinned catalog doesn't know yet — no svar release, no
   enrich hook. `->svar-model` whitelists which of those keys svar honors, so
   extra keys are harmless."
  [preset]
  (->> (:default-models preset)
       (keep (fn [model]
               (when-let [name (some-> (config/model-name model)
                                       str
                                       str/trim
                                       not-empty)]
                 (if (map? model) (assoc model :name name) {:name name}))))
       distinct
       vec))

(defn authenticated-preset-providers
  "Registered providers that BIND THEMSELVES — the credential lives OUTSIDE the
   persisted fleet, so the provider is usable with no `Add provider` step at all.
   Shaped as minimal picker rows (`{:id … :models …}` carrying the preset's
   default catalog models) and appended by [[picker-fleet]].

   Two ways in. A MANAGED provider ([[managed?]]) binds because its runtime
   issues the credential: there is nothing local to probe and nothing a human
   could add. Every other provider binds only when its OWN `:provider/detect-fn`
   (local, no network) finds one — an OAuth token file, a keychain entry.

   A provider with neither, or with no default models, is skipped."
  []
  (let [configured (into #{} (map :id) (configured-providers))]
    (into []
          (keep (fn [{:provider/keys [id detect-fn is-managed]}]
                  (when (and id
                             (not (contains? configured id))
                             (or is-managed
                                 (and detect-fn
                                      (try (boolean (detect-fn)) (catch Throwable _ false)))))
                    (let [tmpl (config/provider-template id)
                          models (default-model-configs tmpl)]

                      (when (seq models)
                        (cond-> {:id id :models models}
                          (:base-url tmpl)
                          (assoc :base-url (:base-url tmpl))

                          (:api-style tmpl)
                          (assoc :api-style (:api-style tmpl))))))))
          (registry/registered-providers))))

(defn picker-fleet
  "The provider fleet a model picker should render: the persisted
   `configured-providers` first, then `authenticated-preset-providers`
   (authenticated-but-unconfigured OAuth providers whose creds live outside
   config) appended. This is what channel model pickers enumerate so
   authenticated providers are selectable even before they're saved into the
   fleet.

   Failure-isolated by construction: the base fleet reads through the
   never-nil `configured-providers-cached` (no per-open 4-file parse, no
   render-thread stall), and the authenticated-preset enumeration degrades to
   empty on any error. A transient hiccup therefore drops the OAuth extras at
   worst — it NEVER throws and NEVER blanks the picker of already-configured
   providers."
  []
  (let [base
        (or (try (configured-providers-cached) (catch Throwable _ nil))
            (try (configured-providers) (catch Throwable _ nil))
            [])

        extras
        (try (authenticated-preset-providers) (catch Throwable _ nil))

        ;; A deleted provider stays deleted even when its credential is still on the
        ;; machine. `extras` are synthesized on every read from an env var or a
        ;; stored credential, so absence from config cannot express "removed" for
        ;; them — `deleted-provider-ids` is where that decision lives.
        removed
        (try (config/deleted-provider-ids) (catch Throwable _ #{}))]

    (into []
          (remove #(contains? removed
                              (some-> (:id %)
                                      keyword)))
          (into (vec base) extras))))

(defn- split-model-ref
  "Split a config model reference into `[provider-keyword model-name]`.

   `provider/model` yields both halves and its provider part ALWAYS wins over the
   sibling `*_provider` key, exactly as `--model` behaves; a bare name yields
   `[nil name]`; blank or nil yields `[nil nil]`."
  [requested]
  (let [requested (some-> requested
                          str
                          str/trim
                          not-empty)]
    (if-let [idx (some-> requested
                         (str/index-of "/"))]
      (let [idx (long idx)]
        [(keyword (subs requested 0 idx)) (not-empty (subs requested (inc idx)))])
      [nil requested])))

(defn- provider-model-names
  "Model names of ONE fleet entry, in catalog order."
  [provider]
  (into []
        (keep #(some-> (config/model-name %)
                       str
                       not-empty))
        (:models provider)))

(defn- resolve-model-ref
  "Resolve one `<role>_model` / `<role>_provider` config pair against `fleet` to
   `[provider-keyword model-name]`.

   The `provider/model` form names a provider explicitly and wins over the
   sibling provider key, exactly as `--model` does — but ONLY when the prefix is
   a provider the fleet actually has, and never when the tagged provider itself
   exposes the whole name. Model ids legitimately CONTAIN slashes (openrouter
   serves `z-ai/glm-4.6v`), and splitting those made a picked default resolve to
   some other provider's first model, so choosing it appeared to do nothing."
  [requested tagged fleet]
  (let [requested*
        (some-> requested
                str
                str/trim
                not-empty)

        tagged*
        (some-> tagged
                str
                str/trim
                not-empty
                keyword)

        tagged-provider
        (some #(when (= tagged* (:id %)) %) fleet)

        [slash-provider slash-model]
        (split-model-ref requested*)]

    (cond (and requested* (some #{requested*} (provider-model-names tagged-provider))) [tagged*
                                                                                        requested*]
          (and slash-provider (some #(= slash-provider (:id %)) fleet)) [slash-provider slash-model]
          slash-provider [tagged* requested*]
          :else [tagged* slash-model])))

(defn resolve-default-selection
  "PURE: the valid PRIMARY provider/model pair `cfg`'s tags name within `fleet`.
   Explicit config wins; an untagged config falls back to the first provider and
   its first model, so a fleet always HAS a primary root while it has a provider.

   Pure because every surface must resolve the tag the way the router does —
   channels hold the config they just read, and a channel that re-read global
   state here would answer for a different machine's config in a test and for a
   stale one in a race."
  [cfg fleet]
  (let [;; `default_model` accepts the same `provider/model` form as `--model`, and
        ;; its provider part wins — the pair must resolve identically here and in
        ;; `loop/honor-config-roots!`, or the picker and the router disagree.
        [requested-provider requested-model]
        (resolve-model-ref (:default-model cfg) (:default-provider cfg) fleet)

        selected-provider
        (or (some #(when (= requested-provider (:id %)) %) fleet) (first fleet))

        model-names
        (provider-model-names selected-provider)

        selected-model
        (if (some #{requested-model} model-names) requested-model (first model-names))]

    (when (and selected-provider selected-model)
      {:provider-id (:id selected-provider) :model selected-model})))

(defn default-selection
  "The valid PRIMARY provider/model pair for `fleet`, read against THIS machine's
   merged config. `resolve-default-selection` is the resolution itself."
  ([] (default-selection (picker-fleet)))
  ([fleet] (resolve-default-selection (or (config/load-config) {}) fleet)))

(defn fallback-selection
  "Return the valid FALLBACK provider/model pair for `fleet`, or nil.

   The fallback is the pair the router drops to when the primary provider cannot
   serve the turn, so — unlike the primary — it is never implicit: an unset,
   unknown, or same-provider-as-primary tag resolves to nil rather than inventing
   a second choice nobody asked for. Only the MODEL is lenient (an unknown model
   name falls back to the provider's first), mirroring `default-selection`."
  ([] (fallback-selection (picker-fleet)))
  ([fleet] (fallback-selection fleet (default-selection fleet)))
  ([fleet primary]
   (let [cfg
         (or (config/load-config) {})

         [requested-provider requested-model]
         (resolve-model-ref (:fallback-model cfg) (:fallback-provider cfg) fleet)

         selected-provider
         (some #(when (= requested-provider (:id %)) %) fleet)

         model-names
         (provider-model-names selected-provider)

         selected-model
         (if (some #{requested-model} model-names) requested-model (first model-names))]

     (when (and selected-provider
                selected-model
                (not= (:id selected-provider) (:provider-id primary)))
       {:provider-id (:id selected-provider) :model selected-model}))))

(defn provider-config-with-models
  "Persistable provider config carrying the provider's complete catalog."
  [preset models]
  (cond-> {:id (:id preset) :models (vec models)}
    (:base-url preset)
    (assoc :base-url (:base-url preset))

    (:api-style preset)
    (assoc :api-style (:api-style preset))))

(def ^:private selection-keys
  "Config keys per router-root ROLE. `:primary` is the root every turn starts on;
   `:fallback` is the second root, and only ever names ANOTHER provider."
  {:primary {:provider "default_provider" :model "default_model"}
   :fallback {:provider "fallback_provider" :model "fallback_model"}})

(defn- ensure-default-selection!
  "Re-tag the PRIMARY root when a fleet mutation left it naming nobody. Answers
   true when config was rewritten.

   `default-selection` degrades a missing or dangling `default_provider` to the
   fleet's first provider, so ROUTING never broke — but the tag is what every
   surface READS, so the only provider a machine had just added showed up with no
   default at all (nothing was tagged until the user set it by hand), and removing
   the tagged provider left the tag naming what is gone. Both are the same missing
   write: whenever the fleet no longer holds the tagged provider, persist the pair
   the router would have picked anyway — the newcomer when it is the only one, the
   first survivor after a removal — and drop both keys when the fleet is empty, so
   config never names a ghost.

   A LIVE tag is left alone: adding a second provider must never steal a default
   the user chose. The check runs against the merged config, the write against the
   global file, exactly like `save-selection!` — a tag a project overlay owns is
   never copied into the machine's own config."
  [source]
  (let [fleet
        (picker-fleet)

        cfg
        (or (config/load-config) {})

        tagged
        (first (resolve-model-ref (:default-model cfg) (:default-provider cfg) fleet))]

    (when-not (some #(= tagged (:id %)) fleet)
      (when (config/update-machine-config!
              (fn [raw]
                (let [{:keys [provider-id model]}
                      (resolve-default-selection cfg fleet)

                      {provider-key :provider model-key :model}
                      (:primary selection-keys)

                      raw*
                      (if (and provider-id model)
                        (assoc raw
                          provider-key (name provider-id)
                          model-key model)
                        (apply dissoc raw (vals (:primary selection-keys))))]

                  (when (not= raw raw*) raw*)))
              source)
        (try (config/reload-config!) (catch Throwable _ nil))
        true))))

(defn update-providers!
  "Read-modify-write the persisted provider fleet under the machine-store lock:
   `f` receives the CURRENT provider vector and answers the next one.

   The read has to happen inside the lock. Reading the fleet, adding one provider
   and writing the whole vector back is a lost update the moment a second session
   (or the gateway's own toggle writer) does the same: both saw the same fleet and
   the last write silently dropped the other's provider."
  ([f] (update-providers! f nil))
  ([f source]
   (let [written (volatile! nil)]
     (config/update-machine-config!
       (fn [raw]
         (let [current (vec (:providers (config/runtime-config raw)))
               providers* (mapv persisted-provider-config (vec (f current)))]

           (vreset! written providers*)
           (if (seq providers*) (assoc raw "providers" providers*) (dissoc raw "providers"))))
       source)
     (try (config/reload-config!) (catch Throwable _ nil))
     (invalidate-configured-providers!)
     (ensure-default-selection! source)
     (rebuild-shared-router!)
     @written)))

(defn save-providers!
  "Replace the provider vector in the global string-keyed config while preserving
   unrelated keys, then refresh runtime provider state."
  ([providers] (save-providers! providers nil))
  ([providers source] (update-providers! (constantly providers) source)))

(defn- raw-tagged-provider
  "The provider a RAW config's `<role>_provider`/`<role>_model` pair names, as a
   keyword, resolved by `resolve-model-ref` so the `provider/model` form and a
   model id that merely CONTAINS a slash are told apart exactly as they are on
   the read path in `default-selection`/`fallback-selection`."
  [raw {provider-key :provider model-key :model} fleet]
  (first (resolve-model-ref (get raw model-key) (get raw provider-key) fleet)))

(defn- selectable-model-names
  "Every model id the PICKER offers for one fleet entry: the configured catalog
   plus the provider's LIVE-fetched and preset ids.

   The picker and the save path MUST agree. `model-options` is what channels
   list, so a model the user can see and choose has to be persistable — reading
   only the configured catalog here rejected every live model with \"Unknown
   model for provider\", which is exactly the case where the user is switching
   the default AWAY from what config already names. Degrades to the configured
   catalog when the live fetch is unavailable."
  [provider]
  (into (set (provider-model-names provider))
        (try (:models (model-options provider (default-model-names provider) true))
             (catch Throwable _ nil))))

(defn- save-selection!
  "Validate one provider/model pair against the live fleet and persist it under
   `role`'s config keys. The selected provider is persisted with its complete
   model catalog when it was authenticated but not yet configured. Provider/model
   ordering is not changed.

   The two roles are kept mutually exclusive at the point of intent: `:fallback`
   REFUSES the provider the primary already names — a same-provider second choice
   is what svar's own model retry covers, and a fleet whose two tags name one
   provider has no second opinion left — and a new `:primary` drops a fallback
   tag that would now collide with it."
  [role provider-id model source]
  (let [provider-id*
        (cond (keyword? provider-id) provider-id
              (string? provider-id) (keyword provider-id))

        model*
        (some-> model
                str
                str/trim
                not-empty)

        fleet
        (picker-fleet)

        selected
        (some #(when (= provider-id* (:id %)) %) fleet)

        model-names
        (selectable-model-names selected)

        primary
        (:provider-id (default-selection fleet))

        {provider-key :provider model-key :model}
        (get selection-keys role)]

    (when-not selected (throw (ex-info "Unknown provider" {:provider provider-id})))
    (when-not (and model* (contains? model-names model*))
      (throw (ex-info "Unknown model for provider" {:provider provider-id* :model model})))
    (when (and (= role :fallback) primary (= primary provider-id*))
      (throw (ex-info
               (str "Fallback provider must differ from the primary provider (" (name primary) ")")
               {:type :vis/invalid-fallback-provider :provider provider-id* :primary primary})))
    ;; The one place an unresolvable credential IS fatal: the user is explicitly
    ;; reaching for THIS provider, so the error is actionable rather than
    ;; collateral. Loading stays lenient (see `config/interpolate-env`) and
    ;; implicit routing silently skips the provider; only intent hard-fails.
    (when-let [{:keys [reason env-vars]} (config/provider-credential-gap selected)]
      (throw (ex-info reason
                      {:type :vis/provider-env-unset :provider provider-id* :env-vars env-vars})))
    (config/update-machine-config!
      (fn [raw]
        (let [current
              (vec (:providers (config/runtime-config raw)))

              existing
              (some #(when (= provider-id* (:id %)) %) current)

              ;; A model picked from the LIVE catalog is written into the provider's
              ;; persisted models: `default-selection` resolves `default_model` against
              ;; that catalog only, so an unlisted name would silently revert to the
              ;; provider's first model on the very next read.
              catalog
              (let [models (vec (:models selected))]
                (if (some #(= model* (config/model-name %)) models)
                  models
                  (conj models {:name model*})))

              selected-config
              (merge selected existing {:models catalog})

              providers*
              (if existing
                (mapv #(if (= provider-id* (:id %)) selected-config %) current)
                (conj current selected-config))

              raw*
              (assoc raw
                "providers" (mapv persisted-provider-config providers*)
                provider-key (name provider-id*)
                model-key model*)

              raw*
              (if (and (= role :primary)
                       (= provider-id* (raw-tagged-provider raw* (:fallback selection-keys) fleet)))
                (apply dissoc raw* (vals (:fallback selection-keys)))
                raw*)]

          raw*))
      source)
    (try (config/reload-config!) (catch Throwable _ nil))
    (rebuild-shared-router!)
    {:provider-id provider-id* :model model*}))

(defn save-default-selection!
  "Persist exactly one PRIMARY provider/model pair — the router root every turn
   starts on. A fallback tag naming the same provider is dropped, so the two tags
   always name two providers."
  ([provider-id model] (save-default-selection! provider-id model nil))
  ([provider-id model source] (save-selection! :primary provider-id model source)))

(defn save-fallback-selection!
  "Persist exactly one FALLBACK provider/model pair — the router's SECOND root,
   used when the primary provider cannot serve the turn. Throws when it names the
   primary's provider, an unknown provider, or a model that provider does not
   expose."
  ([provider-id model] (save-fallback-selection! provider-id model nil))
  ([provider-id model source] (save-selection! :fallback provider-id model source)))

(defn clear-fallback-selection!
  "Drop the fallback tag. The fleet keeps every provider; only the second root
   goes away, leaving svar's own priority order to decide what follows the
   primary."
  ([] (clear-fallback-selection! nil))
  ([source]
   (config/update-machine-config! (fn [raw]
                                    (apply dissoc raw (vals (:fallback selection-keys))))
                                  source)
   (try (config/reload-config!) (catch Throwable _ nil))
   (rebuild-shared-router!)
   nil))

(defn add-config-provider!
  "Append a provider config to the persisted fleet (no-op when its id exists)."
  ([provider-cfg] (add-config-provider! provider-cfg nil))
  ([provider-cfg source]
   ;; Adding it back must bring it back: a stale deletion would otherwise hide
   ;; the provider the operator just asked for.
   (try (config/unsuppress-provider! (:id provider-cfg) source) (catch Throwable _ nil))
   (update-providers!
     (fn [current]
       (if (some #(= (:id provider-cfg) (:id %)) current) current (conj current provider-cfg)))
     source)))

(defn update-config-provider!
  "Apply `f` to one persisted provider and save the resulting fleet."
  ([provider-id f] (update-config-provider! provider-id f nil))
  ([provider-id f source]
   (update-providers! (fn [current]
                        (mapv #(if (= provider-id (:id %)) (f %) %) current))
                      source)))

(defn save-provider-api-key!
  "Persist `api-key` for `provider-id` in THIS process' config — the headless
   twin of a channel's API-key dialog, so a phone (or a TUI attached to a remote
   gateway) never writes provider credentials on the wrong machine. Adds the
   provider from its preset when the fleet does not carry it yet."
  ([provider-id api-key] (save-provider-api-key! provider-id api-key nil))
  ([provider-id api-key source]
   (update-providers! (fn [current]
                        (if (some #(= provider-id (:id %)) current)
                          (mapv #(if (= provider-id (:id %)) (assoc % :api-key api-key) %) current)
                          (let [tmpl
                                (config/provider-template provider-id)

                                models
                                (default-model-configs tmpl)]

                            (conj current
                                  (cond-> {:id provider-id :api-key api-key}
                                    (seq models)
                                    (assoc :models models)

                                    (:base-url tmpl)
                                    (assoc :base-url (:base-url tmpl)))))))
                      source)))

(defn clear-provider-api-key!
  "Forget `provider-id`'s stored API key while KEEPING its config entry.

   This is what \"log out\" means for a key-only provider: the credential goes, the
   provider stays configured with its models, base-url and tags, so signing back in
   is one key away. Returns true when a key was actually cleared."
  ([provider-id] (clear-provider-api-key! provider-id nil))
  ([provider-id source]
   (let [cleared (volatile! false)]
     (update-providers! (fn [current]
                          (mapv (fn [entry]
                                  (if (and (= provider-id (:id entry)) (some? (:api-key entry)))
                                    (do (vreset! cleared true) (dissoc entry :api-key))
                                    entry))
                                current))
                        source)
     @cleared)))

(defn remove-provider!
  "Remove a provider from the persisted fleet AND run the registered
   extension's logout when present. Invalidates the fleet snapshot.
   Returns true when config changed."
  ([provider-id] (remove-provider! provider-id nil))
  ([provider-id source]
   (when-let [logout-fn (:provider/logout-fn (registry/provider-by-id provider-id))]
     (try (logout-fn) (catch Throwable _ nil)))
   (let [;; Both halves, always: drop the config entry when there is one, and
         ;; record the deletion so a provider that never had one — synthesized
         ;; from an env var or a credential file — is gone too. Deleting used to
         ;; be a no-op for those, which the operator could only read as a broken
         ;; button.
         changed?
         (config/remove-config-provider! provider-id source)

         _
         (config/suppress-provider! provider-id source)]

     (try (config/reload-config!) (catch Throwable _ nil))
     (invalidate-configured-providers!)
     (ensure-default-selection! source)
     (rebuild-shared-router!)
     changed?)))
