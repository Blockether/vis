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
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.format :as format]
            [com.blockether.vis.internal.limits-format :as limits-format]
            [com.blockether.vis.internal.provider-limits :as provider-limits]
            [com.blockether.vis.internal.registry :as registry])
  (:import [java.net ConnectException URI]
           [java.net.http HttpClient HttpClient$Version HttpConnectTimeoutException HttpRequest
            HttpRequest$Builder HttpResponse$BodyHandlers]
           [java.time Duration]
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

(defn auth-kind
  "How a provider id authenticates: `:oauth` (interactive flow owned by
   the provider extension), `:none` (local, no credentials), or
   `:api-key`."
  [pid]
  (cond (contains? oauth-provider-ids pid) :oauth
        (contains? local-no-auth-provider-ids pid) :none
        :else :api-key))

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
  (try
    (let
      [provider-id
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
   provider `:default-models`, deduped. The list-building below sorts,
   so order here is irrelevant."
  [provider]
  (->> (concat (map config/model-name (:models provider))
               (:default-models (config/provider-template (:id provider)))
               (:default-models provider))
       (remove nil?)
       distinct
       vec))

(defn model-options
  "Selectable model ids for a provider: live-fetched + defaults,
   deduped, sorted, env default pinned first. When `show-all?` is
   false, dated snapshot variants (gpt-4o-2024-08-06) are hidden.

   Returns `{:models [id ...] :hidden-count n}` — channels render
   their own 'show all' affordance from `:hidden-count`."
  ([provider] (model-options provider (default-model-names provider) false))
  ([provider default-models show-all?]
   (let
     [provider-id
      (:id provider)

      fetched
      (or (fetch-models provider) [])

      defaults
      (filterv #(config/provider-model-visible? provider-id %) (or default-models []))

      all-ids
      (->> (concat fetched defaults)
           distinct
           sort
           vec)

      pinned
      (pin-default all-ids)

      visible
      (if show-all? pinned (filterv (complement dated-variant?) pinned))]

     {:models visible :hidden-count (- (count pinned) (count visible))})))

;;; ── Status + limits ─────────────────────────────────────────────────────────

(defn safe-provider-status
  "Status of a REGISTERED provider descriptor via its `:provider/status-fn`
   (falling back to `:provider/detect-fn`). Never throws."
  [provider]
  (try (cond (:provider/status-fn provider) ((:provider/status-fn provider))
             (:provider/detect-fn provider) {:authenticated? (boolean ((:provider/detect-fn
                                                                         provider)))}
             :else nil)
       (catch Throwable e {:authenticated? false :error (or (ex-message e) (str e))})))

(defn probe-local-reachable
  "Probe a local OpenAI-compatible provider (Ollama / LM Studio) by
   GETting its `<base-url>/models` endpoint with a short timeout.
   Reachable → `{:authenticated? true …}`; refused / timeout / other →
   `{:authenticated? false :error \"<human hint>\"}` so the channel can
   SAY why the dot is red. Blocking ≤ ~2.5s — call off the render path."
  [provider]
  (let
    [base
     (or (config/provider-base-url provider) (:base-url provider))

     label
     (config/display-label (:id provider))

     base*
     {:authenticated? false :source :local :provider-id (:id provider) :base-url base}]

    (if (str/blank? base)
      (assoc base* :error (str label ": no base URL configured"))
      (let
        [url
         (str (str/replace base #"/+$" "") "/models")

         host
         (url-host base)]

        (try (let
               [client
                (-> (HttpClient/newBuilder)
                    (.connectTimeout (Duration/ofMillis 1500))
                    ;; Force HTTP/1.1: the default client negotiates h2c
                    ;; (`Upgrade` on plain http) and LM Studio's server
                    ;; hangs on that upgrade instead of answering — the
                    ;; probe then times out against a perfectly live
                    ;; endpoint and the health gate demotes it every turn.
                    (.version HttpClient$Version/HTTP_1_1)
                    (.build))

                ^HttpRequest$Builder rb
                (HttpRequest/newBuilder (URI/create url))

                req
                (-> rb
                    (.timeout (Duration/ofMillis 2500))
                    (.GET)
                    (.build))

                resp
                (.send client req (HttpResponse$BodyHandlers/discarding))

                code
                (.statusCode resp)]

               ;; Any answer below 500 means the server is up — even a
               ;; 401/404 proves the port is live. 5xx is the server failing.
               (if (< code 500)
                 (assoc base* :authenticated? true)
                 (assoc base* :error (str label " returned HTTP " code " at " host))))
             (catch ConnectException _
               (assoc base* :error (str "Can't reach " label " at " host " — is it running?")))
             (catch HttpConnectTimeoutException _
               (assoc base* :error (str label " timed out at " host " — is it running?")))
             (catch Throwable e
               (assoc base*
                 :error
                 (str "Can't reach " label " at " host " (" (or (ex-message e) (str e)) ")"))))))))

(defn provider-status
  "Auth/liveness status for a CONFIGURED provider map. Local providers
   are probed for real; an explicit `:api-key` is trusted; otherwise
   the registered extension's status/detect fns answer. Never throws."
  [provider]
  (let [registered (registry/provider-by-id (:id provider))]
    (cond
      ;; Local no-auth providers (Ollama / LM Studio) have no key and
      ;; their registered status-fn is a hardcoded stub — probe the
      ;; endpoint for real.
      (contains? local-no-auth-provider-ids (:id provider)) (probe-local-reachable provider)
      (some? (:api-key provider))
      {:authenticated? true :source :config :config-path config/config-path}
      registered (or (safe-provider-status registered) {:authenticated? false})
      :else {:authenticated? false})))

(defn provider-reachable?
  "Cheap ROUTING-time liveness verdict: local providers (Ollama /
   LM Studio) get the real HTTP probe; remote providers are assumed
   reachable — their auth/network failures surface as call errors svar
   already fails over on, and a per-turn network check against every
   remote backend would tax every turn."
  [provider]
  (if (contains? local-no-auth-provider-ids (:id provider))
    (boolean (:authenticated? (probe-local-reachable provider)))
    true))

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
               {:router (assoc router :providers (vec (concat ok bad))) :demoted (mapv :id bad)}
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
  "Placeholder status while a real probe runs in the background."
  [provider]
  (if (some? (:api-key provider))
    {:authenticated? true :source :config :config-path config/config-path}
    {:authenticated? nil :loading? true}))

(defn initial-provider-limits
  "Placeholder limits report while the real fetch runs."
  [provider]
  {:provider-id (:id provider) :status :loading :static {} :dynamic {:limits []}})

;;; ── Status report text ──────────────────────────────────────────────────────

(defn- status-entry-label
  [k]
  (-> (name k)
      (str/replace #"-" " ")
      (str/capitalize)))

(defn- format-status-value
  [v]
  (cond (keyword? v) (name v)
        :else (str v)))

(defn- format-limit-window
  [{:keys [kind unit size resets-at-ms]}]
  (when kind
    (str (name kind)
         (when unit (str " " (or size 1) "/" (name unit)))
         (when resets-at-ms (str ", resets " (format/format-date (Date. (long resets-at-ms))))))))

(defn format-limit-row
  [{:keys [label scope kind unlimited? used limit remaining note window]}]
  (let
    [quota
     (cond unlimited? "unlimited"
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

(defn status-text
  "Multi-line human status + limits report for a configured provider.
   The single source for the TUI 'Show Status + Limits' dialog and the
   web status view."
  ([provider] (status-text provider (provider-status provider) (provider-limits-safe provider)))
  ([provider status limits]
   (let
     [status
      (or status (initial-provider-status provider))

      limits
      (or limits (initial-provider-limits provider))

      title
      (str (config/display-label (:id provider)) " Status")

      rows
      (->> status
           (remove (fn [[k _]]
                     (= k :authenticated?)))
           (sort-by (comp str key))
           (map (fn [[k v]]
                  (str (status-entry-label k) ": " (format-status-value v)))))

      dynamic
      (get-in limits [:dynamic :limits])]

     (str/join
       "\n"
       (concat
         [title "" (str "Base URL: " (or (config/provider-base-url provider) "-"))
          (str "Authenticated: " (if (:authenticated? status) "yes" "no"))]
         (when-let [e (:error status)]
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
  "Status-map detail entries (minus :authenticated?/:error) as md list rows."
  [status]
  (->> status
       (remove (fn [[k _]]
                 (contains? #{:authenticated? :error :loading?} k)))
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
   (let
     [status
      (or status (initial-provider-status provider))

      limits
      (or limits (initial-provider-limits provider))

      label
      (config/display-label (:id provider))

      ok?
      (boolean (:authenticated? status))

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
               (if ok? "yes ✓" "no ✗")
               "  ·  **Base URL:** `"
               (or (config/provider-base-url provider) "-")
               "`")]
         (when-let [e (:error status)]
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
  (let
    [now
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
   Provider' picker contents."
  []
  (let [configured (into #{} (map :id) (configured-providers))]
    (vec (remove #(contains? configured (:id %)) (config/provider-presets)))))

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
               (when-let
                 [name (some-> (config/model-name model)
                               str
                               str/trim
                               not-empty)]
                 (if (map? model) (assoc model :name name) {:name name}))))
       distinct
       vec))

(defn provider-config-with-models
  "Minimal persistable provider config for a preset + chosen models."
  [preset models]
  (cond-> {:id (:id preset) :models models}
    (:base-url preset)
    (assoc :base-url (:base-url preset))))

(defn save-providers!
  "Replace the `:providers` vec in the GLOBAL config file (project
   overlay files are never edited), preserving unrelated keys, then
   reload the in-memory config so the running router sees the change.
   Invalidates the fleet snapshot so `configured-providers-cached`
   readers (the TUI footer) pick the change up on their next read.
   Returns the persisted vec."
  ([providers] (save-providers! providers nil))
  ([providers source]
   (let
     [raw
      (or (config/load-global-config-raw) {})

      providers*
      (mapv persisted-provider-config providers)]

     (config/save-config!
       (if (seq providers*) (assoc raw :providers providers*) (dissoc raw :providers))
       source)
     (try (config/reload-config!) (catch Throwable _ nil))
     (invalidate-configured-providers!)
     providers*)))

(defn add-config-provider!
  "Append a provider config to the persisted fleet (no-op when the id
   is already configured). Returns the new fleet or nil on no-op."
  ([provider-cfg] (add-config-provider! provider-cfg nil))
  ([provider-cfg source]
   (let [current (vec (:providers (config/load-global-config-raw)))]
     (when-not (some #(= (:id provider-cfg) (:id %)) current)
       (save-providers! (conj current provider-cfg) source)))))

(defn update-config-provider!
  "Apply `f` to the persisted provider entry with `provider-id` and
   save. Returns the new fleet."
  ([provider-id f] (update-config-provider! provider-id f nil))
  ([provider-id f source]
   (let [current (vec (:providers (config/load-global-config-raw)))]
     (save-providers! (mapv #(if (= provider-id (:id %)) (f %) %) current) source))))

(defn remove-provider!
  "Remove a provider from the persisted fleet AND run the registered
   extension's logout when present. Invalidates the fleet snapshot.
   Returns true when config changed."
  ([provider-id] (remove-provider! provider-id nil))
  ([provider-id source]
   (when-let [logout-fn (:provider/logout-fn (registry/provider-by-id provider-id))]
     (try (logout-fn) (catch Throwable _ nil)))
   (let [changed? (config/remove-config-provider! provider-id source)]
     (try (config/reload-config!) (catch Throwable _ nil))
     (invalidate-configured-providers!)
     changed?)))
