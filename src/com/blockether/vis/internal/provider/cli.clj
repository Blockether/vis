(ns com.blockether.vis.internal.provider.cli
  "`vis-agent providers` commands: list providers, show status and rate limits,
   sign in and sign out."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.commandline :as commandline]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.error :as error]
            [com.blockether.vis.internal.extension.registry :as registry]
            [com.blockether.vis.internal.format :as fmt]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.provider.catalog :as catalog]
            [com.blockether.vis.internal.provider.service :as providers]
            [com.blockether.vis.contract.wire :as wire]))

(def ^:private providers-table-cols
  [{:key :id :label "ID" :width 18 :align :left} {:key :label :label "Label" :width 28 :align :left}
   {:key :auth :label "Auth" :width 6 :align :left}
   {:key :rpm :label "Catalog RPM" :width 11 :align :right}
   {:key :tpm :label "Catalog TPM" :width 12 :align :right}
   {:key :base-url :label "Base URL" :width 36 :align :left}])

(defn- gateway-provider-status-safe
  [provider-id]
  (try (gateway-client/provider-status provider-id)
       (catch Throwable e {"is_authenticated" false "error" (or (ex-message e) (str e))})))

(defn- gateway-provider-limits-safe
  [provider-id]
  (try (gateway-client/provider-limits provider-id)
       (catch Throwable e
         {:provider-id provider-id
          :status :error
          :static {}
          :dynamic {:limits []}
          :error {:message (or (ex-message e) (str e))}})))

(defn- configured-provider-entry
  [provider-id]
  (->> (or (:providers (config/current-config)) [])
       (filter #(= provider-id (:id %)))
       first))

(defn- configured-provider-status [provider] (gateway-provider-status-safe (:provider/id provider)))

(defn- configured-provider-base-url
  [provider-id]
  (or (:base-url (configured-provider-entry provider-id))
      (some-> provider-id
              catalog/template
              :base-url)))

(defn- provider-label-for-id
  "Registered/preset branding first, else the id VERBATIM — a `vis.yml` id keeps
   the casing its author typed (see `config/display-label`)."
  [provider-id]
  (or (some-> (registry/provider-by-id provider-id)
              :provider/label)
      (some-> (catalog/template provider-id)
              :label)
      (some-> provider-id
              name)))

(defn- format-limit-window
  [{:keys [kind unit size resets-at-ms]}]
  (when kind
    (str (name kind)
         (when unit (str " " (or size 1) "/" (name unit)))
         (when resets-at-ms
           (str ", resets " (fmt/format-date (java.util.Date. (long resets-at-ms))))))))

(defn- format-limit-row
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

(defn- provider-limit-lines
  [provider-id]
  (let [report
        (gateway-provider-limits-safe provider-id)

        static
        (:static report)

        dynamic
        (get-in report [:dynamic :limits])

        note
        (get-in report [:dynamic :note])

        error*
        (:error report)]

    (vec (concat [(str "  Limits status: " (name (:status report)))]
                 (when-let [rpm (:rpm static)]
                   [(str "  Catalog RPM:    " rpm)])
                 (when-let [tpm (:tpm static)]
                   [(str "  Catalog TPM:    " tpm)])
                 (if (seq dynamic)
                   (concat ["  Dynamic limits:"] (map #(str "    - " (format-limit-row %)) dynamic))
                   ["  Dynamic limits: none reported"])
                 (when note [(str "  Note:           " note)])
                 (when (seq static)
                   ["  Catalog RPM / TPM come from svar metadata, not live account quota usage."])
                 (when error* [(str "  Error:          " (:message error*))])))))

(defn- print-provider-status!
  ([provider] (print-provider-status! provider (configured-provider-status provider)))
  ([provider status]
   (let [status
         (or status {"is_authenticated" false})

         provider-id
         (:provider/id provider)

         base-url
         (configured-provider-base-url provider-id)

         rows
         (->> status
              (remove (fn [[k _]]
                        (contains? providers/summary-owned-status-keys k)))
              (sort-by (comp str key)))]

     (commandline/stdout! (str "\n  " (:provider/label provider) " Provider Status"))
     (commandline/stdout! "  ─────────────────────────────────")
     (when base-url (commandline/stdout! (str "  Base URL:       " base-url)))
     (commandline/stdout! (str "  Authenticated:  " (providers/auth-summary status false)))
     (doseq [[k v] rows]
       (commandline/stdout! (str "  "
                                 (commandline/pad-right (str (providers/status-entry-label k) ":")
                                                        15)
                                 (providers/format-status-value v))))
     (doseq [line (provider-limit-lines provider-id)]
       (commandline/stdout! line))
     (commandline/stdout! ""))))

(defn- print-provider-limits!
  [provider-id]
  (commandline/stdout! (str "\n  " (provider-label-for-id provider-id) " Limits"))
  (commandline/stdout! "  ─────────────────────────────────")
  (doseq [line (provider-limit-lines provider-id)]
    (commandline/stdout! line))
  (commandline/stdout! ""))

(defn- providers-list-rows
  []
  (->> (registry/registered-providers)
       (sort-by :provider/id)
       (mapv
         (fn [provider]
           (let [status
                 (configured-provider-status provider)

                 report
                 (gateway-provider-limits-safe (:provider/id provider))

                 base-url
                 (configured-provider-base-url (:provider/id provider))]

             {:id (name (:provider/id provider))
              :label (:provider/label provider)
              :auth (name (providers/auth-verdict status))
              :rpm (or (some-> report
                               :static
                               :rpm
                               str)
                       "-")
              :tpm (or (some-> report
                               :static
                               :tpm
                               str)
                       "-")
              :base-url (or base-url "-")})))))

(defn- print-registered-providers!
  []
  (let [all (registry/registered-providers)]
    (if (seq all)
      ;; Width tracks the LONGEST provider id + a 2-space gutter so ids like
      ;; `anthropic-coding-plan` (21 chars) never run into their label.
      (let [w (+ 2 (long (reduce max 0 (map #(count (name (:provider/id %))) all))))]
        (commandline/stdout! "Available providers:")
        (doseq [p (sort-by :provider/id all)]
          (commandline/stdout!
            (str "  " (commandline/pad-right (name (:provider/id p)) w) (:provider/label p)))))
      (commandline/stdout! "No providers registered."))))

(defn- cli-providers-list!
  [_parsed _residual]
  (config/init-cli!)
  (let [rows (providers-list-rows)]
    (if (empty? rows)
      (commandline/stdout! "No providers registered.")
      (do (commandline/stdout! "\n  Providers\n")
          (commandline/print-table! providers-table-cols rows)
          (commandline/stdout! (str "\n  " (count rows) " provider(s)\n")))))
  (shutdown-agents))

(def ^:private status-exit-codes
  "Exit status of `providers status <provider>` for each reading of its state."
  {:verified 0 :unknown-provider 2 :not-authenticated 3 :not-verified 4})

(defn- provider-auth-record
  "The machine-readable auth state of one provider. It never holds a token or a
   token preview."
  [provider status]
  (let [verdict (providers/auth-verdict status)]
    (cond-> {"provider" (name (:provider/id provider))
             "label" (:provider/label provider)
             "authenticated" (or (= :verified verdict)
                                 (and (true? (get status "is_authenticated"))
                                      (not= :rejected verdict)))
             "state" (name verdict)}
      (some? (get status "source"))
      (assoc "source" (str (get status "source")))

      (some? (get status "account_type"))
      (assoc "account_type" (str (get status "account_type")))

      (some? (get status "error"))
      (assoc "error" (str (get status "error"))))))

(defn- auth-exit-code
  [{:strs [authenticated state]}]
  (cond (= "verified" state) (:verified status-exit-codes)
        authenticated (:not-verified status-exit-codes)
        :else (:not-authenticated status-exit-codes)))

(defn- finish!
  "End the command with `code`. A non-zero code ends the process with it."
  [code]
  (shutdown-agents)
  (when-not (zero? (long code)) (System/exit (int code))))

(defn- cli-providers-status!
  [parsed _residual]
  (config/init-cli!)
  (let [provider-name
        (get parsed "provider")

        json?
        (boolean (get parsed "json"))

        quiet?
        (boolean (get parsed "quiet"))

        provider-id
        (some-> provider-name
                keyword)

        provider
        (when provider-id (registry/provider-by-id provider-id))

        providers
        (if provider-name
          (if provider [provider] [])
          (sort-by :provider/id (registry/registered-providers)))]

    (cond (and provider-name (nil? provider))
          (do (when-not quiet?
                (if json?
                  (commandline/stdout! (wire/json-str {"provider" provider-name
                                                       "error" "unknown provider"}))
                  (do (commandline/stdout! (str "Unknown provider: " provider-name))
                      (commandline/stdout! "")
                      (print-registered-providers!))))
              (finish! (:unknown-provider status-exit-codes)))
          (or json? quiet?)
          (let [records (mapv #(provider-auth-record % (configured-provider-status %)) providers)]
            (when (and json? (not quiet?))
              (commandline/stdout! (wire/json-str (if provider-name (first records) records))))
            (let [code (if provider-name (auth-exit-code (first records)) 0)]
              (finish! code)))
          (empty? providers) (do (commandline/stdout! "No providers registered.") (finish! 0))
          :else (let [statuses (mapv (juxt identity configured-provider-status) providers)]
                  (doseq [[p status] statuses]
                    (print-provider-status! p status))
                  (let [code (if provider-name
                               (auth-exit-code (apply provider-auth-record (first statuses)))
                               0)]
                    (finish! code))))))

(defn- cli-providers-limits!
  [_parsed residual]
  (config/init-cli!)
  (let [provider-name
        (first residual)

        registered
        (sort-by :provider/id (registry/registered-providers))]

    (if provider-name
      (let [provider-id
            (keyword provider-name)

            known?
            (or (registry/provider-by-id provider-id)
                (catalog/template provider-id)
                (seq (:static (gateway-provider-limits-safe provider-id))))]

        (if known?
          (print-provider-limits! provider-id)
          (do (commandline/stdout! (str "Unknown provider: " provider-name))
              (commandline/stdout! "")
              (print-registered-providers!))))
      (if (seq registered)
        (doseq [provider registered]
          (print-provider-limits! (:provider/id provider)))
        (commandline/stdout! "No providers registered."))))
  (shutdown-agents))

(defn- cli-providers-auth!
  [parsed residual]
  (config/init-cli!)
  (let [provider-name
        (or (get parsed "provider") (first residual))

        provider-id
        (some-> provider-name
                keyword)

        provider
        (when provider-id (registry/provider-by-id provider-id))]

    (cond (nil? provider-id) (do (commandline/stdout! "Usage: vis-agent providers auth <provider>")
                                 (commandline/stdout! "")
                                 (print-registered-providers!))
          (nil? provider) (do (commandline/stdout! (str "Unknown provider: " provider-name))
                              (commandline/stdout! "")
                              (print-registered-providers!))
          (nil? (:provider/auth-fn provider)) (commandline/stdout!
                                                (str "Provider "
                                                     (:provider/label provider)
                                                     " does not expose an interactive auth flow."))
          :else (try ((:provider/auth-fn provider) commandline/stdout!)
                     (catch Exception e
                       (commandline/stdout! (error/format-error (str "Authentication failed: "
                                                                     (ex-message e))))))))
  (shutdown-agents))

(defn- cli-providers-logout!
  [parsed residual]
  (config/init-cli!)
  (let [provider-name
        (or (get parsed "provider") (first residual))

        provider-id
        (some-> provider-name
                keyword)

        provider
        (when provider-id (registry/provider-by-id provider-id))

        configured?
        (boolean (some #(= (name provider-id) (get % "id"))
                       (get (config/load-config-raw) "providers")))]

    (cond (nil? provider-id) (do (commandline/stdout!
                                   "Usage: vis-agent providers logout <provider>")
                                 (commandline/stdout! "")
                                 (print-registered-providers!))
          (nil? provider) (do (commandline/stdout! (str "Unknown provider: " provider-name))
                              (commandline/stdout! "")
                              (print-registered-providers!))
          (and (nil? (:provider/logout-fn provider)) (not configured?))
          (commandline/stdout!
            (str "Provider " (:provider/label provider) " does not persist credentials."))
          :else (do (if-let [logout-fn (:provider/logout-fn provider)]
                      (logout-fn)
                      ;; Key-only provider: forget the KEY, keep the entry.
                      (providers/clear-provider-api-key! provider-id :cli-provider-logout))
                    ;; The config entry stays: logging out drops the credential, not
                    ;; the provider's models/base-url, so signing back in is one
                    ;; `providers auth` away.
                    (commandline/stdout! (str
                                           "  Logged out of "
                                           (:provider/label provider)
                                           ". Credentials cleared; provider stays configured.")))))
  (shutdown-agents))

(def command
  {:cmd/name "providers"
   :cmd/doc "Inspect, authenticate, and introspect LLM providers."
   :cmd/usage "vis-agent providers <list|status|limits|auth|logout> [...]"
   :cmd/subcommands #(registry/registered-under ["providers"])})

(def subcommands
  "Subcommands registered under `vis-agent providers`."
  [{:cmd/name "list"
    :cmd/parent ["providers"]
    :cmd/doc "List registered providers with auth state, static limits, and base URLs."
    :cmd/usage "vis-agent providers list"
    :cmd/run-fn cli-providers-list!}
   {:cmd/name "status"
    :cmd/parent ["providers"]
    :cmd/doc "Show provider authentication status together with static/dynamic limits."
    :cmd/usage "vis-agent providers status [provider] [--json] [--quiet]"
    :cmd/args
    [{:name "provider"
      :kind :positional
      :type :string
      :doc "Registered provider id (for example: github-copilot). Without it, show every provider."}
     {:name "json"
      :kind :flag
      :type :boolean
      :doc "Print the auth state as JSON, without limits, tokens or token previews."}
     {:name "quiet" :kind :flag :type :boolean :doc "Print nothing. Read the exit status."}]
    :cmd/extra-sections
    [{:title "JSON OUTPUT"
      :body
      (str
        "  One object for a named provider, else an array with one object for each provider.\n"
        "  Keys: provider, label, authenticated, state, and source, account_type and error when known.\n"
        "  state is verified, rejected, degraded (usable; live check unavailable) or unverified.")}
     {:title "EXIT STATUS"
      :body (str "  With a provider name:\n" "    0  the provider is authenticated and verified\n"
                 "    2  the provider is not registered\n"
                 "    3  the provider is not authenticated, or it rejected the credential\n"
                 "    4  a credential is saved, but no live check verified it\n"
                 "  Without a provider name, the exit status is 0.")}]
    :cmd/examples ["vis-agent providers status" "vis-agent providers status github-copilot"
                   "vis-agent providers status github-copilot --json"
                   "vis-agent providers status github-copilot --quiet && echo signed in"]
    :cmd/run-fn cli-providers-status!}
   {:cmd/name "limits"
    :cmd/parent ["providers"]
    :cmd/doc "Show provider rate-limit metadata and any dynamic quota report."
    :cmd/usage "vis-agent providers limits [provider]"
    :cmd/examples ["vis-agent providers limits" "vis-agent providers limits openai-codex"
                   "vis-agent providers limits ollama"]
    :cmd/run-fn cli-providers-limits!}
   {:cmd/name "auth"
    :cmd/parent ["providers"]
    :cmd/doc "Run a provider's interactive authentication flow."
    :cmd/usage "vis-agent providers auth <provider>"
    :cmd/args [{:name "provider"
                :kind :positional
                :type :string
                :doc "Registered provider id (for example: github-copilot or openai-codex)."}]
    :cmd/examples ["vis-agent providers auth github-copilot"
                   "vis-agent providers auth openai-codex"]
    :cmd/run-fn cli-providers-auth!}
   {:cmd/name "logout"
    :cmd/parent ["providers"]
    :cmd/doc "Clear saved credentials for a provider."
    :cmd/usage "vis-agent providers logout <provider>"
    :cmd/args [{:name "provider" :kind :positional :type :string :doc "Registered provider id."}]
    :cmd/examples ["vis-agent providers logout github-copilot"
                   "vis-agent providers logout openai-codex"]
    :cmd/run-fn cli-providers-logout!}])
