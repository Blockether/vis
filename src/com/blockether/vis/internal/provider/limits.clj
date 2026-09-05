(ns com.blockether.vis.internal.provider-limits
  "Normalized provider limits surface.

   Providers may optionally expose `:provider/limits-fn` in the global
   registry. The function returns provider-specific limit/quota data;
   this namespace wraps it in one validated envelope and augments it
   with static provider metadata from svar's catalog (currently RPM /
   TPM).

   The limits vocabulary and the report shape belong to
   `com.blockether.vis.contract.provider`; what stays here is fetching, caching and
   normalizing whatever a provider answered.

   Goals:
   - one host-level shape for all providers,
   - explicit support for providers that only know static limits,
   - contract validation of every returned report,
   - graceful error envelopes instead of exploding the caller when a
     provider-specific implementation is absent or malformed."
  (:require [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.vis.contract.provider :as contract-provider]
            [com.blockether.vis.internal.registry :as registry]
            [com.blockether.vis.internal.util :as util]))

(defn- static-limits
  [provider-id]
  (let [known (get svar-router/KNOWN_PROVIDERS provider-id)]
    (cond-> {}
      (some? (:rpm known))
      (assoc :rpm (long (:rpm known)))

      (some? (:tpm known))
      (assoc :tpm (long (:tpm known))))))

(defn- base-report
  ([provider-id status] (base-report provider-id status nil))
  ([provider-id status note]
   {:provider-id provider-id
    :status status
    :fetched-at-ms (util/now-ms)
    :static (static-limits provider-id)
    :dynamic (cond-> {:limits []}
               note
               (assoc :note note))}))

(defn- error-report
  ([provider-id type message] (error-report provider-id type message nil))
  ([provider-id type message data]
   (assoc (base-report provider-id :error)
     :error (cond-> {:type type :message message}
              data
              (assoc :data data)))))

(defn- rejection-status
  "The explicit upstream auth status carried by a provider exception, if any."
  [^Throwable t]
  (let [data (ex-data t)]
    (or (:status data)
        (:code data)
        (get data "status")
        (get data "code")
        (get-in data [:response :status])
        (get-in data ["response" "status"]))))

(defn- authentication-rejection?
  [^Throwable t]
  (contains? #{401 403 "401" "403"} (rejection-status t)))

(defn- unauthenticated-report
  [provider-id ^Throwable t]
  (assoc (base-report provider-id :unauthenticated "Provider rejected the current credentials.")
    :error {:type :provider/limits-unauthenticated
            :message (or (ex-message t) "Provider rejected the current credentials.")
            :data (cond-> {:class (.getName (class t))}
                    (rejection-status t)
                    (assoc :status (rejection-status t)))}))

(defn- merge-report
  [base raw]
  (cond-> (-> base
              (merge (dissoc raw :static :dynamic :error))
              (update :static merge (:static raw))
              (update :dynamic merge (:dynamic raw)))
    (:error raw)
    (assoc :error (:error raw))))

(defn- invalid-report
  [provider-id raw]
  (error-report provider-id
                :provider/invalid-limits-report
                "Provider limits fn returned an invalid report"
                {:report raw :explain (contract-provider/explain-report raw)}))

(def ^:private default-cache-ms
  "How long one provider's live limits report is reused when the provider does
   not declare a budget of its own.

   `:provider/limits-fn` is an UPSTREAM HTTP call (the provider's usage
   endpoint). Every channel polls limits independently — the TUI footer thread,
   the companion's router dialog, `/v1/router` (which fans out to EVERY
   registered provider in one request) — so without a cache N clients times M
   providers hit the provider's API on every glance. 15s is short enough that a
   quota that just moved shows up promptly and long enough that a burst of
   clients costs ONE upstream call. A provider whose usage endpoint meters its
   own callers asks for a longer budget with `:provider/limits-cache-ms`."
  15000)

(def ^:private throttled-cache-ms
  "How long an answer is reused after the usage endpoint REFUSED the check.
   Asking a metered endpoint again every few seconds is what earned the refusal,
   so a refusal is remembered far longer than a quota — and the last good report
   stands in for it while it lasts."
  (* 10 60 1000))

(def ^:private throttled-statuses
  "Upstream statuses that mean \"ask later\", not \"this is your quota\"."
  #{408 409 425 429 500 502 503 504})

(defonce ^:private limits-cache (atom {}))

(defn flush-limits-cache!
  "Drop cached limits reports so the next read hits the provider again.

   Called after auth changes (sign-in / sign-out): the cached report for a
   provider that just re-authenticated would otherwise keep saying
   `:unauthenticated` until its budget ran out.

   This is the ONE limits cache. A `:provider/limits-fn` that kept a cache of
   its own would outlive this flush and keep painting the pre-login verdict at a
   credential that already works, so a limits fn FETCHES and nothing else."
  ([] (reset! limits-cache {}))
  ([provider-id] (swap! limits-cache dissoc provider-id)))

(defn- throttled-report?
  "True when the provider answered \"ask later\" instead of a quota."
  [report]
  (and (= :error (:status report))
       (contains? throttled-statuses (get-in report [:error :data :status]))))

(defn- backing-off-note
  [status]
  (str "Showing the last known usage; the provider's usage endpoint returned HTTP "
       status
       ", so Vis is backing off."))

(defn- settled
  "What one freshly fetched `report` BECOMES in the cache: the value served, when
   it expires, and the last good report kept to stand in for a throttled one.

   The provider's budget buys reuse of a GOOD answer. A refusal backs off far
   longer, and anything else — an error, a credential the provider rejected — is
   re-asked at the default, so a verdict a sign-in already invalidated cannot
   linger."
  [report cache-ms stale-ok]
  (let [throttled?
        (throttled-report? report)

        served
        (if (and throttled? stale-ok)
          (assoc-in stale-ok
            [:dynamic :note]
            (backing-off-note (get-in report [:error :data :status])))
          report)]

    {:report served
     :expires-at-ms (+ (util/now-ms)
                       (long (cond throttled? throttled-cache-ms
                                   (= :ok (:status report)) cache-ms
                                   :else default-cache-ms)))
     :stale-ok (if (= :ok (:status report)) report stale-ok)}))

(defn- servable?
  "True while `entry` may still be served: a fetch still IN FLIGHT (the second
   reader waits for the first instead of starting its own) or a finished one
   inside its budget. Only a REALIZED delay is dereferenced, so this stays pure
   enough for `swap!` to retry it."
  [entry now-ms]
  (boolean (when-let [d (:value entry)]
             (or (not (realized? d)) (< (long now-ms) (long (:expires-at-ms @d)))))))

(defn- last-good-report
  [entry]
  (when-let [d (:value entry)]
    (when (realized? d) (:stale-ok @d))))

(defn- cached-report
  "Run `fetch` at most once per `cache-ms` per provider.

   The cache holds a `delay`, so concurrent callers that miss together share ONE
   in-flight upstream call instead of stampeding it (single flight)."
  [provider-id cache-ms fetch]
  (let [now
        (util/now-ms)

        entry
        (-> (swap! limits-cache
              (fn [m]
                (let [e (get m provider-id)]
                  (if (servable? e now)
                    m
                    (assoc m
                      provider-id {:value (delay
                                            (settled (fetch) cache-ms (last-good-report e)))})))))
            (get provider-id))]

    (:report @(:value entry))))

(defn provider-limits
  "Return a normalized, contract-validated limits report for one provider id.

   The provider's optional `:provider/limits-fn` supplies the dynamic
   portion. This host wrapper backfills static svar metadata and always
   returns a valid `contract-provider/report` envelope, even when the
   provider-specific implementation is absent, missing, throws, or returns
   malformed data.

   Providers that only have static svar catalog metadata still return a
   usable `:ok` report so callers can surface RPM / TPM without needing a
   registered runtime extension."
  [provider-id]
  (let [provider
        (registry/provider-by-id provider-id)

        static-report
        (base-report provider-id :ok)

        has-static?
        (seq (:static static-report))]

    (cond (and provider (:provider/limits-fn provider))
          (cached-report provider-id
                         (or (:provider/limits-cache-ms provider) default-cache-ms)
                         (fn []
                           (try (let [report (merge-report static-report
                                                           (or ((:provider/limits-fn provider))
                                                               {}))]
                                  (if (contract-provider/report-valid? report)
                                    report
                                    (invalid-report provider-id report)))
                                (catch Throwable t
                                  (if (authentication-rejection? t)
                                    (unauthenticated-report provider-id t)
                                    (error-report provider-id
                                                  :provider/limits-error
                                                  (or (ex-message t) (.getName (class t)))
                                                  {:class (.getName (class t))}))))))
          has-static? (base-report
                        provider-id
                        :ok
                        (if provider
                          "Provider exposes static catalog limits only."
                          "Provider is not registered; showing static catalog limits only."))
          provider (base-report provider-id :unsupported "Provider does not expose limit metadata.")
          :else (base-report provider-id :unknown-provider "Provider is not registered."))))

(defn all-provider-limits
  "Return normalized limits reports for every registered provider in
   registration order."
  []
  (->> (registry/registered-providers)
       (mapv (comp provider-limits :provider/id))))
