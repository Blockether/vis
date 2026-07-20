(ns com.blockether.vis.internal.provider-limits
  "Normalized provider limits surface.

   Providers may optionally expose `:provider/limits-fn` in the global
   registry. The function returns provider-specific limit/quota data;
   this namespace wraps it in one validated envelope and augments it
   with static provider metadata from svar's catalog (currently RPM /
   TPM).

   Goals:
   - one host-level shape for all providers,
   - explicit support for providers that only know static limits,
   - spec validation of every returned report,
   - graceful error envelopes instead of exploding the caller when a
     provider-specific implementation is absent or malformed."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.svar.internal.router :as svar-router]
            [com.blockether.vis.internal.registry :as registry]))

(def ^:private limit-statuses #{:ok :unauthenticated :unsupported :error :unknown-provider})

(def ^:private limit-scopes #{:account :plan :workspace :model})

(def ^:private limit-kinds #{:requests :tokens :usd :credits :sessions :rate})

(def ^:private limit-window-kinds #{:calendar :rolling :lifetime})

(def ^:private limit-window-units #{:minute :hour :day :week :month :year})

(def ^:private limit-precisions #{:exact :estimate :derived :unknown})

(def ^:private limit-sources #{:provider-api :derived :static :local})

(s/def ::provider-id keyword?)
(s/def ::status limit-statuses)
(s/def ::fetched-at-ms integer?)
(s/def ::rpm nat-int?)
(s/def ::tpm nat-int?)
(s/def ::static (s/keys :opt-un [::rpm ::tpm]))

(s/def ::id keyword?)
(s/def ::label (s/and string? #(not (str/blank? %))))
(s/def ::scope limit-scopes)
(s/def ::subject map?)
(s/def ::kind limit-kinds)
(s/def ::precision limit-precisions)
(s/def ::source limit-sources)
(s/def ::used number?)
(s/def ::limit number?)
(s/def ::remaining number?)
(s/def ::unlimited? boolean?)
(s/def ::note string?)

(s/def ::window
  (s/and map?
         #(contains? limit-window-kinds (:kind %))
         #(or (nil? (:unit %)) (contains? limit-window-units (:unit %)))
         #(or (nil? (:size %)) (pos-int? (:size %)))
         #(or (nil? (:resets-at-ms %)) (integer? (:resets-at-ms %)))))

(s/def ::limit-row
  (s/keys :req-un [::id ::label ::scope ::kind ::precision ::source ::unlimited?]
          :opt-un [::subject ::window ::used ::limit ::remaining ::note]))

(s/def ::limits (s/coll-of ::limit-row :kind vector?))
(s/def ::dynamic (s/keys :req-un [::limits] :opt-un [::note]))

(s/def ::type keyword?)
(s/def ::message (s/and string? #(not (str/blank? %))))
(s/def ::data map?)
(s/def ::error (s/keys :req-un [::type ::message] :opt-un [::data]))

(s/def ::report
  (s/keys :req-un [::provider-id ::status ::fetched-at-ms ::static ::dynamic] :opt-un [::error]))

(s/def ::reports (s/coll-of ::report :kind vector?))

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
    :fetched-at-ms (System/currentTimeMillis)
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

(defn- merge-report
  [base raw]
  (cond->
    (-> base
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
                {:report raw :explain (s/explain-data ::report raw)}))

(defn provider-limits
  "Return a normalized, spec-validated limits report for one provider id.

   The provider's optional `:provider/limits-fn` supplies the dynamic
   portion. This host wrapper backfills static svar metadata and always
   returns a valid `::report` envelope, even when the provider-specific
   implementation is absent, missing, throws, or returns malformed data.

   Providers that only have static svar catalog metadata still return a
   usable `:ok` report so callers can surface RPM / TPM without needing a
   registered runtime extension."
  [provider-id]
  (let
    [provider
     (registry/provider-by-id provider-id)

     static-report
     (base-report provider-id :ok)

     has-static?
     (seq (:static static-report))]

    (cond (and provider (:provider/limits-fn provider))
          (try (let [report (merge-report static-report (or ((:provider/limits-fn provider)) {}))]
                 (if (s/valid? ::report report) report (invalid-report provider-id report)))
               (catch Throwable t
                 (error-report provider-id
                               :provider/limits-error
                               (or (ex-message t) (.getName (class t)))
                               {:class (.getName (class t))})))
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
