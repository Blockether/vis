(ns com.blockether.vis.ext.provider-openai-codex.limits
  "OpenAI Codex dynamic quota checker.

   Fetches `https://chatgpt.com/backend-api/wham/usage`, selects the
   regular Codex bucket (or the nested Codex Spark bucket), then exposes
   the 5h and 7d percentage windows as normalized Vis limit rows."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.string :as str]))

(def ^:private usage-url "https://chatgpt.com/backend-api/wham/usage")

(def ^:private spark-limit-name "GPT-5.3-Codex-Spark")

(def ^:private spark-model-id "gpt-5.3-codex-spark")

(def ^:private usage-window-specs
  [{:bucket-key :primary_window :id :codex-5h :label "Codex 5h quota (%)" :unit :hour :size 5}
   {:bucket-key :secondary_window :id :codex-7d :label "Codex 7d quota (%)" :unit :day :size 7}])

(defn- object-map [value] (when (and (map? value) (not (record? value))) value))

(defn- field
  [m k]
  (when-let [m* (object-map m)]
    (cond (contains? m* k) (get m* k)
          (contains? m* (name k)) (get m* (name k)))))

(defn- clamp-percent
  [value]
  (-> (double value)
      (max 0.0)
      (min 100.0)))

(defn- used->left-percent
  [used-percent]
  (when (number? used-percent) (clamp-percent (- 100.0 (double used-percent)))))

(defn- model-id
  [model-ref]
  (cond (keyword? model-ref) (name model-ref)
        (string? model-ref) model-ref
        (map? model-ref) (or (field model-ref :id)
                             (field model-ref :name)
                             (some-> (field model-ref :model)
                                     model-id))
        :else nil))

(defn- spark-model?
  [model-ref]
  (= spark-model-id
     (some-> (model-id model-ref)
             str/lower-case)))

(defn- normalize-rate-limit-bucket [value] (object-map value))

(defn- spark-rate-limit-entry
  [value]
  (let [entry (object-map value)]
    (when (= spark-limit-name
             (some-> (field entry :limit_name)
                     str/trim))
      (normalize-rate-limit-bucket (field entry :rate_limit)))))

(defn- spark-rate-limit-bucket
  [usage]
  (let [additional (field usage :additional_rate_limits)]
    (or (when (sequential? additional) (some spark-rate-limit-entry additional))
        (when-let [additional-map (object-map additional)]
          (some spark-rate-limit-entry (vals additional-map))))))

(defn- select-rate-limit-bucket
  [usage model-ref]
  (if (spark-model? model-ref)
    (spark-rate-limit-bucket usage)
    (normalize-rate-limit-bucket (field usage :rate_limit))))

(defn- epoch-ms
  [value]
  (when (number? value)
    (long (if (> (double value) 100000000000.0) value (* 1000.0 (double value))))))

(defn- reset-at-ms
  [window now-ms]
  (or (epoch-ms (field window :reset_at))
      (when-let [seconds (field window :reset_after_seconds)]
        (when (number? seconds) (+ (long now-ms) (long (* 1000.0 (double seconds))))))))

(defn- window-row
  [now-ms {:keys [bucket-key id label unit size]} bucket]
  (when-let [window (object-map (field bucket bucket-key))]
    (let [used-percent (field window :used_percent)
          left-percent (used->left-percent used-percent)
          reset-ms (reset-at-ms window now-ms)]

      (cond-> {:id id
               :label label
               :scope :account
               :kind :rate
               :precision :exact
               :source :provider-api
               :unlimited? false
               :window (cond-> {:kind :rolling :unit unit :size size}
                         reset-ms
                         (assoc :resets-at-ms reset-ms))}
        (number? used-percent)
        (assoc :used
          (clamp-percent used-percent) :limit
          100.0)

        (number? left-percent)
        (assoc :remaining left-percent)))))

(defn usage->dynamic-limits
  "Convert ChatGPT/Codex `/wham/usage` JSON into Vis dynamic limit rows.

   `model-ref` may be a model id string/keyword or a map with `:id` /
   `:name`. It is used only for Codex Spark, whose bucket is nested in
   `additional_rate_limits`, matching Codex/ChatGPT's usage payload."
  ([usage] (usage->dynamic-limits usage nil))
  ([usage model-ref] (usage->dynamic-limits usage model-ref (System/currentTimeMillis)))
  ([usage model-ref now-ms]
   (let [bucket
         (select-rate-limit-bucket usage model-ref)

         rows
         (if bucket (into [] (keep #(window-row now-ms % bucket)) usage-window-specs) [])

         limited?
         (or (true? (field bucket :limit_reached)) (false? (field bucket :allowed)))]

     (cond-> {:limits rows}
       (and bucket (empty? rows))
       (assoc :note "OpenAI Codex usage endpoint did not return quota windows.")

       (nil? bucket)
       (assoc :note "OpenAI Codex usage endpoint did not return a matching quota bucket.")

       limited?
       (assoc :note "OpenAI Codex reports that the selected quota bucket is currently limited.")))))

(defn fetch-usage!
  "Fetch raw ChatGPT/Codex usage JSON from
   `https://chatgpt.com/backend-api/wham/usage`."
  [access-token account-id]
  (let [response
        (http/get usage-url
                  {:headers {"Accept" "*/*"
                             "Authorization" (str "Bearer " access-token)
                             "chatgpt-account-id" account-id}
                   :timeout 30000
                   :throw false})

        status
        (:status response)

        body
        (:body response)]

    (if (<= 200 status 299)
      (json/read-json body :key-fn keyword)
      (throw
        (ex-info
          (str "OpenAI Codex usage request failed: HTTP " status)
          {:type :provider/openai-codex-usage-error :status status :body body :url usage-url})))))

(defn dynamic-limits!
  "Fetch and normalize OpenAI Codex dynamic quota data for an access
   token/account id pair. Optional `model-ref` selects the Codex Spark
   nested bucket when applicable."
  ([access-token account-id] (dynamic-limits! access-token account-id nil))
  ([access-token account-id model-ref]
   (usage->dynamic-limits (fetch-usage! access-token account-id) model-ref)))
