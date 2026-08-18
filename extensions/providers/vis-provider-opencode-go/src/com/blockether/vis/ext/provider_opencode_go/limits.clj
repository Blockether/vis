(ns com.blockether.vis.ext.provider-opencode-go.limits
  "OpenCode Go dynamic quota checker.

   OpenCode Go is a subscription, but it is NOT unmetered: the plan meters three
   dollar budgets — a 5h rolling window, a calendar week and the billing month.
   Upstream added `GET https://opencode.ai/zen/go/v1/usage` on 2026-08-11
   (`sst/opencode` #16513) and that endpoint is the only place those counters are
   published.

   Request:  `Authorization: Bearer <api-key>` — the endpoint reads the Bearer
             header only; an `x-api-key` header is answered as a missing key.
   200:      `{\"usage\": {\"rolling\"|\"weekly\"|\"monthly\":
             {\"status\", \"percent\", \"resetsAt\"}}}` where `status` is
             `ok` or `rate-limited`, `percent` is an integer 0-100 of that
             window's budget and `resetsAt` is an ISO-8601 instant.
   401:      key missing or unknown.
   403:      valid key, no OpenCode Go subscription behind it.

   The dollar limits themselves ($12 / $30 / $60 at the time of writing) are NOT
   on the wire, so each window is normalized as a percentage tank
   (`:limit 100.0`) — the shape `limits-format/percentage-limit-row?` renders as
   \"63% left\"."
  (:require [babashka.http-client :as http]
            [charred.api :as json]
            [clojure.string :as str])
  (:import [java.time Instant]
           [java.time.format DateTimeParseException]))

(def ^:private usage-url "https://opencode.ai/zen/go/v1/usage")

(def ^:private WINDOW_SPECS
  "The three windows `/usage` meters, shortest first.

   `:json-key` is the wire name. The `-5h` / `-7d` / `-30d` id suffix is the
   spelling `limits-format/limit-window-ms` parses, so the rows still order
   correctly after a gateway hop that strips the window map down to an id."
  [{:json-key :rolling
    :id :opencode-go-5h
    :label "OpenCode Go 5h quota (%)"
    :window {:kind :rolling :unit :hour :size 5}}
   {:json-key :weekly
    :id :opencode-go-7d
    :label "OpenCode Go 7d quota (%)"
    :window {:kind :calendar :unit :week :size 1}}
   {:json-key :monthly
    :id :opencode-go-30d
    :label "OpenCode Go 30d quota (%)"
    :window {:kind :calendar :unit :month :size 1}}])

(defn- clamp-percent
  [value]
  (-> (double value)
      (max 0.0)
      (min 100.0)))

(defn- resets-at-ms
  "Epoch millis for the window's ISO-8601 `resetsAt`, or nil when the provider
   omitted it or sent something unparseable."
  [value]
  (when (string? value)
    (try (.toEpochMilli (Instant/parse ^String value)) (catch DateTimeParseException _ nil))))

(defn- rate-limited?
  [window]
  (= "rate-limited"
     (some-> (:status window)
             str/trim
             str/lower-case)))

(defn- window-row
  [{:keys [id label window]} data]
  (let [percent
        (:percent data)

        used
        (when (number? percent) (clamp-percent percent))

        reset-ms
        (resets-at-ms (or (:resetsAt data) (:resets_at data) (:resets-at data)))]

    (cond-> {:id id
             :label label
             :scope :account
             :kind :rate
             :precision (if (number? percent) :exact :unknown)
             :source :provider-api
             :is-unlimited false
             :window (cond-> window
                       reset-ms
                       (assoc :resets-at-ms reset-ms))}
      (number? used)
      (assoc :used
        used :limit
        100.0 :remaining
        (- 100.0 (double used)))

      (rate-limited? data)
      (assoc :note
        "This OpenCode Go window is exhausted; requests fall back to the Zen balance or are refused."))))

(defn usage->dynamic-limits
  "Convert an OpenCode Go `/usage` payload into Vis dynamic limit rows.

   Windows the payload omits are dropped rather than padded: the endpoint always
   reports all three, so a missing one means the shape changed and a row with no
   numbers would claim knowledge Vis does not have."
  [payload]
  (let [usage
        (:usage payload)

        rows
        (into []
              (keep (fn [{:keys [json-key] :as spec}]
                      (when-let [data (get usage json-key)]
                        (window-row spec data))))
              WINDOW_SPECS)

        exhausted
        (filterv #(rate-limited? (get usage (:json-key %))) WINDOW_SPECS)]

    (cond-> {:limits rows}
      (empty? rows)
      (assoc :note "OpenCode Go usage endpoint did not return quota windows.")

      (seq exhausted)
      (assoc :note
        (str "OpenCode Go has exhausted its "
             (str/join ", " (map (comp name :json-key) exhausted))
             " budget; add Zen balance or wait for the reset.")))))

(defn fetch-usage!
  "Fetch raw OpenCode Go usage JSON. Throws `ex-info` carrying `:status` for any
   non-2xx so the caller can tell a rejected key (401) from a key without a Go
   subscription (403)."
  [api-key]
  (let [response
        (http/get usage-url
                  {:headers {"Accept" "application/json" "Authorization" (str "Bearer " api-key)}
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
          (str "OpenCode Go usage request failed: HTTP " status)
          {:type :provider/opencode-go-usage-error :status status :body body :url usage-url})))))

(defn dynamic-limits!
  "Fetch and normalize the live OpenCode Go quota windows for an API key."
  [api-key]
  (usage->dynamic-limits (fetch-usage! api-key)))
