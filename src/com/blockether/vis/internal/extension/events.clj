(ns com.blockether.vis.internal.extension.events
  "Publish and subscribe for extensions, in the gateway process (`vis.events`).

   Each extension has its own topics: a subscription gets only the events that its
   own extension publishes, from any worker. Each subscription has one agent, so it
   gets its events in publish order, and a slow subscriber never blocks a publisher.
   Events live only in memory: a subscription gets only the events that come after
   it. The `extension_event*` definitions of `gateway.json` give the bounds."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (java.nio.charset StandardCharsets)))

(defonce ^:private subscriptions
  ;; Subscription id -> {:extension :context :pattern :deliver :agent :waiting}.
  (atom {}))

(defn- words [s] (seq (str/split s #"\." -1)))

(defn matches?
  "True when `topic` is one of the topics of subscription pattern `pattern`. `*` takes
   the place of one word, and a last word `#` takes the place of zero or more words."
  [pattern topic]
  (loop [ps (words pattern)
         ts (words topic)]
    (let [p (first ps)]
      (cond (= "#" p) true
            (nil? ps) (nil? ts)
            (nil? ts) false
            (or (= "*" p) (= p (first ts))) (recur (next ps) (next ts))
            :else false))))

(defn- checked
  "`text` when it is a valid `kind` of the contract, else throw with `rule`."
  [kind text regex rule]
  (let [{:keys [name-max-chars]} gateway-contract/extension-event-limits]
    (when-not (and (string? text)
                   (<= (count text) (long name-max-chars))
                   (re-matches regex text))
      (throw (ex-info (str "vis.events: " kind " " (pr-str text) " is not valid: " rule)
                      {:error :extension-event-name :kind kind})))
    text))

(defn- topic
  [text]
  (checked "topic"
           text
           (:topic gateway-contract/extension-event-limits)
           "use words of a-z, 0-9, '_' and '-', joined with '.'"))

(defn subscribe!
  "Call `deliver` with each later event of extension `extension` whose topic matches
   `pattern`. `context` is the extension namespace that owns the subscription.
   Answer the subscription id."
  [extension context pattern deliver]
  (checked "pattern"
           pattern
           (:pattern gateway-contract/extension-event-limits)
           "use a topic in which '*' is one word and a last '#' is the other words")
  (let [id (str (random-uuid))]
    (swap! subscriptions assoc
      id
      {:extension extension
       :context context
       :pattern pattern
       :deliver deliver
       :agent (agent nil)
       :waiting (atom 0)})
    id))

(defn unsubscribe!
  "End subscription `id` of extension `extension`. A running delivery ends, and no
   other starts. Answer true when the subscription was open."
  [extension id]
  (let [[before _]
        (swap-vals! subscriptions
                    #(if (= extension (get-in % [id :extension])) (dissoc % id) %))]
    (and (contains? before id) (= extension (get-in before [id :extension])))))

(defn forget-context!
  "End each subscription of extension namespace `context`, because its Python is gone."
  [context]
  (swap! subscriptions #(into {} (remove (fn [[_ row]] (= context (:context row)))) %))
  nil)

(defn- deliver!
  [id {:keys [extension deliver waiting] :as row} event]
  (if (> (long (swap! waiting inc)) (long (:backlog gateway-contract/extension-event-limits)))
    (do (swap! waiting dec)
        (tel/log! {:level :warn
                   :id ::backlog-full
                   :data {:extension extension :topic (get event "topic") :subscription id}
                   :msg "An extension event subscriber is too far behind; it does not get this event"}))
    (send-off (:agent row)
              (fn [_]
                (try (when (contains? @subscriptions id) (deliver event))
                     (catch Throwable t
                       (tel/log! {:level :warn
                                  :id ::subscriber-failed
                                  :data {:extension extension
                                         :topic (get event "topic")
                                         :error (ex-message t)}
                                  :msg "An extension event subscriber failed"}))
                     (finally (swap! waiting dec)))
                nil))))

(defn publish!
  "Give `data-json`, the JSON text of the data, under `topic-name` to each matching
   subscription of extension `extension`. Answer how many subscriptions get it."
  [extension topic-name data-json]
  (let [topic-name
        (topic topic-name)

        text
        (str data-json)

        limit
        (long (:data-max-bytes gateway-contract/extension-event-limits))]

    (when (> (alength (.getBytes text StandardCharsets/UTF_8)) limit)
      (throw (ex-info (str "vis.events: the data of an event is larger than " limit " bytes of JSON")
                      {:error :extension-event-data :max limit})))
    (let [event
          {"topic" topic-name
           "data" (json/read-json text :key-fn identity)
           "published_at_ms" (util/now-ms)}

          targets
          (filterv (fn [[_ row]]
                     (and (= extension (:extension row)) (matches? (:pattern row) topic-name)))
                   @subscriptions)]

      (doseq [[id row] targets] (deliver! id row event))
      (count targets))))
