(ns com.blockether.vis.internal.bus
  "The signal bus of the gateway process: `vis.bus` for extensions, and the signals
   of Vis itself.

   A signal has a topic, JSON data, a scope and a publish time. Each extension has
   its own topics: a subscription of an extension gets only the signals of that
   extension. A topic whose first word is `vis` is a signal of Vis. Only the gateway
   publishes one, and only a pattern whose first word is `vis` gets it.

   Scope `global` sends a signal to each matching subscription. Scope `session`
   sends it only to the subscriptions of the session of the publisher. A
   subscription with a scope gets only the signals of that scope, and a
   subscription without a scope gets both. Each subscription has one agent, so it
   gets its signals in publish order, and a slow subscriber never blocks a
   publisher. Signals live only in memory: a subscription gets only the signals
   that come after it. The `bus_*` definitions of `gateway.json` give the bounds."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.util :as util]
            [taoensso.telemere :as tel])
  (:import (java.nio.charset StandardCharsets)))

(defonce ^:private subscriptions
  ;; Subscription id -> {:owner :context :session :scope :pattern :deliver :agent
  ;; :waiting}. The owner is an extension name, or :vis for the gateway.
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

(defn- vis-word?
  "True when the first word of topic or pattern `text` is `vis`."
  [text]
  (= "vis" (first (str/split text #"\." 2))))

(defn- fail!
  [message data]
  (throw (ex-info (str "vis.bus: " message) data)))

(defn- checked
  "`text` when it is a valid `kind` of the contract, else throw with `rule`."
  [kind text regex max-chars rule]
  (when-not (and (string? text) (<= (count text) (long max-chars)) (re-matches regex text))
    (fail! (str kind " " (pr-str text) " is not valid: " rule) {:error :bus-name :kind kind}))
  text)

(defn- scope-for
  "`scope` when it is a scope of the contract that a caller in session `session` can
   use, else throw."
  [scope session]
  (when-not (contains? gateway-contract/bus-scopes scope)
    (fail! (str "scope " (pr-str scope) " is not valid: use 'global' or 'session'")
           {:error :bus-scope}))
  (when (and (= "session" scope) (nil? session))
    (fail! "scope 'session' needs a session, and this callback has no session"
           {:error :bus-scope}))
  scope)

(defn subscribe!
  "Call `deliver` with each later signal of `pattern` that the subscriber gets.
   `subscriber` is a map: `:owner` is an extension name, or :vis for the gateway.
   `:context` is the extension namespace that owns the subscription, `:session` the
   session of the subscriber or nil, and `:scope` is \"global\", \"session\" or nil
   for both. The two-argument form subscribes the gateway. Answer the subscription
   id."
  ([pattern deliver] (subscribe! {:owner :vis} pattern deliver))
  ([{:keys [owner context session scope]} pattern deliver]
   (checked "pattern"
            pattern
            (:pattern gateway-contract/bus-limits)
            (:pattern-max-chars gateway-contract/bus-limits)
            "use a topic in which '*' is one word and a last '#' is the other words")
   (some-> scope
           (scope-for session))
   (let [id (str (random-uuid))]
     (swap! subscriptions assoc
       id
       {:owner owner
        :context context
        :session session
        :scope scope
        :pattern pattern
        :deliver deliver
        :agent (agent nil)
        :waiting (atom 0)})
     id)))

(defn unsubscribe!
  "End subscription `id` of owner `owner`. A running delivery ends, and no other
   starts. Answer true when the subscription was open."
  [owner id]
  (let [[before _]
        (swap-vals! subscriptions #(if (= owner (get-in % [id :owner])) (dissoc % id) %))]
    (and (contains? before id) (= owner (get-in before [id :owner])))))

(defn forget-context!
  "End each subscription of extension namespace `context`, because its Python is gone."
  [context]
  (swap! subscriptions #(into {} (remove (fn [[_ row]] (= context (:context row)))) %))
  nil)

(defn- hears?
  "True when subscription `row` gets the signal of `owner` under `topic`, with `scope`
   from session `session`."
  [row {:keys [owner topic scope session]}]
  (and (or (= :vis owner) (= owner (:owner row)))
       (= (vis-word? topic) (vis-word? (:pattern row)))
       (matches? (:pattern row) topic)
       (case scope
         "global" (not= "session" (:scope row))
         "session" (and (not= "global" (:scope row)) (= session (:session row))))))

(defn- deliver!
  [id {:keys [owner deliver waiting] :as row} signal]
  (if (> (long (swap! waiting inc)) (long (:backlog gateway-contract/bus-limits)))
    (do (swap! waiting dec)
        (tel/log! {:level :warn
                   :id ::backlog-full
                   :data {:owner owner :topic (get signal "topic") :subscription id}
                   :msg "A signal subscriber is too far behind; it does not get this signal"}))
    (send-off (:agent row)
              (fn [_]
                (try (when (contains? @subscriptions id) (deliver signal))
                     (catch Throwable t
                       (tel/log! {:level :warn
                                  :id ::subscriber-failed
                                  :data {:owner owner :topic (get signal "topic") :error (ex-message t)}
                                  :msg "A signal subscriber failed"}))
                     (finally (swap! waiting dec)))
                nil))))

(defn publish!
  "Give `data`, JSON data with string keys, under `topic-name` to each subscription
   that gets it. Answer how many subscriptions get it. `publisher` is a map: `:owner`
   is an extension name, or :vis for the gateway, and `:session` the session of the
   publisher or nil. `scope` is \"global\" or \"session\". The two-argument form
   publishes a global signal of Vis, which must be a `bus_vis_signal` of the contract."
  ([topic-name data] (publish! {:owner :vis} topic-name data "global"))
  ([{:keys [owner session]} topic-name data scope]
   (let [topic-name
         (checked "topic"
                  topic-name
                  (:topic gateway-contract/bus-limits)
                  (:topic-max-chars gateway-contract/bus-limits)
                  "use words of a-z, 0-9, '_' and '-', joined with '.'")

         scope
         (scope-for scope session)]

     (cond (and (not= :vis owner) (vis-word? topic-name))
           (fail! (str "only Vis publishes a topic that starts with 'vis.': " (pr-str topic-name))
                  {:error :bus-topic :topic topic-name})
           (and (= :vis owner)
                (not (document/valid? "gateway" "bus_vis_signal" {"topic" topic-name "data" data})))
           (fail! (str "a signal of Vis needs a topic and data of bus_vis_signal: " (pr-str topic-name))
                  {:error :bus-topic :topic topic-name}))
     (let [signal
           {"topic" topic-name "data" data "scope" scope "published_at_ms" (util/now-ms)}

           source
           {:owner owner :topic topic-name :scope scope :session session}

           targets
           (filterv (fn [[_ row]] (hears? row source)) @subscriptions)]

       (doseq [[id row] targets] (deliver! id row signal))
       (count targets)))))

(defn publish-json!
  "[[publish!]] for data given as JSON text, such as the data of an extension. The
   text must fit the size limit of the contract."
  [publisher topic-name data-json scope]
  (let [text
        (str data-json)

        limit
        (long (:data-max-bytes gateway-contract/bus-limits))]

    (when (> (alength (.getBytes text StandardCharsets/UTF_8)) limit)
      (fail! (str "the data of a signal is larger than " limit " bytes of JSON")
             {:error :bus-data :max limit}))
    (publish! publisher topic-name (json/read-json text :key-fn identity) scope)))
