(ns com.blockether.vis.internal.gateway.event-check
  "A test hook that checks the payload of each session and fleet event that the
   gateway publishes against the payload schema of its type (#487). Use
   [[checked]] in `around-each`."
  (:require [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.gateway.state :as state]))

(defonce ^{:doc "The event types whose payloads the hook checked."} checked-types (atom #{}))

(def ^:private envelope-keys
  "The keys of the event envelope. They are not part of a payload."
  ["schema" "seq" "ts" "session_id" "type" "_producer" "_pid" "_store"])

(defn check!
  "Throw when `payload` does not match the payload schema of event `type`. A type
   outside the vocabulary, such as a test event, is not checked."
  [type payload]
  (when-let [schema (get gateway-contract/session-event-payloads type)]
    (let [body (apply dissoc payload envelope-keys)]
      (when-let [errors (document/explain-json "gateway" schema body)]
        (throw (ex-info (str "The payload of event " type
                             " does not match the gateway schema " schema
                             ": " (pr-str (take 3 errors)))
                        {:type type :errors errors :payload body})))
      (swap! checked-types conj type)))
  payload)

(defn checked
  "Run `f` while each session event and each fleet frame is checked before it is
   published."
  [f]
  (let [stamp
        gateway-contract/stamp-session-event

        fleet
        @#'state/publish-fleet!]

    (with-redefs [gateway-contract/stamp-session-event
                  (fn [payload session-id sequence timestamp type]
                    (check! type payload)
                    (stamp payload session-id sequence timestamp type))

                  state/publish-fleet!
                  (fn [frames]
                    (doseq [frame frames]
                      (check! (get frame "type") frame))
                    (fleet frames))]

      (f))))
