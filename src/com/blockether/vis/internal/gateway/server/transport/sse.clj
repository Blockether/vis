(ns com.blockether.vis.internal.gateway.server.transport.sse
  "Concrete SSE framing for the gateway's Ring server transport."
  (:require [com.blockether.vis.contract.wire :as wire]))

(defn sse-frame
  "Render one canonical session event. Its sequence is the reconnect cursor."
  ^String [event]
  (str "id: "
       (get event "seq")
       "\n"
       "event: "
       (get event "type")
       "\n"
       "data: "
       (wire/json-str event)
       "\n\n"))

(defn job-sse-frame
  "Render current job state without a replay cursor."
  ^String [^String event-name job]
  (str "event: " event-name "\ndata: " (wire/json-str job) "\n\n"))
