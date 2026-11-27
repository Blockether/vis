(ns com.blockether.vis.internal.gateway.server.transport.sse
  "Concrete SSE framing for the gateway's Ring server transport."
  (:require [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire])
  (:import [java.nio.charset StandardCharsets]))

(def max-frame-bytes (* 1024 1024))

(defn- frame-bytes
  ^bytes [seq event-name body]
  (.getBytes (str (when (some? seq) (str "id: " seq "\n"))
                  "event: " event-name "\ndata: " body "\n\n")
             StandardCharsets/UTF_8))

(defn- session-frame-bytes
  ^bytes [event]
  (frame-bytes (when-not (= "fleet" (get event "scope"))
                 (or (get event "seq") (get event "cursor")))
               (get event "type")
               (wire/canonical-json-str event)))

(defn encode-frame
  "Encode each body once. Return the bytes to write and whether the body was elided."
  [event]
  (let [frame (session-frame-bytes event)]
    (if (<= (alength frame) (long max-frame-bytes))
      {:bytes frame :elided? (= "event.elided" (get event "type"))}
      (let [marker (if (= "event.elided" (get event "type"))
                     event
                     (gateway-contract/elided-event event "frame_size"))
            fallback (session-frame-bytes marker)
            bounded (if (<= (alength fallback) max-frame-bytes)
                      fallback
                      (session-frame-bytes (dissoc marker "view_id" "turn_id")))]
        (when (> (alength bounded) max-frame-bytes)
          (throw (ex-info "SSE repair marker has invalid routing fields" {})))
        {:bytes bounded :elided? true}))))

(defn sse-frame
  "Render the canonical frame as text. Socket writers use [[encode-frame]]."
  ^String [event]
  (String. ^bytes (:bytes (encode-frame event)) StandardCharsets/UTF_8))

(defn encode-job-frame
  "Render current job state without a replay cursor. A large job is read by ID."
  ^bytes [^String event-name job]
  (let [job (wire/->wire job)
        frame (frame-bytes nil event-name (wire/canonical-json-str job))]
    (if (<= (alength frame) max-frame-bytes)
      frame
      (let [marker (assoc (select-keys job ["id"]) "is_elided" true)
            fallback (frame-bytes nil event-name (wire/canonical-json-str marker))]
        (if (<= (alength fallback) max-frame-bytes)
          fallback
          ;; The subscription already addresses the one job to read.
          (frame-bytes nil event-name "{\"is_elided\":true}"))))))

(defn job-sse-frame
  "Render the job frame as text. Socket writers use [[encode-job-frame]]."
  ^String [^String event-name job]
  (String. (encode-job-frame event-name job) StandardCharsets/UTF_8))

(def HEARTBEAT_MS 15000)

(def sse-headers
  "Response headers of EVERY SSE endpoint. `no-transform` + `X-Accel-Buffering`
   are the point: intermediaries (Cloudflare tunnels, nginx) BUFFER a streaming
   body unless told not to, and a buffered stream delivers nothing until
   disconnect — which reads as \"streaming dead until refresh\" in any proxied
   client."
  {"Content-Type" "text/event-stream"
   "Cache-Control" "no-cache, no-transform"
   "X-Accel-Buffering" "no"})
