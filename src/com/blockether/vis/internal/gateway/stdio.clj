(ns com.blockether.vis.internal.gateway.stdio
  "Gateway-free, serial NDJSON transport for the Python SDK. Uses the same SDK
   handlers and protocol as HTTP; never starts Jetty or performs discovery.
   The owning process must select its own database before entering this loop."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [ring.core.protocols :as body]
            [com.blockether.vis.internal.gateway.view :as view])
  (:import [java.io Reader Writer ByteArrayInputStream ByteArrayOutputStream]
           [java.util Base64]))

(defn- sdk-operation
  [method uri]
  (some (fn [{:keys [path audience operations]}]
          (when (and (= :sdk audience)
                     (re-matches (re-pattern (str/join "/"
                                                       (map #(if (str/starts-with? % ":")
                                                               "[^/]+"
                                                               (java.util.regex.Pattern/quote %))
                                                            (str/split path #"/"))))
                                 uri))
            (get operations method)))
        contract/route-table))

(defn- read-frame
  [^Reader reader]
  (let [text (StringBuilder.)]
    (loop []

      (let [c (.read reader)]
        (cond (= c -1) (when (pos? (.length text)) (str text))
              (= c 10) (str text)
              (>= (.length text) 67108864) (throw (ex-info "stdio frame too large" {}))
              :else (do (.append text (char c)) (recur)))))))

(defn- reply!
  [^Writer writer value]
  (.write writer (str (wire/json-str value) "\n"))
  (.flush writer))

(defn- dispatch
  [handler frame]
  (try
    (let [method
          (some-> (get frame "method")
                  str/lower-case
                  keyword)

          uri
          (get frame "route")

          operation
          (when (string? uri) (sdk-operation method uri))]

      (if (or (nil? operation) (= :sse (:response operation)))
        {:status 400 :headers {} :content ""}
        (let [bytes
              (if-let [encoded (get frame "content")]
                (.decode (Base64/getDecoder) ^String encoded)
                (.getBytes (if (contains? frame "body") (wire/json-str (get frame "body")) "")
                           java.nio.charset.StandardCharsets/UTF_8))

              request
              {:request-method method
               :uri uri
               :query-string (get frame "query")
               :headers {"x-vis-protocol" (str contract/protocol-version)
                         "x-vis-min-gateway-protocol" (str contract/minimum-gateway-protocol)
                         "content-type"
                         (if (contains? frame "body") "application/json" "application/octet-stream")
                         "content-length" (str (alength bytes))}
               :body (ByteArrayInputStream. bytes)}

              response
              (handler request)]

          (with-open [out (ByteArrayOutputStream.)]
            (body/write-body-to-stream (:body response) response out)
            {:status (:status response)
             :headers (:headers response)
             :content (.encodeToString (Base64/getEncoder) (.toByteArray out))}))))
    (catch Exception _ {:status 500 :headers {} :content ""})))

(defn serve!
  "Serve until stdin EOF. One response per request; results contain base64 bytes.
   Own the same View-to-session bridge as HTTP, so input/live Views are delivered
   to polling SDK clients. A timeout kills the owning process rather than reusing
   an ambiguously aligned pipe. Leaving this loop flushes and removes the bridge."
  [^Reader reader ^Writer writer handler]
  (view/install!)
  (try (reply! writer {:protocol contract/protocol-version})
       (loop []

         (when-let [line (read-frame reader)]
           (reply! writer (dispatch handler (wire/parse-json line)))
           (recur)))
       (finally (view/uninstall!))))
