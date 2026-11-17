(ns com.blockether.vis.internal.gateway.response-check
  "A test hook that checks each JSON answer of the gateway router against the
   response schema of its operation (#487). Use [[checked]] in `around-each`."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.contract.wire :as wire]
            [reitit.core :as reitit]
            [reitit.ring :as rr]
            [ring.core.protocols :as ring-protocols]))

(defonce ^{:doc "The `[method path]` operations whose answers the hook checked."} checked-operations
  (atom #{}))

(defn- json-answer?
  [response]
  (and (<= 200 (long (or (:status response) 0)) 299)
       (some-> (get-in response [:headers "Content-Type"])
               str/lower-case
               (str/includes? "application/json"))))

(defn- buffered
  "The `response` with its body read into a string, and that string."
  [response]
  (let [out (java.io.ByteArrayOutputStream.)]
    (ring-protocols/write-body-to-stream (:body response) response out)
    (let [text (.toString out "UTF-8")]
      [(assoc response :body text) text])))

(defn- check!
  "Throw when the JSON answer of `request` does not match its operation schema."
  [router request response]
  (let [path
        (some-> (reitit/match-by-path router (:uri request))
                :template)

        method
        (:request-method request)

        {:keys [response-schema] :as operation}
        (when path (gateway-contract/operation method path))]

    (if (and response-schema (= :json (:response operation)) (json-answer? response))
      (let [[response text]
            (buffered response)

            body
            (wire/parse-json text)

            errors
            (document/explain-json "gateway" (name response-schema) body)]

        (when errors
          (throw (ex-info (str "The answer of " (str/upper-case (name method))
                               " " path
                               " does not match the gateway schema " (name response-schema))
                          {:operation [method path] :errors errors :body body})))
        (swap! checked-operations conj [method path])
        response)
      response)))

(defn checked
  "Run `f` while each handler that `reitit.ring/ring-handler` makes checks its
   JSON answers against the gateway response schemas."
  [f]
  (let [ring-handler rr/ring-handler]
    (with-redefs [rr/ring-handler (fn [router & more]
                                    (let [handler (apply ring-handler router more)]
                                      (fn ([request] (check! router request (handler request)))
                                        ([request respond raise] (handler request respond
                                                                   raise)))))]
      (f))))
