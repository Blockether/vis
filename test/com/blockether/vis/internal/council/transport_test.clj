(ns com.blockether.vis.internal.council.transport-test
  "Real HTTP boundaries for the Rooms client. No live gateway is used."
  (:require [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.council.transport :as transport]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress]
           [java.nio.charset StandardCharsets]))

(set! *warn-on-reflection* true)

(defn- with-relay
  [f]
  (let [response
        (atom {:status 200 :body "[]"})

        requests
        (atom [])

        server
        (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]

    (.createContext server
                    "/"
                    (reify
                      HttpHandler
                        (handle [_ exchange]
                          (let [^HttpExchange exchange
                                exchange

                                {:keys [status body]}
                                @response

                                data
                                (.getBytes ^String body StandardCharsets/UTF_8)]

                            (swap! requests conj (.getPath (.getRequestURI exchange)))
                            (.set (.getResponseHeaders exchange) "Location" "/credential-leak")
                            (.sendResponseHeaders exchange (int status) (alength data))
                            (with-open [out (.getResponseBody exchange)]
                              (.write out data)))
                          nil)))
    (.start server)
    (try (f {:state {:relay_url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
                     :credential (apply str (repeat 43 "a"))}
             :response response
             :requests requests})
         (finally (.stop server 0)))))

(defdescribe
  rooms-http-boundary
  (it "validates real HTTP responses and never follows redirects"
      (with-relay
        (fn [{:keys [state response requests]}]
          (expect (= [] (transport/call! state :get "/v1/rooms" {} nil {})))
          (doseq [[status body expected]
                  [[302 "{}" 502] [200 "{}" 502]
                   [200 (apply str (repeat (inc (get transport/limits "response_bytes")) "x")) 502]
                   [403 (wire/json-str {:error {:code "forbidden" :message "Denied"}}) 403]]]
            (reset! response {:status status :body body})
            (expect (= expected
                       (try (transport/call! state :get "/v1/rooms" {} nil {})
                            nil
                            (catch clojure.lang.ExceptionInfo e (:status (ex-data e)))))))
          (expect (= 5 (count @requests)))
          (expect (every? #{"/v1/rooms"} @requests)))))
  (it "refuses malformed and secret-bearing origins before HTTP"
      (doseq [origin ["https://user:private@gateway.example.com"
                      "https://gateway.example.com/private" "http://gateway.example.com"
                      "https://gateway.example.com#private"]]
        (expect (= "Council Rooms request failed"
                   (try (transport/origin origin) nil (catch Exception e (ex-message e))))))))
