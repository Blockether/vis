(ns com.blockether.vis.internal.provider.callback
  "Short-lived OAuth loopback receivers. They carry only a callback, never tokens.

   `listen!` binds a numeric loopback address, validates the exact destination and
   state before accepting ONE response, and closes on completion, stop or expiry.
   The browser sees a receipt, not a claim that token exchange has succeeded.
   No request values are reflected into HTML or logged. Shared by provider and MCP auth."
  (:require [clojure.string :as str])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress URI URLDecoder]
           [java.nio.charset StandardCharsets]))

(set! *unchecked-math* :warn-on-boxed)

(defn query
  "Decode a bounded OAuth query. Duplicate keys are refused, not silently overwritten."
  [raw]
  (when (and (string? raw) (<= (count raw) 8192))
    (try (reduce (fn [m part]
                   (let [[k v]
                         (str/split part #"=" 2)

                         k
                         (URLDecoder/decode ^String k StandardCharsets/UTF_8)

                         v
                         (URLDecoder/decode (str (or v "")) StandardCharsets/UTF_8)]

                     (if (contains? m k) (reduced nil) (assoc m k v))))
                 {}
                 (str/split raw #"&"))
         (catch IllegalArgumentException _ nil))))

(defn response-uri?
  "True only for an exact OAuth redirect URI with a matching state and code OR error.
   Neither a bare code nor a URL from another destination is an automatic callback."
  [redirect-uri expected-state input]
  (try (let [expected
             (URI. ^String redirect-uri)

             actual
             (URI. ^String input)

             q
             (query (.getRawQuery actual))]

         (boolean (and (string? expected-state)
                       (not (str/blank? expected-state))
                       (= expected-state (get q "state"))
                       (= [(.getScheme expected) (.getRawAuthority expected) (.getRawPath expected)]
                          [(.getScheme actual) (.getRawAuthority actual) (.getRawPath actual)])
                       (nil? (.getRawFragment actual))
                       (nil? (.getRawUserInfo actual))
                       (not= (contains? q "code") (contains? q "error"))
                       (not= (str/blank? (get q "code")) (str/blank? (get q "error"))))))
       (catch Exception _ false)))

(defn- reply!
  [^HttpExchange exchange status message]
  (let [headers
        (.getResponseHeaders exchange)

        body
        (.getBytes (str "<!doctype html><meta charset=utf-8><title>Vis sign-in</title>"
                        "<h1>Vis sign-in</h1><p>"
                        message
                        "</p>")
                   StandardCharsets/UTF_8)]

    (doseq [[k v] {"Content-Type" "text/html; charset=utf-8"
                   "Cache-Control" "no-store"
                   "Referrer-Policy" "no-referrer"
                   "Content-Security-Policy" "default-src 'none'; frame-ancestors 'none'"
                   "X-Content-Type-Options" "nosniff"}]
      (.set headers k v))
    (.sendResponseHeaders exchange (int status) (long (alength body)))
    (with-open [out (.getResponseBody exchange)]
      (.write out body))))

(defn listen!
  "Listen for one callback for at most `ttl-ms` (1..900000). Returns
   {:redirect-uri :result :stop!}; result yields the validated URL, :expired or
   :cancelled. Port 0 requests an ephemeral port (MCP); fixed provider ports and
   paths are preserved. Only http://localhost or http://127.0.0.1 is accepted.
   A bind failure propagates; the caller must explicitly offer another transport."
  [redirect-uri expected-state ttl-ms]
  (let [uri
        (URI. ^String redirect-uri)

        port
        (.getPort uri)

        path
        (.getRawPath uri)]

    (when-not (and (= "http" (.getScheme uri))
                   (contains? #{"localhost" "127.0.0.1"} (.getHost uri))
                   (<= 0 port 65535)
                   (not (str/blank? path))
                   (nil? (.getRawUserInfo uri))
                   (nil? (.getRawQuery uri))
                   (nil? (.getRawFragment uri))
                   (string? expected-state)
                   (not (str/blank? expected-state))
                   (<= 1 (long ttl-ms) 900000))
      (throw (ex-info "Invalid OAuth loopback receiver" {})))
    (let [server
          (HttpServer/create (InetSocketAddress. "127.0.0.1" port) 0)

          redirect
          (str "http://" (.getHost uri) ":" (.getPort (.getAddress server)) path)

          authority
          (.getRawAuthority (URI. redirect))

          result
          (promise)

          claimed?
          (atom false)

          stopped?
          (atom false)

          stop!
          (fn []
            (when (compare-and-set! stopped? false true)
              (deliver result :cancelled)
              (.stop server 0)))]

      (try
        (.createContext
          server
          path
          (reify
            HttpHandler
              (handle [_ exchange]
                (try (let [request-uri
                           (.getRequestURI exchange)

                           input
                           (str redirect "?" (.getRawQuery request-uri))

                           valid?
                           (and (= "GET" (.getRequestMethod exchange))
                                (= authority (.getFirst (.getRequestHeaders exchange) "Host"))
                                (= path (.getRawPath request-uri))
                                (not @stopped?)
                                (response-uri? redirect expected-state input))]

                       (cond (not valid?)
                             (reply!
                               exchange
                               400
                               "This callback does not match the pending sign-in. Return to Vis.")
                             (not (compare-and-set! claimed? false true))
                             (reply! exchange
                                     409
                                     "This callback has already been received. Return to Vis.")
                             :else
                             (try (reply!
                                    exchange
                                    200
                                    "Callback received. Return to Vis to check the sign-in result.")
                                  (finally (deliver result input)))))
                     (catch Exception _ nil)
                     (finally (.close exchange))))))
        (.setExecutor server nil)
        (.start server)
        (future (when (= :expired (deref result (long ttl-ms) :expired)) (deliver result :expired))
                (stop!))
        {:redirect-uri redirect :result result :stop! stop!}
        (catch Throwable t (stop!) (throw t))))))
