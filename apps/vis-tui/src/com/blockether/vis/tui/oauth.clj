(ns com.blockether.vis.tui.oauth
  "Browser sign-in for the thin TUI. A remote gateway cannot receive this
   machine's localhost redirect, so receive it here and forward only the matched
   callback over the already-paired gateway client. PKCE and tokens stay there.
   Providers and MCP use the same waiting/cancellation interaction."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.external-opener :as opener])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.net BindException InetSocketAddress URI URLDecoder]
           [java.nio.charset StandardCharsets]))

(set! *unchecked-math* :warn-on-boxed)

(defn- query
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

(defn- destination
  [flow]
  (try (let [q
             (query (.getRawQuery (URI. ^String (get flow "url"))))

             redirect
             (or (get flow "redirect_uri") (get q "redirect_uri"))

             uri
             (URI. ^String redirect)]

         (when (and (= "http" (.getScheme uri))
                    (contains? #{"localhost" "127.0.0.1"} (.getHost uri))
                    (<= 1 (.getPort uri) 65535)
                    (nil? (.getRawUserInfo uri))
                    (nil? (.getRawQuery uri))
                    (nil? (.getRawFragment uri))
                    (not (str/blank? (.getRawPath uri)))
                    (not (str/blank? (get q "state"))))
           {:uri uri :redirect redirect :state (get q "state")}))
       (catch Exception _ nil)))

(defn- listen!
  [{:keys [^URI uri redirect state]}]
  (try
    (let [server
          (HttpServer/create (InetSocketAddress. "127.0.0.1" (.getPort uri)) 0)

          result
          (promise)

          path
          (.getRawPath uri)]

      (try (.createContext
             server
             path
             (reify
               HttpHandler
                 (handle [_ exchange]
                   (try (let [request-uri
                              (.getRequestURI exchange)

                              q
                              (query (.getRawQuery request-uri))

                              valid?
                              (and (= "GET" (.getRequestMethod exchange))
                                   (= (.getRawAuthority uri)
                                      (.getFirst (.getRequestHeaders exchange) "Host"))
                                   (= path (.getRawPath request-uri))
                                   (= state (get q "state"))
                                   (not= (str/blank? (get q "code")) (str/blank? (get q "error"))))

                              status
                              (cond (not valid?) 400
                                    (realized? result) 409
                                    :else 200)

                              body
                              (.getBytes
                                (if (= 200 status)
                                  "Callback received. Return to Vis to check the sign-in result."
                                  "Callback does not match a pending Vis sign-in.")
                                StandardCharsets/UTF_8)

                              headers
                              (.getResponseHeaders exchange)]

                          (doseq [[k v] {"Content-Type" "text/plain; charset=utf-8"
                                         "Cache-Control" "no-store"
                                         "Referrer-Policy" "no-referrer"
                                         "Content-Security-Policy"
                                         "default-src 'none'; frame-ancestors 'none'"}]
                            (.set headers k v))
                          (try (.sendResponseHeaders exchange status (long (alength body)))
                               (with-open [out (.getResponseBody exchange)]
                                 (.write out body))
                               (finally (when (= 200 status)
                                          (deliver
                                            result
                                            (str redirect "?" (.getRawQuery request-uri)))))))
                        (catch Exception _ nil)
                        (finally (.close exchange))))))
           (.setExecutor server nil)
           (.start server)
           {:result result :stop! #(.stop server 0)}
           (catch Throwable t (.stop server 0) (throw t))))
    ;; On the gateway's own machine its receiver already owns the port; poll it.
    (catch BindException _ nil)))

(defn login!
  "Open a browser and hold `q`'s band until completion or Escape. `complete!`
   takes the matched URL; `poll!` and `cancel!` take no arguments. Each closure
   pins the original gateway and flow. Returns the wire verdict or nil on cancel.
   Browser flows without a loopback destination retain explicit manual input."
  [q label flow complete! poll! cancel!]
  (let [target
        (destination flow)

        receiver
        (try (when target (listen! target)) (catch Throwable t (cancel!) (throw t)))

        ttl
        (min 900000
             (max 1
                  (- (long (or (get flow "expires_at")
                               (get flow "expires_at_ms")
                               (+ (System/currentTimeMillis) 900000)))
                     (System/currentTimeMillis))))

        deadline
        (+ (System/currentTimeMillis) ttl)

        worker
        (atom nil)

        completed?
        (atom false)]

    (try (opener/open! (or (get flow "url") (get flow "verification_uri")))
         (let [verdict
               (if (or target (= "device" (get flow "kind")))
                 (let [result (future
                                (loop []

                                  (if (>= (System/currentTimeMillis) deadline)
                                    {"status" "error"
                                     "message" "Authorization timed out. Start again."}
                                    (if-let [input (some-> (:result receiver)
                                                           (deref 0 nil))]
                                      (complete! input)
                                      (let [v (poll!)]
                                        (if (= "pending" (get v "status"))
                                          (do (Thread/sleep
                                                (max 500 (long (or (get flow "interval_ms") 500))))
                                              (recur))
                                          v))))))]
                   (reset! worker result)
                   (when ((:wait! q)
                           (str label " — waiting for authorization")
                           (constantly
                             (if-let [code (get flow "user_code")]
                               (str "Enter "
                                    code
                                    " at "
                                    (or (get flow "verification_uri") (get flow "url"))
                                    " · Waiting for approval. Esc cancels.")
                               "Finish sign-in in the browser. Vis will finish automatically."))
                           #(realized? result))
                     @result))
                 (when-let [input ((:read! q)
                                    (str label " — paste the final browser URL:")
                                    {:placeholder (get flow "url")})]
                   (when-not (str/blank? input) (complete! (str/trim input)))))]
           (reset! completed? (= "ok" (get verdict "status")))
           verdict)
         (finally (when-let [f @worker]
                    (future-cancel f))
                  (when-let [stop! (:stop! receiver)]
                    (stop!))
                  (when-not @completed? (cancel!))))))
