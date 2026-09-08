(ns com.blockether.vis.tui.client-test
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.paths :as paths]
            [taoensso.telemere :as tel]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest multiplexed-listeners-share-the-session-subscription
  (let [state
        (atom {:subs {} :epoch 0 :future nil :stream nil})

        restarts
        (atom 0)]

    (with-redefs-fn {#'client/mux state
                     #'client/client-finalizing? (atom false)
                     #'client/ensure-release-hook! (fn []
                                                     nil)
                     #'client/restart-mux! #(swap! restarts inc)}
      (fn []
        (let [stop-a
              (client/mux-subscribe! "sid"
                                     (fn [_])
                                     10)

              stop-b
              (client/mux-subscribe! "sid"
                                     (fn [_])
                                     10)]

          (is (= 1 @restarts))
          (is (= 2 (count (get-in @state [:subs "sid" :sinks]))))
          (stop-a)
          (is (= 1 @restarts))
          (stop-b)
          (is (= 2 @restarts))
          (is (empty? (:subs @state))))))))

(deftest shutdown-stops-the-multiplexed-reader
  (let [closed
        (atom false)

        pending
        (java.util.concurrent.FutureTask. ^java.util.concurrent.Callable
                                          (fn []
                                            nil))

        stream
        (reify
          java.io.Closeable
            (close [_] (reset! closed true)))]

    (with-redefs-fn {#'client/mux (atom {:subs {} :epoch 0 :future pending :stream stream})}
      (fn []
        (#'client/shutdown-subscriptions!)
        (is @closed)
        (is (.isCancelled pending))))))

(deftest default-gateway-infers-local-bearer-token
  ;; Regression: bare `vis-agent tui` must authenticate to a local --pair gateway.
  (let [token-file
        (java.io.File/createTempFile "vis-tui-token-" ".txt")

        target-entry
        #'client/target-entry

        requests
        (atom [])]

    (try (spit token-file "local-test-token\n")
         (with-redefs [io/file
                       (fn [& _]
                         token-file)

                       http/request
                       (fn [request]
                         (swap! requests conj request)
                         {:status 200 :body "{}"})]

           (let [entry (target-entry nil nil)]
             (is (= "http://127.0.0.1:7890" (:base-url entry)))
             (is (= "local-test-token" (:secret entry)))
             (#'client/gw-send! entry "POST" "/v1/clients" {:body {:kind "tui"}})
             (is (= "Bearer local-test-token"
                    (get-in (first @requests) [:headers "Authorization"]))))
           (testing "explicit credentials win"
             (is (= "provided" (:secret (target-entry nil " provided "))))))
         (finally (.delete token-file)))))

(deftest explicit-gateways-never-read-local-credentials
  (with-redefs [io/file (fn [& _]
                          (throw (ex-info "must not read local token" {})))]
    (doseq [url ["gateway.example.com" "127.0.0.1:7899" "http://127.0.0.1:7890"]]
      (is (nil? (:secret (#'client/target-entry url nil)))))
    (is (= "provided" (:secret (#'client/target-entry nil "provided"))))))

(deftest absent-or-empty-local-token-stays-tokenless
  (let [token-file (java.io.File/createTempFile "vis-tui-token-" ".txt")]
    (try (with-redefs [io/file (fn [& _]
                                 token-file)]
           (spit token-file " \n")
           (is (nil? (:secret (#'client/target-entry nil nil))))
           (.delete token-file)
           (is (nil? (:secret (#'client/target-entry nil nil)))))
         (finally (.delete token-file)))))

(defn- failed-health-check
  [entry cause]
  (let [log (java.io.StringWriter.)]
    {:failure (binding [*err* log]
                (with-redefs-fn {#'http/request (fn [_]
                                                  (throw cause))
                                 #'client/target-checked? (atom false)}
                  #(try (#'client/check-target! entry) (catch Exception e e))))
     :log (str log)}))

(deftest connection-failure-is-an-actionable-user-error
  ;; Regression: startup health checks leaked a bare ConnectException to the terminal.
  (let [cause
        (doto (java.net.ConnectException.) (.initCause (java.nio.channels.ClosedChannelException.)))

        entry
        (#'client/target-entry
         "http://user:password@gateway.example.com:7890/private?token=query-secret#fragment"
         "bearer-secret")

        {:keys [failure log]}
        (failed-health-check entry cause)]

    (is (true? (:vis/user-error (ex-data failure))))
    (is (= :gateway/connection-failed (:type (ex-data failure))))
    (is (identical? cause (ex-cause failure)))
    (is (re-find #"Could not connect to the Vis gateway at http://gateway.example.com:7890"
                 (ex-message failure)))
    (is (re-find #"connection could not be established" (ex-message failure)))
    (is (re-find #"vis-agent gateway start" (ex-message failure)))
    (is (re-find #"VIS_GATEWAY_URL" (ex-message failure)))
    (is (re-find #"VPN" (ex-message failure)))
    (is (re-find #"Diagnostic log:" (ex-message failure)))
    (is (re-find #"java.net.ConnectException" log))
    (is (re-find #"java.nio.channels.ClosedChannelException" log))
    (doseq [secret ["password" "query-secret" "bearer-secret" "private" "fragment"]]
      (is (not (.contains (str (ex-message failure) (ex-data failure) log) secret))))))

(deftest transport-errors-have-specific-safe-reasons
  (let [entry (#'client/target-entry "https://gateway.example.com" "bearer-secret")]
    (doseq [[cause reason]
            [[(java.net.UnknownHostException. "bearer-secret") #"hostname could not be resolved"]
             [(doto (java.net.ConnectException.)
                (.initCause (java.nio.channels.UnresolvedAddressException.)))
              #"hostname could not be resolved"]
             [(java.net.http.HttpConnectTimeoutException. "bearer-secret") #"timed out"]
             [(java.net.http.HttpTimeoutException. "bearer-secret") #"timed out"]
             [(java.net.SocketTimeoutException. "bearer-secret") #"timed out"]
             [(javax.net.ssl.SSLHandshakeException. "bearer-secret") #"TLS certificate"]
             [(java.io.IOException. "bearer-secret") #"Network I/O failed"]]]
      (let [{:keys [failure log]} (failed-health-check entry cause)]
        (is (true? (:vis/user-error (ex-data failure))))
        (is (re-find reason (ex-message failure)))
        (is (= "https://gateway.example.com:443" (:endpoint (ex-data failure))))
        (is (identical? cause (ex-cause failure)))
        (is (not (.contains (str failure log) "bearer-secret")))))))

(deftest non-transport-errors-are-not-disguised-as-connection-failures
  (let [entry (#'client/target-entry "gateway.example.com" nil)]
    (doseq [cause [(IllegalStateException. "bug") (InterruptedException. "cancelled")]]
      (let [{:keys [failure log]} (failed-health-check entry cause)]
        (is (identical? cause failure))
        (is (= "" log))))
    (doseq [status [200 401 403 503]]
      (with-redefs [http/request (fn [_]
                                   {:status status :body "{}"})]
        (is (= status (:status (#'client/gw-send! entry "GET" "/healthz" {}))))))))

(deftest standalone-tui-installs-diagnostic-file-handler
  ;; The standalone client's no-op init left slow-frame signals with no file sink.
  (let [calls (atom [])]
    (with-redefs [paths/log-file (constantly "tui-test.log")
                  tel/handler:file (fn [opts]
                                     (swap! calls conj [:file opts])
                                     identity)
                  tel/add-handler! (fn [id _ opts]
                                     (swap! calls conj [:add id opts]))]

      (client/init!))
    (is (= [:file :add] (mapv first @calls)))
    (is (= "tui-test.log" (get-in @calls [0 1 :path])))
    (is (= :info (get-in @calls [1 2 :min-level])))
    (is (= :dropping (get-in @calls [1 2 :async :mode])))))

(deftest standalone-diagnostics-reach-a-real-file-and-flush-on-shutdown
  (let [dir
        (.toFile (java.nio.file.Files/createTempDirectory
                   "vis-tui-diagnostics-"
                   (make-array java.nio.file.attribute.FileAttribute 0)))

        log
        (io/file dir "tui.log")]

    (try (with-redefs-fn {#'paths/log-file (constantly (.getPath log))
                          #'client/shutdown-subscriptions! (fn []
                                                             nil)
                          #'client/release-client! (fn []
                                                     nil)
                          #'client/client-finalizing? (atom false)}
           #(do (client/init!)
                (tel/log! {:level :warn :id ::diagnostic-probe :msg "diagnostic-probe"})
                (client/shutdown!)))
         (is (.exists log))
         (is (.contains (slurp log) "diagnostics-ready"))
         (is (.contains (slurp log) "diagnostic-probe"))
         (finally (tel/remove-handler! :file/tui)
                  (doseq [f (reverse (file-seq dir))]
                    (.delete ^java.io.File f))))))

(defn- with-router-cache
  "Run a cache scenario with a controllable clock and queued background work."
  [cached f]
  (let [cache
        (atom cached)

        pending
        (atom [])

        now
        (atom 60000)

        calls
        (atom 0)

        response
        (atom [{"id" "test-provider" "default_model" "new-model"}])]

    (with-redefs-fn {#'client/router-cache* cache
                     #'client/now-ms (fn ^long []
                                       (long @now))
                     #'client/router (fn []
                                       (swap! calls inc)
                                       (let [value @response]
                                         (if (instance? Throwable value) (throw value) value)))
                     #'clojure.core/future-call (fn [task]
                                                  (swap! pending conj task)
                                                  nil)}
      #(f {:cache cache :pending pending :now now :calls calls :response response}))))

(defn- run-router-refresh!
  [pending]
  (let [[tasks _] (swap-vals! pending #(vec (rest %)))]
    (when-let [task (first tasks)]
      (task))))

(deftest footer-router-cache-never-fetches-on-the-render-thread
  ;; Typing and scrolling paused together when the footer's 30s cache expired.
  (doseq [cached [nil {:at 0 :rows [{"id" "test-provider" "default_model" "old-model"}]}]]
    (with-router-cache cached
                       (fn [{:keys [pending calls response]}]
                         (is (= (:rows cached) (#'client/router-cached)))
                         (is (zero? @calls) "A cold or stale read must not call the gateway inline")
                         (dotimes [_ 20]
                           (#'client/router-cached))
                         (is (= 1 (count @pending)) "Render-frequency misses share one refresh")
                         (run-router-refresh! pending)
                         (is (= @response (#'client/router-cached)))
                         (is (= 1 @calls))
                         (is (empty? @pending))))))

(deftest router-refresh-failure-keeps-data-and-backs-off
  (let [rows [{"id" "test-provider" "default_model" "old-model"}]]
    (with-router-cache {:at 0 :rows rows}
                       (fn [{:keys [pending calls response now]}]
                         (reset! response (ex-info "test gateway unavailable" {}))
                         (is (= rows (#'client/router-cached)))
                         (run-router-refresh! pending)
                         (dotimes [_ 20]
                           (is (= rows (#'client/router-cached))))
                         (is (= 1 @calls) "A failing gateway must not be retried every frame")
                         (is (empty? @pending))
                         (swap! now + 30000)
                         (reset! response [{"id" "test-provider"
                                            "default_model" "recovered-model"}])
                         (is (= rows (#'client/router-cached)))
                         (run-router-refresh! pending)
                         (is (= @response (#'client/router-cached)))
                         (is (= 2 @calls))))))

(deftest router-invalidation-does-not-publish-an-obsolete-in-flight-response
  (let [rows [{"id" "test-provider" "default_model" "old-model"}]]
    (with-router-cache
      {:at 0 :rows rows}
      (fn [{:keys [pending response calls]}]
        (#'client/router-cached)
        (client/refresh-cached-routers!)
        (is (= rows (#'client/router-cached)) "Invalidation keeps the displayed snapshot")
        (is (= 1 (count @pending)) "Invalidation must not overlap requests")
        (run-router-refresh! pending)
        (is (= rows (#'client/router-cached)) "The pre-invalidation request cannot win")
        (is (= 1 (count @pending)))
        (reset! response [{"id" "test-provider" "default_model" "latest-model"}])
        (run-router-refresh! pending)
        (is (= @response (#'client/router-cached)))
        (is (= 2 @calls))))))

(deftest router-refresh-notifies-only-when-displayed-data-changes
  (with-router-cache nil
                     (fn [{:keys [pending now response]}]
                       (let [wakes (atom 0)]
                         (client/watch-router! ::test #(swap! wakes inc))
                         (try (#'client/router-cached)
                              (is (zero? @wakes))
                              (run-router-refresh! pending)
                              (is (= 1 @wakes))
                              (swap! now + 30000)
                              (#'client/router-cached)
                              (run-router-refresh! pending)
                              (is (= 1 @wakes) "Unchanged metadata does not force extra frames")
                              (client/unwatch-router! ::test)
                              (reset! response [{"id" "test-provider"
                                                 "default_model" "another-model"}])
                              (swap! now + 30000)
                              (#'client/router-cached)
                              (run-router-refresh! pending)
                              (is (= 1 @wakes))
                              (finally (client/unwatch-router! ::test)))))))

(deftest explicit-config-load-primes-the-nonblocking-footer-cache
  (with-router-cache
    nil
    (fn [{:keys [pending calls]}]
      (is (false? (client/router-initialized?)))
      (is (= "new-model" (:default-model (client/load-config))))
      (is (true? (client/router-initialized?)))
      (is (= "new-model" (get-in (client/get-router) [:providers 0 :default-model])))
      (is (= 1 @calls))
      (is (empty? @pending))
      (client/reload-config!)
      (is (false? (client/router-initialized?)))
      (is (= "new-model" (get-in (client/get-router) [:providers 0 :default-model])))
      (is (= 1 @calls)))))

(deftest explicit-config-load-wins-over-an-older-background-refresh
  (with-router-cache nil
                     (fn [{:keys [pending response]}]
                       (#'client/router-cached)
                       (client/load-config)
                       (reset! response [{"id" "test-provider" "default_model" "obsolete-model"}])
                       (run-router-refresh! pending)
                       (is (= "new-model"
                              (get-in (client/get-router) [:providers 0 :default-model])))
                       (is (empty? @pending)))))
