(ns com.blockether.vis.tui.client-test
  (:require [babashka.http-client :as http]
            [clojure.java.io :as io]
            [com.blockether.vis.tui.client :as client]
            [com.blockether.vis.tui.paths :as paths]
            [taoensso.telemere :as tel]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

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
