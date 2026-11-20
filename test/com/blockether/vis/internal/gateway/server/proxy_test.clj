;; Copyright (c) Blockether. All rights reserved.
(ns com.blockether.vis.internal.gateway.server.proxy-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config-core]
            [com.blockether.vis.internal.gateway.server.instance :as instance]
            [com.blockether.vis.internal.gateway.server.proxy :as proxy]
            [com.blockether.vis.internal.sandbox.egress-proxy :as egress]
            [lazytest.core :refer [defdescribe expect it throws?]])
  (:import (java.io InputStream)
           (java.net ConnectException InetSocketAddress ServerSocket Socket URI)
           (java.nio.charset StandardCharsets)
           (java.util Base64)))

(def ^:private routes
  (wire/parse-json (slurp (io/resource "vis-contract/fixtures/proxy-routes.json"))))

(defn- proxied?
  "The decision of the gateway for the link `url`, as a client routes it."
  [policy url]
  (let [uri
        (try (URI. url) (catch Exception _ nil))

        scheme
        (some-> uri
                .getScheme
                str/lower-case)

        host
        (some-> uri
                .getHost)]

    (boolean (when (and host (#{"http" "https"} scheme))
               (let [port (if (pos? (.getPort uri)) (.getPort uri) (if (= "https" scheme) 443 80))]
                 (:allow? (egress/decide policy nil host nil port)))))))

(defdescribe proxy-routes-fixture
             (it "the gateway proxies exactly the links that the contract fixture names"
                 (doseq [{:strs [allowed_domains denied_domains ports cases]}
                         (get routes "groups")

                         :let [settings
                               (proxy/settings {"gateway" {"proxy" {"allowed_domains"
                                                                    allowed_domains
                                                                    "denied_domains" denied_domains
                                                                    "ports" ports}}})

                               policy
                               (proxy/policy settings "tok" "tok" [])]
                         {:strs [url is_proxied]}
                         cases]

                   (expect (= is_proxied (proxied? policy url)) url))))

(defdescribe
  proxy-settings
  (it "is off without allowed domains, and never opens every host"
      (expect (nil? (proxy/settings {})))
      (expect (nil? (proxy/settings {"gateway" {"proxy" {"allowed_domains" []}}})))
      (expect (nil? (proxy/settings {"gateway" {"proxy" {"allowed_domains" ["*" " "]}}})))
      (expect (= {:allowed-domains ["corp.internal"] :denied-domains [] :ports [80 443] :port nil}
                 (proxy/settings {"gateway" {"proxy" {"allowed_domains" ["*"
                                                                         " Corp.Internal "]}}})))))

(def ^:private real-global-settings
  "The configuration reader of the proxy, before a test replaces it."
  @#'proxy/global-settings)

(defn- upstream!
  "A loopback server that answers `hello` to each connection."
  []
  (let [server (ServerSocket. 0 50 (java.net.InetAddress/getByName "127.0.0.1"))]
    (future (try (while true
                   (with-open [s (.accept server)]
                     (.write (.getOutputStream s) (.getBytes "hello\n" StandardCharsets/UTF_8))
                     (.flush (.getOutputStream s))))
                 (catch Throwable _ nil)))
    server))

(defn- read-line*
  [^InputStream in]
  (loop [sb (StringBuilder.)]
    (let [b (.read in)]
      (cond (neg? b) (str sb)
            (= b 10) (str/trimr (str sb))
            :else (recur (.append sb (char b)))))))

(defn- connect!
  "Send `CONNECT target` to the proxy on `port`. Returns the status code and, after a
   200, the first line of the tunnel."
  [port target token]
  (with-open [s (Socket.)]
    (.connect s (InetSocketAddress. "127.0.0.1" (int port)) 2000)
    (.setSoTimeout s 5000)
    (let [out (.getOutputStream s)
          in (.getInputStream s)
          auth (when token
                 (str "Proxy-Authorization: Basic "
                      (.encodeToString (Base64/getEncoder)
                                       (.getBytes (str token ":x") StandardCharsets/UTF_8))
                      "\r\n"))]

      (.write out
              (.getBytes (str "CONNECT " target " HTTP/1.1\r\nHost: " target "\r\n" auth "\r\n")
                         StandardCharsets/ISO_8859_1))
      (.flush out)
      (let [status (second (str/split (read-line* in) #" "))]
        (if (= "200" status)
          (do (loop []

                (when-not (str/blank? (read-line* in)) (recur)))
              [status (read-line* in)])
          [status])))))

(defn- proxy-answer
  "The JSON answer of `GET /v1/proxy`."
  []
  (wire/parse-json (:body ((get proxy/handlers [:get "/v1/proxy"]) {:request-method :get}))))

(defdescribe
  proxy-listener
  (it
    "opens an allowed host only for the gateway token, and follows the configuration"
    (let [before
          @instance/server-state

          upstream
          (upstream!)

          gateway
          (ServerSocket. 0 50 (java.net.InetAddress/getByName "127.0.0.1"))

          up
          (.getLocalPort upstream)

          gw
          (.getLocalPort gateway)

          current
          (atom {:allowed-domains ["localhost"] :denied-domains [] :ports [up gw] :port nil})]

      (reset! instance/server-state {:host "127.0.0.1" :port gw :token "tok"})
      (try
        (with-redefs-fn {#'proxy/global-settings (fn []
                                                   @current)}
          (fn []
            (let [port (proxy/sync!)]
              (expect (pos-int? port))
              (expect (= port (proxy/sync!)) "a repeated sync keeps the listener")
              (expect (= {"is_enabled" true
                          "port" port
                          "allowed_domains" ["localhost"]
                          "denied_domains" []
                          "ports" [up gw]}
                         (proxy-answer))
                      "a client gets the port and the domains of the running listener")
              (expect (= ["407"] (connect! port (str "localhost:" up) nil)))
              (expect (= ["407"] (connect! port (str "localhost:" up) "wrong")))
              (expect (= ["200" "hello"] (connect! port (str "localhost:" up) "tok")))
              (expect (= ["403"] (connect! port "example.org:443" "tok")) "a host off the list")
              (expect (= ["403"] (connect! port (str "localhost:" gw) "tok")) "the gateway port")
              (expect (= ["403"] (connect! port (str "localhost:" port) "tok")) "the proxy port")
              (with-redefs-fn {#'proxy/global-settings real-global-settings
                               #'config-core/load-global-config-raw (fn []
                                                                      (throw (ex-info "broken YAML"
                                                                                      {})))}
                #(expect (= ["403"] (connect! port (str "localhost:" up) "tok"))
                         "an unreadable configuration refuses, never opens every host"))
              (swap! current assoc :denied-domains ["127.0.0.1"])
              (expect (= ["200" "hello"] (connect! port (str "localhost:" up) "tok"))
                      "a denied name blocks no other name on the same address")
              (swap! current assoc :denied-domains ["localhost"])
              (expect (= ["403"] (connect! port (str "localhost:" up) "tok"))
                      "a domain change applies to the next connection")
              (reset! current nil)
              (expect (nil? (proxy/sync!)))
              (expect (= {"is_enabled" false
                          "port" nil
                          "allowed_domains" []
                          "denied_domains" []
                          "ports" []}
                         (proxy-answer))
                      "a client routes no link when the proxy is off")
              (expect (throws? ConnectException #(connect! port (str "localhost:" up) "tok"))
                      "the listener stops when the configuration turns the proxy off"))))
        (finally (proxy/stop!)
                 (.close upstream)
                 (.close gateway)
                 (reset! instance/server-state before))))))
