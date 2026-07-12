(ns com.blockether.vis.ext.react-native-companion.core
  "React Native mobile companion for the vis gateway.

   This extension does not register a `vis channels ...` entry. It only contributes
   the tiny `/rn` and `/rn/config.json` gateway helper routes that the Expo app can
   probe before talking to the canonical `/v1` JSON API. The actual React Native app
   lives in `app/` next to this extension and is a strict TypeScript Expo project."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]))

(def ^:private route-load-stamp
  "Re-evaluated on namespace reload so the gateway remounts changed /rn routes."
  (System/currentTimeMillis))

(defn- json-response
  ([body] (json-response 200 body))
  ([status body]
   {:status status
    :headers {"Content-Type" "application/json; charset=utf-8" "Cache-Control" "no-cache"}
    :body (json/write-json-str body)}))

(defn- text-response
  [body]
  {:status 200
   :headers {"Content-Type" "text/plain; charset=utf-8" "Cache-Control" "no-cache"}
   :body body})

(defn- loopback-host?
  [host]
  (contains? #{"localhost" "127.0.0.1" "::1" "0:0:0:0:0:0:0:1"} (str/lower-case (str host))))

(defn- lan-host
  "Best-effort IPv4 address a phone on the same network can reach.

   Falls back to the gateway bind host when no non-loopback address is visible
   (VPN/offline machines, CI, etc.)."
  [gateway-host]
  (or (try (some (fn [^java.net.NetworkInterface iface]
                   (when (and (.isUp iface) (not (.isLoopback iface)) (not (.isVirtual iface)))
                     (some (fn [^java.net.InetAddress addr]
                             (when (and (instance? java.net.Inet4Address addr)
                                        (not (.isLoopbackAddress addr)))
                               (.getHostAddress addr)))
                           (enumeration-seq (.getInetAddresses iface)))))
                 (enumeration-seq (java.net.NetworkInterface/getNetworkInterfaces)))
           (catch Throwable _ nil))
      gateway-host))

(defn- public-host [host] (if (loopback-host? host) (lan-host host) host))

(defn- gateway-url
  [{:keys [host port]} & [{:keys [public?]}]]
  (str "http://" (if public? (public-host host) host) ":" port))

(defn- config-payload
  []
  (let [{:keys [host port require_token] :as status} (vis/gateway-daemon-status)]
    {:name "vis React Native"
     :channel "react-native"
     :api_base (str (gateway-url status {:public? true}) "/v1")
     :gateway_url (gateway-url status {:public? true})
     :loopback_gateway_url (gateway-url status)
     :host host
     :lan_host (public-host host)
     :port port
     :requires_token (boolean require_token)
     :auth_header "Authorization: Bearer <token>"}))

(defn- index-handler
  [_request]
  (text-response (str "vis React Native companion\n\n" "Probe: /rn/config.json\n"
                      "API:   /v1\n\n"
                      "Run the Expo app in app/ with EXPO_PUBLIC_VIS_GATEWAY_URL set.")))

(defn- config-handler [_request] (json-response (config-payload)))

(defn- rn-routes
  [_token]
  [["/rn" {:get #'index-handler}] ["/rn/config.json" {:get #'config-handler}]])

(defn- rn-contribution
  []
  {:prefix "/rn" :rev route-load-stamp :routes rn-routes :open-uris #{"/rn" "/rn/config.json"}})


(def react-native-extension
  (vis/extension {:ext/name "companion-react-native"
                  :ext/description "Expo React Native mobile companion for the vis gateway."
                  :ext/version "0.1.0"
                  :ext/author "Blockether"
                  :ext/owner "vis"
                  :ext/license "Apache-2.0"
                  :ext/kind "companion"
                  :ext/channel-contributions {:gateway.slot/http-routes
                                              [{:id :react-native/config :fn #'rn-contribution}]}}))

(vis/register-extension! react-native-extension)
