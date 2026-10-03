(ns com.blockether.vis.internal.council.transport
  "Bounded, authenticated calls to the canonical Rooms protocol. Errors contain no payloads."
  (:require [babashka.http-client :as http]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.util :as util])
  (:import (java.io InputStream)
           (java.net URI URLEncoder)
           (java.nio.charset StandardCharsets)))

(set! *warn-on-reflection* true)

(def schema (document/schema-document "rooms"))

(def limits (get schema "x-vis-limits"))

(defonce ^:private client (delay (http/client {:connect-timeout 3000 :redirect-policy :never})))

(defn fail!
  [status code]
  (throw (ex-info "Council Rooms request failed" {:status status :error :rooms-error :code code})))

(defn validate!
  [definition value]
  (when-not (document/valid? "rooms" definition value) (fail! 400 "invalid_request"))
  value)

(defn origin
  "Accept HTTPS origins and loopback HTTP. Never accept credentials, paths or redirects."
  [value]
  (try (let [uri
             (URI/create (str value))

             scheme
             (.getScheme uri)

             host
             (.getHost uri)]

         (when-not (and host
                        (nil? (.getUserInfo uri))
                        (nil? (.getRawQuery uri))
                        (nil? (.getRawFragment uri))
                        (#{"" "/"} (.getPath uri))
                        (or (= "https" scheme)
                            (and (= "http" scheme) (#{"127.0.0.1" "localhost" "[::1]"} host))))
           (fail! 400 "invalid_relay"))
         (str scheme "://" (.getRawAuthority uri)))
       (catch Exception _ (fail! 400 "invalid_relay"))))

(defn invite-parts
  "Read a secret from the fragment. Opening the link does not redeem it."
  [value]
  (try (let [uri
             (URI/create (str value))

             base
             (origin (str (.getScheme uri) "://" (.getRawAuthority uri)))

             fragment
             (.getRawFragment uri)

             token
             (second (re-matches #"invite=([A-Za-z0-9_-]+)" (or fragment "")))]

         (when-not (and (= "/rooms/join" (.getPath uri))
                        (nil? (.getRawQuery uri))
                        (document/valid? "rooms" "secret" token))
           (fail! 400 "invalid_invite"))
         {:relay_url base :token token})
       (catch Exception _ (fail! 400 "invalid_invite"))))

(defn call!
  "Validate both sides of one declared route. Never follow an HTTP redirect."
  [state method template params body query]
  (let [route
        (some #(when (and (= (str/upper-case (name method)) (get % "method"))
                          (= template (get % "path")))
                 %)
              (get schema "x-vis-http"))

        _
        (when-not route (fail! 400 "invalid_route"))

        _
        (doseq [[key value] params]
          (validate! (if (= key :entry_id) "entry_id" "id") value))

        _
        (when-let [definition (get route "request")]
          (validate! definition body))

        _
        (if-let [definition (get route "query")]
          (validate! definition query)
          (when (seq query) (fail! 400 "invalid_request")))

        path
        (reduce-kv #(str/replace %1 (str "{" (name %2) "}") (str %3)) template params)

        query-string
        (str/join "&"
                  (for [[k v] query]
                    (str (name k) "=" (URLEncoder/encode (str v) StandardCharsets/UTF_8))))

        payload
        (when body (wire/json-str body))

        cap
        (long (get limits "response_bytes"))]

    (when (and payload (> (alength (util/utf8 payload)) (long (get limits "request_bytes"))))
      (fail! 413 "too_large"))
    (try (let [response
               (http/request {:client @client
                              :method method
                              :uri (str (origin (:relay_url state))
                                        path
                                        (when (seq query-string) (str "?" query-string)))
                              :headers {"authorization" (str "Bearer " (:credential state))
                                        "content-type" "application/json"}
                              :body payload
                              :timeout 5000
                              :throw false
                              :as :stream})

               value
               (with-open [stream ^InputStream (:body response)]
                 (let [data (.readNBytes stream (int (inc cap)))]
                   (when (> (alength data) cap) (fail! 502 "invalid_response"))
                   (wire/parse-json (String. data StandardCharsets/UTF_8))))

               status
               (long (:status response))]

           (if (<= 200 status 299)
             (do (when-not (document/valid-json? "rooms" (get route "response") value)
                   (fail! 502 "invalid_response"))
                 (walk/keywordize-keys value))
             (fail! (if (<= 400 status 599) status 502)
                    (if (document/valid-json? "rooms" "error" value)
                      (get-in value ["error" "code"])
                      "invalid_response"))))
         (catch clojure.lang.ExceptionInfo e
           (if (= :rooms-error (:error (ex-data e))) (throw e) (fail! 503 "unavailable")))
         (catch Exception _ (fail! 503 "unavailable")))))
