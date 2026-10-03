(ns com.blockether.vis.internal.automation.webhook
  "Webhook checks: signatures, event names, filters and prompt templates.

   Every function here is pure. A payload is untrusted data: it can fill a
   prompt template, but it never selects a target, a model or a tool."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util Arrays Base64 HexFormat)
           (javax.crypto Mac)
           (javax.crypto.spec SecretKeySpec)))

(defn- utf8 ^bytes [^String text] (.getBytes text StandardCharsets/UTF_8))

(defn hmac
  "HMAC-SHA256 of `data` with `key`."
  ^bytes [^bytes key ^bytes data]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. key "HmacSHA256"))
    (.doFinal mac data)))

(defn- concat-bytes
  ^bytes [^bytes head ^bytes tail]
  (let [result (Arrays/copyOf head (+ (alength head) (alength tail)))]
    (System/arraycopy tail 0 result (alength head) (alength tail))
    result))

(defn standard-key
  "The HMAC key of a Standard Webhooks secret. A `whsec_` secret uses its decoded
   bytes; any other secret uses its UTF-8 bytes."
  ^bytes [^String secret]
  (if (str/starts-with? secret "whsec_")
    (.decode (Base64/getDecoder) (subs secret 6))
    (utf8 secret)))

(defn standard-signature
  "The `v1,<base64>` signature of Standard Webhooks for one message."
  [^String secret ^String message-id ^String timestamp ^bytes body]
  (str "v1,"
       (.encodeToString (Base64/getEncoder)
                        (hmac (standard-key secret)
                              (concat-bytes (utf8 (str message-id "." timestamp ".")) body)))))

(defn- same-bytes? [^bytes a ^bytes b] (boolean (and a b (MessageDigest/isEqual a b))))

(defn- hex-bytes
  [^String text]
  (try (.parseHex (HexFormat/of) (str/lower-case text)) (catch IllegalArgumentException _ nil)))

(defn- base64-bytes
  [^String text]
  (try (.decode (Base64/getDecoder) text) (catch IllegalArgumentException _ nil)))

(defn- fresh?
  [timestamp now skew-seconds]
  (when-let [seconds (some-> timestamp
                             str/trim
                             parse-long)]
    (<= (abs (- (quot (long now) 1000) (long seconds))) (long skew-seconds))))

(defn verify
  "Answer nil when the request is signed with `secret`, else the reason
   `signature` or `timestamp`. `headers` have lower-case names."
  [kind ^String secret {:keys [headers ^bytes body now skew-seconds]}]
  (case kind
    "github"
    (let [header (str (get headers "x-hub-signature-256"))]
      (when-not (and (str/starts-with? header "sha256=")
                     (same-bytes? (hmac (utf8 secret) body) (hex-bytes (subs header 7))))
        "signature"))

    "standard"
    (let [message-id
          (get headers "webhook-id")

          timestamp
          (get headers "webhook-timestamp")

          signatures
          (get headers "webhook-signature")]

      (cond (not (and message-id timestamp signatures)) "signature"
            (not (fresh? timestamp now skew-seconds)) "timestamp"
            :else (let [expected (base64-bytes
                                   (subs (standard-signature secret message-id timestamp body) 3))]
                    (when-not (some (fn [item]
                                      (let [[version value] (str/split item #"," 2)]
                                        (and (= "v1" version)
                                             (same-bytes? expected
                                                          (some-> value
                                                                  base64-bytes)))))
                                    (str/split (str/trim signatures) #"\s+"))
                      "signature"))))

    "generic"
    (let [timestamp
          (get headers "x-webhook-timestamp")

          signature
          (get headers "x-webhook-signature-v2")]

      (cond (not (and timestamp signature)) "signature"
            (not (fresh? timestamp now skew-seconds)) "timestamp"
            (not (same-bytes? (hmac (utf8 secret)
                                    (concat-bytes (utf8 (str (str/trim timestamp) ".")) body))
                              (hex-bytes (str/trim signature))))
            "signature"
            :else nil))

    "token"
    (let [bearer
          (some-> (get headers "authorization")
                  (str/replace #"(?i)^bearer\s+" ""))

          token
          (or (get headers "x-gitlab-token") (get headers "x-webhook-token") bearer)]

      (when-not (and token (same-bytes? (utf8 secret) (utf8 (str/trim token)))) "signature"))

    "signature"))

(defn event-name
  "The event of a webhook request, from its headers or its payload."
  [headers payload]
  (or (get headers "x-github-event")
      (get headers "x-gitlab-event")
      (get headers "x-webhook-event")
      (when (map? payload)
        (some #(let [value (get payload %)] (when (string? value) value))
              ["type" "event_type" "object_kind"]))))

(defn delivery-id
  "The sender's delivery identity, used to drop a repeated delivery."
  [headers]
  (some #(not-empty (some-> (get headers %)
                            str/trim))
        ["x-github-delivery" "webhook-id" "x-gitlab-event-uuid" "idempotency-key" "x-request-id"]))

(defn event-accepted?
  "True when `events` is empty, or names the event or `event.action`."
  [events event payload]
  (or (empty? events)
      (let [action (when (map? payload)
                     (let [value (get payload "action")]
                       (when (string? value) value)))]
        (boolean (some (cond-> #{}
                         event
                         (conj event)

                         (and event action)
                         (conj (str event "." action)))
                       events)))))

(defn value-at
  "The value at a dot path such as `pull_request.base.ref` or `commits.0.id`,
   or ::missing."
  [payload path]
  (reduce (fn [value part]
            (cond (map? value) (if (contains? value part) (get value part) (reduced ::missing))
                  (and (sequential? value) (re-matches #"\d+" part))
                  (let [index (long (parse-long part))]
                    (if (< index (count value)) (nth value index) (reduced ::missing)))
                  :else (reduced ::missing)))
          payload
          (str/split path #"\.")))

(defn- filter-passes?
  [payload {:strs [field equals contains in] :as check}]
  (let [value (value-at payload field)]
    (and (not= ::missing value)
         (cond (contains? check "equals") (= equals value)
               (contains? check "contains") (cond (string? value) (str/includes? value contains)
                                                  (sequential? value) (boolean (some #{contains}
                                                                                     value))
                                                  :else false)
               (contains? check "in") (boolean (some #(= value %) in))
               :else false))))

(defn filters-pass?
  "True when every filter passes for the payload."
  [filters payload]
  (every? #(filter-passes? payload %) filters))

(defn- clip
  [^String text max-bytes]
  (let [data (utf8 text)]
    (if (<= (alength data) (long max-bytes))
      text
      (let [cut (loop [end (long max-bytes)]
                  (if (and (pos? end) (= 0x80 (bit-and (aget data (int end)) 0xC0)))
                    (recur (dec end))
                    end))]
        (str (String. data 0 (int cut) StandardCharsets/UTF_8) "…")))))

(defn- render-value [value] (if (string? value) value (wire/json-str value)))

(defn render
  "Fill `{dot.path}` placeholders from the payload and `{__raw__}` with the raw
   body. A missing path stays as written. Each value is clipped to `max-bytes`."
  [template payload ^String raw max-bytes]
  (str/replace template
               #"\{([A-Za-z0-9_][A-Za-z0-9_.\-]*)\}"
               (fn [[placeholder path]]
                 (if (= "__raw__" path)
                   (clip raw max-bytes)
                   (let [value (if (some? payload) (value-at payload path) ::missing)]
                     (if (= ::missing value) placeholder (clip (render-value value) max-bytes)))))))
