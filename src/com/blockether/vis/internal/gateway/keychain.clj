(ns com.blockether.vis.internal.gateway.keychain
  "The ONE reader of a push credential out of the macOS login keychain.

   Both push transports keep their signing secret there rather than in a file on
   disk: APNs its ES256 `.p8` (service `vis-apns`), FCM its Google service-account
   JSON (service `vis-fcm`). Read on demand and never cached, so locking the
   keychain revokes access immediately and the secret never sits in a
   world-readable file.

   nil anywhere but macOS, and nil while `vis.push.home` is set: a redirected push
   home means a test fixture, and the developer's real keychain must never leak
   into it."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]))

(defn- unhex
  "`security -w` prints hex, not text, whenever the stored password is not plain
   printable ASCII — which an embedded PEM never is. Decode that back."
  [s]
  (if (and (even? (count s)) (re-matches #"(?i)[0-9a-f]{32,}" s))
    (String. (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 16))
                              (partition 2 s)))
             StandardCharsets/UTF_8)
    s))

(defn secret
  "The generic password stored under `service` / `account`, or nil when there is
   none to read here."
  [service account]
  (when (and (str/includes? (str/lower-case (str (System/getProperty "os.name"))) "mac")
             (nil? (System/getProperty "vis.push.home")))
    (try (let [{:keys [exit out]}
               (sh/sh "security" "find-generic-password" "-s" service "-a" account "-w")]
           (when (and (= 0 (long exit)) (not (str/blank? out))) (unhex (str/trim out))))
         (catch Throwable _ nil))))
