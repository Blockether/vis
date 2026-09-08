(ns com.blockether.vis.internal.gateway.keychain
  "The ONE reader of a push credential out of the macOS login keychain.

   Both push transports keep their signing secret there rather than in a file on
   disk: APNs its ES256 `.p8` (service `vis-apns`), FCM its Google service-account
   JSON (service `vis-fcm`). The secret never sits in a world-readable file.

   Each answer is held in memory for `CACHE_TTL_MS` before `security` is asked
   again. It used to be read on demand and never cached, so locking the keychain
   revoked access immediately - but `push/config` asks for five secrets, and it
   runs inside `capabilities`, a request every connected client makes: a JFR
   profile of a live gateway caught the daemon forking `security` at a steady
   rate, ~21 ms of wall and ~10 ms of CPU a fork, to re-read five values that
   change only when a human edits the keychain. Locking the keychain now revokes
   access within the TTL rather than instantly; an unlocked-again keychain is
   likewise noticed within it.

   nil anywhere but macOS, and nil while `vis.push.home` is set: a redirected push
   home means a test fixture, and the developer's real keychain must never leak
   into it."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [com.blockether.vis.internal.util :as util])
  (:import [java.nio.charset StandardCharsets]))

(def ^:private ^:const CACHE_TTL_MS
  "How long one `security` answer - present OR absent - is reused. Long enough
   that a client polling `capabilities` never forks, short enough that a locked
   or newly-provisioned keychain is honoured within a minute."
  60000)

(defonce ^:private cache
  ;; [service account] -> {:at epoch-ms :value secret-or-nil}
  (atom {}))

(defn reset-cache!
  "Forget every cached answer, so the next read asks `security` again. For tests
   and for the moment a credential is written."
  []
  (reset! cache {})
  nil)

(defn- unhex
  "`security -w` prints hex, not text, whenever the stored password is not plain
   printable ASCII — which an embedded PEM never is. Decode that back."
  [s]
  (if (and (even? (count s)) (re-matches #"(?i)[0-9a-f]{32,}" s))
    (String. (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 16))
                              (partition 2 s)))
             StandardCharsets/UTF_8)
    s))

(defn- read-secret!
  "Ask `security` for the generic password under `service` / `account`: the
   value, or nil when there is none (or the keychain is locked)."
  [service account]
  (try (let [{:keys [exit out]}
             (sh/sh "security" "find-generic-password" "-s" service "-a" account "-w")]
         (when (and (= 0 (long exit)) (not (str/blank? out))) (unhex (str/trim out))))
       (catch Throwable _ nil)))

(defn secret
  "The generic password stored under `service` / `account`, or nil when there is
   none to read here. One `security` fork per `CACHE_TTL_MS` per key."
  [service account]
  (when (and (str/includes? (str/lower-case (str (System/getProperty "os.name"))) "mac")
             (nil? (System/getProperty "vis.push.home")))
    (let [k
          [service account]

          now
          (util/now-ms)

          hit
          (get @cache k)]

      (if (and hit (< (- now (long (:at hit))) (long CACHE_TTL_MS)))
        (:value hit)
        (let [v (read-secret! service account)]
          (swap! cache assoc k {:at now :value v})
          v)))))
