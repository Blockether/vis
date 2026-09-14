(ns com.blockether.vis.internal.extension.client
  "Ephemeral, session-owned callbacks into a live SDK application. No code is uploaded.
   Registration outlives environment recycling, but never its owning client lease."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.python.host :as python-host])
  (:import [java.util UUID]))

(defonce ^:private registrations (atom {}))

(def ^:dynamic *call-timeout-ms* 300000)

(defn- refuse! [status code message] (throw (ex-info message {:status status :code code})))

(defn- validate!
  [definition value]
  (when-not (contract/valid-json? "gateway" definition value)
    (refuse! 400 :invalid_client_extension "Invalid client extension payload")))

(defn- owned
  [sid owner]
  (let [registration (get @registrations (str sid))]
    (when-not (and registration (= owner (:owner registration)) ((:live? registration)))
      (refuse! 403 :client_extension_owner "The client does not own these session extensions"))
    registration))

(defn detach!
  "Forget a session's callbacks and wake pending calls. Existing adapters fail closed."
  [sid]
  (locking registrations
    (when-let [registration (get @registrations (str sid))]
      (swap! registrations dissoc (str sid))
      (doseq [[_ call] (:calls registration)]
        (deliver (:signal call) ::detached))))
  nil)

(defn detach-owner!
  "Release every registration held by a disconnected client."
  [owner]
  (locking registrations
    (doseq [[sid registration]
            @registrations

            :when (= owner (:owner registration))]

      (detach! sid)))
  nil)

(defn detach-owned!
  "Refuse cross-owner detach; repeated detach by a live owner is harmless."
  [sid owner]
  (locking registrations (when (get @registrations (str sid)) (owned sid owner) (detach! sid)))
  {})

(defn extensions-for
  "Live declarations for a newly created or recycled session environment."
  [sid]
  (when-let [registration (get @registrations (str sid))]
    (when ((:live? registration)) (:extensions registration))))

(defn- prune-completed
  [calls]
  (let [completed (sort-by (comp :finished val) (filter (comp :finished val) calls))]
    (reduce dissoc calls (map key (take (max 0 (- (count completed) 256)) completed)))))

(defn- invoke!
  [sid owner generation tool args]
  (let [id
        (str (UUID/randomUUID))

        kwargs?
        (::python-host/keyword-arguments (meta (peek args)))

        call
        {:wire
         {:id id :name tool :args (if kwargs? (pop args) args) :kwargs (if kwargs? (peek args) {})}
         :signal (promise)
         :activity (atom nil)}

        deadline
        (+ (System/nanoTime) (* (long *call-timeout-ms*) 1000000))]

    (locking registrations
      (let [registration (owned sid owner)]
        (when-not (= generation (:generation registration))
          (refuse! 410 :client_call_gone "Application registration has been detached"))
        (when (>= (count (remove (comp :finished val) (:calls registration))) 64)
          (refuse! 429 :client_call_limit "Too many pending application calls"))
        (swap! registrations assoc-in [(str sid) :calls id] call)))
    (try (loop []

           (when (Thread/interrupted) (throw (InterruptedException. "Application call cancelled")))
           (owned sid owner)
           (when-let [presentation (first (reset-vals! (:activity call) nil))]
             (extension/publish-activity! presentation))
           (let [result (deref (:signal call) 50 ::waiting)]
             (cond (= result ::detached) (refuse! 410 :client_call_gone "Application disconnected")
                   (= result ::waiting)
                   (if (< (System/nanoTime) deadline)
                     (recur)
                     (refuse! 410 :client_call_gone "Application call timed out"))
                   :else (do (when-let [presentation (first (reset-vals! (:activity call) nil))]
                               (extension/publish-activity! presentation))
                             (if (= "success" (get result "status"))
                               (extension/success {:result (get result "result")})
                               (extension/failure
                                 {:result nil
                                  :throwable (ex-info (str (get-in result ["error" "type"])
                                                           ": "
                                                           (get-in result ["error" "message"]))
                                                      {})}))))))
         (finally (locking registrations
                    (when (get-in @registrations [(str sid) :calls id])
                      (swap! registrations update-in
                        [(str sid) :calls]
                        (fn [calls]
                          (if (:response (get calls id))
                            (prune-completed (assoc-in calls [id :finished] (System/nanoTime)))
                            (dissoc calls id))))))))))

(defn- extension-declaration
  [sid owner generation spec]
  (let [active?
        (fn [_]
          (let [registration (get @registrations (str sid))]
            (boolean (and registration
                          (= owner (:owner registration))
                          (= generation (:generation registration))
                          ((:live? registration))))))

        symbols
        (mapv (fn [tool]
                (let [tool-name (get tool "name")]
                  (extension/python-symbol-entry
                    (symbol tool-name)
                    tool
                    (fn [& args]
                      (invoke! sid owner generation tool-name (vec args))))))
              (get spec "symbols"))]

    (extension/extension (cond-> {:ext/name (get spec "name")
                                  :ext/description (get spec "description")
                                  :ext/kind (get spec "kind" "python")
                                  :ext/activation-fn active?
                                  :ext/engine {:ext.engine/alias (symbol (get spec "alias"))
                                               :ext.engine/exact-symbol-names? true
                                               :ext.engine/symbols symbols}}
                           (get spec "prompt")
                           (assoc :ext/prompt-fn (get spec "prompt"))

                           (get spec "version")
                           (assoc :ext/version (get spec "version"))))))

(defn- public-names
  [ext]
  (map (fn [entry]
         (let [n
               (str (:ext.symbol/symbol entry))

               alias
               (extension/ext-alias-symbol ext)]

           (if (and alias (not (extension/ext-exact-symbol-names? ext)))
             (str (name alias) "_" (str/replace n "-" "_"))
             (str/replace n "-" "_"))))
       (extension/ext-symbols ext)))

(defn- names-conflict?
  [a b]
  (or (= a b) (str/starts-with? a (str b ".")) (str/starts-with? b (str a "."))))

(defn register!
  "Validate an append-only declaration set against an idle session's loaded tools.
   The caller holds its turn lock through installation. live? checks the actual lease."
  [sid owner payload live? environment]
  (validate! "client_extensions" payload)
  (when-not (and (string? owner) (not (str/blank? owner)) (live?))
    (refuse! 403 :client_extension_owner "A live client lease is required"))
  (locking registrations
    (let [sid
          (str sid)

          before
          (get @registrations sid)

          specs
          (get payload "extensions")

          previous
          (into {} (map (juxt #(get % "name") identity) (:specs before)))

          by-name
          (into {} (map (juxt #(get % "name") identity) specs))]

      (when (and before (not= owner (:owner before)))
        (refuse! 403 :client_extension_owner "Another client owns these session extensions"))
      (when (or (not= (count specs) (count by-name))
                (not-every? (fn [[k v]]
                              (= v (get by-name k)))
                            previous))
        (refuse! 409 :client_extension_conflict "Existing declarations cannot be replaced"))
      (let [generation
            (or (:generation before) (str (UUID/randomUUID)))

            existing
            (remove #(contains? previous (:ext/name %)) @(:extensions environment))

            previous-extensions
            (into {} (map (juxt :ext/name identity) (:extensions before)))

            declarations
            (mapv #(or (get previous-extensions (get % "name"))
                       (extension-declaration sid owner generation %))
                  specs)

            names
            (vec (mapcat public-names declarations))

            installed-names
            (mapcat public-names existing)]

        (when (or (some #(contains? by-name (:ext/name %)) existing)
                  (not= (count specs) (count (set (map #(get % "alias") specs))))
                  (some (fn [[i a]]
                          (some #(names-conflict? a %) (subvec names (inc i))))
                        (map-indexed vector names))
                  (some (fn [a]
                          (some #(names-conflict? a %) installed-names))
                        names))
          (refuse! 409 :client_extension_conflict "Extension or symbol name already exists"))
        (swap! registrations assoc
          sid
          {:owner owner
           :generation generation
           :live? live?
           :specs specs
           :extensions declarations
           :calls (or (:calls before) {})})
        declarations))))

(defn pending
  "Read pending invocations for exactly one live owning client, without consuming them."
  [sid owner]
  (locking registrations
    {:calls (mapv :wire (remove :response (vals (:calls (owned sid owner)))))}))

(defn complete!
  "Deliver once, acknowledging identical retries and refusing conflicting outcomes."
  [sid owner id payload]
  (validate! "client_call_result" payload)
  (when (> (alength (.getBytes ^String (wire/json-str payload)
                               java.nio.charset.StandardCharsets/UTF_8))
           1048576)
    (refuse! 413 :client_call_result_limit "Application result exceeds 1 MiB"))
  (locking registrations
    (let [registration
          (owned sid owner)

          call
          (get-in registration [:calls id])]

      (when-not call (refuse! 410 :client_call_gone "Application call is no longer pending"))
      (when (and (:response call) (not= payload (:response call)))
        (refuse! 409 :client_call_conflict "Application call already has a different result"))
      (swap! registrations assoc-in [(str sid) :calls id :response] payload)
      (deliver (:signal call) payload)))
  {})

(defn activity!
  "Publish presentation data; lifecycle and invocation identity remain host-owned."
  [sid owner id presentation]
  (when-not (contract/valid-json? "activity" "presentation" presentation)
    (refuse! 400 :invalid_client_activity "Invalid Activity presentation"))
  (locking registrations
    (let [call (get-in (owned sid owner) [:calls id])]
      (when (or (nil? call) (:response call))
        (refuse! 410 :client_call_gone "Application call is no longer pending"))
      (reset! (:activity call) presentation)))
  {})
