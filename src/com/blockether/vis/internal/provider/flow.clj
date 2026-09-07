(ns com.blockether.vis.internal.provider.flow
  "Gateway-owned authentication lifecycle shared by MCP and model adapters.

   `start!` takes an owner `[domain id]` and protocol legs `:start` (0-arg),
   `:complete` (private flow, input), `:await` (private flow), `:settle` (0-arg).
   Start returns `:kind`, private `:flow`, and allowlisted presentation fields.
   An adapter doing dynamic registration can allocate `callback-transport!`
   before constructing its authorization URL; all other adapters let us do it.

   One live attempt per owner. All completion paths serialize on the same state;
   terminal verdicts remain pollable until expiry/cancellation. Private flow data
   and adapter results never cross the public allowlist. No browser or relay here.
   Cancellation stops workers and rejects late verdicts; adapters must also obey
   interruption/expiry before persisting credentials during an in-flight exchange."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.provider.callback :as callback]
            [com.blockether.vis.internal.util :as util]))

(set! *unchecked-math* :warn-on-boxed)

(def app-callback-uri "com.blockether.viscompanion://oauth/callback")

(def ^:private ttl-ceiling-ms 900000)

(defonce ^:private flows (atom {}))

(defn callback-transport!
  "Allocate only a registered loopback or Vis app callback. Returns public mode/URI
   plus private `:callback`. Dynamic registration must use the returned URI for both
   authorization and exchange, and stop the receiver if preparation fails."
  [mode redirect state ttl-ms]
  (when-not (contains? #{nil "loopback" "app" "manual"} mode)
    (throw (ex-info "Unsupported OAuth callback mode" {})))
  (when (and (= "app" mode) (not= app-callback-uri redirect))
    (throw (ex-info "Unsupported app callback destination" {})))
  (let [receiver (when (and redirect (contains? #{nil "loopback"} mode))
                   (try (callback/listen! redirect state ttl-ms)
                        (catch java.net.BindException _ nil)))]
    {:callback receiver
     :redirect-uri (or (:redirect-uri receiver) redirect)
     :callback-mode (cond (= "app" mode) "app"
                          receiver "loopback"
                          :else "manual")}))

(defn- expired? [{:keys [expires-at]}] (>= (util/now-ms) (long expires-at)))

(defn- live?
  [{:keys [id] :as entry}]
  (and (identical? entry (get @flows id)) (not (expired? entry))))

(defn- stop-receiver!
  [entry]
  (when-let [stop! (get-in entry [:public :callback :stop!])]
    (stop!)))

(defn- dispose!
  [entry]
  (stop-receiver! entry)
  (when-let [worker @(:worker entry)]
    (future-cancel worker)))

(defn- drop-where!
  [pred]
  (let [[before _] (swap-vals! flows #(into {} (remove (comp pred val)) %))]
    (run! dispose! (filter pred (vals before)))))

(defn cancel-owner!
  "Stop every attempt for this exact domain/subject, including on logout."
  [owner]
  (drop-where! #(= owner (:owner %))))

(defn- lookup
  [domain id]
  (drop-where! expired?)
  (let [entry (get @flows id)]
    (when (= domain (first (:owner entry))) entry)))

(defn- view
  [{:keys [id owner kind public expires-at result]}]
  (merge (select-keys public
                      [:url :redirect-uri :callback-mode :user-code :verification-uri :interval-ms
                       :instructions])
         {:flow-id id :subject (second owner) :kind (name kind) :expires-at expires-at}
         (update @result :status #(if (= :exchanging %) "pending" (name %)))))

(defn- verdict
  [entry]
  (let [public (view entry)]
    (merge {:ok? true :flow public} (select-keys public [:status :message]))))

(def ^:private unknown-flow
  {:ok? false :error :unknown-flow :message "Unknown or completed authorization flow."})

(defn- run-exchange!
  [{:keys [result settle] :as entry} exchange retry?]
  (locking result
    (if-not (and (live? entry) (= :pending (:status @result)))
      unknown-flow
      (do (reset! result {:status :exchanging})
          (try (exchange)
               (if-not (live? entry)
                 unknown-flow
                 (do (when settle (settle))
                     (if-not (live? entry)
                       unknown-flow
                       (do (reset! result {:status :ok}) (stop-receiver! entry) (verdict entry)))))
               (catch Throwable _
                 (when (live? entry)
                   (reset! result (if retry?
                                    {:status :pending}
                                    {:status :error
                                     :message "Authorization failed. Start sign-in again."}))
                   (when-not retry? (stop-receiver! entry)))
                 {:ok? false
                  :error :auth-failed
                  :message "Authorization failed. Check the input or start sign-in again."}))))))

(defn- complete-entry!
  [{:keys [kind public flow complete] :as entry} input automatic?]
  (let [value
        (when (string? input) (str/trim input))

        api-key?
        (= :api-key kind)]

    (cond (nil? complete)
          {:ok? false :error :auth-unsupported :message "This flow finishes by polling."}
          (or (nil? input) (= "" value))
          {:ok? false :error :missing-input :message "Authorization input is required."}
          (or (nil? value) (> (count value) 8192) (and api-key? (re-find #"\s" value)))
          {:ok? false
           :error :invalid-input
           :message "Authorization input must be a string of at most 8192 characters."}
          (and (not api-key?)
               (:redirect-uri public)
               (or (= "app" (:callback-mode public))
                   (str/includes? value "://")
                   (str/includes? value "?"))
               (not (callback/response-uri? (:redirect-uri public) (:state flow) value)))
          {:ok? false
           :error :invalid-input
           :message "Callback URL does not match this sign-in flow."}
          :else (run-exchange! entry
                               #(complete flow value)
                               (not (or automatic? (= "app" (:callback-mode public))))))))

(defn- launch!
  [entry task]
  (let [worker (future (task))]
    (reset! (:worker entry) worker)
    (when-not (live? entry) (future-cancel worker))))

(defn start!
  "Start one adapter through the common lifecycle. Returns only a public result.
   Domain scopes prevent one API's completion/cancel route from spending another's flow."
  [owner {:keys [start complete await settle]}]
  (drop-where! expired?)
  (let [id
        (str (java.util.UUID/randomUUID))

        reservation
        {:id id
         :owner owner
         :expires-at (+ (util/now-ms) (long ttl-ceiling-ms))
         :result (atom {:status :pending})
         :worker (atom nil)}

        [before _]
        (swap-vals! flows
                    #(assoc (into {}
                                  (remove (fn [[_ e]]
                                            (= owner (:owner e))))
                                  %)
                       id reservation))]

    (run! dispose! (filter #(= owner (:owner %)) (vals before)))
    (try
      (let [{:keys [kind flow expires-in-ms] :as started}
            (start)

            kind
            (or kind :pkce)

            ttl
            (max 1 (min (long ttl-ceiling-ms) (long (or expires-in-ms ttl-ceiling-ms))))

            public
            (if (and (= :pkce kind) (not (contains? started :callback)))
              (merge started
                     (callback-transport! (:callback-mode started)
                                          (:redirect-uri started)
                                          (:state flow)
                                          ttl))
              started)

            entry
            (assoc reservation
              :kind kind
              :flow flow
              :public public
              :complete complete
              :settle settle
              :expires-at (+ (util/now-ms) ttl))]

        ;; Preparation can do network IO. An older, slower request must not replace
        ;; a newer attempt or resurrect itself after cancellation.
        (swap! flows #(if (and (identical? reservation (get % id)) (not (expired? reservation)))
                        (assoc % id entry)
                        %))
        (if-not (live? entry)
          (do (dispose! entry) unknown-flow)
          (do (cond (and (= :device kind) await) (launch! entry
                                                          #(run-exchange! entry
                                                                          (fn []
                                                                            (await flow))
                                                                          false))
                    (get-in public [:callback :result])
                    (launch! entry
                             #(let [input @(get-in public [:callback :result])]
                                (cond (string? input) (complete-entry! entry input true)
                                      (and (= :expired input) (live? entry))
                                      (reset! (:result entry)
                                        {:status :error
                                         :message
                                         "Authorization timed out. Start sign-in again."})))))
              (verdict entry))))
      (catch Throwable t (drop-where! #(= id (:id %))) (throw t)))))

(defn complete!
  "Validate and spend a callback/key once. Failed manual input remains retryable."
  [domain id input]
  (if-let [entry (lookup domain id)]
    (complete-entry! entry input false)
    unknown-flow))

(defn poll!
  "Read the same retained verdict for browser, app and device flows, without blocking."
  [domain id]
  (if-let [entry (lookup domain id)]
    (verdict entry)
    unknown-flow))

(defn cancel!
  "Idempotently forget this domain's flow and stop its listener/worker."
  [domain id]
  (drop-where! #(and (= id (:id %)) (= domain (first (:owner %)))))
  {:ok? true :status "cancelled"})
