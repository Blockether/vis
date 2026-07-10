(ns com.blockether.vis.internal.gateway.client
  "HTTP/SSE client for the long-lived gateway daemon.

   Interactive channels call this facade instead of `gateway.state` directly. It
   discover-or-starts the one daemon for the current DB, then speaks the same
   HTTP/SSE API every other client uses. This is the thin-client half of the
   gateway-daemon plan: token refresh, turn execution, and live streaming happen
   in ONE process."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.gateway.discovery :as discovery]
            [com.blockether.vis.internal.gateway.wire :as wire])
  (:import
    (java.io BufferedReader InputStream InputStreamReader)
    (java.net URI URLEncoder)
    (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
    (java.nio.charset StandardCharsets)
    (java.time Duration)))

(def ^:private DEFAULT_PORT 7890)
(def ^:private DEFAULT_HOST "127.0.0.1")

(defonce ^:private http-client
  (delay (-> (HttpClient/newBuilder)
             (.connectTimeout (Duration/ofSeconds 2))
             (.build))))

(defonce ^:private cached-entry (atom nil))
(defonce ^:private client-id (atom nil))
(defonce ^:private release-hook-installed? (atom false))
(defonce ^:private subscriptions (atom {}))

(defn- db-target [] (config/resolve-db-spec))

(defn- enc [x] (URLEncoder/encode (str x) StandardCharsets/UTF_8))

(defn- base-url
  [{:keys [host port]}]
  (str "http://" (or host DEFAULT_HOST) ":" (or port DEFAULT_PORT)))

(defn- request-builder
  [{:keys [secret] :as entry} path]
  (doto (HttpRequest/newBuilder (URI/create (str (base-url entry) path)))
    (.timeout (Duration/ofSeconds 30))
    (.header "Accept" "application/json")
    (.header "X-Vis-Gateway-Secret" (str secret))))

(defn- parse-json-body [^String body] (or (wire/parse-json body) {}))

(declare ensure-gateway! ensure-client!)

(defn- send-json-with-entry!
  ([entry method path] (send-json-with-entry! entry method path nil))
  ([entry method path body]
   (let [builder
         (request-builder entry path)

         _
         (if (some? body)
           (doto builder
             (.header "Content-Type" "application/json")
             (.method method (HttpRequest$BodyPublishers/ofString (wire/json-str body))))
           (.method builder method (HttpRequest$BodyPublishers/noBody)))

         response
         (.send @http-client (.build builder) (HttpResponse$BodyHandlers/ofString))

         status
         (.statusCode response)

         parsed
         (parse-json-body (.body response))]

     (when (>= status 400)
       (throw (ex-info (or (:message parsed) (str "gateway HTTP " status))
                       (assoc parsed :http-status status))))
     parsed)))

(defn- send-json!
  ([method path] (send-json! method path nil))
  ([method path body]
   (let [entry (ensure-gateway!)]
     (ensure-client! entry)
     (send-json-with-entry! entry method path body))))

(defn- probe-entry?
  [entry]
  (try (let [builder
             (request-builder entry "/healthz")

             response
             (.send @http-client (.build (.GET builder)) (HttpResponse$BodyHandlers/ofString))

             body
             (parse-json-body (.body response))]

         (and (= 200 (.statusCode response)) (= "ok" (:status body)) (true? (:secret_match body))))
       (catch Throwable _ false)))

(defn ensure-gateway!
  "Return a fresh daemon registry entry for the current DB, auto-starting the
   detached gateway if needed. `:memory` is a programmer error for this client;
   headless one-shots stay in-process and should not call here."
  []
  (let [db (db-target)]
    (when (discovery/memory-db? db)
      (throw (ex-info "gateway daemon is disabled for :memory DB" {:type :gateway/no-daemon})))
    (if (discovery/registry-fresh? @cached-entry probe-entry?)
      @cached-entry
      (let [{:keys [entry] :as result} (discovery/discover-or-start!
                                         {:db db :port DEFAULT_PORT :host DEFAULT_HOST}
                                         :probe
                                         probe-entry?)]
        (if entry
          (do (reset! cached-entry entry) entry)
          (throw (ex-info "gateway daemon did not become ready"
                          (assoc result :type :gateway/start-timeout))))))))

(defn- release-client!
  []
  (when-let [cid @client-id]
    (try (when-let [entry @cached-entry]
           (send-json-with-entry! entry "DELETE" (str "/v1/clients/" (enc cid))))
         (catch Throwable _ nil)
         (finally (reset! client-id nil)))))

(defn- ensure-release-hook!
  []
  (when (compare-and-set! release-hook-installed? false true)
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable release-client!))))

(defn- ensure-client!
  "Register this JVM as a daemon client exactly once. This is the refcount lease
   that keeps a detached gateway alive while a TUI/client process is alive; the
   shutdown hook releases it gracefully, and the daemon ignores the lease if this
   pid is killed."
  [entry]
  (when-not @client-id
    (locking client-id
      (when-not @client-id
        (let [{:keys [client_id]} (send-json-with-entry! entry
                                                         "POST"
                                                         "/v1/clients"
                                                         {:pid (discovery/current-pid)
                                                          :kind "clojure-client"})]
          (reset! client-id client_id)
          (ensure-release-hook!)))))
  @client-id)

(defn create-session! [opts] (send-json! "POST" "/v1/sessions" opts))

(defn soul
  [sid]
  (try (send-json! "GET" (str "/v1/sessions/" (enc sid)))
       (catch clojure.lang.ExceptionInfo e
         (when-not (= 404 (:http-status (ex-data e))) (throw e)))))

(defn list-sessions ([] (:sessions (send-json! "GET" "/v1/sessions"))) ([_channel] (list-sessions)))

(defn close-session! [sid] (send-json! "DELETE" (str "/v1/sessions/" (enc sid))))

(defn release-session!
  "Release a session VIEW when the owning channel exits: tell the daemon to
   stop the session's background resources (shell_bg children, REPLs) and drop
   its live runtime, then release the process-level client lease. This is NOT
   a per-session delete (the transcript stays resumable) and never sends daemon
   shutdown; the daemon stops itself only when refcount AND running-turn-count
   hit zero. Best-effort and never daemon-spawning — if no fresh daemon is
   registered there is nothing to release against."
  [sid]
  (when sid
    (try (let [entry (or @cached-entry (discovery/read-registry (db-target)))]
           (when (discovery/registry-fresh? entry probe-entry?)
             (send-json-with-entry! entry "POST" (str "/v1/sessions/" (enc sid) "/release"))))
         (catch Throwable _ nil)))
  (release-client!))

(defn get-turn
  [sid tid]
  (try (send-json! "GET" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid)))
       (catch clojure.lang.ExceptionInfo e
         (when-not (= 404 (:http-status (ex-data e))) (throw e)))))

(defn list-turns [sid] (:turns (send-json! "GET" (str "/v1/sessions/" (enc sid) "/turns"))))

(defn transcript [sid] (:turns (send-json! "GET" (str "/v1/sessions/" (enc sid) "/transcript"))))

(defn context-snapshot [sid] (send-json! "GET" (str "/v1/sessions/" (enc sid) "/context")))

(defn session-model [sid] (:model (send-json! "GET" (str "/v1/sessions/" (enc sid) "/model"))))

(defn session-model-cached [sid] (session-model sid))

(defn set-session-model!
  [sid provider model]
  (:model (send-json! "PATCH"
                      (str "/v1/sessions/" (enc sid) "/model")
                      {:provider provider :model model})))

(defn session-workspace-info
  [sid]
  (:workspace (send-json! "GET" (str "/v1/sessions/" (enc sid) "/workspace"))))

(defn submit-turn!
  [sid opts]
  (let [res (send-json! "POST" (str "/v1/sessions/" (enc sid) "/turns") opts)]
    (if (:turn_id res) {:turn res} res)))

(defn update-queued-turn!
  [sid tid request]
  (send-json! "PATCH" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid)) {:request request}))

(defn delete-queued-turn!
  [sid tid]
  (send-json! "DELETE" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid))))

(defn cancel-turn!
  [sid tid]
  (send-json! "POST" (str "/v1/sessions/" (enc sid) "/turns/" (enc tid) "/cancel")))

(defn reconcile-running-turns!
  "Clients do not sweep. Only the daemon may reconcile its own startup orphans."
  []
  nil)

(defn status
  []
  (let [db
        (db-target)

        entry
        (discovery/read-registry db)]

    (if (discovery/registry-fresh? entry probe-entry?)
      (send-json-with-entry! entry "GET" "/v1/admin/status")
      {:status "stopped" :db (when-not (discovery/memory-db? db) (str (discovery/db-target db)))})))

(defn stop-daemon!
  []
  (let [db
        (db-target)

        entry
        (discovery/read-registry db)]

    (if (discovery/registry-fresh? entry probe-entry?)
      (let [res (send-json-with-entry! entry "POST" "/v1/admin/stop")]
        (reset! cached-entry nil)
        (reset! client-id nil)
        res)
      {:status "stopped" :stopping false})))

(defn current-seq [sid] (:seq (send-json! "GET" (str "/v1/sessions/" (enc sid) "/seq"))))

(defn events-since
  [sid cursor]
  (:events (send-json! "GET"
                       (str "/v1/sessions/" (enc sid)
                            "/events-since?cursor=" (long (or cursor 0))))))

(defn- ir-from-wire
  "Undo gateway.wire's keyword->string JSON coercion for canonical IR vectors."
  [x]
  (cond (vector? x) (mapv (fn [i v]
                            (if (and (zero? i) (string? v)) (keyword v) (ir-from-wire v)))
                          (range)
                          x)
        (map? x) (into {}
                       (map (fn [[k v]]
                              [(if (string? k) (keyword (str/replace k "_" "-")) k)
                               (ir-from-wire v)]))
                       x)
        (sequential? x) (mapv ir-from-wire x)
        :else x))

(defn- terminal-event->result
  [event fallback-turn-id]
  (let [failed?
        (or (= "turn.failed" (:type event)) (= "failed" (:status event)))

        cancelled?
        (= "cancelled" (:status event))

        needs-input?
        (or (true? (:needs_input event)) (= "suspended" (:status event)))

        answer
        (or (:answer event) (:answer_md event))]

    (cond-> {:answer answer
             :answer-ir (some-> (:answer_ir event)
                                ir-from-wire)
             :iteration-count (or (:iteration_count event) 1)
             :duration-ms (:duration_ms event)
             :session-turn-id (or (:engine_turn_id event) fallback-turn-id)
             :utilization (:utilization event)}
      (:model event)
      (assoc :model (:model event))

      (:provider event)
      (assoc :provider (:provider event))

      (:llm_selected event)
      (assoc :llm-selected (:llm_selected event))

      (:llm_actual event)
      (assoc :llm-actual (:llm_actual event))

      (some? (:llm_fallback event))
      (assoc :llm-fallback? (:llm_fallback event))

      (seq (:llm_routing_trace event))
      (assoc :llm-routing-trace (:llm_routing_trace event))

      (:tokens event)
      (assoc :tokens (:tokens event))

      (:cost event)
      (assoc :cost (:cost event))

      (:confidence event)
      (assoc :confidence (:confidence event))

      needs-input?
      (assoc :status :needs-input)

      cancelled?
      (assoc :status :cancelled)

      failed?
      (assoc :error (or (:error event) (:answer_md event) "turn failed")))))

(defn- sse-request
  [sid cursor]
  (let [entry (ensure-gateway!)]
    (-> (request-builder entry
                         (str "/v1/sessions/" (enc sid) "/events?cursor=" (long (or cursor 0))))
        (.header "Accept" "text/event-stream")
        (.GET)
        (.build))))

(defn subscribe!
  "Remote equivalent of gateway.state/subscribe!: start a background SSE reader
   that replays `cursor` then calls `sink` for every live event. Returns an empty
   replay vector because the gateway's SSE endpoint itself handles replay before
   live delivery."
  [sid sub-id sink cursor]
  (let [entry
        (ensure-gateway!)

        _
        (ensure-client! entry)

        stream*
        (atom nil)

        fut
        (future
          (try (let [response (.send @http-client
                                     (sse-request sid cursor)
                                     (HttpResponse$BodyHandlers/ofInputStream))]
                 (when-not (= 200 (.statusCode response))
                   (throw (ex-info (str "gateway SSE HTTP " (.statusCode response))
                                   {:http-status (.statusCode response)})))
                 (with-open [^InputStream in (.body response)
                             rdr (BufferedReader. (InputStreamReader. in StandardCharsets/UTF_8))]

                   (reset! stream* in)
                   (loop [data-lines []]
                     (when-let [line (.readLine rdr)]
                       (if (str/blank? line)
                         (let [data (str/join "
" data-lines)]
                           (when-let [event (when (seq data) (wire/parse-json data))]
                             (sink event))
                           (recur []))
                         (if (str/starts-with? line "data: ")
                           (recur (conj data-lines (subs line 6)))
                           (recur data-lines)))))))
               (catch Throwable _ nil)
               (finally (swap! subscriptions dissoc sub-id))))]

    (swap! subscriptions assoc sub-id {:future fut :stream stream*})
    []))

(defn unsubscribe!
  [_sid sub-id]
  (when-let [{:keys [future stream]} (get @subscriptions sub-id)]
    (try (some-> @stream
                 .close)
         (catch Throwable _ nil))
    (future-cancel future)
    (swap! subscriptions dissoc sub-id))
  nil)

(defn- read-events-until!
  [sid cursor wanted-turn-id on-event]
  (let [response
        (.send @http-client (sse-request sid cursor) (HttpResponse$BodyHandlers/ofInputStream))]
    (when-not (= 200 (.statusCode response))
      (throw (ex-info (str "gateway SSE HTTP " (.statusCode response))
                      {:http-status (.statusCode response)})))
    (with-open [^InputStream in (.body response)
                rdr (BufferedReader. (InputStreamReader. in StandardCharsets/UTF_8))]

      (loop [data-lines []]
        (if-let [line (.readLine rdr)]
          (if (str/blank? line)
            (let [data (str/join "\n" data-lines)
                  event (when (seq data) (wire/parse-json data))]

              (if (and event (= (str (:turn_id event)) (str wanted-turn-id)))
                (do
                  (when on-event (on-event event))
                  (if (contains? #{"turn.completed" "turn.failed"} (:type event)) event (recur [])))
                (recur [])))
            (if (str/starts-with? line "data: ")
              (recur (conj data-lines (subs line 6)))
              (recur data-lines)))
          nil)))))

(defn submit-turn-sync!
  [sid {:keys [on-event] :as opts}]
  (let [submitted
        (submit-turn! sid (dissoc opts :on-event))

        turn
        (:turn submitted)

        turn-id
        (:turn_id turn)]

    (when-let [e (:error submitted)]
      (throw (ex-info (or (:message submitted) (str e)) submitted)))
    (terminal-event->result (read-events-until! sid 0 turn-id on-event) turn-id)))

(defn attach-turn-sync!
  [sid tid {:keys [on-event]}]
  (terminal-event->result (read-events-until! sid 0 tid on-event) tid))
