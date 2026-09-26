(ns com.blockether.vis.internal.gateway.server.turns
  "Turn routes: submit, list, edit and cancel turns, drive the turn queue, read
   traces, and upload or fetch attachments."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.attachment.audio-transcribe :as audio-transcribe]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io InputStream]
           [java.util Base64 UUID]))

(def ^:private upload-ttl-ms
  "How long a binary upload may wait for its tiny turn submission. Long enough for
   a phone retry, short enough that an abandoned picker cannot pin image bytes."
  (* 10 60 1000))

(def ^:private pending-upload-budget-bytes (* 320 1024 1024))

(def ^:private pending-upload-count-limit 64)

(defonce ^:private pending-uploads (atom {}))

(defn reap-uploads!
  [^long now]
  (swap! pending-uploads (fn [uploads]
                           (into {}
                                 (remove (fn [[_ {:keys [created-at]}]]
                                           (> (- now (long created-at)) (long upload-ttl-ms))))
                                 uploads))))

(defn- store-upload!
  [upload-id upload]
  (swap! pending-uploads (fn [uploads]
                           (loop [kept uploads]
                             (let [total (reduce +
                                                 0
                                                 (map (fn [{:keys [bytes]}]
                                                        (alength ^bytes bytes))
                                                      (vals kept)))]
                               (if (and (< (count kept) (long pending-upload-count-limit))
                                        (<= (+ total (alength ^bytes (:bytes upload)))
                                            (long pending-upload-budget-bytes)))
                                 (assoc kept upload-id upload)
                                 (let [[oldest-id] (apply min-key
                                                     (fn [[id value]]
                                                       [(:created-at value) id])
                                                     kept)]
                                   (recur (dissoc kept oldest-id)))))))))

(defn- upload-limit
  [media-type]
  (cond (str/starts-with? (str media-type) "video/") attachments/max-video-bytes
        (str/starts-with? (str media-type) "audio/") attachments/max-audio-bytes
        :else attachments/max-upload-image-bytes))

(defn- upload->attachment
  [{:keys [filename media-type bytes]}]
  {:filename filename
   :media-type media-type
   :size (alength ^bytes bytes)
   :base64 (.encodeToString (Base64/getEncoder) ^bytes bytes)})

(defn- refresh-upload-transcription
  "Start or collect the local words for one staged upload without waiting."
  [upload]
  (first (audio-transcribe/request-attachments! [(upload->attachment upload)])))

(defn- upload-attachment-handler
  "POST raw attachment bytes, returning an opaque id for a later turn submission."
  [request]
  (let [sid
        (http/path-sid request)

        filename
        (http/query-str request "filename")

        media-type
        (http/query-str request "media_type")

        limit
        (long (upload-limit media-type))

        declared
        (some-> (get-in request [:headers "content-length"])
                parse-long)]

    (cond
      (or (nil? sid) (nil? (state/soul sid))) (http/session-404 (get-in request
                                                                        [:path-params :sid]))
      (or (str/blank? filename) (str/blank? media-type))
      (http/error-response 400 :invalid-request "filename and media_type are required")
      (and declared (> (long declared) limit))
      (http/error-response 413 :attachment-too-large "attachment exceeds the upload limit")
      :else
      (let [^bytes bytes (.readNBytes ^InputStream (:body request) (inc limit))]
        (if (> (alength bytes) limit)
          (http/error-response 413 :attachment-too-large "attachment exceeds the upload limit")
          (let [now (util/now-ms)
                upload-id (str (UUID/randomUUID))
                upload
                {:sid sid :filename filename :media-type media-type :bytes bytes :created-at now}]

            (reap-uploads! now)
            (store-upload! upload-id upload)
            ;; Upload completion is the earliest instant the gateway owns the whole
            ;; recording. Start its local transcript here, before the tiny turn POST.
            (refresh-upload-transcription upload)
            (http/json-response 201 {:upload_id upload-id :size (alength bytes)})))))))

(defn- resolve-upload-attachments
  [sid rows]
  (mapv (fn [row]
          (if-let [upload-id (or (get row "upload_id") (get row :upload-id))]
            (when-let [{stored-sid :sid :as upload} (@pending-uploads upload-id)]
              (when (= sid stored-sid)
                (cond-> (refresh-upload-transcription upload)
                  (attachments/image-reference row)
                  (assoc :reference (attachments/image-reference row)))))
            row))
        (or rows [])))

(defn- path-tid [request] (get-in request [:path-params :tid]))

(defn- configured-reasoning-level
  "The shared `reasoning_level` toggle as a plain wire string (`quick` /
   `balanced` / `deep`), or nil when it is unreadable.

   The gateway is the ONLY reasoning source for channels that do not send
   `reasoning_default` themselves (the companion app, plain HTTP clients): the
   TUI reads the same toggle and passes it per turn, so honouring it here makes
   one flip mean the same thing everywhere instead of silently falling back to
   the engine's `balanced`."
  []
  (let [v (try (toggles/value-of "reasoning_level") (catch Throwable _ nil))]
    (cond (keyword? v) (name v)
          (string? v) (not-empty v)
          :else nil)))

(defn- submit-turn-handler
  [request]
  (let [sid
        (http/path-sid request)

        body
        (http/body-json request)]

    (if (nil? sid)
      (http/session-404 (get-in request [:path-params :sid]))
      (let [;; READ-ONLY: an archived session - its own stamp, or the stamp on the group
            ;; holding it - takes no new work, and nothing is resolved or queued on its
            ;; behalf. The app and the TUI disable their composer, so this answers a
            ;; stale screen or a caller coming straight through the SDK.
            archived?
            (state/session-archived? sid)

            attachments
            (when-not archived? (resolve-upload-attachments sid (get body "attachments")))

            missing-upload?
            (some nil? attachments)

            result
            (when-not (or archived? missing-upload?)
              (state/submit-turn! sid
                                  {:request (get body "request")
                                   :idempotency-key (get body "idempotency_key")
                                   :provider (get body "provider")
                                   :model (get body "model")
                                   :reasoning-default (or (get body "reasoning_default")
                                                          (configured-reasoning-level))
                                   :extra-body (get body "extra_body")
                                   :turn-features (get body "turn_features")
                                   :workspace (get body "workspace")
                                   :attachments attachments
                                   ;; The submitter's own pre-expansion prose. Dropping it here
                                   ;; is what made a queued image render as a raw /var/folders path.
                                   :display-request (get body "display_request")}))]

        (cond archived? (http/session-archived-409 sid)
              missing-upload?
              (http/error-response 400 :invalid-upload "attachment upload is missing or expired")
              (:turn result) (http/json-response (if (:idempotent? result) 200 202) (:turn result))
              (= :turn-in-progress (:error result))
              (http/error-response 409
                                   :turn-in-progress "session already has a running turn"
                                   :session_id (str sid)
                                   :turn_id (:turn-id result))
              (= :session-not-found (:error result)) (http/session-404 (str sid))
              :else (http/error-response 400
                                         :invalid-request
                                         (or (:message result) "invalid request")))))))

(defn- list-turns-handler
  "GET the session's turns. `?status=queued` narrows the response to the live
   queued backlog — the tray's poll — which is served straight from the
   registry overlay with no turn-history hydration. Without the filter this
   ships the FULL history (every completed turn's content), so a poller that
   only wants the backlog must pass it."
  [request]
  (let [sid
        (http/path-sid request)

        queued-only?
        (= "queued" (get-in request [:query-params "status"]))]

    (if (and sid (state/soul sid))
      (http/json-response
        (cond-> {:turns (if queued-only? (state/list-queued-turns sid) (state/list-turns sid))}
          queued-only?
          (assoc :queue-paused (state/queue-paused-info sid))))
      (http/session-404 (get-in request [:path-params :sid])))))

(defn- get-turn-handler
  [request]
  (let [sid
        (http/path-sid request)

        tid
        (path-tid request)]

    (if-let [turn (and sid (state/get-turn sid tid))]
      (http/json-response turn)
      (http/error-response 404 :turn-not-found "unknown turn" :turn_id tid))))

(defn- update-queued-turn-handler
  [request]
  (let [sid
        (http/path-sid request)

        tid
        (path-tid request)

        result
        (if sid
          (state/update-queued-turn! sid tid (get (http/body-json request) "request"))
          {:error :turn-not-found})]

    (cond (:turn result) (http/json-response (:turn result))
          (= :turn-not-found (:error result))
          (http/error-response 404 :turn-not-found "unknown turn" :turn_id tid)
          :else (http/error-response 409
                                     (or (:error result) :not-queued)
                                     (or (:message result) "turn is not queued")
                                     :turn_id tid
                                     :turn_status (:status result)))))

(defn- delete-queued-turn-handler
  [request]
  (let [sid
        (http/path-sid request)

        tid
        (path-tid request)

        result
        (if sid (state/delete-queued-turn! sid tid) {:error :turn-not-found})]

    (cond (= "deleted" (:status result)) (http/json-response 200 result)
          (= :turn-not-found (:error result))
          (http/error-response 404 :turn-not-found "unknown turn" :turn_id tid)
          :else (http/error-response 409
                                     (or (:error result) :not-queued)
                                     "turn is not queued"
                                     :turn_id tid
                                     :turn_status (:status result)))))

(defn- cancel-turn-handler
  [request]
  (let [sid
        (http/path-sid request)

        tid
        (path-tid request)

        result
        (if sid (state/cancel-turn! sid tid) {:error :turn-not-found})]

    (cond (:status result) (http/json-response 202 result)
          (= :turn-not-found (:error result))
          (http/error-response 404 :turn-not-found "unknown turn" :turn_id tid)
          :else (http/error-response 409
                                     :not-running "turn is not running"
                                     :turn_id tid
                                     :turn_status (:status result)))))

(defn- cancel-current-turn-handler
  "POST /sessions/:sid/cancel-current {idempotency_key} — tid-less cancel: fire
   the cancel token of the turn holding the session's `:current-turn`, iff the
   caller submitted it under `idempotency_key`. For clients that lost the turn id
   (Esc before `turn.started` bound it, or a cancel self-heal that dropped it) —
   the id-addressed `/turns/:tid/cancel` is useless to them and the still-running
   ghost queues every next submit. A session is shared, so the correlation id is
   what keeps this route from killing another channel's work. 202 + `{:status
   \"cancelling\" :turn_id tid}`, 409 `:not-owner` for someone else's turn, 409
   `:no-running-turn` when idle."
  [request]
  (let [sid
        (http/path-sid request)

        owner-key
        (get (try (http/body-json request) (catch Throwable _ nil)) "idempotency_key")]

    (if (and sid (state/soul sid))
      (let [result (state/cancel-current-turn! sid owner-key)]
        (cond (:status result) (http/json-response 202 result)
              (= :no-running-turn (:error result))
              (http/error-response 409 :no-running-turn "session has no running turn")
              (= :not-owner (:error result))
              (http/error-response 409
                                   :not-owner "the running turn was submitted by another client"
                                   :turn_id (:turn_id result))
              (= :turn-not-found (:error result))
              (http/error-response 404 :turn-not-found "unknown turn")
              :else (http/error-response 409
                                         :not-running "turn is not running"
                                         :turn_status (:status result))))
      (http/session-404 (get-in request [:path-params :sid])))))

(defn- drain-idle-handler
  "POST /sessions/:sid/drain-queue — start the session's oldest queued turn iff
   it is idle. Returns `{:turn <started>|nil}`; nil turn means nothing was
   queued or a turn is already running (both benign)."
  [request]
  (let [sid (http/path-sid request)]
    (if (and sid (state/soul sid))
      (http/json-response {:turn (state/drain-idle! sid)})
      (http/session-404 (get-in request [:path-params :sid])))))

(defn- resume-queue-handler
  "POST /sessions/:sid/resume-queue — clear a queue PAUSED by a provider failure
   and start its head. An explicit resume also resets the failure breaker.
   Returns `{:turn <started>|nil}`; nil turn means the queue was not paused."
  [request]
  (let [sid (http/path-sid request)]
    (if (and sid (state/soul sid))
      (http/json-response {:turn (state/resume-queue! sid {:auto? false})})
      (http/session-404 (get-in request [:path-params :sid])))))

(defn- turn-trace-handler
  [request]
  (if (http/path-sid request)
    (http/json-response {:iterations (state/turn-trace (http/path-sid request)
                                                       (get-in request [:path-params :tid]))})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- path-iid [request] (get-in request [:path-params :iid]))

(defn- path-idx
  [request]
  (some-> (get-in request [:path-params :idx])
          parse-long))

(defn- attachment-bytes-handler
  "GET /v1/sessions/:sid/iterations/:iid/attachments/:idx — the raw bytes of ONE
   outbound artifact (a produced image or document) a tool call emitted
   in iteration `:iid`, addressed by its 0-based `:idx` in the iteration's ordered
   attachment list — the SAME list (and order) the live `iteration.completed`
   descriptors index. Served with the artifact's own Content-Type so a native
   client (iOS/RN) `<img>`-loads it directly; the lazy fetch that keeps the live
   SSE frame lean. HISTORY and LIVE resolve through the SAME durable DB rows, so
   it works the instant the iteration is stored and forever after (404 until the
   row lands — the client retries, as the trace re-fetch already does). An
   attachment is append-only + content-addressed by (iteration, index), so it is
   safely `immutable`-cacheable."
  [request]
  (if (http/path-sid request)
    (let [idx
          (path-idx request)

          atts
          (state/user-iteration-attachments (path-iid request))

          att
          (when (and idx (nat-int? idx)) (nth atts idx nil))

          ^bytes bs
          (some-> att
                  state/attachment-bytes)]

      (if bs
        {:status 200
         :headers {"Content-Type" (or (not-empty (str (:media-type att)))
                                      "application/octet-stream")
                   "Content-Length" (str (alength bs))
                   "Cache-Control" "private, max-age=31536000, immutable"}
         :body (java.io.ByteArrayInputStream. bs)}
        (http/error-response 404
                             :attachment-not-found "unknown attachment"
                             :iteration_id (str (path-iid request))
                             :index idx)))
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- append-attachment-handler
  "POST /v1/sessions/:sid/iterations/:iid/attachments — a HUMAN's revision of an
   artifact the model produced, stored into the iteration that produced it:
   `{\"filename\": \"notes.md\", \"media_type\": \"text/markdown\",
   \"base64\": \"…\"}`.

   The filename is the identity, so re-sending the name is the NEXT VERSION of
   that artifact and not a second file beside it — the annotated note the
   companion saves is `v2` of the note it was reading. Answers with the same
   descriptor shape the transcript and the byte endpoint already speak, so the
   client re-reads the revision through the paths it already has."
  [request]
  (if-let [sid (http/path-sid request)]
    (let [body (http/body-json request)
          filename (some-> (get body "filename")
                           str
                           str/trim)
          base64 (get body "base64")]

      (if (or (str/blank? filename) (str/blank? (str base64)))
        (http/error-response 400 :invalid-attachment "filename and base64 are required")
        (try (if-let [descriptor (state/revise-iteration-attachment!
                                   sid
                                   (path-iid request)
                                   {:filename filename
                                    :media-type (or (not-empty (str (get body "media_type")))
                                                    "application/octet-stream")
                                    :base64 (str base64)})]
               (http/json-response 201 descriptor)
               (http/error-response 404
                                    :attachment-not-stored "unknown iteration"
                                    :iteration_id (str (path-iid request))))
             (catch clojure.lang.ExceptionInfo e
               (case (:type (ex-data e))
                 :attachment/not-found
                 (http/error-response 404 :attachment-not-found (ex-message e))

                 :attachment/read-only
                 (http/error-response 403 :attachment-read-only (ex-message e))

                 :attachment/invalid-revision
                 (http/error-response 400 :invalid-attachment (ex-message e))

                 (throw e))))))
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- turn-attachments-handler
  "GET /v1/sessions/:sid/turns/:tid/attachments — the inline media a USER sent
   with one turn. `?transcription_only=true` omits the large base64 body when a
   client already has the bytes and is only collecting local speech results.

   The live rail deliberately ships byte-free chips, and a turn's persisted row
   only exists once it lands, so before this endpoint the only copy of a
   still-running turn's media was the sending client's own memory — an app
   restart, or a second device, painted the message with its media missing.
   The gateway has held it the whole time (registry entry while in flight,
   attachment store afterwards); this hands it back on demand, which is why it
   is a SEPARATE endpoint and not a fatter turn row."
  [request]
  (if-let [sid (http/path-sid request)]
    (let [tid (path-tid request)
          rows (state/turn-attachments sid tid)
          original (wire/->engine rows)
          refreshed (audio-transcribe/request-attachments! original)
          ;; A cancelled/finished turn can outlive its joiner. Collecting its words
          ;; must repair the durable row too, not only this one HTTP response.
          _ (doseq [[before after] (map vector original refreshed)
                    :let [position (:position before)
                          words (:transcription after)]
                    ;; Live/deduplicated rails have no durable ordinal. Their joiner
                    ;; stores words from the original vector; only stored rows repair here.
                    :when (and (some? position)
                               (not (str/blank? words))
                               (not= words (:transcription before)))]

              (persistance/db-set-turn-attachment-transcription! (lp/db-info)
                                                                 tid
                                                                 position
                                                                 words
                                                                 (:transcription-segments after)))
          response (if (= "true" (http/query-str request "transcription_only"))
                     (mapv #(dissoc % :base64) refreshed)
                     refreshed)]

      (http/json-response {:attachments response}))
    (http/session-404 (get-in request [:path-params :sid]))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:post "/v1/sessions/:sid/iterations/:iid/attachments"] append-attachment-handler
   [:get "/v1/sessions/:sid/iterations/:iid/attachments/:idx"] attachment-bytes-handler
   [:post "/v1/sessions/:sid/attachments"] upload-attachment-handler
   [:get "/v1/sessions/:sid/turns"] list-turns-handler
   [:post "/v1/sessions/:sid/turns"] submit-turn-handler
   [:get "/v1/sessions/:sid/turns/:tid"] get-turn-handler
   [:patch "/v1/sessions/:sid/turns/:tid"] update-queued-turn-handler
   [:delete "/v1/sessions/:sid/turns/:tid"] delete-queued-turn-handler
   [:get "/v1/sessions/:sid/turns/:tid/trace"] turn-trace-handler
   [:get "/v1/sessions/:sid/turns/:tid/attachments"] turn-attachments-handler
   [:post "/v1/sessions/:sid/turns/:tid/cancel"] cancel-turn-handler
   [:post "/v1/sessions/:sid/cancel-current"] cancel-current-turn-handler
   [:post "/v1/sessions/:sid/drain-queue"] drain-idle-handler
   [:post "/v1/sessions/:sid/resume-queue"] resume-queue-handler})
