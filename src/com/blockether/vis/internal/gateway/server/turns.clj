(ns com.blockether.vis.internal.gateway.server.turns
  "Turn routes: submit, list, edit and cancel turns, drive the turn queue, read
   traces, and upload or fetch attachments."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.app :as app]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.attachment.audio-transcribe :as audio-transcribe]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.attachment.storage :as attachment-storage]
            [com.blockether.vis.internal.gateway.server.fs :as fs-api]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.persistance.core :as persistance]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.view.sink :as view-sink])
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
  "The body's attachment rows in the engine's keyword shape. JSON keys convert
   once here; an `upload_id` row becomes its staged upload, or nil when that
   upload is missing, expired or belongs to another session."
  [sid rows]
  (mapv (fn [row]
          (if-let [upload-id (:upload-id row)]
            (when-let [{stored-sid :sid :as upload} (@pending-uploads upload-id)]
              (when (= sid stored-sid)
                (cond-> (refresh-upload-transcription upload)
                  (attachments/image-reference row)
                  (assoc :reference (attachments/image-reference row)))))
            row))
        (wire/->engine (or rows []))))

(defn- path-tid [request] (get-in request [:path-params :tid]))

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
                                   :reasoning-default (get body "reasoning_default")
                                   :extra-body (get body "extra_body")
                                   :turn-features (get body "turn_features")
                                   :workspace (wire/->engine (get body "workspace"))
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
  "PATCH /sessions/:sid/turns/:tid — change a queued turn's text (`request`), its
   delivery mode (`deliver`: `turn_end` or `next_iteration`), or both."
  [request]
  (let [sid
        (http/path-sid request)

        tid
        (path-tid request)

        body
        (http/body-json request)

        changes
        (cond-> {}
          (contains? body "request")
          (assoc :request (get body "request"))

          (contains? body "deliver")
          (assoc :deliver (get body "deliver")))

        result
        (if sid (state/update-queued-turn! sid tid changes) {:error :turn-not-found})]

    (cond (:turn result) (http/json-response (:turn result))
          (= :turn-not-found (:error result))
          (http/error-response 404 :turn-not-found "unknown turn" :turn_id tid)
          (= :invalid-request (:error result))
          (http/error-response 400 :invalid-request (:message result) :turn_id tid)
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

(defn- subagent-stop-refusal
  "The 409 for a stop in subagent session `sid`, or nil for another session. A person stops
   subagents only through their leader: the stop of a leader turn also stops its children."
  [sid]
  (when (and sid (persistance/db-agent-info (lp/db-info) (str sid)))
    (http/error-response 409
                         :subagent-stop "stop the leader session; it also stops its subagents"
                         :session_id (str sid))))

(defn- cancel-turn-handler
  [request]
  (let [sid
        (http/path-sid request)

        tid
        (path-tid request)]

    (or (subagent-stop-refusal sid)
        (let [result (if sid (state/cancel-turn! sid tid) {:error :turn-not-found})]
          (cond (:status result) (http/json-response 202 result)
                (= :turn-not-found (:error result))
                (http/error-response 404 :turn-not-found "unknown turn" :turn_id tid)
                :else (http/error-response 409
                                           :not-running "turn is not running"
                                           :turn_id tid
                                           :turn_status (:status result)))))))

(defn- cancel-current-turn-handler
  "POST /sessions/:sid/cancel-current {idempotency_key} — tid-less cancel: fire
   the cancel token of the turn holding the session's `:current-turn`, iff the
   caller submitted it under `idempotency_key`. For clients that lost the turn id
   (Esc before `turn.started` bound it, or a cancel self-heal that dropped it) —
   the id-addressed `/turns/:tid/cancel` is useless to them and the still-running
   ghost queues every next submit. A session is shared, so the correlation id is
   what keeps this route from killing another channel's work. 202 + `{:status
   \"cancelling\" :turn_id tid}`, 409 `:not-owner` for someone else's turn, 409
   `:no-running-turn` when idle, 409 `:subagent-stop` in a subagent session."
  [request]
  (let [sid
        (http/path-sid request)

        owner-key
        (get (try (http/body-json request) (catch Throwable _ nil)) "idempotency_key")]

    (if-not (and sid (state/soul sid))
      (http/session-404 (get-in request [:path-params :sid]))
      (or (subagent-stop-refusal sid)
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
                                             :turn_status (:status result))))))))

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

(defn- send-queue-now-handler
  "POST /sessions/:sid/queue/send-now — mark every queued turn for delivery into the
   running turn at its next step. Returns `{:marked [...] :skipped [...]}`."
  [request]
  (let [sid (http/path-sid request)]
    (if (and sid (state/soul sid))
      (http/json-response (state/send-queue-now! sid))
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

(defn- path-attachment
  "The attachment row that `request` names by `:iid` and `:idx`, with that index,
   or nil."
  [request]
  (let [idx (path-idx request)]
    {:idx idx
     :att (when (and idx (nat-int? idx))
            (nth (state/user-iteration-attachments (path-iid request)) idx nil))}))

(defn- reference-file
  "The readable file of the reference `path` for `sid` as `{:file f}`, or
   `{:error response}`. The read gate of the file preview runs on each call,
   because access can change after `attach()` recorded the path."
  [sid path]
  (let [{:keys [file error]} (fs-api/session-readable-file sid path)]
    (cond (nil? error) {:file file}
          (= 404 (:status error))
          {:error (http/error-response 404
                                       :attachment-file-missing
                                       "the referenced file is no longer on the gateway machine"
                                       :path path)}
          :else {:error error})))

(defn- attachment-filename
  "The name a browser shows or saves for attachment `att` that refers to `path`."
  ^String [att path]
  (or (not-empty (str (:filename att))) (.getName (java.io.File. (str path)))))

(defn- reference-response
  "The bytes of the file that the reference `att` names, streamed from disk with
   `Range` support. `?download=1` asks a browser to save the file, else it shows
   it when it can."
  [request sid att path]
  (let [{:keys [file error]} (reference-file sid path)]
    (or error
        (try (fs-api/file-content-response
               request
               file
               {"Content-Type" (or (not-empty (str (:media-type att))) "application/octet-stream")
                "Content-Disposition" (fs-api/content-disposition
                                        (attachment-filename att path)
                                        (= "1" (get-in request [:query-params "download"])))
                "Cache-Control" "no-store"
                "Content-Security-Policy" "sandbox"
                "X-Content-Type-Options" "nosniff"})
             (catch java.io.IOException _
               (http/error-response 404
                                    :attachment-file-missing "the referenced file could not be read"
                                    :path path))))))

(defn- attachment-ticket-for
  "The function from `expires` to the attachment ticket of `request`, or nil when
   the request names no session or index."
  [request sid idx]
  (when (and sid idx (nat-int? idx))
    #(fs-api/attachment-ticket (str sid) (str (path-iid request)) idx %)))

(defn- ticketed-attachment-response
  "The answer to a request that carries a ticket of the link route in place of the
   bearer token. The ticket goes first, so that a caller without one cannot tell
   which attachments exist. A ticket opens only an attachment that keeps a
   reference: an inline attachment needs the token."
  [request sid]
  (let [idx (path-idx request)]
    (or (fs-api/ticket-refusal request
                               (attachment-ticket-for request sid idx)
                               "ask the attachment link route again")
        (let [{:keys [att]} (path-attachment request)]
          (if-let [reference (attachment-storage/reference-path (:storage-uri att))]
            (reference-response request sid att reference)
            (http/error-response 401 :invalid-ticket "missing or invalid content ticket"))))))

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
   safely `immutable`-cacheable.

   A REFERENCE (#450) names a file on the gateway machine instead of stored bytes.
   It streams from disk with `Range` support, after the same read gate as the file
   preview, on each request. The file can change or go away, so it is `no-store`.
   The signed `content_url` of the link route (#451) opens a reference without the
   bearer token, because a frame, a download link or another app cannot send it."
  [request]
  (let [sid (http/path-sid request)]
    (cond (fs-api/content-ticket-request? request) (ticketed-attachment-response request sid)
          (nil? sid) (http/session-404 (get-in request [:path-params :sid]))
          :else (let [{:keys [idx att]} (path-attachment request)
                      reference (attachment-storage/reference-path (:storage-uri att))
                      ^bytes bs (when-not reference
                                  (some-> att
                                          state/attachment-bytes))]

                  (cond reference (reference-response request sid att reference)
                        bs {:status 200
                            :headers {"Content-Type" (or (not-empty (str (:media-type att)))
                                                         "application/octet-stream")
                                      "Content-Length" (str (alength bs))
                                      "Cache-Control" "private, max-age=31536000, immutable"
                                      ;; The web app shares this origin (`vis-agent web`) and keeps
                                      ;; its saved gateway tokens there. An agent-made HTML or SVG
                                      ;; file opened straight from this URL must not run script as
                                      ;; that origin.
                                      "Content-Security-Policy" "sandbox"
                                      "X-Content-Type-Options" "nosniff"}
                            :body (java.io.ByteArrayInputStream. bs)}
                        :else (http/error-response 404
                                                   :attachment-not-found "unknown attachment"
                                                   :iteration_id (str (path-iid request))
                                                   :index idx))))))

(defn- attachment-link-handler
  "GET /v1/sessions/:sid/iterations/:iid/attachments/:idx/link — a signed address
   of the bytes of one attachment that keeps a reference (#451). A frame, a
   download link or another app opens it without the bearer token. The ticket
   names this session, iteration and index only, and expires like the media
   address of the file preview. An inline attachment gets 409: the bytes route
   with the token serves it."
  [request]
  (if-let [sid (http/path-sid request)]
    (let [{:keys [idx att]} (path-attachment request)
          reference (attachment-storage/reference-path (:storage-uri att))]

      (cond (nil? att) (http/error-response 404
                                            :attachment-not-found "unknown attachment"
                                            :iteration_id (str (path-iid request))
                                            :index idx)
            (not reference) (http/error-response
                              409
                              :attachment-not-reference
                              "this attachment is stored inline; read it with the bearer token"
                              :iteration_id (str (path-iid request))
                              :index idx)
            :else (or (:error (reference-file sid reference))
                      (let [iid (str (path-iid request))
                            expires (fs-api/ticket-expiry)]

                        (http/json-response
                          {:content-url (str "/v1/sessions/" sid
                                             "/iterations/" iid
                                             "/attachments/" idx
                                             "?expires=" expires
                                             "&ticket="
                                             (fs-api/attachment-ticket (str sid) iid idx expires))
                           :expires-at expires})))))
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- page-media-type?
  "True when `media-type` names an HTML page, which can tell the model its state."
  [media-type]
  (contains? #{(:html attachments/document-media-types) (:xhtml attachments/document-media-types)}
             (str/lower-case (str/trim (first (str/split (str media-type) #";"))))))

(defn- attachment-agent-note-handler
  "PUT /v1/sessions/:sid/iterations/:iid/attachments/:idx/agent-note — keep what
   one HTML page tells the model. The body is `{\"text\": …}`, the page state in
   plain text. The model reads the newest note of each page with the next user
   message. A blank text removes the note, and the same request twice gives the
   same state."
  [request]
  (let [sid (http/path-sid request)]
    (if (and sid (persistance/db-get-session (lp/db-info) sid))
      (let [idx (path-idx request)
            att (when (and idx (nat-int? idx))
                  (nth (state/user-iteration-attachments (path-iid request)) idx nil))
            text (get (http/body-json request) "text")
            limit (:agent-note app/bounds)]

        (cond
          (not (and att (page-media-type? (:media-type att))))
          (http/error-response 404
                               :attachment-not-found "unknown HTML page"
                               :iteration_id (str (path-iid request))
                               :index idx)
          (not (string? text))
          (http/error-response 400 :invalid-request "the body needs text, the note for the model")
          (> (count text) (long limit))
          (http/error-response 400
                               :invalid-request
                               (str "a note for the model holds at most " limit " characters"))
          :else (let [page-id (str (:id att))]
                  (view-sink/note-page! sid {:id page-id :title (:filename att) :agent-note text})
                  (http/json-response {:id page-id
                                       :agent-note (when-not (str/blank? text) (str/trim text))}))))
      (http/session-404 (get-in request [:path-params :sid])))))

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
  {[:get "/v1/sessions/:sid/iterations/:iid/attachments/:idx"] attachment-bytes-handler
   [:get "/v1/sessions/:sid/iterations/:iid/attachments/:idx/link"] attachment-link-handler
   [:put "/v1/sessions/:sid/iterations/:iid/attachments/:idx/agent-note"]
   attachment-agent-note-handler
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
   [:post "/v1/sessions/:sid/resume-queue"] resume-queue-handler
   [:post "/v1/sessions/:sid/queue/send-now"] send-queue-now-handler})
