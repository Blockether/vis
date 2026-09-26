(ns com.blockether.vis.internal.gateway.server.views
  "View routes: gateway capabilities, input and live views, view actions and
   client-extension calls."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.extension.client :as client-extensions]
            [com.blockether.vis.internal.gateway.pairing :as pairing]
            [com.blockether.vis.internal.gateway.push :as push]
            [com.blockether.vis.internal.gateway.runtime :as protocol]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.server.instance :as instance]
            [com.blockether.vis.internal.gateway.server.speech :as speech-api]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.gateway.view :as gw-view]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.speech.core :as speech]))

(defn- client-extension-handler
  [operation]
  (fn [request]
    (let [sid
          (http/path-sid request)

          owner
          (get-in request [:headers "x-vis-client-id"])

          live?
          #(boolean (instance/live-client? owner))]

      (cond (not (live?))
            (http/error-response 403 :client_extension_owner "A live client lease is required")
            (not (and sid (state/soul sid))) (http/session-404 (get-in request [:path-params :sid]))
            :else (try
                    (http/json-response
                      (case operation
                        :register
                        (lp/register-client-extensions! sid owner (http/body-json request) live?)

                        :detach
                        (client-extensions/detach-owned! sid owner)

                        :pending
                        (client-extensions/pending sid owner)

                        :result
                        (client-extensions/complete! sid
                                                     owner
                                                     (get-in request [:path-params :call_id])
                                                     (http/body-json request))

                        :activity
                        (client-extensions/activity! sid
                                                     owner
                                                     (get-in request [:path-params :call_id])
                                                     (http/body-json request))))
                    (catch clojure.lang.ExceptionInfo e
                      (let [{:keys [status code]} (ex-data e)]
                        (http/error-response (or status 400)
                                             (or code :invalid_client_extension)
                                             (ex-message e)))))))))

(defn- view-404
  [view-id]
  (http/error-response 404 :view-not-found "no such open View" :view_id (str view-id)))

(defn- list-input-views-handler
  "GET /v1/sessions/:sid/views/input — the typed input requests this session is
   BLOCKED on right now.

   The live `view.open` event is the fast path; this is how a client that connected
   later still finds the open form instead of watching a turn that never moves."
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:requests (gw-view/input-views sid)})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- view-action-handler
  "POST /v1/sessions/:sid/views/:view-id/actions — apply one closed operator
   action to either View kind.

   The body names `action`: `submit`, `cancel`, `select`, or `interrupt`, with that
   action's payload. Kind is a property of the addressed View, never part of the
   URL. The engine owns validation and capability policy, so every surface receives
   the same verdict."
  [request]
  (let [sid
        (http/path-sid request)

        view-id
        (some-> (get-in request [:path-params :view-id])
                str
                not-empty)

        descriptor
        (when (and sid view-id) (gw-view/view-of sid view-id))]

    (cond (nil? sid) (http/session-404 (get-in request [:path-params :sid]))
          (nil? descriptor) (view-404 view-id)
          :else (try (let [{:keys [is-accepted reason] :as outcome}
                           (gw-view/action! view-id (http/body-json request))]
                       (cond (and (false? is-accepted) (= "unknown" reason)) (view-404 view-id)
                             (and (false? is-accepted) (= "not_cancellable" reason))
                             (http/error-response 409
                                                  :view-action-refused
                                                  "this input View cannot be cancelled"
                                                  :view_id view-id
                                                  :action "cancel")
                             :else (http/json-response outcome)))
                     (catch clojure.lang.ExceptionInfo e
                       (let [{:keys [type action]} (ex-data e)]
                         (http/error-response (if (= :vis/view-action-not-supported type) 409 400)
                                              (or type :vis/view-action-refused)
                                              (ex-message e)
                                              :view_id view-id
                                              :action (some-> action
                                                              name))))))))

(defn- path-view-id
  "The live view id in a file-backed resource path, only when it could BE one."
  [request]
  (some-> (get-in request [:path-params :view-id])
          parse-uuid
          str))

(defn- list-live-views-handler
  "GET /v1/sessions/:sid/views/live — the live views this session is showing
   right now, oldest first, each as the picture a surface paints.

   The `view.open` / `view.patch` / `view.close` events are the fast path; this is
   what a client that connected later reads to recover the current picture."
  [request]
  (if-let [sid (http/path-sid request)]
    (http/json-response {:views (gw-view/live-views sid)})
    (http/session-404 (get-in request [:path-params :sid]))))

(defn- live-view-log-handler
  "GET /v1/sessions/:sid/views/live/:view-id/log?node=… — one bounded page.
   `node` names the log node: a query parameter, never a path segment, because
   node ids are free text a surface chose (a Jenkins job is `folder/job`) and a
   `/` inside a path segment is refused before any route matches. `query` is
   literal and case-insensitive; `from` is a zero-based match offset, `limit`
   bounds returned lines. Answers counts and original line numbers from the
   durable record, including after the view has closed."
  [request]
  (let [sid
        (http/path-sid request)

        view-id
        (path-view-id request)

        node-id
        (get-in request [:query-params "node"])]

    (cond (nil? sid) (http/session-404 (get-in request [:path-params :sid]))
          (nil? view-id) (view-404 (get-in request [:path-params :view-id]))
          (str/blank? node-id)
          (http/error-response 400 :invalid-request "node must name a log node")
          :else (http/json-response (gw-view/live-log-range sid
                                                            view-id
                                                            node-id
                                                            (http/query-long request "from")
                                                            (http/query-long request "limit")
                                                            (get-in request
                                                                    [:query-params "query"]))))))

(defn- reachable-addresses
  "Every base URL this gateway answers on, most durable first (Tailscale before
   LAN — see [[pairing/candidate-hosts]]), led by the operator's `--advertise`
   address when one was given.

   A pairing QR carries the same list, but only ONCE: a phone paired at home
   picks the LAN address, keeps it, and is stranded the moment it leaves the
   house. Advertising the addresses on a live, token-gated endpoint lets an
   already-paired client discover the tailnet address and move itself, with no
   second QR. The port/scheme come from the request when the bind is unknown, so
   a tunnel sees itself correctly.

   A wildcard bind also answers on loopback, so `http://127.0.0.1:<port>` is
   offered LAST: a client on this same machine can take it instead of routing
   through the host's LAN interface. Other clients must not — there 127.0.0.1
   means themselves — so the companion switches to it only after the gateway at
   that URL reports the identity it is already connected to (#277)."
  [request]
  (let [{:keys [host port advertise]}
        @instance/server-state

        port
        (or port (:server-port request) 7890)

        scheme
        (name (or (:scheme request) :http))

        bind
        (or host "0.0.0.0")]

    (->> (concat [(pairing/advertised-url advertise port)]
                 (->> (pairing/candidate-hosts bind)
                      (remove str/blank?)
                      (map #(str scheme "://" % ":" port)))
                 (when (pairing/wildcard-bind? bind) [(str scheme "://127.0.0.1:" port)]))
         (remove str/blank?)
         distinct
         vec)))

(defn- capabilities-handler
  "GET /v1/capabilities — stable feature negotiation for remote/native clients.
   Availability describes what THIS gateway can accept; device-side permissions
   remain the client's responsibility. Voice reports the SELECTED engine, every
   engine that is registered, the phase vocabulary a client may be shown, and the
   selected engine's readiness — without starting any download; `speech` answers the
   same questions for the other direction, and adds the voices each engine can speak
   in and the length at which a line becomes a job instead of a WAV on this
   connection. `protocol` carries the VERSION contract ([[protocol/handshake]]) and
   `compatibility` this gateway's verdict on the CALLER, so one request answers both
   \"what can you do\" and \"can we talk\"."
  [request]
  (let [engine
        (try (speech/default-engine :transcribe) (catch Throwable _ nil))

        voice-caps
        (merge {:enabled (boolean engine)
                :transport "audio/wav"
                :transcription "gateway-local"
                ;; the POST returns a JOB, not a transcript: a client that sees this
                ;; STREAMS the job's own progress (`/voice/jobs/:id/events`) instead of
                ;; holding a socket open for a minute or polling for a percentage. That
                ;; stream is NOT the session event log, so `:progress-event` NAMES every
                ;; frame on it — a client filters on a name it was told rather than
                ;; guessing a job from the payload's shape.
                :is-async true
                :progress "sse"
                :progress-event gateway-contract/voice-job-event
                :phases (mapv name (speech/direction-phases :transcribe))
                :model (if engine
                         (speech-api/speech-state->json (speech/readiness engine))
                         {:status "unavailable"})}
               (speech/engines-info :transcribe))

        speech-engine
        (try (speech/default-engine :synthesize) (catch Throwable _ nil))

        speech-caps
        ;; ONE actionable boolean: this machine HAS an engine that can speak. Whether a
        ;; reply is spoken belongs to the surface's voice conversation, never to a flag here.
        (merge {:is-enabled (boolean speech-engine)
                :transport "audio/wav"
                :synthesis "gateway-local"
                :is-async true
                :progress "sse"
                :progress-event gateway-contract/speech-job-event
                :phases (mapv name (speech/direction-phases :synthesize))
                ;; a short line comes back as the audio on this connection and a long one as
                ;; a job: both thresholds are published so a client never has to discover
                ;; either by being refused.
                :inline-max-chars speech-api/speech-inline-max-chars
                :max-chars speech-api/speech-max-chars
                :model (if speech-engine
                         (speech-api/speech-state->json (speech/readiness speech-engine))
                         {:status "unavailable"})}
               (speech/engines-info :synthesize))]

    (http/json-response
      {:version 1
       :protocol (protocol/handshake)
       :addresses (reachable-addresses request)
       :compatibility (protocol/gateway-verdict request)
       :features {:chat {:enabled true}
                  :attachments {:enabled true
                                :transport "inline-base64"
                                ;; Derived, never a second list: every document or media
                                ;; type the intake keeps is offered by the picker in the same
                                ;; commit. Documents are human-only; the model receives their
                                ;; names rather than bytes.
                                :media-types (into ["image/jpeg" "image/png" "image/gif"
                                                    "image/webp" "image/bmp"]
                                                   (concat (sort attachments/human-only-media-types)
                                                           (sort attachments/video-media-types)
                                                           (sort attachments/audio-media-types)))
                                :video-media-types (vec (sort attachments/video-media-types))
                                ;; A recording rides the same intake as a clip and is kept for
                                ;; the human: the model is told the file is there rather than
                                ;; handed bytes no multimodal wire has a block for.
                                :audio-media-types (vec (sort attachments/audio-media-types))
                                :max-files attachments/max-image-count
                                :max-file-bytes attachments/max-upload-image-bytes
                                :max-video-bytes attachments/max-video-bytes
                                :max-audio-bytes attachments/max-audio-bytes}
                  :voice voice-caps
                  :speech speech-caps
                  :push (push/status)}})))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/capabilities"] capabilities-handler
   [:put "/v1/sessions/:sid/client-extensions"] (client-extension-handler :register)
   [:delete "/v1/sessions/:sid/client-extensions"] (client-extension-handler :detach)
   [:get "/v1/sessions/:sid/client-calls"] (client-extension-handler :pending)
   [:post "/v1/sessions/:sid/client-calls/:call_id/result"] (client-extension-handler :result)
   [:post "/v1/sessions/:sid/client-calls/:call_id/activity"] (client-extension-handler :activity)
   [:get "/v1/sessions/:sid/views/input"] list-input-views-handler
   [:get "/v1/sessions/:sid/views/live"] list-live-views-handler
   [:get "/v1/sessions/:sid/views/live/:view-id/log"] live-view-log-handler
   [:post "/v1/sessions/:sid/views/:view-id/actions"] view-action-handler})
