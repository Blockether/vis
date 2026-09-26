(ns com.blockether.vis.internal.gateway.server.speech
  "Speech routes: transcription and synthesis jobs with their event streams and
   audio, voices, and engine model selection."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.gateway :as gateway-contract]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.gateway.server.transport.sse :as sse]
            [com.blockether.vis.internal.gateway.state :as state]
            [com.blockether.vis.internal.speech.core :as speech]
            [ring.core.protocols :as ring-protocols]
            [taoensso.telemere :as tel])
  (:import [java.io OutputStream]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

;; Speech — canonical transcription and synthesis through the built-in local
;; sherpa-onnx subsystem. Lives once on the GATEWAY so every client — web, iOS
;; and TUI — hits the SAME canonical /v1 routes and shares one model lifecycle.

(defn- requested-engine-id
  "`?engine=pocket-tts-local` selects one fixed built-in for this request; absent
   uses the gateway default."
  [request]
  (some-> (get-in request [:query-params "engine"])
          str/trim
          not-empty
          keyword))

(defn- with-engine
  "Resolve a built-in engine for one direction. An unknown id answers 400; an
   unavailable fixed engine set answers 501."
  [direction request f]
  (let [id
        (requested-engine-id request)

        noun
        (speech/direction-nouns direction)]

    (if-let [engine (try (speech/resolve-engine direction id) (catch Throwable _ nil))]
      (f engine)
      (if id
        (http/error-response 400
                             :unknown-engine (str "unknown " noun " engine: " (name id))
                             :engines (mapv speech/public-engine (speech/engines direction)))
        (http/error-response 501 :engine-unavailable (str "no " noun " engine is available"))))))

(defn- unknown-speech-session?
  "Machine speech routes need no conversation. Session routes still reject unknown or malformed ids."
  [request]
  (and (contains? (:path-params request) :sid)
       (not (when-let [sid (http/path-sid request)]
              (state/soul sid)))))

(defn- with-direction-engine
  "Resolve a local speech engine, validating the session when the route names one."
  [direction request f]
  (let [sid (http/path-sid request)]
    (if (unknown-speech-session? request)
      (http/session-404 (get-in request [:path-params :sid]))
      (with-engine direction
                   request
                   (fn [engine]
                     (f sid engine))))))

(defn- with-voice-engine
  "[[with-direction-engine]] for the transcription routes."
  [request f]
  (with-direction-engine :transcribe request f))

(defn- with-speech-engine
  "[[with-direction-engine]] for the synthesis routes."
  [request f]
  (with-direction-engine :synthesize request f))

(def ^:const speech-inline-max-chars
  "Above this many characters `POST /v1/sessions/:sid/speech` answers a JOB instead of
   the audio itself.

   CHARACTERS, not an estimated duration: the gateway cannot know how long a line takes
   to speak without speaking it, and a client must be able to predict WHICH of the two
   answers it will get before it sends. The number is published as
   `features.speech.inline_max_chars`, so the rule is the server's and the expectation
   is the client's. A couple of sentences is the whole latency budget of a spoken
   acknowledgement; past that a progress stream beats a held socket."
  280)

(def ^:const speech-max-chars
  "The longest line the gateway will speak at all. A runaway reply must not become a
   multi-minute synthesis holding a thread and a temp file, so past this `POST …/speech`
   answers 413 and the caller splits the text itself."
  20000)

(defn speech-state->json
  [st]
  (cond-> {:status (name (:state st))}
    (:progress st)
    (assoc :progress (:progress st))

    ;; "downloading" covers both the transfer and the multi-minute unpack; the
    ;; phase lets a UI name what is actually happening instead of stalling on 99%.
    (:phase st)
    (assoc :phase (name (:phase st)))

    (:error st)
    (assoc :error (:error st))))

(defn- engine-not-ready
  "425 Too Early for an engine that is still preparing. The readiness rides as the
   error's `model`, the same state `/voice/model` and `/speech/model` answer, and a
   failed preparation's own sentence becomes the message."
  [direction engine]
  (let [model (speech-state->json (speech/readiness engine))]
    (http/error-response 425
                         :engine-not-ready (or (:error model)
                                               (str "the "
                                                    (speech/direction-nouns direction)
                                                    " engine is not ready ("
                                                    (:status model)
                                                    ")"))
                         :model model)))

(defn- wav-file?
  "RIFF/WAVE magic + minimum header length — the CHEAP pre-filter that turns an
   obviously-not-audio body into a clear 400 without waking the ASR. sherpa-onnx's
   native WaveReader ABORTS THE WHOLE JVM on malformed input, so the header is
   verified in JVM code before the native reader ever runs."
  [^java.io.File f]
  (and (>= (.length f) 44)
       (with-open [in (io/input-stream f)]
         (let [head (byte-array 12)]
           (and (= 12 (.read in head))
                (= "RIFF" (String. head 0 4 "US-ASCII"))
                (= "WAVE" (String. head 8 4 "US-ASCII")))))))

(defn- model-handler
  "GET/POST /v1/{voice,speech}/model reads or starts engine preparation. Speech may
   name `voice_id`; an opt-in voice additionally requires `is_license_accepted=true`
   on POST, so merely selecting it can never accept its terms."
  [direction request]
  (with-engine
    direction
    request
    (fn [engine]
      (let [voice-id
            (when (= :synthesize direction) (http/query-str request "voice_id"))

            opts
            (when voice-id
              {:voice-id voice-id
               :is-license-accepted (= "true" (http/query-str request "is_license_accepted"))})]

        (http/json-response 200
                            (assoc (speech-state->json (if (= :post (:request-method request))
                                                         (speech/prepare! engine opts)
                                                         (speech/readiness engine opts)))
                              :engine (name (:id engine))))))))

(defn- voice-model-handler [request] (model-handler :transcribe request))

(defn- speech-model-handler [request] (model-handler :synthesize request))

(defn- voice-handler
  "POST /v1/sessions/:sid/voice — body is a recorded WAV blob. ACCEPTS the audio
   and answers **202 with a job**; transcription runs on its own thread and the
   client STREAMS `/voice/jobs/:job-id/events` (SSE) to watch it.

   That split is the whole point: the upload is the CLIENT's progress (bytes in
   flight), and 202 is the gateway saying \"I have your recording\" — from then on
   the phase and percentage come from the job, so a minute-long transcription is
   never an unexplained spinner and never a dead socket.

   The engine must be ready — the client drives preparation via /voice/model; a
   not-ready engine answers 425 (Too Early) with its state, NEVER blocking the
   request thread on a ~465MB download."
  [request]
  (with-voice-engine
    request
    (fn [_sid engine]
      (if-not (speech/ready? engine)
        (engine-not-ready :transcribe engine)
        (let [tmp (java.io.File/createTempFile "vis-speech" ".wav")]
          (try (with-open [in ^java.io.InputStream (:body request)
                           out (io/output-stream tmp)]

                 (io/copy in out))
               (if-not (wav-file? tmp)
                 (do (.delete tmp)
                     (http/error-response 400 :invalid-audio "body must be a RIFF/WAVE audio file"))
                 ;; the temp file outlives this response on purpose: it is the
                 ;; job's input and is deleted by `:on-done`, whichever way the
                 ;; job ends.
                 (http/json-response 202
                                     (speech/submit! :transcribe
                                                     {:audio-path (str tmp)
                                                      :engine-id (:id engine)
                                                      :on-done (fn [_job]
                                                                 (.delete tmp))})))
               (catch Throwable t
                 (.delete tmp)
                 (tel/log! {:level :error :id ::voice-transcribe-failed :data {:error (str t)}})
                 (http/error-response 400 :transcription-failed (speech/error-message t)))))))))

(defn- speech-failure-response
  "The refusal a THROWN synthesis deserves: 500. The request was valid and the engine
   failed, which is the gateway's fact to report rather than the caller's to fix."
  [^Throwable t]
  (tel/log! {:level :error :id ::speech-synthesize-failed :data {:error (str t)}})
  (http/error-response 500 :synthesis-failed (speech/error-message t)))

(defn- file-bytes
  ^bytes [^java.io.File f]
  (with-open [in
              (io/input-stream f)

              out
              (java.io.ByteArrayOutputStream.)]

    (io/copy in out)
    (.toByteArray out)))

(defn- inline-speech-response
  "A short line spoken on THIS connection: the audio itself, with its media type and
   length. The temp file is read and deleted here — an inline answer has no job to own
   the file and no client coming back for it."
  [work]
  (let [{:keys [audio-path media-type]}
        (speech/synthesize! work)

        f
        (io/file audio-path)]

    (try {:status 200
          :headers {"Content-Type" (or media-type "audio/wav")
                    "Content-Length" (str (.length f))
                    "Cache-Control" "no-store"}
          :body (file-bytes f)}
         (finally (.delete f)))))

(defn- speech-handler
  "POST /v1/sessions/:sid/speech — body is JSON `{\"text\": …, \"voice\": …}` and
   `?engine=` names the engine. Answers the AUDIO for a short line and **202 with a job**
   for a long one.

   That split is the whole point: a spoken acknowledgement must land in one round trip,
   while a paragraph is worth watching — the client STREAMS
   `/speech/jobs/:job-id/events` and fetches `/speech/jobs/:job-id/audio` when it is done.
   The threshold is [[speech-inline-max-chars]] and `/v1/capabilities` publishes it, so a
   client knows which of the two answers it is about to get.

   The engine must be ready — the client drives preparation via /speech/model; a not-ready
   engine answers 425 (Too Early) with its state, NEVER blocking the request thread on a
   model download."
  [request]
  (with-speech-engine
    request
    (fn [_sid engine]
      (let [body
            (try (http/body-json request) (catch Throwable _ nil))

            text
            (some-> (get body "text")
                    str
                    str/trim)

            voice-id
            (some-> (get body "voice")
                    str
                    str/trim
                    not-empty
                    keyword)

            work
            (cond-> {:text text :engine-id (:id engine)}
              voice-id
              (assoc :voice-id voice-id))]

        (cond (str/blank? text) (http/error-response 400
                                                     :invalid-request
                                                     "body must be JSON with a non-empty \"text\"")
              (> (count text) (long speech-max-chars))
              (http/error-response 413
                                   :text-too-long
                                   (str "text is longer than " speech-max-chars " characters"))
              (not (speech/ready? engine)) (engine-not-ready :synthesize engine)
              :else (try (if (> (count text) (long speech-inline-max-chars))
                           (http/json-response 202 (speech/submit! :synthesize work))
                           (inline-speech-response work))
                         (catch Throwable t (speech-failure-response t))))))))

(defn- job-404
  "The refusal for a job id this direction's routes do not know."
  [direction job-id]
  (http/error-response 404
                       :job-not-found (str "unknown " (speech/direction-nouns direction) " job")
                       :job-id job-id))

(defn- job-handler
  "GET    /v1/sessions/:sid/{voice,speech}/jobs/:job-id — where this piece of work is:
          `{:id :direction :engine :phase :progress :is_done :text? :audio? :error?}`. ONE
          read of the job resource; a client that wants to WATCH it streams the twin
          below instead of asking again and again.
   DELETE the same path — forget it once the transcript or the audio has been collected
          (finished jobs also expire on their own, and a forgotten spoken reply takes its
          audio file with it).

   A job belongs to exactly ONE direction and the other direction's route does not know
   it: asking `/speech/jobs/` for a transcription is a client bug, answered 404 rather
   than leaking the other half of the store."
  [direction request]
  (let [job-id
        (get-in request [:path-params :job-id])

        job
        (speech/job job-id)

        mine?
        (= (name direction) (:direction job))]

    (cond (unknown-speech-session? request) (http/session-404 (get-in request [:path-params :sid]))
          ;; DELETE stays idempotent — an id nobody knows is already forgotten — but it
          ;; never reaches across into the other direction's job.
          (= :delete (:request-method request))
          (if (and job (not mine?))
            (job-404 direction job-id)
            (do (speech/forget! job-id) (http/json-response 200 {:is-forgotten true})))
          mine? (http/json-response 200 job)
          :else (job-404 direction job-id))))

(defn- voice-job-handler [request] (job-handler :transcribe request))

(defn- speech-job-handler [request] (job-handler :synthesize request))

(defn- speech-job-audio-handler
  "GET /v1/sessions/:sid/speech/jobs/:job-id/audio — the audio this job spoke.

   FETCHED, never named: [[speech/public-job]] carries the media type and the byte count
   but not the path, so the file is reachable only through the job that owns it and only
   while that job lives. A job that has not produced audio yet answers 425 with its own
   state, so a client that raced its stream retries instead of caching an error."
  [request]
  (let [job-id
        (get-in request [:path-params :job-id])

        job
        (speech/job job-id)]

    (cond (unknown-speech-session? request) (http/session-404 (get-in request [:path-params :sid]))
          (not= "synthesize" (:direction job)) (job-404 :synthesize job-id)
          :else (let [^java.io.File f (some-> (speech/job-audio-path job-id)
                                              io/file)]
                  (if-not (and f (.isFile f))
                    (http/error-response 425
                                         :audio-not-ready
                                         (or (:error job)
                                             "the speech synthesis job has no audio yet")
                                         :job job)
                    {:status 200
                     :headers {"Content-Type" (or (get-in job [:audio :media-type]) "audio/wav")
                               "Content-Length" (str (.length f))
                               "Cache-Control" "no-store"}
                     :body f})))))

;; Speech voices - the ones somebody brings, not the ones that shipped

(defn- voice-import-failure
  "The refusal an import deserves: 409 when the selected engine cannot learn a voice at
   all (a fact about the engine, which the client should report rather than retry), 400
   when the RECORDING is the problem and the caller can fix it, 500 only for neither."
  [^Throwable t]
  (let [kind (:type (ex-data t))]
    (cond (= :vis/voice-import-unsupported kind) (http/error-response 409
                                                                      :voice-import-unsupported
                                                                      (ex-message t)
                                                                      :engine (:engine (ex-data t)))
          (= "speech-tts"
             (some-> kind
                     namespace))
          (http/error-response 400 :invalid-voice-clip (ex-message t) :reason (name kind))
          :else (do (tel/log! {:level :error :id ::voice-import-failed :data {:error (str t)}})
                    (http/error-response 500 :voice-import-failed (speech/error-message t))))))

(defn- speech-voices-handler
  "GET  /v1/speech/voices - every voice the speaking engine can use, plus whether it can
         learn another one.
   POST the same path - the body is a RECORDING, described by `?name=`, `?lang=` and
         `?text=` (the clip's own transcript). Answers 201 with the voice it became.

   A cloning voice IS a reference clip, so \"create a voice\" is an upload and nothing
   else. Deliberately NOT gated on model readiness: the clip is stored on disk and needs
   no model, so a voice can be added while the bundle is still downloading.

   No session in the path: an imported clip belongs to the machine, and every session on
   it speaks with the same catalogue."
  [request]
  (with-engine :synthesize
               request
               (fn [engine]
                 (if-not (= :post (:request-method request))
                   (http/json-response 200
                                       {:engine (speech/public-engine engine)
                                        :voices (speech/voices engine)})
                   (let [tmp (java.io.File/createTempFile "vis-speech-clip" ".upload")]
                     (try (with-open [in ^java.io.InputStream (:body request)
                                      out (io/output-stream tmp)]

                            (io/copy in out))
                          (http/json-response 201
                                              {:voice (speech/import-voice!
                                                        engine
                                                        {:path (str tmp)
                                                         :voice-name (http/query-str request "name")
                                                         :language (http/query-str request "lang")
                                                         :text (http/query-str request "text")})})
                          (catch Throwable t (voice-import-failure t))
                          (finally (.delete tmp))))))))

(defn- speech-voice-handler
  "DELETE /v1/speech/voices/:voice-id - forget an imported voice.

   404 when the engine has no such imported voice: a client that deleted a voice it
   could still see is looking at a stale catalogue, and that is worth being told."
  [request]
  (with-engine :synthesize
               request
               (fn [engine]
                 (try (if (speech/forget-voice! engine (get-in request [:path-params :voice-id]))
                        (http/json-response 200 {:is-forgotten true})
                        (http/error-response 404 :voice-not-found "no imported voice with that id"))
                      (catch Throwable t (voice-import-failure t))))))

(defn- speech-voice-sample-handler
  "GET /v1/speech/voices/:voice-id/sample - what this voice SOUNDS like, as audio.

   Machine-level like the rest of the catalogue: a voice is chosen for the machine,
   so hearing one is nobody's session business - and a preview must never have to
   invent a session id to reach POST /v1/sessions/:sid/speech.

   It PREPARES a sample when preparing one is cheap ([[speech/voice-sample!]] owns
   that contract) and 404s when there is none to be had - exactly the voices whose
   catalogue entry carried neither `is_sample_ready` nor `is_sample_preparable`.
   `no-store` like the job audio next door: an imported voice re-imported under the
   same name is a DIFFERENT recording, and a week-old cached sample would be lying."
  [request]
  (with-engine :synthesize
               request
               (fn [engine]
                 (try (let [sample
                            (speech/voice-sample! engine (get-in request [:path-params :voice-id]))

                            ^java.io.File f
                            (some-> (:audio-path sample)
                                    io/file)]

                        (if-not (and f (.isFile f))
                          (http/error-response 404 :sample-not-found "no sample for that voice")
                          {:status 200
                           :headers {"Content-Type" (or (:media-type sample) "audio/wav")
                                     "Content-Length" (str (.length f))
                                     "Cache-Control" "no-store"}
                           :body f}))
                      (catch Throwable t
                        (http/error-response 502
                                             :sample-failed
                                             (or (ex-message t) "could not prepare a sample")))))))

(def ^:private JOB_QUEUE_CAP
  "Per-connection queue of job states. A transcription or a synthesis reports a handful
   of percentages per second at most, so anything the writer cannot keep up with is a
   dead socket, not backpressure worth buffering."
  64)

(def ^:private job-event-names
  "The SSE frame name each direction's job stream carries — the one discriminator a
   client filters on (see [[gateway-contract/voice-job-event]])."
  {:transcribe gateway-contract/voice-job-event :synthesize gateway-contract/speech-job-event})

(defn- job-events-body
  "Ring streamable body for ONE speech job: its CURRENT state first, then a frame named
   `event-name` per change until the job is done or failed.

   The current state rides first on purpose — that is what makes a reconnect free
   of a poll: the stream is subscribed BEFORE the snapshot is read, so a job that
   finished in that instant is still reported, and a client that lost the socket
   re-opens and is told the terminal phase immediately.

   [[speech/watch!]] only enqueues; this thread is the connection's single socket
   writer, so the engine's thread is never blocked by a stalled reader."
  [^String event-name job-id]
  (reify
    ring-protocols/StreamableResponseBody
      (write-body-to-stream [_ _ output-stream]
        (let [^OutputStream out
              output-stream

              queue
              (ArrayBlockingQueue. (int JOB_QUEUE_CAP))

              unwatch
              (speech/watch! job-id
                             (fn [job]
                               (.offer queue job)))

              write!
              (fn [job]
                (.write out (.getBytes (sse/job-sse-frame event-name job) StandardCharsets/UTF_8))
                (.flush out))]

          (try (let [current (speech/job job-id)]
                 (when current (write! current))
                 (loop [done? (or (nil? current) (:is-done current))]
                   (when-not done?
                     (if-let [job (.poll queue (long sse/HEARTBEAT_MS) TimeUnit/MILLISECONDS)]
                       (do (write! job) (recur (:is-done job)))
                       ;; heartbeat comment: keeps proxies from reaping a quiet
                       ;; upload-to-first-chunk gap, and detects a dead client.
                       (do (.write out (.getBytes ": ping\n\n" StandardCharsets/UTF_8))
                           (.flush out)
                           (recur false))))))
               (catch Throwable _ nil)
               (finally (unwatch) (try (.close out) (catch Throwable _ nil))))))))

(defn- job-events-handler
  "GET /v1/sessions/:sid/{voice,speech}/jobs/:job-id/events — the job's phase and
   percentage PUSHED as they happen, as `text/event-stream`.

   Progress that is polled is progress that arrives late and costs a request per
   tick; this is the same job resource, streamed. The stream ENDS itself on the
   terminal frame, so the client neither polls nor guesses when to stop reading."
  [direction request]
  (let [job-id
        (get-in request [:path-params :job-id])

        job
        (speech/job job-id)]

    (cond (unknown-speech-session? request) (http/session-404 (get-in request [:path-params :sid]))
          (not= (name direction) (:direction job)) (job-404 direction job-id)
          :else {:status 200
                 :headers sse/sse-headers
                 :body (job-events-body (job-event-names direction) job-id)})))

(defn- voice-job-events-handler [request] (job-events-handler :transcribe request))

(defn- speech-job-events-handler [request] (job-events-handler :synthesize request))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:post "/v1/voice"] voice-handler
   [:get "/v1/voice/jobs/:job-id"] voice-job-handler
   [:delete "/v1/voice/jobs/:job-id"] voice-job-handler
   [:get "/v1/voice/jobs/:job-id/events"] voice-job-events-handler
   [:post "/v1/speech"] speech-handler
   [:get "/v1/speech/jobs/:job-id"] speech-job-handler
   [:delete "/v1/speech/jobs/:job-id"] speech-job-handler
   [:get "/v1/speech/jobs/:job-id/events"] speech-job-events-handler
   [:get "/v1/speech/jobs/:job-id/audio"] speech-job-audio-handler
   [:get "/v1/speech/voices"] speech-voices-handler
   [:post "/v1/speech/voices"] speech-voices-handler
   [:delete "/v1/speech/voices/:voice-id"] speech-voice-handler
   [:get "/v1/speech/voices/:voice-id/sample"] speech-voice-sample-handler
   [:get "/v1/voice/model"] voice-model-handler
   [:post "/v1/voice/model"] voice-model-handler
   [:get "/v1/speech/model"] speech-model-handler
   [:post "/v1/speech/model"] speech-model-handler
   [:post "/v1/sessions/:sid/voice"] voice-handler
   [:get "/v1/sessions/:sid/voice/jobs/:job-id"] voice-job-handler
   [:delete "/v1/sessions/:sid/voice/jobs/:job-id"] voice-job-handler
   [:get "/v1/sessions/:sid/voice/jobs/:job-id/events"] voice-job-events-handler
   [:post "/v1/sessions/:sid/speech"] speech-handler
   [:get "/v1/sessions/:sid/speech/jobs/:job-id"] speech-job-handler
   [:delete "/v1/sessions/:sid/speech/jobs/:job-id"] speech-job-handler
   [:get "/v1/sessions/:sid/speech/jobs/:job-id/events"] speech-job-events-handler
   [:get "/v1/sessions/:sid/speech/jobs/:job-id/audio"] speech-job-audio-handler})
