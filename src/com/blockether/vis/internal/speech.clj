(ns com.blockether.vis.internal.speech
  "The gateway's fixed local speech engines and their shared asynchronous job lifecycle."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.config :as config]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.speech.engine :as builtin-engine]
            [com.blockether.vis.internal.speech.synthesis :as synthesis])
  (:import [java.io File]
           [java.util UUID]))

(set! *warn-on-reflection* true)

(def directions
  "The two directions of speech, in the order a human meets them: audio in, audio out.
   Each is also the KEY its engines carry their work fn under."
  [:transcribe :synthesize])

(def direction? "Is this one of the two directions of speech?" (set directions))

(def direction-nouns
  "How each direction reads in a line a human is shown."
  {:transcribe "speech transcription" :synthesize "speech synthesis"})

(defn- check-direction!
  "Refuse anything that is not one of the two directions, naming both — a caller that
   invents `:speak` learns the vocabulary instead of resolving the wrong path."
  [direction]
  (when-not (direction? direction)
    (throw (ex-info
             (str "Unknown speech direction: " (pr-str direction))
             {:type :vis/speech-unknown-direction :direction direction :available directions})))
  direction)

(def phases
  "Every phase of speech work, in order, both directions together. `:uploading` is
   client-side (the bytes are in flight, the gateway has no job yet); a stored job
   starts at `:queued`."
  [:uploading :queued :preparing :transcribing :synthesizing :done :failed])

(def phase? "Is this a phase of the shared vocabulary?" (set phases))

(def terminal-phases "Phases a job never leaves." #{:done :failed})

(def work-phases
  "The phase an engine spends its own time in, per direction."
  {:transcribe :transcribing :synthesize :synthesizing})

(defn direction-phases
  "The phases ONE direction actually walks, in order: the shared vocabulary without the
   other direction's working phase. A surface that advertises transcription therefore
   never promises `:synthesizing` to a client that would wait for it."
  [direction]
  (check-direction! direction)
  (let [others (set (vals (dissoc work-phases direction)))]
    (into [] (remove others) phases)))

(defn- clamp-progress
  ^long [progress]
  (cond (nil? progress) 0
        (number? progress) (long (max 0 (min 100 (Math/round (double progress)))))
        :else 0))

;; Errors

(defn error-message
  "One readable line for a Throwable: the DEEPEST cause's own sentence, so an
   opaque wrapper (`ExecutionException: java.lang.RuntimeException: no model`)
   never hides the line a human needs."
  [^Throwable t]
  (let [root (->> (iterate (fn [^Throwable x]
                             (some-> x
                                     .getCause))
                           t)
                  (take-while some?)
                  last)]
    (or (not-empty (str/trim (str (ex-message root))))
        (not-empty (str/trim (str root)))
        "Unknown error")))

;; Fixed built-in engines

(def engine-env-vars
  "Optional operator override for each independent direction."
  {:transcribe "VIS_SPEECH_TRANSCRIPTION_ENGINE" :synthesize "VIS_SPEECH_SYNTHESIS_ENGINE"})

(def ^:private engines-by-direction
  {:transcribe [builtin-engine/descriptor] :synthesize synthesis/descriptors})

(defn env-engine-id
  "The engine id named by the direction's environment variable, or nil."
  [direction]
  (check-direction! direction)
  (some-> (config/extension-env-value (engine-env-vars direction))
          keyword))

(defn engines
  "The gateway's fixed built-in engines for `direction`."
  [direction]
  (check-direction! direction)
  (get engines-by-direction direction))

(defn engine
  "The `direction` engine with `id`, or nil."
  [direction id]
  (first (filter #(= id (:id %)) (engines direction))))

(defn default-engine
  "The configured engine for `direction`, falling back to its first built-in."
  [direction]
  (let [es
        (engines direction)

        configured
        (env-engine-id direction)]

    (or (when configured (engine direction configured)) (first es))))

(defn resolve-engine
  "The `direction` engine for an explicit `id` (nil selects the default)."
  [direction id]
  (or (if id (engine direction id) (default-engine direction))
      (throw (ex-info (if id
                        (str "Unknown " (direction-nouns direction) " engine: " (name id))
                        (str "No " (direction-nouns direction) " engine is available"))
                      {:type :vis/speech-engine-unavailable
                       :direction direction
                       :engine-id id
                       :available (mapv :id (engines direction))}))))

(defn public-engine
  "One engine in the shape every surface reports. `:is-voice-import` is a
   CAPABILITY, not a preference: it is the app's answer to whether it may offer
   \"add a voice\" at all."
  [engine]
  (cond-> {:id (name (:id engine)) :label (:label engine)}
    (:import-voice engine)
    (assoc :is-voice-import true)))

(defn readiness
  "What an engine says about its ability to work right now. With a `:voice-id`, ask
   the optional per-voice callback instead of the engine default."
  ([engine] (readiness engine nil))
  ([engine {:keys [voice-id]}]
   (let [f (if voice-id (:voice-model-state engine) (:model-state engine))]
     (if f
       (try (or (if voice-id (f voice-id) (f)) {:state :ready})
            (catch Throwable t {:state :failed :error (error-message t)}))
       {:state :ready}))))

(defn prepare!
  "Ask an engine, or one named voice, to start making itself ready."
  ([engine] (prepare! engine nil))
  ([engine {:keys [voice-id] :as opts}]
   (let [f (if voice-id (:start-voice-download engine) (:start-download engine))]
     (if f
       (try (or (if voice-id (f opts) (f)) (readiness engine opts))
            (catch Throwable t {:state :failed :error (error-message t)}))
       (readiness engine opts)))))

(defn ready? "Can this engine take work right now?" [engine] (= :ready (:state (readiness engine))))

;; Voices - the catalogue only a SPEAKING engine has

(defn- public-readiness
  [state]
  (cond-> {:status (name (or (:state state) :ready))}
    (number? (:progress state))
    (assoc :progress (:progress state))

    (:phase state)
    (assoc :phase (name (:phase state)))

    (:error state)
    (assoc :error (:error state))))

(defn public-voice
  "One voice in the wire vocabulary, including terms needed for explicit opt-in."
  [voice]
  (cond-> {:id (name (:id voice)) :label (or (not-empty (str (:label voice))) (name (:id voice)))}
    (:language voice)
    (assoc :language (name (:language voice)))

    (:license voice)
    (assoc :license (:license voice))

    (:notice voice)
    (assoc :notice (:notice voice))

    (:source-url voice)
    (assoc :source-url (:source-url voice))

    (:is-opt-in voice)
    (assoc :is-opt-in true)

    (:is-imported voice)
    (assoc :is-imported true)))

(defn public-sample
  "What a play button may promise for this voice, in the wire vocabulary: a
   sample it can play NOW, one that a press would first make out of very little,
   or - when neither - nothing at all, because the honest answer to \"can I hear
   this voice\" is sometimes \"install it first\"."
  [sample]
  (cond (:audio-path sample) {:is-sample-ready true}
        (:is-preparable sample) {:is-sample-preparable true}
        :else nil))

(defn voice-sample
  "Where this engine's sample of `voice-id` is, WITHOUT making one: `{:audio-path
   …}`, `{:is-preparable true}`, or nil - including for an engine that declares no
   sample seam, because a voice nobody can play back is not an error."
  [engine voice-id]
  (when-let [f (:voice-sample engine)]
    (try (f voice-id) (catch Throwable _ nil))))

(defn voices
  "The voices `engine` can speak in, with per-voice readiness when it declares that
   seam, and per-voice sample facts when it declares that one."
  [engine]
  (if-let [catalogue (:voices engine)]
    (mapv (fn [entry]
            (cond-> (public-voice entry)
              (:voice-model-state engine)
              (assoc :model
                (public-readiness (try ((:voice-model-state engine) (:id entry))
                                       (catch Throwable t
                                         {:state :failed :error (error-message t)}))))

              (:voice-sample engine)
              (merge (public-sample (voice-sample engine (:id entry))))))
          (catalogue))
    []))
(defn import-voice!
  "Hand `engine` a recording and get back the voice it became, in the shape every
   surface reports. `clip` is `{:path :voice-name :language :text}`, where `:text` is
   the clip's own transcript when the caller knows it.

   An engine that cannot learn a voice REFUSES BY NAME rather than silently doing
   nothing: cloning is a property of the engine, and a caller that asked the wrong one
   deserves to be told which one it asked."
  [engine clip]
  (if-let [f (:import-voice engine)]
    (public-voice (f clip))
    (throw (ex-info (str (or (:label engine) (name (:id engine)))
                         " cannot learn a voice from a recording")
                    {:type :vis/voice-import-unsupported :engine (name (:id engine))}))))

(defn forget-voice!
  "Delete an imported voice by id. True when there was one to delete, false when the
   engine never had it - a caller deleting a voice twice is not an error."
  [engine id]
  (if-let [f (:forget-voice engine)]
    (boolean (f id))
    (throw (ex-info (str (or (:label engine) (name (:id engine)))
                         " does not keep voices of its own")
                    {:type :vis/voice-import-unsupported :engine (name (:id engine))}))))

(defn voice-sample!
  "The sample WAV for `voice-id` - `{:audio-path :media-type}` - or nil when this
   voice has none to give.

   It MAKES one when the engine said that was cheap, so a press of play is
   answered by audio rather than by a second request: what an engine calls
   preparable is by contract small (a sample pack next to the weights) or local
   (speaking one sentence with a model already installed), never the voice
   download itself. A press of play is not consent to 115 MB."
  [engine voice-id]
  (let [found (voice-sample engine voice-id)]
    (cond (:audio-path found) found
          (and (:is-preparable found) (:prepare-voice-sample engine))
          ((:prepare-voice-sample engine) voice-id))))

(defn engines-info
  "One direction's engine catalogue as capabilities data: what exists and what is
   selected. A SPEAKING engine also carries the voices a caller may name, so
   `/v1/capabilities` populates a picker in one request instead of a call per engine.

   A catalogue that REFUSES leaves `:voices` absent rather than empty: an empty vector
   means \"one fixed voice, nothing to choose\", so a broken engine must not be reported
   as a silent one — and it must not take the whole capabilities response down with it."
  [direction]
  (let [es
        (engines direction)

        selected
        (try (default-engine direction) (catch Throwable _ nil))

        described
        (fn [e]
          (let [vs (when (= :synthesize direction) (try (voices e) (catch Throwable _ nil)))]
            (cond-> (public-engine e)
              vs
              (assoc :voices vs))))]

    {:engines (mapv described es)
     :selected (some-> selected
                       :id
                       name)}))

;; Synchronous work (the TUI's path - same engine, same progress)

(defn- reporter
  "Wrap a caller's `on-progress` so only the two keys of the vocabulary travel and a
   throwing or absent listener can never reach the engine."
  [on-progress]
  (fn [update-map]
    (when on-progress
      (try (on-progress (cond-> (select-keys update-map [:phase :progress])
                          (:progress update-map)
                          (update :progress clamp-progress)))
           (catch Throwable _ nil)))))

(defn- call-engine
  "Run one piece of work on THIS thread and answer the keys a job carries: a transcript
   for `:transcribe`, the audio file and its facts for `:synthesize`."
  [direction engine {:keys [audio-path text voice-id]} report job-id]
  (let [request
        (cond-> (case direction
                  :transcribe
                  {:audio-path (str audio-path) :on-progress report}

                  :synthesize
                  (cond-> {:text (str text) :on-progress report}
                    voice-id
                    (assoc :voice-id voice-id)))
          job-id
          (assoc :job-id job-id))

        result
        ((get engine direction) request)]

    (case direction
      :transcribe
      {:text (str result)}

      :synthesize
      (let [result-map
            (if (map? result) result {:audio-path result})

            path
            (str (:audio-path result-map))]

        (when (str/blank? path)
          (throw (ex-info "Synthesis engine returned no audio file"
                          {:type :vis/speech-empty-synthesis :result result})))
        (let [audio (cond-> (merge {:media-type "audio/wav"} (dissoc result-map :audio-path))
                      (.isFile (File. path))
                      (assoc :bytes (.length (File. path))))]
          {:audio-path path :audio audio})))))

(defn transcribe!
  "Run `audio-path` through the resolved transcription engine on THIS thread, reporting
   `{:phase :progress}` to `on-progress`. Returns the transcript."
  [{:keys [engine-id on-progress] :as request}]
  (let [engine
        (resolve-engine :transcribe engine-id)

        report
        (reporter on-progress)]

    (report {:phase :preparing :progress 0})
    (let [text (:text (call-engine :transcribe engine request report nil))]
      (report {:phase :done :progress 100})
      text)))

(defn synthesize!
  "Speak `text` through the resolved synthesis engine on THIS thread, reporting
   `{:phase :progress}` to `on-progress`. Returns the file the engine wrote —
   `{:audio-path :media-type :bytes …}` — which the CALLER owns and deletes."
  [{:keys [engine-id on-progress] :as request}]
  (let [engine
        (resolve-engine :synthesize engine-id)

        report
        (reporter on-progress)]

    (report {:phase :preparing :progress 0})
    (let [{:keys [audio-path audio]} (call-engine :synthesize engine request report nil)]
      (report {:phase :done :progress 100})
      (assoc audio :audio-path audio-path))))

;; Job store

(def ^:private ^:const job-ttl-ms
  "How long a finished job stays readable — long enough for a client that
   reconnected to still collect its transcript or its audio."
  (* 10 60 1000))

(def ^:private ^:const max-jobs 64)

(defonce ^:private jobs* (atom {}))

(defn- new-job-id [] (str "vj_" (str/replace (str (UUID/randomUUID)) "-" "")))

(defn public-job
  "A job in the shape every surface reads. The audio path never leaves: a recording is
   the client's own upload, and a spoken reply is FETCHED from the gateway rather than
   named to the caller."
  [job]
  (when job
    (cond-> {:id (:id job)
             :direction (name (:direction job))
             :engine (some-> (:engine-id job)
                             name)
             :phase (name (:phase job))
             :progress (clamp-progress (:progress job))
             :is-done (boolean (terminal-phases (:phase job)))
             :created-at (:created-at job)
             :updated-at (:updated-at job)}
      (:voice-id job)
      (assoc :voice (name (:voice-id job)))

      (:text job)
      (assoc :text (:text job))

      (:audio job)
      (assoc :audio (:audio job))

      (:error job)
      (assoc :error (:error job)))))

(defn- sweep
  "Drop finished jobs past their TTL, then the oldest above the cap. Running jobs
   are never dropped."
  [jobs]
  (let [t
        (long (util/now-ms))

        alive
        (into {}
              (remove (fn [[_ j]]
                        (and (terminal-phases (:phase j))
                             (> (- t (long (:updated-at j))) (long job-ttl-ms)))))
              jobs)]

    (if (<= (count alive) max-jobs)
      alive
      (into {} (take-last max-jobs (sort-by (comp :created-at val) alive))))))

(defn- discard-audio!
  "Delete the file a finished SYNTHESIS job wrote. The store is the only handle on that
   file, so a job that leaves without it leaks one WAV per spoken reply into the temp
   directory. A recording is the client's own upload and is never touched here."
  [job]
  (when (= :synthesize (:direction job))
    (when-let [path (:audio-path job)]
      (try (.delete (File. ^String (str path))) (catch Throwable _ nil))))
  nil)

(defn- discard-dropped-audio!
  "Delete the audio of every job that was in `before` and is gone from `after`. Runs
   OUTSIDE the swap: a compare-and-set retry may compute a different casualty list, and
   a deletion must never be replayed against a job that survived."
  [before after]
  (doseq [[id job] before]
    (when-not (contains? after id) (discard-audio! job)))
  nil)
(defn job "The public job for `id`, or nil." [id] (public-job (get @jobs* id)))

(defn job-audio-path
  "The file a finished SYNTHESIS job wrote, or nil. Host-side only: this is how the
   gateway streams the audio back, and it is deliberately absent from [[public-job]]."
  [id]
  (:audio-path (get @jobs* id)))

(defn forget!
  "Drop a job (a client that collected its transcript need not wait for the TTL). A
   spoken reply's audio goes with it — see [[discard-dropped-audio!]]."
  [id]
  (let [[before after] (swap-vals! jobs* dissoc id)]
    (discard-dropped-audio! before after))
  nil)

;; Atom watches push job changes directly into SSE queues.

(defn watch!
  "Call `f` with the public job on every change until the returned fn is called."
  [job-id f]
  (let [k (UUID/randomUUID)]
    (add-watch jobs*
               k
               (fn [_ _ before after]
                 (let [old (get before job-id)
                       new (get after job-id)]

                   (when (not= old new) (try (f (public-job new)) (catch Throwable _ nil))))))
    (fn unwatch []
      (remove-watch jobs* k)
      nil)))

(defn- advance!
  "Move a live job forward without letting progress regress inside one phase."
  [job-id update-map]
  (swap! jobs* (fn [jobs]
                 (if-let [job (get jobs job-id)]
                   (if (terminal-phases (:phase job))
                     jobs
                     (let [same-phase? (or (nil? (:phase update-map))
                                           (= (:phase update-map) (:phase job)))
                           floor (if same-phase? (clamp-progress (:progress job)) 0)]

                       (assoc jobs
                         job-id (merge job
                                       (cond-> (assoc update-map :updated-at (util/now-ms))
                                         (:progress update-map)
                                         (update :progress #(max (clamp-progress %) floor))

                                         (and (not same-phase?)
                                              (nil? (:progress update-map))
                                              (not (terminal-phases (:phase update-map))))
                                         (assoc :progress 0))))))
                   jobs)))
  nil)

(defn- run-job!
  [direction job-id engine request on-done]
  (try (advance! job-id {:phase :preparing :progress 0})
       (let [report
             (fn [update-map]
               (advance! job-id (select-keys update-map [:phase :progress])))

             result
             (call-engine direction engine request report job-id)]

         ;; No `:transcribing 100` filler: the DONE frame is the completion, and a
         ;; pushed stream would otherwise spend a frame saying the same thing twice.
         (advance! job-id
                   (assoc result
                     :phase :done
                     :progress 100)))
       (catch Throwable t (advance! job-id {:phase :failed :error (error-message t)}))
       (finally (when on-done (try (on-done (job job-id)) (catch Throwable _ nil)))))
  (job job-id))

(defn- queue-job!
  "Resolve the engine and store the QUEUED job, or refuse BEFORE a job exists. Returns
   `[engine job]` — the private job, not the public one."
  [direction {:keys [engine-id voice-id]}]
  (let [engine
        (resolve-engine direction engine-id)

        id
        (new-job-id)

        t
        (util/now-ms)

        job
        (cond-> {:id id
                 :direction direction
                 :engine-id (:id engine)
                 :phase :queued
                 :progress 0
                 :created-at t
                 :updated-at t}
          voice-id
          (assoc :voice-id voice-id))]

    (let [[before after] (swap-vals! jobs*
                                     (fn [jobs]
                                       (assoc (sweep jobs) id job)))]
      (discard-dropped-audio! before after))
    [engine job]))

(defn submit!
  "Accept one piece of speech work — a recording to `:transcribe`, a line of text to
   `:synthesize` — and return the QUEUED job immediately, so the caller answers 202 and
   the human is told the server took it. The engine runs on its own thread; `on-done`
   (optional) receives the final public job and is where a temp file is deleted.

   An unknown engine is refused HERE, before a job exists, so the caller can
   answer 400 rather than inventing a job that instantly fails."
  [direction {:keys [on-done] :as request}]
  (let [[engine job] (queue-job! direction request)]
    (future (run-job! direction (:id job) engine request on-done))
    (public-job job)))

(defn submit-sync!
  "[[submit!]] without the thread — the same job lifecycle run inline. Tests and
   any caller that wants determinism use this."
  [direction {:keys [on-done] :as request}]
  (let [[engine job] (queue-job! direction request)]
    (run-job! direction (:id job) engine request on-done)))

(defn reset-jobs!
  "Forget every job (tests)."
  []
  (let [[before _] (reset-vals! jobs* {})]
    (discard-dropped-audio! before {}))
  nil)
