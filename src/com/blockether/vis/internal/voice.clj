(ns com.blockether.vis.internal.voice
  "Speech as a JOB with a visible lifecycle, in BOTH directions, and the engine that
   listens or speaks as a REPLACEABLE ENGINE. Three things live here and nothing else:

   1. The DIRECTIONS. Voice is one subject with two directions — `:transcribe` turns a
      recording into text, `:synthesize` turns text into audio — and every question a
      surface asks (which engines exist, which one is selected, is its model
      downloaded, where did that job go) is asked PER DIRECTION. A second namespace for
      speaking would answer those four questions in a second vocabulary that drifts, so
      the direction is an ARGUMENT here and never a copy of this file.

   2. The ENGINE REGISTRY. An engine is one map: an `:id`, a human `:label` and the work
      fn under the direction's own key — `:transcribe` of `{:audio-path :on-progress}`
      returning text, `:synthesize` of `{:text :voice-id :on-progress}` returning the
      audio file it wrote. The local Parakeet model is registered by
      `vis-foundation-voice`; a colleague who wants a whisper.cpp server or a different
      speaker instead writes an extension that calls [[register-engine!]] and changes
      NOTHING here, in the gateway, in the TUI or in the app — the engine is chosen by
      `VIS_VOICE_ENGINE` / `VIS_SPEECH_ENGINE`, by [[set-default-engine!]], or per
      request. An entry is exactly ONE direction: something that can do both registers
      once per direction, so `:synthesize` can never resolve to an engine that only
      listens.

   3. The JOB STORE, shared by both directions. Transcription used to be one blocking
      POST that either returned text or timed out, so no surface could say where it was.
      A job instead carries a PHASE and a 0..100 `:progress` readable at any moment:
      `:queued` -> `:preparing` -> `:transcribing` | `:synthesizing` -> `:done` |
      `:failed`. `:uploading` is the client's own half — the bytes are still travelling
      and no job exists yet — and is part of the vocabulary so every surface names the
      same things. Every change is PUSHED to [[watch!]] watchers, so the gateway streams
      a job as SSE and no surface ever polls."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.toggles :as toggles])
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
  {:transcribe "voice transcription" :synthesize "speech synthesis"})

(defn- check-direction!
  "Refuse anything that is not one of the two directions, naming both — a caller that
   invents `:speak` learns the vocabulary instead of silently registering into nothing."
  [direction]
  (when-not (direction? direction)
    (throw (ex-info
             (str "Unknown speech direction: " (pr-str direction))
             {:type :vis/voice-unknown-direction :direction direction :available directions})))
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
  (let
    [root (->> (iterate (fn [^Throwable x]
                          (some-> x
                                  .getCause))
                        t)
               (take-while some?)
               last)]
    (or (not-empty (str/trim (str (ex-message root))))
        (not-empty (str/trim (str root)))
        "Unknown error")))

;; The `speech` feature toggle

(def speech-toggle-id
  "The feature toggle that decides whether Vis speaks at all. Registered HERE, at the
   one door both spoken paths go through, so turning it off in a project's `vis.yml`
   silences synthesis everywhere instead of in whichever surface remembered to ask."
  "speech")

;; Registered at load: `toggles/hydrate-from-config!` runs at process start and again on
;; `/reload`, so a project override in the merged config applies without a restart.
(toggles/register-toggle! {:id speech-toggle-id
                           :label "Spoken replies"
                           :description "Speak replies aloud through a text-to-speech engine."
                           :default true
                           :owner :vis
                           :persist? true
                           :group :voice})

(defn speech-enabled?
  "Is spoken output offered at all? Fail-closed, like every toggle."
  []
  (toggles/enabled? speech-toggle-id))

(defn- check-speech-enabled!
  []
  (when-not (speech-enabled?)
    (throw (ex-info "Spoken replies are turned off"
                    {:type :vis/voice-direction-disabled
                     :direction :synthesize
                     :toggle speech-toggle-id})))
  nil)

;; Engine registry

(def engine-env-vars
  "Operator override naming the engine every surface should use, per direction. Two
   variables because the two directions are chosen independently: a laptop may
   transcribe with a local model and speak through a server."
  {:transcribe "VIS_VOICE_ENGINE" :synthesize "VIS_SPEECH_ENGINE"})

(def builtin-engine-nses
  "Namespaces soft-required once before the registry is read. Each must expose a
   0-arity `register!` that registers whatever directions it serves. Soft: a build
   without the extension simply has no engine and every surface answers \"unavailable\"
   instead of failing to load."
  ["com.blockether.vis.ext.foundation-voice.engine"
   "com.blockether.vis.ext.foundation-voice.speech"])

(defonce ^:private engines* (atom {}))
(defonce ^:private default-engine* (atom {}))
;; What each builtin engine namespace did the last time it was tried:
;; `:registered`, `:absent`, or `{:error "…"}`.
(defonce ^:private builtins* (atom {}))

(defn engine-error
  "nil when `engine` is usable in `direction`, else the one-line reason. The direction is
   also the KEY the work fn lives under, so an engine that only listens can never be
   registered as one that speaks."
  [direction engine]
  (check-direction! direction)
  (cond (not (map? engine)) "engine must be a map"
        (not (keyword? (:id engine))) "engine :id must be a keyword"
        (not (ifn? (get engine direction))) (str "engine " direction " must be a function")
        (and (contains? engine :label) (not (string? (:label engine))))
        "engine :label must be a string"
        (and (contains? engine :voices) (not (ifn? (:voices engine))))
        "engine :voices must be a function"
        (and (contains? engine :import-voice) (not (ifn? (:import-voice engine))))
        "engine :import-voice must be a function"
        (and (contains? engine :forget-voice) (not (ifn? (:forget-voice engine))))
        "engine :forget-voice must be a function"
        :else nil))

(defn register-engine!
  "Register (or replace, by `:id`) an engine for ONE direction. Returns its id.

   `:transcribe` is called with `{:audio-path :on-progress :job-id}` and returns the
   transcript. `:synthesize` is called with `{:text :voice-id :on-progress :job-id}` and
   returns the audio FILE it wrote — a path, or a map carrying `:audio-path` plus the
   facts a player needs (`:media-type`, `:sample-rate`, `:duration-ms`).
   `:on-progress` takes `{:phase :progress}` (either key optional, `:progress` 0..100)
   and is how the human sees where the work is.

   A speaking engine that CLONES a recording may also declare `:import-voice`
   (`{:path :voice-name :language :text}` -> the voice it became) and
   `:forget-voice` (id -> true when one was deleted). Declaring them is what makes
   \"add a voice\" appear on every surface at once; declaring neither is a refusal
   the gateway reports by name instead of a button that does nothing."
  [direction engine]
  (when-let [reason (engine-error direction engine)]
    (throw (ex-info
             (str "Invalid " (direction-nouns direction) " engine: " reason)
             {:type :vis/voice-invalid-engine :direction direction :engine engine :reason reason})))
  (let [engine (update engine :label #(or % (name (:id engine))))]
    (swap! engines* update
      direction
      (fn [es]
        (conj (vec (remove #(= (:id engine) (:id %)) es)) engine)))
    (:id engine)))

(defn unregister-engine!
  "Drop an engine from one direction by id (used by tests and by an extension being
   unloaded). The other direction's entry for the same id stays registered."
  [direction id]
  (check-direction! direction)
  (swap! engines* update
    direction
    (fn [es]
      (vec (remove #(= id (:id %)) es))))
  (swap! default-engine* (fn [ds]
                           (if (= id (get ds direction)) (dissoc ds direction) ds)))
  id)

(defn set-default-engine!
  "Pin the engine `direction` uses when a caller names none. `nil` clears the pin."
  [direction id]
  (check-direction! direction)
  (swap! default-engine* (fn [ds]
                           (if (nil? id) (dissoc ds direction) (assoc ds direction id))))
  id)

(defn env-engine-id
  "The engine id an OPERATOR named for `direction` in the environment, or nil. Public
   because it is the outermost rank of [[default-engine]] and a test proves that rank
   without mutating the environment of a running process."
  [direction]
  (some-> (System/getenv (engine-env-vars direction))
          str/trim
          not-empty
          keyword))

(defn- builtin-absent?
  "True when `t` says this namespace is simply NOT IN THIS BUILD - a jar nobody
   assembled - rather than a namespace that failed while loading. Clojure reports
   both as a `FileNotFoundException`, so the file it names must be the
   namespace's own before absence is believed."
  [ns-name t]
  (and (instance? java.io.FileNotFoundException t)
       (str/includes? (str (ex-message t))
                      (-> (str ns-name)
                          (str/replace "-" "_")
                          (str/replace "." "/")))))

(defn- load-builtin!
  "Load ONE builtin engine namespace and run its `register!`. `:registered` when it
   did, `:absent` when this build has no such namespace, else the reason it failed
   - remembered only to be REPORTED, never to stop the next attempt."
  [ns-name]
  (try (if-let [register! (requiring-resolve (symbol ns-name "register!"))]
         (do (register!) :registered)
         {:error (str ns-name " carries no register! fn")})
       (catch Throwable t (if (builtin-absent? ns-name t) :absent {:error (error-message t)}))))

(defn- load-builtins!
  "Register every builtin engine this build carries, RETRYING whichever failed.

   Regression, issue: one latch guarded the whole loop and was set BEFORE any
   `register!` ran, so a single transient failure - a native library still
   downloading, a model directory half written, a class that lost a race - was
   PERMANENT. Every surface answered \"no engine\" for the rest of the process and
   the only cure anyone found was restarting Vis. Success and ABSENCE are
   remembered (a build without the extension must not pay a `requiring-resolve`
   per call); a failure never is."
  []
  (doseq
    [ns-name
     builtin-engine-nses

     :when (not (#{:registered :absent} (get @builtins* ns-name)))]

    (swap! builtins* assoc ns-name (load-builtin! ns-name)))
  nil)

(defn builtin-load-failures
  "Why a builtin engine namespace did not register, by namespace - empty when each
   one registered or is simply absent from this build. A surface with no engine
   says THIS instead of \"unavailable\": the reason is the whole difference between
   a human who installs what is missing and one who restarts and hopes."
  []
  (into {}
        (keep (fn [[ns-name state]]
                (when (map? state) [ns-name (:error state)])))
        @builtins*))

(defn engines
  "Every engine registered for `direction`, in registration order."
  [direction]
  (check-direction! direction)
  (load-builtins!)
  (get @engines* direction []))

(defn engine
  "The `direction` engine with `id`, or nil."
  [direction id]
  (first (filter #(= id (:id %)) (engines direction))))

(defn choose-engine
  "The engine a direction settles on, given what an operator NAMED in the environment,
   what a caller PINNED, and what is registered — in that order, so an operator outranks
   the code and the code outranks the accident of registration order. Pure, so the
   precedence is provable without touching the process environment."
  [{:keys [env-id pinned-id] es :engines}]
  (let
    [by-id #(first (filter (fn [e]
                             (= % (:id e)))
                           es))]
    (or (some-> env-id
                by-id)
        (some-> pinned-id
                by-id)
        (first es))))

(defn default-engine
  "The engine `direction` uses when a caller names none: its own environment variable
   first (`VIS_VOICE_ENGINE` for listening, `VIS_SPEECH_ENGINE` for speaking — an
   operator outranks the code), then [[set-default-engine!]], then the first registered.
   Neither direction can ever resolve to the other's default."
  [direction]
  (choose-engine {:env-id (env-engine-id direction)
                  :pinned-id (get @default-engine* direction)
                  :engines (engines direction)}))

(defn resolve-engine
  "The `direction` engine for an explicit `id` (nil = the default). Throws with the ids
   that DO exist, so a typo in `VIS_SPEECH_ENGINE` reads as a typo - and with the
   REASON when an engine this build carries failed to load, because \"none is
   registered\" is a fact about the process, not advice a human can follow."
  [direction id]
  (or (if id (engine direction id) (default-engine direction))
      (let [failures (builtin-load-failures)]
        (throw (ex-info (cond id (str "Unknown " (direction-nouns direction) " engine: " (name id))
                              (seq failures) (str "No " (direction-nouns direction)
                                                  " engine is registered - "
                                                  (str/join "; " (vals failures)))
                              :else (str "No " (direction-nouns direction) " engine is registered"))
                        {:type :vis/voice-engine-unavailable
                         :direction direction
                         :engine-id id
                         :failures failures
                         :available (mapv :id (engines direction))})))))

(defn public-engine
  "One engine in the shape every surface reports. `:is-voice-import` is a
   CAPABILITY, not a preference: it is the app's answer to whether it may offer
   \"add a voice\" at all."
  [engine]
  (cond-> {:id (name (:id engine)) :label (:label engine)}
    (:import-voice engine)
    (assoc :is-voice-import true)))

(defn readiness
  "What an engine says about its ability to work RIGHT NOW, in the shape the wire
   already speaks: `{:state :ready|:absent|:downloading|:failed :progress? :phase?
   :error?}`. An engine that needs no preparation (a remote server) simply omits
   `:model-state` and is always ready — readiness is the ENGINE's question, never a fact
   the gateway knows about one particular model."
  [engine]
  (if-let [f (:model-state engine)]
    (try (or (f) {:state :ready}) (catch Throwable t {:state :failed :error (error-message t)}))
    {:state :ready}))

(defn prepare!
  "Ask the engine to start making itself ready (idempotent, NON-blocking) and
   return its readiness. A no-op for an engine that declares none."
  [engine]
  (if-let [f (:start-download engine)]
    (try (or (f) (readiness engine)) (catch Throwable t {:state :failed :error (error-message t)}))
    (readiness engine)))

(defn ready? "Can this engine take work right now?" [engine] (= :ready (:state (readiness engine))))

;; Voices - the catalogue only a SPEAKING engine has

(defn public-voice
  "One voice in the shape every surface reports: the id a caller sends back, a human
   label, and the language it speaks. A voice a user has to install DELIBERATELY says so,
   so a picker can mark it instead of discovering the refusal after somebody chose it.

   `:is-imported` marks a voice that came from a recording somebody handed to Vis
   rather than one that shipped: the surface offering \"add a voice\" is the one that
   has to offer \"remove it\" too."
  [voice]
  (cond-> {:id (name (:id voice)) :label (or (not-empty (str (:label voice))) (name (:id voice)))}
    (:language voice)
    (assoc :language (name (:language voice)))

    (:is-opt-in voice)
    (assoc :is-opt-in true)

    (:is-imported voice)
    (assoc :is-imported true)))

(defn voices
  "The voices `engine` can speak in, in the shape every surface reports. An engine with
   ONE fixed voice declares no `:voices` and answers `[]` — a caller then names none. A
   refusal from the engine PROPAGATES: an empty list means \"nothing to choose\", never
   \"the catalogue failed\", and the surface reports the reason it was given."
  [engine]
  (if-let [f (:voices engine)]
    (mapv public-voice (f))
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

(defn engines-info
  "One direction's engine catalogue as capabilities data: what exists and what is
   selected. A SPEAKING engine also carries the voices a caller may name, so
   `/v1/capabilities` populates a picker in one request instead of a call per engine.

   A catalogue that REFUSES leaves `:voices` absent rather than empty: an empty vector
   means \"one fixed voice, nothing to choose\", so a broken engine must not be reported
   as a silent one — and it must not take the whole capabilities response down with it."
  [direction]
  (let
    [es
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

(defn- engine-request
  "The map an engine is called with: the work itself plus the progress channel. Each
   direction names its own input — a recording for `:transcribe`, a line of text and an
   optional voice for `:synthesize` — and neither ever sees the job store."
  [direction {:keys [audio-path text voice-id]} report job-id]
  (cond->
    (case direction
      :transcribe
      {:audio-path (str audio-path) :on-progress report}

      :synthesize
      (cond-> {:text (str text) :on-progress report}
        voice-id
        (assoc :voice-id voice-id)))
    job-id
    (assoc :job-id job-id)))

(defn- synthesis-result
  "What a `:synthesize` engine handed back, normalized: the FILE it wrote plus the facts
   a player needs. A bare path is accepted — most engines write a WAV and have nothing
   else to say — and a nameless result is a refusal, never a job that says `:done` with
   silence behind it."
  [result]
  (let
    [m
     (if (map? result) result {:audio-path result})

     path
     (str (:audio-path m))]

    (when (str/blank? path)
      (throw (ex-info "Synthesis engine returned no audio file"
                      {:type :vis/voice-empty-synthesis :result result})))
    (cond-> (assoc (merge {:media-type "audio/wav"} (dissoc m :audio-path)) :audio-path path)
      (.isFile (File. path))
      (assoc :bytes (.length (File. path))))))

(defn- call-engine
  "Run one piece of work on THIS thread and answer the keys a job carries: a transcript
   for `:transcribe`, the audio file and its facts for `:synthesize`."
  [direction engine request report job-id]
  (let
    [work
     (get engine direction)

     result
     (work (engine-request direction request report job-id))]

    (case direction
      :transcribe
      {:text (str result)}

      :synthesize
      (let [r (synthesis-result result)]
        {:audio-path (:audio-path r) :audio (dissoc r :audio-path)}))))

(defn transcribe!
  "Run `audio-path` through the resolved transcription engine on THIS thread, reporting
   `{:phase :progress}` to `on-progress`. Returns the transcript."
  [{:keys [engine-id on-progress] :as request}]
  (let
    [engine
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
  (check-speech-enabled!)
  (let
    [engine
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

(defn- now-ms [] (System/currentTimeMillis))

(defn- new-job-id [] (str "vj_" (str/replace (str (UUID/randomUUID)) "-" "")))

(defn public-job
  "A job in the shape every surface reads. The audio path never leaves: a recording is
   the client's own upload, and a spoken reply is FETCHED from the gateway rather than
   named to the caller."
  [job]
  (when job
    (cond->
      {:id (:id job)
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
  (let
    [t
     (long (now-ms))

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

;; Watchers - a job PUSHES, nobody polls

(defonce ^:private watchers* (atom {}))

(defn watch!
  "Call `f` with the PUBLIC job on every state change of `job-id`, until the
   returned zero-arg fn is called.

   This is what makes progress a STREAM rather than a poll: the gateway's SSE
   body parks on a queue this fills, so a percentage reaches the human the
   instant the engine reports it instead of on the next tick of a timer. `f`
   runs on the ENGINE's own thread, so it must only ENQUEUE - a slow or throwing
   watcher can never stall the work, and a throw costs that one delivery
   and nothing else."
  [job-id f]
  (let [k (str (UUID/randomUUID))]
    (swap! watchers* assoc-in [job-id k] f)
    (fn unwatch []
      (swap! watchers* (fn [ws]
                         (let [left (dissoc (get ws job-id) k)]
                           (if (seq left) (assoc ws job-id left) (dissoc ws job-id)))))
      nil)))

(defn- notify-watchers!
  [job-id job]
  (doseq [f (vals (get @watchers* job-id))]
    (try (f job) (catch Throwable _ nil)))
  nil)

(defn- advance!
  "Move a live job forward and tell its watchers. Progress never goes BACKWARDS
   inside a phase - a surface must not flicker 60% -> 20% - but a new phase is a
   new scale and starts over: `:preparing 100` is followed by `:transcribing 0`.
   A TERMINAL phase keeps the percentage it arrived with: `:failed` at 40% is
   where the engine gave up, not zero."
  [job-id update-map]
  (let
    [[before after] (swap-vals! jobs*
                                (fn [jobs]
                                  (if-let [j (get jobs job-id)]
                                    (if (terminal-phases (:phase j))
                                      jobs
                                      (let
                                        [same-phase? (or (nil? (:phase update-map))
                                                         (= (:phase update-map) (:phase j)))
                                         floor (if same-phase? (clamp-progress (:progress j)) 0)]

                                        (assoc jobs
                                          job-id
                                          (merge j
                                                 (cond-> (assoc update-map :updated-at (now-ms))
                                                   (:progress update-map)
                                                   (update :progress
                                                           (fn [p]
                                                             (max (clamp-progress p) floor)))

                                                   (and (not same-phase?)
                                                        (nil? (:progress update-map))
                                                        (not (terminal-phases (:phase update-map))))
                                                   (assoc :progress 0))))))
                                    jobs)))]
    (when (not= (get before job-id) (get after job-id))
      (notify-watchers! job-id (public-job (get after job-id)))))
  nil)

(defn- run-job!
  [direction job-id engine request on-done]
  (try (advance! job-id {:phase :preparing :progress 0})
       (let
         [report
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
  (when (= :synthesize direction) (check-speech-enabled!))
  (let
    [engine
     (resolve-engine direction engine-id)

     id
     (new-job-id)

     t
     (now-ms)

     job
     (cond->
       {:id id
        :direction direction
        :engine-id (:id engine)
        :phase :queued
        :progress 0
        :created-at t
        :updated-at t}
       voice-id
       (assoc :voice-id voice-id))]

    (let
      [[before after] (swap-vals! jobs*
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
  "Forget every job and every watcher (tests)."
  []
  (let [[before _] (reset-vals! jobs* {})]
    (discard-dropped-audio! before {}))
  (reset! watchers* {})
  nil)
