(ns com.blockether.vis.internal.voice
  "Transcription as a JOB with a visible lifecycle, and the transcriber itself as
   a REPLACEABLE ENGINE. Two things live here and nothing else:

   1. The ENGINE REGISTRY. An engine is one map: an `:id`, a human `:label` and a
      `:transcribe` fn of `{:audio-path :on-progress}` returning text. The local
      Parakeet model is registered by `vis-foundation-voice`; a colleague who
      wants a whisper.cpp server instead writes an extension that calls
      [[register-engine!]] and changes NOTHING here, in the gateway, in the TUI
      or in the app — the engine is chosen by `VIS_VOICE_ENGINE`, by
      [[set-default-engine!]], or per request.

   2. The JOB STORE. Transcription used to be one blocking POST that either
      returned text or timed out, so no surface could say where it was. A job
      instead carries a PHASE and a 0..100 `:progress` readable at any moment:
      `:queued` -> `:preparing` -> `:transcribing` -> `:done` | `:failed`.
      `:uploading` is the client's own half — the bytes are still travelling and
      no job exists yet — and is part of the vocabulary so every surface names
      the same five things."
  (:require [clojure.string :as str])
  (:import [java.util UUID]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Phases
;; =============================================================================

(def phases
  "Every transcription phase, in order. `:uploading` is client-side (the bytes are
   in flight, the gateway has no job yet); a stored job starts at `:queued`."
  [:uploading :queued :preparing :transcribing :done :failed])

(def phase? "Is this a phase of the shared vocabulary?" (set phases))

(def terminal-phases "Phases a job never leaves." #{:done :failed})

(defn- clamp-progress
  ^long [progress]
  (cond (nil? progress) 0
        (number? progress) (long (max 0 (min 100 (Math/round (double progress)))))
        :else 0))

;; =============================================================================
;; Errors
;; =============================================================================

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

;; =============================================================================
;; Engine registry
;; =============================================================================

(def engine-env-var
  "Operator override naming the engine every surface should use."
  "VIS_VOICE_ENGINE")

(def builtin-engine-nses
  "Namespaces soft-required once before the registry is read. Each must expose a
   0-arity `register!`. Soft: a build without the extension simply has no engine
   and every surface answers \"unavailable\" instead of failing to load."
  ["com.blockether.vis.ext.foundation-voice.engine"])

(defonce ^:private engines* (atom []))
(defonce ^:private default-engine* (atom nil))
(defonce ^:private builtins-loaded* (atom false))

(defn engine-error
  "nil when `engine` is a usable engine, else the one-line reason."
  [engine]
  (cond (not (map? engine)) "engine must be a map"
        (not (keyword? (:id engine))) "engine :id must be a keyword"
        (not (ifn? (:transcribe engine))) "engine :transcribe must be a function"
        (and (contains? engine :label) (not (string? (:label engine))))
        "engine :label must be a string"
        :else nil))

(defn register-engine!
  "Register (or replace, by `:id`) a transcription engine. Returns its id.

   `:transcribe` is called with `{:audio-path :on-progress :job-id}` and returns
   the transcript. `:on-progress` takes `{:phase :progress}` (either key optional,
   `:progress` 0..100) and is how the human sees where the work is."
  [engine]
  (when-let [reason (engine-error engine)]
    (throw (ex-info (str "Invalid voice engine: " reason)
                    {:type :vis/voice-invalid-engine :engine engine :reason reason})))
  (let [engine (update engine :label #(or % (name (:id engine))))]
    (swap! engines* (fn [es]
                      (conj (vec (remove #(= (:id engine) (:id %)) es)) engine)))
    (:id engine)))

(defn unregister-engine!
  "Drop an engine by id (used by tests and by an extension being unloaded)."
  [id]
  (swap! engines* (fn [es]
                    (vec (remove #(= id (:id %)) es))))
  (swap! default-engine* #(when-not (= id %) %))
  id)

(defn set-default-engine!
  "Pin the engine used when a caller names none. `nil` clears the pin."
  [id]
  (reset! default-engine* id))

(defn- env-engine-id
  []
  (some-> (System/getenv engine-env-var)
          str/trim
          not-empty
          keyword))

(defn- load-builtins!
  []
  (when (compare-and-set! builtins-loaded* false true)
    (doseq [ns-name builtin-engine-nses]
      (try (when-let [register! (requiring-resolve (symbol ns-name "register!"))]
             (register!))
           (catch Throwable _ nil))))
  nil)

(defn engines "Every registered engine, in registration order." [] (load-builtins!) @engines*)

(defn engine "The engine with `id`, or nil." [id] (first (filter #(= id (:id %)) (engines))))

(defn default-engine
  "The engine used when a caller names none: `VIS_VOICE_ENGINE` first (an
   operator outranks the code), then [[set-default-engine!]], then the first
   registered."
  []
  (let
    [es
     (engines)

     by-id
     #(first (filter (fn [e]
                       (= % (:id e)))
                     es))]

    (or (some-> (env-engine-id)
                by-id)
        (some-> @default-engine*
                by-id)
        (first es))))

(defn resolve-engine
  "The engine for an explicit `id` (nil = the default). Throws with the ids that
   DO exist, so a typo in `VIS_VOICE_ENGINE` reads as a typo."
  [id]
  (or (if id (engine id) (default-engine))
      (throw (ex-info (if id
                        (str "Unknown voice engine: " (name id))
                        "No voice transcription engine is registered")
                      {:type :vis/voice-engine-unavailable
                       :engine-id id
                       :available (mapv :id (engines))}))))

(defn public-engine
  "One engine in the shape every surface reports."
  [engine]
  {:id (name (:id engine)) :label (:label engine)})

(defn readiness
  "What an engine says about its ability to transcribe RIGHT NOW, in the shape the
   wire already speaks: `{:state :ready|:absent|:downloading|:failed :progress?
   :phase? :error?}`. An engine that needs no preparation (a remote server) simply
   omits `:model-state` and is always ready — readiness is the ENGINE's question,
   never a fact the gateway knows about one particular model."
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

(defn ready?
  "Can this engine take a recording right now?"
  [engine]
  (= :ready (:state (readiness engine))))

(defn engines-info
  "The engine catalogue as capabilities data: what exists and what is selected."
  []
  (let
    [es
     (engines)

     selected
     (try (default-engine) (catch Throwable _ nil))]

    {:engines (mapv public-engine es)
     :selected (some-> selected
                       :id
                       name)}))

;; =============================================================================
;; Synchronous transcription (the TUI's path — same engine, same progress)
;; =============================================================================

(defn transcribe!
  "Run `audio-path` through the resolved engine on THIS thread, reporting
   `{:phase :progress}` to `on-progress`. Returns the transcript."
  [{:keys [audio-path engine-id on-progress]}]
  (let
    [engine
     (resolve-engine engine-id)

     report
     (fn [update-map]
       (when on-progress
         (try (on-progress (cond-> (select-keys update-map [:phase :progress])
                             (:progress update-map)
                             (update :progress clamp-progress)))
              (catch Throwable _ nil))))]

    (report {:phase :preparing :progress 0})
    (let [text (str ((:transcribe engine) {:audio-path (str audio-path) :on-progress report}))]
      (report {:phase :done :progress 100})
      text)))

;; =============================================================================
;; Job store
;; =============================================================================

(def ^:private ^:const job-ttl-ms
  "How long a finished job stays readable — long enough for a client that
   reconnected to still collect its transcript."
  (* 10 60 1000))

(def ^:private ^:const max-jobs 64)

(defonce ^:private jobs* (atom {}))

(defn- now-ms [] (System/currentTimeMillis))

(defn- new-job-id [] (str "vj_" (str/replace (str (UUID/randomUUID)) "-" "")))

(defn public-job
  "A job in the shape every surface reads. The audio path never leaves."
  [job]
  (when job
    (cond->
      {:id (:id job)
       :engine (some-> (:engine-id job)
                       name)
       :phase (name (:phase job))
       :progress (clamp-progress (:progress job))
       :is-done (boolean (terminal-phases (:phase job)))
       :created-at (:created-at job)
       :updated-at (:updated-at job)}
      (:text job)
      (assoc :text (:text job))

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

(defn job "The public job for `id`, or nil." [id] (public-job (get @jobs* id)))

(defn forget!
  "Drop a job (a client that collected its transcript need not wait for the TTL)."
  [id]
  (swap! jobs* dissoc id)
  nil)

(defn- advance!
  "Move a live job forward. Progress never goes BACKWARDS inside a phase — a
   surface must not flicker 60% → 20% — but a new phase is a new scale and starts
   over: `:preparing 100` is followed by `:transcribing 0`."
  [job-id update-map]
  (swap! jobs* (fn [jobs]
                 (if-let [j (get jobs job-id)]
                   (if (terminal-phases (:phase j))
                     jobs
                     (let
                       [same-phase? (or (nil? (:phase update-map))
                                        (= (:phase update-map) (:phase j)))
                        floor (if same-phase? (clamp-progress (:progress j)) 0)]

                       (assoc jobs
                         job-id (merge j
                                       (cond-> (assoc update-map :updated-at (now-ms))
                                         (:progress update-map)
                                         (update :progress
                                                 (fn [p]
                                                   (max (clamp-progress p) floor)))

                                         (and (not same-phase?) (nil? (:progress update-map)))
                                         (assoc :progress 0))))))
                   jobs)))
  nil)

(defn- run-job!
  [job-id engine audio-path on-done]
  (try
    (advance! job-id {:phase :preparing :progress 0})
    (let
      [text (str ((:transcribe engine)
                   {:audio-path (str audio-path)
                    :job-id job-id
                    :on-progress (fn [update-map]
                                   (advance! job-id
                                             (select-keys update-map [:phase :progress])))}))]
      (advance! job-id {:phase :transcribing :progress 100})
      (swap! jobs*
        (fn [jobs]
          (cond-> jobs
            (contains? jobs job-id)
            (update job-id assoc :phase :done :progress 100 :text text :updated-at (now-ms))))))
    (catch Throwable t
      (swap! jobs*
        (fn [jobs]
          (cond-> jobs
            (contains? jobs job-id)
            (update job-id assoc :phase :failed :error (error-message t) :updated-at (now-ms))))))
    (finally (when on-done (try (on-done (job job-id)) (catch Throwable _ nil)))))
  (job job-id))

(defn submit!
  "Accept `audio-path` for transcription and return the QUEUED job immediately —
   the caller answers 202 and the human is told the server took the recording.
   The engine runs on its own thread; `on-done` (optional) receives the final
   public job and is where a temp file is deleted.

   An unknown engine is refused HERE, before a job exists, so the caller can
   answer 400 rather than inventing a job that instantly fails."
  [{:keys [audio-path engine-id on-done]}]
  (let
    [engine
     (resolve-engine engine-id)

     id
     (new-job-id)

     t
     (now-ms)

     job
     {:id id :engine-id (:id engine) :phase :queued :progress 0 :created-at t :updated-at t}]

    (swap! jobs* (fn [jobs]
                   (assoc (sweep jobs) id job)))
    (future (run-job! id engine audio-path on-done))
    (public-job job)))

(defn submit-sync!
  "[[submit!]] without the thread — the same job lifecycle run inline. Tests and
   any caller that wants determinism use this."
  [{:keys [audio-path engine-id on-done]}]
  (let
    [engine
     (resolve-engine engine-id)

     id
     (new-job-id)

     t
     (now-ms)

     job
     {:id id :engine-id (:id engine) :phase :queued :progress 0 :created-at t :updated-at t}]

    (swap! jobs* (fn [jobs]
                   (assoc (sweep jobs) id job)))
    (run-job! id engine audio-path on-done)))

(defn reset-jobs! "Forget every job (tests)." [] (reset! jobs* {}) nil)
