(ns com.blockether.vis.internal.speech.live
  "Bounded local speech streams. Call inference from a worker, never a socket thread."
  (:require [com.blockether.vis.internal.speech.assets :as assets]
            [com.blockether.vis.internal.speech.engine :as engine]
            [com.blockether.vis.internal.speech.tts :as tts]
            [com.blockether.vis.internal.speech.vibevoice :as vibevoice])
  (:import [java.util.concurrent Semaphore]
           [java.util.concurrent.locks ReentrantLock]))

(set! *warn-on-reflection* true)

(def max-samples "Thirty seconds of mono audio at 24 kHz." 720000)

(defonce ^{:private true :tag Semaphore} asr-permit (Semaphore. 1))

(def ^:dynamic *open-decoder*
  "Decoder factory. Tests replace native inference, not the stream lifecycle."
  (fn []
    (vibevoice/open-live-asr! (assets/install-dir (vibevoice/asr-asset)))))

(defn capabilities
  "Report local asset readiness without downloads or native model loading."
  []
  (let [asr?
        (assets/installed? (vibevoice/asr-asset))

        tts?
        (tts/live-ready?)]

    (cond-> {:available (and asr? tts?)
             :asr {:available asr?
                   :engine :vibevoice-asr-local
                   :sample-rate 24000
                   :max-samples max-samples
                   :concurrency 1
                   :languages ["en"]
                   :abort-bound-ms nil}
             :tts {:available tts?
                   :engine :pocket-tts-local
                   :sample-rate 24000
                   :max-characters tts/live-max-characters
                   :concurrency 1
                   :abort-bound-ms nil}}
      (not (and asr? tts?))
      (assoc :reason "Local ASR model or Pocket voice assets are missing."))))

(defn- close-stream!
  [{:keys [state decoder release! cancelled?]} terminal]
  (when-not (:closed? @state)
    (swap! state assoc :state :closing :pending nil)
    (try ((:close! decoder))
         (finally (release!)
                  (swap! state assoc
                    :closed? true
                    :state (if (and (= :finished terminal) @cancelled?) :aborted terminal))))))

(defn- check-open!
  [{:keys [state cancelled?]}]
  (when (or @cancelled? (not= :open (:state @state)))
    (throw (ex-info "Speech stream is not open" {:type :speech-live/closed}))))

(defn- snapshot
  [{:keys [state]}]
  (let [{:keys [chunks revision]} @state]
    {:text (engine/words->text (engine/clean-words (vibevoice/chunk-words chunks)))
     :revision revision}))

(defn open-asr!
  "Open one serialized ASR stream. Feed offsets count samples, not bytes."
  [{:keys [on-partial max-samples] :or {max-samples 720000}}]
  (when-not (and (integer? max-samples) (<= 1 max-samples 720000))
    (throw (ex-info "Invalid speech sample limit" {:type :speech-live/sample-limit})))
  (when-not (.tryAcquire ^Semaphore asr-permit)
    (throw (ex-info "Local ASR stream is busy" {:type :speech-live/busy})))
  (try (let [decoder
             (*open-decoder*)

             {:keys [window chunk sample-rate]}
             decoder]

         (if-not
           (and (= 24000 sample-rate) (integer? chunk) (integer? window) (<= 1 chunk window 240000))
           (do ((:close! decoder))
               (throw (ex-info "Unsupported ASR window" {:type :speech-live/model-contract})))
           {:decoder decoder
            :lock (ReentrantLock.)
            :cancelled? (atom false)
            :release! #(.release ^Semaphore asr-permit)
            :on-partial on-partial
            :limit max-samples
            :state (atom {:state :open
                          :closed? false
                          :pending (float-array window)
                          :pending-count 0
                          :received 0
                          :processed 0
                          :chunks []
                          :revision 0})}))
       (catch Throwable e (.release ^Semaphore asr-permit) (throw e))))

(defn- step!
  [{:keys [state decoder] :as handle} real-count]
  (check-open! handle)
  (let [{:keys [pending processed]}
        @state

        {:keys [window chunk sample-rate]}
        decoder

        text
        ((:step! decoder) pending)

        advance
        (min (long chunk) (long real-count))

        remain
        (- (long real-count) advance)]

    (check-open! handle)
    (swap! state (fn [s]
                   (-> s
                       (update :revision inc)
                       (update :processed + advance)
                       (update :chunks
                               conj
                               {:start (/ (long processed) (double sample-rate))
                                :end (/ (+ (long processed) advance) (double sample-rate))
                                :text text})
                       (assoc :pending-count remain))))
    (System/arraycopy pending advance pending 0 remain)
    (java.util.Arrays/fill ^floats pending (int remain) (int window) (float 0))))

(defn- run-locked!
  [{:keys [^ReentrantLock lock state cancelled?] :as handle} f]
  (.lock lock)
  (try (f)
       (catch Throwable e
         (try (close-stream! handle (if @cancelled? :aborted :failed))
              (catch Throwable close-error (.addSuppressed e close-error)))
         (throw e))
       (finally (try (when (and @cancelled? (not (:closed? @state)))
                       (close-stream! handle :aborted))
                     (finally (.unlock lock))))))

(defn feed-asr!
  "Feed consecutive PCM. Return a replacement partial only when a model window completes."
  [handle ^floats samples sample-offset]
  (run-locked!
    handle
    (fn []
      (check-open! handle)
      (let [{:keys [state decoder limit on-partial]}
            handle

            {:keys [received revision]}
            @state

            n
            (alength samples)

            window
            (long (:window decoder))]

        (when-not (and (= received sample-offset) (<= (+ (long received) n) (long limit)))
          (throw (ex-info "Invalid speech offset or sample limit"
                          {:type :speech-live/input-limit})))
        (dotimes [i n]
          (when-not (Float/isFinite (aget samples i))
            (throw (ex-info "Non-finite speech sample" {:type :speech-live/invalid-sample}))))
        (loop [offset 0]
          (when (< offset n)
            (let [{:keys [pending pending-count]} @state
                  take-n (min (- window (long pending-count)) (- n offset))]

              (System/arraycopy samples offset pending pending-count take-n)
              (swap! state #(-> %
                                (update :pending-count + take-n)
                                (update :received + take-n)))
              (when (= window (:pending-count @state)) (step! handle window))
              (recur (long (+ offset take-n))))))
        (when (< revision (:revision @state))
          (let [partial (snapshot handle)]
            (when on-partial (on-partial partial))
            partial))))))

(defn finish-asr!
  "Pad the last windows once. Return the same final on repeated calls."
  [{:keys [state] :as handle}]
  (run-locked! handle
               (fn []
                 (if (= :finished (:state @state))
                   (:final @state)
                   (do (check-open! handle)
                       (loop []

                         (when (pos? (long (:pending-count @state)))
                           (step! handle (:pending-count @state))
                           (recur)))
                       (let [final (snapshot handle)]
                         (swap! state assoc :final final)
                         (close-stream! handle :finished)
                         (when @(:cancelled? handle)
                           (throw (ex-info "Speech stream was aborted"
                                           {:type :speech-live/closed})))
                         final))))))

(defn asr-status
  "Acknowledgement means native calls have exited and per-stream resources are closed."
  [{:keys [state cancelled?]}]
  (let [s @state]
    {:state (:state s) :abort-state (when @cancelled? (if (:closed? s) :acknowledged :requested))}))

(defn abort-asr!
  "Request native termination. Do not wait on an inference worker."
  [{:keys [^ReentrantLock lock state decoder cancelled?] :as handle}]
  (reset! cancelled? true)
  ;; Serialize RunOptions close against termination, without taking the inference lock.
  ((:abort! decoder))
  (when (.tryLock lock)
    (try (when-not (:closed? @state) (close-stream! handle :aborted)) (finally (.unlock lock))))
  {:state (if (:closed? @state) :acknowledged :requested)})

(defn prepare!
  "Load installed models on a worker. Check cancellation between native constructors."
  ([] (prepare! {}))
  ([{:keys [cancelled?] :or {cancelled? (constantly false)}}]
   (let [ready (capabilities)]
     (when-not (:available ready)
       (throw (ex-info "Local speech assets are missing" {:type :speech-live/unavailable})))
     (if (cancelled?)
       {:ready false :state :aborted}
       (do (let [handle (open-asr! {})]
             (abort-asr! handle))
           (when-not (cancelled?) (tts/prepare-live!))
           (if (cancelled?) {:ready false :state :aborted} (assoc ready :ready true)))))))

(defn synthesize-stream!
  "Generate committed speech through a bounded, blocking PCM consumer. No fallback or download occurs."
  [request]
  (tts/synthesize-live! request))
