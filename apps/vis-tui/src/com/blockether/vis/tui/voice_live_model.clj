(ns com.blockether.vis.tui.voice-live-model
  "Pure endpoint and playback accounting for experimental headset voice."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.document :as document]))

(def limits (get (document/schema-document "voice") "x-vis-voice-limits"))

(defn initial
  [generation epoch]
  {:generation generation
   :epoch epoch
   :next-id 1
   :preroll []
   :utterance nil
   :blocked-output? false
   :segments {}})

(defn speech?
  [^bytes pcm]
  (let [n (quot (alength pcm) 2)]
    (and (pos? n)
         (> (Math/sqrt (/ (reduce (fn [sum i]
                                    (let [v (short (bit-or (bit-and 255 (aget pcm (* 2 (long i))))
                                                           (bit-shift-left
                                                             (aget pcm (inc (* 2 (long i))))
                                                             8)))]
                                      (+ (double sum) (* (double v) v))))
                                  0.0
                                  (range n))
                          n))
            550.0))))

(defn- frame-effect
  [s u pcm]
  [:frame
   {:kind :input
    :generation (:generation s)
    :stream-id (:id u)
    :sequence (:sequence u)
    :sample-offset (:samples u)
    :sample-rate (get limits "sample_rate")
    :sample-count (quot (alength ^bytes pcm) 2)} pcm])

(defn capture
  "Consume one 20 ms frame. Effects preserve interrupt, start, audio and end order."
  [s pcm voiced?]
  (let [preroll
        (get limits "preroll_ms")

        frame-ms
        (get limits "frame_ms")]

    (if-let [u (:utterance s)]
      (let [samples (+ (long (:samples u)) (quot (alength ^bytes pcm) 2))
            silence (if voiced? 0 (+ (long (:silence u)) (long frame-ms)))
            end? (or (>= (long silence) (long (get limits "endpoint_silence_ms")))
                     (>= samples
                         (* (long (get limits "sample_rate"))
                            (quot (long (get limits "max_utterance_ms")) 1000))))
            next-u (assoc u
                     :samples samples
                     :sequence (inc (long (:sequence u)))
                     :silence silence)]

        [(assoc s
           :utterance (when-not end? next-u)
           :preroll [])
         (cond-> [(frame-effect s u pcm)]
           end?
           (conj
             [:control
              {:type "utterance.end" :capture_generation (:generation s) :utterance_id (:id u)}]))])
      (let [frames (conj (:preroll s) pcm)
            voiced-ms (if voiced? (+ (long (:voiced-ms s 0)) (long frame-ms)) 0)
            interrupt? (>= voiced-ms (long (get limits "min_speech_ms")))
            stop-effects
            (if interrupt?
              [[:stop-output]
               [:control {:type "output.interrupt" :output_epoch (:epoch s) :reason "barge_in"}]]
              [])
            s (cond-> (assoc s :voiced-ms voiced-ms)
                interrupt?
                (assoc :blocked-output?
                  true :segments
                  {}))]

        (if interrupt?
          (let [u {:id (:next-id s) :sequence 0 :samples 0 :silence 0}
                [end-u effects] (reduce (fn [[current effects] frame]
                                          [(-> current
                                               (update :sequence inc)
                                               (update :samples + (quot (alength ^bytes frame) 2)))
                                           (conj effects (frame-effect s current frame))])
                                        [u []]
                                        frames)]

            [(assoc s
               :next-id (inc (long (:next-id s)))
               :utterance end-u
               :preroll []
               :voiced-ms 0)
             (into (conj stop-effects
                         [:control
                          {:type "utterance.start"
                           :capture_generation (:generation s)
                           :utterance_id (:id u)}])
                   effects)])
          [(assoc s
             :preroll (vec (take-last (quot (+ (long preroll) (long voiced-ms)) frame-ms) frames)))
           stop-effects])))))

(defn accept-output
  "Reject stale, unknown, duplicate and discontinuous output before scheduling it."
  [s header]
  (let [{:keys [generation stream-id sequence sample-offset sample-count kind]}
        header

        segment
        (get-in s [:segments stream-id])]

    (cond (or (:blocked-output? s) (not= generation (:epoch s))) [s false]
          (or (not= kind :output)
              (nil? segment)
              (not= sequence (:sequence segment))
              (not= sample-offset (:received segment)))
          (throw (ex-info "Invalid live voice output sequence" {}))
          :else [(-> s
                     (update-in [:segments stream-id :sequence] inc)
                     (update-in [:segments stream-id :received] + sample-count)) true])))

(defn receipt
  [epoch segment-id segment position]
  (let [scheduled
        (:scheduled segment 0)

        played
        (min (long scheduled) (max 0 (- (long position) (long (:base segment position)))))]

    {:type "playback.receipt"
     :output_epoch epoch
     :segment_id segment-id
     :received (:received segment 0)
     :decoded (:received segment 0)
     :scheduled scheduled
     :played played}))

(defn status-text
  "Keep capture, output and task state independent in the voice banner."
  [state muted?]
  (let [label #(str/replace (or % "unknown") "_" " ")]
    (str "Hands-free · headset only · Mic: "
         (if muted? "muted" (label (get state "capture")))
         " · Speech: "
         (label (get state "output"))
         " · Task: "
         (label (get state "agent"))
         (when (= "waiting_for_input" (get state "agent")) " (use task controls)")
         " · /hands-free end")))

(defn cancellation-text
  "Describe the control result without asserting task completion."
  [event]
  (case (get event "status")
    "requested"
    "Task cancellation requested. Wait for task confirmation."

    "ambiguous"
    "No task cancelled. Choose a task in the normal task controls."

    "none"
    "No eligible task to cancel."

    "Task cancellation result unavailable."))

(defn ended
  [s id]
  (when (>= (count (:ended s)) (long (get limits "max_admissions")))
    (throw (ex-info "Voice admission tracking limit reached" {})))
  (update s :ended (fnil conj #{}) id))

(defn admitted
  [s voice-id event]
  (let [id
        (get event "utterance_id")

        turn
        (get event "turn_id")]

    (when-not (and (contains? (:ended s) id)
                   (= (str "voice/" voice-id "/" id) (get event "idempotency_key")))
      (throw (ex-info "Unowned voice admission" {})))
    (when-let [previous (get-in s [:admissions id])]
      (when-not (= previous turn) (throw (ex-info "Conflicting voice admission" {}))))
    (-> s
        (assoc-in [:admissions id] turn)
        (update :owned-turns (fnil conj #{}) turn))))

(defn rejected
  [s event]
  (let [id (get event "utterance_id")]
    (if (and (contains? (:ended s) id) (not (contains? (:admissions s) id)))
      (update s :rejected (fnil conj #{}) id)
      s)))

(defn final-transcript
  [s event]
  (let [id
        (get event "utterance_id")

        text
        (-> (get event "text")
            str/trim
            str/lower-case
            (str/replace #"[.!?]+$" ""))]

    (if (and (get event "final")
             (contains? (:ended s) id)
             (contains? #{"stop speaking" "end voice mode" "cancel that task"} text))
      (assoc-in s [:spoken-controls id] text)
      s)))

(defn control-outcome
  [s type]
  (let [ids (for [[id text] (:spoken-controls s)
                  :when (= type
                           ({"stop speaking" "output.interrupted"
                             "end voice mode" "session.closed"
                             "cancel that task" "task.cancellation"}
                            text))]

              id)]
    (if (seq ids) (update s :control-outcomes (fnil into #{}) ids) s)))

(defn recovery
  "Resolve explicit rejections and matching receipts. Never replay missing requests."
  [s voice-id snapshot]
  (let [receipts
        (when (= voice-id (get-in snapshot ["state" "voice_session_id"]))
          (take (long (get limits "max_admissions")) (get snapshot "receipts")))

        confirmed
        (into (set (keys (:admissions s)))
              (keep (fn [receipt]
                      (let [id (get receipt "utterance_id")]
                        (when (and (= voice-id (get receipt "voice_session_id"))
                                   (= (str "voice/" voice-id "/" id)
                                      (get receipt "idempotency_key"))
                                   (contains? (:ended s) id)
                                   (string? (get receipt "turn_id")))
                          id))))
              receipts)

        rejected
        (into (set (:rejected s))
              (keep (fn [receipt]
                      (let [id (get receipt "utterance_id")]
                        (when (and (= voice-id (get receipt "voice_session_id"))
                                   (= (str "voice/" voice-id "/" id)
                                      (get receipt "idempotency_key"))
                                   (contains? (:ended s) id)
                                   (= "input_overflow" (get receipt "error"))
                                   (nil? (get receipt "turn_id")))
                          id))))
              receipts)

        resolved
        (into (into confirmed (:control-outcomes s)) rejected)

        unknown
        (remove resolved (:ended s))]

    {:confirmed (count confirmed) :unknown (count unknown)}))

(defn recovery-text
  [{:keys [confirmed unknown]}]
  (str
    "Voice stopped. " confirmed
    " task admissions confirmed. "
    (if (pos? (long unknown))
      (str
        unknown
        " utterance outcomes are unknown. Check the session before sending again. Nothing was replayed.")
      "No requests were replayed. Tasks may still be running.")))
