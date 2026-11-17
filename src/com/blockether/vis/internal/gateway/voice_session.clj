(ns com.blockether.vis.internal.gateway.voice-session
  "Transport-neutral live voice state. Effects never run native inference on the caller."
  (:require [clojure.string :as str]
            [com.blockether.vis.contract.voice :as contract]))

(set! *warn-on-reflection* true)

(defn create
  "Create one exclusive owner. Effects must enqueue bounded work without blocking."
  [{:keys [voice-session-id client-id muted now emit persist admit cancel-task open-input feed-input
           finish-input abort-input speak abort-output close]
    :or {now #(quot (System/nanoTime) 1000000)
         emit (constantly nil)
         persist (constantly nil)
         close (constantly nil)}}]
  {:lock (Object.)
   :now now
   :effects {:emit emit
             :persist persist
             :admit admit
             :cancel-task cancel-task
             :open-input open-input
             :feed-input feed-input
             :finish-input finish-input
             :abort-input abort-input
             :speak speak
             :abort-output abort-output
             :close close}
   :state (atom
            {:voice-session-id voice-session-id
             :client-id client-id
             :capture-generation 0
             :output-epoch 0
             :capture (if muted "muted" "live")
             :utterance "idle"
             :agent "idle"
             :output "idle"
             :output-muted false
             :closed false
             :lease-until (+ (long (now)) (long (:lease-ms contract/limits)))
             :last-utterance -1
             :pending {}
             :receipts {}
             :owned-turns {}
             :input-views {}
             :next-segment 0
             :segments {}})})

(defn snapshot
  "Return independent public states, without audio or authentication material."
  [session]
  (select-keys @(:state session)
               [:voice-session-id :capture-generation :output-epoch :capture :utterance :agent
                :output]))

(defn receipts [session] (vec (vals (:receipts @(:state session)))))

(defn- emit! [session event] ((get-in session [:effects :emit]) event))

(defn- persist! [session type payload] ((get-in session [:effects :persist]) type payload))

(defn- effect!
  [session key & args]
  (if-let [f (get-in session [:effects key])]
    (apply f args)
    true))

(defn- state! [session] (emit! session {:type "session.state" :state (snapshot session)}))

(defn- touch!
  [session]
  (swap! (:state session) assoc
    :lease-until
    (+ (long ((:now session))) (long (:lease-ms contract/limits)))))

(defn- played-receipts!
  [session reason]
  (doseq [[id segment] (:segments @(:state session))]
    (persist! session
              "voice.playback"
              {:voice-session-id (:voice-session-id @(:state session))
               :segment-id id
               :turn-id (:turn-id segment)
               :output-epoch (:epoch segment)
               :generated (:generated segment)
               :received (:received segment)
               :decoded (:decoded segment)
               :scheduled (:scheduled segment)
               :played (:played segment)
               :reason reason})))

(defn close!
  "Invalidate media and release devices. Leave all agent tasks intact."
  [session reason]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when-not (:closed @(:state session))
      (played-receipts! session reason)
      (swap! (:state session) #(-> %
                                   (assoc :closed true
                                          :closed-at ((:now session))
                                          :close-reason reason
                                          :capture "off"
                                          :output "interrupted"
                                          :current nil
                                          :pending {}
                                          :segments {})
                                   (update :capture-generation inc)
                                   (update :output-epoch inc)))
      (effect! session :abort-input)
      (effect! session :abort-output)
      (.notifyAll ^Object (:lock session))
      (emit! session {:type "session.closed" :reason reason})
      (effect! session :close reason)))
  nil)

(defn fail!
  "Fail closed without cancelling agent work."
  [session code message]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when-not (:closed @(:state session))
      (emit! session {:type "error" :code code :message message :fatal true})
      (close! session code)))
  false)

(defn expired? [session] (>= (long ((:now session))) (long (:lease-until @(:state session)))))

(defn reap!
  "Close an expired lease even when no client traffic arrives."
  [session]
  (when (and (not (:closed @(:state session))) (expired? session))
    (close! session "lease_expired")))

(defn- live? [session] (reap! session) (not (:closed @(:state session))))

(defn abort-acknowledged!
  "Acknowledge only an output worker that has actually exited and released its state."
  [session epoch]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (and (not (:closed @(:state session))) (= epoch (:output-epoch @(:state session))))
      (emit! session {:type "output.interrupted" :output-epoch epoch :abort "acknowledged"}))))

(defn interrupt!
  "Invalidate output immediately. Do not change capture or task state."
  [session expected-epoch reason]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (and (live? session) (= expected-epoch (:output-epoch @(:state session))))
      (played-receipts! session reason)
      (swap! (:state session) #(-> %
                                   (update :output-epoch inc)
                                   (assoc :output "interrupted"
                                          :segments {})))
      (emit! session
             {:type "output.interrupted"
              :output-epoch (:output-epoch @(:state session))
              :abort "requested"})
      (effect! session :abort-output)
      (.notifyAll ^Object (:lock session))
      (state! session))))

(defn interrupt-turn!
  "Suppress speech from an owned current turn without cancelling its work."
  [session turn-id reason]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (let [state @(:state session)]
      (when (= (:output-epoch state) (get-in state [:owned-turns turn-id :epoch]))
        (interrupt! session (:output-epoch state) reason)))))

(defn- reject-input!
  "Reject unfinished input on overload. Keep admitted tasks and output unchanged."
  [session]
  (let [state
        @(:state session)

        voice-id
        (:voice-session-id state)

        pending
        (:pending state)]

    (swap! (:state session) #(-> %
                                 (update :capture-generation inc)
                                 (assoc :capture "muted"
                                        :utterance "rejected"
                                        :current nil
                                        :pending {})))
    (effect! session :abort-input)
    (doseq [[id input] (sort-by key pending)]
      (let [receipt {:voice-session-id voice-id
                     :utterance-id id
                     :idempotency-key (str "voice/" voice-id "/" id)
                     :text ""
                     :revision (:revision input)
                     :error "input_overflow"}]
        (when (< (count (:receipts @(:state session))) (long (:max-admissions contract/limits)))
          (swap! (:state session) assoc-in [:receipts id] receipt)
          (persist! session "voice.admission" receipt))
        (emit! session {:type "utterance.rejected" :utterance-id id :reason "input_overflow"})))
    (emit!
      session
      {:type "error"
       :code "input_overflow"
       :fatal false
       :message
       "Speech input fell behind. Unfinished requests were not admitted. Unmute to try again."})
    (state! session))
  false)

(defn mute!
  "Stop input on mute. Speech from before the mute stays valid, and the mute ends an
   open utterance. Unmute invalidates unfinished input. It comes only from an
   authenticated client control."
  [session muted]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (and (live? session) (not= muted (= "muted" (:capture @(:state session)))))
      (if muted
        ;; The capture generation stays, so that queued input of this generation finishes.
        (let [current (get-in @(:state session) [:current :id])]
          (swap! (:state session) assoc :capture "muted" :current nil)
          (if current
            (swap! (:state session) assoc-in [:pending current :phase] :finishing)
            (when (empty? (:pending @(:state session)))
              (swap! (:state session) assoc :utterance "idle")))
          (if (and current (false? (effect! session :finish-input current)))
            (reject-input! session)
            (state! session)))
        (do (swap! (:state session) #(-> %
                                         (update :capture-generation inc)
                                         (assoc :capture "live"
                                                :current nil
                                                :pending {}
                                                :utterance "idle")))
            (effect! session :abort-input)
            (state! session))))))

(defn start-utterance!
  [session generation utterance-id]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (live? session)
      (let [state @(:state session)]
        (cond (or (not= generation (:capture-generation state)) (not= "live" (:capture state)))
              false
              (<= (long utterance-id) (long (:last-utterance state))) false
              (or (:current state)
                  (>= (count (:pending state)) (long (:max-pending-utterances contract/limits))))
              (fail! session "input_overflow" "Too many unfinished utterances")
              :else (do (when (= (:input-epoch state) (:output-epoch state))
                          (interrupt! session (:output-epoch state) "barge_in"))
                        (swap! (:state session) assoc
                          :last-utterance utterance-id
                          :utterance "open"
                          :input-epoch (:output-epoch @(:state session))
                          :current {:id utterance-id :generation generation :sequence 0 :samples 0})
                        (swap! (:state session) assoc-in
                          [:pending utterance-id]
                          {:generation generation
                           :phase :open
                           :revision 0
                           :epoch (:output-epoch @(:state session))})
                        (touch! session)
                        (if (false? (effect! session :open-input utterance-id generation))
                          (reject-input! session)
                          (do (state! session) true))))))))

(defn input-frame!
  "Accept only contiguous current audio. Old or duplicate packets cannot reach an engine."
  [session frame]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (live? session)
      (let [{:keys [capture-generation current capture]}
            @(:state session)

            {:keys [kind generation stream-id sequence sample-offset sample-count]}
            frame]

        (cond (or (not= :input kind)
                  (not= generation capture-generation)
                  (not= "live" capture)
                  (not= stream-id (:id current)))
              false
              (< (long sequence) (long (:sequence current))) false
              (or (not= sequence (:sequence current)) (not= sample-offset (:samples current)))
              (fail! session "input_discontinuity" "Input audio has a gap")
              (> (+ (long sample-offset) (long sample-count))
                 (* (quot (long (:sample-rate contract/limits)) 1000)
                    (long (:max-utterance-ms contract/limits))))
              (fail! session "utterance_too_long" "The utterance exceeds the duration limit")
              :else (do (swap! (:state session) update
                          :current
                          #(-> %
                               (update :sequence inc)
                               (update :samples + sample-count)))
                        (touch! session)
                        (if (false? (effect! session :feed-input stream-id frame))
                          (reject-input! session)
                          true)))))))

(defn end-utterance!
  [session generation utterance-id]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (and (live? session)
               (= generation (:capture-generation @(:state session)))
               (= utterance-id (get-in @(:state session) [:current :id])))
      (swap! (:state session) #(-> %
                                   (assoc :current nil)
                                   (assoc-in [:pending utterance-id :phase] :finishing)))
      (touch! session)
      (when (false? (effect! session :finish-input utterance-id)) (reject-input! session)))))

(defn partial!
  "Publish replacement revisions. A provisional transcript never admits a task."
  [session utterance-id {:keys [text revision]}]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when-let [pending (get-in @(:state session) [:pending utterance-id])]
      (when (and (live? session)
                 (= (:generation pending) (:capture-generation @(:state session)))
                 (> (long revision) (long (:revision pending)))
                 (<= (count text) 4000))
        (swap! (:state session) #(-> %
                                     (assoc :utterance "provisional")
                                     (assoc-in [:pending utterance-id :revision] revision)))
        (emit! session
               {:type "transcript"
                :utterance-id utterance-id
                :revision revision
                :text text
                :final false})))))

(defn- refresh-agent!
  [session]
  (let [state
        @(:state session)

        waiting
        (set (vals (:input-views state)))

        statuses
        (set (map (fn [[id turn]]
                    (if (and (= "running" (:status turn)) (contains? waiting id))
                      "waiting_for_input"
                      (:status turn)))
                  (:owned-turns state)))

        status
        (or (some statuses ["waiting_for_input" "cancelling" "running" "queued"])
            (when (seq statuses) "terminal")
            "idle")]

    (swap! (:state session) assoc :agent status)))

(defn- cancel-task!
  [session turn-id]
  (when (get-in @(:state session) [:owned-turns turn-id])
    (let [result (effect! session :cancel-task turn-id)]
      (when-not (:error result)
        (swap! (:state session) assoc-in [:owned-turns turn-id :status] "cancelling")
        (refresh-agent! session)
        (emit!
          session
          {:type "task.cancellation"
           :status "requested"
           :turn-id turn-id
           :message
           "Cancellation was requested. Completed actions are not undone. Check the task history."})
        (state! session)))))

(defn- cancel-spoken-task!
  [session]
  (let [ids (mapv first
                  (filter (fn [[_ turn]]
                            (not= "terminal" (:status turn)))
                          (:owned-turns @(:state session))))]
    (case (count ids)
      1
      (cancel-task! session (first ids))

      0
      (emit!
        session
        {:type "task.cancellation"
         :status "none"
         :message
         "No active task belongs to this voice session. Check the task history for completed actions."})

      (emit!
        session
        {:type "task.cancellation"
         :status "ambiguous"
         :message
         "More than one voice task is active. Select a task in the conversation before you cancel it."}))))

(defn final!
  "Commit one immutable final through normal idempotent admission. Ignore late engine callbacks."
  [session utterance-id {:keys [text revision]}]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when-let [pending (get-in @(:state session) [:pending utterance-id])]
      (when (and (live? session)
                 (= :finishing (:phase pending))
                 (= (:generation pending) (:capture-generation @(:state session))))
        (swap! (:state session) update :pending dissoc utterance-id)
        (let [text (str/trim (str text))
              revision (max (inc (long (:revision pending))) (long (or revision 0)))
              voice-id (:voice-session-id @(:state session))
              key (str "voice/" voice-id "/" utterance-id)
              base {:voice-session-id voice-id
                    :utterance-id utterance-id
                    :idempotency-key key
                    :text text
                    :revision revision}]

          (cond (str/blank? text) (swap! (:state session) assoc :utterance "rejected")
                (> (count text) 4000)
                (fail! session "transcript_too_long" "The transcript exceeds the text limit")
                (>= (count (:receipts @(:state session))) (long (:max-admissions contract/limits)))
                (fail! session "session_limit" "Start a new voice session to continue")
                :else
                (do (emit! session
                           {:type "transcript"
                            :utterance-id utterance-id
                            :revision revision
                            :text text
                            :final true})
                    (persist! session "voice.utterance" base)
                    (case (str/lower-case (str/replace text #"[.!?]+$" ""))
                      "stop speaking"
                      (interrupt! session (:output-epoch @(:state session)) "stop_speech")

                      "end voice mode"
                      (close! session "user_ended")

                      "cancel that task"
                      (cancel-spoken-task! session)

                      (let [result (effect! session :admit {:request text :idempotency-key key})
                            receipt (merge base (select-keys result [:turn-id :status :error]))]

                        (swap! (:state session) assoc-in [:receipts utterance-id] receipt)
                        (persist! session "voice.admission" receipt)
                        (if-let [turn-id (:turn-id result)]
                          (do (swap! (:state session) #(-> %
                                                           (assoc :utterance "admitted")
                                                           (assoc-in [:owned-turns turn-id]
                                                                     {:epoch (:epoch pending)
                                                                      :status (:status result)})))
                              (refresh-agent! session)
                              (emit! session
                                     {:type "utterance.admitted"
                                      :utterance-id utterance-id
                                      :turn-id turn-id
                                      :idempotency-key key
                                      :status (:status result)}))
                          (do (swap! (:state session) assoc :utterance "rejected")
                              (emit! session
                                     {:type "error"
                                      :code "admission_rejected"
                                      :message
                                      "The task was not admitted. Use the text conversation."
                                      :fatal false})))))
                    (state! session))))))))

(defn owns-running-task?
  "Report whether this voice session owns the running task. Ignore playback suppression."
  [session turn-id]
  (let [state @(:state session)]
    (and (not (:closed state)) (= "running" (get-in state [:owned-turns turn-id :status])))))

(defn task-steered!
  "Transfer consumed voice requests to their running task. Never revive an older output epoch."
  [session turn-id queued-turn-ids iteration]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (let [state
          @(:state session)

          consumed
          (filterv #(= "queued" (get-in state [:owned-turns % :status]))
            (distinct queued-turn-ids))]

      (when (and (live? session)
                 (seq consumed)
                 (string? turn-id)
                 (integer? iteration)
                 (not (neg? iteration))
                 (not= "terminal" (get-in state [:owned-turns turn-id :status])))
        (let [epoch (apply max (map #(get-in state [:owned-turns % :epoch]) consumed))]
          (swap! (:state session)
            (fn [s]
              (let [s (reduce #(assoc-in %1 [:owned-turns %2 :status] "terminal") s consumed)]
                (update-in s
                           [:owned-turns turn-id]
                           #(assoc %
                              :status "running"
                              :epoch (max epoch (:epoch % -1))
                              :min-iteration (max iteration (:min-iteration % 0)))))))
          (refresh-agent! session)
          (state! session))))))

(defn commit-speech!
  "Queue only an immutable projection from an owned, unsuppressed turn."
  [session {:keys [turn-id text source iteration]}]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (let [state
          @(:state session)

          epoch
          (:output-epoch state)

          turn
          (get-in state [:owned-turns turn-id])]

      (when (and (live? session)
                 (not (:output-muted state))
                 (not= "terminal" (:status turn))
                 (= epoch (:epoch turn))
                 (or (nil? iteration) (>= (long iteration) (long (:min-iteration turn 0))))
                 (contract/valid? "speech_text" text)
                 (string? source)
                 (not (str/blank? source))
                 (<= (count source) 128)
                 (not (contains? (:sources turn) source)))
        (if (or (>= (count (:segments state)) (long (:max-pending-segments contract/limits)))
                (>= (count (:sources turn)) (long (:max-speech-segments contract/limits))))
          (do (emit! session
                     {:type "error"
                      :code "output_overflow"
                      :message "Speech is behind. Read the remaining answer as text."
                      :fatal false})
              (interrupt! session epoch "output_overflow")
              nil)
          (let [id
                (:next-segment state)

                segment
                {:id id
                 :epoch epoch
                 :turn-id turn-id
                 :text text
                 :source source
                 :sequence 0
                 :generated 0
                 :sent 0
                 :received 0
                 :decoded 0
                 :scheduled 0
                 :played 0
                 :finished false}]

            (swap! (:state session) #(-> %
                                         (update :next-segment inc)
                                         (assoc :output "committed")
                                         (assoc-in [:segments id] segment)
                                         (update-in [:owned-turns turn-id :sources]
                                                    (fnil conj #{})
                                                    source)))
            (persist! session "voice.speech" (dissoc segment :text))
            (emit! session
                   {:type "speech.committed"
                    :output-epoch epoch
                    :segment-id id
                    :turn-id turn-id
                    :text text
                    :source source})
            (if (false? (effect! session :speak segment))
              (do (interrupt! session epoch "output_overflow") nil)
              segment)))))))

(defn output-current?
  [session epoch]
  (let [state @(:state session)]
    (and (not (:closed state)) (not (:output-muted state)) (= epoch (:output-epoch state)))))

(defn generated!
  "Count native PCM before playback credit can block its delivery."
  [session segment-id epoch sample-count]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (and (output-current? session epoch) (get-in @(:state session) [:segments segment-id]))
      (swap! (:state session) update-in [:segments segment-id :generated] + sample-count)
      true)))

(defn reserve-output!
  "Wait for played-sample credit. Return one frame header, or nil after interruption."
  [session segment-id epoch sample-count]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (loop []

      (when (and (live? session) (output-current? session epoch))
        (when-let [segment (get-in @(:state session) [:segments segment-id])]
          (let [outstanding (reduce +
                                    0
                                    (map #(- (long (:sent %)) (long (:played %)))
                                         (vals (:segments @(:state session)))))
                credit (* (quot (long (:sample-rate contract/limits)) 1000)
                          (long (:output-credit-ms contract/limits)))
                earlier? (some (fn [[id s]]
                                 (and (< id segment-id)
                                      (or (not (:finished s)) (< (:played s) (:sent s)))))
                               (:segments @(:state session)))]

            (cond (not (<= 1 sample-count (long (:frame-samples contract/limits))))
                  (throw (ex-info "Invalid output frame size" {:type :voice/invalid-output}))
                  (and (not earlier?) (<= (+ outstanding sample-count) credit))
                  (let [header {:kind :output
                                :generation epoch
                                :stream-id segment-id
                                :sequence (:sequence segment)
                                :sample-offset (:sent segment)
                                :sample-rate (:sample-rate contract/limits)
                                :sample-count sample-count}]
                    (swap! (:state session) #(-> %
                                                 (assoc :output "synthesizing")
                                                 (update-in [:segments segment-id :sequence] inc)
                                                 (update-in [:segments segment-id :sent]
                                                            +
                                                            sample-count)))
                    header)
                  :else (do (.wait ^Object (:lock session) 100) (recur)))))))))

(defn- drain-finished!
  [session segment-id]
  (when-let [segment (get-in @(:state session) [:segments segment-id])]
    (when (and (:finished segment) (= (:played segment) (:sent segment)))
      (persist! session
                "voice.playback"
                {:voice-session-id (:voice-session-id @(:state session))
                 :turn-id (:turn-id segment)
                 :output-epoch (:epoch segment)
                 :segment-id segment-id
                 :generated (:generated segment)
                 :received (:received segment)
                 :decoded (:decoded segment)
                 :scheduled (:scheduled segment)
                 :played (:played segment)
                 :reason "drained"})
      (swap! (:state session) (fn [state]
                                (let [segments (dissoc (:segments state) segment-id)]
                                  (assoc state
                                    :segments segments
                                    :output (if (seq segments) "committed" "drained")))))
      (state! session))))

(defn output-finished!
  [session segment-id epoch]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (and (output-current? session epoch) (get-in @(:state session) [:segments segment-id]))
      (swap! (:state session) assoc-in [:segments segment-id :finished] true)
      (emit! session
             {:type "output.finished"
              :output-epoch epoch
              :segment-id segment-id
              :sample-count (get-in @(:state session) [:segments segment-id :sent])})
      (drain-finished! session segment-id)
      (.notifyAll ^Object (:lock session)))))

(defn playback!
  "Accept monotonic sample receipts. Receipt of bytes is not proof of playback."
  [session {:keys [output-epoch segment-id received decoded scheduled played]}]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (and (live? session) (= output-epoch (:output-epoch @(:state session))))
      (if-let [segment (get-in @(:state session) [:segments segment-id])]
        (if (and (<= 0 played scheduled decoded received (:sent segment))
                 (every? (fn [[key value]]
                           (>= (long value) (long (get segment key))))
                         [[:received received] [:decoded decoded] [:scheduled scheduled]
                          [:played played]]))
          (do (swap! (:state session) update-in
                [:segments segment-id]
                merge
                {:received received :decoded decoded :scheduled scheduled :played played})
              (touch! session)
              (drain-finished! session segment-id)
              (.notifyAll ^Object (:lock session)))
          (fail! session "invalid_receipt" "Playback positions are inconsistent"))
        false))))

(defn task-state!
  [session turn-id status]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (and (not (:closed @(:state session))) (get-in @(:state session) [:owned-turns turn-id]))
      (swap! (:state session) assoc-in [:owned-turns turn-id :status] status)
      (when (= "terminal" status)
        (swap! (:state session) update-in [:owned-turns turn-id] dissoc :sources)
        (swap! (:state session) update
          :input-views
          #(into {}
                 (remove (fn [[_ id]]
                           (= id turn-id)))
                 %)))
      (refresh-agent! session)
      (state! session))))

(defn task-input!
  "Observe authenticated input views. Never answer a form or change task authority."
  [session turn-id view-id opened?]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (and (not (:closed @(:state session))) (string? view-id))
      (if opened?
        (when (= "running" (get-in @(:state session) [:owned-turns turn-id :status]))
          (if (and (not (contains? (:input-views @(:state session)) view-id))
                   (>= (count (:input-views @(:state session)))
                       (long (:max-pending-inputs contract/limits))))
            (fail! session "input_view_overflow" "Use the conversation to answer pending requests.")
            (swap! (:state session) assoc-in [:input-views view-id] turn-id)))
        (swap! (:state session) update :input-views dissoc view-id))
      (refresh-agent! session)
      (state! session))))

(defn control!
  "Apply validated controls from the socket owner. Raw transcripts are not a client command."
  [session control]
  #_{:clj-kondo/ignore [:locking-suspicious-lock]}
  (locking (:lock session)
    (when (live? session)
      (if-not (contract/valid? "client_control" control)
        (fail! session "invalid_control" "The voice control is invalid")
        (do (touch! session)
            (case (:type control)
              "heartbeat"
              (state! session)

              "utterance.start"
              (start-utterance! session (:capture-generation control) (:utterance-id control))

              "utterance.end"
              (end-utterance! session (:capture-generation control) (:utterance-id control))

              "output.interrupt"
              (interrupt! session (:output-epoch control) (:reason control))

              "playback.receipt"
              (playback! session control)

              "capture.mute"
              (mute! session (:muted control))

              "output.mute"
              (do (when (:muted control)
                    (interrupt! session (:output-epoch @(:state session)) "output_muted"))
                  (swap! (:state session) assoc
                    :output-muted (:muted control)
                    :output (if (:muted control) "muted" "idle"))
                  (state! session))

              "task.cancel"
              (if (get-in @(:state session) [:owned-turns (:turn-id control)])
                (cancel-task! session (:turn-id control))
                (emit! session
                       {:type "error"
                        :code "task_not_owned"
                        :message "This voice session does not own that task."
                        :fatal false}))

              "session.end"
              (close! session "user_ended")))))))
