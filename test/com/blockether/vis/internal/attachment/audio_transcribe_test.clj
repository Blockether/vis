(ns com.blockether.vis.internal.attachment.audio-transcribe-test
  "A recording is text the moment it arrives, or it SAYS why it is not."
  (:require [com.blockether.vis.internal.attachment.audio-transcribe :as at]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.speech.core :as speech]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]])
  (:import [java.util Base64]))

(def ^:private engine-id :audio-transcribe-test-engine)

(def ^:private calls
  "Every audio path the fake engine was handed, in order — what proves the work is
   done once."
  (atom []))

(def ^:private fake-engine* (atom nil))

(defn- register-fake!
  "Set the fake transcription answer used by this test."
  ([answer] (register-fake! answer nil))
  ([answer hold]
   (reset! fake-engine* {:id engine-id
                         :label "test"
                         :transcribe (fn [{:keys [audio-path]}]
                                       (swap! calls conj (str audio-path))
                                       (when hold (deref hold 10000 nil))
                                       (if (instance? Throwable answer) (throw answer) answer))})))

(set-ns-context! [(around-each [f]
                               (reset! calls [])
                               (reset! fake-engine* nil)
                               (at/clear-cache!)
                               (with-redefs-fn {#'speech/engines
                                                (fn [direction]
                                                  (if (and (= direction :transcribe) @fake-engine*)
                                                    [@fake-engine*]
                                                    []))
                                                #'speech/env-engine-id (constantly nil)}
                                 (fn []
                                   (try (f) (finally (at/clear-cache!))))))])

(defn- b64 [^String s] (.encodeToString (Base64/getEncoder) (.getBytes s "UTF-8")))

(defn- memo
  ([] (memo "one"))
  ([payload]
   {:path "/tmp/does-not-exist-memo.m4a"
    :filename "memo.m4a"
    :media-type "audio/mp4"
    :source "user"
    :base64 (b64 payload)
    :size 3}))

(defn- settled
  "This recording's outcome once the worker has finished with it — the composer's own
   re-read, bounded so a worker that never answers FAILS the test instead of hanging
   it."
  [attachment]
  (loop [tries 0]
    (let [answer (at/outcome attachment)]
      (if (or (and answer (not= at/PENDING (:status answer))) (>= tries 400))
        answer
        (do (Thread/sleep 25) (recur (inc tries)))))))

(defn- user-content
  "The user message `attachments` produce on a NON-vision wire — the manifest the
   model actually reads."
  [attachments]
  (:content (last (prompt/assemble-initial-messages {:stable-prompt-messages []
                                                     :initial-user-content "listen to this"
                                                     :vision? false
                                                     :user-images attachments}))))

(defdescribe
  transcribe-attachments-test
  (it "gives a recording its own words and leaves everything else alone"
      (register-fake! "  buy milk and call back  ")
      (let [rows (at/transcribe-attachments
                   [{:filename "shot.png" :media-type "image/png" :base64 (b64 "p")} (memo)])]
        (expect (nil? (:transcription (first rows))))
        (expect (nil? (:transcription-status (first rows))))
        ;; Trimmed, because the manifest QUOTES it.
        (expect (= "buy milk and call back" (:transcription (second rows))))
        (expect (= 1 (count @calls)))))
  (it "transcribes one recording ONCE however often the session replays it"
      ;; Attachments ride EVERY later request of a session, so an engine call per
      ;; render would re-run local speech on every turn of a long conversation.
      (register-fake! "the same words")
      (dotimes [_ 3]
        (at/transcribe-attachments [(memo)]))
      (expect (= 1 (count @calls))))
  (it "keys the work on the BYTES, not the file name"
      (register-fake! "words")
      (at/transcribe-attachments [(memo "first recording")])
      (at/transcribe-attachments [(memo "second recording")])
      (expect (= 2 (count @calls))))
  (it "keeps a transcript somebody already computed"
      (register-fake! "fresh words")
      (let [rows (at/transcribe-attachments [(assoc (memo) :transcription "stored words")])]
        (expect (= "stored words" (:transcription (first rows))))
        (expect (empty? @calls)))))

;; Regression, issue: a recording the machine could not transcribe came back as a bare
;; attachment — no words, no status, no log — which read exactly like a memo with no
;; speech in it, so neither the human nor the model could tell the two apart.
(defdescribe
  never-silent-test
  (it "says the recording is UNAVAILABLE when the engine throws"
      (register-fake! (ex-info "model exploded" {}))
      (let [row (first (at/transcribe-attachments [(memo)]))]
        ;; Still an attachment: a failed transcription is a missing convenience,
        ;; never a lost recording.
        (expect (nil? (:transcription row)))
        (expect (= at/UNAVAILABLE (:transcription-status row)))))
  (it "says SILENT when the engine read the whole recording and found no words"
      (register-fake! "   ")
      (expect (= at/SILENT (:transcription-status (first (at/transcribe-attachments [(memo)]))))))
  (it "says UNAVAILABLE when this build carries no speech engine"
      ;; No extension loaded, or its engine failed to load: `resolve-engine` THROWS,
      ;; and that throw must never reach the turn.
      (with-redefs [speech/resolve-engine (fn [& _]
                                            (throw (ex-info "none registered" {})))]
        (expect (false? (at/available?)))
        (expect (= at/UNAVAILABLE
                   (:transcription-status (first (at/transcribe-attachments [(memo)])))))))
  (it "refuses while the toggle is off, and does not REMEMBER the refusal"
      ;; A toggle, a model still downloading and a spent budget are facts about the
      ;; moment. Remembering one as "this recording has no words" would outlive it.
      (register-fake! "words at last")
      (with-redefs [toggles/enabled? (fn [id]
                                       (not= at/TOGGLE_ID id))]
        (expect (= at/UNAVAILABLE
                   (:transcription-status (first (at/transcribe-attachments [(memo)])))))
        (expect (empty? @calls)))
      (expect (= "words at last" (:transcription (first (at/transcribe-attachments [(memo)])))))))

(defdescribe upload-door-test
             (it "answers PENDING the moment a recording is staged, without waiting for a word"
                 (let [hold (promise)]
                   (register-fake! "staged words" hold)
                   (let [row (first (at/request-attachments! [(memo)]))]
                     (expect (= at/PENDING (:transcription-status row)))
                     (expect (nil? (:transcription row))))
                   (deliver hold true)
                   (expect (= "staged words" (:transcription (settled (memo)))))))
             (it "hands the turn the words the staging already made, without asking twice"
                 (register-fake! "already said")
                 (at/request-attachments! [(memo)])
                 (settled (memo))
                 (expect (= "already said"
                            (:transcription (first (at/transcribe-attachments [(memo)])))))
                 (expect (= 1 (count @calls)))))

;; Regression, issue: the temp copy was named after the FILENAME, so a phone memo that
;; is AAC in an MP4 box called ".mp3" was handed to the decoder as an MP3.
(defdescribe container-test
             (it "names the temp copy after the bytes, not after the name the phone sent"
                 (let [mp4 (byte-array (map unchecked-byte
                                            (concat [0 0 0 32] (map int "ftypM4A ") (repeat 8 0))))]
                   (expect (= ".m4a" (at/container-extension mp4 "audio/mp4" "shared.mp3")))))
             (it "falls back to the declared type, and only then to the name"
                 (let [unknown (byte-array (map unchecked-byte (repeat 16 0)))]
                   (expect (= ".ogg" (at/container-extension unknown "audio/ogg" "clip.bin")))
                   (expect (= ".amr" (at/container-extension unknown nil "clip.amr")))
                   (expect (= ".audio" (at/container-extension unknown nil nil))))))

(defdescribe
  manifest-test
  (it "quotes the transcript to the model instead of telling it to open the file"
      ;; The whole point: no provider wire carries audio, so the words ARE the
      ;; attachment as far as the model is concerned.
      (register-fake! "remember to water the plants")
      (let [rows
            (at/transcribe-attachments [(memo)])

            content
            (user-content rows)]

        (expect (= "remember to water the plants" (:transcription (first rows))))
        (expect (re-find #"transcript of the recording: \"remember to water the plants\""
                         (str content)))
        ;; ... and the reason stops sending the model after a file it has already read.
        (expect (re-find #"transcript is quoted below" (str content)))))
  (it "quotes the head of an hour of speech and SAYS how much it is not showing"
      ;; The stored transcript is whole; only what rides every later request of
      ;; the session is bounded.
      (register-fake! (apply str (repeat 9000 "a")))
      (let [content (str (user-content (at/transcribe-attachments [(memo)])))]
        (expect (re-find #"the first 8000 of 9000 characters" content))
        (expect (re-find #"the whole transcript is stored with the file" content))))
  (it "tells the model the words are still being made, instead of saying nothing"
      (let [content (str (user-content [(assoc (memo) :transcription-status at/PENDING)]))]
        (expect (re-find #"still being made" content))
        (expect (re-find #"do not answer as if you had heard it" content))))
  (it "tells the model when the machine could not transcribe the recording"
      (let [content (str (user-content [(assoc (memo) :transcription-status at/UNAVAILABLE)]))]
        (expect (re-find #"could NOT transcribe the recording" content)))))

;; Regression: session 388aa964-07cf-498a-a6d7-71c49c86b491 lost a long recording
;; at the two-minute join deadline, even though the worker later finished it.
(defdescribe completed-recordings-test
             (it "waits for the worker instead of giving the model a timed-out placeholder"
                 (register-fake! "unused")
                 (let [answer
                       {:transcription "the complete recording"}

                       result
                       (reify
                         clojure.lang.IDeref
                           (deref [_] answer)
                         clojure.lang.IBlockingDeref
                           (deref [_ _ timeout-value] timeout-value))]

                   ;; Model a job that outlives any bounded join without a two-minute test sleep.
                   (with-redefs-fn {#'at/start! (fn [& _]
                                                  {:result result})}
                     #(expect (= answer (at/transcribe-attachment (memo)))))))
             (it "does not omit the fifth recording from the model's request"
                 (register-fake! "spoken words")
                 (let [rows (at/transcribe-attachments (mapv (comp memo str) (range 5)))]
                   (expect (= 5 (count @calls)))
                   (expect (every? #(= "spoken words" (:transcription %)) rows))))
             (it "clears pending status when the words arrive"
                 (register-fake! "ready words")
                 (let [row (first (at/transcribe-attachments
                                    [(assoc (memo) :transcription-status at/PENDING)]))]
                   (expect (= "ready words" (:transcription row)))
                   (expect (not (contains? row :transcription-status)))
                   (expect (not (re-find #"still being made" (str (user-content [row])))))))
             (it "shares work when intake corrects the declared MIME type"
                 (register-fake! "same bytes")
                 (at/transcribe-attachments [(memo)])
                 (at/transcribe-attachments [(assoc (memo) :media-type "audio/mpeg")])
                 (expect (= 1 (count @calls))))
             (it "cancels a waiting turn without losing the worker's result"
                 (let [hold
                       (promise)

                       cancelled?
                       (atom false)]

                   (register-fake! "saved after cancellation" hold)
                   (let [staged
                         (at/request-attachments! [(memo)])

                         waiting
                         (future (try (at/transcribe-attachments staged
                                                                 {:cancelled? #(deref cancelled?)})
                                      (catch InterruptedException _ ::cancelled)))]

                     (try (reset! cancelled? true)
                          (expect (= ::cancelled (deref waiting 2000 ::timeout)))
                          (deliver hold true)
                          (expect (= "saved after cancellation" (:transcription (settled (memo)))))
                          (expect (= 1 (count @calls)))
                          (finally (deliver hold true) (future-cancel waiting)))))))

(defdescribe
  bounded-recording-wait-test
  (it "shares one deadline across every recording in a bounded join"
      (let [deadlines (atom [])]
        (with-redefs [at/transcribe-attachment (fn [_ opts]
                                                 (swap! deadlines conj (:deadline-ns opts))
                                                 {:transcription "ready"})]
          (at/transcribe-attachments [(memo "first") (memo "second")] {:timeout-ms 300000}))
        (expect (= 2 (count @deadlines)))
        (expect (every? number? @deadlines))
        (expect (apply = @deadlines))))
  (it "returns pending at the deadline without losing the worker's eventual words"
      (let [hold (promise)]
        (register-fake! "saved after the wait limit" hold)
        (let [staged (at/request-attachments! [(memo)])
              waiting (future (at/transcribe-attachments staged {:timeout-ms 25}))]

          (try (let [rows (deref waiting 1000 ::still-waiting)]
                 (expect (not= ::still-waiting rows))
                 (expect (= at/PENDING (get-in rows [0 :transcription-status]))))
               (deliver hold true)
               (expect (= "saved after the wait limit" (:transcription (settled (memo)))))
               (expect (= 1 (count @calls)))
               (finally (deliver hold true) (deref waiting 1000 nil) (future-cancel waiting)))))))

;; Regression: a 46-minute recording held the one worker while every later turn of
;; the session asked again and was handed `pending`. The words never arrived, the
;; player carried TRANSCRIBING… forever, and nothing ever said why.
(defdescribe recording-length-cap-test
             (it "refuses a recording past the cap instead of queueing it forever"
                 (register-fake! "words nobody would have waited for")
                 (with-redefs [at/MAX_PAYLOAD_BYTES 4]
                   (expect (= at/UNAVAILABLE
                              (:transcription-status (first (at/transcribe-attachments
                                                              [(memo "far past the cap")])))))
                   (expect (empty? @calls)))
                 ;; A cap is a fact about the FILE, so it is remembered — and anything under it
                 ;; is transcribed exactly as before.
                 (expect (= "words nobody would have waited for"
                            (:transcription (first (at/transcribe-attachments
                                                     [(memo "short enough")]))))))
             (it "takes an hour of speech-grade audio"
                 ;; 30 MB is about an hour; the cap sits above it so a generous bitrate is not
                 ;; mistaken for a recording nobody should wait for.
                 (expect (< (* 30 1024 1024) (long at/MAX_PAYLOAD_BYTES)))))
