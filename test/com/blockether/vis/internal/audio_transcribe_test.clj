(ns com.blockether.vis.internal.audio-transcribe-test
  "A recording is text the moment it arrives, or it SAYS why it is not."
  (:require [com.blockether.vis.internal.audio-transcribe :as at]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.voice :as voice]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]])
  (:import [java.util Base64]))

(def ^:private engine-id :audio-transcribe-test-engine)

(def ^:private calls
  "Every audio path the fake engine was handed, in order — what proves the work is
   done once."
  (atom []))

(defn- register-fake!
  "A transcription engine that answers `answer` (or throws it, when it is a
   Throwable) for whatever file it is given, after `hold` — when one is given —
   is delivered."
  ([answer] (register-fake! answer nil))
  ([answer hold]
   (voice/register-engine! :transcribe
                           {:id engine-id
                            :label "test"
                            :transcribe (fn [{:keys [audio-path]}]
                                          (swap! calls conj (str audio-path))
                                          (when hold (deref hold 10000 nil))
                                          (if (instance? Throwable answer) (throw answer) answer))})
   (voice/set-default-engine! :transcribe engine-id)))

(set-ns-context! [(around-each [f]
                               (reset! calls [])
                               (at/clear-cache!)
                               (try (f)
                                    (finally (voice/set-default-engine! :transcribe nil)
                                             (voice/unregister-engine! :transcribe engine-id)
                                             (at/clear-cache!))))])

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
      (with-redefs [voice/resolve-engine (fn [& _]
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
