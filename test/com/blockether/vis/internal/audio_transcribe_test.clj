(ns com.blockether.vis.internal.audio-transcribe-test
  "A recording is text the moment it arrives, or it is nothing the model can use."
  (:require [com.blockether.vis.internal.audio-transcribe :as at]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.voice :as voice]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]])
  (:import [java.util Base64]))

(def ^:private engine-id :audio-transcribe-test-engine)

(def ^:private calls
  "Every audio path the fake engine was handed, in order — what proves the cache."
  (atom []))

(defn- register-fake!
  "A transcription engine that answers `answer` (or throws it, when it is a
   Throwable) for whatever file it is given."
  [answer]
  (voice/register-engine! :transcribe
                          {:id engine-id
                           :label "test"
                           :transcribe (fn [{:keys [audio-path]}]
                                         (swap! calls conj (str audio-path))
                                         (if (instance? Throwable answer) (throw answer) answer))})
  (voice/set-default-engine! :transcribe engine-id))

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
  (it "keys the cache on the BYTES, not the file name"
      (register-fake! "words")
      (at/transcribe-attachments [(memo "first recording")])
      (at/transcribe-attachments [(memo "second recording")])
      (expect (= 2 (count @calls))))
  (it "hands back the recording untouched when the engine throws"
      ;; A failed transcription is a missing convenience, never a lost attachment:
      ;; the memo is still stored, still played, still named to the model.
      (register-fake! (ex-info "model exploded" {}))
      (let [rows (at/transcribe-attachments [(memo)])]
        (expect (= 1 (count rows)))
        (expect (nil? (:transcription (first rows))))))
  (it "does nothing at all when the toggle is off"
      (register-fake! "words")
      (with-redefs [toggles/enabled? (fn [id]
                                       (not= at/TOGGLE_ID id))]
        (expect (nil? (:transcription (first (at/transcribe-attachments [(memo)])))))
        (expect (empty? @calls))))
  (it "does nothing at all when this build carries no speech engine"
      ;; No extension loaded, or its engine failed to load: `resolve-engine` THROWS,
      ;; and that throw must never reach the turn.
      (with-redefs [voice/resolve-engine (fn [& _]
                                           (throw (ex-info "none registered" {})))]
        (expect (false? (at/available?)))
        (expect (= [(memo)] (at/transcribe-attachments [(memo)])))))
  (it "keeps a transcript somebody already computed"
      (register-fake! "fresh words")
      (let [rows (at/transcribe-attachments [(assoc (memo) :transcription "stored words")])]
        (expect (= "stored words" (:transcription (first rows))))
        (expect (empty? @calls)))))

(defdescribe manifest-test
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
             (it "still names an unreadable recording so the model knows it exists"
                 (let [content (user-content [(memo)])]
                   (expect (re-find #"open the file to hear it" (str content)))
                   (expect (not (re-find #"transcript of the recording" (str content)))))))
