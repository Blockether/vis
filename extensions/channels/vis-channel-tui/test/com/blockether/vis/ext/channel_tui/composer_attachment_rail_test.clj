(ns com.blockether.vis.ext.channel-tui.composer-attachment-rail-test
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.capture :as cap]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.composer-attachment-rail :as rail]
            [lazytest.core :refer [defdescribe it expect]]))

(def attachments
  [{:id "image-1"
    :filename "diagram.png"
    :media-type "image/png"
    :size 153600
    :width 1280
    :height 720} {:id "doc-1" :filename "requirements.pdf" :media-type "application/pdf" :size 2048}
   {:id "audio-1" :filename "notes.wav" :media-type "audio/wav" :size 441}])

(defn- paint-rail
  [cols focused?]
  (cr/begin-frame!)
  (let [capture (cap/capture!
                  {:cols cols
                   :rows 5
                   :paint! (fn [{:keys [g]}]
                             (rail/draw! g attachments 1 cols {:focused? focused? :focused-index 1})
                             (cr/commit-frame!))})]
    {:capture capture :regions (cr/current)}))

(defdescribe composer-attachment-rail
             (it "keeps every staged item individually visible with readable fallback metadata"
                 (let [{:keys [capture]}
                       (paint-rail 72 false)

                       text
                       (cap/frame-text capture)]

                   (expect (= 3 (rail/rail-height attachments)))
                   (expect (str/includes? text "IMAGE  diagram.png  ·  1280×720  ·  150 KB"))
                   (expect (str/includes? text "PDF  requirements.pdf  ·  2.0 KB"))
                   (expect (str/includes? text "AUDIO  notes.wav  ·  441 B"))))
             (it "preserves each remove target and a focused row on a narrow terminal"
                 (let [{:keys [capture regions]}
                       (paint-rail 32 true)

                       text
                       (cap/frame-text capture)]

                   (expect (str/includes? text "▶ PDF  requirements"))
                   (expect (= ["image-1" "doc-1" "audio-1"]
                              (mapv :attachment-id
                                    (filter #(= :attachment-remove (:kind %)) regions))))
                   (expect (= 3 (count (filter #(= :attachment-inspect (:kind %)) regions)))))))

;; Regression, issue: a 47-minute recording rode a turn carrying nothing but its
;; filename, and the composer looked exactly the same as one whose words were ready.
(defdescribe
  composer-attachment-rail-transcription
  (it "says a staged recording is being transcribed while the human is still typing"
      (with-redefs [vis/audio-transcribe-outcome (fn [attachment]
                                                   (when (= "notes.wav" (:filename attachment))
                                                     {:status "pending"}))]
        (let [{:keys [capture]} (paint-rail 72 false)]
          (expect (str/includes? (cap/frame-text capture)
                                 "AUDIO  notes.wav  ·  441 B  ·  transcribing…")))))
  (it "spells every outcome the transcription registry can answer"
      (expect (= #{"pending" "unavailable" "silent"} (set (keys rail/transcription-notes))))
      (expect (str/includes? (rail/attachment-label {:filename "memo.m4a"
                                                     :media-type "audio/mp4"
                                                     :size 441
                                                     :transcription-status "unavailable"})
                             "no transcript"))
      (expect (str/includes? (rail/attachment-label {:filename "memo.m4a"
                                                     :media-type "audio/mp4"
                                                     :size 441
                                                     :transcription "hello there"})
                             "transcript ready"))))
