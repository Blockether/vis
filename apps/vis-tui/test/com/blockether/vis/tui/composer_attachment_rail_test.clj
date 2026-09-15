(ns com.blockether.vis.tui.composer-attachment-rail-test
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.composer-attachment-rail :as rail]
            [com.blockether.vis.tui.primitives :as p]
            [lazytest.core :refer [defdescribe it expect]]))

(def attachments
  [{:id "image-1"
    :filename "diagram.png"
    :image-number 1
    :media-type "image/png"
    :size 153600
    :width 1280
    :height 720} {:id "doc-1" :filename "requirements.pdf" :media-type "application/pdf" :size 2048}
   {:id "audio-1" :filename "notes.wav" :media-type "audio/wav" :size 441}])

(defn- paint-rail
  ([cols focused?] (paint-rail cols focused? attachments))
  ([cols focused? staged]
   (.beginFrame interactions/hit-map)
   (let [capture (cap/capture!
                   {:cols cols
                    :rows 9
                    :paint!
                    (fn [{:keys [g]}]
                      (let [height
                            (rail/draw! g staged 1 cols {:focused? focused? :focused-index 1})]
                        (.commitFrame interactions/hit-map)
                        height))})]
     {:capture capture :regions (.current interactions/hit-map)})))

(defdescribe composer-attachment-rail
             (it "keeps every staged item individually visible with readable fallback metadata"
                 (let [{:keys [capture]}
                       (paint-rail 72 false)

                       text
                       (cap/frame-text capture)]

                   (expect (= 5 (rail/rail-height attachments)))
                   (expect (str/includes? text "IMAGE #1  diagram.png  ·  1280×720  ·  150 KB"))
                   (expect (str/includes? text "PDF  requirements.pdf  ·  2.0 KB"))
                   (expect (str/includes? text "AUDIO  notes.wav  ·  441 B"))))
             (it "preserves each remove target and a focused row on a narrow terminal"
                 (let [{:keys [capture regions]}
                       (paint-rail 32 true)

                       text
                       (cap/frame-text capture)]

                   (expect (str/includes? text "▶ PDF  require"))
                   (expect (= ["image-1" "doc-1" "audio-1"]
                              (mapv :attachment-id
                                    (filter #(= :attachment-remove (:kind %)) regions))))
                   (expect (= 3 (count (filter #(= :attachment-inspect (:kind %)) regions)))))))

(defdescribe compact-attachment-spacing
             ;; #249: a staged PNG must not reserve a yellow shadow row or a terminal-wide panel.
             (it "fits one image, its metadata and remove action without bottom or right padding"
                 (let [image
                       (assoc (first attachments)
                         :width 956
                         :height 118
                         :size 16384)

                       {:keys [capture]}
                       (paint-rail 100 false [image])

                       frame
                       (last (:frames capture))

                       paper
                       (:bg (get-in frame [0 0]))

                       expected-row
                       (str " │  " (rail/attachment-label image) " [remove] │")

                       right
                       (p/display-width expected-row)]

                   (expect (nil? (:error capture)))
                   (expect (= 3 (rail/rail-height [image]) (:ret capture)))
                   (expect (= expected-row (nth (str/split-lines (cap/frame-text capture)) 2)))
                   (expect (every? #(= paper (:bg %)) (nth frame 4)))
                   (expect (every? #(= paper (:bg %))
                                   (mapcat #(drop right (nth frame %)) [1 2 3])))))
             (it "reserves and paints nothing without attachments"
                 (let [{:keys [capture regions]} (paint-rail 100 false [])]
                   (expect (nil? (:error capture)))
                   (expect (= 0 (rail/rail-height []) (:ret capture)))
                   (expect (str/blank? (cap/frame-text capture)))
                   (expect (empty? regions))))
             (it "keeps all remove targets inside the terminal down to a single column"
                 (doseq [cols [1 2 3 4 5 8 16 24 32 72 100]]
                   (let [{:keys [capture regions]} (paint-rail cols true)]
                     (expect (nil? (:error capture)))
                     (expect (= 5 (:ret capture)))
                     (expect (= 3 (count (filter #(= :attachment-remove (:kind %)) regions))))
                     (expect (every? (fn [{:keys [bounds]}]
                                       (and (<= 0 (:col bounds))
                                            (<= (+ (:col bounds) (:width bounds)) cols)
                                            (<= 2 (:row bounds) 4)))
                                     regions))))))

(defdescribe
  composer-attachment-rail-transcription
  (it "spells every explicit transcript outcome on the staged row"
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
