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
                 (let [_ (paint-rail 72 false)]
                   (expect (= 5 (rail/rail-height attachments)))))
             (it "preserves each remove target and a focused row on a narrow terminal"
                 (let [{:keys [regions]} (paint-rail 32 true)]
                   (expect (= ["image-1" "doc-1" "audio-1"]
                              (mapv :attachment-id
                                    (filter #(= :attachment-remove (:kind %)) regions))))
                   (expect (= 3 (count (filter #(= :attachment-inspect (:kind %)) regions)))))))

(defdescribe compact-attachment-spacing
             ;; #249: a staged PNG must not reserve a yellow shadow row.
             ;; Regression, user report: the list must span the input, not hug its contents.
             (it "uses the full input width for one image without a shadow row"
                 (let [image
                       (assoc (first attachments)
                         :width 956
                         :height 118
                         :size 16384)

                       {:keys [capture regions]}
                       (paint-rail 100 false [image])

                       row
                       (nth (str/split-lines (cap/frame-text capture)) 2)

                       remove-bounds
                       (:bounds (first (filter #(= :attachment-remove (:kind %)) regions)))]

                   (expect (nil? (:error capture)))
                   (expect (= 3 (rail/rail-height [image]) (:ret capture)))
                   (expect (= 100 (p/display-width row)))
                   (expect (= 98 (+ (:col remove-bounds) (:width remove-bounds))))))
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
  zip-archive-rail-label
  (it "labels a staged ZIP archive ZIP, also under the Windows spelling"
      (doseq [media-type ["application/zip" "application/x-zip-compressed"]]
        (let [{:keys [capture]}
              (paint-rail 72
                          false
                          [{:id "zip-1" :filename "bundle.zip" :media-type media-type :size 2048}])]
          (expect (str/includes? (cap/frame-text capture) "ZIP  bundle.zip  ·  2.0 KB"))))))
