(ns com.blockether.vis.tui.composer-attachment-rail
  "Paint contract for staged composer attachments.

   Staged attachments share one compact bordered surface above the prompt.
   Every row retains its identity, filename, dimensions/size and remove action.
   C-x i focuses this keyboard surface without moving the text cursor."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.primitives :as p]
            [com.blockether.vis.tui.theme :as t]
            [com.blockether.vis.tui.format :as fmt])
  (:import [com.googlecode.lanterna SGR]
           [com.googlecode.lanterna.graphics TextGraphics]))

(defn rail-height
  "Rows reserved by `draw!`: one item row each and two borders, without a shadow."
  ^long [attachments]
  (if (seq attachments) (+ 2 (count attachments)) 0))

(defn- kind-label
  [media-type]
  (let [media-type (str/lower-case (or media-type ""))]
    (cond (str/starts-with? media-type "image/") "IMAGE"
          (str/starts-with? media-type "video/") "VIDEO"
          (str/starts-with? media-type "audio/") "AUDIO"
          (= media-type "application/pdf") "PDF"
          (str/starts-with? media-type "text/") "TEXT"
          :else "FILE")))

(def transcription-notes
  "The TUI's spelling of `com.blockether.vis.tui.client/audio-transcribe-statuses` — ONE
   table, read by the composer rail AND by chat history.

   A memo whose transcript is still being made must not look like one nobody will
   ever transcribe: that silence is exactly how a 47-minute recording reached a model
   carrying nothing but its filename."
  {"pending" "transcribing…" "unavailable" "no transcript" "silent" "no speech"})

(defn- with-live-transcription [attachment] attachment)

(defn attachment-label
  "Readable terminal fallback for one staged attachment.

   A recording also says what its transcript is doing, because the composer starts
   making the words the moment the file is staged and the human deserves to see that
   happening before the turn is sent."
  [{:keys [filename media-type size width height transcription transcription-status image-number]}]
  (let [size-label
        (fmt/format-bytes (or size 0) " ")

        note
        (or (get transcription-notes (str transcription-status))
            (when (not-empty (str transcription)) "transcript ready"))]

    (str (if image-number (str "IMAGE #" image-number) (kind-label media-type))
         "  "
         (or (not-empty filename) "unnamed attachment")
         "  ·  "
         (if (and width height) (str width "×" height "  ·  " size-label) size-label)
         (when note (str "  ·  " note)))))

(defn draw!
  "Paint a themed staging surface and bounded inspect/remove targets.

   Images keep the same number as their input reference. The panel fits its widest
   label and remove action, bounded by the terminal. Focus never hides the remove
   action, and all row content stays inside the border even at tiny widths."
  [^TextGraphics g attachments top cols {:keys [focused? focused-index]}]
  (when (seq attachments)
    (let [cols
          (max 1 (long cols))

          left
          (if (> cols 4) 1 0)

          remove-label
          " [remove] "

          label-w
          (reduce max 0 (map #(p/display-width (attachment-label %)) attachments))

          width
          ;; Two border cells and two cells for the focus marker.
          (max 1 (min (- cols (* 2 left)) (+ 4 label-w (p/display-width remove-label))))

          bordered?
          (>= width 4)

          inset
          (if bordered? 1 0)

          inner-w
          (max 0 (- width (* 2 inset)))

          content-left
          (+ left inset)

          remove-label
          (if (>= inner-w 18) remove-label " × ")

          remove-label
          (p/truncate-cols remove-label inner-w)

          remove-w
          (long (p/display-width remove-label))

          body-w
          (max 0 (- inner-w remove-w))

          height
          (rail-height attachments)]

      (p/set-colors! g t/dialog-border t/dialog-bg)
      (p/fill-rect! g left top width height)
      (when bordered?
        (p/draw-box! g left top width height)
        (p/put-str! g (+ left 2) top (p/truncate-cols " Attachments " (max 0 (- width 4)))))
      (doseq [[idx attachment] (map-indexed vector (map with-live-transcription attachments))]
        (let [row (+ (long top) 1 (long idx))
              focused-row? (and focused? (= (long (or focused-index 0)) (long idx)))
              body (p/truncate-cols (str (if focused-row? "▶ " "  ") (attachment-label attachment))
                                    body-w)
              padded (str body
                          (apply str (repeat (max 0 (- body-w (p/display-width body))) \space)))
              id (:id attachment)
              bg (if focused-row? t/header-active-tab-bg t/dialog-bg)]

          (p/set-colors! g (if focused-row? t/header-active-tab-fg t/dialog-fg) bg)
          (when focused-row? (.enableModifiers g (into-array SGR [SGR/BOLD])))
          (p/put-str! g content-left row padded)
          (when focused-row? (.disableModifiers g (into-array SGR [SGR/BOLD])))
          (p/set-colors! g (if focused-row? t/header-active-tab-fg t/dialog-hint) bg)
          (p/put-str! g (+ content-left body-w) row remove-label)
          (when (pos? body-w)
            (.register interactions/hit-map
                       {:bounds {:row row :col content-left :width body-w}
                        :kind :attachment-inspect
                        :attachment attachment
                        :attachment-id id
                        :enabled? true}))
          (when (pos? remove-w)
            (.register interactions/hit-map
                       {:bounds {:row row :col (+ content-left body-w) :width remove-w}
                        :kind :attachment-remove
                        :attachment-id id
                        :enabled? true}))))))
  (rail-height attachments))
