(ns com.blockether.vis.ext.channel-tui.composer-attachment-rail
  "Paint contract for staged composer attachments.

   Every attachment owns exactly one terminal row directly above the prompt. The row
   remains useful without terminal image protocols: kind, filename, dimensions/size,
   and a remove affordance are always text. `:attachment-focus?` highlights exactly
   one row; C-x i enters that keyboard surface without moving the text cursor."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.click-regions :as cr]
            [com.blockether.vis.ext.channel-tui.primitives :as p]
            [com.blockether.vis.ext.channel-tui.theme :as t]
            [com.blockether.vis.internal.format :as fmt])
  (:import [com.googlecode.lanterna SGR]
           [com.googlecode.lanterna.graphics TextGraphics]))

(defn rail-height
  "Rows reserved by `draw!`; one readable row per staged item."
  ^long [attachments]
  (count attachments))

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
  "The TUI's spelling of `com.blockether.vis.core/audio-transcribe-statuses` — ONE
   table, read by the composer rail AND by chat history.

   A memo whose transcript is still being made must not look like one nobody will
   ever transcribe: that silence is exactly how a 47-minute recording reached a model
   carrying nothing but its filename."
  {"pending" "transcribing…" "unavailable" "no transcript" "silent" "no speech"})

(defn- with-live-transcription
  "The row plus whatever the transcription registry knows about it RIGHT NOW.

   The rail ASKS on every frame instead of remembering an answer: the words are made
   on a worker while the human types, so a row that says \"transcribing…\" has to be
   able to stop saying it without anybody pressing a key."
  [{:keys [media-type transcription] :as attachment}]
  (if (or (not= "AUDIO" (kind-label media-type)) (not-empty (str transcription)))
    attachment
    (let [outcome (vis/audio-transcribe-outcome attachment)]
      (cond-> attachment
        (:transcription outcome)
        (assoc :transcription (:transcription outcome))

        (:status outcome)
        (assoc :transcription-status (:status outcome))))))

(defn attachment-label
  "Readable terminal fallback for one staged attachment.

   A recording also says what its transcript is doing, because the composer starts
   making the words the moment the file is staged and the human deserves to see that
   happening before the turn is sent."
  [{:keys [filename media-type size width height transcription transcription-status]}]
  (let [size-label
        (fmt/format-bytes (or size 0) " ")

        note
        (or (get transcription-notes (str transcription-status))
            (when (not-empty (str transcription)) "transcript ready"))]

    (str (kind-label media-type)
         "  "
         (or (not-empty filename) "unnamed attachment")
         "  ·  "
         (if (and width height) (str width "×" height "  ·  " size-label) size-label)
         (when note (str "  ·  " note)))))

(defn draw!
  "Paint all staged attachments at `top` and register per-row inspect/remove targets.

   Focus is visual only unless `focused?` is true. Each row keeps its full metadata
   in the click region; the fixed `[remove]` suffix never disappears on narrow
   terminals, while the descriptive fallback truncates to the remaining columns."
  [^TextGraphics g attachments top cols {:keys [focused? focused-index]}]
  (let [cols
        (long cols)

        remove-label
        " [remove] "

        remove-w
        (long (p/display-width remove-label))]

    (doseq [[idx attachment] (map-indexed vector (map with-live-transcription attachments))]
      (let [row (+ (long top) (long idx))
            focused-row? (and focused? (= (long (or focused-index 0)) (long idx)))
            body-w (max 0 (- cols remove-w))
            body (p/truncate-cols (str (if focused-row? "▶ " "  ") (attachment-label attachment))
                                  body-w)
            padded (str body (apply str (repeat (max 0 (- body-w (p/display-width body))) \space)))
            id (:id attachment)]

        (.setForegroundColor g (if focused-row? t/header-active-tab-fg t/box-fg))
        (.setBackgroundColor g (if focused-row? t/header-active-tab-bg t/terminal-bg))
        (when focused-row? (.enableModifiers g (into-array SGR [SGR/BOLD])))
        (p/put-str! g 0 row padded)
        (when focused-row? (.disableModifiers g (into-array SGR [SGR/BOLD])))
        (.setForegroundColor g t/dialog-hint)
        (.setBackgroundColor g t/terminal-bg)
        (p/put-str! g body-w row remove-label)
        (cr/register! {:bounds {:row row :col 0 :width body-w}
                       :kind :attachment-inspect
                       :attachment attachment
                       :attachment-id id
                       :enabled? true})
        (cr/register! {:bounds {:row row :col body-w :width remove-w}
                       :kind :attachment-remove
                       :attachment-id id
                       :enabled? true})))
    (rail-height attachments)))
