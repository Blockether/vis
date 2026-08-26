(ns com.blockether.vis.ext.channel-tui.composer-attachment-rail
  "Paint contract for staged composer attachments.

   Every attachment owns exactly one terminal row directly above the prompt. The row
   remains useful without terminal image protocols: kind, filename, dimensions/size,
   and a remove affordance are always text. `:attachment-focus?` highlights exactly
   one row; C-x i enters that keyboard surface without moving the text cursor."
  (:require [clojure.string :as str]
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

(defn attachment-label
  "Readable terminal fallback for one staged attachment."
  [{:keys [filename media-type size width height]}]
  (let [size-label (fmt/format-bytes (or size 0) " ")]
    (str (kind-label media-type)
         "  " (or (not-empty filename) "unnamed attachment")
         "  ·  " (if (and width height) (str width "×" height "  ·  " size-label) size-label))))

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

    (doseq [[idx attachment] (map-indexed vector attachments)]
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
