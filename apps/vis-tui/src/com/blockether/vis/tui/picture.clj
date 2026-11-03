;; Pictures of an app `image` node in terminal cells.
(ns com.blockether.vis.tui.picture
  "Pictures of an app `image` node in terminal cells.

   [[layout]] and [[panes]] are pure: they decide where the old and the new picture
   go. [[raster]] decodes a picture into half-block pixels: one cell is one pixel
   wide and two pixels high, so the frame keeps the picture's aspect. It returns nil
   when the bytes do not decode, and the surface then paints a placeholder. It
   reads the size from the picture header first, and never decodes a picture over
   the contract's pixel bound."
  (:require [clojure.string :as str]
            [com.blockether.imaging :as imaging]
            [com.blockether.vis.contract.view :as view])
  (:import [com.googlecode.lanterna.terminal.image TerminalImage]
           [java.util Base64]))

(set! *warn-on-reflection* true)

(set! *unchecked-math* :warn-on-boxed)

(def side-min-cols "The narrowest picture of a side by side Before and After." 24)

(def gutter-cols "Columns between the Before and the After picture." 2)

(defn layout
  "How an image node shows in `text-w` columns: `:single` without an old picture,
   else `:side` when two pictures fit side by side, else `:stacked`. `flipped?`
   asks for the other compare layout; a narrow band stays stacked."
  [text-w before? flipped?]
  (let [fits? (>= (long text-w) (+ (* 2 (long side-min-cols)) (long gutter-cols)))]
    (cond (not before?) :single
          (and fits? (not flipped?)) :side
          :else :stacked)))

(defn panes
  "The pictures of `node` for `layout`: `[{:label :picture :col :cols}]`, Before first.
   `:col` is the first column of a pane inside `text-w`."
  [node text-w layout]
  (let [after
        (select-keys node [:media-type :data :width :height :bytes])

        before
        (:before node)

        text-w
        (long text-w)]

    (case layout
      :single
      [{:label nil :picture after :col 0 :cols text-w}]

      :stacked
      [{:label "Before" :picture before :col 0 :cols text-w}
       {:label "After" :picture after :col 0 :cols text-w}]

      :side
      (let [w (quot (- text-w (long gutter-cols)) 2)]
        [{:label "Before" :picture before :col 0 :cols w}
         {:label "After" :picture after :col (+ w (long gutter-cols)) :cols w}]))))

(def default-cell-aspect "Cell height over cell width when the terminal gives no metrics." 2.0)

(defn cell-aspect
  "Cell height over cell width of this terminal, from its live cell metrics."
  ^double []
  (let [w
        (long (TerminalImage/cellWidth))

        h
        (long (TerminalImage/cellHeight))]

    (if (and (pos? w) (pos? h)) (/ (double h) (double w)) (double default-cell-aspect))))

(defn fit-cells
  "The `[cols rows]` in cells of a `pw` x `ph` picture inside `cols` columns and
   `max-rows` cell rows. A cell is `aspect` times higher than wide, so the cells keep
   the picture's aspect within one cell. A picture never gets wider than its pixels."
  [pw ph cols max-rows aspect]
  (let [pw
        (max 1 (long pw))

        ph
        (max 1 (long ph))

        aspect
        (max 0.5 (double aspect))

        max-rows
        (max 1 (long max-rows))

        rows-of
        (fn [w]
          (/ (* (double w) ph) (* pw aspect)))

        w
        (max 1 (min (long cols) pw))]

    (if (<= (double (rows-of w)) max-rows)
      [w (max 1 (Math/round (double (rows-of w))))]
      [(max 1 (min w (Math/round (/ (* max-rows pw aspect) (double ph))))) max-rows])))

(defn fit
  "The `[width height]` in half-block pixels of a `pw` x `ph` picture inside `cols`
   columns and `max-rows` cell rows, after [[fit-cells]]. One cell holds two pixels
   above each other, so the height is even."
  ([pw ph cols max-rows] (fit pw ph cols max-rows default-cell-aspect))
  ([pw ph cols max-rows aspect]
   (let [[w rows] (fit-cells pw ph cols max-rows aspect)]
     [w (* 2 (long rows))])))

(defn byte-count
  "The bytes that base64 `data` holds."
  ^long [data]
  (let [s
        (str data)

        pad
        (count (take-while #(= \= %) (reverse s)))]

    (max 0 (- (quot (* 3 (count s)) 4) pad))))

(defn size-text
  "A short size: bytes, KB or MB."
  [^long n]
  (cond (< n 1024) (str n " B")
        (< n (* 1024 1024)) (format "%.0f KB" (/ n 1024.0))
        :else (format "%.1f MB" (/ n 1048576.0))))

(defn info-text
  "Media type, pixel size and byte size of `picture`, for example `PNG · 640x480 · 12 KB`."
  [{:keys [media-type data width height bytes]}]
  (str/join " · "
            (cond-> [(str/upper-case (str/replace (str (some-> media-type
                                                               name))
                                                  #"^image/"
                                                  ""))]
              (and width height)
              (conj (str width "x" height))

              (or bytes (seq data))
              (conj (size-text (long (or bytes (byte-count data))))))))

(defonce ^:private cache (atom {}))

(def ^:private cache-max "Frames kept: a few pictures at a few widths." 16)

(defn decodable-size
  "`[width height]` from the header of picture bytes `data`, or nil when the header
   gives no size or the picture has more pixels than `view/picture-bounds` allows.
   The header decides, because a decode of a huge picture can stop the JVM."
  [^bytes data media-type]
  (when-let [[w h] (view/picture-size data media-type)]
    (when (and (pos? (long w))
               (pos? (long h))
               (<= (* (long w) (long h)) (long (:pixels view/picture-bounds))))
      [w h])))

(defn data-bytes
  "The bytes of base64 picture `data`. Line breaks and spaces in the text are ignored."
  ^bytes [data]
  (.decode (Base64/getDecoder) (str/replace (str data) #"\s" "")))

(defn- decode*
  [data media-type cols max-rows aspect]
  (try (let [bytes (data-bytes data)]
         (when (decodable-size bytes media-type)
           (with-open [im (imaging/decode bytes)]
             (let [{:keys [width height]} (imaging/info im)
                   [w h] (fit width height cols max-rows aspect)]

               (with-open [flat (imaging/flatten im "#0b0e14ff")
                           small (imaging/resize flat w h :triangle)]

                 (let [^bytes rgba (imaging/pixels small)
                       n (* (long w) (long h))
                       out (int-array n)]

                   (dotimes [i n]
                     (let [o (* 4 i)]
                       (aset out
                             i
                             (unchecked-int (bit-or
                                              (bit-shift-left (bit-and 0xff (aget rgba o)) 16)
                                              (bit-shift-left (bit-and 0xff (aget rgba (+ o 1))) 8)
                                              (bit-and 0xff (aget rgba (+ o 2))))))))
                   {:width w :height h :pixels out :source-width width :source-height height}))))))
       (catch Throwable _ nil)))

(defn raster
  "The half-block frame `{:width :height :pixels}` of `picture` inside `cols`
   columns and `max-rows` cell rows, or nil when it does not decode or is over the
   pixel bound. The frame keeps the picture's aspect in cells of [[cell-aspect]]."
  [{:keys [data media-type]} cols max-rows]
  (when (seq data)
    (let [aspect
          (cell-aspect)

          k
          [(hash data) (count data) media-type cols max-rows aspect]]

      (if-let [hit (find @cache k)]
        (val hit)
        (let [frame (decode* data media-type cols max-rows aspect)]
          (swap! cache #(assoc (if (>= (count %) (long cache-max)) {} %) k frame))
          frame)))))

(defn viewer-entries
  "The pictures of app image `node` for the image viewer, the old one first:
   `[{:picture :ext :mime :name :width :height :label}]`. A picture without data is left out."
  [node]
  (let [entry
        (fn [label {:keys [media-type data width height] :as pic}]
          (when (seq data)
            {:picture pic
             :ext (str "."
                       (str/replace (str (some-> media-type
                                                 name))
                                    #"^image/"
                                    ""))
             :mime (some-> media-type
                           name)
             :name (str (or (:label node) (:alt node) "Picture") (when label (str " · " label)))
             :width width
             :height height
             :label (info-text pic)}))

        before
        (:before node)]

    (into []
          (keep identity)
          [(when before (entry "Before" before))
           (entry (when before "After") (select-keys node [:media-type :data :width :height]))])))

(defn cells
  "What one cell row of picture `parts` paints inside `limit` columns, in order.
   A frame part gives `{:col :top :bottom}` pixel cells, upper and lower colors.
   A text part gives `{:col :text :caption?}`, cut at `limit`. Nothing passes `limit`."
  [parts limit]
  (let [limit (long limit)]
    (into []
          (mapcat
            (fn [{:keys [col frame pixel-row pad text]}]
              (let [col (long col)]
                (if frame
                  (let [{:keys [width pixels]} frame
                        ^ints pixels pixels
                        w (long width)
                        start (+ col (long pad))
                        top (* 2 w (long pixel-row))]

                    (for [c (range (min w (max 0 (- limit start))))]
                      {:col (+ start (long c))
                       :top (aget pixels (+ top (long c)))
                       :bottom (aget pixels (+ top w (long c)))}))
                  (when (< col limit)
                    [{:col col
                      :text (str text)
                      :width (- limit col)
                      :caption? (contains? #{"Before" "After"} text)}])))))
          parts)))
