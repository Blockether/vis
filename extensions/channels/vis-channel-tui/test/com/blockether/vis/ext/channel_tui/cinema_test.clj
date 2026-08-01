(ns com.blockether.vis.ext.channel-tui.cinema-test
  "The MP4 export pipeline, END TO END: a captured grid -> imaging-rendered RGB
   `Picture`s -> jcodec H.264 -> a real `.mp4` on disk.

   This is deliberately an INTEGRATION test. Every step below used to run on
   Java2D (`BufferedImage` + `AWTSequenceEncoder` + a system font); it now runs
   on the imaging cdylib with an EMBEDDED mono face, and none of that is
   observable from the pure functions around it -- a screencast whose glyphs
   silently stopped painting, or whose frame geometry stopped being even (H.264
   refuses odd dimensions), still produces a plausible-looking File."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.ext.channel-tui.cinema :as cinema]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- cell
  [ch fg bg]
  {:ch (str ch) :fg fg :bg bg :bold false :italic false :underline false :reverse false})

(defn- row
  "One captured row `cols` wide, `s` left-aligned into it."
  [^String s cols fg bg]
  (vec (for [x (range cols)]
         (cell (if (< x (count s)) (.charAt s x) \space) fg bg))))

(defn- frames
  "`n` frames of a `cols`x2 grid. `text?` false paints blank cells only -- same
   geometry, no glyphs, which is what makes the two encodes comparable."
  [n cols text?]
  (vec (for [i (range n)]
         {:grid [(row (if text? (str "vis cinema " i) "") cols [230 230 230] [20 20 28])
                 (row (if text? "colour ✓ ĄŻ 你好" "") cols [90 200 120] [20 20 28])]})))

(defn- mp4!
  [nm cols text?]
  (let [out (io/file (System/getProperty "java.io.tmpdir") nm)]
    (.delete out)
    (.deleteOnExit out)
    (cinema/frames->mp4! {:cols cols :rows 2 :frames (frames 2 cols text?)}
                         out
                         {:font-size 18 :fps 6})))

(defn- bytes-of ^bytes [f] (java.nio.file.Files/readAllBytes (.toPath (io/file f))))

(defn- ascii-index
  "Index of the first occurrence of ASCII `tag` in `ba` at or after `from`."
  ([^bytes ba ^String tag] (ascii-index ba tag 0))
  ([^bytes ba ^String tag from]
   (let [t (.getBytes tag "US-ASCII")]
     (first (for
              [i (range (long from) (- (alength ba) (alength t)))
               :when (every? #(= (aget ba (+ i (long %))) (aget t (int %))) (range (alength t)))]

              i)))))

(defn- u16
  [^bytes ba i]
  (+ (* 256 (bit-and (aget ba (int i)) 0xff)) (bit-and (aget ba (int (inc i))) 0xff)))

(defn- avc-dimensions
  "`[w h]` from the MP4's `avc1` VisualSampleEntry: 6 reserved + 2 data-ref +
   16 pre-defined/reserved after the box type, then width and height. Searched
   from `stsd` on purpose -- `avc1` is ALSO an ftyp compatible brand, 24 bytes
   into the file, and that copy is not a sample entry at all."
  [^bytes ba]
  (when-let [stsd (ascii-index ba "stsd")]
    (when-let [i (ascii-index ba "avc1" stsd)]
      [(u16 ba (+ i 28)) (u16 ba (+ i 30))])))

(defdescribe frames->mp4-test
             (it "writes a real, non-empty MP4 container"
                 (let
                   [f
                    (mp4! "vis-cinema-test.mp4" 24 true)

                    ba
                    (bytes-of f)]

                   (expect (.exists f))
                   ;; `ftyp` is the first box of every ISO base-media file.
                   (expect (= 4 (ascii-index ba "ftyp")))
                   (expect (some? (ascii-index ba "avc1")))
                   (expect (< 1000 (alength ba)))))
             (it "sizes the video from the CELL metrics, in even H.264 dimensions"
                 (let
                   [[w h]
                    (avc-dimensions (bytes-of (mp4! "vis-cinema-test.mp4" 24 true)))

                    [w2 _]
                    (avc-dimensions (bytes-of (mp4! "vis-cinema-wide-test.mp4" 48 true)))]

                   (expect (pos? w))
                   (expect (pos? h))
                   ;; H.264 4:2:0 cannot encode an odd width/height at all.
                   (expect (even? w))
                   (expect (even? h))
                   ;; Twice the columns is twice the frame, give or take one rounding cell.
                   (expect (< (Math/abs (- (double w2) (* 2.0 w))) (/ w 12.0)))))
             (it "actually PAINTS the glyphs -- text costs bits, blank cells do not"
                 (let
                   [inked
                    (alength (bytes-of (mp4! "vis-cinema-test.mp4" 24 true)))

                    blank
                    (alength (bytes-of (mp4! "vis-cinema-blank-test.mp4" 24 false)))]

                   (expect (> inked (* 1.5 blank))))))
