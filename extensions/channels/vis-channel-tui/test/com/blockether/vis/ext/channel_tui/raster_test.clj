(ns com.blockether.vis.ext.channel-tui.raster-test
  "The \"look at the pixels\" path, END TO END: a captured grid -> imaging draw
   ops with an EMBEDDED mono face -> a real PNG on disk.

   Deliberately an INTEGRATION test. None of what it pins is observable from the
   pure functions around it -- a screenshot whose glyphs silently stopped
   painting, or whose bold, italic and underline quietly rasterized as plain
   text, still produces a plausible-looking File."
  (:require [com.blockether.vis.ext.channel-tui.capture :as cap]
            [com.blockether.vis.ext.channel-tui.raster :as raster]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- cell [ch fg bg bold] {:ch (str ch) :fg fg :bg bg :bold bold})

(defn- row
  "One captured row `cols` wide, `s` left-aligned into it."
  ([^String s cols fg bg] (row s cols fg bg false))
  ([^String s cols fg bg bold]
   (vec (for [x (range cols)]
          (cell (if (< (long x) (.length s)) (.charAt s (int x)) \space) fg bg bold)))))


(defn- png!
  "One captured grid straight to a PNG path, through the shared capture helper."
  [nm grid]
  (cap/shot!
    {:grid grid :out (str (System/getProperty "java.io.tmpdir") "/" nm) :font-size 16 :trim false}))

(defn- styled
  "A captured row with `k` (`:bold` `:italic` `:underline`) set on every cell."
  [row k]
  (mapv #(assoc % k true) row))

(defn- left-ink
  "Raster row → its LEFTMOST inked column, for every row that carries ink. A slant
   shows up here as a profile that leans, where upright type gives a flat one."
  [rows]
  (into {}
        (keep-indexed (fn [y row]
                        (when-let [x (first (keep-indexed (fn [x px]
                                                            (when (not= [255 255 255] px) x))
                                                          row))]
                          [y x]))
                      rows)))

(defdescribe
  grid->png-test
  "`grid->png!` is the \"look at the pixels\" path: ONE captured grid straight to a
   PNG."
  (it "paints an untouched grid on the theme's paper, never on a black void"
      ;; Regression: Lanterna reports an unpainted cell's colour as DEFAULT, which
      ;; reads back as ANSI black -- every screenshot of the app came out on black
      ;; paper, a colour the app never shows.
      (let [paper
            (:bg (raster/cell nil))

            painted
            (set (apply concat
                   (cap/png-rows (png! "vis-raster-paper.png"
                                       [(vec (repeat 8 (raster/cell nil)))]))))]

        (expect (not= [0 0 0] paper))
        (expect (= #{paper} painted))))
  (it "joins box-drawing cells into one unbroken rule, with no seam between columns"
      ;; A font's `\u2500` stops at its own advance, so drawn as GLYPHS a border shows
      ;; hairline gaps at the cell seams; drawn as bars through the cell centre it
      ;; cannot. Every column of the middle raster row must therefore carry ink.
      (let [rows
            (cap/png-rows
              (png!
                "vis-raster-rule.png"
                [(row "\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500" 8 [0 0 0] [255 255 255])]))

            middle
            (nth rows (quot (count rows) 2))]

        (expect (pos? (count middle)))
        (expect (every? #(not= [255 255 255] %) middle))))
  (it "paints a BOLD run visibly heavier than the same text unstyled"
      ;; The embedded mono face carries ONE weight, so a `:weight 700` op resolves
      ;; to exactly the same outlines as `:weight 400`: the table card's bold
      ;; header -- `:bold true` in the Lanterna back-buffer -- rasterized as plain
      ;; text, and every screenshot taken of it lied about the app.
      (let [ink
            (fn [nm bold]
              (cap/ink (png! nm [(row "Name Size" 12 [0 0 0] [255 255 255] bold)])))

            plain
            (ink "vis-raster-plain.png" false)

            heavy
            (ink "vis-raster-bold.png" true)]

        (expect (pos? plain))
        (expect (> heavy (* 1.1 plain)))))
  (it
    "slants an ITALIC run instead of painting it upright"
    ;; Regression: the embedded mono face ships upright ONLY, so `:italic true`
    ;; on a draw op paints the very same outlines back -- every `p/ITALIC` cell
    ;; the transcript, the dialogs and the header paint was photographed as
    ;; roman text.
    (let [roman
          (cap/png-rows (png! "vis-raster-roman.png" [(row "Name Size" 12 [0 0 0] [255 255 255])]))

          slanted
          (cap/png-rows (png! "vis-raster-italic.png"
                              [(styled (row "Name Size" 12 [0 0 0] [255 255 255]) :italic)]))

          upright-profile
          (left-ink roman)

          slanted-profile
          (left-ink slanted)

          top
          (apply min (keys upright-profile))

          bottom
          (apply max (keys upright-profile))]

      (expect (seq upright-profile))
      (expect (not= roman slanted))
      ;; a shear MOVES ink sideways; it must not lose the type on the way
      (expect (< 0.7 (/ (double (cap/ink slanted)) (double (cap/ink roman))) 1.4))
      ;; the top of the glyphs leans right, the baseline stays where it was
      (expect (> (long (get slanted-profile top)) (+ 2 (long (get upright-profile top)))))
      (expect (<= (long (get slanted-profile bottom)) (+ 1 (long (get upright-profile bottom)))))))
  (it "paints an UNDERLINE run as a real rule under the baseline"
      ;; Regression: `SGR/UNDERLINE` -- every link the TUI prints -- was dropped by
      ;; the capture outright, so an underlined run rasterized as plain text.
      (let [rule?
            (fn [rows]
              (boolean (some (fn [r]
                               (every? #(not= [255 255 255] %) (take 80 r)))
                             rows)))

            plain
            (cap/png-rows (png! "vis-raster-plain-rule.png"
                                [(row "Name Size" 12 [0 0 0] [255 255 255])]))

            lined
            (cap/png-rows (png! "vis-raster-underline.png"
                                [(styled (row "Name Size" 12 [0 0 0] [255 255 255]) :underline)]))]

        (expect (not (rule? plain)))
        (expect (rule? lined))
        (expect (> (cap/ink lined) (cap/ink plain))))))
