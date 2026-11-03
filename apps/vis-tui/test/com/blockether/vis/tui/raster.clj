(ns com.blockether.vis.tui.raster
  "A captured terminal grid as a PNG, in the theme's own colours.

   The pixel half of `capture`, and developer tooling like it: a Lanterna
   back-buffer becomes an image IN PROCESS through `com.blockether.imaging`
   (FFM, embedded Noto Sans Mono), so a screenshot looks the same on every
   machine — no system font, no `java.desktop`, nothing to install.

   [[cell]] reads one `TextCharacter` as raster data; [[grid->png!]] paints a
   whole grid. `capture/shot!` wraps the pair into the one-call screenshot a
   design review looks at."
  (:require [clojure.string :as str]
            [com.blockether.imaging :as img]
            [com.blockether.vis.tui.theme :as tui-theme])
  (:import [com.googlecode.lanterna SGR TextCharacter TextColor TextColor$ANSI]
           [java.io File]))

(defn- rgb [^TextColor color] [(.getRed color) (.getGreen color) (.getBlue color)])

(defn cell
  "One Lanterna `TextCharacter` as a raster cell — `{:ch :fg :bg :bold :italic
   :underline}` holding the colours AND the styles a TERMINAL would really show.

   Lanterna's `DEFAULT` colour means \"whatever the host terminal paints\", and
   `.getRed` reports it as ANSI black — which is why an unpainted cell used to
   rasterise as a pitch-black void instead of the app's paper. Here it resolves to
   the theme's own ink/paper. A REVERSE cell is captured already swapped, since
   nothing downstream carries the modifier."
  [^TextCharacter tc]
  (if tc
    (let [mods
          (.getModifiers tc)

          shown
          (fn [^TextColor c fallback]
            (rgb (if (or (nil? c) (= TextColor$ANSI/DEFAULT c)) fallback c)))

          fg
          (shown (.getForegroundColor tc) tui-theme/text-fg)

          bg
          (shown (.getBackgroundColor tc) tui-theme/terminal-bg)

          reversed?
          (.contains mods SGR/REVERSE)]

      {:ch (or (.getCharacterString tc) " ")
       :fg (if reversed? bg fg)
       :bg (if reversed? fg bg)
       :bold (boolean (.contains mods SGR/BOLD))
       :italic (boolean (.contains mods SGR/ITALIC))
       :underline (boolean (.contains mods SGR/UNDERLINE))})
    {:ch " "
     :fg (rgb tui-theme/text-fg)
     :bg (rgb tui-theme/terminal-bg)
     :bold false
     :italic false
     :underline false}))

(defn- hex-color
  "`[r g b]` as the `#rrggbbaa` string the imaging draw ops take."
  ^String [[r g b]]
  (format "#%02x%02x%02xff" (int r) (int g) (int b)))

(defn- io-file ^File [x] (if (instance? File x) x (File. (str x))))

(def ^:private mono-family
  "The mono face EMBEDDED in the imaging cdylib — never a system font, so a
   screenshot renders identically on every machine."
  "Noto Sans Mono")

(defn- cell-metrics
  "Mono cell geometry for `size`, measured through imaging's shaper.

   `:letter-spacing` is the sub-pixel slack between the font's real advance and
   the integer cell width: with it, every glyph of a merged text run lands
   exactly on its column, so a 120-column line cannot drift."
  [^long size]
  (let [m
        (img/text-measure {:text "M" :size size :family mono-family})

        adv
        (double (:width m))

        cw
        (max 1 (Math/round adv))]

    {:cw cw
     :ch (max 1 (Math/round (double (:height m))))
     :ascent (Math/round (- (double (:y m))))
     :letter-spacing (- cw adv)}))

(defn- narrow?
  "True for a single-cell glyph we may merge into a run. Wide/combining/multi-char
   cells are drawn on their own so their advance cannot shift the rest of the row."
  [^String s]
  (and (= 1 (.length s)) (< (int (.charAt s 0)) 0x1100)))

(defn- run-style
  "The styling a cell carries into the raster: `[fg bold italic underline]`."
  [c]
  [(:fg c) (boolean (:bold c)) (boolean (:italic c)) (boolean (:underline c))])

(defn- text-runs
  "Split one captured row into `{:x :style :text}` runs of consecutive cells that
   share `run-style` — one draw op instead of one per column."
  [row]
  (loop [cells
         (map-indexed vector row)

         out
         []]

    (if-let [[i c] (first cells)]
      (let [ch (or (:ch c) " ")
            style (run-style c)]

        (if (narrow? ch)
          (let [run (take-while (fn [[_ d]]
                                  (and (narrow? (or (:ch d) " ")) (= style (run-style d))))
                                (rest cells))]
            (recur (drop (inc (count run)) cells)
                   (conj out
                         {:x i
                          :style style
                          :text (apply str
                                  ch
                                  (map (fn [[_ d]]
                                         (or (:ch d) " "))
                                       run))})))
          (recur (rest cells) (conj out {:x i :style style :text ch}))))
      out)))

(def ^:private box-arms
  "Box-drawing glyph → the `[left right up down]` arms it extends.

   These cells are painted as BARS through the cell centre instead of as glyphs:
   a font's `─` stops at its own advance, so a border shows hairline seams between
   columns at some cell sizes. Bars meet in the middle of every cell, so they
   cannot."
  {"─" [1 1 0 0]
   "━" [1 1 0 0]
   "═" [1 1 0 0]
   "╴" [1 0 0 0]
   "╶" [0 1 0 0]
   "│" [0 0 1 1]
   "┃" [0 0 1 1]
   "║" [0 0 1 1]
   "┌" [0 1 0 1]
   "┏" [0 1 0 1]
   "╭" [0 1 0 1]
   "╔" [0 1 0 1]
   "┐" [1 0 0 1]
   "┓" [1 0 0 1]
   "╮" [1 0 0 1]
   "╗" [1 0 0 1]
   "└" [0 1 1 0]
   "┗" [0 1 1 0]
   "╰" [0 1 1 0]
   "╚" [0 1 1 0]
   "┘" [1 0 1 0]
   "┛" [1 0 1 0]
   "╯" [1 0 1 0]
   "╝" [1 0 1 0]
   "├" [0 1 1 1]
   "┣" [0 1 1 1]
   "╠" [0 1 1 1]
   "┤" [1 0 1 1]
   "┫" [1 0 1 1]
   "╣" [1 0 1 1]
   "┬" [1 1 0 1]
   "┳" [1 1 0 1]
   "╦" [1 1 0 1]
   "┴" [1 1 1 0]
   "┻" [1 1 1 0]
   "╩" [1 1 1 0]
   "┼" [1 1 1 1]
   "╋" [1 1 1 1]
   "╬" [1 1 1 1]})

(defn- box-ops
  "Bars for ONE box-drawing cell — each arm runs from the cell centre to the edge
   it points at, so neighbouring cells overlap and the line is continuous. `[]`
   for anything that is not a box glyph."
  [c px py cell-w cell-h]
  (if-let [[l r u d] (box-arms (:ch c))]
    (let [w (long cell-w)
          h (long cell-h)
          t (max 1 (long (Math/round (/ (double h) 12.0))))
          x0 (long px)
          y0 (long py)
          cx (+ x0 (quot w 2))
          cy (+ y0 (quot h 2))
          fill (hex-color (:fg c))]

      (cond-> []
        (pos? (long l))
        (conj {:op :rect :x x0 :y cy :w (+ (- cx x0) t) :h t :fill fill})

        (pos? (long r))
        (conj {:op :rect :x cx :y cy :w (- (+ x0 w) cx) :h t :fill fill})

        (pos? (long u))
        (conj {:op :rect :x cx :y y0 :w t :h (+ (- cy y0) t) :fill fill})

        (pos? (long d))
        (conj {:op :rect :x cx :y cy :w t :h (- (+ y0 h) cy) :fill fill})))
    []))

(def ^:private italic-shear
  "Slant of a synthetic italic, in x per y away from the baseline.

   The embedded mono face ships upright ONLY — no oblique cut — so `:italic true`
   on a draw op paints the very same outlines back. An italic run is therefore
   drawn upright on its own layer and sheared about its baseline, which is what a
   terminal without an oblique face does too."
  0.2)

(defn- glyph-op
  "The text draw op for one run.

   The embedded mono face has a SINGLE weight, so `:weight 700` would paint the
   same outlines back: bold STROKES the glyph in its own colour instead, scaled
   with the type size so it holds at every `:font-size`."
  [{:keys [x style text]} py cw ascent size letter-spacing]
  (let [[fg bold] style]
    (cond-> {:op :text
             :text text
             :x (* (long x) (long cw))
             :y (+ (long py) (long ascent))
             :fill (hex-color fg)
             :size size
             :family mono-family
             :letter-spacing letter-spacing}
      bold
      (assoc :stroke
        (hex-color fg) :stroke-width
        (/ (double size) 24.0)))))

(defn- underline-op
  "The rule an UNDERLINE run paints: a bar just under the baseline in the run's
   own ink, spanning its cells — the TERMINAL's underline, not a font feature, so
   a run of underlined blanks is a line too."
  [{:keys [x style text]} py cw ch ascent]
  (let [fg
        (first style)

        t
        (max 1 (long (Math/round (/ (double ch) 14.0))))]

    {:op :rect
     :x (* (long x) (long cw))
     :y (+ (long py) (long ascent) t)
     :w (* (count text) (long cw))
     :h t
     :fill (hex-color fg)}))

(defn- grid->ops
  "Drawing ops for one captured grid, in two layers.

   `:base` is everything upright — background rectangles (runs of equal bg
   merged), box-drawing bars, underline rules, then the glyph runs. `:italic`
   holds the italic runs alone and `:italic-rows` the cell rows they sit on, so
   `paint-grid!` can shear those rows and composite them back."
  [grid cw ch ascent size letter-spacing]
  (reduce
    (fn [acc [y row]]
      (let [py
            (* (long y) (long ch))

            backgrounds
            (for [g
                  (partition-by (comp :bg second) (map-indexed vector row))

                  :let [x
                        (ffirst g)]]

              {:op :rect
               :x (* (long x) (long cw))
               :y py
               :w (* (count g) (long cw))
               :h ch
               :fill (hex-color (:bg (second (first g))))})

            bars
            (mapcat (fn [[x c]]
                      (box-ops c (* (long x) (long cw)) py cw ch))
                    (map-indexed vector row))

            ;; box cells are already drawn as bars — blank them out so their
            ;; glyph cannot be painted on top
            runs
            (text-runs (mapv #(cond-> % (box-arms (:ch %)) (assoc :ch " ")) row))

            rules
            (for [{:keys [style] :as run}
                  runs

                  :when (nth style 3)]

              (underline-op run py cw ch ascent))

            glyphs
            (for [{:keys [style text] :as run}
                  runs

                  :when (pos? (count (str/trim text)))]

              [(nth style 2) (glyph-op run py cw ascent size letter-spacing)])

            italic
            (keep (fn [[italic? op]]
                    (when italic? op))
                  glyphs)]

        (-> acc
            (update :base
                    into
                    (concat backgrounds
                            bars
                            rules
                            (keep (fn [[italic? op]]
                                    (when-not italic? op))
                                  glyphs)))
            (update :italic into italic)
            (cond->
              (seq italic)
              (update :italic-rows conj y)))))
    {:base [] :italic [] :italic-rows #{}}
    (map-indexed vector grid)))

(defn- paint-grid!
  "Paint one captured grid into `im`.

   The upright layer goes straight through imaging's draw ops; the italic layer is
   drawn on a transparent layer, then composited back one raster row at a time,
   each row displaced by `italic-shear` about the baseline. Only the cell rows
   that actually carry italic are touched."
  [im grid cw ch ascent size letter-spacing]
  (let [{:keys [base italic italic-rows]} (grid->ops grid cw ch ascent size letter-spacing)]
    (img/draw! im base)
    (when (seq italic)
      (let [w (img/width im)
            h (img/height im)]

        (with-open [layer (img/blank w h "#00000000")]
          (img/draw! layer italic)
          (doseq [y (sort italic-rows)
                  dy (range (long ch))
                  :let [py (+ (* (long y) (long ch)) (long dy))
                        dx (Math/round (* (double italic-shear)
                                          (double (- (long ascent) (long dy)))))]
                  :when (< py (long h))]

            (with-open [strip (img/crop layer 0 py w 1)]
              (img/paste! im strip (int dx) (int py)))))))))

(defn grid->png!
  "Rasterize ONE captured grid to `out` as a PNG and return the output File.

   The whole \"look at the pixels\" path: a Lanterna back-buffer becomes an image
   in process, in the theme's own colours. Test helper `capture/shot!` wraps this
   into a one-call screenshot."
  ^File [grid out {:keys [font-size] :or {font-size 18}}]
  (let [{:keys [cw ch ascent letter-spacing]}
        (cell-metrics font-size)

        out-file
        (io-file out)]

    (with-open [im (img/blank (* (long (reduce max 0 (map count grid))) (long cw))
                              (* (count grid) (long ch))
                              (hex-color (rgb tui-theme/terminal-bg)))]
      (paint-grid! im grid cw ch ascent font-size (double letter-spacing))
      (img/save! im out-file))
    out-file))
