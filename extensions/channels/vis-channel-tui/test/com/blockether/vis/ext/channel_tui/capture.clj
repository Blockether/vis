(ns com.blockether.vis.ext.channel-tui.capture
  "PIXEL-TRUTH pictures of a REAL TUI paint, in one call.

   Run any dialog/screen fn against a Lanterna `DefaultVirtualTerminal`, snapshot
   the back-buffer, and rasterize it with the SAME renderer the MP4 screencast
   uses — bold, italic, underline, colours and box rules exactly as the terminal
   would show them.

     (require '[com.blockether.vis.ext.channel-tui.capture :as cap])

     (cap/shot! {:paint! (fn [{:keys [screen]}] (dlg/magit-dialog! screen root))})
     ;; => \"/tmp/vis-tui/shot.png\"

   That string is a PATH: attach it with `attach` and LOOK at it. Three more
   calls cover everything else:

     (cap/frame-text (cap/capture! {:paint! …}))   ; greppable text of the paint
     (cap/shots! {:paint! … :out \"magit\"})         ; one PNG per flush
     (cap/ink \"/tmp/vis-tui/shot.png\")             ; non-paper pixels, for asserts

   Keys are fed to the terminal BEFORE `:paint!` runs, so a dialog that owns its
   own key loop walks through the states you want to see and then closes:

     (cap/shot! {:keys [\\c :esc] :frame 1 :paint! …})

   The Blockether lanterna fork ships NO Swing/AWT emulator (see `deps.edn` patch
   15), so nothing can open a window or draw itself into an image; a virtual
   terminal plus this capture is the substitute, and it is strictly better for
   review. Lives on the TEST path on purpose: it is developer tooling, never
   runtime code."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.imaging :as img]
            [com.blockether.vis.ext.channel-tui.cinema :as cinema])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal VirtualTerminalListener]
           [java.io File]))

(def ^:private key-types
  "Keyword → Lanterna `KeyType` for every key a captured paint may need."
  {:esc KeyType/Escape
   :escape KeyType/Escape
   :enter KeyType/Enter
   :tab KeyType/Tab
   :reverse-tab KeyType/ReverseTab
   :backspace KeyType/Backspace
   :delete KeyType/Delete
   :insert KeyType/Insert
   :home KeyType/Home
   :end KeyType/End
   :page-up KeyType/PageUp
   :page-down KeyType/PageDown
   :up KeyType/ArrowUp
   :down KeyType/ArrowDown
   :left KeyType/ArrowLeft
   :right KeyType/ArrowRight
   :f1 KeyType/F1
   :f2 KeyType/F2
   :f3 KeyType/F3
   :f4 KeyType/F4})

(defn key-stroke
  "`\\c` → that character key, a keyword → the named key (`:esc` `:enter` `:tab`
   `:up` `:down` `:left` `:right` `:home` `:end` `:page-up` …). An unknown keyword
   says so instead of throwing a `NullPointerException` from inside Lanterna."
  ^KeyStroke [k]
  (cond (instance? KeyStroke k) k
        (keyword? k) (if-let [t (key-types k)]
                       (KeyStroke. t)
                       (throw (ex-info (str "capture: unknown key " k)
                                       {:key k :known (vec (sort (keys key-types)))})))
        (char? k) (KeyStroke. (Character/valueOf (char k)) false false false)
        :else (throw (ex-info (str "capture: not a key: " (pr-str k)) {:key k}))))

(defn- grab
  "The terminal's WHOLE back buffer as a rows×cols vector of `cinema/cell` maps."
  [^DefaultVirtualTerminal terminal ^long cols ^long rows]
  (mapv (fn [y]
          (mapv (fn [x]
                  (cinema/cell (.getCharacter terminal (TerminalPosition. (int x) (int y)))))
                (range cols)))
        (range rows)))

(defn capture!
  "Run `:paint!` on a virtual `TerminalScreen` and return
   `{:cols :rows :frames [frame …] :ret v :error t}`.

   ONE frame per terminal flush (i.e. per `screen.refresh`), so a dialog's every
   repaint is inspectable — and ALWAYS at least one CARRYING THE DRAWING: a paint
   that never flushed is flushed here, because a `TextGraphics` write lands in the
   screen's back buffer and the terminal would otherwise hand back blank paper.

   `:paint!` is called with `{:terminal :screen :g}`; its value comes back as
   `:ret`. A throw is caught, reported as `:error`, and the half-painted frames
   are kept — `shot!` writes the picture and then re-throws, so a broken paint is
   both visible and impossible to pass a test with.

   Options: `:cols` (120) `:rows` (40) `:keys` (queued before `:paint!` runs)."
  [{:keys [cols rows keys paint!] :or {cols 120 rows 40}}]
  (let
    [cols
     (long cols)

     rows
     (long rows)

     terminal
     (DefaultVirtualTerminal. (TerminalSize. (int cols) (int rows)))

     ^TerminalScreen screen
     (doto (TerminalScreen. terminal) (.startScreen))

     frames
     (atom [])]

    (.addVirtualTerminalListener terminal
                                 (reify
                                   VirtualTerminalListener
                                     (onFlush [_] (swap! frames conj (grab terminal cols rows)))
                                     (onBell [_])
                                     (onClose [_])
                                     (onResized [_ _terminal _size])))
    (doseq [k keys]
      (.addInput terminal (key-stroke k)))
    (let
      [error
       (atom nil)

       ret
       (try (paint! {:terminal terminal :screen screen :g (.newTextGraphics screen)})
            (catch Throwable t (reset! error t) nil))]

      ;; A paint that never refreshed has left its drawing in the SCREEN's back
      ;; buffer, where the terminal cannot see it -- so flush it here. Without
      ;; this the guaranteed frame is blank paper and every state photographs to
      ;; the same empty picture.
      (when (empty? @frames) (try (.refresh screen) (catch Throwable _ nil)))
      {:cols cols
       :rows rows
       :frames (if (seq @frames) @frames [(grab terminal cols rows)])
       :ret ret
       :error @error})))

(defn- pick-frame
  "The one frame `which` names: an index, `:first`, or `:last` (the default) —
   the last flush is what the user would be looking at."
  [frames which]
  (let [n (count frames)]
    (when (pos? n)
      (nth frames
           (cond (number? which) (min (dec n) (max 0 (long which)))
                 (= :first which) 0
                 :else (dec n))))))

(defn frame-text
  "A captured frame as plain text, one line per terminal row — the quick,
   greppable check before rasterizing anything. Takes a frame OR a whole
   `capture!` result (then its last frame, or the one named by `which`)."
  ([x] (frame-text x :last))
  ([x which]
   (let [frame (if (map? x) (pick-frame (:frames x) which) x)]
     (str/join "\n"
               (map (fn [row]
                      (str/trimr (apply str (map :ch row))))
                    frame)))))

(defn- blank-cell?
  "True for a cell showing nothing but the terminal's own paper."
  [paper c]
  (and (some? c) (str/blank? (or (:ch c) " ")) (= paper (:bg c))))

(defn- trim-grid
  "Crop the blank margins off a captured grid, keeping ONE cell of padding, so a
   small dialog on a 120×40 terminal is photographed as the dialog and not as a
   stamp in an ocean of paper. A grid with nothing on it is returned untouched."
  [grid]
  (let
    [paper
     (:bg (cinema/cell nil))

     blank?
     (partial blank-cell? paper)

     used-rows
     (keep-indexed (fn [y row]
                     (when-not (every? blank? row) y))
                   grid)

     width
     (reduce max 0 (map count grid))

     used-cols
     (remove (fn [x]
               (every? (fn [row]
                         (blank? (nth row x nil)))
                       grid))
       (range width))]

    (if (or (empty? used-rows) (empty? used-cols))
      grid
      (let
        [y0
         (max 0 (dec (long (first used-rows))))

         y1
         (min (dec (count grid)) (inc (long (last used-rows))))

         x0
         (max 0 (dec (long (first used-cols))))

         x1
         (min (dec (long width)) (inc (long (last used-cols))))]

        (mapv (fn [row]
                (subvec (vec row) x0 (min (count row) (inc x1))))
              (subvec (vec grid) y0 (inc y1)))))))

(defn- out-file
  "Resolve `:out` to a PNG File. A bare name lands under `<tmp>/vis-tui`, an
   absolute path is taken as given, `.png` is optional, and `i` numbers a frame."
  ^File [out i]
  (let
    [base
     (str/replace (str (or out "shot")) #"\.png$" "")

     named
     (str base (when i (str "-" i)) ".png")

     f
     (io/file named)

     f
     (if (.isAbsolute f) f (io/file (System/getProperty "java.io.tmpdir") "vis-tui" named))]

    (io/make-parents f)
    f))

(defn- render!
  ^String [grid out i {:keys [font-size trim] :or {font-size 18 trim true}}]
  (str (cinema/grid->png! (if trim (trim-grid grid) grid) (out-file out i) {:font-size font-size})))

(defn shot!
  "ONE picture of a real paint, and its PATH back.

     (cap/shot! {:paint! (fn [{:keys [screen]}] (dlg/magit-dialog! screen root))})
     ;; => \"/tmp/vis-tui/shot.png\"

   Everything `capture!` takes, plus:

     :out       file name or path (default `shot`); a bare name lands under
                `<tmpdir>/vis-tui`, `.png` is optional
     :frame     which flush to draw — an index, `:first`, or `:last` (default)
     :grid      draw THIS captured grid and run no paint at all
     :font-size type size in px (default 18)
     :trim      crop the blank margins, keeping one cell of padding (default true)

   A paint that threw still writes its picture, then re-throws with `:png` in the
   ex-data: you get to see how far it got, and the test still fails."
  ^String [{:keys [out frame grid] :as opts}]
  (if grid
    (render! grid out nil opts)
    (let
      [{:keys [frames error]}
       (capture! opts)

       png
       (render! (pick-frame frames frame) out nil opts)]

      (when error
        (throw (ex-info (str "capture: :paint! threw — picture written to " png) {:png png} error)))
      png)))

(defn shots!
  "Every flush of ONE capture as its own PNG — `[path …]` in frame order, named
   `<out>-<i>.png`. `:frames` picks which flushes to draw (default: all of them);
   every other option is `shot!`'s."
  [{:keys [out frames] :as opts}]
  (let
    [{captured :frames :keys [error]}
     (capture! opts)

     pngs
     (mapv (fn [i]
             (render! (nth captured i) out i opts))
           (or frames (range (count captured))))]

    (when error
      (throw (ex-info (str "capture: :paint! threw — pictures written to " (pr-str pngs))
                      {:pngs pngs}
                      error)))
    pngs))

(defn png-rows
  "A PNG on disk as raster rows of `[r g b]` triples — the primitive every pixel
   assertion is built from (imaging packs one pixel as 0xRRGGBBAA).

   `(get-in rows [y x])` is one pixel, `(set (apply concat rows))` is the palette
   the render actually used, and one row is the ink profile of a rule."
  [path]
  (with-open [im (img/decode (io/file (str path)))]
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [p (long (img/get-pixel im (int x) (int y)))]
                      [(bit-and (bit-shift-right p 24) 0xff) (bit-and (bit-shift-right p 16) 0xff)
                       (bit-and (bit-shift-right p 8) 0xff)]))
                  (range (img/width im))))
          (range (img/height im)))))

(defn ink
  "How many pixels are NOT paper: the dominant colour of the raster is the
   background, everything else is ink. The blunt, reliable way to assert that one
   render is heavier than another — bold, an underline rule, a slanted italic.

   Takes a PNG path or rows from `png-rows`, so `(ink (subvec rows 0 25))` weighs
   a single terminal row.

   Pass `paper` when the dominant colour is the wrong answer: a line carrying a
   coloured band is mostly BAND, and `(ink rows (set (first rows)))` is the honest
   count, since the raster's top row sits above every glyph and can only hold
   backgrounds and full-height bars."
  ([path-or-rows] (ink path-or-rows nil))
  ([path-or-rows paper]
   (let
     [rows
      (if (sequential? path-or-rows) path-or-rows (png-rows path-or-rows))

      pixels
      (apply concat rows)

      background
      (cond (set? paper) paper
            (some? paper) (set paper)
            (seq pixels) #{(key (apply max-key val (frequencies pixels)))}
            :else #{})]

     (count (remove background pixels)))))
