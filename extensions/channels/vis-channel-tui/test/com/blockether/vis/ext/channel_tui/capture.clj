(ns com.blockether.vis.ext.channel-tui.capture
  "PIXEL-TRUTH capture of a REAL TUI paint: run any dialog/screen fn against a
   Lanterna `DefaultVirtualTerminal`, snapshot the back-buffer on every flush, and
   rasterize the frames worth looking at straight to PNG.

   This is the ONLY sanctioned way to look at what the TUI actually paints. The
   Blockether lanterna fork ships NO Swing/AWT emulator (see `deps.edn` patch 15),
   so nothing can open a window or draw itself into an image; a virtual terminal
   plus this capture is the substitute, and it is strictly better for review —
   every cell carries the character, its fg/bg and its bold flag exactly as the
   terminal held it, and `cinema/grid->png!` paints those cells with the SAME
   rasterizer the MP4 screencast uses, in the theme's own colours.

   Typical use from a REPL (see AGENTS.md → TUI rendering):

     (capture-png! \\\"/tmp/vis-magit\\\"
                   {:cols 120 :rows 40 :keys [\\\\c :esc :esc] :indexes [1]
                    :paint! (fn [{:keys [screen]}] (dlg/magit-dialog! screen root))})

   then attach the returned `:pngs` so both the human and the model see the real
   frame. `cap/frame-text` is the quick greppable check before rasterizing.

   `:keys` are fed to the terminal BEFORE `:paint!` runs, so a dialog that owns
   its own key loop walks through the states you want to see and then closes.
   Lives on the TEST path on purpose: it is developer tooling, never runtime code."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.imaging :as img]
            [com.blockether.vis.ext.channel-tui.cinema :as cinema])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input KeyStroke KeyType]
           [com.googlecode.lanterna.screen TerminalScreen]
           [com.googlecode.lanterna.terminal.virtual DefaultVirtualTerminal
            VirtualTerminalListener]))

(defn key-stroke
  "`\\c` → the character key, a keyword → the named key (`:esc` `:enter` `:tab`
   `:up` `:down` `:backspace`)."
  ^KeyStroke [k]
  (if (keyword? k)
    (KeyStroke. (case k
                  :esc
                  KeyType/Escape

                  :enter
                  KeyType/Enter

                  :tab
                  KeyType/Tab

                  :up
                  KeyType/ArrowUp

                  :down
                  KeyType/ArrowDown

                  :backspace
                  KeyType/Backspace))
    (KeyStroke. (Character/valueOf (char k)) false false false)))

(defn grab
  "The terminal's WHOLE back buffer as a rows×cols vector of `cinema/cell` maps."
  [^DefaultVirtualTerminal terminal ^long cols ^long rows]
  (mapv (fn [y]
          (mapv (fn [x]
                  (cinema/cell (.getCharacter terminal (TerminalPosition. (int x) (int y)))))
                (range cols)))
        (range rows)))

(defn capture!
  "Run `:paint!` on a virtual `TerminalScreen` and return
   `{:cols :rows :frames [frame …] :ret v}` — ONE frame per terminal flush (i.e.
   per `screen.refresh`), so a dialog's every repaint is inspectable.

   `:paint!` is called with `{:terminal :screen :g}`; its value is returned as
   `:ret`, and a throw is captured as `:ret` too so a half-painted frame is still
   available. `:keys` (characters / `:esc` …) are queued before it runs."
  [{:keys [cols rows keys paint!] :or {cols 120 rows 40}}]
  (let
    [cols
     (long cols)

     rows
     (long rows)

     terminal
     (DefaultVirtualTerminal. (TerminalSize. (int cols) (int rows)))

     screen
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
      [ret (try (paint! {:terminal terminal :screen screen :g (.newTextGraphics screen)})
                (catch Throwable t (str t)))]
      {:cols cols :rows rows :frames @frames :ret ret})))

(defn frame-text
  "One captured frame as plain text, one line per terminal row — the quick check
   before rasterizing anything."
  [frame]
  (str/join "\n"
            (map (fn [row]
                   (str/trimr (apply str (map :ch row))))
                 frame)))

(defn capture-png!
  "`capture!`, then rasterize frames to `<out-prefix>-<i>.png`. Returns the capture
   with `:pngs` (the written paths, in `:indexes` order) added.

   `:indexes` picks which flushes to draw (default: all of them); `:font-size`
   sizes the glyphs, and with them the PNG."
  [out-prefix {:keys [indexes font-size] :or {font-size 18} :as opts}]
  (let [{:keys [frames] :as capture} (capture! opts)]
    (assoc capture
      :pngs (mapv (fn [i]
                    (str (cinema/grid->png! (nth frames i)
                                            (str out-prefix "-" i ".png")
                                            {:font-size font-size})))
                  (or indexes (range (count frames)))))))

(defn png-rows
  "A PNG on disk as raster rows of `[r g b]` triples — the primitive every pixel
   assertion is built from (imaging packs one pixel as 0xRRGGBBAA).

   `(get-in rows [y x])` is one pixel, `(set (apply concat rows))` is the palette
   the render actually used, and one row is the ink profile of a rule."
  [path]
  (with-open [im (img/decode (io/file path))]
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [p (long (img/get-pixel im (int x) (int y)))]
                      [(bit-and (bit-shift-right p 24) 0xff) (bit-and (bit-shift-right p 16) 0xff)
                       (bit-and (bit-shift-right p 8) 0xff)]))
                  (range (img/width im))))
          (range (img/height im)))))
