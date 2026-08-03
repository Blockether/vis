(ns com.blockether.vis.ext.channel-tui.capture
  "PIXEL-TRUTH capture of a REAL TUI paint: run any dialog/screen fn against a
   Lanterna `DefaultVirtualTerminal`, snapshot the back-buffer on every flush, and
   hand the frames over as data a rasterizer can turn into PNGs.

   This is the ONLY sanctioned way to look at what the TUI actually paints. The
   Blockether lanterna fork ships NO Swing/AWT emulator (see `deps.edn` patch 15),
   so nothing can open a window or draw itself into an image; a virtual terminal
   plus this capture is the substitute, and it is strictly better for review —
   every cell carries the character, its fg/bg, and its bold flag exactly as the
   terminal held it, with no font/theme guesswork in between.

   Typical use from a REPL (see AGENTS.md → TUI rendering):

     (capture-json! \"/tmp/vis-frames.json\"
                    {:cols 120 :rows 40 :keys [\\c :esc :esc]
                     :paint! (fn [{:keys [screen]}] (dlg/magit-dialog! screen root))})

   then rasterize + attach the PNG with `tools/tui_png.py`.

   `:keys` are fed to the terminal BEFORE `:paint!` runs, so a dialog that owns
   its own key loop walks through the states you want to see and then closes.
   Lives on the TEST path on purpose: it is developer tooling, never runtime code."
  (:require [clojure.string :as str])
  (:import [com.googlecode.lanterna SGR TerminalPosition TerminalSize TextCharacter TextColor]
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

(defn- hex
  [^TextColor c]
  (if c
    (format "#%02x%02x%02x"
            (bit-and (int (.getRed c)) 0xff)
            (bit-and (int (.getGreen c)) 0xff)
            (bit-and (int (.getBlue c)) 0xff))
    "#000000"))

(defn grab
  "The terminal's WHOLE back buffer as `[row [cell …]]`, cell = `[char fg bg bold?]`."
  [^DefaultVirtualTerminal terminal ^long cols ^long rows]
  (vec (for [y (range rows)]
         (vec (for [x (range cols)]
                (let
                  [^TextCharacter ch (.getCharacter terminal (TerminalPosition. (int x) (int y)))]
                  [(if ch (.getCharacterString ch) " ") (hex (when ch (.getForegroundColor ch)))
                   (hex (when ch (.getBackgroundColor ch)))
                   (boolean (and ch (contains? (set (.getModifiers ch)) SGR/BOLD)))]))))))

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
                   (str/trimr (apply str (map first row))))
                 frame)))

(defn- json-str
  [^String s]
  (str \"
       (-> s
           (str/replace "\\" "\\\\")
           (str/replace "\"" "\\\"")
           (str/replace "\n" "\\n")
           (str/replace "\t" "\\t"))
       \"))

(defn write-json!
  "Write a `capture!` result to `path` as
   `{\"cols\":n,\"rows\":n,\"frames\":[[[[char,fg,bg,bold], …], …], …]}` — the shape
   `tools/tui_png.py` rasterizes. Returns `path`."
  [path {:keys [cols rows frames]}]
  (spit path
        (str "{\"cols\":"
             cols
             ",\"rows\":"
             rows
             ",\"frames\":["
             (str/join ","
                       (for [frame frames]
                         (str "["
                              (str/join ","
                                        (for [row frame]
                                          (str "["
                                               (str/join ","
                                                         (for [[ch fg bg bold?] row]
                                                           (str "["
                                                                (json-str ch)
                                                                ","
                                                                (json-str fg)
                                                                ","
                                                                (json-str bg)
                                                                ","
                                                                (if bold? "true" "false")
                                                                "]")))
                                               "]")))
                              "]")))
             "]}"))
  path)

(defn capture-json!
  "`capture!` + `write-json!` in one call. Returns the capture (its `:ret` is the
   dialog's own return value), so the frame count and result are inspectable."
  [path opts]
  (doto (capture! opts)
    (->> (write-json! path))))
