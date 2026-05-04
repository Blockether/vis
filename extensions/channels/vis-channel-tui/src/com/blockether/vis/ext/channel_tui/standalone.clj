(ns com.blockether.vis.ext.channel-tui.standalone
  "Standalone Swing backend for the TUI channel.

   This namespace is intentionally isolated from `screen.clj`: the normal
   terminal path does not require it, and the Swing/AWT classes are loaded only
   when the user passes `vis channels tui --standalone`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [com.googlecode.lanterna TerminalPosition TerminalSize]
           [com.googlecode.lanterna.input MouseAction MouseActionType]
           [com.googlecode.lanterna.terminal.swing
            SwingTerminalFontConfiguration
            SwingTerminalFrame
            TerminalEmulatorAutoCloseTrigger
            TerminalEmulatorColorConfiguration
            TerminalEmulatorDeviceConfiguration]
           [java.awt Component Dimension Font Frame GraphicsEnvironment]
           [java.awt.event MouseAdapter MouseEvent MouseWheelEvent]
           [javax.swing SwingUtilities WindowConstants]))

(def default-font-bundle
  "Default standalone font bundle. Mono NF gives terminal-friendly metrics plus
   the broadest Cascadia glyph coverage we bundle."
  :mono-nf)

(def bundled-cascadia-fonts
  "Redistributable Cascadia TTF variants bundled for standalone Swing.

   Cascadia is distributed under the SIL Open Font License 1.1; see
   resources/fonts/cascadia/OFL-1.1-Cascadia.txt. Variants:
   - `:code` / `:code-pl` / `:code-nf` include programming ligatures.
   - `:mono` / `:mono-pl` / `:mono-nf` are terminal-friendly no-ligature faces.
   - `PL` variants include Powerline glyphs; `NF` variants include Nerd Font
     glyphs and are intentionally larger."
  {:code    [{:resource "fonts/cascadia/CascadiaCode.ttf" :style Font/PLAIN}
             {:resource "fonts/cascadia/CascadiaCodeItalic.ttf" :style Font/ITALIC}]
   :code-pl [{:resource "fonts/cascadia/CascadiaCodePL.ttf" :style Font/PLAIN}
             {:resource "fonts/cascadia/CascadiaCodePLItalic.ttf" :style Font/ITALIC}]
   :code-nf [{:resource "fonts/cascadia/CascadiaCodeNF.ttf" :style Font/PLAIN}
             {:resource "fonts/cascadia/CascadiaCodeNFItalic.ttf" :style Font/ITALIC}]
   :mono    [{:resource "fonts/cascadia/CascadiaMono.ttf" :style Font/PLAIN}
             {:resource "fonts/cascadia/CascadiaMonoItalic.ttf" :style Font/ITALIC}]
   :mono-pl [{:resource "fonts/cascadia/CascadiaMonoPL.ttf" :style Font/PLAIN}
             {:resource "fonts/cascadia/CascadiaMonoPLItalic.ttf" :style Font/ITALIC}]
   :mono-nf [{:resource "fonts/cascadia/CascadiaMonoNF.ttf" :style Font/PLAIN}
             {:resource "fonts/cascadia/CascadiaMonoNFItalic.ttf" :style Font/ITALIC}]})

(def bundled-cascadia-resource
  "Default regular face resource for callers that need one concrete bundled
   Cascadia file."
  (-> bundled-cascadia-fonts default-font-bundle first :resource))

(def default-options
  {:title       "Vis"
   :columns     120
   :rows        36
   :font-size    16
   :font-bundle  default-font-bundle
   :pixel-width  1200
   :pixel-height 800
   :maximized    false})

(defn standalone-options
  "Normalize standalone Swing options. This is pure so CLI parsing and tests can
   verify the conditional standalone contract without starting AWT."
  [opts]
  (merge default-options
    (select-keys opts [:title :columns :rows :font-size :font-bundle
                       :pixel-width :pixel-height :maximized])))

(defn- user-error
  [message data]
  (throw (ex-info message (assoc data :vis/user-error true))))

(defn- assert-not-headless!
  []
  (when (GraphicsEnvironment/isHeadless)
    (user-error "--standalone requires a graphical JVM; current environment is headless"
      {:standalone true
       :headless   true})))

(defn- font-array
  [fonts]
  (into-array Font fonts))

(defn- derive-font
  (^Font [^Font font font-size]
   (derive-font font Font/PLAIN font-size))
  (^Font [^Font font style font-size]
   (.deriveFont font (int style) (float font-size))))

(defn- font-bundle-key
  [font-bundle]
  (if (keyword? font-bundle)
    font-bundle
    (some-> font-bundle str/lower-case (str/replace #"_" "-") keyword)))

(defn- font-from-resource!
  ^Font [{:keys [resource style]} font-size]
  (let [res (io/resource resource)]
    (when-not res
      (user-error (str "bundled Cascadia font resource is missing from the classpath: " resource)
        {:resource resource}))
    (with-open [in (io/input-stream res)]
      (derive-font (Font/createFont Font/TRUETYPE_FONT in) style font-size))))

(defn- bundled-fonts
  [font-bundle font-size]
  (let [bundle-key (font-bundle-key font-bundle)
        entries    (get bundled-cascadia-fonts bundle-key)]
    (when-not entries
      (user-error (str "unknown --font-bundle: " (name bundle-key)
                    " (expected one of: "
                    (str/join ", " (map name (sort (keys bundled-cascadia-fonts))))
                    ")")
        {:font-bundle font-bundle}))
    (mapv #(font-from-resource! % font-size) entries)))

(defn- emoji-fallback-fonts
  "Monospaced logical fallback, after bundled Cascadia. Lanterna's Swing font
   configuration accepts only monospaced fonts; the JVM logical `Monospaced`
   face can still resolve platform emoji glyphs while preserving terminal cell
   metrics. Cascadia remains the primary terminal/code face."
  [font-size]
  [(Font. "Monospaced" Font/PLAIN (int font-size))])

(defn- terminal-font-configuration
  [{:keys [font-size font-bundle]}]
  (SwingTerminalFontConfiguration/newInstance
    (font-array (concat (bundled-fonts font-bundle font-size)
                  (emoji-fallback-fonts font-size)))))

(defn- content-component
  ^Component [^SwingTerminalFrame terminal]
  (first (seq (.getComponents (.getContentPane terminal)))))

(defn- mouse-position
  ^TerminalPosition [^SwingTerminalFrame terminal ^Component component ^MouseEvent event]
  (let [size (.getTerminalSize terminal)
        cols (max 1 (.getColumns size))
        rows (max 1 (.getRows size))
        width (max 1 (.getWidth component))
        height (max 1 (.getHeight component))
        col (min (dec cols) (max 0 (long (Math/floor (* cols (/ (.getX event) (double width)))))))
        row (min (dec rows) (max 0 (long (Math/floor (* rows (/ (.getY event) (double height)))))))]
    (TerminalPosition. (int col) (int row))))

(defn- mouse-button
  ^long [^MouseEvent event]
  (case (.getButton event)
    1 1
    2 2
    3 3
    0))

(defn- enqueue-mouse!
  [^SwingTerminalFrame terminal ^Component component action-type button ^MouseEvent event]
  (.addInput terminal
    (MouseAction. action-type (int button) (mouse-position terminal component event))))

(defn- install-mouse-input!
  [^SwingTerminalFrame terminal]
  (when-let [component (content-component terminal)]
    (let [listener (proxy [MouseAdapter] []
                     (mousePressed [event]
                       (enqueue-mouse! terminal component MouseActionType/CLICK_DOWN
                         (mouse-button event) event))
                     (mouseReleased [event]
                       (enqueue-mouse! terminal component MouseActionType/CLICK_RELEASE
                         (mouse-button event) event))
                     (mouseDragged [event]
                       (enqueue-mouse! terminal component MouseActionType/DRAG
                         (mouse-button event) event))
                     (mouseMoved [event]
                       (enqueue-mouse! terminal component MouseActionType/MOVE
                         0 event))
                     (mouseWheelMoved [event]
                       (let [^MouseWheelEvent wheel event]
                         (enqueue-mouse! terminal component
                           (if (neg? (.getWheelRotation wheel))
                             MouseActionType/SCROLL_UP
                             MouseActionType/SCROLL_DOWN)
                           0 event))))]
      (.addMouseListener component listener)
      (.addMouseMotionListener component listener)
      (.addMouseWheelListener component listener))))

(defn create-terminal!
  "Create a visible Lanterna Swing terminal for standalone TUI use.

   Supported opts:
   - `:font-size`   integer point size, default 16
   - `:font-bundle` one of :code, :code-pl, :code-nf, :mono, :mono-pl, :mono-nf
   - `:columns` / `:rows` initial terminal character grid size
   - `:pixel-width` / `:pixel-height` minimum window size, default 1200x800
   - `:maximized` request OS/window-manager maximization on startup

   The Swing terminal is configured with bundled Cascadia first. System emoji
   fonts are appended only as glyph fallback for emoji that Cascadia does not
   contain."
  [opts]
  (let [{:keys [title columns rows pixel-width pixel-height maximized] :as opts'}
        (standalone-options opts)]
    (assert-not-headless!)
    (let [terminal (SwingTerminalFrame.
                     title
                     (TerminalSize. (int columns) (int rows))
                     (TerminalEmulatorDeviceConfiguration.)
                     (terminal-font-configuration opts')
                     (TerminalEmulatorColorConfiguration/getDefault)
                     (into-array TerminalEmulatorAutoCloseTrigger
                       [TerminalEmulatorAutoCloseTrigger/CloseOnExitPrivateMode]))]
      (.setDefaultCloseOperation terminal WindowConstants/DISPOSE_ON_CLOSE)
      (.setResizable terminal true)
      (let [minimum-size (Dimension. (int pixel-width) (int pixel-height))]
        (.setMinimumSize terminal minimum-size)
        (.setSize terminal minimum-size))
      (install-mouse-input! terminal)
      (.setLocationByPlatform terminal true)
      (when maximized
        (.setExtendedState terminal Frame/MAXIMIZED_BOTH))
      (.setVisible terminal true)
      (when maximized
        (SwingUtilities/invokeLater
          (fn []
            (.setExtendedState terminal
              (bit-or (.getExtendedState terminal) Frame/MAXIMIZED_BOTH)))))
      (.requestFocus terminal)
      terminal)))
