(ns com.blockether.vis.ext.channel-tui.standalone
  "Standalone Swing backend for the TUI channel.

   This namespace is intentionally isolated from `screen.clj`: the normal
   terminal path does not require it, and the Swing/AWT classes are loaded only
   when the user passes `vis channels tui --standalone`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [com.googlecode.lanterna TerminalSize]
           [com.googlecode.lanterna.terminal.swing
            AWTTerminalFontConfiguration
            SwingTerminalFrame
            TerminalEmulatorAutoCloseTrigger
            TerminalEmulatorColorConfiguration
            TerminalEmulatorDeviceConfiguration]
           [java.awt Font GraphicsEnvironment]
           [java.io File]
           [javax.swing WindowConstants]))

(def default-font-bundle
  "Default standalone font bundle. Mono PL gives terminal-friendly metrics plus
   Powerline glyphs without the much larger Nerd Font payload."
  :mono-pl)

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
   :font-size   16
   :font-bundle default-font-bundle
   :font-family nil
   :font-path   nil})

(defn standalone-options
  "Normalize standalone Swing options. This is pure so CLI parsing and tests can
   verify the conditional standalone contract without starting AWT."
  [opts]
  (merge default-options
    (select-keys opts [:title :columns :rows :font-size :font-bundle :font-family :font-path])))

(defn- user-error
  [message data]
  (throw (ex-info message (assoc data :vis/user-error true))))

(defn- assert-not-headless!
  []
  (when (GraphicsEnvironment/isHeadless)
    (user-error "--standalone requires a graphical JVM; current environment is headless"
      {:standalone true
       :headless   true})))

(defn- clean-family-name
  [s]
  (some-> s
    str/trim
    (str/replace #"^['\"]|['\"]$" "")))

(defn- family-names
  [family]
  (->> (str/split (or family "") #",")
    (map clean-family-name)
    (remove str/blank?)
    vec))

(defn- font-array
  [fonts]
  (into-array Font fonts))

(defn- derive-font
  (^Font [^Font font font-size]
   (derive-font font Font/PLAIN font-size))
  (^Font [^Font font style font-size]
   (.deriveFont font (int style) (float font-size))))

(defn- font-from-file
  ^Font [path font-size]
  (let [file (io/file path)]
    (when-not (.isFile ^File file)
      (user-error (str "--font-path does not point to a file: " path)
        {:font-path path}))
    (derive-font (Font/createFont Font/TRUETYPE_FONT file) font-size)))

(defn- font-bundle-key
  [font-bundle]
  (if (keyword? font-bundle)
    font-bundle
    (some-> font-bundle str/lower-case (str/replace #"_" "-") keyword)))

(defn- font-from-resource
  ^Font [{:keys [resource style]} font-size]
  (when-let [res (io/resource resource)]
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
    (->> entries
      (keep #(font-from-resource % font-size))
      vec)))

(defn- system-fonts
  [family font-size]
  (let [families (or (seq (family-names family)) ["Cascadia Mono PL" "Cascadia Mono" "Monospaced"])]
    (mapv #(Font. ^String % Font/PLAIN (int font-size)) families)))

(defn- terminal-font-configuration
  [{:keys [font-path font-family font-size font-bundle]}]
  (AWTTerminalFontConfiguration/newInstance
    (font-array
      (cond
        (seq font-path)
        [(font-from-file font-path font-size)]

        (seq font-family)
        (system-fonts font-family font-size)

        :else
        (let [fonts (bundled-fonts font-bundle font-size)]
          (if (seq fonts)
            fonts
            (system-fonts nil font-size)))))))

(defn create-terminal!
  "Create a visible Lanterna Swing terminal for standalone TUI use.

   Supported opts:
   - `:font-size`   integer point size, default 16
   - `:font-bundle` one of :code, :code-pl, :code-nf, :mono, :mono-pl, :mono-nf
   - `:font-family` comma-separated system font stack
   - `:font-path`   path to a local TTF/OTF file
   - `:columns` / `:rows` initial terminal character grid size"
  [opts]
  (let [{:keys [title columns rows] :as opts'} (standalone-options opts)]
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
      (.setLocationByPlatform terminal true)
      (.setVisible terminal true)
      (.requestFocus terminal)
      terminal)))
