(ns com.blockether.vis.tui.system-theme
  "The color scheme of the system, for the `auto` theme when the terminal does not
   report its own scheme.

   Each source is the standard of its platform:

   - `COLORFGBG`: the terminal names its foreground and background colors (rxvt,
     Konsole, iTerm2).
   - macOS: the `AppleInterfaceStyle` user default. It exists only in Dark Mode.
   - Linux: the `color-scheme` setting of the XDG Desktop Portal, then of GNOME.
   - Windows, through WSL: the `AppsUseLightTheme` registry value.

   Each read is one short command at the start. Nothing polls."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.external-opener :as opener])
  (:import [java.util.concurrent TimeUnit]))

(def ^:private command-timeout-ms
  "How long one settings command can take. Each answers at once when it exists."
  1000)

(defn command-output
  "The trimmed standard output of `argv`, or nil when the program is missing, fails
   or is slow. Indirected so tests can `with-redefs` it."
  [argv]
  (try (let [child (.start (doto (ProcessBuilder. ^java.util.List argv)
                             (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)))]
         (.close (.getOutputStream child))
         (if (.waitFor child command-timeout-ms TimeUnit/MILLISECONDS)
           (when (zero? (.exitValue child)) (str/trim (slurp (.getInputStream child))))
           (do (.destroyForcibly child) nil)))
       (catch Throwable _ nil)))

(defn colorfgbg-scheme
  "The scheme that a `COLORFGBG` value such as `15;0` or `0;default;15` names, or nil.
   The last field is the background, an ANSI color number: 7 and 9 to 15 are light."
  [value]
  (when-let [field (some->> value
                            str
                            (re-find #";(\d{1,2})\s*$")
                            second)]
    (let [background (Long/parseLong field)]
      (cond (or (<= 0 background 6) (= 8 background)) :dark
            (<= background 15) :light))))

(defn- macos-scheme
  "`AppleInterfaceStyle` is `Dark` in Dark Mode. In the light appearance the key is absent."
  []
  (if (= "Dark" (command-output ["defaults" "read" "-g" "AppleInterfaceStyle"])) :dark :light))

(def ^:private portal-commands
  "Reads of `org.freedesktop.appearance color-scheme`: `ReadOne` of the current portal,
   then `Read` for an older portal or a desktop without GLib."
  [["gdbus" "call" "--session" "--dest" "org.freedesktop.portal.Desktop" "--object-path"
    "/org/freedesktop/portal/desktop" "--method" "org.freedesktop.portal.Settings.ReadOne"
    "org.freedesktop.appearance" "color-scheme"]
   ["dbus-send" "--session" "--print-reply=literal" "--dest=org.freedesktop.portal.Desktop"
    "/org/freedesktop/portal/desktop" "org.freedesktop.portal.Settings.Read"
    "string:org.freedesktop.appearance" "string:color-scheme"]])

(defn- linux-scheme
  "The portal value is 1 for dark, 2 for light and 0 for no preference. GNOME names
   `prefer-dark` in its own setting."
  []
  (if-let [value (some #(some->> (command-output %)
                                 (re-find #"uint32 (\d)")
                                 second)
                       portal-commands)]
    (if (= "1" value) :dark :light)
    (when-let [value (command-output ["gsettings" "get" "org.gnome.desktop.interface"
                                      "color-scheme"])]
      (if (str/includes? value "dark") :dark :light))))

(defn- windows-scheme
  "`AppsUseLightTheme` is `0x0` when the applications of Windows use the dark mode."
  []
  (when-let [value (some->>
                     (command-output
                       ["reg.exe" "query"
                        "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize"
                        "/v" "AppsUseLightTheme"])
                     (re-find #"AppsUseLightTheme\s+REG_DWORD\s+0x(\d+)")
                     second)]
    (if (zero? (Long/parseLong value)) :dark :light)))

(defn- os-scheme
  []
  (let [os (opener/os-name)]
    (cond (str/includes? os "mac") (macos-scheme)
          ;; WSL can run without access to the Windows tools. Its Linux desktop then answers.
          (opener/wsl?) (or (windows-scheme) (linux-scheme))
          (str/includes? os "linux") (linux-scheme))))

(defn detect
  "The color scheme of the system, `:light` or `:dark`, or nil when no source knows it.
   `env` is the process environment. In an SSH session the operating system here is
   not the one that the person looks at, so only `COLORFGBG` counts."
  [env]
  (or (colorfgbg-scheme (get env "COLORFGBG"))
      (when-not (or (seq (get env "SSH_CONNECTION")) (seq (get env "SSH_TTY"))) (os-scheme))))
