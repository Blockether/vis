(ns com.blockether.vis.tui.system-theme-test
  "The `auto` theme asks the system for its color scheme when the terminal reports none."
  (:require [com.blockether.vis.tui.external-opener :as opener]
            [com.blockether.vis.tui.system-theme :as system-theme]
            [com.blockether.vis.tui.theme :as t]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- detect-on
  "The detected scheme on the system `os`, where each command prefix in `answers` prints its value."
  [os wsl? env answers]
  (with-redefs [opener/os-name
                (constantly os)

                opener/wsl?
                (constantly wsl?)

                system-theme/command-output
                (fn [argv]
                  (some (fn [[prefix out]]
                          (when (= prefix (first argv)) out))
                        answers))]

    (system-theme/detect env)))

(defdescribe
  the-color-scheme-of-the-system
  (it "reads the background color that the terminal names in COLORFGBG"
      (expect (= :dark (system-theme/colorfgbg-scheme "15;0")))
      (expect (= :light (system-theme/colorfgbg-scheme "0;15")))
      (expect (= :light (system-theme/colorfgbg-scheme "0;default;7")))
      (expect (nil? (system-theme/colorfgbg-scheme "0;default")))
      (expect (nil? (system-theme/colorfgbg-scheme nil))))
  (it "reads the appearance of macOS, where the key exists only in Dark Mode"
      (expect (= :dark (detect-on "mac os x" false {} {"defaults" "Dark"})))
      (expect (= :light (detect-on "mac os x" false {} {}))))
  (it "reads the desktop portal of Linux, then the GNOME setting"
      (expect (= :dark (detect-on "linux" false {} {"gdbus" "(<uint32 1>,)"})))
      (expect (= :light (detect-on "linux" false {} {"gdbus" "(<uint32 2>,)"})))
      (expect
        (= :dark
           (detect-on "linux" false {} {"dbus-send" "variant       variant          uint32 1"})))
      (expect (= :dark (detect-on "linux" false {} {"gsettings" "'prefer-dark'"})))
      (expect (nil? (detect-on "linux" false {} {}))))
  (it "reads the application mode of Windows from WSL"
      (expect
        (= :dark
           (detect-on "linux" true {} {"reg.exe" "    AppsUseLightTheme    REG_DWORD    0x0"})))
      (expect
        (= :light
           (detect-on "linux" true {} {"reg.exe" "    AppsUseLightTheme    REG_DWORD    0x1"}))))
  (it "does not ask the remote system in an SSH session"
      (expect (nil? (detect-on "mac os x" false {"SSH_TTY" "/dev/ttys001"} {"defaults" "Dark"})))
      (expect (= :dark
                 (detect-on "mac os x" false {"SSH_TTY" "/dev/ttys001" "COLORFGBG" "15;0"} {})))))

(defdescribe the-auto-theme
             (it "applies the theme for the color scheme of the system, and an explicit theme stays"
                 (let [before @t/active-theme-id]
                   (try (t/set-system-color-scheme! :dark)
                        (t/apply-theme! :auto)
                        (expect (= "blockether-dark" @t/active-theme-id))
                        (t/set-system-color-scheme! :light)
                        (t/apply-theme! :auto)
                        (expect (= "blockether-light" @t/active-theme-id))
                        (t/set-system-color-scheme! :dark)
                        (t/apply-theme! :solarized-light)
                        (expect (= "solarized-light" @t/active-theme-id))
                        (finally (t/set-system-color-scheme! :light) (t/apply-theme! before))))))
