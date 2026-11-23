(ns com.blockether.vis.tui.system-theme-test
  "The `auto` theme asks the system for its color scheme when the terminal reports none."
  (:require [com.blockether.vis.tui.external-opener :as opener]
            [com.blockether.vis.tui.screen :as screen]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.shared-theme :as shared-theme]
            [com.blockether.vis.tui.state :as state]
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
  (it "reads the Linux desktop in WSL when Windows gives no answer"
      (expect (= :dark (detect-on "linux" true {} {"gsettings" "'prefer-dark'"}))))
  (it "does not ask the remote system in an SSH session"
      (expect (nil? (detect-on "mac os x" false {"SSH_TTY" "/dev/ttys001"} {"defaults" "Dark"})))
      (expect
        (nil? (detect-on "mac os x" false {"SSH_CLIENT" "10.0.0.1 50000 22"} {"defaults" "Dark"})))
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
                        (finally (t/set-system-color-scheme! :light) (t/apply-theme! before)))))
             (it "follows the system for a stored id that names no theme"
                 (let [before @t/active-theme-id]
                   (try (t/set-system-color-scheme! :dark)
                        (t/apply-theme! :a-theme-that-is-gone)
                        (expect (= "blockether-dark" @t/active-theme-id))
                        (expect (t/follows-system? :a-theme-that-is-gone))
                        (expect (not (t/follows-system? :solarized-light)))
                        (finally (t/set-system-color-scheme! :light) (t/apply-theme! before)))))
             (it "keeps the id auto for the preference: no theme can take it"
                 (try (expect (= :vis.internal.theme/reserved-theme-id
                                 (try (shared-theme/register-theme! "auto" {"MODE" "dark"})
                                      (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
                      (finally (shared-theme/unregister-theme! "auto")))))

(defdescribe the-start-of-the-terminal
             (it
               "does not wait for a slow answer of the operating system, and uses it when it comes"
               (let [late
                     (promise)

                     detect!
                     @#'screen/detect-system-color-scheme!]

                 (with-redefs [system-theme/detect (fn [_env]
                                                     (Thread/sleep 400)
                                                     :dark)]
                   (let [started (System/nanoTime)
                         answer (detect! {} 0 #(deliver late %))]

                     (expect (nil? answer))
                     (expect (< (- (System/nanoTime) started) 200000000))
                     (expect (= :dark (deref late 2000 ::none)))))))
             (it "gives an answer that comes in time"
                 (with-redefs [system-theme/detect (constantly :dark)]
                   (expect (= :dark (@#'screen/detect-system-color-scheme! {} 1000 identity))))))

(defn- poll-on
  "The calls of one poll step with the stored theme `theme-name`: what the step asked
   and what it applied."
  [theme-name terminal-answers? os-scheme]
  (let [calls
        (atom [])

        saved-db
        @state/app-db]

    (try (reset! state/app-db {:settings {:theme-name theme-name}})
         (with-redefs [system-theme/detect (fn [_env]
                                             (swap! calls conj :os)
                                             os-scheme)]
           (@#'screen/poll-color-scheme-once!
            {}
            terminal-answers?
            #(swap! calls conj :terminal)
            #(swap! calls conj [:apply %])))
         @calls
         (finally (reset! state/app-db saved-db)))))

(defdescribe a-terminal-without-color-scheme-reports
             (it "asks the terminal again when it answered at the start"
                 (expect (= [:terminal] (poll-on :auto true :dark))))
             (it "asks the operating system, and applies only a change"
                 (try (t/set-system-color-scheme! :light)
                      (expect (= [:os [:apply :dark]] (poll-on :auto false :dark)))
                      (t/set-system-color-scheme! :dark)
                      (expect (= [:os] (poll-on :auto false :dark)))
                      (expect (= [:os] (poll-on :auto false nil)))
                      (finally (t/set-system-color-scheme! :light))))
             (it "asks nothing for an explicit theme"
                 (expect (= [] (poll-on :vis-light true :dark)))
                 (expect (= [] (poll-on :vis-light false :dark))))
             (it "leaves the background of a terminal that it asks again to the terminal"
                 (let [written (fn [theme-name]
                                 (let [out (java.io.ByteArrayOutputStream.)]
                                   (with-redefs [vis/tty-out (delay out)]
                                     (state/sync-terminal-background! theme-name))
                                   (.toString out "UTF-8")))]
                   (try (reset! state/terminal-background-polled? true)
                        (expect (= "\u001B]111\u0007\u001B]112\u0007" (written :auto)))
                        (expect (re-find #"^\u001B]11;rgb:" (written :vis-light)))
                        (finally (reset! state/terminal-background-polled? false)))))
             (it "gives the cursor the text color of the theme"
                 (let [saved
                       @t/active-theme-id

                       out
                       (java.io.ByteArrayOutputStream.)]

                   (try (t/apply-theme! :vis-dark)
                        (with-redefs [vis/tty-out (delay out)]
                          (state/sync-terminal-background! :vis-dark))
                        (let [^com.googlecode.lanterna.TextColor$RGB fg t/text-fg]
                          (expect (.contains (.toString out "UTF-8")
                                             (format "\u001B]12;rgb:%02x/%02x/%02x\u0007"
                                                     (.getRed fg)
                                                     (.getGreen fg)
                                                     (.getBlue fg)))))
                        (finally (t/apply-theme! saved))))))
