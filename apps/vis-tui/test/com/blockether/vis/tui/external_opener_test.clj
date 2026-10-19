(ns com.blockether.vis.tui.external-opener-test
  "The detached OS opener needs neither a process library nor terminal IO."
  (:require [com.blockether.vis.tui.external-opener :as opener]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defdescribe
  detached-opener-test
  (it "reports an absent executable without throwing"
      (expect (instance? java.io.IOException (#'opener/spawn! ["/vis-missing-opener/executable"]))))
  (it "reports an early non-zero exit as a failure"
      ;; Regression for #327: `xdg-open` starts on WSL, then exits 3 because it
      ;; finds no browser. Only the exit code shows that nothing opened.
      (expect (= 3 (:exit (ex-data (#'opener/spawn! ["sh" "-c" "exit 3"]))))))
  (it "passes to the next opener and keeps the Windows host first on WSL"
      (with-redefs [opener/open-commands (constantly [["sh" "-c" "exit 3"] ["sh" "-c" "exit 0"]])]
        (expect (= {:status :ok :command ["sh" "-c" "exit 0"]}
                   (select-keys (opener/open! "https://example.com") [:status :command]))))
      (with-redefs [opener/os-name
                    (constantly "linux")

                    opener/wsl?
                    (constantly true)]

        (expect (= [["wslview" "https://example.com"] ["explorer.exe" "https://example.com"]]
                   (take 2 (opener/open-commands "https://example.com"))))))
  (it "closes stdin and discards both output streams"
      (let [path
            (Files/createTempFile "vis-opener-" ".done" (make-array FileAttribute 0))

            task
            (promise)]

        (Files/delete path)
        (try (expect (nil? (#'opener/spawn!
                            ["sh" "-c"
                             "cat >/dev/null; printf output; printf error >&2; printf done > \"$1\""
                             "opener-test" (str path)])))
             ;; The marker is written only after stdin reaches EOF. Bound a broken
             ;; implementation without depending on a fixed scheduling delay.
             (let [deadline (+ (System/nanoTime) 5000000000)]
               (loop []

                 (cond (Files/exists path (make-array java.nio.file.LinkOption 0)) (deliver task
                                                                                            true)
                       (< (System/nanoTime) deadline) (do (Thread/sleep 10) (recur))
                       :else (deliver task false))))
             (expect @task)
             (finally (Files/deleteIfExists path))))))

;; Regression: Open on a ZIP artifact unpacked it in a hidden temp folder and
;; never showed the file. Each host shows the file in its own file manager.
(defdescribe
  reveal-commands-test
  (it "selects the file in Finder on macOS"
      (with-redefs [opener/os-name (constantly "mac os x")]
        (expect (= [["open" "-R" "/tmp/a b/bundle.zip"]]
                   (opener/reveal-commands "/tmp/a b/bundle.zip")))))
  (it "asks the freedesktop file manager on Linux, then opens the folder"
      (with-redefs [opener/os-name
                    (constantly "linux")

                    opener/wsl?
                    (constantly false)]

        (let [[show & folder] (opener/reveal-commands "/home/u/Pobrane/a,b.zip")]
          (expect (= "dbus-send" (first show)))
          (expect (= "org.freedesktop.FileManager1.ShowItems" (nth show 6)))
          (expect (= "array:string:file:///home/u/Pobrane/a%2Cb.zip" (nth show 7)))
          (expect (= ["xdg-open" "/home/u/Pobrane"] (first folder))))))
  (it "selects the file in Explorer on WSL, through its Windows path"
      (with-redefs [opener/os-name
                    (constantly "linux")

                    opener/wsl?
                    (constantly true)

                    opener/windows-path
                    (constantly "\\\\wsl.localhost\\Ubuntu\\tmp\\bundle.zip")]

        (let [commands (opener/reveal-commands "/tmp/bundle.zip")]
          (expect (= ["explorer.exe" "/select,\\\\wsl.localhost\\Ubuntu\\tmp\\bundle.zip"]
                     (first commands)))
          (expect (= "dbus-send" (first (second commands))))
          (expect (some #{["wslview" "/tmp"]} commands)))))
  (it "keeps the Linux chain on WSL when wslpath gives no Windows path"
      (with-redefs [opener/os-name
                    (constantly "linux")

                    opener/wsl?
                    (constantly true)

                    opener/windows-path
                    (constantly nil)]

        (expect (= "dbus-send" (ffirst (opener/reveal-commands "/tmp/bundle.zip"))))))
  (it "refuses a path that is not a file"
      (expect (= :rejected-scheme (:status (opener/reveal-local! "/vis-missing/bundle.zip"))))))
