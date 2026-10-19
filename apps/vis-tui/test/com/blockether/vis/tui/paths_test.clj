(ns com.blockether.vis.tui.paths-test
  (:require [com.blockether.vis.tui.paths :as paths]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  downloads-dir-test
  (it "reads the download folder that the Linux desktop names"
      (expect (= "/home/u/Pobrane"
                 (paths/downloads-dir "/home/u"
                                      (str "# written by xdg-user-dirs-update\n"
                                           "XDG_DESKTOP_DIR=\"$HOME/Pulpit\"\n"
                                           "XDG_DOWNLOAD_DIR=\"$HOME/Pobrane\"\n"))))
      (expect (= "/data/dl" (paths/downloads-dir "/home/u" "XDG_DOWNLOAD_DIR=\"/data/dl\"\n"))))
  (it "uses ~/Downloads without a usable desktop setting"
      (doseq [user-dirs [nil "" "XDG_DESKTOP_DIR=\"$HOME/Desktop\"\n" "XDG_DOWNLOAD_DIR=\"rel\"\n"]]
        (expect (= "/home/u/Downloads" (paths/downloads-dir "/home/u" user-dirs))))))
