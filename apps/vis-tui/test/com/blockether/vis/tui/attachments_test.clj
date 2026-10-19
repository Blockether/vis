(ns com.blockether.vis.tui.attachments-test
  (:require [com.blockether.vis.tui.attachments :as attachments]
            [lazytest.core :refer [defdescribe expect it]]))

;; Regression: Open on a ZIP artifact unpacked it in a hidden temp folder.
(defdescribe file-manager-artifact-test
             (it "sends archives and untyped bytes to the file manager"
                 (doseq [[media-type filename]
                         [["application/zip" "bundle.zip"] ["application/x-zip-compressed" "bundle"]
                          ["application/gzip" "logs.gz"] ["application/x-7z-compressed" "a.7z"]
                          ["application/octet-stream" "blob.bin"] ["" "release.tar.gz"]
                          [nil "BUNDLE.ZIP"]]]
                   (expect (attachments/file-manager-artifact? media-type filename))))
             (it "keeps viewable files in their viewer"
                 (doseq [[media-type filename]
                         [["application/pdf" "report.pdf"] ["image/png" "chart.png"]
                          ["text/markdown; charset=utf-8" "notes.md"] [nil "report.zip.md"]]]
                   (expect (not (attachments/file-manager-artifact? media-type filename))))))
