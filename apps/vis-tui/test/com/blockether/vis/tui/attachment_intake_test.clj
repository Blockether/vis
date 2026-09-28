(ns com.blockether.vis.tui.attachment-intake-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.tui.attachment-intake :as intake]
            [lazytest.core :refer [defdescribe expect it]]))

(def capabilities
  {"features" {"attachments" {"enabled" true
                              "media_types" ["image/png" "application/pdf" "text/html"
                                             "application/xhtml+xml"]
                              "max_files" 8
                              "max_file_bytes" 1048576
                              "max_video_bytes" 1048576
                              "max_audio_bytes" 1048576}}})

(defn- temp-dir
  []
  (.toFile (java.nio.file.Files/createTempDirectory
             "vis-tui-intake"
             (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- write-bytes!
  [^java.io.File dir name bytes]
  (let [f (io/file dir name)]
    (with-open [out (io/output-stream f)]
      (.write out ^bytes bytes))
    f))

(defn- png-file!
  [dir name]
  (write-bytes!
    dir
    name
    (.decode
      (java.util.Base64/getDecoder)
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=")))

(defdescribe all-intake-adapters-share-structured-staging
             (it "drop, clipboard and picker produce the identical record"
                 (let [dir
                       (temp-dir)

                       image
                       (png-file! dir "screen shot.png")

                       path
                       (.getCanonicalPath image)

                       drop
                       (intake/file-drop capabilities [] (str "'" path "'") (.getPath dir))

                       clipboard
                       (intake/clipboard-image capabilities [] {:path path :mime "image/png"})

                       picker
                       (intake/picker-selection capabilities [] [path])]

                   (expect (:handled? drop))
                   (expect (= (:attachments drop) (:attachments clipboard) (:attachments picker)))
                   (expect (= "screen shot.png" (get-in drop [:attachments 0 :filename])))
                   (expect (= "image/png" (get-in drop [:attachments 0 :media-type])))
                   (expect (string? (get-in drop [:attachments 0 :id]))))))

(defdescribe
  drop-is-strict-and-never-produces-prompt-text
  (it "drop is strict and never produces prompt text"
      (let [dir
            (temp-dir)

            image
            (png-file! dir "a.png")

            path
            (.getCanonicalPath image)

            dropped
            (intake/file-drop capabilities [] path (.getPath dir))

            prose
            (intake/file-drop capabilities [] (str path " ordinary prose") (.getPath dir))]

        (expect (:handled? dropped))
        (expect (not (contains? dropped :input)))
        (expect (not (contains? dropped :text)))
        (expect (= {:handled? false :source :drop} prose)))))

(defdescribe
  picker-admits-multiple-advertised-document-types
  (it "picker admits multiple advertised document types"
      (let [dir
            (temp-dir)

            pdf
            (write-bytes! dir "report.pdf" (.getBytes "%PDF-1.7\n" "UTF-8"))

            html
            (write-bytes! dir
                          "page.html"
                          (.getBytes "<!doctype html><html><body>x</body></html>" "UTF-8"))

            xhtml
            (write-bytes!
              dir
              "page.xhtml"
              (.getBytes "<?xml version='1.0'?><html xmlns='http://www.w3.org/1999/xhtml'></html>"
                         "UTF-8"))

            paths
            (mapv #(.getCanonicalPath ^java.io.File %) [pdf html xhtml])

            result
            (intake/picker-selection capabilities [] paths)]

        (expect (= ["application/pdf" "text/html" "application/xhtml+xml"]
                   (mapv :media-type (:attachments result))))
        (expect (= ["report.pdf" "page.html" "page.xhtml"]
                   (mapv :filename (:attachments result)))))))

(defdescribe picker-cancellation-is-harmless
             (it "picker cancellation is harmless"
                 (let [current [{:id "already" :filename "kept.png"}]]
                   (expect (= {:handled? false :source :picker}
                              (intake/picker-selection capabilities current nil))))))

(defdescribe picker-list-is-derived-from-the-advertised-contract
             (it "picker list is derived from the advertised contract"
                 (let [dir
                       (temp-dir)

                       pdf
                       (write-bytes! dir "report.bin" (.getBytes "%PDF-1.7\n" "UTF-8"))

                       _
                       (write-bytes! dir "notes.txt" (.getBytes "plain text" "UTF-8"))]

                   (expect (= [(.getCanonicalPath pdf)]
                              (intake/picker-files capabilities
                                                   (.getPath dir)
                                                   [{:path "report.bin"} {:path "notes.txt"}])))
                   (expect (= [] (intake/picker-files {} (.getPath dir) [{:path "report.bin"}]))))))
