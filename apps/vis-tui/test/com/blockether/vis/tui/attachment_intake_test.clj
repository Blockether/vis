(ns com.blockether.vis.tui.attachment-intake-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.tui.attachment-intake :as intake]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io ByteArrayOutputStream]
           [java.util.zip ZipEntry ZipOutputStream]))

(def ^:private docx-media-type
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document")

(def capabilities
  {"features" {"attachments" {"enabled" true
                              "media_types" ["image/png" "application/pdf" "text/html"
                                             "application/xhtml+xml" docx-media-type]
                              "max_files" 8
                              "max_file_bytes" 1048576
                              "max_video_bytes" 1048576
                              "max_audio_bytes" 1048576}}})

(def ^:private zip-capabilities
  "A gateway that also takes ZIP archives."
  (update-in capabilities ["features" "attachments" "media_types"] conj "application/zip"))

;; The JVM deletes in reverse order of registration: each file, then its directory.
;; Without this, the suite left hundreds of directories in the user's temp directory.
(defn- temp-dir
  []
  (doto (.toFile (java.nio.file.Files/createTempDirectory
                   "vis-tui-intake"
                   (make-array java.nio.file.attribute.FileAttribute 0)))
    (.deleteOnExit)))

(defn- write-bytes!
  [^java.io.File dir name bytes]
  (let [f (doto (io/file dir name) (.deleteOnExit))]
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

(defn- docx-bytes
  "A Word package. Its `[Content_Types].xml` registry comes after a large first
   entry, past the sniffed file head, as in files that some editors write."
  ^bytes [main-type]
  (let [out (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (doseq [[entry-name text]
              [["word/document.xml"
                (let [rnd (java.util.Random. 7)]
                  (apply str (repeatedly 20000 #(char (+ 97 (.nextInt rnd 26))))))]
               ["[Content_Types].xml"
                (str "<Types><Override PartName=\"/word/document.xml\" ContentType=\""
                     main-type
                     "\"/></Types>")]]]
        (.putNextEntry zip (ZipEntry. ^String entry-name))
        (.write zip (.getBytes ^String text "UTF-8"))
        (.closeEntry zip)))
    (.toByteArray out)))

;; Regression: a dropped PDF or Word document must stage like a dropped picture.
(defdescribe
  drop-admits-pdf-and-word-documents
  (it "stages a dropped PDF and a dropped Word document"
      (let [dir
            (temp-dir)

            pdf
            (write-bytes! dir "report.pdf" (.getBytes "%PDF-1.7\n" "UTF-8"))

            docx
            (write-bytes!
              dir
              "Tryb pracy.docx"
              (docx-bytes
                "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"))

            dropped
            (intake/file-drop capabilities
                              []
                              (str (.getCanonicalPath pdf) " '" (.getCanonicalPath docx) "'")
                              (.getPath dir))]

        (expect (:handled? dropped))
        (expect (= [] (:rejected dropped)))
        (expect (= ["application/pdf" docx-media-type] (mapv :media-type (:attachments dropped))))
        (expect (= ["report.pdf" "Tryb pracy.docx"] (mapv :filename (:attachments dropped))))))
  (it "stages a ZIP archive with a Word name as a ZIP, never as a Word document"
      (let [dir
            (temp-dir)

            fake
            (write-bytes!
              dir
              "sheet.docx"
              (docx-bytes
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"))

            dropped
            (intake/file-drop zip-capabilities [] (.getCanonicalPath fake) (.getPath dir))]

        (expect (= [] (:rejected dropped)))
        (expect (= ["application/zip"] (mapv :media-type (:attachments dropped)))))))

;; Regression: a dropped ZIP archive was refused as an unsupported file format.
(defdescribe
  drop-admits-zip-archives
  (it "stages a dropped ZIP archive"
      (let [dir
            (temp-dir)

            archive
            (write-bytes! dir "bundle.zip" (docx-bytes "text/plain"))

            dropped
            (intake/file-drop zip-capabilities [] (.getCanonicalPath archive) (.getPath dir))]

        (expect (= [] (:rejected dropped)))
        (expect (= ["application/zip"] (mapv :media-type (:attachments dropped))))
        (expect (= ["bundle.zip"] (mapv :filename (:attachments dropped))))))
  (it "refuses a ZIP archive when the gateway does not take ZIP files"
      (let [dir
            (temp-dir)

            archive
            (write-bytes! dir "bundle.zip" (docx-bytes "text/plain"))

            dropped
            (intake/file-drop capabilities [] (.getCanonicalPath archive) (.getPath dir))]

        (expect (= [] (:attachments dropped)))
        (expect (= 1 (count (:rejected dropped)))))))

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

(defdescribe picker-list-is-derived-from-the-advertised-contract
             (it "picker list is derived from the advertised contract"
                 (let [dir
                       (temp-dir)

                       pdf
                       (write-bytes! dir "report.bin" (.getBytes "%PDF-1.7\n" "UTF-8"))

                       _
                       (write-bytes! dir "notes.txt" (.getBytes "plain text" "UTF-8"))]

                   (expect (= [(.getCanonicalPath pdf)]
                              (mapv :id
                                    (intake/picker-items capabilities
                                                         (.getPath dir)
                                                         [{:path "report.bin"}
                                                          {:path "notes.txt"}]))))
                   (expect (= [] (intake/picker-items {} (.getPath dir) [{:path "report.bin"}]))))))

;; User report 2026-10-30: the C-x a a picker could not attach a file outside the
;; workspace.
(defdescribe
  typed-path-picker-row
  (it "a typed path outside the workspace gives its file as a row"
      (let [outside
            (temp-dir)

            png
            (png-file! outside "shot.png")

            workspace
            (temp-dir)]

        (expect (= [(.getCanonicalPath png)]
                   (mapv
                     :id
                     (intake/typed-path-items capabilities (.getPath workspace) (.getPath png)))))))
  (it "search text and an unsupported file give no row"
      (let [dir (temp-dir)]
        (write-bytes! dir "notes.txt" (.getBytes "plain text" "UTF-8"))
        (expect (= [] (intake/typed-path-items capabilities (.getPath dir) "shot")))
        (expect (= [] (intake/typed-path-items capabilities (.getPath dir) "./notes.txt"))))))
