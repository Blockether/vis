(ns com.blockether.vis.tui.shared-logs-test
  "Production composer and transcript fixture for shared app logs; no gateway."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.attachment-intake :as intake]
            [com.blockether.vis.tui.chat :as chat]
            [com.blockether.vis.tui.capture :as cap]
            [com.blockether.vis.tui.composer-attachment-rail :as rail]
            [com.blockether.vis.tui.interactions :as interactions]
            [com.blockether.vis.tui.render :as render]
            [com.blockether.vis.tui.theme :as theme]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]])
  (:import [com.googlecode.lanterna.screen TerminalScreen]
           [java.nio.file Files]
           [java.util Base64]))

(def log-files
  [{:id "gzip-log"
    :filename "vis-diagnostics.jsonl.gz"
    :media-type "application/gzip"
    :base64
    "H4sIAAAAAAAC/w3KMQ6AIAxG4Z1j/LMMrtyGQNVGLMQ2LoS72/V7b+JmqUj4WGMeI1bOp3Q1LooNWi56MtK+wgR9JOarWn6Nqufm1FxYjo4VfvjpmRxNAAAA"
    :size 90}
   {:id "plain-log"
    :filename "vis-diagnostics.jsonl"
    :media-type "application/x-ndjson"
    :base64
    "eyJraW5kIjoidmlzLWFwcC1kaWFnbm9zdGljcyIsInNjaGVtYSI6MX0KeyJldmVudCI6InN0YXJ0ZWQiLCJsZXZlbCI6ImluZm8ifQo="
    :size 77}])

(defn log-message
  "Replay an attachment-only turn from durable wire descriptors."
  []
  (first (@#'chat/turns->messages
          [{"turn_id" "shared-logs"
            "status" "completed"
            "request" "Please check these app logs."
            "created_at" 0
            "attachments" (mapv (fn [file]
                                  {"source" "user"
                                   "filename" (:filename file)
                                   "media_type" (:media-type file)
                                   "size" (:size file)})
                                log-files)}])))

(defn paint-shared-logs!
  "Paint the real transcript bubble and next-message attachment rail."
  [^TerminalScreen screen]
  (.clear screen)
  (let [g
        (.newTextGraphics screen)

        cols
        (.getColumns (.getTerminalSize screen))

        height
        (.getRows (.getTerminalSize screen))]

    (doto g
      (.setBackgroundColor theme/terminal-bg)
      (.setForegroundColor theme/text-fg)
      (.fill \space))
    (.beginFrame interactions/hit-map)
    (let [used
          (render/draw-chat-bubble! g (log-message) 2 1 (- cols 4) {:viewport-h height})

          top
          (+ 4 (long used))]

      (.putString g 2 (int (- top 2)) "Next message: staged logs")
      (rail/draw! g log-files top cols {:focused? true :focused-index 0})
      (.commitFrame interactions/hit-map)
      (.refresh screen)
      (+ top 3))))

(deftest shared-logs-grid
  (doseq [cols [40 80]]
    (let [capture (cap/capture! {:cols cols
                                 :rows 24
                                 :paint! (fn [{:keys [screen]}]
                                           (paint-shared-logs! screen))})
          text (cap/frame-text capture)]

      (is (nil? (:error capture)))
      (is (str/includes? text "vis-diagnostics.jsonl.gz"))
      (is (str/includes? text "vis-diagnostics.jsonl"))
      (is (= 2
             (count (filter #(= :attachment-remove (:kind %)) (.current interactions/hit-map))))))))

(deftest shared-logs-paste-roundtrip
  (let [dir
        (Files/createTempDirectory "vis-shared-logs"
                                   (make-array java.nio.file.attribute.FileAttribute 0))

        contract
        {"enabled" true
         "media_types" ["application/gzip" "application/x-ndjson"]
         "max_files" 8
         "max_file_bytes" 1024
         "max_video_bytes" 1024
         "max_audio_bytes" 1024}]

    (try (doseq [file log-files]
           (let [path (.resolve dir ^String (:filename file))
                 bytes (.decode (Base64/getDecoder) ^String (:base64 file))]

             (Files/write path bytes (make-array java.nio.file.OpenOption 0))
             (let [out (intake/file-drop contract [] (str "'" path "'") (str dir))]
               (is (:handled? out))
               (is (empty? (:rejected out)))
               (is (= (:media-type file) (:media-type (first (:added out)))))
               (is (str/includes? (chat/user-request-with-staged-attachments "" (:added out))
                                  (:filename file))))))
         (finally (doseq [file log-files]
                    (Files/deleteIfExists (.resolve dir ^String (:filename file))))
                  (Files/deleteIfExists dir)))))
