(ns com.blockether.vis.internal.tui-attachment-parity-test
  "One boundary proof from TUI staging through durable gateway history."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.chat :as chat]
            [com.blockether.vis.ext.channel-tui.composer-attachments :as composer-attachments]
            [com.blockether.vis.ext.channel-tui.terminal-image :as terminal-image]
            [com.blockether.vis.ext.persistance-sqlite.test-helpers :as h]
            [com.blockether.vis.internal.attachment-fixtures :as fixtures]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.gateway.state :as gateway-state]
            [com.blockether.vis.internal.loop :as lp]
            [lazytest.core :refer [defdescribe expect it]]))

(h/use-mem-store!)

(defn- temp-dir
  []
  (.toFile (java.nio.file.Files/createTempDirectory
             "vis-tui-attachment-parity-"
             (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- write-file
  [dir filename bytes]
  (let [file (io/file dir filename)]
    (with-open [out (io/output-stream file)]
      (.write out ^bytes bytes))
    file))

(defn- wav-bytes
  []
  (byte-array
    (concat (.getBytes "RIFF" "US-ASCII") [36 0 0 0] (.getBytes "WAVE" "US-ASCII") (repeat 32 0))))

(defn- pdf-bytes
  []
  (byte-array (concat (.getBytes "%PDF-1.4" "US-ASCII") [10] (.getBytes "%%EOF" "US-ASCII") [10])))

(defn- capabilities
  []
  {"features" {"attachments" {"enabled" true
                              "media_types" ["image/png" "video/mp4" "audio/wav" "application/pdf"]
                              "max_files" 8
                              "max_file_bytes" (* 4 1024 1024)
                              "max_video_bytes" (* 32 1024 1024)
                              "max_audio_bytes" (* 32 1024 1024)}}})

(defn- history-messages
  [db session-id cache-dir]
  (let [turns
        (with-redefs [lp/db-info (constantly db)]
          (gateway-state/transcript session-id))

        turns->messages
        (var-get (resolve 'com.blockether.vis.ext.channel-tui.chat/turns->messages))]

    (with-redefs [terminal-image/attachment-cache-dir (constantly cache-dir)]
      (turns->messages turns))))

(defdescribe
  tui-attachment-gateway-parity-test
  (it
    "stages, submits, validates, stores, reloads and renders every supported family"
    (let [dir
          (temp-dir)

          cache-dir
          (io/file dir "cache")

          _
          (.mkdirs cache-dir)

          files
          [(write-file dir "pixel.png" fixtures/tiny-png-bytes)
           (write-file dir "clip.mp4" fixtures/tiny-mp4-bytes)
           (write-file dir "memo.wav" (wav-bytes)) (write-file dir "report.pdf" (pdf-bytes))]

          staged
          (composer-attachments/admit-files (capabilities) [] files)

          sent
          (atom nil)

          _
          (with-redefs [vis/gateway-submit-turn-sync! (fn [_ body]
                                                        (reset! sent body)
                                                        {"content" []})]
            (chat/turn! {:id "session"}
                        "review these four files"
                        {:attachments (composer-attachments/inline-payloads (:attachments
                                                                              staged))}))

          prepared
          (attachments/prepare-inline-attachments (:attachments @sent))

          stored
          (mapv (fn [att]
                  (cond-> att
                    (= "audio/wav" (:media-type att))
                    (assoc :transcription "remember the parity check")))
                (:attached prepared))

          db
          (h/store)

          session-id
          (h/store-session! db
                            {:channel :tui
                             :title "Attachment parity"
                             :provider :openai
                             :model "gpt-4o"
                             :system-prompt "system"})

          _
          (vis/db-store-session-turn! db
                                      {:parent-session-id session-id
                                       :user-request "review these four files"
                                       :status :success
                                       :attachments stored})

          messages
          (history-messages db session-id cache-dir)

          user-text
          (:text (first messages))

          transcript
          (with-redefs [lp/db-info (constantly db)]
            (gateway-state/transcript session-id))

          descriptors
          (get (first transcript) "attachments")]

      (expect (empty? (:rejected staged)))
      (expect (= ["image/png" "video/mp4" "audio/wav" "application/pdf"]
                 (mapv :media-type (:attachments staged))))
      (expect (= "review these four files" (:request @sent)))
      (expect (every? #(not (contains? % :path)) (:attachments @sent)))
      (expect (= 4 (count (:attached prepared))))
      (expect (= ["pixel.png" "clip.mp4" "memo.wav" "report.pdf"]
                 (mapv #(get % "filename") descriptors)))
      (expect (= ["image/png" "video/mp4" "audio/wav" "application/pdf"]
                 (mapv #(get % "media_type") descriptors)))
      (expect (every? #(= "user" (get % "source")) descriptors))
      (expect (every? pos? (map #(long (get % "size")) descriptors)))
      (expect (str/includes? user-text "[Image #1: pixel.png]"))
      (expect (str/includes? user-text "[Image #2: clip.mp4]"))
      (expect (str/includes? user-text "[Transcription #1: memo.wav]"))
      (expect (str/includes? user-text "remember the parity check"))
      (expect (str/includes? user-text "[Attachment #4: report.pdf]"))))
  (it
    "keeps count, type and size rejection deterministic"
    (let [dir
          (temp-dir)

          png
          (write-file dir "pixel.png" fixtures/tiny-png-bytes)

          pdf
          (write-file dir "report.pdf" (pdf-bytes))

          reject
          (fn [caps files]
            (:rejected (composer-attachments/admit-files caps [] files)))

          over-count-caps
          (assoc-in (capabilities) ["features" "attachments" "max_files"] 1)

          over-size-caps
          (assoc-in (capabilities) ["features" "attachments" "max_file_bytes"] 1)

          wrong-type-caps
          (assoc-in (capabilities) ["features" "attachments" "media_types"] ["image/jpeg"])

          over-count
          (reject over-count-caps [png pdf])

          over-size
          (reject over-size-caps [png])

          wrong-type
          (reject wrong-type-caps [png])]

      (expect (= over-count (reject over-count-caps [png pdf])))
      (expect (= over-size (reject over-size-caps [png])))
      (expect (= wrong-type (reject wrong-type-caps [png])))
      (expect (= 1 (count over-count) (count over-size) (count wrong-type)))
      (expect (str/includes? (first over-count) "limit of 1 attachments"))
      (expect (str/includes? (first over-size) "larger than"))
      (expect (str/includes? (first wrong-type) "does not accept")))))
