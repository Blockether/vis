(ns com.blockether.vis.ext.channel-tui.composer-attachments-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.channel-tui.composer-attachments :as composer-attachments]
            [com.blockether.vis.ext.channel-tui.state :as state]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private one-pixel-png
  (.decode
    (java.util.Base64/getDecoder)
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII="))

(defn- temporary-file
  [suffix bytes]
  (let [file (java.io.File/createTempFile "vis-composer-attachment-" suffix)]
    (.deleteOnExit file)
    (with-open [out (io/output-stream file)]
      (.write out ^bytes bytes))
    file))

(defn- capabilities
  [{:keys [max-files max-file-bytes media-types]
    :or {max-files 8 max-file-bytes 1024 media-types ["image/png"]}}]
  {"features" {"attachments" {"enabled" true
                              "media_types" media-types
                              "max_files" max-files
                              "max_file_bytes" max-file-bytes
                              "max_video_bytes" 2048
                              "max_audio_bytes" 3072}}})

(defdescribe
  composer-attachment-admission-test
  (it "stages canonical metadata and a content-stable identity"
      (let [file
            (temporary-file ".png" one-pixel-png)

            first-result
            (composer-attachments/admit-files (capabilities {}) [] [file])

            second-result
            (composer-attachments/admit-files (capabilities {}) [] [file])

            attachment
            (first (:added first-result))]

        (expect (= [] (:rejected first-result)))
        (expect (= {:filename (.getName file)
                    :media-type "image/png"
                    :size (alength one-pixel-png)
                    :width 1
                    :height 1}
                   (select-keys attachment [:filename :media-type :size :width :height])))
        (expect (= (:id attachment) (:id (first (:added second-result)))))
        (expect (.startsWith ^String (:id attachment) "sha256:"))))
  (it
    "uses the gateway count, media and byte limits verbatim"
    (let [png
          (temporary-file ".png" one-pixel-png)

          unsupported
          (temporary-file ".png" (.getBytes "plain text" "UTF-8"))

          too-large
          (composer-attachments/admit-files (capabilities {:max-file-bytes (dec (alength
                                                                                  one-pixel-png))})
                                            []
                                            [png])

          wrong-media
          (composer-attachments/admit-files (capabilities {:media-types ["image/jpeg"]}) [] [png])

          over-count
          (composer-attachments/admit-files (capabilities {:max-files 1}) [{:id "existing"}] [png])

          unrecognized
          (composer-attachments/admit-files (capabilities {}) [] [unsupported])]

      (expect (re-find #"larger than the gateway limit" (first (:rejected too-large))))
      (expect (re-find #"does not accept image/png" (first (:rejected wrong-media))))
      (expect (re-find #"limit of 1 attachments" (first (:rejected over-count))))
      (expect (re-find #"unsupported file format" (first (:rejected unrecognized))))))
  (it "refuses intake when the gateway disables attachments"
      (let [file
            (temporary-file ".png" one-pixel-png)

            disabled
            (assoc-in (capabilities {}) ["features" "attachments" "enabled"] false)

            result
            (composer-attachments/admit-files disabled [] [file])]

        (expect (= [] (:added result)))
        (expect (re-find #"gateway did not advertise" (first (:rejected result))))))
  (it "admits every advertised media family by sniffed bytes, not extension"
      (let [fixtures
            [[one-pixel-png "image/png"] [(.getBytes "0000ftypisom" "US-ASCII") "video/mp4"]
             [(.getBytes "RIFF0000WAVE" "US-ASCII") "audio/wav"]
             [(.getBytes "%PDF-1.7\n" "UTF-8") "application/pdf"]
             [(.getBytes "<html><body>hello</body></html>" "UTF-8") "text/html"]
             [(.getBytes "<html xmlns='http://www.w3.org/1999/xhtml'></html>" "UTF-8")
              "application/xhtml+xml"]]

            files
            (mapv #(temporary-file ".bin" (first %)) fixtures)

            expected
            (mapv second fixtures)

            result
            (composer-attachments/admit-files (capabilities {:media-types expected}) [] files)]

        (expect (= expected (mapv :media-type (:added result))))
        (expect (= [] (:rejected result)))))
  (it "deduplicates the same bytes through the one admission path"
      (let [file
            (temporary-file ".png" one-pixel-png)

            staged
            (composer-attachments/admit-files (capabilities {}) [] [file file])]

        (expect (= 1 (count (:added staged))))
        (expect (re-find #"already attached" (first (:rejected staged))))))
  (it "encodes explicit gateway payloads without leaking filesystem paths"
      (let [file
            (temporary-file ".png" one-pixel-png)

            [payload]
            (composer-attachments/inline-payloads [{:path (.getCanonicalPath file)
                                                    :filename "screen shot.png"
                                                    :media-type "image/png"}])]

        (expect (= {:filename "screen shot.png"
                    :media-type "image/png"
                    :base64 (.encodeToString (java.util.Base64/getEncoder) one-pixel-png)}
                   payload))
        (expect (not (contains? payload :path)))))
  (it "removes one staged attachment by stable identity"
      (expect (= [{:id "sha256:b"}]
                 (composer-attachments/remove-attachment [{:id "sha256:a"} {:id "sha256:b"}]
                                                         "sha256:a"))))
  (it "keeps staged files, focus, and feedback with their composer tab"
      (let [snapshot (#'state/tab-snapshot
                      {:attachments [{:id "sha256:a"}]
                       :attachment-feedback ["too large"]
                       :attachment-focus? true
                       :attachment-index 0})]
        (expect (= [{:id "sha256:a"}] (:attachments snapshot)))
        (expect (= ["too large"] (:attachment-feedback snapshot)))
        (expect (true? (:attachment-focus? snapshot)))
        (expect (= 0 (:attachment-index snapshot)))))
  (it "focuses, cycles, and removes one rail item without touching its peers"
      (reset! state/app-db {:attachments [{:id "a"} {:id "b"} {:id "c"}]
                            :attachment-focus? false
                            :attachment-index 0
                            :render-version 0})
      (state/dispatch [:focus-attachments])
      (state/dispatch [:move-attachment-focus -1])
      (expect (true? (:attachment-focus? @state/app-db)))
      (expect (= 2 (:attachment-index @state/app-db)))
      (state/dispatch [:remove-attachment "c"])
      (expect (= ["a" "b"] (mapv :id (:attachments @state/app-db))))
      (expect (= 1 (:attachment-index @state/app-db)))
      (state/dispatch [:remove-attachment "b"])
      (state/dispatch [:remove-attachment "a"])
      (expect (false? (:attachment-focus? @state/app-db)))
      (expect (= 0 (:attachment-index @state/app-db))))
  (it "does not copy the gateway media vocabulary or numeric limits"
      (let [source (slurp (io/resource
                            "com/blockether/vis/ext/channel_tui/composer_attachments.clj"))]
        (expect (not (str/includes? source "attachments/max-")))
        (doseq [prefix ["\"image/" "\"video/" "\"audio/" "\"application/"]]
          (expect (not (str/includes? source prefix)))))))
