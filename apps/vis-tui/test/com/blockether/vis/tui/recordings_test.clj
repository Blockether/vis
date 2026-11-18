(ns com.blockether.vis.tui.recordings-test
  (:require [com.blockether.vis.tui.recordings :as recordings]
            [com.blockether.vis.tui.terminal-image :as timg]
            [lazytest.core :as lt :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util Base64]))

(defn- temp-dir
  ^File []
  (.toFile (Files/createTempDirectory "vis-recordings-test" (make-array FileAttribute 0))))

(def ^:private memo-bytes (.getBytes "RIFF....WAVEfmt " "US-ASCII"))

(def ^:private persisted-memo
  {"id" "att-1"
   "source" "user"
   "media_type" "audio/mp4"
   "filename" "memo.m4a"
   "base64" (.encodeToString (Base64/getEncoder) memo-bytes)})

(lt/set-ns-context! [(lt/around-each [f]
                                     (let [dir (temp-dir)]
                                       (with-redefs [timg/attachment-cache-dir (constantly dir)]
                                         (f))))])

;; User report 2026-11-18: the TUI showed a recording only as a chip, and no key played it.
(defdescribe
  session-recordings-test
  (it
    "lists the recordings that the person sent and the audio that the model attached, oldest first"
    (let [sent
          (recordings/persisted-recordings
            [persisted-memo
             {"source" "user" "media_type" "image/png" "filename" "a.png" "base64" "AAAA"}])

          messages
          [{:role :user :recordings sent}
           {:role :assistant
            :traces [{:iteration-id "it-1"
                      :attachments
                      [{"index" 0 "media_type" "image/png" "filename" "chart.png"}
                       {"index" 1 "media_type" "audio/mpeg" "filename" "answer.mp3"}]}]}]

          entries
          (recordings/session-recordings messages)]

      (expect (= [["memo.m4a" :user] ["answer.mp3" :produced]]
                 (mapv (juxt :filename :source) entries)))
      (expect (= (seq memo-bytes)
                 (seq (Files/readAllBytes (.toPath (File. (str (:path (first entries)))))))))
      (expect (= {:iteration-id "it-1" :index 1}
                 (select-keys (second entries) [:iteration-id :index])))))
  (it "keeps the cached file of a recording when the session loads again"
      (let [path (:path (first (recordings/persisted-recordings [persisted-memo])))]
        (expect (= path (:path (first (recordings/persisted-recordings [persisted-memo])))))))
  (it "lists a recording that the person attaches to a message that is not sent yet"
      (expect (=
                [{:source :user :filename "memo.wav" :media-type "audio/wav" :path "/tmp/memo.wav"}]
                (recordings/staged-recordings
                  [{:path "/tmp/memo.wav" :media-type "audio/wav" :filename "memo.wav"}
                   {:path "/tmp/a.pdf" :media-type "application/pdf" :filename "a.pdf"}])))))
