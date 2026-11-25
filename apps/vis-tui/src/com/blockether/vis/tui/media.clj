(ns com.blockether.vis.tui.media
  "Local playback of a live view media node."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [com.blockether.vis.tui.external-opener :as opener]
            [com.blockether.vis.tui.voice-output :as voice-output])
  (:import [java.io File]
           [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]
           [java.util Base64]))

(set! *warn-on-reflection* true)

(defn- write-temp!
  ^File [{:keys [data media-type]}]
  (let [suffix (case media-type
                 "audio/mpeg" "mp3"
                 "audio/mp4" "m4a"
                 "video/ogg" "ogv"
                 (last (str/split media-type #"/")))
        bytes (.decode (Base64/getDecoder) ^String data)
        ^Path path (Files/createTempFile "vis-media-" (str "." suffix)
                                         (make-array FileAttribute 0))
        file (.toFile path)]
    (try (with-open [out (java.io.FileOutputStream. file)]
           (.write out bytes))
         file
         (catch Exception e
           (.delete file)
           (throw e)))))

(defn- report-open!
  [{:keys [status error]}]
  (when-not (= :ok status)
    (vis/notify! (or error "Vis cannot open this media.") :level :warn)))

(defn open!
  "Play embedded audio, open embedded video in the system player, or open a URL."
  [{:keys [type kind label alt before] :as node} version]
  (when (= :media type)
    (let [{:keys [url data] :as source} (if (= :before version) before node)]
      (cond url (vis/worker-future "vis-media-url" #(report-open! (opener/open! url)))
            (and data (= :audio kind))
            (voice-output/play-recording!
              {:title (str (or label alt)
                           (when before (if (= :before version) " — Before" " — After")))
               :load! #(hash-map :file (write-temp! source) :is-temp true)})
            data (vis/worker-future
                   "vis-media-open"
                   #(try (let [file (write-temp! source)
                               result (opener/open-local! file)]
                           (if (= :ok (:status result)) (.deleteOnExit file) (.delete file))
                           (report-open! result))
                         (catch Exception e
                           (vis/notify! (or (ex-message e) "Vis cannot open this media.")
                                        :level :warn))))))))
