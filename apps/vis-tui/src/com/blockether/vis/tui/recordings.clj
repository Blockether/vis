;; Recordings of a session
(ns com.blockether.vis.tui.recordings
  "The recordings of a session: the audio files that a person sent and the audio files
   that the model attached. C-x a p plays them, because a terminal paints no player in
   the conversation.

   One entry is `{:source :filename :media-type ...}`. A `:user` entry has a local
   `:path`. A `:produced` entry has the `:iteration-id` and `:index` of the byte route,
   and the client downloads it when the person plays it."
  (:require [clojure.string :as str]
            [com.blockether.vis.tui.terminal-image :as timg])
  (:import [java.io File]
           [java.nio.file Files OpenOption]
           [java.util Base64]))

(set! *warn-on-reflection* true)

(defn audio?
  "True when `media-type` names an audio file."
  [media-type]
  (str/starts-with? (str/lower-case (str media-type)) "audio/"))

(defn- extension
  "The extension of `filename` with its dot, or `.audio` when it has none."
  [filename]
  (or (re-find #"\.[A-Za-z0-9]{1,8}$" (str filename)) ".audio"))

(defn- cache-file
  "The cache file of persisted user attachment `att` (wire map, string keys). The file
   name is the attachment ID, so a second load of the session reuses the file. A row
   without an ID gets a new name on each call."
  ^File [att]
  (File. (timg/attachment-cache-dir)
         (str (or (not-empty (str (get att "id"))) (str (random-uuid)))
              (extension (get att "filename")))))

(defn cached?
  "True when the attachment cache holds the file of persisted recording `att`."
  [att]
  (.isFile (cache-file att)))

(defn- cached-recording!
  "The cache file of `att`, written from its inline bytes when the cache does not hold it."
  ^File [att]
  (let [f (cache-file att)]
    (when-not (.isFile f)
      (Files/write (.toPath f)
                   ^bytes (.decode (Base64/getDecoder) (str (get att "base64")))
                   ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0)))
    f))

(defn persisted-recordings
  "The entries for the audio files that a person sent in one persisted turn, from its
   wire `attachments`. Each file goes to the attachment cache, so the entry still plays
   after the original file is gone. A file with no inline bytes and no cache file is
   left out."
  [attachments]
  (into []
        (keep (fn [att]
                (when (and (= "user" (str (get att "source")))
                           (audio? (get att "media_type"))
                           (or (not (str/blank? (str (get att "base64")))) (cached? att)))
                  (try {:source :user
                        :filename (str (or (get att "filename") "recording"))
                        :media-type (str (get att "media_type"))
                        :path (.getAbsolutePath (cached-recording! att))}
                       (catch Exception _ nil)))))
        attachments))

(defn staged-recordings
  "The entries for the audio files among the composer `attachments` of a message that
   the client sends now. They play from their source path until the session reloads."
  [attachments]
  (into []
        (keep (fn [{:keys [path media-type filename]}]
                (when (and (audio? media-type) (not (str/blank? (str path))))
                  {:source :user
                   :filename (str (or filename (.getName (File. (str path)))))
                   :media-type (str media-type)
                   :path (str path)})))
        attachments))

(defn- produced-recordings
  "The entries for the audio files that the steps of `traces` attached."
  [traces]
  (into []
        (mapcat (fn [entry]
                  (keep (fn [att]
                          (when (and (audio? (get att "media_type")) (number? (get att "index")))
                            {:source :produced
                             :filename (str (or (get att "filename") "recording"))
                             :media-type (str (get att "media_type"))
                             :iteration-id (str (or (get att "iteration_id") (:iteration-id entry)))
                             :index (get att "index")
                             :storage (get att "storage")
                             :path (get att "path")}))
                        (:attachments entry))))
        traces))

(defn session-recordings
  "Every recording in `messages`, oldest first: the `:recordings` of each user message
   and the audio files of each assistant step."
  [messages]
  (into [] (mapcat #(into (vec (:recordings %)) (produced-recordings (:traces %)))) messages))
