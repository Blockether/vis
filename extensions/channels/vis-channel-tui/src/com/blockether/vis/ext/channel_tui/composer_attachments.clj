(ns com.blockether.vis.ext.channel-tui.composer-attachments
  "The TUI composer's one file-staging and admission boundary.

   Every intake route hands files to [[admit-files]]. It derives metadata from the
   file itself, then judges that metadata only against the attachment contract the
   gateway advertised. No media vocabulary or byte/count fallback lives here."
  (:require [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.channel-tui.terminal-image :as terminal-image]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.format :as fmt])
  (:import [java.io File FileInputStream]
           [java.security MessageDigest]
           [java.util Base64]))

(set! *unchecked-math* :warn-on-boxed)

(defn- wire-value
  [m wire-key]
  (let [keyword-key
        (keyword wire-key)

        kebab-key
        (keyword (str/replace wire-key "_" "-"))]

    (cond (contains? m wire-key) (get m wire-key)
          (contains? m keyword-key) (get m keyword-key)
          (contains? m kebab-key) (get m kebab-key)
          :else nil)))

(defn attachment-contract
  "Normalize the gateway capabilities response (or its attachment feature map).
   Returns nil when attachment intake is disabled or the gateway omitted any
   admission fact the composer requires."
  [capabilities]
  (let [features
        (or (wire-value capabilities "features") capabilities)

        raw
        (or (wire-value features "attachments") features)

        media-types
        (wire-value raw "media_types")

        max-files
        (wire-value raw "max_files")

        max-file-bytes
        (wire-value raw "max_file_bytes")

        max-video-bytes
        (wire-value raw "max_video_bytes")

        max-audio-bytes
        (wire-value raw "max_audio_bytes")]

    (when (and (not= false (wire-value raw "enabled"))
               (seq media-types)
               (every? pos-int? [max-files max-file-bytes max-video-bytes max-audio-bytes]))
      {:media-types (set media-types)
       :max-files (long max-files)
       :max-file-bytes (long max-file-bytes)
       :max-video-bytes (long max-video-bytes)
       :max-audio-bytes (long max-audio-bytes)})))

(defn- byte-limit
  [{:keys [max-file-bytes max-video-bytes max-audio-bytes]} media-type]
  (cond (attachments/audio-media-type? media-type) max-audio-bytes
        (attachments/video-media-type? media-type) max-video-bytes
        :else max-file-bytes))

(defn- content-id
  [^File file]
  (let [digest
        (MessageDigest/getInstance "SHA-256")

        buffer
        (byte-array 16384)]

    (with-open [in (FileInputStream. file)]
      (loop []

        (let [n (.read in buffer)]
          (when (pos? n) (.update digest buffer 0 n) (recur)))))
    (str "sha256:" (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) (.digest digest)))))

(defn- rejection [^File file reason] (str (.getName file) ": " reason))

(defn- stage-file
  [contract file]
  (let [^File file (if (instance? File file) file (File. (str file)))]
    (cond (not (.isFile file)) {:rejected (rejection file "file does not exist")}
          (not (.canRead file)) {:rejected (rejection file "file is not readable")}
          :else (if-let [media-type (attachments/sniff-file-mime file)]
                  (let [size (.length file)
                        limit (byte-limit contract media-type)]

                    (cond (not (contains? (:media-types contract) media-type))
                          {:rejected (rejection file (str "gateway does not accept " media-type))}
                          (> size (long limit))
                          {:rejected (rejection file
                                                (str (fmt/format-bytes size)
                                                     " is larger than the gateway limit of "
                                                     (fmt/format-bytes limit)))}
                          :else (let [{:keys [w h]} (or (terminal-image/probe-dimensions
                                                          (.getAbsolutePath file)
                                                          media-type)
                                                        {})]
                                  {:attachment (cond-> {:id (content-id file)
                                                        :path (.getCanonicalPath file)
                                                        :filename (.getName file)
                                                        :media-type media-type
                                                        :size size}
                                                 (and w h)
                                                 (assoc :width
                                                   w :height
                                                   h))})))
                  {:rejected (rejection file "unsupported file format")}))))

(defn admit-files
  "Stage `files` into the current composer attachment vector.

   This is the single route for picker, paste and drop intake. `capabilities` is
   the gateway response or its attachment feature map; no local defaults are
   substituted. Returns `{:attachments all :added newly-staged :rejected reasons}`.
   Duplicate bytes keep one stable identity and produce actionable feedback."
  [capabilities current files]
  (let [current (vec (or current []))]
    (if-let [contract (attachment-contract capabilities)]
      (let [result (reduce (fn [{:keys [attachments] :as result} file]
                             (if (>= (long (count attachments)) (long (:max-files contract)))
                               (update result
                                       :rejected
                                       conj
                                       (rejection (if (instance? File file) file (File. (str file)))
                                                  (str "limit of "
                                                       (:max-files contract)
                                                       " attachments reached")))
                               (let [{:keys [attachment rejected]} (stage-file contract file)]
                                 (cond rejected (update result :rejected conj rejected)
                                       (some #(= (:id attachment) (:id %)) attachments)
                                       (update result
                                               :rejected
                                               conj
                                               (str (:filename attachment) ": already attached"))
                                       :else (-> result
                                                 (update :attachments conj attachment)
                                                 (update :added conj attachment))))))
                           {:attachments current :added [] :rejected []}
                           files)]
        ;; A RECORDING starts becoming words the moment it is staged: local speech
        ;; is slow and the human is about to spend a minute typing, so the transcript
        ;; is normally in hand before the turn is sent — and the rail says "transcribing…"
        ;; meanwhile instead of nothing.
        (vis/audio-transcribe-request! (:added result))
        result)
      {:attachments current
       :added []
       :rejected
       ["Attachments are unavailable because the gateway did not advertise a complete attachment contract."]})))

(defn inline-payloads
  "Read staged files into the explicit inline gateway shape.

   The filesystem path is an intake-only implementation detail and never crosses the
   submission boundary. Reading happens once per gateway attempt owner, so transport
   retries reuse the same immutable base64 payload.

   A RECORDING carries the WORDS the composer already made for it. The staged row is
   keyed by its path and the payload by its bytes, so the transcript would otherwise
   be made twice — once here while the human typed, and again inside the turn they
   send. Sending it is also what keeps it: the transcript is stored with the
   attachment, not with this process."
  [staged]
  (mapv (fn [{:keys [path filename media-type] :as attachment}]
          (let [^File file
                (File. ^String path)

                words
                (:transcription (vis/audio-transcribe-outcome attachment))]

            (cond-> {:filename filename
                     :media-type media-type
                     :base64 (.encodeToString (Base64/getEncoder)
                                              (java.nio.file.Files/readAllBytes (.toPath file)))}
              (not-empty (str words))
              (assoc :transcription (str words)))))
        (or staged [])))

(defn remove-attachment
  "Remove exactly the staged attachment with stable `id`, preserving order."
  [current id]
  (into [] (remove #(= id (:id %))) (or current [])))
