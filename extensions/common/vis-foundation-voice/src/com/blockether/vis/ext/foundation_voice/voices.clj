(ns com.blockether.vis.ext.foundation-voice.voices
  "Reference clips somebody brought: the voices Vis did not ship.

   pocket-tts clones a RECORDING instead of selecting a baked speaker, so a
   voice in that engine IS a WAV file - which means a voice is something a
   person can make and hand to Vis, and no licence anywhere can stop them. This
   namespace is where such a clip lives: checked and normalized once on the way
   in, listed beside the clips the bundle carries, and readable by id from every
   surface (CLI, gateway, app).

   Imported clips deliberately do NOT live under the models directory. An asset
   is something Vis can fetch again; a recording is the user's own, and clearing
   a model cache must never take somebody's voice with it."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.foundation-voice.assets :as assets])
  (:import (java.io File)
           (java.nio ByteBuffer ByteOrder)
           (java.nio.charset StandardCharsets)
           (java.nio.file Files StandardCopyOption)))

(set! *warn-on-reflection* true)

(def voices-dir-env "VIS_VOICES_DIR")

(def ^:const max-clip-bytes
  "A reference clip is seconds of speech. Anything past this is a mistake — an
   album, a video's audio track — and it is refused BEFORE it is read into
   memory, because this path is reachable from an upload."
  (* 64 1024 1024))

(def ^:const max-clip-seconds
  "What is kept when a longer recording is imported. The model imitates the
   clip; ten seconds is plenty and a minute only makes every sentence slower to
   speak, so the tail is dropped rather than the import refused."
  30)

(def ^:const min-clip-seconds "Below this there is not enough voice in the recording to imitate." 1)

(defn voices-dir
  "Where imported clips live. A function, never a top-level `def`: `native-image`
   initializes this namespace at BUILD time, so a captured `user.home` would
   point every installed binary at the BUILDER's home."
  []
  (or (assets/env-value voices-dir-env) (str (System/getProperty "user.home") "/.vis/voices")))

(defn voice-id
  "The id a voice is addressed by, from whatever a human typed as its name.
   Lower case, one dash between words, nothing else: the id travels through a
   URL path, a CLI argument and a JSON field, and `nil` when nothing survives."
  [voice-name]
  (let [slug (-> (str voice-name)
                 str/trim
                 str/lower-case
                 (str/replace #"[^a-z0-9]+" "-")
                 (str/replace #"^-+" "")
                 (str/replace #"-+$" ""))]
    (when-not (str/blank? slug) (subs slug 0 (min 48 (count slug))))))

;; RIFF/WAVE - the one format read without help

(defn- four-cc
  [^bytes data ^long offset]
  (String. data (int offset) (int 4) StandardCharsets/US_ASCII))

(defn- little-endian
  ^ByteBuffer [^bytes data]
  (doto (ByteBuffer/wrap data) (.order ByteOrder/LITTLE_ENDIAN)))

(defn wav?
  "Is this file a RIFF/WAVE container at all? Cheap enough to ask before
   deciding whether a converter is needed."
  [^File file]
  (and (.isFile file)
       (> (.length file) 44)
       (with-open [in (io/input-stream file)]
         (let [head (byte-array 12)]
           (and (= 12 (.read in head 0 12))
                (= "RIFF" (four-cc head 0))
                (= "WAVE" (four-cc head 8)))))))

(defn- unsupported-encoding!
  [detail]
  (throw (ex-info (str "That recording is " detail
                       ", and Vis reads 16-bit PCM WAV. Convert it first:"
                       " ffmpeg -i <file> -ac 1 -c:a pcm_s16le clip.wav")
                  {:type :voice-tts/clip-unsupported-encoding :detail detail})))

(defn decode-wav
  "A 16-bit PCM RIFF/WAVE file as `{:sample-rate :channels :samples}`, where
   `:samples` is the interleaved short array. Chunks are walked rather than
   assumed at offset 36: a recorder that writes `LIST` or `fact` before `data`
   is still an ordinary WAV file."
  [^File file]
  (let [data
        (Files/readAllBytes (.toPath file))

        len
        (alength ^bytes data)

        buf
        (little-endian data)]

    (when (or (< len 44) (not= "RIFF" (four-cc data 0)) (not= "WAVE" (four-cc data 8)))
      (throw (ex-info "That file is not a RIFF/WAVE recording"
                      {:type :voice-tts/clip-not-wav :path (str file)})))
    (loop [pos
           12

           tag
           0

           channels
           0

           rate
           0

           bits
           0

           audio
           nil]

      (if (> (+ pos 8) len)
        (do (when (zero? rate) (unsupported-encoding! "a WAV file with no format chunk"))
            (when-not (or (= 1 tag) (= 0xFFFE tag))
              (unsupported-encoding! (str "encoded as WAV format " tag)))
            (when-not (= 16 bits) (unsupported-encoding! (str bits "-bit audio")))
            (when (or (nil? audio) (zero? (alength ^shorts audio)))
              (unsupported-encoding! "a WAV file with no audio in it"))
            {:sample-rate rate :channels (max 1 channels) :samples audio})
        (let [id
              (four-cc data pos)

              size
              (bit-and (long (.getInt buf (+ pos 4))) 0xFFFFFFFF)

              body
              (+ pos 8)

              size
              (min size (- len body))

              next-pos
              (+ body size (if (odd? size) 1 0))]

          (cond (= "fmt " id) (recur next-pos
                                     (bit-and (long (.getShort buf body)) 0xFFFF)
                                     (bit-and (long (.getShort buf (+ body 2))) 0xFFFF)
                                     (long (.getInt buf (+ body 4)))
                                     (bit-and (long (.getShort buf (+ body 14))) 0xFFFF)
                                     audio)
                (= "data" id) (let [frames
                                    (quot size 2)

                                    shorts
                                    (short-array frames)]

                                (dotimes [i frames]
                                  (aset shorts i (.getShort buf (+ body (* 2 i)))))
                                (recur next-pos tag channels rate bits shorts))
                :else (recur next-pos tag channels rate bits audio)))))))

(defn- mono-16
  "One channel, because a reference clip is one voice: the channels of a frame
   are averaged rather than one of them dropped, so a recording panned to one
   side does not import as silence. Trimmed to [[max-clip-seconds]]."
  ^shorts [{:keys [channels sample-rate ^shorts samples]}]
  (let [channels
        (long (max 1 (long channels)))

        frames
        (long (quot (alength samples) channels))

        kept
        (long (min frames (* (long sample-rate) (long max-clip-seconds))))

        out
        (short-array kept)]

    (dotimes [i kept]
      (let [base (* i channels)]
        (aset out
              i
              (short (quot (long (reduce +
                                         (map #(long (aget samples (+ base (long %))))
                                              (range channels))))
                           channels)))))
    out))

(defn- write-wav!
  "Write mono 16-bit PCM. The header is built here rather than by a library
   because it is 44 bytes and this is the only place Vis writes one."
  [^File file ^shorts samples ^long sample-rate]
  (let [frames
        (alength samples)

        payload
        (* 2 frames)

        buf
        (little-endian (byte-array (+ 44 payload)))]

    (.put buf (.getBytes "RIFF" StandardCharsets/US_ASCII))
    (.putInt buf (int (+ 36 payload)))
    (.put buf (.getBytes "WAVEfmt " StandardCharsets/US_ASCII))
    (.putInt buf (int 16))
    (.putShort buf (short 1))
    (.putShort buf (short 1))
    (.putInt buf (int sample-rate))
    (.putInt buf (int (* sample-rate 2)))
    (.putShort buf (short 2))
    (.putShort buf (short 16))
    (.put buf (.getBytes "data" StandardCharsets/US_ASCII))
    (.putInt buf (int payload))
    (dotimes [i frames]
      (.putShort buf (aget samples i)))
    (io/copy (.array buf) file)))

;; The store

(defn- clip-file ^File [id] (io/file (voices-dir) (str (name id) ".wav")))

(defn- meta-file ^File [id] (io/file (voices-dir) (str (name id) ".edn")))

(defn- read-meta
  "What was recorded about a clip when it was imported. A clip whose sidecar is
   missing or unreadable is still a voice - the WAV is the voice - so the id
   stands in for everything else rather than the whole catalogue failing."
  [id]
  (let [f
        (meta-file id)

        stored
        (when (.isFile f) (try (edn/read-string (slurp f)) (catch Throwable _ nil)))]

    (merge {:id id :label id} (when (map? stored) stored))))

(defn imported-voice
  "The imported voice with `id`, or nil. `:clip` is the absolute path of the
   recording, which is what the engine needs and what nobody else should read."
  [id]
  (let [id
        (some-> id
                name
                not-empty)

        f
        (some-> id
                clip-file)]

    (when (and f (.isFile ^File f))
      (assoc (read-meta id)
        :id id
        :clip (str f)
        :is-imported true))))

(defn imported
  "Every imported voice, by id. The catalogue is DERIVED from the directory, so
   a clip dropped in by hand is a voice and a deleted one stops being one."
  []
  (let [files (or (.listFiles (io/file (voices-dir))) (make-array File 0))]
    (->> files
         (map #(.getName ^File %))
         (filter #(str/ends-with? % ".wav"))
         (map #(subs % 0 (- (count %) 4)))
         sort
         (keep imported-voice)
         vec)))

(defn- source-clip!
  "The recording to import, as a 16-bit PCM WAV Vis can read. A container Vis
   does not decode is handed to ffmpeg when the machine has it - a phone records
   .m4a, and refusing that outright would be refusing the most likely file."
  [^File file]
  (if (wav? file)
    (decode-wav file)
    (let [ffmpeg
          (some #(let [f (io/file % "ffmpeg")] (when (.canExecute f) (str f)))
                (str/split (str (System/getenv "PATH")) (re-pattern File/pathSeparator)))

          converted
          (File/createTempFile "vis-voice-import" ".wav")]

      (when-not ffmpeg
        (.delete converted)
        (throw (ex-info (str "That recording is not a WAV file, and this machine has no ffmpeg"
                             " to convert it. Convert it first:"
                             " ffmpeg -i "
                             (.getName file)
                             " -ac 1 -c:a pcm_s16le clip.wav")
                        {:type :voice-tts/clip-not-wav :path (str file)})))
      (try (let [^java.util.List command
                 [ffmpeg "-v" "error" "-y" "-i" (str file) "-ac" "1" "-c:a" "pcm_s16le"
                  (str converted)]

                 ^Process process
                 (.start (doto (ProcessBuilder. command) (.redirectErrorStream true)))

                 output
                 (slurp (.getInputStream process))

                 code
                 (.waitFor process)]

             (when-not (zero? code)
               (throw (ex-info (str "ffmpeg could not read that recording: " (str/trim output))
                               {:type :voice-tts/clip-unreadable :exit code})))
             (decode-wav converted))
           (finally (.delete converted))))))

(defn import!
  "Take the recording at `:path` as a voice and return what it became.

   `:voice-name` is what a human typed and only decides the id; `:language` and
   `:text` are optional. `:text` is the TRANSCRIPT of the clip, and it is worth
   supplying: pocket-tts is given the reference audio AND what it says, and a
   clone tracks the voice far better when it is not guessing at the words.

   The recording is normalized once, here - one channel, 16-bit, its own sample
   rate kept - so that everything downstream reads one shape and the engine
   never meets a file format at synthesis time."
  [{:keys [path voice-name language text]}]
  (let [file
        (io/file (str path))

        stem
        (when (.isFile file) (str/replace (.getName file) #"\.[A-Za-z0-9]+$" ""))

        id
        (or (voice-id voice-name) (voice-id stem))]

    (when-not (.isFile file)
      (throw (ex-info (str "No recording at " path)
                      {:type :voice-tts/clip-missing :path (str path)})))
    (when (> (.length file) (long max-clip-bytes))
      (throw (ex-info (str "That recording is "
                           (quot (.length file) (* 1024 1024))
                           " MB. A reference clip is seconds of speech, not a file this size.")
                      {:type :voice-tts/clip-too-large :bytes (.length file)})))
    (when-not id
      (throw (ex-info "A voice needs a name with a letter or a digit in it"
                      {:type :voice-tts/voice-name-invalid :name voice-name})))
    (let [decoded
          (source-clip! file)

          samples
          (mono-16 decoded)

          rate
          (long (:sample-rate decoded))

          seconds
          (/ (double (alength ^shorts samples)) (double rate))]

      (when (< seconds (double min-clip-seconds))
        (throw (ex-info (format "That recording is %.1f seconds long: too short to imitate a voice"
                                seconds)
                        {:type :voice-tts/clip-too-short :seconds seconds})))
      (io/make-parents (clip-file id))
      (let [staged
            (File/createTempFile "vis-voice" ".wav" (io/file (voices-dir)))

            voice
            (cond-> {:id id
                     :label (or (not-empty (str/trim (str voice-name))) id)
                     :sample-rate rate
                     :seconds (/ (Math/round (* 10.0 seconds)) 10.0)
                     :imported-at (str (java.time.Instant/now))}
              (not-empty (str language))
              (assoc :language (str/trim (str language)))

              (not-empty (str text))
              (assoc :clip-text (str/trim (str text))))]

        (write-wav! staged samples rate)
        (Files/move (.toPath staged)
                    (.toPath (clip-file id))
                    ^"[Ljava.nio.file.CopyOption;"
                    (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
        (spit (meta-file id) (pr-str voice))
        (assoc voice
          :clip (str (clip-file id))
          :is-imported true)))))

(defn forget!
  "Delete an imported voice. True when there was one to delete: the clip is the
   voice, so removing the file removes it from every catalogue at once."
  [id]
  (let [id
        (some-> id
                name
                not-empty)

        clip
        (some-> id
                clip-file)]

    (boolean (when (and clip (.isFile ^File clip)) (.delete (meta-file id)) (.delete ^File clip)))))
