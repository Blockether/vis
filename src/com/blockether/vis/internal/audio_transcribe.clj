(ns com.blockether.vis.internal.audio-transcribe
  "Borrowed EARS: a recording somebody attached, as text.

   No provider wire carries audio. A voice memo dropped into a message is therefore
   stored, played back for the human, and NAMED to the model — which is the same
   dead end a blind model meets in front of a screenshot, and the reason
   `vision-describe` exists. The answer here is the same shape, with one difference
   that decides everything about where it runs: the transcript is not a second-hand
   report bought from another provider, it is the recording's own WORDS, produced
   locally by the speech engine this build already carries
   (`com.blockether.vis.internal.voice`, normally Parakeet on this machine). It costs
   no quota, it leaves no bytes on anybody's wire, and it is true of the file forever.

   So it is computed ONCE, at the turn that carried the recording, and travels with
   the attachment: the model reads it in the manifest, the human reads it under the
   player, and a resumed session re-renders both from the same stored string.

   Three properties keep it affordable:

   - CONTENT-KEYED. Attachments replay on every later request of the session, so the
     digest of the bytes — not the position, not the filename — is the cache key, and
     a memo is transcribed exactly once per process.
   - NEVER BLOCKING THE TURN. The engine runs on a worker with a deadline; a model
     still downloading, a wedged native call or a two-hour recording gives back nil
     and the turn proceeds with today's behaviour.
   - TOTAL. Every failure is nil. A recording that could not be transcribed is still
     stored, still played, and still named to the model.

   A LEAF: attachments + voice + toggles, never back on the loop."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.voice :as voice]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Base64]))

(set! *warn-on-reflection* true)

(def TOGGLE_ID
  "Feature toggle gating attachment transcription (registered in `toggles`)."
  "audio_transcribe_attachments")

(def ^:private ^:const MAX_TRANSCRIBED_PER_PASS
  "How many UNCACHED recordings one turn may transcribe. The cache makes the steady
   state free, so this only bounds a message that arrives carrying a pile of memos."
  4)

(def ^:private ^:const MAX_TRANSCRIPT_CHARS
  "Cap on ONE recording's transcript, in characters. It is quoted into the manifest
   and then into every later request of the session, so an hour of speech must not
   be able to spend the whole context of a turn."
  8000)

(def ^:private ^:const TRANSCRIBE_DEADLINE_MS
  "Wall-clock cap on ONE recording. Local speech is real work — several seconds for a
   minute of audio — but this runs INSIDE the turn, so a wedged native call has to be
   abandoned rather than parking the human's request."
  120000)

(def ^:private ^:const MAX_CACHE_ENTRIES 64)

(defn enabled? "Whether attachment transcription may run at all." [] (toggles/enabled? TOGGLE_ID))

(defn engine
  "The transcription engine this build would use, or nil when none is registered — a
   build without the voice extension, or one whose engine failed to load."
  []
  (try (voice/resolve-engine :transcribe nil) (catch Throwable _ nil)))

(defn available?
  "Whether a recording attached RIGHT NOW would be transcribed: the toggle is on, an
   engine is registered, and that engine can take work (a model still downloading is
   not a failure — it is a later turn)."
  []
  (boolean (and (enabled?)
                (some-> (engine)
                        voice/ready?))))

(defonce ^:private transcript-cache
  ;; {content-digest "the words"} — a recording replays on every request of the
  ;; session, so it is transcribed once per process.
  (atom {}))

(defn clear-cache! "Drop every memoized transcript. Tests only." [] (reset! transcript-cache {}))

(defn- content-digest
  "Cache key: the payload's own bytes plus the container they ride in."
  [{:keys [base64 media-type path]}]
  (let [md
        (MessageDigest/getInstance "SHA-256")

        digest
        (.digest md
                 (.getBytes (str media-type "|" (or (not-empty (str base64)) (str path)))
                            StandardCharsets/UTF_8))]

    (.encodeToString (Base64/getUrlEncoder) digest)))

(defn- cache-put!
  [k text]
  (swap! transcript-cache (fn [cache]
                            (assoc (if (>= (count cache) (long MAX_CACHE_ENTRIES)) {} cache)
                              k text)))
  text)

(defn- clip-extension
  "The suffix a temp copy is written under. The engine sniffs the container itself,
   but a decoder invoked by extension (ffmpeg among them) reads better when the name
   agrees with the bytes."
  [{:keys [filename media-type]}]
  (or (some->> (str filename)
               (re-find #"\.([A-Za-z0-9]{1,5})$")
               second
               str/lower-case
               (str "."))
      (some->> (str media-type)
               (re-find #"^audio/([A-Za-z0-9]+)$")
               second
               str/lower-case
               (str "."))
      ".audio"))

(defn- source-file
  "The recording on disk as `{:file :is-temp}`. A terminal drop already IS a file and
   is read where it lies; an upload carries only base64 and is spilled to a temp file,
   because every speech engine takes a PATH."
  [{:keys [path base64] :as attachment}]
  (let [^String on-disk-path
        (not-empty (str path))

        ^File on-disk
        (when on-disk-path (File. on-disk-path))]

    (if (and on-disk (.isFile on-disk))
      {:file on-disk :is-temp false}
      (when-let [payload (not-empty (str base64))]
        (let [bytes (.decode (Base64/getDecoder) ^String payload)
              temp (File/createTempFile "vis-recording" (clip-extension attachment))]

          (with-open [out (java.io.FileOutputStream. temp)]
            (.write out ^bytes bytes))
          {:file temp :is-temp true})))))

(defn- run-engine
  "One transcription on a worker, abandoned at the deadline. The worker is what makes
   `TRANSCRIBE_DEADLINE_MS` real: `deref` with a timeout cannot interrupt the native
   call, so the future is cancelled and the turn walks away from it."
  [^File file]
  (let [work
        (future (voice/transcribe! {:audio-path (str file)}))

        answer
        (deref work TRANSCRIBE_DEADLINE_MS ::timeout)]

    (if (= ::timeout answer) (do (future-cancel work) nil) answer)))

(defn transcribe-attachment
  "The words in ONE recording, or nil. Cached by content, bounded by the deadline, and
   TOTAL: an engine that is absent, busy, downloading or broken answers nil."
  [attachment]
  (let [k (content-digest attachment)]
    (if (contains? @transcript-cache k)
      (get @transcript-cache k)
      (let [{:keys [^File file is-temp]} (try (source-file attachment) (catch Throwable _ nil))]
        (when file
          (try (let [text (some-> (run-engine file)
                                  str
                                  str/trim
                                  not-empty)]
                 (cache-put! k
                             (some-> text
                                     (subs 0 (min (count text) (long MAX_TRANSCRIPT_CHARS))))))
               (catch Throwable t
                 (tel/log! {:level :warn
                            :id ::transcribe-failed
                            :data {:error (ex-message t) :filename (:filename attachment)}})
                 nil)
               (finally (when is-temp (.delete file)))))))))

(defn transcribe-attachments
  "`attachments` with every RECORDING carrying its own `:transcription`.

   The one call every surface makes. Attachments that are not audio, a build with no
   speech engine, a model still downloading and a recording that already carries a
   transcript all come back untouched, so this is safe to call on any list — the same
   list, in the same order, is what comes out."
  [attachments]
  (let [rows (vec (or attachments []))]
    (if-not (and (seq rows) (available?))
      rows
      (let [budget (volatile! (long MAX_TRANSCRIBED_PER_PASS))]
        (mapv (fn [{:keys [media-type transcription] :as attachment}]
                (if-not (and (attachments/audio-media-type? media-type)
                             (str/blank? (str transcription))
                             (pos? (long @budget)))
                  attachment
                  (do (vswap! budget
                              (fn [n]
                                (dec (long n))))
                      (if-let [text (transcribe-attachment attachment)]
                        (assoc attachment :transcription text)
                        attachment))))
              rows)))))
