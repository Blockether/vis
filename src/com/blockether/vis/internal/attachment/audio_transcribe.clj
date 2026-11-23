(ns com.blockether.vis.internal.attachment.audio-transcribe
  "Borrowed EARS: a recording somebody attached, as text.

   No provider wire carries audio. A voice memo dropped into a message is therefore
   stored, played back for the human, and NAMED to the model — which is the same
   dead end a blind model meets in front of a screenshot, and the reason
   `vision-describe` exists. The answer here is the same shape, with one difference
   that decides everything about where it runs: the transcript is not a second-hand
   report bought from another provider, it is the recording's own WORDS, produced
   locally by the speech engine this build already carries
   (`com.blockether.vis.internal.speech.core`, normally Parakeet on this machine). It costs
   no quota, it leaves no bytes on anybody's wire, and it is true of the file forever.

   WHEN it runs is the whole design. A surface that STAGES a recording — the composer
   rail, an upload, the gateway's own intake — calls [[request!]] the moment the file
   arrives and paints [[outcome]] while the human is still typing. By the time the
   turn is sent the words are normally already in hand; a turn that finds the work
   still running JOINS it before the model is asked. A long recording must not
   become a request that silently omits the user's words. Waiting callers may
   cancel without abandoning the shared transcription worker.

   Four properties keep it affordable and honest:

   - CONTENT-KEYED. Attachments replay on every later request of the session, so the
     digest of the bytes — not the position, not the filename — is the registry key,
     and a memo is transcribed exactly once per process.
   - ONE AT A TIME. Local speech saturates a core, so five memos in one message queue
     on one daemon worker rather than starting five decoders at once.
   - NEVER SILENT. Every miss is an OUTCOME carrying a reason — `pending`,
     `unavailable`, `silent` ([[statuses]]) — logged once and spelled on the wire as
     `transcription_status`, because a blank band under a player must never be
     indistinguishable from a recording that had no words in it. A failure is
     remembered as a FAILURE and never as \"no words\"; a reason that describes this
     moment rather than the file (a model still downloading, a toggle that is off) is
     not remembered at all.
   - TOTAL. No failure escapes: a recording that could not be transcribed is still
     stored, still played, and still named to the model.

   A LEAF: attachments + voice + toggles, never back on the loop."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.attachment.core :as attachments]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.speech.core :as speech]
            [taoensso.telemere :as tel])
  (:import [java.io File FileOutputStream]
           [java.nio.charset StandardCharsets]
           [java.util Base64]
           [java.util.concurrent Callable ExecutorService Executors]))

(set! *warn-on-reflection* true)

(def TOGGLE_ID
  "Feature toggle gating attachment transcription (registered in `toggles`)."
  "audio_transcribe_attachments")

(def PENDING
  "`transcription_status` while the words are being made — what a composer paints as
   a placeholder under the file it has just staged."
  "pending")

(def UNAVAILABLE
  "`transcription_status` when this machine could not produce the words at all: no
   engine, a refused container, a throw. It is NOT \"the recording is empty\"."
  "unavailable")

(def SILENT
  "`transcription_status` when the engine read the whole recording and found no
   speech in it. A fact about the audio, not about the machine."
  "silent")

(def statuses
  "The CLOSED vocabulary of what a surface may be told about a recording whose words
   it does not have. A row that carries `:transcription` carries no status at all."
  #{PENDING UNAVAILABLE SILENT})

(def ^:private ^:const MAX_REGISTRY_ENTRIES 64)

(def MAX_PAYLOAD_BYTES
  "The largest recording this machine will read: about an HOUR of speech.

   Local transcription costs roughly one core for the length of the audio, so
   duration is the whole price — and a recording longer than the sitting it came
   from left the single worker busy while every later turn asked again and was
   handed `pending` forever. An hour of speech-grade audio is around 30 MB; the
   cap sits at 48 MiB so an hour recorded at a generous bitrate is never refused
   for its bitrate, and anything past it settles as `unavailable` with a reason
   the log can name instead of waiting for a transcript nobody will see."
  (* 48 1024 1024))

(defn enabled? "Whether attachment transcription may run at all." [] (toggles/enabled? TOGGLE_ID))

(defn engine
  "The built-in transcription engine, or nil when its native runtime failed to load."
  []
  (try (speech/resolve-engine :transcribe nil) (catch Throwable _ nil)))

(defn available?
  "Whether a recording attached RIGHT NOW would be transcribed: the toggle is on, an
   engine is registered, and that engine can take work (a model still downloading is
   not a failure — it is a later turn)."
  []
  (boolean (and (enabled?)
                (some-> (engine)
                        speech/ready?))))

(defonce ^:private work*
  ;; {content-digest {:outcome {…}}} for what is known, {content-digest {:result
  ;; promise :started-at ms}} for what is being made. One entry per recording, so
  ;; the atom is also the lock that keeps two surfaces from transcribing the same
  ;; bytes twice.
  (atom {}))

(defonce ^:private recording-keys (atom {}))

(defonce ^:private progressed-at
  ;; `System/nanoTime` of the worker's last sign of life: a job start, a progress
  ;; report or an outcome. One worker serves every recording, so a report on any of
  ;; them also moves the queue that a waiting recording sits in.
  (atom (System/nanoTime)))

(defonce ^:private worker
  ;; ONE daemon thread: local speech saturates a core, and a message carrying five
  ;; memos must queue rather than race. Delayed so a build that never transcribes
  ;; never starts it.
  (delay (Executors/newSingleThreadExecutor (util/daemon-thread-factory "vis-audio-transcribe"))))

(defn clear-cache!
  "Drop every transcript and every in-flight job. Tests only."
  []
  (reset! recording-keys {})
  (reset! work* {}))

(defn- content-digest
  "Registry key: the payload, independent of an intake correction to its MIME type."
  [{:keys [base64 path]}]
  (let [digest (util/sha256 (util/utf8 (or (not-empty (str base64)) (str path))))]
    (.encodeToString (Base64/getUrlEncoder) digest)))

(defn- ascii-at?
  "Does `buffer` spell `text` at `offset`? The cheap half of container sniffing."
  [^bytes buffer ^String text ^long offset]
  (let [want (.getBytes text StandardCharsets/US_ASCII)]
    (and (>= (alength buffer) (+ offset (alength want)))
         (loop [i 0]
           (cond (= i (alength want)) true
                 (= (aget buffer (+ offset i)) (aget want i)) (recur (inc i))
                 :else false)))))

(defn- mp3-frame?
  "MPEG audio frame sync — eleven set bits — for an .mp3 that carries no ID3 tag."
  [^bytes buffer]
  (and (>= (alength buffer) 2) (= -1 (aget buffer 0)) (= 0xE0 (bit-and (aget buffer 1) 0xE0))))

(defn container-extension
  "The suffix a temp copy is written under, decided by what the bytes ARE.

   The name is the one thing a phone gets wrong: a shared iPhone memo is AAC in an
   MP4 box called `.mp3`, and a decoder that trusts the suffix reads the wrong
   format or refuses the file. So the magic answers first, the declared media type
   second, and the filename — the only one of the three nobody verifies — last."
  [^bytes buffer media-type filename]
  (or (when (and buffer (>= (alength buffer) 12))
        (cond (and (ascii-at? buffer "RIFF" 0) (ascii-at? buffer "WAVE" 8)) ".wav"
              (ascii-at? buffer "ftyp" 4) ".m4a"
              (ascii-at? buffer "OggS" 0) ".ogg"
              (ascii-at? buffer "fLaC" 0) ".flac"
              (ascii-at? buffer "ID3" 0) ".mp3"
              (ascii-at? buffer "#!AMR" 0) ".amr"
              (and (ascii-at? buffer "FORM" 0) (ascii-at? buffer "AIFF" 8)) ".aiff"
              (mp3-frame? buffer) ".mp3"
              :else nil))
      (some->> (str media-type)
               (re-find #"^audio/(?:x-)?([A-Za-z0-9]+)$")
               second
               str/lower-case
               (str "."))
      (some->> (str filename)
               (re-find #"\.([A-Za-z0-9]{1,5})$")
               second
               str/lower-case
               (str "."))
      ".audio"))

(defn- source-file
  "The recording on disk as `{:file :is-temp}`. A terminal drop already IS a file and
   is read where it lies; an upload carries only base64 and is spilled to a temp file
   named after its own magic, because every speech engine takes a PATH."
  [{:keys [path base64 media-type filename]}]
  (let [^String on-disk-path
        (not-empty (str path))

        ^File on-disk
        (when on-disk-path (File. on-disk-path))]

    (if (and on-disk (.isFile on-disk))
      {:file on-disk :is-temp false}
      (when-let [payload (not-empty (str base64))]
        (let [buffer (.decode (Base64/getDecoder) ^String payload)
              temp (File/createTempFile "vis-recording"
                                        (container-extension buffer media-type filename)
                                        (paths/temp-dir))]

          (with-open [out (FileOutputStream. temp)]
            (.write out ^bytes buffer))
          {:file temp :is-temp true})))))

(defn- unavailable [reason] {:status UNAVAILABLE :reason reason})

(defn- keeps?
  "Is this outcome a fact about the RECORDING (remember it) rather than about this
   moment (ask again next time)? A model still downloading, a toggle that is off and
   a pass that spent its budget all change without the file changing — and none of
   them may ever be remembered as \"this recording has no words\"."
  [{:keys [reason]}]
  (not (contains? #{:disabled :no-engine :not-ready :budget :deadline} reason)))

(defn- log-outcome!
  "One line per recording, whichever way it went. The point of the namespace's NEVER
   SILENT rule: before this, an absent engine, a refused container and a two-hour
   clip were all the same nothing in the log."
  [attachment outcome ^long started-at]
  (let [ms (- (util/now-ms) started-at)]
    (if-let [text (:transcription outcome)]
      (tel/log! {:level :info
                 :id ::transcribed
                 :data {:filename (:filename attachment) :chars (count text) :ms ms}
                 :msg "transcribed an attached recording"})
      (tel/log! {:level :warn
                 :id ::transcribe-unavailable
                 :data {:filename (:filename attachment)
                        :media-type (:media-type attachment)
                        :status (:status outcome)
                        :reason (:reason outcome)
                        :ms ms}
                 :msg "an attached recording produced no transcript"}))))

(defn- note-progress!
  "Record that the worker is alive, and the percent of recording `k` when the
   engine gave one. Only work that is still running keeps a percent."
  [k percent]
  (reset! progressed-at (System/nanoTime))
  (when percent
    (swap! work* (fn [registry]
                   (if (get-in registry [k :result])
                     (assoc-in registry [k :progress] (-> (long percent) (max 0) (min 100)))
                     registry)))))

(defn- transcribe-now
  "Run ONE recording through the engine and answer its outcome. Total: an unreadable
   payload, a throw and a recording with no speech in it are all outcomes.
   `on-progress` receives the percent transcribed, or nil while the engine prepares."
  [attachment on-progress]
  (let [started (util/now-ms)]
    (if-let [{:keys [^File file is-temp]} (try (source-file attachment)
                                               (catch Throwable t
                                                 (tel/log! {:level :warn
                                                            :id ::recording-unreadable
                                                            :data {:filename (:filename attachment)
                                                                   :error (ex-message t)}})
                                                 nil))]
      (try (let [{:keys [text segments]} (speech/transcribe!
                                         {:audio-path (str file)
                                          :on-progress (fn [{:keys [phase progress]}]
                                                         (on-progress (when (= :transcribing phase)
                                                                        progress)))})
                 words (some-> text
                               str
                               str/trim
                               not-empty)
                 outcome (if words
                           ;; The words, and — when the engine timed them — WHERE each line of
                           ;; them is spoken, so a player can follow the transcript it shows.
                           (cond-> {:transcription words}
                             (seq segments)
                             (assoc :transcription-segments (vec segments)))
                           {:status SILENT :reason :no-speech})]

             (log-outcome! attachment outcome started)
             outcome)
           (catch Throwable t
             (let [outcome (unavailable :failed)]
               (tel/log! {:level :warn
                          :id ::transcribe-failed
                          :data {:error (ex-message t) :filename (:filename attachment)}})
               (log-outcome! attachment outcome started)
               outcome))
           (finally (when is-temp (.delete file))))
      (let [outcome (unavailable :unreadable)]
        (log-outcome! attachment outcome started)
        outcome))))

(defn- prune
  "Bound the registry without ever dropping work that is still running: a full table
   forgets what it has already answered, which costs at most one re-transcription of
   a recording nobody has looked at in 64 attachments."
  [registry]
  (if (>= (count registry) (long MAX_REGISTRY_ENTRIES))
    (into {} (remove (comp :outcome val)) registry)
    registry))

(defn- settle!
  "Remember `outcome` for `k`, or forget the entry when the outcome describes this
   moment rather than the file ([[keeps?]])."
  [k outcome]
  (swap! work* (fn [registry]
                 (if (keeps? outcome)
                   (assoc (prune registry) k {:outcome outcome})
                   (dissoc registry k))))
  outcome)

(defn- start!
  "Ensure the work for `k` is on the worker and answer its registry entry. The atom
   is the lock — the caller whose swap CREATED the entry is the one that submits, so
   two surfaces asking at once still transcribe the bytes once."
  [k attachment]
  (let [[before after]
        (swap-vals! work*
                    (fn [registry]
                      (if (get registry k)
                        registry
                        (assoc (prune registry) k {:started-at (util/now-ms) :result (promise)}))))

        entry
        (get after k)]

    (when-not (get before k)
      (.submit ^ExecutorService @worker
               ^Callable
               (fn []
                 (note-progress! k nil)
                 (let [outcome (transcribe-now attachment #(note-progress! k %))]
                   (settle! k outcome)
                   (deliver (:result entry) outcome)
                   (note-progress! k nil)
                   outcome))))
    entry))

(defn- payload-bytes
  "How big this recording IS, without decoding it: base64 spends four characters on
   every three bytes, and a terminal drop is already a file to stat."
  ^long [{:keys [base64 path]}]
  (let [payload (str base64)]
    (if-not (str/blank? payload)
      (quot (* 3 (long (count payload))) 4)
      (if-let [^String on-disk (not-empty (str path))]
        (let [file (File. on-disk)]
          (if (.isFile file) (.length file) 0))
        0))))

(defn- readiness
  "What is already decided before any engine is asked. `::ok` means \"go ahead\"; nil
   means the attachment is not a recording and has no transcription outcome at all;
   anything else IS the outcome — it already carries words, or this machine cannot
   make any right now."
  [{:keys [media-type transcription transcription-segments]}]
  (cond (not (attachments/audio-media-type? media-type)) nil
        (not (str/blank? (str transcription))) (cond-> {:transcription (str transcription)}
                                                 (seq transcription-segments)
                                                 (assoc :transcription-segments
                                                   (vec transcription-segments)))
        (not (enabled?)) (unavailable :disabled)
        (nil? (engine)) (unavailable :no-engine)
        (not (available?)) (unavailable :not-ready)
        :else ::ok))

(defn- payload-gate [attachment]
  (cond (str/blank? (str (or (:base64 attachment) (:path attachment)))) (unavailable :unreadable)
        (> (payload-bytes attachment) (long MAX_PAYLOAD_BYTES)) (unavailable :too-long)
        :else ::ok))

(defn- gate [attachment]
  (let [answer (readiness attachment)]
    (if (= ::ok answer) (payload-gate attachment) answer)))

(defn- entry-outcome [entry]
  (cond (nil? entry) nil
        (:outcome entry) (:outcome entry)
        (realized? (:result entry)) @(:result entry)
        :else {:status PENDING :reason :running}))

(defn outcome
  "The cached transcription or status of this recording, without starting work."
  [attachment]
  (entry-outcome (get @work* (content-digest attachment))))

(defn- remember-recording! [id k]
  (when id
    (swap! recording-keys
           (fn [keys]
             (assoc (if (or (contains? keys id) (< (count keys) (long MAX_REGISTRY_ENTRIES)))
                      keys
                      {})
                    id k)))))

(defn request!
  "Start a recording in the background and return its current transcription outcome.
   Pass `load-base64` only for verified stored rows. It reads an inline payload on a cache miss."
  ([attachment] (request! attachment nil))
  ([{:keys [id base64 path size storage-uri] :as attachment} load-base64]
   (let [gated (readiness attachment)
         stored-id (when (and load-base64 (not (or base64 path storage-uri))) id)]
     (cond (not= ::ok gated) gated
           (and stored-id (number? size) (> (long size) (long MAX_PAYLOAD_BYTES)))
           (unavailable :too-long)
           :else
           (or (when-let [k (get @recording-keys stored-id)]
                 (entry-outcome (get @work* k)))
               (let [attachment (if stored-id
                                  (assoc attachment :base64 (load-base64 attachment))
                                  attachment)
                     check (payload-gate attachment)]
                 (if (not= ::ok check)
                   check
                   (let [k (content-digest attachment)
                         entry (start! k attachment)]
                     (remember-recording! stored-id k)
                     (entry-outcome entry)))))))))

(defn transcribe-attachment
  "Wait for this recording's outcome. Cancellation stops only the waiter. An optional
   `:stall-ms` returns pending when the shared worker reports no progress for that
   long, so a long recording that keeps moving is never cut off. `:on-progress`
   receives this recording's percent (nil while it waits or prepares) each time it
   changes. The shared worker continues so its eventual result can still be stored
   and displayed."
  [attachment & [{:keys [cancelled? stall-ms on-progress]}]]
  (let [gated (gate attachment)]
    (if (not= ::ok gated)
      ;; A refusal this side of the engine is exactly the silence turn 35 could not
      ;; explain: no engine, a toggle somebody turned off, a model still downloading.
      ;; It is logged HERE, once per recording per turn, and nowhere else.
      (do (when (:status gated) (log-outcome! attachment gated (util/now-ms))) gated)
      (let [k (content-digest attachment)
            {:keys [outcome result]} (start! k attachment)]

        (or outcome
            (if-not (or cancelled? stall-ms on-progress)
              @result
              (let [started (System/nanoTime)]
                (loop [reported ::none]

                  (when (and cancelled? (cancelled?))
                    (throw (InterruptedException.
                             "Waiting for recording transcription was cancelled")))
                  (let [percent
                        (get-in @work* [k :progress])

                        _
                        (when (and on-progress (not= reported percent)) (on-progress percent))

                        wait-ms
                        (if stall-ms
                          (let [deadline (+ (max started (long @progressed-at))
                                            (* 1000000 (long stall-ms)))]
                            (min 250 (max 0 (quot (- deadline (System/nanoTime)) 1000000))))
                          250)

                        answer
                        (deref result wait-ms ::waiting)]

                    (cond (not= ::waiting answer) answer
                          (zero? wait-ms) {:status PENDING}
                          :else (recur percent)))))))))))

(defn awaits-words?
  "Is this attachment a recording that has no words yet?"
  [{:keys [media-type transcription]}]
  (and (attachments/audio-media-type? media-type) (str/blank? (str transcription))))

(defn- walk-recordings
  "`attachments` with `answer-for` applied to every RECORDING that has no words yet,
   and everything else left exactly as it came. This is what makes both plural calls
   safe on any list: a picture, a document and a memo that already carries its
   transcript are never touched, and the list comes back in its own order."
  [attachments answer-for]
  (let [rows (vec (or attachments []))]
    (if (empty? rows)
      rows
      (mapv (fn [{:keys [media-type transcription] :as attachment}]
              (if-not (attachments/audio-media-type? media-type)
                attachment
                (if-not (str/blank? (str transcription))
                  (dissoc attachment :transcription-status)
                  (let [answer (answer-for attachment)]
                    (cond-> (dissoc attachment :transcription-status)
                      (:transcription answer)
                      (assoc :transcription (:transcription answer))

                      (seq (:transcription-segments answer))
                      (assoc :transcription-segments (vec (:transcription-segments answer)))

                      (:status answer)
                      (assoc :transcription-status (:status answer)))))))
            rows))))

(defn request-attachments!
  "Start the words for every recording in `attachments` and answer the rows with
   whatever is known NOW — normally `pending`.

   NOTHING waits. This is the call a surface makes the moment files are staged, and
   the one the turn makes when attachments first land, so the speech is already being
   made while the human is still typing and while the rest of the turn is assembled."
  ([attachments] (walk-recordings attachments request!))
  ([attachments load-base64]
   (walk-recordings attachments #(request! % load-base64))))

(defn transcribe-attachments
  "Join recordings in attachment order. An optional `:stall-ms` ([[transcribe-attachment]])
   returns pending rows when the worker stops making progress, without stopping it;
   callers consuming the words must handle those rows. `:on-progress` receives
   `{:recording i :recordings n :percent p}` for the recording being waited on, where
   `i` counts from 1 among the recordings that have no words yet and `:percent` is
   absent until the engine reports one. Without these options, wait for every
   recording."
  [attachments & [{:keys [on-progress] :as opts}]]
  (let [recordings (count (filter awaits-words? attachments))
        position (atom 0)]
    (walk-recordings attachments
                     (fn [attachment]
                       (let [recording (swap! position inc)]
                         (transcribe-attachment
                           attachment
                           (cond-> (dissoc opts :on-progress)
                             on-progress
                             (assoc :on-progress
                               (fn [percent]
                                 (on-progress (cond-> {:recording recording :recordings recordings}
                                                percent (assoc :percent percent))))))))))))
