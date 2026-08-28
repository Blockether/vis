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

   WHEN it runs is the whole design. A surface that STAGES a recording — the composer
   rail, an upload, the gateway's own intake — calls [[request!]] the moment the file
   arrives and paints [[outcome]] while the human is still typing. By the time the
   turn is sent the words are normally already in hand; a turn that finds the work
   still running JOINS it under a deadline instead of starting its own. Nothing is
   ever transcribed twice, and nobody waits for a recording that was attached a
   minute ago.

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
            [com.blockether.vis.internal.attachments :as attachments]
            [com.blockether.vis.internal.toggles :as toggles]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.voice :as voice]
            [taoensso.telemere :as tel])
  (:import [java.io File FileOutputStream]
           [java.nio.charset StandardCharsets]
           [java.util Base64]
           [java.util.concurrent Callable ExecutorService Executors ThreadFactory]))

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

(def ^:private ^:const MAX_STARTED_PER_PASS
  "How many recordings ONE pass may put on the worker. The registry makes the steady
   state free, so this only bounds a message that arrives carrying a pile of memos."
  4)

(def ^:private ^:const JOIN_DEADLINE_MS
  "Wall-clock cap on WAITING for a transcript inside a turn. The work itself is not
   abandoned — it keeps running on the worker and a later pass collects it — but the
   human's request never parks behind an hour of speech."
  120000)

(def ^:private ^:const MAX_REGISTRY_ENTRIES 64)

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

(defonce ^:private work*
  ;; {content-digest {:outcome {…}}} for what is known, {content-digest {:result
  ;; promise :started-at ms}} for what is being made. One entry per recording, so
  ;; the atom is also the lock that keeps two surfaces from transcribing the same
  ;; bytes twice.
  (atom {}))

(defonce ^:private worker
  ;; ONE daemon thread: local speech saturates a core, and a message carrying five
  ;; memos must queue rather than race. Delayed so a build that never transcribes
  ;; never starts it.
  (delay (Executors/newSingleThreadExecutor
           (reify
             ThreadFactory
               (newThread [_ runnable]
                 (doto (Thread. ^Runnable runnable "vis-audio-transcribe") (.setDaemon true)))))))

(defn clear-cache!
  "Drop every transcript and every in-flight job. Tests only."
  []
  (reset! work* {}))

(defn- content-digest
  "Registry key: the payload's own bytes plus the container they ride in."
  [{:keys [base64 media-type path]}]
  (let [digest (util/sha256 (util/utf8
                              (str media-type "|" (or (not-empty (str base64)) (str path)))))]
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
                                        (container-extension buffer media-type filename))]

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

(defn- transcribe-now
  "Run ONE recording through the engine and answer its outcome. Total: an unreadable
   payload, a throw and a recording with no speech in it are all outcomes."
  [attachment]
  (let [started (util/now-ms)]
    (if-let [{:keys [^File file is-temp]} (try (source-file attachment)
                                               (catch Throwable t
                                                 (tel/log! {:level :warn
                                                            :id ::recording-unreadable
                                                            :data {:filename (:filename attachment)
                                                                   :error (ex-message t)}})
                                                 nil))]
      (try (let [text (some-> (voice/transcribe! {:audio-path (str file)})
                              str
                              str/trim
                              not-empty)
                 outcome (if text {:transcription text} {:status SILENT :reason :no-speech})]

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
                 (let [outcome (transcribe-now attachment)]
                   (settle! k outcome)
                   (deliver (:result entry) outcome)
                   outcome))))
    entry))

(defn- gate
  "What is already decided before any engine is asked. `::ok` means \"go ahead\"; nil
   means the attachment is not a recording and has no transcription outcome at all;
   anything else IS the outcome — it already carries words, or this machine cannot
   make any right now."
  [{:keys [media-type transcription] :as attachment}]
  (cond (not (attachments/audio-media-type? media-type)) nil
        (not (str/blank? (str transcription))) {:transcription (str transcription)}
        (not (enabled?)) (unavailable :disabled)
        (nil? (engine)) (unavailable :no-engine)
        (not (available?)) (unavailable :not-ready)
        (str/blank? (str (or (:base64 attachment) (:path attachment)))) (unavailable :unreadable)
        :else ::ok))

(defn outcome
  "What is known about this recording's words RIGHT NOW, without starting anything.

   `{:transcription \"…\"}` once they exist, `{:status \"pending\"}` while the worker
   has it, a settled `{:status …}` when it could not be made, and nil when nobody has
   asked yet — which is what a composer paints as a placeholder and re-reads on the
   next frame."
  [attachment]
  (let [entry (get @work* (content-digest attachment))]
    (cond (nil? entry) nil
          (:outcome entry) (:outcome entry)
          (realized? (:result entry)) (deref (:result entry))
          :else {:status PENDING :reason :running})))

(defn request!
  "Start transcribing `attachment` in the BACKGROUND and answer what is known so far.

   The upload-time door: a surface that has just staged a recording calls this and
   paints the answer — normally `{:status \"pending\"}` — so the words are ready
   before the turn is sent and no turn ever pays for the whole clip. Idempotent per
   recording, and free for anything that is not audio or already carries a
   transcript."
  [attachment]
  (let [gated (gate attachment)]
    (if (not= ::ok gated)
      gated
      (let [k (content-digest attachment)]
        (or (:outcome (get @work* k)) (do (start! k attachment) (outcome attachment)))))))

(defn transcribe-attachment
  "This recording's OUTCOME, waiting up to [[JOIN_DEADLINE_MS]] for words.

   Work already running is JOINED, never restarted; a recording nobody has asked
   about is started here. At the deadline the answer is `pending` and the worker
   keeps going, so the turn walks away from an hour of speech without throwing it
   away."
  [attachment]
  (let [gated (gate attachment)]
    (if (not= ::ok gated)
      ;; A refusal this side of the engine is exactly the silence turn 35 could not
      ;; explain: no engine, a toggle somebody turned off, a model still downloading.
      ;; It is logged HERE, once per recording per turn, and nowhere else.
      (do (when (:status gated) (log-outcome! attachment gated (util/now-ms))) gated)
      (let [k (content-digest attachment)
            {:keys [outcome result]} (start! k attachment)]

        (or outcome
            (let [answer (deref result JOIN_DEADLINE_MS ::timeout)]
              (if (= ::timeout answer)
                (do (tel/log! {:level :warn
                               :id ::transcribe-deadline
                               :data {:filename (:filename attachment) :ms JOIN_DEADLINE_MS}
                               :msg "left an attached recording transcribing past the turn"})
                    {:status PENDING :reason :deadline})
                answer)))))))

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
              (if-not (and (attachments/audio-media-type? media-type)
                           (str/blank? (str transcription)))
                attachment
                (let [answer (answer-for attachment)]
                  (cond-> attachment
                    (:transcription answer)
                    (assoc :transcription (:transcription answer))

                    (:status answer)
                    (assoc :transcription-status (:status answer))))))
            rows))))

(defn request-attachments!
  "Start the words for every recording in `attachments` and answer the rows with
   whatever is known NOW — normally `pending`.

   NOTHING waits. This is the call a surface makes the moment files are staged, and
   the one the turn makes when attachments first land, so the speech is already being
   made while the human is still typing and while the rest of the turn is assembled."
  [attachments]
  (walk-recordings attachments request!))

(defn transcribe-attachments
  "`attachments` with every RECORDING carrying its own `:transcription`, or — when
   there are no words to carry — the `:transcription-status` that says why.

   The call for the moment the words are actually NEEDED: work already running is
   joined, a recording nobody asked about is started here, and a pass may start at
   most [[MAX_STARTED_PER_PASS]] of them. Nothing is transcribed twice, and a
   recording still running at the deadline answers `pending` rather than holding the
   turn."
  [attachments]
  (let [budget (volatile! (long MAX_STARTED_PER_PASS))]
    (walk-recordings attachments
                     (fn [attachment]
                       (let [known (outcome attachment)]
                         (cond
                           ;; Settled, whichever way: the registry already paid for it.
                           (and known (not= PENDING (:status known))) known
                           ;; Running (a composer asked at attach time) — join it, which costs no
                           ;; budget because nothing new is started. Otherwise this pass may start
                           ;; one until its budget runs out.
                           (or known (pos? (long @budget))) (do (when-not known
                                                                  (vswap! budget
                                                                          (fn [n]
                                                                            (dec (long n)))))
                                                                (transcribe-attachment attachment))
                           :else {:status PENDING :reason :budget}))))))
