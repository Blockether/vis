(ns com.blockether.vis.tui.voice-output
  "TUI speaker playback; synthesis runs in the gateway-owned speech engine.

   ONE line plays at a time and [[stop!]] cuts it, so the machine goes quiet the
   moment the human reaches for the microphone. The speaker plays two kinds of
   sound: a spoken answer ([[speak!]]) and a recording of the session
   ([[play-recording!]]). A new sound stops the one before it."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [javax.sound.sampled AudioInputStream AudioSystem DataLine$Info SourceDataLine]))

(def ^:private idle {:line nil :kind nil :token nil})

;; `:kind` is `:speech` or `:recording` from the start of a sound to its end, also while
;; its file loads. `:token` is the identity of that sound: a sound whose token is no
;; longer the current one never opens a line.
(defonce ^:private state* (atom idle))

(defn- publish! [event] (vis/publish-channel-event! :tui event))

(defn- speaking-status!
  [text]
  (publish! {:op :status/set :id :voice/output :text text :level :info}))

(defn- idle-status! [] (publish! {:op :status/clear :id :voice/output}))

(defn stop!
  "Silence whatever is playing; true when there WAS something to silence.

   The player thread owns the line's teardown - this only drops the line from the state
   and flushes it, which is what releases a `write` already blocked on a full buffer."
  []
  (let [{:keys [^SourceDataLine line kind]} @state*]
    (reset! state* idle)
    (when line (try (.stop line) (.flush line) (catch Throwable _ nil)))
    (when (or line kind) (idle-status!) true)))

(defn- begin!
  "Stop the current sound and make a new sound of `kind` the current one. Returns its token."
  [kind]
  (stop!)
  (let [token (Object.)]
    (reset! state* {:line nil :kind kind :token token})
    token))

(defn- current? [token] (identical? token (:token @state*)))

(defn- end!
  "End the sound of `token`. Its status clears unless a newer sound took the speaker."
  [token]
  (swap! state* #(if (identical? token (:token %)) idle %))
  (when-not (:token @state*) (idle-status!)))

(defn recording-playing? "True while a recording loads or plays." [] (= :recording (:kind @state*)))

(def ^:private ^:const chunk-bytes 8192)

(defn- play-file!
  "Play one WAV or AIFF file on the default output device in CHUNKS, so [[stop!]] cuts in
   between two of them instead of after the whole sound. Nothing plays when `token` is no
   longer current. `on-progress` gets the played and the total seconds after each chunk;
   the total is nil when the file does not give its length."
  ([file token] (play-file! file token nil))
  ([^File file token on-progress]
   (with-open [^AudioInputStream stream (AudioSystem/getAudioInputStream file)]
     (let [fmt (.getFormat stream)
           frames (.getFrameLength stream)
           total (when (and (pos? frames) (pos? (.getFrameRate fmt)))
                   (/ (double frames) (double (.getFrameRate fmt))))
           ^SourceDataLine line (AudioSystem/getLine (DataLine$Info. SourceDataLine fmt))
           buffer (byte-array chunk-bytes)]

       (when (current? token)
         (try (.open line fmt)
              (.start line)
              (swap! state* #(cond-> % (identical? token (:token %)) (assoc :line line)))
              (loop []

                (let [n (int (.read stream buffer 0 (int chunk-bytes)))]
                  (when (and (pos? n) (identical? line (:line @state*)))
                    (.write line buffer 0 n)
                    (when on-progress (on-progress (/ (.getMicrosecondPosition line) 1e6) total))
                    (recur))))
              (when (identical? line (:line @state*)) (.drain line))
              (finally (swap! state* #(if (identical? token (:token %)) idle %))
                       (try (.stop line) (.close line) (catch Throwable _ nil)))))))))

(defn speak!
  "Speak `text` for gateway session `sid`; synthesis is remote, playback is local.
   Returns the worker future, or nil when there is no prose to speak."
  ([sid text] (speak! sid text nil))
  ([sid text {:keys [engine-id voice-id]}]
   (let [prose (str/trim (str (vis/extract-text (str text))))]
     (when-not (str/blank? prose)
       (let [token (begin! :speech)]
         (vis/worker-future
           "vis-speech-speak"
           (fn []
             (try (speaking-status! "◉ Speaking")
                  (let [audio-file (vis/gateway-synthesize-speech! sid
                                                                   prose
                                                                   {:engine-id engine-id
                                                                    :voice-id voice-id})]
                    (try (play-file! audio-file token) (finally (io/delete-file audio-file true))))
                  (end! token)
                  (catch Throwable t
                    (tel/log! {:level :error
                               :id ::voice-speak-failed
                               :data {:error (ex-message t) :type (:type (ex-data t))}})
                    (end! token)
                    (publish! {:op :notify
                               :text (str "Voice cannot speak: " (or (ex-message t) (str t)))
                               :level :error}))))))))))

(defn- ffmpeg-path
  "The `ffmpeg` on the PATH, or nil."
  []
  (some #(let [f (io/file % "ffmpeg")] (when (.canExecute f) (str f)))
        (str/split (str (System/getenv "PATH")) (re-pattern File/pathSeparator))))

(defn- line-plays?
  "True when the speaker can play `file` as it is: a WAV, AIFF or AU file with a sample
   format that the output device takes."
  [^File file]
  (try (with-open [^AudioInputStream stream (AudioSystem/getAudioInputStream file)]
         (AudioSystem/isLineSupported (DataLine$Info. SourceDataLine (.getFormat stream))))
       (catch Exception _ false)))

(defn- converted-wav!
  "A temporary 16-bit PCM WAV copy of `file`, made by ffmpeg. The caller deletes it."
  ^File [^File file]
  (let [ffmpeg
        (or (ffmpeg-path)
            (throw (ex-info (str "This computer has no ffmpeg to play "
                                 (.getName file)
                                 ". Install ffmpeg, then play it again.")
                            {:type :voice/no-ffmpeg})))

        out
        (File/createTempFile "vis-recording-" ".wav")

        ^java.util.List command
        [ffmpeg "-v" "error" "-y" "-i" (str file) "-c:a" "pcm_s16le" (str out)]

        process
        (.start (doto (ProcessBuilder. command) (.redirectErrorStream true)))

        output
        (slurp (.getInputStream process))]

    (when-not (zero? (.waitFor process))
      (.delete out)
      (throw (ex-info (str "ffmpeg cannot read " (.getName file) ": " (str/trim output))
                      {:type :voice/unreadable})))
    out))

(defn- clock
  "`seconds` as `m:ss`."
  [seconds]
  (let [s (long seconds)]
    (format "%d:%02d" (quot s 60) (rem s 60))))

(defn play-recording!
  "Play a recording of the session and show its progress in the status line. `load!`
   returns `{:file File :is-temp bool}`; playback deletes a temporary file when it ends.
   `title` names the recording in the status line and `hint` follows the time there.
   Returns the worker future."
  [{:keys [title hint load!]}]
  (let [token
        (begin! :recording)

        shown
        (atom nil)

        progress
        (fn [played total]
          (let [text (str "▶ "
                          title
                          "  "
                          (clock played)
                          (when total (str " / " (clock total)))
                          (when hint (str "  · " hint)))]
            (when (and (current? token) (not= text @shown))
              (reset! shown text)
              (speaking-status! text))))]

    (vis/worker-future "vis-recording-play"
                       (fn []
                         (let [files (atom [])]
                           (try (speaking-status! (str "◌ Loading " title))
                                (let [{:keys [^File file is-temp]} (load!)
                                      _ (when is-temp (swap! files conj file))
                                      playable (if (line-plays? file)
                                                 file
                                                 (let [wav (converted-wav! file)]
                                                   (swap! files conj wav)
                                                   wav))]

                                  (play-file! playable token progress))
                                (end! token)
                                (catch Throwable t
                                  (tel/log! {:level :error
                                             :id ::recording-play-failed
                                             :data {:error (ex-message t)
                                                    :type (:type (ex-data t))}})
                                  (end! token)
                                  (publish! {:op :notify
                                             :text (str "Vis cannot play the recording: "
                                                        (or (ex-message t) (str t)))
                                             :level :error}))
                                (finally (doseq [^File f @files]
                                           (when f (.delete f))))))))))
