(ns com.blockether.vis.ext.foundation-voice.output
  "The SPEAKING half of a voice conversation, on THIS machine.

   `input.clj` turns a recording into text; this turns an answer into sound on the local
   sound card. It serves a MODE, not a setting: a surface ARMS a conversation and every
   answer that lands while it is armed is spoken here. There is deliberately no
   machine-wide \"spoken replies\" switch in front of it, because such a flag cannot say
   ON FOR THIS CONVERSATION - which is the only thing a human ever means by it.

   Only PROSE is spoken: `extract-text` keeps the paragraphs and drops code, tables and
   diffs, because reading a diff aloud is noise and the screen already has it.

   ONE line plays at a time and [[stop!]] cuts it, so the machine goes quiet the moment
   the human reaches for the microphone: talking over the person you are talking to is
   not a conversation."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.internal.voice :as voice]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [javax.sound.sampled AudioInputStream AudioSystem DataLine$Info SourceDataLine]))

(defonce ^:private state* (atom {:line nil}))

(defn- publish! [event] (vis/publish-channel-event! :tui event))

(defn- speaking-status!
  [text]
  (publish! {:op :status/set :id :voice/output :text text :level :info}))

(defn- idle-status! [] (publish! {:op :status/clear :id :voice/output}))

(defn speaking? "Whether an answer is being played right now." [] (some? (:line @state*)))

(defn stop!
  "Silence whatever is playing; true when there WAS something to silence.

   The player thread owns the line's teardown - this only drops the line from the state
   and flushes it, which is what releases a `write` already blocked on a full buffer."
  []
  (let [^SourceDataLine line (:line @state*)]
    (swap! state* assoc :line nil)
    (when line (try (.stop line) (.flush line) (catch Throwable _ nil)) (idle-status!) true)))

(def ^:private ^:const chunk-bytes 8192)

(defn- play-file!
  "Play one WAV on the default output device in CHUNKS, so [[stop!]] cuts in between two
   of them instead of after the whole answer."
  [^File file]
  (with-open [^AudioInputStream stream (AudioSystem/getAudioInputStream file)]
    (let [fmt (.getFormat stream)
          ^SourceDataLine line (AudioSystem/getLine (DataLine$Info. SourceDataLine fmt))
          buffer (byte-array chunk-bytes)]

      (try (.open line fmt)
           (.start line)
           (swap! state* assoc :line line)
           (loop []

             (let [n (int (.read stream buffer 0 (int chunk-bytes)))]
               (when (and (pos? n) (identical? line (:line @state*)))
                 (.write line buffer 0 n)
                 (recur))))
           (when (identical? line (:line @state*)) (.drain line))
           (finally (swap! state* (fn [s]
                                    (cond-> s
                                      (identical? line (:line s))
                                      (assoc :line nil))))
                    (try (.stop line) (.close line) (catch Throwable _ nil)))))))

(defn speak!
  "Speak `text` here and return the future doing it, or nil when there is nothing to say.
   Anything already playing is cut first: the newest answer is the one being waited for.

   Never throws at the caller. A missing model or a mute sound card is worth a status
   line and a notification; it is not worth taking down the turn that produced the text."
  ([text] (speak! text nil))
  ([text {:keys [engine-id voice-id]}]
   (let [prose (str/trim (str (vis/extract-text (str text))))]
     (when-not (str/blank? prose)
       (stop!)
       (vis/worker-future
         "vis-voice-speak"
         (fn []
           (try (speaking-status! "\u266a Speaking")
                (let [request (cond-> {:text prose}
                                engine-id
                                (assoc :engine-id engine-id)

                                voice-id
                                (assoc :voice-id voice-id))
                      audio-file (io/file (str (:audio-path (voice/synthesize! request))))]

                  (try (play-file! audio-file) (finally (io/delete-file audio-file true))))
                (idle-status!)
                (catch Throwable t
                  (tel/log! {:level :error
                             :id ::voice-speak-failed
                             :data {:error (ex-message t) :type (:type (ex-data t))}})
                  (idle-status!)
                  (publish! {:op :notify
                             :text (str "Voice cannot speak: " (or (ex-message t) (str t)))
                             :level :error})))))))))
