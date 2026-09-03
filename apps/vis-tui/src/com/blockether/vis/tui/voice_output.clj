(ns com.blockether.vis.tui.voice-output
  "TUI speaker playback; synthesis runs in the gateway-owned speech engine.

   ONE line plays at a time and [[stop!]] cuts it, so the machine goes quiet the
   moment the human reaches for the microphone."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.tui.client :as vis]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [javax.sound.sampled AudioInputStream AudioSystem DataLine$Info SourceDataLine]))

(defonce ^:private state* (atom {:line nil}))

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
  "Speak `text` for gateway session `sid`; synthesis is remote, playback is local.
   Returns the worker future, or nil when there is no prose to speak."
  ([sid text] (speak! sid text nil))
  ([sid text {:keys [engine-id voice-id]}]
   (let [prose (str/trim (str (vis/extract-text (str text))))]
     (when-not (str/blank? prose)
       (stop!)
       (vis/worker-future
         "vis-speech-speak"
         (fn []
           (try (speaking-status! "♪ Speaking")
                (let [audio-file (vis/gateway-synthesize-speech! sid
                                                                 prose
                                                                 {:engine-id engine-id
                                                                  :voice-id voice-id})]
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
