(ns com.blockether.vis.ext.foundation-voice.voices-test
  "Clips somebody brought. A pocket voice IS a reference recording, so importing one
   is the whole of \"add a voice\" - which makes this store the only thing standing
   between a phone's .m4a and the model, and the only thing that decides what Vis
   refuses to treat as a voice at all."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.ext.foundation-voice.voices :as voices]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- ex-data-of [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- wav!
  "A WAV file of `seconds` at `rate` with `channels` channels, carrying a tone rather
   than silence: a clip store has to move real samples, not merely copy a header."
  [^double seconds ^long rate ^long channels]
  (let
    [frames
     (long (* seconds (double rate)))

     data-bytes
     (* frames channels 2)

     buffer
     (doto (java.nio.ByteBuffer/allocate (+ 44 data-bytes))
       (.order java.nio.ByteOrder/LITTLE_ENDIAN))]

    (.put buffer (.getBytes "RIFF" "US-ASCII"))
    (.putInt buffer (+ 36 data-bytes))
    (.put buffer (.getBytes "WAVEfmt " "US-ASCII"))
    (.putInt buffer 16)
    (.putShort buffer (short 1))
    (.putShort buffer (short channels))
    (.putInt buffer (int rate))
    (.putInt buffer (int (* rate channels 2)))
    (.putShort buffer (short (* channels 2)))
    (.putShort buffer (short 16))
    (.put buffer (.getBytes "data" "US-ASCII"))
    (.putInt buffer (int data-bytes))
    (dotimes [i frames]
      (let [value (short (* 8000 (Math/sin (* 2.0 Math/PI 220.0 (/ (double i) (double rate))))))]
        (dotimes [_ channels]
          (.putShort buffer value))))
    (let [file (java.io.File/createTempFile "vis-voice-source" ".wav")]
      (.deleteOnExit file)
      (io/copy (.array buffer) file)
      file)))

(defn- with-voices-dir
  "Run `f` against a private clip store and then delete it. An import writes real
   files, and a test may never leave a voice in the home directory it ran in."
  [f]
  (let
    [dir (str (java.nio.file.Files/createTempDirectory
                "vis-voices-test"
                (into-array java.nio.file.attribute.FileAttribute [])))]
    (try (with-redefs [voices/voices-dir (constantly dir)]
           (f dir))
         (finally (doseq [file (reverse (file-seq (io/file dir)))]
                    (.delete file))))))

(defdescribe voice-id-test
             (it "makes one id out of whatever a human typed"
                 ;; The id travels through a URL path, a CLI argument and a JSON field, so it
                 ;; may not depend on what a name looked like when it was typed.
                 (expect (= "studio-take-1" (voices/voice-id "Studio Take 1")))
                 (expect (= "studio-take-1" (voices/voice-id "  STUDIO   take_1!  ")))
                 (expect (= "a-voice" (voices/voice-id "a/voice")))
                 (expect (= 48 (count (voices/voice-id (apply str (repeat 80 "a"))))))
                 (expect (nil? (voices/voice-id "   ")))
                 (expect (nil? (voices/voice-id "!!!")))
                 (expect (nil? (voices/voice-id nil)))))

(defdescribe
  import-test
  (it "takes a recording as a voice, hands it back, and forgets it on request"
      (with-voices-dir
        (fn [dir]
          (let
            [voice (voices/import! {:path (str (wav! 4 24000 1))
                                    :voice-name "Studio Take 1"
                                    :language "en-US"
                                    :text "what the clip says"})]
            (expect (= "studio-take-1" (:id voice)))
            (expect (= "Studio Take 1" (:label voice)))
            (expect (= "en-US" (:language voice)))
            (expect (= "what the clip says" (:clip-text voice)))
            (expect (= 24000 (:sample-rate voice)))
            (expect (= 4.0 (:seconds voice)))
            (expect (true? (:is-imported voice)))
            (expect (.isFile (io/file (:clip voice))))
            ;; the catalogue is DERIVED from the directory, so a clip that is on disk
            ;; is a voice and one that is not never was
            (expect (= [voice] (voices/imported)))
            (expect (= voice (voices/imported-voice "studio-take-1")))
            (expect (nil? (voices/imported-voice "someone-else")))
            (expect (true? (voices/forget! "studio-take-1")))
            (expect (empty? (voices/imported)))
            ;; deleting a voice twice is not an error, it is the same outcome
            (expect (false? (voices/forget! "studio-take-1")))
            (expect (empty? (seq (.list (io/file dir)))))))))
  (it "names the voice after the file when nobody named it"
      (with-voices-dir (fn [_dir]
                         (let
                           [file
                            (wav! 3 24000 1)

                            voice
                            (voices/import! {:path (str file)})]

                           (expect (= (voices/voice-id (str/replace (.getName file) #"\.wav$" ""))
                                      (:id voice)))
                           (expect (nil? (:clip-text voice)))))))
  (it "normalizes what it stores instead of handing a format to the engine"
      ;; A reference clip is ONE voice, and the model imitates the first seconds of
      ;; it: two channels are mixed down and a long recording is trimmed here, once,
      ;; rather than at every synthesis.
      (with-voices-dir (fn [_dir]
                         (let
                           [stereo
                            (voices/import! {:path (str (wav! 2 16000 2)) :voice-name "Stereo"})

                            stored
                            (voices/decode-wav (io/file (:clip stereo)))

                            long-one
                            (voices/import! {:path (str
                                                     (wav! (+ voices/max-clip-seconds 10) 8000 1))
                                             :voice-name "Long"})]

                           (expect (= 1 (:channels stored)))
                           (expect (= 16000 (:sample-rate stored)))
                           (expect (= 2.0 (:seconds stereo)))
                           (expect (= (double voices/max-clip-seconds) (:seconds long-one)))))))
  (it "refuses a recording it cannot use, and says which one"
      (with-voices-dir
        (fn [_dir]
          (let
            [garbage (doto (java.io.File/createTempFile "vis-voice-garbage" ".wav")
                       (.deleteOnExit))]
            (spit garbage "this is not a recording")
            (expect (= :voice-tts/clip-missing
                       (:type (ex-data-of #(voices/import! {:path "/nowhere/at/all.wav"})))))
            (expect (= :voice-tts/clip-too-short
                       (:type (ex-data-of #(voices/import! {:path (str (wav! 0.2 24000 1))
                                                            :voice-name "Blip"})))))
            (expect (false? (voices/wav? garbage)))
            ;; ffmpeg converts what Vis cannot decode, so the refusal depends on the
            ;; machine - but it is always a NAMED refusal about the recording
            (expect (contains? #{:voice-tts/clip-not-wav :voice-tts/clip-unreadable}
                               (:type (ex-data-of #(voices/import! {:path (str garbage)
                                                                    :voice-name "Garbage"}))))))))))
