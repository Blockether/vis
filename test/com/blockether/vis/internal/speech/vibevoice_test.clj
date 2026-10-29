(ns com.blockether.vis.internal.speech.vibevoice-test
  "VibeVoice noise, audio IO, transcript timing and request checks. The model round
   trip runs only on a machine where both bundles are installed."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.speech.assets :as assets]
            [com.blockether.vis.internal.speech.tts :as tts]
            [com.blockether.vis.internal.speech.vibevoice :as vibevoice]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]))

(defn- ex-data-of [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- sine
  ^floats [^long rate ^long n ^double hz]
  (let [out (float-array n)]
    (dotimes [i n]
      (aset out i (float (* 0.5 (Math/sin (/ (* 2.0 Math/PI hz i) rate))))))
    out))

(defdescribe noise-test
             (it "draws the values of the reference generator for seed 7"
                 ;; `Noise(7)` in vis-podcaster scripts/vibevoice_onnx.py prints these values. The
                 ;; sixth draw is a pair with its sine dropped, as one ASR noise scale is.
                 (let [state (vibevoice/noise-state 7)]
                   (expect (= [0.9884743094444275 0.1046566441655159 -1.8642557859420776
                               -1.0700430870056152 0.003920207265764475]
                              (mapv double (vibevoice/gaussian state 5))))
                   (expect (= -0.5292707085609436
                              (double (aget ^floats (vibevoice/gaussian state 1) 0)))))))

(defdescribe
  audio-test
  (it "writes a 16-bit WAV that reads back within one sample step"
      (let [samples
            (sine 24000 2400 440.0)

            file
            (File/createTempFile "vis-vibevoice-test" ".wav")]

        (try (vibevoice/write-wav! file samples 24000)
             (let [{:keys [^floats samples sample-rate] :as read} (vibevoice/read-wav file)]
               (expect (= 24000 sample-rate))
               (expect (= 2400 (alength samples)))
               (expect (every? #(< (Math/abs (- (double (aget samples (int %)))
                                                (double (aget ^floats (:samples read) (int %)))))
                                   1.0e-4)
                               (range 2400)))
               (expect (< (Math/abs (- 0.5 (reduce max (map double samples)))) 1.0e-3)))
             (finally (.delete file)))))
  (it "resamples 16 kHz to 24 kHz without changing the tone"
      (let [out
            (vibevoice/resample (sine 16000 16000 440.0) 16000 24000)

            ideal
            (sine 24000 24000 440.0)

            middle
            (range 1000 23000)]

        (expect (= 24000 (alength out)))
        (expect (every? #(< (Math/abs (- (double (aget out (int %))) (double (aget ideal (int %)))))
                            1.0e-2)
                        middle)))))

(defdescribe chunk-words-test
             (it "joins a word that two chunks split, and drops speaker labels and tags"
                 (let [words (vibevoice/chunk-words
                               [{:start 0.0 :end 3.0 :text " \n Speaker 0:Hello wor"}
                                {:start 3.0 :end 6.0 :text "ld again. [Silence]"}])]
                   (expect (= ["Hello" "world" "again."] (mapv :text words)))
                   (expect (< (:start (second words)) 3.0 (:end (second words))))
                   (expect (apply <= (map :start words)))
                   (expect (every? #(<= 0.0 (:start %) (:end %) 6.0) words)))))

(defdescribe
  request-test
  (it "refuses blank text and an unknown voice before any download"
      (expect (= :speech-tts/blank-text (:type (ex-data-of #(vibevoice/synthesize! {:text "  "})))))
      (let [data (ex-data-of #(vibevoice/synthesize! {:text "Hello." :voice-id "nobody"}))]
        (expect (= :speech-tts/unknown-voice (:type data)))
        (expect (contains? (set (:known data)) "carter")))))

(defn- temp-models-root
  ^File []
  (doto (File/createTempFile "vis-vibevoice-models" "") (.delete) (.mkdirs)))

(defdescribe
  sample-test
  (it "levels a sample to a 0.7 peak and keeps silence silent"
      (let [loud (float-array [0.1 -0.35 0.2])]
        (expect (= [0.2 -0.7 0.4]
                   (mapv #(/ (Math/round (* 1000.0 (double %))) 1000.0) (vibevoice/level loud))))
        (expect (= [0.0 0.0] (mapv double (vibevoice/level (float-array 2)))))))
  (it "reads the sample sentence in the language of each voice"
      (expect (= tts/sample-text (vibevoice/sample-text "carter")))
      (expect (= tts/sample-text (vibevoice/sample-text "samuel")))
      (expect (= (get vibevoice/sample-texts "ko") (vibevoice/sample-text "kr-man")))
      (expect (every? #(or (str/starts-with? (:language %) "en")
                           (contains? vibevoice/sample-texts (:language %)))
                      (vibevoice/voices))))
  (it "offers the pack sample of every voice, and nothing for a name outside the catalogue"
      (let [root (temp-models-root)]
        (with-redefs [assets/models-root (constantly (str root))]
          (expect (every? #(= {:is-preparable true} (vibevoice/voice-sample (:id %)))
                          (vibevoice/voices)))
          (expect (nil? (vibevoice/voice-sample "../../etc/passwd")))
          (expect (= :speech-tts/unknown-voice
                     (:type (ex-data-of #(vibevoice/prepare-voice-sample! "nobody"))))))))
  (it "answers with the file once one is on disk, packed or spoken here"
      (let [root (temp-models-root)]
        (with-redefs [assets/models-root (constantly (str root))]
          (let [packed (io/file root "vibevoice-voice-samples" "carter.wav")
                spoken (io/file root "vibevoice-samples-spoken" "emma.wav")]

            (doseq [^File f [packed spoken]]
              (io/make-parents f)
              (vibevoice/write-wav! f (sine 24000 240 440.0) 24000))
            (expect (= {:audio-path (str packed) :media-type "audio/wav"}
                       (vibevoice/voice-sample "carter")))
            (expect (= {:audio-path (str spoken) :media-type "audio/wav"}
                       (vibevoice/prepare-voice-sample! "emma"))))))))

(defdescribe
  round-trip-test
  (it
    "speaks a sentence that its own recognizer reads back, where both bundles are installed"
    (let [tts
          (vibevoice/tts-asset)

          asr
          (vibevoice/asr-asset)]

      (when (and (assets/installed? tts) (assets/installed? asr))
        (let [{:keys [samples]}
              (vibevoice/speak (assets/install-dir tts) "Testing one two three." "carter")

              text
              (str/lower-case (str/join (map :text
                                             (vibevoice/transcribe-samples (assets/install-dir asr)
                                                                           samples))))]

          (expect (str/includes? text "testing")))))))
