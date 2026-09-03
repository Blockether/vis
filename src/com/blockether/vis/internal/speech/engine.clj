(ns com.blockether.vis.internal.speech.engine
  "The built-in local Parakeet transcription engine used by the gateway."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.speech.asr :as asr]
            [com.blockether.vis.internal.speech.sherpa :as sherpa]
            [com.blockether.vis.internal.speech.transcode :as transcode])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

(def engine-id :parakeet-local)


(def ^:private filler-tokens #{"ah" "eh" "er" "erm" "hm" "hmm" "mm" "uh" "um" "huh"})

(defn- comparable-token
  [token]
  (some-> token
          str/lower-case
          (str/replace #"^[\p{Punct}\p{S}]+|[\p{Punct}\p{S}]+$" "")
          not-empty))

(defn- repeated-run?
  [tokens ^long i ^long n]
  (and (<= (+ i n n) (count tokens))
       (= (map comparable-token (subvec tokens i (+ i n)))
          (map comparable-token (subvec tokens (+ i n) (+ i n n))))))

(defn- strip-outer-punct
  [token]
  (let [stripped (-> token
                     (str/replace #"^[\p{Punct}\p{S}]+" "")
                     (str/replace #"[\p{Punct}\p{S}]+$" ""))]
    (if (str/blank? stripped) token stripped)))

(defn- collapse-repeated-runs
  [tokens]
  (loop [tokens
         (vec tokens)

         i
         0]

    (if (>= i (count tokens))
      tokens
      (if-let [n (some #(when (repeated-run? tokens i %) %)
                       (range (min 4 (quot (- (count tokens) i) 2)) 0 -1))]
        (recur (vec (concat (map strip-outer-punct (subvec tokens 0 (+ i n)))
                            (subvec tokens (+ i n n))))
               i)
        (recur tokens (inc i))))))

(defn clean-transcript
  "Deterministically remove ASR hesitation tokens and adjacent stutter runs."
  [text]
  (let [tokens (-> (str text)
                   (str/replace #"(?i)\b(?:you know|i mean)\b" " ")
                   (str/split #"\s+")
                   (->> (remove str/blank?)
                        (remove #(contains? filler-tokens (comparable-token %)))
                        vec))]
    (->> tokens
         collapse-repeated-runs
         (str/join " ")
         str/trim)))

(defn transcribe
  "Transcribe one recording through the gateway's Parakeet engine."
  [{:keys [audio-path on-progress]}]
  (transcode/with-wav (io/file (str audio-path))
                      (fn [^File wav]
                        (sherpa/call-native #(clean-transcript (asr/transcribe-file!
                                                                 (asr/model-dir)
                                                                 (str wav)
                                                                 {:on-progress on-progress}))))))

(def descriptor
  "The gateway's fixed local transcription engine descriptor."
  {:id engine-id
   :label "Parakeet (local)"
   :transcribe transcribe
   :model-state asr/model-state
   :start-download asr/start-download!})
