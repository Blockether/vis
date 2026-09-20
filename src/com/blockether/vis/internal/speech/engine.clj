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

(def ^:private filler-phrases
  "Two-word tics dropped as a PAIR. On their own `you` and `know` are ordinary words;
   side by side they are a hesitation nobody meant to say."
  #{["you" "know"] ["i" "mean"]})

(defn- comparable-text
  "A word reduced to what makes two of them the SAME word: lower case, without the
   punctuation that only says where it stood in a sentence."
  [{:keys [text]}]
  (some-> text
          str
          str/lower-case
          (str/replace #"^[\p{Punct}\p{S}]+|[\p{Punct}\p{S}]+$" "")
          not-empty))

(defn- repeated-run?
  [words ^long i ^long n]
  (and (<= (+ i n n) (count words))
       (= (map comparable-text (subvec words i (+ i n)))
          (map comparable-text (subvec words (+ i n) (+ i n n))))))

(defn- strip-outer-punct
  [{:keys [text] :as word}]
  (let [stripped (-> (str text)
                     (str/replace #"^[\p{Punct}\p{S}]+" "")
                     (str/replace #"[\p{Punct}\p{S}]+$" ""))]
    (if (str/blank? stripped) word (assoc word :text stripped))))

(defn- collapse-repeated-runs
  [words]
  (loop [words
         (vec words)

         i
         0]

    (if (>= i (count words))
      words
      (if-let [n (some #(when (repeated-run? words i %) %)
                       (range (min 4 (quot (- (count words) i) 2)) 0 -1))]
        (recur (vec (concat (map strip-outer-punct (subvec words 0 (+ i n)))
                            (subvec words (+ i n n))))
               i)
        (recur words (inc i))))))

(defn- drop-filler-phrases
  [words]
  (loop [remaining
         (vec words)

         acc
         []]

    (if (empty? remaining)
      acc
      (if (contains? filler-phrases
                     [(comparable-text (nth remaining 0)) (comparable-text (nth remaining 1 nil))])
        (recur (subvec remaining 2) acc)
        (recur (subvec remaining 1) (conj acc (nth remaining 0)))))))

(defn clean-words
  "Deterministically remove ASR hesitation words and adjacent stutter runs, keeping
   every surviving word exactly where it fell in the recording.

   The cleaning happens on WORDS and not on finished text, because the times are the
   point: strip a stutter out of a string and every timestamp after it belongs to a
   word that is no longer there."
  [words]
  (->> (vec words)
       drop-filler-phrases
       (remove #(str/blank? (str (:text %))))
       (remove #(contains? filler-tokens (comparable-text %)))
       vec
       collapse-repeated-runs))

(defn words->text
  "The words as one line of prose."
  [words]
  (str/trim (str/join " "
                      (map (fn [word]
                             (str (:text word)))
                           words))))

(defn clean-transcript
  "Deterministically remove ASR hesitation tokens and adjacent stutter runs from plain
   TEXT — the same pass [[clean-words]] makes over timed words, for a transcript that
   arrived without any times."
  [text]
  (words->text (clean-words (map (fn [token]
                                   {:text token})
                                 (remove str/blank? (str/split (str text) #"\s+"))))))

(def ^:private segment-pause-seconds
  "A silence this long between two words ends the line a reader is following: the
   speaker stopped, so the highlight stops with them."
  0.8)

(def ^:private segment-max-seconds
  "The longest a line may run without a full stop or a pause to end it. A highlight
   that covers half a minute of audio has stopped saying where in the memo you are."
  12.0)

(def ^:private segment-max-words 24)

(defn- sentence-end? [{:keys [text]}] (boolean (re-find #"[.!?…]['\"”’)]*$" (str text))))

(defn- segment-of
  [words]
  (when (seq words)
    {:start (double (:start (first words)))
     :end (double (reduce max
                          (map (fn [word]
                                 (double (:end word)))
                               words)))
     :text (words->text words)}))

(defn words->segments
  "Timed words grouped into the LINES a player can highlight: `{:start :end :text}`,
   in order, each one a piece of the same transcript.

   A line ends at sentence punctuation, at a pause of [[segment-pause-seconds]], or
   once it has grown past what one highlighted line usefully holds — so a memo spoken
   without a single full stop still follows the audio in readable pieces."
  [words]
  (let [timed (filterv #(and (number? (:start %)) (number? (:end %))) words)]
    (loop [remaining timed
           current []
           acc []]

      (if-let [word (first remaining)]
        (let [broken? (and (seq current)
                           (or (> (- (double (:start word)) (double (:end (peek current))))
                                  (double segment-pause-seconds))
                               (>= (count current) (long segment-max-words))
                               (> (- (double (:end (peek current)))
                                     (double (:start (first current))))
                                  (double segment-max-seconds))))
              acc (cond-> acc
                    broken?
                    (conj (segment-of current)))
              current (if broken? [word] (conj current word))]

          (if (sentence-end? word)
            (recur (rest remaining) [] (conj acc (segment-of current)))
            (recur (rest remaining) current acc)))
        (cond-> acc
          (seq current)
          (conj (segment-of current)))))))

(defn transcribe
  "Transcribe one recording through the gateway's Parakeet engine: the words, and the
   timed lines they fall into so a player can follow them."
  [{:keys [audio-path on-progress]}]
  (transcode/with-wav
    (io/file (str audio-path))
    (fn [^File wav]
      (sherpa/call-native
        #(let [{:keys [text words]}
               (asr/transcribe-file! (asr/model-dir) (str wav) {:on-progress on-progress}) cleaned
               (clean-words words)] {:text (if (seq cleaned)
                                             (words->text cleaned)
                                             (clean-transcript text))
                                     :segments (words->segments cleaned)})))))

(def descriptor
  "The gateway's fixed local transcription engine descriptor."
  {:id engine-id
   :label "Parakeet (local)"
   :transcribe transcribe
   :model-state asr/model-state
   :start-download asr/start-download!})
