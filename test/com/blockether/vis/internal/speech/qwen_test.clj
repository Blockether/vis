(ns com.blockether.vis.internal.speech.qwen-test
  "Qwen3-TTS language choice, text chunks and first-code sampling. These checks need
   no model files."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.speech.qwen :as qwen]
            [com.blockether.vis.internal.speech.vibevoice :as vibevoice]
            [com.blockether.vis.internal.speech.voices :as voices]
            [lazytest.core :refer [defdescribe describe expect it]]))

(defn- peaked-logits
  "Talker logits after `offset` padding values where each token of `peaks` has a high
   score and every other token a low one."
  ^floats [offset peaks]
  (let [out (float-array (+ (long offset) (long qwen/codec-vocab)) -50.0)]
    (doseq [[token score] peaks]
      (aset out (+ (long offset) (long token)) (float score)))
    out))

(defdescribe
  text-language-test
  (describe "the script"
            (it "chooses the language of a non-Latin script"
                (expect (= "chinese" (qwen/text-language "今天天气很好。")))
                (expect (= "japanese" (qwen/text-language "こんにちは、元気ですか。")))
                (expect (= "korean" (qwen/text-language "안녕하세요")))
                (expect (= "russian" (qwen/text-language "Привет, как дела?")))))
  (describe
    "common words"
    (it "chooses the Latin-script language with the most common words"
        (expect (= "english" (qwen/text-language "This is the end of the show and you know it.")))
        (expect (= "german" (qwen/text-language "Das ist nicht die Frage, und wir wissen es.")))
        (expect (= "french" (qwen/text-language "Le chat est dans la maison avec nous."))))
    (it "gives nil for a language that the model does not speak"
        (expect (nil? (qwen/text-language "Dzień dobry, wszystkim słuchaczom!"))))))

(defdescribe chunks-test
             (it "keeps a short text as one chunk"
                 (expect (= ["Hello there. How are you?"]
                            (qwen/chunks "  Hello there. How are you?  "))))
             (it "splits a long text at sentence ends into chunks within the limit"
                 (let [sentence
                       "This sentence has a length that repeats many times over. "

                       text
                       (str/join (repeat 20 sentence))

                       parts
                       (qwen/chunks text)]

                   (expect (< 1 (count parts)))
                   (expect (every? #(<= (count %) (long qwen/chunk-chars)) parts))
                   (expect (= (str/trim text) (str/join " " parts)))))
             (it "splits at line breaks"
                 (let [line (str/join " " (repeat 40 "word"))]
                   (expect (= [line line] (qwen/chunks (str line "\n\n" line)))))))

(defdescribe
  voices-test
  (it "lists the built-in speakers when no voice is imported"
      (with-redefs [voices/imported (constantly [])]
        (expect (= 9 (count (qwen/voices))))
        (expect (not-any? :is-imported (qwen/voices)))))
  (it "adds the imported voices, and an import with a speaker name wins"
      (with-redefs [voices/imported (constantly [{:id "ryan" :label "Me" :is-imported true}
                                                 {:id "marius" :label "Marius" :is-imported true}])]
        (let [listed (qwen/voices)]
          (expect (= 10 (count listed)))
          (expect (= [{:id "ryan" :label "Me" :is-imported true}
                      {:id "marius" :label "Marius" :is-imported true}]
                     (filterv :is-imported listed)))
          (expect (= 1 (count (filter #(= "ryan" (name (:id %))) listed)))))))
  (it "needs the Base model only for an imported voice"
      (with-redefs [voices/imported-voice #(when (= "marius" %) {:id "marius" :clip "/x.wav"})]
        (expect (= [qwen/asset-id] (mapv :id (#'qwen/voice-assets :ryan))))
        (expect (= [qwen/asset-id qwen/clone-asset-id qwen/predictor-asset-id]
                   (mapv :id (#'qwen/voice-assets "marius")))))))

(defdescribe sample-first-code-test
             (it "takes a clear winner among the codec tokens"
                 (expect (= 5
                            (long (qwen/sample-first-code (peaked-logits 7 {5 40.0})
                                                          7 []
                                                          2150 (vibevoice/noise-state 1))))))
             (it "blocks control tokens but lets the end-of-speech token through"
                 (expect (= 5
                            (long (qwen/sample-first-code (peaked-logits 0 {5 30.0 2500 40.0})
                                                          0 []
                                                          2150 (vibevoice/noise-state 1)))))
                 (expect (= 2150
                            (long (qwen/sample-first-code (peaked-logits 0 {5 30.0 2150 40.0})
                                                          0 []
                                                          2150 (vibevoice/noise-state 1))))))
             (it "gives the same code for the same seed"
                 (let [logits
                       (peaked-logits 0 (zipmap (range 40) (range 40)))

                       draw
                       #(long (qwen/sample-first-code logits 0 [] 2150 (vibevoice/noise-state %)))]

                   (expect (= (draw 3) (draw 3))))))
