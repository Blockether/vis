(ns com.blockether.vis.internal.decisions.core-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.decisions.cache :as cache]
            [com.blockether.vis.internal.decisions.core :as decisions]
            [com.blockether.vis.internal.decisions.registry :as registry]
            [com.blockether.vis.internal.speech.files :as files])
  (:import [ai.djl.huggingface.tokenizers HuggingFaceTokenizer]))

(deftest request-parser-retains-choice-and-question-order
  (let [labels
        ["z" "r" "a" "b" "c" "d" "e" "f" "g" "x"]

        criteria
        (str "{" (str/join "," (map #(str "\"" % "\":null") labels)) "}")

        input
        (str "{\"model\":\"laya-typed-decisions\",\"state\":\"hello\",\"questions\":{"
             "\"first\":{\"type\":\"choice\",\"instructions\":\"choose\",\"criteria\":" criteria
             "}," "\"second\":{\"type\":\"noul\",\"instructions\":\"answer\"}}}")

        questions
        (get (decisions/parse-body input) "questions")]

    (is (= ["first" "second"] (vec (keys questions))))
    (is (= labels (vec (keys (get-in questions ["first" "criteria"])))))))

(deftest model-selection-refuses-unknown-and-uninstalled-bundles
  (let [request (decisions/parse-body
                  "{\"model\":\"not-a-model\",\"state\":\"hello\",\"questions\":{}}")]
    (is (= :decisions/unknown-model
           (:type (ex-data (try (decisions/infer! request)
                                (catch clojure.lang.ExceptionInfo e e))))))
    (is (= :decisions/model-not-installed
           (:type (ex-data (try (decisions/infer! (assoc (into {} request)
                                                    "model" "laya-typed-decisions"))
                                (catch clojure.lang.ExceptionInfo e e))))))))

(deftest catalog-distinguishes-installed-files-from-resident-sessions
  (let [missing (str (System/getProperty "java.io.tmpdir") "/missing-laya-" (random-uuid))]
    (with-redefs [assets/install-dir (fn [& _]
                                       missing)]
      (is (= [{"model_ref" "laya-typed-decisions"
               "revision" "dd079950600224fb459af2a0cb1d74e1e57ee9cf"
               "installed" false
               "residency" "cold"}]
             (decisions/models-status))))))

(deftest tokenizer-ids-match-djl-without-encoding-metadata
  ;; The production path must not request unused character spans through JNI.
  (when-let [dir (System/getProperty "vis.test.laya.fp32.dir")]
    (with-open [^HuggingFaceTokenizer tokenizer
                (HuggingFaceTokenizer/newInstance
                  (.toPath ^java.io.File (io/file dir "tokenizer/tokenizer.json")))]
      (doseq [text ["A refund for a damaged item." " Piñata [MASK] refund" ""]]
        (is (= (vec (.getIds (.encode tokenizer (str/replace text "[MASK]" " ") false false)))
               (#'decisions/token-ids tokenizer text)))))))

(deftest local-fp32-parity-across-all-typed-heads
  ;; Supply the verified release bundle with -Dvis.test.laya.fp32.dir=<installed inference dir>.
  (when-let [dir (System/getProperty "vis.test.laya.fp32.dir")]
    (let
      [request
       (decisions/parse-body
         (str
           "{\"model\":\"laya-typed-decisions\","
           "\"state\":\"The customer requests a refund after receiving a broken item.\","
           "\"questions\":{"
           "\"intent\":{\"type\":\"choice\",\"instructions\":\"What is the customer asking for?\","
           "\"criteria\":{\"refund\":\"A refund\",\"repair\":\"A repair\"}},"
           "\"priority\":{\"type\":\"score\",\"instructions\":\"Rate urgency\","
           "\"criteria\":[\"not urgent\",\"soon\",\"immediate\"]},"
           "\"policy\":{\"type\":\"noul\",\"instructions\":\"Can the purchase be refunded?\","
           "\"criteria\":{\"false\":\"not refundable\",\"true\":\"refundable\"}}}}"))]
      (with-redefs [assets/install-dir (fn [& _]
                                         dir)]
        (let [result (decisions/infer! request)]
          (testing "Laya FP32 and JVM tokenizer agree on all three question types and action head"
            (is (= "laya-typed-decisions" (get-in result ["routing" "model"])))
            (is (= 108 (get-in result ["usage" "input_tokens"])))
            (is (= "refund" (get-in result ["answers" "intent" "choice"])))
            (is (= {"refund" 0.9427 "repair" 0.0573}
                   (get-in result ["answers" "intent" "probabilities"])))
            (is (= 1.5357 (get-in result ["answers" "priority" "score"])))
            (is (= 0.5466 (get-in result ["answers" "policy" "noul"])))
            (is (= 1.0 (get-in result ["answers" "intent" "action" "act_probability"])))
            (is (= [true "ready"]
                   ((juxt #(get % "installed") #(get % "residency"))
                     (first (decisions/models-status)))))))))))

(deftest imported-release-bundle-runs-both-heads-without-changing-the-baseline
  ;; Supply -Dvis.test.laya.fp32.archive=<assets-pack FP32 zip> for the full import gate.
  (when-let [archive (System/getProperty "vis.test.laya.fp32.archive")]
    (let [root (io/file (System/getProperty "java.io.tmpdir")
                        (str "vis-decision-real-import-" (random-uuid)))
          sha (:sha256 (assets/artifact (assets/entry "laya-typed-decisions") :inference))]

      (try (with-redefs [assets/models-root (constantly (str root))]
             (let [registered (registry/register! (io/file archive) sha decisions/validate-runtime!)
                   ref (get registered "model_ref")
                   answer (decisions/infer!
                            {"model" ref
                             "state" "broken item refund"
                             "questions"
                             (array-map
                               "intent" {"type" "choice"
                                         "instructions" "Choose an intent"
                                         "criteria" (array-map "refund" "refund" "repair" "repair")}
                               "policy" {"type" "noul" "instructions" "Is this refundable?"})})]

               (is (= ref (get-in answer ["routing" "model_ref"])))
               (is (number? (get-in answer ["answers" "intent" "action" "act_probability"])))
               (is (number? (get-in answer ["answers" "policy" "noul"])))
               (is (= ref (get (first (registry/versions)) "model_ref")))
               (is (= "laya-typed-decisions" (get (first (decisions/models-status)) "model_ref")))))
           (finally (cache/release-idle!) (files/delete-dir! root))))))
