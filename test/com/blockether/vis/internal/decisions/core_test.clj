(ns com.blockether.vis.internal.decisions.core-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.decisions.cache :as cache]
            [com.blockether.vis.internal.decisions.core :as decisions]
            [com.blockether.vis.internal.decisions.registry :as registry]
            [com.blockether.vis.internal.speech.files :as files]
            [com.blockether.vis.internal.util :as util])
  (:import [ai.djl.huggingface.tokenizers HuggingFaceTokenizer]
           [java.io File FileInputStream]
           [java.security MessageDigest]))

(defdescribe
  request-parser-retains-choice-and-question-order
  (it "request parser retains choice and question order"
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

        (expect (= ["first" "second"] (vec (keys questions))))
        (expect (= labels (vec (keys (get-in questions ["first" "criteria"]))))))))

(defdescribe
  model-selection-refuses-unknown-and-uninstalled-bundles
  (it "model selection refuses unknown and uninstalled bundles"
      (let [request (decisions/parse-body
                      "{\"model\":\"not-a-model\",\"state\":\"hello\",\"questions\":{}}")]
        (expect (= :decisions/unknown-model
                   (:type (ex-data (try (decisions/infer! request)
                                        (catch clojure.lang.ExceptionInfo e e))))))
        (expect (= :decisions/model-not-installed
                   (:type (ex-data (try (decisions/infer! (assoc (into {} request)
                                                            "model" "laya-typed-decisions"))
                                        (catch clojure.lang.ExceptionInfo e e)))))))))

(defdescribe
  gliner-empty-request-routes-to-explicit-family
  (it "gliner empty request routes to explicit family"
      (doseq [id ["gliner2.5-base" "gliner2.5-decide"]]
        (let [model {:id id
                     :revision (apply str (repeat 64 "a"))
                     :artifacts {:inference {:sha256 (apply str (repeat 64 "b")) :requires []}}}]
          (with-redefs [assets/manifest (constantly [model])
                        assets/install-dir (fn [& _]
                                             "/tmp/gliner-not-installed")
                        assets/installed? (fn [& _]
                                            true)]

            (expect (= id
                       (get (decisions/infer! {"model" id "state" "hello" "questions" {}})
                            "model"))))))))

(defn- gliner-definition
  [{:strs [type instructions options]}]
  {"type" type
   "instructions" instructions
   "criteria" (case type
                "choice"
                (into (array-map)
                      (map (fn [[label display]]
                             [label (subs display (+ 2 (count label)))]))
                      options)

                "score"
                (mapv (fn [[label display]]
                        (subs display (+ 2 (count (str "level " label)))))
                      options)

                "noul"
                nil)})

(defdescribe
  gliner-input-budget
  ;; #295: count the full per-question sequence, not characters or state alone.
  (it "accepts the exact token limit and reports the count above it"
      (with-redefs [decisions/raw-token-ids (fn [_ value]
                                              (vec (repeat (count (str/split value #"\s+")) 1)))]
        (let [item {:id "private-question"
                    :type "choice"
                    :instruction "Select a label."
                    :options [["low" "low"] ["high" "high"]]}
              state (str "Example task: " (apply str (repeat 250 "alpha beta gamma ")))
              encoded
              (#'decisions/gliner-sequence-item nil {"max_position_embeddings" 2048} state item)
              size (count (:ids encoded))
              error (try (#'decisions/gliner-sequence-item
                          nil
                          {"max_position_embeddings" (dec size)}
                          state
                          item)
                         (catch clojure.lang.ExceptionInfo e e))]

          (expect
            (= encoded
               (#'decisions/gliner-sequence-item nil {"max_position_embeddings" size} state item)))
          (expect
            (= {:type :decisions/input-too-long :input-tokens size :max-input-tokens (dec size)}
               (ex-data error)))
          (expect (str/includes? (ex-message error) (str size " tokens")))
          (expect (str/includes? (ex-message error) (str "limit is " (dec size))))
          (expect (str/includes? (ex-message error) "Shorten"))
          (expect (not (str/includes? (ex-message error) "private-question"))))))
  (it "includes instructions and criteria in the same budget without truncating"
      (with-redefs [decisions/raw-token-ids (fn [_ value]
                                              (vec (repeat (count (str/split value #"\s+")) 1)))]
        (let [item {:id "priority"
                    :type "choice"
                    :instruction "Select."
                    :options [["low" "low"] ["high" "high"]]}
              state "alpha"
              size (count (:ids (#'decisions/gliner-sequence-item
                                 nil
                                 {"max_position_embeddings" 512}
                                 state
                                 item)))]

          (doseq [expanded [(assoc item :instruction "Select a label.")
                            (assoc item :options [["low" "low urgency"] ["high" "high"]])]]
            (let [expected (count (:ids (#'decisions/gliner-sequence-item
                                         nil
                                         {"max_position_embeddings" 512}
                                         state
                                         expanded)))
                  error (try (#'decisions/gliner-sequence-item
                              nil
                              {"max_position_embeddings" size}
                              state
                              expanded)
                             (catch clojure.lang.ExceptionInfo e e))]

              (expect (> expected size))
              (expect
                (= {:type :decisions/input-too-long :input-tokens expected :max-input-tokens size}
                   (ex-data error)))))))))

(defdescribe
  gliner-batches-bound-attention-memory
  (it
    "keeps 16 questions of 512 tokens together and splits longer questions in order"
    (let [runs
          (atom [])

          item
          (fn [id width]
            {:id id :ids (vec (repeat width 1))})]

      (with-redefs-fn {#'decisions/run-gliner-chunk (fn [_ _ items _]
                                                      (swap! runs conj (mapv :id items))
                                                      {:answers (into {}
                                                                      (map (fn [{:keys [id]}]
                                                                             [id id]))
                                                                      items)
                                                       :logits (into {}
                                                                     (map (fn [{:keys [id]}]
                                                                            [id [0.0]]))
                                                                     items)})}
        (fn []
          (let [short
                (mapv #(item (str "s" %) 512) (range 16))

                result
                (#'decisions/run-gliner-batch nil nil short nil)]

            (expect (= [(mapv :id short)] @runs))
            (expect (= 16 (count (:answers result)))))
          (reset! runs [])
          (let [items
                [(item "a" 2048) (item "b" 10) (item "c" 1000) (item "d" 1000) (item "e" 1449)]

                result
                (#'decisions/run-gliner-batch nil nil items nil)]

            (expect (= [["a"] ["b" "c" "d"] ["e"]] @runs))
            (expect (= {"a" "a" "b" "b" "c" "c" "d" "d" "e" "e"} (:answers result)))
            (expect (= 5 (count (:logits result))))))))))

(defdescribe
  gliner-words-match-upstream-splitter
  ;; Expected words come from gliner2 2.0.0 `WhitespaceTokenSplitter` and CPython `str.lower`.
  (it "splits and lower-cases multilingual text like the upstream Python splitter"
      (doseq [[text words]
              [["नमस्ते दुनिया" ["नमस" "्" "त" "े" "द" "ु" "न" "ि" "य" "ा"]]
               ["สวัสดีครับ" ["สว" "ั" "สด" "ี" "คร" "ั" "บ"]]
               ["שָׁלוֹם" ["ש" "ָ" "ׁ" "לו" "ֹ" "ם"]] ["Cafe\u0301 x² ½" ["cafe" "\u0301" "x²" "½"]]
               ["zero\u200Dwidth joiner\u200Cnon" ["zero" "\u200D" "width" "joiner" "\u200C" "non"]]
               ["ΟΔΟΣ ΟΔΟΣ_2024 ΣΑΣ." ["οδος" "οδος_2024" "σας" "."]] ["a\u001Cb" ["a" "b"]]
               ["K@Example.COM, @Some_User https://Ex.am/Σ"
                ["k@example.com" "," "@some_user" "https://ex.am/σ"]]
               ["İstanbul — Yes?" ["i̇stanbul" "—" "yes" "?"]]
               ["包裹损坏了，请退款。" ["包裹损坏了" "，" "请退款" "。"]]]]
        (expect (= words (vec (#'decisions/gliner-words text))) text))))

(defdescribe
  gliner-reference-tokenization-logits-and-typed-answers
  (it "gliner reference tokenization logits and typed answers"
      ;; Generated by gliner2 2.0.0 from the six pinned official checkpoints; decide-1b with
      ;; Transformers 5.17.0, the version that wrote its settings.
      ;; Supply -Dvis.test.gliner.<name>.fp32.dir=<complete local inference dir> for each name.
      (let [reference (wire/parse-json
                        (slurp (io/resource
                                 "com/blockether/vis/internal/decisions/gliner_reference.json")))]
        (doseq [name ["base" "small" "multi" "decide" "decide-1b" "multi-decide"]
                :let [dir (System/getProperty (str "vis.test.gliner." name ".fp32.dir"))]
                :when dir]

          (let [model-id (str "gliner2.5-" name)
                provenance (wire/parse-json (slurp (io/file dir "PROVENANCE.json")))
                model {:id model-id
                       :revision (get provenance "revision")
                       :artifacts {:inference {:sha256 (apply str (repeat 64 "a"))
                                               :requires (assets/inference-required model-id)}}}
                cases (get reference "cases")
                expected (get-in reference ["models" name "outputs"])
                loaded (#'decisions/open-model! model (io/file dir))]

            (try
              (expect (= (get-in reference ["models" name "architecture"])
                         (get provenance "architecture")))
              (decisions/validate-runtime! model (io/file dir))
              (let [items (mapv (fn [example]
                                  (let [q (get example "question")]
                                    (#'decisions/gliner-sequence-item
                                     (:tokenizer loaded)
                                     (:config loaded)
                                     (get example "state")
                                     (#'decisions/question (get q "id") (gliner-definition q)))))
                                cases)
                    batched (#'decisions/run-gliner-batch
                             (:environment loaded)
                             (:session loaded)
                             items
                             (:special loaded))]

                (doseq [[example expected-item item] (map vector cases expected items)]
                  (let [q (get example "question")
                        id (get q "id")
                        task (str (get q "type") ": " (get q "instructions"))
                        logits (get-in batched [:logits id])
                        result (get-in batched [:answers id])
                        public (with-redefs [assets/manifest (constantly [model])
                                             assets/install-dir (fn [& _]
                                                                  dir)
                                             assets/installed? (fn [& _]
                                                                 true)]

                                 (decisions/infer! {"model" model-id
                                                    "state" (get example "state")
                                                    "questions" (array-map id
                                                                           (gliner-definition q))}))
                        winner (get-in expected-item ["official" task "label"])
                        expected-key (some (fn [[key display]]
                                             (when (= display winner) key))
                                           (get q "options"))
                        act-confidence (get-in expected-item ["official" "action" "confidence"])
                        expected-act (if (= "act"
                                            (get-in expected-item ["official" "action" "label"]))
                                       act-confidence
                                       (- 1.0 act-confidence))]

                    (expect (= (get expected-item "ids") (:ids item)) (str model-id " / " id))
                    (expect (= (get expected-item "markers") (:markers item))
                            (str model-id " / " id))
                    (expect (= (count (get expected-item "logits")) (count logits))
                            (str model-id " / " id))
                    (expect (every? true?
                                    (map #(< (Math/abs (- (double %1) (double %2))) 0.002)
                                         logits
                                         (get expected-item "logits")))
                            (str model-id " / " id))
                    (expect (= model-id (get public "model")) (str model-id " / " id))
                    (expect (= model-id (get-in public ["routing" "model_ref"]))
                            (str model-id " / " id))
                    (expect (= result (get-in public ["answers" id])) (str model-id " / " id))
                    (expect (< (Math/abs (- (double (get-in result ["action" "act_probability"]))
                                            (double expected-act)))
                               0.002)
                            (str model-id " / " id))
                    (case (get q "type")
                      "choice"
                      (expect (= expected-key (get result "choice")) (str model-id " / " id))

                      "score"
                      (expect (<= 0.0 (double (get result "score")) 2.0) (str model-id " / " id))

                      "noul"
                      (expect (<= 0.0 (double (get result "noul")) 1.0) (str model-id " / " id)))))
                (expect (= :decisions/input-too-long
                           (:type (ex-data (try (#'decisions/gliner-sequence-item
                                                 (:tokenizer loaded)
                                                 (:config loaded)
                                                 (apply str (repeat 2500 "billing "))
                                                 (first items))
                                                (catch clojure.lang.ExceptionInfo e e)))))))
              (finally ((:close loaded)) (cache/release-idle!))))))))

(defdescribe
  catalog-distinguishes-installed-files-from-resident-sessions
  (it
    "catalog distinguishes installed files from resident sessions"
    (let [missing (str (System/getProperty "java.io.tmpdir") "/missing-laya-" (random-uuid))]
      (with-redefs [assets/install-dir (fn [& _]
                                         missing)]
        (expect
          (= [{"model_ref" "laya-typed-decisions"
               "revision" "dd079950600224fb459af2a0cb1d74e1e57ee9cf"
               "installed" false
               "residency" "cold"}
              {"model_ref" "gliner2.5-base"
               "revision" "7f1ae80f150e9d3e262ec1684d0d78208e2595d0"
               "installed" false
               "residency" "cold"}
              {"model_ref" "gliner2.5-small"
               "revision" "7132dc4561c3f94563c6147e75ffa8ef34c4964a"
               "installed" false
               "residency" "cold"}
              {"model_ref" "gliner2.5-multi"
               "revision" "2ca71aafb3446d9014e1c55c7ff51c9bc7209c47"
               "installed" false
               "residency" "cold"}
              {"model_ref" "gliner2.5-decide"
               "revision" "bbe10ff77ebb238777c17d3a8ac9260e30929057"
               "installed" false
               "residency" "cold"}
              {"model_ref" "gliner2.5-decide-1b"
               "revision" "688cd7ba8917a0855ad3ce929cba5a9998932e79"
               "installed" false
               "residency" "cold"}
              {"model_ref" "gliner2.5-multi-decide"
               "revision" "a35a0cd3b7a0f00f2effc576f454cd48fa98aa5f"
               "installed" false
               "residency" "cold"}]
             (decisions/models-status)))))))

(defdescribe tokenizer-ids-match-djl-without-encoding-metadata
             (it "tokenizer ids match djl without encoding metadata"
                 ;; The production path must not request unused character spans through JNI.
                 (when-let [dir (System/getProperty "vis.test.laya.fp32.dir")]
                   (with-open [^HuggingFaceTokenizer tokenizer
                               (HuggingFaceTokenizer/newInstance
                                 (.toPath ^java.io.File (io/file dir "tokenizer/tokenizer.json")))]
                     (doseq [text ["A refund for a damaged item." " Piñata [MASK] refund" ""]]
                       (expect
                         (= (vec (.getIds
                                   (.encode tokenizer (str/replace text "[MASK]" " ") false false)))
                            (#'decisions/token-ids tokenizer text))))))))

(defdescribe
  local-fp32-parity-across-all-typed-heads
  (it
    "Laya FP32 and JVM tokenizer agree on all three question types and action head"
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
            (expect (= "laya-typed-decisions" (get-in result ["routing" "model"])))
            (expect (= 108 (get-in result ["usage" "input_tokens"])))
            (expect (= "refund" (get-in result ["answers" "intent" "choice"])))
            (expect (= {"refund" 0.9427 "repair" 0.0573}
                       (get-in result ["answers" "intent" "probabilities"])))
            (expect (= 1.5357 (get-in result ["answers" "priority" "score"])))
            (expect (= 0.5466 (get-in result ["answers" "policy" "noul"])))
            (expect (= 1.0 (get-in result ["answers" "intent" "action" "act_probability"])))
            (expect (= [true "ready"]
                       ((juxt #(get % "installed") #(get % "residency"))
                         (first (decisions/models-status)))))))))))

(defdescribe
  imported-release-bundle-runs-both-heads-without-changing-the-baseline
  (it
    "imported release bundle runs both heads without changing the baseline"
    ;; Supply -Dvis.test.laya.fp32.archive=<assets-pack FP32 zip> for the full import gate.
    (when-let [archive (System/getProperty "vis.test.laya.fp32.archive")]
      (let [root (io/file (System/getProperty "java.io.tmpdir")
                          (str "vis-decision-real-import-" (random-uuid)))
            sha (:sha256 (assets/artifact (assets/entry "laya-typed-decisions") :inference))]

        (try
          (with-redefs [assets/models-root (constantly (str root))]
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

              (expect (= ref (get-in answer ["routing" "model_ref"])))
              (expect (number? (get-in answer ["answers" "intent" "action" "act_probability"])))
              (expect (number? (get-in answer ["answers" "policy" "noul"])))
              (expect (= ref (get (first (registry/versions)) "model_ref")))
              (expect (= "laya-typed-decisions"
                         (get (first (decisions/models-status)) "model_ref")))))
          (finally (cache/release-idle!) (files/delete-dir! root)))))))

(defn- sha256-file
  [^File archive]
  (with-open [stream (FileInputStream. archive)]
    (let [^MessageDigest digest (util/sha256-digest)
          buffer (byte-array 1048576)]

      (loop []

        (let [n (.read stream buffer)]
          (when (pos? n) (.update digest buffer 0 n) (recur))))
      (util/bytes->hex (.digest digest)))))

(defdescribe
  gliner-fp32-archives-import-warm-and-survive-cache-restart
  (it
    "gliner fp32 archives import warm and survive cache restart"
    ;; Supply -Dvis.test.gliner.{base,decide,decide-1b}.fp32.archive=<complete FP32 zip>.
    (doseq [name
            ["base" "decide" "decide-1b"]

            :let [archive-path
                  (System/getProperty (str "vis.test.gliner." name ".fp32.archive"))]
            :when archive-path]

      (let [root
            (io/file (System/getProperty "java.io.tmpdir")
                     (str "vis-gliner-real-import-" (random-uuid)))

            archive
            (io/file archive-path)

            model-id
            (str "gliner2.5-" name)

            alias
            (str "local-" name)

            sha
            (sha256-file archive)

            request
            {"model" alias
             "state" "A damaged item needs a refund."
             "questions" (array-map
                           "intent" {"type" "choice"
                                     "instructions" "Select intent"
                                     "criteria" (array-map "refund" "A refund" "repair" "A repair")}
                           "priority" {"type" "score"
                                       "instructions" "Rate urgency"
                                       "criteria" ["not urgent" "soon" "immediate"]}
                           "policy" {"type" "noul" "instructions" "Is refund available?"})}]

        (try
          (with-redefs [assets/models-root (constantly (str root))]
            (let [registered (registry/register! archive sha decisions/validate-runtime!)
                  ref (get registered "model_ref")]

              (expect (= model-id (get-in (registry/resolve-model ref) [:model :id])))
              (expect (= :decisions/unknown-model
                         (:type (ex-data (try (decisions/infer! request)
                                              (catch clojure.lang.ExceptionInfo e e))))))
              (expect (nil? (registry/get-alias alias)))
              (expect (= ref (get (registry/activate! alias ref nil) "model_ref")))
              (expect (= ref (get (decisions/warm! alias) "model_ref")))
              (expect (= "ready"
                         (get (some #(when (= ref (get % "model_ref")) %) (decisions/models-status))
                              "residency")))
              (let [answer (decisions/infer! request)]
                (expect (= model-id (get answer "model")))
                (expect (= alias (get-in answer ["routing" "model"])))
                (expect (= ref (get-in answer ["routing" "model_ref"])))
                (expect (= #{"intent" "priority" "policy"} (set (keys (get answer "answers")))))
                (expect (contains? #{"refund" "repair"}
                                   (get-in answer ["answers" "intent" "choice"])))
                (expect (number? (get-in answer ["answers" "priority" "score"])))
                (expect (number? (get-in answer ["answers" "policy" "noul"])))
                (expect (number? (get-in answer ["answers" "policy" "action" "act_probability"]))))
              (cache/release-idle!)
              (expect (= "cold"
                         (get (some #(when (= ref (get % "model_ref")) %) (decisions/models-status))
                              "residency")))
              (expect (= ref (get-in (decisions/infer! request) ["routing" "model_ref"])))
              (expect (= ref (get-in (registry/resolve-model alias) [:model-ref])))))
          (finally (cache/release-idle!) (files/delete-dir! root)))))))
