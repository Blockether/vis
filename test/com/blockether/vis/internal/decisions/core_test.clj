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
               "residency" "cold"}
              {"model_ref" "decision2.0-eos-0.8b"
               "revision" "3594047d69f476f1d01cf84c593e213fc3a4dfe0"
               "installed" false
               "residency" "cold"}
              {"model_ref" "decision2.0-kai-0.6b"
               "revision" "cd49ea3813fd8ba0928a9a23ef6c9a0f2f0cd764"
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

(defn- decision2-item
  [definition]
  (let [item (#'decisions/question "q" definition)]
    (assoc item :options (#'decisions/decision2-options item))))

(defn- error-type [f] (:type (ex-data (try (f) (catch clojure.lang.ExceptionInfo e e)))))

(defdescribe
  decision2-canonical-json-matches-python
  ;; Expected text comes from CPython `json.dumps(sort_keys=True, separators=(",", ":"),
  ;; ensure_ascii=False, allow_nan=False)` and `repr(float)`.
  (it
    "sorts keys by code point, keeps raw Unicode and escapes only JSON control characters"
    (expect
      (=
        "{\"a\":{\"Z\":false,\"é\":null,\"\uffff\":\"x\",\"😀\":true},\"b\":[1,2.5,-0.0,true,null,12345678901234567890],\"quote\\\"\\\\\":\"line\\nbreak\\ttab\\u0001\\u001f\u007f\u2028 \\b\\f\\r ü 中\"}"
        (#'decisions/canonical-json
         (array-map "b" [1 2.5 -0.0 true nil 12345678901234567890N]
                    "a" (array-map "\uffff" "x" "😀" true "é" nil "Z" false)
                    "quote\"\\" "line\nbreak\ttab\u0001\u001f\u007f\u2028 \b\f\r ü 中")))))
  (it "formats doubles like Python repr, also subnormal values"
      (doseq [[value expected] [[0.1 "0.1"] [1.0E16 "1e+16"] [1.0E-5 "1e-05"] [1.0E-4 "0.0001"]
                                [1.23456789E8 "123456789.0"] [100.0 "100.0"] [-2.5 "-2.5"]
                                [-0.0 "-0.0"] [1.5E-7 "1.5e-07"]
                                [9.007199254740992E15 "9007199254740992.0"]
                                [1.1805916207174113E21 "1.1805916207174113e+21"]
                                [1.7976931348623157E308 "1.7976931348623157e+308"]
                                [Double/MIN_VALUE "5e-324"] [(* 2 Double/MIN_VALUE) "1e-323"]]]
        (expect (= expected (#'decisions/python-float value)) expected)))
  (it "refuses values that the JSON contract cannot represent"
      (doseq [value [##NaN ##Inf (Object.)]]
        (expect (= :decisions/invalid-request (error-type #(#'decisions/canonical-json value)))))))

(defdescribe
  decision2-options-follow-the-upstream-task-contract
  (it "keeps choice order and gives label lists null descriptions"
      (expect (= [["refund" "Refunds and returns"] ["other" nil]]
                 (:options (decision2-item {"type" "choice"
                                            "instructions" "Pick."
                                            "criteria" (array-map "refund" "Refunds and returns"
                                                                  "other" nil)}))))
      (expect (= [["a" nil] ["b" nil]]
                 (:options (decision2-item
                             {"type" "choice" "instructions" "Pick." "criteria" ["a" "b"]})))))
  (it "numbers score levels and completes partial noul criteria"
      (expect (= [["0" "Missing"] ["1" "Partial"] ["2" "Complete"]]
                 (:options (decision2-item {"type" "score"
                                            "instructions" "Rate."
                                            "criteria" ["Missing" "Partial" "Complete"]}))))
      (expect (= [["false" "No"] ["true" "Yes"]]
                 (:options (decision2-item {"type" "noul" "instructions" "Holds?"}))))
      (expect (= [["false" "No"] ["true" "It holds"]]
                 (:options (decision2-item {"type" "noul"
                                            "instructions" "Holds?"
                                            "criteria" {"true" "It holds"}}))))
      (expect (= [["true" "T"] ["false" "F"]]
                 (:options (decision2-item {"type" "noul"
                                            "instructions" "Holds?"
                                            "criteria" (array-map "true" "T" "false" "F")})))))
  (it "refuses option counts and noul keys outside the model contract"
      (doseq [definition [{"type" "choice" "instructions" "Pick." "criteria" ["only"]}
                          {"type" "score" "instructions" "Rate." "criteria" (mapv str (range 11))}
                          {"type" "noul" "instructions" "Holds?" "criteria" {"maybe" "Unclear"}}]]
        (expect (= :decisions/invalid-request (error-type #(decision2-item definition)))
                (str definition)))))

(defdescribe
  decision2-prompt-matches-upstream-segments
  ;; Expected segments come from the upstream Decision 2.0 runtime, prompt
  ;; decision2-segmented-options-global-query-v1.
  (it
    "tokenizes each segment alone and marks every option endpoint and the final query"
    (let [seen (atom [])]
      (with-redefs [decisions/raw-token-ids (fn [_ value]
                                              (swap! seen conj value)
                                              [(count value)])]
        (let [item (#'decisions/decision2-sequence-item
                    nil
                    {"max_input_tokens" 4096}
                    (array-map "transfer" "pending" "requested_by" "account owner")
                    (decision2-item {"type" "noul"
                                     "instructions"
                                     "Did the account owner request this transfer?"}))]
          (expect
            (=
              ["Context:\n{\"requested_by\":\"account owner\",\"transfer\":\"pending\"}\n\nTask type: noul\nQuestion:\nDid the account owner request this transfer?\nOptions:"
               "\n<option>\n{\"description\":\"No\",\"key\":\"false\"}\n</option>"
               "\n<option>\n{\"description\":\"Yes\",\"key\":\"true\"}\n</option>"
               "\n\nSelect the single option best supported by the context and instructions.\nDecision:"]
              @seen))
          (expect (= (mapv count @seen) (:ids item)))
          (expect (= [1 2] (:markers item)))
          (expect (= 3 (:query item)))))))
  (it "reports the full token count above the limit without the question id"
      (with-redefs [decisions/raw-token-ids (fn [_ value]
                                              (vec (repeat (count value) 1)))]
        (let [item (assoc (decision2-item
                            {"type" "choice" "instructions" "Pick." "criteria" ["a" "b"]})
                     :id "private-question")
              encode #(#'decisions/decision2-sequence-item nil {"max_input_tokens" %} "state" item)
              size (count (:ids (encode 4096)))
              error (try (encode (dec size)) (catch clojure.lang.ExceptionInfo e e))]

          (expect (= size (count (:ids (encode size)))))
          (expect
            (= {:type :decisions/input-too-long :input-tokens size :max-input-tokens (dec size)}
               (ex-data error)))
          (expect (str/includes? (ex-message error) (str size " tokens")))
          (expect (str/includes? (ex-message error) "Input is not truncated"))
          (expect (not (str/includes? (ex-message error) "private-question"))))))
  (it "refuses an option that produces no token"
      (with-redefs [decisions/raw-token-ids (fn [_ value]
                                              (if (str/includes? value "<option>") [] [1]))]
        (expect (= :decisions/invalid-request
                   (error-type #(#'decisions/decision2-sequence-item
                                  nil
                                  {"max_input_tokens" 4096}
                                  "state"
                                  (decision2-item {"type" "noul" "instructions" "Holds?"}))))))))

(defdescribe decision2-answers-use-temperature-one-probabilities
             (it "selects the first label on a tie and reports rounded probabilities"
                 (expect (= {"type" "choice"
                             "choice" "a"
                             "probabilities" {"a" 0.4683 "b" 0.4683 "c" 0.0634}
                             "confidence" 0.1941}
                            (#'decisions/decision2-answer
                             (decision2-item
                               {"type" "choice" "instructions" "Pick." "criteria" ["a" "b" "c"]})
                             [2.0 2.0 0.0]))))
             (it "reads the true option wherever the caller placed it"
                 (expect (= {"type" "noul" "noul" 0.8808 "confidence" 0.8808}
                            (#'decisions/decision2-answer
                             (decision2-item {"type" "noul"
                                              "instructions" "Holds?"
                                              "criteria" (array-map "true" "T" "false" "F")})
                             [1.0 -1.0]))))
             (it "reports the expected score level with its legend"
                 (expect (= {"type" "score"
                             "score" 1.0
                             "legend" {"0" "Missing" "1" "Partial" "2" "Complete"}
                             "probabilities" {"0" 0.3333 "1" 0.3333 "2" 0.3333}
                             "confidence" 0.0}
                            (#'decisions/decision2-answer
                             (decision2-item {"type" "score"
                                              "instructions" "Rate."
                                              "criteria" ["Missing" "Partial" "Complete"]})
                             [0.0 0.0 0.0])))))

(defdescribe
  decision2-score-offsets-shift-matching-score-logits
  (let [score
        (decision2-item
          {"type" "score" "instructions" "Rate." "criteria" ["Missing" "Partial" "Complete"]})

        choice
        (decision2-item {"type" "choice" "instructions" "Pick." "criteria" ["a" "b" "c"]})

        score-bias
        {3 [1.0 0.0 -1.0]}]

    (it "adds the offsets for the level count to Score logits before softmax"
        (expect (= [1.0 0.0 -1.0] (#'decisions/score-logits score-bias score [0.0 0.0 0.0])))
        (expect (= {"type" "score"
                    "score" 0.4248
                    "legend" {"0" "Missing" "1" "Partial" "2" "Complete"}
                    "probabilities" {"0" 0.6652 "1" 0.2447 "2" 0.09}
                    "confidence" 0.2423}
                   (#'decisions/decision2-answer
                    score
                    (#'decisions/score-logits score-bias score [0.0 0.0 0.0])))))
    (it "keeps other questions, other level counts and bundles without offsets unchanged"
        (expect (= [0.0 0.0 0.0] (#'decisions/score-logits score-bias choice [0.0 0.0 0.0])))
        (expect (= [0.0 0.0 0.0] (#'decisions/score-logits {2 [1.0 1.0]} score [0.0 0.0 0.0])))
        (expect (= [0.0 0.0 0.0] (#'decisions/score-logits nil score [0.0 0.0 0.0]))))
    (it "reads bundle offsets by level count and rejects malformed offsets"
        (expect (nil? (#'decisions/decision2-score-bias nil)))
        (expect (= {5 [0.039188 0.203049 0.079362 -0.15162 -0.169979]}
                   (#'decisions/decision2-score-bias
                    {"5" [0.039188 0.203049 0.079362 -0.15162 -0.169979]})))
        (doseq [value [{} [] {"1" [0.0]} {"05" (vec (repeat 5 0.0))} {"256" (vec (repeat 256 0.0))}
                       {"3" [0.0 0.0]} {"2" [0.0 "x"]} {"2" [0.0 ##NaN]} {5 (vec (repeat 5 0.0))}]]
          (expect (= :decisions/invalid-bundle
                     (error-type #(#'decisions/decision2-score-bias value))))))))

(defdescribe
  decision2-tokenizer-keeps-every-input-token
  ;; DJL truncates to 512 tokens by default, so long Decision 2.0 input skipped the limit check.
  (it "keeps input above 512 tokens for the limit check"
      (let [dir
            (io/file (System/getProperty "java.io.tmpdir")
                     (str "vis-decision2-tokenizer-" (random-uuid)))

            path
            (io/file dir "tokenizer" "tokenizer.json")]

        (try
          (io/make-parents path)
          (spit path
                (wire/json-str
                  {"version" "1.0"
                   "truncation" nil
                   "padding" nil
                   "added_tokens" []
                   "normalizer" nil
                   "pre_tokenizer" {"type" "Whitespace"}
                   "post_processor" nil
                   "decoder" nil
                   "model" {"type" "WordLevel" "vocab" {"word" 0 "[UNK]" 1} "unk_token" "[UNK]"}}))
          (with-open [^HuggingFaceTokenizer tokenizer (#'decisions/decision2-tokenizer dir)]
            (expect
              (= 600
                 (count (#'decisions/raw-token-ids tokenizer (str/join " " (repeat 600 "word")))))))
          (finally (files/delete-dir! dir))))))

(defdescribe
  decision2-fp32-bundle-matches-upstream-reference
  ;; Expected token hashes and probabilities come from the upstream Decision 2.0 runtime at the
  ;; pinned Hugging Face revision of each model. The Kai five-level case checks its Score offsets.
  ;; Supply -Dvis.test.decision2.fp32.dir=<complete local inference dir>.
  (it
    "tokenizes like upstream and returns its probabilities through the public request path"
    (when-let [dir (System/getProperty "vis.test.decision2.fp32.dir")]
      (let [provenance (wire/parse-json (slurp (io/file dir "PROVENANCE.json")))
            model-id (get provenance "model")
            model {:id model-id
                   :revision (get provenance "revision")
                   :artifacts {:inference {:sha256 (apply str (repeat 64 "a"))
                                           :requires (assets/inference-required model-id)}}}
            parcel "Customer: my parcel arrived broken, I want my money back."
            queue {"type" "choice"
                   "instructions" "Pick the support queue for this message."
                   "criteria" (array-map "refund" "Refunds and returns"
                                         "shipping" "Delivery status questions"
                                         "other" nil)}
            transfer (array-map "transfer" "pending" "requested_by" "account owner")
            requested {"type" "noul" "instructions" "Did the account owner request this transfer?"}
            steps "The answer is correct but leaves out one of the three steps."
            completeness (fn [criteria]
                           {"type" "score"
                            "instructions" "Rate the completeness of the answer."
                            "criteria" criteria})
            cases
            (get {"decision2.0-eos-0.8b"
                  [[parcel queue "cc272dab1367e391edfe4878392f2adc8f8d3fb3823123201b4959ce8f480ee7"
                    [55 74 91] {"refund" 0.9886 "shipping" 0.0045 "other" 0.0069}]
                   [transfer requested
                    "74d79f0e33121d94f7995d8ad8022c528b56c8a356221de9ad4248e94acd9895" [51 68]
                    {"true" 0.9708}]
                   [steps (completeness ["Missing" "Partial" "Complete"])
                    "360c6dd46a633e7412142bbecce161f5b193c2579d3079d97df72f168cfcd3aa" [51 68 85]
                    {"0" 0.2933 "1" 0.7019 "2" 0.0049}]]
                  "decision2.0-kai-0.6b"
                  [[parcel queue "a83ed9cae158b1b0a48ca384cf337580d6e91cac4e45457610968a86a1dc97d7"
                    [49 66 81] {"refund" 0.8997 "shipping" 0.0558 "other" 0.0445}]
                   [transfer requested
                    "5b5bbe9f615d8109e706b6e153942bd3939440c8078cd463eb8acf117b596b9a" [45 60]
                    {"true" 0.6555}]
                   [steps (completeness ["Missing" "Partial" "Complete"])
                    "9030832c4698e3f6863510a236aa39c27ed9728225e0d6f911c5c03cf17b6bd5" [45 60 75]
                    {"0" 0.6674 "1" 0.2783 "2" 0.0543}]
                   [steps (completeness ["Missing" "Poor" "Partial" "Good" "Complete"])
                    "7ba664482028e7c063b772f125043d3306e8a08701e0165e907bba127bc5931a"
                    [45 60 75 90 105] {"0" 0.6264 "1" 0.0728 "2" 0.2128 "3" 0.0556 "4" 0.0323}]]}
                 model-id)
            close? #(< (Math/abs (- (double %1) (double %2))) 0.001)]

        (expect (seq cases))
        (try
          (decisions/validate-runtime! model (io/file dir))
          (with-open [^HuggingFaceTokenizer tokenizer (#'decisions/decision2-tokenizer
                                                       (io/file dir))]
            (with-redefs [assets/manifest (constantly [model])
                          assets/install-dir (fn [& _]
                                               dir)
                          assets/installed? (fn [& _]
                                              true)]

              (doseq [[state definition token-sha markers expected] cases]
                (let [item (#'decisions/decision2-sequence-item
                            tokenizer
                            {"max_input_tokens" 4096}
                            state
                            (decision2-item definition))
                      result (decisions/infer!
                               {"model" model-id "state" state "questions" {"q" definition}})
                      answer (get-in result ["answers" "q"])]

                  (expect (= token-sha
                             (util/bytes->hex (.digest ^MessageDigest (util/sha256-digest)
                                                       (.getBytes (#'decisions/canonical-json
                                                                   (:ids item))
                                                                  "UTF-8")))))
                  (expect (= markers (:markers item)))
                  (expect (= model-id (get result "model")))
                  (expect (= (count (:ids item)) (get-in result ["usage" "input_tokens"])))
                  (case (get definition "type")
                    "noul"
                    (expect (close? (get expected "true") (get answer "noul")))

                    "choice"
                    (expect (= (key (apply max-key val expected)) (get answer "choice")))

                    "score"
                    (expect (close? (reduce +
                                            (map (fn [[label probability]]
                                                   (* (parse-long label) probability))
                                                 expected))
                                    (get answer "score"))))
                  (when-not (= "noul" (get definition "type"))
                    (expect (every? (fn [[label probability]]
                                      (close? probability (get-in answer ["probabilities" label])))
                                    expected)))))
              ;; The request path rejects long input instead of truncating it.
              (expect (= :decisions/input-too-long
                         (error-type #(decisions/infer! {"model" model-id
                                                         "state" (str/join " "
                                                                           (repeat 3000 "parcel"))
                                                         "questions" {"q" requested}}))))))
          (finally (cache/release-idle!)))))))

(defdescribe
  decision2-fp32-archive-imports-warms-and-survives-cache-restart
  (it
    "decision2 fp32 archive imports warms and survives cache restart"
    ;; Supply -Dvis.test.decision2.fp32.archive=<complete FP32 zip>.
    (when-let [archive-path (System/getProperty "vis.test.decision2.fp32.archive")]
      (let [root (io/file (System/getProperty "java.io.tmpdir")
                          (str "vis-decision2-real-import-" (random-uuid)))
            archive (io/file archive-path)
            alias "local-decision2"
            request {"model" alias
                     "state" "Customer: my parcel arrived broken, I want my money back."
                     "questions"
                     (array-map "queue" {"type" "choice"
                                         "instructions" "Pick the support queue for this message."
                                         "criteria" (array-map "refund" "Refunds and returns"
                                                               "shipping"
                                                               "Delivery status questions")}
                                "urgency" {"type" "score"
                                           "instructions" "Rate urgency"
                                           "criteria" ["not urgent" "soon" "immediate"]}
                                "refund" {"type" "noul"
                                          "instructions" "Does the customer ask for a refund?"})}]

        (try (with-redefs [assets/models-root (constantly (str root))]
               (let [registered
                     (registry/register! archive (sha256-file archive) decisions/validate-runtime!)
                     ref (get registered "model_ref")
                     model-id (get-in (registry/resolve-model ref) [:model :id])]

                 (expect (contains? assets/decision2-architectures model-id))
                 (expect (= ref (get (registry/activate! alias ref nil) "model_ref")))
                 (expect (= ref (get (decisions/warm! alias) "model_ref")))
                 (let [answer (decisions/infer! request)]
                   (expect (= model-id (get answer "model")))
                   (expect (= alias (get-in answer ["routing" "model"])))
                   (expect (= "refund" (get-in answer ["answers" "queue" "choice"])))
                   (expect (<= 0.0 (double (get-in answer ["answers" "urgency" "score"])) 2.0))
                   (expect (< 0.5 (double (get-in answer ["answers" "refund" "noul"])))))
                 (cache/release-idle!)
                 (expect (= "cold"
                            (get (some #(when (= ref (get % "model_ref")) %)
                                       (decisions/models-status))
                                 "residency")))
                 (expect (= ref (get-in (decisions/infer! request) ["routing" "model_ref"])))))
             (finally (cache/release-idle!) (files/delete-dir! root)))))))
