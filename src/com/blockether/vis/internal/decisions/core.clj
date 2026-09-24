(ns com.blockether.vis.internal.decisions.core
  "Typed Laya and GLiNER inference over verified local FP32 bundles; no downloads."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.decisions.cache :as cache]
            [com.blockether.vis.internal.decisions.registry :as registry]
            [com.blockether.vis.internal.util :as util])
  (:import [ai.djl.huggingface.tokenizers HuggingFaceTokenizer]
           [ai.djl.huggingface.tokenizers.jni TokenizersLibrary]
           [ai.onnxruntime OnnxTensor OnnxValue OrtEnvironment OrtSession OrtSession$Result
            OrtSession$SessionOptions]
           [charred JSONReader$JSONObj]
           [java.io File]
           [java.util LinkedHashMap]))

(set! *warn-on-reflection* true)

(def ^:private qtypes {"choice" 0 "score" 1 "noul" 2})

;; Match the upstream GLiNER2.0 whitespace splitter before per-word subword encoding.
(def ^:private gliner-word-pattern
  #"(?iU)(?:https?://[^\s]+|www\.[^\s]+)|[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}|@[a-z0-9_]+|\w+(?:[-_]\w+)*|\S")

(defn- invalid! [message] (throw (ex-info message {:type :decisions/invalid-request})))

(defn- ordered
  "Charred's raw JSON object retains wire order, including choices past eight keys."
  [value]
  (cond (instance? JSONReader$JSONObj value)
        (let [result (LinkedHashMap.)]
          (doseq [[k v] (partition 2 (.-data ^JSONReader$JSONObj value))]
            (.put result k (ordered v)))
          result)
        (instance? java.util.List value) (mapv ordered value)
        :else value))

(defn parse-body
  "Decode a decision request without losing question and criterion insertion order."
  [text]
  (try (let [body (ordered (json/read-json text :profile :raw))]
         (when-not (instance? java.util.Map body) (invalid! "Decision body must be a JSON object"))
         body)
       (catch clojure.lang.ExceptionInfo e (throw e))
       (catch Exception _ (invalid! "Decision body must be valid JSON"))))

(defn- python-json
  "Laya's json.dumps spacing and key order for structured states and rubrics."
  [value]
  (cond (string? value) (json/write-json-str value :escape-unicode false :escape-slash false)
        (instance? java.util.Map value)
        (str "{"
             (str/join ", "
                       (map (fn [[k v]]
                              (str (python-json (str k)) ": " (python-json v)))
                            value))
             "}")
        (sequential? value) (str "[" (str/join ", " (map python-json value)) "]")
        :else (json/write-json-str value :escape-unicode false :escape-slash false)))

(defn- text [value] (if (string? value) value (python-json value)))

(defn- options
  [type criteria]
  (case type
    "choice"
    (let [pairs (if (instance? java.util.Map criteria)
                  criteria
                  (map (fn [label]
                         [label nil])
                       criteria))]
      (mapv (fn [[label description]]
              [label
               (if (or (nil? description) (= "" description))
                 label
                 (str label ": " (text description)))])
            pairs))

    "score"
    (mapv (fn [i description]
            [(str i) (str "level " i ": " (text description))])
          (range (count criteria))
          criteria)

    "noul"
    (mapv (fn [[label fallback]]
            (let [description (get criteria label)]
              [label
               (str label
                    ": "
                    (if (or (nil? description) (= "" description)) fallback (text description)))]))
          [["false" "no, the statement does not hold"] ["true" "yes, the statement holds"]])))

(defn- question
  [id definition]
  (when-not (and (util/non-blank-string? id) (instance? java.util.Map definition))
    (invalid! "Question ids must name JSON object definitions"))
  (let [type
        (get definition "type")

        instruction
        (get definition "instructions")

        criteria
        (get definition "criteria")]

    (when-not (contains? qtypes type) (invalid! (str "Unknown question type for " id)))
    (when-not (contains? definition "instructions") (invalid! (str "Missing instructions for " id)))
    (when (or (nil? instruction) (> (count (text instruction)) 4096))
      (invalid! (str "Invalid instructions for " id)))
    (case type
      "choice"
      (when-not (and (or (instance? java.util.Map criteria) (sequential? criteria))
                     (<= 1 (count criteria) 64)
                     (every? string?
                             (if (instance? java.util.Map criteria) (keys criteria) criteria)))
        (invalid! (str "Choice criteria must have 1–64 text labels for " id)))

      "score"
      (when-not (and (sequential? criteria) (<= 1 (count criteria) 64))
        (invalid! (str "Score criteria must have 1–64 levels for " id)))

      "noul"
      (when-not (or (nil? criteria) (instance? java.util.Map criteria))
        (invalid! (str "Noul criteria must be an object for " id))))
    {:id id
     :type type
     :instruction (text instruction)
     :criteria criteria
     :options (options type criteria)}))

(defn- raw-token-ids
  [^HuggingFaceTokenizer tokenizer value]
  ;; DJL Encoding requests unused Rust JNI character spans and fails in native-image.
  (let [^TokenizersLibrary library
        TokenizersLibrary/LIB

        encoding
        (.encode library (long (.getHandle tokenizer)) value false)]

    (try (vec (.getTokenIds library encoding)) (finally (.deleteEncoding library encoding)))))

(defn- token-ids
  [^HuggingFaceTokenizer tokenizer value]
  (raw-token-ids tokenizer (str/replace value "[MASK]" " ")))

(defn- special-tokens
  ([^File dir] (special-tokens dir ["[PAD]" "[CLS]" "[SEP]" "[MASK]"]))
  ([^File dir labels]
   (let [tokens
         (get (wire/parse-json (slurp (io/file dir "tokenizer/tokenizer.json"))) "added_tokens")

         lookup
         (into {} (map (juxt #(get % "content") #(get % "id")) tokens))]

     (into {}
           (for [label labels]
             [label
              (long (or (get lookup label)
                        (throw (ex-info "Decision tokenizer lacks special tokens"
                                        {:type :decisions/invalid-bundle}))))])))))

(defn- sequence-item
  [^HuggingFaceTokenizer tokenizer special config state question]
  (let [max-len
        (long (get config "max_len" 512))

        head-max
        (long (get config "head_max_len" 192))

        mask-id
        (get special "[MASK]")

        option-ids
        (mapv (fn [[_ option]]
                (into [mask-id] (take 48 (token-ids tokenizer (str " " option)))))
              (:options question))

        budget
        (- head-max (reduce + (map count option-ids)))

        option-ids
        (if (< budget 16)
          (let [per (max 4 (quot (- head-max 16) (count option-ids)))]
            (mapv #(vec (take per %)) option-ids))
          option-ids)

        budget
        (- head-max (reduce + (map count option-ids)))

        head
        (take (max 8 budget)
              (token-ids tokenizer (str (:type question) " question: " (:instruction question))))

        prefix
        (into [(get special "[CLS]")] (concat head [(get special "[SEP]")]))

        [prefix markers]
        (reduce (fn [[ids positions] option]
                  [(into ids option) (conj positions (count ids))])
                [prefix []]
                option-ids)

        prefix
        (conj prefix (get special "[SEP]"))

        room
        (max 0 (- max-len (count prefix) 1))

        state-ids
        (take room (token-ids tokenizer (if (string? state) state (python-json state))))

        ids
        (vec (take max-len (concat prefix state-ids [(get special "[SEP]")])))]

    (when-not (= (count markers)
                 (count (filter #(< % max-len) markers))
                 (count (:options question)))
      (invalid! (str "Question options exceed model head length for " (:id question))))
    (assoc question
      :ids ids
      :markers markers)))

(defn- gliner-sequence-item
  "Preserve upstream structural label positions, including labels containing marker text."
  [^HuggingFaceTokenizer tokenizer config state item]
  (let [choices
        (map second (:options item))

        _
        (when (some str/blank? choices)
          (invalid! (str "GLiNER decision labels must not be blank for " (:id item))))

        decision
        (concat [["(" false] ["[P]" false] [(str (:type item) ": " (:instruction item)) false]
                 ["(" false]]
                (mapcat (fn [label]
                          [["[L]" true] [label false]])
                        choices)
                [[")" false] [")" false]])

        action
        [["(" false] ["[P]" false] ["action" false] ["(" false] ["[L]" true] ["act" false]
         ["[L]" true] ["escalate" false] [")" false] [")" false]]

        value
        (if (string? state) state (python-json state))

        value
        (if (some #(str/ends-with? value %) ["." "!" "?"]) value (str value "."))

        words
        (map #(.toLowerCase ^String % java.util.Locale/ROOT) (re-seq gliner-word-pattern value))

        tokens
        (concat decision
                [["[SEP_STRUCT]" false]]
                action
                [["[SEP_TEXT]" false]]
                (map #(vector % false) words))

        [ids markers]
        (reduce (fn [[ids markers] [token label?]]
                  (let [subwords (raw-token-ids tokenizer token)]
                    (when (and label? (empty? subwords))
                      (invalid! "GLiNER classifier marker produced no token"))
                    [(into ids subwords) (if label? (conj markers (count ids)) markers)]))
                [[] []]
                tokens)]

    (when (or (> (count ids) (long (get config "max_position_embeddings")))
              (not= (count markers) (+ (count choices) 2)))
      (invalid! (str "GLiNER decision exceeds the encoder limit for " (:id item))))
    (assoc item
      :ids ids
      :markers markers)))

(defn- tensor-batch
  [items pad-id]
  (let [width
        (apply max (map (comp count :ids) items))

        kmax
        (apply max (map (comp count :markers) items))]

    {:input_ids (mapv (fn [{:keys [ids]}]
                        (long-array (concat ids (repeat (- (long width) (count ids)) pad-id))))
                      items)
     :attention_mask (mapv (fn [{:keys [ids]}]
                             (long-array (concat (repeat (count ids) 1)
                                                 (repeat (- width (count ids)) 0))))
                           items)
     :marker_pos (mapv (fn [{:keys [markers]}]
                         (long-array (concat markers (repeat (- kmax (count markers)) 0))))
                       items)
     :marker_mask (mapv (fn [{:keys [markers]}]
                          (boolean-array (concat (repeat (count markers) true)
                                                 (repeat (- kmax (count markers)) false))))
                        items)
     :qtype (long-array (map (comp qtypes :type) items))}))

(defn- tensor
  ^OnnxTensor [^OrtEnvironment environment rows]
  (OnnxTensor/createTensor environment ^Object (into-array (class (first rows)) rows)))

(defn- softmax
  [values]
  (let [peak
        (reduce max values)

        weights
        (mapv #(Math/exp (- (double %) (double peak))) values)

        total
        (reduce + weights)]

    (mapv #(/ % total) weights)))

(defn- round4 [value] (/ (double (Math/round (* (double value) 10000.0))) 10000.0))

(defn- confidence
  [probabilities]
  (if (< (count probabilities) 2)
    1.0
    (let [entropy
          (-
            (reduce + 0.0 (map #(* (double %) (Math/log (max 1.0e-12 (double %)))) probabilities)))]
      (max 0.0 (min 1.0 (- 1.0 (/ entropy (Math/log (count probabilities)))))))))

(defn- answer
  [item logits action config]
  (let [{:keys [type criteria options]}
        item

        k
        (count options)

        bucket
        (str type
             ":"
             (cond (<= k 2) "2"
                   (<= k 5) "3-5"
                   (<= k 10) "6-10"
                   :else "11+"))

        temperature
        (get (get config "temperature_by_options")
             bucket
             (nth (get config "temperature" [1.0 1.0 1.0]) (qtypes type)))

        temperature
        (if (and (number? temperature) (Double/isFinite (double temperature)))
          (max 0.5 (min 5.0 (double temperature)))
          1.0)

        probabilities
        (softmax (mapv #(/ (double %) temperature) (take k logits)))

        act
        (softmax (mapv double action))

        labels
        (mapv first options)

        conf
        (round4 (confidence probabilities))

        action-result
        {"act_probability" (round4 (first act))}]

    (case type
      "choice"
      {"type" type
       "choice" (nth labels (.indexOf ^java.util.List probabilities (apply max probabilities)))
       "probabilities" (zipmap labels (map round4 probabilities))
       "confidence" conf
       "action" action-result}

      "score"
      {"type" type
       "score" (round4 (reduce + (map-indexed #(* %1 %2) probabilities)))
       "legend" (zipmap labels criteria)
       "probabilities" (zipmap labels (map round4 probabilities))
       "confidence" conf
       "action" action-result}

      "noul"
      {"type" type
       "noul" (round4 (second probabilities))
       "confidence" (round4 (max (second probabilities) (- 1.0 (second probabilities))))
       "action" action-result})))

(defn- run-batch
  [^OrtEnvironment environment ^OrtSession session items special config]
  (let [batch (tensor-batch items (get special "[PAD]"))]
    (with-open [^OnnxTensor ids (tensor environment (:input_ids batch))
                ^OnnxTensor attention (tensor environment (:attention_mask batch))
                ^OnnxTensor positions (tensor environment (:marker_pos batch))
                ^OnnxTensor mask (tensor environment (:marker_mask batch))
                ^OnnxTensor qtype (OnnxTensor/createTensor environment ^Object (:qtype batch))
                ^OrtSession$Result outputs (.run session
                                                 {"input_ids" ids
                                                  "attention_mask" attention
                                                  "marker_pos" positions
                                                  "marker_mask" mask
                                                  "qtype" qtype})]

      (let [^"[[F" logits (.getValue ^OnnxValue (.get ^java.util.Optional (.get outputs "logits")))
            ^"[[F" actions (.getValue ^OnnxValue
                                      (.get ^java.util.Optional (.get outputs "act_logits")))]

        (into {}
              (map-indexed (fn [i item]
                             [(:id item)
                              (answer item (vec (aget logits i)) (vec (aget actions i)) config)])
                           items))))))

(defn- run-gliner-batch
  [^OrtEnvironment environment ^OrtSession session items special]
  (let [batch (tensor-batch items (get special "[PAD]"))]
    (with-open [^OnnxTensor ids (tensor environment (:input_ids batch))
                ^OnnxTensor attention (tensor environment (:attention_mask batch))
                ^OnnxTensor markers (tensor environment (:marker_pos batch))
                ^OrtSession$Result outputs
                (.run session {"input_ids" ids "attention_mask" attention "label_indices" markers})]

      (let [^"[[F" logits (.getValue ^OnnxValue (.get ^java.util.Optional (.get outputs "logits")))
            rows (mapv (fn [i item]
                         (let [k (count (:options item))
                               values (vec (aget logits i))]

                           (when-not (and (<= (+ k 2) (count values))
                                          (every? #(Double/isFinite (double %)) values))
                             (throw (ex-info "GLiNER graph returned invalid classification logits"
                                             {:type :decisions/invalid-bundle})))
                           (let [head (subvec values 0 (+ k 2))]
                             [(:id item)
                              {:logits head
                               :answer
                               (answer item (subvec head 0 k) (subvec head k (+ k 2)) {})}])))
                       (range (count items))
                       items)]

        {:answers (into {}
                        (map (fn [[id value]]
                               [id (:answer value)])
                             rows))
         :logits (into {}
                       (map (fn [[id value]]
                              [id (:logits value)])
                            rows))}))))

(def ^:private session-threads 4)

(defn- open-laya-model!
  [model ^File dir]
  (let [config
        (wire/parse-json (slurp (io/file dir "rl_agent_config.json")))

        provenance
        (wire/parse-json (slurp (io/file dir "PROVENANCE.json")))]

    (when-not (and (= "onnx" (get provenance "format"))
                   (= "fp32" (get provenance "precision"))
                   (= (:revision model) (get provenance "revision"))
                   (= (:id model) (get provenance "model"))
                   (<= 64 (long (get config "head_max_len" 0)) 512)
                   (<= 128 (long (get config "max_len" 0)) 2048))
      (throw (ex-info "Decision bundle is not a compatible FP32 export"
                      {:type :decisions/invalid-bundle :model (:id model)})))
    (let [special
          (special-tokens dir)

          ^OrtEnvironment environment
          (OrtEnvironment/getEnvironment)

          ^HuggingFaceTokenizer tokenizer
          (HuggingFaceTokenizer/newInstance (.toPath (io/file dir "tokenizer/tokenizer.json")))]

      (try (with-open [^OrtSession$SessionOptions options
                       (doto (OrtSession$SessionOptions.) (.setIntraOpNumThreads session-threads))]
             (let [^OrtSession session (.createSession environment
                                                       (.getAbsolutePath (io/file dir "model.onnx"))
                                                       options)]
               {:environment environment
                :session session
                :tokenizer tokenizer
                :special special
                :config config
                :close (fn []
                         (try (.close session) (finally (.close tokenizer))))}))
           (catch Throwable e (.close tokenizer) (throw e))))))

(defn- open-gliner-model!
  [model ^File dir]
  (let [config
        (wire/parse-json (slurp (io/file dir "config.json")))

        encoder-config
        (wire/parse-json (slurp (io/file dir "encoder_config/config.json")))

        provenance
        (wire/parse-json (slurp (io/file dir "PROVENANCE.json")))

        limit
        (get encoder-config "max_position_embeddings")]

    (when-not (and (= "onnx" (get provenance "format"))
                   (= "fp32" (get provenance "precision"))
                   (= "gliner2.5" (get provenance "family"))
                   (= (:id model) (get provenance "model"))
                   (= (:revision model) (get provenance "revision"))
                   (= (get assets/gliner-architectures (:id model))
                      (get provenance "architecture")
                      (get config "architecture"))
                   (integer? limit)
                   (<= 128 (long limit) 2048))
      (throw (ex-info "GLiNER decision bundle is not a compatible FP32 export"
                      {:type :decisions/invalid-bundle :model (:id model)})))
    (let [special
          (special-tokens dir ["[PAD]" "[P]" "[L]" "[SEP_STRUCT]" "[SEP_TEXT]"])

          ^OrtEnvironment environment
          (OrtEnvironment/getEnvironment)

          ^HuggingFaceTokenizer tokenizer
          (HuggingFaceTokenizer/newInstance (.toPath (io/file dir "tokenizer/tokenizer.json")))]

      (try (with-open [^OrtSession$SessionOptions options
                       (doto (OrtSession$SessionOptions.) (.setIntraOpNumThreads session-threads))]
             (let [^OrtSession session (.createSession environment
                                                       (.getAbsolutePath (io/file dir "model.onnx"))
                                                       options)]
               {:family :gliner
                :environment environment
                :session session
                :tokenizer tokenizer
                :special special
                :config {"max_position_embeddings" limit}
                :close (fn []
                         (try (.close session) (finally (.close tokenizer))))}))
           (catch Throwable e (.close tokenizer) (throw e))))))

(defn- open-model!
  [model ^File dir]
  (cond (= "laya-typed-decisions" (:id model)) (open-laya-model! model dir)
        (contains? assets/gliner-architectures (:id model)) (open-gliner-model! model dir)
        :else (throw (ex-info "Unsupported decision inference family"
                              {:type :decisions/invalid-bundle :model (:id model)}))))

(defn validate-runtime!
  "Exercise both classifier heads and the tokenizer before publishing an immutable bundle."
  [model ^File dir]
  (let [key [:validation (:revision model) (.getCanonicalPath dir)]]
    (try
      (cache/with-resident!
        key
        #(open-model! model dir)
        (fn [{:keys [family environment session tokenizer special config]}]
          (let [gliner? (= family :gliner)
                input-names (if gliner?
                              #{"input_ids" "attention_mask" "label_indices"}
                              #{"input_ids" "attention_mask" "marker_pos" "marker_mask" "qtype"})
                output-names (if gliner? #{"logits"} #{"logits" "act_logits"})]

            (when-not (and (= input-names (set (.getInputNames ^OrtSession session)))
                           (= output-names (set (.getOutputNames ^OrtSession session))))
              (throw (ex-info "Decision graph inputs or classification heads do not match"
                              {:type :decisions/invalid-bundle})))
            (let [probe (question "probe"
                                  {"type" "choice"
                                   "instructions" "Choose one option"
                                   "criteria" {"a" "First" "b" "Second"}})
                  item
                  (if gliner?
                    (gliner-sequence-item tokenizer config "Decision import validation" probe)
                    (sequence-item tokenizer special config "Decision import validation" probe))
                  answers (if gliner?
                            (:answers (run-gliner-batch environment session [item] special))
                            (run-batch environment session [item] special config))
                  result (get answers "probe")
                  probability (get-in result ["action" "act_probability"])]

              (when-not (and (number? (get-in result ["probabilities" "a"]))
                             (number? probability)
                             (Double/isFinite (double probability)))
                (throw (ex-info "Decision FP32 graph failed the two-head probe"
                                {:type :decisions/invalid-bundle})))))))
      (finally (cache/release-idle!)))))

(defn- selected-model
  [name]
  (if-let [model (some #(when (= name (:id %)) %) (assets/manifest))]
    (let [artifact (assets/artifact model :inference)]
      {:model-ref name
       :model model
       :artifact artifact
       :dir (io/file (assets/install-dir model :inference))})
    (or (registry/resolve-model name)
        (throw (ex-info (str "Unknown decision model: " name)
                        {:type :decisions/unknown-model :model name})))))

(defn- model-key
  [model artifact ^File dir]
  [(:id model) (:revision model) (:sha256 artifact) (.getCanonicalPath dir) session-threads])

(defn models-status
  "Report installed files separately from the in-memory session state."
  []
  (into (mapv (fn [model]
                (let [artifact
                      (assets/artifact model :inference)

                      dir
                      (io/file (assets/install-dir model :inference))]

                  {"model_ref" (:id model)
                   "revision" (:revision model)
                   "installed" (assets/installed? artifact (.getPath dir))
                   "residency" (name (cache/status (model-key model artifact dir)))}))
              (assets/manifest))
        (map (fn [{model-ref "model_ref" :as row}]
               (let [{:keys [model artifact dir]} (registry/resolve-model model-ref)]
                 (assoc row "residency" (name (cache/status (model-key model artifact dir))))))
             (registry/versions))))

(defn infer!
  "Evaluate typed questions against one explicitly installed, immutable model."
  [request]
  (let [name
        (get request "model")

        state
        (get request "state")

        questions
        (get request "questions")]

    (when-not (util/non-blank-string? name)
      (throw (ex-info "Decision model is required" {:type :decisions/model-required})))
    (let [{:keys [model artifact dir model-ref]} (selected-model name)]
      (when-not (assets/installed? artifact (.getPath ^File dir))
        (throw (ex-info "Decision model is not installed"
                        {:type :decisions/model-not-installed :model name})))
      (when-not (and (or (string? state) (instance? java.util.Map state) (sequential? state))
                     (<= (count (text state)) 65536)
                     (instance? java.util.Map questions)
                     (<= (count questions) 16))
        (invalid! "State or questions have an invalid shape or exceed request limits"))
      (let [items (mapv (fn [[id definition]]
                          (question id definition))
                        questions)
            routing {"model" name "model_ref" model-ref "revision" (:revision model)}
            engine (if (= "laya-typed-decisions" (:id model)) "laya-rl-agent" (:id model))]

        (if (empty? items)
          {"model" engine
           "routing" routing
           "answers" {}
           "usage" {"input_tokens" 0 "output_tokens" 0}}
          (cache/with-resident!
            (model-key model artifact dir)
            #(open-model! model dir)
            (fn [{:keys [family environment session tokenizer special config]}]
              (let [gliner? (= family :gliner)
                    items (mapv (if gliner?
                                  #(gliner-sequence-item tokenizer config state %)
                                  #(sequence-item tokenizer special config state %))
                                items)
                    answers (if gliner?
                              (:answers (run-gliner-batch environment session items special))
                              (run-batch environment session items special config))]

                {"model" engine
                 "routing" routing
                 "answers" answers
                 "usage" {"input_tokens" (reduce + (map (comp count :ids) items))
                          "output_tokens" 0}}))))))))

(defn warm!
  "Run one synthetic question against an installed model or alias, without downloading."
  [name]
  (let [ref (get-in (infer! {"model" name
                             "state" "Decision model warmup"
                             "questions" {"ready" {"type" "noul"
                                                   "instructions" "Is this a warmup question?"}}})
                    ["routing" "model_ref"])]
    (some #(when (= ref (get % "model_ref")) %) (models-status))))
