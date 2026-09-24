(ns com.blockether.vis.internal.decisions.jobs-real-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.decisions.cache :as cache]
            [com.blockether.vis.internal.decisions.core :as decisions]
            [com.blockether.vis.internal.decisions.jobs :as jobs]
            [com.blockether.vis.internal.decisions.registry :as registry]
            [com.blockether.vis.internal.speech.files :as files]))

(def ^:private request
  {"train_data" "train.jsonl"
   "eval_data" "eval.jsonl"
   "training_config" "config.json"
   "validation_policy" "policy.json"})

(defn- await-terminal
  [id]
  (loop [remaining 1200]
    (let [status (jobs/get! id)]
      (if (or (contains? #{"completed" "failed" "cancelled"} (get status "status"))
              (zero? remaining))
        status
        (do (Thread/sleep 500) (recur (dec remaining)))))))

(deftest installed-sdk-worker-trains-registers-and-resumes-offline
  ;; Full gate: -Dvis.test.laya.training.dir and -Dvis.test.laya.training.python
  ;; point at the verified release checkpoint and an offline SDK+training environment.
  (when-let [checkpoint (System/getProperty "vis.test.laya.training.dir")]
    (let [python (System/getProperty "vis.test.laya.training.python")
          root (io/file (System/getProperty "java.io.tmpdir")
                        (str "decision-real-training-" (random-uuid)))
          data (io/file root "approved-data")
          models (io/file root "models")
          question
          {"type" "choice" "instructions" "Choose a request" "criteria" ["refund" "repair"]}
          example
          {"state" "Please refund my damaged purchase" "question" question "target" 0 "action" 0}
          original-install-dir assets/install-dir]

      (is (some? python))
      (.mkdirs data)
      (.mkdirs models)
      (spit (io/file data "train.jsonl")
            (str (wire/json-str (assoc example "state" "A second item arrived damaged")) "\n"))
      (spit (io/file data "eval.jsonl") (str (wire/json-str example) "\n"))
      (spit (io/file data "config.json")
            "{\"epochs\":1,\"learning_rate\":0.00001,\"train_encoder\":false,\"max_steps\":1}\n")
      (spit (io/file data "policy.json")
            "{\"min_decision_accuracy\":0,\"min_action_accuracy\":0}\n")
      (try
        (with-redefs [assets/models-root (constantly (.getPath models))
                      assets/install-dir
                      (fn [model kind]
                        (if (= kind :training) checkpoint (original-install-dir model kind)))
                      config/extension-env-value (fn [key]
                                                   (case key
                                                     "VIS_DECISION_TRAINING_PYTHON"
                                                     python

                                                     "VIS_DECISION_TRAINING_DATA_ROOT"
                                                     (.getPath data)

                                                     nil))]

          (cache/enable!)
          (cache/release-idle!)
          (let [first-id (get (jobs/create! request) "job_id")
                first-status (await-terminal first-id)]

            (is (= "completed" (get first-status "status")) (str first-status))
            (when (= "completed" (get first-status "status"))
              (let [ref (get first-status "model_ref")
                    answer (decisions/infer! {"model" ref
                                              "state" "broken item refund"
                                              "questions" {"intent" {"type" "choice"
                                                                     "instructions" "Choose intent"
                                                                     "criteria" ["refund"
                                                                                 "repair"]}}})
                    next-id (get (jobs/create! (assoc request "source_job_id" first-id)) "job_id")
                    second-status (await-terminal next-id)]

                (is (= ref (get-in answer ["routing" "model_ref"])))
                (is (number? (get-in answer ["answers" "intent" "action" "act_probability"])))
                (is (.isFile (io/file models "jobs" first-id "output/checkpoint/PROVENANCE.json")))
                (is (= "completed" (get second-status "status")) (str second-status))
                (is (<= 1 (count (registry/versions))))))))
        (finally (jobs/stop!) (cache/release-idle!) (files/delete-dir! root))))))

(deftest installed-gliner-worker-trains-registers-and-resumes-both-families-offline
  ;; Full gate: -Dvis.test.gliner.training.python and one or both of
  ;; -Dvis.test.gliner.training.base.dir / -Dvis.test.gliner.training.decide.dir.
  ;; The directories hold verified full checkpoints, not encoder-only ONNX bundles.
  (when-let [python (System/getProperty "vis.test.gliner.training.python")]
    (doseq [[model-id property] [["gliner2.5-base" "vis.test.gliner.training.base.dir"]
                                 ["gliner2.5-decide" "vis.test.gliner.training.decide.dir"]]
            :let [checkpoint (System/getProperty property)]
            :when checkpoint]

      (let [root (io/file (System/getProperty "java.io.tmpdir")
                          (str "decision-gliner-job-" (random-uuid)))
            data (io/file root "approved-data")
            models (io/file root "models")
            question {"type" "choice"
                      "instructions" "Choose intent"
                      "criteria" ["refund_request" "order_status" "other"]}
            example
            {"state" "Please refund this purchase" "question" question "target" 0 "action" 1}
            original-entry assets/entry
            original-artifact assets/artifact
            original-install-dir assets/install-dir
            original-installed? assets/installed?]

        (.mkdirs data)
        (.mkdirs models)
        (spit (io/file data "train.jsonl")
              (str (wire/json-str (assoc example "state" "Please refund my order")) "\n"))
        (spit (io/file data "eval.jsonl") (str (wire/json-str example) "\n"))
        (spit (io/file data "config.json")
              (str (wire/json-str {"epochs" 1 "max_steps" 1 "encoder_lr" 0.00001 "task_lr" 0.0005})
                   "\n"))
        (spit (io/file data "policy.json")
              (str (wire/json-str {"min_decision_accuracy" 0 "min_action_accuracy" 0}) "\n"))
        (try
          (with-redefs [assets/models-root (constantly (.getPath models))
                        assets/entry (fn [id]
                                       (if (= id model-id) {:id id} (original-entry id)))
                        assets/artifact
                        (fn [model kind]
                          (if (= kind :training) {:kind :training} (original-artifact model kind)))
                        assets/install-dir
                        (fn [model kind]
                          (if (= kind :training) checkpoint (original-install-dir model kind)))
                        assets/installed? (fn [artifact dir]
                                            (if (= (:kind artifact) :training)
                                              true
                                              (original-installed? artifact dir)))
                        config/extension-env-value (fn [name]
                                                     (case name
                                                       "VIS_DECISION_GLINER_TRAINING_PYTHON"
                                                       python

                                                       "VIS_DECISION_TRAINING_DATA_ROOT"
                                                       (.getPath data)

                                                       nil))]

            (cache/enable!)
            (cache/release-idle!)
            (let [created (jobs/create! (assoc request "model_id" model-id))
                  first-id (get created "job_id")
                  first-status (await-terminal first-id)]

              (is (= model-id (get created "model_id")))
              (is (= "completed" (get first-status "status")) (str first-status))
              (is (nil? (registry/get-alias "active")))
              (when (= "completed" (get first-status "status"))
                (let [ref (get first-status "model_ref")
                      answer (decisions/infer! {"model" ref
                                                "state" "Please refund this purchase"
                                                "questions" {"intent" question}})
                      _ (registry/activate! "active" ref nil)
                      next-id (get (jobs/create! (assoc request
                                                   "model_id" model-id
                                                   "source_job_id" first-id))
                                   "job_id")
                      second-status (await-terminal next-id)]

                  (is (= model-id (get-in (registry/resolve-model ref) [:model :id])))
                  (is (= ref (get-in answer ["routing" "model_ref"])))
                  (is (number? (get-in answer ["answers" "intent" "action" "act_probability"])))
                  (is (.isFile
                        (io/file models "jobs" first-id "output/checkpoint/PROVENANCE.json")))
                  (is (= "completed" (get second-status "status")) (str second-status))
                  (is (= model-id (get second-status "model_id")))
                  (is (= ref (get (registry/get-alias "active") "model_ref")))))))
          (finally (jobs/stop!) (cache/release-idle!) (files/delete-dir! root)))))))
