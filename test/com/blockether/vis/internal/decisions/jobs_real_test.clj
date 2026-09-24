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
