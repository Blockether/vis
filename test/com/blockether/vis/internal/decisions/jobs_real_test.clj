(ns com.blockether.vis.internal.decisions.jobs-real-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.decisions.cache :as cache]
            [com.blockether.vis.internal.decisions.core :as decisions]
            [com.blockether.vis.internal.decisions.jobs :as jobs]
            [com.blockether.vis.internal.decisions.registry :as registry]
            [com.blockether.vis.internal.speech.files :as files]
            [com.blockether.vis.test-network-guard :as network-guard]))

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

(defdescribe
  installed-sdk-worker-trains-registers-and-resumes-offline
  (it
    "installed sdk worker trains registers and resumes offline"
    ;; Full gate: -Dvis.test.laya.training.dir and -Dvis.test.decisions.training.python
    ;; point at the verified release checkpoint and the unified offline environment.
    (when-let [checkpoint (System/getProperty "vis.test.laya.training.dir")]
      (let [python (System/getProperty "vis.test.decisions.training.python")
            root (io/file (System/getProperty "java.io.tmpdir")
                          (str "decision-real-training-" (random-uuid)))
            data (io/file root "approved-data")
            models (io/file root "models")
            question
            {"type" "choice" "instructions" "Choose a request" "criteria" ["refund" "repair"]}
            example
            {"state" "Please refund my damaged purchase" "question" question "target" 0 "action" 0}
            original-install-dir assets/install-dir]

        (expect (some? python))
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

              (expect (= "completed" (get first-status "status")) (str first-status))
              (when (= "completed" (get first-status "status"))
                (let [ref (get first-status "model_ref")
                      answer (decisions/infer! {"model" ref
                                                "state" "broken item refund"
                                                "questions" {"intent"
                                                             {"type" "choice"
                                                              "instructions" "Choose intent"
                                                              "criteria" ["refund" "repair"]}}})
                      next-id (get (jobs/create! (assoc request "source_job_id" first-id)) "job_id")
                      second-status (await-terminal next-id)]

                  (expect (= ref (get-in answer ["routing" "model_ref"])))
                  (expect (number? (get-in answer ["answers" "intent" "action" "act_probability"])))
                  (expect (.isFile
                            (io/file models "jobs" first-id "output/checkpoint/PROVENANCE.json")))
                  (expect (= "completed" (get second-status "status")) (str second-status))
                  (expect (<= 1 (count (registry/versions))))))))
          (finally (jobs/stop!) (cache/release-idle!) (files/delete-dir! root)))))))

(defn- train-and-resume!
  "Train, register, answer and resume one GLiNER or Decision 2.0 model in a new store.
   Without `checkpoint`, first install the pinned FP32 bundle and training checkpoint with the
   call behind `vis-agent decisions models download --training`."
  [python model-id checkpoint]
  (let [decision2?
        (contains? assets/decision2-architectures model-id)

        local?
        (fn [kind]
          (and (some? checkpoint) (= kind :training)))

        root
        (io/file (System/getProperty "java.io.tmpdir") (str "decision-gliner-job-" (random-uuid)))

        data
        (io/file root "approved-data")

        models
        (io/file root "models")

        question
        {"type" "choice"
         "instructions" "Choose intent"
         "criteria" ["refund_request" "order_status" "other"]}

        example
        {"state" "Please refund this purchase" "question" question "target" 0 "action" 1}

        original-entry
        assets/entry

        original-artifact
        assets/artifact

        original-install-dir
        assets/install-dir

        original-installed?
        assets/installed?]

    (.mkdirs data)
    (.mkdirs models)
    (spit (io/file data "train.jsonl")
          (str (wire/json-str (assoc example "state" "Please refund my order")) "\n"))
    (spit (io/file data "eval.jsonl") (str (wire/json-str example) "\n"))
    (spit (io/file data "config.json")
          (str (wire/json-str {"epochs" 1 "max_steps" 1 "encoder_lr" 0.00001 "task_lr" 0.0005})
               "\n"))
    (spit (io/file data "policy.json")
          ;; Decision 2.0 has no action head, so its trainer rejects an action gate.
          (str (wire/json-str (cond-> {"min_decision_accuracy" 0}
                                (not decision2?)
                                (assoc "min_action_accuracy" 0)))
               "\n"))
    (try
      (with-redefs [assets/models-root
                    (constantly (.getPath models))

                    assets/entry
                    (fn [id]
                      (if (and checkpoint (= id model-id)) {:id id} (original-entry id)))

                    assets/artifact
                    (fn [model kind]
                      (if (local? kind) {:kind :training} (original-artifact model kind)))

                    assets/install-dir
                    (fn [model kind]
                      (if (local? kind) checkpoint (original-install-dir model kind)))

                    assets/installed?
                    (fn [artifact dir]
                      (if (local? (:kind artifact)) true (original-installed? artifact dir)))

                    config/extension-env-value
                    (fn [name]
                      (case name
                        "VIS_DECISION_TRAINING_PYTHON"
                        python

                        "VIS_DECISION_TRAINING_DATA_ROOT"
                        (.getPath data)

                        nil))]

        (when-not checkpoint
          (let [model
                (assets/entry model-id)

                installed
                (binding [network-guard/*allow-network* true]
                  (assets/download-model! model-id true))]

            (doseq [kind [:inference :training]]
              (expect (= (assets/install-dir model kind) (get installed kind)))
              (expect (assets/installed? (assets/artifact model kind) (get installed kind))))))
        (cache/enable!)
        (cache/release-idle!)
        (let [created
              (jobs/create! (assoc request "model_id" model-id))

              first-id
              (get created "job_id")

              first-status
              (await-terminal first-id)]

          (expect (= model-id (get created "model_id")))
          (expect (= "completed" (get first-status "status")) (str first-status))
          (expect (nil? (registry/get-alias "active")))
          (when (= "completed" (get first-status "status"))
            (let [ref
                  (get first-status "model_ref")

                  answer
                  (decisions/infer! {"model" ref
                                     "state" "Please refund this purchase"
                                     "questions" {"intent" question}})

                  _
                  (registry/activate! "active" ref nil)

                  next-id
                  (get (jobs/create! (assoc request
                                       "model_id" model-id
                                       "source_job_id" first-id))
                       "job_id")

                  second-status
                  (await-terminal next-id)]

              (expect (= model-id (get-in (registry/resolve-model ref) [:model :id])))
              (expect (= ref (get-in answer ["routing" "model_ref"])))
              (if decision2?
                (do (expect (contains? (set (get question "criteria"))
                                       (get-in answer ["answers" "intent" "choice"])))
                    (expect (nil? (get-in answer ["answers" "intent" "action"]))))
                (expect (number? (get-in answer ["answers" "intent" "action" "act_probability"]))))
              (expect (.isFile
                        (io/file models "jobs" first-id "output/checkpoint/PROVENANCE.json")))
              (expect (= "completed" (get second-status "status")) (str second-status))
              (expect (= model-id (get second-status "model_id")))
              (expect (= ref (get (registry/get-alias "active") "model_ref")))))))
      (finally (jobs/stop!) (cache/release-idle!) (files/delete-dir! root)))))

(defdescribe installed-worker-trains-registers-and-resumes-gliner-and-decision2-offline
             (it "installed worker trains registers and resumes gliner and decision2 offline"
                 ;; Full gate: -Dvis.test.decisions.training.python and at least one
                 ;; -Dvis.test.gliner.training.{base,small,multi,decide,multi-decide,decide-1b}.dir
                 ;; or -Dvis.test.decision2.training.{eos,kai}.dir.
                 ;; The directories hold verified full checkpoints, not encoder-only ONNX bundles.
                 ;; -Dvis.test.decisions.training.download=<comma-separated GLiNER or Decision 2.0 IDs>
                 ;; first downloads those pinned checkpoints from the catalog, as `--training` does.
                 (when-let [python (System/getProperty "vis.test.decisions.training.python")]
                   (doseq [[model-id property]
                           [["gliner2.5-base" "vis.test.gliner.training.base.dir"]
                            ["gliner2.5-small" "vis.test.gliner.training.small.dir"]
                            ["gliner2.5-multi" "vis.test.gliner.training.multi.dir"]
                            ["gliner2.5-decide" "vis.test.gliner.training.decide.dir"]
                            ["gliner2.5-multi-decide" "vis.test.gliner.training.multi-decide.dir"]
                            ["gliner2.5-decide-1b" "vis.test.gliner.training.decide-1b.dir"]
                            ["decision2.0-eos-0.8b" "vis.test.decision2.training.eos.dir"]
                            ["decision2.0-kai-0.6b" "vis.test.decision2.training.kai.dir"]]
                           :let [checkpoint (System/getProperty property)]
                           :when checkpoint]

                     (train-and-resume! python model-id checkpoint))
                   (doseq [model-id (some-> (System/getProperty
                                              "vis.test.decisions.training.download")
                                            (str/split #","))]
                     (train-and-resume! python (str/trim model-id) nil)))))
