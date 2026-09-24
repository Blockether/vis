(ns com.blockether.vis.internal.decisions.jobs-test
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

(defn- fixture
  [f]
  (let [root
        (io/file (System/getProperty "java.io.tmpdir") (str "decision-jobs-" (random-uuid)))

        models
        (io/file root "models")

        data
        (io/file root "approved-data")

        baseline
        (io/file root "baseline")

        binary
        (str (io/file (System/getProperty "java.home") "bin" "java"))]

    (.mkdirs models)
    (.mkdirs data)
    (.mkdirs baseline)
    (spit (io/file data "train.jsonl") "{\"private\":true}\n")
    (spit (io/file data "eval.jsonl") "{\"private\":false}\n")
    (spit (io/file data "config.json") "{\"epochs\":1}\n")
    (spit (io/file data "policy.json") "{\"min_decision_accuracy\":0}\n")
    (try
      (with-redefs [assets/models-root
                    (constantly (.getPath models))

                    assets/install-dir
                    (fn [& _]
                      (.getPath baseline))

                    assets/installed?
                    (fn [& _]
                      true)

                    config/extension-env-value
                    (fn [name]
                      (case name
                        "VIS_DECISION_TRAINING_PYTHON"
                        binary

                        "VIS_DECISION_TRAINING_DATA_ROOT"
                        (.getPath data)

                        nil))]

        (cache/enable!)
        (cache/release-idle!)
        (f root data baseline))
      (finally (jobs/stop!) (cache/end-training!) (cache/release-idle!) (files/delete-dir! root)))))

(def ^:private request
  {"train_data" "train.jsonl"
   "eval_data" "eval.jsonl"
   "training_config" "config.json"
   "validation_policy" "policy.json"})

(defn- await-status
  [id expected]
  (loop [remaining 300]
    (let [status (jobs/get! id)]
      (if (or (= expected (get status "status")) (zero? remaining))
        status
        (do (Thread/sleep 10) (recur (dec remaining)))))))

(deftest jobs-run-with-exclusive-cache-and-persist-private-checkpoints
  (fixture
    (fn [_ _ _]
      (let [registered
            (atom [])

            source
            (atom [])]

        (with-redefs [registry/register!
                      (fn [archive digest validate!]
                        (is (.isFile archive))
                        (is (ifn? validate!))
                        (swap! registered conj digest)
                        {"model_ref" (str "sha256-" digest) "installed" true})

                      decisions/validate-runtime!
                      (fn [& _])]

          (binding [jobs/*execute!* (fn [spec progress cancelled?]
                                      (let [spec (wire/parse-json (slurp spec))
                                            output (io/file (get spec "output_dir"))
                                            checkpoint (io/file output "checkpoint")]

                                        (swap! source conj (get spec "checkpoint"))
                                        (.mkdirs checkpoint)
                                        (spit (io/file checkpoint "PROVENANCE.json") "checkpoint")
                                        (spit (get spec "archive") "fp32-only")
                                        (progress {"stage" "training" "step" 1 "max_steps" 1})
                                        (is (false? (cancelled?)))
                                        {"sha256" (apply str (repeat 64 "a"))
                                         "bytes" 9
                                         "decision_accuracy" 0.8
                                         "action_accuracy" 0.7}))]
            (let [created (jobs/create! request)
                  id (get created "job_id")
                  done (await-status id "completed")]

              (is (= "completed" (get done "status")))
              (is (= 1 (get done "step")))
              (is (= 0.8 (get-in done ["metrics" "decision_accuracy"])))
              (is (= (str "sha256-" (apply str (repeat 64 "a"))) (get done "model_ref")))
              (is (not (.exists (io/file (assets/models-root) "jobs" id "inference.zip"))))
              (is (.isFile
                    (io/file (assets/models-root) "jobs" id "output/checkpoint/PROVENANCE.json")))
              (is (not (contains? done "train_data")))
              (is (= done (jobs/get! id)))
              (is (= "completed"
                     (get (await-status (get (jobs/create! (assoc request "source_job_id" id))
                                             "job_id")
                                        "completed")
                          "status")))
              (is (= 2 (count @registered)))
              (is (= (str (io/file (assets/models-root) "jobs" id "output/checkpoint"))
                     (second @source)))
              (is (= "deleted" (get (jobs/delete! id) "status")))
              (is (nil? (jobs/get! id))))))))))

(deftest jobs-reject-unapproved-inputs-and-cancel-a-running-job
  (fixture
    (fn [_ data _]
      (let [entered
            (promise)

            release
            (promise)]

        (binding [jobs/*execute!* (fn [_ _ cancelled?]
                                    (deliver entered true)
                                    @release
                                    (when (cancelled?) (throw (ex-info "cancelled" {})))
                                    (throw (ex-info "unexpected success" {})))]
          (is (= :decisions/invalid-request
                 (:type (ex-data (try (jobs/create! (assoc request "train_data" "../outside.jsonl"))
                                      (catch clojure.lang.ExceptionInfo e e))))))
          (is (= :decisions/invalid-request
                 (:type (ex-data (try (jobs/create! (assoc request "train_data" "missing.jsonl"))
                                      (catch clojure.lang.ExceptionInfo e e))))))
          (java.nio.file.Files/createSymbolicLink (.toPath (io/file data "linked.jsonl"))
                                                  (.toPath (io/file data "train.jsonl"))
                                                  (make-array java.nio.file.attribute.FileAttribute
                                                              0))
          (is (= :decisions/invalid-request
                 (:type (ex-data (try (jobs/create! (assoc request "train_data" "linked.jsonl"))
                                      (catch clojure.lang.ExceptionInfo e e))))))
          (let [id (get (jobs/create! request) "job_id")]
            (is (= true (deref entered 3000 nil)))
            (is (= :decisions/capacity-exceeded
                   (:type (ex-data (try (jobs/create! request)
                                        (catch clojure.lang.ExceptionInfo e e))))))
            (is (= :decisions/capacity-exceeded
                   (:type (ex-data (try (cache/with-resident! :other
                                                              (fn []
                                                                {:close (fn [])})
                                                              identity)
                                        (catch clojure.lang.ExceptionInfo e e))))))
            (is (= "cancelling" (get (jobs/delete! id) "status")))
            (deliver release true)
            (is (= "cancelled" (get (await-status id "cancelled") "status")))
            (is (= :ok
                   (cache/with-resident! :other
                                         (fn []
                                           {:close (fn [])})
                                         (constantly :ok))))))))))
