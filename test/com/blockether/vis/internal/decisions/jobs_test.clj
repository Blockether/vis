(ns com.blockether.vis.internal.decisions.jobs-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.wire :as wire]
            [lazytest.core :refer [defdescribe expect it]]
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

(defdescribe
  jobs-run-with-exclusive-cache-and-persist-private-checkpoints
  (it
    "jobs run with exclusive cache and persist private checkpoints"
    (fixture
      (fn [_ _ _]
        (let [registered
              (atom [])

              source
              (atom [])]

          (with-redefs [registry/register!
                        (fn [archive digest validate!]
                          (expect (.isFile archive))
                          (expect (ifn? validate!))
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
                                          (expect (false? (cancelled?)))
                                          {"sha256" (apply str (repeat 64 "a"))
                                           "bytes" 9
                                           "decision_accuracy" 0.8
                                           "action_accuracy" 0.7}))]
              (let [created (jobs/create! request)
                    id (get created "job_id")
                    done (await-status id "completed")]

                (expect (= "completed" (get done "status")))
                (expect (= 1 (get done "step")))
                (expect (= 0.8 (get-in done ["metrics" "decision_accuracy"])))
                (expect (= (str "sha256-" (apply str (repeat 64 "a"))) (get done "model_ref")))
                (expect (not (.exists (io/file (assets/models-root) "jobs" id "inference.zip"))))
                (expect
                  (.isFile
                    (io/file (assets/models-root) "jobs" id "output/checkpoint/PROVENANCE.json")))
                (expect (not (contains? done "train_data")))
                (expect (= done (jobs/get! id)))
                (expect (= "completed"
                           (get (await-status (get (jobs/create! (assoc request "source_job_id" id))
                                                   "job_id")
                                              "completed")
                                "status")))
                (expect (= 2 (count @registered)))
                (expect (= (str (io/file (assets/models-root) "jobs" id "output/checkpoint"))
                           (second @source)))
                (expect (= "deleted" (get (jobs/delete! id) "status")))
                (expect (nil? (jobs/get! id)))))))))))

(defdescribe
  jobs-reject-unapproved-inputs-and-cancel-a-running-job
  (it
    "jobs reject unapproved inputs and cancel a running job"
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
            (expect (= :decisions/invalid-request
                       (:type (ex-data (try (jobs/create! (assoc request
                                                            "train_data" "../outside.jsonl"))
                                            (catch clojure.lang.ExceptionInfo e e))))))
            (expect (= :decisions/invalid-request
                       (:type (ex-data (try (jobs/create! (assoc request
                                                            "train_data" "missing.jsonl"))
                                            (catch clojure.lang.ExceptionInfo e e))))))
            (java.nio.file.Files/createSymbolicLink
              (.toPath (io/file data "linked.jsonl"))
              (.toPath (io/file data "train.jsonl"))
              (make-array java.nio.file.attribute.FileAttribute 0))
            (expect (= :decisions/invalid-request
                       (:type (ex-data (try (jobs/create! (assoc request
                                                            "train_data" "linked.jsonl"))
                                            (catch clojure.lang.ExceptionInfo e e))))))
            (let [id (get (jobs/create! request) "job_id")]
              (expect (= true (deref entered 3000 nil)))
              (expect (= :decisions/capacity-exceeded
                         (:type (ex-data (try (jobs/create! request)
                                              (catch clojure.lang.ExceptionInfo e e))))))
              (expect (= :decisions/capacity-exceeded
                         (:type (ex-data (try (cache/with-resident! :other
                                                                    0
                                                                    (fn []
                                                                      {:close (fn [])})
                                                                    identity)
                                              (catch clojure.lang.ExceptionInfo e e))))))
              (expect (= "cancelling" (get (jobs/delete! id) "status")))
              (deliver release true)
              (expect (= "cancelled" (get (await-status id "cancelled") "status")))
              (expect (= :ok
                         (cache/with-resident! :other
                                               0
                                               (fn []
                                                 {:close (fn [])})
                                               (constantly :ok)))))))))))

(defdescribe
  terminal-job-status-follows-training-reservation-release
  (it
    "terminal job status follows training reservation release"
    (doseq [cancel? [false true]]
      (fixture
        (fn [_ _ _]
          (let [id (atom nil)
                entered (promise)
                release (promise)
                releasing-status (promise)
                end-training! cache/end-training!]

            (with-redefs [cache/end-training! (fn []
                                                ;; Observe the ordering in the worker, without timing a polling race.
                                                (deliver releasing-status
                                                         (get (jobs/get! @id) "status"))
                                                (end-training!))]
              (binding [jobs/*execute!* (fn [_ _ _]
                                          (deliver entered true)
                                          @release
                                          (throw (ex-info "synthetic training failure" {})))]
                (try (reset! id (get (jobs/create! request) "job_id"))
                     (expect (= true (deref entered 3000 nil)))
                     (when cancel? (jobs/delete! @id))
                     (deliver release true)
                     (expect (= (if cancel? "cancelling" "running")
                                (deref releasing-status 3000 nil)))
                     (let [terminal (if cancel? "cancelled" "failed")]
                       (expect (= terminal (get (await-status @id terminal) "status"))))
                     (expect (= :ok
                                (cache/with-resident! :after-terminal
                                                      0
                                                      (fn []
                                                        {:close (fn [])})
                                                      (constantly :ok))))
                     (let [next-id (get (jobs/create! request) "job_id")]
                       (expect (= "failed" (get (await-status next-id "failed") "status"))))
                     (finally (deliver release true)))))))))))

(defdescribe
  gliner-jobs-select-approved-checkpoint-and-reject-cross-family-resume
  (it
    "gliner jobs select approved checkpoint and reject cross family resume"
    (fixture
      (fn [_ _ baseline]
        (let [selected
              (atom [])

              executed
              (atom [])

              digest
              (apply str (repeat 64 "b"))]

          (with-redefs [assets/entry
                        (fn [id]
                          (if (contains? assets/gliner-architectures id)
                            {:id id}
                            (throw (ex-info "Unknown model" {:type :decisions/unknown-model}))))

                        assets/artifact
                        (fn [model kind]
                          (expect (= :training kind))
                          {:id (:id model)})

                        assets/install-dir
                        (fn [model kind]
                          (expect (= :training kind))
                          (swap! selected conj (:id model))
                          (.getPath baseline))

                        assets/installed?
                        (fn [& _]
                          true)

                        registry/register!
                        (fn [_ _ _]
                          {"model_ref" (str "sha256-" digest)})

                        registry/activate!
                        (fn [& _]
                          (throw (ex-info "Training must not change aliases" {})))

                        decisions/validate-runtime!
                        (fn [& _])]

            (binding [jobs/*execute!*
                      (fn [spec progress _]
                        (let [description (wire/parse-json (slurp spec))
                              checkpoint (io/file (get description "output_dir") "checkpoint")]

                          (swap! executed conj description)
                          (.mkdirs checkpoint)
                          (spit (io/file checkpoint "PROVENANCE.json")
                                (wire/json-str {"model" (get description "model_id")}))
                          (spit (get description "archive") "fp32-only")
                          (progress {"stage" "training" "step" 1 "max_steps" 1})
                          {"sha256" digest
                           "bytes" 9
                           "decision_accuracy" 0.75
                           "action_accuracy" 0.5}))]
              (doseq [model-id (keys assets/gliner-architectures)]
                (let [created (jobs/create! (assoc request "model_id" model-id))
                      id (get created "job_id")
                      done (await-status id "completed")]

                  (expect (= model-id (get done "model_id")))
                  (expect (= "completed" (get done "status")))
                  (expect (= done (jobs/get! id)))
                  (expect (= 1 (get done "step")))
                  (expect (= :decisions/invalid-request
                             (:type (ex-data (try (jobs/create! (assoc request
                                                                  "model_id"
                                                                  (if (= model-id "gliner2.5-base")
                                                                    "gliner2.5-decide"
                                                                    "gliner2.5-base")
                                                                  "source_job_id" id))
                                                  (catch clojure.lang.ExceptionInfo error
                                                    error))))))
                  (let [resumed (jobs/create! (assoc request
                                                "model_id" model-id
                                                "source_job_id" id))]
                    (expect (= "completed"
                               (get (await-status (get resumed "job_id") "completed") "status")))
                    ;; Delete finished jobs so every GLiNER model fits under the saved-job limit.
                    (doseq [job [(get resumed "job_id") id]]
                      (expect (= "deleted" (get (jobs/delete! job) "status")))))))
              (expect (= (mapcat #(repeat 2 %) (keys assets/gliner-architectures))
                         (mapv #(get % "model_id") @executed)))
              (expect (= (vec (keys assets/gliner-architectures)) @selected)))))))))

(defdescribe
  decision2-jobs-report-no-action-accuracy
  (it
    "decision2 jobs complete without action accuracy, and other families still need it"
    (fixture
      (fn [_ _ baseline]
        (let [digest (apply str (repeat 64 "c"))]
          (with-redefs [assets/entry (fn [id]
                                       {:id id})
                        assets/artifact (fn [model _]
                                          {:id (:id model)})
                        assets/install-dir (fn [_ _]
                                             (.getPath baseline))
                        assets/installed? (fn [& _]
                                            true)
                        registry/register! (fn [_ _ _]
                                             {"model_ref" (str "sha256-" digest)})
                        decisions/validate-runtime! (fn [& _])]

            (binding [jobs/*execute!*
                      (fn [spec _ _]
                        (spit (get (wire/parse-json (slurp spec)) "archive") "fp32-only")
                        {"sha256" digest "bytes" 9 "decision_accuracy" 0.75 "action_accuracy" nil})]
              (let [created (jobs/create! (assoc request "model_id" "decision2.0-eos-0.8b"))
                    done (await-status (get created "job_id") "completed")]

                (expect (= {"decision_accuracy" 0.75 "action_accuracy" nil} (get done "metrics"))))
              (let [created (jobs/create! (assoc request "model_id" "gliner2.5-base"))
                    failed (await-status (get created "job_id") "failed")]

                (expect (= "failed" (get failed "status")))))))))))

(defdescribe
  gliner-jobs-fail-closed-without-their-interpreter-or-valid-model
  (it "gliner jobs fail closed without their interpreter or valid model"
      (fixture
        (fn [_ _ _]
          (let [read-env config/extension-env-value]
            (with-redefs [config/extension-env-value
                          (fn [name]
                            (when-not (= "VIS_DECISION_TRAINING_PYTHON" name) (read-env name)))]
              (expect (= :decisions/training-unavailable
                         (:type (ex-data (try (jobs/create! (assoc request
                                                              "model_id" "gliner2.5-base"))
                                              (catch clojure.lang.ExceptionInfo error error)))))))
            (expect (= :decisions/invalid-request
                       (:type (ex-data (try (jobs/create! (assoc request "model_id" "unknown"))
                                            (catch clojure.lang.ExceptionInfo error error)))))))))))

(defdescribe
  interrupted-gliner-job-keeps-identity-and-existing-alias
  (it
    "interrupted gliner job keeps identity and existing alias"
    (fixture
      (fn [_ _ baseline]
        (let [ref
              (str "sha256-" (apply str (repeat 64 "c")))

              alias-file
              (io/file (assets/models-root) "aliases.json")

              id
              (str (random-uuid))

              status-file
              (io/file (assets/models-root) "jobs" id "status.json")]

          (spit alias-file (wire/json-str {"active" ref}))
          (.mkdirs (.getParentFile status-file))
          (spit
            status-file
            (wire/json-str
              {"job_id" id "model_id" "gliner2.5-decide" "status" "running" "stage" "training"}))
          (expect (= "failed" (get (jobs/get! id) "status")))
          (expect (= "interrupted" (get (jobs/get! id) "stage")))
          (expect (= "gliner2.5-decide" (get (jobs/get! id) "model_id")))
          (expect (= ref (get (registry/get-alias "active") "model_ref")))
          (with-redefs [assets/entry
                        (fn [model-id]
                          {:id model-id})

                        assets/artifact
                        (fn [_ _]
                          {:kind :training})

                        assets/install-dir
                        (fn [_ _]
                          (.getPath baseline))

                        assets/installed?
                        (fn [& _]
                          true)]

            (let [entered
                  (promise)

                  release
                  (promise)]

              (binding [jobs/*execute!* (fn [_ progress cancelled?]
                                          (progress {"stage" "training" "step" 1 "max_steps" 2})
                                          (deliver entered true)
                                          @release
                                          (when (cancelled?) (throw (ex-info "cancelled" {}))))]
                (let [current (get (jobs/create! (assoc request "model_id" "gliner2.5-base"))
                                   "job_id")]
                  (expect (= true (deref entered 3000 nil)))
                  (expect (= 1 (get (jobs/get! current) "step")))
                  (expect (= ref (get (registry/get-alias "active") "model_ref")))
                  (expect (= "cancelling" (get (jobs/delete! current) "status")))
                  (deliver release true)
                  (let [cancelled (await-status current "cancelled")]
                    (expect (= "cancelled" (get cancelled "status")))
                    (expect (= "gliner2.5-base" (get cancelled "model_id")))
                    (expect (not (contains? cancelled "model_ref"))))
                  (expect (= ref (get (registry/get-alias "active") "model_ref"))))))))))))
