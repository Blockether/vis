(ns com.blockether.vis.internal.decisions.jobs
  "One bounded, offline Python trainer per gateway; durable private job results."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.decisions.cache :as cache]
            [com.blockether.vis.internal.decisions.core :as decisions]
            [com.blockether.vis.internal.decisions.registry :as registry]
            [com.blockether.vis.internal.speech.files :as files])
  (:import [java.io BufferedReader File InputStreamReader]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files LinkOption Path StandardCopyOption]
           [java.util.concurrent TimeUnit]))

(set! *warn-on-reflection* true)

(def ^:private lock (Object.))

(defonce ^:private active (atom nil))

(def ^:private max-saved-jobs 4)

(def ^:private training-models #{"laya-typed-decisions" "gliner2.5-base" "gliner2.5-decide"})

(def ^:private inputs
  {"train_data" ["train.jsonl" 16777216]
   "eval_data" ["eval.jsonl" 16777216]
   "training_config" ["config.json" 16384]
   "validation_policy" ["policy.json" 16384]})

(def ^:private stages
  #{"loading" "training" "checkpoint_saved" "exporting" "validated" "publishing" "completed"})

(defn- invalid!
  []
  (throw (ex-info "Invalid or unavailable decision training input"
                  {:type :decisions/invalid-request})))

(defn- busy!
  []
  (throw (ex-info "Decision training capacity is exhausted" {:type :decisions/capacity-exceeded})))

(defn- jobs-root ^File [] (io/file (assets/models-root) "jobs"))

(defn- valid-id?
  [id]
  (and (string? id) (boolean (re-matches #"[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}" id))))

(defn- directory ^File [id] (io/file (jobs-root) id))

(defn- status-file ^File [id] (io/file (directory id) "status.json"))

(defn- write-status!
  [id status]
  (let [dir
        (directory id)

        temporary
        (File/createTempFile ".decision-status-" ".json" dir)]

    (try (spit temporary (str (wire/json-str status) "\n"))
         (Files/move (.toPath temporary)
                     (.toPath (status-file id))
                     (into-array StandardCopyOption
                                 [StandardCopyOption/ATOMIC_MOVE
                                  StandardCopyOption/REPLACE_EXISTING]))
         status
         (finally (.delete temporary)))))

(defn- read-status
  [id]
  (when (valid-id? id)
    (let [file (status-file id)]
      (when (.isFile file) (wire/parse-json (slurp file))))))

(defn get!
  "Read a private job's persisted, path-free status. Interrupted jobs never resume."
  [id]
  (locking lock
    (when-let [status (read-status id)]
      (if (and (contains? #{"running" "cancelling" "registering"} (get status "status"))
               (not= id (:id @active)))
        (write-status! id
                       (assoc status
                         "status" "failed"
                         "stage" "interrupted"
                         "error" "Gateway stopped during training"))
        status))))

(defn- setting
  [name default lower upper]
  (try (let [value (Long/parseLong (str (config/extension-env-value name)))]
         (if (<= lower value upper) value default))
       (catch Exception _ default)))

(defn- settings
  [model-id]
  (let [python
        (config/extension-env-value (if (= model-id "laya-typed-decisions")
                                      "VIS_DECISION_TRAINING_PYTHON"
                                      "VIS_DECISION_GLINER_TRAINING_PYTHON"))

        root
        (config/extension-env-value "VIS_DECISION_TRAINING_DATA_ROOT")]

    (when-not (and (string? python)
                   (string? root)
                   (.isFile (io/file python))
                   (.canExecute (io/file python))
                   (.isDirectory (io/file root)))
      (throw (ex-info "Offline decision trainer and approved data root must be configured"
                      {:type :decisions/training-unavailable})))
    {:python (.getAbsolutePath (io/file python))
     :data-root (.toRealPath (.toPath (io/file root)) (make-array LinkOption 0))
     :timeout-s (setting "VIS_DECISION_TRAINING_TIMEOUT_S" 7200 60 21600)}))

(defn- input-file
  ^File [^Path root request key]
  (let [[_ max-size]
        (get inputs key)

        name
        (get request key)]

    (when-not (and (string? name)
                   (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" name)
                   (if (contains? #{"train_data" "eval_data"} key)
                     (.endsWith ^String name ".jsonl")
                     (.endsWith ^String name ".json")))
      (invalid!))
    (let [path
          (.resolve root ^String name)

          file
          (.toFile path)]

      (when (or (Files/isSymbolicLink path)
                (not (.isFile file))
                (not= root (.getParent (.toRealPath path (make-array LinkOption 0))))
                (zero? (.length file))
                (> (.length file) (long max-size)))
        (invalid!))
      file)))

(defn- checkpoint
  ^File [request model-id]
  (if-let [source (get request "source_job_id")]
    (do (when-not (valid-id? source) (invalid!))
        (let [prior (get! source)
              path (io/file (directory source) "output" "checkpoint")]

          (when-not (and (= model-id (get prior "model_id"))
                         (contains? #{"completed" "failed"} (get prior "status"))
                         (.isFile (io/file path "PROVENANCE.json")))
            (invalid!))
          path))
    (let [model (assets/entry model-id)
          dir (assets/install-dir model :training)]

      (when-not (assets/installed? (assets/artifact model :training) dir)
        (throw (ex-info (str "Install the pinned " model-id " training checkpoint first")
                        {:type :decisions/model-not-installed})))
      (io/file dir))))

(defn- cancelled? [id] (locking lock (boolean (and (= id (:id @active)) @(:cancelled? @active)))))

(defn- progress!
  [id event]
  (when (and (map? event) (contains? stages (get event "stage")))
    (locking lock
      (when (and (= id (:id @active)) (not (cancelled? id)))
        (let [previous
              (read-status id)

              step
              (get event "step")

              limit
              (get event "max_steps")]

          (when (and previous
                     (or (nil? step) (and (integer? step) (<= 0 step 100000)))
                     (or (nil? limit) (and (integer? limit) (<= 1 limit 100000))))
            (write-status! id
                           (cond-> (assoc previous "stage" (get event "stage"))
                             (some? step)
                             (assoc "step" step)

                             (some? limit)
                             (assoc "max_steps" limit)))))))))

(defn- execute-process!
  [^File spec progress cancelled]
  (let [python
        (:python (:settings @active))

        timeout
        (:timeout-s (:settings @active))

        builder
        (ProcessBuilder.
          ^"[Ljava.lang.String;"
          (into-array String [python "-I" "-m" "blockether.vis.decisions._worker" (.getPath spec)]))

        environment
        (.environment builder)]

    (.clear environment)
    (doseq [[name value] {"HOME" (System/getProperty "user.home")
                          "PATH" (or (System/getenv "PATH") "/usr/bin:/bin")
                          "LANG" "C.UTF-8"
                          "HF_HUB_OFFLINE" "1"
                          "TRANSFORMERS_OFFLINE" "1"
                          "HF_DATASETS_OFFLINE" "1"
                          "CUDA_VISIBLE_DEVICES" ""
                          "OMP_NUM_THREADS" "4"}]
      (.put environment name value))
    (.redirectError builder java.lang.ProcessBuilder$Redirect/DISCARD)
    (let [process
          (.start builder)

          reader
          (future (with-open [stream (BufferedReader. (InputStreamReader. (.getInputStream process)
                                                                          StandardCharsets/UTF_8))]
                    (loop []

                      (when-let [line (.readLine stream)]
                        (when (< (count line) 1024)
                          (try (progress (wire/parse-json line)) (catch Exception _ nil)))
                        (recur)))))]

      (locking lock
        (when (= (.getName (.getParentFile spec)) (:id @active))
          (reset! (:process @active) process)))
      (try (when (cancelled) (.destroyForcibly process))
           (when-not (.waitFor process (long timeout) TimeUnit/SECONDS)
             (.destroyForcibly process)
             (throw (ex-info "Decision trainer timed out" {:type :decisions/training-timeout})))
           (when (cancelled)
             (throw (ex-info "Decision training was cancelled" {:type :decisions/cancelled})))
           (when-not (zero? (.exitValue process))
             (throw (ex-info "Decision trainer failed; inspect local gateway logs"
                             {:type :decisions/training-failed})))
           (let [result (io/file (.getParentFile spec) "result.json")]
             (when-not (.isFile result)
               (throw (ex-info "Decision trainer did not produce a result"
                               {:type :decisions/training-failed})))
             (wire/parse-json (slurp result)))
           (finally (deref reader 3000 nil)
                    (locking lock
                      (when (= (.getName (.getParentFile spec)) (:id @active))
                        (reset! (:process @active) nil))))))))

(def ^:dynamic *execute!* execute-process!)

(defn- valid-result?
  [result ^File archive]
  (and (map? result)
       (re-matches #"[0-9a-f]{64}" (str (get result "sha256")))
       (integer? (get result "bytes"))
       (= (long (get result "bytes")) (.length archive))
       (every?
         (fn [name]
           (let [value (get result name)]
             (and (number? value) (<= 0.0 (double value) 1.0) (Double/isFinite (double value)))))
         ["decision_accuracy" "action_accuracy"])))

(defn- cleanup-inference!
  [id]
  (let [dir (directory id)]
    (.delete (io/file dir "inference.zip"))
    (let [inference (io/file dir "output" "inference")]
      (when (.exists inference) (files/delete-dir! inference)))))

(defn- run-job!
  [id]
  (let [dir
        (directory id)

        archive
        (io/file dir "inference.zip")]

    (try (let [result (*execute!* (io/file dir "spec.json") #(progress! id %) #(cancelled? id))]
           (when (cancelled? id)
             (throw (ex-info "Decision training was cancelled" {:type :decisions/cancelled})))
           (when-not (valid-result? result archive)
             (throw (ex-info "Decision trainer result is incomplete"
                             {:type :decisions/training-failed})))
           (locking lock
             (write-status! id
                            (assoc (read-status id)
                              "status" "registering"
                              "stage" "registering")))
           ;; Python exited and released its tensors. Registration now uses the
           ;; ordinary bounded JVM runtime and never activates an existing alias.
           (cache/end-training!)
           (let [model-id (get (read-status id) "model_id")
                 registered (registry/register!
                              archive
                              (get result "sha256")
                              (fn [model inference]
                                (when-not (= model-id (:id model))
                                  (throw (ex-info "Decision trainer returned a different model"
                                                  {:type :decisions/invalid-bundle})))
                                (decisions/validate-runtime! model inference)))]

             (cleanup-inference! id)
             (locking lock
               (write-status! id
                              (assoc (read-status id)
                                "status" "completed"
                                "stage" "completed"
                                "model_ref" (get registered "model_ref")
                                "metrics" {"decision_accuracy" (get result "decision_accuracy")
                                           "action_accuracy" (get result "action_accuracy")})))))
         (catch Throwable _
           (cleanup-inference! id)
           (locking lock
             (when-let [status (read-status id)]
               (write-status! id
                              (assoc status
                                "status" (if (cancelled? id) "cancelled" "failed")
                                "stage" (if (cancelled? id) "cancelled" "failed")
                                "error" (if (cancelled? id)
                                          "Decision training was cancelled"
                                          "Decision training or validation failed"))))))
         (finally (cache/end-training!)
                  (locking lock (when (= id (:id @active)) (reset! active nil)))))))

(defn create!
  "Stage bounded approved inputs, then run one isolated offline CPU trainer.
   A registered model is never activated without a separate alias CAS."
  [request]
  (when-not (and (map? request)
                 (= (set (keys request))
                    (cond-> (set (keys inputs))
                      (contains? request "source_job_id")
                      (conj "source_job_id")

                      (contains? request "model_id")
                      (conj "model_id")))
                 (contains? training-models (get request "model_id" "laya-typed-decisions")))
    (invalid!))
  (let [model-id
        (get request "model_id" "laya-typed-decisions")

        settings
        (settings model-id)

        files
        (into {}
              (map (fn [name]
                     [name (input-file (:data-root settings) request name)])
                   (keys inputs)))

        source
        (checkpoint request model-id)]

    (locking lock
      (when @active (busy!))
      (.mkdirs (jobs-root))
      (when (>= (count (filter #(.isFile (io/file ^File % "status.json")) (.listFiles (jobs-root))))
                max-saved-jobs)
        (busy!))
      (cache/begin-training!)
      (let [id
            (str (random-uuid))

            dir
            (directory id)]

        (try
          (.mkdirs dir)
          (let [input-dir (io/file dir "input")]
            (.mkdirs input-dir)
            (let [paths (into {}
                              (for [[name [filename max-size]] inputs]
                                (let [copied (io/file input-dir filename)]
                                  (with-open [in (io/input-stream (get files name))
                                              out (io/output-stream copied)]

                                    (loop [total 0]
                                      (let [buffer (byte-array 65536)
                                            n (.read in buffer)]

                                        (when (pos? n)
                                          (let [next (+ total n)]
                                            (when (> next max-size) (invalid!))
                                            (.write out buffer 0 n)
                                            (recur next))))))
                                  [name (.getPath copied)])))]
              (spit (io/file dir "spec.json")
                    (str (wire/json-str (merge paths
                                               {"model_id" model-id
                                                "checkpoint" (.getPath source)
                                                "output_dir" (str (io/file dir "output"))
                                                "archive" (str (io/file dir "inference.zip"))
                                                "result" (str (io/file dir "result.json"))}))
                         "\n"))))
          (write-status! id {"job_id" id "model_id" model-id "status" "running" "stage" "staging"})
          (reset! active {:id id :settings settings :cancelled? (atom false) :process (atom nil)})
          (let [worker (future (run-job! id))]
            (when (= id (:id @active)) (swap! active assoc :future worker)))
          (get! id)
          (catch Throwable error
            (when (= id (:id @active)) (reset! active nil))
            (cache/end-training!)
            (when (.exists dir) (files/delete-dir! dir))
            (throw error)))))))

(defn delete!
  "Cancel an active job, or explicitly discard a terminal private checkpoint."
  [id]
  (locking lock
    (when-let [status (get! id)]
      (if (= id (:id @active))
        (do (reset! (:cancelled? @active) true)
            (when-let [^Process process @(:process @active)]
              (.destroyForcibly process))
            (write-status! id
                           (assoc status
                             "status" "cancelling"
                             "stage" "cancelling")))
        (do (when (.exists (directory id)) (files/delete-dir! (directory id)))
            {"job_id" id "status" "deleted"})))))

(defn stop!
  "Stop the gateway-owned worker before releasing the JVM model cache."
  []
  (let [worker (locking lock
                 (when-let [{:keys [id future]} @active]
                   (delete! id)
                   future))]
    (when worker (deref worker 5000 nil))
    nil))
