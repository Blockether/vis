(ns com.blockether.vis.internal.gateway.server.decisions
  "Decision-model routes: the model catalog, imports, aliases, training jobs and
   inference."
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.decisions.assets :as decision-assets]
            [com.blockether.vis.internal.decisions.core :as decisions]
            [com.blockether.vis.internal.decisions.jobs :as decision-jobs]
            [com.blockether.vis.internal.decisions.registry :as decision-registry]
            [com.blockether.vis.internal.gateway.server.http :as http]
            [com.blockether.vis.internal.util :as util])
  (:import [java.io InputStream OutputStream]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util.concurrent Semaphore]))

(defn- decisions-models-handler
  "GET /v1/decisions/models — installation and resident state, never a download."
  [_]
  (http/json-response {"models" (decisions/models-status)}))

(def ^:private decision-upload-slots (Semaphore. 1))

(defn- decision-mutation-error
  [^clojure.lang.ExceptionInfo e]
  (let [type
        (:type (ex-data e))

        status
        (case type
          :decisions/unsafe-path
          400

          :decisions/invalid-archive
          400

          :decisions/archive-checksum
          400

          :decisions/invalid-alias
          400

          :decisions/invalid-request
          400

          :decisions/model-not-installed
          409

          :decisions/training-unavailable
          503

          :decisions/unknown-model
          404

          :decisions/alias-conflict
          409

          :decisions/invalid-registry
          409

          :decisions/invalid-bundle
          422

          :decisions/capacity-exceeded
          503

          :decisions/unavailable
          503

          nil)]

    (if status
      (http/error-response status :decisions/error (.getMessage e) :reason (name type))
      (throw e))))

(defn- decision-import-handler
  "Stream one authenticated archive to disk; verify its declared digest before import."
  [request]
  (let [sha
        (get-in request [:headers "x-content-sha256"])

        length-header
        (get-in request [:headers "content-length"])

        declared
        (when length-header (parse-long length-header))

        limit
        (long decision-assets/max-inference-upload-bytes)]

    (cond (not (and (string? sha) (re-matches #"[0-9a-f]{64}" sha)))
          (http/error-response 400 :invalid-request "A SHA-256 upload digest is required")
          (and length-header (or (nil? declared) (neg? (long declared))))
          (http/error-response 400 :invalid-request "Invalid upload length")
          (and declared (> (long declared) limit)) (http/error-response
                                                     413
                                                     :decisions/upload-too-large
                                                     "Decision archive exceeds the upload limit")
          (not (.tryAcquire ^Semaphore decision-upload-slots))
          (http/error-response 503
                               :decisions/capacity-exceeded
                               "Another decision upload is in progress")
          :else (try
                  (let [root
                        (io/file (decision-assets/models-root) "registered")

                        _
                        (.mkdirs root)

                        archive
                        (java.io.File/createTempFile ".decision-upload-" ".zip" root)]

                    (try
                      (let [digest
                            (util/sha256-digest)

                            buffer
                            (byte-array 1048576)

                            count
                            (with-open [^InputStream in
                                        (:body request)

                                        out
                                        (io/output-stream archive)]

                              (loop [total 0]
                                (let [n (.read in buffer)]
                                  (if (neg? n)
                                    total
                                    (let [next (+ (long total) (long n))]
                                      (when (> next limit)
                                        (throw (ex-info "Decision archive exceeds the upload limit"
                                                        {:type :decisions/upload-too-large})))
                                      (.update ^MessageDigest digest buffer 0 n)
                                      (.write ^OutputStream out buffer 0 n)
                                      (recur next))))))]

                        (when (or (zero? count) (and declared (not= count (long declared))))
                          (throw (ex-info "Decision archive length does not match"
                                          {:type :decisions/invalid-archive})))
                        (when-not (= sha (util/bytes->hex (.digest ^MessageDigest digest)))
                          (throw (ex-info "Decision archive checksum failed"
                                          {:type :decisions/archive-checksum})))
                        (http/json-response
                          201
                          (decision-registry/register! archive sha decisions/validate-runtime!)))
                      (finally (.delete archive))))
                  (catch clojure.lang.ExceptionInfo e
                    (if (= :decisions/upload-too-large (:type (ex-data e)))
                      (http/error-response 413 :decisions/upload-too-large (.getMessage e))
                      (decision-mutation-error e)))
                  (finally (.release ^Semaphore decision-upload-slots))))))

(defn- decision-model-handler
  "Read an immutable ref after an ambiguous upload, without retrying the mutation."
  [request]
  (let [ref (get-in request [:path-params :model-ref])]
    (if-let [entry (some #(when (= ref (get % "model_ref")) %) (decisions/models-status))]
      (http/json-response entry)
      (http/error-response 404 :decisions/unknown-model "Decision model is not registered"))))

(defn- decision-alias-handler
  "Read or explicitly compare-and-swap an alias to a validated imported version."
  [request]
  (let [name (get-in request [:path-params :alias])]
    (if (= :get (:request-method request))
      (if-let [entry (decision-registry/get-alias name)]
        (http/json-response entry)
        (http/error-response 404 :decisions/unknown-model "Decision alias does not exist"))
      (let [^InputStream stream (:body request)
            bytes (when stream (.readNBytes stream 8193))]

        (if (or (nil? bytes) (> (alength ^bytes bytes) 8192))
          (http/error-response 413 :invalid-request "Decision alias request exceeds 8 KiB")
          (try (let [body (decisions/parse-body (String. ^bytes bytes StandardCharsets/UTF_8))]
                 (when-not (and (every? #{"model_ref" "expected_current"} (keys body))
                                (string? (get body "model_ref")))
                   (throw (ex-info "Decision alias requires an immutable model_ref"
                                   {:type :decisions/invalid-request})))
                 (http/json-response (decision-registry/activate! name
                                                                  (get body "model_ref")
                                                                  (get body "expected_current"))))
               (catch clojure.lang.ExceptionInfo e (decision-mutation-error e))))))))

(defn- decision-training-start-handler
  "Start one offline, gateway-owned training job from approved local data filenames."
  [request]
  (let [^InputStream stream
        (:body request)

        bytes
        (when stream (.readNBytes stream 8193))]

    (cond (nil? bytes)
          (http/error-response 400 :invalid-request "A decision training request is required")
          (> (alength ^bytes bytes) 8192)
          (http/error-response 413 :invalid-request "Decision training request exceeds 8 KiB")
          :else (try (http/json-response 202
                                         (decision-jobs/create!
                                           (into {}
                                                 (decisions/parse-body
                                                   (String. ^bytes bytes StandardCharsets/UTF_8)))))
                     (catch clojure.lang.ExceptionInfo e (decision-mutation-error e))))))

(defn- decision-training-job-handler
  "Read status or cancel a running job; DELETE also discards a completed checkpoint."
  [request]
  (let [id
        (get-in request [:path-params :job-id])

        result
        (if (= :delete (:request-method request))
          (decision-jobs/delete! id)
          (decision-jobs/get! id))]

    (if result
      (http/json-response (if (= "cancelling" (get result "status")) 202 200) result)
      (http/error-response 404 :decisions/unknown-job "Decision training job does not exist"))))

(defn- decisions-handler
  "POST /v1/systemone — explicit installed model, typed questions and action head."
  [request]
  (let [bytes (when-let [^InputStream body (:body request)]
                (.readNBytes body 131073))]
    (if (or (nil? bytes) (> (alength ^bytes bytes) 131072))
      (http/error-response 413
                           :invalid-request "Decision request exceeds 128 KiB"
                           :reason "request-too-large")
      (try (http/json-response (decisions/infer! (decisions/parse-body
                                                   (String. ^bytes bytes StandardCharsets/UTF_8))))
           (catch clojure.lang.ExceptionInfo e
             (let [type (:type (ex-data e))
                   status (case type
                            :decisions/model-required
                            400

                            :decisions/invalid-request
                            400

                            :decisions/unknown-model
                            404

                            :decisions/model-not-installed
                            409

                            :decisions/invalid-bundle
                            422

                            :decisions/capacity-exceeded
                            503

                            :decisions/unavailable
                            503

                            nil)]

               (if status
                 (http/error-response status :decisions/error (.getMessage e) :reason (name type))
                 (throw e))))))))

(def handlers
  "Handlers for this namespace's routes, keyed by the gateway contract's `[method path]`."
  {[:get "/v1/decisions/models"] decisions-models-handler
   [:post "/v1/decisions/models"] decision-import-handler
   [:get "/v1/decisions/models/:model-ref"] decision-model-handler
   [:get "/v1/decisions/aliases/:alias"] decision-alias-handler
   [:put "/v1/decisions/aliases/:alias"] decision-alias-handler
   [:post "/v1/decisions/training/jobs"] decision-training-start-handler
   [:get "/v1/decisions/training/jobs/:job-id"] decision-training-job-handler
   [:delete "/v1/decisions/training/jobs/:job-id"] decision-training-job-handler
   [:post "/v1/systemone"] decisions-handler})
