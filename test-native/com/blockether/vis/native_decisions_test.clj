(ns com.blockether.vis.native-decisions-test
  "Pinned FP32 decision heads through an isolated linked native gateway."
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.decisions.assets :as assets]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.native-binary-test :as binary]
            [lazytest.core :refer [defdescribe expect it]])
  (:import [java.io File]
           [java.net ServerSocket]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]))

(defn- wait-for-gateway!
  [^Process process]
  (let [deadline (+ (System/nanoTime) 60000000000)]
    (loop []

      (if (try (= 200 (:status (gateway-client/request! :get "/healthz" {:timeout-ms 1000})))
               (catch Exception _ false))
        true
        (if (and (.isAlive process) (< (System/nanoTime) deadline))
          (do (Thread/sleep 100) (recur))
          (throw (ex-info "Isolated native decision gateway did not start" {})))))))

(defn- sdk-flow!
  [^File home ^File source port python ^Process gateway]
  (let [^File script
        (io/file "test-native/com/blockether/vis/decision_sdk_smoke.py")

        ^File output
        (io/file home "sdk.log")

        builder
        (doto (ProcessBuilder. ^java.util.List
                               (vec [python "-I" (.getAbsolutePath script)
                                     (str "http://127.0.0.1:" port) (.getCanonicalPath source)
                                     (or (System/getProperty "vis.test.laya.training.dir") "-")]))
          (.directory home)
          (.redirectErrorStream true)
          (.redirectOutput output))

        env
        (.environment builder)]

    (expect (.isFile script) (str "Missing native SDK smoke test: " script))
    (.put env "HF_HUB_OFFLINE" "1")
    (.put env "TRANSFORMERS_OFFLINE" "1")
    (.put env "HOME" (.getAbsolutePath home))
    (.remove env "VIS_GATEWAY_URL")
    (.remove env "VIS_GATEWAY_TOKEN")
    (let [process (.start builder)]
      (try (let [finished? (.waitFor process 1200 java.util.concurrent.TimeUnit/SECONDS)]
             (when-not finished? (#'binary/kill-tree! process))
             (let [text (slurp output)
                   succeeded? (and finished? (zero? (.exitValue process)))]

               (expect finished? "Installed SDK did not finish against the native gateway")
               (expect succeeded?
                       (if succeeded?
                         ""
                         (str text
                              "\nNative gateway alive: " (.isAlive gateway)
                              "\nNative gateway log tail:\n" (->> (slurp (io/file home
                                                                                  "gateway.log"))
                                                                  str/split-lines
                                                                  (take-last 25)
                                                                  (str/join "\n")))))
               (when succeeded?
                 (let [line (some #(when (str/starts-with? % "VIS_DECISION_RESULT=")
                                     (subs % (count "VIS_DECISION_RESULT=")))
                                  (str/split-lines text))]
                   (expect (some? line) text)
                   (when line
                     (let [result (json/read-json line)]
                       (expect (= (get result "ref") (get result "routing")))
                       (expect (= "laya-typed-decisions" (get result "baseline")))
                       (expect (= (boolean (System/getProperty "vis.test.laya.training.dir"))
                                  (get result "trained")))
                       (expect (#{"refund" "repair"} (get result "choice")))
                       (expect (number? (get result "score")))
                       (expect (number? (get result "noul")))
                       (expect (number? (get-in result ["action" "act_probability"])))))))))
           (finally (when (.isAlive process) (#'binary/kill-tree! process)))))))

(defn- sha256-file
  [^File archive]
  (with-open [stream (io/input-stream archive)]
    (let [^MessageDigest digest (util/sha256-digest)
          buffer (byte-array 1048576)]

      (loop []

        (let [n (.read stream buffer)]
          (when (pos? n) (.update digest buffer 0 n) (recur))))
      (util/bytes->hex (.digest digest)))))

(defn- gliner-native-check!
  [name ^File archive]
  (expect (.isFile archive) (str "Missing GLiNER FP32 archive: " archive))
  (let [sha
        (sha256-file archive)

        ref
        (str "sha256-" sha)

        model-id
        (str "gliner2.5-" name)

        alias
        (str "native-" name)

        missing
        (gateway-client/request! :post
                                 "/v1/systemone"
                                 {:body {:model alias
                                         :state "missing"
                                         :questions {:ready {:type "noul"
                                                             :instructions "Ready?"}}}})]

    (expect (= 404 (:status missing)) "Import must not select or download a model")
    (with-open [stream (io/input-stream archive)]
      (let [uploaded
            (gateway-client/request!
              :post
              "/v1/decisions/models"
              {:body stream :raw-body? true :headers {"x-content-sha256" sha} :timeout-ms 900000})]
        (expect (= 201 (:status uploaded)) (str (:status uploaded)))
        (expect (= ref (get (json/read-json (:body uploaded)) "model_ref")))))
    (let [not-activated
          (gateway-client/request! :get (str "/v1/decisions/aliases/" alias))

          activated
          (gateway-client/request! :put
                                   (str "/v1/decisions/aliases/" alias)
                                   {:body {:model_ref ref}})

          inference
          (gateway-client/request!
            :post
            "/v1/systemone"
            {:body {:model alias
                    :state "A damaged item needs a refund."
                    :questions {:intent {:type "choice"
                                         :instructions "Select intent"
                                         :criteria {:refund "A refund" :repair "A repair"}}
                                :priority {:type "score"
                                           :instructions "Rate urgency"
                                           :criteria ["not urgent" "soon" "immediate"]}
                                :policy {:type "noul" :instructions "Is refund available?"}}}
             :timeout-ms 180000})

          result
          (json/read-json (:body inference))]

      (expect (= 404 (:status not-activated)) "Import must not activate an alias")
      (expect (= 200 (:status activated)) (str (:status activated)))
      (expect (= ref (get (json/read-json (:body activated)) "model_ref")))
      (expect (= 200 (:status inference)) (str result))
      (expect (= model-id (get result "model")))
      (expect (= ref (get-in result ["routing" "model_ref"])))
      (expect (#{"refund" "repair"} (get-in result ["answers" "intent" "choice"])))
      (expect (number? (get-in result ["answers" "priority" "score"])))
      (expect (number? (get-in result ["answers" "policy" "noul"])))
      (expect (number? (get-in result ["answers" "intent" "action" "act_probability"]))))))

(defdescribe
  native-decision-inference-test
  ;; Regression: the first native /v1/systemone request returned 500 because ORT's
  ;; platform libraries were not embedded; JVM inference alone could not catch it.
  (it
    "answers all three typed questions through bundled native JNI and no network"
    (let [^File home
          (#'binary/temp-dir "vis-native-decisions-")

          ^File executable
          (#'binary/require-binary)

          source
          (some-> (System/getProperty "vis.test.laya.fp32.dir")
                  io/file)

          model
          (assets/entry "laya-typed-decisions")

          store
          (io/file home "models")

          dest
          (io/file store (:id model) (:revision model) "inference")

          port
          (with-open [socket (ServerSocket. 0)]
            (.getLocalPort socket))

          process
          (atom nil)]

      (try
        (when source
          (expect (.isFile (io/file source "model.onnx")))
          (expect (.isFile (io/file source ".vis-verified")))
          (.mkdirs (.getParentFile dest))
          (Files/createSymbolicLink (.toPath dest)
                                    (.toPath (.getCanonicalFile source))
                                    (make-array FileAttribute 0)))
        (let [builder
              (doto (ProcessBuilder. ^java.util.List
                                     [(.getAbsolutePath executable)
                                      (str "-Duser.home=" (.getAbsolutePath home)) "gateway" "start"
                                      "--host" "127.0.0.1" "--port" (str port) "--db"
                                      (.getAbsolutePath (io/file home "sessions"))])
                (.directory home)
                (.redirectErrorStream true)
                (.redirectOutput (io/file home "gateway.log")))

              env
              (.environment builder)]

          (.putAll env (#'binary/native-environment))
          (doseq [name ["VIS_GATEWAY_URL" "VIS_GATEWAY_TOKEN" "VIS_GATEWAY_MANAGED"]]
            (.remove env name))
          (.put env "HOME" (.getAbsolutePath home))
          (.put env "VIS_DECISION_MODELS_DIR" (.getAbsolutePath store))
          (.put env "VIS_DECISION_WARMUP" "off")
          (reset! process (.start builder)))
        (with-redefs-fn {#'gateway-client/ensure-gateway!
                         (constantly {:host "127.0.0.1" :port port :remote? true})
                         #'gateway-client/client-id (atom nil)
                         #'gateway-client/release-hook-installed? (atom true)}
          (fn []
            (wait-for-gateway! @process)
            (let [catalog
                  (gateway-client/request! :get "/v1/decisions/models")

                  rows
                  (get (json/read-json (:body catalog)) "models")]

              (expect (= 200 (:status catalog)))
              (expect (= (boolean source) (get (first rows) "installed")))
              (expect (= "cold" (get (first rows) "residency"))))
            (when-let [python (System/getProperty "vis.test.laya.sdk.python")]
              (expect source "An FP32 install is required for the installed SDK smoke test")
              (when source (sdk-flow! home source port python @process)))
            (let [response
                  (gateway-client/request!
                    :post
                    "/v1/systemone"
                    {:body {:model "laya-typed-decisions"
                            :state "The customer requests a refund after receiving a broken item."
                            :questions {:intent {:type "choice"
                                                 :instructions "What is the customer asking for?"
                                                 :criteria {:refund "A refund" :repair "A repair"}}
                                        :priority {:type "score"
                                                   :instructions "Rate urgency"
                                                   :criteria ["not urgent" "soon" "immediate"]}
                                        :policy {:type "noul"
                                                 :instructions "Can the purchase be refunded?"
                                                 :criteria {:false "not refundable"
                                                            :true "refundable"}}}}
                     :timeout-ms 180000})]
              (if source
                (let [body (json/read-json (:body response))]
                  (expect (= 200 (:status response)) (str body))
                  (expect (= "refund" (get-in body ["answers" "intent" "choice"])))
                  (expect (number? (get-in body ["answers" "priority" "score"])))
                  (expect (number? (get-in body ["answers" "policy" "noul"])))
                  (expect (number? (get-in body ["answers" "intent" "action" "act_probability"])))
                  (expect (= "laya-typed-decisions" (get-in body ["routing" "model"]))))
                (expect (= 409 (:status response))
                        "Missing models must not download or fall back")))
            (doseq [name
                    ["base" "decide"]

                    :let [archive
                          (System/getProperty (str "vis.test.gliner." name ".fp32.archive"))]
                    :when archive]

              (gliner-native-check! name (io/file archive)))))
        (finally (when-let [owned @process]
                   (#'binary/kill-tree! owned))
                 (#'binary/delete-tree! home))))))
