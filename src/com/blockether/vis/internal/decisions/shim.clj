(ns com.blockether.vis.internal.decisions.shim
  "A small first-party decision client for the Vis Python sandbox."
  (:require [charred.api :as json]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.extension :as ext]
            [com.blockether.vis.internal.activity.event :as activity-event]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.util :as util]))

(def ^:private operations
  {"infer" {:method :post :path "/v1/systemone" :headline "Infer decision" :show-start true}
   "models"
   {:method :get :path "/v1/decisions/models" :headline "List decision models" :show-start false}
   "model"
   {:method :get :path "/v1/decisions/models/" :headline "Read decision model" :show-start false}})

(defn- request-options
  [operation args]
  (let [timeout (get args "timeout_ms" (if (= operation "infer") 180000 30000))]
    (when-not (and (integer? timeout) (<= 1000 timeout 600000))
      (throw (ex-info "Decision timeout must be between 1 and 600 seconds"
                      {:type :decisions/invalid-timeout})))
    (cond-> {:timeout-ms timeout}
      (= operation "infer")
      (assoc :body (get args "body")))))

(defn- route
  [operation args]
  (let [{:keys [path]} (get operations operation)]
    (when-not path
      (throw (ex-info "Unknown decision operation" {:type :decisions/unknown-operation})))
    (if (= operation "model")
      (let [ref (get args "model_ref")]
        (when-not (and (string? ref) (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" ref))
          (throw (ex-info "Invalid decision model reference" {:type :decisions/invalid-model-ref})))
        (str path ref))
      path)))

(defn- result-presentation
  [operation status body]
  (let [headline
        (get-in operations [operation :headline])

        code
        (when (map? body) (or (get-in body ["error" "reason"]) (get-in body ["error" "type"])))

        models
        (when (= operation "models") (get body "models"))

        count-models
        (when (sequential? models) (count models))]

    {"headline" (if (<= 200 status 299) headline "Decision request failed")
     "summary" (str "HTTP "
                    status
                    (when (some? count-models)
                      (str " · " count-models (if (= count-models 1) " model" " models")))
                    (when (util/non-blank-string? code)
                      (str " · " (activity-event/bounded-text code 80))))
     "content" []}))

(defn- query!
  [_ args]
  (let [operation
        (get args "operation")

        {:keys [method]}
        (get operations operation)

        path
        (route operation args)

        response
        (gateway-client/request! method path (request-options operation args))

        status
        (:status response)

        body
        (wire/parse-json (:body response))]

    (when-not (and (integer? status) (map? body))
      (throw (ex-info "Invalid decision gateway response" {:type :decisions/invalid-response})))
    (extension/publish-activity! (result-presentation operation status body))
    (extension/success {:result {"status" status "body" body}})))

(defn- symbol-for
  [operation]
  (let [{:keys [headline show-start]} (get operations operation)]
    {:ext.symbol/symbol (symbol (str "decisions." operation))
     :ext.symbol/tag :observation
     :ext.symbol/presenter :observation
     :ext.symbol/activity {:headline headline :show-start show-start}
     :ext.symbol/inject-env? true
     :ext.symbol/fn query!}))

(defn- bridge-bindings
  []
  {"__vis_decision_query__"
   (fn decision-query [args-json]
     (let [args
           (json/read-json (str args-json) :key-fn identity)

           operation
           (get args "operation")

           symbol
           (symbol-for (if (contains? operations operation) operation "models"))]

       (json/write-json-str (extension/invoke-symbol-wrapper {:ext/name "decisions-shim"}
                                                             symbol
                                                             [args]
                                                             extension/*current-environment*))))})

(def vis-extension
  (ext/extension
    {:ext/name "decisions-shim"
     :ext/description "Small built-in Vis Python decision reads using the authenticated gateway."
     :ext/version "0.1.0"
     :ext/author "Blockether"
     :ext/owner "vis"
     :ext/license "Apache-2.0"
     :ext/kind "foundation"
     :ext/sandbox-shims
     [{:shim/name "decisions"
       :shim/imports ["vis_decisions"]
       :shim/docs
       (str
         "`import vis_decisions` provides first-party `infer(model=..., state=..., questions=...)`, "
         "`models()` and `model(model_ref)` through Vis' authenticated gateway. "
         "No SDK wheel, PyTorch, model download or training dependencies are loaded. "
         "A missing model raises DecisionGatewayError(409); use the explicit CLI download. "
         "For upload, training or remote SDK clients, explicitly install `vis-agent` "
         "and the optional `decisions-training` extra.")
       :shim/bindings bridge-bindings
       :shim/source "vis-shims/decisions.py"}]}))

(defn register! [] (ext/register-extension! vis-extension))
