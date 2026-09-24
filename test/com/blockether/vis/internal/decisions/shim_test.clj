(ns com.blockether.vis.internal.decisions.shim-test
  "Built-in decision reads in a Python sandbox without installing the full SDK."
  (:require [charred.api :as json]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.gateway.client :as gateway-client]
            [com.blockether.vis.internal.python.env :as python-env]
            [com.blockether.vis.test-python-context :as python-context]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(deftest decision-client-is-available-without-sdk-install
  (let [ctx
        (:python-context (python-context/new-context {}))

        result
        (python-env/run-python-block ctx
                                     "import vis_decisions\nprint(hasattr(vis_decisions, 'infer'))"
                                     "t1/i1")]

    (is (nil? (:error result)))
    (is (= "True\n" (:stdout result)))))

(deftest light-client-uses-the-authenticated-gateway-without-downloads
  (let
    [ctx
     (:python-context (python-context/new-context {}))

     calls
     (atom [])

     events
     (atom [])

     result
     (with-redefs [gateway-client/request!
                   (fn [method path options]
                     (swap! calls conj [method path options])
                     {:status (if (= "missing" (get-in options [:body "model"])) 409 200)
                      :body (json/write-json-str
                              (cond (= path "/v1/decisions/models") {"models" []}
                                    (= path "/v1/decisions/models/laya-typed-decisions")
                                    {"model_ref" "laya-typed-decisions" "installed" false}
                                    (= "missing" (get-in options [:body "model"]))
                                    {"error" {"type" "error" "reason" "model-not-installed"}}
                                    :else {"answers" {"intent" {"answer" "refund"
                                                                "action" {"act_probability" 0.1}}
                                                      "urgency" {"answer" "high"}
                                                      "refundable" {"answer" true}}
                                           "routing" {"model" "laya-typed-decisions"}}))})]
       (binding [extension/*tool-event-sink* #(swap! events conj %)]
         (python-env/run-python-block
           ctx
           (str
             "import sys, vis_decisions as d\n"
             "print(d.models() == [], d.model('laya-typed-decisions')['installed'] is False)\n"
             "questions = {'intent': {'type': 'choice', 'criteria': ['refund', 'repair']}, "
             "'urgency': {'type': 'score', 'criteria': ['low', 'high']}, "
             "'refundable': {'type': 'noul'}}\n"
             "answer = d.infer(model='laya-typed-decisions', state='test', questions=questions)\n"
             "print(answer['answers']['intent']['action']['act_probability'], "
             "answer['routing']['model'])\n"
             "try:\n" "    d.infer(model='missing', state='test', questions=questions)\n"
             "except d.DecisionGatewayError as error:\n" "    print(error.status, error.code)\n"
             "print(all(name not in sys.modules for name in "
             "('torch', 'onnxruntime', 'jsonschema', 'blockether.vis.decisions')))\n")
           "t1/i1")))]

    (is (nil? (:error result)))
    (is (= "True True\n0.1 laya-typed-decisions\n409 model-not-installed\nTrue\n" (:stdout result)))
    (is (= [:get :get :post :post] (map first @calls)))
    (is (= ["/v1/decisions/models" "/v1/decisions/models/laya-typed-decisions" "/v1/systemone"
            "/v1/systemone"]
           (map second @calls)))
    (is (= 180000 (get-in @calls [2 2 :timeout-ms])))
    (is (= ["intent" "urgency" "refundable"] (vec (keys (get-in @calls [2 2 :body "questions"])))))
    (is (not-any? :headers (map #(nth % 2) @calls)))
    (let [rows (:rows (activity/presentation (activity/replay @events)))]
      (is (= 4 (count rows)))
      (is (some #(= "List decision models" (get-in % [:presentation "headline"])) rows))
      (is (some #(= "HTTP 200 · 0 models" (get-in % [:presentation "summary"])) rows))
      (is (some #(and (= "Decision request failed" (get-in % [:presentation "headline"]))
                      (= "HTTP 409 · model-not-installed" (get-in % [:presentation "summary"])))
                rows)))))
