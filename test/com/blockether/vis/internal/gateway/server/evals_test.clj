(ns com.blockether.vis.internal.gateway.server.evals-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.server.evals :as api]
            [com.blockether.vis.internal.python.extensions :as extensions]
            [lazytest.core :refer [defdescribe describe expect it]])
  (:import (java.io ByteArrayInputStream)
           (java.nio.charset StandardCharsets)))

(defn- call
  "Answer one evaluation request through the route table and parse the JSON body."
  [op body]
  (let [bytes
        (.getBytes (if (string? body) ^String body (wire/json-str body)) StandardCharsets/UTF_8)

        response
        ((get api/handlers [:post (str "/v1/evals/" op)]) {:body (ByteArrayInputStream. bytes)})]

    (assoc response :json (wire/parse-json (:body response)))))

(defn- valid? [definition body] (document/valid-json? "gateway" definition body))

(defn- trial
  [task passed]
  {"task" (str "terminal-bench/" task)
   "trial" (str task "__0102")
   "finished_at" "2026-01-02T00:00:00"
   "scored" true
   "model" "zai/glm-5.3"
   "reward" (if (= passed 5) 1.0 0.0)
   "verifier_tests" {"tests" 5 "passed" passed}
   "agent_seconds" 600
   "iterations" 12})

(def ^:private summary
  {"dataset" "terminal-bench@2.0"
   "total_dataset_tasks" 3
   "scored_tasks" 2
   "solved_tasks" ["terminal-bench/alpha"]
   "task_outcomes" {"solved" ["terminal-bench/alpha"] "failed_tests" ["terminal-bench/beta"]}
   "attempts" 2
   "usage_totals" {"model_attempts" 2 "input_tokens" 100 "output_tokens" 10}
   "trials" [(trial "alpha" 5) (trial "beta" 4)]})

(defn- cases
  [passed]
  (map-indexed (fn [n p]
                 {"task" (str "t" n) "agent" "vis" "model" "m" "passed" p "runs" 1})
               passed))

(defdescribe
  evals-routes-test
  (describe "the bundled SDK operations"
            (it "scores a round"
                (let [{:keys [status json]} (call "report" {"summary" summary})]
                  (expect (= 200 status))
                  (expect (= 1 (get json "solved_tasks")))
                  (expect (valid? "evals_bench_report" json))))
            (it "diagnoses a round and checks the human labels"
                (let [labels
                      {"beta__0102" {"cause" "failed-tests"}}

                      diagnosis
                      (call "diagnose" {"summary" summary "labels" labels})

                      check
                      (call "labels" {"summary" summary "labels" labels})]

                  (expect (= 200 (:status diagnosis)))
                  (expect (valid? "evals_diagnosis" (:json diagnosis)))
                  (expect (= 1 (get-in check [:json "labelled"])))
                  (expect (valid? "evals_agreement" (:json check)))))
            (it "compares two results over their paired cases"
                (let [{:keys [status json]} (call "compare"
                                                  {"baseline" (cases (repeat 20 0))
                                                   "candidate" (cases (repeat 20 1))})]
                  (expect (= 200 status))
                  (expect (= "better" (get json "verdict")))
                  (expect (valid? "evals_case_comparison" json))))
            (it "gives the agreement of automatic results with human labels"
                (let [items
                      (for [n (range 3)]
                        {"name" (str "i" n) "automatic" "pass" "human" "pass"})

                      {:keys [status json]}
                      (call "agreement" {"items" items})]

                  (expect (= 200 status))
                  (expect (= 3 (get json "labelled")))
                  (expect (valid? "evals_agreement" json)))))
  (describe "invalid requests"
            (it "answers 400 for a body that the operation cannot use"
                (let [{:keys [status json]} (call "compare" {"baseline" [{}] "candidate" []})]
                  (expect (= 400 status))
                  (expect (= "invalid-evals-request" (get-in json ["error" "type"])))))
            (it "answers 400 for a body that is not a JSON object"
                (expect (= 400 (:status (call "report" "[1]"))))))
  (describe "the module list"
            (it "names each module that the gateway module imports"
                (doseq [module extensions/evals-modules]
                  (let [source (slurp (io/resource (str "blockether/vis/evals/" module ".py")))
                        imports (map second (re-seq #"(?m)^from \.(\w*) import" source))]

                    (expect (every? (set extensions/evals-modules) imports) module)
                    (expect (not (re-find #"(?m)^from \.\.|^import blockether|^from blockether"
                                          source))
                            module))))))
