(ns com.blockether.vis.tui.plan-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.annotations :as annotations]
            [com.blockether.vis.contract.plan :as plan]
            [lazytest.core :refer [defdescribe expect it]]))

(def fixture
  (json/read-json (slurp (io/resource "vis-contract/fixtures/plans.json")) :key-fn keyword))

(defdescribe
  planning-contract
  (it "reads the same document headers and actions as Companion"
      (doseq [{:keys [filename text expected actions]} (:documents fixture)]
        (let [info (plan/document-info filename text)]
          (expect (= (some-> expected
                             (update :kind keyword))
                     info))
          (expect (= actions (mapv name (plan/available-actions info false))))
          (when info (expect (= [:revise] (plan/available-actions info true)))))))
  (it "keeps the exact selected version in every action"
      (doseq [action [:revise :approve]]
        (expect
          (.startsWith
            ^String (plan/action-request "PLAN-search.md" 3 action)
            "Read `PLAN-search.md` v3 with read_attachment(\"PLAN-search.md\", version=3)."))))
  (it "round-trips annotations without consuming resolved history"
      (doseq [{:keys [body comments rendered]} (:annotations fixture)]
        (expect (= rendered (annotations/render-annotated body comments)))
        (let [parsed (annotations/parse-annotated rendered)]
          (expect (= rendered (annotations/render-annotated (:body parsed) (:comments parsed)))))))
  (it "leaves ordinary headings untouched"
      (doseq [text (:unrecognized fixture)]
        (expect (= {:body text :comments []} (annotations/parse-annotated text)))))
  (it "normalizes passage quotes identically"
      (doseq [{:keys [input expected]} (:quotes fixture)]
        (expect (= expected (annotations/quote-of input))))))
