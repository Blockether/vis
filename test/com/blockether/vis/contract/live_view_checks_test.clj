;; The engine and the Python SDK checked a live view in two ways, so one view opened in
;; one host and failed in the other (#453). Both hosts run every case of
;; `vis-contract/fixtures/live-view-checks.json` and must give its answer.
(ns com.blockether.vis.contract.live-view-checks-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.view :as contract]
            [lazytest.core :refer [defdescribe expect it]]))

(def ^:private fixture
  (delay (json/read-json (slurp (io/resource "vis-contract/fixtures/live-view-checks.json")))))

(defn- with-nan
  "The view of case `c`, with NaN at its `nan_at` path, because JSON has no NaN."
  [c]
  (if-let [at (get c "nan_at")]
    (assoc-in (get c "view") (mapv #(if (string? %) % (long %)) at) Double/NaN)
    (get c "view")))

(defn- answer
  "The check answer for case `c`, in the shape of its `expect`."
  [c]
  (let [{lines :view nodes :nodes dropped :dropped} (contract/live-view-faults (with-nan c) true)]
    (cond-> (cond (seq lines) {"class" "view" "cards" [] "text" lines}
                  (seq nodes) {"class" "node" "cards" (vec (keys nodes)) "text" (vec (vals nodes))}
                  :else {"class" "ok" "cards" [] "text" []})
      (seq dropped)
      (assoc "dropped" (mapv :note dropped)))))

(defdescribe
  live-view-checks-test
  (it "gives the answer of each shared case"
      (expect (< 30 (count (get @fixture "cases"))))
      (doseq [c (get @fixture "cases")]
        (expect (= (get c "expect") (answer c)) (get c "name"))))
  (it "gives the lines of each shared patch case"
      (expect (< 10 (count (get @fixture "patches"))))
      (doseq [c (get @fixture "patches")]
        (expect (= (get c "expect") (contract/live-patch-lines (get c "patch"))) (get c "name"))))
  (it "never shows a value of a node or an op in a fault"
      (doseq [[definition discriminator]
              [["component" "type"] ["live_op" "op"]]

              variant
              (get-in (document/schema-document "view") ["$defs" definition "oneOf"])

              :let [tag
                    (get-in variant ["properties" discriminator "const"])]
              property
              (keys (get variant "properties"))

              :when (not (contains? #{"id" discriminator} property))
              marker
              ["MARKERVALUE" -12345 987654321.5 ["MARKERVALUE"] {"k" "MARKERVALUE"}]]

        (let [text (if (= "component" definition)
                     (pr-str (contract/live-view-faults
                               {"title" "T" "nodes" [(assoc {"id" "n" "type" tag} property marker)]}
                               true))
                     (pr-str (contract/live-patch-lines {"view_id" "v"
                                                         "seq" 1
                                                         "ops" [(assoc {"op" tag "node_id" "n"}
                                                                  property marker)]})))]
          (expect (not (re-find #"MARKERVALUE|12345|987654321" text)) (str tag " " property))))
      (expect (not (re-find #"MARKERVALUE"
                            (pr-str (contract/live-view-faults
                                      {"title" "T" "nodes" [{"id" "n" "type" "MARKERVALUE"}]}
                                      true))))))
  (it "names a node without a type as null, as every host does"
      (expect (= ["/op - the set op does not apply to a node of type null (op_target)"]
                 (contract/live-op-target-lines nil "x" {"op" "set" "text" "y"}))))
  (it "never shows a value that the view holds"
      (doseq [c
              (get @fixture "cases")

              line
              (get-in c ["expect" "text"])]

        (expect
          (not
            (re-find
              #"https?://|-12345|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
              line))
          line))))
