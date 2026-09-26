(ns com.blockether.vis.internal.activity.digest-test
  (:require [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.activity.digest :as digest]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- row
  [id sequence operation signal state & {:as extra}]
  (merge {"id" id
          "sequence" sequence
          "operation" operation
          "presenter" "generic"
          "signal" signal
          "state" state
          "summary" (str operation " " id)
          "resources" []
          "evidence" []}
         extra))

(defn- file [path] {"type" "file" "id" path})

(defn- checked
  [headline summary verdict]
  {"headline" headline "summary" summary "content" [] "verdict" verdict})

(defn- projection
  ([rows] (projection rows {"rows" 0 "by_classification" {}}))
  ([rows omitted]
   (let [states (frequencies (map #(get % "state") rows))]
     (or (contract/from-wire {"state" "succeeded"
                              "counts" (into {}
                                             (map (fn [state]
                                                    [state (get states state 0)]))
                                             ["running" "succeeded" "failed" "cancelled"])
                              "rows" rows
                              "omitted" omitted})
         (throw (ex-info "Invalid test projection" {:rows rows}))))))

(defn- valid? [value] (some? (contract/digest-from-wire (wire/->wire value))))

(defdescribe
  digest-test
  (it "answers nothing for a turn that ran no operation"
      (expect (nil? (digest/digest [])))
      (expect (nil? (digest/digest [(projection [])]))))
  (it "summarizes a read-only turn without attention"
      (let [result (digest/digest [(projection [(row "1" 1 "cat" "observation" "succeeded")
                                                (row "2" 2 "cat" "observation" "succeeded")
                                                (row "3" 3 "grep" "observation" "failed")])])]
        (expect (valid? result))
        (expect (= "0 mutations · 3 observations" (:summary result)))
        (expect (= 3 (:operations result)))
        (expect (= [] (:attention result)))
        (expect (= 0 (:retries result)))
        (expect (not (contains? result :changes)))
        (expect (= [{:operation "cat"
                     :label "Read"
                     :signal "observation"
                     :count 2
                     :failed 0
                     :state "succeeded"
                     :summary "cat 2"}
                    {:operation "grep"
                     :label "Search"
                     :signal "observation"
                     :count 1
                     :failed 1
                     :state "failed"
                     :summary "grep 3"}]
                   (:groups result)))))
  (it
    "settles a retried patch and a fixed check as retries"
    (let [result (digest/digest
                   [(projection
                      [(row "1" 1 "patch" "mutation" "failed" "error_summary" "Stale anchor 12:abc")
                       (row "2"
                            2 "run_tests"
                            "verification" "succeeded"
                            "presentation"
                            (checked "Run tests" "3 passed, 1 failed in 2.0 s" "failed"))])
                    (projection
                      [(row "1"
                            1 "patch"
                            "mutation" "succeeded"
                            "resources" [(file "src/a.clj")]
                            "evidence" [{"kind" "diff"
                                         "text" "src/a.clj"
                                         "lines" []
                                         "additions" 3
                                         "deletions" 1
                                         "modifications" 0
                                         "is_truncated" false
                                         "is_redacted" false}])
                       (row "2"
                            2 "run_tests"
                            "verification" "succeeded"
                            "presentation"
                            (checked "Run tests" "4 passed, 0 failed in 2.1 s" "passed"))])])]
      (expect (valid? result))
      (expect (= "2 mutations · 1 file +3 \u22121 · 2 checks, passing · 2 retries"
                 (:summary result)))
      (expect (= 2 (:retries result)))
      (expect (= [] (:attention result)))
      (expect (= {:files 1 :additions 3 :deletions 1} (:changes result)))
      (expect (= [{:operation "patch"
                   :label "Patch"
                   :signal "mutation"
                   :count 2
                   :failed 1
                   :state "succeeded"
                   :summary "patch 1"}
                  {:operation "run_tests"
                   :label "Run tests"
                   :signal "verification"
                   :count 2
                   :failed 1
                   :state "succeeded"
                   :verdict "passed"
                   :summary "4 passed, 0 failed in 2.1 s"}]
                 (:groups result)))))
  (it "keeps unresolved changes, checks and external effects in view"
      (let [result (digest/digest
                     [(projection
                        [(row "1" 1 "cat" "observation" "failed")
                         (row "2"
                              2 "patch"
                              "mutation" "failed"
                              "resources" [(file "src/b.clj")]
                              "error_summary" "Stale anchor\nline two")
                         (row "3" 3 "patch" "mutation" "succeeded" "resources" [(file "src/c.clj")])
                         (row "4"
                              4 "lint_code"
                              "verification" "succeeded"
                              "presentation" (checked "Lint" "2 findings" "failed"))
                         (row "5" 5 "council.publish" "external" "failed")])])]
        (expect (valid? result))
        (expect (= "2 mutations · 1 observation · 1 check, 1 failing · 1 external action"
                   (:summary result)))
        (expect (= ["2" "4" "5"] (mapv :id (:attention result))))
        (expect (= 3 (:attention-total result)))
        (expect (= 0 (:retries result)))
        (expect (= "Stale anchor"
                   (:summary (first (:groups (digest/digest
                                               [(projection [(row
                                                               "2"
                                                               2 "patch"
                                                               "mutation" "failed"
                                                               "error_summary"
                                                               "Stale anchor\nline two")])]))))))))
  (it "settles a mutation only through a later call covering its resources"
      (let [failed
            (row "1" 1 "patch" "mutation" "failed" "resources" [(file "src/a.clj")])

            other
            (row "2" 2 "patch" "mutation" "succeeded" "resources" [(file "src/b.clj")])

            same
            (row "3" 3 "patch" "mutation" "succeeded" "resources" [(file "src/a.clj")])]

        (expect (= ["1"] (mapv :id (:attention (digest/digest [(projection [failed other])])))))
        (let [result (digest/digest [(projection [failed other same])])]
          (expect (= [] (:attention result)))
          (expect (= 1 (:retries result))))))
  (it "expands observation groups and keeps one live handle as one operation"
      (let [result (digest/digest
                     [(projection [(row "g"
                                        1 "cat"
                                        "observation" "succeeded"
                                        "group_token" "src"
                                        "children" [(row "g1" 1 "cat" "observation" "succeeded")
                                                    (row "g2" 2 "cat" "observation" "succeeded")])
                                   (row "h"
                                        3 "shell"
                                        "mutation" "running"
                                        "handle_id" "server"
                                        "children"
                                        [(row "h1" 3 "shell" "mutation" "succeeded")
                                         (row "h2" 4 "_shell_logs" "observation" "succeeded")])])])]
        (expect (valid? result))
        (expect (= 3 (:operations result)))
        (expect (= [["cat" 2] ["shell" 1]] (mapv (juxt :operation :count) (:groups result))))
        (expect (= ["h"] (mapv :id (:attention result))))))
  (it "counts omitted rows by classification"
      (let [result (digest/digest
                     [(projection [(row "1" 1 "patch" "mutation" "succeeded")]
                                  {"rows" 5 "by_classification" {"observation" 4 "mutation" 1}})])]
        (expect (valid? result))
        (expect (= "2 mutations · 4 observations" (:summary result)))
        (expect (= 6 (:operations result)))
        (expect (= 5 (:omitted result)))))
  (it "bounds attention and groups to the contract limits"
      (let [failures
            (digest/digest [(projection (mapv #(row (str %)
                                                    %
                                                    "patch"
                                                    "mutation" "failed"
                                                    "resources" [(file (str "src/f" % ".clj"))])
                                              (range 1 11)))])

            operations
            (digest/digest [(projection (mapv
                                          #(row (str %) % (str "op" %) "observation" "succeeded")
                                          (range 40)))])]

        (expect (valid? failures))
        (expect (= 10 (:attention-total failures)))
        (expect (= (mapv str (range 3 11)) (mapv :id (:attention failures))))
        (expect (valid? operations))
        (expect (= contract/digest-group-limit (count (:groups operations))))
        (expect (= 40 (:operations operations))))))
