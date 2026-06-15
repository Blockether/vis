(ns com.blockether.vis.internal.observation-projection-test
  (:require
   [com.blockether.vis.internal.observation-projection :as p]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- cat-form
  [scope path lines & {:keys [range mtime size]}]
  {:scope scope
   :tag :observation
   :src (str "cat(\"" path "\")")
   :result (cond-> {:path path
                    :lines lines
                    :mtime (or mtime 10)
                    :size (or size 100)}
             range (assoc :range range))})

(defdescribe observation-projection-test
  (it "cat(path) creates file coverage"
    (let [events (p/observation-events
                   [(cat-form "t1/i1/f1" "a.clj" [[1 "(ns a)"] [2 ""]])]
                   [])]
      (expect (= 1 (count events)))
      (expect (= "cat" (:op (first events))))
      (expect (= "a.clj" (:path (first events))))
      (expect (= 1 (:range-start (first events))))
      (expect (= 2 (:range-end (first events))))))

  (it "covered unchanged cat range becomes a repeat"
    (let [first-event (first (p/observation-events
                               [(cat-form "t1/i1/f1" "a.clj"
                                  [[1 "a"] [2 "b"] [3 "c"]]
                                  :range [1 3])]
                               []))
          repeat-event (first (p/observation-events
                                [(cat-form "t1/i2/f1" "a.clj" [[2 "b"]]
                                   :range [2 2])]
                                [first-event]))]
      (expect (true? (:repeat? repeat-event)))
      (expect (= "t1/i1/f1" (:covered-by-scope repeat-event)))))

  (it "partial new range is not suppressed"
    (let [first-event (first (p/observation-events
                               [(cat-form "t1/i1/f1" "a.clj" [[1 "a"]]
                                  :range [1 1])]
                               []))
          next-event (first (p/observation-events
                              [(cat-form "t1/i2/f1" "a.clj" [[2 "b"]]
                                 :range [2 2])]
                              [first-event]))]
      (expect (not (:repeat? next-event)))
      (expect (nil? (:covered-by-scope next-event)))))

  (it "repeated normalized rg query is detected"
    (let [form {:scope "t1/i1/f1"
                :tag :observation
                :src "rg({\"any\": [\"needle\"]})"
                :result {:hits [{:path "a.clj" :line 1 :text "needle"}]}}
          first-event (first (p/observation-events [form] []))
          repeat-event (first (p/observation-events
                                [(assoc form :scope "t1/i2/f1"
                                   :src " rg({\"any\":   [\"needle\"]}) ")]
                                [first-event]))]
      (expect (true? (:repeat? repeat-event)))
      (expect (= "t1/i1/f1" (:repeat-of-scope repeat-event)))))

  (it "patch/write-style mutation events expose affected paths"
    (let [events (p/observation-events
                   [{:scope "t1/i2/f1"
                     :tag :mutation
                     :src "patch([{\"path\": \"a.clj\", \"search\": \"x\", \"replace\": \"y\"}])"
                     :result [{:path "a.clj" :changed? true}]}]
                   [])]
      (expect (= ["a.clj"] (p/affected-paths events)))))

  (it "DAG resolved_evidence creates compact evidence refs"
    (let [events (p/evidence-events
                   [{:scope "t1/i3/f1"
                     :result {:resolved_evidence
                              [{:id "evidence/impl/0"
                                :task "impl"
                                :kind "check"
                                :status "observed"
                                :value {:stdout "ok"}}]}}]
                   [])]
      (expect (= [{:task-key "impl"
                   :evidence-id "evidence/impl/0"
                   :evidence-kind "check"
                   :status "observed"
                   :payload-scope "t1/i3/f1"
                   :summary "{:stdout \"ok\"}"
                   :observation-ids []}]
                events)))))
