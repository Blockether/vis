(ns com.blockether.vis.internal.activity.presenter-test
  (:require [clojure.java.io :as io]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [charred.api :as json]
            [lazytest.core :refer [defdescribe expect it]]))

(defn result-fixture
  "Production-generated result views shared by Companion stories and TUI grid tests."
  []
  (let
    [path
     "src/com/blockether/vis/internal/activity/presenter.clj"

     cases
     [[:cat path "12:abc│ (defn greeting [name]\n13:def│   (str \"Hello \" name))" nil]
      [:patch path "patched presenter.clj"
       {:metadata
        {:target {:resolved path}
         :diff
         "@@ -12,2 +12,2 @@\n (defn greeting [name]\n-  (str \"Hi \" name))\n+  (str \"Hello \" name))"
         :lines {"added" 1 "removed" 1 "modified" 1}}}]
      [:grep "greeting"
       "grep 'greeting'  2 hits · 2 files\nsrc/greeting.clj  (1)\n  12:abc│ (defn greeting [name]\ntest/greeting_test.clj  (1)\n  8:def│ (is (= \"Hello Ada\" (greeting \"Ada\")))"
       nil]
      [:doc "activity"
       "# Activity\n\nOpen an operation to read its **retained result**.\n\n- Read shows the captured lines.\n- Patch shows the applied changes."
       nil]
      [:council.publish "Activity review"
       {:title "Activity review"
        :thread_id 42
        :ping ["reviewer"]
        :content "Read and Patch now show their **results** after one disclosure."} nil]
      [:run_tests "Activity tests"
       {:is_pass true :total 12 :pass 12 :fail 0 :output "12 tests passed."} nil]]

     ctx
     (event/context)

     events
     (mapcat (fn [[operation label result envelope]]
               (let [invocation
                     (event/invocation ctx nil)

                     details
                     {:operation operation
                      :label label
                      :args [label]
                      :presenter (if (= operation :patch) :patch :generic)
                      :classification (if (= operation :patch) :mutation :observation)}]

                 [(event/start-event ctx invocation details)
                  (event/terminal-event ctx
                                        invocation
                                        (assoc details
                                          :outcome :succeeded
                                          :result result
                                          :result-envelope envelope
                                          :started-at-ms (System/currentTimeMillis)))]))
             cases)]

    (-> (activity/replay events)
        (assoc :state :succeeded)
        activity/presentation
        (update :rows
                #(mapv (fn [i row]
                         (-> row
                             (assoc :id (str "result-" i)
                                    :duration-ms 0)
                             (dissoc :argument-key)))
                       (range)
                       %))
        wire/->wire)))

(defdescribe result-fixture-test
             (it "matches the shared result-view fixture and satisfies the portable contract"
                 (let [actual
                       (result-fixture)

                       expected
                       (json/read-json (slurp (io/resource
                                                "vis-contract/fixtures/activity-results.json")))]

                   (expect (contract/valid-projection? actual))
                   (expect (= expected actual)))))

(defdescribe
  result-table-headings-test
  (it "names scalar-table columns for the operation, with a readable fallback"
      (doseq [[operation heading] [[:run_tests "Metric"] [:lint_code "Metric"]
                                   [:council.publish "Message"] [:council.get "Message"]
                                   [:council.read "Thread"] [:council.threads "Thread"]
                                   [:council.members "Member"] [:custom.lookup "Detail"]]]
        (let [content (get (presenter/result-presentation {:operation operation}
                                                          {:total 12 :title "Activity review"})
                           "content")]
          (expect (= [heading "Result"] (get-in content [0 "columns"]))))))
  (it "keeps nested detail tables distinct from top-level test metrics"
      (let [content (get (presenter/result-presentation {:operation :run_tests}
                                                        {:total 12
                                                         :environment {:language "clojure"}})
                         "content")]
        (expect (= ["Metric" "Result"] (get-in content [0 "columns"])))
        (expect (= ["Detail" "Result"] (get-in content [2 "columns"])))))
  (it "uses the operation's columns for each item in a result list"
      (let [content (get (presenter/result-presentation {:operation "council.members"}
                                                        [{:name "Reviewer"} {:name "Author"}])
                         "content")]
        (expect (= [["Member" "Result"] ["Member" "Result"]] (mapv #(get % "columns") content))))))
