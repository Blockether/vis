(ns com.blockether.vis.loop.journal-preservation-test
  "Regression test for journal propagation across empty iterations.

   Bug (conversation 1f44852d-1426-4356-86c8-43d4e4fb5b11):
     iter N   : (def sheet-src (read-file \"…\"))  ;; full 1165 chars bound
     iter N+1 : :code []                           ;; model only thinks
     iter N+2 : builds <journal> from prev-executions = nil

   Result: iter N+2 had neither the journal (because iter N+1 was empty
   and zeroed prev-executions) nor a complete view via <var_index>
   (capped at 150-char preview). Model re-read the same file, burning
   another iteration.

   Fix: the empty-iteration branch must propagate the most recent
   prev-executions/prev-iteration onward instead of nilling them.
   Empty iterations add no new journal content, but they must not
   DELETE the existing journal for the next real turn."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.svar.internal.llm :as llm]
    [com.blockether.vis.core :as sut]
    [com.blockether.vis.loop.core :as rlm-core]
    [com.blockether.vis.loop.runtime.query.routing :as rlm-routing]))

(defn- make-test-env []
  (let [router (llm/make-router
                 [{:id :test :api-key "test" :base-url "http://localhost"
                   :models [{:name "gpt-4o"}]}])]
    (#'rlm-core/create-rlm-env (atom 0) router {:db :temp})))

(defn- scripted-run-iteration
  "Records every messages-vec it was called with; returns the next
   scripted iteration result."
  [scripts messages-log]
  (let [n (atom 0)]
    (fn [& args]
      (let [messages (nth args 1 nil)]
        (swap! messages-log conj messages)
        (let [i (dec (swap! n inc))
              r (get scripts i)]
          (assert r (str "scripted-run-iteration ran out at call #" i))
          r)))))

(defdescribe journal-across-empty-iter-test
  (describe "journal preservation"
    (it "iter N's execution journal is still visible in iter N+2 after an empty iter N+1"
      ;; Script: iter 0 produces a distinctive execution; iter 1 is empty
      ;; (pure reasoning); iter 2 is the one under test — its incoming
      ;; messages MUST still contain the journal entry from iter 0.
      (let [env (make-test-env)
            messages-log (atom [])
            uniq "SHEET-DELETE-MARKER-XYZ"
            iter-0 {:thinking "bound"
                    :executions [{:id 0
                                  :code "(def sheet-src \"placeholder\")"
                                  :result uniq
                                  :stdout "" :stderr "" :error nil
                                  :execution-time-ms 2}]
                    :final-result nil :api-usage nil :duration-ms 0}
            iter-1 {:thinking "thinking" :executions [] :final-result nil
                    :api-usage nil :duration-ms 0}
            iter-2 {:thinking "done"
                    :executions []
                    :final-result {:answer {:result "ok" :type String}
                                   :confidence :high}
                    :api-usage nil :duration-ms 0}]
        (try
          (with-redefs [rlm-routing/provider-has-reasoning? (constantly true)
                        rlm-core/run-iteration (scripted-run-iteration
                                                 [iter-0 iter-1 iter-2]
                                                 messages-log)]
            (sut/query-env! env [(llm/user "please")]
              {:max-iterations 5 :refine? false}))
          ;; We care about the messages the loop handed to iter 2.
          (let [iter-2-msgs (nth @messages-log 2 nil)
                combined (str/join "\n" (keep :content iter-2-msgs))]
            (expect (some? iter-2-msgs))
            ;; Sanity: iter 2's prompt has a <journal> block at all.
            (expect (str/includes? combined "<journal>"))
            ;; The critical assertion: iter 0's execution value is still
            ;; in the journal for iter 2, even though iter 1 was empty.
            (expect (str/includes? combined uniq)))
          (finally
            (#'rlm-core/dispose-rlm-env! env)))))))
