(ns com.blockether.vis.loop.trace-empty-iter-test
  "Regression test: the iteration-loop's returned `:trace` vector must
   contain one entry per iteration the model ran, including empty
   (thinking-only) iterations.

   Bug (conversation 2e3bf18c-51f1-4c43-a945-d8f1a96a3abd):
     Model ran 10 iterations: #0 normal, #1..#6 empty (thinking only),
     #7 normal, #8..#9 empty (thinking only).

     All 10 iterations persisted to SQLite (the `iteration_attrs` row
     was written unconditionally up-front). But the in-memory `:trace`
     returned from `conversations/send!` contained only entries #0 and
     #7 — the empty-iteration branch in `iteration-loop` bumped
     `:iteration` but never `conj`ed a trace-entry.

     Downstream: `web-conversations/append-message!` cached that
     truncated trace for the live view. Because `messages-cache` is
     lazy and never invalidated, reloading the page kept serving the
     cached 2-entry trace instead of the DB-backed 10-entry trace.
     User saw `ITERATION 1` and `ITERATION 8` with nothing between
     them — no indication the model had spent 6 iterations thinking
     in circles.

   Fix: the empty-iteration branch must `conj` the same trace-entry
   every other branch does. Thinking-only iterations are real agent
   behaviour worth showing; hiding them from the trace makes the UI
   lie about what the model did."
  (:require
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
  "Returns the next scripted iteration-result on each invocation."
  [scripts]
  (let [n (atom 0)]
    (fn [& _]
      (let [i (dec (swap! n inc))
            r (get scripts i)]
        (assert r (str "scripted-run-iteration ran out at call #" i))
        r))))

(defdescribe trace-contains-empty-iterations-test
  (describe "iteration trace preservation"
    (it "returned :trace contains one entry per iteration, including empty ones"
      ;; Script: normal → empty → empty → final. Four iterations run;
      ;; trace must have four entries.
      (let [env (make-test-env)
            iter-0 {:thinking "plan"
                    :executions [{:id 0
                                  :code "(def x 1)"
                                  :result 1
                                  :stdout "" :stderr "" :error nil
                                  :execution-time-ms 1}]
                    :final-result nil :api-usage nil :duration-ms 0}
            iter-1 {:thinking "thinking 1"
                    :executions []
                    :final-result nil :api-usage nil :duration-ms 0}
            iter-2 {:thinking "thinking 2"
                    :executions []
                    :final-result nil :api-usage nil :duration-ms 0}
            iter-3 {:thinking "done"
                    :executions []
                    :final-result {:answer {:result "ok" :type String}
                                   :confidence :high}
                    :api-usage nil :duration-ms 0}]
        (try
          (let [result (with-redefs [rlm-routing/provider-has-reasoning? (constantly true)
                                     rlm-core/run-iteration (scripted-run-iteration
                                                              [iter-0 iter-1 iter-2 iter-3])]
                         (sut/query-env! env [(llm/user "please")]
                           {:max-iterations 10 :refine? false}))
                trace (:trace result)]
            (expect (some? trace))
            ;; The critical assertion: every iteration gets a trace
            ;; entry. Before the fix, this returned 2 (normal + final)
            ;; instead of 4 (normal + empty + empty + final).
            (expect (= 4 (count trace)))
            ;; Thinking must be preserved on the empty entries so the
            ;; UI has something to render.
            (expect (= "thinking 1" (:thinking (nth trace 1))))
            (expect (= "thinking 2" (:thinking (nth trace 2))))
            ;; Empty entries have empty :executions, not nil — the web
            ;; renderer's `(seq executions)` check distinguishes the
            ;; two and an `nil` there would look like "we never ran
            ;; this iteration" instead of "this iteration had no
            ;; tool calls". Pure-thinking iterations are real work.
            (expect (empty? (:executions (nth trace 1))))
            (expect (empty? (:executions (nth trace 2)))))
          (finally
            (#'rlm-core/dispose-rlm-env! env)))))))
