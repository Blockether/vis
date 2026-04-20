(ns com.blockether.vis.loop.cross-query-handover-test
  "Regression test for the cross-query handover block.

   When a user asks a second question in the same conversation, the
   NEW query's iteration 0 should see a `[prior turn]` block with the
   last 2 reasonings + final answer from the PREVIOUS query. Without
   this, the new turn starts cold even though the model's own answer
   to the immediately-prior question is usually the most relevant
   context.

   Sub-RLMs (env with `:parent-iteration-ref`) INTENTIONALLY skip this
   handover — they get a targeted `[parent handoff]` instead via
   `build-sub-rlm-handoff`, scoped to the invoking iteration."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.core :as sut]
    [com.blockether.vis.loop.storage.db :as rlm-db]
    [com.blockether.svar.internal.llm :as llm]))

(defn- make-test-env []
  (let [router (llm/make-router
                 [{:id :test :api-key "test" :base-url "http://localhost"
                   :models [{:name "gpt-4o"}]}])]
    (#'sut/create-rlm-env (atom 0) router {:db :temp})))

(defdescribe build-cross-query-handover-test
  (describe "build-cross-query-handover"
    (it "renders [prior turn] with last 2 iterations' thinking + final answer"
      (let [env (make-test-env)
            db-info (:db-info env)]
        (try
          (let [conv-ref (rlm-db/store-conversation! db-info {:system-prompt "" :model "gpt-4o"})
                ;; Previous query: 3 iterations, final answer set.
                prev-q   (rlm-db/store-query! db-info
                           {:conversation-ref conv-ref
                            :text "where is the delete icon"
                            :answer "sheet-item-del at line 42"
                            :iterations 3
                            :status :final})
                _        (rlm-db/store-iteration! db-info
                           {:query-ref prev-q :executions [] :vars []
                            :thinking "searched src for icon" :duration-ms 0})
                _        (rlm-db/store-iteration! db-info
                           {:query-ref prev-q :executions [] :vars []
                            :thinking "read sheet.clj" :duration-ms 0})
                _        (rlm-db/store-iteration! db-info
                           {:query-ref prev-q :executions [] :vars []
                            :thinking "found .sheet-item-del" :duration-ms 0})
                ;; Current query, no iterations yet (new turn).
                curr-q   (rlm-db/store-query! db-info
                           {:conversation-ref conv-ref
                            :text "now add hover style"
                            :status :running})
                handover (#'sut/build-cross-query-handover db-info conv-ref curr-q nil)]
            (expect (string? handover))
            (expect (str/includes? handover "[prior turn]"))
            (expect (str/includes? handover "[new query]"))
            ;; Last 2 iterations of prior query (iter 1 + iter 2 of prior).
            (expect (str/includes? handover "read sheet.clj"))
            (expect (str/includes? handover "found .sheet-item-del"))
            ;; Earliest iteration must be clipped.
            (expect (not (str/includes? handover "searched src for icon")))
            ;; Final answer present.
            (expect (str/includes? handover "sheet-item-del at line 42")))
          (finally
            (#'sut/dispose-rlm-env! env)))))

    (it "returns nil when the conversation has no prior query"
      (let [env (make-test-env)
            db-info (:db-info env)]
        (try
          (let [conv-ref (rlm-db/store-conversation! db-info {:system-prompt "" :model "gpt-4o"})
                curr-q   (rlm-db/store-query! db-info
                           {:conversation-ref conv-ref :text "first ever" :status :running})]
            (expect (nil? (#'sut/build-cross-query-handover db-info conv-ref curr-q nil))))
          (finally
            (#'sut/dispose-rlm-env! env)))))

    (it "suppresses handover for sub-RLMs (parent-iteration-ref set)"
      ;; Sub-RLMs get `[parent handoff]` from build-sub-rlm-handoff,
      ;; not this broad last-turn block. Mixing both would double-feed
      ;; the same info to the LLM.
      (let [env (make-test-env)
            db-info (:db-info env)]
        (try
          (let [conv-ref (rlm-db/store-conversation! db-info {:system-prompt "" :model "gpt-4o"})
                prev-q   (rlm-db/store-query! db-info
                           {:conversation-ref conv-ref :text "main" :answer "done" :iterations 1 :status :final})
                _        (rlm-db/store-iteration! db-info
                           {:query-ref prev-q :executions [] :vars [] :thinking "t" :duration-ms 0})
                curr-q   (rlm-db/store-query! db-info
                           {:conversation-ref conv-ref :text "sub" :status :running})
                handover (#'sut/build-cross-query-handover db-info conv-ref curr-q [:id "some-iter-id"])]
            (expect (nil? handover)))
          (finally
            (#'sut/dispose-rlm-env! env)))))))
