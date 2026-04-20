(ns com.blockether.vis.loop.runtime.query.subquery-test
  "Sub-RLM contract coverage.

   Regressions this suite locks down:
   - `build-sub-rlm-handoff` surfaces parent's *query*, *reasoning*,
     *answer* so the sub-agent inherits context, not a cold slate.
   - `fork-rlm-env-for-sub` ISOLATES the SCI sandbox: a binding made
     inside the fork never leaks to the parent. Without this, the
     parent's var_index filled with sub-RLM junk.
   - `run-sub-rlm` returns `:query-id` referring to a `:query` entity
     whose `parent_id` is the invoking iteration (not the conversation
     — otherwise sub-queries mix with top-level turns in the sidebar)."
  (:require
    [clojure.string :as str]
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.vis.loop.core :as rlm-core]
    [com.blockether.vis.loop.runtime.core :as rlm-tools]
    [com.blockether.vis.loop.runtime.query.subquery :as sut]
    [com.blockether.vis.loop.storage.db :as rlm-db]
    [com.blockether.vis.loop.storage.sqlite.core :as sqlite-core]
    [com.blockether.svar.internal.llm :as llm]))

(defn- make-test-env []
  (let [router (llm/make-router
                 [{:id :test :api-key "test" :base-url "http://localhost"
                   :models [{:name "gpt-4o"}]}])]
    (#'rlm-core/create-rlm-env (atom 0) router {:db :temp})))

(defdescribe build-sub-rlm-handoff-test
  (describe "build-sub-rlm-handoff"
    (it "surfaces parent's *query*, *reasoning*, *answer* and frames the sub-task"
      (let [env (make-test-env)]
        (try
          (rlm-tools/bind-and-bump! env '*query*     "locate delete icon")
          (rlm-tools/bind-and-bump! env '*reasoning* "searched src, found hiccup in sheet.clj")
          (rlm-tools/bind-and-bump! env '*answer*    "sheet-item-del class at line 42")
          (let [out (sut/build-sub-rlm-handoff env "now wire the CSS hover")]
            (expect (string? out))
            (expect (str/includes? out "[parent handoff]"))
            (expect (str/includes? out "[sub-task]"))
            (expect (str/includes? out "locate delete icon"))
            (expect (str/includes? out "searched src, found hiccup"))
            (expect (str/includes? out "sheet-item-del class"))
            (expect (str/includes? out "now wire the CSS hover")))
          (finally
            (#'rlm-core/dispose-rlm-env! env)))))

    (it "returns nil when parent has no readable state (fresh env)"
      (let [env (make-test-env)]
        (try
          (expect (nil? (sut/build-sub-rlm-handoff env "anything")))
          (finally
            (#'rlm-core/dispose-rlm-env! env)))))))

(defdescribe fork-rlm-env-for-sub-test
  (describe "fork-rlm-env-for-sub"
    (it "binds a var in the fork WITHOUT polluting the parent sandbox"
      (let [parent (make-test-env)]
        (try
          (rlm-tools/bind-and-bump! parent 'parent-only 42)
          (let [sub (sut/fork-rlm-env-for-sub parent nil)]
            ;; Fork sees parent's var at fork time.
            (let [sandbox-sub (get-in @(:env (:sci-ctx sub)) [:namespaces 'sandbox])]
              (expect (contains? sandbox-sub 'parent-only)))
            ;; Write into the fork ONLY.
            (rlm-tools/bind-and-bump! sub 'fork-only 99)
            (let [sandbox-parent (get-in @(:env (:sci-ctx parent)) [:namespaces 'sandbox])
                  sandbox-sub    (get-in @(:env (:sci-ctx sub))    [:namespaces 'sandbox])]
              (expect (contains? sandbox-sub 'fork-only))
              ;; The critical assertion: parent does NOT see the sub's binding.
              (expect (not (contains? sandbox-parent 'fork-only)))))
          (finally
            (#'rlm-core/dispose-rlm-env! parent)))))

    (it "threads :parent-iteration-ref through when supplied"
      (let [parent (make-test-env)
            fake-iter-ref [:id "deadbeef-0000"]]
        (try
          (let [sub (sut/fork-rlm-env-for-sub parent fake-iter-ref)]
            (expect (= fake-iter-ref (:parent-iteration-ref sub))))
          (finally
            (#'rlm-core/dispose-rlm-env! parent)))))))

(defdescribe run-sub-rlm-persistence-test
  (describe "run-sub-rlm persists a :query entity and returns :query-id"
    (it "parents the sub :query under the invoking iteration (not the conversation)"
      ;; Stub iteration-loop-fn so we don't need a real LLM. It must
      ;; behave like the real loop enough for run-sub-rlm to persist
      ;; the sub-query and seal it with a final answer.
      (let [parent (make-test-env)
            db-info (:db-info parent)]
        (try
          ;; Set up a conversation + a top-level query + one iteration
          ;; whose entity-ref we'll feed as the sub's parent.
          (let [conv-ref (rlm-db/store-conversation! db-info
                           {:system-prompt "test" :model "gpt-4o"})
                parent-with-conv (assoc parent :conversation-ref conv-ref)
                main-query-ref (rlm-db/store-query! db-info
                                 {:conversation-ref conv-ref
                                  :text "main query"
                                  :status :running})
                main-iter-ref (rlm-db/store-iteration! db-info
                                {:query-ref main-query-ref
                                 :executions [] :vars []
                                 :thinking "main thinking"
                                 :duration-ms 0})
                ;; Minimal iteration-loop stub: return a final-result shape.
                fake-loop (fn [_env _prompt _opts]
                            {:answer {:result "sub-answer" :type String}
                             :iterations 1
                             :status :final
                             :confidence :high})
                result (sut/run-sub-rlm fake-loop parent-with-conv
                         "go do this sub-task"
                         {:parent-iteration-ref main-iter-ref})]
            (expect (some? (:query-id result)))
            (expect (= "sub-answer" (:content result)))
            ;; The :query-id must identify a real entity whose parent
            ;; is the invoking iteration, not the conversation.
            (let [sub-q-id (:query-id result)
                  parent-row (first (sqlite-core/fetch-entities db-info [(str sub-q-id)]))
                  parent-id-of-sub (:parent-id parent-row)]
              (expect (some? parent-row))
              (expect (= (str (second main-iter-ref)) (str parent-id-of-sub)))))
          (finally
            (#'rlm-core/dispose-rlm-env! parent)))))))
