(ns com.blockether.vis.internal.activity-test
  (:require [com.blockether.vis.internal.activity :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- event-pair
  ([ctx operation outcome result] (event-pair ctx operation outcome result {}))
  ([ctx operation outcome result {:keys [args] :as details}]
   (let [invocation
         (event/invocation ctx nil)

         common
         (merge {:operation operation :presenter :generic} (dissoc details :args))

         args
         (or args [{:path "file.clj"}])

         started
         (System/currentTimeMillis)]

     [(event/start-event ctx invocation (assoc common :args args))
      (event/terminal-event ctx
                            invocation
                            (cond-> (assoc common
                                      :args args
                                      :started-at-ms started
                                      :outcome outcome)
                              (= outcome :succeeded)
                              (assoc :result result)

                              (not= outcome :succeeded)
                              (assoc :error (ex-info (str result) {}))))])))

(defdescribe
  activity-reducer-test
  (it "keeps wrapper-entry order when terminals settle out of order"
      (let [ctx
            (event/context {})

            [a-start a-end*]
            (event-pair ctx :grep :succeeded {:matches 3})

            [b-start* b-end*]
            (event-pair ctx :run_tests :succeeded {:pass 2})

            b-start
            (assoc b-start* :event-sequence 2)

            b-end
            (assoc b-end* :event-sequence 3)

            a-end
            (assoc a-end* :event-sequence 4)

            state
            (activity/replay {:evaluation-id (:evaluation-id ctx) :iteration 4 :form-index 1}
                             [a-start b-start b-end a-end])]

        (expect (= [:grep :run_tests] (mapv :operation (:rows state))))
        (expect (= [:succeeded :succeeded] (mapv :state (:rows state))))
        (expect (= :succeeded (:state state)))
        (expect (= 2 (get-in state [:counts :succeeded])))))
  (it "classifies specialized operations without client-side inference"
      (let [ctx
            (event/context {})

            pairs
            [(event-pair ctx
                         :misleading_read_only_patch
                         :succeeded
                         {:edits 2}
                         {:presenter :generic :classification :observation})
             (event-pair ctx
                         :unknown_verb :failed
                         "one failed" {:presenter :tests :classification :mutation})
             (event-pair ctx
                         :patch
                         :succeeded
                         {:edits 2}
                         {:presenter :patch :classification :mutation})]

            state
            (activity/replay {:evaluation-id (:evaluation-id ctx)} (mapcat identity pairs))]

        (expect (= [:generic :tests :patch] (mapv :presenter (:rows state))))
        (expect (= [:observation :mutation :mutation] (mapv :classification (:rows state))))
        (expect (= :failed (:state state)))))
  (it "exports the current bounded semantic projection without channel markup"
      (let [ctx
            (event/context {})

            anchor
            {:evaluation-id (:evaluation-id ctx) :iteration 3 :form-index 1}

            events
            (mapcat identity
                    [(event-pair ctx
                                 :patch
                                 :succeeded
                                 {:edits 2}
                                 {:presenter :patch :classification :mutation})])

            presentation
            (activity/presentation (activity/replay anchor events))

            row
            (first (:rows presentation))]

        (expect (= 1 (:schema-version presentation)))
        (expect (= anchor (:anchor presentation)))
        (expect (= "patch" (:presenter row)))
        (expect (= "mutation" (:signal row)))
        (expect (= "succeeded" (:state row)))
        (expect (= [:arguments :result] (mapv (comp keyword :kind) (:evidence row))))
        (expect (nil? (activity/presentation-error presentation)))
        (expect (= "unknown Activity schema version"
                   (activity/presentation-error (assoc presentation :schema-version 2))))))
  (it "preserves structured patch evidence through the presentation boundary"
      (let [ctx
            (event/context {})

            anchor
            {:evaluation-id (:evaluation-id ctx) :iteration 0 :form-index 0}

            events
            (event-pair ctx
                        :patch :succeeded
                        "patched fixture.clj" {:presenter :patch
                                               :classification :mutation
                                               :result-envelope
                                               {:metadata {:target {:resolved "fixture.clj"}
                                                           :diff "@@ -1 +1 @@\n-before\n+after"
                                                           :lines
                                                           {"added" 0 "removed" 0 "modified" 1}}}})

            presentation
            (activity/presentation (activity/replay anchor events))

            evidence
            (last (get-in presentation [:rows 0 :evidence]))]

        (expect (= "diff" (:kind evidence)))
        (expect (= ["hunk" "deletion" "addition"] (mapv :kind (:lines evidence))))
        (expect (= 1 (:modifications evidence)))
        (expect (nil? (activity/presentation-error presentation)))))
  (it "terminalizes rows still running when the evaluation is killed"
      (let [ctx
            (event/context {})

            [started _]
            (event-pair ctx :grep :succeeded {:matches 3})

            state
            (activity/settle-running (activity/reduce-event (activity/empty-state
                                                              {:evaluation-id (:evaluation-id ctx)})
                                                            started)
                                     :cancelled
                                     "Evaluation timed out")]

        (expect (= :cancelled (:state state)))
        (expect (= {:running 0 :succeeded 0 :failed 0 :cancelled 1} (:counts state)))
        (expect (= [:cancelled] (mapv :state (:rows state))))))
  (it "never retains more than the row or receipt byte budget"
      (let [ctx
            (event/context {})

            pairs
            (mapv (fn [n]
                    (event-pair ctx
                                (keyword (str "unknown_" n))
                                :succeeded
                                {:text (apply str (repeat 1000 "界"))}))
                  (range 150))

            state
            (activity/replay {:evaluation-id (:evaluation-id ctx)} (mapcat identity pairs))

            snapshot
            (activity/snapshot state)]

        (expect (<= (count (:rows snapshot)) activity/max-rows))
        (expect (<= (activity/byte-size snapshot) activity/max-receipt-bytes))
        (expect (= 150 (get-in snapshot [:counts :succeeded])))))
  ;; Regression, issue td-1e6086: Activity was projected both as semantic data and as
  ;; generic status/stat/steps nodes, so every channel had two sources of truth.
  (it "keeps one bounded semantic presentation at the 128-row ceiling"
      (let [ctx
            (event/context {})

            starts
            (mapv (fn [n]
                    (first (event-pair ctx (keyword (str "operation_" n)) :succeeded {:ok true})))
                  (range activity/max-rows))

            state
            (last (rest (reductions activity/reduce-event
                                    (activity/empty-state {:evaluation-id (:evaluation-id ctx)})
                                    starts)))

            presentation
            (activity/presentation state)]

        (expect (= activity/max-rows (count (:rows presentation))))
        (expect (nil? (activity/presentation-error presentation)))))
  ;; Regression, issues td-1ccd13 and td-574cf3: shell-handle groups replaced the
  ;; command with a generic count or froze its transient `running` phrase into the
  ;; settled Activity receipt.
  (it
    "coalesces one typed shell handle while preserving its command and child chronology"
    (let [ctx
          (event/context {})

          pairs
          [(event-pair ctx
                       :shell
                       :succeeded
                       {"id" "build-1" "status" "running"}
                       {:args ["npm test"] :presenter :shell :phrase "running: npm test"})
           (event-pair ctx
                       :_shell_logs
                       :succeeded
                       {"id" "build-1" "out" "42 lines" "status" "running"}
                       {:args ["build-1"] :presenter :shell})
           (event-pair ctx
                       :_shell_wait
                       :succeeded
                       {"id" "build-1" "exit" 0 "status" "exited"}
                       {:args ["build-1" 30] :presenter :shell})]

          snapshot
          (activity/snapshot (activity/replay {:evaluation-id (:evaluation-id ctx)}
                                              (mapcat identity pairs)))

          group
          (first (:rows snapshot))]

      (expect (= 1 (count (:rows snapshot))))
      (expect (= :shell (:operation group)))
      (expect (= "cmd: npm test" (:summary group)))
      (expect (= ["shell" "cmd: npm test"]
                 ((juxt :operation :summary) (first (:rows (activity/presentation snapshot))))))
      (expect (= [:shell :_shell_logs :_shell_wait] (mapv :operation (:children group))))))
  (it "groups only adjacent observations with the same explicit token"
      (let [ctx
            (event/context {})

            pairs
            [(event-pair ctx
                         :grep
                         :succeeded
                         {:matches 2}
                         {:group-token "inspect" :presenter :observation})
             (event-pair ctx
                         :cat
                         :succeeded
                         {:path "a.clj"}
                         {:group-token "inspect" :presenter :observation})
             (event-pair ctx :patch :succeeded {:edits 1} {:presenter :patch})
             (event-pair ctx
                         :grep
                         :succeeded
                         {:matches 1}
                         {:group-token "inspect" :presenter :observation})]

            rows
            (:rows (activity/snapshot (activity/replay {:evaluation-id (:evaluation-id ctx)}
                                                       (mapcat identity pairs))))]

        (expect (= 3 (count rows)))
        (expect (= :observations (:operation (first rows))))
        (expect (= "observations · 2 operations" (:summary (first rows))))
        (expect (= :patch (:operation (second rows))))
        (expect (= :grep (:operation (last rows))))))
  (it "replay is deterministic for the same immutable event order"
      (let [ctx
            (event/context {})

            events
            (vec (mapcat identity
                         [(event-pair ctx :grep :succeeded {:matches 1})
                          (event-pair ctx :patch :failed "refused")]))

            anchor
            {:evaluation-id (:evaluation-id ctx) :iteration 2 :form-index 0}]

        (expect (= (activity/replay anchor events) (activity/replay anchor events))))))
