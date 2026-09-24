(ns com.blockether.vis.internal.activity.core-test
  (:require [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.activity :as contract]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- event-pair
  ([ctx operation outcome result] (event-pair ctx operation outcome result {}))
  ([ctx operation outcome result {:keys [args] :as details}]
   (let [invocation
         (event/invocation ctx nil)

         common
         (merge {:operation operation :presenter :generic}
                (when (and (= :shell (:presenter details))
                           (contains? #{:shell :_shell-logs :_shell-wait :_shell-type :_shell-stop}
                                      operation))
                  {:activity (presenter/for-tool operation)})
                (dissoc details :args))

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
  generic-handle-receipts-test
  (it
    "shows one current comparison but retains earlier findings and all invocations"
    (let [ctx
          (event/context)

          receipt
          (fn [id headline summary body]
            {:activity {:render (fn [_ _]
                                  {"headline" headline
                                   "summary" summary
                                   "handle_id" id
                                   "content" [{"type" "text" "text" body}]})}
             :extension "visual.tools"
             :presenter :generic
             :label "Checking visual differences"})

          pairs
          [(event-pair ctx
                       :compare :succeeded
                       :started (receipt "compare-1" "Comparing" "In progress" "Early finding"))
           (event-pair ctx
                       :compare-status :succeeded
                       :finished (receipt "compare-1" "Comparison finished"
                                          "Two differences" "Latest finding"))
           (event-pair ctx
                       :compare :succeeded
                       :other (receipt "compare-2" "Comparison finished"
                                       "No differences" "Other comparison"))
           (event-pair ctx
                       :compare :succeeded
                       :unlinked {:label "Checking visual differences" :extension "visual.tools"})
           (event-pair ctx
                       :compare :succeeded
                       :foreign (assoc (receipt "compare-1" "Foreign comparison"
                                                "Finished" "Foreign")
                                  :extension "other.tools"))]

          projection
          (activity/presentation (activity/replay (mapcat identity pairs)))

          [head & remainder]
          (:rows projection)]

      (expect (= 4 (count (:rows projection))))
      (expect (= "Comparison finished" (get-in head [:presentation "headline"])))
      (expect (= "Two differences" (get-in head [:presentation "summary"])))
      (expect (= ["Early finding" "Latest finding"]
                 (mapv #(get % "text")
                       (concat (get-in head [:presentation "sections" 0 "content"])
                               (get-in head [:presentation "content"])))))
      (expect (= ["compare" "compare-status"] (mapv :operation (:children head))))
      (expect (= ["compare-1" "compare-2" nil "compare-1"] (mapv :handle-id (cons head remainder))))
      (expect (= 5 (get-in projection [:counts :succeeded])))
      (expect (contract/valid-projection? projection))))
  (it
    "preserves unique summaries and observed output without a detail body"
    (let [ctx
          (event/context)

          receipt
          (fn [summary]
            {:extension "visual.tools"
             :presenter :generic
             :activity {:render (fn [_ _]
                                  {"headline" "Checking visual differences"
                                   "summary" summary
                                   "handle_id" "compare-3"
                                   "content" []})}})

          pairs
          [(event-pair ctx :compare :succeeded "Original result" (receipt "Early status"))
           (event-pair ctx :compare-status :succeeded "Final result" (receipt "Done"))]

          projection
          (activity/presentation (activity/replay (mapcat identity pairs)))

          head
          (first (:rows projection))

          earlier
          (get-in head [:presentation "sections" 0 "content"])]

      (expect (= 1 (count (:rows projection))))
      (expect (= "Done" (get-in head [:presentation "summary"])))
      (expect (= ["Early status" "\"Original result\""] (mapv #(get % "text") earlier)))
      (expect (= 2 (count (:children head))))
      (expect (= (get-in head [:children 0 :id])
                 (:id (first (contract/operation-groups (:rows projection))))))
      (expect (contract/valid-projection? projection)))))

(defdescribe
  lossless-history-test
  ;; Regression #212: neither invocation count nor aggregate bytes may discard history.
  (it "retains every admitted operation and its detail past both former receipt limits"
      (let [ctx
            (event/context)

            events
            (vec (mapcat (fn [n]
                           (event-pair ctx
                                       (keyword (str "operation_" n))
                                       (cond (= n 298) :failed
                                             (= n 299) :cancelled
                                             :else :succeeded)
                                       (str n " " (apply str (repeat 1500 "x")))))
                         (range 300)))

            state
            (activity/replay events)

            projection
            (activity/presentation state)]

        (expect (= 300 (count (:rows state))))
        (expect (= 300 (count (:rows projection))))
        (expect (= (mapv :invocation-id (take-nth 2 events)) (mapv :id (:rows projection))))
        (expect (= (mapv :result-summary (:rows state)) (mapv :result-summary (:rows projection))))
        (expect (= ["failed" "cancelled"] (mapv :state (take-last 2 (:rows projection)))))
        (expect (zero? (get-in projection [:omitted :rows])))
        (expect (> (activity/byte-size projection) 65536))
        (expect (contract/valid-projection? projection)))))

(defdescribe
  completed-file-operations-test
  (it "uses declared end-only visibility for built-ins and extension operations"
      (doseq [operation [:cat :patch]]
        (expect (false? (:show-start (presenter/for-tool operation)))))
      (doseq [operation
              [:cat :patch :custom.lookup]

              outcome
              [:succeeded :failed :cancelled]]

        (let [[start terminal]
              (event-pair (event/context)
                          operation
                          outcome
                          "retained result"
                          {:activity {:headline "Read record" :show-start false}})

              running
              (activity/reduce-event activity/empty-state start)

              visible
              (activity/presentation running)

              settled
              (activity/presentation (activity/reduce-event running terminal))]

          (expect (= 1 (count (:rows running))))
          (expect (empty? (:rows visible)))
          (expect (= 1 (get-in visible [:counts :running])))
          (expect (= 0 (get-in visible [:omitted :rows])))
          (expect (= [(name outcome)] (mapv :state (:rows settled))))
          (expect (contract/valid-projection? settled)))))
  (it "does not infer visibility from the operation name"
      (let [[start] (event-pair (event/context)
                                :cat
                                :succeeded
                                nil
                                {:activity {:headline "Read file" :show-start true}})]
        (expect (= 1
                   (count (:rows (activity/presentation (activity/reduce-event activity/empty-state
                                                                               start))))))))
  (it "retains hidden content and shows cancellation when evaluation ends"
      (let [ctx
            (event/context)

            invocation
            (event/invocation ctx nil)

            details
            {:operation :custom.lookup
             :presenter :generic
             :activity {:headline "Read record" :show-start false}}

            content
            {"headline" "Read record" "summary" "One record" "content" []}

            running
            (activity/replay [(event/start-event ctx invocation details)
                              (event/content-event ctx invocation details content)])

            settled
            (activity/presentation (activity/settle-running running :cancelled "Cancelled"))]

        (expect (empty? (:rows (activity/presentation running))))
        (expect (= "cancelled" (get-in settled [:rows 0 :state])))
        (expect (= content (get-in settled [:rows 0 :presentation])))
        (expect (contract/valid-projection? settled)))))

(defdescribe
  argument-identity-test
  (it "retains complete argument identity through lifecycle and canonical projection"
      (let [ctx
            (event/context)

            events
            (vec (concat
                   (event-pair ctx
                               :grep :succeeded
                               "first result" {:args [{:query ["same"] :paths ["src"]}]})
                   (event-pair ctx
                               :grep :failed
                               "second result" {:args [(array-map :paths ["src"] :query ["same"])]})
                   (event-pair ctx
                               :grep :succeeded
                               "third result" {:args [{:query ["different"] :paths ["src"]}]})))

            projection
            (-> events
                activity/replay
                activity/presentation)

            keys
            (mapv :argument-key (:rows projection))]

        (expect (contract/valid-projection? projection))
        (expect (every? #(and (string? %) (re-matches #"[0-9a-f]{64}" %)) keys))
        (expect (= (first keys) (second keys)))
        (expect (not= (first keys) (last keys)))
        (expect (= (mapv :argument-key (take-nth 2 events)) keys))
        (expect (= (mapv :argument-key (take-nth 2 (rest events))) keys))
        (expect (= 3 (reduce + (vals (:counts projection)))))
        (expect (= ["succeeded" "failed" "succeeded"] (mapv :state (:rows projection)))))))

(defdescribe
  read-identity-test
  (it "retains target identity through lifecycle and wire projection without merging invocations"
      (let [ctx
            (event/context)

            events
            (vec (concat
                   (event-pair ctx :cat :succeeded "first excerpt" {:args ["PLAN.md" 583 591]})
                   (event-pair ctx :cat :failed "read failed" {:args ["PLAN.md" 615 623]})
                   (event-pair ctx :cat :succeeded "another file" {:args ["README.md" 1 5]})))

            projection
            (-> events
                activity/replay
                activity/presentation)

            keys
            (mapv :read-key (:rows projection))]

        (expect (contract/valid-projection? projection))
        (expect (every? #(and (string? %) (re-matches #"[0-9a-f]{64}" %)) keys))
        (expect (= (first keys) (second keys)))
        (expect (not= (first keys) (last keys)))
        (expect (= keys (mapv :read-key (take-nth 2 events))))
        (expect (= keys (mapv :read-key (take-nth 2 (rest events)))))
        (expect (= 3 (count (:rows projection))))
        (expect (= 3 (reduce + (vals (:counts projection)))))
        (expect (= ["succeeded" "failed" "succeeded"] (mapv :state (:rows projection))))
        (doseq [key [nil "" "guess" (apply str (repeat 64 "A")) 42]]
          (expect (not (contract/valid-projection?
                         (assoc-in projection [:rows 0 :read-key] key))))))))

(defdescribe canonical-presentation-test
             (it "keeps required lines for an empty structured diff"
                 (let [evidence (#'activity/presentation-evidence
                                 {:kind :diff
                                  :text "file.clj"
                                  :lines []
                                  :additions 0
                                  :deletions 0
                                  :modifications 0
                                  :is-truncated false
                                  :is-redacted false})]
                   (expect (= [] (:lines evidence)))
                   (expect (document/valid? "activity" "evidence" evidence))))
             (it "produces a contract-valid receipt for every terminal outcome"
                 (doseq [outcome [:succeeded :failed :cancelled]]
                   (let [projection (-> (event-pair (event/context) :check outcome "result")
                                        activity/replay
                                        activity/presentation)]
                     (expect (contract/valid-projection? projection))))))

(defdescribe
  activity-reducer-test
  (it "keeps wrapper-entry order when terminals settle out of order"
      (let [ctx
            (event/context)

            [a-start a-end*]
            (event-pair ctx :grep :succeeded {:matches 3})

            [b-start* b-end*]
            (event-pair ctx :shell :succeeded {:exit 0})

            b-start
            (assoc b-start* :event-sequence 2)

            b-end
            (assoc b-end* :event-sequence 3)

            a-end
            (assoc a-end* :event-sequence 4)

            state
            (activity/replay [a-start b-start b-end a-end])]

        (expect (= [:grep :shell] (mapv :operation (:rows state))))
        (expect (= [:succeeded :succeeded] (mapv :state (:rows state))))
        (expect (= :succeeded (:state state)))
        (expect (= 2 (get-in state [:counts :succeeded])))))
  (it "classifies specialized operations without client-side inference"
      (let [ctx
            (event/context)

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
            (activity/replay (mapcat identity pairs))]

        (expect (= [:generic :tests :patch] (mapv :presenter (:rows state))))
        (expect (= [:observation :mutation :mutation] (mapv :classification (:rows state))))
        (expect (= :failed (:state state)))))
  (it "exports the current bounded semantic projection without channel markup"
      (let [ctx
            (event/context)

            events
            (mapcat identity
                    [(event-pair ctx
                                 :patch
                                 :succeeded
                                 {:edits 2}
                                 {:presenter :patch :classification :mutation})])

            presentation
            (activity/presentation (activity/replay events))

            row
            (first (:rows presentation))]

        ;; Activity does not know its owner: no version, no anchor, no parent ids.
        (expect (= #{:state :counts :rows :omitted} (set (keys presentation))))
        (expect (= "patch" (:presenter row)))
        (expect (= "mutation" (:signal row)))
        (expect (= "succeeded" (:state row)))
        (expect (= [:arguments :result] (mapv (comp keyword :kind) (:evidence row))))))
  (it "preserves structured patch evidence through the presentation boundary"
      (let [ctx
            (event/context)

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
            (activity/presentation (activity/replay events))

            evidence
            (last (get-in presentation [:rows 0 :evidence]))]

        (expect (= "diff" (:kind evidence)))
        (expect (= ["hunk" "deletion" "addition"] (mapv :kind (:lines evidence))))
        (expect (= 1 (:modifications evidence)))))
  (it "terminalizes rows still running when the evaluation is killed"
      (let [ctx
            (event/context)

            [started _]
            (event-pair ctx :grep :succeeded {:matches 3})

            state
            (activity/settle-running (activity/reduce-event activity/empty-state started)
                                     :cancelled
                                     "Evaluation timed out")]

        (expect (= :cancelled (:state state)))
        (expect (= {:running 0 :succeeded 0 :failed 0 :cancelled 1} (:counts state)))
        (expect (= [:cancelled] (mapv :state (:rows state))))))
  (it "retains every admitted row past former receipt budgets"
      (let [ctx
            (event/context)

            pairs
            (mapv (fn [n]
                    (event-pair ctx
                                (keyword (str "unknown_" n))
                                :succeeded
                                {:text (apply str (repeat 1000 "界"))}))
                  (range 150))

            state
            (activity/replay (mapcat identity pairs))

            snapshot
            (activity/snapshot state)]

        (expect (= 150 (count (:rows snapshot))))
        (expect (> (activity/byte-size snapshot) 65536))
        (expect (= 150 (get-in snapshot [:counts :succeeded])))))
  ;; Regression, issue td-1e6086: Activity was projected both as semantic data and as
  ;; generic status/stat/steps nodes, so every channel had two sources of truth.
  (it "keeps one semantic presentation beyond the former row ceiling"
      (let [ctx
            (event/context)

            starts
            (mapv (fn [n]
                    (first (event-pair ctx (keyword (str "operation_" n)) :succeeded {:ok true})))
                  (range 256))

            state
            (last (rest (reductions activity/reduce-event activity/empty-state starts)))

            presentation
            (activity/presentation state)]

        (expect (= 256 (count (:rows presentation))))))
  ;; Regression, issues td-1ccd13 and td-574cf3: shell-handle groups replaced the
  ;; command with a generic count or froze its transient `running` phrase into the
  ;; settled Activity receipt.
  (it
    "coalesces one typed shell handle while preserving its command and child chronology"
    (let [ctx
          (event/context)

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
          (activity/snapshot (activity/replay (mapcat identity pairs)))

          group
          (first (:rows snapshot))]

      (expect (= 1 (count (:rows snapshot))))
      (expect (= :shell (:operation group)))
      (expect (= "npm test" (:summary group)))
      (expect (= (:argument-key (ffirst pairs)) (:argument-key group)))
      (expect (= (:argument-key group)
                 (:argument-key (first (:rows (activity/presentation snapshot))))))
      (expect (not= (:argument-key group) (:argument-key (first (second pairs)))))
      (expect (= ["shell" "npm test"]
                 ((juxt :operation :summary) (first (:rows (activity/presentation snapshot))))))
      (expect (= [:shell :_shell_logs :_shell_wait] (mapv :operation (:children group))))))
  ;; A shell handle is one current command, not a set of competing completed receipts.
  ;; Each individual call stays available in its chronological history.
  (it
    "reconciles overlapping output and the latest exit while retaining every receipt"
    (let [ctx
          (event/context)

          pairs
          [(event-pair ctx
                       :shell
                       :succeeded
                       {"id" "job-1" "command" "printf result" "status" "running"}
                       {:args ["printf result"] :presenter :shell})
           (event-pair ctx
                       :_shell-logs
                       :succeeded
                       {"id" "job-1"
                        "command" "printf result"
                        "out" "early\nshared\n"
                        "err" "warning\n"
                        "status" "running"}
                       {:args ["job-1"] :presenter :shell})
           (event-pair ctx
                       :_shell-wait
                       :succeeded
                       {"id" "job-1"
                        "command" "printf result"
                        "out" "shared\nlate\n"
                        "exit" 0
                        "status" "exited"}
                       {:args ["job-1" 30] :presenter :shell})]

          state
          (activity/replay (mapcat identity pairs))

          projection
          (activity/presentation state)

          group
          (first (:rows projection))

          blocks
          (get-in group [:presentation "content"])]

      (expect (= 1 (count (:rows projection))))
      (expect (= "Command finished" (get-in group [:presentation "headline"])))
      (expect (= "printf result" (get-in group [:presentation "summary"])))
      (expect (= 1 (count (filter #(= "**Exit code:** 0" (get % "text")) blocks))))
      (expect (= 1 (count (filter #(= "early\nshared\nlate\n" (get % "text")) blocks))))
      (expect (= 1 (count (filter #(= "warning\n" (get % "text")) blocks))))
      (expect (= 3 (count (:children group))))
      (expect (= (mapv :id (:rows state)) (mapv :id (:children group))))
      (expect (= 3 (get-in projection [:counts :succeeded])))
      (expect (contract/valid-projection? projection))))
  (it
    "keeps failed receipts visible after a later update and never mixes handles"
    (let [ctx
          (event/context)

          pairs
          [(event-pair ctx
                       :shell
                       :succeeded
                       {"id" "one" "command" "build" "status" "running"}
                       {:args ["build"] :presenter :shell})
           (event-pair ctx
                       :shell
                       :succeeded
                       {"id" "two" "command" "other" "status" "running"}
                       {:args ["other"] :presenter :shell})
           (event-pair ctx
                       :_shell-wait :failed
                       "connection lost" {:args ["one" 1] :presenter :shell})
           (event-pair ctx
                       :_shell-logs
                       :succeeded
                       {"id" "one" "command" "build" "out" "recovered" "exit" 0 "status" "exited"}
                       {:args ["one"] :presenter :shell})]

          state
          (activity/replay (mapcat identity pairs))

          rows
          (:rows (activity/presentation state))

          one
          (first rows)]

      (expect (= ["build" "other"] (mapv :summary rows)))
      (expect (= "succeeded" (:state one)))
      (expect (= "Command finished" (get-in one [:presentation "headline"])))
      (expect (some #(= "recovered" (get % "text")) (get-in one [:presentation "content"])))
      (expect (some #(= "connection lost" (get % "text")) (get-in one [:presentation "content"])))
      (expect (= ["shell" "_shell-wait" "_shell-logs"] (mapv :operation (:children one))))
      (expect (= ["succeeded" "failed" "succeeded"] (mapv :state (:children one))))
      (expect (= 1 (get-in (:counts (activity/presentation state)) [:failed])))
      (expect (contract/valid-projection? (activity/presentation state)))))
  (it "preserves whitespace output and shows a failed follow-up only once"
      (let [ctx
            (event/context)

            pairs
            [(event-pair ctx
                         :shell
                         :succeeded
                         {"id" "job" "command" "run" "out" "  " "status" "running"}
                         {:args ["run"] :presenter :shell})
             (event-pair ctx
                         :_shell-logs
                         :succeeded
                         {"id" "job" "command" "run" "out" "tail" "status" "running"}
                         {:args ["job"] :presenter :shell})
             (event-pair ctx
                         :_shell-wait :failed
                         "not responding" {:args ["job" 1] :presenter :shell})]

            group
            (first (:rows (activity/presentation (activity/replay (mapcat identity pairs)))))

            blocks
            (get-in group [:presentation "content"])]

        (expect (= "failed" (:state group)))
        (expect (= "Command status unavailable" (get-in group [:presentation "headline"])))
        (expect (some #(= "  \ntail" (get % "text")) blocks))
        (expect (= 1 (count (filter #(= "not responding" (get % "text")) blocks))))
        (expect (nil? (:error-summary group)))
        (expect (= 3 (count (:children group))))))
  (it "groups only adjacent observations with the same explicit token"
      (let [ctx
            (event/context)

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
            (:rows (activity/snapshot (activity/replay (mapcat identity pairs))))]

        (expect (= 3 (count rows)))
        (expect (= :observations (:operation (first rows))))
        (expect (= "observations · 2 operations" (:summary (first rows))))
        (expect (= :patch (:operation (second rows))))
        (expect (= :grep (:operation (last rows))))))
  ;; Regression: a group head borrowed its first child's id, so the tree carried the same
  ;; id twice and the app dropped the whole projection rather than key a duplicate row.
  (it
    "gives a group head an id of its own"
    (let [ctx
          (event/context)

          pairs
          [(event-pair ctx
                       :write
                       :succeeded
                       {:path "a.clj"}
                       {:group-token "fs-mutations-1"
                        :presenter :observation
                        :group-head {:operation :change
                                     :summary "2 files"
                                     :result-summary "The code block changed these itself."
                                     :result-format :markdown}})
           (event-pair ctx
                       :delete
                       :succeeded
                       {:path "b.clj"}
                       {:group-token "fs-mutations-1" :presenter :observation})]

          rows
          (:rows (activity/snapshot (activity/replay (mapcat identity pairs))))

          head
          (first rows)

          ids
          (into [(:id head)] (map :id) (:children head))]

      (expect (= 1 (count rows)))
      (expect (= :change (:operation head)))
      (expect (= "2 files" (:summary head)))
      (expect (= :markdown (:result-format head)))
      (expect (= [:write :delete] (mapv :operation (:children head))))
      (expect (= (count ids) (count (distinct ids))))))
  (it "replay is deterministic for the same immutable event order"
      (let [ctx
            (event/context)

            events
            (vec (mapcat identity
                         [(event-pair ctx :grep :succeeded {:matches 1})
                          (event-pair ctx :patch :failed "refused")]))]

        (expect (= (activity/replay events) (activity/replay events))))))

(defdescribe
  live-activity-budget-test
  ;; A live shell group and its running step vanished behind an inert "more steps"
  ;; count when duplicate result evidence filled the receipt byte budget.
  (it
    "keeps every result and custom view when only duplicate evidence exceeds the budget"
    (let [ctx
          (event/context)

          pairs
          (mapv (fn [n]
                  (event-pair ctx
                              :grep
                              :succeeded
                              {:text (apply str (repeat 2000 "x"))}
                              {:phrase (str "search " n)}))
                (range 20))

          content
          {"headline" "Search results" "summary" "20 matches" "content" []}

          state
          (update (activity/replay (mapcat identity pairs))
                  :rows
                  #(mapv (fn [row]
                           (assoc row :presentation content))
                         %))

          projection
          (activity/presentation state)]

      (expect (> (activity/byte-size state) 65536))
      (expect (= 20 (count (:rows projection))))
      (expect (zero? (get-in projection [:omitted :rows])))
      (expect (= (mapv :result-summary (:rows state)) (mapv :result-summary (:rows projection))))
      (expect (every? #(= content (:presentation %)) (:rows projection)))
      (expect (not-any? :is-truncated (:rows projection)))
      (expect (> (activity/byte-size projection) 65536))
      (expect (contract/valid-projection? projection))))
  (it
    "keeps a large shell group and the current invocation available during live replacement"
    (let [ctx
          (event/context)

          pairs
          (mapv (fn [n]
                  (event-pair ctx
                              (if (zero? n) :shell :_shell_logs)
                              :succeeded
                              {"id" "build-live" "out" (apply str (repeat 2000 "x"))}
                              {:presenter :shell
                               :activity (presenter/for-tool :shell)
                               :classification :mutation
                               :args (if (zero? n) ["npm test"] ["build-live"])}))
                (range 24))

          running
          (first (event-pair ctx
                             :shell
                             :succeeded
                             nil
                             {:presenter :shell :classification :mutation :args ["npm run lint"]}))

          state
          (activity/replay (concat (mapcat identity pairs) [running]))

          projection
          (activity/presentation state)

          [group current]
          (:rows projection)]

      (expect (> (activity/byte-size state) 65536))
      (expect (= 24 (count (:children group))))
      (expect (= (:invocation-id running) (:id current)))
      (expect (= "running" (:state current)))
      ;; Regression #212: grouped children retain their complete admitted bodies.
      (expect (every? #(some (fn [block]
                               (= (apply str (repeat 2000 "x")) (get block "text")))
                             (get-in % [:presentation "content"]))
                      (:children group)))
      (expect (zero? (get-in projection [:omitted :rows])))
      (expect (> (activity/byte-size projection) 65536))
      (expect (contract/valid-projection? projection)))))

(defdescribe
  activity-pressure-fallback-test
  (it "retains grouped invocations independently of their aggregate size"
      (let [ctx
            (event/context)

            pairs
            (mapv (fn [n]
                    (event-pair ctx
                                (if (zero? n) :shell :_shell_logs)
                                :succeeded
                                {"id" "large-build"}
                                {:presenter :shell
                                 :classification :mutation
                                 :args (if (zero? n) ["npm test"] ["large-build"])}))
                  (range 12))

            running
            (first (event-pair ctx :grep :succeeded nil))

            state
            (activity/replay (concat (mapcat identity pairs) [running]))

            projection
            (activity/presentation state)]

        (expect (= 12 (count (:children (first (:rows projection))))))
        (expect (= (:invocation-id running) (:id (last (:rows projection)))))
        (expect (zero? (get-in projection [:omitted :rows])))
        (expect (= {:running 1 :succeeded 12 :failed 0 :cancelled 0} (:counts projection)))
        (expect (contract/valid-projection? projection))))
  (it "retains past and current content without trimming either"
      (let [ctx
            (event/context)

            blocks
            (vec (repeat 4 {"type" "text" "text" (apply str (repeat 6000 "x"))}))

            pairs
            (mapv (fn [_]
                    (event-pair ctx :grep :succeeded nil))
                  (range 4))

            state
            (update (activity/replay (concat (mapcat identity (butlast pairs))
                                             [(first (last pairs))]))
                    :rows
                    #(mapv (fn [row]
                             (assoc row
                               :presentation
                               {"headline" "Search results" "summary" "Matches" "content" blocks}))
                           %))

            projection
            (activity/presentation state)]

        (expect (= 4 (count (:rows projection))))
        (expect (zero? (get-in projection [:omitted :rows])))
        (expect (not-any? :is-truncated (:rows projection)))
        (expect (every? #(= blocks (get-in % [:presentation "content"])) (:rows projection)))
        (expect (every? #(= "Matches" (get-in % [:presentation "summary"])) (:rows projection)))
        (expect (> (activity/byte-size projection) 65536))
        (expect (contract/valid-projection? projection)))))

(defdescribe
  rich-content-test
  (it
    "replaces invocation content without changing lifecycle or counts"
    (let [ctx
          (event/context)

          invocation
          (event/invocation ctx nil)

          details
          {:operation :report :presenter :generic}

          start
          (event/start-event ctx invocation details)

          blocks
          [{"type" "heading" "text" "Checks"}
           {"type" "table" "columns" ["Test" "Result"] "rows" [["unit" "passed"]]}
           {"type" "progress" "label" "Checking" "value" 1 "total" 2}]

          update
          (event/content-event ctx
                               invocation
                               details
                               {"headline" "Checks" "summary" "1 of 2" "content" blocks})

          collector
          (event/collector)

          state
          (activity/replay [start update])

          end
          (event/terminal-event ctx
                                invocation
                                (assoc details
                                  :started-at-ms 0
                                  :outcome :succeeded))

          projection
          (activity/presentation (activity/reduce-event state end))]

      (expect (= :running (:state state)))
      (expect (= 1 (get-in state [:counts :running])))
      (expect (= blocks (get-in projection [:rows 0 :presentation "content"])))
      (expect (contract/valid-projection? projection))
      (expect
        (try (event/accept! collector update) false (catch clojure.lang.ExceptionInfo _ true)))
      (event/accept! collector start)
      (event/accept! collector update)
      (event/accept! collector end)
      (expect
        (try (event/accept! collector update) false (catch clojure.lang.ExceptionInfo _ true)))))
  (it
    "lets a terminal presentation replace live content for the same invocation"
    (doseq [outcome [:succeeded :failed :cancelled]]
      (let [ctx (event/context)
            invocation (event/invocation ctx nil)
            details {:operation :check :presenter :tests}
            progress {"headline" "Running checks"
                      "summary" "In progress"
                      "content" [{"type" "progress" "label" "Checking" "value" 1 "total" 2}]}
            final {"headline" "Checks finished"
                   "summary" (name outcome)
                   "content" [{"type" "text" "text" "Final check evidence"}]}
            independent (event/start-event ctx (event/invocation ctx nil) details)
            running (activity/replay [(event/start-event ctx invocation details)
                                      (event/content-event ctx invocation details progress)
                                      independent])
            terminal (assoc (event/terminal-event ctx
                                                  invocation
                                                  (assoc details
                                                    :started-at-ms 0
                                                    :outcome outcome
                                                    :result "Final result"
                                                    :error (ex-info "Final error" {})))
                       :presentation final)
            settled (activity/presentation (activity/reduce-event running (event/checked terminal)))
            row (first (:rows settled))]

        (expect (= progress (get-in running [:rows 0 :presentation])))
        (expect (= (:invocation-id invocation) (:id row)))
        (expect (= final (:presentation row)))
        (expect (= (name outcome) (:state row)))
        (expect (= 2 (count (:rows settled))))
        (expect (= "running" (get-in settled [:rows 1 :state])))
        (expect (= 1 (get-in settled [:counts :running])))
        (expect (= 1 (get-in settled [:counts outcome])))
        (expect (contract/valid-projection? settled)))))
  (it "rejects unknown content and invalid progress without fabricating success"
      (doseq [blocks [[{"type" "html" "text" "<script>"}]
                      [{"type" "progress" "label" "Checking" "value" 3 "total" 2}]]]
        (expect (try (event/content-event (event/context)
                                          (event/invocation (event/context) nil)
                                          {:operation :report :presenter :generic}
                                          {"headline" "Checks" "summary" "" "content" blocks})
                     false
                     (catch clojure.lang.ExceptionInfo _ true))))))

(defdescribe
  structured-presentation-test
  (it
    "replaces headline, summary, content and sections atomically, never lifecycle"
    (let [ctx
          (event/context)

          invocation
          (event/invocation ctx nil)

          details
          {:operation :ls :presenter :observation}

          view
          {"headline" "Listed 2 directories"
           "summary" "3 directories · 2 files"
           "content" []
           "sections" [{"headline" "src"
                        "summary" "3 directories · 0 files"
                        "content" [{"type" "text" "text" "Source tree"}]}
                       {"headline" "test" "summary" "0 directories · 2 files" "content" []}]}

          start
          (event/start-event ctx invocation details)

          update
          (event/content-event ctx invocation details view)

          state
          (activity/replay [start update])

          projection
          (activity/presentation state)]

      (expect (= "running" (:state projection)))
      (expect (= view (get-in projection [:rows 0 :presentation])))
      (expect (contract/valid-projection? projection))
      (expect (= {:running 1 :succeeded 0 :failed 0 :cancelled 0} (:counts projection))))))
