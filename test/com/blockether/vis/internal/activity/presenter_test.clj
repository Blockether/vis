(ns com.blockether.vis.internal.activity.presenter-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.activity.core :as activity]
            [com.blockether.vis.internal.activity.event :as event]
            [com.blockether.vis.internal.activity.presenter :as presenter]
            [com.blockether.vis.contract.activity :as contract]
            [com.blockether.vis.contract.wire :as wire]
            [charred.api :as json]
            [lazytest.core :refer [defdescribe expect it]]))

(defn result-fixture
  "Production-generated result views shared by Companion stories and TUI grid tests."
  [& [override-cases]]
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
       "# Activity\n\nOpen an operation to read its **retained result**.\n\n- Read opens the file.\n- Patch shows the applied changes."
       nil]
      [:council.publish "Activity review"
       {:title "Activity review"
        :entry_id 42
        :kind "informational"
        :thread_id 42
        :ping ["reviewer"]
        :content "Read links to the file; Patch shows its **changes** after one disclosure."} nil]
      [:council.get "Activity review"
       {:title "Activity review"
        :entry_id 42
        :kind "informational"
        :thread_id 42
        :ping ["reviewer"]
        :content "Read links to the file; Patch shows its **changes** after one disclosure."} nil]]

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
                      :activity (presenter/for-tool operation)
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
             (or override-cases cases))]

    (-> (activity/replay events)
        (assoc :state :succeeded)
        activity/presentation
        (update :rows
                #(mapv (fn [i row]
                         (-> row
                             (assoc :id (str "result-" i)
                                    :duration-ms 0)
                             (dissoc :argument-key :read-key)))
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
  summary-only-read-test
  (it "retains the file and range without copying arguments or file contents into Activity"
      (doseq [[text summary] [["12:abc│ private file content\n13:def│ more file content"
                               "src/example.clj · lines 12–13"] ["" "src/example.clj"]]]
        (let [row (get-in (result-fixture [[:cat "src/example.clj" text nil]]) ["rows" 0])]
          (expect (= {"headline" "Read" "summary" summary "content" []} (get row "presentation")))
          (expect (empty? (get row "evidence")))
          (expect (not (contains? row "result_summary")))
          (expect (not (str/includes? (pr-str row) "file content"))))))
  (it "declares end-only reads while preserving lifecycle, failures and cancellation"
      (let [declared (presenter/for-tool :cat)]
        (expect (true? (:summary-only declared)))
        (expect (false? (:show-start declared)))
        (doseq [outcome [:failed :cancelled]]
          (let [ctx (event/context)
                invocation (event/invocation ctx nil)
                details {:operation :cat
                         :presenter :generic
                         :activity declared
                         :label "src/example.clj"
                         :args ["src/example.clj" 12 13]
                         :classification :observation}
                started (event/start-event ctx invocation details)
                terminal (event/terminal-event ctx
                                               invocation
                                               (assoc details
                                                 :outcome outcome
                                                 :error (ex-info "Read refused" {})
                                                 :started-at-ms (System/currentTimeMillis)))
                row (first (:rows (activity/presentation (activity/replay [started terminal]))))]

            (expect (= :running (:status started)))
            (expect (false? (:show-start started)))
            (expect (not (contains? started :argument-summary)))
            (expect (= outcome (:status terminal)))
            (expect (= "Read refused" (:error-summary terminal)))
            (expect (= (name outcome) (:state row)))
            (expect (some #(= "error" (:kind %)) (:evidence row))))))))

(defdescribe
  compact-built-in-results-test
  (it
    "keeps routine draft and session outcomes on the summary line"
    (doseq [[operation result summary]
            [[:draft_status {:in_draft false} "No active draft"]
             [:draft_status
              {:in_draft true :branch "vis/review" :target_branch "main" :pending 2 :ahead 1}
              "vis/review → main · 2 pending files · 1 commit ahead"]
             [:draft_status
              {"in_draft" true "branch" "vis/review" "target_branch" "main" "pending" 0 "ahead" 0}
              "vis/review → main · No pending changes"]
             ;; #246: recovery must not look like a clean or deleted draft.
             [:draft_status
              {:in_draft true :recovery_required true :recovery_hint "Use draft_discard()."}
              "Draft recovery required · Use draft_discard()."]
             [:draft_create {:branch "vis/review" :target_branch "main" :clean true}
              "vis/review → main · Clean snapshot"]
             [:draft_create {:branch "vis/review" :target_branch "main" :clean false}
              "vis/review → main · Includes pending changes"]
             [:draft_approve {:status "approved" :published false :target_branch "main" :files []}
              "Approved locally on main"]
             [:draft_approve
              {:status "nothing-to-approve" :published true :target_branch "main" :files []}
              "Nothing to approve · Published to main"]
             [:draft_diff {:filename "DIFF-review.json" :empty true :checkpoint "internal"}
              "DIFF-review.json · No changes"]
             [:draft_diff {:filename "DIFF-review.json" :empty false :checkpoint "internal"}
              "DIFF-review.json · Diff attached"]
             [:draft_diff {:filename "DIFF-group.json" :empty false :repository_count 2}
              "DIFF-group.json · 2 repositories · Diff attached"]
             [:draft_sync {:status "synced" :repositories [{:status "synced"} {:status "synced"}]}
              "Synchronized · 2 repositories"]
             [:draft_sync {:status "conflicts" :repositories [{:conflicts ["a.txt" "b.txt"]}]}
              "Resolve conflicts · 1 repository · 2 conflict paths"]
             [:draft_sync {:status "aborted" :repositories [{:status "aborted"}]}
              "Synchronization aborted · 1 repository"]
             [:draft_discard {:label "review" :root "~/vis" :approved_ahead 1}
              "review · Returned to ~/vis · 1 approved commit kept"]
             [:draft_discard
              {:status "recovered" :label "review" :root "~/vis" :preserved_root "~/draft"}
              "review · Returned to ~/vis · Preserved ~/draft"]
             [:update_goal {:status "complete" :objective "Verify Activity" :reason "Checks pass"}
              "Complete · Verify Activity"]]]
      (let [view (get-in (result-fixture [[operation "" result nil]]) ["rows" 0 "presentation"])]
        (expect (contract/valid-presentation? view))
        (expect (= summary (get view "summary")))
        (expect (not (re-find #"\"type\" \"table\"" (pr-str view))))
        (when-not (contains? #{:update_goal :draft_sync} operation)
          (expect (empty? (get view "content")))))))
  (it "names absent results instead of showing empty summaries or tables"
      (doseq [[operation summary]
              [[:draft_status "No draft status"] [:draft_create "No draft result"]
               [:draft_approve "No draft result"] [:draft_discard "No draft result"]
               [:draft_diff "No draft result"] [:draft_sync "No draft result"]
               [:list_sessions "No sessions found"]]

              result
              [nil {} []]]

        (let [view (presenter/result-presentation {:operation operation} result)]
          (expect (= summary (get view "summary")))
          (expect (empty? (get view "content"))))))
  (it
    "retains actionable evidence without metadata tables"
    (doseq [[operation result summary evidence]
            [[:draft_approve
              {:status "approved" :published true :target_branch "main" :files ["src/example.clj"]}
              "Published to main · 1 file" ["src/example.clj"]]
             [:update_goal
              {:status "blocked" :objective "Verify Activity" :reason "Missing test dependency"}
              "Blocked · Verify Activity" ["Missing test dependency"]]]]
      (let [view (presenter/result-presentation {:operation operation} result)
            content (pr-str (get view "content"))]

        (expect (contract/valid-presentation? view))
        (expect (= summary (get view "summary")))
        (doseq [text evidence]
          (expect (str/includes? content text)))
        (expect (not (str/includes? content "\"table\""))))))
  (it
    "keeps binding registration, start policy, failure and cancellation intact"
    (doseq [[operation show-start?]
            [[:draft_status false] [:draft_create true] [:draft_diff true] [:draft_approve true]
             [:draft_discard true] [:update_goal false]]

            outcome
            [:failed :cancelled]]

      (let [ctx
            (event/context)

            invocation
            (event/invocation ctx nil)

            declared
            (presenter/for-tool operation)

            details
            {:operation operation
             :presenter :generic
             :activity declared
             :started-at-ms (System/currentTimeMillis)}

            start
            (event/start-event ctx invocation details)

            terminal
            (event/terminal-event ctx
                                  invocation
                                  (assoc details
                                    :outcome outcome
                                    :error (ex-info "Operation unavailable" {})))

            projection
            (activity/presentation (activity/replay [start terminal]))

            row
            (first (:rows projection))]

        (expect (= show-start? (:show-start declared)))
        (expect (= (name outcome) (:state row)))
        (expect (= "Operation unavailable" (:error-summary row)))
        (expect (contract/valid-projection? projection))))))

(defdescribe
  semantic-results-test
  (it "hides transport identifiers and redundant flags recursively"
      (let [view
            (presenter/result-presentation {:operation :council.publish}
                                           {:title "Review"
                                            :thread_id 258
                                            :entry_id "secret-handle"
                                            :content "Useful result"
                                            :nested {:session_id "uuid" :name "Reviewer"}})

            body
            (pr-str (get view "content"))]

        (expect (not (re-find #"258|secret-handle|uuid|Review\"" body)))
        (expect (re-find #"Useful result" body))))
  (it "shows command, preserved output and actual exit status for shell handles"
      (let [view
            (presenter/result-presentation {:operation :_shell-wait :label "internal"}
                                           {:command "printf 'a\nb' && false"
                                            :out "a\nb"
                                            :exit 1
                                            :id "handle-123"
                                            :log_path "/private/log"
                                            :status "exited"})

            blocks
            (get view "content")]

        (expect (= "Command finished" (get view "headline")))
        (expect (some #(= {"type" "code" "language" "bash" "text" "printf 'a\nb' && false"} %)
                      blocks))
        (expect (some #(= "a\nb" (get % "text")) blocks))
        (expect (some #(= {"type" "markdown" "text" "**Exit code:** 1"} %) blocks))
        (expect (not (re-find #"handle-123|/private/log|internal" (pr-str view))))))
  (it "never invents a successful exit for a running command"
      (let [view (presenter/result-presentation {:operation :shell}
                                                {:command "sleep 10" :exit nil :status "running"})]
        (expect (= {"type" "text" "text" "Running"} (last (get view "content"))))
        (expect (not (re-find #"Exit code: 0" (pr-str view))))))
  (it "labels an unavailable exit beside its bold label"
      (let [view (presenter/result-presentation {:operation :_shell-wait}
                                                {:command "sleep 10" :exit nil :status "exited"})]
        (expect (= {"type" "markdown" "text" "**Exit code:** unavailable"}
                   (last (get view "content"))))))
  (it "does not turn a publish receipt identifier into expandable content"
      (expect
        (= [] (get (presenter/result-presentation {:operation :council.publish} 279) "content"))))
  (it "retains useful titles within member lists"
      (expect (re-find #"Reviewer"
                       (pr-str (presenter/result-presentation {:operation :council.members}
                                                              [{:session_id "opaque"
                                                                :title "Reviewer"}]))))))

(defdescribe
  council-activity-test
  (it "shows only the Council publish label and complete message body"
      (let [body
            (apply str (repeat 200 "Full **message**.\n"))

            entry
            {:title "Internal title"
             :content body
             :entry_id 42
             :thread_id 42
             :group_id "internal-group"
             :author_session_id "internal-author"
             :created_at 1789503371213
             :kind "coordination"
             :source "host"
             :source_ref {:session_id "internal-author"}
             :ping ["internal-recipient"]
             :reply_required true
             :replies [{:session_id "internal-recipient" :state "pending"}]}]

        (doseq [value [entry (wire/->wire entry)]]
          (let [view (presenter/result-presentation {:operation :council.publish} value)]
            (expect (= {"headline" "Published Council message"
                        "summary" ""
                        "content" [{"type" "markdown" "text" body}]}
                       view))
            (expect (contract/valid-projection? (result-fixture [[:council.publish "" value
                                                                  nil]])))))))
  (it "names an absent message body without exposing receipt metadata"
      (doseq [[operation headline]
              [[:council.publish "Published Council message"] [:council.get "Read Council message"]]

              value
              [nil {} {:content ""} {"content" ""} 279]]

        (expect (= {"headline" headline "summary" "No message content" "content" []}
                   (presenter/result-presentation {:operation operation} value)))))
  (it "shows only the Council read label and message body"
      (let [view (presenter/result-presentation
                   {:operation :council.get}
                   {:title "Review"
                    :content "Tests **passed**."
                    :entry_id 42
                    :thread_id 42
                    :group_id "internal-group"
                    :created_at 1789470180407
                    :kind "coordination"
                    :source "host"
                    :source_ref {:session_id "internal-author"}
                    :ping ["internal-recipient"]
                    :reply_required true
                    :replies [{:session_id "internal-recipient" :state "pending"}
                              {:session_id "internal-other" :state "replied" :reply_entry_id 43}]})]
        (expect (= {"headline" "Read Council message"
                    "summary" ""
                    "content" [{"type" "markdown" "text" "Tests **passed**."}]}
                   view))))
  (it "summarizes pages and keeps each read message behind its own disclosure"
      (let [value
            {:entries [{:title "Review"
                        :content "Full **message**."
                        :created_at 1
                        :entry_id 42
                        :kind "coordination"}]
             :after 42
             :has_more true}

            view
            (presenter/result-presentation {:operation :council.read} value)]

        (expect (= "1 message · more available" (get view "summary")))
        (expect (empty? (get view "content")))
        (expect (= [{"headline" "Review"
                     "summary" ""
                     "content" [{"type" "markdown" "text" "Full **message**."}]}]
                   (get view "sections")))))
  (it "uses one compact table for member and thread lists"
      (doseq [[operation value summary columns rows]
              [[:council.members
                [{:session_id "internal-member" :title "Reviewer" :state "running"}] "1 member"
                ["Member" "State"] [["Reviewer" "Running"]]]
               [:council.threads
                {:entries [{:thread_id 42 :title "Review" :created_at 1 :kind "coordination"}]
                 :after 42
                 :has_more false} "1 thread" ["Thread"] [["Review"]]]]]
        (let [view (presenter/result-presentation {:operation operation} value)]
          (expect (= summary (get view "summary")))
          (expect (= [{"type" "table" "columns" columns "rows" rows}] (get view "content"))))))
  (it "names empty Council results instead of exposing empty envelopes"
      (doseq [[operation value summary]
              [[:council.members [] "No active members"]
               [:council.threads {:entries [] :after 0 :has_more false} "No threads"]
               [:council.read {:entries [] :after 0 :has_more false} "No messages"]]]
        (let [view (presenter/result-presentation {:operation operation} value)]
          (expect (= summary (get view "summary")))
          (expect (empty? (get view "content")))
          (expect (empty? (get view "sections"))))))
  (it "retains full message bodies without reply metadata with wire keys"
      (let [body
            (apply str (repeat 200 "Full **message**.\n"))

            value
            {"content" body
             "reply_to" 42
             "replies" [{"state" "delivered"} {"state" "interrupted"} {"state" "unavailable"}]}

            view
            (presenter/result-presentation {:operation :council.get} value)]

        (expect (= "" (get view "summary")))
        (expect (= [{"type" "markdown" "text" body}] (get view "content")))
        (expect (contract/valid-projection? (result-fixture [[:council.get "" value nil]])))))
  (it "keeps local messages and lookups end-only"
      (expect (= "Publish Council message" (:headline (presenter/for-tool :council.publish))))
      (expect (= "Read Council message" (:headline (presenter/for-tool :council.get))))
      (doseq [operation [:council.publish :council.get :council.read :council.threads
                         :council.members]]
        (expect (false? (:show-start (presenter/for-tool operation)))))))

(defdescribe
  managed-agent-presentation-test
  (it "keeps cancellation counts without exposing recipient identifiers"
      (let [view (presenter/result-presentation {:operation :council.cancel}
                                                {:session_id "internal-child"
                                                 :status "cancelled"
                                                 :cancelled ["internal-child"
                                                             "internal-grandchild"]})]
        (expect (= "Cancelled · 2 subagents" (get view "summary")))
        (expect (empty? (get view "content")))
        (expect (not (re-find #"internal-" (pr-str view))))))
  (it "bounds Unicode summaries without dropping the delegated task"
      (let [task
            (apply str (repeat 160 "😀"))

            agent
            {:task task
             :status "running"
             :model "fixture-model-with-a-long-name"
             :iteration_budget 12
             :iterations_used 2
             :pending_input true}

            projection
            (result-fixture [[:council.publish_spawn "" agent nil]
                             [:council.subagents "" [agent] nil]])

            views
            (mapv #(get % "presentation") (get projection "rows"))]

        (expect (contract/valid-projection? projection))
        (expect (= task (get-in views [0 "content" 0 "text"])))
        (expect (= task (get-in views [1 "sections" 0 "content" 0 "text"])))
        (expect (re-find #"Input pending" (get-in views [1 "sections" 0 "summary"])))))
  (it "shows meaningful empty and populated team counts"
      (doseq [[value expected] [[[] "No subagents"]
                                [[{:session_id "child" :status "queued"}] "1 subagent"]]]
        (expect (= expected
                   (get (presenter/result-presentation {:operation :council.subagents} value)
                        "summary")))))
  (it "keeps all agent result views valid and retains lifecycle evidence"
      (let [projection
            (result-fixture
              [[:council.publish_spawn ""
                {:session_id "child" :task "Verify" :status "queued" :iteration_budget 2} nil]
               [:council.subagents ""
                [{:session_id "child"
                  :task "Verify"
                  :status "running"
                  :iterations_used 1
                  :iteration_budget 2}] nil]
               [:council.cancel "" {:session_id "child" :status "cancelled"} nil]
               [:council.route ""
                {:session_id "child" :provider "fixture" :model "small" :effective "next_request"}
                nil]])]
        (expect (contract/valid-projection? projection))
        (let [views (mapv #(get % "presentation") (get projection "rows"))]
          (expect (= ["Verify · Queued · Up to 2 iterations" "1 subagent" "Cancelled"
                      "fixture/small · next request"]
                     (mapv #(get % "summary") views)))
          (expect (= "Running · Iterations: 1/2" (get-in views [1 "sections" 0 "summary"])))
          (expect (= [{"type" "markdown" "text" "Verify"}]
                     (get-in views [1 "sections" 0 "content"])))
          (expect (every? empty? (map #(get-in views [% "content"]) [2 3])))
          (expect (not (re-find #"child|next_request|Session|Parent" (pr-str views)))))))
  (it "shows only spawn as running work"
      (doseq [[operation visible?] [[:council.publish_spawn true] [:council.subagents false]
                                    [:council.cancel false] [:council.route false]]]
        (expect (= visible? (:show-start (presenter/for-tool operation))))))
  (it
    "preserves failures and cancellation across every Council binding"
    (doseq [operation
            [:council.publish :council.get :council.read :council.threads :council.members
             :council.publish_spawn :council.subagents :council.cancel :council.route]

            outcome
            [:failed :cancelled]]

      (let [ctx
            (event/context)

            invocation
            (event/invocation ctx nil)

            details
            {:operation operation
             :presenter :generic
             :activity (presenter/for-tool operation)
             :started-at-ms (System/currentTimeMillis)}

            start
            (event/start-event ctx invocation details)

            terminal
            (event/terminal-event ctx
                                  invocation
                                  (assoc details
                                    :outcome outcome
                                    :error (ex-info "Agent request refused" {})))

            projection
            (activity/presentation (activity/replay [start terminal]))

            row
            (first (:rows projection))]

        (expect (= (name outcome) (:state row)))
        (expect (= "Agent request refused" (:error-summary row)))
        (expect (contract/valid-projection? projection))))))

(defn read-session-result
  "Representative multi-turn read with complete, repeated diagnostic evidence."
  []
  (let [request
        (str "Review the long request.\n"
             (apply str (repeat 30 "Keep every requirement.\n"))
             "Final request requirement.")

        failure
        {"turn_id" "turn-2"
         "iteration_id" "iteration-2"
         "iteration" 1
         "source" "code"
         "tool" "python_execution"
         "classification" "python-error"
         "user_request" request
         "code" "raise ValueError(\"Fixture failure\")"
         "message" (str "Fixture failure\n"
                        (apply str (repeat 20 "Traceback detail\n"))
                        "Final failure detail.")
         "advice" "Check the failing input before retrying."
         "auth_token" "fixture-credential"}]

    {"session"
     {"title" "Session review"
      "model" "example-model"
      "turn_count" 3
      "goal" "Verify a complete session read."
      "turns"
      [{"id" "turn-1"
        "outcome" "completed"
        "iteration_count" 2
        "user_request" "Inspect the session."
        "answer" "Inspection complete."}
       {"id" "turn-2" "outcome" "failed" "iteration_count" 1 "user_request" request}
       {"id" "turn-3" "outcome" "running" "iteration_count" 1 "user_request" "Check the fix."}]}
     "current_turn" {"id" "turn-3"
                     "user_request" "Check the fix."
                     "status" "running"
                     "elapsed_ms" 2300
                     "iteration" {"current" 2}
                     "failures" []
                     "cost" {"input_tokens" 1200
                             "input_cache_read_tokens" 700
                             "input_cache_write_tokens" 100
                             "input_regular_tokens" 400
                             "output_tokens" 90
                             "output_reasoning_tokens" 12
                             "total_cost" 0.025}}
     "usage" {"totals" {"tokens" {"input" 12345
                                  "cached" 6700
                                  "cache_created" 100
                                  "uncached" 5545
                                  "output" 890
                                  "reasoning" 120}
                        "cost_usd" 0.125
                        "turns" 3
                        "iterations" 4}}
     "failures" [failure]
     "diagnosis" {"failure_count" 1
                  "by_classification" {"python-error" 1}
                  "repetition_loop" false
                  "failures" [failure]
                  "next_actions" ["Check the failing input before retrying."]}
     "transcript" {"turns" [{"complete" "Raw transcript remains available."}]}}))

(defn read-session-fixture
  "Production-generated Read session presentation for both clients."
  []
  (result-fixture [[:read_session "" (read-session-result) nil]]))

(defdescribe
  read-session-presentation-test
  ;; Regression #230: keep the overview compact without discarding full evidence.
  (it
    "groups turns and discloses complete requests and each failure only once"
    (let [value
          (read-session-result)

          view
          (presenter/result-presentation {:operation :read_session} value)

          content
          (get view "content")

          sections
          (get view "sections")

          turn-table
          (first (filter #(= ["Turn" "Outcome" "Request"] (get % "columns")) content))

          all-text
          (pr-str view)]

      (expect (= "Read session" (get view "headline")))
      (expect (= ["Current turn" "Usage" "Diagnosis" "Turns"]
                 (mapv #(get % "text") (filter #(= "heading" (get % "type")) content))))
      (expect (= ["Turn 1" "Turn 2" "Turn 3"] (mapv first (get turn-table "rows"))))
      (expect (every? #(<= (count (last %)) 49) (get turn-table "rows")))
      (expect (= "Review the long request.…" (get-in turn-table ["rows" 1 2])))
      (expect (= ["Session details" "Turn details" "Failure details"]
                 (mapv #(get % "headline") sections)))
      (expect (not (.contains (pr-str content) "Final request requirement.")))
      (expect (not (.contains (pr-str content) "Final failure detail.")))
      (expect (= 1 (count (re-seq #"Final request requirement\." all-text))))
      (expect (= 1 (count (re-seq #"Final failure detail\." all-text))))
      (expect (.contains all-text "12345"))
      (expect (.contains all-text "6700"))
      (expect (.contains all-text "1200"))
      (expect (contract/valid-presentation? view))))
  (it "bounds the recent-turn list while retaining every complete request"
      (let [turns
            (mapv (fn [n]
                    {"id" (str n)
                     "outcome" "completed"
                     "user_request" (str "Request " n " ends here.")})
                  (range 1 21))

            value
            (assoc (read-session-result)
              "session" {"title" "Many turns" "turns" turns}
              "current_turn" nil
              "failures" []
              "diagnosis" {})

            view
            (presenter/result-presentation {:operation :read_session} value)

            rows
            (mapcat #(get % "rows") (get view "content"))]

        (expect (= ["Turn 15" "Turn 16" "Turn 17" "Turn 18" "Turn 19" "Turn 20"] (mapv first rows)))
        (expect (.contains (pr-str (get view "content")) "6 of 20"))
        (expect (.contains (pr-str (get view "sections")) "Request 1 ends here."))
        (expect (not-any? #(= "Failure details" (get % "headline")) (get view "sections")))))
  (it "states empty and unavailable data without inventing zero usage"
      (let [view (presenter/result-presentation {:operation :read_session} {})]
        (expect (= "No session data" (get view "summary")))
        (expect (empty? (get view "sections")))
        (expect (contract/valid-presentation? view))))
  (it "matches the shared wire fixture with numeric usage and redacted credentials"
      (let [actual
            (read-session-fixture)

            expected
            (json/read-json (slurp (io/resource
                                     "vis-contract/fixtures/activity-read-session.json")))

            view
            (get-in actual ["rows" 0 "presentation"])]

        (expect (= expected actual))
        (expect (contract/valid-projection? actual))
        (expect (not (.contains (pr-str actual) "fixture-credential")))
        (expect (.contains (pr-str actual) "[REDACTED]"))
        (expect (.contains (pr-str (get view "content")) "12345"))
        (expect (.contains (pr-str (get view "sections")) "1200"))))
  (it "bounds Unicode previews without splitting characters or discarding their complete request"
      (let [request
            (apply str (repeat 100 "😀"))

            view
            (presenter/result-presentation
              {:operation :read_session}
              {"session" {"title" request "turns" [{"id" "unicode" "user_request" request}]}})

            rows
            (mapcat #(get % "rows") (get view "content"))]

        (expect (= (str (apply str (repeat 48 "😀")) "…") (last (first rows))))
        (expect (.contains (pr-str (get view "sections")) request))
        (expect (contract/valid-presentation? view))))
  (it "uses canonical failure records rather than copies nested in diagnosis or current turn"
      (let [value
            (read-session-result)

            failure
            (first (get value "failures"))

            view
            (presenter/result-presentation {:operation :read_session}
                                           (-> value
                                               (assoc "failures" [failure failure])
                                               (assoc-in ["current_turn" "failures"] [failure])))]

        (expect (= 1 (count (re-seq #"Final failure detail\." (pr-str view)))))
        (expect (.contains (get view "summary") "1 failure"))))
  (it "retains the failure request when the corresponding turn snapshot is unavailable"
      (let [failure
            (first (get (read-session-result) "failures"))

            view
            (presenter/result-presentation {:operation :read_session} {"failures" [failure]})]

        (expect (= 1 (count (re-seq #"Final request requirement\." (pr-str view)))))
        (expect (= 1 (count (re-seq #"Final failure detail\." (pr-str view)))))))
  (it "keeps the declared running state and genuine failure or cancellation evidence"
      (doseq [outcome [:failed :cancelled]]
        (let [ctx (event/context)
              invocation (event/invocation ctx nil)
              details {:operation :read_session
                       :presenter :generic
                       :activity (presenter/for-tool :read_session)
                       :started-at-ms (System/currentTimeMillis)}
              start (event/start-event ctx invocation details)
              running (activity/presentation (activity/replay [start]))
              terminal (event/terminal-event ctx
                                             invocation
                                             (assoc details
                                               :outcome outcome
                                               :error (ex-info "Unable to read session" {})))
              settled (activity/presentation (activity/replay [start terminal]))]

          (expect (= "Read session" (get-in running [:rows 0 :presentation "headline"])))
          (expect (= "running" (get-in running [:rows 0 :state])))
          (expect (= (name outcome) (get-in settled [:rows 0 :state])))
          (expect (= "Unable to read session" (get-in settled [:rows 0 :error-summary])))
          (expect (contract/valid-projection? settled)))))
  (it "distinguishes uncached input from regular input excluding cache writes"
      (let [view
            (presenter/result-presentation {:operation :read_session} (read-session-result))

            rows
            (mapcat #(get % "rows") (get (first (get view "sections")) "content"))

            regular
            (first (filter #(= "Regular input tokens" (first %)) rows))]

        (expect (.contains (pr-str (get view "content")) "uncached input 5545"))
        (expect (= ["Regular input tokens" "5445" "400"] regular)))))
