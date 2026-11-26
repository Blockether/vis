(ns com.blockether.vis.internal.task-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [com.blockether.vis.internal.task :as task]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe task-lifetime-test
  (it "keeps success after a cancellation request if the code returns a value"
      (let [sid (random-uuid)
            ready (promise)
            finish (promise)
            row (task/start! sid {:name "watch" :origin {:turn 1 :iteration 2 :block "first"}}
                             (fn [] (task/install-interrupt! #(deliver ready %))
                               @finish
                               {:state :done :value 42}))
            id (:id row)]
        (try
          (expect (document/valid? "task" (wire/->wire row)))
          (expect (document/valid-json? "task" nil
                    (json/read-json (slurp (io/resource "vis-contract/fixtures/task.json")))))
          (expect (true? (task/cancel! sid id :agent)))
          (expect (= :cancel (deref ready 5000 nil)))
          (expect (= :running (:state (task/state sid id))))
          (deliver finish true)
          (expect (= {:state :done :value 42 :cancelling 1} (task/await! sid id {:timeout-ms 5000})))
          (expect (= [id] (mapv :id (task/tasks sid))))
          (expect (document/valid? "task" (wire/->wire (task/state sid id))))
          (task/acknowledge! sid id)
          (expect (empty? (task/tasks sid)))
          (expect (= {:state :done :value 42 :cancelling 1} (task/await! sid id {})))
          (finally (deliver finish true) (task/sweep! sid :session-close)))))
  (it "detaches existing waits without stopping the Task or detaching later waits"
      (let [sid (random-uuid)
            finish (promise)
            id (:id (task/start! sid {:name "watch"} #(do @finish {:state :done :value 42})))]
        (try
          (expect (= {:state :running} (task/await! sid id {:block "one" :token 1})))
          (expect (= [id] (task/awaited-ids sid "one")))
          (task/detach-awaits! sid {:question "Ready?"})
          (expect (= ["Ready?"] (mapv #(get-in % [:reason :question])
                                      (task/take-detach-notices! sid "one"))))
          (expect (empty? (task/take-detach-notices! sid "one")))
          (expect (= {:state :detached} (task/await! sid id {:block "one" :token 1})))
          (expect (empty? (task/awaited-ids sid "one")))
          (expect (= {:state :running} (task/await! sid id {:block "two" :token 2})))
          (deliver finish true)
          (expect (= {:state :done :value 42} (task/await! sid id {:timeout-ms 5000})))
          (expect (= [id] (mapv :id (task/take-ended-notices! sid))))
          (expect (empty? (task/take-ended-notices! sid)))
          (finally (deliver finish true) (task/sweep! sid :session-close)))))
  (it "waits for the end of a Task after a cancel request instead of detaching it again"
      (let [sid (random-uuid)
            gate (promise)
            {:keys [row complete!]} (task/start-manual! sid
                                                        {:kind :subagent :name "child"
                                                         :detach-reason (constantly {:question "Ready?"})}
                                                        (fn [_] @gate))
            id (:id row)]
        (try
          (expect (= {:state :detached} (task/await! sid id {:block "one" :token 1})))
          (expect (true? (task/cancel! sid id :agent)))
          (expect (= {:state :running} (task/await! sid id {:block "two" :token 2 :timeout-ms 20})))
          (deliver gate true)
          (complete! (constantly {:state :cancelled}))
          (expect (= :cancelled (:state (task/await! sid id {:block "one" :token 1}))))
          (expect (= :cancelled (:state (task/await! sid id {:block "two" :token 2}))))
          (finally (deliver gate true) (task/sweep! sid :session-close)))))
  (it "keeps a wait on a Task with a cancel request when another child asks"
      (let [sid (random-uuid)
            gate (promise)
            {:keys [row complete!]} (task/start-manual! sid
                                                        {:kind :subagent :name "child"
                                                         :detach-reason (constantly nil)}
                                                        (fn [_] @gate))
            id (:id row)]
        (try
          (expect (= {:state :running} (task/await! sid id {:block "one" :token 1})))
          (expect (true? (task/cancel! sid id :agent)))
          (expect (empty? (task/detach-awaits! sid {:question "Other child asks"})))
          (expect (nil? (task/take-detach-notices! sid "one")))
          (deliver gate true)
          (complete! (constantly {:state :cancelled}))
          (expect (= :cancelled (:state (task/await! sid id {:block "one" :token 1}))))
          (finally (deliver gate true) (task/sweep! sid :session-close)))))
  (it "does not detach a tool that disables detachment, but releases its view on sweep"
      (let [sid (random-uuid)
            finish (promise)
            ended (promise)
            id (:id (task/start! sid {:name "watch" :detachable? false}
                                 #(do @finish {:state :done})))
            owner [(str sid) id]]
        (try
          (expect (true? (task/track-view! owner "view" #(deliver ended %))))
          (task/await! sid id {:block "one" :token 1})
          (expect (empty? (task/detach-awaits! sid {:question "Ready?"})))
          (expect (= {:state :running} (task/await! sid id {:block "one" :token 1})))
          (expect (= ["view"] (:view-ids (task/state sid id))))
          (task/sweep! sid :worker-death)
          (expect (= {:reason :interrupted} (deref ended 1000 nil)))
          (expect (nil? (task/state sid id)))
          (finally (deliver finish true) (task/sweep! sid :session-close))))))

(defdescribe task-events-test
  (it "publishes committed public snapshots in order and stops at settlement"
      (let [sid (random-uuid)
            events (atom [])
            ended (promise)
            failure (fn [kind row]
                      (swap! events conj [kind row])
                      (when (= kind "task.ended") (deliver ended true))
                      (when (= kind "task.started") (throw (ex-info "sink failed" {}))))
            ;; Resolve first so the test fails with a missing publisher before the fix.
            install! (ns-resolve 'com.blockether.vis.internal.task 'set-publisher!)
            ;; The publisher is process-wide; gateway wiring installed it once.
            previous @@#'task/publisher]
        (expect (some? install!))
        (when install!
          (install! (fn [session kind row]
                      (when (= (str sid) session) (failure kind row))))
          (try
            (let [{:keys [row complete!]} (task/start-manual! sid
                                           {:kind :shell :name "shell(secret script)"}
                                           (constantly nil))
                  id (:id row)]
              (task/track-view! [(str sid) id] "view" (constantly nil))
              (task/await! sid id {:block "one" :token 1})
              (task/detach-awaits! sid {:question "Ready?"})
              (complete! (constantly {:state :failed :error "secret exception"}))
              (task/rename! sid id "late rename")
              (task/sweep! sid :session-close)
              (expect (true? (deref ended 5000 false)))
              (expect (= ["task.started" "task.updated" "task.updated" "task.ended"] (mapv first @events)))
              (let [rows (mapv second @events)]
                (expect (every? #(document/valid? "task" (wire/->wire %)) rows))
                (expect (= [false false true true] (mapv :detached rows)))
                (expect (= [[] ["view"] ["view"] []] (mapv :view-ids rows)))
                (expect (= [:running :running :running :failed] (mapv :state rows)))
                (expect (not (.contains (pr-str rows) "secret")))))
            (finally (task/sweep! sid :session-close) (install! previous)))))))

(defdescribe task-sweep-race-test
  (it "does not run queued work swept before its worker starts"
      (let [sid (random-uuid)
            queued (atom nil)
            effects (atom [])]
        (with-redefs [cancellation/worker-future (fn [_ run] (reset! queued run))]
          (let [{:keys [id]} (task/start! sid {:name "queued"} #(swap! effects conj :ran))]
            (try
              (task/sweep! sid :reload)
              (@queued)
              (expect (nil? (task/state sid id)))
              (expect (empty? @effects))
              (finally (task/sweep! sid :session-close)))))))
  (it "closes a view once when sweep overlaps settlement"
      (let [sid (random-uuid)
            finish (promise)
            entered (promise)
            release (promise)
            calls (atom [])
            {:keys [id]} (task/start! sid {:name "watch"} #(do @finish {:state :done}))]
        (try
          (task/track-view! [(str sid) id] "view"
                           (fn [ending]
                             (swap! calls conj ending)
                             (deliver entered true)
                             @release))
          (deliver finish true)
          (expect (true? (deref entered 5000 false)))
          (task/sweep! sid :reload)
          (expect (= [{:reason :completed}] @calls))
          (finally
            (deliver finish true)
            (deliver release true)
            (task/sweep! sid :session-close))))))
