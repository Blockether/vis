(ns com.blockether.vis.internal.session.goals-tool-test
  (:require [charred.api :as json]
            [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.env :as ep]
            [com.blockether.vis.internal.session.goals :as goals]
            [com.blockether.vis.test-python-context :as tpc]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(h/use-mem-store!)

(defn- environment [] {:db-info (h/store) :session-id (h/store-session! (h/store) {:channel :api})})

(defn- bindings
  [env]
  {'update_goal
   (fn [& args]
     (extension/invoke-symbol-wrapper foundation/vis-extension (first goals/symbols) args env))})

;; Regression #200: checking persisted state alone missed a failure after the write.
(deftest update-goal-envelope-test
  (doseq [status ["complete" "blocked"]]
    (let [{:keys [db-info session-id] :as env} (environment)
          before (goals/set-goal! db-info session-id "Verify the goal result" nil)
          entry (first goals/symbols)
          result ((:ext.symbol/fn entry)
                   env
                   (get before "id")
                   (get before "version")
                   status
                   "  Verified evidence or concrete blocker.  ")
          after (goals/check-goal env)]

      (is (= 'update_goal (:ext.symbol/symbol entry)))
      (is (false? (get-in entry [:ext.symbol/activity :show-start])))
      (is (some #{entry} (get-in foundation/vis-extension [:ext/engine :ext.engine/symbols])))
      (is (= "goal_id, version, status, reason" (extension/symbol-signature entry)))
      (is (every? :required? (:ext.symbol/params entry)))
      (is (str/includes? (extension/symbol-doc-text entry) "mixed calls are supported"))
      (is (extension/tool-result? result))
      (is (true? (:success? result)))
      (is (= after (:result result)))
      (is (= status (get after "status")))
      (is (= "Verified evidence or concrete blocker." (get after "reason")))
      (is (= 2 (get after "version")))
      (is (= 2 (get after "revision"))))))

(deftest update-goal-python-call-shapes-test
  ;; #200 also rejected mixed positional/keyword calls advertised by the signature.
  (doseq [worker? [false true]]
    (let [{:keys [db-info session-id] :as env} (environment)]
      (tpc/with-own
        [ctx (bindings env) nil {:worker? worker?}]
        (doseq [status ["complete" "blocked"]
                positional-count (range 5)]

          (testing (str "worker=" worker? ", status=" status ", positional=" positional-count)
            (let [before (goals/set-goal! db-info session-id "Verify Python goal calls" nil)
                  parameters [["goal_id" "g['id']"] ["version" "g['version']"]
                              ["status" (pr-str status)] ["reason" "'  Checked evidence.  '"]]
                  args (concat (map second (take positional-count parameters))
                               (map (fn [[k v]]
                                      (str k "=" v))
                                    (reverse (drop positional-count parameters))))
                  _ (ep/bind-ctx! ctx {"goal" before})
                  result (ep/run-python-block ctx
                                              (str "g = session['goal']\n"
                                                   "r = update_goal(" (str/join ", " args)
                                                   ")\n" "print(json.dumps({k: r[k] for k in g}))")
                                              "t1/i1")
                  after (goals/check-goal env)]

              (is (nil? (:error result)) (str (:error result)))
              (when (nil? (:error result))
                (is (= after (json/read-json (:stdout result) :key-fn identity))))
              (is (= status (get after "status")))
              (is (= "Checked evidence." (get after "reason")))
              (is (= 2 (get after "version")))
              (is (= (inc (get before "revision")) (get after "revision"))))))))))

(deftest update-goal-python-rejections-do-not-write-test
  (doseq [worker? [false true]]
    (let [{:keys [db-info session-id] :as env} (environment)
          before (goals/set-goal! db-info session-id "Preserve the active goal" nil)]

      (tpc/with-own
        [ctx (bindings env) nil {:worker? worker?}]
        (ep/bind-ctx! ctx {"goal" before})
        (doseq [args ["" "g['id'], g['version'], 'complete', 'Checked', 'extra'"
                      "g['id'], g['version'], 'complete', status='blocked', reason='Duplicate'"
                      "g['id'], g['version'], 'complete'"
                      "g['id'], g['version'], status='complete', reason='Checked', extra=True"
                      "g['id'], g['version'], 'complete', 'Checked', goal_id=g['id']"
                      "goal_id=g['id'], status='complete', reason='Checked'"
                      "g['id'], g['version'], 'active', 'Not permitted'"
                      "g['id'], g['version'], 'complete', '  '"
                      "'wrong-id', g['version'], 'blocked', 'Stale identity'"
                      "g['id'], g['version'] + 1, 'blocked', 'Stale version'"]]
          (testing (str "worker=" worker? ", args=" args)
            (let [result (ep/run-python-block ctx
                                              (str "g = session['goal']\nupdate_goal(" args ")")
                                              "t1/i1")]
              (is (some? (:error result)))
              (is (= before (goals/check-goal env))))))
        (let [result (ep/run-python-block
                       ctx
                       "update_goal(g['id'], g['version'], 'blocked', 'Needs external input')"
                       "t1/i2")
              after (goals/check-goal env)
              retry-result (ep/run-python-block
                             ctx
                             "update_goal(g['id'], g['version'], 'blocked', 'Retry')"
                             "t1/i3")]

          (is (nil? (:error result)) (str (:error result)))
          (is (some? (:error retry-result)))
          (is (= after (goals/check-goal env)))
          (is (= 2 (get after "version"))))))))

(deftest update-goal-activity-outcome-test
  (doseq [status ["complete" "blocked" nil]]
    (let [{:keys [db-info session-id] :as env} (environment)
          before (when status (goals/set-goal! db-info session-id "Verify goal Activity" nil))
          events (atom [])
          result (try (binding [extension/*tool-event-sink* #(swap! events conj %)]
                        ((get (bindings env) 'update_goal)
                          (get before "id" "missing")
                          1
                          (or status "blocked")
                          "Verified evidence"))
                      (catch clojure.lang.ExceptionInfo e e))
          [start terminal] @events]

      (is (= [:start :terminal] (mapv :phase @events)))
      (is (false? (:show-start start)))
      (is (= [:mutation :mutation] (mapv :classification @events)))
      (is (= :update_goal (:operation terminal)))
      (if status
        (do (is (true? (:succeeded terminal)))
            (is (= status (get result "status")))
            (is (= "Updated goal" (get-in terminal [:presentation "headline"])))
            (is (str/includes? (str (:presentation terminal)) status)))
        (do (is (true? (:failed terminal)))
            (is (str/includes? (ex-message result) "stopped or changed"))
            (is (nil? (goals/check-goal env))))))))
