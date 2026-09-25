(ns com.blockether.vis.internal.session.agents-integration-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.iteration :as iteration]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.model :as smodel]
            [lazytest.core :refer [around-each set-ns-context!]]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is]]))

(set-ns-context! [(around-each [f]
                               (let [enabled?
                                     (toggles/enabled? "subagents")

                                     registered?
                                     (some #(= "foundation-core" (:ext/name %))
                                           (extension/registered-extensions))]

                                 (when-not registered? (foundation/register!))
                                 (toggles/set-enabled! "subagents" true)
                                 (try (f)
                                      (finally (toggles/set-enabled! "subagents" enabled?)
                                               (when-not registered?
                                                 (extension/deregister-extension!
                                                   "foundation-core"))))))])

(defn- router
  []
  ;; These fixtures exercise checkpoints and iteration budgets, not context overflow.
  (svar/make-router [{:id :fixture
                      :api-key "test"
                      :base-url "http://127.0.0.1:1/v1"
                      :root "small"
                      :models [{:name "small" :context 200000} {:name "large" :context 200000}]}]))

(defn- message-text
  [message]
  (let [content (:content message)]
    (if (string? content) content (str/join "\n" (map :text content)))))

(defn- code-response
  [code]
  {:stop-reason :tool-calls
   :tool-calls [{:id "agent-boundary" :name "python_execution" :input {:code code}}]})

(defn- child-environment
  [parent checkpoint budget allowed]
  (let [db
        (:db-info parent)

        sid
        (:session-id parent)

        turn
        (:id (last (ps/db-list-session-turns db sid)))

        child
        (h/fork-session-at-turn! db
                                 sid
                                 {:through-turn-id turn
                                  :agent {:parent_id (str sid)
                                          :leader_id (str sid)
                                          :team_id (str turn)
                                          :task "Verify the delegated boundary only"
                                          :depth 1
                                          :iteration_budget budget
                                          :allowed_models allowed
                                          :spawn_key (str turn)
                                          :spawn_fingerprint "boundary-fixture"
                                          :checkpoint checkpoint}})]

    (loop-env/create-environment (:router parent)
                                 {:db {:datasource (:datasource db)} :session child})))

(deftest inherited-checkpoint-fresh-sandbox-and-durable-budget-test
  (let [parent
        (loop-env/create-environment (router) {:db :memory})

        parent-calls
        (atom 0)]

    (try
      (with-redefs [svar/ask-code!
                    (fn [_ _]
                      (case (swap! parent-calls inc)
                        1
                        (code-response
                          "parent_only = object()\nprint('Raw parent evidence to fold')")

                        2
                        (code-response
                          "print(fold_session('-t1/i1', 'Accepted folded parent conclusion'))")

                        {:stop-reason :end :content "Parent result"}))]
        (turn/run-turn! parent "Preserve the accepted conclusion and delegate." {}))
      (let [checkpoint
            (:agent-checkpoint (ctx-loop/read-turn-state parent))

            child
            (child-environment parent checkpoint 2 nil)

            calls
            (atom [])]

        (is (str/includes? (str/join "\n" (map message-text checkpoint))
                           "Accepted folded parent conclusion"))
        (is (not (str/includes? (str/join "\n" (map message-text checkpoint))
                                "Raw parent evidence to fold")))
        (try
          (with-redefs
            [svar/ask-code!
             (fn [_ opts]
               (swap! calls conj (:messages opts))
               (if (= 1 (count @calls))
                 (code-response
                   "print('parent_only' in globals())\nprint(session['agent']['role'])\nprint(session['agent']['parent_id'])")
                 {:stop-reason :end :content "Delegated result"}))]
            (let [result (turn/run-turn! child "Verify the delegated boundary only" {})]
              (is (= "Delegated result" (get-in result [:answer :answer]))))
            (is (= 2 (count @calls)))
            ;; Provider cache markers may wrap text blocks without changing their contents.
            (is (boolean (= (mapv message-text checkpoint)
                            (mapv message-text (take (count checkpoint) (first @calls))))))
            (is (boolean (str/includes? (str/join "\n" (map message-text (first @calls)))
                                        "\"role\": \"subagent\"")))
            (is (str/includes? (pr-str (first @calls)) "Verify the delegated boundary only"))
            (is (str/includes? (str/join "\n" (map message-text (second @calls)))
                               "False\nsubagent\n"))
            (is (str/includes? (pr-str (second @calls)) (str (:session-id parent))))
            (is (= 2 (:iterations_used (agents/info (:db-info child) (:session-id child)))))
            (turn/run-turn! child "An exhausted child cannot make another paid request" {})
            (is (= 2 (count @calls)))
            (is (= "budget_limited" (:status (agents/info (:db-info child) (:session-id child)))))
            (h/fork-session! (:db-info child) (:session-id child) {})
            (is
              (nil? (agents/inherited-base child 2 [{:role "user" :content "Post-fold task"}] []))
              "A later state must not reinherit the parent's checkpoint at a repeated turn position"))
          (finally (loop-env/dispose-environment! child))))
      (finally (loop-env/dispose-environment! parent)))))

(deftest session-route-changes-at-the-next-model-request-test
  (let [shared
        (router)

        env
        (loop-env/create-environment shared {:db :memory})

        calls
        (atom [])]

    (try (with-redefs [loop-router/get-router
                       (constantly shared)

                       svar/ask-code!
                       (fn [request-router opts]
                         (swap! calls conj [request-router (:routing opts)])
                         (if (= 1 (count @calls))
                           (do (smodel/set-model! (:db-info env)
                                                  (:session-id env)
                                                  "fixture"
                                                  "large"
                                                  :agent-routing)
                               (code-response "print('Route selected')"))
                           {:stop-reason :end :content "Routed result"}))]

           (turn/run-turn! env "Verify a local route change" {}))
         (is (= 2 (count @calls)))
         (is (= "large" (get-in @calls [1 1 :model])))
         (is (= "small" (get-in shared [:providers 0 :root])))
         (is (= 2 (count (get-in shared [:providers 0 :models]))))
         (finally (loop-env/dispose-environment! env)))))

(deftest session-route-change-is-visible-to-a-retry-test
  (let [shared
        (router)

        env
        (loop-env/create-environment shared {:db :memory})

        calls
        (atom [])]

    (try (with-redefs [iteration/MAX_MAX_TOKENS_EXCEEDED_RETRIES
                       2

                       loop-router/get-router
                       (constantly shared)

                       svar/ask-code!
                       (fn [request-router opts]
                         (swap! calls conj
                           (or (get-in opts [:routing :model])
                               (get-in request-router [:providers 0 :root])))
                         (if (< (count @calls) 3)
                           (do (smodel/set-model! (:db-info env)
                                                  (:session-id env)
                                                  "fixture"
                                                  (if (= 1 (count @calls)) "large" "small")
                                                  :agent-routing)
                               (throw (ex-info "Reasoning exhausted the output budget"
                                               {:type :svar.llm/max-tokens-exceeded
                                                :output-tokens 32})))
                           {:stop-reason :end :content "Retried result"}))]

           (smodel/set-model! (:db-info env) (:session-id env) "fixture" "small" :agent-routing)
           (turn/run-turn! env "Observe routing at every model request boundary" {}))
         (is (= ["small" "large" "small"] @calls))
         (is (= "small" (get-in shared [:providers 0 :root])))
         (finally (loop-env/dispose-environment! env)))))

(deftest registered-python-agent-bindings-reach-host-test
  (let [env
        (loop-env/create-environment (router) {:db :memory})

        calls
        (atom [])

        requests
        (atom 0)]

    (try
      (with-redefs
        [agents/operation!
         (fn [host op opts]
           (swap! calls conj [(:session-id host) op opts])
           {:status "observed"})

         svar/ask-code!
         (fn [_ _]
           (if (= 1 (swap! requests inc))
             (code-response
               "assert 'agents' not in globals()\nassert callable(council.members)\nassert council.subagents()['op'] == 'council.subagents'\nassert council.publish_spawn('Verify only the boundary', iteration_budget=2)['op'] == 'council.publish_spawn'\nassert council.route('small', provider='fixture')['op'] == 'council.route'\nassert council.cancel('fixture-child')['op'] == 'council.cancel'")
             {:stop-reason :end :content "Bindings invoked"}))]

        (turn/run-turn! env "Exercise registered Python methods" {}))
      (is (= [:list :spawn :route :cancel] (mapv second @calls)))
      (is (= [{} {:task "Verify only the boundary" :iteration_budget 2}
              {:model "small" :provider "fixture"} {:session_id "fixture-child"}]
             (mapv #(nth % 2) @calls)))
      (is (every? #(= (:session-id env) (first %)) @calls))
      (finally (loop-env/dispose-environment! env)))))

(deftest child-allowlist-survives-router-rehydration-and-retries-test
  (let [shared
        (router)

        parent
        (loop-env/create-environment shared {:db :memory})]

    (try (with-redefs [svar/ask-code! (fn [_ _]
                                        {:stop-reason :end :content "Parent result"})]
           (turn/run-turn! parent "Prepare the delegation checkpoint" {}))
         (let [child
               (child-environment parent
                                  (:agent-checkpoint (ctx-loop/read-turn-state parent))
                                  10
                                  [{:provider "fixture" :model "small"}])

               calls
               (atom [])]

           (try
             (with-redefs [loop-router/get-router
                           (constantly shared)

                           svar/ask-code!
                           (fn [request-router opts]
                             (swap! calls conj [request-router (:routing opts)])
                             (if (= 1 (count @calls))
                               (throw (ex-info "Retry within inherited policy"
                                               {:type :svar.llm/max-tokens-exceeded
                                                :output-tokens 32}))
                               {:stop-reason :end :content "Allowed model result"}))]

               (turn/run-turn! child "Retry only within the inherited model allowlist" {})
               (is (= 2 (count @calls)))
               (is (every? #(= ["small"] (mapv :name (get-in % [0 :providers 0 :models]))) @calls))
               (smodel/set-model! (:db-info child)
                                  (:session-id child)
                                  "fixture"
                                  "large"
                                  :agent-routing)
               (turn/run-turn! child "A disallowed persisted pin cannot bypass the allowlist" {})
               (is (every? #(not= "large" (get-in % [1 :model])) @calls))
               (is (every? #(= ["small"] (mapv :name (get-in % [0 :providers 0 :models]))) @calls))
               (is (= 2 (count (get-in shared [:providers 0 :models])))))
             (finally (loop-env/dispose-environment! child))))
         (finally (loop-env/dispose-environment! parent)))))
