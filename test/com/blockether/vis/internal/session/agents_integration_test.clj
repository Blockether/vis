(ns com.blockether.vis.internal.session.agents-integration-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.session.model :as smodel]
            [lazytest.core :refer [around-each defdescribe expect it set-ns-context!]]))

(set-ns-context!
  [(around-each
     [f]
     (let [enabled?
           (toggles/enabled? "subagents")

           registered?
           (some #(= "foundation-core" (:ext/name %)) (extension/registered-extensions))]

       (when-not registered? (foundation/register!))
       (toggles/set-enabled! "subagents" true)
       (try
         ;; Session snapshots also need the fixture's explicit override.
         (binding [toggles/*invocation-overrides* (assoc toggles/*invocation-overrides*
                                                    "subagents" true)]
           (f))
         (finally (toggles/set-enabled! "subagents" enabled?)
                  (when-not registered? (extension/deregister-extension! "foundation-core"))))))])

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

(defn- requested-model
  "The model one provider request asks for: its forced route, else the router root."
  [request-router opts]
  (or (get-in opts [:routing :model]) (get-in request-router [:providers 0 :root])))

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

(defdescribe
  inherited-checkpoint-fresh-sandbox-and-durable-budget-test
  (it
    "inherited checkpoint fresh sandbox and durable budget"
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

          (expect (str/includes? (str/join "\n" (map message-text checkpoint))
                                 "Accepted folded parent conclusion"))
          (expect (not (str/includes? (str/join "\n" (map message-text checkpoint))
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
                (expect (= "Delegated result" (get-in result [:answer :answer]))))
              (expect (= 2 (count @calls)))
              ;; Provider cache markers may wrap text blocks without changing their contents.
              (expect (boolean (= (mapv message-text checkpoint)
                                  (mapv message-text (take (count checkpoint) (first @calls))))))
              (expect (boolean (str/includes? (str/join "\n" (map message-text (first @calls)))
                                              "\"role\": \"subagent\"")))
              (expect (str/includes? (pr-str (first @calls)) "Verify the delegated boundary only"))
              (expect (str/includes? (str/join "\n" (map message-text (second @calls)))
                                     "False\nsubagent\n"))
              (expect (str/includes? (pr-str (second @calls)) (str (:session-id parent))))
              (expect (= 2 (:iterations_used (agents/info (:db-info child) (:session-id child)))))
              (turn/run-turn! child "An exhausted child cannot make another paid request" {})
              (expect (= 2 (count @calls)))
              (expect (= "budget_limited"
                         (:status (agents/info (:db-info child) (:session-id child)))))
              (h/fork-session! (:db-info child) (:session-id child) {})
              (expect
                (nil? (agents/inherited-base child 2 [{:role "user" :content "Post-fold task"}] []))
                "A later state must not reinherit the parent's checkpoint at a repeated turn position"))
            (finally (loop-env/dispose-environment! child))))
        (finally (loop-env/dispose-environment! parent))))))

(defdescribe session-route-changes-at-the-next-model-request-test
             (it "session route changes at the next model request"
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
                        (expect (= 2 (count @calls)))
                        (expect (= "large" (get-in @calls [1 1 :model])))
                        (expect (= "small" (get-in shared [:providers 0 :root])))
                        (expect (= 2 (count (get-in shared [:providers 0 :models]))))
                        (finally (loop-env/dispose-environment! env))))))

(defdescribe
  session-route-change-is-visible-to-output-budget-recovery-test
  (it
    "session route change is visible to the output-budget recovery request"
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
                           (swap! calls conj (requested-model request-router opts))
                           (if (= 1 (count @calls))
                             (do (smodel/set-model! (:db-info env)
                                                    (:session-id env)
                                                    "fixture"
                                                    "large"
                                                    :agent-routing)
                                 ;; Svar already spent its own larger-budget re-send.
                                 (throw (ex-info "Reasoning exhausted the output budget"
                                                 {:type :svar.llm/max-tokens-exceeded
                                                  :output-tokens 32
                                                  :output-budget-resends 1})))
                             {:stop-reason :end :content "Recovered result"}))]

             (smodel/set-model! (:db-info env) (:session-id env) "fixture" "small" :agent-routing)
             (turn/run-turn! env "Observe routing at the output-budget recovery request" {}))
           (expect (= ["small" "large"] @calls))
           (expect (= "small" (get-in shared [:providers 0 :root])))
           (finally (loop-env/dispose-environment! env))))))

;; Regression: a model picked in the app or the TUI while a turn was running re-routed
;; that turn from its very next iteration. The picker is composer state: the running turn
;; keeps the route it started with, and the pick takes effect on the next turn. The cases
;; call `turn!`, the production entry that pins each turn's route when the turn starts.
(defdescribe
  manual-pick-waits-for-the-next-turn-test
  "A model picked during a turn changes the next turn, never the running one."
  (it "keeps the running turn on its route and moves the next turn"
      (let [shared
            (router)

            env
            (loop-env/create-environment shared {:db :memory})

            calls
            (atom [])]

        (try
          (with-redefs [loop-router/get-router
                        (constantly shared)

                        svar/ask-code!
                        (fn [request-router opts]
                          (swap! calls conj (requested-model request-router opts))
                          (if (= 1 (count @calls))
                            (do
                              (smodel/set-model! (:db-info env) (:session-id env) "fixture" "large")
                              (code-response "print('Picker changed')"))
                            {:stop-reason :end :content "Turn result"}))]

            (turn/turn! env [(svar/user "Finish on the route this turn started with")] {})
            (turn/turn! env [(svar/user "Start on the newly picked route")] {}))
          (expect (= ["small" "small" "large"] @calls))
          (finally (loop-env/dispose-environment! env)))))
  (it "keeps the output-budget recovery request on the running turn's route"
      (let [shared
            (router)

            env
            (loop-env/create-environment shared {:db :memory})

            calls
            (atom [])]

        (try
          (with-redefs [loop-router/get-router
                        (constantly shared)

                        svar/ask-code!
                        (fn [request-router opts]
                          (swap! calls conj (requested-model request-router opts))
                          (if (= 1 (count @calls))
                            (do
                              (smodel/set-model! (:db-info env) (:session-id env) "fixture" "large")
                              (throw (ex-info "Reasoning exhausted the output budget"
                                              {:type :svar.llm/max-tokens-exceeded
                                               :output-tokens 32})))
                            {:stop-reason :end :content "Turn result"}))]

            (turn/turn! env [(svar/user "Recover on the route this turn started with")] {})
            (turn/turn! env [(svar/user "Start on the newly picked route")] {}))
          (expect (= ["small" "small" "large"] @calls))
          (finally (loop-env/dispose-environment! env)))))
  (it
    "applies an agent route at the next request and a later manual pick at the next turn"
    (let [shared
          (router)

          env
          (loop-env/create-environment shared {:db :memory})

          calls
          (atom [])]

      (try
        (with-redefs [loop-router/get-router
                      (constantly shared)

                      svar/ask-code!
                      (fn [request-router opts]
                        (swap! calls conj (requested-model request-router opts))
                        (case (count @calls)
                          1
                          (do (smodel/set-model! (:db-info env)
                                                 (:session-id env)
                                                 "fixture"
                                                 "large"
                                                 :agent-routing)
                              (code-response "print('Agent routed')"))

                          2
                          (do (smodel/set-model! (:db-info env) (:session-id env) "fixture" "small")
                              (code-response "print('Picker changed')"))

                          {:stop-reason :end :content "Turn result"}))]

          (turn/turn! env [(svar/user "Follow the agent route inside this turn")] {})
          (turn/turn! env [(svar/user "Start on the newly picked route")] {}))
        (expect (= ["small" "large" "large" "small"] @calls))
        (finally (loop-env/dispose-environment! env))))))

(defdescribe
  registered-python-agent-bindings-reach-host-test
  (it
    "registered python agent bindings reach host"
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
        (expect (= [:list :spawn :route :cancel] (mapv second @calls)))
        (expect (= [{} {:task "Verify only the boundary" :iteration_budget 2}
                    {:model "small" :provider "fixture"} {:session_id "fixture-child"}]
                   (mapv #(nth % 2) @calls)))
        (expect (every? #(= (:session-id env) (first %)) @calls))
        (finally (loop-env/dispose-environment! env))))))

(defdescribe
  child-allowlist-survives-router-rehydration-and-retries-test
  (it
    "child allowlist survives router rehydration and retries"
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
                 (expect (= 2 (count @calls)))
                 (expect (every? #(= ["small"] (mapv :name (get-in % [0 :providers 0 :models])))
                                 @calls))
                 (smodel/set-model! (:db-info child)
                                    (:session-id child)
                                    "fixture"
                                    "large"
                                    :agent-routing)
                 (turn/run-turn! child "A disallowed persisted pin cannot bypass the allowlist" {})
                 (expect (every? #(not= "large" (get-in % [1 :model])) @calls))
                 (expect (every? #(= ["small"] (mapv :name (get-in % [0 :providers 0 :models])))
                                 @calls))
                 (expect (= 2 (count (get-in shared [:providers 0 :models])))))
               (finally (loop-env/dispose-environment! child))))
           (finally (loop-env/dispose-environment! parent))))))
