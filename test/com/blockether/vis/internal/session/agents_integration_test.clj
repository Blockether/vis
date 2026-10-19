(ns com.blockether.vis.internal.session.agents-integration-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config.scoped :as scoped]
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
            [com.blockether.vis.internal.sandbox.jail :as pj]
            [com.blockether.vis.internal.sandbox.scoped-policy :as scoped-policy]
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
  [parent checkpoint budget allowed & [files]]
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
                                          :iteration_budget budget
                                          :allowed_models allowed
                                          :files files
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
              (expect (= (vec checkpoint) (agents/stable-prefix child ""))
                      "A later state of the child still starts from the parent's checkpoint"))
            (finally (loop-env/dispose-environment! child))))
        (finally (loop-env/dispose-environment! parent))))))

;; Regression: a child copied the parent's turns. A fold rebuilt the child's request from those
;; copies, so the parent's raw requests replaced the handoff and its end marker.
(defdescribe
  child-keeps-the-handoff-after-a-fold-test
  (it
    "keeps the parent handoff as the child's base after a fold, without copied turns"
    (let [parent (loop-env/create-environment (router) {:db :memory})]
      (try (with-redefs [svar/ask-code! (fn [_ _]
                                          {:stop-reason :end :content "Parent result"})]
             (turn/run-turn! parent "Parent request that the child must not own" {}))
           (let [handoff (agents/handoff (:agent-checkpoint (ctx-loop/read-turn-state parent)))
                 child (child-environment parent handoff 4 nil)
                 calls (atom [])]

             (try (with-redefs
                    [svar/ask-code!
                     (fn [_ opts]
                       (swap! calls conj (:messages opts))
                       (case (count @calls)
                         1
                         (code-response "print('Raw child evidence')")

                         2
                         (code-response
                           "print(fold_session(f\"-t{session['turn']}/i1\", 'Child gist'))")

                         {:stop-reason :end :content "Delegated result"}))]
                    (turn/run-turn! child "Child task after the handoff" {}))
                  (expect (= 3 (count @calls)))
                  (doseq [messages @calls]
                    (expect (boolean (= (mapv message-text handoff)
                                        (mapv message-text (take (count handoff) messages))))
                            "Every request starts with the handoff"))
                  (let [after-fold (str/join "\n" (map message-text (last @calls)))]
                    (expect (str/includes? after-fold "END-OF-PARENT-CONVERSATION"))
                    (expect (str/includes? after-fold "Child gist"))
                    (expect (not (str/includes? after-fold "Raw child evidence"))))
                  (expect (= ["Child task after the handoff"]
                             (mapv :user-request
                                   (ps/db-list-session-turns (:db-info child) (:session-id child))))
                          "A child owns only its own turns")
                  (finally (loop-env/dispose-environment! child))))
           (finally (loop-env/dispose-environment! parent))))))

(defdescribe
  subagent-file-grant-test
  (it
    "a file grant limits a child's writes to its rules, also for future files"
    (let [root
          (.getCanonicalPath (doto (java.io.File. (str "target/agent-files-" (System/nanoTime)))
                               (.mkdirs)))

          parent
          (loop-env/create-environment (router) {:db :memory})]

      (try
        (with-redefs [svar/ask-code! (fn [_ _]
                                       {:stop-reason :end :content "Parent result"})]
          (turn/run-turn! parent "Delegate with a file grant." {}))
        ;; The parent's own session policy closes a file; the child must keep that rule.
        (let [host (#'scoped-policy/host-config)]
          (scoped/set-definition! (:db-info parent)
                                  (scoped/target (:db-info parent) "session" (:session-id parent))
                                  ["jail"]
                                  "filesystem"
                                  (update (get-in host ["jail" "filesystem"] {})
                                          "deny_read"
                                          (fnil conj [])
                                          (str root "/parent-secret.txt"))))
        (let [checkpoint
              (:agent-checkpoint (ctx-loop/read-turn-state parent))

              child
              (child-environment parent
                                 checkpoint
                                 4
                                 nil
                                 {:write [(str root "/out/**")] :deny [(str root "/secret.txt")]})

              calls
              (atom [])

              code
              (str "import pathlib\n"
                   "pathlib.Path('"
                   root
                   "/out').mkdir(exist_ok=True)\n"
                   "pathlib.Path('" root
                   "/out/new.txt').write_text('ok')\n" "try:\n"
                   "    pathlib.Path('" root
                   "/other.txt').write_text('no')\n" "    print('other written')\n"
                   "except Exception as e:\n" "    print('other refused', type(e).__name__, e)\n"
                   "print(session['access']['filesystem']['read_write'])\n"
                   "print(session['agent']['files']['write'])")]

          (try (with-redefs [svar/ask-code! (fn [_ opts]
                                              (swap! calls conj (:messages opts))
                                              (case (count @calls)
                                                1
                                                (code-response code)

                                                2
                                                (code-response
                                                  (str "open('" root "/other.txt', 'w')"))

                                                {:stop-reason :end :content "Grant checked"}))]
                 (turn/run-turn! child "Write only inside out/" {}))
               (let [output
                     (str/join "\n" (map message-text (second @calls)))

                     refusal
                     (str/join "\n" (map message-text (nth @calls 2)))

                     rules
                     (get-in child [:security-policy :process-jail :write-rules])]

                 ;; The rule names a file that did not exist before the grant.
                 (expect (= "ok" (slurp (str root "/out/new.txt"))))
                 (expect (not (.exists (java.io.File. (str root "/other.txt")))))
                 (expect (str/includes? output "other refused PermissionError"))
                 (expect (str/includes? output (str root "/out/**")))
                 (expect (= [(str root "/out/**")] rules))
                 ;; Host tools ask the same policy: deny closes reads, write rules close the rest.
                 (expect (nil? (pj/deny-refusal child "file-write" (str root "/out/later/a.clj"))))
                 (expect (str/includes? (pj/deny-refusal child "file-write" (str root "/other.txt"))
                                        "Subagent file grant"))
                 (expect (some? (pj/deny-refusal child "file-read" (str root "/secret.txt"))))
                 (expect (nil? (pj/deny-refusal child "file-read" (str root "/other.txt"))))
                 ;; The parent's policy still applies to the child.
                 (expect (some?
                           (pj/deny-refusal child "file-read" (str root "/parent-secret.txt"))))
                 ;; An uncaught refusal explains the grant and keeps the runtime's own text.
                 (expect (str/includes? refusal "Subagent file grant: write only"))
                 (expect (str/includes? refusal "is outside the writable roots")))
               (finally (loop-env/dispose-environment! child))))
        (finally (loop-env/dispose-environment! parent))))))

(defdescribe
  subagent-file-grant-reaches-a-running-child-test
  (it
    "a later grant reaches a running child at its next block"
    (let [root
          (.getCanonicalPath (doto (java.io.File. (str "target/agent-grant-" (System/nanoTime)))
                               (.mkdirs)))

          parent
          (loop-env/create-environment (router) {:db :memory})]

      (try
        (with-redefs [svar/ask-code! (fn [_ _]
                                       {:stop-reason :end :content "Parent result"})]
          (turn/run-turn! parent "Delegate with a file grant." {}))
        (let [checkpoint
              (:agent-checkpoint (ctx-loop/read-turn-state parent))

              child
              (child-environment parent checkpoint 4 nil {:write [(str root "/out/**")]})

              db
              (:db-info child)

              sid
              (str (:session-id child))

              target
              (str root "/later.txt")

              ;; Like council.grant: store the rules, then replace the live jail.
              grant!
              (fn []
                (ps/db-agent-update! db sid {:files {:write [(str root "/out/**") target]}})
                (pj/grant-jail! sid (:process-jail (scoped-policy/snapshot db sid))))

              calls
              (atom [])]

          (try (with-redefs [svar/ask-code!
                             (fn [_ opts]
                               (swap! calls conj (:messages opts))
                               (case (count @calls)
                                 1
                                 (code-response (str "kept = 41\n"
                                                     "try:\n"
                                                     "    open('"
                                                     target
                                                     "', 'w').write('no')\n"
                                                     "except PermissionError:\n"
                                                     "    print('before grant refused')"))

                                 2
                                 (do (grant!)
                                     (code-response (str "open('" target
                                                         "', 'w').write('yes')\n"
                                                         "print('after grant kept', kept)")))

                                 {:stop-reason :end :content "Grant used"}))]
                 (turn/run-turn! child "Write later.txt after the grant" {}))
               (let [before
                     (str/join "\n" (map message-text (second @calls)))

                     after
                     (str/join "\n" (map message-text (nth @calls 2)))]

                 (expect (str/includes? before "before grant refused"))
                 (expect (= "yes" (slurp target)))
                 ;; The worker restarts with the new policy and restores the saved variables.
                 (expect (str/includes? after "after grant kept 41"))
                 (expect (nil? (pj/deny-refusal child "file-write" target)))
                 (expect (some? (pj/deny-refusal child "file-write" (str root "/other.txt"))))
                 (expect (= [(str root "/out/**") target]
                            (get-in (agents/info db sid) [:files :write]))))
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
             {:session_id (str "child-" (count @calls))})

           agents/wait-records
           (fn [host ids timeout]
             (swap! calls conj [(:session-id host) :wait {:ids (vec ids) :timeout timeout}])
             [{:session_id "child-1" :status "completed" :answer "Done"}])

           svar/ask-code!
           (fn [_ _]
             (if (= 1 (swap! requests inc))
               (code-response
                 "assert 'agents' not in globals()\nassert callable(council.members)\nr = council.run(['Verify only the boundary', {'task': 'Check docs', 'iteration_budget': 2}], timeout_secs=5)\nassert r[0]['answer'] == 'Done', r\nassert council.wait(['child-1'])[0]['status'] == 'completed'\nassert council.wait(timeout_secs=3)[0]['status'] == 'completed'\nassert council.cancel('child-1')[0]['session_id'] == 'child-1'")
               {:stop-reason :end :content "Bindings invoked"}))]

          (turn/run-turn! env "Exercise registered Python methods" {}))
        (expect (= [:spawn :spawn :wait :wait :wait :cancel :wait] (mapv second @calls)))
        (expect (= [{:task "Verify only the boundary"} {:task "Check docs" :iteration_budget 2}
                    {:ids ["child-1" "child-2"] :timeout 5} {:ids ["child-1"] :timeout 240}
                    {:ids [] :timeout 3} {:session_id "child-1"} {:ids ["child-1"] :timeout 5}]
                   (mapv #(nth % 2) @calls)))
        (expect (every? #(= (:session-id env) (first %)) @calls))
        (finally (loop-env/dispose-environment! env))))))

;; Regression: a turn with a CLI `--model` gave its subagents the router default model.
(defdescribe turn-route-reaches-agent-operations-test
             (it "a per-turn model reaches agent operations without a session pick"
                 (let [env
                       (loop-env/create-environment (router) {:db :memory})

                       routes
                       (atom [])

                       requests
                       (atom 0)]

                   (try
                     (with-redefs [agents/operation!
                                   (fn [host _ _]
                                     (swap! routes conj (:route (ctx-loop/read-turn-state host)))
                                     {:session_id "child-1"})

                                   agents/wait-records
                                   (fn [_ _ _]
                                     [{:session_id "child-1" :status "completed" :answer "Done"}])

                                   svar/ask-code!
                                   (fn [_ _]
                                     (if (= 1 (swap! requests inc))
                                       (code-response "council.run(['Check docs'], timeout_secs=5)")
                                       {:stop-reason :end :content "Spawned"}))]

                       (turn/turn! env [(svar/user "Spawn on the turn model")] {:model "large"}))
                     (expect (= [{:provider "fixture" :model "large"}] @routes))
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
