(ns com.blockether.vis.internal.session.agents-integration-test
  (:require [clojure.string :as str]
            [com.blockether.svar.core :as svar]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.context.loop :as ctx-loop]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.loop.environment :as loop-env]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.loop.transcript :as transcript]
            [com.blockether.vis.internal.loop.turn :as turn]
            [com.blockether.vis.internal.paths :as paths]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.python.env :as py-env]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.task :as task]
            [com.blockether.vis.internal.sandbox.jail :as pj]
            [com.blockether.vis.internal.sandbox.scoped-policy :as scoped-policy]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.test-temp :as test-temp]
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
   :tool-calls [{:id "agent-boundary" :name "py3" :input {:code code}}]})

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
              (expect (boolean (= (mapv #(str/replace (message-text %)
                                                       (agents/prompt parent) (agents/prompt child))
                                         checkpoint)
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
              (expect (= (mapv message-text (agents/stable-prefix child ""))
                          (mapv message-text (take (count checkpoint) (first @calls))))
                      "A later state of the child still starts from the parent's checkpoint"))
            (finally (loop-env/dispose-environment! child))))
        (finally (loop-env/dispose-environment! parent))))))

;; Review of #520: a request base that resumes from the provider cache holds the answer of the
;; provider, not the canonical answer. Without a mark, a child lost the last answer of its parent.
(defdescribe
  cached-parent-answer-handoff-test
  (it
    "keeps the complete answer of the parent when its next request resumes from the provider cache"
    (let [parent (loop-env/create-environment (router) {:db :memory})
          context {:id "handoff-test" :fixed-prefix-weight 0}]
      (try
        (with-redefs [svar/provider-prompt-cache (constantly {:ttl-ms 600000})
                      svar/ask-code!
                      (fn [_ _]
                        {:stop-reason :end
                         :content "Accepted cache answer"
                         :prompt-cache-context context
                         :usage {:input-tokens 100 :output-tokens 10}
                         :assistant-message
                         {:role "assistant"
                          :content [{:type "thinking" :thinking "Not parent narrative"}
                                    {:type "text" :text "Accepted cache answer"}]}})]
          (turn/run-turn! parent "The parent request" {})
          (let [{:keys [route entry] :as state}
                (transcript/load-prompt-cache-state (:db-info parent) (:session/state-id parent))

                [provider model]
                route

                completed
                (:completed-turn entry)

                base
                (transcript/resumable-prompt-message-base
                  state provider model context 2 (:summaries completed)
                  (vec (take (:stable-message-count completed) (:messages entry)))
                  [{:role "user" :content "The next request"}])

                text
                (str (:content (peek (agents/handoff (:messages base) "gpt-4o"))))]

            (expect (true? (:resumed? base)))
            (expect (str/includes? text ";; -- PARENT-ANSWER --\nAccepted cache answer"))
            (expect (not (str/includes? text "Not parent narrative")))))
        (finally (loop-env/dispose-environment! parent))))))

;; Regression: a child copied the parent's turns. A fold rebuilt the child's request from those
;; copies, so the parent's raw requests replaced the handoff and its end marker.
(defdescribe
  child-keeps-the-handoff-after-a-fold-test
  (it
    "keeps the parent handoff as the child's base after a fold, without copied turns"
    (let [parent (loop-env/create-environment (router) {:db :memory})]
      (try (let [requests (atom 0)]
             (with-redefs [svar/ask-code!
                           (fn [_ _]
                             (case (swap! requests inc)
                               1 (code-response "print('Raw parent evidence')")
                               2 (code-response
                                   "print(fold_session(f\"-t{session['turn']}/i1\", 'Parent gist'))")
                               3 {:stop-reason :end :content "Parent result"}
                               4 {:stop-reason :end :content "Second parent result"}
                               {:stop-reason :end :content "Third parent result"}))]
               (turn/run-turn! parent "Parent request that the child must not own" {})
               (turn/run-turn! parent "Second parent request" {})
               (turn/run-turn! parent "Third parent request" {})))
           (let [handoff (agents/handoff (:agent-checkpoint (ctx-loop/read-turn-state parent)) "gpt-4o")
                 inherited (str/join "\n" (map message-text handoff))
                 child (child-environment parent handoff 4 nil)
                 calls (atom [])]

             ;; The real prompt builder marks requests, complete answers and fold gists.
             (expect (str/includes? inherited ";; -- PARENT-ANSWER --\nSecond parent result"))
             (expect (str/includes? inherited ";; -- PARENT-REQUEST --\nThird parent request"))
             (expect (re-find #";; -- PARENT-FOLD-GIST --\n[^;]*Parent gist" inherited))
             (expect (not (str/includes? inherited "Raw parent evidence")))
             ;; #520: a child cannot use the history of other sessions, so it gets no rules for it.
             (expect (str/includes? (prompt/stable-prompt-text
                                      (prompt/assemble-stable-prompt-messages
                                        parent
                                        {:active-extensions (prompt/active-extensions parent)}))
                                    "## Session introspection"))
             (expect (not (str/includes? inherited "## Session introspection")))

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
                    (expect (boolean (= (mapv message-text (agents/stable-prefix child ""))
                                        (mapv message-text (take (count handoff) messages))))
                            "Every request starts with the handoff"))
                  (let [after-fold (str/join "\n" (map message-text (last @calls)))]
                    (expect (str/includes? after-fold ";; -- PARENT-REQUEST --"))
                    (expect (str/includes? after-fold "Parent request that the child must not own"))
                    (expect (str/includes? after-fold "END-OF-PARENT-CONVERSATION"))
                    (expect (str/includes? after-fold "Child gist"))
                    (expect (not (str/includes? after-fold "Raw child evidence"))))
                  (expect (= ["Child task after the handoff"]
                             (mapv :user-request
                                   (ps/db-list-session-turns (:db-info child) (:session-id child))))
                          "A child owns only its own turns")
                  (finally (loop-env/dispose-environment! child))))
           (finally (loop-env/dispose-environment! parent))))))

;; #520: the prompt of the parent has no text for subagents only, so the handoff has none.
(defdescribe
  subagent-only-extension-prompt-test
  (it "gives a child the extension prompt for subagents, and the leader none"
      (let [parent (loop-env/create-environment (router) {:db :memory})
            calls (atom [])
            answer (fn [content]
                     (fn [_ opts]
                       (swap! calls conj (mapv message-text (:messages opts)))
                       {:stop-reason :end :content content}))]
        (extension/register-extension! (extension/build-extension
                                         {:ext/name "test-worker"
                                          :ext/description "Rules for delegated work."
                                          :ext/subagent-prompt-fn "Worker rules."}))
        (try (with-redefs [svar/ask-code! (answer "Parent result")]
               (turn/run-turn! parent "Parent request" {}))
             (let [handoff (agents/handoff (:agent-checkpoint (ctx-loop/read-turn-state parent)) "gpt-4o")
                   child (child-environment parent handoff 2 nil)]
               (try (with-redefs [svar/ask-code! (answer "Child result")]
                      (turn/run-turn! child "Child task" {}))
                    (let [[leader-request child-request] @calls
                          at (fn [text]
                               (first (keep-indexed (fn [i m] (when (str/includes? m text) i))
                                                    child-request)))]
                      (expect (= 2 (count @calls)))
                      (expect (not-any? #(str/includes? % "Worker rules.") leader-request))
                      (expect (some? (at ";; -- EXTENSION test-worker --\nWorker rules.")))
                      (expect (< (long (at "Worker rules."))
                                 (long (at "END-OF-PARENT-CONVERSATION")))))
                    (finally (loop-env/dispose-environment! child))))
             (finally (extension/deregister-extension! "test-worker")
                      (loop-env/dispose-environment! parent))))))

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
                   "print(session['access']['filesystem']['write_rules'])\n"
                   "print(session['agent']['files']['write'])")]

          ;; A write-limited sandbox keeps the temp roots writable. In a checkout under
          ;; `$TMPDIR` that would also open `root`, so the root of the checkout is removed.
          (try (with-redefs-fn {#'py-env/tmp-roots
                                (let [roots ((deref #'py-env/tmp-roots))]
                                  (constantly (into [] (remove test-temp/holds-checkout?) roots)))}
                 #(with-redefs [svar/ask-code!
                                (fn [_ opts]
                                  (swap! calls conj (:messages opts))
                                  (case (count @calls)
                                    1
                                    (code-response code)

                                    2
                                    (code-response (str "open('" root "/other.txt', 'w')"))

                                    {:stop-reason :end :content "Grant checked"}))]
                    (turn/run-turn! child "Write only inside out/" {})))
               (let [output
                     (str/join "\n" (map message-text (second @calls)))

                     refusal
                     (str/join "\n" (map message-text (nth @calls 2)))

                     rules
                     (get-in child [:security-policy :process-jail :write-rules])

                     ;; The child reads its grant without the home path; the jail keeps it absolute.
                     shown
                     (pr-str (paths/abbreviate-home (str root "/out/**")))]

                 ;; The rule names a file that did not exist before the grant.
                 (expect (= "ok" (slurp (str root "/out/new.txt"))))
                 (expect (not (.exists (java.io.File. (str root "/other.txt")))))
                 (expect (str/includes? output "other refused PermissionError"))
                 (expect (str/includes? output shown))
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
        (finally (loop-env/dispose-environment! parent)
                 (run! #(.delete ^java.io.File %) (reverse (file-seq (java.io.File. root)))))))))

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
        (finally (loop-env/dispose-environment! parent)
                 (run! #(.delete ^java.io.File %) (reverse (file-seq (java.io.File. root)))))))))

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

(defdescribe registered-python-agent-bindings-reach-host-test
  (it "returns typed Task handles and keeps unread terminal records discoverable"
      (let [env (loop-env/create-environment (router) {:db :memory})
            owner (:session-id env)
            calls (atom [])
            requests (atom 0)]
        (try
          (with-redefs-fn
            {#'agents/start-tasks
             (fn [host tasks]
               (swap! calls conj [(:session-id host) tasks])
               (mapv (fn [n]
                       (let [child-id (str "a" n)
                             record {:id child-id :session_id (str "child-" n)
                                     :status "completed" :answer "Done"}
                             {:keys [row complete!]} (task/start-manual!
                                                       owner {:kind :subagent :name child-id
                                                              :guest-data {:child-id child-id
                                                                           :child-session-id (:session_id record)}}
                                                       (fn [_]))]
                         (complete! (constantly {:state :done :record record
                                                 :value {"__vis_object__" "SubagentRecord"
                                                         "__vis_attrs__" record}}))
                         (task/guest-task owner (:id row))))
                     [1 2]))
             #'svar/ask-code!
             (fn [_ _]
               (case (swap! requests inc)
                 1 (code-response (str "import asyncio\nassert 'agents' not in globals()\n"
                                       "a, b = await council.run(['First', {'task': 'Second'}])\n"
                                       "print(a.id, b.session_id, isinstance(a, asyncio.Task))"))
                 2 (code-response (str "assert a in asyncio.all_tasks()\n"
                                       "assert a.record().answer == 'Done'\n"
                                       "try:\n    a.wait\nexcept AttributeError as exc:\n"
                                       "    assert 'asyncio.wait' in str(exc) and '.record()' in str(exc)\n"
                                       "else:\n    raise AssertionError('The SubagentTask still has wait')\n"
                                       "print((await a).answer, a in asyncio.all_tasks(), b in asyncio.all_tasks())"))
                 {:stop-reason :end :content "Bindings invoked"}))}
            #(turn/run-turn! env "Exercise registered Python methods" {}))
          (expect (= [[owner ["First" {"task" "Second"}]]] @calls))
          (let [db (:db-info env)
                tid (:id (first (ps/db-list-session-turns db owner)))
                forms (pr-str (mapcat :forms (ps/db-list-session-turn-iterations db tid)))]
            (expect (str/includes? forms "a1 child-2 True"))
            (expect (str/includes? forms "Done False True")))
          (finally (loop-env/dispose-environment! env))))))

(defdescribe verifier-verdict-in-task-result-test
  (it "returns the verifier verdict separately from the Task status"
      (let [env (loop-env/create-environment (router) {:db :memory})
            db (:db-info env)
            owner (:session-id env)
            requests (atom 0)]
        (try
          (with-redefs-fn
            {#'agents/runtime (atom {:live? (constantly false)})
             #'agents/start-tasks
             (fn [host _]
               (let [turn (:id (last (ps/db-list-session-turns db owner)))
                     child (h/fork-session-at-turn!
                             db owner
                             {:through-turn-id turn
                              :agent {:parent_id (str owner) :leader_id (str owner)
                                      :team_id (str turn) :task "Check the saved result"
                                      :role "verify" :iteration_budget 4
                                      :spawn_key (str turn) :spawn_fingerprint "verdict"
                                      :checkpoint [{:role :user :content "Verify the result"}]}})
                     row (agents/info db child)
                     handle (agents/register-task! host row (fn [_]) nil)]
                 (agents/report-result (assoc host :session-id child)
                                       "Check failed" {"outcome" "failed"})
                 (agents/finish! db child "completed")
                 (agents/settle-task! host (agents/info db child))
                 [handle]))
             #'svar/ask-code!
             (fn [_ _]
               (if (= 1 (swap! requests inc))
                 (code-response (str "(child,) = await council.run([{'task': 'Check', 'role': 'verify'}])\n"
                                     "assert child.record()['outcome'] == 'failed'\n"
                                     "r = await child\n"
                                     "assert r['status'] == 'completed'\n"
                                     "assert r['outcome'] == child.record().outcome == 'failed'\n"
                                     "print(str(r))"))
                 {:stop-reason :end :content "Read the verdict"}))}
            #(turn/run-turn! env "Verify a result" {}))
          (let [turn (:id (first (ps/db-list-session-turns db owner)))
                forms (pr-str (mapcat :forms (ps/db-list-session-turn-iterations db turn)))]
            (expect (str/includes? forms "a1 verify: completed [failed] - Check failed") forms))
          (finally (loop-env/dispose-environment! env))))))

;; Regression: a turn with a CLI `--model` gave its subagents the router default model.
(defdescribe turn-route-reaches-agent-operations-test
  (it "a per-turn model reaches agent operations without a session pick"
      (let [env (loop-env/create-environment (router) {:db :memory})
            routes (atom [])
            requests (atom 0)]
        (try
          (with-redefs-fn {#'agents/start-tasks
                           (fn [host _]
                             (swap! routes conj (:route (ctx-loop/read-turn-state host)))
                             [])
                           #'svar/ask-code!
                           (fn [_ _]
                             (if (= 1 (swap! requests inc))
                               (code-response "await council.run(['Check docs'])")
                               {:stop-reason :end :content "Spawned"}))}
            #(turn/turn! env [(svar/user "Spawn on the turn model")] {:model "large"}))
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
