(ns com.blockether.vis.internal.session.agents-test
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.provider.auth-health :as auth-health]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.gateway.state :as gateway]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.sandbox.jail :as pj]
            [com.blockether.vis.internal.sandbox.policy :as security-policy]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.core :refer [defdescribe describe expect it]]))

(wiring/install!)

(h/use-mem-store! {"subagents" true})

(defdescribe
  schema-owned-agent-bounds-test
  (it
    "schema owned agent bounds"
    (let [schema
          (document/schema-document "agents")

          agent
          {"session_id" "child"
           "parent_id" "parent"
           "leader_id" "parent"
           "team_id" "task"
           "task" "Inspect the existing tests"
           "status" "queued"
           "iteration_budget" 32
           "iterations_used" 0}]

      (expect (= agents/default-iterations
                 (get-in schema ["$defs" "spawn" "properties" "iteration_budget" "default"])
                 120))
      (expect (= agents/max-team-children (get schema "x-vis-max-team-children") 128))
      (expect (= agents/max-active-children (get schema "x-vis-max-active-children") 16))
      (expect (document/valid? "agents" agent))
      (expect (document/valid? "agents" (assoc agent "note" "Reading the parser")))
      (expect (not (document/valid? "agents" (assoc agent "note" ""))))
      (expect (not (document/valid? "agents" (assoc agent "iteration_budget" 601))))
      (expect (not (document/valid? "agents" (assoc agent "iteration_budget" -1))))
      (expect (document/valid-json? "agents" "spawn" {"task" (apply str (repeat 65536 "x"))}))
      (expect (document/valid-json? "agents"
                                    "spawn"
                                    {"task" "Edit one owner"
                                     "files" {"write" ["src/a/**"] "deny" [".env"]}}))
      (expect (document/valid-json? "agents" "spawn" {"task" "x" "files" {}}))
      (expect (not (document/valid-json? "agents" "spawn" {"task" "x" "files" {"exec" ["a"]}})))
      (expect (document/valid-json? "agents"
                                    "spawn"
                                    {"task" "x" "files" {"write" ["a"] "read_only" [] "deny" []}}))
      (expect (document/valid-json? "agents" "grant" {"session_id" "c" "files" {"write" ["a"]}}))
      (expect (not (document/valid-json? "agents" "grant" {"session_id" "c"})))
      (expect (document/valid-json? "agents" "ask_files" {"files" {"write" ["a"]} "reason" "Fix"}))
      (expect (not
                (document/valid-json? "agents" "ask_files" {"files" {"write" ["a"]} "reason" " "})))
      (expect (not
                (document/valid-json? "agents" "spawn" {"task" (apply str (repeat 65537 "x"))}))))))

(defn child!
  [db parent opts]
  (let [turn (ps/db-store-session-turn!
               db
               {:parent-session-id parent :user-request "Parent task" :status :running})]
    (str (h/fork-session-at-turn! db
                                  parent
                                  {:through-turn-id turn
                                   :agent (merge {:parent_id (str parent)
                                                  :leader_id (str parent)
                                                  :team_id (str turn)
                                                  :task "Read existing evidence"
                                                  :iteration_budget 4
                                                  :spawn_key (str turn)
                                                  :spawn_fingerprint "fixture"
                                                  :checkpoint [{:role :user
                                                                :content "Full current context"}]}
                                                 opts)}))))

;; The leader computed whole answers itself and spawned children only to confirm them. The rules
;; follow the Codex and Claude Code delegation rules and the Vis subagent evaluation.

(defdescribe
  subagent-role-test
  (it "accepts a contract role and refuses another one"
      (expect (document/valid-json? "agents" "spawn" {"task" "Fix" "role" "implement"}))
      (expect (document/valid-json? "agents" "run" {"tasks" [{"task" "Read" "role" "research"}]}))
      (expect (not (document/valid-json? "agents" "spawn" {"task" "Fix" "role" "boss"})))
      (expect (not (document/valid-json? "agents" "run" {"tasks" [{"task" "Fix" "role" "boss"}]}))))
  (it "stores the role with the child, and a child without a role has no role key"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {:role "verify"})

            plain
            (child! db leader {})]

        (expect (= "verify" (:role (agents/info db child))))
        (expect (not (contains? (agents/info db plain) :role)))
        (expect (= ["verify"] (keep :role (ps/db-agent-list db leader)))))))

(defdescribe
  managed-lineage-and-context-test
  (it
    "managed lineage and context"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          other
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {})

          metadata
          (agents/info db child)

          env
          {:db-info db :session-id child :turn-state-atom (atom {})}]

      (expect (= leader (:parent_id metadata)))
      (expect (= "subagent" (get (agents/context env) "role")))
      (expect (= "leader" (get (agents/context {:db-info db :session-id other}) "role")))
      ;; One system prompt serves every channel; this flag tells the model nobody answers.
      (expect (true? (get (agents/context {:db-info db :session-id other}) "interactive")))
      (expect (false? (get (agents/context {:db-info db :session-id other :channel :cli})
                           "interactive")))
      (expect (agents/wake-allowed? db leader child))
      (expect (agents/wake-allowed? db child leader))
      (expect (agents/wake-allowed? db child child))
      (expect (not (agents/wake-allowed? db other child)))
      (expect (agents/wake-allowed? db leader other))
      (expect (not (agents/wake-allowed? db child other)))
      ;; #202: only a managed subagent wakes itself; an independent leader stays idle.
      (expect (not (agents/wake-allowed? db leader leader)))
      (expect (not (agents/wake-allowed? db other other)))
      (expect (= [{:role :user :content "Full current context"}
                  {:role "user" :content ";; -- CURRENT-SUBAGENT-CONTEXT --\nChild block"}]
                 (agents/stable-prefix env "Child block")))
      (expect (nil? (agents/stable-prefix {:db-info db :session-id other} "Leader block")))
      (expect (empty? (ps/db-list-session-turns db child)) "A child copies no parent turn")
      (expect (not (some #(= child (str (:id %))) (ps/db-list-sessions db :all))))
      (expect (agents/controls? db leader child))
      (expect (agents/controls? db child child))
      (expect (not (agents/controls? db child leader))))))

;; Regression: a subagent sent its own session id as the OpenAI prompt cache key. Its first
;; request held the leader's whole prefix but missed the leader's warm cache (0% cache read).
(defdescribe team-shares-the-leader-cache-key-test
             (it "gives every subagent of a team the leader's id as its prompt cache key"
                 (let [db
                       (h/store)

                       leader
                       (str (h/store-session! db {:channel :api}))

                       other
                       (str (h/store-session! db {:channel :api}))

                       child
                       (child! db leader {})]

                   (expect (= leader (agents/leader-id db leader)))
                   (expect (= leader (agents/leader-id db child)))
                   (expect (= other (agents/leader-id db other))
                           "An independent session keeps its own key")
                   (expect (nil? (agents/leader-id db nil))))))

;; A subagent may not ask the person: its leader answers its questions and decides its grants.
(defdescribe person-answers-test
             (it "answers for a leader and an independent session, never for a managed subagent"
                 (let [db
                       (h/store)

                       leader
                       (str (h/store-session! db {:channel :api}))

                       other
                       (str (h/store-session! db {:channel :api}))

                       child
                       (child! db leader {})]

                   (expect (true? (agents/person-answers? db leader)))
                   (expect (true? (agents/person-answers? db other)))
                   (expect (false? (agents/person-answers? db child)))
                   (expect (false? (agents/person-answers? db (parse-uuid child))))
                   (expect (true? (agents/person-answers? nil child))))))

;; ctb edits2-vis (openai, 20 trials): every first request read 0 of about 4.4k prefix
;; tokens from the cache, because each one-shot session sent its own key. A workspace key
;; lets a new session reach the warm cache of earlier sessions in the same workspace.
(defdescribe
  workspace-prompt-cache-key-test
  (it "gives every session of one workspace the same opaque key"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            other
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {})

            key-of
            (fn [sid root]
              (agents/prompt-cache-key {:db-info db :session-id sid :workspace/root root}))

            k
            (key-of leader "/work/app")]

        (expect (str/starts-with? k "vis-ws-"))
        (expect (= 39 (count k)))
        (expect (not (str/includes? k "work")) "The raw path never leaves the host")
        (expect (= k (key-of other "/work/app") (key-of child "/work/app")))
        (expect (not= k (key-of leader "/work/other")))))
  (it "falls back to the leader id without a workspace root"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {})]

        (expect (= leader (agents/prompt-cache-key {:db-info db :session-id child})))
        (expect (= leader
                   (agents/prompt-cache-key {:db-info db :session-id child :workspace/root ""}))))))

(defdescribe
  durable-budget-cancellation-and-router-test
  (it
    "durable budget cancellation and router"
    (let [db
          (h/store)

          leader
          (h/store-session! db {:channel :api})

          child
          (child! db leader {:iteration_budget 1 :allowed_models [{:provider "p" :model "small"}]})

          env
          {:db-info db :session-id child}

          router
          {:providers [{:id :p :root "large" :models [{:name "small"} {:name "large"}]}
                       {:id :other :root "small" :models [{:name "small"}]}]}]

      (expect (= [{:id :p :root "small" :models [{:name "small"}]}]
                 (:providers (agents/restrict-router env router))))
      (expect (= 2 (count (:providers router))) "The shared router was not mutated")
      (expect (agents/claim-iteration! env))
      (expect (agents/wake-allowed? db child leader)
              "The final paid iteration can return its result")
      (expect (not (agents/claim-iteration! env)))
      (expect (= "budget_limited" (:status (agents/info db child))))
      (expect
        (=
          "Subagent stopped: it used its full iteration budget of 1 and its handoff iteration. Its parent can add iterations with council.extend and continue it with a Council ping."
          (agents/stop-reason env)))
      (expect (not (agents/wake-allowed? db leader child)))
      (expect (agents/wake-allowed? db child leader)
              "Exhaustion can be reported without rerunning the child")
      (agents/finish! db child "completed")
      (expect (= "budget_limited" (:status (agents/info db child))))
      (smodel/set-model! db leader "p" "small")
      (expect (ps/db-routing-locked? db leader))
      (smodel/set-model! db leader nil nil)
      (expect (not (ps/db-routing-locked? db leader))))))

(defdescribe
  rate-limited-child-test
  (it
    "a 429 ends the child turn and its record names the working routes"
    (let [db
          (h/store)

          leader
          (h/store-session! db {:channel :api})

          child
          (child! db leader {})

          router
          {:rate-limit {:same-provider-delays-ms [2000]}
           :providers [{:id :busy :root "large" :models [{:name "large"}]}
                       {:id :spare :root "small" :models [{:name "small"}]}]}

          turn
          {:created-at (java.util.Date.)
           :duration-ms 10
           :provider :busy
           :model "large"
           :content [{"type" "error"
                      "code" "provider_rate_limit"
                      "title" "Provider rate-limited"
                      "provider" "busy"}]}

          refusal
          (fn [opts]
            (try (#'agents/check-route! {:router router} opts) nil (catch Exception e e)))]

      (expect (= {:same-provider-delays-ms [2000] :fail-fast? true :fallback-provider? false}
                 (:rate-limit (agents/restrict-router {:db-info db :session-id child} router)))
              "A child gets no same-provider retry and no automatic fallback")
      (reset! @#'agents/rate-limited {})
      (try (with-redefs [ps/db-list-session-turns
                         (fn [_ _]
                           [turn])

                         auth-health/cooled
                         (fn []
                           #{})]

             (let [rec (#'agents/record
                        db
                        router
                        (constantly true)
                        (constantly false)
                        (System/currentTimeMillis)
                        {:session_id child :status "failed"})]
               (expect (= {:code "rate_limited"
                           :message "Provider rate-limited"
                           :provider "busy"
                           :model "large"}
                          (:error rec)))
               (expect (= [{:provider "spare" :model "small"}] (:alternatives rec)))
               (expect (document/valid-json? "agents"
                                             "record"
                                             (walk/stringify-keys
                                               (into {} (remove (comp nil? val)) rec)))))
             (expect (= :rate-limited (:error (ex-data (refusal {:provider "busy"})))))
             (expect (str/includes? (ex-message (refusal {:provider "busy"})) "spare/small"))
             (expect (nil? (refusal {:provider "spare"}))))
           (finally (reset! @#'agents/rate-limited {}))))))

(defn- with-fake-runtime
  "Call `(f entries)` with a runtime whose Council lives in memory. A reply to an entry is
   visible through `ps/db-council-get`, as in the gateway. `entries` is the atom of the
   published entries. Restores the gateway runtime, and registers the Council ops for
   direct calls."
  [db leader live f]
  (let [entries
        (atom {})

        saved
        @@#'agents/runtime

        registered?
        (some #(= "foundation-core" (:ext/name %)) (extension/registered-extensions))]

    (when-not registered? (foundation/register!))
    (agents/install-runtime!
      {:live? (fn [sid]
                (contains? @live (str sid)))
       :list (fn [_ _]
               (ps/db-agent-list db leader))
       ;; The gateway also resolves and checks the rules; here they stay as given.
       :grant (fn [_ {:keys [session_id files replace]}]
                (let [old (:files (ps/db-agent-info db session_id))]
                  (ps/db-agent-update!
                    db
                    session_id
                    {:files
                     (if replace files (merge-with #(vec (distinct (concat %1 %2))) old files))})))
       :publish (fn [env opts]
                  (let [id (inc (count @entries))]
                    (swap! entries assoc id (assoc opts :author (str (:session-id env))))
                    (when-let [to (:reply_to opts)]
                      (swap! entries update-in [to :replies] (fnil conj []) {:reply_entry_id id}))
                    {:entry_id id :replies [{:state "delivered"}]}))})
    (try (f entries)
         (finally (agents/install-runtime! saved)
                  (when-not registered? (extension/deregister-extension! "foundation-core"))))))

(defn- outcome [f] (try (:result (f)) (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))

(defn- asking-record
  "Poll the record of `child` until it asks, for at most 5 s."
  [env child]
  (loop [n 0]
    (let [r (first (:result (agents/wait-agents env [child] {:timeout_secs 1})))]
      (if (or (= "asking" (get r "status")) (> n 50)) r (do (Thread/sleep 100) (recur (inc n)))))))

;; Issue #388: a child had 64 iterations, no warning and a hard stop without a handoff. A
;; parent could not add iterations, so the only option was a new child without the context.
(defdescribe
  subagent-budget-test
  (it
    "warns the child near the end of its budget"
    (let [db
          (h/store)

          leader
          (h/store-session! db {:channel :api})

          child
          (child! db leader {:iteration_budget 40})

          env
          {:db-info db :session-id child}]

      (expect (nil? (get (agents/context env) "budget_notice")))
      (dotimes [_ 32]
        (agents/claim-iteration! env))
      (expect
        (=
          "8 of 40 iterations left. Finish the task, or call council.ask_budget(iterations, reason) for more. Before the budget ends, report the done work, the changed files and the next step."
          (get (agents/context env) "budget_notice")))
      (dotimes [_ 8]
        (agents/claim-iteration! env))
      (expect (re-find #"^Your iteration budget is used\. Your next response is your handoff\."
                       (get (agents/context env) "budget_notice")))))
  (it "gives one handoff iteration after the budget and then stops"
      (let [db
            (h/store)

            leader
            (h/store-session! db {:channel :api})

            child
            (child! db leader {:iteration_budget 1})

            env
            {:db-info db :session-id child :turn-state-atom (atom {})}]

        (expect (agents/claim-iteration! env))
        (expect (agents/claim-iteration! env) "The handoff iteration runs without a claim")
        (expect (= 1 (:iterations_used (agents/info db child))))
        (expect (not= "budget_limited" (:status (agents/info db child))))
        (expect (not (agents/claim-iteration! env)) "Only one handoff iteration")
        (expect (= "budget_limited" (:status (agents/info db child))))
        (expect (re-find #"council\.extend" (agents/stop-reason env)))
        (expect (re-find #"Last report:\nParser fixed\.$"
                         (agents/refusal-answer env "Parser fixed.")))
        (expect (re-find #"no report\. Read its work with read_session\(\""
                         (agents/refusal-answer env nil))
                "A handoff without prose still tells the parent where the work is")))
  (it "marks a child budget_limited only when its turn ends in the handoff iteration"
      (let [db
            (h/store)

            leader
            (h/store-session! db {:channel :api})

            done
            (child! db leader {:iteration_budget 1})

            cut
            (child! db leader {:iteration_budget 1})

            cut-env
            {:db-info db :session-id cut :turn-state-atom (atom {})}]

        (agents/claim-iteration! {:db-info db :session-id done :turn-state-atom (atom {})})
        (agents/finish! db done "completed")
        (expect (= "completed" (:status (agents/info db done)))
                "An answer on the last claimed iteration is a finished task")
        (agents/claim-iteration! cut-env)
        (agents/claim-iteration! cut-env)
        (agents/finish! db cut "completed")
        (expect (= "budget_limited" (:status (agents/info db cut))))))
  (it
    "extends a budget within the limits and keeps the child budget_limited until it continues"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          other
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:iteration_budget 1})

          env
          {:db-info db :session-id leader}]

      (with-fake-runtime
        db
        leader
        (atom #{})
        (fn [_]
          (let [child-env {:db-info db :session-id child :turn-state-atom (atom {})}]
            (agents/claim-iteration! child-env)
            (agents/claim-iteration! child-env))
          (agents/finish! db child "completed")
          (expect (not (agents/wake-allowed? db leader child)))
          (expect (= {"status" "budget_limited" "iteration_budget" 6 "iterations_used" 1}
                     (select-keys (first (outcome #(agents/extend-agent env child 5)))
                                  ["status" "iteration_budget" "iterations_used"])))
          (expect (agents/wake-allowed? db leader child) "A ping continues an extended child")
          ;; The follow-up ping runs the child again.
          (ps/db-agent-update! db child {:status "running"})
          (expect (= :budget-exceeded (outcome #(agents/extend-agent env child 595))))
          (expect (= :invalid-request (outcome #(agents/extend-agent env child 0))))
          (expect (= :not-owner
                     (outcome #(agents/extend-agent {:db-info db :session-id other} child 1))))
          (expect (= :not-owner
                     (outcome #(agents/extend-agent {:db-info db :session-id child} child 1))))
          (ps/db-agent-update! db child {:status "cancelled"})
          (expect (= 7
                     (get (first (outcome #(agents/extend-agent env child 1))) "iteration_budget"))
                  "A cancelled child can get iterations before a resume")))))
  (it
    "lets a child ask for iterations and the parent grant or deny them"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:iteration_budget 4})

          env
          {:db-info db :session-id leader}

          child-env
          {:db-info db :session-id child}]

      (with-fake-runtime
        db
        leader
        (atom #{leader})
        (fn [entries]
          (with-redefs [ps/db-council-get (fn [_ id]
                                            (get @entries id))]
            (let [asked (future (outcome #(agents/ask-budget child-env
                                                             10
                                                             "The suite has 40 files"
                                                             {:timeout_secs 10})))
                  r (asking-record env child)]

              (expect (= {"iterations" 10 "reason" "The suite has 40 files"}
                         (get r "budget_request")))
              (expect (= "Budget request: 10 more iterations. Reason: The suite has 40 files"
                         (get r "question")))
              (expect (= (get r "question") (:question (agents/open-request child)))
                      "The team list shows the open question")
              (outcome #(agents/extend-agent env child 10))
              (expect (= {"granted" 10
                          "iteration_budget" 14
                          "reply" "Granted 10 more iterations. New budget: 14."}
                         (deref asked 5000 ::timeout))))
            (let [asked (future
                          (outcome
                            #(agents/ask-budget child-env 50 "More tests" {:timeout_secs 10})))]
              (asking-record env child)
              (expect (nil? (get (first (outcome
                                          #(agents/deny-agent env child "Stop at the parser.")))
                                 "budget_request"))
                      "The answered request leaves the record")
              (let [denied (deref asked 5000 ::timeout)]
                (expect (= 0 (get denied "granted")))
                (expect (re-find #"^Denied: Stop at the parser\." (get denied "reply")))))
            (expect (= :invalid-request (outcome #(agents/deny-agent env child "No request"))))
            ;; A person answers the question of a child for its parent (the Questions inbox).
            (let [asked (future (outcome
                                  #(agents/ask child-env "Which parser?" {:timeout_secs 10})))]
              (asking-record env child)
              (expect (nil? (get (first (agents/operation! env
                                                           :answer
                                                           {:session_id child
                                                            :text "Use the new parser."}))
                                 "question"))
                      "The answered question leaves the record")
              (expect (= "Use the new parser." (deref asked 5000 ::timeout))))
            (expect (= :invalid-request
                       (outcome
                         #(agents/operation! env :answer {:session_id child :text "Late"})))))
          (expect (= :invalid-request (outcome #(agents/ask-budget child-env 601 "Too many"))))
          (expect (= :invalid-request (outcome #(agents/ask-budget env 5 "Not a child"))))))))
  (it
    "lets a child ask for files and the parent grant or deny them"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:files {:write ["/w/a/**"]}})

          sibling
          (child! db leader {:files {:write ["/w/b/**"]}})

          env
          {:db-info db :session-id leader}

          child-env
          {:db-info db :session-id child}

          delivered
          (atom [])]

      (ps/db-agent-update! db sibling {:status "running"})
      (with-fake-runtime
        db
        leader
        (atom #{leader})
        (fn [entries]
          (with-redefs [ps/db-council-get
                        (fn [_ id]
                          (get @entries id))

                        ps/db-council-delivered!
                        (fn [_ sid ids]
                          (swap! delivered conj [sid ids]))]

            (let [asked
                  (future (outcome #(agents/ask-files child-env
                                                      {"write" ["/w/b/x.txt"]}
                                                      "The fix touches b"
                                                      {:timeout_secs 10})))

                  r
                  (asking-record env child)]

              (expect (= {"files" {"write" ["/w/b/x.txt"]} "reason" "The fix touches b"}
                         (get r "files_request")))
              (expect (= "File request: write /w/b/x.txt. Reason: The fix touches b"
                         (get r "question")))
              (let [granted (first (outcome
                                     #(agents/grant-files env child {"write" ["/w/b/x.txt"]})))]
                (expect (= ["/w/a/**" "/w/b/x.txt"] (get-in granted ["files" "write"])))
                ;; A real gateway run showed the answered request still in the grant record.
                (expect (nil? (get granted "files_request"))
                        "The returned record no longer shows the answered request")
                (expect (str/includes? (get granted "warning") sibling)
                        "A sibling that writes the same files gets a warning"))
              (let [answer (deref asked 5000 ::timeout)]
                (expect (true? (get answer "granted")))
                (expect (= ["/w/a/**" "/w/b/x.txt"] (get-in answer ["files" "write"])))
                (expect (=
                          "Granted: write /w/b/x.txt. Your sandbox restarts before your next block."
                          (get answer "reply")))
                ;; A real run delivered each reply twice: as the result and as Council input.
                (expect (= child (ffirst @delivered))
                        "The child's call consumes the reply, so Council input skips it")))
            (let [asked (future (outcome #(agents/ask-files child-env
                                                            {"write" ["/w/c"]}
                                                            "More"
                                                            {:timeout_secs 10})))]
              (asking-record env child)
              (outcome #(agents/deny-agent env child "Use a/ only"))
              (let [denied (deref asked 5000 ::timeout)]
                (expect (false? (get denied "granted")))
                (expect (= "Denied: Use a/ only. Continue without these files."
                           (get denied "reply"))
                        "A reason without a final period still reads as a sentence")))
            ;; A real gateway run granted another file and ask_files still answered granted.
            (let [asked (future (outcome #(agents/ask-files child-env
                                                            {"write" ["/w/a/c/y.txt" "/w/d.txt"]}
                                                            "Two files"
                                                            {:timeout_secs 10})))]
              (asking-record env child)
              (outcome #(agents/grant-files env child {"write" ["/w/e.txt"]}))
              (let [partial (deref asked 5000 ::timeout)]
                (expect (false? (get partial "granted"))
                        "A grant that misses a requested file is not a grant")
                (expect (= ["/w/d.txt"] (get partial "missing"))
                        "A granted glob covers /w/a/c/y.txt; only /w/d.txt is missing"))))
          (expect (= :invalid-request
                     (outcome #(agents/ask-files env {"write" ["/w/x"]} "Not a child"))))
          (expect (= :invalid-request (outcome #(agents/ask-files child-env {} "Empty"))))
          (expect (= :not-owner
                     (outcome #(agents/grant-files child-env child {"write" ["/w/x"]}))))))))
  ;; Subagent records and the child context showed whole absolute grant rules. They now show
  ;; rules under the workspace root as relative and other rules under home as `~/…`.
  (it "shows grant rules relative to the workspace root, or under ~"
      (let [home
            (System/getProperty "user.home")

            env
            {:workspace/root "/w/"}

            shown
            (#'agents/shown-files
             env
             {:write ["/w/src/a/**" "/w"]
              :read_only [(str home "/other/b.md")]
              :deny ["/etc/hosts"]})]

        (expect (= {:write ["src/a/**" "."] :read_only ["~/other/b.md"] :deny ["/etc/hosts"]}
                   shown))
        (expect (nil? (#'agents/shown-files env nil)))
        (expect (= "~/other/b.md" (#'agents/shown-rule nil (str home "/other/b.md"))))))
  ;; Review of #388: a budget request in the handoff iteration used that iteration. After a
  ;; refusal, the child stopped without a report.
  (it "gives one more handoff iteration once after a refused request in the handoff"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {:iteration_budget 1})

            child-env
            {:db-info db :session-id child :turn-state-atom (atom {})}]

        (with-fake-runtime
          db
          leader
          (atom #{})
          (fn [_]
            (expect (agents/claim-iteration! child-env))
            (expect (agents/claim-iteration! child-env) "The handoff iteration")
            (expect (= 0 (get (outcome #(agents/ask-budget child-env 10 "More files")) "granted")))
            (expect (agents/claim-iteration! child-env) "A refusal renews the handoff")
            (outcome #(agents/ask-budget child-env 10 "More files"))
            (expect (not (agents/claim-iteration! child-env)) "Only one renewal in a turn")
            (expect (= "budget_limited" (:status (agents/info db child))))))))
  (it
    "gives a finished child a follow-up task through a ping and never returns its old answer"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:iteration_budget 4})

          env
          {:db-info db :session-id leader}

          turns
          (atom [{:id "old" :content [{"type" "prose" "markdown" "Old answer"}]}])]

      (with-fake-runtime
        db
        leader
        (atom #{})
        (fn [_]
          (with-redefs [ps/db-list-session-turns (fn [_ sid]
                                                   (if (= child (str sid)) @turns []))]
            (agents/finish! db child "completed")
            (expect (= "Old answer"
                       (get (first (:result (agents/wait-agents env [child] {:timeout_secs 1})))
                            "answer")))
            ;; The wake of a ping from the parent queues the child, as state/council-wake! does.
            (expect (fn? (agents/queue-turn! db child)))
            (expect (= 1 (:resumed_turns (agents/info db child)))
                    "The stale-answer guard is stored with the child, so a restart keeps it")
            ;; The gateway stores the status before the turn, so the old turn is still the last.
            (agents/finish! db child "completed")
            (expect (nil? (get (first (:result (agents/wait-agents env [child] {:timeout_secs 1})))
                               "answer")))
            (swap! turns conj {:id "new" :content [{"type" "prose" "markdown" "New answer"}]})
            (expect (= "New answer"
                       (get (first (:result (agents/wait-agents env [child] {:timeout_secs 1})))
                            "answer"))))))))
  ;; council.resume is gone: a ping is the one follow-up verb, so it refuses a task that cannot start.
  (it
    "refuses a follow-up ping to an exhausted or stopped child and lets other pings through"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          other
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:iteration_budget 2})

          saved
          @@#'agents/runtime]

      (try (swap! @#'agents/runtime dissoc :leader-active?)
           (agents/finish! db child "completed")
           (expect (nil? (agents/follow-up-refusal db leader child))
                   "A finished child with iterations left takes the task")
           (dotimes [_ 2]
             (ps/db-agent-claim-iteration! db child))
           (ps/db-agent-update! db child {:status "budget_limited"})
           (expect (= :budget-exceeded (first (agents/follow-up-refusal db leader child))))
           (expect (str/includes? (second (agents/follow-up-refusal db leader child))
                                  "council.extend")
                   "The refusal names the next call")
           (expect (nil? (agents/follow-up-refusal db other child))
                   "Only the parent or the leader gives a follow-up task")
           (expect (nil? (agents/follow-up-refusal db child leader))
                   "A ping to the leader is no follow-up task")
           (expect (some? (ps/db-agent-extend-budget! db child 2)))
           (expect (nil? (agents/follow-up-refusal db leader child)))
           (ps/db-agent-update! db child {:status "running"})
           (expect (nil? (agents/follow-up-refusal db leader child))
                   "A running child reads the ping inside its turn")
           (ps/db-agent-update! db child {:status "cancelled"})
           (expect (= :invalid-recipient (first (agents/follow-up-refusal db leader child)))
                   "A late ping cannot restart a child that a stop cancelled")
           (swap! @#'agents/runtime assoc :leader-active? #(= leader %))
           (expect (nil? (agents/follow-up-refusal db leader child))
                   "A running leader turn continues a cancelled child")
           (finally (reset! @#'agents/runtime saved)))))
  ;; #414: files on a follow-up ping replace the grant before the wake, so the task starts in scope.
  (it
    "replaces the grant of a pinged child before its wake and restores it when the ping fails"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {})

          env
          {:db-info db :session-id leader}

          live
          (atom #{})

          ping
          {:content "Next task" :ping [child]}

          refused
          (fn [opts publish!]
            (try (agents/publish-with-files! env opts publish!)
                 nil
                 (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))]

      (with-fake-runtime
        db
        leader
        live
        (fn [_]
          (agents/finish! db child "completed")
          (ps/db-agent-update! db child {:files {:read_only ["docs"]}})
          (let [seen
                (atom nil)

                entry
                (agents/publish-with-files! env
                                            (assoc ping :files {"write" ["out.txt"]})
                                            (fn [opts]
                                              (reset! seen (:files (agents/info db child)))
                                              {:entry_id 1 :opts opts}))]

            (expect (= {:write ["out.txt"]} @seen) "The child has the new grant at its wake")
            (expect (= 1 (:entry_id entry))))
          (expect (= {:write ["out.txt"]} (:files (agents/info db child))))
          (expect (= :boom
                     (refused (assoc ping :files {:write ["b.txt"]})
                              (fn [_]
                                (throw (ex-info "No entry" {:error :boom}))))))
          (expect (= {:write ["out.txt"]} (:files (agents/info db child)))
                  "A failed publication restores the grant")
          (agents/publish-with-files! env
                                      (assoc ping
                                        :reply_required true
                                        :files {:write ["c.txt"]})
                                      (fn [_]
                                        {:entry_id 2 :replies [{:state "unavailable"}]}))
          (expect (= {:write ["out.txt"]} (:files (agents/info db child)))
                  "A child that cannot start keeps its grant")
          (expect (= ping (agents/publish-with-files! env ping identity))
                  "Without files, the ping passes unchanged")
          (doseq [target [[child leader] "all" [leader]]]
            (expect (= :invalid-request
                       (refused (assoc ping
                                  :ping target
                                  :files {:write ["d.txt"]})
                                identity))
                    (pr-str target)))
          (swap! live conj child)
          (expect (= :invalid-request (refused (assoc ping :files {:write ["d.txt"]}) identity))
                  "A running child gets more files with council.grant")
          (expect (= {:write ["out.txt"]} (:files (agents/info db child))))))))
  ;; A follow-up ping passes the active cap: only a new spawn counts against it.
  (it
    "queues a pinged child past the active cap and undoes it on demand"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {})]

      (with-redefs [ps/db-list-session-turns (fn [_ sid]
                                               (if (= child (str sid)) [{:id "t1"}] []))]
        (agents/finish! db child "completed")
        (dotimes [_ agents/max-active-children]
          (child! db leader {}))
        (let [restore! (agents/queue-turn! db child)]
          (expect (fn? restore!))
          (expect (= {:status "queued" :resumed_turns 1}
                     (select-keys (agents/info db child) [:status :resumed_turns]))
                  "The stale-answer guard also holds for a ping")
          (expect (some? (:resumed_at (agents/info db child))))
          (expect (fn? (agents/queue-turn! db child)) "A queued child stays as is")
          (expect (= 1 (:resumed_turns (agents/info db child))))
          (restore!)
          (expect (= {:status "completed" :resumed_turns 0}
                     (select-keys (agents/info db child) [:status :resumed_turns]))))
        (dotimes [_ (:iteration_budget (agents/info db child))]
          (ps/db-agent-claim-iteration! db child))
        (ps/db-agent-update! db child {:status "budget_limited"})
        (expect (not (agents/wake-allowed? db leader child)) "An exhausted child stays idle")
        (expect (nil? (agents/queue-turn! db child)) "A child without iterations stays idle")
        (expect (some? (ps/db-agent-extend-budget! db child 2)))
        (expect (agents/wake-allowed? db leader child)
                "A ping continues an extended budget_limited child")
        (expect (fn? (agents/queue-turn! db child)))
        (expect (= "queued" (:status (agents/info db child))))
        (ps/db-agent-update! db child {:status "cancelled"})
        (let [saved @@#'agents/runtime]
          (try (swap! @#'agents/runtime dissoc :leader-active?)
               (expect (not (agents/wake-allowed? db leader child))
                       "A late ping cannot restart a child that a stop cancelled")
               (expect (nil? (agents/queue-turn! db child)))
               (swap! @#'agents/runtime assoc :leader-active? #(= leader %))
               (expect (agents/wake-allowed? db leader child)
                       "A ping from a running leader continues a cancelled child")
               (expect (fn? (agents/queue-turn! db child)))
               (expect (= "queued" (:status (agents/info db child))))
               (finally (reset! @#'agents/runtime saved))))))))

;; Regression: Subagents was on only in the leader's session settings. A child starts without
;; those overrides, so its turn snapshot read the default and refused its first iteration.
(defdescribe
  leader-session-setting-governs-its-team-test
  (it
    "admits and stops children with the leader's session setting"
    (with-redefs [config/load-global-yaml-config-raw
                  (constantly {})

                  config/load-global-config-raw
                  (constantly {})

                  config/load-project-tiers-raw
                  (constantly {})

                  config/load-project-config-raw
                  (constantly {})]

      (toggles/set-value! "subagents" false)
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {})

            env
            {:db-info db :session-id child}

            ;; A turn binds the settings of its own session, as `loop.turn/turn!` does.
            in-turn
            (fn [sid f]
              (binding [toggles/*overrides* (scoped/values db sid)]
                (f)))]

        (ps/db-set-scoped-setting! db "session" leader "subagents" true)
        (expect (not (contains? (ps/db-scoped-settings db "session" child) "subagents"))
                "A child starts without the session overrides of its leader")
        (expect (nil? (agents/require-enabled! db leader))
                "A gateway route outside a turn reads the session setting")
        (expect (in-turn child #(agents/claim-iteration! env)))
        (expect (in-turn child #(agents/wake-allowed? db child leader))
                "The child can return its outcome to an idle leader")
        (expect (in-turn leader #(agents/wake-allowed? db leader child)))
        (expect (in-turn child
                         #(and (nil? (agents/require-enabled! db child))
                               (every? (fn [entry]
                                         ((:ext.symbol/active-fn entry) env))
                                       agents/symbols)
                               (agents/prompt env)))
                "The child keeps its Council tools, its prompt and its spawn permission")
        (ps/db-set-scoped-setting! db "session" leader "subagents" false)
        (expect (not (in-turn child #(agents/claim-iteration! env)))
                "Off in the leader's session stops further child iterations")
        (expect
          (=
            "Subagent stopped: Subagents are disabled for this team. Enable Subagents in Experimental settings."
            (in-turn child #(agents/stop-reason env))))
        (expect (not (in-turn leader #(agents/wake-allowed? db leader child))))))))

(defdescribe
  gateway-spawn-is-idempotent-and-owned-test
  (it
    "gateway spawn is idempotent and owned"
    (doseq [grouped? [false true]]
      (let [db (h/store)
            leader (str (h/store-session! db {:channel :api}))
            project-id (str (:id (ps/db-create-project! db {:name "Agent team"})))
            group-id (when grouped?
                       (str (:id (ps/db-create-session-group! db project-id {:name "Research"}))))
            gid (or group-id project-id)
            router {:providers [{:id :p :root "small" :models [{:name "small"} {:name "large"}]}]}
            checkpoint [{:role :system :content "Rules"} {:role :user :content "Parent request"}
                        {:role :assistant
                         :content [{:type "text" :text "Note"}
                                   {:type "tool_use" :id "t1" :name "python_execution" :input {}}]}
                        {:role :user
                         :content [{:type "tool_result" :tool_use_id "t1" :content "Tool output"}]}
                        {:role :assistant :content "Answer"}
                        {:role :user :content "Current task and folded evidence"}]
            handoff [{:role :system :content "Rules"} {:role :user :content "Parent request"}
                     {:role :assistant :content "Note\n\nAnswer"}
                     {:role :user :content "Current task and folded evidence"}
                     (peek (agents/handoff [{:role :user :content "Any"}]))]
            env {:db-info db
                 :session-id leader
                 :router router
                 :turn-state-atom (atom {:agent-checkpoint checkpoint
                                         :council {:activation-id "parent"}})
                 ;; A disabled jail: a grant passes the parent's write roots check.
                 :security-policy {:process-jail {}}}
            launches (atom [])]

        (ps/db-set-session-project! db leader project-id)
        ;; Managed children must join an explicit Council group before delegation.
        (when group-id (ps/db-set-session-group! db leader group-id))
        (ps/db-store-session-turn!
          db
          {:parent-session-id leader :user-request "Parent task" :status :running})
        (with-redefs-fn {#'lp/db-info (constantly db)
                         #'loop-router/get-router (constantly router)
                         #'toggles/enabled? (constantly true)
                         #'council/runtime (fn [_]
                                             {leader {:activation-id "parent" :group-id gid}})
                         (ns-resolve 'com.blockether.vis.internal.gateway.state 'live-env)
                         (constantly env)
                         (ns-resolve 'com.blockether.vis.internal.council.core 'runtime-waker)
                         (atom {:eligible? (constantly true)
                                :wake! (fn [_ sid _]
                                         (swap! launches conj sid))})}
          (fn []
            (let [opts {:task "Check the isolated test owner; report evidence" :key "tests"}
                  child (gateway/agents-operation! leader :spawn opts)
                  sid (:session_id child)]

              (expect (= [sid] @launches))
              (expect (= handoff (ps/db-agent-checkpoint db sid))
                      "A child gets the parent's conversation without its tool work by default")
              (expect (= project-id (str (:project-id (ps/db-get-session db sid)))))
              (expect (= group-id
                         (some-> (:group-id (ps/db-get-session db sid))
                                 str)))
              (expect (= gid (council/session-group db (ps/db-get-session db sid))))
              (expect (= sid (:session_id (gateway/agents-operation! leader :spawn opts))))
              (expect (= [sid] @launches))
              (expect (= :idempotency-conflict
                         (try (gateway/agents-operation! leader
                                                         :spawn
                                                         (assoc opts :task "Different task"))
                              nil
                              (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
              ;; #405: a spawn outside its own model allowlist starts no child.
              (expect (= :model-not-allowed
                         (try (gateway/agents-operation! leader
                                                         :spawn
                                                         {:task "Review on the large model"
                                                          :key "outside"
                                                          :provider "p"
                                                          :model "large"
                                                          :allowed_models [{:provider "p"
                                                                            :model "small"}]})
                              nil
                              (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
              (expect (= [sid] @launches))
              ;; #405: a caller key names one delegation across parent turns.
              (ps/db-store-session-turn!
                db
                {:parent-session-id leader :user-request "Next parent task" :status :running})
              (expect (= sid (:session_id (gateway/agents-operation! leader :spawn opts))))
              (expect (= [sid] @launches))
              ;; A child on any model gets the handoff, and the spawn takes no context option.
              (let [other {:task "Review on another model" :provider "p" :model "large"}
                    task-child (:session_id (gateway/agents-operation! leader :spawn other))]

                (expect (= handoff (ps/db-agent-checkpoint db task-child)))
                (expect (= :invalid-request
                           (try
                             (gateway/agents-operation! leader :spawn (assoc other :context "full"))
                             nil
                             (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
                ;; A file grant resolves against the parent's root and stays on the record.
                (let [grant {:task "Edit one owner" :files {:write ["src/owner/**"] :deny [".env"]}}
                      owner (:session_id (gateway/agents-operation! leader :spawn grant))
                      files (:files (ps/db-agent-info db owner))]

                  (expect (str/ends-with? (first (:write files)) "/src/owner/**"))
                  (expect (str/starts-with? (first (:write files)) "/"))
                  (expect (str/ends-with? (first (:deny files)) "/.env"))
                  (expect (nil? (:read_only files)))
                  ;; council.grant adds rules, keeps the old ones and replaces the live jail.
                  (pj/own-grants! owner ::live-env)
                  (try (let [granted (-> (gateway/agents-operation! leader
                                                                    :grant
                                                                    {:session_id owner
                                                                     :files {:write ["docs/**"]}})
                                         first
                                         walk/keywordize-keys)]
                         (expect (= [(first (:write files))
                                     (str/replace (first (:write files)) "src/owner/**" "docs/**")]
                                    (get-in granted [:files :write])))
                         (expect (= (:deny files) (get-in granted [:files :deny])))
                         (expect (= (get-in granted [:files :write])
                                    (:write-rules (pj/live-jail owner nil))))
                         (expect (pj/take-stale-worker! owner))
                         (expect (not (pj/take-stale-worker! owner))))
                       (finally (pj/release-grants! owner ::live-env)))
                  ;; #414: a child without write rules already writes where its parent writes,
                  ;; so a write grant changes nothing and says so.
                  (let [granted (-> (gateway/agents-operation! leader
                                                               :grant
                                                               {:session_id sid
                                                                :files {:write ["docs/**"]}})
                                    first
                                    walk/keywordize-keys)]
                    (expect (nil? (:files granted)))
                    (expect (nil? (:files (ps/db-agent-info db sid))))
                    (expect (str/includes? (:warning granted) "No change for")))
                  ;; #414: a read_only rule of the child wins over a write grant and names the
                  ;; resume call that gives a new task its own files.
                  (let [auditor (:session_id (gateway/agents-operation!
                                               leader
                                               :spawn
                                               {:task "Audit only" :files {:read_only ["**"]}}))
                        refusal (try (gateway/agents-operation! leader
                                                                :grant
                                                                {:session_id auditor
                                                                 :files {:write ["docs/a.md"]}})
                                     nil
                                     (catch clojure.lang.ExceptionInfo e (ex-message e)))]

                    (expect (str/includes? refusal "/docs/a.md (read_only "))
                    (expect (str/includes? refusal
                                           "council.publish(task, ping=[session_id], files="))
                    (expect (not (str/includes? refusal "every file"))))
                  ;; Only the parent grants: a child cannot widen its own access.
                  (expect (= :not-owner
                             (try (gateway/agents-operation! owner
                                                             :grant
                                                             {:session_id owner
                                                              :files {:write ["/tmp/x"]}})
                                  nil
                                  (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
                  (with-redefs [security-policy/outside-write-rules (fn [_ rules _]
                                                                      (vec rules))]
                    (expect (str/includes? (try (gateway/agents-operation!
                                                  leader
                                                  :grant
                                                  {:session_id owner
                                                   :files {:write ["/elsewhere"]}})
                                                nil
                                                (catch clojure.lang.ExceptionInfo e (ex-message e)))
                                           "files.write cannot grant /elsewhere"))
                    ;; A real gateway run listed only ~/.vis, not the workspace that the parent writes.
                    (with-redefs-fn {#'gateway/resolve-workspace (fn [_ _]
                                                                   {:root "/project"})}
                      #(expect (re-find #"Writable roots: /project"
                                        (try (gateway/agents-operation!
                                               leader
                                               :grant
                                               {:session_id owner :files {:write ["/elsewhere"]}})
                                             nil
                                             (catch clojure.lang.ExceptionInfo e (ex-message e))))
                               "The error names the workspace root that the parent can write"))))
                ;; A grant never widens the parent's policy: the spawn names the problem.
                (with-redefs [security-policy/outside-write-rules (fn [_ rules _]
                                                                    (vec rules))]
                  (expect (str/includes?
                            (try (gateway/agents-operation! leader
                                                            :spawn
                                                            {:task "Write outside"
                                                             :files {:write ["/elsewhere"]}})
                                 nil
                                 (catch clojure.lang.ExceptionInfo e (ex-message e)))
                            "files.write cannot grant /elsewhere: the parent cannot write there")))
                ;; Only a leader delegates: a child asks its parent for more subagents.
                (expect (= :not-allowed
                           (try (gateway/agents-operation! task-child :spawn {:task "Nested"})
                                nil
                                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
                ;; The task message names the parent session whose tool work the child cannot see.
                (let [delegated (->> (ps/db-council-page db gid nil false 0 100)
                                     (filter #(= "Delegated task" (:title %)))
                                     (map :content))
                      hint (str "When the task is ambiguous or needs a fact from that tool output, "
                                "read it: s = await read_session(\""
                                leader
                                "\")")]

                  (expect (= 4 (count (filter #(str/includes? % hint) delegated))))))
              (expect (= "cancelled"
                         (get (first (gateway/agents-operation! leader :cancel {:session_id sid}))
                              "status")))
              (expect (not (agents/claim-iteration! {:db-info db :session-id sid})))
              (expect (= "cancelled" (:status (agents/info db sid))))
              (expect (= "Subagent stopped: it is cancelled."
                         (agents/stop-reason {:db-info db :session-id sid}))))))))))

(defdescribe
  handoff-test
  (describe
    "the conversation that a task-context child inherits"
    (it "keeps system messages, requests, gists, notes and answers, and drops tool work"
        (expect
          (= [{:role "system" :content [{:type "text" :text "Rules"}]}
              {:role "user" :content "Request"} {:role "assistant" :content "Plan\n\nAnswer"}
              {:role "user" :content "# ⋯ folded t1/* · Gist\n\nNext request"}]
             (pop (agents/handoff
                    [{:role "system" :content [{:type "text" :text "Rules"}]}
                     {:role "user" :content "Request"}
                     {:role "assistant"
                      :content [{:type "thinking" :thinking "Hidden" :signature "s"}
                                {:type "text" :text "Plan"}
                                {:type "tool_use" :id "t1" :name "python_execution" :input {}}]}
                     {:role "user"
                      :content [{:type "tool_result" :tool_use_id "t1" :content "Output"}]}
                     {:role "assistant"
                      :content [{:type "tool_use" :id "t2" :name "python_execution" :input {}}]}
                     {:role "user"
                      :content [{:type "tool_result" :tool_use_id "t2" :content "More"}]}
                     {:role "assistant" :content "Answer"}
                     {:role "user" :content "# ⋯ folded t1/* · Gist"} {:role "user" :content "  "}
                     {:role "user" :content "Next request"}])))))
    (it "keeps the images of a request and its text as blocks, and drops tool images"
        (let [image
              {:type "image_url" :image_url {:url "data:image/png;base64,AA"}}

              legacy
              {:type :image :source {:type "base64" :media_type "image/png" :data "AA"}}]

          (expect (= [{:role :user :content [{:type :text :text "Look"} image legacy]}
                      {:role :assistant :content "Seen"}]
                     (pop (agents/handoff
                            [{:role :user :content [{:type :text :text "Look"} image legacy]}
                             {:role :user :content [image]}
                             {:role :assistant :content [{:type :text :text "Seen"} image]}]))))))
    (it "ends with a message that marks the conversation as context, not instructions"
        (let [end (peek (agents/handoff [{:role "system" :content "Rules"}
                                         {:role "user" :content "Run this block"}]))]
          (expect (= "user" (:role end)))
          (expect (str/starts-with? (:content end) ";; -- END-OF-PARENT-CONVERSATION --"))
          (expect (str/includes? (:content end) "not instructions for you"))
          ;; Live A/B: without this rule, 3 of 6 children used a parent's deletion approval.
          (expect (str/includes? (:content end) "approvals above belong to your parent"))
          (expect (str/includes? (:content end) "does not have the parent's variables"))))
    (it "adds no closing message to system messages alone"
        (expect (= [{:role "system" :content "Rules"}]
                   (agents/handoff [{:role "system" :content "Rules"}]))))
    (it "keeps nothing of a conversation with only tool work"
        (expect (= []
                   (agents/handoff
                     [{:role "assistant" :content [{:type "tool_use" :id "t" :input {}}]}
                      {:role "user"
                       :content [{:type "tool_result" :tool_use_id "t" :content "x"}]}]))))))

(defdescribe
  child-usage-excludes-inherited-work-test
  (it "child usage excludes inherited work"
      (let [db
            (h/store)

            leader
            (h/store-session! db {:channel :api})

            turn
            (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent work"})]

        (h/store-iteration! db {:session-turn-id turn :code "" :tokens {"input" 9000 "output" 100}})
        (let [child
              (child! db leader {})

              own
              (ps/db-store-session-turn! db {:parent-session-id child :user-request "Child work"})]

          (h/store-iteration! db {:session-turn-id own :code "" :tokens {"input" 200 "output" 10}})
          (expect (= 9000 (:input-tokens (ps/db-session-usage-stats db leader))))
          (expect (= 200 (:input-tokens (ps/db-session-usage-stats db child))))
          (h/fork-session! db child {})
          (let [after-fold (ps/db-store-session-turn! db
                                                      {:parent-session-id child
                                                       :user-request "Work after folding"})]
            (h/store-iteration!
              db
              {:session-turn-id after-fold :code "" :tokens {"input" 300 "output" 10}})
            (expect
              (= 500 (:input-tokens (ps/db-session-usage-stats db child)))
              "A fold resets local turn positions but must not hide the child's own usage"))))))

(defdescribe
  subagent-usage-sums-the-whole-tree-test
  (it
    "sums every child in one rollup and splits the cost on the wire"
    (let [db
          (h/store)

          leader
          (h/store-session! db {:channel :api})

          turn
          (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent work"})]

      (h/store-iteration!
        db
        {:session-turn-id turn :code "" :tokens {"input" 9000 "output" 100} :cost-usd 1.0})
      (expect (nil? (ps/db-session-subagent-usage db leader)))
      (let [children (conj (vec (repeatedly 99 #(child! db leader {:status "completed"})))
                           (child! db leader {}))]
        (doseq [sid children]
          (let [own (ps/db-store-session-turn! db {:parent-session-id sid :user-request "Work"})]
            (h/store-iteration!
              db
              {:session-turn-id own :code "" :tokens {"input" 100 "output" 10} :cost-usd 0.25})))
        (expect (= {:agent-count 100
                    :active-count 1
                    :iteration-count 100
                    :input-tokens 10000
                    :output-tokens 1000
                    :cost-usd 25.0}
                   (ps/db-session-subagent-usage db leader))
                "Inherited parent work is not counted again")
        (expect (nil? (ps/db-session-subagent-usage db (first children)))
                "A child has no subagents")
        (with-redefs [lp/db-info (constantly db)]
          (let [usage (gateway/session-usage-info leader)]
            (expect (= 1.0 (get usage "cost_usd")))
            (expect (= 26.0 (get usage "total_cost_usd")))
            (expect (= 100 (get-in usage ["subagents" "agent_count"])))
            (expect (= 25.0 (get-in usage ["subagents" "cost_usd"]))))
          (expect (not (contains? (gateway/session-usage-info (first children)) "subagents"))))))))

(defdescribe
  terminal-outcomes-report-to-the-persisted-leader-test
  (it "terminal outcomes report to the persisted leader"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {:iteration_budget 1})

            published
            (atom [])]

        (expect (agents/claim-iteration! {:db-info db :session-id child}))
        (expect (not (agents/claim-iteration! {:db-info db :session-id child})))
        (with-redefs [council/enabled?
                      (constantly true)

                      council/publish!
                      (fn [_ _ author opts]
                        (swap! published conj [author opts]))]

          (#'gateway/report-agent-outcome! db child "turn" {:content [{"text" "Verified result"}]})
          (expect (= [leader] (:ping (second (first @published)))))
          (expect (re-find #"budget_limited" (:content (second (first @published)))))
          (expect (re-find #"Verified result" (:content (second (first @published)))))
          (expect (= "agent-result:turn" (:idempotency_key (second (first @published)))))
          (ps/db-agent-update! db child {:status "cancelled"})
          (#'gateway/report-agent-outcome! db child "stopped" {})
          (expect (= [] (:ping (second (last @published)))))))))

;; Regression: a gateway that died mid-team left its children queued or running forever,
;; and a resumed leader waited for children that no process ran any more.
(defdescribe
  orphaned-children-reconcile-test
  (it
    "fails queued and running children that no live process runs"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          dead
          (child! db leader {})

          queued
          (child! db leader {})

          done
          (child! db leader {})

          live
          (child! db leader {})

          other-leader
          (str (h/store-session! db {:channel :api}))

          spared
          (child! db other-leader {})]

      (ps/db-agent-update! db dead {:status "running"})
      (ps/db-agent-update! db done {:status "completed"})
      (ps/db-agent-update! db live {:status "running"})
      (ps/db-agent-update! db spared {:status "running"})
      (expect (= 2 (agents/reconcile-orphans! db [live other-leader])))
      (expect (= "failed" (:status (agents/info db dead))))
      (expect (= "failed" (:status (agents/info db queued))))
      (expect (= "completed" (:status (agents/info db done))))
      (expect (= "running" (:status (agents/info db live))) "A child with a live turn stays")
      (expect (= "running" (:status (agents/info db spared))) "A live leader still owns its team")
      (expect (= 0 (agents/reconcile-orphans! db [live other-leader]))
              "A second sweep finds nothing"))))

;; User review: a wait record never says lost. A child without a live turn settles from the
;; last turn of its task, and a failure names why the child stopped.
(defdescribe
  child-without-live-turn-test
  (it
    "settles from the last task turn and names why the child stopped"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          later
          (+ (System/currentTimeMillis) 60000)

          rec
          (fn [sid turns]
            (with-redefs [ps/db-list-session-turns (fn [_ id]
                                                     (if (= sid (str id)) turns []))]
              (#'agents/record
               db
               nil
               (constantly false)
               (constantly false)
               later
               (agents/info db sid))))]

      (let [done
            (child! db leader {})

            r
            (rec done [{:id "t" :status :success :content [{"type" "prose" "markdown" "Done"}]}])]

        (expect (= "completed" (:status r)))
        (expect (= "Done" (:answer r)))
        (expect (= "completed" (:status (agents/info db done))) "The store keeps the end"))
      (let [never
            (child! db leader {})

            r
            (rec never [])]

        (expect (= "failed" (:status r)))
        (expect (= "not_started" (get-in r [:error :code])))
        (expect (document/valid-json? "agents"
                                      "record"
                                      (walk/stringify-keys (into {} (remove (comp nil? val)) r)))))
      (let [cut
            (child! db leader {})

            r
            (rec cut [{:id "t" :status :running}])]

        (expect (= "failed" (:status r)))
        (expect (= "interrupted" (get-in r [:error :code]))))
      (let [young
            (child! db leader {})

            r
            (#'agents/record
             db
             nil
             (constantly false)
             (constantly false)
             (System/currentTimeMillis)
             (agents/info db young))]

        (expect (= "queued" (:status r)) "A new child waits for its first turn"))
      (let [real (child! db leader {})]
        (expect (= "not_started"
                   (get-in (#'agents/record
                            db
                            nil
                            (constantly false)
                            (constantly false)
                            later
                            (agents/info db real))
                           [:error :code]))
                "A new child has no turn of its own")))))

(defdescribe
  stopping-a-parent-arms-its-backstop-before-stopping-children-test
  (it
    "stopping a parent arms its backstop before stopping children"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {})

          token
          (cancellation/cancellation-token)

          child-token
          (cancellation/cancellation-token)

          armed
          (atom [])

          registry
          (atom {leader {:current-turn "parent-turn"
                         :turns {"parent-turn" {:status "running" :cancel-token token}}}
                 child {:current-turn "child-turn"
                        :turns {"child-turn" {:status "running" :cancel-token child-token}}}})]

      (with-redefs-fn {#'lp/db-info (constantly db)
                       #'gateway/registry registry
                       #'gateway/start-cancel-terminal-backstop!
                       (fn [sid & _]
                         (when (= sid child)
                           (expect
                             (cancellation/cancelled? token)
                             "The parent cannot run another iteration while a child stop blocks"))
                         (swap! armed conj sid))}
        (fn []
          (expect (= {:status "cancelling"} (gateway/cancel-turn! leader "parent-turn")))
          (expect (= [leader child] @armed))
          (expect (cancellation/cancelled? token))
          (expect (cancellation/cancelled? child-token))
          (expect (= "cancelled" (:status (agents/info db child))))
          (expect (not (#'gateway/agent-leader-active? leader))
                  "A stopped leader cannot continue its cancelled children")
          (swap! registry assoc-in
            [leader :turns "next-turn"]
            {:status "running" :cancel-token (cancellation/cancellation-token)})
          (expect (#'gateway/agent-leader-active? leader)
                  "The next leader turn can continue them"))))))

(defdescribe
  inactive-and-incomplete-delegations-do-not-allocate-children-test
  (it
    "inactive and incomplete delegations do not allocate children"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          router
          {:providers [{:id :p :root "small" :models [{:name "small"}]}]}

          turn-state
          (atom {:council {:activation-id "active"}})

          runtime
          (atom {})

          env
          {:db-info db :session-id leader :router router :turn-state-atom turn-state}

          refusal
          (fn []
            (try (gateway/agents-operation! leader :spawn {:task "Check evidence"})
                 (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))]

      (ps/db-store-session-turn!
        db
        {:parent-session-id leader :user-request "Parent" :status :running})
      (with-redefs-fn {#'lp/db-info (constantly db)
                       #'loop-router/get-router (constantly router)
                       #'council/enabled? (constantly true)
                       #'council/runtime (fn [_]
                                           @runtime)
                       (ns-resolve 'com.blockether.vis.internal.gateway.state 'live-env) (constantly
                                                                                           env)}
        (fn []
          (expect (= :inactive-session (refusal)))
          (reset! runtime {leader {:activation-id "active"}})
          (expect (= :no-checkpoint (refusal)))
          (swap! turn-state assoc :agent-checkpoint [{:role :user :content "Current context"}])
          (reset! runtime {leader {:activation-id "different"}})
          (expect (= :inactive-session (refusal)))
          (expect (empty? (ps/db-agent-list db leader))))))))

(defdescribe
  persisted-team-and-active-limits-test
  (it
    "persisted team and active limits"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          turn
          (ps/db-store-session-turn! db {:parent-session-id leader :user-request "Parent"})

          fork
          (fn [n]
            (str (h/fork-session-at-turn! db
                                          leader
                                          {:through-turn-id turn
                                           :agent {:parent_id leader
                                                   :leader_id leader
                                                   :team_id (str turn)
                                                   :task "Check evidence"
                                                   :iteration_budget 1
                                                   :spawn_key (str n)
                                                   :spawn_fingerprint (str n)
                                                   :checkpoint [{:role :user
                                                                 :content "Current context"}]}})))

          refusal
          (fn [n]
            (try (fork n) (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))]

      ;; The store uses the contract limits, not its own copies.
      (dotimes [n agents/max-active-children]
        (fork n))
      (expect (= :agent-limit (refusal agents/max-active-children)))
      (expect (= agents/max-active-children (count (ps/db-agent-list db leader))))
      ;; The agent contract allows files only as an object, so a child without a grant has no key.
      (expect (not-any? #(contains? % :files) (ps/db-agent-list db leader)))
      (let [[idle] (map :session_id (ps/db-agent-list db leader))]
        (ps/db-agent-update! db idle {:status "completed"})
        (fork agents/max-active-children)
        (expect (agents/claim-iteration! {:db-info db :session-id idle})
                "A woken child can pass the active cap; only a new spawn counts against it")
        (expect (= {:status "running" :iterations_used 1}
                   (select-keys (agents/info db idle) [:status :iterations_used]))))
      (doseq [child (ps/db-agent-list db leader)]
        (ps/db-agent-update! db (:session_id child) {:status "completed"}))
      (doseq [n (range (inc agents/max-active-children) agents/max-team-children)]
        (ps/db-agent-update! db (fork n) {:status "completed"}))
      (expect (= :agent-limit (refusal agents/max-team-children)))
      (expect (= agents/max-team-children (count (ps/db-agent-list db leader)))))))

;; Regression, issue #405: an extension's extend or deny of an open budget request
;; failed with inactive-session, because the reply had no live parent execution.
(defdescribe agent-answers-use-the-live-parent-env
             (it "extend and deny run in the parent's live env; reads do not need it"
                 (let [seen (atom [])]
                   (with-redefs-fn {#'lp/db-info (constantly ::db)
                                    #'ps/db-get-session (constantly {:id "parent"})
                                    (ns-resolve 'com.blockether.vis.internal.gateway.state
                                                'live-env)
                                    (constantly {:live? true})
                                    #'agents/operation! (fn [env operation _]
                                                          (swap! seen conj [operation (:live? env)])
                                                          [])}
                     (fn []
                       (doseq [operation [:extend :deny :wait :list]]
                         (gateway/agents-operation! "parent" operation {}))
                       (expect (= [[:extend true] [:deny true] [:wait nil] [:list nil]] @seen)))))))

;; Regression for #405: a cancel of a finished child must keep its outcome.
(defdescribe cancel-of-a-finished-child-test
             (it "answers a finished child unchanged and cancels nothing"
                 (let [db
                       (h/store)

                       leader
                       (str (h/store-session! db {:channel :api}))

                       env
                       {:db-info db :session-id leader}]

                   (doseq [status ["completed" "failed" "cancelled"]]
                     (let [child (child! db leader {})]
                       (agents/finish! db child status)
                       (with-redefs [lp/db-info (constantly db)]
                         (expect (= {:session_id child :status status :cancelled []}
                                    (#'gateway/agent-cancel env {:session_id child}))
                                 status))
                       (expect (= status (:status (agents/info db child))) status))))))

;; Regression for #405: a human hold of a child's queue ends the wait with status held.
(defdescribe wait-reports-a-held-child-test
             (it "reports held for a queued child whose queue a human holds"
                 (let [db
                       (h/store)

                       leader
                       (str (h/store-session! db {:channel :api}))

                       child
                       (child! db leader {:status "queued"})]

                   (with-redefs [lp/db-info (constantly db)]
                     (try (#'gateway/put-session!
                           child
                           {:next-seq 0 :queue-paused true :turns {"t1" {:status "queued"}}})
                          (let [record (first (:result (agents/wait-agents {:db-info db
                                                                            :session-id leader}
                                                                           [child]
                                                                           {:timeout_secs 5})))]
                            (expect (= "held" (get record "status")) (pr-str record)))
                          (finally (#'gateway/drop-session! child)))))))

;; Council orchestration study: a blocking wait idled on finished work and returned only status
;; and iteration counts. A wait now returns at the first event with a progress digest.
(defdescribe
  event-wait-test
  (let [now
        #(java.util.Date.)

        team
        (fn []
          (let [db
                (h/store)

                leader
                (str (h/store-session! db {:channel :api}))]

            {:db db
             :leader leader
             :env {:db-info db :session-id leader}
             :done (child! db leader {:status "running" :iteration_budget 10})
             :busy (child! db leader {:status "running" :iteration_budget 10})}))

        answer
        (fn [done]
          (fn [_ sid]
            (if (= done (str sid))
              [{:id "t" :content [{"type" "prose" "markdown" "Done answer"}]}]
              [])))

        by-id
        (fn [records]
          (into {} (map (juxt #(get % "session_id") identity)) records))]

    (it
      "returns when the first child ends and shows only new progress notes"
      (let [{:keys [db leader env done busy]}
            (team)

            live
            (atom #{done busy})

            turn
            (str (random-uuid))]

        (with-fake-runtime
          db
          leader
          live
          (fn [_]
            (with-redefs [ps/db-list-session-turns
                          (answer done)

                          ps/db-list-session-turns-meta
                          (fn [_ sid]
                            (if (= busy (str sid)) [{:id turn :position 0 :created-at (now)}] []))

                          ps/db-list-session-turn-iterations
                          (fn [_ id]
                            (if (= turn (str id))
                              [{:position 0 :assistant-prose "Reading the parser" :created-at (now)}
                               {:position 1
                                :assistant-prose "The bug is in tokenize"
                                :created-at (now)}]
                              []))]

              (let [started
                    (System/currentTimeMillis)

                    ender
                    (future (Thread/sleep 1500)
                            (agents/finish! db done "completed")
                            (swap! live disj done))

                    records
                    (by-id (:result (agents/wait-agents env [done busy] {:timeout_secs 30})))

                    elapsed
                    (- (System/currentTimeMillis) started)]

                @ender
                (expect (< elapsed 15000) (str "The wait ended at the first child: " elapsed " ms"))
                (expect (= ["completed" "Done answer"]
                           [(get-in records [done "status"]) (get-in records [done "answer"])]))
                (expect (= "running" (get-in records [busy "status"])))
                (expect (= ["Reading the parser" "The bug is in tokenize"]
                           (get-in records [busy "notes"])))
                (expect (integer? (get-in records [busy "step_secs"])))
                (expect (integer? (get-in records [busy "elapsed_secs"])))
                (expect (nil? (get (first (:result
                                            (agents/wait-agents env [busy] {:timeout_secs 1})))
                                   "notes"))
                        "A second check-in shows only new notes")))))))
    (it
      "returns when a running child publishes a Council message"
      (let [{:keys [db leader env busy]}
            (team)

            live
            (atom #{busy})

            turn
            (str (random-uuid))

            steps
            (atom [{:position 0 :assistant-prose "Reading the parser" :created-at (now)}])]

        (with-fake-runtime
          db
          leader
          live
          (fn [_]
            (with-redefs [ps/db-list-session-turns-meta
                          (fn [_ sid]
                            (if (= busy (str sid)) [{:id turn :position 0 :created-at (now)}] []))

                          ps/db-list-session-turn-iterations
                          (fn [_ id]
                            (if (= turn (str id)) @steps []))

                          ps/db-council-get
                          (fn [_ id]
                            (when (= 7 id) {:entry_id 7 :content "tokenize drops tabs"}))]

              (let [publisher
                    (future (Thread/sleep 1500)
                            (swap! steps conj
                              {:position 1 :council-publications [{:entry_id 7}] :created-at (now)})
                            (agents/claim-iteration! {:db-info db :session-id busy}))

                    started
                    (System/currentTimeMillis)

                    record
                    (first (:result (agents/wait-agents env [busy] {:timeout_secs 30})))]

                @publisher
                (expect (< (- (System/currentTimeMillis) started) 15000))
                (expect (= [{"entry_id" 7 "content" "tokenize drops tabs"}] (get record "messages"))
                        (pr-str record))))))))
    (it "does not end the wait for a child that ended before it"
        (let [{:keys [db leader env done busy]}
              (team)

              live
              (atom #{done busy})]

          (with-fake-runtime
            db
            leader
            live
            (fn [_]
              (with-redefs [ps/db-list-session-turns (answer done)]
                (agents/finish! db done "completed")
                (swap! live disj done)
                (let [started (System/currentTimeMillis)
                      records (by-id (:result
                                       (agents/wait-agents env [done busy] {:timeout_secs 2})))]

                  (expect (>= (- (System/currentTimeMillis) started) 1500))
                  (expect (= "running" (get-in records [busy "status"])))
                  (expect (not (contains? (get records busy) "notes")))))))))))

;; Council orchestration study: the leader gets event waits and independent checks, and each
;; subagent reports with evidence.

;; Regression for #405: only a finished child with iterations left takes a follow-up task.
(defdescribe
  follow-up-of-a-running-or-cancelled-child-test
  (it "lets a running child read the ping, and refuses a cancelled child without a leader turn"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            saved
            @@#'agents/runtime]

        (try (swap! @#'agents/runtime dissoc :leader-active?)
             (doseq [[status refusal] [["running" nil] ["cancelled" :invalid-recipient]]]
               (let [child (child! db leader {:status status})]
                 (expect (= refusal (first (agents/follow-up-refusal db leader child))) status)
                 (expect (= {:status status :resumed_turns 0}
                            (select-keys (agents/info db child) [:status :resumed_turns]))
                         status)))
             (finally (reset! @#'agents/runtime saved))))))

;; Regression for Council complaint #7930: council.run spawned four children, then refused the
;; fifth task for empty file rule lists. The error named no field, so the model guessed again.
(defdescribe
  run-checks-every-task-before-the-first-spawn-test
  (it "starts no child for a bad task and names the field and its rule"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            spawned
            (atom 0)

            tasks
            [{"task" "Compute a hash" "iteration_budget" 20}
             {"task" "Write a file" "files" {"write" ["/tmp/out.txt"] "exec" ["/tmp/run"]}}]

            error
            (with-redefs [agents/runtime-op! (fn [_ _ _]
                                               (swap! spawned inc)
                                               {:session_id "child"})]
              (try (agents/run {:db-info db :session-id leader} tasks)
                   nil
                   (catch clojure.lang.ExceptionInfo e e)))]

        (expect (= 0 @spawned) "No child starts before every task is valid")
        (expect (= :invalid-request (:error (ex-data error))))
        (expect (str/includes? (ex-message error)
                               "tasks[1].files: additional property \"exec\" is not allowed"))
        (expect (str/includes? (ex-message error) "No subagent started."))
        (expect (not (str/includes? (ex-message error) "anyOf")))))
  (it "names an unknown task key and the missing task"
      (let [error (try (agents/run {:db-info (h/store) :session-id "leader"}
                                   [{"question" "Count files"}])
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
        (expect (str/includes? (ex-message error) "tasks[0]: additional property \"question\""))
        (expect (str/includes? (ex-message error) "missing required property \"task\"")))))

;; Council complaint #7930: a model gave "read_only": [] and "deny": [], and the schema refused them.
(defdescribe file-grants-accept-empty-rule-lists-test
             (it "drops empty lists from a task grant and refuses a grant or request without rules"
                 (let [db
                       (h/store)

                       leader
                       (str (h/store-session! db {:channel :api}))

                       env
                       {:db-info db :session-id leader}

                       spawn
                       (fn [files]
                         (#'agents/checked env :spawn {"task" "Write a file" "files" files}))]

                   (expect (= {:write ["out.txt"]}
                              (:files (spawn {"write" ["out.txt"] "read_only" [] "deny" []}))))
                   (expect (not (contains? (spawn {"write" [] "read_only" []}) :files)))
                   (expect (not (contains? (spawn {}) :files)))
                   (doseq [[operation opts] [[:grant {"session_id" "child" "files" {"write" []}}]
                                             [:ask_files
                                              {"files" {"deny" []} "reason" "Need a file"}]]]
                     (expect (= "Give at least one write, read_only or deny rule in files."
                                (try (#'agents/checked env operation opts)
                                     nil
                                     (catch clojure.lang.ExceptionInfo e (ex-message e))))
                             (name operation))))))

;; Subagent views: people see a short title, a one-line result and a timeline of each child.
(defdescribe
  child-title-result-and-timeline-test
  (it "accepts a one-line title of 1 to 40 characters and a one-line result"
      (expect (document/valid-json? "agents" "spawn" {"task" "x" "title" "Fix login test"}))
      (expect (document/valid-json? "agents" "run" {"tasks" [{"task" "x" "title" "Audit"}]}))
      (doseq [bad ["" " " (apply str (repeat 41 "x")) "Two\nlines"]]
        (expect (not (document/valid-json? "agents" "spawn" {"task" "x" "title" bad})) bad))
      (expect (document/valid-json? "agents" "result" {"text" "All pass" "outcome" "passed"}))
      (expect (not (document/valid-json? "agents" "result" {"text" "All pass" "outcome" "maybe"})))
      (expect (not (document/valid-json? "agents" "result" {"text" "a\nb"}))))
  (it
    "stores council.result and else the first line of the answer, and a resume clears it"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          reported
          (child! db leader {:title "Run the suite" :role "verify"})

          answer
          (fn [sid text]
            (with-redefs [ps/db-list-session-turns (fn [_ id]
                                                     (if (= sid (str id))
                                                       [{:id "t"
                                                         :status :success
                                                         :content [{"type" "prose"
                                                                    "markdown" text}]}]
                                                       []))]
              (agents/finish! db sid "completed")))]

      (expect (= "Run the suite" (:title (agents/info db reported))))
      (expect (= :invalid-request
                 (outcome #(agents/report-result {:db-info db :session-id leader} "Done")))
              "Only a subagent reports a result")
      (expect (= :invalid-request
                 (outcome #(agents/report-result {:db-info db :session-id reported}
                                                 "Done"
                                                 {"outcome" "maybe"}))))
      (agents/report-result {:db-info db :session-id reported}
                            " 42 tests pass "
                            {"outcome" "passed"})
      (answer reported "Long answer\nwith details")
      (let [row (agents/info db reported)]
        (expect (= ["completed" "42 tests pass" "passed"] ((juxt :status :result :outcome) row)))
        (expect (pos-int? (:finished_at row))))
      (let [plain (child! db leader {})]
        (expect (nil? (:title (agents/info db plain))))
        (answer plain "## **Found the cause**\n\nThe parser drops a newline.")
        (expect (= "Found the cause" (:result (agents/info db plain)))))
      (ps/db-agent-resume! db reported 1 (System/currentTimeMillis))
      (let [row (agents/info db reported)]
        (expect (= "queued" (:status row)))
        (expect (every? #(not (contains? row %)) [:result :outcome :finished_at])))))
  (it
    "lists the turns, notes, messages and end of a child, oldest first"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:title "Check docs"})

          t0
          (:created_at (agents/info db child))]

      (agents/report-result {:db-info db :session-id child} "Docs are current")
      (with-redefs [ps/db-list-session-turns
                    (constantly [])

                    ps/db-list-session-turns-meta
                    (fn [_ _]
                      [{:id "old" :created-at (- t0 60000)} {:id "t1" :created-at (+ t0 10)}
                       {:id "t2" :created-at (+ t0 50)}])

                    ps/db-list-session-turn-iterations
                    (fn [_ turn-id]
                      (case turn-id
                        "t1"
                        [{:position 1
                          :created-at (+ t0 30)
                          :finished-at (+ t0 40)
                          :assistant-prose "I read the docs."
                          :council-publications [{:entry_id 7}]}
                         {:position 0 :created-at (+ t0 20) :assistant-prose "I start."}]

                        "t2"
                        [{:position 0 :created-at (+ t0 60) :assistant-prose " "}]

                        []))

                    ps/db-council-get
                    (fn [_ id]
                      (when (= 7 id) {:entry_id 7 :content "A finding"}))]

        (agents/finish! db child "completed")
        (let [view (agents/timeline db leader child)]
          (expect (= [["started" nil] ["note" "I start."] ["note" "I read the docs."]
                      ["message" "A finding"] ["continued" nil] ["finished" "Docs are current"]]
                     (mapv (juxt :kind :text) (:items view))))
          (expect (= "completed" (:status (peek (:items view)))))
          (expect (document/valid-json? "agents" "timeline" (walk/stringify-keys view)))
          (expect (= :session-not-found (outcome #(agents/timeline db child child)))
                  "Only the parent reads the timeline of its child"))))))

;; The team rows show what a running child does now: its newest progress note.
(defdescribe
  latest-note-test
  (it
    "gives the newest note of the current task, clipped, and ignores older tasks"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:title "Check docs"})

          row
          (agents/info db child)

          t0
          (:created_at row)

          note-of
          (fn [turns iterations]
            (with-redefs [ps/db-list-session-turns-meta
                          (fn [_ _]
                            turns)

                          ps/db-list-session-turn-iterations
                          (fn [_ turn-id]
                            (get iterations turn-id []))]

              (agents/latest-note db row (+ t0 100))))

          old
          {:id "old" :created-at (- t0 60000)}

          current
          {:id "t1" :created-at (+ t0 10)}]

      (expect (= "I read the docs."
                 (note-of [old current]
                          {"old" [{:position 0 :assistant-prose "Old task"}]
                           "t1" [{:position 1 :assistant-prose "I read the docs."}
                                 {:position 2 :assistant-prose " "}
                                 {:position 0 :assistant-prose "I start."}]})))
      (expect (nil? (note-of [old] {"old" [{:position 0 :assistant-prose "Old task"}]}))
              "A note of an older task never shows")
      (expect (nil? (note-of [] {})))
      (expect (= (str (apply str (repeat 300 "a")) "…")
                 (note-of [current]
                          {"t1" [{:position 0 :assistant-prose (apply str (repeat 400 "a"))}]}))))))

;; A subagent question that only the person can decide: the leader ends its turn suspended to ask
;; the person. The child keeps waiting for the reply of the next leader turn.
(defdescribe
  child-question-waits-for-person-test
  (it
    "keeps the question open while the parent waits for the person"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:iteration_budget 4})

          env
          {:db-info db :session-id leader}

          child-env
          {:db-info db :session-id child}

          live
          (atom #{leader})

          suspended?
          (atom false)]

      (with-fake-runtime
        db
        leader
        live
        (fn [entries]
          (agents/install-runtime! (assoc @@#'agents/runtime
                                     :awaits-person? (fn [sid]
                                                       (and @suspended? (= leader (str sid))))))
          (with-redefs [ps/db-council-get (fn [_ id]
                                            (get @entries id))]
            (let [asked (future (outcome
                                  #(agents/ask child-env "Keep the old API?" {:timeout_secs 20})))]
              (asking-record env child)
              (expect (= [{:session_id child :entry_id (:entry_id (agents/open-request child))}]
                         (agents/open-questions db leader))
                      "The leader sees the open question of its child")
              (expect (= [] (agents/open-questions db child)))
              ;; The leader turn ends suspended: no turn runs, but the person decides.
              (reset! suspended? true)
              (reset! live #{})
              (expect (= ::waiting (deref asked 2500 ::waiting))
                      "The child waits while the parent asks the person")
              ;; The person answers; the next leader turn replies to the child.
              (reset! suspended? false)
              (reset! live #{leader})
              (agents/operation! env :answer {:session_id child :text "Keep it."})
              (expect (= "Keep it." (deref asked 5000 ::timeout))))
            (let [asked (future (outcome #(agents/ask child-env "Again?" {:timeout_secs 20})))]
              (asking-record env child)
              ;; The person cancels the next leader turn instead of an answer.
              (reset! live #{})
              (expect (nil? (deref asked 5000 ::timeout))
                      "A parent that neither runs nor waits for the person ends the wait"))
            (reset! live #{leader})
            (reset! suspended? true)
            (let [asked (future (outcome #(agents/ask child-env "Last?" {:timeout_secs 20})))]
              (asking-record env child)
              (reset! live #{})
              (ps/db-agent-update! db child {:status "cancelled"})
              (expect
                (nil? (deref asked 5000 ::timeout))
                "A cancelled child ends the wait also while the parent asks the person"))))))))
