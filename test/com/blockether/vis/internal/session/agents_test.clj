(ns com.blockether.vis.internal.session.agents-test
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [com.blockether.vis.contract.document :as document]
            [com.blockether.vis.contract.wire :as wire]
            [com.blockether.vis.internal.gateway.wiring :as wiring]
            [com.blockether.vis.internal.session.agents :as agents]
            [com.blockether.vis.internal.task :as task]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.core :as foundation]
            [com.blockether.vis.internal.session.model :as smodel]
            [com.blockether.vis.internal.persistance.core :as ps]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.provider.auth-health :as auth-health]
            [com.blockether.vis.internal.persistance.sqlite.test-helpers :as h]
            [com.blockether.vis.internal.gateway.state :as gateway]
            [com.blockether.vis.internal.loop :as lp]
            [com.blockether.vis.internal.loop.router :as loop-router]
            [com.blockether.vis.internal.council.core :as council]
            [com.blockether.vis.internal.council.host :as host]
            [com.blockether.vis.internal.config.core :as config]
            [com.blockether.vis.internal.config.scoped :as scoped]
            [com.blockether.vis.internal.config.toggles :as toggles]
            [com.blockether.vis.internal.sandbox.jail :as pj]
            [com.blockether.vis.internal.sandbox.policy :as security-policy]
            [com.blockether.vis.internal.session.cancellation :as cancellation]
            [lazytest.core :refer [around-each defdescribe describe expect it]]))

(wiring/install!)

(h/use-mem-store! {"subagents" true})

(def ^:private fast-waits
  "Use a short signal tick in the question tests."
  (around-each [f] (with-redefs-fn {#'agents/signal-tick-ms 200} f)))

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
      (expect (= agents/max-active-children (get schema "x-vis-max-active-children") 32))
      (expect (document/valid? "agents" agent))
      (expect (document/valid? "agents" (assoc agent "note" "Reading the parser")))
      (expect (not (document/valid? "agents" (assoc agent "note" ""))))
      (expect (document/valid? "agents" (assoc agent "iteration_budget" 5000)) "No upper limit")
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

(defn- start!
  "Store the first model iteration of the current task of `child` at epoch ms `at`."
  [db child at]
  (ps/db-agent-update! db child {:status "running" :started_at at}))

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
  [db live f]
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
                    ;; The gateway store wakes the waiting question, as here. Without it the
                    ;; question woke only on the 5 s tick, the bound of the test's deref (CI).
                    (agents/council-inserted! opts)
                    {:entry_id id :replies [{:state "delivered"}]}))})
    (try (f entries)
         (finally (agents/install-runtime! saved)
                  (when-not registered? (extension/deregister-extension! "foundation-core"))))))

(defn- outcome [f] (try (:result (f)) (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))

(defn- snapshot [env child]
  (wire/->wire (agents/task-record env (agents/info (:db-info env) child))))

(defn- asking-record
  "Poll the record of `child` until it asks, for at most 5 s."
  [env child]
  (loop [n 0]
    (let [r (snapshot env child)]
      (if (or (= "asking" (get r "status")) (> n 50)) r (do (Thread/sleep 100) (recur (inc n)))))))

;; Issue #388: a child had 64 iterations, no warning and a hard stop without a handoff. A
;; parent could not add iterations, so the only option was a new child without the context.
(defdescribe
  subagent-budget-test
  {:context [fast-waits]}
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
          (expect (= :invalid-request (outcome #(agents/ask-budget child-env 0 "Too few"))))
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
        (atom #{})
        (fn [_]
          (with-redefs [ps/db-list-session-turns (fn [_ sid]
                                                   (if (= child (str sid)) @turns []))]
            (agents/finish! db child "completed")
            (expect (= "Old answer"
                       (get (snapshot env child)
                            "answer")))
            ;; The wake of a ping from the parent queues the child, as state/council-wake! does.
            (expect (fn? (agents/queue-turn! db child)))
            (expect (= 1 (:resumed_turns (agents/info db child)))
                    "The stale-answer guard is stored with the child, so a restart keeps it")
            ;; The gateway stores the status before the turn, so the old turn is still the last.
            (agents/finish! db child "completed")
            (expect (nil? (get (snapshot env child)
                               "answer")))
            (swap! turns conj {:id "new" :content [{"type" "prose" "markdown" "New answer"}]})
            (expect (= "New answer"
                       (get (snapshot env child)
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
                                   {:type "tool_use" :id "t1" :name "py3" :input {}}]}
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
                         ;; The settings of this machine can name models that this router does not have.
                         #'config/load-global-yaml-config-raw (constantly {})
                         #'config/load-global-config-raw (constantly {})
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
                         (:status (#'gateway/cancel-child! env {:session_id sid}))))
              (expect (not (agents/claim-iteration! {:db-info db :session-id sid})))
              (expect (= "cancelled" (:status (agents/info db sid))))
              (expect (= "Subagent stopped: it is cancelled."
                         (agents/stop-reason {:db-info db :session-id sid})))
              (let [child-ids #(mapv :session_id (ps/db-agent-list db leader))
                    before (child-ids)
                    launched @launches
                    run! #(#'gateway/agent-run env %)]
                ;; The second request fails during reservation, after the first child was forked.
                (expect (= :invalid-request
                           (try (run! ["Must roll back"
                                       {:task "Unknown model" :provider "p" :model "unknown"}])
                                nil
                                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
                (expect (= before (child-ids)))
                (expect (= launched @launches))
                ;; The fixture has three active children. Leave one slot open for the batch.
                (with-redefs [agents/max-active-children 4]
                  (let [[one two] (run! ["First new task" "Second new task"])
                        first-id (:child-session-id one)
                        second-id (:child-session-id two)
                        states #(mapv (fn [id] (:status (agents/info db id))) [first-id second-id])]
                    (expect (= ["queued" "waiting"] (states)))
                    (expect (= first-id (peek @launches)))
                    (agents/finish! db first-id "completed")
                    (agents/settle-task! env (agents/info db first-id))
                    (#'gateway/settle-dependents! db leader)
                    (expect (= ["completed" "queued"] (states)))
                    (expect (= second-id (peek @launches)))
                    (let [[follow] (run! [{:task "Follow up" :child first-id}])
                          old (task/status leader (:id one))]
                      (expect (= first-id (:child-session-id follow)))
                      (expect (not= (:id one) (:id follow)))
                      (expect (= ["completed" "waiting"]
                                 [(get-in old [:record :status]) (first (states))])))
                    (agents/finish! db second-id "completed")
                    (#'gateway/settle-dependents! db leader)
                    (expect (= ["queued" "completed"] (states)))
                    (expect (= first-id (peek @launches)))
                    (expect (= "completed"
                               (get-in (task/status leader (:id one)) [:record :status])))))))))))))

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
                                {:type "tool_use" :id "t1" :name "py3" :input {}}]}
                     {:role "user"
                      :content [{:type "tool_result" :tool_use_id "t1" :content "Output"}]}
                     {:role "assistant"
                      :content [{:type "tool_use" :id "t2" :name "py3" :input {}}]}
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
              "A second sweep finds nothing")))
  ;; Cross-check 2026-11-22: a child that a stopped gateway left got the whole downtime as work.
  (it "does not count the time without a process as the work of a child"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            queued
            (child! db leader {})

            running
            (child! db leader {})

            start
            #(:created_at (agents/info db %))

            last-iteration
            (+ (long (start running)) 5000)]

      (doseq [at [(+ (long (start running)) 1000) last-iteration]]
        (with-redefs [util/now-ms (fn ^long []
                                    (long at))]
          (ps/db-agent-claim-iteration! db running)))
      (with-redefs [util/now-ms (fn ^long []
                                  (+ (long last-iteration) 86400000))]
        (agents/reconcile-orphans! db []))
      (expect (= [0 (start queued)] ((juxt :worked_ms :finished_at) (agents/info db queued)))
              "A child that started no iteration did no work")
      (expect (= [4000 last-iteration] ((juxt :worked_ms :finished_at) (agents/info db running)))
              "A child works from its first iteration to its last one")
      (expect (= 4000
                 (:worked_ms (get (ps/db-team-clocks db leader)
                                  (:team_id (agents/info db running)))))))))

;; Regression (session 6866e0a0): after a gateway restart, the wait records of the ended
;; children were empty, and the leader rebuilt their work from git diff.
(defdescribe
  ended-child-notes-test
  (it
    "gives the last notes of a child that ended without an answer"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          cut
          (child! db leader {})

          now
          (System/currentTimeMillis)

          r
          (with-redefs [ps/db-list-session-turns
                        (fn [_ id]
                          (if (= cut (str id)) [{:id "t" :status :running}] []))

                        ps/db-list-session-turns-meta
                        (fn [_ id]
                          (if (= cut (str id)) [{:id "t" :position 0 :created-at now}] []))

                        ps/db-list-session-turn-iterations
                        (fn [_ id]
                          (if (= "t" (str id))
                            [{:position 3 :assistant-prose "Wrote the fix"}
                             {:position 0 :assistant-prose "Started"}
                             {:position 1 :assistant-prose "Read the parser"}
                             {:position 2 :assistant-prose " "} {:position 4}]
                            []))]

            (#'agents/record
             db
             nil
             (constantly false)
             (constantly false)
             (+ now 60000)
             (agents/reconcile-child! db (agents/info db cut))))]

      (expect (= "failed" (:status r)))
      (expect (nil? (:answer r)))
      (expect (= ["Started" "Read the parser" "Wrote the fix"] (:notes r)))
      (expect (document/valid-json? "agents"
                                    "record"
                                    (walk/stringify-keys (into {} (remove (comp nil? val)) r)))))))

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
               (agents/reconcile-child! db (agents/info db sid)))))]

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
                            (agents/reconcile-child! db (agents/info db real)))
                           [:error :code]))
                "A new child has no turn of its own")))))

(defdescribe
  stopping-a-parent-arms-its-backstop-before-stopping-children-test
  (it
    "stopping a parent arms its backstop and leaves an unawaited child running"
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
          (expect (= [leader] @armed))
           (expect (cancellation/cancelled? token))
           (expect (not (cancellation/cancelled? child-token)))
           (expect (= "queued" (:status (agents/info db child)))))))))

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
                       ;; The settings of this machine can name models that this router does not have.
                       #'config/load-global-yaml-config-raw (constantly {})
                       #'config/load-global-config-raw (constantly {})
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
                   (select-keys (agents/info db idle) [:status :iterations_used])))))))

;; Regression, issue #405: an extension's extend or deny of an open budget request
;; failed with inactive-session, because the reply had no live parent execution.
(defdescribe
  agent-answers-use-the-live-parent-env
  (it "extend and deny run in the parent's live env; reads do not need it"
      (let [seen (atom [])]
        (with-redefs-fn {#'lp/db-info (constantly ::db)
                         #'ps/db-get-session (constantly {:id "parent"})
                         (ns-resolve 'com.blockether.vis.internal.gateway.state 'live-env)
                         (constantly {:live? true})
                         #'agents/operation! (fn [env operation _]
                                               (swap! seen conj [operation (:live? env)])
                                               [])}
          (fn []
            (doseq [operation [:extend :deny :timeline]]
              (gateway/agents-operation! "parent" operation {}))
            (expect (= [[:extend true] [:deny true] [:timeline nil]] @seen)))))))

;; Regression for #405: a cancel of a finished child must keep its outcome.
(defdescribe cancel-of-a-finished-child-test
  (it "does not accept a cancel after settlement"
      (let [db (h/store)
            leader (str (h/store-session! db {:channel :api}))
            env {:db-info db :session-id leader}]
        (doseq [status ["completed" "failed" "cancelled"]]
          (let [child (child! db leader {})
                id (:id (agents/register-task! env (agents/info db child)
                                              (fn [_] (expect false "No interrupt")) nil))]
            (agents/finish! db child status)
            (agents/settle-task! env (agents/info db child))
            (expect (false? (task/cancel! leader id :person)))
            (expect (= status (get-in (task/status leader id) [:record :status]))))))))

(defdescribe child-id-arguments-take-task-ids-test
  (it "names a child by a1, A1 or its session id in Council controls"
      (let [db (h/store)
            leader (str (h/store-session! db {:channel :api}))
            child (child! db leader {})
            env {:db-info db :session-id leader}]
        (doseq [ref ["a1" "A1" child]]
          (expect (= child (get (agents/operation! env :timeline {:session_id ref}) "session_id"))))
        (expect (nil? (#'agents/check-after! env [{:task "Next" :after ["A1"]}])))
        (expect (= :invalid-request
                   (outcome #(agents/operation! env :extend {:session_id "A9" :iterations 1})))))))

;; Regression for #405: a held queue is visible without ending the attempt.
(defdescribe wait-reports-a-held-child-test
  (it "reports held for a queued child whose queue a human holds"
      (let [db (h/store)
            leader (str (h/store-session! db {:channel :api}))
            child (child! db leader {:status "queued"})
            env {:db-info db :session-id leader}
            id (:id (agents/register-task! env (agents/info db child) (fn [_]) nil))]
        (with-redefs [lp/db-info (constantly db)]
          (try (#'gateway/put-session! child {:next-seq 0 :queue-paused true
                                             :turns {"t1" {:status "queued"}}})
               (let [status (task/status leader id)]
                 (expect (= "held" (get-in status [:record :status])))
                 (expect (= :running (get-in status [:task :state]))))
               (finally (#'gateway/drop-session! child)))))))

(defdescribe event-wait-test
  (let [now #(java.util.Date.)
        team (fn [] (let [db (h/store)
                          leader (str (h/store-session! db {:channel :api}))]
                      {:db db :env {:db-info db :session-id leader}
                       :busy (child! db leader {:status "running" :iteration_budget 10})}))]
    ;; Issue #494: a tool of a child waits for the person at a form in the leader session.
    ;; The leader must not read that wait as a slow step and cancel the child.
    (it
      "alerts awaits_person, not slow_step, while a form of the child waits for the person"
      (let [{:keys [db env busy]}
            (team)

            turn
            (str (random-uuid))

            long-ago
            (java.util.Date. (- (System/currentTimeMillis) 400000))

            forms
            (atom 1)]

        (with-fake-runtime
          db
          (atom #{busy})
          (fn [_]
            (agents/install-runtime! (assoc @@#'agents/runtime
                                       :pending-input (fn [sid]
                                                        (if (= busy (str sid)) @forms 0))))
            (with-redefs [ps/db-list-session-turns-meta
                          (fn [_ sid]
                            (if (= busy (str sid)) [{:id turn :position 0 :created-at (now)}] []))

                          ;; The last step ended 400 s ago: the child is in a slow step.
                          ps/db-list-session-turn-iterations
                          (fn [_ id]
                            (if (= turn (str id)) [{:position 0 :finished-at long-ago}] []))]

              (let [alerts #(get (snapshot env busy)
                                 "alerts")]
                (expect (= ["awaits_person"] (alerts)))
                (reset! forms 0)
                (expect (= ["slow_step"] (alerts))
                        "After the answer, a slow step is slow again")))))))
    (it "shows Council messages in repeated reads of the same live snapshot"
        (let [{:keys [env busy]} (team)
              turn (str (random-uuid))]
          (with-redefs [ps/db-list-session-turns-meta (fn [_ _] [{:id turn :position 0 :created-at (now)}])
                        ps/db-list-session-turn-iterations (fn [_ _] [{:position 0 :created-at (now)
                                                                        :council-publications [{:entry_id 7}]}])
                        ps/db-council-get (fn [_ _] {:entry_id 7 :content "tokenize drops tabs"})]
            (dotimes [_ 2]
              (let [record (snapshot env busy)]
                (expect (= [{"entry_id" 7 "content" "tokenize drops tabs"}]
                           (get record "messages")))
                ;; Release run 41120931788: the SDK refused this record.
                (expect (document/valid-json? "agents" "record" record)))))))))

(defdescribe waits-park-on-team-signals-test
  (it "releases only the active wait without ending the child attempt"
      (let [db (h/store)
            leader (str (h/store-session! db {:channel :api}))
            env {:db-info db :session-id leader}
            child (child! db leader {:status "running"})
            id (:id (agents/register-task! env (agents/info db child) (fn [_]) nil))
            wait {:block "b" :token "wait"}]
        (expect (= :running (:state (task/await! leader id wait))))
        (task/detach-awaits! leader :council)
        (expect (= :detached (:state (task/await! leader id wait))))
        (expect (= :running (:state (task/await! leader id (assoc wait :token "again")))))))
  (it "detaches an await that starts after the child asks"
      (let [db (h/store)
            leader (str (h/store-session! db {:channel :api}))
            env {:db-info db :session-id leader}
            child (child! db leader {:status "running"})
            id (:id (agents/register-task! env (agents/info db child) (fn [_]) nil))]
        (with-fake-runtime db (atom #{leader})
          (fn [_]
            (let [asked (future (agents/ask {:db-info db :session-id child}
                                            "Which parser?" {:timeout_secs 10}))]
              (try (expect (= "asking" (get (asking-record env child) "status")))
                   (expect (= :detached (:state (task/await! leader id {:block "b" :token "wait"}))))
                   (expect (= ["Which parser?"]
                              (mapv #(get-in % [:reason :question])
                                    (task/take-detach-notices! leader "b"))))
                   (expect (= :running (:state (task/state leader id))))
                   (finally (agents/finish! db child "cancelled")
                            (deref asked 2000 nil))))))))
  (it "a question gets its reply at the signal of the reply"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {})]

        (with-fake-runtime
          db
          (atom #{leader})
          (fn [entries]
            (with-redefs [ps/db-council-get (fn [_ id]
                                              (get @entries id))]
              (let [asked (future (outcome #(agents/ask {:db-info db :session-id child}
                                                        "Which parser?"
                                                        {:timeout_secs 20})))
                    _ (asking-record {:db-info db :session-id leader} child)
                    entry-id (some (fn [[id e]]
                                     (when (= "Which parser?" (:content e)) id))
                                   @entries)]

                ((:publish @@#'agents/runtime)
                  {:session-id leader}
                  {:content "Use the new parser." :reply_to entry-id})
                (agents/council-inserted! {:thread_id entry-id :reply_to entry-id})
                (expect (= "Use the new parser." (deref asked 2000 ::timeout)))))))))
  ;; child.cancel() returned while the interrupt was still on its way; a parent reply in that gap
  ;; resumed the question, and the cancelled child went on working.
  (it "a question ignores a reply that lands after a cancel request"
      (let [db (h/store)
            leader (str (h/store-session! db {:channel :api}))
            env {:db-info db :session-id leader}
            child (child! db leader {:status "running"})
            id (:id (agents/register-task! env (agents/info db child) (fn [_]) nil))]
        (with-fake-runtime
          db
          (atom #{leader})
          (fn [entries]
            (with-redefs [ps/db-council-get (fn [_ id] (get @entries id))]
              (let [asked (future (outcome #(agents/ask {:db-info db :session-id child}
                                                        "Which parser?"
                                                        {:timeout_secs 20})))
                    _ (asking-record env child)
                    entry-id (some (fn [[id e]] (when (= "Which parser?" (:content e)) id))
                                   @entries)]
                (expect (task/cancel! leader id :agent))
                ((:publish @@#'agents/runtime)
                  {:session-id leader}
                  {:content "Use the new parser." :reply_to entry-id})
                (agents/council-inserted! {:thread_id entry-id :reply_to entry-id})
                (expect (not= "Use the new parser." (deref asked 2000 ::timeout)))
                (expect (realized? asked)))))))))

;; council.run promises that more than 16 tasks start as earlier ones finish. The gateway refused
;; the next spawn with budget-exceeded, which a run does not retry, so the whole run failed.
(defdescribe
  gateway-active-limit-is-retryable-test
  (it
    "refuses a spawn over the active limit with agent-limit"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          router
          {:providers [{:id :p :root "small" :models [{:name "small"}]}]}

          env
          {:db-info db
           :session-id leader
           :router router
           :turn-state-atom (atom {:council {:activation-id "active"}
                                   :agent-checkpoint [{:role :user :content "Current context"}]})}]

      (dotimes [_ agents/max-active-children]
        (child! db leader {:status "running"}))
      (with-redefs-fn {#'lp/db-info (constantly db)
                       ;; The settings of this machine can name models that this router does not have.
                       #'config/load-global-yaml-config-raw (constantly {})
                       #'config/load-global-config-raw (constantly {})
                       #'loop-router/get-router (constantly router)
                       #'council/enabled? (constantly true)
                       #'council/runtime (constantly {leader {:activation-id "active"}})
                       (ns-resolve 'com.blockether.vis.internal.gateway.state 'live-env) (constantly
                                                                                           env)}
        (fn []
          (expect (= :agent-limit
                     (try (gateway/agents-operation! leader :spawn {:task "Check evidence"})
                          (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
          ;; Council run dependencies: a task that waits is not active, and gets no task ping yet.
          (with-redefs-fn {#'council/publish! (fn [& _]
                                                (expect false "A waiting task gets no ping"))}
            (fn []
              (let [row (gateway/agents-operation! leader :spawn {:task "Verify" :after ["a1"]})]
                (expect (= ["waiting" ["a1"]] [(:status row) (:after row)]) (pr-str row))))))))))

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
             (doseq [[status refusal] [["running" nil] ["cancelled" :invalid-recipient]
                                       ;; A task with after starts only when they complete.
                                       ["waiting" :invalid-recipient]
                                       ["blocked" :invalid-recipient]]]
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
              (try (agents/prepare-run! {:db-info db :session-id leader} tasks)
                   nil
                   (catch clojure.lang.ExceptionInfo e e)))]

        (expect (= 0 @spawned) "No child starts before every task is valid")
        (expect (= :invalid-request (:error (ex-data error))))
        (expect (str/includes? (ex-message error)
                               "tasks[1].files: additional property \"exec\" is not allowed"))
        (expect (str/includes? (ex-message error) "No subagent started."))
        (expect (not (str/includes? (ex-message error) "anyOf")))))
  (it "names an unknown task key and the missing task"
      (let [error (try (let [db (h/store)] (agents/prepare-run! {:db-info db :session-id (h/store-session! db {:channel :api})}
                                   [{"question" "Count files"}]))
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
  (it "accepts a one-line title of 1 to 60 characters and a one-line result"
      (expect (document/valid-json? "agents" "spawn" {"task" "x" "title" "Fix login test"}))
      (expect
        (document/valid-json? "agents" "spawn" {"task" "x" "title" (apply str (repeat 60 "x"))}))
      (expect (document/valid-json? "agents" "run" {"tasks" [{"task" "x" "title" "Audit"}]}))
      (doseq [bad ["" " " (apply str (repeat 61 "x")) "Two\nlines"]]
        (expect (not (document/valid-json? "agents" "spawn" {"task" "x" "title" bad})) bad))
      (expect (document/valid-json? "agents" "result" {"text" "All pass" "outcome" "passed"}))
      (expect (not (document/valid-json? "agents" "result" {"text" "All pass" "outcome" "maybe"})))
      (expect (not (document/valid-json? "agents" "result" {"text" "a\nb"}))))
  (it "stores each title that the agents contract accepts"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            longest
            (get-in (document/schema-document "agents")
                    ["$defs" "spawn" "properties" "title" "maxLength"])

            title
            (apply str (repeat longest "x"))]

        (expect (= title (:title (agents/info db (child! db leader {:title title})))))))
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
    "keeps each earlier task of a resumed child, so a failed task stays on the team timeline"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:title "Build the engine"})

          start
          (:created_at (agents/info db child))]

      (start! db child start)
      (ps/db-agent-update! db child {:status "failed" :finished_at (+ start 60000)})
      (ps/db-agent-resume! db child 1 (+ start 90000))
      (expect (= [{:start start :end (+ start 60000) :status "failed"}]
                 (:runs (agents/info db child)))
              "The resume keeps the failed task that it replaces")
      (start! db child (+ start 90000))
      (ps/db-agent-update! db
                           child
                           {:status "completed" :outcome "passed" :finished_at (+ start 120000)})
      (let [restore!
            (agents/queue-turn! db child)

            row
            (agents/info db child)]

        (expect
          (= [{:start start :end (+ start 60000) :status "failed"}
              {:start (+ start 90000) :end (+ start 120000) :status "completed" :outcome "passed"}]
             (:runs row)))
        (expect (document/valid-json? "agents" "agent" (walk/stringify-keys row))
                "The rows of a team keep the contract")
        (restore!)
        (expect (= [{:start start :end (+ start 60000) :status "failed"}]
                   (:runs (agents/info db child)))
                "An undone ping takes its task out of runs again"))))
  ;; User report 2026-11-21: each client added the time of each task of a child at each paint.
  (it
    "stores the time of the ended tasks of a child, so a reader adds no earlier task"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {})

          start
          (:created_at (agents/info db child))

          worked
          #(:worked_ms (agents/info db child))]

      (expect (= 0 (worked)))
      (start! db child start)
      (ps/db-agent-update! db child {:status "failed" :finished_at (+ start 60000)})
      (expect (= 60000 (worked)))
      (ps/db-agent-update! db child {:status "failed" :finished_at (+ start 70000)})
      (expect (= 60000 (worked)) "A repeated end adds nothing")
      (ps/db-agent-resume! db child 1 (+ start 90000))
      (expect (= 60000 (worked)) "The time stops while the child does not work")
      (expect (= [(+ start 60000)] (mapv :end (:runs (agents/info db child))))
              "A repeated end keeps the first end, so the earlier task keeps its time")
      (start! db child (+ start 90000))
      (ps/db-agent-update! db child {:status "completed" :finished_at (+ start 120000)})
      (expect (= 90000 (worked)))
      (expect (document/valid-json? "agents" "agent" (walk/stringify-keys (agents/info db child))))
      (let [restore! (agents/queue-turn! db child)]
        (restore!)
        (expect (= [90000 (+ start 120000)] ((juxt :worked_ms :finished_at) (agents/info db child)))
                "An undone ping adds nothing"))))
  ;; User report 2026-11-22: each client joined the work of all agents of a team at each paint.
  (it
    "stores the clock of each team, so a reader joins no work of its agents"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          a
          (child! db leader {})

          team
          (:team_id (agents/info db a))

          start
          (:created_at (agents/info db a))

          clocks
          #(ps/db-team-clocks db leader)]

      (start! db a start)
      (expect (= {team {:worked_ms 0 :busy_since start}} (clocks)))
      (ps/db-agent-update! db a {:status "completed" :finished_at (+ start 60000)})
      (expect (= {team {:worked_ms 60000}} (clocks)) "The time stops while no agent works")
      (let [b (child! db leader {:team_id team :spawn_key "b"})]
        (start! db b start)
        (expect (= {team {:worked_ms 0 :busy_since start}} (clocks))
                "An agent that started in the time of another continues that work")
        (ps/db-agent-update! db b {:status "completed" :finished_at (+ start 30000)})
        (expect (= {team {:worked_ms 60000}} (clocks)) "Work at the same time counts one time"))
      (let [ping-turn
            (str (ps/db-store-session-turn!
                   db
                   {:parent-session-id leader :user-request "Next" :status :running}))

            restore!
            (agents/queue-turn! db a)]

        (restore!)
        (expect (= {team {:worked_ms 60000}} (clocks)) "An undone ping changes no clock")
        (expect (nil? (:turn_ids (agents/info db a))) "An undone ping adds no turn")
        (ps/db-agent-resume! db a 1 (+ start 90000))
        (expect (= [team ping-turn] (:turn_ids (agents/info db a)))
                "A follow-up task joins the newest turn of the leader")
        (start! db a (+ start 90000))
        (expect (= {team {:worked_ms 60000 :busy_since (+ start 90000)}
                    ping-turn {:worked_ms 60000 :busy_since (+ start 90000)}}
                   (clocks)))
        (ps/db-agent-update! db a {:status "completed" :finished_at (+ start 120000)})
        (expect (= {team {:worked_ms 90000} ping-turn {:worked_ms 90000}} (clocks))))))
  ;; Cross-check 2026-11-22: the time in the queue counted as work, a cancel in the queue got the
  ;; time to the end of its turn, and a deleted child left the clock of its team running.
  (it
    "counts the work of a child from its first iteration to its end"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {})

          team
          (:team_id (agents/info db child))

          start
          (long (:created_at (agents/info db child)))

          at
          (fn [ms f]
            (with-redefs [util/now-ms (fn ^long []
                                        (+ start (long ms)))]
              (f)))

          clocks
          #(ps/db-team-clocks db leader)]

      (expect (= {} (clocks)) "A queued child does no work")
      (at 60000 #(ps/db-agent-claim-iteration! db child))
      (at 60500 #(ps/db-agent-claim-iteration! db child))
      (expect (= {team {:worked_ms 0 :busy_since (+ start 60000)}} (clocks)))
      (ps/db-agent-update! db child {:status "completed" :finished_at (+ start 61000)})
      (expect (= 1000 (:worked_ms (agents/info db child))))
      (expect (= {team {:worked_ms 1000}} (clocks)))
      (ps/db-agent-resume! db child 1 (+ start 90000))
      (expect (= {team {:worked_ms 1000}} (clocks))
              "A follow-up task waits for its first iteration")
      (at 95000 #(ps/db-agent-claim-iteration! db child))
      (at 97000 #(ps/db-agent-update! db child {:status "cancelled"}))
      (at 150000 #(agents/finish! db child "cancelled"))
      (expect (= 3000 (:worked_ms (agents/info db child))) "A cancel ends the task")
      (expect (= {team {:worked_ms 3000}} (clocks)))
      (ps/db-agent-resume! db child 1 (+ start 200000))
      (expect (= [{:start (+ start 60000) :end (+ start 61000) :status "completed"}
                  {:start (+ start 95000) :end (+ start 97000) :status "cancelled"}]
                 (:runs (agents/info db child))))
      (at 200500 #(ps/db-agent-update! db child {:status "cancelled"}))
      (at 260000 #(agents/finish! db child "cancelled"))
      (expect (= 3000 (:worked_ms (agents/info db child)))
              "A task that is cancelled in the queue did no work")
      (expect (= {team {:worked_ms 3000}} (clocks)))))
  (it "stops the clock of a team when its child is deleted"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {})]

        (ps/db-agent-claim-iteration! db child)
        (expect (some? (:busy_since (first (vals (ps/db-team-clocks db leader))))))
        (ps/db-delete-session-tree! db child)
        (expect (= {} (ps/db-team-clocks db leader)))))
  ;; Cross-check 2026-11-22: a person wrote to a child that had ended. The row said running, but
  ;; no time ran, and the time of that turn was lost.
  (it
    "counts a turn of an ended child without a ping as a new task"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {})

          team
          (:team_id (agents/info db child))

          start
          (long (:created_at (agents/info db child)))

          at
          (fn [ms f]
            (with-redefs [util/now-ms (fn ^long []
                                        (+ start (long ms)))]
              (f)))]

      (at 0 #(ps/db-agent-claim-iteration! db child))
      (ps/db-agent-update! db child {:status "completed" :finished_at (+ start 1000)})
      (at 10000 #(ps/db-agent-claim-iteration! db child))
      (let [row (agents/info db child)]
        (expect (= ["running" (+ start 10000) nil] ((juxt :status :busy_since :finished_at) row)))
        (expect (= [{:start start :end (+ start 1000) :status "completed"}] (:runs row)))
        (expect (document/valid-json? "agents" "agent" (walk/stringify-keys row))))
      (expect (= {team {:worked_ms 1000 :busy_since (+ start 10000)}}
                 (ps/db-team-clocks db leader)))
      (at 12000 #(agents/finish! db child "completed"))
      (expect (= 3000 (:worked_ms (agents/info db child))))
      (expect (= {team {:worked_ms 3000}} (ps/db-team-clocks db leader)))))
  ;; Cross-check 2026-11-22: the system clock went back before a follow-up task. The end of the
  ;; orphan used the last iteration of the earlier task, so the queued task got 5 s of work.
  (it "gives no time to a queued follow-up task that a dead process left"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            child
            (child! db leader {})

            start
            (long (:created_at (agents/info db child)))]

        (doseq [ms [5000 6000]]
          (with-redefs [util/now-ms (fn ^long []
                                      (+ start (long ms)))]
            (ps/db-agent-claim-iteration! db child)))
        (ps/db-agent-update! db child {:status "completed" :finished_at (+ start 6000)})
        (ps/db-agent-resume! db child 1 (+ start 1000))
        (agents/reconcile-orphans! db [])
        (expect (= ["failed" 1000] ((juxt :status :worked_ms) (agents/info db child))))
        (expect (= [{:worked_ms 1000}] (vals (ps/db-team-clocks db leader))))))
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
          (fn [turns prose]
            (with-redefs [ps/db-list-session-turns-meta
                          (fn [_ _]
                            turns)

                          ps/db-latest-turn-prose
                          (fn [_ turn-id]
                            (get prose turn-id))]

              (agents/latest-note db row (+ t0 100))))

          old
          {:id "old" :created-at (- t0 60000)}

          current
          {:id "t1" :created-at (+ t0 10)}]

      (expect (= "I read the docs."
                 (note-of [old current] {"old" "Old task" "t1" "I read the docs."})))
      (expect (nil? (note-of [old] {"old" "Old task"})) "A note of an older task never shows")
      (expect (nil? (note-of [] {})))
      (expect (= (str (apply str (repeat 300 "a")) "…")
                 (note-of [current] {"t1" (apply str (repeat 400 "a"))}))))))

;; A subagent question that only the person can decide: the leader ends its turn suspended to ask
;; the person. The child keeps waiting for the reply of the next leader turn.
(defdescribe
  child-question-waits-for-person-test
  {:context [fast-waits]}
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
              (expect (= ::waiting (deref asked 1000 ::waiting))
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
              ;; A stopped parent sends no signal; the tick of the question sees it.
              (expect (nil? (deref asked 8000 ::timeout))
                      "A parent that neither runs nor waits for the person ends the wait"))
            (reset! live #{leader})
            (reset! suspended? true)
            (let [asked (future (outcome #(agents/ask child-env "Last?" {:timeout_secs 20})))]
              (asking-record env child)
              (reset! live #{})
              (ps/db-agent-update! db child {:status "cancelled"})
              (expect
                (nil? (deref asked 8000 ::timeout))
                "A cancelled child ends the wait also while the parent asks the person"))))))))

;; Council run dependencies: a task waits only for an earlier task, so no cycle can form.
(defdescribe
  run-after-names-earlier-tasks-test
  (it
    "refuses a later, own or unknown task in after before the first spawn"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          old
          (child! db leader {:status "completed"})

          env
          {:db-info db :session-id leader}]

      (with-fake-runtime
        db
        (atom #{})
        (fn [_]
           (doseq [tasks [[{:task "A" :after ["a2"]}] [{:task "A"} {:task "B" :after ["a4"]}]
                          [{:task "A" :after ["a9"]}]]]
             (expect (= :invalid-request (outcome #(agents/prepare-run! env tasks))) (pr-str tasks)))
           (expect (= [old] (mapv (comp str :session_id) (ps/db-agent-list db leader))))
           (expect (= [["a1"] ["a2" old]]
                      (mapv :after (agents/prepare-run!
                                     env [{:task "A" :after ["a1"]}
                                          {:task "B" :after ["a2" old]}])))))))))

;; Session 18979591: a leader read elapsed_secs of a completed child and got KeyError. A reply
;; with files and no ping was refused, and an implementer got new edits while its verifiers ran.
(defdescribe
  council-leader-feedback-test
  (it
    "gives every record each contract key, also for an ended child"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          done
          (child! db leader {})

          r
          (with-redefs [ps/db-list-session-turns (fn [_ id]
                                                   (if (= done (str id))
                                                     [{:id "t"
                                                       :status :success
                                                       :content [{"type" "prose"
                                                                  "markdown" "Done"}]}]
                                                     []))]
            (#'agents/record
             db
             nil
             (constantly false)
             (constantly false)
             (+ (System/currentTimeMillis) 60000)
             (agents/reconcile-child! db (agents/info db done))))

          wire
          (walk/stringify-keys r)]

      (expect (= "completed" (:status r)))
      (expect (= (set (keys (get-in (document/schema-document "agents")
                                    ["$defs" "record" "properties"])))
                 (set (keys wire))))
      (expect (nil? (get wire "elapsed_secs")))
      (expect (document/valid-json? "agents" "record" wire) "A null value fits the contract")))
  (it
    "gives files to the child that wrote the reply_to entry"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {})

          stranger
          (str (h/store-session! db {:channel :api}))

          env
          {:db-info db :session-id leader}

          live
          (atom #{})

          refusal
          (fn [entry-id]
            (try (agents/publish-with-files!
                   env
                   {:content "Fix it" :reply_to entry-id :files {:write ["e.txt"]}}
                   identity)
                 nil
                 (catch clojure.lang.ExceptionInfo e (ex-message e))))]

      (with-fake-runtime
        db
        live
        (fn [_]
          (with-redefs [ps/db-council-get (fn [_ id]
                                            (case (long id)
                                              5
                                              {:entry_id 5 :author_session_id child}

                                              6
                                              {:entry_id 6 :author_session_id stranger}

                                              nil))]
            (agents/finish! db child "completed")
            (expect (nil? (refusal 5)))
            (expect (= {:write ["e.txt"]} (:files (agents/info db child))))
            (expect (str/includes? (refusal 6) "files needs exactly one recipient"))
            (swap! live conj child)
            (expect (str/includes? (refusal 5) "council.grant")
                    "A running child gets more files with council.grant"))))))
  (it "warns when a ping can change the files that a running verifier checks"
      (let [db
            (h/store)

            leader
            (str (h/store-session! db {:channel :api}))

            writer
            (child! db leader {:role "implement" :files {:write ["src/a.clj"]}})

            reader
            (child! db leader {:role "research"})

            checker
            (child! db leader {:role "verify"})

            env
            {:db-info db :session-id leader}]

        (agents/finish! db writer "completed")
        (agents/finish! db reader "completed")
        (ps/db-agent-update! db checker {:status "running"})
        (expect (str/includes? (str (agents/publish-warning env {:ping [writer]}))
                               "Verifiers still run"))
        (expect (nil? (agents/publish-warning env {:ping [reader]})) "A child without write rules")
        (expect (nil? (agents/publish-warning env {:ping [checker]}))
                "A ping to the verifier itself")
        (expect (nil? (agents/publish-warning env {:content "No ping"})))
        (agents/finish! db checker "completed")
        (expect (nil? (agents/publish-warning env {:ping [writer]})) "No verifier runs")))
  (it
    "returns the warning in the council.publish result of the model tool only"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          writer
          (child! db leader {:role "implement" :files {:write ["src/a.clj"]}})

          checker
          (child! db leader {:role "verify"})

          env
          {:db-info db :session-id leader :turn-state-atom (atom {})}

          entry
          (fn [opts]
            {:entry_id 1
             :thread_id 1
             :group_id "g"
             :content (:content opts)
             :author_session_id leader
             :created_at 1
             :source "host"
             :ping (vec (:ping opts))})]

      (agents/finish! db writer "completed")
      (ps/db-agent-update! db checker {:status "running"})
      (with-redefs [council/publish!
                    (fn [_ _ _ opts]
                      (entry opts))

                    extension/publish-activity!
                    (constantly nil)]

        (let [shown (:result (host/publish env "Fix the finding" {"ping" [writer]}))]
          (expect (str/includes? (str (get shown "warning")) "Verifiers still run"))
          (expect (document/valid-json? "council" "entry" (dissoc shown "warning"))))
        (expect (nil? (get (:result (host/publish env "Note" {"ping" [checker]})) "warning"))))
      (let [sdk (wire/->wire
                  (agents/publish-with-files! env {:content "Fix" :ping [writer]} entry))]
        (expect (document/valid-json? "council" "entry" sdk)
                "The SDK publication has no warning key, so the entry schema holds"))))
  ;; User decision: the iteration budget has no upper limit. The leader of session 18979591
  ;; reached 530 iterations and could not give more.
  (it
    "extends a budget past 600 and starts a task with a budget of 1000"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          child
          (child! db leader {:iteration_budget 530})

          big
          (child! db leader {:iteration_budget 1000})

          env
          {:db-info db :session-id leader}]

      (with-fake-runtime
        db
        (atom #{})
        (fn [_]
          (agents/finish! db child "completed")
          (expect
            (= 730 (get (first (outcome #(agents/extend-agent env child 200))) "iteration_budget")))
          (expect (= 1000 (:iteration_budget (agents/info db big)))
                  "The store keeps a large budget")
           (expect (= [1000]
                      (mapv :iteration_budget (agents/prepare-run! env [{:task "Large" :iteration_budget 1000}]))))
          (expect (map? (outcome #(agents/ask-budget {:db-info db :session-id big}
                                                     5000
                                                     "Large"
                                                     {:timeout_secs 1})))
                  "A child can ask for more than 600"))))))

(defdescribe
  gateway-spawn-follows-subagent-model-settings-test
  (it
    "a spawn uses the first project subagent model by default, the next as its fallback, and the list as its limit"
    (let [db
          (h/store)

          leader
          (str (h/store-session! db {:channel :api}))

          project-id
          (str (:id (ps/db-create-project! db
                                           {:name "Model team"
                                            :workspace-root (.getCanonicalPath
                                                              (java.io.File.
                                                                "/tmp/subagent-model-project"))})))

          ;; The `toggles` of the project configuration files.
          project-toggles
          (atom {"subagent_models" [{"provider" "p" "model" "large"} {"provider" "q" "model" "mid"}
                                    {"provider" "q" "model" "big"}]})

          router
          {:providers [{:id :p :root "small" :models [{:name "small"} {:name "large"}]}
                       {:id :q :root "big" :models [{:name "big"} {:name "mid"}]}]}

          env
          {:db-info db
           :session-id leader
           :router router
           :turn-state-atom (atom {:agent-checkpoint [{:role :system :content "Rules"}
                                                      {:role :user :content "Parent request"}]
                                   :council {:activation-id "parent"}})
           :security-policy {:process-jail {}}}

          refusal
          (fn [op opts]
            (try (gateway/agents-operation! leader op opts)
                 nil
                 (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))

          spawn!
          (fn [opts]
            (:session_id (gateway/agents-operation! leader :spawn opts)))]

      (ps/db-set-session-project! db leader project-id)
      (ps/db-store-session-turn!
        db
        {:parent-session-id leader :user-request "Parent task" :status :running})
      (with-redefs-fn {#'lp/db-info (constantly db)
                       #'config/load-project-tiers-raw (fn []
                                                         {"toggles" @project-toggles})
                       #'loop-router/get-router (constantly router)
                       #'toggles/enabled? (constantly true)
                       #'council/runtime (fn [_]
                                           {leader {:activation-id "parent" :group-id project-id}})
                       (ns-resolve 'com.blockether.vis.internal.gateway.state 'live-env) (constantly
                                                                                           env)
                       (ns-resolve 'com.blockether.vis.internal.council.core 'runtime-waker)
                       (atom {:eligible? (constantly true) :wake! (constantly true)})}
        (fn []
          (let [child
                (spawn! {:task "Check on the setting model" :key "setting"})

                tasked
                (spawn! {:task "Check on the task model" :key "task" :provider "q" :model "big"})]

            (expect (= {"models" [{"provider" "p" "model" "large"} {"provider" "q" "model" "mid"}
                                  {"provider" "q" "model" "big"}]}
                       (agents/routing-view db leader))
                    "The sandbox routing shows the list in its order")
            (expect (= {:provider "p" :model "large"} (smodel/model-of db child))
                    "The first subagent model wins over the model of the parent")
            (expect (= {:provider "q" :model "big"} (smodel/model-of db tasked))
                    "A task model from the list wins over the first one")
            (expect (= {:provider "q" :model "mid"} (:fallback_model (agents/info db child)))
                    "The next model of the list is the fallback of the child")
            (expect (= {:provider "p" :model "large"} (:fallback_model (agents/info db tasked)))
                    "A child on a later model falls back to the first model")
            (expect (= [:q "mid"]
                       ((juxt :id :root)
                         (first (sort-by :priority
                                         (:providers (agents/restrict-router {:db-info db
                                                                              :session-id child}
                                                                             router))))))
                    "Automatic fallback of the child tries its fallback model first")
            (expect (= :routing-locked
                       (refusal :route {:session_id child :provider "p" :model "small"}))
                    "A queued child keeps its model")
            (expect (= {"provider" "p" "model" "large"}
                        (select-keys (snapshot env child)
                                    ["provider" "model"]))
                    "The wait record names the model of the child")
            (expect (= :model-not-allowed
                       (refusal
                         :spawn
                         {:task "Check elsewhere" :key "other" :provider "p" :model "small"}))
                    "The list refuses a task model outside it")
            (expect (= [{:provider "p" :model "large"} {:provider "q" :model "mid"}
                        {:provider "q" :model "big"}]
                       (:allowed_models (agents/info db child)))
                    "The list limits the child")
            (swap! project-toggles assoc "subagent_models" [])
            (let [free (spawn! {:task "Check anywhere" :key "free" :provider "p" :model "small"})]
              (expect (= {:provider "p" :model "small"} (smodel/model-of db free))
                      "An empty list allows every model")
              (expect (empty? (:allowed_models (agents/info db free)))
                      "An empty list sets no limit")
              (expect (nil? (:fallback_model (agents/info db free)))
                      "An empty list gives no fallback model"))))))))
