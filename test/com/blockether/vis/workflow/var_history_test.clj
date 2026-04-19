(ns com.blockether.vis.workflow.var-history-test
  "End-to-end tests for var-history, var-diff, diffable? guard, and
   auto-bound SYSTEM vars (*query* / *reasoning* / *answer*).

   Exercises the full path — conversations/send! → rlm iteration loop →
   LLM-stubbed response → iteration-var persistence — then queries
   db-var-history and the SCI-bound var-history / var-diff tools.
   No network, no real model."
  (:require [babashka.fs :as fs]
            [lazytest.core :refer [defdescribe describe expect it throws?]]
            [com.blockether.svar.internal.llm :as llm]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.loop.conversations.core :as conversations]
            [com.blockether.vis.loop.storage.db :as db]
            [sci.core :as sci]))

;;; ── Helpers ─────────────────────────────────────────────────────────────

(defn- sandbox-value
  "Return the live value of `sym` in the sandbox, or nil if unbound."
  [env sym]
  (let [v (get-in @(:env (:sci-ctx env)) [:namespaces 'sandbox sym])]
    (if (instance? clojure.lang.IDeref v) @v v)))

(defn- zero-usage []
  {:result {}
   :tokens {:input 0 :output 0 :total 0 :reasoning 0 :cached 0}
   :cost   {:input-cost 0 :output-cost 0 :total-cost 0}
   :duration-ms 0})

(defn- scripted-llm
  "Build a stub for `llm/ask!` that returns pre-canned iteration responses
   one at a time, in order."
  [responses]
  (let [queue (atom (vec responses))]
    (fn [_router _opts]
      (let [[head & rest] @queue]
        (when-not head
          (throw (ex-info "scripted-llm exhausted" {})))
        (reset! queue (vec rest))
        (merge (zero-usage) {:result head})))))

(defn- with-temp-db*
  [f]
  (let [original-path config/db-path
        temp-dir (str (fs/create-temp-dir {:prefix "vis-var-history-"}))]
    (try
      (alter-var-root #'config/db-path (constantly temp-dir))
      (conversations/close-all!)
      (f)
      (finally
        (conversations/close-all!)
        (alter-var-root #'config/db-path (constantly original-path))
        (fs/delete-tree temp-dir)))))

(defmacro ^:private with-temp-db [& body]
  `(with-temp-db* (fn [] ~@body)))

;;; ── db-var-history tests ────────────────────────────────────────────────

(defdescribe var-history-test
  (describe "db-var-history versioning"
    (it "returns version 1 and 2 when the same var is def'd twice across queries"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "var-history v1v2"})]
          ;; Query 1: (def data {:a 1})
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def data {:a 1})" :time-ms 100}]}
                                    {:answer "v1" :confidence "high"}])]
            (conversations/send! id "define data" {:max-iterations 3}))
          ;; Query 2: (def data {:a 2 :b 3})
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def data {:a 2 :b 3})" :time-ms 100}]}
                                    {:answer "v2" :confidence "high"}])]
            (conversations/send! id "update data" {:max-iterations 3}))
          (let [env (conversations/env-for id)
                history (db/db-var-history (:db-info env) (:conversation-ref env) 'data)]
            (expect (= 2 (count history)))
            (expect (= 1 (:version (first history))))
            (expect (= {:a 1} (:value (first history))))
            (expect (= 2 (:version (second history))))
            (expect (= {:a 2 :b 3} (:value (second history)))))
          (conversations/delete! id))))

    (it "returns version 1 and 2 when the same var is def'd twice within ONE query"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "var-history same-query"})]
          ;; Single query, two iterations redefining the same var
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def scores [10 20])" :time-ms 100}]}
                                    {:code [{:expr "(def scores [10 20 30])" :time-ms 100}]}
                                    {:answer "done" :confidence "high"}])]
            (conversations/send! id "build scores" {:max-iterations 4}))
          (let [env (conversations/env-for id)
                history (db/db-var-history (:db-info env) (:conversation-ref env) 'scores)
                values (set (map :value history))]
            ;; Two iterations def'd scores → two entries in history
            (expect (= 2 (count history)))
            ;; Both values present (order may vary with sub-ms created_at)
            (expect (contains? values [10 20]))
            (expect (contains? values [10 20 30]))
            ;; Versions are 1 and 2
            (expect (= #{1 2} (set (map :version history)))))
          (conversations/delete! id)))))

  (describe "diffable? flag"
    (it "marks maps, vectors, sets as diffable"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "diffable collections"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def m {:a 1})" :time-ms 100}
                                            {:expr "(def v [1 2 3])" :time-ms 100}
                                            {:expr "(def s #{:x :y})" :time-ms 100}]}
                                    {:answer "ok" :confidence "high"}])]
            (conversations/send! id "define collections" {:max-iterations 3}))
          (let [env (conversations/env-for id)
                db-info (:db-info env)
                cref (:conversation-ref env)]
            (expect (true? (:diffable? (first (db/db-var-history db-info cref 'm)))))
            (expect (true? (:diffable? (first (db/db-var-history db-info cref 'v)))))
            (expect (true? (:diffable? (first (db/db-var-history db-info cref 's))))))
          (conversations/delete! id))))

    (it "marks strings, numbers, keywords as NOT diffable"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "non-diffable prims"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def txt \"hello\")" :time-ms 100}
                                            {:expr "(def num 42)" :time-ms 100}
                                            {:expr "(def kw :foo)" :time-ms 100}]}
                                    {:answer "ok" :confidence "high"}])]
            (conversations/send! id "define primitives" {:max-iterations 3}))
          (let [env (conversations/env-for id)
                db-info (:db-info env)
                cref (:conversation-ref env)]
            (expect (false? (:diffable? (first (db/db-var-history db-info cref 'txt)))))
            (expect (false? (:diffable? (first (db/db-var-history db-info cref 'num)))))
            (expect (false? (:diffable? (first (db/db-var-history db-info cref 'kw))))))
          (conversations/delete! id)))))

  (describe "var-history SCI tool"
    (it "is callable from the sandbox and returns versioned entries"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "sci var-history"})]
          ;; Two queries defining the same var
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def cfg {:port 3000})" :time-ms 100}]}
                                    {:answer "v1" :confidence "high"}])]
            (conversations/send! id "cfg v1" {:max-iterations 3}))
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def cfg {:port 8080 :host \"0.0.0.0\"})" :time-ms 100}]}
                                    {:answer "v2" :confidence "high"}])]
            (conversations/send! id "cfg v2" {:max-iterations 3}))
          ;; Now call var-history from sandbox
          (let [env (conversations/env-for id)
                result (sci/eval-string+ (:sci-ctx env)
                         "(var-history 'cfg)"
                         {:ns (sci/find-ns (:sci-ctx env) 'sandbox)})]
            (expect (= 2 (count (:val result))))
            (expect (= {:port 3000} (:value (first (:val result)))))
            (expect (= {:port 8080 :host "0.0.0.0"} (:value (second (:val result))))))
          (conversations/delete! id)))))

  (describe "var-diff SCI tool"
    (it "returns structural diff between two collection versions"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "sci var-diff"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def data {:a 1 :b 2})" :time-ms 100}]}
                                    {:answer "v1" :confidence "high"}])]
            (conversations/send! id "data v1" {:max-iterations 3}))
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def data {:a 1 :b 99 :c 3})" :time-ms 100}]}
                                    {:answer "v2" :confidence "high"}])]
            (conversations/send! id "data v2" {:max-iterations 3}))
          (let [env (conversations/env-for id)
                result (sci/eval-string+ (:sci-ctx env)
                         "(var-diff 'data 1 2)"
                         {:ns (sci/find-ns (:sci-ctx env) 'sandbox)})
                diff (:val result)]
            (expect (= 1 (:from-version diff)))
            (expect (= 2 (:to-version diff)))
            (expect (pos? (:edit-count diff)))
            (expect (seq (:edits diff))))
          (conversations/delete! id))))

    (it "strings get a line-level unified diff (:string-diff)"
      ;; var-diff's diff-values dispatch treats two strings as a `:string-diff`
      ;; (unified line diff via java-diff-utils). It is NOT a "non-diffable"
      ;; case — that's reserved for heterogeneous types that don't support
      ;; any structural comparison (→ `:replacement`). This test pins the
      ;; :string-diff semantics so future refactors can't silently regress it.
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "sci var-diff string"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def msg \"hello\")" :time-ms 100}]}
                                    {:answer "v1" :answer-type "mustache-text" :confidence "high"}])]
            (conversations/send! id "msg v1" {:max-iterations 3}))
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def msg \"world\")" :time-ms 100}]}
                                    {:answer "v2" :answer-type "mustache-text" :confidence "high"}])]
            (conversations/send! id "msg v2" {:max-iterations 3}))
          (let [env (conversations/env-for id)
                result (sci/eval-string+ (:sci-ctx env)
                         "(var-diff 'msg 1 2)"
                         {:ns (sci/find-ns (:sci-ctx env) 'sandbox)})
                diff (:val result)]
            (expect (= :string-diff (:type diff)))
            (expect (= 1 (:from-version diff)))
            (expect (= 2 (:to-version diff)))
            (expect (pos? (:edit-count diff)))
            (expect (re-find #"-hello" (:unified diff)))
            (expect (re-find #"\+world" (:unified diff))))
          (conversations/delete! id)))))

  (describe "auto-bound *query* var"
    (it "persists user query text and is retrievable via var-history"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "auto query var"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:answer "first" :confidence "high"}])]
            (conversations/send! id "What is 2+2?" {:max-iterations 1}))
          (with-redefs [llm/ask! (scripted-llm
                                   [{:answer "second" :confidence "high"}])]
            (conversations/send! id "Explain monads" {:max-iterations 1}))
          (let [env (conversations/env-for id)
                history (db/db-var-history (:db-info env) (:conversation-ref env) '*query*)]
            (expect (= 2 (count history)))
            (expect (= "What is 2+2?" (:value (first history))))
            (expect (= "Explain monads" (:value (second history)))))
          (conversations/delete! id))))

    (it "is accessible as a live sandbox var"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "query sandbox"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:answer "ok" :confidence "high"}])]
            (conversations/send! id "Hello world" {:max-iterations 1}))
          (let [env (conversations/env-for id)]
            (expect (= "Hello world" (sandbox-value env '*query*))))
          (conversations/delete! id)))))

  (describe "auto-bound *reasoning* var"
    (it "persists thinking text when present"
      ;; :answer-type is required when :answer is set — without it, mock[1]
      ;; would fail validation and the loop would keep spinning until the
      ;; scripted script is exhausted. We pin it explicitly so the mock
      ;; reflects a well-formed iteration response.
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "auto reasoning var"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:thinking "Let me think about this carefully"
                                     :code [{:expr "(def x 42)" :time-ms 100}]}
                                    {:thinking "Now I know the answer"
                                     :answer "42" :answer-type "mustache-text" :confidence "high"}])]
            (conversations/send! id "What is the answer?" {:max-iterations 3}))
          (let [env (conversations/env-for id)
                history (db/db-var-history (:db-info env) (:conversation-ref env) '*reasoning*)
                values  (set (map :value history))]
            ;; Two iterations with thinking → two *reasoning* versions.
            ;; Assert by SET: iterations can share a millisecond timestamp,
            ;; which makes sequence ordering DB-engine-dependent.
            (expect (= 2 (count history)))
            (expect (= #{"Let me think about this carefully" "Now I know the answer"} values)))
          (conversations/delete! id))))

    (it "is accessible as a live sandbox var with latest thinking"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "reasoning sandbox"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:thinking "Deep thought"
                                     :answer "ok" :confidence "high"}])]
            (conversations/send! id "Think about this" {:max-iterations 1}))
          (let [env (conversations/env-for id)]
            (expect (= "Deep thought" (sandbox-value env '*reasoning*))))
          (conversations/delete! id))))

    (it "is NOT persisted when thinking is empty"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "no reasoning"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:answer "quick" :confidence "high"}])]
            (conversations/send! id "Quick question" {:max-iterations 1}))
          (let [env (conversations/env-for id)
                history (db/db-var-history (:db-info env) (:conversation-ref env) '*reasoning*)]
            (expect (empty? history)))
          (conversations/delete! id))))

    (it "tracks reasoning across multiple queries via var-history"
      (with-temp-db
        (let [{id :id} (conversations/create! :cli {:title "reasoning multi-query"})]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:thinking "Query 1 reasoning"
                                     :answer "a1" :confidence "high"}])]
            (conversations/send! id "Q1" {:max-iterations 1}))
          (with-redefs [llm/ask! (scripted-llm
                                   [{:thinking "Query 2 reasoning"
                                     :answer "a2" :confidence "high"}])]
            (conversations/send! id "Q2" {:max-iterations 1}))
          (let [env (conversations/env-for id)
                history (db/db-var-history (:db-info env) (:conversation-ref env) '*reasoning*)]
            (expect (= 2 (count history)))
            (expect (= 1 (:version (first history))))
            (expect (= "Query 1 reasoning" (:value (first history))))
            (expect (= 2 (:version (second history))))
            (expect (= "Query 2 reasoning" (:value (second history)))))
          (conversations/delete! id)))))

  (describe "version resource"
    (it "config/version reads from resources/vis-version.txt"
      (expect (string? config/version))
      (expect (re-matches #"\d+\.\d+\.\d+.*" config/version)))))
