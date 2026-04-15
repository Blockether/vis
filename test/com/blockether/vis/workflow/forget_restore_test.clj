(ns com.blockether.vis.workflow.forget-restore-test
  "End-to-end workflow test for the :forget iteration-spec field and the
   round-trip with (restore-var …).

   Exercises the full path — `conv/send!` → rlm iteration loop →
   LLM-structured response → `:forget` applied to the SCI sandbox — with
   the LLM stubbed via `with-redefs` on `llm/ask!`. No network, no real
   model, but every other layer (SQLite, SCI sandbox, iteration-var
   persistence) runs for real."
  (:require [babashka.fs :as fs]
            [lazytest.core :refer [defdescribe describe expect it]]
            [com.blockether.svar.internal.llm :as llm]
            [com.blockether.vis.config :as config]
            [com.blockether.vis.conversations :as conv]
            [com.blockether.vis.rlm :as rlm]
            [sci.core :as sci]))

;;; ── Helpers ─────────────────────────────────────────────────────────────

(defn- sandbox-names
  "Return the set of sym-name strings currently bound in the conversation's
   SCI sandbox namespace. Used by assertions to check what's present after
   an iteration."
  [env]
  (into #{}
    (map str)
    (keys (get-in @(:env (:sci-ctx env)) [:namespaces 'sandbox]))))

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
   one at a time, in order. Throws if the rlm asks for more iterations than
   the script covers — makes under-specified scripts fail loudly."
  [responses]
  (let [queue (atom (vec responses))]
    (fn [_router _opts]
      (let [[head & rest] @queue]
        (when-not head
          (throw (ex-info "scripted-llm exhausted" {})))
        (reset! queue (vec rest))
        (merge (zero-usage) {:result head})))))

(defn- with-temp-db*
  "Point `config/db-path` at a brand-new temp directory and reset the
   conversations cache so nothing from a prior test leaks in. Restores
   original path + closes opened envs in finally. Each test gets a pristine
   DB."
  [f]
  (let [original-path config/db-path
        temp-dir (str (fs/create-temp-dir {:prefix "vis-workflow-"}))]
    (try
      (alter-var-root #'config/db-path (constantly temp-dir))
      (conv/close-all!)
      (f)
      (finally
        (conv/close-all!)
        (alter-var-root #'config/db-path (constantly original-path))
        (fs/delete-tree temp-dir)))))

(defmacro ^:private with-temp-db [& body]
  `(with-temp-db* (fn [] ~@body)))

(def ^:private integration-providers
  [{:id :openai
    :api-key (System/getenv "OPENAI_API_KEY")
    :base-url (or (System/getenv "OPENAI_BASE_URL")
                "https://api.openai.com/v1")
    :models [{:name "gpt-4o"}
             {:name "gpt-4o-mini"}]}])

(defn- live-integration-enabled?
  []
  (some? (:api-key (first integration-providers))))

;;; ── Tests ───────────────────────────────────────────────────────────────

(defdescribe forget-workflow-test
  (describe ":forget drops a live sandbox var during a turn"
    (it "unmaps the named sym from the SCI sandbox"
      (with-temp-db
        (let [{id :id} (conv/create! :cli {:title "forget live var"})
              env (conv/env-for id)]
          (sci/eval-string+ (:sci-ctx env)
            "(def keep-var 99) (def drop-me 42)"
            {:ns (sci/find-ns (:sci-ctx env) 'sandbox)})
          (expect (= #{"keep-var" "drop-me"}
                    (clojure.set/intersection
                      (sandbox-names env) #{"keep-var" "drop-me"})))
          (with-redefs [llm/ask! (scripted-llm
                                   [{:forget ["drop-me"]
                                     :final {:answer "forgotten"
                                             :confidence "high"}}])]
            (conv/send! id "drop it" {:max-iterations 1}))
          (let [names (sandbox-names env)]
            (expect (contains? names "keep-var"))
            (expect (not (contains? names "drop-me"))))
          (conv/delete! id))))

    (it "leaves the :iteration-var row in the DB so it can be restored later"
      (with-temp-db
        (let [{id :id} (conv/create! :cli {:title "forget-preserves-db"})
              env (conv/env-for id)]
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def memory \"hello\")"
                                             :time-ms 1000}]}
                                    {:final {:answer "defined"
                                             :confidence "high"}}])]
            (conv/send! id "remember this" {:max-iterations 3}))
          (expect (= "hello" (sandbox-value env 'memory)))
          (with-redefs [llm/ask! (scripted-llm
                                   [{:forget ["memory"]
                                     :final {:answer "forgotten"
                                             :confidence "high"}}])]
            (conv/send! id "drop it" {:max-iterations 1}))
          (expect (nil? (sandbox-value env 'memory)))
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(restore-var 'memory)"
                                             :time-ms 1000}]}
                                    {:final {:answer "restored"
                                             :confidence "high"}}])]
            (conv/send! id "bring it back" {:max-iterations 3}))
          (expect (= "hello" (sandbox-value env 'memory)))
          (conv/delete! id))))

    (it "handles a response with no :forget as a no-op"
      (with-temp-db
        (let [{id :id} (conv/create! :cli {:title "no forget"})
              env (conv/env-for id)]
          (sci/eval-string+ (:sci-ctx env)
            "(def survivor 1)"
            {:ns (sci/find-ns (:sci-ctx env) 'sandbox)})
          (with-redefs [llm/ask! (scripted-llm
                                   [{:final {:answer "ok"
                                             :confidence "high"}}])]
            (conv/send! id "nothing to forget" {:max-iterations 1}))
          (expect (contains? (sandbox-names env) "survivor"))
          (conv/delete! id))))

    (it "recovers when a cleanup claim is rejected and the next iteration emits :forget"
      (with-temp-db
        (let [{id :id} (conv/create! :cli {:title "forget recovery"})
              env (conv/env-for id)]
          (sci/eval-string+ (:sci-ctx env)
            "(def keep-var 99) (def drop-me 42)"
            {:ns (sci/find-ns (:sci-ctx env) 'sandbox)})
          (with-redefs [llm/ask! (scripted-llm
                                   [{:final {:answer "Posprzatane, usunalem vars z indexu."
                                             :confidence "high"}}
                                    {:forget ["drop-me"]
                                     :final {:answer "FORGOTTEN"
                                             :confidence "high"}}])]
            (let [result (conv/send! id "drop it" {:max-iterations 3})
                  names (sandbox-names env)]
              (expect (= "FORGOTTEN" (:answer result)))
              (expect (= 2 (:iterations result)))
              (expect (contains? names "keep-var"))
              (expect (not (contains? names "drop-me")))))
          (conv/delete! id))))

    (it "live model can still complete a real forget workflow"
      (when (live-integration-enabled?)
        (with-temp-db
          (let [router (llm/make-router integration-providers)
                env (rlm/create-env router {:db config/db-path})]
            (try
              (#'com.blockether.vis.rlm.core/execute-code env "(def drop-me 42)")
              (let [result (rlm/query-env! env [(llm/user "Forget variable drop-me, then reply with exactly FORGOTTEN.")]
                             {:max-iterations 4
                              :refine? false
                              :system-prompt "If you claim a variable was forgotten, you must emit :forget in the same iteration. Reply with exactly FORGOTTEN after the forget succeeds."})
                    names (sandbox-names env)]
                (expect (re-find #"FORGOTTEN" (str (:answer result))))
                (expect (not (contains? names "drop-me"))))
              (finally
                (rlm/dispose-env! env))))))))

;;; ── Auto-forget integration tests ──────────────────────────────────────

  (describe "deterministic auto-forget at query boundary"
    (it "evicts undocumented vars after they go stale (not touched in 3 queries)"
      (with-temp-db
        (let [{id :id} (conv/create! :cli {:title "auto-forget stale"})
              trivial-final {:final {:answer "ok" :confidence "high"}}]
          ;; Query 1: define an undocumented scratch var
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def scratch 42)" :time-ms 100}]}
                                    {:final {:answer "defined" :confidence "high"}}])]
            (conv/send! id "define scratch" {:max-iterations 3}))
          (let [env (conv/env-for id)]
            (expect (= 42 (sandbox-value env 'scratch))))

          ;; Queries 2, 3, 4: trivial queries that don't touch scratch
          (dotimes [_ 3]
            (with-redefs [llm/ask! (scripted-llm [trivial-final])]
              (conv/send! id "noop" {:max-iterations 1})))

          ;; After query 4: scratch was defined in query 1, which is now
          ;; 3 queries behind (queries 2, 3, 4 are the recent 3).
          ;; Auto-forget should have evicted it during query 4's startup.
          (let [env (conv/env-for id)
                names (sandbox-names env)]
            (expect (not (contains? names "scratch"))))
          (conv/delete! id))))

    (it "preserves documented vars regardless of staleness"
      (with-temp-db
        (let [{id :id} (conv/create! :cli {:title "auto-forget preserves docs"})
              trivial-final {:final {:answer "ok" :confidence "high"}}]
          ;; Query 1: define a var WITH docstring
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def config \"app configuration\" {:port 3000})"
                                             :time-ms 100}]}
                                    {:final {:answer "defined" :confidence "high"}}])]
            (conv/send! id "define config" {:max-iterations 3}))

          ;; Queries 2, 3, 4: trivial
          (dotimes [_ 3]
            (with-redefs [llm/ask! (scripted-llm [trivial-final])]
              (conv/send! id "noop" {:max-iterations 1})))

          ;; config should STILL be in the sandbox — it has a docstring
          (let [env (conv/env-for id)]
            (expect (= {:port 3000} (sandbox-value env 'config))))
          (conv/delete! id))))

    (it "auto-forgotten vars can be restored via restore-var"
      (with-temp-db
        (let [{id :id} (conv/create! :cli {:title "auto-forget restorable"})
              trivial-final {:final {:answer "ok" :confidence "high"}}]
          ;; Query 1: define undocumented var
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def ephemeral \"hello\")" :time-ms 100}]}
                                    {:final {:answer "defined" :confidence "high"}}])]
            (conv/send! id "define ephemeral" {:max-iterations 3}))
          (let [env (conv/env-for id)]
            (expect (= "hello" (sandbox-value env 'ephemeral))))

          ;; Queries 2, 3, 4: go stale
          (dotimes [_ 3]
            (with-redefs [llm/ask! (scripted-llm [trivial-final])]
              (conv/send! id "noop" {:max-iterations 1})))

          ;; Verify it's gone
          (let [env (conv/env-for id)]
            (expect (nil? (sandbox-value env 'ephemeral))))

          ;; Query 5: restore it
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(restore-var 'ephemeral)" :time-ms 100}]}
                                    {:final {:answer "restored" :confidence "high"}}])]
            (conv/send! id "bring it back" {:max-iterations 3}))

          ;; Should be back with original value
          (let [env (conv/env-for id)]
            (expect (= "hello" (sandbox-value env 'ephemeral))))
          (conv/delete! id))))

    (it "does not evict vars defined in recent queries"
      (with-temp-db
        (let [{id :id} (conv/create! :cli {:title "auto-forget recent safe"})
              trivial-final {:final {:answer "ok" :confidence "high"}}]
          ;; Query 1: trivial
          (with-redefs [llm/ask! (scripted-llm [trivial-final])]
            (conv/send! id "noop" {:max-iterations 1}))

          ;; Query 2: define undocumented var
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def recent-var 77)" :time-ms 100}]}
                                    {:final {:answer "defined" :confidence "high"}}])]
            (conv/send! id "define recent-var" {:max-iterations 3}))

          ;; Query 3: trivial — recent-var is only 1 query old, within the 3-query window
          (with-redefs [llm/ask! (scripted-llm [trivial-final])]
            (conv/send! id "noop" {:max-iterations 1}))

          ;; recent-var should still be there (defined in query 2, queries 1,2,3 are recent)
          (let [env (conv/env-for id)]
            (expect (= 77 (sandbox-value env 'recent-var))))
          (conv/delete! id))))))