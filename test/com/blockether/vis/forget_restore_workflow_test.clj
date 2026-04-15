(ns com.blockether.vis.forget-restore-workflow-test
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
          ;; Seed the sandbox with two defs as if a prior iteration had run.
          (sci/eval-string+ (:sci-ctx env)
            "(def keep-var 99) (def drop-me 42)"
            {:ns (sci/find-ns (:sci-ctx env) 'sandbox)})
          (expect (= #{"keep-var" "drop-me"}
                    (clojure.set/intersection
                      (sandbox-names env) #{"keep-var" "drop-me"})))
          ;; One iteration: no code, just :forget + final.
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
          ;; Turn 1 — two iterations: first defs `memory`, second finals. The
          ;; :iteration-var snapshot persists `memory` to SQLite.
          (with-redefs [llm/ask! (scripted-llm
                                   [{:code [{:expr "(def memory \"hello\")"
                                             :time-ms 1000}]}
                                    {:final {:answer "defined"
                                             :confidence "high"}}])]
            (conv/send! id "remember this" {:max-iterations 3}))
          (expect (= "hello" (sandbox-value env 'memory)))
          ;; Turn 2 — one iteration: forget it + final.
          (with-redefs [llm/ask! (scripted-llm
                                   [{:forget ["memory"]
                                     :final {:answer "forgotten"
                                             :confidence "high"}}])]
            (conv/send! id "drop it" {:max-iterations 1}))
          (expect (nil? (sandbox-value env 'memory)))
          ;; Turn 3 — two iterations: first calls (restore-var 'memory) to
          ;; pull it back from the DB into the sandbox, second finals. If
          ;; :forget had deleted the iteration-var row, restore-var would
          ;; throw here.
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
                (rlm/dispose-env! env)))))))))
