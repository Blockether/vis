(ns com.blockether.vis.loop.iteration-persistence-test
  "Regression tests for iteration-persistence invariants.

   Bug caught here (conversation 33b6d8ae-8acb-4f77-a591-552dfeec7ac4):
   every empty iteration used to be persisted TWICE. The unconditional
   write inside the success branch already covered empty iterations (with
   `:executions []` and full vars-snapshot), but the empty-branch below
   fired a second `store-iteration!` with `:executions nil` and `:vars []`.
   Downstream: SQLite grew a ghost row per empty iteration, sheet view
   showed duplicates, any iteration-count aggregate was off by
   N-empty-iterations.

   These tests pin down the invariants with a mocked `store-iteration!`
   that counts calls — no real SQLite writer, no real LLM. The iteration
   loop is driven by a scripted `run-iteration` so each scenario is
   deterministic.

   Invariants under test:
     1. A single iteration that returns `:executions []` and no `:final` is
        persisted exactly ONCE (not twice).
     2. A normal iteration with executions is persisted exactly once.
     3. A final-result iteration is persisted exactly once."
  (:require
    [lazytest.core :refer [defdescribe describe expect it]]
    [com.blockether.svar.internal.llm :as llm]
    [com.blockether.vis.core :as sut]
    [com.blockether.vis.loop.core :as rlm-core]
    [com.blockether.vis.loop.storage.db :as rlm-db]
    [com.blockether.vis.loop.runtime.query.routing :as rlm-routing]))

;; =============================================================================
;; Test harness
;; =============================================================================

(defn- make-test-env
  "A minimal env for exercising iteration-loop without hitting real SQLite
   or a real LLM. Uses `:db :temp` so schema migration runs against an
   ephemeral SQLite file. The leading atom is the same sub-RLM
   recursion-depth atom the private 3-ary `create-rlm-env` expects."
  []
  (let [router (llm/make-router
                 [{:id :test :api-key "test" :base-url "http://localhost"
                   :models [{:name "gpt-4o"}]}])]
    (#'rlm-core/create-rlm-env (atom 0) router {:db :temp})))

(defn- scripted-run-iteration
  "Return a `run-iteration` replacement that pops one fake-iteration
   result per call from `scripts` (vector). When a script is shorter than
   the iteration budget, the loop simply runs out of fake results — test
   should set `:max-iterations` to match."
  [scripts]
  (let [n (atom 0)]
    (fn [& _]
      (let [i (dec (swap! n inc))
            r (get scripts i)]
        (assert r (str "scripted-run-iteration ran out of scripted results at call #" i))
        r))))

(defn- run-with-mocks!
  "Execute `(query-env! env [(llm/user q)] opts)` with:
     - provider-has-reasoning? faked
     - run-iteration replaced by `script` (vector of fake iteration results)
     - store-iteration! counting every call into `counter-atom`

   Returns the query-env! result. The counter-atom is side-channel."
  [env q opts counter-atom script]
  (let [real-store rlm-db/store-iteration!]
    (with-redefs [rlm-routing/provider-has-reasoning? (constantly true)
                  rlm-core/run-iteration (scripted-run-iteration script)
                  rlm-db/store-iteration! (fn [& args]
                                            (swap! counter-atom inc)
                                            (apply real-store args))]
      (sut/query-env! env [(llm/user q)] opts))))

(defn- with-env! [f]
  (let [env (make-test-env)]
    (try (f env)
      (finally (#'rlm-core/dispose-rlm-env! env)))))

;; =============================================================================
;; Canonical fake iteration results
;; =============================================================================

(def ^:private fake-empty
  "One iteration returning neither code nor a final answer — the exact
   shape that caused the double-persist bug."
  {:thinking "thinking out loud but no code" :executions [] :final-result nil
   :api-usage nil :duration-ms 0})

(def ^:private fake-code
  "One iteration with a successful execution and no final."
  {:thinking "running (+ 1 1)"
   :executions [{:id 0 :code "(+ 1 1)" :result 2
                 :stdout "" :stderr "" :error nil :execution-time-ms 1}]
   :final-result nil :api-usage nil :duration-ms 0})

(def ^:private fake-final
  "A final-answer iteration — loop terminates after this."
  {:thinking "done"
   :executions []
   :final-result {:answer {:result "ok" :type String} :confidence :high}
   :api-usage nil :duration-ms 0})

;; =============================================================================
;; Tests
;; =============================================================================

(defdescribe iteration-persistence-invariants
  (describe "store-iteration! call count"
    (it "writes EXACTLY ONCE for a single empty iteration (regression)"
      ;; The historical bug: empty-iteration branch called store-iteration!
      ;; a SECOND time with :executions nil. Scenario: one empty iteration
      ;; followed by a final. If the bug returned, count would be 3.
      (with-env!
        (fn [env]
          (let [cnt (atom 0)]
            (run-with-mocks! env "please" {:max-iterations 2 :refine? false}
              cnt
              [fake-empty fake-final])
            (expect (= 2 @cnt))))))

    (it "writes EXACTLY ONCE per non-empty iteration"
      (with-env!
        (fn [env]
          (let [cnt (atom 0)]
            (run-with-mocks! env "please" {:max-iterations 3 :refine? false}
              cnt
              [fake-code fake-code fake-final])
            (expect (= 3 @cnt))))))

    (it "writes EXACTLY ONCE for mixed empty + code iterations"
      ;; Scenario from conversation 33b6d8ae…: code, empty, empty, final.
      ;; Pre-fix count would be 6 (empties double-counted). Post-fix: 4.
      (with-env!
        (fn [env]
          (let [cnt (atom 0)]
            (run-with-mocks! env "please" {:max-iterations 4 :refine? false}
              cnt
              [fake-code fake-empty fake-empty fake-final])
            (expect (= 4 @cnt))))))

    (it "writes EXACTLY ONCE for an immediate-final iteration"
      (with-env!
        (fn [env]
          (let [cnt (atom 0)]
            (run-with-mocks! env "please" {:max-iterations 1 :refine? false}
              cnt
              [fake-final])
            (expect (= 1 @cnt)))))))

  (describe "tool-error recovery — next iteration carries the error text"
    ;; Scenario from conversation 33b6d8ae…: one execution raised
    ;; `grep path must be a directory` and the next iteration had to see
    ;; that error text verbatim in its user message so the model could
    ;; recover (switch to read-file). If the loop silently swallowed the
    ;; error, the model would repeat the same broken call.
    (it "error message from a failed execution propagates into the next iteration's messages"
      (with-env!
        (fn [env]
          (let [captured (atom [])
                fake-err  {:thinking "try grep on a file"
                           :executions [{:id 0
                                         :code "(grep \"x\" \"/tmp/file.css\")"
                                         :result nil
                                         :stdout "" :stderr ""
                                         :error "ExceptionInfo: grep path must be a directory"
                                         :execution-time-ms 1}]
                           :final-result nil :api-usage nil :duration-ms 0}]
            (with-redefs [rlm-routing/provider-has-reasoning? (constantly true)
                          rlm-core/run-iteration (let [n (atom 0)]
                                                   (fn [_ messages _opts]
                                                     (swap! captured conj messages)
                                                     (if (zero? (first (swap-vals! n inc)))
                                                       fake-err
                                                       fake-final)))]
              (sut/query-env! env [(llm/user "please")]
                {:max-iterations 2 :refine? false}))
            ;; Second iteration's messages must mention the error text so
            ;; the model can adapt its strategy.
            (let [second-iter-msgs (nth @captured 1)
                  all-text (apply str (keep :content second-iter-msgs))]
              (expect (clojure.string/includes? all-text "ExceptionInfo"))
              (expect (clojure.string/includes? all-text "grep path must be a directory")))))))))
