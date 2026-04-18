(ns com.blockether.vis.test-helpers
  "Shared test helpers for vis test suites."
  (:require
   [com.blockether.svar.internal.llm :as llm]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.loop.core :as rlm-core]))

;; =============================================================================
;; Test routers
;; =============================================================================

(defn make-stub-router
  "A router that points nowhere — for DB-only tests that don't call LLMs."
  []
  (llm/make-router [{:id       :stub
                      :api-key  "stub"
                      :base-url "http://localhost:1"
                      :models   [{:name "stub-model"}]}]))

;; =============================================================================
;; Test environments
;; =============================================================================

(defn with-temp-env
  "Creates a temp RLM environment via the public API, calls `(f env)`, disposes.
   Uses a stub router — no LLM calls. For DB-only tests."
  [f]
  (let [env (vis/create-env (make-stub-router) {:db :temp})]
    (try
      (f env)
      (finally
        (vis/dispose-env! env)))))

(defn with-temp-raw-env
  "Creates a temp RLM environment via the internal API, calls `(f env)`, disposes.
   Uses a stub router. For tests that need internal env structure."
  ([f] (with-temp-raw-env {} f))
  ([opts f]
   (let [opts   (if (contains? opts :db) opts (assoc opts :db :temp))
         router (make-stub-router)]
     (let [env (#'rlm-core/create-rlm-env (atom 0) router opts)]
       (try
         (f env)
         (finally
           (#'rlm-core/dispose-rlm-env! env)))))))
