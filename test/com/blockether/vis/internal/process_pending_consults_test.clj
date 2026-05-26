(ns com.blockether.vis.internal.process-pending-consults-test
  "Parallel runner integration tests.

   `process-pending-consults!` drains intents, spawns one future per
   intent, awaits all, and appends each result as a synthetic
   trailer pin via `consult/append-resolution-pin!`. Tests use an
   injectable `:consult-runner` so they don't hit the real LLM."
  (:require
   [com.blockether.vis.internal.consult :as consult]
   [com.blockether.vis.internal.ctx-loop :as cl]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- mk-env []
  {:ctx-atom (atom {:session/id "test"
                    :session/turn 1
                    :session/scope {:turn 1 :iter 1 :next-form 1}
                    :session/specs {}
                    :session/tasks {}
                    :session/facts {}
                    :session/trailer []})
   :consult-budget-atom (consult/fresh-budget-atom)
   :current-form-scope "t1/i1/f1"
   :active-extensions []})

(defn- stub-runner
  "Returns a runner that pretends to do work for a known duration and
   stamps the captured snapshot back onto the result so tests can
   verify thread-isolation."
  [{:keys [sleep-ms]}]
  (fn [_env intent]
    (when sleep-ms (Thread/sleep ^long sleep-ms))
    {:id  (:id intent)
     :status      :active
     :content     (str "answer for " (:id intent))
     :citations   []
     :confidence  :medium
     :focus       (:focus intent)
     :focus-met?  []
     :preference  (:preference intent)
     :duration-ms 1
     :retries     0
     :saw-snapshot (:ctx-snapshot intent)}))

(defn- trailer-consult-pins [ctx]
  (for [iter-pin (or (:session/trailer ctx) [])
        form     (or (:forms iter-pin) [])
        :when (= :consult (:tag form))]
    form))

;; ---------------------------------------------------------------------------
;; fan out + materialise as trailer pins
;; ---------------------------------------------------------------------------

(defdescribe parallel-drain-test
  (describe "3 intents → 3 trailer pins"
    (let [env (assoc (mk-env) :consult-runner (stub-runner {}))]
      (consult/request-consult! env :K1 :fast    {:question "q1"})
      (consult/request-consult! env :K2 :balanced {:question "q2"})
      (consult/request-consult! env :K3 :deep    {:question "q3"})

      (let [processed (cl/process-pending-consults! env)
            ctx       @(:ctx-atom env)
            pins      (vec (trailer-consult-pins ctx))]

        (it "returns the vec of processed consult-ids"
          (expect (= 3 (count processed)))
          (expect (= #{:K1 :K2 :K3} (set processed))))

        (it "trailer carries one synthetic consult pin per intent"
          (expect (= 3 (count pins)))
          (expect (= #{:K1 :K2 :K3} (set (map :id pins)))))

        (it ":engine/pending-consults drained to empty"
          (expect (empty? (:engine/pending-consults ctx))))

        (it "each pin carries :status :active + non-blank :content on :result"
          (doseq [p pins]
            (expect (= :active (-> p :result :status)))
            (expect (string? (-> p :result :content)))
            (expect (not (clojure.string/blank? (-> p :result :content)))))))))

  (describe "no pending → no-op (returns nil)"
    (let [env (assoc (mk-env) :consult-runner (stub-runner {}))]
      (it "returns nil"
        (expect (nil? (cl/process-pending-consults! env))))
      (it "no trailer pins"
        (expect (empty? (trailer-consult-pins @(:ctx-atom env))))))))

;; ---------------------------------------------------------------------------
;; parallel: wall-clock ≈ max(per-intent durations), not sum
;; ---------------------------------------------------------------------------

(defdescribe parallel-wallclock-test
  (describe "5 intents @ 100ms each → wall time well under 5*100ms"
    (let [env (assoc (mk-env) :consult-runner (stub-runner {:sleep-ms 100}))]
      (doseq [i (range 5)]
        (consult/request-consult! env (keyword (str "K" i)) :fast
          {:question "q"}))
      (let [start (System/nanoTime)
            _ (cl/process-pending-consults! env)
            elapsed-ms (/ (- (System/nanoTime) start) 1e6)]
        (it "elapsed-ms is closer to one tick than to N ticks"
          (expect (< elapsed-ms 350)))))))

;; ---------------------------------------------------------------------------
;; thread isolation: snapshot pinned at request time
;; ---------------------------------------------------------------------------

(defdescribe thread-isolation-test
  (describe "consult side sees REQUEST-TIME snapshot, not live ctx-atom"
    (let [env (assoc (mk-env) :consult-runner (stub-runner {:sleep-ms 50}))
          _ (swap! (:ctx-atom env) assoc-in
              [:session/facts :before] {:content "pre-snapshot" :status :active})
          _ (consult/request-consult! env :K :deep {:question "q"})
          _ (swap! (:ctx-atom env) assoc-in
              [:session/facts :after] {:content "POST-enqueue mutation"
                                       :status :active})
          processed (cl/process-pending-consults! env)
          pin (first (filter #(= :K (:id %))
                       (trailer-consult-pins @(:ctx-atom env))))
          seen (-> pin :result :saw-snapshot)]

      (it "result materialised under the same id"
        (expect (= [:K] processed)))

      (it "saw-snapshot CONTAINS the pre-enqueue fact :before"
        (expect (clojure.string/includes? (str seen) ":before"))
        (expect (clojure.string/includes? (str seen) "pre-snapshot")))

      (it "saw-snapshot does NOT contain the POST-enqueue mutation"
        (expect (not (clojure.string/includes? (str seen)
                       "POST-enqueue mutation"))))

      (it "the live ctx-atom DOES carry the POST mutation (sanity check)"
        (expect (= "POST-enqueue mutation"
                  (get-in @(:ctx-atom env)
                    [:session/facts :after :content])))))))

;; ---------------------------------------------------------------------------
;; failure propagation: failed entries land as trailer pins too
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; on-chunk emission: each resolved consult fires :phase :consult-resolved
;; ---------------------------------------------------------------------------

(defdescribe on-chunk-emission-test
  (describe "process-pending-consults! fires one :consult-resolved chunk per intent"
    (let [env (assoc (mk-env) :consult-runner (stub-runner {}))
          chunks (atom [])
          on-chunk (fn [c] (swap! chunks conj c))]
      (consult/request-consult! env :K1 :fast {:question "q1"})
      (consult/request-consult! env :K2 :deep {:question "q2"})
      (cl/process-pending-consults! env on-chunk)

      (it "one chunk per resolved intent"
        (expect (= 2 (count @chunks))))

      (it "chunk :phase is :consult-resolved"
        (doseq [c @chunks]
          (expect (= :consult-resolved (:phase c)))))

      (it "chunk carries :id + :tag :consult + :scope + :result + :iteration-count"
        (let [ids (set (map :id @chunks))]
          (expect (= #{:K1 :K2} ids)))
        (doseq [c @chunks]
          (expect (= :consult (:tag c)))
          (expect (re-matches #"t\d+/i\d+/c-.*" (:scope c)))
          (expect (integer? (:iteration-count c)))
          (expect (= :active (-> c :result :status)))))))

  (describe "omitting on-chunk is fine (1-arity path)"
    (let [env (assoc (mk-env) :consult-runner (stub-runner {}))]
      (consult/request-consult! env :K :fast {:question "q"})
      (it "runs without errors and still appends pin"
        (expect (= [:K] (cl/process-pending-consults! env)))
        (expect (= 1 (count (trailer-consult-pins @(:ctx-atom env)))))))))

(defdescribe failed-runner-propagation-test
  (describe "a runner that returns :status :failed lands the pin"
    (let [env (assoc (mk-env)
                :consult-runner
                (fn [_env intent]
                  {:id (:id intent)
                   :status :failed
                   :error :provider-error
                   :reason "stub error"
                   :focus (:focus intent)
                   :preference (:preference intent)
                   :duration-ms 0
                   :retries 0}))]
      (consult/request-consult! env :K :fast {:question "q"})
      (cl/process-pending-consults! env)
      (let [pin (first (trailer-consult-pins @(:ctx-atom env)))]
        (it "trailer pin synthesized with :status :failed"
          (expect (= :failed (-> pin :result :status))))
        (it ":error + :reason preserved on :result"
          (expect (= :provider-error (-> pin :result :error)))
          (expect (= "stub error" (-> pin :result :reason))))))))
