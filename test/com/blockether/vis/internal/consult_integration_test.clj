(ns com.blockether.vis.internal.consult-integration-test
  "End-to-end integration of the consult subsystem.

   Stitches together the data-plumbing layer (consult.clj), the
   parallel runner (ctx-loop/process-pending-consults!), the done
   gate (ctx-engine/apply-done), and the trailer pin synthesis.
   No real LLM is involved; `:consult-runner` is injected so the
   test drives outcomes deterministically.

   Scenarios:
     - N intents in one turn → N synthetic trailer pins
     - consult-promote! moves entry from trailer pin → fact
     - consult-dismiss! drops the pin without promoting
     - done-gate refusal while consults are pending"
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.consult :as consult]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-loop :as cl]
   [com.blockether.vis.internal.ctx-renderer :as renderer]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- mk-env [overrides]
  (merge {:ctx-atom (atom {:session/id "test"
                           :session/turn 1
                           :session/scope {:turn 1 :iter 1 :next-form 1}
                           :session/specs {} :session/tasks {}
                           :session/facts {} :session/trailer []})
          :consult-budget-atom (consult/fresh-budget-atom)
          :current-form-scope "t1/i1/f1"
          :active-extensions []}
    overrides))

(defn- runner-returning [shape-fn]
  (fn [_env intent]
    (shape-fn intent)))

(defn- stub-success [intent]
  {:id (:id intent)
   :status :active
   :content (str "synthesis for " (:id intent))
   :citations [{:type :paper :url "https://arxiv.org/abs/2303.11366"
                :title "Reflexion"}]
   :confidence :high
   :focus (:focus intent)
   :focus-met? (vec (map (constantly true) (:focus intent)))
   :preference (:preference intent)
   :duration-ms 1
   :retries 0})

(defn- trailer-consult-pins [ctx]
  (for [iter-pin (or (:session/trailer ctx) [])
        form     (or (:forms iter-pin) [])
        :when (= :consult (:tag form))]
    form))

;; ---------------------------------------------------------------------------
;; N intents in one turn → N synthetic trailer pins next iter
;; ---------------------------------------------------------------------------

(defdescribe N-intents-one-turn-test
  (describe "primary declares 3 consults; engine resolves all by iter end"
    (let [env (mk-env {:consult-runner (runner-returning stub-success)})]
      (consult/request-consult! env :critique :fast
        {:question "Critique my draft"})
      (consult/request-consult! env :research :deep
        {:focus ["91% claim"] :question "Verify reflexion HumanEval result"})
      (consult/request-consult! env :review-A :balanced
        {:question "Architectural review"})

      (cl/process-pending-consults! env)

      (let [ctx @(:ctx-atom env)
            pins (vec (trailer-consult-pins ctx))
            ids (set (map :id pins))]

        (it "all 3 consult ids materialised as trailer pins"
          (expect (= 3 (count pins)))
          (expect (= #{:critique :research :review-A} ids)))

        (it ":engine/pending-consults drained to empty"
          (expect (empty? (:engine/pending-consults ctx))))

        (it "each pin carries :tag :consult + entry-shaped :result"
          (doseq [p pins]
            (expect (= :consult (:tag p)))
            (expect (= :active (-> p :result :status)))
            (expect (string? (-> p :result :content)))
            (expect (not (str/blank? (-> p :result :content))))))

        (it "renderer surfaces trailer pins inline"
          (let [text (renderer/render-ctx
                       {:ctx ctx :warnings []
                        :progression {} :next-actions []})]
            (expect (str/includes? text ":session/trailer"))
            (expect (str/includes? text ":critique"))
            (expect (str/includes? text ":research"))))))))

;; ---------------------------------------------------------------------------
;; consult-promote! → fact + scrub trailer pin
;; ---------------------------------------------------------------------------

(defdescribe promote-roundtrip-test
  (describe "request → resolve → promote"
    (let [env (mk-env {:consult-runner (runner-returning stub-success)})
          _   (consult/request-consult! env :reflexion :deep
                {:focus ["91% claim"] :question "Summarize Reflexion paper"})
          _   (cl/process-pending-consults! env)
          ;; The pin is now in the trailer; the model can read it
          ;; inline. We assert the pin exists, then promote.
          ctx-after-resolve @(:ctx-atom env)
          pin-pre-promote (consult/find-trailer-consult-pin
                            ctx-after-resolve :reflexion)
          _ (consult/promote-consult! env :reflexion :reflexion-paper)
          ctx-final @(:ctx-atom env)]

      (it "consult pin exists in trailer after resolution"
        (expect (some? pin-pre-promote))
        (expect (= :active (-> pin-pre-promote :result :status))))

      (it "fact materialised under :session/facts :reflexion-paper"
        (let [fact (-> ctx-final :session/facts :reflexion-paper)]
          (expect (string? (:content fact)))
          (expect (= :active (:status fact)))
          (expect (= :consult (:source fact)))))

      (it "trailer pin scrubbed (no more :reflexion in trailer)"
        (expect (empty? (filter #(= :reflexion (:id %))
                          (trailer-consult-pins ctx-final))))))))

;; ---------------------------------------------------------------------------
;; consult-dismiss! drops the pin
;; ---------------------------------------------------------------------------

(defdescribe dismiss-test
  (describe "request → resolve → dismiss"
    (let [env (mk-env {:consult-runner (runner-returning stub-success)})
          _   (consult/request-consult! env :research :deep
                {:question "background research"})
          _   (cl/process-pending-consults! env)
          _   (consult/dismiss-consult! env :research)
          ctx-final @(:ctx-atom env)]

      (it "trailer pin scrubbed"
        (expect (empty? (filter #(= :research (:id %))
                          (trailer-consult-pins ctx-final))))))

    (it "no fact created"
      (expect (empty? (:session/facts @(:ctx-atom (mk-env {})))))))

  (describe "promote on failed entry refuses + warns"
    (let [env (mk-env {:consult-runner
                       (fn [_env intent]
                         {:id (:id intent)
                          :status :failed :error :timeout
                          :reason "30s ttft"
                          :focus (:focus intent)
                          :preference (:preference intent)
                          :duration-ms 0 :retries 0})})
          _ (consult/request-consult! env :K :fast {:question "q"})
          _ (cl/process-pending-consults! env)
          _ (consult/promote-consult! env :K :K-fact)
          warnings (-> env :ctx-atom deref :engine/warnings)]
      (it ":consult-promote-failed warning"
        (expect (some #(= :consult-promote-failed (:code %)) warnings)))
      (it "no fact created"
        (expect (nil? (-> env :ctx-atom deref :session/facts :K-fact)))))))

;; ---------------------------------------------------------------------------
;; done-gate
;; ---------------------------------------------------------------------------

(defdescribe done-gate-integration-test
  (describe "done refused while pending"
    (let [env (mk-env {})]
      (consult/request-consult! env :research :deep
        {:question "background research"})

      (let [ctx @(:ctx-atom env)
            result (eng/apply-done ctx "t1/i1/f9" {})]

        (it "apply-done returns :blocked? true"
          (expect (true? (:blocked? result))))

        (it ":done-blocked-by-pending-consults warning emitted"
          (expect (some #(= :done-blocked-by-pending-consults (:code %))
                    (:warnings result)))))))

  (describe "done proceeds after the runner resolves the futures"
    (let [env (mk-env {:consult-runner (runner-returning stub-success)})]
      (consult/request-consult! env :research :deep {:question "q"})
      (cl/process-pending-consults! env)

      (let [ctx @(:ctx-atom env)
            result (eng/apply-done ctx "t1/i1/f9" {})]

        (it "no :blocked? after the runner resolved"
          (expect (not (:blocked? result))))))))
