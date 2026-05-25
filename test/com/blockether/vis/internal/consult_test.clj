(ns com.blockether.vis.internal.consult-test
  "Async cross-iter consult protocol.

   Tests the data-plumbing layer:
     - `request-consult!`  — async push, validation, :vis/silent return
     - trailer pin synthesis / lookup / scrub
     - `consult-promote!`  — copy to fact, scrub trailer pin
     - `consult-dismiss!`  — drop, scrub trailer pin"
  (:require
   [com.blockether.vis.internal.consult :as consult]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- mk-env
  ([] (mk-env {}))
  ([overrides]
   (merge {:ctx-atom (atom {:session/id "test"
                            :session/turn 1
                            :session/scope {:turn 1 :iter 1 :next-form 1}
                            :session/facts {}
                            :session/trailer []})
           :consult-budget-atom (consult/fresh-budget-atom)
           :current-form-scope "t1/i1/f1"}
     overrides)))

(defn- warnings-of [env]
  (-> env :ctx-atom deref :engine/warnings (or [])))

(defn- seed-pin!
  "Helper: install a consult-resolution pin on the env's trailer as if
   the runner had just resolved one."
  [env entry]
  (consult/append-resolution-pin! (:ctx-atom env) entry))

;; ---------------------------------------------------------------------------
;; request-consult! — async push
;; ---------------------------------------------------------------------------

(defdescribe request-consult-test
  (describe "happy path: map shape with :focus + :question"
    (let [env (mk-env)
          ret (consult/request-consult! env :research :balanced
                {:focus ["fact A" "fact B"]
                 :question "Verify both claims."})]
      (it "returns :vis/silent"
        (expect (= :vis/silent ret)))

      (it "intent enqueued under :engine/pending-consults"
        (let [pending (-> env :ctx-atom deref :engine/pending-consults)]
          (expect (= 1 (count pending)))
          (expect (= :research (-> pending first :consult-id)))
          (expect (= :balanced (-> pending first :preference)))
          (expect (= "Verify both claims." (-> pending first :question)))
          (expect (= ["fact A" "fact B"] (-> pending first :focus)))))

      (it ":ctx-snapshot pinned on the intent at request time"
        (let [intent (-> env :ctx-atom deref :engine/pending-consults first)]
          (expect (string? (:ctx-snapshot intent)))))

      (it "no warnings on valid input"
        (expect (empty? (warnings-of env))))))

  (describe ":focus omitted defaults to empty vec"
    (let [env (mk-env)
          _   (consult/request-consult! env :review :deep
                {:question "Summarize X."})]
      (it ":focus defaults to []"
        (let [intent (-> env :ctx-atom deref :engine/pending-consults first)]
          (expect (= [] (:focus intent)))))))

  (describe "bare string question REFUSED"
    (let [env (mk-env)]
      (consult/request-consult! env :K :deep "bare-string-not-allowed")
      (it ":consult-invalid-request warning"
        (expect (some #(= :consult-invalid-request (:code %))
                  (warnings-of env))))
      (it "intent NOT enqueued"
        (expect (empty? (-> env :ctx-atom deref :engine/pending-consults))))))

  (describe "validation routes via :engine/warnings"
    (it "invalid :consult-id → :consult-invalid-id"
      (let [env (mk-env)]
        (consult/request-consult! env "not-a-kw" :fast {:question "q"})
        (expect (some #(= :consult-invalid-id (:code %)) (warnings-of env)))
        (expect (empty? (-> env :ctx-atom deref :engine/pending-consults)))))

    (it "unknown preference → :consult-unknown-preference"
      (let [env (mk-env)]
        (consult/request-consult! env :K :nuclear {:question "q"})
        (expect (some #(= :consult-unknown-preference (:code %))
                  (warnings-of env)))
        (expect (empty? (-> env :ctx-atom deref :engine/pending-consults)))))

    (it "blank :question → :consult-invalid-request"
      (let [env (mk-env)]
        (consult/request-consult! env :K :fast {:question "  "})
        (expect (some #(= :consult-invalid-request (:code %))
                  (warnings-of env)))))

    (it "missing :question → :consult-invalid-request"
      (let [env (mk-env)]
        (consult/request-consult! env :K :fast {:focus ["x"]})
        (expect (some #(= :consult-invalid-request (:code %))
                  (warnings-of env)))))

    (it ":focus must be a vec of strings"
      (let [env (mk-env)]
        (consult/request-consult! env :K :fast
          {:focus [:keyword :not-string] :question "q"})
        (expect (some #(= :consult-invalid-request (:code %))
                  (warnings-of env))))))

  (describe "budget cap"
    (let [env (mk-env {:consult-budget-atom (atom {:used 20 :cap 20})})]
      (consult/request-consult! env :K :fast {:question "q"})
      (it ":consult-budget-exhausted warning"
        (expect (some #(= :consult-budget-exhausted (:code %))
                  (warnings-of env))))
      (it "intent NOT enqueued"
        (expect (empty? (-> env :ctx-atom deref :engine/pending-consults)))))))

;; ---------------------------------------------------------------------------
;; Trailer pin synthesis + lookup
;; ---------------------------------------------------------------------------

(defdescribe trailer-pin-test
  (describe "append-resolution-pin! appends a synthetic pin to the current iter"
    (let [env (mk-env)
          _ (seed-pin! env {:consult-id :K :status :active :content "x"})
          ctx @(:ctx-atom env)
          iter-pin (first (:session/trailer ctx))
          form (first (:forms iter-pin))]
      (it "iter entry created under tN/iM"
        (expect (= "t1/i1" (:scope iter-pin))))
      (it ":tag :consult, :consult-id stamped"
        (expect (= :consult (:tag form)))
        (expect (= :K (:consult-id form))))
      (it ":result carries the entry map"
        (expect (= "x" (-> form :result :content))))))

  (describe "find-trailer-consult-pin returns the pin by id"
    (let [env (mk-env)
          _ (seed-pin! env {:consult-id :A :status :active :content "a"})
          _ (seed-pin! env {:consult-id :B :status :active :content "b"})
          ctx @(:ctx-atom env)]
      (it "finds A"
        (expect (= :A (:consult-id (consult/find-trailer-consult-pin ctx :A)))))
      (it "finds B"
        (expect (= :B (:consult-id (consult/find-trailer-consult-pin ctx :B)))))
      (it "nil for unknown id"
        (expect (nil? (consult/find-trailer-consult-pin ctx :unknown))))))

  (describe "re-issuing a consult REPLACES the pin (no duplicate)"
    (let [env (mk-env)
          _ (seed-pin! env {:consult-id :K :status :active :content "first"})
          _ (seed-pin! env {:consult-id :K :status :active :content "second"})
          ctx @(:ctx-atom env)
          forms (-> ctx :session/trailer first :forms)
          consult-forms (filter #(= :K (:consult-id %)) forms)]
      (it "only one pin for :K"
        (expect (= 1 (count consult-forms))))
      (it "pin carries the latest :result"
        (expect (= "second" (-> (first consult-forms) :result :content)))))))

;; ---------------------------------------------------------------------------
;; consult-promote!
;; ---------------------------------------------------------------------------

(defdescribe promote-test
  (describe "promote copies pin → fact, scrubs trailer"
    (let [env (mk-env)
          _ (seed-pin! env {:consult-id :review :status :active
                            :content "synthesis"
                            :citations [{:type :paper :url "u" :title "t"}]
                            :focus ["x"] :confidence :high
                            :preference :deep :born "t1"})
          ret (consult/promote-consult! env :review :review-fact)]

      (it "returns :vis/silent"
        (expect (= :vis/silent ret)))

      (it "fact created under :session/facts"
        (let [fact (-> env :ctx-atom deref :session/facts :review-fact)]
          (expect (= "synthesis" (:content fact)))
          (expect (= :active     (:status fact)))
          (expect (= :consult    (:source fact)))
          (expect (= ["x"]       (:focus fact)))
          (expect (= :high       (:confidence fact)))))

      (it "trailer pin scrubbed"
        (expect (nil? (consult/find-trailer-consult-pin
                        @(:ctx-atom env) :review))))))

  (describe "promote on unknown id warns"
    (let [env (mk-env)
          _ (consult/promote-consult! env :never :K-fact)]
      (it ":consult-unknown-id warning"
        (expect (some #(= :consult-unknown-id (:code %)) (warnings-of env))))))

  (describe "promote on :failed pin refuses + warns"
    (let [env (mk-env)
          _ (seed-pin! env {:consult-id :K :status :failed
                            :error :timeout :reason "30s"})
          _ (consult/promote-consult! env :K :K-fact)]
      (it ":consult-promote-failed warning"
        (expect (some #(= :consult-promote-failed (:code %)) (warnings-of env))))
      (it "no fact created"
        (expect (nil? (-> env :ctx-atom deref :session/facts :K-fact))))))

  (describe "cross-iter scrub: promote in iter N+1 removes pin landed in iter N"
    (let [env (mk-env)
          _ (seed-pin! env {:consult-id :K :status :active :content "x"})
          ;; simulate iter bump
          _ (swap! (:ctx-atom env) assoc :session/scope
              {:turn 1 :iter 2 :next-form 1})
          _ (consult/promote-consult!
              (assoc env :current-form-scope "t1/i2/f1")
              :K :K-fact)
          ctx-final @(:ctx-atom env)]
      (it ":session/trailer empty (the only iter entry's only form was scrubbed)"
        (expect (empty? (:session/trailer ctx-final))))
      (it "fact materialised"
        (expect (some? (-> ctx-final :session/facts :K-fact)))))))

;; ---------------------------------------------------------------------------
;; consult-dismiss!
;; ---------------------------------------------------------------------------

(defdescribe dismiss-test
  (describe "dismiss scrubs trailer pin without promoting"
    (let [env (mk-env)
          _ (seed-pin! env {:consult-id :research :status :active :content "x"})
          ret (consult/dismiss-consult! env :research)]
      (it "returns :vis/silent"
        (expect (= :vis/silent ret)))
      (it "trailer pin scrubbed"
        (expect (nil? (consult/find-trailer-consult-pin
                        @(:ctx-atom env) :research))))
      (it "no fact created"
        (expect (empty? (-> env :ctx-atom deref :session/facts))))))

  (describe "dismiss on unknown id warns"
    (let [env (mk-env)
          _ (consult/dismiss-consult! env :never)]
      (it ":consult-unknown-id warning"
        (expect (some #(= :consult-unknown-id (:code %)) (warnings-of env)))))))

;; ---------------------------------------------------------------------------
;; Pending-consult helpers (done-gate detection)
;; ---------------------------------------------------------------------------

(defdescribe pending-consult-ids-test
  (describe "pending-consult-ids reflects :engine/pending-consults"
    (let [env (mk-env)]
      (consult/request-consult! env :K1 :fast {:question "q1"})
      (consult/request-consult! env :K2 :deep {:question "q2"})
      (it "returns the vec of declared ids"
        (expect (= [:K1 :K2]
                  (consult/pending-consult-ids @(:ctx-atom env)))))))

  (describe "pending-consult-ids on empty is empty"
    (it "returns empty vec"
      (expect (empty? (consult/pending-consult-ids {}))))))

;; ---------------------------------------------------------------------------
;; clear-pending! drains :engine/pending-consults
;; ---------------------------------------------------------------------------

(defdescribe clear-pending-test
  (describe "clear-pending! returns drained intents and resets the slot"
    (let [env (mk-env)
          _ (consult/request-consult! env :K1 :fast {:question "q"})
          _ (consult/request-consult! env :K2 :deep {:question "q2"})
          drained (consult/clear-pending! (:ctx-atom env))]
      (it "drained vec has both intents"
        (expect (= [:K1 :K2] (mapv :consult-id drained))))
      (it ":engine/pending-consults is empty after drain"
        (expect (empty? (-> env :ctx-atom deref :engine/pending-consults)))))))
