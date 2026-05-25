(ns com.blockether.vis.internal.consult-test
  "Unit tests for Phase H consult layer. Router calls are mocked via
   with-redefs; we test budget, recursion cap, preference resolution,
   and the bounded-failure return-map contract."
  (:require
   [com.blockether.svar.core :as svar]
   [com.blockether.vis.internal.consult :as consult]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- mk-env
  ([] (mk-env {}))
  ([overrides]
   (merge {:router ::router
           :ctx-atom (atom {:session/id "test" :session/turn 1
                            :session/scope {:turn 1 :iter 1 :next-form 1}})
           :consult-budget-atom (consult/fresh-budget-atom)
           :consult-config {:fast     {:provider :anthropic :model "haiku"}
                            :balanced {:provider :anthropic :model "sonnet"}
                            :deep     {:provider :anthropic :model "opus"}}
           :turn/system-prompt "PRIMARY-SYS"
           :turn/user-request  "user question text"}
     overrides)))

(defdescribe consult-happy-path-test
  (describe "consult! returns the consultant's response string on success"
    (let [seen (atom nil)
          env  (mk-env)]
      (with-redefs [svar/ask-code!
                    (fn [_router opts]
                      (reset! seen opts)
                      {:content "consultant says: do X" :raw "raw text"})]
        (let [out (consult/consult! env :fast "should I do X?")]
          (it "returns the consultant's content string"
            (expect (= "consultant says: do X" out)))

          (it "routes via the resolved provider/model for the preference"
            (expect (= {:provider :anthropic :model "haiku"}
                      (:routing @seen))))

          (it "embeds the model's question as the last user message"
            (expect (= "should I do X?"
                      (-> @seen :messages last :content))))

          (it "embeds the primary system prompt in the consultant system msg"
            (let [sys (-> @seen :messages first :content)]
              (expect (re-find #"SECONDARY CONSULTANT" sys))
              (expect (re-find #"PRIMARY-SYS" sys))))

          (it "embeds the primary user request"
            (let [user-msg (-> @seen :messages (nth 1) :content)]
              (expect (re-find #"user question text" user-msg))))

          (it "increments the session budget"
            (expect (= 1 (-> env :consult-budget-atom deref :used)))))))))

(defdescribe consult-budget-exhaustion-test
  (describe "consult! short-circuits when the session cap is exceeded"
    (let [env (mk-env {:consult-budget-atom (atom {:used 20 :cap 20})})]
      (with-redefs [svar/ask-code! (fn [& _] (throw (ex-info "should not be called" {})))]
        (let [out (consult/consult! env :fast "any question")]
          (it "returns an error map (does not call the router)"
            (expect (map? out))
            (expect (= :consult-budget-exhausted (:error out))))

          (it "does not increment past the cap"
            (expect (= 20 (-> env :consult-budget-atom deref :used)))))))))

(defdescribe consult-unknown-preference-test
  (describe "consult! rejects preferences outside #{:fast :balanced :deep}"
    (let [env (mk-env)
          out (consult/consult! env :nuclear "x")]
      (it "returns :consult-unknown-preference"
        (expect (= :consult-unknown-preference (:error out))))
      (it "does not bump the budget"
        (expect (= 0 (-> env :consult-budget-atom deref :used)))))))

(defdescribe consult-empty-question-test
  (describe "consult! rejects blank/nil questions"
    (let [env (mk-env)]
      (it "nil question → :consult-empty-question"
        (expect (= :consult-empty-question
                  (:error (consult/consult! env :fast nil)))))
      (it "blank question → :consult-empty-question"
        (expect (= :consult-empty-question
                  (:error (consult/consult! env :fast "   "))))))))

(defdescribe consult-recursion-cap-test
  (describe "nested consults beyond depth 2 short-circuit"
    (let [env (mk-env)]
      (binding [consult/*recursion-depth* 2]
        (let [out (consult/consult! env :fast "deep recursion attempt")]
          (it "returns :consult-recursion-cap"
            (expect (= :consult-recursion-cap (:error out)))))))))

(defdescribe consult-error-passthrough-test
  (describe "router errors surface as a bounded error map, not a throw"
    (let [env (mk-env)]
      (with-redefs [svar/ask-code! (fn [& _] (throw (ex-info "boom" {})))]
        (let [out (consult/consult! env :fast "x")]
          (it "returns :consult-error with the ex message"
            (expect (= :consult-error (:error out)))
            (expect (= "boom" (:reason out))))
          (it "still bumps the budget (call was attempted)"
            (expect (= 1 (-> env :consult-budget-atom deref :used)))))))))
