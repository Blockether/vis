(ns com.blockether.vis.ext.foundation-core.hints-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.core :as vis]
   [com.blockether.vis.ext.foundation-core.hints :as hints]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe title-hint-test
  (it "emits a :critical hook-task when the title is blank AND we're on a cadence tick (iter 0 of turn 1 or every Nth turn)"
    (doseq [tp [1 hints/TITLE_REFRESH_TURN_PERIOD
                (* 2 hints/TITLE_REFRESH_TURN_PERIOD)
                (* 5 hints/TITLE_REFRESH_TURN_PERIOD)]]
      (let [n (hints/title-hint {:session-title nil
                                 :title-refresh? true
                                 :turn-position tp
                                 :iteration 0})]
        (expect (= :critical (:importance n)))
        (expect (vis/validator-fn? (:validator-fn n)))
        (expect (str/includes? (:title n) "session title"))
        (expect (str/includes? (:title n) "set-session-title!"))
        (expect (str/includes? (:title n) "task-set! :vis.foundation/session-title")))))

  (it "stays silent on blank title at non-cadence turns even when refresh is flagged"
    (doseq [tp [2 3 4 5 6 7 8 9 11 19 21 99]]
      (expect (nil? (hints/title-hint {:session-title nil
                                       :title-refresh? true
                                       :turn-position tp
                                       :iteration 0})))))

  (it "stays silent on blank title when host did NOT flag :title-refresh? (mid-turn iterations)"
    (doseq [tp [1 10 20]]
      (expect (nil? (hints/title-hint {:session-title nil
                                       :title-refresh? false
                                       :turn-position tp
                                       :iteration 3})))))

  (it "fires on turn 1 (first turn) when host flags :title-refresh?"
    (let [n (hints/title-hint {:session-title "Refactor auth flow"
                               :title-refresh? true
                               :turn-position 1
                               :iteration 1})]
      (expect (= :info (:importance n)))
      (expect (vis/validator-fn? (:validator-fn n)))
      (expect (str/includes? (:title n) "Refactor auth flow"))
      (expect (str/includes? (:title n) "set-session-title!"))
      (expect (str/includes? (:title n) "1 turn(s)"))))

  (it "fires on every TITLE_REFRESH_TURN_PERIOD-th turn when host flags :title-refresh?"
    (doseq [tp [hints/TITLE_REFRESH_TURN_PERIOD
                (* 2 hints/TITLE_REFRESH_TURN_PERIOD)
                (* 5 hints/TITLE_REFRESH_TURN_PERIOD)]]
      (let [n (hints/title-hint {:session-title "Triage 148 path failures"
                                 :title-refresh? true
                                 :turn-position tp
                                 :iteration 1})]
        (expect (some? n))
        (expect (str/includes? (:title n) (str tp " turn(s)")))
        (expect (str/includes? (:title n) "Triage 148 path failures")))))

  (it "stays silent on non-cadence turns even when refresh is flagged"
    (doseq [tp [2 3 4 5 6 7 8 9 11 19 21 99]]
      (expect (nil? (hints/title-hint {:session-title "Stable"
                                       :title-refresh? true
                                       :turn-position tp
                                       :iteration 1})))))

  (it "stays silent on cadence turn when host did not flag :title-refresh? (set OR blank)"
    (doseq [tp [1 hints/TITLE_REFRESH_TURN_PERIOD
                (* 2 hints/TITLE_REFRESH_TURN_PERIOD)]
            title ["Stable" nil ""]]
      (expect (nil? (hints/title-hint {:session-title title
                                       :title-refresh? false
                                       :turn-position tp
                                       :iteration 1})))))

  (it "never fires on iteration cadence (the old mod-N-iterations rule is gone)"
    (doseq [it [3 6 9 12 24 36 100]]
      (expect (nil? (hints/title-hint {:session-title "Stable"
                                       :title-refresh? false
                                       :turn-position 5
                                       :iteration it}))))))

(defdescribe title-validator-fn-test
  ;; The validator is purely structural: it pattern-matches on the
  ;; engine-supplied `:form` (parsed sexp) and ignores `:src`. A proof
  ;; scope whose source didn't parse leaves `:form` nil and fails closed
  ;; — string-includes? was a false-positive magnet. We exercise the
  ;; HOST fn (`:fn` slot, the runtime fast path); a separate test below
  ;; round-trips through `:src` to confirm the SCI fallback also works.
  (let [pred (:fn hints/TITLE_VALIDATOR)]
    (it "passes when :form is a real (set-session-title! \"…\") call"
      (expect (true? (boolean (pred {:form '(set-session-title! "Auth refactor")
                                     :result :vis/silent})))))

    (it "namespace-qualified head still passes (matches by name only)"
      (expect (true? (boolean (pred {:form '(vis/set-session-title! "X")
                                     :result :vis/silent})))))

    (it "fails when :form points at a different head"
      (expect (false? (boolean (pred {:form '(v/cat "README.md")
                                      :result :ok})))))

    (it "fails when :form is missing the title arg"
      (expect (false? (boolean (pred {:form '(set-session-title!)})))))

    (it "fails when title arg is a blank string"
      (expect (false? (boolean (pred {:form '(set-session-title! "")})))))

    (it "fails when title arg is non-string"
      (expect (false? (boolean (pred {:form '(set-session-title! 42)})))))

    (it "FALSE POSITIVE GUARD: comment carrying the symbol does NOT pass"
      ;; Regression: the legacy string-includes? validator passed on
      ;; `(v/cat ...)  ;; remember to set-session-title!` because the
      ;; literal substring was present. With `:form` matching, the
      ;; head is `v/cat` and the validator fails as expected.
      (expect (false? (boolean (pred {:form '(v/cat "README.md")
                                      :result :ok})))))

    (it "FALSE POSITIVE GUARD: string literal carrying the symbol does NOT pass"
      (expect (false? (boolean (pred {:form '(println "called set-session-title! before")})))))

    (it "fails when the proof form errored"
      (expect (false? (boolean (pred {:form '(set-session-title! "x")
                                      :error {:message "boom"}})))))

    (it "fails closed when :form is missing (parse failure)"
      ;; No string fallback. An unparseable proof scope is a broken
      ;; proof; we refuse to pretend a substring check substitutes
      ;; for an AST match.
      (expect (false? (boolean (pred {:src "(set-session-title! \"x\")"})))))

    (it "fails on empty envelope"
      (expect (false? (boolean (pred {:result :ok})))))))

(defdescribe context-pressure-hint-test
  (it "stays silent below the threshold"
    (expect (nil? (hints/context-pressure-hint
                    {:input-tokens 1000
                     :context-limit 200000}))))

  (it "fires at or above CONTEXT_PRESSURE_THRESHOLD with :warn importance + validator-fn"
    (let [limit 200000
          used  (long (* limit hints/CONTEXT_PRESSURE_THRESHOLD))
          n     (hints/context-pressure-hint {:input-tokens used
                                              :context-limit limit})]
      (expect (= :warn (:importance n)))
      (expect (vis/validator-fn? (:validator-fn n)))
      (expect (str/includes? (:title n) "Converge now"))))

  (it "fires for 100k/200k (the z.ai GLM sweet-spot boundary)"
    (let [n (hints/context-pressure-hint {:input-tokens 100000
                                          :context-limit 200000})]
      (expect (some? n))
      (expect (str/includes? (:title n) "100000"))
      (expect (str/includes? (:title n) "200000"))))

  (it "is nil-safe when token/limit info is missing or zero"
    (expect (nil? (hints/context-pressure-hint {})))
    (expect (nil? (hints/context-pressure-hint {:input-tokens 0 :context-limit 200000})))
    (expect (nil? (hints/context-pressure-hint {:input-tokens 50000 :context-limit 0})))))

(defdescribe title-validator-src-roundtrip-test
  ;; Persistence safety: the same SCI source string must compile and
  ;; behave identically to the host fn. This proves that a thawed
  ;; session (where `:fn` was lost to nippy + replaced by a runtime-ref
  ;; marker) still validates correctly via `:src`.
  (let [pred (eval (read-string (:src hints/TITLE_VALIDATOR)))]
    (it "src-compiled validator passes on a real set-session-title! call"
      (expect (true? (boolean (pred {:form '(set-session-title! "Auth refactor")})))))
    (it "src-compiled validator fails on wrong head"
      (expect (false? (boolean (pred {:form '(v/cat "x")})))))
    (it "src-compiled validator fails on blank title"
      (expect (false? (boolean (pred {:form '(set-session-title! "")})))))))

(defdescribe context-pressure-validator-fn-test
  (let [pred (:fn hints/CONTEXT_PRESSURE_VALIDATOR)]
    (it "passes on bare (done) form (turn converges either way)"
      (expect (true? (boolean (pred {:form '(done)
                                     :result :vis/answer})))))

    (it "passes on (done {:answer …})"
      (expect (true? (boolean (pred {:form '(done {:answer "x"})
                                     :result :vis/answer})))))

    (it "passes on (done … :trailer-drop …)"
      (expect (true? (boolean (pred {:form '(done {:answer "x" :trailer-drop ["t1/i1"]})
                                     :result :vis/answer})))))

    (it "passes on (done … :trailer-summarize …)"
      (expect (true? (boolean (pred {:form '(done {:answer "x" :trailer-summarize [{:scope-start "t1/i1" :scope-end "t1/i2" :summary "explored"}]})
                                     :result :vis/answer})))))

    (it "FALSE POSITIVE GUARD: comment carrying `:trailer-drop` does NOT pass"
      (expect (false? (boolean (pred {:form '(v/cat "README.md")
                                      :result :ok})))))

    (it "FALSE POSITIVE GUARD: string literal carrying `done` does NOT pass"
      (expect (false? (boolean (pred {:form '(println "will done shortly")})))))

    (it "fails on unrelated form"
      (expect (false? (boolean (pred {:form '(v/ls ".")
                                      :result :ok})))))

    (it "fails when proof form errored"
      (expect (false? (boolean (pred {:form '(done {:answer "x"})
                                      :error {:message "boom"}})))))

    (it "fails closed when :form is missing"
      (expect (false? (boolean (pred {:src "(done)"})))))))

(defdescribe hooks-registration-test
  (it "foundation ships only the two iteration-start soft hooks"
    (let [ids (set (map :id hints/hooks))]
      (expect (= #{:vis.foundation/session-title
                   :vis.foundation/context-pressure}
                ids))))

  (it "every hook declares the four required keys (:id :doc :phase :fn)"
    (doseq [h hints/hooks]
      (expect (keyword? (:id h)))
      (expect (string? (:doc h)))
      (expect (= :turn.iteration/start (:phase h)))
      (expect (fn? (:fn h)))))

  (it "title hook returns hook-task shape with :title + :validator-fn + :importance"
    (let [h (some #(when (= :vis.foundation/session-title (:id %)) %) hints/hooks)
          hit ((:fn h) {:session-title nil :title-refresh? true
                        :turn-position 1 :iteration 0})]
      (expect (string? (:title hit)))
      ;; `:validator-fn` is now the dual `{:fn :src}` map (vis/validator-fn
      ;; macro output) instead of the old SCI source string. The engine
      ;; accepts any of fn / map / string via `vis/validator-fn?`.
      (expect (vis/validator-fn? (:validator-fn hit)))
      (expect (fn? (:fn (:validator-fn hit))))
      (expect (string? (:src (:validator-fn hit))))
      (expect (= :critical (:importance hit)))))

  (it "hooks return nil when their underlying condition is absent"
    (let [title-h    (some #(when (= :vis.foundation/session-title (:id %)) %) hints/hooks)
          pressure-h (some #(when (= :vis.foundation/context-pressure (:id %)) %) hints/hooks)]
      (expect (nil? ((:fn title-h)    {:session-title "Set" :title-refresh? false
                                       :turn-position 5 :iteration 1})))
      (expect (nil? ((:fn pressure-h) {:input-tokens 100 :context-limit 200000}))))))
