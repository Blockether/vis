(ns com.blockether.vis.internal.ctx-engine-done-test
  "Tests for the `done` handler: trailer-drop, trailer-summarize, partial
   overlap detection, and the trailer comparator."
  (:require
   [com.blockether.vis.internal.ctx-engine :as eng]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- pin [scope src]
  {:scope scope :forms [{:scope (str scope "/f1") :tag :observation :src src}]})

(defn- summary [a b text]
  {:scope-start a :scope-end b :summary text :born "t9/i1/f1"})

;; =============================================================================
;; iter-compare + parse-scope-iter
;; =============================================================================

(defdescribe iter-compare-test
  (describe "parse-scope-iter"
    (it "parses turn + iter"
      (expect (= {:turn 3 :iter 2} (eng/parse-scope-iter "t3/i2"))))
    (it "nil for form-scope or garbage"
      (expect (nil? (eng/parse-scope-iter "t3/i2/f1")))
      (expect (nil? (eng/parse-scope-iter "garbage")))))

  (describe "iter-in-range?"
    (it "inclusive on both ends"
      (expect (eng/iter-in-range? "t3/i2" "t3/i2" "t3/i5"))
      (expect (eng/iter-in-range? "t3/i5" "t3/i2" "t3/i5"))
      (expect (eng/iter-in-range? "t3/i3" "t3/i2" "t3/i5")))
    (it "rejects out-of-range"
      (expect (not (eng/iter-in-range? "t3/i1" "t3/i2" "t3/i5")))
      (expect (not (eng/iter-in-range? "t4/i1" "t3/i2" "t3/i5"))))))

;; =============================================================================
;; trailer-drop
;; =============================================================================

(defdescribe trailer-drop-test
  (describe "apply-trailer-drop"
    (it "removes pin entries by exact scope match"
      (let [tr [(pin "t1/i1" "a") (pin "t2/i1" "b") (pin "t2/i2" "c")]
            after (eng/apply-trailer-drop tr ["t2/i1"])]
        (expect (= 2 (count after)))
        (expect (every? #(not= "t2/i1" (:scope %)) after))))

    (it "removes summary entries by scope-start->scope-end key"
      (let [tr [(pin "t1/i1" "a") (summary "t2/i1" "t3/i2" "x")]
            after (eng/apply-trailer-drop tr ["t2/i1->t3/i2"])]
        (expect (= 1 (count after)))
        (expect (= "t1/i1" (:scope (first after))))))

    (it "is silent on non-existent scope key"
      (let [tr [(pin "t1/i1" "a")]]
        (expect (= tr (eng/apply-trailer-drop tr ["t99/i99"])))))))

;; =============================================================================
;; trailer-summarize: contained absorb + partial overlap reject
;; =============================================================================

(defdescribe trailer-summarize-test
  (describe "apply-trailer-summarize"
    (it "absorbs fully-contained pins into a new summary"
      (let [tr [(pin "t2/i1" "a") (pin "t2/i2" "b") (pin "t2/i3" "c")
                (pin "t3/i1" "d")]
            {:keys [trailer warnings]}
            (eng/apply-trailer-summarize tr [{:scope-start "t2/i1" :scope-end "t2/i3"
                                              :summary "explored"}] "t4/i1/f1")]
        (expect (empty? warnings))
        (expect (= 2 (count trailer)))
        (expect (some #(and (= "t2/i1" (:scope-start %)) (= "t2/i3" (:scope-end %)))
                  trailer))
        (expect (some #(= "t3/i1" (:scope %)) trailer))))

    (it "stamps :born on the new summary"
      (let [tr [(pin "t2/i1" "a")]
            {:keys [trailer]}
            (eng/apply-trailer-summarize tr [{:scope-start "t2/i1" :scope-end "t2/i1"
                                              :summary "x"}] "t5/i9/f7")
            s (first (filter :scope-start trailer))]
        (expect (= "t5/i9/f7" (:born s)))))

    (it "HARD rejects partial overlap with existing summary"
      (let [tr [(summary "t2/i1" "t2/i5" "first")]
            {:keys [trailer warnings]}
            (eng/apply-trailer-summarize tr [{:scope-start "t2/i3" :scope-end "t3/i1"
                                              :summary "second"}] "t9/i1/f1")]
        (expect (= 1 (count trailer)))
        (expect (= "first" (:summary (first trailer))))
        (expect (some #(= :trailer-summarize-partial-overlap (:code %)) warnings))))

    (it "allows new summary CONTAINED inside existing summary (absorbed)"
      (let [tr [(summary "t2/i1" "t3/i9" "outer")]
            {:keys [trailer warnings]}
            (eng/apply-trailer-summarize tr [{:scope-start "t2/i5" :scope-end "t2/i8"
                                              :summary "inner"}] "t9/i1/f1")]
        ;; partial-overlap = NOT contained either way; inner is contained
        ;; in outer → outer absorbs inner is not how summarize works,
        ;; but no partial-overlap, so write proceeds: outer is absorbed
        ;; (because outer is contained-by inner? no — outer is bigger).
        ;; Actually inner is contained in outer; summarize will absorb outer
        ;; because outer is iter-ranges-partial-overlap? false (b contains
        ;; a → contained). Then absorb is by entry-contained-by, which is
        ;; outer-contained-in-inner? false. So outer stays, inner appended.
        (expect (empty? warnings))
        (expect (= 2 (count trailer)))))

    (it "rejects inverted range with soft warn"
      (let [tr [(pin "t1/i1" "a")]
            {:keys [trailer warnings]}
            (eng/apply-trailer-summarize tr [{:scope-start "t3/i1" :scope-end "t2/i1"
                                              :summary "oops"}] "t9/i1/f1")]
        (expect (= 1 (count trailer)))
        (expect (some #(= :trailer-summarize-inverted (:code %)) warnings))))))

;; =============================================================================
;; apply-done end-to-end
;; =============================================================================

(defdescribe apply-done-test
  (describe "apply-done"
    (let [ctx (-> (eng/empty-ctx)
                (assoc :session/trailer
                  [(pin "t1/i1" "v/cat")
                   (pin "t2/i1" "v/patch")
                   (pin "t2/i2" "v/cat")
                   (pin "t2/i3" "tests")
                   (pin "t3/i1" "more")]))]

      (it "applies :trailer-drop"
        (let [{ctx' :ctx} (eng/apply-done ctx "t4/i1/f1"
                            {:trailer-drop ["t1/i1"]})]
          (expect (= 4 (count (:session/trailer ctx'))))))

      (it "applies :trailer-summarize"
        (let [{ctx' :ctx ws :warnings}
              (eng/apply-done ctx "t4/i1/f1"
                {:trailer-summarize
                 [{:scope-start "t2/i1" :scope-end "t2/i3"
                   :summary "patch+test cycle"}]})]
          (expect (empty? ws))
          (expect (= 3 (count (:session/trailer ctx'))))
          (expect (some :scope-start (:session/trailer ctx')))))

      (it "warns on partial-overlap scope between drop and summarize"
        (let [ctx-with (-> ctx
                         (assoc :session/trailer
                           [(summary "t2/i1" "t2/i5" "first")
                            (pin "t3/i1" "x")]))
              {ws :warnings}
              (eng/apply-done ctx-with "t9/i1/f1"
                {:trailer-summarize
                 [{:scope-start "t2/i3" :scope-end "t3/i1"
                   :summary "second"}]})]
          (expect (some #(= :trailer-summarize-partial-overlap (:code %)) ws))))

      (it "sorts trailer by (scope-start, kind)"
        (let [ctx-mixed (-> (eng/empty-ctx)
                          (assoc :session/trailer
                            [(pin "t3/i1" "z")
                             (summary "t1/i1" "t1/i9" "x")
                             (pin "t2/i1" "y")]))
              {ctx' :ctx} (eng/apply-done ctx-mixed "t4/i1/f1" {})
              order (map (fn [e] (or (:scope e) (:scope-start e)))
                      (:session/trailer ctx'))]
          (expect (= ["t1/i1" "t2/i1" "t3/i1"] order)))))))

;; =============================================================================
;; validator-fn compile + run
;; =============================================================================

(defdescribe validator-fn-compile-test
  (describe "compile-validator-fn"
    (it "compiles a valid fn source"
      (let [r (eng/compile-validator-fn "(fn [{:keys [result]}] (true? (:ok? result)))")]
        (expect (fn? (:fn r)))))

    (it "returns :error on non-fn expression"
      (let [r (eng/compile-validator-fn "42")]
        (expect (some? (:error r)))))

    (it "returns :error on syntax error"
      (let [r (eng/compile-validator-fn "(fn [")]
        (expect (some? (:error r)))))))

(defdescribe validator-fn-run-test
  (describe "run-validator-fn"
    (it ":ok when predicate returns truthy"
      (let [r (eng/run-validator-fn
                "(fn [{:keys [result]}] (true? (:ok? result)))"
                {:result {:ok? true}})]
        (expect (:ok? r))
        (expect (= :ok (:reason r)))))

    (it ":falsy when predicate returns false"
      (let [r (eng/run-validator-fn
                "(fn [{:keys [result]}] (true? (:ok? result)))"
                {:result {:ok? false}})]
        (expect (not (:ok? r)))
        (expect (= :falsy (:reason r)))))

    (it ":threw when predicate throws"
      (let [r (eng/run-validator-fn
                "(fn [_] (throw (ex-info \"boom\" {})))"
                {:result 1})]
        (expect (not (:ok? r)))
        (expect (= :threw (:reason r)))))

    (it ":timed-out when predicate hangs past 50ms"
      (let [r (eng/run-validator-fn
                "(fn [_] (loop [] (recur)))"
                {:result 1}
                50)]
        (expect (not (:ok? r)))
        (expect (= :timed-out (:reason r)))))

    (it ":compile-error when source does not parse"
      (let [r (eng/run-validator-fn "(fn [" {:result 1})]
        (expect (not (:ok? r)))
        (expect (= :compile-error (:reason r)))))))

;; =============================================================================
;; History introspection
;; =============================================================================

(defdescribe introspect-history-test
  (describe "introspect-spec / -task / -fact"
    (let [snap-1 (-> (eng/empty-ctx "h")
                   (assoc-in [:session/specs :s1]
                     {:title "first" :requirements [{:id :r :title "x"}]
                      :status :draft :born "t1/i1/f1"}))
          snap-2 (-> snap-1
                   (assoc-in [:session/specs :s1 :status] :doing)
                   (assoc-in [:session/specs :s2]
                     {:title "second" :requirements [{:id :r2 :title "y"}]
                      :status :draft :born "t2/i1/f1"}))
          snap-3 (-> snap-2
                   ;; archive s1
                   (update :session/specs dissoc :s1))
          history {1 snap-1 2 snap-2 3 snap-3}]

      (it "introspect-spec returns latest snapshot the entry existed in"
        (let [r (eng/introspect-spec history :s1)]
          (expect (= "first" (:title r)))
          (expect (= :doing (:status r)))             ; updated in turn 2
          (expect (= 2 (:as-of-turn r)))))            ; archived in turn 3

      (it "introspect-spec returns nil for unknown key"
        (expect (nil? (eng/introspect-spec history :never-existed))))

      (it "introspect-ctx-at returns the full snapshot for the turn"
        (expect (= snap-2 (eng/introspect-ctx-at history 2)))
        (expect (nil? (eng/introspect-ctx-at history 99))))

      (it "introspect-archived enumerates entries missing from latest"
        (let [arch (eng/introspect-archived history :specs)
              keys (set (map :key arch))]
          (expect (contains? keys :s1))
          (expect (not (contains? keys :s2))))))))

;; =============================================================================
;; derive-warnings T2 pass — validator-fn integration
;; =============================================================================

(defdescribe derive-warnings-validator-pass-test
  (describe "pass-validators integration in derive-warnings"
    (let [ctx (-> (eng/empty-ctx "test")
                (assoc :session/turn 2)
                (assoc :session/scope {:turn 2 :iter 2 :next-form 1})
                (assoc-in [:session/specs :rl]
                  {:title "race-free"
                   :requirements [{:id :ok-r
                                   :title "always true"
                                   :validator-fn "(fn [{:keys [result]}] (true? (:ok? result)))"}
                                  {:id :fail-r
                                   :title "asserts result==42"
                                   :validator-fn "(fn [{:keys [result]}] (= 42 result))"}]
                   :status :doing
                   :born "t1/i1/f1"})
                (assoc-in [:session/tasks :prove]
                  {:title "prove"
                   :specs {:rl [{:requirement :ok-r   :proof "t2/i1/f1"}
                                {:requirement :fail-r :proof "t2/i1/f2"}]}
                   :status :doing
                   :born "t1/i1/f2"}))
          ;; form-results: f1 satisfies ok-r validator; f2 returns 99 which
          ;; fails the (= 42 result) check
          form-results {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation
                                    :src "(check)" :result {:ok? true}}
                        "t2/i1/f2" {:scope "t2/i1/f2" :tag :observation
                                    :src "(compute)" :result 99}}
          idx (eng/build-indexes ctx)
          warnings (eng/derive-warnings ctx idx form-results)]

      (it "emits :proof-validator-fail for the failing validator only"
        (let [validator-warns (filter #(= :proof-validator-fail (:code %)) warnings)]
          (expect (= 1 (count validator-warns)))
          (expect (= [:prove :rl :fail-r :falsy]
                    (:anchor (first validator-warns))))))

      (it "passing validator (ok-r) emits NO warning"
        (expect (not (some #(and (= :proof-validator-fail (:code %))
                              (= :ok-r (nth (:anchor %) 2)))
                       warnings))))

      (it "skips validator pass when form-results is nil"
        (let [warns-nil (eng/derive-warnings ctx idx nil)]
          (expect (not (some #(= :proof-validator-fail (:code %)) warns-nil))))))))

(defdescribe validator-pass-edge-cases-test
  (describe "validator pass edge cases"
    (let [ctx-base (-> (eng/empty-ctx "test")
                     (assoc :session/turn 1)
                     (assoc :session/scope {:turn 1 :iter 2 :next-form 1})
                     (assoc-in [:session/tasks :t]
                       {:title "x" :specs {:s [{:requirement :r :proof "t1/i1/f1"}]}
                        :status :doing :born "t1/i1/f1"}))]

      (it "compile-error in validator-fn surfaces as :proof-validator-fail with :compile-error reason"
        (let [ctx (assoc-in ctx-base [:session/specs :s]
                    {:title "x"
                     :requirements [{:id :r :title "x"
                                     :validator-fn "(fn ["}] ;; broken
                     :status :doing :born "t1/i1/f1"})
              form-results {"t1/i1/f1" {:scope "t1/i1/f1" :tag :observation
                                        :src "x" :result 1}}
              idx (eng/build-indexes ctx)
              warns (eng/derive-warnings ctx idx form-results)
              vw (first (filter #(= :proof-validator-fail (:code %)) warns))]
          (expect (= :compile-error (nth (:anchor vw) 3)))))

      (it "no warning when requirement has no :validator-fn"
        (let [ctx (assoc-in ctx-base [:session/specs :s]
                    {:title "x"
                     :requirements [{:id :r :title "no validator"}]
                     :status :doing :born "t1/i1/f1"})
              form-results {"t1/i1/f1" {:scope "t1/i1/f1" :tag :observation
                                        :src "x" :result 1}}
              idx (eng/build-indexes ctx)
              warns (eng/derive-warnings ctx idx form-results)]
          (expect (not (some #(= :proof-validator-fail (:code %)) warns))))))))

;; =============================================================================
;; apply-satisfies — hint validator-fn pass (D11)
;; =============================================================================

(def ^:private TITLE_VALIDATOR
  "(fn [{:keys [src error]}]
     (and (string? src)
       (clojure.string/includes? src \"set-session-title!\")
       (nil? error)))")

(defn- with-title-hint [ctx]
  (assoc-in ctx [:session/hints :vis.foundation/session-title]
    {:body "set session title"
     :importance :critical
     :validator-fn TITLE_VALIDATOR}))

(defn- mk-hint-ctx []
  (-> (eng/empty-ctx "test")
    (assoc :session/turn 1)
    (assoc :session/scope {:turn 1 :iter 2 :next-form 1})
    with-title-hint))

(defdescribe apply-satisfies-passing-test
  (describe "apply-satisfies — proof scope passes validator"
    (let [ctx (mk-hint-ctx)
          fr  {"t1/i1/f3" {:scope "t1/i1/f3" :tag :mutation
                           :src "(set-session-title! \"Auth refactor\")"
                           :result :vis/silent}}
          pending [{:id :vis.foundation/session-title :scopes ["t1/i1/f3"]}]
          {ctx' :ctx warns :warnings} (eng/apply-satisfies ctx pending fr)]

      (it "drops the hint id from :session/hints"
        (expect (nil? (get-in ctx' [:session/hints :vis.foundation/session-title]))))

      (it "emits no warnings"
        (expect (empty? warns))))))

(defdescribe apply-satisfies-failing-test
  (describe "apply-satisfies — proof scope fails validator (wrong src)"
    (let [ctx (mk-hint-ctx)
          fr  {"t1/i1/f3" {:scope "t1/i1/f3" :tag :observation
                           :src "(v/ls \".\")" :result []}}
          pending [{:id :vis.foundation/session-title :scopes ["t1/i1/f3"]}]
          {ctx' :ctx warns :warnings} (eng/apply-satisfies ctx pending fr)]

      (it "keeps the hint id (model has not actually called the action)"
        (expect (some? (get-in ctx' [:session/hints :vis.foundation/session-title]))))

      (it "emits :hint-validator-fail warning"
        (expect (some #(= :hint-validator-fail (:code %)) warns))))))

(defdescribe apply-satisfies-no-proof-test
  (describe "apply-satisfies — 0-arity satisfy when validator-fn present"
    (let [ctx (mk-hint-ctx)
          pending [{:id :vis.foundation/session-title :scopes []}]
          {ctx' :ctx warns :warnings} (eng/apply-satisfies ctx pending {})]

      (it "keeps the hint id"
        (expect (some? (get-in ctx' [:session/hints :vis.foundation/session-title]))))

      (it "emits :hint-no-proof warning"
        (expect (some #(= :hint-no-proof (:code %)) warns))))))

(defdescribe apply-satisfies-malformed-scope-test
  (describe "apply-satisfies — malformed proof scope distinguished from unknown"
    (let [ctx (mk-hint-ctx)
          pending [{:id :vis.foundation/session-title :scopes ["not-a-scope"]}]
          {ctx' :ctx warns :warnings} (eng/apply-satisfies ctx pending {})]

      (it "keeps the hint id"
        (expect (some? (get-in ctx' [:session/hints :vis.foundation/session-title]))))

      (it "emits :hint-proof-malformed (NOT :hint-proof-unknown)"
        (expect (some #(= :hint-proof-malformed (:code %)) warns))
        (expect (not (some #(= :hint-proof-unknown (:code %)) warns)))))))

(defdescribe apply-satisfies-future-scope-test
  (describe "apply-satisfies — proof scope pointing to a future iter"
    (let [ctx (mk-hint-ctx) ;; cursor t1/i2
          ;; t1/i5/f1 is :future-iter relative to cursor at t1/i2.
          pending [{:id :vis.foundation/session-title :scopes ["t1/i5/f1"]}]
          {ctx' :ctx warns :warnings} (eng/apply-satisfies ctx pending {})]

      (it "keeps the hint id"
        (expect (some? (get-in ctx' [:session/hints :vis.foundation/session-title]))))

      (it "emits :hint-proof-future warning"
        (expect (some #(= :hint-proof-future (:code %)) warns))))))

(defdescribe apply-satisfies-unknown-hint-test
  (describe "apply-satisfies — satisfy for an id no longer in :session/hints"
    (let [ctx (mk-hint-ctx) ;; no :unknown/hint present
          pending [{:id :unknown/hint :scopes ["t1/i1/f1"]}]
          {ctx' :ctx warns :warnings} (eng/apply-satisfies ctx pending {})]

      (it "idempotent no-op: live hint untouched"
        (expect (some? (get-in ctx' [:session/hints :vis.foundation/session-title]))))

      (it "no warnings (model satisfied a stale hint that was already gone)"
        (expect (empty? warns))))))

(defdescribe apply-satisfies-multi-scope-test
  (describe "apply-satisfies — all-or-nothing across multiple proof scopes"
    (let [ctx (mk-hint-ctx)
          fr  {"t1/i1/f1" {:src "(set-session-title! \"A\")" :result :vis/silent}
               "t1/i1/f2" {:src "(v/ls)" :result []}}
          pending [{:id :vis.foundation/session-title :scopes ["t1/i1/f1" "t1/i1/f2"]}]
          {ctx' :ctx warns :warnings} (eng/apply-satisfies ctx pending fr)]

      (it "ANY scope failing keeps hint alive"
        (expect (some? (get-in ctx' [:session/hints :vis.foundation/session-title]))))

      (it "warning anchors the FAILING scope, not the passing one"
        (let [w (first (filter #(= :hint-validator-fail (:code %)) warns))]
          (expect (= "t1/i1/f2" (nth (:anchor w) 1))))))))

(defdescribe apply-satisfies-monotonic-drop-test
  (describe "apply-satisfies — multiple requests for same id, ANY pass drops it"
    (let [ctx (mk-hint-ctx)
          fr  {"t1/i1/f1" {:src "(v/ls)" :result []}
               "t1/i1/f2" {:src "(set-session-title! \"x\")" :result :vis/silent}}
          ;; first request fails; second passes; hint should drop
          pending [{:id :vis.foundation/session-title :scopes ["t1/i1/f1"]}
                   {:id :vis.foundation/session-title :scopes ["t1/i1/f2"]}]
          {ctx' :ctx warns :warnings} (eng/apply-satisfies ctx pending fr)]

      (it "drops the hint id (second request succeeded)"
        (expect (nil? (get-in ctx' [:session/hints :vis.foundation/session-title]))))

      (it "suppresses warnings from the failed attempt once the hint is satisfied"
        (expect (empty? (filter #(= :hint-validator-fail (:code %)) warns)))))))
