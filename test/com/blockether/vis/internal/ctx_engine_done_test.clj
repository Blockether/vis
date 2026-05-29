(ns com.blockether.vis.internal.ctx-engine-done-test
  "Tests for the `done` handler: trailer-drop, trailer-summarize, partial
   overlap detection, the trailer comparator, the pending-consult gate,
   history introspection, and hook-task idempotent re-emission."
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
        ;; inner is contained in outer → no partial-overlap, so write
        ;; proceeds; outer is not contained-by inner so it survives.
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
;; done gate: refuse close while consults are pending
;; =============================================================================

(defdescribe done-pending-consult-gate-test
  (describe "done is REFUSED when :engine/pending-consults is non-empty"
    (let [ctx (-> (eng/empty-ctx)
                (assoc :engine/pending-consults
                  [{:id :review :preference :deep
                    :focus ["x"] :question "q" :born "t1/i1/f1"}
                   {:id :critique :preference :fast
                    :focus [] :question "q2" :born "t1/i1/f2"}]))
          {:keys [warnings]
           :as result}
          (eng/apply-done ctx "t1/i1/f9" {:archive {:tasks [:something]}})]

      (it ":blocked? true on the returned map"
        (expect (true? (:blocked? result))))

      (it "ctx is returned UNCHANGED — no archive applied"
        (expect (= ctx (:ctx result))))

      (it "warning :done-blocked-by-pending-consults emitted"
        (expect (some #(= :done-blocked-by-pending-consults (:code %))
                  warnings)))

      (it "warning anchor carries every blocking consult-id"
        (let [w (first (filter #(= :done-blocked-by-pending-consults (:code %))
                         warnings))]
          (expect (= [:review :critique] (:anchor w)))))))

  (describe "done proceeds normally when no consults are pending"
    (let [ctx (eng/empty-ctx)
          result (eng/apply-done ctx "t1/i1/f9" {})]
      (it "no :blocked? flag"
        (expect (not (:blocked? result))))
      (it "ctx returned (apply-done-impl ran)"
        (expect (some? (:ctx result))))))

  (describe "done-pending-consult-blockers returns the vec of pending ids"
    (it "non-empty pending => vec of ids"
      (let [ctx {:engine/pending-consults
                 [{:id :K1} {:id :K2}]}]
        (expect (= [:K1 :K2] (eng/done-pending-consult-blockers ctx)))))
    (it "empty pending => empty vec"
      (expect (empty? (eng/done-pending-consult-blockers {}))))))

;; =============================================================================
;; History introspection (tasks + facts)
;; =============================================================================

(defdescribe introspect-history-test
  (describe "introspect-task / -fact / -ctx-at / -archived"
    (let [snap-1 (-> (eng/empty-ctx "h")
                   (assoc-in [:session/tasks :t1]
                     {:title "first" :status :todo :born "t1/i1/f1"}))
          snap-2 (-> snap-1
                   (assoc-in [:session/tasks :t1 :status] :doing)
                   (assoc-in [:session/tasks :t2]
                     {:title "second" :status :todo :born "t2/i1/f1"}))
          snap-3 (-> snap-2
                   ;; archive t1
                   (update :session/tasks dissoc :t1))
          history {1 snap-1 2 snap-2 3 snap-3}]

      (it "introspect-task returns latest snapshot the entry existed in"
        (let [r (eng/introspect-task history :t1)]
          (expect (= "first" (:title r)))
          (expect (= :doing (:status r)))             ; updated in turn 2
          (expect (= 2 (:as-of-turn r)))))            ; archived in turn 3

      (it "introspect-task returns nil for unknown key"
        (expect (nil? (eng/introspect-task history :never-existed))))

      (it "introspect-ctx-at returns the full snapshot for the turn"
        (expect (= snap-2 (eng/introspect-ctx-at history 2)))
        (expect (nil? (eng/introspect-ctx-at history 99))))

      (it "introspect-archived enumerates tasks missing from latest"
        (let [arch (eng/introspect-archived history :tasks)
              keys (set (map :key arch))]
          (expect (contains? keys :t1))
          (expect (not (contains? keys :t2)))))))

  (describe "introspect-fact tracks a fact across snapshots"
    (let [snap-1 (-> (eng/empty-ctx "h")
                   (assoc-in [:session/facts :f1]
                     {:content "v1" :status :active :born "t1/i1/f1"}))
          snap-2 (-> snap-1
                   (assoc-in [:session/facts :f1 :status] :superseded))
          history {1 snap-1 2 snap-2}]
      (it "returns the latest snapshot the fact existed in"
        (let [r (eng/introspect-fact history :f1)]
          (expect (= "v1" (:content r)))
          (expect (= :superseded (:status r)))
          (expect (= 2 (:as-of-turn r))))))))

;; =============================================================================
;; apply-task-set! — hook-task idempotent repeat (D12)
;; =============================================================================

(defdescribe hook-task-repeat-noop-test
  (describe "apply-mutator :task-set! — hook re-emission is a silent no-op"
    (let [ctx0 (-> (eng/empty-ctx "test")
                 (assoc :session/turn 1)
                 (assoc :session/scope {:turn 1 :iter 1 :next-form 1}))
          payload {:title "set title" :status :todo
                   :source :hook :hook-id :vis.foundation/session-title}
          ;; First fire: create
          r1 (eng/apply-mutator ctx0 "t1/i1/f1" :task-set!
               [:vis.foundation/session-title payload])
          ;; Second fire: noop dedup (even with different :title in the partial)
          r2 (eng/apply-mutator (:ctx r1) "t1/i1/f2" :task-set!
               [:vis.foundation/session-title (assoc payload :title "DIFFERENT")])]

      (it "first fire stamps the task"
        (expect (= "set title" (get-in (:ctx r1) [:session/tasks :vis.foundation/session-title :title])))
        (expect (true? (:stamped? r1))))

      (it "second fire is a silent no-op (no overwrite, no warning)"
        (expect (= "set title" (get-in (:ctx r2) [:session/tasks :vis.foundation/session-title :title])))
        (expect (false? (:stamped? r2)))
        (expect (empty? (:warnings r2)))))))

(defdescribe hook-task-doesnt-block-user-edit-test
  (describe "apply-mutator :task-set! — model partial against a hook task overwrites"
    (let [ctx0 (-> (eng/empty-ctx "test")
                 (assoc :session/turn 1)
                 (assoc :session/scope {:turn 1 :iter 1 :next-form 1}))
          ;; Hook plants the task.
          r1 (eng/apply-mutator ctx0 "t1/i1/f1" :task-set!
               [:vis.foundation/session-title
                {:title "hook-title" :status :todo
                 :source :hook :hook-id :vis.foundation/session-title}])
          ;; Model writes :status :done — NOT a hook re-emission (no
          ;; :source :hook on the partial), so the write goes through and
          ;; done is self-asserted.
          r2 (eng/apply-mutator (:ctx r1) "t1/i2/f1" :task-set!
               [:vis.foundation/session-title {:status :done}])]

      (it "model write lands and stamps :done-born"
        (expect (= :done (get-in (:ctx r2) [:session/tasks :vis.foundation/session-title :status])))
        (expect (= "t1/i2/f1" (get-in (:ctx r2) [:session/tasks :vis.foundation/session-title :done-born])))))))
