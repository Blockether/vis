(ns com.blockether.vis.internal.ctx-engine-done-test
  "Tests for the `done` handler (answer-fact + trailer sort) and the
   standalone `summarize` cleanup verb (trailer ranges + entity collapse),
   partial overlap detection, the trailer comparator, history
   introspection, and hook-task idempotent re-emission."
  (:require
   [clojure.string :as str]
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
                  [(pin "t1/i1" "cat")
                   (pin "t2/i1" "patch")
                   (pin "t2/i2" "cat")
                   (pin "t2/i3" "tests")
                   (pin "t3/i1" "more")]))]

      (it "summarize collapses a trailer range into one stub"
        (let [{ctx' :ctx ws :warnings}
              (eng/apply-summarize ctx "t4/i1/f1"
                {:trailer [{:scope-start "t2/i1" :scope-end "t2/i3"
                            :summary "patch+test cycle"}]})]
          (expect (empty? ws))
          (expect (= 3 (count (:session/trailer ctx'))))
          (expect (some :scope-start (:session/trailer ctx')))))

      (it "collapses N facts into one new summary fact and archives originals"
        (let [ctx-f (-> (eng/empty-ctx)
                      (assoc :session/turn 4)
                      (assoc :session/facts
                        {:a {:content "alpha" :status :active :born "t2/i1/f1"}
                         :b {:content "beta"  :status :active :born "t2/i2/f1"}}))
              {ctx' :ctx} (eng/apply-summarize ctx-f "t4/i1/f1"
                            {:facts [{:keys [:a :b] :into :ab
                                      :summary "alpha+beta settled"}]})]
          ;; new summary fact exists with the recap content
          (expect (= "alpha+beta settled" (get-in ctx' [:session/facts :ab :content])))
          (expect (= [:a :b] (get-in ctx' [:session/facts :ab :summarized-from])))
          ;; originals flipped :archived
          (expect (= :archived (get-in ctx' [:session/facts :a :status])))
          (expect (= :archived (get-in ctx' [:session/facts :b :status])))))

      (it "collapses N tasks into one new summary FACT and archives the tasks"
        (let [ctx-t (-> (eng/empty-ctx)
                      (assoc :session/turn 4)
                      (assoc :session/tasks
                        {:t1 {:title "one" :status :done :born "t2/i1/f1"}
                         :t2 {:title "two" :status :done :born "t2/i2/f1"}}))
              {ctx' :ctx} (eng/apply-summarize ctx-t "t4/i1/f1"
                            {:tasks [{:keys [:t1 :t2] :into :setup
                                      :summary "env wired"}]})]
          (expect (= "env wired" (get-in ctx' [:session/facts :setup :content])))
          (expect (= :archived (get-in ctx' [:session/tasks :t1 :status])))
          (expect (= :archived (get-in ctx' [:session/tasks :t2 :status])))))

      (it "auto-generates a summary fact key when :into is omitted"
        (let [ctx-f (-> (eng/empty-ctx)
                      (assoc :session/turn 7)
                      (assoc :session/facts
                        {:a {:content "alpha" :status :active :born "t2/i1/f1"}}))
              {ctx' :ctx} (eng/apply-summarize ctx-f "t7/i1/f1"
                            {:facts [{:keys [:a] :summary "recap"}]})]
          (expect (= "recap" (get-in ctx' [:session/facts :summary-t7-fact-1 :content])))))

      ;; The auto-answer fact must be keyed by the SAME snake STRING the agent
      ;; sees + passes, or recall/restore/summarize of it silently miss. This
      ;; pins the round-trip the prompt teaches: done() -> turn_N fact (Q+A in
      ;; one markdown :content) -> summarize folds it by that string key.
      (it "done() writes a string-keyed turn_<N> Q/A fact that summarize can fold"
        (let [ctx0 (assoc (eng/empty-ctx) :session/turn 3)
              {ctx1 :ctx} (eng/apply-done ctx0 "t3/i2/f1"
                            {:answer "the final answer" :user-request "do x"})
              af (get-in ctx1 [:session/facts "turn_3"])
              content (:content af)]
          ;; agent-facing STRING key "turn_3", not a keyword
          (expect (some? af))
          (expect (= :done-auto (:source af)))
          ;; Q + A live in ONE markdown :content under two headings; no :question
          (expect (str/includes? content "## Question"))
          (expect (str/includes? content "do x"))
          (expect (str/includes? content "## Answer"))
          (expect (str/includes? content "the final answer"))
          (expect (nil? (:question af)))
          (expect (nil? (get-in ctx1 [:session/facts :turn-3])))
          ;; …and it folds by that same string key (what the prompt instructs)
          (let [{ctx2 :ctx ws :warnings}
                (eng/apply-summarize ctx1 "t4/i1/f1"
                  {:facts [{:keys ["turn_3"] :into "early_turns"
                            :summary "t3: did x"}]})]
            (expect (empty? ws))
            (expect (= "t3: did x" (get-in ctx2 [:session/facts "early_turns" :content])))
            (expect (= :archived (get-in ctx2 [:session/facts "turn_3" :status]))))))

      (it "warns on partial-overlap between summarize ranges and existing summary"
        (let [ctx-with (-> ctx
                         (assoc :session/trailer
                           [(summary "t2/i1" "t2/i5" "first")
                            (pin "t3/i1" "x")]))
              {ws :warnings}
              (eng/apply-summarize ctx-with "t9/i1/f1"
                {:trailer [{:scope-start "t2/i3" :scope-end "t3/i1"
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
