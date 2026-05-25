(ns com.blockether.vis.internal.ctx-engine-test
  "Pure-fn tests + REPL-replayable scenarios for the CTX engine.

   Test categories:

     1. **Unit / property tests** — one fn at a time. Mostly hit by
        `s/gen ::cs/ctx` generators when the fn is data-shape-checking,
        plus targeted positive/negative cases for parsing + classification.

     2. **Scenario tests** — multi-turn transcripts (`scenarios` ns) replayed
        through `apply-mutator` etc., with `:expect` assertions at each turn
        boundary. Each scenario is intentionally a *narrative* of what a
        real model would emit. Failing assertions point at gaps in the
        engine.

   The pattern: scenarios are written first, fn stubs added to `ctx-engine`,
   each red turn drives one piece of implementation. Once green, the
   scenario becomes a regression."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-spec :as cs]
   [lazytest.core :refer [defdescribe describe expect it]]))

;; =============================================================================
;; Parse / scope helpers
;; =============================================================================

(defdescribe parse-scope-test
  (describe "parse-scope-form"
    (it "parses well-formed form-scope"
      (expect (= {:turn 3 :iter 2 :form 1}
                (eng/parse-scope-form "t3/i2/f1"))))

    (it "returns nil for iter-scope"
      (expect (nil? (eng/parse-scope-form "t3/i2"))))

    (it "returns nil for malformed"
      (expect (nil? (eng/parse-scope-form "t01/i1/f1")))
      (expect (nil? (eng/parse-scope-form "3/2/1")))
      (expect (nil? (eng/parse-scope-form ""))))

    (it "returns nil for non-strings"
      (expect (nil? (eng/parse-scope-form 42)))
      (expect (nil? (eng/parse-scope-form nil))))))

(defdescribe scope-compare-test
  (describe "scope-compare total ordering"
    (it "orders by turn, then iter, then form"
      (let [scopes ["t2/i1/f1" "t1/i3/f5" "t1/i1/f1" "t10/i1/f1"]
            sorted (sort eng/scope-compare scopes)]
        (expect (= ["t1/i1/f1" "t1/i3/f5" "t2/i1/f1" "t10/i1/f1"] sorted))))

    (it "is stable on equal scopes"
      (expect (zero? (eng/scope-compare "t1/i1/f1" "t1/i1/f1"))))

    (it "places malformed before valid"
      (expect (neg? (eng/scope-compare "bad" "t1/i1/f1"))))))

;; =============================================================================
;; classify-scope — exhaustive class coverage
;; =============================================================================

(defdescribe classify-scope-test
  (describe "classify-scope vs cursor + form-results"
    (let [cursor {:turn 3 :iter 2 :next-form 4}
          fr     {"t3/i2/f1" {:result :ok}
                  "t3/i2/f2" {:result :ok}
                  "t3/i2/f3" {:error "boom"}
                  "t2/i5/f1" {:result :ok}
                  "t1/i1/f1" {:result :ok}}]

      (it ":malformed when regex fails"
        (expect (= :malformed (eng/classify-scope "garbage" cursor fr)))
        (expect (= :malformed (eng/classify-scope "t01/i1/f1" cursor fr))))

      (it ":future-turn"
        (expect (= :future-turn (eng/classify-scope "t4/i1/f1" cursor fr))))

      (it ":future-iter (same turn, later iter)"
        (expect (= :future-iter (eng/classify-scope "t3/i3/f1" cursor fr))))

      (it ":future-form (same iter, form >= next-form)"
        (expect (= :future-form (eng/classify-scope "t3/i2/f4" cursor fr)))
        (expect (= :future-form (eng/classify-scope "t3/i2/f99" cursor fr))))

      (it ":errored when form-results carry an :error"
        (expect (= :errored (eng/classify-scope "t3/i2/f3" cursor fr))))

      (it ":unknown when past-or-current but not in form-results"
        (expect (= :unknown (eng/classify-scope "t1/i2/f1" cursor fr)))
        (expect (= :unknown (eng/classify-scope "t2/i1/f1" cursor fr))))

      (it ":ok when present + no error"
        (expect (= :ok (eng/classify-scope "t3/i2/f1" cursor fr)))
        (expect (= :ok (eng/classify-scope "t2/i5/f1" cursor fr)))
        (expect (= :ok (eng/classify-scope "t1/i1/f1" cursor fr)))))))

;; =============================================================================
;; build-indexes — shape + cross-references
;; =============================================================================

(def ^:private mini-ctx
  "Minimal but realistic CTX for index assertions."
  {:session/id    "test"
   :session/turn  3
   :session/scope {:turn 3 :iter 1 :next-form 1}
   :session/workspace
   {:vcs/branch "main" :vcs/trunk "main" :vcs/head "x"
    :vcs/dirty? false :vcs/stats {}}
   :session/symbols {}
   :session/specs
   {:rl {:title "rate-limit"
         :requirements [{:id :r1 :title "x" :facts [:f1]}
                        {:id :r2 :title "y"}]
         :status :doing
         :born "t1/i1/f1"}}
   :session/tasks
   {:t1 {:title "task 1"
         :specs {:rl [{:requirement :r1 :proof "t2/i1/f1"}]}
         :status :done
         :born "t1/i1/f2"
         :done-born "t2/i1/f2"}
    :t2 {:title "task 2"
         :specs {:rl []}
         :depends-on [:t1]
         :status :todo
         :born "t1/i1/f3"}}
   :session/facts
   {:f1 {:content "fact one" :born "t1/i1/f4"}}
   :session/trailer []})

(defdescribe build-indexes-test
  (describe "build-indexes shape"
    (let [idx (eng/build-indexes mini-ctx)]
      (it ":req-index keyed by requirement id"
        (expect (= :rl (get-in idx [:req-index :r1 :spec])))
        (expect (= :rl (get-in idx [:req-index :r2 :spec]))))

      (it ":proof-index keyed by [spec-id req-id]"
        ;; Phase E: entries now carry :scopes (vec) alongside the
        ;; legacy :scope (first sub-scope) so derive-progression can
        ;; reason about both singleton and compose proofs uniformly.
        (expect (= [{:task :t1 :scope "t2/i1/f1" :scopes ["t2/i1/f1"]}]
                  (get-in idx [:proof-index [:rl :r1]])))
        (expect (nil? (get-in idx [:proof-index [:rl :r2]]))))

      (it ":task-by-spec maps spec-id → set of task-ids"
        (expect (= #{:t1 :t2} (get-in idx [:task-by-spec :rl]))))

      (it ":fact-refs is the reverse of requirement :facts"
        (expect (= #{:r1} (get-in idx [:fact-refs :f1]))))

      (it ":dep-graph captures :depends-on edges as typed refs"
        (expect (= #{[:task :t1]} (get-in idx [:dep-graph [:task :t2]])))
        (expect (= #{} (get-in idx [:dep-graph [:task :t1]]))))

      (it ":rev-deps reverses the typed graph"
        (expect (= #{[:task :t2]} (get-in idx [:rev-deps [:task :t1]]))))

      (it ":fact-status defaults to :active when omitted"
        (expect (= :active (get-in idx [:fact-status :f1]))))))

  (describe "build-indexes is pure"
    (it "produces identical output on repeated calls"
      (expect (= (eng/build-indexes mini-ctx)
                (eng/build-indexes mini-ctx))))

    (it "tolerates empty subtrees"
      (let [empty-ctx (-> mini-ctx
                        (assoc :session/specs {} :session/tasks {} :session/facts {}))]
        (expect (= {} (:req-index (eng/build-indexes empty-ctx))))
        (expect (= {} (:proof-index (eng/build-indexes empty-ctx))))
        (expect (= {} (:dep-graph (eng/build-indexes empty-ctx))))))))

;; =============================================================================
;; depends-on cycle detection
;; =============================================================================

(defdescribe depends-on-cycle-test
  (describe "depends-on-cycle?"
    (it "returns nil on a DAG"
      (expect (nil? (eng/depends-on-cycle? {:a #{:b} :b #{:c} :c #{}}))))

    (it "returns the cycle path on a simple loop"
      (let [cyc (eng/depends-on-cycle? {:a #{:b} :b #{:a}})]
        (expect (and (vector? cyc) (= 3 (count cyc))))))

    (it "returns cycle path on a self-loop"
      (let [cyc (eng/depends-on-cycle? {:a #{:a}})]
        (expect (and (vector? cyc) (= [:a :a] cyc)))))

    (it "handles disconnected components"
      (expect (some? (eng/depends-on-cycle? {:a #{:b} :b #{:a}
                                             :c #{:d} :d #{}}))))))

;; =============================================================================
;; derive-progression
;; =============================================================================

(defdescribe derive-progression-test
  (describe "derive-progression"
    (let [idx  (eng/build-indexes mini-ctx)
          prog (eng/derive-progression mini-ctx idx)]
      (it "computes total + proven counts"
        (expect (= 2 (get-in prog [:rl :total])))
        (expect (= 1 (get-in prog [:rl :proven]))))

      (it ":ratio is double in [0,1]"
        (expect (= 0.5 (get-in prog [:rl :ratio]))))

      (it ":state :partial when 0 < proven < total"
        (expect (= :partial (get-in prog [:rl :state]))))

      (it ":missing carries unproven requirement ids"
        (expect (= #{:r2} (get-in prog [:rl :missing])))))

    (it ":state :empty when spec has no requirements"
      (let [ctx (assoc-in mini-ctx [:session/specs :rl :requirements] [])
            idx (eng/build-indexes ctx)
            prog (eng/derive-progression ctx idx)]
        (expect (= :empty (get-in prog [:rl :state])))
        (expect (zero? (get-in prog [:rl :total])))))

    (it ":state :ready when every requirement has a proof"
      (let [ctx (-> mini-ctx
                  (assoc-in [:session/tasks :t1 :specs :rl]
                    [{:requirement :r1 :proof "t2/i1/f1"}
                     {:requirement :r2 :proof "t2/i1/f2"}]))
            idx (eng/build-indexes ctx)
            prog (eng/derive-progression ctx idx)]
        (expect (= :ready (get-in prog [:rl :state])))
        (expect (= 1.0 (get-in prog [:rl :ratio])))))

    (it "form-results filter narrows :proven when scope not executed"
      (let [idx (eng/build-indexes mini-ctx)
            prog (eng/derive-progression mini-ctx idx #{})]   ;; no executed forms
        (expect (zero? (get-in prog [:rl :proven])))))))

;; =============================================================================
;; Property: generated ctx always builds indexes cleanly
;; =============================================================================

(defdescribe build-indexes-generative-test
  (describe "build-indexes never throws on generated ctx"
    (it "returns a map for 25 random samples"
      (let [samples (gen/sample (s/gen ::cs/ctx) 25)]
        (expect (every? (fn [c] (map? (eng/build-indexes c))) samples))))

    (it "every value in every index slot is iterable / lookup-safe"
      (let [samples (gen/sample (s/gen ::cs/ctx) 10)
            idxs   (map eng/build-indexes samples)]
        (expect (every? #(map? (:req-index %)) idxs))
        (expect (every? #(map? (:proof-index %)) idxs))
        (expect (every? #(map? (:dep-graph %)) idxs))))))

;; =============================================================================
;; Form tag classification + blocks→forms projection
;; =============================================================================

(defdescribe classify-form-tag-test
  (describe "classify-form-tag"
    (it ":mutation for engine-owned mutators"
      (expect (= :mutation (eng/classify-form-tag "(spec-set! :K {:title \"x\"})")))
      (expect (= :mutation (eng/classify-form-tag "(task-set! :A {})")))
      (expect (= :mutation (eng/classify-form-tag "(fact-set! :F {:content \"y\"})")))
      (expect (= :mutation (eng/classify-form-tag "(req-add! :S {})")))
      (expect (= :mutation (eng/classify-form-tag "(proof-add! :T :S {:requirement :r1 :proof \"t1/i1/f1\"})"))))

    (it ":mutation for def / defn / defmacro"
      (expect (= :mutation (eng/classify-form-tag "(def x 42)")))
      (expect (= :mutation (eng/classify-form-tag "(defn f [a] a)"))))

    (it ":mutation for control verbs (D12: satisfy-hint! retired)"
      (expect (= :mutation (eng/classify-form-tag "(done {:answer \"hi\"})")))
      (expect (= :mutation (eng/classify-form-tag "(set-session-title! \"x\")")))
      ;; satisfy-hint! is no longer a primitive; hook-task satisfaction
      ;; goes through task-set!, which is already in mutation-heads.
      (expect (= :observation (eng/classify-form-tag "(satisfy-hint! :h)"))))

    (it ":observation for everything else"
      (expect (= :observation (eng/classify-form-tag "(+ 1 2)")))
      (expect (= :observation (eng/classify-form-tag "(v/cat \"src/x.clj\")")))
      (expect (= :observation (eng/classify-form-tag "(introspect-form \"t1/i1/f1\")")))
      (expect (= :observation (eng/classify-form-tag "42")))
      (expect (= :observation (eng/classify-form-tag ":kw")))
      (expect (= :observation (eng/classify-form-tag ""))))

    (it "extension tool heads default to :observation when no resolver is wired"
      ;; Extension-owned heads like `v/patch` are NOT in the engine's
      ;; core-mutation-heads. The 1-arity `classify-form-tag` (pure,
      ;; no registry access) returns :observation. The integration
      ;; layer in `loop.clj` passes a `head-tag-resolver` that hits
      ;; `extension/op-tag`; with that resolver, the same head
      ;; classifies as :mutation.
      (expect (= :observation (eng/classify-form-tag "(v/patch [{:path \"x\"}])")))
      (expect (= :mutation
                (eng/classify-form-tag "(v/patch [{:path \"x\"}])"
                  (fn [head] (when (= 'v/patch head) :mutation))))))

    (it "resolver-supplied tag wins over the engine's core fallback"
      ;; A resolver can also flip an engine-owned head, but in
      ;; practice extensions only declare their own ops; this just
      ;; documents the precedence rule.
      (expect (= :observation
                (eng/classify-form-tag "(spec-set! :K {:title \"x\"})"
                  (fn [_] :observation))))
      (expect (= :mutation
                (eng/classify-form-tag "(spec-set! :K {:title \"x\"})"))))))

(defdescribe advance-iter-trailer-test
  (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
              :session/trailer []}
        obs1 [{:scope "t1/i1/f1" :tag :observation :src "(v/cat \"a\")" :result "old"}]
        obs2 [{:scope "t1/i2/f1" :tag :observation :src "(v/cat \"b\")" :result "new"}]
        mut3 [{:scope "t1/i3/f1" :tag :mutation :src "(v/patch [])" :result []}]]

    (it "carries observation-only pins while no mutation occurs"
      (let [ctx1 (eng/advance-iter base obs1)
            ctx2 (eng/advance-iter ctx1 obs2)]
        (expect (= ["t1/i1" "t1/i2"]
                  (mapv :scope (:session/trailer ctx2))))))

    (it "drops older observation-only pins when a later mutation occurs"
      (let [ctx1 (eng/advance-iter base obs1)
            ctx2 (eng/advance-iter ctx1 obs2)
            ctx3 (eng/advance-iter ctx2 mut3)]
        (expect (= ["t1/i3"]
                  (mapv :scope (:session/trailer ctx3))))))

    (it "keeps earlier pins even when the model rebinds the same def"
      ;; Rebinding `(def persist …)` across iterations is intentional model
      ;; behaviour: each attempt refines tool arguments. Trailer pins from
      ;; prior attempts MUST stay so the model can see what it tried and
      ;; switch tactic. Collapsing prior pins on rebind hides the loop
      ;; signal and lets the model run rg 20 times without realising.
      ;; `(done {:trailer-summarize …})` is the only contract for trimming.
      (let [step1 (eng/advance-iter base
                    [{:scope "t1/i1/f1" :tag :mutation
                      :src "(def persist (v/rg {:any [\"a\"]}))"}])
            step2 (eng/advance-iter step1
                    [{:scope "t1/i2/f1" :tag :mutation
                      :src "(def persist (v/rg {:any [\"b\"]}))"}])
            step3 (eng/advance-iter step2
                    [{:scope "t1/i3/f1" :tag :mutation
                      :src "(def other 1)"}])]
        (expect (= ["t1/i1" "t1/i2" "t1/i3"]
                  (mapv :scope (:session/trailer step3))))))))

(defdescribe rebind-loop-warning-test
  (it "flags a `:trailer-rebind-loop` once the same def is rebound 3+ times"
    (let [trailer [{:scope "t1/i1"
                    :forms [{:src "(def persist (v/rg {:any [\"a\"]}))"
                             :tag :mutation :scope "t1/i1/f1"}]}
                   {:scope "t1/i2"
                    :forms [{:src "(def persist (v/rg {:any [\"b\"]}))"
                             :tag :mutation :scope "t1/i2/f1"}]}
                   {:scope "t1/i3"
                    :forms [{:src "(def persist (v/rg {:any [\"c\"]}))"
                             :tag :mutation :scope "t1/i3/f1"}]}]
          ctx {:session/turn 1
               :session/scope {:turn 1 :iter 4 :next-form 1}
               :session/trailer trailer}
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      (expect (some #(= :trailer-rebind-loop (:code %)) warns))
      (expect (= ["persist"]
                (->> warns (filter #(= :trailer-rebind-loop (:code %)))
                  first :anchor)))))

  (it "stays quiet when a different def lands between rebinds"
    (let [trailer [{:scope "t1/i1"
                    :forms [{:src "(def persist 1)" :tag :mutation :scope "t1/i1/f1"}]}
                   {:scope "t1/i2"
                    :forms [{:src "(def other 2)" :tag :mutation :scope "t1/i2/f1"}]}
                   {:scope "t1/i3"
                    :forms [{:src "(def persist 3)" :tag :mutation :scope "t1/i3/f1"}]}]
          ctx {:session/turn 1
               :session/scope {:turn 1 :iter 4 :next-form 1}
               :session/trailer trailer}
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      (expect (not-any? #(= :trailer-rebind-loop (:code %)) warns)))))

;; ---------------------------------------------------------------------------
;; trailer pin filter for :vis/silent results.
;; ---------------------------------------------------------------------------

(defdescribe advance-iter-silent-result-filter-test
  (describe "forms whose :result is :vis/silent are NOT pinned"
    (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
                :session/trailer []}
          mixed [{:scope "t1/i1/f1" :tag :mutation
                  :src "(spec-set! :K {:title \"x\"})"
                  :result :vis/silent}
                 {:scope "t1/i1/f2" :tag :observation
                  :src "(v/cat \"a\")"
                  :result "old"}
                 {:scope "t1/i1/f3" :tag :mutation
                  :src "(fact-set! :baseline {:content \"x\"})"
                  :result :vis/silent}]
          ctx (eng/advance-iter base mixed)
          pin (first (:session/trailer ctx))]

      (it "trailer pin exists for the iter"
        (expect (= "t1/i1" (:scope pin))))

      (it "only the non-silent v/cat form is kept under :forms"
        (expect (= 1 (count (:forms pin))))
        (expect (= "(v/cat \"a\")" (:src (first (:forms pin))))))))

  (describe "an iter with ONLY :vis/silent forms produces no pin"
    (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
                :session/trailer []}
          all-silent [{:scope "t1/i1/f1" :tag :mutation
                       :src "(spec-set! :K {:title \"x\"})"
                       :result :vis/silent}
                      {:scope "t1/i1/f2" :tag :mutation
                       :src "(task-set! :T {:title \"x\"})"
                       :result :vis/silent}]
          ctx (eng/advance-iter base all-silent)]

      (it ":session/trailer stays empty"
        (expect (empty? (:session/trailer ctx))))))

  (describe ":vis/silent flag in the envelope (loop's wrapped shape) also filters"
    (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
                :session/trailer []}
          forms [{:scope "t1/i1/f1" :tag :mutation
                  :src "(spec-set! :K {})"
                  :vis/silent true
                  :result :vis/silent}
                 {:scope "t1/i1/f2" :tag :observation
                  :src "(v/cat \"a\")"
                  :result "..."}]
          ctx (eng/advance-iter base forms)
          pin (first (:session/trailer ctx))]

      (it "only v/cat lands in :forms"
        (expect (= 1 (count (:forms pin)))))))

  (describe "(done …) still excluded even when its :result is not :vis/silent"
    (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
                :session/trailer []}
          forms [{:scope "t1/i1/f1" :tag :observation
                  :src "(v/cat \"a\")"
                  :result "..."}
                 {:scope "t1/i1/f2" :tag :mutation
                  :src "(done {:answer \"x\"})"
                  :result :vis/answer}]
          ctx (eng/advance-iter base forms)
          pin (first (:session/trailer ctx))]

      (it "only v/cat lands in :forms (done excluded)"
        (expect (= 1 (count (:forms pin))))
        (expect (= "(v/cat \"a\")" (:src (first (:forms pin)))))))))

(defdescribe block-envelope-def-deref-test
  (it "derefs the Var returned by `(def NAME …)` so the trailer carries the bound value"
    ;; Regression: model writes `(def persist (v/rg …))`, SCI returns the
    ;; Var, trailer shows `{:vis/ref :expr}` (or `#'sandbox/persist`).
    ;; Model then re-emits `persist` to inspect, wasting an iter.
    ;; block->envelope now derefs IDeref results for def-shaped sources.
    (let [boxed (atom {:files ["a.clj"] :count 1})
          env (eng/block->envelope {:code "(def persist (v/rg :any [\"x\"]))"
                                    :result boxed}
                1 {:turn 3 :iter 4})]
      (expect (= {:files ["a.clj"] :count 1} (:result env)))
      (expect (= :mutation (:tag env)))))

  (it "leaves non-def results untouched"
    (let [env (eng/block->envelope {:code "(+ 1 2)" :result 3}
                1 {:turn 1 :iter 1})]
      (expect (= 3 (:result env)))))

  (it "survives a Var that throws on deref (cell broken at runtime)"
    ;; Defensive: deref-def-result must never bubble an exception. We pass
    ;; through the raw Var so the channel can still render a placeholder.
    (let [boom (reify clojure.lang.IDeref
                 (deref [_] (throw (ex-info "boom" {}))))
          env (eng/block->envelope {:code "(def x boom)" :result boom}
                1 {:turn 1 :iter 1})]
      (expect (identical? boom (:result env)))))

  (it "realizes lazy seqs nested inside vec results so persistence keeps data"
    ;; Regression: model writes `(def env-files (map :path ...))`, the
    ;; next iter observes `[env-files (:hit-count snap-fn) ...]`. Without
    ;; force-realize the lazy seq survives in-memory but freezes as
    ;; `{:vis/ref :expr}` on turn-end; the resumed model sees
    ;; `#:vis{:ref :expr}` and re-runs the probe thinking it's empty.
    (let [lazy (map inc (range 3))
          env (eng/block->envelope {:code "[env-files 7 0]"
                                    :result [lazy 7 0]}
                1 {:turn 1 :iter 1})]
      (expect (= [[1 2 3] 7 0] (:result env)))))

  (it "realizes a bare lazy seq result without crashing"
    (let [lazy (map identity ["a.clj" "b.clj"])
          env (eng/block->envelope {:code "(map identity files)" :result lazy}
                1 {:turn 1 :iter 1})]
      (expect (= ["a.clj" "b.clj"] (:result env)))))

  (it "carries the per-form `:channel` sink onto the envelope when present (regression: badge missing on def-wrapped tool calls)"
    ;; Regression for conversation 11d4f817-fbd1-43ab-a6b4-052c8557af0a
    ;; turn 2 \"show me ls\":
    ;;   model wrote `(def r (v/ls \".\"))` per the engine contract
    ;;   (\"bind values to defs\"). SCI's def unwrapped the tool envelope
    ;;   to its inner :result, so the persisted block's :result is a
    ;;   plain `{:vis.op :v/ls …}` map without `:success?`. The TUI's
    ;;   `render-tool-result` then refused to dispatch to the v/ls
    ;;   renderer (envelope guard) and the bubble showed plain EDN —
    ;;   no widget, no badge. The pre-rendered IR already lives on
    ;;   each per-form `:channel` sink entry; block->envelope must
    ;;   carry that vec through so persistence + replay can paint the
    ;;   badge from the sink.
    (let [channel [{:position 1 :form "(v/ls \".\")"
                    :success? true
                    :result [:ir {} [:strong {} "v/ls"] [:p {} ". (844)"]]
                    :error nil}]
          env (eng/block->envelope
                {:code "(def r (v/ls \".\"))"
                 :result {:vis.op :v/ls :path "." :entry-count 844}
                 :channel channel}
                1 {:turn 2 :iter 1})]
      (expect (= (vec channel) (:channel env)))
      (expect (= "t2/i1/f1" (:scope env)))
      (expect (= :mutation (:tag env)))))

  (it "omits `:channel` when the form did not call any tool (no empty-vec noise)"
    (let [env-no-channel (eng/block->envelope {:code "(+ 1 2)" :result 3}
                           1 {:turn 1 :iter 1})
          env-empty-channel (eng/block->envelope {:code "(+ 1 2)" :result 3 :channel []}
                              1 {:turn 1 :iter 1})
          env-nil-channel (eng/block->envelope {:code "(+ 1 2)" :result 3 :channel nil}
                            1 {:turn 1 :iter 1})]
      (expect (not (contains? env-no-channel :channel)))
      (expect (not (contains? env-empty-channel :channel)))
      (expect (not (contains? env-nil-channel :channel))))))

(defdescribe blocks->forms-test
  (describe "blocks->forms"
    (let [cursor {:turn 5 :iter 2}
          blocks [{:code "(spec-set! :K {:title \"x\"})" :result :ok}
                  {:code "(v/cat \"a.clj\")"          :result "(ns a) ..."}
                  {:code "(/ 1 0)" :error {:message "Divide by zero"}}]
          forms (eng/blocks->forms blocks cursor)]

      (it "preserves block order (1-based scope :form)"
        (expect (= ["t5/i2/f1" "t5/i2/f2" "t5/i2/f3"] (mapv :scope forms))))

      (it "classifies tags from source"
        (expect (= [:mutation :observation :observation] (mapv :tag forms))))

      (it "carries :result when present"
        (expect (= :ok (:result (first forms))))
        (expect (= "(ns a) ..." (:result (second forms)))))

      (it "carries :error when present, omits :result on error"
        (expect (= {:message "Divide by zero"} (:error (nth forms 2))))
        (expect (not (contains? (nth forms 2) :result))))

      (it "every envelope has :src from block :code"
        (expect (= "(spec-set! :K {:title \"x\"})" (:src (first forms)))))

      (it "empty input returns empty vec"
        (expect (= [] (eng/blocks->forms [] cursor)))))))

(defdescribe gc-pass-turn-lifetime-test
  ;; Vis conv 11d4f817 / t14–t16: a transient context-pressure
  ;; hook-task lingered for 6 turns after the originating hint had
  ;; stopped firing, because the model marked it :done with a proof
  ;; scope that pointed at the (task-set! …) form itself instead of
  ;; the sibling (done …), the validator (which requires (done …) or
  ;; trailer-summarize/drop source) said no, and the renderer kept
  ;; surfacing the resulting :done :validated? false task. Hooks now
  ;; opt in via `:lifetime :turn` and `gc-pass` drops those entries at
  ;; advance-turn so the next iter starts clean — re-created on
  ;; demand by the hook only when the condition still holds.
  (describe "gc-pass drops :lifetime :turn hook-tasks regardless of status"
    (it "drops an open :lifetime :turn hook-task at advance-turn"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/context-pressure
                                 {:title "warn" :source :hook
                                  :hook-id :vis.foundation/context-pressure
                                  :status :todo :lifetime :turn
                                  :validator-fn "(fn [_] true)"
                                  :born "t1/i1/f1"}}}]
        (expect (= {} (:session/tasks (eng/advance-turn ctx))))))

    (it "drops a :done :validated? false :lifetime :turn task immediately"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/context-pressure
                                 {:title "warn" :source :hook
                                  :hook-id :vis.foundation/context-pressure
                                  :status :done :validated? false
                                  :lifetime :turn
                                  :validator-fn "(fn [_] true)"
                                  :proof "t1/i1/f2" :done-born "t1/i1/f2"
                                  :born "t1/i1/f1"}}}]
        (expect (= {} (:session/tasks (eng/advance-turn ctx))))))

    (it "keeps :lifetime :session hook-tasks until the TTL kicks in"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/session-title
                                 {:title "set title" :source :hook
                                  :hook-id :vis.foundation/session-title
                                  :status :todo
                                  :validator-fn "(fn [_] true)"
                                  :born "t1/i1/f1"}}}
            advanced (eng/advance-turn ctx)]
        (expect (contains? (:session/tasks advanced) :vis.foundation/session-title))))

    (it "keeps user-source tasks even if :lifetime :turn is set on them"
      ;; Defensive: only HOOK-source tasks are turn-pruned. A model-
      ;; written task with :lifetime :turn (no current path produces
      ;; this, but the renderer can't tell) survives until its own
      ;; TTL or an explicit :cancelled flip.
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:user.thing
                                 {:title "do x" :source :user
                                  :status :todo :lifetime :turn
                                  :born "t1/i1/f1"}}}]
        (expect (contains? (:session/tasks (eng/advance-turn ctx)) :user.thing))))))

(defdescribe advance-iter-iteration-lifetime-test
  ;; `:lifetime :iteration` hook-tasks evaporate at every iter
  ;; boundary, not just turn boundary. Use for hyper-transient signals
  ;; whose value disappears the moment the iter that emitted them
  ;; ends (e.g. a one-iter retry-shape banner). The next iter's hook
  ;; fire is the single source of truth.
  (describe "advance-iter drops :lifetime :iteration hook-tasks"
    (it "drops an open :lifetime :iteration hook-task at advance-iter"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/retry-shape
                                 {:title "retry" :source :hook
                                  :hook-id :vis.foundation/retry-shape
                                  :status :todo :lifetime :iteration
                                  :validator-fn "(fn [_] true)"
                                  :born "t1/i1/f1"}}}]
        (expect (= {} (:session/tasks (eng/advance-iter ctx []))))))

    (it "drops a :done :validated? false :lifetime :iteration task immediately"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/retry-shape
                                 {:title "retry" :source :hook
                                  :hook-id :vis.foundation/retry-shape
                                  :status :done :validated? false
                                  :lifetime :iteration
                                  :validator-fn "(fn [_] true)"
                                  :proof "t1/i1/f2" :done-born "t1/i1/f2"
                                  :born "t1/i1/f1"}}}]
        (expect (= {} (:session/tasks (eng/advance-iter ctx []))))))

    (it "keeps :lifetime :turn hook-tasks across advance-iter (turn boundary owns them)"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/context-pressure
                                 {:title "warn" :source :hook
                                  :hook-id :vis.foundation/context-pressure
                                  :status :todo :lifetime :turn
                                  :validator-fn "(fn [_] true)"
                                  :born "t1/i1/f1"}}}]
        (expect (contains? (:session/tasks (eng/advance-iter ctx []))
                  :vis.foundation/context-pressure))))

    (it "keeps :lifetime :session hook-tasks across advance-iter"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/session-title
                                 {:title "set title" :source :hook
                                  :hook-id :vis.foundation/session-title
                                  :status :todo
                                  :validator-fn "(fn [_] true)"
                                  :born "t1/i1/f1"}}}]
        (expect (contains? (:session/tasks (eng/advance-iter ctx []))
                  :vis.foundation/session-title))))

    (it "ignores :lifetime :iteration on non-hook-source tasks"
      ;; Defensive: only HOOK-source tasks are iter-pruned. A model-
      ;; or user-written task that happens to carry the keyword keeps
      ;; its TTL-based lifetime.
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:user.thing
                                 {:title "do x" :source :user
                                  :status :todo :lifetime :iteration
                                  :born "t1/i1/f1"}}}]
        (expect (contains? (:session/tasks (eng/advance-iter ctx []))
                  :user.thing))))

    (it "still advances the cursor + appends trailer pin as before"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 2 :next-form 1}
                 :session/trailer []
                 :session/tasks {:vis.foundation/retry-shape
                                 {:title "retry" :source :hook
                                  :hook-id :vis.foundation/retry-shape
                                  :status :todo :lifetime :iteration
                                  :validator-fn "(fn [_] true)"
                                  :born "t1/i2/f1"}}}
            advanced (eng/advance-iter ctx
                       [{:scope "t1/i2/f1" :src "(v/cat \"a.clj\")"
                         :tag :observation :result "ok"}])]
        (expect (= 3 (:iter (:session/scope advanced))))
        (expect (= 1 (:next-form (:session/scope advanced))))
        (expect (= 1 (count (:session/trailer advanced))))
        (expect (= {} (:session/tasks advanced)))))))

;; =============================================================================
;; Failed-proof archive (Phase A: introspect-failed-proofs)
;;
;; When a task-level proof references a req carrying a :validator-fn, and
;; that validator REJECTS the form result at the proof scope, the engine's
;; `archive-failed-task-proofs` records the rejection on the task's
;; `:archived-proofs` vec. The :specs entry stays — model owns regular
;; task lifecycle — but the failure is now introspectable next iter so
;; the model can swap evidence or change strategy instead of re-emitting
;; the same rejected scope.
;;
;; For HOOK-source tasks, the existing `reconcile-done-hook-tasks` reverts
;; the task to :todo and now also stamps :archived-proofs with the same
;; entry shape.
;; =============================================================================

(defdescribe archive-failed-task-proofs-test
  (describe "regular task proof rejected by validator → archive entry"
    (let [;; spec :K with req :r1 whose validator demands :result = 42
          spec-K {:title "answer"
                  :requirements [{:id :r1 :title "answer must be 42"
                                  :validator-fn "(fn [{:keys [result]}] (= result 42))"}]
                  :status :doing
                  :born "t1/i1/f1"}
          task-K {:title "compute answer"
                  :specs {:K [{:requirement :r1 :proof "t2/i1/f1"}]}
                  :status :doing
                  :born "t2/i1/f1"}
          ctx {:session/turn 2
               :session/scope {:turn 2 :iter 2 :next-form 1}
               :session/specs {:K spec-K}
               :session/tasks {:T task-K}}
          ;; form-result at the proof scope returned 7, NOT 42 → validator rejects
          form-results {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation
                                    :src "(+ 1 6)" :result 7}}
          ctx' (eng/archive-failed-task-proofs ctx form-results)
          archive (get-in ctx' [:session/tasks :T :archived-proofs])]

      (it "emits one archive entry"
        (expect (= 1 (count archive))))

      (it "captures the rejected proof scope + req + spec identifiers"
        (let [e (first archive)]
          (expect (= "t2/i1/f1" (:proof e)))
          (expect (= :K          (:spec e)))
          (expect (= :r1         (:requirement e)))))

      (it "captures the rejection cause as :proof-validator-fail"
        (expect (= :proof-validator-fail (:rejected-by (first archive)))))

      (it "stamps the rejection at the CURRENT iter scope, not the proof scope"
        ;; cursor sits at t2/i2; the proof scope was t2/i1. The archive
        ;; records WHEN the rejection happened, not where the proof points.
        (expect (= "t2/i2" (:rejected-at (first archive)))))

      (it "does NOT remove the :specs proof entry — model owns the lifecycle"
        ;; The model can still see the proof in :specs and decide to
        ;; (proof-remove!) + (proof-add!) a different scope. Engine only
        ;; stamps history.
        (expect (= [{:requirement :r1 :proof "t2/i1/f1"}]
                  (get-in ctx' [:session/tasks :T :specs :K]))))))

  (describe "validator passes → no archive entry"
    (let [spec-K {:title "answer"
                  :requirements [{:id :r1 :title "answer must be 42"
                                  :validator-fn "(fn [{:keys [result]}] (= result 42))"}]
                  :status :doing :born "t1/i1/f1"}
          task-K {:title "compute" :specs {:K [{:requirement :r1 :proof "t2/i1/f1"}]}
                  :status :doing :born "t2/i1/f1"}
          ctx {:session/turn 2 :session/scope {:turn 2 :iter 2 :next-form 1}
               :session/specs {:K spec-K} :session/tasks {:T task-K}}
          ;; form-result actually equals 42 → validator passes
          form-results {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation
                                    :src "(* 6 7)" :result 42}}
          ctx' (eng/archive-failed-task-proofs ctx form-results)]
      (it "leaves :archived-proofs empty (or nil)"
        (expect (empty? (get-in ctx' [:session/tasks :T :archived-proofs]))))))

  (describe "idempotent within an iter (dedupe by proof+rejected-at+reason)"
    (let [spec-K {:title "answer"
                  :requirements [{:id :r1 :title "must be 42"
                                  :validator-fn "(fn [{:keys [result]}] (= result 42))"}]
                  :status :doing :born "t1/i1/f1"}
          task-K {:title "compute" :specs {:K [{:requirement :r1 :proof "t2/i1/f1"}]}
                  :status :doing :born "t2/i1/f1"}
          ctx {:session/turn 2 :session/scope {:turn 2 :iter 2 :next-form 1}
               :session/specs {:K spec-K} :session/tasks {:T task-K}}
          form-results {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation
                                    :src "(+ 1 6)" :result 7}}
          ;; run TWICE in the same iter — second run must be a no-op
          ctx-1 (eng/archive-failed-task-proofs ctx form-results)
          ctx-2 (eng/archive-failed-task-proofs ctx-1 form-results)]
      (it "does not append a duplicate entry"
        (expect (= 1 (count (get-in ctx-2 [:session/tasks :T :archived-proofs]))))))))

(defdescribe archived-proofs-cap-test
  (describe "archive vec is capped at ARCHIVED_PROOFS_CAP_PER_TASK"
    ;; Simulate many failed iters by manually seeding 11 archive entries
    ;; (each stamped at a unique :rejected-at so dedupe lets them through),
    ;; then run one more archiving pass; oldest must be dropped.
    (let [seeded (vec (for [i (range 11)]
                        {:proof (str "t1/i" i "/f1")
                         :spec :K :requirement :r1
                         :rejected-by :proof-validator-fail
                         :rejected-at (str "t1/i" i)
                         :reason :predicate-false}))
          spec-K {:title "answer"
                  :requirements [{:id :r1 :title "must be 42"
                                  :validator-fn "(fn [{:keys [result]}] (= result 42))"}]
                  :status :doing :born "t1/i1/f1"}
          ;; 12th rejection arrives this iter
          task-K {:title "compute"
                  :specs {:K [{:requirement :r1 :proof "t2/i9/f1"}]}
                  :status :doing
                  :archived-proofs seeded
                  :born "t2/i1/f1"}
          ctx {:session/turn 2 :session/scope {:turn 2 :iter 9 :next-form 2}
               :session/specs {:K spec-K} :session/tasks {:T task-K}}
          form-results {"t2/i9/f1" {:scope "t2/i9/f1" :tag :observation
                                    :src "(+ 1 6)" :result 7}}
          archive (get-in (eng/archive-failed-task-proofs ctx form-results)
                    [:session/tasks :T :archived-proofs])]

      (it "caps the vec at 10 entries"
        (expect (= 10 (count archive))))

      (it "drops the OLDEST entry first (FIFO)"
        ;; the entry stamped at "t1/i0" must be gone
        (expect (not (some #(= "t1/i0" (:rejected-at %)) archive))))

      (it "keeps the newly-archived rejection"
        ;; the new entry is at the current cursor t2/i9
        (expect (some #(= "t2/i9" (:rejected-at %)) archive))))))

(defdescribe reconcile-hook-task-archives-on-revert-test
  (describe "hook-task reverted by reconcile gets :archived-proofs stamped"
    (let [task-K {:title "set title" :source :hook
                  :hook-id :vis.foundation/session-title
                  :status :done
                  :proof "t1/i1/f1"
                  :done-born "t1/i1/f1"
                  :validator-fn "(fn [{:keys [result]}] (= result :title-set))"
                  :born "t1/i1/f1"}
          ctx {:session/turn 1 :session/scope {:turn 1 :iter 2 :next-form 1}
               :session/tasks {:T task-K}}
          ;; form-result :nope ≠ :title-set → validator rejects
          form-results {"t1/i1/f1" {:scope "t1/i1/f1" :tag :mutation
                                    :src "(set-session-title! \"x\")"
                                    :result :nope}}
          {ctx' :ctx warnings :warnings}
          (eng/reconcile-done-hook-tasks ctx form-results)
          archive (get-in ctx' [:session/tasks :T :archived-proofs])]

      (it "reverts :status :done back to :todo"
        (expect (= :todo (get-in ctx' [:session/tasks :T :status]))))

      (it "writes one archive entry capturing the rejected proof"
        (expect (= 1 (count archive)))
        (expect (= "t1/i1/f1" (:proof (first archive)))))

      (it "tags the rejection cause as :task-done-validator-fail"
        (expect (= :task-done-validator-fail (:rejected-by (first archive)))))

      (it "drops :proof from the task body but keeps the archive"
        (expect (nil? (get-in ctx' [:session/tasks :T :proof])))
        (expect (seq archive)))

      (it "still emits a soft warning so the model sees the revert"
        (expect (some #(= :task-done-validator-fail (:code %)) warnings))))))

(defdescribe introspect-failed-proofs-test
  (describe "introspect-failed-proofs returns the archive from the latest snapshot"
    (let [task-T (fn [archive]
                   {:title "compute"
                    :specs {:K [{:requirement :r1 :proof "t2/i1/f1"}]}
                    :status :doing
                    :archived-proofs archive
                    :born "t2/i1/f1"})
          snap-1 {:session/turn 1
                  :session/scope {:turn 1 :iter 1 :next-form 1}
                  :session/tasks {:T (task-T [])}}
          snap-2 {:session/turn 2
                  :session/scope {:turn 2 :iter 2 :next-form 1}
                  :session/tasks {:T (task-T [{:proof "t2/i1/f1" :spec :K
                                               :requirement :r1
                                               :rejected-by :proof-validator-fail
                                               :rejected-at "t2/i2"
                                               :reason :predicate-false}])}}
          history [[1 snap-1] [2 snap-2]]]

      (it "returns the most-recent archive vec"
        (let [archive (eng/introspect-failed-proofs history :T)]
          (expect (= 1 (count archive)))
          (expect (= "t2/i1/f1" (:proof (first archive))))))

      (it "returns nil when the task was never present in any snapshot"
        (expect (nil? (eng/introspect-failed-proofs history :missing))))

      (it "returns empty vec when the task exists but has no archive"
        (let [empty-snap {:session/turn 1
                          :session/scope {:turn 1 :iter 1 :next-form 1}
                          :session/tasks {:T (task-T [])}}]
          (expect (= [] (eng/introspect-failed-proofs [[1 empty-snap]] :T))))))))

;; =============================================================================
;; Phase B: Universal :depends-on across spec/task/fact entities.
;;
;; `:depends-on` becomes a first-class relation on every entity kind, not just
;; tasks. Refs are either bare keys (same-kind shorthand) or typed `[:kind :K]`
;; pairs. Cycle detection is unified across the three subtrees so a chain
;; `task -> spec -> fact -> task` is hard-rejected at write time.
;; =============================================================================

(defdescribe universal-depends-on-test
  (describe "spec-set! / fact-set! accept :depends-on like task-set!"
    (it "spec :depends-on is preserved through apply-spec-set!"
      (let [{ctx :ctx}
            (eng/apply-mutator (eng/empty-ctx "t") "t1/i1/f1"
              :spec-set! [:K {:title "x" :depends-on [:other :extra]}])]
        (expect (= [:other :extra]
                  (get-in ctx [:session/specs :K :depends-on])))))

    (it "fact :depends-on is preserved through apply-fact-set!"
      (let [{ctx :ctx}
            (eng/apply-mutator (eng/empty-ctx "t") "t1/i1/f1"
              :fact-set! [:K {:content "x" :depends-on [[:task :impl]]}])]
        (expect (= [[:task :impl]]
                  (get-in ctx [:session/facts :K :depends-on])))))

    (it "spec-depends! / task-depends! / fact-depends! all write through"
      (let [base (-> (eng/empty-ctx "t")
                   (assoc-in [:session/specs :S] {:title "s" :born "t1/i1/f1"})
                   (assoc-in [:session/tasks :T] {:title "t" :born "t1/i1/f1"})
                   (assoc-in [:session/facts :F] {:content "f" :born "t1/i1/f1"}))
            after-s (:ctx (eng/apply-mutator base "t1/i1/f1" :spec-depends! [:S [[:fact :F]]]))
            after-t (:ctx (eng/apply-mutator after-s "t1/i1/f1" :task-depends! [:T [[:spec :S]]]))
            after-f (:ctx (eng/apply-mutator after-t "t1/i1/f1" :fact-depends! [:F []]))]
        (expect (= [[:fact :F]] (get-in after-f [:session/specs :S :depends-on])))
        (expect (= [[:spec :S]] (get-in after-f [:session/tasks :T :depends-on])))
        (expect (= []           (get-in after-f [:session/facts :F :depends-on])))))))

(defdescribe cross-entity-cycle-rejection-test
  (describe "spec→task→fact→spec cycle is hard-rejected at write time"
    ;; Seed a chain that's already valid up to fact :F, then attempt to close
    ;; the loop with a fact-depends! that points back to spec :S. The engine
    ;; must refuse the write and emit :depends-on-cycle.
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/specs :S]
                  {:title "s" :born "t1/i1/f1" :depends-on [[:task :T]]})
                (assoc-in [:session/tasks :T]
                  {:title "t" :born "t1/i1/f1" :depends-on [[:fact :F]]})
                (assoc-in [:session/facts :F]
                  {:content "f" :born "t1/i1/f1"}))
          {ctx' :ctx warnings :warnings :as out}
          (eng/apply-mutator ctx "t1/i2/f1" :fact-depends! [:F [[:spec :S]]])]

      (it "refuses the write (no :depends-on on :F)"
        (expect (not (:stamped? out)))
        (expect (nil? (get-in ctx' [:session/facts :F :depends-on]))))

      (it "emits a :depends-on-cycle warning"
        (expect (some #(= :depends-on-cycle (:code %)) warnings))))))

(defdescribe build-indexes-dep-graph-typed-test
  (describe "build-indexes derives the typed :dep-graph across all entity kinds"
    ;; Phase B canonical shape: :dep-graph keys are typed `[:kind :K]`
    ;; refs. Bare-key entries on `:depends-on` are normalized to
    ;; same-kind typed refs at index-build time.
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/specs :S]
                  {:title "s" :born "t1/i1/f1" :depends-on [[:fact :F]]})
                (assoc-in [:session/tasks :T]
                  {:title "t" :born "t1/i1/f1" :depends-on [[:spec :S]]})
                (assoc-in [:session/facts :F]
                  {:content "f" :born "t1/i1/f1"}))
          idx (eng/build-indexes ctx)]

      (it "indexes the typed dep-graph with each node present"
        (let [g (:dep-graph idx)]
          (expect (contains? g [:spec :S]))
          (expect (contains? g [:task :T]))
          (expect (contains? g [:fact :F]))))

      (it "edges normalize bare and typed refs uniformly"
        (let [g (:dep-graph idx)]
          (expect (= #{[:fact :F]} (get g [:spec :S])))
          (expect (= #{[:spec :S]} (get g [:task :T])))))

      (it "empty-deps entity still appears as a node with an empty edge set"
        (let [g (:dep-graph idx)]
          (expect (= #{} (get g [:fact :F]))))))))

;; introspect-dep-graph removed: the typed dep-graph is visible inline
;; on every entity's `:depends-on` field in rendered ctx, so a separate
;; introspection fn was redundant surface. `build-indexes` still returns
;; `:dep-graph` for engine internals (cycle detection, derive-warnings)
;; — the test in build-indexes-dep-graph-typed-test covers that path.

(defdescribe depends-on-dangling-warning-test
  (describe "typed :depends-on refs that point at nonexistent entities surface as warnings"
    ;; Engine-level invariant for Phase B: bare-key task→task dangling refs
    ;; were already covered by `:task-dep-dangling`. The new typed pass adds
    ;; cross-entity coverage so a missing `[:spec :ghost]` on any owner is
    ;; flagged.
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/tasks :T]
                  {:title "t" :born "t1/i1/f1" :depends-on [[:spec :ghost]]}))
          idx (eng/build-indexes ctx)
          warns (eng/derive-warnings ctx idx)]
      (it "emits :depends-on-dangling pointing at the missing kind+key"
        (expect (some (fn [w]
                        (and (= :depends-on-dangling (:code w))
                          (some #(= [:spec :ghost] %) (or (:anchor w) []))))
                  warns))))))

;; =============================================================================
;; Phase C: Contradiction detection on facts.
;;
;; `(fact-contradicts! :K1 :K2)` declares a SYMMETRIC, NON-TRANSITIVE link
;; between two facts. The engine writes `:contradicts` on both sides so the
;; warning surfaces regardless of read direction. When both facts stay
;; `:active`, `derive-warnings` emits `:contradicting-facts`. Resolving a
;; contradiction is the model's job (flip one to `:superseded`) — engine
;; never auto-resolves.
;; =============================================================================

(defdescribe fact-contradicts-test
  (describe "fact-contradicts! writes the link on both facts symmetrically"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :f1] {:content "x" :born "t1/i1/f1"})
                (assoc-in [:session/facts :f2] {:content "y" :born "t1/i1/f1"}))
          {ctx' :ctx :as out}
          (eng/apply-mutator ctx "t1/i1/f2" :fact-contradicts! [:f1 :f2])]

      (it "writes :contradicts on the first fact"
        (expect (= #{:f2} (get-in ctx' [:session/facts :f1 :contradicts]))))

      (it "writes :contradicts on the second fact"
        (expect (= #{:f1} (get-in ctx' [:session/facts :f2 :contradicts]))))

      (it "stamped? true"
        (expect (:stamped? out)))))

  (describe "self-contradiction is rejected with a warning"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :f1] {:content "x" :born "t1/i1/f1"}))
          {ctx' :ctx warnings :warnings :as out}
          (eng/apply-mutator ctx "t1/i1/f2" :fact-contradicts! [:f1 :f1])]
      (it "stamped? false"
        (expect (not (:stamped? out))))
      (it "emits :fact-contradicts-self"
        (expect (some #(= :fact-contradicts-self (:code %)) warnings)))
      (it "leaves the fact untouched"
        (expect (nil? (get-in ctx' [:session/facts :f1 :contradicts]))))))

  (describe "missing-fact reference is rejected with a warning"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :f1] {:content "x" :born "t1/i1/f1"}))
          {warnings :warnings :as out}
          (eng/apply-mutator ctx "t1/i1/f2" :fact-contradicts! [:f1 :ghost])]
      (it "stamped? false"
        (expect (not (:stamped? out))))
      (it "emits :fact-contradicts-missing"
        (expect (some #(= :fact-contradicts-missing (:code %)) warnings))))))

(defdescribe fact-contradicts-remove-test
  (describe "fact-contradicts-remove! removes the link on both sides"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :f1] {:content "x" :born "t1/i1/f1"
                                                :contradicts #{:f2}})
                (assoc-in [:session/facts :f2] {:content "y" :born "t1/i1/f1"
                                                :contradicts #{:f1}}))
          {ctx' :ctx}
          (eng/apply-mutator ctx "t1/i2/f1" :fact-contradicts-remove! [:f1 :f2])]
      (it "drops :contradicts from the first fact"
        (expect (nil? (get-in ctx' [:session/facts :f1 :contradicts]))))
      (it "drops :contradicts from the second fact"
        (expect (nil? (get-in ctx' [:session/facts :f2 :contradicts]))))))

  (describe "remove is idempotent on a fresh ctx (no warning)"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :f1] {:content "x" :born "t1/i1/f1"})
                (assoc-in [:session/facts :f2] {:content "y" :born "t1/i1/f1"}))
          {warnings :warnings :as out}
          (eng/apply-mutator ctx "t1/i2/f1" :fact-contradicts-remove! [:f1 :f2])]
      (it "stamped? true"
        (expect (:stamped? out)))
      (it "emits no warnings"
        (expect (empty? warnings))))))

(defdescribe contradicting-facts-warning-test
  (describe "derive-warnings emits :contradicting-facts when both stay :active"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :f1] {:content "x" :status :active
                                                :born "t1/i1/f1"
                                                :contradicts #{:f2}})
                (assoc-in [:session/facts :f2] {:content "y" :status :active
                                                :born "t1/i1/f1"
                                                :contradicts #{:f1}}))
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      (it "fires exactly one warning per unordered pair"
        (let [hits (filter #(= :contradicting-facts (:code %)) warns)]
          (expect (= 1 (count hits)))))
      (it "warning anchors carry both fact ids"
        (let [w (first (filter #(= :contradicting-facts (:code %)) warns))]
          (expect (= 2 (count (:anchor w))))
          (expect (contains? (set (:anchor w)) :f1))
          (expect (contains? (set (:anchor w)) :f2))))))

  (describe "flipping one fact to :superseded silences the warning"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :f1] {:content "x" :status :active
                                                :born "t1/i1/f1"
                                                :contradicts #{:f2}})
                (assoc-in [:session/facts :f2] {:content "y" :status :superseded
                                                :born "t1/i1/f1"
                                                :contradicts #{:f1}}))
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      (it "no :contradicting-facts warning is emitted"
        (expect (not-any? #(= :contradicting-facts (:code %)) warns))))))

(defdescribe contradiction-is-not-transitive-test
  (describe "A↔B and B↔C does NOT imply A↔C"
    ;; Engine must NOT synthesize the closure. Each pair is its own
    ;; explicit declaration; the model can be wrong about which pairs
    ;; actually conflict, and silent transitive inference would force
    ;; false positives.
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :A] {:content "a" :status :active
                                               :born "t1/i1/f1"
                                               :contradicts #{:B}})
                (assoc-in [:session/facts :B] {:content "b" :status :active
                                               :born "t1/i1/f1"
                                               :contradicts #{:A :C}})
                (assoc-in [:session/facts :C] {:content "c" :status :active
                                               :born "t1/i1/f1"
                                               :contradicts #{:B}}))
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))
          hits (filter #(= :contradicting-facts (:code %)) warns)
          anchored-pairs (set (map (comp set :anchor) hits))]
      (it "two warnings emitted (A↔B, B↔C)"
        (expect (= 2 (count hits))))
      (it "A↔C is NOT emitted"
        (expect (not (contains? anchored-pairs #{:A :C})))))))

;; =============================================================================
;; Proof composition.
;;
;; Proof entries can carry `:proof-compose [s1 s2 …]` plus `:proof-rule
;; :and|:or`. The validator pass evaluates each sub-scope and combines the
;; results. Failed sub-scopes archive individually so the model can swap
;; only the broken evidence without rebuilding the whole composition.
;; =============================================================================

(defn- with-spec-req-validator [ctx spec-k req-id src]
  (-> ctx
    (assoc-in [:session/specs spec-k]
      {:title "s" :born "t1/i1/f1" :status :doing
       :requirements [{:id req-id :title "r" :validator-fn src}]})))

(defn- with-compose-proof [ctx task-k spec-k req-id scopes rule]
  (assoc-in ctx [:session/tasks task-k]
    {:title "t" :born "t1/i1/f1" :status :doing
     :specs {spec-k [(cond-> {:requirement req-id :proof-compose scopes}
                       rule (assoc :proof-rule rule))]}}))

(defdescribe proof-compose-and-test
  (describe ":and composition passes when EVERY sub-scope's validator passes"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc :session/scope {:turn 2 :iter 2 :next-form 1})
                (with-spec-req-validator :S :r1
                  "(fn [{:keys [result]}] (true? result))")
                (with-compose-proof :T :S :r1 ["t2/i1/f1" "t2/i1/f2"] :and))
          form-results {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation :result true}
                        "t2/i1/f2" {:scope "t2/i1/f2" :tag :observation :result true}}
          ctx' (eng/archive-failed-task-proofs ctx form-results)]
      (it "writes no archive entry — both passed"
        (expect (empty? (get-in ctx' [:session/tasks :T :archived-proofs]))))))

  (describe ":and composition archives ONLY the failed sub-scope"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc :session/scope {:turn 2 :iter 2 :next-form 1})
                (with-spec-req-validator :S :r1
                  "(fn [{:keys [result]}] (true? result))")
                (with-compose-proof :T :S :r1 ["t2/i1/f1" "t2/i1/f2"] :and))
          form-results {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation :result true}
                        "t2/i1/f2" {:scope "t2/i1/f2" :tag :observation :result false}}
          ctx' (eng/archive-failed-task-proofs ctx form-results)
          archive (get-in ctx' [:session/tasks :T :archived-proofs])]

      (it "archives exactly one entry (the rejected scope)"
        (expect (= 1 (count archive))))

      (it "captures the failing sub-scope, not the whole compose"
        (let [e (first archive)]
          (expect (= "t2/i1/f2" (:proof e)))
          (expect (= :r1 (:requirement e))))))))

(defdescribe proof-compose-or-test
  (describe ":or composition passes when AT LEAST ONE sub-scope passes"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc :session/scope {:turn 2 :iter 2 :next-form 1})
                (with-spec-req-validator :S :r1
                  "(fn [{:keys [result]}] (true? result))")
                (with-compose-proof :T :S :r1 ["t2/i1/f1" "t2/i1/f2"] :or))
          form-results {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation :result false}
                        "t2/i1/f2" {:scope "t2/i1/f2" :tag :observation :result true}}
          ctx' (eng/archive-failed-task-proofs ctx form-results)]
      (it "writes no archive entry — one passed"
        (expect (empty? (get-in ctx' [:session/tasks :T :archived-proofs]))))))

  (describe ":or composition archives ALL sub-scopes when none pass"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc :session/scope {:turn 2 :iter 2 :next-form 1})
                (with-spec-req-validator :S :r1
                  "(fn [{:keys [result]}] (true? result))")
                (with-compose-proof :T :S :r1 ["t2/i1/f1" "t2/i1/f2"] :or))
          form-results {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation :result false}
                        "t2/i1/f2" {:scope "t2/i1/f2" :tag :observation :result false}}
          ctx' (eng/archive-failed-task-proofs ctx form-results)
          archive (get-in ctx' [:session/tasks :T :archived-proofs])]
      (it "archives every sub-scope"
        (expect (= 2 (count archive))))
      (it "covers both failing scopes"
        (let [scopes (set (map :proof archive))]
          (expect (= #{"t2/i1/f1" "t2/i1/f2"} scopes)))))))

(defdescribe proof-compose-schema-test
  (describe "apply-proof-add! validates compose shape at write time"
    (let [base (-> (eng/empty-ctx "t")
                 (with-spec-req-validator :S :r1 "(fn [_] true)")
                 (assoc-in [:session/tasks :T]
                   {:title "t" :born "t1/i1/f1" :status :doing}))]
      (it "rejects empty :proof-compose with :proof-compose-empty"
        (let [{warnings :warnings}
              (eng/apply-mutator base "t1/i2/f1" :proof-add!
                [:T :S {:requirement :r1 :proof-compose []}])]
          (expect (some #(= :proof-compose-empty (:code %)) warnings))))

      (it "rejects non-string sub-scopes with :proof-compose-non-string"
        (let [{warnings :warnings}
              (eng/apply-mutator base "t1/i2/f1" :proof-add!
                [:T :S {:requirement :r1 :proof-compose [:not-a-string]}])]
          (expect (some #(= :proof-compose-non-string (:code %)) warnings))))

      (it "warns on unknown :proof-rule with :proof-rule-unknown"
        (let [{warnings :warnings}
              (eng/apply-mutator base "t1/i2/f1" :proof-add!
                [:T :S {:requirement :r1 :proof-compose ["t1/i1/f1"]
                        :proof-rule :xor}])]
          (expect (some #(= :proof-rule-unknown (:code %)) warnings)))))))

(defdescribe proof-compose-progression-test
  (describe "progression counts compose proofs as proven only when every sub-scope is reachable"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc :session/scope {:turn 2 :iter 2 :next-form 1})
                (with-spec-req-validator :S :r1 "(fn [_] true)")
                (with-compose-proof :T :S :r1 ["t2/i1/f1" "t2/i1/f2"] :and))
          ;; only one of the two compose scopes is in form-results
          partial-results {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation :result true}}
          full-results    {"t2/i1/f1" {:scope "t2/i1/f1" :tag :observation :result true}
                           "t2/i1/f2" {:scope "t2/i1/f2" :tag :observation :result true}}
          prog-partial (eng/derive-progression ctx (eng/build-indexes ctx) partial-results)
          prog-full    (eng/derive-progression ctx (eng/build-indexes ctx) full-results)]
      (it "partial form-results → spec still :open"
        (expect (= :open (get-in prog-partial [:S :state]))))
      (it "full form-results → spec :ready"
        (expect (= :ready (get-in prog-full [:S :state])))))))

;; =============================================================================
;; Phase L: introspect-changes — turn-to-turn delta over snapshots.
;;
;; Diff is a pure projection over the persisted snapshots
;; (session_turn_state.ctx). Returns per-entity change records:
;;   {:kind :spec|:task|:fact|:rule :K k :change <:added | :removed | vec>}
;; Each per-field change is `[field before after]`.
;; Trailer is intentionally excluded — model already reads it inline.
;; =============================================================================

(defdescribe diff-ctx-test
  (describe "diff-ctx returns per-entity change records"
    (let [before {:session/specs {:S {:title "switch to bcrypt" :status :doing
                                      :born "t1/i1/f1"}}
                  :session/tasks {:T {:title "impl" :status :todo
                                      :born "t1/i1/f1"}}
                  :session/facts {:F {:content "uses literal compare"
                                      :status :active :born "t1/i1/f1"}}}
          after  {:session/specs {:S {:title "switch to bcrypt" :status :done
                                      :born "t1/i1/f1" :done-born "t2/i1/f2"}
                                  :S2 {:title "new spec" :status :draft :born "t2/i1/f3"}}
                  :session/tasks {:T {:title "impl" :status :doing
                                      :born "t1/i1/f1"
                                      :archived-proofs [{:proof "t2/i1/f1"}]}}
                  :session/facts {:F {:content "uses bcrypt now"
                                      :status :active :born "t1/i1/f1"}}}
          diff (eng/diff-ctx before after)
          by-key (group-by (juxt :kind :K) diff)]

      (it "captures :added entities"
        (let [[entry] (get by-key [:spec :S2])]
          (expect (= :added (:change entry)))
          (expect (= "new spec" (get-in entry [:after :title])))))

      (it "captures :status transitions"
        (let [[entry] (get by-key [:spec :S])
              tuples  (:change entry)
              status  (some (fn [[f b a]] (when (= f :status) [b a])) tuples)]
          (expect (= [:doing :done] status))))

      (it "captures :done-born stamping"
        (let [[entry] (get by-key [:spec :S])
              tuples  (:change entry)]
          (expect (some #(= [:done-born nil "t2/i1/f2"] %) tuples))))

      (it "captures rejected-proofs growth as a :rejected-proofs delta"
        (let [[entry] (get by-key [:task :T])
              tuples  (:change entry)]
          (expect (some (fn [[f b a]]
                          (and (= f :rejected-proofs) (= b 0) (= a 1)))
                    tuples))))

      (it "captures fact :content changes"
        (let [[entry] (get by-key [:fact :F])
              tuples  (:change entry)
              ch (some (fn [[f b a]] (when (= f :content) [b a])) tuples)]
          (expect (= ["uses literal compare" "uses bcrypt now"] ch)))))))

(defdescribe introspect-changes-test
  (describe "introspect-changes reads snapshots N-1 and N and diffs them"
    (let [snap-1 {:session/specs {:S {:title "x" :status :doing :born "t1/i1/f1"}}}
          snap-2 {:session/specs {:S {:title "x" :status :done :born "t1/i1/f1"
                                      :done-born "t2/i1/f1"}}}
          history [[1 snap-1] [2 snap-2]]
          changes (eng/introspect-changes history "t2")]

      (it "returns a vec of change records"
        (expect (vector? changes))
        (expect (pos? (count changes))))

      (it "includes the :status flip from :doing to :done"
        (let [[entry] (filter #(and (= :spec (:kind %)) (= :S (:K %))) changes)
              tuples  (:change entry)]
          (expect (some (fn [[f b a]]
                          (and (= f :status) (= :doing b) (= :done a)))
                    tuples)))))

    (describe "returns nil when snapshot N or N-1 is missing"
      (let [history [[1 {:session/specs {}}]]]
        (it "nil when N doesn't exist"
          (expect (nil? (eng/introspect-changes history "t5"))))
        (it "nil when N-1 doesn't exist (e.g. turn 1)"
          (expect (nil? (eng/introspect-changes history "t1"))))))))
