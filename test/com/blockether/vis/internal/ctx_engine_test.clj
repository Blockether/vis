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
        (expect (= [{:task :t1 :scope "t2/i1/f1"}]
                  (get-in idx [:proof-index [:rl :r1]])))
        (expect (nil? (get-in idx [:proof-index [:rl :r2]]))))

      (it ":task-by-spec maps spec-id → set of task-ids"
        (expect (= #{:t1 :t2} (get-in idx [:task-by-spec :rl]))))

      (it ":fact-refs is the reverse of requirement :facts"
        (expect (= #{:r1} (get-in idx [:fact-refs :f1]))))

      (it ":dep-graph captures :depends-on edges"
        (expect (= #{:t1} (get-in idx [:dep-graph :t2])))
        (expect (= #{} (get-in idx [:dep-graph :t1]))))

      (it ":rev-deps reverses the graph"
        (expect (= #{:t2} (get-in idx [:rev-deps :t1]))))

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

    (it ":mutation for foundation filesystem write tools"
      (expect (= :mutation (eng/classify-form-tag "(v/patch [{:path \"x\" :search \"a\" :replace \"b\"}])")))
      (expect (= :mutation (eng/classify-form-tag "(v/write {:path \"x\" :content \"b\"})"))))))

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
