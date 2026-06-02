(ns com.blockether.vis.internal.ctx-engine-test
  "Pure-fn tests + REPL-replayable scenarios for the CTX engine.

   The engine is a typed, dependency-checked working memory of tasks +
   facts. Done is self-asserted; there is no proof / validator / spec /
   stage machinery. These tests exercise the surviving surface:

     • parse / classify-scope helpers
     • build-indexes (dep-graph / rev-deps / task-status / fact-status)
     • depends-on cycle detection + hard reject at write time
     • fact contradictions + derive-warnings (vec of short strings)
     • task / fact CRUD via apply-mutator (slimmed shapes)
     • advance-iter trailer behaviour + hook-task lifetimes
     • form tag classification + blocks->forms projection"
  ;; rewind/lens/grep recovery surface is tested in `ctx-loop-test`.
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
;; build-indexes — shape + cross-references (tasks + facts only)
;; =============================================================================

(def ^:private mini-ctx
  "Minimal but realistic CTX for index assertions."
  {:session/id    "test"
   :session/turn  3
   :session/scope {:turn 3 :iter 1 :next-form 1}
   :session/workspace
   {:workspace/root "/repo" :workspace/sandbox? false
    :vcs/ref "main" :vcs/mainline "main" :vcs/head "x"
    :vcs/dirty? false :vcs/stats {}}
   :session/symbols {}
   :session/tasks
   {:t1 {:title "task 1"
         :status :done
         :born "t1/i1/f2"
         :done-born "t2/i1/f2"}
    :t2 {:title "task 2"
         :depends-on [:t1]
         :status :todo
         :born "t1/i1/f3"}}
   :session/facts
   {:f1 {:content "fact one" :born "t1/i1/f4"}}
   :session/trailer []})

(defdescribe build-indexes-test
  (describe "build-indexes shape"
    (let [idx (eng/build-indexes mini-ctx)]
      (it "returns exactly the four expected keys"
        (expect (= #{:dep-graph :rev-deps :task-status :fact-status}
                  (set (keys idx)))))

      (it ":dep-graph captures :depends-on edges as typed refs"
        (expect (= #{[:task :t1]} (get-in idx [:dep-graph [:task :t2]])))
        (expect (= #{} (get-in idx [:dep-graph [:task :t1]]))))

      (it ":rev-deps reverses the typed graph"
        (expect (= #{[:task :t2]} (get-in idx [:rev-deps [:task :t1]]))))

      (it ":task-status maps task-id → status"
        (expect (= :done (get-in idx [:task-status :t1])))
        (expect (= :todo (get-in idx [:task-status :t2]))))

      (it ":fact-status defaults to :active when omitted"
        (expect (= :active (get-in idx [:fact-status :f1]))))))

  (describe "build-indexes is pure"
    (it "produces identical output on repeated calls"
      (expect (= (eng/build-indexes mini-ctx)
                (eng/build-indexes mini-ctx))))

    (it "tolerates empty subtrees"
      (let [empty-ctx (-> mini-ctx
                        (assoc :session/tasks {} :session/facts {}))]
        (expect (= {} (:dep-graph (eng/build-indexes empty-ctx))))
        (expect (= {} (:task-status (eng/build-indexes empty-ctx))))
        (expect (= {} (:fact-status (eng/build-indexes empty-ctx))))))))

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
        (expect (every? #(map? (:dep-graph %)) idxs))
        (expect (every? #(map? (:rev-deps %)) idxs))
        (expect (every? #(map? (:task-status %)) idxs))
        (expect (every? #(map? (:fact-status %)) idxs))))))

;; =============================================================================
;; Form tag classification + blocks→forms projection
;; =============================================================================

(defdescribe classify-form-tag-test
  (describe "classify-form-tag"
    (it ":mutation for engine-owned mutators"
      (expect (= :mutation (eng/classify-form-tag "(task-set! :A {})")))
      (expect (= :mutation (eng/classify-form-tag "(fact-set! :F {:content \"y\"})")))
      (expect (= :mutation (eng/classify-form-tag "(task-depends! :A [:b])")))
      (expect (= :mutation (eng/classify-form-tag "(fact-contradicts! :f1 :f2)"))))

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
      (expect (= :observation (eng/classify-form-tag "(lens \"t1/i1/f1\")")))
      (expect (= :observation (eng/classify-form-tag "(rewind \"t1/i1/f1\")")))
      (expect (= :observation (eng/classify-form-tag "(find {:match \"x\"})")))
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
                (eng/classify-form-tag "(task-set! :K {:title \"x\"})"
                  (fn [_] :observation))))
      (expect (= :mutation
                (eng/classify-form-tag "(task-set! :K {:title \"x\"})"))))))

(defdescribe advance-iter-trailer-test
  (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
              :session/trailer []}
        obs1 [{:scope "t1/i1/f1" :tag :observation :src "(v/cat \"a\")" :result "old"}]
        obs2 [{:scope "t1/i2/f1" :tag :observation :src "(v/cat \"b\")" :result "new"}]
        mut3 [{:scope "t1/i3/f1" :tag :mutation :src "(v/patch [])" :result []}]]

    (it "carries observation-only pins forward"
      (let [ctx1 (eng/advance-iter base obs1)
            ctx2 (eng/advance-iter ctx1 obs2)]
        (expect (= ["t1/i1" "t1/i2"]
                  (mapv :scope (:session/trailer ctx2))))))

    (it "KEEPS observation pins even after a later mutation (no auto-prune)"
      ;; The old `prune-stale-observation-pins` (drop all obs pins on
      ;; first mutation) was removed — it forced needless re-reads (read
      ;; file A, patch unrelated B, lose A). Pins now carry forward;
      ;; size is bounded by the engine's size-triggered auto-summarize
      ;; and the model's explicit `(done {:summarize …})`.
      (let [ctx1 (eng/advance-iter base obs1)
            ctx2 (eng/advance-iter ctx1 obs2)
            ctx3 (eng/advance-iter ctx2 mut3)]
        (expect (= ["t1/i1" "t1/i2" "t1/i3"]
                  (mapv :scope (:session/trailer ctx3))))))

    (it "keeps earlier pins even when the model rebinds the same def"
      ;; Rebinding `(def persist …)` across iterations is intentional model
      ;; behaviour: each attempt refines tool arguments. Trailer pins from
      ;; prior attempts MUST stay so the model can see what it tried and
      ;; switch tactic. Collapsing prior pins on rebind hides the loop
      ;; signal and lets the model run rg 20 times without realising.
      ;; `(done {:summarize {:trailer …}})` is the only contract for trimming.
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
  (it "flags a rebind-loop warning once the same def is rebound 3+ times"
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
      ;; derive-warnings returns a vec of short strings now.
      (expect (some #(re-find #"def persist" %) warns))))

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
      (expect (not-any? #(re-find #"rebound" %) warns)))))

;; ---------------------------------------------------------------------------
;; trailer pin filter for :vis/silent results.
;; ---------------------------------------------------------------------------

(defdescribe advance-iter-silent-result-filter-test
  (describe "forms whose :result is :vis/silent are NOT pinned"
    (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
                :session/trailer []}
          mixed [{:scope "t1/i1/f1" :tag :mutation
                  :src "(task-set! :K {:title \"x\"})"
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
                       :src "(task-set! :K {:title \"x\"})"
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
                  :src "(fact-set! :K {})"
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

  (it "carries duration from the loop block envelope for restored TUI footers"
    (let [env (eng/block->envelope {:code "(v/patch [])"
                                    :result :ok
                                    :envelope {:started-at-ms 1000
                                               :finished-at-ms 1012}}
                1 {:turn 24 :iter 1})]
      (expect (= "t24/i1/f1" (:scope env)))
      (expect (= 12 (:duration-ms env)))))

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
          blocks [{:code "(task-set! :K {:title \"x\"})" :result :ok}
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
        (expect (= "(task-set! :K {:title \"x\"})" (:src (first forms)))))

      (it "empty input returns empty vec"
        (expect (= [] (eng/blocks->forms [] cursor)))))))

(defdescribe gc-pass-turn-lifetime-test
  ;; Vis conv 11d4f817 / t14–t16: a transient context-pressure
  ;; hook-task lingered for 6 turns after the originating hint had
  ;; stopped firing. Hooks now opt in via `:lifetime :turn` and
  ;; `gc-pass` drops those entries at advance-turn so the next iter
  ;; starts clean — re-created on demand by the hook only when the
  ;; condition still holds.
  (describe "gc-pass drops :lifetime :turn hook-tasks regardless of status"
    (it "drops an open :lifetime :turn hook-task at advance-turn"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/context-pressure
                                 {:title "warn" :source :hook
                                  :hook-id :vis.foundation/context-pressure
                                  :status :todo :lifetime :turn
                                  :born "t1/i1/f1"}}}]
        (expect (= {} (:session/tasks (eng/enter-turn ctx 2))))))

    (it "drops a :done :lifetime :turn task immediately"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/context-pressure
                                 {:title "warn" :source :hook
                                  :hook-id :vis.foundation/context-pressure
                                  :status :done
                                  :lifetime :turn
                                  :done-born "t1/i1/f2"
                                  :born "t1/i1/f1"}}}]
        (expect (= {} (:session/tasks (eng/enter-turn ctx 2))))))

    (it "keeps :lifetime :session hook-tasks until the TTL kicks in"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/session-title
                                 {:title "set title" :source :hook
                                  :hook-id :vis.foundation/session-title
                                  :status :todo
                                  :born "t1/i1/f1"}}}
            advanced (eng/enter-turn ctx 2)]
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
        (expect (contains? (:session/tasks (eng/enter-turn ctx 2)) :user.thing))))))

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
                                  :born "t1/i1/f1"}}}]
        (expect (= {} (:session/tasks (eng/advance-iter ctx []))))))

    (it "drops a :done :lifetime :iteration task immediately"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/retry-shape
                                 {:title "retry" :source :hook
                                  :hook-id :vis.foundation/retry-shape
                                  :status :done
                                  :lifetime :iteration
                                  :done-born "t1/i1/f2"
                                  :born "t1/i1/f1"}}}]
        (expect (= {} (:session/tasks (eng/advance-iter ctx []))))))

    (it "keeps :lifetime :turn hook-tasks across advance-iter (turn boundary owns them)"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {:vis.foundation/context-pressure
                                 {:title "warn" :source :hook
                                  :hook-id :vis.foundation/context-pressure
                                  :status :todo :lifetime :turn
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
                                  :born "t1/i2/f1"}}}
            advanced (eng/advance-iter ctx
                       [{:scope "t1/i2/f1" :src "(v/cat \"a.clj\")"
                         :tag :observation :result "ok"}])]
        (expect (= 3 (:iter (:session/scope advanced))))
        (expect (= 1 (:next-form (:session/scope advanced))))
        (expect (= 1 (count (:session/trailer advanced))))
        (expect (= {} (:session/tasks advanced)))))))

;; =============================================================================
;; Universal :depends-on across task/fact entities.
;;
;; `:depends-on` is a first-class relation on tasks and facts. Refs are
;; either bare keys (same-kind shorthand) or typed `[:kind :K]` pairs.
;; Cycle detection is unified across the two subtrees so a chain
;; `task -> fact -> task` is hard-rejected at write time.
;; =============================================================================

(defdescribe universal-depends-on-test
  (describe "task-set! / fact-set! accept :depends-on"
    (it "fact :depends-on is preserved through apply-fact-set!"
      (let [{ctx :ctx}
            (eng/apply-mutator (eng/empty-ctx "t") "t1/i1/f1"
              :fact-set! [:K {:content "x" :depends-on [[:task :impl]]}])]
        (expect (= [[:task :impl]]
                  (get-in ctx [:session/facts :K :depends-on])))))

    (it "task-depends! / fact-depends! all write through"
      (let [base (-> (eng/empty-ctx "t")
                   (assoc-in [:session/tasks :T] {:title "t" :born "t1/i1/f1"})
                   (assoc-in [:session/facts :F] {:content "f" :born "t1/i1/f1"}))
            after-t (:ctx (eng/apply-mutator base "t1/i1/f1" :task-depends! [:T [[:fact :F]]]))
            after-f (:ctx (eng/apply-mutator after-t "t1/i1/f1" :fact-depends! [:F []]))]
        (expect (= [[:fact :F]] (get-in after-f [:session/tasks :T :depends-on])))
        (expect (= []           (get-in after-f [:session/facts :F :depends-on])))))))

(defdescribe cross-entity-cycle-rejection-test
  (describe "task→fact→task cycle is hard-rejected at write time"
    ;; Seed a chain that's already valid up to fact :F, then attempt to close
    ;; the loop with a fact-depends! that points back to task :T. The engine
    ;; must refuse the write and emit :depends-on-cycle.
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/tasks :T]
                  {:title "t" :born "t1/i1/f1" :depends-on [[:fact :F]]})
                (assoc-in [:session/facts :F]
                  {:content "f" :born "t1/i1/f1"}))
          {ctx' :ctx warnings :warnings :as out}
          (eng/apply-mutator ctx "t1/i2/f1" :fact-depends! [:F [[:task :T]]])]

      (it "refuses the write (no :depends-on on :F)"
        (expect (not (:stamped? out)))
        (expect (nil? (get-in ctx' [:session/facts :F :depends-on]))))

      (it "emits a :depends-on-cycle warning"
        (expect (some #(= :depends-on-cycle (:code %)) warnings))))))

(defdescribe build-indexes-dep-graph-typed-test
  (describe "build-indexes derives the typed :dep-graph across task + fact kinds"
    ;; Canonical shape: :dep-graph keys are typed `[:kind :K]` refs.
    ;; Bare-key entries on `:depends-on` are normalized to same-kind
    ;; typed refs at index-build time.
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/tasks :T]
                  {:title "t" :born "t1/i1/f1" :depends-on [[:fact :F]]})
                (assoc-in [:session/facts :F]
                  {:content "f" :born "t1/i1/f1"}))
          idx (eng/build-indexes ctx)]

      (it "indexes the typed dep-graph with each node present"
        (let [g (:dep-graph idx)]
          (expect (contains? g [:task :T]))
          (expect (contains? g [:fact :F]))))

      (it "edges normalize bare and typed refs uniformly"
        (let [g (:dep-graph idx)]
          (expect (= #{[:fact :F]} (get g [:task :T])))))

      (it "empty-deps entity still appears as a node with an empty edge set"
        (let [g (:dep-graph idx)]
          (expect (= #{} (get g [:fact :F]))))))))

(defdescribe depends-on-dangling-warning-test
  (describe "typed :depends-on refs that point at nonexistent entities surface as warnings"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/tasks :T]
                  {:title "t" :born "t1/i1/f1" :depends-on [[:fact :ghost]]}))
          idx (eng/build-indexes ctx)
          warns (eng/derive-warnings ctx idx)]
      (it "emits a dangling-dep warning naming the missing kind+key"
        ;; derive-warnings is a vec of short strings now.
        (expect (some #(and (re-find #"ghost" %)
                         (re-find #"depends-on" %))
                  warns))))))

;; =============================================================================
;; Task done semantics — self-asserted, no validator, no reversion
;; =============================================================================

(defdescribe task-done-self-asserted-test
  (describe "task-set! :status :done is accepted as-is and stamps :done-born"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc :session/scope {:turn 2 :iter 1 :next-form 1})
                (assoc-in [:session/tasks :T]
                  {:title "impl" :status :doing :born "t1/i1/f1"}))
          {ctx' :ctx :as out}
          (eng/apply-mutator ctx "t2/i1/f1" :task-set! [:T {:status :done}])]

      (it "the write is stamped"
        (expect (:stamped? out)))

      (it ":status is :done"
        (expect (= :done (get-in ctx' [:session/tasks :T :status]))))

      (it "engine stamps :done-born at the form scope"
        (expect (= "t2/i1/f1" (get-in ctx' [:session/tasks :T :done-born]))))

      (it "no warnings — done is self-asserted, never re-run"
        (expect (empty? (:warnings out))))))

  (describe "flipping a :done task back to a non-terminal status clears :done-born"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc :session/scope {:turn 2 :iter 1 :next-form 1})
                (assoc-in [:session/tasks :T]
                  {:title "impl" :status :done :born "t1/i1/f1"
                   :done-born "t1/i2/f1"}))
          {ctx' :ctx}
          (eng/apply-mutator ctx "t2/i1/f1" :task-set! [:T {:status :doing}])]
      (it ":done-born cleared on the transition"
        (expect (nil? (get-in ctx' [:session/tasks :T :done-born])))))))

(defdescribe task-done-pending-dep-warning-test
  (describe "task :done while a dep is non-terminal surfaces a soft warning"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/tasks :a] {:title "dep" :status :doing
                                               :born "t1/i1/f1"})
                (assoc-in [:session/tasks :b] {:title "main" :status :done
                                               :done-born "t1/i2/f1"
                                               :depends-on [:a]
                                               :born "t1/i1/f2"}))
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      (it "emits a short warning string naming the task + non-terminal dep"
        ;; e.g. "task :b :done but dep :a is :doing"
        (expect (some #(and (re-find #":b" %) (re-find #":a" %)
                         (re-find #"done" %))
                  warns)))))

  (describe "no warning when the dep is terminal"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/tasks :a] {:title "dep" :status :done
                                               :done-born "t1/i1/f1"
                                               :born "t1/i1/f1"})
                (assoc-in [:session/tasks :b] {:title "main" :status :done
                                               :done-born "t1/i2/f1"
                                               :depends-on [:a]
                                               :born "t1/i1/f2"}))
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      (it "stays quiet"
        (expect (not-any? #(re-find #"but dep" %) warns))))))

;; =============================================================================
;; Contradiction detection on facts.
;;
;; `(fact-contradicts! :K1 :K2)` declares a SYMMETRIC, NON-TRANSITIVE link
;; between two facts. The engine writes `:contradicts` on both sides so the
;; warning surfaces regardless of read direction. When both facts stay
;; `:active`, `derive-warnings` emits a contradiction string. Resolving a
;; contradiction is the model's job (flip one to `:superseded`).
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
  (describe "derive-warnings emits a contradiction string when both stay :active"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :f1] {:content "x" :status :active
                                                :born "t1/i1/f1"
                                                :contradicts #{:f2}})
                (assoc-in [:session/facts :f2] {:content "y" :status :active
                                                :born "t1/i1/f1"
                                                :contradicts #{:f1}}))
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      (it "fires exactly one warning per unordered pair"
        (let [hits (filter #(re-find #"↔" %) warns)]
          (expect (= 1 (count hits)))))
      (it "warning string names both fact ids"
        (let [w (first (filter #(re-find #"↔" %) warns))]
          (expect (re-find #":f1" w))
          (expect (re-find #":f2" w))))))

  (describe "flipping one fact to :superseded silences the warning"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts :f1] {:content "x" :status :active
                                                :born "t1/i1/f1"
                                                :contradicts #{:f2}})
                (assoc-in [:session/facts :f2] {:content "y" :status :superseded
                                                :born "t1/i1/f1"
                                                :contradicts #{:f1}}))
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      (it "no contradiction warning is emitted"
        (expect (not-any? #(re-find #"↔" %) warns))))))

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
          hits (filter #(re-find #"↔" %) warns)]
      (it "two warnings emitted (A↔B, B↔C)"
        (expect (= 2 (count hits))))
      (it "A↔C is NOT emitted"
        (expect (not-any? #(and (re-find #":A" %) (re-find #":C" %)
                             (not (re-find #":B" %)))
                  hits))))))

