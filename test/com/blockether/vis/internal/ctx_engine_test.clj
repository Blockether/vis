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

(defdescribe subtree-of-test
  ;; The focus slice a sub_loop child gets (exposed to the sandbox as
  ;; `task_subtree`): the root task + its transitive descendants, in order.
  (let [tasks (array-map "root"  {:parent nil    :status :doing}
                "a"     {:parent "root" :status :todo}
                "a1"    {:parent "a"    :status :todo}
                "b"     {:parent "root" :status :todo}
                "other" {:parent nil    :status :todo})]
    (it "root → the whole tree under it, excluding unrelated roots"
      (expect (= ["root" "a" "a1" "b"] (vec (keys (eng/subtree-of tasks "root")))))
      (expect (not (contains? (eng/subtree-of tasks "root") "other"))))
    (it "a mid-node → that node + its descendants only (no parent, no siblings)"
      (expect (= ["a" "a1"] (vec (keys (eng/subtree-of tasks "a")))))
      (expect (= ["b"] (vec (keys (eng/subtree-of tasks "b"))))))
    (it "a leaf → just itself; an absent root → empty"
      (expect (= ["a1"] (vec (keys (eng/subtree-of tasks "a1")))))
      (expect (= {} (eng/subtree-of tasks "nope"))))
    (it "preserves the entry maps verbatim (it's a slice, not a rewrite)"
      (expect (= {:parent "root" :status :todo} (get (eng/subtree-of tasks "a") "a"))))
    (it "is FRACTAL — re-slicing a slice (subtree of a subtree) gives the deeper subtree"
      ;; A sub_loop child seeded with task_subtree(\"a\") can itself
      ;; task_subtree(\"a1\") for a grandchild — focus IS the subroot at each level.
      (let [child-slice (eng/subtree-of tasks "a")]           ; {a, a1}
        (expect (= ["a1"] (vec (keys (eng/subtree-of child-slice "a1")))))))
    (it "a slice root keeps its (now dangling) :parent yet renders as a ROOT"
      ;; task-tree-walk treats \"parent not in the map\" as a root, so the focus
      ;; node is depth 0 at every level even though its original parent is gone.
      (let [slice (eng/subtree-of tasks "a")]                 ; a(:parent root), a1
        (expect (= "root" (get-in slice ["a" :parent])))      ; dangling parent kept
        (expect (= [["a" 0] ["a1" 1]]
                  (mapv (juxt :key :depth) (eng/task-tree-walk slice))))))))

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
   {"t1" {:title "task 1"
          :status :done
          :born "t1/i1/f2"
          :done-born "t2/i1/f2"}
    "t2" {:title "task 2"
          :depends_on [[:task "t1"]]
          :status :todo
          :born "t1/i1/f3"}}
   :session/facts
   {"f1" {:content "fact one" :born "t1/i1/f4"}}
   :session/trailer []})

(defdescribe build-indexes-test
  (describe "build-indexes shape"
    (let [idx (eng/build-indexes mini-ctx)]
      (it "returns exactly the four expected keys"
        (expect (= #{:dep-graph :rev-deps :task-status :fact-status}
                  (set (keys idx)))))

      (it ":dep-graph captures :depends_on edges as typed refs"
        (expect (= #{[:task "t1"]} (get-in idx [:dep-graph [:task "t2"]])))
        (expect (= #{} (get-in idx [:dep-graph [:task "t1"]]))))

      (it ":rev-deps reverses the typed graph"
        (expect (= #{[:task "t2"]} (get-in idx [:rev-deps [:task "t1"]]))))

      (it ":task-status maps task-id → status"
        (expect (= :done (get-in idx [:task-status "t1"])))
        (expect (= :todo (get-in idx [:task-status "t2"]))))

      (it ":fact-status defaults to :active when omitted"
        (expect (= :active (get-in idx [:fact-status "f1"]))))))

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
;; blocks→forms projection
;; =============================================================================

(defdescribe advance-iter-trailer-test
  (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
              :session/trailer []}
        obs1 [{:scope "t1/i1/f1" :src "cat(\"a\")" :result "old"}]
        obs2 [{:scope "t1/i2/f1" :src "(cat \"b\")" :result "new"}]
        mut3 [{:scope "t1/i3/f1" :src "(patch [])" :result []}]]

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
      ;; and the model's explicit `(summarize …)`.
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
      ;; `(summarize {:trailer …})` is the only contract for trimming.
      (let [step1 (eng/advance-iter base
                    [{:scope "t1/i1/f1"
                      :src "(def persist (rg {:any [\"a\"]}))"}])
            step2 (eng/advance-iter step1
                    [{:scope "t1/i2/f1"
                      :src "(def persist (rg {:any [\"b\"]}))"}])
            step3 (eng/advance-iter step2
                    [{:scope "t1/i3/f1"
                      :src "(def other 1)"}])]
        (expect (= ["t1/i1" "t1/i2" "t1/i3"]
                  (mapv :scope (:session/trailer step3))))))))

(defdescribe rebind-loop-warning-test
  (it "flags a rebind-loop warning once the same def is rebound 3+ times"
    (let [trailer [{:scope "t1/i1"
                    :forms [{:src "(def persist (rg {:any [\"a\"]}))"
                             :scope "t1/i1/f1"}]}
                   {:scope "t1/i2"
                    :forms [{:src "(def persist (rg {:any [\"b\"]}))"
                             :scope "t1/i2/f1"}]}
                   {:scope "t1/i3"
                    :forms [{:src "(def persist (rg {:any [\"c\"]}))"
                             :scope "t1/i3/f1"}]}]
          ctx {:session/turn 1
               :session/scope {:turn 1 :iter 4 :next-form 1}
               :session/trailer trailer}
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      ;; derive-warnings returns a vec of short strings now.
      (expect (some #(re-find #"def persist" %) warns))))

  (it "stays quiet when a different def lands between rebinds"
    (let [trailer [{:scope "t1/i1"
                    :forms [{:src "(def persist 1)" :scope "t1/i1/f1"}]}
                   {:scope "t1/i2"
                    :forms [{:src "(def other 2)" :scope "t1/i2/f1"}]}
                   {:scope "t1/i3"
                    :forms [{:src "(def persist 3)" :scope "t1/i3/f1"}]}]
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
          mixed [{:scope "t1/i1/f1"
                  :src "task_set(\"K\", {\"title\": \"x\"})"
                  :result "vis_silent"}
                 {:scope "t1/i1/f2"
                  :src "cat(\"a\")"
                  :result "old"}
                 {:scope "t1/i1/f3"
                  :src "fact_set(\"baseline\", {\"content\": \"x\"})"
                  :result "vis_silent"}]
          ctx (eng/advance-iter base mixed)
          pin (first (:session/trailer ctx))]

      (it "trailer pin exists for the iter"
        (expect (= "t1/i1" (:scope pin))))

      (it "only the non-silent cat form is kept under :forms"
        (expect (= 1 (count (:forms pin))))
        (expect (= "cat(\"a\")" (:src (first (:forms pin))))))))

  (describe "an iter with ONLY :vis/silent forms produces no pin"
    (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
                :session/trailer []}
          all-silent [{:scope "t1/i1/f1"
                       :src "task_set(\"K\", {\"title\": \"x\"})"
                       :result "vis_silent"}
                      {:scope "t1/i1/f2"
                       :src "task_set(\"T\", {\"title\": \"x\"})"
                       :result "vis_silent"}]
          ctx (eng/advance-iter base all-silent)]

      (it ":session/trailer stays empty"
        (expect (empty? (:session/trailer ctx))))))

  (describe ":vis/silent flag in the envelope (loop's wrapped shape) also filters"
    (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
                :session/trailer []}
          forms [{:scope "t1/i1/f1"
                  :src "fact_set(\"K\", {})"
                  :vis/silent true
                  :result "vis_silent"}
                 {:scope "t1/i1/f2"
                  :src "cat(\"a\")"
                  :result "..."}]
          ctx (eng/advance-iter base forms)
          pin (first (:session/trailer ctx))]

      (it "only cat lands in :forms"
        (expect (= 1 (count (:forms pin)))))))

  (describe "(done …) still excluded even when its :result is not :vis/silent"
    (let [base {:session/scope {:turn 1 :iter 1 :next-form 1}
                :session/trailer []}
          forms [{:scope "t1/i1/f1"
                  :src "cat(\"a\")"
                  :result "..."}
                 {:scope "t1/i1/f2"
                  :src "done(\"x\")"
                  :result "vis_answer"}]
          ctx (eng/advance-iter base forms)
          pin (first (:session/trailer ctx))]

      (it "only cat lands in :forms (done excluded)"
        (expect (= 1 (count (:forms pin))))
        (expect (= "cat(\"a\")" (:src (first (:forms pin)))))))))

(defdescribe block-envelope-def-deref-test
  (it "derefs the Var returned by `(def NAME …)` so the trailer carries the bound value"
    ;; Regression: model writes `(def persist (rg …))`, SCI returns the
    ;; Var, trailer shows `{:vis/ref :expr}` (or `#'sandbox/persist`).
    ;; Model then re-emits `persist` to inspect, wasting an iter.
    ;; block->envelope now derefs IDeref results for def-shaped sources.
    (let [boxed (atom {:files ["a.clj"] :count 1})
          env (eng/block->envelope {:code "update_plan([{\"title\": \"persist\"}])"
                                    :result boxed}
                1 {:turn 3 :iter 4})]
      (expect (= {:files ["a.clj"] :count 1} (:result env)))
      (expect (not (contains? env :tag)))))

  (it "leaves non-def results untouched"
    (let [env (eng/block->envelope {:code "1 + 2" :result 3}
                1 {:turn 1 :iter 1})]
      (expect (= 3 (:result env)))))

  (it "carries duration from the loop block envelope for restored TUI footers"
    (let [env (eng/block->envelope {:code "(patch [])"
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
    ;;   model wrote `(def r (ls \".\"))` per the engine contract
    ;;   (\"bind values to defs\"). SCI's def unwrapped the tool envelope
    ;;   to its inner :result, so the persisted block's :result is a
    ;;   plain `{:op :ls …}` map without `:success?`. The TUI's
    ;;   `render-tool-result` then refused to dispatch to the ls
    ;;   renderer (envelope guard) and the bubble showed plain EDN —
    ;;   no widget, no badge. The pre-rendered IR already lives on
    ;;   each per-form `:channel` sink entry; block->envelope must
    ;;   carry that vec through so persistence + replay can paint the
    ;;   badge from the sink.
    (let [channel [{:position 1 :form "(ls \".\")"
                    :success? true
                    :result [:ir {} [:strong {} "ls"] [:p {} ". (844)"]]
                    :error nil}]
          env (eng/block->envelope
                {:code "update_plan([{\"title\": ls(\".\")}])"
                 :result {:op :ls :path "." :entry-count 844}
                 :channel channel}
                1 {:turn 2 :iter 1})]
      (expect (= (vec channel) (:channel env)))
      (expect (= "t2/i1/f1" (:scope env)))
      (expect (not (contains? env :tag)))))

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
          blocks [{:code "update_plan([{\"title\": \"x\"}])" :result :ok}
                  {:code "cat(\"a.clj\")"          :result "(ns a) ..."}
                  {:code "1 / 0" :error {:message "Divide by zero"}}]
          forms (eng/blocks->forms blocks cursor)]

      (it "preserves block order (1-based scope :form)"
        (expect (= ["t5/i2/f1" "t5/i2/f2" "t5/i2/f3"] (mapv :scope forms))))

      (it "carries :result when present"
        (expect (= :ok (:result (first forms))))
        (expect (= "(ns a) ..." (:result (second forms)))))

      (it "carries :error when present, omits :result on error"
        (expect (= {:message "Divide by zero"} (:error (nth forms 2))))
        (expect (not (contains? (nth forms 2) :result))))

      (it "every envelope has :src from block :code"
        (expect (= "update_plan([{\"title\": \"x\"}])" (:src (first forms)))))

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
  ;; `:lifetime :iteration` hook/policy projections evaporate at every
  ;; iter boundary, not just turn boundary. Use for hyper-transient
  ;; signals and provider-owned projections whose value disappears the
  ;; moment the iter that emitted them ends. The next iter's hook fire
  ;; is the single source of truth.
  (describe "advance-iter drops :lifetime :iteration projected tasks"
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

    (it "drops an open :lifetime :iteration policy task at advance-iter"
      (let [ctx {:session/turn 1
                 :session/scope {:turn 1 :iter 1 :next-form 1}
                 :session/tasks {"policy.obligation.foundation-bridge.unit-tests"
                                 {:title "Policy obligation: unit-tests"
                                  :source :policy
                                  :status :todo
                                  :lifetime :iteration
                                  :policy/provider "foundation-bridge"
                                  :born "t1/i1/f1"}}}]
        (expect (= {} (:session/tasks (eng/advance-iter ctx []))))))

    (it "ignores :lifetime :iteration on non-projected tasks"
      ;; Defensive: only hook/policy projected tasks are iter-pruned. A
      ;; model- or user-written task that happens to carry the keyword
      ;; keeps its TTL-based lifetime.
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
                       [{:scope "t1/i2/f1" :src "(cat \"a.clj\")"
                         :result "ok"}])]
        (expect (= 3 (:iter (:session/scope advanced))))
        (expect (= 1 (:next-form (:session/scope advanced))))
        (expect (= 1 (count (:session/trailer advanced))))
        (expect (= {} (:session/tasks advanced)))))))

;; =============================================================================
;; Universal :depends_on across task/fact entities.
;;
;; `:depends_on` is a first-class relation on tasks and facts. Refs are
;; either bare keys (same-kind shorthand) or typed `[:kind :K]` pairs.
;; Cycle detection is unified across the two subtrees so a chain
;; `task -> fact -> task` is hard-rejected at write time.
;; =============================================================================

(defdescribe universal-depends-on-test
  (describe "task-set! / fact-set! accept :depends_on"
    (it "fact :depends_on is preserved through apply-fact-set!"
      (let [{ctx :ctx}
            (eng/apply-mutator (eng/empty-ctx "t") "t1/i1/f1"
              :fact-set! ["K" {:content "x" :depends_on [[:task "impl"]]}])]
        (expect (= [[:task "impl"]]
                  (get-in ctx [:session/facts "K" :depends_on])))))

    (it "fact_set {:depends_on} REPLACES the edge vec (the one fact verb owns the relation)"
      (let [base (-> (eng/empty-ctx "t")
                   (assoc-in [:session/facts "F"] {:content "f" :born "t1/i1/f1"})
                   (assoc-in [:session/facts "G"] {:content "g" :born "t1/i1/f1"}))
            after (:ctx (eng/apply-mutator base "t1/i1/f1" :fact-set! ["F" {:depends_on [[:fact "G"]]}]))
            after2 (:ctx (eng/apply-mutator after "t1/i1/f1" :fact-set! ["G" {:depends_on []}]))]
        (expect (= [[:fact "G"]] (get-in after2 [:session/facts "F" :depends_on])))
        (expect (= []           (get-in after2 [:session/facts "G" :depends_on])))))))

(defdescribe cross-entity-cycle-rejection-test
  (describe "task→fact→task cycle is hard-rejected at write time"
    ;; Seed a chain that's already valid up to fact :F, then attempt to close
    ;; the loop with a fact_set {:depends_on} that points back to task :T. The
    ;; engine must refuse the write and emit :depends_on_cycle.
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/tasks "T"]
                  {:title "t" :born "t1/i1/f1" :depends_on [[:fact "F"]]})
                (assoc-in [:session/facts "F"]
                  {:content "f" :born "t1/i1/f1"}))
          {ctx' :ctx warnings :warnings :as out}
          (eng/apply-mutator ctx "t1/i2/f1" :fact-set! ["F" {:depends_on [[:task "T"]]}])]

      (it "refuses the write (no :depends_on on :F)"
        (expect (not (:stamped? out)))
        (expect (nil? (get-in ctx' [:session/facts "F" :depends_on]))))

      (it "emits a :depends_on_cycle warning"
        (expect (some #(= :depends_on_cycle (:code %)) warnings))))))

(defdescribe build-indexes-dep-graph-typed-test
  (describe "build-indexes derives the typed :dep-graph across task + fact kinds"
    ;; Canonical shape: :dep-graph keys are typed `[:kind :K]` refs.
    ;; Bare-key entries on `:depends_on` are normalized to same-kind
    ;; typed refs at index-build time.
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/tasks "T"]
                  {:title "t" :born "t1/i1/f1" :depends_on [[:fact "F"]]})
                (assoc-in [:session/facts "F"]
                  {:content "f" :born "t1/i1/f1"}))
          idx (eng/build-indexes ctx)]

      (it "indexes the typed dep-graph with each node present"
        (let [g (:dep-graph idx)]
          (expect (contains? g [:task "T"]))
          (expect (contains? g [:fact "F"]))))

      (it "edges normalize bare and typed refs uniformly"
        (let [g (:dep-graph idx)]
          (expect (= #{[:fact "F"]} (get g [:task "T"])))))

      (it "empty-deps entity still appears as a node with an empty edge set"
        (let [g (:dep-graph idx)]
          (expect (= #{} (get g [:fact "F"]))))))))

(defdescribe depends-on-dangling-warning-test
  (describe "typed :depends_on refs that point at nonexistent entities surface as warnings"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/tasks "T"]
                  {:title "t" :born "t1/i1/f1" :depends_on [[:fact "ghost"]]}))
          idx (eng/build-indexes ctx)
          warns (eng/derive-warnings ctx idx)]
      (it "emits a dangling-dep warning naming the missing kind+key"
        ;; derive-warnings is a vec of short strings now.
        (expect (some #(and (re-find #"ghost" %)
                         (re-find #"depends_on" %))
                  warns))))))

;; =============================================================================
;; Task done semantics — self-asserted, no validator, no reversion
;; =============================================================================

(defdescribe utilization-test
  (describe "utilization — last request's share of the context window"
    (it "spells out each field; :pct-of-limit = request / limit"
      (let [u (eng/utilization 95000 200000 240000 144000)]
        (expect (= 95000 (:last-request-tokens u)))
        (expect (= 200000 (:model-input-limit u)))
        (expect (= 48 (:pct-of-limit u)))
        (expect (= 240000 (:turn-total-tokens u)))
        (expect (= 144000 (:auto-compress-above u)))))
    (it "nil until a request is measured (iter 0 shows nothing)"
      (expect (nil? (eng/utilization 0 200000 0 144000))))
    (it "omits :pct-of-limit/:model-input-limit when the window is unknown"
      (let [u (eng/utilization 95000 nil 95000 144000)]
        (expect (= 95000 (:last-request-tokens u)))
        (expect (nil? (:pct-of-limit u)))
        (expect (nil? (:model-input-limit u)))))))

(defdescribe entity-id-test
  (describe "entity-id + :id stamping (turn-qualified stable ids)"
    (it "entity-id derives :t<N>/key from the birth scope"
      (expect (= "t3/auth" (eng/entity-id "t3/i2/f1" :auth)))
      (expect (= "t12/setup" (eng/entity-id "t12/i1/f4" :setup))))

    (it "find-entity-by-id locates across live facts+tasks AND :session/archived"
      (let [ctx {:session/facts {:a {:id :t1/a}}
                 :session/tasks {:b {:id :t2/b}}
                 :session/archived {:t3/c {:id :t3/c :content "gone"
                                           :vis/kind :fact :vis/key :c}}}]
        (expect (= {:source :live :kind :fact} (select-keys (eng/find-entity-by-id ctx :t1/a) [:source :kind])))
        (expect (= {:source :live :kind :task} (select-keys (eng/find-entity-by-id ctx :t2/b) [:source :kind])))
        (let [hit (eng/find-entity-by-id ctx :t3/c)]
          (expect (= :archived (:source hit)))
          (expect (= :fact (:kind hit)))
          (expect (= :c (:key hit)))
          (expect (= "gone" (get-in hit [:entry :content]))))
        (expect (nil? (eng/find-entity-by-id ctx :t9/none)))))

    (it "gc-pass moves an archived entity (past TTL) into :session/archived"
      (let [ctx {:session/scope {:turn 5}
                 :session/facts {:auth {:id :t3/auth :content "JWT"
                                        :status :archived :done-born "t3/i2/f1"}}}
            gced (eng/gc-pass ctx)]
        (expect (not (contains? (:session/facts gced) :auth)))
        (expect (= "JWT" (get-in gced [:session/archived :t3/auth :content])))
        (expect (= :fact (get-in gced [:session/archived :t3/auth :vis/kind])))))

    (it "recall-entity restores from :session/archived (removes the archived copy)"
      (let [ctx {:session/facts {}
                 :session/archived {:t3/auth {:id :t3/auth :content "JWT"
                                              :vis/kind :fact :vis/key :auth}}}
            {rc :ctx :keys [found? kind]} (eng/recall-entity ctx :t3/auth "t5/i1/f1" "need it")]
        (expect found?)
        (expect (= :fact kind))
        (expect (= :active (get-in rc [:session/facts :auth :status])))
        (expect (= "need it" (get-in rc [:session/facts :auth :recalled :why])))
        (expect (not (contains? (:session/archived rc) :t3/auth)))))

    (it "task-set! stamps :id on creation (turn from the form-scope)"
      (let [{ctx' :ctx} (eng/apply-mutator (eng/empty-ctx "s") "t3/i1/f1"
                          :task-set! ["swap" {:title "x" :status :todo}])]
        (expect (= "t3/swap" (get-in ctx' [:session/tasks "swap" :id])))))

    (it "fact-set! stamps :id on creation; update does NOT re-stamp"
      (let [{c1 :ctx} (eng/apply-mutator (eng/empty-ctx "s") "t3/i1/f1"
                        :fact-set! ["auth" {:content "v1"}])
            {c2 :ctx} (eng/apply-mutator c1 "t5/i2/f1"
                        :fact-set! ["auth" {:content "v2"}])]
        (expect (= "t3/auth" (get-in c1 [:session/facts "auth" :id])))
        ;; same logical entity — :id stays the turn-3 birth id
        (expect (= "t3/auth" (get-in c2 [:session/facts "auth" :id])))
        (expect (= "v2" (get-in c2 [:session/facts "auth" :content])))))))

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
                                               :depends_on [:a]
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
                                               :depends_on [:a]
                                               :born "t1/i1/f2"}))
          warns (eng/derive-warnings ctx (eng/build-indexes ctx))]
      (it "stays quiet"
        (expect (not-any? #(re-find #"but dep" %) warns))))))

;; =============================================================================
;; Contradiction relations on facts — declared as a FIELD on the ONE fact verb.
;;
;; `fact_set(:K, {:contradicts [...]})` REPLACES K's contradiction set and
;; reconciles the symmetric back-links on the other facts (NON-TRANSITIVE).
;; Re-sending the list without a link RETRACTS it on both sides. Self-refs are
;; dropped; missing targets warn. When both facts stay `:active`,
;; `derive-warnings` emits a contradiction string; resolving it is the model's
;; job (flip one to `:superseded`).
;; =============================================================================

(defdescribe fact-contradicts-test
  (describe "fact_set {:contradicts} writes the link on both facts symmetrically"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts "f1"] {:content "x" :born "t1/i1/f1"})
                (assoc-in [:session/facts "f2"] {:content "y" :born "t1/i1/f1"}))
          {ctx' :ctx :as out}
          (eng/apply-mutator ctx "t1/i1/f2" :fact-set! ["f1" {:contradicts ["f2"]}])]

      (it "writes :contradicts on the first fact"
        (expect (= #{"f2"} (get-in ctx' [:session/facts "f1" :contradicts]))))

      (it "writes :contradicts on the second fact (symmetric back-link)"
        (expect (= #{"f1"} (get-in ctx' [:session/facts "f2" :contradicts]))))

      (it "stamped? true"
        (expect (:stamped? out)))))

  (describe "a self-reference is dropped silently (no self-contradiction)"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts "f1"] {:content "x" :born "t1/i1/f1"}))
          {ctx' :ctx :as out}
          (eng/apply-mutator ctx "t1/i1/f2" :fact-set! ["f1" {:contradicts ["f1"]}])]
      (it "stamped? true (the upsert still applies)"
        (expect (:stamped? out)))
      (it "no self-link is written"
        (expect (empty? (get-in ctx' [:session/facts "f1" :contradicts]))))))

  (describe "a missing-fact reference warns but still upserts"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts "f1"] {:content "x" :born "t1/i1/f1"}))
          {warnings :warnings :as out}
          (eng/apply-mutator ctx "t1/i1/f2" :fact-set! ["f1" {:contradicts ["ghost"]}])]
      (it "stamped? true"
        (expect (:stamped? out)))
      (it "emits :fact-contradicts-missing"
        (expect (some #(= :fact-contradicts-missing (:code %)) warnings))))))

(defdescribe fact-contradicts-remove-test
  (describe "fact_set {:contradicts []} retracts the link on both sides"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts "f1"] {:content "x" :born "t1/i1/f1"
                                                 :contradicts #{"f2"}})
                (assoc-in [:session/facts "f2"] {:content "y" :born "t1/i1/f1"
                                                 :contradicts #{"f1"}}))
          {ctx' :ctx}
          (eng/apply-mutator ctx "t1/i2/f1" :fact-set! ["f1" {:contradicts []}])]
      (it "drops :contradicts from the first fact"
        (expect (empty? (get-in ctx' [:session/facts "f1" :contradicts]))))
      (it "drops :contradicts from the second fact (symmetric retract)"
        (expect (empty? (get-in ctx' [:session/facts "f2" :contradicts]))))))

  (describe "retract on a fresh ctx is a no-op (no warning)"
    (let [ctx (-> (eng/empty-ctx "t")
                (assoc-in [:session/facts "f1"] {:content "x" :born "t1/i1/f1"}))
          {warnings :warnings :as out}
          (eng/apply-mutator ctx "t1/i2/f1" :fact-set! ["f1" {:contradicts []}])]
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

;; =============================================================================
;; W1 — actionable summaries: model-produced :files survive on the trailer stub
;; =============================================================================

(defdescribe trailer-summarize-files-test
  (let [files [{:path "ext/.../components.clj"
                :regions [{:src "(def close-button-width 4)\n(def close-button-glyph \"│ ✕ \")"
                           :note "close-button consts — glyph │ ✕ width 4"
                           :from-anchor "42:a1b2" :to-anchor "43:c3d4"}
                          ;; content-only region: :src is the anchor, anchors optional
                          {:src "(when (and closable? (>= width …)) …)"
                           :note "show-close? gate"}]}]
        directive {:scope-start "t2/i1" :scope-end "t2/i3"
                   :summary "patched glyph; raw reads no longer needed"
                   :files files}
        {:keys [trailer warnings]}
        (eng/apply-trailer-summarize [] [directive] "t3/i1/f1")
        stub (first trailer)]
    (describe "apply-trailer-summarize with :files"
      (it "carries :files onto the stored stub verbatim"
        (expect (= files (:files stub))))
      (it "still stamps the standard summary fields"
        (expect (= "t2/i1" (:scope-start stub)))
        (expect (= "t2/i3" (:scope-end stub)))
        (expect (= "t3/i1/f1" (:born stub))))
      (it "emits no warnings for a well-formed :files summary"
        (expect (= [] warnings)))
      (it "is spec-valid as a trailer-summary"
        (expect (s/valid? ::cs/trailer-summary stub))))
    (describe "without :files (back-compat)"
      (let [{t2 :trailer} (eng/apply-trailer-summarize
                            [] [(dissoc directive :files)] "t3/i1/f1")
            s2 (first t2)]
        (it "omits the :files key entirely"
          (expect (not (contains? s2 :files))))
        (it "remains spec-valid"
          (expect (s/valid? ::cs/trailer-summary s2)))))
    ;; REGRESSION: the Python sandbox is full-snake (env-python/py-key->clj),
    ;; so a `summarize({"trailer":[{"scope_start": …}]})` spec arrives with
    ;; :scope_start / :scope_end (underscore). Before the boundary fix the
    ;; kebab-only destructure silently no-op'd with a bad-scope warning and
    ;; the trailer never folded. Honor both; keep storing kebab.
    (describe "snake_case keys from the Python boundary"
      (let [{:keys [trailer warnings]}
            (eng/apply-trailer-summarize
              []
              [{:scope_start "t2/i1" :scope_end "t2/i3"
                :summary "patched glyph; raw reads no longer needed"
                :files files}]
              "t3/i1/f1")
            stub (first trailer)]
        (it "folds the range (no bad-scope warning)"
          (expect (= [] warnings))
          (expect (= 1 (count trailer))))
        (it "stores the canonical kebab :scope-start / :scope-end"
          (expect (= "t2/i1" (:scope-start stub)))
          (expect (= "t2/i3" (:scope-end stub))))
        (it "carries :files through verbatim"
          (expect (= files (:files stub))))
        (it "is spec-valid as a trailer-summary"
          (expect (s/valid? ::cs/trailer-summary stub)))))))

;; =============================================================================
;; W2 — file knowledge graduates into a DURABLE fact (same :files region shape)
;; =============================================================================

(defdescribe fact-files-test
  (let [files [{:path "ext/.../components.clj"
                :regions [{:src "(def close-button-width 4)"
                           :note "consts" :from-anchor "42:a1b2" :to-anchor "42:a1b2"}]}]
        {ctx :ctx} (#'eng/apply-fact-set! (eng/empty-ctx) "t1/i1/f1"
                                          ["btn" {:content "close-button geometry" :files files}])
        fact (get-in ctx [:session/facts "btn"])]
    (describe "fact-set! with :files"
      (it "carries the structured :files through the merge-based upsert"
        (expect (= files (:files fact))))
      (it "stamps the standard fact fields (:born, :id)"
        (expect (= "t1/i1/f1" (:born fact)))
        (expect (some? (:id fact))))
      (it "is spec-valid as a fact"
        (expect (s/valid? ::cs/fact fact)))
      (it "a fact without :files stays spec-valid (back-compat)"
        (let [{c2 :ctx} (#'eng/apply-fact-set! (eng/empty-ctx) "t1/i1/f1"
                                               ["plain" {:content "no files here"}])]
          (expect (not (contains? (get-in c2 [:session/facts "plain"]) :files)))
          (expect (s/valid? ::cs/fact (get-in c2 [:session/facts "plain"]))))))))

;; =============================================================================
;; W3 — task acceptance + verification: done-unverified structural warning
;; =============================================================================

(defdescribe task-done-unverified-test
  (let [warns (fn [task]
                (let [ctx (assoc (eng/empty-ctx) :session/tasks {:t1 task})]
                  (eng/derive-warnings ctx (eng/build-indexes ctx))))
        unverified? (fn [ws] (boolean (some #(re-find #":verified\? not true" %) ws)))
        base {:title "x" :status :done :born "t1/i1/f1"}]
    (describe "pass-task-done-unverified"
      (it "warns when :done with an :acceptance but :verified? not true"
        (expect (unverified? (warns (assoc base :acceptance "tests pass")))))
      (it "is silent when :verified? true"
        (expect (not (unverified? (warns (assoc base :acceptance "tests pass"
                                           :verified? true))))))
      (it "is silent when there is no :acceptance (trivial task)"
        (expect (not (unverified? (warns base)))))
      (it "is silent for a :doing task with acceptance (not closed yet)"
        (expect (not (unverified? (warns (assoc base :status :doing
                                           :acceptance "tests pass")))))))
    (describe "::task spec accepts acceptance + verified?"
      (it "is spec-valid with both fields"
        (expect (s/valid? ::cs/task (assoc base :acceptance "tests pass"
                                      :verified? true)))))))

;; =============================================================================
;; recall SEARCH — pure helpers (no DB; the I/O is injected)
;; =============================================================================

(defdescribe iter-scope-after?-test
  (describe "iter-scope-after? — strictly-after paging predicate"
    (it "nil cursor admits every scope"
      (expect (true? (boolean (eng/iter-scope-after? 1 1 nil))))
      (expect (true? (boolean (eng/iter-scope-after? 9 9 nil)))))
    (it "later turn is after, regardless of iter"
      (expect (eng/iter-scope-after? 3 1 {:turn 2 :iter 9})))
    (it "same turn, later iter is after"
      (expect (eng/iter-scope-after? 2 5 {:turn 2 :iter 4})))
    (it "the cursor scope itself is NOT after (strict)"
      (expect (not (eng/iter-scope-after? 2 4 {:turn 2 :iter 4}))))
    (it "earlier turn / earlier iter is not after"
      (expect (not (eng/iter-scope-after? 1 9 {:turn 2 :iter 1})))
      (expect (not (eng/iter-scope-after? 2 3 {:turn 2 :iter 4}))))))

(defdescribe search-hits->scopes-test
  (let [turns    [{:id "soul-1" :position 1} {:id "soul-2" :position 2}]
        iter-map {"soul-1" [{:id "it-1a" :position 1} {:id "it-1b" :position 2}]
                  "soul-2" [{:id "it-2a" :position 1}]}
        iters-of iter-map]
    (describe "search-hits->scopes — resolve FTS5 hits to tN/iM pointers"
      (it "maps owner-id → scope, carrying preview + rank"
        (let [hits [{:owner-id "it-1b" :snippet "(rg …)" :rank -1.2}]]
          (expect (= [{:scope "t1/i2" :preview "(rg …)" :rank -1.2}]
                    (eng/search-hits->scopes hits turns iters-of nil)))))
      (it "preserves FTS5 rank order across turns (does not re-sort)"
        (let [hits [{:owner-id "it-2a" :snippet "b" :rank -2.0}
                    {:owner-id "it-1a" :snippet "a" :rank -1.0}]]
          (expect (= ["t2/i1" "t1/i1"]
                    (mapv :scope (eng/search-hits->scopes hits turns iters-of nil))))))
      (it "drops a hit whose owner-id resolves to no iteration row"
        (let [hits [{:owner-id "ghost" :snippet "x" :rank -1.0}
                    {:owner-id "it-1a" :snippet "y" :rank -1.1}]]
          (expect (= ["t1/i1"]
                    (mapv :scope (eng/search-hits->scopes hits turns iters-of nil))))))
      (it "applies the scope_after cursor (drops at-or-before)"
        (let [hits [{:owner-id "it-1a" :snippet "a" :rank -1.0}   ; t1/i1 — dropped
                    {:owner-id "it-1b" :snippet "b" :rank -1.1}   ; t1/i2 — dropped (== cursor)
                    {:owner-id "it-2a" :snippet "c" :rank -1.2}]] ; t2/i1 — kept
          (expect (= ["t2/i1"]
                    (mapv :scope (eng/search-hits->scopes hits turns iters-of {:turn 1 :iter 2}))))))
      (it "empty hits → empty vector (never nil)"
        (expect (= [] (eng/search-hits->scopes [] turns iters-of nil))))
      (it "is a vector, not a lazy seq"
        (expect (vector? (eng/search-hits->scopes
                           [{:owner-id "it-1a" :snippet "x" :rank -1.0}]
                           turns iters-of nil))))
      (it "matches String owner-ids against UUID row ids (the PRODUCTION shape)"
        ;; regression: FTS hits carry :owner-id as the raw TEXT column (a
        ;; String) while the persistence layer coerces row :id to
        ;; java.util.UUID — a typed `=` matched NOTHING, so recall search
        ;; returned [] for EVERY query in every session (the model could
        ;; never find past work or failures; session f5aba6d4 turn 2 hunted
        ;; a prior clj_edit failure and recall came back empty)
        (let [u1    #uuid "7aadb186-267c-4ccd-803f-3aa9796f2b86"
              u2    #uuid "533b3e38-8a6f-43fa-ab9b-3a678d8e702d"
              turns [{:id u2 :position 1}]
              iters {u2 [{:id u1 :position 3}]}
              hits  [{:owner-id (str u1) :snippet "clj_edit(…)" :rank -3.4}]]
          (expect (= [{:scope "t1/i3" :preview "clj_edit(…)" :rank -3.4}]
                    (eng/search-hits->scopes hits turns iters nil))))))))
