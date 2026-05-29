(ns com.blockether.vis.internal.iteration-test
  "Phase-4 high-fan-out policy — engine soft warning + renderer aggregation,
   both as PURE projections off the canonical iteration-entry `:ops`.

   The fixture is a `doseq` fan-out: ONE fence / ONE proof envelope whose
   channel slice carries N `(v/cat …)` observation ops. That is the exact
   bad-agent-behaviour Phase 4 targets:

     - the header counts must stay REAL (`100 observations`),
     - a same-op run > aggregate-threshold collapses into ONE synthetic row,
     - the block surfaces a soft BATCH HINT once the run exceeds the
       threshold (default 5, per-tool overridable)."
  (:require
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.iteration :as iteration]
   [lazytest.core :refer [defdescribe expect it describe]]))

;; ---------------------------------------------------------------------------
;; Fixture builders.
;; ---------------------------------------------------------------------------

(defn- cat-sink-entry
  "One `(v/cat \"<name>\")` observation sink entry whose summary label is the
   file name — so the aggregated head reads `CAT × N (a.txt, b.txt, …)`."
  [position file]
  {:position position
   :form     (str "(v/cat \"" file "\")")
   :symbol   :cat
   :op       :cat
   :tag      :observation
   :success? true
   :error    nil
   :result   {:summary (extension/ir-root
                         (extension/ir-p (extension/ir-strong file)
                           "  120 lines"))
              :display (extension/ir-root
                         (extension/ir-p (extension/ir-strong file)
                           "  full body"))}})

(defn- fanout-entry
  "A single-fence iteration entry whose one proof envelope ran `n` cat ops
   plus one trailing `(v/patch …)` mutation, mirroring a `doseq` fan-out."
  [n]
  (let [cats  (mapv (fn [i] (cat-sink-entry i (str "f" i ".txt"))) (range n))
        patch {:position n
               :form     "(v/patch ...)"
               :symbol   :patch
               :op       :patch
               :tag      :mutation
               :success? true
               :error    nil
               :result   {:summary (extension/ir-root
                                     (extension/ir-p (extension/ir-strong "PATCH")
                                       "  +24 -8"))
                          :display (extension/ir-root
                                     (extension/ir-p (extension/ir-strong "PATCH")))}}
        channel (conj cats patch)]
    {:position 0
     :code     "(doseq [f files] (v/cat f))\n(v/patch ...)"
     :forms    [{:scope       "t1/i1/f1"
                 :tag         :observation
                 :src         "(doseq [f files] (v/cat f))"
                 :channel     channel
                 :duration-ms 4200}]}))

;; ---------------------------------------------------------------------------
;; Engine soft warning — block-batch-hints.
;; ---------------------------------------------------------------------------

(defdescribe batch-hint-test
  (it "no hint below the default threshold"
    (expect (= [] (iteration/block-batch-hints (fanout-entry 5)))))

  (it "fires once the same-op count EXCEEDS the default threshold"
    (let [hints (iteration/block-batch-hints (fanout-entry 6))]
      (expect (= 1 (count hints)))
      (let [{:keys [op count threshold text]} (first hints)]
        (expect (= :cat op))
        (expect (= 6 count))
        (expect (= iteration/default-batch-hint-threshold threshold))
        (expect (= "BATCH HINT  call (cat [...]) once instead of 6 times" text)))))

  (it "the lone mutation never trips the hint"
    (let [hints (iteration/block-batch-hints (fanout-entry 100))]
      (expect (= [:cat] (mapv :op hints)))))

  (it "honours a per-tool threshold override"
    (let [;; cat overridden to 50 → 30 cats no longer trips.
          th-fn (fn [op] (when (= :cat op) 50))]
      (expect (= [] (iteration/block-batch-hints (fanout-entry 30) th-fn)))
      (expect (= [:cat] (mapv :op (iteration/block-batch-hints (fanout-entry 60) th-fn)))))))

;; ---------------------------------------------------------------------------
;; Renderer aggregation — aggregate-ops.
;; ---------------------------------------------------------------------------

(defdescribe aggregate-ops-test
  (it "leaves short same-op runs untouched"
    (let [ops (:ops (iteration/canonicalize (fanout-entry 5)))]
      (expect (every? (complement :aggregate) (iteration/aggregate-ops ops)))
      (expect (= 6 (count (iteration/aggregate-ops ops)))))) ;; 5 cats + 1 patch

  (it "collapses a run longer than aggregate-threshold into one synthetic op"
    (let [ops  (:ops (iteration/canonicalize (fanout-entry 100)))
          agg  (iteration/aggregate-ops ops)
          ;; 100 cats collapse to 1 synthetic row; the lone patch stays.
          synth (first (filter :aggregate agg))]
      (expect (= 2 (count agg)))
      (expect (= :cat (:op synth)))
      (expect (= 100 (:count synth)))
      (expect (= :observation (:tag synth)))
      (expect (= 100 (count (:ops synth))))
      ;; The trailing mutation is NOT aggregated.
      (expect (= [:patch] (mapv :op (remove :aggregate agg))))))

  (it "the synthetic head reads `CAT × 100 (a, b, c, …)`"
    (let [ops   (:ops (iteration/canonicalize (fanout-entry 100)))
          synth (first (filter :aggregate (iteration/aggregate-ops ops)))]
      (expect (= "CAT × 100 (f0.txt, f1.txt, f2.txt, …)"
                (iteration/aggregate-op-head synth)))))

  (it "errored runs surface :error status on the synthetic op"
    (let [ops (mapv (fn [o] (assoc o :status :error))
                (:ops (iteration/canonicalize (fanout-entry 100))))
          synth (first (filter :aggregate (iteration/aggregate-ops ops)))]
      (expect (= :error (:status synth))))))

;; ---------------------------------------------------------------------------
;; Header counts stay REAL regardless of aggregation.
;; ---------------------------------------------------------------------------

(defdescribe display-block-fanout-test
  (describe "100-cat doseq fan-out"
    (it "header counts are the REAL totals, not the aggregated view"
      (let [block (iteration/iteration-entry->display-block (fanout-entry 100))]
        ;; 100 real observation ops + 1 mutation — never `1 observation`.
        (expect (= {:observations 100 :mutations 1} (:counts block)))
        ;; The raw :ops stay un-aggregated (proof/display granular).
        (expect (= 101 (count (:ops block))))))

    (it "exposes both the batch hint and the aggregated view on the block"
      (let [block (iteration/iteration-entry->display-block (fanout-entry 100))]
        (expect (= [:cat] (mapv :op (:batch-hints block))))
        (expect (= 1 (count (filter :aggregate (:aggregated-ops block)))))
        (expect (= 100 (:count (first (filter :aggregate (:aggregated-ops block))))))))))
