(ns com.blockether.vis.rlm.context-tools-test
  (:require
   [clojure.string :as str]
   [lazytest.core :refer [defdescribe describe expect it]]
   [sci.core :as sci]
   [com.blockether.vis.rlm.core :as core]
   [com.blockether.vis.rlm.tools :as tools]))

(defn- make-ctx
  "Create a sci context for testing."
  []
  (let [{:keys [sci-ctx sandbox-ns initial-ns-keys]} (tools/create-sci-context (fn [_] {:content "ok"}) nil nil nil)]
    {:sci-ctx sci-ctx :sandbox-ns sandbox-ns :initial-ns-keys initial-ns-keys}))

(defn- eval-in [ctx code]
  (:val (sci/eval-string+ (:sci-ctx ctx) code {:ns (:sandbox-ns ctx)})))

(defn- sandbox-map
  "Extract the sandbox namespace map from a SCI context."
  [ctx]
  (get-in @(:env (:sci-ctx ctx)) [:namespaces 'sandbox]))

(defdescribe var-based-state-test
  (describe "def with docstrings"
    (it "def creates a var accessible in SCI"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def results \"search results\" [1 2 3])")
        (expect (= [1 2 3] (eval-in ctx "results")))))

    (it "docstring is accessible via meta"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def results \"search results\" [1 2 3])")
        (expect (= "search results" (eval-in ctx "(:doc (meta (var results)))"))))))

  (describe "var index"
    (it "build-var-index returns nil when no user vars"
      (let [{:keys [sci-ctx initial-ns-keys]} (make-ctx)]
        (expect (nil? (tools/build-var-index sci-ctx initial-ns-keys)))))

    (it "build-var-index uses bounded size for lazy/infinite seqs"
      (let [{:keys [sci-ctx initial-ns-keys] :as ctx} (make-ctx)]
        (eval-in ctx "(def xs (range))")
        (let [idx (tools/build-var-index sci-ctx initial-ns-keys)]
          (expect (string? idx))
          (expect (str/includes? idx "xs"))
          (expect (str/includes? idx "1000+ items")))))

    (it "build-var-index caps rendered rows"
      (let [{:keys [sci-ctx initial-ns-keys] :as ctx} (make-ctx)]
        (doseq [i (range 45)]
          (eval-in ctx (str "(def v" i " " i ")")))
        (let [idx (tools/build-var-index sci-ctx initial-ns-keys)]
          (expect (string? idx))
          (expect (str/includes? idx "more vars omitted"))))))

  (describe "vars persist across evaluations"
    (it "def'd vars persist across eval calls"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def x \"first var\" 42)")
        (eval-in ctx "(def y \"second var\" (* x 2))")
        (expect (= 84 (eval-in ctx "y")))))))

;; =============================================================================
;; Auto-forget tests
;; =============================================================================

(defdescribe auto-forget-candidates-test
  (describe "basic eligibility"
    (it "returns empty set when no vars in sandbox"
      (let [candidates (core/auto-forget-candidates {} #{} {} #{"q1"})]
        (expect (empty? candidates))))

    (it "returns empty set when all vars are built-in tools"
      (let [sandbox {'read-file (fn [_] nil) 'write-file (fn [_] nil)}
            initial-keys #{'read-file 'write-file}
            candidates (core/auto-forget-candidates sandbox initial-keys {} #{"q1"})]
        (expect (empty? candidates))))

    (it "does NOT forget vars with docstrings"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def important \"this is documented\" 42)")
        (let [smap (sandbox-map ctx)
              registry {'important {:query-id "old-query" :value 42 :code "(def important ...)"}}
              recent #{"q1" "q2" "q3"}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry recent)]
          (expect (not (contains? candidates 'important))))))

    (it "does NOT forget vars defined in a recent query"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def scratch 99)")
        (let [smap (sandbox-map ctx)
              registry {'scratch {:query-id "q2" :value 99 :code "(def scratch 99)"}}
              recent #{"q1" "q2" "q3"}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry recent)]
          (expect (not (contains? candidates 'scratch))))))

    (it "DOES forget vars without docstring defined in an old query"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def scratch 99)")
        (let [smap (sandbox-map ctx)
              registry {'scratch {:query-id "old-query" :value 99 :code "(def scratch 99)"}}
              recent #{"q1" "q2" "q3"}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry recent)]
          (expect (contains? candidates 'scratch)))))

    (it "does NOT forget vars that are not in the registry (fresh, not yet persisted)"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def fresh-var 123)")
        (let [smap (sandbox-map ctx)
              ;; fresh-var has no entry in registry — it's brand new
              registry {}
              recent #{"q1"}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry recent)]
          (expect (not (contains? candidates 'fresh-var)))))))

  (describe "mixed scenarios"
    (it "forgets only the stale undocumented vars from a mixed set"
      (let [ctx (make-ctx)]
        ;; documented var from old query — should survive
        (eval-in ctx "(def config \"app config\" {:port 3000})")
        ;; undocumented var from old query — should be forgotten
        (eval-in ctx "(def tmp-data [1 2 3])")
        ;; undocumented var from recent query — should survive
        (eval-in ctx "(def recent-scratch 42)")
        ;; undocumented var from old query — should be forgotten
        (eval-in ctx "(def old-calc 99)")
        (let [smap (sandbox-map ctx)
              registry {'config         {:query-id "old-q" :value {:port 3000} :code "..."}
                        'tmp-data       {:query-id "old-q" :value [1 2 3] :code "..."}
                        'recent-scratch {:query-id "q3"    :value 42 :code "..."}
                        'old-calc       {:query-id "old-q" :value 99 :code "..."}}
              recent #{"q1" "q2" "q3"}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry recent)]
          ;; config has docstring → safe
          (expect (not (contains? candidates 'config)))
          ;; recent-scratch in recent query → safe
          (expect (not (contains? candidates 'recent-scratch)))
          ;; tmp-data: no doc + old query → forgotten
          (expect (contains? candidates 'tmp-data))
          ;; old-calc: no doc + old query → forgotten
          (expect (contains? candidates 'old-calc))
          ;; exactly 2 candidates
          (expect (= 2 (count candidates))))))

    (it "empty docstring counts as no docstring"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def x \"\" 42)")
        (let [smap (sandbox-map ctx)
              registry {'x {:query-id "old-q" :value 42 :code "..."}}
              recent #{"q1"}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry recent)]
          (expect (contains? candidates 'x)))))

    (it "whitespace-only docstring counts as no docstring"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def x \"  \" 42)")
        (let [smap (sandbox-map ctx)
              registry {'x {:query-id "old-q" :value 42 :code "..."}}
              recent #{"q1"}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry recent)]
          (expect (contains? candidates 'x))))))

  (describe "edge cases"
    (it "works with empty recent-query-ids (all stale undocumented vars forgotten)"
      (let [ctx (make-ctx)]
        (eval-in ctx "(def a 1)")
        (eval-in ctx "(def b \"documented\" 2)")
        (let [smap (sandbox-map ctx)
              registry {'a {:query-id "q1" :value 1 :code "..."}
                        'b {:query-id "q1" :value 2 :code "..."}}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry #{})]
          ;; a: no doc + not in recent (empty) → forgotten
          (expect (contains? candidates 'a))
          ;; b: has doc → safe
          (expect (not (contains? candidates 'b))))))

    (it "handles fn vars (defn) the same as value vars"
      (let [ctx (make-ctx)]
        (eval-in ctx "(defn helper [x] (* x 2))")
        (let [smap (sandbox-map ctx)
              ;; defn creates :arglists meta but no :doc unless provided
              registry {'helper {:query-id "old-q" :value nil :code "..."}}
              recent #{"q1"}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry recent)]
          (expect (contains? candidates 'helper)))))

    (it "defn with docstring is preserved"
      (let [ctx (make-ctx)]
        (eval-in ctx "(defn helper \"doubles input\" [x] (* x 2))")
        (let [smap (sandbox-map ctx)
              registry {'helper {:query-id "old-q" :value nil :code "..."}}
              recent #{"q1"}
              candidates (core/auto-forget-candidates smap (:initial-ns-keys ctx)
                           registry recent)]
          (expect (not (contains? candidates 'helper))))))))
