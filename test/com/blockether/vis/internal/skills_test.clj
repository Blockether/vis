(ns com.blockether.vis.internal.skills-test
  "Smoke tests for the internal skills loader + sandbox bindings.

   Hard rule (S3 - AGENTS.md): every src ns needs a matching test ns
   with at least one real public-API smoke test. This file covers
   `list-all`, `list-summaries`, and the `sandbox-bindings` map (in
   particular the `(skills)` lazy sandbox fn that replaced the
   `TURN_ACCESSIBLE_SKILLS` SYSTEM-var snapshot for ad-hoc reads;
   `TURN_ACCESSIBLE_SKILLS` itself stays alive for the var-history /
   per-iteration snapshot pipeline)."
  (:require
   [com.blockether.vis.internal.skills :as skills]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe list-all-shape-test
  (it "returns a vec (possibly empty) of compact skill maps"
    (let [v (skills/list-all)]
      (expect (vector? v))
      (doseq [s v]
        (expect (string? (:name s)))
        (expect (string? (:description s)))
        (expect (string? (:path s)))
        (expect (keyword? (:source s)))))))

(defdescribe list-summaries-shape-test
  (it "list-summaries returns the same compact shape as list-all but only :name :description :path :source (+ :extra when present)"
    (let [v (skills/list-summaries)]
      (expect (vector? v))
      (doseq [s v]
        (expect (every? #{:name :description :path :source :extra} (keys s)))
        (expect (string? (:name s)))
        (expect (string? (:description s)))
        (expect (string? (:path s)))
        (expect (keyword? (:source s))))))

  (it "list-summaries never throws on a partial or empty filesystem"
    ;; Public-API smoke: even when no skills are discoverable the fn
    ;; must degrade to [] (the contract `(skills)` advertises to the
    ;; sandbox).
    (expect (vector? (skills/list-summaries)))))

(defdescribe sandbox-bindings-shape-test
  (it "returns a map containing load-skill, reload-skills!, and the new (skills) lazy fn"
    (let [bindings (skills/sandbox-bindings (atom {}))]
      (expect (fn? (get bindings 'load-skill)))
      (expect (fn? (get bindings 'reload-skills!)))
      (expect (fn? (get bindings 'skills)))))

  (it "(skills) sandbox fn returns the same data as (list-summaries)"
    (let [bindings (skills/sandbox-bindings (atom {}))
          skills-fn (get bindings 'skills)]
      (expect (= (skills/list-summaries) (skills-fn))))))
