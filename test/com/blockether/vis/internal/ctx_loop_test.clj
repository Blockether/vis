(ns com.blockether.vis.internal.ctx-loop-test
  "Tests for the loop integration adapter — scope synthesis, ctx atom
   swapping, mutator binding wiring."
  (:require
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-loop :as cl]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- mk-env []
  {:ctx-atom (cl/make-ctx-atom "test-session")
   :ctx-warnings-atom (cl/make-warnings-atom)
   :current-turn-position-atom (atom 2)
   :current-iteration-atom (atom 3)
   :current-form-idx-atom (atom 4)})

(defdescribe synthesize-scope-test
  (describe "synthesize-scope"
    (it "builds tN/iM/fK from the loop counters (form is 1-based)"
      (expect (= "t2/i3/f5" (cl/synthesize-scope (mk-env)))))

    (it "supports :current-iteration-atom holding a map (positional shape)"
      (let [env (assoc (mk-env)
                  :current-iteration-atom (atom {:position 7 :other :stuff}))]
        (expect (= "t2/i7/f5" (cl/synthesize-scope env)))))

    (it "defaults to t1/i1/f1 when atoms unset"
      (expect (= "t1/i1/f1" (cl/synthesize-scope {}))))))

(defdescribe cursor-snapshot-test
  (describe "cursor-snapshot"
    (it "produces engine-cursor shape {:turn :iter :next-form}"
      (expect (= {:turn 2 :iter 3 :next-form 5}
                (cl/cursor-snapshot (mk-env)))))

    (it "defaults safely on empty env"
      (expect (= {:turn 1 :iter 1 :next-form 1}
                (cl/cursor-snapshot {}))))))

(defdescribe build-sci-bindings-test
  (describe "SCI bindings build"
    (let [env (mk-env)
          bindings (cl/build-sci-bindings env)]
      (it "exposes every engine mutator"
        (expect (= #{'spec-set! 'task-set! 'fact-set!
                     'req-add! 'req-update! 'req-remove!
                     'proof-add! 'proof-remove!}
                  (set (keys bindings)))))

      (it "each binding is a callable function"
        (expect (every? fn? (vals bindings)))))))

(defdescribe mutator-roundtrip-test
  (describe "spec-set! through the binding mutates the ctx atom"
    (let [env (mk-env)
          {spec-set 'spec-set!} (cl/build-sci-bindings env)
          ret (spec-set :auth {:title "switch to bcrypt" :status :draft})]
      (it "returns :vis/silent (no echo)"
        (expect (= :vis/silent ret)))

      (it "ctx atom carries the new spec"
        (expect (= "switch to bcrypt"
                  (get-in @(:ctx-atom env) [:session/specs :auth :title]))))

      (it "the :born scope was stamped from the loop cursor"
        (expect (= "t2/i3/f5"
                  (get-in @(:ctx-atom env) [:session/specs :auth :born])))))))

(defdescribe req-add-collision-warning-test
  (describe "req-add! collision routes to the warnings atom"
    (let [env (mk-env)
          {spec-set 'spec-set! req-add 'req-add!} (cl/build-sci-bindings env)
          _ (spec-set :s {:title "x" :status :draft})
          _ (req-add :s {:id :r1 :title "first"})
          _ (req-add :s {:id :r1 :title "duplicate"})]
      (it "collision rejected; spec keeps original :r1 only"
        (expect (= 1 (count (get-in @(:ctx-atom env) [:session/specs :s :requirements])))))

      (it "warning captured in the warnings atom"
        (let [ws @(:ctx-warnings-atom env)]
          (expect (some #(= :req-add-collision (:code %)) ws)))))))

(defdescribe cycle-hard-reject-test
  (describe "task-set! :depends-on cycle is hard-rejected via the binding"
    (let [env (mk-env)
          {task-set 'task-set!} (cl/build-sci-bindings env)
          _ (task-set :a {:title "a" :specs {} :status :todo})
          _ (task-set :b {:title "b" :specs {} :status :todo :depends-on [:a]})
          _ (task-set :a {:depends-on [:b]})]
      (it ":a has no :depends-on (write refused)"
        (expect (not (contains? (get-in @(:ctx-atom env) [:session/tasks :a])
                       :depends-on))))

      (it ":depends-on-cycle warning captured"
        (expect (some #(= :depends-on-cycle (:code %))
                  @(:ctx-warnings-atom env)))))))

(defdescribe drain-warnings-test
  (describe "drain-warnings! returns + clears"
    (let [env (mk-env)
          {req-add 'req-add!} (cl/build-sci-bindings env)
          _ (req-add :missing-spec {:id :r :title "x"})]
      (it "drain yields the recorded warnings"
        (let [ws (cl/drain-warnings! env)]
          (expect (>= (count ws) 1))))

      (it "second drain returns empty (atom cleared)"
        (expect (empty? (cl/drain-warnings! env)))))))

(defdescribe current-ctx-test
  (describe "current-ctx stamps the cursor at render time"
    (let [env (mk-env)
          {spec-set 'spec-set!} (cl/build-sci-bindings env)
          _ (spec-set :s {:title "x" :status :draft})
          c (cl/current-ctx env)]
      (it "ctx carries the spec"
        (expect (= "x" (get-in c [:session/specs :s :title]))))

      (it ":session/scope reflects current cursor"
        (expect (= {:turn 2 :iter 3 :next-form 5}
                  (:session/scope c))))

      (it "ctx is engine-valid against ::cs/ctx after live mutations"
        ;; engine consumers (renderer, derive-warnings) need a valid ctx
        (expect (some? (:session/id c)))
        (expect (some? (:session/turn c)))
        (expect (some? (:session/scope c)))
        (expect (= "test-session" (:session/id c)))))))

(defdescribe end-to-end-mini-scenario-test
  (describe "small scenario via SCI bindings: facts → spec → req → task → proof"
    (let [env (mk-env)
          {sp 'spec-set! tk 'task-set! ft 'fact-set!
           ra 'req-add! pa 'proof-add!} (cl/build-sci-bindings env)
          _ (ft :rl-bug {:content "race"})
          _ (sp :rl {:title "race-free" :status :draft})
          _ (ra :rl {:id :linearizable :title "..." :facts [:rl-bug]})
          _ (tk :swap {:title "CAS" :specs {:rl []} :status :doing})
          _ (pa :swap :rl {:requirement :linearizable :proof "t2/i3/f5"})
          ctx (cl/current-ctx env)
          idx (eng/build-indexes ctx)
          prog (eng/derive-progression ctx idx)]
      (it "ctx has all four memo entries"
        (expect (= 1 (count (:session/facts ctx))))
        (expect (= 1 (count (:session/specs ctx))))
        (expect (= 1 (count (:session/tasks ctx)))))

      (it "spec progression is :ready (1/1) since the proof landed"
        (expect (= 1 (get-in prog [:rl :total])))
        (expect (= 1 (get-in prog [:rl :proven])))
        (expect (= :ready (get-in prog [:rl :state])))))))
