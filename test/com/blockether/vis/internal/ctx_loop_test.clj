(ns com.blockether.vis.internal.ctx-loop-test
  "Tests for the loop integration adapter — scope synthesis, ctx atom
   swapping, mutator binding wiring."
  (:require
   [com.blockether.vis.internal.ctx-engine :as eng]
   [com.blockether.vis.internal.ctx-loop :as cl]
   [lazytest.core :refer [defdescribe describe expect it]]))

(defn- mk-env []
  ;; Single ctx-atom carries everything; `:engine/warnings` and
  ;; `:engine/pending-satisfies` live as ephemeral keys on the ctx
  ;; itself (seeded by `eng/empty-ctx`).
  {:ctx-atom (cl/make-ctx-atom "test-session")
   :current-turn-position-atom (atom 2)
   :current-iteration-atom (atom 3)
   :current-form-idx-atom (atom 4)})

(defn- warnings-of
  "Helper: read accumulated `:engine/warnings` from the ctx atom on env."
  [env]
  (-> env :ctx-atom deref :engine/warnings (or [])))

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
      (it "exposes every engine mutator + satisfy-hint!"
        (expect (= #{'spec-set! 'task-set! 'fact-set!
                     'req-add! 'req-update! 'req-remove!
                     'proof-add! 'proof-remove!
                     'satisfy-hint!}
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

      (it "warning captured on `:engine/warnings`"
        (let [ws (warnings-of env)]
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
                  (warnings-of env)))))))

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

;; =============================================================================
;; Introspect verb bindings
;; =============================================================================

(defdescribe introspect-bindings-test
  (describe "build-introspect-bindings"
    (let [env (mk-env)
          {sp 'spec-set! tk 'task-set!} (cl/build-sci-bindings env)
          _   (sp :auth {:title "switch to bcrypt" :status :draft})
          _   (tk :swap {:title "CAS rewrite" :specs {:auth []} :status :doing})
          ;; History loader returns a vec of [turn ctx] pairs as if loaded
          ;; from persistance. We fake one prior turn snapshot.
          prior-ctx (-> (eng/empty-ctx "test-session")
                      (assoc :session/turn 1)
                      (assoc-in [:session/facts :rl-bug]
                        {:content "old race fact" :born "t1/i1/f1"}))
          history-loader (constantly [[1 prior-ctx]])
          {ispec 'introspect-spec
           itask 'introspect-task
           ifact 'introspect-fact
           iarch 'introspect-archived
           ictx  'introspect-ctx-at}
          (cl/build-introspect-bindings env history-loader)]

      (it "introspect-spec finds spec in LIVE ctx (this iter)"
        (let [r (ispec :auth)]
          (expect (= "switch to bcrypt" (:title r)))))

      (it "introspect-task finds task in LIVE ctx"
        (let [r (itask :swap)]
          (expect (= "CAS rewrite" (:title r)))))

      (it "introspect-fact finds fact from PRIOR turn (history)"
        (let [r (ifact :rl-bug)]
          (expect (= "old race fact" (:content r)))))

      (it "introspect-archived enumerates entries missing from latest"
        ;; the LIVE ctx (treated as latest turn 2 from env atoms) has no
        ;; :rl-bug; turn 1 had it → archived
        (let [arch (iarch :facts)]
          (expect (some #(= :rl-bug (:key %)) arch))))

      (it "introspect-ctx-at \"t1\" returns the turn 1 snapshot"
        (let [snap (ictx "t1")]
          (expect (= "old race fact"
                    (get-in snap [:session/facts :rl-bug :content])))))

      (it "introspect-ctx-at \"t99\" → nil for unknown turn"
        (expect (nil? (ictx "t99"))))

      (it "introspect-ctx-at malformed string → nil"
        (expect (nil? (ictx "bogus"))))

      (it "introspect-ctx-at accepts integer too"
        (expect (= "old race fact"
                  (get-in (ictx 1) [:session/facts :rl-bug :content])))))))

;; =============================================================================
;; trailer→form-results projection
;; =============================================================================

(defdescribe trailer-form-results-test
  (describe "trailer->form-results"
    (let [trailer [{:scope "t1/i1"
                    :forms [{:scope "t1/i1/f1" :tag :observation :src "(read)"
                             :result "ok"}
                            {:scope "t1/i1/f2" :tag :mutation :src "(write!)"
                             :result :ok}]}
                   {:scope "t1/i2"
                    :forms [{:scope "t1/i2/f1" :tag :observation :src "(/ 1 0)"
                             :error {:message "boom"}}]}
                   ;; summary entries get skipped (no :forms)
                   {:scope-start "t0/i1" :scope-end "t0/i5"
                    :summary "older summarized" :born "t1/i1/f1"}]
          fr (cl/trailer->form-results trailer)]

      (it "keys by scope-form string"
        (expect (contains? fr "t1/i1/f1"))
        (expect (contains? fr "t1/i1/f2"))
        (expect (contains? fr "t1/i2/f1")))

      (it "skips summary entries"
        (expect (= 3 (count fr))))

      (it "preserves :result for ok forms"
        (expect (= "ok" (:result (get fr "t1/i1/f1")))))

      (it "preserves :error for errored forms"
        (expect (= "boom" (:message (:error (get fr "t1/i2/f1")))))))))

;; =============================================================================
;; satisfy-hint! queue + drain-and-apply-satisfies! (D11 wiring)
;; =============================================================================

(def ^:private TITLE_VALIDATOR
  "(fn [{:keys [src error]}]
     (and (string? src)
       (clojure.string/includes? src \"set-session-title!\")
       (nil? error)))")

(defdescribe satisfy-hint-queue-test
  (describe "satisfy-hint! queues onto :engine/pending-satisfies"
    (let [env (mk-env)
          {sat 'satisfy-hint!} (cl/build-sci-bindings env)]
      (it "0-arity queues with empty scopes"
        (sat :vis.foundation/session-title)
        (expect (= [{:id :vis.foundation/session-title :scopes []}]
                  (-> env :ctx-atom deref :engine/pending-satisfies))))

      (it "1-arity (scopes vec) carries scopes through"
        (sat :other ["t1/i1/f3" "t1/i1/f5"])
        (expect (= [{:id :vis.foundation/session-title :scopes []}
                    {:id :other :scopes ["t1/i1/f3" "t1/i1/f5"]}]
                  (-> env :ctx-atom deref :engine/pending-satisfies))))

      (it "non-keyword id lands as :hint-bad-id warning, not in queue"
        (sat "string-id")
        (let [w (-> env :ctx-atom deref :engine/warnings)]
          (expect (some #(= :hint-bad-id (:code %)) w)))))))

(defdescribe drain-and-apply-satisfies-test
  (describe "drain-and-apply-satisfies! end-to-end"
    (let [env (mk-env)
          {sat 'satisfy-hint!} (cl/build-sci-bindings env)
          ;; Plant a hint with validator-fn so apply-satisfies has work.
          _ (swap! (:ctx-atom env) assoc-in
              [:session/hints :vis.foundation/session-title]
              {:body "set it" :importance :critical
               :validator-fn TITLE_VALIDATOR})
          ;; Model satisfies it with a real proof scope.
          _ (sat :vis.foundation/session-title ["t1/i1/f1"])
          ;; Form-results map (would normally be built from trailer).
          fr {"t1/i1/f1" {:scope "t1/i1/f1" :tag :mutation
                          :src "(set-session-title! \"x\")"
                          :result :vis/silent}}
          dropped (cl/drain-and-apply-satisfies! env fr)]

      (it "returns the dropped hint id set"
        (expect (= #{:vis.foundation/session-title} dropped)))

      (it "removes the hint id from :session/hints"
        (expect (nil? (get-in @(:ctx-atom env) [:session/hints :vis.foundation/session-title]))))

      (it "clears :engine/pending-satisfies after drain"
        (expect (empty? (-> env :ctx-atom deref :engine/pending-satisfies))))

      (it "second drain is a no-op (queue is empty)"
        (expect (= #{} (cl/drain-and-apply-satisfies! env fr)))))))

(defdescribe drain-failing-validator-test
  (describe "drain-and-apply-satisfies! keeps hint + emits warning on failed validator"
    (let [env (mk-env)
          {sat 'satisfy-hint!} (cl/build-sci-bindings env)
          _ (swap! (:ctx-atom env) assoc-in
              [:session/hints :vis.foundation/session-title]
              {:body "set it" :importance :critical
               :validator-fn TITLE_VALIDATOR})
          _ (sat :vis.foundation/session-title ["t1/i1/f1"])
          ;; The proof form does NOT call set-session-title!.
          fr {"t1/i1/f1" {:scope "t1/i1/f1" :tag :observation
                          :src "(v/ls \".\")" :result []}}
          dropped (cl/drain-and-apply-satisfies! env fr)]

      (it "no hint dropped"
        (expect (= #{} dropped)))

      (it "hint id still present"
        (expect (some? (get-in @(:ctx-atom env) [:session/hints :vis.foundation/session-title]))))

      (it ":hint-validator-fail warning appended to :engine/warnings"
        (expect (some #(= :hint-validator-fail (:code %))
                  (-> env :ctx-atom deref :engine/warnings)))))))
