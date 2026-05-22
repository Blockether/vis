(ns com.blockether.vis.internal.ctx-spec-test
  "Property-based round-trip tests for `ctx-spec/*` shapes.

   For every spec, two checks run:
     - **generator round-trip** — `gen/sample` 25 times; every value must
       `s/valid?` against its own spec. If a generated value fails validation,
       either the spec is wrong OR the generator drifted.
     - **negative cases** — explicit invalid examples must `s/valid?` => false
       on the spec they violate. Documents the failure surface.

   Coverage:
     ::scope-form ::scope-iter ::scope-turn
     ::fact ::task ::spec
     ::trailer-form ::trailer-pin ::trailer-summary ::trailer-entry
     ::workspace ::symbol-info ::hint
     :session/scope (cursor) ::ctx (top-level)"
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [com.blockether.vis.internal.ctx-spec :as cs]
   [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private sample-count
  "How many random samples per spec for the round-trip pass.
   Higher = stronger property check, slower test."
  25)

(defn- round-trip-valid?
  "True iff every generated sample for `spec-kw` validates against itself.
   Catches generator/spec drift."
  [spec-kw]
  (every? #(s/valid? spec-kw %) (gen/sample (s/gen spec-kw) sample-count)))

(defdescribe scope-coord-test
  (describe "::scope-form, ::scope-iter, ::scope-turn — regex-validated string coords"
    (it "scope-form round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/scope-form)))
    (it "scope-iter round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/scope-iter)))
    (it "scope-turn round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/scope-turn)))

    (it "rejects leading zero"
      (expect (not (s/valid? ::cs/scope-form "t01/i1/f1"))))
    (it "rejects zero turn"
      (expect (not (s/valid? ::cs/scope-form "t0/i1/f1"))))
    (it "rejects missing prefix"
      (expect (not (s/valid? ::cs/scope-form "3/2/1"))))
    (it "rejects iter scope as form"
      (expect (not (s/valid? ::cs/scope-form "t3/i2"))))
    (it "rejects form scope as iter"
      (expect (not (s/valid? ::cs/scope-iter "t3/i2/f1"))))
    (it "rejects iter scope as turn"
      (expect (not (s/valid? ::cs/scope-turn "t3/i2"))))))

(defdescribe fact-test
  (describe "::fact — {:content :born}; no labels or edge fields"
    (it "round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/fact)))

    (it "minimal valid"
      (expect (s/valid? ::cs/fact {:content "x" :born "t1/i1/f1"})))

    (it "rejects missing :content"
      (expect (not (s/valid? ::cs/fact {:born "t1/i1/f1"}))))

    (it "rejects missing :born"
      (expect (not (s/valid? ::cs/fact {:content "x"}))))

    (it "rejects bad :born scope"
      (expect (not (s/valid? ::cs/fact {:content "x" :born "bad-scope"}))))

    (it "allows unknown keys (open schema)"
      (expect (s/valid? ::cs/fact
                {:content "x" :born "t1/i1/f1" :random-field 42})))))

(defdescribe task-test
  (describe "::task — {:title :specs :status :born + optional :depends-on}"
    (it "round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/task)))

    (it "minimal valid"
      (expect (s/valid? ::cs/task
                {:title "x" :specs {:the-spec []} :status :todo :born "t1/i1/f1"})))

    (it "allows :depends-on vec"
      (expect (s/valid? ::cs/task
                {:title "x" :specs {:s []} :depends-on [:a]
                 :status :todo :born "t1/i1/f1"})))

    (it "allows :specs proof entries"
      (expect (s/valid? ::cs/task
                {:title "x" :specs {:s [{:requirement :r1
                                         :proof "t1/i2/f3"}]}
                 :status :done :born "t1/i1/f1"})))

    (it "rejects missing :specs (required proofs map)"
      (expect (not (s/valid? ::cs/task
                     {:title "x" :status :todo :born "t1/i1/f1"}))))

    (it "rejects :blocked status (dropped from enum)"
      (expect (not (s/valid? ::cs/task
                     {:title "x" :specs {:s []} :status :blocked :born "t1/i1/f1"}))))

    (it "allows unknown keys (open schema)"
      (expect (s/valid? ::cs/task
                {:title "x" :specs {:s []} :status :done :born "t1/i1/f1"
                 :extra "ok"})))

    (it "has no :journal field — status history lives in trailer"
      (expect (s/valid? ::cs/task
                {:title "x" :specs {:s []} :status :done :born "t1/i1/f1"})))))

(defdescribe spec-test
  (describe "::spec — {:title :requirements :status :born}"
    (it "round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/spec)))

    (it "minimal valid"
      (expect (s/valid? ::cs/spec
                {:title "x"
                 :requirements [{:id :r1
                                 :title "requirement"}]
                 :status :draft
                 :born "t1/i1/f1"})))

    (it "allows requirement facts and validator-fn"
      (expect (s/valid? ::cs/spec
                {:title "x"
                 :requirements [{:id :r1
                                 :title "requirement"
                                 :facts [:f1]
                                 :validator-fn "(fn [{:keys [result]}] (:ok? result))"}]
                 :status :doing
                 :born "t1/i1/f1"})))

    (it "rejects empty :requirements"
      (expect (not (s/valid? ::cs/spec
                     {:title "x" :requirements []
                      :status :draft :born "t1/i1/f1"}))))

    (it "rejects bad :status value"
      (expect (not (s/valid? ::cs/spec
                     {:title "x"
                      :requirements [{:id :r1 :title "r"}]
                      :status :wip :born "t1/i1/f1"}))))

    (it "allows unknown keys (open schema)"
      (expect (s/valid? ::cs/spec
                {:title "x"
                 :requirements [{:id :r1 :title "r"}]
                 :status :draft :born "t1/i1/f1"
                 :extra "ok"})))))

(defdescribe trailer-test
  (describe "::trailer-form, ::trailer-pin, ::trailer-summary, ::trailer-entry"
    (it "trailer-form round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/trailer-form)))
    (it "trailer-pin round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/trailer-pin)))
    (it "trailer-summary round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/trailer-summary)))
    (it "trailer-entry (union) round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/trailer-entry)))

    (it "rejects pin with empty :forms"
      (expect (not (s/valid? ::cs/trailer-pin
                     {:scope "t3/i2" :forms []}))))

    (it "rejects pin with form-scope on iter slot"
      (expect (not (s/valid? ::cs/trailer-pin
                     {:scope "t3/i2/f1"
                      :forms [{:scope "t3/i2/f1" :tag :observation :src "x"}]}))))

    (it "rejects unknown :tag value"
      (expect (not (s/valid? ::cs/trailer-form
                     {:scope "t3/i2/f1" :tag :weird :src "x"}))))

    (it "summary requires :scope-start, :scope-end, :summary, :born"
      (expect (not (s/valid? ::cs/trailer-summary
                     {:scope-start "t1/i1" :summary "x"}))))

    (it "summary rejects form-scope as :scope-start (must be iter)"
      (expect (not (s/valid? ::cs/trailer-summary
                     {:scope-start "t1/i1/f1" :scope-end "t3/i2"
                      :summary "x" :born "t4/i1/f1"}))))

    (it "trailer-entry as pin works"
      (expect (s/valid? ::cs/trailer-entry
                {:scope "t3/i2"
                 :forms [{:scope "t3/i2/f1" :tag :observation :src "x"}]})))

    (it "trailer-entry as summary works"
      (expect (s/valid? ::cs/trailer-entry
                {:scope-start "t1/i1" :scope-end "t5/i3"
                 :summary "x" :born "t6/i1/f1"})))

    (it "trailer-entry as neither (pure :scope) rejected"
      (expect (not (s/valid? ::cs/trailer-entry {:scope "t3/i2"}))))))

(defdescribe workspace-test
  (describe "::workspace — {:git/branch :git/trunk :git/head :git/dirty? :git/stats}"
    (it "round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/workspace)))

    (it "minimal valid"
      (expect (s/valid? ::cs/workspace
                {:git/branch "main" :git/trunk "main" :git/head "abc"
                 :git/dirty? false :git/stats {}})))

    (it "rejects old flat keys"
      (expect (not (s/valid? ::cs/workspace
                     {:branch "main" :trunk "main" :head "x"
                      :dirty? false :stats {}}))))

    (it "rejects negative :added in stats"
      (expect (not (s/valid? ::cs/workspace
                     {:git/branch "x" :git/trunk "y" :git/head "z"
                      :git/dirty? false
                      :git/stats {"a.clj" {:added -1 :removed 0}}}))))))

(defdescribe symbol-info-test
  (describe "::symbol-info — {:born :arglists? :doc?}"
    (it "round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/symbol-info)))

    (it "minimal valid (only :born)"
      (expect (s/valid? ::cs/symbol-info {:born "t1/i1/f1"})))

    (it "rejects missing :born"
      (expect (not (s/valid? ::cs/symbol-info {:arglists '(([x]))}))))

    (it "omits :doc when no docstring (engine never emits :doc nil)"
      (expect (s/valid? ::cs/symbol-info {:born "t1/i1/f1"})))

    (it "rejects :doc nil — :doc is a non-nil string or omitted entirely"
      (expect (not (s/valid? ::cs/symbol-info {:doc nil :born "t1/i1/f1"}))))

    (it "accepts non-blank :doc string"
      (expect (s/valid? ::cs/symbol-info {:doc "what it does" :born "t1/i1/f1"})))))

(defdescribe hint-test
  (describe "::hint — {:body :importance? :satisfy-with?}"
    (it "round-trips for 25 samples"
      (expect (round-trip-valid? ::cs/hint)))

    (it "rejects bad :importance value"
      (expect (not (s/valid? ::cs/hint {:body "do X" :importance :ultra}))))))

(defdescribe scope-cursor-test
  (describe ":session/scope — engine-rendered cursor {:turn :iter :next-form}"
    (it "round-trips for 25 samples"
      (expect (round-trip-valid? :session/scope)))

    (it "minimal valid"
      (expect (s/valid? :session/scope {:turn 1 :iter 1 :next-form 1})))

    (it "rejects :turn 0 (must be pos-int)"
      (expect (not (s/valid? :session/scope {:turn 0 :iter 1 :next-form 1}))))

    (it "rejects :iter 0"
      (expect (not (s/valid? :session/scope {:turn 1 :iter 0 :next-form 1}))))

    (it "rejects :next-form 0 (must be 1-based)"
      (expect (not (s/valid? :session/scope {:turn 1 :iter 1 :next-form 0}))))

    (it "rejects missing :next-form"
      (expect (not (s/valid? :session/scope {:turn 1 :iter 1}))))

    (it "allows unknown keys (open schema)"
      (expect (s/valid? :session/scope
                {:turn 1 :iter 1 :next-form 1 :extra "ok"})))))

(defdescribe ctx-test
  (describe "::ctx — top-level"
    (it "minimal valid ctx"
      (expect (s/valid? ::cs/ctx
                {:session/id        "01HXYZ"
                 :session/turn      1
                 :session/scope     {:turn 1 :iter 1 :next-form 1}
                 :session/workspace {:git/branch "main" :git/trunk "main"
                                     :git/head "x" :git/dirty? false :git/stats {}}
                 :session/symbols   {}
                 :session/hints     {}
                 :session/specs     {}
                 :session/tasks     {}
                 :session/facts     {}
                 :session/trailer   []})))

    (it "rejects missing :session/scope cursor (required for proof orientation)"
      (expect (not (s/valid? ::cs/ctx
                     {:session/id "x" :session/turn 1
                      :session/workspace {:git/branch "main" :git/trunk "main"
                                          :git/head "x" :git/dirty? false :git/stats {}}
                      :session/symbols {} :session/hints {}
                      :session/specs {} :session/tasks {} :session/facts {}
                      :session/trailer []}))))

    (it "rejects missing required top-level key"
      (expect (not (s/valid? ::cs/ctx
                     {:session/id "x" :session/turn 1
                      :session/scope {:turn 1 :iter 1 :next-form 1}
                      :session/workspace {:git/branch "main" :git/trunk "main"
                                          :git/head "x" :git/dirty? false :git/stats {}}
                      :session/symbols {} :session/hints {}
                      :session/specs {} :session/tasks {}}))))

    (it "rejects :session/turn 0 (must be pos-int)"
      (expect (not (s/valid? ::cs/ctx
                     {:session/id "x" :session/turn 0
                      :session/scope {:turn 1 :iter 1 :next-form 1}
                      :session/workspace {:git/branch "main" :git/trunk "main"
                                          :git/head "x" :git/dirty? false :git/stats {}}
                      :session/symbols {} :session/hints {}
                      :session/specs {} :session/tasks {} :session/facts {}
                      :session/trailer []}))))

    (it "validates a realistic mixed ctx"
      (expect (s/valid? ::cs/ctx
                {:session/id        "01HXYZ"
                 :session/turn      7
                 :session/scope     {:turn 7 :iter 3 :next-form 1}
                 :session/workspace {:git/branch "feat/x" :git/trunk "main"
                                     :git/head "abc1234" :git/dirty? true
                                     :git/stats {"src/a.clj" {:added 5 :removed 2}}}
                 :session/symbols
                 {'auth-check {:arglists '(([tok])) :doc "literal-compare check"
                               :born "t5/i1/f1"}
                  'emit-event {:arglists '(([{:keys [level msg]}]))
                               :born "t4/i1/f1"}}
                 :session/hints     {}
                 :session/specs
                 {:auth {:title "switch auth to bcrypt"
                         :requirements [{:id :bcrypt-check
                                         :title "check/1 uses bcrypt"
                                         :facts [:auth-fact]
                                         :validator-fn "(fn [{:keys [result]}] (:ok? result))"}]
                         :status :doing
                         :born "t5/i1/f1"}}
                 :session/tasks
                 {:add-dep {:title "add bcrypt"
                            :specs {:auth [{:requirement :bcrypt-check
                                            :proof "t5/i2/f1"}]}
                            :depends-on []
                            :status :done
                            :born "t5/i1/f2"}}
                 :session/facts
                 {:auth-fact {:content "auth.clj uses literal compare" :born "t3/i2/f1"}}
                 :session/trailer
                 [{:scope "t3/i2"
                   :forms [{:scope "t3/i2/f1" :tag :observation
                            :src "(v/cat \"src/auth.clj\")"
                            :result "(ns auth) …"}]}
                  {:scope-start "t6/i1" :scope-end "t6/i2"
                   :summary "verified bcrypt ok; tests green"
                   :born "t6/i3/f1"}]})))))
