(ns com.blockether.vis.internal.lifecycle-test
  "Coverage for the lifecycle event bus. Pure module \u2014 no router, no
   SQLite, no SCI; just composition + broadcast. The integration with
   the real iteration loop is exercised separately."
  (:require
   [com.blockether.vis.internal.lifecycle :as lc]
   [lazytest.core :refer [defdescribe it expect]]))

(defn- mk-ext
  "Fixture extension carrying any subset of lifecycle listener fns."
  [ns-sym & {:as listeners}]
  (merge {:ext/namespace ns-sym} listeners))

;; ---------------------------------------------------------------------------
;; compose-listeners \u2014 the composition contract.
;; ---------------------------------------------------------------------------

(defdescribe compose-listeners-test
  (it "returns one entry per canonical phase, even when nothing subscribed"
    (let [out (lc/compose-listeners nil [])]
      (expect (= (set lc/phases) (set (keys out))))
      (doseq [p lc/phases]
        (expect (= [] (get out p))))))

  (it "wires per-call hooks under the matching phase"
    (let [f1     (fn [_payload])
          out    (lc/compose-listeners {:on-iteration-end f1} [])]
      (expect (= [f1] (:iteration-end out)))
      (expect (= [] (:iteration-start out)))))

  (it "accepts a single fn OR a coll of fns under a hook slot"
    (let [a (fn [_])
          b (fn [_])
          c (fn [_])
          single (lc/compose-listeners {:on-turn-end a} [])
          many   (lc/compose-listeners {:on-turn-end [a b c]} [])]
      (expect (= [a]      (:turn-end single)))
      (expect (= [a b c]  (:turn-end many)))))

  (it "drops non-fn entries from a coll without crashing"
    (let [a (fn [_])
          out (lc/compose-listeners {:on-iteration-end [a "not-a-fn" nil 42]} [])]
      (expect (= [a] (:iteration-end out)))))

  (it "wires extension manifest fns under the matching phase"
    (let [ext-fn (fn [_])
          ext    (mk-ext 'fixture.ext :ext/on-iteration-end-fn ext-fn)
          out    (lc/compose-listeners nil [ext])]
      (expect (= [ext-fn] (:iteration-end out)))))

  (it "fans out across multiple extensions in registration order"
    (let [a (fn [_])
          b (fn [_])
          c (fn [_])
          out (lc/compose-listeners nil
                [(mk-ext 'a :ext/on-iteration-end-fn a)
                 (mk-ext 'b :ext/on-iteration-end-fn b)
                 (mk-ext 'c :ext/on-iteration-end-fn c)])]
      (expect (= [a b c] (:iteration-end out)))))

  (it "fires per-call listeners BEFORE extension listeners"
    ;; Channels run first so the user-facing UI updates immediately,
    ;; then extension side effects (logging, billing) trail. This
    ;; ordering is part of the contract; downstream tests rely on it.
    (let [hook (fn [_])
          ext1 (fn [_])
          ext2 (fn [_])
          out  (lc/compose-listeners {:on-iteration-end hook}
                 [(mk-ext 'a :ext/on-iteration-end-fn ext1)
                  (mk-ext 'b :ext/on-iteration-end-fn ext2)])]
      (expect (= [hook ext1 ext2] (:iteration-end out)))))

  (it "wires every phase independently \u2014 unrelated phases stay empty"
    (let [ts  (fn [_])
          is  (fn [_])
          ie  (fn [_])
          te  (fn [_])
          out (lc/compose-listeners {:on-turn-start ts
                                     :on-iteration-start is
                                     :on-iteration-end ie
                                     :on-turn-end te}
                [])]
      (expect (= [ts] (:turn-start out)))
      (expect (= [is] (:iteration-start out)))
      (expect (= [ie] (:iteration-end out)))
      (expect (= [te] (:turn-end out))))))

;; ---------------------------------------------------------------------------
;; emit! \u2014 broadcast + isolation.
;; ---------------------------------------------------------------------------

(defdescribe emit-test
  (it "fires every listener for the given phase, in order"
    (let [seen (atom [])
          a    (fn [p] (swap! seen conj [:a (:phase p)]))
          b    (fn [p] (swap! seen conj [:b (:phase p)]))
          c    (fn [p] (swap! seen conj [:c (:phase p)]))]
      (lc/emit! (lc/compose-listeners {:on-iteration-end [a b c]} [])
        :iteration-end {:iteration 7})
      (expect (= [[:a :iteration-end] [:b :iteration-end] [:c :iteration-end]] @seen))))

  (it "stamps :phase on every payload"
    (let [seen (atom nil)
          f    (fn [p] (reset! seen p))]
      (lc/emit! (lc/compose-listeners {:on-turn-start f} [])
        :turn-start {:conversation-id "abc"})
      (expect (= :turn-start (:phase @seen)))
      (expect (= "abc" (:conversation-id @seen)))))

  (it "passes the payload through verbatim besides the :phase stamp"
    (let [seen (atom nil)
          f    (fn [p] (reset! seen p))
          payload {:conversation-id "c1" :query-id "q1" :iteration 3
                   :status :success :cost-usd 0.0042
                   :tokens {:input 100 :output 20}}]
      (lc/emit! (lc/compose-listeners {:on-iteration-end f} [])
        :iteration-end payload)
      (expect (= (assoc payload :phase :iteration-end) @seen))))

  (it "isolates listener exceptions \u2014 a thrower does NOT abort the broadcast"
    (let [seen   (atom [])
          good-1 (fn [_] (swap! seen conj :good-1))
          bad    (fn [_] (throw (ex-info "boom" {})))
          good-2 (fn [_] (swap! seen conj :good-2))]
      (lc/emit! (lc/compose-listeners {:on-iteration-end [good-1 bad good-2]} [])
        :iteration-end {})
      (expect (= [:good-1 :good-2] @seen))))

  (it "is a no-op when no listener subscribed to that phase"
    ;; If the phase had zero listeners, payload work shouldn't even
    ;; happen \u2014 the loop hot-path optimises around `(seq listeners)`.
    (let [out (lc/emit! (lc/compose-listeners nil []) :iteration-end {})]
      (expect (nil? out)))))
