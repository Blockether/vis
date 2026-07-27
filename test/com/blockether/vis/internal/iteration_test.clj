(ns com.blockether.vis.internal.iteration-test
  "Canonical iteration-entry projections — block-level scope / merged code /
   status / duration / error, plus the stdout display surface.

   The high-fan-out `:ops` policy (sink-entry->op, aggregate-ops, batch
   hints, display blocks) was removed alongside the render-fn / op-card
   system: tool output is now shown purely as the program's stdout. What
   remains are the PURE projections off an iteration entry that already
   carries `:forms`."
  (:require [com.blockether.vis.internal.iteration :as iteration]
            [lazytest.core :refer [defdescribe expect it]]))

;; ---------------------------------------------------------------------------
;; Fixture: a plain iteration entry carrying `:forms` (no ops/sink shape).
;; ---------------------------------------------------------------------------

(defn- form [m] (merge {:tag :observation} m))

(defn- entry [forms] {:position 0 :forms (vec forms)})

;; ---------------------------------------------------------------------------
;; Block-level projections: scope / code / status / duration / error.
;; ---------------------------------------------------------------------------

(defdescribe block-scope-test
             (it "reduces a form/op scope to its block-level tN/iM"
                 (expect (= "t1/i2" (iteration/block-scope "t1/i2/f3")))
                 (expect (= "t1/i2" (iteration/block-scope "t1/i2")))
                 (expect (nil? (iteration/block-scope nil)))))

(defdescribe
  canonicalize-test
  (it "derives the block scope from the first form that carries one"
      (let [e (iteration/canonicalize (entry [(form {:scope "t1/i1/f1" :code "(cat \"a\")"})]))]
        (expect (= "t1/i1" (:scope e)))))
  (it "merges visible form sources into the block :code in order"
      (let [e (iteration/canonicalize (entry [(form {:code "(cat \"a\")"})
                                              (form {:code "(cat \"b\")"})]))]
        (expect (= "(cat \"a\")\n(cat \"b\")" (:code e)))))
  (it "sums per-form durations into block :duration-ms"
      (let [e (iteration/canonicalize (entry [(form {:duration-ms 100})
                                              (form {:duration-ms 250})]))]
        (expect (= 350 (:duration-ms e)))))
  (it "is idempotent"
      (let [e1
            (iteration/canonicalize (entry [(form {:scope "t1/i1/f1" :code "(x)" :duration-ms 5})]))

            e2
            (iteration/canonicalize e1)]

        (expect (= e1 e2)))))

(defdescribe
  entry-status-test
  (it ":ok when no error and no running form"
      (expect (= :ok (iteration/entry-status (entry [(form {:success? true})])))))
  (it ":error when any form carries an error"
      (expect (= :error
                 (iteration/entry-status (entry [(form {:success? true})
                                                 (form {:error {:message "boom"}})])))))
  (it ":error when the iter-level error is set"
      (expect (= :error (iteration/entry-status (assoc (entry []) :error {:message "boom"})))))
  (it ":running when a started form has no success? yet"
      (expect (= :running
                 (iteration/entry-status (entry [(form {:started-at-ms 123 :success? nil})]))))))

(defdescribe entry-error-test
             (it "prefers the iter-level error, else the first errored form"
                 (expect (= {:message "iter"}
                            (iteration/entry-error (assoc (entry [(form {:error {:message "f"}})])
                                                     :error {:message "iter"}))))
                 (expect (= {:message "f"}
                            (iteration/entry-error (entry [(form {:success? true})
                                                           (form {:error {:message "f"}})]))))))

(defdescribe op-status-test
             (it "maps per-op success?/error to the status enum"
                 (expect (= :ok (iteration/op-status {:success? true})))
                 (expect (= :error (iteration/op-status {:success? false})))
                 (expect (= :error (iteration/op-status {:error {:message "x"}})))))
