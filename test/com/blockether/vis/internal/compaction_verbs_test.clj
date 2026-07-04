(ns com.blockether.vis.internal.compaction-verbs-test
  "Raw-Python integration: drive session_fold / session_drop THROUGH the GraalPy
   sandbox so the real argument marshalling is exercised (Python list/dict →
   `->clj`), not just the pure Clojure fns. This closes the gap flagged in
   review — the `{\"through\": …}` options-dict path and the visible return value
   were only reasoned about, never executed end to end."
  (:require
   [com.blockether.vis.internal.env-python :as ep]
   [com.blockether.vis.internal.loop :as lp]
   [lazytest.core :refer [defdescribe expect it]]))

(def ^:private compaction-verbs (var-get #'lp/compaction-verbs))
(def ^:private apply-summaries  (var-get #'lp/apply-summaries))

(defn- with-verbs
  "Fresh ctx-atom + a GraalPy context with session_fold/session_drop bound.
   Returns [ctx-atom eval-fn]; eval-fn runs Python and returns the result string."
  []
  (let [ca  (atom {})
        ctx (:python-context (ep/create-python-context (compaction-verbs ca)))]
    [ca (fn [^String code] (.asString (.eval ctx "python" code)))]))

(defdescribe compaction-verbs-python-test
  (it "session_fold(list, gist): records a \"scopes\" intent + returns a visible confirmation"
    (let [[ca ev] (with-verbs)
          out (ev "session_fold([\"t1/i2\", \"t1/i3\"], \"explored auth\")")]
      (expect (= [{"scopes" #{"t1/i2" "t1/i3"} "gist" "explored auth"}]
                (get @ca "session_summaries")))
      (expect (re-find #"^folded " out))
      (expect (re-find #"explored auth" out))))

  (it "session_fold({\"through\": …}): the options DICT marshals to a \"through\" cursor"
    (let [[ca ev] (with-verbs)
          out (ev "session_fold({\"through\": \"t1/i5\"}, \"early reads\")")]
      (expect (= [{"through" "t1/i5" "gist" "early reads"}] (get @ca "session_summaries")))
      (expect (re-find #"through t1/i5" out))))

  (it "session_drop(list, reason): records \"drop\" true + the reason, returns a dropped confirmation"
    (let [[ca ev] (with-verbs)
          out (ev "session_drop([\"t1/i1\"], \"wrong file\")")]
      (expect (= [{"scopes" #{"t1/i1"} "drop" true "gist" "wrong file"}]
                (get @ca "session_summaries")))
      (expect (re-find #"^dropped " out))
      (expect (re-find #"wrong file" out))))

  (it "session_drop without a reason still records the drop (reason optional)"
    (let [[ca ev] (with-verbs)
          _ (ev "session_drop([\"t1/i1\"])")]
      (expect (= [{"scopes" #{"t1/i1"} "drop" true}] (get @ca "session_summaries")))))

  (it "an empty/blank target is a no-op: records nothing, returns a hint"
    (let [[ca ev] (with-verbs)
          out (ev "session_fold([])")]
      (expect (nil? (get @ca "session_summaries")))
      (expect (re-find #"nothing to fold" out))))

  (it "summary-of-summary through Python: a broader re-fold SUPERSEDES the finer one (ONE breadcrumb)"
    ;; Record two overlapping folds via real Python, then render the trailer: the
    ;; finer fold (i2,i3) must be superseded by the broader one (i2,i3,i4) → a
    ;; single `folded: B` line, not two stacked breadcrumbs.
    (let [[ca ev] (with-verbs)]
      (ev "session_fold([\"t1/i2\", \"t1/i3\"], \"A\")")
      (ev "session_fold([\"t1/i2\", \"t1/i3\", \"t1/i4\"], \"B\")")
      (expect (= 2 (count (get @ca "session_summaries"))))     ; both intents recorded
      (let [trailer [[1 {:forms-vec [{:scope "t1/i2/f1" :stdout "x"}]}]
                     [2 {:forms-vec [{:scope "t1/i3/f1" :stdout "y"}]}]
                     [3 {:forms-vec [{:scope "t1/i4/f1" :stdout "z"}]}]]
            out (apply-summaries trailer (get @ca "session_summaries"))
            summary-forms (mapcat (fn [[_ rec]] (filter :summary? (:forms-vec rec))) out)]
        (expect (= 1 (count summary-forms)))                   ; ONE line, not two
        (expect (= "B" (:summary-gist (first summary-forms))))  ; the broader gist won
        (expect (every? (fn [[_ rec]] (:collapsed? rec)) out))))))
