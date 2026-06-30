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

(defn- with-verbs
  "Fresh ctx-atom + a GraalPy context with session_fold/session_drop bound.
   Returns [ctx-atom eval-fn]; eval-fn runs Python and returns the result string."
  []
  (let [ca  (atom {})
        ctx (:python-context (ep/create-python-context (compaction-verbs ca)))]
    [ca (fn [^String code] (.asString (.eval ctx "python" code)))]))

(defdescribe compaction-verbs-python-test
  (it "session_fold(list, gist): records a :scopes intent + returns a visible confirmation"
    (let [[ca ev] (with-verbs)
          out (ev "session_fold([\"t1/i2\", \"t1/i3\"], \"explored auth\")")]
      (expect (= [{:scopes #{"t1/i2" "t1/i3"} :gist "explored auth"}]
                (:session/summaries @ca)))
      (expect (re-find #"^folded " out))
      (expect (re-find #"explored auth" out))))

  (it "session_fold({\"through\": …}): the options DICT marshals to a :through cursor"
    (let [[ca ev] (with-verbs)
          out (ev "session_fold({\"through\": \"t1/i5\"}, \"early reads\")")]
      (expect (= [{:through "t1/i5" :gist "early reads"}] (:session/summaries @ca)))
      (expect (re-find #"through t1/i5" out))))

  (it "session_drop(list, reason): records :drop? true + the reason, returns a dropped confirmation"
    (let [[ca ev] (with-verbs)
          out (ev "session_drop([\"t1/i1\"], \"wrong file\")")]
      (expect (= [{:scopes #{"t1/i1"} :drop? true :gist "wrong file"}]
                (:session/summaries @ca)))
      (expect (re-find #"^dropped " out))
      (expect (re-find #"wrong file" out))))

  (it "session_drop without a reason still records the drop (reason optional)"
    (let [[ca ev] (with-verbs)
          _ (ev "session_drop([\"t1/i1\"])")]
      (expect (= [{:scopes #{"t1/i1"} :drop? true}] (:session/summaries @ca)))))

  (it "an empty/blank target is a no-op: records nothing, returns a hint"
    (let [[ca ev] (with-verbs)
          out (ev "session_fold([])")]
      (expect (nil? (:session/summaries @ca)))
      (expect (re-find #"nothing to fold" out)))))
