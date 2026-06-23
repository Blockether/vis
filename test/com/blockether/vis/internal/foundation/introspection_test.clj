(ns com.blockether.vis.internal.foundation.introspection-test
  (:require
   [com.blockether.vis.internal.foundation.introspection :as introspection]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe introspection-public-surface-test
  (it "exposes only the session introspection symbols (symbol docs moved to engine `doc`/`apropos`)"
    (let [symbols (set (map :ext.symbol/symbol introspection/all-symbols))]
      (expect (contains? symbols 'session-state))
      (expect (contains? symbols 'session-report))
      ;; engine-symbol-* tools were retired in favour of the bare
      ;; `doc` / `apropos` engine system calls.
      (expect (not (contains? symbols 'engine-symbol-documentation)))
      (expect (not (contains? symbols 'engine-symbol-apropos)))
      (expect (= 2 (count symbols))))))

(defdescribe session-state-envelope-test
  (it "returns a canonical envelope so observed symbol wrapping can unwrap it"
    (let [inspect @#'introspection/foundation-inspect
          result  (inspect {:session-id nil :db-info nil})]
      (expect (extension/tool-result? result))
      (expect (= :session-state (:symbol result)))
      (expect (map? (:result result))))))
