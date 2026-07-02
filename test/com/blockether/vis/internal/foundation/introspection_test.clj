(ns com.blockether.vis.internal.foundation.introspection-test
  (:require
   [com.blockether.vis.internal.foundation.introspection :as introspection]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe introspection-public-surface-test
  (it "exposes only the session introspection symbols (symbol docs moved to engine `doc`/`apropos`)"
    (let [symbols (set (map :ext.symbol/symbol introspection/all-symbols))]
      (expect (contains? symbols 'session-state))
      (expect (contains? symbols 'session-report-md))
      (expect (contains? symbols 'sessions))
      ;; engine-symbol-* tools were retired in favour of the bare
      ;; `doc` / `apropos` engine system calls.
      (expect (not (contains? symbols 'engine-symbol-documentation)))
      (expect (not (contains? symbols 'engine-symbol-apropos)))
      (expect (= 3 (count symbols))))))

(defdescribe session-state-envelope-test
  (it "returns a canonical envelope so observed symbol wrapping can unwrap it"
    (let [inspect @#'introspection/foundation-inspect
          result  (inspect {:session-id nil :db-info nil})]
      (expect (extension/tool-result? result))
      (expect (= :session-state (:symbol result)))
      (expect (map? (:result result))))))

(defdescribe sessions-envelope-test
  ;; Regression (session 9c829d10): `sessions()` was the ONE introspection
  ;; verb without the `session-envelope` wrap — it returned the raw vector,
  ;; so `assert-symbol-envelope!` rejected EVERY call ("Symbol 'sessions'
  ;; must return a canonical :envelope map").
  (it "no-arg arity returns a canonical envelope (empty index without a db)"
    (let [sessions @#'introspection/foundation-sessions
          result   (sessions {:session-id nil :db-info nil})]
      (expect (extension/tool-result? result))
      (expect (= :sessions (:symbol result)))
      (expect (= [] (:result result)))))
  (it "channel-filtered arity is enveloped too"
    (let [sessions @#'introspection/foundation-sessions
          result   (sessions {:db-info nil} :tui)]
      (expect (extension/tool-result? result))
      (expect (= [] (:result result))))))
