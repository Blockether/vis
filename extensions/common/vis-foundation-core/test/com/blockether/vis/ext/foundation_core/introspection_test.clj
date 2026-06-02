(ns com.blockether.vis.ext.foundation-core.introspection-test
  (:require
   [clojure.string]
   [com.blockether.vis.ext.foundation-core.introspection :as introspection]
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
      (expect (= :v/session-state (:symbol result)))
      (expect (map? (:result result)))))

  (it "renders channel output as a {:summary :display} contract (no pr-str data dump)"
    (let [render-channel @#'introspection/session-state-channel
          result {:session-id #uuid "fc9d9b41-05d9-4099-83e8-c9abeb1ce08a"
                  :session-index [{:id 1} {:id 2}]
                  :session {:title "Reducing session-state output verbosity"}
                  :current-turn {:id #uuid "2d5226c2-32e8-4aa5-9f19-b480ea7e7cae"}
                  :failures [{}]
                  :diagnosis {:status :ok}
                  :session-forks []
                  :turn-retries {}
                  :llm-diagnostics [{} {}]
                  :transcript {:turns [{:iterations [{} {}]}]
                               :totals {:tokens {:input 10} :cost-usd 0.01}}}
          rendered-result (render-channel result)
          display (:display rendered-result)
          summary-rendered (pr-str (:summary rendered-result))
          display-rendered (pr-str display)
          has-session-badge? (some #(and (vector? %)
                                      (= :strong (first %))
                                      (clojure.string/includes? (pr-str %) "SESSION"))
                               (tree-seq sequential? seq
                                 (extension/summary->ir (:summary rendered-result))))]
      (expect (extension/render-fn-result? rendered-result))
      ;; display is the full IR body
      (expect (= :ir (first display)))
      ;; summary is a labelled zone badge with the right-anchored failure count
      (expect has-session-badge?)
      (expect (clojure.string/includes? summary-rendered "1 turn"))
      (expect (clojure.string/includes? summary-rendered "2 iter"))
      (expect (clojure.string/includes? summary-rendered "failures=1"))
      ;; full data still lives in the display, not a raw pr-str dump
      (expect (not (clojure.string/includes? display-rendered ":llm-raw-response-preview"))))))

(defdescribe render-fn-contract-test
  (it "the surviving introspection render-fns return the {:summary :display} contract"
    (let [report-result "# Report\n\nsome markdown body\n"
          channels {:report [@#'introspection/session-report-channel report-result]}]
      (doseq [[_label [render-fn arg]] channels]
        (let [r (render-fn arg)]
          (expect (extension/render-fn-result? r))
          (expect (some? (:summary r)))
          (expect (vector? (:display r)))
          (expect (= :ir (first (:display r)))))))))
