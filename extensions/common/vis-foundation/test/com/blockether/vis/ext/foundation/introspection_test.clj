(ns com.blockether.vis.ext.foundation.introspection-test
  (:require
   [clojure.string]
   [com.blockether.vis.ext.foundation.introspection :as introspection]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe introspection-public-surface-test
  (it "exposes conversation and Clojure symbol introspection symbols"
    (let [symbols (set (map :ext.symbol/symbol introspection/all-symbols))]
      (expect (contains? symbols 'conversation-state))
      (expect (contains? symbols 'conversation-report))
      (expect (contains? symbols 'engine-symbol-documentation))
      (expect (contains? symbols 'engine-symbol-source-code))
      (expect (contains? symbols 'engine-symbol-metadata))
      (expect (contains? symbols 'engine-symbol-apropos))
      (expect (= 6 (count symbols))))))

(defdescribe conversation-state-envelope-test
  (it "returns a canonical envelope so observed symbol wrapping can unwrap it"
    (let [inspect @#'introspection/foundation-inspect
          result  (inspect {:conversation-id nil :db-info nil})]
      (expect (extension/tool-result? result))
      (expect (= :v/conversation-state (:symbol result)))
      (expect (map? (:result result)))))

  (it "renders channel output as compact IR instead of pr-str data dump"
    (let [render-channel @#'introspection/conversation-state-channel
          result {:conversation-id #uuid "fc9d9b41-05d9-4099-83e8-c9abeb1ce08a"
                  :conversation-index [{:id 1} {:id 2}]
                  :conversation {:title "Reducing conversation-state output verbosity"}
                  :current-turn {:id #uuid "2d5226c2-32e8-4aa5-9f19-b480ea7e7cae"}
                  :failures [{}]
                  :diagnosis {:status :ok}
                  :conversation-forks []
                  :turn-retries {}
                  :llm-diagnostics [{} {}]
                  :transcript {:turns [{:iterations [{} {}]}]
                               :totals {:tokens {:input 10} :cost-usd 0.01}}}
          ir (render-channel result)
          rendered (pr-str ir)]
      (expect (= :ir (first ir)))
      (expect (clojure.string/includes? rendered "summary only"))
      (expect (not (clojure.string/includes? rendered ":llm-raw-response-preview"))))))
