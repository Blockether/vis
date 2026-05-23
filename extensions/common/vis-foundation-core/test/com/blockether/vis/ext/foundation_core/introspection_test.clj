(ns com.blockether.vis.ext.foundation-core.introspection-test
  (:require
   [clojure.string]
   [com.blockether.vis.ext.foundation-core.introspection :as introspection]
   [com.blockether.vis.internal.extension :as extension]
   [lazytest.core :refer [defdescribe expect it]]
   [sci.core :as sci]))

(defdescribe introspection-public-surface-test
  (it "exposes session and Clojure symbol introspection symbols"
    (let [symbols (set (map :ext.symbol/symbol introspection/all-symbols))]
      (expect (contains? symbols 'session-state))
      (expect (contains? symbols 'session-report))
      (expect (contains? symbols 'engine-symbol-documentation))
      (expect (contains? symbols 'engine-symbol-source-code))
      (expect (contains? symbols 'engine-symbol-metadata))
      (expect (contains? symbols 'engine-symbol-apropos))
      (expect (= 6 (count symbols)))))

  (it "documents quoted aliased SCI symbols"
    (let [git-ns  (sci/create-ns 'vis.ext.git)
          sci-ctx (sci/init {:namespaces {'vis.ext.git {'diff (sci/new-var 'diff
                                                                (fn [] nil)
                                                                {:ns git-ns
                                                                 :doc "diff docs"
                                                                 :arglists '([])})}}
                             :ns-aliases {'git 'vis.ext.git}})
          tool    @#'introspection/engine-symbol-documentation-tool
          result  (tool {:sci-ctx sci-ctx} 'git/diff)]
      (expect (extension/tool-result? result))
      (expect (get-in result [:result :found?]))
      (expect (= 'git/diff (get-in result [:result :symbol])))
      (expect (= 'vis.ext.git/diff (get-in result [:result :resolved-symbol])))
      (expect (= "diff docs" (get-in result [:result :doc]))))))

(defdescribe session-state-envelope-test
  (it "returns a canonical envelope so observed symbol wrapping can unwrap it"
    (let [inspect @#'introspection/foundation-inspect
          result  (inspect {:session-id nil :db-info nil})]
      (expect (extension/tool-result? result))
      (expect (= :v/session-state (:symbol result)))
      (expect (map? (:result result)))))

  (it "renders channel output as a badged SESSION IR (no pr-str data dump)"
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
          ir (render-channel result)
          rendered (pr-str ir)
          has-session-badge? (some #(and (vector? %)
                                      (= :strong (first %))
                                      (= "SESSION" (last %)))
                               (tree-seq sequential? seq ir))]
      (expect (= :ir (first ir)))
      (expect has-session-badge?)
      ;; header carries turn / iter / failure stats inline
      (expect (clojure.string/includes? rendered "1 turn"))
      (expect (clojure.string/includes? rendered "2 iter"))
      (expect (clojure.string/includes? rendered "failures=1"))
      (expect (not (clojure.string/includes? rendered ":llm-raw-response-preview"))))))
