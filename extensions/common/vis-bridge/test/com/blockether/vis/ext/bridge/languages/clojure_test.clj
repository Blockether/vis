(ns com.blockether.vis.ext.bridge.languages.clojure-test
  (:require
   [com.blockether.vis.ext.bridge.languages.clojure :as clj]
   [com.blockether.vis.ext.bridge.languages.schema :as schema]
   [lazytest.core :refer [defdescribe expect it]]))

(def sample-dump
  {:source-paths ["src"]
   :analysis
   {"file:///repo/src/demo/core.clj"
    {:namespace-definitions
     [{:name 'demo.core :uri "file:///repo/src/demo/core.clj" :row 1 :end-row 2 :bucket :namespace-definitions}]
     :namespace-usages
     [{:from 'demo.core :name 'clojure.string :alias 'str :uri "file:///repo/src/demo/core.clj" :row 2 :bucket :namespace-usages}]
     :var-definitions
     [{:ns 'demo.core :name 'run :defined-by 'clojure.core/defn :uri "file:///repo/src/demo/core.clj" :row 4 :end-row 5 :bucket :var-definitions}]
     :var-usages
     [{:from 'demo.core :to 'clojure.string :name 'trim :uri "file:///repo/src/demo/core.clj" :row 4 :col 18 :bucket :var-usages}]}}
   :dep-graph
   {'demo.core {:dependencies {'clojure.string 1}}}})

(defdescribe clojure-extractor-test
  (it "reports executable status shape without throwing"
    (let [status (clj/executable-status {:command "definitely-not-clojure-lsp-bridge-test"})]
      (expect (false? (:available? status)))
      (expect (= "definitely-not-clojure-lsp-bridge-test" (:command status)))))

  (it "converts clojure-lsp dump data into normalized Bridge facts"
    (let [result (clj/extract-project {:project-root "/repo" :dump sample-dump})]
      (expect (schema/valid-extract-result? result))
      (expect (some #(= "demo.core/run" (:qualified-name %)) (:nodes result)))
      (expect (some #(and (= :imports (:edge-kind %))
                       (= "clojure.string" (:target %)))
                (:edges result)))
      (expect (= :clojure-lsp/external-cli (get-in result [:stats :backend]))))))
