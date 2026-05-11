(ns com.blockether.vis.ext.bridge.doctor-test
  (:require
   [com.blockether.vis.ext.bridge.doctor :as doctor]
   [com.blockether.vis.ext.bridge.languages.clojure-lsp :as lsp]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe bridge-doctor-test
  (it "returns CommonMark and clojure-lsp readiness messages"
    (with-redefs-fn {#'lsp/executable-status
                     (constantly {:available? true
                                  :command "clojure-lsp"
                                  :path "/bin/clojure-lsp"
                                  :version "clojure-lsp test"})}
      #(let [msgs (doctor/check-fn {})]
         (expect (= #{::doctor/commonmark ::doctor/clojure-lsp}
                   (set (map :check-id msgs))))
         (expect (every? #{:info} (map :level msgs))))))

  (it "warns when external clojure-lsp is missing"
    (with-redefs-fn {#'lsp/executable-status
                     (constantly {:available? false :command "clojure-lsp"})}
      (fn []
        (let [msgs (doctor/check-fn {})]
          (expect (some (fn [msg]
                          (and (= ::doctor/clojure-lsp (:check-id msg))
                            (= :warn (:level msg))))
                    msgs)))))))
