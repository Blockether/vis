(ns com.blockether.vis.loop.knowledge.pageindex.core-test
  (:require [com.blockether.vis.loop.knowledge.pageindex.core :as sut]
            [lazytest.core :refer [defdescribe describe expect it throws?]]))

(defdescribe index-core-test
  (describe "index! dependency injection"
    (it "throws when :index-impl-fn is missing"
      (expect (throws? clojure.lang.ExceptionInfo
                #(sut/index! "doc.pdf" {}))))

    (it "delegates to injected :index-impl-fn"
      (let [calls (atom [])
            impl (fn [file-path opts]
                   (swap! calls conj {:file-path file-path :opts opts})
                   {:ok true :file-path file-path})
            result (sut/index! "doc.pdf" {:output "out.pageindex"
                                          :deps {:index-impl-fn impl}})]
        (expect (= {:ok true :file-path "doc.pdf"} result))
        (expect (= [{:file-path "doc.pdf"
                     :opts {:output "out.pageindex"
                            :deps {:index-impl-fn impl}}}]
                  @calls))))))
