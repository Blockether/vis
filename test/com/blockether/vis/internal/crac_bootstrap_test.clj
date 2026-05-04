(ns com.blockether.vis.internal.crac-bootstrap-test
  (:require
   [com.blockether.vis.internal.crac-bootstrap :as crac]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe pre-extension-bootstrap-test
  (it "requires requested namespaces only once but records every phase"
    (crac/reset-state!)
    (let [required (atom [])]
      (with-redefs-fn {#'crac/require-namespace! (fn [ns-sym]
                                                   (swap! required conj ns-sym))}
        #(do
           (crac/pre-extension-bootstrap! {:phase :cli
                                           :namespaces '[example.one example.two]})
           (crac/pre-extension-bootstrap! {:phase :dev
                                           :namespaces '[example.three]})))
      (expect (= '[example.one example.two] @required))
      (expect (= {:ran? true
                  :phases [:cli :dev]}
                (select-keys (crac/state) [:ran? :phases]))))))

(defdescribe request-checkpoint-restore-test
  (it "fails with a tagged diagnostic when no CRaC API is available"
    (with-redefs-fn {#'crac/crac-core-class (constantly nil)}
      #(try
         (crac/request-checkpoint-restore!)
         (expect false)
         (catch clojure.lang.ExceptionInfo e
           (expect (= :vis/crac-unavailable (:type (ex-data e)))))))))

(defdescribe checkpoint-bootstrap-test
  (it "loads the host bootstrap namespaces before requesting CRaC"
    (crac/reset-state!)
    (let [events (atom [])]
      (with-redefs-fn {#'crac/require-namespace! (fn [ns-sym]
                                                   (swap! events conj [:require ns-sym]))
                       #'crac/request-checkpoint-restore! (fn []
                                                            (swap! events conj [:checkpoint])
                                                            {:status :restored})}
        #(expect (= {:status :restored}
                   (crac/checkpoint-bootstrap!))))
      (expect (= [[:require 'com.blockether.vis.core]
                  [:checkpoint]]
                @events)))))
