(ns com.blockether.vis.ext.provider-lmstudio-test
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-lmstudio]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private enrich-models
  (deref (requiring-resolve 'com.blockether.vis.ext.provider-lmstudio/enrich-models)))

(defdescribe provider-lmstudio-test
  (it "registers one LM Studio provider extension"
    (let [provider (vis/provider-by-id :lmstudio)
          status ((:provider/status-fn provider))]
      (expect (= :lmstudio (:provider/id provider)))
      (expect (= "LM Studio" (:provider/label provider)))
      (expect (= "http://localhost:1234/v1" (:base-url status)))))

  (it "exposes a context-detection hook"
    (expect (ifn? (:provider/enrich-models-fn (vis/provider-by-id :lmstudio)))))

  (describe "enrich-models"
    (it "defaults every model reasoning-capable + server-managed (routable for reasoning turns)"
      (with-redefs [svar/models! (fn [& _] (throw (ex-info "should not fetch" {})))]
        (let [out (enrich-models {:id :lmstudio :models [{:name "a" :context 200000}]} {})]
          (expect (true? (:reasoning? (first out))))
          (expect (= :server-managed (:reasoning-style (first out)))))))

    (it "short-circuits (no network) when every model already has :context"
      ;; svar/models! redef'd to throw — proves it is never called.
      (with-redefs [svar/models! (fn [& _] (throw (ex-info "should not fetch" {})))]
        (let [out (enrich-models {:id :lmstudio :models [{:name "a" :context 200000}]} {})]
          (expect (= 200000 (:context (first out)))))))

    (it "merges detected :context/:tool-call? onto models that lack one"
      (with-redefs [svar/make-router (fn [& _] ::router)
                    svar/models! (fn [_] [{:id "a" :context 262144 :tool-call? true}
                                          {:id "b" :context 8192}])]
        (let [out (enrich-models {:id :lmstudio :models [{:name "a"} {:name "b"}]} {})]
          (expect (= 262144 (:context (first out))))
          (expect (true? (:tool-call? (first out))))
          (expect (true? (:reasoning? (first out))))
          (expect (= 8192 (:context (second out)))))))

    (it "preserves an explicit :context override over detection"
      (with-redefs [svar/make-router (fn [& _] ::router)
                    svar/models! (fn [_] [{:id "a" :context 262144}])]
        (let [out (enrich-models {:id :lmstudio :models [{:name "a" :context 16384}]} {})]
          (expect (= 16384 (:context (first out)))))))

    (it "still applies defaults when detection throws"
      (with-redefs [svar/make-router (fn [& _] (throw (ex-info "boom" {})))]
        (let [out (enrich-models {:id :lmstudio :models [{:name "a"}]} {})]
          (expect (true? (:reasoning? (first out))))
          (expect (nil? (:context (first out)))))))))
