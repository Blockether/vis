(ns com.blockether.vis.ext.provider-lmstudio-test
  (:require [com.blockether.svar.core :as svar]
            [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.provider-lmstudio]
            [lazytest.core :refer [defdescribe describe expect it]]))

(def ^:private enrich-models
  (deref (requiring-resolve 'com.blockether.vis.ext.provider-lmstudio/enrich-models)))

(defdescribe
  provider-lmstudio-test
  (it "registers one LM Studio provider extension"
      (let [provider
            (vis/provider-by-id :lmstudio)

            status
            ((:provider/status-fn provider))]

        (expect (= :lmstudio (:provider/id provider)))
        (expect (= "LM Studio" (:provider/label provider)))
        (expect (= "http://localhost:1234/v1" (:base-url status)))))
  (it "exposes a context-detection hook"
      (expect (ifn? (:provider/enrich-models-fn (vis/provider-by-id :lmstudio)))))
  (it "ships anti-loop sampler defaults in :extra-body (Qwen/DeepSeek thinking loops)"
      ;; Local thinking models spiral into endless reasoning under the default
      ;; greedy sampler; the preset carries vendor-recommended anti-loop params
      ;; that svar merges as the provider base layer (per-turn :extra-body wins).
      (let [eb (get-in (vis/provider-by-id :lmstudio) [:provider/preset :extra-body])]
        (expect (= 0.6 (:temperature eb)))
        (expect (= 0.95 (:top_p eb)))
        (expect (= 20 (:top_k eb)))
        (expect (= 0.0 (:min_p eb)))
        (expect (= 1.5 (:presence_penalty eb)))))
  (describe "enrich-models"
            (it "short-circuits (no network) when every model already has :context"
                ;; svar/models! redef'd to throw — proves it is never called.
                (with-redefs [svar/models! (fn [& _]
                                             (throw (ex-info "should not fetch" {})))]
                  (let [models [{:name "a" :context 200000} {:name "b" :context 8192}]]
                    (expect (= models (enrich-models {:id :lmstudio :models models} {}))))))
            (it "merges detected :context/:tool-call? onto models that lack one"
                (with-redefs [svar/make-router
                              (fn [& _]
                                ::router)

                              svar/models!
                              (fn [_]
                                [{:id "a" :context 262144 :tool-call? true}
                                 {:id "b" :context 8192}])]

                  (let [out (enrich-models {:id :lmstudio :models [{:name "a"} {:name "b"}]} {})]
                    (expect (= 262144 (:context (first out))))
                    (expect (true? (:tool-call? (first out))))
                    (expect (= 8192 (:context (second out)))))))
            (it "does NOT infer :reasoning? (LM Studio exposes no reasoning signal)"
                (with-redefs [svar/make-router
                              (fn [& _]
                                ::router)

                              svar/models!
                              (fn [_]
                                [{:id "a" :context 262144 :tool-call? true}])]

                  (let [out (enrich-models {:id :lmstudio :models [{:name "a"}]} {})]
                    (expect (nil? (:reasoning? (first out)))))))
            (it "preserves an explicit :context override over detection"
                (with-redefs [svar/make-router
                              (fn [& _]
                                ::router)

                              svar/models!
                              (fn [_]
                                [{:id "a" :context 262144}])]

                  (let [out (enrich-models {:id :lmstudio :models [{:name "a" :context 16384}]} {})]
                    (expect (= 16384 (:context (first out)))))))
            (it "returns models unchanged when detection throws"
                (with-redefs [svar/make-router (fn [& _]
                                                 (throw (ex-info "boom" {})))]
                  (let [models [{:name "a"}]]
                    (expect (= models (enrich-models {:id :lmstudio :models models} {}))))))))
