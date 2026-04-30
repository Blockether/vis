(ns com.blockether.vis.ext.channel-tui.footer-test
  (:require [com.blockether.vis.ext.channel-tui.footer :as footer]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe build-segments-test
  (it "shows the selected reasoning level for reasoning-capable models"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-5"
                                                          :provider :openai
                                                          :reasoning? true})}
        (fn []
          (expect (= ["openai/gpt-5" "(deep)"]
                    (->> (build-segments {:messages []
                                          :settings {:reasoning-level :deep}}
                           0)
                      (filter #(= :left (:region %)))
                      (mapv :text))))))))

  (it "omits the reasoning suffix for non-reasoning models"
    (let [build-segments @#'footer/build-segments]
      (with-redefs-fn {#'footer/chosen-model-info (fn [] {:name "gpt-4o"
                                                          :provider :openai
                                                          :reasoning? false})}
        (fn []
          (expect (= ["openai/gpt-4o"]
                    (->> (build-segments {:messages []
                                          :settings {:reasoning-level :deep}}
                           0)
                      (filter #(= :left (:region %)))
                      (mapv :text)))))))))
