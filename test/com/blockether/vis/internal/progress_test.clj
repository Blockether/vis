(ns com.blockether.vis.internal.progress-test
  (:require
   [com.blockether.vis.internal.progress :as progress]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe progress-tracker-error-test
  (it "stores form eval errors separately from rendered results"
    (let [tracker (progress/make-progress-tracker)
          err {:message "Unable to resolve symbol: x"
               :block {:source "(+ x 1)" :row 1 :col 4}}]
      ((:on-chunk tracker) {:phase :form-result
                            :iteration-count 1
                            :position 0
                            :code "(+ x 1)"
                            :error err
                            :envelope {:started-at-ms 10
                                       :finished-at-ms 15}})
      (let [entry (first ((:get-timeline tracker)))]
        (expect (= ["(+ x 1)"] (:code entry)))
        (expect (= [nil] (:results entry)))
        (expect (= [:error] (:result-kinds entry)))
        (expect (= [err] (:errors entry)))
        (expect (= [false] (:successes entry)))
        (expect (= [5] (:durations entry))))))

  (it "keeps a recap for hidden session title changes"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :form-result
                            :iteration-count 1
                            :position 0
                            :code "(set-session-title! \"New title\")"
                            :render-segments [{:kind :title :value "New title"}]
                            :vis/structurally-silent? true
                            :result :vis/silent
                            :silent? true})
      (let [entry (first ((:get-timeline tracker)))]
        (expect (= [] (:code entry)))
        (expect (= ["Title changed to \"New title\"."] (:recaps entry)))))))
