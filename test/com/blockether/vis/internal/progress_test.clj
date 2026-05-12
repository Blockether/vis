(ns com.blockether.vis.internal.progress-test
  (:require [com.blockether.vis.internal.progress :as progress]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe progress-test
  (it "retains successful :vis/silent forms with a visibility marker"
    (let [{:keys [on-chunk get-timeline]} (progress/make-progress-tracker)]
      (on-chunk {:phase :form-result
                 :iteration 1
                 :form-idx 0
                 :code "(set-conversation-title \"Greeting\")"
                 :result :vis/silent
                 :execution-time-ms 1})
      (on-chunk {:phase :form-result
                 :iteration 1
                 :form-idx 1
                 :code "(+ 1 2)"
                 :result 3
                 :execution-time-ms 1})
      (let [entry (first (get-timeline))]
        (expect (= ["(set-conversation-title \"Greeting\")" "(+ 1 2)"] (:code entry)))
        (expect (= [true false] (:silents entry)))
        (expect (= [":vis/silent" "3"] (:results entry))))))

  (it "still elides the final answer form"
    (let [{:keys [on-chunk get-timeline]} (progress/make-progress-tracker)]
      (on-chunk {:phase :form-result
                 :iteration 1
                 :form-idx 0
                 :code "(+ 1 2)"
                 :result 3
                 :execution-time-ms 1})
      (on-chunk {:phase :form-result
                 :iteration 1
                 :form-idx 1
                 :code "(turn-answer! [:ir [:p \"Done\"]])"
                 :result :vis/answer
                 :execution-time-ms 1})
      (on-chunk {:phase :iteration-final
                 :iteration 1
                 :final [:ir {} [:p {} [:span {} "Done"]]]
                 :done? true
                 :answer-form-idx 1})
      (let [entry (first (get-timeline))]
        (expect (= ["(+ 1 2)"] (:code entry)))
        (expect (= [false] (:silents entry)))))))
