(ns com.blockether.vis.internal.progress-test
  (:require [com.blockether.vis.internal.progress :as progress]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe progress-test
  (it "retains successful :vis/silent forms with a visibility marker"
    (let [{:keys [on-chunk get-timeline]} (progress/make-progress-tracker)]
      (on-chunk {:phase :form-result
                 :iteration 1
                 :form-idx 0
                 :code "(set-conversation-title! \"Greeting\")"
                 :result :vis/silent
                 :execution-time-ms 1})
      (on-chunk {:phase :form-result
                 :iteration 1
                 :form-idx 1
                 :code "(+ 1 2)"
                 :result 3
                 :execution-time-ms 1})
      (let [entry (first (get-timeline))]
        (expect (= ["(set-conversation-title! \"Greeting\")" "(+ 1 2)"] (:code entry)))
        (expect (= [true false] (:silents entry)))
        (expect (= [":vis/silent" "3"] (:results entry))))))

  (it "renders plain live values without zprint pretty-printing"
    (let [{:keys [on-chunk get-timeline]} (progress/make-progress-tracker)]
      (on-chunk {:phase :form-result
                 :iteration 1
                 :form-idx 0
                 :code "{:b 2 :a [1 2 3]}"
                 :result {:b 2 :a [1 2 3]}
                 :execution-time-ms 1})
      (let [entry (first (get-timeline))]
        (expect (= ["{:b 2, :a [1 2 3]}"] (:results entry))))))

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
        (expect (= [false] (:silents entry))))))

  (it "retains provider fallback notices for live bubbles"
    (let [{:keys [on-chunk get-timeline]} (progress/make-progress-tracker)
          notice {:phase :provider-fallback
                  :iteration 1
                  :reason :transient-error
                  :failed-provider {:id :anthropic-coding-plan
                                    :model "claude-opus-4-7"
                                    :error "HTTP 529 overloaded"}
                  :new-provider {:id :zai-coding-plan
                                 :model "glm-5.1"}}]
      (on-chunk notice)
      (let [entry (first (get-timeline))]
        (expect (= :provider-call (:activity entry)))
        (expect (= [(select-keys notice [:reason :failed-provider :new-provider :fallback])]
                  (:provider-fallbacks entry))))))

  (it "tracks provider and response-parse activity separately from code execution"
    (let [{:keys [on-chunk get-timeline]} (progress/make-progress-tracker)]
      (on-chunk {:phase :provider-call :iteration 1})
      (expect (= :provider-call (:activity (first (get-timeline)))))
      (on-chunk {:phase :response-parse :status :start :iteration 1})
      (expect (= :response-parse (:activity (first (get-timeline)))))
      (on-chunk {:phase :form-start :iteration 1 :form-idx 0 :code "(+ 1 2)"})
      (let [entry (first (get-timeline))]
        (expect (nil? (:activity entry)))
        (expect (= ["(+ 1 2)"] (:code entry)))))))
