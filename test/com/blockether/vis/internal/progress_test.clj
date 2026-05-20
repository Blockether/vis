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
      (let [entry (first ((:get-timeline tracker)))
            form  (first (:forms entry))]
        (expect (= 1 (count (:forms entry))))
        (expect (= "(+ x 1)" (:code form)))
        (expect (nil? (:result-render form)))
        (expect (= :error (:result-kind form)))
        (expect (= err (:error form)))
        (expect (false? (:success? form)))
        (expect (= 5 (:duration-ms form))))))

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
        (expect (= [] (:forms entry)))
        (expect (= ["Title changed to \"New title\"."] (:recaps entry))))))

  (it "keeps mixed answer/code chunks visible"
    (let [tracker (progress/make-progress-tracker)]
      ((:on-chunk tracker) {:phase :form-result
                            :iteration-count 1
                            :position 0
                            :code "(done {:answer \"ok\"})\n(def x \"doc\" 1)"
                            :render-segments [{:kind :answer-ref}
                                              {:kind :code :source "(def x \"doc\" 1)"}]
                            :result :vis/answer
                            :error nil})
      (let [entry (first ((:get-timeline tracker)))
            form  (first (:forms entry))]
        (expect (= 1 (count (:forms entry))))
        (expect (= "(done {:answer \"ok\"})\n(def x \"doc\" 1)" (:code form)))
        (expect (false? (:silent? form)))))))

(defdescribe progress-tracker-iteration-key-aliasing-test
  (it "routes `:iteration`-only chunks to the same bucket as `:iteration-count` chunks"
    ;; Regression: the iteration loop emits `:provider-call`,
    ;; `:response-parse`, and `:iteration-error` chunks with only
    ;; `:iteration` set (no `:iteration-count`). The tracker used to
    ;; key the sorted-map exclusively on `:iteration-count`, so those
    ;; chunks landed in a `nil` bucket that sorted before every real
    ;; iteration. Result: live TUI labels showed "ITERATION 2" for
    ;; what the final result correctly reported as a single iteration.
    (let [tracker (progress/make-progress-tracker)
          on     (:on-chunk tracker)]
      ;; Transport-level chunk that historically only carried `:iteration`.
      (on {:phase :provider-call :iteration 1 :started-at-ms 0})
      ;; Lifecycle chunks that carry `:iteration-count`.
      (on {:phase :reasoning :iteration-count 1 :thinking "warm-up"})
      (on {:phase :iteration-final :iteration-count 1 :final nil :done? false})
      (let [timeline ((:get-timeline tracker))]
        ;; One iteration in the timeline — no phantom nil-bucket entry.
        (expect (= 1 (count timeline)))
        (expect (= 1 (:iteration (first timeline))))
        (expect (= "warm-up" (:thinking (first timeline)))))))

  (it "silently drops chunks that carry neither key"
    ;; Defensive: a malformed producer must not resurrect the phantom
    ;; bucket bug. The tracker just no-ops.
    (let [tracker (progress/make-progress-tracker)
          on     (:on-chunk tracker)]
      (on {:phase :reasoning :thinking "orphan"})
      (expect (= [] ((:get-timeline tracker)))))))
