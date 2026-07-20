(ns com.blockether.vis.internal.progress-test
  (:require [com.blockether.vis.internal.progress :as progress]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  progress-tracker-error-test
  (it "stores form eval errors separately from rendered results"
      (let
        [tracker
         (progress/make-progress-tracker)

         err
         {:message "Unable to resolve symbol: x" :block {:source "(+ x 1)" :row 1 :col 4}}]

        ((:on-chunk tracker)
          {:phase :form-result
           :iteration 1
           :position 0
           :code "(+ x 1)"
           :error err
           :envelope {:started-at-ms 10 :finished-at-ms 15}})
        (let
          [entry
           (first ((:get-timeline tracker)))

           form
           (first (:forms entry))]

          (expect (= 1 (count (:forms entry))))
          (expect (= "(+ x 1)" (:code form)))
          (expect (nil? (:result-render form)))
          (expect (= :error (:result-kind form)))
          (expect (= err (:error form)))
          (expect (false? (:success? form)))
          (expect (= 5 (:duration-ms form))))))
  (it "tracks a hidden session-title change as a silent entry with no forms (no recap chrome)"
      ;; Recap chrome was removed in the sentinel-chrome teardown: a structurally
      ;; silent title change is tracked as an entry with empty :forms and surfaces
      ;; NO recap line (the field is gone).
      (let [tracker (progress/make-progress-tracker)]
        ((:on-chunk tracker)
          {:phase :form-result
           :iteration 1
           :position 0
           :code "(set-session-title! \"New title\")"
           :render-segments [{:kind :title :value "New title"}]
           :vis/structurally-silent? true
           :result "vis_silent"
           :silent? true})
        (let [entry (first ((:get-timeline tracker)))]
          (expect (= [] (:forms entry)))
          (expect (nil? (:recaps entry))))))
  (it "keeps a normal code chunk visible"
      (let [tracker (progress/make-progress-tracker)]
        ((:on-chunk tracker)
          {:phase :form-result
           :iteration 1
           :position 0
           :code "(def x \"doc\" 1)"
           :render-segments [{:kind :code :source "(def x \"doc\" 1)"}]
           :result 1
           :error nil})
        (let
          [entry (first ((:get-timeline tracker)))
           form (first (:forms entry))]

          (expect (= 1 (count (:forms entry))))
          (expect (= "(def x \"doc\" 1)" (:code form)))
          (expect (false? (:silent? form)))))))

(defdescribe
  progress-tracker-iteration-key-test
  (it "buckets every phase by its :iteration position; skips a chunk with none"
      ;; Canonical contract: EVERY streaming chunk — transport (`:provider-call`,
      ;; `:response-parse`), per-token (`:reasoning`, `:content`), and per-form
      ;; (`:form-start`, `:form-result`) — carries its 1-based iteration POSITION
      ;; under `:iteration`. (`:iteration-count` is reserved for the result map's
      ;; TOTAL — a different key for a different meaning.) A chunk with no
      ;; `:iteration` is SKIPPED, not routed into a `nil` bucket that would sort
      ;; before every real iteration and shift live numbering by +1 (the
      ;; phantom-bucket bug).
      (let
        [tracker
         (progress/make-progress-tracker)

         on
         (:on-chunk tracker)]

        (on {:phase :provider-call :iteration 1 :started-at-ms 0})
        (on {:phase :reasoning :iteration 1 :thinking "warm-up"})
        (on {:phase :iteration-final :iteration 1 :final nil :done? false})
        ;; A malformed chunk with no iteration is ignored (no phantom bucket).
        (on {:phase :reasoning :thinking "no-iteration"})
        (let [timeline ((:get-timeline tracker))]
          (expect (= 1 (count timeline)))
          (expect (= 1 (:iteration (first timeline))))
          (expect (= "warm-up" (:thinking (first timeline)))))))
  (it "streams provider content as :content-stream until response-parse done"
      ;; User-visible regression: prior to live answer streaming the
      ;; bubble froze for several seconds between reasoning end and the
      ;; first parsed form. Streaming `:content` keeps the bubble alive.
      (let
        [tracker
         (progress/make-progress-tracker)

         on
         (:on-chunk tracker)]

        (on {:phase :reasoning :iteration 1 :thinking "think"})
        (on {:phase :content :iteration 1 :content "```clojure\n(done {:answer \"yellow\""})
        (let [entry (first ((:get-timeline tracker)))]
          (expect (= "think" (:thinking entry)))
          (expect (= "```clojure\n(done {:answer \"yellow\"" (:content-stream entry))))
        (on {:phase :response-parse :iteration 1 :status :done})
        (let [entry (first ((:get-timeline tracker)))]
          (expect (nil? (:content-stream entry))))))
  (it "provider retry reset rewinds stale streamed reasoning and content"
      (let
        [tracker
         (progress/make-progress-tracker)

         on
         (:on-chunk tracker)

         event
         {:event/type :llm.routing/provider-retry
          :provider "zai-coding-plan"
          :model "glm-5.1"
          :reason :stream-connection-error
          :attempt 1
          :delay-ms 1000
          :error "Stream connection error: closed"}]

        (on {:phase :reasoning :iteration 1 :thinking "dead thinking"})
        (on {:phase :content :iteration 1 :content "dead content"})
        (on {:phase :provider-retry-reset :iteration 1 :event event})
        (let [entry (first ((:get-timeline tracker)))]
          (expect (nil? (:thinking entry)))
          (expect (nil? (:content-stream entry)))
          (expect (= :provider-call (:activity entry)))
          (expect (= [event] (:provider-fallbacks entry))))))
  (it "silently drops chunks that carry neither key"
      ;; Defensive: a malformed producer must not resurrect the phantom
      ;; bucket bug. The tracker just no-ops.
      (let
        [tracker
         (progress/make-progress-tracker)

         on
         (:on-chunk tracker)]

        (on {:phase :reasoning :thinking "orphan"})
        (expect (= [] ((:get-timeline tracker)))))))
