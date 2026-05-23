(ns com.blockether.vis.internal.render-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.render :as render]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe heading-shorthand-test
  (it "normalizes [:h level ...] without rendering the level as text"
    (let [ast (render/->ast [:ir [:h 3 "Regular hook (" [:c ":ext/hooks"] ")"]])]
      (expect (= [:ir {}
                  [:h {:level 3}
                   [:span {} "Regular hook ("]
                   [:c {} ":ext/hooks"]
                   [:span {} ")"]]]
                ast))
      (expect (= "### Regular hook (`:ext/hooks`)"
                (str/trim (render/render ast :markdown {}))))))

  (it "clamps shorthand heading levels to the supported 1-6 range"
    (expect (= [:ir {} [:h {:level 1} [:span {} "Low"]]]
              (render/->ast [:ir [:h 0 "Low"]])))
    (expect (= [:ir {} [:h {:level 6} [:span {} "High"]]]
              (render/->ast [:ir [:h 9 "High"]])))))

(defdescribe host-bookkeeping-render-test
  (it "classifies only top-level structural host forms"
    (expect (= [{:kind :title :value "Render cleanup"}
                {:kind :code :source "(def x 1)"}
                {:kind :answer-ref}]
              (render/parse-block-display
                "(set-session-title! \"Render cleanup\")\n(def x 1)\n(done [:ir [:p \"ok\"]])"))))

  (it "does not source-prune nested host calls"
    (expect (= [{:kind :code
                 :source "(do (satisfy-hint! :vis.foundation/session-title) (def x 1))"}]
              (render/parse-block-display
                "(do (satisfy-hint! :vis.foundation/session-title) (def x 1))"))))

  (it "does not treat satisfy-hint! source as structurally silent"
    (expect (false? (render/block-structurally-silent?
                      "(satisfy-hint! :vis.foundation/session-title)"))))

  (it "flags (def NAME (qualified/call …)) wrappers as hidden code"
    ;; The model wraps tool calls in `(def …)` for downstream reuse.
    ;; The channel shows the tool result pane anyway, so the def
    ;; source itself is noise. Mark the segment hidden so the renderer
    ;; collapses the code lines but still keeps the form in the
    ;; iteration trace.
    (expect (= [{:kind :code
                 :source "(def render-sb-code (v/cat \"x.clj\"))"
                 :hidden? true}]
              (render/parse-block-display
                "(def render-sb-code (v/cat \"x.clj\"))"))))

  (it "keeps plain (def x 1) visible (no tool wrapper)"
    (expect (= [{:kind :code :source "(def x 1)"}]
              (render/parse-block-display "(def x 1)"))))

  (it "hides def docstring form when wrapping a tool call"
    (expect (= [{:kind :code
                 :source "(def named \"docstring\" (v/cat \"x.clj\"))"
                 :hidden? true}]
              (render/parse-block-display
                "(def named \"docstring\" (v/cat \"x.clj\"))"))))

  (it "hides bare qualified tool calls so the result pane speaks for itself"
    (expect (= [{:kind :code
                 :source "(v/cat \"src/foo.clj\")"
                 :hidden? true}]
              (render/parse-block-display "(v/cat \"src/foo.clj\")"))))

  (it "classifies ctx mutators as structured recap segments (engine-only — raw source is hidden)"
    (expect (= [{:kind :spec-update :id :K :title "x"}]
              (render/parse-block-display "(spec-set! :K {:title \"x\"})")))
    (expect (= [{:kind :task-update :id :K :status :done :proof "t1/i1/f1"}]
              (render/parse-block-display "(task-set! :K {:status :done :proof \"t1/i1/f1\"})")))
    (expect (= [{:kind :fact-update :id :K}]
              (render/parse-block-display "(fact-set! :K {:summary \"f\"})"))))

  (it "coalesces neighboring hidden tool calls into one hidden segment"
    (let [out (render/parse-block-display
                "(v/cat \"a.clj\")\n(v/cat \"b.clj\")")]
      (expect (= 1 (count out)))
      (expect (true? (-> out first :hidden?)))))

  (it "keeps visible code and hidden tool calls in separate segments"
    (let [out (render/parse-block-display
                "(def x 1)\n(v/cat \"a.clj\")")]
      (expect (= 2 (count out)))
      (expect (nil? (-> out first :hidden?)))
      (expect (true? (-> out second :hidden?)))))

  (it "marks `(def r (v/ls …))` as hidden so the channel can collapse the raw source row"
    ;; Real-world fence the model emits when it wants to bind a tool
    ;; result and then project it. Pi-style render: the def wrapping
    ;; collapses, only the derived form (`select-keys`) stays
    ;; visible. The tool's channel preview lives in `:channel`
    ;; sink entries on the chunk, separate from this source
    ;; classification.
    (let [out (render/parse-block-display
                "(def r (v/ls \".\"))\n(select-keys r [:entry-count :file-count])")]
      (expect (= 2 (count out)))
      (expect (true?  (-> out first  :hidden?)))
      (expect (nil?   (-> out second :hidden?))))))
