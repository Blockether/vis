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
  (it "drops satisfy-hint! from mixed display segments"
    (expect (= [{:kind :code :source "(def x 1)"}]
              (render/parse-block-display
                "(do (satisfy-hint! :vis.foundation/conversation-title) (def x 1))"))))

  (it "treats standalone satisfy-hint! as structurally silent"
    (expect (true? (render/block-structurally-silent?
                     "(satisfy-hint! :vis.foundation/conversation-title)")))))
