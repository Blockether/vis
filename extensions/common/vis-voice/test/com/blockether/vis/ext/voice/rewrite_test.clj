(ns com.blockether.vis.ext.voice.rewrite-test
  (:require [com.blockether.vis.core :as vis]
            [com.blockether.vis.ext.voice.rewrite :as rewrite]
            [lazytest.core :refer [defdescribe it expect]]))

(defdescribe rewrite-test
  (it "does not call LLM for blank transcripts"
    (expect (= "" (rewrite/rewrite-transcript! "  "))))

  (it "uses vis dynamic helper LLM for non-blank transcripts"
    (with-redefs [vis/llm-text! (fn [opts]
                                  (expect (= :off (:reasoning opts)))
                                  {:text "clean prompt"})]
      (expect (= "clean prompt" (rewrite/rewrite-transcript! "uh fix it"))))))
