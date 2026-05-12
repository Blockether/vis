(ns com.blockether.vis.internal.skills-test
  (:require [com.blockether.vis.internal.skills :as skills]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe skills-test
  (it "exposes bang skill activation and keeps non-bang compatibility alias"
    (let [active-skills (atom {})
          loaded-skill {:found? true
                        :name "diagnose"
                        :description "Debug loop."
                        :body "Full body."}]
      (with-redefs [skills/lookup (constantly loaded-skill)]
        (let [bindings (skills/sandbox-bindings active-skills)]
          (expect (ifn? (get bindings 'load-skill!)))
          (expect (ifn? (get bindings 'load-skill)))
          (expect (= loaded-skill ((get bindings 'load-skill!) "diagnose")))
          (expect (= loaded-skill (get @active-skills "diagnose"))))))))
