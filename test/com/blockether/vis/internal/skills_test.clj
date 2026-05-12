(ns com.blockether.vis.internal.skills-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.skills :as skills]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe skills-test
  (it "exposes only bang skill activation as an internal sandbox primitive"
    (let [active-skills (atom {})
          loaded-skill {:found? true
                        :name "diagnose"
                        :description "Debug loop."
                        :body "Full body."}]
      (with-redefs [skills/lookup (constantly loaded-skill)]
        (let [bindings (skills/sandbox-bindings active-skills)]
          (expect (ifn? (get bindings 'load-skill!)))
          (expect (nil? (get bindings 'load-skill)))
          (expect (= loaded-skill ((get bindings 'load-skill!) "diagnose")))
          (expect (= loaded-skill (get @active-skills "diagnose")))))))

  (it "renders load-skill results as Markdown instead of escaped maps"
    (let [loaded-skill {:found? true
                        :name "find-skills"
                        :path "/tmp/SKILL.md"
                        :body "# Find Skills\n\nNice body."}
          rendered (skills/render-load-result loaded-skill)]
      (expect (skills/load-result? loaded-skill))
      (expect (= {:kind :skill-load :name "find-skills"}
                (skills/load-result-detail loaded-skill)))
      (expect (str/includes? rendered "Loaded skill `find-skills`"))
      (expect (str/includes? rendered "# Find Skills"))
      (expect (not (str/includes? rendered ":body"))))))
