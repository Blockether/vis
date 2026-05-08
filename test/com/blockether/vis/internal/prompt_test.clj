(ns com.blockether.vis.internal.prompt-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.skills :as skills]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe core-system-prompt-test
  (it "keeps the prompt on inspect/change/test/answer workflow"
    (let [p prompt/CORE_SYSTEM_PROMPT]
      (expect (str/includes? p "inspect before edit"))
      (expect (str/includes? p "Do normal inspect -> change -> test -> answer")))))

(defdescribe internal-skills-prompt-test
  (it "renders skills from core prompt assembly, not extension prompt blocks"
    (with-redefs [skills/list-all (fn [] [{:name "diagnose"
                                           :source :repo
                                           :description "Debug loop."}])]
      (let [out (prompt/assemble-system-prompt {}
                  {:active-extensions []
                   :system-prompt nil})]
        (expect (str/includes? out "<skills count=\"1\">"))
        (expect (str/includes? out "(load-skill \"name\")"))
        (expect (not (str/includes? out "(v/load-skill")))
        (expect (not (str/includes? out "<extensions>"))))))

  (it "does not render an empty skills block"
    (with-redefs [skills/list-all (fn [] [])]
      (let [out (prompt/assemble-system-prompt {}
                  {:active-extensions []
                   :system-prompt nil})]
        (expect (not (str/includes? out "<skills")))))))
