(ns com.blockether.vis.ext.foundation.environment.core-test
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.environment.core :as env-core]
   [lazytest.core :refer [defdescribe expect it]]))

(defdescribe environment-core-test
  (it "exports the expected environment symbol surface"
    (let [syms (set (map :ext.symbol/sym env-core/environment-symbols))]
      (expect (contains? syms 'snapshot))
      (expect (contains? syms 'git))
      (expect (contains? syms 'repositories))
      (expect (contains? syms 'languages))
      (expect (contains? syms 'monorepo))
      (expect (contains? syms 'refresh!))
      (expect (contains? syms 'render))
      (expect (contains? syms 'main-agent-instructions))
      (expect (contains? syms 'load-skill))
      (expect (contains? syms 'scan-warnings))
      (expect (contains? syms 'reload-instructions!))
      (expect (contains? syms 'reload-skills!))
      (expect (contains? syms 'reload-extensions!))))

  (it "renders a prompt fragment for the unified v/ alias"
    (let [prompt (env-core/environment-prompt {})]
      (expect (string? prompt))
      (expect (str/includes? prompt "(v/snapshot)"))
      (expect (str/includes? prompt "(v/load-skill \"name\")"))
      (expect (str/includes? prompt "(v/reload-extensions!)"))
      (expect (not (str/includes? prompt "md/")))))

  (it "renders foundation environment info separately from prompt extras"
    (let [info (env-core/environment-info {})]
      (expect (string? info))
      (expect (str/includes? info "<environment>"))
      (expect (str/includes? info "git.summary"))))

  (it "tracks loaded skills for the <active_skills> prompt block"
    (let [active-skills (atom {})
          load-skill (some #(when (= 'load-skill (:ext.symbol/sym %)) %)
                       env-core/environment-symbols)
          after-fn (:ext.symbol/after-fn load-skill)
          result {:found? true
                  :name "diagnose"
                  :description "Debug loop."
                  :body "Full body."}]
      (after-fn {:active-skills-atom active-skills} nil ["diagnose"] result)
      (expect (= result (get @active-skills "diagnose"))))))
