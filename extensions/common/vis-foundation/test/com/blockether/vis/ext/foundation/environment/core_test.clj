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
      (expect (not (str/includes? prompt "md/"))))))
