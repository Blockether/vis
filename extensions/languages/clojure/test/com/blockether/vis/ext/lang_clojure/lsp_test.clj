(ns com.blockether.vis.ext.lang-clojure.lsp-test
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [com.blockether.vis.ext.lang-clojure.lsp :as lsp]
   [lazytest.core :refer [defdescribe expect it]]))

(defn- temp-project []
  (let [root (fs/file "target/lang-clojure-lsp-test")]
    (fs/delete-tree root)
    (fs/create-dirs (fs/file root "src" "demo"))
    (spit (fs/file root "deps.edn") "{:paths [\"src\"]}\n")
    (spit (fs/file root "src" "demo" "core.clj")
      (str "(ns demo.core\n"
        "  (:require [clojure.string :as str]\n"
        "            [clojure.set :as set]))\n"
        "(defn bar [x] (inc x))\n"
        "(defn foo [x] (bar x))\n"
        "(defn blank? [] (str/blank? \"\"))\n"))
    {:root (str root)
     :file (str (fs/file root "src" "demo" "core.clj"))}))

(defn- tool-fn [symbol-entry]
  (:ext.symbol/fn symbol-entry))

(defdescribe clojure-lsp-plan-tools-test
  (it "returns diagnostics and dry-run edit plans without writing files"
    (let [{:keys [root file]} (temp-project)
          diagnostics         (tool-fn lsp/diagnostics-symbol)
          rename-plan         (tool-fn lsp/rename-plan-symbol)
          clean-ns-plan       (tool-fn lsp/clean-ns-plan-symbol)
          before              (slurp file)
          diag-out            (diagnostics {:project-root root :filenames [file]})
          rename-out          (rename-plan 'demo.core/bar 'demo.core/baz {:project-root root})
          clean-out           (clean-ns-plan {:project-root root :filenames [file]})]
      (expect (true? (:op/success? diag-out)))
      (expect (vector? (:result diag-out)))
      (expect (true? (:op/success? rename-out)))
      (expect (= 'demo.core/bar (get-in rename-out [:result :from])))
      (expect (= 'demo.core/baz (get-in rename-out [:result :to])))
      (expect (seq (get-in rename-out [:result :edits])))
      (expect (str/includes? (get-in rename-out [:result :edits 0 :new-text]) "baz"))
      (expect (true? (:op/success? clean-out)))
      (expect (seq (get-in clean-out [:result :edits])))
      (expect (not (str/includes? (get-in clean-out [:result :edits 0 :new-text]) "clojure.set")))
      (expect (= before (slurp file))))))
