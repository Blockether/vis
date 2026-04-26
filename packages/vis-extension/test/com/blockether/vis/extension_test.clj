(ns com.blockether.vis.extension-test
  "Pure tests for the extension contract \u2014 lives in `vis-extension`
   so the standalone library carries its own coverage and consumers
   that pull only this jar can run them in isolation.

   Cross-package tests that exercise vis-core's runtime composition
   (`active-extensions`, `assemble-system-prompt`, registry
   interaction) live in `vis-core/test/.../extension_api_test.clj`."
  (:require
   [com.blockether.vis.extension :as ext]
   [lazytest.core :refer [defdescribe it expect]]))

(def read-symbol
  (ext/symbol 'read-file (fn [& _] nil)
    {:doc "Read a file preview."
     :arglists '([path] [path offset limit])}))

(def retries-value
  (ext/value 'max-retries 3
    {:doc "Maximum retry attempts."}))

(defdescribe extension-prompt-rendering-test

  (it "renders canonical prompt text from symbol docstrings + arglists"
    (expect
      (= (str "Filesystem tools (use fs/ prefix; positional args only)\n"
           "- (fs/read-file path) or (fs/read-file path offset limit) \u2014 Read a file preview.\n"
           "- fs/max-retries \u2014 Maximum retry attempts.\n"
           "RULES:\n"
           "- Discover paths first.")
        (ext/render-prompt
          {:ext/doc "Filesystem tools"
           :ext/ns-alias {:ns 'vis.ext.fs :alias 'fs}
           :ext/symbols [read-symbol retries-value]
           :usage-note "positional args only"
           :notes ["RULES:" "- Discover paths first."]})))))

(defdescribe extension-builder-test

  (it "extension/symbol validates docstring + arglists"
    (let [s (ext/symbol 'read-file (fn [& _] nil)
              {:doc "Read a file." :arglists '([path])})]
      (expect (= 'read-file (:ext.symbol/sym s)))
      (expect (= "Read a file." (:ext.symbol/doc s)))
      (expect (= ["(read-file path)"] (:ext.symbol/examples s)))))

  (it "extension/value carries doc + value"
    (let [v (ext/value 'cap 42 {:doc "Cap."})]
      (expect (= 42 (:ext.symbol/val v)))
      (expect (= "Cap." (:ext.symbol/doc v)))))

  (it "extension/extension fills :ext/activation-fn + :ext/classes defaults"
    (let [e (ext/extension
              {:ext/namespace 'com.acme.ext.fs
               :ext/doc       "Filesystem tools"
               :ext/group     "filesystem"
               :ext/ns-alias  {:ns 'vis.ext.fs :alias 'fs}
               :ext/prompt    "placeholder"
               :ext/symbols   [read-symbol retries-value]})]
      (expect (fn? (:ext/activation-fn e)))
      (expect (true? ((:ext/activation-fn e) {})))
      (expect (= {} (:ext/classes e)))
      (expect (= {} (:ext/imports e))))))
