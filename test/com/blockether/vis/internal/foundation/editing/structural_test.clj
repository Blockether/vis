(ns com.blockether.vis.internal.foundation.editing.structural-test
  "Cross-language coverage for the tree-sitter outline + structural editing
   tools. Exercises Clojure, Python and Rust through the same unified API so a
   regression in any placement strategy (after-name / body / comment-before)
   is caught. The platform native FFI lib is resolved at runtime by
   com.blockether.tree-sitter-language-pack."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.foundation.editing.outline :as outline]
   [com.blockether.vis.internal.foundation.editing.structural :as structural]
   [lazytest.core :refer [defdescribe expect it describe]]))

(def ^:private clj-src "(ns demo)\n(defn add [a b] (+ a b))\n(defn sub [a b] (- a b))\n")
(def ^:private py-src "def add(a, b):\n    return a + b\n")
(def ^:private rs-src "fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n")

(defn- edit [path src m] (structural/edit-source path src m))

(defn- throws? [f]
  (try (f) false (catch Exception _ true)))

(defdescribe outline-test
  (it "Clojure outline lists defs with anchors"
    (let [s (outline/file-skeleton "demo.clj" clj-src)]
      (expect (str/includes? s "function add"))
      (expect (str/includes? s "function sub"))
      (expect (re-find #"@\d+:\w+\.\.\d+:\w+" s))))
  (it "Python outline lists the function"
    (expect (str/includes? (outline/file-skeleton "m.py" py-src) "function add")))
  (it "Rust outline lists the function"
    (expect (str/includes? (outline/file-skeleton "m.rs" rs-src) "function add")))
  (it "unknown language yields no skeleton"
    (expect (nil? (outline/file-skeleton "x.unknownext" "blah")))))

(defdescribe replace-test
  (it "Clojure replace by name"
    (expect (str/includes?
              (edit "demo.clj" clj-src {:op :replace :target "add" :code "(defn add [a b c] (+ a b c))"})
              "(defn add [a b c] (+ a b c))")))
  (it "Python replace by name"
    (expect (str/includes?
              (edit "m.py" py-src {:op :replace :target "add" :code "def add(a, b):\n    return a + b + 0"})
              "+ 0")))
  (it "rejects a syntax-breaking replace"
    (expect (throws? #(edit "demo.clj" clj-src {:op :replace :target "add" :code "(defn add [a b"})))))

(defdescribe add-doc-test
  (it "Clojure add_doc places the doc after the name"
    (expect (str/includes?
              (edit "demo.clj" clj-src {:op :add-doc :target "add" :code "\"Sum.\""})
              "(defn add \"Sum.\" [a b]")))
  (it "Python add_doc places the doc as first body statement"
    (let [r (edit "m.py" py-src {:op :add-doc :target "add" :code "\"\"\"Sum.\"\"\""})]
      (expect (str/includes? r "def add(a, b):\n    \"\"\"Sum.\"\"\""))))
  (it "Rust add_doc places a comment before the fn"
    (let [r (edit "m.rs" rs-src {:op :add-doc :target "add" :code "/// Sum."})]
      (expect (str/includes? r "/// Sum.\nfn add")))))

(defdescribe replace-node-test
  (it "replaces a unique sub-expression"
    (expect (str/includes?
              (edit "demo.clj" clj-src {:op :replace-node :match "(+ a b)" :code "(+ a b 1)"})
              "(+ a b 1)")))
  (it "refuses an ambiguous match without scope"
    (let [s "(defn f [] (+ a b))\n(defn g [] (+ a b))\n"]
      (expect (throws? #(edit "demo.clj" s {:op :replace-node :match "(+ a b)" :code "(- a b)"})))))
  (it "scoping disambiguates"
    (let [s "(defn f [] (+ a b))\n(defn g [] (+ a b))\n"
          r (edit "demo.clj" s {:op :replace-node :match "(+ a b)" :code "(- a b)" :target "g"})]
      (expect (str/includes? r "(defn f [] (+ a b))"))
      (expect (str/includes? r "(defn g [] (- a b))")))))

(defdescribe doc-ops-test
  (describe "replace_doc"
    (it "swaps an existing Clojure doc"
      (let [s "(defn add \"old\" [a b] (+ a b))\n"
            r (edit "demo.clj" s {:op :replace-doc :target "add" :code "\"new\""})]
        (expect (str/includes? r "\"new\""))
        (expect (not (str/includes? r "\"old\"")))))
    (it "rejects add_doc when a doc already exists"
      (let [s "(defn add \"old\" [a b] (+ a b))\n"]
        (expect (throws? #(edit "demo.clj" s {:op :add-doc :target "add" :code "\"x\""})))))))
