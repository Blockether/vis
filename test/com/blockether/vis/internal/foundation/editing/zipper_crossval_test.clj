(ns com.blockether.vis.internal.foundation.editing.zipper-crossval-test
  "CROSS-VALIDATION of the tree-sitter editing engine against INDEPENDENT
   oracles — not just self-consistency (round-trip/re-parse), but agreement with
   a different computation:

     A. byte ranges ↔ line/col ranges  — the node's UTF-8 byte-sliced text must
        equal the text located by its (line, column) coordinates, sliced
        independently from the source as a string. Two coordinate systems the
        engine reports separately must agree.
     B. struct_patch ↔ sexpr_edit        — the SAME edit via the name-based Rust
        `StructuralApi` and via the FFM `jtreesitter` zipper must converge to the
        same file. Two independent tree-sitter bindings cross-checking each other.
     C. tree-sitter ↔ Clojure reader    — every top-level form node, read back by
        the real Clojure reader, must equal the form the reader gets from the
        whole source. Validates node boundaries against ground-truth parsing."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.zipper :as z]
            [com.blockether.vis.internal.foundation.editing.structural :as structural]
            [lazytest.core :refer [defdescribe it expect]])
  (:import [java.io PushbackReader StringReader]))

(defn- all-paths
  [lang src]
  (letfn [(walk [p]
            (let [i (z/inspect lang src p)]
              (cons p (mapcat #(walk (conj p %)) (range (:named-child-count i))))))]
    (walk [])))

;; ── A. byte ranges ↔ line/col ranges (ASCII, so byte col == char col) ───────
(defn- slice-linecol
  "Substring of `src` from (sl, sc) to (el, ec) — sl/el 1-based lines, sc/ec
   0-based columns — computed by STRING indexing, independent of byte offsets."
  [src sl sc el ec]
  (let [lines (vec (str/split src #"\n" -1))]
    (if (= sl el)
      (subs (lines (dec sl)) sc ec)
      (str/join "\n"
                (concat [(subs (lines (dec sl)) sc)]
                        (subvec lines sl (dec el))
                        [(subs (lines (dec el)) 0 ec)])))))

(def ^:private ascii-bank
  [["clj" "(ns demo)\n\n(defn add [a b]\n  (+ a b))\n\n(def x 10)\n"]
   ["py"
    "class C:\n    def total(self, xs):\n        acc = 0\n        for x in xs:\n            acc += x\n        return acc\n"]
   ["js" "function add(a, b) {\n  const s = a + b;\n  return s;\n}\n\nconst y = 3;\n"]
   ["go" "package m\n\nfunc add(a int, b int) int {\n\treturn a + b\n}\n"]
   ["java" "class M {\n  int add(int a, int b) {\n    return a + b;\n  }\n}\n"]
   ["rs" "fn add(a: i32, b: i32) -> i32 {\n    let s = a + b;\n    s\n}\n"]])

(defdescribe byte-vs-linecol-test
             (doseq [[ext src] ascii-bank]
               (it (str ext " byte-sliced node text == its own line/col-sliced text (every node)")
                   (let [lang (z/detect-language (str "f." ext))]
                     (expect (true? (every? (fn [p]
                                              (let [n (z/inspect lang src p)]
                                                (= (:text n)
                                                   (slice-linecol src
                                                                  (:start-line n)
                                                                  (:start-col n)
                                                                  (:end-line n)
                                                                  (:end-col n)))))
                                            (all-paths lang src))))))))

;; ── B. struct_patch (Rust StructuralApi) ↔ sexpr_edit (FFM zipper) converge ──
(defn- ws-norm [s] (str/trim (str/replace s #"\s+" " ")))

(def ^:private converge-bank
  [["clj" "(defn add [a b] (+ a b))\n" "(defn add [a b] (* a b 2))"]
   ["py" "def add(a, b):\n    return a + b\n" "def add(a, b):\n    return a * b"]
   ["js" "function add(a, b) { return a + b; }\n" "function add(a, b) { return a * b; }"]
   ["rs" "fn add(a: i32, b: i32) -> i32 { a + b }\n" "fn add(a: i32, b: i32) -> i32 { a * b }"]
   ["go" "package m\nfunc add(a int, b int) int { return a + b }\n"
    "func add(a int, b int) int { return a * b }"]
   ["rb" "def add(a, b)\n  a + b\nend\n" "def add(a, b)\n  a * b\nend"]])

(defn- node-idx-of
  [lang src needle]
  (let [root (z/inspect lang src [])]
    (some (fn [{:keys [idx]}]
            (when (str/includes? (:text (z/inspect lang src [idx])) needle) idx))
          (:children root))))

(defdescribe struct-patch-vs-zipper-test
             (doseq [[ext src code] converge-bank]
               (it (str ext " name-based struct_patch and path-based sexpr_edit converge")
                   (let [path (str "f." ext)
                         lang (z/detect-language path)
                         ;; Rust StructuralApi, by NAME
                         via-struct
                         (structural/edit-source path src {:op :replace :target "add" :code code})
                         ;; FFM jtreesitter zipper, by PATH to the same node
                         i (node-idx-of lang src "add")
                         via-zipper (:new-source (z/edit lang src [i] :replace code))]

                     (expect (some? i))
                     (expect (string? via-struct))
                     (expect (string? via-zipper))
                     ;; two independent tree-sitter bindings → same structural result
                     (expect (= (ws-norm via-struct) (ws-norm via-zipper)))))))

;; ── C. tree-sitter Clojure node boundaries ↔ the real Clojure reader ────────
(defn- read-forms
  [src]
  (let [r (PushbackReader. (StringReader. src))]
    (loop [acc []]
      (let [f (read {:eof ::eof :read-cond :preserve} r)]
        (if (= f ::eof) acc (recur (conj acc f)))))))

(def ^:private clj-srcs
  ["(ns demo)\n(defn add [a b] (+ a b))\n(def x 10)\n"
   "(defn f [x]\n  (let [y (* x x)]\n    (+ y 1)))\n(defmacro m [& body] `(do ~@body))\n"
   "(def data {:a 1 :b [2 3] :c \"str with )(\"})\n(defn g [] (data :a))\n"])

(defdescribe treesitter-vs-clojure-reader-test
             (doseq [src clj-srcs]
               (it "every top-level form node read-backs to the reader's form (boundaries match)"
                   (let [reader-forms (read-forms src)
                         node-forms (->> (:children (z/inspect "clojure" src []))
                                         (map #(z/inspect "clojure" src [(:idx %)]))
                                         (map :text)
                                         (map read-string))]

                     ;; same count of top-level forms ...
                     (expect (= (count reader-forms) (count node-forms)))
                     ;; ... and each tree-sitter node delimits EXACTLY that form
                     (expect (= reader-forms node-forms))))))
