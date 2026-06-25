(ns com.blockether.vis.internal.foundation.editing.tree-sitter-langs-test
  "Cross-language stress test for the tree-sitter structural ZIPPER engine —
   ensures the grammar pack (com.blockether/tree-sitter-language-pack) is sound
   across many languages and edit shapes. Three checks per language:
     A. PARSE        — root parses to named children with no ERROR node.
     B. ROUND-TRIP   — replacing every top-level node with its OWN source text
                       yields a BYTE-IDENTICAL file (proves byte ranges/UTF-8
                       splicing are exact for that grammar — the property the
                       reconstruction footgun violated).
   Plus targeted CONTENT edits and SYNTAX-REFUSAL across paradigms."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.zipper :as z]
            [lazytest.core :refer [defdescribe it expect]]))

;; ---------------------------------------------------------------------------
;; ~46 languages, one tiny valid snippet each — every grammar in the pack we
;; lean on. detect-language maps the extension; inspect/edit drive the grammar.
;; ---------------------------------------------------------------------------
(def ^:private lang-bank
  [["clj" "(defn add [a b] (+ a b))\n"]
   ["py" "def add(a, b):\n    return a + b\n"]
   ["rs" "fn add(a: i32, b: i32) -> i32 { a + b }\n"]
   ["js" "function add(a, b) { return a + b; }\n"]
   ["ts" "function add(a: number, b: number): number { return a + b; }\n"]
   ["tsx" "const A = () => <div>hi</div>;\n"]
   ["go" "package m\nfunc add(a int, b int) int { return a + b }\n"]
   ["java" "class M { int add(int a, int b) { return a + b; } }\n"]
   ["rb" "def add(a, b)\n  a + b\nend\n"]
   ["c" "int add(int a, int b) { return a + b; }\n"]
   ["cpp" "int add(int a, int b) { return a + b; }\n"]
   ["h" "int add(int a, int b);\n"]
   ["cs" "class M { int Add(int a, int b) { return a + b; } }\n"]
   ["php" "<?php\nfunction add($a, $b) { return $a + $b; }\n"]
   ["swift" "func add(a: Int, b: Int) -> Int { return a + b }\n"]
   ["kt" "fun add(a: Int, b: Int): Int { return a + b }\n"]
   ["scala" "def add(a: Int, b: Int): Int = a + b\n"]
   ["hs" "add a b = a + b\n"]
   ["lua" "function add(a, b) return a + b end\n"]
   ["sh" "add() { echo $(($1 + $2)); }\n"]
   ["json" "{\"a\": 1, \"b\": [2, 3]}\n"]
   ["yaml" "a: 1\nb:\n  - 2\n  - 3\n"]
   ["toml" "a = 1\n[b]\nc = 2\n"]
   ["html" "<html><body><p>hi</p></body></html>\n"]
   ["css" "a { color: red; }\n"]
   ["scss" "a { color: red; .b { x: 1; } }\n"]
   ["sql" "select a, b from t where a = 1;\n"]
   ["ex" "defmodule M do\n  def add(a, b), do: a + b\nend\n"]
   ["erl" "add(A, B) -> A + B.\n"]
   ["ml" "let add a b = a + b\n"]
   ["r" "add <- function(a, b) a + b\n"]
   ["jl" "function add(a, b)\n  a + b\nend\n"]
   ["dart" "int add(int a, int b) => a + b;\n"]
   ["pl" "sub add { return $_[0] + $_[1]; }\n"]
   ["groovy" "def add(a, b) { a + b }\n"]
   ["zig" "fn add(a: i32, b: i32) i32 { return a + b; }\n"]
   ["nim" "proc add(a, b: int): int = a + b\n"]
   ["vim" "function! Add(a, b)\n  return a:a + a:b\nendfunction\n"]
   ["md" "# Title\n\nSome *text*.\n"]
   ["proto" "message M { int32 a = 1; }\n"]
   ["dockerfile" "FROM x\nRUN echo hi\n"]
   ["graphql" "type Q { a: Int }\n"]])

(defn- lang-of [ext] (z/detect-language (str "f." ext)))

;; ── A. PARSE: every grammar yields a non-error tree with named children ──
(defdescribe parse-coverage-test
  (doseq [[ext src] lang-bank]
    (it (str ext " parses to a clean named tree")
      (let [lang (lang-of ext)
            r    (z/inspect lang src [])]
        (expect (some? lang))
        (expect (:ok? r))
        (expect (pos? (:named-child-count r)))
        (expect (not (:has-error? r)))))))

;; ── B. BYTE ROUND-TRIP: replace each top-level node with its own text → the
;;      file must be byte-identical. Catches any byte-range / UTF-8 drift. ──
(defn- roundtrip-identity? [lang src]
  (let [root (z/inspect lang src [])]
    (every? (fn [{:keys [idx]}]
              (let [node (z/inspect lang src [idx])
                    r    (z/edit lang src [idx] :replace (:text node))]
                (and (:ok? r) (= src (:new-source r)))))
      (:children root))))

(defdescribe byte-roundtrip-test
  (doseq [[ext src] lang-bank]
    (it (str ext " round-trips every top-level node byte-for-byte")
      (expect (true? (roundtrip-identity? (lang-of ext) src))))))

;; ── C. CONTENT edits across paradigms: find the root child holding `needle`,
;;      replace it, assert the new text lands and the file still parses. ──
(def ^:private edit-cases
  [{:ext "clj" :needle "add" :code "(defn plus [a b] (* a b))" :want "plus" :gone "add"}
   {:ext "py"  :needle "add" :code "def plus(a, b):\n    return a * b" :want "plus" :gone "add"}
   {:ext "js"  :needle "add" :code "function plus(a, b) { return a * b; }" :want "plus"}
   {:ext "ts"  :needle "add" :code "function plus(a: number): number { return a; }" :want "plus"}
   {:ext "rs"  :needle "add" :code "fn plus(a: i32) -> i32 { a }" :want "plus"}
   {:ext "go"  :needle "func add" :code "func plus(a int) int { return a }" :want "plus"}
   {:ext "c"   :needle "add" :code "int plus(int a) { return a; }" :want "plus"}
   {:ext "java" :needle "add" :code "class M { int plus(int a) { return a; } }" :want "plus"}
   {:ext "rb"  :needle "add" :code "def plus(a)\n  a\nend" :want "plus"}
   {:ext "lua" :needle "add" :code "function plus(a) return a end" :want "plus"}
   {:ext "css" :needle "color" :code "a { color: blue; }" :want "blue" :gone "red"}
   {:ext "json" :needle "a" :code "{\"plus\": 9}" :want "plus"}
   {:ext "sql" :needle "select" :code "select x from u" :want "from u"}])

(defn- child-idx [lang src needle]
  (let [root (z/inspect lang src [])]
    (some (fn [{:keys [idx]}]
            (when (str/includes? (:text (z/inspect lang src [idx])) needle) idx))
      (:children root))))

(defdescribe content-edit-test
  (doseq [{:keys [ext needle code want gone]} edit-cases]
    (it (str ext " replaces a node and the file still parses")
      (let [lang (lang-of ext)
            src  (some (fn [[e s]] (when (= e ext) s)) lang-bank)
            i    (child-idx lang src needle)
            r    (z/edit lang src [i] :replace code)]
        (expect (some? i))
        (expect (:ok? r))
        (expect (str/includes? (:new-source r) want))
        (when gone (expect (not (str/includes? (:new-source r) gone))))
        ;; the spliced file re-parses cleanly
        (expect (not (:has-error? (z/inspect lang (:new-source r) []))))))))

;; ── D. SYNTAX REFUSAL: an unbalanced replacement is rejected (strict grammars) ──
(def ^:private refusal-cases
  [{:ext "clj" :code "(defn x"}
   {:ext "py"  :code "def f("}
   {:ext "rs"  :code "fn f("}
   {:ext "c"   :code "int f("}
   {:ext "json" :code "{\"a\":"}
   {:ext "go"  :code "func f("}])

(defdescribe syntax-refusal-test
  (doseq [{:keys [ext code]} refusal-cases]
    (it (str ext " refuses a syntax-breaking splice")
      (let [lang (lang-of ext)
            src  (some (fn [[e s]] (when (= e ext) s)) lang-bank)
            i    (child-idx lang src "add")
            i    (or i 0)
            r    (z/edit lang src [i] :replace code)]
        (expect (= :syntax-broken (get-in r [:error :reason])))))))
