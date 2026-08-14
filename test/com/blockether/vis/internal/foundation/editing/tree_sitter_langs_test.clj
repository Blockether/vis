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
            [com.blockether.vis.internal.foundation.editing.index :as ix]
            [com.blockether.vis.internal.foundation.editing.structural :as st]
            [com.blockether.vis.internal.foundation.editing.zipper :as z]
            [lazytest.core :refer [defdescribe it expect]]))

;; ---------------------------------------------------------------------------
;; ~46 languages, one tiny valid snippet each — every grammar in the pack we
;; lean on. detect-language maps the extension; inspect/edit drive the grammar.
;; ---------------------------------------------------------------------------
(def ^:private lang-bank
  [["clj" "(defn add [a b] (+ a b))\n"] ["py" "def add(a, b):\n    return a + b\n"]
   ["rs" "fn add(a: i32, b: i32) -> i32 { a + b }\n"]
   ["js" "function add(a, b) { return a + b; }\n"]
   ["ts" "function add(a: number, b: number): number { return a + b; }\n"]
   ["tsx" "const A = () => <div>hi</div>;\n"]
   ["go" "package m\nfunc add(a int, b int) int { return a + b }\n"]
   ["java" "class M { int add(int a, int b) { return a + b; } }\n"]
   ["rb" "def add(a, b)\n  a + b\nend\n"] ["c" "int add(int a, int b) { return a + b; }\n"]
   ["cpp" "int add(int a, int b) { return a + b; }\n"] ["h" "int add(int a, int b);\n"]
   ["cs" "class M { int Add(int a, int b) { return a + b; } }\n"]
   ["php" "<?php\nfunction add($a, $b) { return $a + $b; }\n"]
   ["swift" "func add(a: Int, b: Int) -> Int { return a + b }\n"]
   ["kt" "fun add(a: Int, b: Int): Int { return a + b }\n"]
   ["scala" "def add(a: Int, b: Int): Int = a + b\n"] ["hs" "add a b = a + b\n"]
   ["lua" "function add(a, b) return a + b end\n"] ["sh" "add() { echo $(($1 + $2)); }\n"]
   ["json" "{\"a\": 1, \"b\": [2, 3]}\n"] ["yaml" "a: 1\nb:\n  - 2\n  - 3\n"]
   ["toml" "a = 1\n[b]\nc = 2\n"] ["html" "<html><body><p>hi</p></body></html>\n"]
   ["css" "a { color: red; }\n"] ["scss" "a { color: red; .b { x: 1; } }\n"]
   ["sql" "select a, b from t where a = 1;\n"]
   ["ex" "defmodule M do\n  def add(a, b), do: a + b\nend\n"] ["erl" "add(A, B) -> A + B.\n"]
   ["ml" "let add a b = a + b\n"] ["r" "add <- function(a, b) a + b\n"]
   ["jl" "function add(a, b)\n  a + b\nend\n"] ["dart" "int add(int a, int b) => a + b;\n"]
   ["pl" "sub add { return $_[0] + $_[1]; }\n"] ["groovy" "def add(a, b) { a + b }\n"]
   ["zig" "fn add(a: i32, b: i32) i32 { return a + b; }\n"]
   ["nim" "proc add(a, b: int): int = a + b\n"]
   ["vim" "function! Add(a, b)\n  return a:a + a:b\nendfunction\n"]
   ["md" "# Title\n\nSome *text*.\n"] ["proto" "message M { int32 a = 1; }\n"]
   ["dockerfile" "FROM x\nRUN echo hi\n"] ["graphql" "type Q { a: Int }\n"]
   ;; ── languages vis gained structural intelligence for in pack 1.12.3-blockether.27 ──
   ["mli" "val add : int -> int -> int\n"]
   ["nix" "{ pkgs }:\nlet\n  add = a: b: a + b;\nin add 1 2\n"]
   ["tf" "variable \"region\" {\n  default = \"eu\"\n}\n"] ["hcl" "job \"web\" {\n  count = 1\n}\n"]
   ["gradle" "def add(a, b) { a + b }\n"]
   ["svelte"
    "<script>\n  export let n = 1;\n  function bump() { n += 1; }\n</script>\n\n<button on:click={bump}>{n}</button>\n"]
   ["vue"
    "<template>\n  <p>{{ n }}</p>\n</template>\n\n<script>\nexport default { data() { return { n: 1 }; } };\n</script>\n"]])

(defn- lang-of [ext] (z/detect-language (str "f." ext)))

;; ── A. PARSE: every grammar yields a non-error tree with named children ──
(defdescribe parse-coverage-test
             (doseq [[ext src] lang-bank]
               (it (str ext " parses to a clean named tree")
                   (let
                     [lang (lang-of ext)
                      r (z/inspect lang src [])]

                     (expect (some? lang))
                     (expect (:ok? r))
                     (expect (pos? (:named-child-count r)))
                     (expect (not (:has-error? r)))))))

;; ── B. BYTE ROUND-TRIP: replace each top-level node with its own text → the
;;      file must be byte-identical. Catches any byte-range / UTF-8 drift. ──
(defn- roundtrip-identity?
  [lang src]
  (let [root (z/inspect lang src [])]
    (every? (fn [{:keys [idx]}]
              (let
                [node (z/inspect lang src [idx])
                 r (z/edit lang src [idx] :replace (:text node))]

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
   {:ext "py" :needle "add" :code "def plus(a, b):\n    return a * b" :want "plus" :gone "add"}
   {:ext "js" :needle "add" :code "function plus(a, b) { return a * b; }" :want "plus"}
   {:ext "ts" :needle "add" :code "function plus(a: number): number { return a; }" :want "plus"}
   {:ext "rs" :needle "add" :code "fn plus(a: i32) -> i32 { a }" :want "plus"}
   {:ext "go" :needle "func add" :code "func plus(a int) int { return a }" :want "plus"}
   {:ext "c" :needle "add" :code "int plus(int a) { return a; }" :want "plus"}
   {:ext "java" :needle "add" :code "class M { int plus(int a) { return a; } }" :want "plus"}
   {:ext "rb" :needle "add" :code "def plus(a)\n  a\nend" :want "plus"}
   {:ext "lua" :needle "add" :code "function plus(a) return a end" :want "plus"}
   {:ext "css" :needle "color" :code "a { color: blue; }" :want "blue" :gone "red"}
   {:ext "json" :needle "a" :code "{\"plus\": 9}" :want "plus"}
   {:ext "sql" :needle "select" :code "select x from u" :want "from u"}])

(defn- child-idx
  [lang src needle]
  (let [root (z/inspect lang src [])]
    (some (fn [{:keys [idx]}]
            (when (str/includes? (:text (z/inspect lang src [idx])) needle) idx))
          (:children root))))

(defdescribe content-edit-test
             (doseq [{:keys [ext needle code want gone]} edit-cases]
               (it (str ext " replaces a node and the file still parses")
                   (let
                     [lang (lang-of ext)
                      src (some (fn [[e s]]
                                  (when (= e ext) s))
                                lang-bank)
                      i (child-idx lang src needle)
                      r (z/edit lang src [i] :replace code)]

                     (expect (some? i))
                     (expect (:ok? r))
                     (expect (str/includes? (:new-source r) want))
                     (when gone (expect (not (str/includes? (:new-source r) gone))))
                     ;; the spliced file re-parses cleanly
                     (expect (not (:has-error? (z/inspect lang (:new-source r) []))))))))

;; ── D. SYNTAX REFUSAL: an unbalanced replacement is rejected (strict grammars) ──
(def ^:private refusal-cases
  [{:ext "clj" :code "(defn x"} {:ext "py" :code "def f("} {:ext "rs" :code "fn f("}
   {:ext "c" :code "int f("} {:ext "json" :code "{\"a\":"} {:ext "go" :code "func f("}])

(defdescribe syntax-refusal-test
             (doseq [{:keys [ext code]} refusal-cases]
               (it (str ext " refuses a syntax-breaking splice")
                   (let
                     [lang (lang-of ext)
                      src (some (fn [[e s]]
                                  (when (= e ext) s))
                                lang-bank)
                      i (child-idx lang src "add")
                      i (or i 0)
                      r (z/edit lang src [i] :replace code)]

                     (expect (= :syntax-broken (get-in r [:error :reason])))))))

;; ===========================================================================
;; SUBTLE / DEEP coverage — not just top-level nodes: nested classes, methods,
;; for-loops, if-conditions, comments. Snippets each carry a class/fn + a for
;; loop + an `if acc > 10` + line comments, so we can drill into sub-expressions.
;; ===========================================================================
(def ^:private deep-bank
  [["py"
    "# count things\nclass Counter:\n    def total(self, items):\n        acc = 0\n        for x in items:\n            acc += x  # add x\n        if acc > 10:\n            return acc\n        return 0\n"]
   ["js"
    "// count things\nclass Counter {\n  total(items) {\n    let acc = 0;\n    for (const x of items) {\n      acc += x; // add x\n    }\n    if (acc > 10) {\n      return acc;\n    }\n    return 0;\n  }\n}\n"]
   ["ts"
    "// count things\nclass Counter {\n  total(items: number[]): number {\n    let acc = 0;\n    for (const x of items) {\n      acc += x; // add x\n    }\n    if (acc > 10) {\n      return acc;\n    }\n    return 0;\n  }\n}\n"]
   ["java"
    "// count things\nclass Counter {\n  int total(int[] items) {\n    int acc = 0;\n    for (int x : items) {\n      acc += x; // add x\n    }\n    if (acc > 10) {\n      return acc;\n    }\n    return 0;\n  }\n}\n"]
   ["c"
    "// count things\nint total(int items[], int n) {\n  int acc = 0;\n  for (int i = 0; i < n; i++) {\n    acc += items[i]; // add\n  }\n  if (acc > 10) {\n    return acc;\n  }\n  return 0;\n}\n"]
   ["cpp"
    "// count things\nint total(int items[], int n) {\n  int acc = 0;\n  for (int i = 0; i < n; i++) {\n    acc += items[i]; // add\n  }\n  if (acc > 10) {\n    return acc;\n  }\n  return 0;\n}\n"]
   ["go"
    "package m\n\n// total sums items\nfunc total(items []int) int {\n\tacc := 0\n\tfor _, x := range items {\n\t\tacc += x // add x\n\t}\n\tif acc > 10 {\n\t\treturn acc\n\t}\n\treturn 0\n}\n"]
   ["rs"
    "// count things\nfn total(items: &[i32]) -> i32 {\n    let mut acc = 0;\n    for x in items {\n        acc += x; // add x\n    }\n    if acc > 10 {\n        return acc;\n    }\n    0\n}\n"]
   ["rb"
    "# count things\ndef total(items)\n  acc = 0\n  for x in items\n    acc += x # add x\n  end\n  if acc > 10\n    return acc\n  end\n  0\nend\n"]
   ["lua"
    "-- count things\nfunction total(items)\n  local acc = 0\n  for _, x in ipairs(items) do\n    acc = acc + x -- add x\n  end\n  if acc > 10 then\n    return acc\n  end\n  return 0\nend\n"]])

(defn- all-node-paths
  "Every node path in the tree (pre-order DFS) via repeated inspect. Small
   snippets only — re-parses per node, which is fine for tests."
  [lang src]
  (letfn [(walk [path]
            (let [info (z/inspect lang src path)]
              (cons path (mapcat #(walk (conj path %)) (range (:named-child-count info))))))]
    (walk [])))

(defn- find-path
  "Path to the FIRST node (DFS) whose inspect-info satisfies `pred`, or nil."
  [lang src pred]
  (some (fn [p]
          (when (pred (z/inspect lang src p)) p))
        (all-node-paths lang src)))

;; Deep byte round-trip: EVERY node at EVERY depth replaced with its own text
;; must yield a byte-identical file. The strongest per-grammar fidelity check.
(defdescribe deep-byte-roundtrip-test
             (doseq [[ext src] deep-bank]
               (it (str ext " round-trips EVERY nested node byte-for-byte")
                   (let [lang (lang-of ext)]
                     (expect (true? (every? (fn [p]
                                              (let
                                                [node (z/inspect lang src p)
                                                 r (z/edit lang src p :replace (:text node))]

                                                (and (:ok? r) (= src (:new-source r)))))
                                            (all-node-paths lang src))))))))

;; Deep targeted edit: reach the `if` condition `acc > 10` — buried inside the
;; if, inside the method body, inside the class — and rewrite it.
(defdescribe deep-condition-edit-test
             (doseq [[ext src] deep-bank]
               (it (str ext " edits a deep if-condition (acc > 10 -> acc > 5)")
                   (let
                     [lang (lang-of ext)
                      p (find-path lang src #(= "acc > 10" (str/trim (:text %))))
                      r (when p (z/edit lang src p :replace "acc > 5"))]

                     (expect (some? p))
                     (expect (:ok? r))
                     (expect (str/includes? (:new-source r) "acc > 5"))
                     (expect (not (str/includes? (:new-source r) "acc > 10")))
                     (expect (not (:has-error? (z/inspect lang (:new-source r) []))))))))

;; Deep statement edit: rewrite the loop body accumulator.
(defdescribe deep-loop-body-edit-test
             (doseq
               [[ext needle code] [["py" "acc += x" "acc -= x"] ["js" "acc += x" "acc -= x"]
                                   ["java" "acc += x" "acc -= x"] ["rs" "acc += x" "acc -= x"]
                                   ["go" "acc += x" "acc -= x"]]]
               (it (str ext " edits the loop-body accumulator statement")
                   (let
                     [lang (lang-of ext)
                      src (some (fn [[e s]]
                                  (when (= e ext) s))
                                deep-bank)
                      p (find-path lang src #(= needle (str/trim (:text %))))
                      r (when p (z/edit lang src p :replace code))]

                     (expect (some? p))
                     (expect (:ok? r))
                     (expect (str/includes? (:new-source r) "acc -= x"))
                     (expect (not (:has-error? (z/inspect lang (:new-source r) []))))))))

;; Comments are real nodes: locate one, confirm its kind, and round-trip it.
(defdescribe
  comment-node-test
  (doseq [[ext src] deep-bank]
    (it (str ext " exposes a comment node that round-trips")
        (let
          [lang (lang-of ext)
           p (find-path lang src #(str/includes? (str/lower-case (str (:kind %))) "comment"))
           node (when p (z/inspect lang src p))]

          (expect (some? p))
          (expect (str/includes? (str/lower-case (:kind node)) "comment"))
          ;; replacing the comment with itself is byte-identical
          (let [r (z/edit lang src p :replace (:text node))]
            (expect (:ok? r))
            (expect (= src (:new-source r))))))))

;; ===========================================================================
;; E. STRUCTURAL INTELLIGENCE per language — `index/file-index` must return real
;;    definitions (name + kind + nesting) and imports, not just a parse tree.
;;    Guards the pack's intel modules (ts-pack-core/src/intel/lang/*.rs) and
;;    vis's `code-languages` allowlist for the languages added in
;;    tree-sitter-language-pack 1.12.3-blockether.27.
;; ===========================================================================
(def ^:private intel-bank
  [{:path "a.hs"
    :lang "haskell"
    :src
    "module Main where\n\nimport Data.List (sort)\n\nadd :: Int -> Int -> Int\nadd a b = a + b\n\ndata Shape = Circle Double | Square Double\n\nmain :: IO ()\nmain = print (add 1 2)\n"
    :defs #{["Main" "module"] ["add" "fn"] ["Shape" "type"]}
    :imports ["import Data.List (sort)"]}
   {:path "a.ml"
    :lang "ocaml"
    :src
    "let add a b = a + b\n\ntype shape = Circle of float\n\nmodule M = struct\n  let x = 1\nend\n"
    :defs #{["add" "fn"] ["shape" "type"] ["M" "module"]}}
   {:path "a.mli"
    :lang "ocaml_interface"
    :src "val add : int -> int -> int\n\ntype shape = Circle of float\n"
    :defs #{["add" "fn"] ["shape" "type"]}}
   {:path "a.rs"
    :lang "rust"
    :src
    "use std::fmt;\n\npub struct P { x: i32 }\n\nimpl P {\n    pub fn new(x: i32) -> Self { P { x } }\n}\n\npub fn add(a: i32, b: i32) -> i32 { a + b }\n"
    :defs #{["P" "struct"] ["P" "impl"] ["new" "fn"] ["add" "fn"]}
    :nested #{["new" "fn"]}
    :imports ["use std::fmt;"]}
   {:path "a.groovy"
    :lang "groovy"
    :src "class Greeter {\n  String hi(String n) { \"hi $n\" }\n}\n\ndef add(a, b) { a + b }\n"
    :defs #{["Greeter" "class"] ["hi" "method"] ["add" "fn"]}
    :nested #{["hi" "method"]}}
   {:path "a.gradle"
    :lang "groovy"
    :src "plugins { id 'java' }\n\ndef add(a, b) { a + b }\n"
    :defs #{["add" "fn"]}}
   {:path "a.nix"
    :lang "nix"
    :src
    "{ pkgs ? import <nixpkgs> {} }:\nlet\n  add = a: b: a + b;\n  name = \"demo\";\nin pkgs.stdenv.mkDerivation { inherit name; }\n"
    :defs #{["add" "fn"] ["name" "constant"]}}
   {:path "a.tf"
    :lang "terraform"
    :src
    "variable \"region\" {\n  default = \"eu\"\n}\n\nresource \"aws_s3_bucket\" \"b\" {\n  bucket = \"x\"\n}\n\nmodule \"vpc\" {\n  source = \"./vpc\"\n}\n"
    :defs #{["region" "variable"] ["aws_s3_bucket.b" "resource"] ["vpc" "module"]}}
   {:path "a.hcl"
    :lang "hcl"
    :src "job \"web\" {\n  group \"g\" {\n    count = 1\n  }\n}\n"
    ;; HCL's label IS the block type — `job "web"` is a `job`, not an `other`
    :defs #{["web" "job"]}}
   {:path "a.graphql"
    :lang "graphql"
    :src "type Query {\n  user(id: ID!): User\n}\n\ninput NewUser { name: String }\n"
    :defs #{["Query" "type"] ["user" "field"] ["NewUser" "input"] ["name" "field"]}
    :nested #{["user" "field"] ["name" "field"]}}
   {:path "a.svelte"
    :lang "svelte"
    :src
    "<script>\n  import { onMount } from 'svelte';\n  export let n = 1;\n  function bump() { n += 1; }\n</script>\n\n<button on:click={bump}>{n}</button>\n"
    :defs #{["script" "script"] ["n" "variable"] ["bump" "fn"]}
    :nested #{["bump" "fn"]}
    ;; the <script> island's imports are lifted into HOST-file coordinates
    :imports ["import { onMount } from 'svelte';"]}
   {:path "a.vue"
    :lang "vue"
    :src
    "<template>\n  <p>{{ n }}</p>\n</template>\n\n<script>\nimport { ref } from 'vue';\nexport default {\n  data() { return { n: 1 }; },\n  methods: { bump() { this.n += 1; } }\n};\n</script>\n"
    :defs #{["template" "template"] ["script" "script"] ["data" "method"] ["bump" "method"]}
    :nested #{["bump" "method"]}
    :imports ["import { ref } from 'vue';"]}])

(defdescribe structural-intel-test
             (doseq [{:keys [path lang src defs nested imports]} intel-bank]
               (it (str path " indexes real definitions (" lang ")")
                   (let
                     [idx (ix/file-index path src)
                      rows (:definitions idx)
                      pairs (set (map (juxt :name :kind) rows))]

                     ;; the language is detected AND vetted as CODE (syntax guard applies)
                     (expect (= lang (ix/detect-language path)))
                     (expect (= lang (ix/code-language path)))
                     (expect (= lang (:language idx)))
                     ;; every expected definition is present with its exact kind
                     (expect (empty? (remove pairs defs)))
                     ;; nested defs really are nested (depth > 0), not flattened to top level
                     (doseq [want (or nested #{})]
                       (expect (some (fn [r]
                                       (and (= want ((juxt :name :kind) r)) (pos? (:depth r))))
                                     rows)))
                     ;; every row carries its line and its name
                     (expect (every? #(and (pos-int? (:line %)) (seq (:name %))) rows))
                     ;; imports are extracted (Svelte/Vue: lifted out of the <script> island)
                     (doseq [want (or imports [])]
                       (expect (some #(= want (:source %)) (:imports idx))))
                     ;; the skeleton renders and mentions the file
                     (expect (str/includes? (:skeleton idx) lang))))))

;; ===========================================================================
;; F. KIND LABELS — Rust's `StructureKind::Other("resource")` carries a payload
;;    a Java enum cannot hold. Until tree-sitter-language-pack
;;    1.12.3-blockether.28 the binding dropped it, so a GraphQL `type`, a
;;    Terraform `resource` and an Elixir `defmacro` all collapsed into one
;;    indistinguishable `other` bucket. `StructureItem/kindLabel` restores it and
;;    `index/item-kind` prefers it — for display AND for kind-targeted edits.
;; ===========================================================================
(def ^:private kind-label-bank
  [{:path "a.graphql"
    :src "type Query { me: User }\n\ninput NewUser { name: String }\n"
    :defs #{["Query" "type"] ["me" "field"] ["NewUser" "input"]}
    :sections ["types:" "inputs:"]
    :span ["Query" "type"]}
   {:path "a.tf"
    :src
    "variable \"region\" {\n  default = \"eu\"\n}\n\nresource \"aws_s3_bucket\" \"b\" {\n  bucket = \"x\"\n}\n\nlocals {\n  a = 1\n}\n\ndata \"aws_ami\" \"x\" {}\n"
    :defs #{["region" "variable"] ["aws_s3_bucket.b" "resource"] ["aws_ami.x" "data"]}
    ;; `data`/`locals` must NOT be naively pluralised into `datas`/`localss`
    :sections ["resources:" "variables:" "data:" "locals:"]
    :span ["aws_s3_bucket.b" "resource"]}
   {:path "a.ex"
    :src
    "defmodule M do\n  defmacro mac(x) do\n    quote do: unquote(x)\n  end\n\n  def f(y), do: y\nend\n"
    :defs #{["M" "module"] ["mac" "macro"] ["f" "fn"]}
    :sections ["macros:"]
    ;; a macro is only ever nested inside its module, so it exercises the
    ;; label on a NESTED row instead of a top-level span lookup
    :nested ["mac" "macro"]}
   ;; every other language whose intel emits `StructureKind::Other(..)`:
   ;; clojure's arbitrary `def*` heads, ocaml exceptions, generic HCL blocks,
   ;; groovy blocks and the web single-file-component section tags.
   {:path "a.clj"
    :src "(defthing foo 1)\n\n(defrecord R [a])\n"
    :defs #{["foo" "defthing"] ["R" "struct"]}
    :sections ["defthings:" "structs:"]
    :span ["foo" "defthing"]}
   {:path "a.ml"
    :src "exception Boom\n\nlet f x = x + 1\n"
    :defs #{["Boom" "exception"] ["f" "fn"]}
    :sections ["exceptions:"]
    :span ["Boom" "exception"]}
   {:path "a.hcl"
    :src "job \"web\" {\n  group \"g\" {\n  }\n}\n"
    :defs #{["web" "job"]}
    :sections ["jobs:"]
    :span ["web" "job"]}
   {:path "a.groovy"
    :src "task hello {\n  doLast {\n    println 'hi'\n  }\n}\n"
    :defs #{["task hello" "block"]}
    :sections ["blocks:"]
    :span ["task hello" "block"]}
   {:path "a.vue"
    :src
    "<template>\n  <p>hi</p>\n</template>\n\n<script>\nexport default {}\n</script>\n\n<style>\np { color: red; }\n</style>\n"
    :defs #{["template" "template"] ["script" "script"] ["style" "style"]}
    :sections ["templates:" "scripts:" "styles:"]
    :span ["script" "script"]}
   {:path "a.svelte"
    :src "<script>\n  let a = 1;\n</script>\n\n<style>\n  p { color: red; }\n</style>\n"
    :defs #{["script" "script"] ["style" "style"]}
    :sections ["scripts:" "styles:"]
    :span ["style" "style"]
    :nested ["a" "variable"]}])

(defdescribe kind-label-test
             (doseq [{:keys [path src defs sections span nested]} kind-label-bank]
               (it (str path " reports the Other(..) kind label, never a bare `other`")
                   (let
                     [idx (ix/file-index path src)
                      rows (:definitions idx)
                      pairs (set (map (juxt :name :kind) rows))
                      lang (:language idx)
                      [target kind] span]

                     ;; the labelled kinds arrive verbatim on every definition row
                     (expect (empty? (remove pairs defs)))
                     ;; nothing degraded into the generic bucket
                     (expect (not-any? #(= "other" (:kind %)) rows))
                     ;; and the skeleton groups them under real section headings
                     (doseq [want sections]
                       (expect (str/includes? (:skeleton idx) want)))
                     (expect (not (str/includes? (:skeleton idx) "other:")))
                     ;; a labelled kind is also SELECTABLE: it must resolve a span,
                     ;; and a wrong kind must resolve none (no accidental match-all)
                     (when span
                       (expect (some? (ix/node-span src lang target kind)))
                       (expect (nil? (ix/node-span src lang target "interface"))))
                     ;; a labelled NESTED row keeps its label too
                     (when nested
                       (expect (some (fn [r]
                                       (and (= nested ((juxt :name :kind) r)) (pos? (:depth r))))
                                     rows)))))))

;; ── E. SPAN END: an exclusive end position must not swallow the next node ──
;; Several grammars let a definition node run to column 0 of the FOLLOWING line
;; (Groovy `command`) and even past the blank rows before the next sibling.
;; A line-based splice that trusts that end deletes whatever starts there.
(def ^:private groovy-span-src
  (str "class Greeter {\n"
       "    String hi(String n) { \"hi $n\" }\n" "}\n"
       "\n" "interface I { int m() }\n"
       "\n" "enum E { A, B }\n"
       "\n" "def add(a, b) { a + b }\n"))

(defdescribe span-end-test
             (it "a span ending at column 0 stops on its last content line"
                 ;; 0-based: `interface I { int m() }` is row 4 and nothing else
                 (expect (= [4 4] (ix/node-span groovy-span-src "groovy" "I" nil))))
             (it "a kind-targeted replace keeps the definitions that follow"
                 (let
                   [out (st/edit-source
                          "a.groovy"
                          groovy-span-src
                          {:op :replace :target "I" :code "interface I { int m(); long n() }"})]
                   (expect (str/includes? out "long n()"))
                   (expect (str/includes? out "enum E { A, B }"))
                   (expect (str/includes? out "def add(a, b)"))
                   (expect (str/includes? out "class Greeter {")))))

;; ---------------------------------------------------------------------------
;; SPAN TORTURE — the property behind the `endLineOf` fix, checked over layouts
;; designed to break it: blank rows before a closing delimiter, several blank
;; rows between definitions, no blank rows at all, nested definitions, heredocs
;; and template literals holding blank lines, unicode, CRLF, a leading gap and a
;; trailing gap.
;; ---------------------------------------------------------------------------

(def ^:private span-torture-bank
  {"a.groovy"
   "class Alpha {\n  void a() {}\n\n}\n\n\n\ninterface Beta {\n  int m()\n}\n\n\n\nenum Delta { A, B }\n"
   "A.java" "class Alpha {\n\n  int m() {\n    return 1;\n  }\n\n}\n\nclass Beta {\n}\n"
   "a.py"
   "class Alpha:\n    def a(self):\n        pass\n\n    def b(self):\n        pass\n\n\n\nclass Beta:\n    pass\n"
   "u.py"
   "def αlpha():\n    s = \"🚀中文\"\n    return s\n\n\ndef beta():\n    return \"café naïve\"\n"
   "a.rs" "pub mod outer {\n    pub fn inner() {\n    }\n\n}\n\npub fn beta() {}\n"
   "a.ex"
   "defmodule Alpha do\n  defmacro mac(x) do\n    quote do: unquote(x)\n  end\n\nend\n\ndefmodule Beta do\n  def run, do: :ok\nend\n"
   "a.tf"
   "resource \"aws_s3_bucket\" \"alpha\" {\n  bucket = \"x\"\n\n}\n\n\n\nvariable \"beta\" {\n  type = string\n}\n"
   "a.graphql" "type Alpha {\n  n: Int\n\n}\n\n\ntype Beta {\n  m: Int\n}\n"
   "a.ts"
   "export function alpha() {\n  const s = `a\n\nb`;\n  return s;\n}\n\nexport class Beta {\n\n  m() {}\n\n}\n"
   "a.js" "function alpha() {}\nfunction beta() {}\nclass Delta { m() {} }\n"
   "a.rb" "def alpha\n  <<~TXT\n    a\n\n    b\n  TXT\nend\n\ndef beta\n  2\nend\n"
   "a.hcl"
   "job \"alpha\" {\n  group \"g\" {\n    count = 1\n\n  }\n\n}\n\nservice \"beta\" {\n  port = 80\n}\n"
   "a.yaml" "alpha:\n  n: 1\n\n\nbeta:\n  m: |\n    text\n\n    more\n\ndelta:\n  k: 3\n"})

(defn- torture-variants
  "The same source under layouts that stress trailing-blank-row handling."
  [src]
  {:plain src
   :trailing-blanks (str src "\n\n\n")
   :leading-blanks (str "\n\n" src)
   :wide-gaps (str/replace src "\n\n" "\n\n\n\n")
   :crlf (str/replace src "\n" "\r\n")})

(defn- torture-rows
  "Definitions with their 1-based start/end lines under the torture-test names."
  [path src]
  (mapv #(assoc %
           :start (:line %)
           :end (:end-line %))
        (ix/definitions src (ix/detect-language path))))

(defn- own-text
  "A row's own source, never reaching into the next same-or-shallower row."
  [lines rows ^long i]
  (let
    [row
     (nth rows i)

     cap
     (or (some (fn [r]
                 (when (<= (long (:depth r)) (long (:depth row))) (dec (long (:start r)))))
               (subvec rows (inc i)))
         (:end row))]

    (str/join "\n"
              (subvec lines
                      (dec (long (:start row)))
                      (max (long (:start row)) (min (count lines) (long (:end row)) (long cap)))))))

(defn- span-findings
  "Every way this file's spans could be wrong: a span that runs past EOF, ends on
   a blank row, inverts, overlaps its next sibling or escapes its parent — and,
   the end-to-end property, a replace of a definition with its OWN text that is
   not byte-identical (an overshooting end splices the NEXT definition away)."
  [path src]
  (let
    [lines
     (vec (str/split src #"\n" -1))

     n
     (count lines)

     rows
     (torture-rows path src)

     out
     (atom [])

     bad!
     (fn [& xs]
       (swap! out conj (str/join " " (cons path (map str xs)))))]

    (doseq [row rows]
      (let [{:keys [name kind start end]} row]
        (if-not (and start end)
          (bad! "missing span" name)
          (do (when (> (long start) (long end)) (bad! "start>end" name start end))
              (when (> (long end) n) (bad! "end past EOF" name end n))
              (when (and (<= (long end) n) (str/blank? (nth lines (dec (long end)) "x")))
                (bad! "end line blank" name start end))
              (when (= "other" (str kind)) (bad! "bare other kind" name))))))
    (doseq [[a b] (partition 2 1 rows)]
      (if (> (long (:depth b)) (long (:depth a)))
        (when-not (and (>= (long (:start b)) (long (:start a)))
                       (<= (long (:end b)) (long (:end a))))
          (bad! "child escapes parent" (:name a) (:name b)))
        (when (>= (long (:end a)) (long (:start b)))
          (bad! "sibling overlap"
                (:name a)
                [(:start a) (:end a)]
                (:name b)
                [(:start b) (:end b)]))))
    (let
      [unique (set (for
                     [[k v] (frequencies (map :name rows))
                      :when (and (= 1 v) (string? k) (seq k) (str/includes? src k))]

                     k))]
      (doseq
        [i (range (count rows))
         :let [row (nth rows i)]
         :when (unique (:name row))]

        (let
          [res (try (st/edit-source path
                                    src
                                    {:op :replace
                                     :target (:name row)
                                     :kind (:kind row)
                                     :code (own-text lines rows i)})
                    (catch Exception e (.getMessage e)))]
          (when-not (= res src)
            (bad! "replacing" (:name row)
                  "with its own text is not identity —" (first (str/split-lines (str res))))))))
    @out))

(defdescribe span-torture-test
             (it "spans stay inside their own definition across nasty layouts"
                 (let
                   [findings (for
                               [[path src] span-torture-bank
                                [_ variant] (torture-variants src)
                                :when (nil? (z/describe-syntax-errors (ix/detect-language path)
                                                                      variant))
                                f (span-findings path variant)]

                               f)]
                   (expect (= [] (vec findings)))))
             (it "the identity property really catches a wrong span end"
                 ;; Guard the guard: an end four rows too long swallows `interface Beta`,
                 ;; and such an edit must be refused rather than silently applied.
                 (let
                   [src
                    (get span-torture-bank "a.groovy")

                    lines
                    (vec (str/split src #"\n" -1))

                    overshoot
                    (str/join "\n" (subvec lines 0 8))]

                   (expect (= :refused
                              (try (st/edit-source
                                     "a.groovy"
                                     src
                                     {:op :replace :target "Alpha" :kind "class" :code overshoot})
                                   (catch Exception _ :refused)))))))

;; ---------------------------------------------------------------------------
;; LINE-ENDING FIDELITY. A structural edit is line surgery: it must return the
;; file's own terminator (and its own CR) untouched. Two real regressions this
;; guards: `StructuralApi.splice` trimmed the trailing "" element that IS the
;; final newline (INSERT_AFTER on the LAST definition stripped it in every
;; language, and Groovy — whose grammar demands a terminating newline — had the
;; edit REFUSED outright), and vis' own `move-source` rebuilt the file with
;; `str/split-lines`, which drops both the final newline and every CR.
;; ---------------------------------------------------------------------------
(def ^:private newline-bank
  {"a.py" "def alpha(x):\n    return x + 1\n\n\ndef beta():\n    return 2\n"
   "a.groovy"
   "class Alpha {\n    String hi(String n) { \"hi $n\" }\n}\n\ndef beta(a, b) { a + b }\n"
   "a.rs" "fn alpha() -> i32 { 1 }\n\nfn beta() -> i32 { 2 }\n"
   "a.rb" "def alpha\n  1\nend\n\ndef beta\n  2\nend\n"
   "a.go" "package main\n\nfunc alpha() int { return 1 }\n\nfunc beta() int { return 2 }\n"
   "a.java"
   "class Alpha {\n    int a() { return 1; }\n}\n\nclass Beta {\n    int b() { return 2; }\n}\n"
   "a.clj" "(defn alpha [] 1)\n\n(defn beta [] 2)\n"
   "a.ts"
   "export function alpha(): number { return 1; }\n\nexport function beta(): number { return 2; }\n"
   "a.lua" "function alpha() return 1 end\n\nfunction beta() return 2 end\n"})

(def ^:private newline-insert
  "A syntactically harmless line to splice in, per language."
  {"a.py" "# tail"
   "a.groovy" "// tail"
   "a.rs" "// tail"
   "a.rb" "# tail"
   "a.go" "// tail"
   "a.java" "// tail"
   "a.clj" ";; tail"
   "a.ts" "// tail"
   "a.lua" "-- tail"})

(defn- top-names
  "Top-level definition names of `src`, in source order."
  [path src]
  (->> (ix/definitions src (ix/detect-language path))
       (filter #(zero? (long (or (:depth %) 0))))
       (keep :name)
       vec))

(defn- lone-lf?
  "True when `s` contains a bare LF that is not part of a CRLF pair."
  [^String s]
  (boolean (re-find #"(?<!\r)\n" s)))

(defdescribe
  newline-fidelity-test
  (it "insert_after the last definition keeps the file's final newline"
      (let
        [findings (for
                    [[path src] (sort newline-bank)
                     :let [names (top-names path src)
                           last-name (last names)
                           res (try (st/edit-source path
                                                    src
                                                    {:op :insert-after
                                                     :target last-name
                                                     :code (get newline-insert path)})
                                    (catch Exception e (.getMessage e)))]
                     :when (or (not (string? res))
                               (not (str/ends-with? res "\n"))
                               (not (str/includes? res (str (first names)))))]

                    [path last-name (str res)])]
        (expect (= [] (vec findings)))))
  (it "moving a definition keeps the final newline and every other definition"
      (let
        [findings (for
                    [[path src] (sort newline-bank)
                     :let [names (top-names path src)
                           res (try (st/edit-source
                                      path
                                      src
                                      {:op :move-before :target (last names) :anchor (first names)})
                                    (catch Exception e (.getMessage e)))]
                     :when (or (not (string? res))
                               (not (str/ends-with? res "\n"))
                               (not (every? #(str/includes? res %) names)))]

                    [path names (str res)])]
        (expect (= [] (vec findings)))))
  (it "a CRLF file stays CRLF through insert and move"
      (let
        [findings
         (for
           [[path src] (sort newline-bank)
            :let [crlf (str/replace src "\n" "\r\n")
                  names (top-names path crlf)]
            [op m] [[:insert-after
                     {:op :insert-after :target (last names) :code (get newline-insert path)}]
                    [:move-before {:op :move-before :target (last names) :anchor (first names)}]]
            :let [res (try (st/edit-source path crlf m) (catch Exception e (.getMessage e)))]
            :when (or (not (string? res)) (not (str/ends-with? res "\r\n")) (lone-lf? res))]

           [path op (pr-str res)])]
        (expect (= [] (vec findings))))))

(defdescribe replace-node-identity-test
             (it "replace_node with a node's own text touches nothing around it"
                 ;; The snippet match is whitespace-NORMALISED, so the winning node's byte
                 ;; span can reach past the snippet — Groovy and Elixir end a definition node
                 ;; ON the following newline. Splicing that span glued the next definition to
                 ;; the replacement (or ate the file's final newline).
                 (let
                   [findings
                    (for
                      [[path src] (sort newline-bank)
                       :let [lang (ix/detect-language path)
                             lines (vec (str/split src #"\n" -1))]
                       row (remove #(pos? (long (or (:depth %) 0))) (ix/definitions src lang))
                       :let [s (dec (long (:line row)))
                             e (dec (long (:end-line row)))
                             own (str/join "\n" (subvec lines s (inc e)))
                             res
                             (try (st/edit-source path src {:op :replace-node :match own :code own})
                                  (catch Exception ex (.getMessage ex)))]
                       :when (not= res src)]

                      [path (:name row) (pr-str res)])]
                   (expect (= [] (vec findings))))))

(def doc-bank
  "Definitions that carry NO doc yet, with the comment syntax their language docs with.
   Svelte's function lives inside a `<script>` body the host grammar keeps as one opaque
   raw-text node, so it has no comment NODE above it — the case that broke last."
  {"A.java" ["public class Alpha {\n  void a() {}\n}\n" "/** New doc. */"]
   "a.go" ["func Alpha() int {\n\treturn 1\n}\n" "// New doc."]
   "a.rs" ["pub fn alpha() -> i32 {\n    1\n}\n" "/// New doc."]
   "a.ts" ["export function alpha(): number {\n  return 1;\n}\n" "/** New doc. */"]
   "a.zig" ["pub fn alpha() i32 {\n    return 1;\n}\n" "/// New doc."]
   "a.svelte" ["<script>\nexport function alpha() {\n  return 1;\n}\n</script>\n\n<p>hi</p>\n"
               "/** New doc. */"]})

(defn- doc-target
  "The deepest definition named like the sample's subject."
  [path src]
  (let [rows (ix/definitions src (ix/detect-language path))]
    (:name (or (first (filter #(#{"Alpha" "alpha"} (:name %)) rows)) (first rows)))))

(defn- try-edit
  "The edited source, or a map carrying the refusal message — never a bare string for failure."
  [path src m]
  (try (st/edit-source path src m) (catch Exception e {:error (.getMessage e)})))

(defdescribe
  doc-comment-test
  (it "add_doc adds a comment doc once, then refuses a second one"
      ;; A comment doc is not a docstring NODE, so the doc ops used to ignore it:
      ;; add_doc stacked a second comment and replace_doc claimed there was none.
      (let
        [findings (for
                    [[path [src doc]] (sort doc-bank)
                     :let [target (doc-target path src)
                           added (try-edit path src {:op :add-doc :target target :code doc})
                           again (when (string? added)
                                   (try-edit path added {:op :add-doc :target target :code doc}))]
                     :when (or (not (string? added))
                               (not (str/includes? added doc))
                               (not (str/ends-with? added "\n"))
                               (str/includes? (str again) (str doc "\n" doc)))]

                    [path target (pr-str added) (pr-str again)])]
        (expect (= [] (vec findings)))))
  (it "replace_doc rewrites the comment doc in place"
      (let
        [findings
         (for
           [[path [src doc]] (sort doc-bank)
            :let [target (doc-target path src)
                  added (try-edit path src {:op :add-doc :target target :code doc})
                  second-doc (str/replace doc "New" "Second")
                  res (when (string? added)
                        (try-edit path added {:op :replace-doc :target target :code second-doc}))]
            :when (or (not (string? res))
                      (not (str/includes? res second-doc))
                      (str/includes? res doc)
                      (not= (count (str/split-lines added)) (count (str/split-lines res))))]

           [path target (pr-str res)])]
        (expect (= [] (vec findings)))))
  (it "an existing doc comment is never counted twice or lost"
      (let
        [findings (for
                    [[path [src doc]] (sort doc-bank)
                     :let [target (doc-target path src)
                           documented (try-edit path src {:op :add-doc :target target :code doc})
                           refused
                           (when (string? documented)
                             (try-edit path documented {:op :add-doc :target target :code doc}))]
                     :when (string? refused)]

                    [path target (pr-str refused)])]
        (expect (= [] (vec findings))))))
