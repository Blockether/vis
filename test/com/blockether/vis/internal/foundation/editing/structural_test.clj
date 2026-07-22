(ns com.blockether.vis.internal.foundation.editing.structural-test
  "Cross-language coverage for the tree-sitter outline + structural editing
   tools. Exercises Clojure, Python and Rust through the same unified API so a
   regression in any placement strategy (after-name / body / comment-before)
   is caught. The platform native FFI lib is resolved at runtime by
   com.blockether.tree-sitter-language-pack."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.index :as index]
            [com.blockether.vis.internal.foundation.editing.patch :as patch]
            [com.blockether.vis.internal.foundation.editing.structural :as structural]
            [com.blockether.vis.internal.foundation.editing.zipper :as zipper]
            [lazytest.core :refer [defdescribe expect it describe]]))

(def ^:private clj-src "(ns demo)\n(defn add [a b] (+ a b))\n(defn sub [a b] (- a b))\n")

(def ^:private py-src "def add(a, b):\n    return a + b\n")

(def ^:private rs-src "fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n")

(defn- edit [path src m] (structural/edit-source path src m))

(defn- throws? [f] (try (f) false (catch Exception _ true)))

(defdescribe
  outline-test
  (it "Clojure outline lists defs with anchors"
      (let [s (index/file-skeleton "demo.clj" clj-src)]
        ;; Clojure defs carry a structured visibility + a clean name — no
        ;; `^:private` glued on (pack >= .25). Public is the default, so the
        ;; skeleton leaves it implicit (only `private` is surfaced).
        (expect (str/includes? s "function add"))
        (expect (str/includes? s "function sub"))
        (expect (not (str/includes? s "public")))
        (expect (re-find #"@\d+:\w+\.\.\d+:\w+" s))))
  (it "Clojure outline shows clean names, visibility, and docstrings"
      (let
        [s (index/file-skeleton
             "demo.clj"
             "(def ^:private lim \"the cap\" 10)\n(defn pub \"hi there\" [x] x)\n")]
        ;; clean name (metadata stripped), `private` marker, arglist + docstring
        (expect (str/includes? s "constant private lim"))
        (expect (not (str/includes? s "^:private")))
        (expect (str/includes? s "\"the cap\""))
        (expect (str/includes? s "function pub  [x]"))
        (expect (str/includes? s "\"hi there\""))))
  (it "Python outline lists the function"
      (expect (str/includes? (index/file-skeleton "m.py" py-src) "function add")))
  (it "Rust outline lists the function"
      (expect (str/includes? (index/file-skeleton "m.rs" rs-src) "function add")))
  (it "unknown language yields no skeleton"
      (expect (nil? (index/file-skeleton "x.unknownext" "blah"))))
  (it
    "definitions returns STRUCTURED rows — same fields as an occurrences def (name/kind/visibility/signature/doc/anchor/end-anchor) plus nesting depth"
    (let
      [defs
       (index/definitions clj-src "clojure")

       add-def
       (first (filter #(= "add" (:name %)) defs))]

      (expect (= 3 (count defs))) ;; ns + add + sub
      (expect (= "fn" (:kind add-def)))
      (expect (= "public" (:visibility add-def)))
      (expect (= "[a b]" (:signature add-def)))
      (expect (= 0 (:depth add-def)))
      (expect (some? (:anchor add-def)))
      (expect (some? (:end-anchor add-def))))
    ;; NESTING: a Python class's methods report depth 1 under the depth-0 class
    (let [defs (index/definitions "class C:\n    def m(self):\n        return 1\n" "python")]
      (expect (= 0 (:depth (first (filter #(= "C" (:name %)) defs)))))
      (expect (= 1 (:depth (first (filter #(= "m" (:name %)) defs))))))))

(defdescribe zipper-anchor-path-test
             (let [src "(ns demo)\n\n(defn foo [x]\n  (+ x 1))\n\n(defn bar [y]\n  (* y 2))\n"]
               (it "resolves a fresh lineno:hash anchor to the node path for that row"
                   (let
                     [anchor (patch/line-anchor 6 "(defn bar [y]")
                      r (zipper/path-at-anchor "clojure" src anchor)]

                     (expect (:ok? r))
                     ;; root named children: ns, foo, bar
                     (expect (= [2] (:path r)))
                     (expect (= 6 (:line r)))))
               (it "refuses a stale anchor instead of silently landing on the line"
                   (let
                     [stale (patch/line-anchor 6 "(defn bar [y]")
                      changed (str/replace src "(defn bar [y]" "(defn bar [z]")
                      r (zipper/path-at-anchor "clojure" changed stale)]

                     (expect (= :hashline-not-found (get-in r [:error :reason])))))))

(defdescribe
  code-language-allowlist-test
  "`index/code-language` is the CURATED gate — only real code (+ strict configs)
   resolve; the pack's prose/markup/data grammars (`.txt`→vimdoc, `.md`, `.csv`,
   `.log`) return nil so a syntax guard never false-fires on them."
  (it "real code + strict-config extensions resolve to their language"
      (expect (= "clojure" (index/code-language "a.clj")))
      (expect (= "python" (index/code-language "a.py")))
      (expect (= "rust" (index/code-language "a.rs")))
      (expect (= "json" (index/code-language "a.json")))
      ;; EDN is Clojure-reader data — the pack's ext table omits `.edn`, so vis
      ;; maps it to the `clojure` grammar as a strict structured-config format.
      (expect (= "clojure" (index/code-language "deps.edn")))
      (expect (= "clojure" (index/detect-language "a/b/vis.edn"))))
  (it "prose / markup / data / unknown resolve to nil"
      (expect (nil? (index/code-language "a.txt")))   ;; pack → vimdoc
      (expect (nil? (index/code-language "a.md")))    ;; pack → markdown
      (expect (nil? (index/code-language "a.csv")))   ;; pack → csv
      (expect (nil? (index/code-language "a.log")))   ;; pack → nil
      (expect (nil? (index/code-language "README")))) ;; extensionless
  (it "detect-language still sees the pack's broad set (unchanged)"
      (expect (= "vimdoc" (index/detect-language "a.txt"))))
  (it "EDN files get real structural editing (node replace) via the clojure grammar"
      (let [deps "{:deps {foo/bar {:mvn/version \"1.0\"}}}\n"]
        (expect (= "{:deps {foo/bar {:mvn/version \"2.0\"}}}\n"
                   (edit "deps.edn" deps {:op :replace-node :match "\"1.0\"" :code "\"2.0\""}))))))

(defdescribe occurrences-test
             (it "Clojure: the definition is MARKED among the uses (kind/visibility/signature/span)"
                 (let
                   [src
                    "(defn add [a b] (+ a b))\n(def y (add 1 2))\n(println (add y 3))\n"

                    occ
                    (structural/occurrences "m.clj" src "add")

                    defs
                    (filterv :is-definition occ)

                    uses
                    (remove :is-definition occ)]

                   (expect (= 3 (count occ))) ;; 1 def + 2 uses
                   (expect (= 1 (count defs)))
                   (let [d (first defs)]
                     (expect (= 1 (patch/anchor->line (:anchor d)))) ;; anchor IS the position
                     (expect (= "fn" (:kind d)))
                     (expect (= "public" (:visibility d)))
                     (expect (= "[a b]" (:signature d)))
                     (expect (some? (:anchor d)))
                     (expect (some? (:end-anchor d)))) ;; span = :anchor..:end-anchor
                   (expect (every? #(and (:anchor %) (nil? (:is-definition %))) uses))))
             (it "Python: the def is marked even under a decorator; uses are not"
                 (let
                   [src
                    "@deco\ndef add(a, b):\n    return add(a, b)\ny = add(1, 2)\n"

                    occ
                    (structural/occurrences "m.py" src "add")

                    defs
                    (filterv :is-definition occ)]

                   (expect (= 1 (count defs)))
                   (expect (= "fn" (:kind (first defs))))
                   (expect (= 2 (patch/anchor->line (:anchor (first defs))))))) ;; the `def` line, not @decorator
             (it "Rust: the def is marked"
                 (let
                   [src
                    "pub fn add(a: i32) -> i32 { add(a) }\nfn main() { add(1); }\n"

                    occ
                    (structural/occurrences "m.rs" src "add")

                    defs
                    (filterv :is-definition occ)]

                   (expect (= 1 (count defs)))
                   (expect (= "fn" (:kind (first defs))))))
             (it "unknown language → empty"
                 (expect (= [] (structural/occurrences "x.unknownext" "add add" "add")))))

(defdescribe
  replace-test
  (it "Clojure replace by name"
      (expect (str/includes? (edit
                               "demo.clj"
                               clj-src
                               {:op :replace :target "add" :code "(defn add [a b c] (+ a b c))"})
                             "(defn add [a b c] (+ a b c))")))
  (it "Python replace by name"
      (expect (str/includes?
                (edit "m.py"
                      py-src
                      {:op :replace :target "add" :code "def add(a, b):\n    return a + b + 0"})
                "+ 0")))
  (it "rejects a syntax-breaking replace"
      (expect (throws?
                #(edit "demo.clj" clj-src {:op :replace :target "add" :code "(defn add [a b"})))))

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

;; The forward-reference case: a def that uses a dependency defined BELOW it.
(def ^:private fwd-src
  (str "(ns demo)\n\n"
       "(defn user [s]\n  (norm s))\n\n" ; uses norm before it's defined
       "(defn norm [s] s)\n"))

(defdescribe
  move-test
  (it "move_after relocates the node below its dependency (forward-ref fix)"
      (let [r (edit "demo.clj" fwd-src {:op :move-after :target "user" :anchor "norm"})]
        ;; norm now comes BEFORE user
        (expect (< (.indexOf ^String r "defn norm") (.indexOf ^String r "defn user")))
        ;; the moved body is intact
        (expect (str/includes? r "(defn user [s]\n  (norm s))"))))
  (it "move_before relocates above the anchor"
      (let [r (edit "demo.clj" clj-src {:op :move-before :target "sub" :anchor "add"})]
        (expect (< (.indexOf ^String r "defn sub") (.indexOf ^String r "defn add")))))
  (it "leaves whitespace ELSEWHERE in the file untouched (no file-wide rewrite)"
      (let
        [src
         (str "(ns demo)\n\n(defn a [x] x)\n\n\n\n(defn far [x] x)\n\n"
              "(defn mover [x] x)\n\n(defn anchor [x] x)\n")

         r
         (edit "demo.clj" src {:op :move-after :target "mover" :anchor "anchor"})]

        ;; the intentional 3-blank gap between `a` and `far` survives
        (expect (str/includes? r "(defn a [x] x)\n\n\n\n(defn far [x] x)"))))
  (it "refuses moving a node next to itself"
      (expect (throws? #(edit "demo.clj" clj-src {:op :move-after :target "add" :anchor "add"}))))
  (it "errors on an unknown target"
      (expect (throws? #(edit "demo.clj" clj-src {:op :move-after :target "nope" :anchor "add"}))))
  (it "works for Python too"
      (let
        [src
         "def user():\n    return norm()\n\ndef norm():\n    return 1\n"

         r
         (edit "m.py" src {:op :move-after :target "user" :anchor "norm"})]

        (expect (< (.indexOf ^String r "def norm") (.indexOf ^String r "def user"))))))

(defdescribe
  replace-node-test
  (it "replaces a unique sub-expression"
      (expect (str/includes?
                (edit "demo.clj" clj-src {:op :replace-node :match "(+ a b)" :code "(+ a b 1)"})
                "(+ a b 1)")))
  (it "refuses an ambiguous match without scope"
      (let [s "(defn f [] (+ a b))\n(defn g [] (+ a b))\n"]
        (expect (throws?
                  #(edit "demo.clj" s {:op :replace-node :match "(+ a b)" :code "(- a b)"})))))
  (it "scoping disambiguates"
      (let
        [s
         "(defn f [] (+ a b))\n(defn g [] (+ a b))\n"

         r
         (edit "demo.clj" s {:op :replace-node :match "(+ a b)" :code "(- a b)" :target "g"})]

        (expect (str/includes? r "(defn f [] (+ a b))"))
        (expect (str/includes? r "(defn g [] (- a b))")))))

(defdescribe
  defmethod-dispatch-test
  ;; A multimethod has many (defmethod NAME DISPATCH …) sharing NAME; the
  ;; index/struct_patch must target one by "NAME DISPATCH" (pack >= .19).
  (let [s "(defmethod area :circle [s] 1)\n(defmethod area :rect [s] 2)\n"]
    (it "outline distinguishes dispatch values"
        (let [sk (index/file-skeleton "demo.clj" s)]
          (expect (str/includes? sk "area :circle"))
          (expect (str/includes? sk "area :rect"))))
    (it "replace targets one defmethod by name+dispatch"
        (let
          [r (edit "demo.clj"
                   s
                   {:op :replace :target "area :rect" :code "(defmethod area :rect [s] 99)"})]
          (expect (str/includes? r "(defmethod area :rect [s] 99)"))
          (expect (str/includes? r "(defmethod area :circle [s] 1)"))))))

(defdescribe
  fuzzy-replace-node-test
  (it "matches a snippet despite different whitespace/line breaks"
      (let
        [s
         "(defn f [s]\n  (* 3\n     (:r s)))\n"

         r
         (edit "demo.clj" s {:op :replace-node :match "(* 3 (:r s))" :code "(* 9 (:r s))"})]

        (expect (str/includes? r "(* 9 (:r s))")))))

(defdescribe references-test
             (it "finds every occurrence with line + anchor"
                 (let
                   [s
                    "(defn area [r] (* r r))\n(def a (area 2))\n"

                    hits
                    (structural/references "demo.clj" s "area")]

                   (expect (= 2 (count hits)))
                   (expect (every? :anchor hits))
                   (expect (= [1 2] (mapv :line hits)))))
             (it "empty for an unknown language"
                 (expect (= [] (structural/references "x.zzz" "area" "area")))))

(defdescribe rename-test
             (it "renames an identifier everywhere (Clojure)"
                 (let
                   [s
                    "(defn add [a b] (+ a b))\n(def y (add 1 2))\n"

                    r
                    (edit "demo.clj" s {:op :rename :target "add" :code "plus"})]

                   (expect (str/includes? r "(defn plus [a b]"))
                   (expect (str/includes? r "(plus 1 2)"))
                   (expect (not (str/includes? r "add")))))
             (it "renames in Python"
                 (let
                   [s
                    "def add(a, b):\n    return add(a, b)\n"

                    r
                    (edit "m.py" s {:op :rename :target "add" :code "plus"})]

                   (expect (str/includes? r "def plus(a, b):"))
                   (expect (str/includes? r "return plus(a, b)"))))
             (it "rejects renaming a missing identifier"
                 (expect (throws? #(edit "demo.clj"
                                         "(defn f [] 1)\n"
                                         {:op :rename :target "nope" :code "x"})))))

;; ---------------------------------------------------------------------------
;; Many languages: every one should outline its function `add` and rename it.
;; ---------------------------------------------------------------------------
(def ^:private lang-cases
  [{:ext "clj" :src "(defn add [a b] (+ a b))\n"}
   {:ext "py" :src "def add(a, b):\n    return a + b\n"}
   {:ext "rs" :src "fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n"}
   {:ext "js" :src "function add(a, b) {\n  return a + b;\n}\n"}
   {:ext "ts" :src "function add(a: number, b: number): number {\n  return a + b;\n}\n"}
   {:ext "go" :src "package m\nfunc add(a int, b int) int {\n\treturn a + b\n}\n"}
   {:ext "java" :src "class M {\n  int add(int a, int b) {\n    return a + b;\n  }\n}\n"}
   {:ext "rb" :src "def add(a, b)\n  a + b\nend\n"}])

(defdescribe outline-many-langs-test
             (doseq [{:keys [ext src]} lang-cases]
               (it (str ext " outline contains add")
                   (expect (str/includes? (str (index/file-skeleton (str "f." ext) src)) "add")))))

(defdescribe rename-many-langs-test
             (doseq [{:keys [ext src]} lang-cases]
               (it (str ext " rename add -> plus")
                   (let [r (edit (str "f." ext) src {:op :rename :target "add" :code "plus"})]
                     (expect (str/includes? r "plus"))
                     (expect (not (str/includes? r "add")))))))

(defdescribe
  insert-ops-test
  (it "insert_before a definition"
      (expect (str/includes?
                (edit "demo.clj" clj-src {:op :insert-before :target "sub" :code "(def MARK 1)"})
                "(def MARK 1)\n\n(defn sub")))
  (it "insert_after a definition"
      (expect (str/includes?
                (edit "demo.clj" clj-src {:op :insert-after :target "add" :code "(def MARK 2)"})
                "(+ a b))\n\n(def MARK 2)")))
  (it "append at end of file"
      (expect (str/ends-with? (str/trimr
                                (edit "demo.clj" clj-src {:op :append :code "(def END 3)"}))
                              "(def END 3)")))
  (it "errors on a missing target (and names struct_index, not the engine's leaked `index`)"
      (let
        [msg (try (edit "demo.clj" clj-src {:op :replace :target "ghost" :code "x"})
                  nil
                  (catch Throwable e (.getMessage e)))]
        (expect (some? msg))
        (expect (str/includes? msg "struct_index(path)"))
        (expect (not (str/includes? msg "Use index(")))))
  (it "errors on an ambiguous target without kind"
      (let [s "(defn dup [] 1)\n(def dup 2)\n"]
        ;; two defs named dup, different kinds
        (expect (throws?
                  #(edit "demo.clj" s {:op :replace :target "dup" :code "(defn dup [] 9)"}))))))

(defdescribe replace-doc-langs-test
             (it "Python replace_doc swaps the docstring"
                 (let
                   [s
                    "def f():\n    \"\"\"old\"\"\"\n    return 1\n"

                    r
                    (edit "m.py" s {:op :replace-doc :target "f" :code "\"\"\"new\"\"\""})]

                   (expect (str/includes? r "\"\"\"new\"\"\""))
                   (expect (not (str/includes? r "old")))))
             (it "Python add_doc then has a doc (refuses a second add)"
                 (let
                   [s
                    "def g():\n    return 1\n"

                    r
                    (edit "m.py" s {:op :add-doc :target "g" :code "\"\"\"Doc.\"\"\""})]

                   (expect (str/includes? r "\"\"\"Doc.\"\"\""))
                   (expect (throws?
                             #(edit "m.py" r {:op :add-doc :target "g" :code "\"\"\"x\"\"\""}))))))

(defdescribe doc-ops-test
             (describe
               "replace_doc"
               (it "swaps an existing Clojure doc"
                   (let
                     [s
                      "(defn add \"old\" [a b] (+ a b))\n"

                      r
                      (edit "demo.clj" s {:op :replace-doc :target "add" :code "\"new\""})]

                     (expect (str/includes? r "\"new\""))
                     (expect (not (str/includes? r "\"old\"")))))
               (it "rejects add_doc when a doc already exists"
                   (let [s "(defn add \"old\" [a b] (+ a b))\n"]
                     (expect (throws?
                               #(edit "demo.clj" s {:op :add-doc :target "add" :code "\"x\""})))))))
