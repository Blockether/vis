(ns com.blockether.vis.internal.foundation.editing.structural-test
  "Cross-language coverage for the tree-sitter outline + structural editing
   tools. Exercises Clojure, Python and Rust through the same unified API so a
   regression in any placement strategy (after-name / body / comment-before)
   is caught. The platform native FFI lib is resolved at runtime by
   com.blockether.tree-sitter-language-pack."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.index :as index]
            [com.blockether.vis.internal.foundation.editing.structural :as structural]
            [com.blockether.vis.internal.foundation.editing.zipper :as zipper]
            [lazytest.core :refer [defdescribe expect it describe]]))

(def ^:private clj-src "(ns demo)\n(defn add [a b] (+ a b))\n(defn sub [a b] (- a b))\n")

(def ^:private py-src "def add(a, b):\n    return a + b\n")

(def ^:private rs-src "fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n")

(defn- edit [path src m] (structural/edit-source path src m))

(defn- throws? [f] (try (f) false (catch Exception _ true)))

(def ^:dynamic *scan-probe* "Binding-conveyance probe for `scan-mapv`'s worker pool." :unbound)

(defdescribe
  outline-test
  (it "Clojure outline lists defs with line ranges"
      (let [s (index/file-skeleton "demo.clj" clj-src)]
        ;; Clojure defs carry a structured visibility + a clean name — no
        ;; `^:private` glued on (pack >= .25). Public is the default, so the
        ;; skeleton leaves it implicit (only `private` is surfaced).
        (expect (str/includes? s "function add"))
        (expect (str/includes? s "function sub"))
        (expect (not (str/includes? s "public")))
        (expect (re-find #"@\d+\.\.\d+" s))))
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
    "definitions returns STRUCTURED rows — same fields as an occurrences def (name/kind/visibility/signature/doc/line/end-line) plus nesting depth"
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
      (expect (= 2 (:line add-def)))
      (expect (= 2 (:end-line add-def))))
    ;; NESTING: a Python class's methods report depth 1 under the depth-0 class
    (let [defs (index/definitions "class C:\n    def m(self):\n        return 1\n" "python")]
      (expect (= 0 (:depth (first (filter #(= "C" (:name %)) defs)))))
      (expect (= 1 (:depth (first (filter #(= "m" (:name %)) defs))))))))

(defdescribe zipper-line-path-test
             (let [src "(ns demo)\n\n(defn foo [x]\n  (+ x 1))\n\n(defn bar [y]\n  (* y 2))\n"]
               (it "resolves a struct_index row's line to the node path for that row"
                   (let [r (zipper/path-at-line "clojure" src 6)]
                     (expect (:ok? r))
                     ;; root named children: ns, foo, bar
                     (expect (= [2] (:path r)))
                     (expect (= 6 (:line r)))))
               (it "refuses a line that starts no node instead of guessing one"
                   (let [r (zipper/path-at-line "clojure" src 5)]
                     (expect (= :line-no-node (get-in r [:error :reason])))))
               (it "refuses a line number that is not 1-based"
                   (expect (= :invalid-line
                              (get-in (zipper/path-at-line "clojure" src 0) [:error :reason]))))))

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
                     (expect (= 1 (:line d))) ;; the line IS the position
                     (expect (= "fn" (:kind d)))
                     (expect (= "public" (:visibility d)))
                     (expect (= "[a b]" (:signature d)))
                     (expect (some? (:line d)))
                     (expect (some? (:end-line d)))) ;; span = :line..:end-line
                   (expect (every? #(and (:line %) (nil? (:is-definition %))) uses))))
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
                   (expect (= 2 (:line (first defs)))))) ;; the `def` line, not @decorator
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

(defdescribe occurrences-in-test
             (it
               "the BATCH traces every name in one pass, identically to per-name calls"
               (let
                 [src
                  "(defn add [a b] (+ a b))\n(defn mul [a b] (* a b))\n(def y (add (mul 1 2) 3))\n"

                  names
                  ["add" "mul" "y" "b" "absent"]

                  batched
                  (structural/occurrences-in "m.clj" src names)]

                 (doseq [n names]
                   (expect (= (structural/occurrences "m.clj" src n) (get batched n []))))
                 (expect (= 2 (count (get batched "add")))) ;; the def + one use
                 (expect (= 4 (count (get batched "b"))))   ;; params and uses, both defns
                 (expect (= 1 (count (filterv :is-definition (get batched "mul")))))
                 (expect (nil? (get batched "absent")))))   ;; never occurs → absent, not empty
             (it "duplicate names collapse; no names / unknown language → empty map"
                 (let [src "(defn add [a b] (+ a b))\n"]
                   (expect (= (structural/occurrences-in "m.clj" src ["add"])
                              (structural/occurrences-in "m.clj" src ["add" "add"])))
                   (expect (= {} (structural/occurrences-in "m.clj" src [])))
                   (expect (= {} (structural/occurrences-in "x.unknownext" "add add" ["add"]))))))

(defdescribe
  scan-batch-test
  (it "scan-mapv keeps REQUEST ORDER across workers"
      (let [items (vec (range 200))]
        (expect (= items (structural/scan-mapv #(do (Thread/sleep (long (rand-int 2))) %) items)))
        (expect (= [] (structural/scan-mapv identity [])))
        (expect (= [:a] (structural/scan-mapv identity [:a])))))
  (it "scan-mapv rethrows the ORIGINAL ex-info, never an ExecutionException"
      (let
        [thrown (try (structural/scan-mapv (fn [i]
                                             (if (= i 7) (throw (ex-info "boom" {:i i})) i))
                                           (vec (range 64)))
                     nil
                     (catch Exception e e))]
        (expect (instance? clojure.lang.ExceptionInfo thrown))
        (expect (= {:i 7} (ex-data thrown)))))
  ;; Regression: workers ran with the ROOT bindings, because Clojure conveys a
  ;; thread binding to `future`/`pmap` but never to a raw Java pool. A caller's
  ;; per-turn dynamic state (the workspace filesystem roots) read as unbound
  ;; inside `f`, so a batch of two behaved differently from a batch of one, which
  ;; runs inline on the calling thread.
  (it "scan-mapv conveys the caller's dynamic bindings into every worker"
      (binding [*scan-probe* :bound]
        (expect (= [:bound]
                   (structural/scan-mapv (fn [_]
                                           *scan-probe*)
                                         [1])))
        (expect (= (repeat 8 :bound)
                   (seq (structural/scan-mapv (fn [_]
                                                *scan-probe*)
                                              (vec (range 8)))))))
      ;; ...and the pooled thread is left exactly as it was found, so the next
      ;; batch cannot inherit the previous caller's bindings.
      (expect (= (repeat 8 :unbound)
                 (seq (structural/scan-mapv (fn [_]
                                              *scan-probe*)
                                            (vec (range 8)))))))
  (it "occurrences-in-files traces every path in one parallel pass"
      (let
        [sources
         {"a.clj" "(defn add [a b] (+ a b))\n"
          "b.clj" "(def y (add 1 2))\n"
          "c.clj" "(defn unrelated [] nil)\n"}

         scans
         (structural/occurrences-in-files (vec (keys sources)) ["add"] sources)]

        ;; REQUEST ORDER, one row per path, `read-fn` owns the read.
        (expect (= (vec (keys sources)) (mapv :path scans)))
        (expect (= 1 (count (get (:occurrences (first scans)) "add"))))
        (expect (:is-definition (first (get (:occurrences (first scans)) "add"))))
        (expect (= 1 (count (get (:occurrences (second scans)) "add"))))
        (expect (= {} (:occurrences (last scans))))))
  (it "occurrences-in-files batches MIXED languages in ONE pack pass"
      (let
        [sources
         {"a.clj" "(defn add [a b] (+ a b))\n"
          "b.py" "def add(a, b):\n    return a + b\n"
          "c.unknownext" "add add add\n"}

         scans
         (structural/occurrences-in-files (vec (keys sources)) ["add"] sources)]

        ;; Every language in the batch is resolved once, up front, by the pack.
        (expect (= (vec (keys sources)) (mapv :path scans)))
        (expect (:is-definition (first (get (:occurrences (first scans)) "add"))))
        (expect (:is-definition (first (get (:occurrences (second scans)) "add"))))
        ;; A path with no known language is a SCANNED row with nothing found —
        ;; never an error, and never handed to the pack.
        (expect (= {} (:occurrences (last scans))))
        (expect (nil? (:error (last scans))))))
  (it "occurrences-in-files is TOTAL: an unreadable path is one :error row"
      (let
        [scans (structural/occurrences-in-files ["ok.clj" "missing.clj"]
                                                ["add"]
                                                (fn [p]
                                                  (if (= p "ok.clj")
                                                    "(defn add [a b] (+ a b))\n"
                                                    (throw (java.io.FileNotFoundException.
                                                             "missing.clj")))))]
        (expect (= ["ok.clj" "missing.clj"] (mapv :path scans)))
        (expect (nil? (:error (first scans))))
        (expect (string? (:error (second scans))))
        (expect (nil? (:occurrences (second scans))))))
  (it "no names → no scan at all"
      (expect (= []
                 (structural/occurrences-in-files ["a.clj"]
                                                  []
                                                  (fn [_]
                                                    (throw (AssertionError. "read"))))))))

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
        (expect (str/includes? msg "struct_index({\"paths\": [path]})"))
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

;; =============================================================================
;; Body docstrings (Python) — lookup cost
;; =============================================================================

(defn- py-doc-source
  "`n` documented top-level Python defs — the shape that makes the pack emit a
   result-level docstrings list instead of a per-item `docComment`."
  [n]
  (apply str
    (for [i (range n)]
      (str "def fn_"
           i
           "(a, b):\n"
           "    \"\"\"Docstring number " i
           " explaining the function.\"\"\"\n" "    return a + b\n\n"))))

(defn- min-index-ms
  "BEST (minimum) wall-clock ms to index `source`, across `batches` runs after a
   warmup. The MIN reflects the cost when the scheduler gave a clean slice, so a
   shared runner's noise cannot inflate it (same tactic as the hashline bench)."
  [source batches]
  (dotimes [_ 2]
    (index/file-index "bench.py" source))
  (reduce min
          (for [_ (range batches)]
            (let [t0 (System/nanoTime)]
              (index/file-index "bench.py" source)
              (/ (- (System/nanoTime) t0) 1e6)))))

(defdescribe
  python-body-docstring-test
  "Python carries its doc INSIDE the body, so the structure tagger leaves
   `docComment` empty and the pack surfaces docs as a separate result-level list.
   `doc-snippet` resolves each definition against that list — once per definition,
   and every definition is rendered twice (skeleton line + machine row) — so
   scanning the whole list was O(defs x docstrings): 4 -> 14 -> 54 -> 212 ms
   across 500/1000/2000/4000 documented defs, quadrupling per doubling while the
   parse only doubled. The list is indexed by the associated NAME now."
  (it "keeps duplicate method names bound to their own class's docstring"
      (let
        [src
         (str "class Alpha:\n" "    def go(self):\n"
              "        \"\"\"Alpha go doc.\"\"\"\n" "        return 1\n\n"
              "class Beta:\n" "    def go(self):\n"
              "        \"\"\"Beta go doc.\"\"\"\n" "        return 2\n")

         defs
         (index/definitions src "python")

         docs
         (->> defs
              (filter #(= "go" (:name %)))
              (mapv :doc))]

        ;; Both methods share a name; only the span decides, exactly as the flat
        ;; scan did — a name-keyed lookup that ignored spans would give both the
        ;; same doc.
        (expect (= 2 (count docs)))
        (expect (= ["Alpha go doc." "Beta go doc."] docs))))
  (it "indexes 4x the documented defs in far less than 4x-squared the time"
      (let
        [small
         (min-index-ms (py-doc-source 750) 3)

         big
         (min-index-ms (py-doc-source 3000) 3)]

        ;; 4x the input: linear work lands near 4x, the quadratic lookup landed
        ;; near 16x. The generous 8x bound never flakes on scheduler noise yet
        ;; still trips the moment the per-definition scan comes back.
        (expect (< big (* 8.0 small))))))

(defdescribe
  clojure-meta-docstring-test
  "A Clojure var can carry its doc in a `^{:doc \"…\"}` METADATA map instead of the
   docstring position, and the pack reports that shape nowhere: no `docComment`,
   and no entry in the result-level docstrings list. Every metadata-documented
   var — the whole sandbox tool surface (`read_session`, `list_sessions`,
   `shell`, `mcp_*`) documents itself this way — indexed with a blank doc until
   `doc-snippet` learned to read the definition's own metadata head."
  (it "reads :doc from var metadata on def and defonce"
      (let
        [src
         (str "(ns demo)\n" "(def ^{:doc \"Alpha metadata doc.\"} alpha 1)\n"
              "(defonce ^:private ^{:doc \"Beta metadata doc.\"} beta 2)\n"
              "(defn plain-fn \"Fn docstring.\" [] 1)\n")

         by-name
         (into {} (map (juxt :name identity)) (index/definitions src "clojure"))]

        (expect (= "Alpha metadata doc." (:doc (get by-name "alpha"))))
        (expect (= "Beta metadata doc." (:doc (get by-name "beta"))))
        ;; The ordinary docstring path must keep working untouched.
        (expect (= "Fn docstring." (:doc (get by-name "plain-fn"))))))
  (it "never mistakes a :doc inside the VALUE for the var's own doc"
      (let
        [src
         (str "(ns demo)\n"
              ;; Both traps at once: the doc TEXT contains the var's own name,
              ;; and the VALUE contains a competing :doc key.
              "(def ^{:doc \"probe runs probes; the :doc below is data.\"}\n" "  probe\n"
              "  {:doc \"value map doc\" :handler nil})\n" "(def plain {:doc \"value only\"})\n")

         by-name
         (into {} (map (juxt :name identity)) (index/definitions src "clojure"))]

        (expect (= "probe runs probes; the :doc below is data." (:doc (get by-name "probe"))))
        ;; No metadata head at all: a :doc in the value is not documentation.
        (expect (nil? (:doc (get by-name "plain")))))))
