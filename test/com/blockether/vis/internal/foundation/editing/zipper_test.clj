(ns com.blockether.vis.internal.foundation.editing.zipper-test
  "Language-neutral structural zipper over tree-sitter: parse → navigate by
   named-child path → splice-edit with syntax refusal."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.editing.core :as editing]
            [com.blockether.vis.internal.foundation.editing.zipper :as zip]
            [com.blockether.vis.internal.extension :as ext]
            ;; Side-effecting: registers the foundation editing extension at load
            ;; so the op->tag index covers sexpr/sexpr_edit (tool-success reads it).
            [com.blockether.vis.internal.foundation.core]
            [lazytest.core :refer [defdescribe it expect]]))

(def ^:private clj-src "(ns foo)\n(defn bar [x] (+ x 1))\n")

(def ^:private py-src "def foo(x):\n    return x + 1\n")

(defn- child-idx-containing
  [info needle]
  (some (fn [{:keys [idx head]}]
          (when (str/includes? (str head) needle) idx))
        (:children info)))

(defdescribe zipper-test
             (it "parses, navigates by path, and edits a Clojure tree"
                 (let [root (zip/inspect "clojure" clj-src [])]
                   (expect (:ok? root))
                   (expect (>= (:named-child-count root) 2))
                   (let
                     [i (child-idx-containing root "defn")
                      node (zip/inspect "clojure" clj-src [i])]

                     (expect (some? i))
                     (expect (str/includes? (:text node) "defn bar"))
                     ;; replace that whole form structurally
                     (let [r (zip/edit "clojure" clj-src [i] :replace "(defn bar [x] (* x 2))")]
                       (expect (:ok? r))
                       (expect (str/includes? (:new-source r) "(* x 2)"))
                       (expect (not (str/includes? (:new-source r) "(+ x 1)"))))
                     ;; a syntax-breaking replace is refused
                     (let [r (zip/edit "clojure" clj-src [i] :replace "(defn bar [x]")]
                       (expect (= :syntax-broken (get-in r [:error :reason])))))))
             (it "works language-neutrally on a Python tree"
                 (let [root (zip/inspect "python" py-src [])]
                   (expect (:ok? root))
                   (let
                     [i (child-idx-containing root "def foo")
                      node (zip/inspect "python" py-src [i])]

                     (expect (some? i))
                     (expect (str/includes? (:text node) "def foo"))
                     (let
                       [r (zip/edit "python" py-src [i] :replace "def foo(x):\n    return x * 2")]
                       (expect (:ok? r))
                       (expect (str/includes? (:new-source r) "x * 2"))))))
             (it "descends a deeper named-child path (the cursor going down)"
                 (let
                   [i
                    (child-idx-containing (zip/inspect "clojure" clj-src []) "defn")

                    deeper
                    (zip/inspect "clojure" clj-src [i 0])]

                   (expect (:ok? deeper))
                   (expect (string? (:kind deeper)))))
             (it "errors cleanly on a bad path"
                 (let [r (zip/inspect "clojure" clj-src [99])]
                   (expect (= :bad-path (get-in r [:error :reason]))))))

(defn- write-temp!
  [name content]
  (fs/create-dirs "target/editing-test")
  (let [rel (str "target/editing-test/" name)]
    (spit (fs/file rel) content)
    rel))

(defdescribe
  sexpr-verbs-test
  (it
    "sexpr navigates + struct_patch splices the SAME path (unified surface)"
    (let
      [sexpr
       @#'editing/nodes-tool

       struct-patch
       @#'editing/struct-patch-tool

       path
       (write-temp! "zip.clj" "(ns z)\n(defn g [x] (+ x 1))\n")

       root
       (first (get (:result (sexpr path)) "results"))]

      (expect (>= (get root "named_child_count") 2))
      (let
        [i (some (fn [c]
                   (when (str/includes? (str (get c "head")) "defn") (get c "idx")))
                 (get root "children"))]
        (expect (some? i))
        (expect (str/includes? (get (first (get (:result (sexpr path {"at" [i]})) "results"))
                                    "source")
                               "defn g"))
        ;; relative move sugar: at=[i], nav=["down"] resolves to [i 0]
        (expect (:success? (sexpr path {"at" [i] "nav" ["down"]})))
        ;; struct_patch takes the zipper PATH (sexpr_edit folded into it)
        (let [ed (struct-patch "path" path "at" [i] "op" "replace" "code" "(defn g [x] (* x 9))")]
          (expect (:success? ed))
          (expect (str/includes? (slurp (fs/file path)) "(* x 9)")))
        ;; syntax-breaking edit refused
        (expect (try (struct-patch "path" path "at" [i] "op" "replace" "code" "(defn g [x]")
                     false
                     (catch clojure.lang.ExceptionInfo _ true)))))))

(defdescribe
  op-keyword-regression-test
  (it "every structural op emits an op-keyword that resolves its registered tag"
      ;; struct_patch / project_references were long broken: their tools emitted a
      ;; DASH op (:struct-patch) while the registry key derived from the underscore
      ;; symbol (:struct_patch), so op-tag threw on every real invocation. Guard it.
      (doseq [op [:struct_nodes :struct_patch :struct_index :create-dirs :delete :patch]]
        (expect (#{:observation :mutation} (ext/op-tag op))))))

(defdescribe
  error-localization-test
  (it "error-nodes locates each ERROR/MISSING node with a 1-based line"
      ;; a `[` closed with `)` — the classic bracket-TYPE mismatch
      (let [errs (zip/error-nodes "clojure" "(defn f [x)\n  (+ x 1))\n")]
        (expect (seq errs))
        ;; tree-sitter names the delimiter it expected: a MISSING `]`
        (expect (some (fn [e]
                        (and (:missing? e) (= "]" (:kind e))))
                      errs))
        (expect (every? (fn [e]
                          (pos? (long (:line e))))
                        errs))))
  (it "error-nodes is empty on clean source"
      (expect (empty? (zip/error-nodes "clojure" "(defn f [x] (+ x 1))\n"))))
  (it "describe-syntax-errors names the location + the expected delimiter"
      (let [d (zip/describe-syntax-errors "clojure" "(defn f [x)\n  (+ x 1))\n")]
        (expect (string? d))
        (expect (str/includes? d "line"))
        (expect (str/includes? d "a `]` at line"))
        (expect (str/includes? d "delimiter TYPES"))
        ;; the broken lines are SHOWN with a caret, not just numbered
        (expect (str/includes? d "│"))
        (expect (str/includes? d "^")))
      ;; nil when the source parses clean
      (expect (nil? (zip/describe-syntax-errors "clojure" "(defn f [x] (+ x 1))"))))
  (it "a refused edit carries the located diagnostic in its message"
      (let
        [i
         (child-idx-containing (zip/inspect "clojure" clj-src []) "defn")

         r
         (zip/edit "clojure" clj-src [i] :replace "(defn bar [x)\n  (+ x 1))")]

        (expect (= :syntax-broken (get-in r [:error :reason])))
        ;; the message now includes a real line/col + the expected delimiter,
        ;; not just a bare "would introduce a syntax error"
        (expect (str/includes? (get-in r [:error :message]) "a `]` at line"))))
  (it "an unclosed form is located at the FORM, not at the file's first line"
      ;; tree-sitter opens ONE ERROR node over the whole file when a form is
      ;; left unclosed, so the node's own start is line 1 — reporting it sent
      ;; every "would break syntax" rejection to the `ns` form instead of to
      ;; the edit that broke.
      (let
        [src
         (str "(ns demo.core)\n\n" (apply str (repeat 40 "(defn ok [x] (inc x))\n"))
              "(defn boom [x]\n  (let [y (inc x)]\n    {:a y})\n\n"
              ;; forms AFTER the unclosed one are what makes tree-sitter
              ;; stretch one ERROR node over the whole file
              (apply str (repeat 5 "(defn after [x] (dec x))\n")))

         errs
         (zip/error-nodes "clojure" src)

         d
         (zip/describe-syntax-errors "clojure" src)]

        (expect (= 43 (long (:line (first errs)))))
        (expect (= "(" (:delimiter (first errs))))
        ;; the ERROR node itself still starts at line 1 — that is the artefact
        (expect (= 1 (long (:error-line (first errs)))))
        (expect (str/includes? d "break     line 43"))
        (expect (str/includes? d "unclosed  `(`"))
        (expect (not (str/includes? d "break     line 1 ")))))
  (it "an earlier actionable outer fault beats a later nested parse error"
      ;; The first unclosed form contains the later mismatch in the
      ;; recovery tree. A blanket "prefer leaf ERROR" rule blamed line
      ;; 8 even though the actionable `(` on line 3 is the first fault.
      (let
        [src
         (str "(ns demo)\n\n" "(defn boom [x]\n  (+ x 1)\n\n"
              "(def ok 1)\n\n" "(defn later [x] (+ x 1]))\n")

         d
         (zip/describe-syntax-errors "clojure" src)]

        (expect (str/includes? d "break     line 3"))
        (expect (str/includes? d "unclosed  `(`"))
        (expect (not (str/includes? d "break     line 8")))))
  (it "a stray closing delimiter is named as unmatched"
      (let [d (zip/describe-syntax-errors "clojure" "(def a 1)\n\n(defn f [x] (+ x 1)))\n")]
        (expect (str/includes? d "break     line 3"))
        (expect (str/includes? d "unmatched `)`"))))
  (it "an unterminated string is located at its quote, not at a class opener on line 1"
      (doseq [[lang return-type] [["java" "String"] ["csharp" "string"]]]
        (let
          [src (str "class Demo {\n"
                    (apply str
                      (for [i (range 1 10)]
                        (str "  int f" i "(int x) { return x + " i "; }\n")))
                    "  "
                    return-type
                    " boom() { return \"unterminated\n"
                    (apply str
                      (for [i (range 10 19)]
                        (str "  int f" i "(int x) { return x + " i "; }\n")))
                    "}\n")
           d (zip/describe-syntax-errors lang src)]

          (expect (str/includes? d "break     line 11"))
          (expect (not (str/includes? d "break     line 1 ")))
          (expect (str/includes? d "unclosed  string quote `\"`")))))
  (it "Unicode and CRLF do not shift the reported column or caret"
      (let
        [src
         "class Demo {\r\n  String boom() { String café = \"unterminated\r\n}\r\n"

         err
         (first (zip/error-nodes "java" src))

         d
         (zip/describe-syntax-errors "java" src)

         lines
         (str/split-lines d)

         code-line
         (first (filter #(and (str/includes? % "│") (str/includes? % "café")) lines))

         caret-line
         (first (filter #(str/includes? % "^") lines))]

        ;; `é` occupies two UTF-8 bytes but one user-facing character column.
        (expect (= 2 (long (:line err))))
        (expect (= 32 (long (:col err))))
        (expect (= 33 (long (:byte-col err))))
        (expect (str/includes? d "break     line 2 col 32"))
        (expect (= (.indexOf ^String code-line "\"") (.indexOf ^String caret-line "^"))))))
