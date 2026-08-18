(ns com.blockether.vis.internal.foundation.editing.parse-test
  "Language detection and located parse errors — the two verdicts `patch`'s
   syntax gate spends."
  (:require [com.blockether.vis.internal.foundation.editing.parse :as parse]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest detect-language-test
  (testing "the pack's own table answers for ordinary source"
    (is (= "clojure" (parse/detect-language "src/a/b.clj")))
    (is (= "python" (parse/detect-language "a/b/c.py"))))
  (testing "Clojure-family extensions the pack omits fall back to the clojure grammar"
    (is (= "clojure" (parse/detect-language "a/b/vis.edn"))))
  (testing "detection stays broad — prose grammars are recognized, then excluded by policy"
    (is (= "vimdoc" (parse/detect-language "a.txt")))
    (is (contains? parse/code-languages "clojure"))
    (is (not (contains? parse/code-languages "vimdoc")))
    (is (not (contains? parse/code-languages "markdown")))))

(deftest error-nodes-test
  (testing "each ERROR/MISSING node carries a 1-based line, and the expected delimiter"
    ;; a `[` closed with `)` — the classic bracket-TYPE mismatch
    (let [errs (parse/error-nodes "clojure" "(defn f [x)\n  (+ x 1))\n")]
      (is (seq errs))
      (is (some (fn [e]
                  (and (:missing? e) (= "]" (:kind e))))
                errs))
      (is (every? (fn [e]
                    (pos? (long (:line e))))
                  errs))))
  (testing "clean source has no error nodes"
    (is (empty? (parse/error-nodes "clojure" "(defn f [x] (+ x 1))\n"))))
  (testing "an unclosed form is located at the FORM, not at the file's first line"
    ;; tree-sitter opens ONE ERROR node over the whole file when a form is left
    ;; unclosed, so the node's own start is line 1 — reporting that sent every
    ;; refusal to the `ns` form instead of to the edit that broke.
    (let [src
          (str "(ns demo.core)\n\n" (apply str (repeat 40 "(defn ok [x] (inc x))\n"))
               "(defn boom [x]\n  (let [y (inc x)]\n    {:a y})\n\n"
               (apply str (repeat 5 "(defn after [x] (dec x))\n")))

          errs
          (parse/error-nodes "clojure" src)]

      (is (= 43 (long (:line (first errs)))))
      (is (= "(" (:delimiter (first errs))))
      ;; the ERROR node itself still starts at line 1 — that is the artefact
      (is (= 1 (long (:error-line (first errs)))))))
  (testing "Unicode and CRLF do not shift the reported column"
    ;; `é` occupies two UTF-8 bytes but one user-facing character column.
    (let [src
          "class Demo {\r\n  String boom() { String café = \"unterminated\r\n}\r\n"

          err
          (first (parse/error-nodes "java" src))]

      (is (= 2 (long (:line err))))
      (is (= 32 (long (:col err))))
      (is (= 33 (long (:byte-col err))))))
  (testing "an unparseable language fails open with no rows"
    (is (= [] (parse/error-nodes nil "(defn f [x)")))))

(deftest top-level-nodes-test
  (testing "the root's NAMED children, one level deep, in document order"
    (let [nodes (parse/top-level-nodes
                  "toml"
                  "# [tool.uv] in a comment\n[tool.uv]\nx = 1\n\n[[tool.uv.index]]\n")]
      (is (= ["comment" "table" "table_array_element"] (mapv :kind nodes)))
      ;; punctuation is skipped, so a table's header key is its child 0 — the
      ;; whole point for a caller reading DECLARATIONS out of a config file.
      (is (= ["tool.uv" "x = 1"] (mapv :text (:children (nth nodes 1)))))
      (is (= "tool.uv.index" (:text (first (:children (nth nodes 2))))))))
  (testing "a declaration written inside prose is not a declaration"
    (let [nodes (parse/top-level-nodes "toml" "desc = \"see [tool.uv]\"\n")]
      (is (= ["pair"] (mapv :kind nodes)))))
  (testing "no language answers nothing, and unparseable input declares nothing"
    (is (= [] (parse/top-level-nodes nil "[tool.uv]\n")))
    (is (= ["ERROR"] (mapv :kind (parse/top-level-nodes "toml" "[[[ broken"))))))
