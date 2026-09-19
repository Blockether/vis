(ns com.blockether.vis.internal.foundation.editing.parse-test
  "Language detection and located parse errors — the two verdicts `patch`'s
   syntax gate spends, from tree-sitter and from a registered language surface."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.editing.parse :as parse]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

(deftest detect-language-test
  (testing "the pack's own table answers for ordinary source"
    (is (= "clojure" (parse/detect-language "src/a/b.clj")))
    (is (= "python" (parse/detect-language "a/b/c.py"))))
  (testing "representative expanded languages are reachable from real filenames"
    (is (= "solidity" (parse/guarded-language "contracts/token.sol")))
    (is (= "powershell" (parse/guarded-language "scripts/build.ps1")))
    (is (= "proto" (parse/guarded-language "wire/session.proto")))
    (is (= "json5" (parse/guarded-language "config/settings.json5")))
    (is (= "wgsl" (parse/guarded-language "shaders/main.wgsl"))))
  (testing "Clojure-family extensions the pack omits fall back to the clojure grammar"
    (is (= "clojure" (parse/detect-language "a/b/manifest.edn"))))
  (testing "detection stays broad — prose grammars are recognized, then excluded by policy"
    (is (= "vimdoc" (parse/detect-language "a.txt")))
    (is (contains? parse/code-languages "clojure"))
    (is (not (contains? parse/code-languages "vimdoc")))
    (is (not (contains? parse/code-languages "markdown")))))

(deftest expanded-code-languages-test
  (testing "newly guarded grammars accept a minimal document and reject a stray closer"
    (doseq [lang #{"ada" "bicep" "c3" "cairo" "capnp" "clarity" "commonlisp" "crystal" "cuda" "cue"
                   "d" "dhall" "elisp" "erlang" "fish" "fortran" "fsharp" "gdscript" "gleam" "glsl"
                   "haxe" "hlsl" "json5" "jsonnet" "kdl" "matlab" "nim" "objc" "odin" "pascal" "pkl"
                   "powershell" "prisma" "proto" "purescript" "racket" "rego" "rescript" "ron"
                   "scheme" "solidity" "starlark" "systemverilog" "tcl" "thrift" "typespec" "v"
                   "verilog" "vhdl" "wat" "wgsl" "zsh"}]
      (is (empty? (parse/error-nodes lang
                                     (get {"jsonnet" "{}"
                                           "json5" "{}"
                                           "ron" "()"
                                           "powershell" "$x = 1"
                                           "dhall" "True"
                                           "prisma"
                                           "generator client { provider = \"prisma-client-js\" }"}
                                          lang
                                          "")))
          lang)
      (is (seq (parse/error-nodes lang ")")) lang))))

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

(deftest transition-verdict-test
  (testing "one shared verdict distinguishes clean, introduced, and pre-existing errors"
    (is (= :clean (:status (parse/transition-verdict "clojure" "(def x 1)" "(def x 2)"))))
    (is (= :introduced-error (:status (parse/transition-verdict "clojure" "(def x 1)" ")"))))
    (is (= :still-broken (:status (parse/transition-verdict "clojure" "(" ")"))))
    (is (= :unguarded (:status (parse/transition-verdict nil "anything" ")"))))))

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

(defn- exclamation-verdict
  "A fixture syntax verdict in the JSON spelling a Python surface answers: a line
   ending in `!` is an unclosed delimiter, anything else is clean."
  [{:keys [language source]}]
  (let [faults (into []
                     (comp (map-indexed (fn [i line]
                                          [(inc (long i)) line]))
                           (filter (fn [[_ line]]
                                     (str/ends-with? line "!")))
                           (map
                             (fn [[n line]]
                               {"line" n "col" 0 "kind" "unclosed" "delimiter" "!" "text" line})))
                     (str/split-lines (str source)))]
    {"language" language "is_clean" (empty? faults) "findings" faults}))

(defn- fixture-surface
  "One extension declaring a language surface for `.vislang` files — a language
   tree-sitter has no grammar for — whose syntax verdict is `syntax-fn`."
  [syntax-fn]
  [{:ext/name "fixture-language"
    :ext/language-tools [{:language "fictional" :extensions ["vislang"] :syntax-fn syntax-fn}]}])

(defn- clojure-surface
  "One extension claiming the SYNTAX verdict for clojure, which tree-sitter already
   parses — the precedence D2 grants a declared handler."
  [syntax-fn]
  [{:ext/name "rival-language" :ext/language-tools [{:language "clojure" :syntax-fn syntax-fn}]}])

(deftest surface-declared-language-test
  (testing "a file type tree-sitter does not map is neither detected nor guarded"
    (is (nil? (parse/detect-language "notes/thing.vislang")))
    (is (nil? (parse/guarded-language "notes/thing.vislang"))))
  (testing "a registered surface claims the extension and guards its language"
    (with-redefs [extension/registered-extensions (constantly (fixture-surface
                                                                exclamation-verdict))]
      (is (= "fictional" (parse/detect-language "notes/thing.vislang")))
      (is (= "fictional" (parse/detect-language "notes/THING.VISLANG")))
      (is (= "fictional" (parse/guarded-language "notes/thing.vislang")))))
  (testing "a surface with no syntax verdict names its language but does not guard it"
    (with-redefs [extension/registered-extensions (constantly [{:ext/name "fixture-language"
                                                                :ext/language-tools
                                                                [{:language "fictional"
                                                                  :extensions [".vislang"]
                                                                  :format-fn identity}]}])]
      (is (= "fictional" (parse/detect-language "notes/thing.vislang")))
      (is (nil? (parse/guarded-language "notes/thing.vislang")))))
  (testing "built-in detection is untouched while a surface is registered"
    (with-redefs [extension/registered-extensions (constantly (fixture-surface
                                                                exclamation-verdict))]
      (is (= "clojure" (parse/detect-language "src/a/b.clj")))
      (is (= "clojure" (parse/guarded-language "src/a/b.clj")))
      (is (= "python" (parse/guarded-language "a/b/c.py"))))))

(deftest surface-syntax-verdict-test
  (testing "a registered verdict answers for a language tree-sitter cannot parse"
    (with-redefs [extension/registered-extensions (constantly (fixture-surface
                                                                exclamation-verdict))]
      (is (= [] (parse/error-nodes "fictional" "fine\n")))
      (let [err (first (parse/error-nodes "fictional" "fine\nbroken!\n"))]
        (is (= 2 (long (:line err))))
        (is (= 0 (long (:col err))))
        (is (= "unclosed" (:kind err)))
        (is (false? (:missing? err)))
        (is (= "broken!" (:text err)))
        ;; fields beyond the rows the gate reads survive normalization
        (is (= "!" (:delimiter err))))))
  (testing "the write gate refuses an edit that introduces a fault in that language"
    (with-redefs [extension/registered-extensions (constantly (fixture-surface
                                                                exclamation-verdict))]
      (is (= :clean (:status (parse/transition-verdict "fictional" "fine\n" "still fine\n"))))
      (is (= :introduced-error
             (:status (parse/transition-verdict "fictional" "fine\n" "broken!\n"))))
      (is (= :still-broken
             (:status (parse/transition-verdict "fictional" "broken!\n" "worse!\n"))))))
  (testing "Clojure keyword findings and a missing kind normalize the same way"
    (with-redefs [extension/registered-extensions
                  (constantly
                    (fixture-surface
                      (fn [{:keys [language]}]
                        {:language language
                         :is-clean false
                         :findings
                         [{:line 3 :col 4 :kind :missing :expected ")" :message "expected )"}]})))]
      (let [err (first (parse/error-nodes "fictional" "anything"))]
        (is (= 3 (long (:line err))))
        (is (= 4 (long (:col err))))
        (is (= "missing" (:kind err)))
        (is (true? (:missing? err)))
        (is (= "expected )" (:text err))))))
  (testing "a not-clean verdict with no usable finding still refuses the write"
    (with-redefs [extension/registered-extensions
                  (constantly (fixture-surface
                                (fn [{:keys [language]}]
                                  {"language" language "is_clean" false "findings" []})))]
      (let [err (first (parse/error-nodes "fictional" "anything"))]
        (is (= 1 (long (:line err))))
        (is (= "parse" (:kind err)))))))

(deftest surface-syntax-fallback-test
  (testing "a throwing handler falls back to the built-in verdict"
    (with-redefs [extension/registered-extensions
                  (constantly (clojure-surface (fn [_]
                                                 (throw (ex-info "handler is broken" {})))))]
      (is (seq (parse/error-nodes "clojure" "(defn f [x)")))
      (is (empty? (parse/error-nodes "clojure" "(defn f [x] x)")))))
  (testing "a result the contract refuses falls back to the built-in verdict"
    (with-redefs [extension/registered-extensions (constantly (clojure-surface (fn [_]
                                                                                 {"is_clean"
                                                                                  false})))]
      (is (seq (parse/error-nodes "clojure" "(defn f [x)")))
      (is (empty? (parse/error-nodes "clojure" "(defn f [x] x)")))))
  (testing "a language name no grammar answers to stays unguarded instead of throwing"
    (is (= [] (parse/error-nodes "fictional" "broken!")))
    (is (nil? (parse/guarded-language "notes/thing.vislang")))))
