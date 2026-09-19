(ns com.blockether.vis.internal.foundation.editing.parse-test
  "Language detection and located parse errors — the two verdicts `patch`'s syntax
   gate spends. Vis owns no parser, so every verdict here comes from a registered
   language surface and a language nobody claims stays unguarded."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.foundation.editing.parse :as parse]
            [lazytest.experimental.interfaces.clojure-test :refer [deftest is testing]]))

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
  "One extension declaring a language surface for `.vislang` files — a file type vis
   has no built-in name for — whose syntax verdict is `syntax-fn`."
  [syntax-fn]
  [{:ext/name "fixture-language"
    :ext/language-tools [{:language "fictional" :extensions ["vislang"] :syntax-fn syntax-fn}]}])

(defn- clojure-surface
  "One extension claiming the SYNTAX verdict for clojure, a language vis detects on
   its own but never judges by itself."
  [syntax-fn]
  [{:ext/name "rival-language" :ext/language-tools [{:language "clojure" :syntax-fn syntax-fn}]}])

(defmacro ^:private with-no-surfaces
  "Body runs with an empty extension registry, so a verdict that never arrives is
   this test's own doing and not another namespace's leftover registration."
  [& body]
  `(with-redefs [extension/registered-extensions (constantly [])]
     ~@body))

(deftest detect-language-test
  (testing "the built-in table names the languages vis ships tools for"
    (is (= "clojure" (parse/detect-language "src/a/b.clj")))
    (is (= "clojure" (parse/detect-language "a/b/manifest.edn")))
    (is (= "clojure" (parse/detect-language "bb.edn")))
    (is (= "python" (parse/detect-language "a/b/c.py")))
    (is (= "python" (parse/detect-language "A/B/C.PY"))))
  (testing "every other file type is nameless until a surface claims it"
    (with-no-surfaces (is (nil? (parse/detect-language "a.txt")))
                      (is (nil? (parse/detect-language "README.md")))
                      (is (nil? (parse/detect-language "contracts/token.sol")))
                      (is (nil? (parse/detect-language "Makefile"))))))

(deftest guarded-language-test
  (testing "detection alone gates nothing — vis carries no parser of its own"
    (with-no-surfaces (is (nil? (parse/guarded-language "src/a/b.clj")))
                      (is (nil? (parse/guarded-language "a/b/c.py")))
                      (is (nil? (parse/guarded-language "notes/prose.txt")))))
  (testing "the surface that judges a language is what makes it guarded"
    (with-redefs [extension/registered-extensions (constantly (clojure-surface
                                                                exclamation-verdict))]
      (is (= "clojure" (parse/guarded-language "src/a/b.clj")))
      (is (= "clojure" (parse/guarded-language "deps.edn")))
      (is (nil? (parse/guarded-language "a/b/c.py"))))))

(deftest error-nodes-test
  (testing "no registered verdict means no faults to report"
    (with-no-surfaces (is (= [] (parse/error-nodes "clojure" "(defn f [x)")))
                      (is (= [] (parse/error-nodes nil "(defn f [x)")))
                      (is (= [] (parse/error-nodes "fictional" "broken!"))))))

(deftest transition-verdict-test
  (testing "an unjudged write is UNGUARDED, never a clean bill of health"
    (with-no-surfaces (is (= :unguarded (:status (parse/transition-verdict nil "anything" ")"))))
                      (is (= :unguarded
                             (:status (parse/transition-verdict "clojure" "(def x 1)" ")"))))))
  (testing "one shared verdict distinguishes clean, introduced, and pre-existing errors"
    (with-redefs [extension/registered-extensions (constantly (clojure-surface
                                                                exclamation-verdict))]
      (is (= :clean (:status (parse/transition-verdict "clojure" "(def x 1)" "(def x 2)"))))
      (is (= :introduced-error (:status (parse/transition-verdict "clojure" "(def x 1)" "oops!"))))
      (is (= :still-broken (:status (parse/transition-verdict "clojure" "bad!" "worse!")))))))

(deftest surface-declared-language-test
  (testing "a file type vis has no built-in name for is neither detected nor guarded"
    (with-no-surfaces (is (nil? (parse/detect-language "notes/thing.vislang")))
                      (is (nil? (parse/guarded-language "notes/thing.vislang")))))
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
      (is (= "python" (parse/detect-language "a/b/c.py"))))))

(deftest surface-syntax-verdict-test
  (testing "a registered verdict answers for a language vis cannot parse"
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

(deftest surface-syntax-failure-test
  (testing "a throwing handler leaves the language unguarded instead of wedging the editor"
    (with-redefs [extension/registered-extensions
                  (constantly (clojure-surface (fn [_]
                                                 (throw (ex-info "handler is broken" {})))))]
      (is (= [] (parse/error-nodes "clojure" "(defn f [x)")))
      (is (= :unguarded (:status (parse/transition-verdict "clojure" "(def x 1)" ")"))))))
  (testing "a result the contract refuses reads the same way"
    (with-redefs [extension/registered-extensions (constantly (clojure-surface (fn [_]
                                                                                 {"is_clean"
                                                                                  false})))]
      (is (= [] (parse/error-nodes "clojure" "(defn f [x)")))
      (is (= :unguarded (:status (parse/transition-verdict "clojure" "(def x 1)" ")"))))))
  (testing "a language no surface serves stays unguarded instead of throwing"
    (with-no-surfaces (is (= [] (parse/error-nodes "fictional" "broken!")))
                      (is (nil? (parse/guarded-language "notes/thing.vislang"))))))
