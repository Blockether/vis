(ns com.blockether.vis.internal.render-test
  "Tests for the answer-IR rendering pipeline.

   Coverage:
   - ->ast normalization for every surface form (string, hiccup,
     variadic, [:ir...] passthrough, garbage)
   - Attrs map insertion
   - :li content discriminator (all-blocks, all-inlines, mixed)
   - render :html / :markdown / :plain across every block & inline tag
   - extract-code, extract-text
   - :max-length truncation
   - :context :thinking expandable blockquote
   - Adversarial inputs (unknown tag, missing attrs)"
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [com.blockether.vis.internal.render :as r]))

;; ---------------------------------------------------------------------------
;; ->ast normalization
;; ---------------------------------------------------------------------------

(deftest ->ast-canonical-passthrough
  (is (= [:ir {} "x"] (r/->ast [:ir "x"])))
  (is (= [:ir {} "x"] (r/->ast [:ir {} "x"])))
  (is (r/ir? (r/->ast [:ir]))))

(deftest ->ast-bare-string
  (is (= [:ir {} "hello"] (r/->ast "hello")))
  (is (= [:ir {} ""] (r/->ast ""))))

(deftest ->ast-hiccup-non-ir-wraps
  (is (= [:ir {} [:p {} "x"]] (r/->ast [:p "x"])))
  (is (= [:ir {} [:strong {} "bold"]] (r/->ast [:strong "bold"]))))

(deftest ->ast-vector-of-mixed
  (is (= [:ir {} "one" [:strong {} "two"] [:code {:lang "edn"} "{:foo 1}"]]
        (r/->ast ["one" [:strong "two"] {:foo 1}]))))

(deftest ->ast-anything-else-debug-coerced
  (let [out (r/->ast 42)]
    (is (r/ir? out))
    (is (= :code (-> out (nth 2) first)))))

(deftest ->ast-attrs-always-present
  (let [out (r/->ast [:ir [:p "x"]])]
    (is (= {} (second out)))
    (is (= {} (-> out (nth 2) second)))))

(deftest ->ast-li-content-discriminator
  (testing "all-inlines li stays as-is"
    (is (= [:ir {} [:ul {} [:li {} "x" [:strong {} "y"]]]]
          (r/->ast [:ir [:ul [:li "x" [:strong "y"]]]]))))
  (testing "all-blocks li stays as-is"
    (is (= [:ir {} [:ul {} [:li {} [:p {} "x"] [:p {} "y"]]]]
          (r/->ast [:ir [:ul [:li [:p "x"] [:p "y"]]]]))))
  (testing "mixed li wraps loose inlines in :p preserving order"
    (is (= [:ir {} [:ul {} [:li {} [:p {} "a"] [:p {} "b"] [:p {} "c"]]]]
          (r/->ast [:ir [:ul [:li "a" [:p "b"] "c"]]])))))

;; ---------------------------------------------------------------------------
;; HTML render
;; ---------------------------------------------------------------------------

(deftest html-text-escapes
  (is (= "a &amp; b &lt;c&gt;" (r/render "a & b <c>" :html))))

(deftest html-headings-collapse-to-bold
  (is (str/starts-with? (r/render [:ir [:h {:level 1} "Title"]] :html)
        "<b>Title</b>")))

(deftest html-paragraph
  (is (str/includes? (r/render [:ir [:p "hi " [:strong "there"]]] :html)
        "hi <b>there</b>")))

(deftest html-inline-formatters
  (is (= "<b>bold</b>"   (r/render [:ir [:strong "bold"]] :html)))
  (is (= "<i>ital</i>"   (r/render [:ir [:em "ital"]] :html)))
  (is (= "<code>x</code>" (r/render [:ir [:c "x"]] :html))))

(deftest html-code-block-with-lang
  (is (str/includes?
        (r/render [:ir [:code {:lang "clojure"} "(+ 1 1)"]] :html)
        "<pre><code class=\"language-clojure\">(+ 1 1)</code></pre>")))

(deftest html-code-block-no-lang
  (is (str/includes?
        (r/render [:ir [:code "raw"]] :html)
        "<pre>raw</pre>")))

(deftest html-list-bullets
  (let [out (r/render [:ir [:ul [:li "a"] [:li "b"]]] :html)]
    (is (str/includes? out "• a"))
    (is (str/includes? out "• b"))))

(deftest html-list-ordered
  (let [out (r/render [:ir [:ol {:start 3} [:li "a"] [:li "b"]]] :html)]
    (is (str/includes? out "3. a"))
    (is (str/includes? out "4. b"))))

(deftest html-link
  (is (= "<a href=\"https://x.com\">click</a>"
        (r/render [:ir [:a {:href "https://x.com"} "click"]] :html))))

(deftest html-quote
  (is (str/includes?
        (r/render [:ir [:quote [:p "wisdom"]]] :html)
        "<blockquote>")))

(deftest html-quote-thinking-context-expandable
  (is (str/includes?
        (r/render [:ir [:quote [:p "deep"]]] :html {:context :thinking})
        "<blockquote expandable>")))

(deftest html-table
  (let [out (r/render [:ir [:table
                            [:tr [:th "A"] [:th "B"]]
                            [:tr [:td "1"] [:td "2"]]]] :html)]
    (is (str/starts-with? out "<pre>"))
    (is (str/includes? out "A"))
    (is (str/includes? out "1"))))

(deftest html-img-placeholder
  (is (= "<i>🖼 chart</i>"
        (r/render [:ir [:img {:src "x.png" :alt "chart"}]] :html))))

(deftest html-unknown-tag-fallthrough
  (is (= "inner"
        (r/render [:ir [:unknown-future-tag "inner"]] :html))))

(deftest html-kbd-renders-as-code
  (is (= "<code>Ctrl+C</code>"
        (r/render [:ir [:kbd "Ctrl+C"]] :html))))

(deftest html-mark-renders-as-bold
  (is (= "<b>highlight</b>"
        (r/render [:ir [:mark "highlight"]] :html))))

(deftest html-sup-sub-strip
  (is (= "x2" (r/render [:ir "x" [:sup "2"]] :html)))
  (is (= "H2O" (r/render [:ir "H" [:sub "2"] "O"] :html))))

;; ---------------------------------------------------------------------------
;; Markdown render
;; ---------------------------------------------------------------------------

(deftest md-headings
  (is (str/starts-with? (r/render [:ir [:h {:level 2} "Title"]] :markdown)
        "## Title")))

(deftest md-paragraph-preserves-emphasis
  (is (str/includes?
        (r/render [:ir [:p "hi " [:strong "bold"] " " [:em "ital"]]] :markdown)
        "hi **bold** *ital*")))

(deftest md-code-block
  (is (str/includes?
        (r/render [:ir [:code {:lang "python"} "print(1)"]] :markdown)
        "```python\nprint(1)\n```")))

(deftest md-inline-code-no-escape
  (is (= "`(+ 1 1)`" (r/render [:ir [:c "(+ 1 1)"]] :markdown))))

(deftest md-list-unordered
  (let [out (r/render [:ir [:ul [:li "x"] [:li "y"]]] :markdown)]
    (is (str/includes? out "- x"))
    (is (str/includes? out "- y"))))

(deftest md-list-ordered
  (let [out (r/render [:ir [:ol [:li "x"] [:li "y"]]] :markdown)]
    (is (str/includes? out "1. x"))
    (is (str/includes? out "2. y"))))

(deftest md-link
  (is (= "[click](https://x.com)"
        (r/render [:ir [:a {:href "https://x.com"} "click"]] :markdown))))

(deftest md-quote-prefixes-each-line
  (let [out (r/render [:ir [:quote [:p "line1"] [:p "line2"]]] :markdown)]
    (is (every? #(str/starts-with? % "> ") (str/split-lines (str/trim out))))))

(deftest md-table-with-headers
  (let [out (r/render [:ir [:table
                            [:tr [:th "A"] [:th "B"]]
                            [:tr [:td "1"] [:td "2"]]]] :markdown)]
    (is (str/includes? out "| A | B |"))
    (is (str/includes? out "| --- | --- |"))
    (is (str/includes? out "| 1 | 2 |"))))

;; ---------------------------------------------------------------------------
;; Plain render
;; ---------------------------------------------------------------------------

(deftest plain-strips-formatting
  (is (= "Hello world"
        (str/trim (r/render [:ir [:p "Hello " [:strong "world"]]] :plain)))))

(deftest plain-code-bare-content
  (is (= "(+ 1 1)"
        (str/trim (r/render [:ir [:code {:lang "clojure"} "(+ 1 1)"]] :plain)))))

(deftest plain-link-shows-text-and-url-when-different
  (is (str/includes?
        (r/render [:ir [:a {:href "https://x.com"} "click"]] :plain)
        "click (https://x.com)")))

(deftest plain-link-shows-url-only-when-text-equals-url
  (is (= "https://x.com"
        (str/trim (r/render [:ir [:a {:href "https://x.com"} "https://x.com"]] :plain)))))

(deftest plain-quote-uses-bar-prefix
  (let [out (r/render [:ir [:quote [:p "deep"]]] :plain)]
    (is (str/includes? out "│ deep"))))

;; ---------------------------------------------------------------------------
;; extract-code & extract-text
;; ---------------------------------------------------------------------------

(deftest extract-code-source-order
  (is (= ["one" "two"]
        (r/extract-code [:ir
                         [:p "intro"]
                         [:code {:lang "clj"} "one"]
                         [:p "middle"]
                         [:code {:lang "py"} "two"]]))))

(deftest extract-code-empty-when-no-code
  (is (= [] (r/extract-code [:ir [:p "just prose"]]))))

(deftest extract-text-concatenates-paragraphs
  (is (= "first\n\nsecond"
        (r/extract-text [:ir [:p "first"] [:code "x"] [:p "second"]]))))

;; ---------------------------------------------------------------------------
;; Render flavor errors
;; ---------------------------------------------------------------------------

(deftest unknown-flavor-throws
  (is (thrown? Exception (r/render "x" :unknown-flavor))))

;; ---------------------------------------------------------------------------
;; :max-length truncation
;; ---------------------------------------------------------------------------

(deftest max-length-truncates-at-paragraph-boundary
  (let [long-md (apply str (repeat 200 "word "))
        out     (r/render [:ir [:p long-md] [:p long-md]] :markdown {:max-length 100})]
    (is (<= (count out) 100))
    (is (str/ends-with? out "…"))))

;; ---------------------------------------------------------------------------
;; Adversarial / robustness
;; ---------------------------------------------------------------------------

(deftest renderer-never-throws-on-garbage
  (doseq [v ["string" "" [:ir] [:p] [:weird-tag "x"] [] {:not "hiccup"} 42 nil]]
    (doseq [flavor [:html :markdown :plain]]
      (is (string? (r/render v flavor))
        (str "render must return string for " (pr-str v) " " flavor)))))

(deftest empty-ir-renders-empty-string
  (is (= "" (r/render [:ir] :html)))
  (is (= "" (r/render [:ir] :markdown)))
  (is (= "" (r/render [:ir] :plain))))

(deftest deeply-nested-emphasis
  (is (str/includes?
        (r/render [:ir [:p [:strong [:em [:c "code"]]]]] :html)
        "<b><i><code>code</code></i></b>")))
