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
   [com.blockether.vis.internal.render :as r]
   [lazytest.core :refer [defdescribe describe expect it]]))

;; ---------------------------------------------------------------------------
;; ->ast normalization
;; ---------------------------------------------------------------------------

(defdescribe ->ast-normalization-test
  (it "passes through canonical [:ir ...]"
    (expect (= [:ir {} "x"] (r/->ast [:ir "x"])))
    (expect (= [:ir {} "x"] (r/->ast [:ir {} "x"])))
    (expect (r/ir? (r/->ast [:ir]))))

  (it "wraps bare strings as text-node child of :ir"
    (expect (= [:ir {} "hello"] (r/->ast "hello")))
    (expect (= [:ir {} ""] (r/->ast ""))))

  (it "wraps non-:ir Hiccup vectors in :ir"
    (expect (= [:ir {} [:p {} "x"]] (r/->ast [:p "x"])))
    (expect (= [:ir {} [:strong {} "bold"]] (r/->ast [:strong "bold"]))))

  (it "coerces variadic mixed vectors element-by-element"
    (expect (= [:ir {} "one" [:strong {} "two"] [:code {:lang "edn"} "{:foo 1}"]]
              (r/->ast ["one" [:strong "two"] {:foo 1}]))))

  (it "debug-coerces anything else as edn code-block"
    (let [out (r/->ast 42)]
      (expect (r/ir? out))
      (expect (= :code (-> out (nth 2) first)))))

  (it "always inserts {} attrs after normalization"
    (let [out (r/->ast [:ir [:p "x"]])]
      (expect (= {} (second out)))
      (expect (= {} (-> out (nth 2) second))))))

(defdescribe li-content-discriminator-test
  (it "leaves all-inlines :li alone"
    (expect (= [:ir {} [:ul {} [:li {} "x" [:strong {} "y"]]]]
              (r/->ast [:ir [:ul [:li "x" [:strong "y"]]]]))))

  (it "leaves all-blocks :li alone"
    (expect (= [:ir {} [:ul {} [:li {} [:p {} "x"] [:p {} "y"]]]]
              (r/->ast [:ir [:ul [:li [:p "x"] [:p "y"]]]]))))

  (it "wraps loose inlines under mixed :li in :p, preserving order"
    (expect (= [:ir {} [:ul {} [:li {} [:p {} "a"] [:p {} "b"] [:p {} "c"]]]]
              (r/->ast [:ir [:ul [:li "a" [:p "b"] "c"]]])))))

;; ---------------------------------------------------------------------------
;; HTML render
;; ---------------------------------------------------------------------------

(defdescribe html-render-test
  (describe "text escaping"
    (it "escapes &, <, > in text nodes"
      (expect (= "a &amp; b &lt;c&gt;" (r/render "a & b <c>" :html)))))

  (describe "block tags"
    (it "collapses headings to bold"
      (expect (str/starts-with? (r/render [:ir [:h {:level 1} "Title"]] :html)
                "<b>Title</b>")))

    (it "renders paragraphs with inline emphasis"
      (expect (str/includes? (r/render [:ir [:p "hi " [:strong "there"]]] :html)
                "hi <b>there</b>")))

    (it "fenced code with language uses tg <pre><code class>"
      (expect (str/includes?
                (r/render [:ir [:code {:lang "clojure"} "(+ 1 1)"]] :html)
                "<pre><code class=\"language-clojure\">(+ 1 1)</code></pre>")))

    (it "fenced code without language drops to plain <pre>"
      (expect (str/includes?
                (r/render [:ir [:code "raw"]] :html)
                "<pre>raw</pre>")))

    (it "unordered list renders bullet glyphs"
      (let [out (r/render [:ir [:ul [:li "a"] [:li "b"]]] :html)]
        (expect (str/includes? out "• a"))
        (expect (str/includes? out "• b"))))

    (it "ordered list with :start renders correct numbering"
      (let [out (r/render [:ir [:ol {:start 3} [:li "a"] [:li "b"]]] :html)]
        (expect (str/includes? out "3. a"))
        (expect (str/includes? out "4. b"))))

    (it "blockquote renders as <blockquote>"
      (expect (str/includes?
                (r/render [:ir [:quote [:p "wisdom"]]] :html)
                "<blockquote>")))

    (it ":context :thinking promotes :quote to <blockquote expandable>"
      (expect (str/includes?
                (r/render [:ir [:quote [:p "deep"]]] :html {:context :thinking})
                "<blockquote expandable>")))

    (it "table renders as aligned monospace <pre>"
      (let [out (r/render [:ir [:table
                                [:tr [:th "A"] [:th "B"]]
                                [:tr [:td "1"] [:td "2"]]]] :html)]
        (expect (str/starts-with? out "<pre>"))
        (expect (str/includes? out "A"))
        (expect (str/includes? out "1")))))

  (describe "inline tags"
    (it "renders strong/em/c/a/img"
      (expect (= "<b>bold</b>"   (r/render [:ir [:strong "bold"]] :html)))
      (expect (= "<i>ital</i>"   (r/render [:ir [:em "ital"]] :html)))
      (expect (= "<code>x</code>" (r/render [:ir [:c "x"]] :html)))
      (expect (= "<a href=\"https://x.com\">click</a>"
                (r/render [:ir [:a {:href "https://x.com"} "click"]] :html)))
      (expect (= "<i>🖼 chart</i>"
                (r/render [:ir [:img {:src "x.png" :alt "chart"}]] :html))))

    (it "kbd renders as <code>"
      (expect (= "<code>Ctrl+C</code>"
                (r/render [:ir [:kbd "Ctrl+C"]] :html))))

    (it "mark renders as <b> (Telegram has no <mark>)"
      (expect (= "<b>highlight</b>"
                (r/render [:ir [:mark "highlight"]] :html))))

    (it "sup/sub strip wrapper (Telegram has no <sup>/<sub>)"
      (expect (= "x2" (r/render [:ir "x" [:sup "2"]] :html)))
      (expect (= "H2O" (r/render [:ir "H" [:sub "2"] "O"] :html)))))

  (describe "robustness"
    (it "unknown tag falls through to children"
      (expect (= "inner"
                (r/render [:ir [:unknown-future-tag "inner"]] :html))))

    (it "deeply nested emphasis preserves nesting"
      (expect (str/includes?
                (r/render [:ir [:p [:strong [:em [:c "code"]]]]] :html)
                "<b><i><code>code</code></i></b>")))))

;; ---------------------------------------------------------------------------
;; Markdown render
;; ---------------------------------------------------------------------------

(defdescribe md-render-test
  (it "headings emit ATX hashes"
    (expect (str/starts-with? (r/render [:ir [:h {:level 2} "Title"]] :markdown)
              "## Title")))

  (it "paragraph preserves emphasis as **bold** *ital*"
    (expect (str/includes?
              (r/render [:ir [:p "hi " [:strong "bold"] " " [:em "ital"]]] :markdown)
              "hi **bold** *ital*")))

  (it "fenced code with lang"
    (expect (str/includes?
              (r/render [:ir [:code {:lang "python"} "print(1)"]] :markdown)
              "```python\nprint(1)\n```")))

  (it "inline code is verbatim, no escape"
    (expect (= "`(+ 1 1)`" (r/render [:ir [:c "(+ 1 1)"]] :markdown))))

  (it "lists use - and 1. markers"
    (let [u (r/render [:ir [:ul [:li "x"] [:li "y"]]] :markdown)
          o (r/render [:ir [:ol [:li "x"] [:li "y"]]] :markdown)]
      (expect (str/includes? u "- x"))
      (expect (str/includes? u "- y"))
      (expect (str/includes? o "1. x"))
      (expect (str/includes? o "2. y"))))

  (it "link uses [text](url)"
    (expect (= "[click](https://x.com)"
              (r/render [:ir [:a {:href "https://x.com"} "click"]] :markdown))))

  (it "blockquote prefixes each line with >"
    (let [out (r/render [:ir [:quote [:p "line1"] [:p "line2"]]] :markdown)]
      (expect (every? #(str/starts-with? % "> ") (str/split-lines (str/trim out))))))

  (it "GFM table with header row"
    (let [out (r/render [:ir [:table
                              [:tr [:th "A"] [:th "B"]]
                              [:tr [:td "1"] [:td "2"]]]] :markdown)]
      (expect (str/includes? out "| A | B |"))
      (expect (str/includes? out "| --- | --- |"))
      (expect (str/includes? out "| 1 | 2 |")))))

;; ---------------------------------------------------------------------------
;; Plain render
;; ---------------------------------------------------------------------------

(defdescribe plain-render-test
  (it "strips inline emphasis"
    (expect (= "Hello world"
              (str/trim (r/render [:ir [:p "Hello " [:strong "world"]]] :plain)))))

  (it "code blocks emit bare content"
    (expect (= "(+ 1 1)"
              (str/trim (r/render [:ir [:code {:lang "clojure"} "(+ 1 1)"]] :plain)))))

  (it "link shows text and URL when distinct"
    (expect (str/includes?
              (r/render [:ir [:a {:href "https://x.com"} "click"]] :plain)
              "click (https://x.com)")))

  (it "link shows URL only when text equals URL"
    (expect (= "https://x.com"
              (str/trim (r/render [:ir [:a {:href "https://x.com"} "https://x.com"]] :plain)))))

  (it "blockquote uses bar prefix"
    (let [out (r/render [:ir [:quote [:p "deep"]]] :plain)]
      (expect (str/includes? out "│ deep")))))

;; ---------------------------------------------------------------------------
;; extract-code & extract-text
;; ---------------------------------------------------------------------------

(defdescribe extract-helpers-test
  (it "extract-code returns block contents in source order"
    (expect (= ["one" "two"]
              (r/extract-code [:ir
                               [:p "intro"]
                               [:code {:lang "clj"} "one"]
                               [:p "middle"]
                               [:code {:lang "py"} "two"]]))))

  (it "extract-code returns empty when answer has no [:code]"
    (expect (= [] (r/extract-code [:ir [:p "just prose"]]))))

  (it "extract-text concatenates [:p] content"
    (expect (= "first\n\nsecond"
              (r/extract-text [:ir [:p "first"] [:code "x"] [:p "second"]])))))

;; ---------------------------------------------------------------------------
;; Render flavor errors + opts
;; ---------------------------------------------------------------------------

(defdescribe render-options-test
  (it "throws on unknown flavor"
    (expect
      (try (r/render "x" :unknown-flavor) false
        (catch Exception _ true))))

  (it ":max-length truncates at paragraph boundary with ellipsis"
    (let [long-md (apply str (repeat 200 "word "))
          out     (r/render [:ir [:p long-md] [:p long-md]] :markdown {:max-length 100})]
      (expect (<= (count out) 100))
      (expect (str/ends-with? out "…")))))

;; ---------------------------------------------------------------------------
;; Adversarial / robustness
;; ---------------------------------------------------------------------------

(defdescribe robustness-test
  (it "renderer never throws on garbage input"
    (doseq [v ["string" "" [:ir] [:p] [:weird-tag "x"] [] {:not "hiccup"} 42 nil]]
      (doseq [flavor [:html :markdown :plain]]
        (expect (string? (r/render v flavor))
          (str "render must return string for " (pr-str v) " " flavor)))))

  (it "empty :ir renders empty string in every flavor"
    (expect (= "" (r/render [:ir] :html)))
    (expect (= "" (r/render [:ir] :markdown)))
    (expect (= "" (r/render [:ir] :plain)))))
