(ns com.blockether.vis.ext.foundation.markdown-test
  "Smoke tests for the `md/` extension. Every fn is a pure string
   builder, so the assertions are pure equality / substring checks
   against the rendered output."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.ext.foundation.markdown :as md]
   [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe headings-test
  (it "h1..h6 prefix the right number of `#`"
    (expect (= "# Title"     (md/h1 "Title")))
    (expect (= "## Title"    (md/h2 "Title")))
    (expect (= "### Title"   (md/h3 "Title")))
    (expect (= "#### Title"  (md/h4 "Title")))
    (expect (= "##### Title" (md/h5 "Title")))
    (expect (= "###### Title" (md/h6 "Title"))))

  (it "h clamps to [1, 6]"
    (expect (= "# x" (md/h 0 "x")))
    (expect (= "# x" (md/h -1 "x")))
    (expect (= "###### x" (md/h 6 "x")))
    (expect (= "###### x" (md/h 9 "x"))))

  (it "coerces nil to empty"
    (expect (= "# " (md/h1 nil)))))

(defdescribe inline-test
  (it "bold / italic / bold-italic / strike"
    (expect (= "**x**"   (md/bold "x")))
    (expect (= "*x*"     (md/italic "x")))
    (expect (= "***x***" (md/bold-italic "x")))
    (expect (= "~~x~~"   (md/strike "x"))))

  (it "code, kbd"
    (expect (= "`vis/cat`" (md/code "vis/cat")))
    (expect (= "<kbd>Ctrl+K</kbd>" (md/kbd "Ctrl+K"))))

  (it "link / image (2-arg)"
    (expect (= "[docs](https://example.com)"
              (md/link "docs" "https://example.com")))
    (expect (= "![alt](./x.png)"
              (md/image "alt" "./x.png"))))

  (it "link with a title attribute"
    (expect (= "[docs](https://example.com \"Project docs\")"
              (md/link "docs" "https://example.com" "Project docs")))
    ;; blank/nil title falls back to the 2-arg shape
    (expect (= "[docs](https://example.com)"
              (md/link "docs" "https://example.com" nil)))
    (expect (= "[docs](https://example.com)"
              (md/link "docs" "https://example.com" "")))
    ;; embedded `\"` in the title is escaped
    (expect (= "[x](u \"a\\\"b\")"
              (md/link "x" "u" "a\"b"))))

  (it "image with a title attribute"
    (expect (= "![flow](./flow.png \"Iteration flow\")"
              (md/image "flow" "./flow.png" "Iteration flow"))))

  (it "file-link cites source paths"
    (expect (= "[src/foo.clj](src/foo.clj)"
              (md/file-link "src/foo.clj")))
    (expect (= "[src/foo.clj:142](src/foo.clj#L142)"
              (md/file-link "src/foo.clj" 142))))

  (it "anchor links to in-doc headings"
    (expect (= "[summary](#summary)"            (md/anchor "summary")))
    (expect (= "[Patch report](#patch-report)"  (md/anchor "Patch report")))
    ;; punctuation is stripped from auto-slug
    (expect (= "[Step 1: setup](#step-1-setup)"
              (md/anchor "Step 1: setup")))
    ;; explicit slug overrides slugifier
    (expect (= "[Jump](#summary)"
              (md/anchor "Jump" "summary")))))

(defdescribe block-test
  (it "p coerces nil"
    (expect (= "" (md/p nil)))
    (expect (= "hello" (md/p "hello"))))

  (it "code-block fences with optional lang and ensures trailing newline before close"
    (expect (= "```clojure\n(println :ok)\n```"
              (md/code-block "(println :ok)" "clojure")))
    (expect (= "```\nplain\n```"
              (md/code-block "plain")))
    (it "preserves embedded newline runs"
      (expect (= "```\na\nb\n```"
                (md/code-block "a\nb")))))

  (it "blockquote prefixes every line"
    (expect (= "> a\n> b" (md/blockquote "a\nb")))
    (expect (= ">"        (md/blockquote ""))))

  (it "hr / br are constants"
    (expect (= "---" md/hr))
    (expect (= "  "  md/br)))

  (it "details wraps body"
    (let [out (md/details "trace" "body text")]
      (expect (str/includes? out "<details>"))
      (expect (str/includes? out "<summary>trace</summary>"))
      (expect (str/includes? out "body text"))
      (expect (str/includes? out "</details>")))))

(defdescribe lists-test
  (it "ul renders one `- item` per entry"
    (expect (= "- a\n- b\n- c" (md/ul ["a" "b" "c"]))))

  (it "ul handles nil / empty"
    (expect (= "" (md/ul nil)))
    (expect (= "" (md/ul []))))

  (it "ol numbers from 1"
    (expect (= "1. first\n2. second" (md/ol ["first" "second"]))))

  (it "checklist accepts vec pairs and maps"
    (expect (= "- [x] done\n- [ ] todo"
              (md/checklist [["done" true] ["todo" false]])))
    (expect (= "- [x] done\n- [ ] todo"
              (md/checklist [{:text "done" :done? true}
                             {:text "todo" :done? false}])))))

(defdescribe table-test
  (it "renders header + separator + body"
    (let [out (md/table ["k" "v"] [["a" 1] ["b" 2]])]
      (expect (= (str "| k | v |\n"
                   "| --- | --- |\n"
                   "| a | 1 |\n"
                   "| b | 2 |") out))))

  (it "pads short rows with empty cells"
    (let [out (md/table ["a" "b" "c"] [["x"]])]
      (expect (str/includes? out "| x |  |  |"))))

  (it "escapes pipes and newlines in cells"
    (let [out (md/table ["k"] [["a|b"] ["c\nd"]])]
      (expect (str/includes? out "a\\|b"))
      (expect (str/includes? out "c d"))))

  (it "honors :align"
    (let [out (md/table ["a" "b" "c"] [] {:align [:left :center :right]})]
      (expect (str/includes? out ":---"))
      (expect (str/includes? out ":---:"))
      (expect (str/includes? out "---:"))))

  (it "renders an empty body gracefully"
    (let [out (md/table ["a"] [])]
      (expect (= "| a |\n| --- |" out)))))

(defdescribe compose-test
  (it "join uses blank lines between parts"
    (expect (= "a\n\nb\n\nc" (md/join "a" "b" "c"))))

  (it "join drops nils"
    (expect (= "a\n\nb" (md/join "a" nil "b" nil))))

  (it "join splices a single sequential arg"
    (expect (= "a\n\nb\n\nc" (md/join (mapv name [:a :b :c])))))

  (it "join splices a lazy seq from map without LazySeq toString leak"
    (let [out (md/join (md/h1 "Top") (map identity ["x" "y"]))]
      (expect (= "# Top\n\nx\n\ny" out))
      (expect (not (re-find #"LazySeq@" out)))))

  (it "join splices output of map-indexed (regression: conv 8ea17da9 LazySeq leak)"
    (let [render (fn [i x] (str (inc i) ". " x))
          out    (md/join
                   (md/h1 "Conversation")
                   md/hr
                   (map-indexed render ["first" "second"]))]
      (expect (= "# Conversation\n\n---\n\n1. first\n\n2. second" out))))

  (it "join drops nils inside a spliced sequential"
    (expect (= "a\n\nb" (md/join ["a" nil "b"]))))

  (it "->str refuses bare lazy seq inside a builder slot"
    ;; e.g. `(md/p (map render xs))` — builder fn expects a string,
    ;; collection input fails loudly so the iteration loop surfaces
    ;; the error to the LLM instead of shipping `clojure.lang.LazySeq@<hex>`.
    (expect (throws? clojure.lang.ExceptionInfo
              #(md/p (map identity ["a" "b"])))))

  (it "lines uses single newlines"
    (expect (= "a\nb\nc" (md/lines "a" "b" "c"))))

  (it "lines splices a sequential arg"
    (expect (= "a\nb\nc" (md/lines (map str ["a" "b" "c"])))))

  (it "section emits heading + body"
    (expect (= "## Summary\n\nbody" (md/section "Summary" "body")))
    (expect (= "### Details\n\nbody" (md/section 3 "Details" "body"))))

  (it "escape backslash-escapes commonmark specials"
    (expect (= "a\\*b\\_c"   (md/escape "a*b_c")))
    (expect (= "1 \\+ 2"     (md/escape "1 + 2")))
    (expect (= "\\!\\[\\]"   (md/escape "![]")))))

(defdescribe end-to-end-test
  (it "assembles a multi-section answer"
    (let [out (md/join
                (md/h1 "Patch report")
                (md/p "Three files touched.")
                (md/table ["file" "+/-"]
                  [["core.clj" "+12 / -4"]
                   ["loop.clj" "+0 / -38"]])
                (md/h2 "Next")
                (md/ul ["Run verify.sh" "Update CHANGELOG"])
                (md/code-block "(println :done)" "clojure"))]
      (expect (str/starts-with? out "# Patch report\n\n"))
      (expect (str/includes? out "| file | +/- |"))
      (expect (str/includes? out "## Next"))
      (expect (str/includes? out "- Run verify.sh"))
      (expect (str/ends-with? out "```")))))

(defdescribe extension-registration-test
  (it "exposes one symbol entry per public surface fn"
    (let [syms  (set (map :ext.symbol/sym md/markdown-symbols))
          names #{'h 'h1 'h2 'h3 'h4 'h5 'h6
                  'p 'bold 'italic 'bold-italic 'strike 'code 'kbd
                  'link 'image 'file-link 'anchor
                  'code-block 'blockquote 'hr 'br 'details
                  'ul 'ol 'checklist
                  'table
                  'join 'lines 'section 'escape}]
      (expect (= names syms))))

  (it "extension descriptor declares the `md` alias"
    (expect (= 'md (get-in md/markdown-extension [:ext/ns-alias :alias])))
    (expect (= 'vis.ext.md (get-in md/markdown-extension [:ext/ns-alias :ns]))))

  (it "prompt fragment lists the surface"
    (expect (str/includes? md/markdown-prompt "md/h1"))
    (expect (str/includes? md/markdown-prompt "md/table"))
    (expect (str/includes? md/markdown-prompt "md/join"))
    (expect (str/includes? md/markdown-prompt "md/link"))
    (expect (str/includes? md/markdown-prompt "md/image"))
    (expect (str/includes? md/markdown-prompt "md/file-link"))
    (expect (str/includes? md/markdown-prompt "md/anchor"))))
