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

  (it "p joins variadic args with a single space"
    (expect (= "a b c"            (md/p "a" "b" "c")))
    (expect (= "Patched 12 files" (md/p "Patched" 12 "files")))
    (expect (= "Status: **OK**"   (md/p "Status:" (md/bold "OK")))))

  (it "p drops nils between parts (no double-spaces)"
    (expect (= "a b"  (md/p "a" nil "b")))
    (expect (= "only" (md/p nil "only" nil))))

  (it "p splices a sequential arg one level deep (matches md/join / md/lines)"
    (expect (= "a b c"     (md/p ["a" "b" "c"])))
    (expect (= "a b c"     (md/p (map identity ["a" "b" "c"]))))
    (expect (= "head a b"  (md/p "head" (map str ["a" "b"])))))

  (it "p with no args yields the empty string"
    (expect (= "" (md/p))))

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
    ;; Updated for the variadic-only API: a disclosure label is
    ;; built explicitly with `md/summary`, not auto-wrapped from
    ;; the first arg. Plain strings stay body.
    (let [out (md/details (md/summary "trace") "body text")]
      (expect (str/includes? out "<details>"))
      (expect (str/includes? out "<summary>trace</summary>"))
      (expect (str/includes? out "body text"))
      (expect (str/includes? out "</details>"))))

  (it "summary emits a bare <summary>…</summary> tag"
    (expect (= "<summary>Logs</summary>" (md/summary "Logs")))
    (expect (= "<summary></summary>"     (md/summary nil)))
    (expect (= "<summary>**Logs**</summary>" (md/summary (md/bold "Logs")))))

  (it "details accepts a pre-wrapped md/summary without double-wrapping"
    ;; The whole point of exposing `summary` separately: callers can
    ;; decorate the disclosure label with bold/code/italic spans and
    ;; feed the wrapped tag straight into `details`. `details` must
    ;; detect it and embed verbatim — no `<summary><summary>…` chain.
    (let [out (md/details (md/summary (md/bold "Logs")) "hidden body")]
      (expect (str/includes? out "<summary>**Logs**</summary>"))
      (expect (not (str/includes? out "<summary><summary>")))
      (expect (not (str/includes? out "</summary></summary>")))
      (expect (str/includes? out "hidden body"))))

  (it "details treats plain strings as body, never as auto-summary"
    ;; Legacy auto-wrap was removed: `(md/details "trace" "body")`
    ;; emits both args as body (blank-line joined). Callers wanting
    ;; a disclosure label use `(md/summary X)` explicitly.
    (let [out (md/details "trace" "body")]
      (expect (str/includes? out "<details>"))
      (expect (not (str/includes? out "<summary>")))
      (expect (str/includes? out "trace"))
      (expect (str/includes? out "body"))
      ;; Body parts are blank-line separated.
      (expect (str/includes? out "trace\n\nbody"))))

  (it "details (variadic, 3+ args) lifts a <summary> from a tail position to the first-child slot"
    ;; HTML5 + GitHub require <summary> to be the first flow child of
    ;; <details>. If the caller wrote intro / body / summary in source
    ;; order, the rendered block must still emit summary FIRST so the
    ;; disclosure renders. Source order of body parts is preserved.
    (let [out (md/details "intro paragraph"
                "snippet line"
                (md/summary "Trace"))
          summary-pos (str/index-of out "<summary>")
          intro-pos   (str/index-of out "intro paragraph")
          snippet-pos (str/index-of out "snippet line")]
      (expect (str/includes? out "<summary>Trace</summary>"))
      ;; Summary appears BEFORE every body part in the rendered
      ;; output — the lift moved it to the canonical slot.
      (expect (< summary-pos intro-pos))
      (expect (< summary-pos snippet-pos))
      ;; Body parts kept their relative order (intro before snippet).
      (expect (< intro-pos snippet-pos))
      ;; And the closer lives at the end.
      (expect (str/ends-with? out "</details>"))))

  (it "details (variadic) accepts <summary> as the FIRST arg too — idempotent placement"
    (let [out (md/details (md/summary "Trace") "body1" "body2")]
      (expect (str/includes? out "<summary>Trace</summary>"))
      (expect (< (str/index-of out "<summary>")
                (str/index-of out "body1")))
      (expect (< (str/index-of out "body1")
                (str/index-of out "body2")))))

  (it "details (variadic) splices sequential body parts one level deep"
    ;; Same splice rule as md/join / md/lines: a (map render xs)
    ;; flows into the body without LazySeq leaks.
    (let [out (md/details "hdr"
                (md/summary "Files")
                (mapv #(str "• " %) ["a.clj" "b.clj" "c.clj"]))]
      (expect (str/includes? out "<summary>Files</summary>"))
      (doseq [f ["a.clj" "b.clj" "c.clj"]]
        (expect (str/includes? out f)))))

  (it "details (variadic, no summary) emits a label-less <details> block"
    ;; Caller explicitly omitted <summary>; we don't fabricate one.
    ;; Browsers fall back to a default 'Details' label — their job,
    ;; not ours.
    (let [out (md/details "para1" "para2" "para3")]
      (expect (str/starts-with? out "<details>"))
      (expect (str/ends-with? out "</details>"))
      (expect (not (str/includes? out "<summary>")))
      (expect (str/includes? out "para1"))
      (expect (str/includes? out "para2"))
      (expect (str/includes? out "para3"))))

  (it "details (variadic) refuses more than one <summary> part"
    ;; Two summaries inside one <details> is invalid HTML and almost
    ;; certainly a caller bug. Surface it to the iteration loop
    ;; instead of silently shipping malformed markup.
    (expect (throws? clojure.lang.ExceptionInfo
              #(md/details (md/summary "A") "body" (md/summary "B")))))

  (it "details 1-arity: single body or single <summary>, no auto-wrap"
    ;; 1-arity is the variadic path with one part — no implicit
    ;; <summary> creation. Useful for hand-rolled blocks.
    (let [body-only (md/details "raw body only")]
      (expect (str/includes? body-only "raw body only"))
      (expect (not (str/includes? body-only "<summary>"))))
    (let [tag-only (md/details (md/summary "Just a label"))]
      (expect (str/includes? tag-only "<summary>Just a label</summary>"))))

  (it "details 0-arity: empty disclosure block"
    ;; Edge case but cheap to support. Useful as a placeholder the
    ;; caller mutates downstream via str-concat.
    (let [out (md/details)]
      (expect (str/starts-with? out "<details>"))
      (expect (str/ends-with? out "</details>")))))

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

  (it "->str still refuses bare lazy seq inside a strict-arity inline builder"
    ;; md/p is now variadic-with-splicing, but every other builder
    ;; that takes a single string slot (md/bold, md/italic, md/code,
    ;; md/code-block, md/blockquote, …) still routes through `->str`,
    ;; so a stray `(map render xs)` keeps failing loudly instead of
    ;; shipping `clojure.lang.LazySeq@<hex>` into the answer.
    (expect (throws? clojure.lang.ExceptionInfo
              #(md/bold (map identity ["a" "b"]))))
    (expect (throws? clojure.lang.ExceptionInfo
              #(md/code-block (map identity ["line1" "line2"])))))

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
                  'code-block 'blockquote 'hr 'br 'details 'summary
                  'ul 'ol 'checklist
                  'table
                  'join 'lines 'section 'escape}]
      (expect (= names syms))))

  (it "system prompt deliberately stays silent on md/summary"
    ;; Intentional: md/summary is a low-level HTML-tag helper for
    ;; users who already know what they want. Advertising it in the
    ;; system prompt would push every model toward collapsible UI
    ;; for normal answers. Discoverable via symbol-info, hidden from
    ;; the prompt surface.
    (expect (not (str/includes? md/markdown-prompt "md/summary"))))

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
