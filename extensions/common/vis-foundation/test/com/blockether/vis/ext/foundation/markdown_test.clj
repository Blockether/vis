(ns com.blockether.vis.ext.foundation.markdown-test
  "Smoke tests for the markdown builder implementation that is now
   re-exported under `v/`. Every fn is a pure string builder, so the
   assertions are pure equality / substring checks against the
   rendered output."
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
    (expect (= "# " (md/h1 nil))))

  (it "variadic: concatenates parts (author owns whitespace)"
    ;; This is the regression that broke conversation 4b1ed602: the
    ;; LLM wrote `(md/h3 \"Proposal: \" (md/code \":foo\") \" sentinel\")`
    ;; and we threw ArityException. After the variadic fix the form
    ;; reads naturally:
    (expect (= "### Proposal: `:foo` sentinel"
              (md/h3 "Proposal: " (md/code ":foo") " sentinel")))
    (expect (= "# Build of `v1.2.3`"
              (md/h1 "Build of " (md/code "v1.2.3"))))
    (expect (= "## a b"
              (md/h 2 "a" " " "b"))))

  (it "variadic: nil parts dropped, seqs spliced one level"
    (expect (= "# ab"  (md/h1 "a" nil "b")))
    (expect (= "# abc" (md/h1 ["a" "b" "c"]))))

  (it "no args -> empty heading body"
    (expect (= "# " (md/h1)))))

(defdescribe inline-test
  (it "bold / italic / bold-italic / strike"
    (expect (= "**x**"   (md/bold "x")))
    (expect (= "*x*"     (md/italic "x")))
    (expect (= "***x***" (md/bold-italic "x")))
    (expect (= "~~x~~"   (md/strike "x"))))

  (it "em / strong are HTML-semantic aliases for italic / bold (registered as SCI symbols)"
    ;; The aliases are sandbox-only (not Clojure-side defns), so we
    ;; verify them via the symbols vector instead of calling them
    ;; directly from the test ns.
    (let [by-sym (into {} (map (juxt :ext.symbol/sym identity)) md/markdown-symbols)]
      (expect (= md/italic (get-in by-sym ['em :ext.symbol/fn])))
      (expect (= md/bold   (get-in by-sym ['strong :ext.symbol/fn])))))

  (it "inline emphasis helpers are variadic: parts concatenated, nil dropped, seqs spliced"
    ;; Same regression class as md/h3 — the LLM composes naturally
    ;; (md/bold \"build \" (md/code \"v1.2.3\")) instead of pre-joining
    ;; with str. Variadic shape removes the foot-gun.
    (expect (= "**build `v1.2.3`**"
              (md/bold "build " (md/code "v1.2.3"))))
    (expect (= "*a b*"
              (md/italic "a" " " "b")))
    (expect (= "`v/cat`"
              (md/code "v/" "cat")))
    (expect (= "~~old~~"
              (md/strike "o" nil "ld")))
    (expect (= "<kbd>Ctrl+K</kbd>"
              (md/kbd "Ctrl+K"))))

  (it "code, kbd"
    (expect (= "`v/cat`" (md/code "v/cat")))
    (expect (= "<kbd>Ctrl+K</kbd>" (md/kbd "Ctrl+K"))))

  (it "link / image (2-arg)"
    (expect (= "[docs](https://example.com)"
              (md/link "docs" "https://example.com")))
    (expect (= "![alt](./x.png)"
              (md/image "alt" "./x.png"))))

  (it "link/image/file-link/anchor reject missing required targets loudly"
    (expect (throws? clojure.lang.ExceptionInfo
              (md/link nil "https://example.com")))
    (expect (throws? clojure.lang.ExceptionInfo
              (md/link "docs" nil)))
    (expect (throws? clojure.lang.ExceptionInfo
              (md/image "alt" nil)))
    (expect (throws? clojure.lang.ExceptionInfo
              (md/file-link nil)))
    (expect (throws? clojure.lang.ExceptionInfo
              (md/anchor nil))))

  (it "required target helpers reject tool-result envelopes with targeted hints"
    (let [cat-result {:ok? true
                      :result {:lines ["a" "b"]}
                      :provenance {:op :v/cat}}
          rg-result  {:ok? true
                      :result {:hits [{:path "x" :line 1 :text "needle"}]}
                      :provenance {:op :v/rg}}]
      (try
        (md/link "docs" cat-result)
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (.contains (ex-message e) "[:result :lines]"))))
      (try
        (md/file-link rg-result)
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (.contains (ex-message e) "[:result :hits]"))))))

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

  (it "p normalizes double spaces from trailing/leading whitespace in parts"
    (expect (= "a b c" (md/p "a " " b " " c")))
    (expect (= "a b c" (md/p "a  " "  b" "c")))
    (expect (= "a b c" (md/p "a  b" "c")))
    (expect (= "foo `bar`" (md/p "foo " (md/code "bar")))))

  (it "p with no args yields the empty string"
    (expect (= "" (md/p))))

  (it "code-block fences with optional lang first and ensures trailing newline before close"
    (expect (= "```clojure\n(println :ok)\n```"
              (md/code-block "clojure" "(println :ok)")))
    (expect (= "```\nplain\n```"
              (md/code-block "plain")))
    (expect (= "```clojure\n(println :ok)\n```"
              (md/code-block :clojure "(println :ok)")))
    (expect (= "```edn\n{:ok true}\n```"
              (md/code-block 'edn "{:ok true}")))
    (expect (= "```\na\nb\n```"
              (md/code-block "a\nb"))))

  (it "code-block rejects nil body so missing tool envelope access fails loudly"
    (expect (throws? clojure.lang.ExceptionInfo
              (md/code-block nil)))
    (expect (throws? clojure.lang.ExceptionInfo
              (md/code-block "text" nil))))

  (it "code-block rejects tool-result envelopes with targeted payload hint"
    (let [bash-result {:ok? true
                       :result {:stdout "hello"}
                       :provenance {:op :v/bash}}]
      (try
        (md/code-block bash-result)
        (expect false)
        (catch clojure.lang.ExceptionInfo e
          (expect (.contains (ex-message e) "[:result :stdout]"))))))

  (it "blockquote prefixes every line"
    (expect (= "> a\n> b" (md/blockquote "a\nb")))
    (expect (= ">"        (md/blockquote ""))))

  (it "blockquote variadic: parts concatenated, then per-line prefix"
    (expect (= "> a b"        (md/blockquote "a " "b")))
    (expect (= "> head\n> tail" (md/blockquote "head" "\n" "tail"))))

  (it "quote is a shorter alias for blockquote (registered via SCI symbol 'quote)"
    ;; `clojure.core/quote` is a special form, so we can't invoke
    ;; `md/quote` as a regular Clojure fn from the test file. Verify
    ;; the SCI symbol entry instead: it MUST be registered, and its
    ;; impl MUST be the same fn as `md/blockquote`.
    (let [entry (->> md/markdown-symbols
                  (filter #(= 'quote (:ext.symbol/sym %)))
                  first)]
      (expect (some? entry))
      (expect (= md/blockquote (:ext.symbol/fn entry)))))

  (it "hr / br are constants"
    (expect (= "---" md/hr))
    (expect (= "  "  (str md/br)))
    (expect (= "  "  (md/br))))

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
  (it "li renders a single list item"
    (expect (= "- hello" (md/li "hello")))
    (expect (= "- build `v1.2.3`" (md/li "build " (md/code "v1.2.3"))))
    (expect (= "- a b c" (md/li "a" " " "b" " " "c")))
    (expect (= "- " (md/li nil)))
    (expect (= "- abc" (md/li ["a" "b" "c"]))))

  (it "ul renders one `- item` per entry"
    (expect (= "- a\n- b\n- c" (md/ul ["a" "b" "c"]))))

  (it "ul coalesces LLM-flattened inline fragments into their parent item"
    (expect (= (str "- Intencje żyją na poziomie `conversation_soul` — persystują między turami\n"
                 "- Intent sam jest bramką\n"
                 "- Nowe API: `v/issue-intent!`, `v/issue-plan!`\n"
                 "- 5 nowych tabel")
              (md/ul ["Intencje żyją na poziomie " (md/code "conversation_soul") " — persystują między turami"
                      "Intent sam jest bramką"
                      "Nowe API: " (md/code "v/issue-intent!") ", " (md/code "v/issue-plan!")
                      "5 nowych tabel"]))))

  (it "ol coalesces LLM-flattened inline fragments without eating next item"
    (expect (= "1. Path: `src/foo.clj`, `src/bar.clj`\n2. Run tests"
              (md/ol ["Path: " (md/code "src/foo.clj") ", " (md/code "src/bar.clj")
                      "Run tests"]))))

  (it "ul keeps a code-only item separate when prior text is complete"
    (expect (= "- Use\n- `foo`"
              (md/ul ["Use" (md/code "foo")]))))

  (it "ul accepts preformatted li output without double bullets"
    (expect (= "- a\n- b"
              (md/ul [(md/li "a") (md/li "b")]))))

  (it "ul preserves alternate unordered markers that are already present"
    (expect (= "* a\n+ b\n- c"
              (md/ul ["* a" "+ b" "- c"]))))

  (it "ul handles nil / empty"
    (expect (= "" (md/ul nil)))
    (expect (= "" (md/ul []))))

  (it "ol numbers from 1"
    (expect (= "1. first\n2. second" (md/ol ["first" "second"]))))

  (it "ol accepts preformatted li output without double markers"
    (expect (= "1. first\n2. second"
              (md/ol [(md/li "first") (md/li "second")]))))

  (it "ul indents nested ordered-list blocks"
    (expect (= "-\n  1. first\n  2. second"
              (md/ul [(md/ol ["first" "second"])]))))

  (it "ol indents nested unordered-list blocks"
    (expect (= "1.\n   - first\n   - second"
              (md/ol [(md/ul ["first" "second"])]))))

  (it "deep ul/ol combinations keep each child level indented"
    (expect (= "1. parent\n   - child\n     1. leaf-a\n     2. leaf-b"
              (md/ol [["parent\n"
                       (md/ul [["child\n"
                                (md/ol ["leaf-a" "leaf-b"])]])]]))))

  (it "lists indent multiline code-block items"
    (expect (= "- Example:\n  ```clojure\n  (+ 1 2)\n  ```"
              (md/ul [["Example:\n" (md/code-block "clojure" "(+ 1 2)")]])))
    (expect (= "1. Example:\n   ```clojure\n   (+ 1 2)\n   ```"
              (md/ol [["Example:\n" (md/code-block "clojure" "(+ 1 2)")]]))))

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
      (expect (= "| a |\n| --- |" out))))

  (it "table rejects nil headers loudly"
    (expect (throws? clojure.lang.ExceptionInfo
              (md/table nil [])))))

(defdescribe needs-input-test
  (it "returns a host-visible marker with ask text"
    (expect (= {:vis/answer-mode :needs-input
                :answer/text "Please paste the ideas you want reviewed."}
              (md/needs-input "Please paste the ideas you want reviewed.")))
    (expect (= {:vis/answer-mode :needs-input
                :answer/text "Please paste the ideas you currently have."
                :missing "the ideas to review"}
              (md/needs-input {:missing "the ideas to review"
                               :ask "Please paste the ideas you currently have."}))))

  (it "is registered as a sandbox symbol"
    (let [by-sym (into {} (map (juxt :ext.symbol/sym identity)) md/markdown-symbols)]
      (expect (= md/needs-input (get-in by-sym ['needs-input :ext.symbol/fn]))))))

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

  (it "variadic inline helpers splice ONE level (matching md/p), reject NESTED sequentials"
    ;; Inline helpers (md/bold / md/italic / md/code / …) are now
    ;; variadic-with-splicing, same contract as md/p. A bare lazy
    ;; seq is treated as splice-able parts — the LazySeq@<hex> leak
    ;; is prevented by `expand-parts` flattening, not by rejection.
    (expect (= "**ab**" (md/bold (map identity ["a" "b"]))))
    (expect (= "`v/cat`" (md/code (map identity ["v/" "cat"]))))
    ;; Nested-beyond-one-level still surfaces as an explicit throw
    ;; via `->str` so a tree of seqs doesn't silently drop
    ;; structure or splat reader-syntax into the answer.
    (expect (throws? clojure.lang.ExceptionInfo
              #(md/bold [["nested"]])))
    ;; md/code-block is NOT variadic (single :code positional or
    ;; :lang + :code) — a stray seq there still throws.
    (expect (throws? clojure.lang.ExceptionInfo
              #(md/code-block (map identity ["line1" "line2"])))))

  (it "lines uses single newlines"
    (expect (= "a\nb\nc" (md/lines "a" "b" "c"))))

  (it "lines splices a sequential arg"
    (expect (= "a\nb\nc" (md/lines (map str ["a" "b" "c"])))))

  (it "section emits heading + body"
    (expect (= "## Summary\n\nbody" (md/section "Summary" "body")))
    (expect (= "### Details\n\nbody" (md/section 3 "Details" "body"))))

  (it "section rejects nil title/body loudly"
    (expect (throws? clojure.lang.ExceptionInfo
              (md/section nil "body")))
    (expect (throws? clojure.lang.ExceptionInfo
              (md/section "Title" nil))))

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
                (md/code-block "clojure" "(println :done)"))]
      (expect (str/starts-with? out "# Patch report\n\n"))
      (expect (str/includes? out "| file | +/- |"))
      (expect (str/includes? out "## Next"))
      (expect (str/includes? out "- Run verify.sh"))
      (expect (str/ends-with? out "```")))))

(defdescribe extension-registration-test
  (it "exposes one symbol entry per public surface fn"
    (let [syms  (set (map :ext.symbol/sym md/markdown-symbols))
          names #{'h 'h1 'h2 'h3 'h4 'h5 'h6
                  'p 'bold 'strong 'italic 'em 'bold-italic 'strike 'code 'kbd
                  'link 'image 'file-link 'anchor
                  'code-block 'blockquote 'quote 'hr 'br 'details 'summary
                  'li 'ul 'ol 'checklist
                  'table
                  'join 'lines 'section 'escape 'needs-input}]
      (expect (= names syms))))

  (it "system prompt deliberately stays silent on v/summary"
    ;; Intentional: v/summary is a low-level HTML-tag helper for
    ;; users who already know what they want. Advertising it in the
    ;; system prompt would push every model toward collapsible UI
    ;; for normal answers. Discoverable via v/symbol-doc, hidden from
    ;; the prompt surface.
    (expect (not (str/includes? md/markdown-prompt "v/summary"))))

  (it "prompt fragment lists the unified v/ surface"
    (expect (str/includes? md/markdown-prompt "v/h1"))
    (expect (str/includes? md/markdown-prompt "v/table"))
    (expect (str/includes? md/markdown-prompt "v/join"))
    (expect (str/includes? md/markdown-prompt "v/link"))
    (expect (str/includes? md/markdown-prompt "v/image"))
    (expect (str/includes? md/markdown-prompt "v/file-link"))
    (expect (str/includes? md/markdown-prompt "v/anchor"))))
