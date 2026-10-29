(ns com.blockether.vis.internal.channel.render-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.channel.render :as render]
            [lazytest.core :refer [defdescribe expect it]]))

(defdescribe
  heading-shorthand-test
  (it "normalizes [:h level ...] without rendering the level as text"
      (let [ast (render/->ast [:ast [:h 3 "Regular hook (" [:c ":ext/hooks"] ")"]])]
        (expect (= [:ast {}
                    [:h {:level 3} [:span {} "Regular hook ("] [:c {} ":ext/hooks"] [:span {} ")"]]]
                   ast))
        (expect (= "### Regular hook (`:ext/hooks`)" (str/trim (render/render ast :markdown {}))))))
  (it "clamps shorthand heading levels to the supported 1-6 range"
      (expect (= [:ast {} [:h {:level 1} [:span {} "Low"]]] (render/->ast [:ast [:h 0 "Low"]])))
      (expect (= [:ast {} [:h {:level 6} [:span {} "High"]]] (render/->ast [:ast [:h 9 "High"]])))))

(defdescribe
  host-bookkeeping-render-test
  (it
    "returns the model's authored source VERBATIM as one :code segment (full-Python: no splitting, no classification, no pretty-print)"
    (expect (= [{:kind :code :source "ls(\".\")\ndone(\"\"\"ok\"\"\")"}]
               (render/parse-block-display "ls(\".\")\ndone(\"\"\"ok\"\"\")"))))
  (it "keeps multi-line Python verbatim (triple-quoted strings are never tokenized)"
      (expect (= [{:kind :code :source "done(\"\"\"\nmulti\nline\n\"\"\")"}]
                 (render/parse-block-display "done(\"\"\"\nmulti\nline\n\"\"\")"))))
  (it "coalesces neighboring hidden tool calls into one hidden segment"
      (let [out (render/parse-block-display "(cat \"a.clj\")\n(cat \"b.clj\")")]
        (expect (= 1 (count out)))
        (expect (= :code
                   (-> out
                       first
                       :kind))))))

(defdescribe
  markdown-html-comment-test
  (it "drops standalone HTML comments instead of painting them in the bubble"
      (let [md
            "Confirming missing database and cleaning files\n\n<!-- -->\nFixed."

            ast
            (render/markdown->ast md)

            rendered
            (render/render ast :markdown {})]

        (expect (not (str/includes? rendered "<!-- -->")))
        (expect (= "Confirming missing database and cleaning files\n\nFixed." rendered))))
  (it "keeps non-comment raw HTML visible as escaped/text content"
      (let [rendered
            (render/render (render/markdown->ast "<details>secret</details>") :markdown {})]
        (expect (str/includes? rendered "<details>secret</details>")))))

(defdescribe
  markdown-soft-break-test
  (it "prose default collapses a bare newline to a single space (CommonMark)"
      ;; Model-authored answers / thinking are prose: soft wraps are not
      ;; intentional breaks, so they join with a space.
      (expect (= [:ast {} [:p {} [:span {} "line A"] [:span {} " "] [:span {} "line B"]]]
                 (render/markdown->ast "line A\nline B")))
      (expect (= [:ast {} [:p {} [:span {} "line A"] [:span {} " "] [:span {} "line B"]]]
                 (render/markdown->ast "line A\nline B" nil))))
  (it "`{:soft-break :hard}` lifts every bare newline to [:br] (line-oriented user/paste input)"
      ;; A pasted code/observation dump must keep its line structure;
      ;; otherwise consecutive lines merge into one wrapped wall of text.
      (expect (= [:ast {} [:p {} [:span {} "line A"] [:br {}] [:span {} "line B"]]]
                 (render/markdown->ast "line A\nline B" {:soft-break :hard}))))
  (it "hard-break mode preserves indentation + line breaks of a pasted code dump"
      (let [ast
            (render/markdown->ast "2493:    ;; foo\n2494:    (when x)" {:soft-break :hard})

            [_ _ p]
            ast]

        ;; Three inline children: line, [:br], line — no space-join that
        ;; would mash "2493: ;; foo 2494: (when x)" onto one row.
        (expect (= [:br {}] (nth p 3)))
        (expect (str/starts-with? (last (nth p 2)) "2493:"))
        (expect (str/starts-with? (last (nth p 4)) "2494:"))))
  (it "hard-break mode keeps paragraph breaks (blank line) as separate [:p] blocks"
      (let [ast (render/markdown->ast "a\nb\n\nc" {:soft-break :hard})]
        (expect (= 2 (count (filter #(and (vector? %) (= :p (first %))) ast))))))
  (it "both arities are idempotent on canonical IR"
      (let [canon [:ast {} [:p {} [:span {} "x"]]]]
        (expect (identical? canon (render/markdown->ast canon)))
        (expect (identical? canon (render/markdown->ast canon {:soft-break :hard}))))))

(defdescribe
  markdown-lone-code-span-test
  (it "promotes a long whole-paragraph inline code span to a copyable :code block"
      ;; A bookmarklet / long shell command authored as a lone `` `…` `` span
      ;; must render as a code block (verbatim + copy affordance), not an
      ;; inline chip that wraps and corrupts a select-copy.
      (let [src "javascript:(function(){var o=\"http://127.0.0.1\";window.__bridge.connect();})();"]
        (expect (= [:ast {} [:code {} src]] (render/markdown->ast (str "`" src "`"))))))
  (it "keeps a long lone file-path code span as an inline :c chip"
      ;; grep/patch/outline op-cards title every per-file section with a lone
      ;; `` `path` `` paragraph. Real repo paths routinely exceed the
      ;; promotion threshold; promoting them to :code blocks silently drops
      ;; the path-chip styling in every channel. Path-like literals (no
      ;; whitespace, has `/`, no URL scheme) stay inline however long.
      (let [path "apps/vis-tui/src/com/blockether/vis/tui/screen.clj"]
        (expect (= [:ast {} [:p {} [:c {} path]]] (render/markdown->ast (str "`" path "`"))))))
  (it "still promotes a long URL code span (scheme prefix = not a path)"
      (let [url "https://example.com/some/very/long/path/that/exceeds/the/threshold"]
        (expect (= [:ast {} [:code {} url]] (render/markdown->ast (str "`" url "`"))))))
  (it "still promotes a long shell command code span (whitespace = not a path)"
      (let [cmd "git log --oneline --graph --decorate --all -20 | head -40"]
        (expect (= [:ast {} [:code {} cmd]] (render/markdown->ast (str "`" cmd "`"))))))
  (it "keeps a short lone inline code span as an inline :c chip"
      (expect (= [:ast {} [:p {} [:c {} "foo"]]] (render/markdown->ast "`foo`"))))
  (it "keeps an inline code span embedded in prose inline"
      (expect (= [:ast {} [:p {} [:span {} "use "] [:c {} "some-fn"] [:span {} " here"]]]
                 (render/markdown->ast "use `some-fn` here")))))
