(ns com.blockether.vis.internal.render-test
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.render :as render]
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
  (it "any non-blank source is code-bearing, so the block is never structurally silent"
      (expect (false? (render/block-structurally-silent? "compute(1, 2)"))))
  (it "flags (def NAME (qualified/call …)) wrappers as hidden code"
      ;; The model wraps tool calls in `(def …)` for downstream reuse.
      ;; The channel shows the tool result pane anyway, so the def
      ;; source itself is noise. Mark the segment hidden so the renderer
      ;; collapses the code lines but still keeps the form in the
      ;; iteration trace.
      (expect (= [{:kind :code :source "(def render-sb-code (cat \"x.clj\"))"}]
                 (render/parse-block-display "(def render-sb-code (cat \"x.clj\"))"))))
  (it "hides plain (def x 1) so the DEF SINK is the single source of bound-var truth"
      ;; Pre-fix the renderer kept bare def source visible because the
      ;; comment claimed \"the value is the only artifact\". In practice
      ;; the trailer's `:vars` envelope already surfaces every bound var
      ;; to both model and user, so painting the raw `(def …)` line
      ;; just duplicates that signal and pushes the actual recap /
      ;; result rows off-screen. Hide every def-shaped form; let the
      ;; def sink speak.
      (expect (= [{:kind :code :source "(def x 1)"}] (render/parse-block-display "(def x 1)"))))
  (it "hides defn / defn- / defmacro / defmulti / defmethod / defonce uniformly"
      (doseq [src ["(defn foo [] 1)" "(defn- bar [] 2)" "(defmacro baz [] `(println 1))"
                   "(defmulti qux :kind)" "(defmethod qux :a [_] :a)"
                   "(defonce only-once (atom {}))"]]
        (let [out (render/parse-block-display src)]
          (expect (= 1 (count out)))
          (expect (= :code
                     (-> out
                         first
                         :kind))))))
  (it
    "hides top-level keyword-lookup and accessor calls (e.g. `(:size ir-file)`, `(get-in m [...])`, `(first xs)`)"
    ;; The model often does `(def x (tool/call))` followed by
    ;; `(:size x)` or `(get-in x [:k])` or `(first (:items x))` to
    ;; project a sub-value. The bound value is already on the DEF
    ;; SINK + the channel preview of the underlying tool call; the
    ;; projection row only repeats `<runtime ref>` next to a code
    ;; line nobody reads. Hide them.
    (doseq [src ["(:size ir-file)" "(:k m :default)" "(get m :k)" "(get-in m [:a :b])"
                 "(select-keys r [:a :b])" "(first xs)" "(count xs)" "(name :foo)" "(str x)"]]
      (let [out (render/parse-block-display src)]
        (expect (= 1 (count out)))
        (expect (= :code
                   (-> out
                       first
                       :kind))))))
  (it "hides bare-symbol top-level forms (e.g. `st` following `(def st (git/status))`)"
      ;; The model often writes `(def NAME (tool/call ...)) NAME` so the
      ;; trailer shows the value. The channel preview of the tool call
      ;; already paints the data; the loose symbol row is just chrome
      ;; that pushes recap / answer rows off-screen. Same hide-by-default
      ;; policy as bare tool calls and def-shaped forms.
      (let [out (render/parse-block-display "(def st (git/status))\nst")]
        ;; Both segments hidden → coalesce merges them into one entry.
        (expect (= 1 (count out)))
        (expect (= :code
                   (-> out
                       first
                       :kind)))))
  (it "renders a plain non-engine model call as visible code"
      ;; Any model form that isn't engine chrome (done / set_session_title)
      ;; or a tool call renders as the visible CALL.
      (expect (= [{:kind :code :source "compute(1, 2)"}]
                 (render/parse-block-display "compute(1, 2)"))))
  (it "hides def docstring form when wrapping a tool call"
      (expect (= [{:kind :code :source "(def named \"docstring\" (cat \"x.clj\"))"}]
                 (render/parse-block-display "(def named \"docstring\" (cat \"x.clj\"))"))))
  (it "hides bare qualified tool calls so the result pane speaks for itself"
      (expect (= [{:kind :code :source "(cat \"src/foo.clj\")"}]
                 (render/parse-block-display "(cat \"src/foo.clj\")"))))
  (it "coalesces neighboring hidden tool calls into one hidden segment"
      (let [out (render/parse-block-display "(cat \"a.clj\")\n(cat \"b.clj\")")]
        (expect (= 1 (count out)))
        (expect (= :code
                   (-> out
                       first
                       :kind)))))
  (it "def and tool call both render hidden so the bubble shows only recap / result rows"
      (let [out (render/parse-block-display "(def x 1)\n(cat \"a.clj\")")]
        ;; Both forms hidden — same :hidden? flag means the coalesce
        ;; pass merges them into one hidden code segment.
        (expect (= 1 (count out)))
        (expect (= :code
                   (-> out
                       first
                       :kind)))))
  (it
    "marks `(def r (ls …))` + `(select-keys r […])` BOTH hidden so the channel preview speaks alone"
    ;; Real-world fence the model emits when it wants to bind a tool
    ;; result and then project a sub-value. The def wrapping AND the
    ;; accessor row are both bookkeeping; the value rides on the DEF
    ;; SINK + the channel preview of the underlying tool call. The
    ;; renderer coalesces consecutive hidden code segments into one
    ;; entry, so the result is a single suppressed source row.
    (let [out (render/parse-block-display
                "(def r (ls \".\"))\n(select-keys r [:entry-count :file-count])")]
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
      (let [rendered (render/render (render/markdown->ast "<details>secret</details>") :markdown {})]
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
      (let [src "javascript:(function(){var o=\"http://127.0.0.1\";window.__spel.connect();})();"]
        (expect (= [:ast {} [:code {} src]] (render/markdown->ast (str "`" src "`"))))))
  (it "keeps a long lone file-path code span as an inline :c chip"
      ;; rg/patch/outline op-cards title every per-file section with a lone
      ;; `` `path` `` paragraph. Real repo paths routinely exceed the
      ;; promotion threshold; promoting them to :code blocks silently drops
      ;; the path-chip styling in every channel. Path-like literals (no
      ;; whitespace, has `/`, no URL scheme) stay inline however long.
      (let [path
            "extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/screen.clj"]
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
