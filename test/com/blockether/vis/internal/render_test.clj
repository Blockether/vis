(ns com.blockether.vis.internal.render-test
  "Tests for the answer-IR rendering pipeline.

   Covers:
   - `->ast` canonicalization to the strict post-`->ast` invariants
     (text only inside :span / raw-bodies of :code/:c/:kbd; no '\\n'
     in spans; explicit :br for hard breaks; attrs map present on every
     node).
   - render :html / :markdown / :plain across every block & inline tag.
   - extract-code, extract-text.
   - :max-length truncation, :context :thinking expandable blockquote.
   - Adversarial inputs (unknown tag, missing attrs, garbage values).
   - Real-world fixture from conversation `bdc79ae9` — assistant
     message contained literal `\" \\n   \"` mid-paragraph soft breaks
     that produced a 3-space hanging indent in the TUI. Canonical
     form must collapse them so the bug is structurally impossible."
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [com.blockether.vis.internal.persistance :as persistance]
   [com.blockether.vis.internal.render :as r]
   [lazytest.core :refer [defdescribe describe expect it]]))

;; ---------------------------------------------------------------------------
;; Canonical-form invariant checker
;; ---------------------------------------------------------------------------

(defn- canonical?
  "Walk an [:ir ...] AST and assert every invariant promised by
   `->ast`. Returns true or a {:violation ... :node ...} map."
  [ast]
  (letfn [(string-ok-context?
            [parent-tag]
            (contains? #{:span :code :c :kbd} parent-tag))
          (walk [n parent-tag]
            (cond
              (string? n)
              (cond
                (not (string-ok-context? parent-tag))
                {:violation :bare-string-in-vector :parent parent-tag :s n}
                (and (= :span parent-tag) (str/includes? n "\n"))
                {:violation :newline-in-span :s n}
                :else true)

              (not (vector? n))
              {:violation :non-vector :node n}

              (not (map? (second n)))
              {:violation :missing-attrs :node n}

              :else
              (let [tag (first n)
                    children (drop 2 n)]
                (cond
                  (= :br tag)
                  (if (seq children)
                    {:violation :br-has-children :node n}
                    true)

                  (= :img tag)
                  (if (seq children)
                    {:violation :img-has-children :node n}
                    true)

                  (#{:span :code :c :kbd} tag)
                  (let [bad (remove (fn [c] (or (string? c) (nil? c))) children)]
                    (if (seq bad)
                      {:violation :raw-body-must-be-string :tag tag :children children}
                      true))

                  :else
                  (or (some #(let [r (walk % tag)] (when (not= true r) r))
                        children)
                    true)))))]
    (let [r (walk ast nil)]
      (if (= true r) true r))))

;; ---------------------------------------------------------------------------
;; ->ast canonicalization
;; ---------------------------------------------------------------------------

(defdescribe ->ast-canonical-test
  (it "wraps bare top-level string in :p > :span"
    (expect (= [:ir {} [:p {} [:span {} "hello"]]] (r/->ast "hello"))))

  (it "drops empty-string content"
    (expect (= [:ir {}] (r/->ast ""))))

  (it "wraps non-:ir Hiccup in :ir, lifting strings into :span"
    (expect (= [:ir {} [:p {} [:span {} "x"]]]                 (r/->ast [:p "x"])))
    (expect (= [:ir {} [:p {} [:strong {} [:span {} "bold"]]]] (r/->ast [:strong "bold"]))))

  (it "lifts every bare string in :p children into :span"
    (let [out (r/->ast [:ir [:p "hi " [:strong "there"] " world"]])]
      (expect (= [:ir {} [:p {}
                          [:span {} "hi "]
                          [:strong {} [:span {} "there"]]
                          [:span {} " world"]]]
                out))))

  (it "collapses '\\s*\\n\\s*' soft breaks to a single space inside :p"
    ;; This is the bdc79ae9 reproduction in miniature.
    (let [out (r/->ast [:ir [:p [:c "z/locators"] " \n   znajduje nodes"]])]
      (expect (= [:ir {} [:p {}
                          [:c {} "z/locators"]
                          [:span {} " znajduje nodes"]]]
                out))))

  (it "preserves whitespace in :code block bodies verbatim"
    (let [src "(let [x 1]\n  x)"
          out (r/->ast [:ir [:code {:lang "clj"} src]])]
      (expect (= [:ir {} [:code {:lang "clj"} src]] out))))

  (it "preserves whitespace in :c (inline code) bodies verbatim"
    (let [out (r/->ast [:ir [:p [:c "  spaced  "] " after"]])]
      (expect (= [:ir {} [:p {}
                          [:c {} "  spaced  "]
                          [:span {} " after"]]]
                out))))

  (it "is idempotent on already-canonical input"
    (let [a (r/->ast [:ir [:p "foo " [:c "bar"] " baz"]])
          b (r/->ast a)]
      (expect (= a b))))

  (it "is identity-preserving on already-canonical input (cache friendliness)"
    ;; Critical for `format-answer-with-thinking-data` whose cache
    ;; keys on `System/identityHashCode answer`. Before identity
    ;; preservation every render would allocate a fresh canonical
    ;; vector and miss cache; now repeat passes return the SAME
    ;; object so cached walker output stays hot.
    (let [a (r/->ast [:ir [:p "foo " [:c "bar"] " baz"]
                      [:ul [:li "x"] [:li "y"]]
                      [:code {:lang "clj"} "(+ 1 1)"]])]
      (expect (r/canonical? a))
      (expect (identical? a (r/->ast a)))
      (expect (identical? a (r/->ast (r/->ast a))))
      (expect (= (System/identityHashCode a)
                (System/identityHashCode (r/->ast a))))))

  (it "`canonical?` rejects non-canonical inputs (bare strings, missing attrs, newline in :span)"
    (expect (not (r/canonical? [:ir {} [:p {} "bare string"]])))
    (expect (not (r/canonical? [:ir {} [:p [:span {} "x"]]])))
    (expect (not (r/canonical? [:ir {} [:p {} [:span {} "a\nb"]]]))))

  (it "sub-tree identity preserved across partial-canonical input"
    ;; Root fails `canonical?` (first :p has bare string), but the second
    ;; :p IS already canonical — the partial-canonical fast path keeps it
    ;; `identical?` so downstream identityHashCode caches stay hot.
    (let [canonical-p [:p {} [:span {} "already canonical"]]
          mixed       [:ir [:p "raw"] canonical-p]
          out         (r/->ast mixed)]
      (expect (r/canonical? out))
      ;; second :p kept identical despite root rebuild
      (expect (identical? canonical-p (nth out 3)))))

  (it "identity propagates through nested wrapper inlines (:strong, :em, :a)"
    (let [inner-span [:span {} "unchanged"]
          strong     [:strong {} inner-span]
          mixed-p    [:p {} [:span {} "raw inline"] strong]
          mixed      [:ir mixed-p]
          out        (r/->ast mixed)
          out-p      (nth out 2)
          out-strong (nth out-p 3)]
      ;; the :strong wrapper itself stays identical because none of its
      ;; sub-tree changed under canonicalization.
      (expect (identical? strong out-strong))))

  (it "always inserts {} attrs on every vector node"
    (let [out (r/->ast [:ir [:p "x" [:strong "y"]]])]
      (expect (every? (fn [n] (or (not (vector? n)) (map? (second n))))
                (tree-seq vector? rest out)))))

  (it "debug-coerces non-Hiccup values as edn code-block"
    (let [out (r/->ast 42)]
      (expect (r/ir? out))
      (expect (= :code (-> out (nth 2) first)))
      (expect (= "42" (-> out (nth 2) (nth 2))))))

  (it "every output passes the canonical-form invariant checker"
    (doseq [v ["hello" "" [:ir] [:p "x"] [:p "x" [:strong "y"]]
               [:ir [:ul [:li "a"] [:li [:p "b"] [:p "c"]]]]
               42 nil {:not "hiccup"}]]
      (expect (= true (canonical? (r/->ast v)))
        (str "canonical? failed on " (pr-str v) " → " (pr-str (r/->ast v)))))))

;; ---------------------------------------------------------------------------
;; :li content discriminator
;; ---------------------------------------------------------------------------

(defdescribe li-content-discriminator-test
  (it "all-inlines :li wraps inline run in single :p"
    (expect (= [:ir {} [:ul {} [:li {} [:p {}
                                        [:span {} "x"]
                                        [:strong {} [:span {} "y"]]]]]]
              (r/->ast [:ir [:ul [:li "x" [:strong "y"]]]]))))

  (it "all-blocks :li keeps blocks at top level"
    (expect (= [:ir {} [:ul {} [:li {}
                                [:p {} [:span {} "x"]]
                                [:p {} [:span {} "y"]]]]]
              (r/->ast [:ir [:ul [:li [:p "x"] [:p "y"]]]]))))

  (it "mixed :li buckets each inline run into its own :p, preserving order"
    (expect (= [:ir {} [:ul {} [:li {}
                                [:p {} [:span {} "a"]]
                                [:p {} [:span {} "b"]]
                                [:p {} [:span {} "c"]]]]]
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
      (expect (= "H2O" (r/render [:ir "H" [:sub "2"] "O"] :html))))

    (it ":br renders as newline"
      (expect (str/includes? (r/render [:ir [:p "a" [:br] "b"]] :html) "a\nb"))))

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

  (it ":br renders as GFM hard-break (two spaces + newline)"
    (expect (str/includes? (r/render [:ir [:p "a" [:br] "b"]] :markdown) "a  \nb")))

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

;; ---------------------------------------------------------------------------
;; Conversation Markdown export
;; ---------------------------------------------------------------------------

(defdescribe conversation-export-test
  (it "renders persisted :user-request text under the You heading"
    (let [cid (java.util.UUID/randomUUID)]
      (with-redefs [persistance/db-get-conversation
                    (fn [_db conversation-ref]
                      (when (= cid conversation-ref)
                        {:id cid :title "Casual greeting"}))
                    persistance/db-list-conversation-turns
                    (fn [_db conversation-ref]
                      (when (= cid conversation-ref)
                        [{:user-request "siema"
                          :answer [:ir [:p "Siema!"]]}]))]
        (let [out (r/conversation->markdown :db cid)]
          (expect (str/includes? out "## You\nsiema\n\n## Assistant\nSiema!"))
          (expect (not (str/includes? out "## You\n\nsiema")))
          (expect (str/includes? out "Siema!")))))))

;; ---------------------------------------------------------------------------
;; bdc79ae9 fixture — real LLM output that produced the 3-space hanging
;; indent in the TUI. Canonical IR must collapse soft breaks so no
;; downstream walker can reproduce the bug.
;; ---------------------------------------------------------------------------

(defn- read-fixture-ir []
  (when-let [r (io/resource "fixtures/bdc79ae9_answer_ir.edn")]
    (edn/read-string (slurp r))))

(defn- find-fixture-file ^java.io.File []
  (let [f (io/file "extensions/channels/vis-channel-tui/test/resources/fixtures/bdc79ae9_answer_ir.edn")]
    (when (.exists f) f)))

(defdescribe bdc79ae9-fixture-test
  (it "fixture is reachable"
    (expect (or (some? (read-fixture-ir))
              (some? (find-fixture-file)))))

  (it "canonical AST contains no '\\n' inside :span bodies"
    (when-let [raw (or (read-fixture-ir)
                     (some-> (find-fixture-file) slurp edn/read-string))]
      (let [ast (r/->ast raw)
            spans (filter #(and (vector? %) (= :span (first %))) (tree-seq vector? rest ast))
            offenders (filter (fn [[_ _ s]] (and (string? s) (str/includes? s "\n"))) spans)]
        (expect (empty? offenders)
          (str "spans with newline: " (vec offenders))))))

  (it "fixture passes the canonical-form invariant checker"
    (when-let [raw (or (read-fixture-ir)
                     (some-> (find-fixture-file) slurp edn/read-string))]
      (expect (= true (canonical? (r/->ast raw))))))

  (it "markdown projection of the fixture has no 3-space-indented continuation lines"
    (when-let [raw (or (read-fixture-ir)
                     (some-> (find-fixture-file) slurp edn/read-string))]
      (let [md (r/render raw :markdown)
            offenders (filter #(re-matches #"^   \S.*" %) (str/split-lines md))]
        (expect (empty? offenders)
          (str "lines with 3-space hanging indent: " (vec (take 3 offenders))))))))
