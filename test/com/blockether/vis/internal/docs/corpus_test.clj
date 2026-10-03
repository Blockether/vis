(ns com.blockether.vis.internal.docs.corpus-test
  "The corpus behind `apropos`/`doc`: one record per document, a usable first
   line, and one regular expression over names and page outlines."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.docs.corpus :as dc]
            [com.blockether.vis.internal.foundation.harness.discovery :as discovery]
            [lazytest.core :refer [defdescribe expect it throws?]]))

(defdescribe
  entry-shape-test
  "ONE record for every document, whatever seeded it: `name` + `text`, a `kind`
   from the closed vocabulary — what the document IS, which is how a reader
   decides whether to CALL it or read it — and `call` only when there is
   something to call."
  (it "carries exactly the specified keys"
      (let [es (dc/entries)]
        (expect (seq es))
        (doseq [e es]
          (expect (string? (:name e)))
          (expect (not (str/blank? (:name e))))
          (expect (string? (:text e)))
          (expect (not (str/blank? (:text e))))
          (expect (contains? dc/kinds (:kind e)) (str (:name e) " carries no kind"))
          (expect (empty? (dissoc e :name :text :call :kind)))))))

(defdescribe
  every-entry-has-a-usable-first-line-test
  "The gist is not a stored field, it is the FIRST LINE — so the lint that
   replaces it lives on the contract text: every document opens with one
   non-blank, single-sentence line short enough to scan."
  (it "opens every document with a one-liner"
      (doseq [e (dc/entries)]
        (let [g (dc/gist (:text e))]
          (expect (not (str/blank? g)) (str (:name e) " has no first line"))
          (expect (<= (count g) 240) (str (:name e) " first line is too long"))
          (expect (not (str/includes? g "\n")))))))

(defdescribe
  skill-entries-test
  "A skill's text IS its `SKILL.md`: the frontmatter summary, then the whole body
   verbatim. There is no second, shorter description anywhere to drift from it."
  (it "carries the whole body and no call — a skill is prose"
      (let [skill
            {:name "fixture-skill"
             :description "Fixture summary."
             :body "# Fixture

Whole skill body."}

            entries
            (with-redefs [discovery/skills (constantly [skill])]
              (#'dc/skill-entries))

            entry
            (first entries)]

        (expect (= "fixture-skill" (:name entry)))
        (expect (str/starts-with? (:text entry) "Fixture summary."))
        (expect (str/ends-with? (:text entry) (:body skill)))
        ;; No `call`: there is no skill verb. `doc(name)` IS the whole use.
        (expect (nil? (:call entry)))))
  (it "keeps package provenance and resources without altering the skill body"
      ;; #176: doc() must make resources in the admitted package usable.
      (let [skill
            {:name "vis-greeter/greeting"
             :description "Greet when requested."
             :body "# Greeting\nRead references/style.md.\n"
             :package {:name "vis-greeter" :version "1.0.0"}
             :dir "/fixture/greeter/skills/greeting"
             :resources ["references/style.md"]}

            entry
            (first (with-redefs [discovery/skills (constantly [skill])]
                     (#'dc/skill-entries)))]

        (expect (str/starts-with? (:text entry) (:description skill)))
        (expect (str/includes? (:text entry) "vis-greeter@1.0.0"))
        (expect (str/includes? (:text entry) (:dir skill)))
        (expect (str/includes? (:text entry) "references/style.md"))
        (expect (str/ends-with? (:text entry) (:body skill)))
        (expect (nil? (:call entry))))))

(defdescribe
  reading-a-skill-has-no-session-effect-test
  "A skill is a DOCUMENT: the corpus reads the discovery registry directly, so
   `apropos`/`doc` can neither activate nor mark anything."
  (it "never reaches the harness verb namespace"
      (let [src (slurp (io/resource "com/blockether/vis/internal/docs/corpus.clj"))]
        (expect (not (str/includes? src "harness.core")))
        (expect (not (str/includes? src "harness-core"))))))

(def ^:private drafts-page
  (str/join "\n"
            ["# Drafts" "" "A draft gives a session its own working copy." "" "## When to use" ""
             "- **You want to read the whole"
             "  diff first.** See [Review changes](#review-changes)." "" "## Review changes" ""
             "Approval happens after a matrix check." "" "```md" "## Fenced heading" "```"]))

(defdescribe
  search-test
  "`apropos` is one regular expression that ignores case: over the names of
   callables and over the outline of pages and skills. It preserves corpus order,
   never reads a callable's contract or a page's body prose, and ranks nothing."
  (let [es [{:name "numpy" :kind "module" :text "Array module."}
            {:name "numpy.linalg.solve" :kind "function" :text "Solve a matrix equation."}
            {:name "pandas.read_csv" :kind "function" :text "Read comma-separated data."}
            {:name "shell" :kind "tool" :text "Runs a command."}
            {:name "drafts" :kind "doc" :text drafts-page}]]
    (it "matches names with a caller-supplied regular expression"
        (expect (= ["numpy" "numpy.linalg.solve"] (mapv :name (dc/search es #"numpy(?:\..*)?"))))
        (expect (= ["numpy.linalg.solve" "pandas.read_csv"] (mapv :name (dc/search es #"\.\w")))))
    (it "ignores case in a string pattern and keeps a compiled pattern's own flags"
        (expect (= ["pandas.read_csv"] (mapv :name (dc/search es "READ_CSV"))))
        (expect (empty? (dc/search es #"READ_CSV"))))
    (it "finds a page by its opening, headings and When to use problems"
        (doseq [pattern ["working copy" "^drafts$" "review changes" "whole diff first"]]
          (expect (= ["drafts"] (mapv :name (dc/search es pattern))) pattern)))
    (it "reads a hyphenated name as words too"
        (let [pages [{:name "human-input" :kind "doc" :text "Ask for a value."}]]
          (expect (= ["human-input"] (mapv :name (dc/search pages "human input"))))
          (expect (= ["human-input"] (mapv :name (dc/search pages "HUMAN-INPUT"))))))
    (it "never searches a callable's contract or a page's body prose"
        (expect (empty? (dc/search es "matrix")))
        (expect (empty? (dc/search es "fenced heading"))))
    (it "preserves corpus order" (expect (= (mapv :name es) (mapv :name (dc/search es #".*")))))
    (it "treats a blank pattern as a listing"
        (expect (= (mapv :name es) (mapv :name (dc/search es "")))))
    (it "refuses an invalid regular expression"
        (expect (= java.util.regex.PatternSyntaxException
                   (try (dc/search es "[") nil (catch Throwable t (class t))))))))

(defdescribe
  outline-test
  "What a page or skill is found by besides its name: its opening, headings
   outside code and the problems its `When to use` list opens with, one line each."
  (it "reads a page's outline in document order"
      (expect (= ["A draft gives a session its own working copy." "Drafts" "When to use"
                  "You want to read the whole diff first." "Review changes"]
                 (dc/outline {:name "drafts" :kind "doc" :text drafts-page}))))
  (it "gives a callable no outline"
      (expect (= [] (dc/outline {:name "shell" :kind "tool" :text "# Shell\n\nRuns a command."})))))

(defdescribe
  miss-text-test
  "A miss names the handles whose name or outline contains the target, so a near
   miss costs one more `doc` call instead of a search."
  (let [es (into [{:name "drafts" :kind "doc" :text drafts-page}]
                 (map (fn [i]
                        {:name (str "tool-" i) :kind "tool" :text "A tool."}))
                 (range 7))]
    (it "suggests the handles that contain the target"
        (let [text (dc/miss-text es "Working Copy")]
          (expect (str/starts-with? text "\"Working Copy\" is not a handle."))
          (expect (str/includes? text "contains it: drafts."))))
    (it "lists name matches before outline matches"
        (expect (str/includes?
                  (dc/miss-text (conj es {:name "copy-tool" :kind "tool" :text "Copies."}) "copy")
                  "contains it: copy-tool, drafts.")))
    (it "counts a hyphenated name read as words as a name match"
        (expect (str/includes? (dc/miss-text
                                 (conj es {:name "working-copy" :kind "tool" :text "Copies."})
                                 "working copy")
                               "contains it: working-copy, drafts.")))
    (it "caps the suggestions and counts the rest"
        (expect (str/includes? (dc/miss-text es "tool")
                               "tool-0, tool-1, tool-2, tool-3, tool-4 and 2 more.")))
    (it "reads the target as literal text, not as a pattern"
        (expect (not (str/includes? (dc/miss-text es "tool.*") "contains it"))))
    (it "keeps the discovery advice when nothing matches"
        (let [text (dc/miss-text es "nothing-like-it")]
          (expect (not (str/includes? text "contains it")))
          (expect (str/includes? text "apropos(pattern)"))))))

(defdescribe
  reader-words-find-their-page-test
  "People search in their own words. Each phrase below missed its page while
   `apropos` read names only; the page's outline now carries it."
  (it "finds the page a reader means"
      (let [es (dc/entries)]
        (doseq [[words page] [["context window" "token-optimization"]
                              ["tokens" "token-optimization"] ["api key" "configuration"]
                              ["worktree" "drafts"] ["iphone" "index"] ["android" "index"]
                              ["permissions" "jail"] ["log file" "logging"] ["crash" "logging"]
                              ["plugin" "extending"] ["custom tool" "extending"]
                              ["transcript" "sessions"] ["upgrade" "distributions"]
                              ["graalvm" "jvm-native-image"] ["password" "human-input"]
                              ["llm provider" "provider-extensions"] ["workflow" "skills"]
                              ["stop" "sessions"] ["team" "council"] ["embed" "python-sdk"]
                              ["remote" "gateway-service"] ["keybindings" "keyboard-shortcuts"]
                              ["new line" "keyboard-shortcuts"] ["fork" "sessions"]]]
          (expect (some #{page} (map :name (dc/search es words))) (str words " -> " page))))))

(defdescribe experimental-guide-discovery-test
             (it "does not offer the experimental planning guide through doc or apropos"
                 (expect (not (contains? (set (map :name (dc/entries))) "working-with-plans")))))

(defdescribe index-text-test
             "`doc()` is CURATED: a hand-ordered short list that names where the rest is."
             (it "prints only curated names that exist, and points at apropos"
                 (let [es
                       [{:name "grep" :text "Search file content."}
                        {:name "zzz" :text "Not curated."}]

                       out
                       (dc/index-text es)]

                   (expect (str/includes? out "grep — Search file content."))
                   (expect (not (str/includes? out "zzz")))
                   (expect (str/includes? out "`apropos(pattern)`"))))
             (it "names verbs the way the sandbox binds them"
                 ;; Regression: the index listed the MCP verb by its WIRE name,
                 ;; `mcp__call`. `index-text` keeps only curated names that exist
                 ;; and sandbox entries are keyed by the Python binding, so the
                 ;; whole MCP surface silently vanished from `doc()`.
                 (expect (contains? (set dc/curated) "mcp_call"))
                 (doseq [nm dc/curated]
                   (expect (not (str/includes? nm "__")) nm))))

(def ^:private refused-call-shapes
  "Call shapes the live handlers REFUSE, each one cross-validated against the
   running tool before it was banned here — a document that shows one of them
   teaches a call that cannot work."
  [[#"grep\(\s*[\"\[]" "a positional query: grep takes ONE options map"]])

(defdescribe
  no-document-teaches-a-refused-call-shape-test
  "Every corpus document is model-facing instruction: `doc`/`apropos` hand it
   back as the contract to call against. A shape the handler refuses is worse
   than a missing document, because the model spends a turn discovering the
   refusal — so the corpus is scanned for the shapes the runtime rejects."
  (it "documents only call shapes the runtime accepts"
      (let [es (dc/entries)]
        (expect (seq es))
        (doseq [e es
                [re what] refused-call-shapes]

          (expect (nil? (re-find re (:text e))) (str (:name e) " documents " what)))))
  (it "catches each banned shape when one does appear"
      (let [offender "grep(\"q\")"]
        (expect (= (count refused-call-shapes)
                   (count (filter (fn [[re _]]
                                    (re-find re offender))
                                  refused-call-shapes)))))))

(defdescribe
  live-sources-test
  "Dynamic documents are plain functions. Reading them directly keeps the corpus
   current without a search index, generation stamp or invalidation protocol."
  (it "sees a source change on the next read"
      (let [value
            (atom "v1")

            runs
            (atom 0)]

        (try (dc/register-source!
               ::live
               (fn []
                 (swap! runs inc)
                 [{:name (str "live-" @value) :kind "function" :text "A live document."}]))
             (expect (some (comp #{"live-v1"} :name) (dc/entries)))
             (reset! value "v2")
             (expect (some (comp #{"live-v2"} :name) (dc/entries)))
             (expect (= 2 @runs))
             (finally (dc/register-source! ::live (constantly []))))))
  (it "keeps a throwing source out of the way of the others"
      (try (dc/register-source! ::throwing
                                (fn []
                                  (throw (ex-info "no entries" {}))))
           (expect (seq (dc/entries)))
           (finally (dc/register-source! ::throwing (constantly []))))))

(defdescribe
  body-text-test
  "What a search ROW shows: 100 characters of the symbol's own documentation, no
   more. The whole of it is one `doc(name)` away, so the row only has to prove the
   symbol is worth opening."
  (it "answers the opening of the document, whitespace collapsed"
      (expect (= "Read a CSV file into a DataFrame."
                 (dc/body-text "Read a CSV file into a DataFrame.\n\nIgnores `dtype`."))))
  (it "uses page prose rather than repeating its Markdown title"
      ;; #176: the new authoring guides must have useful apropos rows.
      (doseq [text ["# Extension design\n\nDesign typed tools from one tested package.\n\n## Next"
                    "# Extension design\r\n\r\nDesign typed tools from one tested package."]]
        (expect (= "Design typed tools from one tested package." (dc/body-text text))))
      (expect (= "Standalone title" (dc/body-text "# Standalone title"))))
  (it "stays bounded whatever the document weighs"
      (let [huge (apply str (repeat 4000 "screenshot everything everywhere. "))]
        (expect (<= (count (dc/body-text huge)) 100))))
  (it "answers an empty string when there is no prose"
      (expect (= "" (dc/body-text nil)))
      (expect (= "" (dc/body-text "   \n  ")))))

(defdescribe
  entry-text-test
  "What `doc(name)` prints for one entry: first the call, keys and raw-result
   lines, then the whole document."
  (it "puts the structure lines above the document, in that order"
      (expect (= (str "# grep  ·  callable\n\n"
                      "grep(*, query=...)\n" "Keys: query (REQUIRED)\n"
                      "Raw result: One row for each hit.\n\n" "Search file contents.")
                 (dc/entry-text {:name "grep"
                                 :call "grep(*, query=...)"
                                 :params "Keys: query (REQUIRED)"
                                 :result "Raw result: One row for each hit."
                                 :text "Search file contents.\n"}
                                "callable"))))
  (it "leaves out each structure line that the entry does not declare"
      (expect (= "# cat\n\ncat(path)\nRaw result: The lines.\n\nShow one file."
                 (dc/entry-text {:name "cat"
                                 :call "cat(path)"
                                 :result "Raw result: The lines."
                                 :text "Show one file."})))
      (expect (= "# guide\n\nRead this first."
                 (dc/entry-text {:name "guide" :text "Read this first."})))))

(defdescribe
  static-record-test
  "A static record is CHECKED where it is READ. The manifest declares which
   resources exist; `:vis.doc/record` declares what a record inside one has to be
   — before this, a catalogue with a typo contributed nothing to search and said
   nothing about it."
  (it "accepts the two shapes the store carries"
      (expect (dc/record? {:name "pandas.read_csv" :kind "function" :text "Read a CSV file."}))
      (expect (dc/record? {:name "index" :kind "doc" :resource "vis-docs/index.md"})))
  (it "refuses a record no reader could use, naming the resource it came from"
      (doseq [bad [{:kind "doc" :resource "vis-docs/index.md"} {:name "" :kind "function" :text "x"}
                   {:name "x" :kind "page" :text "x"} {:name "x" :kind "function"}
                   {:name "x" :kind "function" :text "x" :resource "vis-docs/index.md"}
                   ;; Site navigation is the docs site's own resource, never a record.
                   {:name "x" :kind "doc" :resource "vis-docs/index.md" :blurb "One sentence."}]]
        (expect (not (dc/record? bad)) (pr-str bad))
        (expect (throws? clojure.lang.ExceptionInfo #(#'dc/checked-record "test.edn" bad))
                (pr-str bad))))
  (it "refuses a page that carries prose instead of naming its file"
      ;; Refused at DECLARATION only: `:vis.doc/record` says what a READER receives,
      ;; and by then `resolved-record` has spent the address and slurped the file
      ;; into `:text`, so the resolved page carries no `:resource` at all.
      (let [inline {:name "x" :kind "doc" :text "inline"}]
        (expect (dc/record? inline))
        (expect (throws? clojure.lang.ExceptionInfo #(#'dc/checked-record "test.edn" inline)))))
  (it "reads every declared record once, spending the resource it named"
      (let [rs (#'dc/manifest-records)]
        (expect (seq rs))
        (doseq [r rs]
          (expect (contains? dc/kinds (:kind r)) (str (:name r) " carries no kind"))
          (expect (not (contains? r :resource)) (str (:name r) " still points at a resource"))
          (expect (not (str/blank? (:text r))) (str (:name r) " carries no text")))))
  (it "reads the resources on the FIRST ask and never bakes them into the Var"
      ;; A `def` here would be evaluated by `graal-build-time` inside the BUILDER, so
      ;; every parsed record would ship in the image heap of every process.
      (expect (fn? @#'dc/manifest-records))
      (let [before (#'dc/manifest-records)]
        (expect (seq before))
        ;; Read once, then answered from the cache.
        (expect (identical? before (#'dc/manifest-records)))
        (dc/forget-records!)
        (let [after (#'dc/manifest-records)]
          ;; Forgotten, so read from the resources AGAIN - same records, new value.
          (expect (not (identical? before after)))
          (expect (= before after))))))

(defdescribe normalize-name-test
             (it "unwraps the JSON-keyed doc() argument map"
                 ;; Issue #291: sandbox arguments arrive with JSON (string) keys only.
                 (expect (= "index" (dc/normalize-name {"name" "Index.md"})))
                 (expect (= "extending" (dc/normalize-name {"slug" " Extending "})))
                 (expect (= "readme" (dc/normalize-name "README.md")))))
