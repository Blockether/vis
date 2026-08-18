(ns com.blockether.vis.internal.doc-corpus-test
  "The corpus behind `apropos`/`doc`: one record per document, a first line that
   is a usable gist, and a search that ranks a name above a body."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.doc-corpus :as dc]
            [com.blockether.vis.internal.foundation.harness.discovery :as discovery]
            [lazytest.core :refer [defdescribe expect it]]))

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
      (let [skills
            (discovery/skills)

            by-name
            (into {} (map (juxt :name identity)) (dc/entries))]

        (expect (seq skills))
        (doseq [s skills]
          (let [e (get by-name (:name s))]
            (expect (some? e))
            (expect (str/ends-with? (:text e) (str (:body s))))
            ;; No `call`: there is no skill verb. `doc(name)` IS the whole use.
            (expect (nil? (:call e))))))))

(defdescribe
  reading-a-skill-has-no-session-effect-test
  "A skill is a DOCUMENT: the corpus reads the discovery registry directly, so
   `apropos`/`doc` can neither activate nor mark anything."
  (it "never reaches the harness verb namespace"
      (let [src (slurp (io/resource "com/blockether/vis/internal/doc_corpus.clj"))]
        (expect (not (str/includes? src "harness.core")))
        (expect (not (str/includes? src "harness-core"))))))

(defdescribe
  search-test
  "BM25F: terms are ORed and priced by IDF, so a description ranks rather
              than filters, and only a query nothing carries answers nothing."
  (let
    [es
     [{:name "grep" :text "Search file CONTENT and names.\n\nRipgrep-backed."}
      {:name "cat" :text "Read files.\n\nReturns anchors for patching."}
      {:name "patch"
       :text (str "Anchored edits: replace lines in a file.\n\n"
                  "Uses the anchors cat returned. Every edit is "
                  "{\"from_anchor\": a, \"to_anchor\": b, \"replace\": text}.")}
      ;; A long prose document — the shape that used to win every
      ;; natural-language query by containing all of its words.
      {:name "prose-page"
       :text (str "A long page of workflow narrative.\n\n"
                  (str/join " " (repeat 200 "how do I open a file in the session and read it")))}
      ;; Four documents priced every term alike — `replace` and `how` shared one
      ;; IDF — so a natural-language ask could only be told apart by the field a
      ;; term sat in. A corpus that uses the common words more than once prices
      ;; them, which is what IDF is for.
      {:name "shell"
       :text
       "Run a command.\n\nHow do I run a command in the session and read its log? A shell answers a handle."}
      {:name "ls"
       :text "Map a directory.\n\nHow do I open a directory and read what is in it, file by file?"}
      {:name "read_session"
       :text "Read a session.\n\nHow do I read another session, and what does it hold in it?"}]]
    (it "ranks an exact handle first, whatever the bodies say"
        ;; "cat" IS a name and is also a word inside `patch`'s body.
        (expect (= "cat" (:name (first (dc/search es "cat")))))
        (expect (= ["grep"] (mapv :name (dc/search es "grep"))))
        ;; A handle typed in any casing/separator style is the same ask.
        (expect (= "patch" (:name (first (dc/search es "Patch"))))))
    (it "keeps a document that covers only part of the query"
        ;; Regression: `every? pos?` used to discard the entry before
        ;; scoring, so three matching terms out of six answered nothing.
        (let [hits (mapv :name (dc/search es "patch from_anchor to_anchor replace edits schema"))]
          (expect (seq hits))
          (expect (= "patch" (first hits)))))
    (it "does not let a long document win a natural-language query"
        ;; Regression: without full body length normalization the long
        ;; prose page outranked the short contract that actually answers.
        (expect (= "patch" (:name (first (dc/search es "how do I replace lines in a file"))))))
    (it "splits snake_case and camelCase into the same terms"
        (expect (= "patch" (:name (first (dc/search es "from anchor to anchor")))))
        (expect (= "patch" (:name (first (dc/search es "fromAnchor"))))))
    (it "rescues a typo, and only a typo"
        (expect (= "patch" (:name (first (dc/search es "pathc")))))
        (expect (empty? (dc/search es "kubernetes helm rollout"))))
    (it "answers a blank query with the whole corpus in name order"
        (expect (= ["cat" "grep" "ls" "patch" "prose-page" "read_session" "shell"]
                   (mapv :name (dc/search es "")))))))

(defdescribe
  every-document-answers-its-own-name-test
  "The floor no ranking change may cross: asking for a handle by name must
              return that document first, over the whole real corpus."
  (it "returns itself first for every corpus name"
      (let [es (dc/entries)]
        (expect (seq es))
        (doseq [e es]
          (expect (= (:name e) (:name (first (dc/search es (:name e)))))
                  (str (:name e) " does not answer its own name first"))))))

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
                   (expect (str/includes? out "`apropos(text)`")))))

(def ^:private refused-call-shapes
  "Call shapes the live handlers REFUSE, each one cross-validated against the
   running tool before it was banned here — a document that shows one of them
   teaches a call that cannot work."
  [[#"(?:run_tests|repl_eval|format_code|lint_code)\(\"[^\"]*\"\)"
    (str "a lone string argument: the language surface reads the pack from "
         "{\"language\": \"…\"} (or the FIRST of two arguments), so a lone string "
         "is the PAYLOAD and the call lands on the workspace's primary pack")]
   [#"grep\(\s*[\"\[]" "a positional query: grep takes ONE options map"]])

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
      (let [offender "run_tests(\"python\") grep(\"q\")"]
        (expect (= (count refused-call-shapes)
                   (count (filter (fn [[re _]]
                                    (re-find re offender))
                                  refused-call-shapes)))))))

(defdescribe
  stamped-sources-test
  "Every `apropos` and every `doc` rebuilt the whole corpus — re-reading every
   documentation page and every SKILL.md — because a source could only be asked
   for its ENTRIES. Now it is asked for a cheap stamp first."
  (it "answers the identical corpus while no source has changed"
      (expect (identical? (dc/entries) (dc/entries))))
  (it "does not run a source whose stamp is unchanged"
      (let [runs (atom 0)]
        (try (dc/register-source! ::stamped
                                  (constantly :v1)
                                  (fn []
                                    (swap! runs inc)
                                    [{:name "stamped-doc" :text "A document behind a stamp."}]))
             (dc/entries)
             (dc/entries)
             (dc/entries)
             (expect (= 1 @runs))
             (finally (dc/register-source! ::stamped (constantly :gone) (constantly []))))))
  (it "re-runs a source the moment its stamp changes"
      (let [stamp
            (atom :v1)

            runs
            (atom 0)]

        (try (dc/register-source! ::restamped
                                  (fn []
                                    @stamp)
                                  (fn []
                                    (swap! runs inc)
                                    [{:name (str "restamped-" (name @stamp))
                                      :text "Behind a stamp."}]))
             (dc/entries)
             (expect (some (comp #{"restamped-v1"} :name) (dc/entries)))
             (reset! stamp :v2)
             (expect (some (comp #{"restamped-v2"} :name) (dc/entries)))
             (expect (= 2 @runs))
             (finally (dc/register-source! ::restamped (constantly :gone) (constantly []))))))
  (it "keeps a source that cannot be stamped out of the way of the others"
      (try (dc/register-source! ::throwing
                                (fn []
                                  (throw (ex-info "no stamp" {})))
                                (fn []
                                  (throw (ex-info "no entries" {}))))
           (expect (seq (dc/entries)))
           (finally (dc/register-source! ::throwing (constantly :gone) (constantly []))))))

(def ^:private deep-doc
  "A document whose answer is nowhere near its first line — the shape that made
   a one-line gist useless: a skill, a documentation page, a long contract."
  (str "Automate a browser end to end with the spel command line.\n\n"
       (str/join "\n" (repeat 40 "Filler prose about unrelated matters."))
       "\nCapture a screenshot of the page before asserting.\n"
       (str/join "\n" (repeat 40 "More filler that says nothing."))
       "\nThe screenshot lands beside the report.\n"))

(defn- term "One resolved query term, as `bm25/rank` hands them over." [t] {:term t :as t :idf 3.0})

(defdescribe
  preview-test
  "What a search row SHOWS. The body is never in it — `doc(name)` answers that
   whole — so the excerpt has to prove the document is worth opening: its own
   opening, the region the query landed in, and a fragment from deeper down."
  (it "shows the opening, the matched region and a fragment from further down"
      (let [{:keys [gist at hit]}
            (dc/preview deep-doc [(term "screenshot")])

            wanted
            (inc (count (take-while #(not (str/includes? % "Capture"))
                                    (str/split-lines deep-doc))))]

        (expect (str/starts-with? gist "Automate a browser end to end"))
        (expect (str/includes? gist "Capture a screenshot"))
        (expect (str/includes? gist "lands beside the report"))
        (expect (= wanted at) "`at` must be the line the matched region starts on")
        (expect (= ["screenshot"] hit))))
  (it "answers `at` 0 when the opening already held the match"
      (let [text
            "Run pack tests; prefer the smallest target: a file or a directory."

            {:keys [gist at]}
            (dc/preview text [(term "tests")])]

        (expect (zero? at))
        (expect (= text gist))))
  (it "takes the line under a breadcrumb opening, which says nothing alone"
      (let
        [{:keys [gist]}
         (dc/preview
           "Drafts \u00b7 Using Vis\n\nIsolated workspaces for speculative changes.\n\n# Drafts\n"
           nil)]
        (expect (str/starts-with? gist "Drafts \u00b7 Using Vis \u2014 Isolated workspaces"))))
  (it "answers the opening alone when nothing was asked"
      (let [{:keys [gist at hit]} (dc/preview deep-doc nil)]
        (expect (= "Automate a browser end to end with the spel command line." gist))
        (expect (zero? at))
        (expect (= [] hit))))
  (it "renders a correction, so a rewritten query is never silent"
      (expect (= ["pathc\u2192patch"]
                 (:hit (dc/preview "Apply every anchored edit for one file: patch(path, edits)."
                                   [{:term "pathc" :as "patch" :idf 3.0}])))))
  (it "stays bounded whatever the document weighs"
      (let [huge (apply str (repeat 4000 "screenshot everything everywhere all the time. "))]
        (expect (<= (count (:gist (dc/preview huge [(term "screenshot")]))) 300))))
  (it "never re-shows what the opening already printed"
      (let [text
            (str "Apply EVERY anchored edit for one file in a single atomic write, "
                 "in prose or in code, with patch(path, edits).\n\n"
                 "Every anchor resolves against ONE read.")

            {:keys [gist]}
            (dc/preview text [(term "anchor")])]

        (expect (< (count (re-seq #"anchored edit" gist)) 2)))))
