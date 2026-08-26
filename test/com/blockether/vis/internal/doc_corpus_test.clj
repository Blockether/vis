(ns com.blockether.vis.internal.doc-corpus-test
  "The corpus behind `apropos`/`doc`: one record per document, a usable first
   line, and a regular-expression filter over names."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.doc-corpus :as dc]
            [com.blockether.vis.internal.foundation.harness.discovery :as discovery]
            [clojure.spec.alpha :as s]
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
        (expect (nil? (:call entry))))))

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
  "`apropos` is a regular-expression filter over symbol names. It preserves the
   corpus order and ignores document bodies."
  (let [es [{:name "numpy" :text "Array module."}
            {:name "numpy.linalg.solve" :text "Solve a matrix equation."}
            {:name "pandas.read_csv" :text "Read comma-separated data."}
            {:name "run_tests" :text "Runs project tests."}]]
    (it "matches names with a caller-supplied regular expression"
        (expect (= ["numpy" "numpy.linalg.solve"] (mapv :name (dc/search es #"numpy(?:\..*)?"))))
        (expect (= ["numpy.linalg.solve" "pandas.read_csv"] (mapv :name (dc/search es #"\.")))))
    (it "does not search document text" (expect (empty? (dc/search es #"matrix"))))
    (it "preserves corpus order" (expect (= (mapv :name es) (mapv :name (dc/search es #".*")))))
    (it "treats a blank pattern as a listing"
        (expect (= (mapv :name es) (mapv :name (dc/search es "")))))
    (it "refuses an invalid regular expression"
        (expect (= java.util.regex.PatternSyntaxException
                   (try (dc/search es "[") nil (catch Throwable t (class t))))))))

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
                   (expect (str/includes? out "`apropos(pattern)`")))))

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
  live-sources-test
  "Dynamic documents are plain functions. Reading them directly keeps the corpus
   current without a search index, generation stamp or invalidation protocol."
  (it "sees a source change on the next read"
      (let [value
            (atom "v1")

            runs
            (atom 0)]

        (try (dc/register-source! ::live
                                  (fn []
                                    (swap! runs inc)
                                    [{:name (str "live-" @value) :text "A live document."}]))
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
  (it "stays bounded whatever the document weighs"
      (let [huge (apply str (repeat 4000 "screenshot everything everywhere. "))]
        (expect (<= (count (dc/body-text huge)) 100))))
  (it "answers an empty string when there is no prose"
      (expect (= "" (dc/body-text nil)))
      (expect (= "" (dc/body-text "   \n  ")))))

(defdescribe
  static-record-test
  "A static record is CHECKED where it is READ. The manifest declares which
   resources exist; `:vis.doc/record` declares what a record inside one has to be
   — before this, a catalogue with a typo contributed nothing to search and said
   nothing about it."
  (it "accepts the two shapes the store carries"
      (expect (s/valid? :vis.doc/record
                        {:name "pandas.read_csv" :kind "function" :text "Read a CSV file."}))
      (expect (s/valid? :vis.doc/record
                        {:name "gateway"
                         :kind "doc"
                         :resource "vis-docs/gateway.md"
                         :title "Gateway"
                         :section "Run it"
                         :order 30
                         :blurb "One sentence."})))
  (it "refuses a record no reader could use, naming the resource it came from"
      (doseq [bad [{:kind "doc" :resource "vis-docs/gateway.md"}
                   {:name "" :kind "function" :text "x"} {:name "x" :kind "page" :text "x"}
                   {:name "x" :kind "function"}
                   {:name "x" :kind "function" :text "x" :resource "vis-docs/gateway.md"}
                   {:name "x" :kind "doc" :text "inline"}]]
        (expect (not (s/valid? :vis.doc/record bad)) (pr-str bad))
        (expect (throws? clojure.lang.ExceptionInfo #(#'dc/checked-record "test.edn" bad))
                (pr-str bad))))
  (it "reads every declared record once, spending the resource it named"
      (let [rs (dc/records)]
        (expect (seq rs))
        (doseq [r rs]
          (expect (contains? dc/kinds (:kind r)) (str (:name r) " carries no kind"))
          (expect (not (contains? r :resource)) (str (:name r) " still points at a resource"))
          (expect (not (str/blank? (:text r))) (str (:name r) " carries no text")))))
  (it "reads the resources on the FIRST ask and never bakes them into the Var"
      ;; A `def` here would be evaluated by `graal-build-time` inside the BUILDER, so
      ;; every parsed record would ship in the image heap of every process.
      (expect (fn? dc/records))
      (let [before (dc/records)]
        (expect (seq before))
        ;; Read once, then answered from the cache.
        (expect (identical? before (dc/records)))
        (dc/forget-records!)
        (let [after (dc/records)]
          ;; Forgotten, so read from the resources AGAIN - same records, new value.
          (expect (not (identical? before after)))
          (expect (= before after))))))
