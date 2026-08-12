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
  "ONE record for every document, whatever seeded it: `name` + `text`, and `call`
   only when there is something to call. No `kind`, no `group` — a taxonomy the
   model cannot act on is a field on every row."
  (it "carries exactly the specified keys"
      (let [es (dc/entries)]
        (expect (seq es))
        (doseq [e es]
          (expect (string? (:name e)))
          (expect (not (str/blank? (:name e))))
          (expect (string? (:text e)))
          (expect (not (str/blank? (:text e))))
          (expect (empty? (dissoc e :name :text :call)))))))

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
      (let
        [skills
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

(defdescribe search-test
             "Rank IS the \"where did it match\" answer, and terms are ANDed."
             (let
               [es [{:name "grep" :text "Search file CONTENT and names.\n\nRipgrep-backed."}
                    {:name "cat" :text "Read files.\n\nReturns anchors for patching."}
                    {:name "patch" :text "Anchored edits.\n\nUses the anchors cat returned."}]]
               (it "ranks an exact name above a body hit"
                   ;; "cat" IS a name and is also a word inside `patch`'s body.
                   (expect (= ["cat" "patch"] (mapv :name (dc/search es "cat"))))
                   (expect (= ["grep"] (mapv :name (dc/search es "grep"))))
                   (expect (= ["cat" "patch"] (mapv :name (dc/search es "anchors")))))
               (it "ANDs every term"
                   (expect (= ["cat"] (mapv :name (dc/search es "read anchors"))))
                   (expect (empty? (dc/search es "read ripgrep"))))
               (it "answers a blank query with the whole corpus in name order"
                   (expect (= ["cat" "grep" "patch"] (mapv :name (dc/search es "")))))))

(defdescribe index-text-test
             "`doc()` is CURATED: a hand-ordered short list that names where the rest is."
             (it "prints only curated names that exist, and points at apropos"
                 (let
                   [es
                    [{:name "grep" :text "Search file content."} {:name "zzz" :text "Not curated."}]

                    out
                    (dc/index-text es)]

                   (expect (str/includes? out "grep — Search file content."))
                   (expect (not (str/includes? out "zzz")))
                   (expect (str/includes? out "`apropos(text)`")))))
