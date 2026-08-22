(ns com.blockether.vis.internal.bm25-test
  "The ranker itself: ORed terms priced by IDF, a shared immutable index, a
   memoized build and a typo rescue that stays off the hot path."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.bm25 :as bm25]
            [lazytest.core :refer [defdescribe expect it]]))

(defn- docs
  "A miniature corpus with the two shapes that decide every ranking question: a
   short precise contract and a long prose page that mentions everything."
  []
  [{:name "patch"
    :gist "Edit a file by ADDRESS."
    :body (str "Edit a file by ADDRESS. Every edit for one file goes in one call: "
               "from_anchor, to_anchor and replace. An empty replace deletes.")
    :value {:name "patch"}}
   {:name "format_code"
    :gist "Format code in place."
    :body "Reformat a file or a code string with the project's own formatter."
    :value {:name "format_code"}}
   {:name "run_tests"
    :gist "Run the test suite."
    :body "Select tests by paths only: a clojure var, a file, a directory."
    :value {:name "run_tests"}}
   {:name "prose-page"
    :gist "A long page of workflow narrative."
    :body (apply str
            (repeat 400 "how do I edit a file and replace lines in it patch tests var clojure "))
    :value {:name "prose-page"}}])

(defn- names [hits] (mapv :name hits))

(defdescribe
  ored-terms-test
  "`search` ANDed its terms, so a six-word ask that several documents covered in
   part answered nothing at all. Terms are ORed and priced by IDF now."
  (it "answers partial coverage instead of nothing"
      (let [hits (bm25/search (docs) "patch from_anchor to_anchor replace edits schema")]
        (expect (seq hits))
        (expect (= "patch" (:name (first hits))))))
  (it "still answers a handle typed alone with that handle"
      (expect (= "patch" (:name (first (bm25/search (docs) "patch"))))))
  (it "reads snake_case, camelCase and spaces as the same ask"
      (expect (= (names (bm25/search (docs) "from_anchor"))
                 (names (bm25/search (docs) "fromAnchor"))
                 (names (bm25/search (docs) "from anchor")))))
  (it "answers everything in name order for a blank query"
      (expect (= ["format_code" "patch" "prose-page" "run_tests"]
                 (names (bm25/search (docs) ""))))))

(defdescribe
  length-normalization-test
  "A long prose page must not win a query the short contract answers — the body
   field is fully length-normalized so size alone buys no rank."
  (it "does not let a long page outrank a contract on the contract's own words"
      (let [ranked (names (bm25/search (docs) "edit one file by address in a single call"))]
        (expect (= "patch" (first ranked)))
        (expect (< (.indexOf ^java.util.List ranked "patch")
                   (.indexOf ^java.util.List ranked "prose-page")))))
  (it "answers the tool page for a description of what it does"
      (expect (= "run_tests"
                 (:name (first (bm25/search (docs) "run tests for one clojure var")))))))

(defdescribe typo-and-nonsense-test
             "A term nothing carries is either a typo worth rescuing or an honest miss."
             (it "rescues a transposition"
                 (expect (= "patch" (:name (first (bm25/search (docs) "pathc"))))))
             (it "answers nothing at all when nothing is close"
                 (expect (empty? (bm25/search (docs) "zzqqxk plorbfnat")))))

(defdescribe
  index-cache-test
  "`apropos` rebuilds an EQUAL corpus on every call, so the cache is keyed by
   value: identity would never hit and the index would be rebuilt per query."
  (it "reuses one index for a rebuilt-but-equal document set"
      (expect (identical? (bm25/cached-index (docs)) (bm25/cached-index (docs)))))
  (it "builds a fresh index when the documents change"
      (expect (not (identical? (bm25/cached-index (docs))
                               (bm25/cached-index
                                 (conj
                                   (docs)
                                   {:name "extra" :gist "x" :body "x" :value {:name "extra"}})))))))

(defdescribe
  parallel-ranking-test
  "An index is an immutable value over arrays and `rank` allocates only its own
   accumulator, so any number of threads may rank against one index at once."
  (it "answers identically from many threads"
      (let [ix
            (bm25/cached-index (docs))

            qs
            ["patch" "how do I replace lines in a file" "run tests for one clojure var" "pathc"]

            expected
            (mapv #(names (bm25/rank ix %)) qs)

            answers
            (mapv deref
                  (mapv (fn [_]
                          (future (mapv #(names (bm25/rank ix %)) qs)))
                        (range 16)))]

        (expect (= 1 (count (distinct answers))))
        (expect (= expected (first answers)))))
  (it "builds one index under concurrent first callers"
      (let [ds
            (conj (docs) {:name "race" :gist "r" :body "r" :value {:name "race"}})

            built
            (mapv deref
                  (mapv (fn [_]
                          (future (bm25/cached-index ds)))
                        (range 16)))]

        (expect (= 1 (count (distinct (map #(System/identityHashCode %) built))))))))

(defn- doc*
  "One document, body defaulting to the gist."
  ([nm gist] (doc* nm gist gist))
  ([nm gist body] {:name nm :gist gist :body body :value {:name nm}}))

(defdescribe
  stemming-test
  "`run tests` and `run test` shared no document, so one query word's plural
   decided whether the tool page was found at all — and `formatting the source
   code` shared none with a page that says `format`, until the fold covered
   TENSE as well as number."
  (it "answers the same document for a plural and a singular ask"
      (let [ds [(doc* "run_tests" "Run the test suite." "Select tests by paths only.")
                (doc* "shell" "Run a program." "Start a process and read its logs.")]]
        (expect (= "run_tests" (:name (first (bm25/search ds "run tests")))))
        (expect (= "run_tests" (:name (first (bm25/search ds "run test")))))))
  ;; A gerund or a past tense shared no term with a page written in the present:
  ;; `formatting the source code` and `format_code` had nothing in common, and 10
  ;; of 14 such asks answered the wrong tool.
  (it "answers the same document for a gerund, a past tense and a plain ask"
      (let [ds [(doc* "format_code"
                      "Format source code."
                      "Formats a file or a whole project in place.")
                (doc* "list_sessions" "List past sessions." "Lists every session, newest first.")]]
        (expect (= "format_code" (:name (first (bm25/search ds "formatting the source code")))))
        (expect (= "format_code" (:name (first (bm25/search ds "formatted a file")))))
        (expect (= "list_sessions" (:name (first (bm25/search ds "listing my past sessions")))))))
  (it "folds a tense onto ONE term, index and query alike"
      (expect (= ["defin"] (vec (bm25/terms "define"))))
      (expect (= ["defin"] (vec (bm25/terms "defines"))))
      (expect (= ["defin"] (vec (bm25/terms "defined"))))
      (expect (= ["run"] (vec (bm25/terms "running"))))
      (expect (= ["use"] (vec (bm25/terms "used")))))
  (it "never cuts a technical word down to a vowel-less stump"
      (expect (= ["string"] (vec (bm25/terms "string"))))
      (expect (= ["bring"] (vec (bm25/terms "bring"))))
      (expect (= ["read"] (vec (bm25/terms "read")))))
  (it "folds the plural of a handle, never the handle itself"
      (expect (= ["run" "test"] (vec (bm25/terms "run tests"))))
      (expect (= "run_tests" (bm25/normalized-handle "run_tests")))))

(defdescribe
  unicode-tokenization-test
  "Tokens were `[A-Za-z0-9]+`, so an accented or non-Latin ask tokenized to
   NOTHING and fell into the typo rescue, which answered confident noise."
  (it "reads a non-ASCII word as a word"
      (let [ds [(doc* "zapytanie" "Wyszukiwanie dokumentów po treści.")
                (doc* "patch" "Edit a file by address.")]]
        (expect (= "zapytanie" (:name (first (bm25/search ds "dokumentów")))))
        (expect (= "zapytanie" (:name (first (bm25/search ds "wyszukiwanie")))))))
  (it "answers nothing for a non-Latin ask no document covers"
      (expect (empty? (bm25/search (docs) "wyszukiwanie dokumentów")))))

(defdescribe
  compound-word-test
  "A document spelling it `arXiv` indexed `ar` + `xiv`, while the way anyone
   types it — `arxiv` — stayed ONE term, so the two never met: `apropos` for
   `arxiv` found no paper search at all and answered whatever the typo rescue
   reached instead."
  (it "keeps the whole word beside its camelCase pieces"
      (expect (= ["ar" "xiv" "arxiv"] (vec (bm25/terms "arXiv"))))
      (expect (= ["git" "hub" "github"] (vec (bm25/terms "GitHub")))))
  (it "never joins across a separator"
      (expect (= ["git" "hub"] (vec (bm25/terms "git hub"))))
      (expect (= ["from" "anchor"] (vec (bm25/terms "from_anchor")))))
  (it "answers the tool for the way a human spells its subject"
      (let [ds [(doc* "search_papers" "Search arXiv for relevant papers.")
                (doc* "search_code" "Search public code with GitHub Code Search.")
                (doc* "patch" "Edit a file by address.")]]
        (expect (= "search_papers" (:name (first (bm25/search ds "arxiv")))))
        (expect (= "search_code" (:name (first (bm25/search ds "github")))))))
  (it "leaves a handle an identifier"
      (expect (= "from_anchor" (bm25/normalized-handle "fromAnchor")))))

(defdescribe
  prefix-and-typo-test
  "A term nothing carries was only ever spell-corrected, so an interactively
   typed PREFIX answered the wrong document — or, at distance 1 from two
   handles, the shorter one."
  (it "completes a prefix of at least three characters"
      (expect (= "format_code" (:name (first (bm25/search (docs) "form"))))))
  (it "prefers the same-length correction to a shorter neighbour"
      (let [ds [(doc* "patch" "Edit a file by address.") (doc* "path" "A filesystem path.")]]
        (expect (= "patch" (:name (first (bm25/search ds "pathc")))))))
  (it "leaves a term alone when a document carries it"
      (expect (= "patch" (:name (first (bm25/search (docs) "patch")))))))

(defdescribe
  handle-bonus-test
  "The exact-name bonus was whole-query only: one extra word and the handle
   lost outright, which made `patch anchors` a ranking cliff."
  (it "keeps the handle first when the ask carries more than the handle"
      (expect (= "patch" (:name (first (bm25/search (docs) "patch anchors")))))
      (expect (= "run_tests" (:name (first (bm25/search (docs) "run_tests for a var"))))))
  (it "does not hand the bonus to a document the ask only mentions"
      (let [ds [(doc* "prose" "Everything" (str/join " " (repeat 200 "patch tests file")))
                (doc* "patch" "Edit a file by address.")]]
        (expect (= "patch" (:name (first (bm25/search ds "patch"))))))))

;; Cycle 2: a shim page is filed under `pandas`, but the model types `read_csv`.
(defdescribe
  alias-bonus-test
  "A page was reachable only by the name it was filed under, so `read_csv`
   answered `read_attachment` — a document that merely carries the word `read`."
  (it "answers the page that LENDS the name, not one that only spells it"
      (let [ds [(doc* "read_attachment" "Read one attachment's bytes by id.")
                (assoc (doc* "pandas" "Pure-Python pandas subset over plain Python.")
                  :aliases ["read_csv" "DataFrame"])]]
        (expect (= "pandas" (:name (first (bm25/search ds "read_csv")))))
        (expect (= "pandas" (:name (first (bm25/search ds "DataFrame")))))))
  (it "wants the whole lent name, never the English half of it"
      (let [ds [(doc* "read_attachment" "Read one attachment's bytes by id.")
                (assoc (doc* "pandas" "Pure-Python pandas subset over plain Python.")
                  :aliases ["read_csv"])]]
        (expect (= "read_attachment" (:name (first (bm25/search ds "read")))))))
  (it "leaves a described ask to the text that answers it"
      (let [ds [(assoc (doc* "pandas"
                             "Pure-Python pandas subset."
                             "Series and DataFrame over plain Python.")
                  :aliases ["open" "read_csv"])
                (doc* "attach"
                      "Persist an artifact."
                      "Open and read the bytes of an attachment you saved earlier.")]]
        (expect (= "attach"
                   (:name (first (bm25/search ds "open and read the bytes of an attachment"))))))))
(defdescribe
  limit-test
  "`rank` scored, kept and sorted EVERY positive document, so a ten-row answer
   still cost a full sort of the corpus."
  (it "answers exactly the requested number, in the same order"
      (let [all (names (bm25/search (docs) "file patch tests"))]
        (expect (= 2 (count (bm25/search (docs) "file patch tests" {:limit 2}))))
        (expect (= (vec (take 2 all)) (names (bm25/search (docs) "file patch tests" {:limit 2}))))))
  (it "treats a limit past the end as no limit"
      (expect (= (names (bm25/search (docs) "patch"))
                 (names (bm25/search (docs) "patch" {:limit 99}))))))

(defdescribe
  opts-test
  "Weights were private constants, so a second consumer could not retune the
   ranker without editing it."
  (it "scores with the caller's weights"
      (let [ds [(doc* "zeta" "A short contract." "It takes one argument.")
                (doc* "alpha" "Mentions zeta a lot." "zeta zeta zeta zeta zeta")]]
        (expect (= "zeta" (:name (first (bm25/search ds "zeta")))))
        (expect
          (= "alpha"
             (:name (first
                      (bm25/search ds "zeta" {:field-weights [0.0 3.0 1.0] :handle-bonus 0.0})))))))
  (it "indexes the same documents once per distinct option map"
      (expect (identical? (bm25/cached-index (docs) {:k1 2.0})
                          (bm25/cached-index (docs) {:k1 2.0})))
      (expect (not (identical? (bm25/cached-index (docs)) (bm25/cached-index (docs) {:k1 2.0}))))))

(defdescribe
  index-eviction-test
  "The cache cleared WHOLESALE at capacity, so one odd corpus threw away every
   warm index."
  (it "evicts the least recently used index and keeps the newest"
      (let [ds-n
            (fn [n]
              [(doc* (str "doc" n) (str "Document number " n))])

            first-ix
            (bm25/cached-index (ds-n 0))

            newer
            (mapv #(bm25/cached-index (ds-n (inc %))) (range 8))]

        (expect (identical? (last newer) (bm25/cached-index (ds-n 8))))
        (expect (not (identical? first-ix (bm25/cached-index (ds-n 0))))))))

(defdescribe
  resolved-terms-meta-test
  "Prefix completion and spell correction happen INSIDE the ranker, so a reader
   cannot re-derive what the query became. The answer carries the resolution."
  (it "names what each term resolved to, and prices it"
      (let [terms (:terms (meta (bm25/search (docs) "pathc")))]
        (expect (= [{:term "pathc" :as "patch"}] (mapv #(select-keys % [:term :as]) terms)))
        (expect (every? #(pos? (double (:idf %))) terms))))
  (it "keeps a term that needed no rescue as itself"
      (expect (= [["patch" "patch"]]
                 (mapv (juxt :term :as) (:terms (meta (bm25/search (docs) "patch")))))))
  (it "drops a term no document can answer"
      (expect (empty? (:terms (meta (bm25/search (docs) "zzqqxk"))))))
  (it "carries an empty term list for a blank query, so the shape never varies"
      (expect (= [] (:terms (meta (bm25/search (docs) "")))))))

(defn- patch-page
  "The `patch` contract, with `lead` — a structural line such as a rendered call
   signature — moved ABOVE its opening line, exactly as one would arrive. The
   prose never changes, so what a ranking loses is one line's worth of evidence."
  ([] (patch-page nil))
  ([lead]
   (let [prose (str
                 "Edit a file by ADDRESS. Every edit for one file goes in one call: from_anchor, "
                 "to_anchor and replace. An empty replace deletes the addressed lines. The call "
                 "answers a status line with the path, the edit count and the lines before and "
                 "after. Address a region by the anchor a read printed, never by retyping the "
                 "text you replace. One call carries every edit for that file, so two calls on "
                 "one file are a mistake.")]
     (into [{:name "patch"
             :gist (or lead "Edit a file by ADDRESS.")
             :body (if lead (str lead " " prose) prose)
             :value {:name "patch"}}]
           (rest (docs))))))

;; Regression: the three fields were scored as three independent BM25s and summed,
;; so each saturated at its own weight and ONE line could hold 61% of a document's
;; score. A call line rendered above `patch`'s opening line — its prose untouched —
;; dropped it from rank 1 to rank 19 for `how do I replace lines in a file`.
(defdescribe
  field-saturation-test
  "The three fields saturate TOGETHER, over one weighted pseudo-frequency, so an
   opening line is evidence and never the verdict."
  (it "keeps a document's score when a structural line takes over its first line"
      (let [q
            "edit one file by address in a single call"

            score-of
            (fn [ds]
              (double (:score (first (filter #(= "patch" (:name %)) (bm25/search ds q))))))]

        (expect (= "patch"
                   (:name (first (bm25/search (patch-page "patch(path, edits) -> status") q)))))
        (expect (> (/ (score-of (patch-page "patch(path, edits) -> status"))
                      (score-of (patch-page)))
                   0.9))))
  (it "answers the body that covers the ask over the line that mentions it once"
      (let [ds [{:name "one-liner"
                 :gist "Replace lines in a file."
                 :body
                 (str
                   "Replace lines in a file. "
                   (apply str
                     (repeat
                       24
                       "It is about something else entirely and never returns to the subject. ")))
                 :value {:name "one-liner"}}
                {:name "worker"
                 :gist "A tool page."
                 :body
                 (str "A tool page. "
                      (apply str
                        (repeat
                          12
                          "Replace the lines of a file, then replace more lines in that file. ")))
                 :value {:name "worker"}}]]
        (expect (= "worker" (:name (first (bm25/search ds "replace lines in a file"))))))))
