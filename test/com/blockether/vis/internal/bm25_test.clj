(ns com.blockether.vis.internal.bm25-test
  "The ranker itself: ORed terms priced by IDF, a shared immutable index, a
   memoized build and a typo rescue that stays off the hot path."
  (:require [com.blockether.vis.internal.bm25 :as bm25]
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
   {:name "struct_patch"
    :gist "Edit code by NAME."
    :body "Replace a function or class by its name, without an address."
    :value {:name "struct_patch"}}
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
      (expect (= ["patch" "prose-page" "run_tests" "struct_patch"]
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
      (let
        [ix
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
      (let
        [ds
         (conj (docs) {:name "race" :gist "r" :body "r" :value {:name "race"}})

         built
         (mapv deref
               (mapv (fn [_]
                       (future (bm25/cached-index ds)))
                     (range 16)))]

        (expect (= 1 (count (distinct (map #(System/identityHashCode %) built))))))))
