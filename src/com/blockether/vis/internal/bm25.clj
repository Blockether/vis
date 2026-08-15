(ns com.blockether.vis.internal.bm25
  "BM25F over small in-memory document sets — the one ranker behind `apropos`.

   A document is `{:name … :gist … :body … :value …}`: three text fields and an
   opaque payload the caller gets back on a hit. Nothing here knows what a
   corpus, a skill or a Python callable is, so a second consumer (session
   search, the TUI picker) needs no new engine.

   Three properties the callers depend on:

   - **ORed, not ANDed.** A term no document carries costs nothing instead of
     emptying the result, so a six-word natural-language ask answers the
     document that covers most of it.
   - **IDF, never a stoplist.** `how`, `do` and `a` sit in nearly every
     document, so they price themselves at ~0 and no word list can go stale.
   - **Length-normalized bodies (`b` 1.0).** Without it a 70 KB skill outranks
     a 350 B contract by containing every word.

   Speed and concurrency, because `apropos` rebuilds its corpus on every call:

   - An index is built ONCE per distinct document set and memoized in
     `index-cache`; the entries vector is the key, so the rebuilt-but-equal
     corpus of the next call hits.
   - An index is an immutable value over Java arrays and is safe to share
     across threads; `rank` allocates only its own score accumulator, so any
     number of threads may rank against one index at once. `ConcurrentHashMap`
     `computeIfAbsent` makes the miss path single-build, not single-file.
   - Scoring walks the POSTINGS of the query's terms, never the corpus: a
     one-term query touches the handful of documents that carry it, not all
     194. Per-document, per-field length normalization is precomputed at index
     time, so the inner loop is two array reads and a divide."
  (:require [clojure.string :as str])
  (:import (java.util HashMap Map$Entry)
           (java.util.concurrent ConcurrentHashMap)))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Tuning
;; =============================================================================

(def ^:private ^:const k1
  "BM25 term-frequency saturation. The textbook value; nothing in a corpus of
   contracts and skills repeats a term often enough to want another."
  1.2)

(def ^:private ^:const exact-name-bonus
  "Flat bonus when the whole query normalizes to a document's own handle.
   `apropos(\"patch\")` must answer `patch` first whatever the bodies say, and a
   flat term-independent bonus does that without letting a long name win a
   natural-language query the way a per-term name boost does."
  100.0)

(def ^:private ^:const field-count
  "Slots per document: name, gist, body. Every flat array is strided by it."
  3)

(def ^:private field-keys
  "The three scored fields, in slot order: name, first line, whole body."
  [:name :gist :body])

(def ^:private field-weight
  "Weight per field slot. A name hit is worth much more than a body hit."
  (double-array [8.0 3.0 1.0]))

(def ^:private field-b
  "Length-normalization strength per field slot. The body is FULLY normalized."
  (double-array [0.75 0.75 1.0]))

(def ^:private camel-boundary #"([a-z0-9])([A-Z])")
(def ^:private word-run #"[A-Za-z0-9]+")

(defn tokens
  "Split `s` into comparable terms: camelCase and snake_case both break apart
   (`from_anchor` and `fromAnchor` are `from` + `anchor`), everything
   non-alphanumeric is a separator, and the result is lower-case."
  [s]
  (into [] (map str/lower-case) (re-seq word-run (str/replace (str s) camel-boundary "$1 $2"))))

(defn normalized-handle
  "A name or a whole query as ONE comparable string, so `from_anchor`,
   `fromAnchor` and `from anchor` all name the same handle."
  [s]
  (str/join "_" (tokens s)))

;; =============================================================================
;; Index — built once, shared by every thread that ranks against it
;; =============================================================================

(defn- idf
  "Probabilistic IDF. A term in every document is worth ~0, which is the whole
   reason this needs no stoplist."
  ^double [^long n ^long df]
  (Math/log (+ 1.0 (/ (+ (- (double n) (double df)) 0.5) (+ (double df) 0.5)))))

(defn- posting
  "One term's postings as flat arrays: the document ids that carry it and, for
   each, its term frequency in every field slot. Arrays instead of maps because
   this is the only structure the scoring loop reads."
  [^HashMap per-doc ^long n]
  (let
    [k
     (.size per-doc)

     ids
     (int-array k)

     tfs
     (double-array (* k (long field-count)))]

    (loop
      [it
       (.iterator (.entrySet per-doc))

       i
       0]

      (when (.hasNext it)
        (let
          [^Map$Entry e
           (.next it)

           ^doubles a
           (.getValue e)]

          (aset ids i (int (.getKey e)))
          (System/arraycopy a 0 tfs (* i (long field-count)) (long field-count))
          (recur it (inc i)))))
    {:ids ids :tfs tfs :idf (idf n k)}))

(defn index
  "Everything BM25F needs about `docs`, computed once: per-term postings, the
   handle of every document, and the precomputed `k1 * norm` denominator term
   for every document and field."
  [docs]
  (let
    [docs
     (vec docs)

     nd
     (count docs)

     acc
     (HashMap.)

     lens
     (double-array (* (max 1 nd) (long field-count)))

     handles
     (HashMap.)]

    (dotimes [i nd]
      (let [d (nth docs i)]
        (.putIfAbsent handles (normalized-handle (:name d)) (int i))
        (dotimes [f (long field-count)]
          (let [ts (tokens (get d (nth field-keys f)))]
            (aset lens (+ (* i (long field-count)) f) (double (count ts)))
            (doseq [t ts]
              (let
                [^HashMap per (or (.get acc t)
                                  (let [m (HashMap.)]
                                    (.put acc t m)
                                    m))
                 ^doubles tf (or (.get per (int i))
                                 (let [a (double-array (long field-count))]
                                   (.put per (int i) a)
                                   a))]

                (aset tf f (+ 1.0 (aget tf f)))))))))
    (let [kn (double-array (* (max 1 nd) (long field-count)))]
      (dotimes [f (long field-count)]
        (let
          [total (loop
                   [i 0
                    s 0.0]

                   (if (< i nd) (recur (inc i) (+ s (aget lens (+ (* i (long field-count)) f)))) s))
           avg (max 1.0e-9 (/ total (double (max 1 nd))))
           b (aget ^doubles field-b f)]

          (dotimes [i nd]
            (aset kn
                  (+ (* i (long field-count)) f)
                  (* k1 (+ (- 1.0 b) (* b (/ (aget lens (+ (* i (long field-count)) f)) avg))))))))
      {:docs docs
       :n nd
       :kn kn
       :handles (into {} handles)
       :vocab-by-len (persistent! (reduce (fn [m ^String t]
                                            (let [k (.length t)]
                                              (assoc! m k (conj (get m k []) t))))
                                          (transient {})
                                          (keys acc)))
       :postings (persistent! (reduce (fn [m ^Map$Entry e]
                                        (assoc! m (.getKey e) (posting (.getValue e) nd)))
                                      (transient {})
                                      (.entrySet acc)))})))

(def ^:private ^:const cache-capacity
  "How many distinct document sets keep a live index. `apropos` sees one corpus
   that changes only when the sandbox namespace does, so this is generous; the
   cache is cleared wholesale rather than evicted, because a stale index is
   worth nothing and rebuilding costs milliseconds."
  8)

(defonce ^:private index-cache
  ;; `apropos` rebuilds an EQUAL corpus on every call, so identity would never
  ;; hit: the key must be a VALUE. It is a FINGERPRINT rather than the document
  ;; vector itself, because the rebuilt corpus carries fresh String objects and
  ;; a full `.equals` then walks every byte of every document — measured at
  ;; 430 us against 8 us to fingerprint, for exactly the same answer.
  (ConcurrentHashMap.))

(defn- fingerprint
  "The cache key for a document set: its size and its hash. Two distinct
   corpora must collide in BOTH to serve a stale index, and the corpus is
   derived deterministically from the registered sources, so the risk is a
   ranking built one registration behind — never wrong data."
  [docs]
  [(count docs) (hash docs)])

(defn cached-index
  "The index for `docs`, built at most once per distinct document set.
   `computeIfAbsent` is atomic, so concurrent first callers build once and the
   losers block rather than duplicating the work; every later caller shares one
   immutable index and may rank against it in parallel."
  [docs]
  (let [ds (vec docs)]
    (when (>= (.size ^ConcurrentHashMap index-cache) (long cache-capacity))
      (.clear ^ConcurrentHashMap index-cache))
    (.computeIfAbsent ^ConcurrentHashMap index-cache
                      (fingerprint ds)
                      (reify
                        java.util.function.Function
                          (apply [_ _] (index ds))))))

;; =============================================================================
;; Query
;; =============================================================================

(defn- edit-distance
  "Damerau-Levenshtein (optimal string alignment) — a transposition costs ONE,
   because `pathc` and `aprpos` are how a name is actually mistyped. Two bail-
   outs keep it off the hot path: a length gap wider than `budget` is refused
   before any work, and a row whose every cell already exceeds `budget` ends
   the walk."
  ^long [^String a ^String b ^long budget]
  (let
    [m
     (.length a)

     n
     (.length b)

     over
     (inc budget)]

    (if (> (Math/abs (- m n)) budget)
      over
      (loop
        [i
         1

         pprev
         nil

         prev
         (long-array (map long (range (inc n))))]

        (if (> i m)
          (aget ^longs prev n)
          (let
            [^longs pv
             prev

             ^longs row
             (long-array (inc n))

             _
             (aset row 0 (long i))

             best
             (loop
               [j
                1

                best
                (long i)]

               (if (> j n)
                 best
                 (let
                   [cost
                    (if (= (.charAt a (dec i)) (.charAt b (dec j))) 0 1)

                    v
                    (min (inc (aget row (dec j))) (inc (aget pv j)) (+ (aget pv (dec j)) cost))

                    v
                    (if (and pprev
                             (> i 1)
                             (> j 1)
                             (= (.charAt a (dec i)) (.charAt b (- j 2)))
                             (= (.charAt a (- i 2)) (.charAt b (dec j))))
                      (min v (inc (aget ^longs pprev (- j 2))))
                      v)]

                   (aset row j (long v))
                   (recur (inc j) (min best (long v))))))]

            (if (> (long best) budget) over (recur (inc i) prev row))))))))

(defn- resolve-term
  "A term NO document contains is a probable typo: answer the closest term in
   the vocabulary within one edit per three characters, or `nil` to drop it.
   Short terms are never rescued — every one-edit neighbour of a four-letter
   word is another real word.

   Only candidates whose LENGTH is within the budget are examined (`vocab-by-len`
   buckets them at index time) and the walk stops at the first distance-1 hit,
   because nothing non-identical can beat it. That is what keeps a typo off the
   hot path: a 20 000-term vocabulary costs three buckets, not a full scan."
  [{:keys [postings vocab-by-len]} ^String term]
  (cond (contains? postings term) term
        (< (.length term) 4) nil
        :else
        (let
          [len
           (.length term)

           budget
           (if (>= len 6) 2 1)

           cands
           (into [] (mapcat #(get vocab-by-len % nil)) (range (- len budget) (+ len budget 1)))]

          (loop
            [i
             0

             best
             nil

             bd
             (inc (long budget))]

            (if (or (= i (count cands)) (= bd 1))
              (when (<= (long bd) (long budget)) best)
              (let
                [cand
                 (nth cands i)

                 d
                 (edit-distance term cand bd)]

                (if (< d (long bd)) (recur (inc i) cand d) (recur (inc i) best bd))))))))

(defn- accumulate!
  "Add one term's contribution to every document that carries it. Walks the
   term's postings, so an unrelated document is never touched."
  [^doubles scores ^doubles kn posting]
  (let
    [^ints ids
     (:ids posting)

     ^doubles tfs
     (:tfs posting)

     w
     (double (:idf posting))]

    (dotimes [p (alength ids)]
      (let
        [id (aget ids p)
         base (* p (long field-count))
         dbase (* id (long field-count))]

        (loop
          [f 0
           acc 0.0]

          (if (= f (long field-count))
            (aset scores id (+ (aget scores id) (* w acc)))
            (let [tf (aget tfs (+ base f))]
              (recur (inc f)
                     (if (zero? tf)
                       acc
                       (+ acc
                          (* (aget ^doubles field-weight f)
                             (/ (* tf (+ (double k1) 1.0)) (+ tf (aget kn (+ dbase f))))))))))))))
  scores)

(defn rank
  "Rank `ix`'s documents against `query`, best first. Answers the documents'
   `:value` payloads with a `:score`, ties broken on `:name` so the order is
   stable. A blank query is not a failure, it is \"everything\": every document
   in name order, scored 0."
  [ix query]
  (let
    [raw
     (tokens query)

     docs
     (:docs ix)]

    (if (empty? raw)
      (vec (sort-by :name
                    (map (fn [d]
                           (assoc (:value d) :score 0.0))
                         docs)))
      (let
        [scores
         (double-array (max 1 (count docs)))

         ^doubles kn
         (:kn ix)

         postings
         (:postings ix)

         hit
         (into #{} (keep #(resolve-term ix %)) raw)]

        (doseq [t hit]
          (when-let [p (get postings t)]
            (accumulate! scores kn p)))
        (when-let [i (get (:handles ix) (str/join "_" raw))]
          (aset scores (int i) (+ (aget scores (int i)) (double exact-name-bonus))))
        (->> (range (count docs))
             (keep (fn [i]
                     (let [s (aget scores (int i))]
                       (when (pos? s) (assoc (:value (nth docs i)) :score s)))))
             (sort-by (juxt (comp - :score) :name))
             vec)))))

(defn search
  "Index `docs` (memoized) and rank them against `query`."
  [docs query]
  (rank (cached-index docs) query))
