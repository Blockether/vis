(ns com.blockether.vis.internal.bm25
  "BM25F over small in-memory document sets — the one ranker behind `apropos`.

   A document is `{:name … :gist … :body … :value …}`: three text fields and an
   opaque payload the caller gets back on a hit. Nothing here knows what a
   corpus, a skill or a Python callable is, so a second consumer (session
   search, the TUI picker) needs no new engine.

   Four properties the callers depend on:

   - **ORed, not ANDed.** A term no document carries costs nothing instead of
     emptying the result, so a six-word natural-language ask answers the
     document that covers most of it.
   - **IDF, never a stoplist.** `how`, `do` and `a` sit in nearly every
     document, so they price themselves at ~0 and no word list can go stale.
   - **One saturation, not one per field.** A field's weight goes INSIDE the
     term's saturation, so an opening line adds bounded evidence instead of
     outscoring the body. Scoring the three fields separately and summing let ONE
     line hold 61% of a document's score: rewriting that line alone dropped
     `patch` from rank 1 to rank 19 for `how do I replace lines in a file`, while
     the body still said everything it had said.
   - **Length-normalized fields (body `b` 1.0).** Without it a 70 KB skill
     outranks a 350 B contract by containing every word.
   - **A covered handle wins.** A document whose whole handle appears in the
     query takes a bonus scaled by how much of the query that handle is, so
     `patch` answers `patch` outright and still leads `patch anchors`.

   Terms are compared after a plural fold and the tokenizer is Unicode-aware,
   so `run tests`/`run test` and an accented word are not three different
   vocabularies. A term NO document carries is completed as a prefix
   (`apro` -> `apropos`) or spell-corrected (`pathc` -> `patch`) — but only
   inside its own first-letter bucket, which is what keeps an off-corpus query
   both cheap and honestly empty.

   Speed and concurrency, because `apropos` rebuilds its corpus on every call:

   - An index is built ONCE per distinct document set and memoized in
     `index-cache`; the entries vector is the key, so the rebuilt-but-equal
     corpus of the next call hits, and the cache evicts the LEAST RECENTLY USED
     index rather than clearing itself.
   - An index is an immutable value over Java arrays and is safe to share
     across threads; `rank` allocates only its own score accumulator, so any
     number of threads may rank against one index at once. `ConcurrentHashMap`
     `computeIfAbsent` makes the miss path single-build, not single-file.
   - Scoring walks the POSTINGS of the query's terms, never the corpus: a
     one-term query touches the handful of documents that carry it, not all
     194. Per-document, per-field length normalization is precomputed at index
     time, so the inner loop is two array reads and a divide.
   - `:limit` selects the top k through a bounded heap instead of sorting every
     document that scored."
  (:require [clojure.string :as str])
  (:import (java.util Comparator HashMap Map$Entry PriorityQueue)
           (java.util.concurrent ConcurrentHashMap)
           (java.util.concurrent.atomic AtomicLong)
           (java.util.function Function)))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Tuning — defaults a second consumer may override per index
;; =============================================================================

(def default-opts
  "Every knob, in one public map, so a consumer retunes by passing `opts` to
   `index`/`search` instead of editing this namespace. The opts are baked INTO
   the index (`:field-b` decides its precomputed norms) and are part of its
   cache key, so two weightings never share one index.

   - `:k1`           saturation of the WEIGHTED pseudo-frequency, not of a raw
                     term count: the field weights are summed INSIDE it, so it
                     lives on their scale and the textbook 1.2 would saturate a
                     single name hit outright. Weights and `k1` scale together —
                     only their ratio ranks.
   - `:field-weights` name / gist / body: what ONE occurrence in each field is
                     worth, per unit of that field's length, before saturation.
   - `:field-b`      length-normalization strength per field. The body is FULLY
                     normalized (1.0) — that is what stops a 70 KB skill from
                     outranking a 350 B contract.
   - `:handle-bonus` full bonus for a document whose handle the query covers
                     completely; scaled down by how little of the query it is."
  {:k1 8.0 :field-weights [12.0 6.0 1.0] :field-b [0.75 0.75 1.0] :handle-bonus 100.0})

(def ^:private ^:const field-count
  "Slots per document: name, gist, body. Every flat array is strided by it."
  3)

(def ^:private field-keys
  "The three scored fields, in slot order: name, first line, whole body."
  [:name :gist :body])

;; =============================================================================
;; Tokens — one vocabulary for a name, a body and a query
;; =============================================================================

(defn tokens
  "Split `s` into comparable words: camelCase and snake_case both break apart
   (`from_anchor` and `fromAnchor` are `from` + `anchor`), every non-letter and
   non-digit is a separator, and the result is lower-case.

   Unicode by `Character/isLetterOrDigit`, not by an ASCII range: an accented
   or non-Latin query used to tokenize to NOTHING and fall entirely into
   spell-correction, which is how a query in another language answered with
   three confident and unrelated documents."
  [s]
  (let
    [^String s
     (str s)

     n
     (.length s)

     sb
     (StringBuilder.)]

    (loop
      [i
       0

       in-word?
       false

       out
       (transient [])]

      (if (= i n)
        (persistent! (if (pos? (.length sb)) (conj! out (.toString sb)) out))
        (let [c (.charAt s i)]
          (cond (not (Character/isLetterOrDigit c))
                (let [out (if (pos? (.length sb)) (conj! out (.toString sb)) out)]
                  (.setLength sb 0)
                  (recur (inc i) false out))
                ;; camelCase boundary: a run of lower-case or digits, then an upper.
                (and in-word? (Character/isUpperCase c))
                (let [out (if (pos? (.length sb)) (conj! out (.toString sb)) out)]
                  (.setLength sb 0)
                  (.append sb (Character/toLowerCase c))
                  (recur (inc i) false out))
                :else
                (do (.append sb (Character/toLowerCase c))
                    (recur (inc i) (or (Character/isLowerCase c) (Character/isDigit c)) out))))))))

(def ^:private ^:const min-stem-len
  "The shortest stem a suffix rule may leave. A rule that would cut below this
   is dropped instead, which is what keeps `use`, `one` and `read` whole."
  3)

(defn- has-vowel?
  "Does `s`, up to `end`, carry a vowel? Porter's condition that what is left of
   a word is a WORD: it is what keeps `string` off `str` and `bring` off `br`."
  [^String s ^long end]
  (loop [i 0]
    (cond (>= i end) false
          (case (.charAt s i)
            (\a \e \i \o \u \y)
            true

            false)
          true
          :else (recur (inc i)))))

(defn- undoubled
  "`s` minus a doubled final consonant — `runn` → `run`, `stopp` → `stop`, the
   letter `-ing`/`-ed` doubled on the way in. Only the consonants English
   actually doubles, and never below `min-stem-len`."
  ^String [^String s]
  (let
    [n
     (.length s)

     c
     (.charAt s (dec n))]

    (if (and (> n (long min-stem-len))
             (= c (.charAt s (- n 2)))
             (case c
               (\b \d \f \g \m \n \p \r \t)
               true

               false))
      (subs s 0 (dec n))
      s)))

(defn- plural-fold
  "The plural half of Porter step 1a: `tests` → `test`, `entries` → `entry`."
  ^String [^String t]
  (let [n (.length t)]
    (cond (< n 4) t
          (and (>= n 5) (.endsWith t "ies")) (str (subs t 0 (- n 3)) "y")
          (.endsWith t "sses") (subs t 0 (- n 2))
          (and (>= n 5) (.endsWith t "es") (contains? #{\s \x \z \h} (.charAt t (- n 3))))
          (subs t 0 (- n 2))
          (and (.endsWith t "s")
               (not (.endsWith t "ss"))
               (not (.endsWith t "us"))
               (not (.endsWith t "is")))
          (subs t 0 (dec n))
          :else t)))

(defn stem
  "Fold an English word onto ONE term, so the tense a question is asked in never
   decides whether it is answered: `run tests`, `running the test` and `the test
   run` are one ask, and `define`, `defines` and `defined` are one word.

   Porter step 1a's plural rules, then `-ing`/`-ed`, then a final `e` — each
   guarded so a technical vocabulary survives: a rule never cuts below
   `min-stem-len`, never leaves a vowel-less stump (`string` stays `string`,
   `bring` stays `bring`), and the letter a suffix doubled comes back off
   (`running` → `run`). Every rule is applied to the INDEX and the QUERY alike,
   so the worst case is two words sharing one term.

   Measured on the live corpus: asks phrased as gerunds or in the past tense
   (`formatting the source code`, `listing my past sessions`) answered their
   tool 4 times in 14 with the plural rules alone, and 13 in 14 with these —
   with no change to the 51-ask battery the plural rules already answered."
  ^String [^String t]
  (let
    [folded
     (plural-fold t)

     n
     (.length folded)

     cut
     (cond
       (and (.endsWith folded "ing") (>= (- n 3) (long min-stem-len)) (has-vowel? folded (- n 3)))
       (undoubled (subs folded 0 (- n 3)))
       (and (.endsWith folded "ed") (>= (- n 2) (long min-stem-len)) (has-vowel? folded (- n 2)))
       (undoubled (subs folded 0 (- n 2)))
       ;; `used`, `named`: the `e` is the stem's own, so only the `d` goes.
       (and (.endsWith folded "ed") (>= (- n 1) (long min-stem-len)) (has-vowel? folded (- n 1)))
       (subs folded 0 (dec n))
       :else folded)]

    ;; The final `e` goes LAST and from every word, which is the rule that makes
    ;; `define`, `defines` and `defined` the one term `defin`.
    (if (and (.endsWith cut "e") (> (.length cut) (long min-stem-len)))
      (subs cut 0 (dec (.length cut)))
      cut)))

(defn terms
  "`s` as the terms the index and the query both speak: tokenized, then folded."
  [s]
  (into [] (map stem) (tokens s)))

(defn normalized-handle
  "A name or a whole query as ONE comparable string, so `from_anchor`,
   `fromAnchor` and `from anchor` all name the same handle. UNFOLDED — a handle
   is an identifier, not a word."
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
   folded handle of every document, the vocabulary bucketed by first letter and
   the precomputed length norm `B` of every document and field.
   `opts` overrides `default-opts` and is carried on the index itself."
  ([docs] (index docs nil))
  ([docs opts]
   (let
     [o
      (merge default-opts opts)

      ^doubles weights
      (double-array (:field-weights o))

      ^doubles bs
      (double-array (:field-b o))

      k1
      (double (:k1 o))

      docs
      (vec docs)

      nd
      (count docs)

      acc
      (HashMap.)

      lens
      (double-array (* (max 1 nd) (long field-count)))

      handle-toks
      (object-array (max 1 nd))

      handle-ids
      (HashMap.)]

     (dotimes [i nd]
       (let [d (nth docs i)]
         (let [ht (terms (:name d))]
           (aset handle-toks i ht)
           (doseq [t ht]
             (.put handle-ids t (conj (or (.get handle-ids t) []) (int i)))))
         (dotimes [f (long field-count)]
           (let [ts (terms (get d (nth field-keys f)))]
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
     (let [bn (double-array (* (max 1 nd) (long field-count)))]
       (dotimes [f (long field-count)]
         (let
           [total
            (loop
              [i 0
               s 0.0]

              (if (< i nd) (recur (inc i) (+ s (aget lens (+ (* i (long field-count)) f)))) s))
            avg (max 1.0e-9 (/ total (double (max 1 nd))))
            b (aget bs f)]

           (dotimes [i nd]
             (aset bn
                   (+ (* i (long field-count)) f)
                   (+ (- 1.0 b) (* b (/ (aget lens (+ (* i (long field-count)) f)) avg)))))))
       {:docs docs
        :n nd
        :bn bn
        :k1 k1
        :weights weights
        :handle-bonus (double (:handle-bonus o))
        :handle-toks handle-toks
        ;; term -> the documents whose HANDLE uses it, so the covered-handle bonus
        ;; visits a candidate or two instead of every document in the corpus.
        :handle-ids (into {} handle-ids)
        :opts o
        ;; Bucketed by FIRST LETTER, unsorted: prefix completion and spell
        ;; correction both scan one bucket, and a term whose first letter no
        ;; document uses costs a map miss instead of a vocabulary walk.
        :vocab-by-head (persistent! (reduce (fn [m ^String t]
                                              (let [c (.charAt t 0)]
                                                (assoc! m c (conj (get m c []) t))))
                                            (transient {})
                                            (keys acc)))
        :postings (persistent! (reduce (fn [m ^Map$Entry e]
                                         (assoc! m (.getKey e) (posting (.getValue e) nd)))
                                       (transient {})
                                       (.entrySet acc)))}))))

;; =============================================================================
;; Index cache — one build per document set, least-recently-used eviction
;; =============================================================================

(def ^:private ^:const cache-capacity
  "How many distinct document sets keep a live index. `apropos` sees one corpus
   that changes only when the sandbox namespace does, so this is generous."
  8)

(defonce ^:private index-cache
  ;; `apropos` rebuilds an EQUAL corpus on every call, so identity would never
  ;; hit: the key must be a VALUE. It is a FINGERPRINT rather than the document
  ;; vector itself, because the rebuilt corpus carries fresh String objects and
  ;; a full `.equals` then walks every byte of every document — measured at
  ;; 430 us against 8 us to fingerprint, for exactly the same answer.
  (ConcurrentHashMap.))

(defonce ^:private cache-clock
  ;; Monotonic use counter. An entry carries the tick of its last read, so the
  ;; victim is the LEAST RECENTLY USED one — a second corpus shape no longer
  ;; throws away the warm index of the first.
  (AtomicLong. 0))

(defn- fingerprint
  "The cache key for a document set: its size and its hash. Two distinct
   corpora must collide in BOTH to serve a stale index, and the corpus is
   derived deterministically from the registered sources, so the risk is a
   ranking built one registration behind — never wrong data."
  [docs opts]
  [(count docs) (hash docs) opts])

(defn- evict-lru!
  "Drop least-recently-used entries until the cache fits. Removal is
   key+value-conditional, so a concurrent rebuild of the same key is never
   thrown away."
  []
  (while (> (.size ^ConcurrentHashMap index-cache) (long cache-capacity))
    (let
      [victim (reduce (fn [worst ^Map$Entry e]
                        (if (or (nil? worst)
                                (< (.get ^AtomicLong (:used (.getValue e)))
                                   (.get ^AtomicLong (:used (second worst)))))
                          [(.getKey e) (.getValue e)]
                          worst))
                      nil
                      (.entrySet ^ConcurrentHashMap index-cache))]
      (if victim
        (.remove ^ConcurrentHashMap index-cache (first victim) (second victim))
        ;; Emptied underneath us — nothing left to evict.
        (.clear ^ConcurrentHashMap index-cache)))))

(defn cached-index
  "The index for `docs` under `opts`, built at most once per distinct document
   set. `computeIfAbsent` is atomic, so concurrent first callers build once and
   the losers block rather than duplicating the work; every later caller shares
   one immutable index and may rank against it in parallel."
  ([docs] (cached-index docs nil))
  ([docs opts]
   (let
     [ds
      (vec docs)

      o
      (merge default-opts opts)

      entry
      (.computeIfAbsent ^ConcurrentHashMap index-cache
                        (fingerprint ds o)
                        (reify
                          Function
                            (apply [_ _]
                              {:ix (index ds o)
                               :used (AtomicLong. (.incrementAndGet ^AtomicLong cache-clock))})))]

     (.set ^AtomicLong (:used entry) (.incrementAndGet ^AtomicLong cache-clock))
     (evict-lru!)
     (:ix entry))))

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

(defn- complete-prefix
  "The shortest vocabulary term `term` is a prefix of, or nil. This is the
   interactive ask: `apro` means `apropos`, and no edit distance can reach a
   name three characters longer."
  [bucket ^String term]
  (when (>= (.length term) 3)
    (reduce (fn [best ^String cand]
              (if (and (> (.length cand) (.length term))
                       (.startsWith cand term)
                       (or (nil? best)
                           (let [^String b best]
                             (or (< (.length cand) (.length b))
                                 (and (= (.length cand) (.length b)) (neg? (.compareTo cand b)))))))
                cand
                best))
            nil
            bucket)))

(defn- nearest
  "The closest vocabulary term within one edit per three characters, or nil.
   Short terms are never rescued — every one-edit neighbour of a four-letter
   word is another real word.

   A tie on distance is broken by LENGTH: a typo is usually a substitution or a
   transposition, so `pathc` is `patch` and not the equally-close `path`. The
   walk stops only once it holds a same-length distance-1 hit, which nothing
   can beat."
  [bucket ^String term]
  (when (>= (.length term) 4)
    (let
      [len
       (.length term)

       budget
       (if (>= len 7) 2 1)]

      (loop
        [ts
         (seq bucket)

         best
         nil

         bd
         (inc (long budget))

         bdl
         Long/MAX_VALUE]

        (if (or (nil? ts) (and (= bd 1) (= bdl 0)))
          (when (<= (long bd) (long budget)) best)
          (let
            [^String cand
             (first ts)

             dl
             (Math/abs (- (.length cand) len))

             ;; Cheaper than entering the DP: a candidate further away in
             ;; length than the budget cannot be within it.
             d
             (if (> dl (long budget)) (inc (long budget)) (edit-distance term cand bd))]

            (if (or (< d (long bd)) (and (= d (long bd)) (< dl (long bdl))))
              (recur (next ts) cand d dl)
              (recur (next ts) best bd bdl))))))))

(defn- resolve-term
  "A term NO document contains is a partial handle or a typo: complete it as a
   prefix, else spell-correct it, else drop it.

   Both only ever look inside the term's OWN first-letter bucket. That is what
   makes an off-corpus query honest AND cheap: a word starting with a letter no
   document uses costs one map miss and contributes nothing, instead of walking
   the vocabulary to hand back a confident, unrelated document."
  [{:keys [postings vocab-by-head]} ^String term]
  (if (contains? postings term)
    term
    (when (>= (.length term) 3)
      (when-let [bucket (get vocab-by-head (.charAt term 0))]
        (or (complete-prefix bucket term) (nearest bucket term))))))

(defn- resolve-query
  "Every query term the corpus can answer, in query order, deduplicated by what
   it RESOLVED to and carrying that resolution: `{:term \"pathc\" :as \"patch\"
   :idf 3.4}`. A reader renders the correction (`pathc` -> `patch`) and orders
   an excerpt by `:idf`, so a hit can show WHY it came back instead of only how
   high it scored."
  [{:keys [postings] :as ix} raw]
  (loop
    [ts
     (seq raw)

     seen
     #{}

     out
     []]

    (if-not ts
      out
      (let
        [t
         (first ts)

         r
         (resolve-term ix t)]

        (if (or (nil? r) (contains? seen r))
          (recur (next ts) seen out)
          (recur (next ts)
                 (conj seen r)
                 (conj out {:term t :as r :idf (double (:idf (get postings r) 0.0))})))))))
(defn- accumulate!
  "Add one term's contribution to every document that carries it — BM25F proper:
   the term's occurrences in the three fields become ONE weighted, length-normalized
   pseudo-frequency, and the saturation applies to that sum, ONCE.

   Saturating each field on its own and summing the three is the wrong way round:
   every field then saturates at its own weight, so ONE occurrence in a one-line
   field outweighs any amount of body evidence and rewriting that line alone costs
   a document most of its score. Walks the term's postings, so an unrelated
   document is never touched."
  [^doubles scores ^doubles bn ^doubles weights k1 posting]
  (let
    [k1
     (double k1)

     ^ints ids
     (:ids posting)

     ^doubles tfs
     (:tfs posting)

     w
     (double (:idf posting))]

    (dotimes [p (alength ids)]
      (let
        [id (aget ids p)
         base (* p (long field-count))
         dbase (* id (long field-count))
         ;; A field the term misses adds nothing, so the divisor is never zero:
         ;; a field with an occurrence has a length.
         ptf (loop
               [f 0
                acc 0.0]

               (if (= f (long field-count))
                 acc
                 (let [tf (aget tfs (+ base f))]
                   (recur (inc f)
                          (if (zero? tf)
                            acc
                            (+ acc (/ (* (aget weights f) tf) (aget bn (+ dbase f)))))))))]

        (aset scores id (+ (aget scores id) (* w (/ (* ptf (+ k1 1.0)) (+ k1 ptf))))))))
  scores)

(defn- add-handle-bonus!
  "A document whose whole handle is covered by the query takes the bonus,
   scaled by how much of the query that handle accounts for. `patch` for the
   query `patch` takes all of it; `patch` for `patch anchors` takes half; a
   handle the query only partly names takes none — which is why one extra word
   is no longer a ranking cliff.

   The terms are the RESOLVED ones, so `strcut_patch` - one transposition off
   a real handle - still covers `struct_patch` outright. Only the documents
   whose handle uses one of the query's terms are visited (`:handle-ids`), so
   this stays flat as a corpus grows."
  [^doubles scores ^objects handle-toks handle-ids bonus query-terms]
  (let
    [bonus
     (double bonus)

     qset
     (set query-terms)

     qn
     (count qset)]

    (when (pos? qn)
      (doseq [i (into #{} (mapcat #(get handle-ids %)) qset)]
        (let [ht (aget handle-toks (int i))]
          (when (and (seq ht) (every? qset ht))
            (aset scores
                  (int i)
                  (+ (aget scores (int i)) (* bonus (/ (double (count ht)) (double qn)))))))))))

(defn- top-k
  "The `k` best of `hits` by (score desc, name asc) through a bounded heap: a
   250-document corpus is not sorted to answer five rows."
  [hits ^long k]
  (let
    [worst-first
     (reify
       Comparator
         (compare [_ a b]
           (let [d (compare (:score a) (:score b))]
             (if (zero? d) (compare (:name b) (:name a)) d))))

     pq
     (PriorityQueue. (max 1 (int k)) worst-first)]

    (doseq [h hits]
      (.offer pq h)
      (when (> (.size pq) k) (.poll pq)))
    (vec (sort-by (juxt (comp - :score) :name) (vec pq)))))

(defn rank
  "Rank `ix`'s documents against `query`, best first. Answers the documents'
   `:value` payloads with a `:score`, ties broken on `:name` so the order is
   stable. `:limit` keeps only the top k. A blank query is not a failure, it is
   \"everything\": every document in name order, scored 0.

   The answer carries the RESOLVED query terms as metadata
   (`{:terms [{:term :as :idf}]}`), because prefix completion and spell
   correction happen in here: a reader that wants to show WHY a document
   came back cannot re-derive them from the query string."
  ([ix query] (rank ix query nil))
  ([ix query {:keys [limit]}]
   (let
     [raw
      (terms query)

      docs
      (:docs ix)

      limit
      (when limit (long limit))]

     (if (empty? raw)
       (with-meta (cond->>
                    (sort-by :name
                             (map (fn [d]
                                    (assoc (:value d) :score 0.0))
                                  docs))
                    limit
                    (take limit)

                    :always
                    vec)
         {:terms []})
       (let
         [nd
          (count docs)

          scores
          (double-array (max 1 nd))

          ^doubles bn
          (:bn ix)

          ^doubles weights
          (:weights ix)

          k1
          (double (:k1 ix))

          postings
          (:postings ix)

          resolved
          (resolve-query ix raw)

          hit
          (into #{} (map :as) resolved)]

         (doseq [t hit]
           (when-let [p (get postings t)]
             (accumulate! scores bn weights k1 p)))
         (add-handle-bonus! scores (:handle-toks ix) (:handle-ids ix) (:handle-bonus ix) hit)
         (let
           [hits (into []
                       (keep (fn [i]
                               (let [s (aget scores (int i))]
                                 (when (pos? s) (assoc (:value (nth docs i)) :score s)))))
                       (range nd))]
           (with-meta (if (and limit (> (count hits) (long limit)))
                        (top-k hits limit)
                        (vec (sort-by (juxt (comp - :score) :name) hits)))
             {:terms resolved})))))))

(defn search
  "Index `docs` (memoized) and rank them against `query`. `opts` retunes the
   scoring (see `default-opts`) and carries `:limit`. `rank`'s resolved-term
   metadata rides along."
  ([docs query] (search docs query nil))
  ([docs query opts] (rank (cached-index docs (dissoc opts :limit)) query opts)))
