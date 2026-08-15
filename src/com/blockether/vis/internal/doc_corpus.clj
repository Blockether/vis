(ns com.blockether.vis.internal.doc-corpus
  "The ONE corpus behind the two discovery verbs.

   The sandbox answers exactly two questions about itself, and this namespace
   holds the documents both of them read:

     - `apropos(query)` SEARCHES — ranked full text over every document the
       session can reach.
     - `doc(target)`    RETRIEVES — the one authoritative text for a name.

   ONE RECORD for every document, whatever seeded it:

     {:name \"grep\"          ;; the only handle
      :text \"...\"           ;; the whole document; its FIRST LINE is the gist
      :call \"grep({\"query\": …})\"} ;; the python that USES it; absent = prose

   There is no `kind` and no `group`. A taxonomy is not actionable: what a hit
   actually needs is the expression that uses it, so `call` is the field and it
   spells the vocabulary out — a function answers `grep({\"query\": …})`, an MCP tool
   answers `mcp__call(\"server\", \"tool\", {…})`, and a skill or a documentation
   page answers NOTHING, because a missing `call` is exactly \"this is prose,
   read it\".

   There is no stored `gist` either. Two texts for one entry are two places to
   drift, invisibly, because nothing reads both at once: `gist` is a RENDERING
   of the first line, so the index prints first lines and `doc` prints the whole
   thing from the SAME string.

   Sources are REGISTERED, never required in: documentation pages and skills
   are seeded here because both are leaves; the MCP catalogue registers itself
   from `foundation.mcp.core`, which sits above this namespace. A fifth source
   is a new seeder, not a new verb.

   Function contracts (sandbox verbs, shims, language tools) are NOT seeded
   here — they come off the live extension registry inside `env-python`, which
   merges them over these entries so a callable name always wins a collision."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.internal.docs :as docs]
            [com.blockether.vis.internal.foundation.harness.discovery :as discovery]))

(set! *warn-on-reflection* true)

;; =============================================================================
;; Data — what crosses the Clojure -> GraalPy boundary
;; =============================================================================

(s/def :vis.doc/name string?)
(s/def :vis.doc/text string?)
(s/def :vis.doc/call (s/nilable string?))
(s/def :vis.doc/entry (s/keys :req-un [:vis.doc/name :vis.doc/text] :opt-un [:vis.doc/call]))
(s/def :vis.doc/entries (s/coll-of :vis.doc/entry :kind vector?))
(s/def :vis.doc/result
  (s/or :index (s/keys :req-un [:vis.doc/entries])
        :entry :vis.doc/entry))

;; `apropos` = the same records, ranked. Rank IS the "where did it match"
;; answer: a name hit outranks a body hit, so a per-hit field set would only
;; restate the order.
(s/def :vis.apropos/score number?)
(s/def :vis.apropos/hit (s/merge :vis.doc/entry (s/keys :req-un [:vis.apropos/score])))
(s/def :vis.apropos/result (s/coll-of :vis.apropos/hit :kind vector?))

;; =============================================================================
;; Rendering a gist, comparing a name
;; =============================================================================

(def ^:private index-gist-max-len
  "Tighter cap for a curated-index row: twenty rows of 240 characters is a wall,
   and the whole document is one `doc(name)` away."
  140)

(def ^:private gist-max-len
  "Cap on a rendered gist. Long enough to carry the essence of a contract line,
   short enough that an index of sixty entries stays scannable."
  240)

(defn gist
  "The FIRST LINE of `text`, as a one-liner: leading markdown heading marks are
   dropped (a page's first line is its `# Title`) and the result is capped at
   `gist-max-len` — or at `max-len`, which the curated index tightens so twenty
   rows stay scannable. This is the only place a gist exists — never a stored
   field."
  ([text] (gist text gist-max-len))
  ([text max-len]
   (let
     [line (some->> (str text)
                    str/split-lines
                    (remove str/blank?)
                    first
                    str/trim
                    (#(str/replace % #"^#+\s*" ""))
                    str/trim)]
     (cond (str/blank? (str line)) ""
           (> (count line) (long max-len)) (str (subs line 0 (dec (long max-len))) "\u2026")
           :else line))))

(defn normalize-name
  "Coerce a caller's target to a comparable handle: unwrap the map/kwargs shape,
   trim, drop a trailing `.md` (pages cross-link by filename), lower-case. This
   is why `doc(\"Gateway.md\")` and `doc(\"gateway\")` are the same ask."
  [target]
  (-> (if (map? target) (or (get target "name") (get target :name) (get target "slug") "") target)
      str
      str/trim
      (str/replace #"(?i)\.md$" "")
      str/lower-case))

;; =============================================================================
;; Sources — registered, never required in
;; =============================================================================

(defonce ^:private sources
  ;; `[[id f] ...]` in registration order, so precedence is readable rather than
  ;; hash-ordered: the FIRST source to claim a name keeps it.
  (atom []))

(defn register-source!
  "Register `f` — a 0-arity returning a coll of `:vis.doc/entry` — under `id`.
   Re-registering an `id` replaces it IN PLACE, so a reloaded namespace never
   duplicates its own entries."
  [id f]
  (swap! sources (fn [ss]
                   (if (some (comp #{id} first) ss)
                     (mapv (fn [[k v]]
                             (if (= k id) [k f] [k v]))
                           ss)
                     (conj ss [id f]))))
  id)

(defn- documentation-page-entries
  "Every embedded `vis-docs` page as an entry. Re-collected per call (the pages
   are tiny and the live `/docs` site re-reads them too, so a dev edit shows
   without a restart). A page has no `:call` — it is prose."
  []
  (into []
        (keep (fn [{:keys [slug title section blurb md]}]
                (when (and (seq (str slug)) (seq (str md)))
                  {:name (str slug)
                   ;; The TITLE is the first line, unheaded: `entry-text` already
                   ;; prints `# <slug>`, and two headings in a row read as a
                   ;; mistake rather than as a document.
                   :text (str (or (not-empty (str title)) (str slug))
                              (when (seq (str section)) (str " \u00b7 " section))
                              (when (seq (str blurb)) (str "\n\n" blurb))
                              "\n\n"
                              md)})))
        (:pages (docs/collect))))

(defn- skill-entries
  "Every discovered skill as an entry carrying its WHOLE `SKILL.md` body. The
   frontmatter `description` is restored as the document's first line — that is
   the file's own summary, not a second one — and the body follows verbatim, so
   `doc(name)` answers the same string `apropos` searched.

   A skill is PROSE, exactly like a documentation page: it carries no `call`,
   because there is no verb to invoke — reading it IS using it, and this reads
   the discovery cache, so search has no effect on the session."
  []
  (into []
        (keep (fn [{:keys [name description body]}]
                (when (seq (str name))
                  {:name (str name)
                   :text (str (when (seq (str description)) (str description "\n\n")) body)})))
        (discovery/skills)))

(register-source! :documentation-pages #'documentation-page-entries)
(register-source! :skills #'skill-entries)

(defn- dedupe-by-name
  "Transducer keeping the FIRST entry for each name."
  []
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn ([] (rf)) ([acc] (rf acc))
        ([acc e]
         (let [k (normalize-name (:name e))]
           (if (contains? @seen k) acc (do (vswap! seen conj k) (rf acc e)))))))))

(defn entries
  "The whole corpus, in registration order, deduplicated by name (first wins).
   A source that throws contributes nothing — discovery must never be the reason
   an env fails to build."
  []
  (let
    [raw (into []
               (mapcat (fn [[_id f]]
                         (try (f) (catch Throwable _ nil))))
               @sources)]
    (into []
          (comp (filter #(and (seq (str (:name %))) (some? (:text %))))
                (map (fn [e]
                       (cond-> {:name (str (:name e)) :text (str (:text e))}
                         (seq (str (:call e)))
                         (assoc :call (str (:call e))))))
                (dedupe-by-name))
          raw)))

;; =============================================================================
;; Search — BM25F over three fields
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

(def ^:private fields
  "The three fields BM25F scores, with their weight and their length
   normalization. A name hit is worth much more than a body hit, but the body is
   FULLY length-normalized (`b` 1.0): without it a 70 KB skill outranks a 350 B
   tool contract on a natural-language query simply by containing every word."
  [{:key :name :weight 8.0 :b 0.75} {:key :gist :weight 3.0 :b 0.75}
   {:key :body :weight 1.0 :b 1.0}])

(defn- tokens
  "Split `s` into comparable terms: camelCase and snake_case both break apart
   (`from_anchor` and `fromAnchor` are `from` + `anchor`), everything
   non-alphanumeric is a separator, and the result is lower-case. There is NO
   stoplist — `how`, `do` and `a` sit in nearly every document, so IDF prices
   them at ~0 by itself and no hand-maintained word list can go stale."
  [s]
  (into []
        (map str/lower-case)
        (re-seq #"[A-Za-z0-9]+" (str/replace (str s) #"([a-z0-9])([A-Z])" "$1 $2"))))

(defn- normalized-handle
  "A name or a whole query as ONE comparable string, so `from_anchor`,
   `fromAnchor` and `from anchor` all name the same handle."
  [s]
  (str/join "_" (tokens s)))

(defn- edit-distance
  "Damerau-Levenshtein (optimal string alignment) — a transposition costs ONE,
   because `pathc` and `aprpos` are how a name is actually mistyped. Used only to
   rescue a term NO document contains, so it never runs on the hot path of a
   query that already matched."
  ^long [^String a ^String b]
  (let
    [m
     (count a)

     n
     (count b)]

    (loop
      [i
       1

       pprev
       nil

       prev
       (vec (range (inc n)))]

      (if (> i m)
        (long (peek prev))
        (let
          [row (loop
                 [j 1
                  row [i]]

                 (if (> j n)
                   row
                   (let
                     [cost (if (= (.charAt a (dec i)) (.charAt b (dec j))) 0 1)
                      v (min (inc (long (nth row (dec j))))
                             (inc (long (nth prev j)))
                             (+ (long (nth prev (dec j))) cost))
                      v (if (and pprev
                                 (> i 1)
                                 (> j 1)
                                 (= (.charAt a (dec i)) (.charAt b (- j 2)))
                                 (= (.charAt a (- i 2)) (.charAt b (dec j))))
                          (min v (inc (long (nth pprev (- j 2)))))
                          v)]

                     (recur (inc j) (conj row v)))))]
          (recur (inc i) prev row))))))

(defn- doc-fields
  "The three token streams of one entry. The body is the WHOLE text — the gist
   line is deliberately counted twice, once cheaply and once at its own weight."
  [{:keys [name text]}]
  (let [t (str text)]
    {:name (tokens name) :gist (tokens (gist t)) :body (tokens t)}))

(defn- index
  "Everything BM25F needs about `es`, built once per query: per-field term
   frequencies and lengths, the document frequency of every term, and the mean
   length of every field."
  [es]
  (let
    [docs
     (into []
           (map (fn [e]
                  (let [f (doc-fields e)]
                    {:entry e
                     :handle (normalized-handle (:name e))
                     :tf (update-vals f frequencies)
                     :len (update-vals f count)
                     :terms (set (mapcat val f))})))
           es)

     n
     (max 1 (count docs))]

    {:docs docs
     :n n
     :df (frequencies (mapcat :terms docs))
     :avg (into {}
                (map (fn [{:keys [key]}]
                       [key (/ (double (reduce + (map #(get-in % [:len key] 0) docs))) n)]))
                fields)}))

(defn- idf
  "Probabilistic IDF. A term in every document is worth ~0, which is the whole
   reason this needs no stoplist."
  ^double [^long n ^long df]
  (Math/log (+ 1.0 (/ (+ (- (double n) (double df)) 0.5) (+ (double df) 0.5)))))

(defn- resolve-term
  "A term NO document contains is a probable typo: answer the closest term in the
   vocabulary within one edit per three characters, or `nil` to drop it. Short
   terms are never rescued — every one-edit neighbour of a four-letter word is
   another real word."
  [{:keys [df]} term]
  (cond (contains? df term) term
        (< (count term) 4) nil
        :else (let
                [budget
                 (if (>= (count term) 6) 2 1)

                 [best d]
                 (reduce (fn [[bt bd] cand]
                           (let [d (edit-distance term cand)]
                             (if (< d (long bd)) [cand d] [bt bd])))
                         [nil (inc budget)]
                         (keys df))]

                (when (<= (long d) budget) best))))

(defn- bm25f
  "One document's score for `terms`: the weighted, length-normalized sum over the
   three fields. OR by construction — a term that missed contributes nothing
   instead of discarding the document."
  ^double [{:keys [n df avg]} doc terms]
  (reduce (fn [^double acc term]
            (let [d (long (get df term 0))]
              (if (zero? d)
                acc
                (+ acc
                   (* (idf n d)
                      (double (reduce (fn [^double a {:keys [key ^double weight ^double b]}]
                                        (let [tf (double (get-in doc [:tf key term] 0))]
                                          (if (zero? tf)
                                            a
                                            (let
                                              [len (double (get-in doc [:len key] 0))
                                               av (double (max 1.0e-9 (double (get avg key 1.0))))
                                               norm (+ (- 1.0 b) (* b (/ len av)))
                                               sat (double k1)]

                                              (+ a
                                                 (* weight
                                                    (/ (* tf (+ sat 1.0)) (+ tf (* sat norm)))))))))
                                      0.0
                                      fields)))))))
          0.0
          terms))

(defn search
  "Rank `es` against `query` with BM25F over three fields — name, first line,
   whole body. Terms are ORed and priced by IDF, so a query is a description
   rather than a conjunction: a six-word natural-language ask answers the
   document that covers most of it, and a word no document carries costs
   nothing instead of emptying the result. A query that IS a handle wins that
   handle outright, and a term nothing contains is spell-corrected against the
   vocabulary before it is dropped. Ties break on name, so the order is stable.

   A blank query is not a failure, it is \"everything\": the whole corpus, in
   name order."
  [es query]
  (let [raw (tokens query)]
    (if (empty? raw)
      (vec (sort-by :name (map #(assoc % :score 0.0) es)))
      (let
        [ix (index es)
         terms (into [] (keep #(resolve-term ix %)) raw)
         handle (str/join "_" raw)]

        (->> (:docs ix)
             (keep (fn [{:keys [entry] :as d}]
                     (let
                       [s (+ (bm25f ix d terms)
                             (double (if (= handle (:handle d)) exact-name-bonus 0.0)))]
                       (when (pos? s) (assoc entry :score s)))))
             (sort-by (juxt (comp - :score) :name))
             vec)))))

;; =============================================================================
;; The curated index — `doc()` with no argument
;; =============================================================================

(def curated
  "What `doc()` prints: a hand-ordered short list of the verbs a session starts
   from, NOT a dump. Sixty functions plus every page plus every skill is just
   another prompt; anything off this list stays discoverable through
   `apropos(text)`, which is the whole point of dropping the schemas. A name
   that is not in the corpus is simply skipped, so this vector never has to
   track which extensions are active."
  ["apropos" "doc" "read_session" "fold_session" "ls" "grep" "cat" "patch" "struct_index"
   "struct_nodes" "struct_patch" "shell" "run_tests" "repl_eval" "lint_code" "format_code" "attach"
   "mcp__call"])

;; =============================================================================
;; The three things the two verbs PRINT
;; =============================================================================

(defn entry-text
  "What `doc(target)` answers for one entry: the handle, the expression that
   uses it when there is one, then the WHOLE document. `note` is the caller's
   one-word remark about the handle (`env-python` marks a live callable)."
  ([entry] (entry-text entry nil))
  ([{:keys [name text call]} note]
   (str "# "
        name
        (when (seq (str note)) (str "  \u00b7  " note))
        (when (seq (str call)) (str "\n\n" call))
        "\n\n"
        (str/trim (str text)))))

(defn index-text
  "What bare `doc()` answers: the curated verbs that are actually present, one
   `name \u2014 first line` per row, then the sentence that says where the rest is.
   Never a dump — everything off `curated` is one `apropos(text)` away."
  [es]
  (let
    [by-name
     (into {} (map (juxt :name identity)) es)

     rows
     (into []
           (keep (fn [nm]
                   (when-let [e (get by-name nm)]
                     (let [g (gist (:text e) index-gist-max-len)]
                       (if (str/blank? g) nm (str nm " \u2014 " g))))))
           curated)]

    (str "# doc()\n\n" (str/join "\n" rows)
         "\n\nEverything else \u2014 " (count es)
         " documents in all, including every skill and every Vis documentation page \u2014"
         " is one `apropos(text)` away; `doc(name)` prints any of them whole.")))

(defn miss-text
  "What `doc(target)` answers when nothing carries that handle: the closest hits
   for the SAME string, so a near-miss costs one call instead of two."
  [es target]
  (let [near (take 5 (search es (str target)))]
    (if (seq near)
      (str (pr-str (str target))
           " is not a handle. Closest documents:\n\n"
           (str/join "\n"
                     (map (fn [e]
                            (let [g (gist (:text e))]
                              (str "  " (:name e) (when-not (str/blank? g) (str " \u2014 " g)))))
                          near))
           "\n\nRead one with `doc(name)`.")
      (str (pr-str (str target))
           " is not a handle and nothing in the corpus mentions it. `doc()` lists the"
           " verbs a session starts from; `apropos(text)` searches everything."))))
