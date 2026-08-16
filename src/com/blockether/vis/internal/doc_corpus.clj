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
      :kind \"tool\"          ;; what it IS, out of the closed `kinds` vocabulary
      :call \"grep({\"query\": …})\" ;; the python that USES it; absent = prose
      :params \"Keys: query (REQUIRED) · paths\"} ;; its options-dict vocabulary

   `kind` says what a reader should DO with a hit; `call` spells the vocabulary
   out — a function answers `grep({\"query\": …})`, an MCP tool answers
   `mcp__call(\"server\", \"tool\", {…})`, and a skill or a documentation page
   answers NOTHING, because a missing `call` is exactly \"this is prose,
   read it\". `params` names the keys of a dict-shaped verb and marks the ones a
   caller cannot omit.

   How a verb is CALLED is STRUCTURE, never text: `call` and `params` are printed
   above the document and are not part of it. The first line is what `apropos`
   PREVIEWS and a scored field besides, so a tool whose text opened with its own
   signature previewed as a signature and stopped matching the words its prose is
   written in.

   There is no stored `gist`. Two texts for one entry are two places to
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
            [com.blockether.vis.internal.bm25 :as bm25]
            [com.blockether.vis.internal.docs :as docs]
            [com.blockether.vis.internal.foundation.harness.discovery :as discovery]))

(set! *warn-on-reflection* true)

;; Data — what crosses the Clojure -> GraalPy boundary

(s/def :vis.doc/name string?)
(s/def :vis.doc/text string?)
(s/def :vis.doc/call (s/nilable string?))
(s/def :vis.doc/params (s/nilable string?))
(def kinds
  "The closed vocabulary of `:kind` — what a document IS, which is how a reader
   decides what to DO with it: `tool` a callable verb's contract, `shim` an
   importable sandbox module, `page` a Vis documentation page, `skill` a whole
   `SKILL.md`, `mcp` an MCP server's tool, `local` a callable this session
   defined that carries no contract at all. A source that invents a kind is a
   bug, not a new category."
  #{"tool" "shim" "page" "skill" "mcp" "local"})

(s/def :vis.doc/kind kinds)
(s/def :vis.doc/entry
  (s/keys :req-un [:vis.doc/name :vis.doc/text]
          :opt-un [:vis.doc/call :vis.doc/params :vis.doc/kind]))
(s/def :vis.doc/entries (s/coll-of :vis.doc/entry :kind vector?))
(s/def :vis.doc/result
  (s/or :index (s/keys :req-un [:vis.doc/entries])
        :entry :vis.doc/entry))

;; `apropos` = the same records, ranked. The rank ORDER is one answer and the
;; `preview` of each hit is the other: a row shows where the query landed in
;; that document and how deep, so a reader never opens a 70 KB skill to find
;; out it was matched once in a table.
(s/def :vis.apropos/score number?)
(s/def :vis.apropos/hit (s/merge :vis.doc/entry (s/keys :req-un [:vis.apropos/score])))
(s/def :vis.apropos/result (s/coll-of :vis.apropos/hit :kind vector?))

;; Rendering a gist, comparing a name

(def ^:private index-gist-max-len
  "Tighter cap for a curated-index row: twenty rows of 240 characters is a wall,
   and the whole document is one `doc(name)` away."
  140)

(def ^:private gist-max-len
  "Cap on a rendered gist. Long enough to carry the essence of a contract line,
   short enough that an index of sixty entries stays scannable."
  240)

(defn- first-non-blank-line
  "The first non-blank line of `s`, scanned with `indexOf` rather than by
   splitting: `gist` runs over every document on every `apropos` call and the
   bodies are whole skills, so splitting 70 KB to read its first line was the
   single most expensive thing search did."
  ^String [^String s]
  (loop [from 0]
    (if (>= from (.length s))
      ""
      (let
        [nl (.indexOf s "\n" from)
         end (if (neg? nl) (.length s) nl)
         line (.trim (.substring s from end))]

        (if (and (.isEmpty line) (not (neg? nl))) (recur (inc end)) line)))))

(defn gist
  "The FIRST LINE of `text`, as a one-liner: leading markdown heading marks are
   dropped (a page's first line is its `# Title`) and the result is capped at
   `gist-max-len` — or at `max-len`, which the curated index tightens so twenty
   rows stay scannable. This is the only place a gist exists — never a stored
   field."
  ([text] (gist text gist-max-len))
  ([text max-len]
   (let [line (str/trim (str/replace (first-non-blank-line (str text)) #"^#+\s*" ""))]
     (cond (str/blank? line) ""
           (> (count line) (long max-len)) (str (subs line 0 (dec (long max-len))) "…")
           :else line))))


(def ^:private ^:const preview-head-len
  "Characters of the document's own opening. Long enough for a contract's first
   clause, short enough that ten rows stay scannable."
  110)

(def ^:private ^:const preview-mid-len 90)
(def ^:private ^:const preview-tail-len 60)

(def ^:private ^:const preview-lead-in
  "Characters of lead-in before a match. Enough to know which sentence the
   window cut into, not so much that the match itself is the tail of the row."
  35)

(def ^:private ^:const preview-tail-lead 12)

(def ^:private ^:const preview-tail-gap
  "How far below the matched region a second match must sit to be worth showing:
   closer than this it is the same passage twice."
  400)

(def ^:private ^:const preview-short-head
  "An opening this short is a breadcrumb (`Drafts · Using Vis`), not a
   summary — the line under it carries the meaning, so take both."
  45)

(def ^:private ^:const preview-hit-terms 3)

(defn- collapse
  "One line: every whitespace run becomes a single space and markdown scaffolding
   is trimmed off the ends, so a window cut out of a table or a bullet reads as
   prose."
  ^String [^String s]
  (-> s
      (str/replace #"\s+" " ")
      (str/replace #"^[\s#*`|>_-]+" "")
      (str/replace #"[\s|]+$" "")
      str/trim))

(defn- clip
  "`s` capped at `n` characters, cut at a word boundary when that keeps most of
   it, marked with an ellipsis when anything was dropped."
  ^String [^String s ^long n]
  (if (<= (count s) n)
    s
    (let
      [cut
       (subs s 0 n)

       sp
       (.lastIndexOf cut " ")]

      (str (if (> sp (long (* 0.6 n))) (subs cut 0 sp) cut) "…"))))

(defn- word-start?
  "Is offset `i` the start of a word — nothing alphanumeric to its left? Matching
   mid-word would put `cat` inside `concatenate`."
  [^String s ^long i]
  (or (zero? i) (not (Character/isLetterOrDigit (.charAt s (dec i))))))

(defn- find-term
  "First word-start offset of `term` in `low` at or after `from`, or -1."
  ^long [^String low ^String term ^long from]
  (loop [i (.indexOf low term (int from))]
    (cond (neg? i) -1
          (word-start? low i) i
          :else (recur (.indexOf low term (int (inc i)))))))

(defn- last-term
  "Last word-start offset of `term` in `low`, or -1."
  ^long [^String low ^String term]
  (loop [i (.lastIndexOf low term)]
    (cond (neg? i) -1
          (word-start? low i) i
          :else (recur (.lastIndexOf low term (int (dec i)))))))

(defn- line-of
  "The 1-based line `pos` falls on."
  ^long [^String s ^long pos]
  (loop
    [from
     0

     line
     1]

    (let [j (.indexOf s "\n" (int from))]
      (if (or (neg? j) (>= j pos)) line (recur (inc j) (inc line))))))

(defn- word-window
  "The text around `pos`, widened to whole words: `back` characters of lead-in
   and `fwd` after it, never reaching back before `floor` — so a window never
   re-shows what the caller already printed. A match is worth more forwards than
   backwards, which is why the two spans differ."
  ^String [^String s pos back fwd floor]
  (let
    [p
     (long pos)

     fl
     (long floor)

     n
     (.length s)

     lo
     (loop [i (max fl (- p (long back)))]
       (if (and (> i fl) (Character/isLetterOrDigit (.charAt s (dec i)))) (recur (dec i)) i))

     hi
     (loop [i (min n (+ p (long fwd)))]
       (if (and (< i n) (Character/isLetterOrDigit (.charAt s i))) (recur (inc i)) i))]

    (collapse (subs s lo hi))))

(defn- opening
  "`[rendered-head raw-end]` — the document's first non-blank line, plus the next
   one when the first is only a breadcrumb."
  [^String s ^long start]
  (let
    [n
     (.length s)

     eol
     (let [j (.indexOf s "\n" (int start))]
       (if (neg? j) n j))

     head
     (collapse (subs s start eol))]

    (if (or (>= (long (count head)) (long preview-short-head)) (>= (long eol) n))
      [head eol]
      (let
        [nxt
         (loop [i (inc eol)]
           (if (and (< i n) (Character/isWhitespace (.charAt s i))) (recur (inc i)) i))

         eol2
         (let [j (.indexOf s "\n" (int nxt))]
           (if (neg? j) n j))

         more
         (collapse (subs s nxt eol2))]

        (if (str/blank? more) [head eol] [(str head " — " more) eol2])))))

(defn preview
  "A BOUNDED excerpt of one document for the terms that matched it — what a
   search row shows instead of the body, which `doc(name)` answers whole.

   Three parts, joined by ellipses: the document's own OPENING (its first line,
   plus the line under it when the first is a breadcrumb), the best MATCHED
   region below what the opening already showed, and — when the terms recur far
   deeper — a fragment from DOWN the document, which is what separates a page
   that is about the query from one that mentions it once.

   `terms` is `bm25/rank`'s resolved-term metadata; the rarest term (highest
   `:idf`) picks the window, and `:hit` names the terms that landed, rendering a
   correction as `pathc→patch` so a rewritten query is never silent.

   Answers `{:gist :at :hit}`. `:at` is the 1-based LINE the matched region
   starts on — 0 when the opening already held the match — so a 70 KB skill can
   be read from where it answers."
  ([text] (preview text nil))
  ([text terms]
   (let
     [s
      (str text)

      n
      (.length s)

      start
      (loop [i 0]
        (if (and (< i n) (Character/isWhitespace (.charAt s i))) (recur (inc i)) i))

      [raw-head head-end]
      (opening s start)

      head
      (clip (str/replace raw-head #"^#+\s*" "") preview-head-len)

      ;; Where the rendered opening stops in the RAW text: a window may never
      ;; reach back into it.
      shown
      (min (long head-end) (+ (long start) (long (count head))))

      low
      (str/lower-case s)

      found
      (into []
            (comp (filter #(>= (count (str (:as %))) 3))
                  (keep (fn [{:keys [as] :as t}]
                          (let [f (find-term low (str as) 0)]
                            (when-not (neg? f)
                              (assoc t
                                :below (find-term low (str as) shown)
                                :last (last-term low (str as))))))))
            (sort-by (comp - :idf) terms))

      ;; A term the corpus almost never uses says what the document is; one it
      ;; uses everywhere ("file", "the") would put the window on noise.
      strong
      (if-let [top (when (seq found) (apply max (map :idf found)))]
        (or (seq (filter #(>= (double (:idf %)) (* 0.5 (double top))) found)) found)
        [])

      mpos
      (let [bs (remove neg? (map :below strong))]
        (when (seq bs) (apply min bs)))

      deepest
      (when mpos (apply max (map :last strong)))

      mid
      (when mpos (clip (word-window s mpos preview-lead-in preview-mid-len shown) preview-mid-len))

      tail
      (when (and deepest (> (long deepest) (+ (long mpos) (long preview-tail-gap))))
        (clip (word-window s
                           deepest
                           preview-tail-lead
                           preview-tail-len
                           (+ (long mpos) (long preview-mid-len)))
              preview-tail-len))]

     {:gist (str/join " … " (remove str/blank? [head mid tail]))
      :at (if mpos (line-of s mpos) 0)
      :hit (into []
                 (comp (take preview-hit-terms)
                       (map (fn [{:keys [term as]}]
                              (if (= (str term) (str as)) (str as) (str term "→" as)))))
                 strong)})))
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

;; Sources — registered, never required in

(defonce ^:private sources
  ;; `[[id {:stamp f :entries g}] ...]` in registration order, so precedence is
  ;; readable rather than hash-ordered: the FIRST source to claim a name keeps
  ;; it.
  (atom []))

(defonce ^:private corpus-cache
  ;; `{:stamps [[id stamp] …] :entries [...]}`. Building the corpus re-reads
  ;; every documentation page, every SKILL.md and every cached MCP listing;
  ;; asking each source for its STAMP is a counter or a stat pass. Same stamps,
  ;; same entries — and the identical vector, so the ranker's own index cache
  ;; hits too.
  (atom nil))

(defn register-source!
  "Register `entries-fn` — a 0-arity returning a coll of `:vis.doc/entry` —
   under `id`, guarded by `stamp-fn`.

   `stamp-fn` is a 0-arity answering a CHEAP value that changes exactly when
   this source's entries would: a generation counter, a stat mark, a count.
   `entries` re-runs NO source while every stamp is unchanged, so a stamp that
   lies pins a stale corpus and a stamp that costs as much as the source itself
   buys nothing. `(constantly ::always)` is not a stamp — it is a source that
   refuses to be cached, and it must earn that.

   Re-registering an `id` replaces it IN PLACE, so a reloaded namespace never
   duplicates its own entries."
  [id stamp-fn entries-fn]
  (let [src {:stamp stamp-fn :entries entries-fn}]
    (swap! sources (fn [ss]
                     (if (some (comp #{id} first) ss)
                       (mapv (fn [[k v]]
                               (if (= k id) [k src] [k v]))
                             ss)
                       (conj ss [id src]))))
    ;; A replaced source may answer differently under an unchanged stamp.
    (reset! corpus-cache nil)
    id))

(defn- documentation-page-entries
  "Every embedded `vis-docs` page as an entry. Re-collected per call (the pages
   are tiny and the live `/docs` site re-reads them too, so a dev edit shows
   without a restart). A page has no `:call` — it is prose."
  []
  (into []
        (keep (fn [{:keys [slug title section blurb md]}]
                (when (and (seq (str slug)) (seq (str md)))
                  {:name (str slug)
                   :kind "page"
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
                   :kind "skill"
                   :text (str (when (seq (str description)) (str description "\n\n")) body)})))
        (discovery/skills)))

;; The stamps: a documentation page and a SKILL.md are files, and both layers
;; already answer a generation that ticks only when they were re-read.
(register-source! :documentation-pages #(docs/generation) #'documentation-page-entries)
(register-source! :skills #(discovery/generation) #'skill-entries)

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
   an env fails to build.

   Memoized on the sources' stamps: while every stamp is unchanged this answers
   the IDENTICAL vector without re-running a single source."
  []
  (let
    [ss
     @sources

     stamps
     (mapv (fn [[id {:keys [stamp]}]]
             [id (try (stamp) (catch Throwable _ ::unstamped))])
           ss)

     cached
     @corpus-cache]

    (if (and cached (= stamps (:stamps cached)))
      (:entries cached)
      (let
        [raw
         (into []
               (mapcat (fn [[_id {:keys [entries]}]]
                         (try (entries) (catch Throwable _ nil))))
               ss)

         built
         (into []
               (comp (filter #(and (seq (str (:name %))) (some? (:text %))))
                     (map (fn [e]
                            (cond-> {:name (str (:name e)) :text (str (:text e))}
                              (contains? kinds (:kind e))
                              (assoc :kind (:kind e))

                              (seq (str (:call e)))
                              (assoc :call (str (:call e))))))
                     (dedupe-by-name))
               raw)]

        (reset! corpus-cache {:stamps stamps :entries built})
        built))))

;; Search — one call into the BM25F ranker

(defn- ranked-docs
  "The corpus as the ranker's three fields, carrying the entry itself as the
   payload. The opening line is a field of its own AND part of the body: the
   ranker saturates the three together (`bm25/default-opts`), so it counts as
   extra evidence and never as the whole of a document's score.

   The field is the FIRST LINE, breadcrumb and all: a measured attempt to rank
   the breadcrumb-plus-next-line `opening` instead lost 2 of 51 asks (MRR .898
   → .872), because lengthening one field spends its length normalization on
   pages while every tool's one-line description keeps its own."
  [es]
  (mapv (fn [e]
          (let [t (str (:text e))]
            {:name (str (:name e)) :gist (gist t) :body t :value e}))
        es))

(defn search
  "Rank `es` against `query` with BM25F over three fields — name, first line,
   whole body. Terms are ORed and priced by IDF, so a query is a description
   rather than a conjunction: a six-word natural-language ask answers the
   document that covers most of it, and a word no document carries costs
   nothing instead of emptying the result. A query that IS a handle wins that
   handle outright, and a term nothing contains is spell-corrected against the
   vocabulary before it is dropped. Ties break on name, so the order is stable.

   A blank query is not a failure, it is \"everything\": the whole corpus, in
   name order.

   The ranker owns the scoring, the index and its cache (`bm25`); this maps the
   corpus onto its fields and nothing else, so the index of an unchanged corpus
   is reused across calls and across threads. `opts` is `bm25/search`'s —
   `:limit` for a bounded answer, the scoring knobs for a caller that needs
   different weights."
  ([es query] (bm25/search (ranked-docs es) query))
  ([es query opts] (bm25/search (ranked-docs es) query opts)))

;; The curated index — `doc()` with no argument

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

;; The three things the two verbs PRINT

(defn entry-text
  "What `doc(target)` answers for one entry: the handle, the expression that
   uses it when there is one, the keys that expression's options dict must carry,
   then the WHOLE document. `note` is the caller's one-word remark about the
   handle (`env-python` marks a live callable)."
  ([entry] (entry-text entry nil))
  ([{:keys [name text call params]} note]
   (str "# "
        name
        (when (seq (str note)) (str "  ·  " note))
        (when (seq (str call)) (str "\n\n" call))
        (when (seq (str params)) (str "\n" params))
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
  (let [near (search es (str target) {:limit 5})]
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
