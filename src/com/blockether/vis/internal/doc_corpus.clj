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
   above the document and are not part of it. The first line is a scored field of
   its own AND the `body` an `apropos` row shows, so a tool whose text opened with
   its own signature answered with a signature and stopped matching the words its
   prose is written in.

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
   decides what to DO with it, and what `doc` RETURNS for it: `function` a
   callable's docstring, `class` a class's, `module` an importable module's,
   `tool` a Vis verb's contract, `doc` a whole documentation page, `skill` a whole
   `SKILL.md`, `local` a callable this session defined that carries no contract at
   all — reachable by name through `doc`, never ranked by `apropos`. A source that
   invents a kind is a bug, not a new category."
  #{"function" "class" "module" "tool" "doc" "skill" "local"})

(s/def :vis.doc/kind kinds)
(s/def :vis.doc/entry
  (s/keys :req-un [:vis.doc/name :vis.doc/text]
          :opt-un [:vis.doc/call :vis.doc/params :vis.doc/kind]))
(s/def :vis.doc/entries (s/coll-of :vis.doc/entry :kind vector?))
(s/def :vis.doc/result
  (s/or :index (s/keys :req-un [:vis.doc/entries])
        :entry :vis.doc/entry))

;; `apropos` = the same records, ranked. A row is what a reader needs to decide
;; whether to open it: what it IS, its name, its rank, and the opening of its own
;; text — never a window cut around the query, because the unit of the index is
;; the SYMBOL, and a symbol's first line already describes the whole of it.
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
      (let [nl (.indexOf s "\n" from)
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


(def ^:private body-max-len
  "Characters of a document's own opening that an `apropos` row carries. One
   sentence of a docstring — enough to choose between two hits, short enough that
   ten rows cost less than one page."
  100)

(defn body-text
  "The opening of `text` as ONE line, capped at `body-max-len`: the `body` an
   `apropos` row shows. Whitespace collapses so a wrapped docstring reads as the
   sentence its author wrote, and a leading markdown heading goes the way `gist`
   drops it. The first PARAGRAPH is the whole of it — a docstring's opening sentence
   describes the symbol; what follows is one `doc(name)` away."
  [text]
  (let [para
        (first (str/split (str/trim (str text)) #"\n\s*\n"))

        s
        (str/trim (str/replace (str/replace (str para) #"\s+" " ") #"^#+\s*" ""))]

    (if (> (count s) (long body-max-len))
      (str (str/trim (subs s 0 (dec (long body-max-len)))) "…")
      s)))
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
                   :kind "doc"
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
      (fn ([] (rf)) ([acc] (rf acc)) ([acc e] (let [k (normalize-name (:name e))]
                                                (if (contains? @seen k)
                                                  acc
                                                  (do (vswap! seen conj k) (rf acc e)))))))))

(defn entries
  "The whole corpus, in registration order, deduplicated by name (first wins).
   A source that throws contributes nothing — discovery must never be the reason
   an env fails to build.

   Memoized on the sources' stamps: while every stamp is unchanged this answers
   the IDENTICAL vector without re-running a single source."
  []
  (let [ss
        @sources

        stamps
        (mapv (fn [[id {:keys [stamp]}]]
                [id (try (stamp) (catch Throwable _ ::unstamped))])
              ss)

        cached
        @corpus-cache]

    (if (and cached (= stamps (:stamps cached)))
      (:entries cached)
      (let [raw
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
    pages while every tool's one-line description keeps its own.

    A document is one SYMBOL, so its name IS the handle a reader types and its
    first line IS what a reader is shown; nothing is lent, aliased or previewed."
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
  ["apropos" "doc" "read_session" "fold_session" "ls" "grep" "cat" "patch" "shell" "run_tests"
   "repl_eval" "lint_code" "format_code" "attach" "mcp__call"])

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
  (let [by-name
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
