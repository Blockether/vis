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
;; Search — ranked full text, terms ANDed
;; =============================================================================

(def ^:private name-hit 100.0)
(def ^:private name-part-hit 50.0)
(def ^:private gist-hit 10.0)
(def ^:private body-hit 1.0)

(defn- term-score
  [nm gst txt term]
  (cond (= nm term) name-hit
        (str/includes? nm term) name-part-hit
        (str/includes? gst term) gist-hit
        (str/includes? txt term) body-hit
        :else 0.0))

(defn search
  "Rank `es` against `query`: whitespace-separated terms are ANDed, and every
   term scores where it hit — an exact name beats a name substring beats the
   first line beats the body. Ties break on name, so the order is stable.

   A blank query is not a failure, it is \"everything\": the whole corpus, in
   name order."
  [es query]
  (let
    [terms (into [] (remove str/blank?) (str/split (str/lower-case (str/trim (str query))) #"\s+"))]
    (if (empty? terms)
      (vec (sort-by :name (map #(assoc % :score 0.0) es)))
      (->> es
           (keep (fn [{:keys [name text] :as e}]
                   (let
                     [nm (str/lower-case (str name))
                      txt (str/lower-case (str text))
                      gst (str/lower-case (gist text))
                      scores (mapv #(term-score nm gst txt %) terms)]

                     (when (every? pos? scores) (assoc e :score (reduce + scores))))))
           (sort-by (juxt (comp - :score) :name))
           vec))))

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
