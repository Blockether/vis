(ns com.blockether.vis.internal.doc-corpus
  "The one ordered document corpus behind `apropos(pattern)` and `doc(name)`.

   `META-INF/vis/manifest.edn` names every static EDN resource explicitly, and each
   resource is a vector of records shaped as `{:name :kind :text}` or, for a
   documentation page, `{:name :kind doc :resource ...}`. There is ONE shape and ONE
   spec — `:vis.doc/record` — for a harvested symbol, a page, a skill and an MCP tool
   alike: a static record that breaks it throws naming its resource, a dynamic
   source's is logged and dropped. Dynamic skills, MCP tools and live callable
   contracts append records through `register-source!`.

   `entries` is the whole corpus in source order, deduplicated by EXACT name; `pages`
   is the documentation subset the docs site renders. `apropos` applies one regular
   expression to record names and preserves corpus order — there is no ranking,
   tokenization, search index or classpath discovery. `doc` retrieves the same record
   by name and prints its whole text."
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.harness.discovery :as discovery]
            [com.blockether.vis.internal.manifest :as manifest]
            [taoensso.telemere :as tel]))

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
   all — reachable by name through `doc`, never returned by `apropos`."
  #{"function" "class" "module" "tool" "doc" "skill" "local"})

(s/def :vis.doc/kind kinds)

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

;; Sources — plain ordered functions

(defonce ^:private sources
  ;; `[[id entries-fn] ...]` in registration order, so precedence is readable
  ;; rather than hash-ordered: the FIRST source to claim a name keeps it.
  (atom []))

(defn register-source!
  "Register a 0-arity `entries-fn` under `id`.

   Sources are read directly whenever `apropos` or `doc` asks for the corpus.
   Re-registering an `id` replaces it IN PLACE, so a reloaded namespace never
   duplicates its own entries."
  [id entries-fn]
  (swap! sources (fn [ss]
                   (if (some (comp #{id} first) ss)
                     (mapv (fn [[k v]]
                             (if (= k id) [k entries-fn] [k v]))
                           ss)
                     (conj ss [id entries-fn]))))
  id)

(defn- one-body?
  "A record carries its text EITHER inline — `:text`, how a harvested symbol lends
   its own docstring — OR in the resource it names, never both and never neither,
   so nothing downstream decides which of the two is authoritative."
  [{:keys [text resource]}]
  (if (some? resource) (nil? text) (not (str/blank? (str text)))))

(s/def :vis.doc/resource string?)
(s/def :vis.doc/record
  (s/and (s/keys :req-un [:vis.doc/name :vis.doc/kind]
                 :opt-un [:vis.doc/text :vis.doc/resource :vis.doc/call :vis.doc/params])
         ;; CLOSED: a key no reader reads is a key that drifts. Site navigation —
         ;; how a page is titled, grouped and ordered — is the docs site's own
         ;; business and lives in `vis-docs/site.edn`, not in the search corpus.
         #(every? #{:name :kind :text :resource :call :params} (keys %))
         #(not (str/blank? (:name %)))
         one-body?))

(defn- page-names-a-file?
  "A page IS a markdown file: a `doc` record DECLARES the resource `docs` renders.
   Checked where a record is DECLARED and not in `:vis.doc/record`, which says what
   every READER receives — by then `resolved-record` has spent the address and the
   page carries its `:text`."
  [{:keys [kind resource]}]
  (or (not= "doc" kind) (string? resource)))

(defn- checked-record
  "`record`, or a throw naming it and explaining why. The manifest declares WHICH
   resources exist; this is what a record inside one has to BE — so a hand-edited
   catalogue and a regenerated symbol index both fail at load with `explain-data`
   instead of quietly contributing nothing to search."
  [resource record]
  (if (and (s/valid? :vis.doc/record record) (page-names-a-file? record))
    record
    (throw (ex-info (str "Invalid document record in " (pr-str resource))
                    {:type :vis.doc/invalid-record
                     :resource resource
                     :name (:name record)
                     :explain (s/explain-data :vis.doc/record record)}))))

(defn- resolved-record
  "The record with its body in `:text`: a named resource is slurped HERE, once, so
   no reader downstream reaches for the classpath again."
  [{:keys [name resource] :as record}]
  (if resource
    (let [url (or (io/resource resource)
                  (throw (ex-info
                           "Missing document resource"
                           {:type :vis.doc/missing-resource :name name :resource resource})))]
      (-> record
          (assoc :text (slurp url))
          (dissoc :resource)))
    record))

(defonce ^:private cached-records
  ;; Only SUCCESS is cached, and NEVER at the top level: `graal-build-time` initializes
  ;; this namespace inside the BUILDER, so a `def` bakes every parsed record into the
  ;; image heap of every process. A bad resource throws naming itself on every ask
  ;; instead of once, far away, at load.
  (atom nil))

(defn- manifest-records
  "Every static record the manifest names, in manifest order and already whole:
   checked against `:vis.doc/record` and carrying its `:text`.

   Read on the FIRST ask, then cached: a distribution's documents cannot change under
   a running process, so the resources stay resources and nothing is baked into a
   native image. `forget-records!` is how an edited page becomes visible in a
   development JVM."
  []
  (or @cached-records
      (reset! cached-records (into []
                                   (comp (mapcat (fn [[resource value]]
                                                   (map #(checked-record resource %) value)))
                                         (map resolved-record))
                                   (map vector
                                        (manifest/apropos-resource-paths)
                                        (manifest/read-apropos-resources))))))

(defn forget-records!
  "Drop the cached read so the next ask reaches for the resources again — what
   `/reload` calls. In a binary the resources are frozen and this costs one re-read;
   in a development JVM it is what makes an edited page visible without a restart."
  []
  (reset! cached-records nil)
  nil)

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
                   :text (str (when (seq (str description)) (str description "

")) body)})))
        (discovery/skills)))

(register-source! :manifest-apropos #'manifest-records)
(register-source! :skills #'skill-entries)

(defn- dedupe-by-name
  "Transducer keeping the FIRST entry for each EXACT name. Names are compared as
   written, never case-folded: Python is case-sensitive, so `requests.Session` the
   class and `requests.session` the function are two symbols, and folding them left
   the second one with no way to be reached."
  []
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn ([] (rf)) ([acc] (rf acc)) ([acc e] (let [k (str (:name e))]
                                                (if (contains? @seen k)
                                                  acc
                                                  (do (vswap! seen conj k) (rf acc e)))))))))

(defn- usable-entry?
  "True when a source's entry is a `:vis.doc/record` every reader can use. A broken
   one is named in the log and skipped rather than coerced into something smaller:
   the static resources already threw at read, so what this catches is a live skill,
   an MCP listing or an extension's own source."
  [id entry]
  (or (s/valid? :vis.doc/record entry)
      (do (tel/log! {:level :warn
                     :id ::unusable-entry
                     :data {:source id
                            :name (:name entry)
                            :explain (s/explain-data :vis.doc/record entry)}})
          false)))

(defn entries
  "The whole corpus, read from its plain ordered sources and deduplicated by name
   (first wins). Every entry travels WHOLE, in the one shape `:vis.doc/record`
   declares. A source that throws contributes nothing — discovery must never be the
   reason an environment fails to build."
  []
  (into []
        (comp (mapcat (fn [[id entries-fn]]
                        (into []
                              (filter #(usable-entry? id %))
                              (try (entries-fn) (catch Throwable _ nil)))))
              (dedupe-by-name))
        @sources))

(defn pages
  "Every documentation PAGE, in manifest order and whole — the corpus filtered to the
   `doc` kind. The docs site renders from THIS: one read, one validation, one order,
   and no second reader of the same resources."
  []
  (filterv #(= "doc" (:kind %)) (entries)))

 ;; Search — one regular expression over names

(defn search
  "Return entries whose `:name` contains a match for `pattern`, preserving corpus
   order. A blank string lists every entry. Invalid regular expressions are errors."
  [es pattern]
  (let [re (cond (instance? java.util.regex.Pattern pattern) pattern
                 (str/blank? (str pattern)) (re-pattern ".*")
                 :else (re-pattern (str pattern)))]
    (into [] (filter #(re-find re (str (:name %)))) es)))

;; The curated index — `doc()` with no argument

(def curated
  "What `doc()` prints: a hand-ordered short list of the verbs a session starts
   from, not a corpus dump. Everything else remains addressable through
   `apropos(pattern)` and `doc(name)`."
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
  "What bare `doc()` answers: curated verbs that are actually present, one
   `name — first line` per row. Everything else is one `apropos(pattern)` away."
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

    (str
      "# doc()

"
      (str/join "
" rows)
      "

Everything else — "
      (count es)
      " documents in all — is one `apropos(pattern)` away; `doc(name)` prints any of them whole.")))

(defn miss-text
  "What `doc(target)` answers when nothing carries that handle."
  [_es target]
  (str (pr-str (str target))
       " is not a handle. `doc()` lists the verbs a session starts from; "
       "`apropos(pattern)` filters every known name."))
