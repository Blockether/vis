(ns com.blockether.vis.internal.docs.corpus
  "The one ordered document corpus behind `apropos(pattern)` and `doc(name)`.

   Static resources and live sources contribute the same closed record shape.
   Invalid static records fail at load; invalid dynamic records are logged and dropped.

   `entries` is the whole corpus in source order, deduplicated by EXACT name; `pages`
   is the documentation subset the docs site renders. A page can give an example
   twice, as a Python and an HTTP variant. `entries` gives the agent only the
   Python variant, without the markup. `pages` keeps both for the site.

   `apropos` applies one regular expression to record names, reading a hyphenated
   name also as words, and to the outline of pages and skills: title, opening,
   headings, `When to use` problems.
   It preserves corpus order. There is no ranking, tokenization, search index or
   classpath discovery. `doc` retrieves the same record by name and prints its
   whole text."
  (:refer-clojure :exclude [record?])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.vis.internal.foundation.harness.discovery :as discovery]
            [com.blockether.vis.internal.extension.manifest :as manifest]
            [taoensso.telemere :as tel]))

(set! *warn-on-reflection* true)

(def kinds
  "The closed vocabulary of `:kind` — what a document IS, which is how a reader
   decides what to DO with it, and what `doc` RETURNS for it: `function` a
   callable's docstring, `class` a class's, `module` an importable module's,
   `tool` a Vis verb's contract, `doc` a whole documentation page, `skill` a whole
   `SKILL.md`, `local` a callable this session defined that carries no contract at
   all — reachable by name through `doc`, never returned by `apropos`."
  #{"function" "class" "module" "tool" "doc" "skill" "local"})

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

(defn- opening
  "The first paragraph of `text` as ONE line: what a document says it is for.
   Whitespace collapses so a wrapped docstring reads as the sentence its author
   wrote. Skip a leading Markdown title when prose follows it, so pages describe
   their purpose instead of repeating their name."
  [text]
  (let [prose
        (str/replace-first (str/trim (str text)) #"^#{1,6}[ \t]+[^\r\n]*(?:\r?\n[ \t]*)+" "")

        para
        (first (str/split prose #"\n\s*\n"))]

    (str/trim (str/replace (str/replace (str para) #"\s+" " ") #"^#+\s*" ""))))

(defn body-text
  "The opening of `text` capped at `body-max-len`: the `body` an `apropos` row
   shows. The first paragraph is enough; the complete document remains one
   `doc(name)` away."
  [text]
  (let [s (opening text)]
    (if (> (count s) (long body-max-len))
      (str (str/trim (subs s 0 (dec (long body-max-len)))) "…")
      s)))

(defn normalize-name
  "Coerce a caller's target to a comparable handle: unwrap the map/kwargs shape,
   trim, drop a trailing `.md` (pages cross-link by filename), lower-case. This
   is why `doc(\"Index.md\")` and `doc(\"index\")` are the same ask."
  [target]
  (-> (if (map? target) (or (get target "name") (get target "slug") "") target)
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

(defn record?
  [record]
  (and (map? record)
       (every? #{:name :kind :text :resource :call :params} (keys record))
       (string? (:name record))
       (not (str/blank? (:name record)))
       (contains? kinds (:kind record))
       (or (not (contains? record :text)) (string? (:text record)))
       (or (not (contains? record :resource)) (string? (:resource record)))
       (or (not (contains? record :call)) (nil? (:call record)) (string? (:call record)))
       (or (not (contains? record :params)) (nil? (:params record)) (string? (:params record)))
       (one-body? record)))

(defn- page-names-a-file?
  "A page IS a markdown file: a `doc` record DECLARES the resource `docs` renders.
   Checked where a record is DECLARED and not in `:vis.doc/record`, which says what
   every READER receives — by then `resolved-record` has spent the address and the
   page carries its `:text`."
  [{:keys [kind resource]}]
  (or (not= "doc" kind) (string? resource)))

(defn- checked-record
  [resource record]
  (if (and (record? record) (page-names-a-file? record))
    record
    (throw (ex-info (str "Invalid document record in " (pr-str resource))
                    {:type :vis.doc/invalid-record
                     :resource resource
                     :name (:name record)
                     :explain {:valid false :value record}}))))

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
        (keep
          (fn [{:keys [name description body package dir resources]}]
            (when (seq (str name))
              {:name (str name)
               :kind "skill"
               :text (str (when (seq (str description)) (str description "\n\n"))
                          (when package
                            (str "Package: "
                                 (:name package)
                                 "@"
                                 (:version package)
                                 "\n"
                                 "Skill directory: "
                                 dir
                                 "\n"
                                 (when (seq resources)
                                   (str "Resources:\n"
                                        (str/join "\n" (map #(str "- " %) resources))
                                        "\n"))
                                 "\n"))
                          body)})))
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
  (or (record? entry)
      (do (tel/log! {:level :warn
                     :id ::unusable-entry
                     :data {:source id :name (:name entry) :explain {:valid false :value entry}}})
          false)))

(defn- whole-entries
  "The whole corpus, read from its plain ordered sources and deduplicated by name
   (first wins). Every entry travels WHOLE, in the one shape `:vis.doc/record`
   declares, and a page keeps every variant. A source that throws contributes
   nothing — discovery must never be the reason an environment fails to build."
  []
  (into []
        (comp (mapcat (fn [[id entries-fn]]
                        (into []
                              (filter #(usable-entry? id %))
                              (try (entries-fn) (catch Throwable _ nil)))))
              (dedupe-by-name))
        @sources))

;; Paired variants — the site shows each one, the agent reads Python

(def agent-variant
  "The variant of a paired page that `apropos` and `doc` give the agent. A page
   can give one example twice, in a `<div data-variant=\"python\">` block and then
   a `<div data-variant=\"http\">` block. The site shows both, with a switch. The
   agent writes Python, so it gets the Python block without the markup."
  "python")

(def ^:private variant-open
  "The line that opens a variant block. The tag stands alone on its line."
  #"<div data-variant=\"([a-z]+)\">")

(defn- fence?
  "Whether `line` opens or closes a fenced code block."
  [line]
  (some? (re-find #"^\s*(?:```|~~~)" line)))

(defn variant-lines
  "PURE: each line of `md` as `{:text line :variant name :tag kind :fenced? bool}`.
   `:variant` names the block that holds the line, and is nil outside every block.
   `:tag` is `:open` or `:close` on the markup lines of a block. A line in fenced
   code is never a tag, so a page can show the markup in an example."
  [md]
  (loop [[line & more]
         (str/split-lines (str md))

         fenced?
         false

         variant
         nil

         acc
         (transient [])]

    (if (nil? line)
      (persistent! acc)
      (let [fence-line?
            (fence? line)

            open
            (when-not (or fenced? fence-line? variant)
              (second (re-matches variant-open (str/trim line))))

            close?
            (and (some? variant) (not fenced?) (not fence-line?) (= "</div>" (str/trim line)))]

        (recur more
               (if fence-line? (not fenced?) fenced?)
               (cond open open
                     close? nil
                     :else variant)
               (conj! acc
                      (cond open {:text line :variant open :tag :open}
                            close? {:text line :variant variant :tag :close}
                            :else
                            {:text line :variant variant :fenced? (or fenced? fence-line?)})))))))

(defn variants
  "PURE: the names of the variant blocks in `md`, in page order, each one once."
  [md]
  (if (str/includes? (str md) "data-variant=")
    (into [] (comp (filter #(= :open (:tag %))) (map :variant) (distinct)) (variant-lines md))
    []))

(defn variant-text
  "PURE: `md` as a reader of `variant` gets it. Lines outside every block stay.
   A `variant` block keeps its lines without its tags. Other blocks go. A blank
   line next to a removed line merges with its neighbour, so no gap stays. Text
   without a block comes back unchanged."
  [md variant]
  (let [md (str md)]
    (if-not (str/includes? md "data-variant=")
      md
      (let [{:keys [out]}
            (reduce (fn [{:keys [out squeeze?] :as acc} {:keys [text tag fenced?] :as line}]
                      (let [blank? (and (not fenced?) (str/blank? text))]
                        (cond (or tag (not (contains? #{nil variant} (:variant line))))
                              (assoc acc :squeeze? true)
                              (and squeeze? blank? (or (empty? out) (str/blank? (peek out)))) acc
                              :else {:out (conj out text) :squeeze? (and squeeze? blank?)})))
                    {:out [] :squeeze? false}
                    (variant-lines md))]
        (str (str/join "\n" out) (when (str/ends-with? md "\n") "\n"))))))

(defn- agent-entry
  "`entry` as the agent reads it: a documentation page in its `agent-variant`."
  [entry]
  (if (= "doc" (:kind entry)) (update entry :text variant-text agent-variant) entry))

(defn entries
  "The corpus that `apropos` and `doc` read, in source order and deduplicated by
   name (first wins). A documentation page arrives in its `agent-variant` only,
   so every agent-facing read, search included, shares ONE filter."
  []
  (mapv agent-entry (whole-entries)))

(defn pages
  "Every documentation PAGE, in manifest order and whole, with every variant — the
   `doc` kind of the corpus. The docs site renders from THIS: one read, one
   validation, one order, and no second reader of the same resources."
  []
  (filterv #(= "doc" (:kind %)) (whole-entries)))

;; Search — one regular expression over names and page outlines

(def ^:private outlined-kinds
  "The kinds `search` also finds by their outline. A callable is found by its name
   alone, so a word its contract happens to use never floods a search."
  #{"doc" "skill"})

(defn- problems
  "The bold statements that open the items of a `When to use` list, each as ONE
   line however its source wraps."
  [lines]
  (into []
        (keep #(some->> (re-find #"^\*\*(.+?)\*\*" (str/replace % #"\s+" " "))
                        second
                        str/trim))
        (rest (str/split (str/join "\n" lines) #"(?m)^[ \t]*[-*+][ \t]+"))))

(defn outline
  "The phrases a documentation page or skill is found by besides its name: its
   opening paragraph, every heading outside fenced code — the title included —
   and the bold problem statements its `When to use` section lists. Other kinds
   have no outline."
  [{:keys [kind text]}]
  (if-not (contains? outlined-kinds (str kind))
    []
    (loop [lines
           (str/split-lines (str text))

           fenced?
           false

           when-to-use
           nil

           phrases
           [(opening text)]]

      (let [[line & more]
            lines

            heading
            (when (and line (not fenced?))
              (second (re-matches #"#{1,6}[ \t]+(.*?)(?:[ \t]+#+)?[ \t]*" line)))

            phrases
            (if (and when-to-use (or (nil? line) heading))
              (into phrases (problems when-to-use))
              phrases)]

        (cond (nil? line) (into [] (remove str/blank?) phrases)
              (fence? line) (recur more
                                   (not fenced?)
                                   (some-> when-to-use
                                           (conj line))
                                   phrases)
              heading (recur more
                             fenced?
                             (when (= "when to use" (str/lower-case heading)) [])
                             (conj phrases heading))
              :else (recur more
                           fenced?
                           (some-> when-to-use
                                   (conj line))
                           phrases))))))

(defn- name-forms
  "An entry's name as written and, when it is hyphenated, read as words: a search
   for `human input` finds `human-input`."
  [{:keys [name]}]
  (let [n (str name)]
    (distinct [n (str/replace n "-" " ")])))

(defn- search-pattern
  "Compile a caller's pattern: a string ignores case, a compiled `Pattern` keeps
   its own flags and a blank string matches everything."
  ^java.util.regex.Pattern [pattern]
  (cond (instance? java.util.regex.Pattern pattern) pattern
        (str/blank? (str pattern)) (re-pattern ".*")
        :else (java.util.regex.Pattern/compile (str pattern)
                                               (int (bit-or
                                                      java.util.regex.Pattern/CASE_INSENSITIVE
                                                      java.util.regex.Pattern/UNICODE_CASE)))))

(defn search
  "Return entries `pattern` finds, preserving corpus order: a match in one of the
   `name-forms` or in one phrase of a page's or skill's `outline`. A string pattern
   ignores case; a compiled `Pattern` keeps its own flags. A blank string lists
   every entry. Invalid regular expressions are errors."
  [es pattern]
  (let [re
        (search-pattern pattern)

        found?
        #(some? (re-find re (str %)))]

    (into [] (filter #(or (some found? (name-forms %)) (some found? (outline %)))) es)))

;; The curated index — `doc()` with no argument

(def curated
  "What `doc()` prints: a hand-ordered short list of the verbs a session starts
   from, not a corpus dump. Everything else remains addressable through
   `apropos(pattern)` and `doc(name)`."
  ["apropos" "doc" "read_session" "fold_session" "ls" "grep" "cat" "patch" "shell" "attach"
   "mcp_call"])

;; The three things the two verbs PRINT

(defn entry-text
  "What `doc(target)` answers for one entry: the handle, the expression that
   uses it when there is one, the keys that expression's options dict must carry,
   the raw result it returns, then the WHOLE document. `note` is the caller's
   one-word remark about the handle (`env-python` marks a live callable)."
  ([entry] (entry-text entry nil))
  ([{:keys [name text call params result]} note]
   (str "# "
        name
        (when (seq (str note)) (str "  ·  " note))
        (when (seq (str call)) (str "\n\n" call))
        (when (seq (str params)) (str "\n" params))
        (when (seq (str result)) (str "\n" result))
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

(def ^:private miss-suggestion-limit
  "Handles a miss names: enough to cover a near name, few enough to read."
  5)

(defn miss-text
  "What `doc(target)` answers when nothing carries that handle: the handles in
   `es` whose name or outline contains the target literally, names first, so a
   near miss costs one more `doc` call instead of a search."
  [es target]
  (let [wanted
        (normalize-name target)

        hits
        (when-not (str/blank? wanted) (search es (java.util.regex.Pattern/quote wanted)))

        named?
        (fn [e]
          (some #(str/includes? (str/lower-case %) wanted) (name-forms e)))

        found
        (mapv :name (concat (filter named? hits) (remove named? hits)))

        shown
        (take miss-suggestion-limit found)

        more
        (- (count found) (count shown))

        near
        (when (seq shown)
          (str "Handles whose name or outline contains it: "
               (str/join ", " shown)
               (when (pos? more) (str " and " more " more"))
               ". "))]

    (str (pr-str (str target))
         " is not a handle. " near
         "`doc()` lists the verbs a session starts from; "
         "`apropos(pattern)` searches every known name and page outline.")))
