(ns com.blockether.vis.internal.skills
  "Internal skills catalog: scans `.agents/skills/*/SKILL.md` under both the
   repo root and `~/.agents/skills/*/SKILL.md` (user-global), parses
   YAML frontmatter via yamlstar, surfaces a flat alphabetical
   catalog of `{:name :description :path :source :body :extra}`
   maps. Repo entries win silently over user-global on name
   collision (plan §1 Q6).

   Frontmatter format follows the mattpocock convention:

     ---
     name: my-skill
     description: One-or-more-line description that says
       what the skill does and *when* to invoke it.
     ---

     # Body markdown ...

   The required fields are `name` and `description` (plan §1 Q10).
   Anything else (`disable-model-invocation`, `aliases`, future
   fields) is preserved under `:extra` (plan §1 Q10/a5). Files
   that fail YAML parsing or are missing required fields are
   DROPPED from the catalog and recorded in scan-warnings, so
   the user sees them at startup and during `vis extensions doctor`."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.workspace :as workspace]
   [taoensso.telemere :as tel]
   [yamlstar.core :as yaml]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Frontmatter splitting + parsing.
;; ---------------------------------------------------------------------------

(def ^:private FRONTMATTER_DELIM "---")

(defn- split-frontmatter
  "Split a SKILL.md body string into [frontmatter-yaml body-md].
   Returns nil when the file lacks `---` delimiters at the start.

   Convention: the file MUST begin with `---` on its first line; the
   second `---` line closes the frontmatter; everything after is the
   body. Liberal about trailing whitespace, strict about leading
   delimiter (a SKILL.md without `---` at the very top has no
   declared metadata)."
  [^String content]
  (when (and content (str/starts-with? content FRONTMATTER_DELIM))
    (let [lines (str/split-lines content)
          head  (first lines)
          rest-lines (vec (rest lines))]
      (when (= FRONTMATTER_DELIM (str/trimr head))
        (let [closer-idx (some (fn [[i line]]
                                 (when (= FRONTMATTER_DELIM (str/trimr line)) i))
                           (map-indexed vector rest-lines))]
          (when closer-idx
            (let [yaml-lines (subvec rest-lines 0 closer-idx)
                  body-lines (subvec rest-lines (inc closer-idx))]
              [(str/join "\n" yaml-lines)
               (str/triml (str/join "\n" body-lines))])))))))

(defn- parse-frontmatter
  "Parse a YAML frontmatter string into a Clojure map with KEYWORD
   keys. yamlstar returns string-keyed maps; we keywordize at the
   boundary so callers see idiomatic data.

   Returns {:ok parsed-map} on success, {:error reason} on parse
   failure or when the result isn't a map (top-level YAML must be
   a mapping for SKILL.md frontmatter to make sense)."
  [^String yaml-text]
  (try
    (let [parsed (yaml/load yaml-text)]
      (if (map? parsed)
        {:ok (into {} (map (fn [[k v]] [(keyword (str k)) v]) parsed))}
        {:error (str "frontmatter must be a YAML mapping, got " (type parsed))}))
    (catch Throwable t
      {:error (or (ex-message t) (str t))})))

(def ^:private REQUIRED_FIELDS [:name :description])

(defn- blank-string? [v]
  (or (nil? v) (and (string? v) (str/blank? v))))

(defn- normalize-string [v]
  ;; YAML can yield non-string scalars (numbers, booleans). Coerce
  ;; the required fields to trimmed strings.
  (when (some? v)
    (let [s (if (string? v) v (str v))
          t (str/trim s)]
      (when-not (str/blank? t) t))))

(defn- build-skill-map
  "Build a `{:name :description :path :source :body :extra}` map
   from a parsed frontmatter map + body. Returns
     {:ok skill-map}      on success
     {:error reason}      when required fields are missing/blank."
  [front body source ^java.io.File f]
  (let [name-v        (normalize-string (:name front))
        description-v (normalize-string (:description front))
        missing       (cond-> []
                        (blank-string? name-v)        (conj :name)
                        (blank-string? description-v) (conj :description))]
    (if (seq missing)
      {:error (str "missing required field(s): " (str/join ", " (map name missing)))}
      {:ok {:name        name-v
            :description description-v
            :path        (.getAbsolutePath f)
            :source      source
            :body        (or body "")
            :extra       (apply dissoc front REQUIRED_FIELDS)}})))

(defn- read-skill-file
  "Read and parse a single SKILL.md file. Returns {:ok skill-map}
   on success or {:error reason :path ...} on any failure."
  [source ^java.io.File f]
  (try
    (let [content (slurp f)
          split   (split-frontmatter content)]
      (if (nil? split)
        {:error "missing YAML frontmatter (file does not start with `---`)"
         :path  (.getAbsolutePath f)}
        (let [[yaml-text body]   split
              {:keys [ok error]} (parse-frontmatter yaml-text)]
          (if error
            {:error (str "YAML parse error: " error)
             :path  (.getAbsolutePath f)}
            (let [{built :ok built-error :error} (build-skill-map ok body source f)]
              (if built-error
                {:error built-error :path (.getAbsolutePath f)}
                {:ok built}))))))
    (catch Throwable t
      (tel/log! {:level :warn :id ::read-failed
                 :data  {:path  (.getAbsolutePath f)
                         :error (ex-message t)}})
      {:error (str "I/O error: " (or (ex-message t) (str t)))
       :path  (.getAbsolutePath f)})))

;; ---------------------------------------------------------------------------
;; Directory scan.
;; ---------------------------------------------------------------------------

(defn- skill-files-under
  "List SKILL.md files inside `<root>/.agents/skills/*/`. Returns
   vec of File objects, or [] when the parent doesn't exist."
  [^java.io.File root]
  (let [skills-root (java.io.File. root ".agents/skills")]
    (if (.isDirectory skills-root)
      (vec (for [^java.io.File entry (.listFiles skills-root)
                 :when (.isDirectory entry)
                 :let  [skill-md (java.io.File. entry "SKILL.md")]
                 :when (.isFile skill-md)]
             skill-md))
      [])))

(defn- scan-source
  "Scan one source root for SKILL.md files. Returns
     {:loaded   [<skill-map> ...]
      :warnings [{:source :reason :path} ...]}."
  [source ^java.io.File root]
  (let [files   (skill-files-under root)
        results (mapv (partial read-skill-file source) files)
        loaded  (vec (keep :ok results))
        errors  (vec (for [{:keys [error path]} results
                           :when error]
                       {:source :skill-frontmatter
                        :reason error
                        :path   path}))]
    {:loaded loaded :warnings errors}))

(defn scan-with-roots
  "Scan repo + user-global roots for skills. Public for testing
   against fixture roots; production code calls [[scan]].

   Repo skills win silently over user-global on `:name` collision
   (plan §1 Q6)."
  [^java.io.File repo-root ^java.io.File user-global-root]
  (let [repo          (when repo-root (scan-source :repo repo-root))
        user          (when user-global-root (scan-source :user-global user-global-root))
        repo-loaded   (or (:loaded repo) [])
        user-loaded   (or (:loaded user) [])
        repo-names    (set (map :name repo-loaded))
        user-filtered (vec (remove #(contains? repo-names (:name %)) user-loaded))
        loaded        (->> (concat repo-loaded user-filtered)
                        (sort-by :name)
                        vec)
        warnings      (vec (concat (or (:warnings repo) [])
                             (or (:warnings user) [])))]
    {:loaded loaded :warnings warnings}))

(defn- repo-cwd ^java.io.File []
  (workspace/cwd))

(defn- user-home ^java.io.File []
  (java.io.File. ^String (System/getProperty "user.home")))

(defn scan
  "Scan the cwd's repo skills + the current user's user-global
   skills. See [[scan-with-roots]] for the testable form."
  []
  (scan-with-roots (repo-cwd) (user-home)))

;; ---------------------------------------------------------------------------
;; Cache. defonce so the atom survives a `(require :reload)` during
;; an extension reload (per plan caveat). Keyed on (cwd, $HOME) so
;; cd or HOME change invalidates implicitly. Explicit invalidation
;; via [[reload!]].
;; ---------------------------------------------------------------------------

(defonce ^:private state
  (atom {:key nil :loaded nil :warnings nil}))

(defn- cache-key []
  [(try (.getCanonicalPath ^java.io.File (repo-cwd))
     (catch Throwable _
       (or workspace/*workspace-root*
         (System/getProperty "user.dir"))))
   (System/getProperty "user.home")])

(defn current
  "Return the cached scan result. Recomputes on cwd or HOME change."
  []
  (let [k (cache-key)]
    (if (= k (:key @state))
      @state
      (let [{:keys [loaded warnings]} (scan)
            v {:key k :loaded loaded :warnings warnings}]
        (reset! state v)
        v))))

(defn reload!
  "Invalidate and recompute the cached scan. Returns a summary
   compatible with the internal `(reload-skills!)` sandbox binding:
     {:scanned N :loaded M :dropped K :warnings [...]}"
  []
  (let [{:keys [loaded warnings]} (scan)
        scanned  (+ (count loaded) (count warnings))
        v        {:key (cache-key) :loaded loaded :warnings warnings}]
    (reset! state v)
    {:scanned  scanned
     :loaded   (count loaded)
     :dropped  (count warnings)
     :warnings warnings}))

;; ---------------------------------------------------------------------------
;; Public helpers used by the internal prompt assembler, sandbox
;; bindings, doctor, and auto-skill activation.
;; ---------------------------------------------------------------------------

(defn list-all
  "Vec of skill maps, alphabetical by `:name`. Empty when no skills
   are found anywhere. Drives both the internal `<skills>` block in the system prompt
   and the sandbox `(skills)` discovery fn."
  []
  (or (:loaded (current)) []))

(defn list-summaries
  "Vec of compact skill-summary maps `{:name :description :path :source
   :extra?}`. Same shape the retired `TURN_ACCESSIBLE_SKILLS` SYSTEM var
   carried; now surfaced lazily through the sandbox `(skills)` fn so the
   model only pays the cost when it actually queries. Bodies are NOT
   included - load via `(load-skill! name)` (the activation step).

   Degrades to `[]` on any scan failure so a partial filesystem never
   throws into the sandbox."
  []
  (try
    (->> (list-all)
      (mapv (fn [s]
              (cond-> (select-keys s [:name :description :path :source])
                (seq (:extra s)) (assoc :extra (:extra s))))))
    (catch Throwable _ [])))

(defn lookup
  "Find one skill by `:name`. Always returns a map; `:found?` flag
   discriminates present vs absent (plan Q6). Used by the internal `(load-skill! \"name\")` sandbox binding -
   the activation step that materialises the SKILL.md body into a
   sandbox value (TURN_ACCESSIBLE_SKILLS carries only the summary)."
  [^String skill-name]
  (if-let [s (some #(when (= skill-name (:name %)) %) (list-all))]
    (assoc s :found? true)
    {:found? false :name skill-name}))

(defn scan-warnings
  "Vec of warning maps for malformed SKILL.md files. Empty when
   clean."
  []
  (or (:warnings (current)) []))

(defn sandbox-bindings
  "Return internal sandbox bindings for skill activation. These are host
   primitives, not extension symbols: `(load-skill! \"name\")` loads the
   full body and records it in `active-skills-atom` for the next
   `<active_skills>` trailer; `(reload-skills!)` cache-busts the scanner.
   `(load-skill \"name\")` remains as a compatibility alias, but prompts
   teach only the bang form because it mutates the active-skill set."
  [active-skills-atom]
  (let [load-skill!* (fn load-skill! [skill-name]
                       (let [result (lookup skill-name)]
                         (when (and (:found? result) (string? (:name result)) active-skills-atom)
                           (swap! active-skills-atom assoc (:name result) result))
                         result))]
    {'load-skill! load-skill!*
     'load-skill  load-skill!*
     'reload-skills!
     (fn reload-skills! []
       (reload!))
     ;; (skills) - lazy replacement for the retired TURN_ACCESSIBLE_SKILLS
     ;; SYSTEM var. Returns the same compact summary vec; the model uses
     ;; plain Clojure (filter, some, map) over the result.
     'skills
     (fn skills [] (list-summaries))}))
