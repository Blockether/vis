(ns com.blockether.vis.ext.foundation-harness.discovery
  "Cross-HARNESS discovery of agents + skills — the sibling of the shell
   layer's POSIX compat, for the agent/skill definitions vis' OWN project dir
   and OTHER AI coding harnesses (Claude Code, pi, opencode, the agents
   standard, …) leave on disk.

   An AGENT is a markdown file with YAML-ish `---` frontmatter
   (`name`, `description`, `model`, `tools`) + a body that IS a system
   prompt. A SKILL is a `SKILL.md` (same frontmatter, name+description) in
   its own directory, alongside bundled resource files.

   Discovery is PURE except for the directory scan: `parse-frontmatter`,
   `parse-agent`, `parse-skill-meta`, and `dedup-by-name` take strings and
   are unit-tested without the filesystem; the `discover-*` fns walk the
   known source roots. Precedence is source ORDER, first-name-wins
   (vis project-local > other harnesses' project > user > plugin; Vis and
   Claude before pi/agents/opencode)."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.blockether.vis.internal.paths :as paths]
   [com.blockether.vis.internal.workspace :as workspace]))

;; =============================================================================
;; Frontmatter parsing — minimal, no YAML dependency
;; =============================================================================

(defn parse-frontmatter
  "Split a markdown doc into `{:meta {kw str} :body str}`. A leading
   `---`-fenced block is parsed as `key: value` lines; a line with no
   `key:` head CONTINUES the previous value (folded multi-line description).
   No frontmatter → `{:meta {} :body <whole>}`. Keys are lower-cased keywords."
  [content]
  (let [content (str content)]
    (if-let [[_ fm body] (re-find #"(?s)\A---\r?\n(.*?)\r?\n---\r?\n?(.*)\z" content)]
      {:meta (loop [lines (str/split-lines fm), k nil, acc {}]
               (if (empty? lines)
                 acc
                 (let [line (first lines)]
                   (if-let [[_ key val] (re-matches #"\s*([A-Za-z][\w-]*)\s*:\s*(.*)" line)]
                     (let [kw (keyword (str/lower-case key))]
                       (recur (rest lines) kw (assoc acc kw (str/trim val))))
                     (if (and k (not (str/blank? line)))
                       (recur (rest lines) k (update acc k #(str/trim (str % " " (str/trim line)))))
                       (recur (rest lines) k acc))))))
       :body (str/triml body)}
      {:meta {} :body (str/triml content)})))

(defn- non-blank [s] (let [s (some-> s str str/trim)] (when-not (str/blank? s) s)))

;; =============================================================================
;; Pure entry builders (string in, entry out — testable without the fs)
;; =============================================================================

(defn parse-agent
  "Build an agent entry from raw markdown `content`. `name-default` (the
   filename stem) backs a missing frontmatter `name`. Returns nil when there
   is no usable name. `tool`/`path` are provenance, carried through verbatim."
  [content {:keys [name-default tool path]}]
  (let [{:keys [meta body]} (parse-frontmatter content)
        nm (or (non-blank (:name meta)) (non-blank name-default))]
    (when nm
      {:name        nm
       :description (or (non-blank (:description meta)) "")
       :model       (non-blank (:model meta))
       :tools       (non-blank (:tools meta))
       :body        (str body)
       :tool        tool
       :path        path})))

(defn parse-skill-meta
  "Build a skill entry (sans `:resources`) from a SKILL.md `content`.
   `name-default` is the skill directory name. `dir`/`tool`/`path` are
   provenance. Returns nil when there is no usable name."
  [content {:keys [name-default tool dir path]}]
  (let [{:keys [meta body]} (parse-frontmatter content)
        nm (or (non-blank (:name meta)) (non-blank name-default))]
    (when nm
      {:name        nm
       :description (or (non-blank (:description meta)) "")
       :body        (str body)
       :dir         dir
       :tool        tool
       :path        path
       :resources   []})))

(defn dedup-by-name
  "First occurrence of each `:name` wins (precedence = input order)."
  [entries]
  (->> entries
    (reduce (fn [[seen out] e]
              (if (contains? seen (:name e))
                [seen out]
                [(conj seen (:name e)) (conj out e)]))
      [#{} []])
    second))

;; =============================================================================
;; Source roots (filesystem)
;; =============================================================================

(def ^:private home (System/getProperty "user.home"))

(defn- dir ^java.io.File [& parts] (apply io/file parts))

(defn- existing-dir? [^java.io.File d] (and d (.isDirectory d)))

(defn- project-root
  "The active WORKSPACE root (falls back to process cwd outside a turn
   binding). `:rel` sources resolve against this, NOT the process cwd —
   they must follow the session's workspace."
  ^java.io.File []
  (try (.getCanonicalFile (workspace/cwd))
    (catch Throwable _ (io/file (System/getProperty "user.dir")))))

(defn- git-boundary
  "Nearest ancestor of `start` (inclusive) containing `.git`, or nil
   when not inside a repository."
  ^java.io.File [^java.io.File start]
  (loop [d start]
    (when d
      (if (.exists (io/file d ".git")) d (recur (.getParentFile d))))))

(defn- walk-dirs
  "`<d>/<parts…>` for `d` = workspace root, then each ancestor, up to
   the git repo root (or the filesystem root when not in a repo) —
   nearest first, so nearer definitions win the name dedup. pi-style
   `.agents/skills` ancestor discovery."
  [parts]
  (let [start (project-root)
        stop  (git-boundary start)]
    (loop [d start, acc []]
      (if (nil? d)
        acc
        (let [acc (conj acc (apply dir d parts))]
          (if (and stop (= d stop))
            acc
            (recur (.getParentFile d) acc)))))))

(defn- plugin-leaf-dirs
  "Every `<cache>/<plugin>/<version>/<leaf>` directory that exists under the
   Claude Code plugin cache — one per installed plugin/version."
  [leaf]
  (let [cache (dir home ".claude" "plugins" "cache")]
    (when (existing-dir? cache)
      (for [plugin  (.listFiles ^java.io.File cache)  :when (existing-dir? plugin)
            version (.listFiles ^java.io.File plugin)  :when (existing-dir? version)
            :let [d (io/file version leaf)] :when (existing-dir? d)]
        d))))

;; ── cross-HARNESS source registry (extensible) ───────────────────────────────
;; Each spec is `[tool kind & parts]`; `resolve-source` expands it to existing
;; `[tool ^File dir]` pairs. Precedence = ORDER (project → user → plugins; Claude
;; before opencode). Supporting another harness is one more row — no code change.
(def agent-sources
  [[:vis      :rel     ".vis" "agents"]         ; vis project-local (highest precedence)
   [:claude   :rel     ".claude" "agents"]      ; project
   [:claude   :home    ".claude" "agents"]      ; user
   [:claude   :plugins "agents"]                ; installed plugin caches
   [:pi       :rel     ".pi" "agents"]          ; pi project
   [:pi       :home    ".pi" "agent" "agents"]  ; pi user
   [:agents   :rel-walk ".agents" "agents"]     ; agents-standard project (+ ancestors up to git root)
   [:agents   :home    ".agents" "agents"]      ; agents-standard user
   [:opencode :rel     ".opencode" "agent"]     ; opencode project (singular `agent`)
   [:opencode :home    ".config" "opencode" "agent"]])

(def skill-sources
  [[:vis      :rel     ".vis" "skills"]         ; vis project-local (highest precedence)
   [:claude   :rel     ".claude" "skills"]
   [:claude   :home    ".claude" "skills"]
   [:claude   :plugins "skills"]
   [:pi       :rel     ".pi" "skills"]          ; pi project
   [:pi       :home    ".pi" "agent" "skills"]  ; pi user (~/.pi/agent/skills)
   [:agents   :rel-walk ".agents" "skills"]     ; agents-standard project (+ ancestors up to git root)
   [:agents   :home    ".agents" "skills"]      ; agents-standard user (~/.agents/skills)
   [:opencode :rel     ".opencode" "skill"]
   [:opencode :home    ".config" "opencode" "skill"]])

(def known-tools
  "Every harness tag a source row can carry — the closed set discovery emits."
  #{:vis :claude :pi :agents :opencode})

(defn- resolve-source
  "Expand a `[tool kind & parts]` spec into existing `[tool ^File dir]` pairs.
   `:rel` resolves against the active WORKSPACE root (absolute specs pass
   through untouched); `:rel-walk` additionally walks the root's ancestors
   up to the git repo root, nearest first."
  [[tool kind & parts]]
  (->> (case kind
         :rel     (let [^java.io.File f (apply dir parts)]
                    [(if (.isAbsolute f) f (io/file (project-root) (str f)))])
         :rel-walk (walk-dirs parts)
         :home    [(apply dir home parts)]
         :plugins (plugin-leaf-dirs (first parts))
         [])
    (filter existing-dir?)
    (map (fn [d] [tool d]))))

(defn agent-dirs
  "Ordered `[tool ^File dir]` pairs for agents (existing dirs only)."
  []
  (mapcat resolve-source agent-sources))

(defn skill-dirs
  "Ordered `[tool ^File dir]` pairs for skills (existing dirs only)."
  []
  (mapcat resolve-source skill-sources))

(defn- md-files
  "Direct `*.md` children of `d`, name-sorted."
  [^java.io.File d]
  (->> (.listFiles d)
    (filter #(and (.isFile ^java.io.File %)
               (str/ends-with? (.getName ^java.io.File %) ".md")))
    (sort-by #(.getName ^java.io.File %))))

(defn- skill-md-files
  "`<d>/<skill>/SKILL.md` for each immediate subdir of `d`, path-sorted."
  [^java.io.File d]
  (->> (.listFiles d)
    (filter existing-dir?)
    (map #(io/file % "SKILL.md"))
    (filter #(.isFile ^java.io.File %))
    (sort-by #(.getPath ^java.io.File %))))

(defn- name-stem [^String filename]
  (str/replace filename #"\.md\z" ""))

(defn- skill-resources
  "Relative paths of every file in a skill dir EXCEPT SKILL.md — the bundled
   resources the model reads with the existing file tools. Recursive, bounded."
  [^java.io.File skill-dir]
  (let [root (.toPath skill-dir)]
    (->> (file-seq skill-dir)
      (filter #(.isFile ^java.io.File %))
      (remove #(= "SKILL.md" (.getName ^java.io.File %)))
      (map #(paths/unixify (.relativize root (.toPath ^java.io.File %))))
      (sort)
      (vec))))

;; =============================================================================
;; Discovery (filesystem → deduped entries)
;; =============================================================================

(defn discover-agents
  "Parse every agent file across `agent-dirs`, first-name-wins, tagged by tool."
  []
  (dedup-by-name
    (for [[tool ^java.io.File d] (agent-dirs)
          ^java.io.File f (md-files d)
          :let [e (try (parse-agent (slurp f)
                         {:name-default (name-stem (.getName f))
                          :tool tool
                          :path (.getPath f)})
                    (catch Throwable _ nil))]
          :when e]
      e)))

(defn discover-skills
  "Parse every SKILL.md across `skill-dirs`, first-name-wins, with each
   skill's bundled resource paths attached."
  []
  (dedup-by-name
    (for [[tool ^java.io.File d] (skill-dirs)
          ^java.io.File f (skill-md-files d)
          :let [sdir (.getParentFile f)
                e (try (some-> (parse-skill-meta (slurp f)
                                 {:name-default (.getName sdir)
                                  :tool tool
                                  :dir (.getPath sdir)
                                  :path (.getPath f)})
                         (assoc :resources (skill-resources sdir)))
                    (catch Throwable _ nil))]
          :when e]
      e)))

;; =============================================================================
;; Cache + accessors — marker-revalidated, so a skill/agent added (or edited)
;; mid-session is picked up without a process restart. The marker is a cheap
;; stat pass over the candidate files; content is re-parsed only on change.
;; =============================================================================

(defonce ^:private cache (atom nil))

(defn- file-mark [^java.io.File f]
  [(.getPath f) (.lastModified f) (.length f)])

(defn- source-marker []
  {:root   (.getPath (project-root))
   :agents (vec (for [[tool ^java.io.File d] (agent-dirs)
                      ^java.io.File f (md-files d)]
                  [tool (file-mark f)]))
   :skills (vec (for [[tool ^java.io.File d] (skill-dirs)
                      ^java.io.File f (skill-md-files d)]
                  [tool (file-mark f)]))})

(defn- ensure! []
  (let [m (source-marker)
        c @cache]
    (if (and c (= m (:marker c)))
      c
      (reset! cache {:marker m
                     :agents (vec (discover-agents))
                     :skills (vec (discover-skills))}))))

(defn reload!
  "Rescan the source dirs and refresh the cache. Returns `{:agents :skills}`."
  []
  (reset! cache nil)
  (select-keys (ensure!) [:agents :skills]))

(defn agents [] (:agents (ensure!)))
(defn skills [] (:skills (ensure!)))

(defn agent-by-name [nm] (first (filter #(= nm (:name %)) (agents))))
(defn skill-by-name [nm] (first (filter #(= nm (:name %)) (skills))))
