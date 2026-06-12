(ns com.blockether.vis.ext.foundation-harness.discovery
  "Cross-HARNESS discovery of agents + skills — the sibling of the shell
   layer's POSIX compat, for the agent/skill definitions OTHER AI coding
   harnesses (Claude Code, opencode, …) leave on disk.

   An AGENT is a markdown file with YAML-ish `---` frontmatter
   (`name`, `description`, `model`, `tools`) + a body that IS a system
   prompt. A SKILL is a `SKILL.md` (same frontmatter, name+description) in
   its own directory, alongside bundled resource files.

   Discovery is PURE except for the directory scan: `parse-frontmatter`,
   `parse-agent`, `parse-skill-meta`, and `dedup-by-name` take strings and
   are unit-tested without the filesystem; the `discover-*` fns walk the
   known source roots. Precedence is source ORDER, first-name-wins
   (project > user > plugin; Claude before other harnesses)."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

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

(defn- plugin-leaf-dirs
  "Every `<cache>/<plugin>/<version>/<leaf>` directory that exists under the
   Claude Code plugin cache — one per installed plugin/version."
  [leaf]
  (let [cache (dir home ".claude" "plugins" "cache")]
    (when (existing-dir? cache)
      (for [plugin  (.listFiles cache)  :when (existing-dir? plugin)
            version (.listFiles plugin)  :when (existing-dir? version)
            :let [d (io/file version leaf)] :when (existing-dir? d)]
        d))))

(defn agent-dirs
  "Ordered agent source dirs: project → user → plugin caches (Claude Code)."
  []
  (->> (concat [(dir ".claude" "agents")
                (dir home ".claude" "agents")]
         (plugin-leaf-dirs "agents"))
    (filter existing-dir?)))

(defn skill-dirs
  "Ordered skill source dirs: project → user → plugin caches (Claude Code)."
  []
  (->> (concat [(dir ".claude" "skills")
                (dir home ".claude" "skills")]
         (plugin-leaf-dirs "skills"))
    (filter existing-dir?)))

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
      (map #(str (.relativize root (.toPath ^java.io.File %))))
      (sort)
      (vec))))

;; =============================================================================
;; Discovery (filesystem → deduped entries)
;; =============================================================================

(defn discover-agents
  "Parse every agent file across `agent-dirs`, first-name-wins."
  []
  (dedup-by-name
    (for [^java.io.File d (agent-dirs)
          ^java.io.File f (md-files d)
          :let [e (try (parse-agent (slurp f)
                         {:name-default (name-stem (.getName f))
                          :tool :claude
                          :path (.getPath f)})
                    (catch Throwable _ nil))]
          :when e]
      e)))

(defn discover-skills
  "Parse every SKILL.md across `skill-dirs`, first-name-wins, with each
   skill's bundled resource paths attached."
  []
  (dedup-by-name
    (for [^java.io.File d (skill-dirs)
          ^java.io.File f (skill-md-files d)
          :let [sdir (.getParentFile f)
                e (try (some-> (parse-skill-meta (slurp f)
                                 {:name-default (.getName sdir)
                                  :tool :claude
                                  :dir (.getPath sdir)
                                  :path (.getPath f)})
                         (assoc :resources (skill-resources sdir)))
                    (catch Throwable _ nil))]
          :when e]
      e)))

;; =============================================================================
;; Cache + accessors
;; =============================================================================

(defonce ^:private cache (atom nil))

(defn- ensure! []
  (or @cache
    (reset! cache {:agents (vec (discover-agents))
                   :skills (vec (discover-skills))})))

(defn reload!
  "Rescan the source dirs and refresh the cache. Returns `{:agents :skills}`."
  []
  (reset! cache {:agents (vec (discover-agents))
                 :skills (vec (discover-skills))}))

(defn agents [] (:agents (ensure!)))
(defn skills [] (:skills (ensure!)))

(defn agent-by-name [nm] (first (filter #(= nm (:name %)) (agents))))
(defn skill-by-name [nm] (first (filter #(= nm (:name %)) (skills))))
