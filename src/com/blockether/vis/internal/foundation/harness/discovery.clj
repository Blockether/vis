(ns com.blockether.vis.internal.foundation.harness.discovery
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
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.blockether.fff :as fff]
            [com.blockether.vis.internal.fff-index :as fff-index]
            [com.blockether.vis.internal.util :as util]
            [com.blockether.vis.internal.workspace :as workspace]))

;; Frontmatter parsing — minimal, no YAML dependency

(defn- fold-value
  "Normalize one folded frontmatter value. Frontmatter is YAML-ISH, not YAML:
   a block-scalar indicator (`description: >`, `|`, `>-`) only OPENS the fold,
   so it must not survive as the first word of the description, and a quoted
   scalar is unwrapped once the whole fold is joined."
  [v]
  (let [v (str/trim (str/replace (str/trim (str v)) #"\A[>|][-+]?\d*(\s+|\z)" ""))]
    (if (and (>= (count v) 2) (#{\" \'} (first v)) (= (first v) (last v)))
      (str/trim (subs v 1 (dec (count v))))
      v)))

(defn parse-frontmatter
  "Split a markdown doc into `{:meta {kw str} :body str}`. A leading
   `---`-fenced block is parsed as `key: value` lines; a line with no
   `key:` head CONTINUES the previous value (folded multi-line description).
   Values pass through `fold-value`. No frontmatter → `{:meta {} :body
   <whole>}`. Keys are lower-cased keywords."
  [content]
  (let [content (str content)]
    (if-let [[_ fm body] (re-find #"(?s)\A---\r?\n(.*?)\r?\n---\r?\n?(.*)\z" content)]
      {:meta
       (update-vals
         (loop [lines (str/split-lines fm)
                k nil
                acc {}]

           (if (empty? lines)
             acc
             (let [line (first lines)]
               (if-let [[_ key val] (re-matches #"\s*([A-Za-z][\w-]*)\s*:\s*(.*)" line)]
                 (let [kw (keyword (str/lower-case key))]
                   (recur (rest lines) kw (assoc acc kw (str/trim val))))
                 (if (and k (not (str/blank? line)))
                   (recur (rest lines) k (update acc k #(str/trim (str % " " (str/trim line)))))
                   (recur (rest lines) k acc))))))
         fold-value)
       :body (str/triml body)}
      {:meta {} :body (str/triml content)})))

;; Pure entry builders (string in, entry out — testable without the fs)

(defn parse-agent
  "Build an agent entry from raw markdown `content`. `name-default` (the
   filename stem) backs a missing frontmatter `name`. Returns nil when there
   is no usable name. `tool`/`path` are provenance, carried through verbatim."
  [content {:keys [name-default tool path]}]
  (let [{:keys [meta body]}
        (parse-frontmatter content)

        nm
        (or (util/non-blank (:name meta)) (util/non-blank name-default))]

    (when nm
      {:name nm
       :description (or (util/non-blank (:description meta)) "")
       :model (util/non-blank (:model meta))
       :tools (util/non-blank (:tools meta))
       :body (str body)
       :tool tool
       :path path})))

(defn parse-skill-meta
  "Build a skill entry (sans `:resources`) from a SKILL.md `content`.
   `name-default` is the skill directory name. `dir`/`tool`/`path` are
   provenance. Returns nil when there is no usable name."
  [content {:keys [name-default tool dir path]}]
  (let [{:keys [meta body]}
        (parse-frontmatter content)

        nm
        (or (util/non-blank (:name meta)) (util/non-blank name-default))]

    (when nm
      {:name nm
       :description (or (util/non-blank (:description meta)) "")
       :body (str body)
       :dir dir
       :tool tool
       :path path
       :resources []})))

(defn dedup-by-name
  "First occurrence of each `:name` wins (precedence = input order)."
  [entries]
  (->> entries
       (reduce (fn [[seen out] e]
                 (if (contains? seen (:name e)) [seen out] [(conj seen (:name e)) (conj out e)]))
               [#{} []])
       second))

;; Source roots (filesystem)

(defn- home
  "`user.home` read per call — a top-level `def` is folded at native-image build time."
  ^String []
  (System/getProperty "user.home"))

(defn- dir ^java.io.File [& parts] (apply io/file parts))

(defn- existing-dir? [^java.io.File d] (and d (.isDirectory d)))

(defn- project-root
  "The active WORKSPACE root (falls back to process cwd outside a turn
   binding). `:rel` sources resolve against this, NOT the process cwd —
   they must follow the session's workspace."
  ^java.io.File []
  (try (.getCanonicalFile (workspace/cwd))
       (catch Throwable _ (io/file (System/getProperty "user.dir")))))


(defn- walk-dirs
  "`<d>/<parts…>` for the active workspace and its project ancestors, nearest
   first, bounded by `workspace/ancestor-roots`."
  [parts]
  (mapv #(apply dir % parts) (workspace/ancestor-roots (project-root))))

(defn- plugin-leaf-dirs
  "Every `<cache>/<plugin>/<version>/<leaf>` directory that exists under the
   Claude Code plugin cache — one per installed plugin/version."
  [leaf]
  (let [cache (dir (home) ".claude" "plugins" "cache")]
    (when (existing-dir? cache)
      (for [plugin (.listFiles ^java.io.File cache)
            :when (existing-dir? plugin)
            version (.listFiles ^java.io.File plugin)
            :when (existing-dir? version)
            :let [d (io/file version leaf)]
            :when (existing-dir? d)]

        d))))

;; ── cross-HARNESS source registry (extensible) ───────────────────────────────
;; Each spec is `[tool kind & parts]`; `resolve-source` expands it to existing
;; `[tool ^File dir]` pairs. Precedence = ORDER (project → user → plugins; Claude
;; before opencode). Supporting another harness is one more row — no code change.
(def agent-sources
  [[:vis :rel-walk ".vis" "agents"]       ; vis project-local (highest precedence)
   [:claude :rel-walk ".claude" "agents"] ; project
   [:claude :home ".claude" "agents"]     ; user
   [:claude :plugins "agents"] ; installed plugin caches
   [:pi :rel-walk ".pi" "agents"]         ; pi project
   [:pi :home ".pi" "agent" "agents"]     ; pi user
   [:agents :rel-walk ".agents" "agents"] ; agents-standard project
   [:agents :home ".agents" "agents"]     ; agents-standard user
   [:opencode :rel-walk ".opencode" "agents"] [:opencode :rel-walk ".opencode" "agent"]
   [:opencode :home ".config" "opencode" "agents"] [:opencode :home ".config" "opencode" "agent"]])

(def skill-sources
  [[:vis :rel-walk ".vis" "skills"] [:claude :rel-walk ".claude" "skills"]
   [:claude :home ".claude" "skills"] [:claude :plugins "skills"] [:pi :rel-walk ".pi" "skills"]
   [:pi :home ".pi" "agent" "skills"] [:agents :rel-walk ".agents" "skills"]
   [:agents :home ".agents" "skills"] [:opencode :rel-walk ".opencode" "skills"]
   [:opencode :rel-walk ".opencode" "skill"] [:opencode :home ".config" "opencode" "skills"]
   [:opencode :home ".config" "opencode" "skill"]])

(def command-sources
  "Cross-HARNESS COMMAND (prompt-template) dirs — markdown prompts a user
   invokes as `/<name> [args]`, `$ARGUMENTS`-substituted. Vis' OWN commands are
   the `.vis/prompts` FILE templates (prompt-templates ns, highest precedence);
   these rows add the OTHER harnesses' command dirs. Precedence = ORDER."
  [[:claude :rel-walk ".claude" "commands"] [:claude :home ".claude" "commands"]
   [:pi :rel-walk ".pi" "commands"] [:pi :home ".pi" "agent" "commands"]
   [:agents :rel-walk ".agents" "commands"] [:agents :home ".agents" "commands"]
   [:opencode :rel-walk ".opencode" "command"] [:opencode :rel-walk ".opencode" "commands"]
   [:opencode :home ".config" "opencode" "command"]
   [:opencode :home ".config" "opencode" "commands"]])

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
         :rel
         (let [^java.io.File f (apply dir parts)]
           [(if (.isAbsolute f) f (io/file (project-root) (str f)))])

         :rel-walk
         (walk-dirs parts)

         :home
         [(apply dir (home) parts)]

         :plugins
         (plugin-leaf-dirs (first parts))

         [])
       (filter existing-dir?)
       (map (fn [d]
              [tool d]))))

(defn agent-dirs
  "Ordered `[tool ^File dir]` pairs for agents (existing dirs only)."
  []
  (mapcat resolve-source agent-sources))

(defn command-dirs
  "Ordered `[tool ^File dir]` pairs for commands (existing dirs only)."
  []
  (mapcat resolve-source command-sources))

(defn- md-files
  "Direct `*.md` children of `d`, name-sorted."
  [^java.io.File d]
  (->> (.listFiles d)
       (filter #(and (.isFile ^java.io.File %) (str/ends-with? (.getName ^java.io.File %) ".md")))
       (sort-by #(.getName ^java.io.File %))))

(defn- skill-md-files
  "`<d>/<skill>/SKILL.md` for each immediate subdir of `d`, path-sorted."
  [^java.io.File d]
  (->> (.listFiles d)
       (filter existing-dir?)
       (map #(io/file % "SKILL.md"))
       (filter #(.isFile ^java.io.File %))
       (sort-by #(.getPath ^java.io.File %))))

(defn- name-stem [^String filename] (str/replace filename #"\.md\z" ""))

(def ^:private skill-resource-max-depth
  "Depth bound for the bundled-resource walk. `fff/list-directory` takes a
   POSITIVE bound (its own `0` lists the immediate children only), and no skill
   nests resources anywhere near this deep."
  64)

(defn- skill-resources
  "Relative paths of every file in a skill dir EXCEPT SKILL.md — the bundled
   resources the model reads with the existing file tools.

   ONE stateless `fff/list-directory` walk: the same Rust engine every other
   traversal in vis goes through, with no `Fff` instance, no index and no pool
   slot to evict the workspace's own. Hidden files are included and ignore files
   are OFF — a bundled resource belongs to its skill even when the repository
   ignores its path, which is exactly how a nested project's skill ships."
  [^java.io.File skill-dir]
  (->> (fff/list-directory
         (.getPath skill-dir)
         {:max-depth skill-resource-max-depth :include-hidden? true :respect-ignore-files? false})
       (remove :dir?)
       (map :relative-path)
       (remove #(= "SKILL.md" (peek (str/split % #"/"))))
       (sort)
       (vec)))
;; Discovery (filesystem → deduped entries)

(defn discover-agents
  "Parse every agent file across `agent-dirs`, first-name-wins, tagged by tool."
  []
  (dedup-by-name (for [[tool ^java.io.File d]
                       (agent-dirs)

                       ^java.io.File f
                       (md-files d)

                       :let [e
                             (try (parse-agent (slurp f)
                                               {:name-default (name-stem (.getName f))
                                                :tool tool
                                                :path (.getPath f)})
                                  (catch Throwable _ nil))]
                       :when e]

                   e)))

(defn- repository-root
  "Nearest Git root for the active workspace, or nil outside a repository."
  ^java.io.File []
  (when-let [^java.io.File root (last (workspace/ancestor-roots (project-root)))]
    (let [^java.io.File canonical-root (.getCanonicalFile root)]
      (when (.exists (io/file canonical-root ".git")) canonical-root))))

(defn- nested-skill-files
  "Project-skill files below a repository-root session, discovered through Vis' one
   pooled fff index. Git ignores stay enabled for the tree; only the explicit harness
   configuration directories are reopened. Nested app sessions deliberately do not
   scan siblings."
  []
  (let [^java.io.File repo
        (repository-root)

        ^java.io.File active-root
        (project-root)

        ^java.io.File active
        (.getCanonicalFile active-root)]

    (when (and repo (= (.getPath repo) (.getPath active)))
      (let [specs
            (->> skill-sources
                 (filter #(= :rel-walk (second %)))
                 distinct
                 vec)

            harnesses
            (map #(nth % 2) specs)

            overlay
            {:unignore-globs (vec (distinct (mapcat (fn [h]
                                                      [(str h "/**") (str "**/" h "/**")])
                                                    harnesses)))}]

        (fff-index/with-index
          [idx (fff-index/lease repo true overlay)]
          (vec (mapcat (fn [[tool _ & parts]]
                         (let [pattern (str "**/" (str/join "/" parts) "/*/SKILL.md")]
                           (for [{:keys [relative-path]}
                                 (sort-by :relative-path
                                          (:items (fff/glob idx
                                                            {:pattern pattern :page-size 10000})))
                                 :let [f (io/file repo relative-path)]
                                 :when (.isFile ^java.io.File f)]

                             [tool f])))
                       specs)))))))

(defn- skill-candidates
  "Ordered `[tool SKILL.md]` candidates. Active/ancestor project definitions win,
   then descendant projects when the session is at the Git root, then user/plugin
   definitions."
  []
  (let [project?
        #(= :rel-walk (second %))

        files-for
        (fn [specs]
          (for [spec
                specs

                [tool ^java.io.File d]
                (resolve-source spec)

                ^java.io.File f
                (skill-md-files d)]

            [tool f]))]

    (concat (files-for (filter project? skill-sources))
            (nested-skill-files)
            (files-for (remove project? skill-sources)))))

(defn- skill-project-root
  "Owning project for a repository-local skill file. The skill directory sits at
   `<project>/<harness>/skills/<name>` (or `skill` for OpenCode). User-level and
   plugin-cache skills return nil and never re-root a turn."
  [^java.io.File skill-md]
  (when-let [^java.io.File repo (repository-root)]
    (let [^java.io.File skill-dir (.getParentFile skill-md)
          ^java.io.File skills-dir (some-> skill-dir
                                           .getParentFile)
          ^java.io.File harness-dir (some-> skills-dir
                                            .getParentFile)
          ^java.io.File owner (some-> harness-dir
                                      .getParentFile
                                      .getCanonicalFile)
          harness-name (some-> harness-dir
                               .getName)
          leaf-name (some-> skills-dir
                            .getName)
          ^java.nio.file.Path owner-path (some-> owner
                                                 .toPath)
          ^java.nio.file.Path repo-path (.toPath repo)]

      (when (and owner-path
                 (contains? #{".vis" ".claude" ".pi" ".agents" ".opencode"} harness-name)
                 (contains? #{"skill" "skills"} leaf-name)
                 (.startsWith owner-path repo-path))
        (.getPath owner)))))

(defn discover-skills
  "Parse every project, nested-project, user, and plugin SKILL.md in precedence
   order. First name wins. Repository-local skills carry `:project-root`, which
   makes their slash-expanded turn execute from the project that owns the skill."
  []
  (dedup-by-name
    (for [[tool ^java.io.File f]
          (skill-candidates)

          :let [sdir
                (.getParentFile f)

                project-root
                (skill-project-root f)

                e
                (try (some-> (parse-skill-meta (slurp f)
                                               {:name-default (.getName sdir)
                                                :tool tool
                                                :dir (.getPath sdir)
                                                :path (.getPath f)})
                             (assoc :resources (skill-resources sdir))
                             (cond->
                               project-root
                               (assoc :project-root project-root)))
                     (catch Throwable _ nil))]
          :when e]

      e)))

(defn discover-commands
  "Parse every command markdown across `command-dirs`, first-name-wins, tagged by
   tool. Each entry's `:body` IS the prompt template (`$ARGUMENTS`-aware at expand
   time); `:model`/`:tools` frontmatter is carried through but currently unused."
  []
  (dedup-by-name (for [[tool ^java.io.File d]
                       (command-dirs)

                       ^java.io.File f
                       (md-files d)

                       :let [e
                             (try (parse-agent (slurp f)
                                               {:name-default (name-stem (.getName f))
                                                :tool tool
                                                :path (.getPath f)})
                                  (catch Throwable _ nil))]
                       :when e]

                   e)))

;; Cache + accessors — marker-revalidated, so a skill/agent added (or edited)
;; mid-session is picked up without a process restart. The marker is a cheap
;; stat pass over the candidate files; content is re-parsed only on change.

(defonce ^:private cache (atom {}))

(defn- file-mark [^java.io.File f] [(.getPath f) (.lastModified f) (.length f)])

(defn- source-marker
  []
  {:root (.getPath (project-root))
   :agents (vec (for [[tool ^java.io.File d]
                      (agent-dirs)

                      ^java.io.File f
                      (md-files d)]

                  [tool (file-mark f)]))
   :skills (vec (for [[tool ^java.io.File f] (skill-candidates)]
                  [tool (file-mark f)]))
   :commands (vec (for [[tool ^java.io.File d]
                        (command-dirs)

                        ^java.io.File f
                        (md-files d)]

                    [tool (file-mark f)]))})

(defn- ensure!
  []
  (let [m
        (source-marker)

        root
        (:root m)

        c
        (get @cache root)]

    (if (and c (= m (:marker c)))
      c
      (let [fresh {:marker m
                   :generation (inc (long (:generation c 0)))
                   :agents (vec (discover-agents))
                   :skills (vec (discover-skills))
                   :commands (vec (discover-commands))}]
        (swap! cache assoc root fresh)
        fresh))))

(defn reload!
  "Rescan every workspace-root cache on its next access, then return the active
   workspace's `{:agents :skills :commands}` discovery result."
  []
  (reset! cache {})
  (select-keys (ensure!) [:agents :skills :commands]))

(defn generation
  "How many times THIS workspace's agents, skills and commands have actually
   been re-read. A cheap freshness token — it costs the stat pass `ensure!`
   already pays and changes exactly when the discovered set would, which is
   what lets the doc corpus memoize entries built from `skills`."
  []
  (:generation (ensure!)))

(defn agents [] (:agents (ensure!)))

(defn skills [] (:skills (ensure!)))

(defn commands [] (:commands (ensure!)))
