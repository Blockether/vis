(ns com.blockether.vis.ext.foundation.environment.render
  "Render the assembled host/git/languages/monorepo snapshot as the
   `<environment>...</environment>` block injected into the system
   prompt.

   The block is dense and conditionally rendered \u2014 absent git
   repository or empty language scan drops the corresponding lines
   instead of emitting empty placeholders. This keeps the prompt
   small for non-repository sessions and expressive for monorepo
   sessions."
  (:require
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(defn- format-percentage [^double fraction]
  (let [rounded (long (Math/round (* 100.0 fraction)))]
    (str rounded "%")))

(defn- format-bytes [^long bytes]
  (cond
    (< bytes 1024)             (str bytes "B")
    (< bytes (* 1024 1024))    (str (long (Math/round (/ (double bytes) 1024.0))) "KB")
    (< bytes (* 1024 1024 1024)) (str (long (Math/round (/ (double bytes) (* 1024.0 1024.0)))) "MB")
    :else                      (str (long (Math/round (/ (double bytes) (* 1024.0 1024.0 1024.0)))) "GB")))

(defn- top-languages-line [{:keys [languages]} ^long take-n]
  (let [top (take take-n languages)]
    (when (seq top)
      (->> top
        (map (fn [{:keys [language bytes-pct files]}]
               (str language " " (format-percentage bytes-pct) " (" files " files)")))
        (string/join ", ")))))

(defn- monorepo-line [{:keys [shape totals]}]
  (when shape
    (let [parts (for [[kind n] (sort-by (fn [[_ n]] (- (long n))) totals)]
                  (str (name kind) ": " n))]
      (str shape " — " (string/join ", " parts)))))

(defn- relativize-cwd
  "Return how the JVM's `cwd` relates to the git working tree:
   `:root` (cwd == repo root), or a relative subpath, or nil."
  [^String cwd ^String git-root]
  (cond
    (or (nil? cwd) (nil? git-root)) nil
    (= cwd git-root)                :root
    (.startsWith cwd (str git-root "/")) (subs cwd (inc (count git-root)))
    :else                           nil))

(defn render
  "Build the textual `<environment>` block from a snapshot map of
   the shape:

     {:host       <com.blockether.vis.ext.foundation.environment.host/snapshot>
      :git        <com.blockether.vis.ext.foundation.environment.git/snapshot or nil>
      :languages  <com.blockether.vis.ext.foundation.environment.languages/scan>
      :monorepo   <com.blockether.vis.ext.foundation.environment.monorepo/snapshot or nil>}"
  [{:keys [host git languages monorepo]}]
  (let [{:keys [cwd user home shell os-name os-arch os-version locale jvm]} host
        cwd-vs-root      (relativize-cwd cwd (:root git))
        cwd-line         (cond
                           (= cwd-vs-root :root) (str cwd " (= git root)")
                           (string? cwd-vs-root) (str cwd " (in repo, subpath: " cwd-vs-root ")")
                           :else                 cwd)
        platform-line    (str (or os-name "?") " " (or os-version "?")
                           " (" (or os-arch "?") ")"
                           " | shell: " (or shell "unknown")
                           " | locale: " (or locale "?"))
        git-branch-line  (when git
                           (cond
                             (:detached?     git) (str "detached @ " (:detached-sha git))
                             (:branch        git) (:branch git)
                             :else                "(unknown)"))
        git-status-line  (when (and git (not (:status-unavailable? git)))
                           (cond
                             (:clean? git) "clean"
                             :else
                             (let [bits (cond-> []
                                          (pos? (long (or (:modified git) 0)))    (conj (str (:modified git) " modified"))
                                          (pos? (long (or (:added git) 0)))       (conj (str (:added git) " added"))
                                          (pos? (long (or (:changed git) 0)))     (conj (str (:changed git) " changed"))
                                          (pos? (long (or (:removed git) 0)))     (conj (str (:removed git) " removed"))
                                          (pos? (long (or (:missing git) 0)))     (conj (str (:missing git) " missing"))
                                          (pos? (long (or (:untracked git) 0)))   (conj (str (:untracked git) " untracked"))
                                          (pos? (long (or (:conflicting git) 0))) (conj (str (:conflicting git) " conflicting")))]
                               (if (seq bits)
                                 (str "dirty (" (string/join ", " bits) ")")
                                 "clean"))))
        flags-line       (when git
                           (str "submodules: " (boolean (:submodules? git))
                             " | worktree: " (boolean (:worktree? git))))
        languages-line   (top-languages-line languages 5)
        monorepo-line*   (monorepo-line monorepo)
        scan-suffix      (when languages
                           (str " (scanned " (:total-files languages) " files, "
                             (format-bytes (long (or (:total-bytes languages) 0)))
                             (when (:truncated? languages) ", truncated")
                             ", " (:elapsed-ms languages) "ms)"))
        lines (cond-> ["<environment>"
                       (str "  cwd: " cwd-line)
                       (str "  user: " user " (home: " home ")")
                       (str "  platform: " platform-line)
                       (str "  jvm: " jvm)]

                git              (conj (str "  git.root: " (:root git)))
                git-branch-line  (conj (str "  git.branch: " git-branch-line))
                git-status-line  (conj (str "  git.status: " git-status-line))
                flags-line       (conj (str "  git: " flags-line))

                (and languages (seq (:languages languages)))
                (conj (str "  languages: " languages-line scan-suffix))

                (and languages (:primary languages))
                (conj (str "  primary-language: " (:primary languages)))

                monorepo-line*   (conj (str "  monorepo: " monorepo-line*)))]
    (str (string/join "\n" (conj lines "</environment>")) "\n")))

;; ---------------------------------------------------------------------------
;; Project guidance + skills + scan-warnings blocks. Rendered
;; alongside <environment> by the foundation aggregator's prompt fn.
;; Each format-*-block is a pure fn from data → string-or-nil; the
;; caller (environment-prompt) drops nil blocks (conditional render).
;; ---------------------------------------------------------------------------

(defn format-project-guidance-block
  "Render the `<project-guidance>` block from an instructions map
   shaped like `(agents/instructions)`. Returns the XML string when
   `:found?` is true, nil otherwise. See plan Q3 + Q4."
  [{:keys [found? source path content]}]
  (when found?
    (str "<project-guidance source=\"" (name source) "\""
      " path=\"" path "\">\n"
      content
      (when-not (.endsWith ^String content "\n") "\n")
      "</project-guidance>")))

(def ^:const SKILLS_PROMPT_BUDGET_BYTES
  "Total byte cap for the rendered <skills> block. Plan Q7: skills
   alphabetized; full descriptions; `+ N more skills not shown
   (prompt budget). Enumerate full list via (vis/skills).` marker
   when truncated."
  8192)

(defn- skill-line
  "One-line skill entry: `name [source]: description`."
  [{:keys [name source description]}]
  (str "  " name " [" (clojure.core/name (or source :unknown)) "]: " description))

(defn format-skills-block
  "Render the `<skills>` block. Skills come pre-sorted from
   `(skills/list-all)` (alphabetical by `:name`). Honors
   SKILLS_PROMPT_BUDGET_BYTES; remaining skills are dropped from the
   prompt index but still callable via `(vis/skill ...)`.
   Returns nil when the catalog is empty."
  [skills]
  (let [skills (vec skills)]
    (when (seq skills)
      (let [tail-line "  Load full body via (vis/skill \"name\"). Enumerate all via (vis/skills)."
            header    (str "<skills count=\"" (count skills) "\">")
            footer    "</skills>"
            ;; Greedily fit lines under the byte budget. Each line ~UTF-8.
            taken     (loop [acc []  used (+ (count header) 1 (count tail-line) 1 (count footer))
                             remain skills]
                        (if (empty? remain)
                          [acc 0]
                          (let [line  (skill-line (first remain))
                                size  (inc (count line))]
                            (if (and (seq acc) (> (+ used size) SKILLS_PROMPT_BUDGET_BYTES))
                              [acc (count remain)]
                              (recur (conj acc line) (+ used size) (rest remain))))))
            [lines dropped] taken
            trunc     (when (pos? dropped)
                        (str "  + " dropped " more skills not shown (prompt budget). "
                          "Enumerate full list via (vis/skills)."))
            body      (->> (cond-> lines
                             trunc (conj "" trunc)
                             true  (conj "" tail-line))
                        (string/join "\n"))]
        (str header "\n" body "\n" footer)))))

(defn- scan-warning-line
  [{:keys [path reason]}]
  (str "  " path ": " reason))

(defn format-scan-warnings-block
  "Render the `<scan-warnings>` block from a vec of warning maps
   like `[{:source :reason :path}]`. Returns nil when warnings is
   empty (conditional render — don't emit an empty block).
   See plan Q10."
  [warnings]
  (when (seq warnings)
    (let [header (str "<scan-warnings count=\"" (count warnings) "\">")
          lines  (mapv scan-warning-line warnings)]
      (str header "\n"
        (string/join "\n" lines)
        "\n</scan-warnings>"))))
