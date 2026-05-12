(ns com.blockether.vis.ext.foundation.environment.render
  "Render the assembled host/git/languages/monorepo snapshot as the
   `<environment>...</environment>` block injected into the system
   prompt.

   The block is dense and conditionally rendered - absent git
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
      (str shape " - " (string/join ", " parts)))))

(defn- yes-no [v]
  (cond
    (true? v)  "yes"
    (false? v) "no"
    :else      "unknown"))

(defn- git-change-bits [{:keys [modified added changed removed missing untracked conflicting]}]
  (cond-> []
    (pos? (long (or modified 0)))    (conj (str modified " modified"))
    (pos? (long (or added 0)))       (conj (str added " added"))
    (pos? (long (or changed 0)))     (conj (str changed " changed"))
    (pos? (long (or removed 0)))     (conj (str removed " removed"))
    (pos? (long (or missing 0)))     (conj (str missing " missing"))
    (pos? (long (or untracked 0)))   (conj (str untracked " untracked"))
    (pos? (long (or conflicting 0))) (conj (str conflicting " conflicting"))))

(defn- git-status-line [git]
  (when (and git (not (:status-unavailable? git)))
    (if (:clean? git)
      "clean"
      (let [bits (git-change-bits git)]
        (if (seq bits)
          (str "dirty (" (string/join ", " bits) ")")
          "clean")))))

(defn- git-summary-line [{:keys [dirty? changes? stale? stash-count ahead behind upstream status-unavailable?]}]
  (let [stash-count (long (or stash-count 0))]
    (str "stale: " (yes-no stale?)
      (when (or ahead behind upstream)
        (str " (upstream: " (or upstream "?")
          ", ahead: " (long (or ahead 0))
          ", behind: " (long (or behind 0)) ")"))
      " | changes: " (if status-unavailable? "unknown" (yes-no changes?))
      " | dirty: " (if status-unavailable? "unknown" (yes-no dirty?))
      " | stash: " (if (pos? stash-count) "yes" "no") " (" stash-count ")")))

(defn- repository-line [{:keys [path branch detached? detached-sha] :as repo}]
  (let [branch-part (cond
                      detached? (str "detached @ " detached-sha)
                      branch    branch
                      :else     "unknown")]
    (str "  - " path ": " branch-part "; " (git-summary-line repo))))

(defn- repositories-lines [{:keys [count repositories truncated?]}]
  (when (> (long (or count 0)) 1)
    (concat [(str "  repositories: " count " git repos"
               (when truncated? " (truncated)"))]
      (map repository-line repositories))))

(defn- relativize-cwd
  "Return how the JVM's `cwd` relates to the git working tree:
   `:root` (cwd == repo root), or a relative subpath, or nil."
  [^String cwd ^String git-root]
  (cond
    (or (nil? cwd) (nil? git-root)) nil
    (= cwd git-root)                :root
    (.startsWith cwd (str git-root "/")) (subs cwd (inc (count git-root)))
    :else                           nil))

(defn- attr-str
  [v]
  (-> (str v)
    (string/replace "&" "&amp;")
    (string/replace "\"" "&quot;")
    (string/replace "<" "&lt;")
    (string/replace ">" "&gt;")))

(defn render
  "Build the textual `<environment>` block from a snapshot map of
   the shape:

     {:host       <com.blockether.vis.ext.foundation.environment.host/snapshot>
      :git        <com.blockether.vis.ext.foundation.environment.git/snapshot or nil>
      :languages  <com.blockether.vis.ext.foundation.environment.languages/scan>
      :monorepo   <com.blockether.vis.ext.foundation.environment.monorepo/snapshot or nil>}"
  [{:keys [host git languages monorepo repositories]}]
  (let [{:keys [cwd user home shell os-name os-arch os-version locale jvm
                time timezone]} host
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
        git-status-line* (git-status-line git)
        git-summary      (when git (git-summary-line git))
        flags-line       (when git
                           (str "submodules: " (boolean (:submodules? git))
                             " | worktree: " (boolean (:worktree? git))))
        languages-line   (top-languages-line languages 5)
        monorepo-line*   (monorepo-line monorepo)
        repositories*    (repositories-lines repositories)
        scan-suffix      (when languages
                           (str " (scanned " (:total-files languages) " files, "
                             (format-bytes (long (or (:total-bytes languages) 0)))
                             (when (:truncated? languages) ", truncated")
                             ", " (:elapsed-ms languages) "ms)"))
        time-line        (str "<current_time timezone=\"" (attr-str (or timezone "?")) "\">"
                           (attr-str (or time "?"))
                           "</current_time>")
        lines (cond-> ["<environment>"
                       (str "  cwd: " cwd-line)
                       (str "  user: " user " (home: " home ")")
                       (str "  " time-line)
                       (str "  platform: " platform-line)
                       (str "  jvm: " jvm)]

                git               (conj (str "  git.root: " (:root git)))
                git-branch-line   (conj (str "  git.branch: " git-branch-line))
                git-status-line*  (conj (str "  git.status: " git-status-line*))
                git-summary       (conj (str "  git.summary: " git-summary))
                flags-line        (conj (str "  git: " flags-line))

                repositories*     (into repositories*)

                (and languages (seq (:languages languages)))
                (conj (str "  languages: " languages-line scan-suffix))

                (and languages (:primary languages))
                (conj (str "  primary-language: " (:primary languages)))

                monorepo-line*    (conj (str "  monorepo: " monorepo-line*)))]
    (str (string/join "\n" (conj lines "</environment>")) "\n")))

;; ---------------------------------------------------------------------------
;; Project guidance + scan-warnings blocks. Rendered alongside
;; <environment> by the foundation aggregator's prompt fn.
;; Each format-*-block is a pure fn from data -> string-or-nil; the caller
;; (environment-prompt) drops nil blocks (conditional render).
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

(defn- scan-warning-line
  [{:keys [path reason]}]
  (str "  " path ": " reason))

(defn format-scan-warnings-block
  "Render the `<scan-warnings>` block from a vec of warning maps
   like `[{:source :reason :path}]`. Returns nil when warnings is
   empty (conditional render - don't emit an empty block).
   See plan Q10."
  [warnings]
  (when (seq warnings)
    (let [header (str "<scan-warnings count=\"" (count warnings) "\">")
          lines  (mapv scan-warning-line warnings)]
      (str header "\n"
        (string/join "\n" lines)
        "\n</scan-warnings>"))))
