(ns com.blockether.vis.internal.context.env-digest
  "Slim `\"session_env\"` digest. Internal, not extension-owned. STRING-KEYED —
   crosses the Python boundary as `session[\"env\"]`, so keys AND enum values
   (os/shell/kind/primary_language) are strings, never keywords.

   Produces a bounded map the model reads each iter:

     {\"host\"       {\"os\" \"shell\" \"clock\"}   ; cwd lives in session[\"workspace\"][\"root\"]
      \"project\"    {\"kind\" \"primary_language\"}
      \"extensions\" {\"active_count\" \"aliases\"}}

   Each slice is small (~50 bytes), so the section costs <200 bytes/turn.
   Extensions deep-merge their own slices via `:ext/ctx-fn` returning
   `{\"session_env\" {their-key {…}}}`; the merge happens in
   `ctx-loop/render-block!` so internal owns the base section, extensions
   layer on top.

   Heavy environment scans (full byte-counted language tables, polylith
   brick listings, multi-repo git status) live in the foundation-core
   focused project-shape helpers for explicit deep-dives. The digest never calls
   into extensions — host facts come from `System/getProperty`,
   project shape from a single directory peek. AGENTS.md / CLAUDE.md
   contents ride in their own system block (`internal.prompt`), not here."
  (:require [clojure.string :as str]
            [com.blockether.vis.internal.extension.core :as extension]
            [com.blockether.vis.internal.context.prompt :as prompt]
            [com.blockether.vis.internal.workspace.core :as workspace])
  (:import [java.io File]
           [java.nio.file Files LinkOption]
           [java.time ZoneId ZonedDateTime]
           [java.time.format DateTimeFormatter]))

(defn- normalize-os
  [^String os-name]
  (let [s (some-> os-name
                  str/lower-case)]
    (cond (nil? s) :unknown
          (str/includes? s "mac") :macos
          (str/includes? s "darwin") :macos
          (str/includes? s "linux") :linux
          (str/includes? s "bsd") :bsd
          :else :unknown)))

(defn- normalize-shell
  [^String shell]
  (when shell
    (let [base (some-> shell
                       str/lower-case
                       (str/split #"/")
                       last
                       not-empty)]
      (case base
        "zsh"
        :zsh

        "bash"
        :bash

        "fish"
        :fish

        "sh"
        :sh

        (when base (keyword base))))))

(defn- iso-clock
  []
  (try (let [zone (ZoneId/systemDefault)]
         (.format DateTimeFormatter/ISO_OFFSET_DATE_TIME (ZonedDateTime/now zone)))
       (catch Throwable _ nil)))

(defn- host-digest
  "Cheap host facts via `System/getProperty` + `System/getenv`. Workspace
   root (when bound) wins over the JVM `user.dir`, so the model sees
   the active workspace cwd instead of the shell from which Vis launched."
  []
  (try (let [os
             (normalize-os (System/getProperty "os.name"))

             shell
             (normalize-shell (System/getenv "SHELL"))

             clock
             (iso-clock)]

         ;; No "cwd" — it duplicates session["workspace"]["root"] (both the
         ;; active workspace dir). The model reads the cwd from the workspace block.
         (cond-> {"os" (name os)}
           shell
           (assoc "shell" (name shell))

           clock
           (assoc "clock" clock)))
       (catch Throwable _ nil)))

(def ^:private monorepo-markers
  "Lightweight monorepo heuristic. Polylith repos have a top-level
   `bases/` + `components/` pair. Anything else with a `packages/`,
   `apps/`, `modules/`, or `crates/` is treated as `:monorepo`. The
   slim digest never enumerates package count — the deep-dive
   focused project-shape helpers cover that."
  {:polylith #{"bases" "components"} :monorepo #{"packages" "apps" "modules" "crates"}})

(def ^:private clojure-lang-files #{"deps.edn" "project.clj" "shadow-cljs.edn"})

(def ^:private language-markers
  "Diagnostic build files in check order: the first language with a present file wins."
  [[:clojure clojure-lang-files] [:typescript #{"package.json"}] [:go #{"go.mod"}]
   [:rust #{"Cargo.toml"}] [:python #{"pyproject.toml" "setup.py" "requirements.txt"}]
   [:ruby #{"Gemfile"}] [:java #{"pom.xml" "build.gradle"}]])

(def ^:private marker-names
  (into (reduce into #{} (vals monorepo-markers)) (mapcat second) language-markers))

(def ^:private ^"[Ljava.nio.file.LinkOption;" no-follow
  (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))

(defn- exact-entry?
  "True when directory `root` has an entry named exactly `entry-name`, as its
   listing shows it. A case-insensitive file system also finds another case, so
   the real name decides. A link, also a broken one, counts by its own name:
   only then is the listing read, through the `listing` delay."
  [^File root listing ^String entry-name]
  (let [f
        (File. root entry-name)

        path
        (.toPath f)]

    (cond (not (Files/exists path no-follow)) false
          (Files/isSymbolicLink path) (contains? @listing entry-name)
          :else (= entry-name (.getName (.getCanonicalFile f))))))

(defn- present-markers
  "The names of `marker-names` that are top-level entries of `cwd`, or nil when
   `cwd` is not a directory. Checks each name, so a large directory without
   linked markers is never listed."
  [cwd]
  (let [root (File. (str cwd))]
    (when (.isDirectory root)
      (let [listing (delay (set (.list root)))]
        (into #{} (filter #(exact-entry? root listing %)) marker-names)))))

(defn- project-kind
  "Detect project shape from the top-level marker names in `names`."
  [names]
  (cond (every? names (:polylith monorepo-markers)) :polylith
        (some names (:monorepo monorepo-markers)) :monorepo
        :else :single))

(defn- primary-language-guess
  "Cheap primary-language heuristic: the first language with one of its
   diagnostic build files in `names`. Internal never byte-counts files;
   focused project-shape helpers own that."
  [names]
  (some (fn [[language files]]
          (when (some names files) language))
        language-markers))

(defn- project-digest
  []
  ;; Project guidance (AGENTS.md / CLAUDE.md) ships its content via
  ;; the PROJECT-INSTRUCTIONS system block (see `internal.prompt`), so
  ;; we no longer surface a boolean `:agents-md?` here — the model sees
  ;; the actual rules instead of a stale hint.
  (try (let [cwd
             (.getPath (workspace/cwd))

             names
             (or (try (present-markers cwd) (catch Throwable _ nil)) #{})

             kind
             (project-kind names)

             primary
             (primary-language-guess names)]

         (cond-> {"kind" (name kind)}
           primary
           (assoc "primary_language" (name primary))))
       (catch Throwable _ nil)))

(defn- extensions-digest
  "Count and aliases of `active`, the active extensions."
  [active]
  (try (let [active
             (or active [])

             ;; ext-alias-symbol yields a Clojure SYMBOL — forbidden as a boundary
             ;; value; stringify each alias so the set carries plain strings.
             aliases
             (into (sorted-set) (comp (keep extension/ext-alias-symbol) (map str)) active)]

         {"active_count" (count active) "aliases" aliases})
       (catch Throwable _ nil)))

(defn deep-merge
  "Merge maps recursively. Latter map wins on scalar collision; nested
   maps merge key-by-key. Used to fold extension `:ext/ctx-fn` contributions
   into the internal-base `:session/env`."
  [a b]
  (cond (and (map? a) (map? b)) (merge-with deep-merge a b)
        (some? b) b
        :else a))

(defn base-digest
  "Compose the internal-owned base `:session/env` digest. Extensions
   layer additional slices on top via `:ext/ctx-fn` (`render-block!`
   deep-merges before render). Pass `active`, the active extensions, when
   the caller has resolved them already."
  ([environment]
   (base-digest environment (when environment (prompt/active-extensions environment))))
  ([environment active]
   (let [host
         (host-digest)

         project
         (project-digest)

         exts
         (when environment (extensions-digest active))]

     (cond-> {}
       host
       (assoc "host" host)

       project
       (assoc "project" project)

       exts
       (assoc "extensions" exts)))))

(defn extension-contributions
  "Collect `\"session_env\"` slices from every active extension's `:ext/ctx-fn`
   fn. Returns a single deep-merged map. Extensions that contribute
   under other top-level keys (e.g. their own `\"session_voice\"`) are
   silently ignored here — those go straight onto ctx through the
   broader `:ext/ctx-fn` merge path."
  [environment active-extensions]
  (let [full (extension/ctx-contributions environment (or active-extensions []))]
    (or (get full "session_env") {})))

(defn session-env
  "Top-level helper: compose internal base + extension contributions
   into the final `\"session_env\"` value that the renderer pins into the
   ctx text."
  [environment active-extensions]
  (deep-merge (base-digest environment) (extension-contributions environment active-extensions)))
