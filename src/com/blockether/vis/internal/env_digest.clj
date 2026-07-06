(ns com.blockether.vis.internal.env-digest
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
            [com.blockether.vis.internal.extension :as extension]
            [com.blockether.vis.internal.prompt :as prompt]
            [com.blockether.vis.internal.workspace :as workspace])
  (:import [java.io File]
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
          (str/includes? s "windows") :windows
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

        "pwsh"
        :pwsh

        "cmd"
        :cmd

        "powershell"
        :powershell

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

(defn- project-kind
  "Detect project shape from top-level directory listing."
  [cwd]
  (try (let [root
             (File. (str cwd))

             names
             (when (.isDirectory root)
               (into #{}
                     (map (fn [^File f]
                            (.getName f)))
                     (.listFiles root)))]

         (cond (every? names (:polylith monorepo-markers)) :polylith
               (some names (:monorepo monorepo-markers)) :monorepo
               :else :single))
       (catch Throwable _ :single)))

(def ^:private clojure-lang-files #{"deps.edn" "project.clj" "shadow-cljs.edn"})

(defn- primary-language-guess
  "Cheap primary-language heuristic: peek at top-level files for the
   one universally-diagnostic build artifact per language. Internal
   never byte-counts files; focused project-shape helpers own that."
  [cwd]
  (try (let [root
             (File. (str cwd))

             names
             (when (.isDirectory root)
               (into #{}
                     (map (fn [^File f]
                            (.getName f)))
                     (.listFiles root)))]

         (cond (some names clojure-lang-files) :clojure
               (some names ["package.json"]) :typescript
               (some names ["go.mod"]) :go
               (some names ["Cargo.toml"]) :rust
               (some names ["pyproject.toml" "setup.py" "requirements.txt"]) :python
               (some names ["Gemfile"]) :ruby
               (some names ["pom.xml" "build.gradle"]) :java
               :else nil))
       (catch Throwable _ nil)))

(defn- project-digest
  []
  ;; Project guidance (AGENTS.md / CLAUDE.md) ships its content via
  ;; the PROJECT-INSTRUCTIONS system block (see `internal.prompt`), so
  ;; we no longer surface a boolean `:agents-md?` here — the model sees
  ;; the actual rules instead of a stale hint.
  (try (let [cwd
             (.getPath (workspace/cwd))

             kind
             (project-kind cwd)

             primary
             (primary-language-guess cwd)]

         (cond-> {"kind" (name kind)}
           primary
           (assoc "primary_language" (name primary))))
       (catch Throwable _ nil)))

(defn- extensions-digest
  [environment]
  (try (let [active
             (or (prompt/active-extensions environment) [])

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
   deep-merges before render)."
  [environment]
  (let [host
        (host-digest)

        project
        (project-digest)

        exts
        (when environment (extensions-digest environment))]

    (cond-> {}
      host
      (assoc "host" host)

      project
      (assoc "project" project)

      exts
      (assoc "extensions" exts))))

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
