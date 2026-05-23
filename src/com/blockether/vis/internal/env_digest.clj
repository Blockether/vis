(ns com.blockether.vis.internal.env-digest
  "Pi-style slim `:session/env` digest. Internal, not extension-owned.

   Produces a bounded map the model reads each iter:

     {:host       {:cwd :os :shell :clock}
      :project    {:kind :primary-language? :extension-count? :agents-md?}
      :extensions {:active-count :aliases}}

   Each slice is small (~50 bytes), so the section costs <200 bytes/turn.
   Extensions deep-merge their own slices via `:ext/ctx` returning
   `{:session/env {their-key {\u2026}}}`; the merge happens in
   `ctx-loop/render-block!` so internal owns the base section, extensions
   layer on top.

   Heavy environment scans (full byte-counted language tables, polylith
   brick listings, multi-repo git status) live in the foundation-core
   `v/snapshot` tool for explicit deep-dives. The digest never calls
   into extensions \u2014 host facts come from `System/getProperty`,
   project shape from a single directory peek, AGENTS.md status from
   `internal.agents`."
  (:require
   [clojure.string :as str]
   [com.blockether.vis.internal.agents :as agents]
   [com.blockether.vis.internal.extension :as extension]
   [com.blockether.vis.internal.prompt :as prompt]
   [com.blockether.vis.internal.workspace :as workspace])
  (:import
   [java.io File]
   [java.time ZoneId ZonedDateTime]
   [java.time.format DateTimeFormatter]))

(defn- normalize-os
  [^String os-name]
  (let [s (some-> os-name str/lower-case)]
    (cond
      (nil? s)                       :unknown
      (str/includes? s "mac")        :macos
      (str/includes? s "darwin")     :macos
      (str/includes? s "linux")      :linux
      (str/includes? s "windows")    :windows
      (str/includes? s "bsd")        :bsd
      :else                          :unknown)))

(defn- normalize-shell
  [^String shell]
  (when shell
    (let [base (some-> shell str/lower-case
                 (str/split #"/")
                 last
                 not-empty)]
      (case base
        "zsh"        :zsh
        "bash"       :bash
        "fish"       :fish
        "sh"         :sh
        "pwsh"       :pwsh
        "cmd"        :cmd
        "powershell" :powershell
        (when base (keyword base))))))

(defn- iso-clock
  []
  (try
    (let [zone (ZoneId/systemDefault)]
      (.format DateTimeFormatter/ISO_OFFSET_DATE_TIME (ZonedDateTime/now zone)))
    (catch Throwable _ nil)))

(defn- host-digest
  "Cheap host facts via `System/getProperty` + `System/getenv`. Workspace
   root (when bound) wins over the JVM `user.dir`, so the model sees
   the active workspace cwd instead of the shell from which Vis launched."
  []
  (try
    (let [cwd   (or workspace/*workspace-root* (System/getProperty "user.dir"))
          os    (normalize-os (System/getProperty "os.name"))
          shell (normalize-shell (System/getenv "SHELL"))
          clock (iso-clock)]
      (cond-> {:cwd cwd :os os}
        shell (assoc :shell shell)
        clock (assoc :clock clock)))
    (catch Throwable _ nil)))

(def ^:private monorepo-markers
  "Lightweight monorepo heuristic. Polylith repos have a top-level
   `bases/` + `components/` pair. Anything else with a `packages/`,
   `apps/`, `modules/`, or `crates/` is treated as `:monorepo`. The
   slim digest never enumerates package count — the deep-dive
   `v/snapshot` tool covers that."
  {:polylith #{"bases" "components"}
   :monorepo #{"packages" "apps" "modules" "crates"}})

(defn- project-kind
  "Detect project shape from top-level directory listing."
  [cwd]
  (try
    (let [root  (File. (str cwd))
          names (when (.isDirectory root)
                  (into #{} (map (fn [^File f] (.getName f))) (.listFiles root)))]
      (cond
        (every? names (:polylith monorepo-markers)) :polylith
        (some names (:monorepo monorepo-markers))   :monorepo
        :else                                       :single))
    (catch Throwable _ :single)))

(def ^:private clojure-lang-files
  #{"deps.edn" "project.clj" "shadow-cljs.edn"})

(defn- primary-language-guess
  "Cheap primary-language heuristic: peek at top-level files for the
   one universally-diagnostic build artifact per language. Internal
   never byte-counts files; that's the deep-dive `v/snapshot` job."
  [cwd]
  (try
    (let [root  (File. (str cwd))
          names (when (.isDirectory root)
                  (into #{} (map (fn [^File f] (.getName f))) (.listFiles root)))]
      (cond
        (some names clojure-lang-files)        :clojure
        (some names ["package.json"])          :typescript
        (some names ["go.mod"])                :go
        (some names ["Cargo.toml"])            :rust
        (some names ["pyproject.toml" "setup.py" "requirements.txt"]) :python
        (some names ["Gemfile"])               :ruby
        (some names ["pom.xml" "build.gradle"]) :java
        :else                                  nil))
    (catch Throwable _ nil)))

(defn- project-digest
  []
  (try
    (let [cwd     (or workspace/*workspace-root* (System/getProperty "user.dir"))
          kind    (project-kind cwd)
          primary (primary-language-guess cwd)
          ag      (try (boolean (:found? (agents/instructions)))
                    (catch Throwable _ false))]
      (cond-> {:kind kind :agents-md? ag}
        primary (assoc :primary-language primary)))
    (catch Throwable _ nil)))

(defn- extensions-digest
  [environment]
  (try
    (let [active  (or (prompt/active-extensions environment) [])
          aliases (into (sorted-set) (keep extension/ext-alias-symbol) active)]
      {:active-count (count active)
       :aliases      aliases})
    (catch Throwable _ nil)))

(defn deep-merge
  "Merge maps recursively. Latter map wins on scalar collision; nested
   maps merge key-by-key. Used to fold extension `:ext/ctx` contributions
   into the internal-base `:session/env`."
  [a b]
  (cond
    (and (map? a) (map? b)) (merge-with deep-merge a b)
    (some? b)               b
    :else                   a))

(defn base-digest
  "Compose the internal-owned base `:session/env` digest. Extensions
   layer additional slices on top via `:ext/ctx` (`render-block!`
   deep-merges before render)."
  [environment]
  (let [host    (host-digest)
        project (project-digest)
        exts    (when environment (extensions-digest environment))]
    (cond-> {}
      host    (assoc :host host)
      project (assoc :project project)
      exts    (assoc :extensions exts))))

(defn extension-contributions
  "Collect `:session/env` slices from every active extension's `:ext/ctx`
   fn. Returns a single deep-merged map. Extensions that contribute
   under other top-level keys (e.g. their own `:session/voice`) are
   silently ignored here \u2014 those go straight onto ctx through the
   broader `:ext/ctx` merge path."
  [environment active-extensions]
  (let [full (extension/ctx-contributions environment (or active-extensions []))]
    (or (:session/env full) {})))

(defn session-env
  "Top-level helper: compose internal base + extension contributions
   into the final `:session/env` value that the renderer pins into the
   ctx text."
  [environment active-extensions]
  (deep-merge (base-digest environment)
    (extension-contributions environment active-extensions)))
