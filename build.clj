(ns build
  "One build script, every package.

   Each package under `packages/` is published as its own
   `com.blockether/<name>` jar. Sibling `:local/root` deps are rewritten
   to `:mvn/version` coords pointing at the SAME shared version (read
   from the repo-root `VERSION` file) so the published POMs reference
   each other through Clojars instead of dangling at relative paths.

   Tasks
   =====

     clojure -T:build jar              # build every package's jar
     clojure -T:build install          # build + install all into ~/.m2
     clojure -T:build deploy           # build + deploy all to Clojars
     clojure -T:build clean            # delete target/

     clojure -T:build jar     :package vis-tui   # one package only
     clojure -T:build install :package vis-tui
     clojure -T:build deploy  :package vis-tui

   The `:package` selector matches `:lib` short name (after the slash)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

;; =============================================================================
;; Version
;; =============================================================================

(def version
  "Single source of truth for the published version. The `VERSION` env
   var (set by CI on tag pushes) wins; otherwise the repo-root VERSION
   file is read and tagged `-SNAPSHOT` for non-release builds."
  (let [env (System/getenv "VERSION")
        env (when env (if (str/starts-with? env "v") (subs env 1) env))]
    (or env (str (str/trim (slurp "VERSION")) "-SNAPSHOT"))))

;; =============================================================================
;; Package catalog
;; =============================================================================

(def packages
  "Every publishable package in the monorepo, in dependency-friendly
   order (deeper deps first so a sequential install fills `~/.m2`
   before any later package needs them)."
  [{:lib 'com.blockether/vis-extension                 :dir "packages/vis-extension"}
   {:lib 'com.blockether/vis-commandline               :dir "packages/vis-commandline"}
   {:lib 'com.blockether/vis-persistance               :dir "packages/vis-persistance"}
   {:lib 'com.blockether/vis-persistance-sqlite        :dir "packages/vis-persistance-sqlite"}
   {:lib 'com.blockether/vis-persistance-sqlite-flyway :dir "packages/vis-persistance-sqlite-flyway"}
   {:lib 'com.blockether/vis-provider                  :dir "packages/vis-provider"}
   {:lib 'com.blockether/vis-logging                   :dir "packages/vis-logging"}
   {:lib 'com.blockether/vis-core                      :dir "packages/vis-core"}
   {:lib 'com.blockether/vis-telegram                  :dir "packages/vis-telegram"}
   {:lib 'com.blockether/vis-tui                       :dir "packages/vis-tui"}])

(def ^:private sibling-versions
  "Map of every monorepo lib → mvn coord at the shared version. Passed
   as `:override-deps` to each per-package basis so `:local/root` sibling
   deps are emitted into the published POM as `<dependency>` entries
   referencing Clojars artifacts instead of pointing at relative paths."
  (into {} (map (fn [{:keys [lib]}] [lib {:mvn/version version}])) packages))

(defn- pkg-by-name
  "Resolve a `:package` selector (short name, e.g. `\"vis-tui\"`) to a
   package descriptor. Throws with the available list when missing."
  [pkg-name]
  (or (some (fn [{:keys [lib] :as p}]
              (when (= pkg-name (name lib)) p))
        packages)
    (throw (ex-info (str "Unknown :package '" pkg-name "'. Available: "
                      (str/join ", " (map (comp name :lib) packages)))
             {:package pkg-name :available (mapv (comp name :lib) packages)}))))

(defn- target-paths
  "All build artifacts for a single package live under
   `target/<short-name>/` so cleaning, jarring, and deploying don't
   collide between packages."
  [{:keys [lib]}]
  (let [short    (name lib)
        cls-dir  (str "target/" short "/classes")
        jar-file (format "target/%s/%s-%s.jar" short short version)]
    {:class-dir cls-dir :jar-file jar-file}))

;; =============================================================================
;; POM data
;; =============================================================================

(def ^:private base-pom-data
  "Fields shared by every published POM. Per-package `:description` is
   merged in by `build-pom-data`."
  [[:url "https://github.com/Blockether/vis"]
   [:licenses
    [:license
     [:name "Apache License, Version 2.0"]
     [:url "https://www.apache.org/licenses/LICENSE-2.0"]]]
   [:scm
    [:url "https://github.com/Blockether/vis"]
    [:connection "scm:git:https://github.com/Blockether/vis.git"]
    [:developerConnection "scm:git:ssh://git@github.com/Blockether/vis.git"]]])

(def ^:private package-descriptions
  "Per-package one-line descriptions used in the published POM."
  {'com.blockether/vis-core
   "vis runtime — conversation/iteration loop, SCI sandbox, CLI dispatcher."
   'com.blockether/vis-commandline
   "Reusable command-line primitives — command tree, dispatch, help rendering."
   'com.blockether/vis-extension
   "Plug-in spec — extension + channel registries with classpath autodiscovery."
   'com.blockether/vis-logging
   "Telemere → persistence facade log handler (dynaload, no JDBC)."
   'com.blockether/vis-persistance
   "Persistence facade — backend-agnostic API + spec, no database drivers."
   'com.blockether/vis-persistance-sqlite
   "SQLite backend for the vis persistence facade — JDBC plumbing only, migrator-agnostic."
   'com.blockether/vis-persistance-sqlite-flyway
   "Flyway migrator for the SQLite backend — applies vis-persistance's V*__schema.sql files."
   'com.blockether/vis-provider
   "Provider auth (currently GitHub Copilot OAuth device flow)."
   'com.blockether/vis-tui
   "Lanterna-based TUI channel — registers as `vis channel tui`."
   'com.blockether/vis-telegram
   "Telegram bot channel — registers as `vis channel telegram`."})

(defn- build-pom-data [lib]
  (into [[:description (or (get package-descriptions lib)
                         (str lib " — vis monorepo package."))]]
    base-pom-data))

;; =============================================================================
;; Per-package build
;; =============================================================================

(defn- override-siblings
  "Walk one `:deps` (or `:extra-deps`) map and replace every sibling
   `:local/root` coord with the matching `:mvn/version` coord. Non-
   sibling entries pass through untouched. We pre-process instead of
   relying on `:override-deps` because tools.deps tries to validate
   `:local/root` paths BEFORE applying overrides, and our package
   dirs sit one level deep — so the resolver looks for siblings up at
   the repo root and barfs."
  [deps]
  (into {}
    (map (fn [[lib coord]]
           (if (and (contains? sibling-versions lib)
                 (map? coord)
                 (:local/root coord))
             [lib (get sibling-versions lib)]
             [lib coord])))
    deps))

(defn- read-package-deps
  "Read `<dir>/deps.edn` and rewrite every sibling `:local/root` dep
   (top-level `:deps` AND aliases' `:extra-deps`) to a Maven coord at
   the shared version. The result is fed straight to `b/create-basis`
   as `:project`, so the generated POM lists Clojars-installable
   dependencies."
  [dir]
  (let [edn (-> (str dir "/deps.edn") slurp read-string)]
    (cond-> edn
      (:deps edn)    (update :deps override-siblings)
      (:aliases edn) (update :aliases
                       (fn [aliases]
                         (update-vals aliases
                           (fn [a]
                             (cond-> a
                               (:extra-deps a) (update :extra-deps override-siblings)))))))))

(defn- package-basis
  "Build the basis for one package. The standard `:root` (which carries
   Maven Central + Clojars repos + the bundled clojure version) and
   `:user` sources stay in place; only the `:project` slot is replaced
   with the package's pre-rewritten deps map so `b/create-basis`
   resolves dependencies exactly the way a Clojars consumer would."
  [pkg]
  (b/create-basis
    {:project (read-package-deps (:dir pkg))}))

(defn- src-dirs
  "Source roots to copy into the jar. `resources/` is included only when
   the package actually ships one (vis-tui, vis-telegram, and
   vis-persistance-sqlite do; the others don't)."
  [{:keys [dir]}]
  (let [src (str dir "/src")
        res (str dir "/resources")]
    (cond-> [src]
      (.exists (io/file res)) (conj res))))

(defn- write-version-resource!
  "vis-core embeds the resolved version into its jar as
   `META-INF/vis/VERSION` so `com.blockether.vis.config/version` can
   `(io/resource ...)` it at runtime."
  [class-dir]
  (let [target (io/file class-dir "META-INF/vis/VERSION")]
    (io/make-parents target)
    (spit target version)))

(defn- install-local!
  "Install one already-built package into `~/.m2` so subsequent
   `package-basis` calls in the same chain can resolve it as a Maven
   coord. This is what makes the monorepo `jar` / `deploy` chain work
   without round-tripping through Clojars: each package is published
   to the local Maven repo as soon as its jar is on disk, and the
   next package's basis resolution finds it there."
  [{:keys [lib class-dir jar-file]}]
  (dd/deploy {:installer :local
              :artifact  jar-file
              :pom-file  (b/pom-path {:lib lib :class-dir class-dir})}))

(defn- build-one!
  "Build a single package: clean → POM → copy sources → jar → install
   to `~/.m2`. The local install is REQUIRED so the next package in
   the dependency-friendly order can find this one when its basis
   resolves. Returns `{:lib :class-dir :jar-file}`."
  [{:keys [lib dir] :as pkg}]
  (let [{:keys [class-dir jar-file]} (target-paths pkg)
        basis (package-basis pkg)
        srcs  (src-dirs pkg)]
    (b/delete {:path (str "target/" (name lib))})
    (b/write-pom {:class-dir class-dir
                  :lib       lib
                  :version   version
                  :basis     basis
                  :src-dirs  [(str dir "/src")]
                  :pom-data  (build-pom-data lib)})
    (b/copy-dir {:src-dirs srcs :target-dir class-dir})
    (when (= lib 'com.blockether/vis-core)
      (write-version-resource! class-dir))
    (b/jar {:class-dir class-dir :jar-file jar-file})
    (let [result {:lib lib :class-dir class-dir :jar-file jar-file}]
      (install-local! result)
      (println "  →" jar-file "(installed to ~/.m2)")
      result)))

(defn- selected-packages
  "Pick the package subset to operate on based on a `:package` selector;
   nil means all packages."
  [{:keys [package]}]
  (if package
    [(pkg-by-name package)]
    packages))

;; =============================================================================
;; Public tasks
;; =============================================================================

(defn clean
  "Remove the entire `target/` tree."
  [_]
  (b/delete {:path "target"})
  (println "Cleaned target/"))

(defn jar
  "Build a jar for every package (or just `:package` if given).
   Output: `target/<short-name>/<short-name>-<version>.jar`."
  [opts]
  (println "Building" (count (selected-packages opts)) "package(s) at version" version)
  (doseq [pkg (selected-packages opts)]
    (println "[" (name (:lib pkg)) "]")
    (build-one! pkg)))

(defn install
  "Build + install every package into the local Maven repo (`~/.m2`).
   Identical to `jar` (which already installs as a side-effect of the
   monorepo build chain) — kept as a separate task so the intent reads
   correctly in scripts and CI."
  [opts] (jar opts))

(defn deploy
  "Build, install locally, then deploy every package to Clojars under
   its own coordinate.

   Requires the env vars `CLOJARS_USERNAME` and `CLOJARS_PASSWORD`
   (deps-deploy reads them automatically). Packages are deployed in
   dependency-friendly order so the Clojars-side dep graph is sane
   if a consumer pulls them mid-deploy. A failure stops the chain;
   partial deploys are safe because each package is idempotent on
   its own coordinate (re-deploying the same version is a no-op on
   Clojars)."
  [opts]
  (doseq [pkg (selected-packages opts)]
    (println "[" (name (:lib pkg)) "] deploy")
    (let [{:keys [lib class-dir jar-file]} (build-one! pkg)]
      (dd/deploy {:installer :remote
                  :artifact  jar-file
                  :pom-file  (b/pom-path {:lib lib :class-dir class-dir})})
      (println "  → deployed" lib version "to Clojars"))))
