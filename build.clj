(ns build
  "Build script for vis: ONE jar at `com.blockether/vis`, plus separate
   jars for every classpath plug-in under `extensions/`.

   Earlier this monorepo published three host packages (vis-sdk, vis-runtime,
   vis-main). They've been merged into a single namespace at
   `src/com/blockether/vis/core.clj` shipped as `com.blockether/vis`.

   Tasks
   =====

     clojure -T:build jar              # build every jar
     clojure -T:build install          # build + install all into ~/.m2
     clojure -T:build deploy           # build + deploy all to Clojars
     clojure -T:build clean            # delete target/

     clojure -T:build jar     :package vis-channel-tui    # one only
     clojure -T:build install :package vis-channel-tui
     clojure -T:build deploy  :package vis-channel-tui

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
  "Every publishable jar in the monorepo, in dependency-friendly order.
   The repo-root `vis` package goes first; every classpath plug-in
   depends on `com.blockether/vis` and ships in its own jar."
  [{:lib 'com.blockether/vis                        :dir "."}
   {:lib 'com.blockether/vis-persistance-sqlite     :dir "extensions/persistance/vis-persistance-sqlite"}
   {:lib 'com.blockether/vis-provider-github-copilot :dir "extensions/providers/vis-provider-github-copilot"}
   {:lib 'com.blockether/vis-channel-telegram       :dir "extensions/channels/vis-channel-telegram"}
   {:lib 'com.blockether/vis-channel-tui            :dir "extensions/channels/vis-channel-tui"}
   {:lib 'com.blockether/vis-exa                    :dir "extensions/common/vis-exa"}])

(def ^:private sibling-versions
  "Map of every monorepo lib -> mvn coord at the shared version. Passed
   as `:override-deps` to each per-package basis so `:local/root` sibling
   deps are emitted into the published POM as `<dependency>` entries
   referencing Clojars artifacts instead of pointing at relative paths."
  (into {} (map (fn [{:keys [lib]}] [lib {:mvn/version version}])) packages))

(defn- pkg-by-name
  "Resolve a `:package` selector (short name) to a package descriptor.
   Throws with the available list when missing."
  [pkg-name]
  (or (some (fn [{:keys [lib] :as p}]
              (when (= pkg-name (name lib)) p))
        packages)
    (throw (ex-info (str "Unknown :package '" pkg-name "'. Available: "
                      (str/join ", " (map (comp name :lib) packages)))
             {:package pkg-name :available (mapv (comp name :lib) packages)}))))

(defn- target-paths
  "All build artifacts for a single package live under
   `target/<short-name>/`."
  [{:keys [lib]}]
  (let [short    (name lib)
        cls-dir  (str "target/" short "/classes")
        jar-file (format "target/%s/%s-%s.jar" short short version)]
    {:class-dir cls-dir :jar-file jar-file}))

;; =============================================================================
;; POM data
;; =============================================================================

(def ^:private base-pom-data
  "Fields shared by every published POM."
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
  {'com.blockether/vis
   "vis - single-namespace SDK + iteration runtime + binary entry point."
   'com.blockether/vis-persistance-sqlite
   "SQLite backend for the vis persistence facade."
   'com.blockether/vis-provider-github-copilot
   "GitHub Copilot OAuth device-flow provider."
   'com.blockether/vis-channel-tui
   "Lanterna-based TUI channel."
   'com.blockether/vis-channel-telegram
   "Telegram bot channel."
   'com.blockether/vis-exa
   "Exa MCP web/code search tools for the Vis SCI sandbox."})

(defn- build-pom-data [lib]
  (into [[:description (or (get package-descriptions lib)
                         (str lib " - vis monorepo package."))]]
    base-pom-data))

;; =============================================================================
;; Per-package build
;; =============================================================================

(defn- override-siblings
  "Walk one `:deps` (or `:extra-deps`) map and replace every sibling
   `:local/root` coord with the matching `:mvn/version` coord."
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
  [pkg]
  (b/create-basis
    {:project (read-package-deps (:dir pkg))}))

(defn- src-dirs
  [{:keys [dir]}]
  (let [src (str dir "/src")
        res (str dir "/resources")]
    (cond-> [src]
      (.exists (io/file res)) (conj res))))

(defn- install-local!
  [{:keys [lib class-dir jar-file]}]
  (dd/deploy {:installer :local
              :artifact  jar-file
              :pom-file  (b/pom-path {:lib lib :class-dir class-dir})}))

(defn- build-one!
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
    (b/jar {:class-dir class-dir :jar-file jar-file})
    (let [result {:lib lib :class-dir class-dir :jar-file jar-file}]
      (install-local! result)
      (println "  ->" jar-file "(installed to ~/.m2)")
      result)))

(defn- selected-packages
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
  "Build a jar for every package (or just `:package` if given)."
  [opts]
  (println "Building" (count (selected-packages opts)) "package(s) at version" version)
  (doseq [pkg (selected-packages opts)]
    (println "[" (name (:lib pkg)) "]")
    (build-one! pkg)))

(defn install
  "Build + install every package into the local Maven repo (`~/.m2`)."
  [opts] (jar opts))

(defn deploy
  "Build, install locally, then deploy every package to Clojars."
  [opts]
  (doseq [pkg (selected-packages opts)]
    (println "[" (name (:lib pkg)) "] deploy")
    (let [{:keys [lib class-dir jar-file]} (build-one! pkg)]
      (dd/deploy {:installer :remote
                  :artifact  jar-file
                  :pom-file  (b/pom-path {:lib lib :class-dir class-dir})})
      (println "  -> deployed" lib version "to Clojars"))))
