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
  (let [env
        (System/getenv "VERSION")

        env
        (when env (if (str/starts-with? env "v") (subs env 1) env))]

    (or env (str (str/trim (slurp "VERSION")) "-SNAPSHOT"))))

;; =============================================================================
;; Package catalog
;; =============================================================================

(def ^:private extension-package-root "extensions")

(defn- extension-package-deps-file?
  [^java.io.File f]
  (let [path (str/replace (.getPath f) "\\" "/")]
    (and (.isFile f)
         (= "deps.edn" (.getName f))
         (some? (re-matches #"extensions/[^/]+/[^/]+/deps\.edn" path)))))

(defn- extension-package-dirs
  "Every extension subproject that declares its own deps.edn. New extension
  packages are publishable automatically — no hard-coded package list to
  remember when adding `extensions/<kind>/<name>/deps.edn`."
  []
  (->> (file-seq (io/file extension-package-root))
       (filter extension-package-deps-file?)
       (map #(-> %
                 .getParentFile
                 .getPath))
       sort))

(defn- extension-dir->package
  [dir]
  {:lib (symbol "com.blockether" (.getName (io/file dir))) :dir dir})

(def packages
  "Every publishable jar in the monorepo. Deploy builds every selected package
  with local-root deps first, then rewrites publish POMs to same-version Maven
  coords and pushes the jars to Clojars. Extension packages are discovered from
  `extensions/**/deps.edn`, so adding a new extension package automatically
  includes it in `jar`, `install`, and `deploy`."
  (into [{:lib 'com.blockether/vis :dir "."}]
        (map extension-dir->package)
        (extension-package-dirs)))

(def ^:private sibling-versions
  "Map of every monorepo lib -> mvn coord at the shared version. Passed
   as `:override-deps` to each per-package basis so `:local/root` sibling
   deps are emitted into the published POM as `<dependency>` entries
   referencing Clojars artifacts instead of pointing at relative paths."
  (into {}
        (map (fn [{:keys [lib]}]
               [lib {:mvn/version version}]))
        packages))

(defn- pkg-by-name
  "Resolve a `:package` selector (short name) to a package descriptor.
   Throws with the available list when missing."
  [pkg-name]
  (let [needle (name pkg-name)]
    (or (some (fn [{:keys [lib] :as p}]
                (when (= needle (name lib)) p))
              packages)
        (throw (ex-info (str "Unknown :package '" pkg-name
                             "'. Available: " (str/join ", " (map (comp name :lib) packages)))
                        {:package pkg-name :available (mapv (comp name :lib) packages)})))))

(defn- target-paths
  "All build artifacts for a single package live under
   `target/<short-name>/`."
  [{:keys [lib]}]
  (let [short
        (name lib)

        cls-dir
        (str "target/" short "/classes")

        jar-file
        (format "target/%s/%s-%s.jar" short short version)]

    {:class-dir cls-dir :jar-file jar-file}))

;; =============================================================================
;; POM data
;; =============================================================================

(def ^:private base-pom-data
  "Fields shared by every published POM."
  [[:url "https://github.com/Blockether/vis"]
   [:licenses
    [:license [:name "Apache License, Version 2.0"]
     [:url "https://www.apache.org/licenses/LICENSE-2.0"]]]
   [:scm [:url "https://github.com/Blockether/vis"]
    [:connection "scm:git:https://github.com/Blockether/vis.git"]
    [:developerConnection "scm:git:ssh://git@github.com/Blockether/vis.git"]]])

(def ^:private package-descriptions
  {'com.blockether/vis "vis - single-namespace SDK + iteration runtime + binary entry point."
   'com.blockether/vis-persistance-sqlite "SQLite backend for the vis persistence facade."
   'com.blockether/vis-provider-github-copilot "GitHub Copilot OAuth device-flow provider."
   'com.blockether/vis-channel-tui "Lanterna-based TUI channel."
   'com.blockether/vis-channel-telegram "Telegram bot channel."
   'com.blockether/vis-foundation-exa "Exa MCP web/code search tools for the Vis SCI sandbox."})

(defn- build-pom-data
  [lib]
  (into [[:description (or (get package-descriptions lib) (str lib " - vis monorepo package."))]]
        base-pom-data))

;; =============================================================================
;; Per-package build
;; =============================================================================

(defn- absolute-local-root
  "Resolve a `:local/root` coord relative to its package deps.edn dir."
  [dir root]
  (let [f (io/file root)]
    (if (.isAbsolute f) (.getCanonicalPath f) (.getCanonicalPath (io/file dir root)))))

(defn- prepare-package-deps
  "Normalize package deps for basis creation. Local roots must be absolute
  because release builds pass an in-memory deps map to tools.deps; publish POMs
  additionally rewrite listed sibling packages to same-version Maven coords."
  [dir publish? deps]
  (into {}
        (map (fn [[lib coord]]
               [lib
                (cond
                  (and publish? (contains? sibling-versions lib) (map? coord) (:local/root coord))
                  (get sibling-versions lib)
                  (and (map? coord) (:local/root coord))
                  (update coord :local/root #(absolute-local-root dir %))
                  :else coord)]))
        deps))

(defn- read-package-deps
  [dir & {:keys [publish?] :or {publish? false}}]
  (let [edn (-> (str dir "/deps.edn")
                slurp
                read-string)]
    (cond-> edn
      (:deps edn)
      (update :deps #(prepare-package-deps dir publish? %))

      (:aliases edn)
      (update :aliases
              (fn [aliases]
                (update-vals aliases
                             (fn [a]
                               (cond-> a
                                 (:extra-deps a)
                                 (update :extra-deps #(prepare-package-deps dir publish? %))))))))))

(defn- package-basis [pkg] (b/create-basis {:project (read-package-deps (:dir pkg))}))

(defn- package-publish-basis
  [pkg]
  (b/create-basis {:project (read-package-deps (:dir pkg) :publish? true)}))

(defn- src-dirs
  [{:keys [dir]}]
  (let [src
        (str dir "/src")

        res
        (str dir "/resources")]

    (cond-> [src]
      (.exists (io/file res))
      (conj res))))

(defn- install-local!
  [{:keys [lib class-dir jar-file]}]
  (dd/deploy
    {:installer :local :artifact jar-file :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))

(defn- write-package-pom!
  [{:keys [lib dir]} class-dir basis]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs [(str dir "/src")]
                :pom-data (build-pom-data lib)}))

(defn- build-one!
  [{:keys [lib] :as pkg}]
  (let [{:keys [class-dir jar-file]}
        (target-paths pkg)

        basis
        (package-basis pkg)

        srcs
        (src-dirs pkg)]

    (b/delete {:path (str "target/" (name lib))})
    (write-package-pom! pkg class-dir basis)
    (b/copy-dir {:src-dirs srcs :target-dir class-dir})
    (b/jar {:class-dir class-dir :jar-file jar-file})
    (let [result {:pkg pkg :lib lib :class-dir class-dir :jar-file jar-file}]
      (install-local! result)
      (println "  ->" jar-file "(installed to ~/.m2)")
      result)))

(defn- selected-packages [{:keys [package]}] (if package [(pkg-by-name package)] packages))

(defn- deploy-build-order
  "Build extension/sibling packages before the root `com.blockether/vis` jar. The
   root publish POM rewrites its `:local/root` extension deps to same-version
   Maven coordinates, so a fresh tag release must have already installed those
   sibling jars into ~/.m2 before `package-publish-basis` resolves the root POM."
  [pkgs]
  (sort-by #(if (= 'com.blockether/vis (:lib %)) 1 0) pkgs))

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
  [opts]
  (jar opts))

(defn deploy
  "Build and install every selected package locally, then deploy them to Clojars.

  The publish POMs rewrite in-repo `:local/root` sibling deps to same-version
  Maven coords. We generate those publish POMs only after the full selected set
  has been installed locally so a fresh release can resolve same-version sibling
  artifacts before they exist on Clojars."
  [opts]
  (let [built (doall (for [pkg (deploy-build-order (selected-packages opts))]
                       (do (println "[" (name (:lib pkg)) "] build") (build-one! pkg))))]
    (doseq [{:keys [pkg lib class-dir jar-file]} built]
      (println "[" (name lib) "] deploy")
      (write-package-pom! pkg class-dir (package-publish-basis pkg))
      (dd/deploy {:installer :remote
                  :artifact jar-file
                  :pom-file (b/pom-path {:lib lib :class-dir class-dir})})
      (println "  -> deployed" lib version "to Clojars"))))

;; =============================================================================
;; GraalVM native-image build
;;
;; The vis CLI (`bin/vis` = `clojure -M:vis`) compiles to a standalone native
;; binary. Pipeline: AOT EVERY namespace (core + every extension — extensions
;; are `require`d at runtime by manifest discovery, so they MUST be in the image)
;; -> uberjar (Main-Class com.blockether.vis.core) -> native-image.
;;
;; Embedded GraalPy + dynamic extension loading make this non-trivial; see the
;; `:native` alias (graal-build-time) and `native-image-args`. Cross-platform:
;; the same `uber`/`native` tasks run on Linux/macOS/Windows; CI matrixes them.
;; =============================================================================

(def ^:private native-class-dir "target/native-classes")
(def ^:private native-uber "target/vis.jar")

(def ^:private uber-exclusions
  "Entry patterns dropped from the portable uberjar. The jar DELIBERATELY keeps
   every platform's JNI libs (sherpa-onnx, onnxruntime, sqlite-jdbc) — it is the
   single cross-OS `vis --jvm` distribution — but the onnxruntime macOS libs drag
   ~16.5 MB of nested *.dSYM DWARF debug bundles along; no runtime reads those."
  ["ai/onnxruntime/native/.*\\.dSYM/.*"
   ;; dep-jar warts: babashka/http-client ships scratch.clj and sci ships
   ;; scratch.cljs at the classpath ROOT of their published jars
   "scratch\\.cljs?"
   ;; oh-my-claudecode agent session state — recreated whenever an agent
   ;; runs with its cwd inside a source tree; must never ship
   ".*\\.omc/.*"])
(def ^:private native-bin
  (str "target/vis"
       (when (str/includes? (str/lower-case (System/getProperty "os.name")) "windows") ".exe")))

(defn- native-image-command
  "The native-image launcher to invoke via `b/process` (Java ProcessBuilder).
   On Windows the GraalVM launcher is a `native-image.cmd` batch script, and
   ProcessBuilder does NOT apply PATHEXT resolution to a bare `native-image` —
   it dies with `CreateProcess error=2, The system cannot find the file
   specified` (exactly why every Windows native build failed while Linux/macOS
   — which have a real `native-image` binary — succeeded). Resolve the concrete
   launcher from GRAALVM_HOME / JAVA_HOME (…/bin/native-image[.cmd]); fall back
   to the platform-correct bare name."
  []
  (let [windows?
        (str/includes? (str/lower-case (System/getProperty "os.name")) "windows")

        exe
        (if windows? "native-image.cmd" "native-image")

        home
        (or (System/getenv "GRAALVM_HOME") (System/getenv "JAVA_HOME"))

        launcher
        (when home (io/file home "bin" exe))]

    (if (and launcher (.isFile launcher)) (.getAbsolutePath launcher) exe)))

;; ── Distribution profiles ───────────────────────────────────────────────────
;; Profiles select what ships, via `:profile` on `native` / `uber`:
;;   :tui       — MINIMAL: the TUI channel only. Web + Telegram channels and
;;                voice are dropped (agent substrate, providers, languages,
;;                persistence all stay — a full agent, single-channel/voiceless).
;;   :cross     — all channels (TUI + web + Telegram), NO voice.
;;   :voice     — all channels + voice ASR (DEFAULT for local builds; the model
;;                still downloads on first use unless :with-assets embeds it).
;; ── Shipped end-user distributions ──
;;   :community — FULL agent: every extension incl. web `search` + voice ASR;
;;                models download on first use (lean binary).
;;   :corporate — AIR-GAPPED: `search` extension dropped entirely + the parakeet
;;                ASR model ALWAYS embedded (no runtime HuggingFace/network fetch).
;; Dropping a dep here removes its whole subtree: vis-foundation-voice is the
;; ONLY way sherpa-onnx/onnxruntime JNI libs reach the classpath, so :tui and
;; :cross contain zero voice natives for ANY platform. Every channel
;; soft-resolves voice fns (web answers 501, Telegram/TUI show 'not loaded'),
;; and channels themselves are manifest-discovered extensions, so no code
;; change is needed to sever them.

(def ^:private profile->dropped-libs
  {:tui #{'com.blockether/vis-foundation-voice
          'com.blockether/vis-channel-telegram}
   :cross #{'com.blockether/vis-foundation-voice}
   :voice #{}
   ;; ── Shipped end-user distributions ──────────────────────────────────────
   ;; :community — the FULL agent: every extension (all channels, voice ASR,
   ;;   AND the web/network `search` extension). Park (parakeet) voice models
   ;;   and any other on-demand assets download on FIRST USE, so the binary
   ;;   stays lean. Same classpath as :voice; named for the distribution.
   :community #{}
   ;; :corporate — the AIR-GAPPED agent for locked-down networks. The `search`
   ;;   extension is dropped ENTIRELY (its namespaces never reach the image, so
   ;;   `search_web`/`search_code`/`search_papers` do not exist and nothing can
   ;;   phone a search API). Corporates typically also forbid pulling models
   ;;   from HuggingFace at runtime, so this profile ALWAYS embeds the parakeet
   ;;   ASR model into the image (see `native` / `voice-profile?`) — the binary
   ;;   is fully offline out of the box.
   :corporate #{'com.blockether/vis-foundation-search}})

(defn- resolve-profile
  [opts]
  (let [p (keyword (or (:profile opts) :voice))]
    (when-not (contains? profile->dropped-libs p)
      (throw (ex-info
               (str "Unknown :profile " p " — use :tui, :cross, :voice, :community or :corporate")
               {:profile p :available (keys profile->dropped-libs)})))
    p))

(defn- voice-profile?
  "True when the voice extension SURVIVES this profile (its lib is not dropped),
   so the sherpa-onnx/onnxruntime JNI libs — and, when embedding assets, the
   parakeet ASR model — belong in the image."
  [profile]
  (not (contains? (profile->dropped-libs profile) 'com.blockether/vis-foundation-voice)))

(defn- embed-assets?
  "Whether to vendor the parakeet ASR model INTO the image. Explicit
   `:with-assets true` requests it; the `:corporate` profile forces it (an
   air-gapped binary must not fetch the model at runtime)."
  [opts profile]
  (boolean (or (:with-assets opts) (= :corporate profile))))

(defn- dropped-lib-roots
  "The `:local/root` dirs of the deps a profile drops — used to keep their
   extension manifests OUT of the merged discovery file."
  [profile]
  (let [deps (:deps (read-string (slurp "deps.edn")))]
    (into #{}
          (keep #(some-> (get deps %)
                         :local/root))
          (profile->dropped-libs profile))))

(defn- root-deps-edn
  "Root deps.edn as an edn map, minus the deps the `profile` drops. Dropping
   the top-level edge alone is NOT enough: every extension declares
   `com.blockether/vis {:local/root ../../..}`, which re-imports the full root
   deps.edn transitively and smuggles the dropped extension (and its natives)
   right back onto the classpath. So the dropped libs are ALSO added as
   `:exclusions` on every remaining dep coordinate — resolution then can't
   reach them from anywhere."
  [profile]
  (let [dropped (profile->dropped-libs profile)]
    (cond-> (read-string (slurp "deps.edn"))
      (seq dropped)
      (update
        :deps
        (fn [deps]
          (into {}
                (map (fn [[lib coord]]
                       [lib
                        (if (map? coord) (update coord :exclusions (fnil into []) dropped) coord)]))
                (apply dissoc deps dropped)))))))

(defn- all-source-roots
  "Every production src/resources dir on the vis classpath: the repo root plus
   each `:local/root` extension. AOT covers all of these so every extension ns
   the runtime manifest scan `require`s is already compiled into the image."
  [profile]
  (let [deps
        (:deps (root-deps-edn profile))

        roots
        (->> deps
             vals
             (keep :local/root))

        dirs
        (into ["src" "resources"]
              (mapcat (fn [r]
                        [(str r "/src") (str r "/resources")])
                      roots))]

    (filterv #(.exists (io/file %)) dirs)))

(defn- merge-extension-manifests!
  "Each extension ships its own `META-INF/vis-extension/vis.edn`. Across many
   jars/dirs `manifest.clj` ENUMERATES them (getResources returns one per
   classpath entry); but in a single uberjar/native image they all collide to
   ONE path and only one survives → only one extension registers. Fix: merge
   every extension's manifest map into ONE combined file in the class-dir.
   `manifest.clj` already iterates a multi-id map, so a single merged resource
   carries every extension id with no runtime change."
  [class-dir profile]
  (let [dropped
        (dropped-lib-roots profile)

        files
        (->> (file-seq (io/file "extensions"))
             (filter (fn [^java.io.File f]
                       ;; normalize separators: on Windows File/toString uses
                       ;; `\`, so a forward-slash substring check would match
                       ;; nothing and silently merge zero manifests.
                       (let [p (str/replace (str f) "\\" "/")]
                         (and (= "vis.edn" (.getName f))
                              (str/includes? p "META-INF/vis-extension")
                              ;; profile-dropped extensions are off the classpath,
                              ;; so their manifests must not be merged — discovery
                              ;; would `require` namespaces not in the image.
                              (not-any? (fn [root]
                                          (str/includes? p (str root "/")))
                                        dropped))))))

        merged
        (reduce (fn [m f]
                  (merge m (read-string (slurp f))))
                {}
                files)

        out
        (io/file class-dir "META-INF" "vis-extension" "vis.edn")]

    (io/make-parents out)
    (spit out (pr-str merged))
    (println "Merged" (count files)
             "extension manifests ->" (count merged)
             "ids:" (str/join " " (sort (map str (keys merged)))))))

(defn- ns-name-of
  "The namespace symbol of a Clojure source string, or nil. Reads the first
   form so metadata before the name (e.g. `(ns ^{:doc …} foo)`) is handled, and
   files loaded via `in-ns` (no `(ns …)` form) are skipped."
  [content]
  (try (with-open [r (java.io.PushbackReader. (java.io.StringReader. content))]
         (binding [*read-eval* false]
           (let [form (read {:read-cond :allow :eof nil} r)]
             (when (and (seq? form) (= 'ns (first form))) (first (filter symbol? (rest form)))))))
       (catch Throwable _ nil)))

(defn- all-source-root-namespaces
  "Every namespace defined under vis + extension source roots. The preload must
   build-time `require` ALL of them: any may be loaded at RUNTIME via
   `requiring-resolve` (e.g. the sqlite backend core, lazily loaded from its
   registrar), and a runtime `require` in a native image RECOMPILES the ns
   (eval → 'Classes cannot be defined at runtime'). Build-time-requiring them
   makes the lazy resolve a no-op. Covers nses that lack a
   `(set! *warn-on-reflection* …)` and aren't manifest entry points."
  [profile]
  (->> (all-source-roots profile)
       (mapcat (fn [d]
                 (->> (file-seq (io/file d))
                      (filter #(and (.isFile ^java.io.File %)
                                    (re-matches #".*\.cljc?$" (.getName ^java.io.File %))))
                      (keep (fn [f]
                              (ns-name-of (slurp f)))))))
       (map str)
       distinct))

(def ^:private warn-on-reflection-re #"\(set!\s+\*(?:warn-on-reflection|unchecked-math)\*")

(defn- preload-namespaces
  "Namespaces whose source has a top-level `(set! *warn-on-reflection* …)` /
   `(set! *unchecked-math* …)`. On GraalVM these must be initialized via `require`
   (which binds the var) BEFORE build-time class init runs their `<clinit>` raw on
   a parallel worker — otherwise the `set!` throws `Can't change/establish root
   binding`. Scans every source dir + dependency jar on the :native classpath.
   The native-image Feature reads this list and requires each one. See
   native-image handling (com.blockether.vis.internal.nativeimage)."
  [basis]
  (let [cljc?
        #(re-matches #".*\.cljc?$" %)

        from-dir
        (fn [d]
          (->> (file-seq (io/file d))
               (filter #(and (.isFile ^java.io.File %) (cljc? (.getName ^java.io.File %))))
               (keep (fn [f]
                       (let [c (slurp f)]
                         (when (re-find warn-on-reflection-re c) (ns-name-of c)))))))

        from-jar
        (fn [jar]
          (with-open [zf (java.util.zip.ZipFile. ^String jar)]
            (doall (->> (enumeration-seq (.entries zf))
                        (filter #(cljc? (.getName ^java.util.zip.ZipEntry %)))
                        (keep (fn [e]
                                (let [c (slurp (.getInputStream zf ^java.util.zip.ZipEntry e))]
                                  (when (re-find warn-on-reflection-re c) (ns-name-of c)))))))))]

    (->> (:classpath-roots basis)
         (mapcat (fn [r]
                   (let [f (io/file r)]
                     (cond (not (.exists f)) nil
                           (str/ends-with? r ".jar") (from-jar r)
                           (.isDirectory f) (from-dir r)
                           :else nil))))
         (remove nil?)
         distinct
         (sort-by str)
         vec)))

;; Built-in extension entry namespaces vis `require`s at RUNTIME via
;; extension/load-builtin-extensions! (they ship in the main jar, not via a
;; classpath manifest). Keep in sync with extension/builtin-extension-nses.
(def ^:private builtin-extension-nses
  ["com.blockether.vis.internal.foundation.core" "com.blockether.vis.internal.foundation.shell"])

(defn- manifest-entry-namespaces
  "Every namespace under `:nses` across the merged extension manifest written by
   `merge-extension-manifests!`. vis `require`s these at runtime
   (manifest/scan-extensions!), so they must be build-time-initialized too."
  [class-dir]
  (let [f (io/file class-dir "META-INF" "vis-extension" "vis.edn")]
    (if (.exists f)
      (->> (read-string (slurp f))
           vals
           (mapcat :nses)
           (map str))
      [])))

(defn- write-preload-namespaces!
  [class-dir basis profile]
  ;; The native Feature `require`s every ns in this list at BUILD time. It must
  ;; cover not just the (set! *warn-on-reflection* …) namespaces, but also every
  ;; namespace vis `require`s at RUNTIME during extension discovery
  ;; (extension/discover-extensions! -> load-builtin-extensions! +
  ;; manifest/scan-extensions!). A runtime `require` in a native image must
  ;; DEFINE the namespace's classes at runtime — forbidden ("Classes cannot be
  ;; defined at runtime", the foundation/core.clj smoke-test crash). Build-time
  ;; initializing them here makes the runtime require a no-op.
  (let [warn
        (map str (preload-namespaces basis))

        srcs
        (all-source-root-namespaces profile)

        exts
        (concat builtin-extension-nses (manifest-entry-namespaces class-dir))

        nses
        (->> (concat warn srcs exts)
             distinct
             sort
             vec)

        out
        (io/file class-dir "META-INF" "vis-native-image" "preload.edn")]

    (io/make-parents out)
    (spit out (pr-str nses))
    (println "Preload list:" (count nses)
             "namespaces (warn-on-reflection + extension entry nses) ->" (str out))))

(defn- write-migration-indexes!
  "Flyway discovers migrations by LISTING its classpath location dir — which
   native-image can't do. For every `**/migration/` dir of `.sql` we copied,
   write an `_index.edn` of filenames so the SQLite backend's `migrate!` can
   serve them by exact path via a ResourceProvider. JVM builds ignore the index."
  [class-dir]
  (let [sql
        (->> (file-seq (io/file class-dir))
             (filter #(and (.isFile ^java.io.File %)
                           (str/ends-with? (.getName ^java.io.File %) ".sql")
                           (str/includes? (str %) "/migration/"))))

        by-dir
        (group-by #(.getParentFile ^java.io.File %) sql)]

    (doseq [[^java.io.File dir files] by-dir]
      (let [names (vec (sort (map #(.getName ^java.io.File %) files)))]
        (spit (io/file dir "_index.edn") (pr-str names))
        (println "Migration index:" (str (io/file dir "_index.edn")) "->" names)))))

(defn- prepare-native-classes!
  "AOT-compile every ns (core + every extension) into `native-class-dir` and copy
   all resources, collapsing the per-extension manifests into ONE merged file.
   Also writes the build-time-init preload list. Shared by `uber` and `native`.
   Returns the `:native`-alias basis."
  [profile]
  (b/delete {:path native-class-dir})
  (let [basis
        (b/create-basis {:project (root-deps-edn profile) :aliases [:native]})

        srcs
        (all-source-roots profile)]

    (println "AOT compiling every ns across" (count srcs)
             "source roots…" (str "(profile " (name profile) ")"))
    ;; copy resources (incl. META-INF/vis-extension + META-INF/native-image)
    (b/copy-dir {:src-dirs srcs :target-dir native-class-dir})
    ;; sweep agent-session state (.omc/) that lands INSIDE source trees when
    ;; an agent runs with its cwd there — copy-dir happily copies it, and it
    ;; once shipped agent-replay transcripts in the uberjar. Deleted here so
    ;; neither the jar nor the image can ever carry it.
    (doseq [^java.io.File f
            (file-seq (io/file native-class-dir))

            :when (and (.isDirectory f) (= ".omc" (.getName f)))]

      (b/delete {:path (.getPath f)})
      (println "Swept agent-state dir from class-dir:" (.getPath f)))
    ;; collapse the per-extension manifests into ONE so discovery finds them all
    (merge-extension-manifests! native-class-dir profile)
    ;; list every namespace the native Feature must require before build-time init
    (write-preload-namespaces! native-class-dir basis profile)
    ;; index Flyway migrations so they're discoverable without dir listing
    (write-migration-indexes! native-class-dir)
    ;; `vis/VERSION` resource (git short sha) so `vis --version` has a value.
    (let [sha
          (try (str/trim (:out (b/process {:command-args ["git" "rev-parse" "--short" "HEAD"]
                                           :out :capture})))
               (catch Throwable _ nil))

          vfile
          (io/file native-class-dir "vis" "VERSION")]

      (io/make-parents vfile)
      (spit vfile (or (not-empty sha) "dev")))
    ;; no :ns-compile => compile EVERY ns found in :src-dirs (extensions included)
    (b/compile-clj {:basis basis :src-dirs srcs :class-dir native-class-dir})
    basis))

(defn uber
  "Build the all-in-one vis uberjar (`target/vis.jar`) with Main-Class
   `com.blockether.vis.core`. Handy for `java -jar target/vis.jar --version` to
   sanity-check the AOT'd app. NOTE: the native build does NOT use this jar —
   GraalPy's polyglot jar declares `ForceOnModulePath`, which a flat uberjar
   (no module-info) breaks; `native` builds from a classpath of real jars."
  [opts]
  (b/delete {:path native-uber})
  (let [basis (prepare-native-classes! (resolve-profile opts))]
    (b/uber {:class-dir native-class-dir
             :uber-file native-uber
             :basis basis
             :main 'com.blockether.vis.core
             :exclude uber-exclusions})
    (println "->" native-uber)))

(defn- native-lib-token
  "Host-platform suffix for the blockether FFM native artifacts
   (`<lib>-native-<token>`): darwin-arm64 / darwin-x64 / linux-x64 / linux-arm64
   / windows-x64."
  []
  (let [os
        (str/lower-case (System/getProperty "os.name"))

        arch
        (str/lower-case (System/getProperty "os.arch"))

        a
        (cond (#{"aarch64" "arm64"} arch) "arm64"
              (#{"x86_64" "amd64" "x64"} arch) "x64"
              :else arch)

        o
        (cond (str/includes? os "mac") "darwin"
              (str/includes? os "win") "windows"
              :else "linux")]

    (str o "-" a)))

(defn- pack-native-token
  "tree-sitter-language-pack native artifact suffix for the build host. The
   pack publishes its OWN rid scheme (macos-arm64 / macos-x86_64 /
   linux-aarch64 / linux-x86_64 / windows-x86_64) — NOT the fff/rift/ruff
   darwin-arm64 style; both verified against the Clojars artifact list."
  []
  (let [os
        (str/lower-case (System/getProperty "os.name"))

        arch
        (str/lower-case (System/getProperty "os.arch"))

        arm?
        (boolean (#{"aarch64" "arm64"} arch))]

    (cond (str/includes? os "mac") (str "macos-" (if arm? "arm64" "x86_64"))
          (str/includes? os "win") "windows-x86_64"
          :else (str "linux-" (if arm? "aarch64" "x86_64")))))

(defn- native-lib-jars
  "Resolve the host-platform native artifacts of the FFM libs (fff / rift /
   ruff `prebuilds/**`, tree-sitter pack `natives/<rid>/**`) — at the SAME
   version as each main jar already in `basis` — so their native libs EMBED
   into the image (fff/rift/ruff carry a `prebuilds/**` glob in their own
   metadata; the pack ships NO glob, so native-image-args adds a host-scoped
   one). ONLY the build host's jar goes on the classpath — that is the whole
   per-platform guarantee: even the broad `prebuilds/**` glob can only match
   host libs. A native image can't download natives, so without this the FFM
   libs fall back to verbatim / fail in the binary. Returns the resolved
   native-jar paths (empty if a lib isn't in the basis or its native jar
   isn't published yet)."
  [basis]
  (let [tok
        (native-lib-token)

        want
        (keep (fn [[main artifact]]
                (when-let [v (some-> basis
                                     :libs
                                     (get main)
                                     :mvn/version)]
                  [(symbol "com.blockether" artifact) {:mvn/version v}]))
              {'com.blockether/fff (str "fff-native-" tok)
               'com.blockether/rift (str "rift-native-" tok)
               'com.blockether/ruff (str "ruff-native-" tok)
               'com.blockether/tree-sitter-language-pack (str "tree-sitter-language-pack-native-"
                                                              (pack-native-token))})

        deps
        (into {} want)

        tree-sitter-native
        (symbol "com.blockether" (str "tree-sitter-language-pack-native-" (pack-native-token)))]

    (when (seq deps)
      (let [b (try (b/create-basis {:project nil :extra {:deps deps}})
                   (catch Exception e (println "WARN native-lib-jars:" (ex-message e)) nil))]
        (let [jars (->> (keys deps)
                        (mapcat #(some-> b
                                         :libs
                                         (get %)
                                         :paths))
                        (filter #(and % (str/ends-with? % ".jar"))))]
          (when (and (contains? deps tree-sitter-native)
                     (not-any? #(str/includes? % (name tree-sitter-native)) jars))
            (throw
              (ex-info
                "Native build requires the tree-sitter native artifact for its target platform."
                {:artifact tree-sitter-native :platform (pack-native-token)})))
          jars)))))

(defn- native-classpath
  "Classpath for the native build: the AOT classes dir FIRST (so compiled app +
   merged manifest win), then every dependency JAR. We deliberately DROP the
   :local/root source/resource dirs — their compiled+copied form already lives in
   `native-class-dir`, and re-adding them would resurrect the per-extension
   manifest collision. Keeping deps as separate jars lets native-image honor each
   jar's module-info + native-image.properties (polyglot's `ForceOnModulePath`,
   GraalPy's build-time init, etc.)."
  [basis]
  (let [jars
        (->>
          (:classpath-roots basis)
          (filter #(str/ends-with? % ".jar"))
          ;; Drop the tools.deps runtime download-fallback (+ its
          ;; cognitect.aws S3 transporter tail) from the NATIVE classpath
          ;; ONLY. A native image bundles/locates natives explicitly and
          ;; never downloads — and cognitect.aws is not native-image-safe
          ;; (objects land in the image heap → build failure). The plain-JVM
          ;; classpath (deps.edn) keeps tools.deps so download still works.
          ;; NOTE the AWS-scoped `/com/cognitect/aws/` (NOT /com/cognitect/):
          ;; the broad form also stripped cognitect/transit-clj, which
          ;; clj-kondo.impl.cache requires — that silently failed the whole
          ;; clj-kondo build-time preload chain and left the language-clojure
          ;; extension UNBOUND in the native binary.
          (remove
            #(re-find
               #"/org/clojure/tools\.deps/|/tools\.deps\.maven-s3-transporter/|/com/cognitect/aws/"
               %)))]
    (->> (concat jars (native-lib-jars basis))
         (into [native-class-dir])
         (str/join java.io.File/pathSeparator))))

(defn- native-platform-token
  "sherpa-onnx / onnxruntime native-lib dir token for the BUILD host
   (e.g. `osx-aarch64`, `linux-x64`, `win-x64`). Both jars use this layout."
  []
  (let [os
        (str/lower-case (System/getProperty "os.name"))

        arch
        (str/lower-case (System/getProperty "os.arch"))

        a
        (cond (#{"aarch64" "arm64"} arch) "aarch64"
              (#{"x86_64" "amd64" "x64"} arch) "x64"
              :else arch)]

    (cond (str/includes? os "mac") (str "osx-" a)
          (str/includes? os "win") (str "win-" a)
          :else (str "linux-" a))))

(defn- truffle-platform-tokens
  "[os arch] the GraalPy/Truffle internal-resource dirs use under
   META-INF/resources/ (verified against python-resources jar layout):
   darwin|linux|windows / aarch64|amd64."
  []
  (let [os
        (str/lower-case (System/getProperty "os.name"))

        arch
        (str/lower-case (System/getProperty "os.arch"))]

    [(cond (str/includes? os "mac") "darwin"
           (str/includes? os "win") "windows"
           :else "linux") (if (#{"aarch64" "arm64"} arch) "aarch64" "amd64")]))

;; Voice (ASR) assets. The model is downloaded on first use by default; the
;; --with-assets build vendors it INTO the image for a fully-offline binary.
(def ^:private voice-model-url
  "https://github.com/k2-fsa/sherpa-onnx/releases/download/asr-models/sherpa-onnx-nemo-parakeet-tdt-0.6b-v3-int8.tar.bz2")
(def ^:private voice-asset-resource-dir "voice-assets/parakeet")
(def ^:private voice-model-cache
  (str (System/getProperty "user.home") "/.vis/build-cache/parakeet.tar.bz2"))

(defn- vendor-voice-model!
  "Download + extract the parakeet ASR model into
   `<class-dir>/voice-assets/parakeet/` so a --with-assets native image embeds it
   (asr.clj extracts it to ~/.vis/models on first run instead of downloading).
   The tar.bz2 is cached under ~/.vis/build-cache so repeat builds don't refetch."
  [class-dir]
  (let [cache
        (io/file voice-model-cache)

        out
        (io/file class-dir voice-asset-resource-dir)]

    (io/make-parents cache)
    (when-not (.isFile cache)
      (println "Vendoring voice model (~465 MB) <-" voice-model-url)
      (with-open [in
                  (io/input-stream (java.net.URI. voice-model-url))

                  os*
                  (io/output-stream cache)]

        (io/copy in os*)))
    (.mkdirs out)
    ;; commons-compress (on the :build classpath) handles the bz2 archive
    (with-open [fis
                (io/input-stream cache)

                bz
                (org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream. fis)

                tar
                (org.apache.commons.compress.archivers.tar.TarArchiveInputStream. bz)]

      (loop []

        (when-let [e (.getNextEntry tar)]
          (when-not (.isDirectory e)
            (let [name (.getName e)
                  base (subs name (inc (.lastIndexOf name "/")))]

              (io/copy tar (io/file out base))))
          (recur))))
    (println "Vendored voice model ->"
             (str out)
             (vec (map #(.getName ^java.io.File %) (.listFiles out))))))

(defn- native-image-args
  "native-image CLI args. Config travels INSIDE the classpath jars
   (META-INF/native-image/…); here we add only classpath/main/output, the
   vis-extension/edn/db resource includes, and the build-host voice native libs
   (sherpa-onnx + onnxruntime JNI dylibs) so voice ASR works in the binary.
   `with-assets?` also embeds the vendored voice model resources; voiceless
   profiles drop every voice resource pattern (the extension is off the
   classpath, so the jars carrying those resources are absent anyway)."
  [basis with-assets? profile]
  (let [tok
        (native-platform-token)

        voice?
        (voice-profile? profile)

        [t-os t-arch]
        (truffle-platform-tokens)

        ;; Extra native-image args spliced from the environment (space-separated).
        ;; Lets CI tune the builder JVM per-runner (e.g. -J-Xmx6g -J-Xms2g to fit a
        ;; RAM-constrained free macOS runner, overriding GraalPy's bundled -Xms14g
        ;; since command-line -J args are applied AFTER the classpath properties).
        extra
        (some-> (System/getenv "VIS_NATIVE_EXTRA_ARGS")
                str/trim
                not-empty
                (str/split #"\s+"))]

    (cond-> ["-cp" (native-classpath basis) "-o" (str/replace native-bin #"\.exe$" "")
             "-H:IncludeResources=META-INF/vis-extension/.*" "-H:IncludeResources=.*\\.edn$"
             ;; the build-written `vis/VERSION` (git sha) read by `vis --version`
             "-H:IncludeResources=vis/VERSION"
             ;; Flyway migration SQL (not in the agent-traced metadata)
             "-H:IncludeResources=db/.*"
             ;; The WHOLE embedded docs corpus (markdown pages + manifest +
             ;; woff2 fonts/logos) — ALL read at RUNTIME via io/resource
             ;; (gateway /docs site AND the model-facing `vis_docs` tool), and
             ;; NONE of it in the agent-traced metadata (the trace never called
             ;; vis_docs), so without this pattern vis_docs returns zero pages
             ;; in the native binary.
             "-H:IncludeResources=vis-docs/.*"
             ;; vendored Prism highlighter, inlined into standalone HTML
             ;; transcript exports at RUNTIME via io/resource.
             "-H:IncludeResources=vis-transcript/.*"
             ;; tree-sitter pack FFI lib for THIS platform. The pack's own
             ;; metadata ships NO resource glob (unlike fff/rift/ruff's
             ;; prebuilds/**), so without this the shipped binary embeds no
             ;; tree-sitter native at all and the runtime resolver-download
             ;; path — which a native image cannot take — is the only hope.
             (str "-H:IncludeResources=natives/" (pack-native-token) "/.*")
             ;; GraalPy/Truffle per-platform internal-resource manifests.
             ;; These used to ride in via the macOS agent trace with
             ;; darwin/aarch64 HARDCODED — which embedded the Mac entries
             ;; into Linux/Windows images (python-resources ships every
             ;; platform's dirs in one jar) and left the build host's own
             ;; manifests out everywhere else. Host-parameterized instead.
             (str "-H:IncludeResources=META-INF/resources/" t-os "/" t-arch "/native.sha256")
             (str "-H:IncludeResources=META-INF/resources/engine/libtruffleattach/"
                  t-os
                  "/"
                  t-arch
                  "/.*")
             ;; tree-sitter binding: a few pure-data classes (enums / structural
             ;; op tables) reach the image heap and must initialize at BUILD time.
             ;; NativeLib / TreeSitterLanguagePackRs stay run-time (they load the
             ;; FFI lib) via the lib's own native-image.properties.
             "--initialize-at-build-time=dev.kreuzberg.treesitterlanguagepack.StructuralApi$Op"
             ;; ── GraalPy native-image bring-up ──────────────────────────────
             ;; `org.graalvm.python/python-resources` ships its config at the
             ;; NON-standard `META-INF/resources/native-image.properties`, which
             ;; native-image does NOT auto-discover. Apply it explicitly:
             ;;   • embed the Python stdlib VirtualFileSystem (org.graalvm.python.vfs)
             ;;     — without it GraalPy scans the real FS for a home that isn't
             ;;     there and the first Context.create() hangs (readdir + cond_wait).
             ;;   • PreinitializeContexts=python snapshots an initialized Python
             ;;     context INTO the image, so runtime `Context.create("python")`
             ;;     resumes the snapshot instead of doing full (hanging) init.
             ;;   • Python needs a big charset set + a deep C stack.
             "-H:+UnlockExperimentalVMOptions" "-H:IncludeResources=org.graalvm.python.vfs/.*"
             "-J-Dpolyglot.image-build-time.PreinitializeContexts=python" "-R:StackSize=16777216"
             "-H:+AddAllCharsets"
             ;; ── Binary-size + build-time reduction ──────────────────────────
             ;; A GraalPy image is huge (~558 MB): ~115 MB machine code +
             ;; ~465 MB SVM image heap (embedded CPython interpreter/stdlib +
             ;; icu4j locale data + charsets). Two levers, both from GraalPy's
             ;; own "Reducing Binary Size" guide — they ALSO slash native-image
             ;; BUILD time (the full :voice image otherwise stalls the runner
             ;; building/compiling ~18k Truffle runtime-compiled methods):
             ;;   • -Os optimizes the COMPILED CODE for size instead of -O2
             ;;     speed — trims the ~115 MB __text with negligible impact on
             ;;     an I/O-bound agent, and cuts compile time.
             "-Os"
             ;;   • Run embedded Python INTERPRETED: drop the Truffle/Graal JIT
             ;;     from the image. GraalPy documents this as ~40% smaller — the
             ;;     single biggest lever — and it removes the giant runtime-
             ;;     compiled-method set that made native-image hang in universe
             ;;     building on the full profile. vis's python_execution sandbox
             ;;     runs SHORT, I/O-bound glue scripts, exactly the
             ;;     "performance not critical / short-running" case this targets.
             ;;     Delete these two lines to restore the JIT if a workload needs it.
             "-Dtruffle.TruffleRuntime=com.oracle.truffle.api.impl.DefaultTruffleRuntime"
             "-Dpolyglot.engine.WarnInterpreterOnly=false"]
      ;; voice JNI native libs for THIS platform (sherpa + onnxruntime).
      ;; Per-host `tok` keeps foreign-OS libs OUT of each binary; the
      ;; onnxruntime pattern stops at the dir level ([^/]*$) so the macOS
      ;; jar's nested *.dSYM DWARF debug bundles (~8 MB) don't ride in.
      voice?
      (conj (str "-H:IncludeResources=native/" tok "/.*")
            (str "-H:IncludeResources=ai/onnxruntime/native/" tok "/[^/]*$"))

      with-assets?
      (conj "-H:IncludeResources=voice-assets/.*")

      (seq extra)
      (into extra)

      :always
      (conj "com.blockether.vis.core"))))

(defn native-image-only
  "FAST native-image iteration: re-run native-image ONLY, reusing the existing
   `target/native-classes` from a prior `native` build (no re-AOT). For tuning
   native-image flags; run `native` once first to populate the AOT classes."
  [opts]
  (let [profile
        (resolve-profile opts)

        basis
        (b/create-basis {:project (root-deps-edn profile) :aliases [:native]})]

    (println "native-image (reusing target/native-classes)…")
    (let [{:keys [exit]} (b/process
                           {:command-args
                            (into [(native-image-command)]
                                  (native-image-args basis (embed-assets? opts profile) profile))})]
      (if (zero? exit)
        (println "-> built" native-bin)
        (throw (ex-info "native-image build failed" {:exit exit}))))))

(defn native
  "Build BOTH vis distributions in one shot:
     1. `target/vis.jar`  — portable JVM uberjar (LOCAL `vis --jvm` fallback, not shipped)
     2. `target/vis`      — standalone native binary (`target/vis.exe` on Windows)
   They share one AOT pass. Requires `native-image` on PATH (Oracle GraalVM /
   GraalVM CE 25+) and ≥16 GB RAM (GraalPy's libpythonvm needs -Xms14g).
   `bin/vis` then proxies to the native binary by default.

   Options:
     :profile :tui       — MINIMAL: TUI channel only (no web, Telegram, voice).
     :profile :cross     — all channels, NO voice.
     :profile :voice     — all channels + voice ASR (the default).
     :profile :community — SHIPPED full distribution: every extension incl. web
                           `search` + voice ASR; models download on first use.
     :profile :corporate — SHIPPED air-gapped distribution: `search` extension
                           dropped entirely + the parakeet ASR model ALWAYS
                           embedded (fully offline, no HuggingFace/network fetch).
     :with-assets true   — embed the ~465 MB voice ASR model for a fully-offline
                           binary (default: download on first use). Requires a
                           voice-capable profile; :corporate implies it."
  [opts]
  (let
    [profile
     (resolve-profile opts)

     with-assets?
     (embed-assets? opts profile)

     _
     (when (and with-assets? (not (voice-profile? profile)))
       (throw
         (ex-info
           ":with-assets embeds the voice model — it requires a voice-capable profile (:voice, :community or :corporate)"
           {:opts opts :profile profile})))

     basis
     (prepare-native-classes! profile)]

    (when with-assets? (vendor-voice-model! native-class-dir))
    ;; (1) JVM distribution — also the `vis --jvm` fallback. Portable uberjar.
    (b/delete {:path native-uber})
    (b/uber {:class-dir native-class-dir
             :uber-file native-uber
             :basis basis
             :main 'com.blockether.vis.core
             :exclude uber-exclusions})
    (println "->" native-uber)
    ;; (2) native distribution. Built from a classpath of real jars (NOT the
    ;; uberjar) so polyglot/graalpy keep their module-info + native-image.properties.
    (println "native-image:"
             native-bin
             (str "(profile " (name profile) (when with-assets? " +assets") ")")
             "(this takes several minutes)…")
    (let [{:keys [exit]} (b/process {:command-args
                                     (into [(native-image-command)]
                                           (native-image-args basis with-assets? profile))})]
      (if (zero? exit)
        (println "-> built" native-bin)
        (throw (ex-info "native-image build failed" {:exit exit}))))))
