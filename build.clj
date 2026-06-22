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
   {:lib 'com.blockether/vis-foundation-exa                    :dir "extensions/common/vis-foundation-exa"}])

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
   'com.blockether/vis-foundation-exa
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
(def ^:private native-bin
  (str "target/vis" (when (str/includes? (str/lower-case (System/getProperty "os.name")) "windows") ".exe")))

(defn- all-source-roots
  "Every production src/resources dir on the vis classpath: the repo root plus
   each `:local/root` extension. AOT covers all of these so every extension ns
   the runtime manifest scan `require`s is already compiled into the image."
  []
  (let [deps  (:deps (read-string (slurp "deps.edn")))
        roots (->> deps vals (keep :local/root))
        dirs  (into ["src" "resources"]
                (mapcat (fn [r] [(str r "/src") (str r "/resources")]) roots))]
    (filterv #(.exists (io/file %)) dirs)))

(defn- merge-extension-manifests!
  "Each extension ships its own `META-INF/vis-extension/vis.edn`. Across many
   jars/dirs `manifest.clj` ENUMERATES them (getResources returns one per
   classpath entry); but in a single uberjar/native image they all collide to
   ONE path and only one survives → only one extension registers. Fix: merge
   every extension's manifest map into ONE combined file in the class-dir.
   `manifest.clj` already iterates a multi-id map, so a single merged resource
   carries every extension id with no runtime change."
  [class-dir]
  (let [files  (->> (file-seq (io/file "extensions"))
                 (filter #(and (= "vis.edn" (.getName ^java.io.File %))
                            (str/includes? (str %) "META-INF/vis-extension"))))
        merged (reduce (fn [m f] (merge m (read-string (slurp f)))) {} files)
        out    (io/file class-dir "META-INF" "vis-extension" "vis.edn")]
    (io/make-parents out)
    (spit out (pr-str merged))
    (println "Merged" (count files) "extension manifests ->" (count merged) "ids:"
      (str/join " " (sort (map str (keys merged)))))))

(defn- ns-name-of
  "The namespace symbol of a Clojure source string, or nil. Reads the first
   form so metadata before the name (e.g. `(ns ^{:doc …} foo)`) is handled, and
   files loaded via `in-ns` (no `(ns …)` form) are skipped."
  [content]
  (try
    (with-open [r (java.io.PushbackReader. (java.io.StringReader. content))]
      (binding [*read-eval* false]
        (let [form (read {:read-cond :allow :eof nil} r)]
          (when (and (seq? form) (= 'ns (first form)))
            (first (filter symbol? (rest form)))))))
    (catch Throwable _ nil)))

(def ^:private warn-on-reflection-re
  #"\(set!\s+\*(?:warn-on-reflection|unchecked-math)\*")

(defn- preload-namespaces
  "Namespaces whose source has a top-level `(set! *warn-on-reflection* …)` /
   `(set! *unchecked-math* …)`. On GraalVM these must be initialized via `require`
   (which binds the var) BEFORE build-time class init runs their `<clinit>` raw on
   a parallel worker — otherwise the `set!` throws `Can't change/establish root
   binding`. Scans every source dir + dependency jar on the :native classpath.
   The native-image Feature reads this list and requires each one. See
   docs/NATIVE_IMAGE.md and com.blockether.vis.internal.nativeimage."
  [basis]
  (let [cljc? #(re-matches #".*\.cljc?$" %)
        from-dir (fn [d]
                   (->> (file-seq (io/file d))
                     (filter #(and (.isFile ^java.io.File %) (cljc? (.getName ^java.io.File %))))
                     (keep (fn [f] (let [c (slurp f)] (when (re-find warn-on-reflection-re c) (ns-name-of c)))))))
        from-jar (fn [jar]
                   (with-open [zf (java.util.zip.ZipFile. ^String jar)]
                     (doall
                       (->> (enumeration-seq (.entries zf))
                         (filter #(cljc? (.getName ^java.util.zip.ZipEntry %)))
                         (keep (fn [e]
                                 (let [c (slurp (.getInputStream zf ^java.util.zip.ZipEntry e))]
                                   (when (re-find warn-on-reflection-re c) (ns-name-of c)))))))))]
    (->> (:classpath-roots basis)
      (mapcat (fn [r]
                (let [f (io/file r)]
                  (cond
                    (not (.exists f))            nil
                    (str/ends-with? r ".jar")    (from-jar r)
                    (.isDirectory f)             (from-dir r)
                    :else                         nil))))
      (remove nil?) distinct (sort-by str) vec)))

(defn- write-preload-namespaces! [class-dir basis]
  (let [nses (preload-namespaces basis)
        out  (io/file class-dir "META-INF" "vis-native-image" "preload.edn")]
    (io/make-parents out)
    (spit out (pr-str (mapv str nses)))
    (println "Preload list:" (count nses) "namespaces with (set! *warn-on-reflection* …) ->" (str out))))

(defn- write-migration-indexes!
  "Flyway discovers migrations by LISTING its classpath location dir — which
   native-image can't do. For every `**/migration/` dir of `.sql` we copied,
   write an `_index.edn` of filenames so the SQLite backend's `migrate!` can
   serve them by exact path via a ResourceProvider. JVM builds ignore the index."
  [class-dir]
  (let [sql    (->> (file-seq (io/file class-dir))
                 (filter #(and (.isFile ^java.io.File %)
                            (str/ends-with? (.getName ^java.io.File %) ".sql")
                            (str/includes? (str %) "/migration/"))))
        by-dir (group-by #(.getParentFile ^java.io.File %) sql)]
    (doseq [[^java.io.File dir files] by-dir]
      (let [names (vec (sort (map #(.getName ^java.io.File %) files)))]
        (spit (io/file dir "_index.edn") (pr-str names))
        (println "Migration index:" (str (io/file dir "_index.edn")) "->" names)))))

(defn- prepare-native-classes!
  "AOT-compile every ns (core + every extension) into `native-class-dir` and copy
   all resources, collapsing the per-extension manifests into ONE merged file.
   Also writes the build-time-init preload list. Shared by `uber` and `native`.
   Returns the `:native`-alias basis."
  []
  (b/delete {:path native-class-dir})
  (let [basis (b/create-basis {:project "deps.edn" :aliases [:native]})
        srcs  (all-source-roots)]
    (println "AOT compiling every ns across" (count srcs) "source roots…")
    ;; copy resources (incl. META-INF/vis-extension + META-INF/native-image)
    (b/copy-dir {:src-dirs srcs :target-dir native-class-dir})
    ;; collapse the per-extension manifests into ONE so discovery finds them all
    (merge-extension-manifests! native-class-dir)
    ;; list every namespace the native Feature must require before build-time init
    (write-preload-namespaces! native-class-dir basis)
    ;; index Flyway migrations so they're discoverable without dir listing
    (write-migration-indexes! native-class-dir)
    ;; no :ns-compile => compile EVERY ns found in :src-dirs (extensions included)
    (b/compile-clj {:basis basis :src-dirs srcs :class-dir native-class-dir})
    basis))

(defn uber
  "Build the all-in-one vis uberjar (`target/vis.jar`) with Main-Class
   `com.blockether.vis.core`. Handy for `java -jar target/vis.jar --version` to
   sanity-check the AOT'd app. NOTE: the native build does NOT use this jar —
   GraalPy's polyglot jar declares `ForceOnModulePath`, which a flat uberjar
   (no module-info) breaks; `native` builds from a classpath of real jars."
  [_]
  (b/delete {:path native-uber})
  (let [basis (prepare-native-classes!)]
    (b/uber {:class-dir native-class-dir :uber-file native-uber :basis basis
             :main 'com.blockether.vis.core})
    (println "->" native-uber)))

(defn- native-classpath
  "Classpath for the native build: the AOT classes dir FIRST (so compiled app +
   merged manifest win), then every dependency JAR. We deliberately DROP the
   :local/root source/resource dirs — their compiled+copied form already lives in
   `native-class-dir`, and re-adding them would resurrect the per-extension
   manifest collision. Keeping deps as separate jars lets native-image honor each
   jar's module-info + native-image.properties (polyglot's `ForceOnModulePath`,
   GraalPy's build-time init, etc.)."
  [basis]
  (->> (:classpath-roots basis)
    (filter #(str/ends-with? % ".jar"))
    (into [native-class-dir])
    (str/join java.io.File/pathSeparator)))

(defn- native-platform-token
  "sherpa-onnx / onnxruntime native-lib dir token for the BUILD host
   (e.g. `osx-aarch64`, `linux-x64`, `win-x64`). Both jars use this layout."
  []
  (let [os   (str/lower-case (System/getProperty "os.name"))
        arch (str/lower-case (System/getProperty "os.arch"))
        a    (cond (#{"aarch64" "arm64"} arch)     "aarch64"
                   (#{"x86_64" "amd64" "x64"} arch) "x64"
                   :else arch)]
    (cond
      (str/includes? os "mac") (str "osx-" a)
      (str/includes? os "win") (str "win-" a)
      :else                     (str "linux-" a))))

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
  (let [cache (io/file voice-model-cache)
        out   (io/file class-dir voice-asset-resource-dir)]
    (io/make-parents cache)
    (when-not (.isFile cache)
      (println "Vendoring voice model (~465 MB) <-" voice-model-url)
      (with-open [in  (io/input-stream (java.net.URI. voice-model-url))
                  os* (io/output-stream cache)]
        (io/copy in os*)))
    (.mkdirs out)
    ;; commons-compress (on the :build classpath) handles the bz2 archive
    (with-open [fis (io/input-stream cache)
                bz  (org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream. fis)
                tar (org.apache.commons.compress.archivers.tar.TarArchiveInputStream. bz)]
      (loop []
        (when-let [e (.getNextEntry tar)]
          (when-not (.isDirectory e)
            (let [name (.getName e)
                  base (subs name (inc (.lastIndexOf name "/")))]
              (io/copy tar (io/file out base))))
          (recur))))
    (println "Vendored voice model ->" (str out)
      (vec (map #(.getName ^java.io.File %) (.listFiles out))))))

(defn- native-image-args
  "native-image CLI args. Config travels INSIDE the classpath jars
   (META-INF/native-image/…); here we add only classpath/main/output, the
   vis-extension/edn/db resource includes, and the build-host voice native libs
   (sherpa-onnx + onnxruntime JNI dylibs) so voice ASR works in the binary.
   `with-assets?` also embeds the vendored voice model resources."
  [basis with-assets?]
  (let [tok (native-platform-token)]
    (cond-> ["-cp" (native-classpath basis)
             "-o" (str/replace native-bin #"\.exe$" "")
             "-H:IncludeResources=META-INF/vis-extension/.*"
             "-H:IncludeResources=.*\\.edn$"
             ;; Flyway migration SQL (not in the agent-traced metadata)
             "-H:IncludeResources=db/.*"
             ;; voice JNI native libs for THIS platform (sherpa + onnxruntime)
             (str "-H:IncludeResources=native/" tok "/.*")
             (str "-H:IncludeResources=ai/onnxruntime/native/" tok "/.*")]
      with-assets? (conj "-H:IncludeResources=voice-assets/.*")
      :always (conj "com.blockether.vis.core"))))

(defn native
  "Build BOTH vis distributions in one shot:
     1. `target/vis.jar`  — portable JVM uberjar (the `vis --jvm` distribution)
     2. `target/vis`      — standalone native binary (`target/vis.exe` on Windows)
   They share one AOT pass. Requires `native-image` on PATH (Oracle GraalVM /
   GraalVM CE 25+) and ≥16 GB RAM (GraalPy's libpythonvm needs -Xms14g).
   `bin/vis` then proxies to the native binary by default. See docs/NATIVE_IMAGE.md.

   Options:
     :with-assets true  — also embed the ~465 MB voice ASR model for a
                          fully-offline 'fat' binary (default: download on first use)."
  [opts]
  (let [with-assets? (boolean (:with-assets opts))
        basis        (prepare-native-classes!)]
    (when with-assets?
      (vendor-voice-model! native-class-dir))
    ;; (1) JVM distribution — also the `vis --jvm` fallback. Portable uberjar.
    (b/delete {:path native-uber})
    (b/uber {:class-dir native-class-dir :uber-file native-uber :basis basis
             :main 'com.blockether.vis.core})
    (println "->" native-uber)
    ;; (2) native distribution. Built from a classpath of real jars (NOT the
    ;; uberjar) so polyglot/graalpy keep their module-info + native-image.properties.
    (println "native-image:" native-bin (if with-assets? "(+assets)" "")
      "(this takes several minutes)…")
    (let [{:keys [exit]} (b/process {:command-args (into ["native-image"] (native-image-args basis with-assets?))})]
      (if (zero? exit)
        (println "-> built" native-bin)
        (throw (ex-info "native-image build failed" {:exit exit}))))))
