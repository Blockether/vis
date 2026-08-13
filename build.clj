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
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

;; =============================================================================
;; Version
;; =============================================================================

(def version
  "Single source of truth for the published version: the repo-root VIS_VERSION
   file, verbatim. No env override, no snapshot suffix, no git sha — the jar
   coordinates, the native image, the container and `vis-agent --version` all
   report this one string."
  (str/trim (slurp "VIS_VERSION")))

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
  (let
    [short
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
  (let
    [edn (-> (str dir "/deps.edn")
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
  (let
    [src
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
  (let
    [{:keys [class-dir jar-file]}
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
  (let
    [built (doall (for [pkg (deploy-build-order (selected-packages opts))]
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
;; The Vis Agent application (`bin/vis-agent` = `clojure -M:vis`) compiles to a
;; private native runtime used behind the shipped Bash wrapper. Pipeline: AOT
;; EVERY namespace (core + every extension — extensions are `require`d at runtime
;; by manifest discovery, so they MUST be in the image) -> uberjar -> native-image.
;;
;; Embedded GraalPy + dynamic extension loading make this non-trivial; see the
;; `:native` alias (graal-build-time) and `native-image-args`. Cross-platform:
;; the same `uber`/`native` tasks run on Linux/macOS; CI matrixes them.
;; =============================================================================

(def ^:private native-class-dir "target/native-classes")

(def ^:private native-uber "target/vis.jar")

(def ^:private uber-exclusions
  "Entry patterns dropped from the build-only uberjar. The jar DELIBERATELY keeps
   every platform's JNI libs (sherpa-onnx, onnxruntime, sqlite-jdbc), but the
   onnxruntime macOS libs drag ~16.5 MB of nested *.dSYM DWARF debug bundles
   along and its win-x64 entry drags a 286 MB *.pdb; no runtime reads either."
  ["ai/onnxruntime/native/.*\\.dSYM/.*" "ai/onnxruntime/native/.*\\.pdb"
   ;; dep-jar warts: babashka/http-client ships scratch.clj and sci ships
   ;; scratch.cljs at the classpath ROOT of their published jars
   "scratch\\.cljs?"
   ;; oh-my-claudecode agent session state — recreated whenever an agent
   ;; runs with its cwd inside a source tree; must never ship
   ".*\\.omc/.*"])

(def ^:private native-bin "target/vis")

(defn- native-image-command
  "The native-image launcher to invoke via `b/process` (Java ProcessBuilder).
   Resolve the concrete launcher from GRAALVM_HOME / JAVA_HOME
   (…/bin/native-image); fall back to the bare name on PATH."
  []
  (let
    [home
     (or (System/getenv "GRAALVM_HOME") (System/getenv "JAVA_HOME"))

     launcher
     (when home (io/file home "bin" "native-image"))]

    (if (and launcher (.isFile launcher)) (.getAbsolutePath launcher) "native-image")))

(def ^:private graal-pin
  "The `.graalvm-version` pin, parsed. That file is the SINGLE source of truth for
   which JDK vis is built with; the CI action, the Dockerfile and bin/require-graalvm
   read the same file, so this check can never drift from what CI installs."
  (delay (let [f (io/file ".graalvm-version")]
           (when (.isFile f)
             (into {}
                   (keep (fn [line]
                           (when-let
                             [[_ k v] (re-matches #"\s*([A-Z0-9_]+)=\"?([^\"#]*)\"?\s*" line)]
                             [k (str/trim v)])))
                   (str/split-lines (slurp f)))))))

(defn- assert-graal-pins!
  "The OTHER half of the `.graalvm-version` contract, enforced on the JVM side.
  A correct JDK is only half the story: deps.edn's org.graalvm.* jars must name
  the same version as `.graalvm-version`, or Truffle refuses a runtime whose
  built-in version differs from the polyglot jars — minutes into the image
  build, with an opaque NoClassDefFoundError. The Dockerfile's `check-graal-pins`
  and `bin/require-graalvm`'s `check_pins` are the same gate; this is the host /
  CI gate so `clojure -T:build native` can never build on a drifted pin."
  []
  (when-let [{:strs [GRAAL_VERSION GRAAL_MAX_VERSION GRAAL_PIN_LOCKED]} @graal-pin]
    ;; The pin is LOCKED: 25.1.3, nothing higher. 25.2.x's points-to analysis
    ;; never converges on this tree and OOMs the builder at every heap size, so
    ;; a bump is refused here rather than six minutes into `native`.
    (when (and (= "true" GRAAL_PIN_LOCKED) (not= GRAAL_VERSION GRAAL_MAX_VERSION))
      (throw (ex-info (str ".graalvm-version is LOCKED at " (or GRAAL_MAX_VERSION "<unset>")
                           " (NOT UPGRADABLE), but GRAAL_VERSION=" GRAAL_VERSION
                           ".\n  Move GRAAL_MAX_VERSION deliberately, or set "
                           "GRAAL_PIN_LOCKED=\"false\" — see .graalvm-version.")
                      {:locked GRAAL_MAX_VERSION :got GRAAL_VERSION})))
    (let
      [bad (into []
                 (comp (filter #(str/includes? (str %) "org.graalvm."))
                       (keep (fn [form]
                               (let
                                 [sym (first form)
                                  v (-> form
                                        second
                                        :mvn/version)]

                                 (when (and v (not= v GRAAL_VERSION)) [sym v])))))
                 (->> (edn/read-string (slurp (io/file "deps.edn")))
                      :deps))]
      (when (seq bad)
        (throw (ex-info (str "deps.edn org.graalvm.* pins do not match .graalvm-version "
                             GRAAL_VERSION
                             ".\n" (str/join "\n"
                                             (map (fn [[sym v]]
                                                    (str "  " sym " " v " ≠ " GRAAL_VERSION))
                                                  bad))
                             "\n  .graalvm-version is the single source of truth — "
                             "bump it and every pin together.")
                        {:expected GRAAL_VERSION :mismatched bad}))))))

(def ^:private graalvm-script
  "The one resolver/installer — the same file CI, the Dockerfile and humans use.
   Printing a home on stdout means success; every diagnostic goes to stderr."
  "bin/require-graalvm")

(defn- custom-truststore
  "The keystore this build must trust, or nil — `bin/require-graalvm
   --truststore` decides, we only forward the answer.

   A freshly installed JDK trusts the public roots and NOTHING else, so behind
   a TLS-intercepting corporate proxy dependency resolution and native-image
   both die with `SunCertPathBuilderException: unable to find valid
   certification path`, while the system JDK (whose cacerts the corporate
   installer patched) works. Set VIS_CA_CERT=/path/ca.pem or
   VIS_TRUSTSTORE=/path/store.p12; the script imports a PEM into a cached COPY
   of that JDK's cacerts and never modifies the JDK itself."
  [home]
  (let [script (io/file graalvm-script)]
    (when (and (.isFile script) (or (System/getenv "VIS_CA_CERT") (System/getenv "VIS_TRUSTSTORE")))
      (let
        [{:keys [exit out]}
         (b/process (cond-> {:command-args ["bash" (.getPath script) "--truststore"] :out :capture}
                      home
                      (assoc :env {"JAVA_HOME" home})))]
        (when (zero? exit) (not-empty (str/trim (or out ""))))))))

(defn- truststore-properties
  "`-Djavax.net.ssl.trustStore*` for the resolved keystore, or nil. A store the
   user supplied keeps the user's type/password; one generated from a PEM is
   always a PKCS12 copy of cacerts under the JDK default password."
  [home]
  (when-let [store (custom-truststore home)]
    (let [supplied? (some? (System/getenv "VIS_TRUSTSTORE"))]
      [(str "-Djavax.net.ssl.trustStore=" store)
       (str "-Djavax.net.ssl.trustStoreType="
            (or (when supplied? (System/getenv "VIS_TRUSTSTORE_TYPE")) "PKCS12"))
       (str "-Djavax.net.ssl.trustStorePassword="
            (or (when supplied? (System/getenv "VIS_TRUSTSTORE_PASSWORD")) "changeit"))])))

(defn- resolve-pinned-graalvm
  "Where the pinned GraalVM CE home is, according to `bin/require-graalvm`.
   With `install?` false the script only SEARCHES what is already on this
   machine — no download, no network; with true it may fetch + checksum +
   install it. Returns the home path, or nil when nothing usable was found."
  [install?]
  (let [script (io/file graalvm-script)]
    (when (.isFile script)
      (let
        [{:keys [exit out]} (b/process {:command-args (cond-> ["bash" (.getPath script)]
                                                        install?
                                                        (conj "--install"))
                                        :out :capture})]
        (when (zero? exit) (not-empty (str/trim (or out ""))))))))

(defn- auto-install-graalvm?
  "May this build install the pinned JDK by itself? YES by default: a missing
   GraalVM CE is a machine-setup detail, and `bin/require-graalvm --install`
   downloads, checksums and installs exactly the pin — so `clojure -T:build
   native` just works on a stock JDK, and on Oracle GraalVM too (Oracle is not
   a CE substitute, so it is installed alongside rather than accepted).
   Opt OUT with `:auto-install-graalvm false` or VIS_AUTO_INSTALL_GRAALVM=0
   (also `false`/`no`), which turns the missing JDK back into a hard refusal."
  [opts]
  (let
    [env (some-> (System/getenv "VIS_AUTO_INSTALL_GRAALVM")
                 str/trim
                 str/lower-case)]
    (cond (contains? opts :auto-install-graalvm) (boolean (:auto-install-graalvm opts))
          (contains? #{"0" "false" "no"} env) false
          :else true)))

(defn- rerun-under-graalvm!
  "Re-run THIS build task in a child process rooted at `home` — the `nvm use`
   move. Truffle/SVM read the RUNNING JDK, so switching JDKs means a new
   process, never a new system property; the child is marked so a still-wrong
   JVM fails hard instead of forking forever. Never returns: the child's exit
   code becomes ours."
  [home task opts]
  (println (str "· " home))
  (println (str "· re-running :" (name task) " under the pinned GraalVM CE"))
  (let
    [args
     (into ["clojure" "-T:build" (name task)]
           (mapcat (fn [[k v]]
                     [(str k) (pr-str v)]))
           (dissoc opts :auto-install-graalvm))

     trust
     (truststore-properties home)

     {:keys [exit]}
     (b/process
       {:command-args args
        :env (cond->
               {"JAVA_HOME" home
                "GRAALVM_HOME" home
                ;; The clojure CLI prefers JAVA_CMD over JAVA_HOME, and
                ;; `bin/vis-agent` exports it — without pinning it here the child
                ;; silently starts on the INHERITED JDK and dies on the
                ;; hard refusal below (VIS_GRAALVM_SWITCHED already set).
                "JAVA_CMD" (str home "/bin/java")
                "PATH" (str home "/bin" java.io.File/pathSeparator (or (System/getenv "PATH") ""))
                "VIS_GRAALVM_SWITCHED" "1"}
               ;; Corporate CA: the child resolves dependencies over TLS under a
               ;; JDK that trusts only the public roots. JAVA_TOOL_OPTIONS (not
               ;; JDK_JAVA_OPTIONS) because every JVM the child forks — clojure,
               ;; native-image, its builder — must inherit the same trust.
               (seq trust)
               (assoc "JAVA_TOOL_OPTIONS"
                 (str/join " "
                           (remove nil?
                             (cons (not-empty (System/getenv "JAVA_TOOL_OPTIONS")) trust)))))})]

    (System/exit (or exit 1))))

(defn- assert-graalvm-ce!
  "Refuse to build on anything but the pinned GraalVM COMMUNITY Edition — but
   switch to it by itself when it is already on this machine. Three JDKs get
   this far and each fails later and more expensively: a stock JDK 25 has no
   `native-image` at all; Oracle GraalVM builds a binary that is no longer
   GPL+CE-licensed (audit/README.md §4.1 promises CE only); a near CE version
   is hard-rejected by Truffle/SVM against the org.graalvm.* pins in deps.edn —
   usually minutes into the image build.

   `java.vendor.version` is the one property that separates all three
   (\"GraalVM CE 25.1.3+9.1\" vs \"Oracle GraalVM 25.1.3+9.1\" vs \"Temurin-25…\").

   Wrong JVM, in order: already installed → re-exec the task under it;
   not installed → install the pin with `bin/require-graalvm --install`, then
   re-exec under it; installation declined (`:auto-install-graalvm false` /
   VIS_AUTO_INSTALL_GRAALVM=0) or failed → the hard refusal."
  [task opts]
  (assert-graal-pins!)
  (when-let
    [{want "GRAAL_VENDOR_VERSION" edition "GRAAL_EDITION" version "GRAAL_VERSION"} @graal-pin]
    (let [got (or (System/getProperty "java.vendor.version") "unknown JDK")]
      (if (= got want)
        (println (str "· " edition " " version " (" got ")"))
        (let
          [switched? (= "1" (System/getenv "VIS_GRAALVM_SWITCHED"))
           installed (when-not switched? (resolve-pinned-graalvm false))
           home (or installed
                    (when (and (not switched?) (auto-install-graalvm? opts))
                      (println (str "· " edition " " version " is missing — installing it"))
                      (resolve-pinned-graalvm true)))]

          (if home
            (rerun-under-graalvm! home task opts)
            (throw
              (ex-info
                (str
                  "this build requires "
                  edition
                  " "
                  version
                  " — the build JVM reports \""
                  got
                  "\"\n"
                  "  expected java.vendor.version: \""
                  want
                  "\"\n"
                  "  install it here:  clojure -T:build "
                  (name task)
                  " :auto-install-graalvm true\n"
                  "           or:      VIS_AUTO_INSTALL_GRAALVM=1 clojure -T:build " (name task)
                  "\n" "  install it yourself:  bin/require-graalvm --install\n"
                  "  then:                 sdk env   (or: eval \"$(bin/require-graalvm --export)\")\n"
                  "  An already-installed pinned JDK is picked up and used automatically, and a\n"
                  "  missing one is installed automatically unless VIS_AUTO_INSTALL_GRAALVM=0.\n"
                  "  Stock JDKs and Oracle GraalVM are NOT substitutes — see .graalvm-version.")
                {:expected want :actual got :task task}))))))))

;; ── Distribution: community, and only community ─────────────────────────────
;; There is ONE distribution and it bundles EVERYTHING: every channel, every
;; provider/language extension, voice ASR, the web `search` extension. Nothing
;; is ever dropped from the classpath, so there is nothing to select — the old
;; :tui / :cross / :voice cuts are gone. They existed only to shed
;; com.blockether/vis-foundation-voice (the ONLY route by which the
;; sherpa-onnx/onnxruntime JNI libs reach the classpath), i.e. to ship a
;; voiceless vis — not a product we ship.
;; `:profile :community` is still ACCEPTED on `native` / `uber` (the Dockerfile
;; and the release workflow pass it); anything else is a hard error.
;; The parakeet ASR model is NEVER embedded in the binary: it is distributed
;; separately and fetched on first use, so the binary stays lean.

(def ^:private the-profile
  "The one and only distribution. Kept named so `:profile :community` parses."
  :community)

(defn- resolve-profile
  [opts]
  (let [p (keyword (or (:profile opts) the-profile))]
    (when-not (= p the-profile)
      (throw (ex-info (str "Unknown :profile "
                           p
                           " — :community is the only distribution, and it bundles everything")
                      {:profile p :available [the-profile]})))
    p))

(defn- root-deps-edn
  "Root deps.edn as an edn map. Community bundles everything, so nothing is
   dissoc'd and nothing is excluded — the whole dep tree ships."
  []
  (read-string (slurp "deps.edn")))

(defn- all-source-roots
  "Every production src/resources dir on the vis classpath: the repo root plus
   each `:local/root` extension. AOT covers all of these so every extension ns
   the runtime manifest scan `require`s is already compiled into the image."
  []
  (let
    [deps
     (:deps (root-deps-edn))

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
  [class-dir]
  (let
    [files
     (->> (file-seq (io/file "extensions"))
          (filter (fn [^java.io.File f]
                    ;; normalize separators so a forward-slash substring check
                    ;; can never silently merge zero manifests.
                    (let [p (str/replace (str f) "\\" "/")]
                      (and (= "vis.edn" (.getName f))
                           (str/includes? p "META-INF/vis-extension"))))))

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
  (let
    [cljc?
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
;; classpath manifest). EVERY one of them, in the same order: a name missing here
;; is missing from the image, and `load-builtin-extensions!` aborts the binary at
;; startup with "Could not locate <ns>__init.class on classpath" -- there is no
;; degraded mode. `native-reachability-test` reads this vector and fails when it
;; drifts from extension/builtin-extension-nses.
(def ^:private builtin-extension-nses
  ["com.blockether.vis.internal.foundation.core"
   "com.blockether.vis.internal.foundation.introspection"
   "com.blockether.vis.internal.foundation.shell" "com.blockether.vis.internal.foundation.shim-yaml"
   "com.blockether.vis.internal.foundation.shim-matplotlib"
   "com.blockether.vis.internal.foundation.shim-requests"
   "com.blockether.vis.internal.foundation.shim-pytest"
   "com.blockether.vis.internal.foundation.shim-ruff"
   "com.blockether.vis.internal.foundation.shim-pil"
   "com.blockether.vis.internal.foundation.shim-numpy"
   "com.blockether.vis.internal.foundation.shim-bs4"
   "com.blockether.vis.internal.foundation.shim-pandas"
   "com.blockether.vis.internal.foundation.shim-tabulate"
   "com.blockether.vis.internal.foundation.shim-toml"
   "com.blockether.vis.internal.foundation.shim-tzdata"
   "com.blockether.vis.internal.foundation.shim-sqlite3"
   "com.blockether.vis.internal.foundation.shim-nippy"
   "com.blockether.vis.internal.foundation.shim-httpx"
   "com.blockether.vis.internal.foundation.shim-urllib3"
   "com.blockether.vis.internal.foundation.shim-paramiko"
   "com.blockether.vis.internal.foundation.shim-xlsxwriter"
   "com.blockether.vis.internal.foundation.shim-pptx"
   "com.blockether.vis.internal.foundation.shim-attach"
   "com.blockether.vis.internal.foundation.shim-ls"
   "com.blockether.vis.internal.foundation.shim-fonttools"
   "com.blockether.vis.internal.foundation.shim-anydoc"
   "com.blockether.vis.internal.foundation.rewind" "com.blockether.vis.internal.foundation.mcp.core"
   "com.blockether.vis.internal.foundation.harness.core"])

(defn- manifest-entry-namespaces
  "Every namespace an extension manifest declares, across the merged manifest
   written by `merge-extension-manifests!` — BOTH keys, because both must be in
   the image:

     :nses        required at DISCOVERY (manifest/scan-extensions!);
     :image-nses  loaded BY NAME on first use (`requiring-resolve`, a backend
                  registrar's `:persistance/ns`) and never required at startup.

   A native image cannot define classes at run time, so a namespace that is not
   build-time initialized is absent from the binary: the TUI's `screen` and the
   sqlite backend's `core` are reached only by name, and leaving them out shipped
   a binary that aborted the moment a human opened the terminal UI or touched the
   DB. `native-reachability-test` fails when a by-name namespace is undeclared."
  [class-dir]
  (let [f (io/file class-dir "META-INF" "vis-extension" "vis.edn")]
    (if (.exists f)
      (->> (read-string (slurp f))
           vals
           (mapcat (fn [entry]
                     (concat (:nses entry) (:image-nses entry))))
           (map str))
      [])))

(defn- write-preload-namespaces!
  [class-dir basis]
  ;; The native Feature `require`s every ns in this list at BUILD time, and the
  ;; list is deliberately NARROW: the (set! *warn-on-reflection* …) namespaces,
  ;; which must be initialized through `require` so the set! has a binding, plus
  ;; the EXTENSION entry namespaces vis `require`s at RUNTIME during discovery
  ;; (extension/discover-extensions! -> load-builtin-extensions! +
  ;; manifest/scan-extensions!) — a runtime `require` in a native image would
  ;; have to DEFINE classes at runtime, which is forbidden ("Classes cannot be
  ;; defined at runtime"), and build-time initializing them makes it a no-op.
  ;;
  ;; It must NOT be "every namespace under the source roots". Requiring the whole
  ;; tree inside the builder JVM runs load-time side effects of code the image
  ;; never reaches, and whatever those effects construct lands in the image heap:
  ;; that is how a live jdk.internal.net.http.HttpClientFacade (babashka's
  ;; implicit @default-client) got persisted and aborted the points-to analysis.
  ;; An extension entry ns pulls its own transitive requires with it, so the
  ;; reachable graph is covered without loading the unreachable one.
  (let
    [warn
     (map str (preload-namespaces basis))

     exts
     (concat builtin-extension-nses (manifest-entry-namespaces class-dir))

     nses
     (->> (concat warn exts)
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
  (let
    [sql
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
  []
  (b/delete {:path native-class-dir})
  (let
    [basis
     (b/create-basis {:project (root-deps-edn) :aliases [:native]})

     srcs
     (all-source-roots)]

    (println "AOT compiling every ns across" (count srcs) "source roots… (profile community)")
    ;; copy resources (incl. META-INF/vis-extension + META-INF/native-image)
    (b/copy-dir {:src-dirs srcs :target-dir native-class-dir})
    ;; sweep agent-session state (.omc/) that lands INSIDE source trees when
    ;; an agent runs with its cwd there — copy-dir happily copies it, and it
    ;; once shipped agent-replay transcripts in the uberjar. Deleted here so
    ;; neither the jar nor the image can ever carry it.
    (doseq
      [^java.io.File f
       (file-seq (io/file native-class-dir))

       :when (and (.isDirectory f) (= ".omc" (.getName f)))]

      (b/delete {:path (.getPath f)})
      (println "Swept agent-state dir from class-dir:" (.getPath f)))
    ;; collapse the per-extension manifests into ONE so discovery finds them all
    (merge-extension-manifests! native-class-dir)
    ;; list every namespace the native Feature must require before build-time init
    (write-preload-namespaces! native-class-dir basis)
    ;; index Flyway migrations so they're discoverable without dir listing
    (write-migration-indexes! native-class-dir)
    ;; `vis/VERSION` resource: what `vis-agent --version` prints and the gateway
    ;; advertises. The repo-root VIS_VERSION file is the ONLY version source, so
    ;; its contents ship verbatim. Which build produced an artifact is the image
    ;; tag's job, never the version string's.
    (let [vfile (io/file native-class-dir "vis" "VERSION")]
      (io/make-parents vfile)
      (spit vfile version))
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
  (resolve-profile opts)
  (let [basis (prepare-native-classes!)]
    (b/uber {:class-dir native-class-dir
             :uber-file native-uber
             :basis basis
             :main 'com.blockether.vis.core
             :exclude uber-exclusions})
    (println "->" native-uber)))

(defn- native-lib-token
  "Host-platform suffix for the blockether FFM native artifacts
   (`<lib>-native-<token>`): darwin-arm64 / darwin-x64 / linux-x64 / linux-arm64."
  []
  (let
    [os
     (str/lower-case (System/getProperty "os.name"))

     arch
     (str/lower-case (System/getProperty "os.arch"))

     a
     (cond (#{"aarch64" "arm64"} arch) "arm64"
           (#{"x86_64" "amd64" "x64"} arch) "x64"
           :else arch)

     o
     (cond (str/includes? os "mac") "darwin"
           :else "linux")]

    (str o "-" a)))

(defn- pack-native-token
  "tree-sitter-language-pack native artifact suffix for the build host. The
   pack publishes its OWN rid scheme (macos-arm64 / macos-x86_64 /
   linux-aarch64 / linux-x86_64) — NOT the fff/rift/ruff darwin-arm64 style;
   both verified against the Clojars artifact list."
  []
  (let
    [os
     (str/lower-case (System/getProperty "os.name"))

     arch
     (str/lower-case (System/getProperty "os.arch"))

     arm?
     (boolean (#{"aarch64" "arm64"} arch))]

    (cond (str/includes? os "mac") (str "macos-" (if arm? "arm64" "x86_64"))
          :else (str "linux-" (if arm? "aarch64" "x86_64")))))

(defn- native-lib-jars
  "Resolve every host-platform FFM artifact at the exact version of its main jar
   in `basis`. Only the host native jar enters the image classpath, so each
   platform image embeds only its own fff/rift/ruff/tree-sitter library.

   Native images cannot use the runtime tools.deps downloader. A missing main
   dependency, failed resolution, or missing direct native jar is therefore a
   hard build failure, never a warning."
  [basis]
  (let
    [tok
     (native-lib-token)

     native-artifacts
     {'com.blockether/fff (str "fff-native-" tok)
      'com.blockether/rift (str "rift-native-" tok)
      'com.blockether/ruff (str "ruff-native-" tok)
      'com.blockether/imaging (str "imaging-native-" tok)
      'com.blockether/tree-sitter-language-pack (str "tree-sitter-language-pack-native-"
                                                     (pack-native-token))}

     missing-mains
     (->> (keys native-artifacts)
          (remove #(get-in basis [:libs % :mvn/version]))
          vec)]

    (when (seq missing-mains)
      (throw (ex-info "Native build requires every FFM main artifact." {:artifacts missing-mains})))
    (let
      [deps
       (into {}
             (map (fn [[main artifact]]
                    [(symbol "com.blockether" artifact)
                     {:mvn/version (get-in basis [:libs main :mvn/version])}]))
             native-artifacts)

       native-basis
       (b/create-basis {:project nil :extra {:deps deps}})

       jars-by-artifact
       (into {}
             (map (fn [artifact]
                    [artifact
                     (->> (get-in native-basis [:libs artifact :paths])
                          (filter #(str/ends-with? % ".jar"))
                          vec)]))
             (keys deps))

       missing-natives
       (->> jars-by-artifact
            (keep (fn [[artifact jars]]
                    (when (empty? jars) artifact)))
            vec)]

      (when (seq missing-natives)
        (throw (ex-info "Native build requires every FFM native artifact for its target platform."
                        {:artifacts missing-natives
                         :platform tok
                         :tree-sitter-platform (pack-native-token)})))
      (->> (keys deps)
           (mapcat jars-by-artifact)
           vec))))

(defn- native-classpath
  "Classpath for the native build: the AOT classes dir FIRST (so compiled app +
   merged manifest win), then every dependency JAR. We deliberately DROP the
   :local/root source/resource dirs — their compiled+copied form already lives in
   `native-class-dir`, and re-adding them would resurrect the per-extension
   manifest collision. Keeping deps as separate jars lets native-image honor each
   jar's module-info + native-image.properties (polyglot's `ForceOnModulePath`,
   GraalPy's build-time init, etc.)."
  [basis]
  (let
    [jars
     (->>
       (:classpath-roots basis)
       (filter #(str/ends-with? % ".jar"))
       ;; Drop the tools.deps runtime download-fallback (+ any
       ;; cognitect.aws S3 transporter tail) from the NATIVE classpath
       ;; ONLY. A native image bundles/locates natives explicitly and
       ;; never downloads — and cognitect.aws is not native-image-safe
       ;; (objects land in the image heap → build failure). The plain-JVM
       ;; classpath (deps.edn) keeps tools.deps so download still works.
       ;; deps.edn now also :exclusions the maven-s3-transporter, so the aws
       ;; jars are never resolved, downloaded or logged in the first place;
       ;; the pattern stays as belt-and-braces against a transitive re-entry.
       ;; NOTE the AWS-scoped `/com/cognitect/aws/` (NOT /com/cognitect/):
       ;; the broad form also stripped cognitect/transit-clj, which
       ;; clj-kondo.impl.cache requires — that silently failed the whole
       ;; clj-kondo build-time preload chain and left the language-clojure
       ;; extension UNBOUND in the native binary.
       ;; The whole tools.deps TAIL goes with it: tools.deps.edn and the
       ;; Apache maven-resolver stack are reachable only FROM tools.deps, so
       ;; once it is gone they are ~15 dead jars the image builder would
       ;; still scan and build-time-initialize. slf4j-api deliberately STAYS
       ;; (telemere-slf4j binds it).
       (remove
         #(re-find
            #"/org/clojure/tools\.deps/|/org/clojure/tools\.deps\.edn/|/tools\.deps\.maven-s3-transporter/|/com/cognitect/aws/|/org/apache/maven/"
            %)))]
    (->> (concat jars (native-lib-jars basis))
         (into [native-class-dir])
         (str/join java.io.File/pathSeparator))))

(defn- native-platform-token
  "sherpa-onnx / onnxruntime native-lib dir token for the BUILD host
   (e.g. `osx-aarch64`, `linux-x64`). Both jars use this layout."
  []
  (let
    [os
     (str/lower-case (System/getProperty "os.name"))

     arch
     (str/lower-case (System/getProperty "os.arch"))

     a
     (cond (#{"aarch64" "arm64"} arch) "aarch64"
           (#{"x86_64" "amd64" "x64"} arch) "x64"
           :else arch)]

    (cond (str/includes? os "mac") (str "osx-" a)
          :else (str "linux-" a))))

(defn- truffle-platform-tokens
  "[os arch] the GraalPy/Truffle internal-resource dirs use under
   META-INF/resources/ (verified against python-resources jar layout):
   darwin|linux / aarch64|amd64."
  []
  (let
    [os
     (str/lower-case (System/getProperty "os.name"))

     arch
     (str/lower-case (System/getProperty "os.arch"))]

    [(cond (str/includes? os "mac") "darwin"
           :else "linux") (if (#{"aarch64" "arm64"} arch) "aarch64" "amd64")]))

(defn- oracle-native-image?
  "True when the native build KEEPS the GraalPy/Truffle optimizing JIT in the image
   (bigger binary, much longer build, faster CPU-bound Python). Default false = the
   lean interpreter build. Set with `:oracle-native-image true` or the VIS_ORACLE_NATIVE_IMAGE
   env token (1/true/yes/on)."
  [opts]
  (if (some? (:oracle-native-image opts))
    (boolean (:oracle-native-image opts))
    (contains? #{"1" "true" "yes" "on"}
               (some-> (System/getenv "VIS_ORACLE_NATIVE_IMAGE")
                       str/trim
                       str/lower-case))))

(defn- native-image-args
  "native-image CLI args. Config travels INSIDE the classpath jars
   (META-INF/native-image/…); here we add only classpath/main/output, the
   vis-extension/edn/db resource includes, and the build-host voice native libs
   (sherpa-onnx + onnxruntime JNI dylibs) so voice ASR works in the binary.
   The ~465 MB parakeet model is NEVER embedded — it ships separately."
  [basis jit?]
  (let
    [tok
     (native-platform-token)

     [t-os t-arch]
     (truffle-platform-tokens)

     ;; ── Builder JVM heap ────────────────────────────────────────────────
     ;; The points-to analysis live set for this image peaks around 12 GiB, and a
     ;; ceiling only a little above the live set is what kills the build: the old
     ;; generation fills, full GCs take over and native-image dies with "GC
     ;; overhead limit exceeded" after ~20 wasted minutes. native-image's DEFAULT
     ;; is 80% of physical RAM, which lets the builder page instead; GraalVM
     ;; 25.2.x makes both failure modes dramatically worse (see .graalvm-version).
     ;; So pin a deterministic, RAM-clamped ceiling with real headroom over the
     ;; live set — 75% of RAM, up to 26 GiB — and start the heap high enough that
     ;; the builder is not growing it while the analysis is already hot.
     ;; VIS_NATIVE_EXTRA_ARGS is spliced LAST and overrides both.
     total-ram
     (try (.getTotalMemorySize ^com.sun.management.OperatingSystemMXBean
                               (java.lang.management.ManagementFactory/getOperatingSystemMXBean))
          (catch Throwable _ 0))

     heap-gib
     (max 6 (min 26 (long (/ (* 0.75 (double total-ram)) 1073741824.0))))

     init-gib
     (max 2 (quot heap-gib 2))

     ;; Extra native-image args spliced from the environment (space-separated).
     ;; Lets CI tune the builder JVM per-runner (e.g. -J-Xmx6g -J-Xms2g to fit a
     ;; RAM-constrained free macOS runner, overriding GraalPy's bundled -Xms14g
     ;; since command-line -J args are applied AFTER the classpath properties).
     extra
     (some-> (System/getenv "VIS_NATIVE_EXTRA_ARGS")
             str/trim
             not-empty
             (str/split #"\s+"))

     ;; Corporate CA (VIS_CA_CERT / VIS_TRUSTSTORE): the builder JVM and every
     ;; JVM it forks must trust the same roots as the rest of the build, or a
     ;; TLS-intercepting proxy turns a 20-minute image build into a
     ;; SunCertPathBuilderException. `-J` passes them to the builder JVM.
     trust
     (mapv #(str "-J" %) (truststore-properties nil))]

    (cond->
      ["-cp" (native-classpath basis) "-o" native-bin
       ;; Restricted native access (java.lang.foreign): lanterna's TTYDeviceControl
       ;; drives the TTY with termios/ioctl downcalls instead of forking /bin/stty.
       ;; Without this the JDK prints a 4-line "restricted method" warning on the
       ;; first paint — and a future JDK blocks the call outright. The downcall
       ;; DESCRIPTORS themselves are registered in the build Feature
       ;; (com.blockether.vis.internal.nativeimage/-duringSetup).
       "--enable-native-access=ALL-UNNAMED"
       ;; …and that class must decide IN THE BINARY. Its <clinit> builds the
       ;; termios/ioctl MethodHandles; graal-build-time initializes it inside the
       ;; BUILDER JVM, where java.lang.foreign simply works, so the image
       ;; inherited SUPPORTED=true together with handles whose downcall stubs the
       ;; image never generated. The Linux binary then SIGSEGV'd inside
       ;; `DowncallStubsHolder` the first time the TUI opened /dev/tty — measured
       ;; on v0.1.33-v0.1.35 and again on the 2026-08-13 dry run, in
       ;; `native-binary-paints-the-tui-test`, on x64 and arm64 alike. Initialized
       ;; at RUN time it decides for itself: the termios fast path where the
       ;; descriptors were registered (macOS), and lanterna's own catch-and-degrade
       ;; to forking /bin/stty where they were not.
       "--initialize-at-run-time=com.googlecode.lanterna.terminal.ansi.TTYDeviceControl"
       "-H:IncludeResources=META-INF/vis-extension/.*" "-H:IncludeResources=.*\\.edn$"
       ;; the build-written `vis/VERSION` (git sha) read by `vis-agent --version`
       "-H:IncludeResources=vis/VERSION"
       ;; Flyway migration SQL (not in the agent-traced metadata)
       "-H:IncludeResources=db/.*"
       ;; The WHOLE embedded docs corpus (markdown pages + manifest +
       ;; woff2 fonts/logos) — ALL read at RUNTIME via io/resource
       ;; (gateway /docs site AND the `doc-corpus` documents `apropos`/`doc`
       ;; search), and NONE of it in the agent-traced metadata (the trace never
       ;; read a page), so without this pattern the corpus holds zero pages in
       ;; the native binary.
       "-H:IncludeResources=vis-docs/.*"
       ;; Python SHIM sources (resources/vis-shims/*.py), slurped at sandbox
       ;; context creation via io/resource. Without this pattern EVERY shim
       ;; (numpy, pandas, yaml, ...) is missing in the native binary.
       "-H:IncludeResources=vis-shims/.*"
       ;; Python helper sources (resources/vis-python/*.py) slurped at RUNTIME
       ;; via io/resource -- e.g. the packaging-metadata reader `vis-agent python`
       ;; uses to discover a project's import roots.
       "-H:IncludeResources=vis-python/.*"
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
       ;; into Linux images (python-resources ships every
       ;; platform's dirs in one jar) and left the build host's own
       ;; manifests out everywhere else. Host-parameterized instead.
       (str "-H:IncludeResources=META-INF/resources/" t-os "/" t-arch "/native.sha256")
       (str "-H:IncludeResources=META-INF/resources/engine/libtruffleattach/" t-os "/" t-arch "/.*")
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
       ;; ── Locales ─────────────────────────────────────────────────────
       ;; native-image ships ONE locale: whichever one the BUILD HOST
       ;; happened to default to (`-H:DefaultLocale` defaults to the
       ;; builder's). So the binary was non-deterministic — built here it
       ;; carried `en-PL`, on a CI runner `en-US` — and every other locale
       ;; silently degraded to root-ish formatting at runtime, even though
       ;; `-H:+UseSystemLocale` (default on) makes the RUNTIME honor the
       ;; user's `LANG`: honoring a locale that was never embedded is how
       ;; you get English month names and `1,234.5` on a `de_DE` machine.
       ;; Pin the default and embed the set vis actually renders for:
       ;; English (US/GB/IN), Polish, German, Chinese (Simplified/Taiwan)
       ;; and Hindi. Full tags on purpose — the option resolves each entry
       ;; with `Locale/forLanguageTag`, so a bare `zh` is NOT `zh-TW`.
       ;; ~1 MB of CLDR data, not `-H:+IncludeAllLocales` (~20 MB).
       "-H:DefaultLocale=en-US"
       (str "-H:IncludeLocales="
            (str/join "," ["en-US" "en-GB" "en-IN" "pl-PL" "de-DE" "zh-CN" "zh-TW" "hi-IN"]))
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
       "-Os"]
      ;; ── Embedded-Python JIT vs interpreter — the single biggest size lever ──
      ;; DEFAULT (`:oracle-native-image` false): run GraalPy INTERPRETED by forcing Truffle's
      ;; fallback runtime, dropping the Graal JIT from the image. GraalPy documents
      ;; this as ~40% smaller, and it removes the ~18k runtime-compiled Truffle
      ;; methods that otherwise make native-image build slow/hang — right for vis's
      ;; short, I/O-bound python_execution glue. `:oracle-native-image true` KEEPS the
      ;; optimizing runtime for CPU-bound Python, at the cost of a bigger binary and
      ;; a much longer build. The runtime engine adapts to either (build-engine*).
      (not jit?)
      (conj "-Dtruffle.TruffleRuntime=com.oracle.truffle.api.impl.DefaultTruffleRuntime"
            "-Dpolyglot.engine.WarnInterpreterOnly=false")

      ;; ── Auxiliary engine cache — Oracle/JIT image ONLY ──────────────────────
      ;; env_python's `shared-engine` persists the warmed, JIT-compiled Truffle
      ;; code with `Engine.storeCache`/`engine.CacheLoad`, skipping the JVMCI
      ;; warm-up that dominates GraalPy boot. The feature is contributed only by
      ;; the optimizing Truffle runtime, so it exists ONLY in the Oracle/JIT
      ;; image; without these build flags `.storeCache` throws
      ;; UnsupportedOperationException and the runtime silently cold-starts.
      ;; ReservedAuxiliaryImageBytes is address space only, not resident memory;
      ;; both are experimental (UnlockExperimentalVMOptions is set above).
      jit?
      (conj "-H:+AuxiliaryEngineCache" "-H:ReservedAuxiliaryImageBytes=2145482548")

      ;; voice JNI native libs for THIS platform (sherpa + onnxruntime).
      ;; Per-host `tok` keeps foreign-OS libs OUT of each binary; the
      ;; onnxruntime pattern stops at the dir level ([^/]*$) so the macOS
      ;; jar's nested *.dSYM DWARF debug bundles (~8 MB) don't ride in.
      :always
      (conj (str "-H:IncludeResources=native/" tok "/.*")
            (str "-H:IncludeResources=ai/onnxruntime/native/" tok "/[^/]*$"))

      ;; Builder heap ceiling (see total-ram above); VIS_NATIVE_EXTRA_ARGS,
      ;; spliced right after, can still override both -J flags.
      :always
      (conj (str "-J-Xmx" heap-gib "g") (str "-J-Xms" init-gib "g"))

      (seq trust)
      (into trust)

      (seq extra)
      (into extra)

      :always
      (conj "com.blockether.vis.core"))))

(defn native-image-only
  "FAST native-image iteration: re-run native-image ONLY, reusing the existing
   `target/native-classes` from a prior `native` build (no re-AOT). For tuning
   native-image flags; run `native` once first to populate the AOT classes. Honors
   `:oracle-native-image true` (or VIS_ORACLE_NATIVE_IMAGE) to keep the GraalPy JIT in the image."
  [opts]
  (assert-graalvm-ce! :native-image-only opts)
  (resolve-profile opts)
  (let [basis (b/create-basis {:project (root-deps-edn) :aliases [:native]})]
    ;; A prior `package`/uber run can leave a target/vis DIRECTORY behind; the
    ;; builder then dies at [8/8] Creating image with "Path exists as directory".
    (b/delete {:path native-bin})
    (println "native-image (reusing target/native-classes)…")
    (let
      [{:keys [exit]} (b/process {:command-args
                                  (into [(native-image-command)]
                                        (native-image-args basis (oracle-native-image? opts)))})]
      (if (zero? exit)
        (println "-> built" native-bin)
        (throw (ex-info "native-image build failed" {:exit exit}))))))

(defn native
  "Build the private Vis Agent native runtime and its intermediate AOT jar:
     1. `target/vis.jar`  — build artifact, never a selectable distribution
     2. `target/vis`      — private native runtime behind `bin/vis-agent`
   They share one AOT pass. Requires `native-image` on PATH (GraalVM CE 25.1.3)
   and ≥16 GB RAM (GraalPy's libpythonvm needs -Xms14g). Releases always package
   `bin/vis-agent` together with the native runtime.

   Options:
     :profile :community — the ONE distribution and the default: every extension
                           (all channels, voice ASR, web `search`). Accepted for
                           compatibility; there is nothing else to pick.
     :oracle-native-image true — KEEP the GraalPy JIT in the image (bigger binary, slower
                           build, faster CPU-bound Python). Default: lean interpreter.
     :auto-install-graalvm false — keep a missing GraalVM CE a hard error instead of
                           installing the pin (VIS_AUTO_INSTALL_GRAALVM=0 does the same).
                           By default a missing pin is downloaded + installed, and an
                           already-installed pinned JDK is switched to automatically."
  [opts]
  (assert-graalvm-ce! :native opts)
  (resolve-profile opts)
  (let [basis (prepare-native-classes!)]
    ;; (1) Intermediate AOT uberjar for build tooling. Never shipped or selected at runtime.
    (b/delete {:path native-uber})
    (b/uber {:class-dir native-class-dir
             :uber-file native-uber
             :basis basis
             :main 'com.blockether.vis.core
             :exclude uber-exclusions})
    (println "->" native-uber)
    ;; A prior `package`/uber run can leave a target/vis DIRECTORY behind; the
    ;; builder then dies at [8/8] Creating image with "Path exists as directory".
    (b/delete {:path native-bin})
    ;; (2) Private native runtime. Built from a classpath of real jars (NOT the
    ;; uberjar) so polyglot/graalpy keep their module-info + native-image.properties.
    (println "native-image:" native-bin "(community)" "(this takes several minutes)…")
    (let
      [{:keys [exit]} (b/process {:command-args
                                  (into [(native-image-command)]
                                        (native-image-args basis (oracle-native-image? opts)))})]
      (if (zero? exit)
        (println "-> built" native-bin)
        (throw (ex-info "native-image build failed" {:exit exit}))))))
