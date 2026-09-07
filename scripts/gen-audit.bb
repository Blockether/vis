#!/usr/bin/env bb
;; Regenerate audit/README.md from the deps.edn graph.
;;
;; Walks the root deps.edn + every sibling deps.edn (skipping e2e test
;; fixtures), collects every DIRECT Maven or git coordinate (top-level :deps plus
;; each alias's :extra-deps / :replace-deps / :deps), de-duplicates so each
;; coordinate is reported once under the FIRST module that declares it, then
;; resolves each artifact's license and distributable size from its source.
;; From that it renders the whole audit/README.md: the dependency inventory, the
;; license distribution, the resource footprint, and a copyleft warning that
;; appears automatically whenever an (L)GPL license is detected.
;;
;; Usage:
;;   bb scripts/gen-audit.bb            # rewrite audit/README.md in place
;;   bb scripts/gen-audit.bb --check    # exit 1 if audit/README.md is out of date
;;
;; Deterministic + offline-friendly: network failures degrade a single cell to
;; "UNKNOWN"/"—" rather than aborting. Run after any dependency bump; CI keeps
;; it fresh (see .github/workflows/audit-md.yml).

(require '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[babashka.fs :as fs]
         '[babashka.http-client :as http])

(def repos ["https://repo.clojars.org" "https://repo1.maven.org/maven2"])

;; Module dir-name -> one-line blurb for its inventory section. Unknown modules
;; still render (just without a blurb), so a new module never breaks the doc.
(def blurbs
  {"core" "_Core runtime — the `vis-agent` CLI, agent loop, HTTP gateway, sandbox._"
   "vis-channel-tui" "_Terminal UI (Lanterna)._"})

;; Licenses that POMs express oddly / not at all — pin them explicitly so the
;; audit never regresses to UNKNOWN on the ones we've already vetted by hand.
(def license-overrides
  {;; k2-fsa publishes sherpa-onnx through JitPack, which serves an
   ;; install:install-file POM with no <licenses> block. Apache-2.0 is the
   ;; license of the k2-fsa/sherpa-onnx repository these artifacts are built
   ;; from, vetted by hand at v1.13.5. The five native-lib coordinates are no
   ;; longer declared in any deps.edn — build.clj resolves the BUILD host's for
   ;; the image and the extension fetches the RUNNING host's on demand — so they
   ;; no longer reach the inventory below. They stay named here because they are
   ;; still shipped, and a license vetted by hand must not silently go UNKNOWN.
   "com.github.k2-fsa.sherpa-onnx/sherpa-onnx-jvm" "Apache-2.0"
   "com.github.k2-fsa.sherpa-onnx/sherpa-onnx-native-lib-osx-aarch64" "Apache-2.0"
   "com.github.k2-fsa.sherpa-onnx/sherpa-onnx-native-lib-osx-x64" "Apache-2.0"
   "com.github.k2-fsa.sherpa-onnx/sherpa-onnx-native-lib-linux-x64" "Apache-2.0"
   "com.github.k2-fsa.sherpa-onnx/sherpa-onnx-native-lib-linux-aarch64" "Apache-2.0"
   "com.github.k2-fsa.sherpa-onnx/sherpa-onnx-native-lib-win-x64" "Apache-2.0"
   ;; These git dependencies have no Maven POM to carry their repository license.
   "com.blockether/vis-python-runtime" "MIT"
   "io.github.clj-holmes/clj-watson" "EPL-2.0"})

;; ---------------------------------------------------------------- deps parsing

(defn- coords-from-map
  "Pull {sym display-version} pairs out of a :deps-style map."
  [m]
  (into {}
        (keep (fn [[sym coord]]
                (when (and (symbol? sym) (map? coord))
                  (when-let [version (cond (:mvn/version coord) (:mvn/version coord)
                                           (:git/sha coord) (str "git:" (:git/sha coord)))]
                    [sym version]))))
        m))

(defn- module-coords
  "Every direct Maven or git coord a deps.edn declares."
  [deps]
  (apply merge
    (coords-from-map (:deps deps))
    (for [[_ a] (:aliases deps)]
      (merge (coords-from-map (:extra-deps a))
             (coords-from-map (:replace-deps a))
             (coords-from-map (:deps a))))))

(defn- module-label
  [rel-path]
  (if (= rel-path "deps.edn") "core" (fs/file-name (fs/parent rel-path))))

(defn- discover-modules
  "Ordered [label rel-path coords] for every deps.edn with Maven or git coords,
   core first then the rest by path. e2e scenario fixtures are skipped."
  [root]
  (let [edns (->> (cons (fs/file root "deps.edn") (fs/glob root "**/deps.edn"))
                  (map #(str (fs/relativize root %)))
                  distinct
                  ;; Anchored on a path SEGMENT: a top-level `target/` (a stale
                  ;; build copy of an older checkout, GraalPy deps and all) has
                  ;; no leading slash to match, and inventoried this repository
                  ;; twice with dependencies it no longer has.
                  (remove #(re-find #"(^|/)(e2e|target)/" %))
                  sort
                  (sort-by #(if (= % "deps.edn") "" %)))]
    (for [rel edns
          :let [deps (edn/read-string (slurp (fs/file root rel)))
                coords (module-coords deps)]
          :when (seq coords)]

      [(module-label rel) rel coords])))

;; ------------------------------------------------------------- artifact lookup

(defn- coord->path [sym] (str (str/replace (namespace sym) "." "/") "/" (name sym)))

(defn- http-ok? [resp] (= 200 (:status resp)))

(defn- fetch-first
  "GET/HEAD `f` against each repo, returning the first 200 response body/headers."
  [method sym version]
  (some (fn [repo]
          (let [ext
                (if (= method :pom) "pom" "jar")

                url
                (format "%s/%s/%s/%s-%s.%s" repo (coord->path sym) version (name sym) version ext)

                verb
                (if (= method :pom) http/get http/head)]

            (try (let [resp (verb url {:throw false :headers {"User-Agent" "vis-audit"}})]
                   (when (http-ok? resp) resp))
                 (catch Exception _ nil))))
        repos))

(defn- normalize-license
  "Map a raw POM <license><name> string to a short SPDX-ish id."
  [raw]
  (let [s (str/lower-case (str raw))]
    (cond (str/blank? s) "UNKNOWN"
          (or (str/includes? s "lesser general public") (str/includes? s "lgpl")) "LGPL-3.0"
          (or (str/includes? s "gnu general public") (re-find #"\bgpl\b" s)) "GPL"
          (str/includes? s "eclipse public") (if (str/includes? s "2.0") "EPL-2.0" "EPL-1.0")
          (str/includes? s "apache") "Apache-2.0"
          (str/includes? s "universal permissive") "UPL-1.0"
          (re-find #"\bupl\b" s) "UPL-1.0"
          (str/includes? s "bsd") (cond (str/includes? s "2") "BSD-2-Clause"
                                        (str/includes? s "3") "BSD-3-Clause"
                                        :else "BSD")
          (re-find #"\bmit\b" s) "MIT"
          (or (str/includes? s "public domain")
              (str/includes? s "unlicense")
              (str/includes? s "cc0"))
          "Public-Domain"
          (or (str/includes? s "python software") (str/includes? s "psf")) "PSF"
          :else raw)))

(defn- pom-license
  [pom-body]
  (when pom-body
    (when-let [m (re-find #"(?s)<licenses>.*?<name>(.*?)</name>" pom-body)]
      (normalize-license (str/trim (second m))))))

(defn- pom-url
  [repo group artifact version]
  (format "%s/%s/%s/%s/%s-%s.pom"
          repo
          (str/replace group "." "/")
          artifact
          version
          artifact
          version))

(defn- m2-file
  "The locally cached ~/.m2 artifact, when Maven already resolved this
   coordinate. That cache is exactly what the build consumed, so it is a
   truthful offline source for both the license and the jar size."
  [group artifact version ext]
  (let [f (fs/file (fs/home)
                   ".m2"
                   "repository"
                   (str/replace group "." "/")
                   artifact
                   version
                   (format "%s-%s.%s" artifact version ext))]
    (when (fs/exists? f) f)))

(defn- fetch-pom-body
  [group artifact version]
  (or (some (fn [repo]
              (try (let [r (http/get (pom-url repo group artifact version)
                                     {:throw false :headers {"User-Agent" "vis-audit"}})]
                     (when (http-ok? r) (:body r)))
                   (catch Exception _ nil)))
            repos)
      (some-> (m2-file group artifact version "pom")
              slurp)))

(defn- parent-coords
  "[group artifact version] of the POM's <parent>, if any."
  [pom]
  (when-let [seg (some-> (re-find #"(?s)<parent>(.*?)</parent>" pom)
                         second)]
    (let [g (some-> (re-find #"<groupId>(.*?)</groupId>" seg)
                    second
                    str/trim)
          a (some-> (re-find #"<artifactId>(.*?)</artifactId>" seg)
                    second
                    str/trim)
          v (some-> (re-find #"<version>(.*?)</version>" seg)
                    second
                    str/trim)]

      (when (and g a v) [g a v]))))

(defn- resolve-license
  "License for group/artifact/version, following <parent> POMs when a child
   inherits its license (Apache Commons, Flyway, …). Bounded recursion."
  ([group artifact version] (resolve-license group artifact version 0))
  ([group artifact version ^long depth]
   (when (and version (< depth 6))
     (when-let [pom (fetch-pom-body group artifact version)]
       (or (pom-license pom)
           (when-let [[g a v] (parent-coords pom)]
             (resolve-license g a v (inc depth))))))))

(defn- jar-size-bytes
  [head-resp]
  (some-> head-resp
          :headers
          (get "content-length")
          parse-long))

(defn- fmt-size
  "Human size for one artifact. Locale-INDEPENDENT on purpose: `format` would
   otherwise render `1,1 MB` under a comma-decimal locale and churn the whole
   document (and fail `--check`) on any developer machine that is not en_US."
  [bytes]
  (if (nil? bytes)
    "—"
    (let [n (long bytes)]
      (if (>= n (* 1024 1024))
        (String/format java.util.Locale/ROOT
                       "%.1f MB"
                       (into-array Object [(/ (double n) 1024.0 1024.0)]))
        (format "%d KB" (long (Math/round (/ (double n) 1024.0))))))))

(defn- reported-size
  "The exact string rendered in one inventory size cell."
  [info]
  (or (:size info) (fmt-size (:size-bytes info))))

(defn- reported-size-bytes
  "Numeric counterpart of `reported-size` for aggregates and thresholds.
   Derive calculations from the rendered cell so a clean CI checkout that must
   reuse a vetted size produces the same document as a populated Maven cache."
  [info]
  (when-let [[_ amount unit] (re-matches #"([0-9]+(?:\.[0-9]+)?) (KB|MB)" (reported-size info))]
    (* (Double/parseDouble amount)
       (case unit
         "KB"
         1024.0

         "MB"
         1048576.0))))

(defn- in-house? [sym] (= "com.blockether" (namespace sym)))

(defn- previous-rows
  "The license + jar-size cells audit/README.md ALREADY states, keyed by
   \"coord version\". This file is a compliance record: when a lookup fails —
   a just-released artifact the CDN has not published yet, or no network — the
   vetted value for that exact version is reused instead of silently regressing
   a real license to UNKNOWN/—."
  [root]
  (let [f (fs/file root "audit" "README.md")]
    (if-not (fs/exists? f)
      {}
      (into {}
            (keep (fn [line]
                    (when-let [[_ sym version lic size]
                               (re-matches
                                 #"\|\s+`([^`]+)`\s+\|\s+`([^`]+)`\s+\|([^|]+)\|([^|]+)\|.*"
                                 line)]
                      [(str sym " " version) {:license (str/trim lic) :size (str/trim size)}])))
            (str/split-lines (slurp f))))))

(defn- artifact-info
  "License + distributable size for one coordinate. Git dependencies are source
   checkouts, not jars, so their inventory cell says that instead of inventing a
   package size. `prev` preserves vetted Maven data across transient failures."
  [prev sym version]
  (binding [*out* *err*]
    (println "  ·" (str sym) version))
  (if (str/starts-with? version "git:")
    {:license (or (license-overrides (str sym)) "UNKNOWN") :size "source checkout"}
    (if (contains? #{"RELEASE" "LATEST"} version)
      {:license "(floating)" :size-bytes nil :floating true}
      (let [head
            (fetch-first :head sym version)

            known
            (get prev (str sym " " version))

            bytes
            (or (jar-size-bytes head)
                (some-> (m2-file (namespace sym) (name sym) version "jar")
                        fs/size))]

        {:license (or (license-overrides (str sym))
                      (resolve-license (namespace sym) (name sym) version)
                      (:license known)
                      "UNKNOWN")
         :size-bytes bytes
         :size (when (nil? bytes) (:size known))}))))

;; ------------------------------------------------------------------- rendering

(defn- inventory-rows
  "De-duped [module sym version info] rows: each coord under its first module."
  [prev modules]
  (let [seen (atom #{})]
    (for [[label _ coords] modules
          [sym version] (sort-by (comp str first) coords)
          :when (not (@seen sym))
          :let [_ (swap! seen conj sym)]]

      [label sym version (artifact-info prev sym version)])))

(defn- section-header
  [label]
  (if (= label "core") "### core (deps.edn)" (format "### `%s` module" label)))

(defn- render-module
  [label rows]
  (str/join "\n"
            (concat [(section-header label) ""]
                    (when-let [b (blurbs label)]
                      [b ""])
                    ["| Dependency | Version | License | Jar size | Ownership |"
                     "|---|---|---|---|---|"]
                    (for [[_ sym version info] rows]
                      (format "| `%s` | `%s` | %s | %s | %s |"
                              (str sym)
                              version
                              (:license info)
                              (reported-size info)
                              (if (in-house? sym) "Blockether (in-house)" "3rd-party")))
                    [""])))

(def ^:private date-placeholder "@@GENERATED-DATE@@")

(defn- stamp-date
  "Resolve `body`'s date placeholder against the committed document.

   CI regenerates this file and compares it byte-for-byte with the copy in git,
   so a wall-clock date would fail that gate on any push made on a later day
   than the last regeneration — with the date line as the ONLY diff. Keep the
   date the committed document already states while the rest of the document is
   identical, and move it (UTC, the zone CI runs in) only when the content
   really changed."
  [root body]
  (let [f
        (fs/file root "audit" "README.md")

        current
        (when (fs/exists? f) (slurp f))

        line
        #"(?m)^> Generated (\d{4}-\d{2}-\d{2})\.$"

        stated
        (second (re-find line (or current "")))

        today
        (subs (str (java.time.LocalDate/now java.time.ZoneOffset/UTC)) 0 10)

        kept?
        (and stated (= body (str/replace current line (str "> Generated " date-placeholder "."))))]

    (str/replace body date-placeholder (if kept? stated today))))

(defn- gen
  [root]
  (binding [*out* *err*]
    (println "Discovering modules…"))
  (let [modules
        (discover-modules root)

        rows
        (vec (inventory-rows (previous-rows root) modules))

        by-mod
        (group-by first rows)

        licenses
        (frequencies (map (comp :license #(nth % 3)) rows))

        heavy
        (->> rows
             (keep (fn [[_ sym v info]]
                     (when-let [b (reported-size-bytes info)]
                       (when (>= b (* 1024 1024)) [sym v b]))))
             (sort-by #(- (nth % 2))))

        total-b
        (reduce + 0 (keep #(reported-size-bytes (nth % 3)) rows))

        copyleft
        (filter #(re-find #"GPL" (str (:license (nth % 3)))) rows)

        ;; The date is a placeholder until `stamp-date` decides whether it may move.
        today
        date-placeholder]

    (stamp-date
      root
      (str
        "# Vis — Security & Dependency Audit

> Generated "
        today
        ".

Vis is a coding agent that runs Python, uses tools to inspect and change
projects, and stores sessions locally. The `vis-agent` wrapper starts a JVM
runtime or a GraalVM native binary.

This generated document lists dependencies, licenses, vulnerability-scan
locations and data handling. Blockether maintains it for technical and
procurement review. It is not a certification of security or regulatory
compliance.

---

## 1. Product

- **Execution:** the model runs Python in the bundled CPython runtime and uses
  host tools to read files, edit code and run tests. Gateway sessions have
  separate worker processes.
- **State:** sessions and results are stored locally, outside model requests.
  Configured providers receive the context required for inference.
- **Distribution:** `vis-agent` selects a managed JVM source checkout or native
  binary. Language packs, providers, persistence and speech are part of the
  engine; the TUI is a separate app module.
- **Network:** providers, tools, package downloads and notifications can make
  network requests. See §9 for data handling.

### Summary

- **Source repository:** <https://github.com/Blockether/vis> — issues, releases, CI and the Security tab.
- **Primary language:** Clojure 1.12 on the JVM (Java 25 / GraalVM), compiled to a native image.
- **Direct dependency coordinates:** "
        (count rows)
        " unique, across "
        (count modules)
        " `deps.edn` modules (root + siblings).
- **Total direct jar size:** ~"
        (format "%.0f" (/ total-b 1024.0 1024.0))
        " MB; most space is used by Python and optional speech components (§8).
- **Licenses:** dependencies include EPL, MIT, Apache-2.0, BSD and UPL — "
        (if (seq copyleft) "**copyleft exception(s) flagged in §6.**" "no copyleft exceptions.")
        "
- **Vulnerability scans:** [clj-watson](https://github.com/clj-holmes/clj-watson) runs on dependency changes, weekly and on request. Results are published to GitHub Security (§7).

---

## 2. Maintainer

Vis is built and maintained by **Blockether** (BLOCKETHER SP. Z O.O.),
a software consultancy and development company based in Kraków, Poland
(KRS 0001171097, NIP 675-18-13-221).

Blockether develops data, AI, web, cloud and blockchain software and maintains
open-source projects including `svar` and `holy-lambda`.
- **Contact.** General enquiries and commercial support:
  <contact@blockether.com>. Security disclosure: <security@blockether.com>
  (§11). Web: <https://blockether.com>. Source repository:
  <https://github.com/Blockether/vis>.

---

## 3. Trademarks & attributions

Names used in this document identify third-party technologies and remain the
property of their respective owners; their use here is descriptive and does
**not** imply endorsement.

- **GraalVM™**, **Oracle®**, **Java™** and **OpenJDK** are trademarks of Oracle
  and/or its affiliates. Vis embeds and builds with the **Community Edition**
  distribution (§4).
- **Python®** and the Python logo are trademarks of the **Python Software
  Foundation (PSF)**; the bundled CPython standard library is under the PSF
  License.
- **Clojure** is maintained by the Clojure core team / Nubank.
- All other product, project and company names (clj-watson, Lanterna, ONNX
  Runtime, SQLite, Jackson, etc.) are trademarks or trade names of their
  respective holders. Per-artifact licenses are inventoried in §5–§6.

---

## 4. Technology & distribution

Both distributions use the `vis-agent` wrapper. Native installations include a
GraalVM native binary; source installations run the managed JVM checkout.
Build tools, runtime libraries and model files have separate license terms.
Review the applicable licenses before redistribution.

### 4.1 The build tool and the embedded interpreter

| Layer | What it is | Coordinates / tool | License | Redistribution *for a fee* |
|---|---|---|---|---|
| **Embedded interpreter** | CPython 3.14, included beside the binary | `com.blockether/vis-python-runtime` at a pinned Git commit and platform release | **MIT** bridge; **PSF-2.0** CPython | Subject to those licenses |
| **Build tool** | GraalVM native-image compiler | **GraalVM CE**, version pinned by `.graalvm-version` | **GPL-2.0 with Classpath Exception** | Review the exception and bundled-library terms |

The build uses the Community Edition version pinned in `.graalvm-version`,
not Oracle GraalVM. Compiler licensing and the licenses of code included in
the resulting binary are separate considerations.

### 4.2 Python and speech

The core runtime requires CPython 3.14 and its standard library, supplied by
`com.blockether/vis-python-runtime` and a platform archive. The bridge is MIT;
CPython is PSF-2.0.

Local speech uses `sherpa-onnx` (Apache-2.0) and ONNX Runtime (MIT). Its API jar
is a declared dependency; platform-specific native libraries are loaded
separately. Some native builds include eSpeak NG (GPL-3.0), which requires
separate license review. Model licenses and download sources are listed in
[THIRD_PARTY_MODELS.md](../THIRD_PARTY_MODELS.md), including models with
non-commercial restrictions. Do not infer model permissions from library
licenses.

### 4.3 Lanterna

The optional TUI uses `com.blockether/lanterna`, a Blockether-maintained fork
of Lanterna under LGPL-3.0. Maintaining a fork does not remove upstream license
obligations. Review notice, source and relinking requirements for the chosen
distribution method, particularly static native-image builds. §6 lists the
dependency automatically.

### Redistribution

Vis's first-party code is Apache-2.0. Redistribution must also comply with
dependency and model licenses. A commercial agreement with Blockether does
not replace third-party license obligations.

---

## 5. Dependency inventory

Grouped by the module that declares each dependency; a coordinate shared by
several modules is listed once, under the first module that declares it. Jar
sizes are the direct artifact only (not the transitive closure). \"Ownership\"
identifies Blockether-maintained libraries and third-party projects.

"
        (str/join "\n"
                  (for [[label _ _]
                        modules

                        :let [mrows
                              (by-mod label)]
                        :when (seq mrows)]

                    (render-module label mrows)))
        "
---

## 6. Licenses & code ownership

### License distribution (direct coordinates)

| License | Count |
|---|---|
"
        (str/join "\n"
                  (for [[lic n] (sort-by (comp - val) licenses)]
                    (format "| %s | %d |" lic n)))
        "

Dependencies use several licenses, listed above. Vis first-party code uses
Apache-2.0. Review dependency terms for the intended distribution method"
        (if (seq copyleft)
          (str
            " — **with the copyleft exception(s) below that need legal sign-off:**\n\n"
            (str/join
              "\n"
              (for [[_ sym v info] copyleft]
                (format
                  "> **Copyleft: `%s` (`%s`) uses %s.** Review source, notice and relinking
> obligations before distribution, particularly for static native-image builds.
> Keeping an optional dependency separate may affect those obligations (§4.3)."
                  (str sym)
                  v
                  (:license info)))))
          " with no copyleft exceptions.")
        "

### Code ownership

- **First-party (this repo, Apache-2.0):** the `com.blockether.vis.core` package
  and every sibling module in this tree.
- **Blockether-maintained libraries:** `com.blockether/*` coordinates above,
  including forks with upstream copyrights and license obligations.
- **3rd-party:** everything else in §5, sourced from its declared Maven or git repository.

---

## 7. Vulnerability scans

### Scan results

View published findings in GitHub **Security → Code scanning**. Check each
scan's date and status; this generated inventory does not report current CVEs:

**<https://github.com/Blockether/vis/security/code-scanning>**

Every [clj-watson](https://github.com/clj-holmes/clj-watson) run resolves the
*entire* dependency graph (direct **and** transitive), matches each artifact
against a vulnerability database on every dependency change, and publishes its
findings (SARIF) there. Each run is also archived as a downloadable build
artifact under **Actions → Security audit**
(<https://github.com/Blockether/vis/actions/workflows/security-audit.yml>). This
document is not the system of record for live CVE state — the Security tab is.

### Scanner

[clj-watson](https://github.com/clj-holmes/clj-watson) is an EPL-2.0 Software
Composition Analysis tool for Clojure. It checks direct and transitive
dependencies against vulnerability databases. Results depend on the resolved
dependency graph and available advisory data; a scan is not a source-code
security review.

### How it runs here

**Locally** — use the pinned `:clj-watson` alias in `deps.edn`:

```bash
# github-advisory strategy: only needs a GitHub token, no NVD download.
GITHUB_TOKEN=<your-token> clojure -M:clj-watson scan -p deps.edn -a '*' -t github-advisory -s
```

`-a '*'` includes every alias so the scan covers the root package **and** all
`:local/root` siblings; `-s` adds fix suggestions.

**In CI** — `.github/workflows/security-audit.yml` runs the scan on every
`deps.edn` change, on pull requests, weekly (Mondays 06:00 UTC), and via manual
dispatch. It emits **SARIF**, uploads it to GitHub **code scanning** (findings
appear in *Security → Code scanning*) and archives it as a build artifact. The
scan does not fail builds by default. Enable `-f` or a `-c <cvss>` threshold
to fail on findings. The `nvd-scan` job uses OWASP Dependency-Check for NVD
results weekly and on manual dispatch when `NVD_API_KEY` is configured; it
skips otherwise.

| Strategy | Source | Auth / cost | When |
|---|---|---|---|
| `github-advisory` (default) | GitHub Advisory Database (GraphQL) | `GITHUB_TOKEN` | CI and local runs |
| `dependency-check` | NIST NVD (OWASP Dependency-Check) | needs a free **NVD API key**, downloads the full NVD DB on first run (cached in `~/.m2`) | deeper CVSS coverage / compliance |

---

## 8. Resource sizes

Direct artifacts of at least 1 MB:

| Dependency | Version | Jar size |
|---|---|---|
"
        (str/join "\n"
                  (for [[sym v b] heavy]
                    (format "| `%s` | `%s` | %s |" (str sym) v (fmt-size b))))
        "

Notes:
- CPython is required by the core runtime and installed beside the binary (§4.2).
- Speech's platform-specific JNI and ONNX Runtime libraries are loaded
  separately and are not included in the direct-dependency table.
- The core session store includes `sqlite-jdbc`.
- Native binaries include JDK code. Track their size and the separate Python
  runtime directory in native-release output.

---

## 9. Data governance

- **Local storage:** sessions, tool output and traces are stored on the gateway
  machine. Exports can contain private data and are not automatically redacted.
- **Model requests:** configured providers receive prompts and selected session
  context, which may include project files and tool output.
- **Tools and downloads:** extensions, MCP servers, shell commands and package
  or model installation can contact external services. Apply filesystem and
  network policy according to the deployment's requirements.
- **Notifications:** enabled push notifications send session titles, identifiers
  and answer previews to the publisher's relay and platform push provider.
  The relay processes notification content; it does not receive the full
  transcript in that payload. See [PRIVACY.md](../PRIVACY.md).
- **Speech:** built-in speech processing runs locally after model downloads.
  External speech services used by extensions have their own data handling.
- **Telemetry:** the app includes no analytics or advertising SDK. Local
  diagnostics and metrics do not imply that all network traffic remains local.

---

## 10. Commercial licensing, support & warranty

Vis is distributed under Apache-2.0, including its warranty and liability
limitations. Third-party dependencies retain their own terms.

Commercial support, service levels, warranties or indemnification apply only
where a signed agreement provides them. Contact <contact@blockether.com>
for installation, integration, maintenance, extension development or security
review support. No such commitments are implied by this inventory.

---

## 11. Security contact & vulnerability disclosure

Report suspected vulnerabilities in Vis privately to
**<security@blockether.com>**. Please include affected version / commit,
reproduction steps and impact. Do **not** open a public issue for an
undisclosed vulnerability. Coordinated-disclosure timelines and any remediation
SLA are governed by a commercial agreement (§10); absent one, Blockether
addresses reports on a best-effort basis.

Report ordinary bugs and feature requests at
<https://github.com/Blockether/vis/issues>. Keep undisclosed security reports
private and send them to <security@blockether.com>.

"))))

;; --------------------------------------------------------------------- runner

(let [root
      (str (fs/cwd))

      target
      (fs/file root "audit" "README.md")

      md
      (gen root)

      check?
      (some #{"--check"} *command-line-args*)]

  (if check?
    (let [current (when (fs/exists? target) (slurp target))]
      (if (= current md)
        (do (println "audit/README.md is up to date.") (System/exit 0))
        (do (binding [*out* *err*]
              (println "audit/README.md is STALE — run `bb scripts/gen-audit.bb`."))
            (System/exit 1))))
    (do (fs/create-dirs (fs/parent target))
        (spit target md)
        (println "Wrote" (str (fs/relativize root target)))
        (System/exit 0))))
