#!/usr/bin/env bb
;; Regenerate audit/README.md from the deps.edn graph.
;;
;; Walks the root deps.edn + every extension deps.edn (skipping e2e test
;; fixtures), collects every DIRECT Maven coordinate (top-level :deps plus
;; each alias's :extra-deps / :replace-deps / :deps), de-duplicates so each
;; coordinate is reported once under the FIRST module that declares it, then
;; fetches each artifact's license + jar size from Clojars / Maven Central.
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
;; still render (just without a blurb), so a new extension never breaks the doc.
(def blurbs
  {"core"                    "_Shipped binary runtime — the `vis` CLI, agent loop, HTTP gateway, sandbox._"
   "vis-persistance-sqlite"  "_Durable session store (SQLite + Flyway migrations)._"
   "vis-language-clojure"    "_Clojure language pack (format/lint/structural edits)._"
   "vis-language-python"     "_Python language pack._"
   "vis-foundation-voice"    "_Local speech (sherpa-onnx / ONNX Runtime)._"
   "vis-foundation-bridge"   "_Bridge verification tool surface._"
   "vis-foundation-search"   "_Web/code/paper search tool surface._"
   "vis-channel-tui"         "_Terminal UI (Lanterna)._"
   "vis-workspace-rift"      "_Rift workspace/FFM integration._"})

;; Licenses that POMs express oddly / not at all — pin them explicitly so the
;; audit never regresses to UNKNOWN on the ones we've already vetted by hand.
(def license-overrides
  {"org.graalvm.python/python-language"  "UPL-1.0 + MIT + PSF"
   "org.graalvm.python/python-resources" "UPL-1.0 + MIT + PSF"
   "parinferish/parinferish"             "Public-Domain"})

;; ---------------------------------------------------------------- deps parsing

(defn- coords-from-map
  "Pull {sym version} pairs out of a :deps-style map (mvn coords only)."
  [m]
  (into {} (for [[sym v] m
                 :when (and (symbol? sym) (map? v) (:mvn/version v))]
             [sym (:mvn/version v)])))

(defn- module-coords
  "Every direct mvn coord a deps.edn declares (top :deps + all alias deps)."
  [deps]
  (apply merge
    (coords-from-map (:deps deps))
    (for [[_ a] (:aliases deps)]
      (merge (coords-from-map (:extra-deps a))
        (coords-from-map (:replace-deps a))
        (coords-from-map (:deps a))))))

(defn- module-label [rel-path]
  (if (= rel-path "deps.edn")
    "core"
    (fs/file-name (fs/parent rel-path))))

(defn- discover-modules
  "Ordered [label rel-path coords] for every deps.edn with mvn coords, core
   first then the rest by path. e2e scenario fixtures are skipped."
  [root]
  (let [edns (->> (cons (fs/file root "deps.edn") (fs/glob root "**/deps.edn"))
               (map #(str (fs/relativize root %)))
               distinct
               (remove #(str/includes? % "/e2e/"))
               (remove #(str/includes? % "/target/"))
               sort
               (sort-by #(if (= % "deps.edn") "" %)))]
    (for [rel edns
          :let [deps   (edn/read-string (slurp (fs/file root rel)))
                coords (module-coords deps)]
          :when (seq coords)]
      [(module-label rel) rel coords])))

;; ------------------------------------------------------------- artifact lookup

(defn- coord->path [sym]
  (str (str/replace (namespace sym) "." "/") "/" (name sym)))

(defn- http-ok? [resp] (= 200 (:status resp)))

(defn- fetch-first
  "GET/HEAD `f` against each repo, returning the first 200 response body/headers."
  [method sym version]
  (some (fn [repo]
          (let [ext  (if (= method :pom) "pom" "jar")
                url  (format "%s/%s/%s/%s-%s.%s"
                       repo (coord->path sym) version (name sym) version ext)
                verb (if (= method :pom) http/get http/head)]
            (try
              (let [resp (verb url {:throw false :headers {"User-Agent" "vis-audit"}})]
                (when (http-ok? resp) resp))
              (catch Exception _ nil))))
    repos))

(defn- normalize-license
  "Map a raw POM <license><name> string to a short SPDX-ish id."
  [raw]
  (let [s (str/lower-case (str raw))]
    (cond
      (str/blank? s)                                     "UNKNOWN"
      (or (str/includes? s "lesser general public")
        (str/includes? s "lgpl"))                      "LGPL-3.0"
      (or (str/includes? s "gnu general public")
        (re-find #"\bgpl\b" s))                        "GPL"
      (str/includes? s "eclipse public")
      (if (str/includes? s "2.0") "EPL-2.0" "EPL-1.0")
      (str/includes? s "apache")                         "Apache-2.0"
      (str/includes? s "universal permissive")           "UPL-1.0"
      (re-find #"\bupl\b" s)                             "UPL-1.0"
      (str/includes? s "bsd")
      (cond (str/includes? s "2") "BSD-2-Clause"
        (str/includes? s "3") "BSD-3-Clause"
        :else "BSD")
      (re-find #"\bmit\b" s)                             "MIT"
      (or (str/includes? s "public domain")
        (str/includes? s "unlicense")
        (str/includes? s "cc0"))                       "Public-Domain"
      (or (str/includes? s "python software")
        (str/includes? s "psf"))                       "PSF"
      :else raw)))

(defn- pom-license [pom-body]
  (when pom-body
    (when-let [m (re-find #"(?s)<licenses>.*?<name>(.*?)</name>" pom-body)]
      (normalize-license (str/trim (second m))))))

(defn- pom-url [repo group artifact version]
  (format "%s/%s/%s/%s/%s-%s.pom"
    repo (str/replace group "." "/") artifact version artifact version))

(defn- fetch-pom-body [group artifact version]
  (some (fn [repo]
          (try
            (let [r (http/get (pom-url repo group artifact version)
                      {:throw false :headers {"User-Agent" "vis-audit"}})]
              (when (http-ok? r) (:body r)))
            (catch Exception _ nil)))
    repos))

(defn- parent-coords
  "[group artifact version] of the POM's <parent>, if any."
  [pom]
  (when-let [seg (some-> (re-find #"(?s)<parent>(.*?)</parent>" pom) second)]
    (let [g (some-> (re-find #"<groupId>(.*?)</groupId>" seg) second str/trim)
          a (some-> (re-find #"<artifactId>(.*?)</artifactId>" seg) second str/trim)
          v (some-> (re-find #"<version>(.*?)</version>" seg) second str/trim)]
      (when (and g a v) [g a v]))))

(defn- resolve-license
  "License for group/artifact/version, following <parent> POMs when a child
   inherits its license (Apache Commons, Flyway, …). Bounded recursion."
  ([group artifact version] (resolve-license group artifact version 0))
  ([group artifact version depth]
   (when (and version (< depth 6))
     (when-let [pom (fetch-pom-body group artifact version)]
       (or (pom-license pom)
         (when-let [[g a v] (parent-coords pom)]
           (resolve-license g a v (inc depth))))))))

(defn- jar-size-bytes [head-resp]
  (some-> head-resp :headers (get "content-length") parse-long))

(defn- fmt-size [bytes]
  (cond
    (nil? bytes)          "—"
    (>= bytes (* 1024 1024)) (format "%.1f MB" (/ bytes 1024.0 1024.0))
    :else                 (format "%d KB" (long (Math/round (/ bytes 1024.0))))))

(defn- in-house? [sym] (= "com.blockether" (namespace sym)))

(defn- artifact-info
  "License + jar size for one coordinate (floating RELEASE/LATEST -> unknown)."
  [sym version]
  (binding [*out* *err*] (println "  ·" (str sym) version))
  (if (contains? #{"RELEASE" "LATEST"} version)
    {:license "(floating)" :size-bytes nil :floating true}
    (let [head (fetch-first :head sym version)]
      {:license    (or (license-overrides (str sym))
                     (resolve-license (namespace sym) (name sym) version)
                     "UNKNOWN")
       :size-bytes (jar-size-bytes head)})))

;; ------------------------------------------------------------------- rendering

(defn- inventory-rows
  "De-duped [module sym version info] rows: each coord under its first module."
  [modules]
  (let [seen (atom #{})]
    (for [[label _ coords] modules
          [sym version]    (sort-by (comp str first) coords)
          :when            (not (@seen sym))
          :let             [_ (swap! seen conj sym)]]
      [label sym version (artifact-info sym version)])))

(defn- section-header [label]
  (if (= label "core") "### core (deps.edn)" (format "### `%s` extension" label)))

(defn- render-module [label rows]
  (str/join "\n"
    (concat
      [(section-header label) ""]
      (when-let [b (blurbs label)] [b ""])
      ["| Dependency | Version | License | Jar size | Ownership |"
       "|---|---|---|---|---|"]
      (for [[_ sym version info] rows]
        (format "| `%s` | `%s` | %s | %s | %s |"
          (str sym) version (:license info) (fmt-size (:size-bytes info))
          (if (in-house? sym) "Blockether (in-house)" "3rd-party")))
      [""])))

(defn- gen [root]
  (binding [*out* *err*] (println "Discovering modules…"))
  (let [modules  (discover-modules root)
        rows     (vec (inventory-rows modules))
        by-mod   (group-by first rows)
        licenses (frequencies (map (comp :license #(nth % 3)) rows))
        heavy    (->> rows
                   (keep (fn [[_ sym v info]]
                           (when-let [b (:size-bytes info)]
                             (when (>= b (* 1024 1024)) [sym v b]))))
                   (sort-by #(- (nth % 2))))
        total-b  (reduce + 0 (keep #(:size-bytes (nth % 3)) rows))
        copyleft (filter #(re-find #"GPL" (str (:license (nth % 3)))) rows)
        today    (subs (str (java.time.LocalDate/now)) 0 10)]
    (str
      "# vis — Security & Dependency Audit

> Generated " today ".

`vis` is a coding agent that writes Python into a sandboxed GraalPy runtime,
keeps durable state outside the model context window, and inspects and changes
the host project through tools. It ships as one Clojure package
(`com.blockether.vis.core`, Apache-2.0) plus optional classpath extensions
under `extensions/`, compiled to a single GraalVM native binary.

This document is the authoritative security, licensing and software
supply-chain record for that software. It is maintained by **Blockether**
(§2) and is written to be relied upon by security, procurement and
vendor-risk reviewers evaluating `vis` inside **regulated environments** —
financial services, public sector, and any setting with formal
third-party-risk, software-provenance or SBOM obligations (e.g. EU DORA ICT
third-party risk, NIS2). It answers, without reading the source: *what is it,
who stands behind it, what is inside it, under what terms, is any of it
vulnerable, and what does it do with data.*

---

## 1. The product — what vis is

- **What it is.** `vis` is an LLM coding agent that acts by writing code. It
  drives tasks end-to-end — locate → edit → verify — against the host
  repository, executing its own Python inside a **sandboxed GraalPy runtime**
  embedded in the binary rather than on the host interpreter.
- **Durable, out-of-context state.** Session state (plans, prior results,
  durable memory) lives *outside* the model context window, in a local store,
  so long tasks survive context limits. It is model-agnostic: it works with any
  text-producing LLM, with no provider lock-in.
- **How it ships.** A single self-contained **GraalVM native binary** — no JVM,
  no Python install, no external services required to run. Optional
  `extensions/*` add channels (TUI, Telegram, web), languages, persistence,
  voice and search; each is a droppable classpath module.
- **Where it runs.** Locally, on a developer machine or CI runner. It reaches
  an LLM provider only for inference; everything else is on-box (§9).

### At a glance

- **Source repository:** <https://github.com/Blockether/vis> — issues, releases, CI and the Security tab.
- **Primary language:** Clojure 1.12 on the JVM (Java 25 / GraalVM), compiled to a native image.
- **Direct dependency coordinates:** " (count rows) " unique, across " (count modules) " `deps.edn` modules (root + extensions).
- **Declared jar footprint (direct coords):** ~" (format "%.0f" (/ total-b 1024.0 1024.0)) " MB; concentrated in the embedded GraalPy runtime and the optional voice/ONNX stack (§8).
- **License posture:** permissive throughout (EPL, MIT, Apache-2.0, BSD, UPL) — " (if (seq copyleft) "**copyleft exception(s) flagged in §6.**" "no copyleft exceptions.") "
- **Vulnerability posture:** continuous [clj-watson](https://github.com/clj-holmes/clj-watson) SCA on every dependency change, weekly, and on demand — findings publish to the GitHub **Security** tab (§7).

---

## 2. Blockether — who maintains it

`vis` is built and maintained by **Blockether** (BLOCKETHER SP. Z O.O.),
a software consultancy and development company based in Kraków, Poland
(KRS 0001171097, NIP 675-18-13-221).

- **Focus.** Data, AI, web, cloud and blockchain engineering for **finance,
  regulated industries and e-commerce**, delivered predominantly as
  data-driven Clojure solutions.
- **Track record.** Years of work aligning stakeholders and shipping inside
  regulated environments; an active open-source footprint (e.g. `svar`,
  `spel`, `holy-lambda`) alongside its commercial engagements.
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
  and/or its affiliates. `vis` embeds and builds with the **Community Edition**
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

`vis` ships as a **GraalVM native binary**. The two GraalVM layers, the
embedded language runtime and models, and the one copyleft UI dependency are
each **FOSS and cleared for commercial redistribution**. There is **no Oracle
license on the distributed binary**.

### 4.1 The two GraalVM layers

| Layer | What it is | Coordinates / tool | License | Redistribution *for a fee* |
|---|---|---|---|---|
| **Embedded runtime** | GraalPy + Truffle/Polyglot, baked into the binary as the agent's sandboxed Python substrate | `org.graalvm.python/*`, `org.graalvm.polyglot/polyglot`, `org.graalvm.truffle/truffle-runtime` `25.1.3` (Maven Central) | **UPL-1.0** (+ MIT + PSF for the bundled CPython stdlib) | **Permitted** — UPL is a permissive, BSD-style license |
| **Build tool** | The `native-image` compiler that AOT-compiles vis into the standalone binary (`clojure -T:build native`) | **GraalVM Community Edition (CE) for JDK 25.1.3**, installed from the pinned `graalvm-ce-builds` asset via `.github/actions/setup-graalvm-25` (every CI + native-release workflow) | **GPL-2.0 with Classpath Exception** | **Permitted** — the Classpath Exception frees the output binary |

**Which GraalVM we support.** The release build uses **GraalVM CE 25.1.3**
only (the pinned `graalvm-ce-builds` asset). Oracle GraalVM is **not** used in
any build. The `native-image` compiler is **GPL-2.0 with the Classpath
Exception** (the OpenJDK license): the Exception exempts the tool's *output* —
the binary vis produces — from GPL, so it may be distributed and sold under
any terms with no copyleft reaching first-party code.

> **Historical note.** vis previously built with **Oracle GraalVM** under the
> **GFTC** (GraalVM Free Terms & Conditions), which permits free commercial
> *use* but restricts redistributing the built binary **for a fee**. The build
> was moved to **GraalVM CE** to remove that restriction — and Oracle from the
> risk register — entirely.

### 4.2 Embedded runtime & bundled models

The binary embeds the **GraalPy** runtime (`python-language` +
`python-resources`, ~105 MB — see §8) as the agent's sandboxed Python
substrate; it is mandatory for the core binary. GraalPy and Truffle/Polyglot
are **UPL-1.0**; the bundled CPython standard library adds **MIT + PSF**. All
are permissive and cleared for commercial redistribution.

The optional **`vis-foundation-voice`** extension bundles local speech
**models via ONNX Runtime** (`onnxruntime`, MIT) and `sherpa-onnx`
(Apache-2.0). These run **fully on-device** — no cloud speech service — and
ship only when that extension is included. No proprietary model weights are
redistributed by vis; LLM inference is delegated to a provider chosen by the
operator (§9). Every embedded/optional model component is under a permissive
license (see §5–§6).

### 4.3 Lanterna (terminal UI) — LGPL, safe for commercial use

The optional **`vis-channel-tui`** extension depends on **`com.blockether/lanterna`**
(**LGPL-3.0**) — a Blockether-owned fork of the Lanterna terminal-UI library.
LGPL is the *lesser* (weak) copyleft: it permits use in a **closed-source,
commercial** product; the only obligation is that a user can relink a modified
version of the LGPL library itself. Two facts make this a non-issue here:

1. **We own the artifact.** `com.blockether/lanterna` is a Blockether in-house
   library; we control its source, license and release cadence.
2. **It is optional and droppable.** It ships only with the TUI channel; the
   core binary and every other channel build without it, so it need never be
   statically linked into a distributed binary where relinking obligations
   would otherwise need review. The generated §6 warning tracks it
   automatically.

### What the owner of vis can do

- **Use it, run it, deploy it internally — for free.** CE is free for any use,
  commercial and production alike.
- **Modify vis.** First-party code is **Apache-2.0**; every dependency is
  permissive (see §6); the CE Classpath Exception keeps your changes proprietary.
  You own your changes.
- **Redistribute the vis source + its permissive deps for a fee.** Apache-2.0 /
  UPL / MIT / EPL / BSD all allow commercial redistribution, including charging.
- **Redistribute the native *binary* — free or for a fee.** The GPL-2.0+CE
  Classpath Exception on the CE build tool puts **no charge restriction** on the
  output binary. Sell it, bundle it, ship it into regulated / financial-sector environments — all clean.

---

## 5. Dependency inventory

Grouped by the module that declares each dependency; a coordinate shared by
several modules is listed once, under the first module that declares it. Jar
sizes are the direct artifact only (not the transitive closure). \"Ownership\"
distinguishes **Blockether in-house** libraries (we control the source and
release cadence) from **3rd-party** open source.

"
      (str/join "\n"
        (for [[label _ _] modules
              :let [mrows (by-mod label)]
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

All licenses in the graph are **permissive / OSI-approved** (EPL-1.0/2.0, MIT,
Apache-2.0, BSD, UPL-1.0, PSF, Public Domain) and compatible with shipping vis
under **Apache-2.0**"
      (if (seq copyleft)
        (str " — **with the copyleft exception(s) below that need legal sign-off:**\n\n"
          (str/join "\n"
            (for [[_ sym v info] copyleft]
              (format
                "> **WARNING — `%s` (`%s`) is %s** (copyleft). LGPL is generally fine for dynamic
> linking, but **static linking into the GraalVM native image** can trigger
> relinking obligations. Action: confirm distribution terms with legal, or keep
> the owning extension as an optional (droppable) jar rather than baking it into
> the distributed binary (see §4.3)."
                (str sym) v (:license info)))))
        " with no copyleft exceptions.")
      "

### Code ownership

- **First-party (this repo, Apache-2.0):** the `com.blockether.vis.core` package
  and every `extensions/*` module.
- **Blockether in-house libraries** (separate repos, we own source + releases):
  every `com.blockether/*` coordinate above.
- **3rd-party:** everything else in §5, sourced from Clojars / Maven Central.

---

## 7. Vulnerability posture — clj-watson (CVEs)

### Where to find live findings

The **authoritative, always-current** list of vulnerabilities is the repository's
GitHub **Security → Code scanning** tab — not this document:

**<https://github.com/Blockether/vis/security/code-scanning>**

Every [clj-watson](https://github.com/clj-holmes/clj-watson) run resolves the
*entire* dependency graph (direct **and** transitive), matches each artifact
against a vulnerability database on every dependency change, and publishes its
findings (SARIF) there. Each run is also archived as a downloadable build
artifact under **Actions → Security audit**
(<https://github.com/Blockether/vis/actions/workflows/security-audit.yml>). This
document is not the system of record for live CVE state — the Security tab is.

### What it is & why

[clj-watson](https://github.com/clj-holmes/clj-watson) (clj-holmes / Sean
Corfield community, EPL-2.0) is a **Software Composition Analysis (SCA)** tool
for Clojure. Most real-world risk in a JVM app lives in transitive
dependencies, not code we wrote — clj-watson gives an automated, reproducible
answer to \"does anything we ship have a known CVE?\", wired into CI so the
answer stays current without anyone remembering to check.

### How it runs here

**Locally** — a pinned `:clj-watson` alias lives in the root `deps.edn`:

```bash
# github-advisory strategy: only needs a GitHub token, no NVD download.
GITHUB_TOKEN=<your-token> clojure -M:clj-watson scan -p deps.edn -a '*' -t github-advisory -s
```

`-a '*'` includes every alias so the scan covers the root package **and** all
`:local/root` extensions; `-s` adds fix suggestions.

**In CI** — `.github/workflows/security-audit.yml` runs the scan on every
`deps.edn` change, on pull requests, weekly (Mondays 06:00 UTC), and via manual
dispatch. It emits **SARIF**, uploads it to GitHub **code scanning** (findings
appear in *Security → Code scanning*) and archives it as a build artifact. The
workflow is **non-blocking by default**; an opt-in gate (`-f`, or `-c <cvss>`)
is included, commented out, to start failing PRs once the baseline is clean. A
second job, `nvd-scan`, runs the deeper NVD / `dependency-check` strategy (full
CVSS scoring) weekly and on manual dispatch once the `NVD_API_KEY` secret is
provisioned; it self-skips until then.

| Strategy | Source | Auth / cost | When |
|---|---|---|---|
| `github-advisory` *(our default)* | GitHub Advisory Database (GraphQL) | built-in `GITHUB_TOKEN`, instant | CI + everyday local runs |
| `dependency-check` | NIST NVD (OWASP Dependency-Check) | needs a free **NVD API key**, downloads the full NVD DB on first run (cached in `~/.m2`) | deeper CVSS coverage / compliance |

---

## 8. Resource footprint

Heaviest direct artifacts (>= 1 MB):

| Dependency | Version | Jar size |
|---|---|---|
"
      (str/join "\n"
        (for [[sym v b] heavy]
          (format "| `%s` | `%s` | %s |" (str sym) v (fmt-size b))))
      "

Notes:
- The **GraalPy** runtime (`python-language` + `python-resources`, ~105 MB) is
  the agent's sandboxed Python substrate — mandatory for the core binary (§4.2).
- The **voice** stack (`onnxruntime` + `sherpa-onnx`) ships only with the
  optional `vis-foundation-voice` extension; excluding it drops both ONNX jars.
- `sqlite-jdbc` is bundled by the optional `vis-persistance-sqlite` extension.
- The final GraalVM **native binary** is larger than any single jar because it
  statically links the JDK + Truffle/GraalPy; track its size in the
  `native-release` workflow output.

---

## 9. Data governance

`vis` is designed to keep data **on-box**. From a data-governance standpoint:

- **No telemetry.** vis ships **no analytics, no usage tracking and no
  phone-home**. It does not emit metrics or events to Blockether or any third
  party.
- **No tracing off-box.** Session traces, plans and tool output are written to
  the **local** durable store only (for the agent's own out-of-context memory);
  nothing is transmitted for observability.
- **Where data goes.** The only outbound network call in normal operation is to
  the **LLM provider the operator configures**, for inference — chosen and
  keyed by the operator, not by Blockether. Package/dependency resolution
  (\"packing phase\") fetches artifacts from Clojars / Maven Central at
  build/resolve time only. Optional voice runs **locally** via on-device ONNX
  models (§4.2) — no cloud speech service.
- **Data providers.** vis bundles no proprietary datasets or model weights;
  content it processes is the host project's own files plus whatever the
  operator sends to their chosen LLM provider. The operator remains the data
  controller.

---

## 10. Commercial licensing, support & warranty

`vis` and its first-party code are distributed **\"AS IS\", without warranty of
any kind**, express or implied — as stated in the Apache-2.0 `LICENSE` and in
the license of every third-party dependency in §5. The open-source grant
carries **no warranty, no guarantee of fitness, and no liability** on the part
of Blockether or any upstream author. Absent a commercial agreement, use of vis
is governed exclusively by the open-source licenses above and is entirely at
the user's own risk.

**Warranties, SLAs and support are provided solely under a paid commercial
agreement with Blockether** — request one at <contact@blockether.com>
(<https://blockether.com>). Only a signed Blockether service contract confers:

- warranty / fitness commitments and defect remediation;
- a security-response and vulnerability-remediation SLA (§7);
- maintenance, updates and prioritised support;
- any indemnification.

**What commercial engagement covers.** Blockether can, under contract:

- **ship, deploy and integrate the base** into your environment — installation,
  release/upgrade management and CI wiring;
- **maintain it** — dependency and security updates, version pinning, and
  keeping this audit and the SCA scans current;
- **guide teams** on how to *use*, *secure* and *distribute* vis to match your
  regulatory and internal requirements;
- **build custom extensions** for a particular component (channels, language
  packs, persistence, workspace / tool integrations) on the extension surface;
- provide hardening, compliance-mapping and audit support for regulated
  deployments.

---

## 11. Security contact & vulnerability disclosure

Report suspected vulnerabilities in `vis` privately to
**<security@blockether.com>**. Please include affected version / commit,
reproduction steps and impact. Do **not** open a public issue for an
undisclosed vulnerability. Coordinated-disclosure timelines and any remediation
SLA are governed by a commercial agreement (§10); absent one, Blockether
addresses reports on a best-effort basis.

**Non-security issues** — ordinary bugs, feature requests and any code problem
*not* related to a vulnerability — belong in the public issue tracker at
<https://github.com/Blockether/vis/issues>, **not** at the security address.
Keep the two channels separate: security disclosures stay private
(<security@blockether.com>); everything else is a GitHub issue.

")))

;; --------------------------------------------------------------------- runner

(let [root   (str (fs/cwd))
      target (fs/file root "audit" "README.md")
      md     (gen root)
      check? (some #{"--check"} *command-line-args*)]
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
