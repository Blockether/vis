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
   "vis-channel-telegram"    "_Telegram bot channel._"
   "vis-channel-web"         "_Web trace / HTML views._"
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

> Generated " today " by `scripts/gen-audit.bb` (run in CI — see §10). Dependency
> versions, licenses and jar sizes are pulled straight from the resolved
> `deps.edn` graph and the Clojars / Maven Central POMs. **Do not edit by hand**
> — rerun `bb scripts/gen-audit.bb` (or let the `audit-md` workflow regenerate it).

`vis` is an autonomous coding agent: a single Clojure package
(`com.blockether.vis.core`, Apache-2.0) plus a set of optional classpath
extensions under `extensions/`. It ships as a GraalVM **native binary** that
embeds a **GraalPy** sandbox (the agent's action substrate) and a small HTTP
gateway. This document inventories what we depend on, under which licenses, how
heavy each piece is, who owns it, and how we continuously audit it for known
vulnerabilities.

---

## 1. Purpose & scope

**What this is.** A standing security and software-supply-chain audit of `vis`:
one source of truth that inventories every dependency we ship — its version,
license, size and ownership — and records how we continuously scan the whole
dependency graph for known vulnerabilities.

**Who it is for.** Teams evaluating or running `vis` inside **regulated
environments** — financial services, public sector and any setting with formal
third-party-risk, software-provenance or supply-chain obligations (e.g. EU DORA
ICT third-party risk, NIS2, and SBOM expectations). It is written so a security
or vendor-risk reviewer can answer \"what is in it, under what terms, and is any
of it vulnerable?\" without reading the source.

**Scope.** The `vis` core package plus every optional `extensions/*` module in
this repository, and all of their **direct** Maven / Clojars coordinates.
Transitive dependencies are covered by the continuous clj-watson scan (§3),
which resolves the full graph; the inventory tables (§4) list direct
coordinates only.

**Methodology.**

- *Inventory, licenses, sizes, ownership (§4–§5) and footprint (§7)* — generated
  automatically from the resolved `deps.edn` graph and the upstream Clojars /
  Maven Central POMs, so the figures can never drift from the code.
- *Vulnerability posture (§3)* — [clj-watson](https://github.com/clj-holmes/clj-watson)
  Software Composition Analysis runs in CI on every dependency change, weekly,
  and on demand; findings publish to the GitHub **Security** tab.
- *Licensing & distribution rights (§5–§6), warranty (§8)* — reviewed against
  shipping `vis` as a commercial, redistributable native binary.

This audit is **static where it must be** — methodology, licensing and warranty
prose, reviewed when policy changes — and **dynamic where it can be**: the
inventory, license-distribution and footprint tables are regenerated by CI on
every dependency change, so they always describe the current tree.

---

## 2. At a glance

- **Primary language:** Clojure 1.12 on the JVM (Java 25 / GraalVM), compiled to a native image.
- **Direct dependency coordinates:** " (count rows) " unique, across " (count modules) " `deps.edn` modules (root + extensions).
- **Declared jar footprint (direct coords):** ~" (format "%.0f" (/ total-b 1024.0 1024.0)) " MB — dominated by the embedded GraalPy runtime and the voice/ONNX stack (both optional extensions).
- **License posture:** permissive throughout (EPL, MIT, Apache-2.0, BSD, UPL) — "
      (if (seq copyleft) "**copyleft exception(s) flagged below.**" "no copyleft exceptions.") "
- **Continuous auditing:** [clj-watson](https://github.com/clj-holmes/clj-watson) SCA scan on every dependency change, on a weekly schedule, and on demand — results published to the GitHub **Security** tab (see §3).

---

## 3. How we audit — clj-watson

### What it is
[clj-watson](https://github.com/clj-holmes/clj-watson) (clj-holmes / Sean
Corfield community, EPL-2.0) is a **Software Composition Analysis (SCA)** tool
for Clojure. It resolves the *entire* dependency graph declared by a
`deps.edn` — direct **and** transitive — and matches every artifact against a
vulnerability database, reporting known CVEs (and, optionally, remediation
suggestions).

### Why
Most real-world risk in a JVM app lives in transitive dependencies, not in code
we wrote. clj-watson gives us an automated, reproducible answer to \"does
anything we ship have a known CVE?\", wired into CI so the answer stays current
without anyone remembering to check.

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
appear in the repository *Security -> Code scanning* tab) and archives it as a
build artifact. The workflow is **non-blocking by default**; an opt-in gate
(`-f`, or `-c <cvss>` for a threshold) is included, commented out, to start
failing PRs once the baseline is clean.

**This document** — `.github/workflows/audit-md.yml` reruns
`bb scripts/gen-audit.bb` on every `deps.edn` change and weekly, and commits any
resulting audit/README.md diff, so the inventory below can never drift from the code.

### Database strategies

| Strategy | Source | Auth / cost | When |
|---|---|---|---|
| `github-advisory` *(our default)* | GitHub Advisory Database (GraphQL) | built-in `GITHUB_TOKEN`, instant | CI + everyday local runs |
| `dependency-check` | NIST NVD (OWASP Dependency-Check) | needs a free **NVD API key**, downloads the full NVD DB on first run (cached in `~/.m2`) | deeper CVSS coverage / compliance |

To use the NVD strategy, get a key at
<https://nvd.nist.gov/developers/request-an-api-key>, expose it to the scanner
(`NVD_API_KEY` secret in CI), and switch `-t github-advisory` -> `-t dependency-check`.

---

## 4. Dependency inventory

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

## 5. Licenses & code ownership

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
> the distributed binary."
                (str sym) v (:license info)))))
        " with no copyleft exceptions.")
      "

### Code ownership

- **First-party (this repo, Apache-2.0):** the `com.blockether.vis.core` package
  and every `extensions/*` module.
- **Blockether in-house libraries** (separate repos, we own source + releases):
  every `com.blockether/*` coordinate above.
- **3rd-party:** everything else in §3, sourced from Clojars / Maven Central.

---

## 6. GraalVM, native image & commercial-use rights

`vis` ships as a **GraalVM native binary**, and two distinct GraalVM layers are
involved. Both are now **FOSS**: the release binary is built with **GraalVM
Community Edition**, so there is **no Oracle license on the distributed binary**
at all — which is what matters to anyone who wants to **sell or redistribute
vis**.

### The two layers

| Layer | What it is | Coordinates / tool | License | Redistribution *for a fee* |
|---|---|---|---|---|
| **Embedded runtime** | GraalPy + Truffle/Polyglot, baked into the binary as the agent's sandboxed Python substrate | `org.graalvm.python/*`, `org.graalvm.polyglot/polyglot`, `org.graalvm.truffle/truffle-runtime` `25.1.3` (Maven Central) | **UPL-1.0** (+ MIT + PSF for the bundled CPython stdlib) | **Permitted** — UPL is a permissive, BSD-style license |
| **Build tool** | The `native-image` compiler that AOT-compiles vis into the standalone binary (`clojure -T:build native`) | **GraalVM Community Edition (CE) for JDK 25.1.3**, installed from the pinned `graalvm-ce-builds` asset via `.github/actions/setup-graalvm-25` (every CI + native-release workflow) | **GPL-2.0 with Classpath Exception** | **Permitted** — the Classpath Exception frees the output binary |

### Why GraalVM CE — the Classpath Exception is the whole point

GraalVM CE's native-image compiler is licensed **GPL-2.0 with the Classpath
Exception**. That exception is the decisive clause: it explicitly lets the
**output** of the tool — the native binary vis produces — be distributed and
**sold under any terms you choose**, with **no copyleft reaching your own
code**. This is the same license posture the entire Java/OpenJDK ecosystem
ships under, and it is the standard route for commercial native-image products.

Because vis is built with **CE** (not Oracle GraalVM), the \"redistribution for
a fee\" restriction that Oracle's GFTC imposes simply **does not apply**. There
is no Oracle build-tool license baked into the binary to reason about — the
story a bank's legal desk sees is just *\"GPL-2.0 + Classpath Exception\"*, a
license they already know. (The embedded GraalPy/Truffle Maven artifacts above
are UPL and independent of the build tool either way.)

> **Historical note:** vis previously built with **Oracle GraalVM** under the
> **GFTC** (GraalVM Free Terms & Conditions). GFTC permits free commercial and
> production *use*, but restricts redistributing the built binary **for a fee**.
> The release build was moved to **GraalVM CE** (`.github/actions/setup-graalvm-25`,
> replacing `graalvm/setup-graalvm`'s `distribution: graalvm`) to remove that
> restriction — and Oracle from the risk register — entirely.

### What the owner of vis can do

- **Use it, run it, deploy it internally — for free.** CE is free for any use,
  commercial and production alike.
- **Modify vis.** First-party code is **Apache-2.0**; every dependency is
  permissive (see §5); the CE Classpath Exception keeps your changes proprietary.
  You own your changes.
- **Redistribute the vis source + its permissive deps for a fee.** Apache-2.0 /
  UPL / MIT / EPL / BSD all allow commercial redistribution, including charging.
- **Redistribute the native *binary* — free or for a fee.** The GPL-2.0+CE
  Classpath Exception on the CE build tool puts **no charge restriction** on the
  output binary. Sell it, bundle it, ship it into regulated / financial-sector environments — all clean.

---

## 7. Resource footprint

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
  the agent's sandboxed Python substrate — mandatory for the core binary.
- The **voice** stack (`onnxruntime` + `sherpa-onnx`) ships only with the
  optional `vis-foundation-voice` extension; drop the jar, drop the weight.
- `sqlite-jdbc` is bundled by the optional `vis-persistance-sqlite` extension.
- The final GraalVM **native binary** is larger than any single jar because it
  statically links the JDK + Truffle/GraalPy; track its size in the
  `native-release` workflow output.

---

## 8. Warranty & disclaimer

`vis` and its first-party code are distributed **\"AS IS\", without warranty of
any kind**, express or implied — as stated in the Apache-2.0 `LICENSE` and in the
license of every third-party dependency in §3. The open-source grant carries
**no warranty, no guarantee of fitness, and no liability** on the part of
Blockether or any upstream author.

**Warranties, SLAs and support are provided solely under a paid commercial
agreement with Blockether** (<https://blockether.com>). Only a signed Blockether
service contract confers:

- warranty / fitness commitments and defect remediation;
- a security-response and vulnerability-remediation SLA (see §3);
- maintenance, updates and prioritised support;
- any indemnification.

Absent such an agreement, use of vis is governed exclusively by the open-source
licenses above and is entirely at the user's own risk.

---

## 9. Pending / follow-ups

- [ ] Add the free **NVD API key** secret and evaluate switching CI to the
      `dependency-check` strategy for full CVSS scoring.
- [ ] Resolve the **LGPL / Lanterna** distribution question (§5) with legal.
- [x] **Native-image distribution** for paid delivery (§6): **done** — the release
      build uses **GraalVM CE** (GPL-2.0 + Classpath Exception), so the binary can
      be shipped or sold under any terms. Oracle GraalVM/GFTC no longer applies.
- [ ] Flip on the CI **fail gate** (`-f` / `-c`) once the first clean baseline lands.
- [ ] _(from Wojtek — pending input)_ additional compliance / threat-model items.

(These items are hand-maintained inline in the `gen` string of
`scripts/gen-audit.bb`; everything else on this page is generated.)

---

## 10. Maintenance

This file is generated. To refresh it after a dependency bump:

```bash
bb scripts/gen-audit.bb        # regenerate audit/README.md from the deps graph
clojure -M:antq                # list outdated deps
clojure -M:clj-watson scan -p deps.edn -a '*' -t github-advisory -s   # re-scan CVEs
```

CI enforces freshness: the `audit-md` workflow regenerates and commits audit/README.md
whenever any `deps.edn` changes, and `bb scripts/gen-audit.bb --check` fails a PR
that leaves it stale. Keep the `:clj-watson` alias pinned to a released tag
**and** its `:git/sha` so scans stay reproducible.
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
