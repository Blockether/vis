# Vis — Security & Dependency Audit

> Generated 2026-09-12.

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
- **Direct dependency coordinates:** 58 unique, across 3 `deps.edn` modules (root + siblings).
- **Total direct jar size:** ~38 MB; most space is used by Python and optional speech components (§8).
- **Licenses:** dependencies include EPL, MIT, Apache-2.0, BSD and UPL — **copyleft exception(s) flagged in §6.**
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
sizes are the direct artifact only (not the transitive closure). "Ownership"
identifies Blockether-maintained libraries and third-party projects.

### core (deps.edn)

_Core runtime — the `vis-agent` CLI, agent loop, HTTP gateway, sandbox._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `babashka/fs` | `0.5.34` | EPL-1.0 | 35 KB | 3rd-party |
| `babashka/process` | `0.6.25` | EPL-1.0 | 23 KB | 3rd-party |
| `borkdude/edamame` | `1.6.42` | EPL-1.0 | 32 KB | 3rd-party |
| `clj-kondo/clj-kondo` | `2026.07.24` | EPL-1.0 | 731 KB | 3rd-party |
| `com.blockether/anomaly` | `1.0.1` | EPL-1.0 | 4 KB | Blockether (in-house) |
| `com.blockether/fff` | `0.12.9` | MIT | 11 KB | Blockether (in-house) |
| `com.blockether/imaging` | `0.1.10` | MIT | 23 KB | Blockether (in-house) |
| `com.blockether/parinferish` | `0.1.2` | MIT | 34 KB | Blockether (in-house) |
| `com.blockether/rift` | `0.0.10-11` | MIT | 11 KB | Blockether (in-house) |
| `com.blockether/ruff` | `0.3.5` | MIT | 10 KB | Blockether (in-house) |
| `com.blockether/svar` | `0.7.167` | Apache-2.0 | 619 KB | Blockether (in-house) |
| `com.blockether/tree-sitter-language-pack` | `1.12.3-blockether.39` | MIT | 175 KB | Blockether (in-house) |
| `com.blockether/vis-python-runtime` | `git:86e8e7a4faa66c003da97f9c13173e4142d2e16e` | MIT | source checkout | Blockether (in-house) |
| `com.cnuernber/charred` | `1.041` | MIT | 49 KB | 3rd-party |
| `com.fasterxml.jackson.core/jackson-core` | `2.22.1` | Apache-2.0 | 580 KB | 3rd-party |
| `com.fasterxml.jackson.dataformat/jackson-dataformat-cbor` | `2.22.1` | Apache-2.0 | 72 KB | 3rd-party |
| `com.fasterxml.jackson.dataformat/jackson-dataformat-smile` | `2.22.1` | Apache-2.0 | 95 KB | 3rd-party |
| `com.github.clj-easy/graal-build-time` | `1.0.6` | MIT | 27 KB | 3rd-party |
| `com.github.k2-fsa.sherpa-onnx/sherpa-onnx-jvm` | `v1.13.5` | Apache-2.0 | 183 KB | 3rd-party |
| `com.github.liquidz/antq` | `RELEASE` | (floating) | — | 3rd-party |
| `com.github.seancorfield/honeysql` | `2.7.1425` | EPL-2.0 | 43 KB | 3rd-party |
| `com.github.seancorfield/next.jdbc` | `1.3.1118` | EPL-2.0 | 55 KB | 3rd-party |
| `com.google.zxing/core` | `3.5.4` | Apache-2.0 | 596 KB | 3rd-party |
| `com.taoensso/nippy` | `3.8.0` | EPL-1.0 | 52 KB | 3rd-party |
| `com.taoensso/telemere` | `1.2.1` | EPL-1.0 | 59 KB | 3rd-party |
| `com.taoensso/telemere-slf4j` | `1.2.1` | EPL-1.0 | 19 KB | 3rd-party |
| `com.taoensso/trove` | `1.1.0` | EPL-1.0 | 17 KB | 3rd-party |
| `com.zaxxer/HikariCP` | `7.1.0` | Apache-2.0 | 169 KB | 3rd-party |
| `dev.weavejester/cljfmt` | `0.16.5` | EPL-1.0 | 20 KB | 3rd-party |
| `info.sunng/ring-jetty9-adapter` | `0.40.3` | EPL-1.0 | 163 KB | 3rd-party |
| `io.github.clj-holmes/clj-watson` | `git:be98e4db74fb8927db4825cc73bbe2606e44e5e3` | EPL-2.0 | source checkout | 3rd-party |
| `io.github.clojure/tools.build` | `0.10.14` | EPL-1.0 | 32 KB | 3rd-party |
| `io.github.java-diff-utils/java-diff-utils` | `4.17` | Apache-2.0 | 77 KB | 3rd-party |
| `io.github.noahtheduke/lazytest` | `2.1.0` | EPL-1.0 | 47 KB | 3rd-party |
| `io.github.tonsky/clojure-plus` | `1.7.2` | MIT | 33 KB | 3rd-party |
| `metosin/reitit-ring` | `0.10.1` | EPL-1.0 | 9 KB | 3rd-party |
| `nrepl/nrepl` | `1.7.0` | EPL-1.0 | 103 KB | 3rd-party |
| `org.apache.commons/commons-compress` | `1.28.0` | Apache-2.0 | 1.1 MB | 3rd-party |
| `org.babashka/http-client` | `0.4.24` | MIT | 16 KB | 3rd-party |
| `org.bouncycastle/bcpkix-jdk18on` | `1.85` | Bouncy Castle Licence | 1.3 MB | 3rd-party |
| `org.bouncycastle/bcprov-jdk18on` | `1.85` | Bouncy Castle Licence | 9.8 MB | 3rd-party |
| `org.clojure/clojure` | `1.12.5` | EPL-1.0 | 4.0 MB | 3rd-party |
| `org.clojure/core.memoize` | `1.2.281` | EPL-1.0 | 8 KB | 3rd-party |
| `org.clojure/tools.deps` | `0.31.1638` | EPL-1.0 | 49 KB | 3rd-party |
| `org.commonmark/commonmark` | `0.29.0` | BSD-2-Clause | 211 KB | 3rd-party |
| `org.commonmark/commonmark-ext-gfm-strikethrough` | `0.29.0` | BSD-2-Clause | 13 KB | 3rd-party |
| `org.commonmark/commonmark-ext-gfm-tables` | `0.29.0` | BSD-2-Clause | 23 KB | 3rd-party |
| `org.flywaydb/flyway-core` | `12.11.0` | Apache-2.0 | 791 KB | 3rd-party |
| `org.flywaydb/flyway-database-nc-sqlite` | `12.11.0` | Apache-2.0 | 6 KB | 3rd-party |
| `org.xerial/sqlite-jdbc` | `3.53.2.1` | Apache-2.0 | 11.4 MB | 3rd-party |
| `org.yamlstar/yamlstar` | `0.1.21` | MIT | 28 KB | 3rd-party |
| `ring/ring-core` | `1.15.5` | MIT | 34 KB | 3rd-party |
| `slipset/deps-deploy` | `0.2.5` | EPL-1.0 | 8 KB | 3rd-party |
| `tools.jackson.core/jackson-databind` | `3.2.1` | Apache-2.0 | 1.9 MB | 3rd-party |
| `zprint/zprint` | `1.2.9` | MIT | 220 KB | 3rd-party |

### `vis-tui` module

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.blockether/lanterna` | `3.1.5-vis.50` | LGPL-3.0 | 607 KB | Blockether (in-house) |
| `org.jcodec/jcodec` | `0.2.5` | BSD | 2.0 MB | 3rd-party |

### `vis-contract` module

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.blockether/skjema` | `0.3.0` | MIT | 54 KB | Blockether (in-house) |

---

## 6. Licenses & code ownership

### License distribution (direct coordinates)

| License | Count |
|---|---|
| EPL-1.0 | 19 |
| MIT | 15 |
| Apache-2.0 | 13 |
| BSD-2-Clause | 3 |
| EPL-2.0 | 3 |
| Bouncy Castle Licence | 2 |
| (floating) | 1 |
| LGPL-3.0 | 1 |
| BSD | 1 |

Dependencies use several licenses, listed above. Vis first-party code uses
Apache-2.0. Review dependency terms for the intended distribution method — **with the copyleft exception(s) below that need legal sign-off:**

> **Copyleft: `com.blockether/lanterna` (`3.1.5-vis.50`) uses LGPL-3.0.** Review source, notice and relinking
> obligations before distribution, particularly for static native-image builds.
> Keeping an optional dependency separate may affect those obligations (§4.3).

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
| `org.xerial/sqlite-jdbc` | `3.53.2.1` | 11.4 MB |
| `org.bouncycastle/bcprov-jdk18on` | `1.85` | 9.8 MB |
| `org.clojure/clojure` | `1.12.5` | 4.0 MB |
| `org.jcodec/jcodec` | `0.2.5` | 2.0 MB |
| `tools.jackson.core/jackson-databind` | `3.2.1` | 1.9 MB |
| `org.bouncycastle/bcpkix-jdk18on` | `1.85` | 1.3 MB |
| `org.apache.commons/commons-compress` | `1.28.0` | 1.1 MB |

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

