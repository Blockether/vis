# vis — Security & Dependency Audit

> Generated 2026-07-04. Dependency versions, licenses and jar sizes in this file
> are pulled straight from the resolved `deps.edn` graph and the Clojars /
> Maven Central POMs. Regenerate whenever dependencies change.

`vis` is an autonomous coding agent: a single Clojure package
(`com.blockether.vis.core`, Apache-2.0) plus a set of optional classpath
extensions under `extensions/`. It ships as a GraalVM **native binary** that
embeds a **GraalPy** sandbox (the agent's action substrate) and a small HTTP
gateway. This document inventories what we depend on, under which licenses, how
heavy each piece is, who owns it, and how we continuously audit it for known
vulnerabilities.

---

## 1. At a glance

- **Primary language:** Clojure 1.12 on the JVM (Java 25 / GraalVM), compiled to a native image.
- **Direct dependency coordinates:** 53 across 18 `deps.edn` files (root + extensions).
- **Declared jar footprint (direct coords):** ~217 MB — dominated by the embedded GraalPy runtime and the voice/ONNX stack (both optional extensions).
- **License posture:** permissive throughout (EPL, MIT, Apache-2.0, BSD, UPL) — **one copyleft exception** flagged below.
- **Continuous auditing:** [clj-watson](https://github.com/clj-holmes/clj-watson) SCA scan on every dependency change, on a weekly schedule, and on demand — results published to the GitHub **Security** tab (see §5).

---

## 2. How we audit — clj-watson

### What it is
[clj-watson](https://github.com/clj-holmes/clj-watson) (clj-holmes / Sean
Corfield community, EPL-2.0) is a **Software Composition Analysis (SCA)** tool
for Clojure. It resolves the *entire* dependency graph declared by a
`deps.edn` — direct **and** transitive — and matches every artifact against a
vulnerability database, reporting known CVEs (and, optionally, remediation
suggestions).

### Why
Most real-world risk in a JVM app lives in transitive dependencies, not in code
we wrote. clj-watson gives us an automated, reproducible answer to "does
anything we ship have a known CVE?", wired into CI so the answer stays current
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

### Database strategies

| Strategy | Source | Auth / cost | When |
|---|---|---|---|
| `github-advisory` *(our default)* | GitHub Advisory Database (GraphQL) | built-in `GITHUB_TOKEN`, instant | CI + everyday local runs |
| `dependency-check` | NIST NVD (OWASP Dependency-Check) | needs a free **NVD API key**, downloads the full NVD DB on first run (cached in `~/.m2`) | deeper CVSS coverage / compliance |

To use the NVD strategy, get a key at
<https://nvd.nist.gov/developers/request-an-api-key>, expose it to the scanner
(`NVD_API_KEY` secret in CI), and switch `-t github-advisory` -> `-t dependency-check`.

---

## 3. Dependency inventory

Grouped by the module that declares each dependency. Jar sizes are the direct
artifact only (not the transitive closure). "Ownership" distinguishes
**Blockether in-house** libraries (we control the source and release cadence)
from **3rd-party** open source.

### core (deps.edn)

_Shipped binary runtime — the `vis` CLI, agent loop, HTTP gateway, sandbox._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `babashka/fs` | `0.5.33` | EPL-1.0 | 28 KB | 3rd-party |
| `babashka/process` | `0.6.25` | EPL-1.0 | 23 KB | 3rd-party |
| `com.blockether/anomaly` | `1.0.1` | EPL-1.0 | 4 KB | Blockether (in-house) |
| `com.blockether/fff` | `0.9.6-8` | MIT | 11 KB | Blockether (in-house) |
| `com.blockether/ruff` | `0.1.2` | MIT | 9 KB | Blockether (in-house) |
| `com.blockether/svar` | `0.7.45` | Apache-2.0 | 369 KB | Blockether (in-house) |
| `com.blockether/tree-sitter-language-pack` | `1.12.3-blockether.4` | MIT | 148 KB | Blockether (in-house) |
| `com.cnuernber/charred` | `1.039` | MIT | 48 KB | 3rd-party |
| `com.github.clj-easy/graal-build-time` | `1.0.6` | MIT | 27 KB | 3rd-party |
| `com.github.liquidz/antq` | `RELEASE` | (floating) | — | 3rd-party |
| `com.taoensso/telemere` | `1.2.1` | EPL-1.0 | 59 KB | 3rd-party |
| `com.taoensso/telemere-slf4j` | `1.2.1` | EPL-1.0 | 19 KB | 3rd-party |
| `criterium/criterium` | `0.4.6` | EPL-1.0 | 21 KB | 3rd-party |
| `io.github.clojure/tools.build` | `0.10.14` | EPL-1.0 | 32 KB | 3rd-party |
| `io.github.java-diff-utils/java-diff-utils` | `4.17` | Apache-2.0 | 77 KB | 3rd-party |
| `io.github.noahtheduke/lazytest` | `2.0.0` | EPL-1.0 | 46 KB | 3rd-party |
| `io.github.tonsky/clj-reload` | `1.0.0` | MIT | 19 KB | 3rd-party |
| `io.github.tonsky/clojure-plus` | `1.7.2` | MIT | 33 KB | 3rd-party |
| `metosin/malli` | `0.20.1` | EPL-2.0 | 97 KB | 3rd-party |
| `metosin/reitit-ring` | `0.10.1` | EPL-1.0 | 9 KB | 3rd-party |
| `nrepl/nrepl` | `1.7.0` | EPL-1.0 | 103 KB | 3rd-party |
| `org.apache.commons/commons-compress` | `1.28.0` | Apache-2.0 | 1.1 MB | 3rd-party |
| `org.clojure/core.memoize` | `1.2.281` | EPL-1.0 | 8 KB | 3rd-party |
| `org.clojure/tools.deps` | `0.31.1629` | EPL-1.0 | 49 KB | 3rd-party |
| `org.commonmark/commonmark` | `0.29.0` | BSD-2-Clause | 211 KB | 3rd-party |
| `org.commonmark/commonmark-ext-gfm-strikethrough` | `0.29.0` | BSD-2-Clause | 13 KB | 3rd-party |
| `org.commonmark/commonmark-ext-gfm-tables` | `0.29.0` | BSD-2-Clause | 23 KB | 3rd-party |
| `org.graalvm.polyglot/polyglot` | `25.1.3` | UPL-1.0 | 502 KB | 3rd-party |
| `org.graalvm.python/python-language` | `25.1.3` | UPL-1.0 + MIT + PSF | 90.7 MB | 3rd-party |
| `org.graalvm.python/python-resources` | `25.1.3` | UPL-1.0 + MIT + PSF | 13.8 MB | 3rd-party |
| `org.graalvm.truffle/truffle-runtime` | `25.1.3` | UPL-1.0 | 913 KB | 3rd-party |
| `ring/ring-core` | `1.15.5` | MIT | 34 KB | 3rd-party |
| `ring/ring-jetty-adapter` | `1.15.5` | MIT | 7 KB | 3rd-party |
| `slipset/deps-deploy` | `0.2.5` | EPL-1.0 | 8 KB | 3rd-party |

### `vis-persistance-sqlite` extension

_Durable session store (SQLite + Flyway migrations)._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.github.seancorfield/honeysql` | `2.7.1399` | EPL-2.0 | 43 KB | 3rd-party |
| `com.github.seancorfield/next.jdbc` | `1.3.1118` | EPL-2.0 | 55 KB | 3rd-party |
| `com.taoensso/nippy` | `3.7.0-RC2` | EPL-1.0 | 51 KB | 3rd-party |
| `com.zaxxer/HikariCP` | `7.1.0` | Apache-2.0 | 169 KB | 3rd-party |
| `org.flywaydb/flyway-core` | `12.10.0` | Apache-2.0 | 789 KB | 3rd-party |
| `org.flywaydb/flyway-database-nc-sqlite` | `12.10.0` | Apache-2.0 | 6 KB | 3rd-party |
| `org.xerial/sqlite-jdbc` | `3.53.2.0` | Apache-2.0 | 11.4 MB | 3rd-party |

### `vis-language-clojure` extension

_Clojure language pack (format/lint/structural edits)._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `borkdude/edamame` | `1.5.39` | EPL-1.0 | 26 KB | 3rd-party |
| `clj-kondo/clj-kondo` | `2026.05.25` | EPL-1.0 | 701 KB | 3rd-party |
| `dev.weavejester/cljfmt` | `0.16.4` | EPL-1.0 | 19 KB | 3rd-party |
| `parinferish/parinferish` | `0.8.0` | Public-Domain | 8 KB | 3rd-party |

### `vis-foundation-voice` extension

_Local speech (sherpa-onnx / ONNX Runtime)._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.litongjava/sherpa-onnx-java-api` | `1.0.1` | Apache-2.0 | 7.7 MB | 3rd-party |
| `com.microsoft.onnxruntime/onnxruntime` | `1.17.1` | MIT | 83.5 MB | 3rd-party |

### `vis-channel-tui` extension

_Terminal UI (Lanterna)._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.blockether/lanterna` | `3.1.5-vis.20` | LGPL-3.0 | 584 KB | Blockether (in-house) |

### `vis-channel-telegram` extension

_Telegram bot channel._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `org.babashka/http-client` | `0.4.23` | MIT | 16 KB | 3rd-party |
| `org.clojure/clojure` | `1.12.5` | EPL-1.0 | 4.0 MB | 3rd-party |

### `vis-channel-web` extension

_Web trace / HTML views._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `hiccup/hiccup` | `2.0.0` | EPL-1.0 | 21 KB | 3rd-party |

### `vis-foundation-bridge` extension

_Bridge verification tool surface._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.blockether/bridge` | `0.1.2` | Apache-2.0 | 69 KB | Blockether (in-house) |

### `vis-workspace-rift` extension

_Rift workspace/FFM integration._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.blockether/rift` | `0.0.10-5` | MIT | 11 KB | Blockether (in-house) |

---

## 4. Licenses & code ownership

### License distribution (direct coordinates)

| License | Count |
|---|---|
| EPL-1.0 | 19 |
| MIT | 12 |
| Apache-2.0 | 9 |
| BSD-2-Clause | 3 |
| EPL-2.0 | 3 |
| UPL-1.0 | 2 |
| UPL-1.0 + MIT + PSF | 2 |
| (floating) | 1 |
| Public-Domain | 1 |
| LGPL-3.0 | 1 |

All licenses in the graph are **permissive / OSI-approved** (EPL-1.0/2.0, MIT,
Apache-2.0, BSD-2-Clause, UPL-1.0, PSF, Public Domain) and compatible with
shipping vis under **Apache-2.0** — **with one exception that needs legal sign-off:**

> **WARNING — `com.blockether/lanterna` (`3.1.5-vis.20`) is LGPL-3.0** (copyleft,
> inherited from upstream Lanterna). It is used only by the `vis-channel-tui`
> extension. LGPL is generally fine for dynamic linking, but **static linking
> into the GraalVM native image** can trigger LGPL relinking obligations.
> Action: confirm distribution terms with legal, or keep the TUI as an optional
> (droppable) extension jar rather than baking it into the distributed binary.

### Code ownership

- **First-party (this repo, Apache-2.0):** the `com.blockether.vis.core` package
  and every `extensions/*` module.
- **Blockether in-house libraries** (separate repos, we own source + releases):
  `svar`, `anomaly`, `fff`, `ruff`, `rift`, `bridge`,
  `tree-sitter-language-pack`, and the `lanterna` fork.
- **3rd-party:** everything else in §3, sourced from Clojars / Maven Central.

---

## 5. Resource footprint

Heaviest direct artifacts (>= 1 MB):

| Dependency | Version | Jar size |
|---|---|---|
| `org.graalvm.python/python-language` | `25.1.3` | 90.7 MB |
| `com.microsoft.onnxruntime/onnxruntime` | `1.17.1` | 83.5 MB |
| `org.graalvm.python/python-resources` | `25.1.3` | 13.8 MB |
| `org.xerial/sqlite-jdbc` | `3.53.2.0` | 11.4 MB |
| `com.litongjava/sherpa-onnx-java-api` | `1.0.1` | 7.7 MB |
| `org.clojure/clojure` | `1.12.5` | 4.0 MB |
| `org.apache.commons/commons-compress` | `1.28.0` | 1.1 MB |

Notes:
- The **GraalPy** runtime (`python-language` + `python-resources`, ~105 MB) is
  the agent's sandboxed Python substrate — mandatory for the core binary.
- The **voice** stack (`onnxruntime` 83 MB + `sherpa-onnx` 8 MB) ships only with
  the optional `vis-foundation-voice` extension; drop the jar, drop the weight.
- `sqlite-jdbc` (11 MB) is bundled by the optional `vis-persistance-sqlite`
  extension (native SQLite for all platforms).
- The final GraalVM **native binary** is larger than any single jar because it
  statically links the JDK + Truffle/GraalPy; track its size in the
  `native-release` workflow output.

---

## 6. Pending / follow-ups

- [ ] Add the free **NVD API key** secret and evaluate switching CI to the
      `dependency-check` strategy for full CVSS scoring.
- [ ] Resolve the **LGPL / Lanterna** distribution question (§4) with legal.
- [ ] Flip on the CI **fail gate** (`-f` / `-c`) once the first clean baseline lands.
- [ ] _(from Wojtek — pending input)_ additional compliance / threat-model items.

---

## 7. Maintenance

Regenerate this inventory after any dependency bump:

```bash
clojure -M:antq          # list outdated deps
clojure -M:clj-watson scan -p deps.edn -a '*' -t github-advisory -s  # re-scan CVEs
```

Keep the `:clj-watson` alias pinned to a released tag **and** its `:git/sha`
(currently `v6.1.0` / `be98e4d`) so scans are reproducible; bump both together.
