# vis — Security & Dependency Audit

> Generated 2026-07-05 by `scripts/gen-audit.bb` (run in CI — see §12). The
> dependency inventory (§5), license distribution (§6), CVE posture (§7) and
> resource footprint (§8) are pulled straight from the resolved `deps.edn` graph
> and the Clojars / Maven Central POMs — **do not edit those by hand**; rerun
> `bb scripts/gen-audit.bb`. The surrounding prose (product, ownership,
> technology, licensing, data-governance, warranty) is reviewed on change.

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

- **Primary language:** Clojure 1.12 on the JVM (Java 25 / GraalVM), compiled to a native image.
- **Direct dependency coordinates:** 57 unique, across 15 `deps.edn` modules (root + extensions).
- **Declared jar footprint (direct coords):** ~218 MB; concentrated in the embedded GraalPy runtime and the optional voice/ONNX stack (§8).
- **License posture:** permissive throughout (EPL, MIT, Apache-2.0, BSD, UPL) — **copyleft exception(s) flagged in §6.**
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
  (§11). Web: <https://blockether.com>.

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
sizes are the direct artifact only (not the transitive closure). "Ownership"
distinguishes **Blockether in-house** libraries (we control the source and
release cadence) from **3rd-party** open source.

### core (deps.edn)

_Shipped binary runtime — the `vis` CLI, agent loop, HTTP gateway, sandbox._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `babashka/fs` | `0.5.33` | EPL-1.0 | 28 KB | 3rd-party |
| `babashka/process` | `0.6.25` | EPL-1.0 | 23 KB | 3rd-party |
| `com.blockether/anomaly` | `1.0.1` | EPL-1.0 | 4 KB | Blockether (in-house) |
| `com.blockether/fff` | `0.9.6-8` | MIT | 11 KB | Blockether (in-house) |
| `com.blockether/ruff` | `0.1.2` | MIT | 9 KB | Blockether (in-house) |
| `com.blockether/svar` | `0.7.46` | Apache-2.0 | 369 KB | Blockether (in-house) |
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
| `org.yamlstar/yamlstar` | `0.1.11` | MIT | 40 KB | 3rd-party |
| `ring/ring-core` | `1.15.5` | MIT | 34 KB | 3rd-party |
| `ring/ring-jetty-adapter` | `1.15.5` | MIT | 7 KB | 3rd-party |
| `slipset/deps-deploy` | `0.2.5` | EPL-1.0 | 8 KB | 3rd-party |

### `vis-channel-telegram` extension

_Telegram bot channel._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `org.babashka/http-client` | `0.4.23` | MIT | 16 KB | 3rd-party |
| `org.clojure/clojure` | `1.12.5` | EPL-1.0 | 4.0 MB | 3rd-party |

### `vis-channel-tui` extension

_Terminal UI (Lanterna)._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.blockether/lanterna` | `3.1.5-vis.20` | LGPL-3.0 | 584 KB | Blockether (in-house) |

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

### `vis-foundation-voice` extension

_Local speech (sherpa-onnx / ONNX Runtime)._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.litongjava/sherpa-onnx-java-api` | `1.0.1` | Apache-2.0 | 7.7 MB | 3rd-party |
| `com.microsoft.onnxruntime/onnxruntime` | `1.17.1` | MIT | 83.5 MB | 3rd-party |

### `vis-language-clojure` extension

_Clojure language pack (format/lint/structural edits)._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `borkdude/edamame` | `1.5.39` | EPL-1.0 | 26 KB | 3rd-party |
| `clj-kondo/clj-kondo` | `2026.05.25` | EPL-1.0 | 701 KB | 3rd-party |
| `com.fasterxml.jackson.core/jackson-core` | `2.19.4` | Apache-2.0 | 578 KB | 3rd-party |
| `com.fasterxml.jackson.dataformat/jackson-dataformat-cbor` | `2.19.4` | Apache-2.0 | 70 KB | 3rd-party |
| `com.fasterxml.jackson.dataformat/jackson-dataformat-smile` | `2.19.4` | Apache-2.0 | 95 KB | 3rd-party |
| `dev.weavejester/cljfmt` | `0.16.4` | EPL-1.0 | 19 KB | 3rd-party |
| `parinferish/parinferish` | `0.8.0` | Public-Domain | 8 KB | 3rd-party |

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

### `vis-workspace-rift` extension

_Rift workspace/FFM integration._

| Dependency | Version | License | Jar size | Ownership |
|---|---|---|---|---|
| `com.blockether/rift` | `0.0.10-5` | MIT | 11 KB | Blockether (in-house) |

---

## 6. Licenses & code ownership

### License distribution (direct coordinates)

| License | Count |
|---|---|
| EPL-1.0 | 19 |
| MIT | 13 |
| Apache-2.0 | 12 |
| BSD-2-Clause | 3 |
| EPL-2.0 | 3 |
| UPL-1.0 | 2 |
| UPL-1.0 + MIT + PSF | 2 |
| (floating) | 1 |
| Public-Domain | 1 |
| LGPL-3.0 | 1 |

All licenses in the graph are **permissive / OSI-approved** (EPL-1.0/2.0, MIT,
Apache-2.0, BSD, UPL-1.0, PSF, Public Domain) and compatible with shipping vis
under **Apache-2.0** — **with the copyleft exception(s) below that need legal sign-off:**

> **WARNING — `com.blockether/lanterna` (`3.1.5-vis.20`) is LGPL-3.0** (copyleft). LGPL is generally fine for dynamic
> linking, but **static linking into the GraalVM native image** can trigger
> relinking obligations. Action: confirm distribution terms with legal, or keep
> the owning extension as an optional (droppable) jar rather than baking it into
> the distributed binary (see §4.3).

### Code ownership

- **First-party (this repo, Apache-2.0):** the `com.blockether.vis.core` package
  and every `extensions/*` module.
- **Blockether in-house libraries** (separate repos, we own source + releases):
  every `com.blockether/*` coordinate above.
- **3rd-party:** everything else in §5, sourced from Clojars / Maven Central.

---

## 7. Vulnerability posture — clj-watson (CVEs)

### Are there any CVEs?

**No known CVEs are outstanding against the shipped dependency graph.**
[clj-watson](https://github.com/clj-holmes/clj-watson) resolves the *entire*
graph (direct **and** transitive) and matches every artifact against a
vulnerability database on every dependency change; the current run reports no
open advisories. Live findings — the always-current answer — are published to
the repository **Security → Code scanning** tab; this document is not the
system of record for live CVE state, the Security tab is.

### What it is & why

[clj-watson](https://github.com/clj-holmes/clj-watson) (clj-holmes / Sean
Corfield community, EPL-2.0) is a **Software Composition Analysis (SCA)** tool
for Clojure. Most real-world risk in a JVM app lives in transitive
dependencies, not code we wrote — clj-watson gives an automated, reproducible
answer to "does anything we ship have a known CVE?", wired into CI so the
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
| `org.graalvm.python/python-language` | `25.1.3` | 90.7 MB |
| `com.microsoft.onnxruntime/onnxruntime` | `1.17.1` | 83.5 MB |
| `org.graalvm.python/python-resources` | `25.1.3` | 13.8 MB |
| `org.xerial/sqlite-jdbc` | `3.53.2.0` | 11.4 MB |
| `com.litongjava/sherpa-onnx-java-api` | `1.0.1` | 7.7 MB |
| `org.clojure/clojure` | `1.12.5` | 4.0 MB |
| `org.apache.commons/commons-compress` | `1.28.0` | 1.1 MB |

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
  ("packing phase") fetches artifacts from Clojars / Maven Central at
  build/resolve time only. Optional voice runs **locally** via on-device ONNX
  models (§4.2) — no cloud speech service.
- **Data providers.** vis bundles no proprietary datasets or model weights;
  content it processes is the host project's own files plus whatever the
  operator sends to their chosen LLM provider. The operator remains the data
  controller.

---

## 10. Commercial licensing, support & warranty

`vis` and its first-party code are distributed **"AS IS", without warranty of
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

- **ship and integrate the base** into your environment;
- **guide teams** on how to *use*, *secure* and *distribute* vis to match your
  regulatory and internal requirements;
- **build custom extensions** (channels, language packs, workspace/tool
  integrations) on the extension surface;
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

---

## 12. Maintenance & follow-ups

This file is generated. To refresh it after a dependency bump:

```bash
bb scripts/gen-audit.bb        # regenerate audit/README.md from the deps graph
clojure -M:antq                # list outdated deps
clojure -M:clj-watson scan -p deps.edn -a '*' -t github-advisory -s   # re-scan CVEs
```

CI enforces freshness: the `audit-md` workflow reruns `bb scripts/gen-audit.bb`
on every `deps.edn` change and weekly, and `bb scripts/gen-audit.bb --check`
fails a PR that leaves audit/README.md stale. Keep the `:clj-watson` alias
pinned to a released tag **and** its `:git/sha` so scans stay reproducible.

**Pending / follow-ups** (hand-maintained inline in the `gen` string of
`scripts/gen-audit.bb`; everything else on this page is generated):

- [ ] Provision the free **NVD API key** as the `NVD_API_KEY` repo secret to
      activate the `nvd-scan` (dependency-check / CVSS) job. The CI wiring is
      already in place; the job self-skips until the secret is set.
- [ ] Resolve the **LGPL / Lanterna** distribution question (§4.3, §6) with legal.
- [x] **Native-image distribution** for paid delivery (§4): **done** — the release
      build uses **GraalVM CE** (GPL-2.0 + Classpath Exception), so the binary can
      be shipped or sold under any terms. Oracle GraalVM/GFTC no longer applies.
- [ ] Flip on the CI **fail gate** (`-f` / `-c`) once the first clean baseline lands.
