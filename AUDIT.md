# vis — Security Audit

> Full-source security audit of the vis agent runtime, performed 2026-07-19.
> Method: manual static review of the security-critical surfaces (sandbox
> filesystem confinement, OAuth/token handling, gateway HTTP server + auth,
> web channel, shell/git executors, MCP client, discovery/pairing, config &
> credential persistence), cross-checked against the prior code-holes pass in
> `audit/SECURITY-FINDINGS.md`. Static analysis only — no runtime
> exploitation. The dependency/CVE surface is covered separately by the
> automated clj-watson / NVD scans (`audit/README.md`).
>
> Severity reflects impact **once the affected boundary is exposed** (e.g. a
> non-loopback / tunnelled deployment), not the default single-user desktop
> posture.

## Trust model

vis executes model-authored code, so "security" here means **containment
boundaries**. There are four:

- **Model Python sandbox** — filesystem confinement is enforced *below*
  Python, at the Truffle `FileSystem` layer
  (`src/com/blockether/vis/internal/sandbox_fs.clj`), so it is a **hard**
  boundary the model cannot monkey-patch away. Network is a **soft**
  guardrail (self-documented as such); the only hard network control is the
  `:network/enabled` capability.
- **Python extensions** — fully trusted: `allowIO ALL` +
  `allowCreateProcess true`
  (`src/com/blockether/vis/internal/python_extensions.clj`). A dropped `.py`
  extension has no confinement. By design — but it is the real blast radius
  if a hostile extension is installed.
- **Shell layer** — arbitrary `bash -lc`, but **off by default** and
  toggle-gated (`src/com/blockether/vis/internal/foundation/shell.clj`).
- **Gateway** — loopback = authless by default; non-loopback = mandatory
  bearer token.

## Findings

Legend: **new** (this pass) / **open** (prior pass, re-confirmed in current
source) / **fixed** (verified present in current source) / **accepted**
(documented design choice).

### Open — needs remediation

| # | Sev | Finding | Location | Status |
|---|-----|---------|----------|--------|
| 6 | MED | **Unbounded request-body slurp.** `body-json` does `(some-> (:body request) slurp wire/parse-json)` with no size cap on any JSON endpoint → heap-exhaustion DoS from a single large POST once the gateway is reachable. Fix: reject on `Content-Length` over a cap and read through a bounded stream. | `src/com/blockether/vis/internal/gateway/server.clj:190-194` | **open** (re-confirmed) |
| 8 | LOW | **sandbox-fs TOCTOU.** `confine!` validates the *canonicalised* path (`real-path pp`) but returns the *original* `pp`; the delegated file op then runs on the original path. A symlink swapped between check and use (needs a concurrent thread; `allowCreateThread` is true in the sandbox) could escape the roots. Fix: perform the delegate op on the resolved real path. | `src/com/blockether/vis/internal/sandbox_fs.clj:116-139` | **open** (re-confirmed) |

### Fixed — verified present in current source

| # | Finding | Location | Verified |
|---|---------|----------|----------|
| 1 | **DOM-XSS in web channel client re-render** (`innerHTML = marked.parse(...)` unsanitised). Now sanitised through vendored DOMPurify 3.4.11 (served locally, no CDN) before DOM insertion. | `resources/vis-channel-web/public/ui.js` | DOMPurify wired at every renderProse sink |
| 2 | **`javascript:`/`data:` link schemes in server-side markdown render.** `safe-href` gates the `ast->hiccup` `a`-branch: only `http`/`https`/`mailto` + relative/`#fragment`/protocol-relative pass; control chars stripped pre-check so tab/newline-smuggled schemes can't slip. | `extensions/channels/vis-channel-web/.../channel_web/core.clj` | `safe-href` present |
| 3 | **Provider API keys world-readable.** `save-config!` writes `~/.vis/config.edn` via `spit-private!` (atomic create mode `600`, POSIX perm attribute — not write-then-chmod); `ensure-private-dir!` tightens `~/.vis` to `700`. | `src/com/blockether/vis/internal/config.clj` | `spit-private!` present |
| 4 | **Non-constant-time token comparison.** All secret comparisons (gateway `wrap-auth` bearer + `X-Vis-Gateway-Secret`; web channel `ui-authed?`, magic-link, `auth-handler`) routed through nil-safe `constant-time=?` backed by `MessageDigest/isEqual`. | `gateway/server.clj`, `channel_web/core.clj` | `constant-time=?` present in both |
| 5 | **`vis_token` cookie lacked `Secure`.** Cookie now carries `:secure (request-secure? request)` — set on direct HTTPS or `X-Forwarded-Proto: https` (Cloudflare tunnel), gated so plain-HTTP local deployments still work. | `channel_web/core.clj` | present |
| 10 | **Token-file permission race.** `ensure-token!` creates `gateway.token` mode `600` atomically via `Files/createFile` + `PosixFilePermissions/asFileAttribute`. | `gateway/server.clj` | present |
| 11 | **UI spoofing via allowed HTML classes in re-rendered markdown.** Client sanitiser tightened: `FORBID_ATTR ["style","id"]` + a DOMPurify `uponSanitizeAttribute` hook that keeps ONLY `language-*` classes — attacker markdown can no longer reuse app chrome classes to forge system UI. | `resources/vis-channel-web/public/ui.js` | hook present |

### Accepted — documented design choices

| # | Finding | Location | Posture |
|---|---------|----------|---------|
| 7 | **Loopback gateway is authless** — any local process/user can drive the full agent API. Fine single-user; on shared hosts it is local privilege escalation. | `gateway/server.clj` | accepted — recommend `--require-token` on multi-user hosts |
| 9 | **Network guard is best-effort** — patched inside the model's own interpreter, defeatable by the model. Only the `:network/enabled` capability is a hard control; with it ON, egress is open except the deny-list. | `src/com/blockether/vis/internal/env_python.clj` | accepted — self-documented in code |

## What is solid (credited)

- **Filesystem confinement below Python** — enforced at the Truffle
  `FileSystem` layer; model code cannot patch it away (`sandbox_fs.clj`).
- **No shell injection in git/shell executors** — `git.clj` / `shell.clj`
  spawn via `ProcessBuilder` **argument vectors**, never string
  interpolation into a shell.
- **SSRF hardening** — cloud-metadata endpoints (169.254.169.254 et al.)
  denied by default even under a `*` network allow-list
  (`env_python.clj`).
- **External URL opener is allow-listed** — `external_opener.clj` classifies
  schemes and returns `:rejected-scheme` / `:path-escape` instead of blindly
  execing; never throws into callers.
- **Shell layer off by default**; SSE and shell output bounded against
  memory blow-up.
- **OAuth refresh single-flighted** to prevent token-rotation races; gateway
  token and config written atomically with owner-only permissions.
- **Server-side markdown render is escape-safe** (hiccup2 escaping) with the
  `safe-href` scheme gate on top.

## Recommendations (priority order)

1. **Cap request bodies** in `body-json` (`gateway/server.clj:190`) —
   reject `Content-Length` above a sane limit (e.g. 10 MB) and wrap the body
   stream in a bounded reader. Closes finding #6.
2. **Return the resolved path from `confine!`** (`sandbox_fs.clj:139` — the
   final `pp)` should be `real)`) so the delegated op acts on the validated
   real path. Closes finding #8. Verify the outbox-tap path still receives
   the path shape it expects.
3. On multi-user hosts, document/encourage token-required gateway even on
   loopback (finding #7).

## Scope notes

- Python extensions and MCP servers are trusted-by-design; the audit treats
  "hostile extension installed" as out of scope (equivalent to hostile code
  execution on the host).
- Dependency CVEs: see the automated clj-watson / NVD scan results under the
  repository Security tab; not re-reviewed here.
