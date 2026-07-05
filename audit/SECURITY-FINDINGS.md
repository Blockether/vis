# vis — Code Security Findings

> Manual source-code security review of the vis runtime — the HTTP/gateway
> stack, both Python sandboxes, the shell/git executors, the web channel and
> its client JavaScript, and credential persistence. This is the **code-holes**
> pass; the dependency/CVE surface is covered separately by the automated
> clj-watson / NVD scans (see `audit/README.md` and the repository Security tab).
>
> Scope: static analysis, no runtime exploitation. Severity reflects impact
> **once the affected boundary is exposed** (e.g. a non-loopback / tunnelled
> deployment), not the default single-user desktop posture.

## Trust model (context for every finding)

vis executes model-authored code, so "security" here is about **containment
boundaries**. There are four:

- **Model Python sandbox** — filesystem confinement is enforced *below* Python,
  at the Truffle `FileSystem` layer (`src/com/blockether/vis/internal/sandbox_fs.clj`),
  so it is a **hard** boundary. Network is a **soft** guardrail (labelled as such
  in the code); the only hard network control is the `:network/enabled` capability.
- **Python *extensions*** — fully trusted: `allowIO ALL` + `allowCreateProcess true`
  (`src/com/blockether/vis/internal/python_extensions.clj:273`). A dropped `.py`
  extension has no confinement. By design, but it is the real blast radius if a
  hostile extension is installed.
- **Shell layer** — arbitrary `bash -lc`, but **off by default** and toggle-gated
  (`src/com/blockether/vis/internal/foundation/shell.clj:59`). Correct posture.
- **Gateway** — loopback = authless by default; non-loopback = mandatory token.

## Findings

Legend: **status** is `fixed` / `open` / `accepted` (documented design choice).

### HIGH / MEDIUM

| # | Finding | Location | Status |
|---|---------|----------|--------|
| 1 | **DOM-XSS in the web channel — no HTML sanitizer.** The client re-render did `el.innerHTML = marked.parse(data-md)` with no sanitisation. Every `data-md` sink carries attacker-influenceable markdown (raw user message, model answer, thinking, tool-result bodies), so `<img src=x onerror=…>` executed JS inside the authenticated `/ui` origin. The server-side `ir->hiccup` path is safe (hiccup2 escapes); the client re-render bypassed it. | `resources/vis-channel-web/public/ui.js` (renderProse) | **fixed** |
| 2 | **`javascript:` / `data:` link schemes not filtered.** Server-side `ir->hiccup` renders any markdown link href verbatim (`[x](javascript:…)` becomes a clickable JS link — escaping does not stop the scheme). | `src/com/blockether/vis/ext/channel_web/core.clj:282` (server); client re-render now sanitised by #1 | **open** (client path covered by DOMPurify; server path should allowlist `http/https/mailto`) |
| 3 | **Provider API keys persisted world-readable.** `save-config!` does `.mkdirs` + `spit` on `~/.vis/config.edn` with no permission tightening; the file holds `:api-key` in plaintext at the process umask (typically `644`). Contrast the gateway token, deliberately `chmod rw-------`. On a shared host any local user can read the LLM provider keys. | `src/com/blockether/vis/internal/config.clj:544` | **open** (fix: create `~/.vis` `700`, config `600`, mirroring `ensure-token!`) |
| 4 | **Non-constant-time token comparison.** The bearer token is compared with `=`, a timing side-channel once auth is enabled (non-loopback). | `gateway/server.clj:422`; `channel_web/core.clj:1800,1851` | **open** (fix: `MessageDigest.isEqual` on the bytes) |
| 5 | **`vis_token` cookie has no `Secure` flag.** Set HttpOnly + SameSite=Lax + path=/ but not `:secure`. Behind a TLS-terminating tunnel (Cloudflare — explicitly targeted) any plain-HTTP hop leaks the token. | `channel_web/core.clj:1841,1855` | **open** (fix: `:secure true` when non-loopback) |
| 6 | **Unbounded request-body slurp.** `body-json` does `(slurp (:body request))` with no size cap on any JSON endpoint → heap-exhaustion DoS from a large POST. | `gateway/server.clj:83` | **open** (fix: cap Content-Length / bounded read) |

### LOW / INFO (mostly documented design choices)

| # | Finding | Location | Status |
|---|---------|----------|--------|
| 7 | **Loopback gateway is authless** — any local process/user can drive the full agent API with no token. Fine for single-user desktop; on shared hosts it is local privilege escalation. | `gateway/server.clj:398,598` | **accepted** (recommend `--require-token` for multi-user) |
| 8 | **sandbox-fs TOCTOU.** `confine!` validates the *canonicalised* path but returns the *original* path; the delegate op runs on the original. A symlink swapped between check and use (needs a concurrent thread; `allowCreateThread` is `true`) could escape. Low likelihood, real gap. | `sandbox_fs.clj:80` | **open** (fix: delegate on the resolved real path) |
| 9 | **Network guard is best-effort** — patched in the model's own interpreter, defeatable by the model. The only hard control is the `:network/enabled` capability. With network capability ON, the model can egress anywhere not on the deny-list. | `env_python.clj:1538` | **accepted** (self-documented) |
| 10 | **Token-file permission race.** Write-then-chmod leaves a sub-millisecond window at umask. | `gateway/server.clj:59` | **open** (minor; create with restrictive perms atomically) |

## What is solid (credited)

- Filesystem confinement enforced **below** Python — cannot be patched away by
  model code (`sandbox_fs.clj`).
- `git.clj` / `shell.clj` spawn via `ProcessBuilder` **argument vectors** — no
  shell-injection despite the surface.
- Cloud-metadata SSRF endpoints denied by default even under `*`
  (`env_python.clj:1533`).
- Shell layer off by default; SSE / shell output bounded against memory blow-up.
- OAuth refresh single-flighted to prevent token-rotation races.

## Remediation log

- **#1 DOM-XSS — fixed.** Vendored **DOMPurify 3.4.11** (Apache-2.0 / MPL-2.0,
  both commercial-use permissive) into
  `resources/vis-channel-web/public/purify.min.js`, served locally like the other
  assets (no CDN, no external request). The client re-render now runs
  `DOMPurify.sanitize(marked.parse(data-md), {USE_PROFILES:{html:true}})` before
  DOM insertion, which also strips `javascript:` / `data:` link schemes and
  `on*=` handlers — so the client half of #2 is covered by the same fix.
