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
| 2 | **`javascript:` / `data:` link schemes not filtered.** Server-side `ir->hiccup` rendered any markdown link href verbatim (`[x](javascript:…)` becomes a clickable JS link — escaping does not stop the scheme). | `src/com/blockether/vis/ext/channel_web/core.clj` (`ast->hiccup` `a` branch) | **fixed** |
| 3 | **Provider API keys persisted world-readable.** `save-config!` did `.mkdirs` + `spit` on `~/.vis/config.edn` with no permission tightening; the file holds `:api-key` in plaintext at the process umask (typically `644`). Contrast the gateway token, deliberately `chmod rw-------`. On a shared host any local user could read the LLM provider keys. | `src/com/blockether/vis/internal/config.clj` (`save-config!`) | **fixed** |
| 4 | **Non-constant-time token comparison.** The bearer token was compared with `=`, a timing side-channel once auth is enabled (non-loopback). | `gateway/server.clj` (`wrap-auth`); `channel_web/core.clj` (`ui-authed?` / `index-handler` / `auth-handler`) | **fixed** |
| 5 | **`vis_token` cookie has no `Secure` flag.** Set HttpOnly + SameSite=Lax + path=/ but not `:secure`. Behind a TLS-terminating tunnel (Cloudflare — explicitly targeted) any plain-HTTP hop leaks the token. | `channel_web/core.clj` (cookie drop sites) | **fixed** |
| 6 | **Unbounded request-body slurp.** `body-json` did `(slurp (:body request))` with no size cap on any JSON endpoint → heap-exhaustion DoS from a large POST. | `gateway/server.clj` (`body-json`) | **fixed** |
| 11 | **UI spoofing via allowed HTML in re-rendered markdown.** The client re-render sanitised with `DOMPurify.sanitize(…, {USE_PROFILES:{html:true}})`, whose default allow-list keeps `class`/`style`/`id`. That is not script-XSS (so #1 didn't catch it), but it let attacker-influenceable markdown (user message, tool-result body, model output) inject raw `<div>`/`<span>` reusing the app's OWN chrome classes + tool color-role vars to forge an authentic-looking system badge, e.g. `<div class="block-result-label" style="color:var(--tool-shell)">SHELL BACKGROUND … running (pid …)</div>` — a phishing / trust-forgery surface. | `resources/vis-channel-web/public/ui.js` (renderProse) | **fixed** |

### LOW / INFO (mostly documented design choices)

| # | Finding | Location | Status |
|---|---------|----------|--------|
| 7 | **Loopback gateway is authless** — any local process/user can drive the full agent API with no token. Fine for single-user desktop; on shared hosts it is local privilege escalation. | `gateway/server.clj:398,598` | **accepted** (recommend `--require-token` for multi-user) |
| 8 | **sandbox-fs TOCTOU.** `confine!` validates the *canonicalised* path but returns the *original* path; the delegate op runs on the original. A symlink swapped between check and use (needs a concurrent thread; `allowCreateThread` is `true`) could escape. Low likelihood, real gap. | `sandbox_fs.clj:80` | **open** (fix: delegate on the resolved real path) |
| 9 | **Network guard is best-effort** — patched in the model's own interpreter, defeatable by the model. The only hard control is the `:network/enabled` capability. With network capability ON, the model can egress anywhere not on the deny-list. | `env_python.clj:1538` | **accepted** (self-documented) |
| 10 | **Token-file permission race.** Write-then-chmod left a sub-millisecond world-readable window at umask. | `gateway/server.clj` (`ensure-token!`) | **fixed** |

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
- **#11 UI spoofing — fixed.** Tightened the client sanitiser: `FORBID_ATTR:
  ["style","id"]` drops inline styling/anchors, and a DOMPurify
  `uponSanitizeAttribute` hook keeps ONLY `language-*` classes (Prism / diff /
  vis-paste highlighting), stripping every other class. Re-rendered markdown can
  no longer reuse the app's structural/chrome classes to impersonate system UI,
  while code-fence highlighting is preserved.
- **#3 world-readable API keys — fixed.** `save-config!` now writes
  `~/.vis/config.edn` through `spit-private!`, which creates the file mode
  `600` via `Files/createFile` with a POSIX perm attribute (atomic, not
  write-then-chmod), and `ensure-private-dir!` tightens `~/.vis` to `700`.
  Falls back to plain `spit` on a non-POSIX filesystem.
- **#4 non-constant-time token compare — fixed.** Added a nil-safe
  `constant-time=?` (backed by `MessageDigest/isEqual` on UTF-8 bytes) and
  routed every secret comparison through it: the gateway `wrap-auth`
  (`Authorization: Bearer` + `X-Vis-Gateway-Secret`) and the web channel's
  `ui-authed?`, magic-link, and `auth-handler` cookie/form checks.
- **#5 cookie missing `Secure` — fixed.** The `vis_token` cookie now carries
  `:secure (request-secure? request)`, set on a TLS hop — a direct HTTPS
  scheme or `X-Forwarded-Proto: https` from a terminating proxy (Cloudflare).
  Gated so a plain-HTTP deployment can still store and return the cookie.
- **#10 token-file perm race — fixed.** `ensure-token!` now creates
  `gateway.token` mode `600` atomically via `Files/createFile` +
  `PosixFilePermissions/asFileAttribute`, closing the write-then-chmod window.
- **#2 link-scheme allowlist — fixed.** Added `safe-href`, gating the
  `ast->hiccup` `a`-tag branch: only `http` / `https` / `mailto` (plus
  schemeless relative / `#fragment` / `//host` protocol-relative) hrefs are
  emitted; `javascript:` / `data:` / `vbscript:` / `file:` are dropped and the
  anchor renders without an `href` (non-navigable), mirroring DOMPurify on the
  client. ASCII control chars are stripped before the scheme check so a
  `java\tscript:` newline/tab-smuggled scheme cannot slip past. This closes
  the server render path (#1 already covered the client re-render).
- **#6 unbounded body slurp — fixed.** `body-json` no longer `slurp`s the whole
  body. It streams `(:body request)` STRAIGHT into `charred` via
  `wire/parse-json-stream` through a `bounded-input-stream` capped at
  `MAX_BODY_BYTES` (4 MiB): a declared-oversize `Content-Length` is rejected
  before a byte is read, and an actually-oversize / slow-drip body trips a
  `:body-too-large` ex-info mid-read that `wrap-errors` maps to **413 Payload
  Too Large** — the read aborts instead of buffering, so the heap can't be
  exhausted. nil / blank / malformed bodies still resolve to nil (→ the
  caller's 400), preserving the existing contract.
