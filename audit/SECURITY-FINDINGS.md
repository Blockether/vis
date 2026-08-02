# vis — Code Security Findings

> Manual source-code security review of the vis runtime — the HTTP/gateway
> stack, both Python sandboxes, the shell/git executors, and credential
> persistence. This is the **code-holes** pass; the dependency/CVE surface is
> covered separately by the automated
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
- **Python *extensions*** — trusted by design: `allowIO ALL` and full host
  filesystem reach; native process creation is off (`allowCreateProcess false`) and
  `subprocess` / `os.system` route through `vis.shell` under the session `wrap-argv`
  policy (`src/com/blockether/vis/internal/python_extensions.clj:172-187`). A dropped
  `.py` extension still has no meaningful confinement — the real blast radius if a
  hostile extension is installed.
- **Shell layer** — arbitrary `bash -lc`, **default ON** behind the user-owned `shell`
  toggle and the OS process jail
  (`src/com/blockether/vis/internal/foundation/shell.clj:2066-2091`).
- **Gateway** — loopback = authless by default; non-loopback = mandatory token.

## Findings

Legend: **status** is `fixed` / `open` / `accepted` (documented design choice).

### HIGH / MEDIUM

| # | Finding | Location | Status |
|---|---------|----------|--------|
| 3 | **Provider API keys persisted world-readable.** `save-config!` did `.mkdirs` + `spit` on the `~/.vis` config store with no permission tightening; the file holds the provider API key in plaintext at the process umask (typically `644`). Contrast the gateway token, deliberately `chmod rw-------`. On a shared host any local user could read the LLM provider keys. | `src/com/blockether/vis/internal/config.clj` (`save-config!`, today `~/.vis/state.yml`) | **fixed** |
| 4 | **Non-constant-time token comparison.** The bearer token was compared with `=`, a timing side-channel once auth is enabled (non-loopback). | `gateway/server.clj` (`wrap-auth`) | **fixed** |
| 6 | **Unbounded request-body slurp.** `body-json` does `(slurp (:body request))` with no size cap on any JSON endpoint → heap-exhaustion DoS from a large POST. | `gateway/server.clj:356` | **open** (fix: cap Content-Length / bounded read) |

Findings 1, 2, 5 and 11 were browser web-channel issues (`vis-channel-web` and its
client JavaScript). That channel has since been removed from the repository, so
those findings are retired — the code they described no longer exists.

### LOW / INFO (mostly documented design choices)

| # | Finding | Location | Status |
|---|---------|----------|--------|
| 7 | **Loopback gateway is authless** — any local process/user can drive the full agent API with no token. Fine for single-user desktop; on shared hosts it is local privilege escalation. | `gateway/server.clj:3034-3046` | **accepted** (recommend `--require-token` for multi-user) |
| 8 | **sandbox-fs TOCTOU.** `confine!` validates the *canonicalised* path but returns the *original* path; the delegate op runs on the original. A symlink swapped between check and use (needs a concurrent thread; `allowCreateThread` is `true`) could escape. Low likelihood, real gap. | `sandbox_fs.clj:110-140` | **open** (fix: delegate on the resolved real path) |
| 9 | **Network guard is best-effort** — patched in the model's own interpreter, defeatable by the model. The only hard control is the `:network/enabled` capability. With network capability ON, the model can egress anywhere not on the deny-list. | `env_python.clj` | **accepted** (self-documented) |
| 10 | **Token-file permission race.** Write-then-chmod left a sub-millisecond world-readable window at umask. | `gateway/server.clj` (`ensure-token!`) | **fixed** |

## What is solid (credited)

- Filesystem confinement enforced **below** Python — cannot be patched away by
  model code (`sandbox_fs.clj`).
- `git.clj` / `shell.clj` spawn via `ProcessBuilder` **argument vectors** — no
  shell-injection despite the surface.
- Cloud-metadata SSRF endpoints denied by default even under `*`
  (`env_python.clj:1517`).
- Shell layer runs behind a user-owned toggle and the OS process jail; SSE /
  shell output bounded against memory blow-up.
- OAuth refresh single-flighted to prevent token-rotation races.

## Remediation log

- **#1, #2, #5 and #11 — retired.** All four were web-channel findings (client
  `renderProse` sanitising, the server-side `safe-href` link-scheme gate, and the
  `vis_token` cookie). The web channel was removed from the repository, so there
  is no code left to carry those fixes.
- **#3 world-readable API keys — fixed.** `save-config!` now writes
  the `~/.vis` config store (today `state.yml`) through `spit-private!`, which creates the file mode
  `600` via `Files/createFile` with a POSIX perm attribute (atomic, not
  write-then-chmod), and `ensure-private-dir!` tightens `~/.vis` to `700`.
  Falls back to plain `spit` on a non-POSIX filesystem.
- **#4 non-constant-time token compare — fixed.** Added a nil-safe
  `constant-time=?` (backed by `MessageDigest/isEqual` on UTF-8 bytes) and
  routed every secret comparison through it: the gateway `wrap-auth`
  (`Authorization: Bearer` + `X-Vis-Gateway-Secret`).
- **#10 token-file perm race — fixed.** `ensure-token!` now creates
  `gateway.token` mode `600` atomically via `Files/createFile` +
  `PosixFilePermissions/asFileAttribute`, closing the write-then-chmod window.
