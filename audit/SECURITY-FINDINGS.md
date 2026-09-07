# vis — Code Security Findings

> Historical static review of gateway, Python execution, shell/Git operations
> and credential storage. Dependency scans are documented in
> [the generated audit](README.md) and the repository Security tab.
> No runtime exploitation was performed. Statuses below record this review,
> not a current security assessment; some referenced code has since changed.

## Trust model (context for every finding)

Vis executes model-generated code. Review these components separately:

- **Model Python sandbox:** CPython audit hooks check filesystem and network
  operations according to the session policy. Filesystem confinement depends
  on whether the jail is enabled.
- **Python extensions:** trusted code with the user's permissions. Review
  project extensions before loading an unfamiliar repository.
- **Shell processes:** available when the shell toggle is enabled. The process
  jail applies when enabled; see [process policy](../resources/vis-docs/jail.md).
- **Gateway:** loopback access does not require authentication by default;
  other bind addresses require a token.

## Findings

Legend: **status** is `fixed` / `open` / `accepted` (documented design choice).

### HIGH / MEDIUM

| # | Finding | Location | Status |
|---|---------|----------|--------|
| 3 | **Provider API keys persisted world-readable.** `save-config!` did `.mkdirs` + `spit` on the `~/.vis` config store with no permission tightening; the file holds the provider API key in plaintext at the process umask (typically `644`). Contrast the gateway token, deliberately `chmod rw-------`. On a shared host any local user could read the LLM provider keys. | `src/com/blockether/vis/internal/config/core.clj` (`save-config!`, today `~/.vis/state.yml`) | **fixed** |
| 4 | **Non-constant-time token comparison.** The bearer token was compared with `=`, a timing side-channel once auth is enabled (non-loopback). | `gateway/server.clj` (`wrap-auth`) | **fixed** |
| 6 | **Unbounded JSON request bodies.** The reviewed implementation lacked a request-size limit, risking memory exhaustion. Use bounded reads and enforce body-size limits. | `gateway/server.clj:356` | **open** at review |

Findings 1, 2, 5 and 11 were browser web-channel issues (`vis-channel-web` and its
client JavaScript). That channel has since been removed from the repository, so
those findings are retired — the code they described no longer exists.

### LOW / INFO (mostly documented design choices)

| # | Finding | Location | Status |
|---|---------|----------|--------|
| 7 | **Loopback gateway without authentication.** Local processes can access the agent API. Require a token on shared hosts. | `gateway/server.clj:3034-3046` | **accepted** at review |
| 8 | **Filesystem check/use race.** Path validation and access used different path representations. Use the resolved path consistently. | `sandbox_fs.clj:110-140` | **open** at review |
| 9 | **Interpreter-level network checks.** The reviewed checks did not provide OS-level network isolation. See current process policy before relying on network restrictions. | `env_python.clj` | **accepted** at review |
| 10 | **Token-file permissions.** Setting permissions after writing briefly exposed the token. | `gateway/server.clj` (`ensure-token!`) | **fixed** |

## Other controls reviewed

- CPython audit hooks for filesystem checks.
- Argument vectors in `git.clj` and `shell.clj` process creation.
- Default blocking of cloud-metadata destinations.
- Shell toggle and process-jail configuration.
- Bounded SSE and shell output.
- Serialized OAuth token refresh to avoid concurrent rotation.

## Remediation log

- **#1, #2, #5 and #11 — retired.** All four were web-channel findings (client
  `renderProse` sanitising, the server-side `safe-href` link-scheme gate, and the
  `vis_token` cookie). The web channel was removed from the repository, so there
  these fixes no longer apply to current code.
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
