# Process sandbox and gateway egress

Vis treats model-started processes as untrusted. On macOS, every managed shell,
nested shell, Python `subprocess`, managed Clojure/Python/Bun REPL, and project
test runner inherits one Seatbelt profile. The profile confines files and makes
the gateway's loopback proxy the only outbound network endpoint.

This page describes the security boundary, its configuration, and its explicit
exceptions. See [Configuration](configuration.md) for all other `vis.yml` keys.

## One master switch

The sandbox is on by default. There is no shell toggle and no independent
network-enabled toggle. On a supported host, the only user-configurable process
escape hatch is:

```yaml
sandbox: false
```

That value disables the OS process jail and the forced gateway egress path for
managed child processes. Use it only when intentionally granting those children
the gateway user's full host permissions. A missing policy, failed policy
function, unknown session, or disposed session does **not** disable confinement;
managed process launch fails closed.

`sandbox: false` does not turn the in-process GraalPy context into a trusted
context. `python_execution` still has its Truffle filesystem and host/socket
restrictions.

## Filesystem policy

The session's active workspace roots are readable and writable. Temporary
locations needed by ordinary programs are also available. Add explicit
carve-outs in YAML:

```yaml
jail:
  filesystem:
    allow-read-write:
      - ~/shared-repository
    allow-read:
      - ~/reference-data
    allow-write:
      - /srv/generated
    deny-read:
      - ~/shared-repository/.secrets
    deny-write:
      - /srv/generated/locked
    language-caches:
      - ~/.m2
      - path: ~/.clojure
        access: read-only
```

`jail.filesystem.allow-read-write` grants both operations. `allow-write` is also
readable (a writable file must be inspectable); `allow-read` is process-only
read access. Deny entries are emitted after allows, so deny wins for an
overlapping subtree. Managed language dependency caches are **not** implicit: a
bare `language-caches` path is read/write; a map with `access: read-only` is
read-only. Every filesystem path must be absolute (`/…`) or home-relative
(`~`/`~/…`); a bare-relative path is rejected when the config is read, because it
would resolve against the gateway process directory rather than a session root.

`/cd` changes the active workspace root. `/fs add <path>` adds a temporary
session root. Those roots participate in draft isolation and the same policy;
they are not a second permission store.

## Environment scrubbing

A confined child does **not** inherit the operator's environment. Only an
allowlist of non-secret variables is passed through (`PATH`, `HOME`, `USER`,
`SHELL`, `LANG`/`LC_*`, `TERM`, `TZ`, `TMPDIR`, `PWD`, …) plus this session's
proxy and CA variables; every `*_KEY` / `*_TOKEN` / `*_SECRET` / `*_PASSWORD`
and other operator credential is dropped before the process starts. This covers
`shell_run`, `shell_bg`, Python `subprocess`, and every managed language REPL /
test runner.

To pass a specific extra variable through to confined children, list its exact
name under `jail.env`:

```yaml
jail:
  env: [CI, MY_BUILD_TOKEN]   # exact var names; everything else stays dropped
```

File **metadata** (existence, size, mtime) is likewise scoped: a child may stat
its granted roots and the directory ancestors it needs to resolve paths, but not
read the size/mtime of files beneath `$HOME` such as `~/.ssh/id_ed25519`.

## Network policy

Every confined child is denied direct sockets except the gateway proxy and any
explicit inbound listener ports. The proxy attributes each connection to its
session, blocks SSRF destinations, and applies one policy to shell HTTP clients,
managed language processes, and GraalPy HTTP clients.

```yaml
network:
  allowed-domains:
    - "*"
  denied-domains:
    - evil.example
  exclude-domains:
    - pinned.example
  allow-private: false
  rules:
    - host: api.example.com
      access: read-only
      allow:
        - method: POST
          path: /v1/issues/**
    - host: "*"
      access: read-only

jail:
  inbound-ports:
    - 5273
```

Host denies win over allows. `read-only` means `GET`, `HEAD`, and `OPTIONS`;
`full` allows every method; `none` denies every method. `allow` entries add
method/path exceptions. Paths use glob matching.

HTTPS verb and path enforcement uses a gateway-owned ephemeral CA and TLS
termination. Common clients receive CA environment variables, and managed JVMs
receive a temporary PKCS12 truststore. Hosts in `exclude-domains` are opaque TLS
tunnels for certificate-pinned or otherwise incompatible clients; host policy
still applies, but methods, paths, and plaintext network filters cannot be
inspected there.

Loopback development services are reachable except Vis's reserved gateway and
proxy ports. Link-local, cloud metadata, wildcard, and multicast destinations
are always denied. RFC1918, CGNAT, and IPv6 ULA destinations require
`allow-private: true`. The proxy resolves and validates addresses before dialing
the validated address, preventing DNS-rebinding pivots.

`jail.inbound-ports` permits a confined development server to accept on those
ports. Managed nREPL receives only its preselected loopback port and does not
inherit this general server list.

## Programmable network filters

A trusted Python extension may add one phase-aware gateway filter:

```python
import vis


def policy(ctx):
    if ctx["phase"] == "http-request" and ctx["method"] == "POST":
        return vis.block("POST is not permitted")
    if ctx["phase"] == "http-response" and ctx["status"] >= 500:
        return vis.block("upstream failure hidden")
    return None


vis.extension(
    name="network-policy",
    description="Project network policy",
    network_filters=[vis.network_filter(policy)],
)
```

Filters run in the gateway at the decrypted request/response boundary and fail
closed on an exception. They receive request method, host, path, and headers;
response phases also include status. Tunnelled hosts are intentionally opaque.

## Denying a command, then re-allowing it through a trusted extension

A common policy is "the model's shell and its agent tools must never run `aws`,
but a reviewed extension may proxy specific AWS calls." The sandbox supports this
because confinement is applied **per spawn**, not ambiently, and the two callers
take different code paths to the OS.

**Deny the binary in the jail.** There is no argv denylist; a command is blocked
by denying read on its executable, because macOS requires read access to exec a
binary. Point `jail.filesystem.deny-read` at the CLI's path:

```yaml
jail:
  filesystem:
    deny-read:
      - /opt/homebrew/bin/aws
      - /usr/local/aws-cli
```

Every process spawned through the jail — the managed shell (`shell_run` /
`shell_bg`), agent tool commands, managed REPLs, and project test runners — is
wrapped with `sandbox-exec` and inherits `(deny file-read*)` on those paths, so
`aws …` fails to exec. Deny wins over allow, so this holds even inside an
otherwise readable directory.

**Re-allow it inside a trusted extension.** A Python extension runs in its own
context with real process creation, and its subprocesses are **not** routed
through `wrap-argv`, so they are never `sandbox-exec`-wrapped. A reviewed
extension can therefore shell out to the same denied binary and expose it as a
narrow, audited tool. The whole thing is one file dropped into
`~/.vis/extensions/` (global) or `<project>/.vis/extensions/` (project-local) —
no rebuild, `/reload`-live. See [Python extensions](python-extensions.md) for
the full authoring surface; here is a complete, buildable proxy:

```python
# ~/.vis/extensions/aws_proxy.py
"""aws-proxy — audited, read-only AWS access that bypasses the jail's aws deny."""
import subprocess

import vis

# Scope the hole as tightly as the policy it re-opens: an allowlist, read-only
# verbs, no shell string, an explicit timeout. This tool IS the audit surface.
_ALLOWED_BUCKETS = {"acme-public-assets", "acme-reports"}


def aws_s3_ls(bucket):
    """await aws_s3_ls(bucket) -> {"listing"} — list one allowlisted S3 bucket."""
    if bucket not in _ALLOWED_BUCKETS:
        # Raising is the failure path — the message surfaces to the model.
        raise ValueError(f"bucket {bucket!r} not in allowlist {sorted(_ALLOWED_BUCKETS)}")
    # Runs UNCONFINED: this subprocess is not routed through wrap-argv, so it is
    # never sandbox-exec-wrapped and the jail's deny-read on the aws binary
    # never applies here. argv form (no shell) — the model never shapes a string.
    out = subprocess.run(
        ["aws", "s3", "ls", f"s3://{bucket}"],
        capture_output=True, text=True, check=True, timeout=30,
    )
    vis.log("info", f"aws-proxy: s3 ls {bucket}")
    return {"listing": out.stdout}


vis.extension(
    name="aws-proxy",
    description="Audited, read-only AWS access.",
    kind="integration",
    alias="aws",
    symbols=[vis.symbol(aws_s3_ls, tag="observation")],
)
```

The model calls `await aws_s3_ls("acme-reports")`. The sandbox name is
`f"{alias}_{name}"`, but a leading `"{alias}_"` on the function name is stripped
first, so `alias="aws"` + `def aws_s3_ls` lands as `aws_s3_ls`, not doubled. Raw
`aws` from the model's shell stays denied.

The result: the *capability* to exec the binary stays open at the OS layer, while
*authorization* moves to the tool layer. Raw `aws` from the model is denied; the
extension's `aws_s3_ls` tool — which you wrote, reviewed, and scoped — is the
only way that binary runs. Treat such an extension as privileged: it is a
deliberate hole in the jail, so review it like an executable build file and keep
its surface as narrow as the policy it bypasses.

**Caveat — an inherited kernel Seatbelt closes the hole.** This asymmetry exists
only when Vis's own JVM is not itself launched under an ambient Seatbelt profile.
When it is (`VIS_SEATBELT_ACTIVE=1`, as in a sandboxed harness session), the
kernel enforces the parent profile on the **entire** process tree; Seatbelt
inheritance is one-way, so children — extension subprocesses included — can only
tighten it, never loosen it. In that mode no extension can escape, because the
kernel owns the JVM, and `wrap-argv` deliberately skips re-wrapping
(`inherited-jail?`) since a nested `sandbox-exec` is rejected.

## Trust boundaries and exceptions

- `python_execution` is in-process GraalPy. It cannot receive a per-thread
  Seatbelt profile, but its filesystem is confined and HTTP is routed through
  the same gateway policy. Its raw-socket host guard remains a separate floor.
- Managed REPLs started by `repl_start` and managed project test processes are
  child processes and are jailed.
- `repl_connect` attaches to an already-running, user-owned external process.
  Vis did not spawn it and cannot retroactively apply Seatbelt. Stopping the
  resource detaches; it never kills that process.
- Python extension files are intentionally trusted plugins. Their separate
  contexts have real filesystem, network, inherited environment, threads, and
  process creation. They have no arbitrary Java/native/polyglot interop. Review
  project `.vis/extensions/` with the same care as executable build files.
- On hosts without an implemented OS enforcer, process wrapping is currently an
  explicit platform gap. The gateway policy remains useful, but it is not a
  kernel boundary.

## Reload, inheritance, and model context

Security configuration is resolved and snapshotted when a root session environment
is built. Relative paths become absolute and symlinks are resolved at that boundary.
Every child environment inherits the exact same snapshot; it never rereads
`vis.yml`. Editing a model-writable configuration file therefore cannot widen a
running parent or child. `/reload` bumps one process-wide policy epoch that
invalidates **every** active session, not only the one you typed it in. The
rebuild is lazy and per session: each session recycles its environment and
re-snapshots from the current `vis.yml` on its **next** message, so an idle
session keeps its old snapshot until you send it something.

The same effective snapshot is exposed read-only as `session["access"]`. It includes
a SHA-256 generation id, filesystem modes, network policy, inbound ports, and
`changes_require: "reload"`. Live workspace roots are a controlled overlay and
appear as context deltas. Paths beneath the user's home are displayed as `~` paths
(for example `~/vis` and `~/spel`); enforcement always uses the resolved absolute
paths.

## macOS verification

Pure compiler/policy tests run on every OS. Socket and Seatbelt enforcement tests
run only in an unconfined macOS test JVM; a managed JVM that already has
`VIS_SEATBELT_ACTIVE=1` cannot apply a second profile or bind arbitrary fixture
ports, so those cases report a conditional skip. The macOS CI job is unconfined
and therefore executes the real boundary tests.

The focused suites cover:

- Seatbelt read/write/deny rules and nested-process inheritance;
- direct-network denial and proxy-only egress;
- HTTP and HTTPS MITM method/path policy;
- session token attribution, network filters, and SSRF denial;
- PTY/background input and attach bridge behavior;
- managed process launch, fail-closed session lookup, CA/truststore injection;
- config validation and `sandbox: false` as the explicit opt-out.

Run the relevant namespaces through the Clojure language pack or the full macOS
CI job. A test JVM already started by a sandboxed session validates its inherited
profile but cannot substitute for the unconfined CI enforcement run.
