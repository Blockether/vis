# Process sandbox and gateway egress

Vis treats model-started processes as untrusted. On macOS, every managed shell,
nested shell, Python `subprocess`, managed Clojure/Python/Bun REPL, and project
test runner inherits one Seatbelt profile. The profile confines files and makes
the gateway's loopback proxy the only outbound network endpoint.

This page describes the security boundary, its configuration, and its explicit
exceptions. See [Configuration](configuration.md) for all other `vis.yml` keys.

## One master switch

**The jail is OFF by default.** It is opt-in via one boolean. There is no shell
toggle and no independent network-enabled toggle.

```yaml
jail:
  enabled: true
```

> **Strongly recommended.** When the jail is off, every model-started shell,
> subprocess, and managed REPL runs with the gateway user's **full host
> permissions** — it can read your SSH keys, exfiltrate secrets, and reach any
> network host. Set `jail.enabled: true` on any project where the model runs
> untrusted code. Leave it off only when you deliberately want that unrestricted
> access (e.g. a fully trusted local workflow).

With `jail.enabled: true` the OS process jail and forced gateway egress path are
active for managed child processes on:

- **macOS** — Seatbelt via the system `sandbox-exec` (ships with the OS, zero install).
- **Linux** — bubblewrap (`bwrap`) mount + network namespaces; install `bubblewrap`
  (e.g. `apt-get install bubblewrap`). Filesystem confinement and network-off are
  kernel-enforced. Per-host/verb *filtered* egress through the proxy still needs
  seccomp, so a proxy-restricted network is denied ENTIRELY (safe) on Linux rather
  than left open; an explicitly-open network shares the host namespace.

If the host cannot enforce a jail (e.g. Linux without `bwrap`, or Windows), a
requested `jail.enabled: true` **fails loud** — a one-time stderr WARNING that
children run unconfined — instead of silently pretending safety. A missing policy,
failed policy function, unknown session, or disposed session does **not** silently
disable confinement; managed process launch fails closed.

Omitting `jail.enabled` (or setting it `false`) leaves confinement off. Setting it
`true` and running `/reload` enables it with no restart.

`jail.enabled: false` does not turn the in-process GraalPy context into a trusted
context. `python_execution` still has its Truffle filesystem and host/socket
restrictions.

## Filesystem policy

The session's active workspace roots are readable and writable, and temporary
locations needed by ordinary programs are available. Every other filesystem root
is declared ONCE in the `workspace.filesystem` catalog and admitted into the jail
by id.

Each catalog entry has an `id`, a `path` (absolute `/…` or home-relative `~`/`~/…`
— a bare-relative path is rejected when the config is read), an optional
`description` (shown in the session access view), an `access` of `read-write`
(default) or `read-only`, and `search` (default `true`; `search: false` keeps the
root out of the default `rg`/`find_files` sweep while explicit paths still reach
it).

`jail.filesystem.allow` then lists the ids that enter the OS jail
(deny-by-omission — a catalog root NOT listed is not confined-granted); RW vs
read-only comes from the catalog entry.

```yaml
workspace:
  filesystem:
    - id: shared
      path: ~/shared-repository
      description: shared code the agent may edit
    - id: reference
      path: ~/reference-data
      access: read-only
    - id: m2
      path: ~/.m2
      description: Maven/Clojure dependency cache
      search: false          # granted, but kept out of the default search sweep

jail:
  filesystem:
    allow: [shared, reference, m2]
```

Managed language dependency caches (`~/.m2`, `~/.clojure`, `~/.npm`, …) are **not**
implicit — grant them as catalog entries (typically `search: false`) and list
their ids under `jail.filesystem.allow`.

`/cd` changes the active workspace root. `/fs add <path>` adds a temporary session
root. Those roots participate in draft isolation and the same policy; they are not
a second permission store.

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
    - host: db.internal
      ports: [5432]           # only Postgres; every other port on this host is denied

jail:
  inbound-ports:
    - 5273
```

Host denies win over allows. `read-only` means `GET`, `HEAD`, and `OPTIONS`;
`full` allows every method; `none` denies every method. `allow` entries add
method/path exceptions. Paths use glob matching. A rule's optional `ports` list
restricts which destination ports reach that host for EVERY protocol (HTTP(S)
CONNECT and the SOCKS5 lane) — e.g. `ports: [22, 443]` allows only ssh + https
to a host, `ports: [5432]` only Postgres. A rule with no `ports` allows any port
(the default); a rule that lists only `ports` leaves verbs unrestricted.

`denied-domains` blocks a host by BOTH its name and its resolved IP: a concrete
denied name (not a `*.` glob) is resolved to its addresses, and any dial whose
destination resolves to one of those addresses is refused at the connect
chokepoint — so a child cannot bypass the denylist by resolving the name itself
and dialing the raw IP literal. Glob entries (`*.evil.com`) still match by name
only (a wildcard has no single IP to resolve). For the hardest boundary, use a
strict `allowed-domains` allowlist — it is enforced against IP-literal targets
too (a non-listed IP is denied) — and combine it with the always-on SSRF floor,
which validates every *resolved* address (loopback reserved ports,
link-local/metadata, private ranges) and cannot be bypassed by IP literal or DNS
rebinding. This applies
identically to HTTP(S) and the SOCKS5 lane — both share one host gate.

HTTPS verb and path enforcement uses a gateway-owned ephemeral CA and TLS
termination. Common clients receive CA environment variables, and managed JVMs
receive a temporary PKCS12 truststore. Hosts in `exclude-domains` are opaque TLS
tunnels for certificate-pinned or otherwise incompatible clients; host policy
still applies, but methods, paths, and plaintext network filters cannot be
inspected there.

Non-HTTP TCP (ssh, git-over-ssh, databases, arbitrary raw TCP) traverses a SOCKS5
lane on the SAME loopback port — the proxy multiplexes it by the first byte, so
the jail needs no extra opening. `ALL_PROXY` points non-HTTP clients at
`socks5h://…` while `http_proxy`/`https_proxy` keep the HTTP proxy so web verb and
path policy still apply. The SOCKS lane carries the session token in the RFC 1929
username and enforces the same host allow/deny and SSRF floor, but no method or
path (those protocols carry none). Tools that read no proxy environment (`ssh`
itself) still need an explicit `ProxyCommand`.

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
closed on an exception. HTTP and MITM'd HTTPS pass method, host, path, and headers
(response phases also include status). The **SOCKS5 lane runs the same filters** at
connect time with `phase="socks"` plus host and port (no method/path — raw TCP
carries no verb), so one guard covers both HTTP and SOCKS. Tunnelled hosts are
intentionally opaque.

### Debugging filters with `/net-probe`

A filter runs in the gateway's trusted context, so a bug shows up only as "traffic
 denied" — never a stack trace at the call site (a filter that throws **fails
closed**). Use `/net-probe` to see, without touching the network, exactly what the
host gate and every registered filter decide for a synthetic request:

```
/net-probe POST https://api.github.com/repos    # HTTP: method + path visible
/net-probe github.com:22                         # bare host:port -> SOCKS phase
```

It reports the Tier-1 host/port/SSRF verdict, then **each** filter individually
(never collapsing on the first deny) with its allow/deny reason — and, when a
filter crashed, the fail-closed marker plus the Python **traceback**. The dev loop
is: edit the extension `.py`, `/reload`, `/net-probe …`.

### Authoring + probing a filter inside `python_execution`

The same loop is available **without an extension file or `/reload`**, right in the
Python sandbox, via two baseline builtins:

```python
def block_writes(ctx):
    # ctx = {phase, method, host, path, port, headers}
    if ctx.get('method') in ('POST', 'PUT', 'DELETE', 'PATCH'):
        return 'mutations blocked'          # a str reason (or {'reason': ...}) DENIES
    return None                             # None ALLOWS; a raise FAILS CLOSED

network_filter(block_writes)                # register a local (session-scoped) guard
network_probe('POST', 'https://api.github.com/repos')   # http: verb + path
network_probe('db.host:5432')               # bare host[:port] -> SOCKS phase
```

`network_probe` is **guard-only** — it runs the gateway's Tier-1 host/port/SSRF gate
**+ every registered gateway filter + your local `network_filter`s** against a
*synthetic* request and prints each verdict (with the Python traceback for a crash).
It **never opens a socket and never sends anything** — it exercises only the
decision. Two honest limits: a `network_filter` registered here is **local to the
session probe** (it does not affect live egress — author a `.py` extension for that),
and these are bare names in the sandbox (there is no `vis.` module inside
`python_execution`).


## Denying a command, then re-allowing it through a trusted extension

A common policy is "the model's shell and its agent tools must never run `aws`,
but a reviewed extension may proxy specific AWS calls." The sandbox supports this
because confinement is applied **per spawn**, not ambiently, and the two callers
take different code paths to the OS.

**Block a command with `jail.deny-exec`.** There is no argv denylist; a command
is blocked by forbidding EXECUTION of its binary. List the command names and Vis
resolves each on `PATH` at config-read, emitting a Seatbelt `(deny process-exec*)`
for **every** matching executable — kernel-enforced, no leaky argv parsing:

```yaml
jail:
  deny-exec: [curl, wget, ssh]
```

An absolute or `~`-rooted entry (e.g. `/opt/homebrew/bin/aws`) is denied
verbatim; an unresolvable name is a no-op. Every process spawned through the jail
— the managed shell (`shell_run` / `shell_bg`), agent tool commands, managed
REPLs, and project test runners — inherits the profile, so `curl …` fails to
exec with `Operation not permitted`.

This overrides the jail's blanket exec allow. Note that denying **read** on a
binary would not stop its execution on macOS (the kernel maps an allowed binary
without a file-read check) — `deny-exec` is what actually blocks the command.

**`deny-exec` is convenience, not containment.** It blocks a *named* binary and
its symlinks — not the *capability*. An interpreter already on the allow-list
substitutes trivially: with `curl` denied, `python3 -c "import urllib.request; …"`
or `bash`'s `/dev/tcp` do the same job, and a script the child writes into a
writable root and runs is never on your list. Treat `deny-exec` as a guardrail
that stops the obvious/lazy invocation and trims the tool surface — the real
egress boundary is the **network** layer (net-off, or proxy-filtered
allow-domains + verb rules), which contains *whatever* binary tries to reach out.
Never rely on `deny-exec` alone to keep a capability away from the child.

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
    # never sandbox-exec-wrapped and the jail's deny-exec on the aws binary
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
- The OS enforcer is implemented on macOS (Seatbelt) and Linux (bubblewrap);
  Windows and other hosts have none. There the gateway policy remains useful but
  is not a kernel boundary, and a requested `jail.enabled: true` fails loud rather than
  pretending to confine. On Linux, filtered egress through the proxy is not yet
  kernel-enforced (needs seccomp), so a proxy-restricted network is denied entirely.

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

## Verification (macOS + Linux CI)

Pure compiler/policy tests run on every OS. Socket and Seatbelt enforcement tests
run only in an unconfined test JVM: a managed JVM that already has
`VIS_SEATBELT_ACTIVE=1` cannot apply a second profile or bind arbitrary fixture
ports, so those cases report a conditional skip. CI runs both the macOS job (real
Seatbelt, `VIS_REQUIRE_MACOS_SANDBOX_E2E=1`) and the Linux job (real bubblewrap,
`VIS_REQUIRE_LINUX_SANDBOX_E2E=1`), so the OS boundary is executed — not vacuously
skipped — on both platforms.

The focused suites cover:

- Seatbelt read/write/deny rules and nested-process inheritance;
- direct-network denial and proxy-only egress;
- HTTP and HTTPS MITM method/path policy, and the SOCKS5 lane for raw TCP;
- session token attribution, network filters, and SSRF denial;
- PTY/background input and attach bridge behavior;
- managed process launch, fail-closed session lookup, CA/truststore injection;
- config validation and `jail.enabled: true` opt-in / omitted-or-`false` as the off default;
- Linux bubblewrap argv compilation (every OS) and real bwrap filesystem containment (Linux CI);
- fail-loud passthrough + reason when a jail is requested on a host that cannot enforce it.

Run the relevant namespaces through the Clojure language pack or the full macOS
CI job. A test JVM already started by a sandboxed session validates its inherited
profile but cannot substitute for the unconfined CI enforcement run.
