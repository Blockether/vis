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
      - ../shared-repository
    allow-read:
      - ~/reference-data
    allow-write:
      - ./generated
    deny-read:
      - ./.secrets
    deny-write:
      - ./generated/locked
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
read-only.

`/cd` changes the active workspace root. `/fs add <path>` adds a temporary
session root. Those roots participate in draft isolation and the same policy;
they are not a second permission store.

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
running parent or child. `/reload` explicitly rebuilds the root environment and
creates a new snapshot.

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
