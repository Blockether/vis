# Process jail & egress

The process jail confines code started for a session. It limits filesystem access,
scrubs the child environment, and routes outbound connections through the gateway's
policy. The boundary covers managed child processes; the in-process GraalPy sandbox
uses its own filesystem and socket guards.

## Enable the boundary

The jail is off unless the merged `vis.yml` contains:

```yaml
jail:
  enabled: true
```

Run `/reload` after changing security configuration. The next message in each active
session rebuilds that session's immutable policy snapshot. `session["access"]` shows
the effective snapshot, including `is_jailed`, filesystem modes, network policy, and
`changes_require`.

Shell availability is separate. `toggles.shell: false` removes `shell(...)`; it does
not change the policy for other managed processes.

## What is confined

| execution path | boundary |
|---|---|
| `shell(...)`, nested shells, and Python `subprocess` reached through the sandbox | OS process jail plus gateway egress policy |
| REPLs started by `repl_start` and project test runners | same session policy as `shell(...)` |
| `python_execution` | GraalPy filesystem and socket guards; HTTP uses the gateway policy while the jail is enabled |
| `repl_connect` | not confined; it attaches to a process Vis did not start |
| Python extension code and its ordinary `subprocess` calls | trusted host code, outside the session jail |

A trusted extension can opt into confinement with `vis.jailed_shell(...)` or use the
invoking session's snapshot with `vis.jailed_shell_session(...)`. Project extension
files are executable plugins and require the same review as build scripts.

## Filesystem admission

Declare additional roots once under `workspace.filesystem`, then admit them by id
under `jail.filesystem.allow`:

```yaml
workspace:
  filesystem:
    - id: sibling
      path: ~/sibling-repository
      description: repository used by this project
      access: read-write
      draft: copy-and-apply
    - id: reference
      path: ~/reference-data
      access: read-only
    - id: m2
      path: ~/.m2
      access: read-only
      search: false

jail:
  enabled: true
  filesystem:
    allow: [sibling, reference, m2]
```

The active workspace and temporary directories are writable. An admitted catalog
root uses its declared `access`; a catalog root omitted from `allow` is absent from a
confined child. Dependency caches are not granted automatically.

Every path must be absolute or home-relative. `when.os`, `when.exists`, and
`optional: true` let one catalog cover hosts where a root is unavailable. An id may
remain in `allow` when its conditional catalog entry does not apply. An unknown id is
a configuration error.

`search: false` keeps a granted root out of default `grep` searches without blocking
an explicit path. `draft` controls isolated workspace copies independently of the OS
jail. Vis also grants `~/.vis` read/write and excludes it from default searches;
declare it explicitly only to change that access.

## Environment scrubbing

With the jail enabled, a child receives:

1. basic non-secret variables such as `PATH`, `HOME`, `LANG`, `TERM`, `TZ`, and
   `TMPDIR`;
2. values resolved from the project's `.env`, `.env.local`, and top-level
   `environment:` block;
3. the session's proxy and CA variables.

The operator's remaining environment is omitted. Declare a needed ambient value at
the top level instead of copying it into the jail block:

```yaml
environment:
  CI: {env: CI}
  BUILD_TOKEN: {keychain: vis-build}
```

`jail.environment: inherit` passes the operator's ambient environment to confined
children, including exported credentials. Filesystem and network rules still apply.
Pre-exec injection variables such as `LD_*`, `DYLD_*`, `BASH_ENV`, and `PERL*` are
refused in both modes because they could run before the jail is installed.

When the jail is disabled, child processes inherit the host environment with project
values layered on top.

## Network egress

A confined process cannot dial the network directly. HTTP, HTTPS, and proxy-aware raw
TCP clients reach a session-authenticated gateway proxy. The proxy resolves the
requested host, applies the session policy, and dials the validated address.

Without a `jail.network` block, public destinations are allowed. These protections
still apply:

- link-local, cloud metadata, wildcard, and multicast addresses are blocked;
- private IPv4 ranges, CGNAT, and IPv6 ULA require `allow_private: true`;
- loopback services are allowed except the gateway's control and proxy ports.

Use `allowed_domains` for an allowlist and `denied_domains` for explicit blocks.
Denies win. A concrete denied hostname is also blocked by its resolved addresses;
wildcard entries match names. `exclude_domains` disables TLS inspection for clients
that pin certificates, but it does not bypass host, port, or SSRF checks.

```yaml
jail:
  enabled: true
  network:
    allowed_domains:
      - api.github.com
      - "*.pypi.org"
    denied_domains:
      - blocked.example
    allow_private: false
```

### Method, path, and port rules

Rules narrow a host by HTTP method, path, and destination port:

```yaml
jail:
  enabled: true
  network:
    rules:
      - host: api.example.com
        access: read-only
        allow:
          - method: POST
            path: /v1/issues/**
      - host: db.example.com
        access: full
        ports: [5432]
```

`read-only` permits `GET`, `HEAD`, and `OPTIONS`; `full` permits all methods; `none`
permits none. `methods` can name an explicit method set, and `allow` adds method/path
exceptions. `ports` applies to HTTP CONNECT and SOCKS as well as ordinary HTTP.

The gateway terminates inspected HTTPS with an ephemeral session CA. Common HTTP
clients receive CA environment variables, and managed JVMs receive a temporary trust
store. Raw TCP uses a SOCKS5 lane on the same proxy port and therefore has host and
port checks but no HTTP method or path. A program that ignores proxy variables, such
as `ssh`, needs an explicit proxy command.

### Inbound development ports

A confined server may accept only ports listed in
`jail.network.inbound_ports`. Managed nREPL uses its own preselected loopback port and
does not inherit this list.

```yaml
jail:
  enabled: true
  network:
    inbound_ports: [5273]
```

### Project network filters

A trusted Python extension can register a gateway `network_filter`. Filters see HTTP
request and response phases plus SOCKS connection attempts; an exception denies the
request. Use `/net-probe` to test the host gate and every registered filter without
opening a socket. Inside `python_execution`, `network_filter(...)` and
`network_probe(...)` test session-local filters, but those local filters do not alter
live egress.

See [Extending Vis](extending.md) for the extension API.

## Platform enforcement

| host | enforcer | requirement |
|---|---|---|
| macOS | Seatbelt through `/usr/bin/sandbox-exec` | included with macOS |
| Linux and WSL2 | bubblewrap mount and network namespaces | `bubblewrap`; `passt` for filtered egress |
| WSL1 and other systems | no supported OS process jail | use a supported host for kernel confinement |

On Debian or Ubuntu:

```bash
sudo apt-get install -y bubblewrap passt
```

On Linux, `bwrap` is required for filesystem confinement. `pasta`, supplied by
`passt`, creates a private network namespace that can reach only the gateway proxy
port. If `pasta` is missing, filtered egress becomes no egress and Vis prints one
warning.

If an enabled jail cannot be enforced, Vis prints one warning and starts the child
unconfined. This is not a security boundary. A missing or failed session policy is a
different case: managed process launch is denied because Vis cannot determine which
policy to apply.

## Executables and macOS services

`jail.deny_exec` blocks named executables inside confined children:

```yaml
jail:
  enabled: true
  deny_exec: [curl, wget]
```

This is a command guardrail, not capability containment. Another interpreter or a
new script can perform the same operation. Use filesystem and network policy for the
actual boundary.

Seatbelt denies Mach service lookups by default. Permit macOS Keychain helpers with:

```yaml
jail:
  enabled: true
  mach_services:
    keychain: true
    allow: [com.example.agent]
```

`keychain: true` grants the Security, trust, and revocation services plus read access
to the system and user keychain databases. `allow` grants other global Mach service
names. These settings are ignored outside macOS.

## Diagnose the effective policy

1. Inspect `session["access"]`; do not infer access from the YAML file alone.
2. Run `/reload` after a config edit, then send a message in each session that must
   adopt it.
3. Use `/net-probe METHOD URL` or `/net-probe host:port` for egress decisions.
4. On Linux, verify `bwrap --version` and `pasta --version` when the startup warning
   reports a missing enforcer.
5. Treat a child started after an unenforceable-jail warning as unconfined.

The policy snapshot resolves paths and symlinks when the session environment is
built. Live workspace roots can change within that snapshot, but editing `vis.yml`
cannot widen an existing environment until `/reload` invalidates it.

## See also

- [Configuration](configuration.md): complete `workspace`, `jail`, `environment`, and toggle key reference.
- [GraalPython sandbox](graalpython.md): the in-process Python boundary.
- [Gateway, pairing & remote access](gateway.md): the daemon that owns the egress proxy.
