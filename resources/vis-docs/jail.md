# Process jail and network policy

The process jail lets you limit which files, environment variables and network
connections are available to commands that Vis runs. It is optional and disabled
by default. These limits apply to managed processes, not trusted extension code.
The [Python sandbox](python-sandbox.md) also has its own permission checks.

## Enable the jail

The jail is disabled by default. Enable it in configuration:

```yaml
jail:
  enabled: true
```

After changing security settings, run `/reload`. Each active session uses the
new policy on its next message. The agent can inspect the effective policy in
`session["access"]`, including `is_jailed`, filesystem modes, network rules and
`changes_require`.

Shell availability is separate. `toggles.shell: false` removes `shell(...)`; it does
not change the policy for other managed processes.

## What is confined

| Execution path | Enforcement |
|---|---|
| `shell(...)` and its child processes | OS process jail plus gateway egress policy |
| `python_execution` | CPython filesystem, process and socket guards while the jail is enabled; HTTP then uses the gateway policy |
| Python extension code and its ordinary `subprocess` calls | trusted host code, outside the session jail |

A trusted extension can opt into confinement with `vis.jailed_shell(...)`, which reads
the merged configuration at every spawn. Project extension files are executable plugins
and require the same review as build scripts. A confined child can be given the exact
paths it needs on top of the session roots with the `allow_read_write` and `unix_connect`
options; see [Extension API](extension-api.md).

## Filesystem access

Declare additional roots under `workspace.filesystem`, then allow them by id
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

The active workspace and temporary directories are writable. Allowed roots use
their declared `access`; other than runtime access below, unlisted roots are not
available to jailed children. Dependency caches require explicit access.

Vis automatically grants jailed processes read-only access to recognized Java
installations used by the host JVM, host `JAVA_HOME`, or the first absolute `java`
executable on the host `PATH`. Detection resolves symlinks and requires a Java
installation layout; it does not execute launchers, scan other versions, or grant
entire toolchain-manager directories.

These grants are frozen in the session's
policy snapshot and appear under `session["access"]["filesystem"]["process_read_only"]`
with descriptions. They are excluded from default searches and do not become
workspace roots or grant Python filesystem tools additional access. Explicit
catalog grants retain their access mode and search setting; deny rules still win.

When a language call or an `environment:` declaration selects `JAVA_HOME`, Vis puts
that JDK's `bin` directory first on the child's `PATH` and grants the jailed child
read-only access to it. The launcher and every JVM it starts then run the JDK you
chose — including `tools.deps` dependency preparation, which spawns a bare `java`
and never reads `JAVA_HOME` itself. An environment that also sets or unsets `PATH`
wins, and Vis leaves the search order alone. A `JAVA_HOME` whose `bin` holds no
executable `java` selects nothing.

Other per-call environment overrides do not add runtime grants. A different or
unrecognized toolchain needs an explicit grant; host toolchain changes require
`/reload` before an existing session gains access.

Paths must be absolute or home-relative. Use `when.os`, `when.exists` or
`optional: true` for roots available only on some hosts. An id may remain in
`allow` when its conditional entry does not apply. An unknown id is an error.

`search: false` keeps a granted root out of default `grep` searches without blocking
an explicit path. `draft` controls isolated workspace copies independently of the OS
jail. Vis also grants `~/.vis` read/write and excludes it from default searches;
declare it explicitly only to change that access.

### Deny specific files

Deny rules take individual files back out of every grant. List patterns under
`jail.filesystem.deny_read` and `jail.filesystem.deny_write`:

```yaml
jail:
  enabled: true
  filesystem:
    allow: [sibling, reference]
    deny_read:
      - .env
      - "**/.env*"
      - ~/secrets
    deny_write:
      - deploy/production
```

A pattern is absolute, home-relative, or relative to the workspace root. `*` matches
inside one path segment, `**` crosses directories, and a rule that names a directory
covers everything under it.

Denial wins over allow lists, workspace roots and runtime grants. Vis matches a rule
against the normalized path with symlinks resolved, so a denied file stays denied when
you reach it through another root, a relative path or a link. `deny_read` blocks both
reading and writing; `deny_write` blocks writing only.

The same rules cover host tools (`cat`, `ls`, `grep`, `patch` and the other readers and
writers), `python_execution` and every child process Vis starts. A call that touches
several paths is refused whole: no path in the batch is read or written.

A rule also covers files that appear later. On macOS the pattern itself goes into the
sandbox profile, so a secret written after the session started is denied as soon as it
exists. On Linux, bubblewrap works with mount points rather than patterns: a child is
kept out of the files the pattern matched when the session started, and Vis' own tools
keep refusing every path the rule covers.

`jail.enabled` turns the OS sandbox on and off; the deny rules are separate
configuration and stay valid either way. With the jail off, Vis' own tools keep refusing
every path a rule covers, but nothing confines the commands and code Vis starts: a shell
child or `python_execution` can still open the file. Keep the jail on when a rule has to
hold against code Vis does not run itself.

## Environment filtering

With the jail enabled, a child receives:

1. basic non-secret variables such as `PATH`, `HOME`, `LANG`, `TERM`, `TZ`, and
   `TMPDIR`;
2. values resolved from the project's `.env`, `.env.local`, and top-level
   `environment:` block;
3. the session's proxy and CA variables.

Other parent-process variables are excluded. Declare required variables in the
top-level `environment` block:

```yaml
environment:
  CI: {env: CI}
  BUILD_TOKEN: {keychain: vis-build}
```

`jail.environment: inherit` passes the operator's ambient environment to confined
children, including exported credentials. Filesystem and network rules still apply.
Pre-exec injection variables such as `LD_*`, `DYLD_*`, `BASH_ENV`, and `PERL*` are
refused in both modes because they could run before the jail is installed.

When the jail is disabled, children inherit the host environment; project
values override it.

## Network egress

Jailed processes use a session-authenticated gateway proxy for HTTP, HTTPS and
proxy-aware TCP connections. The proxy resolves the host, checks session policy
and connects to the validated address.

Without a `jail.network` block, public destinations are allowed. These protections
still apply:

- link-local, cloud metadata, wildcard, and multicast addresses are blocked;
- private IPv4 ranges, CGNAT, and IPv6 ULA require `allow_private: true`;
- loopback services are allowed except the gateway's control and proxy ports.

Use `allowed_domains` for an allowlist and `denied_domains` for explicit blocks.
Deny rules take precedence. A denied hostname also blocks its resolved
addresses; wildcard entries match names. `exclude_domains` disables TLS
inspection for clients that pin certificates, but host, port and SSRF checks
still apply.

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

The gateway inspects HTTPS using a temporary session CA. Common HTTP clients
receive CA environment variables, and managed JVMs receive a temporary trust
store. Raw TCP uses SOCKS5 on the same proxy port, with host and port checks
but no HTTP method or path checks. Programs that ignore proxy variables, such
as `ssh`, require explicit proxy configuration.

### Inbound development ports

A confined server accepts connections on a port listed in
`jail.network.inbound_ports`. On macOS the child shares the host's network stack, so
a loopback listener (`localhost:5273`) is reachable from the operator's browser even
unlisted and listing the port additionally opens it to other hosts; on Linux the
child has its own network namespace and the host reaches it only through a listed
port. Managed nREPL uses its own preselected loopback port and does not inherit
this list.

```yaml
jail:
  enabled: true
  network:
    inbound_ports: [5273]
```

### Project network filters

Trusted Python extensions can register gateway `network_filters` for HTTP
requests, responses and SOCKS connections. An exception denies the request.
`/net-probe` checks host policy and registered filters without opening a socket.
Inside `python_execution`, `network_filter(...)` and `network_probe(...)` test
session-local filters; those filters do not change gateway network policy.

See [Extension API](extension-api.md#registration) for `network_filters`.

## Platform enforcement

| host | enforcer | requirement |
|---|---|---|
| macOS | Seatbelt through bundled `libvisjail.dylib` | included with the Vis Python runtime |
| Linux and WSL2 | embedded bubblewrap through bundled `libvisjail.so` | included with the Vis Python runtime |
| WSL1 and other systems | no supported OS process jail | use a supported host for kernel confinement |

No system package, helper executable, `PATH` entry, or operator install is required. Vis
loads the platform library adjacent to `libvispython`; the library applies Seatbelt or
bubblewrap before the child command starts. On Linux, a filtered proxy policy currently
uses a private network namespace with no route, so it fails closed rather than exposing
direct egress.

An enabled jail that this host cannot enforce refuses to start the child, and so
does a missing session policy: Vis never falls back to an unconfined process when
it cannot tell which policy applies.

## Executables and macOS services

`jail.deny_exec` blocks named executables inside confined children:

```yaml
jail:
  enabled: true
  deny_exec: [curl, wget]
```

Blocking an executable does not block every way to perform its operations.
Use filesystem and network policy to restrict those operations.

Jailed children cannot access the OS credential store by default, so `gh`,
`git` credential helpers and similar tools cannot retrieve credentials. Allow
access with:

```yaml
jail:
  enabled: true
  keychain: true
```

On macOS this grants the Keychain services plus read access to the system and user
keychain databases; on Linux it exposes the session D-Bus so the Secret Service
(GNOME Keyring, KWallet) can respond.

## Diagnose the effective policy

1. Inspect `session["access"]`; do not infer access from the YAML file alone.
   `session["access"]["filesystem"]["deny_read"]` and `["deny_write"]` list the
   effective deny rules.
2. Run `/reload` after a config edit, then send a message in each session that must
   adopt it.
3. Use `/net-probe METHOD URL` or `/net-probe host:port` for egress decisions.
4. If startup reports a missing enforcer, verify that the selected Python runtime
   contains the matching `libvisjail` platform library.
5. An enabled jail that cannot be enforced refuses to start the child.

The policy snapshot resolves paths and symlinks when the session environment is
built. Live workspace roots can change within that snapshot, but editing `vis.yml`
cannot widen an existing environment until `/reload` invalidates it.

## See also

- [Configuration](configuration.md): the complete `workspace`, `jail`, `environment` and toggle keys.
- [Python sandbox](python-sandbox.md): Python runtime permissions.
- [Gateway reference](index.md#gateway-reference): client connections and authentication.
