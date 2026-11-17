# Process jail and network policy

The process jail limits the permissions of the commands that Vis runs: their files, environment
variables, and network connections. The jail is optional and off by default. It applies to managed
processes, not to trusted extension code. The [Python sandbox](python-sandbox.md) has its own
permission checks.

## When to use

- **The agent runs code that you did not review, such as the tests of an unfamiliar repository.**
  [Enable the jail](#enable-the-jail).
- **Commands must not read secrets such as `.env` files.**
  [Deny specific files](#deny-specific-files) and
  [filter environment variables](#environment-filtering).
- **Commands must reach only the hosts that you approve, or must only read from them.** Set
  [network egress](#network-egress) rules, down to
  [method, path, and port](#method-path-and-port-rules).
- **A development server in the jail must accept connections.** Open
  [inbound development ports](#inbound-development-ports).
- **Tests start a local server or stop their own child processes.** Read
  [Local servers and child processes](#local-servers-and-child-processes).
- **You run Vis in your own sandbox, and that sandbox denies `/tmp`.**
  [Set a task runtime directory](#run-vis-inside-your-own-sandbox).
- **A command is blocked and you need to know why.**
  [Diagnose the effective policy](#diagnose-the-effective-policy).

## Enable the jail

```yaml
jail:
  enabled: true
```

After you change security settings, run `/reload`. Each active session uses the new policy on its
next message. The agent reads the effective policy in `session["access"]`: `is_jailed`, the deny
rules, the network rules, and `changes_require`. One list,
`session["workspace"]["filesystem_roots"]`, holds each path with its mode, description, and search
setting.

`toggles.shell: false` removes `shell(...)`. It does not change the policy of other managed
processes.

## What is confined

| Execution path | Enforcement |
|---|---|
| `shell(...)` and its child processes | OS process jail and gateway egress policy |
| `python_execution` | CPython filesystem, process, and socket guards while the jail is enabled. HTTP then uses the gateway policy |
| Python extension code and its ordinary `subprocess` calls | Trusted host code, outside the session jail |

A trusted extension can start a jailed child with `vis.jailed_shell(...)`. That call reads the
merged configuration at each spawn. The `allow_read_write` and `unix_connect` options give the child exact paths in addition to the session roots. An extension
declares its fixed toolchain paths, such as a JDK or a package cache, as `vis.Grant` values. Vis
gives those paths only to the jailed children of that extension. Read [Toolchain
grants](extension-api.md#toolchain-grants) in the Extension API.

Project extension files are executable plugins. Review them as you review build scripts.

## Filesystem access

Declare more roots under `workspace.filesystem`, then allow them by id under
`jail.filesystem.allow`:

```yaml
workspace:
  filesystem:
    - id: sibling
      path: ~/sibling-repository
      description: repository used by this project
      access: read-write
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

- The active workspace and the temporary directories are writable.
- An allowed root has its declared `access`.
- A root that is not listed is not available to jailed children. The only exceptions are the
  [runtime grants](#runtime-grants). A dependency cache needs an explicit root.
- A path must be absolute or home-relative.
- For a root that only some hosts have, use `when.os`, `when.exists`, or `optional: true`. An id
  can stay in `allow` when its conditional entry does not apply. An unknown id is an error.
- `search: false` keeps an allowed root out of default `grep` searches. An explicit path still
  works.
- Vis also grants read and write access to `~/.vis` and keeps it out of default searches. Declare
  `~/.vis` only to change that access.

### Runtime grants

Vis gives jailed processes read-only access to two sets of host files.

- **Java installations.** Vis finds them through the host JVM, the host `JAVA_HOME`, and the first
  absolute `java` executable on the host `PATH`. Detection resolves symlinks and requires the
  layout of a Java installation. It does not run launchers, scan other versions, or grant the full
  directory of a toolchain manager.
- **The global git configuration.** With it, `git` works in a jailed shell. The grant covers
  `~/.gitconfig`, `~/.config/git/config`, `~/.config/git/ignore`, `~/.config/git/attributes`, and
  the files that their `include` sections name. It respects `XDG_CONFIG_HOME` and
  `GIT_CONFIG_GLOBAL`. Git credential files stay closed.

These grants show in `session["workspace"]["filesystem_roots"]` with a description, `"mode":
"read"`, and `"no_search": true`. Default searches skip them, and they give Python filesystem tools
no extra access. Explicit catalog grants keep their access mode and search setting. Deny rules still
win. After the host toolchain changes, an existing session gets access only after `/reload`.

A language call or an `environment:` declaration can select `JAVA_HOME`. Vis then puts the `bin`
directory of that JDK first on the `PATH` of the child. It also gives the jailed child read-only
access to that directory. The launcher, each JVM that it starts, and the dependency preparation of
`tools.deps` then run the JDK that you chose.

If the environment also sets or unsets `PATH`, that setting wins. A `JAVA_HOME` whose `bin` has no
executable `java` selects nothing. Other environment overrides of a call add no runtime grant. A
different or unrecognized toolchain needs an explicit grant.

### Deny specific files

To remove single files from every grant, list patterns under `jail.filesystem.deny_read` and
`jail.filesystem.deny_write`:

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

A pattern is absolute, home-relative, or relative to the workspace root. `*` matches in one path
segment, and `**` crosses directories. A rule that names a directory covers everything under it.
`deny_read` blocks read and write. `deny_write` blocks write only.

A deny rule wins over allow lists, workspace roots, and runtime grants. Vis matches a rule against
the normalized path, with symlinks resolved. So a denied file stays denied through a different root,
a relative path, or a link.

The rules cover the host tools (`ls`, `grep`, and the other readers and writers),
`python_execution`, and each child process that Vis starts. When a call names many paths and one is
denied, Vis refuses the whole call.

A rule also covers files that appear later. On macOS, a secret that is written after the session
started is denied when it exists. On Linux, bubblewrap works with mount points, not patterns. A
child cannot open the files that the pattern matched when the session started. The tools of Vis
still refuse every path that the rule covers.

The deny rules are separate from `jail.enabled`, which turns the OS sandbox on and off. With the
jail off, the tools of Vis still refuse each denied path. But nothing confines the commands and
code that Vis starts, so a shell child or `python_execution` can open the file. When a rule must
hold against code that Vis does not run itself, keep the jail on.

## Environment filtering

With the jail enabled, a child gets these variables:

1. Basic variables that are not secret, such as `PATH`, `HOME`, `LANG`, `TERM`, `TZ`, and `TMPDIR`.
2. Values from the `.env` file, the `.env.local` file, and the top-level `environment:` block of
   the project.
3. The proxy and CA variables of the session.

A child gets no other variable of the parent process. Declare each variable that a command needs
in the top-level `environment` block:

```yaml
environment:
  CI: {env: CI}
  BUILD_TOKEN: {keychain: vis-build}
```

With the jail disabled, children inherit the host environment, and project values override it.

Vis always refuses the variables that can run code before the jail is in place. Children never
inherit `NODE_OPTIONS` from the environment that started Vis.
[Environment](configuration.md#environment) lists the variables. If your
project needs Node options, declare `NODE_OPTIONS` under `environment:` or in `.env`.

## Network egress

Jailed processes use a gateway proxy for HTTP, HTTPS, and proxy-aware TCP connections. The proxy
authenticates the session, resolves the host, checks the session policy, and connects to the
validated address.

Without a `jail.network` block, public destinations are allowed. These protections still apply:

- Link-local, cloud metadata, wildcard, and multicast addresses are blocked.
- Private IPv4 ranges, CGNAT, and IPv6 ULA require `allow_private: true`.
- Loopback services are allowed, except the control port and the proxy ports of the gateway.

Use `allowed_domains` for an allowlist and `denied_domains` for explicit blocks. Deny rules win. A
denied hostname also blocks the addresses that it resolves to. Wildcard entries match names.
`exclude_domains` turns off TLS inspection for clients that pin certificates. Host, port, and SSRF
checks still apply to those clients.

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

A rule narrows a host by HTTP method, path, and destination port:

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

`access` sets the allowed HTTP methods:

- `read-only` permits `GET`, `HEAD`, and `OPTIONS`.
- `full` permits all methods.
- `none` permits no methods.

`methods` names an explicit method set. `allow` adds exceptions by method and path. `ports` applies
to HTTP CONNECT, SOCKS, and ordinary HTTP.

The gateway inspects HTTPS with a temporary session CA. Common HTTP clients get CA environment
variables, and managed JVMs get a temporary trust store. Raw TCP uses SOCKS5 on the same proxy
port. It gets host and port checks, but no HTTP method or path checks. A program that ignores
proxy variables, such as `ssh`, needs explicit proxy configuration.

Some clients do not send the proxy credentials, for example the Java HTTP clients. So the Python
worker of each session gets its own proxy port. A port belongs to one session until the gateway
stops, also after the session ends. One gateway run has 256 of these ports. When each port has an
owner, the Python workers of new sessions use the shared proxy. Those clients then get a 407 error
until you restart the gateway.

When the gateway stops, it ends the programs that it started in the jail. On macOS, it also ends a
program that left its parent process, such as a server that a script started in the background.
After a gateway crash, the next gateway start ends these programs.

### Inbound development ports

A jailed server accepts connections on a port in `jail.network.inbound_ports`.

- On macOS, the child shares the network stack of the host. Your browser can reach a loopback
  listener (`localhost:5273`) also when the port is not listed. A listed port is also open to other
  hosts.
- On Linux, the child has its own network namespace. The host reaches the child only through a
  listed port.

Managed nREPL uses its own preselected loopback port and does not inherit this list.

```yaml
jail:
  enabled: true
  network:
    inbound_ports: [5273]
```

### Local servers and child processes

A jailed command can start a server on a loopback port and connect to it without the proxy.

- On macOS, direct loopback connections are allowed only when the proxy also allows loopback. So
  there is no `allowed_domains` list, or the list includes `localhost` and `127.0.0.1`. And no
  denied domain, rule, or project network filter limits those names when the command starts. In
  all other cases, loopback connections go through the proxy and its rules.
- The gateway control port and the proxy ports of all sessions stay closed.
- The ports of other Vis gateways on your computer also stay closed, also of a gateway that starts
  after the command. This limit applies to direct connections and to connections through the proxy.
- macOS cannot separate loopback from the other addresses of your computer. So a direct connection
  can also reach `::1`, and a service that listens only on a LAN or VPN address of your computer.
  Other computers stay blocked.
- On Linux, each command has its own network namespace. A server and a client in the same command
  connect directly. A service on the loopback interface of the host is available only through the
  proxy.
- Windows has no supported process jail.

A jailed command can send signals to the processes that it started and to their children. It
cannot send signals to other processes. On Linux, this limit needs kernel 6.12 or later (Landlock
ABI 6). On an older kernel, a jailed command can send signals to all processes of your user. The
gateway log then shows a warning at the gateway start.

On macOS, `ps` does not run in the jail. To learn if a process that you started still runs, use
`kill -0 <pid>`.

### Project network filters

A trusted Python extension can register gateway `network_filters` for HTTP requests, responses,
and SOCKS connections. An exception in a filter denies the request. `/net-probe` checks the host
policy and the registered filters, and it opens no socket. In `python_execution`,
`network_filter(...)` and `network_probe(...)` test filters that are local to the session. Those
filters do not change the gateway network policy.
[Registration](extension-api.md#registration) in the Extension API describes `network_filters`.

## Platform enforcement

| Host | Enforcer | Requirement |
|---|---|---|
| macOS | Seatbelt through the bundled `libvisjail.dylib` | Included with the Vis Python runtime |
| Linux and WSL2 | Embedded bubblewrap through the bundled `libvisjail.so` | Included with the Vis Python runtime |
| WSL1 and other systems | No supported OS process jail | Use a supported host for kernel confinement |

You install no system package, helper executable, or `PATH` entry. Vis loads the platform library
next to `libvispython`. The library applies Seatbelt or bubblewrap before the child command starts.
On Linux, a filtered proxy policy uses a private network namespace with no
route. If the proxy fails, the child has no network and never gets direct egress.

Vis never starts an unconfined process. It refuses the child when the host cannot enforce an
enabled jail, or when the session policy is missing or unknown.

## Run Vis inside your own sandbox

An evaluation runner or a container can confine Vis itself. By default, Vis keeps its temporary
files and the control socket of each Python worker in the system temporary directory, on most
systems `/tmp`. If your sandbox denies `/tmp`, set `VIS_RUNTIME_DIR` to a directory in the task
directory:

```bash
mkdir -m 700 "$TASK/runtime"
VIS_RUNTIME_DIR="$TASK/runtime" HOME="$TASK/home" vis-agent "Fix the failing test"
```

The JVM property `-Dvis.runtime.dir` has the same effect and wins over the variable. The Python
SDK gives its environment, with this variable, to the engine that it starts. The directory must
meet these conditions:

- It is an absolute path to a directory that exists.
- The path has no symbolic link. On macOS, write `/private/tmp`, not `/tmp`.
- Only you can write to it.

If one condition is false, Vis does not start and names the cause. It never falls back to `/tmp`.

Vis puts these items in `VIS_RUNTIME_DIR`:

- `tmp`, a private directory with mode 700 for temporary files. Vis keeps it. Commands that Vis
  starts for a session get it as `TMPDIR`.
- A new private directory with mode 700 for each Python worker. Vis removes it after the worker
  connects, also when the start fails.

Other files in `VIS_RUNTIME_DIR` stay. Logs and other worker state stay under `HOME`. Host tools
such as `git` and `uv` keep the environment that you give Vis. Set `TMPDIR` for them yourself.

A socket path has at most 102 bytes. If the full path is longer, Vis uses a path relative to its
working directory. For a long path, start Vis in the directory that `-Duser.dir` names, or do not
set `-Duser.dir`. If the relative path is also too long, the worker does not start, so use a shorter
directory.

## Executables and the credential store

`jail.deny_exec` blocks named executables in jailed children:

```yaml
jail:
  enabled: true
  deny_exec: [curl, wget]
```

A blocked executable leaves other ways to do the same operation. To restrict the operation, use
filesystem and network policy.

By default, jailed children cannot use the credential store of the operating system. So `gh`,
`git` credential helpers, and similar tools get no credentials. To allow access, set:

```yaml
jail:
  enabled: true
  keychain: true
```

On macOS, this setting grants the Keychain services and read access to the system and user
keychain databases. On Linux, it exposes the session D-Bus, so that the Secret Service (GNOME
Keyring or KWallet) can respond.

## Diagnose the effective policy

1. Inspect `session["access"]`. Do not infer access from the YAML file alone.
   `session["access"]["filesystem"]["deny_read"]` and `["deny_write"]` list the effective deny
   rules. The map leaves out an empty rule list and a default network setting.
2. After a configuration edit, run `/reload`. Then send a message in each session that needs the
   new policy.
3. For an egress decision, run `/net-probe METHOD URL` or `/net-probe host:port`.
4. If startup reports a missing enforcer, check that the selected Python runtime has the
   `libvisjail` library of your platform.

Vis resolves paths and symlinks when it builds the session environment. Live workspace roots can
change in that policy snapshot. An edit of `vis.yml` cannot widen an existing environment until
`/reload`.

## See also

- For the complete `workspace`, `jail`, `environment`, and toggle keys, read
  [Configuration](configuration.md).
- For the permissions of the Python runtime, read [Python sandbox](python-sandbox.md).
- For client connections and authentication, read [Running a gateway](gateway-service.md).
