# Runtime distributions

For everyday use on a supported Linux or macOS host, install the prebuilt native
release. It does not need Java. The `dev` track runs from source on the JVM. Both
install `vis-agent`, which runs the engine and gateway, and `vis-tui`, the terminal
client that connects to it. The [desktop apps](#open-the-desktop-app) connect to
the same gateway from Windows, macOS or Linux.

## When to use

- **You are installing Vis and must choose a build.** Compare [native and
  JVM](#native-vs-jvm), then [install](#installing).
- **You want to upgrade Vis to a newer release.** See [Updating and selecting a
  track](#updating-and-selecting-a-track).
- **A fix you need is only in a beta.** [Switch to that
  version](#switching-between-versions), and back to the stable release afterwards.
- **You develop Vis or need the latest code from `main`.** Use the `dev` track, or
  [run on the JVM for one launch](#one-launch-jvm-override).
- **The gateway runs a different version than your terminal after an update.** See
  [How you learn that a newer version is
  running](#how-you-learn-that-a-newer-version-is-running).
- **You want the desktop app.** [Open the desktop app](#open-the-desktop-app) for
  your platform and release track.
- **A desktop fix you need is only in a beta.** Open the [beta desktop
  app](#beta-the-newest-published-beta), which stays separate from your stable app.
- **You want Vis in a browser.** [Open the web app](#open-the-web-app) that your gateway
  serves.

To add Java or Clojure code inside the engine, see [Native builds for Java and
Clojure extensions](jvm-native-image.md).

## Native vs JVM

Choose native for everyday use. Choose JVM when developing Vis itself or
running the latest code from `main`.

| What matters | Native | JVM |
| --- | --- | --- |
| Best for | Daily work and multiple sessions | Developing Vis and trying the latest changes |
| Version track | `release` (default) or `beta` | `dev` |
| Java / Git | Not needed | Git and JDK 25+ |
| Gateway startup | ~0.7 s | ~8.6 s |
| Gateway RAM | ~122 MiB | ~566 MiB |
| RAM per session (light use) | ~60 MiB | ~153 MiB |

*Performance figures: Vis v0.2.0 on Apple M4 Max.*

## Installing

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

By default the installer downloads the latest complete stable native release. It
puts the engine, bundled Python worker and matching TUI in `~/.local/bin` and adds
that directory to the shell profile if needed. Use `--install-dir PATH` to change it.
Native installation requires `curl` and `tar`, not Java or Git. Missing or incomplete
native packages fail without installing a JVM fallback.

The installer accepts the same tracks as update:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --track beta
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --track dev
```

The bootstrap scripts refresh after successful `main` CI, independently of stable
releases. They are published as GitHub release assets because some networks block
`raw.githubusercontent.com`. Native installations still use the matching launcher
from their selected release. Running `bin/install-vis-agent` from a clone also works.

## Updating and selecting a track

```bash
vis-agent update                          # release, always the default
vis-agent update --track release          # latest complete stable native release
vis-agent update --track beta             # latest complete native beta
vis-agent update --track dev              # newest main source, always JVM
```

| Track | Source | Execution |
|---|---|---|
| `release` | Latest complete stable GitHub release | Native engine, gateway, workers and TUI |
| `beta` | Published beta of a main commit that passed CI and native checks | Native engine, gateway, workers and TUI |
| `dev` | Newest commit of `main` | JVM engine, gateway, workers and TUI |

Every update without `--track` selects `release`, also after beta or dev. A successful update
records the selection for later launches. To stay on beta or dev, name that track in each update. A
failed download does not change the recorded selection. Dev ignores any native binaries from a
previous install.

`vis-agent update vX.Y.Z` installs a specific stable version on the release track.
Version pins are not accepted with beta or dev. Native updates acquire the engine,
Python worker and TUI from the same immutable release before replacing installed files.
A missing native TUI fails rather than starting a JVM client.

### How you learn that a newer version is running

The gateway is a daemon that keeps running after the command that started it ends. So the gateway
can already run a newer Vis than your client. For example, an open TUI can stay alive through an
update, or you can point a client at a gateway on another machine.

When the client and the gateway run different releases, Vis tells you in three places:

- The terminal client prints one line per run.
- The TUI shows a notice in its header.
- `vis-agent gateway status` names both releases.

Vis refuses nothing, because both halves still use the same wire protocol. To close the gap, run
`vis-agent update` on the machine that is behind.

## Switching between versions

`update` follows a track forward. `switch` names the build that you want to run. So you can move to
a published beta to try a fix, and then back to the stable release.

```bash
vis-agent switch list                     # installed build, tracks and published versions
vis-agent switch release                  # newest stable release
vis-agent switch beta                     # the beta the installer index selects today
vis-agent switch v0.2.8                   # that stable release
vis-agent switch beta-<commit>            # that published beta build
vis-agent switch dev                      # newest main source, always JVM
```

`switch list` first prints the installed version and track. Then it lists the tracks, the published
releases and the published betas, and marks the entry that the installed build came from. It needs
GitHub only for the published lists. If Vis cannot read them, you can still name a version.

A switch installs the bundle it names through the same installer `update` uses, and records
the selection, so later launches use it. Configuration, sessions and extensions are shared by
every version, so switching back costs only the download. `--keep-gateway` leaves a running
gateway alone, exactly as it does for `update`.

If managed source already exists in `~/.vis/install/src`, a native update also pins it to the exact
commit of the selected native build. This needs Git. Native-only installations do not download
source.

For native and dev updates, staged, unstaged or untracked changes stop the update. Vis then shows
the checkout path and tells you to inspect it with `git status`. Resolve conflicts and commit or
stash your local changes, including untracked files. Then retry with the same track options. Until
then, the source checkout and pin, the native installation and the selected track stay the same. The
updater does not replace source that has local changes with a fresh checkout.

Managed source is a detached pin, not a tracking branch. A manual pull needs an
explicit branch/ref. Rerunning the updater after resolving local changes selects
the correct pin: the exact native build commit for release/beta, or newest main
for dev. A source fetch failure also leaves the installed versions and track unchanged.

By default an update releases an idle managed gateway using its old executable
before replacing it. Busy or user-owned gateways are never stopped. Add
`--keep-gateway` to leave even an idle gateway running.

Dev needs Git and JDK 25+. The launcher can install the Clojure CLI.
`VIS_NO_AUTO_INSTALL=1` disables that installation. Dev does not build native images.
To run local repository edits independently of installed tracks, use
`clojure -M:vis` inside that checkout. Build those edits with `clojure -T:build native`.

### Older launchers that reject dev

If `vis-agent update --track dev` reports that dev is not a distribution track,
your installed launcher predates the dev selector. `--dev` is not an update option.
Rerun the current bootstrap with dev selected. A plain update reinstalls the stable
release's launcher and may still lack the selector:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --track dev
```

Use `--install-dir PATH` if Vis was installed somewhere other than `~/.local/bin`.
Dev requires Git and JDK 25+. This replaces the launcher and installs main source.
It does not erase your sessions or configuration.

## One-launch JVM override

Use `--jvm` to run on the JVM without changing the installed track:

```bash
vis-agent tui --jvm
# Equivalent:
vis-agent --jvm tui

# Start a foreground gateway on the JVM:
vis-agent gateway start --jvm
# Equivalent:
vis-agent --jvm gateway start
```

The flag applies to this launch only and is consumed by the launcher, not the
engine or terminal client. It uses the managed source in `~/.vis/install/src`. If
no managed source exists, a checkout-owned launcher uses its own source tree:
`./bin/vis-agent tui --jvm`. An installed launcher with no source reports how to
install it with `vis-agent update --track dev`. It does not download source or
change tracks automatically. JVM execution requires JDK 25+.

For gateway inspection and control commands, `--jvm` selects the local command's
runtime. It does not change the runtime of an already-running or remote daemon.

`--jvm` is not an update option. Use `--track dev` to install or update JVM source.
Arguments after `--` or `python uv` are passed through unchanged.

## Terminal gateway lifecycle

`vis-agent tui` discovers the local gateway and starts one when none is running.
A native installation starts a native gateway. Dev or an explicit `--jvm` launch
starts it with the same JVM and engine classpath. A compatible gateway already
serving other clients is reused, never killed to change its runtime.

You do not need to start a gateway before you run `vis-agent tui`. A start can take a while, most of
all with `--jvm`, which runs Vis from source. `vis-agent tui` waits while the new gateway starts. If
the start takes longer than 15 seconds, it prints the path of the gateway's boot log under
`~/.vis/logs/`. If the gateway exits before it is ready, or is not ready after 10 minutes,
`vis-agent tui` stops. It then shows the last lines of that boot log, so you can see what went
wrong.

The launcher holds a client lease until the TUI exits. The TUI also registers its
local PID so crashed clients and their event streams can be reaped. A managed
gateway stops after its last client disconnects and no work remains. Closing one
TUI does not stop another TUI, the companion, or an active turn. Manually started
gateways remain user-owned.

`--gateway` or `VIS_GATEWAY_URL` selects an explicit gateway: the TUI only connects
to it and never starts or stops a local replacement. If your shell profile sets
`VIS_GATEWAY_URL`, unset it to let `vis-agent tui` start a local gateway again.
Help and version commands do not start a gateway. Direct `vis-tui` execution
remains a connection-only client. Use `vis-agent tui` for automatic local
lifecycle management.

## Open the web app

The web app is the Companion app, served by your own gateway. Use it to open Vis in a browser
without a desktop app. It works on the computer where Vis runs and on other devices on your network.

```bash
vis-agent web                  # use or start the gateway on 127.0.0.1:7890 and open the app
vis-agent web --port 8080      # use or start the gateway on port 8080 instead
vis-agent web --host 0.0.0.0   # also serve other devices on your network
vis-agent web --no-open        # print the address without opening a browser
```

The command prints the address it opens. Keep it running while you use the app, and
press Ctrl-C when you are done. If the gateway stops answering, the command ends and
tells you. `vis-agent gateway start` serves the web app too, and prints its address when
the web app is installed.

### Choose the gateway address

`--host` and `--port` name the gateway the web app uses. They default to `127.0.0.1`
and `7890`.

- If your gateway already answers at that address, the web app uses it.
- If no gateway is running, `vis-agent web` starts one there.
- If your gateway runs at another address and nothing uses it, `vis-agent web` restarts it at the
  new address. Some gateways stay where they are: a gateway that another Vis session uses, or one
  that you started with `vis-agent gateway start`. For those, the command tells you where the
  gateway runs and how to open the web app there.
- If the host is another machine, or another gateway already answers at that address,
  `vis-agent web` opens the web app of that gateway. Set `VIS_GATEWAY_TOKEN` when that
  gateway requires a token.

A gateway bound to `0.0.0.0` or to a network address always requires its token, also in
your own browser. The web app asks for it, and `vis-agent gateway pair` shows it. The
command also prints the addresses that other devices can open.

### Get the web app

Release and beta builds publish the web app with the native runtime. `vis-agent update` installs it
next to the runtime, so you do not need Node.js. An installation can have no web app, for example
because an older `vis-agent` ran the update. Then the first `vis-agent web` downloads the copy
published with the installed build. A build published before the web app existed has no copy.
Install a newer build with `vis-agent update` or `vis-agent update --track beta`.

The dev track runs Vis from source. There, the first `vis-agent web` builds the web app with Node.js
and npm. Later runs rebuild it only after the Companion sources change. A gateway that is already
running serves the new build without a restart. To serve a different build, set `VIS_WEB_DIR` before
the gateway starts. Point it to a directory that contains the build's `index.html`.

<a id="windows-app"></a>

## Open the desktop app

1. [Download the installer from GitHub Releases](https://github.com/Blockether/vis/releases/latest)
   for your computer.
2. Install the package and open **Vis** from your application launcher.
3. [Connect to your gateway](index.md#connect-an-app) to open your
   projects and continue your sessions.

| Platform | Package | Install |
| --- | --- | --- |
| Windows 10/11 x64 | `.msi` | Run the installer. |
| macOS, Apple silicon or Intel | Universal `.dmg` | Open the disk image and copy Vis to Applications. |
| Linux x64 or ARM64 | `.AppImage` or `.deb` | Make the AppImage executable and open it, or install the Debian package with your package manager. |

The Windows installer downloads Microsoft Edge WebView2 if needed. Installation
may require network access and administrator approval. To update the app, close
Vis and install the newer package from the same release page.

Each published beta has the same installers. A desktop fix can reach a beta before a stable release.
To try it, download the installers from that beta's prerelease on the [releases
page](https://github.com/Blockether/vis/releases). A beta package installs as the same Vis app as
the stable package. To keep your stable app, use
[`vis-agent desktop --track beta`](#beta-the-newest-published-beta) on macOS or Linux instead.

Run the gateway on the computer where your projects live: macOS, Linux, or [Linux in
WSL2](https://learn.microsoft.com/en-us/windows/wsl/install) on Windows. For a local WSL2 gateway,
enable localhost forwarding. The [connection guide](index.md#connect-an-app) covers
both local addresses and remote pairing. [Isolated drafts](drafts.md) have more filesystem
requirements.

### Notifications on the desktop

The desktop app raises a system alert when a session on a connected machine answers you or asks
you a question. The alert carries the session's name and what Vis said, the same as the alert on
your phone. Turn it on for each machine in Settings, under Notifications: pick the machine and
switch its notifications on. macOS asks for permission the first time, and System Settings, under
Notifications, controls how those alerts appear.

Alerts arrive while the desktop app is open. It cannot receive push notifications, so nothing
reaches it after you quit. To hear about a session while the desktop app is closed, keep a
connection to that machine open. Use the Companion app on your phone or a browser tab.

### Microphone and camera on the desktop

Dictation uses the microphone and pairing by QR code uses the camera, so macOS asks for permission
the first time you use each one. Allow it once and the desktop app keeps that access. To change it
later, open System Settings, under Privacy & Security, and look at Microphone and Camera.

Earlier desktop builds could not ask at all and refused dictation with a message about the request
not being allowed by the user agent. Update to the current release if you still see that.

### macOS and Linux launcher

Open the desktop Companion for your selected release track:

```bash
vis-agent desktop                  # use the track selected by vis-agent update
vis-agent desktop --update         # check for a release or beta update, or rebuild dev
vis-agent desktop --track release  # download and open stable for this launch
vis-agent desktop --track beta     # download and open the newest beta for this launch
vis-agent desktop --track dev      # build and open your current source checkout
vis-agent desktop --no-gateway     # open the app without the local gateway
vis-agent desktop --help
```

`--track` overrides the desktop track for one launch. It does not change your
engine selection. With no installed track, a source-only checkout defaults to dev.
Otherwise the default is release.

### Release: download and reuse

On macOS and Linux, the release track chooses the universal macOS app (Apple
silicon or Intel), or the Linux AppImage for x64 or ARM64. Downloads need `curl`
and network access. On macOS, the launcher copies `Vis.app` from the signed
disk image into your Vis cache. On Linux, it runs the AppImage with built-in
extraction, so FUSE is not required. You still need a graphical
desktop and the system libraries required by the app.

Later release launches reuse the cached app and do not contact GitHub. `vis-agent update` updates an
installed release app together with the engine. So the desktop app that you open matches the runtime
that you installed. It never installs an app that you do not have.

`--update` checks for a newer stable version and downloads it only when it is not cached. If a
download or an installation step fails, the app that you selected before stays as it was. An engine
update still succeeds. Retry the command, or omit `--update` to open the existing copy. If a cached
executable is missing, Vis downloads it again automatically.

### Beta: the newest published beta

The beta track downloads the desktop app from the newest published beta, the same
build that `vis-agent update --track beta` installs. It otherwise works like release:
later launches reuse the cached app, `--update` checks for a newer beta, and
`vis-agent update --track beta` refreshes an installed beta app. Beta apps are kept
apart from stable ones, so opening one track never removes the other's app.

If the newest beta has no app for your platform, the launcher says so and keeps any cached beta app.
Open `--track release` instead.

### Dev: build from source

On macOS and Linux, the dev track builds the web bundle and native desktop app
from your selected source on **every launch**, including uncommitted changes. It uses the managed
checkout at `~/.vis/install/src` when present. Otherwise, a checkout-owned
`bin/vis-agent` builds that checkout. It never downloads a released desktop app or
falls back to an older build after a failure.

Before you run it, install these tools:

- Node.js 20 or newer, with npm.
- Rust 1.85 or newer, with Cargo.
- The native build tools. macOS needs Xcode and its command-line tools. Linux needs a C/C++
  toolchain, WebKitGTK 4.1 development packages and `xdg-utils`.

The [desktop build workflow](https://github.com/Blockether/vis/blob/main/.github/workflows/desktop-companion.yml)
lists the Ubuntu packages. Some of these installs need administrator access. Dependency downloads
need network access, and the first native build can take several minutes. Later builds reuse the
npm and Rust download and build caches, but they still run the build steps.

The launcher runs `npm ci`, `npm run build`, and `npm run package:desktop -- --dev`
in `apps/vis-companion`. This reinstalls that checkout's `node_modules` and updates
its build outputs. The result targets only your machine's architecture, needs no
release signing credentials, and has a separate app identity from stable.
`desktop --update` also builds the **current** checkout. To fetch newer source
first, run `vis-agent update --track dev`.

### Cached files and pairing

Release files live in `~/.vis/install/desktop/<platform>/<version>/`, beta files in
`~/.vis/install/desktop/beta/<platform>/<version>-beta.<commit>/`, and source builds
in `~/.vis/install/desktop/dev/<platform>/<version>.<build>/`. All of them respect
`VIS_HOME`, and installing an app there needs no administrator access.

Only one desktop app runs at a time. A launch closes a desktop running from another
version or track, then opens the version you selected. If that version is already
running, its window comes to the front.

The launch then deletes the other versions in that track's folder, leaving one app
on disk. The other tracks keep their files. Release and beta apps deleted this way
download again when you need them, and dev builds are rebuilt from source on every
launch.

A desktop launch does not change your engine track. If no local gateway runs, it starts one
in the background through your installed engine. The gateway stays up while the app runs.
After you quit the app, a gateway started this way stops when no other client or work
remains. A gateway that you started with `vis-agent gateway start` keeps running.

`--no-gateway` opens the app alone. `--gateway` or `VIS_GATEWAY_URL` names another gateway, so
a launch with either one also opens the app alone. The TUI and `vis-agent web` follow the
same rule. A launch without an installed engine also opens the app alone.

On first launch, add this machine in the app with `http://127.0.0.1:7890` and an empty
bearer token. For a gateway on another machine, use its URL and bearer token.
See [Desktop and mobile setup](index.md#connect-an-app) for connection options.

## Automatic native betas

The Beta Native workflow starts after successful push CI on `main`. It verifies
that CI passed for the exact commit in this repository and selects the latest
successful main run. A newer successful run cancels an older beta build. New
commits whose CI is pending or failed do not block publication of a green beta.

Beta uses the same native build and test workflow as stable releases on Linux x86-64, Linux ARM64
and macOS ARM64. It also uses the same desktop packaging for macOS, Windows and Linux. Each build
has an immutable `beta-<commit>` tag. Its prerelease stays a draft until two conditions are true:

- All native tests, SDK checks, TUI checks and desktop packaging pass.
- The six engine and TUI archives, the web app and the six desktop installers are present and not
  empty.

A failed or cancelled build cannot replace the last published beta. A beta never becomes the latest
stable release on GitHub, and it does not publish mobile apps.

## Files

| Path | Contents |
|---|---|
| `~/.local/bin/vis-agent-native` | Installed native engine |
| `~/.local/bin/vis-agent-python/` | Its bundled Python worker and interpreter |
| `~/.local/bin/vis-tui` | Matching native terminal client |
| `~/.local/bin/vis-web/` | Web app that the gateway serves |
| `~/.vis/install/desktop/` | Desktop apps by platform and version, with beta apps in `beta/` and dev builds in `dev/` |
| `~/.vis/install/track` | Selection for later launches |
| `~/.vis/install/src` | Managed dev checkout at a detached main commit |
| `~/.vis/install/ref` | Commit pinned by the last dev update |
| `~/.vis/install/vis-agent-native` | Native engine for a checkout-owned launcher |

Each native engine has a `vis-agent-native.build` file beside it, containing its
version, commit, build track and timestamp. `vis-agent --version` reports the version.
`VIS_HOME` changes the state directory from `~/.vis`. `vis-agent --measure` prints
startup timings. `--jfr` saves Java Flight Recorder profiles.

## Native bundles

```text
vis-agent-<os>-<arch>.tar.gz
├── vis-agent
├── vis-agent-native
├── vis-agent-native.build
├── vis-agent-python/
└── install-vis-agent

vis-tui-<os>-<arch>.tar.gz
└── vis-tui
```

To build an image, you need the GraalVM CE version in `.graalvm-version` and about 32 GB of RAM.
`bin/release-native` builds and smoke-tests the assets that the host supports. On Apple silicon, it
builds macOS natively, Linux ARM64 in a container and Linux x86-64 through Rosetta.

Enable Rosetta in Docker Desktop or podman, because the build rejects qemu.
`VIS_CONTAINER_CONNECTION` and `VIS_CONTAINER_CLI` select the container machine and engine. On
platforms without native bundles, use the dev track.

## See also

- [Native builds for JVM extensions](jvm-native-image.md) — only for adding Java/Clojure capabilities inside Vis.
- [Getting started](index.md#connect-an-app) — download a client and connect to your sessions.
- [Configuration](configuration.md)
