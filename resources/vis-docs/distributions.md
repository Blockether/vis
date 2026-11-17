# Runtime distributions

For everyday use on a supported Linux or macOS host, install the prebuilt native release. It needs
no Java. The `dev` track runs from source on the JVM. Both install `vis-agent`, which runs the
engine and the gateway, and `vis-tui`, the terminal client. The
[desktop apps](#open-the-desktop-app) connect to the same gateway from Windows, macOS, or Linux.

## When to use

- **You install Vis and must choose a build.** Compare [native and JVM](#native-vs-jvm), then
  [install](#installing).
- **You want to upgrade Vis.** Read
  [Updating and selecting a track](#updating-and-selecting-a-track).
- **A fix that you need is only in a beta.** [Switch to that version](#switching-between-versions),
  and later back to the stable release.
- **You develop Vis or need the latest code from `main`.** Use the `dev` track, or
  [run on the JVM for one launch](#one-launch-jvm-override).
- **After an update, the gateway and your terminal run different versions.** Read
  [How you learn that a newer version is running](#how-you-learn-that-a-newer-version-is-running).
- **You want the desktop app.** [Open the desktop app](#open-the-desktop-app).
- **CI or a team must stay on a tested version.** [Pin an exact version](#pin-an-exact-version)
  and [verify what you download](#verify-what-you-download).
- **A desktop fix that you need is only in a beta.** Open the
  [beta desktop app](#beta-the-newest-published-beta).
- **You want Vis in a browser.** [Open the web app](#open-the-web-app) that your gateway serves.
- **You want to remove Vis.** Read [Removing Vis](#removing-vis).

To add Java or Clojure code to the engine, read
[Native builds for Java and Clojure extensions](jvm-native-image.md).

## Native vs JVM

| What matters | Native | JVM |
| --- | --- | --- |
| Best for | Daily work and many sessions | Development of Vis and the latest changes |
| Version track | `release` (default) or `beta` | `dev` |
| Java and Git | Not needed | Git and JDK 25+ |
| Gateway startup | About 0.7 s | About 8.6 s |
| Gateway RAM | About 122 MiB | About 566 MiB |
| RAM for each session (light use) | About 60 MiB | About 153 MiB |

The figures are from an Apple M4 Max.

## Installing

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

The installer downloads the latest complete stable native release. It puts the engine, the bundled
Python worker, and the matching TUI in `~/.local/bin`, and adds that directory to your shell
profile if needed. `--install-dir PATH` changes the directory. A native installation needs `curl`
and `tar`, not Java or Git. A missing or incomplete native package fails the installation, with no
JVM fallback.

The installer accepts the same tracks as `vis-agent update`:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --track beta
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --track dev
```

The bootstrap scripts are GitHub release assets, not files on `raw.githubusercontent.com`. They
refresh after each successful CI run on `main`. A native installation still uses the launcher of
its selected release. `bin/install-vis-agent` from a clone also works.

### Pin an exact version

`--version` installs one tested release or beta, never a newer build.

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --version 0.2.46
# The same with an environment variable:
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | VIS_AGENT_VERSION=0.2.46 bash
```

Write `0.2.46` or `v0.2.46`. The installer takes the `vis-agent` command from that release and
runs `vis-agent update v0.2.46`. If the release does not exist, or has no bundle for your
platform, the installation stops. A stable version selects the release track, so you cannot add
`--track beta` or `--track dev`.

To pin a tested beta, give its full tag: `beta-` and the 40-character commit.
`vis-agent switch list` shows the published beta tags.

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --version beta-<commit>
```

A beta tag selects the beta track, so you cannot add `--track release` or `--track dev`. The
installer runs `vis-agent update beta-<commit>`. The update takes the engine, Python, the TUI, and
the web app from that beta. The installation stops if the beta does not exist, has no `SHA256SUMS`,
or lacks a selected part. It never falls back to the latest beta, and the installed build and track
stay the same.

### Choose what to install

By default, the installer adds the terminal client (TUI) and the web app. If you use only the
`vis-agent` command line, for example on a server or in CI, install less with `--components`:

```bash
# Only the command line, the engine and Python:
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --components none
# The TUI, without the web app:
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash -s -- --components tui
# Everything, and the desktop app now:
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | VIS_COMPONENTS=all bash
```

| Component | What it adds |
| --- | --- |
| `tui` | The terminal client for `vis-agent tui`. |
| `web` | The web app for `vis-agent web`. |
| `desktop` | The desktop app, downloaded during the installation. |

The installer always installs the engine and the Python runtime. Give `none`, `all`, or a list
separated by commas. Vis saves your selection, and `vis-agent update` keeps it. To change it
later, run `vis-agent update --components tui,web`. An update removes the parts that you do not
select. Without `desktop`, `vis-agent desktop` still downloads the desktop app on its first run.

### Verify what you download

Each release publishes `SHA256SUMS`, with the SHA-256 hash of each of its assets. The installer
checks the `vis-agent` command against this list before it runs the command. `vis-agent update`
checks each archive before it unpacks the archive. A changed file or a missing entry stops the
installation, and the installed version stays. Older releases have no `SHA256SUMS`, so Vis
installs them with a warning.

`SHA256SUMS` comes from the same place as the assets. In CI, keep the hashes in your repository,
so that a replaced asset cannot pass. Pin the installer with `sha256sum --check`, and pin the
`vis-agent` command with `--sha256` or `VIS_AGENT_SHA256`:

```bash
tag=v0.2.46
base=https://github.com/Blockether/vis/releases/download/$tag
curl -fsSLO "$base/install-vis-agent"
echo "<installer hash>  install-vis-agent" | sha256sum --check
bash install-vis-agent --version "$tag" --sha256 "<vis-agent hash>"
```

With a stable `--version` or a beta tag, the command comes from that release, so its hash does not
change. The `SHA256SUMS` of the release lists the command. Older betas have no separate command.
For them, the installer uses the command of the rolling installer assets, and that hash changes
when the assets refresh. A different command stops the installation before it runs.

To check release assets by hand, download them and `SHA256SUMS` to one directory:

```bash
tag=v0.2.46
base=https://github.com/Blockether/vis/releases/download/$tag
curl -fsSLO "$base/SHA256SUMS"
curl -fsSLO "$base/vis-agent"
sha256sum --check --ignore-missing SHA256SUMS
```

The expected output is `vis-agent: OK`. On macOS, use
`shasum -a 256 --check --ignore-missing SHA256SUMS`. To get the hash to pin, run
`sha256sum vis-agent`.

## Updating and selecting a track

```bash
vis-agent update                          # release, always the default
vis-agent update --track release          # latest complete stable native release
vis-agent update --track beta             # latest complete native beta
vis-agent update beta-<commit>            # that published beta build
vis-agent update --track dev              # newest main source, always JVM
```

| Track | Source | Execution |
|---|---|---|
| `release` | Latest complete stable GitHub release | Native engine, gateway, workers, and TUI |
| `beta` | Published beta of a `main` commit that passed CI and the native checks | Native engine, gateway, workers, and TUI |
| `dev` | Newest commit of `main` | JVM engine, gateway, workers, and TUI |

An update without `--track` or a beta tag selects `release`, also after beta or dev. To stay on
beta or dev, name that track in each update. A successful update records the selection for later
launches. A failed download does not change it. Dev ignores the native binaries of an earlier
installation.

`vis-agent update vX.Y.Z` installs that stable version on the release track.
`vis-agent update beta-<commit>` installs that published beta on the beta track. A missing,
incomplete, or unverified beta stops the update. Dev accepts no version pin.

A native update gets the engine, the Python worker, and the TUI from the same immutable release
before it replaces installed files. A missing native TUI fails. The update does not start a JVM
client in its place.

An update stops an idle managed gateway before it replaces the files. It never stops a busy
gateway or a gateway that you started yourself. `--keep-gateway` also leaves an idle gateway
running.

### Managed source

If managed source exists in `~/.vis/install/src`, a native update also pins it to the exact commit
of the selected native build. This step needs Git. A native-only installation downloads no source.
Managed source is a detached commit, not a tracking branch. For a manual pull, name a branch.

If that checkout has staged, unstaged, or untracked changes, the update stops and shows the
checkout path. Nothing changes: not the source, not the native installation, and not the selected
track. Commit or stash your changes, then run the same command again. A failed source fetch also
changes nothing.

Dev needs Git and JDK 25+. The launcher can install the Clojure CLI, and `VIS_NO_AUTO_INSTALL=1`
prevents that installation. To run local repository edits independently of the installed tracks,
use `clojure -M:vis` in that checkout. To build those edits, use `clojure -T:build native`.

### How you learn that a newer version is running

The gateway can run a newer Vis than your client. An open TUI can stay alive through an update, and
a client can connect to a gateway on a different machine.

When the client and the gateway run different releases, Vis tells you in three places:

- The terminal client prints one line in each run.
- The TUI shows a notice in its header.
- `vis-agent gateway status` names both releases.

Vis refuses nothing. To remove the difference, run `vis-agent update` on the machine with the older
release.

## Switching between versions

`update` follows a track forward. `switch` names the build that you want to run.

```bash
vis-agent switch list                     # installed build, tracks and published versions
vis-agent switch release                  # newest stable release
vis-agent switch beta                     # the beta the installer index selects today
vis-agent switch v0.2.46                  # that stable release
vis-agent switch beta-<commit>            # that published beta build
vis-agent switch dev                      # newest main source, always JVM
```

`switch list` prints the installed version and track. Then it lists the tracks, the published
releases, and the published betas, and marks the entry of the installed build. It needs GitHub
only for the published lists. If Vis cannot read them, you can still name a version.

A switch records the selection for later launches. All versions share configuration, sessions, and
extensions, so a switch back costs only the download. `--keep-gateway` works as it does for
`update`.

## One-launch JVM override

Use `--jvm` to run on the JVM without a change to the installed track:

```bash
vis-agent tui --jvm
# Equivalent:
vis-agent --jvm tui

# Start a foreground gateway on the JVM:
vis-agent gateway start --jvm
# Equivalent:
vis-agent --jvm gateway start
```

JVM execution needs JDK 25+. The launcher selects the source in this order:

1. The managed source in `~/.vis/install/src`.
2. For a launcher in a checkout, its own source tree: `./bin/vis-agent tui --jvm`.

An installed launcher with no source tells you to install it with `vis-agent update --track dev`.
It does not download source or change tracks itself. `--jvm` is not an update option.

For the gateway inspection and control commands, `--jvm` selects the runtime of the local command.
It does not change the runtime of a running or remote daemon. Vis passes the arguments after `--`
or `python uv` through unchanged.

## Terminal gateway lifecycle

`vis-agent tui` finds the local gateway and starts one when none runs. A native installation starts
a native gateway. Dev or a `--jvm` launch starts a JVM gateway. If a compatible gateway already
serves other clients, the TUI uses it. Vis never stops such a gateway to change its runtime.

`vis-agent tui` waits for the gateway start, which is longest with `--jvm`. After
15 seconds, it prints the path of the gateway boot log under `~/.vis/logs/`. If the gateway exits
before it is ready, or is not ready after 10 minutes, `vis-agent tui` stops. It then shows the
last lines of that boot log.

A managed gateway stops after its last client disconnects and no work remains. To close one TUI
does not stop a second TUI, the Companion app, or an active turn. A gateway that you started by
hand stays yours.

`--gateway` or `VIS_GATEWAY_URL` selects an explicit gateway. The TUI then only connects to it,
and never starts or stops a local gateway. If your shell profile sets `VIS_GATEWAY_URL`, unset it
to let `vis-agent tui` start a local gateway again. Help and version commands start no gateway.
`vis-tui`, run directly, only connects. For automatic local lifecycle management, use
`vis-agent tui`.

## Open the web app

The web app is the Companion app, served by your own gateway. It opens Vis in a browser, on the
computer that runs Vis and on other devices on your network.

```bash
vis-agent web                  # use or start the gateway on 127.0.0.1:7890 and open the app
vis-agent web --port 8080      # use or start the gateway on port 8080 instead
vis-agent web --host 0.0.0.0   # also serve other devices on your network
vis-agent web --no-open        # print the address without opening a browser
vis-agent web --pair           # also print a pairing QR for the Companion app on your phone
```

The command prints the address that it opens. Keep the command running while you use the app, and
press Ctrl-C to end it. If the gateway stops answering, the command ends and tells you.
`vis-agent gateway start` also serves the web app, and prints its address when the web app is
installed.

The printed address connects the browser to this gateway, and you type no token. The address
works one time, for 5 minutes, and does not hold the gateway token. For an old or used address,
the web app shows an error and the connection screen. Run `vis-agent web` again to get a new
address.

When the gateway gives no launch code, the command prints the plain address and the cause. The
gateway did not accept the token, or it is too old to give a code. Open the address and connect
on the connection screen. If the app asks for a token, type the token of that gateway.

With `--no-open`, open the printed address yourself, or give it to a script. A script can replace
the host with `localhost` and keep the rest of the address.

`--pair` prints the same pairing block as `vis-agent gateway start --pair`: the link, the token,
and a QR code. Without `--host`, it starts the gateway on an address that your phone can reach.
`--advertise` sets the address in the pairing link, as it does for `vis-agent gateway start`.

Voice input works only on a secure page: HTTPS, or `localhost` and `127.0.0.1` on the computer
that runs Vis. Browsers block the microphone on other `http://` addresses. For example, a Windows
browser that opens the address of Vis in WSL cannot record. To allow such an address in Chrome:

1. Open `chrome://flags/#unsafely-treat-insecure-origin-as-secure`.
2. Add the address of the web app, for example `http://172.20.1.2:7890`.
3. Set the flag to **Enabled** and start Chrome again.

### Choose the gateway address

`--host` and `--port` name the gateway that the web app uses. They default to `127.0.0.1` and
`7890`.

- If your gateway answers at that address, the web app uses it.
- If no gateway runs, `vis-agent web` starts one there.
- If your gateway runs at a different address and nothing uses it, `vis-agent web` starts it again
  at the new address. Two kinds of gateway stay where they are: one that a second Vis session
  uses, and one that you started with `vis-agent gateway start`. For those, the command tells you
  where the gateway runs and how to open the web app there.
- If the host is a different machine, or a different gateway answers at that address,
  `vis-agent web` opens the web app of that gateway. For a gateway on this computer, the command
  uses the token of this Vis home. For a different machine, set `VIS_GATEWAY_TOKEN` when the
  gateway requires a token.

A gateway that binds to `0.0.0.0` or to a network address always requires its token, also in your
own browser. The address that `vis-agent web` prints gives the token to that browser. On other
devices, the web app asks for the token, and `vis-agent gateway pair` shows it. The command also
prints the addresses that other devices can open.

### Get the web app

Release and beta builds publish the web app with the native runtime. `vis-agent update` installs
it next to the runtime, so you need no Node.js. If the web app is missing, the first
`vis-agent web` downloads the copy of your installed build.

The dev track runs Vis from source. There, the first `vis-agent web` builds the web app with
Node.js and npm. Later runs build it again only after the Companion sources change. A running
gateway serves the new build without a restart. To serve a different build, set `VIS_WEB_DIR`
before the gateway starts. Give it the directory that holds the `index.html` of the build.

To give the web app your own name, logo, and theme, read [White label](white-label.md).

<a id="windows-app"></a>

## Open the desktop app

1. [Download the installer from GitHub Releases](https://github.com/Blockether/vis/releases/latest)
   for your computer.
2. Install the package and open **Vis** from your application launcher.
3. [Connect to your gateway](index.md#connect-an-app) to open your projects and continue your
   sessions.

| Platform | Package | Install |
| --- | --- | --- |
| Windows 10 or 11 x64 | `.msi` | Run the installer. |
| macOS, Apple silicon or Intel | Universal `.dmg` | Open the disk image and copy Vis to Applications. |
| Linux x64 or ARM64 | `.AppImage` or `.deb` | Make the AppImage executable and open it, or install the Debian package with your package manager. |

The Windows installer downloads Microsoft Edge WebView2 if needed. The installation can need
network access and administrator approval. To update the app, close Vis and install the newer
package from the same release page.

Each published beta has the same installers. Download them from the prerelease of that beta on the
[releases page](https://github.com/Blockether/vis/releases). A beta package installs as the same
Vis app as the stable package. To keep your stable app on macOS or Linux, use
[`vis-agent desktop --track beta`](#beta-the-newest-published-beta).

Run the gateway on the computer that holds your projects: macOS, Linux, or
[Linux in WSL2](https://learn.microsoft.com/en-us/windows/wsl/install) on Windows. For a local
WSL2 gateway, enable localhost forwarding. The [connection guide](index.md#connect-an-app) covers
local addresses and remote pairing.

### Notifications on the desktop

The desktop app shows a system alert when a session on a connected machine answers you or asks you
a question. The alert has the name of the session and the text from Vis. Turn alerts on for each
machine in **Settings** > **Notifications**. On macOS, the
system asks for permission the first time. **System Settings** > **Notifications** controls how
the alerts appear.

Alerts arrive only while the desktop app is open. To get alerts while the desktop app is closed,
keep a different connection to that machine open. Use the Companion app on your phone or a browser
tab.

### Microphone and camera on the desktop

Dictation uses the microphone, and pairing by QR code uses the camera. macOS asks for permission
the first time that you use each one, and the desktop app keeps that access. To change it, open
**System Settings** > **Privacy & Security**, then **Microphone** or **Camera**.

### macOS and Linux launcher

Open the desktop Companion app of your selected release track:

```bash
vis-agent desktop                  # use the track selected by vis-agent update
vis-agent desktop --update         # check for a release or beta update, or rebuild dev
vis-agent desktop --track release  # download and open stable for this launch
vis-agent desktop --track beta     # download and open the newest beta for this launch
vis-agent desktop --track dev      # build and open your current source checkout
vis-agent desktop --no-gateway     # open the app without the local gateway
vis-agent desktop --help
```

`--track` overrides the desktop track for one launch. It does not change your engine selection.
With no installed track, a source-only checkout defaults to dev. In all other cases, the default
is release.

On macOS, the launcher also copies the app to `/Applications` when you can write there. Launchpad,
Spotlight, and the Dock then open the version that the launcher installed. Each track has its own
copy: `Vis.app`, `Vis Beta.app`, or `Vis Dev.app`. Each installation, update, or dev build
refreshes the copy of its track.

The launcher never replaces an app of that name from a different publisher. Before it opens the
app, it closes a copy that runs from `/Applications`. `VIS_APPLICATIONS_DIR` selects a different
folder, and an empty value turns the copy off.

### Release: download and reuse

The release track selects the universal macOS app (Apple silicon or Intel), or the Linux AppImage
for x64 or ARM64. A download needs `curl` and network access. On macOS, the launcher copies
`Vis.app` from the signed disk image into your Vis cache. On Linux, it runs the AppImage with
built-in extraction, so you need no FUSE. You still need a graphical desktop and the system
libraries of the app.

Later release launches use the cached app and do not contact GitHub. `vis-agent update` updates an
installed release app together with the engine. It never installs an app that you do not have.

`--update` checks for a newer stable version and downloads it only when it is not in the cache. If
a download or an installation step fails, the app that you had stays as it was, and an engine
update still succeeds. Run the command again, or omit `--update` to open the existing copy. If a
cached executable is missing, Vis downloads it again.

### Beta: the newest published beta

The beta track downloads the desktop app of the newest published beta, the build that
`vis-agent update --track beta` installs. The rest works as on the release track. Later launches
use the cached app, `--update` checks for a newer beta, and `vis-agent update --track beta`
refreshes an installed beta app. Vis keeps beta apps apart from stable apps, so one track never
removes the app of the other.

If the newest beta has no app for your platform, the launcher says so and keeps a cached beta app.
Open `--track release` then.

### Dev: build from source

The dev track builds the web bundle and the native desktop app from your selected source on each
launch, uncommitted changes included. It uses the managed checkout at `~/.vis/install/src` when
that exists. If not, a `bin/vis-agent` in a checkout builds that checkout. The dev track never
downloads a released desktop app, and never opens an older build after a failure.

Before you run it, install these tools:

- Node.js 20 or newer, with npm.
- Rust 1.85 or newer, with Cargo.
- The native build tools. macOS needs Xcode and its command-line tools. Linux needs a C and C++
  toolchain, the WebKitGTK 4.1 development packages, and `xdg-utils`.

The [desktop build workflow](https://github.com/Blockether/vis/blob/main/.github/workflows/desktop-companion.yml)
lists the Ubuntu packages. Some of these installations need administrator access. Dependency
downloads need network access, and the first native build can take many minutes. Later builds
reuse the npm and Rust caches, but they still run the build steps.

The launcher runs `npm ci`, `npm run build`, and `npm run package:desktop -- --dev` in
`apps/vis-companion`. These commands install the `node_modules` of that checkout again and update
its build outputs. The result is for the architecture of your machine only. It needs no release
signing credentials, and its app identity is separate from the stable app. `desktop --update` also
builds the current checkout. To get newer source first, run `vis-agent update --track dev`.

### Cached files and pairing

| Track | Folder |
| --- | --- |
| Release | `~/.vis/install/desktop/<platform>/<version>/` |
| Beta | `~/.vis/install/desktop/beta/<platform>/<version>-beta.<commit>/` |
| Dev | `~/.vis/install/desktop/dev/<platform>/<version>.<build>/` |

Each folder respects `VIS_HOME`. An installation there needs no administrator access.

Only one desktop app runs at a time. A launch closes a desktop app of a different version or
track, then opens the version that you selected. If that version already runs, its window comes
to the front.

The launch then deletes the other versions in the folder of that track, so one app stays on disk.
The other tracks keep their files. A deleted release or beta app downloads again when you need it.

If no local gateway runs, the launch starts one in the background through your installed engine.
That gateway runs while the app runs. After
you quit the app, it stops when no other client or work remains. A gateway that you started with
`vis-agent gateway start` keeps running.

The launch opens the app alone, without a local gateway, in three cases: with `--no-gateway`, with
`--gateway` or `VIS_GATEWAY_URL`, and without an installed engine. `--gateway` and
`VIS_GATEWAY_URL` name a different gateway. The TUI and `vis-agent web` follow the same rule.

On the first launch, add this machine in the app with `http://127.0.0.1:7890` and an empty bearer
token. For a gateway on a different machine, use its URL and bearer token.
[Desktop and mobile setup](index.md#connect-an-app) has the connection options.

## Automatic native betas

The Beta Native workflow starts after a successful push CI run on `main` and builds the latest
commit whose CI passed. A newer successful run cancels an older beta build.

Beta uses the native build and test workflow of stable releases on Linux x86-64, Linux ARM64, and
macOS ARM64. It also uses the same desktop packaging for macOS, Windows, and Linux. Each build has
an immutable `beta-<commit>` tag. Its prerelease stays a draft until two conditions are true:

- All native tests, SDK checks, TUI checks, and desktop packaging pass.
- The six engine and TUI archives, the web app, and the six desktop installers are present and not
  empty.

A failed or cancelled build cannot replace the last published beta. A beta never becomes the
latest stable release on GitHub, and it does not publish mobile apps.

## Removing Vis

`vis-agent uninstall` removes Vis from this computer. First it lists what it removes.

```bash
vis-agent uninstall --dry-run     # show the list and change nothing
vis-agent uninstall               # show the list, then ask before it removes
vis-agent uninstall --keep-data   # remove the installation, keep sessions and settings
```

Without `--keep-data`, the command also removes `~/.vis`. That folder holds your sessions,
configuration, and provider credentials, and you cannot get them back. To keep them, copy `~/.vis`
first.

The command stops the gateway and closes the desktop app. Then it removes these items:

- The `vis-agent` command and the runtime next to it.
- The desktop app copies in `/Applications` that are Vis apps.
- The desktop app data in `~/Library` on macOS, or under `~/.local/share`, `~/.config`, and
  `~/.cache` on Linux.
- The `PATH` line that the installer added to your shell profile. The line stays when other
  commands share the folder.
- `~/.vis`, or only `~/.vis/install` with `--keep-data`.

These items stay:

- A launcher in a source checkout.
- A desktop app that you installed from a `.deb` or `.msi` package. Remove it with your package
  manager.
- A gateway service that you set up, for example a systemd unit. Disable it and remove its unit
  file, or it fails at the next start.

A script or a CI job must give `--yes`.

## Files

| Path | Contents |
|---|---|
| `~/.local/bin/vis-agent-native` | Installed native engine |
| `~/.local/bin/vis-agent-python/` | Its bundled Python worker and interpreter |
| `~/.local/bin/vis-tui` | Matching native terminal client |
| `~/.local/bin/vis-web/` | Web app that the gateway serves |
| `~/.vis/install/desktop/` | Desktop apps by platform and version, with beta apps in `beta/` and dev builds in `dev/` |
| `~/.vis/install/track` | Selection for later launches |
| `~/.vis/install/src` | Managed dev checkout at a detached `main` commit |
| `~/.vis/install/ref` | Commit that the last dev update pinned |
| `~/.vis/install/vis-agent-native` | Native engine for a launcher in a checkout |

Each native engine has a `vis-agent-native.build` file next to it, with its version, commit, build
track, and timestamp. `vis-agent --version` reports the version. `VIS_HOME` changes the state
directory from `~/.vis`. `vis-agent --measure` prints startup timings. `--jfr` saves Java Flight
Recorder profiles.

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
`bin/release-native` builds and smoke-tests the assets that the host supports. On Apple silicon,
it builds macOS natively, Linux ARM64 in a container, and Linux x86-64 through Rosetta.

Enable Rosetta in Docker Desktop or podman, because the build rejects qemu.
`VIS_CONTAINER_CONNECTION` and `VIS_CONTAINER_CLI` select the container machine and the container
engine. On a platform without native bundles, use the dev track.

## See also

- To add Java or Clojure code to the engine, read
  [Native builds for Java and Clojure extensions](jvm-native-image.md).
- To download a client and connect to your sessions, read
  [Getting started](index.md#connect-an-app).
- For the settings of Vis and its state directory, read [Configuration](configuration.md).
