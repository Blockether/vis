Vis is a coding agent you can adapt to your tools and workflow.
Use it to explore a project, make changes and check the results. You can work
in the terminal or desktop app, then follow the same session from your phone.

<nav class="quick-links" aria-label="Getting started">
  <a href="motivation.md">Motivation</a>
  <a href="#install">Install</a>
  <a href="#first-session">First session</a>
  <a href="#connecting-the-companion-app">Desktop and mobile</a>
</nav>

## Why Vis

You know which tests matter, how your team reviews changes and what must be
checked before a release. Vis lets you turn that knowledge into reusable tools
and automatic checks, rather than rely only on instructions the agent has to
remember. The model combines those tools in Python; you can inspect both the
code and the results.

Start with the built-in tools. Add your own when you want Vis to follow a
specific workflow. Activities show what happened along the way, and the
terminal, desktop and phone let you follow the same work. Read
[Why I built Vis](motivation.md) for the thinking behind these choices.

## See Vis in action

Browse with the arrows, your keyboard or a swipe. All screenshots use fictional
work in a fresh demo gateway, database and sessions—not personal work.

<section class="screenshot-gallery" id="screenshot-gallery" data-screenshot-gallery role="region" aria-roledescription="carousel" aria-label="Vis screenshots">
  <div class="screenshot-gallery__track" id="screenshot-slides" tabindex="0" aria-label="Vis screenshots; use Left and Right arrow keys to browse">

  <figure class="screenshot-gallery__slide" id="desktop-gallery" role="group" aria-roledescription="slide" aria-label="1 of 9">
    <a class="screenshot-gallery__image" href="assets/screenshots/desktop-conversation.png" aria-label="Open full-size screenshot: Desktop · Conversation">
      <img src="assets/screenshots/desktop-conversation.png" alt="Desktop Vis with the Fieldnotes project sidebar and a completed search task." width="1280" height="800" loading="lazy" decoding="async">
    </a>
    <figcaption>Desktop · Conversation</figcaption>
  </figure>

  <figure class="screenshot-gallery__slide" id="ios-gallery" role="group" aria-roledescription="slide" aria-label="2 of 9">
    <a class="screenshot-gallery__image" href="assets/screenshots/ios-conversation.png" aria-label="Open full-size screenshot: iOS · Conversation">
      <img src="assets/screenshots/ios-conversation.png" alt="Vis on iOS showing a completed search task, its goal, and a table of example checks." width="1206" height="2622" loading="lazy" decoding="async">
    </a>
    <figcaption>iOS · Conversation</figcaption>
  </figure>

  <figure class="screenshot-gallery__slide" id="tui-gallery" role="group" aria-roledescription="slide" aria-label="3 of 9">
    <a class="screenshot-gallery__image" href="assets/screenshots/tui-conversation.png" aria-label="Open full-size screenshot: TUI · Conversation">
      <img src="assets/screenshots/tui-conversation.png" alt="Vis TUI showing the search task, example check results, and a completed goal." width="1255" height="756" loading="lazy" decoding="async">
    </a>
    <figcaption>TUI · Conversation</figcaption>
  </figure>

  <figure class="screenshot-gallery__slide" role="group" aria-roledescription="slide" aria-label="4 of 9">
    <a class="screenshot-gallery__image" href="assets/screenshots/desktop-project.png" aria-label="Open full-size screenshot: Desktop · Project tour">
      <img src="assets/screenshots/desktop-project.png" alt="Desktop Vis showing a project tour and a table of fictional source directories." width="1280" height="800" loading="lazy" decoding="async">
    </a>
    <figcaption>Desktop · Project tour</figcaption>
  </figure>

  <figure class="screenshot-gallery__slide" role="group" aria-roledescription="slide" aria-label="5 of 9">
    <a class="screenshot-gallery__image" href="assets/screenshots/ios-project.png" aria-label="Open full-size screenshot: iOS · Project tour">
      <img src="assets/screenshots/ios-project.png" alt="Vis on iOS showing a tour of the fictional Fieldnotes project." width="1206" height="2622" loading="lazy" decoding="async">
    </a>
    <figcaption>iOS · Project tour</figcaption>
  </figure>

  <figure class="screenshot-gallery__slide" role="group" aria-roledescription="slide" aria-label="6 of 9">
    <a class="screenshot-gallery__image" href="assets/screenshots/tui-project.png" aria-label="Open full-size screenshot: TUI · Project tour">
      <img src="assets/screenshots/tui-project.png" alt="Vis TUI with two session tabs and the fictional Fieldnotes project tour." width="1255" height="756" loading="lazy" decoding="async">
    </a>
    <figcaption>TUI · Project tour</figcaption>
  </figure>

  <figure class="screenshot-gallery__slide" role="group" aria-roledescription="slide" aria-label="7 of 9">
    <a class="screenshot-gallery__image" href="assets/screenshots/desktop-release.png" aria-label="Open full-size screenshot: Desktop · Release checklist">
      <img src="assets/screenshots/desktop-release.png" alt="Desktop Vis showing a fictional release checklist with verify, review, and publish steps." width="1280" height="800" loading="lazy" decoding="async">
    </a>
    <figcaption>Desktop · Release checklist</figcaption>
  </figure>

  <figure class="screenshot-gallery__slide" role="group" aria-roledescription="slide" aria-label="8 of 9">
    <a class="screenshot-gallery__image" href="assets/screenshots/ios-sessions.png" aria-label="Open full-size screenshot: iOS · Sessions">
      <img src="assets/screenshots/ios-sessions.png" alt="Vis on iOS listing three fictional Fieldnotes sessions, with the search task starred." width="1206" height="2622" loading="lazy" decoding="async">
    </a>
    <figcaption>iOS · Sessions</figcaption>
  </figure>

  <figure class="screenshot-gallery__slide" role="group" aria-roledescription="slide" aria-label="9 of 9">
    <a class="screenshot-gallery__image" href="assets/screenshots/tui-sessions.png" aria-label="Open full-size screenshot: TUI · Session navigator">
      <img src="assets/screenshots/tui-sessions.png" alt="Vis TUI session navigator listing only the three fictional Fieldnotes sessions." width="1255" height="756" loading="lazy" decoding="async">
    </a>
    <figcaption>TUI · Session navigator</figcaption>
  </figure>

  </div>

  <div class="screenshot-gallery__controls" hidden>
    <button type="button" data-previous aria-controls="screenshot-slides" aria-label="Previous screenshot">← Previous</button>
    <span role="status" aria-live="polite" aria-atomic="true">1 / 9</span>
    <button type="button" data-next aria-controls="screenshot-slides" aria-label="Next screenshot">Next →</button>
  </div>
</section>

## Install

### Install Vis where your work runs

The stable release includes the engine, Python and terminal client. Native
packages support Apple silicon macOS and Linux (x64 or ARM64); installation
needs `curl` and `tar`, not Java or Git. For other platforms or a source build,
see [Runtime distributions](distributions.md).

The installer writes to `~/.local/bin` and can update your shell profile. You can
[read the installer](https://github.com/Blockether/vis/releases/download/installer/install-vis-agent)
before running it:

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

If you connect to Vis on another computer, install the engine there. The desktop
and phone apps are clients: they connect to its **gateway**, the service that
runs your sessions and works with your files.

### Get the desktop app

Download the latest stable app from
[GitHub Releases](https://github.com/Blockether/vis/releases/latest).
Choose the universal macOS `.dmg`, or the Linux `.AppImage` for x64 or ARM64.
Windows is not a desktop target.

<div class="store-links" aria-label="Download the desktop app">
<a class="store-macos" href="https://github.com/Blockether/vis/releases/latest"><img src="assets/install-macos.png" alt="Latest desktop release for macOS" width="224" height="56"></a>
<span aria-hidden="true">&nbsp;&nbsp;</span>
<a class="store-linux" href="https://github.com/Blockether/vis/releases/latest"><img src="assets/install-linux.png" alt="Latest desktop release for Linux" width="224" height="56"></a>
</div>

Or run `vis-agent desktop --track release` to download and open it automatically.
Opening the app does not start a gateway. Follow the
[connection steps below](#connecting-the-companion-app), or see
[Desktop setup](distributions.md#open-the-desktop-app) for launcher options.

### Get the phone app

The iPhone, iPad and Android apps are public betas. Install one, then
[pair it with your gateway](#pair-a-phone) to see the same projects
and sessions.

<div class="store-links" aria-label="Install the Companion app">
<a class="store-apple" href="https://testflight.apple.com/join/4anYT4Wk"><img src="assets/install-testflight.png" alt="TestFlight for iOS and iPadOS" width="224" height="56"></a>
<span aria-hidden="true">&nbsp;&nbsp;</span>
<a class="store-android" href="https://play.google.com/apps/testing/com.blockether.viscompanion"><img src="assets/install-google-play.png" alt="Google Play beta for Android" width="224" height="56"></a>
</div>

## Connecting the Companion app

Use the desktop or phone app to follow the same work from another device.
The apps connect to a **gateway**, the service running Vis on the computer where
your projects live. Your files, commands and sessions stay on that computer.

The app does not install or start the engine. [Install Vis](#install) on the
computer that will run your work first. You do not need a separate Vis account
to connect an app; model providers may require their own account or API key.

### Connect the desktop app

For an app and gateway on the same computer, use the local address
`http://127.0.0.1:7890`. If a gateway is already running, check its address with
`vis-agent gateway status` and reuse it. Otherwise, start one:

```bash
vis-agent gateway start --host 127.0.0.1
```

Keep that terminal open: this command runs the gateway in the foreground.
Open the desktop app, or use another terminal to run:

```bash
vis-agent desktop --track release
```

In **Add a machine**, paste the gateway's address (`http://127.0.0.1:7890` for
the command above). Leave the bearer token empty for the default local gateway;
if you enabled token authentication, supply its token. Once connected, open
your project and start or resume a session. The
[first-session guide](#first-session) covers choosing a model and a task.

An Intel Mac can use the desktop app with a remote gateway, or run a local
engine from the [JVM source distribution](distributions.md). There is no native
Intel macOS engine bundle.

For a gateway on another computer, follow the phone pairing steps below and
paste the pairing link instead of scanning its QR code. `127.0.0.1` always means
the device you are using; it cannot reach a different computer.

### Pair a phone

Your phone needs an address it can reach over your local network or a private
VPN such as Tailscale. A local-only gateway on `127.0.0.1` is not reachable from
your phone. Keep token authentication enabled for remote access. The pairing
link and QR code contain that token: treat them like a password and do not share
them publicly. Use a trusted network, VPN or HTTPS for remote connections.

On the computer running Vis, start a gateway on its network address. Replace
`10.0.0.5` with that computer's address:

```bash
vis-agent gateway start --host 10.0.0.5 --require-token --pair
```

In the app, open **Add a machine** and scan the QR, or paste the
`vis://gateway?url=…&token=…` link printed under it. Both fill in the address and
token. Once connected, you can open the same projects and sessions as in the
terminal or desktop app.

For a gateway that is already running, print a new pairing QR without restarting:

```bash
vis-agent gateway pair
```

If that gateway is bound to `127.0.0.1`, the command asks you to restart with a
reachable address. Check that other clients and sessions can be interrupted
before stopping it; see [Starting the gateway](#starting-the-gateway).

You can also type a reachable address and supply the token from
`~/.vis/gateway.token` on the gateway's computer. Each saved machine shows its
connection state: green online, red offline, amber wrong or missing token.

### Access from anywhere with Tailscale

Put both devices on a [Tailscale](https://tailscale.com) tailnet to reach your
gateway away from the local network. Listen on the computer's Tailscale address,
then pair the app. The pairing QR prefers the machine's `100.x` Tailscale address.

You can use `--host 0.0.0.0` to listen on all IPv4 interfaces, but that includes
public interfaces if present. Bind to a specific private address when you only
need private access. A bearer token controls access; it does not encrypt HTTP.

## First session

Before sending a task, choose how you will access a model. You need a supported
provider account or API key, and your provider may charge for usage. You can
also use Ollama or LM Studio for a local model. See
[Providers and models](configuration.md#providers-and-models).

Vis can edit files and run commands in your workspace. Start with a read-only
task while you get familiar with it. Review changes before using them; you can
[restrict file and network access](jail.md).

### In the terminal

Open a terminal in your project:

```bash
cd /path/to/project
vis-agent tui
```

Vis starts a local gateway if needed.

1. Add a provider in the provider picker.
2. Follow its sign-in instructions.
3. Select a model.

### In the desktop or phone app

[Connect to your gateway](#connecting-the-companion-app), open your project and start a session.
Choose a provider and model there. The files and commands belong to the
computer running the gateway, not the phone or computer displaying the app.

### Try a first task

> Explain how this project is organized and where its tests live. Don't change any files.

You should get an explanation based on the project files. Activities in the
conversation show the searches and reads behind it. Then try a small change,
ask Vis to run the relevant tests and inspect the diff.

## Work with a project

### Put your expertise into code

Use `AGENTS.md` for project context and [Skills](skills.md) for reusable
procedures. When you need an operation to follow the same rules every time,
turn it into a tool or check. [Extending Vis](extending.md) starts with one
Python file; you do not need to build an extension before using Vis.

### Combine steps in Python

Your tools can work together: read a build report, select the failed tests and
rerun them. The model connects the steps in Python and can keep useful helper
definitions for later. [Extension design](extension-design.md) shows how to
make those operations useful for your environment.

### Follow the work on every screen

Activities show actions and their results in the conversation. Use
[desktop and mobile apps](#connecting-the-companion-app) to follow the same session from another
device. You can send a follow-up or stop a task while it runs; see
[Controlling a session](queue-and-cancel.md).

### Keep useful work when you return

Return to a session to continue its conversation. Reusable helper definitions
survive restarts, and the full history stays stored even when Vis summarizes
completed work to make room for the next task. See
[How Vis manages context](token-optimization.md).

## Update

```bash
vis-agent update
```

This selects the latest stable release, even if you previously used another
track. For beta or source builds, name the track explicitly; see
[updates and release tracks](distributions.md#updating-and-selecting-a-track).

## Native vs JVM

The default native release is the everyday option. Use the JVM source build
when developing Vis or trying the latest code. See the
[runtime comparison](distributions.md#native-vs-jvm) for requirements and measurements.

## Gateway reference

Use this reference when you need to manage a gateway, connect another CLI client
or build an integration. You do not need it to try your first task.

### Starting the gateway

The terminal client finds a local gateway and starts one in the background if
needed. This managed gateway stops after the last client disconnects and no
work remains. Opening the desktop app does not start a gateway.

Stopping a busy gateway interrupts its clients and work. The unconditional stop
can escalate to SIGTERM/SIGKILL. Use `--if-idle` when you do not want to interrupt
another session.

```bash
vis-agent gateway status          # show the address and connected clients
vis-agent gateway stop --if-idle  # stop only when nobody is using it
vis-agent gateway stop            # stop even if clients are connected
```

`vis-agent gateway start` runs in the foreground and does not stop on its own.
For a gateway you want to keep available, run it under a process supervisor
such as systemd or launchd, or in a terminal multiplexer such as tmux.

`vis-agent update` stops an idle managed gateway so the next client starts the
new build. Use `--keep-gateway` if you want to leave it running.

### Using a remote gateway from the CLI

The terminal client can also connect to another computer. Supply its address
and token with these root flags:

```bash
vis-agent --gateway 10.0.0.5 --gateway-token "$TOKEN" tui
vis-agent --gateway https://gateway.example.com/vis --gateway-token "$TOKEN" gateway status
```

`--gateway` accepts `HOST`, `HOST:PORT` or a full URL; a bare host means HTTP on
port `7890`. You can instead set `VIS_GATEWAY_URL` and `VIS_GATEWAY_TOKEN` in
your shell. To reach a local-only gateway through an SSH tunnel:

```bash
ssh -N -L 7890:127.0.0.1:7890 you@10.0.0.5 &
vis-agent --gateway 127.0.0.1 tui
```

Supply a token if that gateway requires one. With `--gateway`, Vis never starts,
restarts or stops a replacement gateway. An unreachable target is an error,
not a fallback to a local one. The `sessions` commands (`list`, `show`, `fork`,
`delete`, `export`) still read the local database.

### Tokens and HTTP 401

| Gateway address | Token required? |
| --- | --- |
| `127.0.0.1` (default) | No |
| Any other address (`0.0.0.0`, LAN, Tailscale) | Yes |
| `127.0.0.1 --require-token` | Yes |

Vis creates the token in `~/.vis/gateway.token` with owner-only permissions
(mode `600`). `--token-file PATH` chooses a different file. Local CLI clients
read it automatically; remote clients receive it through pairing. In the
desktop app, supply it when adding a token-protected local gateway.

An `HTTP 401` error means the gateway is reachable but the token is missing or
incorrect. Pair the client again or check that you supplied the token for the
right gateway. Do not disable remote authentication to work around the error.

### HTTP API

The gateway serves its OpenAPI 3.1 schema without a token:

```bash
curl -sS http://127.0.0.1:7890/openapi.json -o vis-gateway.json
```

Use the schema for routes, request formats and responses. Protected routes
require the gateway token. An incompatible client receives `HTTP 426`;
update the client or gateway to a compatible version.

### Python SDK

Start with the [Python SDK](python-sdk.md) to run a task in your project with
`Agent(project=".")`, or pass `gateway_url` to the same Agent interface for remote
work. Follow-up requests reuse the conversation. For JVM applications, see the
[Java and Clojure SDK](jvm-sdk.md). To share the engine across clients,
[run a gateway](gateway-service.md); no native build is needed. Use `GatewayClient`
when your program needs to create or manage several gateway sessions.

### Resource limits

Set these environment variables before starting the gateway:

| Variable | Default | Purpose |
| --- | ---: | --- |
| `VIS_GATEWAY_MAX_CONCURRENT_TURNS` | `50` | Turns executing at once across all sessions |
| `VIS_GATEWAY_EVENT_RING_MAX` | `2000` | Events kept per session for SSE replay |
| `VIS_ENV_CACHE_MAX` | `8` | Idle session environments kept resident |
| `VIS_ENV_MAX_TURNS_PER_CTX` | `5` | Turns before a Python session is recycled |
| `VIS_ENV_RSS_BUDGET_MB` | `3072` native / `5120` JVM | Process memory threshold for eviction |

A value `<= 0` disables an eviction threshold.

<span id="see-also"></span>

## Learn more

### Guides

- [Configuration](configuration.md) — providers, models and project settings.
- [Project instructions](context-and-prompts.md) — tell the agent how your codebase works.
- [Skills](skills.md) — reusable task instructions.
- [Controlling a session](queue-and-cancel.md) — send follow-ups, cancel a task and exit.
- [Drafts](drafts.md) — try a change in an isolated working copy and review it before approval.
- [Exporting sessions](exporting-sessions.md) — save or share a session.
- [Council](council.md) — ask another session for help or a second review.
- [Reporting a bug](reporting-bugs.md) — report a problem without exposing private data.

### SDKs

- [Python SDK](python-sdk.md) — run a local agent, continue a conversation or connect remotely.
- [Java and Clojure SDK](jvm-sdk.md) — call Vis from a JVM application.
- [Running a gateway](gateway-service.md) — install and operate a shared agent service.

### Extensions

- [Extending Vis](extending.md) — choose a capability and build your first tool.
- [Extension design](extension-design.md) — design and test typed tools.
- [Installing and sharing extensions](extension-packages.md) — layouts, installation, reload and distribution.
- [Using an existing Python project](extension-development.md) — prepare editable uv packages for Vis.
- [Native builds for Java and Clojure extensions](jvm-native-image.md) — only for adding JVM capabilities inside Vis.
- [Extension API](extension-api.md) — declarations, tool contracts and host operations.
- [Extension troubleshooting](extension-troubleshooting.md) — diagnose loading and call errors.
- [Forms and user input](human-input.md) — ask for choices, credentials or confirmation.
- [Live views](live-views.md) — show progress a person can watch and stop.
- [Provider extensions](provider-extensions.md) — register an LLM provider from an extension.

### Concepts

- [Why I built Vis](motivation.md) — the motivation for reusable tools, visible work and shared sessions.
- [How Vis manages context](token-optimization.md) — how filtering and summaries keep conversations manageable.
- [Python sandbox](python-sandbox.md) — Python execution, packages and permissions.

### Reference

- [Process jail and network policy](jail.md) — rules for child processes.
- [Runtime distributions](distributions.md) — installation methods and updates.
