Vis is a coding agent you can adapt to your tools and workflow.
Use it to explore a project, make changes and check the results. You can work
in the terminal or desktop app, then follow the same session from your phone.

<nav class="quick-links" aria-label="Getting started">
  <a href="motivation.md">Motivation</a>
  <a href="#install">Install</a>
  <a href="#first-session">First session</a>
  <a href="gateway.md">Desktop and mobile</a>
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

<div class="store-links" aria-label="Download the desktop app">
<a class="store-macos" href="https://github.com/Blockether/vis/releases/latest"><img src="assets/install-macos.png" alt="Latest desktop release for macOS" width="224" height="56"></a>
<span aria-hidden="true">&nbsp;&nbsp;</span>
<a class="store-linux" href="https://github.com/Blockether/vis/releases/latest"><img src="assets/install-linux.png" alt="Latest desktop release for Linux" width="224" height="56"></a>
</div>

Or run `vis-agent desktop --track release` to download and open it automatically.
Opening the app does not start a gateway. Follow
[Desktop and mobile apps](gateway.md) to connect it, or see
[Desktop setup](distributions.md#open-the-desktop-app) for launcher options.

### Get the phone app

The iPhone, iPad and Android apps are public betas. Install one, then
[pair it with your gateway](gateway.md#pair-a-phone) to see the same projects
and sessions.

<div class="store-links" aria-label="Install the Companion app">
<a class="store-apple" href="https://testflight.apple.com/join/4anYT4Wk"><img src="assets/install-testflight.png" alt="TestFlight for iOS and iPadOS" width="224" height="56"></a>
<span aria-hidden="true">&nbsp;&nbsp;</span>
<a class="store-android" href="https://play.google.com/apps/testing/com.blockether.viscompanion"><img src="assets/install-google-play.png" alt="Google Play beta for Android" width="224" height="56"></a>
</div>

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

[Connect to your gateway](gateway.md), open your project and start a session.
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
[Desktop and mobile apps](gateway.md) to follow the same session from another
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

## Learn more

### Guides

- [Configuration](configuration.md) — providers, models and project settings.
- [Project instructions](context-and-prompts.md) — tell the agent how your codebase works.
- [Skills](skills.md) — reusable task instructions.
- [Controlling a session](queue-and-cancel.md) — send follow-ups, cancel a task and exit.
- [Drafts](drafts.md) — try a change in an isolated working copy and review it before approval.
- [Exporting sessions](exporting-sessions.md) — save or share a session.
- [Desktop and mobile apps](gateway.md) — download an app and connect to the same sessions.
- [Council](council.md) — ask another session for help or a second review.
- [Reporting a bug](reporting-bugs.md) — report a problem without exposing private data.

### Extensions

- [Extending Vis](extending.md) — choose a capability and build your first tool.
- [Extension design](extension-design.md) — design and test typed tools.
- [Installing and sharing extensions](extension-packages.md) — layouts, installation, reload and distribution.
- [Using an existing Python project](extension-development.md) — prepare editable uv packages for Vis.
- [Extension API](extension-api.md) — declarations, tool contracts and host operations.
- [Extension troubleshooting](extension-troubleshooting.md) — diagnose loading and call errors.
- [Forms and user input](human-input.md) — ask for choices, credentials or confirmation.
- [Live views](live-views.md) — show progress a person can watch and stop.
- [Provider extensions](provider-extensions.md) — register an LLM provider from an extension.
- [Python SDK](https://pypi.org/project/vis-agent/) — develop and test extensions outside Vis.

### Concepts

- [Why I built Vis](motivation.md) — the motivation for reusable tools, visible work and shared sessions.
- [How Vis manages context](token-optimization.md) — how filtering and summaries keep conversations manageable.
- [Python sandbox](python-sandbox.md) — Python execution, packages and permissions.

### Reference

- [Process jail and network policy](jail.md) — rules for child processes.
- [Runtime distributions](distributions.md) — installation methods and updates.
- [Building the native binary](jvm-native-image.md) — GraalVM build, metadata and TLS proxies.
