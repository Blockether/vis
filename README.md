<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="logo.png">
    <img src="logo.png" alt="vis logo" width="240"/>
  </picture>
</p>

<div align="center">
  <h2>
    <a href="https://clojars.org/com.blockether/vis"><img src="https://img.shields.io/clojars/v/com.blockether/vis?color=%23007ec6&label=clojars" alt="Clojars version"></a>
    <a href="https://pypi.org/project/vis-agent/"><img src="https://img.shields.io/pypi/v/vis-agent?color=%23007ec6&label=PyPI%20SDK" alt="Vis Python SDK version on PyPI"></a>
    <a href="https://github.com/Blockether/vis/blob/main/LICENSE">
      <img src="https://img.shields.io/badge/license-Apache%202.0-green" alt="License - Apache 2.0">
    </a>
    <a href="https://vis.blockether.com/"><img src="https://img.shields.io/badge/Documentation-teal" alt="Documentation"></a>
    <a href="https://vis.blockether.com/extensions/"><img src="https://img.shields.io/badge/Extension_Center-dimgray" alt="Extension Center"></a>
  </h2>
</div>

# Vis

Vis is a coding agent you can adapt to your tools and workflow.
Give it tools for your project, measure the results on real tasks, and use failures
to improve those tools and checks.

<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset=".github/assets/vis-loop-dark.png">
    <img src=".github/assets/vis-loop-light.png" width="100%" alt="The improvement loop. First, build tools for your project. Then run real tasks, measure the results, inspect the failures, and improve the tools and checks. Then run the tasks again.">
  </picture>
</p>

Repeat the same tasks after each change to compare the results.

Follow the work from your terminal, browser, desktop or phone, with the same
sessions on each.

## Install

For macOS (Apple silicon) and Linux (x64 or ARM64). On Windows, use WSL2.
You can [read the installer](https://github.com/Blockether/vis/releases/download/installer/install-vis-agent) first.

```bash
curl -fsSL https://github.com/Blockether/vis/releases/download/installer/install-vis-agent | bash
```

Then run `vis-agent tui` (terminal), `vis-agent web` (browser) or `vis-agent desktop` (desktop app).
All clients show the same sessions.

<p>
<a class="store-windows" href="https://github.com/Blockether/vis/releases/download/v0.5.19/vis-companion-0.5.19-windows-x64.msi"><img src="resources/vis-docs/assets/install-windows.png" alt="Download Vis for Windows x64" height="40"></a>
<a class="store-macos" href="https://github.com/Blockether/vis/releases/download/v0.5.19/vis-companion-0.5.19-macos-universal.dmg"><img src="resources/vis-docs/assets/install-macos.png" alt="Download Vis for macOS" height="40"></a>
<a class="store-linux" href="https://github.com/Blockether/vis/releases/download/v0.5.19/vis-companion-0.5.19-linux-x64.AppImage"><img src="resources/vis-docs/assets/install-linux.png" alt="Download Vis for Linux x64" height="40"></a>
<a class="store-apple" href="https://testflight.apple.com/join/4anYT4Wk"><img src="resources/vis-docs/assets/install-testflight.png" alt="TestFlight for iOS and iPadOS" height="40"></a>
<a class="store-android" href="https://play.google.com/apps/testing/com.blockether.viscompanion"><img src="resources/vis-docs/assets/install-google-play.png" alt="Google Play beta for Android" height="40"></a>
</p>

[Linux ARM64](https://github.com/Blockether/vis/releases/download/v0.5.19/vis-companion-0.5.19-linux-arm64.AppImage) · [All packages](https://github.com/Blockether/vis/releases/latest) · [Other installation options](resources/vis-docs/distributions.md) · [Pair a phone](resources/vis-docs/index.md#pair-a-phone)

## Documentation

Read the manual at [vis.blockether.com](https://vis.blockether.com/).

## License

Apache License 2.0 — see [LICENSE](LICENSE).

The speech service can install third-party models with separate license terms.
[THIRD_PARTY_MODELS.md](THIRD_PARTY_MODELS.md) lists their authors and licenses.
