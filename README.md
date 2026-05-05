<p align="center">
  <img src="https://raw.githubusercontent.com/Blockether/vis/main/docs/src/logo.png" alt="Vis logo" width="200">
</p>

# Vis

A from-the-ground-up coding agent inspired by
[Recursive Language Models](https://arxiv.org/abs/2512.24601) (Zhang,
Kraska & Khattab, 2025). Works with any text-based model.

Instead of accumulating messages into an ever-growing context window,
Vis treats the context as an **external environment** the model
interacts with through code. The model writes Clojure, a sandboxed
[SCI](https://github.com/babashka/sci) interpreter executes it, and
results flow back as a compact journal. State lives in named vars and
a SQLite DB, not in the token budget. No compaction, no sliding
windows, no "summarize the last 50 messages".

## Quick install

**macOS / Linux / WSL**

```bash
curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
~/.local/bin/vis help
```

**Windows PowerShell**

```powershell
irm https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source.ps1 | iex
~\.local\bin\vis.cmd help
```

Requires Java 8+ and the [Clojure CLI](https://clojure.org/guides/install_clojure).
The installer clones Vis under `~/.vis/sourcecode` and links the
`vis` binary to `~/.local/bin`.

## Corporate / proxy install

Zscaler and similar corporate proxies intercept HTTPS and re-sign with
their own CA, which causes 403 errors from `curl` and `git`.

**Skip verification (quick & dirty):**

```bash
curl -k -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
git config --global http.sslVerify false
```

**Use the corporate CA bundle (proper fix):**

Ask IT for your proxy's CA PEM file, then:

```bash
export https_proxy="http://proxy.corp.local:8080"
export CURL_CA_BUNDLE=/path/to/corp-ca-bundle.pem
git config --global http.sslCAInfo /path/to/corp-ca-bundle.pem

curl -fsSL https://raw.githubusercontent.com/Blockether/vis/main/bin/install-source | bash
```

---

[Visit the book for the full guide, including manual setup and dev paths.](https://blockether.github.io/vis/usage.html)