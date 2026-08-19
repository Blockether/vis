# Configuration

Vis reads config from four YAML sources, deep-merged in order — later sources win, nested maps merge, scalars and vectors replace:

1. `~/.vis/config.yml` (or `config.yaml` / `vis.yml` / `vis.yaml`) — **global base**, hand-written, optional.
2. `~/.vis/state.yml` — **machine store**, written by Vis itself (provider setup, OAuth tokens, TUI-added providers). Kept separate from the hand-written base so the read-modify-write cycle never clobbers your file; wins over the base per key.
3. `<project>/vis.yml` (or `vis.yaml`) — **project root**, visible. The natural home for team-shared, committed settings.
4. `<project>/.vis/config.yml` (or `.vis/config.yaml`) — **project overlay**, hidden. The nested overlay wins over the root file: personal beats committed.

"Project" means the directory you launched `vis-agent` from. Everything else Vis owns lives next to the global config: the session database at `~/.vis/vis.mdb` and the log at `~/.vis/vis.log`.

## Gateway-managed MCP servers

The gateway API — and the Companion on top of it — writes MCP servers to the machine store (`~/.vis/state.yml`) and nowhere else. Servers you declare by hand in any other source are still listed, connected, and usable, but they come back with `is_managed: false`: a client never rewrites them. The project tiers win on merge, so a write from there would either be silently shadowed or fork a stale duplicate of your own spec into the machine store. Toggling, saving, or deleting one answers `409`; edit the file that declares it.

Saving a managed server keeps its stored `env` and `headers` when the request omits those keys — the inventory a client reads never carries secret values, so a round-trip through a UI cannot wipe them. Sending the key explicitly, including as an empty map, still replaces it.

The gateway also owns the *connections*, not just the config. Every enabled server is connected once, daemon-wide, and shared by every session; the daemon re-checks that pool on its own clock (and on every turn and `/reload`), reaps a crashed `stdio` child or a dropped HTTP session, and reconnects it. So a session never connects or disconnects anything — there is no such tool, and none is needed: a server you save, enable, or start is simply there, in the TUI and in the Companion alike. Stopping one is an explicit admin action (Kill, or `enabled: false`), and it applies everywhere, because everyone is using the same connection.

## Killing a server, and signing one in

Two things are **runtime**, not config, so they work on hand-written servers too:

- **Kill / Start.** Killing a server closes its connection — and, for a `stdio` server, its child process — and keeps it closed: the gateway reconciles connections continuously, and a killed server is skipped instead of reconnected. Nothing is written to disk, so a kill does not survive a gateway restart and never edits your YAML. `Start` releases it. Disabling, by contrast, persists `enabled: false`.
- **Browser sign-in.** An HTTP MCP server that answers `401` needs OAuth. The gateway runs the whole flow — discovery, dynamic client registration, PKCE — and holds the tokens; a client only receives an authorization URL to open and hands back the code. Start a flow, open the URL, and either let the loopback redirect complete it by itself or paste the redirect URL (or the bare `code`) back. Poll until it reports `authorized`; `is_authorized` on the inventory row says whether tokens are already stored. Signing out forgets them.

Because the flow lives on the gateway, a Companion on your phone and a TUI attached to a remote gateway authorize a server exactly the same way, and neither one ever holds a token or a PKCE verifier. In the TUI it is the `MCP Servers` command in the palette.

### CLI: `vis-agent gateway mcp`

Every MCP admin action above is also a CLI verb, talking to the gateway already running for `--db` (or the default DB) over the same HTTP surface as the Companion and the TUI: `list`, `add`, `test`, `remove`, `enable`, `disable`, `kill`, `start`, and the OAuth legs `auth-start` / `auth-complete` / `auth-poll` / `auth-cancel` / `auth-logout`. Run `vis-agent gateway mcp --help` for the full flag reference.

Step-by-step for a remote OAuth server (e.g. Linear's `https://mcp.linear.app/mcp`):

```sh
# 1. A gateway must already be running (vis-agent gateway start, or the one
#    Vis starts for you). Then save the server -- a URL alone infers
#    Streamable HTTP:
vis-agent gateway mcp add linear --url https://mcp.linear.app/mcp

# 2. Begin the headless OAuth 2.1 flow (RFC 9728/8414 discovery, dynamic
#    client registration, PKCE). This prints an authorize URL and a flow_id:
vis-agent gateway mcp auth-start linear

# 3. Open the printed URL in a browser and approve access. The provider
#    redirects to a loopback URL (http://127.0.0.1:PORT/mcp-callback?code=...).
#    Copy that FULL URL (or just the bare code) and finish the flow:
vis-agent gateway mcp auth-complete linear --flow-id <FLOW_ID> --input "<PASTED_URL_OR_CODE>"

# 4. Confirm it is authorized and connected:
vis-agent gateway mcp list
```

Read-only access is a URL, not a flag: point `--url` at Linear's `/mcp/readonly` endpoint instead. A static API key/bearer token (Linear's non-interactive alternative) skips `auth-start` entirely: pass it as a header instead --

```sh
vis-agent gateway mcp add linear --url https://mcp.linear.app/mcp --headers "Authorization=Bearer <TOKEN>"
```

`vis-agent gateway mcp test` connects a candidate spec (same flags as `add`) without saving it, so a bad URL/command is caught before it is persisted. `kill`/`start` are the runtime pause/resume described above; `disable`/`enable` persist the on/off switch; `auth-logout` forgets stored tokens; `remove` deletes the server and stops it.


## Keys are snake_case strings

Config is YAML only, validated exactly as parsed:

1. **Configuration keys stay strings.** Canonical keys use snake_case. No recursive
   keywordization or kebab normalization occurs, so `system_prompt` is valid and
   `system-prompt` is rejected as an unknown key. Boolean flags use an `is_` prefix
   and never a `?` suffix, e.g. `is_replace`, `is_respect_retry_after`, `is_tool_call`.
2. **User-owned keys stay verbatim.** Environment variables, MCP server names and
   `env`/`headers`, HTTP headers, request-body fields, pricing/model ids, and toggle ids
   retain their exact spelling and case.

## Executable configuration contract

`com.blockether.vis.internal.config-spec/config` is the complete `clojure.spec`
contract for the original string-keyed YAML representation. It covers these closed
top-level blocks: `providers`, `default_provider`, `default_model`, `fallback_provider`,
`fallback_model`, `router`, `system_prompt`, `workspace`, `jail`, `environment`,
`db_spec`, `grep`, `toggles`, `tui_settings`, `mcp`, `python`, and `titling`. Filesystem
admission is a closed block at `jail.filesystem`, and egress policy is a closed block at `jail.network`.

Nested maps are also closed except maps whose keys are user-defined, such as environment
variables, HTTP headers, toggle ids, MCP server names, pricing tables, and request bodies.
Unknown keys and invalid value types fail config loading with source-aware spec problems;
credentials are redacted from those problems.

The parser validates this string-keyed map before any internal adaptation. The same
namespace derives process-jail and network policies directly from that validated map, so
security enforcement and the YAML schema cannot maintain different key lists.

## Invalid config: what you see

A bad config is a *user error*, not a crash. Vis prints a panel that names every
offending field by its full path — no stack trace, no log file to open — and exits
with status 2:

```
  Invalid Vis configuration in /project/.vis/config.yml:

  - providers[0].models[0].contxt: unknown key (config is closed) — did you mean "providers[0].models[0].context"?
  - grep.include-gitignored-paths: unknown key (config is closed) — did you mean "grep.include_gitignored_paths"?
  - jail.filesystem.allow_reed: unknown key (config is closed)
  - mcp.servers.docs.transport: value rejected by the transport contract

  Fix the entries above and run vis-agent again.
```

The most common cause is kebab-case: `grep.include-gitignored-paths` must be
`grep.include_gitignored_paths`. Kebab-case names such as
`:include-gitignored-paths` appear in the CHANGELOG and in engine internals — they
are the *internal keyword* mirrors of the YAML keys, never the YAML spelling.
Model names (`providers[].models[].name`) are free-form strings and are never
validated against a known-model list; a wrong one fails at the provider API, not here.

A small config:

```yaml
# vis.yml
system_prompt: Prefer restructuredText docstrings.
router:
  budget:
    max_cost: 5.0
environment:
  ANTHROPIC_API_KEY: {env: ANTHROPIC_API_KEY}
```

## Environment

**The workspace's `.env` and `.env.local` are loaded by default** — the whole
file, with nothing declared, because the project file is part of the project.
Every variable in it reaches Vis' own children: `shell(...)` (confined or not),
managed REPLs, test runners, and Python extensions.

Within the dotenv files the usual rules apply — a later assignment wins, `.env`
beats `.env.local`, an explicit blank masks a lower one, and `NAME=value`,
`export NAME=value`, quotes, comments, CRLF and a UTF-8 BOM are all understood.

`environment:` is for **what a dotenv file cannot say**, and it names the source
of each entry. It never carries the value itself: a literal is rejected, because
every read-modify-write into `~/.vis/state.yml` passes the loaded config back
through the writer, and a secret typed here would land on disk in plaintext.

```yaml
environment:
  # 1. another process variable, passed through or renamed
  OPENAI_API_KEY: {env: WORK_OPENAI_KEY}

  # 2. a dotenv name under a different name (a plain `.env` entry needs nothing)
  STRIPE_KEY: {dotenv: STRIPE_TEST_KEY}

  # 3. the OS credential store (macOS Keychain; `secret-tool` elsewhere)
  EXA_API_KEY:
    keychain: vis-exa
    account: alice          # optional, qualifies the keychain item

  # 4. a helper command; its trimmed stdout IS the value
  GITHUB_TOKEN:
    command: [gh, auth, token]
```

Exactly one source per entry, and **that declaration is the only source for the
name** — a declared name never falls back to `.env` or to the ambient
environment. A blank value is no value, whatever produced it: an explicit `FOO=`
means "not this one".

So the resolution order for any variable is: **`environment:` declaration →
workspace `.env`, then `.env.local` → the environment that started Vis.** It is
the same order everywhere: a Clojure extension, a Python extension, the TUI's
settings row, and every child process.

A `command:`/`keychain:` value is fetched by running the argv directly — never
through a shell — cached briefly and single-flighted, so a keychain prompt or a
vault helper is not forked once per turn. Its stdout is never logged, never
persisted and never placed in an error message.

**The jail does not change any of this** — it decides what comes *besides* the
project. With `jail.enabled: true` the operator's ambient environment is dropped
(see `jail.md`) and only the project's variables plus a non-secret basics
allowlist remain, so `{env: NAME}` is how an ambient variable is re-admitted to a
confined child. `LD_*`, `DYLD_*`, `PERL*`, `BASH_ENV` and friends are refused
from either source — they are consumed before the jail exists.

`jail.environment: inherit` is the all-or-nothing alternative to re-admitting
names one by one: the confined child keeps the operator's whole environment,
secrets included, while every other confinement stays on. The default is
`declared`.

### One call's own environment

`environment:` says what **every** child of Vis gets. A verb that *spawns* one
also carries what **this** child gets on top, as an argument of the call:

```python
sh = await shell("npm test", {"env": {"NODE_ENV": "test"}})
r  = await repl_start({"language": "python",
                       "env": {"DJANGO_SETTINGS_MODULE": "app.settings.test",
                               "STRIPE_KEY": {"keychain": "vis-stripe"}}})
```

It is a **delta, not a replacement**: the workspace's `.env` and its
`environment:` declarations still reach that child, this map wins where a name
collides, and `null` unsets one name for this child only.

A value is either a **literal** — string, number or boolean — or the same
`{env|dotenv|keychain|command}` source map `environment:` takes. That split
matters because, unlike a config file, this map is an *argument*: a literal is
written into the session journal and stays in the transcript for good. Literals
are for switches (`NODE_ENV`, `RUST_LOG`, `PYTHONHASHSEED`); a secret names its
source, and only the child ever sees the value.

Every refusal names the key: a name that is not an environment variable name, a
pre-exec hijack name (`LD_*`, `DYLD_*`, `PERL*`, `BASH_ENV`…), a map that names
no source, and a source that produced nothing. A standing declaration that
resolves to nothing is simply unset — one *call* asking for that variable is an
error, because the call said it needed it.

For a REPL the env is part of that REPL's **identity**. `repl_status` reports
it by name and digest, never by value, and a `repl_start` for a REPL that is
already running with a different env is refused by the keys that differ: there is
no restart verb, so `repl_stop` it and start it again.

## Providers and models

The `providers` vector holds your configured AI providers; **the first entry is the active one**. You normally manage this through the TUI (provider picker / "Add Provider") or the companion app (**Settings → Providers → Add provider**), which know the presets — OpenAI, Anthropic (API and coding plan), OpenAI Codex, GitHub Copilot, Z.AI, plus local Ollama and LM Studio — and handle OAuth where needed. The on-disk shape, if you do edit it:

```yaml
providers:
  - id: anthropic
    api_key: sk-…
    models:
      - name: claude-sonnet-4-5-20250929
  - id: my-gateway
    compatibility: openai        # anthropic | openai | openai-responses
    base_url: https://llm.internal/v1
    api_key: ${LLM_TOKEN}
    models:
      - name: qwen3-coder-30b
        context: 262144          # input window
        output_limit: 32768      # max output tokens
        is_tool_call: true
```

Per-model keys Vis honors: `context` (input window — the override for servers that can't report one), `output_limit` (max output tokens), `is_tool_call`, and `api_style` (a wire override when one provider routes different models through different APIs). Both limits are forwarded to the router, which uses them for pre-flight context checks and output capping, so filling them in makes routing decisions accurate instead of conservative.

Per-provider keys: `compatibility`, `base_url`, `api_style`, `llm_headers`, `extra_body`. **`compatibility`** is the wire dialect the endpoint speaks — `anthropic` (Anthropic Messages), `openai` (OpenAI chat completions), or `openai-responses` (OpenAI Responses API) — and is all a custom or self-hosted endpoint normally needs. `api_style` remains the raw escape hatch for anything outside those dialects (e.g. `gemini`) and wins if both are set. Note that the dialect is normally a property of the **endpoint**, so it belongs on a provider; use a per-model `api_style` only when one endpoint deliberately routes models through different APIs. Providers with managed auth (Copilot, coding plans) resolve tokens through their extension at runtime — no `api_key` needed in the file.

**Model order is your order.** Models are offered in exactly the order you wrote them under a provider in `vis.yml`; anything discovered from the provider's live catalog is appended after them, sorted. With nothing else set, the first provider is the active one and its first model is the default.

Vis is model-agnostic: anything that speaks an OpenAI- or Anthropic-style chat API works, including fully local models.

**A model may name its provider: `provider/model`.** Anywhere the CLI takes `--model`, a slash-qualified name is accepted and is the one-shot equivalent of passing `--provider` and `--model` together:

```bash
vis-agent --model zai-coding-plan/glm-5.2 "task"      # same as --provider zai-coding-plan --model glm-5.2
vis-agent --model glm-5.2 "task"                      # bare name: selects on the ACTIVE provider
```

The named provider is promoted to the router root for that run only, and it does not have to exist in `vis.yml` yet — an unconfigured one is synthesized from its built-in preset, so a provider that already has managed auth (a coding plan, Copilot) is usable without touching config. Nothing is persisted. A provider Vis cannot resolve is reported as a user error naming it, before the first request.

### The default selection is ONE pair

Picking a model in the TUI provider manager, the gateway, or the companion app writes exactly two top-level keys:

```yaml
default_provider: zai-coding-plan   # provider id
default_model: glm-5.2              # model name WITHIN that provider
```

There is one default in the whole config — one provider and one model — not one default per provider. Consequences worth knowing:

- `default_model` is resolved **inside `default_provider`'s catalog**. A name that provider does not offer is not an error: Vis falls back to that provider's **first** model — the named provider still wins.
- `default_model` also accepts the `provider/model` form, exactly like `--model`, and then its provider part wins over `default_provider`:

  ```yaml
  default_model: zai-coding-plan/glm-5.2   # one line, both halves of the pair
  ```
- Vis does **not** remember a model per provider. Change `default_provider` alone and you get that provider's first model until you set `default_model` too.
- Choosing a default never reorders `providers:` — order stays yours (above), the pair is just a pointer into it.
- **The pair is per user, never per repository.** Which provider you are entitled to and which model you pay for is your own decision, so the committed `<project>/vis.yml` may not carry it: that tier is loaded with `default_provider`, `default_model`, `fallback_provider` and `fallback_model` **dropped**, and one warning naming the file. Their homes are `~/.vis/config.yml` (hand-written), `~/.vis/state.yml` (what the TUI, gateway and companion write when you pick a model), and the gitignored `<project>/.vis/config.yml` overlay when a pin really is per-checkout. `--model provider/model` (above) still overrides both for one run without persisting anything.

### The fallback selection is a SECOND pair

Two more top-level keys name where Vis goes when the default provider cannot serve the turn — rate-limited, circuit-open, erroring:

```yaml
fallback_provider: anthropic-coding-plan   # must be a DIFFERENT provider than default_provider
fallback_model: claude-sonnet-5            # accepts the same `provider/model` form
```

Same shape and the same resolution rules as the default pair: the model is looked up inside that provider's catalog, a name it does not offer degrades to that provider's first model, and a slash-qualified `fallback_model` carries its own provider.

- **It must be a different provider.** A fallback on the default's own provider is not a fallback at all: the TUI and the companion app both disable the action on the current default's card, the daemon answers `400`, and a config file that tags it anyway is ignored when the router is built.
- The tagged provider is seated **immediately behind the default** in router priority, so it is the first provider Vis moves to; every other provider keeps its configured order behind the two.
- The tag lives and dies with its provider: logging that provider out clears the pair, and clearing the fallback from any client drops both keys.
- Per user, exactly like the default pair: a committed `<project>/vis.yml` cannot tag a fallback — both keys are dropped from that tier with a warning.
- Unrelated key, easy to confuse: `router.rate_limit.is_fallback_provider` (below) is the *boolean* deciding whether a rate-limit budget exhaustion may move on at all — it does not name a provider.

### Model capabilities: chat and vision

Capabilities are per **model**, not per provider, and come from the router's pinned model registry (svar) — the same source as context windows and pricing. Every model has `chat`; `vision` is the one Vis actually gates on.

Vis produces images on its own: matplotlib figures captured from the Python sandbox and anything `attach`ed with an `image/*` media type are stored as durable session attachments (downsized first, so they stay legible without flooding the context). On later iterations and later turns they are replayed to the model as a canonical `image_url` data-URI block, emitted as its own message right after that iteration's `<results>`; the router translates it to Anthropic `image`, OpenAI `image_url`, or Gemini inline data, whichever the active provider speaks.

That replay is gated on the target model advertising `vision`:

- a vision model (`claude-opus-5`, `gpt-5.6-sol`, `gemini-3-pro-preview`, `glm-4.6v`) sees the figures it generated;
- a text-only model (`glm-4.7`, `glm-5.2`, Copilot models without vision) is silently sent the results text and no image block — a broken payload is never fabricated for it;
- non-image attachments (csv, json, wav) are never turned into image blocks; the filter is the stored media type, not the file name.

Model names are matched with version/date suffixes tolerated, so `claude-opus-5-20991231` still resolves to a vision model. A name the registry does not know at all resolves to `chat` only — conservative on purpose, but it means a brand-new model reached through a custom `base_url` gets no image replay until its name is known to the router.

### Environment references

Any string value may reference an environment variable as `${NAME}`. Vis resolves it while loading config, so a key never has to sit in the file:

```yaml
providers:
  - id: anthropic
    api_key: ${ANTHROPIC_API_KEY}
    base_url: https://${LLM_HOST}/v1
```

`${NAME}` is the only spelling — bare `$NAME` is deliberately not recognised, because it cannot be told apart from a value that legitimately starts with `$`. References work in any string, nested anywhere; map keys are left alone.

An **unset** variable is not a load failure. Vis is a long-lived gateway whose config is re-read live and on `/reload`, so one unused provider's missing key must never kill a session running happily on a healthy provider. The reference is left verbatim instead, and that provider is reported unusable in three places:

- the **provider manager** shows `NEEDS ENV · ANTHROPIC_API_KEY` instead of an authenticated verdict;
- **`vis-agent doctor`** warns `can't use anthropic: ANTHROPIC_API_KEY is not set` and names the export to run;
- one warning is logged at load time, once per set of missing variables.

The provider is also dropped from the router fleet, so it can never be picked implicitly. Reaching for it **explicitly** — selecting it in the provider picker — is the one place that hard-errors, with the same message. Failure lands at the point of intent, never globally.

A value resolved from a whole-value `${NAME}` is written back as `${NAME}` whenever Vis re-saves config, so a theme flip or a toggle change cannot bake the plaintext secret into `~/.vis/state.yml`.

### Provider-native evaluation effort

Use `--reasoning-effort high|max` when a controlled evaluation must send the
provider's exact effort value instead of Vis's adaptive, provider-agnostic
reasoning levels:

```bash
vis-agent --provider zai-coding-plan --model glm-5.2 \
  --reasoning-effort high --json "task"
```

Vis validates the selected provider and model before the first request. An
unsupported model or value returns the accepted values and exits `2`. A
completed run also exits `2` if any iteration changed provider or model;
same-model retries remain valid. Execution failures exit `1`, and a valid run
exits `0`.

Structured output contains an `eval` object with `valid?`, `invalid-reasons`,
and per-iteration reasoning evidence: requested/effective effort, actual
provider and model, the emitted wire fragment, and fallback status. The same
object appears in the final `--full-trace-json-stream` result frame. It is
run evidence only and is not added to the session database schema.

### GitHub Copilot premium requests

Copilot bills per *premium request*, and the price of a call is decided by one
header: `X-Initiator: user` is a full premium request, `agent` is the reduced
agentic rate, and a **missing** header is billed as `user`. The same task can
therefore cost a multiple of itself depending on which calls claim to be human.

Vis marks exactly one call per turn as human. The first iteration of your
request goes out as `X-Initiator: user`; every autonomous continuation inside
that turn — each tool-call round trip — goes out as `agent`. A fifty-iteration
turn is one premium request plus forty-nine agentic ones, not fifty premium
ones.

Everything Vis starts for itself is `agent` outright: LLM session titling, and
the one-shot model calls extensions make (`ask-code!`, `llm-text!`). You never
pay a premium request for a call you did not send.

This applies to every Copilot tier (`github-copilot`, `-individual`,
`-business`, `-enterprise`) and needs no configuration. A per-provider
`llm_headers` entry still wins if you set one — which also means writing
`X-Initiator: user` there bills every iteration at the premium rate.

One more Copilot-specific guard, for the same reason: Claude models on Copilot
never receive `:deep` reasoning implicitly (it is capped to `:balanced`, since
deep reasoning can burn several premium interactions on one prompt), and a
casual message — `hi`, `thanks` — is sent with no reasoning parameter at all.

## System prompt

Append project house rules to the core prompt, or replace it outright:

```yaml
# addendum (string form)
system_prompt: Prefer restructuredText docstrings. Never touch generated/.
```

```yaml
# full replacement
system_prompt:
  text: You are …
  is_replace: true
```

Markdown files work too — `.vis/SYSTEM.md` replaces the core prompt,
`.vis/APPEND_SYSTEM.md` appends to it, in both the project and `~/.vis`. A
project `SYSTEM.md` beats a global one, and both beat the config `is_replace`
form. See [Context files & prompts](context-and-prompts.md).

Independently of this, Vis stacks **`AGENTS.md`** (or `CLAUDE.md` as fallback) context files into every turn as project-owned instructions: `~/.vis/AGENTS.md` (user-global), each ancestor directory of the workspace root, then the workspace root itself — those files, not config, are the right place for repo conventions. See [Context files & prompts](context-and-prompts.md).

## Router

The `router` block tunes the request pipeline — retry pacing, network timeouts, spend limits:

```yaml
router:
  rate_limit:
    same_provider_delays_ms: [2000, 3000, 6000]
    is_respect_retry_after: true
    is_fallback_provider: true
  network:
    timeout_ms: 300000
    idle_timeout_ms: 45000
  budget:
    max_tokens: 1000000
    max_cost: 5.0
```

Omit it and built-in defaults apply. Unknown keys are rejected by the configuration spec.

## Jail, filesystem, and network

The process jail is **off by default** and opt-in via `jail.enabled: true`.
**Strongly recommended** whenever the model runs untrusted code: without it,
managed shells and language processes run with the gateway user's full host
permissions. With it enabled, shell commands and managed language processes run
under the OS jail (Seatbelt on macOS, bubblewrap on Linux) and use the gateway
egress proxy. Shell access has a separate `toggles.shell` switch. Unsupported hosts
currently have no OS boundary and a requested `jail.enabled: true` fails loud.

Filesystem roots are declared once in the `workspace.filesystem` catalog (`id`,
`path`, optional `description`, `access` = `read-write`/`read-only`, `search`,
`draft`, plus the conditional-mount keys `when` and `optional`), and
`jail.filesystem.allow` lists the ids that enter the jail
(deny-by-omission). `draft` is that root's isolation policy for a drafted
session: `shared` (default) writes through to the real root, `copy-only` forks a
private copy the draft never lands back, `copy-and-apply` lands that private copy
on `/draft apply`, and `not-allowed` withholds the root from a drafted session on
read and write. Draft isolation is independent of the jail and applies with
`jail.enabled: false` too — see [Drafts](drafts.md).
Vis's own session folder `~/.vis` is granted implicitly (read/write, `search: false`)
whatever the catalog and the allow list say; declare it to override that.

A declared read-write root that holds a `.git` also appears in the TUI's `C-x g`
status buffer, next to the project and the repositories nested inside it, so one
buffer stages, commits and pushes across every repository the session works on.
It is labelled by its catalog `id`; a `read-only` root is left out, and a drafted
session sees the private copy of an isolated root instead of the real one.
Every repository is listed ONCE: a root declared under another spelling of a
directory already shown — a symlink, a trailing slash, the project itself, a
repository nested inside it, or the trunk a drafted session already shows as its
private copy — earns no second header. Two repositories that would wear the same
name are told apart by as much of their path as it takes (`work/vis` beside
`src/vis`), because that header is what every verb acts on.

Nothing caps how many repositories a buffer shows: a mega-repo that vendors forty
clones under `repositories/` opens as forty headers, each clean one folded to its
single summary line. The only bound is on DISCOVERY — the walk that finds nested
repositories stops after 512 of them, 200k visited files or two seconds — and
when it does stop early the title reads `Git — N roots · scan truncated`, so a
short list is never mistaken for the whole fleet.

`when` and `optional` let ONE catalog serve several machines: `when.os` mounts a
root only on `macos`, `linux`, `wsl` or `windows`, `when.exists` only when that
path is present, and `optional: true` only when the root's own path is. A root
that does not apply is dropped before the jail is built and its id may stay in
`jail.filesystem.allow`; an admitted root whose path is missing is reported by
`vis-agent doctor` and the startup hint instead of failing silently.

```yaml
# vis.yml
workspace:
  filesystem:
    - id: sibling
      path: ~/sibling-repository
      draft: copy-and-apply      # forked per draft, landed by /draft apply
    - id: reference
      path: ~/shared-reference
      access: read-only
    - id: m2
      path: ~/.m2
      description: Maven/Clojure dependency cache
      search: false            # granted but kept out of the default search sweep
    - id: cuda
      path: /usr/local/cuda
      when:
        exists: /usr/local/cuda  # skipped on hosts that do not have it
    - id: scratch
      path: ~/scratch
      optional: true             # mounted only when it exists
jail:
  enabled: true
  # What of the OPERATOR's ambient environment a confined child keeps:
  # `declared` (default, nothing) or `inherit` (all of it, secrets included).
  # The project's own `.env` + `environment:` reach the child either way.
  environment: declared
  filesystem:
    allow: [sibling, reference, m2, cuda, scratch]
  # macOS only: Mach lookups a confined child may make (deny by default).
  mach_services:
    keychain: true             # `gh`/`git` credential helpers may read the Keychain
  # Egress policy is one facet of the jail; jail.enabled is the single gate.
  network:
    allowed_domains:
      - github.com
      - npmjs.org
    denied_domains:
      - example.invalid
    allow_private: false
    # Extra local ports on which a confined shell child may accept connections.
    inbound_ports:
      - 5273
```

[Process jail and gateway egress](jail.md) is the single authoritative
reference for this boundary: the `workspace.filesystem` catalog and
`jail.filesystem.allow` admission model, the network model (HTTPS method/path
policy, MITM behavior, SSRF denial, programmable filters), `jail.network.inbound_ports`,
`jail.mach_services` (macOS Keychain and Mach lookups), snapshot inheritance and
`/reload`, and the read-only `session["access"]` view.
Every filesystem path must be absolute or home-relative (`~`); a bare-relative
path is rejected when the config is read.

One exception is called out there and worth repeating: **`repl_connect` is not
jailed.** It attaches to an already-running, user-owned external process that
Vis did not spawn, so Seatbelt cannot be applied retroactively; stopping the
resource only detaches. Everything Vis *starts* — shells, `subprocess`, managed
REPLs, test runners — is confined.

### macOS native-image startup failures

`CSunMiscSignal.open() failed` with `errno: 1` is not a domain-allowlist error.
GraalVM Native Image tools such as `spel`, `bb`, and `clj-kondo` create a named
POSIX semaphore while installing signal handlers, before their command runs.
The macOS profile permits that single IPC class with `ipc-posix-sem`; network
permissions remain unchanged.

Seatbelt policy is inherited and cannot be replaced inside an already confined
process. After upgrading from a Vis build without this permission, restart the
Vis client/gateway before retrying the native tool. An actual egress-policy
failure instead reports the rejected host (for example, `host not permitted`);
add that hostname to `jail.network.allowed_domains` when appropriate.

### GraalPy internal-resource cache

GraalPy extracts its Python standard library ("internal resources") on first
use into `$XDG_CACHE_HOME/org.graalvm.polyglot` (default
`~/.cache/org.graalvm.polyglot`). This happens at **runtime** on both the JVM
and the compiled native-image binary — the stdlib is not baked into the
executable. If that directory is unwritable (a confined process, a read-only
home, minimal CI), the very first Python block fails with
`ModuleNotFoundError: No module named 'ast'`.

Resolution order for the cache root:

1. The GraalVM system property always wins:
   `vis-agent -J-Dpolyglot.engine.userResourceCache=/path` on the JVM launcher, or
   `JAVA_TOOL_OPTIONS`-style `-D` flags where applicable.
2. `python.resource_cache` in config (`~` expands to your home directory):

   ```yaml
   # vis.yml
   python:
     resource_cache: ~/.vis/cache/graal-resources
   ```

3. `~/.vis/cache/graal-resources` — always preferred when writable. vis redirects
   here unconditionally rather than reusing the default
   `~/.cache/org.graalvm.polyglot` root, so behavior is identical whether or not a
   jail happens to admit that root. This directory is git-ignored.
4. Final fallback: `./.graal-resources` under the working directory (also
   git-ignored) when even `~/.vis` is unwritable.

The property is read **once per process** when the polyglot engine
initializes, so changing it requires restarting the client and the gateway
daemon — `/reload` is not enough. An unusable configured path silently degrades
to steps 3–4 rather than failing startup.

## Python import roots

`vis-agent python` puts a project's own packages on `sys.path` before running, so
`vis-agent python -m pytest tests/` imports a `src/` layout the same way an explicit
`PYTHONPATH=src` invocation would.

The roots are read from the project's packaging metadata with Python's own
parsers — `tomllib` for `pyproject.toml`, `configparser` for `setup.cfg`,
`pytest.ini` and `tox.ini` — never by pattern-matching the file text. Inference
is strictly declarative: nothing is guessed from directory names, a project
without such metadata gets nothing, and a malformed file yields nothing instead
of a partial scrape. Recognized declarations:

```toml
[tool.setuptools.packages.find]      where       = ["src"]
[tool.setuptools]                    package-dir = {"" = "src"}
[tool.pdm.build]                     package-dir = "src"
[tool.poetry]                        packages    = [{include = "pkg", from = "src"}]
[tool.hatch.build.targets.wheel]     packages    = ["src/pkg"]   # parent wins
[tool.pytest.ini_options]            pythonpath  = ["src"]
```

plus `package_dir` under `setup.cfg`'s `[options]` and pytest's `pythonpath`
under `setup.cfg` `[tool:pytest]`, `pytest.ini` `[pytest]` and `tox.ini`
`[pytest]`.

A read that *fails* — a broken interpreter, a transient I/O error — is not the
same as a project that declares nothing. It is retried once and then reported as
a `warning` on the `run_tests` result, so the run continues with no inferred
roots instead of failing outright or degrading silently.

When that is wrong, absent, or simply not how you lay a project out, say it
outright:

```yaml
# vis.yml
python:
  source_paths: [src, lib/vendor, ~/shared/py]
```

Configured paths come **first**, ahead of anything inferred. Relative entries
resolve against the working directory, `~` expands, and an entry that is not an
existing directory is dropped. An explicit `PYTHONPATH` in the environment still
precedes both.

Vis otherwise picks the interpreter itself — uv, then Poetry, then a project
`.venv`, then `python3`. When your workspace mandates a launcher Vis cannot
detect (a wrapper script, a container shim, `vis-agent python`), pin it:

```yaml
# vis.yml
python:
  interpreter: [vis-agent, python]   # argv prefix, or a bare path
  runner: project                    # default run_tests backend
```

The pin is used by `repl_start` / `repl_eval` and by the `project` test runner, ahead
of all detection. A list is the argv prefix verbatim; a bare string is **one**
argument and is never word-split, so a path may contain spaces. A path-like
entry resolves against the project directory, `~` expands, and a bare name is
looked up on `PATH`.

Without a pin, a detected project `.venv` interpreter is invoked by its absolute
path, and by the venv's own executable — never canonicalized into the base
installation, which would leave `pyvenv.cfg` unread and the venv's packages
(`pytest` among them) missing.

`runner` chooses the default `run_tests({"language": "python"})` backend: `graalpy`, the
hermetic stdlib-only sandbox, or `project`, the interpreter's own pytest, where
installed dependencies are visible. An explicit `runner` argument on the call
still wins — the same spelling the result's `runner` key reports.

## Extension environment

Extensions may declare the environment variables they read so Vis can report whether they are available. Their values never come from `vis.yml` itself — the file only says where to fetch them. Resolution is the one order described under [`environment:`](#environment): a declaration first, then the workspace's `.env` / `.env.local`, then the environment that started Vis. An extension receives the names it declared plus the project's own — everything in `environment:` and in `.env` — and nothing else of the host environment. Dotenv files support `NAME=value` and `export NAME=value`, quoted values, comments, CRLF, and a UTF-8 BOM:

```dotenv
ANTHROPIC_API_KEY=…
```

For shell use, export the variable from your shell startup file (such as `.bashrc`) before starting Vis. Vis does not execute shell startup files itself.

## Code search and public repository downloads

GitHub is available for `search(..., {"kind": "code", "provider": "github"})`, or as the `auto` provider’s fallback after Exa returns HTTP 429 or a 5xx response. It requires an authenticated GitHub CLI because GitHub Code Search does not support anonymous requests. Run `gh auth login` and authorize access to the repositories you need; Vis reads the CLI credential only for the request and never stores or logs it. If the CLI is unavailable or unauthenticated, the search result tells the user exactly how to authenticate. Use `provider: "exa"` to disable fallback, or `provider: "github"` to choose GitHub first. GitHub cannot replace general web search.

For a repository already found by search, `download_code("owner/repo", {"ref": "main", "path": "src"})` fetches its public `codeload.github.com` tarball and returns bounded source excerpts directly to the agent—without writing an archive to disk or requiring `gh`. It accepts only an `owner/repo` name, limits compressed archives to 10 MiB, and caps the result to 6 files / 51,200 bytes by default (`max_files` ≤ 20, `max_bytes` ≤ 131,072). It is intentionally not a code-discovery provider, and it cannot read private repositories.

`download_archive("owner/repo", {"ref": "main"})` downloads the complete public codeload archive and **extracts it automatically** beneath the active project working directory. Its result includes the absolute saved `path` (by default `downloads/owner-repo-ref`); pass a relative `directory` to choose another destination within that project. It refuses an existing destination rather than overwriting it, never places archive bytes in model context, rejects unsafe archive entries, and caps compressed downloads at 100 MiB and extracted content at 1 GiB / 10,000 files.

## Feature toggles

Built-in extensions can expose a toggle under `toggles:` — boolean, or an enum with a fixed set of choices. Toggle values merge with the rest of the config and take effect after `/reload` (or the next environment build):

```yaml
toggles:
  # Default: true. Set false to remove the Exa/GitHub/arXiv live-research extension.
  web_search: false
  # Default: true. Set false to remove the sandbox's `shell(...)` call, including from sub-agents.
  shell: false
  # Default: false. Set true to let the agent read its own session database and
  # gateway event journals (session introspection).
  introspection: true
```

After editing `vis.yml`, run `/reload` in the session. With `shell: false`, Vis does not bind `shell` into the Python sandbox (including for sub-agents), so it cannot launch commands or managed language processes. `jail.enabled` is independent: it confines commands when shell access is enabled.

## Session titling

Vis names a session from its first request. The name is written locally and
instantly — the request's first sentence — before anything is sent to a
provider, and it is what the session keeps unless a short LLM call later
improves it. That upgrade is cosmetic, so it never competes with your own turn:
it runs *after* the foreground request has finished, and it never waits out a
rate limit (no `Retry-After` sleep, no retry, no provider failover — a refused
title just leaves the local one in place until a later turn tries again). The
`titling:` block controls all of it:

```yaml
titling:
  # llm (default) | first_sentence | first_words | disabled
  mode: llm
  # optional: pin the title call instead of walking the provider fleet
  provider: zai-coding-plan
  model: glm-4.7
```

- `mode: llm` — local title first, then one LLM upgrade after the turn;
  generated once and never regenerated.
- `mode: first_sentence` / `first_words` — purely local titles. No provider call,
  no quota, no 429 on a trivial first message. `first_sentence` is also the
  shape of the local title used by `llm` mode.
- `mode: disabled` — no auto-title at all.
- `provider` / `model` — pin the title call to one cheap endpoint. Without them
  Vis walks its own cheap-first provider order.

A broken or missing `titling:` block never costs the session its name: config
errors here fall back to the defaults.

## Database

Sessions, turns, and durable agent state live in SQLite. Resolution order: explicit `--db` flag → `VIS_DB_PATH` env var → `db_spec` in config → the default `~/.vis/vis.mdb`. Use `--db :memory` for a throwaway session.

```yaml
db_spec:
  backend: sqlite
  path: /somewhere/else/vis.db
```

## Grep

The `grep` block tunes what `grep` may see. `.gitignore` is ALWAYS
honored — there is no per-call opt-out — so this config block is the only
way to change what search sees. `include_gitignored_paths` re-includes
chosen gitignored subtrees: the walker descends them, bypassing every
nested `.gitignore` layer inside them, while the rest of
the workspace keeps honoring `.gitignore`. This is the fix for
intentionally-gitignored vendored or cloned repos (`repositories/**`): a
`.gitignore` `!` negation cannot re-include them (git never descends into
an excluded directory, so a negation on a child is dead code), but a
tool-side overlay can.

```yaml
# vis.yml
grep:
  include_gitignored_paths:
    - repositories/
  # pruned even inside re-included subtrees; setting it REPLACES the default list:
  always_exclude:
    - .git/
    - node_modules/
    - target/
    - build/
    - dist/
    - __pycache__/
    - .venv/
    - .gradle/
    - vendor/
    - .next/
    - out/
```

```yaml
# vis.yml
grep:
  include_gitignored_paths: [repositories/]
  always_exclude: [.git/, node_modules/, target/]
```

Semantics:

- Both lists speak **`.gitignore` pattern syntax** (`dir/`, `**`, `?`, char
  classes) — not a second glob dialect. `repositories/` and
  `repositories/**` both re-include the whole subtree.
- A path is searched when it is **not** gitignored, **or** it falls under
  an `include_gitignored_paths` pattern — unless `always_exclude` matches
  it. Formally: `excluded?(f) = always-exclude?(f) OR (gitignored?(f) AND
  NOT included?(f))`.
- A pattern also opens the directories **above** it: `repositories/**`
  makes the walker descend into `repositories/` itself even though
  `.gitignore` excludes it.
- `always_exclude` defaults to the denylist in the example above. Setting
  the key replaces the defaults (vectors replace on merge, like everywhere
  else in config). It guards the re-included subtrees; outside them
  `.gitignore` already governs.
- There is no per-call gitignore flag: edit `vis.yml` and `/reload` to
  change what search sees.
- Hidden files stay governed by `is_hidden`: re-including `repositories/`
  never surfaces the repos' `.git` internals (doubly guarded — `.git/` is
  also in the default `always_exclude`).
- The overlay is applied **natively by the fff index** (its ignore walker
  *and* its live file watcher), not by a second pass in vis — so a
  re-included subtree is indexed once, stays incrementally up to date, and
  costs nothing per search. The same mechanism registers `.rgignore`, the
  one ignore filename ripgrep's `ignore` crate does not pick up on its own
  (`.gitignore`, `.ignore`, `.git/info/exclude` and the global gitignore
  are native).
