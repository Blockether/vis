# Vis CLI Specification

Status: proposal  
Scope: command-line UX, TUI entrypoints, session/model/provider ergonomics  
Goal: make Vis feel like a daily terminal coding agent, while preserving Vis-specific runtime/state architecture.

---

## 1. Design principles

### 1.1 No ceremony for common paths

Common actions must be one command:

```bash
vis                         # open TUI
vis "fix failing tests"      # open TUI, send prompt
vis -p "explain this repo"   # print answer, exit
vis "fix bug"                # one-shot agent, writes files, exits
```

Current Vis exposes internal structure (`channels tui`, `sessions`, `run --persist`). Proposed CLI exposes user intent first.

### 1.2 Default interactive, explicit automation

- `vis` starts interactive TUI.
- `vis "prompt"` starts TUI and submits prompt.
- `vis -p "prompt"` is answer-only print mode.
- `vis "prompt"` is autonomous one-shot mode.

### 1.3 Short flags mirror other agents

Use expected short flags:

| Flag | Meaning |
|---|---|
| `-p` | print and exit |
| `-c` | continue latest session |
| `-r` | resume with picker |
| `-s ID` | resume session id/prefix |
| `-m MODEL` | model |
| `-y` | approve safe actions |
| `-n` | dry/read-only |

### 1.4 Keep power visible, not mandatory

Vis has unique runtime state: SCI vars, SQLite history, transcripts, extension tools. Surface that under `memory`, `state`, `transcript`, `tools`, not in first-run command paths.

---

## 2. Top-level command shape

```text
vis - coding agent with persistent runtime state

Usage:
  vis [options] [@files...] [prompt]
  vis <command> [args...]

Common:
  vis                         Open TUI
  vis "prompt"                Open TUI and send prompt
  vis -p "prompt"             Print answer and exit
  vis resume                  Resume session picker
  vis models                  Pick/list models
  vis login                   Authenticate provider
  vis doctor                  Diagnose setup

Commands:
  tui          terminal UI
  resume       resume session picker
  sessions     list/search/resume/fork/export sessions
  models       list/select models
  model        set active model
  login        authenticate provider
  logout       clear provider credentials
  providers    auth/status/limits/provider metadata
  tools        list runtime/tool surface
  memory       inspect persistent runtime state
  transcript   print/export transcript
  config       view/edit config
  extensions   list/install/scaffold/doctor extensions
  doctor       diagnose environment
  update       update Vis
```

---

## 3. Mode selection

### 3.1 Default: TUI

```bash
vis
```

Opens TUI in current working directory.

Equivalent:

```bash
vis tui
```

### 3.2 TUI with initial prompt

```bash
vis "fix failing tests"
vis @src/foo.clj "review this file"
```

Behavior:

1. Create/resume session according to flags.
2. Attach referenced files.
3. Open TUI.
4. Submit initial prompt.
5. Keep TUI open for follow-up.

### 3.3 Print mode

```bash
vis -p "summarize this repo"
vis --print @README.md "summarize"
cat error.log | vis -p "explain failure"
```

Behavior:

- Runs one turn.
- Prints final answer only by default.
- Does not persist unless `--persist` or `--session` used.
- Clean output for pipes.

### 3.4 Run mode

```bash
vis "fix lint and run tests"
vis --yes "update docs"
vis --dry "plan migration"
```

Behavior:

- Runs autonomous one-shot workflow.
- Can edit files unless read-only/dry mode.
- Exits with meaningful status code.
- Best for scripts/CI/batch work.

---

## 4. Global flags

```text
-p, --print              Print final answer and exit.
-c, --continue           Continue latest session.
-r, --resume             Open session picker.
-s, --session ID         Resume session by id/prefix.
--fork ID                Fork session by id/prefix.
--no-session             Ephemeral; do not persist.
--persist                Persist print/run mode.
-m, --model MODEL        Model id or provider/model.
--provider PROVIDER      Provider id.
--thinking LEVEL         off|minimal|low|medium|high|xhigh.
--cwd DIR                Run as if invoked from DIR.
--json                   JSON final envelope.
--jsonl                  JSON events stream.
--edn                    EDN final envelope.
--trace                  Human-readable trace.
--trace-json             JSONL trace.
--raw                    Plain text output.
-y, --yes                Auto-approve safe operations.
-n, --dry                No writes; planning/read-only.
--readonly               Read/search only; no edits or mutating bash.
--tools LIST             Allowlist tools.
--no-tools               Disable all tools.
--no-builtin-tools       Disable builtin tools, keep extension tools.
--help, -h               Help.
--version, -v            Version.
```

---

## 5. File and stdin input

### 5.1 File references

```bash
vis @src/foo.clj "review"
vis @src/foo.clj @test/foo_test.clj "fix failing test"
vis -p @screenshot.png "what is wrong?"
```

Rules:

- `@path` attaches file to initial prompt.
- `@` in TUI opens fuzzy file picker.
- Binary/image support depends on provider capability.
- Missing file causes usage error with suggestions.

### 5.2 Stdin

```bash
cat README.md | vis -p "summarize"
git diff | vis -p "review diff"
clojure -M:test 2>&1 | vis -p "diagnose"
```

Rules:

- In print/run mode, stdin is appended as context.
- In TUI mode, piped stdin becomes initial attached context, then TUI opens.
- Large stdin gets summarized/chunked with explicit notice.

---

## 6. Session commands

### 6.1 Quick session flags

```bash
vis -c                         # continue latest in TUI
vis -c "continue fix"           # continue latest and submit prompt
vis -s 42d580bb                 # resume by id/prefix
vis --fork 42d580bb             # fork and open TUI
```

### 6.2 `resume`

```bash
vis resume
vis r                           # alias
```

Opens fuzzy session picker.

### 6.3 `sessions`

```bash
vis sessions
vis sessions list
vis sessions search "sqlite"
vis sessions show 42d580bb
vis sessions fork 42d580bb
vis sessions delete 42d580bb
vis sessions export 42d580bb --md
vis sessions export 42d580bb --html out.html
```

Output example:

```text
Sessions

ID        Title                         Turns  Channel  Updated
42d580bb  Fix TUI copy regression       12     tui      2026-05-18 02:12
94b2768b  Explain provider auth         1      cli      2026-05-17 23:28
3d820e72  smoke                         1      tui      2026-05-17 23:27

Use: vis -s <id>   vis sessions fork <id>   vis sessions search <query>
```

### 6.4 Rationale

Current:

```bash
vis channels tui --session-id 42d580bb
vis sessions --fork 42d580bb
```

Proposed:

```bash
vis -s 42d580bb
vis fork 42d580bb
```

Session should be first-class, not buried under channel plumbing.

---

## 7. Model and provider commands

### 7.1 Login/logout

```bash
vis login
vis login openai
vis login anthropic
vis logout openai
```

`vis login` opens provider picker and runs provider-owned auth flow.

### 7.2 Models

```bash
vis models                     # interactive picker/list
vis models list
vis models search sonnet
vis models refresh
vis model anthropic/claude-sonnet-4
vis model sonnet:high
```

Output example:

```text
Models

ACTIVE  PROVIDER   MODEL                         THINKING  AUTH
*       anthropic  claude-sonnet-4-20250514      high      yes
        openai     gpt-4o                        -         no
        ollama     qwen2.5-coder                 -         local

Use: vis model <provider/model>    vis login <provider>
```

### 7.3 Providers

```bash
vis providers
vis providers list
vis providers status
vis providers status openai-codex
vis providers limits
vis providers auth github-copilot-business
vis providers logout openai-codex
```

Keep current strong provider diagnostics. Improve naming around model list and login.

---

## 8. One-shot root command

### 8.1 Usage

```text
vis [options] [@files...] <prompt>
```

### 8.2 Examples

```bash
vis "fix failing tests"
vis --yes "format code and update snapshots"
vis --dry "plan sqlite migration"
vis --readonly "review auth code"
vis --tools read,rg,bash "investigate slow startup"
vis --json "report status"
vis --trace "debug agent loop"
vis --code "write SQL migration"
```

### 8.3 One-shot flags

```text
-y, --yes                 Auto-approve safe operations.
-n, --dry                 No writes; plan/read only.
--readonly                Read/search only.
--check                   Run verification after changes.
--no-check                Do not auto-run verification.
--max-turns N             Stop after N assistant turns.
--timeout DURATION        Stop after duration.
--code                    Print only code blocks from final answer.
--title TITLE             Session title.
```

### 8.4 Exit codes

| Code | Meaning |
|---:|---|
| 0 | success |
| 1 | agent/runtime failure |
| 2 | usage/config error |
| 3 | verification failed |
| 4 | cancelled |
| 5 | provider/auth/rate-limit failure |

---

## 9. TUI spec

### 9.1 Startup header

```text
vis 0.3.0  cwd ~/repo  model anthropic/sonnet:high  provider ok
session new  tokens 12k/200k  cost $0.00  sqlite ok

AGENTS.md loaded
Tools: read rg patch bash repl web
Shortcuts: Ctrl+K menu  Ctrl+L model  Ctrl+G sessions  Tab plan/build
```

### 9.2 Layout

```text
┌────────────────────────────────────────────────────────────────────────────┐
│ Vis  cwd ~/repo  model anthropic/sonnet:high  provider ok  quota 94% left │
├────────────────────────────────────────────────────────────────────────────┤
│ user                                                                       │
│   Fix failing tests                                                        │
│                                                                            │
│ assistant                                                                  │
│   I found one failing assertion...                                         │
│                                                                            │
│ tool bash                                                                  │
│   clojure -M:test                                                          │
│   exit 1                                                                   │
│                                                                            │
│ diff                                                                       │
│   test/foo_test.clj                                                        │
│   - old                                                                    │
│   + new                                                                    │
├────────────────────────────────────────────────────────────────────────────┤
│ > type message...                                                          │
├────────────────────────────────────────────────────────────────────────────┤
│ Ctrl+K menu  Ctrl+L model  Ctrl+G sessions  Esc cancel  Shift+Enter newline│
└────────────────────────────────────────────────────────────────────────────┘
```

### 9.3 Editor behavior

| Input | Action |
|---|---|
| `Enter` | send |
| `Shift+Enter` / `Alt+Enter` | newline |
| `@` | file picker |
| `!cmd` | run shell, attach output |
| `/` | slash command autocomplete |
| `Ctrl+K` | command palette |
| `Ctrl+L` | model picker |
| `Ctrl+G` | session picker |
| `Ctrl+B` | voice record toggle |
| `Ctrl+T` | cycle model or thinking, depending setting |
| `Esc` | cancel current turn; if draft non-empty, clear draft first |
| `Ctrl+C` | clear draft; second on empty exits |

### 9.4 Slash commands

```text
/help
/model
/provider
/session
/resume
/fork
/export
/settings
/doctor
/tools
/memory
/state
/transcript
/compact
/undo
/redo
/plan
/build
/yes
/no
```

### 9.5 Command palette

`Ctrl+K` opens fuzzy action list:

```text
Command Palette

> mod

  Model: switch model                 Ctrl+L
  Model: cycle next                   Ctrl+T
  Provider: status
  Providers: limits
  Session: resume                     Ctrl+G
  Session: fork
  Session: copy as Markdown
  Settings: open
```

### 9.6 Plan/build mode

```text
Mode: PLAN   tools: read/search only   writes blocked
Mode: BUILD  tools: read/search/edit/bash allowed
```

- `Tab` toggles Plan/Build.
- Plan mode cannot mutate files.
- Build mode can request approvals.

### 9.7 Approvals

Bad approval:

```text
Allow bash?
```

Required approval:

```text
Approve command?
  clojure -M:test

Reason: run regression tests
Risk: reads source, writes target/ only

[y] yes   [n] no   [a] always allow test commands this session
```

Approval must show:

- exact command or file edit summary
- reason
- risk
- scope of approval

---

## 10. Runtime/state commands

Vis differs from generic agents because model can maintain runtime state in SCI and SQLite. CLI should expose this advantage.

### 10.1 Tools

```bash
vis tools
vis tools list
vis tools show v/patch
vis tools doctor
```

Output example:

```text
Tools

NAME       SOURCE       MUTATES  DESCRIPTION
v/cat      foundation   no       read files
v/rg       foundation   no       search text
v/patch    foundation   yes      exact text patch
exa/web    exa          no       web search
```

### 10.2 Memory/state

```bash
vis memory
vis memory vars
vis memory show last-plan
vis memory rm last-plan
vis state
vis state show <session-id>
```

Output example:

```text
Runtime State for 42d580bb

VARS
  last-plan          map     2.1 KB
  failing-tests      vector  812 B
  repo-summary       string  4.4 KB

Use: vis memory show <name>
```

### 10.3 Transcript

```bash
vis transcript 42d580bb
vis transcript 42d580bb --md
vis transcript 42d580bb --json
vis transcript 42d580bb --grep sqlite
```

---

## 11. Config commands

```bash
vis config
vis config path
vis config edit
vis config get model
vis config set model anthropic/claude-sonnet-4
vis config set tui.theme gruvbox
vis config doctor
```

Output example:

```text
Config

Global:  ~/.vis/config.edn
Project: .vis/config.edn

active provider: anthropic
active model:    claude-sonnet-4-20250514
TUI theme:       default
```

---

## 12. Extensions

```bash
vis extensions
vis extensions list
vis extensions install github:user/repo
vis extensions install ./my-ext
vis extensions remove my-ext
vis extensions scaffold my-tools
vis extensions doctor
```

Current `vis extensions list` table is useful but too wide. Default should be compact:

```text
Extensions

ID           KIND       STATUS  DESCRIPTION
foundation   tools      ok      file/repl/render helpers
sqlite       storage    ok      SQLite persistence
anthropic    provider   ok      Anthropic auth/provider
exa          search     ok      Exa web/code search
voice        voice      ok      TTS/ASR

actions: vis extensions show <id>   vis extensions doctor
```

Detailed metadata stays under:

```bash
vis extensions show foundation
vis extensions list --verbose
```

---

## 13. Doctor

```bash
vis doctor
vis doctor --fix
vis doctor providers
vis doctor tui
vis doctor extensions
```

Doctor should check:

- Java/Clojure versions
- config paths
- DB availability
- provider auth
- token expiry
- extension load failures
- TUI terminal capabilities
- clipboard helpers
- voice binaries/models
- project guidance files

Output style:

```text
vis doctor

OK   Java 25.0.1
OK   SQLite ~/.vis/vis.mdb
OK   AGENTS.md loaded from repo
WARN OpenAI unauthenticated
ERR  extension foo failed to load

Summary: 1 error, 1 warning, 3 ok
```

---

## 14. Update

```bash
vis update
vis update self
vis update extensions
vis update extension exa
```

Rules:

- Show current version and target version.
- Ask confirmation unless `--yes`.
- Never hide failed update logs.

---

## 15. Current vs proposed mapping

| Current | Proposed | Reason |
|---|---|---|
| `vis` shows help | `vis` opens TUI | matches agent CLI expectation |
| `vis help` | `vis help` | unchanged |
| no root prompt | `vis "prompt"` | removes ceremony |
| no `-p` | `vis -p "prompt"` | expected print mode |
| `vis channels tui` | `vis tui` / `vis` | hides channel plumbing |
| `vis channels tui --session-id ID` | `vis -s ID` | session first-class |
| `vis channels tui --resume` | `vis -c` / `vis resume` | expected resume UX |
| `vis sessions` | `vis sessions` | user-facing term |
| `vis sessions search Q` | `vis sessions search Q` | session namespace |
| `vis sessions --fork ID` | `vis sessions fork ID` / `vis fork ID` | direct action |
| `vis providers auth P` | `vis login P` | common auth verb |
| `vis providers logout P` | `vis logout P` | common auth verb |
| no model command | `vis models`, `vis model` | model UX parity |
| `vis extensions list` wide table | compact `vis extensions`, verbose details | readability |

---

## 16. Implementation phases

### Phase 1: aliases and entrypoint ergonomics

- `vis` -> TUI by default.
- `vis help` keeps help.
- `vis "prompt"` -> TUI with prompt.
- `vis -p "prompt"` -> `vis --raw/print` equivalent.
- Add `-c`, `-r`, `-s`, `-m` global aliases.

### Phase 2: session simplification

- Add `vis sessions` namespace.
- Keep `vis sessions` as compatibility alias.
- Add `vis resume`, `vis fork` shortcuts.
- Make session picker available from TUI and CLI.

### Phase 3: input ergonomics

- Add stdin support.
- Add `@file` CLI attachments.
- Add missing-file suggestions.
- Add `--cwd`.

### Phase 4: model/provider polish

- Add `vis login/logout` aliases.
- Add `vis models` and `vis model`.
- Compact provider/model tables.

### Phase 5: TUI command palette and slash commands

- Make `Ctrl+K` primary discovery surface.
- Add `/help`, `/model`, `/session`, `/tools`, `/memory`, `/state`.
- Add plan/build toggle if permissions exist.

### Phase 6: power-user state surfaces

- Add `vis memory`, `vis state`, `vis transcript`.
- Make Vis runtime-state advantage visible.

---

## 17. Compatibility policy

Do not break existing scripts immediately.

Keep aliases:

```bash
vis channels tui
vis channels tui --session-id ID
vis sessions
vis sessions search Q
vis sessions --fork ID
vis providers auth P
vis providers logout P
```

Print deprecation hints only in TTY:

```text
Hint: `vis channels tui --session-id ID` can be shortened to `vis -s ID`.
```

No deprecation hints in JSON/EDN/pipe output.

---

## 18. Why this differs from current Vis

Current Vis CLI is accurate to architecture:

- channels
- sessions
- extensions
- run
- providers

Proposed Vis CLI is accurate to user tasks:

- chat/work
- print
- resume
- model
- login
- inspect state
- configure

Architecture remains available, but does not dominate first-run UX.

---

## 19. Success criteria

User should be able to guess these without docs:

```bash
vis
vis "fix this"
vis -p "what is this repo?"
vis -c
vis -s abc123
vis models
vis login
vis doctor
```

Power user should still have:

```bash
vis --trace-json --session abc123 --tools read,rg,bash "investigate"
vis memory show last-plan
vis transcript abc123 --json
vis extensions scaffold my-tool
```

If both work, CLI has right shape.
