# Coding Agent Harness Comparison

**Pi · OpenCode · Hermes Agent · Vis (svar)**

*Last updated: 2026-04-17*

---

## Executive Summary

| Dimension | Pi | OpenCode | Hermes Agent | Vis (svar) |
|---|---|---|---|---|
| **Language** | TypeScript/Node.js | Go | Python | Clojure (JVM) |
| **Stars / Maturity** | ~30k+ | ~140k+ (archived v1; reborn) | ~95k+ | Private / Pre-release |
| **Harness Model** | LLM ↔ tool-call loop (standard) | LLM ↔ tool-call loop (standard) | LLM ↔ tool-call loop (standard) | **Deterministic SCI sandbox loop** (code-eval, not tool-call) |
| **Extensibility** | TS extensions, skills, prompt templates, packages | MCP, LSP, custom agents, custom commands | 40+ tools, MCP, skills, hooks, plugins | SCI bindings, skills (SKILL.md), sub-rlm-query, hooks |
| **Memory / Persistence** | JSONL session tree | SQLite sessions | 5-layer memory (skills, FTS5, Honcho, MEMORY.md) | SQLite entity tree (conversation → query → iteration → var) |
| **Determinism** | Non-deterministic (LLM chooses tools) | Non-deterministic (LLM chooses tools) | Non-deterministic (LLM chooses tools) | **Semi-deterministic** (LLM emits Clojure; sandbox evaluates deterministically) |
| **Multi-frontend** | CLI only (SDK for embedding) | TUI, Desktop, Web, IDE | CLI, Telegram, Discord, Slack, WhatsApp, Signal, Email | TUI, Web, CLI, Telegram |

---

## 1. Architecture Deep-Dive

### 1.1 Pi — Minimal Extensible Harness

**Core philosophy:** "Aggressively extensible so it doesn't have to dictate your workflow." Pi is deliberately minimal — no sub-agents, no plan mode, no permission popups, no MCP baked in. Everything is delegable to extensions.

**Agent loop:**
```
User → prompt() → Agent (LLM call) → tool_calls[] → execute tools → append results → loop
```

The loop is the standard LLM-driven tool-calling pattern: the model emits structured `tool_calls`, Pi executes them via registered tool handlers, appends results as `tool` messages, and loops until the model stops emitting tool calls.

**Tool system:** Four built-in tools: `read`, `write`, `edit`, `bash`. Tools are standard function-call schema objects. Extensions can register additional tools or replace built-ins entirely via `pi.registerTool()`.

**Key differentiator:** The TypeScript extension API is the richest of the four systems. Extensions can:
- Intercept and block/modify any tool call before execution (`tool_call` event)
- Inject context at multiple lifecycle points
- Replace the TUI editor, add widgets, overlays, custom rendering
- Register custom compaction strategies
- Store persistent state in the session file
- Provide full custom UI components with keyboard input via `ctx.ui.custom()`

**Session model:** JSONL files with a tree structure (`id`/`parentId`). Enables in-place branching — navigate to any prior point with `/tree` and continue from there. All history preserved in a single file. Compaction is supported (manual and automatic) with full history preserved in the JSONL for later traversal.

**What Pi does NOT do:**
- No sandboxed code execution — `bash` tool runs directly on the host
- No deterministic execution layer — tool outcomes depend entirely on host state
- No built-in learning loop — no skill auto-creation
- No cross-session memory (by design — extensions can add it)

---

### 1.2 OpenCode — Go-Based Multi-Agent TUI

**Core philosophy:** Provider flexibility and "the open source AI coding agent." Ships free models, supports 75+ providers, LSP integration, MCP protocol, and multiple frontends (TUI, desktop, web, IDE extension).

**Agent loop:**
```
User → agent.Run() → LLM stream → tool_calls[] → execute (with permission) → append → loop
```

Standard tool-calling loop implemented in Go. Three specialized agent types:
- **Coder** — main coding agent, full tool set
- **Task** — read-only sub-agent for code search/analysis
- **Title** — internal summarization agent (no tools)
- **Summarizer** — context compression agent

**Tool system:** ~15 built-in tools organized by category:
- File tools: `glob`, `grep`, `ls`, `view`, `write`, `edit`, `patch`
- Shell tools: `bash`, `fetch`
- Code intelligence: `diagnostics`, `lsp_definition`, `lsp_references`, `lsp_diagnostics`
- Meta: `agent` (sub-agent), `sourcegraph`

Unique: `edit` tool requires the file to have been `read` first (permission tracking). Diff-based output with additions/removals counts. LSP integration provides real code intelligence to the LLM.

**MCP integration:** First-class. Dynamically discovers tools from configured MCP servers. Supports stdio and SSE transports.

**LSP integration:** Unique among these four. Spawns language server subprocesses (gopls, tsserver, etc.) and exposes their capabilities as tools. The LLM gets go-to-definition, find-references, diagnostics — actual code intelligence, not just grep.

**Session model:** SQLite-backed. Sessions with messages, tool results, and metadata. Supports `--continue` to resume.

**Architecture pattern:** Clean Go modular design — `cmd/` (CLI), `internal/app/` (DI/wiring), `internal/llm/` (agent/provider/tools), `internal/tui/` (Bubble Tea UI), `internal/db/` (sqlc queries), `internal/lsp/` (LSP clients).

**What OpenCode does NOT do:**
- No sandboxed execution — bash runs on host
- No learning loop or procedural memory
- No deterministic harness — standard tool-call loop
- No cross-session state transfer
- Archived original repo; reborn under opencode.ai with significant changes

---

### 1.3 Hermes Agent — Self-Improving Persistent Agent

**Core philosophy:** "The agent that grows with you." Server-side persistent agent with a closed learning loop. Not tied to your laptop — runs on VPS, GPU cluster, or serverless. Multi-platform gateway (Telegram, Discord, Slack, WhatsApp, Signal, Email).

**Agent loop (run_agent.py — ~10,700 lines):**
```
User → chat() → run_conversation() → build system prompt → LLM call
  → tool_calls? → execute (sequential or concurrent via ThreadPool) → append → loop
  → no tool_calls? → check for final → compress if needed → return
```

Supports three API execution modes: `chat_completions` (OpenAI), `codex_responses` (Responses API), `anthropic_messages`. Mode determines message formatting, tool call structure, and response parsing.

**Tool system:** 40+ built-in tools organized into composable toolsets:
```python
TOOLSETS = {
    "web": ["web_search", "web_extract"],
    "terminal": ["terminal", "process"],
    "files": ["read_file", "write_file", "patch", "search_files"],
    "vision": ["vision_analyze"],
    "skills": ["skill_search", "skill_create", "skill_manage"],
    "memory": ["memory_search", "memory_save", "best_practices"],
    "session_history": ["session_search"],
    "code_execution": ["execute_code"],
    "delegation": ["delegate_task"],
    ...
}
```

Toolsets are composable (`"includes"` key) and platform-specific (`hermes-cli`, `hermes-telegram`, etc.). Plugin-registered toolsets integrate seamlessly.

**5-Layer Memory Architecture (unique):**
1. **Short-term context** — standard in-context window, compressed at 50% utilization
2. **Procedural skill documents (SKILL.md)** — autonomous creation after complex tasks, follows agentskills.io standard, FTS5-searchable, self-improving during use
3. **Persistent memory (MEMORY.md)** — agent-curated key facts, flushed on session end
4. **User modeling (Honcho)** — dialectic reasoning from Plastic Labs, builds evolving user profile
5. **Session search (FTS5)** — SQLite full-text search across all past conversations with LLM summarization

**The Closed Learning Loop:**
```
Task completed → complexity heuristic → SKILL.md created
  → next similar task → FTS5 retrieves skill → agent starts from scaffold
  → skill improved during use → nudge to persist knowledge
  → Honcho observes → user model updated → personalized future sessions
```

**Terminal backends (6):** Local, Docker, SSH, Daytona, Singularity, Modal. Enables running on remote infrastructure.

**Research tooling:** Batch trajectory generation, Atropos RL environments, trajectory compression for training. Built for Nous Research's own model training pipeline.

**What Hermes does NOT do:**
- No deterministic code execution sandbox — tools execute on host/container
- No structured code-eval loop — standard tool-call pattern
- No entity-level persistence (vars, intermediate state) — session-level granularity
- Skill creation trigger is undocumented (complexity heuristic) — unpredictable
- No built-in LSP integration

---

### 1.4 Vis (svar) — Deterministic SCI Sandbox Agent

**Core philosophy:** Deterministic code-eval loop with structured persistence. The LLM doesn't choose tools — it writes Clojure code that executes in a sandboxed SCI (Small Clojure Interpreter) environment. Tools are Clojure functions bound into the sandbox namespace.

**Agent loop (iteration_loop):**
```
User → query-env! → build system prompt → LLM emits JSON: {thinking, code[], final?}
  → validate syntax (edamame parse) → paren-repair if broken
  → execute each code block in SCI sandbox with timeout
  → capture results, stdout, stderr per block
  → build <execution_results> + <var_index> + <execution_journal>
  → inject as next user message → loop
  → final? → validate answer → return
```

**This is fundamentally different from the other three.** The LLM doesn't emit tool names and parameters — it emits actual Clojure expressions that execute deterministically in SCI. The iteration spec is provider-enforced JSON structured output (`ITERATION_SPEC_REASONING` / `ITERATION_SPEC_NON_REASONING`), not free-form text with tool_calls.

**Key architectural components:**

**SCI Sandbox (`loop.sci.runtime`):**
- Full Clojure sandbox with `clojure.string`, `clojure.set`, `clojure.walk`, `fast-edn.core`, `zprint.core`, `lazytest.core`
- Tools are regular Clojure functions bound as SCI vars: `search-documents`, `fetch-document-content`, `git-blame`, `git-commit-diff`, `sub-rlm-query`, etc.
- User `(def ...)` forms persist as SCI vars — state accumulates across iterations within a query
- `build-var-index` renders a compact `<var_index>` block showing all user-defined vars to the LLM

**Paren Repair (`loop.paren_repair`):**
- Inspired by clojure-mcp-light (Bruce Hauman)
- Tokenizer-based: reader state machine identifies delimiters outside strings/comments/char literals
- Two-phase: delimiter repair (local swap mismatches) → paren repair (stack balancing, append missing closers)
- Recovers from LLM bracket errors without forcing re-emission

**Deterministic Auto-Forget:**
- At query boundary: vars without docstrings that haven't been touched in the last N queries are automatically evicted from the sandbox
- DB rows untouched — `(restore-var 'sym)` can bring them back
- Replaces the unreliable "ask LLM to emit :forget" pattern

**Repetition Detection:**
- Tracks `[code, result-str]` pairs across iterations
- Fires warning after 3+ identical call→result pairs
- Forces the LLM to try a different approach

**Strategy Restart:**
- After `max-consecutive-errors` failures: resets with "anti-knowledge" (summary of what failed)
- Up to `max-restarts` times before giving up
- Each restart re-injects the original requirement with error context

**Adaptive Reasoning Effort:**
- Maps consecutive error count to reasoning effort level
- 0 errors → base level; 1 error → medium; 2+ → high
- Providers with native reasoning (thinking tokens) get effort escalation on failures

**Error Hints:**
- Pattern-matched error messages get actionable hints injected into execution results
- "Unable to resolve symbol: X" → "'X' is not defined. (def X ...) or check spelling."
- "Wrong number of args" → specific guidance based on target type
- "cannot be cast to IFn" → "You're calling a non-function. Bare (1 2 3) calls 1 as fn."

**Entity Tree Persistence (`loop.persistence.db`):**
```
:conversation
  └─ :query (one per user turn)
       └─ :iteration (one per LLM iteration)
            └─ :iteration-var (one per `(def ...)` in that iteration)
```

Every iteration is stored with: executions, vars snapshot, thinking, answer, duration. This is trajectory-grade data — every intermediate step is persisted for replay, fine-tuning, and debugging.

**Skills System (`loop.skills`):**
- Compatible with agentskills.io standard (same as Pi and Hermes)
- Discovers from `.svar/skills/`, `.claude/skills/`, `.opencode/skills/`, `.agents/skills/`
- **RLM can create/patch/refine/delete skills** via `skill-manage` SCI tool
- Skills ingested into SQLite as searchable documents (same search path as all other docs)
- Content-hash based change detection for re-ingestion

**Multi-frontend architecture:**
- **Adapters:** TUI (blessed/React), Web (Jetty/HTMX), CLI (one-shot), Telegram (bot)
- All share one SQLite DB (`~/.vis/vis.mdb`)
- Conversations are channel-tagged (`:vis`, `:telegram`, `:cli`)
- Same `conversations/send!` API regardless of frontend

**Sub-queries (`sub-rlm-query` / `sub-rlm-query-batch`):**
- SCI-callable function for the LLM to spawn nested LLM queries
- Batch variant runs in parallel via Clojure futures with reentrant semaphore for HTTP slot control
- Deadline propagation: child never exceeds parent's timeout
- Depth-limited recursion (configurable `max-recursion-depth`)

---

## 2. Deterministic Harness Approaches

This is where the systems diverge most fundamentally.

### 2.1 Pi / OpenCode / Hermes: Tool-Call Pattern

All three use the standard LLM tool-calling protocol:

```
LLM → {"tool_calls": [{"name": "bash", "arguments": {"command": "ls"}}]}
Harness → execute tool → return result string
LLM → next response (possibly more tool_calls)
```

**Properties:**
- ✅ Works with any LLM that supports function calling
- ✅ Easy to add new tools (just register a handler)
- ✅ Familiar pattern — every provider supports it
- ❌ **Non-deterministic at the harness level** — given the same prompt, different models produce different tool call sequences
- ❌ **Tool call overhead** — each tool invocation is a separate round-trip in the LLM's output
- ❌ **No intermediate state** — between tool calls, the only state is the conversation history
- ❌ **Composability limited to text** — tool results are strings; no structured data flow between tool calls within a single turn

**Mitigation strategies:**
- Pi: Extensions can intercept tool calls, inject context, modify results — but the core loop is still tool-call driven
- OpenCode: LSP integration adds deterministic code intelligence, but the agent loop is standard
- Hermes: Concurrent tool execution via ThreadPoolExecutor when multiple tools are called, but still LLM-directed

### 2.2 Vis: Code-Eval Sandbox Pattern

Vis uses a fundamentally different approach:

```
LLM → {"code": [{"expr": "(def data (search-documents \"auth\"))", "time-ms": 2000},
                 {"expr": "(assert (seq data) \"No results\")", "time-ms": 500}],
        "thinking": "Need to find auth-related code"}
Harness → parse with edamame → paren-repair if needed → eval in SCI sandbox
        → capture result/stdout/stderr per block → build execution receipt
LLM → sees <execution_results> + <var_index> → next iteration
```

**Properties:**
- ✅ **Deterministic at the execution layer** — same Clojure code always produces the same result in SCI
- ✅ **Structured state accumulation** — `(def ...)` forms create persistent vars visible via `<var_index>`
- ✅ **Composability within a turn** — multiple code blocks in one iteration can build on each other
- ✅ **Pre-execution validation** — syntax checking, lint, paren repair before eval
- ✅ **Timeout per code block** — each `{:expr ... :time-ms N}` gets its own timeout budget
- ✅ **Trajectory-grade persistence** — every code block + result stored for replay/training
- ❌ **Requires Clojure** — the LLM must emit valid Clojure (mitigated by paren repair and error hints)
- ❌ **Higher token overhead for system prompt** — Clojure syntax rules, tool docs, var index
- ❌ **Not provider-native** — uses structured output spec instead of native tool_calls
- ❌ **Model familiarity** — most models are better at Python/JS tool calls than Clojure code generation

**Unique determinism features:**
1. **Auto-forget** — deterministic var cleanup at query boundary based on recency, not LLM request
2. **Paren repair** — recovers from LLM bracket errors without retry
3. **Strategy restart** — deterministic reset with anti-knowledge after N consecutive errors
4. **Repetition detection** — deterministic detection of repeated call→result pairs
5. **Bare string detection** — rejects prose in `:code` blocks (must go in `:answer`)
6. **Validation before finalization** — code answers must pass syntax parse; untested code rejected

---

## 3. Extensibility Comparison

### 3.1 Tool Registration

| Aspect | Pi | OpenCode | Hermes | Vis |
|---|---|---|---|---|
| **Mechanism** | `pi.registerTool()` in TS extensions | Go interface + registration in `internal/llm/tools/` | Python `tools/registry.py` + decorator | SCI `sci-update-binding!` / `register-env-fn!` |
| **Hot-reload** | `/reload` command | No (recompile) | Plugin system, runtime re-registration | Dynamic SCI binding update |
| **Schema format** | TypeBox (`@sinclair/typebox`) | JSON Schema (Go struct tags) | JSON Schema (Python dicts) | Clojure maps (:doc, :params, :returns) |
| **Replace built-ins** | ✅ Yes (extensions can override) | ❌ Requires code change | ✅ Via registry override | ✅ Rebind SCI vars |
| **Access to UI** | ✅ Full TUI access via `ctx.ui` | ❌ No extension UI API | ✅ Callbacks (CLI, gateway) | ❌ Tools are pure functions |

### 3.2 Extension / Plugin Model

| Aspect | Pi | OpenCode | Hermes | Vis |
|---|---|---|---|---|
| **Extension format** | TypeScript modules | None (MCP servers) | Python plugins + hooks | Clojure functions in SCI |
| **Event system** | Rich lifecycle events (session_start, tool_call, agent_start, etc.) | Pub/sub broker (AgentEvent) | pre/post tool_call hooks, callbacks | Hook fns (:on-chunk, :on-iteration, :on-cancel) |
| **Package system** | npm/git packages with manifest | MCP servers | Skills Hub, pip packages | Skills (SKILL.md files) |
| **Shareable** | ✅ Pi Packages via npm/git | ✅ MCP servers | ✅ Skills Hub + pip | ✅ SKILL.md files (agentskills.io) |
| **Can modify core loop** | ✅ Via event interception | ❌ | ✅ Via hooks/plugins | ✅ Via SCI bindings and hooks |

### 3.3 Skills System

All four support the agentskills.io standard (Pi, Hermes natively; OpenCode and Vis with compatibility):

| Aspect | Pi | OpenCode | Hermes | Vis |
|---|---|---|---|---|
| **Discovery** | `~/.pi/agent/skills/`, `.pi/skills/`, `.agents/skills/` | `.opencode/skills/` | `~/.hermes/skills/` | `.svar/skills/`, `.claude/skills/`, `.opencode/skills/`, `.agents/skills/` |
| **Loading** | Progressive disclosure (descriptions in prompt, full SKILL.md on demand via `read`) | Context file injection | FTS5 search + preload | Ingested into SQLite as searchable documents |
| **Auto-creation** | ❌ (ask Pi to build one) | ❌ | ✅ Autonomous after complex tasks | ✅ RLM can create/patch/refine/delete via `skill-manage` |
| **Self-improvement** | ❌ | ❌ | ✅ Skills improve during use | ✅ `skill-manage :patch` / `:refine` |
| **Cross-agent compat** | ✅ Reads from Claude/Codex dirs | ✅ Reads from standard dirs | ✅ Imports from OpenClaw | ✅ Reads from Claude/OpenCode/agents dirs |

### 3.4 Sub-Agents / Delegation

| Aspect | Pi | OpenCode | Hermes | Vis |
|---|---|---|---|---|
| **Built-in** | ❌ (use extensions or spawn pi instances via tmux) | ✅ `agent` tool (Task sub-agent) | ✅ `delegate_task` (spawns isolated subagents) | ✅ `sub-rlm-query` / `sub-rlm-query-batch` |
| **Parallelism** | Manual (tmux) | Single sub-agent | ThreadPoolExecutor | Clojure futures + reentrant semaphore |
| **Context isolation** | Process isolation | Separate agent type (read-only tools) | Isolated context per subagent | Depth-limited recursion, deadline propagation |
| **Batch** | ❌ | ❌ | ❌ | ✅ `sub-rlm-query-batch` with slot control |

---

## 4. Persistence & State Model

### 4.1 Session / Conversation Format

| Aspect | Pi | OpenCode | Hermes | Vis |
|---|---|---|---|---|
| **Format** | JSONL tree (id/parentId) | SQLite | SQLite + Markdown files | SQLite entity tree |
| **Branching** | ✅ In-place (single file) | ❌ | ❌ | ❌ (linear per conversation) |
| **Cross-session state** | ❌ (extension-managed) | ❌ | ✅ MEMORY.md, skills, user model | ✅ Var registry persists across queries |
| **Granularity** | Message-level | Message-level | Message-level | **Iteration-level** (code + result + vars per step) |
| **Compaction** | ✅ Built-in (lossy, with full history in JSONL) | ✅ Summarizer agent | ✅ Context compressor (50% utilization trigger) | ✅ Journal squash (every 5 iterations) |
| **Replay** | ✅ Navigate tree, re-branch | ✅ Resume session | ✅ Resume by ID or title | ✅ `restore-var` / `restore-vars` from DB |

### 4.2 Entity Model (Vis-specific)

Vis has the richest persistence model of the four:

```
:conversation (system-prompt, model)
  └─ :query (text, answer, status, iterations, duration-ms, messages)
       └─ :iteration (code[], results, thinking, answer, duration-ms)
            └─ :iteration-var (name, value, code)
```

This enables:
- **Fine-tuning data extraction** — every LLM input/output pair with execution results
- **Var replay** — `restore-var 'x` re-binds a previously defined variable from any past iteration
- **Cost tracking** — per-iteration token usage with input/output/reasoning/cached breakdown
- **Q-value reward computation** — confidence + iteration efficiency → reward signal for training

None of the other three systems persist at this granularity.

---

## 5. Provider & Model Support

| Provider | Pi | OpenCode | Hermes | Vis |
|---|---|---|---|---|
| Anthropic | ✅ (API + subscription) | ✅ | ✅ | ✅ (via svar router) |
| OpenAI | ✅ (API + subscription) | ✅ | ✅ | ✅ |
| Google Gemini | ✅ (API + subscription) | ✅ | ✅ | ✅ |
| GitHub Copilot | ✅ (subscription) | ✅ (subscription) | ❌ | ❌ |
| OpenRouter | ✅ | ✅ | ✅ (200+ models) | ✅ |
| Ollama (local) | ✅ (via custom provider) | ✅ | ✅ | ✅ (via router) |
| Free built-in models | ❌ | ✅ (opencode/big-pickle, gpt-5-nano) | ✅ (via Nous Portal) | ❌ |
| Custom endpoints | ✅ (models.json + extensions) | ✅ (providers config) | ✅ (any OpenAI-compatible) | ✅ (svar router config) |
| **Model routing** | Manual (Ctrl+P cycling) | Per-agent model config | Runtime `hermes model` | **Automatic** (svar router: optimize :cost / :quality) |

Vis's svar router deserves special mention: it supports automatic routing where the system prompt specifies optimization hints (`:optimize :cost` or `:optimize :quality`) and the router selects the appropriate model. Sub-queries can use different routing than the main query.

---

## 6. Security & Sandboxing

| Aspect | Pi | OpenCode | Hermes | Vis |
|---|---|---|---|---|
| **Code execution** | Direct bash on host | Direct bash on host (permission prompt) | 6 terminal backends (local, Docker, SSH, Daytona, Singularity, Modal) | **SCI sandbox** (no host access except through bound tools) |
| **Permission model** | None built-in (extensions can add) | Permission service (approve/deny per tool call) | Dangerous command detection + approval callback | Sandbox boundary — only exposed functions callable |
| **File access** | Unrestricted (extensions can gate) | Unrestricted (permission prompts) | Unrestricted (approval for dangerous ops) | Only through `read-file`, `write-file`, `edit-file` if bound |
| **Network access** | Via bash | Via bash + fetch tool | Via terminal + web tools | Only through bound tools (e.g., `sub-rlm-query` → HTTP) |
| **Container isolation** | Manual (user responsibility) | ❌ | ✅ Built-in Docker/Singularity/Modal backends | ❌ (JVM process boundary) |

Vis's SCI sandbox provides the strongest default isolation: the LLM cannot execute arbitrary system commands unless `bash` is explicitly bound as a tool. In practice, the standard tool set includes file operations and search, but not shell access.

---

## 7. Code Intelligence

| Feature | Pi | OpenCode | Hermes | Vis |
|---|---|---|---|---|
| **LSP integration** | ❌ | ✅ (go-to-definition, references, diagnostics) | ❌ | ❌ |
| **Git integration** | ❌ (use bash) | ❌ (use bash) | ❌ (use terminal tool) | ✅ JGit-based (blame, file-history, commit-diff, search-commits, etc.) |
| **Document indexing** | ❌ | ❌ | ❌ | ✅ PageIndex (PDF, Markdown, HTML → pages → FTS5 search) |
| **Entity extraction** | ❌ | ❌ | ❌ | ✅ LLM-powered entity/relationship extraction from documents |
| **Code search** | grep/find tools | `glob`, `grep`, `sourcegraph` | `search_files` tool | `search-documents` (FTS5 over all ingested content) |

---

## 8. Unique Strengths

### Pi
- **Best extension API** — deepest lifecycle hooks, UI replacement, session persistence, event bus
- **Session tree branching** — unmatched for exploring alternative approaches in-place
- **Package ecosystem** — npm/git distribution with manifest, enable/disable, versioning
- **SDK for embedding** — TypeScript SDK with full programmatic control, run modes (interactive, print, RPC)
- **Philosophy** — intentionally missing features become extension opportunities

### OpenCode
- **LSP integration** — only system with real language server protocol support
- **Provider breadth** — 75+ providers, free built-in models, Models.dev powered
- **Multi-frontend** — TUI, desktop app, web UI, IDE extension from the same codebase
- **Go performance** — fast binary, low memory, instant startup
- **ACP protocol** — Agent Client Protocol for editor integration

### Hermes
- **Learning loop** — only system with autonomous skill creation and self-improvement
- **5-layer memory** — most sophisticated memory architecture
- **Multi-platform gateway** — single agent reachable from 10+ messaging platforms
- **Terminal backends** — run anywhere: local, Docker, SSH, Daytona, Singularity, Modal
- **Research tooling** — batch trajectories, Atropos RL, trajectory compression
- **User modeling** — Honcho dialectic reasoning builds evolving user profile

### Vis
- **Deterministic sandbox** — SCI execution guarantees reproducible tool results
- **Iteration-level persistence** — finest-grained trajectory data of any system
- **Paren repair** — recovers from LLM syntax errors without retry
- **Auto-forget** — deterministic var lifecycle management
- **Structured state** — `<var_index>` gives the LLM explicit awareness of all accumulated state
- **Sub-query batching** — parallel nested LLM calls with semaphore-controlled concurrency
- **Q-value rewards** — built-in reward signal computation for reinforcement learning
- **Multi-channel architecture** — one SQLite DB, one API (`conversations/send!`), multiple frontends

---

## 9. Weaknesses & Trade-offs

### Pi
- No built-in memory or learning — every session starts fresh unless extensions provide state
- No code sandboxing — bash runs directly on host
- No sub-agents without extensions — manual tmux or custom extension
- JavaScript ecosystem dependency — Node.js required, potential for dependency bloat
- No structured execution data — tool results are strings, no intermediate state model

### OpenCode
- Archived original repo (Go) → reborn as different product under opencode.ai — community fragmentation risk
- No extension API beyond MCP — cannot intercept/modify core behavior
- No learning loop — sessions don't build on each other
- Go binary means no hot-reload of tools — recompile required
- No session branching — linear history only

### Hermes
- Massive codebase (~10,700 lines in run_agent.py alone) — complexity risk
- Skill creation trigger undocumented — unpredictable when skills are auto-created
- Honcho dependency — external service for user modeling (not fully local)
- No deterministic execution layer — standard tool-call pattern
- No fine-grained persistence — message-level, not iteration-level
- Python performance — slower startup, higher memory than Go

### Vis
- Clojure requirement — models less fluent in Clojure than Python/JS/bash
- Smaller ecosystem — private/pre-release, no package marketplace
- Higher system prompt overhead — Clojure syntax rules, SCI capabilities, var index
- No LSP integration — relies on text search rather than code intelligence
- No built-in container isolation — SCI sandbox is process-level, not OS-level
- No session branching — linear conversation model
- JVM startup time — slower cold start than Go or Node.js

---

## 10. When to Use What

| Use Case | Best Choice | Why |
|---|---|---|
| **Quick one-off coding tasks** | Pi or OpenCode | Minimal setup, fast, broad model support |
| **Custom workflow automation** | Pi | Extension API can build anything |
| **Enterprise with LSP needs** | OpenCode | Only system with real language server support |
| **Persistent personal assistant** | Hermes | Learning loop, multi-platform, runs 24/7 |
| **Research / fine-tuning data** | Vis or Hermes | Vis: iteration-level trajectories; Hermes: batch generation |
| **Deterministic agent execution** | Vis | SCI sandbox guarantees reproducible results |
| **Multi-frontend product** | Vis or OpenCode | Vis: TUI/Web/CLI/Telegram from one DB; OpenCode: TUI/Desktop/Web/IDE |
| **Document analysis / Q&A** | Vis | PageIndex, entity extraction, FTS5 search over PDFs/Markdown |
| **Team with security concerns** | Vis or Hermes | Vis: SCI sandbox; Hermes: Docker/Modal backends |
| **Model-agnostic exploration** | OpenCode or Hermes | OpenCode: 75+ providers; Hermes: 200+ via OpenRouter |

---

## 11. Architectural Patterns Summary

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        TOOL-CALL PATTERN                                │
│  (Pi, OpenCode, Hermes)                                                 │
│                                                                         │
│  LLM ──→ tool_calls[{name, args}] ──→ execute handler ──→ string result│
│    ↑                                                          │         │
│    └──────────── append to messages ──────────────────────────┘         │
│                                                                         │
│  Properties: non-deterministic, string-mediated, provider-native        │
└─────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────┐
│                      CODE-EVAL SANDBOX PATTERN                          │
│  (Vis / svar)                                                           │
│                                                                         │
│  LLM ──→ {code: [{expr, time-ms}], thinking, final?}                   │
│    │                                                                    │
│    ↓                                                                    │
│  Parse (edamame) → Paren repair → SCI eval → {result, stdout, stderr}  │
│    │                                                                    │
│    ↓                                                                    │
│  <execution_results> + <var_index> + <execution_journal>                │
│    │                                                                    │
│    ↓                                                                    │
│  LLM sees structured state, iterates or finalizes                       │
│                                                                         │
│  Properties: semi-deterministic, structured state, sandbox-isolated     │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 12. Data Flow Comparison

### Pi: Extension-Intercepted Tool Loop
```
                    ┌──────────────┐
                    │  Extensions  │ ← intercept, modify, block
                    └──────┬───────┘
                           │
User ──→ prompt() ──→ Agent ──→ tool_calls ──→ handlers ──→ results
                       ↑                                      │
                       └──────── messages array ──────────────┘
                                     │
                              JSONL session file (tree)
```

### OpenCode: Multi-Agent with LSP
```
User ──→ Coder Agent ──→ tool_calls ──→ handlers ──→ results
              │                             │
              ├──→ Task Agent (sub-agent)    ├──→ LSP servers
              │                             ├──→ MCP servers
              └──→ Title Agent              └──→ Permission service
                                     │
                              SQLite sessions DB
```

### Hermes: Learning Loop Gateway
```
                    ┌──────────────────┐
                    │  Gateway Process │
                    │  (multi-platform)│
                    └──────┬───────────┘
                           │
Telegram/Discord/CLI ──→ AIAgent ──→ tool_calls ──→ registry ──→ results
                           │              │              │
                           ├──→ Skills    ├──→ ThreadPool ├──→ Memory
                           ├──→ Honcho    │              └──→ session_search
                           └──→ MEMORY.md │
                                          │
                                   skill_create (autonomous)
                                          │
                                   .hermes/skills/*.md
```

### Vis: Deterministic Sandbox with Entity Persistence
```
TUI/Web/CLI/Telegram ──→ conversations/send!
                              │
                       ┌──────┴──────┐
                       │  query-env! │
                       └──────┬──────┘
                              │
                    ┌─────────┴─────────┐
                    │  iteration_loop   │
                    │  ┌──────────────┐ │
                    │  │ SCI Sandbox  │ │ ← tools are bound fns
                    │  │ (def x ...)  │ │ ← state accumulates
                    │  │ (search ...) │ │ ← deterministic eval
                    │  └──────────────┘ │
                    │        │          │
                    │  paren-repair     │
                    │  auto-forget      │
                    │  repetition-detect│
                    │  strategy-restart │
                    └─────────┬─────────┘
                              │
              SQLite entity tree
              conversation → query → iteration → iteration-var
```

---

## Appendix: Feature Matrix

| Feature | Pi | OpenCode | Hermes | Vis |
|---|---|---|---|---|
| Interactive TUI | ✅ | ✅ | ✅ | ✅ |
| Web UI | ❌ | ✅ (desktop + web) | ❌ | ✅ |
| CLI one-shot | ✅ (`-p`) | ✅ (`run`) | ✅ (`-q`) | ✅ (`vis run`) |
| Telegram bot | ❌ | ❌ | ✅ | ✅ |
| Session branching | ✅ (tree) | ❌ | ❌ | ❌ |
| Session resume | ✅ (`-c`, `-r`) | ✅ (`-c`) | ✅ (`--resume`) | ✅ (conversation ID) |
| Compaction | ✅ (manual + auto) | ✅ (summarizer agent) | ✅ (50% context trigger) | ✅ (journal squash) |
| Skills (agentskills.io) | ✅ | ✅ | ✅ | ✅ |
| Skill auto-creation | ❌ | ❌ | ✅ | ✅ |
| MCP support | ❌ (extension) | ✅ | ✅ | ❌ |
| LSP support | ❌ | ✅ | ❌ | ❌ |
| Sub-agents | ❌ (extension) | ✅ (Task agent) | ✅ (delegate_task) | ✅ (sub-rlm-query) |
| Parallel sub-queries | ❌ | ❌ | ✅ (ThreadPool) | ✅ (futures + semaphore) |
| Code sandbox | ❌ | ❌ | ❌ | ✅ (SCI) |
| Paren/syntax repair | ❌ | ❌ | ❌ | ✅ |
| Var persistence | ❌ | ❌ | ❌ | ✅ (iteration-var entity) |
| Git tools (native) | ❌ | ❌ | ❌ | ✅ (JGit) |
| Document indexing | ❌ | ❌ | ❌ | ✅ (PageIndex) |
| Entity extraction | ❌ | ❌ | ❌ | ✅ |
| Learning loop | ❌ | ❌ | ✅ | Partial (skill-manage) |
| User modeling | ❌ | ❌ | ✅ (Honcho) | ❌ |
| Batch trajectories | ❌ | ❌ | ✅ | ✅ (iteration-level) |
| RL integration | ❌ | ❌ | ✅ (Atropos) | ✅ (Q-value rewards) |
| Container backends | ❌ | ❌ | ✅ (6 backends) | ❌ |
| Themes | ✅ (hot-reload) | ✅ | ✅ (skin engine) | ✅ |
| Custom keybindings | ✅ | ✅ | ✅ | ✅ |
| OAuth login | ✅ | ✅ | ❌ | ❌ |
| SDK / embeddable | ✅ (TypeScript) | ❌ | ✅ (Python) | ✅ (Clojure) |
| RPC mode | ✅ (JSONL) | ❌ | ✅ (ACP) | ❌ |
| Export / share | ✅ (HTML, gist) | ✅ (share links) | ❌ | ❌ |

---

*This comparison is based on publicly available documentation, source code, and the Vis project as of 2026-04-17. Systems evolve rapidly — verify current state before architectural decisions.*
