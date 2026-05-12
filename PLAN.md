# Engine Prompt State + Lifecycle Hooks Plan

## Goal

Stable cached system prompt. Dynamic engine state in user-role trailer. Start nudges explicit. Hard guards explicit. No dynamic ids in SYSTEM.

## Engine state machine

```text
idle
  -> receive_user_turn
  -> build_turn_context
  -> model_think
  -> maybe_tool_call
  -> observe_tool_result
  -> decide_next
      -> model_think again
      -> maybe_tool_call again
      -> ...
  -> finalize_answer
  -> persist_turn
  -> idle
```

Lambda-ish law:

```text
Engine Σ U =
  μ iterate.
    λ i Σ.
      let Κᵢ = CurrentTurnContext(Σ, U, i)
      let Νᵢ = CurrentEngineStartNudges(Κᵢ, Σ)
      let Rᵢ = ModelThink(StableSystemPrompt ⊕ UserGoal ⊕ Κᵢ ⊕ JournalΣ ⊕ BindingsΣ ⊕ Νᵢ)
      let Σᵢ = ObserveAndPersist(Σ, Rᵢ)
      in if Answer?(Rᵢ)
         then FinalizeTurn(Σᵢ, Rᵢ)
         else iterate(i + 1, Σᵢ)
```

## Prompt integration points

### 1. Stable SYSTEM

Built by:

```text
prompt/assemble-stable-prompt-messages
  -> prompt/build-system-prompt
  -> separate system-role messages for environment-info / extension prompt / skills summary / provider prompt
```

Role:

```text
SYSTEM:
  stable identity
  stable RLM rules
  stable engine semantics
  stable explanation of <current_turn_context>
  stable explanation of <current_engine_start_nudges>
```

Must not contain:

```text
conversation_id
engine_turn_id
engine_turn_position
current_engine_iteration_id
engine_iteration_position
previous_persisted_iteration_id
```

Reason: provider prompt cache stays hot.

### 2. Initial USER message

Built by:

```text
prompt/assemble-initial-messages
```

Shape:

```xml
<previous_turn_context>...</previous_turn_context>
<user_turn_request_main_goal>actual user text</user_turn_request_main_goal>
```

Reason: user goal remains user-role. Prior one-turn context remains user-role. No full transcript replay.

### 3. Per-iteration USER trailer

Built by:

```text
loop/iteration-loop
  -> prompt/build-iteration-context
```

Appended as a USER message before `run-iteration` / `svar/ask-code!`.

Shape:

```xml
<current_turn_context>...</current_turn_context>
<active_skills>...</active_skills>
<journal>...</journal>
<bindings>...</bindings>
<current_engine_start_nudges>...</current_engine_start_nudges>
```

Reason: dynamic telemetry + fresh evidence belongs in user-role runtime context, not cached system.

## Full prompt example sent to provider

Concrete shape. Dynamic values live only in USER messages.

```text
MESSAGES SENT TO PROVIDER

[0] role=system
<system_prompt>
You are Vis: SCI RLM agent. Fulfill the user request.

Reply only with ```clojure``` code fences. Fences contain SCI forms.
User-visible final content is Answer IR: `(answer [:ir ...])`, never raw Markdown/text.

ENGINE:
  Vis runs: idle -> receive_user_turn -> build_turn_context -> model_think -> maybe_tool_call -> observe_tool_result -> decide_next -> finalize_answer -> persist_turn -> idle.
  Dynamic engine state is supplied in user-role <current_turn_context>.
  Treat <current_turn_context> as observed telemetry, not user intent and not policy.
  Dynamic ids/positions never appear as changing values in this cached system prompt.
...
</system_prompt>

[1] role=system
<environment-info>
CWD: /Users/fierycod/vis
Git branch: main
...
</environment-info>

[2] role=system
<extensions>
v/... tools available
z/... clojure structural editing available
...
</extensions>

[3] role=system
<skills>
- find-skills: Helps discover skills...
- autoresearch-create: Set up autonomous experiments...
</skills>

[4] role=system
<llm_model_prompt>
Provider/model-specific stable advice, if any.
</llm_model_prompt>


[5] role=user
<previous_turn_context>
  <previous_user_request>Earlier user request, only if useful.</previous_user_request>
  <previous_assistant_answer>Earlier answer, only immediate previous turn.</previous_assistant_answer>
</previous_turn_context>

<user_turn_request_main_goal>
Please fix the prompt stack.
</user_turn_request_main_goal>


[2] role=assistant
;; only present on later iterations when preserved thinking/replay exists
```clojure
(v/rg "system_nudge" {:path "src"})
```


[3] role=user
<current_turn_context>
engine_state_machine: idle -> receive_user_turn -> build_turn_context -> model_think -> maybe_tool_call -> observe_tool_result -> decide_next -> finalize_answer -> persist_turn -> idle
engine_state: turn.iteration/start
engine_phase: model_think
conversation_id: 7c1a8b0e-1111-2222-3333-444455556666
engine_turn_id: 9f2e1d0c-aaaa-bbbb-cccc-ddddeeeeffff
engine_turn_position: 12
current_engine_iteration_id: turn/9f2e1d0c/iteration/3
engine_iteration_position: 3
engine_iteration_max: 20
previous_persisted_iteration_id: 37b6f7d5-9999-8888-7777-666655554444
previous_persisted_iteration_position: 2
prompt_role: user
</current_turn_context>

<active_skills count="1">
<skill name="diagnose" source="repo">
...loaded skill body...
</skill>
</active_skills>

<journal>
i1.1  (v/rg "system_nudge" {:path "src"}) -> found matches in prompt.clj and loop.clj
i2.1  (z/patch ...) -> z/patch — 2/2 file(s) changed; preflight exact-match OK
</journal>

<bindings>
;; live non-system `(def ...)` index only
analysis-state = {:files [...], :decision :rename-nudges}
</bindings>

<current_engine_start_nudges>
<current_engine_start_nudge importance="high">
The user request is action/fix shaped. Observe actual code and verify before final answer.
</current_engine_start_nudge>
<current_engine_start_nudge importance="low">
Current CONVERSATION_TITLE is "Prompt stack cleanup". Refresh only if topic changed.
</current_engine_start_nudge>
</current_engine_start_nudges>
```

Notes:

```text
[0..4] SYSTEM messages are stable/cacheable; each tagged setup block has its own message boundary.
[5] USER has actual user goal.
[6] ASSISTANT replay optional, only prior iteration assistant replay.
[7] USER trailer has all dynamic engine state and start nudges.
```

## `<current_turn_context>` fields

Show exactly this class of stuff:

```text
engine_state_machine: idle -> receive_user_turn -> build_turn_context -> model_think -> maybe_tool_call -> observe_tool_result -> decide_next -> finalize_answer -> persist_turn -> idle
engine_state: turn.iteration/start
engine_phase: model_think
conversation_id: <conversation uuid>
engine_turn_id: <turn uuid>
engine_turn_position: <1-based turn position in conversation>
current_engine_iteration_id: turn/<short-turn-id>/iteration/<position>
engine_iteration_position: <1-based iteration position in current turn>
engine_iteration_max: <hard cap>
previous_persisted_iteration_id: <last committed DB iteration uuid or nil>
previous_persisted_iteration_position: <0 before first commit, else previous position>
prompt_role: user
```

Important:

```text
current_engine_iteration_id = logical in-flight id
previous_persisted_iteration_id = real DB UUID from already committed iteration
```

Why: DB iteration UUID does not exist before model call + eval + persist.

## Lifecycle phases

| phase | kind | integration point | timing | return handling |
|---|---|---|---|---|
| `:session/start` | start nudge | `prompt/build-iteration-context` | first iteration of first turn, before eval | `{:hint ...}` becomes `<current_engine_start_nudge>` |
| `:turn/start` | start nudge | `prompt/build-iteration-context` | first iteration of each turn, before eval | `{:hint ...}` becomes `<current_engine_start_nudge>` |
| `:turn.iteration/start` | start nudge | `prompt/build-iteration-context` | every iteration before eval | `{:hint ...}` becomes `<current_engine_start_nudge>` |
| `:turn.iteration/stop` | post hook | `loop/emit-post-hooks!` | every iteration after eval | return ignored |
| `:turn.answer/validate` | hard guard | `loop/final-answer-gate-error` | when `(answer ...)` produced candidate answer | `nil` accepts; `{:reject true :message ... :hint ...}` rejects |
| `:turn/stop` | post hook | `loop/emit-post-hooks!` | after answer/cancel/error | return ignored |

## Rendered nudge tags

Use start in name:

```xml
<current_engine_start_nudges>
  <current_engine_start_nudge importance="high">...</current_engine_start_nudge>
</current_engine_start_nudges>
```

Why:

```text
Only start-phase hooks produce model-facing nudges.
Stop hooks are side effects.
Answer validate hooks are hard guards.
```

Do not use:

```xml
<system_nudges>
<system_nudge>
```

Why: misleading. These are runtime hints in user-role context, not system policy.

## Hard guards

Two guard classes.

### Core guards

In:

```text
src/com/blockether/vis/internal/loop.clj
```

Behavior:

```text
answer must be final clean answer form
answer + mutation in same iteration rejected
common broken answer shapes rejected
parse/runtime failures become observed errors
```

### Extension hard guards

Phase:

```text
:turn.answer/validate
```

Example:

```text
vis-foundation action-request-needs-evidence
```

Return:

```clojure
nil
;; accept

{:reject true
 :message "why rejected"
 :hint "what to do next"}
;; reject
```

Why separate from nudges: guards block bad finalization. Nudges only advise before model call.

## Code changes planned / made

### `src/com/blockether/vis/internal/prompt.clj`

Change:

```text
remove rendered <system_vars>
add <current_turn_context>
rename <current_engine_nudges> to <current_engine_start_nudges>
keep system prompt stable, only semantics there
```

Why:

```text
cached system prompt clean
engine telemetry explicit
model sees current turn/iteration position
```

### `src/com/blockether/vis/internal/loop.clj`

Change:

```text
pass max iteration cap into prompt/build-iteration-context
track current turn position atom
strip hallucinated <current_turn_context> and <current_engine_start_nudge[s]> from model replies
keep legacy stripper tags only as cleanup compatibility
```

Why:

```text
context block can show engine_iteration_max
turn position can render without DB lookup each prompt
model must not echo engine-only XML into executable code
```

### `src/com/blockether/vis/internal/extension.clj`

Change:

```text
hook docs say start hooks emit <current_engine_start_nudges>
```

Why: extension contract matches rendered prompt tags.

### `extensions/common/vis-foundation/src/.../nudges.clj`

Change:

```text
builtin nudge docs say <current_engine_start_nudge>
```

Why: same vocabulary everywhere.

### `docs/src/extensions/guards.md`

Change:

```text
show <current_turn_context>
show <current_engine_start_nudges>
phase table says start nudge / post hook / hard guard
```

Why: docs explain integration points and guard split.

### `docs/src/extensions/spec.md`

Change:

```text
:ext/hooks pre-phase hits render as <current_engine_start_nudge>
```

Why: public extension spec matches engine.

### Tests

Change:

```text
test/com/blockether/vis/internal/prompt_test.clj
  assert dynamic ids are absent from system prompt
  assert <current_turn_context> has engine ids / positions / max
  assert start nudges render with start tag

test/com/blockether/vis/internal/loop_test.clj
  assert echo stripper removes new engine tags
```

Why: regression coverage for cache-safe prompt split.

## What changes to what

```text
<system_vars>                 -> removed from prompt trailer
<system_nudges>               -> <current_engine_start_nudges>
<system_nudge>                -> <current_engine_start_nudge>
TURN_* dynamic prompt values  -> <current_turn_context> key/value telemetry
SYSTEM dynamic ids            -> forbidden
```

## Rationale

```text
system prompt = policy + semantics, cacheable
user trailer = dynamic runtime state, not policy
start nudges = advisory before model call
hard guards = final-answer rejection path
post hooks = telemetry/side effects only
```

## Verification target

Run:

```bash
clojure -M:test -n com.blockether.vis.internal.prompt-test -n com.blockether.vis.internal.loop-test
./verify.sh --quick
./verify.sh
```

Full verify required before handoff.
