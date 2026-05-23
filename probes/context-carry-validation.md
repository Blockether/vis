# Context carry validation

Purpose: catch regressions where resumed sessions forget turn/iter scope, short follow-ups lose prior Q/A, or Ctrl+K reopens forever.

## Manual/CLI probes

Use explicit provider/model so runs are comparable:

```bash
vis --persist --provider anthropic-coding-plan --model claude-haiku-4-5 "Remember this preference: I like yellow. Answer only OK."
# Copy returned session id.
vis --session-id <ID> --provider anthropic-coding-plan --model claude-haiku-4-5 "Do one unrelated thing: say the word bridge."
vis --session-id <ID> --provider anthropic-coding-plan --model claude-haiku-4-5 "Do one unrelated thing: say the word river."
vis --session-id <ID> --provider anthropic-coding-plan --model claude-haiku-4-5 "Do one unrelated thing: say the word stone."
vis --session-id <ID> --provider anthropic-coding-plan --model claude-haiku-4-5 "What color do I like?"
```

Expected: final answer says yellow.

## Resume scope probe

```bash
vis channels tui --session-id 51834f45-b0ab-4c5c-ab7d-f8c01573b858
```

Start a new turn, then inspect `vis sessions export <ID> --md` or transcript prompt snapshots.
Expected new `;; ctx` uses persisted turn position, not `:session/turn 1` unless session has only one turn.

## Short follow-up probe

```bash
vis --persist --provider anthropic-coding-plan --model claude-haiku-4-5 "Give me two options named alpha and beta, then ask which one I want."
vis --session-id <ID> --provider anthropic-coding-plan --model claude-haiku-4-5 "the second one"
```

Expected: model resolves "second" against immediately previous Q/A without asking clarification.

## Ctrl+K palette probe

In TUI, press Ctrl+K.
Expected:
- No `/workspace`, `/voice`, or extension slash roots by default.
- Selecting one command closes palette after command completes.
- Typed `/` still suggests slash commands.

## Observation carry probe

Ask agent to read a file, then ask follow-up before mutation.
Expected: read observation remains in ctx trailer.
Then ask agent to patch/write.
Expected: older observation-only pins are pruned after mutation; mutation evidence remains.

## Verified run: 2026-05-23

Provider/model:

```bash
--provider anthropic-coding-plan --model claude-haiku-4-5
```

Session:

```text
e84e621b-9526-42aa-9e4f-aae6be3f62c0
```

Turns:

1. "remember favorite color is yellow" → `OK`
2. "Say only bridge." → `bridge`
3. "Say only river." → `river`
4. "Say only stone." → `stone`
5. "What color did I say I like?" → `yellow`

Prompt snapshot check via `tr/prompt-snapshots`:

```clojure
{:turn-count 5
 :turn-positions [1 2 3 4 5]
 :ctx-turns ["1" "2" "3" "4" "5"]
 :last-answer "yellow"}
```

Result: resumed CLI continuation keeps turn number and prior memory across distractor turns.
