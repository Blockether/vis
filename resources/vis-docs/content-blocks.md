# Content-block protocol

A Vis message has a role and an ordered array of typed content blocks.
Persistence, gateway responses, SSE replay and clients use the same JSON
format. This reference is for gateway client and renderer authors.

Markdown is stored in a `prose` block. Parsed renderer data is temporary and
is not sent or stored as message content.

Canonical contracts and schemas are in
[`packages/vis-contract/resources/vis-contract/`](https://github.com/Blockether/vis/tree/main/packages/vis-contract/resources/vis-contract).
`com.blockether.vis.internal.content` provides the Clojure constructors.

## Invariants

1. Every map key is a `snake_case` JSON string, recursively.
2. Every enum value is a lowercase string, never a Clojure keyword.
3. A message has exactly one `role` and one ordered `content` array.
4. Every block has a stable string `id` and string `type`.
5. Completed blocks and terminal messages are immutable.
6. Tools, errors, attachments, and lifecycle state remain structured data.
7. A renderer may parse `prose.markdown` for display, but must not persist or
   send the parsed data as message content.
8. Pending messages are not persisted. Submitted turns and their content are
   stored. A restart interrupts running work and does not resubmit it.

These rules apply at every nesting depth. This is invalid because `provider` is
a keyword value and the nested keys are keywords:

```clojure
{"llm_actual" {:provider :openai-codex :model "gpt-5.6"}}
```

The canonical value is:

```json
{"llm_actual":{"provider":"openai-codex","model":"gpt-5.6"}}
```

## Message envelope

```json
{
  "id": "turn_01J...",
  "role": "assistant",
  "status": "completed",
  "content": [
    {
      "id": "block_01J...",
      "type": "prose",
      "markdown": "Implemented it. **Six tests pass.**"
    }
  ],
  "created_at": 1740000000000,
  "completed_at": 1740000001234,
  "model": "gpt-5.6",
  "provider": "openai-codex"
}
```

Required fields are `id`, `role`, `status`, `content`, and `created_at`.
`content` is always an array, including when empty.

### Roles

`role` is one of:

- `user` — human-authored input;
- `assistant` — model-authored output and visible work;
- `system` — runtime-authored instructions or notices;
- `developer` — application/developer instructions;
- `tool` — standalone externally produced content.

A tool invoked during an assistant turn is normally a `tool` block inside the
assistant message. Optional `author` metadata may supply a display name but never
replaces `role`.

### Status

Message status is `streaming`, `completed`, `failed`, `cancelled`, or
`suspended`. Terminal statuses are immutable. A failed or cancelled message may
retain completed blocks and append an `error` or `notice` block.

## Block contract

All blocks require `id` and `type`. Receivers preserve unknown block types and
render them safely without interpreting arbitrary fields as markup.

### `prose`

```json
{"id":"b1","type":"prose","markdown":"A **Markdown** response."}
```

`markdown` is required and is the only mutable field while this block streams.
The type is called `prose`, not `paragraph`, because paragraph layout belongs to
the renderer.

### `code`

```json
{"id":"b2","type":"code","text":"(println :done)","language":"clojure"}
```

`text` is required and verbatim. `language` is optional. Renderers do not parse
`text` as Markdown and do not infer execution intent.

### `tool`

```json
{
  "id": "b3",
  "type": "tool",
  "tool": "run_tests",
  "status": "completed",
  "input": {"paths":["test/example_test.clj"]},
  "output": {"pass":6,"fail":0},
  "started_at": 1740000000100,
  "completed_at": 1740000001100
}
```

`tool` and `status` are required. Tool status is `pending`, `running`,
`completed`, `failed`, or `cancelled`. `input`, `output`, `error`, timestamps,
and attachment references are optional JSON values. Their nested maps obey the
same string-key rule.

### `reasoning`

```json
{"id":"b4","type":"reasoning","text":"Checking quota data…","visibility":"private"}
```

`text` is required. `visibility` is `private` or `visible` and defaults to
`private`. Unauthorized channels must not receive private reasoning.

### `error`

```json
{
  "id": "b5",
  "type": "error",
  "code": "provider_unavailable",
  "message": "OpenAI Codex is unavailable right now.",
  "retryable": true
}
```

`code` and `message` are required. `retryable` defaults to `false`. Diagnostics
must be safe to expose and must never include credentials.

### `attachment`

```json
{"id":"b6","type":"attachment","attachment_id":"att_01J","name":"report.png","media_type":"image/png"}
```

An attachment block references stored attachment metadata. The message does
not contain the binary file.

### `notice`

```json
{"id":"b7","type":"notice","code":"turn_cancelled","message":"Stopped by user."}
```

A notice reports lifecycle changes or other information without indicating an error.

The canonical contract also defines specialized blocks such as `speech`; use
its schema when implementing support for additional types.

## Streaming contract

Streaming creates a block, appends deltas to its declared text field, completes
the block, and finally terminates the turn:

```json
{"type":"content.block.started","turn_id":"turn_01J","block":{"id":"b1","type":"prose","markdown":""}}
{"type":"content.block.delta","turn_id":"turn_01J","block_id":"b1","field":"markdown","text":"Implemented "}
{"type":"content.block.delta","turn_id":"turn_01J","block_id":"b1","field":"markdown","text":"it."}
{"type":"content.block.completed","turn_id":"turn_01J","block_id":"b1"}
{"type":"turn.completed","turn_id":"turn_01J","status":"completed"}
```

A delta is valid only for an existing incomplete block and its declared mutable
field: `markdown` for prose or `text` for enabled reasoning. Receivers concatenate
deltas in sequence order. Reconnect uses gateway event sequence numbers.

Terminal events do not repeat the answer. Clients read the completed message
from the state built by applying events or from the turn endpoint.

## Persistence

Persistence stores the message envelope, its `content` array and queryable turn
metadata. It does not store rendered exports, HTML, parsed Markdown, client
layout data or a pending request queue.

A persisted running turn is reconciled to `interrupted` after restart. The
runtime never reconstructs and automatically resubmits its request.

## Rendering and projections

Renderers switch on `block.type` and preserve block order and message role:

- TUI parses prose only while producing styled terminal lines.
- Clipboard, search and export generate text or Markdown from the blocks.
- `code.text` is always literal.
- Private reasoning remains hidden unless explicitly authorized.

Generated display output must not replace stored content. The `content` array
is the only answer representation; renderers cannot supply an alternate one.

## See also

- [Exporting sessions](exporting-sessions.md) — the same blocks rendered to Markdown, HTML or a screencast.
- [Remote access and the Companion app](gateway.md) — delivery through SSE.
- [Live views](live-views.md) — saving an extension's progress display in a transcript.
