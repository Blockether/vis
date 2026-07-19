# Content blocks

Vis has one answer model: a role-labelled message containing an ordered array of
typed content blocks. The same JSON-ready shape is used by persistence, gateway
responses, SSE replay, and channels.

Markdown is not an alternate answer. It is a field inside a `prose` block.
Renderer-specific trees are temporary implementation details and are never sent,
stored, or accepted as message content.

The executable Clojure contract is in
`com.blockether.vis.internal.content`. Its specs and constructors are the source
of truth for validation.

## Invariants

1. Every map key is a `snake_case` JSON string, recursively.
2. Every enum value is a lowercase string, never a Clojure keyword.
3. A message has exactly one `role` and one ordered `content` array.
4. Every block has a stable string `id` and string `type`.
5. Completed blocks and terminal messages are immutable.
6. Tools, errors, attachments, and lifecycle state remain structured data.
7. A renderer may parse `prose.markdown` while rendering, but the parsed result
   is disposable and cannot cross a process or persistence boundary.
8. No pending-message queue is persisted. Only submitted turns and their settled
   content are durable. A restart interrupts in-flight work; it does not replay a
   request automatically.

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
  "input": {"namespaces":["example.test"]},
  "output": {"passed":6,"failed":0},
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

An attachment block references durable attachment metadata. Binary bytes are not
embedded in the message.

### `notice`

```json
{"id":"b7","type":"notice","code":"turn_cancelled","message":"Stopped by user."}
```

A notice is a non-error lifecycle or informational item.

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

Terminal events do not duplicate the answer. Clients obtain the settled message
from event-applied state or the canonical turn endpoint.

## Persistence

Persistence stores the message envelope and `content` array as the only answer
truth, alongside queryable turn metadata. It does not store rendered Markdown
exports, HTML, parsed Markdown trees, channel layout data, or a pending request
queue.

A persisted running turn is reconciled to `interrupted` after restart. The
runtime never reconstructs and automatically resubmits its request.

## Rendering and projections

Renderers switch on `block.type` and preserve block order and message role:

- TUI parses prose only while producing styled terminal lines.
- Clipboard, search, and export derive disposable text or Markdown projections.
- `code.text` is always literal.
- Private reasoning remains hidden unless explicitly authorized.

Derived output cannot be written back as canonical content. There is no second
answer field to synchronize and no renderer tree fallback.

## Contract tests

The implementation must prove that:

1. Clojure constructors and specs reject non-string nested map keys and
   non-JSON values.
2. REST, SSE, in-process clients, persistence, and channels observe identical
   string-keyed maps and string enum values.
3. Prose streaming, code, tool lifecycle, errors, cancellation, reconnect, and
   restart rendering are covered.
4. A completed answer survives restart using only `content`.
5. Restart never resubmits pending or interrupted user requests.
