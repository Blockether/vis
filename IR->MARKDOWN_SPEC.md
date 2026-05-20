# IR -> Markdown answer contract

## Problem

Model-facing `[:ir ...]` is too brittle. One nesting mistake can turn final prose into an EDN code block. Models write Markdown naturally; IR should not be prompt syntax.

## Decision

Markdown is answer source of truth. IR becomes derived render AST/cache.

## Final-answer API

Preferred model call:

```clojure
(done-md "Summary\n\n- Item one\n- Item two")
```

Optional structured form:

```clojure
(done {:format :markdown
       :text "Summary\n\n- Item one"})
```

Legacy supported during migration:

```clojure
(done [:ir {} [:p "text"]])
```

Prompt must tell models: use `done-md`; do not hand-author `[:ir ...]` unless specifically debugging renderer internals.

## Persistence

Canonical persisted answer:

```text
answer_markdown TEXT NOT NULL
answer_ir       BLOB NULL      -- derived cache, invalidatable
answer_format   TEXT NOT NULL  -- "markdown" | "legacy-ir"
```

Migration-light variant: keep current `answer` BLOB but persist `(render/text->ir markdown)` from `done-md`; add Markdown column later.

## Runtime flow

```text
model markdown
  -> done-md
  -> CommonMark parse via render/text->ir
  -> canonical IR for TUI/Telegram/voice renderers
  -> persist markdown source + optional IR cache
```

Copy/export uses original Markdown. TUI renders IR cache, reparsed from Markdown when cache missing/stale.

## Compatibility

- Existing IR answers render as today.
- Export old IR by `vis/render :markdown`.
- If old answer is malformed `[:ir {} [blocks...]]`, repair by flattening nested block vector when all children look like IR blocks.
- If old answer is EDN code block containing vector-of-blocks, optional forensic repair can parse EDN and lift to IR.

## Validation

Reject final answers that render as one EDN code block only because of nested IR blocks. Error:

```text
Final answer looks like nested IR blocks. Use done-md Markdown, or flatten IR with (into [:ir {}] blocks).
```

## Rollout

1. Add `done-md` sandbox binding.
2. Add tests for Markdown bullets, code fences, tables, links.
3. Change prompt to require `done-md`.
4. Keep `done` accepting IR/needs-input for compatibility.
5. Add Markdown persistence columns or cache layer.
6. Make TUI copy/export prefer Markdown source.
7. Deprecate model-authored IR after stored sessions migrate.
