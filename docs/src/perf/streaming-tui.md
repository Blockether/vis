# TUI live-streaming performance

Goal: per-frame render cost **flat across body size**. Today it grows
linearly in the streamed character count; at ~50 KB the frame budget
of the 80 ms render throttle is gone, frames pile up, scrolling and
input feel laggy.

## Baseline measurement (criterium + progressive simulator)

Bench harness: `dev/com/blockether/vis/bench-stream.clj`.

Reproduce from nREPL:

```clojure
(require '[com.blockether.vis.bench-stream :as b] :reload)
(b/run-progressive! {:target-chars 100000 :chunk-chars 1000})
(b/run-criterium!  {:sizes [5000 50000 100000]})
```

### `run-progressive!` baseline (100 frames, fresh IR per chunk, bubble-w=100)

| body bucket | mean | p95   | max    |
| ---         | ---  | ---   | ---    |
| <5k         | 5.7  | 7.4   | 8.7    |
| 5k–20k      | 21.4 | 36.8  | 53.9   |
| 20k–50k     | 37.4 | 57.8  | 96.6   |
| 50k–100k    | 73.0 | 121.6 | 243.7  |

Linearity ratio `mean(50k-100k) / mean(<5k)` ≈ **12.8×**.
Render throttle is 80 ms. Anything above that overshoots the frame
budget and the render queue backlogs.

## What pi (`@mariozechner/pi-tui` + `pi-coding-agent`) does differently

Source paths read for this analysis:

- `pi-tui/dist/tui.js`
- `pi-tui/dist/components/markdown.js`
- `pi-coding-agent/dist/modes/interactive/components/assistant-message.js`
- `pi-coding-agent/dist/modes/interactive/interactive-mode.js`

### What pi DOES NOT do

- **No incremental Markdown parse.** `Markdown.setText(text)` invalidates
  its cache and the next `render(width)` re-runs `marked.lexer` on the
  *entire* buffer, then `wrapTextWithAnsi` over every produced line.
- **No incremental component reuse on stream.** On every `message_update`,
  `AssistantMessageComponent.updateContent(message)` calls
  `contentContainer.clear()` and constructs a *fresh* `Markdown(...)`
  per text/thinking block. Cache is dropped.

So pi pays the same O(N) reparse cost we do, in principle.

### Why pi feels smooth anyway

1. **Frame coalescing at 60 fps cap** (`tui.js:308-355`):
   `requestRender()` is idempotent (single boolean flag), drained on
   `process.nextTick`, and rate-limited by
   `MIN_RENDER_INTERVAL_MS = 16` via `setTimeout`. N streamed tokens
   between two frames collapse to one render.

2. **Differential terminal write** (`tui.js:765-`): diffs
   `previousLines` vs `newLines` by JS string `!==`, computes
   `firstChanged..lastChanged`, wraps the writes in
   `\x1b[?2026h … \x1b[?2026l` (synchronized output), and only emits
   `\x1b[2K` + the new content for the changed range. For append-only
   streaming `firstChanged === previousLines.length`, so only the new
   tail line(s) hit the terminal. Lanterna's `REFRESH_DELTA` already
   does the same thing structurally, so this is **not** the gap.

3. **JS `marked.lexer` is fast and JIT-friendly** on V8. Commonmark
   on the JVM via our IR pipeline is heavier per char. Order of
   magnitude difference at 100 KB.

### Net

Pi has no magic incremental renderer. Two structural advantages:

- 60 fps coalescing (vs. our 12.5 fps / 80 ms).
- Cheaper per-frame parse cost from V8.

To beat them, we need something pi does *not* do: **actual
incremental parse + wrap** so the per-frame cost is bounded by the
chunk size, not the total body.

## Optimization ladder for vis (in expected impact order)

Each entry is a single autoresearch experiment with the bench above
as the gate. Keep ⇔ `stream_50k_100k_mean_ms` improves ≥10% without
regressing `stream_total_ms`.

1. **Stable cache key for the streaming bubble.**
   Replace `(System/identityHashCode answer)` in
   `format-answer-markdown-data` with the existing `text-fingerprint`
   (`render.clj:2298`) so the cache survives `(vec ...)` /
   `assoc`-driven turn rebuilds. **Won't help fresh-stream growth
   directly** (every chunk grows the body), but it eliminates the
   entire scrollback's re-projection cost on resize/scroll/dialog
   open. Cheap, safe, prerequisite for everything below.

2. **Paragraph-split incremental IR.**
   Streaming text grows by suffix. Split body on `\n\n` boundaries:
   - Stable paragraphs: parse + walk + wrap once, cache by
     `text-fingerprint`. Forever-hits.
   - Tail paragraph (incomplete): re-parse on every chunk. O(tail
     paragraph), bounded.
   Concatenate prewrapped lines. New per-frame cost ≈ O(tail
   paragraph + lines count), independent of total body length.

3. **Incremental wrap.**
   Persist `prewrapped-lines` + last-wrap byte offset on the
   streaming bubble. On chunk arrival, only re-wrap the suffix from
   the last hard line break. Bounds wrap cost to chunk size.

4. **Drop render throttle 80 ms → 33 ms** (`state.clj:204`).
   Only after (1)+(2)+(3): when frame cost is ≤ 16 ms p99 we can
   afford 30 fps without backlog and the perceived stream becomes
   buttery.

5. **`identical?` swap in single-prev memos.**
   Only useful where a hashmap cache is overkill (one current value,
   not many). Tiny win, do last as a polish pass.

## Constant-time render acceptance criteria

A change is a "win toward constant-time" when, against the same
bench:

- `stream_50k_100k_mean_ms` ≤ 1.5 × `stream_<5k_mean_ms`
  (linearity target)
- `stream_p99_ms` ≤ 80 ms
- `stream_total_ms` for the 100-frame run drops monotonically across
  the optimization ladder.
