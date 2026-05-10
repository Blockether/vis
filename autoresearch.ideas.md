## ir-char-count + StringBuilder optimizations (deferred)

### ir-char-count (proven neutral on layout bench, kept the design notes)
- Sum string-leaf `.length()` directly without building a plain-text concatenation. Identity-hash cache is correct because IR is immutable.
- Bench-neutral on `stream_layout_50k_100k_mean_ms` because only ONE bubble (the streaming one) hits the estimated-height cold path per frame; other bubbles cache-hit. Real wins would show on a "long-scrollback resume" bench (many fresh-IR bubbles in pass 1).

### StringBuilder for extract-text and render-plain-children
- `extract-text` builds per-paragraph strings via `render-plain-children` (which itself does string concat) then `(str/join "\n\n" ...)` over them.
- Replace the per-paragraph result-collection with a single shared `StringBuilder` threaded through the walker; emit `\n\n` separators inline.
- Targets: TTS path, clipboard fallback, anywhere `extract-text` is actually called for the string output.
- Not in the layout hot loop (we replaced that with `ir-char-count` already).

### Mid-window walker for scroll-up-during-streaming
- A4 wireup falls through to full render when `scroll != nil` (~34ms/frame at 100k chars while streaming is happening and user scrolled up).
- Mid-window walker: emit only the rows in `[viewport-top, viewport-top + inner-h]` of the bubble. Requires either (a) reverse-walking from end of IR while skipping rows past viewport-bottom, or (b) forward-walking with a row counter that only emits when in window.
- Bigger architectural change. The 80ms throttle currently tolerates 34ms per frame; not blocking but tight.
