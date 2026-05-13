# Tasks: channel IR rendering and z/ tool surface

Source analysis: conversation `2c07dd3d-845c-4199-a245-0a4343414b18` plus current code scan.

## Problem classification

1. **P0 rendering-contract break:** some channel/tool render paths treat `:channel-render-fn` output as text/Markdown and later show `[:ir ...]` literally. Channel render fns must return canonical IR, and TUI must render that IR in place.
2. **P0 wrong visual container:** TUI wraps channel-rendered tool IR in automatic result/answer background bands. This is magic styling. IR should own its own structure; raw fallback text may get a result pane, but IR should not inherit hidden background markers.
3. **P1 z/ tool renderer bugs:** `z/locators {:depth :all}`, `z/symbols`, and `z/locator-for-symbol` expose rows where `:name` can be nil. Renderers must be nil-safe and useful for symbol rows.
4. **P1 broken `z/inspect`:** transcript shows `(z/inspect ...)` fails preflight with `Symbol 'inspect' must return a canonical :envelope map`. It needs to be either a raw helper or return a tool envelope.
5. **P1 live/history divergence:** live progress and restored-history paths must preserve channel IR the same way. No live-only fix.
6. **P2 stale docs/comments/prompts:** comments still say tool render fns return Markdown. That is now wrong and trains agents into the bug.
7. **P2 regression coverage gap:** existing tests must prove IR is not stringified and no magic background markers are prepended for channel IR.

## Fix tasks

- [ ] Reproduce the literal-IR bug through the real TUI/progress path.
  - Use a small z/ call, e.g. `(def t (z/forms "src/com/blockether/vis/internal/error.clj"))`.
  - Capture the progress entry/result before TUI paint.
  - Expected bad state to guard against: result text contains visible `[:ir` / `{:level ...}` / Hiccup instead of rendered paragraphs/code blocks.

- [ ] Enforce one channel renderer contract.
  - `:channel-render-fn` returns canonical `[:ir ...]` only.
  - Non-IR returns should become explicit diagnostic IR at the sink boundary, not raw strings.
  - Keep `:journal-render-fn` string-only.

- [ ] Fix live progress preservation.
  - In `src/com/blockether/vis/internal/progress.clj`, keep channel sink `:result` values as IR.
  - Do not run `safe-pr-str`, Markdown rendering, or `str` on channel IR.
  - Combine multiple channel entries by concatenating IR blocks in source order.

- [ ] Fix restored-history preservation.
  - In `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/chat.clj`, restore `:channel` sink entries as IR, same as live progress.
  - Ensure history never converts channel IR to `vis/text->ir` over a printed EDN string.

- [ ] Fix TUI in-place IR rendering for tool/channel bodies.
  - In `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj`, route IR bodies through `render-ir/ir->entries` directly.
  - Do not prepend generic result/body markers to IR entries.
  - Raw non-IR fallback text may still use result markers/background.

- [ ] Remove magic background from channel-rendered IR.
  - Channel IR should paint on normal assistant background unless the IR element itself is code/table/etc. and the IR renderer explicitly styles it.
  - Specifically audit `ir-body-entries`, `maybe-collapse-block`, and `maybe-collapse-raw-text-block` for marker injection.

- [ ] Fix `z/locators` / `z/symbols` / `z/locator-for-symbol` rendering.
  - Renderer must tolerate `:name nil`.
  - For symbol rows, display `:value` / `:source-preview` / `:span` clearly.
  - Avoid calling `name` on nil.

- [ ] Fix `z/inspect`.
  - Either mark it `:raw? true` if it is meant as a pure helper, or wrap `inspect-locator` in `tool-success` with render fns.
  - Add a regression for `(z/inspect (first (z/forms ...)))` through SCI.

- [ ] Update stale docs/comments/nudges.
  - Replace “tool render-fns return Markdown” with “channel render-fns return canonical IR”.
  - Audit `render.clj`, extension docs, prompt/nudge text, and foundation docs.

- [ ] Add regression tests.
  - `progress_test`: channel sink IR remains equal to original canonical IR.
  - TUI render test: channel IR produces entries without visible `[:ir` text.
  - TUI render test: channel IR entries do not get generic result background markers.
  - z/ tests: locators/symbols render with nil `:name` without throwing.
  - z/ test: `z/inspect` succeeds through the extension wrapper.

- [ ] Verify.
  - Reload changed namespaces via nREPL.
  - Run `./verify.sh --quick` during code edits.
  - Run full `./verify.sh` before handoff.
