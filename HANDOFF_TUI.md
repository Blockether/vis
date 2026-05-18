# TUI header tabs handoff

## Request

Merge workspace tabs into main TUI header row:

- Tabs live in header, not separate tab band.
- Header content row uses 20% left / 60% center / 20% right layout.
- Tabs occupy center 60% slot.
- Left/right arrows appear and are clickable when tab count overflows center slot.

## Files changed

- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/header.clj`
- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/state.clj`
- `extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/header_test.clj`
- `extensions/channels/vis-channel-tui/test/com/blockether/vis/ext/channel_tui/state_test.clj`

## What changed

### Header layout

`draw-header!` now paints tabs inside content row:

```text
[ left 20% ][ center 60% tabs ][ right 20% ]
```

- Left slot: latest notification wins; otherwise conversation title/placeholder.
- Center slot: workspace tabs when more than one tab exists.
- Right slot: channel status + copy-id affordance.
- Header row count no longer increases when tabs exist.

### Tab overflow

Added center tab windowing:

- `visible-tab-window`
- active tab kept visible
- max visible tabs computed from center width / `tab-min-width`
- overflow draws clickable arrows:
  - `‹` registered as `:workspace-tab` with `:index :prev`
  - `›` registered as `:workspace-tab` with `:index :next`

Existing click path still works because screen dispatches `[:select-workspace-tab-index (:index hit)]`.

### State navigation

`state.clj` now accepts:

- `:next`
- `:prev`
- numeric tab index

for `:select-workspace-tab-index`.

## Tests added/updated

Updated header tests for merged layout:

- tabs do not add rows (`header-rows` stays 3)
- tabs render at center slot (`left=16 width=48` for 80 cols)
- copy/status stays in right 20% slot
- overflow arrows register clickable regions at center slot edges

Updated state test:

- validates `:prev` tab cycling.

## Verification run

Passed:

```bash
clojure -M:test -n com.blockether.vis.ext.channel-tui.header-test -n com.blockether.vis.ext.channel-tui.state-test
```

except one pre-existing/unrelated failure:

```text
com.blockether.vis.ext.channel-tui.state-test
send-message-test
restores a cancelled prompt to the input instead of rendering a cancelled answer

Expected input-history ["prior"], got ["prior" "edit me hello"]
```

Header tests all pass. Workspace tab tests pass.

## Notes / risks

- Right slot is hard-capped to 20%; long voice/status text is truncated before copy-id.
- Center tab overflow arrows cycle active tab, not scroll independent viewport state.
- `:workspace-tab` click kind reused for arrows to avoid new screen click plumbing.
- Top header doc comments still describe old title-centered layout and should be cleaned if docs polish needed.
