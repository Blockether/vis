# PLAN — One execution, one quiet receipt

*Python, Result, and Activity are evidence from one execution; the interface should reveal them as one thing.*

## Context

**State before.** Companion already joins Python and Result in `apps/vis-companion/src/components/ChatContent.tsx:1431-1485`, then appends an independently collapsed Activity panel from `apps/vis-companion/src/components/LiveView.tsx:526-607`. The TUI similarly places Activity after the anchored form in `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/render.clj:4955-5005`, but its live receipt derives `finished N/N` while calls are still being discovered in `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/live_view.clj:386-400`. The stable Activity v1 projection already carries anchor, state, counts, chronological rows, omitted totals, presenter, signal, resources, and evidence (`apps/vis-companion/src/lib/live-view.ts:280-306`).

**Root problem.** Three visual disclosures describe one execution, and the Activity receipt claims a live total the runtime cannot know. Opening the trace therefore requires several taps and still does not establish which evidence belongs together.

**Alternatives considered.** Keeping three sibling blocks loses because grouping remains implicit. Adding an Activity lifecycle update event loses for phase one because visual unification needs no wire change and current producers cannot truthfully report arbitrary intermediate progress. Scraping shell output loses because prose is not a semantic progress contract. Making every nested section open loses because large Results would restore the DOM and first-paint cost that lazy mounting removed.

## Phase 1 — Unify the Companion receipt

**Rationale.** The phone is the narrowest surface and currently pays the most interaction cost for three separate blocks.

**Data.** No wire changes. Derive the headline from `ActivityProjection`: first running row plus `and others` while live; terminal state plus the sum of terminal counts after settlement.

**Acceptance criteria.** One outer disclosure owns the anchored Python form, Result, and Activity. Its collapsed state is frameless and has no live denominator. First open shows the PYTHON header and five-line preview, keeps RESULT closed and lazily unmounted, and opens ACTIVITY. Live, settled, restored, failed, and cancelled behavior is tested.

**Unknowns.** Whether mutation signal belongs in the collapsed sentence; defer until the hierarchy is proven.

## Phase 2 — Unify the TUI receipt

**Rationale.** The terminal must express the same ownership and truthful progress with keyboard disclosure semantics.

**Data.** Reuse the existing Activity projection and trace anchor; do not add a second presentation payload.

**Acceptance criteria.** One transcript-native disclosure owns Python, Result, and Activity; nested defaults match Companion; virtual-terminal tests cover narrow and wide grids and every terminal state without a live denominator.

**Unknowns.** Which existing detail-expansion key should own the outer receipt without colliding with nested Result and Activity keys.

## Phase 3 — Remove duplicate projection

**Rationale.** Once both channels consume the semantic Activity projection, retaining generic status/stat/steps nodes creates two authorities that can drift.

**Data.** Remove the obsolete node projection and its patch stream; Activity v1 remains the sole lifecycle payload.

**Acceptance criteria.** No channel reads the duplicate Activity nodes, while host lifecycle, persistence, redaction, bounded-receipt, and cross-channel tests remain green.

**Unknowns.** Whether transcript HTML or model introspection still consumes any generic Activity node; trace before deletion.

## Phase 4 — Prove the complete design

**Rationale.** Disclosure hierarchy, anchoring, persistence, and narrow layouts cross component and language boundaries that unit snapshots alone do not prove.

**Data.** None beyond existing fixtures and persisted Activity records.

**Acceptance criteria.** Companion is verified at 320, 390, 768, and 1440 CSS pixels; TUI at narrow and wide grids. Tests cover chronology, omitted totals, mutation semantics, restored identity, keyboard/screen-reader disclosure state, lazy Result mounting, and no horizontal overflow. Relevant tests, lint, formatting, and builds pass.

**Unknowns.** None once the phase-specific questions are resolved.

## State of the plan

COMPLETE.

- Phase 1 — COMPLETE (`td-f9035e`): Companion owns the unified disclosure and its honest lifecycle sentence.
- Phase 2 — COMPLETE (`td-7bb5f7`): TUI matches the hierarchy and width-bounded behavior.
- Phase 3 — COMPLETE (`td-1e6086`): Activity v1 is the sole projection; duplicate generic nodes are gone.
- Phase 4 — COMPLETE (`td-2d3dbe`): viewport, grid, lifecycle, accessibility, lazy-mount, test, lint, format, and build gates pass.
- Epic — `td-dc8d7d`.
