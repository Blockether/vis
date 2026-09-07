---
name: design
description: Design or review Vis UI, including layout, controls, copy, states and visual artifacts.
---

# Design

This skill defines visual rules for Companion and TUI. Use it for visual work,
not backend-only changes. Component docstrings define implementation contracts.
Paths below are relative to `.vis/skills/design/`.

## Load only what the task needs

- Visual decisions or screenshot review: [visual system](references/visual-system.md).
- Companion UI implementation, rendering or artifacts: [Companion](references/companion.md).
- TUI UI implementation, rendering or artifacts: [TUI](references/tui.md).

For UI changes, read the visual system and the affected client reference.
For a design explanation, read the visual rules; browser setup is not required.

## Intent and boundaries

For a new screen or substantial restyle, identify the primary element, text
hierarchy, grouping and main interaction. Compare with a similar existing
screen. Small copy or spacing fixes do not require a design proposal. Preserve
information, states, controls and navigation unless a behavior change is requested.

Use production components rather than copied mockups or preview-only versions.
Keep single-use fragments local. Add a shared control when a second caller
needs it. Preserve accessibility and input behavior.

## Completion

Render the implementation, test changed interactions and fix in-scope defects.
Review accessibility, platform behavior, hierarchy, states and wording. Support
visual claims with measurements and identify what should remain unchanged.
The client reference specifies required checks and review artifacts. A design
explanation alone needs no build or attachment. Report missing verification.
