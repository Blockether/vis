---
name: design
description: Design or review Vis UI, including layout, controls, copy, states and visual artifacts.
---

# Design

This skill owns visual decisions shared by Companion and TUI, not general coding procedure.
Use it for visual work; a non-visual backend change does not need it. Component docstrings own
implementation contracts. Paths below are relative to `.vis/skills/design/`.

## Load only what the task needs

- Visual decisions or screenshot review: [visual system](references/visual-system.md).
- Companion UI implementation, rendering or artifacts: [Companion](references/companion.md).
- TUI UI implementation, rendering or artifacts: [TUI](references/tui.md).

Read the visual system plus the affected surface for UI changes. A design explanation needs the
visual rules, not browser setup. Do not reread unchanged documents just to satisfy a checklist.

## Intent and boundaries

For a new screen or substantial restyle, establish the lead element, type hierarchy, grouping edge
and distinctive functional choice; compare with a similar shipped screen. A small copy or spacing
fix does not require a four-line design proposal. Restyling preserves facts, states, controls and
flow unless the user also requested a behavior change.

Use production components, not a copied mockup or preview-only twin. Keep one-use fragments local;
shared controls enter the existing vocabulary at their second real caller. Preserve accessibility,
state information and input behavior while simplifying appearance.

## Completion

For implementation, continue through rendering, inspecting changed interactions and fixing defects
within scope. Review accessibility, platform behavior, hierarchy, visual rules, states and words.
Use measurements to support visual claims, and say what should stay as well as what needs fixing.
The affected surface reference specifies its verification and artifact requirements; an answer
about design alone does not require a build or attachment. If verification is blocked, name the
missing evidence rather than declaring the design done.
