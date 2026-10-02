# Settings layout restored in the Companion and TUI

Keep the per-machine Settings layout, and save each change where you make it.

## Context

b272eefd8 replaced the Companion machine settings with one editor: an "Editing: Machine" header,
a scope picker, a draft with review and apply, and profiles with import and export. The TUI got
the same staged flow. On a phone, this put several controls above every setting and removed the
per-machine sections. The earlier layout lists each connected machine, opens its settings under
it, keeps Application settings separate and saves a change when you make it.

This task restores that layout from b272eefd8^ and keeps the typed values that b272eefd8 added:
numbers, lists and objects with their guided path, network, list and JSON editors.
Companion code is in `apps/vis-companion/src/screens/SettingsScreen.tsx` and
`apps/vis-companion/src/screens/settings/`. TUI code is in
`apps/vis-tui/src/com/blockether/vis/tui/dialogs.clj` and its client. The gateway keeps the typed
catalog, its `revision` and `PATCH /v1/settings`, which the Python SDK `patch_settings` uses.

Rejected alternatives: hide the editor controls only on small screens (two layouts to maintain);
revert b272eefd8 completely (removes typed values, gateway validation and the SDK route); keep
client-side profiles without the editor (no owner for review, import and export).

## 1. Companion layout

Rationale: settings belong under the machine that owns them.

Data: SettingsScreen, MachineSettings, ScopedSettingsDialog, SettingsLayout, SettingField,
`lib/gateway.ts`, `lib/types.ts`, their stories and tests.

Acceptance criteria: machine sections and the Application fold work as before b272eefd8.
Switches and choices save immediately. Number, list and object rows edit in place with Save,
Cancel and inline errors. No draft, review, profile, import or export controls remain. A row
that a more specific scope decides is locked and names that scope.

Unknowns: none.

## 2. TUI dialog

Rationale: the TUI follows the same model as the Companion.

Data: `dialogs.clj`, `client.clj`, `state.clj`, `screen.clj` and their tests.

Acceptance criteria: the pre-b272eefd8 dialog returns. Text, number, list and object rows save
when their editor confirms. The guided editors and the text-editor key fix (1bf1b2471) stay.

Unknowns: none.

## 3. Contract, documentation and verification

Rationale: schemas and guides describe only behavior that exists.

Data: `gateway.json`, `configuration.md`, `index.md`, `site.edn`, `docs.edn`, this plan.

Acceptance criteria: the settings profile schemas and the Settings guide are removed.
Configuration describes locked rows again. Affected Companion and Lazytest suites, format and
lint pass. Unrelated failures are reported, never bypassed.

Unknowns: none.

## Plan state

- Phases 1-3 are complete and verified.
- Companion: typecheck, React compiler lint (542 files) and the full Vitest run pass: 3948 unit
  and Storybook tests. The one failure is outside this diff: the Storybook `IterationTrace`
  "Joined Activity" story fails at the base, as recorded for the previous plan.
- TUI: the full Lazytest suite passes (2644 cases), and clj format and lint are clean. Typed rows
  save immediately. List editing keeps the 1bf1b2471 key handling. Both are covered in
  `settings_test.clj`.
- Contract and docs: gateway contract, OpenAPI, JSON Schema, docs, manifest and assets tests pass.
- Unchanged: the gateway `PATCH /v1/settings`, the Python SDK and `native_settings_test`.
- No product release, live restart or store submission is part of this task.
