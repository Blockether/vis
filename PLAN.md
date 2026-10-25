# PLAN: One component catalog, view placements and Swarm Forge

Build custom Vis UIs from one static component catalog, then rebuild Swarm Forge on it.

## Context

### Problem

- `view.json` has two component lists. Live views use `live_node` (14 types). Forms use `field`,
  `group` and `decor` (8 field types). `group` is defined twice; `decor` overlaps `paragraph` and
  `heading`.
- A live view cannot take typed input. A form cannot show a table, a log or progress.
- A live view closes when the block that opened it ends (`close-owned` in
  `src/com/blockether/vis/internal/view/core.clj`). No view can outlive a call.
- Live views show only in the step digest, after a click (user rule). A dashboard has no place
  to stay visible.

### Goal

- One `component` union in the contract. Forms and live views use the same components.
- Three placements: `inline` (step digest, as today), `panel` (right side panel of a session)
  and `header` (global pin in the Vis header, across sessions).
- Extension code owns panel and header views. They outlive the callback that opened them.
- Swarm Forge as a Vis extension: role pipeline, board, approvals, clarifications, work queue and
  tasks, in the TUI and the Companion.
- Each new surface passes a Hallmark audit.

### Current paths

| Area | Path |
|---|---|
| Schemas | `packages/vis-contract/resources/vis-contract/schema/view.json` |
| Contract code | `packages/vis-contract/src/com/blockether/vis/contract/view.clj` |
| Engine | `src/com/blockether/vis/internal/view/core.clj` (`normalize-live-view`, `live-view`, `close-owned`, view actions) |
| Gateway | `src/com/blockether/vis/internal/gateway/view.clj`, `gateway/server/views.clj` |
| Python host API | `packages/vis-agent/src/blockether/vis/extension.py` (`LiveView`, `live`, `button`, `_LiveRecorder`) |
| TUI | `apps/vis-tui/src/com/blockether/vis/tui/live_view.clj`, `human_input.clj`, `form.clj`, `header.clj`, `screen.clj` |
| Companion | `apps/vis-companion/src/components/LiveView.tsx`, `HumanInputPrompt.tsx`, `lib/live-view.ts`, `screens/SessionScreen.tsx` |
| Docs | `resources/vis-docs/live-views.md`, `human-input.md`, `extension-api.md`, `sessions-api.md`, `http-api.md`, `site.edn` |
| Swarm Forge | New: `.vis/extensions/swarm_forge.py`, `.vis/extensions/swarm_forge/packs/*.yml`, `.vis/extensions/test_swarm_forge.py` |

### Rejected alternatives

- Keep two lists and add inputs to `live_node` only. This gives two meanings to one field.
- Add a separate "app" schema next to views. This makes a third component list.
- Let an inline live view stay open after its block. This breaks the step digest rule.
- Run roles in tmux as upstream does. Vis already has sessions, drafts and Council.
- Copy upstream scripts or prompts. The upstream repository has no license file. Rebuild the
  behavior from its README only.
- Keep aliases or migrations for `live_node`, `field`, `decor` or `input_node`. The general
  components give the same features. Update the consumers in this repository only.
- Let `python_execution` open `panel` or `header` views. Only extension code can open them.
- Change the `draft_backend` setting of the project for Swarm Forge. It would also force drafts on
  the other sessions of the project.
- Use the `rift` draft backend for roles. Its draft branch reaches the original repository only at
  approval, so the next role cannot merge it.

### Decisions

| Topic | Decision |
|---|---|
| Event stream for header views | Use the existing `GET /v1/events?scope=fleet`. |
| Header pin scope | Per machine. Each gateway has its own pins. |
| Phone panel | A full-page sheet. An icon next to Settings in the session header opens it. |
| Desktop and tablet panel | A resizable right side panel in the session screen. |
| TUI keys | `C-x V` opens the panel, then moves focus between the panel and the transcript. `C-x P` lists header pins. |
| TUI panel size | Use the rule of the project sidebar: 40% of the columns, at least 40. Dock only with 60 chat columns left, else overlay. |
| `card` | A first-class component. |
| View events | Each `change`, `submit` or `activate` runs as an extension callback, with the view session bound. |
| View ownership | An extension and a `key` own one view. Registration code and extension threads can open it. |
| Header limit | Show 4 pins, then a `+N` overflow. A panel with views from several owners shows tabs. |
| Drafts | Always on, only in the sessions that Swarm Forge manages. The project setting does not change. |
| Swarm Forge location | `.vis/extensions/swarm_forge.py` in this repository. |
| Role models | The person chooses a provider and a model for each role from the models that Vis serves. |
| Lieutenant | The session that runs `/swarm start`. It holds the panel and can add tasks. |

## Phase 0: Proposals and Hallmark baseline

**Rationale:** The layouts are new. Agree on them before the engine work.

**Data:** Storybook stories in `apps/vis-companion/src/dev/` under the tab `Views/*`:

- Input components inside a live view.
- Session right side panel on desktop and tablet.
- The phone full-page sheet, opened from the icon next to Settings in the session header.
- Header pins of one machine: the chips, the `+N` overflow and an opened view.
- A Swarm Forge board mockup with fixture data.

TUI mockups render through Lanterna HtmlTerminal and Spel screenshots.

**Acceptance criteria:**

- The user approves the proposals.
- The Hallmark audit has no open finding.
- The rules hold: a label is larger than its caption, there are no secondary labels, folds use ▸ and ▾.

**Unknowns:** none known.

## Phase 1: One component catalog in the contract

**Rationale:** One list is the base for every later phase.

**Data:** `view.json` gets `$defs/component` with one `oneOf`:

| Kind | Components |
|---|---|
| Text | `heading`, `paragraph`, `code`, `divider` |
| State | `status`, `spinner`, `progress`, `stat`, `steps`, `log` |
| Data | `table`, `link`, `card` (new: title, text, tone, meta, `is_selectable`) |
| Input | `plaintext`, `multiline`, `password`, `otp`, `select`, `multiselect`, `checkbox`, `range` |
| Action | `button` (new `action`: `activate` or `submit`) |
| Layout | `group` (one definition, `children`, `direction`, `is_collapsible`, `default_expanded`) |

- Remove `live_node`, `field`, `node`, `decor`, `input_node` and the second `group`. Do not keep aliases.
- Update every consumer in this repository: the engine, the SDK, `.vis/extensions/`, the TUI, the
  Companion, fixtures and docs. Do not migrate extensions in other repositories.
- `input_view.fields` and `live_view.nodes` both become `nodes` of `component`.
- Keep the public projection: input components omit `validate` and `is_secret` on the wire.
- An input component has `value` in a live view. A secret input never carries its value; the host keeps a `secret_handle`.
- `live_view` gets `placement` (`inline`, `panel`, `header`) and an optional `key`.
- `operator_action` gets `change` (`node_id`, `value`) and `submit` with a `node_id` (the submit button).
- Bump the gateway protocol number.

**Acceptance criteria:**

- Skjema validates all fixtures. The contract tests and the schema-derived vocabulary tests pass.
- No consumer reads a removed definition.

**Unknowns:** none known.

## Phase 2: Engine normalization and actions

**Rationale:** The engine validates and stores views. Forms and live views must use one path.

**Data:**

- One component normalizer in `view/core.clj`. `normalize-live-view` and the form normalizer use it.
- `change` patches the input `value` and appends an event to the view event queue.
- `submit` validates the inputs in the nearest `group` of the button. It appends one event with all values.
- `vis.ask` builds its form from the same components. Its submit and cancel stay as today.

**Acceptance criteria:**

- The engine view tests and the gateway `view_test` pass, with new cases for `change` and `submit`.
- An invalid input value gives a field error, not a closed view.
- The secret values of inputs never reach the journal, the snapshot or a log.

**Unknowns:** none known.

## Phase 3: Placements and view lifetime

**Rationale:** Panel and header views must outlive the call that opened them.

**Data:**

- `inline` stays block-owned and closes with its block.
- Only extension code can open `panel` (one session) or `header` (one machine) views. The
  `python_execution` sandbox refuses both placements.
- An extension and a `key` own one view. Registration code, callbacks and extension threads can open it.
- An owned view closes on an explicit close or on extension unload.
- A `header` view needs no session.
- Each `change`, `submit` or `activate` runs as an extension callback with the view session bound.
  So a click can call `Session.send`, `Session.answer` or `vis.sessions.create`.
- The store saves the last picture of an owned view. After a gateway restart, the view shows as
  stale until its extension opens the same `key` again.
- Extension threads can update and read the views that their extension owns.
- Routes: `GET /v1/views/live` lists header views. `GET /v1/sessions/:sid/views/live` gets a `placement` filter.
- Events for header views go on the existing `GET /v1/events?scope=fleet` stream.
- A header shows 4 pins, then `+N`. A panel with views from several owners shows tabs.
- `POST /v1/sessions` gets `draft_backend` (`worktree`, `rift` or `auto`). The value is a setting of
  that one session in `toggles/*overrides*`. The project and global settings do not change.
- `vis.sessions.create(draft_backend=...)` passes it.

**Acceptance criteria:**

- A panel view survives `/reload` and a gateway restart.
- A session created with `draft_backend: worktree` works in a draft. Another session of the same
  project with `draft_backend: off` does not.
- Two surfaces see the same input `value` after a `change`.
- The SDK route-coverage test covers the new routes.

**Unknowns:** none known.

## Phase 4: Python API

**Rationale:** Extension authors build UIs only in Python.

**Data:**

- Builders for each component, for example `vis.plaintext(...)`, `vis.select(...)`, `vis.card(...)`, `vis.group(...)`.
- `vis.ask(title, nodes)` and `vis.live(title, nodes, placement=..., key=...)` take the same builders.
- `LiveView.values()`, `LiveView.events(timeout=None)` and `LiveView.on_event(callback)`.
- Typed handles for `card` and the input components.
- `_LiveRecorder` simulates `change` and `submit`.
- The client SDK gets the new routes.

**Acceptance criteria:**

- The SDK tests pass.
- `test_api_guides.py` passes.
- The test harness drives a full form inside a live view.

**Unknowns:** none known.

## Phase 5: TUI

**Rationale:** The terminal must render every component and both new placements.

**Data:**

- One component registry in `live_view.clj`. Field renderers from `human_input.clj` and `form.clj` move into it.
- A right side panel in the session screen. It takes 40% of the columns, at least 40.
  It docks only when 60 chat columns remain, else it opens as an overlay.
- Header pins in `header.clj`: a chip with the title and the first status or progress.
- Keys:
  - `C-x V` opens the panel, then moves focus between the panel and the transcript.
  - `C-x P` lists the header pins.
  - ← and → fold groups.
  - List the keys in the help.
- Use only glyphs that Monaco 12 draws; box glyphs U+2500-257F are safe.

**Acceptance criteria:**

- The full TUI suite passes.
- HtmlTerminal screenshots of the panel and the pins pass the Hallmark audit.

**Unknowns:** none known.

## Phase 6: Companion

**Rationale:** The app must render every component and both new placements.

**Data:**

- One component registry for `LiveView.tsx` and `HumanInputPrompt.tsx`.
- A resizable right side panel in `SessionScreen.tsx` on desktop and tablet.
- On a phone, a full-page sheet. An icon next to Settings in the session header opens it.
- Header pins in the app header, for the machine of the open session. A tap opens the view.
- Send `change` debounced, and at once on blur and on submit.

**Acceptance criteria:**

- Vitest, Storybook tests, `tsc` and lint pass (`env -u NODE_OPTIONS`).
- Spel checks pass in Chromium, WebKit, an iPhone and an iPad.
- The Hallmark audit has no open finding.

**Unknowns:** none known.

## Phase 7: Swarm Forge extension

**Rationale:** This proves the catalog and the placements with a real product.

**Data:**

- `/swarm start <pack>`, `/swarm stop` and `/swarm status` slash commands.
- Packs as YAML files:
  - `two-pack`: coder, cleaner.
  - `four-pack`: specifier, coder, refactorer, architect.
  - `six-pack`: adds hardening and QA.
- Each role sets its provider, model, receive mode (`task` or `batch`) and propagation
  (`forward-only`, `back-one`, `back-all`).
- The person chooses each provider and model from the models that Vis serves. The panel shows
  them as `select` components.
- `/swarm start` creates each role session with `draft_backend: worktree`. Drafts are always on in
  these sessions only. The project setting does not change.
- Each role is a session that the extension creates. Each role session works in its own draft.
- The extension sends each role turn with `Session.send`. A Pydantic model checks the final answer:
  the commit, a summary and a status (`done`, `blocked` or `question`). A bad answer gets one
  correction turn.
- On `done`, the next role merges the branch `vis/<draft>` of the previous role, then does its task.
- The last role ends at an approval. Approve lands its draft on the target branch.
- Clarifications come from `Session.input_views()` of each role session.
  The answer goes back with `Session.answer`.
- The panel view of the leader session holds these parts:
  - One lane for each role, with a card for each task.
  - Attention: approvals with Approve, Reject and a comment, and clarifications with an inline answer.
  - The work queue table.
  - A New Task form.
  - A Teardown button with a confirmation.
- A header pin shows the swarm state in other sessions.
- `vis.state` keeps the swarm state. After `/reload`, the extension opens its view again with the same `key`.
- Role prompts are new text in Simplified English.

**Acceptance criteria:**

- Pytest with `_LiveRecorder` and fake sessions covers these flows:
  - The pipeline.
  - A rejection and a retry.
  - A clarification.
  - Teardown.
  - A restart.
- A live run of `two-pack` on a small fixture repository completes one task end to end.
  Choose a low-cost provider and model from the models that Vis serves.
- The Hallmark audit of the board passes in the TUI and the Companion.

**Unknowns:** none known.

## Phase 8: Documentation

**Rationale:** People and extension developers must find the new features.

**Data:**

- A `Components` reference page with one section for each component, in both modes.
- `live-views.md`: placements, inputs, the side panel and header pins.
- `human-input.md`: forms use the same components.
- `extension-api.md`, `sessions-api.md` and `http-api.md`: the new calls and routes.
- A Swarm Forge guide in the Extensions module.
- Update `site.edn` and the apropos catalog.

**Acceptance criteria:** `docs-page-canon-test`, `docs-modules-test`, `test_api_guides.py` and link checks pass.

**Unknowns:** none known.

## Phase 9: Verification and closeout

**Rationale:** The change crosses the contract, the engine, the SDK and both clients.

**Data:** the full suite (`clojure -M:test:test-parallel`), the TUI and Companion suites, the
`test-native/` live view tests against a native image, and the Phase 7 live run.

**Acceptance criteria:**

- All checks pass. Report failures outside this work separately.
- Each phase has its own conventional commit on `main`.

**Unknowns:** none known.

## Plan state

| Phase | State |
|---|---|
| 0 Proposals and Hallmark baseline | Done: shipped stories under `Live View/*` replace the mockups; Hallmark audit of the board in Phase 7 |
| 1 Component catalog | Done: schema, fixtures and contract tests |
| 2 Engine | Done: inputs, events and actions in `view/core.clj` |
| 3 Placements and lifetime | Done: panel and header owners, stale restore, fleet events, header routes, `draft_backend`, protocol 19 |
| 4 Python API | Done: SDK builders, `LiveView` events and `on_event`, client routes |
| 5 TUI | Done: side panel, header pins, `C-x V` and `C-x P` (`9842a85f2`) |
| 6 Companion | Done: docked panel, phone sheet, owner tabs, header pins, live inputs (`c2b6701d3`) |
| 7 Swarm Forge | Done: panel lifetime fix (`5b4e6c3e8`), TUI label fix (`e16ee72af`), extension, packs, board story (`e73e16bea`), live two-pack run landed one task |
| 8 Documentation | Done: Components and Swarm Forge pages, placements and inputs in Live views, new calls and routes (`05708e2a2`) |
| 9 Verification | Done: full suite 7275 cases (clock fix `6e0d0ef83`; one unrelated failure), TUI 3028, Companion, tsc, lint, native build and 49 native tests |
