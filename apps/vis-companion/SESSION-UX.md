# Starting work: machine, project, draft

Design decisions for starting sessions. Review controls in Storybook with
`npm run storybook` and the complete interaction in the app with `npm run dev`.

## Machine, project and session actions

A machine contains projects; each project contains sessions. Put actions on
the relevant row. Do not ask users to reselect a machine or project already
identified by their selection.

| Row | Owns | Actions |
| --- | --- | --- |
| Machine (gateway) | its projects | new session · new session in a draft · **Switch project** · machine settings |
| Project (folder) | its sessions | delete sessions (unchanged) |
| Session | itself | open · rename · delete (unchanged) |

## Decisions

1. The project `⋯` menu continues to delete that project's sessions. Do not add
   machine-level actions to it.
2. Add one `⋯` menu to the machine header for machine-level actions (board C).
   The selected header identifies the machine; no extra selector is needed.
3. `New session` uses the machine's current project. Show that project in the
   entry's hint, such as `in vis · ~/vis · last used 7m ago`.
4. Put `New session in a draft…` in the same menu (board F). Hide it when
   `Offer drafts` is disabled.
5. Label the project-selection action `Switch project`.
6. On phones, open project selection in a bottom sheet. Use an anchored sheet
   on desktop. Reserve two-button dialogs for confirmation.
7. The sheet browses the machine's filesystem with breadcrumbs (board H).
   Users can open folders, select ancestors, navigate above home to `/`,
   create a folder and select it. The gateway determines whether the selected
   folder is a Git repository. Mark known projects in the list.
8. A pencil icon switches the header between breadcrumbs and an editable path.
   Typing filters the list. Keep the icon selected in path-entry mode, with a
   full touch target and no visible border.
9. `Offer drafts` is an app setting shared across machines on that device
   (board K). When disabled, do not ask whether to use the project or a copy.
   The gateway still validates whether a draft is supported.
10. Review proposals at phone and desktop sizes, in light and dark themes,
    including a state that could invalidate the design. Follow the
    [design skill](../../.vis/skills/design/SKILL.md) for current artifact rules.
11. Use production components in proposals. On phones, menus use bottom sheets
    with a scrim, not floating popovers. Use `Button`, `MachineMark` and
    `PencilIcon` rather than custom copies or font characters. Keep labels in
    sentence case and emphasize only the primary action.
12. Import existing machine, project and session components. Preserve their
    row heights, column alignment and unbroken count values. If multiple
    examples share class strings, define them once. Reuse the input, badge,
    row and settings-panel styles rather than creating proposal-only styles.

## Required states

- **One machine:** omit redundant machine selection and machine headers. Put
  the same `⋯` actions in the top bar.
- **Unreachable machine:** identify it as unavailable and disable actions that
  require a connection.
- **Deep path:** support `/`, a home folder with 90 entries and paths wider
  than 390px. Truncate breadcrumbs from the left, preserving the current folder.

## Recorded proposal

The proposal used `#/__design?v=session-ux-board` to show the machine menu,
draft action, folder browser, path field, folder creation, preference switch
and single-machine state. `#/__design?v=session-flow&state=…` showed individual
states at full-screen size. These are historical proposal routes, not public
application routes.
