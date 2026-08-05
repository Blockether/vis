# Starting work: machine, project, draft

Decisions about the sessions list — who owns which action, and what a session costs
to start. Photographed proposals live in `src/dev/projectVariants.tsx` and are shot
with `npm run design:shots` (`#/__design`); this file records only what is settled.

## The model everything follows

A **machine** owns **projects**; a **project** owns **sessions**. Every action belongs
to the row that owns the noun it acts on, and no dialog ever asks again for something
the tap already answered.

| Row | Owns | Actions |
| --- | --- | --- |
| Machine (gateway) | its projects | new session · **Switch project** · machine settings |
| Project (folder) | its sessions | new session here · delete sessions (unchanged) |
| Session | itself | open · rename · delete (unchanged) |

## Settled

1. **The project `⋯` does not change.** It keeps deleting the sessions of that project
   and gains nothing. Anything about the *machine* is not its business.
2. **The machine header gets its own `⋯`.** Machine-level actions live there and
   nowhere else, so "which machine" is never a question inside the menu — the header
   you tapped is the answer.
3. **`Switch project` is the name.** Not "new project", not "add folder": from the
   user's side one project is on screen and another one takes its place.
4. **Switch project opens a bottom sheet**, not a dialog. Sheets are what this app
   uses for "pick one of many" on a phone; a dialog is for a decision with two
   buttons. Same surface on desktop, anchored instead of docked.
5. **That sheet is a real file browser** of the machine's own filesystem: descend into
   any folder, climb back through the path, go above `~` to `/`, create a folder and
   choose it in the same breath. It commits a folder — the gateway decides what that
   folder *is* (git repo or not).
6. **Drafts are a preference, not a step.** There is one switch, `Offer drafts`. With
   it off no surface in the app ever asks "the project or a copy?" — not the menu, not
   the sheet, not the header. With it on the fork is offered exactly once, in the same
   surface that picks the destination. A gateway still refuses a draft where a draft is
   impossible (not a git repo) — capability comes from the machine, the *question*
   comes from this switch.
7. **A proposal is photographed before it is built** (see `AGENTS.md` → Companion GUI
   design shots): both viewports, both palettes, plus a falsifying state.

## Non-negotiable states

- **Solo:** one machine paired ⇒ nothing costs a machine question, and no machine
  chrome appears at all — not a header, not a chip, not a disabled control.
- **Unreachable machine:** never offered as a place to start; its own row says so and
  its actions are disabled instead of failing later.
- **Deep path:** the sheet must survive `/`, a home folder with 90 entries, and a path
  too long for 390px — the breadcrumb elides from the left, never the right.

## Open — decide from the board (`#/__design?v=session-ux-board`)

| Question | Options |
| --- | --- |
| Where "New session" lives | **A** shipped (global button ⇒ which machine?) · **B** labelled button per machine · **C** inside the machine `⋯` · **D** `+` on every machine *and* project row |
| How a draft is offered | **E** drafts off (nothing asked) · **F** a second verb in the menu · **G** a chip row in the destination sheet |
| The Switch-project sheet | **H** breadcrumb browser · **I** path field with completion · **J** creating a folder inline |
| Where `Offer drafts` lives | **K** app settings (this device, every machine) · **L** per machine (travels with the gateway) |
