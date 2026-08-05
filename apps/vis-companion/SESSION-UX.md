# Starting work: machine, project, draft

Decisions about the sessions list — who owns which action, and what a session costs
to start. The chosen flow is photographed in `src/dev/projectVariants.tsx` and shot
with `npm run design:shots` (`#/__design?v=session-ux-board`); this file records only
what is settled.

## The model everything follows

A **machine** owns **projects**; a **project** owns **sessions**. Every action belongs
to the row that owns the noun it acts on, and no dialog ever asks again for something
the tap already answered.

| Row | Owns | Actions |
| --- | --- | --- |
| Machine (gateway) | its projects | new session · new session in a draft · **Switch project** · machine settings |
| Project (folder) | its sessions | delete sessions (unchanged) |
| Session | itself | open · rename · delete (unchanged) |

## Settled

1. **The project `⋯` does not change.** It keeps deleting the sessions of that project
   and gains nothing. Anything about the *machine* is not its business.
2. **The machine header gets its own `⋯`, and it is the ONLY new control.** (Board: C.)
   Machine-level actions live there and nowhere else, so "which machine" is never a
   question inside the menu — the header you tapped is the answer. No labelled button,
   no `+` on every row: the list keeps its density and the rarest verbs cost the same
   tap as the commonest one.
3. **`New session` starts in the machine's current project**, named in the entry's own
   hint (`in vis · ~/vis · last used 7m ago`). Starting is one tap; *changing* which
   project that is has its own verb, below.
4. **`New session in a draft…` is a second verb in that menu.** (Board: F.) Two entries
   that differ in four words, and with `Offer drafts` off the second one is simply not
   rendered — nobody is asked a question they turned off.
5. **`Switch project` is the name.** Not "new project", not "add folder": from the
   user's side one project is on screen and another one takes its place.
6. **Switch project opens a bottom sheet**, not a dialog. Sheets are what this app
   uses for "pick one of many" on a phone; a dialog is for a decision with two
   buttons. Same surface on desktop, anchored instead of docked.
7. **That sheet is a breadcrumb browser** of the machine's own filesystem. (Board: H.)
   Descend into any folder, tap an ancestor in the path to climb, go above `~` to `/`,
   create a folder and choose it in the same breath. It commits a folder — the gateway
   decides what that folder *is* (git repo or not). Folders already known as projects
   are badged so the common case is recognised, not typed.
8. **A pencil in that same header types the path.** The breadcrumb is for recognition,
   the pencil is for people who know where they are going: it replaces the crumbs with
   the path itself, the list below narrows to matches as you type, and the pencil stays
   lit as the way back to browsing. Two modes, one header, one control to switch them.
   The pencil is **ink, never a box**: a bare glyph at a full hit box, because a bordered
   button beside the path reads as a second, competing action.
9. **Drafts are a preference, not a step.** There is one switch, `Offer drafts`, in
   **app settings** — this device, every machine. (Board: K.) With it off no surface in
   the app ever asks "the project or a copy?". A gateway still refuses a draft where a
   draft is impossible (not a git repo) — capability comes from the machine, the
   *question* comes from this switch.
10. **A proposal is photographed before it is built** (see `AGENTS.md` → Companion GUI
    design shots): both viewports, both palettes, plus a falsifying state.

## Non-negotiable states

- **Solo:** one machine paired ⇒ nothing costs a machine question, and no machine
  chrome appears at all — not a header, not a chip, not a disabled control. The fleet
  bar *is* the machine and carries the same `⋯`, so the flow above is unchanged while
  the fleet vocabulary disappears.
- **Unreachable machine:** never offered as a place to start; its own row says so and
  its actions are disabled instead of failing later.
- **Deep path:** the sheet must survive `/`, a home folder with 90 entries, and a path
  too long for 390px — the breadcrumb elides from the left, never the right.

## The chosen flow, photographed

`#/__design?v=session-ux-board` (its own viewport) walks the seven steps: the machine
menu, the draft verb inside it, the browsing sheet, the pencil's path field, the inline
folder, the app switch, and the solo falsifier. Each step is also a state of
`#/__design?v=session-flow&state=…` for a full-screen look.
