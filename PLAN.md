# PLAN — Let the gateway own the list, so a page is a page

*A pager over a list the client re-filters and re-orders is arithmetic on a lie: the page must be cut by whoever owns the order.*

## Context

**State before.**

- The companion downloads EVERY session of every machine before it paints a page. `listSessions`
  asks for `SESSIONS_PAGE = 100` rows (`apps/vis-companion/src/lib/gateway.ts:509`), hands the first
  window to the screen, and then `drain` walks every remaining window into one array
  (`apps/vis-companion/src/lib/gateway.ts:2406-2420`). On a 1192-session store that is 12 serial
  requests per machine per poll.
- The pager then slices that array LOCALLY: `projectPage(sessions, page, pageSize)`
  (`apps/vis-companion/src/lib/fleet.ts:759-768`), called at
  `apps/vis-companion/src/screens/SessionsScreen.tsx:1887`.
- The gateway can already serve a project's page: `GET /v1/sessions?limit=&after=&root=` cuts one
  project's window by a KEYSET cursor and answers `total`/`next_cursor`/`has_more`
  (`src/com/blockether/vis/internal/gateway/server.clj:1824-1912`,
  `src/com/blockether/vis/internal/gateway/state.clj:4584-4673`). Project headers already come from
  one gateway tally (`state.clj:4693-4797`), not from counting rows.
- It is not used for paging because the list the reader SEES is not the gateway's list. The client
  re-filters it — `sessionIsListed` hides empty, draft-less, unstarred sessions
  (`apps/vis-companion/src/lib/fleet.ts:452-457`) — and re-orders it — `sessionOrder` lifts starred
  rows, then rows holding unsent text, above the gateway's ranking
  (`apps/vis-companion/src/lib/fleet.ts:417-443`), both applied at
  `apps/vis-companion/src/screens/SessionsScreen.tsx:1239-1268`.
- Measured consequence, recorded in the code that gave up on server paging
  (`apps/vis-companion/src/lib/fleet.ts:740-758`): on one machine the gateway counted 1034 sessions
  in a project this list painted 763 of, putting the gateway's last page 27 pages beyond the
  pager's; the last page painted 3 real rows (239px) and swapped them 119ms later for an unrelated
  ten-row window (582px).
- Of the three rules the client adds, TWO are already the gateway's own facts. A star is
  `Session.favorite_rank`, allocated and stored by the gateway — the device holds no copy
  (`apps/vis-companion/src/lib/favorites.ts:1-20`) — and it rides the cheap record
  `session-ranking` already reads (`src/com/blockether/vis/internal/loop.clj:11420`). Emptiness is
  title + `turn_count` + liveness, all of which the ranking holds
  (`state.clj:4481-4515`). Exactly ONE fact is the device's alone: unsent words parked in this
  phone's composer (`apps/vis-companion/src/lib/draft-messages.ts`).
- The page SIZE is already measured off the device rather than configured
  (`apps/vis-companion/src/screens/SessionsScreen.tsx:328-340`, commit `be2e8f22e`), so the client
  has a number to ask the gateway for.

**Root problem.** Two processes each hold a partial definition of one list. The gateway owns the
rows and their ranking; the client owns the filter and the bands. Neither can cut a page, because a
page is only meaningful to whoever owns the whole order — so the client compensates by holding
EVERYTHING in memory, and the header counts, the page count and the last page disagree with each
other by construction.

**What we solve.** The gateway becomes the single owner of the navigator list: which sessions are in
it, in what order. The one fact it cannot know is SENT to it. A project's page is then cut by the
gateway, at a size the device measures, and header counts, `total`, page count and the painted rows
are the same arithmetic.

**What we do not solve.** Search keeps its own contract: `GET /v1/sessions/actions/search` answers a
COMPLETE match set in the gateway's own order, so slicing it locally is honest. Favourite ranks stay
where they are (gateway-allocated). Nothing here changes how a session is opened, deleted or forked.

**Alternatives considered.**

- *Leave paging local and just cache better.* Loses: the counts still disagree — a header from the
  gateway's tally over a list the client re-filters can only agree by accident, which is the report.
- *Move the star and the composer draft into device storage and keep all ordering client-side.* Loses:
  the star was device-local once and produced two truths for one fact — one screen starred, another
  plain (`apps/vis-companion/src/lib/favorites.ts:3-9`). Reverting that trades a fixed bug for a
  worse one, and still requires the whole fleet in memory.
- *Send the device's whole overlay (stars + drafts) on every request.* Loses to sending drafts only:
  stars are ALREADY the gateway's, so the URL would carry a copy of the server's own state, and a
  disagreement between the two copies would silently reorder the list.
- *Band the list by liveness / parked-on-a-human as well.* Loses: rejected before and for the same
  reason — a band that flips mid-turn moves rows under the reader's finger and pushes another row
  out of the window (`state.clj:4486-4496`); those states travel as FIELDS and, for parked runs, as
  the `awaiting` answer beside the window.
- *Replace the pager with an infinite scroller.* Not decided here. It is a different question — how
  a reader travels a long list — and it needs the same server-owned order first. This plan makes
  either possible.

## Phase 1 — Make the gateway the owner of the navigator list

**Rationale.** Without it every page number, page count and header count in the app is derived from
two disagreeing definitions of the same list, and no client can page anything.

**Data.** The wire contract of `GET /v1/sessions` changes: a new request key, and a cursor that
names a row in a BANDED order.

```clojure
(s/def :vis.sessions/dirty
  ;; Comma-separated session ids holding unsent words in THIS device's composer.
  ;; The only part of the navigator list the gateway cannot know. Absent = none.
  (s/nilable string?))

(s/def :vis.sessions/cursor
  ;; "<band>:<sort-key>:<id>" — band 0 starred, 1 dirty, 2 the rest; sort-key is the
  ;; favourite rank inside band 0 and the NEGATED recency in every other band, so one
  ;; ascending [band sort-key id] compare walks the whole list. Replaces
  ;; "<recency-ms>:<id>", which could only address a single-band order.
  (s/and string? #(re-matches #"\d+:-?\d+:.+" %)))

(s/def :vis.sessions/window
  (s/keys :req-un [:vis.sessions/sessions :vis.sessions/awaiting :vis.sessions/total
                   :vis.sessions/limit :vis.sessions/next-cursor :vis.sessions/has-more]))
```

**Acceptance criteria.**

- `src/com/blockether/vis/internal/gateway/state.clj` — `session-ranking` answers ROWS
  (`{:id :band :sort-key :recency-ms}`), drops sessions no navigator paints (no title, no turns, not
  live, not starred, not named by `:dirty`) and bands starred / dirty / rest; the cursor helpers
  read the three-part cursor; `list-sessions-page` takes `:dirty` and keeps the ranking's order
  through decoration; `projects-overview` tallies the same rows.
- `src/com/blockether/vis/internal/gateway/server.clj` — `list-sessions-handler` and
  `projects-overview-handler` parse `dirty`; the 400 names the new cursor shape.
- `src/com/blockether/vis/internal/main.clj` — CLI session-prefix resolution reads ids from a new
  `state/session-ids` instead of the navigator list, so hiding an empty session cannot hide it from
  `--session <prefix>`.
- Tests in `test/com/blockether/vis/internal/gateway/state_test.clj` and `server_test.clj`: an empty
  draft-less session is not listed; a star or a `dirty` id lists it; the bands order; a cursor walk
  crosses a band boundary without duplicating or dropping a row; and `projects-overview`'s
  `session_count` for a root equals `(:total (list-sessions-page :all {:root root}))` — the 1034 vs
  763 report.

**Unknowns.** None.

## Phase 2 — Tell the gateway the one fact it cannot know

**Rationale.** Without it the gateway's list is right for every device and wrong for the one holding
unsent words, and the client must keep its own filter as a safety net — which is the disagreement
this plan removes.

**Data.** None. Phase 1 declared `dirty`; this phase only fills it.

**Acceptance criteria.**

- `apps/vis-companion/src/lib/draft-messages.ts` — expose the ids currently holding unsent words.
- `apps/vis-companion/src/lib/gateway.ts` — `listSessions` sends them as `dirty=`; the value is part
  of the snapshot pin key so a changed overlay cannot be answered from a stale window.
- `apps/vis-companion/src/lib/fleet.ts` / `SessionsScreen.tsx` — `sessionIsListed` and `sessionOrder`
  leave the paint path; `sessionOrder` survives only where the answer is complete (search).
- Test: a fixture with an unsent draft in an empty session paints that row in the dirty band with no
  client-side reordering at all.

**Unknowns.** Does any surface other than the sessions list depend on `sessionIsListed` (the group
delete fan-out reads ALL sessions of a group on purpose — `fleet.ts:459-484`)?

## Phase 3 — Cut a project's page on the gateway, at the size the screen measured

**Rationale.** Without it the app still slices a local array, so a project's page count is the
client's arithmetic and the reader still pays for the whole fleet to see ten rows.

**Data.** None. `limit`/`after`/`root` are Phase 1's contract.

**Acceptance criteria.**

- `apps/vis-companion/src/lib/gateway.ts` — a per-project window read: `(root, limit, after)` in,
  rows + `total` + `next_cursor` out, revalidated by ETag like every other window.
- `apps/vis-companion/src/screens/SessionsScreen.tsx` — `ProjectGroup` reads its page from that,
  passing the measured `useSessionsPerPage()` as `limit`; `first`/`goToPage` become cursor moves.
- `apps/vis-companion/src/lib/fleet.ts` — `projectPage` deleted.
- Test: paging a project issues ONE request per page, and the last page paints the rows the header
  counted — no second paint.

**Unknowns.** How does a page interact with `lib/order-epoch` (the hold that keeps rows still while
a reader looks at them) once arrivals no longer land in a local array?

## Phase 4 — Stop draining the fleet

**Rationale.** Without it every poll still downloads every session of every machine, and the paging
above is a nicer arithmetic over the same download.

**Data.** The device's session snapshot stops being "every row" and becomes the windows this device
has actually read; the snapshot store is on disk, so the shape is declared before the code.

**Acceptance criteria.**

- `apps/vis-companion/src/lib/gateway.ts` — `drain` is deleted; the poll reads the head window and
  the windows a screen asked for.
- `apps/vis-companion/src/screens/SessionsScreen.tsx` — project groups are built from
  `overview.projects` (which already carries every project and its counts), not from the rows held.
- Test: with a fleet of 1200 sessions the sessions screen issues one request per machine per poll,
  and every project header still paints its true count.

**Unknowns.** Which non-list surfaces read `machine.sessions` as if it were the whole fleet
(`Machines.tsx`, `machineProject`, the drafts picker, notifications)?

## State of the plan

DONE.

- Phase 1 — DONE, `4fc1553fc`: the gateway owns the list, `?root=&limit=&after=` is the list a
  client paints, and the cursor is `<band>:<sort-key>:<id>`.
- Phase 2 — DONE, `49d39d5fc`: the device sends `dirty=` and stops filtering and banding for
  itself.
- Phase 3 — DONE, `509950bfc`: a project's page is the gateway's own window —
  `GatewayClient.listProjectPage` asks `?root=&limit=&after=` at the size the screen measured,
  `first`/`goToPage` are cursor moves, and `projectPage` is gone from `src/lib/fleet.ts`.
- Phase 4 — DONE: the drain is deleted — `listSessions` reads the head window alone, project
  groups are built from `overview.projects` so a project exists and counts truthfully before any
  row of it lands, and each group reads its own page (and the next two ahead, so a page turn is a
  paint rather than a wait) into a store it owns. A 1200-session fleet costs one list read per
  machine (`SessionsScreen.projectCounts.test.tsx`).
