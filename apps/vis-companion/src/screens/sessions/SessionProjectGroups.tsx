/** One project's band, its session rows, and how that project is paged. */

import { memo, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react';

import {
  SessionRow,
  type SessionListActions,
  type SessionRowDeletion,
} from '../../components/SessionList';
import {
  HeaderActions,
  HeaderTally,
  NewSessionButton,
  Pager,
  ProjectCrumb,
  ProjectStatusCounts,
  SectionHeader,
} from '../../components/SessionNavigator';
import { TextButton } from '../../components/ui';
import {
  draftMessageKey,
  EMPTY_DRAFT_MESSAGE,
  type DraftMessageStore,
} from '../../lib/draft-messages';
import { isFavorite } from '../../lib/favorites';
import {
  machineKey,
  machineLabel,
  sessionMillis,
  type FleetMachine,
  type ProjectGroupView,
} from '../../lib/fleet';
import type { GatewayClient, ProjectWindows, SessionMatch } from '../../lib/gateway';
import { holdOrder, type OrderEpoch } from '../../lib/order-epoch';
import { compactProjectPath } from '../../lib/path';
import { projectFoldKey, readProjectFold, writeProjectFold } from '../../lib/project-fold';
import type { GatewayConn, Session } from '../../lib/types';

type SessionClient = (conn: GatewayConn) => GatewayClient;

/** Stable services and view facts every session row in these bands shares. */
export type SessionRowsContext = {
  getClient: SessionClient;
  drafts: DraftMessageStore;
  matches: Map<string, SessionMatch> | null;
  needle: string;
  actions: SessionListActions;
};

/** The reader agreement shared by every project on one machine. */
export type ProjectGroupReading = {
  pageSize: number;
  epoch: OrderEpoch | null;
  admitted: ReadonlySet<string>;
  isVisible: boolean;
  pendingByRoot: ReadonlyMap<string, readonly string[]>;
  acceptUpdates: (ids: readonly string[]) => void;
};

/** One project-creation lifecycle, shared so headers report the request they started. */
export type ProjectCreation = {
  state: { at: string | null; label: string } | null;
  start: (conn: GatewayConn, root: string) => Promise<void>;
};

// A page whose read has not answered yet paints nothing rather than rows from
// another place in the project (`ProjectGroup`).
const NO_ROWS: Session[] = [];

/** Two project pages are read ahead after the visible page answers. */
const PAGES_AHEAD = 2;

// Memoised: a 5.5s poll that changes nothing returns the SAME row objects
// (`reconcileSessions`), so an unchanged group must not re-render its rows.
export const ProjectGroup = memo(function ProjectGroup({
  group,
  machine,
  context,
  reading,
  creation,
  initiallyOpen,
}: {
  /** Canonical gateway-owned project identity, counts, and held preview rows. */
  group: ProjectGroupView;
  /** The machine is the project namespace and owns the list validator. */
  machine: Pick<FleetMachine, 'conn' | 'sessions'>;
  context: SessionRowsContext;
  reading: ProjectGroupReading;
  creation: ProjectCreation;
  /** Only the first project in the machine's own order opens by default. */
  initiallyOpen: boolean;
}) {
  const { label: project, root, sessions, tally } = group;
  const { conn, sessions: list } = machine;
  const { getClient, drafts, matches, needle, actions: rowActions } = context;
  const { pageSize, epoch, admitted, isVisible, pendingByRoot, acceptUpdates } = reading;
  const pendingIds = pendingByRoot.get(root) ?? [];
  const hasPending = pendingIds.length > 0;
  const { state: creating, start: onNewSession } = creation;
  const base = useMemo(() => getClient(conn).base, [conn, getClient]);
  const pendingDeleteId =
    rowActions.deletion.target && machineKey(rowActions.deletion.target.conn) === machineKey(conn)
      ? rowActions.deletion.target.session.id
      : null;

  // A PROJECT'S PAGE IS CUT BY WHOEVER OWNS THE LIST.
  //
  // What this group paints is the gateway's answer to `?root=&limit=&after=` —
  // this project, at the size this screen measured, ordered and banded there
  // (`GatewayClient.listProjectPage`). It used to be a slice of an array this
  // device had downloaded, re-filtered and re-ordered for itself: the gateway
  // counted 1034 sessions in a project this list painted 763 of, page one
  // therefore disagreed with every page after it, and the last page painted its
  // three real rows and swapped them 119ms later for an unrelated ten.
  //
  // A PAGE IS A PLACE IN THE PROJECT, NOT A NUMBER. The step belongs to the
  // screen (`useSessionsPerPage`), so it changes when the device is rotated or
  // the window opened wider — and page 77 of 102 then names a different stretch
  // of the history. What the reader is holding is the FIRST ROW on screen, so its
  // INDEX is what is kept and the page is asked for from there again at the new
  // step; keeping the number would have sent a reader deep in a project back to
  // page one through the clamp below.
  const [first, setFirst] = useState(0);
  const page = Math.floor(first / pageSize) + 1;
  const goToPage = (next: number) => setFirst((next - 1) * pageSize);
  // The page that index FALLS IN, from its first row: the reader keeps a ROW, and
  // the page is the grid that row lands on at the step the screen now holds.
  const start = (page - 1) * pageSize;
  // WHERE A PAGE BEGINS IS A ROW, NOT A COUNT: first row index → the cursor of the
  // row before it, learned one answer at a time as the project is walked. Index 0
  // is the top of the project and needs no cursor at all. A cursor NAMES a row, so
  // the place survives everything the fleet does under the reader, which an offset
  // into an ordering recomputed per request could not (`state/list-sessions-page`).
  const cursors = useRef(new Map<number, string>([[0, '']]));
  // Deeper windows belong to this group. The client also snapshots each project's
  // head, so projects outside the machine's head window survive a remount.
  const pins = useRef<ProjectWindows>(new Map());
  // The question the last read asked, so a page TURN paints what is already held and a
  // poll that only moved the list under an unchanged page repaints nothing.
  const asked = useRef('');
  // The page LAST ANSWERED, whichever it is: its place, rows, and the project's own
  // count travel together. A slow answer must never put one page's number over another's rows.
  const [paged, setPaged] = useState<{
    start: number;
    rows: Session[];
    total: number;
    awaiting: Session[];
  } | null>(() => {
    const held = getClient(conn).heldProjectPage(root, pageSize, '', new Map());
    return held ? { start: 0, rows: held.rows, total: held.total, awaiting: held.awaiting } : null;
  });
  // A project FOLDS, and only the top one starts open: the screen's job is to show
  // the work that moved last, not four checkouts' history at once. What the reader
  // folds afterwards is theirs and outlives this component — see `lib/project-fold`.
  const foldKey = projectFoldKey(machineKey(conn), root);
  const [isOpen, setIsOpen] = useState(() => readProjectFold(foldKey) ?? initiallyOpen);
  // A fold is a DECISION, not a frame: it is written where it was made, so the next
  // screen built from nothing starts where this reader left it.
  const fold = (open: boolean) => {
    writeProjectFold(foldKey, open);
    setIsOpen(open);
  };
  // A project with no sessions has nothing to reveal. It still names the destination
  // of its New-session action, but it is not a fold and must not wear disclosure furniture.
  const hasSessions = tally.count > 0;
  // WHERE THIS CHECKOUT IS, and only when that is not what its NAME already said.
  //
  // The path exists to tell two `vis` checkouts apart. A project that sits directly in
  // home under its own folder name answers that question with the name itself, so
  // `vis` wore `~/vis` under it — the same word twice, in the line that also has to
  // carry the count and the live states, and on a 393px phone the address won that
  // fight and truncated to `~/v…`. `HeaderTitle` already refuses exactly this for a
  // machine whose address IS its name; a project is the same rule one level down.
  const where = compactProjectPath(root, project);
  const qualifierPath = where
    ? where === project || where === `~/${project}`
      ? ''
      : where
    : 'No workspace path';
  // A FILTER is a fleet-wide question and its answer may not sit behind a fold: while
  // a query is on, every project that still has rows shows them. The fold the reader
  // set is untouched and is back the moment the query is.
  const isShowing = hasSessions && (isOpen || needle !== '');
  const searching = needle !== '';
  // THE PAGE IS ASKED FOR, NOT SLICED.
  //
  // A folded group asks for nothing — the read IS the paint, and a project nobody
  // opened has no page to be wrong. A query is the one answer this device holds
  // COMPLETE (the search fanout narrows a list it was given), so its pages are cut
  // below instead of read here.
  //
  // AND THE PAGES AFTER IT ARE READ AHEAD. Turning a page is the one thing a reader
  // asks for that this group could already know, so the next `PAGES_AHEAD` windows are
  // read once the visible one has landed — serially, behind it, never beside it, and
  // only for a group that is on the glass and open. What it buys is each of those
  // pages' CURSOR and a validator for it: the turn then paints held rows in the frame
  // of the tap and confirms them with one conditional read, instead of standing on the
  // page before it for a whole round trip.
  useEffect(() => {
    if (!isVisible || !isShowing || searching) return;
    const control = new AbortController();
    let live = true;
    // The deepest place this group has walked to that is not past the one asked
    // for. Page one needs no cursor; a number tapped out of nowhere is asked for
    // from there with a `limit` spanning the gap, and the TAIL of that one answer
    // is the page — a cursor can only ever be the row a page ended on.
    let from = start;
    while (from > 0 && !cursors.current.has(from)) from -= 1;
    const after = cursors.current.get(from) ?? '';
    const limit = start - from + pageSize;
    const api = getClient(conn);
    // A page this group already HOLDS paints in the frame of the tap that asked for
    // it. Only when the question changed: a poll that moved the list under an unchanged
    // page must not repaint it from a validator that is about to be revalidated anyway.
    const question = `${limit}\u0000${after}`;
    if (question !== asked.current) {
      asked.current = question;
      const held = api.heldProjectPage(root, limit, after, pins.current);
      if (held)
        setPaged({
          start,
          rows: held.rows.slice(start - from),
          total: held.total,
          awaiting: held.awaiting,
        });
    }
    void (async () => {
      try {
        const answer = await api.listProjectPage(
          root,
          limit,
          after,
          pins.current,
          control.signal,
          start === 0,
        );
        if (!live) return;
        if (answer.nextCursor) cursors.current.set(from + answer.rows.length, answer.nextCursor);
        setPaged({
          start,
          rows: answer.rows.slice(start - from),
          total: answer.total,
          awaiting: answer.awaiting,
        });
        // Behind the answer, never beside it: the reader's own page is never waiting on
        // a read taken for a page they have not asked for. A project that has ended
        // (`nextCursor === ''`) is not read past.
        let at = from + answer.rows.length;
        let cursor = answer.nextCursor;
        for (let ahead = 0; ahead < PAGES_AHEAD && cursor; ahead += 1) {
          const next = await api.listProjectPage(
            root,
            pageSize,
            cursor,
            pins.current,
            control.signal,
          );
          if (!live) return;
          if (next.rows.length === 0) break;
          if (next.nextCursor) cursors.current.set(at + next.rows.length, next.nextCursor);
          at += next.rows.length;
          cursor = next.nextCursor;
        }
      } catch {
        // A read that failed, or one this effect replaced, leaves the page already
        // on screen standing: an unreachable machine is said once, by its own band.
      }
    })();
    return () => {
      live = false;
      control.abort();
    };
  }, [conn, root, start, pageSize, isVisible, isShowing, searching, list, getClient]);
  // The count under the header and the pages beside it are ONE number — the
  // project's own total, as the gateway counted it. Under a query the complete
  // answer is on this device, and then what is on screen is the honest count.
  const total = searching ? sessions.length : (paged?.total ?? tally.count);
  const pageCount = Math.max(1, Math.ceil(Math.max(total, 1) / pageSize));
  const shownPage = searching
    ? Math.min(page, pageCount)
    : Math.min(paged ? Math.floor(paged.start / pageSize) + 1 : 1, pageCount);
  // A PAGE ARRIVES OVER THE ONE BEFORE IT, NEVER OVER A HOLE. The read a step takes
  // lands a beat after the tap, and a group that painted nothing meanwhile lost its
  // rows, its height AND the pager the thumb had just pressed — the reflow this seam
  // exists to end. The last page answered therefore stays on the glass until the next
  // one lands, and its number stays attached to those rows for that whole wait. Only
  // page ONE has something else to open on: what this device holds of this project,
  // out of the machine's own window.
  const pageRows = paged?.rows ?? null;
  // Those held rows are a HEAD, not a page — a project deeper than the machine's window
  // has none of them — so they are only painted when they can fill the page. A group
  // that would otherwise paint three rows and swap them for twelve waits the one read
  // out instead, which is the reflow this seam exists to end.
  const held = sessions.slice(0, pageSize);
  const painting = searching
    ? sessions.slice((shownPage - 1) * pageSize, shownPage * pageSize)
    : (pageRows ??
      (start === 0 && held.length >= Math.min(pageSize, tally.count) ? held : NO_ROWS));
  // A ROW THIS DEVICE JUST CHANGED IS THE ROW IT PAINTS. A star or a rename is
  // echoed into the list this screen holds the moment the gateway answers the
  // PATCH; the window carrying it is a read of its own and lands a beat later, so
  // a page takes the held copy of any row it has one for — the same row, the same
  // identity the rest of the screen is rendering, wearing the mark the tap just
  // made. Order is never taken from there: WHERE a row sits is the answer above.
  const local = useMemo(
    () => new Map(sessions.map((session) => [session.id, session])),
    [sessions],
  );
  // NOTHING MOVES WHILE THE READER IS LOOKING AT IT (`lib/order-epoch`). The list
  // of projects is held by the screen; a page read from the gateway is held HERE,
  // or a turn finishing on another machine would slide this page under the thumb on
  // the next poll. A search answer arrives held already, and a row this reader
  // started or is holding words for is admitted rather than parked behind the pill.
  const rows = useMemo(() => {
    const shown = painting.map((session) => local.get(session.id) ?? session);
    if (searching) return shown;
    const held = holdOrder(
      epoch,
      shown,
      (session) => ({ id: session.id, millis: sessionMillis(session) }),
      admitted,
    ).rows;
    // A RUN PARKED ON A HUMAN IS SAID WHERE IT LIVES, however deep it sits. The
    // gateway carries the project's parked sessions BESIDE every window it cuts
    // (`ProjectPage.awaiting`) instead of lifting them into the order, so they are
    // pinned above the page here: the header said `1 needs input` while the row
    // it counted sat forty pages down and no page showed INPUT NEEDED. A parked row
    // the page already holds is painted once, in its place.
    const onPage = new Set(held.map((session) => session.id));
    const parked = (paged?.awaiting ?? NO_ROWS)
      .filter((session) => !onPage.has(session.id))
      .map((session) => local.get(session.id) ?? session);
    return parked.length === 0 ? held : [...parked, ...held];
  }, [searching, painting, local, epoch, admitted, paged]);
  useEffect(() => {
    // The project shrank under the pager (a deletion, a smaller step): the page that
    // no longer exists becomes the first one rather than the last one a reader never
    // asked for.
    if (page > pageCount) setFirst(0);
  }, [page, pageCount]);
  // A star PINS its row to the top of the project, and the top of the project is
  // PAGE ONE — so a row starred from any other page LEFT the page under the thumb
  // that starred it. Nothing was broken about the mark: the row carrying it was two
  // pages away, which is why it only ever turned up after the screen was left and
  // re-entered on page one.
  // Regression, user report: after starring, no star appeared on the session row
  // until the session was opened and closed again.
  // The group FOLLOWS the row it moved — page one is where the list puts it, and the
  // row is brought back under the eye that starred it.
  //
  // The flip is read off the ROWS ON SCREEN, and only for a row that was on the page
  // before and after: a page TURN takes every starred row off the page at once, and
  // reading that as an unstar would have snapped the reader straight back to page one.
  const marks = useMemo(
    () => new Map(rows.map((session) => [session.id, isFavorite(session)])),
    [rows],
  );
  const wasMarked = useRef(marks);
  const rowsRef = useRef<HTMLDivElement>(null);
  const following = useRef<string | null>(null);
  // Before paint: the reader must never see a frame of the page the row just left.
  useLayoutEffect(() => {
    const before = wasMarked.current;
    wasMarked.current = marks;
    // One tap flips one row. UNSTARRING moves a row just as far — down, out of the
    // pinned band — so it is followed the same way instead of being dropped
    // wherever the ordering sends it.
    let flipped: string | null = null;
    for (const [id, marked] of marks) {
      const was = before.get(id);
      if (was !== undefined && was !== marked) flipped = id;
    }
    if (!flipped) return;
    following.current = flipped;
    setFirst(0);
  }, [marks]);
  // The row may land on the page already shown (starred from page one) or on the
  // one this group just walked to; either way it is placed back under the eye on the
  // commit that paints it.
  //
  // THE PIN IS A PLACE, NEVER AN ANIMATION — the same rule the drawer's way home
  // already lives by (`SwipeActions`). `scrollIntoView` walks EVERY scrollable
  // ancestor, and the FIRST one it meets is the row's own swipe track: the mandatory
  // snap track the verb that fired this pin has just sent home, in this same commit.
  //
  // Regression, user report on iOS (paraphrased: slide the LAST row open, tap the
  // star, the row moves up wearing no mark, and the next slide shows the mark and the
  // strip saying two different things): an animated scroll inside a mandatory snap
  // track is what WebKit is free to swallow, and a drawer left standing over its row
  // hides the row's LEADING edge — which is exactly where the mark that tap just left
  // sits. Measured in WebKit on this screen at 390px, same track, same call: an open
  // track (216px) was still at 163px 150ms after `behavior: 'smooth'` was asked for
  // and only reached home ~900ms later, against home in the SAME FRAME for
  // `behavior: 'auto'`.
  useEffect(() => {
    const id = following.current;
    if (!id || !rows.some((session) => session.id === id)) return;
    following.current = null;
    rowsRef.current?.querySelector(`[data-session-id="${CSS.escape(id)}"]`)?.scrollIntoView({
      block: 'nearest',
      inline: 'nearest',
      behavior: 'auto',
    });
  }, [rows]);

  const pager =
    pageCount > 1 ? (
      <Pager
        page={shownPage}
        pageCount={pageCount}
        disabled={!isShowing}
        onPage={goToPage}
        label={`${project} sessions`}
      />
    ) : null;

  const qualifier = (
    <span className="flex max-w-full min-w-0 items-center gap-2">
      {qualifierPath && (
        <span className="min-w-0 shrink-[8] truncate @max-md:hidden">
          {qualifierPath}
          <span aria-hidden> ·</span>
        </span>
      )}
      <span className="min-w-0 truncate">
        <HeaderTally count={tally.count} unit="session" />
        {hasPending && (
          <>
            <span aria-hidden> | </span>
            <TextButton
              className="pointer-events-auto relative whitespace-nowrap"
              aria-label={`Show ${pendingIds.length} newer ${pendingIds.length === 1 ? 'session' : 'sessions'}`}
              onClick={() => {
                acceptUpdates(pendingIds);
                setFirst(0);
                fold(true);
              }}
            >
              {pendingIds.length} new
            </TextButton>
          </>
        )}
        <ProjectStatusCounts live={tally.live} awaiting={tally.awaiting} unread={tally.unread} />
      </span>
    </span>
  );

  return (
    <>
      {/* The rail's index finds this band by the two facts that identify it, and the
        only two a jump can be sure of: which machine, and which root. */}
      <section
        // A compact gap and the incoming header rule separate projects, not individual rows.
        aria-label={`${project} sessions`}
        data-machine={machineKey(conn)}
        data-project-root={root}
        className="[&+&]:pt-2"
      >
        <SectionHeader navigation={pager}>
          <ProjectCrumb
            name={project}
            qualifier={qualifier}
            qualifierTitle={root}
            disclosure={
              hasSessions
                ? {
                    isOpen: isShowing,
                    onToggle: () => fold(!isShowing),
                    label: `${isShowing ? 'Collapse' : 'Expand'} ${project}`,
                  }
                : null
            }
          />
          <HeaderActions align="center">
            <NewSessionButton
              machine={machineLabel(conn)}
              where={project}
              isBusy={creating?.at === `${base}\u0000${root}`}
              onPress={() => void onNewSession(conn, root)}
            />
          </HeaderActions>
        </SectionHeader>
        {/* The header closes the band; rows draw only separators between sessions. */}
        {isShowing && rows.length > 0 && (
          <div ref={rowsRef}>
            {rows.map((session) => {
              const pending = pendingDeleteId === session.id;
              const deletion: SessionRowDeletion = pending
                ? {
                    isBusy: rowActions.deletion.isBusy,
                    error: rowActions.deletion.error,
                    confirm: rowActions.deletion.confirm,
                    cancel: rowActions.deletion.cancel,
                  }
                : null;
              return (
                <SessionRow
                  key={session.id}
                  session={session}
                  draft={drafts[draftMessageKey(base, session.id)] ?? EMPTY_DRAFT_MESSAGE}
                  conn={conn}
                  match={matches?.get(session.id) ?? null}
                  needle={needle}
                  commands={rowActions.commands}
                  deletion={deletion}
                />
              );
            })}
          </div>
        )}
      </section>
    </>
  );
});
