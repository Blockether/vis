/** One project's band, its session rows, and how that project is paged. */

import { memo, useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react';

import {
  SessionRow,
  type SessionListActions,
  type SessionRowCommands,
  type SessionRowDeletion,
} from '../../components/SessionList';
import {
  HeaderActions,
  HeaderMeta,
  HeaderTally,
  LIST_MARK,
  NewSessionButton,
  Pager,
  ProjectCrumb,
  ProjectStatusCounts,
  SectionHeader,
} from '../../components/SessionNavigator';
import { Menu, MenuBack, MenuHeading, MenuItem, MenuNote, MENU_WIDTH } from '../../components/Menu';
import { ChevronIcon, DotsIcon } from '../../components/icons';
import { Button, IconButton, Input, TextButton } from '../../components/ui';
import { menuPosition, type MenuPosition } from '../../lib/anchored-menu';
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
import {
  GatewayError,
  type GatewayClient,
  type ProjectWindows,
  type SessionMatch,
} from '../../lib/gateway';
import { GROUP_COLORS, groupColor, groupSwatch } from '../../lib/group-colors';
import { holdOrder, type OrderEpoch } from '../../lib/order-epoch';
import { compactProjectPath } from '../../lib/path';
import {
  groupFoldKey,
  projectFoldKey,
  readProjectFold,
  writeProjectFold,
} from '../../lib/project-fold';
import type { GatewayConn, Session, SessionGroup } from '../../lib/types';

/** Where inside the group sheet the reader is standing (`ProjectGroup`). */
type MenuStep =
  | { kind: 'root' }
  | { kind: 'new' }
  | { kind: 'group'; id: string }
  | { kind: 'rename'; id: string }
  | { kind: 'delete'; id: string }
  | { kind: 'move'; sid: string };

/** One group as a band paints it: the gateway's row, or what a row itself said. */
type GroupBandView = { id: string; name: string; color: string | null; count: number };

/**
 * The row to paint for one session: what the list holds, unless this band has just
 * re-filed it and the list's own window has not caught up yet (`ProjectGroup`).
 */
function settled(
  session: Session,
  local: Map<string, Session>,
  refiled: ReadonlyMap<string, Session>,
): Session {
  const current = local.get(session.id) ?? session;
  const held = refiled.get(session.id);
  return held && held.group_id !== current.group_id ? held : current;
}

/** What a session is called in a list of choices. */
function rowTitle(session: Session): string {
  const title = typeof session.title === 'string' ? session.title.trim() : '';
  return title === '' ? 'Untitled session' : title;
}

/** A group's colour, as the one mark that carries it. */
function Swatch({ color }: { color: string | null }) {
  return <span aria-hidden className={`size-2.5 shrink-0 ${groupSwatch(color)}`} />;
}

/** The one field a group action needs, committed by Enter or by the cell beside it. */
function NameForm({
  label,
  value,
  commit,
  isBusy,
  onChange,
  onCommit,
}: {
  /** What is being typed, for a reader who cannot see the field: `Group name`. */
  label: string;
  value: string;
  /** The verb on the commit: `Create`, `Rename`. */
  commit: string;
  isBusy: boolean;
  onChange: (value: string) => void;
  onCommit: () => void;
}) {
  return (
    <form
      className="flex items-center gap-2 border-b border-dialog-edge p-3"
      onSubmit={(event) => {
        event.preventDefault();
        if (value.trim() !== '') onCommit();
      }}
    >
      <Input
        autoFocus
        aria-label={label}
        placeholder={label}
        value={value}
        className="flex-1"
        onChange={(event) => onChange(event.target.value)}
      />
      <Button type="submit" variant="primary" disabled={isBusy || value.trim() === ''}>
        {commit}
      </Button>
    </form>
  );
}

/**
 * One group's own band, inside its project's list.
 *
 * A level quieter than the project header above it, and deliberately so: the project
 * owns the boundary rule, the counts and the pager (a group is not paged — it is a
 * place inside the page), while a group owns its name, its colour and its own fold.
 */
function GroupBand({
  name,
  color,
  count,
  isOpen,
  onToggle,
  machine,
  onActions,
  onNewSession,
  isCreating = false,
  onDropSession,
}: {
  name: string;
  color: string | null;
  /** The gateway's tally for the WHOLE group, which is also what stands under it. */
  count: number;
  isOpen: boolean;
  onToggle: () => void;
  /** The machine this band lives on, for the accessible name of its own plus. */
  machine: string;
  onActions: (anchor: HTMLElement) => void;
  /** Start a session INSIDE this group. Absent: the band offers no plus. */
  onNewSession?: () => void;
  /** A create started from THIS band is still in flight. */
  isCreating?: boolean;
  /** Called with the session a reader DROPPED on this band. Absent: the band takes no drops. */
  onDropSession?: (sid: string) => void;
}) {
  // A ROW DRAGGED ONTO THE BAND IS FILED INTO IT, and the band lights while the pointer
  // is over it: the reader sees WHERE the session lands before letting go.
  const [isOver, setIsOver] = useState(false);
  return (
    <div
      className={`flex items-stretch border-t border-edge ${isOver ? 'bg-white/10' : ''}`}
      onDragOver={(event) => {
        if (!onDropSession) return;
        event.preventDefault();
        event.dataTransfer.dropEffect = 'move';
        setIsOver(true);
      }}
      onDragLeave={() => setIsOver(false)}
      onDrop={(event) => {
        if (!onDropSession) return;
        event.preventDefault();
        setIsOver(false);
        const sid = event.dataTransfer.getData('text/plain');
        if (sid) onDropSession(sid);
      }}
    >
      {/* The band and its rows share one coloured edge, so a group reads as a place
          rather than as a caption. */}
      <span aria-hidden className={`w-1 shrink-0 ${groupSwatch(color)}`} />
      <button
        type="button"
        aria-expanded={isOpen}
        aria-label={`${isOpen ? 'Collapse' : 'Expand'} ${name}`}
        onClick={onToggle}
        className="flex min-w-0 flex-1 items-center gap-2 py-1.5 pl-4 text-left focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-inset focus-visible:ring-white"
      >
        <span className={LIST_MARK}>
          <ChevronIcon open={isOpen} className="size-3 text-dialog-hint" />
        </span>
        <Swatch color={color} />
        <span className="min-w-0 truncate font-mono text-ui font-bold text-white">{name}</span>
        <HeaderMeta>
          <HeaderTally count={count} unit="session" />
        </HeaderMeta>
      </button>
      <HeaderActions align="center">
        <IconButton
          label={`Actions for ${name}`}
          variant="quiet"
          density="band"
          aria-haspopup="dialog"
          onClick={(event) => onActions(event.currentTarget)}
        >
          <DotsIcon className="size-3.5" />
        </IconButton>
        {onNewSession && (
          <NewSessionButton
            machine={machine}
            group={name}
            density="band"
            isBusy={isCreating}
            onPress={() => onNewSession()}
          />
        )}
      </HeaderActions>
    </div>
  );
}

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
  /** `groupId` starts the session inside that group, not loose in the project. */
  start: (conn: GatewayConn, root: string, groupId?: string) => Promise<void>;
};

/**
 * Which plus is busy: one machine's project, and the band inside it when the reader
 * started the session on a group rather than on the project header.
 */
export function creationKey(base: string, root: string, groupId?: string): string {
  return groupId ? `${base}\u0000${root}\u0000${groupId}` : `${base}\u0000${root}`;
}

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
    grouped: Session[];
  } | null>(() => {
    const held = getClient(conn).heldProjectPage(root, pageSize, '', new Map());
    return held
      ? {
          start: 0,
          rows: held.rows,
          total: held.total,
          awaiting: held.awaiting,
          grouped: held.grouped ?? NO_ROWS,
        }
      : null;
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
          grouped: held.grouped ?? NO_ROWS,
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
          grouped: answer.grouped,
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
  // A ROW THIS BAND JUST RE-FILED IS PAINTED WHERE IT NOW BELONGS. The gateway's
  // answer is the truth of it, and the machine's own window carries that truth on the
  // next poll — 5.5s later. Until then the move is held here, or a row would sit in
  // the band it just left for a whole cycle after the tap that moved it.
  const [refiled, setRefiled] = useState<ReadonlyMap<string, Session>>(() => new Map());
  // NOTHING MOVES WHILE THE READER IS LOOKING AT IT (`lib/order-epoch`). The list
  // of projects is held by the screen; a page read from the gateway is held HERE,
  // or a turn finishing on another machine would slide this page under the thumb on
  // the next poll. A search answer arrives held already, and a row this reader
  // started or is holding words for is admitted rather than parked behind the pill.
  const rows = useMemo(() => {
    const api = getClient(conn);
    const shown = painting
      .filter((session) => !api.isSessionDeleted(session.id))
      .map((session) => settled(session, local, refiled));
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
      .filter((session) => !onPage.has(session.id) && !api.isSessionDeleted(session.id))
      .map((session) => settled(session, local, refiled));
    return parked.length === 0 ? held : [...parked, ...held];
  }, [searching, painting, local, refiled, epoch, admitted, paged, getClient, conn, list]);
  // THE GROUPS ARE NOT PAGED. The gateway answers a project's FILED sessions complete
  // and beside the window (`?grouped=aside`), because a group is a shelf a reader reads
  // whole: bands cut from the current page printed a name with `none on this page`
  // under it while its sessions sat four pages down, and a session filed from the
  // sheet left the very band that had just taken it.
  const shelved = useMemo(() => {
    const api = getClient(conn);
    // A query is answered over the whole project already; its hits are the list.
    if (searching) return NO_ROWS;
    return (paged?.grouped ?? NO_ROWS)
      .filter((session) => !api.isSessionDeleted(session.id))
      .map((session) => settled(session, local, refiled));
  }, [searching, paged, local, refiled, getClient, conn]);
  // Every row this project is painting: the shelves, and the page under them. A verb
  // aimed at a row - a drop, a `Move to...` - has to find it wherever it stands.
  const painted = useMemo(() => [...shelved, ...rows], [shelved, rows]);
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

  // THE GROUPS INSIDE THIS PROJECT, and they are the GATEWAY's rather than this
  // device's: the TUI files a session under one with the same call, so a band that
  // exists here is a band every client of the machine paints. Read while the project
  // is FOLDED too — the ⋮ beside its name manages groups without opening the list.
  const [groups, setGroups] = useState<SessionGroup[]>([]);
  const [groupsRead, setGroupsRead] = useState(0);
  useEffect(() => {
    if (!isVisible) return;
    const control = new AbortController();
    let live = true;
    void (async () => {
      try {
        const answer = await getClient(conn).listSessionGroups(root, control.signal);
        if (live) setGroups(answer?.groups ?? []);
      } catch {
        // A project whose groups cannot be read paints as an ungrouped one. Nothing
        // else in this band depends on them, and an unreachable machine is said once,
        // by its own band.
      }
    })();
    return () => {
      live = false;
      control.abort();
    };
  }, [conn, root, isVisible, getClient, groupsRead]);
  // WHERE EACH ROW IS FILED. A group's rows stay CONTIGUOUS under its own name, and
  // whatever nobody filed keeps the project's own order below them — the shape the
  // TUI's navigator paints (`tui/dialogs`), so one list is not two different pictures.
  const filed = useMemo(() => {
    const byGroup = new Map<string, Session[]>();
    const loose: Session[] = [];
    const seen = new Set<string>();
    // The shelves first, then the page: a row the reader just filed is on both until
    // the next poll lands, and it belongs to its band either way.
    for (const session of painted) {
      if (seen.has(session.id)) continue;
      seen.add(session.id);
      const gid = typeof session.group_id === 'string' ? session.group_id : '';
      if (gid === '') {
        loose.push(session);
        continue;
      }
      const held = byGroup.get(gid);
      if (held) held.push(session);
      else byGroup.set(gid, [session]);
    }
    return { byGroup, loose };
  }, [painted]);
  // A band for every group the project HAS, plus one for any group a ROW names that
  // this device has not read yet: a row is never dropped because the list arrived a
  // beat ahead of the groups.
  const bands = useMemo<GroupBandView[]>(() => {
    const known = groups.map((group) => ({
      id: group.id,
      name: group.name,
      color: group.color,
      count: group.session_count,
    }));
    const unread = [...filed.byGroup.entries()]
      .filter(([gid]) => !groups.some((group) => group.id === gid))
      .map(([gid, held]) => ({
        id: gid,
        name: typeof held[0].group_name === 'string' ? held[0].group_name : 'Group',
        color: typeof held[0].group_color === 'string' ? held[0].group_color : null,
        count: held.length,
      }));
    return [...known, ...unread];
  }, [groups, filed]);
  // A GROUP FOLDS ON ITS OWN, out of the store the project's fold lives in, so a band
  // this reader shut stays shut on the next screen. A group nobody shut is open: a
  // name and a count with nothing under them say less than the rows do.
  const [groupFolds, setGroupFolds] = useState<Record<string, boolean>>({});
  const isGroupOpen = (gid: string) =>
    groupFolds[gid] ?? readProjectFold(groupFoldKey(machineKey(conn), root, gid)) ?? true;
  const foldGroup = (gid: string, open: boolean) => {
    writeProjectFold(groupFoldKey(machineKey(conn), root, gid), open);
    setGroupFolds((held) => ({ ...held, [gid]: open }));
  };
  // ONE SHEET, wherever it was opened from: the project's ⋮ lists its groups, a band's
  // own ⋮ opens that group's verbs, and naming a group is a STEP inside it. A step is
  // left the way it was entered (`MenuBack`), never out to blank paper.
  const [menu, setMenu] = useState<{ at: MenuPosition; step: MenuStep } | null>(null);
  const [typed, setTyped] = useState('');
  const [failure, setFailure] = useState<string | null>(null);
  const [isBusy, setIsBusy] = useState(false);
  const openMenu = useCallback((anchor: HTMLElement, step: MenuStep) => {
    const at = menuPosition(anchor.getBoundingClientRect(), MENU_WIDTH);
    if (!at) return;
    setFailure(null);
    setMenu({ at, step });
  }, []);
  const goTo = (step: MenuStep) => {
    setFailure(null);
    setMenu((held) => (held ? { ...held, step } : held));
  };
  // A VERB THE GATEWAY REFUSED IS SAID IN THE SHEET THE READER IS STILL HOLDING. A
  // name this project already uses is the one failure a group action has that is the
  // reader's to fix, and it arrives as a 409 rather than as silence.
  const attempt = async (act: () => Promise<unknown>, next: MenuStep | 'close') => {
    setIsBusy(true);
    setFailure(null);
    try {
      await act();
      setGroupsRead((count) => count + 1);
      if (next === 'close') setMenu(null);
      else goTo(next);
    } catch (failed) {
      setFailure(
        failed instanceof GatewayError && failed.status === 409
          ? 'This project already has a group with that name.'
          : 'That did not reach the machine. Try again.',
      );
    } finally {
      setIsBusy(false);
    }
  };
  // FILING A SESSION IS THE GATEWAY'S ANSWER, held here until the list catches up.
  const fileSession = (session: Session, gid: string | null, back: MenuStep | 'close') =>
    void attempt(async () => {
      const moved = await getClient(conn).assignSessionGroup(session.id, gid);
      setRefiled((held) => new Map(held).set(moved.id, moved));
    }, back);
  // One session's row, wherever it stands: inside a group's band, or under the
  // project itself with everything nobody filed.
  // A ROW DROPPED ON A BAND RUNS THE SAME FILING VERB the sheet's `Move to...` does.
  const dropSession = (sid: string, gid: string | null) => {
    const found = painted.find((one) => one.id === sid);
    if (!found || (found.group_id ?? null) === gid) return;
    fileSession(found, gid, 'close');
  };
  // Every row in these bands can be filed, so the list's own verbs carry one more: the
  // sheet opens under the strip's button, on the session that strip belongs to.
  const rowCommands = useMemo<SessionRowCommands>(
    () => ({
      ...rowActions.commands,
      moveToGroup: (session, _conn, anchor) => openMenu(anchor, { kind: 'move', sid: session.id }),
    }),
    [openMenu, rowActions.commands],
  );
  const row = (session: Session) => {
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
      // DRAG IS THE LIST'S OWN MOVE: the row carries its session id and a band takes the
      // drop, so filing by hand needs no menu at all.
      <SessionRow
        key={session.id}
        session={session}
        draft={drafts[draftMessageKey(base, session.id)] ?? EMPTY_DRAFT_MESSAGE}
        conn={conn}
        match={matches?.get(session.id) ?? null}
        needle={needle}
        commands={rowCommands}
        deletion={deletion}
        isDraggable
      />
    );
  };

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
      {/* THE COUNT GIVES WAY, NOT WHAT STANDS BESIDE IT. On the narrowest phone a
        paged project's caption is fuller than its column, and a clipped `1 new` is an
        arrival the reader cannot reach; the total ellipsises instead and keeps it whole. */}
      <span className="flex min-w-0 items-center">
        <HeaderTally count={tally.count} unit="session" className="min-w-0 truncate" />
        {hasPending && (
          <>
            <span aria-hidden className="shrink-0 whitespace-pre"> | </span>
            <TextButton
              className="pointer-events-auto relative shrink-0 whitespace-nowrap"
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
            <IconButton
              label={`Groups in ${project}`}
              variant="quiet"
              density="band"
              aria-haspopup="dialog"
              aria-expanded={menu !== null}
              onClick={(event) => openMenu(event.currentTarget, { kind: 'root' })}
            >
              <DotsIcon className="size-3.5" />
            </IconButton>
            <NewSessionButton
              machine={machineLabel(conn)}
              where={project}
              isBusy={creating?.at === creationKey(base, root)}
              onPress={() => void onNewSession(conn, root)}
            />
          </HeaderActions>
        </SectionHeader>
        {/* Rows own their internal dividers; the wrapper closes the final session. */}
        {isShowing && painted.length > 0 && (
          <div
            ref={rowsRef}
            className={`border-b ${needle ? 'border-dialog-hint' : 'border-edge'}`}
          >
            {bands.map((band) => {
              const held = filed.byGroup.get(band.id) ?? NO_ROWS;
              const isBandOpen = isGroupOpen(band.id);
              return (
                <div key={band.id}>
                  <GroupBand
                    name={band.name}
                    color={band.color}
                    count={band.count}
                    isOpen={isBandOpen}
                    onToggle={() => foldGroup(band.id, !isBandOpen)}
                    machine={machineLabel(conn)}
                    onActions={(anchor) => openMenu(anchor, { kind: 'group', id: band.id })}
                    // A session started HERE is minted inside this group, so it opens
                    // at the top of this band instead of loose in the project.
                    onNewSession={() => void onNewSession(conn, root, band.id)}
                    isCreating={creating?.at === creationKey(base, root, band.id)}
                    onDropSession={(sid) => dropSession(sid, band.id)}
                  />
                  {isBandOpen && held.map(row)}
                </div>
              );
            })}
            {filed.loose.map(row)}
          </div>
        )}
      </section>
      {menu && (
        <Menu label={`Groups in ${project}`} at={menu.at} onDismiss={() => setMenu(null)}>
          {(() => {
            const step = menu.step;
            if (step.kind === 'root')
              return (
                <>
                  <MenuHeading>Groups in {project}</MenuHeading>
                  <MenuItem
                    title="New group"
                    hint="File some of this project's sessions under a name of your own."
                    onSelect={() => {
                      setTyped('');
                      goTo({ kind: 'new' });
                    }}
                  />
                  {bands.length === 0 ? (
                    <MenuNote>Nothing in this project is grouped yet.</MenuNote>
                  ) : (
                    bands.map((band) => (
                      <MenuItem
                        key={band.id}
                        title={band.name}
                        meta={`${band.count}`}
                        icon={<Swatch color={band.color} />}
                        onSelect={() => goTo({ kind: 'group', id: band.id })}
                      />
                    ))
                  )}
                  {failure && <MenuNote>{failure}</MenuNote>}
                </>
              );
            if (step.kind === 'new')
              return (
                <>
                  <MenuBack
                    label={`Back to groups in ${project}`}
                    onBack={() => goTo({ kind: 'root' })}
                  >
                    New group
                  </MenuBack>
                  <NameForm
                    label="Group name"
                    value={typed}
                    commit="Create"
                    isBusy={isBusy}
                    onChange={setTyped}
                    onCommit={() =>
                      void attempt(() => getClient(conn).createSessionGroup(root, typed.trim()), {
                        kind: 'root',
                      })
                    }
                  />
                  {failure && <MenuNote>{failure}</MenuNote>}
                </>
              );
            if (step.kind === 'move') {
              const session = painted.find((one) => one.id === step.sid);
              // The row left the page while its sheet was open: say so, rather than
              // offering a verb with nothing behind it.
              if (!session) return <MenuNote>That session is gone.</MenuNote>;
              return (
                <>
                  <MenuBack
                    label={`Back to groups in ${project}`}
                    onBack={() => goTo({ kind: 'root' })}
                  >
                    Move {rowTitle(session)}
                  </MenuBack>
                  {bands.length === 0 ? (
                    <MenuNote>Nothing in this project is grouped yet.</MenuNote>
                  ) : (
                    bands.map((band) => (
                      <MenuItem
                        key={band.id}
                        title={band.name}
                        icon={<Swatch color={band.color} />}
                        badge={session.group_id === band.id ? 'filed' : undefined}
                        onSelect={() => fileSession(session, band.id, 'close')}
                      />
                    ))
                  )}
                  {session.group_id ? (
                    <MenuItem
                      title="Take out of its group"
                      onSelect={() => fileSession(session, null, 'close')}
                    />
                  ) : null}
                  {failure && <MenuNote>{failure}</MenuNote>}
                </>
              );
            }
            const band = bands.find((one) => one.id === step.id);
            // The group was deleted under the sheet, or by another client of the
            // machine: the step says so instead of offering verbs with nothing behind them.
            if (!band) return <MenuNote>That group is gone.</MenuNote>;
            const here: MenuStep = { kind: 'group', id: band.id };
            if (step.kind === 'rename')
              return (
                <>
                  <MenuBack label={`Back to ${band.name}`} onBack={() => goTo(here)}>
                    Rename {band.name}
                  </MenuBack>
                  <NameForm
                    label="New name"
                    value={typed}
                    commit="Rename"
                    isBusy={isBusy}
                    onChange={setTyped}
                    onCommit={() =>
                      void attempt(
                        () => getClient(conn).updateSessionGroup(band.id, { name: typed.trim() }),
                        here,
                      )
                    }
                  />
                  {failure && <MenuNote>{failure}</MenuNote>}
                </>
              );
            // DELETING ASKS. A group is a folder to some people and a batch of work to
            // others, so both answers are spelled out and the destructive one is second.
            if (step.kind === 'delete') {
              const drop = (sessions: 'detach' | 'with-sessions') =>
                void attempt(async () => {
                  const { detached, deleted } = await getClient(conn).deleteSessionGroup(
                    band.id,
                    sessions,
                  );
                  setRefiled((kept) => {
                    const next = new Map(kept);
                    for (const sid of [...detached, ...deleted]) next.delete(sid);
                    return next;
                  });
                }, { kind: 'root' });
              return (
                <>
                  <MenuBack label={`Back to ${band.name}`} onBack={() => goTo(here)}>
                    Delete {band.name}
                  </MenuBack>
                  <MenuItem
                    title="Keep its sessions"
                    hint="They stay in this project and go back to ungrouped."
                    onSelect={() => drop('detach')}
                  />
                  <MenuItem
                    title="Delete its sessions too"
                    tone="danger"
                    hint={`Deletes ${band.count} ${
                      band.count === 1 ? 'session' : 'sessions'
                    } with the group. This cannot be undone.`}
                    onSelect={() => drop('with-sessions')}
                  />
                  {failure && <MenuNote>{failure}</MenuNote>}
                </>
              );
            }
            return (
              <>
                <MenuBack
                  label={`Back to groups in ${project}`}
                  onBack={() => goTo({ kind: 'root' })}
                >
                  {band.name}
                </MenuBack>
                <MenuItem
                  title="Rename group"
                  onSelect={() => {
                    setTyped(band.name);
                    goTo({ kind: 'rename', id: band.id });
                  }}
                />
                <MenuItem
                  title="Delete group"
                  tone="danger"
                  hint="Asks what becomes of the sessions filed under it."
                  onSelect={() => goTo({ kind: 'delete', id: band.id })}
                />
                <MenuHeading tone="quiet">Colour</MenuHeading>
                {GROUP_COLORS.map((color) => (
                  <MenuItem
                    key={color}
                    title={`${color[0].toUpperCase()}${color.slice(1)}`}
                    icon={<Swatch color={color} />}
                    badge={color === groupColor(band.color) ? 'now' : undefined}
                    onSelect={() =>
                      void attempt(
                        () => getClient(conn).updateSessionGroup(band.id, { color }),
                        here,
                      )
                    }
                  />
                ))}
                {failure && <MenuNote>{failure}</MenuNote>}
              </>
            );
          })()}
        </Menu>
      )}
    </>
  );
});
